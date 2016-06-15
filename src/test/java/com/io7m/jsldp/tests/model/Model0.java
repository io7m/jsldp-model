package com.io7m.jsldp.tests.model;

import com.io7m.jserial.core.SerialNumber16;
import org.junit.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

public final class Model0
{
  private static final Logger LOG;

  static {
    LOG = LoggerFactory.getLogger(Model0.class);
  }

  static final class Message<T>
  {
    boolean reliable = false;
    int sequence = 0;
    List<Integer> acks = new LinkedList<>();
    T data = null;

    @Override
    public String toString()
    {
      final StringBuilder sb = new StringBuilder(128);
      sb.append("[message ");
      sb.append(this.sequence);
      sb.append(" ");
      sb.append(this.reliable ? "reliable" : "unreliable");
      sb.append(" ");
      sb.append(this.data);
      sb.append("]");
      return sb.toString();
    }
  }

  enum PeerProcessStatus
  {
    PROCESS_CONTINUE,
    PROCESS_DONE
  }

  static final class Peer<T>
  {
    private static final Logger LOG;

    static {
      LOG = LoggerFactory.getLogger(Peer.class);
    }

    private static final class UnacknowledgedMessage<T>
    {
      Message<T> message;
      int age = 0;
    }

    private final OneWayLink<T> incoming;
    private final OneWayLink<T> outgoing;
    private int sequence_outgoing = 0;
    private int sequence_incoming = 0;
    private final LinkedList<Message<T>> incoming_buffer = new LinkedList<>();
    private final String name;
    private final Map<Integer, UnacknowledgedMessage<T>> acks_not_received = new HashMap<>();
    private final List<Integer> acks_to_send = new LinkedList<>();
    private final LinkedList<Message<T>> received_done = new LinkedList<>();

    Peer(
      final String in_name,
      final OneWayLink<T> in_incoming,
      final OneWayLink<T> in_outgoing)
    {
      this.name = in_name;
      this.incoming = in_incoming;
      this.outgoing = in_outgoing;
    }

    void send(
      final T x,
      final boolean reliable)
    {
      this.sendWithAcks(x, new LinkedList<>(), reliable);
    }

    void sendWithAcks(
      final T x,
      final List<Integer> acks,
      final boolean reliable)
    {
      final Message<T> m = new Message<>();
      m.reliable = reliable;
      m.data = x;
      m.acks.addAll(acks);
      m.sequence = this.sequence_outgoing;
      this.sequence_outgoing = SerialNumber16.increment(this.sequence_outgoing);
      this.sendActual(m);

      if (reliable) {
        final UnacknowledgedMessage<T> um = new UnacknowledgedMessage<>();
        um.message = m;
        um.age = 0;
        this.acks_not_received.put(Integer.valueOf(m.sequence), um);
      }

      if (acks.isEmpty()) {
        Peer.LOG.debug("send: {}: {}", this.name, m);
      } else {
        Peer.LOG.debug("send: {}: {} (acks {})", this.name, m, acks);
      }
    }

    private void sendActual(final Message<T> m)
    {
      this.outgoing.add(m);
    }

    void receiveAll()
    {
      this.incoming_buffer.addAll(this.incoming.messages);
      this.incoming.messages.clear();
      Peer.LOG.debug(
        "receive: {}: {} messages",
        this.name,
        Integer.valueOf(this.incoming_buffer.size()));
    }

    private static final class MessageTooOld extends Exception
    {
      private final UnacknowledgedMessage<?> message;

      MessageTooOld(final UnacknowledgedMessage<?> um)
      {
        this.message = um;
      }
    }

    PeerProcessStatus processOneMessage(final int time)
      throws MessageTooOld
    {
      final boolean no_incoming = this.incoming_buffer.isEmpty();
      final boolean no_acks_to_receive = this.acks_not_received.isEmpty();
      final boolean no_acks_to_send = this.acks_to_send.isEmpty();

      if (no_incoming && no_acks_to_receive && no_acks_to_send) {
        return PeerProcessStatus.PROCESS_DONE;
      }

      if (no_incoming) {
        this.resendUnackedMessages();
        return PeerProcessStatus.PROCESS_CONTINUE;
      }

      final Message<T> m = this.incoming_buffer.remove();
      for (final Integer ack : m.acks) {
        if (this.acks_not_received.containsKey(ack)) {
          Peer.LOG.debug(
            "processOneMessage: {}: [seq {}]: received ack for {}",
            this.name, Integer.valueOf(this.sequence_incoming), ack);
          this.acks_not_received.remove(ack);
        } else {
          Peer.LOG.error(
            "processOneMessage: {}: [seq {}]: received unwanted ack {}",
            this.name, Integer.valueOf(this.sequence_incoming), ack);
        }
      }

      if (SerialNumber16.lessThanOrEqual(this.sequence_incoming, m.sequence)) {
        this.sequence_incoming = SerialNumber16.increment(m.sequence);
        this.received_done.add(m);
        Peer.LOG.debug(
          "processOneMessage: {}: [seq {}]: accepted {}",
          this.name, Integer.valueOf(this.sequence_incoming), m);

        if (m.reliable) {
          final LinkedList<Integer> acks = new LinkedList<>();
          acks.add(Integer.valueOf(m.sequence));
          this.sendWithAcks(null, acks, false);
        }
      } else {
        Peer.LOG.debug(
          "processOneMessage: {}: [seq {}]: discarded {}",
          this.name, Integer.valueOf(this.sequence_incoming), m);
      }

      this.resendUnackedMessages();
      return PeerProcessStatus.PROCESS_CONTINUE;
    }

    private void resendUnackedMessages()
      throws MessageTooOld
    {
      for (final Integer k : this.acks_not_received.keySet()) {
        final UnacknowledgedMessage<T> um = this.acks_not_received.get(k);
        um.age = Math.addExact(um.age, 1);
        if (um.age % 4 == 0) {
          Peer.LOG.debug(
            "processOneMessage: {}: [seq {}]: resending unacked message {}",
            this.name, Integer.valueOf(this.sequence_incoming), um.message);
          this.sendActual(um.message);
        }

        if (um.age > 12) {
          throw new MessageTooOld(um);
        }
      }
    }

    void checkInvariants()
    {
      final Collection<Integer> seqs = new HashSet<>(32);

      for (int index = 0; index < this.received_done.size(); ++index) {
        final Message<T> m_curr = this.received_done.get(index);
        final Integer boxed = Integer.valueOf(m_curr.sequence);
        Assert.assertFalse(seqs.contains(boxed));
        seqs.add(boxed);
      }

      for (int index = 0; index < this.received_done.size(); ++index) {
        if (index > 0) {
          final Message<T> m_prev = this.received_done.get(index - 1);
          final Message<T> m_curr = this.received_done.get(index);
          Peer.LOG.debug("check: {}: {} {}", this.name, m_prev, m_curr);
          Assert.assertTrue(
            SerialNumber16.lessThan(m_prev.sequence, m_curr.sequence));
        }
      }
    }
  }

  static final class OneWayLink<T>
  {
    private static final Logger LOG;

    static {
      LOG = LoggerFactory.getLogger(OneWayLink.class);
    }

    private final LinkedList<Message<T>> messages = new LinkedList<>();
    private final Random random;
    private final String name;
    private double loss;
    private double reordering;
    private double duplication;
    private int reordering_distance = 2;

    OneWayLink(
      final String in_name,
      final Random r)
    {
      this.name = in_name;
      this.random = r;
    }

    void add(final Message<T> m)
    {
      this.messages.add(m);
    }

    void setLoss(final double in_loss)
    {
      this.loss = in_loss;
    }

    void setReordering(final double in_reordering)
    {
      this.reordering = in_reordering;
    }

    void setReorderingDistance(final int in_distance)
    {
      this.reordering_distance = in_distance;
    }

    void setDuplication(final double in_duplication)
    {
      this.duplication = in_duplication;
    }

    void process()
    {
      this.processDuplicate();
      this.processLoss();
      this.processReorder();

      if (!this.messages.isEmpty()) {
        for (int index = 0; index < this.messages.size(); ++index) {
          OneWayLink.LOG.debug(
            "link: {}: [{}]: {}",
            this.name,
            Integer.valueOf(index),
            this.messages.get(index));
        }
      } else {
        OneWayLink.LOG.trace("link: {}: empty", this.name);
      }
    }

    private void processReorder()
    {
      int count = 0;
      int index = 0;
      while (index < this.messages.size()) {
        if (this.random.nextDouble() <= this.reordering) {
          count += 1;

          final int offset =
            this.random.nextInt(this.reordering_distance * 2);
          final int signed_offset =
            offset - (this.reordering_distance);
          final int signed = index + signed_offset;
          final int other;
          if (signed < 0) {
            other = 0;
          } else if (signed >= this.messages.size()) {
            other = this.messages.size() - 1;
          } else {
            other = signed;
          }

          final Message<T> m0 = this.messages.get(index);
          final Message<T> m1 = this.messages.get(other);
          final int diff =
            Math.abs(SerialNumber16.distanceBetween(m0.sequence, m1.sequence));

          OneWayLink.LOG.debug(
            "link: {}: reorder {} <-> {}, distance {}",
            this.name,
            Integer.valueOf(index),
            Integer.valueOf(other),
            Integer.valueOf(diff));

          this.messages.set(other, m0);
          this.messages.set(index, m1);
        }
        index += 1;
      }

      OneWayLink.LOG.trace(
        "link: {}: reordered {} messages",
        this.name,
        Integer.valueOf(count));
    }

    private void processLoss()
    {
      final int before = this.messages.size();
      int index = 0;
      while (index < this.messages.size()) {
        if (this.random.nextDouble() <= this.loss) {
          OneWayLink.LOG.debug(
            "link: {}: lost {}",
            this.name,
            this.messages.get(index));
          this.messages.remove(index);
        } else {
          index += 1;
        }
      }
      OneWayLink.LOG.trace(
        "link: {}: lost {} messages",
        this.name,
        Integer.valueOf(before - this.messages.size()));
    }

    private void processDuplicate()
    {
      final int before = this.messages.size();
      int index = 0;
      while (index < this.messages.size()) {
        if (this.random.nextDouble() <= this.duplication) {
          final Message<T> m = this.messages.get(index);
          OneWayLink.LOG.debug(
            "link: {}: duplicated {}",
            this.name,
            m);
          this.messages.add(index, m);
          index += 2;
        } else {
          index += 1;
        }
      }

      OneWayLink.LOG.trace(
        "link: {}: duplicate created {} new messages",
        this.name,
        Integer.valueOf(this.messages.size() - before));
    }
  }

  private final Random random = new Random(0L);
  private final OneWayLink<Integer> server_to_client =
    new OneWayLink<>("server->client", this.random);
  private final OneWayLink<Integer> client_to_server =
    new OneWayLink<>("client->server", this.random);
  private final Peer<Integer> server =
    new Peer<>("server", this.client_to_server, this.server_to_client);
  private final Peer<Integer> client =
    new Peer<>("client", this.server_to_client, this.client_to_server);

  void run()
  {
    this.server.send(Integer.valueOf(23), true);
    this.server.send(Integer.valueOf(24), true);
    this.server.send(Integer.valueOf(25), true);

    this.server_to_client.setLoss(0.0);
    this.server_to_client.setReordering(0.00);
    this.server_to_client.setDuplication(0.0);
    this.server_to_client.process();

    this.client_to_server.setLoss(0.3);
    this.client_to_server.setReordering(0.00);
    this.client_to_server.setDuplication(0.0);
    this.client_to_server.process();

    this.server.receiveAll();
    this.client.receiveAll();

    int time = 0;
    try {
      while (true) {
        final PeerProcessStatus sr = this.server.processOneMessage(time);
        final PeerProcessStatus cr = this.client.processOneMessage(time);
        if (sr == PeerProcessStatus.PROCESS_DONE &&
          cr == PeerProcessStatus.PROCESS_DONE) {
          break;
        }
        this.server_to_client.process();
        this.client_to_server.process();
        this.server.receiveAll();
        this.client.receiveAll();
        ++time;
      }
    } catch (final Peer.MessageTooOld e) {
      Model0.LOG.error("message too old: {}", e.message.message);
    }

    this.server.received_done.forEach(m -> LOG.debug("server: result: {}", m));
    this.client.received_done.forEach(m -> LOG.debug("client: result: {}", m));

    this.server.checkInvariants();
    this.client.checkInvariants();
  }

  public static void main(final String[] args)
  {
    final Model0 m = new Model0();
    m.run();
  }
}
