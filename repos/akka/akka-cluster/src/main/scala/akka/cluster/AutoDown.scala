/**
  * Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>
  */
package akka.cluster

import akka.actor.Actor
import scala.concurrent.duration.FiniteDuration
import akka.actor.Props
import akka.cluster.ClusterEvent._
import akka.actor.Cancellable
import scala.concurrent.duration.Duration
import akka.actor.Address
import akka.actor.Scheduler

/**
  * INTERNAL API
  */
private[cluster] object AutoDown {

  def props(autoDownUnreachableAfter: FiniteDuration): Props =
    Props(classOf[AutoDown], autoDownUnreachableAfter)

  final case class UnreachableTimeout(node: UniqueAddress)
}

/**
  * INTERNAL API
  *
  * An unreachable member will be downed by this actor if it remains unreachable
  * for the specified duration and this actor is running on the leader node in the
  * cluster.
  *
  * The implementation is split into two classes AutoDown and AutoDownBase to be
  * able to unit test the logic without running cluster.
  */
private[cluster] class AutoDown(autoDownUnreachableAfter: FiniteDuration)
    extends AutoDownBase(autoDownUnreachableAfter) {

  val cluster = Cluster(context.system)
  import cluster.InfoLogger._

  override def selfAddress = cluster.selfAddress

  override def scheduler: Scheduler = cluster.scheduler

  // re-subscribe when restart
  override def preStart(): Unit = {
    cluster.subscribe(self, classOf[ClusterDomainEvent])
    super.preStart()
  }
  override def postStop(): Unit = {
    cluster.unsubscribe(self)
    super.postStop()
  }

  override def down(node: Address): Unit = {
    require(leader)
    logInfo("Leader is auto-downing unreachable node [{}]", node)
    cluster.down(node)
  }
}

/**
  * INTERNAL API
  *
  * The implementation is split into two classes AutoDown and AutoDownBase to be
  * able to unit test the logic without running cluster.
  */
private[cluster] abstract class AutoDownBase(
    autoDownUnreachableAfter: FiniteDuration)
    extends Actor {

  import AutoDown._

  def selfAddress: Address

  def down(node: Address): Unit

  def scheduler: Scheduler

  import context.dispatcher

  val skipMemberStatus = Gossip.convergenceSkipUnreachableWithMemberStatus

  var scheduledUnreachable: Map[UniqueAddress, Cancellable] = Map.empty
  var pendingUnreachable: Set[UniqueAddress] = Set.empty
  var leader = false

  override def postStop(): Unit = {
    scheduledUnreachable.values foreach { _.cancel }
  }

  def receive = {
    case state: CurrentClusterState ⇒
      leader = state.leader.exists(_ == selfAddress)
      state.unreachable foreach unreachableMember

    case UnreachableMember(m) ⇒ unreachableMember(m)

    case ReachableMember(m) ⇒ remove(m.uniqueAddress)
    case MemberRemoved(m, _) ⇒ remove(m.uniqueAddress)

    case LeaderChanged(leaderOption) ⇒
      leader = leaderOption.exists(_ == selfAddress)
      if (leader) {
        pendingUnreachable.foreach(node ⇒ down(node.address))
        pendingUnreachable = Set.empty
      }

    case UnreachableTimeout(node) ⇒
      if (scheduledUnreachable contains node) {
        scheduledUnreachable -= node
        downOrAddPending(node)
      }

    case _: ClusterDomainEvent ⇒ // not interested in other events
  }

  def unreachableMember(m: Member): Unit =
    if (!skipMemberStatus(m.status) &&
        !scheduledUnreachable.contains(m.uniqueAddress))
      scheduleUnreachable(m.uniqueAddress)

  def scheduleUnreachable(node: UniqueAddress): Unit = {
    if (autoDownUnreachableAfter == Duration.Zero) {
      downOrAddPending(node)
    } else {
      val task = scheduler.scheduleOnce(
          autoDownUnreachableAfter, self, UnreachableTimeout(node))
      scheduledUnreachable += (node -> task)
    }
  }

  def downOrAddPending(node: UniqueAddress): Unit = {
    if (leader) {
      down(node.address)
    } else {
      // it's supposed to be downed by another node, current leader, but if that crash
      // a new leader must pick up these
      pendingUnreachable += node
    }
  }

  def remove(node: UniqueAddress): Unit = {
    scheduledUnreachable.get(node) foreach { _.cancel }
    scheduledUnreachable -= node
    pendingUnreachable -= node
  }
}
