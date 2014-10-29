/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package play.sbtclient

import akka.actor._
import java.io.File
import sbt.client.{Subscription,SbtConnector,SbtClient,Interaction, SettingKey, TaskKey}
import scala.concurrent.ExecutionContext
import scala.util.{ Try, Success, Failure }
import sbt.protocol.{ Analysis, CompileFailedException, TaskResult, ScopedKey, BuildValue, fromXsbtiPosition, CompilationFailure }
import sbt.protocol
import scala.language.existentials, scala.language.higherKinds

object SbtClientProxy {
  sealed trait Notification
  object Notifications {
    case object Reconnected extends Notification
  }

  case class UpdateClient(client:SbtClient)
  case class WatchEvent(key:protocol.ScopedKey, result:protocol.TaskResult)

  sealed trait Response
  case class LookupScopedKeyResponse(name: String, result: Try[Seq[ScopedKey]]) extends Response
  case class ExecutionId(id:Try[Long],execution:RequestExecution) extends Response
  case class CancelExecutionResponse(id:Long, result:Try[Boolean]) extends Response
  case object EventsSubscribed extends Response
  case object EventsUnsubscribed extends Response
  case object BuildSubscribed extends Response
  case object BuildUnsubscribed extends Response
  case class WatchingTask(key:TaskKey[_]) extends Response
  case class TaskWatchRemoved(key:TaskKey[_]) extends Response
  case class WatchingSetting(key:SettingKey[_]) extends Response
  case class SettingWatchRemoved(key:SettingKey[_]) extends Response
  case object Closed extends Response

  sealed trait LocalRequest[Resp] extends Request[Resp]
  case class LookupScopedKey(name: String, sendTo:ActorRef) extends LocalRequest[LookupScopedKeyResponse] {
    def responseWithResult(result: Try[Seq[ScopedKey]])(implicit sender: ActorRef):Unit = response(LookupScopedKeyResponse(name,result))
  }

  sealed trait RequestExecution extends LocalRequest[ExecutionId] {
    def interaction: Option[(Interaction, ExecutionContext)]
    final def responseWithExecutionId(id:Try[Long])(implicit sender: ActorRef):Unit = response(ExecutionId(id,this))
  }
  object RequestExecution {
    case class ByCommandOrTask(commandOrTask: String, interaction: Option[(Interaction, ExecutionContext)], sendTo:ActorRef) extends RequestExecution
    case class ByScopedKey(key: ScopedKey, interaction: Option[(Interaction, ExecutionContext)], sendTo:ActorRef) extends RequestExecution
  }

  case class CancelExecution(id:Long, sendTo:ActorRef) extends LocalRequest[CancelExecutionResponse] {
    final def responseWithResult(r:Try[Boolean])(implicit sender: ActorRef):Unit = response(CancelExecutionResponse(id,r))
  }

  case class SubscribeToEvents(sendTo:ActorRef) extends LocalRequest[EventsSubscribed.type] {
    def subscribed()(implicit sender: ActorRef):Unit = response(EventsSubscribed)
  }
  case class UnsubscribeFromEvents(sendTo:ActorRef) extends LocalRequest[EventsUnsubscribed.type] {
    def unsubscribed()(implicit sender: ActorRef):Unit = response(EventsUnsubscribed)
  }
  case class SubscribeToBuild(sendTo:ActorRef) extends LocalRequest[BuildSubscribed.type] {
    def subscribed()(implicit sender: ActorRef):Unit = response(BuildSubscribed)
  }
  case class UnsubscribeFromBuild(sendTo:ActorRef) extends LocalRequest[BuildUnsubscribed.type] {
    def unsubscribed()(implicit sender: ActorRef):Unit = response(BuildUnsubscribed)
  }
  case class WatchTask(key:TaskKey[_],sendTo:ActorRef) extends LocalRequest[WatchingTask] {
    def watching(key:TaskKey[_])(implicit sender: ActorRef):Unit = response(WatchingTask(key))
  }
  case class RemoveTaskWatch(key:TaskKey[_],sendTo:ActorRef) extends LocalRequest[TaskWatchRemoved] {
    def removed(key:TaskKey[_])(implicit sender: ActorRef):Unit = response(TaskWatchRemoved(key))
  }
  case class WatchSetting(key:SettingKey[_],sendTo:ActorRef) extends LocalRequest[WatchingSetting] {
    def watching(key:SettingKey[_])(implicit sender: ActorRef):Unit = response(WatchingSetting(key))
  }
  case class RemoveSettingWatch(key:SettingKey[_],sendTo:ActorRef) extends LocalRequest[SettingWatchRemoved] {
    def removed(key:SettingKey[_])(implicit sender: ActorRef):Unit = response(SettingWatchRemoved(key))
  }
  case class Close(sendTo:ActorRef) extends LocalRequest[Closed.type] {
    def closed()(implicit sender: ActorRef):Unit = response(Closed)
  }

  sealed trait GenericKey[T] {
    def scopedKey:ScopedKey
  }
  case class GenericSettingKey[T](key:SettingKey[T],manifest:Manifest[T]) extends GenericKey[T] {
    def scopedKey:ScopedKey = key.key
  }
  case class GenericTaskKey[T](key:TaskKey[T],manifest:Manifest[T]) extends GenericKey[T] {
    def scopedKey:ScopedKey = key.key
  }

  object GenericKey {
    def fromSettingKey[T](key:SettingKey[T])(implicit mf:Manifest[T]):GenericKey[T] =
      GenericSettingKey[T](key,mf)
    def fromTaskKey[T](key:TaskKey[T])(implicit mf:Manifest[T]):GenericKey[T] =
      GenericTaskKey[T](key,mf)
  }

  case class EventSubscribers(subscription:Subscription,subscribers:Set[ActorRef] = Set.empty[ActorRef])
  case class BuildSubscribers(subscription:Subscription,subscribers:Set[ActorRef] = Set.empty[ActorRef])
  case class WatchSubscribers(key:GenericKey[_],subscription:Subscription,subscribers:Set[ActorRef] = Set.empty[ActorRef])

  case class State(client:SbtClient,
                   eventSubscriptions:Option[EventSubscribers] = None,
                   buildSubscriptions:Option[BuildSubscribers] = None,
                   watchSubscriptions:Map[protocol.ScopedKey,WatchSubscribers] = Map.empty[protocol.ScopedKey,WatchSubscribers]) {
    def allSubscribers():Set[ActorRef] = {
      (eventSubscriptions.map(_.subscribers) getOrElse Set.empty[ActorRef]) ++
      (buildSubscriptions.map(_.subscribers) getOrElse Set.empty[ActorRef]) ++
      (watchSubscriptions.values.map(_.subscribers).flatten.toSet)
    }
  }
}

final class SbtClientProxy(initialClient:SbtClient,
                           ec: ExecutionContext,
                           notificationSink:SbtClientProxy.Notification => Unit  = _ => ()) extends Actor with ActorLogging {
  import SbtClientProxy._
  implicit val ec1 = ec

  private def eventListener(self:ActorRef)(event:protocol.Event):Unit = {
    self ! event
  }

  private def buildListener(self:ActorRef)(event:protocol.MinimalBuildStructure):Unit = {
    self ! event
  }

  private def watchListener(self:ActorRef)(key:protocol.ScopedKey, result:protocol.TaskResult):Unit = {
    self ! WatchEvent(key,result)
  }

  private def canUnwatch(state:State, target:ActorRef):Boolean = state.allSubscribers()(target)

  private def onRequest(req:LocalRequest[_], state:State, self:ActorRef):Unit = {
    implicit val localSelf = self

    req match {
      case r:Close =>
        state.eventSubscriptions foreach { es =>
          es.subscription.cancel() // Can this throw an exeption?
        }
        state.buildSubscriptions foreach { bs =>
          bs.subscription.cancel() // Can this throw an exeption?
        }
        state.watchSubscriptions foreach { case (_,ws) =>
          ws.subscription.cancel() // Can this throw an exeption?
        }
        state.client.close()
        r.closed()
        context stop self
      case r:LookupScopedKey =>
        state.client.lookupScopedKey(r.name).onComplete(r.responseWithResult)
      case r:RequestExecution.ByCommandOrTask =>
        state.client.requestExecution(r.commandOrTask,r.interaction).onComplete(id => r.responseWithExecutionId(id))
      case r:RequestExecution.ByScopedKey =>
        state.client.requestExecution(r.key,r.interaction).onComplete(id => r.responseWithExecutionId(id))
      case r:CancelExecution =>
        state.client.cancelExecution(r.id).onComplete(x => r.responseWithResult(x))
      case r:SubscribeToEvents =>
        state.eventSubscriptions match {
          case Some(es) =>
            context.become(running(state.copy(eventSubscriptions = Some(es.copy(subscribers = es.subscribers + r.sendTo)))))
          case None =>
            val s = state.client.handleEvents(eventListener(self))
            context.become(running(state.copy(eventSubscriptions = Some(EventSubscribers(s,Set(r.sendTo))))))
        }
        context.watch(r.sendTo)
        r.subscribed()
      case r:UnsubscribeFromEvents =>
        val newState = state.eventSubscriptions match {
          case Some(es) =>
            val newSubs = es.subscribers - r.sendTo
            if (newSubs.isEmpty) {
              es.subscription.cancel()
              state.copy(eventSubscriptions = None)
            } else state.copy(eventSubscriptions = Some(es.copy(subscribers = newSubs)))
          case None => state
        }
        context.become(running(newState))
        if (canUnwatch(newState,r.sendTo)) context.unwatch(r.sendTo)
        r.unsubscribed()
      case r:SubscribeToBuild =>
        state.buildSubscriptions match {
          case Some(bs) =>
            context.become(running(state.copy(buildSubscriptions = Some(bs.copy(subscribers = bs.subscribers + r.sendTo)))))
          case None =>
            val s = state.client.watchBuild(buildListener(self))
            context.become(running(state.copy(buildSubscriptions = Some(BuildSubscribers(s,Set(r.sendTo))))))
        }
        context.watch(r.sendTo)
        r.subscribed()
      case r:UnsubscribeFromBuild =>
        val newState = state.buildSubscriptions match {
          case Some(bs) =>
            val newSubs = bs.subscribers - r.sendTo
            if (newSubs.isEmpty) {
              bs.subscription.cancel()
              state.copy(buildSubscriptions = None)
            } else state.copy(buildSubscriptions = Some(bs.copy(subscribers = newSubs)))
          case None => state
        }
        context.become(running(newState))
        if (canUnwatch(newState,r.sendTo)) context.unwatch(r.sendTo)
        r.unsubscribed()
      case r:WatchTask =>
        state.watchSubscriptions.get(r.key.key) match {
          case Some(ws) =>
            val newWatcher = ws.copy(subscribers = ws.subscribers + r.sendTo)
            context.become(running(state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))))
          case None =>
            val s = state.client.rawLazyWatch(r.key)(watchListener(self))
            val newWatcher = WatchSubscribers(GenericKey.fromTaskKey(r.key),s,Set(r.sendTo))
            context.become(running(state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))))
        }
        context.watch(r.sendTo)
        r.watching(r.key)
      case r:RemoveTaskWatch =>
        val newState = state.watchSubscriptions.get(r.key.key) match {
          case Some(ws) =>
            val newSubs = ws.subscribers - r.sendTo
            if (newSubs.isEmpty) {
              ws.subscription.cancel()
              state.copy(watchSubscriptions = state.watchSubscriptions - r.key.key)
            } else {
              val newWatcher = ws.copy(subscribers = newSubs)
              state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))
            }
          case None => state
        }
        context.become(running(newState))
        if (canUnwatch(newState,r.sendTo)) context.unwatch(r.sendTo)
        r.removed(r.key)
      case r:WatchSetting =>
        state.watchSubscriptions.get(r.key.key) match {
          case Some(ws) =>
            val newWatcher = ws.copy(subscribers = ws.subscribers + r.sendTo)
            context.become(running(state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))))
          case None =>
            val s = state.client.rawLazyWatch(r.key)(watchListener(self))
            val newWatcher = WatchSubscribers(GenericKey.fromSettingKey(r.key),s,Set(r.sendTo))
            context.become(running(state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))))
        }
        context.watch(r.sendTo)
        r.watching(r.key)
      case r:RemoveSettingWatch =>
        val newState = state.watchSubscriptions.get(r.key.key) match {
          case Some(ws) =>
            val newSubs = ws.subscribers - r.sendTo
            if (newSubs.isEmpty) {
              ws.subscription.cancel()
              state.copy(watchSubscriptions = state.watchSubscriptions - r.key.key)
            } else {
              val newWatcher = ws.copy(subscribers = newSubs)
              state.copy(watchSubscriptions = state.watchSubscriptions + (r.key.key -> newWatcher))
            }
          case None => state
        }
        context.become(running(newState))
        if (canUnwatch(newState,r.sendTo)) context.unwatch(r.sendTo)
        r.removed(r.key)
    }
  }


  private def running(state:State):Receive = {
    case UpdateClient(c) =>
      val newEventSubscriptions = state.eventSubscriptions map { es =>
        es.subscription.cancel() // Can this throw an exeption?
        es.copy(subscription = c.handleEvents(eventListener(self)))
      }
      val newBuildSubscriptions = state.buildSubscriptions map { bs =>
        bs.subscription.cancel() // Can this throw an exeption?
        bs.copy(subscription = c.watchBuild(buildListener(self)))
      }
      val newWatchSubscriptions = state.watchSubscriptions mapValues { ws =>
        ws.subscription.cancel() // Can this throw an exeption?
        ws.key match {
          case GenericTaskKey(k,_) =>
            ws.copy(subscription = c.rawLazyWatch(k)(watchListener(self)))
          case GenericSettingKey(k,_) =>
            ws.copy(subscription = c.rawLazyWatch(k)(watchListener(self)))
        }
      }
      notificationSink(Notifications.Reconnected)
      context.become(running(state.copy(client = c, eventSubscriptions = newEventSubscriptions, buildSubscriptions = newBuildSubscriptions, watchSubscriptions = newWatchSubscriptions)))
    case Terminated(t) =>
      context.unwatch(t)
      val newEventSubscriptions = state.eventSubscriptions flatMap { es =>
        val newSubs = es.subscribers - t
        if (newSubs.isEmpty) {
          es.subscription.cancel() // Can this throw an exeption?
          None
        } else Some(es.copy(subscribers = newSubs))
      }
      val newBuildSubscriptions = state.buildSubscriptions flatMap { bs =>
        val newSubs = bs.subscribers - t
        if (newSubs.isEmpty) {
          bs.subscription.cancel() // Can this throw an exeption?
          None
        } else Some(bs.copy(subscribers = newSubs))
      }
      val newWatchSubscriptions = state.watchSubscriptions mapValues { ws =>
        ws.copy(subscribers = ws.subscribers - t)
      } filter { case (_,ws) =>
        if (ws.subscribers.isEmpty) {
          ws.subscription.cancel()
          false
        } else true
      }
      context.become(running(state.copy(eventSubscriptions = newEventSubscriptions, buildSubscriptions = newBuildSubscriptions, watchSubscriptions = newWatchSubscriptions)))
    case req:LocalRequest[_] => onRequest(req,state,self)
    case pEvent:protocol.Event =>
      state.eventSubscriptions.foreach { es =>
        es.subscribers.foreach(_ ! pEvent)
      }
    case pEvent:protocol.MinimalBuildStructure =>
      state.buildSubscriptions.foreach { bs =>
        bs.subscribers.foreach(_ ! pEvent)
      }
    case event:WatchEvent =>
      state.watchSubscriptions.get(event.key).foreach { ws =>
        ws.subscribers.foreach(_ ! event)
      }
  }

  def receive:Receive = running(State(client = initialClient))
}
