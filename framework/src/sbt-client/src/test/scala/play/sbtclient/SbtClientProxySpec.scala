/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package play.sbtclient

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

import akka.actor._
import akka.testkit._
import concurrent.ExecutionContext.Implicits.global
import sbt.protocol
import java.net.URI
import scala.concurrent.Future
import play.api.libs.json._
import sbt.client.{ SettingKey, TaskKey}

object SbtClientProxySpec {
  val sampleManifest = implicitly[Manifest[String]]
  val sampleEvent:protocol.Event = protocol.ExecutionStarting(100)
  val sampleBuild:protocol.MinimalBuildStructure = protocol.MinimalBuildStructure(Seq.empty[URI], Seq.empty[protocol.MinimalProjectStructure])
  val sampleScopedKey = protocol.ScopedKey(protocol.AttributeKey("foo",protocol.TypeInfo.fromManifest(sampleManifest)), protocol.SbtScope())
  val sampleScopedKey1 = protocol.ScopedKey(protocol.AttributeKey("foo1",protocol.TypeInfo.fromManifest(sampleManifest)), protocol.SbtScope())
  def sampleScopedKeyLookup(in:String):Future[Seq[protocol.ScopedKey]] = Future.successful(in match {
    case "foo" => Seq(sampleScopedKey)
    case _ => Seq.empty[protocol.ScopedKey]
  })

  val sampleTaskKey = TaskKey[String](sampleScopedKey)
  val sampleSettingKey = SettingKey[String](sampleScopedKey1)
  val sampleBuildValue = protocol.BuildValue("sample")
  val sampleTaskResult = protocol.TaskSuccess(sampleBuildValue)
}

class Forwarder(target:ActorRef) extends Actor with ActorLogging {
  def receive:Receive = {
    case msg => target.tell(msg,sender)
  }
}

class SbtClientProxySpec extends Specification with NoTimeConversions {
   // sequential
   import SbtClientProxy._, SbtClientProxySpec._

  "A SbtClientProxy" should {
    "close down gracefully" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        val cp = system.actorOf(Props(new SbtClientProxy(client,global,x => testActor ! x)))
        cp ! Close(testActor)
        expectMsgType[Closed.type] must be equalTo Closed
      }
    }
    "subscribe to Events" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        withSbtClientProxy(client) { cp =>
          client.sendEvent(sampleEvent)
          expectNoMsg()
          cp ! SubscribeToEvents(testActor)
          expectMsgType[EventsSubscribed.type] must be equalTo EventsSubscribed
          client.sendEvent(sampleEvent)
          expectMsgType[protocol.Event] must be equalTo sampleEvent
          cp ! UnsubscribeFromEvents(testActor)
          expectMsgType[EventsUnsubscribed.type] must be equalTo EventsUnsubscribed
          client.sendEvent(sampleEvent)
          expectNoMsg()
        }
      }
    }
    "subscribe to BuildEvents" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        withSbtClientProxy(client) { cp =>
          client.sendBuildStructureUpdate(sampleBuild)
          expectNoMsg()
          cp ! SubscribeToBuild(testActor)
          expectMsgType[BuildSubscribed.type] must be equalTo BuildSubscribed
          client.sendBuildStructureUpdate(sampleBuild)
          expectMsgType[protocol.MinimalBuildStructure] must be equalTo sampleBuild
          cp ! UnsubscribeFromBuild(testActor)
          expectMsgType[BuildUnsubscribed.type] must be equalTo BuildUnsubscribed
          client.sendBuildStructureUpdate(sampleBuild)
          expectNoMsg()
        }
      }
    }
    "subscribe to TaskEvents" in new AkkaTestKitHelper() {
      withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
        withSbtClientProxy(client) { cp =>
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectNoMsg()
          cp ! WatchTask(sampleTaskKey,testActor)
          expectMsgType[WatchingTask] must be equalTo WatchingTask(sampleTaskKey)
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey, sampleTaskResult)
          cp ! RemoveTaskWatch(sampleTaskKey,testActor)
          expectMsgType[TaskWatchRemoved] must be equalTo TaskWatchRemoved(sampleTaskKey)
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectNoMsg()
        }
      }
    }
    "subscribe to SettingEvents" in new AkkaTestKitHelper() {
      withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
        withSbtClientProxy(client) { cp =>
          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectNoMsg()
          cp ! WatchSetting(sampleSettingKey,testActor)
          expectMsgType[WatchingSetting] must be equalTo WatchingSetting(sampleSettingKey)
          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey1, sampleTaskResult)
          cp ! RemoveSettingWatch(sampleSettingKey,testActor)
          expectMsgType[SettingWatchRemoved] must be equalTo SettingWatchRemoved(sampleSettingKey)
          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectNoMsg()
        }
      }
    }
    "handle client termination" in new AkkaTestKitHelper() {
      val forwarder = system.actorOf(Props(new Forwarder(testActor)))
      withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
        withSbtClientProxy(client) { cp =>
          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          client.sendBuildStructureUpdate(sampleBuild)
          client.sendEvent(sampleEvent)
          expectNoMsg()

          cp ! WatchSetting(sampleSettingKey,forwarder)
          expectMsgType[WatchingSetting] must be equalTo WatchingSetting(sampleSettingKey)
          cp ! WatchTask(sampleTaskKey,forwarder)
          expectMsgType[WatchingTask] must be equalTo WatchingTask(sampleTaskKey)
          cp ! SubscribeToBuild(forwarder)
          expectMsgType[BuildSubscribed.type] must be equalTo BuildSubscribed
          cp ! SubscribeToEvents(forwarder)
          expectMsgType[EventsSubscribed.type] must be equalTo EventsSubscribed

          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey, sampleTaskResult)

          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey1, sampleTaskResult)

          client.sendBuildStructureUpdate(sampleBuild)
          expectMsgType[protocol.MinimalBuildStructure] must be equalTo sampleBuild

          client.sendEvent(sampleEvent)
          expectMsgType[protocol.Event] must be equalTo sampleEvent

          system stop forwarder

          expectMsg(SbtClientProxy.Notifications.HandledTermination)

          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          client.sendBuildStructureUpdate(sampleBuild)
          client.sendEvent(sampleEvent)
          expectNoMsg()
        }
      }
    }
    "properly handle reconnect" in new AkkaTestKitHelper() {
      withFakeSbtClient(scopedKeyLookup = sampleScopedKeyLookup) { client =>
        withSbtClientProxy(client) { cp =>
          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          client.sendBuildStructureUpdate(sampleBuild)
          client.sendEvent(sampleEvent)
          expectNoMsg()

          cp ! WatchSetting(sampleSettingKey,testActor)
          expectMsgType[WatchingSetting] must be equalTo WatchingSetting(sampleSettingKey)
          cp ! WatchTask(sampleTaskKey,testActor)
          expectMsgType[WatchingTask] must be equalTo WatchingTask(sampleTaskKey)
          cp ! SubscribeToBuild(testActor)
          expectMsgType[BuildSubscribed.type] must be equalTo BuildSubscribed
          cp ! SubscribeToEvents(testActor)
          expectMsgType[EventsSubscribed.type] must be equalTo EventsSubscribed

          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey, sampleTaskResult)

          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey1, sampleTaskResult)

          client.sendBuildStructureUpdate(sampleBuild)
          expectMsgType[protocol.MinimalBuildStructure] must be equalTo sampleBuild

          client.sendEvent(sampleEvent)
          expectMsgType[protocol.Event] must be equalTo sampleEvent

          cp ! UpdateClient(client)

          expectMsg(SbtClientProxy.Notifications.Reconnected)

          client.sendWatchEvent(sampleScopedKey,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey, sampleTaskResult)

          client.sendWatchEvent(sampleScopedKey1,sampleTaskResult)
          expectMsgType[WatchEvent] must be equalTo WatchEvent(sampleScopedKey1, sampleTaskResult)

          client.sendBuildStructureUpdate(sampleBuild)
          expectMsgType[protocol.MinimalBuildStructure] must be equalTo sampleBuild

          client.sendEvent(sampleEvent)
          expectMsgType[protocol.Event] must be equalTo sampleEvent
        }
      }
    }
  }
}