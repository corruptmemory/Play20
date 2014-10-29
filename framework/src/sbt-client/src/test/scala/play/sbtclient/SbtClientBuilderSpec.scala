/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package play.sbtclient

import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

import akka.actor._
import akka.testkit._
import concurrent.ExecutionContext.Implicits.global
import sbt.client.{Subscription,SbtConnector,SbtClient,Interaction, SettingKey, TaskKey}

object SbtClientBuilderSpec {
  val fakeSubscription:Subscription = new Subscription {
    def cancel():Unit = {}
  }

  val reconnectableErrorResult = SbtClientBuilder.Error(true,"reconnectable")
  val notReconnectableErrorResult = SbtClientBuilder.Error(false,"not reconnectable")
}

class SbtClientBuilderSpec extends Specification with NoTimeConversions {
   // sequential
   import SbtClientBuilderSpec._, SbtClientBuilder._

  "A SbtClientBuilder" should {
    "deliver a new client connection" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        val nc = SbtConnectionProxy.NewClient(testActor)
        val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
        cb ! Subscription(fakeSubscription)
        cb ! Client(client)
        expectMsgType[Result.Success] must be equalTo Result.Success(nc,client,fakeSubscription,cb)
        system stop cb
      }
    }
    "deliver an error" in new AkkaTestKitHelper() {
      val nc = SbtConnectionProxy.NewClient(testActor)
      val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
      cb ! Subscription(fakeSubscription)
      cb ! reconnectableErrorResult
      expectNoMsg()
      cb ! notReconnectableErrorResult
      expectMsgType[Result.Failure] must be equalTo Result.Failure(nc,fakeSubscription,notReconnectableErrorResult)
    }
    "handle errors after connect" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        val nc = SbtConnectionProxy.NewClient(testActor)
        val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
        cb ! Subscription(fakeSubscription)
        cb ! Client(client)
        expectMsgType[Result.Success] must be equalTo Result.Success(nc,client,fakeSubscription,cb)
        cb ! reconnectableErrorResult
        expectNoMsg()
        cb ! notReconnectableErrorResult
        expectMsgType[Disconnect] must be equalTo Disconnect(fakeSubscription,cb)
      }
    }
    "handle reconnect" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        val nc = SbtConnectionProxy.NewClient(testActor)
        val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
        cb ! Subscription(fakeSubscription)
        cb ! Client(client)
        expectMsgType[Result.Success] must be equalTo Result.Success(nc,client,fakeSubscription,cb)
        cb ! Client(client)
        expectMsgType[Reconnect] must be equalTo Reconnect(fakeSubscription,client,cb)
        cb ! reconnectableErrorResult
        expectNoMsg()
        cb ! Client(client)
        expectMsgType[Reconnect] must be equalTo Reconnect(fakeSubscription,client,cb)
        cb ! notReconnectableErrorResult
        expectMsgType[Disconnect] must be equalTo Disconnect(fakeSubscription,cb)
      }
    }
    "not deliver anything after disconnect" in new AkkaTestKitHelper() {
      withFakeSbtClient() { client =>
        val nc = SbtConnectionProxy.NewClient(testActor)
        val cb = system.actorOf(Props(new SbtClientBuilder(nc, testActor)))
        cb ! Subscription(fakeSubscription)
        cb ! Client(client)
        expectMsgType[Result.Success] must be equalTo Result.Success(nc,client,fakeSubscription,cb)
        cb ! Client(client)
        expectMsgType[Reconnect] must be equalTo Reconnect(fakeSubscription,client,cb)
        cb ! reconnectableErrorResult
        expectNoMsg()
        cb ! Client(client)
        expectMsgType[Reconnect] must be equalTo Reconnect(fakeSubscription,client,cb)
        cb ! notReconnectableErrorResult
        expectMsgType[Disconnect] must be equalTo Disconnect(fakeSubscription,cb)
        cb ! reconnectableErrorResult
        expectNoMsg()
        cb ! Client(client)
        expectNoMsg()
        cb ! notReconnectableErrorResult
        expectNoMsg()
      }
    }
  }
}