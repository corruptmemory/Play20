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
import com.typesafe.config.{ ConfigFactory, Config }

object SbtConnectionProxySpec {
  def builderProps(newClient:SbtConnectionProxy.NewClient, notificationSink:ActorRef) = Props(new SbtClientBuilder(newClient,notificationSink))
  def clientProps(client:SbtClient,notificationSink:SbtClientProxy.Notification => Unit  = _ => ()) = Props(new SbtClientProxy(client,global,notificationSink))

  abstract class SbtConnectionProxySpecHelper(_system: ActorSystem) extends AkkaTestKitHelper(_system) {
    import SbtConnectionProxy._
    import concurrent.ExecutionContext

    def this(config: Config) = this(ActorSystem(AkkaTestKitHelper.randomActorSystemName, config))
    def this() = this(AkkaTestKitHelper.config)

    def withSbtConnectionProxy[T](conn:SbtConnector,
                                  client:SbtClient,
                                  notificationSink:SbtConnectionProxy.Notification => Unit  = _ => (),
                                  testClosed:Boolean = true,
                                  builderProps:(SbtConnectionProxy.NewClient, ActorRef) => Props = SbtConnectionProxySpec.builderProps _,
                                  clientProps:SbtClient => Props = SbtConnectionProxySpec.clientProps(_))(body:ActorRef => T)(implicit ex:ExecutionContext):T = {
      import SbtConnectionProxy._
      val cp = system.actorOf(Props(new SbtConnectionProxy(conn, (_ => client),builderProps,clientProps, ex, notificationSink)))
      try {
        body(cp)
      } finally {
        cp ! Close(testActor)
        expectMsgType[Closed.type] must be equalTo Closed
        if (testClosed) client.isClosed must be equalTo true
      }
    }
  }
}

class SbtConnectionProxySpec extends Specification with NoTimeConversions {
   // sequential
   import SbtConnectionProxySpec._, SbtConnectionProxy._

  "A SbtConnectionProxy" should {
    "shutdown gracefully" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client, testClosed = false) { cp => }
      }
    }
    "create a new client" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client,x => testActor ! x) { cp =>
          cp ! NewClient(testActor)
          expectMsg(Notifications.BuilderAwaitingChannel)
          conn.sendValue(FakeSbtConnector.Channel)
          val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
        }
      }
    }
    "propogate reconnect" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client,x => testActor ! x, clientProps = clientProps(_,x => testActor ! x)) { cp =>
          cp ! NewClient(testActor)
          expectMsg(Notifications.BuilderAwaitingChannel)
          conn.sendValue(FakeSbtConnector.Channel)
          val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
          conn.sendValue(FakeSbtConnector.Channel)
          expectMsg(SbtClientProxy.Notifications.Reconnected)
        }
      }
    }
    "absorb recoverable errors" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client,x => testActor ! x) { cp =>
          cp ! NewClient(testActor)
          expectMsg(Notifications.BuilderAwaitingChannel)
          conn.sendValue(FakeSbtConnector.Channel)
          val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
          conn.sendValue(FakeSbtConnector.Error(true,"recoverable"))
          expectNoMsg()
        }
      }
    }
    "clean up after client closure" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client,x => testActor ! x) { cp =>
          cp ! NewClient(testActor)
          expectMsg(Notifications.BuilderAwaitingChannel)
          conn.sendValue(FakeSbtConnector.Channel)
          val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
          proxy ! SbtClientProxy.Close(testActor)
          expectMsgAllClassOf(SbtClientProxy.Closed.getClass(),Notifications.CleanedUpAfterClientClosure.getClass())
        }
      }
    }
    "clean up after unrecoverable error" in new SbtConnectionProxySpecHelper() {
      withFakeEverything() { (conn,client) =>
        withSbtConnectionProxy(conn,client,x => testActor ! x) { cp =>
          cp ! NewClient(testActor)
          expectMsg(Notifications.BuilderAwaitingChannel)
          conn.sendValue(FakeSbtConnector.Channel)
          val NewClientResponse.Connected(proxy) = expectMsgType[NewClientResponse.Connected]
          conn.sendValue(FakeSbtConnector.Error(false,"unrecoverable"))
          expectMsg(Notifications.ClientClosedDueToDisconnect)
        }
      }
    }
  }
}