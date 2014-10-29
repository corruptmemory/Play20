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

case class FromBuilder(message:Any)
case class FromClient(message:Any)
case object Created
case object ReceivedSubscription

object ProbeProxy {
  def defaultSink[T](probe:ActorRef,msg:Any,wrapper:Any => T,sender:ActorRef):Unit = {
    probe.tell(wrapper(msg),sender)
  }

  def builderSink[T](probe:ActorRef,msg:Any,wrapper:Any => T,sender:ActorRef):Unit = {
    println(wrapper(msg))
    msg match {
      case SbtClientBuilder.Subscription(_) =>
        probe.tell(wrapper(ReceivedSubscription),sender)
      case _ =>
    }

  }


  def emptySink[T](probe:ActorRef,msg:Any,wrapper:Any => T,sender:ActorRef):Unit = {}

  def printingSink[T](probe:ActorRef,msg:Any,wrapper:Any => T,sender:ActorRef):Unit = {
    println(wrapper(msg))
  }
}

class ProbeProxy[T](underlyingProps:Props, probe:ActorRef, wrapper:Any => T, sink:(ActorRef,Any,Any => T,ActorRef) => Unit = ProbeProxy.defaultSink _) extends Actor with ActorLogging {
  val underlying = context.actorOf(underlyingProps)

  def receive:Receive = {
    probe ! wrapper(Created)

    {
      case Terminated(_) =>
        context stop self
      case msg =>
        sink(probe,msg,wrapper,sender)
        underlying.tell(msg,sender)
    }
  }
}

object SbtConnectionProxySpec {
  def builderProps(newClient:SbtConnectionProxy.NewClient, notificationSink:ActorRef, probe:ActorRef,sink:(ActorRef,Any,Any => FromBuilder,ActorRef) => Unit = ProbeProxy.defaultSink _) = Props(new ProbeProxy(Props(new SbtClientBuilder(newClient,notificationSink)),probe, FromBuilder.apply, sink))
  def clientProps(client:SbtClient, probe:ActorRef,sink:(ActorRef,Any,Any => FromClient,ActorRef) => Unit = ProbeProxy.defaultSink _) = Props(new ProbeProxy(Props(new SbtClientProxy(client,global)),probe, FromClient.apply, sink))
}

class SbtConnectionProxySpec extends Specification with NoTimeConversions {
   sequential
   import SbtConnectionProxySpec._, SbtConnectionProxy._

  "A SbtConnectionProxy" should {
    "shutdown gracefully" in new AkkaTestKitHelper() {
      withFakeEverything() { (conn,client) =>
        val cp = system.actorOf(Props(new SbtConnectionProxy(conn, (_ => client),(nc,ns) => builderProps(nc,ns,testActor), c => clientProps(c,testActor), global)))
        cp ! Close(testActor)
        expectMsgType[Closed.type] must be equalTo Closed
      }
    }
    "create a builder and client" in new AkkaTestKitHelper() {
      withFakeEverything() { (conn,client) =>
        val cp = system.actorOf(Props(new SbtConnectionProxy(conn, (_ => client),(nc,ns) => builderProps(nc,ns,testActor, ProbeProxy.builderSink _), c => clientProps(c,testActor, ProbeProxy.printingSink _), global)))
        cp ! NewClient(testActor)
        expectMsgType[FromBuilder] must be equalTo FromBuilder(Created)
        expectMsgType[FromBuilder] must be equalTo FromBuilder(ReceivedSubscription)
        conn.sendValue(FakeSbtConnector.Channel)
        expectMsgAllClassOf(classOf[NewClientResponse.Connected],classOf[FromClient]) must contain (FromClient(Created))
        cp ! Close(testActor)
        expectMsgType[Closed.type] must be equalTo Closed
      }
    }
  }
}