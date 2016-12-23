package com.twitter.finagle.integration

import com.twitter.concurrent.AsyncQueue
import com.twitter.finagle._
import com.twitter.finagle.exp._
import com.twitter.finagle.http.codec.HttpClientDispatcher
import com.twitter.finagle.exp.mysql
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.transport.{QueueTransport, Transport}
import com.twitter.util._
import com.twitter.util.TimeConversions._
import org.junit.runner.RunWith
import org.mockito.Mockito.when
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar

/**
  * We want client session statuses to reflect the status of their underlying transports.
  */
@RunWith(classOf[JUnitRunner])
class ClientSessionTest extends FunSuite with MockitoSugar {

  def testSessionStatus[Req, Rep](
      name: String,
      sessionFac: (Transport[Req, Rep]) => () => Status
  ): Unit = {

    test(s"$name: session status reflects underlying transport") {
      val clientToServer = new AsyncQueue[Req]
      val serverToClient = new AsyncQueue[Rep]

      val transport: Transport[Req, Rep] =
        new QueueTransport(writeq = clientToServer, readq = serverToClient)

      val sessionStatus = sessionFac(transport)
      assert(sessionStatus() == Status.Open)

      Await.ready(transport.close(), 5.seconds)
      assert(sessionStatus() == Status.Closed)
    }
  }

  testSessionStatus[mux.transport.Message, mux.transport.Message](
    "mux-transport", {
      tr: Transport[mux.transport.Message, mux.transport.Message] =>
        val session: mux.ClientSession =
          new mux.ClientSession(tr,
                                mux.FailureDetector.NullConfig,
                                "test",
                                NullStatsReceiver)
        () =>
          session.status
    }
  )

  testSessionStatus[mux.transport.Message, mux.transport.Message](
    "mux-dispatcher", {
      tr: Transport[mux.transport.Message, mux.transport.Message] =>
        val dispatcher = mux.ClientDispatcher.newRequestResponse(tr)
        () =>
          dispatcher.status
    }
  )

  testSessionStatus(
    "http-transport", { tr: Transport[Any, Any] =>
      val manager = mock[http.codec.ConnectionManager]
      when(manager.shouldClose).thenReturn(false)
      val wrappedT = new http.HttpTransport(tr, manager)
      () =>
        wrappedT.status
    }
  )

  testSessionStatus(
    "http-dispatcher", { tr: Transport[Any, Any] =>
      val dispatcher = new HttpClientDispatcher(tr)
      () =>
        dispatcher.status
    }
  )

  class MyClient extends com.twitter.finagle.Memcached.Client {
    def newDisp(transport: Transport[In, Out]): Service[In, Out] =
      super.newDispatcher(transport)
  }

  testSessionStatus(
    "memcached-dispatcher", {
      tr: Transport[memcached.protocol.Command, memcached.protocol.Response] =>
        val cl: MyClient = new MyClient
        val svc = cl.newDisp(tr)
        () =>
          svc.status
    }
  )

  testSessionStatus(
    "mysql-dispatcher", {
      tr: Transport[mysql.transport.Packet, mysql.transport.Packet] =>
        val handshake = mysql.Handshake(Some("username"), Some("password"))
        val dispatcher = new mysql.ClientDispatcher(tr, handshake)
        () =>
          dispatcher.status
    }
  )
}
