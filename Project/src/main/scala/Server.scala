import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route

import scala.language.postfixOps
//import akka.http.javadsl.server.Route
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Directives.{complete, path}
import akka.stream.ActorMaterializer
import patmat.Huffman

import scala.concurrent.ExecutionContext

object Server extends App {
  val host = "0.0.0.0"
  val port = 9000
  implicit val system: ActorSystem = ActorSystem("helloworld")
  implicit val executor: ExecutionContext = system.dispatcher
  val materializer: ActorMaterializer = ActorMaterializer()

  var send = Huffman.createCodeTree("i am the amazing moriya malka".toList)
  var encoding = Huffman.quickEncode(send)("moriya malka".toList)
  var decoding = Huffman.decode(send, encoding)

  val itemRoutes: Route =
    Directives.concat(
      path("encode") {
        Directives.get {
          println("enter msg")
          val msg = scala.io.StdIn.readLine()
          var encoding = Huffman.quickEncode(send)(msg.toList)

          complete("Hi Elisheva! encode " + encoding.toString() + msg.toString())
        }
      },
      path("decode") {
        Directives.get {
          val msg = scala.io.StdIn.readLine()
          var decoding = Huffman.decode(send, encoding)
          complete("Hi Elisheva! decode " + decoding.toString())
        }
      }
    )

  //  val route =
  //    parameters("msg", "codeTree".optional) { (msg, codeTree) =>
  //      val codeTreeStr = codeTree.getOrElse("<undefined>")
  //      complete(s"The msg is '$msg' and the codeTree is '$codeTree'")
  //    }
  //
  //  // tests:
  //  Get("/?msg=blue&codeTree=red") ~> route ~> check {
  //    responseAs[String] shouldEqual "The color is 'blue' and the background is 'red'"
  //  }
  //  Get("/?msg=blue") ~> route ~> check {
  //    responseAs[String] shouldEqual "The color is 'blue' and the background is '<undefined>'"
  //  }


  Http().bindAndHandle(itemRoutes, host, port)

}

