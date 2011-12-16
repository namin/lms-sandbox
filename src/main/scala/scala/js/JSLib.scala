package scala.js

import scala.virtualization.lms.common._

trait JSLib extends JSProxyBase with JSLiteral {
  val window: Rep[Window]
  trait Window
  trait WindowOps {
    def setTimeout(func: Rep[Unit => Unit], delay: Rep[Int]): Rep[Int]
  }
  implicit def repToWindowOps(x: Rep[Window]): WindowOps =
    repProxy[Window,WindowOps](x)

  val json: Rep[JSON]
  trait JSON
  trait JSONOps {
    def stringify(literal: Rep[JSLiteral]): Rep[String]
    def parse[T <: JSLiteral](data: Rep[String]): Rep[T]
  }
  implicit def repToJSONOps(x: Rep[JSON]): JSONOps =
    repProxy[JSON,JSONOps](x)
}

trait JSLibExp extends JSLib with JSProxyExp with JSLiteralExp {
  case object WindowVar extends Exp[Window]
  val window = WindowVar

  case object JSONVar extends Exp[JSON]
  val json = JSONVar
}

trait JSGenLib extends JSGenProxy with JSGenLiteral {
  val IR: JSLibExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case WindowVar => "window"
    case JSONVar => "JSON"
    case _ => super.quote(x)
  }
}
