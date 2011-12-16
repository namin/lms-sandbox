package scala.js

import scala.virtualization.lms.common._

trait Doms extends JSProxyBase {
  trait Element
  trait ElementOps {
    def getElementById(id: Rep[String]): Rep[Element]
  }
  trait Canvas extends Element
  trait CanvasOps extends ElementOps {
    def getContext(context: Rep[String]): Rep[Context]
  }
  trait Context
  trait ContextOps {
    def save(): Rep[Unit]
    def lineTo(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def scale(x1: Rep[Double], x2: Rep[Double]): Rep[Unit]
    def rotate(x: Rep[Double]): Rep[Unit]
    def restore(): Rep[Unit]
    def translate(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def moveTo(x: Rep[Int], y: Rep[Int])
    def closePath(): Rep[Unit]
    def stroke(): Rep[Unit]
  }
  trait AsRep {
    def as[T]: Rep[T]
  }
  val document: Rep[Element]
  implicit def repToElementOps(x: Rep[Element]): ElementOps = repProxy[Element,ElementOps](x)
  implicit def repTocanvasOps(x: Rep[Canvas]): CanvasOps = repProxy[Canvas,CanvasOps](x)
  implicit def repToContextOps(x: Rep[Context]): ContextOps = repProxy[Context,ContextOps](x)
  implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}

trait DomsExp extends Doms with JSProxyExp {
  case object DocumentVar extends Exp[Element]
  val document = DocumentVar
}

trait GenDoms extends JSGenProxy {
  val IR: DomsExp
  import IR._
  override def quote(x: Exp[Any]) : String = x match {
    case DocumentVar => "document"
    case _ => super.quote(x)
  }
}
