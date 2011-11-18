package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
  
trait JSStruct extends Base with EmbeddedControls {
  type JSStruct <: Row[Rep]
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
    newJSStruct(args.map(x => (x._1, x._3).asInstanceOf[(String, Rep[JSStruct] => Rep[_])]): _*).asInstanceOf[Rep[T]]
  def newJSStruct(args: (String, Rep[JSStruct] => Rep[_])*): Rep[JSStruct]
  
  abstract class JSStructOps {
    def selectDynamic[T](field: String): Rep[T]
    def applyDynamic(field: String) = new UpdateOps(field)
    class UpdateOps(field: String) {
      def update(value: Rep[Any]) = updateDynamic(field)(value)
    }
    def updateDynamic[T](field: String)(value: Rep[Any]): Unit
  }
  implicit def jsStructOps(receiver: Rep[JSStruct]): JSStructOps

  // -- workaround --
  // Since JSStruct is abstract, it doesn't have a manifest, but one
  // is required by the LMS core framework. For instance, this will be
  // needed when returning a JSStruct from a function abstraction.
  implicit def jsStructManifest[T <: JSStruct]: Manifest[T] =
    implicitly[Manifest[AnyRef]].asInstanceOf[Manifest[T]]
}

trait JSStructExp extends JSStruct with EffectExp {
  trait JSStruct extends Row[Rep]

  case class JSStructDef(members: List[(String, Exp[Any])]) extends Def[JSStruct]
  case class MemberSelect(receiver: Exp[Any], field: String) extends Def[Any]
  case class MemberUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]
  case class NewVar(e: Exp[Any]) extends Def[Any]
  case class ReadVar(e: Exp[Any]) extends Def[Any]

  private class Self(members: Map[String, Exp[JSStruct] => Exp[Any]]) extends Exp[JSStruct] with Serializable {
    import scala.collection.mutable.{Map => MutMap}
    private val pending: MutMap[String, Exp[JSStruct] => Exp[Any]] = MutMap(members.toSeq: _*)
    private val done: MutMap[String, Exp[Any]] = MutMap.empty
    private def eval(member: String): Exp[Any] = {
      val x = reflectEffect(NewVar(pending(member)(this)))
      pending.remove(member)
      done.update(member, x)
      x
    }
    def apply(member: String): Exp[Any] = done.getOrElseUpdate(member, eval(member))
  }
  class SelfOps(receiver: Self) extends JSStructOps {
    def selectDynamic[T](field: String): Exp[T] =
      reflectEffect(ReadVar(receiver(field))).asInstanceOf[Exp[T]]
    def updateDynamic[T](field: String)(value: Exp[Any]): Unit =
      reflectEffect(MemberUpdate(receiver, field, value))
  }

  class JSStructOpsImpl(val receiver: Exp[JSStruct]) extends JSStructOps {
    def selectDynamic[T](field: String): Exp[T] =
      reflectEffect(ReadVar((MemberSelect(receiver, field)))).asInstanceOf[Exp[T]]
    def updateDynamic[T](field: String)(value: Exp[Any]): Unit =
      reflectEffect(MemberUpdate(receiver, field, value))
  }
  implicit def jsStructOps(receiver: Exp[JSStruct]): JSStructOps = receiver match {
    case receiver: Self => new SelfOps(receiver)
    case receiver =>       new JSStructOpsImpl(receiver)
  }
  def newJSStruct(args: (String, Rep[JSStruct] => (Rep[t] forSome{type t}))*): Exp[JSStruct] = {
    val self = new Self(args.toMap)
    val argNames = args.toList.map(_._1)
    val evalArgs = argNames.map(x => x -> self(x))
    JSStructDef(evalArgs)
  }
}

trait JSGenStruct extends JSGenBase {
  val IR: JSStructExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case JSStructDef(members) => emitValDef(sym,
      members.map({case (name, value) => "'" + name + "' : " + quote(value)}).mkString("{", ",", "}"))
    case MemberSelect(receiver, field) => emitValDef(sym,
      quote(receiver) + "." + field)
    case MemberUpdate(receiver, field, value) => emitValDef(sym,
      quote(receiver) + "." + field + "._ = " + quote(value))
    case NewVar(e) => emitValDef(sym,
      "{_:" + quote(e) + "}")
    case ReadVar(e) => emitValDef(sym,
      quote(e) + "._")
    case _ => super.emitNode(sym, rhs)
  }
}
