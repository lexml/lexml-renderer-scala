package br.gov.lexml.renderer.terms

import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution.attr

object XML {

  object AST {
    abstract sealed class XObject extends Product {
      final override def toString: String = Conversions.xobject2node(this).toString
    }

    case class XElem(name: String,
      prefix: String = "",
      attributes: Map[String, String],
      contents: List[XObject]) extends XObject
    case class XText(text: String) extends XObject
  }

  import AST._

  object Conversions {

    import scala.xml.{ Elem, Node, Text }

    def elem2xelem(e: Elem): XElem =
      XElem(e.label, Option(e.prefix).getOrElse(""), e.attributes.asAttrMap, e.child.toList.flatMap(node2xobject))

    def node2xobject(n: Node): Option[XObject] = n match {
      case e: Elem ⇒ Some(elem2xelem(e))
      case t: Text ⇒ Some(XText(t.text))
      case _ ⇒ None
    }

    import scala.xml.{ MetaData, PrefixedAttribute, UnprefixedAttribute, Null, TopScope }
    def attrMapToMetadata(m: Map[String, String]): MetaData = {
      val al = for {
        (k, v) ← m.toList
      } yield {
        k.split(":").toList match {
          case List(prefix, name) ⇒ (next: MetaData) ⇒ new PrefixedAttribute(prefix, name, v, next)
          case List(name) ⇒ (next: MetaData) ⇒ new UnprefixedAttribute(name, v, next)
        }
      }
      al.foldRight[MetaData](Null)((x, y) ⇒ x(y))
    }
    def nullIfEmpty(s: String): String = s match {
      case "" ⇒ null
      case _ ⇒ s
    }

    def xelem2elem(e: XElem): Elem = Elem(nullIfEmpty(e.prefix), e.name, attrMapToMetadata(e.attributes), TopScope, false, e.contents.map(xobject2node): _*)

    def xobject2node(o: XObject): Node = o match {
      case e: XElem ⇒ xelem2elem(e)
      case t: XText ⇒ Text(t.text)
    }
  }

  import Conversions._

  object Implicits {
    implicit val e2xe: scala.xml.Elem ⇒ XElem = elem2xelem
    implicit val xe2e: XElem ⇒ scala.xml.Elem = xelem2elem
  }

  object Attributions  {
    lazy val alltext: XObject ⇒ String =
      attr({
        case e: XElem ⇒ e.contents.map(_ -> alltext).mkString("")
        case t: XText ⇒ t.text
      }: XObject ==> String)
  }

}

