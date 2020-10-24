package br.gov.lexml.renderer.strategies

import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.rewriting._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import br.gov.lexml.renderer.terms.XML.AST._
import scala.xml.Elem


import br.gov.lexml.renderer.terms.XML.Conversions._
import br.gov.lexml.renderer.epub._

trait RenderOptions {
  def getUrlFor(href: String): Option[String] =
    Some(if (href startsWith "urn:") { "http://www.lexml.gov.br/urn/" + href } else { href })
  def makeXhtmlId: Option[String ⇒ String] = None
  val sizeLimit: Int = 150000
  def makeXhtmlFileName(n: Int): String = "Section%04d.xhtml".format(n)
}

trait Renderer {
  def render(input: Elem): EpubPackage
}

//final case class Toc(id: String, pos: Int, rotulo: String, subTocs: List[Toc] = List()) extends Attributable

object XhtmlRenderer {
  val baseRenderOptions: RenderOptions = new RenderOptions {}

  def completeHref(base: String) : Strategy = rule ({
    case e: XElem if e.attributes.get("xlink:href").exists(_.startsWith("urn:")) ⇒
      e copy (attributes = e.attributes + ("xlink:href" -> (base + e.attributes("xlink.href"))))
  } : XElem ==> XElem)

  val hasXmlBase: Strategy = rule[XObject] { case e: XElem if e.attributes contains "xml:base" => e : XObject}
  //val removeXmlBase = rule { case e : XElem => e copy (attributes = e.attributes - "xml:base") }

  val propagateXmlBase: Strategy = everywherebu {
    hasXmlBase <* rulefs ({
      case e: XElem ⇒ everywheretd { completeHref(e.attributes.getOrElse("xml:base","")) }
    } : XElem ==> Strategy)
  }

  val spanToRemissao: Strategy = everywheretd {
    rule ({
      case e @ XElem("span", _, attrs, _) if attrs contains "xlink:href" ⇒
        XElem("Remissao", "", attrs.view.filterKeys(_ == "xlink:href").toMap,
          List(e copy (attributes = e.attributes - "xlink:href")))
    } : XElem ==> XElem)
  }

  val dealWithRotuloAndOmissis: Strategy = {
    def f(l: List[XObject]): Option[List[XObject]] = {
      val (rl, r) = l.foldRight(List[XObject](), false) {
        case (XElem("Omissis", _, attrs, _), (r, _)) ⇒
          (XElem("p", "", (attrs.view.filterKeys(_ == "id") + ("class" -> "omissis")).toMap,
            List(XText("....."))) :: r, true)

        case (e1 @ XElem("Rotulo", _, _, _), ((_: XText) :: (e2 @ XElem("Caput", _, _, _)) :: r, _)) ⇒

          val res = e2 copy (contents = e1 :: e2.contents)

          (res :: r, true)
        case (e1 @ XElem("Rotulo", _, _, _), ((e2 @ XElem("Caput", _, _, _)) :: r, _)) ⇒

          ((e2 copy (contents = e1 :: e2.contents)) :: r, true)
        case (e1 @ XElem("Rotulo", _, _, _), ((e2 @ XElem("p", _, _, _)) :: r, _)) ⇒
          ((e2 copy (contents = XElem("span", "", Map("class" -> "rotulo"),
            e1.contents :+ XText(" ")) :: e2.contents)) :: r, true)
        case (e1 @ XElem("Rotulo", _, _, _), ((_: XText) :: (e2 @ XElem("p", _, _, _)) :: r, _)) ⇒
          ((e2 copy (contents = XElem("span", "", Map("class" -> "rotulo"),
            e1.contents :+ XText(" ")) :: e2.contents)) :: r, true)
        case (e, (rl, r)) ⇒ (e :: rl, r)
      }
      if (r) { Some(rl) } else { None }
    }
    reduce {
      rule ({
        case e: XElem if f(e.contents).isDefined ⇒
          f(e.contents).map(ll ⇒ e copy (contents = ll)).get
      } : XElem ==> XElem)
    }
  }

  val simpleDivElements: Set[String] = Set(
    "ParteInicial", "Preambulo",
    "ParteFinal", "LocalDataFecho", "Justificacao",
    "Articulacao", "AgrupamentoHierarquico",
    "Livro", "Parte", "Titulo", "Capitulo", "Secao", "Subsecao",
    "Artigo", "Inciso", "Caput", "Paragrafo", "Alinea", "Item", "Pena")

  val simplePinDivElements: Set[String] = Set(
    "Epigrafe", "Ementa")
  val simplePelements: Set[String] = Set(
    "NomeAgrupador")

  val simpleSpanElements: Set[String] = Set(
    "Rotulo", "Caput")

  def makeToc(idMap: Map[String, String], el: XElem): Seq[NavPoint] = {
    val ignored = """epigrafe|ementa|preambulo|art\d+(-\d+)?_""".r
    def removeIgnored(el: XElem): XElem = rewrite(everywheretd {
      rule ({
        case e: XElem if ignored.findFirstIn(e.attributes.getOrElse("id","")).isDefined ⇒ XText("") : XObject
      } : XObject ==> XObject)
    })(el)

    import br.gov.lexml.renderer.terms.XML.Attributions._    
    def rotulos(e: XElem) = {
      val l = collectl[Either[String, (Int,String)]]({
        case e: XElem if e.attributes.contains("id") ⇒ Left(e.attributes("id"))
        case e: XElem if e.name == "Rotulo" ⇒ Right((0,alltext(e)))
        case e: XElem if e.name == "NomeAgrupador" ⇒ Right((1,alltext(e)))
      })(e)

      val (_, rots) = l.foldLeft[(Option[String], List[(String, String)])]((None, List())) {
        case ((_, l), Left(id)) ⇒ (Some(id), l)
        case ((Some(id), l), Right((0,rotulo))) ⇒ (None, (id, rotulo) :: l)
        case ((None, (id, rotulo) :: l), Right((1,nomeAgrupador))) ⇒ (None, (id, rotulo + " – " + nomeAgrupador) :: l)
        case (x, _) ⇒ x
      }

      def isUnder(id1: String): NavPoint ⇒ Boolean = (np : NavPoint) => np.id match {
        case id2 if !id1.startsWith("art") && id2.startsWith("art") ⇒ true
        case id2 if id2.startsWith(id1 + "_") ⇒ true
        case _ ⇒ false
      }
      val grouped = rots.reverse.foldRight(List[NavPoint]()) {
        case ((id, rotulo), tocs) ⇒
          val isu = isUnder(id)
          val (under, next) = tocs.span(isu)
          val file = TextSectionSource.base + idMap.getOrElse(id,"")
          NavPoint(id, file, rotulo, under: _*) :: next
      }
      grouped
    }

    val el1 = removeIgnored(el)
    rotulos(el1)
  }

  val recordInheritedIds: Strategy = {
    def record(ids: List[String]): XObject ⇒ XObject = {
      case e: XElem ⇒
        val tid = e.attributes.get("id").toList
        val ids2 = ids ++ tid
        val contents2 = e.contents.map(record(ids2))
        e copy (attributes = e.attributes + ("inheritedIds" -> ids2.mkString(",")),
          contents = contents2)
      case x ⇒ x
    }
    rulef {
      case x: XObject ⇒ record(List())(x)
    }
  }

  lazy val explodeStrategy: Strategy = {
    def explode: XObject ⇒ List[XObject] = {
      case e: XElem if e.name == "div" ⇒
        val (cl1, cl2) = e.contents.span {
          case e: XElem ⇒ e.name == "p"
          case _ ⇒ true
        }
        val e1 = e copy (contents = cl1)
        e1 :: cl2.flatMap(explode)
      case x ⇒ List(x)
    }
    rule ({
      case e: XElem ⇒
        e copy (contents = e.contents.flatMap(explode)) : XObject
    } : XObject ==> XObject)
  }

  def splitBySizeLimit(maxSize: Int, l: List[XElem]): List[List[XElem]] = {
    def size(e: XElem) = xelem2elem(e).toString.length
    val (_, rl) = l.foldRight[(Int, List[List[XElem]])]((0, List())) {
      case (e, (_, Nil)) ⇒ (size(e), List(List(e)))
      case (e, (curSize, l @ ll :: r)) ⇒
        val len = size(e)
        val curSize2 = curSize + len
        if (curSize2 >= maxSize) {
          (len, List(e) :: l)
        } else {
          (curSize2, (e :: ll) :: r)
        }
    }
    rl
  }

  def getIds: XElem ⇒ Set[String] = collects {
    case e: XElem if e.attributes.contains("id") ⇒ e.attributes("id")
  }

  def rewriteHrefs(fname: Option[String], idMap: Map[String, String]): XElem ⇒ XElem = {
    val lookup1 = (href: String) ⇒ idMap.get(href.substring(1)).map(fname ⇒ fname + href).getOrElse(href)
    def lookup2(thisfname: String) = (href: String) ⇒
      idMap.get(href.substring(1)).map({
        case fname if fname == thisfname ⇒ href
        case fname ⇒ fname + href
      }).getOrElse(href)
    val lookup = fname.map(lookup2).getOrElse(lookup1)

    rewrite {
      everywheretd {
        rule ({
          case e: XElem if e.attributes.contains("href") ⇒
            val oldHref = e.attributes("href")
            val newHref = lookup(oldHref)
            e copy (attributes = e.attributes + ("href" -> newHref)) : XObject
        } : XObject ==> XObject)
      }
    }
  }

  def makeRenderer(opts: RenderOptions = baseRenderOptions): Renderer = {

    val fixIds = everywheretd {
      opts.makeXhtmlId.map(f ⇒ rule ({
        case e: XElem if e.attributes contains "id" ⇒
          e copy (attributes = e.attributes + ("id" -> f(e.attributes("id")))) : XObject
      } : XObject ==> XObject)).getOrElse(fail)
    }

    val bottomUp1 = everywherebu {
      rule ({
        case XElem(name, _, attrs, contents) if simpleDivElements contains name ⇒
          XElem("div", "", Map("class" -> name.toLowerCase) ++ attrs.view.filterKeys(_ == "id"), contents) : XObject
        case XElem(name, _, attrs, contents) if simplePelements contains name ⇒
          XElem("p", "", Map("class" -> name.toLowerCase) ++ attrs.view.filterKeys(_ == "id"), contents) : XObject
        case XElem(name, _, attrs, contents) if simplePinDivElements contains name ⇒
          XElem("div", "", Map("class" -> name.toLowerCase) ++ attrs.view.filterKeys(_ == "id"),
            List(XElem("p", "", Map(), contents))) : XObject
        case XElem(name, _, attrs, contents) if simpleDivElements contains name ⇒
          XElem("span", "", Map("class" -> name.toLowerCase) ++ attrs.view.filterKeys(_ == "id"), contents) : XObject

        case XElem("Remissao", _, attrs, contents) ⇒
          attrs.get("xlink:href") match {
            case None ⇒ XElem("span", "", Map("class" -> "remissao"), contents) : XObject
            case Some(href) ⇒ opts.getUrlFor(href) match {
              case None ⇒ XElem("span", "", Map("class" -> "remissao"), contents) : XObject
              case Some(url) ⇒
                XElem("a", "", Map("href" -> url, "class" -> "remissao"), contents) : XObject

            }
          }
        case XElem("Rotulo", _, _, contents) ⇒ XElem("p", "", Map("class" -> "rotulo"), contents) : XObject
        case XElem("Norma", _, _, contents) ⇒ XElem("body", "", Map(), contents) : XObject
        /*        case XElem("Identificacao", _, attrs, _) ⇒ XElem("title", "", Map(), List(XText(attrs.get("URN").get)))
        case XElem("Metadado", _, _, contents) ⇒ XElem("head", "", Map(),
          XElem("link", "", Map("type" -> "text/css", "rel" -> "stylesheet", "href" -> "lexml.css"), List()) ::
            contents)
        case XElem("LexML", _, _, contents) ⇒
          XElem("html", "", Map("xmlns" -> "http://www.w3.org/1999/xhtml", "xml:lang" -> "pt"), contents flatMap {
            case e: XElem if e.name == "ProjetoNorma" ⇒ e.contents
            case x ⇒ List(x)
          })*/
      } : XObject ==> XObject)
    }

    val mainStrategy = fixIds <*
      propagateXmlBase <*
      spanToRemissao <*
      dealWithRotuloAndOmissis <*
      bottomUp1 <*
      explodeStrategy

    //def extractArticTopElems(e: Elem) = e \\ "articulacao" \ "*"

    (e: Elem) => {

      val urn = (e \\ "Metadado" \\ "Identificacao" \\ "@URN").
        headOption.map(_.text).getOrElse("")

      val epigrafe = (e \\ "Epigrafe").headOption.map(_.text).getOrElse("")

      val norma: Elem = (e \\ "Norma").toList.collect({ case x: Elem ⇒ x }).head

      val xe = elem2xelem(norma)

      val le = rewrite(mainStrategy)(xe) match {
        case e: XElem ⇒ e.contents.collect({ case e: XElem ⇒ e })
      }
      val splitted = splitBySizeLimit(opts.sizeLimit, le)
      val fl = for {
        (els, n) ← splitted.zipWithIndex
      } yield {
        val body = XElem("body", "", Map(), els)
        val ids = getIds(body)
        (n, ids, XElem("body", "", Map(), els))
      }
      val idMap = (
        for {
          (n, ids, _) ← fl
          id ← ids
        } yield (id, TextSectionSource.fname(n))).toMap
      val cssFiles = List(
        CssResourceSource("css", "epub_root/Style/lexml.css"))
      val textFiles =
        for {
          (n, _, body) ← fl
        } yield {
          TextSectionSource(n,
            epigrafe,
            urn,
            xelem2elem(rewriteHrefs(Some(TextSectionSource.fname(n)), idMap)(body)),
            cssFiles: _*)
        }
      val ncx = Ncx("ncx", epigrafe, makeToc(idMap, xe): _*)

      val dcInfo = DcInfo(titulo = Some(epigrafe), autor = Some("LexML Epub Renderer 1.1.0"), urn = Some(urn))
      val cont = Content(textFiles, cssFiles, ncx, dcInfo)

      new EpubPackage {
        override val content: Content = cont
      }
    }
  }
}