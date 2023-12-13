package br.gov.lexml.renderer.strategies

import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.*
import org.bitbucket.inkytonik.kiama.==>
import br.gov.lexml.renderer.terms.XML.AST.*

/**
 * Estratégias de transformação do XHTML de entrada
 *
 * @author João Rafael Moraes Nicola
 */
object XML:
  lazy val matchBlockElement : Strategy =
    val blockElements = Set("p", "ol", "li", "table", "thead", "tbody", "tfoot",
      "tr", "td")
    rule ({
      case e : XElem if blockElements contains e.name => e
    } : XElem ==> XElem)
  
  /**
   * Filtra os elementos do XHTML de entrada, mantendo apenas os elementos que interessam ao processo de conversão.
   *
   * A estratégia reescreve, de baixo para cima, as listas de de elementos presentes na árvore (o atributos `contents`
   * de objetos `XElem`), mantendo os elementos cujos rotulos estão definidos na variável `keepIntactElements`,
   * substituindo os elementos cujos rótulos estão definidos na variável `explodedElements` e removendo interamente os
   * outros.
   */
  lazy val filterAndExplode: Strategy =
    /**
     * Lista de elementos a serem preservados.
     */
    val elementsToKeep = Set("p", "ol", "li", "table", "thead", "tbody", "tfoot",
      "tr", "td", "span", "b", "i", "sup", "sub")

    /**
     * Lista de elementos a serem renomeados para <p>
     */
    val elementsToRename = Set("blockquote", "h1", "h2", "h3", "h4")

    /**
     * Lista de elementos a serve explodidos (substituídos por seus conteúdos).
     */
    val elementsToExplode = Set("div", "body", "html")

    /**
     * Elementos inline
     */
    val inlineElements = Set("span", "sup", "sub", "i", "b")

    /**
     * Envelopa sequências de elementos inline em conteúdos mistos (inline e bloco)
     * de elementos de bloco
     */
    def wrapAnonymousInlineInP(el: List[XObject]): List[XObject] =
      def isInline(x: XObject) = x match {
        case _: XText => true
        case e: XElem => inlineElements.contains(e.name)
      }
      def wrapIfNeeded: ((List[XObject], List[XObject])) => List[XObject] = {
        case (Nil, notInline) => notInline
        case (inline, Nil) => inline
        case (inline, notInline) => XElem("p", "", Map(), inline) :: notInline
      }
      wrapIfNeeded(el.foldRight((List[XObject](), List[XObject]())) {
        case (o, (inline, notInline)) if isInline(o) => (o :: inline, notInline)
        case (o, (Nil, notInline)) => (Nil, o :: notInline)
        case (o, (inline, notInline)) => (Nil, o :: XElem("p", "", Map(), inline) :: notInline)
      })
    end wrapAnonymousInlineInP

    everywherebu {
      rule ({
        case e: XElem if elementsToExplode.contains(e.name) =>
          e copy (contents = wrapAnonymousInlineInP(e.contents))
        case l: List[XObject] if l.nonEmpty => l flatMap {
          case e: XElem if elementsToKeep.contains(e.name) => List(e)
          case e: XElem if elementsToRename.contains(e.name) => List(e copy (name = "p"))
          case e: XElem if elementsToExplode.contains(e.name) => e.contents
          case _: XElem => List()
          case x => List(x)
        }
        case x => x
      } : Any ==> Any)
    } <* rule ({
      case x: List[XObject] => wrapAnonymousInlineInP(x) : Any
    } : Any ==> Any)
  end filterAndExplode
end XML