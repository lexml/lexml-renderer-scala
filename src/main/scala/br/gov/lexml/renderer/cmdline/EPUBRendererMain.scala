package br.gov.lexml.renderer.cmdline

import br.gov.lexml.renderer.strategies.RenderOptions
import org.apache.commons.io.{FileUtils, IOUtils}
import org.slf4j.LoggerFactory
import scopt.OParser

import java.io.File
import scala.annotation.static
import scala.io.Source
import scala.util.Using

case class Config(
                 urlForURNPattern : String = "https://normas.leg.br/?urn={URN}",
                 idPattern : Option[String] = None,
                 sizeLimit_ : Int = 150000,
                 addLinks : Boolean = true,
                 input : Option[File] = None,
                 output : Option[File] = None,
                 debug : Boolean = false
                 ) extends RenderOptions:
  override def getUrlFor(href: String): Option[String] =
    Option.when(addLinks) {
      if href.startsWith("urn:") then
        urlForURNPattern.replaceAll("\\{URN}", href)
      else href
    }

  override val makeXhtmlId: Option[String => String] =
      idPattern.map { pat =>
        (id : String) => pat.replaceAll("\\{ID}",id)
      }

  override val sizeLimit: Int = sizeLimit_

object Config:
  private val programVersion : Option[String] =
    Option(classOf[Config].getPackage.getImplementationVersion)
      .orElse(
        try {
          import scala.xml.*
          (XML.loadFile("pom.xml") \ "version")
            .headOption
            .map(_.text.trim)
        } catch {
          case _ : Exception => None
        }
      )
  private val builder = OParser.builder[Config]
  private val default = Config()
  val parser: OParser[Unit, Config] = {
    import builder.*
    OParser.sequence(
      programName("lexml-epub-renderer"),
      head("lexml-epub-renderer",programVersion.getOrElse("undetermined")),
      version('v',"version"),
      help('h',"help")
        .text(
          """This program renders a LexML Brasil's XML file in EPUB format.""".stripMargin),
      opt[File]('i',"in")
        .valueName("<file> | '_'")
        .text("Read input from <file> or from stdin ('_') (default)")
        .validate { f =>
          if f.getPath == "_" then success
          else if !f.exists then failure(s"Input file $f does not exists!")
          else if !f.canRead then failure(s"Input file $f cannot be read!")
          else success
        }
        .action { (x,c) =>
          c.copy(input =
            if(x.getPath == "_") None else Some(x)
          )
        },
      opt[File]('o',"out")
        .valueName("<file> | '_'")
        .text("Write output to <file> or to stdout ('_') (default)")
        .action { (x,c) =>
          c.copy(output =
            if(x.getPath == "_") None else Some(x)
          )
        },
      opt[Int]("size-limit")
        .valueName("<limit>")
        .text(s"EPUB segment file size limit in bytes (default: ${default.sizeLimit_})")
        .action { (x,c) => c.copy(sizeLimit_ =  x) },
      opt[String]("link-pattern")
        .valueName("<pattern>")
        .text(s"""
              |Pattern to be used when generating external links. Occurrences of the
              |string '{URN}' shall be replaced with the recognized URN.
              |The default pattern is '${default.urlForURNPattern}'""".stripMargin)
        .validate { pat =>
          if pat.contains("{URN}") then success
          else failure(s"pattern $pat must contain at least one occurrence of '{URN}'")
        },
      opt[Unit]("no-links")
        .text("Suppress the creation of external links")
        .action{ (_,c) => c.copy(addLinks = false) },
      opt[Unit]('d',"debug")
        .text("Display debug messages")
        .action { (_,c) => c.copy(debug = true)}
    )

  }

class EPUBRendererMain

object EPUBRendererMain:
  @static def main(args : Array[String]) : Unit =
    OParser.parse(Config.parser,args, Config()) match {
      case Some(config) =>
        import java.io._
        import br.gov.lexml.renderer.strategies.{XML => _, _}
        val logLevel = if config.debug then "DEBUG" else "WARN"
        sys.props.put("log.level",logLevel)
        sys.props.put("logback.configurationFile", "lexml-renderer-scala/logback.xml")
        val logger = LoggerFactory.getLogger("main")

        val renderer = XhtmlRenderer.makeRenderer(config)
        logger.info("Reading LexML file")
        val xml = config.input.match {
          case None => scala.xml.XML.load(System.in)
          case Some(f) => scala.xml.XML.loadFile(f)
        }
        logger.info("Rendering EPUB")
        val epub = renderer.render(xml)
        logger.info(s"Writing EPUB")
        config.output.match {
          case None =>
            epub.writeZip(System.out)
          case Some(f) =>
            Using(BufferedOutputStream(FileOutputStream(f)))(epub.writeZip(_))
        }
        logger.info("Done")
      case None =>
    }