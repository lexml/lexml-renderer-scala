package test
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
import java.io._

object Check1 {

  def main(args: Array[String]): Unit = {
    println("main: starting")
    import scala.xml._
    import br.gov.lexml.renderer.strategies.{XML => _, _}
    val renderer = XhtmlRenderer.makeRenderer()
    val xml1 = scala.xml.XML.loadFile(new java.io.File("src/test/xml/RISF-com-RSF-7-2015.xml"))
    val epub = renderer.render(xml1)
    val os = new BufferedOutputStream(new FileOutputStream("target/RISF-com-RSF-7-2015.epub"))
    epub.writeZip(os)
    os.close()    
    println("main: ending")
  }

}