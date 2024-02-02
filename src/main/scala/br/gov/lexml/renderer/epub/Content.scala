package br.gov.lexml.renderer.epub

import scala.xml.*
import scala.xml.dtd.*
import java.io.*
import java.util.UUID

import org.apache.commons.io.*
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry

trait StreamSource:
  def id: String
  def path: String
  def mimeType: String
  def write(os: OutputStream) : Unit

trait CharStreamSource extends StreamSource:
  def encoding = "UTF-8"
  def write(w: Writer) : Unit
  final override def write(os: OutputStream) : Unit =
    val w = new OutputStreamWriter(os, encoding)
    write(w)
    w.flush()

trait ResourceSource extends StreamSource:
  def classLoader: ClassLoader
  def resourceName: String
  final override def write(os: OutputStream) : Unit = 
    val is = classLoader.getResourceAsStream(resourceName)
    if is != null then
      IOUtils.copy(is, os)
      IOUtils.closeQuietly(is)
    os.flush()

trait ElemSource extends CharStreamSource:
  def elem: Elem
  def xmlDecl: Boolean = true
  def doctype: Option[DocType] = None
  final override def write(w: Writer) : Unit =
    XML.write(w, elem, encoding, xmlDecl, doctype.orNull)

case class CssResourceSource(id : String,resourceName: String, mid: Option[String] = None,
  subPath: Option[String] = None,
  classLoader: ClassLoader = classOf[CssResourceSource].getClassLoader) extends ResourceSource:
  override val mimeType : String = "text/css"
  override val path: String = CssResourceSource.base + subPath.getOrElse(FilenameUtils.getName(resourceName))

object CssResourceSource:
  val base = ""

final case class NavPoint(id: String, file: String, text: String, subPoints: NavPoint*):
  def makeXML(pos: Int): (Int, Elem) =
    val (pos1, rsubels) = subPoints.foldLeft[(Int, List[Elem])](pos + 1, List()) {
      case ((p, l), n) => val (p1, e) = n.makeXML(p); (p1, e :: l)
    }
    val el = <navPoint id={ "navPoint-" + pos } playOrder={ pos.toString }>
                <navLabel>
                  <text>{ text }</text>
                </navLabel>
                <content src={ file + "#" + id }/>
                { NodeSeq fromSeq rsubels.reverse }
              </navPoint>
    (pos1, el)

  lazy val toXML: Elem = makeXML(1)._2
  lazy val depth: Int = (1 :: subPoints.toList.map(_.depth + 1)).max

final case class Ncx(id: String, docTitle: String, toc: NavPoint*) extends ElemSource:
  lazy val uuid: UUID = java.util.UUID.randomUUID()

  override val path: String = id + ".ncx"

  lazy val depth: Int = toc.map(_.depth).max

  lazy val toXML: Elem =
    <ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
      <head>
        <meta name="dtb:uid" content={ uuid.toString }/>
        <meta name="dtb:depth" content={ (depth + 1).toString }/>
        <meta name="dtb:totalPageCount" content="0"/>
        <meta name="dtb:maxPageNumber" content="0"/>
      </head>
      <docTitle>
        <text>{ docTitle }</text>
      </docTitle>
      <navMap>
        { NodeSeq fromSeq toc.map(_.toXML) }
      </navMap>
    </ncx>

  override def elem: Elem = toXML
  override def doctype: Option[DocType] = Some(
    DocType("ncx", PublicID(
      "-//NISO//DTD ncx 2005-1//EN",
      "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd"), Seq()))
  override val mimeType = "application/x-dtbncx+xml"
end Ncx

object TextSectionSource:
  def id(num: Int): String = "Section%04d".format(num)
  def fname(num: Int): String = id(num) + ".xhtml"
  def path(num: Int): String = base + fname(num)
  val base = ""
  def reducePath(f : File) : String = f match {
    case null => ""
    case _ => "../" + reducePath(f.getParentFile)
  }
  def reducePath(p : String) : String = reducePath(File(p).getParentFile)

final case class TextSectionSource(
  num: Int,
  titulo: String,
  urn: String,
  body: Elem,
  styles : CssResourceSource*) extends ElemSource:

  override val id: String = TextSectionSource.id(num)
  override val path: String = TextSectionSource.path(num)
  override val mimeType = "application/xhtml+xml"
  val reducedPath: String = TextSectionSource.reducePath(path)

  lazy val toXML: Elem =
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="pt">
      <head>
        { NodeSeq fromSeq styles.map(s =>
        <link type="text/css" rel="stylesheet" href={reducedPath + s.path}/>
          ) }
        <title>{ titulo }</title>
        <meta name="LEXML.urn" content={ urn }/>
        <meta name="LEXML.EPUB.part" content={ num.toString }/>
      </head>
      { NodeSeq fromSeq body }
    </html>
  override def elem: Elem = toXML
  override def doctype: Option[DocType] = Some(
    DocType("html", PublicID(
      "-//W3C//DTD XHTML 1.1//EN",
      "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"), Seq()))
end TextSectionSource

final case class DcInfo(
    urn : Option[String] = None,
    titulo : Option[String] = None,
    lingua : String = "pt_BR",
    assunto : Option[String] = None,
    descricao : Option[String] = None,
    relacao : Option[String] = None,
    autor : Option[String] = None,
    publicador : Option[String] = None,
    data : String = 
      new java.text.SimpleDateFormat("yyyy-MM-dd").
      format(new java.util.Date),
    direitos : Option[String] = None,
    identifier : (String,String) = 
      ("UUID","urn:uuid:" + java.util.UUID.randomUUID().toString)    
    ):
  def o2e(v : Option[String])(f : String => Elem) : NodeSeq = v.map(f).getOrElse(NodeSeq fromSeq Seq())
    
  def toXML: NodeSeq = NodeSeq fromSeq Seq(
        <dc:language xsi:type="dcterms:RFC3066">{lingua}</dc:language>,
        <dc:identifier id="BookId" opf:scheme={identifier._1}>
          {identifier._2}
        </dc:identifier>, 
        /* o2e(urn){ u => <dc:identifier opf:scheme="LEXML">
          {u}
        </dc:identifier>} , */
        o2e(titulo) { s => <dc:title>{s}</dc:title> },
        o2e(assunto) { s => <dc:subject>{s}</dc:subject> },
        o2e(descricao) { s => <dc:description>{s}</dc:description> },
        o2e(relacao) { s => <dc:relation>{s}</dc:relation>},
        o2e(autor) { s => <dc:creator opf:role="aut" opf:file-as={s} >{s}</dc:creator> },
        o2e(publicador) { s => <dc:publisher>{s}</dc:publisher>},
        <dc:date xsi:type="dcterms:W3CDTF">{data}</dc:date>,        
        o2e(direitos) { s => <dc:rights>{s}</dc:rights>}      
  ).flatten
end DcInfo

final case class Content(textFiles: List[TextSectionSource],
  otherFiles: List[StreamSource],
  toc: Ncx, dcInfo : DcInfo = DcInfo()) extends ElemSource:

  override val id = "content"

  override val path = "content.opf"

  override val mimeType = "application/oebps-package+xml"

  lazy val uuid: UUID = java.util.UUID.randomUUID()

  lazy val toXML: Elem =
    <package xmlns="http://www.idpf.org/2007/opf" unique-identifier="BookId" version="2.0">
      <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:dcterms="http://purl.org/dc/terms/">
        {dcInfo.toXML}
      </metadata>
      <manifest>
        {
          for s <- textFiles ++ otherFiles yield
            <item href={ s.path } id={ s.id } media-type={ s.mimeType }/>
        }
        <item href={ toc.path } id={ toc.id } media-type={ toc.mimeType }/>
      </manifest>
      <spine toc="ncx">
        {
          for s <- textFiles yield
            <itemref idref={ s.id }/>
        }
      </spine>
    </package>

  override def elem: Elem = toXML

  lazy val allFiles: Seq[StreamSource] = this :: toc :: textFiles ++ otherFiles
end Content

object EpubPackage:
  val oebpsBase = ""

trait EpubPackage:
  val content: Content

  val container: ElemSource = new ElemSource:
    val id = "container"
    val path = "META-INF/container.xml"
    val mimeType = "text/xml"
    override def elem: Elem =
      <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
        <rootfiles>
          <rootfile full-path={ EpubPackage.oebpsBase + content.path } media-type={ content.mimeType }/>
        </rootfiles>
      </container>

  def writeZip(os: OutputStream) : Unit =
    val zos = new ZipOutputStream(os)
    val ze = new ZipEntry("mimetype")
    val mimeBytes = "application/epub+zip".getBytes
    ze.setMethod(ZipEntry.STORED)
    ze.setCrc(0x2cab616f)
    ze.setSize(mimeBytes.length)
    ze.setCompressedSize(mimeBytes.length)
    zos.setMethod(ZipOutputStream.STORED)
    zos.putNextEntry(ze)
    zos.write(mimeBytes)
    zos.closeEntry()
    zos.setMethod(ZipOutputStream.DEFLATED)
    zos.setLevel(9)
    for s <- content.allFiles do
      zos.putNextEntry(new ZipEntry(EpubPackage.oebpsBase + s.path))
      s.write(zos)
    zos.putNextEntry(new ZipEntry(container.path))
    container.write(zos)
    zos.close()
end EpubPackage

