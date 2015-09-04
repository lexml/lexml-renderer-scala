package br.gov.lexml.renderer.gui

import scala.swing.Frame
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.BorderLayout
import javax.swing.JTextArea
import javax.swing.BoxLayout
import javax.swing.JTextField
import javax.swing.JButton
import scala.swing.TextArea
import scala.swing.BorderPanel
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import scala.swing.TextField
import scala.swing.Button
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.event.ButtonClicked
import scala.swing.Dialog
import scala.swing.FileChooser
import java.io.File
import javax.swing.filechooser.FileFilter
import javax.swing.filechooser.FileNameExtensionFilter

object Processador {
  def processa(inFile : File, outFile : File, log : String => Unit) : Unit = {
    log(s"Processando arquivo ${inFile.getPath}")
    import java.io._
    import scala.xml._
    import br.gov.lexml.renderer.strategies.{XML => _, _}
    val renderer = XhtmlRenderer.makeRenderer()
    log("Lendo arquivo LexML")
    val xml1 = scala.xml.XML.loadFile(inFile)
    log("Renderizando EPUB")
    val epub = renderer.render(xml1)
    log(s"Gravando EPUB em ${outFile.getPath}")
    val os = new BufferedOutputStream(new FileOutputStream(outFile))
    epub.writeZip(os)
    os.close()    
    log("Renderização terminada")
  }
}

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "EPub Tool"
    
    object logArea extends TextArea {
      rows = 30
      columns = 60
      editable = false
      wordWrap = true
    }
    
    def log(s : String) = logArea.append(s + scala.util.Properties.lineSeparator)
    
    
    object bottomPanel extends BoxPanel(Orientation.Horizontal) {
      object inputFileField extends TextField {
        columns = 30      
        editable = false      
      }
      object outputFileField extends TextField {
        editable = false
        columns = 30               
      }
      object inputFileButton extends Button {
        text = "Arquivo de entrada"
      }
      
      object inputFileChooser extends FileChooser(new File(".")) {
        title = "Escolha o arquivo de entrada"
        peer.addChoosableFileFilter(new FileNameExtensionFilter("Arquivo LexML", "*.xml"))
        peer.addChoosableFileFilter(acceptAllFileFilter)         
      }
      
      object outputFileButton extends Button {
        text = "Arquivo de saida"
        enabled = false
      }
      
      object outputFileChooser extends FileChooser(new File(".")) {
        title = "Escolha o arquivo de saída"
        fileFilter = new FileNameExtensionFilter("Arquivo EPUB", "*.epub")
      }
      
      object processarButton extends Button {
        text = "Processar"
        enabled = false
      }
      
      object sairButton extends Button {
        text = "Sair"
      }
      
      
      
      contents.append(inputFileField)
      contents.append(inputFileButton)
      contents.append(outputFileField)
      contents.append(outputFileButton)
      contents.append(processarButton)
      contents.append(sairButton)
      
      
      listenTo(outputFileButton)
      listenTo(processarButton)
      listenTo(sairButton)
      
      listenTo(inputFileButton)
      
      reactions += {
        case ButtonClicked(`inputFileButton`) =>         
          if(!inputFileField.text.isEmpty) {            
            inputFileChooser.selectedFile = new File(inputFileField.text)
          } else {            
            inputFileChooser.selectedFile = null
          }
          if(inputFileChooser.showOpenDialog(this) == FileChooser.Result.Approve) {            
            var newPath = inputFileChooser.selectedFile.getPath
            log(s"Arquivo de entrada escolhido: ${newPath}")
            if(newPath != inputFileField.text) {              
              inputFileField.text = newPath
              val outPath = newPath.replaceAll("\\.[^.]*$",".epub")              
              outputFileField.text = outPath              
              outputFileChooser.selectedFile = new File(outPath)
              outputFileButton.enabled = inputFileChooser.selectedFile.exists
              processarButton.enabled = inputFileChooser.selectedFile.exists
            }
          }
        case ButtonClicked(`outputFileButton`) =>          
          if(!outputFileField.text.isEmpty) {            
            inputFileChooser.selectedFile = new File(inputFileField.text)
          } else  {            
            val inPath = inputFileField.text              
            val outPath = inPath.replaceAll("\\.[^.]*$",".epub")
            outputFileField.text = outPath     
          }
          if(outputFileChooser.showSaveDialog(this) == FileChooser.Result.Approve) {
            log(s"Arquivo de saída escolhido: ${outputFileChooser.selectedFile.getPath}")
            outputFileField.text = outputFileChooser.selectedFile.getPath            
          }
        case ButtonClicked(`processarButton`) =>          
          Processador.processa(inputFileChooser.selectedFile,outputFileChooser.selectedFile,log)
        case ButtonClicked(`sairButton`) =>          
          sys.exit()
      }
      
    }
    
    contents = new BorderPanel {
      add(logArea, BorderPanel.Position.Center)
      add(bottomPanel, BorderPanel.Position.South)
    }
    
  }
  
  
}