import com.restfb.exception.FacebookException
import java.awt.Desktop
import java.io.{IOException, FileNotFoundException, File}
import java.net.URI
import javax.swing.border.EmptyBorder
import javax.swing.filechooser.FileFilter
import javax.swing.UIManager
import scala.concurrent._
import scala.swing._
import scala.swing.event.ButtonClicked
import ExecutionContext.Implicits.global

object WeightedFacebook extends SimpleSwingApplication {
  private val progressBar = new ProgressBar
  private var archive: Option[File] = None

  def top = new MainFrame {
    title = "Weighted Facebook"
    resizable = false

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    val archiveName = "Facebook Data Archive"

    val tokenField = new TextField(34) { text = "Facebook Access Token" }
    val archiveField = new TextField { text = archiveName }
    val tokenButton = new Button { text = "Get Token" }
    val browseButton = new Button { text = "Browse..." }
    val generateButton = new Button { text = "Generate" }

    contents = new BoxPanel(Orientation.Vertical) {
      val margin = 10
      border = new EmptyBorder(margin, margin, margin, margin)

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += tokenField
        contents += tokenButton
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += archiveField
        contents += browseButton
      }
      contents += new Separator(Orientation.Horizontal)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += progressBar
        contents += generateButton
      }
    }

    listenTo(tokenButton, browseButton, generateButton)

    reactions += {
      case ButtonClicked(`tokenButton`) =>
        Desktop.getDesktop.browse(new URI("https://developers.facebook.com/tools/explorer"))
      case ButtonClicked(`browseButton`) =>
        archive = Option(
          new FileChooser {
            fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fileFilter = new FileFilter {
              override def accept(file: File) = file.getName.toLowerCase.endsWith(".zip") || file.isDirectory
              override def getDescription = "ZIP Archive"
            }
            showOpenDialog(contents(0))
          }.selectedFile
        )

        archiveField.text = archive match {
          case Some(f) => f.getAbsolutePath
          case None => archiveName
        }
      case ButtonClicked(`generateButton`) =>
        if (tokenField.text.size > 100) {
          (archive match {
            case Some(file) =>
              if (file.getAbsolutePath == archiveField.text) {
                Some(file)
              } else {
                openFile(archiveField.text)
              }
            case None =>
              openFile(archiveField.text)
          }) match {
            case Some(file) =>
              val graph = future {
                new GraphApi(tokenField.text, file, updateProgressBar)
              }

              graph onSuccess {
                case g =>
                  try {
                    Option(new FileChooser {
                      fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                      fileFilter = new FileFilter {
                        override def accept(file: File) = file.getName.toLowerCase.endsWith(".gdf") || file.isDirectory
                        override def getDescription = "GUESS gdf"
                      }
                      showSaveDialog(contents(0))
                    }.selectedFile) match {
                      case Some(f) =>
                        val fileName = f.getAbsolutePath + (if (!f.getName.endsWith(".gdf")) ".gdf" else "")

                        g.save(new File(fileName))
                      case None => ()
                    }
                  } catch {
                    case ex: IOException => showError("There was a problem saving your file.")
                  }
              }

              graph onFailure {
                case ex: FacebookException => showError("Facebook API error:\n" + ex.getMessage)
                case ex: Exception => showError("An unknown error occurred:\n" + ex.getMessage)
              }
            case None => ()
          }
        } else showError("Token is not valid.")
    }
  }

  private def openFile(file: String) = {
    try {
      Some(new File(file))
    } catch {
      case ex: FileNotFoundException => showError("File was not found."); None
      case ex: IOException => showError("Cannot read from file."); None
    }
  }

  private def updateProgressBar(value: Int) = {
    Swing.onEDT(progressBar.value = value)
  }

  private def showError(message: String) = Dialog.showMessage(top.contents(0), message, "An error has occurred",
    Dialog.Message.Error)
}
