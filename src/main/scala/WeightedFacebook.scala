import java.io.{IOException, FileNotFoundException, File}
import javax.swing.border.EmptyBorder
import javax.swing.filechooser.FileFilter
import javax.swing.UIManager
import scala.swing._
import scala.swing.event.ButtonClicked

object WeightedFacebook extends SimpleSwingApplication {
  private var graph: Option[File] = None
  private var archive: Option[File] = None

  def top = new MainFrame {
    title = "Weighted Facebook"
    resizable = false

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    val graphName = "Facebook Friendship Graph"
    val archiveName = "Facebook Data Archive"

    val graphField = new TextField { text = graphName }
    val graphBrowseButton = new Button { text = "Browse..." }
    val archiveField = new TextField { text = archiveName }
    val archiveBrowseButton = new Button { text = "Browse..." }
    val generateButton = new Button { text = "Generate" }

    contents = new BoxPanel(Orientation.Vertical) {
      val margin = 10
      border = new EmptyBorder(margin, margin, margin, margin)

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += graphField
        contents += graphBrowseButton
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += archiveField
        contents += archiveBrowseButton
      }
      contents += new Separator(Orientation.Horizontal)
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += generateButton
      }
    }

    listenTo(graphBrowseButton, archiveBrowseButton, generateButton)

    reactions += {
      case ButtonClicked(`graphBrowseButton`) => {
        graph = Option(
          new FileChooser {
            fileSelectionMode = FileChooser.SelectionMode.FilesOnly
            fileFilter = new FileFilter {
              override def accept(file: File) = file.getName.toLowerCase.endsWith(".gdf") || file.isDirectory

              override def getDescription = "GUESS gdf"
            }
            showOpenDialog(contents(0))
          }.selectedFile
        )

        graphField.text = graph match {
          case Some(file) => file.getAbsolutePath
          case None => graphName
        }
      }

      case ButtonClicked(`archiveBrowseButton`) => {
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
          case Some(file) => file.getAbsolutePath
          case None => archiveName
        }
      }

      case ButtonClicked(`generateButton`) => {
        (graph match {
          case Some(file) => {
            if (file.getAbsolutePath == graphField.text) {
              Some(file)
            } else {
              openFile(graphField.text)
            }
          }

          case None => openFile(graphField.text)
        },
        archive match {
          case Some(file) => {
            if (file.getAbsolutePath == archiveField.text) {
              Some(file)
            } else {
              openFile(archiveField.text)
            }
          }

          case None => openFile(archiveField.text)
        }) match {
          case (Some(graph), Some(archive)) => {
            val solver = new Solver(graph, archive)

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

                  solver.save(new File(fileName))
              }
            } catch {
              case ex: IOException => showError("There was a problem saving your file.")
            }
          }
        }
      }
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

  private def showError(message: String) = Dialog.showMessage(top.contents(0), message, "An error has occurred",
    Dialog.Message.Error)
}
