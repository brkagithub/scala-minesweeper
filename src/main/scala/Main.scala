import scala.swing._

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Scala Minesweeper"
    contents = new UI(9, 9, 10)
    size = new Dimension(400, 400)
    centerOnScreen()
    visible = true
  }
}