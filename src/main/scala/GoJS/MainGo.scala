package GoJS

import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport

import GobanLogic._

@JSExport
object MainGo {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val renderer: CanvasRenderingContext2D = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    val size = Math.min(canvas.parentElement.clientWidth, canvas.parentElement.clientHeight)
    canvas.width = size
    canvas.height = size
    val draw = new DrawingEngine(renderer, size, size)
    renderer.translate(-0.5, -0.5)
    val BIG_BOARD = 19
    val SMALLER_BOARD = 13
    val SMALLEST_BOARD = 9

    var gameState = BoardState(BIG_BOARD, emptyBoard(BIG_BOARD))


    def run() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      draw.all(gameState)
    }


    canvas.onclick = (e: dom.MouseEvent) => {
      val rect = canvas.getBoundingClientRect()
      val canvasx: Int = (e.clientX - rect.left).toInt
      val canvasy: Int = (e.clientY - rect.top).toInt
      val (x, y) = canvasToGoban((canvasx, canvasy))
      gameState = GobanLogic.wantToPlayOn(x, y, gameState)
      println(s"-----now turn to ${gameState.currentPlayer}-----")
    }

    def canvasToGoban(coord: (Int, Int)): (Int, Int) = {
      val verticalSpace = canvas.height / (BIG_BOARD + 1)
      val horizontalSpace = canvas.width / (BIG_BOARD + 1)
      var x = coord._1 - (canvas.width  / (2* (BIG_BOARD + 1)))
      var y = coord._2 - (canvas.height / (2* (BIG_BOARD + 1)))
      x = (x-(x%horizontalSpace)) / horizontalSpace
      y = (y-(y%verticalSpace))  / verticalSpace
      (x,y)
    }

    def gobanToCanvas(coord: (Int, Int)): (Int, Int) = {
      val (x, y) = coord
      val horizontalSpace = canvas.width / (BIG_BOARD + 1)
      val verticalSpace = canvas.height / (BIG_BOARD + 1)
      (x * horizontalSpace + horizontalSpace , y * verticalSpace + verticalSpace)
    }

    dom.setInterval(() => run(), 20)
  }
}
