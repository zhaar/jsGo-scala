package GoJS

import GobanLogic._
import org.scalajs.dom._
import scala.util.Random

class DrawingEngine(ctx: CanvasRenderingContext2D, width: Int, height: Int) {

  def all(goban: BoardState) = {

    val horizontalSpace = width / (goban.size + 1)
    val verticalSpace = height / (goban.size + 1)

    val random = new Random()

    def background() = {
      ctx.beginPath()
      ctx.strokeStyle = "black"
      ctx.lineWidth = 1.0
      val size = goban.size + 1
      for (i <- 1 to goban.size) {
        ctx.moveTo(i*width/ size, height/ size)
        ctx.lineTo(i*width/ size, height-height/ size)
        ctx.moveTo(width/ size, i*height/ size)
        ctx.lineTo(width - width/ size, i*height/ size)
      }
      ctx.stroke()
      stars()
    }

    def stars() = {
      for(i <- 0 until 3){
        ctx.beginPath()
        val size = goban.size + 1
        ctx.fillStyle="black"
        ctx.arc(i*6*width/size+(4*width/size),4*width/ size,3,-Math.PI/2,3*Math.PI/2)
        ctx.arc(i*6*width/ size +(4*width/ size),10*width/ size,3,-Math.PI/2,3*Math.PI/2)
        ctx.arc(i*6*width/ size +(4*width/ size),16*width/ size,3,-Math.PI/2,3*Math.PI/2)
        ctx.fill()
        ctx.stroke()
      }
    }

    def stone(coord: (Int, Int), color: Stone) = {
      val (canvasx, canvasy) = gobanToCanvas(coord._1, coord._2)
      val margin = 0.1
      val r = (horizontalSpace / 2) * (1-margin)
      val paddingX = (random.nextFloat() * margin) - margin/2
      val paddingY = (random.nextFloat() * margin) - margin/2
      ctx.save()
      ctx.beginPath()
      ctx.arc(canvasx + paddingX, canvasy + paddingY, radius = r, 0, 2*Math.PI)
      ctx.fillStyle		= playerColor(color)
      ctx.strokeStyle		= "rgba(0,0,0,0)"
      ctx.shadowOffsetX	= r/3
      ctx.shadowOffsetY	= r/3
      ctx.shadowBlur	= r/3
      ctx.shadowColor	= "rgba(0, 0, 0, 0.5)"
      ctx.fill()
      ctx.stroke()
      ctx.restore()
    }

    def gobanToCanvas(coord: (Int, Int)): (Int, Int) = {
      val (x, y) = coord
      (x * horizontalSpace + horizontalSpace , y * verticalSpace + verticalSpace)
    }

    background()

//    goban.board.foreach { x =>
//      case col:Array[Option[Stone]] => {
//        col.foreach {
//          case Some(s)=> stone((x,y),s)
//          case _ =>
//        }
//      }
//    }
    for{
      x <- 0 until goban.board.length
      y <- 0 until goban.board(x).length
    } yield goban.board(x)(y) match {
      case Some(s) => stone((x,y),s)
      case _ =>
    }
  }

  def playerColor(player: Stone): String = player match {
    case White => "white"
    case Black => "black"
  }
}
