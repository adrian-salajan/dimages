package dimages

import java.io.File

import dimages.Dimages.Loc

object Main {

  def main(args: Array[String]): Unit = {

    val baloons = this.getClass.getClassLoader.getResource("baloons.bmp")
    val screen = this.getClass.getClassLoader.getResource("a.bmp")

    val out = new File("over.bmp")
    if (!out.exists()) out.createNewFile()

    val img1: Image = Dimages.loadDimage(baloons.getFile)
    val img2: Image = Dimages.loadDimage(screen.getFile)


    val over = img1.over(img2, {
      case (c1, c2) => if (c1.brightness > c2.brightness) c1 else c2
    })


    print(Dimages.saveDimage(930, 985, over, out))

    val crop = new File("crop.bmp")
    if (!crop.exists()) crop.createNewFile()
    val cropImg = img1.crop{ Dimages.rectangleRegion(Loc(300, 500),Loc(600, 800))}
    print(Dimages.saveDimage(900, 900, cropImg, crop))

 }

}
