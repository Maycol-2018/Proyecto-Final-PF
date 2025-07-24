import scala.util.Random
package object ArbolSufijos {
  def reduceSeg(ent: Array[Int], izq: Int, der: Int, a0: Int, f: (Int, Int) => Int): Int = {
    var a = a0
    var i = izq
    while (i < der) {
      a = f(a, ent(i))
      i = i + 1
    }
    a
  }



  def mapSeg(idxs: Array[Int], izq: Int, der: Int, f: (Int) => Int, sal: Array[Int]) = {
    var i = izq
    while (i < der) {
      
      i += 1
    }
  }


  def scanLeft(ent: Array[Int], a0: Int, f: (Int, Int) => Int, sal: Array[Int]) = {
    val fi = { (i: Int) => reduceSeg(ent, 0, i, a0, f) }

   
    val ult = ent.length - 1
    sal(ult + 1) = f(sal(ult), ent(ult))
  }


  val e =Array(5, 5, 7, 9)
  val s1  =Array(0, 0, 0, 0,0)
  val f2 = (x:Int,y:Int)=>x+y

  val res3 = scanLeft(e,100,f2,s1)
}
