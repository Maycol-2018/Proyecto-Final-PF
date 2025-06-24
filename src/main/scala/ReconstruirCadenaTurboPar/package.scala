package object ReconstruirCadenaTurboPar {
  import Oraculo.Oraculo
  import Oraculo.alfabeto

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, Ψ: Oraculo): Seq[Char] = {
    var SC: Set[Seq[Char]] = Set(Seq.empty)
    var k = 1

    while (k <= n) {
      val candidatos = for {
        s <- SC
        c <- alfabeto
      } yield s :+ c

      val filtrado = if (candidatos.size >= umbral) {
        val seqC = candidatos.toSeq
        val (yes, _) = seqC.partition(Ψ)
        yes.toSet // Aprovechamos la paralelización implícita
      } else {
        candidatos.filter(Ψ)
      }

      SC = filtrado

      SC.find(_.length == n) match {
        case Some(result) => return result
        case None => k = k * 2
      }
    }
    Seq.empty
  }
}
