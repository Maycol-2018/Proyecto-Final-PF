package object ReconstruirCadenaTurbo {
  import Oraculo.Oraculo
  import Oraculo.alfabeto

  def reconstruirCadenaTurbo(n: Int, Ψ: Oraculo): Seq[Char] = {
    var SC: Set[Seq[Char]] = Set(Seq.empty)
    var k = 1

    while (k <= n) {
      val candidatos = for {
        s <- SC
        c <- alfabeto
      } yield s :+ c

      SC = candidatos.filter(Ψ) // consultamos al oráculo

      SC.find(_.length == n) match {
        case Some(result) => return result
        case None => k = k * 2 // paso "turbo": duplicamos k
      }
    }
    Seq.empty
  }
}
