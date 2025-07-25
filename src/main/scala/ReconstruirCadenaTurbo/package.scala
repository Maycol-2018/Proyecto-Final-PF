package object ReconstruirCadenaTurbo {
  import Oraculo.Oraculo
  import Oraculo.alfabeto

  def reconstruirCadenaTurbo(n: Int, Ψ: Oraculo): Seq[Char] = {
    def expand(SC: Set[Seq[Char]]): Option[Seq[Char]] = {
      val candidatos = for {
        s1 <- SC
        s2 <- SC
      } yield s1 ++ s2

      val filtrados = candidatos.filter(Ψ)

      filtrados.find(_.length == n) match {
        case Some(result) => Some(result)
        case None if filtrados.nonEmpty => expand(filtrados)
        case None => None
      }
    }

    expand(alfabeto.map(Seq(_)).toSet).getOrElse(Seq.empty)
  }
}