package object ReconstruirCadenaTurboPar {
  import Oraculo.Oraculo
  import Oraculo.alfabeto
  import scala.collection.parallel.CollectionConverters._

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, Ψ: Oraculo): Seq[Char] = {
    def expand(SC: Set[Seq[Char]]): Option[Seq[Char]] = {
      val candidatos = for {
        s1 <- SC
        s2 <- SC
      } yield s1 ++ s2

      val filtrados: Set[Seq[Char]] =
        if (candidatos.size >= umbral)
          candidatos.toSeq.par.filter(Ψ).seq.toSet
        else
          candidatos.filter(Ψ)

      filtrados.find(_.length == n) match {
        case Some(result) => Some(result)
        case None if filtrados.nonEmpty => expand(filtrados)
        case None => None
      }
    }

    expand(alfabeto.map(Seq(_)).toSet).getOrElse(Seq.empty)
  }
}