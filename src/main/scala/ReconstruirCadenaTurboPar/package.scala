package object ReconstruirCadenaTurboPar {
  import Oraculo.Oraculo
  import Oraculo.alfabeto
  import scala.collection.parallel.CollectionConverters._
  import scala.collection.parallel.immutable.ParSeq
  import java.util.concurrent.ForkJoinPool
  import scala.collection.parallel.ForkJoinTaskSupport

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, Ψ: Oraculo): Seq[Char] = {
    def expand(SC: Set[Seq[Char]], k: Int): Option[Seq[Char]] = {
      val candidatos = SC.flatMap(s => alfabeto.map(c => s :+ c))

      val filtrados: Set[Seq[Char]] =
        if (candidatos.size >= umbral) {
          val paralelo: ParSeq[Seq[Char]] = candidatos.toSeq.par
          paralelo.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(4)) // Limita a 4 hilos
          paralelo.filter(Ψ).seq.toSet
        } else {
          candidatos.filter(Ψ)
        }

      filtrados.find(_.length == n) match {
        case Some(result) => Some(result)
        case None if k <= n && filtrados.nonEmpty => expand(filtrados, k * 2)
        case _ => None
      }
    }

    expand(Set(Seq.empty), 1).getOrElse(Seq.empty)
  }
}