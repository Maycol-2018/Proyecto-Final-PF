import Oraculo.*
import org.scalameter.measure

import scala.collection.parallel.CollectionConverters.*

package object ReconstCadenasPar {
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def generarCombinaciones(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq())
      else for {
        c <- alfabeto
        resto <- generarCombinaciones(n - 1)
      } yield c +: resto
    }

    val combinaciones = generarCombinaciones(n)

    val resultado = if (combinaciones.size >= umbral) {
      // Paralelismo de datos
      combinaciones.par.find(o)
    } else {
      // Secuencial si es pequeño
      combinaciones.find(o)
    }

    resultado match {
      case Some(seq) => seq
      case None => Seq()
    }
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def generarSecuencias(k: Int): Set[Seq[Char]] = {
      (1 to k).foldLeft(Set(Seq.empty[Char])) {
        case (prev, _) =>
          val candidatos = prev.par.flatMap(seq => alfabeto.par.map(c => seq :+ c)).toSet
          candidatos.par.filter(o).seq.toSet
      }
    }

    if (n <= umbral) {
      generarSecuencias(n).find(_.length == n).getOrElse(Seq.empty)
    } else {
      val mitad = n / 2

      // Genera todas las secuencias válidas para cada mitad en paralelo
      val (izquierdas, derechas) = (
        generarSecuencias(mitad).par,
        generarSecuencias(n - mitad).par
      )

      // Intentamos combinar cada izquierda con cada derecha
      val combinadasValidas = for {
        s1 <- izquierdas
        s2 <- derechas
        combinada = s1 ++ s2
        if combinada.length == n && o(combinada)
      } yield combinada

      combinadasValidas.headOption.getOrElse(Seq.empty)
    }
  }


  val sec = List('a', 'g', 'g', 'a')
  val or = crearOraculo(1)(sec)
  val par = reconstruirCadenaMejoradoPar(10)(sec.length,or)
}
