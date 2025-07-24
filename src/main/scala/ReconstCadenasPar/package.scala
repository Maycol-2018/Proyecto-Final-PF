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

  def reconstruirCadenaTurboMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(sc: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val combinaciones = sc.toIndexedSeq
      val size = combinaciones.size

      val resultados = for (i <- 0 until size) yield task {
        val s1 = combinaciones(i)
        (0 until size).flatMap { j =>
          val s2 = combinaciones(j)
          val s = s1 ++ s2
          if (s.sliding(k).forall(sc.contains)) Some(s) else None
        }
      }

      resultados.flatMap(_.join()).toSet
    }

    @annotation.tailrec
    def construir(sc: Set[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) {
        sc.find(o).getOrElse(throw new Exception("No se pudo reconstruir la cadena"))
      } else {
        val filtered = filtrar(sc, k)
        val valid = filtered.filter(o)
        construir(valid, 2 * k)
      }
    }

    val sc1: Set[Seq[Char]] = alfabeto.map(c => Seq(c)).toSet
    construir(sc1, 1)
  }


  val sec = List('a', 'g', 'g', 'a')
  val or = crearOraculo(1)(sec)
  val par = reconstruirCadenaMejoradoPar(10)(sec.length,or)
}
