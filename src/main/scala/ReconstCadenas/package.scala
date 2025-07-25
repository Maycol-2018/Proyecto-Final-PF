import ArbolSufijos._
import Oraculo._


package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    // Generador perezoso de combinaciones
    def generarCombinaciones(n: Int): LazyList[Seq[Char]] = {
      if (n == 0) LazyList(Seq())
      else for {
        c <- LazyList.from(alfabeto)
        resto <- generarCombinaciones(n - 1)
      } yield c +: resto
    }

    // Buscar la primera secuencia que el oráculo reconozca como válida
    generarCombinaciones(n).find(o).getOrElse(Seq())
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val secuenciasValidas: Set[Seq[Char]] = (1 to n).foldLeft(Set(Seq.empty[Char])) {
      case (secuenciasAnteriores, _) =>
        val candidatos: Set[Seq[Char]] = secuenciasAnteriores.flatMap(seq => alfabeto.map(c => seq :+ c))
        val filtrados: Set[Seq[Char]] = candidatos.filter(o)
        filtrados
    }

    val resultado: Option[Seq[Char]] = secuenciasValidas.find(_.length == n)

    resultado.getOrElse(Seq.empty)
  }
  
}


