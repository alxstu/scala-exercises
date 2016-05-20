object Exercise2 {
  def main(args: Array[String]) {
println("----------------------------------")
println(münzStückl(Euro, Betrag))
println(quicksort(Liste))
println("----------------------------------")
}

//##############################################################
 var Liste = List(2, 4, 6, 1, 0, 6, 3, 1)
 var Euro = List(1, 2, 5, 10, 20, 50, 100, 200)
 var Betrag = 44
//##############################################################
//##############################################################
	 def münzStückl (werte: List[Int], betrag: Int): Int = {
  	if (betrag == 0) 1
  	else if (werte.isEmpty || betrag < 0) 0
    else münzStückl(werte.tail, betrag) + münzStückl(werte, betrag - werte.head)
	}
//##############################################################
//##############################################################
def quicksort (l: List[Int]): List[Int] = l match {
    case head :: Nil => l 
    case head :: tail => val (smaller, bigger) = tail partition (_ < head)
                         quicksort(smaller) ::: head :: quicksort(bigger)
    case _ => l
  }
//##############################################################
} //object end