object Exercise1 {
  def main(args: Array[String]) {
println("----------------------------------")
println("used list: " + someNumbers)
println("")
println("length: " + length(someNumbers))
println("take: " + take(someNumbers, 4))
println("drop: " + drop(someNumbers, 4))
println("uniq: " + uniq(someNumbers))
println("dupall: " + dupall(someNumbers))
println("insertAt: " + insertAt(someNumbers, 2, 119))
println("removeAt: " + removeAt(someNumbers, 3))
println("zählzeichen: der string: '" + aString + "' hat: " + zählZeichen(aString))
println("----------------------------------")
}

val aString = "this is a string lalala"
val someNumbers = List(-11, -10, 3, 3, 3, 2,2, 1)


//##############################################################
def length (l: List[Int]): Int = {
    def _length(n:Int, l:List[Int]):Int = l match {
        case Nil => n
        case _::tail => _length(n + 1, tail)
    }
    _length(0,l)
}

//##############################################################
//##############################################################
def take (l: List[Int], n: Int): List[Int] =  n match {
	    case 0 => l
      case _ => take(l.init , n - 1)
}

//##############################################################
//##############################################################

def drop (l: List[Int], n: Int): List[Int] = n match {
    case 0 => l
    case _ => drop(l.tail, n - 1)
  }

//##############################################################
//##############################################################

def uniq (l: List[Int]): List[Int] = {
 l match {
    case Nil => Nil
    case head::List() => List(head)
    case head::tail if (head == tail.head) => uniq(tail)
    case head::tail => head::uniq(tail)
	}
}

//##############################################################
//##############################################################

def dupall (l: List[Int]): List[Int] = {
    l flatMap { e => List(e, e)}
}

//##############################################################
//##############################################################

 def insertAt (l: List[Int], i: Int, x: Int): List[Int] = l.splitAt(i) match {
     case (pre, post) => pre ::: x :: post
 }

//##############################################################
//##############################################################

def removeAt (l: List[Int], i: Int): List[Int] = l.splitAt(i) match{
    case (_, Nil) => l
    case (pre, e :: post)  => (pre ::: post)
  }

//##############################################################
//##############################################################

 def zählZeichen(s: String): List[(Char, Int)] = {

    def counter(c: Char, l: List[(Char, Int)]): List[(Char, Int)] = {
      if (l == Nil) List((c, 1))
      else if (l.head._1 == c) (c, l.head._2 + 1) :: l.tail
      else l.head :: counter(c, l.tail)
    } 
    if (s.isEmpty) Nil
    else counter(s.head, zählZeichen(s.tail))
  }
  
//##############################################################
}//object end

