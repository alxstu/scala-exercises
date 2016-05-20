case class Complex (re: Double, im: Double) {
	
	override def toString = re + " " + im + "i"
 
		def + (that: Complex) = Complex(re + that.re, im + that.im)
		def - (that: Complex) = Complex((re-that.re), (im-that.im))
		def * (that: Complex) = Complex(re*that.re-im*that.im, re*that.im+im*that.re)
 		def / (that: Complex) = {
        val r = (re * that.re + im * that.im) / (Math.pow(that.re,2) + Math.pow(that.im,2));
        val i = ((re * (that.im * -1))+(im * that.re)) / (Math.pow(that.re,2) + Math.pow(that.im,2));
       Complex(r,i)
       }
  def unary_- = Complex(-re, -im)
}

val c1 = Complex (2, 1)
val c2 = -c1
val c3 = Complex (1, -2)
val c4 = c1 + c3 
val c5 = c1 - c3 
val c6 = c1 * c3
val c7 = c1 / c3


println(c1)
println(c2)
println(c3)
println(c4)
println(c5)
println(c6)
println(c7)