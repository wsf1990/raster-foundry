package com.azavea.rf.datamodel.color.functions

object Approximations {
  def pow(a: Double, b: Double): Double = { // exponentiation by squaring
    if(a < 1 && java.lang.Double.isInfinite(b)) return 0d
    if(a >= 1 && java.lang.Double.isInfinite(b)) return Double.NaN
    if(java.lang.Double.isNaN(a) || java.lang.Double.isNaN(b)) return Double.NaN

    var r = 1d
    var exp = b.toInt
    var base = a
    while (exp != 0) {
      if ((exp & 1) != 0) r *= base
      base *= base
      exp >>= 1
    }
    // use the IEEE 754 trick for the fraction of the exponent
    val b_faction = b - b.toInt
    val tmp = java.lang.Double.doubleToLongBits(a)
    val tmp2 = (b_faction * (tmp - 4606921280493453312L)).toLong + 4606921280493453312L
    r * java.lang.Double.longBitsToDouble(tmp2)
  }
}
