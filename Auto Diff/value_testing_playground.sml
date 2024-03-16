val a = mkDefaultValue(1.0)
val b = mkDefaultValue 6.0
val c = mkDefaultValue 3.0

val d = add(a, b)
val e = mul(d, c)
val () = backwards(e)