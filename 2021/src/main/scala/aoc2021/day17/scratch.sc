val x = (0 to 20).toList

def inc(step: Int): Int => Int = i => (i + step-1) % 9 + 1

val y = (0 to 20).map(inc(2))