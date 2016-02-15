import solution._


def findNumberOfE(s: List[Char], n: Int): Int = s match {
  case Nil => n
  case (h :: t) => if (h == 'E') findNumberOfE(t, n + 1) else findNumberOfE(t, n)
}

findNumberOfE(List('A', 'B', 'A', 'D', 'A', 'B', 'B', 'C'), 0)
Solver.charToNumberWithDelta('A', 0)



