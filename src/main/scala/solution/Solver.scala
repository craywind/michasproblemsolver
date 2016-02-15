package solution

  /**
  * Solution to problem suggested by Michael
  * Created by CrayWind on 2016-02-14.
  */
object Solver {
  /*convert char answer to number of answer using delta*/
  def charToNumberWithDelta(c: Char, d: Int) = c.toInt - 'A'.toInt + d

 /*Find number of occurence of char c in list s*/
  @annotation.tailrec
  def findNumberOfChar(s: List[Char], c: Char, n: Int): Int = s match {
    case Nil => n
    case (h :: t) => if (h == c) findNumberOfChar(t, c, n + 1) else findNumberOfChar(t, c, n)
  }

  /**check the answer 0:
  * what is the first question with answer B? A == 0, B == 1, C == 2, D == 3, E == 4
  */
  def check00(s: List[Char]): Boolean =  {
    val v = s.head
    if (v == 'A') false
    else if (v == 'B') false
    else if (v == 'C') s(2) == 'B'
    else if (v == 'D') s(3) == 'B'
    else if (v == 'E') s(4) == 'B'
    else false
    }

  /**check the answer 1:
  * What are the only 2 cosequtive questions with same answer? A == 5&6, B == 6&7, C == 7&8, D == 8&9, E == 9&10
  */
  def check01(s: List[Char]): Boolean =  {
    def findFirst(s: List[Char], n: Int): Option[Int] = s match {
      case Nil => None
      case (h :: Nil) => None
      case (h :: b :: t) => if (h == b) Some(n) else findFirst(b :: t, n + 1)
    }
    @annotation.tailrec
    def findLast(s: List[Char], n: Int, a: Int): Int = s match {
      case Nil => a
      case (h :: Nil) => a
      case (h :: b :: t) => if (h == b) findLast(b :: t, n + 1, n) else findLast(b :: t, n + 1, a)
    }
    findFirst(s, 0) match {
      case None => false
      case Some(n) => (n == findLast(s.drop(n), n, n)) && (n == charToNumberWithDelta(s(1), 5))
      }
  }

  /**check the answer 2:
    * How many questions have answer E? A == 0, B == 1, C == 2, D == 3, E == 4
    */
  def check02(s: List[Char]): Boolean =
    findNumberOfChar(s, 'E', 0) == charToNumberWithDelta(s(2), 0)


  /**check the answer 3:
    * How many questions have answer A? A == 4, B == 5, C == 6, D == 7, E == 8
    */
  def check03(s: List[Char]): Boolean =
    findNumberOfChar(s, 'A', 0) == charToNumberWithDelta(s(2), 4)

  /**check the answer 4:
    * Answer to this question is the same as to the question? A == 0, B == 1, C == 2, D == 3, E == 4
    */
  def check04(s: List[Char]): Boolean =
    s(4) == s(charToNumberWithDelta(s(4), 0))

  /**check the answer 5:
  * Answer to question 16 is? A == C, B == D, C == E, D == None of above, E == All of above
  */
  def check05(s: List[Char]): Boolean = {
    val v05 = s(5)
    val v16 = s(16)
    ((v05 == 'A' && v16 == 'C')
    || (v05 == 'B' && v16 == 'D')
    || (v05 == 'C' && v16 == 'E')
    || (v05 == 'D' && v16 != 'C' && v16 != 'D' && v16 != 'E'))
  }

  /**check the answer 6:
    * In alphabet distance betwwen answer to this question and answer to the next one is? A == 4, B == 3, C == 2, D == 1, E == 0
    */
  def check06(s: List[Char]): Boolean =
    Math.abs(s(6).toInt - s(7).toInt) == Math.abs(charToNumberWithDelta(s(6), -4))

  /**check the answer 7:
    * Number of questions with answer being vowel (A or E)? A == 4, B == 5, C == 6, D == 7, E == 8
    */
  def check07(s: List[Char]): Boolean =
    s.foldLeft(0)((x, c) => if (c == 'A' || c == 'E') x + 1 else x) == charToNumberWithDelta(s(7), 4)

  /**check the answer 8:
    * Next question with same answer is? A == 9, B == 10, C == 11, D == 12, E == 13
    */
  def check08(s: List[Char]): Boolean =
    s(8) == s(charToNumberWithDelta(s(8), 9))

  /**check the answer 9:
    * Answer to the question 15 is? A == D, B == A, C == E, D == B, E == C
    * we could of optimised to A (it's the only valid answer, but for purity adding here mapping table)
    */
  def check09(s: List[Char]): Boolean = {
    val v09 = s(9)
    val v15 = s(15)
    ((v09 == 'A' && v15 == 'D')
    || (v09 == 'B' && v15 == 'A')
    || (v09 == 'C' && v15 == 'E')
    || (v09 == 'D' && v15 == 'B')
    || (v09 == 'E' && v15 == 'C'))
  }
}