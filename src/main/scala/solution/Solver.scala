package solution

import scala.collection.mutable.ListBuffer

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
    case h :: t => if (h == c) findNumberOfChar(t, c, n + 1) else findNumberOfChar(t, c, n)
  }

  /*find number of occurence of each answer variant */
  @annotation.tailrec
  def findAllChars(s: List[Char], a: (Int, Int, Int, Int, Int)): (Int, Int, Int, Int, Int) = s match {
    case Nil => a
    case h :: t => if (h == 'A') findAllChars(t, (a._1 + 1, a._2, a._3, a._4, a._5))
    else if (h == 'B') findAllChars(t, (a._1, a._2 + 1, a._3, a._4, a._5))
    else if (h == 'C') findAllChars(t, (a._1, a._2, a._3 + 1, a._4, a._5))
    else if (h == 'D') findAllChars(t, (a._1, a._2, a._3, a._4 + 1, a._5))
    else findAllChars(t, (a._1, a._2, a._3, a._4, a._5 + 1))
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
      case h :: Nil => None
      case h :: b :: t => if (h == b) Some(n) else findFirst(b :: t, n + 1)
    }
    @annotation.tailrec
    def findLast(s: List[Char], n: Int, a: Int): Int = s match {
      case Nil => a
      case h :: Nil => a
      case h :: b :: t => if (h == b) findLast(b :: t, n + 1, n) else findLast(b :: t, n + 1, a)
    }
    findFirst(s, 0) match {
      case None => false
      case Some(n) => (n == findLast(s.drop(n + 1), n + 1, n)) && (n == charToNumberWithDelta(s(1), 5))
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
    findNumberOfChar(s, 'A', 0) == charToNumberWithDelta(s(3), 4)

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

  /**check the answer 10:
    * Number of questions with answer B precinig this one is? A == 0, B == 1, C == 2, D == 3, E == 4
    */
  def check10(s: List[Char]): Boolean =
  findNumberOfChar(s.take(10), 'B', 0) == charToNumberWithDelta(s(10), 0)

  /**check the answer 11:
    * Number of questions with answer being consonant (B, C or D)? A == is even, B == is odd, C == is full square, D == is prime, E == is divisible by 5
    */
  def check11(s: List[Char]): Boolean = {
    val v11 = s(11)
    val n = s.foldLeft(0)((x, c) => if (c == 'B' || c == 'C' || c == 'D') x + 1 else x)
    ((v11 == 'A' && n % 2 == 0)
    || (v11 == 'B' && n % 2 == 1)
    || (v11 == 'C' && List(1, 4, 9, 16).contains(n))
    || (v11 == 'D' && List(1, 2, 3, 5, 7, 11, 13, 17, 19).contains(n))
    || (v11 == 'E' && List(5, 10, 15).contains(n)))
  }

  /**check the answer 12:
    * The only question with even number and answer A is? A == 8, B == 10, C == 12, D == 14, E == 16
    */
  def check12(s: List[Char]): Boolean = {
    def findFirstEvenA(s: List[Char], n: Int, e: Boolean): Option[Int] = s match {
      case Nil => None
      case h :: t => if (h == 'A' && e) Some(n) else findFirstEvenA(t, n + 1, !e)
    }
    @annotation.tailrec
    def findLastEvenA(s: List[Char], n: Int, a: Int, e: Boolean): Int = s match {
      case Nil => a
      case h :: t => if (h == 'A' && e) findLastEvenA(t, n + 1, n, !e) else findLastEvenA(t, n + 1, a, !e)
    }
    findFirstEvenA(s, 0, true) match {
      case None => false
      case Some(n) => (n == findLastEvenA(s.drop(n + 1), n + 1, n, false)) && (n == (charToNumberWithDelta(s(12), 4) * 2))
    }
  }

  /**check the answer 13:
    * Number of questions with answer D is? A == 6, B == 7, C == 8, D == 9, E == 10
    */
  def check13(s: List[Char]): Boolean =
    findNumberOfChar(s, 'D', 0) == charToNumberWithDelta(s(13), 6)

  /**check the answer 14:
    * Answer to the question 11 is? A == A, B == B, C == C, D == D, E == E
    */
  def check14(s: List[Char]): Boolean =
    s(14) == s(11)

  /**check the answer 15:
    * Answer to the question 9 is? A == D, B == C, C == B, D == A, E == E
    */
  def check15(s: List[Char]): Boolean = {
    val v09 = s(9)
    val v15 = s(15)
    ((v15 == 'A' && v09 == 'D')
    || (v15 == 'B' && v09 == 'C')
    || (v15 == 'C' && v09 == 'B')
    || (v15 == 'D' && v09 == 'A')
    || (v15 == 'E' && v09 == 'E'))
  }

  /**check the answer 16:
    * Answer to the question 6 is? A == C, B == D, C == E, D == None of above, E == All of above
    */
  def check16(s: List[Char]): Boolean = {
    val v05 = s(5)
    val v16 = s(16)
    ((v16 == 'A' && v05 == 'C')
      || (v16 == 'B' && v05 == 'D')
      || (v16 == 'C' && v05 == 'E')
      || (v16 == 'D' && v05 != 'C' && v05 != 'D' && v05 != 'E'))
  }



  /**check the answer 17:
    * Number of questions with answer A eqauls to number of questions with answer? A == B, B == C, C == D, D == E, E == None of above
    */
  def check17(s: List[Char]): Boolean = {
    val v17 = s(17)
    val n = findAllChars(s, (0, 0, 0, 0, 0))
    ((v17 == 'A' && n._1 == n._2)
    || (v17 == 'B' && n._1 == n._3)
    || (v17 == 'C' && n._1 == n._4)
    || (v17 == 'D' && n._1 == n._5)
    || (v17 == 'E' && n._1 != n._2 && n._1 != n._3 && n._1 != n._4 && n._1 != n._5))
  }

  /**check the answer 18:
    * Answer to this question is? A == A, B == B, C == C, D == D, E == E
    */
  def check18(s: List[Char]): Boolean = true

  /**check the answer 19:
    * Snadartized test is related to intelligence as barometer to? A == temperature (only), B == wind speed (only),
    * C == latitude (only), D == longtitude (only), E == all of the above
    * I do not know the answer to this question so the check is always true
    */
  def check19(s: List[Char]): Boolean = true


  /* possible answers to the questions.
  Should not be used in search as results in quite big number of solution candidates
  */
  val variants = List('A', 'B', 'C', 'D', 'E')

  /**smart filling of variants based on rules
    * mind that list is built in reverse mode by depthSearch
    * List('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
    * List('D', 'A', 'D', 'B', 'E', 'D', 'D', 'E', 'D', 'A', 'B', 'A', 'D', 'B', 'A', 'D', 'B', 'E', 'C', 'A')
    * List('D', 'A', 'D', 'B', 'E', 'D', 'D', 'E', 'D', 'A', 'B', 'A', 'D', 'B', 'A', 'D', 'B', 'A', 'E', 'B')
    * List('D', 'A', 'D', 'B', 'E', 'D', 'D', 'E', 'D', 'A', 'B', 'A', 'D', 'B', 'A', 'D', 'B', 'A', 'B', 'E')
  */
  def smartVariants(d: Int, p: List[Char]): List[Char] = {
    val o = if (d < 17) findAllChars(p, (0, 0, 0, 0, 0)) else (0, 0, 0, 0, 0)
    var variants = ListBuffer('A', 'B', 'C', 'D', 'E')
    if (d == 6) if (p(10) == 'B') variants = ListBuffer('D') else variants = ListBuffer('B')
    if (d == 12) variants = ListBuffer(p(2))
    if (d == 10) variants = ListBuffer('A')
    if (d == 16) variants = ListBuffer('D')
    if (d == 17) variants = ListBuffer('B', 'D')
    if (d == 7) if (p.head == 'A') variants = ListBuffer('C')
                else if (p.head == 'B' || p.head == 'D') variants = ListBuffer()
                else if (p.head == 'C') variants = ListBuffer('D')
    if (d == 4 && p.head == 'D') variants = ListBuffer('D')
    if (d == 3 && p(1) == 'C') variants = ListBuffer('C')
    if (d == 2 && p(2) == 'B') variants = ListBuffer('B')
    if (d < 6 || d > 10) variants -= p.head
    if (List(1, 3, 5, 7, 19).contains(d)) variants -= 'A'
    if (d < 7 && findNumberOfChar(p.take(10 - d), 'B', 0) == 4) variants -= 'B'
    if (d == 9 && p.head != 'A') variants -= 'A'
    if (d == 9 && p(1) != 'B') variants -= 'B'
    if (d == 9 && p(2) != 'C') variants -= 'C'
    if (d == 9 && p(3) != 'D') variants -= 'D'
    if (d == 9 && p(4) != 'E') variants -= 'E'
    if (d < 13 && (o._1 + o._5) == 8) variants -= ('A', 'E')
    if (d < 13 && o._1 == 8) variants -= 'A'
    if (d < 17 && o._5 == 4) variants -= 'E'
    variants.toList
  }

  /* All checks together*/
  val checks: List[List[Char] => Boolean] = List(check00(_), check01(_), check02(_), check03(_), check04(_), check04(_),
    check05(_), check06(_), check07(_), check08(_), check09(_), check10(_), check11(_), check12(_), check13(_),
    check14(_), check15(_), check16(_), check17(_), check18(_), check19(_))

  /* Perform list of checks on answer*/
  @annotation.tailrec
  def checkAnswer(a: List[Char], c: List[List[Char] => Boolean]): Boolean = c match {
    case Nil => true
    case h :: t => if (h(a)) checkAnswer(a, t) else false
  }

  /*depth search that checks if any of answers with given length passes all checks */
  def depthSearch(d: Int, a: List[Char], v: List[Char]): Option[List[Char]] = {
    if (d == 0) if (a.length == 20 && checkAnswer(a, checks)) Some(a) else None
    else v match {
      case Nil => None
      case h :: t =>  depthSearch(d - 1, h :: a, smartVariants(d - 1, h :: a)) match {
        case None => depthSearch(d, a, t)
        case Some(r) => Some(r)
      }
    }
  }

  /**solve the problem using depth search with suggestion of the answer to last question
  * I hoped that only one will work, however there is at least one solution for answers A, B, and E
  * */
  def solution: Option[List[Char]] =
    depthSearch(19, List('E'), smartVariants(20, List('E')))
}