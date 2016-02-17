package solution
import org.scalatest.FunSuite


/**
  * Created by CrayWind on 2016-02-14.
  */
class SolverTest extends FunSuite {

  test("testCheck00 correct answer") {
    assert(Solver.check00(List('C', '1', 'B')))
  }

  test("testCheck00 answer B") {
    assert(!Solver.check00(List('B', '1', 'B')))
  }

  test("testCheck00 wrong answer") {
    assert(!Solver.check00(List('C', '1', '2', 'B')))
  }

  test("testCheck01 correct answer") {
    assert(Solver.check01(List('0', 'B', '2', '3', '4', '5', 'C', 'C')))
  }

  test("testCheck01 check multiple sequences") {
    assert(!Solver.check01(List('B', 'B', 'B')))
  }

  test("testCheck01 no sequences") {
    assert(!Solver.check01(List('0', 'B', '2', '3')))
  }

  test("testCheck01 incorrect answer") {
    assert(!Solver.check01(List('0', 'B', '1', '2', '3', 'B', 'B', '6')))
  }

  test("testCheck02 correct answer") {
    assert(Solver.check02(List('0', '1', 'A', '3', '4', '5', '6', '7')))
  }

  test("testCheck02 incorrect answer") {
    assert(!Solver.check02(List('0', '1', 'A', '3', '4', '5', 'E', '7')))
  }

  test("testCheck02 no E while expected") {
    assert(!Solver.check02(List('0', '1', 'B', '3', '4', '5', '6', '7')))
  }

  test("testCheck03 correct answer") {
    assert(Solver.check03(List('A', '1', 'A', 'A', 'A', '5', '6', '7')))
  }

  test("testCheck03 incorrect answer") {
    assert(!Solver.check03(List('A', '1', 'A', 'D', 'A', '5', '6', '7')))
  }

  test("testCheck03 no A while expected") {
    assert(!Solver.check03(List('0', '1', '2', 'D', '4', '5', '6', '7')))
  }

  test("testCheck04 correct answer") {
    assert(Solver.check04(List('A', '1', '2', '3', 'A', '5', '6', '7')))
  }

  test("testCheck04 incorrect answer") {
    assert(!Solver.check04(List('0', '1', 'A', '3', 'A', '5', '6', '7')))
  }

  test("testCheck05 correct answer B") {
    assert(Solver.check05(List('0', '1', '2', '3', '4', 'B', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'D')))
  }

  test("testCheck05 correct answer D") {
    assert(Solver.check05(List('0', '1', '2', '3', '4', 'D', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'A')))
  }

  test("testCheck05 answer E (has no branch)") {
    assert(!Solver.check05(List('0', '1', '2', '3', '4', 'E', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6')))
  }

  test("testCheck05 incorrect answer C") {
    assert(!Solver.check05(List('0', '1', '2', '3', '4', 'C', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'D')))
  }

  test("testCheck06 correct answer A6 > A7") {
    assert(Solver.check06(List('0', '1', '2', '3', '4', '5', 'C', 'A')))
  }

  test("testCheck06 correct answer A6 < A7") {
    assert(Solver.check06(List('0', '1', '2', '3', '4', '5', 'A', 'E')))
  }

  test("testCheck06 incorrect answer") {
    assert(!Solver.check06(List('0', '1', '2', '3', '4', '5', 'C', 'D')))
  }

  test("testCheck07 correct answer") {
    assert(Solver.check07(List('E', '1', 'A', 'E', 'A', '5', 'E', 'B')))
  }

  test("testCheck07 incorrect answer") {
    assert(!Solver.check07(List('E', '1', 'A', 'E', 'A', '5', 'E', 'A')))
  }

  test("testCheck08 correct answer") {
    assert(Solver.check08(List('0', '1', '2', '3', '4', '5', '6', '7', 'A', 'A', '0')))
  }

  test("testCheck08 incorrect answer") {
    assert(!Solver.check08(List('0', '1', '2', '3', '4', '5', '6', '7', 'A', '9', 'A')))
  }

  test("testCheck09 correct answer") {
    assert(Solver.check09(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'A', '0', '1', '2', '3', '4', 'D')))
  }

  test("testCheck09 incorrect answer") {
    assert(!Solver.check09(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'C', '0', '1', '2', '3', '4', 'B')))
  }

  test("testCheck10 correct answer") {
    assert(Solver.check10(List('0', '1', '2', 'B', '4', '5', '6', '7', '8', '9', 'B')))
  }

  test("testCheck10 incorrect answer") {
    assert(!Solver.check10(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'B')))
  }

  test("testCheck11 even number") {
    assert(Solver.check11(List('0', 'B', '2', 'C', '4', '5', '6', '7', '8', '9', '0', 'A')))
  }

  test("testCheck11 odd number") {
    assert(Solver.check11(List('0', 'B', '2', 'C', '4', '5', '6', '7', '8', '9', '0', 'B')))
  }

  test("testCheck11 full square") {
    assert(Solver.check11(List('0', 'B', '2', 'C', '4', 'D', '6', '7', '8', '9', '0', 'C')))
  }

  test("testCheck11 prime number") {
    assert(Solver.check11(List('0', 'B', '2', 'C', '4', '5', '6', '7', '8', '9', '0', 'D')))
  }

  test("testCheck11 divisible by 5") {
    assert(Solver.check11(List('0', 'B', '2', 'C', '4', 'D', '6', 'B', '8', 'C', '0', 'E')))
  }

  test("testCheck11 incorrect answer") {
    assert(!Solver.check11(List('0', 'B', '2', 'C', '4', 'D', '6', '7', '8', '9', '0', 'A')))
  }

  test("testCheck12 correct answer") {
    assert(Solver.check12(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', '1', 'B')))
  }

  test("testCheck12 incorrect answer") {
    assert(!Solver.check12(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', '1', 'C')))
  }

  test("testCheck12 more than 1 even A") {
    assert(!Solver.check12(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', '1', 'A')))
  }

  test("testCheck13 correct answer") {
    assert(Solver.check13(List('0', 'D', '2', 'D', '4', 'D', '6', 'D', '8', 'D', '0', 'D', '2', 'A')))
  }

  test("testCheck13 incorrect answer") {
    assert(!Solver.check13(List('0', 'D', '2', 'D', '4', 'D', '6', 'D', '8', 'D', '0', 'D', '2', 'B')))
  }

  test("testCheck13 no D") {
    assert(!Solver.check13(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', 'A')))
  }

  test("testCheck14 correct answer") {
    assert(Solver.check14(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'D', '2', '3', 'D')))
  }

  test("testCheck14 incorrect answer") {
    assert(!Solver.check14(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'D', '2', '3', 'C')))
  }

  test("testCheck15 correct answer") {
    assert(Solver.check15(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'A', '0', '1', '2', '3', '4', 'D')))
  }

  test("testCheck15 incorrect answer") {
    assert(!Solver.check15(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'D', '0', '1', '2', '3', '4', 'B')))
  }

  test("testCheck16 correct answer B") {
    assert(Solver.check16(List('0', '1', '2', '3', '4', 'D', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'B')))
  }

  test("testCheck16 correct answer D") {
    assert(Solver.check16(List('0', '1', '2', '3', '4', 'A', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'D')))
  }

  test("testCheck16 answer E (has no branch)") {
    assert(!Solver.check16(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'E')))
  }

  test("testCheck16 incorrect answer C") {
    assert(!Solver.check16(List('0', '1', '2', '3', '4', 'D', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', 'C')))
  }

  test("testCheck17 correct answer A") {
    assert(Solver.check17(List('0', 'B', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', 'A')))
  }

  test("testCheck17 correct answer E") {
    assert(Solver.check17(List('0', '1', 'A', 'A', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', 'E')))
  }

  test("testCheck17 incorrect answer A") {
    assert(!Solver.check17(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', 'A')))
  }

  test("testCheck18 any answer works") {
    assert(Solver.check18(List('0', '1', '2', '3')))
  }

  test("testCheck19 any answr works") {
    assert(Solver.check19(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', 'C')))
  }

  test("test char to number on A with some delta") {
    assert(Solver.charToNumberWithDelta('A', 1) == 1)
  }

  test("test char to number on C") {
    assert(Solver.charToNumberWithDelta('C', 0) == 2)
  }

  test("test char to number on '3' with some delta") {
    assert(Solver.charToNumberWithDelta('3', 5) == -9)
  }

  test("test findNumberOfChar on empty list") {
    assert(Solver.findNumberOfChar(List(), 'A', 0) == 0)
  }

  test("test findNumberOfChar on list that does not contain it and non zero start") {
    assert(Solver.findNumberOfChar(List('B', 'C'), 'A', 1) == 1)
  }

  test("test findNumberOfChar on list that contains several occurrence of char") {
    assert(Solver.findNumberOfChar(List('1', 'A', '2', 'A'), 'A', 0) == 2)
  }

  test("test findAllChars on list that does not contain A, B, C, D (all list elements are counted towards E)") {
    assert(Solver.findAllChars(List('0', '1', '2'), (4, 3, 2, 1, 0)) == (4, 3, 2, 1, 3))
  }

  test("test that findAllChars returns correct number of occurences") {
    assert(Solver.findAllChars(List('A', 'B', 'B', 'C', 'C', 'C', 'D', 'D', 'E'), (0, 0, 0, 0, 0)) == (1, 2, 3, 2, 1))
  }

  test("get final result") {
    assert(!(Solver.solution == None))
  }
}
