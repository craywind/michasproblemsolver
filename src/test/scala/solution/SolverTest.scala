package solution
import org.scalatest.FunSuite


/**
  * Created by CrayWind on 2016-02-14.
  */
class SolverTest extends FunSuite {

  test("testCheck00 correct answer") {
    assert(Solver.check00(List('C', ' ', 'B')))
  }

  test("testCheck00 answer B") {
    assert(!Solver.check00(List('B', ' ', 'B')))
  }

  test("testCheck00 wrong answer") {
    assert(!Solver.check00(List('C', ' ', ' ', 'B')))
  }

  test("testCheck01 correct answer") {
    assert(Solver.check01(List('A', 'B', 'C', 'D', 'A', 'B', 'C', 'C')))
  }

  test("testCheck01 check multiple sequences") {
    assert(!Solver.check01(List('B', 'B', 'B')))
  }

  test("testCheck01 no sequences") {
    assert(!Solver.check01(List('A', 'B', 'C', 'D')))
  }

  test("testCheck01 incorrect answer") {
    assert(!Solver.check01(List('A', 'B', 'C', 'D', 'A', 'B', 'B', 'C')))
  }

  test("testCheck02 correct answer") {
    assert(Solver.check02(List('A', 'B', 'A', 'D', 'A', 'B', 'B', 'C')))
  }

  test("testCheck02 incorrect answer") {
    assert(!Solver.check02(List('A', 'B', 'A', 'D', 'A', 'B', 'E', 'C')))
  }

  test("testCheck02 no E while expected") {
    assert(!Solver.check02(List('A', 'B', 'B', 'D', 'A', 'B', 'D', 'C')))
  }

  test("testCheck03 correct answer") {
    assert(Solver.check03(List('A', 'B', 'A', 'A', 'A', 'B', 'B', 'C')))
  }

  test("testCheck03 incorrect answer") {
    assert(!Solver.check03(List('A', 'B', 'A', 'D', 'A', 'B', 'E', 'C')))
  }

  test("testCheck03 no A while expected") {
    assert(!Solver.check03(List('E', 'B', 'B', 'D', 'E', 'B', 'D', 'C')))
  }

  test("testCheck04 correct answer") {
    assert(Solver.check04(List('A', 'B', 'A', 'A', 'A', 'B', 'B', 'C')))
  }

  test("testCheck04 incorrect answer") {
    assert(!Solver.check04(List('E', 'B', 'A', 'D', 'A', 'B', 'E', 'C')))
  }

  test("testCheck05 correct answer B") {
    assert(Solver.check05(List('A', 'B', 'A', 'A', 'B', 'B', 'B', 'C', 'A', 'B', 'A', 'A', 'A', 'B', 'B', 'C', 'D')))
  }

  test("testCheck05 correct answer D") {
    assert(Solver.check05(List('A', 'B', 'A', 'A', 'B', 'D', 'B', 'C', 'A', 'B', 'A', 'A', 'A', 'B', 'B', 'C', 'A')))
  }

  test("testCheck05 answer E") {
    assert(!Solver.check05(List('A', 'B', 'A', 'A', 'B', 'E', 'B', 'C', 'A', 'B', 'A', 'A', 'A', 'B', 'B', 'C', 'D')))
  }

  test("testCheck05 incorrect answer C") {
    assert(!Solver.check05(List('A', 'B', 'A', 'A', 'B', 'C', 'B', 'C', 'A', 'B', 'A', 'A', 'A', 'B', 'B', 'C', 'D')))
  }

  test("testCheck06 correct answer A6 > A7") {
    assert(Solver.check06(List('E', 'B', 'A', 'D', 'A', 'B', 'C', 'A')))
  }

  test("testCheck06 correct answer A6 < A7") {
    assert(Solver.check06(List('E', 'B', 'A', 'D', 'A', 'B', 'A', 'E')))
  }

  test("testCheck06 incorrect answer") {
    assert(!Solver.check06(List('E', 'B', 'A', 'D', 'A', 'B', 'C', 'D')))
  }

  test("testCheck07 correct answer") {
    assert(Solver.check07(List('E', 'B', 'A', 'E', 'A', 'B', 'E', 'B')))
  }

  test("testCheck07 incorrect answer") {
    assert(!Solver.check07(List('E', 'B', 'A', 'E', 'A', 'B', 'E', 'A')))
  }

  test("testCheck08 correct answer") {
    assert(Solver.check08(List('A', 'B', 'A', 'A', 'A', 'B', 'B', 'C', 'A', 'A', 'B')))
  }

  test("testCheck08 incorrect answer") {
    assert(!Solver.check08(List('E', 'B', 'A', 'D', 'A', 'B', 'E', 'C', 'A', 'B', 'A')))
  }

  test("testCheck09 correct answer") {
    assert(Solver.check09(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'A', '0', '1', '2', '3', '4', 'D')))
  }

  test("testCheck09 incorrect answer") {
    assert(!Solver.check09(List('0', '1', '2', '3', '4', '5', '6', '7', '8', 'C', '0', '1', '2', '3', '4', 'B')))
  }
}
