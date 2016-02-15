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
}
