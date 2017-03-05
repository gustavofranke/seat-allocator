import ie.gustavo.{Plane, SeatAllocator}
import org.scalatest.FunSuite

class SeatAllocatorSuite extends FunSuite {
  val myPlane = Plane(List("4", "4"))

  test("allocateWindowRequest should move the passengers that required a window seat") {
    val seatAllocator = new SeatAllocator(myPlane, List())

    def check(underTest: List[String], expected: List[String]) = assert(seatAllocator.allocateWindowRequest(underTest) === expected)

    check(List("9", "10", "11W", "12W"), List("11W", "9", "10", "12W"))
    check(List("1W", "2", "3", "8"), List("1W", "2", "3", "8"))
    check(List("4", "5", "6", "7"), List("4", "5", "6", "7"))
    check(List("4W", "5W", "6W", "7W"), List("4W", "7W")) // TODO: review this example
  }

  test("allocate test works under the assumption that two rows of passengers are equal when the group destined to that row ends up there, that is two lists are equal if they have the same elements") {
    val groupsOfPassengers = List(List("1W", "2", "3"), List("4", "5", "6", "7"), List("8"), List("9", "10", "11W"), List("12W"), List("13", "14"), List("15", "16"))
    val seatAllocator = new SeatAllocator(myPlane, groupsOfPassengers)

    assert(seatAllocator.allocate() === List(List("13", "14", "15", "16"), List("4", "5", "6", "7"), List("8", "1W", "2", "3"), List("12W", "9", "10", "11W")))
  }

  test("test that two lists are equal if they have the same elements, for the given seating-passengers-in-a-row context, this is before allocateWindowRequest gets called so we can work with integers") {
    assert(List(8, 1, 2, 3).toSet === List(1, 2, 3, 8).toSet)
    assert(List(12, 9, 10, 11).toSet === List(11, 9, 10, 12).toSet)
  }

}
