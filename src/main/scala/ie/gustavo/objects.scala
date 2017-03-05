package ie.gustavo

import java.io.Serializable

case class Plane(inputData: List[String]) {
  val totalSeats    = inputData.map(_.toInt).product
  val numberOfSeats = inputData.head.toInt
  val numberOfRows  = inputData(1).toInt
}

class SeatAllocator(plane: Plane, groupsOfPassengers: List[List[String]]) {
   /**
    * Removes the "W" char from the passenger representation, only for presentation purposes
    */
  val formatSeatAllocationResult: Iterable[List[String]] => String = _.map {
    allocateWindowRequest(_).mkString(" ").replaceAll("W", "")
  }.mkString("\n")

  /**
    * Given a Plane row, move the passengers with Window preference to the Plane's Window
    * @param passengers List[String])
    * @return a Row in the Plane
    */
  def allocateWindowRequest(passengers: List[String]): List[Serializable] = {
    val window   = passengers filter {  _.contains("W") }
    val noWindow = passengers filter { !_.contains("W") }
    import scala.util.Try
    Try(window.head).getOrElse(Nil) +: noWindow :+ Try(window.last).getOrElse(Nil) filter { _ != Nil } take plane.numberOfSeats
  }

  /**
    * Determines the best sitting arrangements for a given the Flight
    * The approach is to group the passengers list by length,
    * and then zip the obtained map's keys when the sum equals the numberOfSeats in the plane.
    * @return the allocation arrangement List, one List per row
    */
  def allocate(): Iterable[List[String]] = {
    val groupsOfPassengersByLength = groupsOfPassengers.groupBy {
      _.length
    }
    val parts = groupsOfPassengersByLength.partition(_._2.flatten.size % plane.numberOfSeats == 0)
    val almostThere = parts._1
    val notQuiteYet = parts._2
    val resX = almostThere.map {
      _._2.flatten
    }
    val resY = (notQuiteYet(1) zip notQuiteYet(3)).map { case (x, y) => x ::: y }
    (resX ++ resY) take plane.numberOfRows
  }

  /**
    * Presentation for the computation of "sitting arrangements" and "customer satisfaction"
    * @return the allocation result string
    */
  def seatArrangementsReport(): String = {
    s"${formatSeatAllocationResult(allocate())}\n${customerSatisfaction()}"
  }

  /**
    *
    * @return the customer satisfaction result percentage
    */
  def customerSatisfaction(): String = {
    val totalPassengers = groupsOfPassengers.flatten.length
    val satisfiedPassengers = allocate().flatten.size
    val result = satisfiedPassengers / totalPassengers * 100
    s"$result%"
  }
}

class Flight(path: String) {
  val inputLines = io.Source
    .fromFile(path)
    .getLines
    .toList

  val sanitised: List[List[String]] = inputLines map {
    _.split(" ").toList
  }

  val groupsOfPassengers = sanitised.tail
  val myPlane = Plane(sanitised.head)
  val seatAllocator = new SeatAllocator(myPlane, groupsOfPassengers)

  def printReport() = seatAllocator.seatArrangementsReport()
}