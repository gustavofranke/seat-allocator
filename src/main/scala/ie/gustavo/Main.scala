package ie.gustavo

object Main extends App {
  require(args.length == 1, "\n Usage: sbt \"path to the input file\"")
  val file = args(0)
  println(s"""\nWelcome! your input parameter is:\n - Path to the file to analise: $file""")

  val flight = new Flight(file)
  println(flight.printReport)
}
