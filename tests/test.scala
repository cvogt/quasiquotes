
object Test extends App {
  import quasiquotes._
  val two = 2
  val four = 4
  println(q"one + two")
}
