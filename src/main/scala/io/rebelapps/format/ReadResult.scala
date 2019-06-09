package io.rebelapps.format

sealed trait ReadResult[+A] {

  def map[B](f: A => B): ReadResult[B]

}
case class Match[A](result: A, rest: Vector[Byte]) extends ReadResult[A] {
  override def map[B](f: A => B): ReadResult[B] = this.copy[B](result = f(result))
}
case object NoMatch extends ReadResult[Nothing] {
  override def map[B](f: Nothing => B): ReadResult[B] = this
}
