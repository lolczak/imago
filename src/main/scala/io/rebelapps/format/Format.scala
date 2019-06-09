package io.rebelapps.format

abstract class Format[A](val read: Vector[Byte] => ReadResult[A])
                        (val write: A => Vector[Byte]) {
  self =>

  def imap[B](f: A => B)(g: B => A): Format[B] = Format(read andThen (_.map(f)))(write compose g)

  def ~[B](right: Format[B]): Format[A ~ B] = {
    val write: A ~ B => Vector[Byte] = {
      case a ~ b => self.write(a) ++ right.write(b)
    }

    val read: Vector[Byte] => ReadResult[A ~ B] = { bytes =>
      self.read(bytes) match {
        case NoMatch => NoMatch
        case Match(result, rest) => right.read(rest).map(b => new ~(result, b))
      }
    }

    Format(read)(write)
  }

}

object Format {

  def apply[A](read: Vector[Byte] => ReadResult[A])
              (write: A => Vector[Byte]): Format[A] = new Format[A](read)(write) {}

}

case class ~[+A, +B](left: A, right: B)
