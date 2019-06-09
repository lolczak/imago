package io.rebelapps.format

object Formats {

  val byte: Format[Byte] = Format { bytes =>
    if (bytes.isEmpty) NoMatch
    else Match(bytes(0), bytes.tail)
  }(Vector(_))

  val beShort: Format[Short] =
    Format { bytes =>
      if (bytes.length >= 2) {
        val b1 = (bytes(0) & 0xFF) << 8
        val b2 = bytes(1) & 0xFF

        val short = b1 | b2
        Match(short.toShort, bytes.drop(2))
      } else
        NoMatch
    } { short =>
      val b1 = short >>> 8
      val b2 = short & 0xFF

      Vector(b1.toByte, b2.toByte)
    }


  val beInt: Format[Int] =
    Format { bytes =>
      if (bytes.length >= 4) {
        val b1 = (bytes(0) & 0xFF) << 24
        val b2 = (bytes(1) & 0xFF) << 16
        val b3 = (bytes(2) & 0xFF) << 8
        val b4 = bytes(3) & 0xFF

        val value = b1 | b2 | b3 | b4
        Match(value, bytes.drop(4))
      } else
        NoMatch
    } { value =>
      val b1 = value >>> 24
      val b2 = (value >>> 16) & 0xFF
      val b3 = (value >>> 8) & 0xFF
      val b4 = value & 0xFF

      Vector(b1.toByte, b2.toByte, b3.toByte, b4.toByte)
    }

}
