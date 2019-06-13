package io.rebelapps.format

import org.scalatest.{FlatSpec, Matchers}

class FormatsSpec extends FlatSpec with Matchers {

  "A short value" should "be serialized in big-endian form" in {
    (Short.MinValue to Short.MaxValue) foreach  { int =>
      val short = int.toShort
      val bytes = Formats.beShort.write(short)
      Formats.beShort.read(bytes) shouldBe Match(short, Vector.empty)
    }
  }

  "A int value" should "be serialized in big-endian form" in {
    (Int.MinValue to Int.MaxValue).by(1000) foreach  { int =>
      val bytes = Formats.beInt.write(int)
      Formats.beInt.read(bytes) shouldBe Match(int, Vector.empty)
    }
  }

  "A uint16" should "be serialized in big-endian form" in {
    (0 to (2^16-1)) foreach  { int =>
      val bytes = Formats.uint16.write(int)
      Formats.uint16.read(bytes) shouldBe Match(int, Vector.empty)
    }
  }

}
