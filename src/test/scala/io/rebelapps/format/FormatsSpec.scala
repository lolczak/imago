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
    (Int.MinValue to Int.MaxValue).by(100) foreach  { int =>
      val bytes = Formats.beInt.write(int)
      Formats.beInt.read(bytes) shouldBe Match(int, Vector.empty)
    }
  }

}
