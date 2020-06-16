package com.twitter.util

import scala.util.Random

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class U64Test extends WordSpec {
  import U64._

  "comparable" in {
    {
      val a = 0x0000000000000001L
      assert(a == 1)
      val b = 0x0000000000000002L
      assert(b == 2)

      assert(a.u64_<(b) == true)
      assert(b.u64_<(a) == false)
    }

    {
      val a = 0xffffffffffffffffL
      assert(a == -1)
      val b = 0xfffffffffffffffeL
      assert(b == -2)

      assert(a.u64_<(b) == false)
      assert(b.u64_<(a) == true)
    }

    {
      val a = 0xffffffffffffffffL
      assert(a == -1)
      val b = 0x0000000000000001L
      assert(b == 1)

      assert(a.u64_<(b) == false)
      assert(b.u64_<(a) == true)
    }
  }

  "comparable in range" in {
    assert(0L.u64_within(0, 1) == false)
    assert(0L.u64_contained(0, 1) == true)

    // (inverted range)
    assert(0L.u64_within(-1, 1) == false)
    assert(1L.u64_within(-1, 1) == false)
    assert(2L.u64_within(-1, 1) == false)

    assert(
      0xfffffffffffffffeL
        .u64_within(0xfffffffffffffffdL, 0xffffffffffffffffL) == true)
    assert(
      0xfffffffffffffffdL
        .u64_within(0xfffffffffffffffdL, 0xffffffffffffffffL) == false)
    assert(
      0xffffffffffffffffL
        .u64_within(0xfffffffffffffffdL, 0xffffffffffffffffL) == false)

    assert(
      0xfffffffffffffffeL
        .u64_contained(0xfffffffffffffffdL, 0xffffffffffffffffL) == true)
    assert(
      0xfffffffffffffffdL
        .u64_contained(0xfffffffffffffffdL, 0xffffffffffffffffL) == true)
    assert(
      0xffffffffffffffffL
        .u64_contained(0xfffffffffffffffdL, 0xffffffffffffffffL) == true)

    // Bit flip area!
    assert(
      0x7fffffffffffffffL
        .u64_within(0x7fffffffffffffffL, 0x8000000000000000L) == false)
    assert(
      0x8000000000000000L
        .u64_within(0x7fffffffffffffffL, 0x8000000000000000L) == false)

    assert(
      0x7fffffffffffffffL
        .u64_contained(0x7fffffffffffffffL, 0x8000000000000000L) == true)
    assert(
      0x8000000000000000L
        .u64_contained(0x7fffffffffffffffL, 0x8000000000000000L) == true)

    assert(
      0x7ffffffffffffffaL
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == false)
    assert(
      0x7ffffffffffffffbL
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == true)
    assert(
      0x7fffffffffffffffL
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == true)
    assert(
      0x8000000000000000L
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == true)
    assert(
      0x8000000000000001L
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == true)
    assert(
      0x8000000000000009L
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == true)
    assert(
      0x800000000000000aL
        .u64_within(0x7ffffffffffffffaL, 0x800000000000000aL) == false)
  }

  "divisible" in {
    assert(10L.u64_/(5L) == 2L)
    assert(0xffffffffffffffffL / 0x0fffffffffffffffL == 0L)
    assert(0xffffffffffffffffL.u64_/(0x0fffffffffffffffL) == 16L)

    assert(0x7fffffffffffffffL.u64_/(2) == 0x3fffffffffffffffL)

    assert(0x8000000000000000L / 2 == 0xc000000000000000L)
    assert(0x8000000000000000L.u64_/(2) == 0x4000000000000000L)

    assert(0x8000000000000000L.u64_/(0x8000000000000000L) == 1)
    assert(0x8000000000000000L.u64_/(0x8000000000000001L) == 0)

    assert(0xff00000000000000L.u64_/(0x0f00000000000000L) == 0x11L)
    assert(0x8f00000000000000L.u64_/(0x100) == 0x008f000000000000L)
    assert(0x8000000000000000L.u64_/(3) == 0x2aaaaaaaaaaaaaaaL)
  }

  "ids" should {
    "survive conversions" in {
      val rng = new Random

      (0 until 10000).foreach { _ =>
        val id = rng.nextLong
        assert(id == (id.toU64ByteArray.toU64Long))
        assert(id == (id.toU64ByteArray.toU64HexString.toU64ByteArray.toU64Long))
      }
    }

    "be serializable" in {
      assert(0L.toU64HexString == "0000000000000000")
      assert(0x0102030405060700L.toU64HexString == "0102030405060700")
      assert(0xfff1f2f3f4f5f6f7L.toU64HexString == "fff1f2f3f4f5f6f7")
    }

    "convert from short hex string" in {
      assert(new RichU64String("7b").toU64Long == 123L)
    }

    "don't silently truncate" in {
      intercept[NumberFormatException] {
        new RichU64String("318528893302738945")
      }
    }

    "not parse with +" in {
      intercept[NumberFormatException] { "+0".toU64Long }
      intercept[NumberFormatException] { "0+".toU64Long }
      intercept[NumberFormatException] { "00+0".toU64Long }
      intercept[NumberFormatException] { "0+00".toU64Long }
      intercept[NumberFormatException] { "+ffffffffffffffff".toU64Long }
    }

    "not parse non-latin unicode digits" in {
      // \u09e6 = BENGALI DIGIT ZERO (accepted by parseLong)
      intercept[NumberFormatException] { "\u09e6".toU64Long }
    }

    "parse mixed case" in {
      assert(new RichU64String("aBcDeF").toU64Long == 0xabcdef)
    }

    "not parse if negative" in {
      val actual = intercept[NumberFormatException] { "-1".toU64Long }
      assert(actual.getMessage == "For input string: \"-1\"")

      intercept[NumberFormatException] { "".toU64Long }
      intercept[NumberFormatException] { "-f".toU64Long }
      intercept[NumberFormatException] { "-af".toU64Long }
      intercept[NumberFormatException] { "10-aff".toU64Long }
      intercept[NumberFormatException] { "1-0aff".toU64Long }
    }

    "not parse empty string" in {
      // this is what Long.parseLong("") does
      intercept[NumberFormatException] { "".toU64Long }
    }
  }
}
