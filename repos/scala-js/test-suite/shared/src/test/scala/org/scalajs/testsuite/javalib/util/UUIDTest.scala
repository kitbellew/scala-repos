/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Assert._
import org.junit.Test

import java.util.UUID

import org.scalajs.testsuite.utils.AssertThrows._

class UUIDTest {

  @Test
  def constructor(): Unit = {
    val uuid = new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L)
    assertEquals(0xF81D4FAE7DEC11D0L, uuid.getMostSignificantBits())
    assertEquals(0xA76500A0C91E6BF6L, uuid.getLeastSignificantBits())
    assertEquals(2, uuid.variant())
    assertEquals(1, uuid.version())
    assertEquals(0x1D07DECF81D4FAEL, uuid.timestamp())
    assertEquals(0x2765, uuid.clockSequence())
    assertEquals(0xA0C91E6BF6L, uuid.node())
  }

  @Test
  def getLeastSignificantBits(): Unit = {
    assertEquals(0L, new UUID(0L, 0L).getLeastSignificantBits())
    assertEquals(
      Long.MinValue,
      new UUID(0L, Long.MinValue).getLeastSignificantBits())
    assertEquals(
      Long.MaxValue,
      new UUID(0L, Long.MaxValue).getLeastSignificantBits())
  }

  @Test
  def getMostSignificantBits(): Unit = {
    assertEquals(0L, new UUID(0L, 0L).getMostSignificantBits())
    assertEquals(
      Long.MinValue,
      new UUID(Long.MinValue, 0L).getMostSignificantBits())
    assertEquals(
      Long.MaxValue,
      new UUID(Long.MaxValue, 0L).getMostSignificantBits())
  }

  @Test
  def version(): Unit = {
    assertEquals(0, new UUID(0L, 0L).version())
    assertEquals(1, new UUID(0x0000000000001000L, 0L).version())
    assertEquals(2, new UUID(0x00000000000F2F00L, 0L).version())
  }

  @Test
  def variant(): Unit = {
    assertEquals(0, new UUID(0L, 0L).variant())
    assertEquals(0, new UUID(0L, 0x7000000000000000L).variant())
    assertEquals(0, new UUID(0L, 0x3FF0000000000000L).variant())
    assertEquals(0, new UUID(0L, 0x1FF0000000000000L).variant())

    assertEquals(2, new UUID(0L, 0x8000000000000000L).variant())
    assertEquals(2, new UUID(0L, 0xB000000000000000L).variant())
    assertEquals(2, new UUID(0L, 0xAFF0000000000000L).variant())
    assertEquals(2, new UUID(0L, 0x9FF0000000000000L).variant())

    assertEquals(6, new UUID(0L, 0xC000000000000000L).variant())
    assertEquals(6, new UUID(0L, 0xDF00000000000000L).variant())
  }

  @Test
  def timestamp(): Unit = {
    assertEquals(
      0L,
      new UUID(0x0000000000001000L, 0x8000000000000000L).timestamp())
    assertEquals(
      0x333555577777777L,
      new UUID(0x7777777755551333L, 0x8000000000000000L).timestamp())

    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000000000L, 0x8000000000000000L).timestamp())
    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000002000L, 0x8000000000000000L).timestamp())
  }

  @Test
  def clockSequence(): Unit = {
    assertEquals(
      0,
      new UUID(0x0000000000001000L, 0x8000000000000000L).clockSequence())
    assertEquals(
      0x0fff,
      new UUID(0x0000000000001000L, 0x8FFF000000000000L).clockSequence())
    assertEquals(
      0x3fff,
      new UUID(0x0000000000001000L, 0xBFFF000000000000L).clockSequence())

    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000000000L, 0x8000000000000000L).clockSequence())
    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000002000L, 0x8000000000000000L).clockSequence())
  }

  @Test
  def node(): Unit = {
    assertEquals(0L, new UUID(0x0000000000001000L, 0x8000000000000000L).node())
    assertEquals(
      0xFFFFFFFFFFFFL,
      new UUID(0x0000000000001000L, 0x8000FFFFFFFFFFFFL).node())

    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000000000L, 0x8000000000000000L).node())
    assertThrows(
      classOf[Exception],
      new UUID(0x0000000000002000L, 0x8000000000000000L).node())
  }

  @Test
  def compareTo(): Unit = {
    val uuid0101 = new UUID(1L, 1L)
    val uuid0111 = new UUID(1L, 0x100000001L)
    val uuid1000 = new UUID(0x100000000L, 0L)

    assertEquals(0, uuid0101.compareTo(uuid0101))
    assertEquals(0, uuid0111.compareTo(uuid0111))
    assertEquals(0, uuid1000.compareTo(uuid1000))

    assertTrue(uuid0101.compareTo(uuid0111) < 0)
    assertTrue(uuid0101.compareTo(uuid1000) < 0)
    assertTrue(uuid0111.compareTo(uuid1000) < 0)

    assertTrue(uuid0111.compareTo(uuid0101) > 0)
    assertTrue(uuid1000.compareTo(uuid0101) > 0)
    assertTrue(uuid1000.compareTo(uuid0111) > 0)
  }

  @Test
  def hashCodeTest(): Unit = {
    assertEquals(0, new UUID(0L, 0L).hashCode())
    assertEquals(
      new UUID(123L, 123L).hashCode(),
      new UUID(123L, 123L).hashCode())
  }

  @Test
  def equalsTest(): Unit = {
    val uuid1 = new UUID(0L, 0L)
    assertTrue(uuid1.equals(uuid1))
    assertFalse(uuid1.equals(null))
    assertFalse(uuid1.equals("something else"))

    val uuid2 = new UUID(0L, 0L)
    assertTrue(uuid1.equals(uuid2))

    val uuid3 = new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L)
    val uuid4 = new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L)
    assertTrue(uuid3.equals(uuid4))
    assertFalse(uuid3.equals(uuid1))

    assertFalse(
      uuid3.equals(new UUID(0x781D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L)))
    assertFalse(
      uuid3.equals(new UUID(0xF81D4FAE7DEC11D1L, 0xA76500A0C91E6BF6L)))
    assertFalse(
      uuid3.equals(new UUID(0xF81D4FAE7DEC11D0L, 0xA76530A0C91E6BF6L)))
    assertFalse(
      uuid3.equals(new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6CF6L)))
  }

  @Test
  def toStringTest(): Unit = {
    assertEquals(
      "f81d4fae-7dec-11d0-a765-00a0c91e6bf6",
      new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L).toString)
    assertEquals(
      "00000000-0000-1000-8000-000000000000",
      new UUID(0x0000000000001000L, 0x8000000000000000L).toString)
  }

  @Test
  def randomUUID(): Unit = {
    val uuid = UUID.randomUUID()
    assertEquals(2, uuid.variant())
    assertEquals(4, uuid.version())
  }

  @Test
  def fromString(): Unit = {
    val uuid1 = UUID.fromString("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
    assertTrue(uuid1.equals(new UUID(0xF81D4FAE7DEC11D0L, 0xA76500A0C91E6BF6L)))
    assertEquals(0xF81D4FAE7DEC11D0L, uuid1.getMostSignificantBits())
    assertEquals(0xA76500A0C91E6BF6L, uuid1.getLeastSignificantBits())
    assertEquals(2, uuid1.variant())
    assertEquals(1, uuid1.version())
    assertEquals(130742845922168750L, uuid1.timestamp())
    assertEquals(10085, uuid1.clockSequence())
    assertEquals(690568981494L, uuid1.node())

    val uuid2 = UUID.fromString("00000000-0000-1000-8000-000000000000")
    assertEquals(uuid2, new UUID(0x0000000000001000L, 0x8000000000000000L))
    assertEquals(0x0000000000001000L, uuid2.getMostSignificantBits())
    assertEquals(0x8000000000000000L, uuid2.getLeastSignificantBits())
    assertEquals(2, uuid2.variant())
    assertEquals(1, uuid2.version())
    assertEquals(0L, uuid2.timestamp())
    assertEquals(0, uuid2.clockSequence())
    assertEquals(0L, uuid2.node())

    assertThrows(classOf[Exception], UUID.fromString(null))
    assertThrows(classOf[Exception], UUID.fromString(""))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae_7dec-11d0-a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec_11d0-a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec-11d0_a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec-11d0-a765_00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("-7dec-11d0-a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae--11d0-a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec--a765-00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec-11d0--00a0c91e6bf6"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dec-11d0-a765-"))
    assertThrows(classOf[Exception], UUID.fromString("f81d4fae-7dec-11d0-a765"))
    assertThrows(
      classOf[Exception],
      UUID.fromString("f81d4fae-7dZc-11d0-a765-00a0c91e6bf6"))
  }
}
