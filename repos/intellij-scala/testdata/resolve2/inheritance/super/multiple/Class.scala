class C1 {
  def a {}
  case class A
}

class C2 extends C1 {
  def b {}
  case class B
}

class C3 extends C2 {
  def c {}
  case class C

  println( /* line: 2 */ a)
  println( /* line: 7 */ b)
  println( /* line: 12 */ c)

  println(super. /* line: 2 */ a)
  println(super. /* line: 7 */ b)
  println(super. /* resolved: false */ c)

  println( /* */ A.getClass)
  println(classOf[ /* line: 3 */ A])

  println( /* */ B.getClass)
  println(classOf[ /* line: 8 */ B])

  println( /* */ C.getClass)
  println(classOf[ /* line: 13 */ C])

  println(super. /* */ A.getClass)
  println(classOf[super. /* line: 3 */ A])

  println(super. /* */ B.getClass)
  println(classOf[super. /* line: 8 */ B])

  println(super. /* resolved: false */ C.getClass)
  println(classOf[super. /* resolved: false */ C])
}
