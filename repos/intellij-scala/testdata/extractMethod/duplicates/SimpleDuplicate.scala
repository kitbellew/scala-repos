object SimpleDuplicate {
  def foo(i: Int) {
    /*start*/
    println(i + 1)
    /*end*/
    println(2 + 1)
  }
}
/*
object SimpleDuplicate {
  def foo(i: Int) {

    testMethodName(i)

    testMethodName(2)
  }

  def testMethodName(i: Int): Unit = {
    println(i + 1)
  }
}
 */
