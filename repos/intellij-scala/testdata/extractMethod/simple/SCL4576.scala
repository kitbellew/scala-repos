object SCL4576 {

  val zz = 1

  def foo() {
    val x = 1
    /*start*/
    //some text
    print(x + zz)
    /*end*/
  }
}
/*
object SCL4576 {
  val zz = 1
  def foo() {
    val x = 1
    testMethodName(x)
  }
  def testMethodName(x: Int): Unit = {
    //some text
    print(x + zz)
  }
}
 */
