object Test {
  class C
  class A {
    val foo: C = new C
    //def foo(x: Int, y: Int): String = ""
  }

  val a = new A
  implicit def c2fun = (c: C) => (x: String) => ""
  a.foo(""). /* resolved: true */ substring(1)
}
