class A {
  def foo: Class[_ <: A] = getClass
}
