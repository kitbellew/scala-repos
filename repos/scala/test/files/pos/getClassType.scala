trait IdlBase

class IdlConcrete extends IdlBase

class A {
  // In general, this method should not need an instance to reflect on it, so
  // take a Class[]
  def reflect(clazz: Class[_ <: IdlBase]) = {
    // Get a list of all its methods and build a hash keyed by method name
    // for statistics recording.
  }

  // But I also really have an IdlConcrete generated by Spring here...
  val idl = new IdlConcrete
  reflect(idl.getClass)
}
