package org.jetbrains.plugins.scala
package lang
package parameterInfo
package typeParameterInfo
package generated

class TypeParameterInfoExtendsTest extends TypeParameterInfoTestBase {
  //This class was generated by build script, please don't change this
  override def folderPath: String = super.folderPath + "Extends/"

  def testAllBounds() = doTest()

  def testJavaGeneric() = doTest()

  def testScalaGenericExtends() = doTest()

  def testScalaLowerBound() = doTest()

  def testScalaViewBound() = doTest()
}
