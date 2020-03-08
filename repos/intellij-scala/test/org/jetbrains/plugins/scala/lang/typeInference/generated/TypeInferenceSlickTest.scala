package org.jetbrains.plugins.scala.lang.typeInference.generated

import org.jetbrains.plugins.scala.lang.typeInference.TypeInferenceTestBase

/**
  * @author Alefas
  * @since  11/12/15
  */
class TypeInferenceSlickTest extends TypeInferenceTestBase {
  //This class was generated by build script, please don't change this
  override def folderPath: String = super.folderPath + "slick/"

  protected override def additionalLibraries = Array("slick")

  def testSCL9261(): Unit = {
    doTest()
  }
}
