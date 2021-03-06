package org.jetbrains.plugins.scala.testingSupport.scalatest.scala2_10.scalatest2_2_1

import org.jetbrains.plugins.scala.testingSupport.scalatest.fileStructureView._

/**
  * @author Roman.Shein
  * @since 20.04.2015.
  */
class Scalatest2_10_2_2_1_StructureViewTest
    extends Scalatest2_10_2_2_1_Base with FeatureSpecFileStructureViewTest
    with FlatSpecFileStructureViewTest with FreeSpecFileStructureViewTest
    with FunSuiteFileStructureViewTest with PropSpecFileStructureViewTest
    with WordSpecFileStructureViewTest with FunSpecFileStructureViewTest {}
