/*
 * Copyright (c) 2011-2015, ScalaFX Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the ScalaFX Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE SCALAFX PROJECT OR ITS CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalafx.scene.control

import javafx.beans.{property => jfxbp, value => jfxbv}
import javafx.scene.{control => jfxsc}
import javafx.{collections => jfxc, util => jfxu}

import scala.language.{implicitConversions, reflectiveCalls}
import scalafx.Includes._
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.beans.value.ObservableValue
import scalafx.delegate.SFXDelegate
import scalafx.util.StringConverter

/**
  * Wraps [[http://docs.oracle.com/javase/8/javafx/api/javafx/scene/control/cell/package-summary.html `javafx.scene.control.cell`]] package.
  */
package object cell {

  /**
    * Types that contains the method `converterProperty(): jfxbp.ObjectProperty[jfxu.StringConverter[T]]`
    *
    * @tparam J Original Java type used by converter.
    */
  type Convertable[J] = {
    def converterProperty(): jfxbp.ObjectProperty[jfxu.StringConverter[J]]
  }

  /**
    * Cells which delegate contains the method `converterProperty(): jfxbp.ObjectProperty[jfxu.StringConverter[T]]`
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    * @tparam C  Derivated type from JavaFX Cell
    * @tparam J Original Java type used by converter.
    */
  trait ConvertableCell[C <: jfxsc.Cell[T] with Convertable[J], T, J]
      extends SFXDelegate[C] {

    /**
      * The `StringConverter` property.
      */
    def converter: ObjectProperty[StringConverter[J]] = ObjectProperty(
      delegate.converterProperty().getValue)
    def converter_=(v: StringConverter[J]) {
      converter() = v
    }

  }

  /**
    * Types that contains the method `comboBoxEditableProperty(): jfxbp.BooleanProperty`
    */
  type ComboBoxEditable = {
    def comboBoxEditableProperty(): jfxbp.BooleanProperty
  }

  /**
    * Cells which delegate contains the method `comboBoxEditableProperty(): jfxbp.BooleanProperty`.
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    * @tparam C  Derivated type from JavaFX Cell
    */
  trait ComboBoxEditableCell[C <: jfxsc.Cell[T] with ComboBoxEditable, T]
      extends SFXDelegate[C] {

    /**
      * A property representing whether the `ComboBox`, when shown to the user, is editable or not.
      */
    def comboBoxEditable: BooleanProperty = delegate.comboBoxEditableProperty()
    def comboBoxEditable_=(v: Boolean) {
      comboBoxEditable() = v
    }

  }

  /**
    * Cells which delegate contains the method `updateItem(item: Any, empty: Boolean): Unit`.
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    * @tparam C  Derivated type from JavaFX Cell
    */
  trait UpdatableCell[C <: jfxsc.Cell[T], T] extends SFXDelegate[C] {
    /* IMPLEMENTATION NOTE:
     * Unlike to what happened with other traits of this package object, it was not possible create a type like
     * "type Updated[T] = { def updateItem(item: T, empty: Boolean): Unit }". In this case, the compiler shows this
     * error message: "Parameter type in structural refinement may not refer to an abstract type defined outside that
     * refinement". The only way for which was possible implement the trait was create an internal type in which
     * updateItem method from JavaFX class receive a item of type Any instead type T.
     */

    /**
      * Types that contains the method `updateItem(item: Any, empty: Boolean): Unit`
      */
    type Updated = {
      def updateItem(item: Any, empty: Boolean)
    }

    /**
      * Updates the item associated with this Cell.
      *
      * @param item  The new item for the cell
      * @param empty whether or not this cell represents data from the list. If it is empty, then it does not
      *              represent any domain data, but is a cell being used to render an "empty" row.
      */
    def updateItem(item: T, empty: Boolean) {
      delegate.asInstanceOf[Updated].updateItem(item, empty)
    }

  }

  /**
    * Types that contains the method `getItems(): ObservableList[T]`.
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    */
  type Itemable[T] = {
    def getItems(): jfxc.ObservableList[T]
  }

  /**
    * [[http://docs.oracle.com/javase/8/javafx/api/javafx/scene/control/Cell.html javafx.scene.control.Cell]]s that contains the method `getItems(): ObservableList[T]`.
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    * @tparam C  Derived type from JavaFX Cell
    */
  trait ItemableCell[C <: jfxsc.Cell[T] with Itemable[T], T]
      extends SFXDelegate[C] {

    /**
      * Returns the items to be displayed in the ChoiceBox when it is showing.
      */
    def items = delegate.getItems()
  }

  /**
    * Converts a Function of type `T => ObservableValue[Boolean, java.lang.Boolean]` to a JavaFX `Callback` of type
    * `[T, javafx.beans.value.ObservableValue[java.lang.Boolean]]`
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    */
  private[cell] implicit def selectedBooleanPropertyToGetSelectedProperty[T](
      selectedProperty: T => ObservableValue[Boolean, java.lang.Boolean])
      : jfxu.Callback[T, jfxbv.ObservableValue[java.lang.Boolean]] =
    new jfxu.Callback[T, jfxbv.ObservableValue[java.lang.Boolean]] {
      def call(x: T) = selectedProperty(x)
    }

  /**
    * Types that contains the property `selectedStateCallback`.
    *
    * @tparam J Original Java type used by converter.
    */
  type StateSelectable[J] = {
    def selectedStateCallbackProperty(): jfxbp.ObjectProperty[
      jfxu.Callback[J, jfxbv.ObservableValue[java.lang.Boolean]]]
  }

  /**
    * Cells which delegate contains the property `selectedStateCallback`.
    *
    * TODO: Convert selectedStateCallback getter return type from
    * `jfxbp.ObjectProperty[jfxu.Callback[J, javafx.beans.property.ObservableValue[java.lang.Boolean]]]` to
    * `ObjectProperty[J => ObservableValue[Boolean, java.lang.Boolean]]`.
    *
    * @tparam T The type of the elements contained within the inner element inside the Cell.
    * @tparam C  Derivated type from JavaFX Cell
    * @tparam J Original Java type used by converter.
    */
  trait StateSelectableCell[C <: jfxsc.Cell[T] with StateSelectable[J], T, J]
      extends SFXDelegate[C] {

    /**
      * Property representing the Callback that is bound to by the element inside the Cell shown on screen.
      */
    def selectedStateCallback = delegate.selectedStateCallbackProperty()
    def selectedStateCallback_=(
        v: J => ObservableValue[Boolean, java.lang.Boolean]) {
      selectedStateCallback() = v
    }

  }

}
