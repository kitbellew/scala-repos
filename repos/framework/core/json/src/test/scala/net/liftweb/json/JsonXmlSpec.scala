/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package json

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

/**
  * System under specification for JSON XML.
  */
object JsonXmlSpec
    extends Specification with NodeGen with JValueGen with ScalaCheck {
  "JSON XML Specification".title

  import Xml._
  import scala.xml.Node

  "Valid XML can be converted to JSON and back (symmetric op)" in {
    val conversion = (xml: Node) => { toXml(toJson(xml)).head == xml }
    forAll(conversion)
  }

  "JSON can be converted to XML, and back to valid JSON (non symmetric op)" in {
    val conversion =
      (json: JValue) => { parse(compactRender(toJson(toXml(json)))); true }
    forAll(conversion)
  }

  implicit def arbXml: Arbitrary[Node] = Arbitrary(genXml)
  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genObject)
}
