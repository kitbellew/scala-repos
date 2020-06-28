/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import doclet._

/** The default doclet used by the scaladoc command line tool
  * when no user-provided doclet is provided.
  */
class Doclet extends Generator with Universer {

  def generateImpl() {
    new html.HtmlFactory(universe, new ScalaDocReporter(universe.settings))
      .generate()
  }

}
