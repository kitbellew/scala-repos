/** Copyright 2015 TappingStone, Inc.
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

package io.prediction.controller

import org.apache.spark.SparkContext

/** Mix in and implement this trait if your model cannot be persisted by
  * PredictionIO automatically. A companion object extending
  * IPersistentModelLoader is required for PredictionIO to load the persisted
  * model automatically during deployment.
  *
  * Notice that models generated by [[PAlgorithm]] cannot be persisted
  * automatically by nature and must implement these traits if model persistence
  * is desired.
  *
  * {{{
  * class MyModel extends PersistentModel[MyParams] {
  *   def save(id: String, params: MyParams, sc: SparkContext): Boolean = {
  *     ...
  *   }
  * }
  *
  * object MyModel extends PersistentModelLoader[MyParams, MyModel] {
  *   def apply(id: String, params: MyParams, sc: Option[SparkContext]): MyModel = {
  *     ...
  *   }
  * }
  * }}}
  *
  * In Java, all you need to do is to implement this interface, and add a static
  * method with 3 arguments of type String, [[Params]], and SparkContext.
  *
  * {{{
  * public class MyModel implements PersistentModel<MyParams>, Serializable {
  *   ...
  *   public boolean save(String id, MyParams params, SparkContext sc) {
  *     ...
  *   }
  *
  *   public static MyModel load(String id, Params params, SparkContext sc) {
  *     ...
  *   }
  *   ...
  * }
  * }}}
  *
  * @tparam AP Algorithm parameters class.
  * @see [[PersistentModelLoader]]
  * @group Algorithm
  */
trait PersistentModel[AP <: Params] {
  /** Save the model to some persistent storage.
    *
    * This method should return true if the model has been saved successfully so
    * that PredictionIO knows that it can be restored later during deployment.
    * This method should return false if the model cannot be saved (or should
    * not be saved due to configuration) so that PredictionIO will re-train the
    * model during deployment. All arguments of this method are provided by
    * automatically by PredictionIO.
    *
    * @param id ID of the run that trained this model.
    * @param params Algorithm parameters that were used to train this model.
    * @param sc An Apache Spark context.
    */
  def save(id: String, params: AP, sc: SparkContext): Boolean
}

/** Implement an object that extends this trait for PredictionIO to support
  * loading a persisted model during serving deployment.
  *
  * @tparam AP Algorithm parameters class.
  * @tparam M Model class.
  * @see [[PersistentModel]]
  * @group Algorithm
  */
trait PersistentModelLoader[AP <: Params, M] {
  /** Implement this method to restore a persisted model that extends the
    * [[PersistentModel]] trait. All arguments of this method are provided
    * automatically by PredictionIO.
    *
    * @param id ID of the run that trained this model.
    * @param params Algorithm parameters that were used to train this model.
    * @param sc An optional Apache Spark context. This will be injected if the
    *           model was generated by a [[PAlgorithm]].
    */
  def apply(id: String, params: AP, sc: Option[SparkContext]): M
}

/** DEPRECATED. Use [[PersistentModel]] instead.
  *
  * @group Algorithm */
@deprecated("Use PersistentModel instead.", "0.9.2")
trait IPersistentModel[AP <: Params] extends PersistentModel[AP]

/** DEPRECATED. Use [[PersistentModelLoader]] instead.
  *
  * @group Algorithm */
@deprecated("Use PersistentModelLoader instead.", "0.9.2")
trait IPersistentModelLoader[AP <: Params, M] extends PersistentModelLoader[AP, M]
