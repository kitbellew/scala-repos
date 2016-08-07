/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.ml.api.r

import org.apache.spark.ml.{Pipeline, PipelineModel}
import org.apache.spark.ml.attribute._
import org.apache.spark.ml.classification.{
  LogisticRegression, LogisticRegressionModel
}
import org.apache.spark.ml.clustering.{KMeans, KMeansModel}
import org.apache.spark.ml.feature.{RFormula, VectorAssembler}
import org.apache.spark.ml.regression.{LinearRegression, LinearRegressionModel}
import org.apache.spark.sql.DataFrame

private[r] object SparkRWrappers {
  def fitRModelFormula(value: String,
                       df: DataFrame,
                       family: String,
                       lambda: Double,
                       alpha: Double,
                       standardize: Boolean,
                       solver: String): PipelineModel = {
    val formula = new RFormula().setFormula(value)
    val estimator = family match {
      case "gaussian" =>
        new LinearRegression()
          .setRegParam(lambda)
          .setElasticNetParam(alpha)
          .setFitIntercept(formula.hasIntercept)
          .setStandardization(standardize)
          .setSolver(solver)
      case "binomial" =>
        new LogisticRegression()
          .setRegParam(lambda)
          .setElasticNetParam(alpha)
          .setFitIntercept(formula.hasIntercept)
          .setStandardization(standardize)
    }
    val pipeline = new Pipeline().setStages(Array(formula, estimator))
    pipeline.fit(df)
  }

  def fitKMeans(df: DataFrame,
                initMode: String,
                maxIter: Double,
                k: Double,
                columns: Array[String]): PipelineModel = {
    val assembler = new VectorAssembler().setInputCols(columns)
    val kMeans = new KMeans()
      .setInitMode(initMode)
      .setMaxIter(maxIter.toInt)
      .setK(k.toInt)
      .setFeaturesCol(assembler.getOutputCol)
    val pipeline = new Pipeline().setStages(Array(assembler, kMeans))
    pipeline.fit(df)
  }

  def getModelCoefficients(model: PipelineModel): Array[Double] = {
    model.stages.last match {
      case m: LinearRegressionModel => {
        val coefficientStandardErrorsR =
          Array(m.summary.coefficientStandardErrors.last) ++ m.summary.coefficientStandardErrors
            .dropRight(1)
        val tValuesR =
          Array(m.summary.tValues.last) ++ m.summary.tValues.dropRight(1)
        val pValuesR =
          Array(m.summary.pValues.last) ++ m.summary.pValues.dropRight(1)
        if (m.getFitIntercept) {
          Array(m.intercept) ++ m.coefficients.toArray ++ coefficientStandardErrorsR ++ tValuesR ++ pValuesR
        } else {
          m.coefficients.toArray ++ coefficientStandardErrorsR ++ tValuesR ++ pValuesR
        }
      }
      case m: LogisticRegressionModel => {
        if (m.getFitIntercept) {
          Array(m.intercept) ++ m.coefficients.toArray
        } else {
          m.coefficients.toArray
        }
      }
      case m: KMeansModel =>
        m.clusterCenters.flatMap(_.toArray)
    }
  }

  def getModelDevianceResiduals(model: PipelineModel): Array[Double] = {
    model.stages.last match {
      case m: LinearRegressionModel =>
        m.summary.devianceResiduals
      case m: LogisticRegressionModel =>
        throw new UnsupportedOperationException(
            "No deviance residuals available for LogisticRegressionModel")
    }
  }

  def getKMeansModelSize(model: PipelineModel): Array[Int] = {
    model.stages.last match {
      case m: KMeansModel => Array(m.getK) ++ m.summary.size
      case other =>
        throw new UnsupportedOperationException(
            s"KMeansModel required but ${other.getClass.getSimpleName} found.")
    }
  }

  def getKMeansCluster(model: PipelineModel, method: String): DataFrame = {
    model.stages.last match {
      case m: KMeansModel =>
        if (method == "centers") {
          // Drop the assembled vector for easy-print to R side.
          m.summary.predictions.drop(m.summary.featuresCol)
        } else if (method == "classes") {
          m.summary.cluster
        } else {
          throw new UnsupportedOperationException(
              s"Method (centers or classes) required but $method found.")
        }
      case other =>
        throw new UnsupportedOperationException(
            s"KMeansModel required but ${other.getClass.getSimpleName} found.")
    }
  }

  def getModelFeatures(model: PipelineModel): Array[String] = {
    model.stages.last match {
      case m: LinearRegressionModel =>
        val attrs = AttributeGroup.fromStructField(
            m.summary.predictions.schema(m.summary.featuresCol))
        if (m.getFitIntercept) {
          Array("(Intercept)") ++ attrs.attributes.get.map(_.name.get)
        } else {
          attrs.attributes.get.map(_.name.get)
        }
      case m: LogisticRegressionModel =>
        val attrs = AttributeGroup.fromStructField(
            m.summary.predictions.schema(m.summary.featuresCol))
        if (m.getFitIntercept) {
          Array("(Intercept)") ++ attrs.attributes.get.map(_.name.get)
        } else {
          attrs.attributes.get.map(_.name.get)
        }
      case m: KMeansModel =>
        val attrs = AttributeGroup.fromStructField(
            m.summary.predictions.schema(m.summary.featuresCol))
        attrs.attributes.get.map(_.name.get)
    }
  }

  def getModelName(model: PipelineModel): String = {
    model.stages.last match {
      case m: LinearRegressionModel =>
        "LinearRegressionModel"
      case m: LogisticRegressionModel =>
        "LogisticRegressionModel"
      case m: KMeansModel =>
        "KMeansModel"
    }
  }
}
