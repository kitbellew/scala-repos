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

package org.apache.spark.sql.execution.aggregate

import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.aggregate._
import org.apache.spark.sql.execution.SparkPlan

/**
  * Utility functions used by the query planner to convert our plan to new aggregation code path.
  */
object Utils {

  def planAggregateWithoutPartial(
      groupingExpressions: Seq[NamedExpression],
      aggregateExpressions: Seq[AggregateExpression],
      aggregateFunctionToAttribute: Map[(AggregateFunction, Boolean),
                                        Attribute],
      resultExpressions: Seq[NamedExpression],
      child: SparkPlan): Seq[SparkPlan] = {

    val completeAggregateExpressions =
      aggregateExpressions.map(_.copy(mode = Complete))
    val completeAggregateAttributes = completeAggregateExpressions.map {
      expr =>
        aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
    }

    SortBasedAggregate(
      requiredChildDistributionExpressions = Some(groupingExpressions),
      groupingExpressions = groupingExpressions,
      aggregateExpressions = completeAggregateExpressions,
      aggregateAttributes = completeAggregateAttributes,
      initialInputBufferOffset = 0,
      resultExpressions = resultExpressions,
      child = child
    ) :: Nil
  }

  private def createAggregate(
      requiredChildDistributionExpressions: Option[Seq[Expression]] = None,
      groupingExpressions: Seq[NamedExpression] = Nil,
      aggregateExpressions: Seq[AggregateExpression] = Nil,
      aggregateAttributes: Seq[Attribute] = Nil,
      initialInputBufferOffset: Int = 0,
      resultExpressions: Seq[NamedExpression] = Nil,
      child: SparkPlan): SparkPlan = {
    val usesTungstenAggregate = TungstenAggregate.supportsAggregate(
      aggregateExpressions.flatMap(_.aggregateFunction.aggBufferAttributes))
    if (usesTungstenAggregate) {
      TungstenAggregate(
        requiredChildDistributionExpressions =
          requiredChildDistributionExpressions,
        groupingExpressions = groupingExpressions,
        aggregateExpressions = aggregateExpressions,
        aggregateAttributes = aggregateAttributes,
        initialInputBufferOffset = initialInputBufferOffset,
        resultExpressions = resultExpressions,
        child = child
      )
    } else {
      SortBasedAggregate(
        requiredChildDistributionExpressions =
          requiredChildDistributionExpressions,
        groupingExpressions = groupingExpressions,
        aggregateExpressions = aggregateExpressions,
        aggregateAttributes = aggregateAttributes,
        initialInputBufferOffset = initialInputBufferOffset,
        resultExpressions = resultExpressions,
        child = child
      )
    }
  }

  def planAggregateWithoutDistinct(
      groupingExpressions: Seq[NamedExpression],
      aggregateExpressions: Seq[AggregateExpression],
      aggregateFunctionToAttribute: Map[(AggregateFunction, Boolean),
                                        Attribute],
      resultExpressions: Seq[NamedExpression],
      child: SparkPlan): Seq[SparkPlan] = {
    // Check if we can use TungstenAggregate.

    // 1. Create an Aggregate Operator for partial aggregations.

    val groupingAttributes = groupingExpressions.map(_.toAttribute)
    val partialAggregateExpressions =
      aggregateExpressions.map(_.copy(mode = Partial))
    val partialAggregateAttributes = partialAggregateExpressions.flatMap(
      _.aggregateFunction.aggBufferAttributes)
    val partialResultExpressions =
      groupingAttributes ++ partialAggregateExpressions.flatMap(
        _.aggregateFunction.inputAggBufferAttributes)

    val partialAggregate = createAggregate(
      requiredChildDistributionExpressions = None,
      groupingExpressions = groupingExpressions,
      aggregateExpressions = partialAggregateExpressions,
      aggregateAttributes = partialAggregateAttributes,
      initialInputBufferOffset = 0,
      resultExpressions = partialResultExpressions,
      child = child
    )

    // 2. Create an Aggregate Operator for final aggregations.
    val finalAggregateExpressions =
      aggregateExpressions.map(_.copy(mode = Final))
    // The attributes of the final aggregation buffer, which is presented as input to the result
    // projection:
    val finalAggregateAttributes = finalAggregateExpressions.map { expr =>
      aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
    }

    val finalAggregate = createAggregate(
      requiredChildDistributionExpressions = Some(groupingAttributes),
      groupingExpressions = groupingAttributes,
      aggregateExpressions = finalAggregateExpressions,
      aggregateAttributes = finalAggregateAttributes,
      initialInputBufferOffset = groupingExpressions.length,
      resultExpressions = resultExpressions,
      child = partialAggregate
    )

    finalAggregate :: Nil
  }

  def planAggregateWithOneDistinct(
      groupingExpressions: Seq[NamedExpression],
      functionsWithDistinct: Seq[AggregateExpression],
      functionsWithoutDistinct: Seq[AggregateExpression],
      aggregateFunctionToAttribute: Map[(AggregateFunction, Boolean),
                                        Attribute],
      resultExpressions: Seq[NamedExpression],
      child: SparkPlan): Seq[SparkPlan] = {

    // functionsWithDistinct is guaranteed to be non-empty. Even though it may contain more than one
    // DISTINCT aggregate function, all of those functions will have the same column expressions.
    // For example, it would be valid for functionsWithDistinct to be
    // [COUNT(DISTINCT foo), MAX(DISTINCT foo)], but [COUNT(DISTINCT bar), COUNT(DISTINCT foo)] is
    // disallowed because those two distinct aggregates have different column expressions.
    val distinctExpressions =
      functionsWithDistinct.head.aggregateFunction.children
    val namedDistinctExpressions = distinctExpressions.map {
      case ne: NamedExpression => ne
      case other => Alias(other, other.toString)()
    }
    val distinctAttributes = namedDistinctExpressions.map(_.toAttribute)
    val groupingAttributes = groupingExpressions.map(_.toAttribute)

    // 1. Create an Aggregate Operator for partial aggregations.
    val partialAggregate: SparkPlan = {
      val aggregateExpressions =
        functionsWithoutDistinct.map(_.copy(mode = Partial))
      val aggregateAttributes = aggregateExpressions.map { expr =>
        aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
      }
      // We will group by the original grouping expression, plus an additional expression for the
      // DISTINCT column. For example, for AVG(DISTINCT value) GROUP BY key, the grouping
      // expressions will be [key, value].
      createAggregate(
        groupingExpressions = groupingExpressions ++ namedDistinctExpressions,
        aggregateExpressions = aggregateExpressions,
        aggregateAttributes = aggregateAttributes,
        resultExpressions = groupingAttributes ++ distinctAttributes ++ aggregateExpressions
            .flatMap(_.aggregateFunction.inputAggBufferAttributes),
        child = child
      )
    }

    // 2. Create an Aggregate Operator for partial merge aggregations.
    val partialMergeAggregate: SparkPlan = {
      val aggregateExpressions =
        functionsWithoutDistinct.map(_.copy(mode = PartialMerge))
      val aggregateAttributes = aggregateExpressions.map { expr =>
        aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
      }
      createAggregate(
        requiredChildDistributionExpressions =
          Some(groupingAttributes ++ distinctAttributes),
        groupingExpressions = groupingAttributes ++ distinctAttributes,
        aggregateExpressions = aggregateExpressions,
        aggregateAttributes = aggregateAttributes,
        initialInputBufferOffset =
          (groupingAttributes ++ distinctAttributes).length,
        resultExpressions = groupingAttributes ++ distinctAttributes ++ aggregateExpressions
            .flatMap(_.aggregateFunction.inputAggBufferAttributes),
        child = partialAggregate
      )
    }

    // 3. Create an Aggregate operator for partial aggregation (for distinct)
    val distinctColumnAttributeLookup =
      distinctExpressions.zip(distinctAttributes).toMap
    val rewrittenDistinctFunctions = functionsWithDistinct.map {
      // Children of an AggregateFunction with DISTINCT keyword has already
      // been evaluated. At here, we need to replace original children
      // to AttributeReferences.
      case agg @ AggregateExpression(aggregateFunction, mode, true) =>
        aggregateFunction
          .transformDown(distinctColumnAttributeLookup)
          .asInstanceOf[AggregateFunction]
    }

    val partialDistinctAggregate: SparkPlan = {
      val mergeAggregateExpressions =
        functionsWithoutDistinct.map(_.copy(mode = PartialMerge))
      // The attributes of the final aggregation buffer, which is presented as input to the result
      // projection:
      val mergeAggregateAttributes = mergeAggregateExpressions.map { expr =>
        aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
      }
      val (distinctAggregateExpressions, distinctAggregateAttributes) =
        rewrittenDistinctFunctions.zipWithIndex.map {
          case (func, i) =>
            // We rewrite the aggregate function to a non-distinct aggregation because
            // its input will have distinct arguments.
            // We just keep the isDistinct setting to true, so when users look at the query plan,
            // they still can see distinct aggregations.
            val expr = AggregateExpression(func, Partial, isDistinct = true)
            // Use original AggregationFunction to lookup attributes, which is used to build
            // aggregateFunctionToAttribute
            val attr = aggregateFunctionToAttribute(
              functionsWithDistinct(i).aggregateFunction,
              true)
            (expr, attr)
        }.unzip

      val partialAggregateResult =
        groupingAttributes ++ mergeAggregateExpressions.flatMap(
          _.aggregateFunction.inputAggBufferAttributes) ++ distinctAggregateExpressions
          .flatMap(_.aggregateFunction.inputAggBufferAttributes)
      createAggregate(
        groupingExpressions = groupingAttributes,
        aggregateExpressions = mergeAggregateExpressions ++ distinctAggregateExpressions,
        aggregateAttributes = mergeAggregateAttributes ++ distinctAggregateAttributes,
        initialInputBufferOffset =
          (groupingAttributes ++ distinctAttributes).length,
        resultExpressions = partialAggregateResult,
        child = partialMergeAggregate
      )
    }

    // 4. Create an Aggregate Operator for the final aggregation.
    val finalAndCompleteAggregate: SparkPlan = {
      val finalAggregateExpressions =
        functionsWithoutDistinct.map(_.copy(mode = Final))
      // The attributes of the final aggregation buffer, which is presented as input to the result
      // projection:
      val finalAggregateAttributes = finalAggregateExpressions.map { expr =>
        aggregateFunctionToAttribute(expr.aggregateFunction, expr.isDistinct)
      }

      val (distinctAggregateExpressions, distinctAggregateAttributes) =
        rewrittenDistinctFunctions.zipWithIndex.map {
          case (func, i) =>
            // We rewrite the aggregate function to a non-distinct aggregation because
            // its input will have distinct arguments.
            // We just keep the isDistinct setting to true, so when users look at the query plan,
            // they still can see distinct aggregations.
            val expr = AggregateExpression(func, Final, isDistinct = true)
            // Use original AggregationFunction to lookup attributes, which is used to build
            // aggregateFunctionToAttribute
            val attr = aggregateFunctionToAttribute(
              functionsWithDistinct(i).aggregateFunction,
              true)
            (expr, attr)
        }.unzip

      createAggregate(
        requiredChildDistributionExpressions = Some(groupingAttributes),
        groupingExpressions = groupingAttributes,
        aggregateExpressions = finalAggregateExpressions ++ distinctAggregateExpressions,
        aggregateAttributes = finalAggregateAttributes ++ distinctAggregateAttributes,
        initialInputBufferOffset = groupingAttributes.length,
        resultExpressions = resultExpressions,
        child = partialDistinctAggregate
      )
    }

    finalAndCompleteAggregate :: Nil
  }
}
