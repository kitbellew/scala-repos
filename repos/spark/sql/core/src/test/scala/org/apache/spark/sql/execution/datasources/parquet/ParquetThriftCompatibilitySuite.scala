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

package org.apache.spark.sql.execution.datasources.parquet

import org.apache.spark.sql.Row
import org.apache.spark.sql.test.SharedSQLContext

class ParquetThriftCompatibilitySuite
    extends ParquetCompatibilityTest
    with SharedSQLContext {
  import ParquetCompatibilityTest._

  private val parquetFilePath =
    Thread
      .currentThread()
      .getContextClassLoader
      .getResource("parquet-thrift-compat.snappy.parquet")

  test("Read Parquet file generated by parquet-thrift") {
    logInfo(s"""Schema of the Parquet file written by parquet-thrift:
         |${readParquetSchema(parquetFilePath.toString)}
       """.stripMargin)

    checkAnswer(
      sqlContext.read.parquet(parquetFilePath.toString),
      (0 until 10).map { i =>
        val suits = Array("SPADES", "HEARTS", "DIAMONDS", "CLUBS")

        val nonNullablePrimitiveValues = Seq(
          i % 2 == 0,
          i.toByte,
          (i + 1).toShort,
          i + 2,
          i.toLong * 10,
          i.toDouble + 0.2d,
          // Thrift `BINARY` values are actually unencoded `STRING` values, and thus are always
          // treated as `BINARY (UTF8)` in parquet-thrift, since parquet-thrift always assume
          // Thrift `STRING`s are encoded using UTF-8.
          s"val_$i",
          s"val_$i",
          // Thrift ENUM values are converted to Parquet binaries containing UTF-8 strings
          suits(i % 4)
        )

        val nullablePrimitiveValues =
          if (i % 3 == 0)
            Seq.fill(nonNullablePrimitiveValues.length)(null)
          else
            nonNullablePrimitiveValues

        val complexValues = Seq(
          Seq.tabulate(3)(n => s"arr_${i + n}"),
          // Thrift `SET`s are converted to Parquet `LIST`s
          Seq(i),
          Seq.tabulate(3)(n => (i + n: Integer) -> s"val_${i + n}").toMap,
          Seq
            .tabulate(3) { n =>
              (i + n) -> Seq.tabulate(3) { m =>
                Row(Seq.tabulate(3)(j => i + j + m), s"val_${i + m}")
              }
            }
            .toMap
        )

        Row(
          nonNullablePrimitiveValues ++ nullablePrimitiveValues ++ complexValues: _*)
      }
    )
  }

  test("SPARK-10136 list of primitive list") {
    withTempPath { dir =>
      val path = dir.getCanonicalPath

      // This Parquet schema is translated from the following Thrift schema:
      //
      //   struct ListOfPrimitiveList {
      //     1: list<list<i32>> f;
      //   }
      val schema =
        s"""message ListOfPrimitiveList {
           |  required group f (LIST) {
           |    repeated group f_tuple (LIST) {
           |      repeated int32 f_tuple_tuple;
           |    }
           |  }
           |}
         """.stripMargin

      writeDirect(
        path,
        schema, { rc =>
          rc.message {
            rc.field("f", 0) {
              rc.group {
                rc.field("f_tuple", 0) {
                  rc.group {
                    rc.field("f_tuple_tuple", 0) {
                      rc.addInteger(0)
                      rc.addInteger(1)
                    }
                  }

                  rc.group {
                    rc.field("f_tuple_tuple", 0) {
                      rc.addInteger(2)
                      rc.addInteger(3)
                    }
                  }
                }
              }
            }
          }
        }, { rc =>
          rc.message {
            rc.field("f", 0) {
              rc.group {
                rc.field("f_tuple", 0) {
                  rc.group {
                    rc.field("f_tuple_tuple", 0) {
                      rc.addInteger(4)
                      rc.addInteger(5)
                    }
                  }

                  rc.group {
                    rc.field("f_tuple_tuple", 0) {
                      rc.addInteger(6)
                      rc.addInteger(7)
                    }
                  }
                }
              }
            }
          }
        }
      )

      logParquetSchema(path)

      checkAnswer(
        sqlContext.read.parquet(path),
        Seq(Row(Seq(Seq(0, 1), Seq(2, 3))), Row(Seq(Seq(4, 5), Seq(6, 7)))))
    }
  }
}
