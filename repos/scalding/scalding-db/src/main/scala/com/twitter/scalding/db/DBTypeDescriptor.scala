/*
Copyright 2015 Twitter, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package com.twitter.scalding.db
import com.twitter.scalding._
import cascading.tuple.Fields

trait DBTypeDescriptor[T] {
  def columnDefn: ColumnDefinitionProvider[T]
  def converter: TupleConverter[T]
  def setter: TupleSetter[T]
  def fields: Fields
  def jdbcSetter: JdbcStatementSetter[T]
}
