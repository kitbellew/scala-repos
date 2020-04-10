package kafka.common

/**
  * Licensed to the Apache Software Foundation (ASF) under one or more
  * contributor license agreements.  See the NOTICE file distributed with
  * this work for additional information regarding copyright ownership.
  * The ASF licenses this file to You under the Apache License, Version 2.0
  * (the "License"); you may not use this file except in compliance with
  * the License.  You may obtain a copy of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

/**
  * Convenience case class since (clientId, brokerInfo) pairs are used to create
  * SyncProducer Request Stats and SimpleConsumer Request and Response Stats.
  */

trait ClientIdBroker {}

case class ClientIdAndBroker(
    clientId: String,
    brokerHost: String,
    brokerPort: Int)
    extends ClientIdBroker {
  override def toString = "%s-%s-%d".format(clientId, brokerHost, brokerPort)
}

case class ClientIdAllBrokers(clientId: String) extends ClientIdBroker {
  override def toString = "%s-%s".format(clientId, "AllBrokers")
}
