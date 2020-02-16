package com.twitter.scalding.examples

import com.twitter.scalding._

/**
  * weighted page rank for the given graph, start from the given pagerank,
  * perform one iteartion, test for convergence, if not yet, clone itself
  * and start the next page rank job with updated pagerank as input.
  *
  * This class is very similar to the PageRank class, main differences are:
  * 1. supported weighted pagerank
  * 2. the reset pagerank is pregenerated, possibly through a previous job
  * 3. dead pagerank is evenly distributed
  *
  * Options:
  * --pwd: working directory, will read/generate the following files there
  *        numnodes: total number of nodes
  *        nodes: nodes file <'src_id, 'dst_ids, 'weights, 'mass_prior>
  *        pagerank: the page rank file eg pagerank_0, pagerank_1 etc
  *        totaldiff: the current max pagerank delta
  * Optional arguments:
  * --weighted: do weighted pagerank, default false
  * --curiteration: what is the current iteration, default 0
  * --maxiterations: how many iterations to run.  Default is 20
  * --jumpprob: probability of a random jump, default is 0.1
  * --threshold: total difference before finishing early, default 0.001
  */
class WeightedPageRank(args: Args) extends Job(args) {
  val ROW_TYPE_1 = 1
  val ROW_TYPE_2 = 2

  val PWD = args("pwd")
  val ALPHA = args.getOrElse("jumpprob", "0.1").toDouble
  val WEIGHTED = args.getOrElse("weighted", "false").toBoolean
  val THRESHOLD = args.getOrElse("threshold", "0.001").toDouble
  val MAXITERATIONS = args.getOrElse("maxiterations", "20").toInt
  val CURITERATION = args.getOrElse("curiteration", "0").toInt

  // 'size
  val numNodes = getNumNodes(PWD + "/numnodes")

  // 'src_id, 'dst_ids, 'weights, 'mass_prior
  val nodes = getNodes(PWD + "/nodes")

  // 'src_id_input, 'mass_input
  val inputPagerank = getInputPagerank(PWD + "/pagerank_" + CURITERATION)

  // one iteration of pagerank
  val outputPagerank = doPageRank(nodes, inputPagerank)
  val outputFileName = PWD + "/pagerank_" + (CURITERATION + 1)
  outputPagerank
    .project('src_id, 'mass_n)
    .write(Tsv(outputFileName))

  // detect convergence
  val totalDiff = outputPagerank
    .mapTo(('mass_input, 'mass_n) -> 'mass_diff) { args: (Double, Double) =>
      scala.math.abs(args._1 - args._2)
    }
    .groupAll { _.sum[Double]('mass_diff) }
    .write(TypedTsv[Double](PWD + "/totaldiff"))

  /**
    * test convergence, if not yet, kick off the next iteration
    */
  override def next = {
    // the max diff generated above
    val totalDiff = TypedTsv[Double](PWD + "/totaldiff").toIterator.next

    if (CURITERATION < MAXITERATIONS - 1 && totalDiff > THRESHOLD) {
      val newArgs = args + ("curiteration", Some((CURITERATION + 1).toString))
      Some(clone(newArgs))
    } else {
      None
    }
  }

  def getInputPagerank(fileName: String) = {
    Tsv(fileName).read
      .mapTo((0, 1) -> ('src_id_input, 'mass_input)) { input: (Int, Double) =>
        input
      }
  }

  /**
    * read the pregenerated nodes file <'src_id, 'dst_ids, 'weights, 'mass_prior>
    */
  def getNodes(fileName: String) = {
    mode match {
      case Hdfs(_, conf) => {
        SequenceFile(fileName).read
          .mapTo((0, 1, 2, 3) -> ('src_id, 'dst_ids, 'weights, 'mass_prior)) {
            input: (Int, Array[Int], Array[Float], Double) => input
          }
      }
      case _ => {
        Tsv(fileName).read
          .mapTo((0, 1, 2, 3) -> ('src_id, 'dst_ids, 'weights, 'mass_prior)) {
            input: (Int, String, String, Double) =>
              {
                (
                  input._1,
                  // convert string to int array
                  if (input._2 != null && input._2.length > 0) {
                    input._2.split(",").map { _.toInt }
                  } else {
                    Array[Int]()
                  },
                  // convert string to float array
                  if (input._3 != null && input._3.length > 0) {
                    input._3.split(",").map { _.toFloat }
                  } else {
                    Array[Float]()
                  },
                  input._4)
              }
          }
      }
    }
  }

  /**
    * the total number of nodes, single line file
    */
  def getNumNodes(fileName: String) = {
    Tsv(fileName).read
      .mapTo(0 -> 'size) { input: Int => input }
  }

  /**
    * one iteration of pagerank
    * inputPagerank: <'src_id_input, 'mass_input>
    * return <'src_id, 'mass_n, 'mass_input>
    *
    * Here is a highlevel view of the unweighted algorithm:
    * let
    * N: number of nodes
    * inputPagerank(N_i): prob of walking to node i,
    * d(N_j): N_j's out degree
    * then
    * pagerankNext(N_i) = (\sum_{j points to i} inputPagerank(N_j) / d_j)
    * deadPagerank = (1 - \sum_{i} pagerankNext(N_i)) / N
    * randomPagerank(N_i) = userMass(N_i) * ALPHA + deadPagerank * (1-ALPHA)
    * pagerankOutput(N_i) = randomPagerank(N_i) + pagerankNext(N_i) * (1-ALPHA)
    *
    * For weighted algorithm:
    * let
    * w(N_j, N_i): weight from N_j to N_i
    * tw(N_j): N_j's total out weights
    * then
    * pagerankNext(N_i) = (\sum_{j points to i} inputPagerank(N_j) * w(N_j, N_i) / tw(N_j))
    *
    */
  def doPageRank(nodeRows: RichPipe, inputPagerank: RichPipe): RichPipe = {
    // 'src_id, 'dst_ids, 'weights, 'mass_prior, 'mass_input
    val nodeJoined = nodeRows
      .joinWithSmaller('src_id -> 'src_id_input, inputPagerank)
      .discard('src_id_input)

    // 'src_id, 'mass_n
    val pagerankNext = nodeJoined
      .flatMapTo(('dst_ids, 'weights, 'mass_input) -> ('src_id, 'mass_n)) {
        args: (Array[Int], Array[Float], Double) =>
          {
            if (args._1.length > 0) {
              if (WEIGHTED) {
                // weighted distribution
                val total: Double = args._2.sum
                (args._1 zip args._2).map { idWeight: (Int, Float) =>
                  (idWeight._1, args._3 * idWeight._2 / total)
                }
              } else {
                // equal distribution
                val dist: Double = args._3 / args._1.length
                args._1.map { id: Int => (id, dist) }
              }
            } else {
              //Here is a node that points to no other nodes (dangling)
              Nil
            }
          }
      }
      .groupBy('src_id) {
        _.sum[Double]('mass_n)
      }

    // 'sum_mass
    val sumPagerankNext = pagerankNext.groupAll {
      _.sum[Double]('mass_n -> 'sum_mass)
    }

    // 'deadMass
    // single row jobs
    // the dead page rank equally distributed to every node
    val deadPagerank = sumPagerankNext
      .crossWithTiny(numNodes)
      .map(('sum_mass, 'size) -> 'deadMass) { input: (Double, Int) =>
        (1.0 - input._1) / input._2
      }
      .discard('size, 'sum_mass)

    // 'src_id_r, 'mass_n_r
    // random jump probability plus dead page rank
    val randomPagerank = nodeJoined
      .crossWithTiny(deadPagerank)
      .mapTo(
        ('src_id, 'mass_prior, 'deadMass, 'mass_input) -> ('src_id, 'mass_n, 'mass_input)) {
        ranks: (Int, Double, Double, Double) =>
          (ranks._1, ranks._2 * ALPHA + ranks._3 * (1 - ALPHA), ranks._4)
      }

    // 'src_id, 'mass_n
    // scale next page rank to 1-ALPHA
    val pagerankNextScaled = pagerankNext
      .map('mass_n -> ('mass_n, 'mass_input)) { m: Double =>
        ((1 - ALPHA) * m, 0.0)
      }

    // 'src_id, 'mass_n, 'mass_input
    // random probability + next probability
    (randomPagerank ++ pagerankNextScaled)
      .groupBy('src_id) {
        _.sum[Double]('mass_input) // keep the input pagerank
          .sum[Double]('mass_n) // take the sum
      }
  }
}
