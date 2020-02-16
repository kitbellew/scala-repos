/*
 *
 *  Copyright 2015 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.optimize

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.linalg._
import breeze.numerics._
import breeze.optimize.proximal.NonlinearMinimizer.Projection
import breeze.optimize.proximal.{ProjectL1, QuadraticMinimizer}
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

@RunWith(classOf[JUnitRunner])
class SpectralProjectedGradientTest
    extends PropSpec
    with PropertyChecks
    with OptimizeTestBaseTrait
    with VectorMatchers
    with Matchers {

  property("optimize a simple multivariate gaussian") {
    val optimizer =
      new SpectralProjectedGradient[DenseVector[Double]](tolerance = 1.0e-9)
    forAll { init: DenseVector[Double] =>
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (sum((x - 3.0) :^ 2.0), (x * 2.0) - 6.0)
        }
      }

      val result = optimizer.minimize(f, init)
      result should beSimilarTo(
        DenseVector.fill(result.size)(3.0),
        allowedDeviation = 1e-5)
    }
  }

  property("optimize a simple multivariate gaussian with projection") {
    val optimizer = new SpectralProjectedGradient[DenseVector[Double]](
      tolerance = 1.0e-5,
      projection = _.map(scala.math.min(_, 2.0)))

    forAll { init: DenseVector[Double] =>
      init := clip(init, Double.NegativeInfinity, 2.0)
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (sum((x - 3.0) :^ 4.0), (x - 3.0) :^ 3.0 :* 4.0)
        }
      }

      val result = optimizer.minimize(f, init)
      result should beSimilarTo(
        DenseVector.fill(result.size)(2.0),
        allowedDeviation = 1e-10)
    }
  }

  property("simple linear solve without projection") {
    val n = 5
    val H = new DenseMatrix(
      n,
      n,
      Array(1.8984250861699135, 0.5955576666769438, -1.484430453342902,
        -1.0434994471390804, -3.675310432634351, 0.5955576666769438,
        0.9090751938470876, -2.146380947361661, -0.13037609428980368,
        -0.40639564652095117, -1.484430453342902, -2.146380947361661,
        10.262733520770384, -6.097698907163584, 2.29625304115155,
        -1.0434994471390804, -0.13037609428980368, -6.097698907163584,
        27.775920405610677, -5.574220233644466, -3.675310432634351,
        -0.40639564652095117, 2.29625304115155, -5.574220233644466,
        12.21329172136971))
    val f = DenseVector(
      -1.2320199653150048,
      -0.14220655875869606,
      0.38477404739124765,
      -0.3480575854151014,
      -0.4729810900829228)

    val cost = QuadraticMinimizer.Cost(H, f :* (-1.0))
    val init = DenseVector.zeros[Double](n)

    init := 0.0
    val x = H \ f

    init := 0.0
    val spgResult =
      new SpectralProjectedGradient[DenseVector[Double]]().minimize(cost, init)
    assert(norm(x - spgResult, inf) < 1e-4, s"$x $spgResult")
  }

  property("simple linear solve with L1 projection") {
    val Hl1 = new DenseMatrix(
      25,
      25,
      Array(253.535098, 236.477785, 234.421906, 223.374867, 241.007512,
        204.695511, 226.465507, 223.351032, 249.179386, 221.411909, 238.679352,
        203.579010, 217.564498, 243.852681, 266.607649, 213.994496, 241.620759,
        223.602907, 220.038678, 264.704959, 240.341716, 223.672378, 244.323303,
        223.216217, 226.074990, 236.477785, 278.862035, 245.756639, 237.489890,
        252.783139, 214.077652, 241.816953, 238.790633, 260.536460, 228.121417,
        255.103936, 216.608405, 237.150426, 258.933231, 281.958112, 228.971242,
        252.508513, 234.242638, 240.308477, 285.416390, 254.792243, 240.176223,
        259.048267, 235.566855, 236.277617, 234.421906, 245.756639, 269.162882,
        231.416867, 251.321527, 208.134322, 236.567647, 236.558029, 255.805108,
        226.535825, 251.514713, 212.770208, 228.565362, 261.748652, 273.946966,
        227.411615, 252.767900, 232.823977, 233.084574, 278.315614, 250.872786,
        235.227909, 255.104263, 238.931093, 235.402356, 223.374867, 237.489890,
        231.416867, 254.771963, 241.703229, 209.028084, 231.517998, 228.768510,
        250.805315, 216.548935, 245.473869, 207.687875, 222.036114, 250.906955,
        263.018181, 216.128966, 244.445283, 227.436840, 231.369510, 270.721492,
        242.475130, 226.471530, 248.130112, 225.826557, 228.266719, 241.007512,
        252.783139, 251.321527, 241.703229, 285.702320, 219.051868, 249.442308,
        240.400187, 264.970407, 232.503138, 258.819837, 220.160683, 235.621356,
        267.743972, 285.795029, 229.667231, 260.870105, 240.751687, 247.183922,
        289.044453, 260.715749, 244.210258, 267.159502, 242.992822, 244.070245,
        204.695511, 214.077652, 208.134322, 209.028084, 219.051868, 210.164224,
        208.151908, 201.539036, 226.373834, 192.056565, 219.950686, 191.459568,
        195.982460, 226.739575, 240.677519, 196.116652, 217.352348, 203.533069,
        204.581690, 243.603643, 217.785986, 204.205559, 223.747953, 203.586842,
        200.165867, 226.465507, 241.816953, 236.567647, 231.517998, 249.442308,
        208.151908, 264.007925, 227.080718, 253.174653, 220.322823, 248.619983,
        210.100242, 223.279198, 254.807401, 269.896959, 222.927882, 247.017507,
        230.484479, 233.358639, 274.935489, 249.237737, 235.229584, 253.029955,
        228.601700, 230.512885, 223.351032, 238.790633, 236.558029, 228.768510,
        240.400187, 201.539036, 227.080718, 258.773479, 249.471480, 215.664539,
        243.078577, 202.337063, 221.020998, 249.979759, 263.356244, 213.470569,
        246.182278, 225.727773, 229.873732, 266.295057, 242.954024, 225.510760,
        249.370268, 227.813265, 232.141964, 249.179386, 260.536460, 255.805108,
        250.805315, 264.970407, 226.373834, 253.174653, 249.471480, 302.360150,
        237.902729, 265.769812, 224.947876, 243.088105, 273.690377, 291.076027,
        241.089661, 267.772651, 248.459822, 249.662698, 295.935799, 267.460908,
        255.668926, 275.902272, 248.495606, 246.827505, 221.411909, 228.121417,
        226.535825, 216.548935, 232.503138, 192.056565, 220.322823, 215.664539,
        237.902729, 245.154567, 234.956316, 199.557862, 214.774631, 240.339217,
        255.161923, 209.328714, 232.277540, 216.298768, 220.296241, 253.817633,
        237.638235, 220.785141, 239.098500, 220.583355, 218.962732, 238.679352,
        255.103936, 251.514713, 245.473869, 258.819837, 219.950686, 248.619983,
        243.078577, 265.769812, 234.956316, 288.133073, 225.087852, 239.810430,
        268.406605, 283.289840, 233.858455, 258.306589, 240.263617, 246.844456,
        290.492875, 267.212598, 243.218596, 265.681905, 244.615890, 242.543363,
        203.579010, 216.608405, 212.770208, 207.687875, 220.160683, 191.459568,
        210.100242, 202.337063, 224.947876, 199.557862, 225.087852, 217.501685,
        197.897572, 229.825316, 242.175607, 201.123644, 219.820165, 202.894307,
        211.468055, 246.048907, 225.135194, 210.076305, 226.806762, 212.014431,
        205.123267, 217.564498, 237.150426, 228.565362, 222.036114, 235.621356,
        195.982460, 223.279198, 221.020998, 243.088105, 214.774631, 239.810430,
        197.897572, 244.439113, 241.621129, 260.400953, 216.482178, 236.805076,
        216.680343, 223.816297, 263.188711, 236.311810, 222.950152, 244.636356,
        219.121372, 219.911078, 243.852681, 258.933231, 261.748652, 250.906955,
        267.743972, 226.739575, 254.807401, 249.979759, 273.690377, 240.339217,
        268.406605, 229.825316, 241.621129, 302.928261, 288.344398, 238.549018,
        267.239982, 248.073140, 254.230916, 296.789984, 267.158551, 252.226496,
        271.170860, 248.325354, 253.694013, 266.607649, 281.958112, 273.946966,
        263.018181, 285.795029, 240.677519, 269.896959, 263.356244, 291.076027,
        255.161923, 283.289840, 242.175607, 260.400953, 288.344398, 343.457361,
        257.368309, 284.795470, 263.122266, 271.239770, 320.209823, 283.933299,
        264.416752, 292.035194, 268.764031, 265.345807, 213.994496, 228.971242,
        227.411615, 216.128966, 229.667231, 196.116652, 222.927882, 213.470569,
        241.089661, 209.328714, 233.858455, 201.123644, 216.482178, 238.549018,
        257.368309, 239.295031, 234.913508, 218.066855, 219.648997, 257.969951,
        231.243624, 224.657569, 238.158714, 217.174368, 215.933866, 241.620759,
        252.508513, 252.767900, 244.445283, 260.870105, 217.352348, 247.017507,
        246.182278, 267.772651, 232.277540, 258.306589, 219.820165, 236.805076,
        267.239982, 284.795470, 234.913508, 289.709239, 241.312315, 247.249491,
        286.702147, 264.252852, 245.151647, 264.582984, 240.842689, 245.837476,
        223.602907, 234.242638, 232.823977, 227.436840, 240.751687, 203.533069,
        230.484479, 225.727773, 248.459822, 216.298768, 240.263617, 202.894307,
        216.680343, 248.073140, 263.122266, 218.066855, 241.312315, 255.363057,
        230.209787, 271.091482, 239.220241, 225.387834, 247.486715, 226.052431,
        224.119935, 220.038678, 240.308477, 233.084574, 231.369510, 247.183922,
        204.581690, 233.358639, 229.873732, 249.662698, 220.296241, 246.844456,
        211.468055, 223.816297, 254.230916, 271.239770, 219.648997, 247.249491,
        230.209787, 264.014907, 271.938970, 246.664305, 227.889045, 249.908085,
        232.035369, 229.010298, 264.704959, 285.416390, 278.315614, 270.721492,
        289.044453, 243.603643, 274.935489, 266.295057, 295.935799, 253.817633,
        290.492875, 246.048907, 263.188711, 296.789984, 320.209823, 257.969951,
        286.702147, 271.091482, 271.938970, 352.825726, 286.200221, 267.716897,
        297.182554, 269.776351, 266.721561, 240.341716, 254.792243, 250.872786,
        242.475130, 260.715749, 217.785986, 249.237737, 242.954024, 267.460908,
        237.638235, 267.212598, 225.135194, 236.311810, 267.158551, 283.933299,
        231.243624, 264.252852, 239.220241, 246.664305, 286.200221, 294.042749,
        246.504021, 269.570596, 243.980697, 242.690997, 223.672378, 240.176223,
        235.227909, 226.471530, 244.210258, 204.205559, 235.229584, 225.510760,
        255.668926, 220.785141, 243.218596, 210.076305, 222.950152, 252.226496,
        264.416752, 224.657569, 245.151647, 225.387834, 227.889045, 267.716897,
        246.504021, 259.897656, 251.730847, 229.335712, 229.759185, 244.323303,
        259.048267, 255.104263, 248.130112, 267.159502, 223.747953, 253.029955,
        249.370268, 275.902272, 239.098500, 265.681905, 226.806762, 244.636356,
        271.170860, 292.035194, 238.158714, 264.582984, 247.486715, 249.908085,
        297.182554, 269.570596, 251.730847, 303.872223, 251.585636, 247.878402,
        223.216217, 235.566855, 238.931093, 225.826557, 242.992822, 203.586842,
        228.601700, 227.813265, 248.495606, 220.583355, 244.615890, 212.014431,
        219.121372, 248.325354, 268.764031, 217.174368, 240.842689, 226.052431,
        232.035369, 269.776351, 243.980697, 229.335712, 251.585636, 257.544914,
        228.810942, 226.074990, 236.277617, 235.402356, 228.266719, 244.070245,
        200.165867, 230.512885, 232.141964, 246.827505, 218.962732, 242.543363,
        205.123267, 219.911078, 253.694013, 265.345807, 215.933866, 245.837476,
        224.119935, 229.010298, 266.721561, 242.690997, 229.759185, 247.878402,
        228.810942, 253.353769))
    val fl1 = DenseVector(-892.842851, -934.071560, -932.936015, -888.124343,
      -961.050207, -791.191087, -923.711397, -904.289301, -988.384984,
      -883.909133, -959.465030, -798.551172, -871.622303, -997.463289,
      -1043.912620, -863.013719, -976.975712, -897.033693, -898.694786,
      -1069.245497, -963.491924, -901.263474, -983.768031, -899.865392,
      -902.283567)
    val cost = QuadraticMinimizer.Cost(Hl1, fl1)

    val octaveL1 = DenseVector(0.18611, 0.00000, 0.06317, -0.10417, 0.11262,
      -0.20495, 0.52668, 0.32790, 0.19421, 0.72180, 0.06309, -0.41326, -0.00000,
      0.52078, -0.00000, 0.18040, 0.62915, 0.16329, -0.06424, 0.37539, 0.01659,
      0.00000, 0.11215, 0.24778, 0.04082)

    val s = octaveL1.foldLeft(0.0) { case (agg, entry) => agg + abs(entry) }
    val projectL1 = ProjectL1(s)
    val spgResult = new SpectralProjectedGradient[DenseVector[Double]](
      Projection(projectL1).project)
      .minimizeAndReturnState(cost, DenseVector.zeros[Double](25))
    println(s"SpectralProjectedGradient L1 projection iters ${spgResult.iter}")
    assert(norm(spgResult.x - octaveL1, 2) < 1e-4)
  }
}
