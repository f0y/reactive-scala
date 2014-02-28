package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("simple orgate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    genericHelperOrGateMethod(in1, in2, out, "simple")
  }

  test("fair orgate") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    genericHelperOrGateMethod(in1, in2, out, "fair")
  }
  
  def genericHelperOrGateMethod(in1: Wire, in2: Wire, out: Wire, name: String) {
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)


    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(!out.getSignal, s"$name or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal, s"$name or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal, s"$name or 3")
  }

  test("demux") {
    val numControlWires = 1
    val numOutputWires = Math.pow(2, numControlWires).toInt
    val in = new Wire
    probe("in", in)
    var controlWires = List[Wire]()
    var outputWires = List[Wire]()
    for { n <- 0 to numControlWires - 1} {
      val wire = new Wire
      probe(s"c$n", wire)
      controlWires = wire :: controlWires
    }
    for { n <- 0 to numOutputWires - 1} {
      val wire = new Wire
      probe(s"o$n", wire)
      outputWires = wire :: outputWires
    }

    demux(in, controlWires, outputWires)
    in.setSignal(true)
    controlWires(0).setSignal(false)
    run

    controlWires(0).setSignal(true)
    run

    controlWires(0).setSignal(false)
    run
  }


  test("demux with no controls test") {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(true)
    run

    assert(out.getSignal === true, "out signal")
  }

  test("demux with 1 controls test") {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out2, out1))
    in.setSignal(false)
    run
    assert(out2.getSignal === false, "out2 false")
    assert(out1.getSignal === false, "out1 false")

    in.setSignal(true)
    run
    assert(out2.getSignal === false, "out2 false")
    assert(out1.getSignal === true, "out1 true")

    c.setSignal(true)
    run
    assert(out2.getSignal === true, "out2 true")
    assert(out1.getSignal === false, "out1 false")
  }

  test("demux with 2 controls test") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c1,c0), List(out3, out2, out1, out0))
    run

    assert(out3.getSignal === false, "out3 false in,c0,c1 false")
    assert(out2.getSignal === false, "out2 false in,c0,c1 false")
    assert(out1.getSignal === false, "out1 false in,c0,c1 false")
    assert(out0.getSignal === false, "out0 false in,c0,c1 false")

    in.setSignal(true)
    run
    assert(out3.getSignal === false, "out3 false in true;c0,c1 false")
    assert(out2.getSignal === false, "out2 false in true;c0,c1 false")
    assert(out1.getSignal === false, "out1 false in true;c0,c1 false")
    assert(out0.getSignal === true, "out0 true in true;c0,c1 false")

    c1.setSignal(true)
    run
    assert(out3.getSignal === false, "out3 false in,c1 true;c0 false")
    assert(out2.getSignal === true, "out2 true in,c1 true;c0 false")
    assert(out1.getSignal === false, "out1 false in,c1 true;c0 false")
    assert(out0.getSignal === false, "out0 false in,c1 true;c0 false")

    c0.setSignal(true)
    run

    assert(out3.getSignal === true, "out3 true in,c1,c0 true; false")
    assert(out2.getSignal === false, "out2 false in,c1,c0 true; false")
    assert(out1.getSignal === false, "out1 false in,c1,c0 true; false")
    assert(out0.getSignal === false, "out0 false in,c1,c0 true; false")

    c1.setSignal(false)
    run

    assert(out3.getSignal === false, "out3 true in,c0 true;c1 false")
    assert(out2.getSignal === false, "out2 false in,c0 true;c1 false")
    assert(out1.getSignal === true, "out1 true in,c0 true;c1 false")
    assert(out0.getSignal === false, "out0 false in,c0 true;c1 false")
  }

  test("simple demux") {
    val in, c, o1, o2 = new Wire
    probe("in", in)
    probe("c", c)
    probe("o1", o1)
    probe("o2", o2)

    simpleDemux(in, c, o1, o2)
    in.setSignal(false)
    c.setSignal(false)
    run
    assert(!o1.getSignal)
    assert(!o2.getSignal)

    c.setSignal(true)
    run
    assert(!o1.getSignal)
    assert(!o2.getSignal)

    in.setSignal(true)
    run

    assert(!o1.getSignal)
    assert(o2.getSignal)

    c.setSignal(false)
    run

    assert(o1.getSignal)
    assert(!o2.getSignal)

  }

}
