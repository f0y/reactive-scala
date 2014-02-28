package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }

}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val na, nb, c = new Wire
    inverter(a1, na)
    inverter(a2, nb)
    andGate(na, nb, c)
    inverter(c, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    assert(Math.pow(2, c.size).toInt == out.size, "amount of output wires " +
      "do not adhere to amount of control wires")
    applyDemux(in, c, out)
  }

  def applyDemux(in: Wire, c: List[Wire], out: List[Wire]): Unit = c match {
      case head::tail =>
        val out0, out1 = new Wire
        val mid = out.length / 2
        simpleDemux(in, head, out0, out1)
        applyDemux(out1, tail, out drop mid)
        applyDemux(out0, tail, out take mid)
      case Nil =>
        andGate(in, in, out(0))
  }

  def simpleDemux(in: Wire, c: Wire, out0: Wire, out1: Wire) {
    val nc = new Wire
    inverter(c, nc)
    andGate(in, nc, out1)
    andGate(in, c, out0)
  }


}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
}