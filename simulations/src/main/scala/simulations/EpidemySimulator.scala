package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Double = 0.01
    val initialNumInfected: Int = (population * prevalenceRate).toInt
    val incubationPeriod: Int = 6
    val diePeriod: Int = 14
    val immunePeriod: Int = 16
    val healthyPeriod: Int = 18
    val dieProbability: Double = 0.25
    val transmissibilityRate: Double = 0.40
  }

  import SimConfig._


  def initPersons(list: List[Person], id: Int): List[Person] = {
    if (id < population) {
      val p = new Person(id)
      p.scheduleNextMove()
      if (id < initialNumInfected) {
        p.infectPerson()
      }
      initPersons(p :: list, id + 1)
    } else {
      list
    }
  }

  val persons: List[Person] = initPersons(List(), 0)


  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)


    def scheduleNextMove() {
      afterDelay(randomBelow(5) + 1) {
        move()
      }
    }

    def infectPerson() {
      infected = true
      afterDelay(incubationPeriod) {
        sick = true
      }
      afterDelay(diePeriod) {
        if (random < dieProbability) {
          dead = true
          sick = false
        }
      }
      afterDelay(immunePeriod) {
        if (!dead) immune = true; sick = false
      }
      afterDelay(healthyPeriod) {
        if (!dead) immune = false; infected = false
      }
    }

    def infect() {
      val isInfectedRoom = !persons.filter(p => {
        p.infected && p.row == row && p.col == col
      }).isEmpty

      if (!infected && !immune && isInfectedRoom && random < transmissibilityRate) {
        infectPerson()
      }

    }


    def move() {
      if (dead) return

      val neighborRooms = List(
        ((row + 1) % roomRows, col),

        (row, (col + 1) % roomColumns),

        (if (row - 1 == -1) roomRows else row - 1, col),

        (row, if (col - 1 == -1) roomColumns else col - 1)

      )

      val visiblyInfectedPeople = persons.filter(p => {
        p.sick || p.dead
      })

      val nextRooms = neighborRooms.filter {
        case (r, c) => visiblyInfectedPeople.find {
          p => (p.row == r) && (p.col == c)
        }.isEmpty
      }

      if (!nextRooms.isEmpty) {
        val pos: (Int, Int) = nextRooms(randomBelow(nextRooms.length))
        row = pos._1
        col = pos._2
        infect()

      }
      scheduleNextMove()
    }
  }

}
