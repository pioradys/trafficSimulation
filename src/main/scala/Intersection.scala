import scala.collection.immutable.Queue


class Intersection {
  var trafficLights: Map[Light, TrafficLight] = Map(
    Light(North, Straight) -> TrafficLight(North, Straight, Red),
    Light(North, Left) -> TrafficLight(North, Left, Red),
    Light(North, Right) -> TrafficLight(North, Right, Red),
    Light(South, Straight) -> TrafficLight(South, Straight, Red),
    Light(South, Left) -> TrafficLight(South, Left, Red),
    Light(South, Right) -> TrafficLight(South, Right, Red),
    Light(East, Straight) -> TrafficLight(East, Straight, Red),
    Light(East, Left) -> TrafficLight(East, Left, Red),
    Light(East, Right) -> TrafficLight(East, Right, Red),
    Light(West, Straight) -> TrafficLight(West, Straight, Red),
    Light(West, Left) -> TrafficLight(West, Left, Red),
    Light(West, Right) -> TrafficLight(West, Right, Red),
  )


  var vehicleNumber: Map[Lane, CarsInfo] = Map(
    Lane(North, LeftLane) -> CarsInfo(0, 0),
    Lane(North, RightLane) -> CarsInfo(0, 0),
    Lane(South, LeftLane) -> CarsInfo(0, 0),
    Lane(South, RightLane) -> CarsInfo(0, 0),
    Lane(East, LeftLane) -> CarsInfo(0, 0),
    Lane(East, RightLane) -> CarsInfo(0, 0),
    Lane(West, LeftLane) -> CarsInfo(0, 0),
    Lane(West, RightLane) -> CarsInfo(0, 0),
  )
  var vehicleQueueNRight: Queue[Turn] = Queue()
  var vehicleQueueNLeft: Queue[Turn] = Queue()
  var vehicleQueueSRight: Queue[Turn] = Queue()
  var vehicleQueueSLeft: Queue[Turn] = Queue()
  var vehicleQueueERight: Queue[Turn] = Queue()
  var vehicleQueueELeft: Queue[Turn] = Queue()
  var vehicleQueueWRight: Queue[Turn] = Queue()
  var vehicleQueueWLeft: Queue[Turn] = Queue()


  def simulateCycle(): Unit = {
    while (true) {

      val min = findMinNonZero(this.vehicleNumber)
      val minNonOpt=min match {
        case Some(minValue) => minValue
        case None => (Lane(North,RightLane),CarsInfo(0,0))
      }

      minNonOpt._1 match {
        case Lane(North, RightLane) | Lane(South, RightLane) => turnOn(North, South, Right)
          turnOff(North, South, Right)
        case Lane(North, LeftLane) | Lane(South, LeftLane) => turnOn(North, South, Left)
          turnOff(North, South, Left)
        case Lane(West, RightLane) | Lane(East, RightLane) => turnOn(East, West, Right)
          turnOff(East, West, Right)
        case Lane(West, LeftLane) | Lane(East, LeftLane) => turnOn(East, West, Left)
          turnOff(East, West, Left)
      }
      //      min._1 match {
      //        case Lane(North, RightLane) | Lane(South, RightLane) => turnOff(North, South, Right)
      //        case Lane(North, LeftLane) | Lane(South, LeftLane) => turnOff(North, South, Left)
      //        case Lane(West, RightLane) | Lane(East, RightLane) => turnOff(East, West, Right)
      //        case Lane(West, LeftLane) | Lane(East, LeftLane) => turnOff(East, West, Left)
      //      }
//      min = this.vehicleNumber.minBy(_._2.amount)
      printStatus()

    }
  }


  def setLights(light: Light, lightColor: LightColor): Unit = {
    trafficLights(light).light = lightColor
    println(s"$light light is now $lightColor")
  }


  def addVehicle(vehicle: Vehicle): Unit = {
    val lane = Lane(vehicle.startDirection, vehicle.laneSelection)
    val currentCarsInfo = vehicleNumber(lane)
    val time = System.nanoTime()
    val updatedCarsInfo = if (currentCarsInfo.amount == 0) {
      CarsInfo(currentCarsInfo.amount + 1, time)
    } else {
      CarsInfo(currentCarsInfo.amount + 1, currentCarsInfo.waitingSince)
    }
    vehicleNumber = vehicleNumber.updated(lane, updatedCarsInfo)
    vehicle.startDirection match {
      case North => vehicle.laneSelection match {
        case RightLane => vehicleQueueNRight = vehicleQueueNRight.enqueue(vehicle.turn)
        case LeftLane => vehicleQueueNLeft = vehicleQueueNLeft.enqueue(vehicle.turn)
      }
      case South => vehicle.laneSelection match {
        case RightLane => vehicleQueueSRight = vehicleQueueSRight.enqueue(vehicle.turn)
        case LeftLane => vehicleQueueSLeft = vehicleQueueSLeft.enqueue(vehicle.turn)
      }
      case East => vehicle.laneSelection match {
        case RightLane => vehicleQueueERight = vehicleQueueERight.enqueue(vehicle.turn)
        case LeftLane => vehicleQueueELeft = vehicleQueueELeft.enqueue(vehicle.turn)
      }
      case West => vehicle.laneSelection match {
        case RightLane => vehicleQueueWRight = vehicleQueueWRight.enqueue(vehicle.turn)
        case LeftLane => vehicleQueueWLeft = vehicleQueueWLeft.enqueue(vehicle.turn)
      }
      case _ => throw InvalidDirectionException(s"Invalid direction: ${vehicle.startDirection}")
    }
  }


  def clearVehicles(light: Light): Unit = {
    if (trafficLights(light).light == Green && vehicleNumber(Lane(light.direction, light.laneSelection)).amount > 0) {
      light.direction match {
        case North => light.laneSelection match {
          case RightLane => if (vehicleQueueNRight.headOption.contains(light.turn)) {
            vehicleQueueNRight = vehicleQueueNRight.drop(1)
            deletingVehicle(light)
          }
          case LeftLane => vehicleQueueNLeft = vehicleQueueNLeft.drop(1)
            deletingVehicle(light)
        }
        case South => light.laneSelection match {
          case RightLane => if (vehicleQueueSRight.headOption.contains(light.turn)) {
            vehicleQueueSRight = vehicleQueueSRight.drop(1)
            deletingVehicle(light)
          }
          case LeftLane => vehicleQueueSLeft = vehicleQueueSLeft.drop(1)
            deletingVehicle(light)
        }
        case East => light.laneSelection match {
          case RightLane => if (vehicleQueueERight.headOption.contains(light.turn)) {
            vehicleQueueERight = vehicleQueueERight.drop(1)
            deletingVehicle(light)
          }
          case LeftLane => vehicleQueueELeft = vehicleQueueELeft.drop(1)
            deletingVehicle(light)
        }
        case West => light.laneSelection match {
          case RightLane => if (vehicleQueueWRight.headOption.contains(light.turn)) {
            vehicleQueueWRight = vehicleQueueWRight.drop(1)
            deletingVehicle(light)
          }
          case LeftLane => vehicleQueueWLeft = vehicleQueueWLeft.drop(1)
            deletingVehicle(light)
        }
        case _ => throw InvalidDirectionException(s"Invalid direction: ${light.direction}")
      }
      Thread.sleep(500)
    }
  }

  private def deletingVehicle(light: Light): Unit = {
    val lane = Lane(light.direction, light.laneSelection)
    val currentCarsInfo = vehicleNumber(lane)
    var updatedCarsInfo = currentCarsInfo.copy(
      amount = currentCarsInfo.amount - 1
    )
    vehicleNumber = vehicleNumber.updated(lane, updatedCarsInfo)
    if (vehicleNumber(Lane(light.direction, light.laneSelection)).amount == 0) {
      updatedCarsInfo = updatedCarsInfo.copy(
        waitingSince = 0
      )
    } else {
      updatedCarsInfo = updatedCarsInfo.copy(
        waitingSince = System.nanoTime()
      )
    }
    vehicleNumber = vehicleNumber.updated(lane, updatedCarsInfo)
  }


  def printStatus(): Unit = {
    println("Current traffic lights:")
    trafficLights.foreach { case (direction, light) =>
      println(s"$direction: ${light.light}")
    }
    println("Vehicle queues:")
    vehicleNumber.foreach { case (direction, count) =>
      val time = System.nanoTime()
      println(s"$direction: ${count.amount} vehicles waiting")
      if (count.waitingSince == 0) {
        println(s"$direction: vehicle waiting for 0")
      } else {
        println(s"$direction: vehicle waiting for ${(time - count.waitingSince) / 1000000000}")
      }

    }
  }

  def changeLightsForTurns(
                            direction: Direction,
                            oppositeDirection: Direction,
                            movements: List[Turn],
                            color: LightColor
                          ): Unit = {
    movements.foreach { move =>
      setLights(Light(direction, move), color)
      setLights(Light(oppositeDirection, move), color)
    }
  }

  def turnOff(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    val movements = if (turn == Right || turn == Straight) List(Straight, Right) else List(Left)

    changeLightsForTurns(direction, oppositeDirection, movements, Yellow)
    Thread.sleep(2000) // wartość z przepisów

    changeLightsForTurns(direction, oppositeDirection, movements, Red)
    Thread.sleep(5000)
  }

  def turnOn(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    val movements = if (turn == Right || turn == Straight) List(Straight, Right) else List(Left)

    changeLightsForTurns(direction, oppositeDirection, movements, RedYellow)
    Thread.sleep(1000) // wartość z przepisów

    changeLightsForTurns(direction, oppositeDirection, movements, Green)
    Thread.sleep(5000)
  }
  //Used Scala version is 2.12.19 where minByOption is not added yet, that's why there is manual implementation for it
  def minByOption[T, U](seq: Seq[T])(f: T => U)(implicit ord: Ordering[U]): Option[T] = {
    if (seq.isEmpty) None
    else Some(seq.minBy(f))
  }

  def findMinNonZero(vehicleNumber: Map[Lane, CarsInfo]): Option[(Lane, CarsInfo)] = {
    minByOption(vehicleNumber.toSeq.filter(_._2.waitingSince > 0))(_._2.waitingSince)
  }




}
