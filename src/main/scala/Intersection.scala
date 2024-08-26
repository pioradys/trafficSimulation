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


  var waitingVehiclesInfo: Map[Lane, CarsInfo] = Map(
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

  private def oppositeDirection(direction: Direction): Direction = {
    direction match {
      case North => South
      case South => North
      case East => West
      case West => East
    }
  }



  def timeAlgorithmCycle(): Unit = {
    while (true) {
      val min = findMinNonZero(this.waitingVehiclesInfo)
      val minNonOpt = min.getOrElse((Lane(North, RightLane), CarsInfo(0, 0)))

      turnOnLights(minNonOpt)
      turnOffLights(minNonOpt)

      printStatus()
      Thread.sleep(5000)
    }
  }

  def quantityAlgorithmCycle(): Unit = {
    while (true) {
      val maximumWaitTime = 60
      val min = findMinNonZero(this.waitingVehiclesInfo)
      val minNonOpt = min.getOrElse((Lane(North, RightLane), CarsInfo(0, 0)))
      if (nanoSecondToSecond(System.nanoTime() - minNonOpt._2.waitingSince) > maximumWaitTime && minNonOpt._2.waitingSince != 0) {
        turnOnLights(minNonOpt)
        turnOffLights(minNonOpt)
      }
      var max = this.waitingVehiclesInfo.maxBy(_._2.amount)
      turnOnLights(max)
      var previousMax = max
      while (previousMax._1 == max._1 && (previousMax._2.amount != 0 || (previousMax._2.amount +10 >max._2.amount) )) {
        previousMax = (max._1, this.waitingVehiclesInfo(max._1))
        max = this.waitingVehiclesInfo.maxBy(_._2.amount)
        Thread.sleep(50)
      }
      turnOffLights(previousMax)
      max = this.waitingVehiclesInfo.maxBy(_._2.amount)
      printStatus()
    }
  }


  def setLights(light: Light, lightColor: LightColor): Unit = {
    trafficLights(light).light = lightColor
    println(s"$light light is now $lightColor")
  }

  private def printStatus(): Unit = {
    println("Current traffic lights:")
    trafficLights.foreach { case (direction, light) =>
      println(s"$direction: ${light.light}")
    }
    println("Vehicle queues:")
    waitingVehiclesInfo.foreach { case (direction, count) =>
      val time = System.nanoTime()
      println(s"$direction: ${count.amount} vehicles waiting")
      if (count.waitingSince == 0) {
        println(s"$direction: vehicle waiting for 0")
      } else {
        println(s"$direction: vehicle waiting for ${nanoSecondToSecond(time - count.waitingSince)}")
      }

    }
  }

  private def nanoSecondToSecond(time: Long): Long = {
    time / 1000000000
  }


  def addVehicle(vehicle: Vehicle): Unit = {
    val lane = Lane(vehicle.startDirection, vehicle.laneSelection)
    val currentCarsInfo = waitingVehiclesInfo(lane)
    val time = System.nanoTime()

    val updatedCarsInfo = CarsInfo(
      amount = currentCarsInfo.amount + 1,
      waitingSince = if (currentCarsInfo.amount == 0) time else currentCarsInfo.waitingSince
    )
    waitingVehiclesInfo = waitingVehiclesInfo.updated(lane, updatedCarsInfo)

    enqueueVehicle(vehicle)
  }

  private def enqueueVehicle(vehicle: Vehicle): Unit = {
    val (updatedRightQueue, updatedLeftQueue) = vehicle.startDirection match {
      case North => updateQueue(vehicle.laneSelection, vehicle.turn, vehicleQueueNRight, vehicleQueueNLeft)
      case South => updateQueue(vehicle.laneSelection, vehicle.turn, vehicleQueueSRight, vehicleQueueSLeft)
      case East  => updateQueue(vehicle.laneSelection, vehicle.turn, vehicleQueueERight, vehicleQueueELeft)
      case West  => updateQueue(vehicle.laneSelection, vehicle.turn, vehicleQueueWRight, vehicleQueueWLeft)
      case _     => throw InvalidDirectionException(s"Invalid direction: ${vehicle.startDirection}")
    }

    vehicle.startDirection match {
      case North =>
        vehicleQueueNRight = updatedRightQueue
        vehicleQueueNLeft = updatedLeftQueue
      case South =>
        vehicleQueueSRight = updatedRightQueue
        vehicleQueueSLeft = updatedLeftQueue
      case East =>
        vehicleQueueERight = updatedRightQueue
        vehicleQueueELeft = updatedLeftQueue
      case West =>
        vehicleQueueWRight = updatedRightQueue
        vehicleQueueWLeft = updatedLeftQueue
    }
  }


  private def updateQueue(laneSelection: LanePosition, turn: Turn, rightQueue: Queue[Turn], leftQueue: Queue[Turn]): (Queue[Turn], Queue[Turn]) = {
    laneSelection match {
      case RightLane => (rightQueue.enqueue(turn), leftQueue)
      case LeftLane  => (rightQueue, leftQueue.enqueue(turn))
    }
  }




  def clearVehicles(light: Light): Unit = {
    if (( trafficLights(light).light == Green) &&      waitingVehiclesInfo(Lane(light.direction, light.laneSelection)).amount > 0) {

      val vehicleQueue = getVehicleQueue(light.direction, light.laneSelection)

      if (light.laneSelection == RightLane && vehicleQueue.headOption.contains(light.turn) ||
        light.laneSelection == LeftLane) {
        updateVehicleQueue(light.direction, light.laneSelection)
        deletingVehicle(light)
      }

      Thread.sleep(100)
    }
  }

  private def getVehicleQueue(direction: Direction, laneSelection: LanePosition): Queue[Turn] = {
    (direction, laneSelection) match {
      case (North, RightLane) => vehicleQueueNRight
      case (North, LeftLane)  => vehicleQueueNLeft
      case (South, RightLane) => vehicleQueueSRight
      case (South, LeftLane)  => vehicleQueueSLeft
      case (East, RightLane)  => vehicleQueueERight
      case (East, LeftLane)   => vehicleQueueELeft
      case (West, RightLane)  => vehicleQueueWRight
      case (West, LeftLane)   => vehicleQueueWLeft
      case _ => throw InvalidDirectionException(s"Invalid direction: $direction")
    }
  }

  private def updateVehicleQueue(direction: Direction, laneSelection: LanePosition): Unit = {
    (direction, laneSelection) match {
      case (North, RightLane) => vehicleQueueNRight = vehicleQueueNRight.drop(1)
      case (North, LeftLane)  => vehicleQueueNLeft = vehicleQueueNLeft.drop(1)
      case (South, RightLane) => vehicleQueueSRight = vehicleQueueSRight.drop(1)
      case (South, LeftLane)  => vehicleQueueSLeft = vehicleQueueSLeft.drop(1)
      case (East, RightLane)  => vehicleQueueERight = vehicleQueueERight.drop(1)
      case (East, LeftLane)   => vehicleQueueELeft = vehicleQueueELeft.drop(1)
      case (West, RightLane)  => vehicleQueueWRight = vehicleQueueWRight.drop(1)
      case (West, LeftLane)   => vehicleQueueWLeft = vehicleQueueWLeft.drop(1)
      case _ => throw InvalidDirectionException(s"Invalid direction: $direction")
    }
  }


  private def deletingVehicle(light: Light): Unit = {
    val lane = Lane(light.direction, light.laneSelection)
    val currentCarsInfo = waitingVehiclesInfo(lane)
    var updatedCarsInfo = currentCarsInfo.copy(
      amount = currentCarsInfo.amount - 1
    )
    waitingVehiclesInfo = waitingVehiclesInfo.updated(lane, updatedCarsInfo)
    if (waitingVehiclesInfo(Lane(light.direction, light.laneSelection)).amount == 0) {
      updatedCarsInfo = updatedCarsInfo.copy(
        waitingSince = 0
      )
    } else {
      updatedCarsInfo = updatedCarsInfo.copy(
        waitingSince = System.nanoTime()
      )
    }
    waitingVehiclesInfo = waitingVehiclesInfo.updated(lane, updatedCarsInfo)
  }


  private def changeLightsForTurns(
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


  def turnOnLights(laneData: (Lane, CarsInfo)): Unit = {
    val (direction, lanePosition) = (laneData._1.direction, laneData._1.lanePosition)
    val turn = if (lanePosition == RightLane) Right else Left
    turnOn(direction, oppositeDirection(direction), turn)
  }

  def turnOffLights(laneData: (Lane, CarsInfo)): Unit = {
    val (direction, lanePosition) = (laneData._1.direction, laneData._1.lanePosition)
    val turn = if (lanePosition == RightLane) Right else Left
    turnOff(direction, oppositeDirection(direction), turn)
  }

  def turnOff(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    val movements = if (turn == Right || turn == Straight) List(Straight, Right) else List(Left)

    changeLightsForTurns(direction, oppositeDirection, movements, Yellow)
    Thread.sleep(2000)

    changeLightsForTurns(direction, oppositeDirection, movements, Red)
    Thread.sleep(5000)
  }

  def turnOn(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    val movements = if (turn == Right || turn == Straight) List(Straight, Right) else List(Left)

    changeLightsForTurns(direction, oppositeDirection, movements, RedYellow)
    Thread.sleep(1000)

    changeLightsForTurns(direction, oppositeDirection, movements, Green)
    Thread.sleep(5000)
  }


  //Used Scala version is 2.12.19 where minByOption is not added yet, that's why there is manual implementation for it
  private def minByOption[T, U](seq: Seq[T])(f: T => U)(implicit ord: Ordering[U]): Option[T] = {
    if (seq.isEmpty) None
    else Some(seq.minBy(f))
  }

  def findMinNonZero(vehicleNumber: Map[Lane, CarsInfo]): Option[(Lane, CarsInfo)] = {
    minByOption(vehicleNumber.toSeq.filter(_._2.waitingSince > 0))(_._2.waitingSince)
  }


}
