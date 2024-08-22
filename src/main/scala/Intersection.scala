import TrafficSimulation.intersection

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Queue


class Intersection {
  // Stan świateł dla każdego kierunku
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

  // Liczba pojazdów czekających w każdym kierunku

  var vehicleNumber: Map[Lane, Int] = Map(
    Lane(North, LeftLane) -> 25,
    Lane(North, RightLane) -> 0,
    Lane(South, LeftLane) -> 0,
    Lane(South, RightLane) -> 10,
    Lane(East, LeftLane) -> 0,
    Lane(East, RightLane) -> 0,
    Lane(West, LeftLane) -> 0,
    Lane(West, RightLane) -> 0,
  )
  var vehicleQueueNRight: Queue[Turn] = Queue()
  var vehicleQueueNLeft: Queue[Turn] = Queue(Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left,Left)
  var vehicleQueueSRight: Queue[Turn] = Queue(Right,Right,Right,Right,Right,Right,Right,Right,Right,Right)
  var vehicleQueueSLeft: Queue[Turn] = Queue()
  var vehicleQueueERight: Queue[Turn] = Queue()
  var vehicleQueueELeft: Queue[Turn] = Queue()
  var vehicleQueueWRight: Queue[Turn] = Queue()
  var vehicleQueueWLeft: Queue[Turn] = Queue()

  val oppositePairs: Seq[(Direction, Direction)] = List(
    (North, South),
    (East, West)
  )

  // Symulacja jednego cyklu świateł
  def simulateCycle(): Unit = {
    while(true) {
      var max = this.vehicleNumber.maxBy(_._2)
      while (true) {
        max._1 match {
          case Lane(North, RightLane) | Lane(South, RightLane) => turnOn(North, South, Right)
          case Lane(North, LeftLane) | Lane(South, LeftLane) => turnOn(North, South, Left)
          case Lane(West, RightLane) | Lane(East, RightLane) => turnOn(East, West, Right)
          case Lane(West, LeftLane) | Lane(East, LeftLane) => turnOn(East, West, Left)
        }
        var previousMax = max
        while (previousMax._1 == max._1 && previousMax._2 != 0) {
          previousMax = (max._1, this.vehicleNumber(max._1))
          max = this.vehicleNumber.maxBy(_._2)
          Thread.sleep(50)
        }
        max._1 match {
          case Lane(North, RightLane) | Lane(South, RightLane) => turnOff(North, South, Right)
          case Lane(North, LeftLane) | Lane(South, LeftLane) => turnOff(North, South, Left)
          case Lane(West, RightLane) | Lane(East, RightLane) => turnOff(East, West, Right)
          case Lane(West, LeftLane) | Lane(East, LeftLane) => turnOff(East, West, Left)
        }
        max = this.vehicleNumber.maxBy(_._2)
        printStatus()
      }
    }
  }

  // Ustawienie świateł dla danego kierunku
  def setLights(light: Light, lightColor: LightColor): Unit = {
    trafficLights(light).light = lightColor
    println(s"$light light is now $lightColor")
  }

  // Dodanie pojazdu do kolejki w danym kierunku
  def addVehicle(vehicle: Vehicle): Unit = {
    vehicleNumber =  vehicleNumber.updated(Lane(vehicle.startDirection, vehicle.laneSelection), vehicleNumber(Lane(vehicle.startDirection, vehicle.laneSelection)) + 1)
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

  // Usunięcie pojazdów z kolejki, gdy światło jest zielone
  def clearVehicles(light: Light): Unit = {
    if (trafficLights(light).light == Green && vehicleNumber(Lane(light.direction, light.laneSelection)) > 0) {
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
      Thread.sleep(50)
    }
  }

  private def deletingVehicle(light: Light): Unit = {
    vehicleNumber= vehicleNumber.updated(Lane(light.direction, light.laneSelection), vehicleNumber(Lane(light.direction, light.laneSelection)) - 1)
  }

  // Wyświetlenie stanu skrzyżowania
  def printStatus(): Unit = {
    println("Current traffic lights:")
    trafficLights.foreach { case (direction, light) =>
      println(s"$direction: ${light.light}")
    }
    println("Vehicle queues:")
    vehicleNumber.foreach { case (direction, count) =>
      println(s"$direction: $count vehicles waiting")
    }
  }

  def turnOn(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    if (turn == Right || turn == Straight) {
      setLights(Light(direction, Straight), RedYellow)
      setLights(Light(direction, Right), RedYellow)
      setLights(Light(oppositeDirection, Straight), RedYellow)
      setLights(Light(oppositeDirection, Right), RedYellow)
      Thread.sleep(1000) // wartość z przepisów


      setLights(Light(direction, Straight), Green)
      setLights(Light(direction, Right), Green)
      setLights(Light(oppositeDirection, Straight), Green)
      setLights(Light(oppositeDirection, Right), Green)
      Thread.sleep(5000)
    } else {
      setLights(Light(direction, Left), RedYellow)
      setLights(Light(oppositeDirection, Left), RedYellow)
      Thread.sleep(1000) // wartość z przepisów


      setLights(Light(direction, Left), Green)
      setLights(Light(oppositeDirection, Left), Green)
      Thread.sleep(5000)

    }
  }

  def turnOff(direction: Direction, oppositeDirection: Direction, turn: Turn): Unit = {
    if (turn == Right || turn == Straight) {
      setLights(Light(direction, Straight), Yellow)
      setLights(Light(direction, Right), Yellow)
      setLights(Light(oppositeDirection, Straight), Yellow)
      setLights(Light(oppositeDirection, Right), Yellow)
      Thread.sleep(1000) // wartość z przepisów


      setLights(Light(direction, Straight), Red)
      setLights(Light(direction, Right), Red)
      setLights(Light(oppositeDirection, Straight), Red)
      setLights(Light(oppositeDirection, Right), Red)
      Thread.sleep(5000)
    } else {
      setLights(Light(direction, Left), Yellow)
      setLights(Light(oppositeDirection, Left), Yellow)
      Thread.sleep(2000) // wartość z przepisów


      setLights(Light(direction, Left), Red)
      setLights(Light(oppositeDirection, Left), Red)
      Thread.sleep(5000)
    }
  }

}
