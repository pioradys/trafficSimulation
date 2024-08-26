import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClearVehiclesSpec extends AnyFlatSpec with Matchers {

  "An Intersection" should "removing vehicles from the queue when the light is green" in {
    val intersection = new Intersection

    intersection.addVehicle(Vehicle(North, Left))
    intersection.addVehicle(Vehicle(North, Right))
    intersection.addVehicle(Vehicle(North, Straight))

    intersection.waitingVehiclesInfo(Lane(North, LeftLane)).amount should be(1)
    intersection.waitingVehiclesInfo(Lane(North, RightLane)).amount should be(2)
    intersection.waitingVehiclesInfo(Lane(South, LeftLane)).amount should be(0)

    intersection.turnOnLights((Lane(North,RightLane),intersection.waitingVehiclesInfo(Lane(North,RightLane))))
    intersection.clearVehicles(Light(North, Right))
    intersection.clearVehicles(Light(North, Straight))
    intersection.waitingVehiclesInfo(Lane(North, RightLane)).amount should be(0)
  }


  it should "not process vehicles when the light is not green" in {
    val intersection = new Intersection

    intersection.addVehicle(Vehicle(North, Left))

    intersection.turnOff(North, South, Left)
    intersection.clearVehicles(Light(North, Left))

    intersection.vehicleQueueNLeft should have size 1
    intersection.vehicleQueueSLeft should have size 0
    intersection.vehicleQueueELeft should have size 0
    intersection.vehicleQueueWLeft should have size 0
    intersection.vehicleQueueNRight should have size 0
    intersection.vehicleQueueERight should have size 0
    intersection.vehicleQueueWRight should have size 0
    intersection.vehicleQueueSRight should have size 0
    intersection.waitingVehiclesInfo(Lane(North, LeftLane)).amount should be(1)

    intersection.setLights(Light(North, Left), Yellow)
    intersection.clearVehicles(Light(North, Left))

    intersection.vehicleQueueNLeft should have size 1
    intersection.vehicleQueueSLeft should have size 0
    intersection.vehicleQueueELeft should have size 0
    intersection.vehicleQueueWLeft should have size 0
    intersection.vehicleQueueNRight should have size 0
    intersection.vehicleQueueERight should have size 0
    intersection.vehicleQueueWRight should have size 0
    intersection.vehicleQueueSRight should have size 0
    intersection.waitingVehiclesInfo(Lane(North, LeftLane)).amount should be(1)

    intersection.setLights(Light(North, Left), RedYellow)
    intersection.clearVehicles(Light(North, Left))

    intersection.vehicleQueueNLeft should have size 1
    intersection.vehicleQueueSLeft should have size 0
    intersection.vehicleQueueELeft should have size 0
    intersection.vehicleQueueWLeft should have size 0
    intersection.vehicleQueueNRight should have size 0
    intersection.vehicleQueueERight should have size 0
    intersection.vehicleQueueWRight should have size 0
    intersection.vehicleQueueSRight should have size 0
    intersection.waitingVehiclesInfo(Lane(North, LeftLane)).amount should be(1)
  }


  it should "process vehicles when the light is green arrow and lane is clear" in {
    val intersection = new Intersection

    intersection.addVehicle(Vehicle(North, Right))

    intersection.turnOff(North, South, Left)
    intersection.clearVehicles(Light(North, Left))

    intersection.vehicleQueueNLeft should have size 0
    intersection.vehicleQueueSLeft should have size 0
    intersection.vehicleQueueELeft should have size 0
    intersection.vehicleQueueWLeft should have size 0
    intersection.vehicleQueueNRight should have size 1
    intersection.vehicleQueueERight should have size 0
    intersection.vehicleQueueWRight should have size 0
    intersection.vehicleQueueSRight should have size 0
    intersection.waitingVehiclesInfo(Lane(North, RightLane)).amount should be(1)

    intersection.turnOnLights((Lane(East,RightLane),intersection.waitingVehiclesInfo(Lane(East,RightLane))))
    intersection.clearVehicles(Light(North, Right))

    intersection.vehicleQueueNLeft should have size 0
    intersection.vehicleQueueSLeft should have size 0
    intersection.vehicleQueueELeft should have size 0
    intersection.vehicleQueueWLeft should have size 0
    intersection.vehicleQueueNRight should have size 0
    intersection.vehicleQueueERight should have size 0
    intersection.vehicleQueueWRight should have size 0
    intersection.vehicleQueueSRight should have size 0
    intersection.waitingVehiclesInfo(Lane(North, LeftLane)).amount should be(0)


  }
}
