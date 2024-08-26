import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClearVehiclesSpec extends AnyFlatSpec with Matchers {

  "An Intersection" should "removing vehicles from the queue" in {
    val intersection = new Intersection

    intersection.addVehicle(Vehicle(North, Left))
    intersection.addVehicle(Vehicle(North, Right))
    intersection.addVehicle(Vehicle(North, Straight))

    intersection.vehicleNumber(Lane(North, LeftLane)).amount should be(1)
    intersection.vehicleNumber(Lane(North, RightLane)).amount should be(2)
    intersection.vehicleNumber(Lane(South, LeftLane)).amount should be(0)

    intersection.setLights(Light(North, Straight), Green)
    intersection.clearVehicles(Light(North, Right))
    intersection.clearVehicles(Light(North, Straight))
    intersection.vehicleNumber(Lane(North, RightLane)).amount should be(2)

    intersection.setLights(Light(North, Right), Green)
    intersection.clearVehicles(Light(North, Right))
    intersection.clearVehicles(Light(North, Straight))
    intersection.vehicleNumber(Lane(North, RightLane)).amount should be(0)
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
    intersection.vehicleNumber(Lane(North, LeftLane)).amount should be(1)

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
    intersection.vehicleNumber(Lane(North, LeftLane)).amount should be(1)

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
    intersection.vehicleNumber(Lane(North, LeftLane)).amount should be(1)
  }
}
