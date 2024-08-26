import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LightCycleSpec extends AnyFlatSpec with Matchers{

  "An Intersection" should "removing vehicles from the queue" in {
    val intersection = new Intersection

    intersection.addVehicle(Vehicle(North, Left))
    intersection.addVehicle(Vehicle(North, Right))
    intersection.addVehicle(Vehicle(North, Straight))

    intersection.setLights(Light(North, Straight), Green)
    intersection.clearVehicles(Light(North, Right))
    intersection.clearVehicles(Light(North, Straight))
    intersection.vehicleNumber(Lane(North, RightLane)).amount should be(2)

    intersection.setLights(Light(North, Right), Green)
    intersection.clearVehicles(Light(North, Right))
    intersection.clearVehicles(Light(North, Straight))
    intersection.vehicleNumber(Lane(North, RightLane)).amount should be(0)
  }

}
