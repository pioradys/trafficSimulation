import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntersectionSpec extends AnyFlatSpec with Matchers {

  "An Intersection" should "allow adding vehicles to the queue" in {
    val intersection = new Intersection

    // Dodajemy pojazdy do kolejki w różnych kierunkach
    intersection.addVehicle(Vehicle(North, Left))
    intersection.addVehicle(Vehicle(North, Right))
    intersection.addVehicle(Vehicle(North, Straight))

    // Sprawdzamy, czy kolejki zostały poprawnie zaktualizowane
    intersection.vehicleNumber(Lane(North,LeftLane)) should be (1)
    intersection.vehicleNumber(Lane(North,RightLane)) should be (2)
    intersection.vehicleNumber(Lane(South,LeftLane)) should be (0)
  }


}
