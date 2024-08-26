import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AlgorithmSpec extends AnyFlatSpec with Matchers{
  "An Intersection" should "correctly turn on lights for maximum quantity" in {
    val intersection = new Intersection

    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(North, LeftLane), CarsInfo(10, 12312455))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(East, LeftLane), CarsInfo(1, 99312455))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(North, RightLane), CarsInfo(6, 12312455))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(West, RightLane), CarsInfo(4, 99312455))


    val max: (Lane, CarsInfo) = intersection.vehicleNumber.maxBy(_._2.amount)

    intersection.turnOnLightsForMaximumQuantity(max)

    intersection.trafficLights(Light(North, Left)).light shouldBe Green
    intersection.trafficLights(Light(East, Left)).light shouldBe Red
    intersection.trafficLights(Light(West, Right)).light shouldBe Red
    intersection.trafficLights(Light(North, Right)).light shouldBe Red
    intersection.trafficLights(Light(West, Straight)).light shouldBe Red
    intersection.trafficLights(Light(North, Straight)).light shouldBe Red

    intersection.turnOffLightForMaximumQuantity(max)

    intersection.trafficLights(Light(North, Left)).light shouldBe Red
    intersection.trafficLights(Light(East, Left)).light shouldBe Red
    intersection.trafficLights(Light(West, Right)).light shouldBe Red
    intersection.trafficLights(Light(North, Right)).light shouldBe Red
    intersection.trafficLights(Light(West, Straight)).light shouldBe Red
    intersection.trafficLights(Light(North, Straight)).light shouldBe Red


  }
  it should "not process vehicles when the light is not green" in {
    val intersection = new Intersection

    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(North, LeftLane), CarsInfo(10, 12312455))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(East, LeftLane), CarsInfo(100, 99312455))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(North, RightLane), CarsInfo(62, 12312457))
    intersection.vehicleNumber = intersection.vehicleNumber.updated(Lane(West, RightLane), CarsInfo(43, 99312453))


    val min = intersection.findMinNonZero(intersection.vehicleNumber)
    val minNonOpt = min.getOrElse((Lane(North, RightLane), CarsInfo(0, 0)))

    intersection.turnOnLightsForMinimumTime(minNonOpt)

    intersection.trafficLights(Light(North, Left)).light shouldBe Green
    intersection.trafficLights(Light(East, Left)).light shouldBe Red
    intersection.trafficLights(Light(West, Right)).light shouldBe Red
    intersection.trafficLights(Light(North, Right)).light shouldBe Red
    intersection.trafficLights(Light(West, Straight)).light shouldBe Red
    intersection.trafficLights(Light(North, Straight)).light shouldBe Red

    intersection.turnOffLightsForMinimumTime(minNonOpt)

    intersection.trafficLights(Light(North, Left)).light shouldBe Red
    intersection.trafficLights(Light(East, Left)).light shouldBe Red
    intersection.trafficLights(Light(West, Right)).light shouldBe Red
    intersection.trafficLights(Light(North, Right)).light shouldBe Red
    intersection.trafficLights(Light(West, Straight)).light shouldBe Red
    intersection.trafficLights(Light(North, Straight)).light shouldBe Red
  }
}
