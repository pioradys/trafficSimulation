import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LightChangesSpec extends AnyFlatSpec with Matchers{
  it should "change traffic light states correctly" in {
    val intersection = new Intersection

    intersection.setLights(Light(North, Right), Green)
    intersection.setLights(Light(South, Left), Red)
    intersection.setLights(Light(East, Straight), Yellow)

    intersection.trafficLights(Light(North, Right)).light shouldBe Green
    intersection.trafficLights(Light(South, Left)).light shouldBe Red
    intersection.trafficLights(Light(East, Straight)).light shouldBe Yellow

  }

  it should "Turn on and turn off whole set of lights" in {
    val intersection = new Intersection

    intersection.turnOn(North, South, Straight)
    intersection.trafficLights(Light(North, Straight)).light shouldBe Green
    intersection.trafficLights(Light(North, Right)).light shouldBe Green
    intersection.trafficLights(Light(South, Right)).light shouldBe Green
    intersection.trafficLights(Light(South, Straight)).light shouldBe Green
    intersection.turnOff(North, South, Straight)
    intersection.trafficLights(Light(North, Straight)).light shouldBe Red
    intersection.trafficLights(Light(North, Right)).light shouldBe Red
    intersection.trafficLights(Light(South, Right)).light shouldBe Red
    intersection.trafficLights(Light(South, Straight)).light shouldBe Red

    intersection.turnOn(East, West, Left)
    intersection.trafficLights(Light(East, Left)).light shouldBe Green
    intersection.trafficLights(Light(West, Left)).light shouldBe Green
    intersection.turnOff(East, West, Left)
    intersection.trafficLights(Light(East, Left)).light shouldBe Red
    intersection.trafficLights(Light(West, Left)).light shouldBe Red
  }



}
