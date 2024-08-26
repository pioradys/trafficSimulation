case class Vehicle(startDirection: Direction, turn: Turn) {
  def laneSelection: LanePosition = {
    if (turn == Left) {
      LeftLane
    } else {
      RightLane
    }
  }
}
