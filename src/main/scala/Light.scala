case class Light(direction: Direction, turn: Turn) {
  def laneSelection: LanePosition = {
    if (turn == Left) {
      LeftLane
    } else {
      RightLane
    }
  }
}
