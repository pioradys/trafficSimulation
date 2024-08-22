// Reprezentacja świateł drogowych
sealed trait LightColor

case object Red extends LightColor

case object RedYellow extends LightColor

case object Yellow extends LightColor

case object Green extends LightColor

// Kierunki ruchu na skrzyżowaniu
sealed trait Direction

case object North extends Direction

case object South extends Direction

case object East extends Direction

case object West extends Direction

sealed trait Turn

case object Left extends Turn

case object Right extends Turn

case object Straight extends Turn

sealed trait LanePosition

case object LeftLane extends LanePosition

case object RightLane extends LanePosition