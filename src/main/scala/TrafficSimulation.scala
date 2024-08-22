object TrafficSimulation extends App {
  val intersection = new Intersection()
  intersection.trafficLights.keys.foreach {
    light => intersection.setLights(light,Red)
  }

  // Dodajemy losowe pojazdy do kolejek co sekundę
  new Thread(() => {
    while (true) {
      val randomStart = scala.util.Random.shuffle(List(North, South, East, West)).head
      val randomTurn = scala.util.Random.shuffle(List(Left,Straight,Right)).filterNot(_ == randomStart).head
      val vehicle = Vehicle(randomStart,randomTurn)
      intersection.addVehicle(vehicle)
      Thread.sleep(1000)
    }
  }).start()

  new Thread(()=> {
    while(true) {
      intersection.trafficLights.keys.foreach {
        light => intersection.clearVehicles(light)
      }
    }
  }).start()

  // Symulacja cykli świateł drogowych


  intersection.simulateCycle()





}
