import org.scalatest.FunSpec

class GameSpec extends FunSpec{

  val players = 0.to(10).map(x=>Player(x.toString,x))
  describe("a deal") {
    it("gives 10 cards to 6 players"){
        val playerStatuses = Dealer.deal(players.take(6))
        playerStatuses.foreach(x=>assert(x.hand.size == 10))
    }

    it("gives 12 cards to 5 players"){
      val playerStatuses = Dealer.deal(players.take(5))
      playerStatuses.foreach(x=>assert(x.hand.size == 12))
    }

    it("gives 10 cards to 4 players"){
      val playerStatuses = Dealer.deal(players.take(4))
      playerStatuses.foreach(x=>assert(x.hand.size == 15))
    }
  }
}
