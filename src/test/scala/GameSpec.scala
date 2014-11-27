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
  describe("a trick"){
    val playerStates = players.map(PlayerState(_,Nil,Nil,Normal))
    it("Hands the lead to the player with the highest card in the leading suit") {
      val trick = Trick(Seq(
        Play(Card(5, Turtle), players(0)),
        Play(Card(7, Turtle), players(1)),
        Play(Card(9, Owl), players(2)),
        Play(Card(10, Owl), players(3))

      ))
      println(trick.orderedPlays)
      assert(trick.newLeader.get == players(1))
      assert(trick.newSkunk.get == players(2))

    }
    it("Hands the skunk to the player with the lowest card in the leading suit") {
      val trick = Trick(Seq(
        Play(Card(5, Turtle), players(0)),
        Play(Card(7, Turtle), players(1)),
        Play(Card(9, Turtle), players(2)),
        Play(Card(10, Turtle), players(3))

      ))
      println(trick.orderedPlays)
      assert(trick.newLeader.get == players(3))
      assert(trick.newSkunk.get == players(0))

    }

    it("Hands the skunk to the player with the lowest card that did not follow suit") {
      val trick = Trick(Seq(
        Play(Card(5, Turtle), players(0)),
        Play(Card(1, Wild), players(1)),
        Play(Card(9, Parrot), players(2)),
        Play(Card(10, Parrot), players(3))

      ))
      println(trick.orderedPlays)
      assert(trick.newLeader.get == players(0))
      assert(trick.newSkunk.get == players(2))

    }

    it("If two people play a wild, the first one to play one goes first") {
      val trick = Trick(Seq(
        Play(Card(5, Turtle), players(0)),
        Play(Card(1, Wild,1), players(1)),
        Play(Card(1, Wild,2), players(2)),
        Play(Card(10, Turtle), players(3))

      ))
      println(trick.orderedPlays)
      assert(trick.newLeader.get == players(3))
      assert(trick.newSkunk.get == players(2))

    }
  }
}
