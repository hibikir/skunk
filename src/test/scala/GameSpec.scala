import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

class GameSpec extends FunSpec with PropertyChecks{

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
      assert(trick.newLeader.get == players(3))
      assert(trick.newSkunk.get == players(2))

    }
  }
  describe("a player"){
    val playerStates = Dealer.deal(players.take(4))
    it("will play a card on an empty trick if he is the leader"){
      val p = playerStates(0).copy(state = Leader)
      0.to(100).foreach{ x=>
        val (trick,p2) = p.play(Trick())
        assert(trick.plays.size ==1)
        assert(p.hand.contains(trick.plays.head.card))
        assert(trick.plays.head.player==p.player)
      }
    }

    it("will follow suit if it can"){
      val p = playerStates(0)
      0.to(100).foreach{ x=>
        val (trick,p2) = p.play(Trick(Seq(Play(Card(1,Turtle),players(2)))))
        assert(trick.plays.size ==2)
        val lastPlay = trick.plays.last
        assert(p.hand.contains(lastPlay.card))
        assert(lastPlay.player==p.player)
        if(lastPlay.card.suit != Turtle && lastPlay.card.suit !=Wild){
          assert(p.hand.forall(_.suit!= Turtle))
        }
      }
    }
  }
}
