import scala.util.Random

case class Trick(cards:Seq[Card])

sealed trait Status
case object Normal extends Status
case object Leader extends Status
case object Skunk extends Status

case class PlayerState(player:Player, hand:Seq[Card], tricks:Seq[Card]=Seq(), status:Status =Normal)

case object Dealer{
  
  def deal(players:Seq[Player]) :Seq[PlayerState] = {
    val deck = Random.shuffle(Deck())
    val handSize = deck.size/players.length
    players.zipWithIndex.map{case (player,idx) => PlayerState(player,deck.slice(idx*handSize,(idx+1)*handSize))}
  }
}

class Game(players:Seq[Player]){
  def play: Unit = {
    
  }
}
