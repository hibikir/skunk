
sealed trait Suit

case object Turtle extends Suit //2-20
case object Fox extends Suit //2-16
case object Parrot extends Suit //2-14
case object Owl extends Suit //2-10
case object Wild extends Suit //4 1s
//case object Skunk extends Suit

case class Player(name:String, position:Int)

case class Card(number:Int,suit:Suit,id:Int = 1){
  def canBePlayedOn(card:Card) = suit == Wild || suit == card.suit
}

object Deck {
  def apply():List[Card] = {
    val seq = 2.to(20).map(Card(_,Turtle)) ++
    2.to(16).map(Card(_,Fox)) ++
    2.to(14).map(Card(_,Parrot)) ++
    2.to(10).map(Card(_,Owl)) ++
    1.to(4).map(Card(1,Wild,_)) //++
    //List(Card(0,Skunk))
    seq.toList
  }
}
