import scala.util.Random

case class Play(card:Card,player:Player)

case class Trick(plays:Seq[Play]= Nil){
  val orderedPlays = {
    plays match {
      case Nil => Nil
      case x =>
        val suit = x.head.card.suit
        val followedSuit = plays.filter(p => p.card.suit == suit || p.card.suit == Wild).sortBy(- _.card.number)
        followedSuit ++ 
          plays.filter(!followedSuit.contains(_)).sortBy(- _.card.number)
    }
  }
  val newLeader = orderedPlays match {
    case Nil =>None
    case _ => Some(orderedPlays.head.player)
  }
  
  val newSkunk = orderedPlays match {
    case Nil =>None
    case _ => Some(orderedPlays.last.player)
  }
}

sealed trait Status
case object Normal extends Status
case object Leader extends Status
case object Skunk extends Status

case class PlayerState(player:Player, hand:Seq[Card], tricks:Seq[Card]=Seq(), status:Status =Normal){
  def play(trick:Trick) :(Trick,PlayerState) = {
    status match {
      case Leader => val card = selectCardAtRandom(hand)
       play(trick,card)
      case Skunk => (trick,copy(status = Normal))
      case _ => val leadFollowingCards = followingLead(trick.plays.head.card)
        val card = if(leadFollowingCards.nonEmpty) selectCardAtRandom(leadFollowingCards) else selectCardAtRandom(hand)
        play(trick,card)
    }
  }
  def play (trick:Trick,card:Card):(Trick,PlayerState) = 
    (trick.copy(plays=trick.plays:+Play(card,player)),copy(hand = hand.filter(_!=card),status = Normal))
  
  def followingLead(card:Card) :Seq[Card] = hand.filter(c=> c.suit == Wild || c.suit == card.suit)
  def selectCardAtRandom(cards:Seq[Card]) = cards(Random.nextInt(cards.size))
}

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
  
  def play(players: Seq[PlayerState]) {
    val played = playTrick(players)
    if(played.exists(_.hand.isEmpty)) played else play(played)
  }
  
  def playTrick(players: Seq[PlayerState]) :Seq[PlayerState] = {
    val leader = players.find(_.status==Leader).getOrElse(players.head)
    val orderedPlayers = turnOrder(leader,players)
    val (finalTrick, updatedPlayers)  = orderedPlayers.foldLeft((Trick(),Seq():Seq[PlayerState])) {
      case ((trick, newPlayers), player) =>
        val (newTrick, playerState) = player.play(trick)
        (newTrick, newPlayers :+ playerState)
    }
    updatePlayerStatus(leader.player,finalTrick,updatedPlayers)
  }
  
  def updatePlayerStatus(leader:Player,trick:Trick,players:Seq[PlayerState]) : Seq[PlayerState] = {
    if(trick.newLeader == None) players.map(_.copy(status = Normal))
    else players.map{p => 
      if (p.player== trick.newLeader.get) p.copy(status=Leader)
      else if (p.player == trick.newSkunk.get) p.copy(status = Skunk)
      else p.copy(status=Normal)
  }}
  
  def turnOrder(leader:PlayerState,allPlayers: Seq[PlayerState]) : Seq[PlayerState] = ???
  /*{
    val initialPosition = leader.player.position
    
  }*/
}
