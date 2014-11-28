import scala.util.Random

sealed trait State
case object Normal extends State
case object Leader extends State
case object Skunk extends State

case class PlayerState(player: Player, hand: Seq[Card], tricks: Seq[Seq[Card]] = Seq(), state: State = Normal) {
  def play(trick: Trick): (Trick, PlayerState) = {
    state match {
      case Leader => val card = selectCardAtRandom(hand)
        play(trick, card)
      case Skunk => (trick, copy(state = Normal))
      case _ => val leadFollowingCards = followingLead(trick.plays.head.card)
        val card = if (leadFollowingCards.nonEmpty) selectCardAtRandom(leadFollowingCards) else selectCardAtRandom(hand)
        play(trick, card)
    }
  }

  def play(trick: Trick, card: Card): (Trick, PlayerState) =
    (trick.copy(plays = trick.plays :+ Play(card, player)), copy(hand = hand.filter(_ != card), state = Normal))

  def followingLead(card: Card): Seq[Card] = hand.filter(c => c.suit == Wild || c.suit == card.suit)

  def selectCardAtRandom(cards: Seq[Card]) = cards(Random.nextInt(cards.size))
}

case object Dealer {

  def deal(players: Seq[Player]): Seq[PlayerState] = {
    val deck = Random.shuffle(Deck())
    val handSize = deck.size / players.length
    players.zipWithIndex.map { case (player, idx) => 
      PlayerState(player, deck.slice(idx * handSize, (idx + 1) * handSize))
    }
  }
}

class Game(players: Seq[Player]) {
  
  def play: Seq[PlayerState] = {
    val playerStates  = Dealer.deal(players)
    play(playerStates.updated(0,playerStates.head.copy(state=Leader)))
  }

  private def play(players: Seq[PlayerState]):Seq[PlayerState]  = {
    val played = playTrick(players)
    if (played.exists(_.hand.isEmpty)) played else play(played)
  }

  private def playTrick(players: Seq[PlayerState]): Seq[PlayerState] = {
    val leader = players.find(_.state == Leader).getOrElse(players.head)
    val orderedPlayers = turnOrder(leader, players)
    val (finalTrick, updatedPlayers) = orderedPlayers.foldLeft((Trick(), Seq(): Seq[PlayerState])) {
      case ((trick, newPlayers), player) =>
        val (newTrick, playerState) = player.play(trick)
        (newTrick, newPlayers :+ playerState)
    }
    updatePlayerStatus(leader.player, finalTrick, updatedPlayers)
  }

  private def updatePlayerStatus(leader: Player, trick: Trick, players: Seq[PlayerState]): Seq[PlayerState] = {
    if (trick.newLeader == None) players.map(_.copy(state = Normal))
    else players.map { p =>
      val trickCards = trick.plays.map(_.card)
      if (p.player == trick.newLeader.get) p.copy( state = Leader)
      else if (p.player == trick.newSkunk.get){
        val newTricks = p.tricks :+ trickCards
        p.copy(tricks=newTricks,state = Skunk)
      } 
      else p.copy(state = Normal)
    }
  }

  private def turnOrder(leader: PlayerState, allPlayers: Seq[PlayerState]): Seq[PlayerState] =
  {
    val initialPosition = leader.player.position
    val sorted = allPlayers.sortBy(_.player.position)
    if (initialPosition == 0) sorted 
    else sorted.slice(initialPosition,allPlayers.size) ++ sorted.slice(0,initialPosition)
  }
}
