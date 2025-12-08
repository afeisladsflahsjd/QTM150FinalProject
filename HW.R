df <- data.frame(name=c("ace", "king", "queen", "jack", "ten", "nine", "ace", 
                        "king", "queen", "jack", "ten", "nine","ace", "king", 
                        "queen", "jack", "ten", "nine","ace", "king", "queen", 
                        "jack", "ten", "nine","ace", "king", "queen", "jack", 
                        "ten", "nine","ace", "king", "queen", "jack", "ten", 
                        "nine","ace", "king", "queen", "jack", "ten", "nine",
                        "ace", "king", "queen", "jack", "ten", "nine"), 
                 suit=c("spades","spades","spades","spades","spades","spades",
                        "spades","spades","spades","spades","spades","spades",
                        "diamonds","diamonds","diamonds","diamonds","diamonds",
                        "diamonds","diamonds","diamonds","diamonds","diamonds",
                        "diamonds","diamonds","hearts","hearts","hearts","hearts",
                        "hearts","hearts","hearts","hearts","hearts","hearts",
                        "hearts","hearts","clubs","clubs","clubs","clubs","clubs",
                        "clubs","clubs","clubs","clubs","clubs","clubs","clubs"),
                 value=c(1, 1, 0, 0, 1, 0,1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0,
                         1, 1, 0, 0, 1, 0,1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0,
                         1, 1, 0, 0, 1, 0,1, 1, 0, 0, 1, 0),
                 back=c("red", "red", "red", "red", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue","red", "red", 
                        "red", "red", "red", "red", "blue","blue", "blue", 
                        "blue", "blue", "blue","red", "red", "red", "red", 
                        "red","red", "blue", "blue", "blue", "blue", "blue", 
                        "blue","red", "red", "red", "red", "red", "red", "blue",
                        "blue", "blue", "blue", "blue", "blue"))

shuffle_deck <- function(deck){
  deck <- deck[sample(nrow(deck)), ]
  write.csv(deck, file="shuffled_deck.csv", row.names=FALSE)
  return(deck)
}



shuffle_and_deal <- function(deck) {
  deck <- deck[sample(nrow(deck)), ]
  vec <- (1:12)*4
  run <- c("ace_spades", "king_spades", "queen_spades", "jack_spades", "ten_spades")
  hand1 <- paste(deck[vec-3,1], deck[vec-3,2], sep="_")
  hand2 <- paste(deck[vec-2,1], deck[vec-2,2], sep="_")
  hand3 <- paste(deck[vec-1,1], deck[vec-1,2], sep="_")
  hand4 <- paste(deck[vec,1], deck[vec,2], sep="_")
  hands <- list(hand1, hand2, hand3, hand4)
  runs <- sapply(hands, function(h) all(run %in% h))
  return(runs)
}
results <- as.vector(replicate(10000, shuffle_and_deal(df)))
spades_hands <- data.frame(suit="spades", has_run=results)

prob_run <- sum(spades_hands$has_run)/10000
prob_run