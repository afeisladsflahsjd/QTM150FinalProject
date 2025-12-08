# create empty data frames
fair_noswitch_df <- data.frame(prize_door = integer(), first_pick = integer(), 
                               reveal_door = integer(), second_pick = integer(),
                               prize_won = character(), win = logical())

fair_switch_df <- data.frame(prize_door = integer(), first_pick = integer(), 
                             reveal_door = integer(), second_pick = integer(),
                             prize_won = character(), win = logical())

general_montyhall_df <- data.frame(bias_or_fair = character(), num_doors =
                                     integer(), num_reveal = integer(), 
                                   strategy = character(), prize_door = 
                                     integer(), first_pick = integer(), 
                                   reveal_door = I(list()), second_pick = 
                                     integer(), prize_won = character(), 
                                   win = logical())

# outputs a single row representing one round of the monty hall problem
# where the player does not switch doors
fair_noswitch_single_round <- function() {
  doors <- 1:3
  # randomly choose one of the doors to be the prize door
  prize_door <- sample(doors, 1)
  # randomly choose one door to be first pick
  first_pick <- sample(doors, 1)
  # reveal a door that is not the first pick and is not the prize door
  reveal_door <- sample(doors[doors != first_pick & doors != prize_door], 1)
  # player wins if prize door was their first pick
  win <- prize_door == first_pick
  # depending on win or not, player gets car or goat
  prize_won <- ifelse(win, "car", "goat")
  # create new row to add to dataframe representing the round
  new_row <- data.frame(list(prize_door = prize_door, first_pick = first_pick, 
                             reveal_door = reveal_door, 
                             second_pick = first_pick, prize_won = prize_won, 
                             win = win))
  return(new_row)
}

# outputs a single row representing one round of the monty hall problem
# where the player does switch doors
fair_switch_single_round <- function() {
  doors <- 1:3
  # randomly choose one of the doors to be the prize door
  prize_door <- sample(doors, 1)
  # randomly choose one door to be first pick
  first_pick <- sample(doors, 1)
  # reveal a door that is not the first pick and is not the prize door
  reveal_door <- ifelse(first_pick == prize_door, sample(
    doors[doors != first_pick], 1), doors[doors != first_pick & doors != 
                                            prize_door])
  # switch doors to the one not revealed
  second_pick <- doors[doors!= reveal_door & doors != first_pick]
  # win if second pick is the prize door
  win <- prize_door == second_pick
  # determine prize won
  prize_won <- ifelse(win, "car", "goat")
  # create a new row to add to the dataframe representing the round
  new_row <- data.frame(list(prize_door = prize_door, first_pick = first_pick, 
                             reveal_door = reveal_door, 
                             second_pick = second_pick, prize_won = prize_won, 
                             win = win))
  return(new_row) 
}

# general monty hall problem single round function
general_single_round <- function(bias_or_fair, num_doors, num_reveal, strat) {
  # doors vector representing number of doors
  doors <- 1:num_doors
  # vector representing relative probabilities of prize being behind each door
  # only use this vector if biased
  probs <- rev(doors)
  # select prize door, according to probabilities if biased
  prize_door <- ifelse(bias_or_fair == "biased", sample(doors, 1, prob=probs),
                       sample(doors, 1))
  # select first door, randomly if strategy is "random", otherwise choose 
  # door selected in strategy
  first_pick <- ifelse(class(strat) == "character", sample(doors, 1), strat)
  # select the chosen number of doors to reveal, ensuring prize door and first
  # pick are not selected
  reveal_door <- sample(doors[doors != prize_door & doors != first_pick], 
                        num_reveal)
  # create a logical vector that aligns with the doors vector, TRUE if the
  # corresponding value in doors is a valid choice for second pick 
  door_options <- !(doors %in% c(reveal_door, first_pick))
  # if biased, select the lowest valid door choice for the second pick, as that
  # will have the highest probability
  if(bias_or_fair == "biased") {
    door_options <- doors*door_options
    door_options[door_options == 0] <- NA
    second_pick <- min(door_options, na.rm=TRUE)
  } 
  # otherwise, randomly choose a valid door for second pick
  else {
    second_pick <- sample(doors[(!doors %in% c(first_pick, reveal_door))],1)
  }
  # determine if player wins
  win <- prize_door == second_pick
  # determine prize
  prize_won <- ifelse(win, "car", "goat")
  # generate row to represent the round
  new_row <- data.frame(bias_or_fair = bias_or_fair, num_doors = num_doors,
                        num_reveal = num_reveal, strategy = as.character(strat),
                        prize_door = prize_door, first_pick = first_pick,
                        reveal_door = I(list(reveal_door)), 
                        second_pick = second_pick, prize_won = prize_won, 
                        win = win)
  return(new_row)
}

# call each function 10,000 times, and concatenate each row into 
# a single dataframe
fair_noswitch_df <- do.call(rbind, replicate(10000, 
                                             fair_noswitch_single_round(), 
                                             simplify = FALSE))
# check probability of winning when you do not switch -- should be around 0.33
sum(fair_noswitch_df$win)/10000

fair_switch_df <- do.call(rbind, replicate(10000, 
                                           fair_switch_single_round(), 
                                           simplify = FALSE))
# check probability of winning when you do switch -- should be around 0.67
sum(fair_switch_df$win)/10000

# check all general monty hall probabilities for those given in instructions
general_montyhall_df <- do.call(rbind, replicate(10000,
                                                 general_single_round("biased",
                                                                     4, 2, 4), 
                                                 simplify =FALSE))
# ~0.9
sum(general_montyhall_df$win)/10000

general_montyhall_df <- do.call(rbind, replicate(10000,
                                                 general_single_round("biased",
                                                                      4, 2, 1), 
                                                 simplify =FALSE))
# ~0.6
sum(general_montyhall_df$win)/10000

general_montyhall_df <- do.call(rbind, replicate(10000,
                                                 general_single_round("biased",
                                                                    10, 2, 10), 
                                                 simplify =FALSE))
# ~0.22
sum(general_montyhall_df$win)/10000

general_montyhall_df <- do.call(rbind, replicate(10000,
                                                 general_single_round("fair",
                                                                      4, 1, 
                                                                      "random"), 
                                                 simplify =FALSE))
# ~0.375
sum(general_montyhall_df$win)/10000

general_montyhall_df <- do.call(rbind, replicate(10000,
                                                 general_single_round("biased",
                                                                      10, 2, 1), 
                                                 simplify =FALSE))
# ~0.21
sum(general_montyhall_df$win)/10000


# combines switch and no switch dataframes
combined_df <- rbind(fair_noswitch_df, fair_switch_df)
# shuffles switch and no switch dataframes
combined_df <- combined_df[sample(nrow(combined_df)),]
# creates logical vector where TRUE represents when the player didnt switch
noswitch <- combined_df$first_pick == combined_df$second_pick
# remakes no switch dataframe based on logical vector
noswitch_sep <- combined_df[noswitch,]
# remakes switch dataframe based on logical vector
switch_sep <- combined_df[!noswitch,]



