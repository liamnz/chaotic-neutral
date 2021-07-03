# Welcome to Chaotic Neutral, the DnD character randomiser

data <- readRDS('data.Rds')

# First, list any specific exclusions here, otherwise leave empty.

excluded_race <- c(' ')
excluded_class <- c(' ')
excluded_background <- c(' ')

#and pick an ability score generation method

ability_score_selection = 
  #'4d6'
  'std_array'
#'point_buy'


# Now randomise 
# seed <- set.seed(42069)

race <- data$race[!grepl(paste0(excluded_race, collapse = '|'), data$race)]
race <- sample(race, 1)

class <- data$class[!grepl(paste0(excluded_class, collapse = '|'), data$class)]
class <- sample(class, 1)

background <- data$background[!grepl(paste0(excluded_background, collapse = '|'), data$background)]
background <- sample(background, 1)

features <- c(
  Personality = sample(1:8, 1),
  Ideal = sample(1:6, 1),
  Bond = sample(1:6, 1),
  Flaw = sample(1:6, 1)
)
features <- t(t(features))
colnames(features) <- 'Index'


if (ability_score_selection == '4d6'){
  #4d6
  ability_scores <- integer(6)
  names(ability_scores) <- c('Strength', 'Dexterity', 'Constitution', 'Intelligence', 'Wisdom', 'Charisma')
  for (i in 1:6){
    x <- sample(1:6, 4, replace = TRUE)
    x <- sort(x)
    x <- tail(x, 3)
    x <- sum(x)
    ability_scores[i] <- x
  }
}

if (ability_score_selection == 'std_array'){
  #std array
  std <- c (15, 14, 13, 12, 10, 8)
  ability_scores <- sample(std)
  names(ability_scores) <- c('Strength', 'Dexterity', 'Constitution', 'Intelligence', 'Wisdom', 'Charisma')
}


if (ability_score_selection == 'point_buy'){
  #Rng Point Buy
  score <- c (8,9,10,11,12,13,14,15)
  cost <- c (0,1,2,3,4,5,7,9)
  
  points_left = 27
  ability_cost <- integer(6)
  ability_scores <- integer(6)
  for (i in 1:6){
    #determine the max points currently available to assign to a score
    max <- min(points_left,9)
    avaliable_cost <- cost[!cost %in% (max+1:10)]
    
    #pick on at random from the costs availble,
    if (i < 5){
      ability_cost[i]<- sample(avaliable_cost,1)
      points_left <- sum(points_left, -ability_cost[i])
    } else {
      # take the max for the last two iterations, to ensure all points are used
      ability_cost[i]<- max(avaliable_cost)
      points_left <- sum(points_left, -ability_cost[i])
    }
    #now find the index of each cost, and get the coresponding ability score
    
    ability_scores[i] <-score[match(ability_cost[i],cost)]
    
  }
  ability_scores <- sample(ability_scores)
  names(ability_scores) <- c('Strength', 'Dexterity', 'Constitution', 'Intelligence', 'Wisdom', 'Charisma')
}


ability_scores <- t(t(ability_scores))
colnames(ability_scores) <- 'Score'

cat('Race:', race, '\nClass:', class, '\nBackground:', background, '\n\n'); features; cat('\n'); ability_scores
