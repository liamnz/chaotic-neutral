# Welcome to Chaotic Neutral, the DnD character randomiser

data <- readRDS('data.Rds')

# First, list any specific exclusions here, otherwise leave empty.

excluded_race <- c(' ')
excluded_class <- c(' ')
excluded_background <- c(' ')

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

ability_scores <- integer(6)
names(ability_scores) <- c('Strength', 'Dexterity', 'Constitution', 'Intelligence', 'Wisdom', 'Charisma')
for (i in 1:6){
  x <- sample(1:6, 4, replace = TRUE)
  x <- sort(x)
  x <- tail(x, 3)
  x <- sum(x)
  ability_scores[i] <- x
}
ability_scores <- t(t(ability_scores))
colnames(ability_scores) <- 'Score'

cat('Race:', race, '\nClass:', class, '\nBackground:', background, '\n\n'); features; cat('\n'); ability_scores
