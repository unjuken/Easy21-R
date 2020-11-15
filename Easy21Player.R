# install.packages("R6")
library(R6)

Easy21State <- R6Class("Easy21State",
   list(
     dealer_score = "numeric", 
     player_score = "numeric",
     initialize = function(dealer_score = 0, player_score = 0) {
       self$dealer_score = dealer_score
       self$player_score = player_score
     },
     print = function (){
       cat("dealer_score:", self$dealer_score, "\n")
       cat("player_score:", self$player_score, "\n")
     }
     )
)


Easy21Player <- R6Class("Easy21Player",
  private = list(
    
  ),
  public = list(
    #Public Properties
    actions = c("hit", "stick"),
    probabilityOfBlack = 2/3,
    state = Easy21State$new(),
    history = list(),
    
    #Public Methods
    initialize = function() {
      player_score = self$draw_card()
      dealer_score = self$draw_card()
      self$state$dealer_score = dealer_score[[2]]
      self$state$player_score = player_score[[2]]
      init_state = self$state$clone()
      self$history = c(self$history, init_state)
    },
    
    draw_card = function(){
      value = sample(1:10, 1)
      colorRand = runif(1, 0, 1)
      color = "red"
      if(colorRand <= self$probabilityOfBlack) {
        color = "black"
      }
      cat("card drawn: ", color, value, "\n")
      return(list(color, value))
    },
    
    goes_bust = function(score){
      return ((score > 21) || (score < 1));
    },
    
    compute_new_score = function(value, color, current_score){
      if(color == "black"){
        new_score = current_score + value
      }
      else{
        new_score = current_score - value 
      }
      return(new_score)
    }
    
    step = function(state, action){
      
    }
  )
)


s = Easy21Player$new()
s$compute_new_score(3, "black", 5)
s$state
s$history
