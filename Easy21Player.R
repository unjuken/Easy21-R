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
    },
    
    step = function(action){
      state=self$state
      self$history  = c(self$history, paste("player: ", action))
      
      if(action == "hit"){
        drawCard = self$draw_card()
        value = drawCard[[2]]
        color = drawCard[[1]]
        
        self$history  = c(self$history, paste("Color: ", color, " . Value: ", value))
        
        self$state$player_score = self$compute_new_score(value, color, current_score = self$state$player_score)
        new_state = self$state$clone()
        
        if(self$goes_bust(self$state$player_score)){
          # player goes bust
          reward = -1
          state = "terminal"
          self$history  = c(self$history, state)
          return(list(state, reward))
        }
        else{
          reward = 0
          self$history  = c(self$history, new_state)
          return(list(self$state, reward))
        }
      }
      else{
        new_state = self$state$clone()
        self$history  = c(self$history, new_state)
        #TODO
        dealer = self$dealer_moves()
        return(dealer)
      }
    },
    
    dealer_moves = function(){
      
      while(self$state$dealer_score < 17)
      {
        drawCard = self$draw_card()
        value = drawCard[[2]]
        color = drawCard[[1]]
        new_dealer_score = self$compute_new_score(value, color, self$state$dealer_score)
        self$state$dealer_score = new_dealer_score
        
        new_state = self$state$clone()
        
        #Add to history
        self$history  = c(self$history, paste("dealer: ", "hit"))
        self$history  = c(self$history, paste("Color: ", color, " . Value: ", value))
        
        if(self$goes_bust(new_dealer_score))
        {
          # dealer goes bust, player wins
          reward = 1
          state = "terminal"
          self$history  = c(self$history, state)
          return(list(state, reward))
        }
        else
        {
          self$history  = c(self$history, new_state)
        }
      }
      
      self$history  = c(self$history, "dealer: stick")
      player_score = self$state$player_score
      dealer_score = self$state$dealer_score
      
      # score > 17 -> dealer sticks
      state = "terminal"
      self$history  = c(self$history, state)
      
      if(dealer_score < player_score){ # player wins 
        reward = 1
        return(list(state, reward))    
      }
      if(dealer_score == player_score){ # draw
        reward = 0
        return(list(state, reward))       
      }
      if(dealer_score > player_score){ # player loses
        reward = -1
        return(list(state, reward))
      }
      
    },
    
    hit = function(){
      self$step("hit")
    },
    
    stick = function(){
      self$step("stick")
    }
  )
)


s = Easy21Player$new()
s$hit()
