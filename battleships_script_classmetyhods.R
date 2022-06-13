install.packages('pryr')
library(pryr)


### Create clear board function ###

gen_clear_board <- function(obj_board)
{
  obj_board <-matrix(6,nrow=10,ncol=10)
  colnames(obj_board) <-c("a","b","c","d","e","f","g","h","i","j")
  rownames(obj_board) <-c(1:10)
  class(obj_board)="board"
  return(obj_board)
}

### Generate two boards using the clear board function ###

board1<-gen_clear_board(board1)  

board2<-gen_clear_board(board2)

#board1

#board1[1,1]<-5

#sum(board1)

#sum(board1==5)

### Create ship object ###


# Ship is a object consisting of a vector 
# It has attribute that says how long it is
# It has attribute that says whether it is vartical or horizontal

create_ship<-function(len){
  
  ship<-rep(1,len)
  class(ship)="ship"
  return(ship)  
}

ship1<-create_ship(3)

## This function will be used to change value on the boards to indicate that the ship is placed onto them ##

board.place_ship_on_board<-function(x,obj_ship,vect_spot,vert=TRUE){
  if (check_if_free(obj_board, obj_ship, vect_spot)==TRUE){
    
        if (vert==TRUE){
    
          
          for (i in c(0:(1+length(obj_ship))))
          {
            obj_board[vect_spot[1]-1,vect_spot[2]+i-1]=0
            obj_board[vect_spot[1],vect_spot[2]+i-1]=0
            obj_board[vect_spot[1]+1,vect_spot[2]+i-1]=0
          }
          
          
          for (i in c(1:length(obj_ship)))
            {
            obj_board[vect_spot[1],vect_spot[2]+i-1]=1
            }
          
          }else {
            
            for (i in c(1:length(obj_ship)))
            {
              obj_board[vect_spot[1]+i-1,vect_spot[2]]=1
            }
          }  
    }
  
          

  return(obj_board)
  }





## This function will check if the ship can be placed in selected spot. Used inside assign ship function

check_if_free <-function(obj_board, obj_ship, vect_spot,vert=TRUE)
{
  
    
  
  if (vert==TRUE){
    
    ### vertical ship placement
    ### check if the ship will fit on board
    
    if (vect_spot[2]+length(obj_ship)>ncol(obj_board)){
      return(FALSE)
    } else {
    
    ### chceck if the fields are avialable
      
    BOARD_TEMP=obj_board[(vect_spot[1]-1):(vect_spot[1]+1),(vect_spot[2]-1):(vect_spot[2]+length(obj_ship))]
    check<-sum(BOARD_TEMP==6)+sum(BOARD_TEMP==0)==dim(BOARD_TEMP)[1]*dim(BOARD_TEMP)[2]
    return(check) }
  } else {
    
    ### horizontal ship placement
    ### check if the ship will fit on board
    
    if (vect_spot[1]+length(obj_ship)>nrow(obj_board)){
      
      return(FALSE)
    } else {
    
    BOARD_TEMP=obj_board[(vect_spot[1]-1):(vect_spot[1]+length(obj_ship)),(vect_spot[2]-1):(vect_spot[2]+1)]
  check<-(sum(BOARD_TEMP==6)+sum(BOARD_TEMP==0))==(dim(BOARD_TEMP)[1]*dim(BOARD_TEMP)[2])
    return(check) }
  }
  
}

board1<-place_ship_on_board(board1,ship1,c(2,5))

board1

## The board fields can have following values:

## 1. Empty_free - ship can be placed here. Returns miss if shot. Displays as empty
## 2. Empty_reserved - ship cannot be placed here, because it is adjacent to another ship. Returns miss if shot. Displays as empty
## 3. Ship_hidden - Ship cannot be placed here, because there is ship here. Returns hit if shot. Displays as empty
## 4. Ship_hit - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as hit
## 5. Ship_destroyed - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as destroyed
## 6. Miss - Ship cannot be placed here, it appears during the game. Cannot be shot. Displays as miss

## This function will populate the board based on semi-random algorithm  
populate_board<-function(obj_board)
{}

# This function will check if the field can be shot and shoot it, if possible
shoot<-function(obj_board,vect_spot)
{}

# Turn indicator tells which players turn it is now
turn_ind<-FALSE

# Check number of ships, that player still have at the beginning of each turn
check_ships <- function(obj_board) # maybe counting number of destroyed fields on board?
{}

