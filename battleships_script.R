### Create clear board function ###

gen_clear_board <- function(obj_board)
{
  obj_board <-matrix(0,nrow=10,ncol=10)
  colnames(obj_board) <-c("a","b","c","d","e","f","g","h","i","j")
  rownames(obj_board) <-c(1:10)
  return(obj_board)
}


### Generate two boards using the clear board function ###

board1<-gen_clear_board(board1)

board2<-gen_clear_board(board2)


### Create ship object ###


# Ship is a object consisting of a vector 
# It has attribute that says how long it is
# It has attribute that says whether it is vartical or horizontal


## This function will be used to change value on the boards to indicate that the ship is placen onto them ##

place_ship_on_board<-function(obj_board,obj_ship,vect_spot)
{}

## This function will check if the ship can be placed in selected spot. Used inside assign ship function

check_if_free <-function(obj_board, obj_ship, vect_spot)
{}

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

# Check number of ships, that player still have at the begginning of each turn
check_ships <- function(obj_board) # maybe counting number of destroyed fields on board?
{}