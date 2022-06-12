install.packages('pryr')
library(pryr)

rm(list = ls())

### Create clear board function ###

gen_clear_board <- function(obj_board)
{
  obj_board <-matrix(6,nrow=10,ncol=10)
  colnames(obj_board) <-c("a","b","c","d","e","f","g","h","i","j")
  rownames(obj_board) <-c(1:10)
  class(obj_board)="board"
  return(obj_board)
}



### Create ship object ###


# Ship is a object consisting of a vector 
# It has attribute that says how long it is
# It has attribute that says whether it is vartical or horizontal

create_ship<-function(len){
  
  ship<-rep(1,len)
  class(ship)="ship"
  return(ship)  
}

## This function will check if the ship can be placed in selected spot. Used inside assign ship function

check_if_free <-function(obj_board, obj_ship, vect_spot,vert=TRUE)
{

  
  if (vert==TRUE){
    
    ### vertical ship placement
    ### check if the ship will fit on board
    if (vect_spot[1]+length(obj_ship)-1>nrow(obj_board)){
      return(FALSE)
    } else {
      
      ### chceck if the fields are avialable
      
      BOARD_TEMP=obj_board[
        (vect_spot[1]-1):min(vect_spot[1]+length(obj_ship),nrow(obj_board)),
        (vect_spot[2]-1):min(vect_spot[2]+1,ncol(obj_board))]
      check<-sum(BOARD_TEMP==6)+sum(BOARD_TEMP==0)==dim(BOARD_TEMP)[1]*dim(BOARD_TEMP)[2]
      return(check) }
  } else {

             
        
    ### horizontal ship placement
    ### check if the ship will fit on board
    
    if (vect_spot[2]+length(obj_ship)-1>ncol(obj_board)){
      return(FALSE)
    } else {
      
      BOARD_TEMP=obj_board[
        (vect_spot[1]-1):min(vect_spot[1]+1,nrow(obj_board)),
        (vect_spot[2]-1):min(vect_spot[2]+length(obj_ship),ncol(obj_board))]
      check<-(sum(BOARD_TEMP==6)+sum(BOARD_TEMP==0))==(dim(BOARD_TEMP)[1]*dim(BOARD_TEMP)[2])
      return(check) }
  }
  
}



## This function will be used to change value on the boards to indicate that the ship is placed onto them ##

place_ship_on_board<-function(obj_board,obj_ship,vect_spot,vert=TRUE){

  if (check_if_free(obj_board, obj_ship, vect_spot,vert)==TRUE){

        if (vert==TRUE){
    
          
          for (i in c(0:(1+length(obj_ship))))
          {
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board)),vect_spot[2]-1]=0
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board)),vect_spot[2]]=0
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board)),min(vect_spot[2]+1,ncol(obj_board))]=0
          }
          
          
          for (i in c(1:length(obj_ship)))
            {
            obj_board[vect_spot[1]+i-1,vect_spot[2]]=1
            }
          
          }
          else {

            
            for (i in c(0:(1+length(obj_ship))))
              
            {
              obj_board[vect_spot[1]-1,min(vect_spot[2]+i-1,ncol(obj_board))]=0
              obj_board[vect_spot[1],min(vect_spot[2]+i-1,ncol(obj_board))]=0
              obj_board[min(vect_spot[1]+1,nrow(obj_board)),min(vect_spot[2]+i-1,ncol(obj_board))]=0
            }
            
            
            
            for (i in c(1:length(obj_ship)))
            {
              obj_board[vect_spot[1],vect_spot[2]+i-1]=1
            }
          }  
    }
  
          

  return(obj_board)
  }



board1<-gen_clear_board()



## The board fields can have following values:

## 6. Empty_free - ship can be placed here. Returns miss if shot. Displays as empty
## 0. Empty_reserved - ship cannot be placed here, because it is adjacent to another ship. Returns miss if shot. Displays as empty
## 1. Ship_hidden - Ship cannot be placed here, because there is ship here. Returns hit if shot. Displays as empty
## 2. Ship_hit - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as hit
## 3. Ship_destroyed - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as destroyed
## 4. Miss - Ship cannot be placed here, it appears during the game. Cannot be shot. Displays as miss

## This function will populate the board based on semi-random algorithm  
populate_board<-function(obj_board)
{
  obj_board<-gen_clear_board()
  
  ### create ships to be placed on board
  
  ship11<-create_ship(1)
  ship12<-create_ship(1)
  ship13<-create_ship(1)
  ship14<-create_ship(1)
    
  ship21<-create_ship(2)
  ship22<-create_ship(2)
  ship23<-create_ship(2)
  
  ship31<-create_ship(3)
  ship32<-create_ship(3)
  
  ship41<-create_ship(4)

  
  ## safety function iterator
  
  u=1
  
  ## list of all possible coordinates from (1,1) to (1,100). Generated in Excel.
  
  coor_list<-list(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(1,7),c(1,8),c(1,9),c(1,10),c(2,1),c(2,2),c(2,3),c(2,4),c(2,5),c(2,6),c(2,7),c(2,8),c(2,9),c(2,10),c(3,1),c(3,2),c(3,3),c(3,4),c(3,5),c(3,6),c(3,7),c(3,8),c(3,9),c(3,10),c(4,1),c(4,2),c(4,3),c(4,4),c(4,5),c(4,6),c(4,7),c(4,8),c(4,9),c(4,10),c(5,1),c(5,2),c(5,3),c(5,4),c(5,5),c(5,6),c(5,7),c(5,8),c(5,9),c(5,10),c(6,1),c(6,2),c(6,3),c(6,4),c(6,5),c(6,6),c(6,7),c(6,8),c(6,9),c(6,10),c(7,1),c(7,2),c(7,3),c(7,4),c(7,5),c(7,6),c(7,7),c(7,8),c(7,9),c(7,10),c(8,1),c(8,2),c(8,3),c(8,4),c(8,5),c(8,6),c(8,7),c(8,8),c(8,9),c(8,10),c(9,1),c(9,2),c(9,3),c(9,4),c(9,5),c(9,6),c(9,7),c(9,8),c(9,9),c(9,10),c(10,1),c(10,2),c(10,3),c(10,4),c(10,5),c(10,6),c(10,7),c(10,8),c(10,9),c(10,10))
  
  ## Group ships into list
  
  ship_list<-list(ship41,ship32,ship31,ship23,ship22,ship21,ship14,ship13,ship12,ship11)
  
  ## Possible vertical/horizontal values
  
  vert_list<-c(TRUE,FALSE)
  
  ## Counter for debbuging
  
  # i=1
  
  ## Iterate through all ships on the list
  
  for (ship in ship_list){
    #message(as.character(i))
    #i=i+1
    #message(ship)
    
    # Randomly select coordinates and vertical/horizontal value
    
    vrt<-sample(vert_list,1)
    crd<-sample(coor_list,1)[[1]]
    
    #message(check_if_free(obj_board,ship,crd,vrt))
    
    
    # Change the randomly selected coordinates and orientation until the place to fit the ship is found
    
    while (check_if_free(obj_board,ship,crd,vrt)==FALSE) {
      
      
    
      vrt<-sample(vert_list,1)
      crd<-sample(coor_list,1)[[1]]
      #message(check_if_free(obj_board,ship,crd,vrt))
    
      
      u=u+1
      #message(paste0("u: ", u))
      
      ## safeguard for the case that too many iterations occur
      
      if (u>1000){  
        
        obj_board<-gen_clear_board()
        obj_board<-populate_board(obj_board)
        break
        
      }
      
      
    }
    
    ## when coordinates and orientation is found place the ship there
    
      #message(check_if_free(obj_board,ship,crd,vrt),crd,vrt)
     obj_board<-place_ship_on_board(obj_board,ship,crd,vrt)

  }
  
  
  return(obj_board)

  }



board1<-populate_board(board1)

sum(board1==1)

board1

board2<-gen_clear_board()

check_if_free(board2,create_ship(2),c(7,3),TRUE)

board2<-place_ship_on_board(board2,create_ship(3),c(6,6),FALSE)

board2

# This function will check if the field can be shot and shoot it, if possible
shoot<-function(obj_board,vect_spot)
{
  if (!obj_board[vect_spot[1],vect_spot[2]] %in% c(2,3,4)){
    
    if (obj_board[vect_spot[1],vect_spot[2]] %in% c(6,0)){
      return(4)
    }
    
    if (obj_board[vect_spot[1],vect_spot[2]]==1){
      
      return(2)
      
    }
    
    
  }
  
  
  
  
}

coo

board3<-board2

board3[2,1]<-shoot(board2,c(2,1))

board3


##########################

check_nearby_hits<- function(board,coord,dir="all"){
  
  coord_list<-list()
  ### check if the field is a hit
  
  if (board[coord[1],coord[2]]==2){
    
  ## if yes append it to the list  
    
    append(coord_list,coord)
    
    
    # check if fields around are 1
    
    
    if (dir=="all"){
    
    if ((board[max(coord[1]-1,1),coord[2]]==1 || board[min(coord[1]+1,nrow(board)),coord[2]]==1) || ( board[coord[1],max(coord[2]-1,1)]==1 || board[coord[1],min(coord[2]+1,ncol(board))]==1 ) ){
      
      return(list())
      } else {
      
   
        
      
    }
    
    if (board[coord[1]+1,coord[2]]==1){
      
      return(list())
    }  else{}
      
   
      
  
    
  }
  }
  return(coord_list)
}

shoot(board3,c(9,9))         


board3[9,9]<-shoot(board2,c(9,9))


board3
check_nearby_hits(board3,c(2,1))
# Turn indicator tells which players turn it is now
turn_ind<-1

turn_ind*(-1)
# Check number of ships, that player still have at the begginning of each turn
check_ships <- function(obj_board) # maybe counting number of destroyed fields on board?
{}

