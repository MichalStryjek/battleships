install.packages('pryr')
library(pryr)


#Create new environment that will be used to contain objects modified by functions

en<-new.env()
ai<-new.env()

### Create clear board function ###

            
new_board <-function(){
  
  obj_board <-array(c(rep(0,200)),dim=c(10,10,3) )
  dimnames(obj_board)<-list(c(1:10),c("a","b","c","d","e","f","g","h","i","j"))
  class(obj_board)="board"
  return(obj_board)
}
            



### Create ship object ###


# Ship is a object consisting of a vector 
# It has attribute that says how long it is
# It has attribute that says whether it is vartical or horizontal

create_ship<-function(len,id){
  
  
  coor_list<-c(-1,-1)
  dups <- list(coor_list)[rep(1,len)]
  
  state<-"intact"
  
  hits<-0
  
  ship<-list(id,len,hits,state,dups)
  
  names(ship)=c("id","length","hits","state","coordinates")
  class(ship)="ship"
  return(ship)  
}


length.ship<-function(ship){
  
  
  return(ship$length)
  
}

state.ship<-function(ship){
  
  
  
  return(ship$state)
}

## This function will check if the ship can be placed in selected spot. Used inside assign ship function

check_if_free <-function(obj_board, obj_ship, vect_spot,vert=TRUE)
{

  
  if (vert==TRUE){
    
    ### vertical ship placement
    ### check if the ship will fit on board
    if (vect_spot[1]+length(obj_ship)-1>nrow(obj_board[,,2])){
      return(FALSE)
    } else {
      
      ### chceck if the fields are avialable
      
      BOARD_TEMP=obj_board[
        (vect_spot[1]-1):min(vect_spot[1]+length(obj_ship),nrow(obj_board[,,2])),
        (vect_spot[2]-1):min(vect_spot[2]+1,ncol(obj_board[,,2])),2]
      check<-sum(BOARD_TEMP==6)+sum(BOARD_TEMP==0)==dim(BOARD_TEMP)[1]*dim(BOARD_TEMP)[2]
      return(check) }
  } else {

             
        
    ### horizontal ship placement
    ### check if the ship will fit on board
    
    if (vect_spot[2]+length(obj_ship)-1>ncol(obj_board[,,2])){
      return(FALSE)
    } else {
      
      BOARD_TEMP=obj_board[
        (vect_spot[1]-1):min(vect_spot[1]+1,nrow(obj_board[,,2])),
        (vect_spot[2]-1):min(vect_spot[2]+length(obj_ship),ncol(obj_board[,,2])),2]
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
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board[,,2])),vect_spot[2]-1,2]=0
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board[,,2])),vect_spot[2],2]=0
            obj_board[min(vect_spot[1]+i-1,nrow(obj_board[,,2])),min(vect_spot[2]+1,ncol(obj_board[,,2])),2]=0
          }
          
          
          for (i in c(1:length(obj_ship)))
            {
            obj_board[vect_spot[1]+i-1,vect_spot[2],2]=1
            obj_board[vect_spot[1]+i-1,vect_spot[2],3]=obj_ship$id
            #message(c(vect_spot[1],vect_spot[2]))
            obj_ship$coordinates[[i]]=c(vect_spot[1]+i-1,vect_spot[2])
            }
          
          }
          else {

            
            for (i in c(0:(1+length(obj_ship))))
              
            {
              obj_board[vect_spot[1]-1,min(vect_spot[2]+i-1,ncol(obj_board[,,2])),2]=0
              obj_board[vect_spot[1],min(vect_spot[2]+i-1,ncol(obj_board[,,2])),2]=0
              obj_board[min(vect_spot[1]+1,nrow(obj_board)),min(vect_spot[2]+i-1,ncol(obj_board[,,2])),2]=0
            }
            
            
            
            for (i in c(1:length(obj_ship)))
            {
              obj_board[vect_spot[1],vect_spot[2]+i-1,2]=1
              obj_board[vect_spot[1],vect_spot[2]+i-1,3]=obj_ship$id
              obj_ship$coordinates[[i]]=c(vect_spot[1],vect_spot[2]+i-1)
            }
          }  
    }
  
          
  return(list(obj_board,obj_ship))
  }





#board1[ship1$coordinates[[1]][1],ship1$coordinates[[1]][2],2]


## The board fields can have following values:

## 6. Empty_free - ship can be placed here. Returns miss if shot. Displays as empty
## 0. Empty_reserved - ship cannot be placed here, because it is adjacent to another ship. Returns miss if shot. Displays as empty
## 1. Ship_hidden - Ship cannot be placed here, because there is ship here. Returns hit if shot. Displays as empty
## 2. Ship_hit - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as hit
## 3. Ship_destroyed - Ship cannot be placed here, because there is ship here (also it appeard during the game). Cannot be shot. Displays as destroyed
## 4. Miss - Ship cannot be placed here, it appears during the game. Cannot be shot. Displays as miss

## This function will populate the board based on semi-random algorithm  


populate_board<-function(qn)
{
  
  
  obj_board<-new_board()
  
  ### create ships to be placed on board
  #ship1$coordinates[[1]][1]<-5
  #en$ship11<-create_ship(1,11)
  #en$ship12<-create_ship(1,12)
  #en$ship13<-create_ship(1,13)
  #en$ship14<-create_ship(1,14)
    
  #en$ship21<-create_ship(2,21)
  #en$ship22<-create_ship(2,22)
  #en$ship23<-create_ship(2,23)
  
  #en$ship31<-create_ship(3,31)
  #en$ship32<-create_ship(3,32)
  
  #en$ship41<-create_ship(4,41)

  ship11<-create_ship(1,11)
  ship12<-create_ship(1,12)
  ship13<-create_ship(1,13)
  ship14<-create_ship(1,14)
  
  ship21<-create_ship(2,21)
  ship22<-create_ship(2,22)
  ship23<-create_ship(2,23)
  
  ship31<-create_ship(3,31)
  ship32<-create_ship(3,32)
  
  ship41<-create_ship(4,41)
  ## safety function iterator
  
  u=1
  
  ## list of all possible coordinates from (1,1) to (1,100). Generated in Excel.
  
  coor_list<-list(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(1,7),c(1,8),c(1,9),c(1,10),c(2,1),c(2,2),c(2,3),c(2,4),c(2,5),c(2,6),c(2,7),c(2,8),c(2,9),c(2,10),c(3,1),c(3,2),c(3,3),c(3,4),c(3,5),c(3,6),c(3,7),c(3,8),c(3,9),c(3,10),c(4,1),c(4,2),c(4,3),c(4,4),c(4,5),c(4,6),c(4,7),c(4,8),c(4,9),c(4,10),c(5,1),c(5,2),c(5,3),c(5,4),c(5,5),c(5,6),c(5,7),c(5,8),c(5,9),c(5,10),c(6,1),c(6,2),c(6,3),c(6,4),c(6,5),c(6,6),c(6,7),c(6,8),c(6,9),c(6,10),c(7,1),c(7,2),c(7,3),c(7,4),c(7,5),c(7,6),c(7,7),c(7,8),c(7,9),c(7,10),c(8,1),c(8,2),c(8,3),c(8,4),c(8,5),c(8,6),c(8,7),c(8,8),c(8,9),c(8,10),c(9,1),c(9,2),c(9,3),c(9,4),c(9,5),c(9,6),c(9,7),c(9,8),c(9,9),c(9,10),c(10,1),c(10,2),c(10,3),c(10,4),c(10,5),c(10,6),c(10,7),c(10,8),c(10,9),c(10,10))
  
  ## Group ships into list
  
  ship_list<-list("ship41"=ship41,"ship32"=ship32,"ship31"=ship31,"ship23"=ship23,"ship22"=ship22,"ship21"=ship21,"ship14"=ship14,"ship13"=ship13,"ship12"=ship12,"ship11"=ship11)
  #en$ship_list<-list(ship41,ship32,ship31,ship23,ship22,ship21,ship14,ship13,ship12,ship11)
  ## Possible vertical/horizontal values
  vert_list<-c(TRUE,FALSE)
  
  ## Counter for debbuging
  
  i=1
  
  ## Iterate through all ships on the list
  
  for (ship in ship_list){
    #message(as.character(i))
    #message(ship)
    
    # Randomly select coordinates and vertical/horizontal value
    
    vrt<-sample(vert_list,1)
    crd<-sample(coor_list,1)[[1]]
    
    #message(check_if_free(obj_board,ship,crd,vrt))
    
    
    # Change the randomly selected coordinates and orientation until the place to fit the ship is found
    #message(check_if_free(obj_board,ship,crd,vrt))
    while (check_if_free(obj_board,ship,crd,vrt)==FALSE) {
      vrt<-sample(vert_list,1)
      crd<-sample(coor_list,1)[[1]]
      #message(check_if_free(obj_board,ship,crd,vrt))
    
      
      u=u+1
      #message(paste0("u: ", u))
      
      ## safeguard for the case that too many iterations occur
      
      if (u>1000){  
        
        
        break
        
      }
      
      
    }
    
    ## when coordinates and orientation is found place the ship there
    
      #message(check_if_free(obj_board,ship,crd,vrt),crd,vrt)
     placing<-place_ship_on_board(obj_board,ship,crd,vrt)
     obj_board<-placing[[1]]
     #message(names(ship_list)[i])
     #message(placing[[2]])

     assign(paste0(names(ship_list)[i]),placing[[2]],envir = qn)
     i=i+1
  }
  
  
  return(obj_board)

  }







# This function will check if the field can be shot and shoot it, if possible
shoot<-function(obj_board,vect_spot)
{
      if (!obj_board[vect_spot[1],vect_spot[2],2] %in% c(2,3,4)){
    
            if (obj_board[vect_spot[1],vect_spot[2],2] %in% c(6,0)){
                  return("MISS")
            }
    
            if (obj_board[vect_spot[1],vect_spot[2],2]==1){
                  return("HIT")
            }
      }
  
  
        return("NA")
  
}



where("id"==14)



game<-function(board1,board2){
  
  
      turn_ind=1
  
      coor_list<-list(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(1,6),c(1,7),c(1,8),c(1,9),c(1,10),c(2,1),c(2,2),c(2,3),c(2,4),c(2,5),c(2,6),c(2,7),c(2,8),c(2,9),c(2,10),c(3,1),c(3,2),c(3,3),c(3,4),c(3,5),c(3,6),c(3,7),c(3,8),c(3,9),c(3,10),c(4,1),c(4,2),c(4,3),c(4,4),c(4,5),c(4,6),c(4,7),c(4,8),c(4,9),c(4,10),c(5,1),c(5,2),c(5,3),c(5,4),c(5,5),c(5,6),c(5,7),c(5,8),c(5,9),c(5,10),c(6,1),c(6,2),c(6,3),c(6,4),c(6,5),c(6,6),c(6,7),c(6,8),c(6,9),c(6,10),c(7,1),c(7,2),c(7,3),c(7,4),c(7,5),c(7,6),c(7,7),c(7,8),c(7,9),c(7,10),c(8,1),c(8,2),c(8,3),c(8,4),c(8,5),c(8,6),c(8,7),c(8,8),c(8,9),c(8,10),c(9,1),c(9,2),c(9,3),c(9,4),c(9,5),c(9,6),c(9,7),c(9,8),c(9,9),c(9,10),c(10,1),c(10,2),c(10,3),c(10,4),c(10,5),c(10,6),c(10,7),c(10,8),c(10,9),c(10,10))
  
      while ((sum(board1[,,2]==4)!=20)&& (sum(board2[,,2]==4)!=20)){
    
    
            if(turn_ind==1){
      
            ## PLAYER TURN ##
      
            ## Ask for coordinates ##
      
            coords_y<-readline(prompt="Insert Coordinate y: ")
            coords_x<-readline(prompt="Insert Coordinate x: ")
      
            coords_x<-as.integer(coords_x)
            coords_y<-as.integer(coords_y)
      
            coords=c(coords_y,coords_x)
      
            ## Shoot the coordinates and return result ##
      
            result<-shoot(board1,coords)
      
            message(result)
      
            ## if missed mark it on visible board and hidden board
      
            if (result=="MISS"){
        
                  en$board1[coords_y,coords_x,2]=4
                  en$board1[coords_y,coords_x,1]=4
                  board1<-en$board1
            }
      
     
            else { 
        
            ## if hit mark it on visible board and hidden board also perform check if ship is sunk  
        
            if(result=="HIT"){
          
                  en$board1[coords_y,coords_x,2]=2
                  en$board1[coords_y,coords_x,1]=2
          
                  ## return id of hit ship
          
                  ship_id<-en$board1[coords_y,coords_x,3]
              
                  ## create temporary ship to make changes to

                  ship_temp<-get(paste0("ship",ship_id),envir = en)
              
                  ship_temp$hits<-ship_temp$hits+1
                  ship_temp$state<-"hit"
                  message(ship_temp$coordinates)
              
                  ## check if sunk
                
                  if (ship_temp$hits>=length(ship_temp)) {
                  
                        ship_temp$state<-"sunk"
                  
                        ### change board to show sunk ship
                      
                        for (i in c(1:length(ship_temp))){
                              message(ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2])
                              en$board1[ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2],2]<-3
                              en$board1[ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2],1]<-3
                        
                        }
                  
                    }
              
                  ## change ship status on en using temporary ship
                
                  assign(paste0("ship",ship_id),ship_temp,envir=en)

                  message("HIIIIT")
                  
                  
                  board1<-en$board1
            }
              
            }
        
        
            }
      
      
      }     
    
    
      if(turn_ind==(-1)){
      
      
      ### AI TURN ###
      ## fields available to shoot ##
      
      
      
      }
  message("GAME")  
  return(0)
  
}

board1<-populate_board(en)
en$board1<-board1

board2<-populate_board(ai)
ai$board2<-board2


board1
game(board1,board2)
en$board1
SHIP55<-create_ship(3,55)

SHIP55$coordinates[1]
