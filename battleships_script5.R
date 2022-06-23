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
  
  coor_list<-list("1"=c(1,1),"2"=c(1,2),"3"=c(1,3),"4"=c(1,4),"5"=c(1,5),"6"=c(1,6),"7"=c(1,7),"8"=c(1,8),"9"=c(1,9),"10"=c(1,10),"11"=c(2,1),"12"=c(2,2),"13"=c(2,3),"14"=c(2,4),"15"=c(2,5),"16"=c(2,6),"17"=c(2,7),"18"=c(2,8),"19"=c(2,9),"20"=c(2,10),"21"=c(3,1),"22"=c(3,2),"23"=c(3,3),"24"=c(3,4),"25"=c(3,5),"26"=c(3,6),"27"=c(3,7),"28"=c(3,8),"29"=c(3,9),"30"=c(3,10),"31"=c(4,1),"32"=c(4,2),"33"=c(4,3),"34"=c(4,4),"35"=c(4,5),"36"=c(4,6),"37"=c(4,7),"38"=c(4,8),"39"=c(4,9),"40"=c(4,10),"41"=c(5,1),"42"=c(5,2),"43"=c(5,3),"44"=c(5,4),"45"=c(5,5),"46"=c(5,6),"47"=c(5,7),"48"=c(5,8),"49"=c(5,9),"50"=c(5,10),"51"=c(6,1),"52"=c(6,2),"53"=c(6,3),"54"=c(6,4),"55"=c(6,5),"56"=c(6,6),"57"=c(6,7),"58"=c(6,8),"59"=c(6,9),"60"=c(6,10),"61"=c(7,1),"62"=c(7,2),"63"=c(7,3),"64"=c(7,4),"65"=c(7,5),"66"=c(7,6),"67"=c(7,7),"68"=c(7,8),"69"=c(7,9),"70"=c(7,10),"71"=c(8,1),"72"=c(8,2),"73"=c(8,3),"74"=c(8,4),"75"=c(8,5),"76"=c(8,6),"77"=c(8,7),"78"=c(8,8),"79"=c(8,9),"80"=c(8,10),"81"=c(9,1),"82"=c(9,2),"83"=c(9,3),"84"=c(9,4),"85"=c(9,5),"86"=c(9,6),"87"=c(9,7),"88"=c(9,8),"89"=c(9,9),"90"=c(9,10),"91"=c(10,1),"92"=c(10,2),"93"=c(10,3),"94"=c(10,4),"95"=c(10,5),"96"=c(10,6),"97"=c(10,7),"98"=c(10,8),"99"=c(10,9),"100"=c(10,10))
  name<-names(sample(coor_list,1))
  coor_list<-coor_list[names(coor_list)!=name]
  coor_list[names(coor_list)=="2"]
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


shoot_player<-function(vect_spot)
{
  if (!en$board1[vect_spot[1],vect_spot[2],2] %in% c(2,3,4)){
    
    if (en$board1[vect_spot[1],vect_spot[2],2] %in% c(6,0)){
      return("MISS")
    }
    
    if (en$board1[vect_spot[1],vect_spot[2],2]==1){
      return("HIT")
    }
  }
  
  
  return("NA")
  
}



player_turn<-function(coords_y,coords_x){
      turn_ind=1
      
      en$player_shots<-0
      ai$ai_shots<-0
      en$player_hits<-0
      ai$ai_hits<-0
      
      
      coor_list<-list("1"=c(1,1),"2"=c(1,2),"3"=c(1,3),"4"=c(1,4),"5"=c(1,5),"6"=c(1,6),"7"=c(1,7),"8"=c(1,8),"9"=c(1,9),"10"=c(1,10),"11"=c(2,1),"12"=c(2,2),"13"=c(2,3),"14"=c(2,4),"15"=c(2,5),"16"=c(2,6),"17"=c(2,7),"18"=c(2,8),"19"=c(2,9),"20"=c(2,10),"21"=c(3,1),"22"=c(3,2),"23"=c(3,3),"24"=c(3,4),"25"=c(3,5),"26"=c(3,6),"27"=c(3,7),"28"=c(3,8),"29"=c(3,9),"30"=c(3,10),"31"=c(4,1),"32"=c(4,2),"33"=c(4,3),"34"=c(4,4),"35"=c(4,5),"36"=c(4,6),"37"=c(4,7),"38"=c(4,8),"39"=c(4,9),"40"=c(4,10),"41"=c(5,1),"42"=c(5,2),"43"=c(5,3),"44"=c(5,4),"45"=c(5,5),"46"=c(5,6),"47"=c(5,7),"48"=c(5,8),"49"=c(5,9),"50"=c(5,10),"51"=c(6,1),"52"=c(6,2),"53"=c(6,3),"54"=c(6,4),"55"=c(6,5),"56"=c(6,6),"57"=c(6,7),"58"=c(6,8),"59"=c(6,9),"60"=c(6,10),"61"=c(7,1),"62"=c(7,2),"63"=c(7,3),"64"=c(7,4),"65"=c(7,5),"66"=c(7,6),"67"=c(7,7),"68"=c(7,8),"69"=c(7,9),"70"=c(7,10),"71"=c(8,1),"72"=c(8,2),"73"=c(8,3),"74"=c(8,4),"75"=c(8,5),"76"=c(8,6),"77"=c(8,7),"78"=c(8,8),"79"=c(8,9),"80"=c(8,10),"81"=c(9,1),"82"=c(9,2),"83"=c(9,3),"84"=c(9,4),"85"=c(9,5),"86"=c(9,6),"87"=c(9,7),"88"=c(9,8),"89"=c(9,9),"90"=c(9,10),"91"=c(10,1),"92"=c(10,2),"93"=c(10,3),"94"=c(10,4),"95"=c(10,5),"96"=c(10,6),"97"=c(10,7),"98"=c(10,8),"99"=c(10,9),"100"=c(10,10))
  
            ## PLAYER TURN ##
      
            ## Ask for coordinates ##
            #correct_input=FALSE
            #while (correct_input==FALSE){  
                #coords_y<-readline(prompt="Insert Coordinate y: ")
                #coords_x<-readline(prompt="Insert Coordinate x: ")
                
                
                ## Defensive programming against incorrect input
                ## If user types characters coordinates change to 55
                #if(is.na(as.integer(coords_x)) | is.na(as.integer(coords_y))){
                  
                  #coords_x=55
                  #coords_y=55
                  
                  
                #}
                
                ## If coordinates are outside (1,10) range the message appears and program waits for proper input
                #if (as.integer(coords_x)>=1 && as.integer(coords_x)<=10 && as.integer(coords_y)>=1 && as.integer(coords_y)<=10){
                #      correct_input=TRUE
                #}else{
                #      message("Please provide correct input")
                #}
                
            #}
            coords_x<-as.integer(coords_x)
            coords_y<-as.integer(coords_y)
      
            coords=c(coords_y,coords_x)
      
            ## Shoot the coordinates and return result ##
      
            result<-shoot_player(coords)
      
            message(result)
      
            ## if missed mark it on visible board and hidden board
      
            if (result=="MISS"){
                  en$player_shots=en$player_shots+1
                  
                  en$board1[coords_y,coords_x,2]=4
                  en$board1[coords_y,coords_x,1]=4
                  board1<-en$board1
                  
                  # change player if miss
                  turn_ind=turn_ind*(-1)
                  
            }
      
     
            else { 
        
            ## if hit mark it on visible board and hidden board also perform check if ship is sunk  
        
                  if(result=="HIT"){
                        en$player_shots=en$player_shots+1
                        en$player_hits=en$player_hits+1
                        
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

                  #message("HIIIIT")
                  
                  
                  board1<-en$board1
                  }
              
            }
        
        
            } # PLAYER TURN CODE ENDS HERE
      
        
shoot_ai<-function(vect_spot)
{
  if (!ai$board2[vect_spot[1],vect_spot[2],2] %in% c(2,3,4)){
    
    if (ai$board2[vect_spot[1],vect_spot[2],2] %in% c(6,0)){
      return("MISS")
    }
    
    if (ai$board2[vect_spot[1],vect_spot[2],2]==1){
      return("HIT")
    }
  }
  
  
  return("NA")
  
}



ai_turn<-function(){
  na_counter<-0
  saved_coords<-FALSE
  turn_ind=-1
  
  en$player_shots<-0
  ai$ai_shots<-0
  en$player_hits<-0
  ai$ai_hits<-0
  
  
  coor_list<-list("1"=c(1,1),"2"=c(1,2),"3"=c(1,3),"4"=c(1,4),"5"=c(1,5),"6"=c(1,6),"7"=c(1,7),"8"=c(1,8),"9"=c(1,9),"10"=c(1,10),"11"=c(2,1),"12"=c(2,2),"13"=c(2,3),"14"=c(2,4),"15"=c(2,5),"16"=c(2,6),"17"=c(2,7),"18"=c(2,8),"19"=c(2,9),"20"=c(2,10),"21"=c(3,1),"22"=c(3,2),"23"=c(3,3),"24"=c(3,4),"25"=c(3,5),"26"=c(3,6),"27"=c(3,7),"28"=c(3,8),"29"=c(3,9),"30"=c(3,10),"31"=c(4,1),"32"=c(4,2),"33"=c(4,3),"34"=c(4,4),"35"=c(4,5),"36"=c(4,6),"37"=c(4,7),"38"=c(4,8),"39"=c(4,9),"40"=c(4,10),"41"=c(5,1),"42"=c(5,2),"43"=c(5,3),"44"=c(5,4),"45"=c(5,5),"46"=c(5,6),"47"=c(5,7),"48"=c(5,8),"49"=c(5,9),"50"=c(5,10),"51"=c(6,1),"52"=c(6,2),"53"=c(6,3),"54"=c(6,4),"55"=c(6,5),"56"=c(6,6),"57"=c(6,7),"58"=c(6,8),"59"=c(6,9),"60"=c(6,10),"61"=c(7,1),"62"=c(7,2),"63"=c(7,3),"64"=c(7,4),"65"=c(7,5),"66"=c(7,6),"67"=c(7,7),"68"=c(7,8),"69"=c(7,9),"70"=c(7,10),"71"=c(8,1),"72"=c(8,2),"73"=c(8,3),"74"=c(8,4),"75"=c(8,5),"76"=c(8,6),"77"=c(8,7),"78"=c(8,8),"79"=c(8,9),"80"=c(8,10),"81"=c(9,1),"82"=c(9,2),"83"=c(9,3),"84"=c(9,4),"85"=c(9,5),"86"=c(9,6),"87"=c(9,7),"88"=c(9,8),"89"=c(9,9),"90"=c(9,10),"91"=c(10,1),"92"=c(10,2),"93"=c(10,3),"94"=c(10,4),"95"=c(10,5),"96"=c(10,6),"97"=c(10,7),"98"=c(10,8),"99"=c(10,9),"100"=c(10,10))
  
  while (turn_ind==(-1)){
    
    
    
    
    ### AI TURN ###
    
    
    ## Get coordinates ##
    ## if previous shoot was a hit with no sinking the AI will try field next to the previous one
    if (saved_coords==TRUE){
      message("saved coords are true")
      coor_mod<-list(c(0,-1),c(0,1),c(-1,0),c(1,0))
      
      if(coords[1]==10 && coords[2]==10){
        message("corner 10 10")  
        coor_mod<-list(c(0,-1),c(-1,0))
        
      }else{
        
        if(coords[1]==1 && coords[2]==1){
          message("corner 1 1")
          coor_mod<-list(c(0,1),c(1,0))
        }else{
          
          if(coords[1]==10 && coords[2]==1){
            message("corner 10 1")
            coor_mod<-list(c(0,1),c(-1,0))
            
          }else{
            
            if(coords[1]==1 && coords[2]==10){
              message("corner 1 10")
              coor_mod<-list(c(0,-1),c(1,0))
              
            }else{
              
              if(coords[1]==10){
                message("bottom line")
                coor_mod<-list(c(0,-1),c(-1,0),c(0,1))  
                
              }else{
                
                if(coords[1]==1){
                  message("top line")
                  coor_mod<-list(c(0,-1),c(1,0),c(0,1))
                  
                }else{
                  if(coords[2]==10){
                    message("right line")
                    coor_mod<-list(c(0,-1),c(1,0),c(-1,0))
                    
                  }else{
                    
                    if (coords[2]==1){
                      message("left line")
                      coor_mod<-list(c(0,1),c(1,0),c(-1,0))
                      
                      
                      
                    }
                  }
                }
              }
            }
          }  
          
        }  
        
      }
      
      
      
      
      
      coor_add<-sample(coor_mod,1)[[1]]
      message("adding to coordinates:", coor_add)
      coords<-coords+coor_add
      message("new coordinates",coords)
      coords_y=coords[1]
      coords_x=coords[2]
      #saved_coords<-FALSE
      
    }
    else{
      
      ## take random name from list 
      message("saved coords FALSE")
      name<-names(sample(coor_list,1))
      
      ## find target coordinates
      
      target<-coor_list[names(coor_list)==name]
      
      
      ## remove used field
      coor_list<-coor_list[names(coor_list)!=name]
      
      coords=target[[1]]
      message("new coords", coords)
      coords_y=coords[1]
      coords_x=coords[2]
      
    }
    
    
    ## Shoot the coordinates and return result ##
    
    result<-shoot_ai(coords)
    message("AI shoots",coords)
    message(result)
    
    ## if missed mark it on visible board and hidden board
    
    if (result=="MISS"){
      message("coords are:", coords_y, coords_x, " setting to 4")
      ai$board2[coords_y,coords_x,2]=4
      ai$board2[coords_y,coords_x,1]=4
      board2<-ai$board2
      message("shot at", coords_y,coords_x)
      if (saved_coords==TRUE){
        coords<-coords-coor_add
        message("returning the coords", coords, " by ", coor_add)
      }
      turn_ind=turn_ind*(-1)
      
    }
    
    
    else { 
      
      ## if hit mark it on visible board and hidden board also perform check if ship is sunk  
      
      if(result=="HIT"){
        saved_coords<-TRUE
        ai$board2[coords_y,coords_x,2]=2
        ai$board2[coords_y,coords_x,1]=2
        message("coords are:", coords_y, coords_x," set to 2")
        ## return id of hit ship
        message(coords_y,coords_x)
        ship_id<-ai$board2[coords_y,coords_x,3]
        
        ## create temporary ship to make changes to
        
        ship_temp<-get(paste0("ship",ship_id),envir = ai)
        
        ship_temp$hits<-ship_temp$hits+1
        ship_temp$state<-"hit"
        #message(ship_temp$coordinates)
        
        ## check if sunk
        
        if (ship_temp$hits>=length(ship_temp)) {
          
          ship_temp$state<-"sunk"
          
          ### change board to show sunk ship
          
          for (i in c(1:length(ship_temp))){
            message(ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2])
            ai$board2[ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2],2]<-3
            ai$board2[ship_temp$coordinates[[i]][1],ship_temp$coordinates[[i]][2],1]<-3
            
          }
          
          
          saved_coords<-FALSE
          message("Sunk")
        }
        
        ## change ship status on en using temporary ship
        
        assign(paste0("ship",ship_id),ship_temp,envir=ai)
        
        #message("HIT")
        
        
        board2<-ai$board2
        
      }
      
      else { 
        
        
        if(result=="NA"){
          if (saved_coords==TRUE){
            
            if(na_counter<=10){
              na_counter=na_counter+1
              coords<-coords-coor_add
              message("reverting coords back to", coords)
            }else{
              message("NA limit seting saved coords to FALSE")
              saved_coords=FALSE
              na_counter<-0
            }
          }
          
        }
        
      }  
      
      
    } # AI TURN CODE ENDS HERE
    
    
  }# while loop ends here
  
}      
           

board1<-populate_board(en)
en$board1<-board1


en$board1

board2<-populate_board(ai)
ai$board2<-board2


game(board1,board2)

coor_list<-list("1"=c(1,1),"2"=c(1,2),"3"=c(1,3),"4"=c(1,4),"5"=c(1,5),"6"=c(1,6),"7"=c(1,7),"8"=c(1,8),"9"=c(1,9),"10"=c(1,10),"11"=c(2,1),"12"=c(2,2),"13"=c(2,3),"14"=c(2,4),"15"=c(2,5),"16"=c(2,6),"17"=c(2,7),"18"=c(2,8),"19"=c(2,9),"20"=c(2,10),"21"=c(3,1),"22"=c(3,2),"23"=c(3,3),"24"=c(3,4),"25"=c(3,5),"26"=c(3,6),"27"=c(3,7),"28"=c(3,8),"29"=c(3,9),"30"=c(3,10),"31"=c(4,1),"32"=c(4,2),"33"=c(4,3),"34"=c(4,4),"35"=c(4,5),"36"=c(4,6),"37"=c(4,7),"38"=c(4,8),"39"=c(4,9),"40"=c(4,10),"41"=c(5,1),"42"=c(5,2),"43"=c(5,3),"44"=c(5,4),"45"=c(5,5),"46"=c(5,6),"47"=c(5,7),"48"=c(5,8),"49"=c(5,9),"50"=c(5,10),"51"=c(6,1),"52"=c(6,2),"53"=c(6,3),"54"=c(6,4),"55"=c(6,5),"56"=c(6,6),"57"=c(6,7),"58"=c(6,8),"59"=c(6,9),"60"=c(6,10),"61"=c(7,1),"62"=c(7,2),"63"=c(7,3),"64"=c(7,4),"65"=c(7,5),"66"=c(7,6),"67"=c(7,7),"68"=c(7,8),"69"=c(7,9),"70"=c(7,10),"71"=c(8,1),"72"=c(8,2),"73"=c(8,3),"74"=c(8,4),"75"=c(8,5),"76"=c(8,6),"77"=c(8,7),"78"=c(8,8),"79"=c(8,9),"80"=c(8,10),"81"=c(9,1),"82"=c(9,2),"83"=c(9,3),"84"=c(9,4),"85"=c(9,5),"86"=c(9,6),"87"=c(9,7),"88"=c(9,8),"89"=c(9,9),"90"=c(9,10),"91"=c(10,1),"92"=c(10,2),"93"=c(10,3),"94"=c(10,4),"95"=c(10,5),"96"=c(10,6),"97"=c(10,7),"98"=c(10,8),"99"=c(10,9),"100"=c(10,10))


