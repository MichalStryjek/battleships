library(shiny)

board1<-populate_board(en)
en$board1<-board1


board2<-populate_board(ai)
ai$board2<-board2



ui <- fluidPage(
  titlePanel("You"),
      plotOutput("boxPlot", click = "boxPlot_click"),
  titlePanel("AI"),
      plotOutput("boxPlot2", click = "boxPlot_click2")
    
  
)
server <- function(input, output) {
  
  vals <- reactiveValues(x=NA,y=NA,i=0)
  
  observeEvent(input$boxPlot_click2, {
    vals$x <- c(input$boxPlot_click2$x)
    vals$y <- c(input$boxPlot_click2$y)
    vals$i<-1
  })
  
  
  
  output$boxPlot <- renderPlot({
    input$boxPlot_click2
    
    cor_x=floor(vals$y-1)
    cor_y=floor(vals$x-1)
    

    
    par(mai=c(0,0,0,0))
    plot(1,ylim=c(2,12),xlim=c(2,12),type='n',yaxs='i',xaxs='i',ylab='',xlab='',axes=F)
    for (i in 2:12) {
      abline(v=i)
      abline(h=i)
    }
    for (cor in coor_list) {
      colr="blue"
      if(ai$board2[cor[1],cor[2],2]==0){
        
        colr="blue"
        
      }
      if(ai$board2[cor[1],cor[2],2]==1){
        
        colr="brown"
        
      }
      
      if(ai$board2[cor[1],cor[2],2]==2){
        
        colr="red"
        
      }
      
      if(ai$board2[cor[1],cor[2],2]==3){
        
        colr="black"
        
      }
      
      
      if(ai$board2[cor[1],cor[2],2]==4){
        
        colr="green"
        
      }
      
      
      
      rect(floor(cor[1]+1.5),floor(cor[2]+1.5),ceiling(cor[1]+1.5),ceiling(cor[2]+1.5),col=colr)
      
      
    }
    
      if(vals$i>0){
        player_turn(cor_y,cor_x)

      }
    
    
  })
  
  
  
  
  
  
  output$boxPlot2 <- renderPlot({
    input$boxPlot_click2
    par(mai=c(0,0,0,0))
    plot(1,ylim=c(2,12),xlim=c(2,12),type='n',yaxs='i',xaxs='i',ylab='',xlab='',axes=F)
    for (i in 2:12) {
      abline(v=i)
      abline(h=i)
    }
    for (cor in coor_list) {
      colr="blue"
      if(en$board1[cor[1],cor[2],1]==0){
        
        colr="blue"
        
      }
      if(en$board1[cor[1],cor[2],1]==1){
        
        colr="brown"
        
      }
      
      if(en$board1[cor[1],cor[2],1]==2){
        
        colr="red"
        
      }
      
      if(en$board1[cor[1],cor[2],1]==3){
        
        colr="black"
        
      }
      
      
      if(en$board1[cor[1],cor[2],1]==4){
        
        colr="green"
        
      }
      
      
      
      rect(floor(cor[1]+1.5),floor(cor[2]+1.5),ceiling(cor[1]+1.5),ceiling(cor[2]+1.5),col=colr)
    }
  })
  

  
  
  output$text <- renderText(paste0(vals$x, ', ' , vals$y, '\n'))
 
  
  

  
   
}









shinyApp(ui = ui, server = server)