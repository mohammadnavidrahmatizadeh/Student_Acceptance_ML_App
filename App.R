library(shiny)

ui = fluidPage(
  
  titlePanel(tags$h3('A Machine Learning App For 
                     Predicting Student Acceptance '
                     ,align='center',
                                   style = 'color:purple;')),
                
sidebarLayout(
  
  sidebarPanel(
    
    tabsetPanel(id = 'id1',type = 'pills',
      
      tabPanel('Start',
      
       tags$br(),               
       tags$h4('What does this app do ? 
              This app is a classification App
              that gets user education data and predict user acceptance in university',
              align='center'),
       tags$br(),
       tags$h4('Created By Mohammad Navid Rahmati Zadeh',align='center'),
       tags$h4(' Senior Scientist : Dr.Nasim Barimani ',align='center'),
       tags$h4('Islamic Azad University, Shahr-e-Rey Branch',align='center')
       
       
       
               ),
               
      
      tabPanel('Data Entry',
               
           tags$br(),
           textInput('name','Enter your name'),
           numericInput('GRE','Enter Your GRE Score',value = 0,min=0,max = 600),
           numericInput('TF','Enter Your TOEFL Score',value=0,min=0,max=100),
           sliderInput('RNK','Enter your University Rank',value = 1,min=1,max=4),
           numericInput('SOP','Enter your SOP',value=0,min = 0,max=5),
           numericInput('LOR','Enter your LOR',value=0,min = 0,max=5),
           numericInput('CGPA','Enter your CGPA',value=0,min = 0,max=10),
           radioButtons('RE','Do you have Reseasrch experience ?',choices = c('yes'=1,'no'=0)),
           submitButton('Submit')         
               
               )
      )
    
    
  ),
  
  
  mainPanel(
    
    
    tabsetPanel(id = 'id2',type = 'pills',selected = 'Logistic Regression',
              
              
                
                
                tabPanel('Linear Regression',
                         
                  tags$br(),
                  tags$h4('This section uses linear regression modeling to
                        predict your admit chance as a numeric value'),
                
                  tags$br(),
                  tags$h4('Dear',textOutput('name1')),
                  tags$h4('Your chance of admit is:'),
                  tags$h4(textOutput('ln_model'))
                         
                        
                ),
                
                
                tabPanel('Logistic Regression',
                  tags$br(),
                  tags$h4('This section uses Logistic regression modeling to
                        predict your admit chance as a classified value'),
                  tags$br(),
                  tags$h4('Dear',textOutput('name2')),
                  tags$h4('Your chance of admit is:'),
                  tags$h4(textOutput('log_model'))
                         
                ),
                
              
                
                tabPanel('Neural Network',
      
          
                         tags$br(),
                         tags$h4('This section uses Neural network (MLP) to
                    predict your admit chance as a numeric value'),
                         tags$br(),
                         tags$h4('Dear',textOutput('name4')),
                         tags$h4('Your chance of admit is:'),
                         tags$h4(textOutput('nn_model'))
                         
                  
                )
                
    )
    
    
  )

)                
)


server = function(input,output,session) {
  
  output$name1 = renderText({input$name})
  output$name2 = renderText({input$name})
  output$name3 = renderText({input$name}) 
  output$name4 = renderText({input$name})

  output$ln_model = renderText({
    
  data0 = data.frame(GRE.Score=input$GRE,TOEFL.Score=input$TF,CGPA=input$CGPA)
  pre0 = predict(ln_model,data0)
  pre0 = as.numeric(pre0)
  pre0 = pre0*100
  ifelse(pre0<100&pre0>0,pre0,'')
  
  })
  
  

  output$log_model = renderText({
    
    data1 = data.frame(gre=input$GRE,gpa=input$CGPA,rank=as.factor(input$RNK))
    pre1 = predict(log_model,data1,type = 'response')
    pre1 = as.numeric(pre1)
    ifelse(pre1>0.5,print('Yes! You Accepted'),
           print('Sorry ... No chance Of Admission'))
    
    
  })
  
  
  output$svm_model = renderText({
    
    data2 = data.frame(GRE.Score =  input$GRE , TOEFL.Score = input$TF, 
                       Research = input$RE ,CGPA = input$CGPA ,
                       SOP = input$SOP ,LOR = input$LOR)
    pre2 = predict(svm_model,data2)
    pre2 = as.numeric(pre2)
    pre2 = pre2*100
    ifelse(pre2<100&pre2>0,pre2,'')
    
    
    
  })
  
  
  output$nn_model = renderText({
    
    data3 = data.frame(CGPA=input$CGPA)
    pre3 = predict(nn_model,data3)
    pre3 = pre3*100
    ifelse(pre3<100&pre3>0,pre3,'')
    
    
  })
  
  
}

source('global.R')
shinyApp(ui,server)