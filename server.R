library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)


contador = TRUE

function(input, output,session) {
    
    observeEvent(input$eixoxauto, ({
        if(input$eixoxauto){
            shinyjs::disable("eixox")
        } else {
            shinyjs::enable("eixox")
            }
        }))
    observeEvent(input$eixoyauto, ({
        if(input$eixoyauto){
            shinyjs::disable("eixoy")
        } else {
            shinyjs::enable("eixoy")
        }
    }))

    output$plotNormal <- renderPlotly({
        if(!contador){
          updateSliderInput(session,"eixox",value = c(-3,3))
          contador <<- TRUE
        }
        x <- seq(input$mu - 50*input$sigma,input$mu + 50*input$sigma,0.01)
        p <- plot_ly(x = x, y = dnorm(x,input$mu, input$sigma), type = 'scatter', mode = 'lines', fill = 'tozeroy')
        
        if(input$eixoxauto & input$eixoyauto){
            p%>%
                layout(xaxis = list(range = c(input$mu - 3*input$sigma,input$mu + 3*input$sigma)))
        }else if(!input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])))
        }else if(input$eixoxauto & !input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(input$mu - 3*input$sigma,input$mu + 3*input$sigma)),
                       yaxis = list(range = c(0,input$eixoy)))
        }else{
            p %>% layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                   yaxis = list(range = c(0,input$eixoy)))
        }

    })
    
    

    output$plot_t_student <- renderPlotly({
        if(!contador){
        updateSliderInput(session,"eixox",value = c(-3,3))
        contador <<- TRUE
        }
        x <- seq(0 - 100,0 + 100,0.01)
        p <- plot_ly(x = x, y = dt(x,df = input$grauliberdade), type = 'scatter', mode = 'lines', fill = 'tozeroy') 
 
        if(input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(0 - 5,0 + 5)))
        }else if(!input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])))
        }else if(input$eixoxauto & !input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(0 - 5,0 + 5)),
                       yaxis = list(range = c(0,input$eixoy)))
        }else{
            p %>% 
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                         yaxis = list(range = c(0,input$eixoy)))
        }
        
    })

    output$plot_qui_quadrado <- renderPlotly({
      if(!contador){
        updateSliderInput(session,"eixox",value = c(0,5))
        contador <<- FALSE
      }
      x <- seq(0 ,0 + qchisq(0.9999,input$quigrauliberdade),0.01)
      p <- plot_ly(x = x, y = dchisq(x,df = input$quigrauliberdade), type = 'scatter', mode = 'lines', fill = 'tozeroy') 

      
      if(input$eixoxauto & input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(0 ,0 + qchisq(0.999,input$quigrauliberdade))))
      }else if(!input$eixoxauto & input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])))
      }else if(input$eixoxauto & !input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(0 ,0 + qchisq(0.999,input$quigrauliberdade))),
                 yaxis = list(range = c(0,input$eixoy)))
      }else{
        p %>% 
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                 yaxis = list(range = c(0,input$eixoy)))
      }
      
    })
    
    
    
        
    output$plotgama <- renderPlotly({
        if(contador){
          updateSliderInput(session,"eixox",value = c(0,5))
          contador <<- FALSE
        }
        x <- seq(0,qgamma(0.9999,shape = input$alpha,rate = input$beta),0.01)
        p <- plot_ly(x = x, y = dgamma(x,shape = input$alpha, rate = input$beta), type = 'scatter', mode = 'lines', fill = 'tozeroy') 
        if(input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(0 ,5*input$alpha/input$beta)))
        }else if(!input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])))
        }else if(input$eixoxauto & !input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(0 ,5*input$alpha/input$beta)),
                       yaxis = list(range = c(0,input$eixoy)))
        }else{
            p %>% 
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                       yaxis = list(range = c(0,input$eixoy)))
        }
        
    
    })
      

    output$plotweibull <- renderPlotly({
        if(contador){
          updateSliderInput(session,"eixox",value = c(0,5))
          contador <<- FALSE
        }
        x <- seq(0,qweibull(0.999,shape = input$shape, scale = input$scale) + 2,0.01)
        media <- input$shape*gamma(1 + 1/input$scale)
        variancia <- input$shape^2*(gamma(1 + 2/input$scale) - (gamma(1 + 1/input$scale))^2)
        p <- plot_ly(x = x, y = dweibull(x,shape = input$shape, scale = input$scale), type = 'scatter', mode = 'lines', fill = 'tozeroy') 
        if(input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(max(c(0-0.1,qweibull(0.001,shape = input$shape, scale = input$scale) -0.5)),qweibull(0.999,shape = input$shape, scale = input$scale) + 0.2)))
        }else if(!input$eixoxauto & input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])))
        }else if(input$eixoxauto & !input$eixoyauto){
            p %>%
                layout(xaxis = list(range = c(min(c(0-0.1,media - 5*variancia)) ,media + 5*variancia)),
                       yaxis = list(range = c(0,input$eixoy)))
        }else{
            p %>% 
                layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                       yaxis = list(range = c(0,input$eixoy)))
        }
        
        
    })
    
    
    output$plotbinomial <- renderPlotly({
      if(contador){
        updateSliderInput(session,"eixox",value = c(0,5))
        contador <<- FALSE
      }
      x <- seq(0,input$n,1)
      y <-  dbinom(x,size = input$n,prob = input$p)
      moda <- max(y)
      p <- plot_ly(x = x,y=y,marker = list(size = 4)) %>%
        add_segments(x = x, xend = x, y = 0, yend = y)
      if(input$eixoxauto & input$eixoyauto){
        p %>%
          layout(yaxis = list(range = c(0,min(1.1,moda +1/input$n))))
      }else if(!input$eixoxauto & input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                 yaxis = list(range = c(0,1)))
      }else if(input$eixoxauto & !input$eixoyauto){
        p %>%
          layout(yaxis = list(range = c(0,input$eixoy)))
      }else{
        p %>% 
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                 yaxis = list(range = c(0,min(1.1,moda +1/input$n))))
      }
      
      
    })
    
    
    output$plotpoisson <- renderPlotly({
      if(contador){
        updateSliderInput(session,"eixox",value = c(0,5))
        contador <<- FALSE
      }
      x <- seq(0,max(10*input$lambda,3),1)
      y <-  dpois(x,lambda = input$lambda)
      moda <- max(y)
      p <- plot_ly(x = x,y=y,marker = list(size = 4)) %>%
        add_segments(x = x, xend = x, y = 0, yend = y)
      if(input$eixoxauto & input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(0,3*input$lambda)),
                 yaxis = list(range = c(0,min(1.1,moda +1/input$lambda))))
      }else if(!input$eixoxauto & input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                 yaxis = list(range = c(0,1)))
      }else if(input$eixoxauto & !input$eixoyauto){
        p %>%
          layout(xaxis = list(range = c(0,3*input$lambda)),
                 yaxis = list(range = c(0,input$eixoy)))
      }else{
        p %>% 
          layout(xaxis = list(range = c(input$eixox[1],input$eixox[2])),
                 yaxis = list(range = c(0,min(1.1,moda +1/input$lambda))))
      }
      
      
    })
    
    
}
