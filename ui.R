
library(shiny)
library(shinydashboard)
library(latex2exp)
library(plotly)
library(shinyjs)

fluidPage(
    shinyjs::useShinyjs(),
    sliderInput(inputId = "eixox",label="eixo x",value = c(-3,3),min=-100,max=100,step = 0.01),
    checkboxInput(inputId = "eixoxauto",label= "ajustar eixo x automaticamente",value = TRUE),
    sliderInput(inputId = "eixoy",label="eixo y",value = 1,min=0,max=3,step = 0.01),
    checkboxInput(inputId = "eixoyauto",label= "ajustar eixo y automaticamente",value = TRUE),
    titlePanel(""),
    navlistPanel(
        "Distribuição",
        tabPanel("Normal",
                 h3("Densidade da distribuição Normal"),    
                 sliderInput(inputId = "mu",label="\u03BC",value=0,min=-10,max=10,step = 0.01),
                 sliderInput(inputId = "sigma",label="\u03C3",value=1,min=0.001,max=10,step = 0.01),
                 plotlyOutput("plotNormal")
        ),        
        tabPanel("t-Student",
                 h3("Densidade da distribuição t de Student"),    
                 sliderInput(inputId = "grauliberdade",label="grau de liberdade",value=1,min=1,max=1000,step = 1),
                 plotlyOutput("plot_t_student")
        ),
        tabPanel("Qui-quadrado",
                 h3("Densidade da distribuição Qui-quadrado"),    
                 sliderInput(inputId = "quigrauliberdade",label="grau de liberdade",value=1,min=1,max=100,step = 1),
                 plotlyOutput("plot_qui_quadrado")
        ),
        tabPanel("Gama",
                 h3("Densidade da distribuição Gama"),    
                 sliderInput(inputId = "alpha",label="\u03B1",value=1,min=0.001,max=10,step = 0.01),
                 sliderInput(inputId = "beta",label="\u03B2",value=1,min=0.001,max=10,step = 0.01),
                 plotlyOutput("plotgama")
        ),
        tabPanel("Weibull",
                 h3("Densidade da distribuição Weibull"),    
                 sliderInput(inputId = "scale",label="Parâmetro de escala (\u03BB)",value=1,min=0.001,max=25,step = 0.01),
                 sliderInput(inputId = "shape",label="Parâmetro de forma (k)",value=1,min=0.001,max=25,step = 0.01),
                 plotlyOutput("plotweibull")
        ),
        tabPanel("Binomial",
                 h3("Função massa da distribuição Binomial"),    
                 sliderInput(inputId = "n",label="n",value=1,min=1,max=100,step = 1),
                 sliderInput(inputId = "p",label="p",value=0.5,min=0,max=1,step = 0.001),
                 plotlyOutput("plotbinomial")
        ),
        tabPanel("poisson",
                 h3("Função massa da distribuição Poisson"),    
                 sliderInput(inputId = "lambda",label="\u03BB",value=1,min=0.001,max=100,step = 0.001),
                 plotlyOutput("plotpoisson")
        ),
        widths = c(3, 8)
    )
)
