library(tidyverse)
library(shinycssloaders)
library(shinyWidgets)

# Source helper functions -----
source('create_objectives.R')
source('create_utility.R')
source('create_stationary.R')
source('create_nonstationary.R')

### 
# params
#define states
#states (relative density) 
K <- 1
#states <- 0:K/K
states <- seq(0,K,by=0.01)

#vector of actions: units of effort, ranging from 0 to 1
actions <- seq(0, 1, 0.01)

#loss params -- sigmoidal
sig_loss_params <- list(
  loss_a=-0.2,
  loss_b=15,
  loss_c=0.4
)
#loss params -- exponential
exp_loss_params <- list(
  loss_a=0.21,
  loss_b=3,
  loss_c=1
)


ui <- navbarPage("Resist-Accept Decision Boundary",
                 id = 'nav-page',
                 
                 theme = 'shiny.css',
                 #Home page
                 tabPanel('Home',
                          fluidPage(
                            mainPanel(fluidRow(column(10, align = 'center', offset = 1,
                                                      div(style = "text-align:center,bottom-padding: 10px",
                                                          h1('Navigating the decision boundary between ',strong('resistance'),' and ',
                                                          strong('acceptance'),' in marine invasive species management.',
                                                             style='color:#660033')))),
                                      width = 12,
                                      tags$hr(),
                                      fluidRow(column(6,
                                                    div(style = "text-align:center; border-style: solid; border-color: black;",
                                                        h2('Expressing decision-maker values',style='color:#006699'),
                                                        h3('Customize your own reward function and discounting of future rewards 
                                                           to find optimal removal efforts in a system with stationary dynamics.'))),
                                               column(6,
                                                      div(style = "text-align:center; border-style: solid; border-color: black",
                                                          h2('Accounting for non-stationarity',style='color:#006699'),
                                                          h3('Describe non-stationary system dynamics to find how accounting for non-stationarity
                                                             impacts the optimal removal effort.')))
                                      ),
                                      tags$hr(),
                                      fluidRow(column(12, align="center",
                                                      img(src='diagram_effort_colonization.png',
                                                                height = '60%',
                                                                width = '60%'))
                                      ),
                                      tags$hr()
                            )
                          )),
                 #Values page
                 tabPanel('Expressing decision-maker values',
                          h2('Expressing decision-maker values',align = 'center',style='color:#660033'),
                          tags$hr(),
                          setSliderColor(c('#660033','#660033','#660033','#660033'), 
                                         c(1,2,3,4)),
                          fluidPage(
                            fluidRow(column(12,
                                            h3('Customize your own reward function and discounting of future rewards 
                                                           to find optimal removal efforts in a system with stationary dynamics.')
                                            )),
                            tags$hr(),
                            sidebarPanel(
                              h4('1. Create your reward function.'),
                              radioButtons('eco_type',
                                           'Select an ecological change function:',
                                           choices = c('Both','Exponential','Sigmoidal'),
                                           inline = TRUE,selected = character(0)),
                              radioButtons('penalty_type',
                                           'Select a removal cost function:',
                                           choices = c('Both','Nonlinear','Linear'),
                                           inline = TRUE,selected = character(0)),
                              h5(strong('Choose a reward ratio:')),
                              actionLink('help_reward_ratio', label = 'Help'),
                              sliderInput(inputId = 'reward_ratio',
                                          label = '',
                                          min = 0.003,
                                          max = 0.075,
                                          value = 0.00375,
                                          step = 0.00025),
                              tags$hr(),
                              h4('2. Choose a discount factor.'),
                              actionLink('help_discount_factor', label = 'Help'),
                              sliderInput(inputId = 'discount_factor',
                                          label = '',
                                          min = 0.75,
                                          max = 0.99,
                                          value = 0.99,
                                          step = 0.01),
                              tags$hr(),
                              h4('3. Choose a colonization intensity.'),
                              actionLink('help_colonization_intensity', label = 'Help'),
                              selectInput(inputId = 'colonization_intensity',
                                          label = NULL,
                                          selected = '0.14',
                                          choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                      '0.14','0.69','0.99','1.00')),
                              conditionalPanel(
                                condition = "input.colonization_intensity == '0.00'",
                                tags$hr(),
                                h5('Choose growth rate (r):'),
                                selectInput(inputId = 'r',
                                            label =  '',
                                            choices = c('0','1'))
                              ),
                              actionButton(inputId = 'AB_stationary',
                                           label = 'Get stationary policy'),
                              tags$style('#AB_stationary{background-color: #660033;color:#FFFFFF;border-color:#000000;}
                                                                   #AB_stationary:hover{background-color:#006699;}')
                              ),
                            
                            mainPanel(
                              h4('Reward function components:'),
                              tags$hr(),
                              conditionalPanel(
                                condition = "(input.eco_type == 'Both' || input.eco_type == 'Exponential' || 
                                input.eco_type == 'Sigmoidal') && (input.penalty_type == 'Both' || input.penalty_type == 'Linear' || input.penalty_type == 'Nonlinear')",
                                fluidRow(
                                  #plot
                                  splitLayout(withSpinner(plotOutput('eco_plot'), type = 4,
                                                          color = '#660033'),
                                              withSpinner(plotOutput('penalty_plot'), type = 4,
                                                          color = '#660033'))),
                                tags$hr(),
                                h4('Stationary optimal removal effort:')
                              ),
                              withSpinner(plotOutput('stationary_plot'), type = 4,
                                          color = '#660033'),
                              tags$hr()
                              
                            )
                           )
                          ),
                 
                 # non-stationary page
                 tabPanel('Accounting for non-stationarity',
                          h2('Accounting for non-stationarity',align = 'center',style='color:#660033'),
                          tags$hr(),
                          
                          fluidPage(
                            fluidRow(column(12,
                                            h3('Describe non-stationary system dynamics to find how accounting for non-stationarity
                          impacts the optimal removal effort.')
                                            )),
                            tags$hr(),
                            sidebarPanel(
                              h4('1. Create your reward function.'),
                              actionLink('note', label = 'Note'),
                              radioButtons('eco_type_nonstat',
                                           'Select an ecological change function:',
                                           choices = c('Exponential','Sigmoidal'),
                                           inline = TRUE,selected = character(0)),
                              h5(strong('Choose a reward ratio:')),
                              actionLink('help_reward_ratio_nonstat', label = 'Help'),
                              sliderInput(inputId = 'reward_ratio_nonstat',
                                          label = '',
                                          min = 0.003,
                                          max = 0.075,
                                          value = 0.00375,
                                          step = 0.00025),
                              tags$hr(),
                              h4('2. Choose a discount factor.'),
                              actionLink('help_discount_factor_nonstat', label = 'Help'),
                              sliderInput(inputId = 'discount_factor_nonstat',
                                          label = '',
                                          min = 0.75,
                                          max = 0.99,
                                          value = 0.99,
                                          step = 0.01),
                              tags$hr(),
                              h4('3. Create non-stationary dynamics.'),
                              radioButtons('regime_no',
                                           'Choose the number of invasion regimes:',
                                           choices = c('2','3','4','5'),
                                           inline = TRUE,selected = character(0)),
                              # two regimes
                              conditionalPanel(
                                condition = "input.regime_no == '2'",
                                tags$hr(),
                                h5(strong('Choose colonization intensities for each regime:')),
                                actionLink('help_colonization_intensity_2a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_2a',
                                                         label = 'Regime 1',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_2b',
                                                         label = 'Regime 2:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00'))))
                                  )
                                ),
                              conditionalPanel(
                                condition = "input.regime_no == '2' && input.colonization_intensity_2a == '0.00'",
                                h5('Choose growth rate (r) for regime 1:'),
                                selectInput(inputId = 'r_2a',
                                            label =  '',
                                            choices = c('0','1'))
                              ),
                              # three regimes
                              conditionalPanel(
                                condition = "input.regime_no == '3'",
                                tags$hr(),
                                h5(strong('Choose colonization intensities for each regime:')),
                                actionLink('help_colonization_intensity_3a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_3a',
                                                         label = 'Regime 1',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_3b',
                                                         label = 'Regime 2:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_3c',
                                                         label = 'Regime 3:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00'))))
                                )
                              ),
                              conditionalPanel(
                                condition = "input.regime_no == '3' && input.colonization_intensity_3a == '0.00'",
                                h5('Choose growth rate (r) for regime 1:'),
                                selectInput(inputId = 'r_3a',
                                            label =  '',
                                            choices = c('0','1'))
                              ),
                              # four regimes
                              conditionalPanel(
                                condition = "input.regime_no == '4'",
                                tags$hr(),
                                h5(strong('Choose colonization intensities for each regime:')),
                                actionLink('help_colonization_intensity_4a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_4a',
                                                         label = 'Regime 1',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_4b',
                                                         label = 'Regime 2:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_4c',
                                                         label = 'Regime 3:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_4d',
                                                         label = 'Regime 4:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00'))))
                                )
                              ),
                              conditionalPanel(
                                condition = "input.regime_no == '4' && input.colonization_intensity_4a == '0.00'",
                                h5('Choose growth rate (r) for regime 1:'),
                                selectInput(inputId = 'r_4a',
                                            label =  '',
                                            choices = c('0','1'))
                              ),
                              # five regimes
                              conditionalPanel(
                                condition = "input.regime_no == '5'",
                                tags$hr(),
                                h5(strong('Choose colonization intensities for each regime:')),
                                actionLink('help_colonization_intensity_5a', label = 'Help'),
                                fixedRow(
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_5a',
                                                         label = 'Regime 1',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_5b',
                                                         label = 'Regime 2:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_5c',
                                                         label = 'Regime 3:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_5d',
                                                         label = 'Regime 4:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00')))),
                                  column(3,
                                         div(style="display: inline-block;vertical-align:top; width: 70px;",
                                             selectInput(inputId = 'colonization_intensity_5e',
                                                         label = 'Regime 5:',
                                                         selected = '0.14',
                                                         choices = c('0.00','0.02','0.04','0.06','0.07','0.12',
                                                                     '0.14','0.69','0.99','1.00'))))
                                )
                              ),
                              conditionalPanel(
                                condition = "input.regime_no == '5' && input.colonization_intensity_5a == '0.00'",
                                h5('Choose growth rate (r) for regime 1:'),
                                selectInput(inputId = 'r_5a',
                                            label =  '',
                                            choices = c('0','1'))
                              ),
                              
                              conditionalPanel(
                                condition = "input.regime_no == '1' || input.regime_no == '2' || input.regime_no == '3' || input.regime_no == '4' || input.regime_no == '5'",
                                actionButton(inputId = 'AB_nonstationary',
                                             label = 'Get non-stationary policy'),
                                tags$style('#AB_nonstationary{background-color: #660033;color:#FFFFFF;border-color:#000000;}
                                                                   #AB_nonstationary:hover{background-color:#006699;}')
                              )
                            ),
                            
                            mainPanel(
                              h4('Reward function components:'),
                              tags$hr(),
                              conditionalPanel(
                                condition = "input.eco_type_nonstat == 'Exponential' || 
                                input.eco_type_nonstat == 'Sigmoidal'",
                                fluidRow(
                                  #plot
                                  splitLayout(withSpinner(plotOutput('eco_plot_nonstat'), type = 4,
                                                          color = '#660033'),
                                              withSpinner(plotOutput('penalty_plot_nonstat'), type = 4,
                                                          color = '#660033'))),
                                tags$hr(),
                                h4('Δ optimal removal effort:'),
                              ),
                              withSpinner(plotOutput('nonstationary_plot',
                                                     click = 'nonstat_hover'), type = 4,
                                          color = '#660033'),
                              tags$hr(),
                              htmlOutput('output_check'),
                              tags$head(tags$style("
                               #output_check {
                                color: #FFFFFF; font-size: 2px}")),
                              conditionalPanel(condition = "output.output_check == 'yes'",
                                               h4('Click anywhere on the above plot to see the stationary and non-stationary policies for the associated time point.'),
                                               tags$hr()),
                              conditionalPanel(condition = "input.nonstat_hover != null",
                                               plotOutput('hover_plot'))
                                               # tags$hr())
                              
                            )
                          )
                 )
                 
)

server <- function(input, output, session) {
  
  ########
  # popups
  
  # Note about not having linear removal cost function
  observeEvent(input$note, {
    showModal(modalDialog(
      h4(HTML(paste0("With the linear removal cost function, the optimal removal effort is often equal to a",tags$sub("max"),
                     ", or the maximum possible removal effort in the optimization. This makes comparisons to stationary policies challenging
                     if both the stationary and non-stationary policies are equal to a",tags$sub("max"),"."))),
      title = "In this section, only a nonlinear removal cost function is used.",
      easyClose = TRUE, footer = NULL)
    )
  })
  
  observeEvent(input$help_reward_ratio, {
    showModal(modalDialog(
      h4("The reward ratio describes the relative contributions of ecological change and removal cost to the overall reward function."),
      tags$hr(),
      h4(HTML(paste0("The ratio is calculated as: scale",tags$sub("R"),"/scale",tags$sub("E"),", where scale",tags$sub("R"), 
                     " is the reward value where the linear and nonlinear removal cost functions intersect and scale ",tags$sub("E"),
                     " is the reward value where the ecological change functions intersect."))),
      h4("For reference, a ratio of 0.00375 is used in the main manuscript."),
      tags$hr(),
      tags$img(
        src = base64enc::dataURI(file = "www/ratio_plot.jpg", mime = "image/jpg"),
        height = '320px',width = '800px'
      ),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_reward_ratio_nonstat, {
    showModal(modalDialog(
      h4("The reward ratio describes the relative contributions of ecological change and removal cost to the overall reward function."),
      tags$hr(),
      h4(HTML(paste0("The ratio is calculated as: scale",tags$sub("R"),"/scale",tags$sub("E"),", where scale",tags$sub("R"), 
                     " is the reward value where the linear and nonlinear removal cost functions intersect and scale ",tags$sub("E"),
                     " is the reward value where the ecological change functions intersect."))),
      h4("For reference, a ratio of 0.00375 is used in the main manuscript."),
      tags$hr(),
      tags$img(
        src = base64enc::dataURI(file = "www/ratio_plot.jpg", mime = "image/jpg"),
        height = '320px',width = '800px'
      ),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_discount_factor, {
    showModal(modalDialog(
      h4("The discount factor is a value between 0 and 1 that balances the importance of immediate rewards vs. future rewards."),
      tags$hr(),
      h4("For reference, a discount factor of 0.99 is used in the main manuscript, which means that the importance of future rewards is nearly equal to the importance of immediate rewards."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_discount_factor_nonstat, {
    showModal(modalDialog(
      h4("The discount factor is a value between 0 and 1 that balances the importance of immediate rewards vs. future rewards."),
      tags$hr(),
      h4("For reference, a discount factor of 0.99 is used in the main manuscript, which means that the importance of future rewards is nearly equal to the importance of immediate rewards."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_colonization_intensity, {
    showModal(modalDialog(
      h4("The colonization intensity, \u03B1, is a summary statistic that describes the frequency and magnitude of intense colonization events, 
         and is defined as the probability the colonization rate, \u03B3, is greater than 90% of carrying capacity (black dotted line below indicates 90% of carrying capacity, K)."),
      h4("\u03B1 = P(\u03B3 > 0.9K)"),
      tags$hr(),
      h4("The figure below depicts the colonization rate probability densities associated with the colonization intensity options. E[\u03B3] represents the expected value of each option."),
      tags$img(
        src = base64enc::dataURI(file = "www/colonizationrates.jpg", mime = "image/jpg"),
        height = '400px',width = '800px'
      ),
      h4("Note: To aid visualization, a probability density for the lowest colonization intensity (\u03B1 = 0.00) is not included, but the expected value of this option is 0.03."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_colonization_intensity_2a, {
    showModal(modalDialog(
      h4("The colonization intensity, \u03B1, is a summary statistic that describes the frequency and magnitude of intense colonization events, 
         and is defined as the probability the colonization rate, \u03B3, is greater than 90% of carrying capacity (black dotted line below indicates 90% of carrying capacity, K)."),
      h4("\u03B1 = P(\u03B3 > 0.9K)"),
      tags$hr(),
      h4("The figure below depicts the colonization rate probability densities associated with the colonization intensity options. E[\u03B3] represents the expected value of each option."),
      tags$img(
        src = base64enc::dataURI(file = "www/colonizationrates.jpg", mime = "image/jpg"),
        height = '400px',width = '800px'
      ),
      h4("Note: To aid visualization, a probability density for the lowest colonization intensity (\u03B1 = 0.00) is not included, but the expected value of this option is 0.03."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_colonization_intensity_3a, {
    showModal(modalDialog(
      h4("The colonization intensity, \u03B1, is a summary statistic that describes the frequency and magnitude of intense colonization events, 
         and is defined as the probability the colonization rate, \u03B3, is greater than 90% of carrying capacity (black dotted line below indicates 90% of carrying capacity, K)."),
      h4("\u03B1 = P(\u03B3 > 0.9K)"),
      tags$hr(),
      h4("The figure below depicts the colonization rate probability densities associated with the colonization intensity options. E[\u03B3] represents the expected value of each option."),
      tags$img(
        src = base64enc::dataURI(file = "www/colonizationrates.jpg", mime = "image/jpg"),
        height = '400px',width = '800px'
      ),
      h4("Note: To aid visualization, a probability density for the lowest colonization intensity (\u03B1 = 0.00) is not included, but the expected value of this option is 0.03."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_colonization_intensity_4a, {
    showModal(modalDialog(
      h4("The colonization intensity, \u03B1, is a summary statistic that describes the frequency and magnitude of intense colonization events, 
         and is defined as the probability the colonization rate, \u03B3, is greater than 90% of carrying capacity (black dotted line below indicates 90% of carrying capacity, K)."),
      h4("\u03B1 = P(\u03B3 > 0.9K)"),
      tags$hr(),
      h4("The figure below depicts the colonization rate probability densities associated with the colonization intensity options. E[\u03B3] represents the expected value of each option."),
      tags$img(
        src = base64enc::dataURI(file = "www/colonizationrates.jpg", mime = "image/jpg"),
        height = '400px',width = '800px'
      ),
      h4("Note: To aid visualization, a probability density for the lowest colonization intensity (\u03B1 = 0.00) is not included, but the expected value of this option is 0.03."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  observeEvent(input$help_colonization_intensity_5a, {
    showModal(modalDialog(
      h4("The colonization intensity, \u03B1, is a summary statistic that describes the frequency and magnitude of intense colonization events, 
         and is defined as the probability the colonization rate, \u03B3, is greater than 90% of carrying capacity (black dotted line below indicates 90% of carrying capacity, K)."),
      h4("\u03B1 = P(\u03B3 > 0.9K)"),
      tags$hr(),
      h4("The figure below depicts the colonization rate probability densities associated with the colonization intensity options. E[\u03B3] represents the expected value of each option."),
      tags$img(
        src = base64enc::dataURI(file = "www/colonizationrates.jpg", mime = "image/jpg"),
        height = '400px',width = '800px'
      ),
      h4("Note: To aid visualization, a probability density for the lowest colonization intensity (\u03B1 = 0.00) is not included, but the expected value of this option is 0.03."),
      tags$hr(),
      easyClose = TRUE, footer = NULL, size = 'l')
    )
  })
  
  ############
  # stationary
  ############
  
  # ecological change plot
  output$eco_plot <- renderPlot({
    req(input$penalty_type)
    req(input$eco_type)
    create_eco_objective_plots(input$eco_type,states,sig_loss_params,
                               exp_loss_params)
  })
  
  # removal cost plot
  output$penalty_plot <- renderPlot({
    req(input$penalty_type)
    req(input$eco_type)
    create_cost_objective_plots(input$penalty_type,input$reward_ratio,actions)
  })
  
  # create stationary df
  stationary_df <- eventReactive(input$AB_stationary, {
    req(input$penalty_type)
    req(input$eco_type)

    if(input$colonization_intensity=='0.00'){
      df <- create_stationary_df(input$eco_type,input$penalty_type,input$reward_ratio,
                           states,actions,sig_loss_params,
                           exp_loss_params,
                           input$discount_factor,input$colonization_intensity,input$r)
    } else {
      df <- create_stationary_df(input$eco_type,input$penalty_type,input$reward_ratio,
                           states,actions,sig_loss_params,
                           exp_loss_params,
                           input$discount_factor,input$colonization_intensity,'1')
    }
    
    create_stationary_plot(df,isolate(input$penalty_type),isolate(input$eco_type))

  })
  
  # create stationary plot
  output$stationary_plot <- renderPlot({
    stationary_df()
  })
  
  
  ################
  # non-stationary
  ################
  
  # ecological change plot
  output$eco_plot_nonstat <- renderPlot({
    req(input$eco_type_nonstat)
    create_eco_objective_plots(input$eco_type_nonstat,states,sig_loss_params,
                               exp_loss_params)
  })
  
  # removal cost plot
  output$penalty_plot_nonstat <- renderPlot({
    req(input$eco_type_nonstat)
    create_cost_objective_plots('Nonlinear',input$reward_ratio_nonstat,actions)
  })
  
  # create stationary df -- for nonstationary plot
  nonstat_statdf <- eventReactive(input$AB_nonstationary, {
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    if(input$regime_no == '2'){
      alpha <- c(input$colonization_intensity_2a,input$colonization_intensity_2b)
      r1 <- input$r_2a
    } else if(input$regime_no == '3'){
      alpha <- c(input$colonization_intensity_3a,input$colonization_intensity_3b,
                 input$colonization_intensity_3c)
      r1 <- input$r_3a
    } else if(input$regime_no == '4'){
      alpha <- c(input$colonization_intensity_4a,input$colonization_intensity_4b,
                 input$colonization_intensity_4c,input$colonization_intensity_4d)
      r1 <- input$r_4a
    } else if(input$regime_no == '5'){
      alpha <- c(input$colonization_intensity_5a,input$colonization_intensity_5b,
                 input$colonization_intensity_5c,input$colonization_intensity_5d,
                 input$colonization_intensity_5e)
      r1 <- input$r_5a
    }
    
    stat_df <- get_nonstat_stationary(alpha,r1,input$eco_type_nonstat,input$regime_no,
                                      input$discount_factor_nonstat,input$reward_ratio_nonstat,
                                      states,actions,sig_loss_params,
                                      exp_loss_params)
    
    nonstat_df <- get_nonstat_nonstationary(alpha,r1,input$eco_type_nonstat,input$regime_no,
                                            input$reward_ratio_nonstat,input$discount_factor_nonstat,
                                            states,actions,sig_loss_params,
                                            exp_loss_params)
    
    combine_stat_nonstat_tile(stat_df,nonstat_df,
                              r1,alpha,input$regime_no,states)
    
  })
  
  # output check
  output$output_check <- renderText({
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    if(length(nonstat_statdf()[[1]]$time) > 1){
      return('yes')
    }
  })
  
  # create non-stationary plot
  output$nonstationary_plot <- renderPlot({
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    if(input$regime_no == '2'){
      alpha <- c(input$colonization_intensity_2a,input$colonization_intensity_2b)
      r1 <- input$r_2a
    } else if(input$regime_no == '3'){
      alpha <- c(input$colonization_intensity_3a,input$colonization_intensity_3b,
                 input$colonization_intensity_3c)
      r1 <- input$r_3a
    } else if(input$regime_no == '4'){
      alpha <- c(input$colonization_intensity_4a,input$colonization_intensity_4b,
                 input$colonization_intensity_4c,input$colonization_intensity_4d)
      r1 <- input$r_4a
    } else if(input$regime_no == '5'){
      alpha <- c(input$colonization_intensity_5a,input$colonization_intensity_5b,
                 input$colonization_intensity_5c,input$colonization_intensity_5d,
                 input$colonization_intensity_5e)
      r1 <- input$r_5a
    }
    
    create_plot_deltaoptim(nonstat_statdf()[[1]],
                           alpha,r1,input$regime_no)
  })
  
  #create plot hover
  output$hover_plot <- renderPlot({
    req(input$eco_type_nonstat)
    req(input$regime_no)
    
    create_hoverplot(nonstat_statdf()[[2]],round(input$nonstat_hover$x))
      
  })
  
  
  #assessment planning; CPUE input -- plot
  # output$mu_prob_plot <- renderPlot({
  #   req(input$sampletype_CPUE)
  #   phi <- phi
  #   beta <- beta
  #   
  #   tryCatch(if(input$sampletype_CPUE == 'Traps' & input$CPUE_trapplanning >=0){
  #     trap_mu_prob_plot(phi, input$CPUE_trapplanning, 
  #                       input$probability_trapplanning_CPUE)
  #   } else if(input$sampletype_CPUE == 'eDNA' & input$CPUE_dnaplanning >=0){
  #     dna_mu_prob_plot(beta, input$CPUE_dnaplanning, input$probability_dnaplanning_CPUE)
  #   } else if(input$sampletype_CPUE == 'Traps & eDNA' & input$CPUE_jointplanning >=0){
  #     joint_mu_prob_plot(phi, beta, input$CPUE_jointplanning, 
  #                        input$probability_jointplanning_CPUE)
  #   },
  #   error = function(e){''})
  #   
  # })
  
  
  
  #assessment planning; CPUE input -- diff. trap types -- check 
  # output$trap_plan_traptypes_CPUE_input_check <- renderText({
  #   req(input$CPUE_trapplanning)
  #   if(input$assessment_planning_type == 'Necessary effort'&
  #      input$sampletype_CPUE == 'Traps'&
  #      trap_mu_prob_xmax(phi,input$CPUE_trapplanning,
  #                       input$probability_trapplanning_CPUE)>0){
  #     return('yes')
  #   }
  #   
  # })
  
  #assessment planning; CPUE input -- diff. trap types -- range of traps/water samples
  # output$trap_plan_traptypes_CPUE_input <- renderText({
  #   req(input$sampletype_CPUE)
  #   req(input$trapplan_traptype_CPUEinput)
  #   req(input$trapplan_crabsize_CPUEinput)
  #   
  #   case_when(input$sampletype_CPUE == 'Traps' & input$CPUE_trapplanning >=0 & input$trapplan_traptype_CPUEinput == 'Shrimp' ~
  #               paste0('If the trap-adjusted capture rate (',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' crabs/100 fukui trap sets) is ',
  #                      input$CPUE_trapplanning,
  #                      ', you need to set ',
  #                      '<font color=\'#9A8822\'><b>',
  #                      trap_mu_prob_xmax_traptype(input$CPUE_trapplanning, input$probability_trapplanning_CPUE, 
  #                                                 input$trapplan_traptype_CPUEinput, input$trapplan_crabsize_CPUEinput),
  #                      ' shrimp trap sets ',
  #                      '</b></font>',
  #                      'to be ',
  #                      input$probability_trapplanning_CPUE,
  #                      '% confident you will capture at least one ',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' green crab.'),
  #             input$sampletype_CPUE == 'Traps' & input$CPUE_trapplanning >=0 & input$trapplan_traptype_CPUEinput == 'Minnow' ~
  #               paste0('If the trap-adjusted capture rate (',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' crabs/100 fukui trap sets) is ',
  #                      input$CPUE_trapplanning,
  #                      ', you need to set ',
  #                      '<font color=\'#9A8822\'><b>',
  #                      trap_mu_prob_xmax_traptype(input$CPUE_trapplanning, input$probability_trapplanning_CPUE, 
  #                                                 input$trapplan_traptype_CPUEinput, input$trapplan_crabsize_CPUEinput),
  #                      ' minnow trap sets ',
  #                      '</b></font>',
  #                      'to be ',
  #                      input$probability_trapplanning_CPUE,
  #                      '% confident you will capture at least one ',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' green crab.'),
  #             input$sampletype_CPUE == 'Traps' & input$CPUE_trapplanning >=0 & input$trapplan_traptype_CPUEinput == 'Fukui' ~
  #               paste0('If the trap-adjusted capture rate (',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' crabs/100 fukui trap sets) is ',
  #                      input$CPUE_trapplanning,
  #                      ', you need to set ',
  #                      '<font color=\'#9A8822\'><b>',
  #                      trap_mu_prob_xmax_traptype(input$CPUE_trapplanning, input$probability_trapplanning_CPUE, 
  #                                                 input$trapplan_traptype_CPUEinput, input$trapplan_crabsize_CPUEinput),
  #                      ' fukui trap sets ',
  #                      '</b></font>',
  #                      'to be ',
  #                      input$probability_trapplanning_CPUE,
  #                      '% confident you will capture at least one ',
  #                      ifelse(input$trapplan_crabsize_CPUEinput=='Small (< 45 mm CW)','small','large'),
  #                      ' green crab.'),
  #             TRUE~'')
  # })
  # 
  # 
  # getData_jointtrap <- reactive({
  #   if(is.null(input$file1_joint)) return(NULL)
  #   data <- read.csv(input$file1_joint$datapath)
  # })
  
  
  
  #management targets -- true capture rate -- site options
  # output$tcaprate_sitesel <- renderUI({
  # 
  #   df <- getData() 
  #   options <- sort(unique(df$Site_Name))
  #   selectInput(
  #     inputId = "tcaprate_siteselection",
  #     label = NULL,
  #     choices = c(options),
  #     selected = NULL)
  #   
  # })
  # 
  
  #management targets -- run trap model, true capture rate, standard
  # mt_trap_model_standard <- eventReactive(input$actionbutton_trapmodel_standard, {
  #   req(input$file1_trap)
  #   ntrap <- input$trapmodel_ntrap
  #   site <- input$tcaprate_siteselection
  #   time <- input$tcaprate_timeselection
  #   phi <- phi
  #   data <- read.csv(input$file1_trap$datapath)
  #   trap_model_run_standard(data,phi,ntrap,site,time)
  # })
  
  
  #management targets -- trap model, plot, standard -- target
  # output$trap_target_plot_standard <- renderPlot({
  #   req(input$file1_trap)
  #   trap_model_plot_standard(mt_trap_model_standard(), input$trap_model_target_standard)
  # })
  
  
  #management targets -- true capture rate, create empty summary table
  # rv_tcaprate <- reactiveValues(data = data.frame(Site = character(), Time = character(), Median = numeric(),
  #                                                 Lower_CrI = numeric(), Upper_CrI = numeric(),
  #                                                 Target = numeric(), Prob = numeric(), Expectation = numeric(),
  #                                                 Latitude=numeric(),Longitude=numeric()))
  # 
  
  #management targets -- true capture rate, standard, add results to summary table
  # observeEvent(input$actionbutton_tcaprate_s_summaryadd, {
  #   req(input$file1_trap)
  #   data_in <- read.csv(input$file1_trap$datapath)
  #   rv_tcaprate$data <- rbind(rv_tcaprate$data, data.frame(Site=input$tcaprate_siteselection, Time=input$tcaprate_timelabel,
  #                                                          Median=round(median(mt_trap_model_standard())*100,2),
  #                                                          Lower_CrI=round(p.interval(mt_trap_model_standard(), HPD = TRUE, prob = 0.95)[1]*100,2),
  #                                                          Upper_CrI=round(p.interval(mt_trap_model_standard(), HPD = TRUE, prob = 0.95)[2]*100,2),
  #                                                          Target=ifelse(input$mtarget_trap_s_check == 0,input$trap_model_target_standard,NA),
  #                                                          Prob=ifelse(input$mtarget_trap_s_check == 0, 
  #                                                                      trap_prob_target_standard(mt_trap_model_standard(), input$trap_model_target_standard), NA),
  #                                                          Expectation=NA,
  #                                                          Latitude= ifelse('Latitude' %in% colnames(data_in),
  #                                                                           calculate_site_latlon(input$tcaprate_timeselection,
  #                                                                                         data_in,
  #                                                                                         input$tcaprate_siteselection)[[1]],
  #                                                                           NA),
  #                                                          Longitude=ifelse('Longitude' %in% colnames(data_in),
  #                                                                           calculate_site_latlon(input$tcaprate_timeselection,
  #                                                                                          data_in,
  #                                                                                         input$tcaprate_siteselection)[[2]],
  #                                                                           NA)))
  #   
  # })
  
  
  
  #management targets -- run trap model, standard -- diff. trap types -- small crabs
  # mt_trap_model_traptype_standard <- eventReactive(input$actionbutton_trapmodel_standard_traptype, {
  #   req(input$file1_traptype)
  #   ntrap_m <- input$ntrap_m_traptype_model
  #   ntrap_f <- input$ntrap_f_traptype_model
  #   ntrap_s <- input$ntrap_s_traptype_model
  #   site <- input$adjcaprate_siteselection
  #   time <- input$adjcaprate_timeselection
  #   data <- read.csv(input$file1_traptype$datapath)
  #   trap_model_run_traptype(data,ntrap_m,ntrap_f,ntrap_s,site,time)
  # })
  

  
  #help page: question 3 -- part 4
  # output$q3_p4 <- renderUI({
  #   if(input$q3_action %% 2 != 0){
  #     t1 <- paste0(strong('eDNA submodel:'))
  #     t2 <- paste0('The eDNA component of the model is linked to the trap model through ',em('µ'),tags$sub('i'),', or the true capture rate at site ',em('i'),'.')
  #     t3 <- paste0('The number of positive qPCR detections, ',em('K'),', out of the number of trials, ',em('N'),', in water sample, ',em('j'),
  #     ', at site, ',em('i'),', is drawn from a binomial distribution with a probability of success on a single trial, ',em('p'),tags$sub('i'),':')
  #     t4 <- paste0('K',tags$sub('i,j'),' ~ Binomial(N',tags$sub('i,j'),', p',tags$sub('i'),')')
  #     t5 <- paste0('The total probability of detection, ',em('p'),tags$sub('i'), ', is the sum of the true positive probability of detection, ',
  #                  em('p11'),tags$sub('i'), ', and the false positive probability of detection, ',em('p10'),':')
  #     t6 <- paste0('p',tags$sub('i'), ' = p11',tags$sub('i'),' + p10')
  #     t7 <- paste0('The true positive probability of detection, ',em('p11'),tags$sub('i'),', is a saturating function of the true capture rate, µ',tags$sub('i'),
  #                  ', and scaling coefficient, ',em('β'),':')
  #     t8 <- paste0('p11',tags$sub('i'),' = µ',tags$sub('i'),'/(µ',tags$sub('i'),' + β)')
  #     HTML(paste(t1,t2,t3,t4,t5,t6,t7,t8, sep = '<br/>'))
  #   } else{
  #     HTML(paste0(''))
  #   }
  # })
  
}

shinyApp(ui = ui, server = server)