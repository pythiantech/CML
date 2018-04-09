library(shiny)


# Define UI for application that draws a histogram
shinyUI(
  
  dashboardPage(
    dashboardHeader(title="CML Analytics",titleWidth = 280),
    dashboardSidebar(title = tags$a(href='http://pythtech.com',
                                    tags$img(src='logo.png',width=250)),width = 280,
                     sidebarMenu(
                       # Create menuItem()
                       menuItem(
                         text="Demographics",
                         tabName="demog",
                         icon = icon("users")
                       ),
                       menuItem(
                         text="Initial Diagnosis",
                         tabName="init",
                         icon=icon("stethoscope")
                       ),
                       menuItem(
                         text="Follow Up",
                         tabName = "follow",
                         icon=icon("edit")
                       )
                     )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "demog",
                fluidRow(
                  infoBox(title="Total Patients", value=total,color="green",fill = TRUE),
                  infoBox(title="Females", value=genderCount[1,2], icon=icon("venus"),color="fuchsia",fill = TRUE),
                  infoBox(title="Males", value=genderCount[2,2], icon=icon("mars"),color="aqua", fill=TRUE)
                 ),
                fluidRow(
                         box(selectInput("varble","Select Variable for Histogram",
                                         choices = c("age","height","weight")),
                             radioButtons("sex","Select Gender",choices=c("Female","Male","Both")),
                             sliderInput("bins","Select Binwidth",min=2,max=50,value=30),
                             width=3
                             ),
                         box(plotlyOutput("histo"),width = 4,height=500),
                         box(plotlyOutput("scatter"),width=5)
                         
                  
                ),
                fluidRow(
                  box(width=12,
                      title = "Where do patients come from?",
                      solidHeader = TRUE,
                      leafletOutput("map")
                      )
                )
              )
            )
    ),
    tags$head(tags$style(HTML("
    .skin-blue .main-sidebar {
                              background-color:  #eaedf2;
                              }
    .skin-blue .main-header .logo {
                              background-color: #6167ad}")))
  )
)
