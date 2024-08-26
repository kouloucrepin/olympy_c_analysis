library(shiny)
library(shinydashboard)
library(fontawesome)
library(DT)
source("package.R")

ui <- dashboardPage(
  dashboardHeader(title = "Olympic Gamming Dashboard ",titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Edition", tabName = "edition", icon = icon("home")),
      menuItem("Players", tabName = "jouer", icon = icon("chart-line")
      )
    )
  ),
  dashboardBody(
    tabItems(
      ### partie edition
      tabItem(tabName = "edition",
          fluidRow(
              valueBoxOutput("nb_edition",width = 3),
              valueBoxOutput("nb_winner",width = 3),
              box(width = 6,height = 100,selectInput("year_edit","Competition year",choices =unique(Olympic_Games_data$year),width = "100%"))
              ),
          fluidRow(
            box(width = 6,height = 500,
                radioButtons("top_medal",label = "Medal type",choices = c("gold","silver","bronze","total"),inline = TRUE,width = "100%"),
                plotlyOutput("pie_top")),
            tabBox(width = 6,height = 500,
                  tabPanel("Plot",
                           plotlyOutput("classement_pays",width ="100%"),
                  icon=fa("chart-bar")),
                  tabPanel("table",dataTableOutput("table_edi",height = 420),icon=fa("table"))
                )
          ),
          fluidRow(
            tabBox(width = 12,height = 500,side = "right",
              tabPanel("Country winner",icon = fa("chart-line"),
                       column(width = 6,
                              selectInput("pay_edit","select contries",choices = sort(unique(Olympic_Games_data$country)),multiple = TRUE,selected = "France",width = 490)
                       ),
                       column(width=6,
                              sliderInput(inputId = "annee_edit", label = "Select range years",min = min(Olympic_Games_data$year),max = max(Olympic_Games_data$year),value = c(2000, 2020),step = 2,width = 490)
                       ),
                       column(width=12,
                              plotlyOutput("evolution_medal")
                       )
                       
              ),
              tabPanel("winner per season",icon = fa("chart-line"),
                       plotlyOutput("partici_peryear")
                       )
              
            )
            
          )
          ),
      ### partie joueur
      tabItem(tabName = "jouer",
              column(width = 6,
              fluidRow(
              valueBoxOutput("total_Equestrian",width = 6),
              valueBoxOutput("total_Summer",width = 6),
              ),
              fluidRow(
              valueBoxOutput("total_participiant",width = 6),
              valueBoxOutput("total_Winter",width = 6)
              ),
              fluidRow(
                box(width = 12,height = 350,
                    plotlyOutput("bmi_ind"))
              )
              ),
              column(width=6,
                     fluidRow(box(width = 12,height = 600,
                         sliderInput(inputId = "annee_play", label = "Select range years",min = min(inter$edition),max = max(inter$edition),value = c(2000, 2020),step = 2),
                         radioButtons(inputId="sex_play",label="participant or winner?",c("participante","winner"),selected = "winner",inline=TRUE),
                         plotlyOutput("pie_part_or_win")
                         ))
                     ),
              fluidRow(
                box(width = 4,height = 400,
                    sliderInput(inputId = "anne_play", label = "Select range years",min = min(inter$edition),max = max(inter$edition),value = c(2000, 2020),step = 2),
                    selectInput("edi_country",label = "choose a country",choices = sort(unique(Olympic_Games_data$country))),
                    radioButtons("radio_opt",label = "choose an option",choices =c("all participant","only winner"),inline = TRUE)
                    ),
                tabBox(width = 8,height = 400,
                       tabPanel("all",
                                plotlyOutput("id_all",height = 320)
                                ),
                       tabPanel("secific",
                                plotlyOutput("id_sepecific",height = 320)
                                ),
                       tabPanel("player info",
                                dataTableOutput("table_play"),icon=fa("table")
                                )
                       )
                )
      )
    )
    
))


server <- function(input, output) {
  ### edition
  #####icone
  output$nb_edition<-renderValueBox({
    valueBox(
      as.vector(Olympic_Games_data %>% select(edition_id) %>% unique() %>% count())$n,
      "Total edition",
      color = "orange",icon = icon("trophy", "fa-fw") 
    )
  })
  output$nb_winner<-renderValueBox({
    valueBox(
      as.vector(Olympic_Games_data %>% select(country) %>% unique() %>% count())$n,
      "Total Country",
      color = "green",icon= icon("globe", "fa-fw")  
    )
  })
  observeEvent(input$top_medal,{
    if(input$top_medal=="gold"){
      output$pie_top<-renderPlotly({
        top_n_coutry(data=Olympic_Games_data,height = 410,varr=gold,tit="gold")
      })}else if(input$top_medal=="silver"){
        output$pie_top<-renderPlotly({
          top_n_coutry(data=Olympic_Games_data,height = 410,varr=silver,tit="silver")
        })
      }else if(input$top_medal=="bronze"){
        output$pie_top<-renderPlotly({
          top_n_coutry(data=Olympic_Games_data,height = 410,varr=bronze,tit="bronze")
        })
      }else{
        output$pie_top<-renderPlotly({
          top_n_coutry(data=Olympic_Games_data,height = 410,varr=total,tit="total")
        })
      }
    })
  observeEvent(input$year_edit,{
  output$classement_pays<-renderPlotly({
      medal_year(data=Olympic_Games_data,year=as.integer(input$year_edit),height = 440)
    })
    
    ds=Olympic_Games_data %>% filter(year==as.integer(input$year_edit)) %>% select(country,gold,silver,bronze,total)
    max_values <- sapply(ds, max, na.rm = TRUE)
    output$table_edi<-renderDataTable({
      datatable(
        ds,
        options = list(pageLength = 7,rowCallback = JS(
          "function(row, data, index) {",
          "  var maxValues = ", jsonlite::toJSON(max_values), ";",
          "  for (var i = 1; i < data.length; i++) {",  # Commencer à 1 pour ignorer la première colonne (Name)
          "    var value = data[i];",
          "    var intensity = Math.min(255, Math.floor((value / maxValues[i - 1]) * 255));",
          "    $(row).find('td:eq(' + i + ')').css('background-color', 'rgb(' + intensity + ', 200, 200)');",
          "  }",
          "}"
        ))  # Définit le nombre de lignes par page
      ) 
      })
  })
  a<-reactive({medal_year_country(countries=input$pay_edit,years=seq(as.integer(input$annee_edit)[1],as.integer(input$annee_edit)[2]),height=338)})
  output$evolution_medal<-renderPlotly({a()})
  output$partici_peryear<-renderPlotly({participant_season(data=inter,height=400,cols=seq(1920,2020))})
  
  ###player
  observeEvent(input$annee_play,{
    fr = seq(as.integer(input$annee_play)[1],as.integer(input$annee_play)[2])
    b=icone_partitcipant(data=inter,interval=fr)
    
    output$total_Equestrian = renderValueBox({
      valueBox(
        b[1],
        "Total Euisterian",
        color = "orange",icon = icon("horse", "fa-fw"),width = 3
      )
    })
    
    output$total_Summer= renderValueBox({
      valueBox(
        b[2],
        "total Summer",
        color = "blue",icon = icon("sun", "fa-fw") ,width = 3
      )
    })
    output$total_participiant = renderValueBox({
      valueBox(
        b[3],
        "total participiant",
        color = "green",icon = icon("eye", "fa-fw") ,width = 3
      )
    })
    output$total_Winter = renderValueBox({
      valueBox(
        b[4],
        "total Winter",
        color = "yellow",icon = icon("skiing", "fa-fw"),width = 3
      )
    })
    
    if(input$sex_play=="participante"){
      output$pie_part_or_win<-renderPlotly({
        participant_sice_begin(data=inter,height=420,cols=fr)
      })
    }else{
      output$pie_part_or_win<-renderPlotly({
        winner_sice_begin(data=inter,height=420,cols=fr)
      })}
    
    })
  observeEvent(input$sex_play,{
    fr2 = seq(as.integer(input$annee_play)[1],as.integer(input$annee_play)[2])
    if(input$sex_play=="participante"){
      output$pie_part_or_win<-renderPlotly({
        participant_sice_begin(data=inter,height=420,cols=fr2)
      })
    }else{
      output$pie_part_or_win<-renderPlotly({
        winner_sice_begin(data=inter,height=420,cols=fr2)
      })
      
    }
  })
  output$bmi_ind<-renderPlotly({
    bmi_stat(datas=inert,height = 332)
  })
  
  output$id_all<-renderPlotly({
    if(input$radio_opt=="all participant"){
      total_paet_sex(data=inter,slice=seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),height=300,titre="Total participant per year and per sex")
    }else{
      
      total_paet_sex(data = inter %>% select(medal,sex,edition) %>% filter(medal!=""),slice=seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),height=300,titre = "Total winner per sex and per year")
    }
    
  })
  
  output$id_sepecific<-renderPlotly({
    if(input$radio_opt=="all participant"){
      total_spec_sex(data=inter,slice=seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),height=300,titre="Total participant per year and per sex",couty = input$edi_country)
    }else{
      x=inter %>% select(medal,sex,edition,country.y) %>% filter(medal!="")
      total_spec_sex(data = x,slice=seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),height=300,titre = "Total winner per sex and per year",couty = input$edi_country)
    }
    
  })
  
  output$table_play<-renderDataTable({
    if(input$radio_opt=="all participant"){
      datatable(
        inter %>% select(athlete,sport,age,sex,height,weight,edition,country.y) %>% filter(edition %in% seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),country.y==input$edi_country),
        options = list(pageLength = 4)) 
    }else{
      datatable(
        inter %>% filter(medal!="") %>% select(athlete,sport,age,sex,height,weight,edition,country.y,medal) %>% filter(edition %in% seq(as.integer(input$anne_play)[1],as.integer(input$anne_play)[2]),country.y==input$edi_country),
        options = list(pageLength = 4)) 
    }
  })
  

}

shinyApp(ui, server)