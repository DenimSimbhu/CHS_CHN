library(tidyverse)
library(gt)
library(janitor)
library(shiny)
#library(shinydashboard)
library(googlesheets4)
library(rmarkdown)
#library(DT)
library(scales)
library(ggpubr)
library(scico)
library(plyr)
library(gtExtras)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(shinythemes)
library(gtExtras)
#library(shinyvalidate)
library(lubridate)
library(purrr)
library(ggtext)
library(extrafont)
library(reactable)
library(reactablefmtr)
library(glue)
library(calendR)
#library(janitor)
library(tidyr)
#library(htmlTable)
library(htmltools)
library(fontawesome)
library(shinycssloaders)
#library(shinyShortcut)
#library(ggcharts)
library(htmltools)
library(svglite)
#library(styler)
library(googlesheets4)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(magick)
#library(corrly)
#Data Cleansing
library(MatrixModels)
#install.packages(c("Matrix", "MatrixModels", "mvtnorm"), type = "binary")



gs4_auth(path = "dh_chs.json")

C_Sprint <-   gs4_get('https://docs.google.com/spreadsheets/d/1N98HzU9G3IhIfjRSpCKx8q2CI9UbSD-CmjyYUvVdTrw/')

sp_tkt <- gs4_get('https://docs.google.com/spreadsheets/d/1DWX6EnZ_p-b-oUPxa4NLOmJM-XjuB9VKkht0mJYQDNo/')

idle<-gs4_get('https://docs.google.com/spreadsheets/d/1XmDQDFamFoSuTDJ1xjQuLmgJwvByslFM7Ce7DLM1NeM/')

chs_chn_ip <-  read_sheet(C_Sprint)




sp_tkt <- read_sheet(sp_tkt)



idle_tkt<- read_sheet(idle)
#chs_chn <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN2.csv')
#chs_chn_ip <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN_IP.csv')
#sp_tkt <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/CHS_CHN.csv')
pr_percent <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/pr_percent.csv')
spillover <- readr::read_csv('https://raw.githubusercontent.com/DenimSimbhu/CHS_CHN/main/Jira_Committed.csv')


idle_tkt<-idle_tkt%>%
  clean_names()%>%
  dplyr::mutate(updateds = as.Date(updated,format = "%m/%d/%Y %H:%M:%S"),last_updated=Sys.Date()-updateds)%>%
  select(key,assignee,status,summary,last_updated)

Tasks_CD_tf <- spillover%>%
  clean_names()%>%
  ## filter(sprint=="Denim Sprint 12")%>%
  select(ID="issue_key",Summary="summary",Spillover="slipover",Assignee = "assignee",Sprint="sprint")
# dplyr::mutate(Sprint="Sprint 13")


reactdata<- Tasks_CD_tf %>%
  dplyr::group_by(Sprint)%>%
  dplyr::count(Spillover)%>%
  select(Spillover,Total="n",Sprint)%>%
  arrange((Total))


color_palette <-  c("#088395","#E69F00")
names(color_palette)<-c("Actual","Estimate")
title_text <- glue::glue(
  "Comparing   <span  style = 'color:{color_palette['Estimate']}'><b>Story Point Estimate (Days)</b> </span> Vs <span style = 'color:{color_palette['Actual']}'> <b>Expended Effort(Days)</b> </span>"
)

pr_percent_overall <- pr_percent %>%
  group_by(Assignee)%>%
  summarise(Sprint="Overall",Tickets_Completed=sum(Tickets_Completed),ReWork_PR=sum(ReWork_PR),
            Clean_PR= sum(Clean_PR),Success_Rate=(Clean_PR/Tickets_Completed)*100 )


#"Comparing   <span style = 'color:{color_palette['Actual']}'> <b>Expended Effort</b> </span> Vs <span style = 'color:{color_palette['Estimate']}'><b>Story Point Estimate</b></span>"

pr_percent <- rbind(pr_percent,pr_percent_overall)

#find a day in a year
#yday(ydm("2024-18-11"))

##Holiday Setup
events <- rep(NA, 366)

# Set the corresponding events
events[276] <- "Official Holiday"
events[285] <- "Official Holiday"
events[305] <- "Official Holiday"
events[360] <- "Official Holiday"
events[250] <- "Official Holiday"
events[228] <- "Official Holiday"
events[122] <- "Official Holiday"
events[100] <- "Official Holiday"
events[102] <- "Official Holiday"
events[89] <- "Official Holiday"
events[26] <- "Official Holiday"
events[275] <- "Himanshu PTO"
events[31] <- "Himanshu PTO"
events[218] <- "Himanshu PTO"
events[277] <- "Himanshu PTO"
events[278] <- "Himanshu PTO"
events[281:284] <- "Sandeep PTO"
events[29:33] <- "Sandeep PTO"
#events[29:33] <- "Divyum PTO"
events[323] <- "Divyum PTO" 


## Holdiay End
#chs_chn_ip <- read_csv("CHS_CHN_IP.csv")

chs_chn_ip <- chs_chn_ip %>%
  clean_names() %>%
  dplyr::mutate(
    ee1 = case_when(
      is.na(expended_effort_16) ~ expended_effort_17,
      TRUE ~ expended_effort_16
    )
  )%>%   
  dplyr::mutate(
    expended_effort = case_when(
      is.na(ee1) ~ expended_effort_18,
      TRUE ~ ee1
    ))%>%
  filter(!is.na(status))%>%
  dplyr::select(
    Task = key ,
    Summary = summary,
    Assignee = assignee,
    Sprint = sprint,
    Status = status,
    story_point_estimate=story_point_estimate,
    expended_effort
  ) %>%
  # filter(Status != "4_Review") %>%
  dplyr::mutate(
    Status = case_when(
      Status == "1_Requirements" ~ "Yet to Start",
      Status == "3_In Progress" ~ "In Progress",
      Status == "4_Review" ~ "In Review",
      Status == "2_Design" ~ "In Progress",
      Status == "2.5_BLOCKED" ~ "Blocked",
      TRUE ~ "Completed"
    )
  )%>%
  dplyr::mutate(
    Estimate = story_point_estimate/2)


####New Try
df_ip <- chs_chn_ip%>%
  filter(Status=="In Progress")%>%
  nrow()

df_tot <- chs_chn_ip%>%
  nrow()


df_blocked <- chs_chn_ip%>%
  filter(Status=="Blocked")%>%
  nrow()

df_completed <- chs_chn_ip%>%
  filter(Status=="Completed")%>%
  nrow()

df_review <- chs_chn_ip%>%
  filter(Status=="In Review")%>%
  nrow()

df_YTS <- chs_chn_ip%>%
  filter(Status=="Yet to Start")%>%
  nrow()



#######New end###

pal1 = scico::scico(100, palette = 'nuuk')


##Sprint wise data transformation

sp_tkt_Tf <- sp_tkt %>%
  clean_names() %>% 
  dplyr::mutate(
    ee1 = case_when(
      is.na(expended_effort_15) ~ expended_effort_17,
      TRUE ~ expended_effort_15
    )
  )%>%   
  dplyr::mutate(
    Expended_effort = case_when(
      is.na(ee1) ~ expended_effort_18,
      TRUE ~ ee1
    ))%>%
  dplyr::select(
    Task = key ,
    Summary = summary,
    Assignee = assignee,
    Actual_Start = chart_date_of_first_response,
    Actual_Finish = updated,
    Sprint = sprint,
    Status = resolution,
    Expended_effort,
    story_point_estimate
  ) 


format_dmy_af  <- as.Date(sp_tkt_Tf$Actual_Finish, format = "%d-%m-%Y %M:%S")
format_dmy2_af <- as.Date(sp_tkt_Tf$Actual_Finish, format = "%d/%b/%y %H:%M %p")
format_dmy  <- as.Date(sp_tkt_Tf$Actual_Start, format = "%d-%m-%Y %M:%S")
format_dmy2 <- as.Date(sp_tkt_Tf$Actual_Start, format = "%m/%d/%Y %H:%M:%S")

##################

chs_chn_tf <- sp_tkt %>%
  dplyr::select(
    "Ticket Summary" = Summary,
    Issue_ID = Key,
    #Type = key,
    Status,
    Priority,
    Assignee,
    Created,
    Updated
  ) %>%
  dplyr::mutate(Created =  ymd(as.Date(Created, format = "%d/%B/%Y %H:%M"))) %>%
  dplyr::mutate(Updated =  ymd(as.Date(Updated, format = "%d/%B/%Y %H:%M"))) %>%
  
  dplyr::mutate(Days = as.integer((Sys.Date() - Created))) %>%
  dplyr::mutate(
    Status = case_when(
      Status == "1_Requirements" ~ "Yet to Start",
      Status == "3_In Progress" ~ "In Progress",
      Status == "4_Review" ~ "In Review",
      TRUE ~ "Completed"
    )
  )



chs_chn_tfd <- chs_chn_tf %>%
  clean_names() %>%
  dplyr::select(ticket_summary,
                issue_id,
                status,
                assignee,
                priority,
                created,
                updated,
                days) %>%
  filter(status != "Completed")

chs_chn_tfc <- chs_chn_tf %>%
  clean_names() %>%
  dplyr::select(ticket_summary, issue_id, status, assignee, priority)

chs_chn_ip_overall<-chs_chn_ip%>%
  filter(Status!= "Completed")%>%
  filter(Sprint==max(Sprint))%>%
  group_by(Assignee)%>%
  dplyr::count()%>%
  dplyr::mutate(`# Tasks` =  n )



chs_chn_ip[is.na(chs_chn_ip)]=0



chs_chn_ip_story <- chs_chn_ip%>%
  # dplyr::mutate(
  #   Estimate = story_point_estimate/2)%>%
  group_by(Assignee)%>%
  filter(Sprint==max(Sprint))%>%
  summarise(Days_Wrk = sum(Estimate))

chs_chn_esti<-chs_chn_ip%>%
  dplyr::mutate(Actual = expended_effort/2 )%>%
  group_by(Assignee)%>%
  summarise(Estimate = sum(Estimate),Actual= sum(Actual,na.rm = TRUE))%>%
  pivot_longer(cols=Estimate:Actual,names_to = "Type",values_to = "Values")
orange_pal <- function(x)
  rgb(colorRamp(c("#6DB9EF", "#F3B664"))(x), maxColorValue = 255)

###Cleansing


user_base <- tibble::tibble(
  user = c("Simbu", "Ben","Santosh"),
  password =  purrr::map_chr(c("Simbu12@", "Eg3*MV_t)H>=P{.fkq&@6S","60{VE7Imo|c!"), sodium::password_store),
  permissions = c("admin", "standard","Editor"),
  name = c("Simbu", "Ben","Santosh")
)

# login tab ui to be rendered on launch
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login",title = "Denim Health Dashboard")
)

# additional tabs to be added after login
home_tab <-   tabPanel("OverallSummary",fluidRow(
  valueBoxOutput("TaskTotal",width=2),
  valueBoxOutput("YTS",width=2),
  valueBoxOutput("Completed",width=2),
  valueBoxOutput("InProgressTotal",width=2),
  valueBoxOutput("Blocked",width=2),
  valueBoxOutput("InReview",width=2)
  
),br(),hr(),
fluidRow(column(6, style = 'border: 1px solid lightgrey; border-radius: 25px',
                br(),
                # ntitle and info button
                #  div(HTML('<b>Sales Map Here</b> '), style = 'display: inline-block;'),
                # uiOutput('sales_map_button', style = 'display: inline-block;'),
                
                # map plot
                plotOutput('OverallPlot'),
                br(), br()
),
column(6, style = 'border: 1px solid lightgrey; border-radius: 25px',
       br(),
       # ntitle and info button
       #  div(HTML('<b>Sales Map Here</b> '), style = 'display: inline-block;'),
       # uiOutput('sales_map_button', style = 'display: inline-block;'),
       
       # map plot
       plotOutput('Plot'),
       br()
)))




Act_Target_Tab <-    tabPanel(title = "Actual vs Target (Task Duration)", sidebarLayout(
  sidebarPanel(
    selectInput(
      "dataset2",
      label = "Sprint",
      choices = unique(sp_tkt_Tf$Sprint),
      selected = max(sp_tkt_Tf$Sprint)
    ),
    selectInput(
      "dataset1",
      label = "Resources",
      choices = unique(sp_tkt_Tf$Assignee),
      selected = "Himanshu Pant"
    )
  ), mainPanel(shinycssloaders::withSpinner(gt_output(outputId = "ResPR")))))

#Sprint Tab
# Sprint_Tab <-  tabPanel("Sprint Spillover",sidebarLayout( sidebarPanel(
#   selectInput(
#     "SpillSprint",
#     label = "Sprint",
#     choices = unique(Tasks_CD_tf$Sprint)
#     # selected = "Himanshu Pant"
# )) ,shinycssloaders::withSpinner(reactableOutput("SpilloverReport"))))
#Holiday Tab


# selected = "Himanshu Pant"



PR_Tab <-   tabPanel("PR Report" ,sidebarLayout( sidebarPanel(
  selectInput(
    "sprint1",
    label = "Sprint",
    choices = unique(pr_percent$Sprint)
    # selected = "Himanshu Pant"
  )) ,mainPanel(reactableOutput("PR1"))))
Sprint_ee_Tab <- tabPanel("Story Point Vs Expended Effort Correlation"
                          , shinycssloaders::withSpinner(plotOutput(
                            "SPEEReport"
                          )))

OG_Tab <- tabPanel("On-Going Tasks"
                   , shinycssloaders::withSpinner(gt_output(
                     "OverallSummaryReport"
                   )))
Idle_Tab <- tabPanel("Idle >2Days "
                     , shinycssloaders::withSpinner(gt_output(
                       "IdleReport"
                     )))
Sprint_Tab <-  tabPanel("PTO Calendar",shinycssloaders::withSpinner(plotOutput("SpilloverReport")))


More_Tab <-  navbarMenu(
  "More",
  tabPanel(
    "Estimate",      value = 4,
    plotOutput("Estimate")
  ),
  tabPanel(
    "Download Data",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "dataset",
          "Choose a dataset:",
          choices = c("PR_Details", "PR_DrillDown")
        ),
        radioButtons("filetype", "File type:", choices = c("csv", "tsv")),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(tableOutput('table'))
    ),
    value = 5,
    gt_output(outputId = "Download"),
    height = px(1600),
    width = px(1600)
  )
)


ui <- navbarPage(
  title =  "Denim Health",
  id = "tabs", # must give id here to add/remove tabs in server
  collapsible = TRUE,
  login_tab,
  theme = shinythemes::shinytheme("flatly"),
  header = tagList(
    useShinydashboard()
  )
)

server <- function(input, output, session) {
  # hack to add the log out button to the navbar on app launch 
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class="nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout")
        )
      )
    )
  )
  
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      # remove the login tab
      removeTab("tabs", "login")
      # add home tab 
      appendTab("tabs", home_tab, select = TRUE)
      # render user data output
      
      output$OverallPlot <- renderPlot({
        chs_chn_ip_overall%>%
          ggplot(aes(x = `# Tasks`, y = fct_reorder(Assignee, `# Tasks`))) +
          geom_col(fill = "#DC5F00", width = .1) +
          geom_text(
            aes(x = 0, y = fct_reorder(Assignee, `# Tasks`),  label = Assignee),
            hjust = 0,
            vjust = 0,
            nudge_y = 0.1,
            color = 'grey30',
            fontface = 'bold',
            size = 4
          ) +
          geom_text(
            aes(x = `# Tasks`, y = fct_reorder(Assignee, `# Tasks`), label = `# Tasks`),
            hjust = 1,
            vjust = 0,
            nudge_y = 0.1,
            nudge_x = 0.02,
            color = 'grey30',
            fontface = 'bold',
            size = 4
          )+
          # geom_text(aes(x = 0, y = fct_reorder(assignee, `# Tasks`),  label = assignee),
          #           color = "black", hjust = 0, position = position_nudge(y = 0.3),
          #           fontface = "bold", family = "Montserrat", size = 4) +
          # 
          # geom_text(aes(x = `# Tasks`, y = fct_reorder(assignee, `# Tasks`), label = `# Tasks`),
          #           color = "#373A40", hjust = 1, position = position_nudge(x = -.02, y = 0.3),
          #           fontface = "bold", family = "Montserrat", size = 4) +
          ggtitle(paste0("# Active Tasks (Inclusive of Tasks in Review) : ",max(chs_chn_ip$Sprint)))+
          theme_minimal() +
          theme(
            axis.text = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()) +
          
          labs(y = "",
               x = "")+
          # title = "# of Tasks Estimated") +
          scale_x_continuous(
            breaks = scales::breaks_pretty(n = 10))
        
      })
      
      output$Plot <- renderPlot({
        chs_chn_esti%>%
          ggplot(aes(x=reorder(Assignee,-Values),y=Values,fill=Type))+
          geom_bar(stat="identity", position=position_dodge())+
          coord_flip()+
          geom_text(aes(label=Values), vjust=1.21,hjust=1.5, color="white",
                    position = position_dodge(1), size=3.5)+
          scale_fill_manual(values = c('#088395', '#E69F00'))+
          # scale_fill_brewer(palette="paired")+
          theme_minimal()+
          theme(
            legend.position = 'none',
            plot.title = element_markdown(size = 15))+
          labs(x="",y="",
               title=title_text)
        
      })
      # add data tab
      appendTab("tabs", Act_Target_Tab)
      # render data tab title and table depending on permissions
      # user_permission <- credentials()$info$permissions
      # if (user_permission == "admin") {
      output$ResPR <-  render_gt({
        #Added march 7th for filtering only status with open
        sp_tkt_Tf %>% 
          
          mutate(Actual_Start = as.Date(Actual_Start, format = "%d-%m-%Y %M:%S")) %>%
          mutate(Actual_Finish = as.Date(Actual_Finish, format = "%d-%m-%Y %M:%S")) %>%
          
          # mutate(Actual_Start = as.Date(ifelse(is.na(format_dmy2),
          #                                      format_dmy,format_dmy2), origin = "01-01-1970"))%>%
          # mutate(Actual_Finish = as.Date(ifelse(is.na(format_dmy2_af),
          #                                       format_dmy_af,format_dmy2_af), origin = "01-01-1970"))%>%
          mutate(Duration = as.numeric(Actual_Finish - Actual_Start),
                 Target = 10) %>%
          #dplyr::mutate(Total_Days = Total_Days/max(Total_Days) * 100)%>%
          mutate(Total_Days = Duration) %>%
          dplyr::select(Task, Assignee, Duration, Status, Total_Days, Target,Sprint) %>%
          filter(Sprint == input$dataset2)%>%
          filter(Assignee == input$dataset1) %>%
          #group_by(Assignee)%>%
          gt() %>%
          #gt(groupname_col = "Assignee")%>%
          gt_plt_bullet(
            column = Total_Days,
            target = Target,
            width = 45,
            # color = "#ffb200", FF004D = "#00008B")%>%
            palette = c("#2D3250", "#FF004D")
          ) %>%
          gt_theme_nytimes() %>%
          #  gt(groupname_col = "Assignee")%>%
          cols_label(
            Total_Days = html(
              "<span style='color:#2D3250;'>Actual</span> vs <span style='color:#FF004D;'><b>Target</b></span>"
            )
          ) %>%
          tab_header(
            title = "Total Duration of the stories completed in the sprint",
            subtitle = md(
              "<span style='color:#FF004D;'><b>Sprint Target</b></span> which is 10 days "
            )
          )
        
      })
      output$InProgressTotal = renderValueBox({
        
        valueBox(
          df_ip,
          subtitle='In-Progress',
          color = 'yellow',
          icon = icon("bars-progress")
        )
        
      })
      
      
      output$Blocked = renderValueBox({
        
        valueBox(
          df_blocked,
          subtitle='Blocked',
          color = 'red',
          icon = icon("lock")
        )
        
      })
      
      
      output$Completed = renderValueBox({
        
        valueBox(
          df_completed,
          subtitle='Completed',
          color = 'green',
          icon = icon("flag-checkered")
        )
        
      })
      
      
      
      
      output$InReview = renderValueBox({
        
        valueBox(
          df_review,
          subtitle='In Review',
          color = 'light-blue',
          icon = icon("magnifying-glass")
        )
        
      })
      
      
      
      output$YTS = renderValueBox({
        
        valueBox(
          df_YTS ,
          subtitle='Yet to Start',
          color = 'light-blue',
          icon = icon("hourglass-start")
        )
        
      })
      
      output$TaskTotal = renderValueBox({
        
        valueBox(
          df_tot,
          subtitle='Total Tasks',
          color = 'light-blue',
          icon = icon("bullseye"))
        
      })
      
      
      
      ##PR Report
      appendTab("tabs", PR_Tab)
      
      
      output$PR1 <-  renderReactable({
        pr_percent %>%
          
          dplyr::select(Assignee, Sprint, "# Stories Worked" = Tickets_Completed, everything()) %>%
          filter(Sprint==input$sprint1)%>%
          reactable(
            theme = fivethirtyeight(),
            columnGroups = list(colGroup(
              name = "PR Categories",
              columns = c("ReWork_PR", "Clean_PR")
            )),
            defaultColDef = colDef(maxWidth = 150, format = colFormat(digits = 0)),
            columns = list(
              "Assignee" = colDef(maxWidth = 110),
              "Success_Rate" = colDef(
                format = colFormat(
                  digits = 2,
                  suffix = " %",
                  separators = TRUE
                ),
                style = color_scales(., colors = pal1)
              ),
              #"Donated to Pride"=colDef(maxWidth = 100, align="center", cell=pill_buttons(.,color_ref = "color_don", opacity = 0.8)),
              "# Stories Worked" = colDef(
                format = colFormat(
                  digits = 0,
                  suffix = " Stories",
                  separators = TRUE
                ),
                style = list(borderRight = "1px solid #777")
              ),
              color_don = colDef(show = FALSE),
              color_hrc = colDef(show = FALSE),
              # "Clean_PR"=colDef(align="center", cell=icon_assign(.,icon="circle",fill_color = "black")),
              "Clean_PR" = colDef(cell = icon_assign(
                ., icon = "circle-check", fill_color = "#8B322C"
              )),
              "ReWork_PR" = colDef(align = "center", cell = data_bars(., force_outside = c(0, 30)))
            )
          ) %>%
          google_font("Roboto", font_weight = 300) %>%
          add_title(title = "Peer Review Report", font_size = 20) %>%
          add_subtitle(
            subtitle = "Report shows the success rate of the PRs without re-work - Our Target is to achieve minimal or no Re-work",
            font_size = 14,
            font_weight = "normal",
            margin = reactablefmtr::margin(
              t = 10,
              r = 20,
              b = 15,
              l = 0
            )
          )
        
      })
      
      #Sprint EE Correlation
      appendTab("tabs", Sprint_ee_Tab)
      output$SPEEReport <- renderPlot({
        sp_tkt_Tf%>%
          mutate(sprint_num = parse_number(Sprint))%>%
          #group_by(sprint_num)%>%
          filter(sprint_num %in% c(max(sprint_num),max(sprint_num)-1,max(sprint_num)-2))%>%
          #filter(Sprint %in% c("Denim Sprint 14","Denim Sprint 15","Denim Sprint 16"))%>%
          # group_by(Sprint)%>%
          select(Sprint,Expended_effort,story_point_estimate)%>%
          ggplot(aes(x = story_point_estimate, y = Expended_effort))+
          geom_point(aes(color = Sprint, shape = Sprint)) +
          geom_rug(aes(color =Sprint)) +
          geom_smooth(aes(color = Sprint), method = lm, 
                      se = FALSE,fullrange = TRUE)+
          scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
          ggpubr::stat_cor(aes(color = Sprint), label.x = 3)+
          ggtitle(paste0("Story Point Vs Expended Effort Correlation of last 3 completed sprints "))
        
      })
      ####
      ##On Going Tasks
      appendTab("tabs", OG_Tab)
      output$OverallSummaryReport <-   render_gt({
        #Added march 7th for filtering only status with open
        chs_chn_ip %>%
          group_by(Assignee) %>%
          dplyr::filter(Status %in% c('Yet to Start','In Progress'))%>%
          gt() %>%
          tab_stubhead("label") %>%
          gt_theme_538() %>%
          ##Start Remove
          data_color(
            # Update cell colors...
            columns = c('Assignee'),
            # ...for supp column!
            colors = scales::col_factor(
              # <- bc it's a factor
              palette = c("#9AC8CD", "#1679AB"),
              # Two factor levels, two colors
              domain = c("Divyum", "Sandeep Rao")# Levels
            )
          ) %>%
          tab_style(
            style = cell_text(color = "black", weight = "bold"),
            locations = list(cells_row_groups(), cells_column_labels(everything()))
          ) %>%
          tab_style(style = cell_fill(color = "bisque"),
                    locations = cells_row_groups(groups = "Ayushi Agarwal")) |>
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_row_groups(groups = "Himanshu Pant")
          ) %>%
          tab_style(
            style = cell_fill(color = "lightgreen"),
            locations = cells_row_groups(groups = "Sandeep Rao")
          )%>%
          tab_style(
            style = cell_fill(color = "#A7E6FF"),
            locations = cells_row_groups(groups = "Divyum")
          )%>%
          row_group_order(groups = c("Sandeep Rao", "Himanshu Pant","Divyum"))%>%
          
          ##Remove
          tab_style(
            style = list(
              cell_fill(color = "#EE4E4E"),
              cell_text(weight = "lighter")
            ),
            locations = cells_body(
              columns = Status,
              rows = Status== "Blocked"
            )
          )
        
        
      })
      
      #Idle tab
      
      
      appendTab("tabs", Idle_Tab)
      
      output$IdleReport <-   render_gt({
        #Added march 7th for filtering only status with open
        tb1 <- idle_tkt %>%
          #  dplyr::filter(status == "Completed") %>%
          gt() %>%
          gt_theme_nytimes() 
        # data_color(
        #   # Update cell colors...
        #   columns = c('assignee'),
        #   # ...for supp column!
        #   colors = scales::col_factor(
        #     # <- bc it's a factor
        #     palette = c("#9AC8CD", "#1679AB"),
        #     # Two factor levels, two colors
        #     domain = c("Divyum", "Sandeep Rao")# Levels
        #   )
        # )
        # 
        
      })
      
      
      ##Sprint SPillover
      appendTab("tabs", Sprint_Tab)
      
      output$SpilloverReport <-  renderPlot({
        
        
        # Creating the calendar with a legend
        calendR(year = 2024,start=c("S","M"),weeknames.size = 7.5,months.size = 10.5,day.size = 3.5,
                # orientation = "p",
                special.days = events,
                special.col = c("pink", "lightblue","lightgreen", "#EB8317"
                ),
                legend.pos = "right"
        ) +
          theme(
            legend.text = element_text(size = 12))# Legend to the right
      })
      
    }
  })
}
#   


shinyApp(ui, server)
