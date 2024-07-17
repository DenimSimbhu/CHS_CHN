library(tidyverse)
library(gt)
library(janitor)
library(shiny)
#library(shinydashboard)
library(googlesheets4)
library(rmarkdown)
#library(DT)
library(scales)
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
#library(janitor)
library(tidyr)
#library(htmlTable)
#library(htmltools)
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
  "Comparing   <span style = 'color:{color_palette['Actual']}'> <b>Expended Effort</b> </span> Vs <span style = 'color:{color_palette['Estimate']}'><b>Story Point Estimate</b></span>"
)

pr_percent_overall <- pr_percent %>%
  group_by(Assignee)%>%
  summarise(Sprint="Overall",Tickets_Completed=sum(Tickets_Completed),ReWork_PR=sum(ReWork_PR),
            Clean_PR= sum(Clean_PR),Success_Rate=(Clean_PR/Tickets_Completed)*100 )



pr_percent <- rbind(pr_percent,pr_percent_overall)


#chs_chn_ip <- read_csv("CHS_CHN_IP.csv")

chs_chn_ip <- chs_chn_ip %>%
  clean_names() %>%
  filter(!is.na(status))%>%
  dplyr::select(
    Task = key ,
    Summary = summary,
    Assignee = assignee,
    Sprint = sprint,
    Status = status,
    story_point_estimate=story_point_estimate,
    expended_effort=expended_effort
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
    Estimate = case_when(
      story_point_estimate <= 3 ~ 1,
      story_point_estimate <= 6 & story_point_estimate > 2  ~ 2,
      #  custom_field_story_point_estimate <= 6 & custom_field_story_point_estimate > 4  ~ 3,
      story_point_estimate <= 8 & story_point_estimate > 6  ~ 3,
      TRUE ~ 4
    )
  )

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
  dplyr::select(
    Task = key ,
    Summary = summary,
    Assignee = assignee,
    Actual_Start = chart_date_of_first_response,
    Actual_Finish = updated,
    Sprint = sprint,
    Status = resolution
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

chs_chn_ip_story <- chs_chn_ip%>%
  dplyr::mutate(
    Estimate = story_point_estimate/2)%>%
  group_by(Assignee)%>%
  filter(Sprint==max(Sprint))%>%
  summarise(Days_Wrk = sum(Estimate))

chs_chn_esti<-chs_chn_ip%>%
  dplyr::mutate(
    Estimate = story_point_estimate/2,Actual = expended_effort/2 )%>%
  group_by(Assignee)%>%
  summarise(Estimate = sum(Estimate),Actual= sum(Actual,na.rm = TRUE))%>%
  pivot_longer(cols=Estimate:Actual,names_to = "Type",values_to = "Values")
orange_pal <- function(x)
  rgb(colorRamp(c("#6DB9EF", "#F3B664"))(x), maxColorValue = 255)


ui <- navbarPage(
  "Denim Health CHS Dash",
  theme = shinythemes::shinytheme("flatly"),
  header = tagList(
    useShinydashboard()
  ),
  tabPanel("OverallSummary",fluidRow(
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
         ))),
          #  shinycssloaders::withSpinner(plotOutput("OverallPlot"))),
  
  
  # tabPanel("Completed Tasks"
  #          , shinycssloaders::withSpinner(gt_output("CompletedReport"))),
  tabPanel(title = "Actual vs Target (Task Duration)", sidebarLayout(
    sidebarPanel(
      selectInput(
        "dataset2",
        label = "Sprint",
        choices = unique(sp_tkt_Tf$Sprint)
        # selected = "Himanshu Pant"
      ),
      selectInput(
        "dataset1",
        label = "Resources",
        choices = unique(sp_tkt_Tf$Assignee),
        selected = "Himanshu Pant"
      )
    ), mainPanel(gt_output(outputId = "ResPR"))))
  ,
  tabPanel("Sprint Spillover",sidebarLayout( sidebarPanel(
    selectInput(
      "SpillSprint",
      label = "Sprint",
      choices = unique(Tasks_CD_tf$Sprint)
      # selected = "Himanshu Pant"
    )) ,shinycssloaders::withSpinner(reactableOutput("SpilloverReport")))),
  tabPanel("PR Report" ,sidebarLayout( sidebarPanel(
    selectInput(
      "sprint1",
      label = "Sprint",
      choices = unique(pr_percent$Sprint)
      # selected = "Himanshu Pant"
    )) ,mainPanel(reactableOutput("PR1")))),
  tabPanel("On-Going Tasks"
           , shinycssloaders::withSpinner(gt_output(
             "OverallSummaryReport"
           ))),
  tabPanel("Idle >2Days "
           , shinycssloaders::withSpinner(gt_output(
             "IdleReport"
           ))),
  navbarMenu(
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
)



server <- function(input, output, session) {
  datasetname <- reactiveVal(unique(chs_chn_tf$Assignee)[1])
  
  observeEvent(input$dataset1, {
    updateSelectInput(session, "dataset1", selected = input$dataset1)
    datasetname(input$dataset1)
  })

  
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
      ggtitle(paste0("# Active Tasks : ",max(chs_chn_ip$Sprint)))+
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
  
  
  ####Plot2 of frist page
  output$Plot <- renderPlot({
    chs_chn_ip_story%>%
      ggplot(aes(x = Days_Wrk, y = fct_reorder(Assignee, Days_Wrk)))+
      geom_col(fill = "grey", width = .4) +
      geom_point(shape = 21, fill = "orange", color = "black", size = 15, stroke = 1) +
      geom_text(aes(x = Days_Wrk, y = fct_reorder(Assignee, Days_Wrk), label = Days_Wrk),
                color = "black", hjust = 0.5, 
                fontface = "bold") +
      ggtitle(paste0("StoryPoint Estimate (in Days )"))+
      theme_minimal() +
      theme(text = element_text(family = "Nunito Bold", face = "bold", size = 14),
            axis.text = element_text(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title.x = element_text(hjust = 0)) +
      labs(y = "",
           x = "")
    
  })
  ####Plot2 end
  output$OverallSummaryReport <-   render_gt({
    #Added march 7th for filtering only status with open
    chs_chn_ip %>%
      group_by(Assignee) %>%
      dplyr::filter(Status %in% c('Yet to Start','In Progress'))%>%
      gt() %>%
      tab_stubhead("label") %>%
      gt_theme_538() %>%
      # data_color(
      #   # Update cell colors...
      #   columns = c('Assignee'),
      #   # ...for supp column!
      #   colors = scales::col_factor(
      #     # <- bc it's a factor
      #     palette = c("#9AC8CD", "#1679AB"),
      #     # Two factor levels, two colors
      #     domain = c("Divyum", "Sandeep Rao")# Levels
      #   )
      # ) %>%
      # tab_style(
      #   style = cell_text(color = "black", weight = "bold"),
      #   locations = list(cells_row_groups(), cells_column_labels(everything()))
      # ) %>%
      # tab_style(style = cell_fill(color = "bisque"),
      #           locations = cells_row_groups(groups = 1)) |>
      # tab_style(
      #   style = cell_fill(color = "lightgreen"),
      #   locations = cells_row_groups(groups = "Himanshu Pant")
      # ) %>%
      # tab_style(
      #   style = cell_fill(color = "lightgreen"),
      #   locations = cells_row_groups(groups = "Sandeep Rao")
      # )%>%
      # tab_style(
      #   style = cell_fill(color = "#A7E6FF"),
      #   locations = cells_row_groups(groups = "Divyum")
      # )%>%
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
  
  
  
  output$CompletedReport <-   render_gt({
    #Added march 7th for filtering only status with open
    tb1 <- chs_chn_tfc %>%
      dplyr::filter(status == "Completed") %>%
      gt() %>%
      gt_theme_nytimes() %>%
      data_color(
        # Update cell colors...
        columns = c('assignee'),
        # ...for supp column!
        colors = scales::col_factor(
          # <- bc it's a factor
          palette = c("#9AC8CD", "#1679AB"),
          # Two factor levels, two colors
          domain = c("Divyum", "Sandeep Rao")# Levels
        )
      )
    
    
  })
  
  
  ###idle Report
  
  
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
  
  ###ACtar
  
  output$AcTar <-   render_gt({
    #Added march 7th for filtering only status with open
    sp_tkt_Tf %>%
      mutate(Actual_Start = as.Date(ifelse(
        is.na(format_dmy2), format_dmy, format_dmy2
      ), origin = "1970-01-01")) %>%
      mutate(Actual_Finish = as.Date(ifelse(
        is.na(format_dmy2_af), format_dmy_af, format_dmy2_af
      ), origin = "1970-01-01")) %>%
      mutate(Duration = as.numeric(Actual_Finish - Actual_Start),
             Target = 10) %>%
      #dplyr::mutate(Total_Days = Total_Days/max(Total_Days) * 100)%>%
      mutate(Total_Days = Duration) %>%
      dplyr::select(Task, Assignee, Duration, Status, Total_Days, Target) %>%
      #group_by(Assignee)%>%
      #  gt() %>%
      gt(groupname_col = "Assignee") %>%
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
  
  ##Test
  output$Estimate <- renderPlot({
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
        plot.title = element_markdown(size = 18))+
      labs(x="",y="",
           title=title_text)
    
  })
  ####
  
  output$SpilloverReport <-  renderReactable({
    
    reactdata %>%
      filter(Sprint==input$SpillSprint)%>%
      
      reactable( pagination = FALSE,
                 compact = TRUE,
                 borderless = FALSE,
                 striped = FALSE,
                 fullWidth = FALSE,
                 defaultColDef = colDef(
                   align = "center",
                   minWidth = 220 ),
                 #    reactdata,
                 theme = reactableTheme(
                   headerStyle = list(
                     "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                     "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                     borderColor = "#555"
                   )
                 ),
                 columnGroups = list(colGroup(
                   name = "Sprint Spillover Report",
                   columns = c("Spillover", "Total")
                 )),  details = function(index) {
                   plant_data <- Tasks_CD_tf[Tasks_CD_tf$Spillover == reactdata$Spillover[index], ]
                   plant_data <- plant_data %>% filter(Sprint==input$SpillSprint)%>%select(ID,Summary,Assignee,Sprint)
                   htmltools::div(style = "padding: 1rem",
                                  reactable(plant_data, theme = reactableTheme(
                                    headerStyle = list(
                                      "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                                      borderColor = "#555"
                                    )
                                  ), outlined = TRUE)
                   )
                 })
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
  
}





shinyApp(ui = ui, server = server)

