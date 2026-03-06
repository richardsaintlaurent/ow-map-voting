library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggh4x)
library(ggrepel)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

gamemodes <- c('Quick Play', 'Competitive')
teamsizes <- c('6v6','5v5')
tier_colors <- {
  c(
    "S" = "#FE7D7D",
    "A" = "#FFBE7E",
    "B" = "#FCFF80",
    "C" = "#7DFF7D",
    "D" = "#7EBFFC",
    "E" = "#7E7EFD",
    "F" = "#FE7FFB"
  )
}

##Function
myApp <- function() {
  ui <- dashboardPage(
    dashboardHeader(title = "OW Map Voting"),
    dashboardSidebar(
      #ACTION BUTTONS
      h4(icon("gavel"), "Actions", style="text-align:center;"),
      column(
        12,
        align = "center", 
        actionButton("refresh", "Refresh", icon = icon("sync"))
      ),
      tags$div(style = "margin-bottom: 75px;"), # Force empty space
      #FILTERS
      h4(icon("filter"), "Filters", style="text-align:center;"),
      box(title = "Players", status = "primary", width = 12, solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE, background = "gray",
          actionButton("player_selectall", "Select All"),
          checkboxGroupInput(
            "selected_players",
            "Selected Players:",
            choices = NULL
          )
      ),
      box(title = "Maps", status = "primary", width = 12, solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE, background = "gray",
          actionButton("map_selectall", "Select All"),
          checkboxGroupInput(
            inputId = "selected_maps",
            label = "Selected Maps:",
            choices = NULL
          )
      ),
      box(title = "Game Mode", status = "primary", width = 12, solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE, background = "gray",
          checkboxGroupInput(
            inputId = "selected_gamemodes",
            label = "Selected Modes:",
            choices = gamemodes,
            selected = gamemodes
          )
      ),
      box(title = "Team Size", status = "primary", width = 12, solidHeader = TRUE,
          collapsible = TRUE, collapsed = TRUE, background = "gray",
          checkboxGroupInput(
            inputId = "selected_teamsize",
            label = "Selected Team Sizes:",
            choices = teamsizes,
            selected = teamsizes
          )
      )
    ),
    dashboardBody(
      fluidRow(
        valueBoxOutput("total_entries"),
        valueBoxOutput("tiptoucher"),
        valueBoxOutput("winrate")
      ),
      # Row1: Map Rankings
      fluidRow(
        box(title = "Map Rankings Table",
            solidHeader = T,
            width = 4, 
            collapsible = T,
            div(DT::DTOutput("ranking_display"), style = "font-size: 100%", height = 400)),
        box(title = "Map Tier List", solidHeader = T,
            width = 8, 
            collapsible = T,
            plotOutput("plot_tierlist",  height = 400))
      ),
      # Row2: Winrate summary
      fluidRow(
        box(title = "Winrate Table",
            solidHeader = T,
            width = 4, 
            collapsible = T,
            div(DT::DTOutput("table_wr"), style = "font-size: 100%", height = 400)),
        box(title = "Winrate Summary",
            solidHeader = T,
            width = 8, 
            collapsible = T,
            plotOutput("plot_wr", height = 400))
      ),
      # Row3: Occurrences Summary
      fluidRow(
        box(title = "Occurrences Summary",
            solidHeader = T,
            width = 4, 
            collapsible = T,
            div(DT::DTOutput("table_oo"), style = "font-size: 100%", height = 400)),
        box(title = "Occurrences vs Score", solidHeader = T,
            width = 8, 
            collapsible = T,
            plotOutput("plot_oo", height = 400))
      ),
      # Row4: Matrices
      fluidRow(
        box(title = "Map Ranking Matrix",
            solidHeader = T,
            width = 6, 
            collapsible = T,
            plotOutput("plot_matrix", height = 600)),
        box(title = "Map Pairwise Counts", solidHeader = T,
            width = 6, 
            collapsible = T,
            plotOutput("plot_matrix_labels", height = 600))
      ),
      # Row5: Raw data
      fluidRow(
        box(title = "Raw Data",
            solidHeader = T,
            width = 12, 
            collapsible = T,
            div(DT::DTOutput("clean_dt"), style = "font-family: Consolas, monospace; font-size: 100%;"))
      )
    ) 
  )
  server <- function(input, output, session) {
    
    # Raw data
    filtered_raw <- reactive({
      raw <- {
        read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSHToeks_abfFEHi4bZNsJZlRr2f4NMveAn9-JYvpRvPRzAg55Za5tHtNN85ilo2YPgoTqz1yc5WQp7/pub?output=csv')
      }
      raw[raw == ""] <- NA
      raw
    })
    
    #Determine statistically relevant lists
    players <- reactive({
      ppl <- fill(filtered_raw(), ID:DATE, RESULT, MATCH, TIPTOUCHER, NOTES, .direction = "down") %>% 
        dplyr::select(-NOTES) %>%
        dplyr::filter(nchar(ID)==28) %>%
        dplyr::select(ID,PLAYER) %>%
        unique() %>%
        group_by(PLAYER) %>%
        summarize(N = n()) %>%
        dplyr::filter(N>=100) %>%
        .$PLAYER
      
      c(ppl, 'Others')
    })
    maps <- reactive({
      maplist <- fill(filtered_raw(), ID:DATE, RESULT, MATCH, TIPTOUCHER, NOTES, .direction = "down") %>% 
        dplyr::select(-NOTES) %>%
        dplyr::filter(nchar(ID)==28) %>%
        pivot_longer(cols=c(MAP1,MAP2,MAP3), values_to = 'VOTE', names_to = 'SLOT') %>%
        pivot_wider(names_from = ATTRIBUTE, values_from = VOTE) %>%
        mutate(
          map = case_when(
            grepl('Bliz', map) ~ 'Blizzard World',
            grepl('Colo', map) ~ 'Colosseo',
            grepl('Esper', map) ~ 'Esperanca',
            grepl('King', map) ~ "King's Row",
            grepl('New Ju', map, ignore.case=T) ~ 'New Junk City',
            grepl('New Qu', map) ~ 'New Queen Street',
            grepl('Numb', map) ~ 'Numbani',
            grepl('Oasis', map, ignore.case=T) ~ 'Oasis',
            grepl('Pariso|Para', map) ~ 'Paraiso',
            grepl('sapi', map) ~ 'Runasapi',
            grepl('Samoa', map, ignore.case=T) ~ 'Samoa',
            grepl('Shamb|Samb', map, ignore.case=T) ~ 'Shambali Monastery',
            grepl('vasa', map) ~ 'Suravasa',
            grepl('Watchpoint', map) ~ 'Gibraltar',
            grepl('Royal', map) ~ 'Circuit Royal',
            grepl('walde', map) ~ 'Eichenwalde',
            TRUE ~ map
          )
        ) %>%
        .$map %>%
        unique() %>%
        sort(.)
      
      maplist[!maplist %in% c('Hanamura','Temple of Anubis')]
      
    })
    
    # Clean data
    filtered_clean <- reactive({
      # Filter requirements
      req(input$selected_players,
          input$selected_maps,
          input$selected_gamemodes,
          input$selected_teamsize)
      # Cleanup
      clean <- {
        fill(filtered_raw(), ID:DATE, RESULT, MATCH, TIPTOUCHER, NOTES, .direction = "down") %>% 
          dplyr::select(-NOTES) %>%
          dplyr::filter(nchar(ID)==28) %>%
          mutate(
            PLAYER = case_when(
              PLAYER %in% players() ~ PLAYER,
              TRUE ~ 'Others'
            ),
            MODE = case_when(
              grepl('Quick', MODE, ignore.case=T) ~ 'Quick Play',
              grepl('Comp', MODE, ignore.case=T) ~ 'Competitive',
              TRUE ~ NA
            ),
            TEAMSIZE = case_when(
              grepl('6', TEAMSIZE) ~ '6v6',
              grepl('5', TEAMSIZE) ~ '5v5',
              TRUE ~ NA
            ),
            RESULT = case_when(
              grepl('Bliz', RESULT) ~ 'Blizzard World',
              grepl('Colo', RESULT) ~ 'Colosseo',
              grepl('Esper', RESULT) ~ 'Esperanca',
              grepl('King', RESULT) ~ "King's Row",
              grepl('New Ju', RESULT, ignore.case=T) ~ 'New Junk City',
              grepl('New Qu', RESULT) ~ 'New Queen Street',
              grepl('Numb', RESULT) ~ 'Numbani',
              grepl('Oasis', RESULT, ignore.case=T) ~ 'Oasis',
              grepl('Pariso|Para', RESULT) ~ 'Paraiso',
              grepl('sapi', RESULT) ~ 'Runasapi',
              grepl('Samoa', RESULT, ignore.case=T) ~ 'Samoa',
              grepl('Shamb|Samb', RESULT, ignore.case=T) ~ 'Shambali Monastery',
              grepl('vasa', RESULT) ~ 'Suravasa',
              grepl('Watchpoint', RESULT) ~ 'Gibraltar',
              grepl('Royal', RESULT) ~ 'Circuit Royal',
              grepl('alde', RESULT) ~ 'Eichenwalde',
              grepl('ios', RESULT) ~ 'Ilios',
              TRUE ~ RESULT
            )
          ) %>%
          na.omit()
      }
      # Apply reactive filters
      clean %>%
        dplyr::filter(
          PLAYER %in% input$selected_players,
          RESULT %in% input$selected_maps,
          MODE %in% input$selected_gamemodes,
          TEAMSIZE %in% input$selected_teamsize
        )
    })
    
    #Update sidebar
    observeEvent(filtered_raw(), {
      
      updateCheckboxGroupInput(
        session,
        "selected_players",
        choices = players(),
        selected = players()
      )
      
      updateCheckboxGroupInput(
        session,
        "selected_maps",
        choices = maps(),
        selected = maps()[!maps() %in% c('Hanaoka','Throne of Anubis')]
      )
      
    }, once = TRUE)
    
    # Map vote object
    filtered_mapvote <- reactive({
      filtered_clean() %>%
        pivot_longer(cols=c(MAP1,MAP2,MAP3),
                     values_to = 'VOTE',
                     names_to = 'SLOT') %>%
        pivot_wider(names_from = ATTRIBUTE,
                    values_from = VOTE) %>%
        mutate(
          vote = gsub(" ", "", vote),
          left_vote = as.numeric(str_split_i(vote, '\\+', 1)),
          right_vote = as.numeric(str_split_i(vote, '\\+', 2)),
          vote = left_vote + right_vote
        ) %>%
        mutate(
          map = case_when(
            grepl('Bliz', map) ~ 'Blizzard World',
            grepl('Colo', map) ~ 'Colosseo',
            grepl('Esper', map) ~ 'Esperanca',
            grepl('King', map) ~ "King's Row",
            grepl('New Ju', map, ignore.case=T) ~ 'New Junk City',
            grepl('New Qu', map) ~ 'New Queen Street',
            grepl('Numb', map) ~ 'Numbani',
            grepl('Oasis', map, ignore.case=T) ~ 'Oasis',
            grepl('Pariso|Para', map) ~ 'Paraiso',
            grepl('sapi', map) ~ 'Runasapi',
            grepl('Samoa', map, ignore.case=T) ~ 'Samoa',
            grepl('Shamb|Samb', map, ignore.case=T) ~ 'Shambali Monastery',
            grepl('vasa', map) ~ 'Suravasa',
            grepl('Watchpoint', map) ~ 'Gibraltar',
            grepl('Royal', map) ~ 'Circuit Royal',
            grepl('walde', map) ~ 'Eichenwalde',
            TRUE ~ map
          )
        ) %>%
        dplyr::filter(map %in% input$selected_maps)
    })
    
    # Textbox data
    total_entries <- reactive({
      filtered_mapvote() %>%
        dplyr::select(ID) %>%
        unique() %>%
        nrow()
    }) #total entries
    tt_ruined <- reactive({
      filtered_mapvote() %>% 
        dplyr::select(ID,TIPTOUCHER) %>%
        unique() %>%
        dplyr::filter(TIPTOUCHER==TRUE) %>%
        nrow()
    }) #tip tainted
    winrate <- reactive({
      filtered_mapvote() %>% 
        dplyr::select(ID,MATCH) %>%
        unique() %>%
        mutate(
          MATCH = case_when(
            MATCH == 'WIN' ~ 1,
            MATCH == 'LOSS' ~ 0,
            MATCH == 'DRAW' ~ 0,
            MATCH == 'CANCELLED' ~ NA,
            TRUE ~ NA
          )
        ) %>%
        na.omit() %>%
        summarize(
          wr = round(100*sum(MATCH)/n(),1)
        ) %>%
        .$wr
    }) #winrate
    
    # Appearance statistics
    appearance <- reactive({
      filtered_mapvote() %>%
        group_by(map) %>%
        summarize(O = n())
    })
    popularity <- reactive({
      filtered_mapvote() %>%
        group_by(ID) %>%
        mutate(
          total = sum(vote),
          total_l = sum(left_vote),
          total_r = sum(right_vote)
        ) %>% 
        group_by(ID, map) %>%
        summarize(
          prop = vote/total,
          left = left_vote/total_l,
          right = right_vote/total_r
        ) %>% 
        group_by(map) %>%
        summarize(
          all = mean(prop, na.rm=T),
          all_sd = sd(prop, na.rm=T),
          L = mean(left, na.rm=T),
          L_sd = sd(left, na.rm=T),
          R = mean(right, na.rm=T),
          R_sd = sd(right, na.rm=T)
        )
    })
    
    # Matrices
    pairwise_matrices <- reactive({
      
      mv <- filtered_mapvote()
      req(nrow(mv) > 0)
      
      maps <- input$selected_maps
      
      rank_matrix <- matrix(
        0,
        nrow = length(maps),
        ncol = length(maps),
        dimnames = list(maps, maps)
      )
      
      count_matrix <- rank_matrix
      
      # split dataset by vote event
      events <- split(mv, mv$ID)
      
      for (event in events) {
        
        # keep needed columns
        df <- event[, c("map", "vote")]
        
        # remove missing votes
        df <- df[!is.na(df$vote), ]
        
        if (nrow(df) < 2) next
        
        # create pairwise combinations
        pairs <- combn(nrow(df), 2)
        
        for (i in seq_len(ncol(pairs))) {
          
          i1 <- pairs[1, i]
          i2 <- pairs[2, i]
          
          map1 <- df$map[i1]
          map2 <- df$map[i2]
          
          v1 <- df$vote[i1]
          v2 <- df$vote[i2]
          
          # record comparison occurrence
          count_matrix[map1, map2] <- count_matrix[map1, map2] + 1
          count_matrix[map2, map1] <- count_matrix[map2, map1] + 1
          
          if (v1 > v2) {
            
            rank_matrix[map1, map2] <- rank_matrix[map1, map2] + 1
            rank_matrix[map2, map1] <- rank_matrix[map2, map1] - 1
            
          } else if (v1 < v2) {
            
            rank_matrix[map1, map2] <- rank_matrix[map1, map2] - 1
            rank_matrix[map2, map1] <- rank_matrix[map2, map1] + 1
            
          }
          # ties contribute 0
        }
      }
      
      matrix_norm <- rank_matrix / count_matrix
      matrix_norm[is.nan(matrix_norm)] <- NA
      
      list(
        rank_matrix = rank_matrix,
        count_matrix = count_matrix,
        matrix_norm = matrix_norm
      )
    }) #matrices
    rank_table <- reactive({
      
      mats <- pairwise_matrices()
      
      rank_matrix  <- mats$rank_matrix
      count_matrix <- mats$count_matrix
      
      appearance <- filtered_mapvote() %>%
        group_by(map) %>%
        summarize(O = n(), .groups = "drop")
      
      scores <- rowSums(rank_matrix)
      
      data.frame(
        map = names(scores),
        score = as.numeric(scores)
      ) %>%
        left_join(appearance, by = "map") %>%
        mutate(
          score = score / (2 * O),
          tier = case_when(
            score >= 0.75 ~ 'S',
            score < 0.75 & score >= 0.50 ~ 'A',
            score < 0.50 & score >= 0.25 ~ 'B',
            score < 0.25 & score >= 0.00 ~ 'C',
            score < 0.00 & score >= -0.25 ~ 'D',
            score < -0.25 & score >= -0.50 ~ 'E',
            score < -0.50 ~ 'F'
          )
        ) %>%
        dplyr::arrange(-score)
    }) #rank table
    data_tiers <- reactive({
      df <- rank_table() %>%
        group_by(tier) %>%
        dplyr::arrange(desc(score), by_group = TRUE) %>%
        mutate(
          tier = factor(tier, levels = c('S','A','B','C','D','E','F')),
          x = seq(from = 1, to = length(tier), by = 1)
        ) %>%
        ungroup()
      df <- df %>% mutate(map = factor(map, levels = df$map))
    }) #tierlist
    ranking_display <- reactive({
      dplyr::select(data_tiers(), map, tier, score) %>%
        mutate(score = round(score,2))
    }) #DT rankings display
    matrix_plot_data <- reactive({
      pairwise_matrices()$matrix_norm %>%
        as.data.frame() %>%
        mutate(map1 = rownames(.)) %>%
        group_by(map1) %>%
        pivot_longer(cols = -map1, names_to = 'map2', values_to = 'mat') %>%
        merge(., appearance(), by.x = 'map1', by.y = 'map') %>%
        dplyr::filter(map1 %in% c('Temple of Anubis','Hanamura') == F & map2 %in% c('Temple of Anubis','Hanamura') == F) %>%
        mutate(
          map1 = factor(map1, levels = rank_table()$map),
          map2 = factor(map2, levels = rank_table()$map)
        ) %>%
        dplyr::filter(is.na(map1) == F & is.na(map2) == F)
    }) #data for matrix popularity plot
    matrix_plot_labels <- reactive({
      pairwise_matrices()$count_matrix %>%
        as.data.frame() %>%
        mutate(map1 = rownames(.)) %>%
        group_by(map1) %>%
        pivot_longer(cols = -map1, names_to = 'map2', values_to = 'mat') %>%
        merge(., appearance(), by.x = 'map1', by.y = 'map') %>%
        dplyr::filter(map1 %in% c('Temple of Anubis','Hanamura') == F & map2 %in% c('Temple of Anubis','Hanamura') == F) %>%
        mutate(
          map1 = factor(map1, levels = rank_table()$map),
          map2 = factor(map2, levels = rank_table()$map)
        ) %>%
        dplyr::filter(is.na(map1) == F & is.na(map2) == F)
    }) #data for matrix counts plot
    
    # Appearance/popularity
    data_oo <- reactive({
      data <- merge(appearance(), data_tiers(), by=c('map','O')) #%>%
        #dplyr::filter(O >= 10 & map %in% c('Hanaoka','Throne of Anubis') == F)
      oo_min <- min(data$O) - (min(data$O) %% 5)
      oo_max <- max(data$O) + 5 - (max(data$O) %% 5)
      list(data = data,
           oo_min = oo_min,
           oo_max = oo_max)
    }) # Appearance ~ Popularity data
    
    # Winrate
    data_wr <- reactive({
      filtered_mapvote() %>%
        dplyr::select(ID, RESULT, MATCH) %>%
        unique() %>%
        mutate(
          MATCH = case_when(
            MATCH == 'WIN' ~ 1,
            MATCH == 'LOSS' ~ 0,
            MATCH == 'DRAW' ~ 0.5,
            MATCH == 'CANCELLED' ~ NA,
            TRUE ~ NA
          )
        ) %>%
        na.omit() %>%
        ungroup() %>% group_by(RESULT) %>%
        summarize(
          WR = round(sum(MATCH)/n(),2),
          N = n()
        ) %>%
        #dplyr::filter(N>=10) %>%
        dplyr::arrange(desc(WR))
    })
    
    # REFRESH
    observeEvent(input$refresh, {
      session$reload()
    })
    
    #Player select update
    observeEvent(input$player_selectall, {
      # Check if all options are currently selected
      if (length(input$selected_players) == length(players())) {
        # If all are selected, deselect all by setting selected to character(0) or NULL
        updateCheckboxGroupInput(
          session = session,
          inputId = "selected_players",
          selected = character(0)
        )
      } else {
        # If not all are selected (or none are selected), select all
        updateCheckboxGroupInput(
          session = session,
          inputId = "selected_players",
          selected = players()
        )
      }
    })
    #Map select update
    observeEvent(input$map_selectall, {
      # Check if all options are currently selected
      if (length(input$selected_maps) == length(maps())) {
        # If all are selected, deselect all by setting selected to character(0) or NULL
        updateCheckboxGroupInput(
          session = session,
          inputId = "selected_maps",
          selected = character(0)
        )
      } else {
        # If not all are selected (or none are selected), select all
        updateCheckboxGroupInput(
          session = session,
          inputId = "selected_maps",
          selected = maps()
        )
      }
    })
    ### Value boxes ###
    # Total Entries
    output$total_entries <- renderValueBox({
      valueBox('Total Entries:', 
               total_entries(), icon = icon("layer-group"), color = "light-blue")
    })
    # TipToucher Games Ruined
    output$tiptoucher <- renderValueBox({
      valueBox('Ruined by TipT:', 
               tt_ruined(), icon = icon("heart-crack"), color = "yellow")
    })
    # Winrate
    output$winrate <- renderValueBox({
      valueBox('Winrate:', 
               paste0(winrate(),'%'), icon = icon("trophy"), color = "red")
    })
    # Map Rankings Table
    output$ranking_display <- DT::renderDataTable(ranking_display(), options = list(pageLength = 100, scrollY = "282"))
    
    #raw data table
    output$clean_dt <- DT::renderDataTable(filtered_clean(), options = list(pageLength = 1000, scrollY = "282"))
    
    # Tierlist plot
    output$plot_tierlist <- renderPlot({ 
      plot_tierlist <- {
        ggplot(data_tiers(), aes(y = tier, x = x, label = str_wrap(map, width = 10))) +
          ggh4x::facet_grid2(
            tier~., scale='free_y', space='free_y', switch = 'y', strip = strip_themed(
              background_y = list(
                element_rect(fill = "#FE7D7D"),
                element_rect(fill = "#FFBE7E"),
                element_rect(fill = "#FCFF80"),
                element_rect(fill = "#7DFF7D"),
                element_rect(fill = "#7EBFFC"),
                element_rect(fill = "#7E7EFD"),
                element_rect(fill = "#FE7FFB")
              )
            )
          ) +
          geom_label(aes(fill = tier)) +
          scale_fill_manual(values = tier_colors) +
          scale_x_continuous(expand=c(0.1,0)) +
          theme_bw(base_size = 14) +
          theme(
            strip.text.y.left = element_text(angle = 0, size = 16, face = 'bold', margin = margin(l = 0.75, r = 0.75, unit = "cm")),
            legend.position = 'none',
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = '#3F3F3F', colour = 'black'),
            panel.spacing = unit(0, "lines"),
          )
      } ; plot_tierlist
    })
    
    # Pairwise matrix
    output$plot_matrix <- renderPlot({ 
      plot_matrix <- {
        matrix_plot_data() %>%
          ggplot(., aes(x = map2, y = fct_rev(map1))) +
          geom_tile(color='darkgray', aes(fill = mat, color = 'NA')) +
          geom_point(aes(size="NA"), shape =NA, colour = "black") +
          #geom_label(data = matrix_plot_labels, aes(label = mat, x = map2, y = fct_rev(map1)), color = 'black', fill = NA, label.size = NA) +
          scale_fill_gradientn(
            colors = rev(paletteer::paletteer_c("ggthemes::Red-Blue Diverging", 30)), 
            name = 'Score', na.value = 'black',
            guide = guide_colorbar(ticks = F, draw.ulim= F, draw.llim = F)
          ) +
          theme_minimal(base_size = 16) +
          labs(x = 'Against These Maps', y = 'Compare These Maps') +
          guides(size=guide_legend("No Data", override.aes=list(shape = 15, size = 7))) +
          theme(
            aspect.ratio=1,
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "right",
            legend.title = element_text(),
            plot.title = element_text(hjust = 0.5, face = "bold"), #Bold title, centered
            plot.subtitle = element_text(hjust = 0.5), #centered subtitle
            panel.grid.major = element_blank(), #Remove major gridmarks
            panel.grid.minor = element_blank(), #Remove minor gridmarks
            strip.background = element_blank(), #Remove facet label background
            strip.text.x = element_text(face = "bold"), #Facet_grid x label bold
            strip.text.y = element_blank(), #Remove facet_grid y label
            axis.line.x = element_blank(), axis.ticks.x = element_blank(),
            axis.line.y = element_blank(), axis.ticks.y = element_blank()
          )
      } ; plot_matrix
    })
    
    # Pairwise matrix counts
    output$plot_matrix_labels <- renderPlot({ 
      plot_matrix_labels <- {
        matrix_plot_labels() %>%
          ggplot(., aes(x = map2, y = fct_rev(map1))) +
          geom_tile(color='darkgray', aes(fill = mat, color = 'NA')) +
          scale_fill_gradientn(
            colors = (paletteer::paletteer_c("ggthemes::Red-Gold", 30)), 
            name = 'Occurrences', na.value = 'black',
            guide = guide_colorbar(ticks = F, draw.ulim= F, draw.llim = F)
          ) +
          theme_minimal(base_size = 16) +
          labs(x = 'Against These Maps', y = 'Compare These Maps') +
          theme(
            aspect.ratio=1,
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "right",
            legend.title = element_text(),
            plot.title = element_text(hjust = 0.5, face = "bold"), #Bold title, centered
            plot.subtitle = element_text(hjust = 0.5), #centered subtitle
            panel.grid.major = element_blank(), #Remove major gridmarks
            panel.grid.minor = element_blank(), #Remove minor gridmarks
            strip.background = element_blank(), #Remove facet label background
            strip.text.x = element_text(face = "bold"), #Facet_grid x label bold
            strip.text.y = element_blank(), #Remove facet_grid y label
            axis.line.x = element_blank(), axis.ticks.x = element_blank(),
            axis.line.y = element_blank(), axis.ticks.y = element_blank()
          )
      } ; plot_matrix_labels
    })
    
    # O vs Score
    output$plot_oo <- renderPlot({ 
      plot_oo <- {
        data_oo()$data %>%
          ggplot(., aes(x = (score), y = (O), label = map)) +
          scale_color_manual(values=tier_colors, name = 'Tier') +
          geom_line(stat='smooth', method='lm', size = 1.75, color = 'gray') +
          geom_label_repel(label.size = NA, size = 4.25, seed = 123, alpha = 0.75, label.padding=0.1, min.segment.length = 0) +
          geom_point(size = 3, aes(color = tier)) +
          scale_x_continuous(limits = c(-1,1), breaks = c(-1.00,-0.75,-0.50,-0.25,0.00,0.25,0.50,0.75,1.00), expand = c(0.01,0)) +
          scale_y_continuous(limits = c(data_oo()$oo_min, data_oo()$oo_max), breaks = seq(data_oo()$oo_min, data_oo()$oo_max, 5)) +
          theme_minimal(base_size = 14) +
          labs(x = 'Score', y = 'Number of Occurrences') +
          guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"), fill = guide_legend(reverse = TRUE)) +
          theme(
            legend.position = "right",
            legend.title = element_text(face='bold', hjust = 0.5),
            plot.title = element_text(hjust = 0.5, face = "bold"), #Bold title, centered
            plot.subtitle = element_text(hjust = 0.5), #centered subtitle
            panel.grid.major = element_blank(), #Remove major gridmarks
            panel.grid.minor = element_blank(), #Remove minor gridmarks
            strip.background = element_blank(), #Remove facet label background
            strip.text.x = element_text(face = "bold"), #Facet_grid x label bold
            strip.text.y = element_blank(), #Remove facet_grid y label
            axis.line.x = element_line(), axis.ticks.x = element_line(),
            axis.line.y = element_line(), axis.ticks.y = element_line()
          )
      } ; plot_oo
    })
    
    # Winrate
    output$plot_wr <- renderPlot({ 
      plot_wr <- {
        data_wr() %>%
          merge(data_oo()$data, by.x = 'RESULT', by.y='map') %>%
          mutate(
            RESULT = factor(RESULT, levels = rev(data_wr()$RESULT)),
            tier = factor(tier, levels = rev(c('S','A','B','C','D','E','F')))
          ) %>%
          ggplot(., aes(x = WR, y = RESULT, label = N, fill = tier)) +
          scale_fill_manual(values=tier_colors, name = 'Tier') +
          geom_bar(stat='identity') +
          geom_vline(xintercept=0.5, linetype = 'longdash', linewidth = 1.25, color = 'black', alpha = 0.5) +
          #geom_text(aes(label = N), stat = "identity", vjust = 0.5, hjust = -0.5) +
          scale_x_continuous(labels = scales::percent, limits = c(0,1), expand = c(0.005,0.005)) +
          theme_minimal(base_size = 14) +
          labs(x = 'Winrate', y = 'Maps') +
          guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both"), fill = guide_legend(reverse = TRUE)) +
          theme(
            legend.position = "right",
            legend.title = element_text(face='bold', hjust = 0.5),
            plot.title = element_text(hjust = 0.5, face = "bold"), #Bold title, centered
            plot.subtitle = element_text(hjust = 0.5), #centered subtitle
            panel.grid.major = element_blank(), #Remove major gridmarks
            panel.grid.minor = element_blank(), #Remove minor gridmarks
            strip.background = element_blank(), #Remove facet label background
            strip.text.x = element_text(face = "bold"), #Facet_grid x label bold
            strip.text.y = element_blank(), #Remove facet_grid y label
            axis.line.x = element_line(), axis.ticks.x = element_line(),
            axis.line.y = element_line(), axis.ticks.y = element_line()
          )
      } ; plot_wr
    })
    
    #datatable winrate
    output$table_wr <- DT::renderDataTable(data_wr(), options = list(pageLength = 100, scrollY = "282"))
    
    #datatable oo
    output$table_oo <- DT::renderDataTable(appearance() %>% dplyr::rename('Occurrences' = 'O') %>% dplyr::arrange(-Occurrences), options = list(pageLength = 100, scrollY = "282"))
    
  }
  shinyApp(ui = ui, server = server)
}
