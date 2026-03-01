library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggh4x)
library(ggrepel)
library(shinydashboard)
library(DT)

#Data processing
raw <- {
  read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSHToeks_abfFEHi4bZNsJZlRr2f4NMveAn9-JYvpRvPRzAg55Za5tHtNN85ilo2YPgoTqz1yc5WQp7/pub?output=csv')
} ; raw[raw == ""] <- NA ; head(raw) #raw data
clean <- {
  fill(raw, ID:DATE, RESULT, MATCH, TIPTOUCHER, NOTES, .direction = "down") %>% 
    dplyr::select(-NOTES) %>%
    dplyr::filter(nchar(ID)==28) %>%
    mutate(
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
    )
} ; head(clean) #cleaned data
mapvote <- {
  clean %>%
    pivot_longer(cols=c(MAP1,MAP2,MAP3), values_to = 'VOTE', names_to = 'SLOT') %>%
    pivot_wider(names_from = ATTRIBUTE, values_from = VOTE) %>%
    mutate(
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
    )
} ; head(mapvote) #organized for map vote statistics

#Map vote statistics
expected <- {
  mapvote %>% 
    group_by(ID) %>%
    mutate(total = sum(vote)) %>% 
    group_by(ID, map) %>%
    summarize(p = vote/total) %>% 
    group_by(map) %>%
    dplyr::filter(p!=0) %>%
    summarize(E = sum(p))
} ; head(expected)
observed <- {
  mapvote %>%
    dplyr::select(ID, RESULT) %>%
    unique() %>%
    group_by(RESULT) %>%
    summarize(O = n())
} ; head(observed)
variance <- {
  mapvote %>% 
    group_by(ID) %>%
    mutate(total = sum(vote)) %>% 
    group_by(ID, map) %>%
    summarize(p = vote/total) %>% 
    group_by(map) %>%
    dplyr::filter(p!=0) %>%
    summarize(var = sum(p*(1-p)))
} ; head(variance)
stat_mapvote <- {
  merge(expected, observed, by.x = 'map', by.y = 'RESULT') %>%
    merge(variance, by = 'map') %>%
    mutate(
      Z = (O-E)/sqrt(var),
      p = 2*(1-pnorm(abs(Z)))
    ) %>%
    dplyr::arrange((Z))
} ; head(stat_mapvote)

#Appearance statistics
appearance <- {
  mapvote %>%
    group_by(map) %>%
    summarize(O = n())
} ; head(appearance)
popularity <- {
  mapvote %>%
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
} ; head(popularity)

#Rankings
maps <- appearance$map %>% unique()
rank_matrix <- matrix(0, nrow=length(maps), ncol=length(maps), dimnames = list(maps,maps))
count_matrix <- matrix(0, nrow=length(maps), ncol=length(maps), dimnames = list(maps, maps))

#Compute map pairwise matrix
mapvote %>% 
  group_by(ID) %>%
  group_split() %>%
  lapply(function(event){
    
    objs  <- event$map
    votes <- event$vote
    
    # All pairwise combinations
    pairs <- combn(seq_along(objs), 2)
    
    for (i in seq_len(ncol(pairs))) {
      
      i1 <- pairs[1, i]
      i2 <- pairs[2, i]
      
      obj1 <- objs[i1]
      obj2 <- objs[i2]
      
      v1 <- votes[i1]
      v2 <- votes[i2]
      
      count_matrix[obj1, obj2] <<- count_matrix[obj1, obj2] + 1
      count_matrix[obj2, obj1] <<- count_matrix[obj2, obj1] + 1
      
      if (v1 > v2) {
        rank_matrix[obj1, obj2] <<- rank_matrix[obj1, obj2] + 1
        rank_matrix[obj2, obj1] <<- rank_matrix[obj2, obj1] - 1
      } else if (v1 < v2) {
        rank_matrix[obj1, obj2] <<- rank_matrix[obj1, obj2] - 1
        rank_matrix[obj2, obj1] <<- rank_matrix[obj2, obj1] + 1
      }
      # ties automatically add 0
    }
  })

#Tierlist objects
scores <- rowSums(rank_matrix) ; scores <- scores[names(scores) %in% dplyr::filter(appearance, O >= 10)$map]
rank_table <- {
  data.frame(map = names(scores), score = as.numeric(scores)) %>%
    merge(., appearance) %>%
    mutate(
      score = score/(2*O),
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
}
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
data_tiers <- {
  rank_table %>%
    group_by(tier) %>%
    dplyr::arrange(desc(score), by_group = TRUE) %>%
    mutate(
      tier = factor(tier, levels = c('S','A','B','C','D','E','F')),
      x = seq(from = 1, to = length(tier), by = 1)
    ) %>%
    ungroup()
} ; data_tiers <- data_tiers %>%mutate(map = factor(map, levels = data_tiers$map))
plot_tierlist <- {
  ggplot(data_tiers, aes(y = tier, x = x, label = str_wrap(map, width = 10))) +
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

matrix_norm <- rank_matrix / count_matrix ; matrix_norm[is.nan(matrix_norm)] <- NA

matrix_plot_data <- {
  matrix_norm %>%
    as.data.frame() %>%
    mutate(map1 = rownames(.)) %>%
    group_by(map1) %>%
    pivot_longer(cols = -map1, names_to = 'map2', values_to = 'mat') %>%
    merge(., appearance, by.x = 'map1', by.y = 'map') %>%
    dplyr::filter(map1 %in% c('Temple of Anubis','Hanamura') == F & map2 %in% c('Temple of Anubis','Hanamura') == F) %>%
    mutate(
      map1 = factor(map1, levels = rank_table$map),
      map2 = factor(map2, levels = rank_table$map)
    ) %>%
    dplyr::filter(is.na(map1) == F & is.na(map2) == F)
}
matrix_plot_labels <- {
  count_matrix %>%
    as.data.frame() %>%
    mutate(map1 = rownames(.)) %>%
    group_by(map1) %>%
    pivot_longer(cols = -map1, names_to = 'map2', values_to = 'mat') %>%
    merge(., appearance, by.x = 'map1', by.y = 'map') %>%
    dplyr::filter(map1 %in% c('Temple of Anubis','Hanamura') == F & map2 %in% c('Temple of Anubis','Hanamura') == F) %>%
    mutate(
      map1 = factor(map1, levels = rank_table$map),
      map2 = factor(map2, levels = rank_table$map)
    ) %>%
    dplyr::filter(is.na(map1) == F & is.na(map2) == F)
}

plot_matrix <- {
  matrix_plot_data %>%
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
} 
plot_matrix_labels <- {
  matrix_plot_labels %>%
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
}

#Textbox data
total_entries <- {
  mapvote %>% 
    dplyr::select(ID,TIPTOUCHER) %>%
    unique() %>%
    nrow()
}
tt_ruined <- {
  mapvote %>% 
    dplyr::select(ID,TIPTOUCHER) %>%
    unique() %>%
    dplyr::filter(TIPTOUCHER==TRUE) %>%
    nrow()
}
winrate <- {
  mapvote %>% 
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
    summarize(wr = round(100*sum(MATCH)/n(),1)) %>%
    .$wr
}

#DT
ranking_display <- {
  dplyr::select(data_tiers, map, tier, score) %>%
    mutate(score = round(score,2))
}

#O vs score data
data_oo <- merge(appearance, data_tiers, by=c('map','O')) %>%
  dplyr::filter(O >= 10 & map %in% c('Hanaoka','Throne of Anubis') == F) 
oo_min <- min(data_oo$O) - (min(data_oo$O) %% 5)
oo_max <- max(data_oo$O) + 5 - (max(data_oo$O) %% 5)
plot_oo <- {
  data_oo %>%
    ggplot(., aes(x = (score), y = (O), label = map)) +
    scale_color_manual(values=tier_colors, name = 'Tier') +
    geom_line(stat='smooth', method='lm', size = 1.75, color = 'gray') +
    geom_label_repel(label.size = NA, seed = 123, alpha = 0.75, label.padding=0.1, min.segment.length = 0) +
    geom_point(size = 2, aes(color = tier)) +
    scale_x_continuous(limits = c(-1,1), breaks = c(-1.00,-0.75,-0.50,-0.25,0.00,0.25,0.50,0.75,1.00)) +
    scale_y_continuous(limits = c(oo_min, oo_max), breaks = seq(oo_min, oo_max, 5)) +
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
}

#winrate
data_wr <- {
  mapvote %>%
    dplyr::select(ID, map, MATCH) %>%
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
    ungroup() %>% group_by(map) %>%
    summarize(
      WR = sum(MATCH)/n(),
      N = n()
    ) %>%
    dplyr::filter(N>=10) %>%
    dplyr::arrange(desc(WR))
}
plot_wr <- {
  data_wr %>%
    merge(data_oo, by='map') %>%
    mutate(
      map = factor(map, levels = rev(data_wr$map)),
      tier = factor(tier, levels = rev(c('S','A','B','C','D','E','F')))
    ) %>%
    ggplot(., aes(x = WR, y = map, label = N, fill = tier)) +
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
}

##Function
myApp <- function() {
  ui <- dashboardPage(
    dashboardHeader(title = "OW Map Voting"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("WIP", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      fluidRow(
        valueBoxOutput("total_entries"),
        valueBoxOutput("tiptoucher"),
        valueBoxOutput("winrate")
      ),
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
      fluidRow(
        box(title = "Winrate Summary",
            solidHeader = T,
            width = 6, 
            collapsible = T,
            plotOutput("plot_wr", height = 600)),
        box(title = "Occurrences vs Score", solidHeader = T,
            width = 6, 
            collapsible = T,
            plotOutput("plot_oo", height = 600))
      ),
      fluidRow(
        box(title = "Raw Data",
            solidHeader = T,
            width = 12, 
            collapsible = T,
            div(DT::DTOutput("clean_dt"), style = "font-family: Consolas, monospace; font-size: 100%;"))
      )
    ) # body
  )
  server <- function(input, output) {
    
    ### Value boxes ###
    # Total Entries
    output$total_entries <- renderValueBox({
      valueBox('Total Entries:', 
               total_entries, icon = icon("layer-group"), color = "light-blue")
    })
    # TipToucher Games Ruined
    output$tiptoucher <- renderValueBox({
      valueBox('Ruined by TipT:', 
               tt_ruined, icon = icon("heart-crack"), color = "yellow")
    })
    # Winrate
    output$winrate <- renderValueBox({
      valueBox('Winrate:', 
               paste0(winrate,'%'), icon = icon("trophy"), color = "red")
    })
    # Map Rankings Table
    output$ranking_display <- DT::renderDataTable(ranking_display, options = list(pageLength = 100, scrollY = "282"))
    
    #raw data table
    output$clean_dt <- DT::renderDataTable(clean, options = list(pageLength = 10000, scrollY = "282"))
    
    # Tierlist plot
    output$plot_tierlist <- renderPlot({ 
      plot_tierlist
    })
    
    # Pairwise matrix
    output$plot_matrix <- renderPlot({ 
      plot_matrix
    })
    
    # Pairwise matrix counts
    output$plot_matrix_labels <- renderPlot({ 
      plot_matrix_labels
    })
    
    # O vs Score
    output$plot_oo <- renderPlot({ 
      plot_oo
    })
    
    # Winrate
    output$plot_wr <- renderPlot({ 
      plot_wr
    })
  }
  shinyApp(ui = ui, server = server)
}
