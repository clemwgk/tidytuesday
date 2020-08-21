### Tidy Tuesday Apr 2020 - Broadway shows ###
### For self-learning purposes ###


packagelist <- c("tidyverse", "ggplot2", "lubridate", 
                 "scales", "gghighlight", "viridis")
check <- FALSE

if(check == TRUE) {
  lapply(packagelist, install.packages, character.only = TRUE)
 
}


library(tidyverse)
library(ggplot2)
library(lubridate)
library(gghighlight)
library(scales)
library(viridis)

library(extrafont)
loadfonts(device = "win")


### 1. Get the Data -----

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

#grosses <- read_csv("grosses.csv")


### 2. Some data cleaning? -----

glimpse(grosses)

summary(grosses)
View(grosses)

  # Check 1: If performances + previews = 0, then the weekly gross should be zero too.
  
  grosses %>%
    filter(performances + previews == 0) %>%
    View() 
  
    #potential issue with Basia on Broadway
    #it has non-zero seats but no shows and sales info?
    #google search suggests there were shows
  
  grosses_1 <- grosses %>%
    mutate_at(vars(performances, previews, weekly_gross),
              ~ifelse(show == "Basia on Broadway", NA, .)
              )
    # don't know how many shows but we know there were some shows, so replace with NA.
    # Also assume shows not free so replace weekly_gross with missing as well


### 3.1 - Which Broadway show earned the most money? ### ----


    # First, generate a yearly total
    
    grosses_2 <- grosses_1 %>%
      mutate(year = year(week_ending)) %>%
      group_by(show, year) %>%
      mutate(yearly_gross = sum(weekly_gross)) %>%
      ungroup()
      
    # Next, find the top x shows
    
    grosses_summary1 <- grosses_2 %>%
      select(show, year, yearly_gross) %>% 
      distinct() %>%
      group_by(show) %>%
      arrange(show, year) %>%
      mutate(cumsum_yearly_gross = cumsum(yearly_gross),
             finalyear_cumgross = ifelse(max(cumsum_yearly_gross) == cumsum_yearly_gross, cumsum_yearly_gross, NA)
             )
            
    View(grosses_summary1)
    
    grosses_summary_plot <- grosses_summary1 %>%
      ungroup() %>%
      mutate(top_10 = ifelse(rank(-finalyear_cumgross, ties.method = "min") <= 10, 1, 0)) %>%
      group_by(show) %>%
      mutate(top_10 = ifelse(max(top_10) == 1, TRUE, FALSE))
    
    grosses_summary_plot %>%
      filter(top_10 == TRUE) %>% View()
    
    
    
    
    ## basic plot
    grosses_summary_plot %>%
      filter(year < 2020) %>% ##remove 2020 because the year's not complete so the slopes will look weird for 2019 to 2020
      
      ggplot(aes(x = year, y = cumsum_yearly_gross, colour = show)) +
      geom_line(size = 1) +
      
      
      ##highlight and facet
      gghighlight(top_10 == TRUE, use_direct_label = FALSE,
                  unhighlighted_params = list(size = 0.5)
                  ) + 
      facet_wrap(~show, nrow = 2) +
      
      ##change axis breaks and labels
      scale_y_continuous(labels = scales::dollar_format(scale = 1e-6)) +
      scale_x_continuous(breaks = seq(1985, 2020, 5)) +
      
      theme_minimal() +
      
      ##labels
      labs(
        title = "Cumulative earnings by top 10 Broadway shows (millions)",
        caption = "Tidy Tuesday Apr 2020"
      ) +
      xlab("Year") +   
      
      
      theme(
      
        plot.caption = element_text(face = "italic"),
      
        axis.title.y = element_blank(),
        
        axis.ticks.y = element_blank(),
        
        axis.text.x = element_text(angle = 50, hjust = 1.2), #axis label rotation
        
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
      
        legend.position = "none"
        
      )
    
    
    ggsave("top10totalgross_line_facet_highlight.png", scale = 2)

### Q2 - Inequality in broadway earnings. ### -----
  
  ### How much of the broadway earnings is earned by the top X% of shows?
  ### Is this growing over time?
  ### What proportion of weekly/yearly total grossed by broadway much does each quartile gross?
    
    View(grosses_2)
    
    grosses_3 <- grosses_2 %>%
      group_by(year) %>%
      mutate(percentile_grp = ntile(yearly_gross, 5),
             yearly_total_gross = sum(yearly_gross, na.rm = TRUE)
             )
    
    
    grosses_3_summary <- grosses_3 %>% 
      group_by(year, percentile_grp) %>% 
      
      summarise(total_yearly_grp_gross = sum(yearly_gross),
                percent_yearly_gross = sum(yearly_gross)/mean(yearly_total_gross),
                n = n()
                ) %>%
      
      mutate(check = sum(percent_yearly_gross, na.rm = TRUE)) %>% 
      mutate(legend_label = paste0((percentile_grp-1)*20 + 1, "st to ", percentile_grp*20,"th percentile")) %>%               
      
      select(-check) %>%
      filter(year < 2020)
    
    
     
      
    #basic plot
    ggplot(grosses_3_summary, 
           aes(x = year, 
               y = percent_yearly_gross, 
               fill = fct_rev(factor(legend_label)))) + #use fct_rev to reverse the stacking order
      geom_col(width = 0.9) +
      
      #looks nicer
      geom_hline(yintercept = 0, size = 1, colour = "#333333") +
      
      #add colour scale
      scale_fill_viridis(option = "viridis",
                         discrete = TRUE,
                         name = "Percentile of show earnings",
                         direction = -1
                         ) +
     
      #reverse order of guide/legend
      guides(fill = guide_legend(reverse = TRUE)) +
    
      
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      
    #labels
      labs(title = "How Broadway's annual earnings are distributed",
           subtitle = "% of annual earnings earned by quintiles (of show earnings), 1985 - 2019",
           caption = "Data from Playbill \n Tidy Tuesday Apr 2020"
            ) +
      
      scale_x_continuous(breaks = seq(from = 1985, to = 2020, by = 5)) +
      
      coord_cartesian(xlim = c(1984.5, 2019.5), expand = FALSE) +
      
       #theme
      theme(
        plot.title = element_text(family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(family = "Helvetica", face = "italic"),
        plot.caption = element_text(family = "Helvetica", face = "italic"),
        
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        
        axis.ticks.y = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_text(angle = 60, hjust = 0.5), #axis label rotation
        axis.text = element_text(family = "Helvetica", colour = "#222222"), 
        axis.text.x = element_text(margin = margin(t = 5, b = 10)),
        
        
        panel.grid.major.y = element_line(color = "#cbcbcb"),
        panel.grid.minor.y = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.background = element_blank(),
        
        legend.position = "top",
        legend.title = element_blank(),
        legend.justification = "left",
        legend.background = element_blank(),
        legend.text = element_text(family = "Helvetica"),
        
        plot.margin = margin(10,10,10,10, unit = "pt")
        
      )
    
    ggsave("broadway earning shares.png", width = 10, height = 8, units = "in", dpi = 350)
    



