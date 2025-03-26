# Loading libraries and data ----------------------------------------------

library(tidyverse)  # always
library(showtext)   # for custom font
library(patchwork)  # for combining multiple ggplot objects into one

font_add_google("Open Sans", family = "os") # adding a font I like and making it the default

showtext_auto()
showtext_opts(dpi = 300)

ge24_results_url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/HoC-GE2024-results-by-constituency.csv"
save_results_file_here <- "HoC-GE2024-results-by-constituency.csv"
download.file(ge24_results_url, save_results_file_here, mode = "wb")

results <- read_csv("HoC-GE2024-results-by-constituency.csv") |> 
  
  mutate(
    
    conservative_prop = Con / `Valid votes`,
    labour_prop = Lab / `Valid votes`,
    ld_prop = LD / `Valid votes`,
    green_prop = Green / `Valid votes`,
    reform_prop = RUK / `Valid votes`,
    snp_prop = SNP / `Valid votes`,
    pc_prop = PC / `Valid votes`,
    dup_prop = DUP / `Valid votes`,
    sf_prop = SF / `Valid votes`,
    uup_prop = UUP / `Valid votes`,
    sdlp_prop = SDLP / `Valid votes`,
    apni_prop = APNI / `Valid votes`,
    other_prop = `Of which other winner` / `Valid votes`
    
  )


# Calculating expected seats for Reform -----------------------------------
  
seat_projections_list <- list()

index <- 1

for (swing in seq(0, 0.2, 0.001)) {
  
  swing_results <- results |>
    
    mutate(
      
      # If possible, assume that Conservative and Labour vote shares fall by the same amount
      # If the swing is bigger than the one of these party's vote share, set that party's new vote share to zero...
      # ...and subtract the difference from the other party's new vote share
      
      new_conservative_prop = case_when(
        conservative_prop >= swing/2 & labour_prop >= swing/2 ~ conservative_prop - swing/2, 
        conservative_prop < swing/2 ~ 0, 
        conservative_prop >= swing/2 & labour_prop < swing/2 ~ pmax(conservative_prop + labour_prop - swing, 0)
      ),
      
      new_labour_prop = case_when(
        labour_prop >= swing/2 & conservative_prop >= swing/2 ~ labour_prop - swing/2,
        labour_prop < swing/2 ~ 0,
        labour_prop >= swing/2 & conservative_prop < swing/2 ~ pmax(labour_prop + conservative_prop - swing, 0)
      ),
      
      new_reform_prop = reform_prop + (conservative_prop - new_conservative_prop) + (labour_prop - new_labour_prop),
      
      reform_win = if_else(new_reform_prop ==
                             pmax(new_reform_prop, new_conservative_prop, new_labour_prop,
                                  green_prop, ld_prop, snp_prop, pc_prop,
                                  dup_prop, sf_prop, sdlp_prop, uup_prop, apni_prop,
                                  other_prop),
                           1, 0)
    )
  
  reform_win_count <- sum(swing_results$reform_win)
  
  seat_projections_list[[index]] <- list(swing = swing, seats = reform_win_count)
  
  index <- index + 1
}

seat_projections <- do.call(rbind, lapply(seat_projections_list, as.data.frame))


# Plotting expected seats -------------------------------------------------

seat_projections_plot <- ggplot(seat_projections, aes(swing, seats)) +
  
  geom_hline(yintercept = seq(0, 400, 100), colour = "grey90") +
  
  geom_vline(xintercept = seq(0, 0.2, 0.05), colour = "grey90") +
  
  geom_segment(y = 326, yend = 326, x = 0, xend = 0.2, linetype = "dashed", colour = "grey30") +
  
  annotate(geom = "text", label = "Majority", x = 0.025, y = 340, hjust = 0.5,
           family = "os", fontface = "bold", colour = "grey30") +
  
  annotate(geom = "text", label = "We are\nhere", x = 0.075, y = 148, hjust = 0.5,
           family = "os", fontface = "bold", colour = "grey30") +
  
  geom_segment(x = 0.09, xend = 0.119, y = 148, yend = 148, arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 0.5,
               lineend = "round", linejoin = "round", colour = "grey30") +
  
  geom_smooth(method = "gam", se = FALSE, colour = "#12B6CF") +
  
  labs(title = "Gradually, then suddenly",
       subtitle = "Reform has reached the steepest point on its votes-seats curve",
       x = "Swing to Reform",
       y = "Seats in House of Commons",
       caption = "Based on uniform national swing away from Labour and Conservatives") +
  
  scale_x_continuous(breaks = seq(0, 0.2, 0.05),
                     labels = function(x) paste0(x*100, "pp")) +
  
  scale_y_continuous(breaks = seq(0, 400, 100)) +
  
  theme_bw() +
  
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "os", colour = "grey30", margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(family = "os", colour = "grey30", margin = margin(r = 10, l = 10)),
        legend.position = "none",
        plot.caption = element_text(family = "os", size = 11, hjust = 0, colour = "grey30", face = "italic"),
        axis.text = element_text(size = 11, family = "os"),
        plot.title = element_text(family = "os", margin = margin(t = 10, b = 10), colour = "grey30", face = "bold"),
        plot.subtitle = element_text(family = "os", colour = "grey30", face = "bold.italic"))

ggsave("Reform seat projections.png", seat_projections_plot, width = 7, height = 7)

