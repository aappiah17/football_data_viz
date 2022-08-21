#install.packages('ggbeeswarm')
#install.packages("fuzzyjoin")
#install.packages('sysfonts')
#devtools::install_github("ryurko/fcscrapR")
# soccermatics
#devtools::install_github("jogall/soccermatics")
#devtools::install_github("JaseZiv/worldfootballR")
#remotes::install_github('ewenme/understatr')
#remotes::install_github("jthomasmock/gtExtras")
suppressPackageStartupMessages({
library(tidyverse)
library(tidymodels)
library(recipes)
library(workflows)
library(tune)
#library(mlbench)
library(ranger)
#library(readr)
#library(broom.mixed)
#library(skimr)
#library(tidytuesdayR)
#library(corrplot)
#library(gridExtra)
library(vip)
#library(ggalluvial)
library(summarytools)
#library(ggflags)
library(gganimate)
library(ggrepel)
library(ggimage)
library(understatr)
library(ggsoccer)
library(gt)
library(lubridate)
#library(tidytext)
library(glue)
#library(ggthemes)
library(RSelenium)
#library(ggpubr)
library(xgboost)
library(tictoc)
library(rpart)
library(ggdark)
library(doParallel)
library(rvest)
library(systemfonts)
library(extrafont)
library(openxlsx)
library(ggtext)
library(magick)
library(waffle)
library(RColorBrewer)
library(scales)
library(patchwork)
library(cowplot)
library(worldfootballR)
library(janitor)
library(ggbeeswarm)
library(fuzzyjoin)
#library(ggchicklet)
#install.packages(c("ggbrace"))
#devtools::install_github("nicolash2/ggbrace")
library(ggforce)
#library(ggbrace)
library(prismatic)
#library(ggwaffle)
library(rtweet)
#library(sysfonts)
library(jsonlite)
library(gridtext)
library(paletteer)
library(jsonlite)
#library(StatsBombR)
#library(ggflags)
library(ggpmisc)
library(readxl)
#library(tabulizer)
library(rsvg)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(webshot)
library(soccermatics)
library(gtExtras)
#library(emojifont)
#library(emoGG)
library(googledrive)
library(reticulate)
library(spotifyr)
library(gtExtras)
library(reticulate)
library(hexbin)
library(metR)
library(ggpattern)
library(ggshakeR)
#install.packages("ggnewscale")
library(ggnewscale)
library(here)
})
#devtools::install_github("ddsjoberg/bstfun")

install.packages("corrr")
library(corrr)

#install.packages("rJava")
#install.packages("RSelenium")
#devtools::install_github("abhiamishra/ggshakeR",force=T)
#library(sysfonts)
#install.packages("emojifont")
#install.packages("rsvg")
#install.packages("paletteer")
#install.packages("doParallel")
#install.packages("xgboost")
#install.packages("reticulate")
#devtools::install_github("jogall/soccermatics")

#remotes::install_github("coolbutuseless/ggpattern")

# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-18.0.1.1/")
# 
# #emoGG::emoji_search('soccer')
# 
# remotes::install_github("jthomasmock/gtExtras")
# detach("package:emojifont", unload=TRUE)
# detach("package:emoGG", unload=TRUE)
# detach("package:ggimage", unload=TRUE)
# detach("package:fuzzyjoin", unload=TRUE)
# detach("package:worldfootballR", unload=TRUE)
# detach("package:ggshakeR", unload=TRUE)
# #install.packages("prismatic")
# #font_import()
# remove.packages("Rttf2pt1")
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# loadfonts(device = "win", quiet = TRUE)
# install.packages("ggtext")
# remotes::install_github("wilkelab/ggtext")
# 
# install.packages("reticulate")
# 
# devtools::install_github("nsgrantham/ggdark")
# devtools::install_github("JaseZiv/worldfootballR")
# remotes::install_github('ewenme/understatr')

AAtheme_leg<-theme(plot.title=element_text(face="bold", size=15),
                   plot.subtitle=element_text(size=11, color="gray50"),
                   axis.text.y = element_text(face="bold",size=9),
                   axis.title.y = element_text(face="bold",size=9),
                   axis.title.x = element_text(face="bold",size=9),
                   axis.text.x = element_text(face="bold",size=9,hjust=0.5),
                   #axis.line.x = element_line(),
                   axis.ticks.y=element_blank(),
                   axis.ticks.x=element_blank(),
                   panel.grid.major.y = element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   legend.text=element_text(size=10),
                   legend.title = element_blank(),
                   legend.background =element_blank(),
                   legend.position="top",
                   legend.direction = "horizontal",
                   plot.caption = element_text(size = 11, face = "italic"))
AAtheme_leg_1<-theme(plot.title=element_text(face="bold", size=15),
                   plot.subtitle=element_text(size=11, color="gray50"),
                   axis.text.y = element_text(face="bold",size=7),
                   axis.title.y = element_text(face="bold",size=7),
                   axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                   axis.ticks.y=element_blank(),
                   axis.ticks.x=element_blank(),
                   panel.grid.major.y = element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   legend.text=element_text(size=10),
                   legend.title = element_blank(),
                   legend.background =element_blank(),
                   legend.position="top",
                   legend.direction = "horizontal",
                   plot.caption = element_text(size = 11, face = "italic"))

AAtheme_sports<-theme(plot.title=element_text(face="bold", size=15,family = "Gadugi"),
                   plot.subtitle=element_text(size=11, color="grey45",family = "Gadugi"),
                   axis.text.y = element_text(face="bold",size=11,family = "Gadugi"),
                   axis.title.y = element_text(face="bold",size=11,family = "Gadugi"),
                   axis.title.x = element_text(face="bold",size=11,family = "Gadugi"),
                   axis.text.x = element_text(face="bold",size=11,hjust=0.5,family = "Gadugi"),
                   #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                   axis.ticks.y=element_blank(),
                   axis.ticks.x=element_blank(),
                   plot.background = element_rect(fill = "grey95", color = "transparent"),
                   panel.border = element_blank(),
                   panel.grid.major.y = element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   legend.text=element_text(size=10),
                   legend.title = element_blank(),
                   legend.background =element_blank(),
                   legend.position="top",
                   legend.direction = "horizontal",
                   plot.caption = element_text(size = 9, face = "bold",family = "Gadugi"))

AAtheme_pap<-theme(plot.title=element_text(face="bold", size=15,family = "Gadugi"),
                      plot.subtitle=element_text(size=11, color="grey20",family = "Gadugi"),
                      axis.text.y = element_text(face="bold",size=11,family = "Gadugi"),
                      axis.title.y = element_text(face="bold",size=11,family = "Gadugi"),
                      axis.title.x = element_text(face="bold",size=11,family = "Gadugi"),
                      axis.text.x = element_text(face="bold",size=11,hjust=0.5,family = "Gadugi"),
                      #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank(),
                      plot.background = element_rect(fill = "white", color = "NA"),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_blank(), 
                      panel.grid.major.x = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.text=element_text(size=10),
                      legend.title = element_blank(),
                      legend.background =element_blank(),
                      legend.position="none",
                      legend.direction = "horizontal",
                      plot.caption = element_text(size = 9, face = "bold",family = "Gadugi"))
AAtheme_sports_1<-theme(plot.title=element_text(face="bold", size=20),
                      plot.subtitle=element_text(size=15, color="gray50"),
                      axis.text.y = element_text(face="bold",size=15),
                      axis.title.y = element_text(face="bold",size=15),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank(),
                      plot.background = element_rect(fill = "grey95", color = "transparent"),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_blank(), 
                      panel.grid.major.x = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.text=element_blank(),
                      legend.title = element_blank(),
                      legend.background =element_blank(),
                      legend.position="none",
                      legend.direction = "horizontal",
                      plot.caption = element_text(size = 9, face = "bold"))

AAtheme_sports_1<-theme(plot.title=element_text(face="bold", size=15, color="white"),
                      plot.subtitle=element_text(size=11, color="white"),
                      axis.text.y = element_text(face="bold",size=11,color="white"),
                      axis.title.y = element_text(face="bold",size=11,color="white"),
                      axis.title.x = element_text(face="bold",size=11,color="white"),
                      axis.text.x = element_text(face="bold",size=11,hjust=0.5,color="white"),
                      #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                      axis.ticks.y=element_blank(),
                      axis.ticks.x=element_blank(),
                      plot.background = element_rect(fill = "#666666", color = "transparent"),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_blank(), 
                      panel.grid.major.x = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.text=element_text(size=10),
                      legend.title = element_blank(),
                      legend.background =element_blank(),
                      legend.position="top",
                      legend.direction = "horizontal",
                      plot.caption = element_text(size = 9, face = "bold",color="white"))

AAtheme_econ_mkd_dark<-theme(plot.title=element_markdown(lineheight = 1.1, size = 20, face = "bold", box.colour = "#999999",family="Gadugi"),
                             plot.subtitle=element_text(size=13, color="gray50",family="Gadugi",face = "bold"),
                             axis.text.y = element_text(face="bold",size=15,family="Gadugi"),
                             axis.title.y = element_text(face="bold",size=15,family="Gadugi"),
                             axis.title.x = element_text(face="bold",size=15,family="Gadugi"),
                             axis.text.x = element_text(face="bold",size=15,hjust=0.5,family="Gadugi"),
                             #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                             axis.ticks.y=element_blank(),
                             axis.ticks.x=element_blank(),
                             # plot.background = element_rect(fill = "grey95", color = "transparent"),
                             plot.background = element_rect(color = "#282a36", fill = "#282a36"),
                             panel.border = element_blank(),
                             panel.grid.major.y = element_blank(), 
                             panel.grid.major.x = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             legend.text=element_text(size=10),
                             legend.title = element_blank(),
                             legend.background =element_blank(),
                             strip.text = element_text(face="bold", size=17,family="Gadugi"),
                             legend.position="none",
                             legend.direction = "horizontal",
                             plot.caption = element_text(size = 10, face = "bold",family="Gadugi"))

AAtheme_soccer <- theme(
  line = element_line(lineend = "round", color = "#000000"),
  text = element_text(color = "#000000"),
  plot.background = element_rect(fill = "grey95", color = "transparent"),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "white", color = "transparent"),
  axis.ticks = element_line(color = "#000000", size = 0.5),
  axis.ticks.length = unit(2.75, "pt"),
  axis.title = element_text(size = 8),
  axis.text = element_text(size = 7, color = "#000000"),
  plot.title = element_text(size = 14),
  plot.subtitle = element_text(size = 8),
  plot.caption = element_text(size = 5),
  legend.background = element_rect(fill = "grey90", color = "#000000"),
  legend.key = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "grey85", size = 0.3),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  strip.background = element_blank(),
  strip.text = element_text(size = 6, color = "#000000"),
  legend.position = "bottom"
)

AAtheme_dark<-dark_mode(AAtheme_leg)

AAtheme_patch<-theme(plot.title=element_markdown(face="bold", size=23,family="Consolas",hjust=0.5, color = "white"),
                     plot.subtitle=element_markdown(size=12, color="white",family="Consolas", face="bold", hjust = 0.5),
                     axis.text.y = element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
                     axis.title.y = element_text(size=13, color="white",family="Consolas",face = "bold"),
                     axis.title.x = element_text(size=13, color="white",family="Consolas",face = "bold"),
                     axis.text.x = element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
                     #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                     axis.ticks.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     # plot.background = element_rect(fill = "grey95", color = "transparent"),
                     plot.background = element_rect(fill = "grey20",color = NA),
                    # panel.border = element_blank(),
                   panel.grid.major.y = element_blank(), 
                    panel.grid.major.x = element_blank(), 
                    panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     legend.text=element_blank(),
                     legend.title = element_blank(),
                     legend.background =element_blank(),
                     legend.position="none",
                     legend.direction = "horizontal",
                     plot.caption = element_markdown(size = 8, face = "bold",family="Consolas",color = "white"))


# invert_geom_defaults()

`%nin%` <- negate(`%in%`)


# Function to pre-process pass data
pre_process_pass <- function(df){
  df%>% clean_names()%>%
    
    filter(type=="Pass") %>%
    select(minute,second,h_a,team_name,x,y,end_x,end_y,match_id,player_id,player_name,outcome_type,qualifiers,satisfied_events_types,pass_accurate,short_pass_inaccurate,short_pass_accurate,
           pass_corner,pass_corner_accurate,pass_corner_inaccurate,pass_freekick,pass_back,pass_forward,pass_left,pass_right,
           pass_long_ball_accurate,pass_long_ball_inaccurate,pass_right_foot,pass_left_foot,pass_head,
           pass_cross_accurate,pass_cross_inaccurate,pass_through_ball_accurate,pass_through_ball_inaccurate,throw_in,
           defensive_third,mid_third,final_third,is_goal_kick,is_cross)%>%
    # Calculate passing distance
    mutate(passing_distance=round(distance_function(x,y,end_x,end_y),1), pass_angle=atan2(end_y*0.68-y*0.68,end_x*1.05-x*1.05),
           pass_angle =round(pass_angle*180/pi/15)*15,initial_dist_to_goal=round(distance_function(x,y,105,34),1),
           final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
           prog=(final_dist_to_goal-initial_dist_to_goal)/initial_dist_to_goal)%>%
    # is_progressive=ifelse((final_dist_to_goal/initial_dist_to_goal)<0.75,T,F)
    # Change location data to international standard
    #across(c(x,end_x),.fns = ~round((.*105/100),1)),
    #across(c(y,end_y),.fns = ~round((.*68/100),1)))%>%
    relocate(c(passing_distance,pass_angle),.after=end_y) %>%
    mutate(pass_body_part=case_when(pass_right_foot==T~"Right Foot",
                                    pass_left_foot==T~"Left Foot",
                                    pass_head==T~"Head"),
           pass_field_area=case_when(defensive_third==T~"Defensive Third",
                                     mid_third==T~"Mid Third",
                                     final_third==T~"Final Third"),
           pass_direction=case_when(pass_back==T~"Back Pass",
                                    pass_forward==T~"Forward Pass"),
           through_ball=ifelse(pass_through_ball_accurate==T|pass_through_ball_inaccurate==T, T,F),
           cross=ifelse(pass_cross_accurate==T|pass_cross_inaccurate==T,TRUE,FALSE),
           pass_cross=ifelse(cross==T,"Cross","Pass"),
           long_ball=ifelse(pass_long_ball_accurate==T|pass_long_ball_inaccurate==T,T,F),
           pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
                                    TRUE~"Open Play"),
           initial_dist_to_goal=round(distance_function(x,y,105,34),1),
           final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
           prog=(final_dist_to_goal-initial_dist_to_goal)/initial_dist_to_goal,
           is_progressive=ifelse((final_dist_to_goal/initial_dist_to_goal)<0.75,T,F)
    )%>%
    relocate(pass_body_part,.after=pass_head)%>%
    relocate(pass_direction,.after=pass_forward) %>%
    select(player_id,player_name,team_name,h_a,x,y,end_x,end_y,pass_situation,passing_distance,pass_angle,pass_field_area,outcome_type,pass_corner,pass_freekick,pass_direction,through_ball,cross,is_cross,long_ball,
           throw_in,pass_head,is_progressive)%>%
    mutate(outcome_type=factor(outcome_type, levels = c("Unsuccessful","Successful")),
           h_a=factor(h_a,levels = c("h","a")))
  
}

prep_shot_df <- function(df){
  df %>% clean_names()%>% filter(type!="Carry")%>%
      mutate(last_action=lag(type),
             goal_own=as.logical(goal_own),
             situation=as.character(situation)) %>%
      filter(is_shot==T) %>% 
      select(id,minute,type,x,y,player_name,h_a,situation,shot_body_type,last_action,is_goal,goal_own,match_id,team_name,shot_counter,big_chance_missed:big_chance_created) %>% 
      mutate(is_big_chance=ifelse(big_chance_missed==T|big_chance_scored==T,T,F)
      ) %>% 
      select(-big_chance_missed,-big_chance_scored,-big_chance_created) %>% 
      filter(goal_own != T) %>% 
    mutate(is_goal=ifelse(type=="Goal","Goal","No Goal"),shot_body_type=as.character(shot_body_type))%>%
    rowwise() %>%
    mutate(distance = distance(x, y),
           angle = goal_angle(x, y), is_goal=factor(is_goal, levels=c("Goal","No Goal")),
           across(c(shot_counter,is_big_chance),.fns = ~as.integer(.))) %>% 
    select(-goal_own,-type) %>% 
    ungroup()
    
}

use_xt <- function(df){
  df %>% clean_names()%>% filter(type %in% c("Pass","Carry")& outcome_type=="Successful")%>%
    select(match_id,id,minute,second,h_a,team_name,player_name,x,y,end_x,end_y,period,type,outcome_type,is_shot,shot_body_type,is_touch,pass_corner,pass_freekick,throw_in,pass_cross_accurate,pass_cross_inaccurate,pass_key,assist,
           is_goal_kick,big_chance_created,start_zone_value,end_zone_value,x_t)%>%
    
    mutate(pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
                                    TRUE~"Open Play"),
           cross=ifelse(pass_cross_accurate==T|pass_cross_inaccurate==T,TRUE,FALSE),
           initial_dist_to_goal=round(distance_function(x,y,105,34),1),
           final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
           prog=(final_dist_to_goal-initial_dist_to_goal)/initial_dist_to_goal,
           is_progressive=ifelse((final_dist_to_goal/initial_dist_to_goal)<0.75,T,F))%>%
    group_by(match_id)%>%
    
    arrange(minute,second)%>%
    mutate(pass_recipient=case_when(type=="Pass"&outcome_type=="Successful"&team_name==lead(team_name)~lead(player_name)))%>%
    ungroup()
}

distance_function <- function(x,y,x_end,y_end){
  
  # Transform coordinates to meters (assuming pitch is 105x68m)
  start_x <- round((x*105/100),1)
  end_x <- round((x_end*105/100),1)
  start_y <- round((y*68/100),1)
  end_y <- round((y_end*68/100),1)
  
  distance <- sqrt((end_x-start_x)^2 + (end_y-start_y)^2)
  
}

distance <- function(x_pos, y_pos){
  
  x_shift <- (100 - x_pos)
  y_shift <- abs(50 - y_pos)
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}

goal_angle <- function(x_pos, y_pos){
  
  x_shift <- (100 - x_pos)
  y_shift <- (50 - y_pos)
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

to_int<-rescale_coordinates(from = pitch_opta,to=pitch_international)

as_ggplot <- function(x, ...) {
  # checks ---------------------------------------------------------------------
  if (!inherits(x, c("gt_tbl", "gtsummary"))) stop("`x=` must be a 'gt' or 'gtsummary' table", call. = FALSE)
  
  # convert gtsummary to gt ----------------------------------------------------
  if (inherits(x, "gtsummary")) x <- gtsummary::as_gt(x)
  
  # save gt as image -----------------------------------------------------------
  path_gt_table_image <- fs::file_temp(ext = "png")
  gt_table_image <- gt::gtsave(x, filename = path_gt_table_image, ...)
  
  # save image in ggplot -------------------------------------------------------
  table_img <-
    magick::image_read(path_gt_table_image) %>% 
    magick::image_trim()%>%
    magick::image_ggplot(interpolate = TRUE)
  
  table_img
}





add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.05 * plot_width
    y_pos = 0.05 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

add_logo_trend <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.001 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

add_logo_map <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.135 * plot_width
    y_pos = 0.045 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

direction_label <- function(x_label = 50,
                            y_label = 70,
                            label_length = 20,
                            colour = "dimgray") {
  layer <- list(
    annotate(
      "segment",
      x = x_label - (label_length / 2),
      y = y_label,
      xend = x_label + (label_length / 2),
      yend = y_label,
      arrow = arrow(length = unit(0.02, "npc"),
                    type = "closed"),
      colour = colour
    ),
    annotate(
      "text",
      x = x_label,
      y = y_label - 1,
      label = c(""),
      vjust = 1.8,
      size = 3,
      colour = colour,angle=90,family="Consolas"
    )
  )
  
  return(layer)
}

gt_theme_538 <- function(data,...) {
  data %>%
    opt_table_font(
      font =list("Consolas",default_fonts())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = NULL, weight = px(2)
      ),
      
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data[["_data"]])
      )
      
    )%>%
    tab_options(
      column_labels.background.color = "#FFFFF0",
      table.border.top.width = px(3),
      table.border.top.color = "#FFFFF0",
      table.border.bottom.color = "#FFFFF0",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "#FFFFF000",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "grey45",
      data_row.padding = px(0),
      source_notes.font.size = 13,
      table.font.size = 14,
      heading.align = "center",
      heading.title.font.size  = 24,
      heading.subtitle.font.size = 14,
      table.background.color = "#FFFFF0",
      ...
    ) 
}

gt_theme_dark<- function(data,...) {
  data %>%
    opt_table_font(
      font =list("Consolas",default_fonts())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = NULL, weight = px(2)
      ),
      
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data[["_data"]])
      )
      
    )%>%
    tab_options(
      column_labels.background.color = "grey25",
      table.border.top.width = px(3),
      table.border.top.color = "grey25",
      table.border.bottom.color = "grey25",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "grey25",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "grey25",
      data_row.padding = px(0),
      source_notes.font.size = 13,
      table.font.size = 14,
      heading.align = "center",
      heading.title.font.size  = 24,
      heading.subtitle.font.size = 14,
      table.background.color = "#404040FF",
      ...
    ) 
}

# heat_colors <- grDevices::colorRampPalette(c("#800026FF", "#FC4E2AFF", "#FEB24CFF", "#FFFFCCFF"))(10)
# 
# heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 10, direction = 1)
# 
# heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(10)
# 
# heat_colors_interpolated %>% scales::show_col()
# 
# test_green <-paletteer_dynamic("dutchmasters::milkmaid", 10)
# heat_palette <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdYlGn", n = 9, direction = -1))(10)
# 
# 
# test_pal <- c("#FEB24CFF","#FECB76","#FEB24C","#FD8D3CFF","#FD9040","#FC6F35","#FC4E2AFF","#E31A1CFF","#BD0026FF","#800026FF")
# test_pal_1 <- c("#FEB24C","#FD8D3CFF","#E31A1CFF","#BD0026FF","#800026FF")
# 

