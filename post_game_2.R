
player_heat_map <- function(player_viz){
  touches <- events %>% clean_names() %>% filter(touches==T, player_name==player_viz)
  
  home_team <- events %>%
    filter(h_a=="h")%>%
    distinct(team_name)%>%
    pull(team_name)
  
  away_team <- events %>%
    filter(h_a=="a")%>%
    distinct(team_name)%>%
    pull(team_name)
  
  game_date <- as.Date(Sys.Date())
  touches %>%
    ggplot() +
    annotate_pitch(colour = "floralwhite",dimensions = pitch_opta,
                   fill = "grey25",
                   limits = FALSE)+
    geom_density_2d_filled(
      aes(x=x,y=y,fill = ..level..,alpha=..level..),
      contour_var = "ndensity", 
      breaks = seq(0.1, 1.0, length.out = 5) 
    )+
    ggsoccer::direction_label(colour = "floralwhite")+
    
    guides(color="none", fill = "none")+
    theme_pitch() +
    scale_fill_manual(values = c(test_pal_1),aesthetics = c("fill", "color"))+
    
    #geom_text(data = summary_stats, aes(x=70, y=10, label = glue("Median shot dist: {round(shot_distance,1)}m")),size=2, hjust=0,color = 'floralwhite',family="Consolas",fontface="bold")+
    # geom_text(data = summary_stats, aes(x=65, y=10, label = glue("Open play xG per shot: {round(xg_per_shot,2)}")),size=2, hjust=0,color = 'floralwhite',family="Consolas",fontface="bold")+
    theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"), plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 15,hjust = 0.5),
          legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),strip.text = element_text(face="bold", size=20,color = "floralwhite",family = "Consolas"),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 15),
          axis.text.x = element_blank(),axis.text.y = element_blank(),plot.background = element_rect(color = NA, fill = "grey25"))+
    labs(title = glue("{player_viz}'s Touch Heat Map"), subtitle =glue("{home_team} v {away_team} | {game_date}"), caption = "Chart: Alfred (@CallmeAlfredo)")
  
  ggsave(glue("Charts/LFC/{player_viz}_pass_map.png"), width = 8, height = 8, dpi=300,type='cairo')
}

map(events%>%clean_names()%>%filter(h_a=="h")%>%
      distinct(player_name)%>%pull(player_name),player_heat_map)


touches %>%
  ggplot() +
  annotate_pitch(colour = "floralwhite",dimensions = pitch_opta,
                 fill = "grey25",
                 limits = TRUE)+
  geom_point(aes(x=x,y=y),color="firebrick1")+
  ggsoccer::direction_label(colour = "floralwhite")+
  
  guides(color="none", fill = "none")+
  theme_pitch() +
  scale_fill_manual(values = c(test_pal_1),aesthetics = c("fill", "color"))+
  
  #geom_text(data = summary_stats, aes(x=70, y=10, label = glue("Median shot dist: {round(shot_distance,1)}m")),size=2, hjust=0,color = 'floralwhite',family="Consolas",fontface="bold")+
  # geom_text(data = summary_stats, aes(x=65, y=10, label = glue("Open play xG per shot: {round(xg_per_shot,2)}")),size=2, hjust=0,color = 'floralwhite',family="Consolas",fontface="bold")+
  theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"), plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 15,hjust = 0.5),
        legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),strip.text = element_text(face="bold", size=20,color = "floralwhite",family = "Consolas"),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 15),
        axis.text.x = element_blank(),axis.text.y = element_blank(),plot.background = element_rect(color = NA, fill = "grey25"))+
  labs(title = glue("{player_viz}'s Touch Map"), subtitle =glue("{home_team} v {away_team} | {game_date}"), caption = "Chart: Alfred (@CallmeAlfredo)")

ggsave(glue("Charts/LFC/{player_viz}_pass_map_1.png"), width = 8, height = 8, dpi=300,type='cairo')



touches <- events %>% clean_names() %>% filter(touches==T)

touches <- touches %>%
  mutate(x=to_int$x(x),
         y=to_int$y(y))

touch_map<- function(player){
  touches %>% filter(player_name==player)%>% 
    ggplot()+
    annotate_pitch(colour = "floralwhite",dimensions = pitch_international,
                   fill = "grey25",
                   limits = TRUE)+
    geom_point(aes(x=x,y=68-y),fill="firebrick1",shape=21,size=4)+
    #ggsoccer::direction_label(colour = "floralwhite") +
    theme_pitch()+
    coord_flip(ylim = c(0, 68))+
    theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
          legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
          panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
          plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
          plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0,vjust=-2),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
    
    #  draw_image(image = xg_map_df%>%filter(h_a=="h")%>%
    #                     distinct(logo)%>%
    #                    pull(logo),x=50,y=50,scale = 0.5)+
    draw_text(glue(touches%>%filter(player_name==player)%>%
                     distinct(player_name)%>%pull(player_name)),y=1,x=106,fontface="bold",color="floralwhite",size=20,family="Consolas",hjust=0,vjust=1)
  # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
  # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
  # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
}

touch_map("Mohamed Salah")+touch_map("Sadio Mané")+plot_annotation(title = "Touch Map",
                                                                   theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                 plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                   caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")

ggsave("Charts/LFC/f3rd_pass_map.png", width = 10, height = 10, dpi=300,type='cairo')


epl_events_analytical <- epl_events_1%>% clean_names()%>%
  select(match_id,team_name,h_a,player_name,time_seconds,x,y,finalX=end_x,finalY=end_y,type,outcome_type,x_t)%>%
  #group_by(match_id)%>%
  #rowwise()%>%
  calculate_threat()%>%
  mutate(x_t_shaker=xTEnd-xTStart)

epl_events_analytical_1 <- epl_events_analytical %>% filter(type %in% c("Pass","Carry"),outcome_type=="Successful")%>%
  mutate(distance=round(distance_function(x,y,finalX,finalX),1),
         is_carry=ifelse(type=="Carry"&distance>=5,T,F),
         x_t=ifelse(type=="Carry"&is_carry==F,0,x_t),
         x_t_shaker=ifelse(type=="Carry"&is_carry==F,0,x_t_shaker))

check <-epl_events_analytical_1 %>% 
  group_by(player_name)%>%
  summarise(x_t=sum(x_t,na.rm = T),x_t_shaker=sum(x_t_shaker,na.rm = T))%>%
  arrange(-x_t)

afcon_xg <- predict(aa_xg_model, afcon_shots, type = "prob")%>% select(xG=.pred_Goal)%>% 
  bind_cols(afcon_shots)%>%
  left_join(afcon_events%>% select(id,goalOwn,period,shotBlocked))%>%
  mutate(goalOwn=ifelse(is.na(goalOwn),F,goalOwn),
         team_name=case_when(goalOwn==T&team_name==home_team~away_team,
                             goalOwn==T&team_name==away_team~home_team,
                             TRUE~team_name),
         h_a=case_when(goalOwn==T&h_a=="h"~"a",
                       goalOwn==T&h_a=="a"~"h",
                       TRUE~h_a),
         xG=ifelse(goalOwn==T,0,xG),xG=as.numeric(xG))

min_xg=min(afcon_xg$xG)

ghana_shots %>% filter(situation!="Penalty",goalOwn==F)%>%
  mutate(X = X*100,
         Y = Y*100) %>% 
  ggplot()+
  annotate_pitch(colour = "floralwhite",dimensions = pitch_opta,fill="grey25",limits = F)+
  geom_point(
    aes(x=X, y = Y,size = xG,fill=is_goal,color=is_goal),
    shape = 21
  ) +
  guides(fill='none',color='none',size=guide_legend(override.aes=list(color="floralwhite")))+
  theme_pitch() +
  #scale_size(range = c(min_xg,5))+
  scale_color_manual(values = c("Goal"="#1E6C5B","No Goal"="#C0212E"))+
  scale_fill_manual(values = c("Goal"="#1E6C5B","No Goal"="#C0212E"))+
  theme_pitch() +
  coord_flip(xlim = c(30,100),
             ylim = c(0,100)) +
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#C0212E", fill="#C0212E")+
  annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=ghana_shots%>%summarize(shots=n())%>%pull(shots),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=7.5, color="floralwhite",label='Shots',family='Consolas',fontface="bold",size=5)+
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 20,ymax=25, color="#929642", fill="#929642")+
  annotate(geom = "text", x=32.5,y=22.5, color="floralwhite",label=ghana_shots%>%filter(shotOnTarget==T)%>%summarize(shots=n())%>%pull(shots),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=24, color="floralwhite",label='On Target',family='Consolas',fontface="bold",size=5)+
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 35,ymax=40, color="#1E6C5B", fill="#1E6C5B")+
  annotate(geom = "text", x=32.5,y=37.5, color="floralwhite",label=ghana_shots%>%filter(is_goal=="Goal")%>%summarize(goals=n())%>%pull(goals),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=37.5, color="floralwhite",label='Goals',family='Consolas',fontface="bold",size=5)+
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 53,ymax=60, color="#F59752", fill="#F59752")+
  annotate(geom = "text", x=32.5,y=56.5, color="floralwhite",label=ghana_shots%>%summarize(xG=round(sum(xG),2))%>%pull(xG),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=57, color="floralwhite",label='xG',family='Consolas',fontface="bold",size=5)+
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 68,ymax=75, color="#FEC753", fill="#FEC753")+
  annotate(geom = "text", x=32.5,y=71.5, color="floralwhite",label=ghana_shots%>%summarize(xG=sum(xG),shots=n(),xg_shot=round(xG/shots,2))%>%pull(xg_shot),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=72.5, color="floralwhite",label='xG/shot',family='Consolas',fontface="bold",size=5)+
  annotate(geom = "rect", xmin=30,xmax=35,ymin = 83,ymax=90, color="#BFCDD4", fill="#BFCDD4")+
  annotate(geom = "text", x=32.5,y=86.5, color="floralwhite",label=glue("{ghana_shots%>%summarize(goals=sum(is_goal=='Goal'),shots=n(),conv=round(goals*100/shots,0))%>%pull(conv)}%"),family='Consolas',fontface="bold",size=5)+
  annotate(geom = "text", x=29,y=87.5, color="floralwhite",label='Conv. ratio',family='Consolas',fontface="bold",size=5)+
  
  theme(plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"), plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 12,hjust = 0.5),
        legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),strip.text = element_text(face="bold", size=9,color = "floralwhite",family = "Consolas"),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10),
        axis.text.x = element_blank(),axis.text.y = element_blank(),plot.background = element_rect(color = NA, fill = "grey25"))+
  labs(title = "Ghana xG Shot Map", subtitle = "AFCON 2021 | Excludes penalties and own goals", caption = "Data: Opta<br>Chart: @CallmeAlfredo")

ggsave("Charts/AFCON/shot_map.png",width = 10, height = 8, dpi=300,type='cairo')

add_logo("Charts/AFCON/shot_map.png","afcon_dark.png","top right") %>%
  image_write("Charts/AFCON/shot_map.png")


p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})

facet_id_df <- big5_elite[match(facet_id, big5_elite$player_abb),]


p_bld$layout$z[grob_strip_index] <- 0

for (i in 1:nrow(facet_id_df)) {
  plr <- facet_id_df[i,]
  player_txt <- grid::textGrob(plr$player_abb, x=unit(0.18, 'npc'), gp=grid::gpar(col='floralwhite', fontfamily='Verdana', fontsize = 12, fontface="bold"), hjust=0)
  player_head <- grid::rasterGrob(image = image_read(plr$league_logo), x = 0.02, y = 0, height = 0.8, hjust = 0, vjust = 0)
  #player_team <- grid::rasterGrob(image = image_read(plr$logo), x = 0.95, y = 0.5, height = 0.8, hjust = 0.8)
  player_tree <- grid::grobTree(player_txt,player_head)
  
  p_bld$grobs[[grob_strip_index[i]]] <- player_tree
}

p_raw <- ggdraw(p_bld)

ggsave("Charts/big5_shot_map.png", p_raw,width = 8, height = 8, dpi=300,type='cairo')


# switch masters

game_passes <- events%>% clean_names()%>%
  select(match_id,id,minute,second,h_a,team_name,player_name,x,y,end_x,end_y,period,type,outcome_type,is_shot,shot_body_type,is_touch,pass_corner,pass_freekick,throw_in,pass_cross_accurate,pass_cross_inaccurate,pass_key,assist,
         big_chance_created)%>% filter(type=="Pass")%>%
  mutate(pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T)~"Set Piece",
                                  TRUE~"Open Play"),
         cross=ifelse(pass_cross_accurate==T|pass_cross_inaccurate==T,TRUE,FALSE),
         initial_dist_to_goal=round(distance_function(x,y,105,34),1),
         final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
         prog=(final_dist_to_goal-initial_dist_to_goal)/initial_dist_to_goal,
         is_progressive=ifelse((final_dist_to_goal/initial_dist_to_goal)<0.75,T,F),
         x=to_int$x(x),
         y=to_int$y(y),
         end_x=to_int$x(end_x),
         end_y=to_int$y(end_y),
         is_switch=ifelse((end_y-y)^2>=36.57^2,T,F)
  )

pass_sum <- game_passes %>% filter(pass_situation=="Open Play", player_name!="Jordan Pickford") %>%
  group_by(player_name)%>%
  summarise(pass_attempts=n(), switch_attempts= sum(is_switch==T),switch_succ=sum(is_switch==T&outcome_type=="Successful"))%>%
  mutate(switch_pct=switch_attempts/pass_attempts,switc_succ_pct=switch_succ/switch_attempts)%>%
  filter(pass_attempts>=500,switch_attempts>=20) %>%
  arrange(-switch_pct)%>%
  dplyr::slice(1:20)%>%
  left_join(teams_player_url)


game_passes_1 <- game_passes %>%
  filter(pass_situation=="Open Play",is_switch==T,player_name %in% pass_sum$player_name)%>%
  left_join(pass_sum) 

p<- game_passes_1 %>% 
  ggplot()+
  annotate_pitch(colour = "floralwhite",dimensions = pitch_international,fill="grey25",limits = F)+
  direction_label(colour = "floralwhite",)+
  geom_link(aes(x=x, y=y, xend = end_x, yend = end_y,
                alpha=stat(index),size=stat(I(2*index)),color=outcome_type)) +
  geom_point(aes(x=end_x,y=end_y,fill=outcome_type), shape = 21, col = "floralwhite",stroke=.5,size=2) +
  theme_pitch()+
  scale_fill_manual(values = c("Successful"="#2398B4FF","Unsuccessful"="grey80"))+
  scale_color_manual(values = c("Successful"="#2398B4FF","Unsuccessful"="grey80"))+
  #coord_flip(ylim = c(0, 68))+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 14,hjust = 0.5,vjust=-2),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,vjust=0.5,face = "bold"),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  facet_wrap(~reorder(player_name,-switch_pct),ncol = 4)+
  # draw_text(glue("% of passes that are switches - {game_passes_1%>% distinct(player_name,.keep_all=T)%>%mutate(switch_pct=round(switch_pct*100,1))%>%pull(switch_pct)}%"),y=1,x=-5,fontface="bold",color="floralwhite",size=5,family="Consolas",hjust=0,vjust=0)+
  # draw_text(glue("Switch success rate - {game_passes_1 %>%distinct%>%(player_name,.keep_all=T) %>%pull(round(switc_succ_pct,1))}%"),y=1,x=-8,fontface="bold",color="floralwhite",size=5,family="Consolas",hjust=0,vjust=0)+
  labs(title = glue("PL Kings of Switches: <span style='color:#2398B4FF;'>Successful</span> and <span style='color:grey80;'>Unsuccessful</span> Switches of Play"), subtitle = "Season: 2021-22 | Only open play passes included<br>Min 500 pass attempts and 20 switch attempts", caption = "Players ordered by % of pass attempts that are switches<br>Chart: Alfred (@CallmeAlfredo)")

p_bld <- ggplot_gtable(ggplot_build(p))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})

facet_id_df <- pass_sum[match(facet_id, pass_sum$player_name),]


p_bld$layout$z[grob_strip_index] <- 0

for (i in 1:nrow(facet_id_df)) {
  plr <- facet_id_df[i,]
  player_txt <- grid::textGrob(plr$player_name, x=unit(0.43, 'npc'), gp=grid::gpar(col='floralwhite', fontfamily='Consolas', fontsize = 10, fontface="bold"), hjust=0.5)
  #player_head <- grid::rasterGrob(image = image_read(plr$club_logo), x = 0.02, y = 0, height = 0.8, hjust = 0, vjust = 0)
  player_team <- grid::rasterGrob(image = image_read(plr$image_url), x = 0.1, y = 0.5, height = 1, hjust = 0.8)
  player_tree <- grid::grobTree(player_txt,player_team)
  
  p_bld$grobs[[grob_strip_index[i]]] <- player_tree
}

p_raw <- ggdraw(p_bld)+
  theme(plot.background = element_rect(fill="grey25", color = NA))

ggsave("Charts/LFC/switches.png",p_raw,width = 12, height = 12, dpi=300,type='cairo')

#xG 
game_shots <- prep_shot_df(events)


# Get home and away teams
home_team <- events %>% filter(h_a=="h")%>%
  distinct(team_name)%>% 
  pull(team_name)
away_team <- events %>% filter(h_a=="a")%>%
  distinct(team_name)%>% 
  pull(team_name)


game_xg <- predict(aa_xg_model, game_shots, type = "prob")%>% select(xG=.pred_Goal)%>% 
  bind_cols(game_shots)%>%
  left_join(events%>% select(id,goalOwn,period,shotBlocked))%>%
  mutate(goalOwn=ifelse(is.na(goalOwn),F,goalOwn),
         team_name=case_when(goalOwn==T&team_name==home_team~away_team,
                             goalOwn==T&team_name==away_team~home_team,
                             TRUE~team_name),
         h_a=case_when(goalOwn==T&h_a=="h"~"a",
                       goalOwn==T&h_a=="a"~"h",
                       TRUE~h_a),
         xG=ifelse(goalOwn==T,0,xG),xG=as.numeric(xG))

home_goals <- game_xg %>% filter(h_a == 'h') %>% 
  summarize(goals = sum(is_goal == 'Goal')) %>%
  pull(goals)
away_goals <- game_xg %>% filter(h_a == 'a') %>% 
  summarize(goals = sum(is_goal == 'Goal')) %>%
  pull(goals)
last_minute <- max(events$expandedMinute)
minute <- c(0:last_minute)
team_name <- c(game_xg %>% filter(h_a=="h")%>%distinct(team_name)%>%pull(team_name),game_xg %>% filter(h_a=="a")%>%distinct(team_name)%>%pull(team_name))

xg_roll_sum <- game_xg%>%
  # Adjust minutes for HT and FT 
  mutate(minute=case_when(minute>45&period=="FirstHalf"~45,
                          TRUE~minute))%>%
  full_join(crossing(minute,team_name))%>%
  arrange(minute)%>%
  group_by(team_name)%>%
  mutate(xG = if_else(is.na(xG), 0, xG),
         rollsum = lag(cumsum(xG)))%>%
  ungroup()%>%
  mutate(rollsum_goal = rollsum + xG,
         rollsum = if_else(is.na(rollsum), 0, rollsum),
         rollsum_goal = if_else(is.na(rollsum_goal), 0, rollsum_goal)) 

xg_roll_sum_1 <- xg_roll_sum %>% 
  left_join(game_xg %>%
              filter(is_goal == "Goal"&goalOwn==F) %>% select(minute, is_goal_lab=is_goal, team_name, player_lab=player), 
            by = c("minute", "team_name")) %>% 
  mutate(
    minute_goal = minute + 1,
    player_label = case_when(
      (is_goal_lab == "Goal") ~ glue::glue("{player_lab}, {round(xG,2)} xG"),
      TRUE ~ ""),
    team_color=ifelse(team_name==home_team,"#2398B4","#DCA62C"))


xg_roll_sum_1 %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team_name, color = team_color)) +
  geom_step(size = 2.5) +
  geom_text_repel(data = xg_roll_sum_1 %>% filter(is_goal_lab == "Goal"),
                  aes(x = minute_goal, y = rollsum_goal, 
                      color = team_color, label = player_label), 
                  nudge_x =-8, nudge_y = 0.15, family = "Consolas",fontface="bold",
                  show.legend = FALSE) +
  geom_point(data = xg_roll_sum_1 %>% filter(is_goal_lab == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, color = team_color), show.legend = FALSE,
             size = 5, shape = 21, fill = "white", stroke = 1.25) +
  scale_x_continuous(breaks = c(seq(0, last_minute, by = 5), last_minute),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, last_minute, by = 5), last_minute),
                     #seq(100, 124, by = 5),""),
                     
                     expand = c(0.01, 0),
                     limits = c(0, last_minute)) +
  scale_color_identity()+
  AAtheme_patch+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
        axis.text.x = element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
        panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
        panel.grid.minor = element_blank(),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  
  labs(title = glue("<span style='color:#2398B4;'>{team_name[1]}:{game_xg%>%filter(h_a=='h')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span> vs
                    <span style='color:#DCA62C;'>{team_name[2]}:{game_xg%>%filter(h_a=='a')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span>"),
       subtitle = "",caption = "Data: Opta<br>Chart by Alfred (@CallmeAlfredo)",
       x = NULL,
       y = "Cumulative xG")
ggsave("Charts/LFC/xg_timeline.png", width = 9.5, height = 6, dpi=300,type='cairo')



# Progressive passing 


select_player_map <- function(player_prog_viz){
  
  game_date <- events %>% clean_names()%>%
    select(start_date)%>%
    mutate(start_date=as.Date(as.character(start_date)))%>% na.omit()%>%
    distinct(start_date)%>%
    pull(start_date)
  # Passes 
  game_passes <- pre_process_pass(events)
  
  #xG 
  game_shots <- prep_shot_df(events)
  
  
  # Get home and away teams
  home_team <- events %>% filter(h_a=="h")%>%
    distinct(team_name)%>% 
    pull(team_name)
  away_team <- events %>% filter(h_a=="a")%>%
    distinct(team_name)%>% 
    pull(team_name)
  
  
  game_xg <- predict(aa_xg_model, game_shots, type = "prob")%>% select(xG=.pred_Goal)%>% 
    bind_cols(game_shots)%>%
    left_join(events%>% select(id,goalOwn,period,shotBlocked))%>%
    mutate(goalOwn=ifelse(is.na(goalOwn),F,goalOwn),
           team_name=case_when(goalOwn==T&team_name==home_team~away_team,
                               goalOwn==T&team_name==away_team~home_team,
                               TRUE~team_name),
           h_a=case_when(goalOwn==T&h_a=="h"~"a",
                         goalOwn==T&h_a=="a"~"h",
                         TRUE~h_a),
           xG=ifelse(goalOwn==T,0,xG),xG=as.numeric(xG))
  
  home_goals <- game_xg %>% filter(h_a == 'h') %>% 
    summarize(goals = sum(is_goal == 'Goal')) %>%
    pull(goals)
  away_goals <- game_xg %>% filter(h_a == 'a') %>% 
    summarize(goals = sum(is_goal == 'Goal')) %>%
    pull(goals)
  
game_xt <- use_xt(events)
prog_pass_map <- game_xt %>% filter(pass_situation=="Open Play")%>%
  filter(player_name==player_prog_viz)%>%
  mutate(x_adj=to_int$x(x),
         y_adj=to_int$y(y),
         end_x_adj=to_int$x(end_x),
         end_y_adj=to_int$y(end_y))

pass_map <- prog_pass_map %>% filter(outcome_type=="Successful",type=="Pass")%>%
  ggplot()+
  annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
#  direction_label(colour = "grey65",)+
  geom_link(aes(x=x_adj, y=68-y_adj, xend = end_x_adj, yend = 68-end_y_adj,
                alpha=stat(index),size=stat(I(2*index)),color=is_progressive)) +
  geom_point(aes(x=end_x_adj,y=68-end_y_adj,fill=is_progressive), shape = 21, col = "floralwhite",stroke=.5,size=2) +
  theme_pitch()+
  scale_fill_manual(values = c("TRUE"="#2398B4","FALSE"="#DCA62C"))+
  scale_color_manual(values = c("TRUE"="#2398B4","FALSE"="#DCA62C"))+
  coord_flip(ylim = c(0, 68))+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0,vjust=-2),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  draw_text("Passes (Open Play)",y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
  draw_text(glue("Total Completed Passes - {prog_pass_map %>% 
    filter(outcome_type=='Successful',type=='Pass')%>%
             summarise(passes=n())%>%
             pull(passes)}"),y=1,x=-5,fontface="bold",color="#DCA62C",size=10,family="Consolas",hjust=0,vjust=0)+
  draw_text(glue("Completed Progressive Passes - {prog_pass_map %>% 
    filter(outcome_type=='Successful',type=='Pass',is_progressive==T)%>%
             summarise(passes=n())%>%
             pull(passes)}"),y=1,x=-8,fontface="bold",color="#2398B4",size=10,family="Consolas",hjust=0,vjust=0)


#Ball Carries

game_carries <- events %>% clean_names() %>% filter(type %in% c("Pass","Carry"))%>%
  arrange(minute,second)%>%
  select(id,minute,h_a,player_name,team_name,type,x,y,end_x,end_y,outcome_type)%>%
  mutate(pass_recipient=case_when(type=="Pass"&outcome_type=="Successful"&team_name==lead(team_name)~lead(player_name)),
         distance=round(distance_function(x,y,end_x,end_y),1),
         is_carry=ifelse(type=="Carry"&distance>=5,T,F),
         initial_dist_to_goal=round(distance_function(x,y,105,34),1),
         final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
         change_carry_distance=final_dist_to_goal-initial_dist_to_goal,
         is_progressive_carry=ifelse(is_carry==T&change_carry_distance<=-5,T,F),
         x_adj=to_int$x(x),
         y_adj=to_int$y(y),
         end_x_adj=to_int$x(end_x),
         end_y_adj=to_int$y(end_y)
  )

carries_map <- game_carries %>% filter(is_carry==T,player_name==player_prog_viz)%>%
  ggplot()+
  annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
  geom_link(aes(x=x_adj, y=68-y_adj, xend = end_x_adj, yend = 68-end_y_adj,
                alpha=stat(index),size=stat(I(2*index)),col=is_progressive_carry)) +
  geom_point(aes(x=end_x_adj,y=68-end_y_adj, col=is_progressive_carry), shape = 21,stroke=.5,size=2) +
  scale_fill_manual(values = c("TRUE"="#2398B4","FALSE"="#DCA62C"))+
  scale_color_manual(values = c("TRUE"="#2398B4","FALSE"="#DCA62C"))+
  theme_pitch()+
  draw_text("Carries",y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
  coord_flip(ylim = c(0, 68))+
  
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0,vjust=-2),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  draw_text(glue("Total Carries - {game_carries %>% filter(is_carry==T,player_name==player_prog_viz)%>%
             summarise(carries=n())%>%
             pull(carries)}"),y=1,x=-5,fontface="bold",color="#DCA62C",size=10,family="Consolas",hjust=0,vjust=0)+
  draw_text(glue("Progressive Carries - {game_carries %>% filter(is_progressive_carry==T,player_name==player_prog_viz)%>%
             summarise(carries=n())%>%
             pull(carries)}"),y=1,x=-8,fontface="bold",color="#2398B4",size=10,family="Consolas",hjust=0,vjust=0)

# Game defense actions
game_defense <- events %>% clean_names() %>% 
  filter(type %in% c('Clearance','Interception','Tackle','BlockedPass','BallRecovery'))%>%
  mutate(
    x_adj=to_int$x(x),
    y_adj=to_int$y(y)
  )

defensive_map <- game_defense %>% filter(player_name==player_prog_viz)%>%
  ggplot()+
  annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
  geom_point(aes(x=x_adj,y=68-y_adj, col=outcome_type,fill=outcome_type), shape = 21,stroke=.5,size=3) +
  scale_fill_manual(values = c("Successful"="#2398B4","Unsuccessful"="grey80"))+
  scale_color_manual(values = c("Successful"="#2398B4","Unsuccessful"="grey80"))+
  theme_pitch()+
  draw_text("Defensive Actions",y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
  coord_flip(ylim = c(0, 68))+
  
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0,vjust=-2),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  draw_text(glue("Successful Defensive Actions - {game_defense %>% filter(outcome_type=='Successful',player_name==player_prog_viz)%>%
             summarise(carries=n())%>%
             pull(carries)}"),y=1,x=-5,fontface="bold",color="#2398B4",size=10,family="Consolas",hjust=0,vjust=0)+
  draw_text(glue("Total Defensive Actions - {game_defense %>% filter(player_name==player_prog_viz)%>%
             summarise(carries=n())%>%
             pull(carries)}"),y=1,x=-8,fontface="bold",color="grey80",size=10,family="Consolas",hjust=0,vjust=0)


(pass_map+carries_map+defensive_map)+plot_annotation(title = glue("{player_prog_viz}'s Ball Progression and Defensive Actions"),
                                     subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                     theme = theme(plot.title=element_markdown(face="bold", size=22,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                   plot.subtitle=element_markdown(face="bold", size=12,family="Consolas",hjust=0.5, color = "grey65"),
                                                   plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                     caption = "Data: Opta <br>A progressive carry is any carry that moves the ball at least five meters towards the centre of the opponent's goal<br>A progressive pass is any pass that moves the ball at least 25% closer to the centre of the opponent's goal<br>Defensive actions include clearances, blocked passes, tackles, interceptions and ball recoveries<br>Chart by Alfred (@CallmeAlfredo)")

ggsave(glue("Charts/LFC/player_{player_prog_viz}.png"), width = 15, height = 10, dpi=300,type='cairo')

}

select_player_map("Joel Matip")
select_player_map("Luis Díaz")
select_player_map("Jordan Henderson")
select_player_map("Virgil van Dijk")
select_player_map("Takumi Minamino")
select_player_map("James Milner")

select_player_map("Konstantinos Tsimikas")
select_player_map("Darwin Núñez")
select_player_map("Erling Haaland")
select_player_map("Andrew Robertson")
select_player_map("Trent Alexander-Arnold")

select_player_map("Mohamed Salah")

select_player_map("Naby Keïta")

select_player_map("Alexander Djiku")

select_player_map("Thiago")

select_player_map("Fabinho")

select_player_map("Roberto Firmino")

select_player_map("Harvey Elliott")

select_player_map("Mohammed Salisu")

select_player_map("Jordan Ayew")

select_player_map("Vinícius Júnior")


glimpse(test)

event_types <-events %>% clean_names() %>% 
  group_by(type) %>% 
  tally()

game_date <- events %>% clean_names()%>%
  select(start_date)%>%
  mutate(start_date=as.Date(as.character(start_date)))%>% na.omit()%>%
  distinct(start_date)%>%
  pull(start_date)


#xG 
game_shots <- prep_shot_df(events)


# Get home and away teams
home_team <- events %>% filter(h_a=="h")%>%
  distinct(team_name)%>% 
  pull(team_name)
away_team <- events %>% filter(h_a=="a")%>%
  distinct(team_name)%>% 
  pull(team_name)



game_xg <- predict(aa_xg_model, game_shots, type = "prob")%>% select(xG=.pred_Goal)%>% 
  bind_cols(game_shots)%>%
  left_join(events%>% select(id,goalOwn,period,shotBlocked))%>%
  mutate(goalOwn=ifelse(is.na(goalOwn),F,goalOwn),
         team_name=case_when(goalOwn==T&team_name==home_team~away_team,
                             goalOwn==T&team_name==away_team~home_team,
                             TRUE~team_name),
         h_a=case_when(goalOwn==T&h_a=="h"~"a",
                       goalOwn==T&h_a=="a"~"h",
                       TRUE~h_a),
         xG=ifelse(goalOwn==T,0,xG),xG=as.numeric(xG))

home_goals <- game_xg %>% filter(h_a == 'h') %>% 
  summarize(goals = sum(is_goal == 'Goal')) %>%
  pull(goals)
away_goals <- game_xg %>% filter(h_a == 'a') %>% 
  summarize(goals = sum(is_goal == 'Goal')) %>%
  pull(goals)

xt_max <-events %>% clean_names()%>%
  filter(type=="Pass",outcome_type=="Successful") %>% 
  group_by(player_name) %>% 
  summarise(xt=sum(x_t,na.rm = T)) %>% ungroup() %>% 
  summarise(xt=max(xt)) %>% 
  pull(xt)

xt_min <-events %>% clean_names()%>%
  filter(type=="Pass",outcome_type=="Successful") %>% 
  group_by(player_name) %>% 
  summarise(xt=sum(x_t,na.rm = T)) %>% ungroup() %>% 
  summarise(xt=min(xt)) %>% 
  pull(xt)











# xT
game_xt=use_xt(events)
game_xt %>% 
rename(finalX=end_x,finalY=end_y) %>% 
calculate_threat() %>% 
mutate(xT=xTEnd-xTStart) %>% 
filter(type %in% c("Pass"),xT>0,pass_situation=="Open Play") %>% 
  # mutate(distance=round(distance_function(x,y,finalX,finalY),1),
  #        is_carry=ifelse(type=="Carry"&distance>=5,T,F),
  #        x_t=ifelse(type=="Carry"&is_carry==F,0,x_t)) %>% 
group_by(player_name) %>% 
summarise(x_t=sum(x_t,na.rm=T),xT=sum(xT,na.rm=T)) %>% 
arrange(-xT)

game_xt %>% filter(type=="Pass") %>% 
  group_by(player_name) %>% 
  summarise(x_t=sum(x_t,na.rm=T)) %>% 
  arrange(-x_t)

events %>% clean_names() %>% 
  glimpse()

events %>% clean_names() %>% 
  mutate(dispossessed=as.logical(dispossessed)) %>% 
  filter(dispossessed==T) %>% 
  group_by(player_name) %>% 
    tally(
      
    ) %>% 
    arrange(-n)


touch_box <- events %>% clean_names() %>% 
  filter(touches==T) %>% 
  mutate(x_new=x*1.05,
         y_new=y*0.68,
         end_x_new=end_x*1.05,
         end_y_new=end_y*.68,
         touch_box=case_when(x_new>=88.5&abs(34-y_new)<20.16~"Box touch",
                             TRUE~"Non box touch"),
         x_adj=to_int$x(x),
         y_adj=to_int$y(y))

touch_box %>% 
  filter(touch_box=="Box touch") %>% 
  group_by(team_name) %>% 
  tally() %>% 
  arrange(-n)

touches <- touch_box %>% 
  filter(touch_box=="Box touch") %>% 
  group_by(team_name,player_name) %>% 
  tally() %>% 
  arrange(-n)


check <- touch_box %>% 
  filter(touch_box=="Box touch") %>% 
  group_by(player_name) %>% 
  tally() %>% 
  arrange(-n)



  image_read("https://d2zywfiolv4f83.cloudfront.net/img/teams/960.png")%>%
  image_quantize(colorspace = "gray")%>%
  image_write("logos/DR Congo.png")
  

game_passes <- pre_process_pass(events)
  
game_switches <- game_passes%>%
    mutate(x_new=x*1.05,
                    y_new=y*0.68,
                    end_x_new=end_x*1.05,
                    end_y_new=end_y*.68,
                    x_adj=to_int$x(x),
                    y_adj=to_int$y(y),
                    end_x_adj=to_int$x(end_x),
                    end_y_adj=to_int$y(end_y),
           is_switch=ifelse((end_y_new-y_new)^2>=36.57^2,T,F)
    )
  
switches_plot <- function(side){
  pass_final_third %>% filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>% 
    ggplot()+
    annotate_pitch(colour = "grey65",dimensions = pitch_international,fill="grey25",limits = F)+
    geom_link(aes(x=x_adj, y=68-y_adj, xend = end_x_adj, yend = 68-end_y_adj,
                  alpha=stat(index),size=stat(I(2*index)),color=outcome_type)) +
    geom_point(aes(x=end_x_adj,y=68-end_y_adj,fill=outcome_type), shape = 21, col = "floralwhite",stroke=.5,size=2) +
    theme_pitch()+
    scale_fill_manual(values = c("Successful"="#2398B4","Unsuccessful"="#DCA62C"))+
    scale_color_manual(values = c("Successful"="#2398B4","Unsuccessful"="#DCA62C"))+
    coord_flip(ylim = c(0, 68))+
    theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
          legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
          panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
          plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
          plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0,vjust=-2),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
    
    #  draw_image(image = xg_map_df%>%filter(h_a=="h")%>%
    #                     distinct(logo)%>%
    #                    pull(logo),x=50,y=50,scale = 0.5)+
    draw_text(glue(game_switches%>%filter(h_a==side)%>%
                     distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey65",size=20,family="Consolas",hjust=0,vjust=1)+
    annotate(geom = "rect", xmin=40,xmax=45,ymin = 5,ymax=10, color="#DCA62C", fill="#DCA62C")+
    annotate(geom = "text", x=42.5,y=7.5, color="floralwhite",label=game_switches %>% 
               filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>%
               summarise(attempts=n())%>%
               pull(attempts),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=38.5,y=7.5, color="floralwhite",label='Attempts',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#2398B4", fill="#2398B4")+
    annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=game_switches %>% 
               filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>%
               summarise(comps=sum(outcome_type=="Successful"))%>%
               pull(comps),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=28.5,y=7.5, color="floralwhite",label='Successful',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=20,xmax=24,ymin = 4,ymax=11, color="#8A4BDE", fill="#8A4BDE")+
    annotate(geom = "text", x=22.5,y=7.5, color="floralwhite",label=game_switches %>% 
               filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>%
               summarise(comps=sum(outcome_type=="Successful"),attempts=n(),rate=glue("{round(comps*100/attempts,1)}%"))%>%
               pull(rate),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=18.5,y=7.5, color="floralwhite",label='Success %',family='Consolas',fontface="bold")
  # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
  # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
  # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
}

switches_table <- function(side){
  pass_switches %>% filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>%
    group_by(player_name)%>%
    summarise(attempts=n(),comp=sum(outcome_type=="Successful"))%>%
    arrange(-attempts) %>%
    dplyr::slice(1:5) %>%
    mutate(comp_pct=comp/attempts)%>%
    gt()%>%
    fmt_percent(columns = c(comp_pct),decimals = 0)%>%
    #fmt_number(columns = c(cpoe,xt_p90),decimals = 2)%>%
    tab_header(title = md("**Leaders**"))%>%
    # tab_header(title = html(glue("<p><img src='home.png' alt='pl logo' style='float:right;height:100px;'></p>
    #                             <p> <span style='font-size:30px;font-weight:bold'>Leaders</span></p>")))%>%
    
    data_color(columns = c(attempts), colors = scales::col_numeric(palette = c("grey25","#DCA62C"),
                                                                   domain = NULL))%>%
    data_color(columns = c(comp), colors = scales::col_numeric(palette = c("grey25","#2398B4"),
                                                               domain = NULL))%>%
    data_color(columns = c(comp_pct), colors = scales::col_numeric(palette = c("grey25","#8A4BDE"),
                                                                   domain = NULL))%>%
    
    cols_label(
      player_name = md("**Player**"),
      attempts = md("**Attempts**"),
      comp = md("**Successful**"),
      comp_pct=md("**Success %**"))%>%
    cols_align(
      align = "center",
      columns = c(attempts,comp,comp_pct)
    )%>%
    # tab_source_note(md(glue("**Minimum 100 pass attempts. xT values from Karun Singh's xT model<br> Table: @CallmeAlfredo**")))%>%
    gt_theme_dark() %>%
    tab_options(heading.title.font.size = 15)%>%
    as_ggplot() 
}

((switches_plot("h")/switches_table("h")+plot_layout(heights = c(1,.2)))|(switches_plot("a")/switches_table("a")+plot_layout(heights = c(1,.2))))+plot_annotation(title = "Switches of Play (Open Play Only)",
                                                                                                                                                                                        subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                                                                                                                                                        theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                                                                                                                                      plot.subtitle=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "grey65"),
                                                                                                                                                                                                      plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                                                                                                                                        caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")


ggsave("Charts/LFC/switch_map.png", width = 10, height = 10, dpi=300,type='cairo')



passes_carries_df <- events%>% 
  clean_names() %>% 
  filter(type %in% c("Pass","Carry"),outcome_type=="Successful") %>%
  mutate(time_minutes=round(time_seconds/60,0),
         distance=round(distance_function(x,y,end_x,end_y),1),
         is_carry=ifelse(type=="Carry"&distance>=5,T,F),
         x_t=ifelse(type=="Carry"&is_carry==F,0,x_t)) %>%
  select(h_a,period,time_seconds,time_minutes,expanded_minute,team_name,type,x_t)

home_team <- events %>% filter(h_a=="h")%>%
  distinct(team_name)%>% 
  pull(team_name)
away_team <- events %>% filter(h_a=="a")%>%
  distinct(team_name)%>% 
  pull(team_name)

all_df <-passes_carries_df %>% 
  mutate(time_minutes=case_when(time_minutes>45&period=="FirstHalf"~45,
                                TRUE~time_minutes))%>%

  group_by(period,h_a,team_name,time_minutes)%>%
  summarise(xt=sum(x_t))%>%
  ungroup()%>%
  group_by(time_minutes) %>%
  mutate(xt_diff=case_when(xt>lead(xt)~xt-lead(xt),
                           xt<lead(xt)~lead(xt)-xt),
         
         xt_diff=ifelse(!is.na(xt_diff),xt_diff,0))%>%
  ungroup() %>%
  arrange(time_minutes)%>%
  group_by(time_minutes)%>%
  mutate(
    team2= ifelse(xt>lead(xt),team_name,lead(team_name)),
    team2= case_when(is.na(team2)&lag(team2)==home_team~away_team,
                     is.na(team2)&lag(team2)==away_team~home_team,
                     TRUE~team2))%>%
  select(team_name=team2,time_minutes,xt_diff,period)%>% 
  left_join(passes_carries_df%>%select(team_name,h_a)%>%distinct(team_name,h_a))%>%
  mutate(xt_diff=ifelse(h_a=="h",abs(xt_diff),abs(xt_diff)*-1)) %>%na.omit()

max_time <-max(passes_carries_df$time_minutes)

all_df_1 <-passes_carries_df %>% 
  mutate(time_minutes=case_when(time_minutes>45&period=="FirstHalf"~45,
                                TRUE~time_minutes))%>%
  mutate(time_interval=cut(time_minutes,breaks=seq(0,max_time,5),include.lowest=T,labels=seq(5,max_time,5))) %>% 
  group_by(period,h_a,team_name,time_interval)%>%
  summarise(xt=sum(x_t))%>%
  ungroup()%>%
  group_by(time_interval) %>%
  mutate(xt_diff=case_when(xt>lead(xt)~xt-lead(xt),
                           xt<lead(xt)~lead(xt)-xt),
         
         xt_diff=ifelse(!is.na(xt_diff),xt_diff,0))%>%
  ungroup() %>%
  arrange(time_interval)%>%
  group_by(time_interval)%>%
  mutate(
    team2= ifelse(xt>lead(xt),team_name,lead(team_name)),
    team2= case_when(is.na(team2)&lag(team2)==home_team~away_team,
                     is.na(team2)&lag(team2)==away_team~home_team,
                     TRUE~team2))%>%
  select(team_name=team2,time_interval,xt_diff,period)%>% 
  left_join(passes_carries_df%>%select(team_name,h_a)%>%distinct(team_name,h_a))%>%
  mutate(xt_diff=ifelse(h_a=="h",abs(xt_diff),abs(xt_diff)*-1)) %>%na.omit()



interp <- approx(all_df_1$time_interval, all_df_1$xt_diff, n=1000)
cbi <- data.frame(time_interval=interp$x, xt_diff=interp$y)
plot_data <- cbi%>%
  mutate(team_name=ifelse(xt_diff>=0,home_team,away_team),
         period = ifelse(time_interval <=9, "First Half", "Second Half"),
         time_interval_1=5*time_interval
  )



ggplot(data = plot_data, aes(x = time_interval_1, y = xt_diff))+ 
  geom_ribbon(aes(ymin=pmin(xt_diff,0), ymax=0), fill="#DCA62C", color="#DCA62C",  alpha=0.8) +
  geom_ribbon(aes(ymin=0, ymax=pmax(xt_diff,0)), fill="#2398B4", color= "#2398B4", alpha=0.8) +
  facet_wrap(~period,nrow = 1,scale="free_x")+
  geom_hline(yintercept = 0,size=.5,color="floralwhite")+
  scale_x_continuous(breaks = pretty_breaks(n=6),)+
  #scale_y_continuous(limits=c(-.12,.12))+
  AAtheme_patch+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
        panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 12,hjust = 0.5),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10),
        strip.text = element_text(face="bold", size=15,color = "floralwhite",family = "Consolas"))+
  labs(title = glue("<span style='color:#2398B4;'>{home_team}:{game_xg%>%filter(h_a=='h')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span> v <span style='color:#DCA62C;'>{away_team}:{game_xg%>%filter(h_a=='a')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span> - Expected Threat Timeline"), y="Difference in threats from passes and carries",x="Minute",
       caption = "xT values from Karun Singh's model <br>Data: Opta | Chart: @CallmeAlfredo")

ggsave("Charts/LFC/xt_timeline.png", width = 10.5, height = 8, dpi=300,type='cairo')

passes_carries_df <- events%>% 
  clean_names() %>% 
  filter(type %in% c("Pass","Carry"),outcome_type=="Successful") %>%
  rename(finalX=end_x,finalY=end_y) %>% 
  calculate_threat() %>% 
  mutate(xT=xTEnd-xTStart)


passes_carries_df %>% 
  group_by(team_name) %>% 
  summarise(xT=sum(xT,na.rm = T),x_t=sum(x_t,na.rm = T))

last_minute <- max(events$minute,na.rm = T)
labels <- game_xg %>%  
  mutate(player_label = dplyr::case_when(
    is_goal == "Goal" & situation != "Penalty" ~ paste0(player, ": ", round(xG, digits = 2), " xG"),
    is_goal == "Goal" & situation == "Penalty" ~ paste0(player, " (Penalty): ", round(xG, digits = 2), " xG"),
    goalOwn==T ~ paste0(player, " (Own Goal): ", round(xG, digits = 2), " xG"),
    TRUE ~ ""),
    minute = as.numeric(minute)) %>%
  group_by(h_a) %>% 
  mutate(xGsum = cumsum(xG)) %>% 
  ungroup() %>% 
  mutate(
    team_color=ifelse(team_name==home_team,"#2398B4","#DCA62C")
  )

home_color <- "#2398B4"
away_color <- "#DCA62C"

home_xg <- game_xg %>% 
  filter(h_a=="h") %>% 
  summarise(xg=sum(xG)) %>% 
  pull(xg)

away_xg <- game_xg %>% 
  filter(h_a=="a") %>% 
  summarise(xg=sum(xG)) %>% 
  pull(xg)

data1 <- game_xg %>%
  dplyr::filter(h_a == "h") %>%
  mutate(xGsum = cumsum(xG))
data2 <- game_xg %>%
  dplyr::filter(h_a == "a") %>%
  mutate(xGsum = cumsum(xG))

data1 <- dplyr::add_row(.data = data1 %>% mutate(goalOwn=as.logical(goalOwn)), .before = 1, 
                        id = 0, minute = 0, is_goal = "0", X = 0, Y = 0, xG = 0, player = "0", h_a = "0",
                        situation = "0", shotType = "0", match_id = 0, team_name= "0",lastAction = "0",period="0",
                        angle = 0, distance = 0, goalOwn=F,xGsum = 0) %>% 
  dplyr::add_row(
                 id = 0, minute = last_minute, is_goal = "0", X = 0, Y = 0, xG = 0, player = "0", h_a = "0",
                 situation = "0", shotType = "0", match_id = 0, team_name= "0",lastAction = "0",period="0",
                 angle = 0, distance = 0, goalOwn=F,xGsum = home_xg) 


data2 <- dplyr::add_row(.data = data2 %>% mutate(goalOwn=as.logical(goalOwn)), .before = 1, 
                        id = 0, minute = 0, is_goal = "0", X = 0, Y = 0, xG = 0, player = "0", h_a = "0",
                        situation = "0", shotType = "0", match_id = 0, team_name= "0",lastAction = "0",period="0",
                        angle = 0, distance = 0, goalOwn=F,xGsum = 0) %>% 
  dplyr::add_row(
                 id = 0, minute = last_minute, is_goal = "0", X = 0, Y = 0, xG = 0, player = "0", h_a = "0",
                 situation = "0", shotType = "0", match_id = 0, team_name= "0",lastAction = "0",period="0",
                 angle = 0, distance = 0, goalOwn=F,xGsum = away_xg) 

dat1 <- data1 %>%
  dplyr::filter(is_goal=='Goal')
d1 <- data1 %>%
  dplyr::filter(goalOwn==T)
dat1 <- bind_rows(dat1, d1)

dat2 <- data2 %>%
  dplyr::filter(is_goal=='Goal')
d2 <- data2 %>%
  dplyr::filter(goalOwn==T)
dat2 <- bind_rows(dat2, d2)

ggplot() +
  geom_step(data = data1, aes(x = minute, y = xGsum), color =home_color, size = 2.7) +
  geom_step(data = data2, aes(x = minute, y = xGsum), color = away_color, size = 2.7) +
  geom_point(data = dat1, aes(x = minute, y = xGsum), color = home_color, fill = "floralwhite", shape = 21, stroke = 2, size = 6) +
  geom_point(data = dat2, aes(x = minute, y = xGsum), color = away_color, fill = "floralwhite", shape = 21, stroke = 2, size = 6) +
  geom_text_repel(data = labels %>% filter(is_goal=="Goal"), 
                  aes(x = minute, y = xGsum, label = player_label,color=team_color), nudge_x =-8, nudge_y = 0.15, family = "Consolas",fontface="bold",
                  alpha=0.8,
                  show.legend = FALSE) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
  scale_color_identity()+
  geom_vline(xintercept = 45, linetype = "dashed", color = "firebrick1", size = 1)+
  AAtheme_patch+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
        axis.text.x = element_text(size=13, color="floralwhite",family="Consolas",face = "bold"),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
        panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
        panel.grid.minor = element_blank(),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0.5),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))



# Custom request 

home_first_sub <- events%>%
  filter(h_a=="h",type=="SubstitutionOff")%>%
  arrange(minute)%>%
  filter(minute==min(minute))%>%
  pull(minute)
away_first_sub <- events%>%
  filter(h_a=="a",type=="SubstitutionOff")%>%
  arrange(minute)%>%
  filter(minute==min(minute))%>%
  pull(minute)

first_sub <- events%>%
  filter(type=="SubstitutionOff")%>%
  arrange(minute)%>%
  filter(minute==min(minute))%>%
  pull(minute)

xt_max <-events %>% clean_names()%>%
  filter(type=="Pass",outcome_type=="Successful") %>% 
  group_by(player_name) %>% 
  summarise(xt=sum(x_t,na.rm = T)) %>% ungroup() %>% 
  summarise(xt=max(xt)) %>% 
  pull(xt)

xt_min <-events %>% clean_names()%>%
  filter(type=="Pass",outcome_type=="Successful") %>% 
  group_by(player_name) %>% 
  summarise(xt=sum(x_t,na.rm = T)) %>% ungroup() %>% 
  summarise(xt=min(xt)) %>% 
  pull(xt)



to_int<-rescale_coordinates(from = pitch_opta,to=pitch_international)
pass_network_fn <- function(side,sub,stage="Before",label="Up to"){
  team <- events%>%
    filter(h_a==side)%>%
    distinct(team_name)%>%
    pull(team_name)
  pass_net_df <- events %>% clean_names()%>%
    filter(type=="Pass",outcome_type=="Successful",h_a==side)%>%
    mutate(pass_recipient=lead(player_name),
           x=to_int$x(x),
           y=to_int$y(y),
           end_x=to_int$x(end_x),
           end_y=to_int$y(end_y))%>%
    select(id,event_id,minute,second,type,outcome_type,team_name,x,y,end_x,end_y,passer=player_name,pass_recipient,x_t)%>%
    separate(passer, into=c("passer_first","passer_last"), sep=" ",extra="merge")%>%
    separate(pass_recipient, into=c("recipient_first","recipient_last"), sep=" ",extra="merge")%>%
    mutate(passer_abb=case_when(!is.na(passer_last)~str_c(substr(passer_first,start = 1,stop = 1),passer_last,sep ="."),
                                is.na(passer_last)~passer_first),
           passer=glue("{passer_first} {passer_last}"),
           recipient_abb=case_when(!is.na(recipient_last)~str_c(substr(recipient_first,start = 1,stop = 1),recipient_last,sep ="."),
                                   is.na(recipient_last)~recipient_first),
           pass_recipient=glue("{recipient_first} {recipient_last}"))
  
  if (stage=="Before"){
    pass_net_df <- 
      pass_net_df %>% 
      filter(
        minute<sub
    )
  }
  else{
    pass_net_df <- 
      pass_net_df %>% 
      filter(
        minute>sub
      )
  }
  
  avg_position <-
    pass_net_df %>%
    group_by(passer_abb,team_name)%>%
    summarise(x=mean(x),y=mean(y),passes=n())%>%
    na.omit() %>% 
    ungroup() %>% 
    left_join(
      events %>% clean_names()%>%
        filter(type=="Pass",outcome_type=="Successful") %>% 
        group_by(player_name,team_name) %>% 
        summarise(xt=sum(x_t,na.rm = T)) %>% ungroup() %>% 
        separate(player_name, into=c("passer_first","passer_last"), sep=" ",extra="merge")%>%
        mutate(
          passer_abb=case_when(!is.na(passer_last)~str_c(substr(passer_first,start = 1,stop = 1),passer_last,sep ="."),
                               is.na(passer_last)~passer_first)
        ) %>% select(passer_abb,team_name,xt)
    )
  
  edgelist <- pass_net_df %>% 
    select(from = passer_abb, to = recipient_abb) %>% 
    group_by(from, to) %>% 
    summarise(n = n()) %>% 
    na.omit()
  
  
  edges <- left_join(edgelist, 
                     avg_position %>% select(passer_abb, x, y),
                     by = c("from" = "passer_abb"))
  
  edges <- left_join(edges, 
                     avg_position %>% select(passer_abb,xend = x, yend = y),
                     by = c("to" = "passer_abb"))
  
  edges <- edges %>% 
    group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
    dplyr::summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
  
  avg_position <- avg_position %>% 
    mutate(passes = rescale(passes, c(3, 30),c(1,200)))
  
  # rescale node size
  edges <- edges %>% 
    filter(n >= 4) %>%
    mutate(n = rescale(n, c(1, 5),c(4,40)))
  
  

  
  
  plot <- ggplot()+
    annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
    geom_segment(data = edges, aes(x, 68-y, xend = xend, yend = 68-yend, size = n), col = "grey80",alpha=.6) +
    geom_point(data = avg_position, aes(x, 68-y, size = passes,fill=xt), shape = 21, col = "floralwhite",stroke=1.1) +
    scale_size_identity() +
    scale_fill_gradient(low="#FCD299",high = "#c64500",limits=c(xt_min,xt_max))+
    geom_text_repel(data = avg_position, aes(x,68-y, label = passer_abb), size = 4,nudge_x=-.2,color="floralwhite",fontface="bold",family = "Consolas")+
    guides(size="none") +
    theme_pitch()+
    coord_flip(ylim = c(0, 68))+
    draw_text(team,y=1,x=110,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
    draw_text(glue("{label} first substitution ({sub}')"),y=1,x=106,fontface="bold",color="grey85",size=10,family="Consolas",hjust=0,vjust=1)+
  
    theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
          legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
          panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
          plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0.5, color = "floralwhite"),
          plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))
  
  return(plot)
}

pass_network_fn("a",away_first_sub,"Before",label = "Up to")+pass_network_fn("a",away_first_sub,"After",label = "After")+
  plot_annotation(title = "Passing Network", glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                  theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 12,hjust = 0.5),
                                plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                  caption = "Minimum 4 passes b/n players<br>Circle size = number of passes<br>Color = Threats from passing based on full game (more orange is better)<br>Line thickness = number of passes between players<br>Chart by Alfred (@CallmeAlfredo)")

ggsave("Charts/LFC/pass_map_halves.png", width = 10, height = 10, dpi=300,type='cairo')

summary <- events %>% clean_names() %>%
  filter(type == "Pass",throw_in==F) %>%
  group_by(player_name, team_name) %>%
  tally()


# Custom request 








