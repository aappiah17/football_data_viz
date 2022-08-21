

summary <- events %>% clean_names() %>% 
  filter(type=='Pass',throw_in==F) %>% 
  group_by(player_name) %>% 
  summarise(value=n()) 


check <- game_passes %>% 
  filter(player_name=="Michael Keane")
# Season events
leagues <- c('Premier League','LaLiga','Bundesliga','Serie A','Ligue 1')

get_leagues_data <- function(league,year){
  get_matches_data(league,year)%>%
    mutate(league_name=league)
}

year_vec <- c("2017/2018","2018/2019","2019/2020","2020/2021","2021/2022")
season_all_events <- pmap_dfr(list(leagues,year_vec),get_leagues_data)

season_all_events <- season_all_events %>% 
  clean_names()
write_rds(season_all_events,"Data/all_events.rds")

season_all_events %>% 
  clean_names() %>% 
  group_by(year=year(start_date)) %>% 
  tally()

season_all_events %>% 
  clean_names() %>% 
  glimpse()

events <- season_all_events %>% 
  clean_names() %>% 
  group_by(type) %>% 
  tally()

write_rds(season_all_events,"Data/big_5_all.rds")

season_all_events <- readRDS("Data/big_5_all.rds")


rm(season_all_events,season_xt,season_xt_sum,pl_shot_location,afcon_events)
# season_all_events <- season_all_events %>% 
#   mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
#          across(c(playerId),.fns=~as.numeric(.)),
#     playerName=case_when(as.character(playerName)=="Luis Suárez"&as.numeric(playerId)==22221~"Luis Alberto Suárez",
#                               TRUE~playerName),
#     across(c(playerName,playerId),.fns=~as.list(.))
#          ) 
# 
# glimpse(season_all_events)


#big5_minutes <- all_player_minutes(season_all_events)

pl_all_events <- get_leagues_data('Premier League')

write_rds(pl_all_events,"Data/pl_all.rds")

pl_all_events <- readRDS("Data/pl_all.rds")



pl_minutes_sum <-
  all_player_minutes(pl_all_events) %>%
  mutate(across(c(playerName),.fns=~as.character(.)),
         across(c(playerId),.fns=~as.numeric(.)))%>%
  group_by(playerName, playerId, team_name) %>%
  summarise(minutes_played = sum(time), minutes_90 = minutes_played / 90)




pl_all_events <- add_carries_all(pl_all_events)


pl_all_events <- add_carries_xT(pl_all_events)

pl_all_events_1 <- pl_all_events %>%  
  mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
         across(c(playerId),.fns=~as.numeric(.)),
         is_goal_kick=ifelse(str_detect(as.character(qualifiers),"GoalKick"),T,F)) %>% 
  clean_names() %>% 
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
                                  TRUE~"Open Play"))


write_rds(pl_all_events_1,"Data/pl_all_carries.rds")
pl_all_events_1 <- readRDS("Data/pl_all_carries.rds")

pl_all_events_carries <- read_csv("Data/pl_all_carries.csv") %>% clean_names() %>% 
  mutate(pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
                                  TRUE~"Open Play"))

pl_all_events_carries <- readRDS("Data/pl_all_carries.rds") %>% 
  mutate(pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
                                  TRUE~"Open Play"))

max(pl_all_events_carries$start_date,na.rm = T)

glimpse(pl_all_events_carries)

pl_minutes_sum <-
  read_csv("Data/pl_minutes.csv") %>% 
  mutate(across(c(playerName),.fns=~as.character(.)),
         across(c(playerId),.fns=~as.numeric(.)))%>%
  group_by(playerName, playerId, team_name) %>%
  summarise(minutes_played = sum(time), minutes_90 = minutes_played / 90)

season_xt_sum <- pl_all_events_carries %>% filter(pass_situation=="Open Play",type %in%c("Pass","Carry"),x_t!="NaN")%>%
  mutate(distance=round(distance_function(x,y,end_x,end_y),1),
         is_carry=ifelse(type=="Carry"&distance>=5,T,F),
         x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
  ungroup()%>%
  group_by(player_name,player_id,type)%>%
  summarise(xT=sum(x_t,na.rm = T))%>%
  left_join(
    pl_all_events_carries %>% filter(pass_situation=="Open Play",type %in%c("Pass","Carry"),x_t!="NaN")%>%
      mutate(distance=round(distance_function(x,y,end_x,end_y),1),
             is_carry=ifelse(type=="Carry"&distance>=5,T,F),
             x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
      ungroup()%>%
      group_by(player_name,player_id)%>%
      summarise(total_xT=sum(x_t,na.rm=T)))%>%
  left_join(teams_player_url %>% select(-team_name)) %>% 
  left_join(pl_minutes_sum%>% clean_names(), by=c("player_name","player_id")) %>% 
  mutate(
    xt_90=xT/minutes_90,total_xt_90=total_xT/minutes_90
  ) %>% 
  filter(minutes_90>=10) %>% ungroup()

pl_cancelo <- pl_all_events_carries %>% 
  filter(player_name=="João Cancelo")

pl_cancelo %>% filter(pass_situation=="Open Play",type %in%c("Pass","Carry"),x_t!="NaN") %>% 
  group_by(type) %>% 
  summarise(xt=sum(x_t,na.rm = T))

lfc_season_xt_sum <- pl_all_events_carries %>% filter(team_name=="Liverpool",
                                                      pass_situation=="Open Play",type %in%c("Pass","Carry"),x_t!="NaN")%>%
  mutate(distance=round(distance_function(x,y,end_x,end_y),1),
         is_carry=ifelse(type=="Carry"&distance>=5,T,F),
         x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
  ungroup()%>%
  group_by(player_name,player_id,type)%>%
  summarise(xT=sum(x_t,na.rm = T))%>%
  left_join(
    pl_all_events_carries %>% filter(team_name=="Liverpool",pass_situation=="Open Play",type %in%c("Pass","Carry"),x_t!="NaN")%>%
      mutate(distance=round(distance_function(x,y,end_x,end_y),1),
             is_carry=ifelse(type=="Carry"&distance>=5,T,F),
             x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
      ungroup()%>%
      group_by(player_name,player_id)%>%
      summarise(total_xT=sum(x_t,na.rm=T)))%>%
  left_join(teams_player_url %>% select(-team_name)) %>% 
  left_join(pl_minutes_sum%>% clean_names(), by=c("player_name","player_id")) %>% 
  mutate(
    xt_90=xT/minutes_90,total_xt_90=total_xT/minutes_90
  ) %>% 
  filter(minutes_90>=8,player_name!="NaN") %>% ungroup()


# xt_all <- function(match_id) {
# season_xT <- season_all_events %>% filter (matchId==match_id) %>% 
#   mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
#          across(c(playerId),.fns=~as.numeric(.)),
#          is_goal_kick=ifelse(str_detect(as.character(qualifiers),"GoalKick"),T,F)) %>% 
#   clean_names() %>% 
#   mutate(pass_body_part=case_when(pass_right_foot==T~"Right Foot",
#                                   pass_left_foot==T~"Left Foot",
#                                   pass_head==T~"Head"),
#          pass_field_area=case_when(defensive_third==T~"Defensive Third",
#                                    mid_third==T~"Mid Third",
#                                    final_third==T~"Final Third"),
#          pass_direction=case_when(pass_back==T~"Back Pass",
#                                   pass_forward==T~"Forward Pass"),
#          through_ball=ifelse(pass_through_ball_accurate==T|pass_through_ball_inaccurate==T, T,F),
#          cross=ifelse(pass_cross_accurate==T|pass_cross_inaccurate==T,TRUE,FALSE),
#          pass_cross=ifelse(cross==T,"Cross","Pass"),
#          long_ball=ifelse(pass_long_ball_accurate==T|pass_long_ball_inaccurate==T,T,F),
#          pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
#                                   TRUE~"Open Play")
#   ) %>% 
#   select(match_id,x,y,finalX=end_x,finalY=end_y,player_name,player_id,team_name,type,outcome_type,pass_situation,league_name)%>%
#   calculate_threat()%>%
#   mutate(x_t=xTEnd-xTStart)
# }
# 
# season_ids <-season_all_events %>%
#   select(matchId)%>%
#   distinct(matchId)%>%
#   pull(matchId)
# 
# season_xt <- map_dfr(season_ids,xt_all)
# 
# 
# season_xt_sum <- season_xt %>% 
#   filter(type=="Pass",outcome_type=="Successful")%>%
#   group_by(player_name,player_id,pass_situation)%>%
#   summarise(x_t=sum(x_t,na.rm=T))%>%
#   ungroup()%>%
#   group_by(player_name,player_id)%>%
#   mutate(total_xt=sum(x_t,na.rm=T))%>% ungroup() %>% 
#   left_join(season_xt%>%
#               distinct(player_id,.keep_all=T) %>% 
#               select(player_id,player_name,league_name))%>%
#   left_join(pl_minutes_sum%>% clean_names()) %>%
#   mutate(league_logo=case_when(league_name=="LaLiga"~"https://upload.wikimedia.org/wikipedia/commons/thumb/b/bb/LaLiga_Santander_logo_%28stacked%29.svg/281px-LaLiga_Santander_logo_%28stacked%29.svg.png",
#                           league_name=="Premier League"~"https://cdn.freelogovectors.net/wp-content/uploads/2020/08/epl-premierleague-logo.png",
#                           league_name=="Bundesliga"~"https://upload.wikimedia.org/wikipedia/en/thumb/d/df/Bundesliga_logo_%282017%29.svg/240px-Bundesliga_logo_%282017%29.svg.png",
#                           league_name=="Ligue 1"~"https://upload.wikimedia.org/wikipedia/commons/thumb/c/cd/Ligue_1_Uber_Eats_logo.svg/163px-Ligue_1_Uber_Eats_logo.svg.png",
#                           league_name=="Serie A"~"https://upload.wikimedia.org/wikipedia/commons/thumb/5/5e/Serie_a_2021.svg/186px-Serie_a_2021.svg.png"),
#          xt_90=x_t/minutes_90,total_xt_90=total_xt/minutes_90)%>%
#   filter(minutes_90>=15)
min_xt <- season_xt_sum %>%
  arrange(-total_xt_90)%>%
  dplyr::slice(1:50) %>% 
  summarise(xt_90=min(xt_90)) %>% 
  pull(xt_90)

season_xt_sum %>%
  #mutate(logo_dark=glue("logos/{league_name}.png"))%>%
  arrange(-total_xt_90)%>%
  mutate(type=factor(type,levels=c("Pass","Carry"))) %>% 
  dplyr::slice(1:50) %>% 
  #mutate(logo_dark=ifelse(h_a=="h","home.png","away.png"))%>%
  ggplot(aes(y=reorder(player_name,total_xt_90),x=xt_90,fill=fct_rev(type)))+
  geom_col_pattern(pattern_angle=45, pattern_density = 0.05,pattern_color="white",
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,alpha=.6,
                   pattern="circle")+
  scale_fill_manual(values = c("Carry"="#DCA62C","Pass"="#2398B4"))+
  geom_image(aes(x=min_xt-.025,y=player_name,image=image_url),size=0.03)+
  #expand_limits(x=min_xt)+
  AAtheme_patch+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=14, color="floralwhite",family="Consolas",face = "bold"),
        axis.text.x = element_text(size=14, color="grey75",family="Consolas",face = "bold"),
        panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
        panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "grey80",family = "Consolas",size = 12,hjust = 0.5),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  #geom_text(aes(y=player_name,x=total_xT,label=ifelse(total_xT>0.1,round(total_xT,3),"")),fontface="bold",hjust=-0.1,color = "floralwhite",family = "Consolas")+
  labs(title = "EPL's Most Dangerous Ball Progressors through <span style='color:#2398B4;'>Open Play Passes</span> and <span style='color:#DCA62C;'>Carries</span>",
       subtitle = "Season: 2021/22 up to Gameweek 32 | Only players with 900+ minutes included", x="Expected threat per 90",
       
       caption = "Expected threat (xT) values from Karun Singh's model <br>Chart: @CallmeAlfredo")
ggsave("Charts/LFC/xt_chart_pl_21.png", width = 14.5, height = 10.5, dpi=300,type='cairo')


lfc_season_xt_sum %>%
  #mutate(logo_dark=glue("logos/{league_name}.png"))%>%
  arrange(-total_xt_90)%>%
  mutate(type=factor(type,levels=c("Pass","Carry"))) %>% 
  #mutate(logo_dark=ifelse(h_a=="h","home.png","away.png"))%>%
  ggplot(aes(y=reorder(player_name,total_xt_90),x=xt_90,fill=fct_rev(type)))+
  geom_col_pattern(pattern_angle=45, pattern_density = 0.05,pattern_color="white",
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,alpha=.65,
                   pattern="circle")+
  scale_fill_manual(values = c("Carry"="#DCA62C","Pass"="#C8102E"))+
  geom_image(aes(x=min_xt-.025,y=player_name,image=image_url),size=0.04)+
  #expand_limits(x=min_xt)+
  AAtheme_patch+
  theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
        legend.text = element_blank(),legend.key = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=14, color="floralwhite",family="Consolas",face = "bold"),
        axis.text.x = element_text(size=14, color="grey75",family="Consolas",face = "bold"),
        panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
        panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
        panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
        plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
        plot.subtitle = element_markdown(color = "grey80",family = "Consolas",size = 12,hjust = 0.5),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10),
        strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  #geom_text(aes(y=player_name,x=total_xT,label=ifelse(total_xT>0.1,round(total_xT,3),"")),fontface="bold",hjust=-0.1,color = "floralwhite",family = "Consolas")+
  labs(title = "LFC's Most Dangerous Ball Progressors through <span style='color:#C8102E;'>Open Play Passes</span> and <span style='color:#DCA62C;'>Carries</span>",
       subtitle = "Season: 2021/22 up to Gameweek 32 | Only players with 720+ minutes included", x="Expected threat per 90",
       
       caption = "Expected threat (xT) values from Karun Singh's model <br>Chart: @CallmeAlfredo")
ggsave("Charts/LFC/xt_chart_pl_21_lfc.png", width = 14.5, height = 10, dpi=300,type='cairo')

# 
# check <- season_all_events %>% filter(league_name=="LaLiga") %>% 
#   mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
#          across(c(playerId),.fns=~as.numeric(.)),
#          is_goal_kick=ifelse(str_detect(as.character(qualifiers),"GoalKick"),T,F)) %>% 
#   filter(playerName=="NaN"&playerId!="NaN")%>%
#   select(playerId,playerName)
# 
# check <- season_all_events %>% filter(league_name=="LaLiga") %>% 
#  # filter(playerName!="NaN"&playerId!="NaN")%>%
#   group_by(matchId)%>%
#   summarise(ids=n_distinct(playerId),names=n_distinct(playerName))%>%
#   mutate(val=ifelse(ids==names,T,F))
# 
# check <- season_all_events %>% filter(league_name=="LaLiga") %>% 
#   mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
#          across(c(playerId),.fns=~as.numeric(.)),
#          is_goal_kick=ifelse(str_detect(as.character(qualifiers),"GoalKick"),T,F)) %>% 
#  group_by(playerName,playerId,team_name) %>% 
#   summarise(ids=n_distinct(playerId),names=n_distinct(playerName))%>%
#   mutate(val=ifelse(ids==names,T,F))


rm(pl_all_events_carries)