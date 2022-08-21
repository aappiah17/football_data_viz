#Get the data from WS using python
use_condaenv("C:/Users/alfre/Anaconda3/envs/AA_DS",required = TRUE)

#py_config()
#source_python("C:/Users/alfre/Documents/Learning/Learn/Python/main.py")
source_python("WS scraper/Scrape_whoscored.py")

source("Helpers.R")

# afcon_logos <- read_excel("C:/Users/alfre/Documents/Learning/Learn/euro_cup_2021/logos.xlsx")
#  image_read("https://upload.wikimedia.org/wikipedia/en/f/fd/2021_Africa_Cup_of_Nations_logo.png") %>%
#    image_quantize(colorspace = "gray")%>%
#    image_write("afcon_dark.png")
#  
# add_carries(afcon_events)
  
team_logos <- read_csv("Data/team_logos.csv")%>%
  mutate(team_name=case_when(team_name=="Manchester City"~"Man City",
                             team_name=="Manchester Utd"~"Man Utd",
                             team_name=="Leicester City"~"Leicester",
                             team_name=="Newcastle Utd"~"Newcastle",
                             team_name=="Leeds United"~"Leeds",
                             TRUE~team_name))%>%
  bind_rows(tribble(~team_name,~color,~logo,"Cardiff","","https://upload.wikimedia.org/wikipedia/en/thumb/3/3c/Cardiff_City_crest.svg/230px-Cardiff_City_crest.svg.png",
                    "Inter","#010E80","https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/FC_Internazionale_Milano_2021.svg/240px-FC_Internazionale_Milano_2021.svg.png",
                    "PSG","#004170","https://upload.wikimedia.org/wikipedia/en/thumb/a/a7/Paris_Saint-Germain_F.C..svg/240px-Paris_Saint-Germain_F.C..svg.png",
                    "Real Madrid","","https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Real_Madrid_CF.svg/179px-Real_Madrid_CF.svg.png",
                    "Atletico","","https://upload.wikimedia.org/wikipedia/en/thumb/f/f4/Atletico_Madrid_2017_logo.svg/180px-Atletico_Madrid_2017_logo.svg.png",
                    "Nottingham Forest","","https://upload.wikimedia.org/wikipedia/en/thumb/e/e5/Nottingham_Forest_F.C._logo.svg/113px-Nottingham_Forest_F.C._logo.svg.png",
                    "Benfica","","https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/SL_Benfica_logo.svg/242px-SL_Benfica_logo.svg.png",
                    "Villareal","","https://upload.wikimedia.org/wikipedia/en/thumb/b/b9/Villarreal_CF_logo-en.svg/283px-Villarreal_CF_logo-en.svg.png"))


# Read my xg model 
aa_xg_model <- readRDS("Models/aa_xg_model.rds")


lfc_viz <- function(events){
  
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
    left_join(events%>% clean_names() %>% 
                select(id,goal_own,period))%>%
    mutate(goal_own=ifelse(is.na(goal_own),F,goal_own),
           team_name=case_when(goal_own==T&team_name==home_team~away_team,
                               goal_own==T&team_name==away_team~home_team,
                               TRUE~team_name),
           h_a=case_when(goal_own==T&h_a=="h"~"a",
                         goal_own==T&h_a=="a"~"h",
                         TRUE~h_a),
           xG=ifelse(goal_own==T,0,xG),xG=as.numeric(xG))
  
  home_goals <- game_xg %>% filter(h_a == 'h') %>% 
    summarize(goals = sum(is_goal == 'Goal')) %>%
    pull(goals)
  away_goals <- game_xg %>% filter(h_a == 'a') %>% 
    summarize(goals = sum(is_goal == 'Goal')) %>%
    pull(goals)
  
  xg_map_df <- game_xg %>% clean_names()%>%
    left_join(team_logos, by="team_name")%>%
    mutate(x_adj=ifelse(h_a=="h",100-x,x),
           y_adj=ifelse(h_a=="h",100-y,y))
  

  
  xg_map_df %>%
    filter(h_a=="h")%>%
    distinct(logo)%>%
    pull(logo)%>%
    image_read()%>%
    image_quantize(colorspace = "gray")%>%
    image_write("home.png")
  
  xg_map_df %>%
    filter(h_a=="a")%>%
    distinct(logo)%>%
    pull(logo)%>%
    image_read()%>%
    image_quantize(colorspace = "gray")%>%
    image_write("away.png")
  
  xg_map_df%>%
    mutate(logo_dark=ifelse(h_a=="h","home.png","away.png"))%>% #filter(x_g>0)%>%
    ggplot()+
    annotate_pitch(colour = "grey65",dimensions = pitch_opta,fill="grey25",limits = F)+
    geom_point(
      aes(x=x_adj, y = y_adj,size = x_g,fill=is_goal,color=is_goal),
      shape = 21
    ) +
   
    theme_pitch() +
    scale_size(range=c(0,4))+
    guides(fill='none',color='none',size=guide_legend(override.aes=list(color="floralwhite")))+
    scale_color_manual(values = c("Goal"="#2398B4","No Goal"="#C0212E"))+
    scale_fill_manual(values = c("Goal"="#2398B4","No Goal"="#C0212E"))+
    annotate(geom = "text", x=20,y=95, color="floralwhite",size=6,label=xg_map_df %>% 
               filter(h_a=="h")%>% distinct(team_name)%>%
               pull(team_name),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=70,y=95, color="floralwhite",size=6,label=xg_map_df %>% 
               filter(h_a=="a")%>% distinct(team_name)%>%
               pull(team_name),fontface="bold",family='Consolas')+
    geom_image(x=3,y=95,aes(image="home.png"),asp=1.618,size=.03)+
    geom_image(x=53,y=95,aes(image="away.png"),asp=1.618,size=.03)+
    annotate(geom = "text", x=20+(nchar(home_team)+3),y=95, color="floralwhite",label=glue("({xg_map_df %>% filter(h_a=='h')%>%
             summarize(goals=sum(is_goal=='Goal'))%>%
             pull(goals)})"),fontface="bold",family='Consolas',size=6)+
    annotate(geom = "text", x=70+(nchar(away_team)+3),y=95, color="floralwhite",label=glue("({xg_map_df %>% filter(h_a=='a')%>%
             summarize(goals=sum(is_goal=='Goal'))%>%
             pull(goals)})"),fontface="bold",family='Consolas',size=6)+
    annotate(geom = "rect", xmin=5,xmax=10,ymin = 85,ymax=90, color="#C0212E", fill="#C0212E")+
    annotate(geom = "text", x=7.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="h",goal_own==F)%>%
               summarise(shots=n())%>%
               pull(shots),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=7.5,y=83.5, color="floralwhite",label='Shots',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=15,xmax=20,ymin = 85,ymax=90, color="#2398B4", fill="#2398B4")+
    annotate(geom = "text", x=17.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="h")%>%
               summarise(goals=sum(is_goal=='Goal'))%>%
               pull(goals),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=17.5,y=83.5, color="floralwhite",label='Goals',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=25,xmax=30,ymin = 85,ymax=90, color="#F59752", fill="#F59752")+
    annotate(geom = "text", x=27.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="h")%>%
               summarise(xG=round(sum(x_g),2))%>%
               pull(xG),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=27.5,y=83.5, color="floralwhite",label='xG',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=35,xmax=40,ymin = 85,ymax=90, color="#FEC753", fill="#FEC753")+
    annotate(geom = "text", x=37.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="h",goal_own==F)%>%
               summarise(xG=sum(x_g),shots=n(),xg_shot=round(xG/shots,2))%>%
               pull(xg_shot),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=37.5,y=83.5, color="floralwhite",label='xG/shot',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=55,xmax=60,ymin = 85,ymax=90, color="#C0212E", fill="#C0212E")+
    annotate(geom = "text", x=57.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="a",goal_own==F)%>%
               summarise(shots=n())%>%
               pull(shots),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=57.5,y=83.5, color="floralwhite",label='Shots',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=65,xmax=70,ymin = 85,ymax=90, color="#2398B4", fill="#2398B4")+
    annotate(geom = "text", x=67.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="a")%>%
               summarise(goals=sum(is_goal=='Goal'))%>%
               pull(goals),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=67.5,y=83.5, color="floralwhite",label='Goals',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=75,xmax=80,ymin = 85,ymax=90, color="#F59752", fill="#F59752")+
    annotate(geom = "text", x=77.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="a")%>%
               summarise(xG=round(sum(x_g),2))%>%
               pull(xG),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=77.5,y=83.5, color="floralwhite",label='xG',family='Consolas',fontface="bold")+
    annotate(geom = "rect", xmin=85,xmax=90,ymin = 85,ymax=90, color="#FEC753", fill="#FEC753")+
    annotate(geom = "text", x=87.5,y=87.5, color="floralwhite",label=xg_map_df %>% 
               filter(h_a=="a",goal_own==F)%>%
               summarise(xG=sum(x_g),shots=n(),xg_shot=round(xG/shots,2))%>%
               pull(xg_shot),fontface="bold",family='Consolas')+
    annotate(geom = "text", x=87.5,y=83.5, color="floralwhite",label='xG/shot',family='Consolas',fontface="bold")+
    annotate("segment", x = 67, xend = 80, y = 6, yend =6,
             colour = "floralwhite",arrow = arrow(length = unit(.2,"cm")))+
    annotate(geom = "text", x=63,y=6, color="floralwhite",label='Low xG',family='Consolas',fontface="bold")+
    annotate(geom = "text", x=85,y=6, color="floralwhite",label='High xG',family='Consolas',fontface="bold")+
    annotate(geom = "text", x=35,y=6, color="grey65",label='Data: Opta\nChart by @CallmeAlfredo',family='Consolas',fontface="bold")+
    theme(legend.position=c(.7,0.13), legend.direction = "horizontal",legend.title = element_blank(),legend.background = element_blank(),
          legend.text = element_blank(),legend.key = element_blank(),aspect.ratio=1/1.618,
          panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
          plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0.5, color = "floralwhite"),
          plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.7),
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))
  ggsave("Charts/LFC/xg_map.png", width = 8*1.618, height = 6, dpi=300,type='cairo') 
  
  xg_map <- image_read("Charts/LFC/xg_map.png")
  
  xg_map %>% 
    image_trim() %>% 
    image_write("Charts/LFC/xg_map_cropped.png")
  
  #add_logo_map("Charts/AFCON/xg_map.png",afcon_logo,"top right",logo_scale = 20) %>%
  #image_write("Charts/AFCON/xg_map.png")
  
  #xg roll sum 
  
  #xg roll sum 
  last_minute <- max(events$minute,na.rm = T)
  labels <- game_xg %>%  
    mutate(player_label = dplyr::case_when(
      is_goal == "Goal" & situation != "Penalty" ~ paste0(player_name, ": ", round(xG, digits = 2), " xG"),
      is_goal == "Goal" & situation == "Penalty" ~ paste0(player_name, " (Penalty): ", round(xG, digits = 2), " xG"),
      goal_own==T ~ paste0(player_name, " (Own Goal): ", round(xG, digits = 2), " xG"),
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
  
  data1 <- dplyr::add_row(.data = data1 %>% mutate(goal_own=as.logical(goal_own)), .before = 1, 
                          id = 0, minute = 0, is_goal = "0", x = 0, y = 0, xG = 0, player_name = "0", h_a = "0",
                          situation = "0", shot_body_type = "0", match_id = 0, team_name= "0",last_action = "0",period="0",
                          angle = 0, distance = 0, shot_counter=0,is_big_chance=0,goal_own=F,xGsum = 0) %>% 
    dplyr::add_row(
      id = 0, minute = last_minute, is_goal = "0", x = 0, y = 0, xG = 0, player_name = "0", h_a = "0",
      situation = "0", shot_body_type = "0", match_id = 0, team_name= "0",last_action = "0",period="0",
      angle = 0, distance = 0,shot_counter=0,is_big_chance=0, goal_own=F,xGsum = home_xg) 
  
  
  data2 <- dplyr::add_row(.data = data2 %>% mutate(goal_own=as.logical(goal_own)), .before = 1, 
                          id = 0, minute = 0, is_goal = "0", x = 0, y = 0, xG = 0, player_name = "0", h_a = "0",
                          situation = "0", shot_body_type = "0", match_id = 0, team_name= "0",last_action = "0",period="0",
                          angle = 0, distance=0,shot_counter=0,is_big_chance=0, goal_own=F,xGsum = 0) %>% 
    dplyr::add_row(
      id = 0, minute = last_minute, is_goal = "0", x = 0, y = 0, xG = 0, player_name = "0", h_a = "0",
      situation = "0", shot_body_type = "0", match_id = 0, team_name= "0",last_action = "0",period="0",
      angle = 0, distance = 0,shot_counter=0,is_big_chance=0,goal_own=F,xGsum = away_xg) 
  
  dat1 <- data1 %>%
    dplyr::filter(is_goal=='Goal')
  d1 <- data1 %>%
    dplyr::filter(goal_own==T)
  dat1 <- bind_rows(dat1, d1)
  
  dat2 <- data2 %>%
    dplyr::filter(is_goal=='Goal')
  d2 <- data2 %>%
    dplyr::filter(goal_own==T)
  dat2 <- bind_rows(dat2, d2)
  
  ggplot() +
    geom_step(data = data1, aes(x = minute, y = xGsum), color =home_color, size = 2.7) +
    geom_step(data = data2, aes(x = minute, y = xGsum), color = away_color, size = 2.7) +
    geom_point(data = dat1, aes(x = minute, y = xGsum), color = home_color, fill = "floralwhite", shape = 21, stroke = 2, size = 5) +
    geom_point(data = dat2, aes(x = minute, y = xGsum), color = away_color, fill = "floralwhite", shape = 21, stroke = 2, size = 5) +
    geom_text_repel(data = labels %>% filter(is_goal=="Goal"), 
                    aes(x = minute, y = xGsum, label = player_label,color=team_color), nudge_x =-8, nudge_y = 0.15, family = "Consolas",fontface="bold",
                    alpha=0.8,size=4.5,
                    show.legend = FALSE) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90)) +
    scale_color_identity()+
    geom_vline(xintercept = 45, linetype = "dashed", color = "floralwhite", size = 1)+
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
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
  
    
    labs(title = glue("<span style='color:#2398B4;'>{home_team}:{game_xg%>%filter(h_a=='h')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span> vs
                    <span style='color:#DCA62C;'>{away_team}:{game_xg%>%filter(h_a=='a')%>%summarize(goals=sum(is_goal=='Goal'))%>%pull(goals)}</span>"),
         subtitle = "",caption = "Data: Opta<br>Chart by Alfred (@CallmeAlfredo)",
         x = "Minute",
         y = "Cumulative xG")
  ggsave("Charts/LFC/xg_timeline.png", width = 9.5, height = 6, dpi=300,type='cairo')
  

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
    scale_y_continuous(limits=c(-.13,.13))+
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
  
  

  
  #xT chart 
  game_xt <- use_xt(events)
  
  # check <- game_xt %>% filter(player_name=="NaN")
  game_xt_sum <- game_xt %>% filter(pass_situation=="Open Play")%>%
    mutate(distance=round(distance_function(x,y,end_x,end_y),1),
           is_carry=ifelse(type=="Carry"&distance>=5,T,F),
           x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
    ungroup()%>%
    group_by(player_name,h_a,team_name,type)%>%
    summarise(xT=sum(x_t))%>%
    inner_join(
      game_xt %>% filter(pass_situation=="Open Play")%>%
        mutate(distance=round(distance_function(x,y,end_x,end_y),1),
               is_carry=ifelse(type=="Carry"&distance>=5,T,F),
               x_t=ifelse(type=="Carry"&is_carry==F,0,x_t))%>%
        ungroup()%>%
        group_by(player_name)%>%
        summarise(total_xT=sum(x_t))%>%
        arrange(-total_xT)%>%
        dplyr::slice(1:20))%>%
    left_join(team_logos,by="team_name")
  
  min_xt <- min(game_xt_sum$xT)
  
  game_xt_sum %>%
    filter(h_a=="h")%>%
    distinct(logo)%>%
    pull(logo)%>%
    image_read()%>%
    image_quantize(colorspace = "gray")%>%
    image_write("home.png")
  
  game_xt_sum %>%
    filter(h_a=="a")%>%
    distinct(logo)%>%
    pull(logo)%>%
    image_read()%>%
    image_quantize(colorspace = "gray")%>%
    image_write("away.png")
  
  
  game_xt_sum %>%
    mutate(logo_dark=ifelse(h_a=="h","home.png","away.png"))%>%
    ggplot(aes(y=reorder(player_name,total_xT),x=xT,fill=type))+
    geom_col_pattern(pattern_angle=45, pattern_density = 0.05,pattern_color="white",
                     pattern_spacing = 0.025,
                     pattern_key_scale_factor = 0.6,alpha=.8,
                     pattern="circle")+
    scale_fill_manual(values = c("Carry"="#DCA62C","Pass"="#2398B4"))+
    geom_image(aes(x=min_xt-.01,y=player_name,image=logo_dark),size=0.035)+
    expand_limits(x=min_xt)+
    AAtheme_patch+
    theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
          legend.text = element_blank(),legend.key = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major.x = element_line(colour="grey", linetype="dotted",size = .1),
          panel.grid.major.y = element_line(colour="grey", linetype="dotted",size=.1),
          panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
          plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, color = "floralwhite"),
          plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0.5),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10),
          strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
    #geom_text(aes(y=player_name,x=total_xT,label=ifelse(total_xT>0.1,round(total_xT,3),"")),fontface="bold",hjust=-0.1,color = "floralwhite",family = "Consolas")+
    labs(title = "Threat Creation from <span style='color:#2398B4;'>Open Play Passes</span> and <span style='color:#DCA62C;'>Carries</span>",
         subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
         caption = "xT values from Karun Singh's model <br>Data: Opta | Chart: @CallmeAlfredo")
  ggsave("Charts/LFC/xt_chart.png", width = 10.5, height = 8.5, dpi=300,type='cairo')
  
  #add_logo("Charts/AFCON/xt_chart.png",afcon_logo,"bottom left",logo_scale = 15) %>%
  #image_write("Charts/AFCON/xt_chart.png")
  
  player_prog_viz <-game_xt_sum %>% ungroup()%>%
    filter(total_xT==max(total_xT))%>%
    distinct(player_name)%>%
    pull(player_name)
  
  # Progressive passing 
  prog_pass_map <- game_xt %>% filter(pass_situation=="Open Play")%>%
    filter(player_name==player_prog_viz)%>%
    mutate(x_adj=to_int$x(x),
           y_adj=to_int$y(y),
           end_x_adj=to_int$x(end_x),
           end_y_adj=to_int$y(end_y))
  

  pass_map <- prog_pass_map %>% filter(outcome_type=="Successful",type=="Pass")%>%
    ggplot()+
    annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
    direction_label(colour = "grey75",)+
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
    draw_text("Passes",y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
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
  
  pass_map+carries_map+plot_annotation(title = glue("{player_prog_viz}'s Ball Progression <br>through Passes and Carries"),
                                       subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                       theme = theme(plot.title=element_markdown(face="bold", size=22,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                     plot.subtitle=element_markdown(face="bold", size=12,family="Consolas",hjust=0.5, color = "grey65"),
                                                     plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                       caption = "Data: Opta <br>A progressive carry is any carry that moves the ball at least five meters towards the centre of the opponent's goal<br>A progressive pass is any pass that moves the ball at least 25% closer to the centre of the opponent's goal<br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/player_pass_carries.png", width = 10, height = 10, dpi=300,type='cairo')
  
  # add_logo("Charts/AFCON/player_pass_carries.png",afcon_logo,"top right") %>%
  #image_write("Charts/AFCON/player_pass_carries.png")
  
  
  # Attacking third entry passes 
  pass_final_third <- game_passes %>%
    mutate(x_new=x*1.05,
           y_new=y*0.68,
           end_x_new=end_x*1.05,
           end_y_new=end_y*.68,
           x_adj=to_int$x(x),
           y_adj=to_int$y(y),
           end_x_adj=to_int$x(end_x),
           end_y_adj=to_int$y(end_y))%>%
    mutate(pass_f3rd=case_when(x_new<70&end_x_new>=70~"Final third entry",
                               TRUE~"Non final third entry"))
  
 filed_tilt_df<- game_passes %>%
    mutate(x_new=x*1.05,
           y_new=y*0.68)%>%
    mutate(pass_f3rd=case_when(x_new>70~"Final third pass",
                               TRUE~"Non final third pass"))
 

  
  final_third_passes_plot <- function(side){
    pass_final_third %>% filter(h_a==side,pass_f3rd=="Final third entry")%>% 
      ggplot()+
      annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
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
      draw_text(glue(pass_final_third%>%filter(h_a==side)%>%
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey75",size=20,family="Consolas",hjust=0,vjust=1)+
      annotate(geom = "rect", xmin=40,xmax=45,ymin = 5,ymax=10, color="#DCA62C", fill="#DCA62C")+
      annotate(geom = "text", x=42.5,y=7.5, color="floralwhite",label=pass_final_third %>% 
                 filter(h_a==side,pass_f3rd=="Final third entry")%>%
                 summarise(attempts=n())%>%
                 pull(attempts),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=38.5,y=7.5, color="floralwhite",label='Attempts',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#2398B4", fill="#2398B4")+
      annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=pass_final_third %>% 
                 filter(h_a==side,pass_f3rd=="Final third entry")%>%
                 summarise(comps=sum(outcome_type=="Successful"))%>%
                 pull(comps),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=28.5,y=7.5, color="floralwhite",label='Successful',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=20,xmax=24,ymin = 4,ymax=11, color="#8A4BDE", fill="#8A4BDE")+
      annotate(geom = "text", x=22.5,y=7.5, color="floralwhite",label=pass_final_third %>% 
                 filter(h_a==side,pass_f3rd=="Final third entry")%>%
                 summarise(comps=sum(outcome_type=="Successful"),attempts=n(),rate=glue("{round(comps*100/attempts,1)}%"))%>%
                 pull(rate),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=18.5,y=7.5, color="floralwhite",label='Success %',family='Consolas',fontface="bold")
    # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
    # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
    # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
  }
  
  pass_f3rd_table <- function(side){
    pass_final_third %>% filter(pass_f3rd=="Final third entry",h_a==side)%>%
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
  
  ((final_third_passes_plot("h")/pass_f3rd_table("h")+plot_layout(heights = c(1,.2)))|(final_third_passes_plot("a")/pass_f3rd_table("a")+plot_layout(heights = c(1,.2))))+plot_annotation(title = "Passes into the Final Third",
                                                                                                                                                                        subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                                                                                                                                        theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                                                                                                                      plot.subtitle=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "grey65"),
                                                                                                                                                                                      plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                                                                                                                        caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")
  
  
  ggsave("Charts/LFC/f3rd_pass_map.png", width = 10, height = 10, dpi=300,type='cairo')
  
  game_switches <- game_passes %>%
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
    game_switches %>% filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>% 
      ggplot()+
      annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
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
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
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
    game_switches %>% filter(h_a==side,is_switch==T,pass_situation=="Open Play")%>%
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
  
    
  
  
  touches_box_table <- function(side){
    touch_box %>% filter(touch_box=="Box touch",h_a==side)%>%
      group_by(player_name)%>%
      summarise(attempts=n())%>%
      arrange(-attempts) %>%
      dplyr::slice(1:5) %>%
      gt()%>%
      tab_header(title = md("**Leaders**"))%>%
      # tab_header(title = html(glue("<p><img src='home.png' alt='pl logo' style='float:right;height:100px;'></p>
      #                             <p> <span style='font-size:30px;font-weight:bold'>Leaders</span></p>")))%>%
      
      data_color(columns = c(attempts), colors = scales::col_numeric(palette = c("grey25","#DCA62C"),
                                                                     domain = NULL))%>%
      
      cols_label(
        player_name = md("**Player**"),
        attempts = md("**Touches**"))%>%
      cols_align(
        align = "center",
        columns = c(attempts)
      )%>%
      # tab_source_note(md(glue("**Minimum 100 pass attempts. xT values from Karun Singh's xT model<br> Table: @CallmeAlfredo**")))%>%
      gt_theme_dark() %>%
      tab_options(heading.title.font.size = 15)%>%
      as_ggplot() 
  }
  
  
  box_touches_plot <- function(side){
    touch_box %>% filter(h_a==side,touch_box=="Box touch")%>% 
      ggplot()+
      annotate_pitch(colour = "grey65",dimensions = pitch_international,fill="grey25",limits = F)+
      geom_point(aes(x=x_adj,y=68-y_adj,fill=outcome_type), shape = 21, col = "floralwhite",stroke=.5,size=3,fill="#DCA62C") +
      theme_pitch()+
      # scale_fill_manual(values = c("Successful"="#2398B4","Unsuccessful"="#DCA62C"))+
      # scale_color_manual(values = c("Successful"="#2398B4","Unsuccessful"="#DCA62C"))+
      coord_flip(ylim = c(0, 68))+
      theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
            legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
            panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
            plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
            plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0,vjust=-2),
            strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
            strip.text = element_text(face="bold", size=10,color = "grey65",family = "Consolas"))+
      draw_text(glue(touch_box%>%filter(h_a==side)%>%
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)
    # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
    # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
    # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
  }
  
  ((box_touches_plot("h")/touches_box_table("h")+plot_layout(heights = c(1,.2)))|(box_touches_plot("a")/touches_box_table("a")+plot_layout(heights = c(1,.2))))+plot_annotation(title = "Touches in the Opponent's Penalty Box",
                                                                                                                                                                                subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                                                                                                                                                theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                                                                                                                              plot.subtitle=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "grey65"),
                                                                                                                                                                                              plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                                                                                                                                caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/touch_box.png", width = 10, height = 10, dpi=300,type='cairo')
  
  pass_box <- game_passes %>%
    mutate(x_new=x*1.05,
           y_new=y*0.68,
           end_x_new=end_x*1.05,
           end_y_new=end_y*.68)%>%
    mutate(pass_box=case_when(x_new>=88.5&abs(34-y_new)<20.16~"Non box entry",
                              end_x_new>=88.5&abs(34-end_y_new)<20.16~"Box entry",
                              TRUE~"Non box entry"),
           x_adj=to_int$x(x),
           y_adj=to_int$y(y),
           end_x_adj=to_int$x(end_x),
           end_y_adj=to_int$y(end_y))
  
  
  pass_box_table <- function(side){
    pass_box %>% filter(pass_box=="Box entry",h_a==side)%>%
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
  
  
  box_passes_plot <- function(side){
    pass_box %>% filter(h_a==side,pass_box=="Box entry")%>% 
      ggplot()+
      annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
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
      draw_text(glue(pass_box%>%filter(h_a==side)%>%
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
      annotate(geom = "rect", xmin=40,xmax=45,ymin = 5,ymax=10, color="#DCA62C", fill="#DCA62C")+
      annotate(geom = "text", x=42.5,y=7.5, color="floralwhite",label=pass_box %>% 
                 filter(h_a==side,pass_box=="Box entry")%>%
                 summarise(attempts=n())%>%
                 pull(attempts),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=38.5,y=7.5, color="floralwhite",label='Attempts',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#2398B4", fill="#2398B4")+
      annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=pass_box %>% 
                 filter(h_a==side,pass_box=="Box entry")%>%
                 summarise(comps=sum(outcome_type=="Successful"))%>%
                 pull(comps),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=28.5,y=7.5, color="floralwhite",label='Successful',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=20,xmax=24,ymin = 4,ymax=11, color="#8A4BDE", fill="#8A4BDE")+
      annotate(geom = "text", x=22.5,y=7.5, color="floralwhite",label=pass_box %>% 
                 filter(h_a==side,pass_box=="Box entry")%>%
                 summarise(comps=sum(outcome_type=="Successful"),attempts=n(),rate=glue("{round(comps*100/attempts,1)}%"))%>%
                 pull(rate),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=18.5,y=7.5, color="floralwhite",label='Success %',family='Consolas',fontface="bold")
    # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
    # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
    # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
  }
  
  ((box_passes_plot("h")/pass_box_table("h")+plot_layout(heights = c(1,.2)))|(box_passes_plot("a")/pass_box_table("a")+plot_layout(heights = c(1,.2))))+plot_annotation(title = "Passes into the Penalty Box",
                                                                                                                                                                        subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                                                                                                                                        theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                                                                                                                      plot.subtitle=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "grey65"),
                                                                                                                                                                                      plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                                                                                                                        caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/pass_box.png", width = 10, height = 10, dpi=300,type='cairo')
  
  game_crosses <- events %>% clean_names() %>% 
    filter(is_cross==T) %>% 
    mutate(pass_situation=case_when((pass_corner==T|pass_freekick==T|throw_in==T|is_goal_kick==T)~"Set Piece",
                                    TRUE~"Open Play"),
           x_new=x*1.05,
           y_new=y*0.68,
           end_x_new=end_x*1.05,
           end_y_new=end_y*.68,
           x_adj=to_int$x(x),
           y_adj=to_int$y(y),
           end_x_adj=to_int$x(end_x),
           end_y_adj=to_int$y(end_y))%>%
    filter(pass_situation=="Open Play")
  

  cross_plot <- function(side){
    game_crosses %>% filter(h_a==side)%>% 
      ggplot()+
      annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
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
      draw_text(glue(game_crosses%>%filter(h_a==side)%>%
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
      annotate(geom = "rect", xmin=40,xmax=45,ymin = 5,ymax=10, color="#DCA62C", fill="#DCA62C")+
      annotate(geom = "text", x=42.5,y=7.5, color="floralwhite",label=game_crosses %>% 
                 filter(h_a==side)%>%
                 summarise(attempts=n())%>%
                 pull(attempts),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=38.5,y=7.5, color="floralwhite",label='Attempts',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#2398B4", fill="#2398B4")+
      annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=game_crosses %>% 
                 filter(h_a==side)%>%
                 summarise(comps=sum(outcome_type=="Successful"))%>%
                 pull(comps),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=28.5,y=7.5, color="floralwhite",label='Successful',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=20,xmax=24,ymin = 4,ymax=11, color="#8A4BDE", fill="#8A4BDE")+
      annotate(geom = "text", x=22.5,y=7.5, color="floralwhite",label=game_crosses %>% 
                 filter(h_a==side)%>%
                 summarise(comps=sum(outcome_type=="Successful"),attempts=n(),rate=glue("{round(comps*100/attempts,1)}%"))%>%
                 pull(rate),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=18.5,y=7.5, color="floralwhite",label='Success %',family='Consolas',fontface="bold")
    # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
    # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
    # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
  }
  
  cross_table <- function(side){
    game_crosses %>% filter(h_a==side)%>%
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
  
  ((cross_plot("h")/cross_table("h")+plot_layout(heights = c(1,.2)))|(cross_plot("a")/cross_table("a")+plot_layout(heights = c(1,.2))))+plot_annotation(title = "Open Play Crosses",
                                                                                                                                                        subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                                                                                                                        theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                                                                                                                      plot.subtitle=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "grey65"),
                                                                                                                                                                      plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                                                                                                                        caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")
  
  
  ggsave("Charts/LFC/cross_map.png", width = 10, height = 10, dpi=300,type='cairo')
  
  
  
  key_assists <- events %>% clean_names()%>% filter(type=="Pass")%>%
    filter(assist==T | pass_key==T) %>%
    mutate(group=ifelse(assist==T,"Assist","Key Pass"),
           x_adj=to_int$x(x),
           y_adj=to_int$y(y),
           end_x_adj=to_int$x(end_x),
           end_y_adj=to_int$y(end_y)) 
  
  
  
  key_assist_plot <- function(side){
    key_assists %>% filter(h_a==side)%>% 
      ggplot()+
      annotate_pitch(colour = "grey75",dimensions = pitch_international,fill="grey25",limits = F)+
      geom_link(aes(x=x_adj, y=68-y_adj, xend = end_x_adj, yend = 68-end_y_adj,
                    alpha=stat(index),size=stat(I(2*index)),color=group)) +
      geom_point(aes(x=end_x_adj,y=68-end_y_adj,fill=group), shape = 21, col = "floralwhite",stroke=.5,size=2) +
      theme_pitch()+
      scale_fill_manual(values = c("Assist"="#2398B4","Key Pass"="#DCA62C"))+
      scale_color_manual(values = c("Assist"="#2398B4","Key Pass"="#DCA62C"))+
      coord_flip(ylim = c(0, 68))+
      theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
            legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
            panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
            plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0, color = "floralwhite"),
            plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0,vjust=-2),
            strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
            strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))+
      
      draw_text(glue(key_assists%>%filter(h_a==side)%>%
                       distinct(team_name)%>%pull(team_name)),y=1,x=106,fontface="bold",color="grey90",size=20,family="Consolas",hjust=0,vjust=1)+
      annotate(geom = "rect", xmin=40,xmax=45,ymin = 5,ymax=10, color="#DCA62C", fill="#DCA62C")+
      annotate(geom = "text", x=42.5,y=7.5, color="floralwhite",label=key_assists %>% 
                 filter(h_a==side,group=="Key Pass")%>%
                 summarise(attempts=n())%>%
                 pull(attempts),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=38.5,y=7.5, color="floralwhite",label='Key Passes',family='Consolas',fontface="bold")+
      annotate(geom = "rect", xmin=30,xmax=35,ymin = 5,ymax=10, color="#2398B4", fill="#2398B4")+
      annotate(geom = "text", x=32.5,y=7.5, color="floralwhite",label=key_assists %>% 
                 filter(h_a==side,group=="Assist")%>%
                 summarise(comps=n())%>%
                 pull(comps),fontface="bold",family='Consolas')+
      annotate(geom = "text", x=28.5,y=7.5, color="floralwhite",label='Assists',family='Consolas',fontface="bold")
    # draw_text("Passes into the Final Third v Tunisia",y=1,x=105,fontface="bold",color="floralwhite",size=10,family="Consolas",hjust=0,vjust=1)
    # annotate(geom = "text",x=2,y=109,label="Ghana",size = 15,fontface="bold",color="floralwhite",family="Consolas")
    # labs(title = "Ghana", subtitle = "<span style='color:#0F5C40;'>Successful</span> and <span style='color:#E75A0B;'>Unsuccessful</span> passes into the final third")
  }
  
  key_assist_plot("h")+key_assist_plot("a")+plot_annotation(title = "<span style='color:#2398B4;'>Assists</span> and <span style='color:#DCA62C;'>Key Passes</span> Map",
                                                            subtitle = glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                                                            theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                                                          plot.subtitle=element_markdown(face="bold", size=12,family="Consolas",hjust=0.5, color = "grey65"), 
                                                                          plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                                                            caption = "Data: Opta <br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/assist_pass_map.png", width = 10, height = 10, dpi=300,type='cairo')
  
  
  #add_logo("Charts/AFCON/f3rd_pass_map.png",afcon_logo,"top right") %>%
  #image_write("Charts/AFCON/f3rd_pass_map.png")
  
  
  stats_table <- 
    game_xg %>% 
    group_by(h_a) %>% 
    summarize(value = sum(is_goal == 'Goal')) %>% 
    mutate(stat="Goals") %>% 
    bind_rows(
      xg_map_df %>% 
        filter(goal_own==F) %>% 
        group_by(h_a) %>% 
        summarise(value=n()) %>% 
        mutate(stat='Shots'),
      events %>% clean_names() %>% 
        filter(is_shot==T,shot_on_target==T) %>% 
        group_by(h_a) %>% 
        summarise(value=n()) %>% 
        mutate(stat="Shots on target"),
      xg_map_df %>% 
        group_by(h_a) %>% 
        summarise(value=sum(x_g)) %>% 
        mutate(stat='xG'),
      passes_carries_df%>% filter(x_t>0) %>% 
        group_by(h_a) %>% 
        summarise(value=sum(x_t,na.rm=T)) %>% 
        mutate(stat='Expected threat'),
      touch_box%>% 
        group_by(h_a) %>% 
        summarise(value=sum(touch_box=='Box touch')) %>% 
        mutate(stat="Touches in the opponent's box"),
      game_passes %>% 
        group_by(h_a) %>% 
        summarise(comp=n())%>% 
        mutate(value=comp/sum(comp),stat='Possession') %>% select(-comp),
      
      filed_tilt_df %>% 
        group_by(h_a) %>% 
        summarise(comp=sum(pass_f3rd=="Final third pass"&outcome_type=="Successful")) %>% 
        mutate(value=comp/sum(comp),stat="Field tilt") %>% select(-comp), 
      game_passes %>% 
        mutate(throw_in=as.logical(throw_in))%>%
        group_by(h_a) %>% 
        summarise(total=n(),comp=sum(outcome_type=="Successful"), value=comp/total) %>% 
        select(-total,-comp) %>% 
        mutate(stat='Pass completion %'),
      game_crosses %>% 
        group_by(h_a) %>% 
        summarise(value=sum(outcome_type=="Successful")) %>% 
        mutate(stat='Successful Open Play Crosses'),
      game_passes %>% 
        filter(is_progressive==T)%>% 
        group_by(h_a) %>% 
        summarise(value=sum(outcome_type=="Successful")) %>% 
        mutate(stat='Successful Progressive Passes'),
      pass_final_third %>% 
        filter(pass_f3rd=='Final third entry')%>% 
        group_by(h_a) %>% 
        summarise(value=sum(outcome_type=="Successful")) %>% 
        mutate(stat='Successful Passes into Final Third'),
      events %>% clean_names() %>% 
        filter(type %in% c('Clearance','Interception','Tackle','BlockedPass'))%>%
        group_by(h_a,stat=type) %>% 
        summarise(value=sum(outcome_type=="Successful")) %>% 
        mutate(stat=case_when(stat=="BlockedPass"~"Blocked passes",
                              stat=="Tackle"~"Successful tackles",
                              TRUE~stat))
    ) %>% 
    pivot_wider(names_from = h_a,values_from = value)
  
  stats_table %>% 
    gt() %>% 
    cols_move_to_start(c(stat,h,a)) %>% 
    fmt_percent(columns = c(h,a),rows = c(7:9),decimals = 1)%>%
    fmt_number(columns = c(h,a),rows = c(4:5),decimals = 2)%>%
    fmt_number(columns = c(h,a),rows = c(1:3,6,10:16),decimals = 0)%>%
    tab_style(
      style = list(
        cell_fill(color = "#2398B4", alpha = 1),
        cell_text(color = "floralwhite", weight = 'bold')
      ),
      locations = cells_body(columns = c(h),
                             rows = h>a
      )) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#2398B4", alpha = 1),
        cell_text(color = "floralwhite", weight = 'bold')
      ),
      locations = cells_body(columns = c(a),
                             rows = a>h
      )) %>% 
   tab_header(title = md(glue("**{home_team} ({home_goals} - {away_goals}) {away_team}**")),
              subtitle = md(glue("{game_date} | Match Summary Statistics")))%>%
    # tab_header(title = html(glue("<p><img src='home.png' alt='pl logo' style='float:right;height:100px;'></p>
    #                             <p> <span style='font-size:30px;font-weight:bold'>Leaders</span></p>")))%>%
    
    cols_label(
      stat = "",
      h = md(glue("**{home_team}**")),
      a = md(glue("**{away_team}**"))
    ) %>% 
    # tab_source_note(md(glue("**Minimum 100 pass attempts. xT values from Karun Singh's xT model<br> Table: @CallmeAlfredo**")))%>%
    gt_theme_dark() %>%
    gtsave("Charts/LFC/summary_table.png",expand=0)
    
  
  # Passing Network
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
  pass_network_fn <- function(side,sub){
    team <- events%>%
      filter(h_a==side)%>%
      distinct(team_name)%>%
      pull(team_name)
    pass_net_df <- events %>% clean_names()%>%
      filter(type=="Pass",outcome_type=="Successful",h_a==side,minute<sub)%>%
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
      draw_text(glue("Up to first substitution ({sub}')"),y=1,x=106,fontface="bold",color="grey85",size=10,family="Consolas",hjust=0,vjust=1)+
      theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
            legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
            panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
            plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0.5, color = "floralwhite"),
            plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5),
            strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
            strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))
    
    return(plot)
  }
  
  pass_network_table <-  function(side){
    events %>% clean_names() %>% 
      filter(h_a==side,type=="Pass",is_cross==F,throw_in==F)%>%
      group_by(player_name)%>%
      summarise(attempts=n(),comp=sum(outcome_type=="Successful"))%>%
      arrange(-attempts) %>%
      dplyr::slice(1:5) %>%
      mutate(comp_pct=comp/attempts)%>%
      gt()%>%
      fmt_percent(columns = c(comp_pct),decimals = 0)%>%
      #fmt_number(columns = c(cpoe,xt_p90),decimals = 2)%>%
      tab_header(title = md("**Passing Leaders (full game)**"))%>%
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
        comp = md("**Completed**"),
        comp_pct=md("**Completion %**"))%>%
      cols_align(
        align = "center",
        columns = c(attempts,comp,comp_pct)
      )%>%
      # tab_source_note(md(glue("**Minimum 100 pass attempts. xT values from Karun Singh's xT model<br> Table: @CallmeAlfredo**")))%>%
      gt_theme_dark() %>%
      tab_options(heading.title.font.size = 15)%>%
      as_ggplot() 
  }
  
  ((pass_network_fn("h",home_first_sub)/pass_network_table("h")+plot_layout(heights = c(1,.2)))|(pass_network_fn("a",away_first_sub)/pass_network_table("a")+plot_layout(heights = c(1,.2))))+
    plot_annotation(title = "Passing Network", glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                    theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                  plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 12,hjust = 0.5),
                                  plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                    caption = "Minimum 4 passes b/n players<br>Circle size = number of passes<br>Color = Threats from passing based on full game (more orange is better)<br>Line thickness = number of passes between players<br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/pass_map.png", width = 10, height = 10, dpi=300,type='cairo')
  
  to_int<-rescale_coordinates(from = pitch_opta,to=pitch_international)
  pass_network_fn <- function(side,half,half_label){
    team <- events%>%
      filter(h_a==side)%>%
      distinct(team_name)%>%
      pull(team_name)
    pass_net_df <- events %>% clean_names()%>%
      filter(type=="Pass",outcome_type=="Successful",h_a==side,period==half)%>%
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
      draw_text(glue("{half_label}"),y=1,x=106,fontface="bold",color="grey85",size=10,family="Consolas",hjust=0,vjust=1)+
      
      theme(legend.position="none",legend.title = element_blank(),legend.background = element_blank(),
            legend.text = element_blank(),legend.key = element_blank(),aspect.ratio = 105/68,
            panel.background =element_blank(),plot.background = element_rect(color = NA, fill = "grey25"),
            plot.title=element_markdown(face="bold", size=14,family="Consolas",hjust=0.5, color = "floralwhite"),
            plot.subtitle = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5),
            strip.background = element_rect(fill="grey25"),plot.caption = element_markdown(color = "floralwhite",family = "Consolas",size = 10,hjust = 0.5,vjust=0.5,face = "bold"),
            strip.text = element_text(face="bold", size=10,color = "floralwhite",family = "Consolas"))
    
    return(plot)
  }
  
  pass_network_fn(side="h",half = "FirstHalf",half_label = "First Half")+pass_network_fn(side="h",half = "SecondHalf",half_label="Second Half")+
    plot_annotation(title = "Passing Network", glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                    theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                  plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 12,hjust = 0.5),
                                  plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                    caption = "Minimum 4 passes b/n players<br>Circle size = number of passes<br>Color = Threats from passing based on full game (more orange is better)<br>Line thickness = number of passes between players<br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/home_pass_map_halves.png", width = 10, height = 10, dpi=300,type='cairo')
  
  pass_network_fn(side="a",half = "FirstHalf",half_label = "First Half")+pass_network_fn(side="a",half = "SecondHalf",half_label="Second Half")+
    plot_annotation(title = "Passing Network", glue("{home_team}: {home_goals} v {away_team}: {away_goals} | {game_date}"),
                    theme = theme(plot.title=element_markdown(face="bold", size=25,family="Consolas",hjust=0.5, color = "floralwhite"),
                                  plot.subtitle = element_markdown(color = "grey65",family = "Consolas",size = 12,hjust = 0.5),
                                  plot.background = element_rect(color = NA, fill = "grey25"),plot.caption = element_markdown(color = "grey65",family = "Consolas",size = 10,hjust = 1,vjust=0.5,face = "bold")),
                    caption = "Minimum 4 passes b/n players<br>Circle size = number of passes<br>Color = Threats from passing based on full game (more orange is better)<br>Line thickness = number of passes between players<br>Chart by Alfred (@CallmeAlfredo)")
  
  ggsave("Charts/LFC/away_pass_map_halves.png", width = 10, height = 10, dpi=300,type='cairo')
  
  
}

events <-game_carries_xT("https://www.whoscored.com/Matches/1640703/Live/England-Premier-League-2022-2023-West-Ham-Brighton")%>%
  filter(period !="PenaltyShootout") %>%
  mutate(across(c(playerName,situation,shotBodyType),.fns=~as.character(.)),
         is_goal_kick=ifelse(str_detect(as.character(qualifiers),"GoalKick"),T,F),
         is_cross=ifelse(type=="Pass"&str_detect(as.character(qualifiers),"Cross"),T,F),
                team_abb=case_when(team_name=="Arsenal"~"ARS",
                                   team_name=="Aston Villa"~"AVL",
                                   team_name=="Brighton"~"BHA",
                                   team_name=="Bournemouth"~"BOU",
                                   team_name=="Brentford"~"BRE",
                                   team_name=="Chelsea"~"CHE",
                                   team_name=="Crystal Palace"~"CRY",
                                   team_name=="Everton"~"EVE",
                                   team_name=="Fulham"~"FUL",
                                   team_name=="Leeds"~"LEE",
                                   team_name=="Leicester City"~"LEI",
                                   team_name=="Liverpool"~"LIV",
                                   team_name=="Man City"~"MCI",
                                   team_name=="Man Utd"~"MUN",
                                   team_name=="Newcastle"~"NEW",
                                   team_name=="Nottingham Forest"~"NFO",
                                   team_name=="Southampton"~"SOT",
                                   team_name=="Tottenham"~"TOT",
                                   team_name=="West Brom"~"WBA",
                                   team_name=="West Ham"~"WHU",
                                   team_name=="Wolves"~"WOL"))


#lfc_viz(events) 


# Tweet viz

make_and_post_viz <- function(df){
  lfc_viz(df)
token <- rtweet::create_token(
  app = "Appiah Sports API",
  consumer_key = Sys.getenv("TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("TWITER_API_KEY_SCECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_TOKEN_SECRET"),
)

home_team <- events %>% filter(h_a=="h")%>%
  distinct(team_abb)%>% 
  pull(team_abb)
away_team <- events %>% filter(h_a=="a")%>%
  distinct(team_abb)%>% 
  pull(team_abb)

game_hashtag <- glue("#{home_team}{away_team}")



post_tweet(glue("{game_hashtag} Game Summary, shot map, xG timeline and xT timeline"),media = c("Charts/LFC/summary_table.png","Charts/LFC/xg_map_cropped.png","Charts/LFC/xg_timeline.png","Charts/LFC/xt_timeline.png"),
           token = token)
my_timeline <- get_timeline(rtweet:::home_user())

reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Passing Network"), media = c("Charts/LFC/pass_map.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Passing Network by Half"), media = c("Charts/LFC/home_pass_map_halves.png","Charts/LFC/away_pass_map_halves.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Most Threatening Ball Progressors"), media = c("Charts/LFC/xt_chart.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Touches in the Opponent's Box"), media = c("Charts/LFC/touch_box.png"),
           in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Passes into the Final Third"), media = c("Charts/LFC/f3rd_pass_map.png"),
           in_reply_to_status_id = reply_id)
my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Open Play Crosses and Passes into the Penalty Box"), media = c("Charts/LFC/cross_map.png","Charts/LFC/pass_box.png"),
           in_reply_to_status_id = reply_id)
my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

post_tweet(glue("{game_hashtag} Open Play Switches of Play"), media = c("Charts/LFC/switch_map.png"),
           in_reply_to_status_id = reply_id)

}

make_and_post_viz(events)

