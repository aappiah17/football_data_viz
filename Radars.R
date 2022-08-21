big5_players <- player_dictionary_mapping() %>% 
  clean_names()


teams_player_url_1 <- teams_player_url %>% 
  mutate(player_name=case_when(player_name=="Thiago"~"Thiago Alcántara",
                               player_name=="Joel Matip"~"Joël Matip",
                               TRUE~player_name)) %>% 
  bind_rows(tribble(~player_name,~team_name,~image_url,
                    "Alexander Djiku","Strasbourg","https://www.rcstrasbourgalsace.fr/wp-content/uploads/2019/07/DJIKU-Alexander_effectif_site.png",
                    "Iddrisu Baba","Mallorca","https://assets.laliga.com/squad/2021/t181/p463170/512x512/p463170_t181_2021_1_003_000.png",
                    "Joseph Aidoo","Celta","https://assets.laliga.com/squad/2021/t176/p210171/256x278/p210171_t176_2021_1_001_000.png",
                    "Gideon Mensah","Bordeaux","https://www.myfootballfacts.com/wp-content/uploads/2022/02/5c6e0ce28d7f52ccf0fc6d77ff86825e.png",
                    "Felix Afena-Gyan","Roma","https://asroma2-cloudinary.corebine.com/asroma2-production/image/upload/c_fit,dpr_1.0,f_webp,g_center,q_auto/v1/asroma2-prod/FELIX_MOTM_WINNER_GENOA_square-removebg-preview_mq79bg",
                    "Osman Bukari","Nantes","https://images.fotmob.com/image_resources/playerimages/949859.png",
                    "Aurélien Tchouaméni","Monaco","https://i.goalzz.com/?i=o%2Fp%2F142%2F747%2Faurelien-tchouameni-1.png")
  )
# Use WorldFootball R data
player_standard_stats <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "standard", team_or_player= "player")
player_possession_stats <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "possession", team_or_player= "player")
player_passing_stats <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "passing", team_or_player= "player")
player_shooting_stats<-fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "shooting", team_or_player= "player")
player_gca_stats<-fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "gca", team_or_player= "player")
player_defense_stats<-fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "defense", team_or_player= "player")

team_season_stats <- fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "standard", team_or_player= "team")

player_defense_stats_1 <- player_defense_stats %>% 
  clean_names() %>% 
  mutate(player=case_when(player=="Emile Smith Rowe"~"Emile Smith-Rowe",
                          TRUE~player)) %>% 
  left_join(big5_players,by=c("player"="player_f_bref","url"="url_f_bref")) %>% 
  relocate(tm_pos,.after = pos) %>% 
  mutate(tm_pos=case_when(player=="Luis Díaz"~"Left Winger",
                          player=="Hugo Álvarez"~"Central Midfield",
                          player=="Aboubakar Sidibé"~"Right Winger",
                          TRUE~tm_pos),
         position_group=case_when(tm_pos=="Goalkeeper"~"Goalkeepers",
                                  tm_pos %in% c("Left-Back","Right-Back")~"Fullbacks",
                                  tm_pos %in% c("Centre-Back")~"Center Backs",
                                  tm_pos %in% c("Central Midfield","Defensive Midfield","Left Midfield","Right Midfield")~"Midfielders",
                                  tm_pos %in% c("Attacking Midfield","Right Winger","Left Winger")~"Attacking Midfielders / Wingers",
                                  tm_pos %in% c("Centre-Forward","Second Striker")~"Forwards")) %>% 
  relocate(position_group,.after = tm_pos) %>% 
  group_by(player)%>%
  mutate(across(c(mins_per_90:err),~sum(.)))%>%
  filter(duplicated(player)&n()==2|n()==1)%>% ungroup()


plot_player_scout_full <- function(First, Last, pl_id,pos,pos_full,league_id,league){

  player_data <-read_html(glue("https://widgets.sports-reference.com/wg.fcgi?css=1&site=fb&url=%2Fen%2Fplayers%2F{pl_id}%2Fscout%2F{league_id}%2F{First}-{Last}-Scouting-Report&div=div_scout_full_{pos}"))%>%
    html_nodes("table")%>%
    html_table() %>% as.data.frame() %>%
    set_names(c("stat","value_per_90","percentile"))%>% as_tibble()%>%
    dplyr::slice(-1)%>%
    filter(stat!="",stat %nin% c("Shooting","Statistic","Passing","Pass Types","Goal and Shot Creation","Defense","Miscellaneous Stats","Possession"))%>%
    mutate(value_per_90_num=str_extract(value_per_90,"\\d+\\.*\\d*"),percentile=as.numeric(percentile))%>%
    group_by(stat)%>%
    distinct(stat,.keep_all=T)%>%
    filter(stat %in% c("Non-Penalty Goals", "npxG","npxG+xA", "xA","Assists","Shot-Creating Actions","Shots Total",
                       "Pass Completion%","Progressive Passes","Progressive Carries","Dribbles Completed","Touches (Att Pen)",
                       "Key Passes", "Passes into Final Third", "Passes into Penalty Area"))%>%
    mutate(group=case_when(stat %in% c("Non-Penalty Goals", "npxG","npxG+xA", "xA","Assists","Shot-Creating Actions","Shots Total")~"Goals,Assists and Shot Creation",
                           stat %in% c("Pass Completion%","Progressive Passes","Progressive Carries","Dribbles Completed","Touches (Att Pen)",
                                       "Key Passes", "Passes into Final Third", "Passes into Penalty Area")~ "Passing and Possession"))
  
  player_shots <-player_data %>% filter(group=="Goals,Assists and Shot Creation")%>%
    ggplot(aes(y=reorder(stat,percentile),x=percentile,fill=percentile))+
    geom_col(aes(x = 100), fill="white", width = .8) +
    geom_col(aes(x = percentile), width = .8) +
    geom_col(width = .8) +
    expand_limits(x=c(0,100))+
    #scale_fill_brewer(palette = "Set1")+
    scale_x_continuous(labels = comma,expand = c(0,0))+
    #ggdark::dark_theme_minimal(base_family = "Consolas", base_size = 18)+
    scale_fill_gradient2(low="firebrick1", midpoint=50, mid="#FCD116",high="#009900",
                         breaks=c(5,10,20,50,70,100),limits=c(1,100))+
    geom_text(aes(label=percentile),fontface="bold",size=4,hjust=1.5,color="floralwhite",family="Consolas")+
    AAtheme_patch+
    theme(plot.title=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "white"),
          legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),
          panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),plot.background = element_rect(color = NA, fill = "grey25"))+
    labs(title = "Goals, Assists and Shot Creation")
  
  player_pass_data <-player_data %>% filter(group=="Passing and Possession")
  temp <- (360/(length(player_pass_data$stat))/2)                             
  myAng <- seq(-temp, -360+temp, length.out = length(player_pass_data$stat))  
  ang<-ifelse(myAng < -90, myAng+180, myAng)                                  
  ang<-ifelse(ang < -90, ang+180, ang)  
  
  player_pass <-player_pass_data%>%
    ggplot(aes(y=percentile,x=str_wrap(stat,15),fill=percentile))+
    geom_col(width = 1,color="white") +
    # geom_col(aes(x = percentile), width = 0.7) +
    # geom_col(width = 0.7) +
    #expand_limits(x=c(0,130))+
    #scale_fill_brewer(palette = "Set1")+
    #scale_x_continuous(labels = comma,expand = c(0,0))+
    geom_hline(yintercept = seq(0, 100, by = 50),
               color = "white",
               size = .6) +
    geom_vline(xintercept = seq(.5, 8.5, by = 1),
               color = "white",
               size = .1) +
   # ggdark::dark_theme_minimal(base_family = "Consolas", base_size = 18)+
    scale_fill_gradient2(low="firebrick1", midpoint=50, mid="#FCD116",high="#009900",
                         breaks=c(0,5,10,20,30,50,60,70,90,100),limits=c(1,100))+
    # scale_fill_manual(values = c("#009900","firebrick1"))+
    coord_polar(clip = "off")+
    # geom_text(aes(label=percentile,vjust=ifelse(percentile>=80,0.5,-0.5)),fontface="bold",size=4)+
    geom_label(data=filter(player_pass_data,percentile>40),aes(fill=percentile,label=glue("{round(percentile,0)}")),size=4,color="white",
               fontface="bold",family="Consolas")+
    AAtheme_patch+
    theme(plot.title=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "white"),
          legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(face = "bold",size = 11,hjust = 0.5,angle = ang),axis.text.y=element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(color = NA, fill = "grey25"))+
    labs(title = "Passing and Possession")
  
  pl_defense_percentiles <- player_defense_stats_1 %>% select(team=squad,comp:position_group,age,mins_per_90,tkl_tackles,tkl_w_tackles,press_pressures,succ_pressures,
                                                              blocks_blocks,int,clr) %>% 
    filter(mins_per_90>=5,comp==league) %>% 
    left_join((team_season_stats%>%clean_names() %>% 
                 filter(team_or_opponent=="opponent")%>%select(team=squad,poss)))%>%
    # Calculate possession adjusted defensive stats
    mutate(across(c(tkl_tackles,tkl_w_tackles,press_pressures,succ_pressures,
                    blocks_blocks,int,clr),.fns = list(padj= ~(50/poss)*.)),
           across(c(tkl_tackles_padj,tkl_w_tackles_padj,press_pressures_padj,succ_pressures_padj,
                    blocks_blocks_padj,int_padj,clr_padj),.fns = list(p90= ~./mins_per_90))) %>% 
    group_by(position_group) %>% 
    mutate(across(c(tkl_tackles_padj_p90,tkl_w_tackles_padj_p90,press_pressures_padj_p90,succ_pressures_padj_p90,
                    blocks_blocks_padj_p90,int_padj_p90,clr_padj_p90),.fns = list(pct= ~ntile(.,99))),
           age = as.numeric(str_sub(age,1,2))) %>% ungroup()
  
  player_defense_df <-pl_defense_percentiles %>% filter(player==glue("{First} {Last}")) %>% 
    select(Tackles=tkl_tackles_padj_p90_pct, `Tackles won`=tkl_w_tackles_padj_p90_pct,
           Pressures=press_pressures_padj_p90_pct,`Successful Pressures`=succ_pressures_padj_p90_pct,Blocks=blocks_blocks_padj_p90_pct,Interceptions=int_padj_p90_pct,
           Clearances=clr_padj_p90_pct) %>% 
    pivot_longer(cols=everything(),names_to="stat",values_to = "percentile")
  temp <- (360/(length(player_defense_df$stat))/2)                             
  myAng <- seq(-temp, -360+temp, length.out = length(player_defense_df$stat))  
  ang<-ifelse(myAng < -90, myAng+180, myAng)                                  
  ang<-ifelse(ang < -90, ang+180, ang)  
  
  player_defense <-player_defense_df%>%
    ggplot(aes(y=percentile,x=str_wrap(stat,15),fill=percentile))+
    geom_col(width = 1,color="white") +
    geom_hline(yintercept = seq(0, 100, by = 50),
               color = "white",
               size = .6) +
    geom_vline(xintercept = seq(.5, 8.5, by = 1),
               color = "white",
               size = .1) +
   # ggdark::dark_theme_minimal(base_family = "Consolas", base_size = 18)+
    scale_fill_gradient2(low="firebrick1", midpoint=50, mid="#FCD116",high="#009900",
                         breaks=c(0,5,10,20,30,50,60,70,90,100),limits=c(1,100))+
    # scale_fill_manual(values = c("#009900","firebrick1"))+
    coord_polar(clip = "off")+
    # geom_text(aes(label=percentile,vjust=ifelse(percentile>=80,0.5,-0.5)),fontface="bold",size=4)+
    geom_label(data=filter(player_defense_df,percentile>40),aes(fill=percentile,label=glue("{round(percentile,0)}")),size=4,color="white",
               fontface="bold",family="Consolas")+
    AAtheme_patch+
    theme(plot.title=element_markdown(face="bold", size=15,family="Consolas",hjust=0.5, color = "white"),
          legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(face = "bold",size = 11,hjust = 0.5,angle = ang),axis.text.y=element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(color = NA, fill = "grey25"))+
    labs(title = "Defensive Actions")
  
  age <- pl_defense_percentiles %>% filter(player==glue("{First} {Last}")) %>% 
    pull(age)
  mins_played <- pl_defense_percentiles %>% filter(player==glue("{First} {Last}")) %>% 
    pull(mins_per_90)
  
  player_image <-teams_player_url_1%>%
    filter(player_name==glue("{First} {Last}"))%>%
    pull(image_url)%>%
    image_read()%>%
    image_ggplot()
  
  player_all<-(player_image/player_shots)|player_pass|player_defense
  
  player_all|plot_annotation(
    title = glue("{First} {Last} v Other {pos_full} in {league}"),
    subtitle = glue("Age: {age} | Season: 2021/22 | 90s played: {mins_played}"),
    caption = 'All defensive stats are posession adjusted <br>Data:Fbref.com | Graph:@CallmeAlfredo',theme =AAtheme_patch+theme(plot.title=element_markdown(face="bold", size=20,family="Consolas",hjust=0.5, vjust=-1.5,color = "floralwhite",
                                                                                                                                                            margin=margin(20,10,0,0)),
                                                                                                                                plot.subtitle = element_markdown(face="bold", size=12,family="Consolas",hjust=0.5, vjust=-1.5,color = "grey80"),
                                                                                                                                legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank(),
                                                                                                                                
                                                                                                                                panel.grid = element_blank(),
                                                                                                                                plot.background = element_rect(color = NA, fill = "grey25")))
  ggsave(glue("Charts/Radars/{First}_{Last}.png"), width = 15.5, height = 9, dpi=300,type="cairo",bg="grey25")
}

plot_player_scout_full ("Trent", "Alexander-Arnold","cd1acf9d","FB","Fullbacks")
plot_player_scout_full ("Andrew", "Robertson","2e4f5f03","FB","Fullbacks")

plot_player_scout_full ("João", "Cancelo","bd6351cd","FB","Fullbacks")
plot_player_scout_full ("Reece", "James","1265a93a","FB","Fullbacks")

plot_player_scout_full ("Antonio", "Rüdiger","18b896d6","CB","Center Backs")
plot_player_scout_full ("Mohammed", "Salisu","0b33f6ad","CB","Center Backs")

plot_player_scout_full ("Raheem", "Sterling","b400bde0","AM","Att. Mids/Wingers")

plot_player_scout_full ("Jordan", "Ayew","da052c14","AM","Attacking Midfielders/Wingers")

plot_player_scout_full ("Thiago", "Alcántara","77e84962","MF","Defensive/Central Midfielders","11160","Premier League")
plot_player_scout_full ("Jordan", "Henderson","935e6b8f","MF","Defensive/Central Midfielders","11160","Premier League")
plot_player_scout_full ("Naby", "Keïta","f25c8e3a","MF","Defensive/Central Midfielders","11160","Premier League")
plot_player_scout_full ("Fabinho", "","77e84962","MF","Defensive/Central Midfielders")

plot_player_scout_full ("Sadio", "Mané","c691bfe2","AM","Attacking Midfielders/Wingers","11160","Premier League")

plot_player_scout_full ("Mohamed", "Salah","e342ad68","AM","Attacking Midfielders/Wingers","11160","Premier League")

plot_player_scout_full ("Luis", "Díaz","4a1a9578","AM","Attacking Midfielders/Wingers","11160","Premier League")


plot_player_scout_full ("Curtis", "Jones","4fb9c88f","MF","Defensive/Central Midfielders","11160","Premier League")

plot_player_scout_full ("Andrew", "Robertson","2e4f5f03","FB","Fullbacks","11160","Premier League")
plot_player_scout_full ("Joël", "Matip","b217ef29","CB","Center Backs","11160","Premier League")

plot_player_scout_full ("Aurélien", "Tchouaméni","4f255115","MF","Defensive/Central Midfielders","11183","Ligue 1")




#Ghana 
plot_player_scout_full ("Thomas", "Partey","529f49ab","MF","Defensive/Central Midfielders","11160","Premier League")
plot_player_scout_full ("Iddrisu", "Baba","bfd381b3","MF","Defensive/Central Midfielders","11174","La Liga")
plot_player_scout_full ("Alexander", "Djiku","43e7a164","CB","Center Backs","11183","Ligue 1")
plot_player_scout_full ("Daniel", "Amartey","1a4ef233","CB","Center Backs","11160","Premier League")
plot_player_scout_full ("Joseph", "Aidoo","18abe173","CB","Center Backs","11174","La Liga")
plot_player_scout_full ("Gideon", "Mensah","9bdd8118","FB","Full Backs","11183","Ligue 1")

plot_player_scout_full ("Jordan", "Ayew","da052c14","AM","Attacking Midfielders/Wingers","11160","Premier League")
plot_player_scout_full ("Felix", "Afena-Gyan","0154d290","FW","Forwards","11222","Serie A")
plot_player_scout_full ("Osman", "Bukari","eab85789","AM","Attacking Midfielders/Wingers","11183","Ligue 1")
plot_player_scout_full ("Tariq", "Lamptey","f4e433d4","FB","Fullbacks","11160","Premier League")



player_defense_stats_1 %>% 
  group_by(comp) %>% 
  tally()





