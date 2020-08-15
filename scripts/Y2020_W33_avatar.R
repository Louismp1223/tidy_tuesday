library(here)
library(tidyverse)
library(tvthemes)
library(extrafont)
extrafont::fonts()
loadfonts(device = "win")
import_avatar()
# library(devtools)
# devtools::install_github("averyrobbins1/appa")
# library(appa)

avatar <- appa::appa

scene_description2 <- avatar %>% 
  select(id, scene_description) %>% 
  unnest_longer(scene_description) %>% 
  filter(!is.na(scene_description))

scene_description <- avatar %>%
  filter(character=="Scene Description") %>% 
  select(-scene_description)

avatar <- avatar %>% 
  filter(character!="Scene Description") %>% 
  select(-scene_description)

# split where paired (ex. Aang and Zuko)
tosplit <- avatar %>% 
  filter(str_detect(character," and ")) %>% 
  separate_rows(character, sep = " and ")

# remove rows with " and " then add in split rows and order
avatar <- avatar %>% 
  filter(!str_detect(character," and ")) %>% 
  add_row(tosplit) %>% 
  arrange(id)


# remove punctuation and count the number of words
avatar <- avatar %>% 
  mutate(new_text=str_replace_all(character_words,"[[:punct:]]+"," "),
         numwords=str_count(new_text,"\\S+"))



# get the top characters based on number of words spoken (could'a told ya these)
topchars <- avatar %>% 
  group_by(character) %>% 
  summarise(words=sum(numwords)) %>% 
  arrange(desc(words)) %>% 
  top_n(n=7) %>% 
  ungroup()

# count number of words
words <- avatar %>% 
  filter(character %in% topchars$character) %>% 
  group_by(book_num,chapter_num,character) %>% 
  summarise(words=sum(numwords)) %>% 
  arrange(book_num,chapter_num,desc(words)) %>% 
  ungroup() %>% 
  mutate(chap_num_pad=str_pad(chapter_num,2,pad="0")) %>%
  select(-chapter_num) %>% 
  unite("episode",c(book_num,chap_num_pad),sep = ".") %>%
  complete(episode,character) %>% 
  replace_na(list(words=0)) %>% 
  group_by(character) %>% 
  mutate(cumsum_words=cumsum(words)) %>% 
  ungroup() %>% 
  mutate(book=rep(rep(levels(factor(avatar$book)),times=c(20,20,21)),each=7),
         title=rep(levels(factor(avatar$chapter)),each=7))

ep_titles <- as.character(levels(factor(avatar$chapter)))
depth <- -6000
marjust <- -190
ggplot(words,aes(x=episode,y=cumsum_words,group=factor(character),color=factor(character)))+
  annotate("segment",x=1,xend=1,y=0,yend=20000,color="darkblue",linetype="dashed")+
  annotate("rect",xmin=1,xmax=20,ymin=0,ymax=20000,fill="lightblue",alpha=0.2)+
  annotate("text",x=10,y=20500,label="Water",color="darkblue",family="Slayer",size=6)+
  annotate("segment",x=20,xend=20,y=depth,yend=20000,color="darkblue",linetype="dashed")+
  annotate("rect",xmin=20,xmax=40,ymin=0,ymax=20000,fill="lightgreen",alpha=0.2)+
  annotate("text",x=30,y=20500,label="Earth",color="darkgreen",family="Slayer",size=6)+
  annotate("segment",x=40,xend=40,y=depth,yend=20000,color="darkgreen",linetype="dashed")+
  annotate("rect",xmin=40,xmax=61,ymin=0,ymax=20000,fill="lightcoral",alpha=0.2)+
  annotate("text",x=50,y=20500,label="Fire",color="firebrick",family="Slayer",size=6)+
  annotate("segment",x=61,xend=61,y=depth,yend=20000,color="firebrick",linetype="dashed")+
  geom_line(size=1.2)+
  annotate("curve",x=10,y=17500,xend=22,yend=9000,curvature=0.4,arrow=arrow(length=unit(2,"mm")),size=1)+
  annotate("text",x=17,y=18000,label="Generally Aang stays on top,\n however...",color="black",family="Slayer",size=3.5,hjust=0.5)+
  annotate("curve",x=34,y=16500,xend=60,yend=19500,curvature=-0.2,arrow=arrow(length=unit(2,"mm")),size=1)+
  annotate("text",x=26,y=15000,label="Sokka overtakes in the end.\nThe show should be called \nSokka: Warrior of the \nSouthern Water Tribe.",color="black",family="Slayer",size=3.5,hjust=0.5)+
  annotate("curve",x=46,y=6500,xend=50,yend=5200,curvature=-0.3,arrow=arrow(length=unit(2,"mm")),size=1)+
  annotate("text",x=36,y=6500,label="Zuko joins Team Avatar \n and gets a bigger role.",color="black",family="Slayer",size=3.5,hjust=0.5)+
  
  scale_color_manual(values = c("#ff9933","#000000","#7E605E","#1DB4D3","#0047ab","#015E05","#FF4500"))+
  labs(title = "Avatar Jibber Jabber",
       subtitle = "Who has the largest speaking role on Avatar: The Last Airbender?",
       color="Character",
       x="episode",
       y="Cumulative Words Spoken",
       caption = "Data from appa::appa\nVizualization by LM Penrod (@P_Louis2)")+
  scale_x_discrete(labels=ep_titles)+
  theme_avatar(title.font = "Slayer",
               legend.position = "right",
               legend.font = "Slayer")+
  theme(legend.key = element_blank(),
        axis.text.x = element_text(angle=90,hjust = 1,vjust=-0.5,margin=margin(t=marjust)),
        axis.title.y = element_text(margin = margin(r=10)),
        panel.grid = element_line(color="transparent"),
        plot.title = element_text(hjust=0.5,size=18),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "gray50"),
        axis.title = element_text(size=12))

ggsave("ttY2020W33_1.png",path=here("plots"),width=10.5,height=11.5,units="in",dpi = 300)
# export size: 1050widex900tall
