# remotes::install_github("wilkelab/ggtext")

library(readxl)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(flexdashboard)
library(extrafont)
library(ggrepel)
library(gganimate)
library(lubridate)
library(ggtext)


loadfonts(device = "win")

df <- read_excel("C:/Users/huglebre/Downloads/Weekly Fuel Prices.xlsx", 
                 col_types = c("date", "numeric", "numeric")) %>% 
  mutate(year = year(Date))


# Data preparation
pt1 <- df %>% 
  select(Date, `Petrol (USD)`  ) %>% 
  mutate(values = `Petrol (USD)`, 
         energy = 'Petrol') %>% 
  select(- `Petrol (USD)` )

pt2 <- df %>% 
  select(Date, `Diesel (USD)`  ) %>% 
  mutate(values = `Diesel (USD)`, 
         energy = 'Diesel') %>% 
  select(-`Diesel (USD)`)


df_def <- rbind(pt1, pt2) %>% 
  mutate(values = values/100,
         year = year(Date),
         labelv = if_else(Date == max(Date), values, NULL)) 

df_def$Date <- as.Date(df_def$Date)


df_petrol <- df_def %>% filter(energy == 'Petrol') 
mean_petrol <- round(mean(df_petrol$values),2)

df_diesel <- df_def %>% filter(energy == 'Diesel') 
mean_diesel <- round(mean(df_diesel$values),2)


p <- df_def %>% 
  ggplot( aes( x = Date, y = values, fill = energy , color = energy )  )+
  geom_line( size = 1.1 )+
  ylim(0,1.6)+
  geom_text_repel(aes(y = values, x = Date, label = round(labelv,2), hjust = 3))+
  geom_hline(yintercept=mean_diesel, color='#111111', linetype = 3)+
  annotate(geom="text", x = as.Date("2004-06-05"), 
           y = 1.2, label = paste0("Avg $",mean_diesel), fontface="bold", colour='#111111')+
  geom_hline(yintercept=mean_petrol, color='#2d9258', linetype = 3)+
  annotate(geom="text", x = as.Date("2004-06-05"), 
           y = 1.08, label = paste0("Avg $",mean_petrol), fontface="bold", colour='#2d9258')+
scale_x_date(date_breaks = "year" , date_labels = "%Y")+
  scale_color_manual(values=c('#111111','#2d9258'))+
  labs(
    title = "<span style='font-size:18pt'> UK
    <span style='color:#111111;'>**DIESEL**</span> &
    <span style='color:#2d9258;'>**PETROL**</span>
    PUMP PRICES *(2003-2020)*
    </span>",
    x = NULL,
    caption = "#MakeoverMonday • Week17 • BEIS • Hugo Lèbre")+
  theme(
    # Titre centrÃ© et Taille
    plot.title = element_markdown(lineheight = 1.1, family = 'Rockwell'),
    legend.position = "none",
    #Texte de la lÃ©gende
    
    # Axe des X
    # Ligne des axes en noir
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    # Label
    axis.text.x = element_text(angle = 90, vjust = 0.1, family = 'Rockwell'),
    
    # Axe des Y
    # Axe
    axis.text.y = element_text(family = 'Rockwell'),
    # Titre de l'axe
    axis.title.y = element_text(family = 'Rockwell'),
    
    # Fond du graphique
    panel.background = element_blank(),
    # Axes
    panel.border = element_blank(),
    # Bordures
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
p

a<-p + transition_reveal(Date)

anim <- animate(a,duration = 30,
        fps = 20,
        width = 900, 
        height = 500,
        end_pause = 200,
        )


anim_save("mom_week17.gif")

magick::image_write(
  anim, 
  "test.gif"
)




