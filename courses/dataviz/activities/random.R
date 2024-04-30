


ggplot()+
  geom_line()+
  facet_wrap(~name, 
             scales = "free_y", #only allowing y axis to scale
             ncol = 1)  

#use the factor function with raw data if you want to rename the column labels or change the order in which they are displayed

ggplot()+
  geom_line()+
  facet_grid(name~code, scales = "free_y")+
  theme_bw()+
  ggtitle("My Plot") +
  theme(legend.position = "bottom", 
        strip.text.y = element_text(color = "red", size = 16), 
        plot.background = element_rect(fill = "colour"),
        legend.background = element_rect(fill = "color")+
        legend.title = element_blank(), 
        axis.text.x = element_blank())

scale_color_discrete(name = "") #removing the legend title