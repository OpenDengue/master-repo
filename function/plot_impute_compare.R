library(ggpubr)

plot_impute_compare<- function(x1, x2){
 # create a list of all possible group categories for all countries
all_source <- c("Raw", "Imputed")

# define a custom color palette that includes all the possible group categories
colors <- c("#1f77b4", "#ff7f0e" )  # define colors for each group
colors2 <- c("#1f77b4", "#59A14F" )  # define colors for each group

names(colors) <- all_source  # assign names to each color
names(colors2) <- all_source  # assign names to each color

  
p1 <- ggplot(data=x1, aes(x=epidemiological_week, y=value, fill=source_cat))+
  geom_bar(stat="identity") +
  scale_fill_manual(values=colors, 
                    breaks=c("Raw", "Imputed"), 
                    labels=c("Raw", "Imputed (na.approx)")) +
  
  labs(title=paste0(x1$adm_0_name, " (", x1$year, ")"))+
  theme(plot.title = element_text(size=25))+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=20))+
  theme(legend.title= element_blank(), 
        legend.text = element_text(size=15))+
  theme(strip.text = element_text(size=22))+
  #facet_wrap(year~., scales= "free_y", strip.position = "top")+
  xlab("Epidemiological week")+
  ylab("Dengue incident cases")

p2 <- ggplot(data=x2, aes(x=epidemiological_week, y=value, fill=source_cat))+
  geom_bar(stat="identity") +
  scale_fill_manual(values=colors2, 
                    breaks=c("Raw", "Imputed"), 
                    labels=c("Raw", "Imputed (na.spline)")) +
  
  labs(title=paste0(x2$adm_0_name, " (", x2$year, ")"))+
  theme(plot.title = element_text(size=25))+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=20))+
  theme(legend.title= element_blank(), 
        legend.text = element_text(size=15))+
  theme(strip.text = element_text(size=22))+
  #facet_wrap(year~., scales= "free_y", strip.position = "top")+
  xlab("Epidemiological week")+
  ylab("Dengue incident cases")

ggarrange(p1, p2, ncol=2, nrow=1)
  
}