

plot_cumulative2<- function(x){
 # create a list of all possible group categories for all countries
all_source <- c("Raw", "Imputed", "Zero")

# define a custom color palette that includes all the possible group categories
colors <- c("#1f77b4", "#ff7f0e", "#59A14E")  # define colors for each group
names(colors) <- all_source  # assign names to each color

  ggplot()+
   #geom_col(aes(fill=factor(source_cat)), color="grey50", linewidth=0.05)+
    geom_bar(data=x, stat="identity", 
             aes(x=epidemiological_week, y=dengue_na_approx2, fill=source_cat)) +
   geom_point(data=x, 
             aes(x=epidemiological_week, y=dengue_na_approx2, color = source_cat))+
    scale_fill_manual(values=colors, 
                     breaks=c("Raw", "Imputed", "Zero"), 
                     labels=c("Raw", "Imputed", "Zero")) +
    scale_color_manual(values=colors, 
                     breaks=c("Raw", "Imputed", "Zero"), 
                     labels=c("Raw", "Imputed", "Zero"))+
    scale_y_continuous(limits=function(x){c(0, max(0.1, x))}, 
                     labels=scales::number_format(big.mark=",", accuracy = 0.1))+
  labs(title=paste0(x$adm_0_name))+
  theme(plot.title = element_text(size=25))+

  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=20))+
  theme(legend.title= element_blank(), 
        legend.text = element_text(size=15))+
  theme(strip.text = element_text(size=22))+
  facet_wrap(year~., scales= "free_y", strip.position = "top")+
    xlab("Epidemiological week")+
    ylab("Dengue cumulative cases")
}



plot_cumulative2_spline<- function(x){
 # create a list of all possible group categories for all countries
all_source <- c("Raw", "Imputed", "Zero")

# define a custom color palette that includes all the possible group categories
colors <- c("#1f77b4", "#ff7f0e", "#59A14E")  # define colors for each group
names(colors) <- all_source  # assign names to each color

  ggplot()+
   #geom_col(aes(fill=factor(source_cat)), color="grey50", linewidth=0.05)+
    geom_bar(data=x, stat="identity", 
             aes(x=epidemiological_week, y=dengue_na_spline2, fill=source_cat)) +
   geom_point(data=x, 
             aes(x=epidemiological_week, y=dengue_na_spline2, color = source_cat))+
    scale_fill_manual(values=colors, 
                     breaks=c("Raw", "Imputed", "Zero"), 
                     labels=c("Raw", "Imputed", "Zero")) +
    scale_color_manual(values=colors, 
                     breaks=c("Raw", "Imputed", "Zero"), 
                     labels=c("Raw", "Imputed", "Zero"))+
    scale_y_continuous(limits=function(x){c(0, max(0.1, x))}, 
                     labels=scales::number_format(big.mark=",", accuracy = 0.1))+
  labs(title=paste0(x$adm_0_name))+
  theme(plot.title = element_text(size=25))+

  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=20))+
  theme(legend.title= element_blank(), 
        legend.text = element_text(size=15))+
  theme(strip.text = element_text(size=22))+
  facet_wrap(year~., scales= "free_y", strip.position = "top")+
    xlab("Epidemiological week")+
    ylab("Dengue cumulative cases")
}