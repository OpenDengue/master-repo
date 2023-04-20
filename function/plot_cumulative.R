plot_cumulative<- function(x){
  
  ggplot(data=x, aes(x=epidemiological_week, y=dengue_total_cumulative, group=factor(year)))+
  geom_col(aes(fill=factor(Year)))+
  facet_wrap(Year~., scales= "free_y")+
  labs(title=paste0(x$adm_0_name))+
    scale_fill_discrete(name="Year")
}