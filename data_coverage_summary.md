OpenDengue data coverage
================
Joe Clarke & Ahyoung Lim
Last update on March 22, 2023

# Background

This document contains the relevant R code and output for producing
country specific summaries of : 1. Current case data data coverage in
OpenDengue that has been extracted + processed + merged, while all may
not yet be available on the rep, some may be in the process of being
uplaoded from shared dropbox. 2. This data prioritises admin level 1
spatial resolution and the highest temporal resolution available. There
may be more data to come at higher spatial resolution. 3. This data
forms part of the “primary” dataset for each country, which contains :
Temporal resolution, spatial resolution, an aggregate of all dengue
cases (varying definitions of dengue and case severity). This does not
include disaggregation by age, sex, disease classification, mortality or
serotype. This will follow in a release of the “secondary” dataset for
each country.

## Objectives

The purpose of this is twofold: 1. to be able to identify gaps in data
coverage such that we can use opportunistic sources to attempt to fill
them. 2. to link to a three updated heat maps showing coverage by time,
space and data source. These will be archived to show progress in
OpenDengue.

# Data preprocessing

Create columns for categorising each row according to it’s spatial
resolution & temporal resolution.

Convert all case counts from PAHO portal to weekly. While they are
currently cumulative with the calendar_end_date increasing weekly, we
will be developing models to spread this data across weekly counts
before release. In the portal itself, all denge cases are only listed
cumulatively in single week increments, while incident “suspected” cases
are listed weekly.

``` r
data <- read.csv("data/master_data.csv")
head(data)
```

    ##   adm_0_name adm_0_code          adm_1_name adm_1_code adm_2_name adm_2_code
    ## 1     Mexico        162      Aguascalientes       2031       <NA>         NA
    ## 2     Mexico        162     Baja California       2032       <NA>         NA
    ## 3     Mexico        162 Baja California Sur       2033       <NA>         NA
    ## 4     Mexico        162            Campeche       2034       <NA>         NA
    ## 5     Mexico        162            Coahuila       2035       <NA>         NA
    ## 6     Mexico        162              Colima       2036       <NA>         NA
    ##   calendar_start_date calendar_end_date dengue_total
    ## 1          2002-12-29        2003-01-04            0
    ## 2          2002-12-29        2003-01-04            0
    ## 3          2002-12-29        2003-01-04            0
    ## 4          2002-12-29        2003-01-04            0
    ## 5          2002-12-29        2003-01-04            0
    ## 6          2002-12-29        2003-01-04            0
    ##                                   UUID                              source_id
    ## 1 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ## 2 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ## 3 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ## 4 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ## 5 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ## 6 9e927e1a-c739-11ed-aa9a-1e00d12e8869 Mexico_2003_adm1_weekly_moh_EW1_source
    ##   source_cat
    ## 1        moh
    ## 2        moh
    ## 3        moh
    ## 4        moh
    ## 5        moh
    ## 6        moh

``` r
plyr::count(data$source_cat)
```

    ##              x    freq
    ## 1          moh 2471899
    ## 2    paho_adm0   13107
    ## 3 paho_imputed     262
    ## 4     paho_sub   36237
    ## 5        tycho    6929

``` r
data <- data %>%
  mutate(calendar_start_date = ymd(calendar_start_date), 
         calendar_end_date = ymd(calendar_end_date), 
         year = year(calendar_end_date), 
         diff = as.duration(interval(calendar_start_date, calendar_end_date))%/% as.duration(days(1)))%>%
  mutate(spatial_res = ifelse(adm_1_name %in% NA & adm_2_name %in% NA,  0, 
                       ifelse(adm_2_name %in% NA, 1, 2)))%>%
  mutate(temporal_res = ifelse(diff <8, 2, #weekly
                          ifelse(diff > 7 & diff < 31, 1, #monthly
                                                       0)))%>% #yearly
  #mutate(temporal_res = ifelse(grepl("all_PAHO", data$source_id, ignore.case = FALSE)== TRUE, "2", temporal_res))%>%
  select(adm_0_name:adm_2_code, spatial_res, temporal_res, year, diff,  calendar_start_date:source_cat)

plyr::count(data$diff)
```

    ##      x    freq
    ## 1    6 1930849
    ## 2   27   49337
    ## 3   28     166
    ## 4   29  194785
    ## 5   30  341538
    ## 6   83     208
    ## 7  334      88
    ## 8  335      44
    ## 9  363    1357
    ## 10 364     976
    ## 11 365     364
    ## 12 370     209
    ## 13  NA    8513

``` r
plyr::count(data$spatial_res)
```

    ##   x    freq
    ## 1 0   15073
    ## 2 1   63442
    ## 3 2 2449919

``` r
plyr::count(data$temporal_res)
```

    ##    x    freq
    ## 1  0    3246
    ## 2  1  585826
    ## 3  2 1930849
    ## 4 NA    8513

``` r
data %>% 
  select(temporal_res, spatial_res, source_cat)%>% 
  group_by(temporal_res, spatial_res, source_cat)%>% tally()
```

    ## # A tibble: 14 × 4
    ## # Groups:   temporal_res, spatial_res [9]
    ##    temporal_res spatial_res source_cat         n
    ##           <dbl>       <dbl> <chr>          <int>
    ##  1            0           0 tycho           1340
    ##  2            0           1 moh              455
    ##  3            0           2 paho_sub        1451
    ##  4            1           1 tycho           5283
    ##  5            1           2 moh           580543
    ##  6            2           0 moh              364
    ##  7            2           0 paho_adm0      13107
    ##  8            2           0 paho_imputed     262
    ##  9            2           1 moh            22895
    ## 10            2           1 paho_sub       34786
    ## 11            2           1 tycho             23
    ## 12            2           2 moh          1859129
    ## 13            2           2 tycho            283
    ## 14           NA           2 moh             8513

``` r
# **** need to check temporal resolution for some of paho subnational data ****
data <- data %>%
  filter(!diff == 83)
```

Convert calendar dates back to epi weeks, to help with tallying the
number of weeks at each temporal and spatial resolution.

``` r
# Overwrite EW as month of the calendar_end_date if the temporal resolution is monthly
data <- data %>%
  mutate(EW = lubridate::week(calendar_end_date))%>%
  mutate(EW = ifelse(temporal_res==1, month(calendar_end_date), 
                ifelse(temporal_res==0, NA, EW)))
```

``` r
plyr::count(data$adm_0_name)
```

    ##                                   x    freq
    ## 1                          Anguilla     151
    ## 2               Antigua and Barbuda     195
    ## 3               Antigua And Barbuda      41
    ## 4                         Argentina    7295
    ## 5                             Aruba     188
    ## 6                           Bahamas     165
    ## 7                          Barbados     483
    ## 8                            Belize     436
    ## 9                           Bermuda     362
    ## 10                          Bolivia    4525
    ## 11                           Brazil 2434000
    ## 12                           Canada     471
    ## 13                   Cayman Islands     172
    ## 14                            Chile     226
    ## 15                         Colombia    2652
    ## 16                       Costa Rica    1736
    ## 17                             Cuba     223
    ## 18                         Dominica     120
    ## 19               Dominican republic    8472
    ## 20               Dominican Republic     508
    ## 21                          Ecuador    4027
    ## 22                      El Salvador    2082
    ## 23                    French Guiana     162
    ## 24                          Grenada     133
    ## 25                       Guadeloupe     141
    ## 26                        Guatemala     578
    ## 27                           Guyana      71
    ## 28                            Haiti     176
    ## 29                         Honduras     486
    ## 30                          Jamaica     456
    ## 31                       Martinique     173
    ## 32                           Mexico   14010
    ## 33                       Montserrat     226
    ## 34                        Nicaragua   19826
    ## 35                           Panama    5121
    ## 36                         Paraguay     407
    ## 37                             Peru    4254
    ## 38                      Puerto Rico     485
    ## 39            Saint Kitts and Nevis     217
    ## 40            Saint Kitts And Nevis      43
    ## 41                      Saint Lucia     313
    ## 42                     Saint Martin       4
    ## 43 Saint Vincent and the Grenadines     172
    ## 44 Saint Vincent And The Grenadines      32
    ## 45                         Suriname     285
    ## 46              Trinidad and Tobago      79
    ## 47              Trinidad And Tobago      34
    ## 48         Turks And Caicos Islands      23
    ## 49         United States of America     464
    ## 50         United States Of America     317
    ## 51                          Uruguay     395
    ## 52                        Venezuela    2100

``` r
lookup <- c("Antigua And Barbuda" = "Antigua and Barbuda",
         "Dominican republic" = "Dominican Republic",
         "Saint Kitts And Nevis" = "Saint Kitts and Nevis",
         "Saint Vincent And the Grenadines" = "Saint Vincent and the Grenadines",
         "Saint Vincent And The Grenadines" = "Saint Vincent and the Grenadines",
         "Trinidad And Tobago" = "Trinidad and Tobago", 
         "Turks And Caicos Islands" = "Turks and Caicos Islands", 
         "United States Of America" = "United States of America" 
         )


data <- data %>%
  mutate(adm_0_name = recode(adm_0_name, !!!lookup))

#plyr::count(data$adm_0_name)

#write.csv(subset(data, adm_0_name %in% c("Bolivia")), "data/bolivia.csv", row.names=F)
```

# Summary statistics of data coverage

Produce summary statistics of temporal resolution by each year. temporal
resolution is grouped into weekly, monthly, annually.  
The maximum temporal resolution reached for that year is prioritised for
colour coding the heatmap. The opacity/gradient of the colour is
determined by the proportion of the year at which that temporal
resolution is available.

## Temporal resolution

``` r
# first check if any country-year has data for more than one temporal resolution
data %>%
  mutate(temporal_res_nm = ifelse(temporal_res==2, "Weekly", 
                         ifelse(temporal_res==1, "Monthly", "Yearly")))%>%
  group_by(adm_0_name, year, temporal_res_nm)%>%
  tally()%>%
  mutate(n2 = ifelse(is.na(n)==FALSE, 1, NA))%>% select(-n)%>% 
  spread(., temporal_res_nm, n2)%>%
  mutate(total = Weekly+Monthly+Yearly) %>%
  arrange(desc(total)) #Data for Nicaragua (2004-2005) include different temporal resolutions 
```

    ## # A tibble: 1,733 × 6
    ## # Groups:   adm_0_name, year [1,733]
    ##    adm_0_name  year Monthly Weekly Yearly total
    ##    <chr>      <int>   <dbl>  <dbl>  <dbl> <dbl>
    ##  1 Nicaragua   2004       1      1      1     3
    ##  2 Nicaragua   2005       1      1      1     3
    ##  3 Anguilla    1980      NA     NA      1    NA
    ##  4 Anguilla    1981      NA     NA      1    NA
    ##  5 Anguilla    1982      NA     NA      1    NA
    ##  6 Anguilla    1983      NA     NA      1    NA
    ##  7 Anguilla    1984      NA     NA      1    NA
    ##  8 Anguilla    1985      NA     NA      1    NA
    ##  9 Anguilla    1986      NA     NA      1    NA
    ## 10 Anguilla    1987      NA     NA      1    NA
    ## # … with 1,723 more rows

No color gradient applied to each cell. It is possible, for example, to
only have data for one month of a certain year available.

``` r
temp_boolean <- data %>%
  group_by(adm_0_name, year, temporal_res)%>%
  tally()%>%
  arrange(adm_0_name, year, desc(temporal_res))%>%  
  group_by(adm_0_name, year) %>% slice_head(n=1)%>% select(-n)%>% ungroup()%>%
  complete(adm_0_name, year) %>%
  mutate(temporal_res_nm = ifelse(temporal_res==2, "Weekly", 
                            ifelse(temporal_res==1, "Monthly", "Yearly")))%>%
  mutate(temporal_res_nm = factor(temporal_res_nm, levels=c("Weekly", "Monthly", "Yearly")))
```

``` r
temp_boolean %>%
  filter(!year < 1990)%>%
  mutate(year = as.character(year))%>%
ggplot( aes(x=year, y=adm_0_name, group=temporal_res_nm))+
  geom_tile(aes(fill=temporal_res_nm), 
            color = "white",  lwd = 0.25, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   breaks=seq(1990, 2020, by=5)
                     )+
  scale_fill_manual(name = "Temporal resolution", 
                    values = c("#1F78B4","#B2DF8A","#FB9A99"), na.value="#D9D9D9")+
  coord_fixed()+
  theme_bw()+
  ggtitle("Best temporal resolution available")+
  xlab("Year")+ylab("Country")+
  theme(plot.title = element_text(size=28), 
        axis.title.x = element_text(size=28, vjust=0),
        axis.title.y = element_text(size=28), 
        axis.text = element_text(size=18), 
        axis.ticks = element_line(size=1),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18), 
        plot.margin = margin(t=-50, b=-50, l=20, r=20), 
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Spatial resolution

``` r
# first check if any country-year has data for more than one spatial resolution
data %>%
  mutate(spatial_res_nm = ifelse(spatial_res==2, "adm2", 
                         ifelse(spatial_res==1, "adm1", "adm0")))%>%
  group_by(adm_0_name, year, spatial_res_nm)%>%
  tally()%>%
  mutate(n2 = ifelse(is.na(n)==FALSE, 1, NA))%>% select(-n)%>% 
  spread(., spatial_res_nm, n2)%>%
  mutate(total = adm2+adm1+adm0) %>%
  arrange(desc(total)) #Data for Brazil (2001-2005) include different spatial resolutions 
```

    ## # A tibble: 1,733 × 6
    ## # Groups:   adm_0_name, year [1,733]
    ##    adm_0_name  year  adm0  adm1  adm2 total
    ##    <chr>      <int> <dbl> <dbl> <dbl> <dbl>
    ##  1 Brazil      2001     1     1     1     3
    ##  2 Brazil      2002     1     1     1     3
    ##  3 Brazil      2003     1     1     1     3
    ##  4 Brazil      2004     1     1     1     3
    ##  5 Brazil      2005     1     1     1     3
    ##  6 Anguilla    1980     1    NA    NA    NA
    ##  7 Anguilla    1981     1    NA    NA    NA
    ##  8 Anguilla    1982     1    NA    NA    NA
    ##  9 Anguilla    1983     1    NA    NA    NA
    ## 10 Anguilla    1984     1    NA    NA    NA
    ## # … with 1,723 more rows

No color gradient applied to each cell. It is possible, for example, to
only have data for one adm2 area of a certain country available.

``` r
spat_boolean <- data %>%
  group_by(adm_0_name, year, spatial_res)%>%
  tally()%>%
  arrange(adm_0_name, year, desc(spatial_res))%>%  
  group_by(adm_0_name, year) %>% slice_head(n=1)%>% select(-n)%>% ungroup()%>%
  complete(adm_0_name, year) %>%
  mutate(spatial_res_nm = ifelse(spatial_res==2, "adm2", 
                         ifelse(spatial_res==1, "adm1", "adm0")))%>%
  mutate(spatial_res_nm = factor(spatial_res_nm, levels=c("adm2", "adm1", "adm0")))
```

``` r
spat_boolean %>%
  filter(!year < 1990)%>%
  mutate(year = as.character(year))%>%
ggplot( aes(x=year, y=adm_0_name, group=spatial_res_nm))+
  geom_tile(aes(fill=spatial_res_nm), 
            color = "white",  lwd = 0.25, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(  breaks = seq(1990,2020, by=5), 
                     expand=c(0,0))+
  scale_fill_manual(name = "Spatial resolution", 
                    values = c("#1F78B4","#B2DF8A","#FB9A99"), na.value="#D9D9D9")+
  coord_fixed()+
  theme_bw()+
  ggtitle("Best spatial resolution available")+
  xlab("Year")+ylab("Country")+
  theme(plot.title = element_text(size=28), 
        axis.title.x = element_text(size=28, vjust=0),
        axis.title.y = element_text(size=28), 
        axis.text = element_text(size=18), 
        axis.ticks = element_line(size=1),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18), 
        plot.margin = margin(t=-50, b=-50, l=20, r=20), 
        plot.background=element_blank(),
        panel.border=element_blank())
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Data source

``` r
# first check if any country-year has data for more than one data source
data %>%
  group_by(adm_0_name, year, source_cat)%>%
  tally()%>%
  mutate(n2 = ifelse(is.na(n)==FALSE, 1, NA))%>% select(-n)%>% 
  spread(., source_cat, n2)%>%
  mutate(paho_tot = ifelse(sum(c_across(paho_adm0:paho_imputed), na.rm=T)>0, 1, NA))%>% 
  select(-paho_adm0, -paho_imputed)%>%
  mutate(total = sum(c_across(moh:paho_tot), na.rm = T)) %>%
  arrange(desc(total)) 
```

    ## # A tibble: 1,733 × 7
    ## # Groups:   adm_0_name, year [1,733]
    ##    adm_0_name  year   moh paho_sub tycho paho_tot total
    ##    <chr>      <int> <dbl>    <dbl> <dbl>    <dbl> <dbl>
    ##  1 Argentina   2019     1       NA    NA        1     2
    ##  2 Argentina   2020     1       NA    NA        1     2
    ##  3 Argentina   2021     1       NA    NA        1     2
    ##  4 Bolivia     2004     1       NA     1       NA     2
    ##  5 Bolivia     2005     1       NA     1       NA     2
    ##  6 Bolivia     2006     1       NA     1       NA     2
    ##  7 Bolivia     2007     1       NA     1       NA     2
    ##  8 Bolivia     2008     1       NA     1       NA     2
    ##  9 Bolivia     2009     1       NA     1       NA     2
    ## 10 Bolivia     2014    NA        1    NA        1     2
    ## # … with 1,723 more rows

``` r
source_boolean <- data %>%
  mutate(source_cat = ifelse(source_cat %in% c("paho_adm0", "paho_imputed"), "paho", source_cat))%>%
  group_by(adm_0_name, year, source_cat)%>%
  tally()%>%
  arrange(adm_0_name, year, desc(n))%>%  
  group_by(adm_0_name, year) %>% slice_head(n=1)%>%  ungroup()%>%
  complete(adm_0_name, year) 
plyr::count(source_boolean$source_cat)
```

    ##          x freq
    ## 1      moh   70
    ## 2     paho  344
    ## 3 paho_sub   39
    ## 4    tycho 1280
    ## 5     <NA> 1211

In the case of multiple data sources available for the same
country-year, the data source with the most data rows is chosen and
shown on the figure below.

``` r
source_boolean %>%
  filter(!year < 1990)%>%
  mutate(year = as.character(year))%>%
ggplot( aes(x=year, y=adm_0_name, group=source_cat))+
  geom_tile(aes(fill=source_cat), 
            color = "white",  lwd = 0.25, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(  breaks = seq(1990,2020, by=5), 
                     expand=c(0,0))+
  scale_fill_manual(name = "Data source", 
                    values = c("#1F78B4","#B2DF8A","#4c841c", "#FB9A99"), na.value="#D9D9D9")+
  coord_fixed()+
  theme_bw()+
  ggtitle("Data source available")+
  xlab("Year")+ylab("Country")+
  theme(plot.title = element_text(size=28), 
        axis.title.x = element_text(size=28, vjust=0),
        axis.title.y = element_text(size=28), 
        axis.text = element_text(size=18), 
        axis.ticks = element_line(size=1),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18), 
        plot.margin = margin(t=-50, b=-50, l=20, r=20), 
        plot.background=element_blank(),
        panel.border=element_blank())
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
Temporal/spatial resolution per data source? This is done by comparing
the number of country-year data points between different sources. This
allows us to identify which data source dominates each temporal
resolution.

``` r
# first check if any country-year has data for more than one data source
source_temp <- data %>%
  mutate(source_cat = ifelse(source_cat %in% c("paho_adm0", "paho_imputed"), "paho", source_cat))%>%
  group_by(temporal_res, source_cat, adm_0_name, year)%>%
  tally() %>% mutate(n = 1)

source_temp2 <- source_temp %>%
  group_by(temporal_res, source_cat) %>% tally() %>%
  mutate(prop = round(prop.table(n)*100, 1))%>% ungroup()%>%
  complete(temporal_res, source_cat)%>%
  mutate(source_cat_nm = ifelse(source_cat %in% c("moh"), "MoH", 
                      ifelse(source_cat %in% c("paho"), "PAHO", 
                      ifelse(source_cat %in% c("paho_sub"), "PAHO\nsubnational", 
                             "Project\nTycho"))))%>%
  mutate(source_cat_nm = factor(source_cat_nm, levels=c("MoH", "PAHO", "PAHO\nsubnational", "Project\nTycho")))%>%
  mutate(temporal_res_nm = ifelse(temporal_res==2, "Weekly", 
                            ifelse(temporal_res==1, "Monthly", "Yearly")))%>%
  mutate(temporal_res_nm = factor(temporal_res_nm, levels=c("Weekly", "Monthly", "Yearly")))%>%
  mutate(text = ifelse(is.na(n), "0", "1"))%>%
  mutate(labels = ifelse(text %in% c("1"), paste0(sprintf("%.1f", prop), "%") , ""))
```

Summed horizontally.

``` r
source_temp2 %>%
  ggplot(aes(x=source_cat_nm, y=temporal_res_nm, fill=prop))+
  geom_tile(color = "white",  lwd = 0.5, linetype = 1)+
  geom_text(aes(x = source_cat_nm, y = temporal_res_nm, label = labels), color = "black", size= 4)+
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value="#D9D9D9", name="Proportion (%)")+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(position = "top", expand=c(0,0))+
  xlab("Data source")+ylab("Temporal resolution")+
  theme(axis.title.x.top = element_text(size=15, face = "bold", margin = margin(b = 0.1, unit = "in")))+
  theme(axis.title.y = element_text(size=15, face = "bold", vjust = 4),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.ticks.x = element_blank(),        
        axis.ticks.y = element_blank(), 
        axis.line=element_blank(),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm")
  )+
  theme(legend.title = element_text(size=10),
        legend.text = element_text(size=8, vjust=1),
        legend.margin = margin(9,9,9,9), 
        legend.text.align = 0
  )
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Weekly - PAHO; Monthly - TYCHO; Yearly - TYCHO
```

``` r
# first check if any country-year has data for more than one data source
source_spat <- data %>%
  mutate(source_cat = ifelse(source_cat %in% c("paho_adm0", "paho_imputed"), "paho", source_cat))%>%
  group_by(spatial_res, source_cat, adm_0_name, year)%>%
  tally() %>% mutate(n = 1)

source_spat2 <- source_spat %>%
  group_by(spatial_res, source_cat) %>% tally() %>%
  mutate(prop = round(prop.table(n)*100, 1))%>% ungroup()%>%
  complete(spatial_res, source_cat)%>%
  mutate(source_cat_nm = ifelse(source_cat %in% c("moh"), "MoH", 
                      ifelse(source_cat %in% c("paho"), "PAHO", 
                      ifelse(source_cat %in% c("paho_sub"), "PAHO\nsubnational", 
                             "Project\nTycho"))))%>%
  mutate(source_cat_nm = factor(source_cat_nm, levels=c("MoH", "PAHO", "PAHO\nsubnational", "Project\nTycho")))%>%
  mutate(spatial_res_nm = ifelse(spatial_res==2, "Admin 2", 
                            ifelse(spatial_res==1, "Admin 1", "Admin 0")))%>%
  mutate(spatial_res_nm = factor(spatial_res_nm, levels=c("Admin 2", "Admin 1", "Admin 0")))%>%
  mutate(text = ifelse(is.na(n), "0", "1"))%>%
  mutate(labels = ifelse(text %in% c("1"), paste0(sprintf("%.1f", prop), "%") , ""))
```

``` r
source_spat2 %>%
  ggplot(aes(x=source_cat_nm, y=spatial_res_nm, fill=prop))+
  geom_tile(color = "white",  lwd = 0.5, linetype = 1)+
  geom_text(aes(x = source_cat_nm, y = spatial_res_nm, label = labels), color = "black", size= 4)+
  scale_fill_distiller(palette = "OrRd", direction = 1, na.value="#D9D9D9", name="Proportion (%)")+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(position = "top", expand=c(0,0))+
  xlab("Data source")+ylab("Spatial resolution")+
  theme(axis.title.x.top = element_text(size=15, face = "bold", margin = margin(b = 0.1, unit = "in")))+
  theme(axis.title.y = element_text(size=15, face = "bold", vjust = 4),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.ticks.x = element_blank(),        
        axis.ticks.y = element_blank(), 
        axis.line=element_blank(),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm")
  )+
  theme(legend.title = element_text(size=10),
        legend.text = element_text(size=8, vjust=1),
        legend.margin = margin(9,9,9,9), 
        legend.text.align = 0
  )
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# Adm2 - MoH; Adm1 - equally distributed; Adm0 - Tycho. adm2 and adm1 code matching needs to be done to ensure more accurate data coverage though...
```

# Work on progress: heatmap with color gradient

Produce summary statistics of spatial resolution by each year. Spatial
resolution is grouped into admin 0, 1 & 2.. The maximum spatial
resolution reached for that year is prioritised for colour coding the
heatmap. The gradient of colour that applies to temporal resolution does
not yet apply to spatial resolution. This is because while we plan to
gradient it according to the proportion of the country that is available
at that that resolution, we do not yet have all of the admin2 code
kmatching complete in order to do so.

# Visualisations

Produce heatmaps for country by temporal resolution and then spatial
resoluton. note that this will only be a single row for this country on
it’s own, though when all country datasets are processed a fuller map
will be developed.

## Data coverage by temporal resolution

## Data coverage by spatial resolution

## Data coverage by spatial resolution

``` r
#ggplot(data_spatial, aes(x=year, y=country))+
#  geom_tile(aes(fill=spatial_res), color = "white",  lwd = 1.5, linetype = 1)+
#  scale_fill_manual(values = c("#1F78B4","#B2DF8A","#FB9A99"))
```