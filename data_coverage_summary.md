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

plyr::count(data$adm_0_name)
```

    ##                                   x    freq
    ## 1                          Anguilla     151
    ## 2               Antigua and Barbuda     236
    ## 3                         Argentina    7295
    ## 4                             Aruba     188
    ## 5                           Bahamas     165
    ## 6                          Barbados     483
    ## 7                            Belize     436
    ## 8                           Bermuda     362
    ## 9                           Bolivia    4525
    ## 10                           Brazil 2434000
    ## 11                           Canada     471
    ## 12                   Cayman Islands     172
    ## 13                            Chile     226
    ## 14                         Colombia    2652
    ## 15                       Costa Rica    1736
    ## 16                             Cuba     223
    ## 17                         Dominica     120
    ## 18               Dominican Republic    8980
    ## 19                          Ecuador    4027
    ## 20                      El Salvador    2082
    ## 21                    French Guiana     162
    ## 22                          Grenada     133
    ## 23                       Guadeloupe     141
    ## 24                        Guatemala     578
    ## 25                           Guyana      71
    ## 26                            Haiti     176
    ## 27                         Honduras     486
    ## 28                          Jamaica     456
    ## 29                       Martinique     173
    ## 30                           Mexico   14010
    ## 31                       Montserrat     226
    ## 32                        Nicaragua   19826
    ## 33                           Panama    5121
    ## 34                         Paraguay     407
    ## 35                             Peru    4254
    ## 36                      Puerto Rico     485
    ## 37            Saint Kitts and Nevis     260
    ## 38                      Saint Lucia     313
    ## 39                     Saint Martin       4
    ## 40 Saint Vincent and the Grenadines     204
    ## 41                         Suriname     285
    ## 42              Trinidad and Tobago     113
    ## 43         Turks and Caicos Islands      23
    ## 44         United States of America     781
    ## 45                          Uruguay     395
    ## 46                        Venezuela    2100

``` r
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
\## Spatial resolution

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

  
str(source_temp)  
```

    ## gropd_df [1,893 × 5] (S3: grouped_df/tbl_df/tbl/data.frame)
    ##  $ temporal_res: num [1:1893] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ source_cat  : chr [1:1893] "moh" "moh" "moh" "moh" ...
    ##  $ adm_0_name  : chr [1:1893] "Colombia" "Colombia" "Colombia" "Colombia" ...
    ##  $ year        : int [1:1893] 2003 2005 2006 2007 2008 2011 2012 2007 2008 2009 ...
    ##  $ n           : num [1:1893] 1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, "groups")= tibble [116 × 4] (S3: tbl_df/tbl/data.frame)
    ##   ..$ temporal_res: num [1:116] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..$ source_cat  : chr [1:116] "moh" "moh" "paho_sub" "tycho" ...
    ##   ..$ adm_0_name  : chr [1:116] "Colombia" "Guatemala" "Costa Rica" "Anguilla" ...
    ##   ..$ .rows       : list<int> [1:116] 
    ##   .. ..$ : int [1:7] 1 2 3 4 5 6 7
    ##   .. ..$ : int [1:6] 8 9 10 11 12 13
    ##   .. ..$ : int [1:5] 14 15 16 17 18
    ##   .. ..$ : int [1:31] 19 20 21 22 23 24 25 26 27 28 ...
    ##   .. ..$ : int [1:41] 50 51 52 53 54 55 56 57 58 59 ...
    ##   .. ..$ : int [1:15] 91 92 93 94 95 96 97 98 99 100 ...
    ##   .. ..$ : int [1:15] 106 107 108 109 110 111 112 113 114 115 ...
    ##   .. ..$ : int [1:42] 121 122 123 124 125 126 127 128 129 130 ...
    ##   .. ..$ : int [1:34] 163 164 165 166 167 168 169 170 171 172 ...
    ##   .. ..$ : int [1:32] 197 198 199 200 201 202 203 204 205 206 ...
    ##   .. ..$ : int [1:18] 229 230 231 232 233 234 235 236 237 238 ...
    ##   .. ..$ : int [1:33] 247 248 249 250 251 252 253 254 255 256 ...
    ##   .. ..$ : int [1:33] 280 281 282 283 284 285 286 287 288 289 ...
    ##   .. ..$ : int [1:2] 313 314
    ##   .. ..$ : int [1:18] 315 316 317 318 319 320 321 322 323 324 ...
    ##   .. ..$ : int [1:11] 333 334 335 336 337 338 339 340 341 342 ...
    ##   .. ..$ : int [1:44] 344 345 346 347 348 349 350 351 352 353 ...
    ##   .. ..$ : int [1:24] 388 389 390 391 392 393 394 395 396 397 ...
    ##   .. ..$ : int [1:31] 412 413 414 415 416 417 418 419 420 421 ...
    ##   .. ..$ : int [1:41] 443 444 445 446 447 448 449 450 451 452 ...
    ##   .. ..$ : int [1:43] 484 485 486 487 488 489 490 491 492 493 ...
    ##   .. ..$ : int [1:25] 527 528 529 530 531 532 533 534 535 536 ...
    ##   .. ..$ : int [1:34] 552 553 554 555 556 557 558 559 560 561 ...
    ##   .. ..$ : int [1:27] 586 587 588 589 590 591 592 593 594 595 ...
    ##   .. ..$ : int [1:37] 613 614 615 616 617 618 619 620 621 622 ...
    ##   .. ..$ : int [1:31] 650 651 652 653 654 655 656 657 658 659 ...
    ##   .. ..$ : int [1:31] 681 682 683 684 685 686 687 688 689 690 ...
    ##   .. ..$ : int [1:34] 712 713 714 715 716 717 718 719 720 721 ...
    ##   .. ..$ : int [1:21] 746 747 748 749 750 751 752 753 754 755 ...
    ##   .. ..$ : int [1:34] 767 768 769 770 771 772 773 774 775 776 ...
    ##   .. ..$ : int [1:49] 801 802 803 804 805 806 807 808 809 810 ...
    ##   .. ..$ : int [1:30] 850 851 852 853 854 855 856 857 858 859 ...
    ##   .. ..$ : int [1:34] 880 881 882 883 884 885 886 887 888 889 ...
    ##   .. ..$ : int [1:23] 914 915 916 917 918 919 920 921 922 923 ...
    ##   .. ..$ : int [1:28] 937 938 939 940 941 942 943 944 945 946 ...
    ##   .. ..$ : int [1:25] 965 966 967 968 969 970 971 972 973 974 ...
    ##   .. ..$ : int [1:23] 990 991 992 993 994 995 996 997 998 999 ...
    ##   .. ..$ : int [1:25] 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 ...
    ##   .. ..$ : int [1:49] 1038 1039 1040 1041 1042 1043 1044 1045 1046 1047 ...
    ##   .. ..$ : int [1:43] 1087 1088 1089 1090 1091 1092 1093 1094 1095 1096 ...
    ##   .. ..$ : int [1:34] 1130 1131 1132 1133 1134 1135 1136 1137 1138 1139 ...
    ##   .. ..$ : int [1:4] 1164 1165 1166 1167
    ##   .. ..$ : int [1:32] 1168 1169 1170 1171 1172 1173 1174 1175 1176 1177 ...
    ##   .. ..$ : int [1:34] 1200 1201 1202 1203 1204 1205 1206 1207 1208 1209 ...
    ##   .. ..$ : int [1:34] 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 ...
    ##   .. ..$ : int [1:23] 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 ...
    ##   .. ..$ : int [1:11] 1291 1292 1293 1294 1295 1296 1297 1298 1299 1300 ...
    ##   .. ..$ : int [1:10] 1302 1303 1304 1305 1306 1307 1308 1309 1310 1311
    ##   .. ..$ : int [1:47] 1312 1313 1314 1315 1316 1317 1318 1319 1320 1321 ...
    ##   .. ..$ : int [1:13] 1359 1360 1361 1362 1363 1364 1365 1366 1367 1368 ...
    ##   .. ..$ : int [1:3] 1372 1373 1374
    ##   .. ..$ : int [1:13] 1375 1376 1377 1378 1379 1380 1381 1382 1383 1384 ...
    ##   .. ..$ : int [1:2] 1388 1389
    ##   .. ..$ : int [1:10] 1390 1391 1392 1393 1394 1395 1396 1397 1398 1399
    ##   .. ..$ : int [1:6] 1400 1401 1402 1403 1404 1405
    ##   .. ..$ : int [1:2] 1406 1407
    ##   .. ..$ : int [1:7] 1408 1409 1410 1411 1412 1413 1414
    ##   .. ..$ : int [1:3] 1415 1416 1417
    ##   .. ..$ : int [1:6] 1418 1419 1420 1421 1422 1423
    ##   .. ..$ : int [1:9] 1424 1425 1426 1427 1428 1429 1430 1431 1432
    ##   .. ..$ : int [1:9] 1433 1434 1435 1436 1437 1438 1439 1440 1441
    ##   .. ..$ : int [1:7] 1442 1443 1444 1445 1446 1447 1448
    ##   .. ..$ : int 1449
    ##   .. ..$ : int [1:6] 1450 1451 1452 1453 1454 1455
    ##   .. ..$ : int [1:3] 1456 1457 1458
    ##   .. ..$ : int [1:5] 1459 1460 1461 1462 1463
    ##   .. ..$ : int [1:7] 1464 1465 1466 1467 1468 1469 1470
    ##   .. ..$ : int [1:9] 1471 1472 1473 1474 1475 1476 1477 1478 1479
    ##   .. ..$ : int [1:9] 1480 1481 1482 1483 1484 1485 1486 1487 1488
    ##   .. ..$ : int [1:8] 1489 1490 1491 1492 1493 1494 1495 1496
    ##   .. ..$ : int [1:7] 1497 1498 1499 1500 1501 1502 1503
    ##   .. ..$ : int [1:9] 1504 1505 1506 1507 1508 1509 1510 1511 1512
    ##   .. ..$ : int [1:9] 1513 1514 1515 1516 1517 1518 1519 1520 1521
    ##   .. ..$ : int [1:9] 1522 1523 1524 1525 1526 1527 1528 1529 1530
    ##   .. ..$ : int [1:9] 1531 1532 1533 1534 1535 1536 1537 1538 1539
    ##   .. ..$ : int [1:9] 1540 1541 1542 1543 1544 1545 1546 1547 1548
    ##   .. ..$ : int [1:9] 1549 1550 1551 1552 1553 1554 1555 1556 1557
    ##   .. ..$ : int [1:9] 1558 1559 1560 1561 1562 1563 1564 1565 1566
    ##   .. ..$ : int [1:8] 1567 1568 1569 1570 1571 1572 1573 1574
    ##   .. ..$ : int [1:9] 1575 1576 1577 1578 1579 1580 1581 1582 1583
    ##   .. ..$ : int [1:9] 1584 1585 1586 1587 1588 1589 1590 1591 1592
    ##   .. ..$ : int [1:7] 1593 1594 1595 1596 1597 1598 1599
    ##   .. ..$ : int [1:9] 1600 1601 1602 1603 1604 1605 1606 1607 1608
    ##   .. ..$ : int [1:9] 1609 1610 1611 1612 1613 1614 1615 1616 1617
    ##   .. ..$ : int [1:9] 1618 1619 1620 1621 1622 1623 1624 1625 1626
    ##   .. ..$ : int [1:9] 1627 1628 1629 1630 1631 1632 1633 1634 1635
    ##   .. ..$ : int [1:8] 1636 1637 1638 1639 1640 1641 1642 1643
    ##   .. ..$ : int [1:9] 1644 1645 1646 1647 1648 1649 1650 1651 1652
    ##   .. ..$ : int [1:9] 1653 1654 1655 1656 1657 1658 1659 1660 1661
    ##   .. ..$ : int [1:9] 1662 1663 1664 1665 1666 1667 1668 1669 1670
    ##   .. ..$ : int [1:9] 1671 1672 1673 1674 1675 1676 1677 1678 1679
    ##   .. ..$ : int [1:6] 1680 1681 1682 1683 1684 1685
    ##   .. ..$ : int [1:9] 1686 1687 1688 1689 1690 1691 1692 1693 1694
    ##   .. ..$ : int [1:9] 1695 1696 1697 1698 1699 1700 1701 1702 1703
    ##   .. ..$ : int [1:9] 1704 1705 1706 1707 1708 1709 1710 1711 1712
    ##   .. ..$ : int [1:9] 1713 1714 1715 1716 1717 1718 1719 1720 1721
    ##   .. ..$ : int [1:9] 1722 1723 1724 1725 1726 1727 1728 1729 1730
    ##   .. ..$ : int [1:9] 1731 1732 1733 1734 1735 1736 1737 1738 1739
    ##   .. ..$ : int [1:9] 1740 1741 1742 1743 1744 1745 1746 1747 1748
    ##   .. .. [list output truncated]
    ##   .. ..@ ptype: int(0) 
    ##   ..- attr(*, ".drop")= logi TRUE

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

  
str(source_temp)  
```

    ## gropd_df [1,893 × 5] (S3: grouped_df/tbl_df/tbl/data.frame)
    ##  $ temporal_res: num [1:1893] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ source_cat  : chr [1:1893] "moh" "moh" "moh" "moh" ...
    ##  $ adm_0_name  : chr [1:1893] "Colombia" "Colombia" "Colombia" "Colombia" ...
    ##  $ year        : int [1:1893] 2003 2005 2006 2007 2008 2011 2012 2007 2008 2009 ...
    ##  $ n           : num [1:1893] 1 1 1 1 1 1 1 1 1 1 ...
    ##  - attr(*, "groups")= tibble [116 × 4] (S3: tbl_df/tbl/data.frame)
    ##   ..$ temporal_res: num [1:116] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..$ source_cat  : chr [1:116] "moh" "moh" "paho_sub" "tycho" ...
    ##   ..$ adm_0_name  : chr [1:116] "Colombia" "Guatemala" "Costa Rica" "Anguilla" ...
    ##   ..$ .rows       : list<int> [1:116] 
    ##   .. ..$ : int [1:7] 1 2 3 4 5 6 7
    ##   .. ..$ : int [1:6] 8 9 10 11 12 13
    ##   .. ..$ : int [1:5] 14 15 16 17 18
    ##   .. ..$ : int [1:31] 19 20 21 22 23 24 25 26 27 28 ...
    ##   .. ..$ : int [1:41] 50 51 52 53 54 55 56 57 58 59 ...
    ##   .. ..$ : int [1:15] 91 92 93 94 95 96 97 98 99 100 ...
    ##   .. ..$ : int [1:15] 106 107 108 109 110 111 112 113 114 115 ...
    ##   .. ..$ : int [1:42] 121 122 123 124 125 126 127 128 129 130 ...
    ##   .. ..$ : int [1:34] 163 164 165 166 167 168 169 170 171 172 ...
    ##   .. ..$ : int [1:32] 197 198 199 200 201 202 203 204 205 206 ...
    ##   .. ..$ : int [1:18] 229 230 231 232 233 234 235 236 237 238 ...
    ##   .. ..$ : int [1:33] 247 248 249 250 251 252 253 254 255 256 ...
    ##   .. ..$ : int [1:33] 280 281 282 283 284 285 286 287 288 289 ...
    ##   .. ..$ : int [1:2] 313 314
    ##   .. ..$ : int [1:18] 315 316 317 318 319 320 321 322 323 324 ...
    ##   .. ..$ : int [1:11] 333 334 335 336 337 338 339 340 341 342 ...
    ##   .. ..$ : int [1:44] 344 345 346 347 348 349 350 351 352 353 ...
    ##   .. ..$ : int [1:24] 388 389 390 391 392 393 394 395 396 397 ...
    ##   .. ..$ : int [1:31] 412 413 414 415 416 417 418 419 420 421 ...
    ##   .. ..$ : int [1:41] 443 444 445 446 447 448 449 450 451 452 ...
    ##   .. ..$ : int [1:43] 484 485 486 487 488 489 490 491 492 493 ...
    ##   .. ..$ : int [1:25] 527 528 529 530 531 532 533 534 535 536 ...
    ##   .. ..$ : int [1:34] 552 553 554 555 556 557 558 559 560 561 ...
    ##   .. ..$ : int [1:27] 586 587 588 589 590 591 592 593 594 595 ...
    ##   .. ..$ : int [1:37] 613 614 615 616 617 618 619 620 621 622 ...
    ##   .. ..$ : int [1:31] 650 651 652 653 654 655 656 657 658 659 ...
    ##   .. ..$ : int [1:31] 681 682 683 684 685 686 687 688 689 690 ...
    ##   .. ..$ : int [1:34] 712 713 714 715 716 717 718 719 720 721 ...
    ##   .. ..$ : int [1:21] 746 747 748 749 750 751 752 753 754 755 ...
    ##   .. ..$ : int [1:34] 767 768 769 770 771 772 773 774 775 776 ...
    ##   .. ..$ : int [1:49] 801 802 803 804 805 806 807 808 809 810 ...
    ##   .. ..$ : int [1:30] 850 851 852 853 854 855 856 857 858 859 ...
    ##   .. ..$ : int [1:34] 880 881 882 883 884 885 886 887 888 889 ...
    ##   .. ..$ : int [1:23] 914 915 916 917 918 919 920 921 922 923 ...
    ##   .. ..$ : int [1:28] 937 938 939 940 941 942 943 944 945 946 ...
    ##   .. ..$ : int [1:25] 965 966 967 968 969 970 971 972 973 974 ...
    ##   .. ..$ : int [1:23] 990 991 992 993 994 995 996 997 998 999 ...
    ##   .. ..$ : int [1:25] 1013 1014 1015 1016 1017 1018 1019 1020 1021 1022 ...
    ##   .. ..$ : int [1:49] 1038 1039 1040 1041 1042 1043 1044 1045 1046 1047 ...
    ##   .. ..$ : int [1:43] 1087 1088 1089 1090 1091 1092 1093 1094 1095 1096 ...
    ##   .. ..$ : int [1:34] 1130 1131 1132 1133 1134 1135 1136 1137 1138 1139 ...
    ##   .. ..$ : int [1:4] 1164 1165 1166 1167
    ##   .. ..$ : int [1:32] 1168 1169 1170 1171 1172 1173 1174 1175 1176 1177 ...
    ##   .. ..$ : int [1:34] 1200 1201 1202 1203 1204 1205 1206 1207 1208 1209 ...
    ##   .. ..$ : int [1:34] 1234 1235 1236 1237 1238 1239 1240 1241 1242 1243 ...
    ##   .. ..$ : int [1:23] 1268 1269 1270 1271 1272 1273 1274 1275 1276 1277 ...
    ##   .. ..$ : int [1:11] 1291 1292 1293 1294 1295 1296 1297 1298 1299 1300 ...
    ##   .. ..$ : int [1:10] 1302 1303 1304 1305 1306 1307 1308 1309 1310 1311
    ##   .. ..$ : int [1:47] 1312 1313 1314 1315 1316 1317 1318 1319 1320 1321 ...
    ##   .. ..$ : int [1:13] 1359 1360 1361 1362 1363 1364 1365 1366 1367 1368 ...
    ##   .. ..$ : int [1:3] 1372 1373 1374
    ##   .. ..$ : int [1:13] 1375 1376 1377 1378 1379 1380 1381 1382 1383 1384 ...
    ##   .. ..$ : int [1:2] 1388 1389
    ##   .. ..$ : int [1:10] 1390 1391 1392 1393 1394 1395 1396 1397 1398 1399
    ##   .. ..$ : int [1:6] 1400 1401 1402 1403 1404 1405
    ##   .. ..$ : int [1:2] 1406 1407
    ##   .. ..$ : int [1:7] 1408 1409 1410 1411 1412 1413 1414
    ##   .. ..$ : int [1:3] 1415 1416 1417
    ##   .. ..$ : int [1:6] 1418 1419 1420 1421 1422 1423
    ##   .. ..$ : int [1:9] 1424 1425 1426 1427 1428 1429 1430 1431 1432
    ##   .. ..$ : int [1:9] 1433 1434 1435 1436 1437 1438 1439 1440 1441
    ##   .. ..$ : int [1:7] 1442 1443 1444 1445 1446 1447 1448
    ##   .. ..$ : int 1449
    ##   .. ..$ : int [1:6] 1450 1451 1452 1453 1454 1455
    ##   .. ..$ : int [1:3] 1456 1457 1458
    ##   .. ..$ : int [1:5] 1459 1460 1461 1462 1463
    ##   .. ..$ : int [1:7] 1464 1465 1466 1467 1468 1469 1470
    ##   .. ..$ : int [1:9] 1471 1472 1473 1474 1475 1476 1477 1478 1479
    ##   .. ..$ : int [1:9] 1480 1481 1482 1483 1484 1485 1486 1487 1488
    ##   .. ..$ : int [1:8] 1489 1490 1491 1492 1493 1494 1495 1496
    ##   .. ..$ : int [1:7] 1497 1498 1499 1500 1501 1502 1503
    ##   .. ..$ : int [1:9] 1504 1505 1506 1507 1508 1509 1510 1511 1512
    ##   .. ..$ : int [1:9] 1513 1514 1515 1516 1517 1518 1519 1520 1521
    ##   .. ..$ : int [1:9] 1522 1523 1524 1525 1526 1527 1528 1529 1530
    ##   .. ..$ : int [1:9] 1531 1532 1533 1534 1535 1536 1537 1538 1539
    ##   .. ..$ : int [1:9] 1540 1541 1542 1543 1544 1545 1546 1547 1548
    ##   .. ..$ : int [1:9] 1549 1550 1551 1552 1553 1554 1555 1556 1557
    ##   .. ..$ : int [1:9] 1558 1559 1560 1561 1562 1563 1564 1565 1566
    ##   .. ..$ : int [1:8] 1567 1568 1569 1570 1571 1572 1573 1574
    ##   .. ..$ : int [1:9] 1575 1576 1577 1578 1579 1580 1581 1582 1583
    ##   .. ..$ : int [1:9] 1584 1585 1586 1587 1588 1589 1590 1591 1592
    ##   .. ..$ : int [1:7] 1593 1594 1595 1596 1597 1598 1599
    ##   .. ..$ : int [1:9] 1600 1601 1602 1603 1604 1605 1606 1607 1608
    ##   .. ..$ : int [1:9] 1609 1610 1611 1612 1613 1614 1615 1616 1617
    ##   .. ..$ : int [1:9] 1618 1619 1620 1621 1622 1623 1624 1625 1626
    ##   .. ..$ : int [1:9] 1627 1628 1629 1630 1631 1632 1633 1634 1635
    ##   .. ..$ : int [1:8] 1636 1637 1638 1639 1640 1641 1642 1643
    ##   .. ..$ : int [1:9] 1644 1645 1646 1647 1648 1649 1650 1651 1652
    ##   .. ..$ : int [1:9] 1653 1654 1655 1656 1657 1658 1659 1660 1661
    ##   .. ..$ : int [1:9] 1662 1663 1664 1665 1666 1667 1668 1669 1670
    ##   .. ..$ : int [1:9] 1671 1672 1673 1674 1675 1676 1677 1678 1679
    ##   .. ..$ : int [1:6] 1680 1681 1682 1683 1684 1685
    ##   .. ..$ : int [1:9] 1686 1687 1688 1689 1690 1691 1692 1693 1694
    ##   .. ..$ : int [1:9] 1695 1696 1697 1698 1699 1700 1701 1702 1703
    ##   .. ..$ : int [1:9] 1704 1705 1706 1707 1708 1709 1710 1711 1712
    ##   .. ..$ : int [1:9] 1713 1714 1715 1716 1717 1718 1719 1720 1721
    ##   .. ..$ : int [1:9] 1722 1723 1724 1725 1726 1727 1728 1729 1730
    ##   .. ..$ : int [1:9] 1731 1732 1733 1734 1735 1736 1737 1738 1739
    ##   .. ..$ : int [1:9] 1740 1741 1742 1743 1744 1745 1746 1747 1748
    ##   .. .. [list output truncated]
    ##   .. ..@ ptype: int(0) 
    ##   ..- attr(*, ".drop")= logi TRUE

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
