OpenDengue data coverage
================
Joe Clarke & Ahyoung Lim
Last update on March 31, 2023

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
    ## 2    paho_adm0   14831
    ## 3 paho_imputed     285
    ## 4     paho_sub   36237
    ## 5        tycho    6929

``` r
plyr::count(data$adm_0_name)
```

    ##                                    x    freq
    ## 1                           Anguilla     151
    ## 2                Antigua and Barbuda     195
    ## 3                Antigua And Barbuda      41
    ## 4                          Argentina    7295
    ## 5                              Aruba     188
    ## 6                            Bahamas     165
    ## 7                           Barbados     483
    ## 8                             Belize     436
    ## 9                            Bermuda     362
    ## 10                           Bolivia    4525
    ## 11 Bonaire, Saint Eustatius and Saba     152
    ## 12                            Brazil 2442513
    ## 13                            Canada     471
    ## 14                    Cayman Islands     172
    ## 15                             Chile     226
    ## 16                          Colombia    2652
    ## 17                        Costa Rica    1944
    ## 18                              Cuba     223
    ## 19                           Curacao     306
    ## 20                          Dominica     120
    ## 21                Dominican republic    8472
    ## 22                Dominican Republic     508
    ## 23                           Ecuador    4027
    ## 24                       El Salvador    2082
    ## 25                     French Guiana     162
    ## 26                           Grenada     133
    ## 27                        Guadeloupe     141
    ## 28                         Guatemala     578
    ## 29                            Guyana      71
    ## 30                             Haiti     176
    ## 31                          Honduras     486
    ## 32                           Jamaica     456
    ## 33                        Martinique     173
    ## 34                            Mexico   14010
    ## 35                        Montserrat     226
    ## 36                         Nicaragua   19826
    ## 37                            Panama    5121
    ## 38                          Paraguay     407
    ## 39                              Peru    4254
    ## 40                       Puerto Rico     485
    ## 41                  Saint Barthelemy     243
    ## 42             Saint Kitts and Nevis     217
    ## 43             Saint Kitts And Nevis      43
    ## 44                       Saint Lucia     313
    ## 45                      Saint Martin     177
    ## 46  Saint Vincent and the Grenadines     172
    ## 47  Saint Vincent And The Grenadines      32
    ## 48                      Sint Maarten      94
    ## 49                          Suriname     285
    ## 50               Trinidad and Tobago      79
    ## 51               Trinidad And Tobago      34
    ## 52          Turks and Caicos Islands     170
    ## 53          Turks And Caicos Islands      23
    ## 54          United States of America     464
    ## 55          United States Of America     317
    ## 56                           Uruguay     395
    ## 57                         Venezuela    2100
    ## 58               Virgin Islands (UK)     140
    ## 59               Virgin Islands (US)     469

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
    ## 1    6 1932596
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
    ## 1 0   16820
    ## 2 1   63442
    ## 3 2 2449919

``` r
plyr::count(data$temporal_res)
```

    ##    x    freq
    ## 1  0    3246
    ## 2  1  585826
    ## 3  2 1932596
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
    ##  7            2           0 paho_adm0      14831
    ##  8            2           0 paho_imputed     285
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

    ##                                    x    freq
    ## 1                           Anguilla     151
    ## 2                Antigua and Barbuda     195
    ## 3                Antigua And Barbuda      41
    ## 4                          Argentina    7295
    ## 5                              Aruba     188
    ## 6                            Bahamas     165
    ## 7                           Barbados     483
    ## 8                             Belize     436
    ## 9                            Bermuda     362
    ## 10                           Bolivia    4525
    ## 11 Bonaire, Saint Eustatius and Saba     152
    ## 12                            Brazil 2434000
    ## 13                            Canada     471
    ## 14                    Cayman Islands     172
    ## 15                             Chile     226
    ## 16                          Colombia    2652
    ## 17                        Costa Rica    1736
    ## 18                              Cuba     223
    ## 19                           Curacao     306
    ## 20                          Dominica     120
    ## 21                Dominican republic    8472
    ## 22                Dominican Republic     508
    ## 23                           Ecuador    4027
    ## 24                       El Salvador    2082
    ## 25                     French Guiana     162
    ## 26                           Grenada     133
    ## 27                        Guadeloupe     141
    ## 28                         Guatemala     578
    ## 29                            Guyana      71
    ## 30                             Haiti     176
    ## 31                          Honduras     486
    ## 32                           Jamaica     456
    ## 33                        Martinique     173
    ## 34                            Mexico   14010
    ## 35                        Montserrat     226
    ## 36                         Nicaragua   19826
    ## 37                            Panama    5121
    ## 38                          Paraguay     407
    ## 39                              Peru    4254
    ## 40                       Puerto Rico     485
    ## 41                  Saint Barthelemy     243
    ## 42             Saint Kitts and Nevis     217
    ## 43             Saint Kitts And Nevis      43
    ## 44                       Saint Lucia     313
    ## 45                      Saint Martin     177
    ## 46  Saint Vincent and the Grenadines     172
    ## 47  Saint Vincent And The Grenadines      32
    ## 48                      Sint Maarten      94
    ## 49                          Suriname     285
    ## 50               Trinidad and Tobago      79
    ## 51               Trinidad And Tobago      34
    ## 52          Turks and Caicos Islands     170
    ## 53          Turks And Caicos Islands      23
    ## 54          United States of America     464
    ## 55          United States Of America     317
    ## 56                           Uruguay     395
    ## 57                         Venezuela    2100
    ## 58               Virgin Islands (UK)     140
    ## 59               Virgin Islands (US)     469

``` r
# Make country names consistent
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

plyr::count(data$adm_0_name) #52 countries
```

    ##                                    x    freq
    ## 1                           Anguilla     151
    ## 2                Antigua and Barbuda     236
    ## 3                          Argentina    7295
    ## 4                              Aruba     188
    ## 5                            Bahamas     165
    ## 6                           Barbados     483
    ## 7                             Belize     436
    ## 8                            Bermuda     362
    ## 9                            Bolivia    4525
    ## 10 Bonaire, Saint Eustatius and Saba     152
    ## 11                            Brazil 2434000
    ## 12                            Canada     471
    ## 13                    Cayman Islands     172
    ## 14                             Chile     226
    ## 15                          Colombia    2652
    ## 16                        Costa Rica    1736
    ## 17                              Cuba     223
    ## 18                           Curacao     306
    ## 19                          Dominica     120
    ## 20                Dominican Republic    8980
    ## 21                           Ecuador    4027
    ## 22                       El Salvador    2082
    ## 23                     French Guiana     162
    ## 24                           Grenada     133
    ## 25                        Guadeloupe     141
    ## 26                         Guatemala     578
    ## 27                            Guyana      71
    ## 28                             Haiti     176
    ## 29                          Honduras     486
    ## 30                           Jamaica     456
    ## 31                        Martinique     173
    ## 32                            Mexico   14010
    ## 33                        Montserrat     226
    ## 34                         Nicaragua   19826
    ## 35                            Panama    5121
    ## 36                          Paraguay     407
    ## 37                              Peru    4254
    ## 38                       Puerto Rico     485
    ## 39                  Saint Barthelemy     243
    ## 40             Saint Kitts and Nevis     260
    ## 41                       Saint Lucia     313
    ## 42                      Saint Martin     177
    ## 43  Saint Vincent and the Grenadines     204
    ## 44                      Sint Maarten      94
    ## 45                          Suriname     285
    ## 46               Trinidad and Tobago     113
    ## 47          Turks and Caicos Islands     193
    ## 48          United States of America     781
    ## 49                           Uruguay     395
    ## 50                         Venezuela    2100
    ## 51               Virgin Islands (UK)     140
    ## 52               Virgin Islands (US)     469

``` r
# ordering country by longitude (?)
lookup_order <- read.csv("data/heatmap_order.csv")
names(lookup_order)
```

    ## [1] "adm_0_name"    "desired_order"

``` r
#data_ctr <- data %>% select(adm_0_name) %>% distinct()
#data_ctr <- merge(data_ctr, lookup_order, by=c("adm_0_name"), all.x=T, all.y=T)

plyr::count(data$adm_0_name) #alphabetic order 
```

    ##                                    x    freq
    ## 1                           Anguilla     151
    ## 2                Antigua and Barbuda     236
    ## 3                          Argentina    7295
    ## 4                              Aruba     188
    ## 5                            Bahamas     165
    ## 6                           Barbados     483
    ## 7                             Belize     436
    ## 8                            Bermuda     362
    ## 9                            Bolivia    4525
    ## 10 Bonaire, Saint Eustatius and Saba     152
    ## 11                            Brazil 2434000
    ## 12                            Canada     471
    ## 13                    Cayman Islands     172
    ## 14                             Chile     226
    ## 15                          Colombia    2652
    ## 16                        Costa Rica    1736
    ## 17                              Cuba     223
    ## 18                           Curacao     306
    ## 19                          Dominica     120
    ## 20                Dominican Republic    8980
    ## 21                           Ecuador    4027
    ## 22                       El Salvador    2082
    ## 23                     French Guiana     162
    ## 24                           Grenada     133
    ## 25                        Guadeloupe     141
    ## 26                         Guatemala     578
    ## 27                            Guyana      71
    ## 28                             Haiti     176
    ## 29                          Honduras     486
    ## 30                           Jamaica     456
    ## 31                        Martinique     173
    ## 32                            Mexico   14010
    ## 33                        Montserrat     226
    ## 34                         Nicaragua   19826
    ## 35                            Panama    5121
    ## 36                          Paraguay     407
    ## 37                              Peru    4254
    ## 38                       Puerto Rico     485
    ## 39                  Saint Barthelemy     243
    ## 40             Saint Kitts and Nevis     260
    ## 41                       Saint Lucia     313
    ## 42                      Saint Martin     177
    ## 43  Saint Vincent and the Grenadines     204
    ## 44                      Sint Maarten      94
    ## 45                          Suriname     285
    ## 46               Trinidad and Tobago     113
    ## 47          Turks and Caicos Islands     193
    ## 48          United States of America     781
    ## 49                           Uruguay     395
    ## 50                         Venezuela    2100
    ## 51               Virgin Islands (UK)     140
    ## 52               Virgin Islands (US)     469

``` r
data$adm_0_name <- factor(data$adm_0_name, 
                          levels = unique(lookup_order$adm_0_name[order(lookup_order$desired_order)]))

plyr::count(data$adm_0_name) #desired order
```

    ##                                    x    freq
    ## 1                             Canada     471
    ## 2           United States of America     781
    ## 3                             Belize     436
    ## 4                         Costa Rica    1736
    ## 5                        El Salvador    2082
    ## 6                          Guatemala     578
    ## 7                           Honduras     486
    ## 8                             Mexico   14010
    ## 9                          Nicaragua   19826
    ## 10                            Panama    5121
    ## 11                           Bolivia    4525
    ## 12                          Colombia    2652
    ## 13                           Ecuador    4027
    ## 14                              Peru    4254
    ## 15                         Venezuela    2100
    ## 16                         Argentina    7295
    ## 17                            Brazil 2434000
    ## 18                             Chile     226
    ## 19                          Paraguay     407
    ## 20                           Uruguay     395
    ## 21                              Cuba     223
    ## 22                Dominican Republic    8980
    ## 23                       Puerto Rico     485
    ## 24                          Anguilla     151
    ## 25               Antigua and Barbuda     236
    ## 26                             Aruba     188
    ## 27                           Bahamas     165
    ## 28                          Barbados     483
    ## 29                           Bermuda     362
    ## 30 Bonaire, Saint Eustatius and Saba     152
    ## 31                    Cayman Islands     172
    ## 32                           Curacao     306
    ## 33                          Dominica     120
    ## 34                     French Guiana     162
    ## 35                           Grenada     133
    ## 36                        Guadeloupe     141
    ## 37                            Guyana      71
    ## 38                             Haiti     176
    ## 39                           Jamaica     456
    ## 40                        Martinique     173
    ## 41                        Montserrat     226
    ## 42                  Saint Barthelemy     243
    ## 43             Saint Kitts and Nevis     260
    ## 44                       Saint Lucia     313
    ## 45                      Saint Martin     177
    ## 46  Saint Vincent and the Grenadines     204
    ## 47                      Sint Maarten      94
    ## 48                          Suriname     285
    ## 49               Trinidad and Tobago     113
    ## 50          Turks and Caicos Islands     193
    ## 51               Virgin Islands (UK)     140
    ## 52               Virgin Islands (US)     469

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

    ## # A tibble: 1,797 × 6
    ## # Groups:   adm_0_name, year [1,797]
    ##    adm_0_name  year Monthly Weekly Yearly total
    ##    <fct>      <int>   <dbl>  <dbl>  <dbl> <dbl>
    ##  1 Nicaragua   2004       1      1      1     3
    ##  2 Nicaragua   2005       1      1      1     3
    ##  3 Canada      2009      NA     NA      1    NA
    ##  4 Canada      2012      NA     NA      1    NA
    ##  5 Canada      2014      NA      1     NA    NA
    ##  6 Canada      2015      NA      1     NA    NA
    ##  7 Canada      2016      NA      1     NA    NA
    ##  8 Canada      2017      NA      1     NA    NA
    ##  9 Canada      2018      NA      1     NA    NA
    ## 10 Canada      2019      NA      1     NA    NA
    ## # … with 1,787 more rows

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

plyr::count(temp_boolean$adm_0_name)
```

    ##                                    x freq
    ## 1                             Canada   64
    ## 2           United States of America   64
    ## 3                             Belize   64
    ## 4                         Costa Rica   64
    ## 5                        El Salvador   64
    ## 6                          Guatemala   64
    ## 7                           Honduras   64
    ## 8                             Mexico   64
    ## 9                          Nicaragua   64
    ## 10                            Panama   64
    ## 11                           Bolivia   64
    ## 12                          Colombia   64
    ## 13                           Ecuador   64
    ## 14                              Peru   64
    ## 15                         Venezuela   64
    ## 16                         Argentina   64
    ## 17                            Brazil   64
    ## 18                             Chile   64
    ## 19                          Paraguay   64
    ## 20                           Uruguay   64
    ## 21                              Cuba   64
    ## 22                Dominican Republic   64
    ## 23                       Puerto Rico   64
    ## 24                          Anguilla   64
    ## 25               Antigua and Barbuda   64
    ## 26                             Aruba   64
    ## 27                           Bahamas   64
    ## 28                          Barbados   64
    ## 29                           Bermuda   64
    ## 30 Bonaire, Saint Eustatius and Saba   64
    ## 31                    Cayman Islands   64
    ## 32                           Curacao   64
    ## 33                          Dominica   64
    ## 34                     French Guiana   64
    ## 35                           Grenada   64
    ## 36                        Guadeloupe   64
    ## 37                            Guyana   64
    ## 38                             Haiti   64
    ## 39                           Jamaica   64
    ## 40                        Martinique   64
    ## 41                        Montserrat   64
    ## 42                  Saint Barthelemy   64
    ## 43             Saint Kitts and Nevis   64
    ## 44                       Saint Lucia   64
    ## 45                      Saint Martin   64
    ## 46  Saint Vincent and the Grenadines   64
    ## 47                      Sint Maarten   64
    ## 48                          Suriname   64
    ## 49               Trinidad and Tobago   64
    ## 50          Turks and Caicos Islands   64
    ## 51               Virgin Islands (UK)   64
    ## 52               Virgin Islands (US)   64

``` r
temp_boolean %>%
  filter(!year < 1990)%>%
  mutate(year = as.character(year))%>%
  
ggplot( aes(x=year, y=adm_0_name))+
  geom_tile(aes(fill=temporal_res_nm), 
            color = "white", lwd = 0.8, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                   breaks=seq(1990, 2020, by=5)
                     )+
  scale_fill_manual(name = "Temporal resolution", 
                    values = c("#B2DF8A",  "#1F78B4", "#CAB2D6"), na.value="#D9D9D9")+
                             #"#6A3D9A", "#FFFF99", "#33A02C",   "#B15928","#A6CEE3"
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

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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

    ## # A tibble: 1,797 × 6
    ## # Groups:   adm_0_name, year [1,797]
    ##    adm_0_name  year  adm0  adm1  adm2 total
    ##    <fct>      <int> <dbl> <dbl> <dbl> <dbl>
    ##  1 Brazil      2001     1     1     1     3
    ##  2 Brazil      2002     1     1     1     3
    ##  3 Brazil      2003     1     1     1     3
    ##  4 Brazil      2004     1     1     1     3
    ##  5 Brazil      2005     1     1     1     3
    ##  6 Canada      2009     1    NA    NA    NA
    ##  7 Canada      2012     1    NA    NA    NA
    ##  8 Canada      2014     1    NA    NA    NA
    ##  9 Canada      2015     1    NA    NA    NA
    ## 10 Canada      2016     1    NA    NA    NA
    ## # … with 1,787 more rows

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
            color = "white",  lwd = 0.8, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(  breaks = seq(1990,2020, by=5), 
                     expand=c(0,0))+
  scale_fill_manual(name = "Spatial resolution", 
                    values = c("#B2DF8A","#1F78B4", "#CAB2D6" ), na.value="#D9D9D9")+
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
        plot.margin = margin(t=20, b=20, l=20, r=20), 
        plot.background=element_blank(),
        panel.border=element_blank())
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

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

    ## # A tibble: 1,797 × 7
    ## # Groups:   adm_0_name, year [1,797]
    ##    adm_0_name                year   moh paho_sub tycho paho_tot total
    ##    <fct>                    <int> <dbl>    <dbl> <dbl>    <dbl> <dbl>
    ##  1 United States of America  2015    NA       NA     1        1     2
    ##  2 United States of America  2016    NA       NA     1        1     2
    ##  3 United States of America  2017    NA       NA     1        1     2
    ##  4 Costa Rica                2015    NA        1    NA        1     2
    ##  5 Costa Rica                2016    NA        1    NA        1     2
    ##  6 Costa Rica                2017    NA        1    NA        1     2
    ##  7 Costa Rica                2018    NA        1    NA        1     2
    ##  8 Guatemala                 2007     1       NA     1       NA     2
    ##  9 Guatemala                 2008     1       NA     1       NA     2
    ## 10 Guatemala                 2009     1       NA     1       NA     2
    ## # … with 1,787 more rows

``` r
source_boolean <- data %>%
  mutate(source_cat = ifelse(source_cat %in% c("paho_adm0", "paho_imputed"), "paho", source_cat))%>%
  group_by(adm_0_name, year, source_cat)%>%
  tally()%>%
  arrange(adm_0_name, year, desc(n))%>%  
  group_by(adm_0_name, year) %>% slice_head(n=1)%>%  ungroup()%>%
  complete(adm_0_name, year) 

plyr::count(source_boolean$source_cat) # here 'NA' created because of the use of 'complete' function
```

    ##          x freq
    ## 1      moh   70
    ## 2     paho  408
    ## 3 paho_sub   39
    ## 4    tycho 1280
    ## 5     <NA> 1531

In the case of multiple data sources available for the same
country-year, the data source with the most data rows is chosen and
shown on the figure below.

``` r
source_boolean %>%
  filter(!year < 1990)%>%
  mutate(year = as.character(year))%>%
ggplot( aes(x=year, y=adm_0_name, group=source_cat))+
  geom_tile(aes(fill=source_cat), 
            color = "white",  lwd = 0.8, linetype = 1)+
  scale_y_discrete(limits=rev, expand=c(0,0))+
  scale_x_discrete(  breaks = seq(1990,2020, by=5), 
                     expand=c(0,0))+
  scale_fill_manual(name = "Data source", 
                    values = c("#1F78B4","#B2DF8A","#4c841c", "#CAB2D6" ), na.value="#D9D9D9")+
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
        plot.margin = margin(t=20, b=20, l=20, r=20), 
        plot.background=element_blank(),
        panel.border=element_blank())
```

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
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

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

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

![](C:\Users\AHYOUN~1\Dropbox\WORK\DENGUE~1\DENGUE~1\MASTER~1\DATA_C~1/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# Adm2 - MoH; Adm1 - equally distributed; Adm0 - Tycho. adm2 and adm1 code matching needs to be done to ensure more accurate data coverage though...
```
