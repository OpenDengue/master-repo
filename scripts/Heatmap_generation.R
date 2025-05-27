rm(list = ls())
pacman::p_load("dplyr", "lubridate", "tidyr", "tidyverse", "knitr", "stringi", "zoo", "data.table", "ggplot2", "sf", "rnaturalearth", "sf", "rnaturalearthdata", "distill", "downloadthis", "patchwork", "plotly", "showtext", "here", "htmlwidgets", "crosstalk")
showtext_auto()
font_add_google("Open Sans")

# setwd("C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/11_DENData")
git_path <- "C:/Users/AhyoungLim/Dropbox/WORK/OpenDengue/master-repo-alim/master-repo/"
today <- gsub("-", "_", Sys.Date())
version <- "V1.3"
source(paste0(git_path, "scripts/Year_checker.R"))

# DATA ====================================================
S_data <- read.csv(paste0(git_path, "data/releases/V1.3/Spatial_extract_V1_3.csv")) %>%
  filter(adm_0_name != "CANADA") %>%
  Year_checker()

T_data <- read.csv(paste0(git_path, "data/releases/V1.3/Temporal_extract_V1_3.csv")) %>%
  filter(adm_0_name != "CANADA") %>%
  Year_checker()

summary(is.na(S_data))
summary(is.na(T_data))

plyr::count(S_data$S_res)
plyr::count(T_data$T_res)

# Heatmap for temporal resolution ========================
temp_boolean <- T_data %>%
  mutate(T_res = ifelse(T_res == "Week", 2,
    ifelse(T_res == "Month", 1, 0)
  )) %>%
  group_by(adm_0_name, Year, T_res) %>%
  tally() %>%
  arrange(adm_0_name, Year, desc(T_res)) %>%
  group_by(adm_0_name, Year) %>%
  slice_head(n = 1) %>%
  select(-n) %>%
  ungroup() %>%
  complete(adm_0_name, Year) %>%
  mutate(T_res_nm = ifelse(T_res == 2, "Weekly",
    ifelse(T_res == 1, "Monthly", "Yearly")
  )) %>%
  mutate(T_res_nm = factor(T_res_nm, levels = c("Weekly", "Monthly", "Yearly"))) %>%
  select(-T_res)

spat_boolean <- S_data %>%
  mutate(S_res = ifelse(S_res == "Admin2", 2,
    ifelse(S_res == "Admin1", 1, 0)
  )) %>%
  group_by(adm_0_name, Year, S_res) %>%
  tally() %>%
  arrange(adm_0_name, Year, desc(S_res)) %>%
  group_by(adm_0_name, Year) %>%
  slice_head(n = 1) %>%
  select(-n) %>%
  ungroup() %>%
  complete(adm_0_name, Year) %>%
  mutate(S_res_nm = ifelse(S_res == 2, "Admin2",
    ifelse(S_res == 1, "Admin1", "Admin0")
  )) %>%
  mutate(S_res_nm = factor(S_res_nm, levels = c("Admin2", "Admin1", "Admin0"))) %>%
  select(-S_res)

dt_heatmap <- merge(temp_boolean, spat_boolean, by = c("adm_0_name", "Year"), all = T) %>%
  mutate(adm_0_name = tools::toTitleCase(tolower(adm_0_name))) %>%
  mutate(adm_0_name = ifelse(adm_0_name == "Virgin Islands (Uk)", "Virgin Islands (UK)",
    ifelse(adm_0_name == "Virgin Islands (Us)", "Virgin Islands (US)", adm_0_name)
  ))

# exclude for now
dt_heatmap <- dt_heatmap %>% filter(!adm_0_name %in% c("Mongolia"))




plyr::count(dt_heatmap$adm_0_name)
summary(is.na(dt_heatmap))

# group by region
# country order ========================================
americas_order <- c(
  # North America
  "United States of America", "Canada",
  # Central America
  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",

  # South America
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
  "Paraguay", "Peru", "Uruguay", "Venezuela",

  # Caribbean
  "Anguilla", "Antigua and Barbuda", "Aruba",
  "Bahamas", "Barbados", "Bermuda", "Bonaire, Saint Eustatius and Saba",
  "Cayman Islands", "Cuba", "Curacao",
  "Dominica", "Dominican Republic",
  "French Guiana",
  "Grenada", "Guadeloupe", "Guyana",
  "Haiti",
  "Jamaica",
  "Martinique", "Montserrat",
  "Puerto Rico",
  "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sint Maarten", "Suriname",
  "Trinidad and Tobago", "Turks and Caicos Islands",
  "Virgin Islands (UK)", "Virgin Islands (US)"
)

americas1 <- c(
  # # North America
  "United States of America", "Canada",
  # Central America
  "Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama"
)
americas2 <- c(
  # South America
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
  "Paraguay", "Peru", "Uruguay", "Venezuela"
)
americas3 <- c(
  # Caribbean
  "Anguilla", "Antigua and Barbuda", "Aruba",
  "Bahamas", "Barbados", "Bermuda", "Bonaire, Saint Eustatius and Saba",
  "Cayman Islands", "Cuba", "Curacao",
  "Dominica", "Dominican Republic",
  "French Guiana",
  "Grenada", "Guadeloupe", "Guyana",
  "Haiti",
  "Jamaica",
  "Martinique", "Montserrat",
  "Puerto Rico",
  "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Vincent and the Grenadines", "Sint Maarten", "Suriname",
  "Trinidad and Tobago", "Turks and Caicos Islands",
  "Virgin Islands (UK)", "Virgin Islands (US)"
)

asia_order <- c(
  # Southeast asia
  "Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Timor-Leste", "Viet Nam",
  # South asia
  "Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Saudi Arabia", "Sri Lanka",
  # East asia
  "China", "Hong Kong", "Japan", "Macau", "Taiwan",

  # Oceania
  "Australia", "American Samoa", "Cook Islands",
  "Fiji", "French Polynesia",
  "Guam", "Kiribati", "Marshall Islands", "Micronesia (Federated States of)", "Nauru", "New Caledonia", "Niue", "Northern Mariana Islands", "Palau", "Papua New Guinea", "Pitcairn", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna"
)

asia1 <- c(
  # Southeast asia
  "Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Timor-Leste", "Viet Nam"
)
asia2 <- c(
  # South asia
  "Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Saudi Arabia", "Sri Lanka"
)

asia3 <- c(
  # East asia
  "China", "Hong Kong", "Japan", "Macau", "Taiwan"
)
asia4 <- c(
  # Oceania
  "Australia", "American Samoa", "Cook Islands",
  "Fiji", "French Polynesia",
  "Guam", "Kiribati", "Marshall Islands", "Micronesia (Federated States of)", "Nauru", "New Caledonia", "Niue", "Northern Mariana Islands", "Palau", "Papua New Guinea", "Pitcairn", "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna"
)

dt_heatmap$region <- ifelse(dt_heatmap$adm_0_name %in% asia_order, "Asia",
  ifelse(dt_heatmap$adm_0_name %in% americas_order, "Americas", NA)
)

dt_heatmap <- dt_heatmap %>%
  mutate(
    subregion = case_when(
      adm_0_name %in% americas1 ~ "North & Central America",
      adm_0_name %in% americas2 ~ "South America",
      adm_0_name %in% americas3 ~ "Caribbean",
      region == "Asia" & adm_0_name %in% asia1 ~ "Southeast Asia",
      region == "Asia" & adm_0_name %in% asia2 ~ "South Asia",
      region == "Asia" & adm_0_name %in% asia3 ~ "East Asia",
      region == "Asia" & adm_0_name %in% asia4 ~ "Oceania",
      TRUE ~ NA_character_
    )
  )

plyr::count(dt_heatmap$adm_0_name)
plyr::count(dt_heatmap$region)
plyr::count(dt_heatmap$subregion)

unique(dt_heatmap$adm_0_name[is.na(dt_heatmap$region)])
dt_heatmap$subregion <- factor(dt_heatmap$subregion, levels = c(
  "North & Central America",
  "South America",
  "Caribbean",
  "Southeast Asia",
  "South Asia",
  "East Asia",
  "Oceania"
))

# HEATMAP function ==========================================
heatmap_base <- function(data, region_name, type, subregion = FALSE) {
  scale_temporal <- scale_fill_manual(
    name = "Temporal resolution",
    values = c("#479A5A", "#99D8CA", "#E5F5F9"), na.value = "#D9D9D9"
  )
  scale_spatial <- scale_fill_manual(
    name = "Spatial resolution",
    values = c("#c51b8a", "#fa9fb5", "#fde0dd"), na.value = "#D9D9D9"
  )
  title <- region_name

  if (type == "temporal") {
    fill_var <- "T_res_nm"
    scale_color <- scale_temporal
  } else {
    fill_var <- "S_res_nm"
    scale_color <- scale_spatial
  }

  if (region_name == "Americas") {
    country_order <- americas_order
    title <- "Americas"
  } else {
    country_order <- asia_order
    title <- "Asia"
  }

  base_p <- data %>%
    filter(!Year < 1990 & region == region_name) %>%
    mutate(Year = as.character(Year)) %>%
    ggplot(aes(
      x = Year, y = factor(adm_0_name, level = country_order), fill = !!sym(fill_var),
      text = paste0(adm_0_name, ", ", Year, ", ", !!sym(fill_var))
    )) +
    geom_tile(color = "white", lwd = 0.8, linetype = 1) +
    scale_y_discrete(limits = rev, expand = c(0, 0)) +
    scale_x_discrete(
      expand = c(0, 0),
      breaks = seq(1990, 2020, by = 5)
    ) +
    # coord_fixed()+
    theme_bw() +
    ggtitle(title) +
    xlab("Year") +
    ylab("Country") +
    theme(
      text = element_text(family = "Open Sans"),
      # plot.title = element_text(size=28),
      # axis.title.x = element_text(size=28, vjust=0),
      # axis.title.y = element_text(size=28),
      # axis.text = element_text(size=20),
      axis.ticks = element_line(size = 0.2),
      axis.ticks.length = unit(1.5, "pt"),
      # legend.title = element_text(size=20),
      # legend.text = element_text(size=20),
      legend.spacing.x = unit(0.2, "cm"),
      # plot.margin = margin(t=-50, b=-50, l=20, r=20),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.2, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    scale_color

  if (subregion) {
    base_p <- base_p +
      facet_grid(subregion ~ ., switch = "y", scales = "free_y", space = "free_y")
  }

  return(base_p)
}

# TEMPORAL ====================================================
# interactive


PAHO_temp <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2023, ], "Americas", "temporal", subregion = FALSE) + xlab(NULL) + ylab(NULL)
ASIA_temp <- heatmap_base(dt_heatmap, "Asia", "temporal", subregion = FALSE) + xlab(NULL) + ylab(NULL)


# annotations
a <- list(
  text = "Americas",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "left",
  x = 0.07,
  y = 1,
  showarrow = FALSE
)

b <- list(
  text = "Asia",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "left",
  x = 0.05,
  y = 1,
  showarrow = FALSE
)

p1 <- ggplotly(PAHO_temp + ggtitle(NULL), tooltip = c("text")) %>% layout(annotations = a)
p2 <- ggplotly(ASIA_temp + ggtitle(NULL), tooltip = c("text")) %>% layout(annotations = b)
p <- subplot(style(p1, showlegend = F), p2, nrows = 1, margin = 0.08) %>%
  layout(
    autosize = T, margin = list(t = 40),
    legend = list(
      orientation = "h", # show entries horizontally
      xanchor = "center", # use center of legend as anchor
      x = 0.5, y = -0.05
    ),
    title = "Best temporal resolution available"
  )

saveWidget(p, paste0(git_path, "docs/figure/heatmap_temporal_", today, ".html"), selfcontained = T, libdir = "lib")

plyr::count(dt_heatmap$T_res_nm)

# static image
PAHO_temp <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Americas", "temporal", subregion = TRUE) + xlab(NULL) + ylab(NULL) +
  theme(
    plot.title = element_text(size = 28),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ASIA_temp <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Asia", "temporal", subregion = TRUE) + xlab(NULL) + ylab(NULL) +
  theme(
    plot.title = element_text(size = 28),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

combined <- PAHO_temp + ASIA_temp & theme(legend.position = "bottom")
combined <- combined + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Best temporal resolution available",
    theme = theme(plot.title = element_text(
      size = 30, face = "bold",
      family = "Open Sans", hjust = 0.5
    ))
  )


ggsave(paste0(git_path, "docs/figure/heatmap_temporal_", today, ".png"), combined, height = 6.4, width = 10.8, dpi = 300, bg = "white")



# SPATIAL ==================================================

# interactive

PAHO_spat <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Americas", "spatial", subregion = FALSE) + xlab(NULL) + ylab(NULL)
ASIA_spat <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Asia", "spatial", subregion = FALSE) + xlab(NULL) + ylab(NULL)

p1 <- ggplotly(PAHO_spat + ggtitle(NULL), tooltip = c("text")) %>% layout(annotations = a)
p2 <- ggplotly(ASIA_spat + ggtitle(NULL), tooltip = c("text")) %>% layout(annotations = b)
p <- subplot(style(p1, showlegend = F), p2, nrows = 1, margin = 0.08) %>%
  layout(
    autosize = T, margin = list(t = 40),
    legend = list(
      orientation = "h", # show entries horizontally
      xanchor = "center", # use center of legend as anchor
      x = 0.5, y = -0.05
    ),
    title = "Best spatial resolution available"
  )
# htmltools::save_html(p,"docs/figure/heatmap_spatial_.html")
saveWidget(p, paste0(git_path, "docs/figure/heatmap_spatial_", today, ".html"), selfcontained = T, libdir = "lib")

# image
PAHO_spat <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Americas", "spatial", subregion = TRUE) + xlab(NULL) + ylab(NULL) +
  theme(
    plot.title = element_text(size = 28),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ASIA_spat <- heatmap_base(dt_heatmap[!dt_heatmap$Year == 2025, ], "Asia", "spatial", subregion = TRUE) + xlab(NULL) + ylab(NULL) +
  theme(
    plot.title = element_text(size = 28),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 17),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

combined2 <- PAHO_spat + ASIA_spat & theme(legend.position = "bottom")
combined2 <- combined2 + plot_layout(guides = "collect") +
  plot_annotation(
    title = "Best spatial resolution available",
    theme = theme(plot.title = element_text(
      size = 30, face = "bold",
      family = "Open Sans", hjust = 0.5
    ))
  )
ggsave(filename = paste0(git_path, "docs/figure/heatmap_spatial_", today, ".png"), combined2, height = 6.4, width = 10.8, units = "in", dpi = 300, bg = "white")
