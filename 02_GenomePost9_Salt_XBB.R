suppressPackageStartupMessages({
  library(covmuller)
  library(tidyverse)
  library(gghighlight)
  library(ggtext)
  library(rgdal)
  library(sf)
})
GetIndianStates <- function() {
  state_names <- list(
    TT = "India",
    AP = "Andhra Pradesh", AR = "Arunachal Pradesh",
    AS = "Assam", BR = "Bihar",
    CT = "Chhattisgarh",
    GA = "Goa", GJ = "Gujarat",
    HR = "Haryana", HP = "Himachal Pradesh",
    JH = "Jharkhand",
    KA = "Karnataka", KL = "Kerala",
    MP = "Madhya Pradesh", MH = "Maharashtra", MN = "Manipur", ML = "Meghalaya", MZ = "Mizoram",
    NL = "Nagaland",
    OR = "Odisha",
    PB = "Punjab",
    RJ = "Rajasthan",
    SK = "Sikkim",
    TN = "Tamil Nadu", TG = "Telangana", TR = "Tripura", UT = "Uttarakhand",
    UP = "Uttar Pradesh",
    WB = "West Bengal",
    AN = "Andaman & Nicobar",
    CH = "Chandigarh",
    DN = "Dadra and Nagar Haveli and Daman and Diu", DL = "Delhi",
    JK = "Jammu and Kashmir",
    LA = "Ladakh", LD = "Lakshadweep",
    PY = "Puducherry"
  )
  return(state_names)
}
indian_state_cases <- GetIndiaConfirmedCasesMonthlyLong()
india_cases <- indian_state_cases %>% filter(State == "India")


p1 <- BarPlot(india_cases, ylabel = "Cases per month", label_si = TRUE, title = "Total cases per month - India",
              caption = paste0("**Source: covid19bharat.org<br>** ", Sys.Date()))
p1

ggsave("plots/02_india_total_cases.png", device=png, dpi=300, width=8, height=5.35)

current_date <- "2023_01_14"
fpath.qs <- paste0("~/github/2021_Covid19_surveillance/data/all_metadata/metadata_tsv_", current_date, ".qs")
gisaid_metadata <- qs::qread(file = fpath.qs)
gisaid_india <- FilterGISAIDIndia(gisaid_metadata_all = gisaid_metadata)
country_seq_stats <- TotalSequencesPerMonthCountrywise(gisaid_india, rename_country_as_state = TRUE)
p2 <- BarPlot(country_seq_stats, ylabel = "Sequenced per month", color = "slateblue1", label_si = TRUE, title = "Total sequences deposited to GISAID from India", caption = paste0("**Source: gisaid.org; covid19bharat.org<br>** ", Sys.Date()))
p2
ggsave("plots/02_india_total_Sequences_per_month.png", device = png, dpi = 300, width = 8, height = 5.35)

india_cases_long <- GetIndiaConfirmedCasesMonthlyLong() %>% filter(State == "India")
india_sequencing_proportion <- CombineSequencedCases(
  cases_sequenced = country_seq_stats,
  confirmed_long = india_cases_long
)

p3 <- BarPlot(india_sequencing_proportion, yaxis = "percent_sequenced_collected", ylabel = "%  deposited to GISAID", color = "yellowgreen", title = "Proportion of cases deposited to GISAID from India",
              caption = paste0("**Source: gisaid.org; covid19bharat.org<br>** ", Sys.Date()))
p3

ggsave("plots/02_india_percent_Sequences_per_month.png", device = png, dpi = 300, width = 8, height = 5.35)

state_seq_stats <- TotalSequencesPerMonthStatewise(gisaid_india, drop_country = TRUE)
seq_stats <- rbind(country_seq_stats, state_seq_stats)
state_cases_long <- GetIndiaConfirmedCasesMonthlyLong()
india_sequencing_proportion <- CombineSequencedCases(
  cases_sequenced = seq_stats,
  confirmed_long = state_cases_long,
  month.min = "Jan 2022",
  month.max = "Feb 2023",
  max.percent = 5
)

india_sequencing_proportion$State <- factor(
  x = india_sequencing_proportion$State,
  levels = as.character(GetIndianStates())
)
india_sequencing_proportion$Year <- stringr::str_split_fixed(india_sequencing_proportion$MonthYear, pattern = " ", n = 2)[, 2]
india_sequencing_proportion_median <- india_sequencing_proportion_median <- india_sequencing_proportion %>%
  group_by(State, Year) %>%
  summarise(
    min_percent_sequenced_collected = min(percent_sequenced_collected, na.rm = T),
    median_percent_sequenced_collected = median(percent_sequenced_collected, na.rm = T),
    mean_percent_sequenced_collected = mean(percent_sequenced_collected, na.rm = T),
    max_percent_sequenced_collected = max(percent_sequenced_collected, na.rm = T),
    sd_percent_sequenced_collected = sd(percent_sequenced_collected, na.rm = T),
    total_cases = sum(Confirmed),
    total_sequenced = sum(Sequenced)
  ) %>%
  mutate(total_percent_sequenced_collected = total_sequenced / total_cases * 100)


p4 <- PlotSequencedPropHeatmap(india_sequencing_proportion)
p4

state_seq_stats <- TotalSequencesPerMonthStatewise(gisaid_india, drop_country = TRUE)
seq_stats <- rbind(country_seq_stats, state_seq_stats)
state_cases_long <- GetIndiaConfirmedCasesMonthlyLong()
state_sequencing_proportion <- CombineSequencedCases(
  cases_sequenced = seq_stats,
  confirmed_long = state_cases_long,
  month.min = "Apr 2020",
  month.max = "Feb 2023",
  max.percent = 5
)

state_sequencing_proportion$State <- factor(
  x = state_sequencing_proportion$State,
  levels = as.character(GetIndianStates())
)

state_sequencing_proportion$Year <- stringr::str_split_fixed(state_sequencing_proportion$MonthYear, pattern = " ", n = 2)[, 2]
state_sequencing_proportion_median <- state_sequencing_proportion %>%
  #filter(!MonthYear %in% c("Jan 2020", "Feb 2020", "Mar 2020")) %>%
  group_by(State, Year) %>%
  summarise(
    min_percent_sequenced_collected = min(percent_sequenced_collected, na.rm = T),
    median_percent_sequenced_collected = median(percent_sequenced_collected, na.rm = T),
    mean_percent_sequenced_collected = mean(percent_sequenced_collected, na.rm = T),
    max_percent_sequenced_collected = max(percent_sequenced_collected, na.rm = T),
    sd_percent_sequenced_collected = sd(percent_sequenced_collected, na.rm = T),
    total_cases = sum(Confirmed, na.rm = T),
    total_sequenced = sum(Sequenced, na.rm = T)
  ) %>%
  mutate(total_percent_sequenced_collected = total_sequenced / total_cases * 100)


# 1. Look at median delay in 2020, 2021, 2023 across states

statewise_delay_yearwise <- gisaid_india %>%
  filter(YearCollected == YearSubmitted) %>%
  group_by(State, YearCollected) %>%
  filter(!is.na(YearCollected)) %>%
  summarise(
    n_seq = n(),
    min_delay = min(delay, na.rm = T),
    median_delay = median(delay, na.rm = T),
    mean_delay = mean(delay, na.rm = T),
    max_delay = max(delay, na.rm = T),
    sd_delay = sd(delay, na.rm = T)
  )


india_delay_yearwise <- gisaid_india %>%
  filter(YearCollected == YearSubmitted) %>%
  group_by(YearCollected) %>%
  filter(!is.na(YearCollected)) %>%
  summarise(
    n_seq = n(),
    min_delay = min(delay, na.rm = T),
    median_delay = median(delay, na.rm = T),
    mean_delay = mean(delay, na.rm = T),
    max_delay = max(delay, na.rm = T),
    sd_delay = sd(delay, na.rm = T)
  )

india_total_monthwise <- gisaid_india %>%
  group_by(MonthYearCollected) %>%
  filter(!is.na(YearCollected)) %>%
  summarise(
    n_seq = n(),
    min_delay = min(delay, na.rm = T),
    median_delay = median(delay, na.rm = T),
    mean_delay = mean(delay, na.rm = T),
    max_delay = max(delay, na.rm = T),
    sd_delay = sd(delay, na.rm = T)
  )

statewise_delay_yearwise_sel <- statewise_delay_yearwise %>% filter(YearCollected %in% c(2020, 2021, 2022))
india_delay_yearwise_sel <- india_delay_yearwise %>% filter(YearCollected %in% c(2020, 2021, 2022))

statewise_delay_yearwise_sel$YearFactor <- factor(statewise_delay_yearwise_sel$YearCollected, levels = c(2022, 2021, 2020))
statewise_delay_yearwise_sel$sd_delay[is.na(statewise_delay_yearwise_sel$sd_delay)] <- 0
statewise_delay_yearwise_sel$median_delay_max <- statewise_delay_yearwise_sel$median_delay + statewise_delay_yearwise_sel$sd_delay
statewise_delay_yearwise_sel$median_delay_min <- statewise_delay_yearwise_sel$median_delay - statewise_delay_yearwise_sel$sd_delay

ggplot(statewise_delay_yearwise_sel, aes(median_delay, State, fill = YearFactor)) +
  geom_bar(stat = "identity", position = "dodge")


theme_set(CovmullerTheme())
statewise_delay_yearwise_sel$State[statewise_delay_yearwise_sel$State == "Dadra and Nagar Haveli and Daman and Diu"] <- "DNH and DD"
# geom_errorbar(aes(ymin = percent_min, ymax = perc_max))

yend_2020 <- india_delay_yearwise_sel %>%
  filter(YearCollected == 2020) %>%
  pull(median_delay)
yend_2021 <- india_delay_yearwise_sel %>%
  filter(YearCollected == 2021) %>%
  pull(median_delay)
yend_2022 <- india_delay_yearwise_sel %>%
  filter(YearCollected == 2022) %>%
  pull(median_delay)

ggplot(statewise_delay_yearwise_sel, aes(State, median_delay, fill = YearFactor)) + # geom_bar(stat="identity", position="dodge")
  geom_linerange(aes(x = State, ymin = 0, ymax = median_delay, colour = YearFactor),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = State, y = median_delay, colour = YearFactor),
    position = position_dodge(width = 1)
  ) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  # geom_segment(aes(y=0, yend= yend_2020, x =0, xend=0),
  # linetype = "dashed",
  #  color = RColorBrewer::brewer.pal(3, "Dark2")[3]
  # ) +
  geom_hline(
    yintercept = yend_2020 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[3]
  ) +
  geom_hline(
    yintercept = yend_2021 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[2]
  ) +
  geom_hline(
    yintercept = yend_2022 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[1]
  ) +
  # annotate(
  #  geom = "curve", x = 36, y = yend_2020, xend = 38, yend = yend_2020+1,
  #  curvature = .3, arrow = arrow(length = unit(2, "mm")), color = RColorBrewer::brewer.pal(3, "Dark2")[3]
  # ) +
  # annotate(geom = "text", x = 38.5, y = yend_2020-3, label = "2020", hjust = "left", color = RColorBrewer::brewer.pal(3, "Dark2")[3]) +

  coord_flip(xlim = c(0, 37), clip = "off") +
  xlab("") +
  ylab("Median delay in days beween sample collection and submission to GISAID ") +
    labs(caption = paste0("**Source: gisaid.org; covid19bharat.org<br>** ", Sys.Date()))

statewise_delay_yearwise_sel$Year <- statewise_delay_yearwise_sel$YearFactor
ggplot(statewise_delay_yearwise_sel, aes(State, median_delay, fill = Year)) + # geom_bar(stat="identity", position="dodge")
  geom_linerange(aes(x = State, ymin = 0, ymax = median_delay, colour = Year),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = State, y = median_delay, colour = Year),
    position = position_dodge(width = 1)
  ) +
  geom_hline(
    yintercept = yend_2020 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[3]
  ) +
  geom_hline(
    yintercept = yend_2021 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[2]
  ) +
  geom_hline(
    yintercept = yend_2022 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[1]
  ) +
  facet_wrap(~YearFactor) +
  coord_flip(clip = "off") +
  scale_color_brewer(type = "qual", palette = "Dark2", name = "Year") +
  xlab("") +
  ylab("Median delay in days beween sample collection and submission to GISAID ") +
  #    geom_errorbar(aes(ymin = median_delay, ymax = median_delay_max)) +
  theme(strip.text = element_text(color = "black")) +
    labs(caption = paste0("**Source: gisaid.org<br>** ", Sys.Date()))

ggsave("plots/02_statewise_delay_wide.png", device = png, dpi = 300, width = 7.6, height = 8.5)
ggplot(statewise_delay_yearwise_sel, aes(State, n_seq, fill = Year)) + # geom_bar(stat="identity", position="dodge")
  geom_linerange(aes(x = State, ymin = 0, ymax = n_seq, colour = Year),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = State, y = n_seq, colour = YearFactor),
    position = position_dodge(width = 1)
  ) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  facet_wrap(~YearFactor) +
  coord_flip() +
  xlab("") +
  ylab("Total sequences deposited to GISAID") +
    labs(caption = paste0("**Source: gisaid.org<br>** ", Sys.Date()))
ggsave("plots/02_statewise_nseq.png", device = png, dpi = 300, width = 7.6, height = 8.5)

# coord_flip(xlim = c(0, 37), clip = "off") + xlab("" ) + ylab("Median delay in days beween sample collection and submission to GISAID ")


# 2. Look at mean/median percentage sequenced across samples
state_sequencing_proportion_median$YearFactor <- factor(state_sequencing_proportion_median$Year, levels = c(2022, 2021, 2020))
state_sequencing_proportion_median$Year <- state_sequencing_proportion_median$YearFactor

country_sequencing_proportion_median <- state_sequencing_proportion_median %>% filter(State == "India")
state_sequencing_proportion_median2 <- state_sequencing_proportion_median %>% filter(State != "India")
india_yend_2020 <- country_sequencing_proportion_median %>%
  filter(Year == 2020) %>%
  pull(total_percent_sequenced_collected)
india_yend_2021 <- country_sequencing_proportion_median %>%
  filter(Year == 2021) %>%
  pull(total_percent_sequenced_collected)
india_yend_2022 <- country_sequencing_proportion_median %>%
  filter(Year == 2022) %>%
  pull(total_percent_sequenced_collected)
state_sequencing_proportion_median2$State[state_sequencing_proportion_median2$State == "Dadra and Nagar Haveli and Daman and"]
state_sequencing_proportion_median2$State <- as.character(state_sequencing_proportion_median2$State)
state_sequencing_proportion_median2$State[state_sequencing_proportion_median2$State == "Dadra and Nagar Haveli and Daman and Diu"] <- "DNH and DD"

ggplot(state_sequencing_proportion_median2 %>% filter(!is.na(Year)), aes(State, total_percent_sequenced_collected, fill = Year)) + # geom_bar(stat="identity", position="dodge")
  geom_linerange(aes(x = State, ymin = 0, ymax = total_percent_sequenced_collected, colour = Year),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = State, y = total_percent_sequenced_collected, colour = Year),
    position = position_dodge(width = 1)
  ) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  coord_flip() +
  geom_hline(
    yintercept = india_yend_2020 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[3]
  ) +
  geom_hline(
    yintercept = india_yend_2021 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[2]
  ) +
  geom_hline(
    yintercept = india_yend_2022 + 1,
    linetype = "dashed",
    color = RColorBrewer::brewer.pal(3, "Dark2")[1]
  ) +
  facet_wrap(~Year) +
  ylab("Overall percentage of samples sequenced and deposited to GISAID") +
  xlab("") +
    labs(caption = paste0("**Source: gisaid.org; covid19bharat.org<br>** ", Sys.Date()))
ggsave("plots/02_statewise_overall_sequencing.png", device = png, dpi = 300, width = 7.6, height = 8.5)

state_sequencing_proportion_median$State <- as.character(state_sequencing_proportion_median$State)
state_sequencing_proportion_median$State[state_sequencing_proportion_median$State == "Dadra and Nagar Haveli and Daman and Diu"] <- "DNH and DD"

mindia_yend_2020 <- country_sequencing_proportion_median %>%
    filter(Year == 2020) %>%
    pull(median_percent_sequenced_collected)
mindia_yend_2021 <- country_sequencing_proportion_median %>%
    filter(Year == 2021) %>%
    pull(median_percent_sequenced_collected)
mindia_yend_2022 <- country_sequencing_proportion_median %>%
    filter(Year == 2022) %>%
    pull(median_percent_sequenced_collected)

ggplot(state_sequencing_proportion_median %>% filter(!is.na(YearFactor)), aes(State, median_percent_sequenced_collected, fill = Year)) + # geom_bar(stat="identity", position="dodge")
  geom_linerange(aes(x = State, ymin = 0, ymax = median_percent_sequenced_collected, colour = Year),
    position = position_dodge(width = 1)
  ) +
  geom_point(aes(x = State, y = median_percent_sequenced_collected, colour = Year),
    position = position_dodge(width = 1)
  ) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  coord_flip() +
    geom_hline(
        yintercept = mindia_yend_2020 + 1,
        linetype = "dashed",
        color = RColorBrewer::brewer.pal(3, "Dark2")[3]
    ) +
    geom_hline(
        yintercept = mindia_yend_2021 + 1,
        linetype = "dashed",
        color = RColorBrewer::brewer.pal(3, "Dark2")[2]
    ) +
    geom_hline(
        yintercept = mindia_yend_2022 + 1,
        linetype = "dashed",
        color = RColorBrewer::brewer.pal(3, "Dark2")[1]
    ) +
  facet_wrap(~YearFactor) +
  ylab("Median (over a year) percentage of samples sequenced and deposited to GISAID") +
  xlab("") +
    labs(caption = paste0("**Source: gisaid.org; covid19bharat.org<br>** ", Sys.Date()))
ggsave("plots/02_statewise_median_sequencing.png", device = png, dpi = 300, width = 7.6, height = 8.5)

# 3. Look at data from November 2022 - January 2023

shp <- read_sf("~/github/2021_Covid19_surveillance/data/maps/india.json")
state_names <- list(
  "AP" = "Andhra Pradesh",
  "AR" = "Arunachal Pradesh",
  "AS" = "Assam",
  "BR" = "Bihar",
  "CT" = "Chhattisgarh",
  "GA" = "Goa",
  "GJ" = "Gujarat",
  "HR" = "Haryana",
  "HP" = "Himachal Pradesh",
  "JH" = "Jharkhand",
  "KA" = "Karnataka",
  "KL" = "Kerala",
  "MP" = "Madhya Pradesh",
  "MH" = "Maharashtra",
  "MN" = "Manipur",
  "ML" = "Meghalaya",
  "MZ" = "Mizoram",
  "NL" = "Nagaland",
  "OR" = "Odisha",
  "PB" = "Punjab",
  "RJ" = "Rajasthan",
  "SK" = "Sikkim",
  "TN" = "Tamil Nadu",
  "TG" = "Telangana",
  "TR" = "Tripura",
  "UT" = "Uttarakhand",
  "UP" = "Uttar Pradesh",
  "WB" = "West Bengal",
  "AN" = "Andaman & Nicobar Islands",
  "CH" = "Chandigarh",
  "DD" = "Dadra & Nagar Haveli and Daman & Diu",
  "DL" = "NCT Delhi",
  "JK" = "Jammu & Kashmir",
  "LH" = "Ladakh",
  "LD" = "Lakshadweep",
  "PY" = "Puducherry",
  "IN" = "India"
)
state_names_df <- as.data.frame(t(data.frame(x = state_names)))
state_names_df$state_code <- gsub(pattern = "x\\.", replacement = "", x = rownames(x = state_names_df))
colnames(x = state_names_df)[1] <- "state"

state_region <- list(
  "AP" = "South",
  "AR" = "Northeast",
  "AS" = "Northeast",
  "BR" = "East",
  "CT" = "Central",
  "GA" = "South",
  "GJ" = "West",
  "HR" = "North",
  "HP" = "North",
  "JH" = "East",
  "KA" = "South",
  "KL" = "South",
  "MP" = "Central",
  "MH" = "West",
  "MN" = "Northeast",
  "ML" = "Northeast",
  "MZ" = "Northeast",
  "NL" = "Northeast",
  "OR" = "East",
  "PB" = "North",
  "RJ" = "North",
  "SK" = "Northeast",
  "TN" = "South",
  "TG" = "South",
  "TR" = "Northeast",
  "UT" = "North",
  "UP" = "North",
  "WB" = "East",
  "AN" = "South",
  "CH" = "North",
  "DD" = "West",
  "DL" = "North",
  "JK" = "North",
  "LH" = "North",
  "LD" = "South",
  "PY" = "South",
  "IN" = "India"
)
state_region_df <- as.data.frame(x = t(data.frame(x = state_region)))
state_region_df$state_code <- gsub(pattern = "x\\.", replacement = "", x = rownames(x = state_region_df))
colnames(x = state_region_df)[1] <- "region"
states <- shp %>%
  group_by(st_nm) %>%
  summarize(State = st_nm, geometry = st_union(geometry)) %>%
  ungroup() # %>%
states$state <- unlist(x = lapply(X = states$st_nm, FUN = get_state_name))


# 4. Heterogeneity in sequencing in 2020, 2021, 2022 (morans'I)
