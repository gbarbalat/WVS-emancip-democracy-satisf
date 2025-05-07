rm(list=ls())
library(dplyr)
library(gtsummary)
library(tidyr)
library(ggplot2)
library(ggplotify)
library(here)
library(stringr)
library(lme4)
library(lmerTest)
library(effects)


#hdr ----
filtered <- "none" #none high or low
filtering <- "none" #none gdp ldi democracy Y020
add_region <- ifelse(filtering=="ldi" & filtered=="high", "region", "")
add_region <- ""

# variable codes
# S002VS==survey_anal
# X003,X003R,X001,X049,X007,G027B,#age, sex, settlment size, martial status, citizen
# X026,X011,X025R,X028,#live w/parents,children,education,employment
# X047_WVS,X047R_WVS,X047CS,#income
# F025,#belong to religious denomination
# H008_01,H008_02,H008_03,#no food, crime, no medicine
# A008,A009,A170,#happiness, health, satisfaction
# E224:E229,E233,E233A,E233B, E235,#QS re-democracy
# Y010,Y020,#Wezel secular and emancipative values
# A098:A106,#active memberships
# S002VS,S022,COUNTRY_ALPHA, S003) #wave, year-month, countryname or code,


rfx <- "(1+Y020|COUNTRY_ALPHA)" # random slope and intercept
#which wave of the WVS
survey_anal <- 7
#using gdp or ldi as a covariate
gdp_ldi <- "ldi"; rm_gdp_ldi <- "gdp"
#using liberal democracy vs. other form of democracies
which_dem <- "v2x_libdem";rm_dem <- c("v2x_partipdem", "v2x_delibdem", "v2x_egaldem")
which_IA <- which_dem
#using democracy values x year prior to the 7th wave of the WVS
how_many_yr_prior <- 1

#load data ----
load(here::here("data",paste0("ultimate_db_",survey_anal,".RData")))

#filtering
if (filtering == "gdp") {
  # Use GDP-related variable for filtering
  filter_variable <- "TDC_1yrprior_gdp"
} else if (filtering == "ldi")  {
  # Use LDI-related variable for filtering
  filter_variable <- "TDC_1yrprior_ldi"
} else if (filtering == "democracy")  {
  # Use LDI-related variable for filtering
  filter_variable <- paste0("TDC_1yrprior_",which_dem)
} else if (filtering == "Y020")  {
  # Use LDI-related variable for filtering
  filter_variable <- "Y020"
}

if (filtered == "high") {
  ultimate_db_filtered <- ultimate_db %>%
    #filter(.data[[filter_variable]] > median(ultimate_db[[filter_variable]]))
    filter(.data[[filter_variable]] > quantile(!!sym(filter_variable), 0.75))
  
} else if (filtered == "low") {
  ultimate_db_filtered <- ultimate_db %>%
    #filter(.data[[filter_variable]] <= median(ultimate_db[[filter_variable]]))
    filter(.data[[filter_variable]] <= quantile(!!sym(filter_variable), 0.75))
  
} else {
  ultimate_db_filtered <- ultimate_db
}

#make A170 (our outcome) numeric
ultimate_db_filtered$A170 <- as.numeric(ultimate_db_filtered$A170 )


# change some of the ordered factors
ultimate_db_filtered$Act_Mb <- factor(ultimate_db_filtered$Act_Mb , ordered = FALSE)
ultimate_db_filtered$X003R <- factor(ultimate_db_filtered$X003R, ordered = FALSE)
ultimate_db_filtered$X049 <- factor(ultimate_db_filtered$X049, ordered = FALSE)
ultimate_db_filtered$X011 <- factor(ultimate_db_filtered$X011, ordered = FALSE) 
ultimate_db_filtered$X025R <- factor(ultimate_db_filtered$X025R, ordered = FALSE)
ultimate_db_filtered$X047_WVS <- factor(ultimate_db_filtered$X047_WVS, ordered = FALSE)
ultimate_db_filtered$H008_01 <- factor(ultimate_db_filtered$H008_01, ordered = FALSE)
ultimate_db_filtered$H008_02 <- factor(ultimate_db_filtered$H008_02, ordered = FALSE)
ultimate_db_filtered$H008_03 <- factor(ultimate_db_filtered$H008_03, ordered = FALSE)
#str(ultimate_db_filtered)

#For some ordinal contrast, make linear
ultimate_db_filtered$Act_Mb <- as.numeric(ultimate_db_filtered$Act_Mb)
ultimate_db_filtered$X047_WVS <- as.numeric(ultimate_db_filtered$X047_WVS)
ultimate_db_filtered$H008_01 <- as.numeric(ultimate_db_filtered$H008_01)
ultimate_db_filtered$H008_03 <- as.numeric(ultimate_db_filtered$H008_03)

# Table 1 ----

# Create 4 bins of the continuous variable
ultimate_db_filtered_T1 <- ultimate_db_filtered %>%
  mutate(across(c(X047_WVS, H008_01, H008_03,Act_Mb),~factor(.))) %>%
  mutate(dem_bins = cut(!!sym(paste0("TDC_1yrprior_", which_dem)), 
                            breaks = 4, labels = c("Q1", "Q2", "Q3", "Q4")))

# Create the table
tbl_each <- ultimate_db_filtered_T1 %>%
  dplyr::select(paste0("TDC_1yrprior_", which_dem), TDC_1yrprior_ldi, 
                         !starts_with("TDC_"),
                  -id, -COUNTRY_ALPHA, -Y010) %>%
  as.data.frame() %>%
  tbl_summary(
    by = dem_bins,
    percent = "column",
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) %>%
  add_overall() %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()

# Display the table
tbl_each


#show countries in different dem bins
result <- ultimate_db_filtered_T1 %>%
  group_by(dem_bins) %>%
  summarise(countries = list(unique(COUNTRY_ALPHA))) %>%
  mutate(country_count = sapply(countries, length))

print(result)
for (i in 1:nrow(result)) {
  cat("Dem bin:", result$dem_bins[i], "\n")
  cat("Countries:", paste(result$countries[[i]], collapse = ", "), "\n\n")
}

# Custom function to modify percentages in the overall column
modify_overall_percentages <- function(x) {
  # Extract the overall column
  overall_col <- x$table_body %>% 
    filter(label == "stat_0") %>% 
    pull(stat_0)
  
  # Convert percentages to numeric and calculate new percentages
  overall_percentages <- as.numeric(sub("%", "", overall_col))
  new_percentages <- overall_percentages / sum(overall_percentages, na.rm = TRUE) * 100
  
  # Replace the old percentages with new ones
  x$table_body <- x$table_body %>%
    mutate(stat_0 = ifelse(label == "stat_0", 
                           paste0(round(new_percentages, 1), "%"), 
                           stat_0))
  return(x)
}

# Create the table
tbl_overall <- ultimate_db_filtered_T1 %>%
  dplyr::select(paste0("TDC_1yrprior_", which_dem), TDC_1yrprior_ldi, 
                !starts_with("TDC_"),
                -id, -COUNTRY_ALPHA, -Y010) %>%
  as.data.frame() %>%
  tbl_summary() %>%
  modify_header(label = "**Variable**") %>%
  bold_labels() 

# Display the table
tbl_overall



# multivariate cs anal ----
cols_to_keep <- c(
  # Columns that don't contain "yrprior"
  names(ultimate_db_filtered)[!grepl("yrprior", names(ultimate_db_filtered))],
  
  # Columns that specifically contain "1yrprior"
  names(ultimate_db_filtered)[grepl("_1yrprior", names(ultimate_db_filtered))]
)
cols_to_keep

# Create ultimate_2021_d by selecting the columns to keep
ultimate_2021 <- ultimate_db_filtered %>% dplyr::select(all_of(cols_to_keep)) %>%
  #scale ldi/gdp
  mutate(across(matches("ldi|gdp"), scale))

colSums(is.na(ultimate_2021))

# Get all column names from ultimate_2021_MM
all_columns <- names(ultimate_2021)

# Remove the columns we don't want in the formula
columns_to_remove <- c("A170","A170_binomial","A008","A009", 
                       "id", "COUNTRY_ALPHA", add_region,
                       "Y010",#"Y020",
                       "TDC_1yrprior_e_wb_pop","TDC_1yrprior_e_pelifeex", 
                       paste0("TDC_1yrprior_",rm_gdp_ldi),
                       paste0("TDC_1yrprior_",rm_dem),
                       
                       #"TDC_1yrprior_ldi",
                       #"TDC_1yrprior_v2x_libdem",
                       "TDC_1yrprior_v2xeg_eqprotec","TDC_1yrprior_v2xeg_eqaccess",
                       "TDC_1yrprior_v2xeg_eqdr",
                       "TDC_1yrprior_haqi", "TDC_1yrprior_urban", "TDC_1yrprior_fertility",
                       "TDC_1yrprior_educ","TDC_1yrprior_war"
)
formula_columns <- setdiff(all_columns, columns_to_remove)

# Create the formula string
formula_str <- paste0("A170 ~ ", rfx,"+", paste(formula_columns, collapse = " + "))

# Construct the new interaction term as a string
interaction_term <- paste0("TDC_1yrprior_", which_IA, ":Y020")

# Combine the existing formula string with the new interaction term
new_formula_string <- paste(formula_str, "+", interaction_term)

# Convert the combined string back to a formula object
formula <- as.formula(new_formula_string)

# Print the updated formula to verify
print(formula)

model <- lmer(formula, data = ultimate_2021)
summary(model) %>% print

#you can thencalculate SMD (Cohen's d) by dividing the effect by sd(ultimate_2021$A170)

#gives slopes at different levels of the moderator
interactions::interact_plot(model=model, 
                            pred=Y020, 
                            modx=TDC_1yrprior_v2x_libdem, 
                            modx.values=c(0,0.1,0.3,0.5,0.7,0.9,1)
)

slopes <- interactions::sim_slopes(model=model, pred=Y020, modx=TDC_1yrprior_v2x_libdem, 
                                   modx.values=c(0,0.1,0.3,0.5,0.7,0.9,1),
                                   inplot=TRUE)

#temporal (for supplementary figure) ----

## multivariate cs anal ----
#chge how many year prior to compute interaction for different values of how_many_year_prior

run_tp_multiv <- function(how_many_yr_prior) {
  pattern <- paste0("TDC_",20:(how_many_yr_prior+1),"yrprior")
  
  # Use select() with matches() to exclude columns matching the pattern
  ultimate_db_filtered_sel <- ultimate_db_filtered %>%
    select(-contains(pattern))
  
  cols_to_keep <- ultimate_db_filtered_sel %>%
    colnames
  cols_to_keep
  
  # Create ultimate_yrprior by selecting the columns to keep
  ultimate_yrprior_sel <- ultimate_db_filtered_sel %>% dplyr::select(all_of(cols_to_keep))
  
  ultimate_yrprior_long_sel <- ultimate_yrprior_sel %>%
    # Select columns that match the pattern and other columns
    dplyr::select(matches("^TDC_\\d+_yrprior_"), everything()) %>%
    # Pivot longer the TDC columns
    pivot_longer(
      cols = starts_with("TDC_"),
      names_to = c("yrprior", ".value"),
      names_pattern = "TDC_(\\d+)yrprior_(.+)"
    ) %>%
    #scale ldi/gdp
    mutate(across(matches("ldi|gdp"), scale)) %>%
    # Convert yrprior to numeric
    mutate(yrprior = as.numeric(yrprior))
  
all_columns <- names(ultimate_yrprior_long_sel)

# Remove the columns we don't want in the formula
columns_to_remove <- c("A170", "A170_binomial","A008","A009", 
                       "id", "COUNTRY_ALPHA", add_region,
                       "Y010",#"Y020",
                       "e_wb_pop","e_pelifeex", 
                       rm_gdp_ldi,
                       rm_dem,
                       "v2xeg_eqprotec","v2xeg_eqaccess",
                       "v2xeg_eqdr",
                       "haqi", "urban", "fertility",
                       "educ","war"
)

formula_columns <- setdiff(all_columns, columns_to_remove)

# Create the formula string
formula_str <- paste0("A170 ~ ", rfx,"+", paste(formula_columns, collapse = " + "))

# Construct the new interaction term as a string
interaction_term <- paste0(which_IA, "*Y020 + yrprior")

# Combine the existing formula string with the new interaction term
new_formula_string <- paste(formula_str, "+", interaction_term)

# Convert the combined string back to a formula object
formula <- as.formula(new_formula_string)

# Print the updated formula to verify
print(formula)

model <- lmer(formula, data = ultimate_yrprior_long_sel)

return(list(model, ultimate_yrprior_long_sel))

}
result <- run_tp_multiv(how_many_yr_prior=how_many_yr_prior)


## plot of IA effects ----

model <- result[[1]]
ultimate_yrprior_long_sel <- result[[2]]

slopes <- interactions::sim_slopes(model=model, pred=Y020, modx=v2x_libdem,#modx=!!sym(which_dem), 
                                   modx.values=c(0,0.1,0.3,0.5,0.7,0.9,1),
                                   inplot=TRUE)
slopes
df_slopes <- slopes$slopes %>%
  rename(`Liberal Democracy`="Value of v2x_libdem",
         `Slope of Emancipative values`="Est.")
g_slope <- ggplot(df_slopes, aes(x=`Slope of Emancipative values`, y=`Liberal Democracy`)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`, 
                    color = (`2.5%` > 0 | `97.5%` < 0)), 
                width = 0) +
  geom_point(aes(color = (`2.5%` > 0 | `97.5%` < 0))) +
  scale_color_manual(values = c("black", "red")) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        axis.title.y = element_text(size = 15, angle = 90, vjust = NULL),
        axis.title.x = element_text(size = 15, vjust = NULL),
        strip.text = element_text(size = 15),
        panel.background = element_rect(fill = "white", colour = "black"),
        strip.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(colour = "black", face = "bold", size = 15, 
                                  hjust = 0.5, vjust = -5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line()
  )
save(g_slope,file=paste0("g_slope_",how_many_yr_prior,".RData"))

#NOw load each plot (one for each how_many_yr_prior) and plot all in one graph using patchwork
p <- list()
for (i in 1:9) {
  load(paste0("g_slope_",i+1,".RData"));p[[i]] <- g_slope; rm(g_slope)
}

final_p <- p[[1]]+p[[2]]+
  p[[3]]+
  p[[4]]+p[[5]]+p[[6]]+
  p[[7]]+p[[8]]+p[[9]]+
  patchwork::plot_layout(
  axis_titles = "collect", guides = "collect",nrow=3,ncol=3) 

print(final_p)