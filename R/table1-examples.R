library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat, #must include this variable in the include line too!
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(ends_with("cat"), glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |> #this |> is a new built-in pipe %>%
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")


#In-class activity
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, region_cat, race_eth_cat,
							income, starts_with("sleep")),
	label = list(
		region_cat ~ "Region",
		race_eth_cat ~ "Race/ethnicity",
		income ~ "Income",
		sleep_wkdy ~ "Hours of sleep (Weekday)",
		sleep_wknd ~ "Hours of sleep (Weekend)"
	),
	missing_text = "Missing",
	statistic = list(income ~ "10th percentile=${p10}; 90th percentile=${p90}",
									 starts_with('sleep')~ "min = {min}; max = {max}"),
	digits = list(income ~ c(2,2),
								starts_with('sleep') ~ c(0,0))
	) |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_header(label = "**Variable**", p.value = "**P**") |>
	modify_footnote(update = everything() ~ NA) |>
	modify_table_styling(
			columns = label,
			rows = label =="Race/ethnicity",
			footnote = "See https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
	)
