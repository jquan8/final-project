install.packages("usethis")
library(usethis)
here::here()
getwd()
library(here)
load(here::here("covid_testing.rda"))
str(covid_testing)
###1 Create a {gtsummary} table of descriptive statistics about your data###
library(gtsummary)
tbl_summary(
	covid_testing,
	by = gender,
	include = c(gender, pan_day, drive_thru_ind, orderset, demo_group, age, ct_result,
							payor_group, result, patient_class, col_rec_tat, rec_ver_tat),
	label = list(
		pan_day ~ "day after start of pandemic",
		drive_thru_ind ~ "collected via a drive-thru site",
		orderset ~ "order set was used for test order",
		demo_group ~ "patient group",
		age ~ "patient age",
		ct_result ~ "cycle threshold",
		payor_group ~ "payor",
		patient_class ~ "patient disposition",
		col_rec_tat ~ "time between collect and receive",
		rec_ver_tat ~ "time between receive and verification"
	),
	missing_text = "Missing")
### 2 Fit a regression and present well-formatted results from the regression###
linear_model <- lm(pan_day ~ gender + result + age,
									 data = covid_testing)
tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		gender ~ "Sex",
		result ~ "Test result",
		age ~ "Patient age"
	))
logistic_model <- glm(drive_thru_ind ~ payor_group + gender + age,
											data = covid_testing, family = binomial())
tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		gender ~ "Sex",
		payor_group ~ "payor",
		age ~ "Patient age"
	))
### 3 Create a figure###

hist (covid_testing $ age)

### 4 Write and use a function that does something with the data###
x<-(covid_testing$age)
new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}
new_mean(x)
### 7 Use the {renv} package to make sure the teaching team can reproduce your results with the same package versions###
library(renv)
renv::init()
