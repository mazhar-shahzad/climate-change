---
title: "Empirical Project 11: Working in R code"
---

# Empirical Project 11 Working in R

These code downloads have been constructed as supplements to the full Doing Economics projects (https://core-econ.org/doing-economics/). You'll need to download the data before running the code that follows. 

## Getting started in R

For this project you will need the following packages:

-   `tidyverse`, to help with data manipulation
-   `readxl`, to import an Excel spreadsheet
-   `knitr`, to format tables
-   `psych`, to compute Cronbach's alpha.

If you need to install any of these packages, run the following code:

```{r}
install.packages(c("tidyverse", "readxl", "knitr", "psych"))
```

You can import the libraries now, or when they are used in the R walk-throughs below.

```{r}
library(tidyverse)
library(readxl)
library(knitr)
library(psych)
```

## Part 11.1 Summarizing the data

### R walk-through 11.1 Importing data and recoding variables

Before importing data in Excel or .csv format, open it in a spreadsheet program (such as Excel) to ensure you understand the structure of the data and check if any additional options are required for the `read_excel` function in order to import the data correctly. In this case, the data is in a worksheet called ‘Data’, there are no missing values to worry about, and the first row contains the variable names. This format is straightforward to import. We can therefore import the data using the `read_excel` function without any additional options. 

```{r}
library(tidyverse)
library(readxl)
library(knitr)

# Set working directory
setwd("YOURFILEPATH")

WTP <- read_excel(
  "Project 11 datafile.xlsx", sheet = "Data")
```

#### Reverse-code variables

The first task is to recode variables related to the respondents’ views on certain aspects of government behaviour and attitudes about global warming (`cog_2`, `cog_5`, `scepticism_6`, and `scepticism_7`). This coding makes the interpretation of high and low values consistent across all questions, since the survey questions do not have this consistency. 

To recode all of these variables in one go, we use the piping operator (`%>%`), which can perform the same sequence of commands on a number of variables at once. (For a more detailed introduction to piping, see the University of Manchester’s Econometric Computing Learning Resource (https://tinyco.re/5531433).) Note that even though the value of 3 for these variables will stay the same, for the `recode` function to work properly we have to specify how each new value corresponds to a previous value. 

```{r}
WTP <- WTP %>%
  mutate_at(c("cog_2", "cog_5", 
    "scepticism_6", "scepticism_7"),
    funs(recode(., "1" = 5, "2" = 4, "3" = 3, 
      "4" = 2, "5" = 1)))
```

#### Create new variables containing WTP amounts

Although we could employ the same technique as above to recode the value for the minimum and maximum willingness to pay variables, an alternative is to use the `merge` function. This function allows us to combine two dataframes via values given in a particular variable.


We start by creating a new dataframe (`category_amount`) that has two variables: the original category value and the corresponding new euro amount. We then apply the `merge` function to the WTP dataframe and the new dataframe, specifying the variables that link the data in each dataframe together (`by.x` indicates which variable in the first dataframe, here `WTP`, is to be matched to `by.y`, the variable in the second dataframe, here `category_amount`). We also use the `all.x = TRUE` option to keep all observations, otherwise the `merge` function will drop any observations with missing values for the `WTP_plmin` and `WTP_plmax` variables. Finally we have to rename the column of the merged new values to something more meaningful (`WTP_plmin_euro` and `WTP_plmax_euro` respectively). 


```{r}
# Vector containing the Euro amounts
wtp_euro_levels <- c(48, 72, 84, 108, 156, 192, 252, 324, 
  432, 540, 720, 960, 1200, 1440)

# Create mapping dataframe
category_amount <- data.frame(original = 1:14, 
  new = wtp_euro_levels)

# Create a new variable for the minimum WTP
WTP <- merge(WTP, category_amount, 
  by.x = "WTP_plmin", by.y = "original", 
  all.x = TRUE) %>%
  rename(., "WTP_plmin_euro" = "new")

# Create a new variable for the maximum WTP
WTP <- merge(WTP, category_amount, 
  by.x = "WTP_plmax", by.y = "original", all.x = TRUE) %>%
  rename(., "WTP_plmax_euro" = "new")
```

[End of walk-through]


### R walk-through 11.2 Creating indices

We can create all of the required indices in three steps using the `rowMeans` function. In each step we use the `cbind` function to join the required variables (columns) together as a matrix. As the data is stored as a single observation per row, the index value is the average of the values in each row of this matrix, which we calculate using the `rowMeans` function.

```{r}
WTP <- WTP %>%
  # Ensure subsequent operations are applied by row
  rowwise() %>%
  mutate(., climate = rowMeans(cbind(
    scepticism_2, scepticism_6, scepticism_7))) %>%
  mutate(., gov_intervention = rowMeans(cbind(
    cog_1, cog_2, cog_3, cog_4, cog_5, cog_6))) %>%
  mutate(., pro_environment  = rowMeans(cbind(
    PN_1, PN_2, PN_3, PN_4, PN_6, PN_7))) %>%
  # Return the dataframe to the original format
  ungroup()
```

[End of walk-through]


### R walk-through 11.3 Calculating correlation coefficients

#### Calculate correlation coefficients and Cronbach's alpha

We covered calculating correlation coefficients in R walk-through 10.1. In this case, since there are no missing values we can use the `cor` function without any additional options.

For the questions on climate change:

```{r}
cor(cbind(WTP$scepticism_2, WTP$scepticism_6, 
  WTP$scepticism_7))
```

For the questions on government behaviour:

```{r}
cor(cbind(WTP$cog_1, WTP$cog_2, WTP$cog_3, 
  WTP$cog_4, WTP$cog_5, WTP$cog_6))
```

For the questions on personal behaviour:

```{r}
cor(cbind(WTP$PN_1, WTP$PN_2, WTP$PN_3, 
  WTP$PN_4, WTP$PN_6, WTP$PN_7))
```

#### Calculate Cronbach’s alpha

It is straightforward to compute the Cronbach’s alpha using the `alpha` function from the `psych` package. This function calculates Cronbach’s alpha and stores it in `$total$std.alpha`.

```{r}
psych::alpha(WTP[c("scepticism_2", 
  "scepticism_6", "scepticism_7")])$total$std.alpha
```

```{r}
psych::alpha(WTP[c("cog_1", "cog_2", "cog_3", 
  "cog_4", "cog_5", "cog_6")])$total$std.alpha
```

```{r}
psych::alpha(WTP[c("PN_1", "PN_2", "PN_3", 
  "PN_4", "PN_6", "PN_7")])$total$std.alpha
```

[End of walk-through]


### R walk-through 11.4 Using loops to obtain summary statistics

The two different formats (DC and TWPL) are recorded in the variable `abst_format`, and take the values `ref` and `ladder` respectively. We will store all the variables of interest into a list called `variables`, and use a ‘for’ loop to calculate summary statistics for each variable and present it in a table. 

```{r}
variables <- list(quo(sex), quo(age),
  quo(kids_nr), quo(hhnetinc),
  quo(member), quo(education))

for (i in seq_along(variables)){
  WTP %>%
    group_by(abst_format, !!variables[[i]]) %>%
    summarize (n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    select(-n) %>%
    spread(abst_format, freq) %>%
    print()
}
```

The output above gives the required tables, but is not easy to read. You may want to tidy up the results, for example by translating (from German to English) and reordering the options in the household net income variable (`hhnetinc`).

[End of walk-through]


### R walk-through 11.5 Calculating summary statistics

The `summarize_at` function can provide multiple statistics for a number of variables in one command. Simply provide a list of the variables you want to summarize and then use the `funs()` option to specify the summary statistics you need. Here, we need the `mean`, `sd`, `mean`, and `max` for the variables `climate`, `gov_intervention`, and `pro_environment`. 

```{r}
WTP %>%
  group_by(abst_format) %>%
  summarise_at(c("climate", "gov_intervention", 
      "pro_environment"),
    funs(mean, sd, min, max)) %>%
  # Use gather and spread functions to reformat output 
  # for aesthetic reasons
  gather(index, value, 
    climate_mean:pro_environment_max) %>%
  spread(abst_format, value) %>%
  kable(., format = "markdown", digits = 2)
```

[End of walk-through]

## Part 11.2 Comparing willingness to pay across methods and individual characteristics

### R walk-through 11.6 Summarizing willingness to pay variables

#### Create column charts for minimum and maximum WTP

Before we can plot a column chart, we need to compute frequencies (number of observations) for each value of the willingness to pay (1–14). We do this separately for the minimum and maximum willingness to pay.

In each case we select the relevant variable and remove any observations with missing values using the `na.omit` function. We can then separate the data by level (WTP amount) of the `WTP_plmin_euro` or `WTP_PLmax_euro` variables (using `group_by`), then obtain a frequency count using the `summarize` function. We also use the `factor` function to set this variable’s type to factor, to get the correct horizontal axis labels in the column chart. 

Once we have the frequency count stored as a dataframe, we can plot the column charts.

For the minimum willingness to pay:

```{r}
df.plmin <- WTP %>%
  select(WTP_plmin_euro) %>%
  na.omit() %>%
  group_by(WTP_plmin_euro) %>%
  summarize(n = n()) %>%
  mutate(WTP_plmin_euro = factor(WTP_plmin_euro, 
    levels = wtp_euro_levels))

ggplot(df.plmin, aes(WTP_plmin_euro, n)) +
  geom_bar(stat = "identity", position = "identity") + 
  xlab("Minimum WTP (euros)") + 
  ylab("Frequency") +
  theme_bw()
```

For the maximum willingness to pay:

```{r}
df.plmax <- WTP %>%
  select(WTP_plmax_euro) %>%
  na.omit() %>%
  group_by(WTP_plmax_euro) %>%
  summarize(n = n()) %>%
  mutate(WTP_plmax_euro = factor(WTP_plmax_euro, 
    levels = wtp_euro_levels))

ggplot(df.plmax, aes(WTP_plmax_euro, n)) +
  geom_bar(stat = "identity", position = "identity") + 
  xlab("Maximum WTP (euros)") + 
  ylab("Frequency") +
  theme_bw()
```

#### Calculate average WTP for each individual

We can use the `rowMeans` function to obtain the average of the minimum and maximum willingness to pay.

```{r}
WTP <- WTP %>%
  rowwise() %>%
  mutate(., WTP_average = rowMeans(cbind(
    WTP_plmin_euro, WTP_plmax_euro))) %>%
  ungroup()
```

#### Calculate mean and median WTP across individuals

The mean and median of this average value can be obtained using the `mean` and `median` functions, although we have to use the `na.rm = TRUE` option to handle missing values correctly.

```{r}
mean(WTP$WTP_average, na.rm = TRUE)
```

```{r}
median(WTP$WTP_average, na.rm = TRUE)
```

#### Calculate correlation coefficients

We showed how to obtain a matrix of correlation coefficients for a number of variables in R walk-through 8.8. We use the same process here, storing the coefficients in an object called `M`.

```{r}
WTP %>%
  # Create the gender variable
  mutate(gender = 
    as.numeric(ifelse(sex == "female", 0, 1))) %>%
  select(WTP_average, education, gender,
    climate, gov_intervention, pro_environment) %>%
  cor(., use = "pairwise.complete.obs") -> M

M[, "WTP_average"]
```

[End of walk-through]


### R walk-through 11.7 Summarizing Dichotomous Choice (DC) variables

#### Create frequency table for DC_ref_outcome

We can group by `costs` and `DC_ref_outcome` to obtain the number of observations for each combination of amount and vote response. We can also `recode` the voting options to ‘Yes’, ‘No’, and ‘Abstain’. 

```{r}
WTP_DC <- WTP %>%
  group_by(costs, DC_ref_outcome) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate_at("DC_ref_outcome", 
    funs(recode(., 
      "do not support referendum and no pay" = "No",
      "support referendum and pay" = "Yes",
      "would not vote" = "Abstain"))) %>%
  spread(DC_ref_outcome, n)

kable(WTP_DC, format = "markdown", digits = 2)
```

#### Add column showing proportion voting yes or no

We can extend the table from Question 2*(a)* to include the proportion voting yes or no (to obtain percentages, multiply the values by 100).

```{r}
WTP_DC <- WTP_DC %>%
  mutate(total = Abstain + No + Yes, 
    prop_no = (Abstain + No) / total, 
    prop_yes = Yes / total) %>%
  # Round all numbers to 2 decimal places
  mutate_if(is.numeric, funs(round(., 2)))

kable(WTP_DC, format = "markdown", digits = 2)
```

#### Make a line chart of WTP

Using the dataframe generated for Questions 2*(a)* and *(b)* (`WTP_DC`), we can plot the ‘demand curve’ as a scatterplot with connected points by using the `geom_point` and `geom_line` options for `ggplot`. Adding the extra option `scale_x_continuous` changes the default labeling on the horizontal axis to display ticks at every 100 euros, enabling us to read the chart more easily. 

```{r}
p <- ggplot(WTP_DC, aes(y = prop_yes, x = costs)) +
  geom_point() + 
  geom_line(size = 1) +
  ylab("% Voting 'Yes'") + 
  xlab("Amount (euros)") +
  scale_x_continuous(breaks = seq(0, 1500, 100)) +
  theme_bw()

print(p)
```

#### Calculate new proportions and add them to the table and chart

It is straightforward to calculate the new proportions and add them to the existing dataframe, however, we will need to reshape the data (using `gather`) to plot multiple lines on the same scatterplot. 

```{r}
WTP_DC <- WTP_DC %>%
  mutate(total_ex = No + Yes, 
    prop_no_ex = No / total_ex, 
    prop_yes_ex = Yes / total_ex) %>%
  # Round all numbers to 2 decimal places
  mutate_if(is.numeric, funs(round(., 2)))

kable(WTP_DC, format = "markdown", digits = 2)
```

```{r}
WTP_DC %>%
  select(costs, prop_yes, prop_yes_ex) %>%
  gather(Vote, value, prop_yes:prop_yes_ex) %>%
  ggplot(., aes(y = value, x = costs, color = Vote)) + 
    geom_line(size = 1) + 
    geom_point() +
    ggtitle("'Demand curve' from DC respondents, under 
      different treatments for 'Abstain' responses.") +
    scale_color_manual(values = c("blue", "red"), 
      labels = c("counted as no", "excluded")) +
    ylab("% voting 'yes'") + 
    xlab("Costs (euros)") +
    theme_bw()
```

[End of walk-through]


### R walk-through 11.8 Calculating confidence intervals for differences in means

#### Calculate the difference in means, standard deviations, and number of observations

We first create two vectors that will contain the WTP values for each of the two question methods. For the DC format, willingness to pay is recorded in the `costs` variable, so we select all observations where the `DC_ref_outcome` variable indicates the individual voted 'yes' and drop any missing observations. For the TWPL format we use the `WTP_average` variable that we created in R walk-through 11.6.

```{r}
DC_WTP <- WTP %>% subset(
  DC_ref_outcome == "support referendum and pay") %>%
  select(costs) %>%
  filter(!is.na(costs)) %>%
  as.matrix()

# Print out the mean, sd, and count
cat(sprintf("DC Format - mean: %.1f, 
  standard deviation %.1f, count %d\n",
  mean(DC_WTP), sd(DC_WTP), length((DC_WTP))))
```

```{r}
TWPL_WTP <- WTP %>%
  select(WTP_average) %>%
  filter(!is.na(WTP_average)) %>%
  as.matrix()

cat(sprintf("TWPL Format - mean: %.1f, 
  standard deviation %.1f, count %d\n", 
  mean(TWPL_WTP), sd(TWPL_WTP),
  length((TWPL_WTP))))
```

#### Calculate 95% confidence intervals

Using the `t.test` function to obtain 95% confidence intervals was covered in R walk-throughs 8.10 and 10.6. As we have already separated the data for the two different question formats in Question 3(*a*), we can obtain the confidence interval directly.

```{r}
t.test(DC_WTP, TWPL_WTP, conf.level = 0.05)$conf.int
```

#### Calculate median WTP for the DC format

In R walk-through 11.6 we obtained the median WTP for the TWPL format (132). We now obtain the WTP using the DC format.

[End of walk-through]

# climate-change
