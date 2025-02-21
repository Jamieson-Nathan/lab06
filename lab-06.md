Lab 06 - Ugly charts and Simpson’s paradox
================
Insert your name here
Insert date here

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 

staff <- read_csv("data/instructional-staff.csv")
```

### 1. Include the line plot you made above in your report and make sure the figure width is large enough to make it legible (also fix the title, axis labels, and legend label)

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line()+
  theme_bw()+
  labs(
    x = "Year",
    y = "Percent of Total Instructional Staff",
    title = "Trends in Instructional Staff Employee Status",
    color = "Faculty Status"
  )
```

![](lab-06_files/figure-gfm/graph-1.png)<!-- -->

### 2. Suppose the objective of this plot was to show that the proportion of part-time faculty have gone up over time compared to other instructional staff types.

``` r
# Adjusting the data for visualization
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year", values_to = "value") %>%
  mutate(
    value = as.numeric(value)
  )

# Creating the plot with a legend
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type  # Use faculty_type directly for coloring
  )) +
  geom_line(size = 1) +  # Adjust line width for clarity
  scale_color_manual(  # Define custom colors for each faculty type
    values = c(
      "Part-Time Faculty" = "firebrick",
      "Full-Time Tenured Faculty" = "navy",
      "Full-Time Tenure-Track Faculty" = "navy",
      "Full-Time Non-Tenure-Track Faculty" = "navy",
      "Graduate Student Employees" = "darkgrey"
    )
  ) +
  theme_classic() +  # Use a classic theme for a clean look
  labs(
    title = "Trend Analysis of Instructional Staff Types Over Time",
    x = "Academic Year",
    y = "Percentage of Instructional Staff",
    color = "Faculty Type"  # Properly label the legend
  ) +
  geom_text(aes(label = ifelse(year == "2019", faculty_type, NA)), hjust = 1.1, vjust = 0)  # Add labels for the latest year
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 55 rows containing missing values or values outside the scale range
    ## (`geom_text()`).

![](lab-06_files/figure-gfm/revised-plot-1.png)<!-- -->

I adjusted the graph to improve clarity and interpretation. By linking
colors to faculty types and using a legend, it’s easier to differentiate
the data. Custom colors emphasize trends, particularly the increase in
part-time faculty. Labels on the latest data point highlight recent
trends without cluttering the graph, ensuring the visualization is
informative and visually appealing.

### 3. Fisheries

These visuals are very confusing and not super informative…

``` r
# Load necessary libraries
library(ggplot2)
library(readr)
library(dplyr)  # For data manipulation

# Load the data
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Check the structure of the data
str(fisheries)
```

    ## spc_tbl_ [216 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ country    : chr [1:216] "Afghanistan" "Albania" "Algeria" "American Samoa" ...
    ##  $ capture    : num [1:216] 1000 7886 95000 3047 0 ...
    ##  $ aquaculture: num [1:216] 1200 950 1361 20 0 ...
    ##  $ total      : num [1:216] 2200 8836 96361 3067 0 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   country = col_character(),
    ##   ..   capture = col_double(),
    ##   ..   aquaculture = col_double(),
    ##   ..   total = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Display the first few rows of the data to understand its contents
head(fisheries)
```

    ## # A tibble: 6 × 4
    ##   country        capture aquaculture  total
    ##   <chr>            <dbl>       <dbl>  <dbl>
    ## 1 Afghanistan       1000        1200   2200
    ## 2 Albania           7886         950   8836
    ## 3 Algeria          95000        1361  96361
    ## 4 American Samoa    3047          20   3067
    ## 5 Andorra              0           0      0
    ## 6 Angola          486490         655 487145

``` r
# Assuming the data has 'capture' and 'aquaculture' as separate columns
# Filter for top 10 in capture
top_capture <- fisheries %>%
  arrange(desc(capture)) %>%
  slice(1:10)

# Filter for top 10 in aquaculture
top_aquaculture <- fisheries %>%
  arrange(desc(aquaculture)) %>%
  slice(1:10)
```

``` r
# Bar chart for top 10 countries in capture
ggplot(top_capture, aes(x = reorder(country, capture), y = capture, fill = country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Countries by Capture Production in 2016",
    x = "Country",
    y = "Production (Tons)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis labels readability
```

![](lab-06_files/figure-gfm/visuals-fishies-1.png)<!-- -->

``` r
# Bar chart for top 10 countries in aquaculture
ggplot(top_aquaculture, aes(x = reorder(country, aquaculture), y = aquaculture, fill = country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Countries by Aquaculture Production in 2016",
    x = "Country",
    y = "Production (Tons)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis labels readability
```

![](lab-06_files/figure-gfm/visuals-fishies-2.png)<!-- -->
