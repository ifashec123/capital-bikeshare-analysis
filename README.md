Capital Bikeshare Analysis using Rstudio
================

- <a href="#introduction" id="toc-introduction">Introduction</a>
- <a href="#data" id="toc-data">Data</a>
- <a href="#questions" id="toc-questions">Questions</a>
  - <a href="#question-1" id="toc-question-1">Question 1</a>
  - <a href="#question-2" id="toc-question-2">Question 2</a>
  - <a href="#question-3" id="toc-question-3">Question 3</a>
- <a href="#analysis" id="toc-analysis">Analysis</a>
  - <a href="#answer-to-qustion-1" id="toc-answer-to-qustion-1">Answer to
    qustion 1</a>
  - <a href="#answer-to-question-2" id="toc-answer-to-question-2">Answer to
    question 2</a>
  - <a href="#answer-to-question-3" id="toc-answer-to-question-3">Answer to
    question 3</a>
- <a href="#conclusion" id="toc-conclusion">Conclusion</a>

# Introduction

This Project focuses on analyzing bike rides from Capital Bikeshare a
bicycle-sharing system operating in Washington, D.C. and surrounding
areas. The system offers both casual and registered users access to a
network of stations, promoting an eco-friendly mode of transport.
Studying this data is valuable as it provides insights into urban
mobility patterns, the impact of pricing structures on rides durations,
and how shared bikes contribute to transportation sustainability.
Exploring these elements can help enhance the efficiency and reach of
similar systems globally.

# Data

The raw data used for this coursework is from Capital Bikeshare, a
public bicycle-sharing program in Washington, D.C., and its surrounding
areas. The dataset consists of historical ride data that captures
various aspects of the bike trips taken by users of the system. The
information provided is anonymized and made publicly available by
Capital Bikeshare.

``` r
#To import the data into r
df <- read_csv(file = here("data", "rides_2020_2021_extract.csv")) 
glimpse(df)
```

    ## Rows: 1,986,564
    ## Columns: 16
    ## $ Duration             <chr> "411", "753", "382", "698", "375", "503", "581", …
    ## $ `Start date`         <dttm> 2020-03-20 13:33:17, 2021-08-29 11:48:52, 2021-1…
    ## $ `End date`           <dttm> 2020-03-20 13:40:08, 2021-08-29 12:01:25, 2021-1…
    ## $ start_station_id     <dbl> 31623, 31320, 31272, 31113, 31626, 31212, 31126, …
    ## $ `Start station name` <chr> "Columbus Circle / Union Station", "American Univ…
    ## $ end_station_id       <dbl> 31617, 31308, 31633, 31257, 31644, 31203, 31400, …
    ## $ `End station name`   <chr> "Bladensburg Rd & Benning Rd NE", "39th & Veazey …
    ## $ bike_number          <chr> "W23722", NA, NA, "W22100", NA, NA, NA, NA, NA, N…
    ## $ member_casual        <chr> "Member", "member", "member", "Member", "member",…
    ## $ ride_id              <chr> NA, "CADE5F217D4DA925", "C120526C8DA3F4DB", NA, "…
    ## $ rideable_type        <chr> NA, "classic_bike", "classic_bike", NA, "classic_…
    ## $ start_lat            <dbl> NA, 38.93630, 38.88698, NA, 38.88732, 38.90571, 3…
    ## $ start_lng            <dbl> NA, -77.08713, -77.01377, NA, -76.98357, -77.0473…
    ## $ end_lat              <dbl> NA, 38.94384, 38.88731, NA, 38.88249, 38.90860, 3…
    ## $ end_lng              <dbl> NA, -77.07708, -77.02576, NA, -76.99012, -77.0323…
    ## $ is_equity            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

``` r
    df <- clean_names(df) # To both clean the column name and get a glimpse of what the tibble currently looks like
# turning to date time data type
df <- df %>%
  mutate(start_date = ymd_hms(start_date, tz = "Etc/GMT-4")) %>%
  mutate(end_date = ymd_hms(end_date, tz = "Etc/GMT-4"))
```

The `start_date` and `end_date` columns from character is coverted to
the `datetime` format using `ymd_hms()`, with the appropriate time zone.

cleaning duration column

``` r
# Removing invalid duration values
df <- df %>%
  mutate(duration = na_if(duration, "---"))

df <- df %>% 
  mutate(duration = as.double(duration))

df <- df %>%
  mutate(duration = ifelse(duration < 0, NA, duration)) %>%
  mutate(duration = ifelse(year(start_date)== "2020" & duration > 2700, NA, duration)) %>% 
  mutate(duration = ifelse(year(start_date) == "2021" & duration > 1800, NA, duration))
```

Invalid values in the `duration` column, such as the placeholder `---`,
is replaced with `NA`.

`duration` column converted to a numeric data type (`double`) so that we
can perform mathematical operations, such as calculating averages or
filtering the data by ride length.

`duration` column cleaned by removing negative values and unrealistic
ride durations based on historical limits (30 minutes in 2020 and 45
minutes in 2021 for members). This step ensures we only keep plausible
trip durations for our analysis.

Cleaning `members casual`

``` r
df <- df %>% 
  mutate(member_casual = tolower(member_casual))
```

`member_casual` column standardized by converting all values to
lowercase. This helps avoid inconsistencies in categorical data, such as
different capitalizations for the same label (e.g., `Member` and
`member`).

Tidying

``` r
df_clean <- df %>% 
  select(-bike_number, -ride_id, -start_lat, -start_lng, -end_lat, -end_lng, -is_equity)

# Rows that have missing values in important columns filtered out.
df_clean <- df_clean %>% 
  filter(!is.na(duration)) %>% 
  filter(!is.na(start_station_id)) %>%
  filter(!is.na(end_station_id)) %>%
  filter(!is.na(rideable_type))
```
columns that are not necessary for the analysis removed, such as
geospatial information (start_lat, start_lng, etc.), bike_number,
ride_id, and is_equity. These columns either contain too many missing
values or are not relevant to the research questions created below.

``` r
 glimpse(df_clean)
```

    ## Rows: 1,366,260
    ## Columns: 9
    ## $ duration           <dbl> 753, 382, 375, 503, 581, 188, 796, 484, 437, 459, 7…
    ## $ start_date         <dttm> 2021-08-29 11:48:52, 2021-12-09 16:57:40, 2021-11-…
    ## $ end_date           <dttm> 2021-08-29 12:01:25, 2021-12-09 17:04:02, 2021-11-…
    ## $ start_station_id   <dbl> 31320, 31272, 31626, 31212, 31126, 31125, 31276, 31…
    ## $ start_station_name <chr> "American University East Campus", "Washington & In…
    ## $ end_station_id     <dbl> 31308, 31633, 31644, 31203, 31400, 31116, 31519, 31…
    ## $ end_station_name   <chr> "39th & Veazey St NW", "Independence Ave & L'Enfant…
    ## $ member_casual      <chr> "member", "member", "member", "casual", "member", "…
    ## $ rideable_type      <chr> "classic_bike", "classic_bike", "classic_bike", "cl…

# Questions

## Question 1

**1. What are the differences in ride duration between casual riders and
members?**

To explore this question, the `duration` of rides between casual and
member riders are compared. The `member_casual` column distinguishes the
ridetype, while the `duration` column, converted to minutes (for clearer
analysis), provides the length of each trip. The analysis will calculate
and visualize the average, median, and distribution of ride durations
for each group. This will help identify any significant differences in
how long rides tend to last based on rider type.

## Question 2

**2. How does the popularity of bike-sharing stations vary across
different locations?**

To answer this question, the `start_station_name` and `end_station_name`
columns are used to identify the most frequently used stations. By
counting the number of rides starting and ending at each station, I can
determine which stations are most popular.

## Question 3

**3. How does the average ride duration vary across different rideable
types and times of the day?**

This question explores how ride durations change based on both the type
of bike (`rideable_type`: classic, electric, dockable.) and the time of
day (morning, afternoon, evening). The goal is to determine whether
certain bike types are used more frequently for longer or shorter rides
during specific times of day.`rideable_type` used to find the different
bike types,Time extracted from the `start_date` and grouped (e.g.,
morning, afternoon, evening) and, `duration` to find the continuous
variable for the ride duration.

# Analysis

## Answer to qustion 1

``` r
summary_stats <- df_clean %>%
  group_by(member_casual) %>%
  summarise(
    avg_duration = mean(duration, na.rm = TRUE),
    median_duration = median(duration, na.rm = TRUE),
    min_duration = min(duration, na.rm = TRUE),
    max_duration = max(duration, na.rm = TRUE)
  )
```

Summary statistics (mean, median, min, max) is calulated for duration
based on the member_casual variable, using the duration column. The
distribution of ride durations for both casual and member riders also
visualized using a density plot, which allows us to compare the spread
of durations.

``` r
ggplot(df_clean, aes(x = duration, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~member_casual) +
  scale_x_log10() +  # Log scale to better handle skewed data
  labs(
    title = "Distribution of Ride Duration for Members vs Casual Riders",
    x = "Duration (minutes)",
    y = "Density"
  ) +
  theme_minimal() +
  scale_fill_discrete(name = "Rider Type")
```

![](introcw_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Density plot created to show the ride duration distribution for both
member and casual riders.

The majority of casual riders take trips that are longer in duration,
with a peak density after 1000 minutes. casual riders also show a
tighter distribution with most around the 700 - 1000 minutes mark. This
indicates that casual riders are more likely to take longer trips
compared to members. Members show a broader distribution with rides
peaking around the 700 minutes mark. Members are more likely to take
shorter rides compared to casual riders, which aligns with the
membership structure of motivating frequent, shorter trips.

## Answer to question 2

``` r
station_popularity <- df_clean %>%
  count(start_station_name, sort = TRUE) %>%
  top_n(10, n)
```

Number of rides starting from and ending at each station counted, top 10
most popular stations identified. A bar chart will be used to visualize
this.

``` r
ggplot(station_popularity, aes(x = reorder(start_station_name, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Popular Start Stations",
    x = "Station Name",
    y = "Number of Rides"
  ) +
  theme_minimal()
```

![](introcw_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

bar chart created to display the top 10 most popular stations.

**New Hampshire Ave & T St NW** is the most popular station, with over
15,000 rides starting and ending there.**15th & P St NW and 1st & M St
NE** follow closely.The concentration of popular stations in the NW
quadrant of Washington, D.C., might suggest that this region has higher
demand for bike-sharing services, possibly due to denser population or
more biking infrastructure.

## Answer to question 3

``` r
df_clean <- df_clean %>%
  mutate(hour = hour(start_date),
         time_of_day = case_when(
           hour >= 6 & hour < 12 ~ "Morning",
           hour >= 12 & hour < 18 ~ "Afternoon",
           hour >= 18 & hour < 24 ~ "Evening",
           TRUE ~ "Night"))
```

new column created called time_of_day that categorizes rides into time
period

``` r
avg_duration_by_type_time <- df_clean %>%
  group_by(rideable_type, time_of_day) %>%
  summarise(avg_duration = mean(duration, na.rm = TRUE))
```

average duration for each rideable_type and time_of_day combination
calculated.

``` r
ggplot(avg_duration_by_type_time, aes(x = time_of_day, y = rideable_type, fill = avg_duration)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Duration (mins)") +
  labs(title = "Average Ride Duration by Bike Type and Time of Day",
       x = "Time of Day", y = "Bike Type") +
  theme_minimal()
```

![](introcw_files/figure-gfm/unnamed-chunk-19-1.png)<!-- --> Majority of
type of bicycles used are the docked bikes, this is evident by a
continuous darker shade all throughout the day from morning to night.
Afternoon time is also the most popular usage time for all three of the
types of rides. This insight could help in understanding user
preferences and operational efficiency of different bike types and
potentially guide decisions about how many of each type of bike to
provide at certain times of the day.

# Conclusion

- Casual riders tend to take longer trips compared to members, who
  generally prefer shorter, more frequent rides. This aligns with the
  membership pricing structure.

- Stations in Washington D.C.’s NW quadrant are the most popular, likely
  due to higher population density or better biking infrastructure.

- Docked bikes are the most used throughout the day, with peak usage
  occurring in the afternoon across all bike types.

- Future Research: Investigate how seasonal changes or weather patterns
  influence station popularity and ride duration to optimize
  bike-sharing operations
