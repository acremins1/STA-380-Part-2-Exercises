STA 380 Part 2: Exercises
================
Aidan Cremins
2022-07-29

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(forcats)
```

# Probability Practice

### Part a.

P(Y) = 0.65 P(N) = 0.35 P(RC) = 0.3 P(TC) = 0.7 (P(RC)-1) P(Y\|RC) = 0.5
P(N\|RC) = 0.5

We’re looking for P(Y\|TC) so we can use the rule of total probability:

P(Y) = P(Y, TC) + P(Y, RC) = P(TC) \* P(Y\|TC) + P(RC) \* P(Y\|RC)

We know all of these inputs to the equation except for P(Y\|TC), so we
want to solve for that unknown.

0.65 = 0.7 \* P(Y\|TC) + 0.3 \* 0.5

From the above equation, we find that P(Y\|TC)
![\approx](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Capprox "\approx")
0.714286. This means that truthful clickers answer yes to the question
about 71.43% of the time.

![\frac{P(Y)\*P(TC\|Y)}{P(Yes)\*P(TC\|Y)+P(No)\*P(RC\|Y)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7BP%28Y%29%2AP%28TC%7CY%29%7D%7BP%28Yes%29%2AP%28TC%7CY%29%2BP%28No%29%2AP%28RC%7CY%29%7D "\frac{P(Y)*P(TC|Y)}{P(Yes)*P(TC|Y)+P(No)*P(RC|Y)}")

![\frac{0.65\*0.5}{.7\*0.5+0.3\*0.5}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B0.65%2A0.5%7D%7B.7%2A0.5%2B0.3%2A0.5%7D "\frac{0.65*0.5}{.7*0.5+0.3*0.5}")

### Part b.

P(Positive\|Disease) = .993 P(Negative\|No Disease) = 0.9999 P(Disease)
= 0.000025 P(No Disease) = 0.999975 (1-0.000025)

We’re looking for P(Disease\|Positive) so we can use Baye’s Law:

![\frac{P(Disease)\*P(Positive\|Disease)}{P(Disease)\*P(Positive\|Disease)+P(No Disease)\*P(Positive\|No Disease)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7BP%28Disease%29%2AP%28Positive%7CDisease%29%7D%7BP%28Disease%29%2AP%28Positive%7CDisease%29%2BP%28No%20Disease%29%2AP%28Positive%7CNo%20Disease%29%7D "\frac{P(Disease)*P(Positive|Disease)}{P(Disease)*P(Positive|Disease)+P(No Disease)*P(Positive|No Disease)}")

We have almost all of the inputs that we need, however, we’re missing
P(Positive\|No Disease). These are false positives. We can find the
missing probability by taking 1 - true negatives, or 1 - 0.9999 to get
P(Positive\|No Disease) as 0.0001. Now we can solve for
P(Disease\|Positive).

![\frac{0.000025\*0.993}{0.000025\*0.993+0.999975\*0.0001}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B0.000025%2A0.993%7D%7B0.000025%2A0.993%2B0.999975%2A0.0001%7D "\frac{0.000025*0.993}{0.000025*0.993+0.999975*0.0001}")
![\approx](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Capprox "\approx")
.198882. Thus, if someone tests positive, they have about a 19.89%
chance of actually having the disease.

# Wrangling the Billboard Top 100

``` r
billboard = read.csv("data/billboard.csv")
```

\#Need a caption - probably something about how most are recent songs

### Part a.

``` r
billboard %>%
  group_by(performer, song) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
```

    ## `summarise()` has grouped output by 'performer'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 3
    ## # Groups:   performer [10]
    ##    performer                                 song                          count
    ##    <chr>                                     <chr>                         <int>
    ##  1 Imagine Dragons                           Radioactive                      87
    ##  2 AWOLNATION                                Sail                             79
    ##  3 Jason Mraz                                I'm Yours                        76
    ##  4 The Weeknd                                Blinding Lights                  76
    ##  5 LeAnn Rimes                               How Do I Live                    69
    ##  6 LMFAO Featuring Lauren Bennett & GoonRock Party Rock Anthem                68
    ##  7 OneRepublic                               Counting Stars                   68
    ##  8 Adele                                     Rolling In The Deep              65
    ##  9 Jewel                                     Foolish Games/You Were Meant…    65
    ## 10 Carrie Underwood                          Before He Cheats                 64

### Part b.

``` r
musical_diversity = billboard %>%
  filter(year != 1958 & year != 2021) %>%
  group_by(year) %>%
  summarize(unique_songs_per_year = length(unique(c(performer,song))))

ggplot(musical_diversity) + geom_line(aes(x = year, y = unique_songs_per_year))
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Part c.

``` r
ten_week_hit_songs <- billboard %>%
  group_by(performer,song) %>%
  summarize(ten_week_hit = ifelse(n()>=10,"Yes","No")) %>%
  filter(ten_week_hit == "Yes")
```

    ## `summarise()` has grouped output by 'performer'. You can override using the
    ## `.groups` argument.

``` r
top_artists <- ten_week_hit_songs %>%
  group_by(performer) %>%
  summarize(num_ten_week_hit = n()) %>%
  filter(num_ten_week_hit>=30)

ggplot(top_artists) + geom_bar(aes(x = fct_reorder(performer,num_ten_week_hit), y = num_ten_week_hit),stat = "identity") + coord_flip()
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\#Visual story telling part 1: green buildings

``` r
green_buildings = read.csv("data/greenbuildings.csv")
```

``` r
ggplot(green_buildings, aes(x = Rent)) + geom_histogram() + facet_grid(.~green_rating)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Green buildings tend to be newer, “newness” could justify higher rents

``` r
ggplot(green_buildings, aes(x = green_rating, y = age ,group = green_rating)) + geom_boxplot()
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(green_buildings, aes(x = age, y = Rent)) + geom_line() + facet_grid(. ~ green_rating)
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Visual story telling part 2: Cap Metro data

``` r
cap_metro <- read.csv("data/capmetro_UT.csv")
```

``` r
cap_metro$time_of_day = ifelse(cap_metro$hour_of_day %in% c(6,7,8,9,10,11),"Morning",ifelse(cap_metro$hour_of_day %in% c(12,13,14,15,16),"Afternoon","Evening"))
time_of_day_order <- c("Morning","Afternoon","Evening")
cap_metro$activity = cap_metro$boarding + cap_metro$alighting
ggplot(cap_metro, aes(x = factor(time_of_day,levels=time_of_day_order), y = activity)) + geom_bar(stat="identity") + facet_grid(. ~ weekend)
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Activity seems to slightly increase as temperature increases, adjusted
for the difference in ridership between weekdays and weekends

``` r
riders_temp = cap_metro %>%
  group_by(timestamp) %>%
  summarize(total_riders = sum(activity), mean_temp = mean(temperature), weekend = weekend)
ggplot(riders_temp, aes(x = mean_temp, y = total_riders)) + geom_line()+facet_grid(.~weekend)
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Clustering and PCA

``` r
wine <- read.csv("data/wine.csv")
```

``` r
set.seed(1)
wine_quant <- wine[,! names(wine) %in% c("color","quality")]
wine_pca = prcomp(wine_quant, rank=10, scale=TRUE)
boxplot(wine_pca$x[,1],as.factor(wine$color))
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Cluster 1 is mostly red wines, whereas Cluster 2 is mostly white wines.
Even just making two clusters distinguishes between the two wine colors
very well.

``` r
set.seed(1)
library(knitr)
wine_quant_scaled <- scale(wine_quant)
wine_clusters <- kmeans(wine_quant_scaled, centers=2, nstart=50)
table(wine_clusters$cluster,wine$color)
```

    ##    
    ##      red white
    ##   1 1575    68
    ##   2   24  4830

While the 2 clusters separated out the two wine colors well, they don’t
seem to distinguish between wine quality because the median quality is
essentially the same for both clusters. Even if we increase the number
of clusters pretty dramatically up to 10, there still doesn’t appear to
be major quality differences between the boxplots.

``` r
wine$cluster = as.factor(wine_clusters$cluster)
ggplot(wine, aes(x = cluster, y = quality)) + geom_boxplot()
```

![](STA-380-Part-2-Exercises_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

# Market Segmentation

``` r
set.seed(1)
library(reshape2)
social_mark <- read.csv("data/social_marketing.csv")
social_mark_quant <- social_mark[,! names(social_mark) %in% "X"]
social_mark_quant_scaled <- scale(social_mark_quant)
social_mark_clusters <- kmeans(social_mark_quant_scaled, centers=10, nstart=50)
social_mark$cluster <- social_mark_clusters$cluster
cluster_means <- aggregate(social_mark[, 2:36], by=list(social_mark$cluster), mean)
cluster_means <- melt(cluster_means,id="Group.1")
cluster_means <- cluster_means %>% 
  group_by(Group.1) %>%
  arrange(desc(value)) %>% 
  slice(1:10)
cluster_means
```

    ## # A tibble: 100 × 3
    ## # Groups:   Group.1 [10]
    ##    Group.1 variable         value
    ##      <int> <fct>            <dbl>
    ##  1       1 health_nutrition 12.5 
    ##  2       1 personal_fitness  6.65
    ##  3       1 chatter           3.94
    ##  4       1 cooking           3.43
    ##  5       1 outdoors          2.88
    ##  6       1 photo_sharing     2.40
    ##  7       1 food              2.21
    ##  8       1 current_events    1.51
    ##  9       1 shopping          1.28
    ## 10       1 travel            1.23
    ## # … with 90 more rows

# The Reuters Corpus

**Figure out how to download this data** - For now, just clone the
Github and copy the folder over; I’ve added it to .gitignore so it won’t
be pushed to Github
