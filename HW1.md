lecture
================
2022-09-22

``` r
# assignment1
ques1 = flights %>% 
  group_by(tailnum) %>% 
  summarise(missing_dep_time = sum(is.na(dep_time)), n = n()) %>% 
  arrange(desc(missing_dep_time))

ques1_b = ques1 %>% 
  filter(missing_dep_time == 1)

nrow(ques1)
```

    ## [1] 4044

``` r
nrow(ques1_b)
```

    ## [1] 620

``` r
other = sapply(flights, function(x) sum(is.na(x)))
la = labels(other[other > 0])
la
```

    ## [1] "dep_time"  "dep_delay" "arr_time"  "arr_delay" "tailnum"   "air_time"

those flights had been cancelled

``` r
ques2 = flights %>% 
  mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100), 
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100))
ques2
```

    ## # A tibble: 336,776 × 19
    ##     year month   day dep_time sched_de…¹ dep_d…² arr_t…³ sched…⁴ arr_d…⁵ carrier
    ##    <int> <int> <int>    <dbl>      <dbl>   <dbl>   <int>   <int>   <dbl> <chr>  
    ##  1  2013     1     1      317        315       2     830     819      11 UA     
    ##  2  2013     1     1      333        329       4     850     830      20 UA     
    ##  3  2013     1     1      342        340       2     923     850      33 AA     
    ##  4  2013     1     1      344        345      -1    1004    1022     -18 B6     
    ##  5  2013     1     1      354        360      -6     812     837     -25 DL     
    ##  6  2013     1     1      354        358      -4     740     728      12 UA     
    ##  7  2013     1     1      355        360      -5     913     854      19 B6     
    ##  8  2013     1     1      357        360      -3     709     723     -14 EV     
    ##  9  2013     1     1      357        360      -3     838     846      -8 B6     
    ## 10  2013     1     1      358        360      -2     753     745       8 AA     
    ## # … with 336,766 more rows, 9 more variables: flight <int>, tailnum <chr>,
    ## #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
    ## #   minute <dbl>, time_hour <dttm>, and abbreviated variable names
    ## #   ¹​sched_dep_time, ²​dep_delay, ³​arr_time, ⁴​sched_arr_time, ⁵​arr_delay

``` r
#[1] "dep_time"  "dep_delay" "arr_time" 
#[4] "arr_delay" "tailnum"   "air_time" 

ques3 = flights %>% 
  mutate(cancel = (is.na(dep_time)) | (is.na(dep_delay)) | (is.na(arr_time)) | (is.na(arr_delay)) | (is.na(air_time))) %>% 
  group_by(day) %>% 
  summarise(po = mean(cancel), sum = sum(cancel), n = n()) %>%
  ggplot(aes(x = day)) +
    geom_point(aes(y = sum)) +
    geom_line(aes(y = po))
ques3
```

![](HW1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
