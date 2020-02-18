Statistical assignment 3
================
Jenson Wong 670034697
15/2/20

In this assignment we will explore political interest (*vote6*) and how it changes over time.

Read data
---------

First we want to read and join the data for the first 7 waves of the Understanding Society. (Wave 8 does not have a variable for political interest). We only want five variables: personal identifier, sample origin, sex, age and political interest. It is tedious to join all the seven waves manually, and it makes sense to use a loop in this case. Since you don't yet know about iteration I'll provide the code for you; please see the explanation of the code here: <http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is to provide a path to the directory where the data are stored on your computer.

``` r
library(tidyverse)
library(data.table)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "~/Documents/University/Year 3//UKDA-6614-tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "/Users/jsn2817/Documents/University/Year 3//UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

Reshape data (20 points)
------------------------

Now we have got the data from all 7 waves in the same data frame **all7** in the wide format. Note that the panel is unbalanced, i.e. we included all people who participated in at least one wave of the survey. Reshape the data to the long format. The resulting data frame should have six columns for six variables.

``` r
Long <- all7 %>%
  gather(a_memorig:g_vote6, key = "variable", value = "value")%>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  spread(key = variable, value = value)
```

Filter and recode (20 points)
-----------------------------

Now we want to filter the data keeping only respondents from the original UKHLS sample for Great Britain (memorig == 1). We also want to clean the variables for sex (recoding it to "male" or "female") and political interest (keeping the values from 1 to 4 and coding all negative values as missing). Tabulate *sex* and *vote6* to make sure your recodings were correct.

``` r
Na <- c(-1, -2, -7, -8, -9)
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 1, "Male", "Female")) %>%
        mutate(vote6 = replace(vote6, vote6<0, NA))

Long %>%
  count(sex_dv)
```

    ## # A tibble: 2 x 2
    ##   sex_dv      n
    ##   <chr>   <int>
    ## 1 Female 117673
    ## 2 Male   100342

``` r
Long %>%
  count(vote6)
```

    ## # A tibble: 5 x 2
    ##   vote6     n
    ##   <int> <int>
    ## 1     1 21660
    ## 2     2 70952
    ## 3     3 56134
    ## 4     4 52145
    ## 5    NA 17124

Calculate mean political interest by sex and wave (10 points)
-------------------------------------------------------------

Political interest is an ordinal variable, but we will treat it as interval and calculate mean political interest for men and women in each wave.

``` r
meanVote6 <- Long %>%
    group_by(sex_dv, wave) %>%
    summarise(Mean = mean(vote6, na.rm=TRUE))

meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex_dv [2]
    ##    sex_dv wave   Mean
    ##    <chr>  <chr> <dbl>
    ##  1 Female a      2.84
    ##  2 Female b      2.82
    ##  3 Female c      2.88
    ##  4 Female d      2.89
    ##  5 Female e      2.87
    ##  6 Female f      2.81
    ##  7 Female g      2.73
    ##  8 Male   a      2.53
    ##  9 Male   b      2.51
    ## 10 Male   c      2.54
    ## 11 Male   d      2.55
    ## 12 Male   e      2.51
    ## 13 Male   f      2.47
    ## 14 Male   g      2.42

Reshape the data frame with summary statistics (20 points)
----------------------------------------------------------

Your resulting data frame with the means is in the long format. Reshape it to the wide format. It should look like this:

| sex\_dv | a   | b   | c   | d   | e   | f   | g   |
|---------|-----|-----|-----|-----|-----|-----|-----|
| female  |     |     |     |     |     |     |     |
| male    |     |     |     |     |     |     |     |

In the cells of this table you should have mean political interest by sex and wave.

Write a short interpretation of your findings.

Over time, the mean political interests for both genders have fallen.

``` r
w.meanvote6 <- meanVote6 %>%
  gather(Mean, key = "variable", value = "value") %>%
  unite("variable", c("wave", "variable"), sep = "_") %>%
  spread(key = variable, value = value)

w.meanvote6
```

    ## # A tibble: 2 x 8
    ## # Groups:   sex_dv [2]
    ##   sex_dv a_Mean b_Mean c_Mean d_Mean e_Mean f_Mean g_Mean
    ##   <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 Female   2.84   2.82   2.88   2.89   2.87   2.81   2.73
    ## 2 Male     2.53   2.51   2.54   2.55   2.51   2.47   2.42

Estimate stability of political interest (30 points)
----------------------------------------------------

Political scientists have been arguing how stable the level of political interest is over the life course. Imagine someone who is not interested in politics at all so that their value of *vote6* is always 4. Their level of political interest is very stable over time, as stable as the level of political interest of someone who is always very interested in politics (*vote6* = 1). On the other hand, imagine someone who changes their value of *votes6* from 1 to 4 and back every other wave. Their level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is going to be equal to the sum of the absolute values of changes in political interest from wave to wave. Let us call this measure Delta. It is difficult for me to typeset a mathematical formula in Markdown, but I'll explain this informally.

Imagine a person with the level of political interest that is constant over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from "very interested in politics" to "fairly interested in politics": {1, 1, 1, 1, 2, 2, 2}. For them, Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from "very interested in politics" to "not at all interested" every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta = (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3 \* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local polynomial curve showing the association between age at wave 1 and mean Delta. You can use either **ggplot2** or the *scatter.smooth()* function from base R.
5.  Write a short interpretation of your findings.

Males tend to have a higher Delta value, hence higher level of political interest stability than females. Additionally, the Delta value decreases as age increases, which indicates a higher level of political stability, however political stability peaks at around the age of 65, before increasing again after wards. This implies that older individuals are more likely to take an active interest in politics, but for the elderly as they enter retirement, they may have lower concerns regarding society, and therefore have a lower interest in politics.

``` r
Pol.stab <- Long %>%
  filter(!is.na(vote6)) %>% 
  group_by(pidp, sex_dv) %>%
  summarise(Delta=sum(vote6))

Pol.stab
```

    ## # A tibble: 47,708 x 3
    ## # Groups:   pidp [47,708]
    ##        pidp sex_dv Delta
    ##       <int> <chr>  <int>
    ##  1 68001367 Male       3
    ##  2 68004087 Male      13
    ##  3 68006127 Female    28
    ##  4 68006135 Female    14
    ##  5 68006139 Female    13
    ##  6 68006807 Female    27
    ##  7 68007487 Female     8
    ##  8 68008167 Female     7
    ##  9 68008847 Female    22
    ## 10 68009527 Male      16
    ## # … with 47,698 more rows

``` r
Pol.stab.mean <- Pol.stab %>% 
  group_by(sex_dv) %>% 
  summarise(mean_delta = mean(Delta))

Pol.stab.mean
```

    ## # A tibble: 2 x 2
    ##   sex_dv mean_delta
    ##   <chr>       <dbl>
    ## 1 Female       12.3
    ## 2 Male         10.2

``` r
Pol.stab1 <- Long %>%
  filter(!is.na(vote6)) %>% 
  filter(wave == "a") %>% 
  group_by(age_dv) %>% 
  summarise(Delta=mean(vote6))

Pol.stab1
```

    ## # A tibble: 85 x 2
    ##    age_dv Delta
    ##     <int> <dbl>
    ##  1     15  3.67
    ##  2     16  3.15
    ##  3     17  3.10
    ##  4     18  3.08
    ##  5     19  3.09
    ##  6     20  3.02
    ##  7     21  2.96
    ##  8     22  3.04
    ##  9     23  2.94
    ## 10     24  2.83
    ## # … with 75 more rows

``` r
p <- ggplot(Pol.stab1, aes(age_dv, Delta)) +
  geom_point() + stat_smooth()

p
```

![](assignment3_files/figure-markdown_github/unnamed-chunk-6-1.png)
