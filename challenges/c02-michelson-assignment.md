Michelson Speed-of-light Measurements
================
Haley Pelletier
2020-07-18

  - [Grading Rubric](#grading-rubric)
      - [Individual](#individual)
      - [Team](#team)
      - [Due Date](#due-date)
      - [Bibliography](#bibliography)

*Purpose*: When studying physical problems, there is an important
distinction between *error* and *uncertainty*. The primary purpose of
this challenge is to dip our toes into these factors by analyzing a real
dataset.

*Reading*: [Experimental Determination of the Velocity of
Light](https://play.google.com/books/reader?id=343nAAAAMAAJ&hl=en&pg=GBS.PA115)
(Optional)

<!-- include-rubric -->

# Grading Rubric

<!-- -------------------------------------------------- -->

Unlike exercises, **challenges will be graded**. The following rubrics
define how you will be graded, both on an individual and team basis.

## Individual

<!-- ------------------------- -->

| Category    | Unsatisfactory                                                                   | Satisfactory                                                               |
| ----------- | -------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| Effort      | Some task **q**’s left unattempted                                               | All task **q**’s attempted                                                 |
| Observed    | Did not document observations                                                    | Documented observations based on analysis                                  |
| Supported   | Some observations not supported by analysis                                      | All observations supported by analysis (table, graph, etc.)                |
| Code Styled | Violations of the [style guide](https://style.tidyverse.org/) hinder readability | Code sufficiently close to the [style guide](https://style.tidyverse.org/) |

## Team

<!-- ------------------------- -->

| Category   | Unsatisfactory                                                                                   | Satisfactory                                       |
| ---------- | ------------------------------------------------------------------------------------------------ | -------------------------------------------------- |
| Documented | No team contributions to Wiki                                                                    | Team contributed to Wiki                           |
| Referenced | No team references in Wiki                                                                       | At least one reference in Wiki to member report(s) |
| Relevant   | References unrelated to assertion, or difficult to find related analysis based on reference text | Reference text clearly points to relevant analysis |

## Due Date

<!-- ------------------------- -->

All the deliverables stated in the rubrics above are due on the day of
the class discussion of that exercise. See the
[Syllabus](https://docs.google.com/document/d/1jJTh2DH8nVJd2eyMMoyNGroReo0BKcJrz1eONi3rPSc/edit?usp=sharing)
for more information.

``` r
# Libraries
library(tidyverse)
library(googlesheets4)
library(viridis)

url <- "https://docs.google.com/spreadsheets/d/1av_SXn4j0-4Rk0mQFik3LLr-uf0YdA06i3ugE6n-Zdo/edit?usp=sharing"

# Parameters
LIGHTSPEED_VACUUM    <- 299792.458 # Exact speed of light in a vacuum (km / s)
LIGHTSPEED_MICHELSON <- 299944.00  # Michelson's speed estimate (km / s)
LIGHTSPEED_PM        <- 51         # Michelson error estimate (km / s)
```

*Background*: In 1879 Albert Michelson led an experimental campaign to
measure the speed of light. His approach was a development upon the
method of Foucault, and resulted in a new estimate of
\(v_0 = 299944 \pm 51\) kilometers per second (in a vacuum). This is
very close to the modern *exact* value of `r LIGHTSPEED_VACUUM`. In this
challenge, you will analyze Michelson’s original data, and explore some
of the factors associated with his experiment.

I’ve already copied Michelson’s data from his 1880 publication; the code
chunk below will load these data from a public googlesheet.

*Aside*: The speed of light is *exact* (there is **zero error** in the
value `LIGHTSPEED_VACUUM`) because the meter is actually
[*defined*](https://en.wikipedia.org/wiki/Metre#Speed_of_light_definition)
in terms of the speed of light\!

``` r
## Note: No need to edit this chunk!
gs4_deauth()
ss <- gs4_get(url)
df_michelson <-
  read_sheet(ss) %>%
  select(Date, Distinctness, Temp, Velocity) %>%
  mutate(Distinctness = as_factor(Distinctness))
```

    ## Reading from "michelson1879"

    ## Range "Sheet1"

``` r
df_michelson %>% glimpse
```

    ## Rows: 100
    ## Columns: 4
    ## $ Date         <dttm> 1879-06-05, 1879-06-07, 1879-06-07, 1879-06-07, 1879-06…
    ## $ Distinctness <fct> 3, 2, 2, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 3, 3, 2,…
    ## $ Temp         <dbl> 76, 72, 72, 72, 72, 72, 83, 83, 83, 83, 83, 90, 90, 71, …
    ## $ Velocity     <dbl> 299850, 299740, 299900, 300070, 299930, 299850, 299950, …

*Data dictionary*:

  - `Date`: Date of measurement
  - `Distinctness`: Distinctness of measured images: 3 = good, 2 = fair,
    1 = poor
  - `Temp`: Ambient temperature (Fahrenheit)
  - `Velocity`: Measured speed of light (km / s)

**q1** Re-create the following table (from Michelson (1880), pg. 139)
using `df_michelson` and `dplyr`. Note that your values *will not* match
those of Michelson *exactly*; why might this be?

| Distinctness | n  | MeanVelocity |
| ------------ | -- | ------------ |
| 3            | 46 | 299860       |
| 2            | 39 | 299860       |
| 1            | 15 | 299810       |

``` r
df_q1 <- 
  df_michelson %>%
    group_by(Distinctness) %>%
    summarize(n = n(), MeanVelocity = mean(Velocity))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
df_q1 %>%
  arrange(desc(Distinctness)) %>%
  knitr::kable()
```

| Distinctness |  n | MeanVelocity |
| :----------- | -: | -----------: |
| 3            | 46 |     299861.7 |
| 2            | 39 |     299858.5 |
| 1            | 15 |     299808.0 |

**Observations**: - Most observations have a distinctness of 2 or 3. -
My values don’t match Michelson’s exactly because his seem to be
rounded. If mine were rounded, they would be the same values.

The `Velocity` values in the dataset are the speed of light *in air*;
Michelson introduced a couple of adjustments to estimate the speed of
light in a vacuum. In total, he added \(+92\) km/s to his mean estimate
for `VelocityVacuum` (from Michelson (1880), pg. 141). While this isn’t
fully rigorous (\(+92\) km/s is based on the mean temperature), we’ll
simply apply this correction to all the observations in the dataset.

**q2** Create a new variable `VelocityVacuum` with the \(+92\) km/s
adjustment to `Velocity`. Assign this new dataframe to `df_q2`.

``` r
df_q2 <- 
  df_michelson %>%
    mutate(VelocityVacuum = Velocity + 92)

df_q2 %>%
  knitr::kable()
```

| Date       | Distinctness | Temp | Velocity | VelocityVacuum |
| :--------- | :----------- | ---: | -------: | -------------: |
| 1879-06-05 | 3            |   76 |   299850 |         299942 |
| 1879-06-07 | 2            |   72 |   299740 |         299832 |
| 1879-06-07 | 2            |   72 |   299900 |         299992 |
| 1879-06-07 | 2            |   72 |   300070 |         300162 |
| 1879-06-07 | 2            |   72 |   299930 |         300022 |
| 1879-06-07 | 2            |   72 |   299850 |         299942 |
| 1879-06-09 | 3            |   83 |   299950 |         300042 |
| 1879-06-09 | 3            |   83 |   299980 |         300072 |
| 1879-06-09 | 3            |   83 |   299980 |         300072 |
| 1879-06-09 | 3            |   83 |   299880 |         299972 |
| 1879-06-09 | 2            |   83 |   300000 |         300092 |
| 1879-06-10 | 2            |   90 |   299980 |         300072 |
| 1879-06-10 | 2            |   90 |   299930 |         300022 |
| 1879-06-12 | 2            |   71 |   299650 |         299742 |
| 1879-06-12 | 2            |   71 |   299760 |         299852 |
| 1879-06-12 | 1            |   71 |   299810 |         299902 |
| 1879-06-13 | 3            |   72 |   300000 |         300092 |
| 1879-06-13 | 3            |   72 |   300000 |         300092 |
| 1879-06-13 | 2            |   72 |   299960 |         300052 |
| 1879-06-13 | 3            |   79 |   299960 |         300052 |
| 1879-06-13 | 3            |   79 |   299960 |         300052 |
| 1879-06-13 | 3            |   79 |   299940 |         300032 |
| 1879-06-13 | 3            |   79 |   299960 |         300052 |
| 1879-06-13 | 3            |   79 |   299940 |         300032 |
| 1879-06-13 | 3            |   79 |   299880 |         299972 |
| 1879-06-13 | 3            |   79 |   299800 |         299892 |
| 1879-06-14 | 1            |   64 |   299850 |         299942 |
| 1879-06-14 | 1            |   64 |   299880 |         299972 |
| 1879-06-14 | 1            |   65 |   299900 |         299992 |
| 1879-06-14 | 1            |   66 |   299840 |         299932 |
| 1879-06-14 | 1            |   67 |   299830 |         299922 |
| 1879-06-14 | 1            |   84 |   299790 |         299882 |
| 1879-06-14 | 1            |   85 |   299810 |         299902 |
| 1879-06-14 | 1            |   84 |   299880 |         299972 |
| 1879-06-14 | 1            |   84 |   299880 |         299972 |
| 1879-06-14 | 1            |   84 |   299830 |         299922 |
| 1879-06-17 | 2            |   62 |   299800 |         299892 |
| 1879-06-17 | 2            |   63 |   299790 |         299882 |
| 1879-06-17 | 1            |   64 |   299760 |         299852 |
| 1879-06-17 | 3            |   77 |   299800 |         299892 |
| 1879-06-17 | 3            |   77 |   299880 |         299972 |
| 1879-06-17 | 3            |   77 |   299880 |         299972 |
| 1879-06-17 | 3            |   77 |   299880 |         299972 |
| 1879-06-17 | 3            |   77 |   299860 |         299952 |
| 1879-06-18 | 1            |   58 |   299720 |         299812 |
| 1879-06-18 | 1            |   58 |   299720 |         299812 |
| 1879-06-18 | 1            |   59 |   299620 |         299712 |
| 1879-06-18 | 2            |   75 |   299860 |         299952 |
| 1879-06-18 | 2            |   75 |   299970 |         300062 |
| 1879-06-18 | 2            |   75 |   299950 |         300042 |
| 1879-06-20 | 3            |   60 |   299880 |         299972 |
| 1879-06-20 | 3            |   61 |   299910 |         300002 |
| 1879-06-20 | 2            |   62 |   299850 |         299942 |
| 1879-06-20 | 2            |   63 |   299870 |         299962 |
| 1879-06-20 | 2            |   78 |   299840 |         299932 |
| 1879-06-20 | 2            |   79 |   299840 |         299932 |
| 1879-06-20 | 2            |   80 |   299850 |         299942 |
| 1879-06-20 | 2            |   79 |   299840 |         299932 |
| 1879-06-20 | 2            |   79 |   299840 |         299932 |
| 1879-06-20 | 2            |   79 |   299840 |         299932 |
| 1879-06-21 | 2            |   61 |   299890 |         299982 |
| 1879-06-21 | 2            |   62 |   299810 |         299902 |
| 1879-06-21 | 2            |   63 |   299810 |         299902 |
| 1879-06-21 | 2            |   64 |   299820 |         299912 |
| 1879-06-21 | 2            |   65 |   299800 |         299892 |
| 1879-06-21 | 3            |   80 |   299770 |         299862 |
| 1879-06-21 | 3            |   81 |   299760 |         299852 |
| 1879-06-21 | 3            |   82 |   299740 |         299832 |
| 1879-06-21 | 3            |   82 |   299750 |         299842 |
| 1879-06-21 | 3            |   81 |   299760 |         299852 |
| 1879-06-23 | 3            |   89 |   299910 |         300002 |
| 1879-06-23 | 3            |   89 |   299920 |         300012 |
| 1879-06-23 | 3            |   90 |   299890 |         299982 |
| 1879-06-23 | 3            |   90 |   299860 |         299952 |
| 1879-06-23 | 3            |   90 |   299880 |         299972 |
| 1879-06-24 | 3            |   72 |   299720 |         299812 |
| 1879-06-24 | 3            |   73 |   299840 |         299932 |
| 1879-06-24 | 3            |   74 |   299850 |         299942 |
| 1879-06-24 | 3            |   75 |   299850 |         299942 |
| 1879-06-24 | 3            |   76 |   299780 |         299872 |
| 1879-06-26 | 2            |   86 |   299890 |         299982 |
| 1879-06-26 | 2            |   86 |   299840 |         299932 |
| 1879-06-27 | 3            |   73 |   299780 |         299872 |
| 1879-06-27 | 3            |   74 |   299810 |         299902 |
| 1879-06-27 | 3            |   75 |   299760 |         299852 |
| 1879-06-27 | 3            |   75 |   299810 |         299902 |
| 1879-06-27 | 3            |   76 |   299790 |         299882 |
| 1879-06-27 | 3            |   76 |   299810 |         299902 |
| 1879-06-30 | 2            |   85 |   299820 |         299912 |
| 1879-06-30 | 2            |   86 |   299850 |         299942 |
| 1879-06-30 | 2            |   86 |   299870 |         299962 |
| 1879-06-30 | 2            |   86 |   299870 |         299962 |
| 1879-07-01 | 2            |   83 |   299810 |         299902 |
| 1879-07-01 | 2            |   84 |   299740 |         299832 |
| 1879-07-01 | 2            |   86 |   299810 |         299902 |
| 1879-07-01 | 2            |   86 |   299940 |         300032 |
| 1879-07-02 | 3            |   86 |   299950 |         300042 |
| 1879-07-02 | 3            |   86 |   299800 |         299892 |
| 1879-07-02 | 3            |   86 |   299810 |         299902 |
| 1879-07-02 | 3            |   85 |   299870 |         299962 |

As part of his study, Michelson assessed the various potential sources
of error, and provided his best-guess for the error in his
speed-of-light estimate. These values are provided in
`LIGHTSPEED_MICHELSON`—his nominal estimate—and
`LIGHTSPEED_PM`—plus/minus bounds on his estimate. Put differently,
Michelson believed the true value of the speed-of-light probably lay
between `LIGHTSPEED_MICHELSON - LIGHTSPEED_PM` and `LIGHTSPEED_MICHELSON
+ LIGHTSPEED_PM`.

Let’s introduce some terminology:\[2\]

  - **Error** is the difference between a true value and an estimate of
    that value; for instance `LIGHTSPEED_VACUUM - LIGHTSPEED_MICHELSON`.
  - **Uncertainty** is an analyst’s *assessment* of the error.

Since a “true” value is often not known in practice, one generally does
not know the error. The best they can do is quantify their degree of
uncertainty. We will learn some means of quantifying uncertainty in this
class, but for many real problems uncertainty includes some amount of
human judgment.\[2\]

**q3** Compare Michelson’s speed of light estimate against the modern
speed of light value. Is Michelson’s estimate of the error (his
uncertainty) greater or less than the true error?

``` r
true_error <- LIGHTSPEED_VACUUM - LIGHTSPEED_MICHELSON

true_error
```

    ## [1] -151.542

``` r
LIGHTSPEED_PM
```

    ## [1] 51

**Observations**: - Michelson’s uncertainty (-51) is less than the true
error (-152).

**q4** You have access to a few other variables. Construct a few
visualizations of `VelocityVacuum` against these other factors. Are
there other patterns in the data that might help explain the difference
between Michelson’s estimate and `LIGHTSPEED_VACUUM`?

``` r
df_q2 %>%
  ggplot() +
    geom_point(aes(x = Distinctness, y = VelocityVacuum, color = Distinctness)) +
    geom_hline(yintercept = LIGHTSPEED_MICHELSON, linetype = "dashed") + 
    geom_hline(yintercept = LIGHTSPEED_VACUUM, color = "red") +
    ggtitle("Velocity Measurement vs Distinctness") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](c02-michelson-assignment_files/figure-gfm/q4-task-1.png)<!-- -->

``` r
df_q2 %>%
  ggplot() +
    geom_point(aes(x = Temp, y = VelocityVacuum)) +
    geom_hline(yintercept = LIGHTSPEED_MICHELSON, linetype = "dashed") + 
    geom_hline(yintercept = LIGHTSPEED_VACUUM, color = "red") +
    ggtitle("Velocity Measurement vs Temperature") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](c02-michelson-assignment_files/figure-gfm/q4-task-2.png)<!-- -->

**Observations**:

  - In the above plots, the solid red line is the actual speed of light
    value and the dashed black line is Michelson’s value.
  - All of Michelson’s measurements are, unsurprisingly, closer to his
    value than the actual value.
  - The higher-distinctness measurements are closer to his value.
  - Temperature does not appear to have any effect on the measurements.

## Bibliography

  - \[1\] Michelson, [Experimental Determination of the Velocity of
    Light](https://play.google.com/books/reader?id=343nAAAAMAAJ&hl=en&pg=GBS.PA115)
    (1880)
  - \[2\] Henrion and Fischhoff, [Assessing Uncertainty in Physical
    Constants](https://www.cmu.edu/epp/people/faculty/research/Fischoff-Henrion-Assessing%20uncertainty%20in%20physical%20constants.pdf)
    (1986)
