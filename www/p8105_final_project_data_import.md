P8105 Final Project
================

``` r
library(tidyverse)
library(dplyr)
library(modelr)
library(mgcv)

options(scipen = 50)
knitr::opts_chunk$set(echo = TRUE)
```

## Read in the data

``` r
resp_2019 = read.table("data/atusresp_2019.dat", header = TRUE, sep = ",")
resp_2020 = read.table("data/atusresp_2020.dat", header = TRUE, sep = ",")
roster_2019 = read.table("data/atusrost_2019.dat", header = TRUE, sep = ",")
roster_2020 = read.table("data/atusrost_2020.dat", header = TRUE, sep = ",")
who_2019 = read.table("data/atuswho_2019.dat", header = TRUE, sep = ",")
who_2020 = read.table("data/atuswho_2020.dat", header = TRUE, sep = ",")
activity_2019 = read.table("data/atusact_2019.dat", header = TRUE, sep = ",")
activity_2020 = read.table("data/atusact_2020.dat", header = TRUE, sep = ",")
activity_summary_2019 = read.table("data/atussum_2019.dat", header = TRUE, sep = ",")
activity_summary_2020 = read.table("data/atussum_2020.dat", header = TRUE, sep = ",")
cps_2019 = read.table("data/atuscps_2019.dat", header = TRUE, sep = ",")
cps_2020 = read.table("data/atuscps_2020.dat", header = TRUE, sep = ",")
```

The activity summary files contains information about total minutes
spent on each activity (I believe respondents were asked to fill out a
time diary for one day of the week). There is only one respondent per
household. The cps datasets contains geographic information as well as
information about each household and each respondent in the household.

Look into the data and ensure coding has been done correctly

``` r
head(activity_2019)
```

    ##         TUCASEID TUACTIVITY_N TEWHERE TRTCCTOT_LN TRTCC_LN TRTCOC_LN TRTEC_LN
    ## 1 20190101190022            1      -1           0       -1         0       -1
    ## 2 20190101190022            2      -1           0       -1         0       -1
    ## 3 20190101190022            3      12           0       -1         0       -1
    ## 4 20190101190022            4       5           0       -1         0       -1
    ## 5 20190101190022            5      12           0       -1         0       -1
    ## 6 20190101190022            6       1           0       -1         0       -1
    ##   TRTHH_LN TRTNOHH_LN TRTOHH_LN TRTONHH_LN TRTO_LN TUACTDUR TUACTDUR24 TUCC5
    ## 1       -1         -1        -1         -1      -1      180        180     0
    ## 2       -1         -1        -1         -1      -1       30         30     0
    ## 3       -1         -1        -1         -1      -1       30         30     0
    ## 4       -1         -1        -1         -1      -1      240        240     0
    ## 5       -1         -1        -1         -1      -1       30         30     0
    ## 6       -1         -1        -1         -1      -1       30         30     0
    ##   TUCC5B TUCC7 TUCC8 TUCUMDUR TUCUMDUR24 TUDURSTOP TUEC24 TUSTARTTIM TUSTOPTIME
    ## 1      0     0    97      180        180         2     -1   04:00:00   07:00:00
    ## 2      0     0     0      210        210         1     -1   07:00:00   07:30:00
    ## 3      0     0     0      240        240         1     -1   07:30:00   08:00:00
    ## 4      0     0     0      480        480         2     -1   08:00:00   12:00:00
    ## 5      0     0     0      510        510         1     -1   12:00:00   12:30:00
    ## 6      0     0     0      540        540         1     -1   12:30:00   13:00:00
    ##   TUTIER1CODE TUTIER2CODE TUTIER3CODE TRCODE TRTIER2 TXWHERE
    ## 1           1           1           1  10101     101       0
    ## 2           1           2           1  10201     102       0
    ## 3          18          14           1 181401    1814       0
    ## 4          14           1           1 140101    1401       0
    ## 5          18          14           1 181401    1814       0
    ## 6          11           1           1 110101    1101       0

``` r
unique(resp_2019$TEHRUSLT) # -1 might be incorrect coding to exclude for hours worked
```

    ##  [1]  -1  20  -4  40  33  50  52  54  15  32  14  26  36  43  60  37  45  57  24
    ## [20]  42  70  25  28  30  12  72  44  35  65  18  29  55  58  48   4  10  23  88
    ## [39]  16  38  34   5  84   2  17  99  56  49  90  27  67  80  39  47  85   3  46
    ## [58]  75  66   8  62   6  86  51   1   7  21  53  76  95  68 120  94  64  77  41
    ## [77]   9   0  61  13  31  83 100  73  22  11  63 144  19  98  81  59

``` r
unique(roster_2019$TESEX)
```

    ## [1] 2 1

``` r
activity_summary_2020_long = 
  pivot_longer(data = activity_summary_2020, 
               cols = t010101:t500107, 
               names_to = "activity_codes", 
               values_to = "total_minutes") %>%
  select(TUCASEID, TU20FWGT, PTDTRACE, TEAGE, TELFS, TESEX, activity_codes, total_minutes) %>% #selected mostly demographic variables 
  mutate(year = 2020) %>% 
  rename(weight = TU20FWGT, 
         race = PTDTRACE, 
         labor_force_status = TELFS, 
         age = TEAGE, 
         sex = TESEX)


activity_summary_2019_long = 
  pivot_longer(data = activity_summary_2019, 
               cols = t010101:t500107, 
               names_to = "activity_codes", 
               values_to = "total_minutes") %>%
  select(TUCASEID, TUFINLWGT, PTDTRACE, TEAGE, TELFS, TESEX, activity_codes, total_minutes) %>% #selected mostly demographic variables 
  mutate(year = 2019) %>% 
  rename(weight = TUFINLWGT,
         race = PTDTRACE, 
         labor_force_status = TELFS, 
         age = TEAGE, 
         sex = TESEX)

actsum_combined = rbind(activity_summary_2019_long, activity_summary_2020_long) %>% 
  mutate(activity_codes = recode(activity_codes, 
                                 t010101 = "Sleeping", 
                                 t010102 = "Sleeplessness",
                                 t090301 = "Using pet services",
                                 t010201 = "Washing, dressing and grooming oneself",
                                 t010299 = "Grooming, n.e.c.*",
                                 t010301 = "Health-related self care",
                                 t010399 = "Self care, n.e.c.*",
                                 t010401 = "Personal/Private activities",
                                 t010499 = "Personal activities, n.e.c.*",
                                 t010501 = "Personal emergencies",
                                 t019999 = "Personal care, n.e.c.*",
                                 t020101 = "Interior cleaning",
                                 t020102 = "Laundry",
                                 t020103 = "Sewing, repairing, and maintaining textiles",
                                 t020104 = "Storing interior hh items, inc. food",
                                 t020199 = "Housework, n.e.c.*",
                                 t020201 = "Food and drink preparation",
                                 t020202 = "Food presentation",
                                 t020203 = "Kitchen and food clean-up",
                                 t020299 = "Food and drink prep, presentation, and clean-up, n.e.c.*",
                                 t020301 = "Interior arrangement, decoration, and repairs",
                                 t020302 = "Building and repairing furniture",
                                 t020303 = "Heating and cooling",
                                 t020399 = "Interior maintenance, repair, and decoration, n.e.c.*",
                                 t020401 = "Exterior cleaning",
                                 t020402 = "Exterior repair, improvements, and decoration",
                                 t020499 = "Exterior maintenance, repair and decoration, n.e.c.*",
                                 t020501 = "Lawn, garden, and houseplant care",
                                 t020502 = "Ponds, pools, and hot tubs",
                                 t020599 = "Lawn and garden, n.e.c.*",
                                 t020601 = "Care for animals and pets (not veterinary care)",
                                 t020602 = "Walking / exercising / playing with animals",
                                 t020699 = "Pet and animal care, n.e.c.*",
                                 t020701 = "Vehicle repair and maintenance (by self)",
                                 t020799 = "Vehicles, n.e.c.*",
                                 t020801 = "Appliance, tool, and toy set-up, repair, and maintenance (by self)",
                                 t020899 = "Appliances and tools, n.e.c.*",
                                 t020901 = "Financial management",
                                 t020902 = "Household and personal organization and planning",
                                 t020903 = "HH and personal mail and messages (except e-mail)",
                                 t020904 = "HH and personal e-mail and messages",
                                 t020905 = "Home security",
                                 t020999 = "Household management, n.e.c.*",
                                 t029999 = "Household activities, n.e.c.*",
                                 t030101 = "Physical care for hh children",
                                 t030102 = "Reading to/with hh children",
                                 t030103 = "Playing with hh children, not sports",
                                 t030104 = "Arts and crafts with hh children",
                                 t030105 = "Playing sports with hh children",
                                 t030106 = "Talking with/listening to hh children",
                                 t030108 = "Organization and planning for hh children",
                                 t030109 = "Looking after hh children (as a primary activity)",
                                 t030110 = "Attending hh children's events",
                                 t030111 = "Waiting for/with hh children",
                                 t030112 = "Picking up/dropping off hh children",
                                 t030199 = "Caring for and helping hh children, n.e.c.*",
                                 t030201 = "Homework (hh children)",
                                 t030202 = "Meetings and school conferences (hh children)",
                                 t030203 = "Home schooling of hh children",
                                 t030204 = "Waiting associated with hh children's education",
                                 t030299 = "Activities related to hh child's education, n.e.c.*",
                                 t030301 = "Providing medical care to hh children",
                                 t030302 = "Obtaining medical care for hh children",
                                 t030303 = "Waiting associated with hh children's health",
                                 t030399 = "Activities related to hh child's health, n.e.c.*",
                                 t030401 = "Physical care for hh adults",
                                 t030402 = "Looking after hh adult (as a primary activity)",
                                 t030403 = "Providing medical care to hh adult",
                                 t030404 = "Obtaining medical and care services for hh adult",
                                 t030405 = "Waiting associated with caring for household adults",
                                 t030499 = "Caring for household adults, n.e.c.*",
                                 t030501 = "Helping hh adults",
                                 t030502 = "Organization and planning for hh adults",
                                 t030503 = "Picking up/dropping off hh adult",
                                 t030504 = "Waiting associated with helping hh adults",
                                 t030599 = "Helping household adults, n.e.c.*",
                                 t040101 = "Physical care for nonhh children",
                                 t040102 = "Reading to/with nonhh children",
                                 t040103 = "Playing with nonhh children, not sports",
                                 t040104 = "Arts and crafts with nonhh children",
                                 t040105 = "Playing sports with nonhh children",
                                 t040106 = "Talking with/listening to nonhh children",
                                 t040108 = "Organization and planning for nonhh children",
                                 t040109 = "Looking after nonhh children (as primary activity)",
                                 t040110 = "Attending nonhh children's events",
                                 t040111 = "Waiting for/with nonhh children",
                                 t040112 = "Dropping off/picking up nonhh children",
                                 t040199 = "Caring for and helping nonhh children, n.e.c.*",
                                 t040201 = "Homework (nonhh children)",
                                 t040202 = "Meetings and school conferences (nonhh children)",
                                 t040301 = "Providing medical care to nonhh children",
                                 t040302 = "Obtaining medical care for nonhh children",
                                 t040399 = "Activities related to nonhh child's health, n.e.c.*",
                                 t040401 = "Physical care for nonhh adults",
                                 t040402 = "Looking after nonhh adult (as a primary activity)",
                                 t040403 = "Providing medical care to nonhh adult",
                                 t040404 = "Obtaining medical and care services for nonhh adult",
                                 t040405 = "Waiting associated with caring for nonhh adults",
                                 t040499 = "Caring for nonhh adults, n.e.c.*",
                                 t040501 = "Housework, cooking, and shopping assistance for nonhh adults",
                                 t040502 = "House and lawn maintenance and repair assistance for nonhh adults",
                                 t040503 = "Animal and pet care assistance for nonhh adults",
                                 t040504 = "Vehicle and appliance maintenance/repair assistance for nonhh adults",
                                 t040505 = "Financial management assistance for nonhh adults",
                                 t040506 = "Household management and paperwork assistance for nonhh adults",
                                 t040507 = "Picking up/dropping off nonhh adult",
                                 t040508 = "Waiting associated with helping nonhh adults",
                                 t040599 = "Helping nonhh adults, n.e.c.*",
                                 t049999 = "Caring for and helping nonhh members, n.e.c.*",
                                 t050101 = "Work, main job",
                                 t050102 = "Work, other job(s)",
                                 t050103 = "Security procedures related to work",
                                 t050104 = "Waiting associated with working",
                                 t050199 = "Working, n.e.c.*",
                                 t050202 = "Eating and drinking as part of job",
                                 t050205 = "Waiting associated with work-related activities",
                                 t050299 = "Work-related activities, n.e.c.*",
                                 t050301 = "Income-generating hobbies, crafts, and food",
                                 t050302 = "Income-generating performances",
                                 t050303 = "Income-generating services",
                                 t050304 = "Income-generating rental property activities",
                                 t050399 = "Other income-generating activities, n.e.c.*",
                                 t050401 = "Job search activities",
                                 t050403 = "Job interviewing",
                                 t050404 = "Waiting associated with job search or interview",
                                 t059999 = "Work and work-related activities, n.e.c.*",
                                 t060101 = "Taking class for degree, certification, or licensure",
                                 t060102 = "Taking class for personal interest",
                                 t060103 = "Waiting associated with taking classes",
                                 t060199 = "Taking class, n.e.c.*",
                                 t060201 = "Extracurricular club activities",
                                 t060202 = "Extracurricular music and performance activities",
                                 t060204 = "Waiting associated with extracurricular activities",
                                 t060301 = "Research/homework for class for degree, certification, or licensure",
                                 t060302 = "Research/homework for class for pers. interest",
                                 t060399 = "Research/homework n.e.c.*",
                                 t060401 = "Administrative activities: class for degree, certification, or licensure",
                                 t060402 = "Administrative activities: class for personal interest",
                                 t060499 = "Administrative for education, n.e.c.*",
                                 t069999 = "Education, n.e.c.*",
                                 t070101 = "Grocery shopping",
                                 t070102 = "Purchasing gas",
                                 t070103 = "Purchasing food (not groceries)",
                                 t070104 = "Shopping, except groceries, food and gas",
                                 t070105 = "Waiting associated with shopping",
                                 t070199 = "Shopping, n.e.c.*",
                                 t070201 = "Comparison shopping",
                                 t070301 = "Security procedures rel. to consumer purchases",
                                 t080101 = "Using paid childcare services",
                                 t080201 = "Banking",
                                 t080202 = "Using other financial services",
                                 t080203 = "Waiting associated w/banking/financial services",
                                 t080301 = "Using legal services",
                                 t080399 = "Using legal services, n.e.c.*",
                                 t080401 = "Using health and care services outside the home",
                                 t080402 = "Using in-home health and care services",
                                 t080403 = "Waiting associated with medical services",
                                 t080501 = "Using personal care services",
                                 t080502 = "Waiting associated w/personal care services",
                                 t080601 = "Activities rel. to purchasing/selling real estate",
                                 t080701 = "Using veterinary services",
                                 t080702 = "Waiting associated with veterinary services",
                                 t089999 = "Professional and personal services, n.e.c.*",
                                 t090101 = "Using interior cleaning services",
                                 t090103 = "Using clothing repair and cleaning services",
                                 t090104 = "Waiting associated with using household services",
                                 t090199 = "Using household services, n.e.c.*",
                                 t090201 = "Using home maint/repair/d???cor/construction svcs",
                                 t090202 = "Waiting associated w/ home main/repair/d???cor/constr",
                                 t090299 = "Using home maint/repair/d???cor/constr services, n.e.c.*",
                                 t090302 = "Waiting associated with pet services",
                                 t090399 = "Using pet services, n.e.c.*",
                                 t090401 = "Using lawn and garden services",
                                 t090402 = "Waiting associated with using lawn and garden services",
                                 t090501 = "Using vehicle maintenance or repair services",
                                 t090502 = "Waiting associated with vehicle main. or repair svcs",
                                 t090599 = "Using vehicle maint. and repair svcs, n.e.c.*",
                                 t099999 = "Using household services, n.e.c.*",
                                 t100101 = "Using police and fire services",
                                 t100102 = "Using social services",
                                 t100103 = "Obtaining licenses and paying fines, fees, taxes",
                                 t100199 = "Using government services, n.e.c.*",
                                 t100201 = "Civic obligations and participation",
                                 t100299 = "Civic obligations and participation, n.e.c.*",
                                 t100304 = "Waiting associated with using government services",
                                 t110101 = "Eating and drinking",
                                 t110201 = "Waiting associated w/eating and drinking",
                                 t120101 = "Socializing and communicating with others",
                                 t120201 = "Attending or hosting parties/receptions/ceremonies",
                                 t120202 = "Attending meetings for personal interest (not volunteering)",
                                 t120299 = "Attending/hosting social events, n.e.c.*",
                                 t120301 = "Relaxing, thinking",
                                 t120302 = "Tobacco and drug use",
                                 t120303 = "Television and movies (not religious)",
                                 t120304 = "Television (religious)",
                                 t120305 = "Listening to the radio",
                                 t120306 = "Listening to/playing music (not radio)",
                                 t120307 = "Playing games",
                                 t120308 = "Computer use for leisure (exc. Games)",
                                 t120309 = "Arts and crafts as a hobby",
                                 t120310 = "Collecting as a hobby",
                                 t120311 = "Hobbies, except arts and crafts and collecting",
                                 t120312 = "Reading for personal interest",
                                 t120313 = "Writing for personal interest",
                                 t120399 = "Relaxing and leisure, n.e.c.*",
                                 t120401 = "Attending performing arts",
                                 t120402 = "Attending museums",
                                 t120403 = "Attending movies/film",
                                 t120404 = "Attending gambling establishments",
                                 t120405 = "Security procedures rel. to arts and entertainment",
                                 t120499 = "Arts and entertainment, n.e.c.*",
                                 t120501 = "Waiting assoc. w/socializing and communicating",
                                 t120502 = "Waiting assoc. w/attending/hosting social events",
                                 t120503 = "Waiting associated with relaxing/leisure",
                                 t120504 = "Waiting associated with arts and entertainment",
                                 t129999 = "Socializing, relaxing, and leisure, n.e.c.*",
                                 t130101 = "Doing aerobics",
                                 t130102 = "Playing baseball",
                                 t130103 = "Playing basketball",
                                 t130104 = "Biking",
                                 t130105 = "Playing billiards",
                                 t130106 = "Boating",
                                 t130107 = "Bowling",
                                 t130108 = "Climbing, spelunking, caving",
                                 t130109 = "Dancing",
                                 t130110 = "Participating in equestrian sports",
                                 t130112 = "Fishing",
                                 t130113 = "Playing football",
                                 t130114 = "Golfing",
                                 t130116 = "Hiking",
                                 t130117 = "Playing hockey",
                                 t130118 = "Hunting",
                                 t130119 = "Participating in martial arts",
                                 t130120 = "Playing racquet sports",
                                 t130122 = "Rollerblading",
                                 t130124 = "Running",
                                 t130125 = "Skiing, ice skating, snowboarding",
                                 t130126 = "Playing soccer",
                                 t130127 = "Softball",
                                 t130128 = "Using cardiovascular equipment",
                                 t130129 = "Vehicle touring/racing",
                                 t130130 = "Playing volleyball",
                                 t130131 = "Walking",
                                 t130132 = "Participating in water sports",
                                 t130133 = "Weightlifting/strength training",
                                 t130134 = "Working out, unspecified",
                                 t130136 = "Doing yoga",
                                 t130199 = "Playing sports n.e.c.*",
                                 t130202 = "Watching baseball",
                                 t130203 = "Watching basketball",
                                 t130207 = "Watching bowling",
                                 t130209 = "Watching dancing",
                                 t130210 = "Watching equestrian sports",
                                 t130213 = "Watching football",
                                 t130214 = "Watching golfing",
                                 t130216 = "Watching hockey",
                                 t130218 = "Watching racquet sports",
                                 t130219 = "Watching rodeo competitions",
                                 t130222 = "Watching running",
                                 t130224 = "Watching soccer",
                                 t130225 = "Watching softball",
                                 t130226 = "Watching vehicle touring/racing",
                                 t130227 = "Watching volleyball",
                                 t130232 = "Watching wrestling",
                                 t130299 = "Attending sporting events, n.e.c.*",
                                 t130301 = "Waiting related to playing sports or exercising",
                                 t130302 = "Waiting related to attending sporting events",
                                 t130399 = "Waiting associated with sports, exercise, and recreation, n.e.c.*",
                                 t130402 = "Security related to attending sporting events",
                                 t139999 = "Sports, exercise, and recreation, n.e.c.*",
                                 t140101 = "Attending religious services",
                                 t140102 = "Participation in religious practices",
                                 t140103 = "Waiting associated w/religious and spiritual activities",
                                 t140105 = "Religious education activities",
                                 t149999 = "Religious and spiritual activities, n.e.c.*",
                                 t150101 = "Computer use",
                                 t150102 = "Organizing and preparing",
                                 t150103 = "Reading",
                                 t150104 = "Telephone calls (except hotline counseling)",
                                 t150105 = "Writing",
                                 t150106 = "Fundraising",
                                 t150199 = "Administrative and support activities, n.e.c.*",
                                 t150201 = "Food preparation, presentation, clean-up",
                                 t150202 = "Collecting and delivering clothing and other goods",
                                 t150203 = "Providing care",
                                 t150204 = "Teaching, leading, counseling, mentoring",
                                 t150299 = "Social service and care activities, n.e.c.*",
                                 t150301 = "Building houses, wildlife sites, and other structures",
                                 t150302 = "Indoor and outdoor maintenance, repair, and clean-up",
                                 t150399 = "Indoor and outdoor maintenance, building and clean-up activities, n.e.c.*",
                                 t150401 = "Performing",
                                 t150402 = "Serving at volunteer events and cultural activities",
                                 t150499 = "Participating in performance and cultural activities, n.e.c.*",
                                 t150501 = "Attending meetings, conferences, and training",
                                 t150602 = "Public safety activities",
                                 t150699 = "Public health and safety activities, n.e.c.*",
                                 t150701 = "Waiting associated with volunteer activities",
                                 t150799 = "Waiting associated with volunteer activities, n.e.c.*",
                                 t150801 = "Security procedures related to volunteer activities",
                                 t150899 = "Security procedures related to volunteer activities, n.e.c.*",
                                 t159999 = "Volunteer activities, n.e.c.*",
                                 t160101 = "Telephone calls to/from family members",
                                 t160102 = "Telephone calls to/from friends, neighbors, or acquaintances",
                                 t160103 = "Telephone calls to/from education services providers",
                                 t160104 = "Telephone calls to/from salespeople",
                                 t160105 = "Telephone calls to/from professional or personal care svcs providers",
                                 t160106 = "Telephone calls to/from household services providers",
                                 t160107 = "Telephone calls to/from paid child or adult care providers",
                                 t160108 = "Telephone calls to/from government officials",
                                 t160199 = "Telephone calls (to or from), n.e.c.*",
                                 t160201 = "Waiting associated with telephone calls",
                                 t169999 = "Telephone calls, n.e.c.*",
                                 t180101 = "Travel related to personal care",
                                 t180201 = "Travel related to housework",
                                 t180202 = "Travel related to food and drink prep., clean-up, and presentation",
                                 t180203 = "Travel related to interior maintenance, repair, and decoration",
                                 t180204 = "Travel related to exterior maintenance, repair, and decoration",
                                 t180205 = "Travel related to lawn, garden, and houseplant care",
                                 t180206 = "Travel related to care for animals and pets (not vet care)",
                                 t180207 = "Travel related to vehicle care and maintenance (by self)",
                                 t180208 = "Travel related to appliance, tool, and toy set-up, repair, and maintenance (by self)",
                                 t180209 = "Travel related to household management",
                                 t180299 = "Travel related to household activities, n.e.c.*",
                                 t180301 = "Travel related to caring for and helping hh children",
                                 t180302 = "Travel related to hh children's education",
                                 t180303 = "Travel related to hh children's health",
                                 t180304 = "Travel related to caring for hh adults",
                                 t180305 = "Travel related to helping hh adults",
                                 t180401 = "Travel related to caring for and helping nonhh children",
                                 t180402 = "Travel related to nonhh children's education",
                                 t180403 = "Travel related to nonhh children's health",
                                 t180404 = "Travel related to caring for nonhh adults",
                                 t180405 = "Travel related to helping nonhh adults",
                                 t180499 = "Travel rel. to caring for and helping nonhh members, n.e.c.*",
                                 t180501 = "Travel related to working",
                                 t180502 = "Travel related to work-related activities",
                                 t180503 = "Travel related to income-generating activities",
                                 t180504 = "Travel related to job search and interviewing",
                                 t180599 = "Travel related to work, n.e.c.*",
                                 t180601 = "Travel related to taking class",
                                 t180602 = "Travel related to extracurricular activities (ex. Sports)",
                                 t180603 = "Travel related to research/homework",
                                 t180604 = "Travel related to registration/administrative activities",
                                 t180699 = "Travel related to education, n.e.c.*",
                                 t180701 = "Travel related to grocery shopping",
                                 t180702 = "Travel related to purchasing gas",
                                 t180703 = "Travel related to purchasing food (not groceries)",
                                 t180704 = "Travel related to shopping, ex groceries, food, and gas",
                                 t180799 = "Travel related to consumer purchases, n.e.c.*",
                                 t180801 = "Travel related to using childcare services",
                                 t180802 = "Travel related to using financial services and banking",
                                 t180803 = "Travel related to using legal services",
                                 t180804 = "Travel related to using medical services",
                                 t180805 = "Travel related to using personal care services",
                                 t180806 = "Travel related to using real estate services",
                                 t180807 = "Travel related to using veterinary services",
                                 t180899 = "Travel rel. to using prof. and personal care services, n.e.c.*",
                                 t180901 = "Travel related to using household services",
                                 t180902 = "Travel related to using home main./repair/d???cor./construction svcs",
                                 t180903 = "Travel related to using pet services (not vet)",
                                 t180905 = "Travel related to using vehicle maintenance and repair services",
                                 t181001 = "Travel related to using government services",
                                 t181002 = "Travel related to civic obligations and participation",
                                 t181101 = "Travel related to eating and drinking",
                                 t181201 = "Travel related to socializing and communicating",
                                 t181202 = "Travel related to attending or hosting social events",
                                 t181203 = "Travel related to relaxing and leisure",
                                 t181204 = "Travel related to arts and entertainment",
                                 t181205 = "Travel as a form of entertainment",
                                 t181299 = "Travel rel. to socializing, relaxing, and leisure, n.e.c.*",
                                 t181301 = "Travel related to participating in sports/exercise/recreation",
                                 t181302 = "Travel related to attending sporting/recreational events",
                                 t181399 = "Travel related to sports, exercise, and recreation, n.e.c.*",
                                 t181401 = "Travel related to religious/spiritual practices",
                                 t181499 = "Travel rel. to religious/spiritual activities, n.e.c.*",
                                 t181501 = "Travel related to volunteering",
                                 t181599 = "Travel related to volunteer activities, n.e.c.*",
                                 t181601 = "Travel related to phone calls",
                                 t181801 = "Security procedures related to traveling",
                                 t181899 = "Security procedures related to traveling, n.e.c.*",
                                 t189999 = "Traveling, n.e.c.*",
                                 t500101 = "Insufficient detail in verbatim",
                                 t500103 = "Missing travel or destination",
                                 t500105 = "Respondent refused to provide information/'none of your business'",
                                 t500106 = "Gap/can't remember",
                                 t500107 = "Unable to code activity at 1st tier",
                                 t040203 = "Home schooling of nonhh children",                                          
                                 t050201 = "Socializing, relaxing, and leisure as part of job",                                                           t050203 = "Sports and exercise as part of job",                                                                              
                                 t050305 = "Waiting associated with other income-generating activities",                                                                              
                                 t050499 = "Job search and Interviewing, n.e.c.*",                                                                            
                                 t060203 = "Extracurricular student government activities",                                                                             
                                 t060299 =  "Education-related extracurricular activities, n.e.c.*",                                                                             
                                 t060303  = "Waiting associated with research/homework",                                                                            
                                 t080299  =  "Using financial services and banking, n.e.c.*",                                                                            
                                 t080499  = "Using medical services, n.e.c.*",                                                                             
                                 t080599  = "Using personal care services, n.e.c.*",                                                                            
                                 t080602  = "Waiting associated w/purchasing/selling real estate",                                                                            
                                 t080801  = "Security procedures rel. to professional/personal svcs.",                                                                            
                                 t100305   =  "Waiting associated with civic obligations and participation",                                                                           
                                 t120199   = "Socializing and communicating, n.e.c.*",                                                                            
                                 t130211  = "Watching fencing",                                                                         
                                 t130212 = "Watching fishing",                                                                             
                                 t130220 = "Watching rollerblading",                                                                             
                                 t130229 = "Watching water sports",                                                                             
                                 t150599  = "Attending meetings, conferences, and training, n.e.c.*",                                                                            
                                 t150601 = "Public health activities",                                                                             
                                 t180399 = "Travel rel. to caring for and helping hh members, n.e.c.*",
                                 t039999 = "Caring for and helping hh members, n.e.c.*"
                                 )
                                                      )

actsum_combined_categorized = actsum_combined %>% 
  mutate(category = case_when(
    activity_codes %in% c("Sleeping", 
                          "Sleeplessness") ~ "sleep", 
    activity_codes %in% c("Using pet services",
                          "Washing, dressing and grooming oneself",
                          "Grooming, n.e.c.*",
                          "Health-related self care",
                          "Self care, n.e.c.*",
                          "Personal/Private activities",
                          "Personal activities, n.e.c.*",
                          "Personal emergencies",
                          "Personal care, n.e.c.*",
                          "Public health activities" #unsure
                          ) ~ "personal_care",
    activity_codes %in% c("Interior cleaning",
                            "Laundry",
                            "Sewing, repairing, and maintaining textiles",
                            "Storing interior hh items, inc. food",
                            "Housework, n.e.c.*",
                            "Food and drink preparation",
                            "Food presentation",
                            "Kitchen and food clean-up",
                            "Food and drink prep, presentation, and clean-up, n.e.c.*",
                            "Interior arrangement, decoration, and repairs",
                            "Building and repairing furniture",
                            "Heating and cooling",
                            "Interior maintenance, repair, and decoration, n.e.c.*",
                            "Exterior cleaning",
                            "Exterior repair, improvements, and decoration",
                            "Exterior maintenance, repair and decoration, n.e.c.*",
                            "Lawn, garden, and houseplant care",
                            "Ponds, pools, and hot tubs",
                            "Lawn and garden, n.e.c.*",
                            "Care for animals and pets (not veterinary care)",
                            "Walking / exercising / playing with animals",
                            "Pet and animal care, n.e.c.*",
                            "Vehicle repair and maintenance (by self)",
                            "Vehicles, n.e.c.*",
                            "Appliance, tool, and toy set-up, repair, and maintenance (by self)",
                            "Appliances and tools, n.e.c.*",
                            "Financial management",
                            "Household and personal organization and planning",
                            "HH and personal mail and messages (except e-mail)",
                            "HH and personal e-mail and messages",
                            "Home security",
                            "Household management, n.e.c.*",
                            "Household activities, n.e.c.*") ~ "household", 
activity_codes %in% c("Physical care for hh children",
                            "Reading to/with hh children",
                            "Playing with hh children, not sports",
                            "Arts and crafts with hh children",
                            "Playing sports with hh children",
                            "Talking with/listening to hh children",
                            "Organization and planning for hh children",
                            "Looking after hh children (as a primary activity)",
                            "Attending hh children's events",
                            "Waiting for/with hh children",
                            "Picking up/dropping off hh children",
                            "Caring for and helping hh children, n.e.c.*",
                            "Homework (hh children)",
                            "Meetings and school conferences (hh children)",
                            "Home schooling of hh children",
                            "Waiting associated with hh children's education",
                            "Activities related to hh child's education, n.e.c.*",
                            "Providing medical care to hh children",
                            "Obtaining medical care for hh children",
                            "Waiting associated with hh children's health",
                            "Activities related to hh child's health, n.e.c.*",
                            "Physical care for hh adults",
                            "Looking after hh adult (as a primary activity)",
                            "Providing medical care to hh adult",
                            "Obtaining medical and care services for hh adult",
                            "Waiting associated with caring for household adults",
                            "Caring for household adults, n.e.c.*",
                            "Helping hh adults",
                            "Organization and planning for hh adults",
                            "Picking up/dropping off hh adult",
                            "Waiting associated with helping hh adults",
                            "Helping household adults, n.e.c.*",
                            "Physical care for nonhh children",
                            "Reading to/with nonhh children",
                            "Playing with nonhh children, not sports",
                            "Arts and crafts with nonhh children",
                            "Playing sports with nonhh children",
                            "Talking with/listening to nonhh children",
                            "Organization and planning for nonhh children",
                            "Looking after nonhh children (as primary activity)",
                            "Attending nonhh children's events",
                            "Waiting for/with nonhh children",
                            "Dropping off/picking up nonhh children",
                            "Caring for and helping nonhh children, n.e.c.*",
                            "Homework (nonhh children)",
                            "Meetings and school conferences (nonhh children)",
                            "Providing medical care to nonhh children",
                            "Obtaining medical care for nonhh children",
                            "Activities related to nonhh child's health, n.e.c.*",
                            "Physical care for nonhh adults",
                            "Looking after nonhh adult (as a primary activity)",
                            "Providing medical care to nonhh adult",
                            "Obtaining medical and care services for nonhh adult",
                            "Waiting associated with caring for nonhh adults",
                            "Caring for nonhh adults, n.e.c.*", 
                            "Housework, cooking, and shopping assistance for nonhh adults",
                            "House and lawn maintenance and repair assistance for nonhh adults",
                            "Animal and pet care assistance for nonhh adults",
                            "Vehicle and appliance maintenance/repair assistance for nonhh adults",
                            "Financial management assistance for nonhh adults",
                            "Household management and paperwork assistance for nonhh adults",
                            "Picking up/dropping off nonhh adult",
                            "Waiting associated with helping nonhh adults",
                            "Helping nonhh adults, n.e.c.*",
                            "Caring for and helping nonhh members, n.e.c.*",
                            "Home schooling of nonhh children",
                            "Caring for and helping hh members, n.e.c.*") ~ "caring_duties", 
activity_codes %in% c("Work, main job",
                            "Work, other job(s)",
                            "Security procedures related to work",
                            "Waiting associated with working",
                            "Working, n.e.c.*",
                            "Eating and drinking as part of job",
                            "Waiting associated with work-related activities",
                            "Work-related activities, n.e.c.*",
                            "Income-generating hobbies, crafts, and food",
                            "Income-generating performances",
                            "Income-generating services",
                            "Income-generating rental property activities",
                            "Other income-generating activities, n.e.c.*",
                            "Job search activities",
                            "Job interviewing",
                            "Waiting associated with job search or interview",
                            "Work and work-related activities, n.e.c.*",
                            "Socializing, relaxing, and leisure as part of job",
                            "Sports and exercise as part of job",
                             "Waiting associated with other income-generating activities", 
                            "Job search and Interviewing, n.e.c.*",
                             "Attending meetings, conferences, and training, n.e.c.*") ~ "work", 
activity_codes %in% c("Taking class for degree, certification, or licensure",
                            "Taking class for personal interest",
                            "Waiting associated with taking classes",
                            "Taking class, n.e.c.*",
                            "Extracurricular club activities",
                            "Extracurricular music and performance activities",
                            "Waiting associated with extracurricular activities",
                            "Research/homework for class for degree, certification, or licensure",
                            "Research/homework for class for pers. interest",
                            "Research/homework n.e.c.*",
                            "Administrative activities: class for degree, certification, or licensure",
                            "Administrative activities: class for personal interest",
                            "Administrative for education, n.e.c.*",
                            "Education, n.e.c.*",
                            "Extracurricular student government activities", 
                            "Education-related extracurricular activities, n.e.c.*",   
                            "Waiting associated with research/homework") ~ "education", 
activity_codes %in% c("Grocery shopping",
                            "Purchasing gas",
                            "Purchasing food (not groceries)",
                            "Shopping, except groceries, food and gas",
                            "Waiting associated with shopping",
                            "Shopping, n.e.c.*",
                            "Comparison shopping") ~ "shopping", 
activity_codes %in% c("Security procedures rel. to consumer purchases",
                            "Using paid childcare services",
                            "Banking",
                            "Using other financial services",
                            "Waiting associated w/banking/financial services",
                            "Using legal services",
                            "Using legal services, n.e.c.*",
                            "Using health and care services outside the home",
                            "Using in-home health and care services",
                            "Waiting associated with medical services",
                            "Using personal care services",
                            "Waiting associated w/personal care services",
                            "Activities rel. to purchasing/selling real estate",
                            "Using veterinary services",
                            "Waiting associated with veterinary services",
                            "Professional and personal services, n.e.c.*",
                            "Using interior cleaning services",
                            "Using clothing repair and cleaning services",
                            "Waiting associated with using household services",
                            "Using household services, n.e.c.*",
                            "Using home maint/repair/d???cor/construction svcs",
                            "Waiting associated w/ home main/repair/d???cor/constr",
                            "Using home maint/repair/d???cor/constr services, n.e.c.*",
                            "Waiting associated with pet services",
                            "Using pet services, n.e.c.*",
                            "Using lawn and garden services",
                            "Waiting associated with using lawn and garden services",
                            "Using vehicle maintenance or repair services",
                            "Waiting associated with vehicle main. or repair svcs",
                            "Using vehicle maint. and repair svcs, n.e.c.*",
                            "Using household services, n.e.c.*",
                            "Using police and fire services",
                            "Using social services",
                            "Obtaining licenses and paying fines, fees, taxes",
                            "Using financial services and banking, n.e.c.*",
                             "Using medical services, n.e.c.*", 
                             "Using personal care services, n.e.c.*",
                             "Waiting associated w/purchasing/selling real estate", 
                            "Security procedures rel. to professional/personal svcs.") ~ "professional_services", 
activity_codes %in% c("Using government services, n.e.c.*",
                            "Civic obligations and participation",
                            "Civic obligations and participation, n.e.c.*",
                            "Waiting associated with using government services",
                             "Waiting associated with civic obligations and participation") ~ "gov_civic_obligations", 
activity_codes %in% c("Eating and drinking",
                            "Waiting associated w/eating and drinking") ~ "eating_drinking", 
activity_codes %in% c("Socializing and communicating with others",
                      "Attending or hosting parties/receptions/ceremonies",
                      "Attending meetings for personal interest (not volunteering)",
                      "Attending/hosting social events, n.e.c.*",
                      "Relaxing, thinking",
                      "Tobacco and drug use",
                      "Television and movies (not religious)",
                      "Television (religious)",
                      "Listening to the radio",
                      "Listening to/playing music (not radio)",
                      "Playing games",
                      "Computer use for leisure (exc. Games)",
                      "Arts and crafts as a hobby",
                      "Collecting as a hobby",
                      "Hobbies, except arts and crafts and collecting",
                      "Reading for personal interest",
                      "Writing for personal interest",
                      "Relaxing and leisure, n.e.c.*",
                      "Attending performing arts",
                      "Attending museums",
                      "Attending movies/film",
                      "Attending gambling establishments",
                      "Security procedures rel. to arts and entertainment",
                      "Arts and entertainment, n.e.c.*",
                      "Watching baseball",
                      "Watching basketball",
                      "Watching bowling",
                      "Watching dancing",
                      "Watching equestrian sports",
                      "Watching football",
                      "Watching golfing",
                      "Watching hockey",
                      "Watching racquet sports",
                      "Watching rodeo competitions",
                      "Watching running",
                      "Watching soccer",
                      "Watching softball",
                      "Watching vehicle touring/racing",
                      "Watching volleyball",
                      "Watching wrestling",
                      "Attending sporting events, n.e.c.*",
                      "Waiting assoc. w/socializing and communicating",
                      "Waiting assoc. w/attending/hosting social events",
                      "Waiting associated with relaxing/leisure",
                      "Waiting associated with arts and entertainment",
                      "Socializing, relaxing, and leisure, n.e.c.*",
                      "Socializing and communicating, n.e.c.*",
                      "Watching fencing",   
                      "Watching fishing",       
                      "Watching rollerblading", 
                       "Watching water sports") ~ "leisure", 
activity_codes %in% c("Doing aerobics",
                      "Playing baseball",
                      "Playing basketball",
                      "Biking",
                      "Playing billiards",
                      "Boating",
                      "Bowling",
                      "Climbing, spelunking, caving",
                      "Dancing",
                      "Participating in equestrian sports",
                      "Fishing",
                      "Playing football",
                      "Golfing",
                      "Hiking",
                      "Playing hockey",
                      "Hunting",
                      "Participating in martial arts",
                      "Playing racquet sports",
                      "Rollerblading",
                      "Running",
                      "Skiing, ice skating, snowboarding",
                      "Playing soccer",
                      "Softball",
                      "Using cardiovascular equipment",
                      "Vehicle touring/racing",
                      "Playing volleyball",
                      "Walking",
                      "Participating in water sports",
                      "Weightlifting/strength training",
                      "Working out, unspecified",
                      "Doing yoga",
                      "Playing sports n.e.c.*",
                      "Waiting related to playing sports or exercising",
                      "Waiting related to attending sporting events",
                      "Waiting associated with sports, exercise, and recreation, n.e.c.*",
                      "Security related to attending sporting events",
                      "Sports, exercise, and recreation, n.e.c.*") ~ "exercise", 
activity_codes %in% c("Attending religious services",
                      "Participation in religious practices",
                      "Waiting associated w/religious and spiritual activities",
                      "Religious education activities",
                      "Religious and spiritual activities, n.e.c.*") ~ "religious_spiritual", 
activity_codes %in% c("Computer use",
                      "Organizing and preparing",
                      "Reading",
                      "Telephone calls (except hotline counseling)",
                      "Writing",
                      "Fundraising",
                      "Administrative and support activities, n.e.c.*",
                      "Food preparation, presentation, clean-up",
                      "Collecting and delivering clothing and other goods",
                      "Providing care",
                      "Teaching, leading, counseling, mentoring",
                      "Social service and care activities, n.e.c.*",
                      "Building houses, wildlife sites, and other structures",
                      "Indoor and outdoor maintenance, repair, and clean-up",
                      "Indoor and outdoor maintenance, building and clean-up activities, n.e.c.*",
                      "Performing",
                      "Serving at volunteer events and cultural activities",
                      "Participating in performance and cultural activities, n.e.c.*",
                      "Attending meetings, conferences, and training",
                      "Public safety activities",
                      "Public health and safety activities, n.e.c.*",
                      "Waiting associated with volunteer activities",
                      "Waiting associated with volunteer activities, n.e.c.*",
                      "Security procedures related to volunteer activities",
                      "Security procedures related to volunteer activities, n.e.c.*",
                      "Volunteer activities, n.e.c.*") ~ "volunteer", 
activity_codes %in% c("Telephone calls to/from family members",
                      "Telephone calls to/from friends, neighbors, or acquaintances",
                      "Telephone calls to/from education services providers",
                      "Telephone calls to/from salespeople",
                      "Telephone calls to/from professional or personal care svcs providers",
                      "Telephone calls to/from household services providers",
                      "Telephone calls to/from paid child or adult care providers",
                      "Telephone calls to/from government officials",
                      "Telephone calls (to or from), n.e.c.*",
                      "Waiting associated with telephone calls",
                      "Telephone calls, n.e.c.*") ~ "telephone", 
activity_codes %in% c("Travel related to personal care",
                      "Travel related to housework",
                      "Travel related to food and drink prep., clean-up, and presentation",
                      "Travel related to interior maintenance, repair, and decoration",
                      "Travel related to exterior maintenance, repair, and decoration",
                      "Travel related to lawn, garden, and houseplant care",
                      "Travel related to care for animals and pets (not vet care)",
                      "Travel related to vehicle care and maintenance (by self)",
                      "Travel related to appliance, tool, and toy set-up, repair, and maintenance (by self)",
                      "Travel related to household management",
                      "Travel related to household activities, n.e.c.*",
                      "Travel related to caring for and helping hh children",
                      "Travel related to hh children's education",
                      "Travel related to hh children's health",
                      "Travel related to caring for hh adults",
                      "Travel related to helping hh adults",
                      "Travel related to caring for and helping nonhh children",
                      "Travel related to nonhh children's education",
                      "Travel related to nonhh children's health",
                      "Travel related to caring for nonhh adults",
                      "Travel related to helping nonhh adults",
                      "Travel rel. to caring for and helping nonhh members, n.e.c.*",
                      "Travel related to working",
                      "Travel related to work-related activities",
                      "Travel related to income-generating activities",
                      "Travel related to job search and interviewing",
                      "Travel related to work, n.e.c.*",
                      "Travel related to taking class",
                      "Travel related to extracurricular activities (ex. Sports)",
                      "Travel related to research/homework",
                      "Travel related to registration/administrative activities",
                      "Travel related to education, n.e.c.*",
                      "Travel related to grocery shopping",
                      "Travel related to purchasing gas",
                      "Travel related to purchasing food (not groceries)",
                      "Travel related to shopping, ex groceries, food, and gas",
                      "Travel related to consumer purchases, n.e.c.*",
                      "Travel related to using childcare services",
                      "Travel related to using financial services and banking",
                      "Travel related to using legal services",
                      "Travel related to using medical services",
                      "Travel related to using personal care services",
                      "Travel related to using real estate services",
                      "Travel related to using veterinary services",
                      "Travel rel. to using prof. and personal care services, n.e.c.*",
                      "Travel related to using household services",
                      "Travel related to using home main./repair/d???cor./construction svcs",
                      "Travel related to using pet services (not vet)",
                      "Travel related to using vehicle maintenance and repair services",
                      "Travel related to using government services",
                      "Travel related to civic obligations and participation",
                      "Travel related to eating and drinking",
                      "Travel related to socializing and communicating",
                      "Travel related to attending or hosting social events",
                      "Travel related to relaxing and leisure",
                      "Travel related to arts and entertainment",
                      "Travel as a form of entertainment",
                      "Travel rel. to socializing, relaxing, and leisure, n.e.c.*",
                      "Travel related to participating in sports/exercise/recreation",
                      "Travel related to attending sporting/recreational events",
                      "Travel related to sports, exercise, and recreation, n.e.c.*",
                      "Travel related to religious/spiritual practices",
                      "Travel rel. to religious/spiritual activities, n.e.c.*",
                      "Travel related to volunteering",
                      "Travel related to volunteer activities, n.e.c.*",
                      "Travel related to phone calls",
                      "Security procedures related to traveling",
                      "Security procedures related to traveling, n.e.c.*",
                      "Traveling, n.e.c.*",
                      "Travel rel. to caring for and helping hh members, n.e.c.*") ~ "traveling"
))
```

Let???s read in and tidy the cps files that have the geographic
information. First we???ll select the appropriate columns from cps and
then rename the columns to be more useful. Then we???ll recode the column
values.

``` r
cps_2020_sub = cps_2020 %>% #select geographic variables
  select(TUCASEID:GTCO, PRTAGE, PESEX) %>% 
  mutate(year = 2020)

cps_2019_sub = cps_2019 %>% #select geographic variables
  select(TUCASEID:GTCO, PRTAGE, PESEX) %>% 
  mutate(year = 2019)

#stack the 2019 and 2020 cps datasets

cps_combined = rbind(cps_2019_sub, cps_2020_sub) %>% 
  rename(division = GEDIV, 
         region = GEREG, 
         state = GESTFIPS, 
         metro_area = GTCBSA, 
         metro_status = GTMETSTA, 
         county = GTCO,
         age = PRTAGE, 
         sex = PESEX) %>% 
  mutate(state = recode(state,
`1 ` = "AL", `17`= "IL", `30` = "MT", `44` = "RI",
`2 ` = "AK", `18`= "IN", `31` = "NE", `45` = "SC",
`4 ` = "AZ", `19`= "IA", `32` = "NV", `46` = "SD",
`5 ` = "AR", `20`= "KS", `33` = "NH", `47` = "TN",
`6 ` = "CA", `21`= "KY", `34` = "NJ", `48` = "TX",
`8 ` = "CO", `22`= "LA", `35` = "NM", `49` = "UT",
`9 ` = "CT", `23`= "ME", `36` = "NY", `50` = "VT",
`10` = "DE", `24`= "MD", `37` = "NC", `51` = "VA",
`11` = "DC", `25`= "MA", `38` = "ND", `53` = "WA",
`12` = "FL", `26`= "MI", `39` = "OH", `54` = "WV",
`13` = "GA", `27`= "MN", `40` = "OK", `55` = "WI",
`15` = "HI", `28`= "MS", `41` = "OR", `56` = "WY",
`16` = "ID", `29`= "MO", `42` = "PA"), 
        region = recode(region, 
                        `1` = "Northeast", 
                        `2` = "Midwest", 
                        `3` = "South", 
                        `4` = "West"), 
        division = recode(division, 
                          `1` = "New England",
                          `2` = "Middle Atlantic",
                          `3` = "East North Central",
                          `4` = "West North Central",
                          `5` = "South Atlantic",
                          `6` = "East South Central",
                          `7` = "West South Central",
                          `8` = "Mountain",
                          `9` = "Pacific"
                          ), 
        metro_status = recode(metro_status, 
                            `1` = "metropolitan", 
                            `2` = "non-metropolitan", 
                            `3` = "not identified")) 
```

Merge the cps and summary datafiles and then group the merged dataset by
case ID (TUCASEID) and category.

``` r
cps_summary_merged = left_join(actsum_combined_categorized, cps_combined, by = c("TUCASEID", "age", "year", "sex"))

summary_household_category <- cps_summary_merged %>% #each row is a unique subject ID with unique category, category_sum_min: the sum of mins a subject spent on a specific category during that year; category_sum_hour: the sum of hours a subject spent on a specific category during that year
  group_by(TUCASEID,category) %>% 
  mutate(category_sum_min = sum(total_minutes), 
         category_sum_hour = category_sum_min/60, 
         category_sum_hour_weight = category_sum_hour*weight) %>% 
  filter(row_number() == 1) %>%
  select(-c("activity_codes","total_minutes"))
```

Let???s create a table showing average daily hours spent on each category
of activity (among those who engage in the activity), and the % change
from 2019 to 2020.

``` r
#check
category_time = 
 summary_household_category %>% 
  drop_na() %>% 
  filter(category_sum_min > 0) %>% 
  group_by(year, category) %>% 
  summarize(sum_product = sum(category_sum_hour_weight), 
            sum_weight = sum(weight)) %>% 
  mutate(average_hours = round(sum_product/sum_weight, 1)) %>% 
  select(-sum_product, -sum_weight) %>% 
  pivot_wider(names_from = "year", 
              values_from = "average_hours") %>% 
  mutate(percent_change = round((`2020`-`2019`)*100/`2019`, 1)) %>% 
  arrange(desc(`2019`))
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

``` r
print(category_time)
```

    ## # A tibble: 16 ?? 4
    ##    category              `2019` `2020` percent_change
    ##    <chr>                  <dbl>  <dbl>          <dbl>
    ##  1 sleep                    9      9.1            1.1
    ##  2 work                     7.6    7.6            0  
    ##  3 education                5.8    5.3           -8.6
    ##  4 leisure                  5.1    5.4            5.9
    ##  5 household                2.3    2.5            8.7
    ##  6 volunteer                2.3    2            -13  
    ##  7 caring_duties            1.7    2             17.6
    ##  8 exercise                 1.7    1.5          -11.8
    ##  9 religious_spiritual      1.5    1.4           -6.7
    ## 10 traveling                1.4    1.2          -14.3
    ## 11 eating_drinking          1.1    1.1            0  
    ## 12 professional_services    1.1    1.3           18.2
    ## 13 personal_care            0.9    0.9            0  
    ## 14 shopping                 0.9    0.8          -11.1
    ## 15 gov_civic_obligations    0.8    0.4          -50  
    ## 16 telephone                0.8    1.1           37.5

## Demographic table

``` r
#race, age, sex, labor force status, and state 
#First we need to recode race, sex, and labor force status
library(table1)
table(summary_household_category$race)
```

    ## 
    ##      1      2      3      4      5      6      7      8      9     10     11 
    ## 249101  39151   2465  14161    578   1173   1666    748     51    153     51 
    ##     12     13     15     16     17     19     21     23 
    ##     34     17     34    170     17     34     68     17

``` r
summary_household_category$race <- # Need to review race categorization
  as_factor(case_when(
    summary_household_category$race %in% c(4, 7) ~ "Asian",
    summary_household_category$race %in% c(1) ~ "White", 
    summary_household_category$race %in% c(2, 6) ~ "Black",
    summary_household_category$race %in% c(5) ~ "Hawaiian/Pacific Islander",
    TRUE ~ "unknown",
    summary_household_category$race %in% c(1) ~ "White",
    summary_household_category$race %in% c(2) ~ "Black", 
    summary_household_category$race %in% c(3) ~ "American Indian",
    summary_household_category$race %in% c(4) ~ "Asian",
    summary_household_category$race %in% c(5) ~ "Hawaiian/Pacific Islander",
    TRUE ~ "2+ races"
  ))

summary_household_category$labor_force_status <-
  as_factor(case_when(
    summary_household_category$labor_force_status == 1 ~ "Employed-At work",
    summary_household_category$labor_force_status == 2 ~ "Employed-Absent",
    summary_household_category$labor_force_status == 3 ~ "Unemployed-On layoff",
    summary_household_category$labor_force_status == 4 ~ "Unemployed-Looking",
    summary_household_category$labor_force_status == 5 ~ "Retired",
    TRUE ~ "unknown"
  ))

summary_household_category$sex <-
  as_factor(case_when(
    summary_household_category$sex == 1 ~ "Male",
    summary_household_category$sex == 2 ~ "Female",
    TRUE ~ "unknown"
  ))

unique_household <- summary_household_category %>%
  group_by(TUCASEID) %>% 
  filter(row_number() == 1) 

table1(~ race + age + sex + labor_force_status + region|year, data = unique_household)
```

    ## Warning in table1.formula(~race + age + sex + labor_force_status + region | :
    ## Terms to the right of '|' in formula 'x' define table columns and are expected
    ## to be factors with meaningful labels.

<div class="Rtable1"><table class="Rtable1">
<thead>
<tr>
<th class='rowlabel firstrow lastrow'></th>
<th class='firstrow lastrow'><span class='stratlabel'>2019<br><span class='stratn'>(N=9435)</span></span></th>
<th class='firstrow lastrow'><span class='stratlabel'>2020<br><span class='stratn'>(N=8782)</span></span></th>
<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=18217)</span></span></th>
</tr>
</thead>
<tbody>
<tr>
<td class='rowlabel firstrow'>race</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Black</td>
<td>1273 (13.5%)</td>
<td>1099 (12.5%)</td>
<td>2372 (13.0%)</td>
</tr>
<tr>
<td class='rowlabel'>White</td>
<td>7580 (80.3%)</td>
<td>7073 (80.5%)</td>
<td>14653 (80.4%)</td>
</tr>
<tr>
<td class='rowlabel'>Asian</td>
<td>458 (4.9%)</td>
<td>473 (5.4%)</td>
<td>931 (5.1%)</td>
</tr>
<tr>
<td class='rowlabel'>unknown</td>
<td>105 (1.1%)</td>
<td>122 (1.4%)</td>
<td>227 (1.2%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Hawaiian/Pacific Islander</td>
<td class='lastrow'>19 (0.2%)</td>
<td class='lastrow'>15 (0.2%)</td>
<td class='lastrow'>34 (0.2%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>age</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Mean (SD)</td>
<td>51.1 (18.2)</td>
<td>51.2 (18.3)</td>
<td>51.1 (18.2)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Median [Min, Max]</td>
<td class='lastrow'>51.0 [15.0, 85.0]</td>
<td class='lastrow'>52.0 [15.0, 85.0]</td>
<td class='lastrow'>52.0 [15.0, 85.0]</td>
</tr>
<tr>
<td class='rowlabel firstrow'>sex</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Female</td>
<td>5124 (54.3%)</td>
<td>4732 (53.9%)</td>
<td>9856 (54.1%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Male</td>
<td class='lastrow'>4311 (45.7%)</td>
<td class='lastrow'>4050 (46.1%)</td>
<td class='lastrow'>8361 (45.9%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>labor_force_status</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Retired</td>
<td>3448 (36.5%)</td>
<td>3383 (38.5%)</td>
<td>6831 (37.5%)</td>
</tr>
<tr>
<td class='rowlabel'>Employed-At work</td>
<td>5449 (57.8%)</td>
<td>4765 (54.3%)</td>
<td>10214 (56.1%)</td>
</tr>
<tr>
<td class='rowlabel'>Unemployed-Looking</td>
<td>242 (2.6%)</td>
<td>264 (3.0%)</td>
<td>506 (2.8%)</td>
</tr>
<tr>
<td class='rowlabel'>Employed-Absent</td>
<td>274 (2.9%)</td>
<td>290 (3.3%)</td>
<td>564 (3.1%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Unemployed-On layoff</td>
<td class='lastrow'>22 (0.2%)</td>
<td class='lastrow'>80 (0.9%)</td>
<td class='lastrow'>102 (0.6%)</td>
</tr>
<tr>
<td class='rowlabel firstrow'>region</td>
<td class='firstrow'></td>
<td class='firstrow'></td>
<td class='firstrow'></td>
</tr>
<tr>
<td class='rowlabel'>Midwest</td>
<td>1799 (19.1%)</td>
<td>1673 (19.1%)</td>
<td>3472 (19.1%)</td>
</tr>
<tr>
<td class='rowlabel'>Northeast</td>
<td>1277 (13.5%)</td>
<td>1188 (13.5%)</td>
<td>2465 (13.5%)</td>
</tr>
<tr>
<td class='rowlabel'>South</td>
<td>2953 (31.3%)</td>
<td>2652 (30.2%)</td>
<td>5605 (30.8%)</td>
</tr>
<tr>
<td class='rowlabel'>West</td>
<td>1667 (17.7%)</td>
<td>1639 (18.7%)</td>
<td>3306 (18.1%)</td>
</tr>
<tr>
<td class='rowlabel lastrow'>Missing</td>
<td class='lastrow'>1739 (18.4%)</td>
<td class='lastrow'>1630 (18.6%)</td>
<td class='lastrow'>3369 (18.5%)</td>
</tr>
</tbody>
</table>
</div>
