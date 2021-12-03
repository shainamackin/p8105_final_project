P8105 Final Project
================

``` r
library(tidyverse)
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
  select(TUCASEID, PTDTRACE, TEAGE, TELFS, TESEX, activity_codes, total_minutes) %>% #selected mostly demographic variables 
  mutate(year = 2020) %>% 
  rename(race = PTDTRACE, 
         labor_force_status = TELFS, 
         age = TEAGE, 
         sex = TESEX) %>%
  mutate(activity_codes = recode(activity_codes, t010101 = "Sleeping",
t010102 = "Sleeplessness",
t010201 = "Washing, dressing and grooming oneself",
t010299 = "Grooming, n.e.c.*",
t010301 = "Health-related self care",
t010399 = "Self care, n.e.c.*",
t010401 = "Personal/Private activities",
t010499 = "Personal activities, n.e.c.*",
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
t039999 = "Caring for and helping hh members, n.e.c.*",
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
t040203 = "Home schooling of nonhh children",
t040301 = "Providing medical care to nonhh children",
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
t050201 = "Socializing, relaxing, and leisure as part of job",
t050203 = "Sports and exercise as part of job",
t050299 = "Work-related activities, n.e.c.*",
t050301 = "Income-generating hobbies, crafts, and food",
t050302 = "Income-generating performances",
t050303 = "Income-generating services",
t050304 = "Income-generating rental property activities",
t050305 = "Waiting associated with other income-generating activities",
t050399 = "Other income-generating activities, n.e.c.*",
t050401 = "Job search activities",
t050403 = "Job interviewing",
t050404 = "Waiting associated with job search or interview",
t050499 = "Job search and Interviewing, n.e.c.*",
t060101 = "Taking class for degree, certification, or licensure",
t060102 = "Taking class for personal interest",
t060103 = "Waiting associated with taking classes",
t060199 = "Taking class, n.e.c.*",
t060201 = "Extracurricular club activities",
t060202 = "Extracurricular music and performance activities",
t060203 = "Extracurricular student government activities",
t060299 = "Education-related extracurricular activities, n.e.c.*",
t060301 = "Research/homework for class for degree, certification, or licensure",
t060302 = "Research/homework for class for pers. interest",
t060303 = "Waiting associated with research/homework",
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
t070201 = "Comparison shopping",
t080101 = "Using paid childcare services",
t080201 = "Banking",
t080202 = "Using other financial services",
t080203 = "Waiting associated w/banking/financial services",
t080299 = "Using financial services and banking, n.e.c.*",
t080301 = "Using legal services",
t080401 = "Using health and care services outside the home",
t080402 = "Using in-home health and care services",
t080403 = "Waiting associated with medical services",
t080499 = "Using medical services, n.e.c.*",
t080501 = "Using personal care services",
t080502 = "Waiting associated w/personal care services",
t080599 = "Using personal care services, n.e.c.*",
t080601 = "Activities rel. to purchasing/selling real estate",
t080602 = "Waiting associated w/purchasing/selling real estate",
t080701 = "Using veterinary services",
t080702 = "Waiting associated with veterinary services",
t080801 = "Security procedures rel. to professional/personal svcs.",
t089999 = "Professional and personal services, n.e.c.*",
t090101 = "Using interior cleaning services",
t090103 = "Using clothing repair and cleaning services",
t090104 = "Waiting associated with using household services",
t090199 = "Using household services, n.e.c.*",
t090201 = "Using home maint/repair/décor/construction svcs",
t090202 = "Waiting associated w/ home main/repair/décor/constr",
t090301 = "Using pet services",
t090401 = "Using lawn and garden services",
t090402 = "Waiting associated with using lawn and garden services",
t090501 = "Using vehicle maintenance or repair services",
t090502 = "Waiting associated with vehicle main. or repair svcs",
t090599 = "Using vehicle maint. and repair svcs, n.e.c.*",
t099999 = "Using household services, n.e.c.*",
t100102 = "Using social services",
t100103 = "Obtaining licenses and paying fines, fees, taxes",
t100199 = "Using government services, n.e.c.*",
t100201 = "Civic obligations and participation",
t100299 = "Civic obligations and participation, n.e.c.*",
t100304 = "Waiting associated with using government services",
t100305 = "Waiting associated with civic obligations and participation",
t110101 = "Eating and drinking",
t110201 = "Waiting associated w/eating and drinking",
t120101 = "Socializing and communicating with others",
t120199 = "Socializing and communicating, n.e.c.*",
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
t130210 = "Watching equestrian sports",
t130211 = "Watching fencing",
t130212 = "Watching fishing",
t130213 = "Watching football",
t130216 = "Watching hockey",
t130220 = "Watching rollerblading",
t130224 = "Watching soccer",
t130227 = "Watching volleyball",
t130229 = "Watching water sports",
t130299 = "Attending sporting events, n.e.c.*",
t130301 = "Waiting related to playing sports or exercising",
t130302 = "Waiting related to attending sporting events",
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
t150302 = "Indoor and outdoor maintenance, repair, and clean-up",
t150399 = "Indoor and outdoor maintenance, building and clean-up activities, n.e.c.*",
t150401 = "Performing",
t150402 = "Serving at volunteer events and cultural activities",
t150499 = "Participating in performance and cultural activities, n.e.c.*",
t150501 = "Attending meetings, conferences, and training",
t150599 = "Attending meetings, conferences, and training, n.e.c.*",
t150601 = "Public health activities",
t150602 = "Public safety activities",
t150701 = "Waiting associated with volunteer activities",
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
t180301 = "Travel related to caring for and helping hh children",
t180302 = "Travel related to hh children's education",
t180303 = "Travel related to hh children's health",
t180304 = "Travel related to caring for hh adults",
t180305 = "Travel related to helping hh adults",
t180399 = "Travel rel. to caring for and helping hh members, n.e.c.*",
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
t180601 = "Travel related to taking class",
t180602 = "Travel related to extracurricular activities (ex. Sports)",
t180603 = "Travel related to research/homework",
t180604 = "Travel related to registration/administrative activities",
t180699 = "Travel related to education, n.e.c.*",
t180701 = "Travel related to grocery shopping",
t180702 = "Travel related to purchasing gas",
t180703 = "Travel related to purchasing food (not groceries)",
t180704 = "Travel related to shopping, ex groceries, food, and gas",
t180801 = "Travel related to using childcare services",
t180802 = "Travel related to using financial services and banking",
t180803 = "Travel related to using legal services",
t180804 = "Travel related to using medical services",
t180805 = "Travel related to using personal care services",
t180806 = "Travel related to using real estate services",
t180807 = "Travel related to using veterinary services",
t180899 = "Travel rel. to using prof. and personal care services, n.e.c.*",
t180901 = "Travel related to using household services",
t180902 = "Travel related to using home main./repair/décor./construction svcs",
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
t189999 = "Traveling, n.e.c.*",
t500101 = "Insufficient detail in verbatim",
t500103 = "Missing travel or destination",
t500105 = "Respondent refused to provide information/'none of your business'",
t500106 = "Gap/can't remember",
t500107 = "Unable to code activity at 1st tier"))
```

Let’s categorize some of the activities for the 2020 data.

``` r
activity_summary_2020_categorized = activity_summary_2020_long %>%
  mutate(category = case_when(
    activity_codes %in% c("Sleeping",
                               "Sleeplessness",
                               "Washing, dressing and grooming oneself",
                               "Grooming, n.e.c.*",
                               "Health-related self care",
                               "Self care, n.e.c.*",
                               "Personal/Private activities",
                               "Personal activities, n.e.c.*",
                               "Personal care, n.e.c.*") ~ "personal_care", 
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
                               "Caring for and helping hh members, n.e.c.*",
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
                               "Home schooling of nonhh children",
                               "Providing medical care to nonhh children",
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
                               "Caring for and helping nonhh members, n.e.c.*") ~ "caring_duties", 
    activity_codes %in% c("Work, main job",
                               "Work, other job(s)",
                               "Security procedures related to work",
                               "Waiting associated with working",
                               "Working, n.e.c.*",
                               "Socializing, relaxing, and leisure as part of job",
                               "Sports and exercise as part of job",
                               "Work-related activities, n.e.c.*",
                               "Income-generating hobbies, crafts, and food",
                               "Income-generating performances",
                               "Income-generating services",
                               "Income-generating rental property activities",
                               "Waiting associated with other income-generating activities",
                               "Other income-generating activities, n.e.c.*",
                               "Job search activities",
                               "Job interviewing",
                               "Waiting associated with job search or interview",
                               "Job search and Interviewing, n.e.c.*") ~ "work", 
    activity_codes %in% c("Taking class for degree, certification, or licensure",
                               "Taking class for personal interest",
                               "Waiting associated with taking classes",
                               "Taking class, n.e.c.*",
                               "Extracurricular club activities",
                               "Extracurricular music and performance activities",
                               "Extracurricular student government activities",
                               "Education-related extracurricular activities, n.e.c.*",
                               "Research/homework for class for degree, certification, or licensure",
                               "Research/homework for class for pers. interest",
                               "Waiting associated with research/homework",
                               "Research/homework n.e.c.*",
                               "Administrative activities: class for degree, certification, or licensure",
                               "Administrative activities: class for personal interest",
                               "Administrative for education, n.e.c.*",
                               "Education, n.e.c.*") ~ "education", 
    activity_codes %in% c("Grocery shopping",
                               "Purchasing gas",
                               "Purchasing food (not groceries)",
                               "Shopping, except groceries, food and gas",
                               "Waiting associated with shopping",
                               "Comparison shopping") ~ "shopping", 
    activity_codes %in% c( "Using paid childcare services",
                               "Banking",
                               "Using other financial services",
                               "Waiting associated w/banking/financial services",
                               "Using financial services and banking, n.e.c.*",
                               "Using legal services",
                               "Using health and care services outside the home",
                               "Using in-home health and care services",
                               "Waiting associated with medical services",
                               "Using medical services, n.e.c.*",
                               "Using personal care services",
                               "Waiting associated w/personal care services",
                               "Using personal care services, n.e.c.*",
                               "Activities rel. to purchasing/selling real estate",
                               "Waiting associated w/purchasing/selling real estate",
                               "Using veterinary services",
                               "Waiting associated with veterinary services",
                               "Security procedures rel. to professional/personal svcs.",
                               "Professional and personal services, n.e.c.*",
                               "Using interior cleaning services",
                               "Using clothing repair and cleaning services",
                               "Waiting associated with using household services",
                               "Using household services, n.e.c.*",
                               "Using home maint/repair/décor/construction svcs",
                               "Waiting associated w/ home main/repair/décor/constr",
                               "Using pet services",
                               "Using lawn and garden services",
                               "Waiting associated with using lawn and garden services",
                               "Using vehicle maintenance or repair services",
                               "Waiting associated with vehicle main. or repair svcs",
                               "Using vehicle maint. and repair svcs, n.e.c.*",
                               "Using household services, n.e.c.*",
                               "Using social services",
                               "Obtaining licenses and paying fines, fees, taxes") ~ "professional_services", 
    activity_codes %in% c( "Using government services, n.e.c.*",
                               "Civic obligations and participation",
                               "Civic obligations and participation, n.e.c.*",
                               "Waiting associated with using government services",
                               "Waiting associated with civic obligations and participation") ~ "gov_civic_obligations", 
    activity_codes %in% c("Eating and drinking",
                               "Waiting associated w/eating and drinking") ~ "eating_drinking", 
    activity_codes %in% c( "Socializing and communicating with others",
                               "Socializing and communicating, n.e.c.*",
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
                               "Arts and entertainment, n.e.c.*",
                               "Watching baseball", #moved watching sports to leisure section
                               "Watching basketball",
                               "Watching equestrian sports",
                               "Watching fencing",
                               "Watching fishing",
                               "Watching football",
                               "Watching hockey",
                               "Watching rollerblading",
                               "Watching soccer",
                               "Watching volleyball",
                               "Watching water sports",
                               "Attending sporting events, n.e.c.*",
                               "Waiting assoc. w/socializing and communicating",
                               "Waiting assoc. w/attending/hosting social events",
                               "Waiting associated with relaxing/leisure",
                               "Waiting associated with arts and entertainment",
                               "Socializing, relaxing, and leisure, n.e.c.*") ~ "leisure",
    activity_codes %in% c( "Doing aerobics",
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
                               "Sports, exercise, and recreation, n.e.c.*"
                               ) ~ "exercise", 
    activity_codes %in% c("Attending religious services",
                               "Participation in religious practices",
                               "Waiting associated w/religious and spiritual activities",
                               "Religious education activities",
                               "Religious and spiritual activities, n.e.c.*") ~ "religious_spiritual", 
    activity_codes %in% c("Fundraising",
                               "Administrative and support activities, n.e.c.*",
                               "Food preparation, presentation, clean-up",
                               "Collecting and delivering clothing and other goods",
                               "Providing care",
                               "Teaching, leading, counseling, mentoring",
                               "Social service and care activities, n.e.c.*",
                               "Indoor and outdoor maintenance, repair, and clean-up",
                               "Indoor and outdoor maintenance, building and clean-up activities, n.e.c.*",
                               "Performing",
                               "Serving at volunteer events and cultural activities",
                               "Participating in performance and cultural activities, n.e.c.*",
                               "Attending meetings, conferences, and training",
                               "Attending meetings, conferences, and training, n.e.c.*",
                               "Public health activities",
                               "Public safety activities",
                               "Waiting associated with volunteer activities",
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
                               "Travel related to caring for and helping hh children",
                               "Travel related to hh children's education",
                               "Travel related to hh children's health",
                               "Travel related to caring for hh adults",
                               "Travel related to helping hh adults",
                               "Travel rel. to caring for and helping hh members, n.e.c.*",
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
                               "Travel related to taking class",
                               "Travel related to extracurricular activities (ex. Sports)",
                               "Travel related to research/homework",
                               "Travel related to registration/administrative activities",
                               "Travel related to education, n.e.c.*",
                               "Travel related to grocery shopping",
                               "Travel related to purchasing gas",
                               "Travel related to purchasing food (not groceries)",
                               "Travel related to shopping, ex groceries, food, and gas",
                               "Travel related to using childcare services",
                               "Travel related to using financial services and banking",
                               "Travel related to using legal services",
                               "Travel related to using medical services",
                               "Travel related to using personal care services",
                               "Travel related to using real estate services",
                               "Travel related to using veterinary services",
                               "Travel rel. to using prof. and personal care services, n.e.c.*",
                               "Travel related to using household services",
                               "Travel related to using home main./repair/décor./construction svcs",
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
                               "Traveling, n.e.c.*") ~ "traveling"
  ))
```

Let’s repeat the process for the 2019 activity summary file.

``` r
activity_summary_2019_long = 
  pivot_longer(data = activity_summary_2019, 
               cols = t010101:t500107, 
               names_to = "activity_codes", 
               values_to = "total_minutes") %>%
  select(TUCASEID, PTDTRACE, TEAGE, TELFS, TESEX, activity_codes, total_minutes) %>% #selected mostly demographic variables 
  mutate(year = 2019) %>% 
  rename(race = PTDTRACE, 
         labor_force_status = TELFS, 
         age = TEAGE, 
         sex = TESEX) %>%
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
                                 t090201 = "Using home maint/repair/d�cor/construction svcs",
                                 t090202 = "Waiting associated w/ home main/repair/d�cor/constr",
                                 t090299 = "Using home maint/repair/d�cor/constr services, n.e.c.*",
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
                                 t180902 = "Travel related to using home main./repair/d�cor./construction svcs",
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
                                 t500107 = "Unable to code activity at 1st tier")
                                                      )
```

Let’s categorize 2019 activities.

``` r
activity_summary_2019_categorized = activity_summary_2019_long %>%
  mutate(category = case_when(
    activity_codes %in% c("Sleeping", 
                          "Sleeplessness",
                          "Using pet services",
                          "Washing, dressing and grooming oneself",
                          "Grooming, n.e.c.*",
                          "Health-related self care",
                          "Self care, n.e.c.*",
                          "Personal/Private activities",
                          "Personal activities, n.e.c.*",
                          "Personal emergencies",
                          "Personal care, n.e.c.*") ~ "personal_care",
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
"Caring for and helping nonhh members, n.e.c.*") ~ "caring_duties", 
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
"Work and work-related activities, n.e.c.*") ~ "work", 
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
"Education, n.e.c.*") ~ "education", 
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
"Using home maint/repair/d�cor/construction svcs",
"Waiting associated w/ home main/repair/d�cor/constr",
"Using home maint/repair/d�cor/constr services, n.e.c.*",
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
"Obtaining licenses and paying fines, fees, taxes") ~ "professional_services", 
activity_codes %in% c("Using government services, n.e.c.*",
"Civic obligations and participation",
"Civic obligations and participation, n.e.c.*",
"Waiting associated with using government services") ~ "gov_civic_obligations", 
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
"Socializing, relaxing, and leisure, n.e.c.*") ~ "leisure", 
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
"Travel related to using home main./repair/d�cor./construction svcs",
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
"Traveling, n.e.c.*") ~ "traveling"
))
```

Bind the 2019 and 2020 summary files.

``` r
activity_summary_combined = rbind(activity_summary_2019_categorized, activity_summary_2020_categorized)
```

Let’s read in and tidy the cps files that have the geographic
information. First we’ll select the appropriate columns from cps and
then rename the columns to be more useful. Then we’ll recode the column
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

Merge the cps and summary datafiles.

``` r
cps_summary_merged = left_join(activity_summary_combined, cps_combined, by = c("TUCASEID", "age", "year", "sex"))
```

Regression analysis with age and time spent on leisure activities

``` r
cps_summary_leisure = cps_summary_merged %>% 
  filter(activity_codes == "Computer use for leisure (exc. Games)")
```

``` r
fit = lm(total_minutes ~ age, data = cps_summary_leisure)

summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = total_minutes ~ age, data = cps_summary_leisure)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -14.42  -11.91  -10.02   -8.44 1336.94 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept) 15.99889    1.00977  15.844 < 0.0000000000000002 ***
    ## age         -0.10494    0.01863  -5.634         0.0000000179 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 46.05 on 18294 degrees of freedom
    ## Multiple R-squared:  0.001732,   Adjusted R-squared:  0.001678 
    ## F-statistic: 31.74 on 1 and 18294 DF,  p-value: 0.00000001786

``` r
summary(fit)$coef
```

    ##               Estimate Std. Error   t value     Pr(>|t|)
    ## (Intercept) 15.9988868 1.00977353 15.844035 3.653569e-56
    ## age         -0.1049371 0.01862547 -5.634061 1.786051e-08

``` r
coef(fit)
```

    ## (Intercept)         age 
    ##  15.9988868  -0.1049371

``` r
fitted.values(fit)
```

    ##         1         2         3         4         5         6         7         8 
    ##  7.079237 13.375460 13.900146  9.597726 12.431027 10.437223 13.270523 11.276719 
    ##         9        10        11        12        13        14        15        16 
    ##  7.079237  8.233544  7.079237 12.221153  8.968104 11.801404 10.437223  7.918733 
    ##        17        18        19        20        21        22        23        24 
    ## 11.801404 10.542160 10.542160 12.116216  8.233544 14.110020  8.023670  8.968104 
    ##        25        26        27        28        29        30        31        32 
    ##  8.863167 14.110020  8.863167  7.603922  9.177978  9.282915  8.233544 14.005083 
    ##        33        34        35        36        37        38        39        40 
    ##  8.863167 11.066845  9.807600 12.535964 11.591530  9.912537 11.801404 10.017474 
    ##        41        42        43        44        45        46        47        48 
    ## 11.171782  8.443419 13.060649  9.492789  7.603922 11.381656  9.073041  9.597726 
    ##        49        50        51        52        53        54        55        56 
    ## 11.696467 10.332286  8.443419  7.603922 13.270523  8.338482 12.955712 11.591530 
    ##        57        58        59        60        61        62        63        64 
    ## 11.066845 11.906342 12.431027  8.338482  9.177978 12.116216 10.752034 11.381656 
    ##        65        66        67        68        69        70        71        72 
    ##  7.079237 12.221153  7.918733  7.813796 10.017474 10.856971  8.338482  7.603922 
    ##        73        74        75        76        77        78        79        80 
    ## 12.640901 12.955712 13.060649  7.813796  9.073041  9.597726 13.270523 11.276719 
    ##        81        82        83        84        85        86        87        88 
    ##  8.128607  9.702663 11.906342  9.807600  9.177978 11.696467 12.640901 10.227349 
    ##        89        90        91        92        93        94        95        96 
    ##  8.863167 12.116216 11.696467 12.850775 10.961908  9.387852 11.906342 11.906342 
    ##        97        98        99       100       101       102       103       104 
    ## 12.221153 12.640901 10.017474  8.968104 14.319894  9.702663 10.332286 10.227349 
    ##       105       106       107       108       109       110       111       112 
    ## 11.381656  8.968104 11.171782 13.060649 10.542160 12.116216 13.375460 14.214957 
    ##       113       114       115       116       117       118       119       120 
    ## 11.486593 11.171782 13.375460 11.066845 11.381656 10.647097 13.480397 11.381656 
    ##       121       122       123       124       125       126       127       128 
    ##  8.863167 13.900146 11.381656 13.270523 11.801404  8.233544  9.597726  7.603922 
    ##       129       130       131       132       133       134       135       136 
    ## 10.332286 11.906342  9.912537 12.011279  8.128607 12.011279  9.073041 13.060649 
    ##       137       138       139       140       141       142       143       144 
    ## 11.696467 11.696467  8.758230  8.863167 12.640901 10.542160 13.165586 14.214957 
    ##       145       146       147       148       149       150       151       152 
    ## 13.480397 10.122412 10.752034 14.319894  8.653293 11.486593 11.801404 10.542160 
    ##       153       154       155       156       157       158       159       160 
    ## 12.326090 12.850775 14.214957 14.110020 11.906342 12.326090 12.221153 14.424831 
    ##       161       162       163       164       165       166       167       168 
    ## 12.955712 13.375460 12.326090 12.326090 10.017474  8.548356 13.270523 10.227349 
    ##       169       170       171       172       173       174       175       176 
    ## 11.381656 12.640901 13.375460  8.968104 14.319894 11.486593  7.708859 10.647097 
    ##       177       178       179       180       181       182       183       184 
    ## 11.696467  9.807600 12.221153 13.270523 13.795209 13.795209 14.319894 11.591530 
    ##       185       186       187       188       189       190       191       192 
    ##  7.603922 12.011279 12.745838 10.856971 11.801404  8.023670 11.066845 10.437223 
    ##       193       194       195       196       197       198       199       200 
    ## 11.486593  8.233544 11.486593 13.585334 11.066845 12.326090 13.480397 11.696467 
    ##       201       202       203       204       205       206       207       208 
    ## 11.906342 13.375460 11.696467 10.437223 11.591530 11.276719 11.906342  7.079237 
    ##       209       210       211       212       213       214       215       216 
    ## 11.696467 13.795209  9.807600  9.073041 11.591530  9.282915 12.116216  8.443419 
    ##       217       218       219       220       221       222       223       224 
    ##  8.548356 12.535964 14.005083 11.696467 12.221153 10.752034  9.597726  7.918733 
    ##       225       226       227       228       229       230       231       232 
    ##  9.492789  8.338482  7.603922 13.165586 11.381656  8.548356 11.066845  9.492789 
    ##       233       234       235       236       237       238       239       240 
    ## 11.381656 10.332286 11.276719 14.005083 12.535964 13.270523 12.326090 14.319894 
    ##       241       242       243       244       245       246       247       248 
    ## 14.110020 12.955712  9.912537  9.177978 10.437223  7.603922 13.690271 11.696467 
    ##       249       250       251       252       253       254       255       256 
    ##  7.603922  8.443419 12.535964 14.424831 13.900146 10.017474 10.227349 12.221153 
    ##       257       258       259       260       261       262       263       264 
    ## 12.116216  8.758230 12.431027 10.227349  9.073041  8.548356  8.023670 11.171782 
    ##       265       266       267       268       269       270       271       272 
    ##  8.653293 10.961908 10.752034 13.060649  7.079237 12.011279 11.171782 13.060649 
    ##       273       274       275       276       277       278       279       280 
    ## 12.221153 12.011279  7.603922 13.900146 12.850775  8.968104 13.060649 10.017474 
    ##       281       282       283       284       285       286       287       288 
    ## 11.906342 10.856971  8.548356 12.326090 10.437223 12.640901 13.375460 13.585334 
    ##       289       290       291       292       293       294       295       296 
    ##  9.702663 11.066845 13.270523 11.381656  9.807600  9.387852  8.548356  9.282915 
    ##       297       298       299       300       301       302       303       304 
    ## 12.850775 12.011279 12.326090  9.282915  9.177978  8.863167  9.597726  8.968104 
    ##       305       306       307       308       309       310       311       312 
    ## 13.585334  8.863167  8.128607  9.807600  8.863167 12.955712 11.171782  8.233544 
    ##       313       314       315       316       317       318       319       320 
    ##  7.079237 12.955712 10.122412  9.073041  9.492789 10.017474  8.968104 11.906342 
    ##       321       322       323       324       325       326       327       328 
    ## 10.227349 14.424831 11.381656  8.128607 12.431027 11.906342  7.813796 12.011279 
    ##       329       330       331       332       333       334       335       336 
    ##  8.233544 10.856971  8.758230 10.961908 11.171782 12.640901 12.745838  9.912537 
    ##       337       338       339       340       341       342       343       344 
    ## 11.801404  8.338482 12.431027 11.591530 11.591530 13.480397 11.066845 12.011279 
    ##       345       346       347       348       349       350       351       352 
    ##  9.387852 10.856971 10.437223 12.535964  9.807600 11.276719 11.171782 11.801404 
    ##       353       354       355       356       357       358       359       360 
    ## 13.375460 12.955712 13.690271 12.011279 12.221153  9.282915  8.443419  9.912537 
    ##       361       362       363       364       365       366       367       368 
    ## 13.480397 10.437223  7.603922 10.017474  8.758230 12.745838 11.591530 11.696467 
    ##       369       370       371       372       373       374       375       376 
    ## 12.326090 11.381656  7.603922 10.227349 13.060649  8.758230 11.591530  8.863167 
    ##       377       378       379       380       381       382       383       384 
    ##  9.807600  9.282915 11.171782  8.338482  9.177978 12.326090 13.165586  7.079237 
    ##       385       386       387       388       389       390       391       392 
    ##  8.653293 13.900146  9.597726  9.912537  9.912537 10.332286 12.116216  8.443419 
    ##       393       394       395       396       397       398       399       400 
    ## 10.961908 11.276719 10.017474 10.227349  7.079237  9.912537 12.116216 10.227349 
    ##       401       402       403       404       405       406       407       408 
    ##  7.603922 14.110020  9.177978  7.918733  7.918733  7.079237  8.653293 10.752034 
    ##       409       410       411       412       413       414       415       416 
    ##  8.338482 10.017474  8.548356  9.073041  8.023670  9.807600 10.332286  8.968104 
    ##       417       418       419       420       421       422       423       424 
    ## 12.116216  8.128607 11.906342  7.603922 12.745838  9.492789 11.696467 11.066845 
    ##       425       426       427       428       429       430       431       432 
    ## 11.486593 12.745838 11.171782 10.752034  8.968104  9.912537 12.116216  9.177978 
    ##       433       434       435       436       437       438       439       440 
    ## 11.066845 13.585334  9.282915  9.912537 10.647097 13.690271  9.073041 12.116216 
    ##       441       442       443       444       445       446       447       448 
    ## 12.640901 14.424831 13.690271  9.387852 11.486593 13.165586 10.332286 11.171782 
    ##       449       450       451       452       453       454       455       456 
    ##  8.128607 11.696467  9.387852 10.961908 12.011279  9.702663  8.653293 13.795209 
    ##       457       458       459       460       461       462       463       464 
    ## 11.066845 11.381656 14.424831 14.424831 14.424831 11.381656  9.387852 12.326090 
    ##       465       466       467       468       469       470       471       472 
    ##  8.863167  8.338482 10.227349 12.850775 10.647097  8.023670 12.011279  9.912537 
    ##       473       474       475       476       477       478       479       480 
    ## 10.122412 14.424831 11.381656 11.591530  8.653293  8.758230  8.548356  8.758230 
    ##       481       482       483       484       485       486       487       488 
    ## 10.961908  7.708859  8.338482 11.696467  8.233544 11.801404 12.850775 10.752034 
    ##       489       490       491       492       493       494       495       496 
    ## 12.431027 11.381656  9.807600 11.276719  8.653293  7.918733  9.073041 11.171782 
    ##       497       498       499       500       501       502       503       504 
    ##  8.338482 10.856971  8.968104 14.424831 12.535964  9.282915  7.079237  7.603922 
    ##       505       506       507       508       509       510       511       512 
    ##  9.282915 11.066845  8.653293 11.276719  8.338482 12.535964 10.647097 11.696467 
    ##       513       514       515       516       517       518       519       520 
    ## 12.221153  9.912537 12.116216 12.326090 11.381656 14.319894 12.640901 10.752034 
    ##       521       522       523       524       525       526       527       528 
    ##  7.813796 12.116216  9.282915 10.542160 11.801404 11.801404  8.023670 12.326090 
    ##       529       530       531       532       533       534       535       536 
    ## 10.856971 13.060649  9.807600  8.548356 10.332286 14.214957 12.640901  8.968104 
    ##       537       538       539       540       541       542       543       544 
    ##  8.128607 11.801404 10.332286  9.282915  7.603922  8.443419 10.542160 10.961908 
    ##       545       546       547       548       549       550       551       552 
    ##  8.338482  7.603922 10.227349  7.603922 11.066845 11.696467 12.535964 12.850775 
    ##       553       554       555       556       557       558       559       560 
    ## 11.066845  7.813796 12.221153  9.492789 12.011279 14.319894 10.647097 10.017474 
    ##       561       562       563       564       565       566       567       568 
    ## 13.690271  8.968104 11.276719  9.282915 11.171782 13.165586 10.017474  9.073041 
    ##       569       570       571       572       573       574       575       576 
    ## 11.171782 12.116216 10.752034  9.807600 11.696467  8.653293 13.795209 10.017474 
    ##       577       578       579       580       581       582       583       584 
    ## 10.332286 10.017474 10.332286  8.968104 10.437223 11.066845 11.591530  8.653293 
    ##       585       586       587       588       589       590       591       592 
    ## 11.906342 14.005083  8.233544  8.338482 11.066845 10.647097 13.900146 13.900146 
    ##       593       594       595       596       597       598       599       600 
    ##  8.653293  9.073041 13.795209  9.702663 10.752034 11.066845 11.066845  9.387852 
    ##       601       602       603       604       605       606       607       608 
    ## 14.214957 10.542160  7.079237 10.227349 10.227349 11.801404 13.900146 11.591530 
    ##       609       610       611       612       613       614       615       616 
    ##  9.702663  9.702663 12.640901  9.282915  8.548356  8.653293 11.486593  8.023670 
    ##       617       618       619       620       621       622       623       624 
    ##  8.233544  9.073041 11.906342 14.110020 12.955712  7.603922 10.332286 11.696467 
    ##       625       626       627       628       629       630       631       632 
    ## 10.227349 11.171782 10.542160  9.177978 11.591530  8.968104 11.486593  8.443419 
    ##       633       634       635       636       637       638       639       640 
    ## 14.319894 13.270523  9.073041  8.968104  8.443419 12.850775 14.319894 10.647097 
    ##       641       642       643       644       645       646       647       648 
    ## 13.165586 13.165586 12.640901 11.486593 10.856971 10.856971 10.437223 11.906342 
    ##       649       650       651       652       653       654       655       656 
    ## 11.066845 12.116216  8.023670  9.282915  8.233544 14.424831  8.548356  9.492789 
    ##       657       658       659       660       661       662       663       664 
    ##  9.702663 12.640901 12.535964  7.708859 12.431027 11.171782 12.011279  7.079237 
    ##       665       666       667       668       669       670       671       672 
    ##  9.702663 10.122412  9.492789 14.005083  9.073041 12.116216 12.640901  9.912537 
    ##       673       674       675       676       677       678       679       680 
    ##  9.073041 12.326090 12.535964  9.702663 11.381656 12.955712 11.801404 13.480397 
    ##       681       682       683       684       685       686       687       688 
    ## 11.171782 11.591530 10.542160 12.745838  8.968104 10.647097 13.060649  9.597726 
    ##       689       690       691       692       693       694       695       696 
    ## 10.647097  7.603922 12.116216 10.122412  9.387852 10.961908  9.492789 10.752034 
    ##       697       698       699       700       701       702       703       704 
    ##  7.079237 12.116216  7.813796  8.968104 11.696467  9.492789  9.073041 11.801404 
    ##       705       706       707       708       709       710       711       712 
    ##  9.177978  9.177978 12.431027 12.745838  9.073041 14.110020  7.918733  9.912537 
    ##       713       714       715       716       717       718       719       720 
    ##  8.443419 10.437223 12.221153 10.122412 11.591530 11.066845  9.807600 13.375460 
    ##       721       722       723       724       725       726       727       728 
    ## 12.116216 11.801404 14.214957  8.863167  8.443419 11.801404 10.856971 11.906342 
    ##       729       730       731       732       733       734       735       736 
    ##  8.758230 10.647097 10.437223 11.171782  8.653293 10.856971 11.696467 11.696467 
    ##       737       738       739       740       741       742       743       744 
    ## 12.535964  8.968104 10.647097  9.702663 12.431027  8.023670 10.122412 10.542160 
    ##       745       746       747       748       749       750       751       752 
    ##  9.387852 10.227349 14.424831 11.381656  7.079237 10.437223 10.122412  9.073041 
    ##       753       754       755       756       757       758       759       760 
    ##  7.708859 11.906342 10.332286 10.227349  8.548356  9.387852  7.079237 12.745838 
    ##       761       762       763       764       765       766       767       768 
    ##  9.912537  7.708859  9.597726 11.906342  9.597726 10.961908  7.603922  9.387852 
    ##       769       770       771       772       773       774       775       776 
    ##  9.177978  9.492789  8.653293 10.437223  8.863167 12.850775 11.801404 14.005083 
    ##       777       778       779       780       781       782       783       784 
    ##  7.079237  9.177978  9.492789 14.424831  9.702663  9.492789 11.171782  8.443419 
    ##       785       786       787       788       789       790       791       792 
    ##  9.912537  9.807600 13.480397  7.918733 12.640901 12.535964  7.918733  9.912537 
    ##       793       794       795       796       797       798       799       800 
    ## 12.116216 12.116216  8.338482  8.758230 12.535964 12.640901  8.443419 13.375460 
    ##       801       802       803       804       805       806       807       808 
    ##  7.708859  8.443419 10.227349  8.968104 11.591530 11.696467 10.332286 12.955712 
    ##       809       810       811       812       813       814       815       816 
    ## 12.431027  7.603922 12.431027  9.597726  9.177978 12.011279  7.603922 10.752034 
    ##       817       818       819       820       821       822       823       824 
    ## 13.060649 10.961908  7.079237  9.597726 11.591530  7.603922 10.647097  9.912537 
    ##       825       826       827       828       829       830       831       832 
    ##  9.702663  8.128607  9.282915 10.542160  9.387852  7.603922 12.431027  7.079237 
    ##       833       834       835       836       837       838       839       840 
    ##  9.387852 11.276719 10.647097 12.116216  8.338482 10.961908 10.856971 11.906342 
    ##       841       842       843       844       845       846       847       848 
    ##  9.597726 12.116216 10.961908 14.319894 11.801404 11.696467  9.807600 12.955712 
    ##       849       850       851       852       853       854       855       856 
    ##  9.073041  8.863167 11.696467 12.326090 11.486593  7.079237  7.079237 13.795209 
    ##       857       858       859       860       861       862       863       864 
    ##  9.912537 10.332286 10.437223 13.480397  9.912537 10.647097  9.177978  7.813796 
    ##       865       866       867       868       869       870       871       872 
    ## 12.745838  9.282915 13.795209  9.177978  9.702663 14.319894 10.437223  9.177978 
    ##       873       874       875       876       877       878       879       880 
    ## 11.381656 14.005083 12.011279 10.122412  9.177978  9.912537 12.850775  9.807600 
    ##       881       882       883       884       885       886       887       888 
    ##  8.443419 10.227349  9.912537 10.017474  8.338482  8.233544  9.912537 12.011279 
    ##       889       890       891       892       893       894       895       896 
    ##  7.603922 10.752034 11.171782  8.758230 12.745838 11.171782 14.214957  9.807600 
    ##       897       898       899       900       901       902       903       904 
    ## 10.647097 10.752034  9.492789  9.702663 12.116216 12.955712 10.542160 13.375460 
    ##       905       906       907       908       909       910       911       912 
    ##  8.128607  9.492789  8.863167 11.066845  9.597726  8.653293 11.591530  7.603922 
    ##       913       914       915       916       917       918       919       920 
    ##  8.128607 13.060649  8.758230  7.079237  7.603922  7.079237  7.813796 11.066845 
    ##       921       922       923       924       925       926       927       928 
    ## 12.745838 12.850775 12.116216 14.214957 10.647097 11.801404 12.326090  7.813796 
    ##       929       930       931       932       933       934       935       936 
    ##  8.968104 11.696467  8.128607  9.282915  8.863167  8.758230 11.591530 11.171782 
    ##       937       938       939       940       941       942       943       944 
    ## 11.801404 10.227349  9.492789 12.431027 11.696467  9.492789 11.906342 11.066845 
    ##       945       946       947       948       949       950       951       952 
    ##  9.387852  9.702663 13.480397  7.603922  7.079237  9.177978 12.221153 12.221153 
    ##       953       954       955       956       957       958       959       960 
    ## 12.221153 12.326090  7.603922  7.603922  8.548356 12.431027  8.653293 10.542160 
    ##       961       962       963       964       965       966       967       968 
    ## 13.480397  8.023670  7.708859  7.708859  9.702663 10.017474  9.177978  9.282915 
    ##       969       970       971       972       973       974       975       976 
    ## 14.319894  9.912537  9.597726 12.850775  7.603922 10.752034 10.752034 10.752034 
    ##       977       978       979       980       981       982       983       984 
    ## 10.752034 10.122412 11.801404 14.319894 14.319894  8.443419  8.548356  8.338482 
    ##       985       986       987       988       989       990       991       992 
    ## 11.906342 13.480397 10.437223  9.492789 11.486593  9.282915 12.011279 12.850775 
    ##       993       994       995       996       997       998       999      1000 
    ## 11.696467  8.233544  9.912537 10.647097  7.603922  9.282915 10.856971 11.591530 
    ##      1001      1002      1003      1004      1005      1006      1007      1008 
    ## 12.221153 11.381656 11.801404  8.128607  8.128607  7.079237 12.745838 14.319894 
    ##      1009      1010      1011      1012      1013      1014      1015      1016 
    ## 12.850775 12.640901 10.017474  8.338482 13.795209 12.850775 11.066845  9.912537 
    ##      1017      1018      1019      1020      1021      1022      1023      1024 
    ##  7.079237  9.912537  9.073041 12.221153  7.079237  7.603922  9.177978  9.702663 
    ##      1025      1026      1027      1028      1029      1030      1031      1032 
    ## 10.227349 10.332286  9.597726 13.375460  8.233544  9.282915 11.696467  8.653293 
    ##      1033      1034      1035      1036      1037      1038      1039      1040 
    ## 12.745838 12.640901 12.221153 10.542160  7.079237 11.591530 10.017474  8.968104 
    ##      1041      1042      1043      1044      1045      1046      1047      1048 
    ##  8.863167 11.381656  8.338482  7.708859 12.745838 11.486593 14.424831  9.492789 
    ##      1049      1050      1051      1052      1053      1054      1055      1056 
    ##  9.597726  7.918733 12.116216  9.597726 10.227349 12.850775 10.542160  9.912537 
    ##      1057      1058      1059      1060      1061      1062      1063      1064 
    ##  9.807600 11.696467 12.955712 12.745838 12.431027  8.548356  9.492789 13.165586 
    ##      1065      1066      1067      1068      1069      1070      1071      1072 
    ## 13.060649 11.066845 13.165586 12.745838  9.282915 12.955712 13.690271 10.227349 
    ##      1073      1074      1075      1076      1077      1078      1079      1080 
    ##  9.807600  8.653293  8.233544 10.961908 10.017474 11.591530 14.319894  8.443419 
    ##      1081      1082      1083      1084      1085      1086      1087      1088 
    ##  9.177978 12.221153 10.647097 14.214957 12.535964 12.431027  8.968104  8.548356 
    ##      1089      1090      1091      1092      1093      1094      1095      1096 
    ## 11.696467  9.073041 12.011279 11.486593 12.535964 11.801404 11.801404 12.955712 
    ##      1097      1098      1099      1100      1101      1102      1103      1104 
    ## 13.585334  8.968104 12.745838 12.745838 10.122412 13.060649  9.282915  9.807600 
    ##      1105      1106      1107      1108      1109      1110      1111      1112 
    ##  8.758230 10.017474 11.591530 11.171782 12.116216 12.116216  7.603922 12.745838 
    ##      1113      1114      1115      1116      1117      1118      1119      1120 
    ##  9.807600  7.918733 10.542160 11.906342 12.431027 11.381656 10.227349 11.486593 
    ##      1121      1122      1123      1124      1125      1126      1127      1128 
    ## 12.535964  8.128607  9.282915 10.437223  8.758230 10.961908  9.912537 11.276719 
    ##      1129      1130      1131      1132      1133      1134      1135      1136 
    ##  8.023670 10.961908 11.591530 10.437223 10.017474 11.801404 12.535964  8.023670 
    ##      1137      1138      1139      1140      1141      1142      1143      1144 
    ##  7.079237  9.073041  8.128607  7.079237  8.863167  8.548356 13.795209 13.795209 
    ##      1145      1146      1147      1148      1149      1150      1151      1152 
    ## 13.795209 12.535964 12.640901 11.591530  8.653293 12.221153  8.863167  8.758230 
    ##      1153      1154      1155      1156      1157      1158      1159      1160 
    ##  9.073041 14.110020  7.079237  7.603922  9.073041 13.060649 12.640901 10.122412 
    ##      1161      1162      1163      1164      1165      1166      1167      1168 
    ## 14.319894 10.017474 10.017474 12.535964  8.548356 11.276719 13.165586 13.270523 
    ##      1169      1170      1171      1172      1173      1174      1175      1176 
    ##  9.702663  7.918733 13.480397  8.863167 13.690271 14.319894  7.079237  8.338482 
    ##      1177      1178      1179      1180      1181      1182      1183      1184 
    ##  9.073041 10.647097 12.116216  8.443419 11.276719 11.696467  7.603922 13.165586 
    ##      1185      1186      1187      1188      1189      1190      1191      1192 
    ## 11.486593  8.128607  9.177978  9.912537 12.431027 10.542160  8.863167 10.227349 
    ##      1193      1194      1195      1196      1197      1198      1199      1200 
    ## 10.437223  7.813796  7.079237 10.437223 12.535964 10.961908 12.326090 10.752034 
    ##      1201      1202      1203      1204      1205      1206      1207      1208 
    ## 11.591530  9.073041 14.424831 10.542160 13.270523  9.073041  7.708859 12.221153 
    ##      1209      1210      1211      1212      1213      1214      1215      1216 
    ##  8.338482  9.807600  9.073041 10.961908  8.548356  9.702663 11.591530 11.801404 
    ##      1217      1218      1219      1220      1221      1222      1223      1224 
    ## 12.011279 10.542160  9.912537 11.696467 13.480397  9.912537 10.122412  9.807600 
    ##      1225      1226      1227      1228      1229      1230      1231      1232 
    ## 12.431027 13.690271  9.177978  7.603922  9.702663 10.437223 11.066845  8.128607 
    ##      1233      1234      1235      1236      1237      1238      1239      1240 
    ##  8.338482 12.326090  9.912537 11.276719  7.079237 12.326090  8.758230  8.758230 
    ##      1241      1242      1243      1244      1245      1246      1247      1248 
    ##  7.918733 10.122412 12.850775 13.375460 12.011279 12.116216 13.690271 10.332286 
    ##      1249      1250      1251      1252      1253      1254      1255      1256 
    ##  9.282915 11.276719 12.535964 13.375460 14.424831 10.542160 10.437223  9.387852 
    ##      1257      1258      1259      1260      1261      1262      1263      1264 
    ## 10.856971  9.387852 10.437223 11.906342 11.066845  8.863167  7.079237 12.221153 
    ##      1265      1266      1267      1268      1269      1270      1271      1272 
    ##  8.233544 14.214957  7.603922 11.906342 11.276719 11.486593 11.171782 11.591530 
    ##      1273      1274      1275      1276      1277      1278      1279      1280 
    ## 10.227349 12.850775 11.906342 14.214957 10.332286  7.918733  9.387852 12.116216 
    ##      1281      1282      1283      1284      1285      1286      1287      1288 
    ## 14.424831 12.431027  9.387852  8.023670  8.548356 11.591530 10.332286 12.850775 
    ##      1289      1290      1291      1292      1293      1294      1295      1296 
    ## 13.165586  9.702663 11.801404 11.486593 10.752034  8.233544 10.542160  7.603922 
    ##      1297      1298      1299      1300      1301      1302      1303      1304 
    ##  7.813796 10.961908 13.165586  7.603922  7.918733 11.171782  8.758230 12.850775 
    ##      1305      1306      1307      1308      1309      1310      1311      1312 
    ## 12.535964  8.338482 12.116216  9.387852 13.585334 13.480397 10.227349  8.968104 
    ##      1313      1314      1315      1316      1317      1318      1319      1320 
    ##  9.492789  9.597726  9.807600 11.381656  9.177978 10.542160 12.011279  7.079237 
    ##      1321      1322      1323      1324      1325      1326      1327      1328 
    ##  8.443419 12.326090 13.480397 12.431027  8.128607  9.807600 10.227349 13.690271 
    ##      1329      1330      1331      1332      1333      1334      1335      1336 
    ## 13.585334  7.079237 10.122412  9.282915  7.079237 11.801404 13.480397 11.066845 
    ##      1337      1338      1339      1340      1341      1342      1343      1344 
    ## 12.955712 10.647097 12.850775  8.653293 10.961908 13.585334  7.813796  7.813796 
    ##      1345      1346      1347      1348      1349      1350      1351      1352 
    ##  9.702663  9.912537 11.696467 11.906342 12.431027 12.745838 10.647097 12.326090 
    ##      1353      1354      1355      1356      1357      1358      1359      1360 
    ##  9.807600 10.017474 10.017474 10.017474  7.918733 11.696467  7.603922  7.079237 
    ##      1361      1362      1363      1364      1365      1366      1367      1368 
    ## 12.535964  8.023670  8.968104  8.233544  8.863167  9.387852 13.690271  9.177978 
    ##      1369      1370      1371      1372      1373      1374      1375      1376 
    ##  8.233544  9.073041  8.443419 12.955712  7.918733  8.968104 11.591530  9.912537 
    ##      1377      1378      1379      1380      1381      1382      1383      1384 
    ## 10.227349 11.486593 11.696467 14.214957  7.813796 10.752034 12.221153 10.856971 
    ##      1385      1386      1387      1388      1389      1390      1391      1392 
    ##  8.023670 12.326090 12.221153 12.850775 12.011279  9.912537 12.431027 11.486593 
    ##      1393      1394      1395      1396      1397      1398      1399      1400 
    ## 10.122412 10.332286  9.282915  8.758230 12.116216 14.110020 11.591530 12.011279 
    ##      1401      1402      1403      1404      1405      1406      1407      1408 
    ##  8.968104 10.856971 10.961908 10.647097 12.431027  8.968104  9.387852 10.122412 
    ##      1409      1410      1411      1412      1413      1414      1415      1416 
    ## 10.332286  9.807600 12.745838  7.079237 10.017474  9.807600 11.906342  7.079237 
    ##      1417      1418      1419      1420      1421      1422      1423      1424 
    ## 12.116216 12.011279 10.122412 10.437223  9.702663 12.116216 10.332286  8.758230 
    ##      1425      1426      1427      1428      1429      1430      1431      1432 
    ## 14.110020 12.326090  9.597726  8.758230 13.165586 11.906342  9.807600  8.968104 
    ##      1433      1434      1435      1436      1437      1438      1439      1440 
    ##  8.968104  9.282915 12.745838  7.603922 11.591530  9.492789  9.073041  8.863167 
    ##      1441      1442      1443      1444      1445      1446      1447      1448 
    ##  8.863167  8.968104 10.752034  7.079237  8.968104  8.338482  8.338482  8.128607 
    ##      1449      1450      1451      1452      1453      1454      1455      1456 
    ## 10.647097 13.480397 12.955712  9.912537  8.968104 11.591530  8.548356  8.023670 
    ##      1457      1458      1459      1460      1461      1462      1463      1464 
    ## 11.906342  9.492789 10.856971  8.443419 13.270523 13.375460 11.276719  7.079237 
    ##      1465      1466      1467      1468      1469      1470      1471      1472 
    ## 10.437223 10.961908 10.542160 10.752034 12.011279 12.326090  8.338482  7.603922 
    ##      1473      1474      1475      1476      1477      1478      1479      1480 
    ##  9.912537 12.745838  9.702663 10.332286 14.214957 14.319894  8.758230 13.900146 
    ##      1481      1482      1483      1484      1485      1486      1487      1488 
    ##  9.387852  8.653293 10.437223 10.961908 12.850775  8.968104 12.011279  8.968104 
    ##      1489      1490      1491      1492      1493      1494      1495      1496 
    ## 12.955712 12.221153  9.282915 12.221153 12.221153 13.060649  9.912537 14.110020 
    ##      1497      1498      1499      1500      1501      1502      1503      1504 
    ## 10.752034 14.214957 11.066845 10.752034 11.381656 10.227349  7.918733 12.011279 
    ##      1505      1506      1507      1508      1509      1510      1511      1512 
    ##  9.807600  8.653293 12.640901  9.282915 12.850775  8.863167  9.387852 11.486593 
    ##      1513      1514      1515      1516      1517      1518      1519      1520 
    ##  9.807600 10.227349 11.591530  8.548356  9.282915 10.122412 13.585334 13.585334 
    ##      1521      1522      1523      1524      1525      1526      1527      1528 
    ##  7.603922 11.276719 11.801404 13.375460  9.912537 12.850775 11.171782 11.486593 
    ##      1529      1530      1531      1532      1533      1534      1535      1536 
    ## 10.017474  8.758230  8.653293  8.128607  7.708859 10.856971 12.850775  9.387852 
    ##      1537      1538      1539      1540      1541      1542      1543      1544 
    ## 14.319894 10.961908 13.900146 12.955712  9.282915  9.177978 10.227349 10.332286 
    ##      1545      1546      1547      1548      1549      1550      1551      1552 
    ## 10.227349 10.647097 13.270523 11.381656 10.542160 10.542160 12.221153 12.535964 
    ##      1553      1554      1555      1556      1557      1558      1559      1560 
    ## 10.437223 13.270523  9.073041 13.900146 13.060649 12.745838 11.696467 12.221153 
    ##      1561      1562      1563      1564      1565      1566      1567      1568 
    ## 11.486593 13.165586 10.332286 12.535964 12.011279 12.640901  9.073041  9.073041 
    ##      1569      1570      1571      1572      1573      1574      1575      1576 
    ## 11.276719  7.603922  9.492789  9.282915 14.110020  9.597726 10.122412 12.535964 
    ##      1577      1578      1579      1580      1581      1582      1583      1584 
    ## 11.381656  9.492789 12.116216  9.492789 11.486593  9.702663 10.332286 13.375460 
    ##      1585      1586      1587      1588      1589      1590      1591      1592 
    ## 10.332286 10.332286 10.856971 10.017474  7.603922 10.961908  9.073041  8.653293 
    ##      1593      1594      1595      1596      1597      1598      1599      1600 
    ## 13.900146 12.640901 11.801404  7.708859  8.968104  9.492789  9.912537 11.801404 
    ##      1601      1602      1603      1604      1605      1606      1607      1608 
    ## 10.017474  7.603922 13.795209  9.177978  8.023670 10.647097  8.653293  7.079237 
    ##      1609      1610      1611      1612      1613      1614      1615      1616 
    ## 10.122412  9.073041 12.221153  7.603922  9.492789  8.233544  9.597726 10.437223 
    ##      1617      1618      1619      1620      1621      1622      1623      1624 
    ## 14.424831  7.603922 10.122412  8.443419 12.326090 11.171782  9.912537 11.696467 
    ##      1625      1626      1627      1628      1629      1630      1631      1632 
    ## 12.431027  8.443419 11.171782  8.443419 10.647097 13.270523  7.918733 11.591530 
    ##      1633      1634      1635      1636      1637      1638      1639      1640 
    ##  8.548356 12.431027  8.233544 11.276719 14.110020 11.276719 10.542160 11.381656 
    ##      1641      1642      1643      1644      1645      1646      1647      1648 
    ## 12.640901 11.276719 12.640901 14.424831 10.122412 13.690271 12.745838 10.752034 
    ##      1649      1650      1651      1652      1653      1654      1655      1656 
    ## 11.276719 13.480397 12.221153  9.282915  9.073041  8.548356  7.603922  9.807600 
    ##      1657      1658      1659      1660      1661      1662      1663      1664 
    ## 10.122412 11.066845  9.492789 13.165586 10.856971  9.492789 14.214957  7.603922 
    ##      1665      1666      1667      1668      1669      1670      1671      1672 
    ##  9.702663 10.856971  8.653293 14.424831 10.542160  8.968104  7.079237  7.813796 
    ##      1673      1674      1675      1676      1677      1678      1679      1680 
    ## 13.060649 13.690271 12.221153  7.079237 14.424831 10.437223  9.492789 14.110020 
    ##      1681      1682      1683      1684      1685      1686      1687      1688 
    ## 14.110020 14.319894  9.702663 11.276719 12.745838 11.696467 10.752034 11.171782 
    ##      1689      1690      1691      1692      1693      1694      1695      1696 
    ## 13.690271 13.270523  7.079237 12.955712 12.431027 10.752034 12.850775 10.961908 
    ##      1697      1698      1699      1700      1701      1702      1703      1704 
    ##  8.233544  9.387852 12.850775 11.171782  8.758230  9.073041 11.276719 12.116216 
    ##      1705      1706      1707      1708      1709      1710      1711      1712 
    ## 11.486593 13.795209 12.221153 11.276719 11.906342 12.535964 12.955712  9.702663 
    ##      1713      1714      1715      1716      1717      1718      1719      1720 
    ## 14.424831 12.850775 13.480397 11.591530 10.647097 13.690271 12.745838 11.906342 
    ##      1721      1722      1723      1724      1725      1726      1727      1728 
    ## 11.276719  7.079237 10.542160 10.752034 14.214957 12.535964 13.060649 11.276719 
    ##      1729      1730      1731      1732      1733      1734      1735      1736 
    ## 10.227349 13.060649  8.548356 13.795209  9.492789 11.171782 10.961908 12.431027 
    ##      1737      1738      1739      1740      1741      1742      1743      1744 
    ##  9.597726 12.011279 12.326090  9.387852 12.431027 11.591530 11.381656  8.758230 
    ##      1745      1746      1747      1748      1749      1750      1751      1752 
    ## 10.437223  9.073041 11.591530 12.535964 11.486593 12.535964 11.591530  8.758230 
    ##      1753      1754      1755      1756      1757      1758      1759      1760 
    ##  9.492789  8.758230  9.282915  9.597726 12.850775  9.073041  9.597726 11.381656 
    ##      1761      1762      1763      1764      1765      1766      1767      1768 
    ##  9.597726 10.332286 13.795209 12.116216 11.066845  9.282915  9.492789  9.807600 
    ##      1769      1770      1771      1772      1773      1774      1775      1776 
    ##  8.968104 11.591530 11.066845  7.079237  9.492789  8.338482  8.233544 13.795209 
    ##      1777      1778      1779      1780      1781      1782      1783      1784 
    ## 11.066845 12.640901 10.856971 13.585334 14.214957  7.603922  9.912537 10.542160 
    ##      1785      1786      1787      1788      1789      1790      1791      1792 
    ##  7.603922  9.073041 10.437223 10.961908 11.696467 10.017474 10.332286  9.073041 
    ##      1793      1794      1795      1796      1797      1798      1799      1800 
    ##  9.177978 11.171782  8.443419  9.073041 12.745838 13.690271  8.443419 11.696467 
    ##      1801      1802      1803      1804      1805      1806      1807      1808 
    ## 12.326090 10.856971  7.079237  9.073041 12.640901  8.128607  7.813796  9.597726 
    ##      1809      1810      1811      1812      1813      1814      1815      1816 
    ##  9.387852  8.653293  8.758230  8.443419 10.856971 11.171782  8.443419 12.326090 
    ##      1817      1818      1819      1820      1821      1822      1823      1824 
    ##  7.603922 12.535964 12.431027 10.961908 11.591530 12.221153 14.214957 12.011279 
    ##      1825      1826      1827      1828      1829      1830      1831      1832 
    ## 10.017474 11.276719  9.702663  7.603922 12.850775 14.319894  9.912537 12.431027 
    ##      1833      1834      1835      1836      1837      1838      1839      1840 
    ## 13.585334 10.122412 11.276719  9.702663  7.813796 12.431027 10.227349 12.011279 
    ##      1841      1842      1843      1844      1845      1846      1847      1848 
    ##  9.387852 11.171782  7.918733 11.381656 10.227349 11.591530 10.017474  7.603922 
    ##      1849      1850      1851      1852      1853      1854      1855      1856 
    ## 12.431027  9.912537 13.270523 14.319894 14.214957 12.116216 12.955712  9.912537 
    ##      1857      1858      1859      1860      1861      1862      1863      1864 
    ## 12.431027 12.431027 13.060649  9.177978 10.961908 11.381656 12.221153  7.708859 
    ##      1865      1866      1867      1868      1869      1870      1871      1872 
    ##  9.492789 11.276719  8.968104  8.023670  7.603922 12.955712 11.591530  9.282915 
    ##      1873      1874      1875      1876      1877      1878      1879      1880 
    ## 10.017474 11.591530 11.171782 11.801404  9.492789  9.912537 10.961908 11.486593 
    ##      1881      1882      1883      1884      1885      1886      1887      1888 
    ## 12.535964 10.332286 11.906342  7.603922 11.591530 10.017474  8.863167  9.282915 
    ##      1889      1890      1891      1892      1893      1894      1895      1896 
    ##  9.912537  9.073041 10.961908  9.912537  8.758230 12.221153 12.431027 10.542160 
    ##      1897      1898      1899      1900      1901      1902      1903      1904 
    ## 13.270523 14.424831  9.387852  8.653293 12.116216 10.752034 11.906342  9.492789 
    ##      1905      1906      1907      1908      1909      1910      1911      1912 
    ## 10.017474 12.535964  9.387852  8.968104 10.647097  7.079237 11.591530 12.955712 
    ##      1913      1914      1915      1916      1917      1918      1919      1920 
    ##  8.653293 11.591530  8.863167  8.128607 11.276719 12.011279  8.548356 11.066845 
    ##      1921      1922      1923      1924      1925      1926      1927      1928 
    ## 10.017474  9.702663  9.912537 12.011279  7.079237 13.900146 11.801404  9.492789 
    ##      1929      1930      1931      1932      1933      1934      1935      1936 
    ## 11.276719  8.233544  8.863167 12.431027 12.640901 11.486593 10.542160 13.375460 
    ##      1937      1938      1939      1940      1941      1942      1943      1944 
    ## 10.752034 12.850775 12.745838 14.005083  8.758230 10.017474 11.066845 14.319894 
    ##      1945      1946      1947      1948      1949      1950      1951      1952 
    ## 13.270523  9.807600 12.326090  9.807600  9.702663 12.535964  9.387852  7.603922 
    ##      1953      1954      1955      1956      1957      1958      1959      1960 
    ##  8.338482  9.177978 14.424831 11.591530 12.535964 10.122412  9.807600 12.955712 
    ##      1961      1962      1963      1964      1965      1966      1967      1968 
    ##  7.079237 13.900146  7.079237  8.023670  9.177978 10.961908  8.128607 13.165586 
    ##      1969      1970      1971      1972      1973      1974      1975      1976 
    ## 13.375460  9.387852 11.486593 11.696467 10.437223 13.585334  9.702663  8.758230 
    ##      1977      1978      1979      1980      1981      1982      1983      1984 
    ##  8.128607 14.005083  9.912537 11.906342 13.060649 13.165586  9.912537 13.165586 
    ##      1985      1986      1987      1988      1989      1990      1991      1992 
    ## 12.116216 13.795209  7.079237  9.492789 10.122412  9.073041  8.128607 10.961908 
    ##      1993      1994      1995      1996      1997      1998      1999      2000 
    ## 12.116216  9.387852 14.319894  8.968104 12.431027 14.424831 10.122412 12.640901 
    ##      2001      2002      2003      2004      2005      2006      2007      2008 
    ## 11.906342  9.387852 11.801404 14.319894 10.752034  8.863167 10.752034 13.165586 
    ##      2009      2010      2011      2012      2013      2014      2015      2016 
    ## 10.332286 12.955712 10.542160 10.122412  7.603922 10.752034 12.535964  8.128607 
    ##      2017      2018      2019      2020      2021      2022      2023      2024 
    ## 13.270523 13.060649 11.801404 10.961908 10.542160 11.801404  7.603922  8.443419 
    ##      2025      2026      2027      2028      2029      2030      2031      2032 
    ##  8.338482  8.653293  8.338482 12.221153 13.690271 14.424831  8.758230 10.017474 
    ##      2033      2034      2035      2036      2037      2038      2039      2040 
    ## 12.431027 11.381656 14.214957 11.171782 12.011279 10.122412 12.745838 10.017474 
    ##      2041      2042      2043      2044      2045      2046      2047      2048 
    ##  8.548356  9.807600 11.276719 12.535964  7.079237  7.603922 11.171782 12.011279 
    ##      2049      2050      2051      2052      2053      2054      2055      2056 
    ## 10.542160 13.270523 11.276719 12.431027 13.060649  8.653293  7.918733  8.548356 
    ##      2057      2058      2059      2060      2061      2062      2063      2064 
    ## 11.276719  9.807600 11.801404 12.431027  8.863167 13.585334 10.332286 11.486593 
    ##      2065      2066      2067      2068      2069      2070      2071      2072 
    ## 11.696467 13.375460 11.381656 11.696467  7.079237 13.060649 11.801404  9.387852 
    ##      2073      2074      2075      2076      2077      2078      2079      2080 
    ##  8.863167 14.319894  8.548356  7.603922  9.177978  7.603922  7.079237  7.918733 
    ##      2081      2082      2083      2084      2085      2086      2087      2088 
    ## 10.122412 10.542160  7.603922  8.128607 11.801404  8.758230  7.603922 11.801404 
    ##      2089      2090      2091      2092      2093      2094      2095      2096 
    ## 11.696467 14.110020 11.801404  8.968104  7.708859 12.011279 13.375460 10.017474 
    ##      2097      2098      2099      2100      2101      2102      2103      2104 
    ##  9.807600 13.480397  8.968104 12.116216  7.603922  7.813796  8.968104 11.276719 
    ##      2105      2106      2107      2108      2109      2110      2111      2112 
    ## 11.591530 10.542160 10.227349 11.171782 11.486593  8.128607  8.548356 10.332286 
    ##      2113      2114      2115      2116      2117      2118      2119      2120 
    ##  8.968104  9.912537  9.702663  7.079237  8.338482 10.122412 11.801404 10.647097 
    ##      2121      2122      2123      2124      2125      2126      2127      2128 
    ##  8.968104  8.338482 12.745838  9.597726  9.807600 12.955712  9.282915 11.276719 
    ##      2129      2130      2131      2132      2133      2134      2135      2136 
    ## 12.850775  8.968104  7.603922 12.116216  9.073041 13.900146  9.073041  7.603922 
    ##      2137      2138      2139      2140      2141      2142      2143      2144 
    ## 11.171782 11.801404  8.863167 12.431027 13.585334 11.906342  9.073041 11.696467 
    ##      2145      2146      2147      2148      2149      2150      2151      2152 
    ## 12.221153 12.011279 12.535964 12.011279  8.233544 11.906342 12.640901 14.214957 
    ##      2153      2154      2155      2156      2157      2158      2159      2160 
    ##  8.233544 11.381656 11.591530 11.906342  9.492789  9.597726 12.955712 12.955712 
    ##      2161      2162      2163      2164      2165      2166      2167      2168 
    ##  9.073041 12.535964  9.912537 13.375460  9.282915 12.326090 11.381656  9.387852 
    ##      2169      2170      2171      2172      2173      2174      2175      2176 
    ##  7.079237 12.850775 13.690271 13.375460  9.387852  8.653293 12.640901  8.863167 
    ##      2177      2178      2179      2180      2181      2182      2183      2184 
    ## 12.326090 13.690271 13.795209 12.221153 12.850775 11.066845 14.319894  8.338482 
    ##      2185      2186      2187      2188      2189      2190      2191      2192 
    ## 12.116216 12.535964  9.073041 11.381656 12.431027 10.752034  9.912537 12.116216 
    ##      2193      2194      2195      2196      2197      2198      2199      2200 
    ##  9.073041  8.548356 10.332286  8.128607 12.221153 10.752034 14.319894 10.017474 
    ##      2201      2202      2203      2204      2205      2206      2207      2208 
    ## 10.542160  9.073041  9.073041 10.752034  8.758230 10.227349 10.122412  9.282915 
    ##      2209      2210      2211      2212      2213      2214      2215      2216 
    ## 12.431027 10.332286  8.443419  9.177978 12.431027  8.548356 12.955712  8.128607 
    ##      2217      2218      2219      2220      2221      2222      2223      2224 
    ## 10.122412 11.486593  7.603922  8.128607 14.214957  9.492789 12.640901  8.338482 
    ##      2225      2226      2227      2228      2229      2230      2231      2232 
    ## 12.850775  8.338482 11.696467  9.073041 12.221153  8.758230 13.480397 12.431027 
    ##      2233      2234      2235      2236      2237      2238      2239      2240 
    ## 11.801404 14.214957 10.542160 12.745838  9.387852 13.900146  9.073041  8.653293 
    ##      2241      2242      2243      2244      2245      2246      2247      2248 
    ## 13.165586 10.437223 10.122412 10.542160 10.752034 11.696467 10.437223 11.696467 
    ##      2249      2250      2251      2252      2253      2254      2255      2256 
    ##  8.548356 13.690271 12.326090 11.906342  8.968104 12.640901 13.165586 10.227349 
    ##      2257      2258      2259      2260      2261      2262      2263      2264 
    ##  9.282915 13.060649 10.752034 13.480397  9.597726 11.381656 12.116216  7.708859 
    ##      2265      2266      2267      2268      2269      2270      2271      2272 
    ## 13.900146 11.906342 12.535964 12.011279  9.702663  8.023670  9.807600 11.696467 
    ##      2273      2274      2275      2276      2277      2278      2279      2280 
    ## 13.270523  8.023670 10.647097  8.023670  9.177978 10.437223 12.431027  8.653293 
    ##      2281      2282      2283      2284      2285      2286      2287      2288 
    ## 10.542160 13.060649 11.381656  8.023670 10.332286 13.060649  9.912537 13.165586 
    ##      2289      2290      2291      2292      2293      2294      2295      2296 
    ## 12.955712  8.548356 11.486593  7.603922 10.122412  8.548356  8.338482 12.850775 
    ##      2297      2298      2299      2300      2301      2302      2303      2304 
    ##  8.548356 12.850775  8.968104 14.005083 12.535964  7.079237 13.480397  8.863167 
    ##      2305      2306      2307      2308      2309      2310      2311      2312 
    ## 10.542160 13.060649  8.233544 11.801404 10.961908 12.116216 11.906342 10.961908 
    ##      2313      2314      2315      2316      2317      2318      2319      2320 
    ##  7.813796 13.060649 13.060649 12.745838  7.603922  9.597726 11.696467  8.338482 
    ##      2321      2322      2323      2324      2325      2326      2327      2328 
    ##  8.968104 10.437223  8.863167  9.282915 11.696467 10.647097 10.437223 12.116216 
    ##      2329      2330      2331      2332      2333      2334      2335      2336 
    ##  9.492789 11.381656  9.807600  8.443419  8.233544  7.603922 11.696467  9.597726 
    ##      2337      2338      2339      2340      2341      2342      2343      2344 
    ##  7.079237 11.801404 10.122412  8.338482 12.955712  9.177978  9.492789  8.653293 
    ##      2345      2346      2347      2348      2349      2350      2351      2352 
    ## 12.221153 13.165586  9.597726 11.381656 13.165586  9.597726 11.696467  8.548356 
    ##      2353      2354      2355      2356      2357      2358      2359      2360 
    ## 12.535964 12.326090 10.961908 14.214957 11.906342  9.912537 11.801404 13.165586 
    ##      2361      2362      2363      2364      2365      2366      2367      2368 
    ##  9.282915 12.955712 10.752034  9.282915  7.918733  9.912537 13.480397 13.900146 
    ##      2369      2370      2371      2372      2373      2374      2375      2376 
    ##  9.492789 11.486593 10.017474  9.073041 10.227349 10.122412 10.542160  8.443419 
    ##      2377      2378      2379      2380      2381      2382      2383      2384 
    ## 10.647097 12.745838  8.548356 12.011279  9.807600  9.282915  9.387852  7.079237 
    ##      2385      2386      2387      2388      2389      2390      2391      2392 
    ## 10.961908  8.023670 12.850775 13.375460 12.640901 12.535964  8.758230 10.227349 
    ##      2393      2394      2395      2396      2397      2398      2399      2400 
    ##  8.653293 10.647097 11.381656 13.585334  8.968104  8.443419 12.116216 11.066845 
    ##      2401      2402      2403      2404      2405      2406      2407      2408 
    ## 12.116216 10.017474 12.955712  9.492789  8.023670  7.813796  9.912537  7.603922 
    ##      2409      2410      2411      2412      2413      2414      2415      2416 
    ## 12.535964 11.906342 13.165586  8.863167 12.221153 11.276719  8.128607  7.603922 
    ##      2417      2418      2419      2420      2421      2422      2423      2424 
    ## 10.856971 10.961908  8.443419  8.338482  8.863167  9.597726 10.542160 11.906342 
    ##      2425      2426      2427      2428      2429      2430      2431      2432 
    ## 12.326090 10.647097  7.603922 11.801404 10.332286 11.906342  9.387852  9.282915 
    ##      2433      2434      2435      2436      2437      2438      2439      2440 
    ## 11.066845 10.017474 12.116216  9.807600 12.326090 14.424831  9.702663  8.548356 
    ##      2441      2442      2443      2444      2445      2446      2447      2448 
    ##  9.492789  7.603922  7.603922 10.542160  7.603922 14.005083  7.079237  9.912537 
    ##      2449      2450      2451      2452      2453      2454      2455      2456 
    ##  8.968104  8.338482 10.227349  7.079237 13.270523  9.912537 11.696467  7.918733 
    ##      2457      2458      2459      2460      2461      2462      2463      2464 
    ## 11.801404  7.603922 13.690271  9.073041 12.850775  8.758230 10.227349  9.702663 
    ##      2465      2466      2467      2468      2469      2470      2471      2472 
    ##  9.807600 10.017474  8.023670 11.906342  9.912537 10.227349 11.171782 12.011279 
    ##      2473      2474      2475      2476      2477      2478      2479      2480 
    ##  9.492789  7.813796  9.282915  8.338482 12.535964 14.424831  9.387852  7.603922 
    ##      2481      2482      2483      2484      2485      2486      2487      2488 
    ## 12.850775 11.486593 10.017474 12.221153 11.906342  9.597726 10.437223 12.116216 
    ##      2489      2490      2491      2492      2493      2494      2495      2496 
    ##  8.968104 13.270523 13.480397 10.227349 13.795209  8.548356 13.060649 13.165586 
    ##      2497      2498      2499      2500      2501      2502      2503      2504 
    ## 13.060649 11.906342 14.110020  8.758230  9.912537 10.122412 12.955712 10.332286 
    ##      2505      2506      2507      2508      2509      2510      2511      2512 
    ##  8.863167  9.492789 12.221153 11.906342 10.332286 10.227349 14.214957 11.381656 
    ##      2513      2514      2515      2516      2517      2518      2519      2520 
    ## 10.542160 12.116216 14.214957 13.795209 10.017474 12.640901  8.443419 12.011279 
    ##      2521      2522      2523      2524      2525      2526      2527      2528 
    ## 11.906342 10.961908 11.906342 11.486593 14.319894  8.023670 11.276719 10.961908 
    ##      2529      2530      2531      2532      2533      2534      2535      2536 
    ##  9.177978 11.381656  9.492789 11.066845 12.221153 11.276719  9.597726 11.801404 
    ##      2537      2538      2539      2540      2541      2542      2543      2544 
    ## 10.227349 10.017474 12.011279 13.165586 11.906342 13.585334  7.079237 11.801404 
    ##      2545      2546      2547      2548      2549      2550      2551      2552 
    ##  8.758230  7.079237  9.912537 13.585334 10.752034 10.332286  9.702663 10.437223 
    ##      2553      2554      2555      2556      2557      2558      2559      2560 
    ## 12.535964  8.758230  9.492789  7.079237 11.171782 12.955712 14.319894 12.011279 
    ##      2561      2562      2563      2564      2565      2566      2567      2568 
    ##  8.023670  9.387852 11.171782  8.758230  9.597726  8.233544 11.066845  8.968104 
    ##      2569      2570      2571      2572      2573      2574      2575      2576 
    ##  8.233544  8.968104 13.165586 12.535964 12.326090 12.745838 10.017474 12.326090 
    ##      2577      2578      2579      2580      2581      2582      2583      2584 
    ## 10.227349 12.850775 12.850775 12.850775 11.696467 10.647097 12.640901 13.480397 
    ##      2585      2586      2587      2588      2589      2590      2591      2592 
    ## 11.066845  9.177978 10.122412 10.122412 12.535964  8.863167  9.073041  7.079237 
    ##      2593      2594      2595      2596      2597      2598      2599      2600 
    ## 12.116216  9.387852 11.276719 13.060649 12.221153 12.955712 13.060649  9.073041 
    ##      2601      2602      2603      2604      2605      2606      2607      2608 
    ##  8.653293  8.338482 10.752034 13.060649  9.073041  9.492789  8.443419 11.801404 
    ##      2609      2610      2611      2612      2613      2614      2615      2616 
    ## 11.486593  9.177978 12.850775 13.585334 12.431027 11.171782 13.165586 12.116216 
    ##      2617      2618      2619      2620      2621      2622      2623      2624 
    ## 13.270523 10.856971 13.270523  9.387852  8.863167 11.171782 11.801404 11.486593 
    ##      2625      2626      2627      2628      2629      2630      2631      2632 
    ## 11.276719 13.480397 10.752034 12.745838 10.961908 11.171782 12.011279 14.214957 
    ##      2633      2634      2635      2636      2637      2638      2639      2640 
    ##  7.603922  7.813796 11.591530 12.326090 13.060649  8.128607 10.856971 12.431027 
    ##      2641      2642      2643      2644      2645      2646      2647      2648 
    ## 11.486593 13.375460  9.807600 10.856971  9.177978 14.110020  9.492789 10.647097 
    ##      2649      2650      2651      2652      2653      2654      2655      2656 
    ## 10.332286 11.591530  7.813796  8.863167  9.912537 12.116216 12.535964  9.702663 
    ##      2657      2658      2659      2660      2661      2662      2663      2664 
    ##  9.387852  9.282915  7.708859  9.807600  7.603922  9.597726 13.480397 12.326090 
    ##      2665      2666      2667      2668      2669      2670      2671      2672 
    ## 13.585334  8.758230 10.752034 11.276719  8.023670 13.480397 13.690271 11.276719 
    ##      2673      2674      2675      2676      2677      2678      2679      2680 
    ##  7.603922 13.165586 11.486593  7.603922 12.640901  9.387852 13.375460 14.319894 
    ##      2681      2682      2683      2684      2685      2686      2687      2688 
    ## 10.961908 14.319894  8.758230  9.912537 11.486593 13.585334  8.653293  9.492789 
    ##      2689      2690      2691      2692      2693      2694      2695      2696 
    ## 12.011279 12.221153 13.165586 11.381656 10.122412  8.653293 11.381656 14.110020 
    ##      2697      2698      2699      2700      2701      2702      2703      2704 
    ##  8.443419  9.807600  8.443419  9.702663 11.696467  9.912537  8.653293  8.863167 
    ##      2705      2706      2707      2708      2709      2710      2711      2712 
    ## 11.801404  9.912537  8.653293 10.017474 12.850775 12.326090 10.332286 11.066845 
    ##      2713      2714      2715      2716      2717      2718      2719      2720 
    ## 12.326090  8.863167 10.332286 11.066845 12.745838 10.856971  8.443419  8.758230 
    ##      2721      2722      2723      2724      2725      2726      2727      2728 
    ##  7.079237  7.813796 13.165586 10.227349  9.597726 12.011279  9.177978  9.807600 
    ##      2729      2730      2731      2732      2733      2734      2735      2736 
    ## 13.060649  8.653293  9.282915 12.431027 13.060649 13.165586 13.585334  9.912537 
    ##      2737      2738      2739      2740      2741      2742      2743      2744 
    ## 12.116216  8.338482 11.906342 12.431027  9.807600  8.968104  9.387852 10.332286 
    ##      2745      2746      2747      2748      2749      2750      2751      2752 
    ## 11.906342 10.017474  8.863167 11.906342 12.745838  9.387852  7.813796 10.227349 
    ##      2753      2754      2755      2756      2757      2758      2759      2760 
    ## 13.165586  8.128607 11.906342 12.431027  9.807600 12.955712  8.968104 13.060649 
    ##      2761      2762      2763      2764      2765      2766      2767      2768 
    ##  7.603922 12.850775 12.011279  8.653293 13.060649  8.653293  7.708859 12.535964 
    ##      2769      2770      2771      2772      2773      2774      2775      2776 
    ## 12.850775 11.801404  7.603922  7.708859 11.906342 14.424831 10.961908 13.165586 
    ##      2777      2778      2779      2780      2781      2782      2783      2784 
    ## 12.116216 11.906342 13.270523  8.968104  9.073041 12.640901 14.214957 11.591530 
    ##      2785      2786      2787      2788      2789      2790      2791      2792 
    ## 14.005083  8.863167  8.653293  8.338482  9.282915  9.912537 12.326090  8.653293 
    ##      2793      2794      2795      2796      2797      2798      2799      2800 
    ## 11.696467 13.165586 13.270523  8.233544  9.282915  9.597726 13.900146 13.690271 
    ##      2801      2802      2803      2804      2805      2806      2807      2808 
    ##  9.492789 13.060649  7.813796 12.640901 11.276719  9.073041 14.110020 10.227349 
    ##      2809      2810      2811      2812      2813      2814      2815      2816 
    ##  9.177978 11.591530  9.177978  8.338482 12.640901 11.801404 10.017474 11.591530 
    ##      2817      2818      2819      2820      2821      2822      2823      2824 
    ## 13.060649 14.319894  7.079237 12.221153 14.110020 12.745838 12.431027  8.338482 
    ##      2825      2826      2827      2828      2829      2830      2831      2832 
    ##  9.492789  9.702663 11.591530  7.079237 10.961908  7.603922 12.850775 10.227349 
    ##      2833      2834      2835      2836      2837      2838      2839      2840 
    ##  9.282915  9.912537 10.122412 10.647097  9.387852 11.171782 11.066845 10.752034 
    ##      2841      2842      2843      2844      2845      2846      2847      2848 
    ## 12.326090 12.850775 14.214957  8.653293 12.955712 12.221153 11.591530 11.171782 
    ##      2849      2850      2851      2852      2853      2854      2855      2856 
    ##  9.177978 11.591530 12.011279  9.387852 12.850775 12.745838  8.758230 10.752034 
    ##      2857      2858      2859      2860      2861      2862      2863      2864 
    ##  7.708859  7.603922 12.535964  9.807600 10.647097  8.338482  8.863167 12.850775 
    ##      2865      2866      2867      2868      2869      2870      2871      2872 
    ##  9.912537 11.801404  8.548356 10.227349 10.856971 10.227349 12.955712 11.696467 
    ##      2873      2874      2875      2876      2877      2878      2879      2880 
    ## 11.906342 12.431027  9.912537 12.850775 11.591530 14.110020  8.443419 12.221153 
    ##      2881      2882      2883      2884      2885      2886      2887      2888 
    ## 12.011279 12.011279 10.647097 10.017474 13.585334 11.171782 12.116216  9.073041 
    ##      2889      2890      2891      2892      2893      2894      2895      2896 
    ## 11.486593 11.906342 12.116216 11.906342  8.023670 11.801404 10.856971  7.603922 
    ##      2897      2898      2899      2900      2901      2902      2903      2904 
    ##  7.603922  8.653293 10.752034 12.640901 12.745838  8.863167 14.424831  8.653293 
    ##      2905      2906      2907      2908      2909      2910      2911      2912 
    ##  9.073041 10.856971 10.961908 13.480397  9.282915  7.813796 10.542160 12.640901 
    ##      2913      2914      2915      2916      2917      2918      2919      2920 
    ##  9.807600 13.375460  7.603922  9.702663 10.647097 12.116216  9.912537 12.116216 
    ##      2921      2922      2923      2924      2925      2926      2927      2928 
    ## 14.319894 12.221153  9.492789 10.332286 11.591530 12.535964 11.801404 11.276719 
    ##      2929      2930      2931      2932      2933      2934      2935      2936 
    ##  7.813796 11.276719  9.282915 10.542160  9.807600  9.492789 13.060649 13.060649 
    ##      2937      2938      2939      2940      2941      2942      2943      2944 
    ##  9.282915 11.171782 13.165586 12.326090 12.955712  8.338482  9.597726  9.597726 
    ##      2945      2946      2947      2948      2949      2950      2951      2952 
    ## 10.332286 10.647097  7.708859 12.116216 12.116216 12.326090 12.326090 11.591530 
    ##      2953      2954      2955      2956      2957      2958      2959      2960 
    ## 10.437223 13.270523  9.807600 10.542160 11.066845 12.955712 10.961908 11.381656 
    ##      2961      2962      2963      2964      2965      2966      2967      2968 
    ## 10.437223 10.122412  9.912537  9.282915 11.171782 11.066845 11.801404 12.955712 
    ##      2969      2970      2971      2972      2973      2974      2975      2976 
    ##  7.813796 12.745838 10.542160 11.906342  8.863167  9.387852  9.702663 12.955712 
    ##      2977      2978      2979      2980      2981      2982      2983      2984 
    ## 11.066845  8.443419 12.326090  9.282915  8.863167 14.214957  9.282915 12.221153 
    ##      2985      2986      2987      2988      2989      2990      2991      2992 
    ## 10.647097  7.708859  7.603922 14.424831 13.690271  7.603922 13.375460 11.696467 
    ##      2993      2994      2995      2996      2997      2998      2999      3000 
    ## 14.319894 11.171782 12.850775 11.591530 10.437223 11.906342 10.332286 13.480397 
    ##      3001      3002      3003      3004      3005      3006      3007      3008 
    ##  9.282915 10.437223  9.597726 10.017474  9.807600  9.912537  9.597726 14.424831 
    ##      3009      3010      3011      3012      3013      3014      3015      3016 
    ## 13.270523  9.492789 11.381656  7.603922  7.603922 14.005083  8.758230  9.387852 
    ##      3017      3018      3019      3020      3021      3022      3023      3024 
    ##  8.548356 11.171782 13.585334  7.813796  8.443419 11.276719  8.233544 11.696467 
    ##      3025      3026      3027      3028      3029      3030      3031      3032 
    ##  7.079237 10.961908  9.073041 10.122412 11.171782 12.955712  9.387852 12.640901 
    ##      3033      3034      3035      3036      3037      3038      3039      3040 
    ## 10.332286 10.752034 12.955712  8.548356 12.640901  9.597726  8.023670  7.079237 
    ##      3041      3042      3043      3044      3045      3046      3047      3048 
    ## 12.640901  8.653293 12.221153  8.023670 11.381656  9.597726 14.005083 12.640901 
    ##      3049      3050      3051      3052      3053      3054      3055      3056 
    ## 13.690271  9.597726 10.332286 10.227349 11.906342  9.702663 10.227349  7.603922 
    ##      3057      3058      3059      3060      3061      3062      3063      3064 
    ##  9.912537 14.424831 12.221153 12.326090  8.233544  9.492789 14.319894 10.332286 
    ##      3065      3066      3067      3068      3069      3070      3071      3072 
    ##  9.282915  9.702663  7.603922  8.758230  8.758230  9.912537 10.752034  9.912537 
    ##      3073      3074      3075      3076      3077      3078      3079      3080 
    ##  8.548356 13.795209 10.542160  9.807600 10.647097 10.017474  9.597726 12.011279 
    ##      3081      3082      3083      3084      3085      3086      3087      3088 
    ##  8.233544 13.165586 13.165586 11.276719 12.326090 10.437223  9.073041  9.702663 
    ##      3089      3090      3091      3092      3093      3094      3095      3096 
    ##  9.597726 11.906342 13.795209  8.338482 11.381656 12.326090  8.863167  8.233544 
    ##      3097      3098      3099      3100      3101      3102      3103      3104 
    ## 12.431027  7.603922 11.591530 11.591530 12.221153  9.282915 11.696467  9.597726 
    ##      3105      3106      3107      3108      3109      3110      3111      3112 
    ## 12.011279  7.603922  8.233544  7.708859 13.165586  7.603922  8.023670 10.961908 
    ##      3113      3114      3115      3116      3117      3118      3119      3120 
    ##  8.128607  8.338482 10.017474 13.165586 13.690271  9.492789  9.492789 13.060649 
    ##      3121      3122      3123      3124      3125      3126      3127      3128 
    ##  8.653293  7.603922 13.060649 12.431027  7.603922 11.171782 11.486593 10.752034 
    ##      3129      3130      3131      3132      3133      3134      3135      3136 
    ## 10.856971 11.696467 10.856971 10.227349 14.005083 13.165586 11.381656 12.955712 
    ##      3137      3138      3139      3140      3141      3142      3143      3144 
    ## 10.752034  9.282915 11.171782 10.122412 12.535964 12.745838 11.276719 10.227349 
    ##      3145      3146      3147      3148      3149      3150      3151      3152 
    ## 12.955712 12.955712 11.171782 14.214957  9.702663  7.603922  9.597726  8.233544 
    ##      3153      3154      3155      3156      3157      3158      3159      3160 
    ##  8.338482  9.387852 11.066845 13.480397 11.171782 13.375460 13.060649 12.221153 
    ##      3161      3162      3163      3164      3165      3166      3167      3168 
    ##  9.492789  7.918733 10.332286 12.326090  7.079237 11.906342  8.128607 11.696467 
    ##      3169      3170      3171      3172      3173      3174      3175      3176 
    ## 11.171782  9.073041 12.326090  7.603922  7.708859 13.270523  8.653293 12.116216 
    ##      3177      3178      3179      3180      3181      3182      3183      3184 
    ## 10.856971  7.708859  7.603922  9.073041 10.752034 10.122412  9.387852 10.961908 
    ##      3185      3186      3187      3188      3189      3190      3191      3192 
    ##  9.702663  9.387852  9.073041 11.276719  9.912537  9.702663 10.856971  9.177978 
    ##      3193      3194      3195      3196      3197      3198      3199      3200 
    ## 12.745838 12.850775  7.603922 10.752034 11.381656  8.233544 11.906342 12.011279 
    ##      3201      3202      3203      3204      3205      3206      3207      3208 
    ## 10.017474  8.863167 12.955712 12.326090 11.276719 13.480397  9.073041 11.486593 
    ##      3209      3210      3211      3212      3213      3214      3215      3216 
    ## 10.542160  7.603922  8.863167  8.863167  8.968104  9.597726  8.758230  7.079237 
    ##      3217      3218      3219      3220      3221      3222      3223      3224 
    ## 12.326090  8.653293  8.233544 12.850775  7.079237 12.745838  9.282915 11.066845 
    ##      3225      3226      3227      3228      3229      3230      3231      3232 
    ##  9.912537  9.492789 11.591530 12.116216  9.177978  9.387852 12.640901  9.492789 
    ##      3233      3234      3235      3236      3237      3238      3239      3240 
    ##  9.073041  9.492789 12.431027  7.813796  8.863167 12.326090  9.282915 10.961908 
    ##      3241      3242      3243      3244      3245      3246      3247      3248 
    ## 10.856971  9.702663  8.233544 11.486593 10.752034 11.696467 12.221153  9.387852 
    ##      3249      3250      3251      3252      3253      3254      3255      3256 
    ## 10.856971  8.968104 13.375460 13.375460 12.011279 11.066845 11.591530 12.955712 
    ##      3257      3258      3259      3260      3261      3262      3263      3264 
    ##  9.702663  7.603922 13.480397 11.171782 12.955712 12.116216  8.548356  7.603922 
    ##      3265      3266      3267      3268      3269      3270      3271      3272 
    ## 10.227349 10.542160  8.758230  9.177978 10.542160 10.856971  9.597726  8.863167 
    ##      3273      3274      3275      3276      3277      3278      3279      3280 
    ##  7.079237  9.282915 11.486593  9.702663  7.079237 10.961908  9.177978  9.492789 
    ##      3281      3282      3283      3284      3285      3286      3287      3288 
    ##  9.702663 11.171782 12.535964 11.486593 11.171782  8.863167  8.968104  8.548356 
    ##      3289      3290      3291      3292      3293      3294      3295      3296 
    ## 13.270523 13.795209  8.653293 11.906342  9.073041 11.276719 10.542160  7.918733 
    ##      3297      3298      3299      3300      3301      3302      3303      3304 
    ## 12.221153  9.387852  8.338482  7.603922  7.079237 14.424831 11.066845  9.702663 
    ##      3305      3306      3307      3308      3309      3310      3311      3312 
    ##  7.918733  9.073041  9.912537 10.227349 12.745838  7.918733  9.597726 13.060649 
    ##      3313      3314      3315      3316      3317      3318      3319      3320 
    ##  9.597726  9.282915 12.535964  7.813796  8.758230 11.381656  7.603922 10.961908 
    ##      3321      3322      3323      3324      3325      3326      3327      3328 
    ##  9.177978 10.856971 11.486593 12.850775  9.807600  9.073041 12.326090 12.011279 
    ##      3329      3330      3331      3332      3333      3334      3335      3336 
    ## 12.850775 11.696467 11.276719  9.177978 11.696467  7.603922 11.171782 10.227349 
    ##      3337      3338      3339      3340      3341      3342      3343      3344 
    ## 11.276719  7.603922 10.122412 13.060649 10.961908 12.221153 12.850775 13.375460 
    ##      3345      3346      3347      3348      3349      3350      3351      3352 
    ##  8.653293 11.696467 12.535964  8.968104 11.696467 11.486593 10.227349 10.647097 
    ##      3353      3354      3355      3356      3357      3358      3359      3360 
    ##  9.807600  8.863167 10.332286 13.795209 10.437223 11.381656  7.079237 10.542160 
    ##      3361      3362      3363      3364      3365      3366      3367      3368 
    ## 13.480397 10.752034 10.752034 11.276719 14.319894 11.591530 12.116216  9.807600 
    ##      3369      3370      3371      3372      3373      3374      3375      3376 
    ##  9.282915 10.542160 14.214957  7.603922  7.603922  8.863167 13.480397  9.282915 
    ##      3377      3378      3379      3380      3381      3382      3383      3384 
    ## 11.171782 10.437223 14.214957 11.276719 12.326090 12.850775 13.270523 10.122412 
    ##      3385      3386      3387      3388      3389      3390      3391      3392 
    ## 12.011279 12.011279 10.856971 10.437223 10.961908  9.912537  8.968104 10.437223 
    ##      3393      3394      3395      3396      3397      3398      3399      3400 
    ## 11.486593 12.535964 12.221153 12.221153 11.801404 11.066845 11.486593 10.647097 
    ##      3401      3402      3403      3404      3405      3406      3407      3408 
    ##  8.548356 10.332286 13.585334 10.961908 11.276719 11.066845 11.381656 10.752034 
    ##      3409      3410      3411      3412      3413      3414      3415      3416 
    ## 10.647097 10.017474 14.110020 14.319894  8.233544 12.850775 10.017474  9.387852 
    ##      3417      3418      3419      3420      3421      3422      3423      3424 
    ## 11.801404 13.585334 11.381656 11.906342 14.319894  8.758230  9.702663  8.653293 
    ##      3425      3426      3427      3428      3429      3430      3431      3432 
    ##  7.603922 12.221153 11.591530 14.110020  8.653293  9.597726 13.585334 12.850775 
    ##      3433      3434      3435      3436      3437      3438      3439      3440 
    ##  9.807600 13.270523 12.221153 11.171782 10.542160 11.801404  9.702663 14.005083 
    ##      3441      3442      3443      3444      3445      3446      3447      3448 
    ## 10.961908  7.708859 11.591530  8.233544  8.758230 14.110020 11.171782 10.542160 
    ##      3449      3450      3451      3452      3453      3454      3455      3456 
    ## 13.060649  9.387852 12.745838  8.338482  8.128607 12.326090 12.116216  8.548356 
    ##      3457      3458      3459      3460      3461      3462      3463      3464 
    ## 13.060649 12.535964  8.653293  7.079237 10.437223 12.745838 11.066845 10.856971 
    ##      3465      3466      3467      3468      3469      3470      3471      3472 
    ##  9.282915 13.375460  9.073041  7.079237 10.542160 11.381656 11.486593 11.486593 
    ##      3473      3474      3475      3476      3477      3478      3479      3480 
    ##  8.653293 10.542160  9.387852 13.795209 12.850775  9.177978 13.165586 10.752034 
    ##      3481      3482      3483      3484      3485      3486      3487      3488 
    ##  8.233544 11.906342 10.017474  7.813796  7.708859 13.165586  8.128607  9.702663 
    ##      3489      3490      3491      3492      3493      3494      3495      3496 
    ## 12.745838 10.122412 10.122412 14.005083 10.542160 12.221153 11.066845  8.023670 
    ##      3497      3498      3499      3500      3501      3502      3503      3504 
    ## 11.696467 13.585334 12.535964 12.221153  9.282915  8.968104  8.548356 11.696467 
    ##      3505      3506      3507      3508      3509      3510      3511      3512 
    ##  7.079237 14.005083  9.387852  9.177978 12.640901  8.758230  8.653293 14.424831 
    ##      3513      3514      3515      3516      3517      3518      3519      3520 
    ## 12.116216  8.338482  9.282915 11.066845  9.492789 12.221153 13.060649  7.079237 
    ##      3521      3522      3523      3524      3525      3526      3527      3528 
    ##  8.968104  9.912537 10.227349 13.165586 10.542160 12.116216 12.221153 12.640901 
    ##      3529      3530      3531      3532      3533      3534      3535      3536 
    ##  9.492789 12.535964  8.023670 11.381656 10.122412 12.221153 10.752034 12.221153 
    ##      3537      3538      3539      3540      3541      3542      3543      3544 
    ## 11.486593  7.708859  9.807600 11.591530  8.548356 10.437223 12.745838 11.591530 
    ##      3545      3546      3547      3548      3549      3550      3551      3552 
    ## 12.431027 10.856971  9.807600 12.326090  8.548356  7.079237  9.492789  9.912537 
    ##      3553      3554      3555      3556      3557      3558      3559      3560 
    ##  7.079237 13.795209  9.702663 12.011279  9.177978 12.221153 10.437223  8.128607 
    ##      3561      3562      3563      3564      3565      3566      3567      3568 
    ## 11.801404  8.128607 11.906342 10.122412  8.863167 12.326090 13.900146 13.795209 
    ##      3569      3570      3571      3572      3573      3574      3575      3576 
    ##  7.813796  9.807600 13.060649 10.332286 13.165586 13.165586  7.603922 10.017474 
    ##      3577      3578      3579      3580      3581      3582      3583      3584 
    ## 11.591530 14.214957 11.906342 12.535964 11.591530 12.431027 11.486593 10.961908 
    ##      3585      3586      3587      3588      3589      3590      3591      3592 
    ##  7.603922 13.165586  9.073041 10.647097  9.912537 12.850775 10.856971  9.702663 
    ##      3593      3594      3595      3596      3597      3598      3599      3600 
    ## 13.165586 13.900146 10.542160  7.079237 10.017474  9.702663 10.647097  9.807600 
    ##      3601      3602      3603      3604      3605      3606      3607      3608 
    ## 12.535964 12.011279  8.863167  9.912537  9.912537 13.375460  8.758230 10.437223 
    ##      3609      3610      3611      3612      3613      3614      3615      3616 
    ##  7.603922  8.653293  9.073041 12.955712  7.603922  8.548356 12.326090 11.906342 
    ##      3617      3618      3619      3620      3621      3622      3623      3624 
    ##  8.968104 10.227349 11.171782 12.640901 12.116216 10.856971 10.752034  9.073041 
    ##      3625      3626      3627      3628      3629      3630      3631      3632 
    ##  8.128607 11.906342 13.375460 12.431027 11.066845 14.319894 11.801404  9.597726 
    ##      3633      3634      3635      3636      3637      3638      3639      3640 
    ##  9.177978  7.079237 10.017474  9.177978  8.863167 12.221153  8.653293  7.079237 
    ##      3641      3642      3643      3644      3645      3646      3647      3648 
    ##  9.492789 11.801404  8.863167  9.492789 12.640901  9.387852 10.332286 12.116216 
    ##      3649      3650      3651      3652      3653      3654      3655      3656 
    ##  8.443419  8.968104 10.752034 10.856971  9.702663 11.801404  9.492789 14.110020 
    ##      3657      3658      3659      3660      3661      3662      3663      3664 
    ## 12.535964 11.171782 10.332286  9.492789  9.702663 12.011279 10.542160 10.647097 
    ##      3665      3666      3667      3668      3669      3670      3671      3672 
    ## 10.017474 11.591530 14.424831 12.955712 11.906342 10.437223 11.591530 12.431027 
    ##      3673      3674      3675      3676      3677      3678      3679      3680 
    ## 13.690271  8.758230 10.122412  8.863167  7.603922 12.850775  9.912537  9.492789 
    ##      3681      3682      3683      3684      3685      3686      3687      3688 
    ##  9.492789 12.745838 11.801404 10.122412 10.961908 10.961908 12.535964  9.807600 
    ##      3689      3690      3691      3692      3693      3694      3695      3696 
    ##  9.597726  8.128607  9.177978 12.221153  9.702663 13.270523 12.535964 14.424831 
    ##      3697      3698      3699      3700      3701      3702      3703      3704 
    ## 11.696467 14.319894 12.116216 13.585334 11.276719 10.017474 12.850775  8.338482 
    ##      3705      3706      3707      3708      3709      3710      3711      3712 
    ## 10.647097 11.276719  9.387852 10.122412  7.603922 10.647097 13.270523 11.171782 
    ##      3713      3714      3715      3716      3717      3718      3719      3720 
    ## 10.542160 11.066845 11.066845 11.696467  9.597726 13.480397 11.906342  7.603922 
    ##      3721      3722      3723      3724      3725      3726      3727      3728 
    ## 14.110020 13.585334 12.955712 11.276719 12.850775  8.968104 14.424831 13.060649 
    ##      3729      3730      3731      3732      3733      3734      3735      3736 
    ## 10.437223 11.381656 13.375460 13.900146 12.850775  9.597726 13.585334 10.752034 
    ##      3737      3738      3739      3740      3741      3742      3743      3744 
    ##  9.177978 10.647097 12.326090 13.165586 10.017474 12.116216 12.011279 14.319894 
    ##      3745      3746      3747      3748      3749      3750      3751      3752 
    ##  8.548356 10.017474 14.424831  8.443419 10.647097  7.079237 13.585334 11.276719 
    ##      3753      3754      3755      3756      3757      3758      3759      3760 
    ## 10.122412 10.961908 12.116216  9.702663  8.758230 10.437223  7.079237  7.079237 
    ##      3761      3762      3763      3764      3765      3766      3767      3768 
    ##  8.443419  7.079237  7.079237  8.548356  7.918733 14.214957 10.437223 10.437223 
    ##      3769      3770      3771      3772      3773      3774      3775      3776 
    ##  7.079237 12.535964  7.603922 13.165586  8.023670 13.585334 11.906342 12.955712 
    ##      3777      3778      3779      3780      3781      3782      3783      3784 
    ##  9.702663 12.535964 12.011279 11.171782 12.745838  9.073041  8.653293  8.548356 
    ##      3785      3786      3787      3788      3789      3790      3791      3792 
    ##  8.023670 11.066845  9.912537 13.480397  8.338482 12.431027  8.653293 14.424831 
    ##      3793      3794      3795      3796      3797      3798      3799      3800 
    ## 11.171782  7.918733 10.647097 12.221153 10.961908  9.702663 10.856971 11.381656 
    ##      3801      3802      3803      3804      3805      3806      3807      3808 
    ## 10.752034  9.492789 12.116216 11.171782  9.702663 10.437223  9.597726 12.011279 
    ##      3809      3810      3811      3812      3813      3814      3815      3816 
    ## 10.647097  9.073041 11.801404 14.319894  7.813796 10.961908  7.603922 13.270523 
    ##      3817      3818      3819      3820      3821      3822      3823      3824 
    ## 11.171782 10.122412 12.116216 13.270523 10.122412 11.906342  8.653293 14.424831 
    ##      3825      3826      3827      3828      3829      3830      3831      3832 
    ##  7.918733 10.332286 14.214957 11.801404 10.017474  8.863167  8.548356 11.276719 
    ##      3833      3834      3835      3836      3837      3838      3839      3840 
    ##  9.807600 12.850775  8.128607  7.079237  9.282915 11.276719  7.603922 11.906342 
    ##      3841      3842      3843      3844      3845      3846      3847      3848 
    ##  9.177978 12.535964  7.918733 10.122412 10.437223 12.535964 13.060649 13.690271 
    ##      3849      3850      3851      3852      3853      3854      3855      3856 
    ##  8.968104  8.128607  7.079237 12.535964  9.912537 10.017474 10.227349  8.023670 
    ##      3857      3858      3859      3860      3861      3862      3863      3864 
    ## 13.900146 13.900146 11.066845  7.603922 12.011279  7.708859  8.968104  8.443419 
    ##      3865      3866      3867      3868      3869      3870      3871      3872 
    ## 11.801404  9.177978 12.011279  8.128607  9.912537 11.696467 12.535964 14.214957 
    ##      3873      3874      3875      3876      3877      3878      3879      3880 
    ##  8.968104  8.023670  9.073041  9.912537  8.653293  8.758230  7.079237  9.177978 
    ##      3881      3882      3883      3884      3885      3886      3887      3888 
    ## 14.214957  9.492789  9.702663 13.795209  8.023670  8.023670  9.597726  9.597726 
    ##      3889      3890      3891      3892      3893      3894      3895      3896 
    ##  7.918733  8.968104  8.863167 10.332286 11.906342 12.011279 12.011279 10.437223 
    ##      3897      3898      3899      3900      3901      3902      3903      3904 
    ## 10.856971 14.319894  8.443419  7.079237 14.110020 10.227349 13.165586  9.597726 
    ##      3905      3906      3907      3908      3909      3910      3911      3912 
    ## 13.480397 10.542160 11.591530  9.073041 12.745838 10.122412  8.653293  8.653293 
    ##      3913      3914      3915      3916      3917      3918      3919      3920 
    ## 10.227349  8.968104 10.332286  9.807600 12.850775  9.387852 12.116216 12.535964 
    ##      3921      3922      3923      3924      3925      3926      3927      3928 
    ##  9.177978  9.282915 11.801404  8.758230 11.591530 10.961908  9.702663 13.480397 
    ##      3929      3930      3931      3932      3933      3934      3935      3936 
    ##  8.128607 14.424831  7.079237  8.653293  9.282915 10.437223  9.282915 10.437223 
    ##      3937      3938      3939      3940      3941      3942      3943      3944 
    ## 13.270523  8.758230 10.437223 12.745838 10.122412  8.128607  8.338482 13.690271 
    ##      3945      3946      3947      3948      3949      3950      3951      3952 
    ## 14.110020  8.338482 11.066845 12.745838 12.326090 11.486593  9.492789 10.961908 
    ##      3953      3954      3955      3956      3957      3958      3959      3960 
    ## 10.332286 12.535964 13.270523  9.282915  7.813796 13.480397 10.647097 12.431027 
    ##      3961      3962      3963      3964      3965      3966      3967      3968 
    ## 12.640901 14.110020 11.171782 10.752034 12.221153 10.647097 10.227349 11.801404 
    ##      3969      3970      3971      3972      3973      3974      3975      3976 
    ## 13.480397  7.708859 12.640901  9.387852  9.282915 10.227349  7.079237  7.079237 
    ##      3977      3978      3979      3980      3981      3982      3983      3984 
    ##  7.708859  8.023670 12.955712 10.227349  8.758230  8.758230 10.856971 10.647097 
    ##      3985      3986      3987      3988      3989      3990      3991      3992 
    ## 10.647097  7.918733 14.110020  9.597726 12.850775  9.073041 11.066845  8.968104 
    ##      3993      3994      3995      3996      3997      3998      3999      4000 
    ## 10.332286 13.060649  8.233544 12.431027 10.961908 10.647097  8.338482 10.856971 
    ##      4001      4002      4003      4004      4005      4006      4007      4008 
    ## 10.227349  9.597726  7.603922 11.906342  8.863167 11.696467 13.060649  8.443419 
    ##      4009      4010      4011      4012      4013      4014      4015      4016 
    ##  9.702663 13.165586 12.221153  8.758230 12.745838  9.177978 13.060649 10.437223 
    ##      4017      4018      4019      4020      4021      4022      4023      4024 
    ## 11.591530 13.165586  8.548356 12.955712  9.492789 13.690271 11.801404  8.758230 
    ##      4025      4026      4027      4028      4029      4030      4031      4032 
    ## 11.171782 10.961908  9.073041 11.486593  8.233544  8.968104  9.702663  9.807600 
    ##      4033      4034      4035      4036      4037      4038      4039      4040 
    ## 10.227349  8.653293 12.011279  7.918733 11.801404 10.856971 11.276719  8.968104 
    ##      4041      4042      4043      4044      4045      4046      4047      4048 
    ##  8.023670  9.702663 10.227349 13.480397  8.128607 11.801404  9.073041 14.005083 
    ##      4049      4050      4051      4052      4053      4054      4055      4056 
    ## 12.431027  9.387852  9.912537 10.647097 12.221153 12.221153 12.955712  9.073041 
    ##      4057      4058      4059      4060      4061      4062      4063      4064 
    ## 10.017474 12.535964  8.653293 14.424831 11.381656 11.276719 10.437223 14.214957 
    ##      4065      4066      4067      4068      4069      4070      4071      4072 
    ## 12.326090 13.060649  7.079237 11.486593  9.597726 12.221153 12.850775 10.856971 
    ##      4073      4074      4075      4076      4077      4078      4079      4080 
    ##  9.702663 12.955712  9.912537 10.122412  9.807600 13.165586 12.535964 11.486593 
    ##      4081      4082      4083      4084      4085      4086      4087      4088 
    ##  9.492789  9.073041  9.492789 14.005083 10.122412 12.011279 14.005083 11.276719 
    ##      4089      4090      4091      4092      4093      4094      4095      4096 
    ## 10.332286 12.955712  9.282915  8.758230 11.276719 12.431027 14.110020 13.375460 
    ##      4097      4098      4099      4100      4101      4102      4103      4104 
    ## 12.745838  7.918733 11.906342  8.653293  9.492789 13.690271  9.702663 14.424831 
    ##      4105      4106      4107      4108      4109      4110      4111      4112 
    ## 11.591530 11.906342 12.955712 10.017474  8.338482 10.752034  9.492789  9.073041 
    ##      4113      4114      4115      4116      4117      4118      4119      4120 
    ## 14.424831  7.813796  8.233544  8.338482 13.060649 13.900146  8.443419  9.282915 
    ##      4121      4122      4123      4124      4125      4126      4127      4128 
    ## 14.319894  9.912537 13.165586 14.110020 13.060649  9.702663 12.221153 10.752034 
    ##      4129      4130      4131      4132      4133      4134      4135      4136 
    ##  8.653293 14.424831  9.282915  8.653293 12.745838  9.282915  7.079237  8.128607 
    ##      4137      4138      4139      4140      4141      4142      4143      4144 
    ## 10.752034  7.708859 13.270523  9.807600  9.702663 14.319894  9.912537 13.270523 
    ##      4145      4146      4147      4148      4149      4150      4151      4152 
    ## 13.795209 13.060649 12.431027  7.603922  9.073041 13.690271 11.171782  8.233544 
    ##      4153      4154      4155      4156      4157      4158      4159      4160 
    ##  8.653293 12.640901  7.603922 12.326090  8.233544 13.795209  7.708859 10.437223 
    ##      4161      4162      4163      4164      4165      4166      4167      4168 
    ## 11.276719  7.079237  9.282915 10.542160  9.387852  8.653293 10.856971  8.968104 
    ##      4169      4170      4171      4172      4173      4174      4175      4176 
    ## 10.227349 12.011279 13.165586 11.906342 12.535964 12.640901 12.640901 11.801404 
    ##      4177      4178      4179      4180      4181      4182      4183      4184 
    ## 13.690271  8.758230 12.850775  9.073041 14.110020 13.165586 14.424831  7.079237 
    ##      4185      4186      4187      4188      4189      4190      4191      4192 
    ## 12.850775  9.912537  9.387852 10.017474  9.282915 10.856971 10.332286 10.122412 
    ##      4193      4194      4195      4196      4197      4198      4199      4200 
    ## 13.270523  7.603922 10.542160  9.073041  9.177978  7.813796 13.060649 11.066845 
    ##      4201      4202      4203      4204      4205      4206      4207      4208 
    ## 11.171782  7.708859  8.758230  9.492789 12.850775 12.116216 11.171782  9.492789 
    ##      4209      4210      4211      4212      4213      4214      4215      4216 
    ## 11.066845  8.863167 13.270523  9.702663 13.480397 12.640901  7.603922  7.813796 
    ##      4217      4218      4219      4220      4221      4222      4223      4224 
    ##  9.912537  9.702663  8.548356  9.702663  7.918733 10.017474 12.745838  8.758230 
    ##      4225      4226      4227      4228      4229      4230      4231      4232 
    ##  8.023670 10.647097 10.856971 13.795209 10.437223 10.227349  8.863167  9.912537 
    ##      4233      4234      4235      4236      4237      4238      4239      4240 
    ##  8.443419 12.745838 11.591530 13.585334  7.603922  7.708859 11.381656  8.653293 
    ##      4241      4242      4243      4244      4245      4246      4247      4248 
    ##  8.863167 12.431027 13.900146 10.437223  7.079237 10.122412 13.795209 13.795209 
    ##      4249      4250      4251      4252      4253      4254      4255      4256 
    ##  9.807600 12.221153 13.165586 12.326090 13.690271 12.745838 11.276719  8.443419 
    ##      4257      4258      4259      4260      4261      4262      4263      4264 
    ##  7.603922  9.177978 11.801404  9.387852  9.912537  7.079237  8.863167 14.424831 
    ##      4265      4266      4267      4268      4269      4270      4271      4272 
    ## 11.906342 11.906342 11.801404 12.745838 12.221153  8.548356 10.017474 13.165586 
    ##      4273      4274      4275      4276      4277      4278      4279      4280 
    ## 12.116216  8.128607  7.079237  8.233544  9.177978 13.375460 10.227349  9.912537 
    ##      4281      4282      4283      4284      4285      4286      4287      4288 
    ## 12.955712 12.116216  9.282915 10.542160  7.603922 10.752034 11.171782 13.060649 
    ##      4289      4290      4291      4292      4293      4294      4295      4296 
    ##  8.863167 10.122412 10.542160 12.116216 11.591530 12.431027  9.387852  9.702663 
    ##      4297      4298      4299      4300      4301      4302      4303      4304 
    ## 10.437223 12.221153 14.319894 10.856971 10.017474  8.968104 10.332286  8.863167 
    ##      4305      4306      4307      4308      4309      4310      4311      4312 
    ##  9.177978 11.591530 11.276719 10.542160  7.603922 11.066845 13.585334 13.795209 
    ##      4313      4314      4315      4316      4317      4318      4319      4320 
    ##  8.968104 12.850775  8.338482  8.338482  7.603922 14.110020 13.375460 13.165586 
    ##      4321      4322      4323      4324      4325      4326      4327      4328 
    ## 12.850775  7.079237  9.597726 12.955712  7.079237  8.548356  7.603922  9.073041 
    ##      4329      4330      4331      4332      4333      4334      4335      4336 
    ##  8.968104 13.480397 13.480397 10.332286 11.696467 14.214957  8.758230  9.177978 
    ##      4337      4338      4339      4340      4341      4342      4343      4344 
    ##  7.918733  7.708859 12.011279 13.270523 12.326090  9.282915 12.745838  7.918733 
    ##      4345      4346      4347      4348      4349      4350      4351      4352 
    ## 14.424831  8.128607 10.332286  9.387852  8.968104 11.906342 12.431027  8.653293 
    ##      4353      4354      4355      4356      4357      4358      4359      4360 
    ## 11.276719  8.968104 13.270523  7.603922  7.708859 10.332286 13.375460 12.326090 
    ##      4361      4362      4363      4364      4365      4366      4367      4368 
    ## 12.431027  8.653293 11.591530  8.128607  8.443419 10.332286 14.214957  9.073041 
    ##      4369      4370      4371      4372      4373      4374      4375      4376 
    ##  8.548356 13.480397 12.850775  8.023670  8.968104  9.492789  9.282915 10.542160 
    ##      4377      4378      4379      4380      4381      4382      4383      4384 
    ##  9.282915  7.708859 11.276719 12.221153 10.856971 12.640901 11.801404  8.863167 
    ##      4385      4386      4387      4388      4389      4390      4391      4392 
    ##  8.443419  7.708859  9.912537  8.338482 11.696467  9.282915 10.542160 12.221153 
    ##      4393      4394      4395      4396      4397      4398      4399      4400 
    ## 12.116216 11.696467  8.443419 11.276719 10.437223 10.752034  7.079237 13.795209 
    ##      4401      4402      4403      4404      4405      4406      4407      4408 
    ## 14.319894 12.745838 11.696467  9.597726  8.758230  8.023670 10.752034  7.813796 
    ##      4409      4410      4411      4412      4413      4414      4415      4416 
    ## 12.535964  7.603922  7.079237  7.079237  8.548356  9.492789 13.165586 10.437223 
    ##      4417      4418      4419      4420      4421      4422      4423      4424 
    ## 11.486593 11.171782  9.387852  8.758230  7.603922 11.906342  9.597726  7.813796 
    ##      4425      4426      4427      4428      4429      4430      4431      4432 
    ## 12.850775  8.758230 11.801404 13.060649  9.807600  7.079237  7.079237  9.492789 
    ##      4433      4434      4435      4436      4437      4438      4439      4440 
    ## 10.542160  9.492789 13.690271 11.906342  8.338482  9.073041 12.955712 12.431027 
    ##      4441      4442      4443      4444      4445      4446      4447      4448 
    ## 14.424831 10.647097 12.011279  9.492789 11.171782 14.214957 12.011279  9.492789 
    ##      4449      4450      4451      4452      4453      4454      4455      4456 
    ## 11.696467 11.906342 13.900146 13.900146 13.900146 13.900146  8.548356 12.116216 
    ##      4457      4458      4459      4460      4461      4462      4463      4464 
    ## 12.640901 13.375460  8.233544 10.437223  8.863167 12.745838  8.653293  7.813796 
    ##      4465      4466      4467      4468      4469      4470      4471      4472 
    ##  9.702663 11.171782  9.702663  9.807600 12.640901  9.073041 10.332286 11.276719 
    ##      4473      4474      4475      4476      4477      4478      4479      4480 
    ##  7.603922  8.968104  7.079237 12.850775 10.017474  9.387852  8.443419  8.548356 
    ##      4481      4482      4483      4484      4485      4486      4487      4488 
    ## 12.221153  9.492789 10.647097  9.282915 12.535964  7.603922 11.696467  8.443419 
    ##      4489      4490      4491      4492      4493      4494      4495      4496 
    ## 12.431027  9.492789 10.961908 10.017474 12.116216 11.801404 11.486593  8.653293 
    ##      4497      4498      4499      4500      4501      4502      4503      4504 
    ##  9.387852  7.079237  9.702663  8.443419  7.079237 11.381656 11.276719 10.647097 
    ##      4505      4506      4507      4508      4509      4510      4511      4512 
    ##  9.702663 13.270523  9.282915 13.060649  8.443419  7.079237  7.079237  9.177978 
    ##      4513      4514      4515      4516      4517      4518      4519      4520 
    ## 12.011279 12.326090  9.912537  7.079237 12.431027 10.017474 11.696467  7.079237 
    ##      4521      4522      4523      4524      4525      4526      4527      4528 
    ## 10.227349  9.492789 10.961908 11.276719 11.066845 13.585334 12.326090 13.900146 
    ##      4529      4530      4531      4532      4533      4534      4535      4536 
    ##  8.128607 12.850775 10.332286  8.758230 14.005083  9.387852  7.708859 12.955712 
    ##      4537      4538      4539      4540      4541      4542      4543      4544 
    ##  9.492789  8.968104 11.276719 11.381656 10.437223 13.165586 10.437223 10.856971 
    ##      4545      4546      4547      4548      4549      4550      4551      4552 
    ## 10.961908  8.548356 11.906342 11.066845  9.177978 11.696467  8.233544 12.850775 
    ##      4553      4554      4555      4556      4557      4558      4559      4560 
    ## 12.011279  8.443419 12.011279 10.332286  8.863167 11.381656  9.702663 12.955712 
    ##      4561      4562      4563      4564      4565      4566      4567      4568 
    ## 14.424831  7.708859 10.332286 10.332286 13.270523  8.548356 13.375460 10.017474 
    ##      4569      4570      4571      4572      4573      4574      4575      4576 
    ## 11.696467 12.011279  7.079237 13.165586  8.863167 11.696467 11.696467 12.640901 
    ##      4577      4578      4579      4580      4581      4582      4583      4584 
    ##  9.282915 13.060649  8.758230  8.863167  8.128607 12.745838 10.856971 10.437223 
    ##      4585      4586      4587      4588      4589      4590      4591      4592 
    ## 12.850775  9.597726  7.603922 11.801404 13.375460  9.387852 11.801404  9.282915 
    ##      4593      4594      4595      4596      4597      4598      4599      4600 
    ##  8.443419  8.338482  9.492789 10.542160  8.443419 12.116216 13.375460 13.060649 
    ##      4601      4602      4603      4604      4605      4606      4607      4608 
    ##  7.079237 12.640901  8.968104  8.443419  8.968104  9.597726  7.708859 10.227349 
    ##      4609      4610      4611      4612      4613      4614      4615      4616 
    ##  9.807600 10.961908  8.548356  9.492789  7.079237  8.023670  8.758230 12.955712 
    ##      4617      4618      4619      4620      4621      4622      4623      4624 
    ##  8.653293  7.603922 11.381656  8.968104  9.177978  8.548356  8.758230  8.548356 
    ##      4625      4626      4627      4628      4629      4630      4631      4632 
    ## 12.431027 10.332286  9.597726 12.221153 11.486593  8.128607 13.270523  9.387852 
    ##      4633      4634      4635      4636      4637      4638      4639      4640 
    ## 12.221153  8.338482  8.443419 12.535964  8.023670 12.326090 12.745838 10.856971 
    ##      4641      4642      4643      4644      4645      4646      4647      4648 
    ## 13.480397  8.443419 12.326090 10.856971  8.968104  9.702663 10.647097 11.906342 
    ##      4649      4650      4651      4652      4653      4654      4655      4656 
    ##  9.282915  8.023670  9.702663 10.542160 11.066845  7.079237 12.116216  8.233544 
    ##      4657      4658      4659      4660      4661      4662      4663      4664 
    ##  8.758230  8.338482 11.486593 11.906342 12.850775  7.603922 10.542160 13.480397 
    ##      4665      4666      4667      4668      4669      4670      4671      4672 
    ##  8.443419 10.961908 10.856971  8.443419 11.696467  9.702663 14.424831 12.745838 
    ##      4673      4674      4675      4676      4677      4678      4679      4680 
    ## 12.535964 10.437223  9.073041 13.480397  9.073041 13.060649 10.017474 11.276719 
    ##      4681      4682      4683      4684      4685      4686      4687      4688 
    ## 10.437223  8.548356  8.233544  9.492789 11.381656  8.443419  8.653293 10.752034 
    ##      4689      4690      4691      4692      4693      4694      4695      4696 
    ##  9.177978 12.221153 11.696467 11.696467  9.702663  9.282915  8.653293  9.807600 
    ##      4697      4698      4699      4700      4701      4702      4703      4704 
    ##  9.387852  8.653293 11.066845 13.060649 11.801404  9.177978  8.863167 11.906342 
    ##      4705      4706      4707      4708      4709      4710      4711      4712 
    ##  9.807600  9.177978  8.443419 10.332286 13.480397 14.110020 12.326090 12.850775 
    ##      4713      4714      4715      4716      4717      4718      4719      4720 
    ## 12.221153 12.850775  9.702663 13.480397 10.122412 10.856971  9.702663  7.708859 
    ##      4721      4722      4723      4724      4725      4726      4727      4728 
    ##  9.387852 12.011279 10.647097 12.221153 10.332286  8.548356  8.863167 13.585334 
    ##      4729      4730      4731      4732      4733      4734      4735      4736 
    ## 11.906342  7.603922 12.535964 12.745838 11.696467 10.332286  7.603922  9.492789 
    ##      4737      4738      4739      4740      4741      4742      4743      4744 
    ## 12.116216 10.647097 10.542160  8.653293  7.079237 12.745838  9.177978 13.480397 
    ##      4745      4746      4747      4748      4749      4750      4751      4752 
    ##  7.918733  8.653293 14.424831  8.128607 10.437223  8.758230  9.177978  8.128607 
    ##      4753      4754      4755      4756      4757      4758      4759      4760 
    ##  8.233544  8.233544 11.591530  9.073041  9.912537 12.745838  9.387852  7.603922 
    ##      4761      4762      4763      4764      4765      4766      4767      4768 
    ## 13.795209 10.017474 14.424831  7.603922 12.640901 13.480397 12.221153  9.702663 
    ##      4769      4770      4771      4772      4773      4774      4775      4776 
    ##  8.233544  9.387852  9.597726 12.850775 11.486593 13.795209 13.795209 10.017474 
    ##      4777      4778      4779      4780      4781      4782      4783      4784 
    ## 10.752034 10.752034 12.535964 13.270523  9.387852 11.906342  7.079237  9.912537 
    ##      4785      4786      4787      4788      4789      4790      4791      4792 
    ## 13.795209  8.653293 12.955712  7.079237  8.758230  7.079237 10.856971  8.863167 
    ##      4793      4794      4795      4796      4797      4798      4799      4800 
    ## 12.535964 12.640901  7.079237 10.437223  9.492789  7.079237  7.813796 14.110020 
    ##      4801      4802      4803      4804      4805      4806      4807      4808 
    ##  7.079237  7.918733 12.011279 10.752034 12.116216 10.122412  9.387852 12.116216 
    ##      4809      4810      4811      4812      4813      4814      4815      4816 
    ##  9.492789  8.653293 13.690271 12.745838  8.548356 12.850775 13.165586  7.079237 
    ##      4817      4818      4819      4820      4821      4822      4823      4824 
    ##  8.653293  9.597726  9.597726  9.912537 12.850775  8.128607  8.653293 11.486593 
    ##      4825      4826      4827      4828      4829      4830      4831      4832 
    ##  8.758230 14.005083 13.480397 10.961908 12.116216  9.597726 11.066845 13.270523 
    ##      4833      4834      4835      4836      4837      4838      4839      4840 
    ##  8.968104  8.968104 14.319894  7.079237 11.696467 11.591530 12.221153 13.060649 
    ##      4841      4842      4843      4844      4845      4846      4847      4848 
    ##  9.282915  8.653293 12.640901 13.375460 11.066845  8.128607 12.326090 10.856971 
    ##      4849      4850      4851      4852      4853      4854      4855      4856 
    ##  7.603922  8.443419  7.708859 11.381656 10.332286 11.801404 11.696467  9.492789 
    ##      4857      4858      4859      4860      4861      4862      4863      4864 
    ## 11.801404  7.918733 10.227349 12.955712 11.381656 12.221153 12.116216  7.813796 
    ##      4865      4866      4867      4868      4869      4870      4871      4872 
    ##  9.492789  8.653293  7.813796  8.233544  9.912537  9.597726 12.640901  8.338482 
    ##      4873      4874      4875      4876      4877      4878      4879      4880 
    ## 11.801404  9.597726  7.079237  8.758230 10.752034 10.856971  9.387852  7.079237 
    ##      4881      4882      4883      4884      4885      4886      4887      4888 
    ## 12.431027  7.813796 14.424831  8.233544  7.603922  8.443419 14.319894  7.708859 
    ##      4889      4890      4891      4892      4893      4894      4895      4896 
    ##  9.177978  7.708859  7.603922 11.696467  8.548356 10.752034  8.968104 10.752034 
    ##      4897      4898      4899      4900      4901      4902      4903      4904 
    ## 12.116216 11.171782  9.177978 10.332286 11.276719 13.690271  8.128607 12.116216 
    ##      4905      4906      4907      4908      4909      4910      4911      4912 
    ## 12.326090 11.906342 12.745838  9.807600 11.276719 11.171782  8.653293 12.431027 
    ##      4913      4914      4915      4916      4917      4918      4919      4920 
    ##  9.912537 14.005083 13.480397 12.011279 12.535964  7.079237  9.073041 10.017474 
    ##      4921      4922      4923      4924      4925      4926      4927      4928 
    ## 12.535964 12.955712 13.375460  8.548356 12.535964 10.227349 12.640901 10.122412 
    ##      4929      4930      4931      4932      4933      4934      4935      4936 
    ## 13.690271  8.653293 12.116216 11.801404 12.116216 11.381656  9.282915 11.486593 
    ##      4937      4938      4939      4940      4941      4942      4943      4944 
    ## 10.227349 10.961908 11.696467 11.486593 12.221153  8.023670  9.912537 13.480397 
    ##      4945      4946      4947      4948      4949      4950      4951      4952 
    ## 11.591530  8.758230 11.381656  9.177978  7.079237 13.480397 13.375460  8.443419 
    ##      4953      4954      4955      4956      4957      4958      4959      4960 
    ##  9.702663 11.276719 10.961908  8.863167  8.863167 12.640901 10.542160  8.758230 
    ##      4961      4962      4963      4964      4965      4966      4967      4968 
    ##  9.912537 12.431027 11.801404 11.066845 11.801404  9.282915 11.381656 10.542160 
    ##      4969      4970      4971      4972      4973      4974      4975      4976 
    ## 12.640901 12.326090  9.807600  9.597726 12.011279 12.326090 12.640901 14.005083 
    ##      4977      4978      4979      4980      4981      4982      4983      4984 
    ## 14.214957 11.486593  9.387852 12.535964 11.066845  8.023670 11.381656  8.758230 
    ##      4985      4986      4987      4988      4989      4990      4991      4992 
    ##  9.073041 12.116216 13.060649 12.640901 10.647097 10.647097 12.326090 10.542160 
    ##      4993      4994      4995      4996      4997      4998      4999      5000 
    ## 14.214957 10.437223  9.912537 11.486593 11.276719 13.690271 12.955712  9.912537 
    ##      5001      5002      5003      5004      5005      5006      5007      5008 
    ## 11.171782 11.906342 12.640901 12.535964 11.801404 12.745838 11.801404 13.375460 
    ##      5009      5010      5011      5012      5013      5014      5015      5016 
    ## 13.165586  8.758230  8.443419 11.906342  9.492789 14.214957  8.548356 10.017474 
    ##      5017      5018      5019      5020      5021      5022      5023      5024 
    ##  9.073041 10.961908 10.017474  9.282915  7.603922 10.017474  7.603922 12.640901 
    ##      5025      5026      5027      5028      5029      5030      5031      5032 
    ##  9.073041 13.795209  9.387852 12.955712  8.128607 11.381656  9.702663 10.856971 
    ##      5033      5034      5035      5036      5037      5038      5039      5040 
    ## 11.486593 13.585334 11.171782 11.801404  7.079237 12.955712  8.338482  8.338482 
    ##      5041      5042      5043      5044      5045      5046      5047      5048 
    ##  7.603922 11.696467 11.696467  9.492789 10.017474 10.856971 11.381656 11.066845 
    ##      5049      5050      5051      5052      5053      5054      5055      5056 
    ## 10.856971  9.492789  8.548356 13.585334  9.073041 10.961908  8.863167 12.431027 
    ##      5057      5058      5059      5060      5061      5062      5063      5064 
    ##  7.603922 10.752034  9.597726  8.443419 10.332286 10.542160 11.066845  9.387852 
    ##      5065      5066      5067      5068      5069      5070      5071      5072 
    ## 14.110020  9.282915 11.066845 12.011279 12.116216 13.270523  7.079237 11.591530 
    ##      5073      5074      5075      5076      5077      5078      5079      5080 
    ##  8.233544 11.066845 14.214957 11.171782  9.912537 11.906342 11.486593  8.443419 
    ##      5081      5082      5083      5084      5085      5086      5087      5088 
    ##  9.073041 12.535964 10.647097 13.375460  9.597726 12.640901 14.110020 12.535964 
    ##      5089      5090      5091      5092      5093      5094      5095      5096 
    ##  7.813796  9.387852  7.813796 12.745838  7.603922 10.437223  8.758230 13.270523 
    ##      5097      5098      5099      5100      5101      5102      5103      5104 
    ## 10.437223  8.968104  8.653293 11.696467 12.326090  7.708859 14.110020  9.807600 
    ##      5105      5106      5107      5108      5109      5110      5111      5112 
    ## 10.437223 10.437223 12.221153  9.282915 10.017474  8.338482  9.282915  8.128607 
    ##      5113      5114      5115      5116      5117      5118      5119      5120 
    ##  7.603922  9.912537 10.542160 13.480397 10.961908  9.597726  8.863167 14.214957 
    ##      5121      5122      5123      5124      5125      5126      5127      5128 
    ## 14.424831 10.437223  8.758230 12.745838  8.968104 10.856971  7.079237  9.702663 
    ##      5129      5130      5131      5132      5133      5134      5135      5136 
    ## 10.227349  8.758230 14.424831 11.381656 13.795209 13.795209 11.696467  7.918733 
    ##      5137      5138      5139      5140      5141      5142      5143      5144 
    ## 12.850775  9.492789  9.177978  7.603922  9.387852 12.116216 10.227349 14.319894 
    ##      5145      5146      5147      5148      5149      5150      5151      5152 
    ##  7.603922 12.116216  8.338482 10.856971  7.603922  8.548356 12.745838 10.961908 
    ##      5153      5154      5155      5156      5157      5158      5159      5160 
    ##  8.653293  7.603922  7.079237  8.863167  9.073041  7.708859 12.431027 10.856971 
    ##      5161      5162      5163      5164      5165      5166      5167      5168 
    ## 14.319894 13.060649  7.603922 11.171782 12.116216 13.060649 10.961908 10.752034 
    ##      5169      5170      5171      5172      5173      5174      5175      5176 
    ##  8.548356  8.023670  8.338482 10.017474 12.221153  8.758230  9.807600  9.597726 
    ##      5177      5178      5179      5180      5181      5182      5183      5184 
    ## 12.535964  7.603922  8.968104 10.122412  8.758230  9.387852 10.856971 13.165586 
    ##      5185      5186      5187      5188      5189      5190      5191      5192 
    ## 14.110020 12.850775  8.863167  8.023670  8.548356 10.332286  9.492789 13.795209 
    ##      5193      5194      5195      5196      5197      5198      5199      5200 
    ## 13.795209 13.795209 11.696467  8.758230  8.023670  7.079237 10.437223  9.073041 
    ##      5201      5202      5203      5204      5205      5206      5207      5208 
    ## 12.745838 13.375460 13.900146  9.807600  8.548356  7.603922 12.326090 13.375460 
    ##      5209      5210      5211      5212      5213      5214      5215      5216 
    ## 13.270523 11.696467 11.906342  7.603922 10.227349 10.227349 12.116216  7.708859 
    ##      5217      5218      5219      5220      5221      5222      5223      5224 
    ## 13.060649  9.387852 10.122412  7.603922  9.387852 10.122412 11.276719 12.221153 
    ##      5225      5226      5227      5228      5229      5230      5231      5232 
    ##  8.758230 10.752034  7.603922 10.961908 11.486593 11.486593 11.381656 13.900146 
    ##      5233      5234      5235      5236      5237      5238      5239      5240 
    ##  9.492789 13.270523  8.653293 13.585334 12.326090 12.011279 11.696467  9.073041 
    ##      5241      5242      5243      5244      5245      5246      5247      5248 
    ## 11.276719 12.850775  9.387852  8.548356 13.270523 11.486593 11.906342 11.696467 
    ##      5249      5250      5251      5252      5253      5254      5255      5256 
    ##  9.282915 10.017474 14.319894 10.437223 11.906342 11.066845  9.073041 13.060649 
    ##      5257      5258      5259      5260      5261      5262      5263      5264 
    ## 11.696467  9.597726  8.023670 11.696467  9.387852 12.850775  9.387852  9.597726 
    ##      5265      5266      5267      5268      5269      5270      5271      5272 
    ## 10.752034 11.171782 13.480397 11.696467 13.165586 12.850775  9.387852  8.863167 
    ##      5273      5274      5275      5276      5277      5278      5279      5280 
    ##  8.128607 12.221153 10.542160 14.110020 11.276719 10.752034  9.073041 13.270523 
    ##      5281      5282      5283      5284      5285      5286      5287      5288 
    ##  7.079237  7.603922  7.918733 12.221153 12.850775  8.863167 10.542160 11.276719 
    ##      5289      5290      5291      5292      5293      5294      5295      5296 
    ##  9.177978 11.591530 11.696467  9.912537 10.332286 12.745838 11.066845 12.116216 
    ##      5297      5298      5299      5300      5301      5302      5303      5304 
    ##  7.079237  8.443419 11.801404 10.542160  8.443419 10.961908 11.171782 10.856971 
    ##      5305      5306      5307      5308      5309      5310      5311      5312 
    ##  7.603922 10.332286 13.165586  7.079237  9.177978  7.079237 14.005083  7.708859 
    ##      5313      5314      5315      5316      5317      5318      5319      5320 
    ##  9.073041 10.647097 11.171782 12.535964 11.801404 10.647097  8.758230 13.795209 
    ##      5321      5322      5323      5324      5325      5326      5327      5328 
    ## 11.486593  8.758230 14.319894 12.535964  8.023670  9.073041  7.603922  7.813796 
    ##      5329      5330      5331      5332      5333      5334      5335      5336 
    ## 13.480397 14.110020 10.542160 12.116216 10.647097  8.653293 13.060649 11.591530 
    ##      5337      5338      5339      5340      5341      5342      5343      5344 
    ##  7.813796  8.548356  8.653293  9.073041 10.961908  9.807600 11.381656 10.227349 
    ##      5345      5346      5347      5348      5349      5350      5351      5352 
    ## 10.017474  9.807600  9.073041  7.603922 12.326090  9.073041 10.961908  8.023670 
    ##      5353      5354      5355      5356      5357      5358      5359      5360 
    ## 12.326090 10.542160  7.603922 12.745838 12.116216 10.227349 12.745838 11.591530 
    ##      5361      5362      5363      5364      5365      5366      5367      5368 
    ##  8.968104 14.110020 10.227349 12.955712  8.338482  9.492789 12.431027  7.603922 
    ##      5369      5370      5371      5372      5373      5374      5375      5376 
    ##  7.079237 11.591530  9.177978 11.066845 11.486593 12.221153 11.276719  9.702663 
    ##      5377      5378      5379      5380      5381      5382      5383      5384 
    ## 10.856971 10.647097  7.918733  8.758230  9.073041  9.807600 12.011279 14.110020 
    ##      5385      5386      5387      5388      5389      5390      5391      5392 
    ## 10.856971  8.968104 12.850775 10.542160 12.326090  7.603922  8.758230 12.116216 
    ##      5393      5394      5395      5396      5397      5398      5399      5400 
    ## 10.332286 12.116216  9.807600 12.011279 11.171782 13.165586  7.079237  8.128607 
    ##      5401      5402      5403      5404      5405      5406      5407      5408 
    ##  7.079237 12.011279 12.955712  9.702663 12.745838 10.961908 13.060649 12.535964 
    ##      5409      5410      5411      5412      5413      5414      5415      5416 
    ## 10.122412 12.640901  7.603922 13.375460  7.079237 10.961908 10.227349 11.171782 
    ##      5417      5418      5419      5420      5421      5422      5423      5424 
    ## 11.906342 14.319894 11.276719 13.480397 11.486593 11.486593 11.591530 12.640901 
    ##      5425      5426      5427      5428      5429      5430      5431      5432 
    ##  8.128607 12.850775 13.375460  9.387852 11.906342 11.801404  9.702663 10.437223 
    ##      5433      5434      5435      5436      5437      5438      5439      5440 
    ##  7.813796 12.221153  9.177978  7.813796  7.079237 11.906342  7.079237  8.863167 
    ##      5441      5442      5443      5444      5445      5446      5447      5448 
    ## 11.906342  9.807600  7.079237  8.968104 13.900146  7.918733 11.171782 12.011279 
    ##      5449      5450      5451      5452      5453      5454      5455      5456 
    ## 10.856971 13.795209  9.597726  9.807600 10.122412 12.326090 12.326090 12.955712 
    ##      5457      5458      5459      5460      5461      5462      5463      5464 
    ## 13.060649  7.603922 12.431027 11.381656 10.017474 12.326090 12.431027  8.443419 
    ##      5465      5466      5467      5468      5469      5470      5471      5472 
    ## 12.431027 10.647097 10.752034  9.912537 13.060649  8.548356 10.227349  9.912537 
    ##      5473      5474      5475      5476      5477      5478      5479      5480 
    ##  9.597726 11.906342  7.918733 12.850775 10.227349 10.542160 10.227349 10.961908 
    ##      5481      5482      5483      5484      5485      5486      5487      5488 
    ## 13.795209 10.961908 11.276719 13.060649  8.968104 10.227349 10.961908  7.603922 
    ##      5489      5490      5491      5492      5493      5494      5495      5496 
    ## 12.955712  8.863167  8.653293  7.603922  9.597726 13.480397 13.900146  8.863167 
    ##      5497      5498      5499      5500      5501      5502      5503      5504 
    ##  8.233544 12.955712 11.801404 12.535964 10.122412  8.233544 12.535964 12.221153 
    ##      5505      5506      5507      5508      5509      5510      5511      5512 
    ## 14.319894 13.375460 12.850775 13.270523  8.548356  8.653293 12.640901 10.227349 
    ##      5513      5514      5515      5516      5517      5518      5519      5520 
    ##  9.807600  7.603922 10.017474 12.850775 14.424831  8.548356  8.443419  7.603922 
    ##      5521      5522      5523      5524      5525      5526      5527      5528 
    ##  7.603922 11.906342 13.165586 12.431027  8.443419  9.492789  9.702663  7.603922 
    ##      5529      5530      5531      5532      5533      5534      5535      5536 
    ## 12.431027 10.017474 11.381656 12.745838  9.492789 12.221153 11.486593  8.128607 
    ##      5537      5538      5539      5540      5541      5542      5543      5544 
    ##  8.548356 13.375460  9.807600 12.116216  8.968104 11.066845 10.961908 11.171782 
    ##      5545      5546      5547      5548      5549      5550      5551      5552 
    ## 12.955712 11.171782 14.110020 11.171782  9.492789 14.319894  8.968104 13.270523 
    ##      5553      5554      5555      5556      5557      5558      5559      5560 
    ## 12.431027 11.906342  8.023670 13.690271 10.332286 12.955712 10.017474 12.011279 
    ##      5561      5562      5563      5564      5565      5566      5567      5568 
    ##  7.603922 14.319894  7.708859 11.381656 12.011279  9.073041 13.900146  9.073041 
    ##      5569      5570      5571      5572      5573      5574      5575      5576 
    ##  9.702663 10.647097  9.702663 10.017474  7.603922  9.177978  7.918733  9.702663 
    ##      5577      5578      5579      5580      5581      5582      5583      5584 
    ## 11.486593  7.918733 12.011279  9.912537  9.073041 10.122412  9.177978  9.282915 
    ##      5585      5586      5587      5588      5589      5590      5591      5592 
    ## 11.381656  8.233544 10.332286 12.850775  9.702663  9.807600  8.128607  8.023670 
    ##      5593      5594      5595      5596      5597      5598      5599      5600 
    ## 10.647097 14.110020  9.702663  9.597726  9.073041 10.961908  8.758230 10.542160 
    ##      5601      5602      5603      5604      5605      5606      5607      5608 
    ## 11.171782 12.640901  7.603922 13.060649 12.011279  9.177978  7.603922  8.758230 
    ##      5609      5610      5611      5612      5613      5614      5615      5616 
    ##  9.492789 11.591530 13.165586  8.443419  7.079237  7.603922 13.060649 12.221153 
    ##      5617      5618      5619      5620      5621      5622      5623      5624 
    ##  9.702663  9.387852  8.443419 13.375460 12.011279 11.591530 11.276719 11.696467 
    ##      5625      5626      5627      5628      5629      5630      5631      5632 
    ##  7.079237 12.745838 10.856971 14.319894  9.073041  7.603922 12.116216 12.745838 
    ##      5633      5634      5635      5636      5637      5638      5639      5640 
    ## 11.171782  9.807600 11.486593 11.276719 11.801404 11.801404  7.603922 11.276719 
    ##      5641      5642      5643      5644      5645      5646      5647      5648 
    ## 11.696467  9.177978  8.338482  9.807600 11.171782 12.431027 11.381656  7.813796 
    ##      5649      5650      5651      5652      5653      5654      5655      5656 
    ##  9.073041 10.017474  8.653293 10.647097  9.073041 10.752034 12.221153 11.591530 
    ##      5657      5658      5659      5660      5661      5662      5663      5664 
    ## 13.480397 11.801404  9.807600 11.381656 10.332286  9.492789  8.548356 11.066845 
    ##      5665      5666      5667      5668      5669      5670      5671      5672 
    ## 11.171782  9.073041 11.066845  8.443419  9.597726  7.603922 12.535964  7.603922 
    ##      5673      5674      5675      5676      5677      5678      5679      5680 
    ##  9.597726 11.486593 12.745838 11.906342 12.326090 13.060649 11.171782 10.752034 
    ##      5681      5682      5683      5684      5685      5686      5687      5688 
    ##  9.702663 11.171782 12.640901 10.227349 13.165586 10.752034 12.326090  9.912537 
    ##      5689      5690      5691      5692      5693      5694      5695      5696 
    ## 10.542160 12.850775 13.480397 10.017474  9.597726  9.282915  7.813796 11.381656 
    ##      5697      5698      5699      5700      5701      5702      5703      5704 
    ## 11.276719 12.640901 11.906342 12.850775 11.591530 12.221153 13.900146 12.745838 
    ##      5705      5706      5707      5708      5709      5710      5711      5712 
    ## 14.319894  9.387852  9.387852 11.801404 11.276719  9.282915 10.752034 13.795209 
    ##      5713      5714      5715      5716      5717      5718      5719      5720 
    ## 10.437223 14.005083 13.270523 11.801404 11.906342 11.486593  7.079237 11.696467 
    ##      5721      5722      5723      5724      5725      5726      5727      5728 
    ## 11.171782 11.381656  9.912537 12.535964  7.079237 13.375460 10.122412 10.752034 
    ##      5729      5730      5731      5732      5733      5734      5735      5736 
    ## 10.332286 10.961908 12.745838  9.177978  9.912537 12.116216  8.863167 14.214957 
    ##      5737      5738      5739      5740      5741      5742      5743      5744 
    ## 10.961908 10.752034 13.375460 10.122412 14.319894 10.437223  8.548356  8.758230 
    ##      5745      5746      5747      5748      5749      5750      5751      5752 
    ## 10.961908 11.171782 11.066845 13.060649 12.326090  9.177978 11.696467  8.863167 
    ##      5753      5754      5755      5756      5757      5758      5759      5760 
    ## 11.276719  9.912537 12.011279 13.480397 13.060649  9.073041 13.585334 11.801404 
    ##      5761      5762      5763      5764      5765      5766      5767      5768 
    ## 10.227349  8.548356 13.165586 13.060649 12.640901  9.597726 12.535964 10.961908 
    ##      5769      5770      5771      5772      5773      5774      5775      5776 
    ## 13.165586 10.647097 13.270523 10.437223  8.443419 12.116216 12.640901 12.116216 
    ##      5777      5778      5779      5780      5781      5782      5783      5784 
    ##  9.702663 10.437223 10.961908 11.696467  7.603922 11.066845 12.326090 12.011279 
    ##      5785      5786      5787      5788      5789      5790      5791      5792 
    ## 12.535964 12.011279 12.955712 12.535964 10.227349 11.066845  9.807600 11.486593 
    ##      5793      5794      5795      5796      5797      5798      5799      5800 
    ## 12.955712 12.431027  8.758230 11.696467 12.850775 12.431027 10.542160 11.696467 
    ##      5801      5802      5803      5804      5805      5806      5807      5808 
    ## 12.640901  7.813796  7.079237 12.535964 11.486593 10.542160 11.591530 11.171782 
    ##      5809      5810      5811      5812      5813      5814      5815      5816 
    ##  9.702663 11.276719 11.696467 11.906342  9.073041  8.338482  9.073041  8.548356 
    ##      5817      5818      5819      5820      5821      5822      5823      5824 
    ## 13.375460 13.060649 11.486593 12.221153 14.110020 11.801404 11.801404 12.640901 
    ##      5825      5826      5827      5828      5829      5830      5831      5832 
    ## 14.319894  7.079237  9.807600  8.863167 14.424831 11.591530  8.863167 10.752034 
    ##      5833      5834      5835      5836      5837      5838      5839      5840 
    ## 12.535964  9.387852 11.696467 13.900146  9.387852 12.221153 10.017474  9.597726 
    ##      5841      5842      5843      5844      5845      5846      5847      5848 
    ## 11.486593 12.955712 12.221153 13.480397 11.696467 10.542160 10.647097  9.282915 
    ##      5849      5850      5851      5852      5853      5854      5855      5856 
    ## 10.647097 10.227349  7.918733  7.079237 11.906342 11.696467 13.270523  8.023670 
    ##      5857      5858      5859      5860      5861      5862      5863      5864 
    ##  7.813796 10.856971 12.011279  9.073041 11.801404 13.270523 12.640901 11.066845 
    ##      5865      5866      5867      5868      5869      5870      5871      5872 
    ##  9.912537 11.171782 14.110020 10.017474 11.486593 10.856971 12.955712  9.492789 
    ##      5873      5874      5875      5876      5877      5878      5879      5880 
    ##  9.492789  8.548356  9.492789  7.079237  8.443419 10.332286 10.856971 12.535964 
    ##      5881      5882      5883      5884      5885      5886      5887      5888 
    ##  8.863167 12.745838  9.282915 11.486593 10.437223  9.702663 13.795209 12.640901 
    ##      5889      5890      5891      5892      5893      5894      5895      5896 
    ## 11.486593 11.906342  8.443419  8.653293 11.276719 10.961908 12.850775 14.424831 
    ##      5897      5898      5899      5900      5901      5902      5903      5904 
    ##  9.912537 13.270523  9.073041 13.900146  9.177978 12.535964 11.801404  8.338482 
    ##      5905      5906      5907      5908      5909      5910      5911      5912 
    ##  7.708859  7.079237 10.332286 10.017474  8.863167 10.437223 14.110020  9.597726 
    ##      5913      5914      5915      5916      5917      5918      5919      5920 
    ## 12.011279  7.603922 13.375460  8.443419 10.227349 12.850775 14.424831 11.591530 
    ##      5921      5922      5923      5924      5925      5926      5927      5928 
    ##  8.023670  7.603922 13.690271  9.702663  9.177978  8.653293 13.270523 11.801404 
    ##      5929      5930      5931      5932      5933      5934      5935      5936 
    ## 11.591530 10.017474 12.850775 10.961908 10.437223 11.276719 10.332286 12.116216 
    ##      5937      5938      5939      5940      5941      5942      5943      5944 
    ##  7.813796  9.702663 12.116216  8.548356  9.597726  8.653293  9.282915 10.542160 
    ##      5945      5946      5947      5948      5949      5950      5951      5952 
    ##  8.653293  8.443419 11.801404 11.801404 12.221153 11.276719  8.233544  9.387852 
    ##      5953      5954      5955      5956      5957      5958      5959      5960 
    ## 11.486593 12.850775 10.752034 10.017474  9.282915  7.918733  9.702663  8.653293 
    ##      5961      5962      5963      5964      5965      5966      5967      5968 
    ##  9.597726  7.603922  9.807600  8.968104 11.171782  9.387852 11.171782  9.177978 
    ##      5969      5970      5971      5972      5973      5974      5975      5976 
    ## 11.381656 13.270523 10.437223 11.591530  8.968104 11.801404 12.535964  8.863167 
    ##      5977      5978      5979      5980      5981      5982      5983      5984 
    ## 14.319894 13.270523  8.023670 12.011279 10.017474 12.431027 13.270523 13.480397 
    ##      5985      5986      5987      5988      5989      5990      5991      5992 
    ##  9.597726 13.900146  9.282915  9.282915 10.332286  9.282915 11.906342 11.696467 
    ##      5993      5994      5995      5996      5997      5998      5999      6000 
    ## 10.437223 14.424831  8.338482 12.745838  9.492789 14.214957  9.807600 13.690271 
    ##      6001      6002      6003      6004      6005      6006      6007      6008 
    ##  9.702663  8.653293  8.863167 14.110020 12.431027  9.912537 10.017474  7.813796 
    ##      6009      6010      6011      6012      6013      6014      6015      6016 
    ## 10.437223 14.110020 12.535964 11.276719 13.165586 12.011279 12.326090  7.079237 
    ##      6017      6018      6019      6020      6021      6022      6023      6024 
    ## 10.332286  7.918733  8.758230 14.424831  7.603922  9.387852  7.708859 14.319894 
    ##      6025      6026      6027      6028      6029      6030      6031      6032 
    ## 10.017474  8.023670 13.165586  9.387852  9.387852 10.017474 10.332286  9.597726 
    ##      6033      6034      6035      6036      6037      6038      6039      6040 
    ## 12.221153  7.708859  7.603922 12.535964 13.690271  9.912537  7.079237  9.597726 
    ##      6041      6042      6043      6044      6045      6046      6047      6048 
    ##  8.653293  8.233544 10.961908 12.326090 14.214957 10.227349  7.708859  9.702663 
    ##      6049      6050      6051      6052      6053      6054      6055      6056 
    ##  7.603922 12.640901 11.906342 14.005083  9.282915 14.214957 10.961908 12.221153 
    ##      6057      6058      6059      6060      6061      6062      6063      6064 
    ##  8.443419  9.387852 10.752034 12.640901 10.961908  8.023670 10.752034 14.424831 
    ##      6065      6066      6067      6068      6069      6070      6071      6072 
    ## 11.381656  7.918733 11.801404 13.375460 12.431027 12.011279 12.116216 11.066845 
    ##      6073      6074      6075      6076      6077      6078      6079      6080 
    ## 12.640901 12.326090 13.165586  9.387852  8.968104 12.745838 12.431027  9.177978 
    ##      6081      6082      6083      6084      6085      6086      6087      6088 
    ## 12.535964 13.585334 10.122412  8.968104 10.332286 14.005083  8.653293 12.011279 
    ##      6089      6090      6091      6092      6093      6094      6095      6096 
    ##  9.702663  8.968104 10.856971  8.023670 13.060649 10.856971  8.968104  7.603922 
    ##      6097      6098      6099      6100      6101      6102      6103      6104 
    ## 11.066845  8.128607 11.801404  9.492789 11.591530 11.066845  8.233544 14.214957 
    ##      6105      6106      6107      6108      6109      6110      6111      6112 
    ## 13.060649 12.955712  8.968104 14.214957 10.961908  9.702663  9.073041  9.492789 
    ##      6113      6114      6115      6116      6117      6118      6119      6120 
    ## 11.171782  9.597726 11.381656  9.177978 11.066845  9.492789 13.480397 11.381656 
    ##      6121      6122      6123      6124      6125      6126      6127      6128 
    ## 12.326090  7.603922  8.233544 13.270523 10.961908  9.597726  9.073041 12.221153 
    ##      6129      6130      6131      6132      6133      6134      6135      6136 
    ##  9.492789 10.122412 11.801404 11.171782 10.332286  9.282915  9.387852 13.165586 
    ##      6137      6138      6139      6140      6141      6142      6143      6144 
    ## 10.227349  8.233544 14.214957 10.647097  9.387852  7.603922 10.227349 12.955712 
    ##      6145      6146      6147      6148      6149      6150      6151      6152 
    ## 11.276719  7.603922  8.338482 12.431027 11.276719 13.375460 14.214957  8.863167 
    ##      6153      6154      6155      6156      6157      6158      6159      6160 
    ## 13.270523  7.603922  9.807600  9.702663 10.647097  9.387852  9.807600 11.171782 
    ##      6161      6162      6163      6164      6165      6166      6167      6168 
    ##  9.702663 11.591530 12.011279 10.122412  7.708859 11.486593  8.128607 12.011279 
    ##      6169      6170      6171      6172      6173      6174      6175      6176 
    ##  9.177978  9.597726 12.011279 10.017474  7.603922 11.801404 11.486593  7.708859 
    ##      6177      6178      6179      6180      6181      6182      6183      6184 
    ## 11.696467 11.276719  8.863167 12.535964  9.177978 13.165586  8.968104 11.591530 
    ##      6185      6186      6187      6188      6189      6190      6191      6192 
    ## 11.066845  8.653293  7.079237 11.801404 12.431027  7.603922  8.548356 10.752034 
    ##      6193      6194      6195      6196      6197      6198      6199      6200 
    ## 10.437223 12.640901  9.177978 10.332286 10.332286  8.233544 10.961908  7.918733 
    ##      6201      6202      6203      6204      6205      6206      6207      6208 
    ##  7.603922 10.227349 12.850775  7.708859 12.955712 10.122412 12.535964  7.603922 
    ##      6209      6210      6211      6212      6213      6214      6215      6216 
    ##  7.079237  9.177978  8.968104 10.752034 12.116216  8.548356 10.542160  8.758230 
    ##      6217      6218      6219      6220      6221      6222      6223      6224 
    ##  7.079237 12.640901 12.640901 11.171782 11.591530  9.282915  7.813796 10.752034 
    ##      6225      6226      6227      6228      6229      6230      6231      6232 
    ## 13.795209 11.381656 12.326090  8.443419 12.431027  8.653293 11.276719 10.227349 
    ##      6233      6234      6235      6236      6237      6238      6239      6240 
    ## 13.165586  8.548356 11.066845 10.542160 13.060649 13.375460  8.758230 13.900146 
    ##      6241      6242      6243      6244      6245      6246      6247      6248 
    ## 12.535964  8.968104  8.128607  7.813796  8.233544  9.807600  8.863167 11.171782 
    ##      6249      6250      6251      6252      6253      6254      6255      6256 
    ##  8.758230 13.375460 10.542160  8.968104  8.758230 12.431027 10.647097 13.900146 
    ##      6257      6258      6259      6260      6261      6262      6263      6264 
    ##  9.807600  8.758230 11.801404  8.338482 13.480397 13.480397 10.752034 12.850775 
    ##      6265      6266      6267      6268      6269      6270      6271      6272 
    ## 10.647097 11.486593 13.165586  8.338482 10.856971  9.177978  8.548356 11.381656 
    ##      6273      6274      6275      6276      6277      6278      6279      6280 
    ##  8.758230 14.424831 13.270523  7.813796 12.431027  7.708859 12.431027  8.863167 
    ##      6281      6282      6283      6284      6285      6286      6287      6288 
    ##  8.443419 13.690271 13.690271 12.326090  7.603922  9.702663 14.319894  9.387852 
    ##      6289      6290      6291      6292      6293      6294      6295      6296 
    ##  7.603922  9.073041  8.338482 11.906342 12.955712  9.807600 10.017474 14.214957 
    ##      6297      6298      6299      6300      6301      6302      6303      6304 
    ## 11.066845 10.647097 12.221153 12.326090 12.011279 13.585334 10.856971 11.801404 
    ##      6305      6306      6307      6308      6309      6310      6311      6312 
    ##  9.492789 12.535964  7.708859 10.122412 12.955712 12.745838 10.227349 12.326090 
    ##      6313      6314      6315      6316      6317      6318      6319      6320 
    ## 10.752034  7.918733  8.128607 11.381656 13.165586  7.918733 12.221153 10.856971 
    ##      6321      6322      6323      6324      6325      6326      6327      6328 
    ## 10.752034  8.863167  9.492789  9.073041  9.597726  9.282915 12.955712 11.486593 
    ##      6329      6330      6331      6332      6333      6334      6335      6336 
    ##  8.968104 13.165586 11.381656  9.387852  9.492789  8.338482 10.647097 13.270523 
    ##      6337      6338      6339      6340      6341      6342      6343      6344 
    ##  7.079237  9.597726  8.968104 10.856971  7.079237 13.060649  8.653293  9.282915 
    ##      6345      6346      6347      6348      6349      6350      6351      6352 
    ## 11.591530  9.387852 10.856971  8.653293 11.906342 10.647097  8.023670 11.801404 
    ##      6353      6354      6355      6356      6357      6358      6359      6360 
    ## 11.801404  8.548356 10.332286 13.480397 12.745838  9.282915 12.431027 13.900146 
    ##      6361      6362      6363      6364      6365      6366      6367      6368 
    ## 12.745838  8.443419  8.443419  8.128607  7.079237 12.116216  8.653293  8.863167 
    ##      6369      6370      6371      6372      6373      6374      6375      6376 
    ## 12.011279 11.696467  8.548356 14.319894 10.332286  8.443419 10.647097  9.702663 
    ##      6377      6378      6379      6380      6381      6382      6383      6384 
    ##  9.807600 10.017474  8.443419 12.431027  9.177978 11.486593 14.319894 11.801404 
    ##      6385      6386      6387      6388      6389      6390      6391      6392 
    ##  7.918733  7.079237  7.708859  9.492789  7.603922 11.591530  7.918733 12.431027 
    ##      6393      6394      6395      6396      6397      6398      6399      6400 
    ## 10.856971 14.319894 14.319894  9.702663 10.542160  9.387852  8.653293 10.856971 
    ##      6401      6402      6403      6404      6405      6406      6407      6408 
    ## 12.955712  8.338482  8.233544  9.912537  9.912537 13.375460  9.702663  8.023670 
    ##      6409      6410      6411      6412      6413      6414      6415      6416 
    ## 12.535964  9.492789 12.326090 12.640901 13.690271  8.653293  7.918733  9.073041 
    ##      6417      6418      6419      6420      6421      6422      6423      6424 
    ##  9.597726 11.696467  9.702663  7.079237  9.702663 10.332286  7.079237 12.640901 
    ##      6425      6426      6427      6428      6429      6430      6431      6432 
    ## 10.752034 14.214957  9.073041  7.079237 14.005083  8.653293 10.332286  8.548356 
    ##      6433      6434      6435      6436      6437      6438      6439      6440 
    ##  8.548356  9.177978 12.955712 13.165586  8.023670 13.900146 10.017474 12.011279 
    ##      6441      6442      6443      6444      6445      6446      6447      6448 
    ## 12.011279 11.906342  9.073041 11.801404 10.647097  9.912537  9.492789  8.863167 
    ##      6449      6450      6451      6452      6453      6454      6455      6456 
    ## 12.011279 14.110020 12.431027 11.486593  9.702663 11.066845 10.122412  7.603922 
    ##      6457      6458      6459      6460      6461      6462      6463      6464 
    ##  8.548356 10.437223 11.381656 12.011279  8.023670 13.480397  9.387852 12.745838 
    ##      6465      6466      6467      6468      6469      6470      6471      6472 
    ## 10.542160 13.900146 10.437223 13.270523 10.332286 12.221153  9.597726  9.912537 
    ##      6473      6474      6475      6476      6477      6478      6479      6480 
    ## 12.431027 12.326090 11.381656  7.079237 11.066845 12.535964 10.332286 12.431027 
    ##      6481      6482      6483      6484      6485      6486      6487      6488 
    ## 13.270523 11.486593 10.542160  8.863167  7.708859  8.968104  9.073041  7.603922 
    ##      6489      6490      6491      6492      6493      6494      6495      6496 
    ##  9.387852  9.282915  7.708859  8.128607  9.912537 13.585334 12.326090 10.647097 
    ##      6497      6498      6499      6500      6501      6502      6503      6504 
    ## 10.542160 10.227349  7.603922 13.270523 12.850775  9.807600 12.221153 13.375460 
    ##      6505      6506      6507      6508      6509      6510      6511      6512 
    ## 13.375460 11.591530  8.653293 12.850775  9.492789 11.906342 13.480397  9.387852 
    ##      6513      6514      6515      6516      6517      6518      6519      6520 
    ## 10.122412  8.233544 13.690271  8.758230 12.326090 10.122412 10.856971  9.807600 
    ##      6521      6522      6523      6524      6525      6526      6527      6528 
    ## 11.801404 12.326090 11.906342  9.492789 12.850775  8.023670 12.221153  8.653293 
    ##      6529      6530      6531      6532      6533      6534      6535      6536 
    ## 11.696467 13.480397  7.079237 11.276719 10.647097 10.332286  8.548356  8.338482 
    ##      6537      6538      6539      6540      6541      6542      6543      6544 
    ## 12.535964  7.918733 10.542160 12.850775  8.548356 10.332286  7.079237 13.060649 
    ##      6545      6546      6547      6548      6549      6550      6551      6552 
    ##  8.758230  8.443419  9.387852 12.221153 10.017474 12.116216 12.535964 11.066845 
    ##      6553      6554      6555      6556      6557      6558      6559      6560 
    ## 14.424831  9.912537 10.227349  9.073041  8.128607 11.696467  8.863167  7.603922 
    ##      6561      6562      6563      6564      6565      6566      6567      6568 
    ##  7.603922  8.233544 14.214957  9.492789 10.647097  9.073041  9.702663 11.801404 
    ##      6569      6570      6571      6572      6573      6574      6575      6576 
    ##  7.603922 12.431027  8.023670 12.745838  9.492789 14.214957 12.535964 12.011279 
    ##      6577      6578      6579      6580      6581      6582      6583      6584 
    ## 11.801404  9.073041 13.060649 11.381656  7.079237 11.696467 10.437223 10.752034 
    ##      6585      6586      6587      6588      6589      6590      6591      6592 
    ##  8.338482  7.079237 13.585334 10.752034 13.375460 11.801404  7.813796 11.066845 
    ##      6593      6594      6595      6596      6597      6598      6599      6600 
    ##  7.603922  8.443419 10.122412 10.332286  8.338482 12.640901 12.431027  8.548356 
    ##      6601      6602      6603      6604      6605      6606      6607      6608 
    ## 12.221153  8.968104 12.640901  9.177978 12.431027 12.221153 10.332286 11.171782 
    ##      6609      6610      6611      6612      6613      6614      6615      6616 
    ## 11.171782  8.968104 14.214957  8.128607 12.640901 11.066845 10.122412 12.326090 
    ##      6617      6618      6619      6620      6621      6622      6623      6624 
    ##  8.653293 10.122412 10.437223  8.863167 11.801404 10.227349  9.912537 13.690271 
    ##      6625      6626      6627      6628      6629      6630      6631      6632 
    ##  8.758230 10.227349  8.863167  8.128607 13.060649 13.060649 12.535964 11.486593 
    ##      6633      6634      6635      6636      6637      6638      6639      6640 
    ## 10.961908  9.597726 11.801404  8.968104 12.326090 11.591530 10.017474 11.381656 
    ##      6641      6642      6643      6644      6645      6646      6647      6648 
    ##  7.079237 10.961908  9.912537  8.023670 13.585334  7.079237 10.961908 12.745838 
    ##      6649      6650      6651      6652      6653      6654      6655      6656 
    ## 13.165586 11.171782  7.603922 11.591530 12.955712 13.690271 11.906342 12.011279 
    ##      6657      6658      6659      6660      6661      6662      6663      6664 
    ##  9.177978  8.338482  7.603922  8.128607  7.708859 10.752034 11.171782 13.270523 
    ##      6665      6666      6667      6668      6669      6670      6671      6672 
    ## 10.542160 11.276719  7.603922  9.702663  9.282915 10.856971 13.270523 12.116216 
    ##      6673      6674      6675      6676      6677      6678      6679      6680 
    ## 12.116216  8.023670  8.443419 10.437223  9.597726 11.381656  8.548356 10.856971 
    ##      6681      6682      6683      6684      6685      6686      6687      6688 
    ##  8.338482  9.912537 12.431027  7.603922 13.585334 10.856971  9.912537  8.443419 
    ##      6689      6690      6691      6692      6693      6694      6695      6696 
    ## 11.486593 11.486593 10.961908  9.177978  9.387852 12.011279 13.060649  8.233544 
    ##      6697      6698      6699      6700      6701      6702      6703      6704 
    ##  9.177978 10.961908 12.326090  7.603922  9.073041 12.116216 13.480397 12.955712 
    ##      6705      6706      6707      6708      6709      6710      6711      6712 
    ##  9.492789 10.647097  9.597726 11.801404  9.597726 13.270523 12.116216  8.443419 
    ##      6713      6714      6715      6716      6717      6718      6719      6720 
    ##  8.863167 12.535964 11.486593 12.116216 10.437223  9.387852 10.227349  9.597726 
    ##      6721      6722      6723      6724      6725      6726      6727      6728 
    ## 11.381656  7.079237 14.110020 11.486593 12.116216  9.912537 12.850775 10.542160 
    ##      6729      6730      6731      6732      6733      6734      6735      6736 
    ## 12.326090 10.961908 12.221153 14.319894  9.073041 10.332286  9.073041  9.073041 
    ##      6737      6738      6739      6740      6741      6742      6743      6744 
    ## 10.437223  9.073041 11.906342 10.227349 13.585334  8.653293 12.116216 12.431027 
    ##      6745      6746      6747      6748      6749      6750      6751      6752 
    ## 10.437223 10.227349 12.535964 11.276719  7.603922  9.702663  9.807600  9.702663 
    ##      6753      6754      6755      6756      6757      6758      6759      6760 
    ## 12.011279 14.110020  9.912537  7.603922  9.387852 12.745838 10.961908 13.480397 
    ##      6761      6762      6763      6764      6765      6766      6767      6768 
    ## 12.850775  8.758230 13.900146 12.535964 13.270523 13.165586  8.758230 11.906342 
    ##      6769      6770      6771      6772      6773      6774      6775      6776 
    ## 11.381656 12.640901 11.066845  9.807600  8.758230 11.066845 11.591530  7.079237 
    ##      6777      6778      6779      6780      6781      6782      6783      6784 
    ##  7.603922 13.060649  9.597726  9.073041  8.968104  8.443419 13.585334  8.443419 
    ##      6785      6786      6787      6788      6789      6790      6791      6792 
    ## 12.011279  9.702663 11.801404 10.647097  7.079237 11.381656 14.214957 11.486593 
    ##      6793      6794      6795      6796      6797      6798      6799      6800 
    ## 12.011279 11.066845  8.548356 12.221153 11.486593  8.023670 10.961908 11.906342 
    ##      6801      6802      6803      6804      6805      6806      6807      6808 
    ##  8.863167  9.912537  8.548356  9.807600  9.073041 12.326090  9.282915  8.863167 
    ##      6809      6810      6811      6812      6813      6814      6815      6816 
    ## 12.640901  7.603922  7.603922 13.480397  8.758230  7.603922 10.332286  8.653293 
    ##      6817      6818      6819      6820      6821      6822      6823      6824 
    ##  9.282915  9.387852  7.079237 11.381656 12.431027  7.813796  8.968104  8.443419 
    ##      6825      6826      6827      6828      6829      6830      6831      6832 
    ## 12.011279 14.319894  8.338482  8.653293 10.227349  8.128607 10.227349  7.603922 
    ##      6833      6834      6835      6836      6837      6838      6839      6840 
    ## 10.122412  8.443419  9.282915 12.326090 12.955712  8.863167  7.079237 11.171782 
    ##      6841      6842      6843      6844      6845      6846      6847      6848 
    ##  9.177978 13.690271 14.319894 11.066845  9.177978 10.227349  8.968104  8.233544 
    ##      6849      6850      6851      6852      6853      6854      6855      6856 
    ##  9.073041  8.863167  7.603922  9.702663  7.813796  8.233544 14.214957  8.023670 
    ##      6857      6858      6859      6860      6861      6862      6863      6864 
    ## 10.961908 10.752034 13.690271  8.233544 12.640901 10.332286 12.116216  9.073041 
    ##      6865      6866      6867      6868      6869      6870      6871      6872 
    ##  7.813796 10.017474  7.079237 12.431027  9.597726  9.492789 12.011279  8.548356 
    ##      6873      6874      6875      6876      6877      6878      6879      6880 
    ##  7.813796  7.603922  9.597726 12.850775  7.603922 12.850775 11.801404  8.233544 
    ##      6881      6882      6883      6884      6885      6886      6887      6888 
    ## 10.961908 12.431027 11.696467  8.233544  9.807600 10.647097 13.060649  7.079237 
    ##      6889      6890      6891      6892      6893      6894      6895      6896 
    ## 12.640901  9.282915  9.492789  7.603922 10.752034 12.431027 12.850775  9.807600 
    ##      6897      6898      6899      6900      6901      6902      6903      6904 
    ##  9.597726  9.597726  8.023670  8.758230 12.326090  9.912537  8.443419 11.486593 
    ##      6905      6906      6907      6908      6909      6910      6911      6912 
    ## 10.961908 11.276719  8.968104 10.437223  7.918733 11.066845 11.171782 13.585334 
    ##      6913      6914      6915      6916      6917      6918      6919      6920 
    ##  7.603922 12.221153 11.486593 12.745838 12.326090 13.795209 13.795209 13.795209 
    ##      6921      6922      6923      6924      6925      6926      6927      6928 
    ## 13.795209  9.387852 12.850775  8.863167  7.079237 11.906342  7.603922 12.955712 
    ##      6929      6930      6931      6932      6933      6934      6935      6936 
    ##  8.863167 11.486593 12.116216  8.653293 12.535964  9.177978 11.591530 10.437223 
    ##      6937      6938      6939      6940      6941      6942      6943      6944 
    ##  9.597726 10.437223  9.807600 10.961908 12.850775 11.801404 10.017474 12.011279 
    ##      6945      6946      6947      6948      6949      6950      6951      6952 
    ## 13.480397 11.591530  7.918733  8.338482  7.603922  8.968104 10.542160 11.276719 
    ##      6953      6954      6955      6956      6957      6958      6959      6960 
    ##  8.443419  9.492789 10.647097 12.850775 11.906342  8.233544  7.918733  9.073041 
    ##      6961      6962      6963      6964      6965      6966      6967      6968 
    ##  7.603922 10.961908 13.480397  8.338482  8.653293 12.640901  8.128607  7.079237 
    ##      6969      6970      6971      6972      6973      6974      6975      6976 
    ##  9.912537 10.332286 10.017474 12.326090 10.647097  8.863167 12.221153  8.338482 
    ##      6977      6978      6979      6980      6981      6982      6983      6984 
    ## 12.116216  9.807600  7.603922  9.282915 11.591530  9.387852  9.597726 10.542160 
    ##      6985      6986      6987      6988      6989      6990      6991      6992 
    ## 12.850775 10.332286 10.542160  9.912537  8.443419  9.912537 10.961908 10.227349 
    ##      6993      6994      6995      6996      6997      6998      6999      7000 
    ##  8.968104  9.073041  7.079237 13.165586 10.017474 10.856971 12.326090  9.282915 
    ##      7001      7002      7003      7004      7005      7006      7007      7008 
    ##  8.548356 12.535964 10.961908  8.653293 12.221153 10.542160  9.492789  9.702663 
    ##      7009      7010      7011      7012      7013      7014      7015      7016 
    ##  7.813796  9.492789 13.375460  9.177978 11.066845 12.431027 10.961908 11.066845 
    ##      7017      7018      7019      7020      7021      7022      7023      7024 
    ## 11.696467 10.332286 11.381656 14.214957 10.017474  8.653293 10.437223 11.591530 
    ##      7025      7026      7027      7028      7029      7030      7031      7032 
    ## 12.116216 10.122412 10.856971 11.591530  9.492789 10.856971  9.073041 11.276719 
    ##      7033      7034      7035      7036      7037      7038      7039      7040 
    ##  9.387852 10.961908 10.542160 12.116216  8.863167 11.276719 13.900146 10.856971 
    ##      7041      7042      7043      7044      7045      7046      7047      7048 
    ##  7.079237 12.850775  9.177978 12.011279 10.437223  8.128607  7.603922  8.443419 
    ##      7049      7050      7051      7052      7053      7054      7055      7056 
    ## 13.060649  9.492789  7.603922  9.702663  8.653293 13.060649 12.431027  9.282915 
    ##      7057      7058      7059      7060      7061      7062      7063      7064 
    ##  7.079237 11.591530 10.856971 13.480397  7.079237  7.813796 10.332286  8.863167 
    ##      7065      7066      7067      7068      7069      7070      7071      7072 
    ##  8.233544 10.017474  9.492789 10.961908 11.486593 11.696467 11.171782 10.122412 
    ##      7073      7074      7075      7076      7077      7078      7079      7080 
    ##  7.079237  8.968104  8.968104 10.647097  7.079237 12.535964 13.690271 11.801404 
    ##      7081      7082      7083      7084      7085      7086      7087      7088 
    ## 10.647097  8.023670  9.912537 10.122412 13.165586  7.079237 12.221153 12.431027 
    ##      7089      7090      7091      7092      7093      7094      7095      7096 
    ##  9.912537 10.332286 10.856971  7.603922 12.011279  9.597726 12.955712 11.276719 
    ##      7097      7098      7099      7100      7101      7102      7103      7104 
    ##  9.597726  8.863167 11.066845  8.128607 12.011279  7.603922 11.801404  9.597726 
    ##      7105      7106      7107      7108      7109      7110      7111      7112 
    ## 12.116216 10.856971  8.548356 13.060649 12.431027 10.752034  9.597726 10.017474 
    ##      7113      7114      7115      7116      7117      7118      7119      7120 
    ##  9.387852  9.912537 12.116216 10.647097  7.603922 12.221153 10.856971 11.381656 
    ##      7121      7122      7123      7124      7125      7126      7127      7128 
    ## 12.431027  7.079237  8.758230 11.171782 11.906342 10.437223  8.233544  9.282915 
    ##      7129      7130      7131      7132      7133      7134      7135      7136 
    ##  7.079237 10.856971  7.603922 14.214957  9.807600 11.276719 12.745838 12.431027 
    ##      7137      7138      7139      7140      7141      7142      7143      7144 
    ## 11.486593 12.221153 11.591530 13.165586  9.807600 12.850775 13.270523 13.270523 
    ##      7145      7146      7147      7148      7149      7150      7151      7152 
    ##  9.597726 10.752034  8.023670 12.221153  8.968104 11.486593 13.060649 12.431027 
    ##      7153      7154      7155      7156      7157      7158      7159      7160 
    ## 12.116216 14.319894  8.128607 10.227349  8.653293  8.653293  9.912537 10.437223 
    ##      7161      7162      7163      7164      7165      7166      7167      7168 
    ##  9.597726  8.128607  9.807600 12.745838 11.381656 12.326090  8.758230 11.906342 
    ##      7169      7170      7171      7172      7173      7174      7175      7176 
    ##  8.863167  8.338482  8.863167  8.338482  8.653293 10.542160  7.708859  8.233544 
    ##      7177      7178      7179      7180      7181      7182      7183      7184 
    ## 11.381656 10.332286 10.752034 13.795209 10.227349  9.807600 11.066845  9.073041 
    ##      7185      7186      7187      7188      7189      7190      7191      7192 
    ## 10.227349  9.177978 12.955712  8.443419 10.437223 10.647097  9.177978 10.647097 
    ##      7193      7194      7195      7196      7197      7198      7199      7200 
    ## 11.066845 13.165586 13.690271 11.381656  9.177978  8.443419 14.110020  9.177978 
    ##      7201      7202      7203      7204      7205      7206      7207      7208 
    ##  9.807600 13.270523  8.968104 11.801404  7.603922 12.221153 12.640901 14.424831 
    ##      7209      7210      7211      7212      7213      7214      7215      7216 
    ##  9.492789  9.492789  8.338482  8.863167 11.696467 11.171782 14.319894 10.437223 
    ##      7217      7218      7219      7220      7221      7222      7223      7224 
    ## 12.745838  8.968104  8.338482 11.591530  8.863167 13.795209 10.332286 11.381656 
    ##      7225      7226      7227      7228      7229      7230      7231      7232 
    ##  9.912537 11.066845  7.079237 11.486593 10.017474 12.535964  9.387852  9.282915 
    ##      7233      7234      7235      7236      7237      7238      7239      7240 
    ## 12.535964 12.221153 10.752034  9.702663 10.332286 11.276719  9.912537  8.758230 
    ##      7241      7242      7243      7244      7245      7246      7247      7248 
    ##  8.968104 12.116216 13.480397  9.282915 12.745838 12.745838 12.850775 13.795209 
    ##      7249      7250      7251      7252      7253      7254      7255      7256 
    ##  9.492789 11.381656 12.116216 13.165586 12.640901 13.375460 12.431027 11.801404 
    ##      7257      7258      7259      7260      7261      7262      7263      7264 
    ##  8.233544 10.437223  7.079237 12.745838 14.214957 10.647097 10.122412 14.214957 
    ##      7265      7266      7267      7268      7269      7270      7271      7272 
    ## 10.332286 11.906342 11.906342 10.017474  8.758230 10.856971  9.282915  8.338482 
    ##      7273      7274      7275      7276      7277      7278      7279      7280 
    ##  9.807600  9.177978 14.319894 11.486593  8.758230 11.591530  9.387852  7.603922 
    ##      7281      7282      7283      7284      7285      7286      7287      7288 
    ## 11.801404 10.542160 11.486593  8.653293  8.128607 11.696467 11.066845 12.431027 
    ##      7289      7290      7291      7292      7293      7294      7295      7296 
    ## 10.752034 12.221153 10.752034  7.813796 11.276719 14.214957 13.165586 13.795209 
    ##      7297      7298      7299      7300      7301      7302      7303      7304 
    ## 12.745838 13.690271  9.073041 14.110020 13.795209  8.863167  8.548356  8.758230 
    ##      7305      7306      7307      7308      7309      7310      7311      7312 
    ##  8.548356 10.856971  9.597726  8.443419 12.535964 10.437223 11.696467  9.807600 
    ##      7313      7314      7315      7316      7317      7318      7319      7320 
    ## 10.227349 11.696467  9.073041 10.856971 12.640901 14.319894  8.548356  9.702663 
    ##      7321      7322      7323      7324      7325      7326      7327      7328 
    ## 11.381656 12.326090 12.745838 11.486593  8.128607 10.332286  8.443419 12.640901 
    ##      7329      7330      7331      7332      7333      7334      7335      7336 
    ## 13.375460  7.813796 12.116216 12.850775 11.696467  9.492789 11.486593  9.177978 
    ##      7337      7338      7339      7340      7341      7342      7343      7344 
    ## 13.375460  9.702663  7.079237 12.745838 14.214957  8.443419 11.171782 12.221153 
    ##      7345      7346      7347      7348      7349      7350      7351      7352 
    ## 12.745838 10.961908  7.918733  8.233544 12.850775 14.424831  8.548356  9.597726 
    ##      7353      7354      7355      7356      7357      7358      7359      7360 
    ## 12.640901 11.801404  7.918733 10.647097 12.326090 13.585334  7.079237  8.338482 
    ##      7361      7362      7363      7364      7365      7366      7367      7368 
    ##  7.603922 10.752034  8.653293 11.276719  9.597726  9.492789 13.060649 13.060649 
    ##      7369      7370      7371      7372      7373      7374      7375      7376 
    ## 13.690271  8.653293 12.116216  7.918733  7.708859 11.066845  8.233544  8.968104 
    ##      7377      7378      7379      7380      7381      7382      7383      7384 
    ## 11.591530  7.918733  9.282915  7.079237 12.011279  8.233544 11.696467 14.319894 
    ##      7385      7386      7387      7388      7389      7390      7391      7392 
    ## 12.326090 10.332286 12.221153 10.437223  8.128607 13.270523 11.906342 11.486593 
    ##      7393      7394      7395      7396      7397      7398      7399      7400 
    ##  9.702663  9.492789 14.110020 10.752034  8.443419  8.968104  9.177978  9.073041 
    ##      7401      7402      7403      7404      7405      7406      7407      7408 
    ## 13.165586 11.381656  9.702663 10.856971  9.177978 11.276719 11.801404  9.282915 
    ##      7409      7410      7411      7412      7413      7414      7415      7416 
    ## 12.640901 11.591530 10.542160  9.282915 10.332286  8.233544  7.708859  9.177978 
    ##      7417      7418      7419      7420      7421      7422      7423      7424 
    ##  9.807600 10.647097  9.597726  7.079237  8.233544 10.752034 13.480397  7.708859 
    ##      7425      7426      7427      7428      7429      7430      7431      7432 
    ## 12.955712 10.437223  7.603922  9.597726 12.745838  9.702663 13.165586 10.752034 
    ##      7433      7434      7435      7436      7437      7438      7439      7440 
    ## 11.801404 10.227349 12.116216 11.381656 11.801404 12.535964  7.079237 11.066845 
    ##      7441      7442      7443      7444      7445      7446      7447      7448 
    ## 12.535964  7.603922  7.603922  9.177978 12.011279  7.813796 11.696467 10.752034 
    ##      7449      7450      7451      7452      7453      7454      7455      7456 
    ## 12.535964 13.480397  9.492789 10.227349 11.066845 11.276719 10.647097 11.486593 
    ##      7457      7458      7459      7460      7461      7462      7463      7464 
    ##  9.912537  9.807600 11.171782 10.752034  7.079237  9.282915 14.110020 11.381656 
    ##      7465      7466      7467      7468      7469      7470      7471      7472 
    ##  8.128607  9.073041  7.708859 12.221153  8.443419 14.319894 10.752034 11.171782 
    ##      7473      7474      7475      7476      7477      7478      7479      7480 
    ## 13.480397  8.023670 10.332286 11.276719  9.807600 13.585334 12.850775 13.900146 
    ##      7481      7482      7483      7484      7485      7486      7487      7488 
    ## 10.227349 12.745838 10.961908 12.431027 11.591530 10.122412  7.813796 11.381656 
    ##      7489      7490      7491      7492      7493      7494      7495      7496 
    ##  9.282915 12.011279  9.387852  7.603922 11.696467 13.690271 12.850775 13.900146 
    ##      7497      7498      7499      7500      7501      7502      7503      7504 
    ## 12.535964  8.758230  9.387852 11.486593 11.906342 11.066845  7.708859 10.437223 
    ##      7505      7506      7507      7508      7509      7510      7511      7512 
    ## 10.542160 11.591530  9.807600  9.807600 12.221153  8.443419 10.961908  7.079237 
    ##      7513      7514      7515      7516      7517      7518      7519      7520 
    ## 12.640901 10.647097 12.326090  7.603922 12.745838 11.381656  9.387852  8.548356 
    ##      7521      7522      7523      7524      7525      7526      7527      7528 
    ## 11.486593 12.745838  9.807600 11.591530  9.387852 10.752034 11.381656 13.270523 
    ##      7529      7530      7531      7532      7533      7534      7535      7536 
    ## 13.060649  8.863167 11.906342 11.591530  7.603922 14.214957 12.011279  7.079237 
    ##      7537      7538      7539      7540      7541      7542      7543      7544 
    ##  8.968104  9.912537  9.702663  9.282915 12.850775 10.227349 10.227349 10.856971 
    ##      7545      7546      7547      7548      7549      7550      7551      7552 
    ##  9.282915  7.918733  8.863167  9.807600 12.535964 12.431027 10.752034 10.437223 
    ##      7553      7554      7555      7556      7557      7558      7559      7560 
    ##  9.807600  9.073041 11.591530  8.653293 14.005083  8.233544  8.548356  7.813796 
    ##      7561      7562      7563      7564      7565      7566      7567      7568 
    ##  7.813796  9.073041 10.227349 12.116216  9.073041  7.918733 11.801404 10.332286 
    ##      7569      7570      7571      7572      7573      7574      7575      7576 
    ## 11.801404  9.702663  8.023670  8.863167  9.387852  9.282915 11.066845  9.807600 
    ##      7577      7578      7579      7580      7581      7582      7583      7584 
    ## 11.486593 12.431027 11.171782 13.690271 12.535964  9.492789 10.017474 10.122412 
    ##      7585      7586      7587      7588      7589      7590      7591      7592 
    ## 12.535964 13.270523 13.795209  9.282915  9.597726 13.060649 12.431027 11.066845 
    ##      7593      7594      7595      7596      7597      7598      7599      7600 
    ##  8.443419  9.387852  9.387852  8.443419 14.214957 10.961908  9.807600 10.752034 
    ##      7601      7602      7603      7604      7605      7606      7607      7608 
    ## 12.431027  7.079237 11.591530 13.270523  8.653293 14.319894 12.955712 12.535964 
    ##      7609      7610      7611      7612      7613      7614      7615      7616 
    ## 11.381656  7.603922 13.480397 13.060649 11.276719 11.066845 12.850775 12.221153 
    ##      7617      7618      7619      7620      7621      7622      7623      7624 
    ##  9.282915 12.745838 10.961908 12.326090 12.326090 11.486593  9.597726  9.282915 
    ##      7625      7626      7627      7628      7629      7630      7631      7632 
    ##  7.079237  8.023670  8.758230 10.227349 11.696467  8.443419  9.702663 14.214957 
    ##      7633      7634      7635      7636      7637      7638      7639      7640 
    ##  9.597726 13.270523 10.017474 11.591530  7.603922  8.233544  9.177978 12.535964 
    ##      7641      7642      7643      7644      7645      7646      7647      7648 
    ## 10.332286 11.906342 12.535964 10.961908 11.696467 12.011279 13.165586 12.326090 
    ##      7649      7650      7651      7652      7653      7654      7655      7656 
    ##  9.177978 13.375460  8.128607 10.332286 10.122412 12.955712 13.375460  8.758230 
    ##      7657      7658      7659      7660      7661      7662      7663      7664 
    ##  7.603922 11.801404  8.548356 12.221153 11.591530  9.807600 10.017474 10.017474 
    ##      7665      7666      7667      7668      7669      7670      7671      7672 
    ## 10.752034  9.177978  9.807600 12.850775 11.486593  9.073041 11.276719  9.492789 
    ##      7673      7674      7675      7676      7677      7678      7679      7680 
    ##  9.073041 12.011279  8.548356 11.486593 12.221153 10.122412  9.387852 12.116216 
    ##      7681      7682      7683      7684      7685      7686      7687      7688 
    ## 11.276719  8.968104  8.758230  9.702663  7.603922  9.177978  8.758230 10.752034 
    ##      7689      7690      7691      7692      7693      7694      7695      7696 
    ## 14.110020 10.017474 13.060649 11.591530 10.437223 14.005083 14.005083  8.023670 
    ##      7697      7698      7699      7700      7701      7702      7703      7704 
    ## 10.437223 10.961908  7.918733 13.480397 11.801404  8.863167 11.171782  8.653293 
    ##      7705      7706      7707      7708      7709      7710      7711      7712 
    ##  9.073041 11.591530 12.326090 11.906342 13.165586  8.863167  9.177978 10.542160 
    ##      7713      7714      7715      7716      7717      7718      7719      7720 
    ## 10.542160 12.011279  8.548356 10.017474  8.548356 12.011279  8.863167  9.492789 
    ##      7721      7722      7723      7724      7725      7726      7727      7728 
    ## 14.214957  9.597726 10.437223  8.443419 11.486593 11.801404  8.758230 12.640901 
    ##      7729      7730      7731      7732      7733      7734      7735      7736 
    ## 12.011279  7.813796  7.603922 13.060649 10.332286 10.227349 10.647097 12.850775 
    ##      7737      7738      7739      7740      7741      7742      7743      7744 
    ## 13.375460 12.116216 13.270523  8.023670  8.968104  9.387852  9.807600  9.702663 
    ##      7745      7746      7747      7748      7749      7750      7751      7752 
    ##  8.443419 12.431027 12.326090 10.647097 10.856971 12.535964 14.424831 10.542160 
    ##      7753      7754      7755      7756      7757      7758      7759      7760 
    ##  9.387852 12.850775 12.011279  8.338482 13.375460 10.961908 13.375460  9.177978 
    ##      7761      7762      7763      7764      7765      7766      7767      7768 
    ## 11.696467 11.066845  9.177978 12.326090  9.702663  8.128607 14.424831  9.807600 
    ##      7769      7770      7771      7772      7773      7774      7775      7776 
    ##  8.653293 10.961908 12.011279  7.603922  9.073041 11.591530 12.850775  8.023670 
    ##      7777      7778      7779      7780      7781      7782      7783      7784 
    ## 10.017474 11.276719  9.702663 14.005083  8.968104  8.548356  8.548356  9.177978 
    ##      7785      7786      7787      7788      7789      7790      7791      7792 
    ##  8.758230 12.011279 14.319894 13.585334 11.906342 10.542160 13.690271 13.690271 
    ##      7793      7794      7795      7796      7797      7798      7799      7800 
    ## 11.696467 12.955712  9.807600  7.079237  9.282915 10.856971 10.227349  7.918733 
    ##      7801      7802      7803      7804      7805      7806      7807      7808 
    ##  8.653293  9.387852 10.332286  9.387852  8.758230 10.017474 12.011279 11.066845 
    ##      7809      7810      7811      7812      7813      7814      7815      7816 
    ##  9.597726 10.961908  7.603922  9.177978  9.492789  8.758230  8.128607 13.270523 
    ##      7817      7818      7819      7820      7821      7822      7823      7824 
    ##  8.863167  8.968104 13.060649  7.918733 11.906342  9.597726 11.801404 13.375460 
    ##      7825      7826      7827      7828      7829      7830      7831      7832 
    ## 13.795209 12.640901  8.653293  7.079237 11.486593 11.591530 11.591530 12.850775 
    ##      7833      7834      7835      7836      7837      7838      7839      7840 
    ##  9.492789  9.912537  9.282915 10.961908 12.955712 10.017474  9.387852 10.332286 
    ##      7841      7842      7843      7844      7845      7846      7847      7848 
    ##  8.338482  9.702663 10.017474 10.542160  8.128607 13.165586 12.745838  8.968104 
    ##      7849      7850      7851      7852      7853      7854      7855      7856 
    ##  9.597726 12.745838 10.647097  7.079237  7.603922 11.381656 10.332286  7.079237 
    ##      7857      7858      7859      7860      7861      7862      7863      7864 
    ## 10.647097  7.708859 10.647097  7.079237  8.128607  8.863167 11.906342  8.023670 
    ##      7865      7866      7867      7868      7869      7870      7871      7872 
    ##  7.918733  9.807600 13.585334 11.906342 10.856971 14.110020 12.955712 11.276719 
    ##      7873      7874      7875      7876      7877      7878      7879      7880 
    ## 11.696467  9.492789 13.585334 10.437223  8.548356 12.535964 10.437223  9.282915 
    ##      7881      7882      7883      7884      7885      7886      7887      7888 
    ##  8.863167 10.856971 11.591530  9.597726  9.177978 13.690271 12.431027 10.542160 
    ##      7889      7890      7891      7892      7893      7894      7895      7896 
    ##  9.912537 12.431027 12.011279 10.647097  8.548356 10.332286  8.863167 13.795209 
    ##      7897      7898      7899      7900      7901      7902      7903      7904 
    ##  7.603922 12.640901  7.708859  9.702663 11.171782 14.319894 10.332286 11.171782 
    ##      7905      7906      7907      7908      7909      7910      7911      7912 
    ## 11.486593 11.171782  8.863167 12.640901  7.603922 10.647097 10.437223 14.110020 
    ##      7913      7914      7915      7916      7917      7918      7919      7920 
    ## 11.381656 10.647097 12.850775  9.912537 13.060649  8.758230  9.177978 12.431027 
    ##      7921      7922      7923      7924      7925      7926      7927      7928 
    ## 11.171782  7.708859 10.227349 11.486593 12.431027  9.597726 10.542160 12.640901 
    ##      7929      7930      7931      7932      7933      7934      7935      7936 
    ##  9.282915 10.856971 12.745838  8.653293  8.338482 12.326090 11.381656  8.548356 
    ##      7937      7938      7939      7940      7941      7942      7943      7944 
    ## 10.227349 13.480397 12.955712  8.443419  9.807600 12.745838  9.597726 13.690271 
    ##      7945      7946      7947      7948      7949      7950      7951      7952 
    ##  7.079237  9.597726 12.955712  8.023670 10.437223 12.116216 11.906342  9.807600 
    ##      7953      7954      7955      7956      7957      7958      7959      7960 
    ## 13.795209  9.282915 11.486593 11.066845 12.640901 13.165586 10.542160 10.647097 
    ##      7961      7962      7963      7964      7965      7966      7967      7968 
    ##  8.653293  9.177978  8.233544 12.431027 12.850775 13.375460  8.863167 14.110020 
    ##      7969      7970      7971      7972      7973      7974      7975      7976 
    ##  8.863167 11.381656 11.486593 10.647097 13.270523  9.807600  7.603922  7.603922 
    ##      7977      7978      7979      7980      7981      7982      7983      7984 
    ##  9.912537 10.856971 11.066845 10.227349 11.591530 12.116216 11.381656 12.850775 
    ##      7985      7986      7987      7988      7989      7990      7991      7992 
    ## 13.375460 10.647097  7.079237 11.906342 10.856971  8.443419  9.387852 10.437223 
    ##      7993      7994      7995      7996      7997      7998      7999      8000 
    ##  9.492789  7.603922 11.171782 10.017474  9.492789  9.073041 12.011279 12.431027 
    ##      8001      8002      8003      8004      8005      8006      8007      8008 
    ##  8.128607  9.912537  7.813796 10.227349  8.443419 10.437223 11.486593 12.221153 
    ##      8009      8010      8011      8012      8013      8014      8015      8016 
    ## 13.795209 11.801404 10.647097  7.603922 14.005083  9.702663 11.276719  7.603922 
    ##      8017      8018      8019      8020      8021      8022      8023      8024 
    ##  9.177978  9.597726 12.535964 13.165586 11.696467 11.486593 12.011279 10.017474 
    ##      8025      8026      8027      8028      8029      8030      8031      8032 
    ## 10.961908 11.171782 12.535964  7.813796 11.696467 13.690271 12.955712 12.850775 
    ##      8033      8034      8035      8036      8037      8038      8039      8040 
    ## 11.171782 12.326090  7.603922  8.653293 13.165586 14.319894  9.282915  8.863167 
    ##      8041      8042      8043      8044      8045      8046      8047      8048 
    ## 13.270523  8.758230 13.060649  9.597726  7.708859  8.968104 12.116216  8.758230 
    ##      8049      8050      8051      8052      8053      8054      8055      8056 
    ## 11.591530 10.542160  8.863167  8.443419  8.758230  7.603922 10.227349  9.387852 
    ##      8057      8058      8059      8060      8061      8062      8063      8064 
    ## 12.955712  9.387852  7.918733  9.597726  8.653293 10.542160 13.585334 12.640901 
    ##      8065      8066      8067      8068      8069      8070      8071      8072 
    ## 12.116216 11.591530 10.961908 12.431027  9.597726 11.801404 12.850775 11.486593 
    ##      8073      8074      8075      8076      8077      8078      8079      8080 
    ## 12.955712 11.171782 10.227349  9.597726  8.338482 11.906342 12.535964 11.696467 
    ##      8081      8082      8083      8084      8085      8086      8087      8088 
    ##  9.177978 12.221153 11.591530 12.221153  7.918733 13.375460 10.961908  8.758230 
    ##      8089      8090      8091      8092      8093      8094      8095      8096 
    ## 12.955712 11.486593 11.381656 12.535964  8.863167  9.807600 11.801404 13.060649 
    ##      8097      8098      8099      8100      8101      8102      8103      8104 
    ## 10.961908  8.653293 12.326090 10.332286 14.214957 12.221153  7.603922 12.535964 
    ##      8105      8106      8107      8108      8109      8110      8111      8112 
    ## 11.171782  9.387852  7.603922 12.326090 10.122412 10.017474 11.276719  9.177978 
    ##      8113      8114      8115      8116      8117      8118      8119      8120 
    ##  9.807600 12.535964  9.492789 10.647097  8.233544  9.177978 12.221153  9.177978 
    ##      8121      8122      8123      8124      8125      8126      8127      8128 
    ##  8.653293 12.221153 14.214957  9.282915  9.912537 11.171782  9.387852  8.968104 
    ##      8129      8130      8131      8132      8133      8134      8135      8136 
    ## 10.122412  9.702663 14.424831 10.437223  8.548356 14.319894  7.603922 13.585334 
    ##      8137      8138      8139      8140      8141      8142      8143      8144 
    ## 13.585334 13.690271 12.850775 12.221153 12.326090  9.492789  9.597726 11.801404 
    ##      8145      8146      8147      8148      8149      8150      8151      8152 
    ##  8.968104 10.227349  9.702663  9.597726 12.640901 12.221153 13.690271 11.486593 
    ##      8153      8154      8155      8156      8157      8158      8159      8160 
    ## 12.011279 13.270523  7.918733  9.387852  7.708859  8.548356  9.387852 13.900146 
    ##      8161      8162      8163      8164      8165      8166      8167      8168 
    ## 10.227349  9.177978 11.381656  9.282915 12.326090 12.011279 13.060649  8.338482 
    ##      8169      8170      8171      8172      8173      8174      8175      8176 
    ##  9.807600  7.918733  9.073041  8.968104 10.017474 14.005083  9.073041 12.850775 
    ##      8177      8178      8179      8180      8181      8182      8183      8184 
    ## 11.591530 14.424831 13.795209 12.745838  7.603922 12.955712 10.332286 10.332286 
    ##      8185      8186      8187      8188      8189      8190      8191      8192 
    ##  9.282915 13.690271 10.122412  8.023670  8.863167  8.863167 10.856971 12.640901 
    ##      8193      8194      8195      8196      8197      8198      8199      8200 
    ## 11.066845 10.542160 12.011279 12.535964 13.060649  9.387852 10.542160  8.443419 
    ##      8201      8202      8203      8204      8205      8206      8207      8208 
    ## 12.535964  8.548356  8.548356 11.906342 10.856971 10.017474 12.011279 12.116216 
    ##      8209      8210      8211      8212      8213      8214      8215      8216 
    ## 12.640901 13.480397 13.060649 13.165586  9.597726 13.060649 12.326090 13.270523 
    ##      8217      8218      8219      8220      8221      8222      8223      8224 
    ## 10.542160  8.443419  8.863167 10.122412 12.011279 12.535964 10.752034 11.066845 
    ##      8225      8226      8227      8228      8229      8230      8231      8232 
    ##  9.177978  9.282915 10.542160 12.745838 12.116216 12.431027 10.437223 10.437223 
    ##      8233      8234      8235      8236      8237      8238      8239      8240 
    ## 12.850775  8.023670 12.221153  8.758230 11.591530  8.653293  8.968104 13.795209 
    ##      8241      8242      8243      8244      8245      8246      8247      8248 
    ##  9.073041  9.807600  9.597726 12.116216 12.221153 11.381656 11.591530 10.332286 
    ##      8249      8250      8251      8252      8253      8254      8255      8256 
    ## 10.017474 14.110020 10.122412  8.023670 14.110020  8.023670 14.110020  8.968104 
    ##      8257      8258      8259      8260      8261      8262      8263      8264 
    ##  7.603922  8.338482  9.702663 10.961908  8.968104 11.801404 10.856971  9.282915 
    ##      8265      8266      8267      8268      8269      8270      8271      8272 
    ## 12.640901 12.431027 10.856971  9.702663 10.017474 11.486593 14.424831 11.486593 
    ##      8273      8274      8275      8276      8277      8278      8279      8280 
    ## 11.591530 13.165586  7.813796  7.603922  7.079237  8.968104 10.122412  8.023670 
    ##      8281      8282      8283      8284      8285      8286      8287      8288 
    ## 13.060649  9.492789 11.486593  9.492789  9.387852 12.221153  9.597726  9.492789 
    ##      8289      8290      8291      8292      8293      8294      8295      8296 
    ##  9.702663 14.110020  7.603922 13.060649  9.912537  9.912537  8.023670 14.005083 
    ##      8297      8298      8299      8300      8301      8302      8303      8304 
    ## 12.221153 12.535964 13.375460 10.017474 11.486593 11.591530  9.702663 10.647097 
    ##      8305      8306      8307      8308      8309      8310      8311      8312 
    ## 11.276719 11.276719 10.122412  7.708859 10.961908  9.912537  7.603922 10.017474 
    ##      8313      8314      8315      8316      8317      8318      8319      8320 
    ## 11.381656  7.813796  8.443419  9.282915 10.332286  8.443419  9.597726  8.443419 
    ##      8321      8322      8323      8324      8325      8326      8327      8328 
    ## 11.906342  8.863167 12.431027 10.752034  9.282915 10.227349  9.282915 10.752034 
    ##      8329      8330      8331      8332      8333      8334      8335      8336 
    ## 10.752034 11.381656 13.690271  9.807600  9.912537 11.696467  9.282915  9.702663 
    ##      8337      8338      8339      8340      8341      8342      8343      8344 
    ## 13.585334 11.276719 10.122412 11.591530  9.387852 11.801404 10.647097 12.431027 
    ##      8345      8346      8347      8348      8349      8350      8351      8352 
    ## 13.165586 11.381656 10.122412 10.542160  8.863167 11.381656  8.653293  9.807600 
    ##      8353      8354      8355      8356      8357      8358      8359      8360 
    ## 11.801404  8.968104 10.647097  8.548356 12.431027 10.017474  9.177978 12.955712 
    ##      8361      8362      8363      8364      8365      8366      8367      8368 
    ##  8.758230  8.338482 12.326090 12.431027 11.171782 10.437223  7.603922 10.752034 
    ##      8369      8370      8371      8372      8373      8374      8375      8376 
    ## 11.171782 12.640901 13.375460  8.653293  8.233544 10.122412  8.653293 13.060649 
    ##      8377      8378      8379      8380      8381      8382      8383      8384 
    ##  9.177978  9.387852  8.023670 10.017474 13.165586 10.122412 13.690271 14.214957 
    ##      8385      8386      8387      8388      8389      8390      8391      8392 
    ##  8.128607 12.221153  7.603922  9.807600  7.708859  8.653293 11.066845 10.647097 
    ##      8393      8394      8395      8396      8397      8398      8399      8400 
    ## 10.227349 12.116216 11.801404  8.443419 12.011279 12.221153 11.486593 10.437223 
    ##      8401      8402      8403      8404      8405      8406      8407      8408 
    ## 11.696467 12.431027 10.227349 13.480397 12.535964 11.066845 13.270523 10.856971 
    ##      8409      8410      8411      8412      8413      8414      8415      8416 
    ## 10.017474  7.603922  7.918733 11.801404 11.486593 11.171782 11.066845 11.906342 
    ##      8417      8418      8419      8420      8421      8422      8423      8424 
    ##  8.443419  7.079237  9.177978 12.955712  9.912537 10.961908  8.443419  9.492789 
    ##      8425      8426      8427      8428      8429      8430      8431      8432 
    ## 10.647097 11.171782 12.640901  9.807600  7.079237  8.023670  8.023670  8.128607 
    ##      8433      8434      8435      8436      8437      8438      8439      8440 
    ## 12.431027  8.338482 10.961908 12.011279  7.603922  8.758230  7.813796 11.171782 
    ##      8441      8442      8443      8444      8445      8446      8447      8448 
    ## 14.214957 12.326090 10.017474 13.480397  8.758230 10.961908  9.597726  7.603922 
    ##      8449      8450      8451      8452      8453      8454      8455      8456 
    ##  9.282915  8.863167 10.122412  9.282915  8.758230  7.079237 10.332286 13.270523 
    ##      8457      8458      8459      8460      8461      8462      8463      8464 
    ##  8.653293  9.177978 12.116216  9.597726  8.653293 13.165586 11.381656  9.282915 
    ##      8465      8466      8467      8468      8469      8470      8471      8472 
    ## 10.017474 10.122412  7.603922 14.214957  9.282915 10.647097 14.005083  9.597726 
    ##      8473      8474      8475      8476      8477      8478      8479      8480 
    ## 10.856971  8.653293 11.486593  8.443419 12.221153 11.906342  8.443419 11.171782 
    ##      8481      8482      8483      8484      8485      8486      8487      8488 
    ##  8.758230 10.437223 11.906342  9.073041 13.690271 11.486593 10.227349  8.758230 
    ##      8489      8490      8491      8492      8493      8494      8495      8496 
    ##  9.702663 11.486593  9.492789  9.282915  9.492789 12.850775  8.548356  9.492789 
    ##      8497      8498      8499      8500      8501      8502      8503      8504 
    ##  7.708859  9.387852 11.066845  9.912537  8.548356 10.752034  8.653293 13.060649 
    ##      8505      8506      8507      8508      8509      8510      8511      8512 
    ## 14.110020 12.640901 11.276719 13.375460 13.795209 10.752034 10.227349 10.017474 
    ##      8513      8514      8515      8516      8517      8518      8519      8520 
    ##  9.492789  8.443419 11.381656 10.017474  7.079237  9.282915  8.758230  9.702663 
    ##      8521      8522      8523      8524      8525      8526      8527      8528 
    ##  9.492789 10.017474 10.122412  9.492789 12.221153 13.480397 12.011279  9.912537 
    ##      8529      8530      8531      8532      8533      8534      8535      8536 
    ##  8.863167 10.542160  9.807600 10.437223 12.431027  9.912537  9.492789  7.603922 
    ##      8537      8538      8539      8540      8541      8542      8543      8544 
    ##  8.863167  9.282915  9.702663  9.702663 13.585334  7.079237  8.548356 14.214957 
    ##      8545      8546      8547      8548      8549      8550      8551      8552 
    ## 10.437223 12.745838 14.424831 11.801404 14.214957 12.011279 10.227349 10.542160 
    ##      8553      8554      8555      8556      8557      8558      8559      8560 
    ## 10.961908  9.387852 11.906342  8.443419 10.856971  7.918733  7.079237 12.535964 
    ##      8561      8562      8563      8564      8565      8566      8567      8568 
    ## 11.906342  8.863167  9.073041 12.431027  8.758230 10.227349  9.912537 13.270523 
    ##      8569      8570      8571      8572      8573      8574      8575      8576 
    ## 10.227349  9.177978  8.758230  8.863167 12.431027 10.437223 11.801404 11.276719 
    ##      8577      8578      8579      8580      8581      8582      8583      8584 
    ## 14.005083  7.918733 10.542160 12.431027  7.603922 10.856971 12.745838 13.375460 
    ##      8585      8586      8587      8588      8589      8590      8591      8592 
    ## 13.375460  9.177978  8.443419  8.023670  8.863167  8.968104 10.752034 12.116216 
    ##      8593      8594      8595      8596      8597      8598      8599      8600 
    ##  9.387852 13.165586 12.745838  8.653293 10.961908  8.443419  8.128607 10.122412 
    ##      8601      8602      8603      8604      8605      8606      8607      8608 
    ##  8.968104 11.591530  9.282915  9.912537  8.968104 12.535964  8.968104  9.177978 
    ##      8609      8610      8611      8612      8613      8614      8615      8616 
    ## 12.326090  7.603922  8.863167  8.023670  9.177978  8.758230 11.171782  9.282915 
    ##      8617      8618      8619      8620      8621      8622      8623      8624 
    ## 10.332286 13.270523 10.752034  8.548356 12.955712  7.918733 12.535964 11.696467 
    ##      8625      8626      8627      8628      8629      8630      8631      8632 
    ##  9.073041  9.807600 11.066845 10.961908 14.110020  8.758230 12.640901 13.060649 
    ##      8633      8634      8635      8636      8637      8638      8639      8640 
    ## 10.017474  9.492789  8.023670  9.073041  8.968104 10.437223  8.968104  8.233544 
    ##      8641      8642      8643      8644      8645      8646      8647      8648 
    ## 10.017474  8.548356 10.647097  8.863167 10.752034  8.233544 13.060649  7.603922 
    ##      8649      8650      8651      8652      8653      8654      8655      8656 
    ##  9.282915 12.745838  8.548356 12.221153 12.535964  9.073041 12.850775 11.171782 
    ##      8657      8658      8659      8660      8661      8662      8663      8664 
    ## 11.066845 13.480397  8.023670 12.221153  8.128607 13.690271  8.863167 12.011279 
    ##      8665      8666      8667      8668      8669      8670      8671      8672 
    ## 11.906342  9.492789 10.856971 12.221153 11.696467 13.795209 13.900146  7.813796 
    ##      8673      8674      8675      8676      8677      8678      8679      8680 
    ##  7.813796 11.381656 11.696467  9.387852 12.011279 10.961908  9.282915  9.597726 
    ##      8681      8682      8683      8684      8685      8686      8687      8688 
    ## 11.801404 10.542160 11.381656 14.319894 12.431027 13.060649  8.233544 12.745838 
    ##      8689      8690      8691      8692      8693      8694      8695      8696 
    ## 11.696467 11.696467 11.801404 11.906342 11.591530 11.276719 11.696467  9.177978 
    ##      8697      8698      8699      8700      8701      8702      8703      8704 
    ##  8.233544 10.752034 10.332286 10.542160 10.227349 11.276719  9.177978 14.110020 
    ##      8705      8706      8707      8708      8709      8710      8711      8712 
    ##  9.073041 11.066845 10.227349 12.535964 13.165586 11.486593  8.758230 13.060649 
    ##      8713      8714      8715      8716      8717      8718      8719      8720 
    ## 12.640901 12.011279  9.492789  8.338482  9.282915 12.640901 12.011279 12.535964 
    ##      8721      8722      8723      8724      8725      8726      8727      8728 
    ## 12.955712  8.338482 11.486593 11.486593 11.381656 12.221153  7.079237 10.856971 
    ##      8729      8730      8731      8732      8733      8734      8735      8736 
    ## 11.801404  9.282915 13.795209  8.758230  7.918733 13.690271  9.177978  9.387852 
    ##      8737      8738      8739      8740      8741      8742      8743      8744 
    ##  9.073041 11.906342 12.116216 12.116216 13.480397 12.116216 10.856971 12.011279 
    ##      8745      8746      8747      8748      8749      8750      8751      8752 
    ## 11.591530 12.640901 10.856971 12.011279 10.017474 12.326090 14.005083 10.856971 
    ##      8753      8754      8755      8756      8757      8758      8759      8760 
    ##  9.807600  9.387852 11.171782 12.745838 11.906342 11.906342 11.171782 12.745838 
    ##      8761      8762      8763      8764      8765      8766      8767      8768 
    ## 11.066845 12.640901 14.110020 12.431027 12.535964 10.017474 10.752034 10.752034 
    ##      8769      8770      8771      8772      8773      8774      8775      8776 
    ## 12.535964 12.745838 11.381656 12.955712  8.653293 10.437223 11.486593 12.745838 
    ##      8777      8778      8779      8780      8781      8782      8783      8784 
    ## 14.319894 12.116216  8.023670  9.492789 13.585334 13.795209 11.066845 10.647097 
    ##      8785      8786      8787      8788      8789      8790      8791      8792 
    ## 12.326090 10.122412 12.535964 10.332286  9.492789  7.708859 10.437223 10.542160 
    ##      8793      8794      8795      8796      8797      8798      8799      8800 
    ## 13.375460  7.813796  9.597726 12.535964 13.060649 10.227349 11.381656  8.758230 
    ##      8801      8802      8803      8804      8805      8806      8807      8808 
    ## 12.011279 13.060649  8.968104 14.110020 10.856971 12.011279 13.375460 10.542160 
    ##      8809      8810      8811      8812      8813      8814      8815      8816 
    ##  8.548356 12.431027 12.011279 12.116216 13.165586 11.171782 12.745838  8.548356 
    ##      8817      8818      8819      8820      8821      8822      8823      8824 
    ##  8.023670 10.332286  9.597726 13.480397 11.486593 12.011279  7.079237  9.282915 
    ##      8825      8826      8827      8828      8829      8830      8831      8832 
    ##  9.807600 11.276719  8.758230 10.752034  7.603922  9.492789 12.745838 10.227349 
    ##      8833      8834      8835      8836      8837      8838      8839      8840 
    ## 11.591530 14.005083 10.437223  7.603922  7.813796  8.338482  7.079237  8.653293 
    ##      8841      8842      8843      8844      8845      8846      8847      8848 
    ##  8.548356  9.177978 10.856971 12.431027 12.431027  8.443419 11.906342 12.326090 
    ##      8849      8850      8851      8852      8853      8854      8855      8856 
    ##  7.708859 10.647097  9.597726  9.492789  8.548356 10.856971 13.165586 10.227349 
    ##      8857      8858      8859      8860      8861      8862      8863      8864 
    ## 13.060649  9.492789  7.079237 12.745838  7.708859  9.597726 14.110020  9.807600 
    ##      8865      8866      8867      8868      8869      8870      8871      8872 
    ## 13.480397 11.591530 13.795209 10.752034 11.696467  9.597726 12.221153 10.017474 
    ##      8873      8874      8875      8876      8877      8878      8879      8880 
    ##  8.233544  7.079237 12.535964 11.591530 12.221153 10.752034  8.023670 12.326090 
    ##      8881      8882      8883      8884      8885      8886      8887      8888 
    ##  9.807600  9.597726  7.603922 10.961908 12.221153 14.424831  7.079237  8.758230 
    ##      8889      8890      8891      8892      8893      8894      8895      8896 
    ##  8.653293 10.752034 11.801404 10.856971 12.850775 11.906342 10.332286 11.906342 
    ##      8897      8898      8899      8900      8901      8902      8903      8904 
    ##  9.492789  8.758230 10.856971  7.603922  8.863167  9.387852  7.603922  8.443419 
    ##      8905      8906      8907      8908      8909      8910      8911      8912 
    ## 10.542160 13.060649 13.375460 11.906342 10.961908 13.480397 11.381656  8.128607 
    ##      8913      8914      8915      8916      8917      8918      8919      8920 
    ##  7.603922 10.332286  9.177978 14.319894  7.079237  7.603922  9.387852 11.066845 
    ##      8921      8922      8923      8924      8925      8926      8927      8928 
    ## 10.542160  7.708859  7.603922 10.752034 12.431027 13.060649  9.177978  8.968104 
    ##      8929      8930      8931      8932      8933      8934      8935      8936 
    ##  9.073041  8.548356  7.918733 12.431027  7.918733 11.696467 11.066845  9.912537 
    ##      8937      8938      8939      8940      8941      8942      8943      8944 
    ## 10.647097 12.326090 10.961908  8.653293 12.535964 12.221153  9.282915  9.597726 
    ##      8945      8946      8947      8948      8949      8950      8951      8952 
    ##  9.282915  8.968104 13.900146 13.165586  8.443419 10.542160 14.110020 11.171782 
    ##      8953      8954      8955      8956      8957      8958      8959      8960 
    ##  8.968104 12.116216 11.486593 12.745838  7.603922  8.233544  9.073041  9.702663 
    ##      8961      8962      8963      8964      8965      8966      8967      8968 
    ## 10.856971  9.492789 11.801404 11.486593  7.708859 11.066845 14.214957 10.227349 
    ##      8969      8970      8971      8972      8973      8974      8975      8976 
    ## 12.011279  9.177978  8.233544 13.690271  9.282915 10.227349 13.900146  8.128607 
    ##      8977      8978      8979      8980      8981      8982      8983      8984 
    ##  9.177978 11.171782 12.011279 11.066845 12.221153 12.221153  8.968104 11.906342 
    ##      8985      8986      8987      8988      8989      8990      8991      8992 
    ##  9.807600 11.591530  8.968104  7.813796 14.319894  7.918733 11.696467 12.221153 
    ##      8993      8994      8995      8996      8997      8998      8999      9000 
    ##  9.807600 12.221153 12.116216 12.535964 12.011279  9.282915 11.696467 10.647097 
    ##      9001      9002      9003      9004      9005      9006      9007      9008 
    ## 14.424831  8.233544 10.332286 11.276719 12.640901 11.906342 13.690271 11.696467 
    ##      9009      9010      9011      9012      9013      9014      9015      9016 
    ## 13.795209 11.801404 11.696467 13.900146 10.856971  7.603922  8.443419 10.542160 
    ##      9017      9018      9019      9020      9021      9022      9023      9024 
    ## 13.690271 10.332286  8.023670 10.437223  9.912537 12.011279  8.548356 11.906342 
    ##      9025      9026      9027      9028      9029      9030      9031      9032 
    ##  7.603922 11.486593 11.906342 11.906342 13.375460 12.326090  8.758230 12.850775 
    ##      9033      9034      9035      9036      9037      9038      9039      9040 
    ## 13.165586  8.758230 13.585334 12.011279  8.758230 11.486593  7.603922 12.221153 
    ##      9041      9042      9043      9044      9045      9046      9047      9048 
    ## 11.801404  7.079237  7.603922 13.585334 12.116216 12.221153 11.696467  9.807600 
    ##      9049      9050      9051      9052      9053      9054      9055      9056 
    ## 13.480397  8.968104 13.270523 12.955712  9.387852  8.233544 12.745838 12.745838 
    ##      9057      9058      9059      9060      9061      9062      9063      9064 
    ## 13.165586  8.758230 11.066845  8.443419 12.116216  9.597726  8.128607 11.486593 
    ##      9065      9066      9067      9068      9069      9070      9071      9072 
    ## 12.221153 11.906342 11.591530  8.863167 12.431027  7.079237  9.492789 11.801404 
    ##      9073      9074      9075      9076      9077      9078      9079      9080 
    ## 10.647097  8.128607 14.214957 12.535964 10.332286  9.702663 10.647097  8.968104 
    ##      9081      9082      9083      9084      9085      9086      9087      9088 
    ##  7.813796  7.603922  9.702663  7.918733 10.752034 10.647097 11.381656 12.850775 
    ##      9089      9090      9091      9092      9093      9094      9095      9096 
    ## 12.221153 12.011279  9.912537 12.745838  8.863167 10.752034 13.375460  7.079237 
    ##      9097      9098      9099      9100      9101      9102      9103      9104 
    ##  9.912537  9.387852 10.122412 10.647097 11.276719 11.066845  8.128607 13.690271 
    ##      9105      9106      9107      9108      9109      9110      9111      9112 
    ## 13.690271  7.708859  7.079237 13.690271 11.801404 13.165586 10.017474  7.079237 
    ##      9113      9114      9115      9116      9117      9118      9119      9120 
    ##  8.548356  8.128607  7.079237 11.066845 10.332286  8.338482  7.079237 12.640901 
    ##      9121      9122      9123      9124      9125      9126      9127      9128 
    ##  9.912537 14.214957 10.122412 12.535964 11.171782 10.122412 10.961908  8.128607 
    ##      9129      9130      9131      9132      9133      9134      9135      9136 
    ## 11.276719 11.276719  9.177978 12.535964  9.492789 12.955712  9.597726 12.745838 
    ##      9137      9138      9139      9140      9141      9142      9143      9144 
    ## 10.856971  8.443419  8.233544  8.338482  7.603922 12.221153 11.801404 12.850775 
    ##      9145      9146      9147      9148      9149      9150      9151      9152 
    ##  8.023670 13.165586 10.542160 10.856971  9.387852 14.214957 12.221153 10.647097 
    ##      9153      9154      9155      9156      9157      9158      9159      9160 
    ##  8.128607  9.492789 11.801404 13.690271 13.690271 10.542160 14.319894 12.221153 
    ##      9161      9162      9163      9164      9165      9166      9167      9168 
    ##  7.079237  8.338482  9.912537  8.758230 11.066845  9.282915 12.221153 12.011279 
    ##      9169      9170      9171      9172      9173      9174      9175      9176 
    ## 13.375460 11.801404  7.079237 12.955712  8.338482 12.745838 14.110020  7.603922 
    ##      9177      9178      9179      9180      9181      9182      9183      9184 
    ## 12.431027 12.011279 10.437223 11.801404  8.443419  7.918733 10.437223 10.122412 
    ##      9185      9186      9187      9188      9189      9190      9191      9192 
    ##  7.603922  9.702663  9.807600  8.338482 12.116216 12.011279  8.128607  9.177978 
    ##      9193      9194      9195      9196      9197      9198      9199      9200 
    ## 10.227349 11.591530 12.745838  8.128607 14.214957  8.128607 12.011279  9.387852 
    ##      9201      9202      9203      9204      9205      9206      9207      9208 
    ## 12.640901 11.066845  8.863167  9.282915 11.486593  8.023670  9.597726 10.017474 
    ##      9209      9210      9211      9212      9213      9214      9215      9216 
    ## 11.906342  8.338482  7.603922  9.597726  7.603922  8.128607  9.807600 11.906342 
    ##      9217      9218      9219      9220      9221      9222      9223      9224 
    ## 10.437223 12.955712 10.017474  8.863167 13.375460  8.233544  9.177978  8.548356 
    ##      9225      9226      9227      9228      9229      9230      9231      9232 
    ##  9.073041 10.332286  7.603922 10.647097 10.332286 12.431027 13.060649 10.017474 
    ##      9233      9234      9235      9236      9237      9238      9239      9240 
    ## 13.060649 12.850775  9.597726 10.332286 11.906342  8.338482  8.548356 10.647097 
    ##      9241      9242      9243      9244      9245      9246      9247      9248 
    ##  8.548356 12.116216 11.381656 14.214957  9.387852  7.603922 10.017474 13.270523 
    ##      9249      9250      9251      9252      9253      9254      9255      9256 
    ##  7.708859  8.653293 12.221153  7.813796  9.282915 14.110020 10.332286  9.492789 
    ##      9257      9258      9259      9260      9261      9262      9263      9264 
    ## 12.221153 12.116216  8.443419 10.437223 10.227349  8.023670 10.961908  7.603922 
    ##      9265      9266      9267      9268      9269      9270      9271      9272 
    ##  8.443419  9.912537  9.282915 12.850775  8.758230 10.856971  9.387852 10.856971 
    ##      9273      9274      9275      9276      9277      9278      9279      9280 
    ## 13.060649  8.443419 12.326090 13.270523 11.591530  7.079237 12.326090  9.702663 
    ##      9281      9282      9283      9284      9285      9286      9287      9288 
    ##  8.443419 14.424831 11.696467 12.535964 10.647097 14.319894 12.640901  8.443419 
    ##      9289      9290      9291      9292      9293      9294      9295      9296 
    ##  7.079237 10.752034  9.492789  7.708859 10.542160  9.807600 12.955712  8.548356 
    ##      9297      9298      9299      9300      9301      9302      9303      9304 
    ##  8.233544  9.807600 11.801404 13.165586 11.906342 11.801404  8.338482 10.542160 
    ##      9305      9306      9307      9308      9309      9310      9311      9312 
    ## 10.017474 10.332286  8.338482  7.603922 13.795209  7.708859 12.850775 12.535964 
    ##      9313      9314      9315      9316      9317      9318      9319      9320 
    ##  7.603922  9.177978 13.690271 12.745838 12.116216 12.640901 11.276719 12.745838 
    ##      9321      9322      9323      9324      9325      9326      9327      9328 
    ## 10.332286  9.807600 12.116216  8.338482 11.906342 11.591530 10.017474 10.017474 
    ##      9329      9330      9331      9332      9333      9334      9335      9336 
    ## 13.900146 11.066845 12.535964  8.758230 11.381656 13.690271 10.752034 12.116216 
    ##      9337      9338      9339      9340      9341      9342      9343      9344 
    ## 10.647097 11.801404  8.128607 11.906342  8.968104 11.696467 13.060649  7.708859 
    ##      9345      9346      9347      9348      9349      9350      9351      9352 
    ## 10.752034 14.319894 12.116216 12.955712  9.912537  9.282915  9.073041 13.375460 
    ##      9353      9354      9355      9356      9357      9358      9359      9360 
    ## 13.690271 13.795209 14.005083 14.214957 13.480397 14.214957 14.214957 11.906342 
    ##      9361      9362      9363      9364      9365      9366      9367      9368 
    ## 10.437223 12.955712  8.023670 11.381656 12.011279  8.863167  7.603922  8.968104 
    ##      9369      9370      9371      9372      9373      9374      9375      9376 
    ## 10.961908 10.856971 11.696467 11.171782  7.603922  9.282915 12.745838 11.696467 
    ##      9377      9378      9379      9380      9381      9382      9383      9384 
    ## 12.221153 12.326090 11.066845 11.171782 11.276719  7.603922  8.548356  7.918733 
    ##      9385      9386      9387      9388      9389      9390      9391      9392 
    ## 10.122412 11.276719  8.863167  7.603922  8.653293 10.752034 12.326090 10.122412 
    ##      9393      9394      9395      9396      9397      9398      9399      9400 
    ## 12.850775 12.850775 12.850775  8.023670  9.282915 12.535964  8.443419  8.233544 
    ##      9401      9402      9403      9404      9405      9406      9407      9408 
    ## 10.122412  9.387852 11.381656  9.282915 11.486593  8.128607 10.017474 13.375460 
    ##      9409      9410      9411      9412      9413      9414      9415      9416 
    ## 11.276719  7.708859  7.603922  8.758230  9.387852  8.443419 10.542160  9.912537 
    ##      9417      9418      9419      9420      9421      9422      9423      9424 
    ## 12.745838  8.863167  8.653293 12.955712  9.387852  9.282915  8.338482 11.906342 
    ##      9425      9426      9427      9428      9429      9430      9431      9432 
    ## 11.276719  8.443419  8.128607  9.702663 12.326090  9.177978  8.128607  8.863167 
    ##      9433      9434      9435      9436      9437      9438      9439      9440 
    ##  7.603922  8.758230 11.906342 13.060649  8.968104 13.270523 12.011279 11.066845 
    ##      9441      9442      9443      9444      9445      9446      9447      9448 
    ## 10.437223 10.856971 13.165586  7.603922  7.918733 12.640901 10.961908  8.233544 
    ##      9449      9450      9451      9452      9453      9454      9455      9456 
    ## 10.017474  9.807600 12.535964  8.128607  7.708859 10.227349  8.653293  7.079237 
    ##      9457      9458      9459      9460      9461      9462      9463      9464 
    ## 10.332286  7.813796 10.227349 12.745838 10.227349 12.850775  9.912537 11.906342 
    ##      9465      9466      9467      9468      9469      9470      9471      9472 
    ##  8.968104  8.863167 11.381656 10.332286 12.535964  8.653293 10.647097  9.597726 
    ##      9473      9474      9475      9476      9477      9478      9479      9480 
    ##  8.548356  7.813796 11.591530 10.122412  8.338482 11.066845  9.073041 11.696467 
    ##      9481      9482      9483      9484      9485      9486      9487      9488 
    ##  8.128607 10.122412 11.276719  7.079237 11.486593 13.585334 14.110020 13.900146 
    ##      9489      9490      9491      9492      9493      9494      9495      9496 
    ## 13.165586 10.542160 12.850775  9.912537 11.276719  7.813796  7.079237 11.801404 
    ##      9497      9498      9499      9500      9501      9502      9503      9504 
    ## 12.011279  7.079237 11.696467 11.696467 12.955712 12.221153  8.128607  8.338482 
    ##      9505      9506      9507      9508      9509      9510      9511      9512 
    ##  8.968104  7.603922  8.548356 13.060649 11.591530  9.807600 10.332286  8.653293 
    ##      9513      9514      9515      9516      9517      9518      9519      9520 
    ## 10.227349  9.702663  8.233544 11.276719 10.017474 11.801404  8.023670  7.603922 
    ##      9521      9522      9523      9524      9525      9526      9527      9528 
    ## 10.647097 10.437223  8.758230 12.011279  8.863167  9.282915  9.282915 10.961908 
    ##      9529      9530      9531      9532      9533      9534      9535      9536 
    ##  8.758230  9.807600  7.813796  8.758230 12.431027  9.282915 12.850775 10.542160 
    ##      9537      9538      9539      9540      9541      9542      9543      9544 
    ##  8.758230 12.745838 12.640901 11.696467 11.381656  9.702663 10.017474  8.863167 
    ##      9545      9546      9547      9548      9549      9550      9551      9552 
    ##  9.073041 10.752034  9.807600 10.752034 10.227349  7.603922 11.486593 11.276719 
    ##      9553      9554      9555      9556      9557      9558      9559      9560 
    ##  8.233544  8.863167 11.171782 12.535964  8.863167 12.011279 12.745838 12.640901 
    ##      9561      9562      9563      9564      9565      9566      9567      9568 
    ## 12.221153  9.807600 10.122412  9.807600 10.542160  9.807600 11.906342 11.801404 
    ##      9569      9570      9571      9572      9573      9574      9575      9576 
    ## 10.647097 10.227349 11.906342 12.640901  7.918733 12.955712 12.745838 12.640901 
    ##      9577      9578      9579      9580      9581      9582      9583      9584 
    ## 13.480397 13.585334 12.535964  9.282915 13.480397 13.585334  9.387852  9.807600 
    ##      9585      9586      9587      9588      9589      9590      9591      9592 
    ## 11.171782 12.116216 12.221153  8.863167 10.856971 13.585334 10.332286 12.011279 
    ##      9593      9594      9595      9596      9597      9598      9599      9600 
    ## 12.640901 10.647097 10.647097  9.492789 13.480397  9.597726 11.276719 13.060649 
    ##      9601      9602      9603      9604      9605      9606      9607      9608 
    ## 12.745838  8.968104 11.171782 12.640901 12.116216 14.005083  9.282915  9.597726 
    ##      9609      9610      9611      9612      9613      9614      9615      9616 
    ## 11.171782 12.535964  8.128607 11.381656  8.338482 12.116216 10.017474 10.647097 
    ##      9617      9618      9619      9620      9621      9622      9623      9624 
    ##  9.807600  9.807600 12.431027 12.431027 13.060649 12.011279  7.603922 13.375460 
    ##      9625      9626      9627      9628      9629      9630      9631      9632 
    ## 12.116216  8.968104 12.535964  8.233544  9.387852 11.276719  9.073041 10.227349 
    ##      9633      9634      9635      9636      9637      9638      9639      9640 
    ## 11.276719  9.387852 11.801404 11.591530  9.702663 10.437223 11.276719 11.276719 
    ##      9641      9642      9643      9644      9645      9646      9647      9648 
    ## 12.640901 13.585334  9.282915 14.005083 14.005083 10.227349  7.603922 12.116216 
    ##      9649      9650      9651      9652      9653      9654      9655      9656 
    ## 10.332286 10.961908  9.492789 12.011279 13.060649 12.326090  8.653293 12.011279 
    ##      9657      9658      9659      9660      9661      9662      9663      9664 
    ##  9.387852 12.955712 11.171782 14.424831 11.486593  9.597726  8.653293 11.276719 
    ##      9665      9666      9667      9668      9669      9670      9671      9672 
    ## 10.542160 10.437223 12.431027  9.387852  9.387852 11.276719  8.443419  8.128607 
    ##      9673      9674      9675      9676      9677      9678      9679      9680 
    ## 14.424831 10.542160 11.591530  8.758230 10.542160 13.060649  8.128607 10.332286 
    ##      9681      9682      9683      9684      9685      9686      9687      9688 
    ## 14.110020 12.850775 12.955712 12.326090 13.690271 10.227349 14.319894 12.221153 
    ##      9689      9690      9691      9692      9693      9694      9695      9696 
    ##  9.387852 12.955712 12.431027 10.122412 12.850775 14.110020 12.011279 10.752034 
    ##      9697      9698      9699      9700      9701      9702      9703      9704 
    ## 10.017474 10.961908 12.116216 12.116216 12.326090 11.171782  9.912537 13.585334 
    ##      9705      9706      9707      9708      9709      9710      9711      9712 
    ## 11.276719 11.486593 11.066845 11.591530 10.332286 10.856971 12.116216 10.752034 
    ##      9713      9714      9715      9716      9717      9718      9719      9720 
    ## 12.745838 10.227349  9.282915 12.431027 11.591530  9.492789  8.863167  9.073041 
    ##      9721      9722      9723      9724      9725      9726      9727      9728 
    ##  7.603922 10.542160 10.122412 11.066845  9.912537 12.850775 11.696467 13.165586 
    ##      9729      9730      9731      9732      9733      9734      9735      9736 
    ## 11.696467  9.807600  8.758230 13.480397  9.073041 11.171782 10.017474 10.856971 
    ##      9737      9738      9739      9740      9741      9742      9743      9744 
    ##  7.603922  9.807600  7.918733 13.585334 10.122412  7.603922  9.702663 12.850775 
    ##      9745      9746      9747      9748      9749      9750      9751      9752 
    ##  8.338482  8.443419 11.171782 13.480397 13.585334 13.795209 14.319894 13.900146 
    ##      9753      9754      9755      9756      9757      9758      9759      9760 
    ## 14.214957 14.214957 13.480397 14.319894 14.319894 13.585334 13.795209 14.214957 
    ##      9761      9762      9763      9764      9765      9766      9767      9768 
    ## 13.480397 14.319894 14.110020 14.319894 13.795209 14.110020 13.900146 13.375460 
    ##      9769      9770      9771      9772      9773      9774      9775      9776 
    ## 14.214957 14.319894 13.480397 13.795209 14.319894 13.375460 13.375460 14.319894 
    ##      9777      9778      9779      9780      9781      9782      9783      9784 
    ## 13.585334 13.795209 14.110020 13.585334 14.005083 14.110020 13.900146 14.005083 
    ##      9785      9786      9787      9788      9789      9790      9791      9792 
    ## 10.437223  8.863167 10.122412 12.640901  7.813796 10.752034  8.653293  9.177978 
    ##      9793      9794      9795      9796      9797      9798      9799      9800 
    ## 10.961908  7.079237  7.708859 12.640901 10.856971 11.696467  8.128607  8.443419 
    ##      9801      9802      9803      9804      9805      9806      9807      9808 
    ##  7.603922 10.332286 10.961908  8.023670  7.918733  9.073041 10.017474 12.011279 
    ##      9809      9810      9811      9812      9813      9814      9815      9816 
    ##  7.079237 10.332286  8.443419 11.171782 11.486593 12.221153  9.597726  7.708859 
    ##      9817      9818      9819      9820      9821      9822      9823      9824 
    ##  8.443419 12.955712 12.431027 12.535964 12.745838  8.338482 10.961908  9.702663 
    ##      9825      9826      9827      9828      9829      9830      9831      9832 
    ## 10.542160  8.653293 12.116216 11.696467 10.856971  8.653293  8.128607 11.696467 
    ##      9833      9834      9835      9836      9837      9838      9839      9840 
    ## 10.752034  8.233544  9.282915 12.431027  9.073041 10.227349  9.282915 10.647097 
    ##      9841      9842      9843      9844      9845      9846      9847      9848 
    ## 12.326090  9.177978 10.227349 10.961908  8.548356 12.850775 11.696467 12.535964 
    ##      9849      9850      9851      9852      9853      9854      9855      9856 
    ## 13.270523  8.128607 11.171782 10.961908  9.597726 12.850775 12.955712 12.326090 
    ##      9857      9858      9859      9860      9861      9862      9863      9864 
    ## 12.431027 11.906342  7.079237 12.326090  8.233544 11.801404 13.375460 11.276719 
    ##      9865      9866      9867      9868      9869      9870      9871      9872 
    ## 10.961908 10.122412 10.437223 11.801404 10.017474  9.282915  7.813796 10.437223 
    ##      9873      9874      9875      9876      9877      9878      9879      9880 
    ##  8.338482 11.486593 13.270523  9.282915  7.813796 11.381656  9.177978 13.165586 
    ##      9881      9882      9883      9884      9885      9886      9887      9888 
    ##  7.603922  7.603922 11.066845  8.758230 13.375460 11.696467 12.116216 10.752034 
    ##      9889      9890      9891      9892      9893      9894      9895      9896 
    ## 10.752034 12.431027 11.171782 11.066845 13.165586 12.431027 12.850775 12.431027 
    ##      9897      9898      9899      9900      9901      9902      9903      9904 
    ## 13.165586  8.758230 12.640901 11.906342  8.653293  9.073041  7.079237 10.017474 
    ##      9905      9906      9907      9908      9909      9910      9911      9912 
    ##  9.177978 12.116216 11.276719 12.221153  9.282915 12.326090 10.542160 11.696467 
    ##      9913      9914      9915      9916      9917      9918      9919      9920 
    ## 12.745838 11.801404 12.326090 12.640901  8.758230 11.801404 10.856971 10.437223 
    ##      9921      9922      9923      9924      9925      9926      9927      9928 
    ##  8.758230 10.542160  8.863167 11.066845  9.492789  8.863167  8.338482 11.906342 
    ##      9929      9930      9931      9932      9933      9934      9935      9936 
    ## 11.486593  8.443419 11.906342  8.338482  7.708859 10.961908 12.431027 10.227349 
    ##      9937      9938      9939      9940      9941      9942      9943      9944 
    ## 10.961908  7.603922 12.640901  8.968104  8.758230  9.492789  9.387852  8.548356 
    ##      9945      9946      9947      9948      9949      9950      9951      9952 
    ## 13.060649 10.752034  8.023670 12.326090 10.961908 10.227349 10.227349  7.603922 
    ##      9953      9954      9955      9956      9957      9958      9959      9960 
    ##  9.177978  7.603922 10.122412  7.603922 12.535964  8.863167 10.017474 12.221153 
    ##      9961      9962      9963      9964      9965      9966      9967      9968 
    ##  9.177978 12.326090  7.813796  9.387852 11.381656 11.801404 10.332286 12.221153 
    ##      9969      9970      9971      9972      9973      9974      9975      9976 
    ## 12.431027  7.918733 11.906342 11.696467  7.603922 12.011279  9.807600 12.745838 
    ##      9977      9978      9979      9980      9981      9982      9983      9984 
    ##  8.443419 12.431027 10.437223  8.863167  8.758230 10.122412  9.492789 13.060649 
    ##      9985      9986      9987      9988      9989      9990      9991      9992 
    ##  9.807600 10.227349 11.801404  8.863167 11.591530 12.221153  8.128607 11.276719 
    ##      9993      9994      9995      9996      9997      9998      9999     10000 
    ## 12.535964 11.381656 11.486593 10.542160 11.801404 10.856971  8.653293  9.597726 
    ##     10001     10002     10003     10004     10005     10006     10007     10008 
    ## 10.227349 12.850775 12.431027 11.276719  8.863167 11.486593 12.011279  9.492789 
    ##     10009     10010     10011     10012     10013     10014     10015     10016 
    ##  9.912537  8.758230 12.011279  8.548356 10.227349  7.079237 11.696467  7.603922 
    ##     10017     10018     10019     10020     10021     10022     10023     10024 
    ##  9.807600  8.128607 10.961908  9.387852  8.968104 12.640901 11.696467  7.603922 
    ##     10025     10026     10027     10028     10029     10030     10031     10032 
    ##  7.603922  7.603922 10.542160 13.165586  9.073041 10.647097  9.597726 12.326090 
    ##     10033     10034     10035     10036     10037     10038     10039     10040 
    ##  7.603922 10.752034  9.597726 10.647097 12.745838 13.060649  7.079237  8.863167 
    ##     10041     10042     10043     10044     10045     10046     10047     10048 
    ## 12.011279 12.116216 11.381656  9.702663 10.542160 10.647097 11.696467 12.326090 
    ##     10049     10050     10051     10052     10053     10054     10055     10056 
    ## 10.961908  8.128607  9.282915  8.548356  9.807600 11.801404 12.955712  9.282915 
    ##     10057     10058     10059     10060     10061     10062     10063     10064 
    ##  9.702663  7.813796  8.548356 11.801404 11.486593 11.801404 12.326090 13.060649 
    ##     10065     10066     10067     10068     10069     10070     10071     10072 
    ##  9.912537 11.801404 11.696467 12.221153  9.492789 11.171782  9.073041 13.165586 
    ##     10073     10074     10075     10076     10077     10078     10079     10080 
    ## 10.227349  9.282915 11.171782 12.640901 12.955712  9.177978 10.752034  7.603922 
    ##     10081     10082     10083     10084     10085     10086     10087     10088 
    ##  8.653293  8.758230 10.752034 10.856971 11.591530  8.968104 12.431027 10.437223 
    ##     10089     10090     10091     10092     10093     10094     10095     10096 
    ## 11.801404 12.640901 12.431027 10.227349  8.338482  7.603922 12.640901  9.912537 
    ##     10097     10098     10099     10100     10101     10102     10103     10104 
    ## 12.850775  8.653293  9.807600 10.227349 12.116216 10.647097  8.443419 10.437223 
    ##     10105     10106     10107     10108     10109     10110     10111     10112 
    ##  9.912537  8.233544 10.961908  7.603922 10.122412  9.177978 10.017474  8.338482 
    ##     10113     10114     10115     10116     10117     10118     10119     10120 
    ##  8.338482  7.603922  9.177978  9.912537 11.486593  7.603922 12.955712 12.640901 
    ##     10121     10122     10123     10124     10125     10126     10127     10128 
    ## 12.640901 11.066845 12.431027 11.591530  7.708859  9.387852  8.233544  9.702663 
    ##     10129     10130     10131     10132     10133     10134     10135     10136 
    ##  8.443419  7.603922  8.968104 13.270523 13.270523 10.332286  7.603922 10.017474 
    ##     10137     10138     10139     10140     10141     10142     10143     10144 
    ## 10.961908  9.912537 10.542160  8.443419 10.856971 12.745838 11.906342 10.647097 
    ##     10145     10146     10147     10148     10149     10150     10151     10152 
    ##  8.863167 11.591530  9.597726 10.647097  8.758230 11.591530  8.548356 12.535964 
    ##     10153     10154     10155     10156     10157     10158     10159     10160 
    ## 11.066845  8.758230 13.270523 12.116216 13.060649  8.758230  9.492789 12.955712 
    ##     10161     10162     10163     10164     10165     10166     10167     10168 
    ##  9.912537 13.060649  9.492789 12.640901  8.968104  8.653293  9.073041  9.387852 
    ##     10169     10170     10171     10172     10173     10174     10175     10176 
    ## 12.326090 10.752034  7.603922  9.387852 11.696467  9.177978  9.912537  9.073041 
    ##     10177     10178     10179     10180     10181     10182     10183     10184 
    ##  8.968104  8.233544 11.381656  8.548356 12.431027  9.282915  9.597726  7.603922 
    ##     10185     10186     10187     10188     10189     10190     10191     10192 
    ## 10.017474 12.116216 11.381656  8.128607 11.591530  8.023670 11.381656 11.696467 
    ##     10193     10194     10195     10196     10197     10198     10199     10200 
    ## 11.801404  9.702663 10.437223  8.023670  7.603922 11.066845  9.282915 13.060649 
    ##     10201     10202     10203     10204     10205     10206     10207     10208 
    ## 12.535964  8.548356 12.535964 13.165586 12.640901  9.702663  7.079237 13.060649 
    ##     10209     10210     10211     10212     10213     10214     10215     10216 
    ##  8.758230  8.443419 10.227349 12.326090 10.437223  9.807600 10.542160 12.955712 
    ##     10217     10218     10219     10220     10221     10222     10223     10224 
    ##  8.863167  9.492789  7.079237 10.542160  8.653293 11.591530 10.752034 12.221153 
    ##     10225     10226     10227     10228     10229     10230     10231     10232 
    ## 10.856971 10.227349 10.437223 10.856971 11.171782  9.702663  9.073041 12.011279 
    ##     10233     10234     10235     10236     10237     10238     10239     10240 
    ## 11.696467 10.542160 12.221153 10.122412  9.702663  8.653293 11.381656  8.548356 
    ##     10241     10242     10243     10244     10245     10246     10247     10248 
    ##  7.079237  7.708859  9.177978  9.177978  7.603922 12.850775  9.387852 10.122412 
    ##     10249     10250     10251     10252     10253     10254     10255     10256 
    ##  7.708859 11.276719 11.381656  8.338482  7.708859  7.079237 10.017474  7.813796 
    ##     10257     10258     10259     10260     10261     10262     10263     10264 
    ##  8.128607 12.221153  9.282915 10.122412  9.492789 12.221153  9.387852  9.177978 
    ##     10265     10266     10267     10268     10269     10270     10271     10272 
    ##  8.863167  9.912537  7.603922 12.955712  9.807600  8.023670 12.011279 10.961908 
    ##     10273     10274     10275     10276     10277     10278     10279     10280 
    ##  8.653293  8.758230  8.968104  8.233544  8.443419  8.548356 11.906342  7.603922 
    ##     10281     10282     10283     10284     10285     10286     10287     10288 
    ## 11.591530 10.122412 10.437223  9.597726 10.856971 11.066845 12.431027 13.060649 
    ##     10289     10290     10291     10292     10293     10294     10295     10296 
    ##  8.338482  9.492789  7.079237 11.591530 12.116216 11.381656  7.079237  7.918733 
    ##     10297     10298     10299     10300     10301     10302     10303     10304 
    ## 11.696467 10.961908  8.128607 12.640901  9.282915  9.177978 14.424831 13.795209 
    ##     10305     10306     10307     10308     10309     10310     10311     10312 
    ## 13.795209 13.480397 13.480397 13.585334 13.900146 13.690271 14.424831 14.319894 
    ##     10313     10314     10315     10316     10317     10318     10319     10320 
    ## 14.005083 14.005083 13.375460 13.585334 13.900146 13.585334 14.005083 14.110020 
    ##     10321     10322     10323     10324     10325     10326     10327     10328 
    ## 14.319894 14.214957 14.110020 14.005083 13.585334 13.795209 14.005083 13.690271 
    ##     10329     10330     10331     10332     10333     10334     10335     10336 
    ## 13.585334 13.585334 13.795209 14.424831 14.110020 14.005083 13.690271 14.214957 
    ##     10337     10338     10339     10340     10341     10342     10343     10344 
    ## 13.690271 13.480397 14.214957 14.319894 14.214957 13.480397 14.110020 13.585334 
    ##     10345     10346     10347     10348     10349     10350     10351     10352 
    ## 14.110020 11.801404  7.079237  8.128607 13.165586 11.171782  9.492789 12.116216 
    ##     10353     10354     10355     10356     10357     10358     10359     10360 
    ##  8.968104 10.856971  9.492789 12.745838 12.850775 11.171782  8.548356 11.486593 
    ##     10361     10362     10363     10364     10365     10366     10367     10368 
    ##  9.177978 12.221153  7.603922  8.338482  8.233544  8.863167  8.338482  8.443419 
    ##     10369     10370     10371     10372     10373     10374     10375     10376 
    ## 10.017474  8.653293 12.955712 10.437223  8.968104  9.702663 12.955712  8.968104 
    ##     10377     10378     10379     10380     10381     10382     10383     10384 
    ##  8.128607 11.381656  9.387852 12.221153  7.603922 11.276719  9.177978  8.758230 
    ##     10385     10386     10387     10388     10389     10390     10391     10392 
    ##  9.492789  8.233544 10.647097 12.326090 11.486593  7.603922  9.387852  8.023670 
    ##     10393     10394     10395     10396     10397     10398     10399     10400 
    ##  9.702663 12.011279  9.492789  9.912537 11.486593 12.011279  8.548356  8.338482 
    ##     10401     10402     10403     10404     10405     10406     10407     10408 
    ## 11.486593 11.276719  9.177978 11.591530 12.431027 12.221153 10.542160 13.060649 
    ##     10409     10410     10411     10412     10413     10414     10415     10416 
    ##  9.807600 11.381656  8.338482  8.653293 12.535964  8.968104  9.177978  8.443419 
    ##     10417     10418     10419     10420     10421     10422     10423     10424 
    ##  8.233544 10.961908 13.165586  9.282915 11.696467  8.128607 13.060649  8.758230 
    ##     10425     10426     10427     10428     10429     10430     10431     10432 
    ##  8.758230  9.282915 10.647097 10.017474  8.653293  9.912537 11.171782  9.177978 
    ##     10433     10434     10435     10436     10437     10438     10439     10440 
    ##  9.912537  9.073041 10.017474  8.443419  8.653293  7.603922  8.758230 13.165586 
    ##     10441     10442     10443     10444     10445     10446     10447     10448 
    ## 10.856971  9.492789 10.122412  9.282915 10.961908 11.381656  8.968104 12.221153 
    ##     10449     10450     10451     10452     10453     10454     10455     10456 
    ## 11.171782 11.066845 11.906342 10.437223 12.535964  8.023670 10.437223 13.165586 
    ##     10457     10458     10459     10460     10461     10462     10463     10464 
    ## 11.066845 12.850775 13.060649  8.128607 12.011279  7.079237  9.073041 12.745838 
    ##     10465     10466     10467     10468     10469     10470     10471     10472 
    ## 10.856971 10.332286  9.073041  7.079237 10.542160  7.708859 11.381656 12.431027 
    ##     10473     10474     10475     10476     10477     10478     10479     10480 
    ## 10.437223  7.603922  9.282915 10.122412  7.603922 11.486593 10.332286  9.702663 
    ##     10481     10482     10483     10484     10485     10486     10487     10488 
    ## 12.640901 12.011279 10.856971 12.850775 12.431027  8.653293  7.079237 10.332286 
    ##     10489     10490     10491     10492     10493     10494     10495     10496 
    ##  9.387852  9.177978  7.918733 11.591530  9.597726 11.591530  7.708859  8.128607 
    ##     10497     10498     10499     10500     10501     10502     10503     10504 
    ##  9.282915  9.387852 10.856971 10.017474  8.233544 11.591530 12.326090 12.640901 
    ##     10505     10506     10507     10508     10509     10510     10511     10512 
    ## 10.542160  9.073041  9.177978  8.548356 12.011279 12.116216 11.906342 11.276719 
    ##     10513     10514     10515     10516     10517     10518     10519     10520 
    ##  8.338482 11.696467  9.387852  8.863167  8.548356 10.332286 12.221153  7.079237 
    ##     10521     10522     10523     10524     10525     10526     10527     10528 
    ##  9.492789 10.752034  9.282915  9.492789  8.548356 12.850775  8.653293 12.850775 
    ##     10529     10530     10531     10532     10533     10534     10535     10536 
    ## 10.647097  7.603922  9.177978 13.270523  9.702663 10.437223 11.591530  8.653293 
    ##     10537     10538     10539     10540     10541     10542     10543     10544 
    ## 12.535964 10.122412  8.863167  8.758230  8.443419 11.276719 12.745838  9.073041 
    ##     10545     10546     10547     10548     10549     10550     10551     10552 
    ##  7.708859  9.073041 12.640901  7.708859  8.968104 12.011279 13.270523 12.221153 
    ##     10553     10554     10555     10556     10557     10558     10559     10560 
    ##  7.079237 11.381656 12.221153  7.079237  8.653293 10.752034 12.116216 10.017474 
    ##     10561     10562     10563     10564     10565     10566     10567     10568 
    ## 12.431027  8.968104 11.276719 12.011279  8.338482  9.387852 12.745838  8.653293 
    ##     10569     10570     10571     10572     10573     10574     10575     10576 
    ## 12.116216  8.653293 12.850775 12.431027 13.060649 10.856971 11.276719 12.535964 
    ##     10577     10578     10579     10580     10581     10582     10583     10584 
    ## 11.906342  8.863167 10.332286 13.270523 12.221153 11.066845 10.332286 12.535964 
    ##     10585     10586     10587     10588     10589     10590     10591     10592 
    ## 12.326090  8.968104 11.591530  7.603922  9.492789  9.073041  9.177978 12.535964 
    ##     10593     10594     10595     10596     10597     10598     10599     10600 
    ##  8.443419 10.647097 12.326090 12.431027  9.492789  8.758230  9.492789  8.968104 
    ##     10601     10602     10603     10604     10605     10606     10607     10608 
    ##  8.653293 10.017474  9.702663  8.863167 12.850775  8.968104  9.492789  8.233544 
    ##     10609     10610     10611     10612     10613     10614     10615     10616 
    ##  7.813796 11.696467 11.381656 10.542160  9.282915  9.282915  9.702663  7.079237 
    ##     10617     10618     10619     10620     10621     10622     10623     10624 
    ##  9.177978 10.542160  7.603922 10.856971 12.011279 12.221153  8.968104  7.079237 
    ##     10625     10626     10627     10628     10629     10630     10631     10632 
    ##  8.863167 13.270523 10.961908 12.431027  8.548356 12.116216 11.066845 10.647097 
    ##     10633     10634     10635     10636     10637     10638     10639     10640 
    ## 10.542160 11.591530 11.171782  9.073041 12.955712 10.122412  9.492789 12.011279 
    ##     10641     10642     10643     10644     10645     10646     10647     10648 
    ##  8.548356 11.696467  8.548356  9.702663  7.603922  9.807600 11.591530  7.603922 
    ##     10649     10650     10651     10652     10653     10654     10655     10656 
    ##  8.443419  8.548356  9.492789  9.492789  7.918733 12.535964 12.011279 11.276719 
    ##     10657     10658     10659     10660     10661     10662     10663     10664 
    ## 10.017474 10.227349  7.813796 11.066845  8.338482  8.128607 11.276719 12.116216 
    ##     10665     10666     10667     10668     10669     10670     10671     10672 
    ##  8.863167 11.381656 10.017474 10.856971 11.906342  8.548356 10.961908  9.807600 
    ##     10673     10674     10675     10676     10677     10678     10679     10680 
    ##  8.653293 11.591530  9.282915 11.801404 12.850775 10.227349  8.233544 11.276719 
    ##     10681     10682     10683     10684     10685     10686     10687     10688 
    ## 11.591530 10.647097 12.955712  8.233544  7.603922 10.961908  9.702663 12.326090 
    ##     10689     10690     10691     10692     10693     10694     10695     10696 
    ## 12.116216  7.079237 10.752034 10.122412  8.653293 12.640901 10.017474 12.011279 
    ##     10697     10698     10699     10700     10701     10702     10703     10704 
    ## 10.437223  9.177978  9.387852  9.597726  7.603922 12.011279 12.955712 10.437223 
    ##     10705     10706     10707     10708     10709     10710     10711     10712 
    ##  7.079237 10.647097  9.702663 12.745838  7.918733  9.492789  9.073041  8.758230 
    ##     10713     10714     10715     10716     10717     10718     10719     10720 
    ## 12.850775  7.603922 12.011279 11.696467 11.801404 10.542160  9.073041 10.017474 
    ##     10721     10722     10723     10724     10725     10726     10727     10728 
    ## 10.752034  9.387852  9.282915 11.276719  8.968104 10.122412  7.918733 13.060649 
    ##     10729     10730     10731     10732     10733     10734     10735     10736 
    ##  7.603922  9.387852 10.227349  9.597726  9.912537  7.603922 10.752034  8.758230 
    ##     10737     10738     10739     10740     10741     10742     10743     10744 
    ## 11.696467  9.177978  9.177978 10.542160  9.492789  8.443419 12.431027  7.603922 
    ##     10745     10746     10747     10748     10749     10750     10751     10752 
    ##  8.023670  9.597726  7.918733 11.486593  8.548356  9.492789  7.603922 11.906342 
    ##     10753     10754     10755     10756     10757     10758     10759     10760 
    ## 10.647097 11.066845  9.387852 10.437223 10.437223 10.647097 11.276719 12.116216 
    ##     10761     10762     10763     10764     10765     10766     10767     10768 
    ## 12.535964 11.381656 12.955712 10.542160  7.918733 13.060649 11.066845 12.640901 
    ##     10769     10770     10771     10772     10773     10774     10775     10776 
    ##  8.443419 10.227349  9.282915  9.073041 12.221153 10.332286  7.603922  8.548356 
    ##     10777     10778     10779     10780     10781     10782     10783     10784 
    ##  8.128607 11.066845 10.856971  8.548356 12.326090 10.647097  7.603922 11.591530 
    ##     10785     10786     10787     10788     10789     10790     10791     10792 
    ##  9.492789 12.326090 10.542160 11.276719 12.850775  8.863167 10.856971  7.079237 
    ##     10793     10794     10795     10796     10797     10798     10799     10800 
    ##  8.653293  8.968104  8.863167 13.060649  8.968104 12.011279 11.591530 12.535964 
    ##     10801     10802     10803     10804     10805     10806     10807     10808 
    ## 13.165586  7.603922  8.653293 10.122412 11.801404 12.850775 11.906342  9.807600 
    ##     10809     10810     10811     10812     10813     10814     10815     10816 
    ## 12.745838  9.702663 12.850775 11.171782 13.060649 12.955712  7.603922 11.696467 
    ##     10817     10818     10819     10820     10821     10822     10823     10824 
    ## 10.017474  9.702663 10.332286 10.961908 12.221153 12.326090 12.640901  8.653293 
    ##     10825     10826     10827     10828     10829     10830     10831     10832 
    ##  7.603922 13.165586  9.387852  9.597726  7.918733  8.128607  8.233544 13.270523 
    ##     10833     10834     10835     10836     10837     10838     10839     10840 
    ##  8.128607 13.270523  8.863167  8.863167 10.122412 10.227349  9.073041 12.535964 
    ##     10841     10842     10843     10844     10845     10846     10847     10848 
    ## 12.431027  8.968104  8.548356 12.326090 12.850775  8.128607 12.745838 11.066845 
    ##     10849     10850     10851     10852     10853     10854     10855     10856 
    ##  9.387852 11.486593  8.233544 10.437223 12.116216 11.906342 12.116216  9.282915 
    ##     10857     10858     10859     10860     10861     10862     10863     10864 
    ##  9.807600  8.758230 11.171782 11.171782  8.443419 10.961908  8.968104 11.801404 
    ##     10865     10866     10867     10868     10869     10870     10871     10872 
    ## 11.276719 11.696467  8.653293  7.603922 13.060649  9.702663 14.214957 14.005083 
    ##     10873     10874     10875     10876     10877     10878     10879     10880 
    ## 13.480397 13.375460 13.480397 13.480397 14.214957 13.480397 13.165586  7.079237 
    ##     10881     10882     10883     10884     10885     10886     10887     10888 
    ## 12.431027  8.968104 12.640901 11.801404  7.603922  7.603922  7.079237 10.542160 
    ##     10889     10890     10891     10892     10893     10894     10895     10896 
    ##  8.233544 11.276719  9.702663 12.221153  7.079237  8.968104  7.603922  9.177978 
    ##     10897     10898     10899     10900     10901     10902     10903     10904 
    ## 12.326090  8.863167  8.023670  7.079237 10.647097 10.647097 11.276719 12.535964 
    ##     10905     10906     10907     10908     10909     10910     10911     10912 
    ## 10.017474  9.807600  9.597726  8.128607  8.548356  8.863167 12.326090 12.431027 
    ##     10913     10914     10915     10916     10917     10918     10919     10920 
    ## 10.437223  7.918733  8.653293  8.653293 12.640901  9.702663  7.603922 12.011279 
    ##     10921     10922     10923     10924     10925     10926     10927     10928 
    ##  9.177978 12.640901  7.079237 12.640901  9.912537  9.177978 12.011279  9.387852 
    ##     10929     10930     10931     10932     10933     10934     10935     10936 
    ## 11.171782  9.177978 13.165586  8.653293 12.221153 12.326090 12.640901  9.702663 
    ##     10937     10938     10939     10940     10941     10942     10943     10944 
    ## 10.437223  9.912537 10.752034 12.326090 13.060649 10.437223  8.968104  8.758230 
    ##     10945     10946     10947     10948     10949     10950     10951     10952 
    ##  8.233544  9.387852  8.653293 10.752034 10.647097 10.647097 11.906342 12.221153 
    ##     10953     10954     10955     10956     10957     10958     10959     10960 
    ## 12.326090 11.906342 13.165586  7.079237  8.968104 10.961908 10.332286  8.338482 
    ##     10961     10962     10963     10964     10965     10966     10967     10968 
    ##  8.443419  8.968104  8.863167 12.326090 12.011279  9.177978  7.079237 10.017474 
    ##     10969     10970     10971     10972     10973     10974     10975     10976 
    ## 10.017474 12.850775 10.961908 12.745838  9.387852 12.850775 14.319894 14.005083 
    ##     10977     10978     10979     10980     10981     10982     10983     10984 
    ## 12.640901  9.492789 10.856971 12.116216 11.066845 11.906342  8.863167 13.480397 
    ##     10985     10986     10987     10988     10989     10990     10991     10992 
    ## 13.165586 10.122412 10.752034  8.758230 13.585334 14.319894 14.110020 13.480397 
    ##     10993     10994     10995     10996     10997     10998     10999     11000 
    ## 14.214957 13.375460 14.214957 14.110020 13.690271 13.165586 11.906342 12.221153 
    ##     11001     11002     11003     11004     11005     11006     11007     11008 
    ##  8.023670  7.813796 11.486593  9.912537 13.270523 10.437223 12.535964 10.961908 
    ##     11009     11010     11011     11012     11013     11014     11015     11016 
    ## 10.856971  7.603922 10.122412 11.801404 12.326090  9.282915 10.017474 11.381656 
    ##     11017     11018     11019     11020     11021     11022     11023     11024 
    ## 10.856971 10.332286 11.381656 11.696467  7.708859  8.863167 12.535964 12.116216 
    ##     11025     11026     11027     11028     11029     11030     11031     11032 
    ## 13.270523  8.653293  7.603922  7.603922 12.431027  9.282915 13.375460 11.591530 
    ##     11033     11034     11035     11036     11037     11038     11039     11040 
    ## 11.906342 12.116216 13.060649 12.116216  9.807600 13.060649  8.443419 10.961908 
    ##     11041     11042     11043     11044     11045     11046     11047     11048 
    ##  9.597726 12.745838 10.647097 12.431027 10.122412 11.696467 11.171782 12.745838 
    ##     11049     11050     11051     11052     11053     11054     11055     11056 
    ## 10.332286  9.702663 12.745838 11.906342 13.270523 13.060649  8.023670 10.437223 
    ##     11057     11058     11059     11060     11061     11062     11063     11064 
    ## 11.486593 10.752034 12.745838 12.535964  9.492789  7.603922  9.597726 13.060649 
    ##     11065     11066     11067     11068     11069     11070     11071     11072 
    ## 12.116216 10.437223 12.850775 12.221153 12.116216 11.171782 10.332286 10.332286 
    ##     11073     11074     11075     11076     11077     11078     11079     11080 
    ##  8.653293  9.912537 12.535964 11.906342 12.431027  8.968104 10.122412 12.011279 
    ##     11081     11082     11083     11084     11085     11086     11087     11088 
    ##  9.282915  8.128607  9.807600 11.696467 11.696467 11.801404 12.221153 10.017474 
    ##     11089     11090     11091     11092     11093     11094     11095     11096 
    ## 12.431027 12.116216 13.270523 11.591530 12.116216 11.171782 12.221153 12.116216 
    ##     11097     11098     11099     11100     11101     11102     11103     11104 
    ## 11.801404 13.375460 10.856971 13.795209 14.319894 13.795209 13.795209 14.110020 
    ##     11105     11106     11107     11108     11109     11110     11111     11112 
    ## 13.585334 14.424831 13.690271 14.319894 14.319894 14.424831 14.110020 13.690271 
    ##     11113     11114     11115     11116     11117     11118     11119     11120 
    ## 13.480397 12.116216 10.017474 12.535964  7.603922 11.276719  7.708859 12.221153 
    ##     11121     11122     11123     11124     11125     11126     11127     11128 
    ## 11.066845 10.961908 10.542160  9.282915  9.387852 12.745838 10.122412  7.079237 
    ##     11129     11130     11131     11132     11133     11134     11135     11136 
    ## 11.591530  7.079237 12.431027 12.535964  8.443419  8.968104 13.270523 12.326090 
    ##     11137     11138     11139     11140     11141     11142     11143     11144 
    ##  9.177978  7.079237 12.326090  9.702663  9.177978  8.338482  9.597726  9.177978 
    ##     11145     11146     11147     11148     11149     11150     11151     11152 
    ## 13.480397  8.548356  9.597726 11.801404 12.326090 13.060649 12.745838  9.912537 
    ##     11153     11154     11155     11156     11157     11158     11159     11160 
    ## 10.647097 11.696467 12.116216 11.381656  9.073041 10.017474 12.431027 12.640901 
    ##     11161     11162     11163     11164     11165     11166     11167     11168 
    ##  9.702663  9.177978 10.437223 12.221153 11.486593  7.079237 11.696467  9.387852 
    ##     11169     11170     11171     11172     11173     11174     11175     11176 
    ## 12.535964 12.116216 11.696467 11.801404  8.968104 10.332286  9.807600  9.282915 
    ##     11177     11178     11179     11180     11181     11182     11183     11184 
    ## 11.171782 11.591530 11.381656 10.961908 12.326090 11.486593 10.227349 11.276719 
    ##     11185     11186     11187     11188     11189     11190     11191     11192 
    ##  7.708859 12.326090  9.177978 12.116216  9.177978  9.073041 10.961908  9.282915 
    ##     11193     11194     11195     11196     11197     11198     11199     11200 
    ## 10.961908  8.548356 12.221153  8.233544 12.326090  8.023670 10.017474  8.758230 
    ##     11201     11202     11203     11204     11205     11206     11207     11208 
    ## 10.647097  9.807600 11.276719  8.863167  7.813796  7.079237 11.591530 11.381656 
    ##     11209     11210     11211     11212     11213     11214     11215     11216 
    ## 14.214957 14.424831 13.690271 13.585334 14.214957 13.690271 14.214957 14.319894 
    ##     11217     11218     11219     11220     11221     11222     11223     11224 
    ## 14.214957 14.110020 14.424831 13.585334 14.005083 14.214957 13.690271 13.900146 
    ##     11225     11226     11227     11228     11229     11230     11231     11232 
    ## 14.214957 14.424831 14.214957 13.585334 14.424831 14.424831 14.005083 14.214957 
    ##     11233     11234     11235     11236     11237     11238     11239     11240 
    ## 13.375460 14.005083 14.214957 14.424831 13.900146 14.214957 14.424831 14.424831 
    ##     11241     11242     11243     11244     11245     11246     11247     11248 
    ## 14.424831 14.110020 12.535964 12.326090 10.961908 12.640901  8.968104  7.603922 
    ##     11249     11250     11251     11252     11253     11254     11255     11256 
    ## 12.221153 10.542160  8.863167 11.591530 10.856971  8.968104  8.128607 12.850775 
    ##     11257     11258     11259     11260     11261     11262     11263     11264 
    ## 13.375460  7.079237 13.270523  7.603922  9.597726  9.912537  8.758230 11.801404 
    ##     11265     11266     11267     11268     11269     11270     11271     11272 
    ## 10.437223  7.079237 12.850775  8.653293  8.443419 11.801404 11.906342 10.961908 
    ##     11273     11274     11275     11276     11277     11278     11279     11280 
    ## 11.276719  7.813796  8.548356  7.813796 12.745838 10.647097 11.696467  9.177978 
    ##     11281     11282     11283     11284     11285     11286     11287     11288 
    ##  7.079237  9.597726 13.270523 12.221153 11.696467 12.955712 11.486593  9.807600 
    ##     11289     11290     11291     11292     11293     11294     11295     11296 
    ##  9.912537 12.221153  7.708859 11.276719  8.968104  8.758230  9.807600 12.955712 
    ##     11297     11298     11299     11300     11301     11302     11303     11304 
    ##  8.443419 12.955712 11.696467  7.079237 11.276719 10.017474 11.801404 11.171782 
    ##     11305     11306     11307     11308     11309     11310     11311     11312 
    ## 12.745838 11.801404  9.177978  9.387852 12.535964 12.850775  7.918733 11.171782 
    ##     11313     11314     11315     11316     11317     11318     11319     11320 
    ##  7.603922 12.535964  7.603922 10.332286  9.387852 13.270523  9.807600  8.443419 
    ##     11321     11322     11323     11324     11325     11326     11327     11328 
    ##  8.338482  8.023670 12.535964  8.863167 11.486593  9.282915  8.758230 12.640901 
    ##     11329     11330     11331     11332     11333     11334     11335     11336 
    ##  8.863167  9.492789 13.060649  8.023670 10.437223  8.863167  8.338482  7.603922 
    ##     11337     11338     11339     11340     11341     11342     11343     11344 
    ## 10.227349  8.653293 10.542160  8.128607  7.603922 11.696467 11.696467 10.856971 
    ##     11345     11346     11347     11348     11349     11350     11351     11352 
    ##  7.918733  8.443419 12.640901 10.332286 13.060649 12.116216  7.603922 11.801404 
    ##     11353     11354     11355     11356     11357     11358     11359     11360 
    ##  9.492789  9.282915  7.918733  8.023670 12.745838  9.702663  8.023670 11.906342 
    ##     11361     11362     11363     11364     11365     11366     11367     11368 
    ## 11.801404 12.116216 12.431027 10.227349 12.535964 11.801404 12.640901  7.918733 
    ##     11369     11370     11371     11372     11373     11374     11375     11376 
    ## 10.122412 11.801404  8.758230  9.282915  8.968104 11.906342 11.801404 12.221153 
    ##     11377     11378     11379     11380     11381     11382     11383     11384 
    ## 10.856971 12.116216 10.647097  8.968104  9.597726 11.801404  9.282915  8.128607 
    ##     11385     11386     11387     11388     11389     11390     11391     11392 
    ## 10.437223  7.079237 12.431027 12.431027 11.171782 12.745838 13.165586  9.702663 
    ##     11393     11394     11395     11396     11397     11398     11399     11400 
    ## 12.535964 10.752034  9.492789 12.640901 13.375460 10.437223 11.696467 11.906342 
    ##     11401     11402     11403     11404     11405     11406     11407     11408 
    ##  9.492789 12.011279 10.961908 10.332286 12.221153  9.073041 11.801404 12.745838 
    ##     11409     11410     11411     11412     11413     11414     11415     11416 
    ## 12.850775  9.597726 10.961908  8.863167  7.603922 11.381656 10.227349 12.640901 
    ##     11417     11418     11419     11420     11421     11422     11423     11424 
    ##  7.603922 10.961908 12.850775 12.221153 12.850775 11.906342 11.906342  8.653293 
    ##     11425     11426     11427     11428     11429     11430     11431     11432 
    ##  8.863167 10.017474  8.758230  8.443419 10.542160 11.801404 11.906342 13.060649 
    ##     11433     11434     11435     11436     11437     11438     11439     11440 
    ##  8.863167 10.227349 12.535964 11.486593  8.863167  9.597726 11.591530 11.591530 
    ##     11441     11442     11443     11444     11445     11446     11447     11448 
    ## 10.122412 11.486593  9.073041 11.066845  9.387852  9.807600  8.548356  8.338482 
    ##     11449     11450     11451     11452     11453     11454     11455     11456 
    ##  9.912537 10.017474  8.548356 12.221153  9.807600 11.381656 11.696467  9.702663 
    ##     11457     11458     11459     11460     11461     11462     11463     11464 
    ##  8.653293 12.640901 10.542160 11.171782  9.073041 10.961908 10.227349  8.758230 
    ##     11465     11466     11467     11468     11469     11470     11471     11472 
    ## 11.801404 11.591530 11.486593  9.177978 10.017474 12.745838 11.591530  9.177978 
    ##     11473     11474     11475     11476     11477     11478     11479     11480 
    ##  8.863167  7.918733 10.752034 10.437223 10.647097 11.171782 12.326090 13.270523 
    ##     11481     11482     11483     11484     11485     11486     11487     11488 
    ## 11.381656  8.443419  7.603922  8.863167 10.542160 11.801404 10.647097 12.535964 
    ##     11489     11490     11491     11492     11493     11494     11495     11496 
    ## 11.276719 12.745838  9.492789  7.603922  9.387852 10.017474  7.708859 10.647097 
    ##     11497     11498     11499     11500     11501     11502     11503     11504 
    ##  8.548356  9.597726  7.603922  9.282915 13.060649  9.702663  8.863167  7.918733 
    ##     11505     11506     11507     11508     11509     11510     11511     11512 
    ## 12.640901 10.961908 10.122412 11.906342 10.647097  8.548356 10.332286  7.918733 
    ##     11513     11514     11515     11516     11517     11518     11519     11520 
    ## 12.535964 11.486593 12.326090 12.745838  8.758230 11.801404  7.603922  8.653293 
    ##     11521     11522     11523     11524     11525     11526     11527     11528 
    ##  7.603922 10.647097  7.603922 10.122412 12.535964  9.387852  7.079237 11.171782 
    ##     11529     11530     11531     11532     11533     11534     11535     11536 
    ##  8.968104 10.542160  9.597726 11.276719  9.177978  7.708859 10.856971  8.233544 
    ##     11537     11538     11539     11540     11541     11542     11543     11544 
    ## 12.955712  8.653293 12.850775 11.906342 12.221153 12.745838 12.745838 10.227349 
    ##     11545     11546     11547     11548     11549     11550     11551     11552 
    ## 10.752034 10.437223 10.332286  7.603922 12.431027 12.116216  7.603922 12.011279 
    ##     11553     11554     11555     11556     11557     11558     11559     11560 
    ##  7.813796  9.282915  8.548356 10.542160 11.066845 11.381656  9.492789  9.492789 
    ##     11561     11562     11563     11564     11565     11566     11567     11568 
    ##  7.079237  8.863167 12.431027 11.591530  8.548356  9.492789 11.486593  8.443419 
    ##     11569     11570     11571     11572     11573     11574     11575     11576 
    ##  8.233544 10.647097 12.745838 10.647097  9.597726  9.387852 12.221153 12.640901 
    ##     11577     11578     11579     11580     11581     11582     11583     11584 
    ##  8.653293 10.122412  7.918733  7.603922 12.745838  9.387852 10.961908  9.912537 
    ##     11585     11586     11587     11588     11589     11590     11591     11592 
    ## 11.171782 12.326090  9.282915  8.233544 12.535964  7.603922 10.437223  9.073041 
    ##     11593     11594     11595     11596     11597     11598     11599     11600 
    ## 12.326090 11.171782 12.535964 12.431027 12.431027  9.597726 10.437223 11.801404 
    ##     11601     11602     11603     11604     11605     11606     11607     11608 
    ##  7.603922 13.060649 10.542160 10.122412 11.276719  9.912537 11.906342 14.319894 
    ##     11609     11610     11611     11612     11613     11614     11615     11616 
    ## 10.437223  8.233544  8.443419  9.073041 13.585334 13.690271 13.690271 13.795209 
    ##     11617     11618     11619     11620     11621     11622     11623     11624 
    ## 13.795209 14.214957 14.214957 13.795209 14.110020 14.424831 14.214957 14.110020 
    ##     11625     11626     11627     11628     11629     11630     11631     11632 
    ## 14.319894 14.214957 13.480397 13.690271 14.214957 14.005083 14.214957 13.795209 
    ##     11633     11634     11635     11636     11637     11638     11639     11640 
    ## 13.690271 14.319894 14.319894 14.110020 13.480397 13.690271 13.690271 14.110020 
    ##     11641     11642     11643     11644     11645     11646     11647     11648 
    ## 13.585334 12.011279 10.332286  9.912537  7.079237  7.079237  7.079237  8.863167 
    ##     11649     11650     11651     11652     11653     11654     11655     11656 
    ##  9.492789  9.807600  9.282915 11.591530 10.961908 11.066845 11.801404  9.177978 
    ##     11657     11658     11659     11660     11661     11662     11663     11664 
    ##  9.912537  8.758230 10.437223  8.233544  8.023670  7.708859  9.597726 12.640901 
    ##     11665     11666     11667     11668     11669     11670     11671     11672 
    ## 10.647097 10.017474  9.912537  7.603922 11.066845  9.492789 10.332286 12.116216 
    ##     11673     11674     11675     11676     11677     11678     11679     11680 
    ## 12.431027  8.443419 12.221153  8.233544  8.233544 11.801404 12.745838  7.603922 
    ##     11681     11682     11683     11684     11685     11686     11687     11688 
    ##  9.387852 12.535964  9.282915 12.116216 10.122412 12.116216  8.233544  7.603922 
    ##     11689     11690     11691     11692     11693     11694     11695     11696 
    ## 11.906342  9.073041  9.702663 11.906342 11.171782  8.233544  8.443419 12.011279 
    ##     11697     11698     11699     11700     11701     11702     11703     11704 
    ## 11.696467 12.640901  9.387852 10.752034 12.431027 11.381656 11.696467 12.850775 
    ##     11705     11706     11707     11708     11709     11710     11711     11712 
    ## 11.486593  7.603922 12.850775  7.813796  8.863167  9.912537 10.437223 12.116216 
    ##     11713     11714     11715     11716     11717     11718     11719     11720 
    ## 12.326090  9.387852  7.813796  9.807600 13.375460 12.850775 10.856971  8.548356 
    ##     11721     11722     11723     11724     11725     11726     11727     11728 
    ##  7.603922 13.060649 13.270523 12.535964 11.696467 12.745838 11.066845 12.850775 
    ##     11729     11730     11731     11732     11733     11734     11735     11736 
    ##  7.708859 11.696467 11.486593  8.338482  8.548356  8.653293 13.165586 11.696467 
    ##     11737     11738     11739     11740     11741     11742     11743     11744 
    ##  8.653293  7.079237 11.696467 12.640901  9.807600  8.233544 11.591530  8.338482 
    ##     11745     11746     11747     11748     11749     11750     11751     11752 
    ##  8.443419  8.338482  9.177978 12.221153 12.535964  8.233544 12.116216  7.603922 
    ##     11753     11754     11755     11756     11757     11758     11759     11760 
    ##  7.603922 10.332286  8.653293 11.591530 11.276719 12.011279 12.326090  7.708859 
    ##     11761     11762     11763     11764     11765     11766     11767     11768 
    ## 12.116216  9.177978  8.758230 12.326090 12.116216  7.079237 11.906342 11.591530 
    ##     11769     11770     11771     11772     11773     11774     11775     11776 
    ##  9.073041  8.023670  8.758230  9.387852  8.863167  8.968104 11.381656 10.856971 
    ##     11777     11778     11779     11780     11781     11782     11783     11784 
    ##  8.968104 10.017474 11.591530 10.332286 11.276719 11.276719 10.122412 13.270523 
    ##     11785     11786     11787     11788     11789     11790     11791     11792 
    ## 11.801404  9.282915 10.017474 10.017474  8.338482  9.177978  8.758230  8.548356 
    ##     11793     11794     11795     11796     11797     11798     11799     11800 
    ##  8.233544 10.542160 12.326090 12.535964  7.813796 12.850775 11.696467 10.122412 
    ##     11801     11802     11803     11804     11805     11806     11807     11808 
    ## 11.906342  7.079237 12.431027 10.961908 12.850775 12.326090 12.745838  8.758230 
    ##     11809     11810     11811     11812     11813     11814     11815     11816 
    ## 12.640901  8.968104  8.968104  9.387852 13.270523  8.128607 12.221153  8.233544 
    ##     11817     11818     11819     11820     11821     11822     11823     11824 
    ##  9.597726  9.282915 11.171782  8.338482  9.492789  9.912537 11.591530 12.640901 
    ##     11825     11826     11827     11828     11829     11830     11831     11832 
    ##  7.079237 13.375460 10.017474 10.542160 10.017474 11.591530  8.443419 10.332286 
    ##     11833     11834     11835     11836     11837     11838     11839     11840 
    ## 11.066845 13.270523  8.338482 13.060649 12.116216 12.955712  9.807600 11.276719 
    ##     11841     11842     11843     11844     11845     11846     11847     11848 
    ## 12.431027 13.060649  8.968104  7.813796  8.443419 12.116216  9.177978  7.603922 
    ##     11849     11850     11851     11852     11853     11854     11855     11856 
    ##  8.443419 12.745838 12.535964  9.387852  8.863167  7.079237  7.079237  8.758230 
    ##     11857     11858     11859     11860     11861     11862     11863     11864 
    ## 13.375460 11.906342 12.326090 12.116216  7.708859 10.227349  8.023670  9.073041 
    ##     11865     11866     11867     11868     11869     11870     11871     11872 
    ## 12.116216  9.387852 10.017474 13.060649  8.548356  7.708859 11.801404 10.542160 
    ##     11873     11874     11875     11876     11877     11878     11879     11880 
    ## 11.906342  7.603922  9.807600 10.961908  9.597726  8.653293  7.918733  9.807600 
    ##     11881     11882     11883     11884     11885     11886     11887     11888 
    ##  9.177978 11.801404 12.116216  7.079237 11.486593 10.752034 10.122412 10.961908 
    ##     11889     11890     11891     11892     11893     11894     11895     11896 
    ##  9.387852  8.968104  9.807600 11.381656  9.387852 12.535964  9.387852  9.912537 
    ##     11897     11898     11899     11900     11901     11902     11903     11904 
    ##  8.233544 10.856971 10.227349 12.011279 12.955712  8.023670 11.696467  8.758230 
    ##     11905     11906     11907     11908     11909     11910     11911     11912 
    ## 10.122412 11.486593 13.060649  9.702663  8.758230 10.437223 13.480397 14.110020 
    ##     11913     11914     11915     11916     11917     11918     11919     11920 
    ## 14.110020 14.319894 14.319894 13.375460 13.375460 13.795209 14.319894 14.214957 
    ##     11921     11922     11923     11924     11925     11926     11927     11928 
    ## 14.110020 13.585334 13.795209 14.214957 14.214957 13.690271 14.319894 13.795209 
    ##     11929     11930     11931     11932     11933     11934     11935     11936 
    ## 14.319894  8.128607 12.011279  7.708859 12.221153 12.221153  7.603922 12.326090 
    ##     11937     11938     11939     11940     11941     11942     11943     11944 
    ## 11.801404 11.381656 12.850775 12.745838 10.961908  7.918733  7.708859  8.338482 
    ##     11945     11946     11947     11948     11949     11950     11951     11952 
    ##  9.177978  7.603922  9.807600 11.171782  9.177978  7.813796 10.017474 12.431027 
    ##     11953     11954     11955     11956     11957     11958     11959     11960 
    ##  8.443419  8.968104  8.863167 10.437223 10.122412 11.801404  7.079237  7.603922 
    ##     11961     11962     11963     11964     11965     11966     11967     11968 
    ##  9.492789 13.165586 10.332286 12.326090 10.437223  8.338482 12.431027  8.653293 
    ##     11969     11970     11971     11972     11973     11974     11975     11976 
    ##  7.918733 10.227349 10.752034 10.332286  8.338482 10.437223 10.017474  8.233544 
    ##     11977     11978     11979     11980     11981     11982     11983     11984 
    ##  7.918733 10.332286  8.863167 10.647097  7.079237 12.535964  9.073041 12.640901 
    ##     11985     11986     11987     11988     11989     11990     11991     11992 
    ##  7.918733  8.863167 11.906342 10.122412  9.912537 10.332286  9.702663 11.276719 
    ##     11993     11994     11995     11996     11997     11998     11999     12000 
    ## 11.066845 10.647097 10.961908  8.968104  9.492789 13.060649  9.177978  9.282915 
    ##     12001     12002     12003     12004     12005     12006     12007     12008 
    ##  8.443419  9.073041 11.801404 10.122412  9.597726 11.591530 13.060649  7.079237 
    ##     12009     12010     12011     12012     12013     12014     12015     12016 
    ## 10.752034  9.807600  7.079237  8.443419  9.702663  9.177978 13.480397 10.332286 
    ##     12017     12018     12019     12020     12021     12022     12023     12024 
    ## 11.276719  9.492789 12.326090 10.017474  9.282915  8.443419 12.535964 12.745838 
    ##     12025     12026     12027     12028     12029     12030     12031     12032 
    ##  8.338482  8.338482  8.338482 10.542160 11.591530 11.696467 11.381656  8.653293 
    ##     12033     12034     12035     12036     12037     12038     12039     12040 
    ##  9.282915 12.535964  7.813796 12.745838 13.375460  9.912537 13.270523  9.177978 
    ##     12041     12042     12043     12044     12045     12046     12047     12048 
    ## 12.745838  9.492789  9.912537  8.338482  7.603922 10.961908 10.752034 12.011279 
    ##     12049     12050     12051     12052     12053     12054     12055     12056 
    ## 12.326090 13.270523 13.270523  9.492789 10.856971 10.647097  8.128607 10.227349 
    ##     12057     12058     12059     12060     12061     12062     12063     12064 
    ##  7.603922 10.332286  9.387852 10.437223 11.171782  7.079237  8.023670  8.443419 
    ##     12065     12066     12067     12068     12069     12070     12071     12072 
    ##  8.863167  8.443419  9.912537 13.060649 12.535964 11.696467 12.116216 12.850775 
    ##     12073     12074     12075     12076     12077     12078     12079     12080 
    ##  8.338482  9.177978  9.492789 12.640901  8.863167  8.233544  9.702663 11.276719 
    ##     12081     12082     12083     12084     12085     12086     12087     12088 
    ##  9.177978 11.486593  8.023670 10.961908  7.603922  9.177978  9.807600 11.381656 
    ##     12089     12090     12091     12092     12093     12094     12095     12096 
    ##  8.863167  8.863167 10.752034 12.955712  8.338482  8.968104 12.431027 12.221153 
    ##     12097     12098     12099     12100     12101     12102     12103     12104 
    ##  9.702663 12.850775 13.375460 12.850775  9.282915 10.227349 12.011279 12.850775 
    ##     12105     12106     12107     12108     12109     12110     12111     12112 
    ##  9.177978 13.165586 12.011279 10.752034 10.017474  9.807600 10.961908 11.591530 
    ##     12113     12114     12115     12116     12117     12118     12119     12120 
    ##  8.548356  7.813796 10.647097 10.961908 10.961908 10.542160  8.968104 11.276719 
    ##     12121     12122     12123     12124     12125     12126     12127     12128 
    ##  8.128607 11.801404  9.282915  9.177978 11.696467  9.177978 11.066845 12.431027 
    ##     12129     12130     12131     12132     12133     12134     12135     12136 
    ## 11.696467 10.542160 12.955712 12.850775 11.591530  9.177978 11.591530 12.640901 
    ##     12137     12138     12139     12140     12141     12142     12143     12144 
    ## 12.640901  9.282915  8.548356 10.437223 11.801404 12.116216 12.431027  9.702663 
    ##     12145     12146     12147     12148     12149     12150     12151     12152 
    ## 11.591530 11.381656 11.591530  7.603922 12.640901 10.122412  8.758230 13.165586 
    ##     12153     12154     12155     12156     12157     12158     12159     12160 
    ## 12.326090 13.060649 11.066845  7.079237 11.801404 13.165586 12.640901  8.128607 
    ##     12161     12162     12163     12164     12165     12166     12167     12168 
    ## 11.486593  9.282915  8.758230  9.282915 10.752034 10.332286  9.073041  9.807600 
    ##     12169     12170     12171     12172     12173     12174     12175     12176 
    ## 10.542160 13.270523 12.745838 12.955712 11.066845  7.708859  8.443419 12.431027 
    ##     12177     12178     12179     12180     12181     12182     12183     12184 
    ## 10.856971 12.955712 11.801404 10.856971  9.073041 13.270523  7.603922 11.906342 
    ##     12185     12186     12187     12188     12189     12190     12191     12192 
    ##  9.807600  7.708859 12.011279 13.585334 13.375460 13.585334 13.585334 14.214957 
    ##     12193     12194     12195     12196     12197     12198     12199     12200 
    ##  8.758230 10.227349  8.443419 10.332286 10.437223 12.431027 12.011279 10.227349 
    ##     12201     12202     12203     12204     12205     12206     12207     12208 
    ## 10.122412 12.955712  7.708859 11.906342 10.122412 13.060649 12.640901 12.326090 
    ##     12209     12210     12211     12212     12213     12214     12215     12216 
    ##  9.597726  7.079237  8.653293  7.813796 12.011279  8.758230  8.653293 12.955712 
    ##     12217     12218     12219     12220     12221     12222     12223     12224 
    ##  8.863167  9.073041 11.381656 11.906342 10.227349 12.745838 10.647097  8.443419 
    ##     12225     12226     12227     12228     12229     12230     12231     12232 
    ##  9.702663  8.443419 11.486593  9.702663  8.968104 10.017474 10.961908 11.066845 
    ##     12233     12234     12235     12236     12237     12238     12239     12240 
    ##  9.387852  8.443419 12.221153  8.338482 10.227349 12.955712 10.437223  9.702663 
    ##     12241     12242     12243     12244     12245     12246     12247     12248 
    ##  8.023670  8.968104 10.332286 12.955712  9.492789 12.326090 12.535964 11.906342 
    ##     12249     12250     12251     12252     12253     12254     12255     12256 
    ##  9.073041 13.060649 10.647097  8.338482  8.338482 11.381656 12.955712  9.177978 
    ##     12257     12258     12259     12260     12261     12262     12263     12264 
    ##  8.863167 10.017474  8.653293  9.177978 10.961908  7.603922 13.900146 11.381656 
    ##     12265     12266     12267     12268     12269     12270     12271     12272 
    ## 11.276719 11.591530  9.597726  8.548356 10.122412 12.745838 13.165586 10.437223 
    ##     12273     12274     12275     12276     12277     12278     12279     12280 
    ## 10.227349 10.961908  9.807600 12.745838 12.955712  9.807600  8.233544  7.079237 
    ##     12281     12282     12283     12284     12285     12286     12287     12288 
    ## 14.319894 14.319894  8.128607  8.338482 11.066845  8.443419 12.431027  7.603922 
    ##     12289     12290     12291     12292     12293     12294     12295     12296 
    ##  7.603922 14.319894 14.319894 13.690271 13.900146 14.424831 13.480397 14.214957 
    ##     12297     12298     12299     12300     12301     12302     12303     12304 
    ## 14.005083 14.424831 13.480397 14.214957 14.214957 14.110020 13.585334 14.214957 
    ##     12305     12306     12307     12308     12309     12310     12311     12312 
    ## 13.585334 13.585334 13.900146 13.900146 14.319894 13.375460 14.319894 14.005083 
    ##     12313     12314     12315     12316     12317     12318     12319     12320 
    ## 13.585334 14.319894 13.690271 13.585334 13.585334 14.319894 14.424831 13.375460 
    ##     12321     12322     12323     12324     12325     12326     12327     12328 
    ## 14.214957 14.214957 14.110020 13.900146 14.110020 14.319894 12.640901  9.912537 
    ##     12329     12330     12331     12332     12333     12334     12335     12336 
    ##  9.597726 12.116216 10.017474  7.079237  7.708859  8.023670  9.073041 12.011279 
    ##     12337     12338     12339     12340     12341     12342     12343     12344 
    ##  8.023670 11.276719  8.443419 11.591530  9.282915 12.745838  7.918733 10.542160 
    ##     12345     12346     12347     12348     12349     12350     12351     12352 
    ## 10.332286  9.597726 10.122412  9.597726 13.270523 12.745838 12.955712 12.955712 
    ##     12353     12354     12355     12356     12357     12358     12359     12360 
    ## 10.227349  7.603922 11.696467  8.653293 10.332286 10.647097  9.073041 12.955712 
    ##     12361     12362     12363     12364     12365     12366     12367     12368 
    ## 10.542160 12.326090 11.801404 11.696467  9.177978 11.276719 11.486593 11.906342 
    ##     12369     12370     12371     12372     12373     12374     12375     12376 
    ##  8.128607 13.270523  7.079237 12.535964 12.640901  8.548356 11.066845  9.492789 
    ##     12377     12378     12379     12380     12381     12382     12383     12384 
    ##  9.282915 10.227349 11.486593 11.801404 10.227349  9.912537  9.702663  8.233544 
    ##     12385     12386     12387     12388     12389     12390     12391     12392 
    ## 11.696467 12.745838  9.597726  8.023670 11.171782 11.486593  8.338482  9.387852 
    ##     12393     12394     12395     12396     12397     12398     12399     12400 
    ##  8.968104 10.647097  7.708859 12.431027  8.443419 10.856971 11.066845  8.233544 
    ##     12401     12402     12403     12404     12405     12406     12407     12408 
    ##  9.387852 10.647097 10.647097  7.918733 10.227349  9.702663 12.116216 13.270523 
    ##     12409     12410     12411     12412     12413     12414     12415     12416 
    ## 11.591530 11.486593  7.603922  9.912537 11.906342  7.603922 12.955712 11.906342 
    ##     12417     12418     12419     12420     12421     12422     12423     12424 
    ##  9.073041  9.177978  9.912537 10.961908 12.955712 11.801404  9.492789 12.955712 
    ##     12425     12426     12427     12428     12429     12430     12431     12432 
    ## 11.906342 12.640901 11.696467 12.535964  9.492789  9.073041  9.492789  8.128607 
    ##     12433     12434     12435     12436     12437     12438     12439     12440 
    ## 11.486593 12.955712 11.486593  7.079237  7.708859 12.116216 11.276719  9.387852 
    ##     12441     12442     12443     12444     12445     12446     12447     12448 
    ## 10.647097  7.918733 10.856971 12.640901 11.276719 13.270523 11.696467 10.961908 
    ##     12449     12450     12451     12452     12453     12454     12455     12456 
    ##  9.807600  9.597726  8.758230 10.227349 12.116216  9.807600  9.282915  9.387852 
    ##     12457     12458     12459     12460     12461     12462     12463     12464 
    ##  8.023670 12.116216 11.486593  8.023670 12.011279  7.918733  8.548356  7.603922 
    ##     12465     12466     12467     12468     12469     12470     12471     12472 
    ##  9.912537  7.079237  9.492789 12.640901  8.863167 11.486593 11.381656 10.961908 
    ##     12473     12474     12475     12476     12477     12478     12479     12480 
    ## 11.696467  9.807600 12.011279  9.597726  9.387852 10.017474 11.171782 11.381656 
    ##     12481     12482     12483     12484     12485     12486     12487     12488 
    ## 12.011279  8.863167  7.079237 10.227349  8.233544  7.813796  9.492789 11.696467 
    ##     12489     12490     12491     12492     12493     12494     12495     12496 
    ##  9.702663  8.443419  7.079237 12.116216  7.603922 11.696467 11.066845 11.696467 
    ##     12497     12498     12499     12500     12501     12502     12503     12504 
    ##  8.758230 12.431027  8.653293 11.486593 12.221153  9.387852 10.437223 11.696467 
    ##     12505     12506     12507     12508     12509     12510     12511     12512 
    ## 10.856971 10.227349  8.023670  8.233544 12.535964 10.647097  9.073041  8.863167 
    ##     12513     12514     12515     12516     12517     12518     12519     12520 
    ## 10.856971  8.758230 11.801404 10.647097 10.332286 11.696467  8.863167  8.128607 
    ##     12521     12522     12523     12524     12525     12526     12527     12528 
    ## 10.227349  9.282915  8.443419 10.752034  7.813796  8.338482  9.807600 10.542160 
    ##     12529     12530     12531     12532     12533     12534     12535     12536 
    ## 10.437223 12.535964 11.066845  7.603922 13.270523  8.968104 11.696467 12.850775 
    ##     12537     12538     12539     12540     12541     12542     12543     12544 
    ##  8.338482 12.221153 11.801404  9.702663  8.968104  9.912537 12.955712 11.801404 
    ##     12545     12546     12547     12548     12549     12550     12551     12552 
    ##  8.968104 12.431027  8.863167  8.968104  9.912537  9.702663  9.492789 12.326090 
    ##     12553     12554     12555     12556     12557     12558     12559     12560 
    ## 11.801404  7.603922  9.387852 12.640901 10.227349 11.486593  9.073041 11.696467 
    ##     12561     12562     12563     12564     12565     12566     12567     12568 
    ##  9.597726  8.758230 12.326090  7.813796  8.863167 11.381656 12.955712  8.338482 
    ##     12569     12570     12571     12572     12573     12574     12575     12576 
    ##  7.079237 10.227349 12.431027 13.375460  7.918733 12.955712  7.079237 12.850775 
    ##     12577     12578     12579     12580     12581     12582     12583     12584 
    ##  8.548356  7.603922  9.807600  7.603922  7.918733 14.214957 13.690271 13.585334 
    ##     12585     12586     12587     12588     12589     12590     12591     12592 
    ## 14.319894 13.900146 14.110020 14.005083 14.319894 13.690271 14.319894 13.585334 
    ##     12593     12594     12595     12596     12597     12598     12599     12600 
    ## 14.005083 14.214957 14.005083 14.319894 14.110020 14.110020 14.214957 13.690271 
    ##     12601     12602     12603     12604     12605     12606     12607     12608 
    ## 13.690271 14.214957 13.690271 13.795209 13.585334 13.690271 14.214957 13.795209 
    ##     12609     12610     12611     12612     12613     12614     12615     12616 
    ## 13.480397 13.690271 13.900146 14.424831 14.110020 13.690271 13.795209 14.214957 
    ##     12617     12618     12619     12620     12621     12622     12623     12624 
    ## 14.319894 14.110020 14.214957 14.005083 12.640901  8.023670 10.856971 10.647097 
    ##     12625     12626     12627     12628     12629     12630     12631     12632 
    ##  8.653293  9.492789 12.535964 10.856971 11.696467  7.079237 12.011279 11.381656 
    ##     12633     12634     12635     12636     12637     12638     12639     12640 
    ## 10.227349 10.542160  8.758230 12.745838  7.079237 10.122412  7.603922  9.282915 
    ##     12641     12642     12643     12644     12645     12646     12647     12648 
    ##  7.918733 12.431027 12.011279 11.381656  7.918733  9.492789 10.856971  7.603922 
    ##     12649     12650     12651     12652     12653     12654     12655     12656 
    ## 10.332286 10.752034  7.603922 10.227349 10.437223 12.116216  9.912537 11.486593 
    ##     12657     12658     12659     12660     12661     12662     12663     12664 
    ##  8.023670  7.603922 11.486593  8.863167 10.017474  9.177978  7.813796 10.332286 
    ##     12665     12666     12667     12668     12669     12670     12671     12672 
    ## 12.011279 12.535964 12.326090  7.708859 11.381656 12.955712  7.603922  9.492789 
    ##     12673     12674     12675     12676     12677     12678     12679     12680 
    ## 11.276719 11.696467  8.023670 12.011279 10.437223  7.603922 10.437223  9.073041 
    ##     12681     12682     12683     12684     12685     12686     12687     12688 
    ## 10.856971  8.443419 10.332286  8.653293 11.066845  9.702663 10.961908 10.856971 
    ##     12689     12690     12691     12692     12693     12694     12695     12696 
    ##  9.177978 10.332286 12.850775  9.492789  8.863167  7.603922 11.906342  8.233544 
    ##     12697     12698     12699     12700     12701     12702     12703     12704 
    ##  8.443419  7.918733  8.443419 10.227349  9.177978 10.752034 13.165586  9.807600 
    ##     12705     12706     12707     12708     12709     12710     12711     12712 
    ## 12.535964 12.431027  7.079237 11.591530 12.745838  7.603922  8.128607 12.431027 
    ##     12713     12714     12715     12716     12717     12718     12719     12720 
    ## 10.332286 12.221153  8.968104  7.813796  9.912537 11.381656  8.023670 13.270523 
    ##     12721     12722     12723     12724     12725     12726     12727     12728 
    ##  9.282915  9.807600  7.603922 11.381656  8.443419  8.863167  9.702663  7.603922 
    ##     12729     12730     12731     12732     12733     12734     12735     12736 
    ## 12.221153  9.912537  9.492789 11.696467 13.270523  8.968104  8.653293 10.017474 
    ##     12737     12738     12739     12740     12741     12742     12743     12744 
    ## 10.122412  8.233544 10.647097  9.387852  7.603922 12.431027 12.850775  8.023670 
    ##     12745     12746     12747     12748     12749     12750     12751     12752 
    ## 13.165586 10.437223 13.375460  9.702663  7.813796 12.535964  9.073041 12.116216 
    ##     12753     12754     12755     12756     12757     12758     12759     12760 
    ## 10.647097 11.171782 11.906342 11.276719  7.918733 11.171782 10.227349 10.856971 
    ##     12761     12762     12763     12764     12765     12766     12767     12768 
    ##  9.387852  9.387852 12.535964 11.696467  9.387852 11.906342 11.486593 12.535964 
    ##     12769     12770     12771     12772     12773     12774     12775     12776 
    ## 12.640901 12.431027  8.443419  8.653293  9.702663  9.702663  9.807600 10.332286 
    ##     12777     12778     12779     12780     12781     12782     12783     12784 
    ##  9.387852  8.968104 11.801404  8.653293 12.850775 10.332286  9.387852 10.647097 
    ##     12785     12786     12787     12788     12789     12790     12791     12792 
    ## 11.381656 10.122412 11.906342 11.591530 13.165586 10.542160 10.122412 11.381656 
    ##     12793     12794     12795     12796     12797     12798     12799     12800 
    ##  8.443419 12.850775 13.060649  7.603922 12.640901 12.535964 12.535964 12.431027 
    ##     12801     12802     12803     12804     12805     12806     12807     12808 
    ## 12.116216 10.227349  9.807600 12.955712 12.011279 12.116216 12.955712 10.961908 
    ##     12809     12810     12811     12812     12813     12814     12815     12816 
    ## 10.961908 11.066845  8.863167 11.696467  8.023670  8.758230 12.431027  8.758230 
    ##     12817     12818     12819     12820     12821     12822     12823     12824 
    ## 13.165586 12.326090 11.486593 11.276719  7.918733 13.375460 12.850775 12.116216 
    ##     12825     12826     12827     12828     12829     12830     12831     12832 
    ##  9.282915 13.060649 11.696467 10.542160  9.912537 10.542160  9.282915  9.807600 
    ##     12833     12834     12835     12836     12837     12838     12839     12840 
    ##  9.282915  9.807600 11.276719  7.918733  7.079237  8.968104 10.437223  8.758230 
    ##     12841     12842     12843     12844     12845     12846     12847     12848 
    ## 12.221153 12.326090 11.486593  9.177978 11.906342 13.060649 10.647097  7.918733 
    ##     12849     12850     12851     12852     12853     12854     12855     12856 
    ##  7.603922 12.640901 13.270523  9.702663  8.023670  9.702663  9.912537 10.332286 
    ##     12857     12858     12859     12860     12861     12862     12863     12864 
    ## 10.437223 13.375460  9.282915  7.079237  9.807600 11.591530  8.233544  9.073041 
    ##     12865     12866     12867     12868     12869     12870     12871     12872 
    ##  7.708859 12.955712 13.165586 11.696467  8.863167  7.603922  8.443419 10.542160 
    ##     12873     12874     12875     12876     12877     12878     12879     12880 
    ##  9.912537 10.542160 12.011279 13.375460  9.702663  9.282915  7.813796 11.696467 
    ##     12881     12882     12883     12884     12885     12886     12887     12888 
    ##  8.758230 10.017474 11.801404  7.603922 12.535964 10.332286 11.591530  9.702663 
    ##     12889     12890     12891     12892     12893     12894     12895     12896 
    ## 11.696467  8.548356 10.332286 12.640901 11.801404  9.597726 10.752034  8.653293 
    ##     12897     12898     12899     12900     12901     12902     12903     12904 
    ##  8.968104 12.011279 11.066845  7.079237 11.906342 12.011279 12.116216  8.338482 
    ##     12905     12906     12907     12908     12909     12910     12911     12912 
    ## 12.431027  9.387852 10.961908  8.863167 12.745838 11.591530 11.381656 11.276719 
    ##     12913     12914     12915     12916     12917     12918     12919     12920 
    ## 11.486593  9.387852 12.221153  8.863167 12.431027 11.801404  9.387852  7.603922 
    ##     12921     12922     12923     12924     12925     12926     12927     12928 
    ## 10.961908  9.177978 13.270523  9.177978 11.906342 10.227349 10.332286 13.165586 
    ##     12929     12930     12931     12932     12933     12934     12935     12936 
    ## 12.640901  8.653293  8.338482 11.801404  8.128607  9.492789  7.079237 11.486593 
    ##     12937     12938     12939     12940     12941     12942     12943     12944 
    ##  8.653293 13.690271 11.276719 11.276719 11.486593  8.443419 12.326090  8.653293 
    ##     12945     12946     12947     12948     12949     12950     12951     12952 
    ##  7.079237  8.968104  9.387852  9.282915 11.066845 12.431027  7.708859 13.270523 
    ##     12953     12954     12955     12956     12957     12958     12959     12960 
    ##  9.597726 13.165586  9.702663 10.017474  8.338482 13.060649 10.542160  8.758230 
    ##     12961     12962     12963     12964     12965     12966     12967     12968 
    ##  7.603922  9.492789  8.968104  9.282915 11.171782 11.801404  9.492789  7.603922 
    ##     12969     12970     12971     12972     12973     12974     12975     12976 
    ##  8.653293 11.591530 12.535964 11.906342 12.011279  8.968104 13.270523 11.696467 
    ##     12977     12978     12979     12980     12981     12982     12983     12984 
    ## 10.017474  9.807600 10.437223  8.338482  8.758230 11.906342  8.548356  9.282915 
    ##     12985     12986     12987     12988     12989     12990     12991     12992 
    ##  8.653293 11.066845 10.961908  9.387852  9.912537  9.387852 11.381656  9.073041 
    ##     12993     12994     12995     12996     12997     12998     12999     13000 
    ## 12.955712 10.647097 10.542160 11.906342 11.276719  8.653293 11.381656  8.443419 
    ##     13001     13002     13003     13004     13005     13006     13007     13008 
    ## 11.066845 11.906342  7.603922  9.912537  8.758230 11.801404 10.332286 10.227349 
    ##     13009     13010     13011     13012     13013     13014     13015     13016 
    ## 11.906342  9.073041 12.850775 11.801404 11.801404 13.060649  9.492789 11.486593 
    ##     13017     13018     13019     13020     13021     13022     13023     13024 
    ##  9.597726  9.073041 12.011279  8.338482 12.431027  8.758230  8.758230  8.548356 
    ##     13025     13026     13027     13028     13029     13030     13031     13032 
    ## 12.640901  9.177978 10.752034  8.968104  9.702663  9.597726 12.640901 10.542160 
    ##     13033     13034     13035     13036     13037     13038     13039     13040 
    ##  8.758230  7.603922  7.603922 10.542160  9.177978 10.647097 11.276719  8.863167 
    ##     13041     13042     13043     13044     13045     13046     13047     13048 
    ## 11.906342  9.387852 12.955712  9.597726 11.486593 12.221153  9.282915  9.702663 
    ##     13049     13050     13051     13052     13053     13054     13055     13056 
    ## 11.276719  9.702663 11.486593 12.116216  8.758230 12.850775  9.807600 13.585334 
    ##     13057     13058     13059     13060     13061     13062     13063     13064 
    ##  9.282915  7.918733 12.640901 10.647097  7.918733  7.918733 11.486593 11.066845 
    ##     13065     13066     13067     13068     13069     13070     13071     13072 
    ## 10.961908 12.640901  7.079237 14.005083 13.480397 13.585334 13.480397 14.319894 
    ##     13073     13074     13075     13076     13077     13078     13079     13080 
    ## 13.480397 14.214957 14.005083 13.585334 13.795209 14.110020 13.795209 13.795209 
    ##     13081     13082     13083     13084     13085     13086     13087     13088 
    ## 14.319894  8.863167 13.165586  9.807600 10.542160 10.437223  9.597726 11.486593 
    ##     13089     13090     13091     13092     13093     13094     13095     13096 
    ##  8.023670  8.758230 11.486593  8.338482 10.227349 10.332286 11.381656  8.968104 
    ##     13097     13098     13099     13100     13101     13102     13103     13104 
    ##  8.548356  8.863167  9.702663  8.338482 13.060649  7.079237  7.079237 12.011279 
    ##     13105     13106     13107     13108     13109     13110     13111     13112 
    ## 12.011279 11.906342  7.918733 11.906342 11.591530 10.227349  8.548356 11.906342 
    ##     13113     13114     13115     13116     13117     13118     13119     13120 
    ## 10.542160  9.492789 10.542160 12.640901 11.696467 13.060649  8.548356  8.758230 
    ##     13121     13122     13123     13124     13125     13126     13127     13128 
    ## 12.326090 10.437223  8.443419 12.431027 10.122412  7.079237 11.276719 10.856971 
    ##     13129     13130     13131     13132     13133     13134     13135     13136 
    ## 13.060649  9.702663 11.801404  8.653293 12.745838  9.702663 12.745838 10.647097 
    ##     13137     13138     13139     13140     13141     13142     13143     13144 
    ##  9.492789  8.128607 10.961908  8.128607 13.165586  9.702663 11.381656 12.850775 
    ##     13145     13146     13147     13148     13149     13150     13151     13152 
    ## 11.486593 12.955712  8.443419  8.653293  8.653293 11.276719 11.486593 12.745838 
    ##     13153     13154     13155     13156     13157     13158     13159     13160 
    ##  9.282915  8.968104 11.066845 10.856971 12.955712  9.282915 11.906342  9.912537 
    ##     13161     13162     13163     13164     13165     13166     13167     13168 
    ## 10.017474  7.813796  8.758230 10.856971 10.856971  8.758230  8.128607 10.856971 
    ##     13169     13170     13171     13172     13173     13174     13175     13176 
    ##  8.863167 13.270523 10.752034  8.023670  8.338482  8.968104  8.443419  8.758230 
    ##     13177     13178     13179     13180     13181     13182     13183     13184 
    ## 11.486593  7.603922 10.332286  9.387852  9.807600 11.276719  8.338482  8.233544 
    ##     13185     13186     13187     13188     13189     13190     13191     13192 
    ## 12.326090 10.332286 10.542160 10.332286  7.708859 12.221153  8.128607 11.276719 
    ##     13193     13194     13195     13196     13197     13198     13199     13200 
    ##  8.548356  7.603922  7.603922 11.066845  9.597726  9.702663 10.227349 13.375460 
    ##     13201     13202     13203     13204     13205     13206     13207     13208 
    ## 10.647097  8.653293 10.017474  7.918733 11.801404 11.801404  9.702663  9.073041 
    ##     13209     13210     13211     13212     13213     13214     13215     13216 
    ##  9.912537 12.221153 11.801404  7.603922 10.961908  9.073041  9.073041  8.023670 
    ##     13217     13218     13219     13220     13221     13222     13223     13224 
    ## 11.696467 12.640901 12.011279 12.535964 11.696467  8.653293 12.745838  9.912537 
    ##     13225     13226     13227     13228     13229     13230     13231     13232 
    ##  8.023670  9.387852 13.060649 12.011279  7.813796 10.122412  9.387852 10.227349 
    ##     13233     13234     13235     13236     13237     13238     13239     13240 
    ##  8.023670 13.060649 10.647097  9.177978  7.603922 12.535964 11.486593 11.276719 
    ##     13241     13242     13243     13244     13245     13246     13247     13248 
    ## 11.486593 10.017474  9.492789  7.079237  7.603922  7.813796  9.702663 12.850775 
    ##     13249     13250     13251     13252     13253     13254     13255     13256 
    ##  8.233544 10.227349  7.813796  8.548356 12.745838  8.863167 10.227349  8.233544 
    ##     13257     13258     13259     13260     13261     13262     13263     13264 
    ##  8.758230  9.492789 12.955712 13.270523 11.381656 11.171782 12.955712 11.276719 
    ##     13265     13266     13267     13268     13269     13270     13271     13272 
    ## 12.850775 12.955712  8.023670  8.863167 12.431027  9.492789  8.443419 12.326090 
    ##     13273     13274     13275     13276     13277     13278     13279     13280 
    ##  8.548356 11.066845  8.233544 13.585334 14.424831 10.332286 12.326090  9.807600 
    ##     13281     13282     13283     13284     13285     13286     13287     13288 
    ##  9.073041  8.653293  9.912537 11.696467 10.122412 12.955712  8.233544  9.282915 
    ##     13289     13290     13291     13292     13293     13294     13295     13296 
    ## 10.332286 11.696467  9.912537 12.745838 12.850775 10.542160 11.906342 10.332286 
    ##     13297     13298     13299     13300     13301     13302     13303     13304 
    ## 10.647097  8.863167  7.813796  9.492789  7.079237  9.177978 11.066845  9.702663 
    ##     13305     13306     13307     13308     13309     13310     13311     13312 
    ## 13.480397 13.795209 13.585334 13.795209 13.795209 13.900146 14.005083 13.900146 
    ##     13313     13314     13315     13316     13317     13318     13319     13320 
    ## 13.690271 14.110020 13.690271 14.110020 13.585334 13.585334 13.690271 14.110020 
    ##     13321     13322     13323     13324     13325     13326     13327     13328 
    ## 14.214957 13.795209 11.276719 11.486593 12.011279 12.955712 12.221153  8.758230 
    ##     13329     13330     13331     13332     13333     13334     13335     13336 
    ## 11.696467 13.270523 10.856971 13.165586 11.591530 12.116216 12.955712  9.492789 
    ##     13337     13338     13339     13340     13341     13342     13343     13344 
    ## 12.535964 12.745838  8.758230 12.326090 12.011279 11.801404  7.603922 10.332286 
    ##     13345     13346     13347     13348     13349     13350     13351     13352 
    ## 11.696467  8.338482  9.387852  9.492789 12.326090  9.912537  7.918733 12.955712 
    ##     13353     13354     13355     13356     13357     13358     13359     13360 
    ## 11.066845 11.801404 12.221153  8.128607 11.486593 11.696467  8.443419 12.745838 
    ##     13361     13362     13363     13364     13365     13366     13367     13368 
    ## 12.011279 12.116216 11.066845 12.955712 11.906342  9.702663 12.116216 11.276719 
    ##     13369     13370     13371     13372     13373     13374     13375     13376 
    ##  9.282915 12.221153 11.801404  9.807600 11.801404  8.758230 13.060649 11.066845 
    ##     13377     13378     13379     13380     13381     13382     13383     13384 
    ## 12.745838  7.079237 12.850775 11.171782 12.011279 10.437223 11.381656  8.968104 
    ##     13385     13386     13387     13388     13389     13390     13391     13392 
    ## 11.906342 10.961908 12.326090  7.603922 10.647097 10.856971  9.807600 10.437223 
    ##     13393     13394     13395     13396     13397     13398     13399     13400 
    ## 11.276719 12.745838 11.801404 12.535964 10.856971  7.079237  8.128607 12.640901 
    ##     13401     13402     13403     13404     13405     13406     13407     13408 
    ## 12.431027 12.640901 10.752034  8.863167  8.548356 11.591530 10.122412 10.647097 
    ##     13409     13410     13411     13412     13413     13414     13415     13416 
    ##  9.702663 12.221153  8.653293 12.745838  7.603922  9.177978 10.437223 10.856971 
    ##     13417     13418     13419     13420     13421     13422     13423     13424 
    ##  8.653293 12.745838 10.437223 10.647097 12.850775  8.863167  9.387852 10.437223 
    ##     13425     13426     13427     13428     13429     13430     13431     13432 
    ## 11.906342 12.116216 10.017474  9.492789 11.381656 11.066845  9.597726 10.961908 
    ##     13433     13434     13435     13436     13437     13438     13439     13440 
    ##  8.443419 12.011279 12.116216 11.906342 12.955712 13.165586  8.968104 10.647097 
    ##     13441     13442     13443     13444     13445     13446     13447     13448 
    ## 11.381656 10.542160  8.548356  9.912537 13.270523 12.221153 11.171782  8.023670 
    ##     13449     13450     13451     13452     13453     13454     13455     13456 
    ##  9.177978 13.375460 12.431027 11.906342 10.122412  7.079237  9.387852 12.116216 
    ##     13457     13458     13459     13460     13461     13462     13463     13464 
    ##  8.758230 12.640901  9.282915 10.122412 11.906342 10.961908 11.906342  9.073041 
    ##     13465     13466     13467     13468     13469     13470     13471     13472 
    ## 10.752034 12.955712 12.640901 11.906342 10.752034  8.863167  8.128607  7.603922 
    ##     13473     13474     13475     13476     13477     13478     13479     13480 
    ## 13.375460 11.066845  8.863167  8.968104 13.375460  9.073041 13.270523  8.233544 
    ##     13481     13482     13483     13484     13485     13486     13487     13488 
    ## 10.856971 14.424831 13.480397 14.424831 14.214957 13.480397 14.319894 13.690271 
    ##     13489     13490     13491     13492     13493     13494     13495     13496 
    ## 14.110020 13.480397 14.319894 13.585334 13.585334 13.375460 14.214957 14.214957 
    ##     13497     13498     13499     13500     13501     13502     13503     13504 
    ## 14.110020 13.480397 13.480397 13.900146 13.585334 13.795209 14.424831 14.424831 
    ##     13505     13506     13507     13508     13509     13510     13511     13512 
    ## 14.424831 13.795209 13.795209 14.319894 13.900146 14.319894 14.424831 14.424831 
    ##     13513     13514     13515     13516     13517     13518     13519     13520 
    ## 14.005083 14.319894 13.585334 14.214957 14.319894 14.319894 13.480397 13.900146 
    ##     13521     13522     13523     13524     13525     13526     13527     13528 
    ## 13.585334 13.900146 14.319894 14.424831 14.424831 14.424831 10.961908 12.116216 
    ##     13529     13530     13531     13532     13533     13534     13535     13536 
    ## 11.696467  8.863167 10.961908  9.597726 12.955712 11.696467  9.807600 12.745838 
    ##     13537     13538     13539     13540     13541     13542     13543     13544 
    ## 10.227349  8.863167 10.752034 11.066845 13.165586  7.079237 10.332286  7.813796 
    ##     13545     13546     13547     13548     13549     13550     13551     13552 
    ##  7.918733  9.597726  7.918733 12.221153  9.492789  9.282915  9.702663 11.906342 
    ##     13553     13554     13555     13556     13557     13558     13559     13560 
    ## 10.122412 11.066845  8.023670  9.912537  8.128607 11.591530 11.801404  9.492789 
    ##     13561     13562     13563     13564     13565     13566     13567     13568 
    ## 11.801404 13.060649  9.492789  7.079237 10.122412  9.387852 10.017474 10.647097 
    ##     13569     13570     13571     13572     13573     13574     13575     13576 
    ##  8.758230 11.906342 11.486593 13.060649 12.116216 10.437223 11.276719  8.023670 
    ##     13577     13578     13579     13580     13581     13582     13583     13584 
    ##  8.653293  9.073041  8.548356 11.276719  7.813796  9.492789 10.542160 12.326090 
    ##     13585     13586     13587     13588     13589     13590     13591     13592 
    ## 11.906342  9.387852 10.647097 13.270523  7.603922  7.079237 13.375460 12.221153 
    ##     13593     13594     13595     13596     13597     13598     13599     13600 
    ##  7.603922  8.758230  8.023670 12.745838 10.437223 12.221153 12.326090  9.492789 
    ##     13601     13602     13603     13604     13605     13606     13607     13608 
    ## 11.906342 12.116216  7.918733 10.017474 11.276719 11.801404  9.492789 11.696467 
    ##     13609     13610     13611     13612     13613     13614     13615     13616 
    ##  7.708859 13.375460  8.758230  9.177978  8.863167  8.338482  8.233544 10.856971 
    ##     13617     13618     13619     13620     13621     13622     13623     13624 
    ## 12.431027  7.079237 10.122412  9.073041 10.437223  9.387852  9.387852 12.011279 
    ##     13625     13626     13627     13628     13629     13630     13631     13632 
    ##  7.079237 12.011279 10.856971  9.492789  9.912537 10.647097  9.702663 13.165586 
    ##     13633     13634     13635     13636     13637     13638     13639     13640 
    ## 12.221153  8.338482  8.653293 12.955712  9.177978 10.122412  9.702663 11.066845 
    ##     13641     13642     13643     13644     13645     13646     13647     13648 
    ##  7.079237 10.961908  8.863167  8.233544 12.431027  9.807600 10.542160  8.968104 
    ##     13649     13650     13651     13652     13653     13654     13655     13656 
    ## 11.171782 10.332286  9.492789 13.375460 11.906342  9.912537 11.066845  8.338482 
    ##     13657     13658     13659     13660     13661     13662     13663     13664 
    ## 10.752034 12.011279 12.326090 11.381656 10.647097 12.431027  8.548356 12.955712 
    ##     13665     13666     13667     13668     13669     13670     13671     13672 
    ## 12.640901  9.282915  9.702663  7.603922  7.603922 12.745838 11.906342 10.332286 
    ##     13673     13674     13675     13676     13677     13678     13679     13680 
    ## 10.122412 10.542160 13.165586  8.863167  9.912537  8.023670 10.227349 11.276719 
    ##     13681     13682     13683     13684     13685     13686     13687     13688 
    ## 10.122412 12.640901  8.758230 12.745838  9.807600  8.968104 10.752034 10.017474 
    ##     13689     13690     13691     13692     13693     13694     13695     13696 
    ##  7.918733  7.079237 12.116216  9.912537  8.338482 11.906342  9.492789 10.961908 
    ##     13697     13698     13699     13700     13701     13702     13703     13704 
    ## 11.696467  8.968104  8.128607 10.017474  9.177978 13.060649 12.535964 10.542160 
    ##     13705     13706     13707     13708     13709     13710     13711     13712 
    ## 11.801404  8.653293  7.813796 12.326090 12.955712 10.647097 13.270523 11.906342 
    ##     13713     13714     13715     13716     13717     13718     13719     13720 
    ## 11.171782 10.437223  8.968104 13.375460  9.177978 10.437223  8.863167 12.535964 
    ##     13721     13722     13723     13724     13725     13726     13727     13728 
    ##  9.177978  8.023670 10.017474  9.177978  8.128607  9.702663 12.326090  8.128607 
    ##     13729     13730     13731     13732     13733     13734     13735     13736 
    ## 12.011279 12.011279 11.276719 12.011279 13.165586 12.745838  7.079237 11.381656 
    ##     13737     13738     13739     13740     13741     13742     13743     13744 
    ##  9.387852 12.326090 12.221153 12.221153  8.758230  9.492789  8.548356 12.850775 
    ##     13745     13746     13747     13748     13749     13750     13751     13752 
    ## 12.955712  9.702663 13.060649 12.221153  9.073041 12.850775 12.745838 12.955712 
    ##     13753     13754     13755     13756     13757     13758     13759     13760 
    ##  9.073041 12.850775  7.603922 10.332286  8.548356 12.640901  8.128607 13.270523 
    ##     13761     13762     13763     13764     13765     13766     13767     13768 
    ## 13.165586 12.955712  9.912537  9.702663 12.745838 12.640901  9.807600  8.548356 
    ##     13769     13770     13771     13772     13773     13774     13775     13776 
    ## 10.542160 10.017474  8.233544 11.486593 11.276719 13.060649 11.591530  8.863167 
    ##     13777     13778     13779     13780     13781     13782     13783     13784 
    ## 10.961908  8.338482 12.850775 11.591530  8.023670  8.758230 11.696467 10.332286 
    ##     13785     13786     13787     13788     13789     13790     13791     13792 
    ## 12.955712 12.955712  9.912537  8.653293  8.653293  8.653293 12.221153 12.535964 
    ##     13793     13794     13795     13796     13797     13798     13799     13800 
    ## 12.640901  8.548356 12.011279  8.653293  9.807600 12.850775 10.752034  9.073041 
    ##     13801     13802     13803     13804     13805     13806     13807     13808 
    ## 11.486593  8.653293 10.961908  9.807600 11.591530  8.338482 13.375460  7.079237 
    ##     13809     13810     13811     13812     13813     13814     13815     13816 
    ## 11.486593 10.647097 13.375460 11.591530  9.702663  8.758230  8.443419 10.647097 
    ##     13817     13818     13819     13820     13821     13822     13823     13824 
    ##  9.073041 10.856971 10.437223  7.603922 12.326090 12.535964 10.752034  9.073041 
    ##     13825     13826     13827     13828     13829     13830     13831     13832 
    ## 12.116216 12.745838  7.079237 12.326090 12.011279  9.492789  8.968104  8.863167 
    ##     13833     13834     13835     13836     13837     13838     13839     13840 
    ##  9.387852 10.961908 12.221153 11.591530  7.918733  9.282915 11.066845  9.807600 
    ##     13841     13842     13843     13844     13845     13846     13847     13848 
    ## 12.745838 13.270523 12.850775  9.073041 13.060649 10.542160 12.326090 10.437223 
    ##     13849     13850     13851     13852     13853     13854     13855     13856 
    ##  9.282915 12.955712 11.906342  7.603922  8.023670  8.863167 12.850775 10.752034 
    ##     13857     13858     13859     13860     13861     13862     13863     13864 
    ## 11.801404 10.542160  8.233544  8.548356  9.597726 12.955712 10.017474 12.116216 
    ##     13865     13866     13867     13868     13869     13870     13871     13872 
    ##  9.702663 12.535964  7.603922  7.603922  7.603922 12.221153  9.387852 10.332286 
    ##     13873     13874     13875     13876     13877     13878     13879     13880 
    ## 12.535964 12.116216  8.128607  8.863167 10.437223 12.116216 12.326090 12.745838 
    ##     13881     13882     13883     13884     13885     13886     13887     13888 
    ## 12.326090  7.079237 12.221153  7.603922  7.603922 11.801404  9.387852 11.171782 
    ##     13889     13890     13891     13892     13893     13894     13895     13896 
    ## 10.752034  9.807600 12.116216 13.480397 11.276719  8.758230 12.640901  9.912537 
    ##     13897     13898     13899     13900     13901     13902     13903     13904 
    ## 11.276719 10.542160  9.807600  9.492789 12.221153 13.375460 12.850775  8.653293 
    ##     13905     13906     13907     13908     13909     13910     13911     13912 
    ## 12.431027  8.863167  9.597726 11.171782 12.640901  7.079237 12.955712  8.023670 
    ##     13913     13914     13915     13916     13917     13918     13919     13920 
    ##  8.338482 10.437223  9.282915  8.233544  8.758230  7.079237 11.276719  9.177978 
    ##     13921     13922     13923     13924     13925     13926     13927     13928 
    ##  9.073041 10.961908 10.332286  9.282915 10.647097  8.758230 10.437223 10.961908 
    ##     13929     13930     13931     13932     13933     13934     13935     13936 
    ##  8.548356 13.270523  9.702663 12.850775  7.603922 11.801404  8.863167 12.535964 
    ##     13937     13938     13939     13940     13941     13942     13943     13944 
    ## 11.591530 10.122412 10.752034  7.603922 11.906342  8.758230 10.332286  8.653293 
    ##     13945     13946     13947     13948     13949     13950     13951     13952 
    ## 12.221153  9.597726  8.968104  9.912537  9.177978  9.282915 12.221153 12.116216 
    ##     13953     13954     13955     13956     13957     13958     13959     13960 
    ##  8.023670  9.073041  7.079237  9.492789 12.955712  7.918733  8.968104 10.017474 
    ##     13961     13962     13963     13964     13965     13966     13967     13968 
    ## 10.647097 10.227349 12.535964 11.906342 13.375460 10.332286 13.270523  7.603922 
    ##     13969     13970     13971     13972     13973     13974     13975     13976 
    ##  9.597726 12.535964 11.381656 12.221153  8.338482 10.542160 13.165586  8.023670 
    ##     13977     13978     13979     13980     13981     13982     13983     13984 
    ##  8.338482  9.492789 11.486593 12.640901  9.912537  8.023670  9.807600  8.968104 
    ##     13985     13986     13987     13988     13989     13990     13991     13992 
    ##  9.597726 11.906342  9.177978  7.708859 11.591530 11.591530 11.066845  9.073041 
    ##     13993     13994     13995     13996     13997     13998     13999     14000 
    ##  9.597726  9.177978  9.073041 12.116216 11.906342 11.591530  7.708859  9.177978 
    ##     14001     14002     14003     14004     14005     14006     14007     14008 
    ##  9.282915  8.653293 12.955712 12.221153 11.906342 10.122412  7.079237 12.850775 
    ##     14009     14010     14011     14012     14013     14014     14015     14016 
    ##  8.968104 10.961908 10.961908 12.535964 10.961908  8.758230 12.535964 12.850775 
    ##     14017     14018     14019     14020     14021     14022     14023     14024 
    ## 11.801404 10.437223  7.079237  8.968104  8.548356 12.745838  7.813796 12.116216 
    ##     14025     14026     14027     14028     14029     14030     14031     14032 
    ## 12.745838 10.122412  8.338482  8.443419  9.597726  8.548356 12.745838 12.326090 
    ##     14033     14034     14035     14036     14037     14038     14039     14040 
    ## 10.647097 10.122412  9.912537  9.177978 10.122412 10.752034  8.758230  8.548356 
    ##     14041     14042     14043     14044     14045     14046     14047     14048 
    ##  8.548356 12.221153 10.122412 12.640901  7.603922 11.906342  8.758230  9.177978 
    ##     14049     14050     14051     14052     14053     14054     14055     14056 
    ## 10.332286 11.591530  9.702663 12.116216  7.813796  9.492789 11.906342  7.603922 
    ##     14057     14058     14059     14060     14061     14062     14063     14064 
    ## 11.801404  8.653293  7.918733 10.227349  9.807600 13.060649 11.591530 12.431027 
    ##     14065     14066     14067     14068     14069     14070     14071     14072 
    ## 12.745838  7.813796  7.603922 14.319894 14.424831 13.480397 13.900146 13.900146 
    ##     14073     14074     14075     14076     14077     14078     14079     14080 
    ## 13.585334 13.480397 13.480397 13.480397 14.424831  9.702663  7.079237 10.332286 
    ##     14081     14082     14083     14084     14085     14086     14087     14088 
    ## 10.437223 11.066845  9.492789 12.011279  7.079237  7.708859  9.282915 10.122412 
    ##     14089     14090     14091     14092     14093     14094     14095     14096 
    ##  7.603922  9.177978 11.906342  9.597726 10.961908 12.431027 11.171782 13.165586 
    ##     14097     14098     14099     14100     14101     14102     14103     14104 
    ##  9.177978  9.387852 11.906342 12.535964  8.443419  7.708859  9.597726  7.603922 
    ##     14105     14106     14107     14108     14109     14110     14111     14112 
    ## 10.961908 10.227349 12.745838  8.863167  8.338482  9.073041  8.023670  9.073041 
    ##     14113     14114     14115     14116     14117     14118     14119     14120 
    ## 11.066845 13.060649  7.603922  9.912537 12.221153 11.066845 11.906342  7.708859 
    ##     14121     14122     14123     14124     14125     14126     14127     14128 
    ##  7.603922  9.387852  9.073041  8.233544 12.326090 10.752034  8.338482 10.647097 
    ##     14129     14130     14131     14132     14133     14134     14135     14136 
    ##  8.758230  8.968104  9.702663  8.548356  8.443419  7.918733 11.801404 12.326090 
    ##     14137     14138     14139     14140     14141     14142     14143     14144 
    ##  7.079237  9.177978  8.233544  9.177978 12.326090  8.233544 10.961908 11.591530 
    ##     14145     14146     14147     14148     14149     14150     14151     14152 
    ##  7.813796  7.079237  9.282915  7.079237  7.603922  9.387852 10.856971 11.801404 
    ##     14153     14154     14155     14156     14157     14158     14159     14160 
    ## 10.647097  8.443419 10.332286 13.375460  8.758230 10.647097  9.177978  9.702663 
    ##     14161     14162     14163     14164     14165     14166     14167     14168 
    ## 11.696467  8.758230 11.696467  8.653293 14.424831 13.795209 13.900146 10.227349 
    ##     14169     14170     14171     14172     14173     14174     14175     14176 
    ##  9.912537 12.011279 10.017474  9.073041 10.227349 13.165586 10.961908 13.480397 
    ##     14177     14178     14179     14180     14181     14182     14183     14184 
    ##  7.708859 10.122412 13.690271 14.005083 14.424831 13.795209 14.424831 13.795209 
    ##     14185     14186     14187     14188     14189     14190     14191     14192 
    ## 11.171782  8.863167  9.597726 11.171782  8.968104 12.116216  9.177978  8.653293 
    ##     14193     14194     14195     14196     14197     14198     14199     14200 
    ## 10.437223  9.807600 10.122412 12.955712 11.171782  8.653293 11.801404 10.227349 
    ##     14201     14202     14203     14204     14205     14206     14207     14208 
    ## 10.227349 12.535964  7.708859  7.603922 11.906342 10.647097  9.702663  9.702663 
    ##     14209     14210     14211     14212     14213     14214     14215     14216 
    ##  9.807600  8.968104 10.542160 11.906342 12.640901  8.548356  8.128607 11.276719 
    ##     14217     14218     14219     14220     14221     14222     14223     14224 
    ## 12.116216 10.017474  9.597726 11.171782 12.535964  9.807600 12.640901 10.961908 
    ##     14225     14226     14227     14228     14229     14230     14231     14232 
    ## 11.276719 12.326090 10.542160 12.955712 10.332286  8.653293 12.116216  8.653293 
    ##     14233     14234     14235     14236     14237     14238     14239     14240 
    ## 11.591530 11.906342 10.752034 12.221153 10.647097 10.122412  9.282915 10.856971 
    ##     14241     14242     14243     14244     14245     14246     14247     14248 
    ## 11.276719 10.647097 12.850775  8.128607 13.165586  9.492789 10.961908  9.912537 
    ##     14249     14250     14251     14252     14253     14254     14255     14256 
    ## 10.647097 11.381656 12.535964 12.431027 10.332286 11.696467 10.752034 13.060649 
    ##     14257     14258     14259     14260     14261     14262     14263     14264 
    ## 11.066845  9.177978  7.603922 11.486593 11.276719 13.375460 10.542160 10.542160 
    ##     14265     14266     14267     14268     14269     14270     14271     14272 
    ##  7.603922  7.708859  9.807600 11.381656 13.375460  8.548356 12.955712 10.752034 
    ##     14273     14274     14275     14276     14277     14278     14279     14280 
    ##  9.177978  8.863167 13.165586 12.221153 11.906342 10.752034 12.640901 13.165586 
    ##     14281     14282     14283     14284     14285     14286     14287     14288 
    ## 11.276719 11.696467  9.912537  8.653293 11.591530  8.548356  8.128607  8.758230 
    ##     14289     14290     14291     14292     14293     14294     14295     14296 
    ##  8.233544  9.702663  7.079237  9.597726  7.813796  9.807600 10.437223 11.906342 
    ##     14297     14298     14299     14300     14301     14302     14303     14304 
    ##  9.177978 13.585334 13.900146 13.480397 13.585334 13.795209 14.319894 14.110020 
    ##     14305     14306     14307     14308     14309     14310     14311     14312 
    ## 13.585334 13.900146 13.375460 13.585334 14.005083 14.319894 14.214957 14.319894 
    ##     14313     14314     14315     14316     14317     14318     14319     14320 
    ## 14.214957 14.214957 14.319894 13.900146 13.900146 14.110020 13.480397 14.424831 
    ##     14321     14322     14323     14324     14325     14326     14327     14328 
    ## 13.375460 14.110020 14.005083 13.585334 13.900146 14.319894 14.214957 14.424831 
    ##     14329     14330     14331     14332     14333     14334     14335     14336 
    ## 13.690271 14.424831 13.690271 14.214957 13.795209 14.214957 13.480397 14.214957 
    ##     14337     14338     14339     14340     14341     14342     14343     14344 
    ## 14.214957 14.319894 14.214957 14.005083 14.319894 14.424831 14.319894 14.319894 
    ##     14345     14346     14347     14348     14349     14350     14351     14352 
    ## 14.214957 14.424831 14.005083 13.690271 13.900146 13.690271 13.690271  9.387852 
    ##     14353     14354     14355     14356     14357     14358     14359     14360 
    ## 12.011279 13.060649 10.542160  8.758230  8.863167  7.918733 12.745838 10.647097 
    ##     14361     14362     14363     14364     14365     14366     14367     14368 
    ## 12.221153 11.696467 13.060649  8.128607 10.227349 12.745838  8.443419 12.535964 
    ##     14369     14370     14371     14372     14373     14374     14375     14376 
    ## 11.066845  8.443419 12.431027 10.856971  8.443419  9.807600 12.116216 12.955712 
    ##     14377     14378     14379     14380     14381     14382     14383     14384 
    ## 12.955712 12.116216  8.443419 12.745838 12.431027 10.332286 13.060649 12.011279 
    ##     14385     14386     14387     14388     14389     14390     14391     14392 
    ## 10.332286  7.603922  8.653293 12.221153 10.961908  8.443419  8.863167  9.177978 
    ##     14393     14394     14395     14396     14397     14398     14399     14400 
    ## 11.906342  8.233544 11.801404 10.647097 12.221153  9.282915 10.437223 12.116216 
    ##     14401     14402     14403     14404     14405     14406     14407     14408 
    ##  8.758230 10.856971 11.906342  7.918733 12.011279  7.079237  8.128607 10.122412 
    ##     14409     14410     14411     14412     14413     14414     14415     14416 
    ##  8.758230 10.647097 11.696467 13.060649 11.696467  8.128607  7.603922  8.758230 
    ##     14417     14418     14419     14420     14421     14422     14423     14424 
    ## 11.696467  8.338482 13.270523 10.856971 11.066845 12.221153  9.597726 13.060649 
    ##     14425     14426     14427     14428     14429     14430     14431     14432 
    ##  9.702663 11.696467 11.276719 10.122412 12.011279 12.431027 12.640901  9.597726 
    ##     14433     14434     14435     14436     14437     14438     14439     14440 
    ## 11.696467 10.122412 10.437223 11.276719  8.548356 11.276719 12.431027 12.221153 
    ##     14441     14442     14443     14444     14445     14446     14447     14448 
    ## 13.270523 10.647097 10.752034 13.060649 11.486593 12.326090  9.912537 10.542160 
    ##     14449     14450     14451     14452     14453     14454     14455     14456 
    ## 10.647097  7.813796 11.696467  9.702663 10.437223 10.542160  9.387852  7.603922 
    ##     14457     14458     14459     14460     14461     14462     14463     14464 
    ##  9.807600  9.177978 12.116216 11.486593  8.233544 10.647097 11.696467 10.647097 
    ##     14465     14466     14467     14468     14469     14470     14471     14472 
    ## 13.375460  9.597726  9.282915 13.165586 12.326090  8.653293  9.912537 12.431027 
    ##     14473     14474     14475     14476     14477     14478     14479     14480 
    ##  7.079237 10.752034  8.968104  8.548356 12.535964  8.128607  8.758230 12.535964 
    ##     14481     14482     14483     14484     14485     14486     14487     14488 
    ##  9.597726  9.702663 10.647097  7.079237 11.591530 10.856971  9.492789 11.066845 
    ##     14489     14490     14491     14492     14493     14494     14495     14496 
    ## 10.961908 13.375460  8.128607  8.233544 11.906342 10.542160  9.492789 10.437223 
    ##     14497     14498     14499     14500     14501     14502     14503     14504 
    ##  7.603922 11.591530  7.603922 10.752034 12.850775 11.486593  9.073041  9.073041 
    ##     14505     14506     14507     14508     14509     14510     14511     14512 
    ##  9.387852 10.017474  7.603922 10.752034 12.431027 12.011279  8.023670  8.758230 
    ##     14513     14514     14515     14516     14517     14518     14519     14520 
    ##  9.387852 11.066845  8.968104 13.165586  7.918733 10.856971 11.696467 10.961908 
    ##     14521     14522     14523     14524     14525     14526     14527     14528 
    ## 10.122412 11.696467 10.437223 11.591530 12.745838  7.603922 10.961908 12.850775 
    ##     14529     14530     14531     14532     14533     14534     14535     14536 
    ##  8.653293  8.023670 13.270523  9.807600  9.702663  8.758230  7.603922 11.381656 
    ##     14537     14538     14539     14540     14541     14542     14543     14544 
    ##  7.079237 11.486593  9.177978 13.165586 11.801404 11.171782 12.326090 12.955712 
    ##     14545     14546     14547     14548     14549     14550     14551     14552 
    ## 10.332286  9.492789 11.801404 10.961908 13.270523  9.177978 11.486593  8.968104 
    ##     14553     14554     14555     14556     14557     14558     14559     14560 
    ##  8.758230  9.282915  8.023670 12.640901  8.968104 12.745838  8.548356  8.548356 
    ##     14561     14562     14563     14564     14565     14566     14567     14568 
    ## 12.326090 11.801404 12.326090 11.171782  7.079237  9.177978  8.443419 10.647097 
    ##     14569     14570     14571     14572     14573     14574     14575     14576 
    ##  9.387852 11.801404  7.079237 12.221153  8.338482 12.221153 12.745838 11.591530 
    ##     14577     14578     14579     14580     14581     14582     14583     14584 
    ##  9.177978 12.011279 10.856971 13.060649 11.276719  9.702663 12.326090  8.128607 
    ##     14585     14586     14587     14588     14589     14590     14591     14592 
    ##  7.603922  8.863167 11.486593 12.640901  8.548356  9.807600 12.116216 12.955712 
    ##     14593     14594     14595     14596     14597     14598     14599     14600 
    ##  8.863167 10.227349 10.227349  9.387852  9.597726 12.745838  9.177978  8.758230 
    ##     14601     14602     14603     14604     14605     14606     14607     14608 
    ##  9.282915 11.486593  8.758230 10.017474 12.640901  8.443419  8.863167 12.640901 
    ##     14609     14610     14611     14612     14613     14614     14615     14616 
    ##  7.813796 11.906342 12.011279 12.326090  7.918733  9.702663 10.017474 12.326090 
    ##     14617     14618     14619     14620     14621     14622     14623     14624 
    ## 13.060649 10.122412 11.696467  7.079237  7.708859 12.326090  8.443419 10.437223 
    ##     14625     14626     14627     14628     14629     14630     14631     14632 
    ## 13.375460 11.171782 13.270523  9.387852  7.708859 10.332286  9.282915 11.066845 
    ##     14633     14634     14635     14636     14637     14638     14639     14640 
    ##  7.708859 11.171782  7.918733  9.807600  9.597726  9.073041  7.079237  7.603922 
    ##     14641     14642     14643     14644     14645     14646     14647     14648 
    ## 11.591530 12.116216 10.332286  9.807600 11.696467 10.752034 11.696467 11.276719 
    ##     14649     14650     14651     14652     14653     14654     14655     14656 
    ##  7.079237 12.850775 12.221153  9.492789  8.968104 11.696467 12.850775 12.011279 
    ##     14657     14658     14659     14660     14661     14662     14663     14664 
    ## 11.906342 12.955712  8.968104 11.801404  9.597726  8.233544 11.276719  8.863167 
    ##     14665     14666     14667     14668     14669     14670     14671     14672 
    ##  9.073041 12.745838  8.128607 10.227349  8.758230  9.807600 10.017474 12.745838 
    ##     14673     14674     14675     14676     14677     14678     14679     14680 
    ## 12.326090  8.863167  9.492789  9.073041  9.387852  8.968104 12.745838 12.431027 
    ##     14681     14682     14683     14684     14685     14686     14687     14688 
    ## 11.591530  9.177978  9.177978  9.387852 10.961908  8.548356 10.332286 12.221153 
    ##     14689     14690     14691     14692     14693     14694     14695     14696 
    ## 10.542160  8.548356  8.338482  9.597726  8.338482 11.696467 10.752034 11.066845 
    ##     14697     14698     14699     14700     14701     14702     14703     14704 
    ## 11.486593  8.863167  7.918733 11.696467  9.387852 12.745838 12.221153 12.535964 
    ##     14705     14706     14707     14708     14709     14710     14711     14712 
    ## 10.856971  8.023670 11.486593 11.696467 11.801404 12.431027 12.221153 12.640901 
    ##     14713     14714     14715     14716     14717     14718     14719     14720 
    ## 11.171782  8.548356 12.640901 10.437223 10.437223 10.017474 10.017474 10.856971 
    ##     14721     14722     14723     14724     14725     14726     14727     14728 
    ##  8.758230 12.955712 10.017474 10.856971 11.801404  7.079237 11.906342 10.017474 
    ##     14729     14730     14731     14732     14733     14734     14735     14736 
    ## 11.066845  7.079237 10.856971  9.597726 12.221153  8.863167  9.282915 12.955712 
    ##     14737     14738     14739     14740     14741     14742     14743     14744 
    ## 12.745838  9.177978 12.535964  7.603922  9.177978  8.128607  9.282915  9.282915 
    ##     14745     14746     14747     14748     14749     14750     14751     14752 
    ## 10.752034 12.535964 11.696467 10.332286  8.758230  9.492789 11.381656  7.918733 
    ##     14753     14754     14755     14756     14757     14758     14759     14760 
    ##  9.597726 12.011279  9.492789  8.758230  7.603922 12.011279 13.165586  9.912537 
    ##     14761     14762     14763     14764     14765     14766     14767     14768 
    ##  8.968104  9.387852 11.591530 10.017474  9.702663 11.591530  8.653293  8.443419 
    ##     14769     14770     14771     14772     14773     14774     14775     14776 
    ##  7.079237 11.591530  9.282915  8.338482  9.282915  7.918733  9.597726  7.079237 
    ##     14777     14778     14779     14780     14781     14782     14783     14784 
    ## 12.011279 10.017474  7.813796  9.597726 11.696467 11.276719 11.591530 12.850775 
    ##     14785     14786     14787     14788     14789     14790     14791     14792 
    ##  9.912537 10.437223 10.647097 10.752034  8.653293  7.708859  9.073041  9.597726 
    ##     14793     14794     14795     14796     14797     14798     14799     14800 
    ##  9.177978 12.011279  7.079237 10.961908 12.850775  9.282915 10.122412  7.079237 
    ##     14801     14802     14803     14804     14805     14806     14807     14808 
    ##  9.597726  7.079237 12.221153 10.017474  9.912537 11.591530 11.906342 11.486593 
    ##     14809     14810     14811     14812     14813     14814     14815     14816 
    ##  9.282915 12.326090  9.387852 10.017474 11.591530 10.437223  8.548356  8.758230 
    ##     14817     14818     14819     14820     14821     14822     14823     14824 
    ## 13.165586 11.906342  8.233544  9.492789 13.270523  8.443419 12.535964 10.961908 
    ##     14825     14826     14827     14828     14829     14830     14831     14832 
    ## 12.535964  7.603922 10.752034 10.122412  7.708859  9.912537  7.079237  9.807600 
    ##     14833     14834     14835     14836     14837     14838     14839     14840 
    ## 11.276719 10.856971 12.431027  8.653293  7.079237  9.073041  7.079237 11.276719 
    ##     14841     14842     14843     14844     14845     14846     14847     14848 
    ## 13.270523  8.443419  8.023670 12.011279  8.233544  8.443419 13.060649 11.696467 
    ##     14849     14850     14851     14852     14853     14854     14855     14856 
    ##  7.813796  8.233544  7.079237 10.122412  9.702663 10.647097  8.338482 12.326090 
    ##     14857     14858     14859     14860     14861     14862     14863     14864 
    ## 11.801404 10.227349  9.807600 11.906342  8.338482  9.912537 12.535964  8.338482 
    ##     14865     14866     14867     14868     14869     14870     14871     14872 
    ##  8.023670 10.647097 12.431027 12.221153  8.863167 11.486593 11.696467  9.807600 
    ##     14873     14874     14875     14876     14877     14878     14879     14880 
    ##  7.813796 11.486593 10.437223  7.918733 12.850775  9.702663 13.480397  9.702663 
    ##     14881     14882     14883     14884     14885     14886     14887     14888 
    ##  8.863167 13.270523  7.079237  9.177978  7.603922 10.437223  9.387852 12.326090 
    ##     14889     14890     14891     14892     14893     14894     14895     14896 
    ##  7.079237 10.856971 11.591530 10.856971 12.326090 10.332286  9.912537 12.535964 
    ##     14897     14898     14899     14900     14901     14902     14903     14904 
    ## 10.227349 13.270523 10.961908  9.807600 11.486593 10.017474  8.968104 11.381656 
    ##     14905     14906     14907     14908     14909     14910     14911     14912 
    ## 12.431027  9.282915  9.282915  7.708859  7.603922  7.603922 10.017474 12.431027 
    ##     14913     14914     14915     14916     14917     14918     14919     14920 
    ##  7.603922 12.221153  8.338482 11.486593 10.752034  9.177978 10.122412  9.492789 
    ##     14921     14922     14923     14924     14925     14926     14927     14928 
    ##  9.597726  8.443419 10.332286  7.603922  9.073041 10.332286  8.233544 12.326090 
    ##     14929     14930     14931     14932     14933     14934     14935     14936 
    ## 11.381656  7.079237 10.017474  8.233544  8.863167 10.122412  9.492789 12.011279 
    ##     14937     14938     14939     14940     14941     14942     14943     14944 
    ## 14.319894 13.480397 13.585334 14.319894 14.005083 13.900146 14.319894  8.968104 
    ##     14945     14946     14947     14948     14949     14950     14951     14952 
    ## 10.017474  9.073041 12.116216 10.332286 12.221153 11.171782 12.535964  9.073041 
    ##     14953     14954     14955     14956     14957     14958     14959     14960 
    ##  8.758230 13.270523 10.856971  7.603922  9.807600  9.282915  9.177978 10.017474 
    ##     14961     14962     14963     14964     14965     14966     14967     14968 
    ## 11.801404 10.542160  8.128607  9.282915  7.603922  7.918733 12.326090  9.492789 
    ##     14969     14970     14971     14972     14973     14974     14975     14976 
    ## 10.647097  9.387852 13.060649 10.227349 10.647097  8.338482  9.702663  8.128607 
    ##     14977     14978     14979     14980     14981     14982     14983     14984 
    ## 12.116216  9.177978  7.813796 12.011279 10.437223 11.591530 12.116216  8.023670 
    ##     14985     14986     14987     14988     14989     14990     14991     14992 
    ## 10.017474 12.535964 12.116216  9.597726 10.542160  9.282915  8.863167  7.079237 
    ##     14993     14994     14995     14996     14997     14998     14999     15000 
    ##  8.968104  9.177978  8.233544  8.443419 12.955712 11.381656 10.227349 12.640901 
    ##     15001     15002     15003     15004     15005     15006     15007     15008 
    ##  7.813796  8.653293 11.486593  7.918733 11.801404  7.079237  8.233544  9.177978 
    ##     15009     15010     15011     15012     15013     15014     15015     15016 
    ##  8.023670 10.647097  7.813796  9.177978  8.233544  9.702663 11.066845  8.338482 
    ##     15017     15018     15019     15020     15021     15022     15023     15024 
    ##  8.758230 12.850775  9.282915  9.912537  7.079237 10.752034  9.387852 10.437223 
    ##     15025     15026     15027     15028     15029     15030     15031     15032 
    ##  9.492789  8.968104  8.338482 13.270523 12.640901 13.270523 11.801404  8.443419 
    ##     15033     15034     15035     15036     15037     15038     15039     15040 
    ##  9.912537 10.437223  9.912537 12.431027 13.165586 11.276719  9.177978 10.017474 
    ##     15041     15042     15043     15044     15045     15046     15047     15048 
    ##  7.603922 12.011279  8.023670  9.492789 10.122412  7.603922 10.332286 10.961908 
    ##     15049     15050     15051     15052     15053     15054     15055     15056 
    ##  9.073041 13.375460 10.961908 10.647097  7.079237  9.177978 11.381656  7.079237 
    ##     15057     15058     15059     15060     15061     15062     15063     15064 
    ##  9.492789 12.326090  8.968104  7.603922 11.276719 10.856971  9.492789  9.807600 
    ##     15065     15066     15067     15068     15069     15070     15071     15072 
    ##  8.443419 11.276719  8.443419  9.702663 10.752034  9.702663  9.597726  8.443419 
    ##     15073     15074     15075     15076     15077     15078     15079     15080 
    ##  9.597726  8.968104 10.227349 10.647097  9.807600 12.326090  9.597726  9.387852 
    ##     15081     15082     15083     15084     15085     15086     15087     15088 
    ## 13.270523 10.017474  8.863167  8.233544  9.912537  9.282915  8.443419 13.165586 
    ##     15089     15090     15091     15092     15093     15094     15095     15096 
    ##  9.492789  9.807600 11.696467  9.807600  9.282915 10.332286 10.437223  9.597726 
    ##     15097     15098     15099     15100     15101     15102     15103     15104 
    ##  9.387852 12.745838 12.745838 11.066845 11.276719 13.270523 11.696467 10.332286 
    ##     15105     15106     15107     15108     15109     15110     15111     15112 
    ## 10.542160  9.387852 13.480397 13.585334 14.110020 14.319894 14.110020 14.424831 
    ##     15113     15114     15115     15116     15117     15118     15119     15120 
    ## 13.795209 13.795209 14.005083 13.795209 14.110020 14.110020 13.900146 14.110020 
    ##     15121     15122     15123     15124     15125     15126     15127     15128 
    ##  9.702663 11.696467 12.535964 11.066845  9.912537  8.758230 14.110020 14.110020 
    ##     15129     15130     15131     15132     15133     15134     15135     15136 
    ##  8.548356 11.066845 11.486593  7.079237 13.375460  8.443419 11.381656 11.906342 
    ##     15137     15138     15139     15140     15141     15142     15143     15144 
    ##  7.603922 12.116216  9.282915  8.968104  8.968104 12.011279 10.332286 11.066845 
    ##     15145     15146     15147     15148     15149     15150     15151     15152 
    ##  9.387852  9.807600  9.073041  8.338482 12.221153 11.381656 10.332286 11.906342 
    ##     15153     15154     15155     15156     15157     15158     15159     15160 
    ##  8.548356  9.282915 12.011279 12.535964 12.326090 11.486593 11.486593  8.443419 
    ##     15161     15162     15163     15164     15165     15166     15167     15168 
    ## 10.227349 11.171782 10.017474 12.640901 10.332286  7.603922  8.968104 13.165586 
    ##     15169     15170     15171     15172     15173     15174     15175     15176 
    ## 12.011279  7.813796 11.486593 12.955712  9.912537 12.850775  9.282915 11.486593 
    ##     15177     15178     15179     15180     15181     15182     15183     15184 
    ##  8.653293 10.542160 13.165586 12.221153 10.856971  9.387852  8.233544 10.752034 
    ##     15185     15186     15187     15188     15189     15190     15191     15192 
    ## 13.165586 11.486593 11.381656 11.696467 12.640901 12.326090 12.326090 11.066845 
    ##     15193     15194     15195     15196     15197     15198     15199     15200 
    ## 12.850775 12.011279  7.918733 10.017474 12.116216  7.603922  8.338482 12.745838 
    ##     15201     15202     15203     15204     15205     15206     15207     15208 
    ## 10.122412  9.282915  7.079237 10.017474 10.647097  9.492789 10.542160 11.801404 
    ##     15209     15210     15211     15212     15213     15214     15215     15216 
    ## 11.906342 10.961908 12.221153 12.535964 11.066845  7.603922 13.165586 10.856971 
    ##     15217     15218     15219     15220     15221     15222     15223     15224 
    ##  8.548356  9.912537 12.955712 12.955712 12.535964 12.535964  9.912537 12.850775 
    ##     15225     15226     15227     15228     15229     15230     15231     15232 
    ## 14.214957 12.326090 12.955712  9.282915 10.332286  8.548356  9.282915 14.214957 
    ##     15233     15234     15235     15236     15237     15238     15239     15240 
    ## 13.585334 13.585334 13.585334 13.900146 13.690271 13.480397 14.319894 14.110020 
    ##     15241     15242     15243     15244     15245     15246     15247     15248 
    ## 13.480397 14.214957 14.110020 14.005083 14.214957 13.690271 14.424831 13.480397 
    ##     15249     15250     15251     15252     15253     15254     15255     15256 
    ## 13.375460 14.110020 14.005083 13.480397 13.690271 14.214957 13.900146 13.900146 
    ##     15257     15258     15259     15260     15261     15262     15263     15264 
    ## 13.585334 14.424831 13.900146 13.690271 13.480397 13.795209 13.795209 14.214957 
    ##     15265     15266     15267     15268     15269     15270     15271     15272 
    ## 14.005083 13.585334 14.214957 14.319894 14.424831 14.424831 14.424831 13.690271 
    ##     15273     15274     15275     15276     15277     15278     15279     15280 
    ## 14.110020 14.005083 13.690271 13.480397 13.585334 13.480397 13.795209 14.424831 
    ##     15281     15282     15283     15284     15285     15286     15287     15288 
    ## 13.690271 13.795209 14.319894 13.060649 12.535964 11.696467 12.011279 11.381656 
    ##     15289     15290     15291     15292     15293     15294     15295     15296 
    ##  9.807600 11.276719 11.276719 11.906342 13.375460 11.906342 10.542160 10.437223 
    ##     15297     15298     15299     15300     15301     15302     15303     15304 
    ##  7.813796 13.060649 10.227349 12.221153 10.122412 10.122412 11.381656 10.542160 
    ##     15305     15306     15307     15308     15309     15310     15311     15312 
    ##  8.443419  8.863167 12.011279  8.968104  9.597726 12.011279  7.603922  9.702663 
    ##     15313     15314     15315     15316     15317     15318     15319     15320 
    ## 11.906342  8.758230  7.708859  9.912537 12.221153  9.492789 11.066845  9.492789 
    ##     15321     15322     15323     15324     15325     15326     15327     15328 
    ## 10.017474  8.443419 10.227349 14.214957 11.696467  8.338482 10.961908  8.128607 
    ##     15329     15330     15331     15332     15333     15334     15335     15336 
    ## 12.955712  8.233544  7.603922  9.912537  9.387852 10.961908 11.381656  9.177978 
    ##     15337     15338     15339     15340     15341     15342     15343     15344 
    ## 12.850775  8.653293  8.443419 12.850775  7.079237 12.640901 11.591530 13.060649 
    ##     15345     15346     15347     15348     15349     15350     15351     15352 
    ## 11.696467 12.850775  9.492789  7.079237  9.702663  9.492789 12.221153  7.603922 
    ##     15353     15354     15355     15356     15357     15358     15359     15360 
    ## 12.011279 12.431027 10.647097 13.165586  8.758230 10.752034 11.696467 12.640901 
    ##     15361     15362     15363     15364     15365     15366     15367     15368 
    ## 13.060649 13.165586 10.542160 10.542160 13.375460 10.437223  7.079237 10.752034 
    ##     15369     15370     15371     15372     15373     15374     15375     15376 
    ## 12.011279  9.387852  9.387852 11.591530 11.276719 10.122412 12.955712  7.918733 
    ##     15377     15378     15379     15380     15381     15382     15383     15384 
    ## 12.535964 12.535964 10.961908  7.079237 11.801404 10.647097  8.968104 10.332286 
    ##     15385     15386     15387     15388     15389     15390     15391     15392 
    ##  9.912537 11.381656 10.647097  9.282915 11.381656 12.745838 11.801404 10.752034 
    ##     15393     15394     15395     15396     15397     15398     15399     15400 
    ## 11.696467 10.542160 11.486593 12.326090  7.079237  9.073041 13.060649  8.863167 
    ##     15401     15402     15403     15404     15405     15406     15407     15408 
    ## 12.011279 10.647097  7.603922  7.603922  8.443419 13.270523  8.548356 10.122412 
    ##     15409     15410     15411     15412     15413     15414     15415     15416 
    ## 12.221153 10.017474 12.116216 11.066845 10.227349 11.696467 11.486593 12.326090 
    ##     15417     15418     15419     15420     15421     15422     15423     15424 
    ## 10.856971  8.863167 10.961908  8.443419  7.813796 10.752034  8.233544  9.492789 
    ##     15425     15426     15427     15428     15429     15430     15431     15432 
    ## 12.745838 12.011279  7.603922  9.177978  8.863167 11.801404 11.381656  8.968104 
    ##     15433     15434     15435     15436     15437     15438     15439     15440 
    ## 10.227349 10.227349 12.850775 11.171782 12.955712 10.122412  7.603922 11.906342 
    ##     15441     15442     15443     15444     15445     15446     15447     15448 
    ## 12.745838  8.758230  9.597726 10.017474 12.011279 10.332286 10.227349  8.443419 
    ##     15449     15450     15451     15452     15453     15454     15455     15456 
    ##  9.492789 12.116216 12.745838  9.807600 11.171782 12.640901 12.116216  8.128607 
    ##     15457     15458     15459     15460     15461     15462     15463     15464 
    ## 10.332286 10.332286 11.381656  9.492789 13.165586 11.801404 11.171782 11.381656 
    ##     15465     15466     15467     15468     15469     15470     15471     15472 
    ##  9.282915  9.073041 13.480397 10.856971 10.227349  7.603922  8.338482 11.066845 
    ##     15473     15474     15475     15476     15477     15478     15479     15480 
    ## 12.326090  8.863167 10.332286  8.653293 11.381656  9.177978  8.443419 12.116216 
    ##     15481     15482     15483     15484     15485     15486     15487     15488 
    ##  8.023670  9.597726  8.338482 10.437223 11.276719  7.079237 10.961908 10.017474 
    ##     15489     15490     15491     15492     15493     15494     15495     15496 
    ##  9.702663 12.116216 12.850775  7.603922  7.079237 11.066845  8.653293 13.270523 
    ##     15497     15498     15499     15500     15501     15502     15503     15504 
    ## 12.011279 11.696467  7.603922 10.437223 11.591530  9.597726 12.011279  8.863167 
    ##     15505     15506     15507     15508     15509     15510     15511     15512 
    ## 12.116216 12.850775 11.171782  8.338482 12.850775  9.702663  8.023670 11.381656 
    ##     15513     15514     15515     15516     15517     15518     15519     15520 
    ##  7.918733 11.591530 12.850775 11.906342  9.702663  9.912537 11.801404 11.381656 
    ##     15521     15522     15523     15524     15525     15526     15527     15528 
    ## 11.801404  9.597726  7.708859  8.548356  9.702663  7.079237 11.696467 10.122412 
    ##     15529     15530     15531     15532     15533     15534     15535     15536 
    ## 10.122412 12.116216  8.233544 10.122412 13.375460  9.597726  7.603922 12.431027 
    ##     15537     15538     15539     15540     15541     15542     15543     15544 
    ## 11.381656  8.233544 12.011279  8.128607 11.381656  7.079237 12.431027 10.122412 
    ##     15545     15546     15547     15548     15549     15550     15551     15552 
    ## 12.640901  9.597726 13.060649  7.079237 12.221153 11.276719  7.079237 10.542160 
    ##     15553     15554     15555     15556     15557     15558     15559     15560 
    ## 13.060649 10.856971 11.276719 11.066845  7.603922  9.702663 10.332286  8.023670 
    ##     15561     15562     15563     15564     15565     15566     15567     15568 
    ## 12.955712 13.060649  7.603922  9.492789  9.597726  8.023670 10.332286  8.023670 
    ##     15569     15570     15571     15572     15573     15574     15575     15576 
    ## 10.122412  9.912537  9.807600 12.745838  9.492789 11.696467  8.758230  9.807600 
    ##     15577     15578     15579     15580     15581     15582     15583     15584 
    ##  9.702663  9.702663 10.017474 10.017474  9.597726  9.597726  9.912537 13.165586 
    ##     15585     15586     15587     15588     15589     15590     15591     15592 
    ## 12.221153  9.177978 12.221153  8.863167 12.850775  9.597726  8.968104 13.165586 
    ##     15593     15594     15595     15596     15597     15598     15599     15600 
    ## 12.431027  9.282915 11.801404 12.535964  8.443419  9.597726 12.326090 12.431027 
    ##     15601     15602     15603     15604     15605     15606     15607     15608 
    ## 13.375460 11.801404  9.807600 12.535964  9.702663 11.591530 12.221153 11.801404 
    ##     15609     15610     15611     15612     15613     15614     15615     15616 
    ##  8.758230 10.856971  8.338482 12.850775  9.702663  8.128607 11.906342 12.850775 
    ##     15617     15618     15619     15620     15621     15622     15623     15624 
    ## 12.640901  8.968104  8.023670 12.640901  9.177978 10.017474 11.801404 12.116216 
    ##     15625     15626     15627     15628     15629     15630     15631     15632 
    ## 13.060649  8.443419 10.332286  8.968104  9.073041  9.492789 10.647097 11.801404 
    ##     15633     15634     15635     15636     15637     15638     15639     15640 
    ##  9.073041  8.443419  9.702663 12.745838 10.017474 13.270523 11.906342  9.492789 
    ##     15641     15642     15643     15644     15645     15646     15647     15648 
    ##  8.023670  8.338482  8.548356 11.801404  7.079237 13.165586  9.177978  7.079237 
    ##     15649     15650     15651     15652     15653     15654     15655     15656 
    ## 12.221153 11.486593  7.079237  9.492789  8.338482 13.165586 12.116216  8.653293 
    ##     15657     15658     15659     15660     15661     15662     15663     15664 
    ##  9.177978 11.276719 12.221153  8.653293  9.492789 12.745838  7.603922 10.227349 
    ##     15665     15666     15667     15668     15669     15670     15671     15672 
    ##  8.968104 11.696467 12.011279  9.597726 11.486593 10.856971  8.758230 11.381656 
    ##     15673     15674     15675     15676     15677     15678     15679     15680 
    ## 10.856971  9.702663 12.431027 10.122412  7.813796 12.535964 10.332286 12.221153 
    ##     15681     15682     15683     15684     15685     15686     15687     15688 
    ## 10.856971  8.233544  9.387852 11.276719  7.918733  9.492789 12.116216 11.801404 
    ##     15689     15690     15691     15692     15693     15694     15695     15696 
    ##  8.653293 11.381656  9.177978  9.387852 10.961908  9.807600 10.122412 12.640901 
    ##     15697     15698     15699     15700     15701     15702     15703     15704 
    ## 11.696467  9.177978 10.017474 12.745838 11.276719  9.387852 12.221153  9.387852 
    ##     15705     15706     15707     15708     15709     15710     15711     15712 
    ##  8.548356  7.079237  9.492789 12.640901 12.640901  9.282915  8.968104 12.116216 
    ##     15713     15714     15715     15716     15717     15718     15719     15720 
    ##  7.603922 11.696467 10.856971 13.165586  7.708859  9.492789  7.079237 10.961908 
    ##     15721     15722     15723     15724     15725     15726     15727     15728 
    ## 10.227349 12.431027  9.282915  8.338482 12.850775 11.486593  9.177978 12.326090 
    ##     15729     15730     15731     15732     15733     15734     15735     15736 
    ## 11.171782 12.640901  9.282915 12.011279  8.443419 10.437223 11.906342  9.912537 
    ##     15737     15738     15739     15740     15741     15742     15743     15744 
    ##  9.912537 12.745838 12.011279 12.850775  8.233544 11.801404  9.597726  8.233544 
    ##     15745     15746     15747     15748     15749     15750     15751     15752 
    ##  9.282915 11.171782  8.548356 12.640901 13.060649  8.338482 10.227349  8.968104 
    ##     15753     15754     15755     15756     15757     15758     15759     15760 
    ##  8.338482  7.603922  8.863167  7.079237  8.968104  9.073041  9.597726  9.702663 
    ##     15761     15762     15763     15764     15765     15766     15767     15768 
    ##  8.023670 10.122412 12.640901  8.338482 10.752034  9.807600 11.381656 12.745838 
    ##     15769     15770     15771     15772     15773     15774     15775     15776 
    ##  9.073041 11.906342 12.326090  9.177978 10.227349 12.535964 11.696467 10.017474 
    ##     15777     15778     15779     15780     15781     15782     15783     15784 
    ##  9.807600  8.548356 11.801404 11.801404  7.708859  8.023670  7.603922  8.443419 
    ##     15785     15786     15787     15788     15789     15790     15791     15792 
    ## 10.017474  7.079237 11.276719 11.381656 13.375460 10.227349  8.653293 13.795209 
    ##     15793     15794     15795     15796     15797     15798     15799     15800 
    ##  9.807600  8.758230 11.906342 11.696467  9.912537  8.758230 12.535964  8.128607 
    ##     15801     15802     15803     15804     15805     15806     15807     15808 
    ## 12.326090  7.918733 12.221153 11.591530  9.597726  9.807600 13.060649  8.653293 
    ##     15809     15810     15811     15812     15813     15814     15815     15816 
    ##  7.079237  9.282915 11.696467  8.863167  8.023670 12.850775  8.758230 12.850775 
    ##     15817     15818     15819     15820     15821     15822     15823     15824 
    ##  7.918733  8.653293  8.023670 11.486593  8.758230 12.535964  8.548356  9.177978 
    ##     15825     15826     15827     15828     15829     15830     15831     15832 
    ## 13.270523 12.955712  9.177978 11.696467  7.918733  7.813796  9.807600 11.381656 
    ##     15833     15834     15835     15836     15837     15838     15839     15840 
    ## 12.011279 11.171782 10.332286 11.276719  8.863167  7.708859 12.326090  7.918733 
    ##     15841     15842     15843     15844     15845     15846     15847     15848 
    ## 10.122412  9.702663 10.332286  8.128607 10.437223  9.912537 11.906342 12.955712 
    ##     15849     15850     15851     15852     15853     15854     15855     15856 
    ## 11.276719 12.116216  8.863167 10.752034 12.535964  7.079237  7.603922 11.696467 
    ##     15857     15858     15859     15860     15861     15862     15863     15864 
    ## 13.165586 10.542160 12.326090 12.326090  9.387852 10.122412 10.227349 12.640901 
    ##     15865     15866     15867     15868     15869     15870     15871     15872 
    ## 12.640901  9.597726 10.542160 11.591530 12.535964 10.542160 11.066845  9.282915 
    ##     15873     15874     15875     15876     15877     15878     15879     15880 
    ##  9.177978 11.381656 11.906342  8.863167 10.856971  9.177978  8.758230 13.795209 
    ##     15881     15882     15883     15884     15885     15886     15887     15888 
    ## 14.319894 14.319894 14.319894 13.585334 13.585334 13.480397 13.795209 13.585334 
    ##     15889     15890     15891     15892     15893     15894     15895     15896 
    ## 14.214957 14.214957 14.005083 14.005083 11.276719  9.282915 11.171782 10.122412 
    ##     15897     15898     15899     15900     15901     15902     15903     15904 
    ## 11.801404  8.758230 10.961908  7.603922  8.653293  7.079237  9.912537 12.221153 
    ##     15905     15906     15907     15908     15909     15910     15911     15912 
    ##  7.603922 11.171782  8.863167  8.548356 10.017474 12.431027  9.807600 11.486593 
    ##     15913     15914     15915     15916     15917     15918     15919     15920 
    ##  9.492789 12.116216 12.011279  7.603922 12.955712 12.535964 12.221153 11.276719 
    ##     15921     15922     15923     15924     15925     15926     15927     15928 
    ##  8.758230  7.079237  9.492789  9.282915 12.221153 12.850775  9.073041 12.640901 
    ##     15929     15930     15931     15932     15933     15934     15935     15936 
    ##  9.912537 10.017474  7.079237 11.486593  9.177978 10.961908  9.282915  8.968104 
    ##     15937     15938     15939     15940     15941     15942     15943     15944 
    ## 12.431027 10.227349 12.326090 11.486593  8.758230  8.023670 12.850775 12.850775 
    ##     15945     15946     15947     15948     15949     15950     15951     15952 
    ## 11.486593 12.431027  8.968104 12.640901  8.548356  9.807600  8.863167 11.906342 
    ##     15953     15954     15955     15956     15957     15958     15959     15960 
    ## 11.591530 11.696467  9.177978  9.702663 10.332286  9.702663 10.647097  9.912537 
    ##     15961     15962     15963     15964     15965     15966     15967     15968 
    ## 11.171782 11.066845 12.955712 10.332286 10.961908  9.492789 10.122412 10.647097 
    ##     15969     15970     15971     15972     15973     15974     15975     15976 
    ## 10.647097  8.128607  9.282915 12.640901 12.955712  8.233544 13.270523 10.122412 
    ##     15977     15978     15979     15980     15981     15982     15983     15984 
    ##  8.863167 12.535964  9.282915  8.653293 10.961908 13.375460 12.011279  8.023670 
    ##     15985     15986     15987     15988     15989     15990     15991     15992 
    ##  9.912537 11.801404  9.282915  9.597726 11.486593 10.227349 12.326090 12.850775 
    ##     15993     15994     15995     15996     15997     15998     15999     16000 
    ## 10.437223 13.270523 11.801404 12.431027 10.122412 13.585334 11.486593  7.603922 
    ##     16001     16002     16003     16004     16005     16006     16007     16008 
    ## 13.375460 13.480397 14.214957 14.424831 14.005083 14.214957 13.795209 13.795209 
    ##     16009     16010     16011     16012     16013     16014     16015     16016 
    ## 13.900146 14.214957 14.110020 14.214957 14.110020 14.319894 14.424831 14.319894 
    ##     16017     16018     16019     16020     16021     16022     16023     16024 
    ## 14.214957 14.214957 13.690271 14.005083 14.319894 13.690271 13.900146 13.795209 
    ##     16025     16026     16027     16028     16029     16030     16031     16032 
    ## 14.110020 13.480397 13.795209 13.375460 14.319894 13.585334 14.319894 13.900146 
    ##     16033     16034     16035     16036     16037     16038     16039     16040 
    ## 14.005083 13.690271 13.585334 13.375460 13.585334 13.900146 14.214957 14.005083 
    ##     16041     16042     16043     16044     16045     16046     16047     16048 
    ## 14.319894 14.319894 13.795209 14.319894 13.795209 13.900146 14.110020 14.424831 
    ##     16049     16050     16051     16052     16053     16054     16055     16056 
    ## 13.480397 13.480397 13.480397 10.752034  8.863167 10.332286  8.443419 12.955712 
    ##     16057     16058     16059     16060     16061     16062     16063     16064 
    ## 10.122412  8.863167  9.282915  8.863167 10.647097 11.276719  8.023670  9.597726 
    ##     16065     16066     16067     16068     16069     16070     16071     16072 
    ##  9.282915 12.221153  8.968104 11.486593  8.338482  8.548356  7.603922  9.387852 
    ##     16073     16074     16075     16076     16077     16078     16079     16080 
    ##  7.603922  7.079237  7.079237 11.276719  7.813796  8.653293  8.863167  9.702663 
    ##     16081     16082     16083     16084     16085     16086     16087     16088 
    ## 12.535964  8.653293 12.640901 10.122412 10.856971 12.326090  7.813796  7.918733 
    ##     16089     16090     16091     16092     16093     16094     16095     16096 
    ## 13.375460  9.282915  9.597726  9.177978  9.282915 10.542160  7.603922  9.492789 
    ##     16097     16098     16099     16100     16101     16102     16103     16104 
    ##  9.912537 11.591530 12.955712  8.443419  9.807600  8.548356 10.542160  9.282915 
    ##     16105     16106     16107     16108     16109     16110     16111     16112 
    ##  9.073041 12.326090  8.233544 10.647097  8.548356 12.535964  9.073041  8.968104 
    ##     16113     16114     16115     16116     16117     16118     16119     16120 
    ## 11.801404  9.807600  9.807600 10.227349 11.066845 12.431027  8.548356 10.856971 
    ##     16121     16122     16123     16124     16125     16126     16127     16128 
    ##  8.968104 11.696467  9.282915  7.813796 10.122412 10.017474 10.122412 10.122412 
    ##     16129     16130     16131     16132     16133     16134     16135     16136 
    ##  8.233544 10.437223 10.961908  9.073041  8.968104 12.850775 12.431027 10.227349 
    ##     16137     16138     16139     16140     16141     16142     16143     16144 
    ##  8.653293  9.177978  7.603922 10.332286 11.171782  9.492789  9.807600 12.116216 
    ##     16145     16146     16147     16148     16149     16150     16151     16152 
    ##  9.912537 11.906342  7.079237  9.282915 12.640901  9.387852 12.116216 11.381656 
    ##     16153     16154     16155     16156     16157     16158     16159     16160 
    ##  7.079237 11.486593  8.548356 10.122412  7.603922  7.603922  7.079237 10.647097 
    ##     16161     16162     16163     16164     16165     16166     16167     16168 
    ## 13.060649 11.696467  8.863167  7.603922 11.801404  9.912537 12.116216  8.653293 
    ##     16169     16170     16171     16172     16173     16174     16175     16176 
    ##  8.548356 10.017474  8.758230 10.332286  8.023670  9.807600  7.813796 10.122412 
    ##     16177     16178     16179     16180     16181     16182     16183     16184 
    ##  8.023670  7.708859 10.122412  8.233544 10.017474  8.128607 10.647097  8.128607 
    ##     16185     16186     16187     16188     16189     16190     16191     16192 
    ##  9.702663  9.807600  9.807600 12.640901  9.912537  7.603922 11.801404  8.023670 
    ##     16193     16194     16195     16196     16197     16198     16199     16200 
    ##  7.603922  8.758230  7.813796 12.221153 12.116216 10.647097 10.752034 11.906342 
    ##     16201     16202     16203     16204     16205     16206     16207     16208 
    ##  8.758230 10.961908 10.961908  7.813796 10.752034  7.708859 11.276719  8.968104 
    ##     16209     16210     16211     16212     16213     16214     16215     16216 
    ## 11.696467 10.961908 13.270523  8.548356  8.653293 10.122412  8.023670 12.326090 
    ##     16217     16218     16219     16220     16221     16222     16223     16224 
    ##  9.912537  9.912537 12.745838 10.017474  8.863167 13.060649  7.603922 12.221153 
    ##     16225     16226     16227     16228     16229     16230     16231     16232 
    ##  9.387852 11.276719  8.863167 13.270523 12.955712 12.116216  9.073041 13.165586 
    ##     16233     16234     16235     16236     16237     16238     16239     16240 
    ## 12.011279  8.338482  7.603922 12.955712  8.338482  8.023670  8.023670  9.073041 
    ##     16241     16242     16243     16244     16245     16246     16247     16248 
    ## 13.060649 10.017474  9.282915 10.752034  8.023670 12.535964  8.548356 12.221153 
    ##     16249     16250     16251     16252     16253     16254     16255     16256 
    ## 11.801404  9.387852 10.961908  8.443419  8.653293  8.863167  9.597726 10.647097 
    ##     16257     16258     16259     16260     16261     16262     16263     16264 
    ## 11.801404  8.548356 11.591530  7.918733  9.492789 11.381656  8.653293 11.801404 
    ##     16265     16266     16267     16268     16269     16270     16271     16272 
    ## 11.381656 11.171782 12.116216 12.535964  8.338482 13.060649 11.171782 10.961908 
    ##     16273     16274     16275     16276     16277     16278     16279     16280 
    ##  9.807600 11.171782 10.856971  8.233544  9.073041 10.227349  7.813796 10.437223 
    ##     16281     16282     16283     16284     16285     16286     16287     16288 
    ## 11.066845  9.282915  8.023670  7.708859  9.912537 12.431027 11.486593 12.745838 
    ##     16289     16290     16291     16292     16293     16294     16295     16296 
    ##  7.708859 12.535964  8.548356 11.276719  9.492789 10.332286  9.807600 10.961908 
    ##     16297     16298     16299     16300     16301     16302     16303     16304 
    ## 10.227349 12.011279 10.437223 12.955712  7.079237  8.023670 10.017474 12.745838 
    ##     16305     16306     16307     16308     16309     16310     16311     16312 
    ## 13.060649  7.603922 12.955712  7.603922  9.597726 11.591530 10.961908 11.906342 
    ##     16313     16314     16315     16316     16317     16318     16319     16320 
    ## 10.332286  7.603922 10.017474 12.850775 10.542160  8.548356 11.801404  7.079237 
    ##     16321     16322     16323     16324     16325     16326     16327     16328 
    ## 10.961908  7.079237  9.282915 12.116216 10.542160  8.968104  9.073041  9.177978 
    ##     16329     16330     16331     16332     16333     16334     16335     16336 
    ## 12.116216 10.017474  8.968104  8.443419  7.079237  9.807600 11.066845  7.079237 
    ##     16337     16338     16339     16340     16341     16342     16343     16344 
    ##  8.443419  7.918733  8.863167 12.011279  9.912537  9.073041 12.745838  9.177978 
    ##     16345     16346     16347     16348     16349     16350     16351     16352 
    ##  8.863167 11.696467 12.955712  8.968104 12.326090  7.708859 11.486593 11.696467 
    ##     16353     16354     16355     16356     16357     16358     16359     16360 
    ## 11.381656  8.758230  8.233544  8.128607 10.752034 12.221153 11.066845  8.233544 
    ##     16361     16362     16363     16364     16365     16366     16367     16368 
    ## 10.752034 12.431027 12.221153  7.918733  8.128607  9.387852 13.060649  8.968104 
    ##     16369     16370     16371     16372     16373     16374     16375     16376 
    ## 11.696467  7.708859  7.603922  8.548356  8.968104 11.066845 11.381656  7.603922 
    ##     16377     16378     16379     16380     16381     16382     16383     16384 
    ##  8.338482 13.375460 13.375460 12.431027  7.079237  8.443419 11.171782 10.332286 
    ##     16385     16386     16387     16388     16389     16390     16391     16392 
    ##  8.128607  9.702663 11.696467 10.122412  9.912537  7.603922 12.326090  9.177978 
    ##     16393     16394     16395     16396     16397     16398     16399     16400 
    ## 12.431027 10.122412 12.640901 10.227349  9.702663  9.282915  9.807600  8.653293 
    ##     16401     16402     16403     16404     16405     16406     16407     16408 
    ## 11.276719 10.542160 10.227349 10.752034 12.326090 10.227349  8.233544 12.326090 
    ##     16409     16410     16411     16412     16413     16414     16415     16416 
    ## 12.640901  9.387852 10.227349 10.752034  9.282915  8.548356 11.066845  9.177978 
    ##     16417     16418     16419     16420     16421     16422     16423     16424 
    ##  9.492789 12.640901  9.282915 12.011279  7.079237  8.758230  8.653293 10.122412 
    ##     16425     16426     16427     16428     16429     16430     16431     16432 
    ## 10.122412  9.912537 10.752034  9.807600 11.486593  7.079237 10.542160  8.863167 
    ##     16433     16434     16435     16436     16437     16438     16439     16440 
    ## 10.017474  8.653293  7.079237 10.961908  9.073041 11.171782  7.918733  8.968104 
    ##     16441     16442     16443     16444     16445     16446     16447     16448 
    ## 11.591530  9.597726  8.443419  7.918733  8.443419  8.548356  9.387852  9.807600 
    ##     16449     16450     16451     16452     16453     16454     16455     16456 
    ## 10.332286 12.221153 10.227349  9.177978  8.338482  8.023670  9.912537 12.955712 
    ##     16457     16458     16459     16460     16461     16462     16463     16464 
    ## 12.535964 10.856971 10.856971 10.437223  9.807600 13.375460 11.171782 11.381656 
    ##     16465     16466     16467     16468     16469     16470     16471     16472 
    ## 10.856971  7.603922 11.171782 11.276719  8.758230  7.079237  7.813796 12.431027 
    ##     16473     16474     16475     16476     16477     16478     16479     16480 
    ## 10.647097 10.752034  8.653293 12.850775 12.535964 13.060649 11.801404 10.017474 
    ##     16481     16482     16483     16484     16485     16486     16487     16488 
    ## 11.171782 10.332286  9.282915  7.079237  9.702663  7.708859  8.968104  8.338482 
    ##     16489     16490     16491     16492     16493     16494     16495     16496 
    ## 10.227349  9.912537 12.116216 12.431027  9.492789  8.338482  8.758230  7.918733 
    ##     16497     16498     16499     16500     16501     16502     16503     16504 
    ## 13.270523 12.326090  7.079237  8.023670  7.603922  9.807600 11.801404 13.165586 
    ##     16505     16506     16507     16508     16509     16510     16511     16512 
    ##  9.282915 11.276719 10.332286  8.023670  7.603922 10.017474  9.282915  8.548356 
    ##     16513     16514     16515     16516     16517     16518     16519     16520 
    ## 10.961908  8.758230 10.017474 12.535964 10.332286  9.492789 12.850775 13.270523 
    ##     16521     16522     16523     16524     16525     16526     16527     16528 
    ## 12.011279 13.375460 10.332286  9.073041 10.856971 12.640901 10.122412 11.591530 
    ##     16529     16530     16531     16532     16533     16534     16535     16536 
    ##  8.758230  8.863167  8.863167  9.073041 13.270523 11.696467 12.640901 11.171782 
    ##     16537     16538     16539     16540     16541     16542     16543     16544 
    ## 11.486593  8.443419 10.122412  8.443419 13.165586 12.535964 11.696467  9.387852 
    ##     16545     16546     16547     16548     16549     16550     16551     16552 
    ##  8.863167  8.338482 10.332286  8.338482 13.060649 12.011279 11.066845 11.486593 
    ##     16553     16554     16555     16556     16557     16558     16559     16560 
    ##  7.603922  8.758230 12.535964 11.486593 11.486593  9.807600 12.116216  9.912537 
    ##     16561     16562     16563     16564     16565     16566     16567     16568 
    ## 10.017474  8.863167 12.221153  9.177978 11.801404 12.116216  8.863167  9.073041 
    ##     16569     16570     16571     16572     16573     16574     16575     16576 
    ##  8.233544 12.745838 10.752034 11.381656 10.332286  9.073041  8.128607  7.918733 
    ##     16577     16578     16579     16580     16581     16582     16583     16584 
    ## 11.381656  8.338482 13.060649  8.968104  8.653293  9.387852 12.326090 10.017474 
    ##     16585     16586     16587     16588     16589     16590     16591     16592 
    ##  9.073041  8.128607 10.332286  8.233544 10.856971  8.128607 11.486593 10.227349 
    ##     16593     16594     16595     16596     16597     16598     16599     16600 
    ##  8.863167 12.850775 10.437223  8.758230  8.443419  8.128607 10.856971  8.338482 
    ##     16601     16602     16603     16604     16605     16606     16607     16608 
    ## 10.332286 10.961908 11.486593  8.863167  7.918733 11.696467 11.276719 13.270523 
    ##     16609     16610     16611     16612     16613     16614     16615     16616 
    ## 13.585334 13.480397 13.375460 13.375460 12.326090 12.326090 12.955712 10.122412 
    ##     16617     16618     16619     16620     16621     16622     16623     16624 
    ##  8.968104  8.233544  7.079237 10.227349  9.073041  8.863167  9.177978 10.122412 
    ##     16625     16626     16627     16628     16629     16630     16631     16632 
    ## 10.122412 11.591530 10.752034  9.492789 12.850775 12.745838 10.647097  9.597726 
    ##     16633     16634     16635     16636     16637     16638     16639     16640 
    ##  7.079237 12.116216  9.702663 12.955712 10.122412 12.850775  9.492789 10.227349 
    ##     16641     16642     16643     16644     16645     16646     16647     16648 
    ## 10.437223  9.702663  8.863167  8.548356  9.177978 11.906342 12.011279 12.011279 
    ##     16649     16650     16651     16652     16653     16654     16655     16656 
    ##  9.387852 11.381656 12.745838 11.171782 11.276719  7.603922  9.073041  7.813796 
    ##     16657     16658     16659     16660     16661     16662     16663     16664 
    ##  8.338482  9.387852 12.640901 10.122412 10.437223 12.535964  8.443419  8.233544 
    ##     16665     16666     16667     16668     16669     16670     16671     16672 
    ##  9.282915  8.863167  9.492789 13.060649 12.011279 10.542160  8.443419 10.437223 
    ##     16673     16674     16675     16676     16677     16678     16679     16680 
    ##  8.023670 11.486593  9.282915  7.079237  7.813796 10.122412 13.375460  9.597726 
    ##     16681     16682     16683     16684     16685     16686     16687     16688 
    ## 10.227349 11.906342  9.282915 11.696467 11.696467 11.066845  7.708859  7.918733 
    ##     16689     16690     16691     16692     16693     16694     16695     16696 
    ## 11.381656 12.431027  8.653293  9.912537  8.653293 11.906342  9.807600  8.968104 
    ##     16697     16698     16699     16700     16701     16702     16703     16704 
    ##  9.702663 12.011279  9.702663  9.492789 12.535964  8.548356  8.758230 10.122412 
    ##     16705     16706     16707     16708     16709     16710     16711     16712 
    ##  8.023670  9.073041  8.338482 12.955712 12.535964  8.548356  7.603922  8.338482 
    ##     16713     16714     16715     16716     16717     16718     16719     16720 
    ## 12.431027 12.535964  9.492789  8.758230  8.548356  7.813796  9.073041 10.332286 
    ##     16721     16722     16723     16724     16725     16726     16727     16728 
    ## 10.961908 11.486593 10.752034  8.338482 12.221153 14.005083 14.005083  7.918733 
    ##     16729     16730     16731     16732     16733     16734     16735     16736 
    ## 12.326090 13.060649 14.214957 13.690271 13.690271 13.690271 13.585334 14.214957 
    ##     16737     16738     16739     16740     16741     16742     16743     16744 
    ## 13.585334 14.005083 13.375460 14.319894 13.690271 13.795209 14.424831 14.424831 
    ##     16745     16746     16747     16748     16749     16750     16751     16752 
    ## 13.585334 13.480397 10.437223 12.011279 12.326090  8.443419  8.863167 10.122412 
    ##     16753     16754     16755     16756     16757     16758     16759     16760 
    ## 12.535964  9.912537 11.276719  9.387852 12.955712 12.116216 10.856971 12.116216 
    ##     16761     16762     16763     16764     16765     16766     16767     16768 
    ## 13.060649 13.060649 11.276719 10.856971 12.640901 10.017474  9.387852  8.968104 
    ##     16769     16770     16771     16772     16773     16774     16775     16776 
    ## 11.171782 10.332286  8.758230  8.758230 10.542160  9.702663 11.066845  8.233544 
    ##     16777     16778     16779     16780     16781     16782     16783     16784 
    ## 12.745838  9.492789 11.801404 11.381656  9.912537  7.079237 11.066845 11.591530 
    ##     16785     16786     16787     16788     16789     16790     16791     16792 
    ## 10.437223  7.918733  7.079237 10.752034 11.066845 11.696467 14.110020 12.011279 
    ##     16793     16794     16795     16796     16797     16798     16799     16800 
    ## 10.961908 10.647097 12.745838 10.961908 11.696467  8.548356 13.270523 10.542160 
    ##     16801     16802     16803     16804     16805     16806     16807     16808 
    ##  9.702663 10.017474  8.863167 12.955712 12.431027 12.221153 11.801404  9.282915 
    ##     16809     16810     16811     16812     16813     16814     16815     16816 
    ##  9.912537  8.443419  9.912537  9.282915 12.535964  7.813796  8.338482 11.591530 
    ##     16817     16818     16819     16820     16821     16822     16823     16824 
    ##  9.597726 13.165586 11.591530  9.912537 10.856971  9.177978 12.745838 12.640901 
    ##     16825     16826     16827     16828     16829     16830     16831     16832 
    ## 12.116216  9.807600  8.548356  8.023670 10.752034 11.276719 13.480397  8.233544 
    ##     16833     16834     16835     16836     16837     16838     16839     16840 
    ##  9.912537  8.968104  8.443419  8.653293 11.906342  9.807600  8.338482 10.437223 
    ##     16841     16842     16843     16844     16845     16846     16847     16848 
    ## 12.011279 10.856971  8.968104 10.752034  8.338482 12.326090 10.856971 12.116216 
    ##     16849     16850     16851     16852     16853     16854     16855     16856 
    ##  8.338482  9.912537 11.906342  9.492789 12.745838  8.128607  8.023670 10.752034 
    ##     16857     16858     16859     16860     16861     16862     16863     16864 
    ## 12.011279 13.270523 12.431027 10.017474 14.005083 14.214957 13.585334 14.214957 
    ##     16865     16866     16867     16868     16869     16870     16871     16872 
    ## 13.690271 13.480397 13.690271 14.319894 14.005083 13.480397 14.005083 14.110020 
    ##     16873     16874     16875     16876     16877     16878     16879     16880 
    ## 13.900146 14.214957 14.424831 14.110020 14.214957 13.480397 14.110020 14.319894 
    ##     16881     16882     16883     16884     16885     16886     16887     16888 
    ## 13.900146 14.319894 13.585334 13.900146 13.690271 14.319894 14.110020 13.480397 
    ##     16889     16890     16891     16892     16893     16894     16895     16896 
    ## 13.480397 13.795209 13.480397 14.424831 13.480397 14.424831 13.480397 14.110020 
    ##     16897     16898     16899     16900     16901     16902     16903     16904 
    ## 14.424831  8.338482 12.431027 12.535964 10.961908  8.233544 10.017474  9.807600 
    ##     16905     16906     16907     16908     16909     16910     16911     16912 
    ##  7.603922  8.653293  9.387852 11.906342 12.221153 13.060649 10.752034 11.381656 
    ##     16913     16914     16915     16916     16917     16918     16919     16920 
    ## 12.326090 12.326090 12.535964 12.011279  9.597726  9.702663  8.653293  8.338482 
    ##     16921     16922     16923     16924     16925     16926     16927     16928 
    ##  9.597726  8.863167  8.863167  9.807600 12.640901 10.856971 11.486593  7.079237 
    ##     16929     16930     16931     16932     16933     16934     16935     16936 
    ## 12.011279  9.912537  9.177978  9.073041  8.233544  9.807600 11.381656 10.227349 
    ##     16937     16938     16939     16940     16941     16942     16943     16944 
    ## 10.647097  9.702663  9.073041 11.486593  8.653293 12.431027 12.011279  7.603922 
    ##     16945     16946     16947     16948     16949     16950     16951     16952 
    ##  9.073041 12.745838 11.906342  9.387852 12.011279 11.381656 13.270523  9.912537 
    ##     16953     16954     16955     16956     16957     16958     16959     16960 
    ##  7.079237 11.486593 11.171782  8.548356  8.653293 11.381656 12.011279  8.023670 
    ##     16961     16962     16963     16964     16965     16966     16967     16968 
    ## 12.431027 10.437223 13.270523 11.801404  8.968104 10.752034 10.227349  9.807600 
    ##     16969     16970     16971     16972     16973     16974     16975     16976 
    ## 10.122412  7.813796 12.745838 12.221153 10.122412  8.653293  8.758230  8.758230 
    ##     16977     16978     16979     16980     16981     16982     16983     16984 
    ## 10.437223 12.326090  8.338482 12.955712  8.233544  7.079237 12.640901 10.961908 
    ##     16985     16986     16987     16988     16989     16990     16991     16992 
    ## 10.437223  9.597726  7.603922  9.912537 12.640901 12.326090  9.387852  8.863167 
    ##     16993     16994     16995     16996     16997     16998     16999     17000 
    ##  9.387852 11.696467  8.548356 11.381656  8.233544 10.227349  9.912537  9.073041 
    ##     17001     17002     17003     17004     17005     17006     17007     17008 
    ## 11.591530  7.603922 11.591530  8.128607  9.807600 11.696467  8.968104 12.955712 
    ##     17009     17010     17011     17012     17013     17014     17015     17016 
    ## 12.011279 12.535964 11.906342  8.443419 11.591530 11.486593  7.708859 12.955712 
    ##     17017     17018     17019     17020     17021     17022     17023     17024 
    ## 10.647097  9.807600  8.758230  9.177978  7.603922 12.326090 12.955712  8.443419 
    ##     17025     17026     17027     17028     17029     17030     17031     17032 
    ## 12.745838 11.591530  9.492789 11.381656  8.863167  7.079237  9.387852 12.850775 
    ##     17033     17034     17035     17036     17037     17038     17039     17040 
    ##  9.492789 11.801404  7.918733  9.282915 11.381656 11.696467  7.079237 12.011279 
    ##     17041     17042     17043     17044     17045     17046     17047     17048 
    ##  9.177978  9.073041 10.122412  9.177978 12.640901  7.813796  8.443419 10.332286 
    ##     17049     17050     17051     17052     17053     17054     17055     17056 
    ## 12.431027  9.702663 11.696467  8.548356  9.282915 11.696467 12.221153  8.863167 
    ##     17057     17058     17059     17060     17061     17062     17063     17064 
    ## 10.856971  8.548356 11.486593  8.233544 12.221153  9.912537 11.591530  7.603922 
    ##     17065     17066     17067     17068     17069     17070     17071     17072 
    ## 10.332286 11.381656 12.745838 11.591530 12.955712  9.807600 12.535964 12.011279 
    ##     17073     17074     17075     17076     17077     17078     17079     17080 
    ## 12.640901  9.702663  8.338482 10.856971  8.653293  8.758230 12.326090 11.696467 
    ##     17081     17082     17083     17084     17085     17086     17087     17088 
    ## 11.381656  7.079237  8.443419  7.079237  8.023670 12.011279  8.758230 12.850775 
    ##     17089     17090     17091     17092     17093     17094     17095     17096 
    ##  8.863167 10.856971 12.221153  8.548356  9.282915  8.758230 12.745838  8.968104 
    ##     17097     17098     17099     17100     17101     17102     17103     17104 
    ##  8.128607 11.801404 11.591530  7.079237 11.696467  8.653293  7.079237 12.221153 
    ##     17105     17106     17107     17108     17109     17110     17111     17112 
    ##  7.813796  7.079237  9.387852  8.968104  8.863167 12.955712 12.221153 11.486593 
    ##     17113     17114     17115     17116     17117     17118     17119     17120 
    ##  9.597726 12.745838 12.431027  7.918733  9.597726  8.863167 12.431027 10.437223 
    ##     17121     17122     17123     17124     17125     17126     17127     17128 
    ## 10.017474 11.906342 10.332286  9.073041  9.702663 12.850775 11.066845 12.116216 
    ##     17129     17130     17131     17132     17133     17134     17135     17136 
    ## 11.696467  9.912537 10.437223 10.542160  7.603922 11.801404 12.221153 11.906342 
    ##     17137     17138     17139     17140     17141     17142     17143     17144 
    ## 12.431027 12.745838  9.282915 12.431027  7.079237 12.326090  7.708859 12.535964 
    ##     17145     17146     17147     17148     17149     17150     17151     17152 
    ## 11.801404 12.850775 10.856971 11.381656 11.066845  8.443419  8.233544 11.171782 
    ##     17153     17154     17155     17156     17157     17158     17159     17160 
    ##  9.282915 11.486593  7.603922 10.752034 12.011279 11.801404  9.912537  9.912537 
    ##     17161     17162     17163     17164     17165     17166     17167     17168 
    ## 11.066845 12.745838 12.955712 12.431027  8.863167 12.221153  9.597726  9.282915 
    ##     17169     17170     17171     17172     17173     17174     17175     17176 
    ## 12.116216 12.221153  7.813796  8.338482  9.912537 11.066845 11.486593  9.177978 
    ##     17177     17178     17179     17180     17181     17182     17183     17184 
    ##  8.548356 10.017474 12.431027 11.486593 10.647097  8.968104  8.338482  8.338482 
    ##     17185     17186     17187     17188     17189     17190     17191     17192 
    ## 12.326090 11.171782  8.863167 11.381656  7.603922  7.079237 12.850775  9.177978 
    ##     17193     17194     17195     17196     17197     17198     17199     17200 
    ## 13.165586 13.165586  8.653293 12.640901 10.122412  9.282915 13.060649 12.431027 
    ##     17201     17202     17203     17204     17205     17206     17207     17208 
    ## 12.745838 11.276719 11.906342  8.863167  9.387852 12.221153  8.128607  9.492789 
    ##     17209     17210     17211     17212     17213     17214     17215     17216 
    ##  8.968104  8.548356  7.079237 12.326090 13.270523  7.603922  8.758230 11.171782 
    ##     17217     17218     17219     17220     17221     17222     17223     17224 
    ## 10.332286 12.640901 11.066845 11.486593  8.548356  8.128607 10.122412  9.177978 
    ##     17225     17226     17227     17228     17229     17230     17231     17232 
    ## 11.906342  8.233544  8.338482 10.227349 11.276719  8.968104  7.708859 10.856971 
    ##     17233     17234     17235     17236     17237     17238     17239     17240 
    ## 10.437223  8.233544 11.696467 13.270523 12.221153  8.338482 10.856971 13.270523 
    ##     17241     17242     17243     17244     17245     17246     17247     17248 
    ## 10.122412 10.227349 11.906342 13.165586 12.326090 12.116216  7.918733  8.128607 
    ##     17249     17250     17251     17252     17253     17254     17255     17256 
    ## 13.375460  8.233544 12.116216  7.813796  7.079237  9.177978 11.066845 12.011279 
    ##     17257     17258     17259     17260     17261     17262     17263     17264 
    ## 11.171782  8.338482  7.708859  7.813796  8.023670 10.227349  9.702663 12.745838 
    ##     17265     17266     17267     17268     17269     17270     17271     17272 
    ##  9.387852  9.073041 12.011279 12.221153 10.961908  7.079237  7.813796 10.542160 
    ##     17273     17274     17275     17276     17277     17278     17279     17280 
    ## 12.326090  8.548356  7.708859  8.863167  9.492789 10.961908  9.073041 13.060649 
    ##     17281     17282     17283     17284     17285     17286     17287     17288 
    ##  9.282915 10.122412  8.758230  9.387852 11.276719  7.918733  7.603922 13.060649 
    ##     17289     17290     17291     17292     17293     17294     17295     17296 
    ##  7.603922  7.603922  8.338482 10.961908 12.221153  9.177978  7.603922 12.326090 
    ##     17297     17298     17299     17300     17301     17302     17303     17304 
    ## 13.060649  7.079237  7.079237  9.282915  8.443419  9.702663 11.276719 11.171782 
    ##     17305     17306     17307     17308     17309     17310     17311     17312 
    ## 10.647097  9.597726 10.122412 11.171782  9.073041  8.758230  9.597726 10.647097 
    ##     17313     17314     17315     17316     17317     17318     17319     17320 
    ##  8.128607 11.276719  7.918733  9.387852 12.011279  8.128607 13.375460 11.696467 
    ##     17321     17322     17323     17324     17325     17326     17327     17328 
    ##  9.912537 12.011279  9.912537 12.850775 12.431027 10.961908 12.640901  9.492789 
    ##     17329     17330     17331     17332     17333     17334     17335     17336 
    ## 11.591530 11.801404  9.073041 10.542160  8.548356  9.282915 12.745838  9.177978 
    ##     17337     17338     17339     17340     17341     17342     17343     17344 
    ## 12.116216  8.128607 11.381656 10.437223 12.640901 10.752034  7.813796 11.171782 
    ##     17345     17346     17347     17348     17349     17350     17351     17352 
    ##  8.968104  7.079237 10.227349  9.073041 10.961908  8.443419 10.856971  8.653293 
    ##     17353     17354     17355     17356     17357     17358     17359     17360 
    ##  7.708859 12.535964  9.387852  8.128607  7.603922 11.381656 12.011279 10.332286 
    ##     17361     17362     17363     17364     17365     17366     17367     17368 
    ##  9.282915 10.227349 11.276719  8.758230  9.492789  7.918733  9.912537  8.338482 
    ##     17369     17370     17371     17372     17373     17374     17375     17376 
    ##  8.548356 10.961908 11.276719  8.548356 13.165586  8.968104 11.381656  8.548356 
    ##     17377     17378     17379     17380     17381     17382     17383     17384 
    ##  7.813796 10.122412 12.745838  9.597726 12.431027  8.863167 13.165586 12.745838 
    ##     17385     17386     17387     17388     17389     17390     17391     17392 
    ## 11.696467 12.535964  9.177978  8.968104  8.653293  8.233544  7.918733 12.431027 
    ##     17393     17394     17395     17396     17397     17398     17399     17400 
    ##  9.177978 10.017474  8.863167 12.640901  8.968104 10.017474 11.906342  8.548356 
    ##     17401     17402     17403     17404     17405     17406     17407     17408 
    ##  7.079237 12.535964 11.381656 10.752034 10.856971  9.702663  8.233544  9.597726 
    ##     17409     17410     17411     17412     17413     17414     17415     17416 
    ##  9.492789  9.073041 11.066845  9.492789 12.221153 12.745838 13.060649  8.758230 
    ##     17417     17418     17419     17420     17421     17422     17423     17424 
    ## 12.955712 10.227349  9.387852  9.702663  8.128607  7.079237  7.079237 11.696467 
    ##     17425     17426     17427     17428     17429     17430     17431     17432 
    ## 10.752034 10.332286 10.227349  8.023670  9.912537  7.603922 10.961908 11.276719 
    ##     17433     17434     17435     17436     17437     17438     17439     17440 
    ## 10.542160  7.603922 11.381656 10.542160 12.116216 10.542160  8.863167  9.177978 
    ##     17441     17442     17443     17444     17445     17446     17447     17448 
    ## 10.647097 10.647097 12.431027 11.066845 10.437223 12.116216 12.011279 11.381656 
    ##     17449     17450     17451     17452     17453     17454     17455     17456 
    ## 12.431027 10.227349 10.437223 13.900146 11.381656 13.690271 14.319894 13.795209 
    ##     17457     17458     17459     17460     17461     17462     17463     17464 
    ## 13.585334 13.795209 13.795209 13.690271 14.319894 14.005083 14.319894 14.214957 
    ##     17465     17466     17467     17468     17469     17470     17471     17472 
    ## 14.214957 14.319894 13.375460 14.110020 14.110020 11.171782  9.597726 10.961908 
    ##     17473     17474     17475     17476     17477     17478     17479     17480 
    ## 11.066845 12.745838 11.486593 12.850775 11.171782 10.122412 11.906342 11.801404 
    ##     17481     17482     17483     17484     17485     17486     17487     17488 
    ## 13.375460  7.603922 11.381656 12.221153  9.177978 10.437223 12.221153 13.165586 
    ##     17489     17490     17491     17492     17493     17494     17495     17496 
    ##  7.918733  7.708859 10.122412 12.011279  9.912537  8.758230 10.752034 10.542160 
    ##     17497     17498     17499     17500     17501     17502     17503     17504 
    ## 13.060649 12.326090 10.752034  9.387852 12.850775  8.233544 11.381656  9.177978 
    ##     17505     17506     17507     17508     17509     17510     17511     17512 
    ##  8.548356  8.023670 12.326090 10.332286  7.603922  9.807600  8.758230 12.116216 
    ##     17513     17514     17515     17516     17517     17518     17519     17520 
    ## 11.276719 10.017474  8.233544 10.227349  9.492789 10.122412 10.752034 12.326090 
    ##     17521     17522     17523     17524     17525     17526     17527     17528 
    ## 11.801404 12.640901 12.221153 10.017474  8.548356 12.326090  8.548356 11.696467 
    ##     17529     17530     17531     17532     17533     17534     17535     17536 
    ##  9.282915  7.918733  8.653293  8.653293 12.011279  9.597726  9.177978  8.023670 
    ##     17537     17538     17539     17540     17541     17542     17543     17544 
    ## 10.122412 12.326090 10.752034 10.647097  8.548356 10.437223  9.282915  9.702663 
    ##     17545     17546     17547     17548     17549     17550     17551     17552 
    ##  9.387852  7.603922  8.338482 11.171782 10.122412  9.492789  8.128607 12.850775 
    ##     17553     17554     17555     17556     17557     17558     17559     17560 
    ##  8.548356 10.227349  9.387852 11.696467 12.850775 12.535964  7.813796  8.128607 
    ##     17561     17562     17563     17564     17565     17566     17567     17568 
    ## 12.535964  9.492789 10.437223 10.227349  9.073041  7.079237  7.079237 10.647097 
    ##     17569     17570     17571     17572     17573     17574     17575     17576 
    ## 10.542160 10.332286  9.807600  9.492789  9.387852  7.918733 10.122412 11.276719 
    ##     17577     17578     17579     17580     17581     17582     17583     17584 
    ## 13.060649  7.603922 11.381656 11.696467 11.696467 10.332286 12.745838 12.116216 
    ##     17585     17586     17587     17588     17589     17590     17591     17592 
    ##  8.758230 10.437223 11.171782  9.492789  9.912537  9.807600 12.221153 12.850775 
    ##     17593     17594     17595     17596     17597     17598     17599     17600 
    ##  7.918733  7.813796  9.492789  7.813796 14.110020 10.017474 12.011279 10.856971 
    ##     17601     17602     17603     17604     17605     17606     17607     17608 
    ## 10.122412 12.850775  9.807600  8.023670 10.752034  9.807600  9.177978 10.647097 
    ##     17609     17610     17611     17612     17613     17614     17615     17616 
    ## 12.011279 10.856971 11.486593  8.653293 12.745838 13.375460 14.424831 14.424831 
    ##     17617     17618     17619     17620     17621     17622     17623     17624 
    ## 14.005083 13.900146 14.214957 13.795209 13.480397 14.110020 14.214957 14.005083 
    ##     17625     17626     17627     17628     17629     17630     17631     17632 
    ## 13.900146  9.387852 11.066845 11.696467  7.603922 10.647097  9.912537 12.431027 
    ##     17633     17634     17635     17636     17637     17638     17639     17640 
    ##  9.702663 12.955712 11.906342 12.850775  9.702663 10.961908 12.116216 11.171782 
    ##     17641     17642     17643     17644     17645     17646     17647     17648 
    ## 11.696467  9.912537  9.702663  8.968104  8.338482 12.745838 10.017474  8.443419 
    ##     17649     17650     17651     17652     17653     17654     17655     17656 
    ## 10.227349 11.906342  9.807600 12.850775 11.696467 12.535964 11.381656 12.011279 
    ##     17657     17658     17659     17660     17661     17662     17663     17664 
    ## 11.381656 11.171782 10.542160 12.431027  9.702663  8.443419  8.023670 10.017474 
    ##     17665     17666     17667     17668     17669     17670     17671     17672 
    ## 11.906342 10.437223  9.807600 11.906342  8.968104  9.912537  9.387852  9.912537 
    ##     17673     17674     17675     17676     17677     17678     17679     17680 
    ## 11.066845 12.955712  9.073041 11.486593 12.640901 10.961908  8.758230 11.801404 
    ##     17681     17682     17683     17684     17685     17686     17687     17688 
    ## 10.332286 12.011279 11.591530  8.023670  9.282915  7.603922 12.850775  9.177978 
    ##     17689     17690     17691     17692     17693     17694     17695     17696 
    ##  9.177978  9.912537 11.066845  8.548356 12.955712  7.079237 11.696467  9.702663 
    ##     17697     17698     17699     17700     17701     17702     17703     17704 
    ## 12.745838  9.177978  9.807600 10.122412 13.060649 11.696467 12.955712 12.116216 
    ##     17705     17706     17707     17708     17709     17710     17711     17712 
    ## 11.801404  9.807600  8.653293 13.060649 12.221153 13.060649 13.060649  7.708859 
    ##     17713     17714     17715     17716     17717     17718     17719     17720 
    ##  8.863167  7.079237 12.011279  7.813796 12.431027 12.745838 12.745838 10.961908 
    ##     17721     17722     17723     17724     17725     17726     17727     17728 
    ## 12.955712 10.961908 10.227349 13.270523 12.221153 12.850775 11.381656 13.480397 
    ##     17729     17730     17731     17732     17733     17734     17735     17736 
    ## 10.647097 13.900146 13.795209 14.319894 13.900146 13.795209 14.319894 13.900146 
    ##     17737     17738     17739     17740     17741     17742     17743     17744 
    ## 14.110020 14.214957 13.480397 13.480397 14.319894 14.110020 14.005083 14.005083 
    ##     17745     17746     17747     17748     17749     17750     17751     17752 
    ## 14.214957 14.005083 13.585334 13.480397 13.795209 14.214957 14.214957 14.424831 
    ##     17753     17754     17755     17756     17757     17758     17759     17760 
    ## 14.424831 13.690271 13.900146 14.424831 13.585334 13.375460 13.585334 13.585334 
    ##     17761     17762     17763     17764     17765     17766     17767     17768 
    ## 14.005083 13.900146 14.214957 14.005083 13.900146 14.214957 13.900146 12.221153 
    ##     17769     17770     17771     17772     17773     17774     17775     17776 
    ## 12.850775  8.653293  8.338482 11.591530 10.647097 10.017474 11.906342  8.128607 
    ##     17777     17778     17779     17780     17781     17782     17783     17784 
    ##  7.603922 11.591530 10.752034 11.171782  7.603922 12.850775 11.591530  7.603922 
    ##     17785     17786     17787     17788     17789     17790     17791     17792 
    ##  9.492789 12.011279 10.227349 11.381656  8.968104 10.437223  8.653293 10.752034 
    ##     17793     17794     17795     17796     17797     17798     17799     17800 
    ##  8.653293  9.073041  8.338482 12.116216 10.017474  7.079237 12.640901  8.968104 
    ##     17801     17802     17803     17804     17805     17806     17807     17808 
    ##  7.079237  7.813796 12.221153  8.758230 10.856971  7.079237  8.128607  9.282915 
    ##     17809     17810     17811     17812     17813     17814     17815     17816 
    ##  7.708859 12.535964 11.696467 12.431027 12.535964  7.813796 12.431027  9.912537 
    ##     17817     17818     17819     17820     17821     17822     17823     17824 
    ##  8.653293 10.752034 13.165586  9.073041 11.276719  8.863167 11.696467  8.863167 
    ##     17825     17826     17827     17828     17829     17830     17831     17832 
    ## 11.906342  9.387852  8.338482  8.338482  9.073041 13.165586 12.221153 11.906342 
    ##     17833     17834     17835     17836     17837     17838     17839     17840 
    ## 12.116216  9.387852 13.060649 11.171782 11.906342  8.233544  8.968104 11.381656 
    ##     17841     17842     17843     17844     17845     17846     17847     17848 
    ## 10.752034  8.233544  7.813796 13.060649  9.912537  9.387852  8.758230  8.443419 
    ##     17849     17850     17851     17852     17853     17854     17855     17856 
    ## 10.961908  8.758230  9.702663 12.850775  8.338482  9.912537 12.011279 12.850775 
    ##     17857     17858     17859     17860     17861     17862     17863     17864 
    ##  8.863167 12.431027  8.023670  9.177978 10.332286  8.863167 10.017474 11.486593 
    ##     17865     17866     17867     17868     17869     17870     17871     17872 
    ##  9.073041  8.128607  7.603922  7.603922 10.961908  8.653293  8.863167 10.332286 
    ##     17873     17874     17875     17876     17877     17878     17879     17880 
    ##  9.597726 11.906342 10.542160  9.282915  9.702663  9.912537 11.066845  9.282915 
    ##     17881     17882     17883     17884     17885     17886     17887     17888 
    ##  8.863167 10.437223 12.850775 11.276719 10.542160 12.431027 11.591530  7.079237 
    ##     17889     17890     17891     17892     17893     17894     17895     17896 
    ## 10.647097 13.375460  9.387852  8.863167 12.221153 10.542160  8.758230 12.431027 
    ##     17897     17898     17899     17900     17901     17902     17903     17904 
    ##  9.807600  9.597726 11.381656 12.955712 10.017474  8.548356  8.548356  9.282915 
    ##     17905     17906     17907     17908     17909     17910     17911     17912 
    ##  8.758230 10.542160  7.603922 10.437223 12.011279 13.270523  7.603922  7.079237 
    ##     17913     17914     17915     17916     17917     17918     17919     17920 
    ## 10.856971 11.801404  8.863167 11.906342 12.116216  8.548356 10.752034 10.647097 
    ##     17921     17922     17923     17924     17925     17926     17927     17928 
    ## 13.375460  9.597726 11.906342 10.856971  8.758230 12.955712 13.060649 11.696467 
    ##     17929     17930     17931     17932     17933     17934     17935     17936 
    ## 12.640901  8.758230 10.227349 12.326090  7.603922 10.017474  9.492789  9.492789 
    ##     17937     17938     17939     17940     17941     17942     17943     17944 
    ##  9.073041 12.116216 10.437223  7.918733  7.813796 13.270523 10.961908 12.535964 
    ##     17945     17946     17947     17948     17949     17950     17951     17952 
    ## 11.381656 10.961908 11.066845  7.708859  9.912537 10.332286 12.431027 12.011279 
    ##     17953     17954     17955     17956     17957     17958     17959     17960 
    ##  9.912537 12.431027 13.270523 10.437223  8.443419  9.073041  8.548356  8.653293 
    ##     17961     17962     17963     17964     17965     17966     17967     17968 
    ##  7.079237 10.437223  7.603922 12.431027 12.116216 11.696467 12.535964  8.968104 
    ##     17969     17970     17971     17972     17973     17974     17975     17976 
    ## 11.066845  7.603922  8.443419 10.332286 10.647097  9.807600  7.813796  9.073041 
    ##     17977     17978     17979     17980     17981     17982     17983     17984 
    ##  7.603922 10.437223  8.548356 10.542160 10.017474  7.603922  9.597726 10.227349 
    ##     17985     17986     17987     17988     17989     17990     17991     17992 
    ## 10.017474  9.177978 10.437223  8.128607  8.233544  8.443419  9.807600  9.282915 
    ##     17993     17994     17995     17996     17997     17998     17999     18000 
    ##  9.492789 10.542160 11.906342 13.165586 13.060649 10.437223  8.863167  7.079237 
    ##     18001     18002     18003     18004     18005     18006     18007     18008 
    ## 12.221153 12.116216 10.227349 10.856971 12.955712  8.863167 11.381656 11.591530 
    ##     18009     18010     18011     18012     18013     18014     18015     18016 
    ##  9.282915 11.381656  8.863167 13.060649  9.807600 12.955712  9.597726  7.079237 
    ##     18017     18018     18019     18020     18021     18022     18023     18024 
    ##  8.548356 11.591530  8.653293  7.918733 10.856971 12.326090 11.486593  9.177978 
    ##     18025     18026     18027     18028     18029     18030     18031     18032 
    ## 10.856971 11.066845  7.918733 11.381656  9.177978  7.079237 12.745838 11.591530 
    ##     18033     18034     18035     18036     18037     18038     18039     18040 
    ##  9.282915 10.856971 10.542160 13.060649  9.807600 12.535964 12.535964  9.807600 
    ##     18041     18042     18043     18044     18045     18046     18047     18048 
    ##  9.177978  8.023670 11.906342 12.640901 11.066845  9.282915 12.116216 10.752034 
    ##     18049     18050     18051     18052     18053     18054     18055     18056 
    ## 12.011279  7.603922 10.647097 12.745838  8.338482 10.017474  9.597726 12.535964 
    ##     18057     18058     18059     18060     18061     18062     18063     18064 
    ## 12.431027  9.387852  9.282915 12.221153  8.863167  9.702663 11.801404  8.863167 
    ##     18065     18066     18067     18068     18069     18070     18071     18072 
    ##  9.282915  8.443419  8.653293 12.850775 12.955712 12.326090 12.850775  9.282915 
    ##     18073     18074     18075     18076     18077     18078     18079     18080 
    ## 12.116216 10.856971  9.492789  7.708859  7.603922  8.863167  9.597726  9.492789 
    ##     18081     18082     18083     18084     18085     18086     18087     18088 
    ## 11.906342 12.326090  8.863167  8.968104 11.381656 11.276719 10.856971 12.535964 
    ##     18089     18090     18091     18092     18093     18094     18095     18096 
    ##  9.807600  8.338482  9.282915  7.918733  9.702663 10.542160 10.227349 10.227349 
    ##     18097     18098     18099     18100     18101     18102     18103     18104 
    ## 12.326090 11.801404  9.282915  7.603922 10.752034 10.542160  8.758230  7.603922 
    ##     18105     18106     18107     18108     18109     18110     18111     18112 
    ## 10.961908 10.961908  9.807600 10.752034 10.332286 12.431027 10.647097 10.332286 
    ##     18113     18114     18115     18116     18117     18118     18119     18120 
    ##  9.807600 12.011279  8.548356 11.906342 11.486593 13.270523 11.591530 12.326090 
    ##     18121     18122     18123     18124     18125     18126     18127     18128 
    ## 12.326090  7.813796 11.906342  8.548356 11.591530 10.122412 11.801404 12.745838 
    ##     18129     18130     18131     18132     18133     18134     18135     18136 
    ## 10.122412  8.443419  8.233544  8.863167 12.535964  9.177978 10.752034 12.221153 
    ##     18137     18138     18139     18140     18141     18142     18143     18144 
    ##  8.023670 11.906342 12.116216  9.597726 11.801404 12.745838 10.122412  9.492789 
    ##     18145     18146     18147     18148     18149     18150     18151     18152 
    ## 11.276719  8.233544 12.850775 11.066845 10.647097 13.375460  8.338482  7.079237 
    ##     18153     18154     18155     18156     18157     18158     18159     18160 
    ## 10.227349  8.128607 10.961908  8.758230 12.535964  8.863167 12.326090 12.850775 
    ##     18161     18162     18163     18164     18165     18166     18167     18168 
    ## 10.332286  7.708859 10.122412 10.332286  9.073041  7.079237  9.177978  8.443419 
    ##     18169     18170     18171     18172     18173     18174     18175     18176 
    ## 11.486593  8.128607  9.073041 13.060649 12.850775  9.597726 10.122412 11.696467 
    ##     18177     18178     18179     18180     18181     18182     18183     18184 
    ## 12.116216  8.863167 12.955712 12.326090 11.066845 10.017474 10.437223 11.801404 
    ##     18185     18186     18187     18188     18189     18190     18191     18192 
    ##  9.807600  9.177978  9.807600  8.758230 12.116216  9.177978 12.955712 12.431027 
    ##     18193     18194     18195     18196     18197     18198     18199     18200 
    ##  7.603922 11.171782  9.702663 11.066845  9.912537 11.276719  8.548356  7.708859 
    ##     18201     18202     18203     18204     18205     18206     18207     18208 
    ## 10.752034 10.542160  9.912537 13.375460  9.387852 11.801404 12.326090  9.387852 
    ##     18209     18210     18211     18212     18213     18214     18215     18216 
    ## 12.116216 11.066845  9.807600  9.177978  8.968104 10.647097  7.079237 11.906342 
    ##     18217     18218     18219     18220     18221     18222     18223     18224 
    ##  9.282915 11.696467  9.177978 11.381656  9.597726  8.968104 11.381656 12.431027 
    ##     18225     18226     18227     18228     18229     18230     18231     18232 
    ##  7.918733  9.073041  8.023670 13.480397 14.110020 14.319894 13.795209 12.326090 
    ##     18233     18234     18235     18236     18237     18238     18239     18240 
    ## 10.647097  8.548356 11.171782 12.221153 10.752034  9.073041  8.128607 12.116216 
    ##     18241     18242     18243     18244     18245     18246     18247     18248 
    ##  9.912537 10.542160 11.066845 12.535964 12.535964  9.492789  8.233544  9.597726 
    ##     18249     18250     18251     18252     18253     18254     18255     18256 
    ## 11.486593 13.270523  8.338482  9.177978  7.813796  8.548356  7.708859  9.492789 
    ##     18257     18258     18259     18260     18261     18262     18263     18264 
    ##  8.548356  9.912537 11.381656  8.443419 10.122412 11.801404 10.647097  7.603922 
    ##     18265     18266     18267     18268     18269     18270     18271     18272 
    ## 10.017474 10.752034 12.955712 10.122412  8.968104  8.653293 10.542160 11.276719 
    ##     18273     18274     18275     18276     18277     18278     18279     18280 
    ##  7.603922 11.591530 10.437223  9.597726 10.437223 10.647097  9.387852  9.597726 
    ##     18281     18282     18283     18284     18285     18286     18287     18288 
    ## 11.801404 10.122412  7.603922 11.276719 10.122412  7.603922 10.332286  7.813796 
    ##     18289     18290     18291     18292     18293     18294     18295     18296 
    ##  8.548356  9.387852 10.017474 12.326090 13.270523 12.116216 11.696467  8.338482

``` r
fit %>% 
  broom::glance()
```

    ## # A tibble: 1 x 12
    ##   r.squared adj.r.squared sigma statistic    p.value    df  logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>      <dbl> <dbl>   <dbl>  <dbl>  <dbl>
    ## 1   0.00173       0.00168  46.0      31.7    1.79e-8     1 -96027. 1.92e5 1.92e5
    ## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
fit %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   16.0      1.01       15.8  3.65e-56
    ## 2 age           -0.105    0.0186     -5.63 1.79e- 8
