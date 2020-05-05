dat <- read.csv(file = "ahi-cesd.csv")
dim(dat)
head(dat)

# Clean data to create a subset that only 
# contains treatment assignment, ahiTotal,
# cesdTotal, id, and measurement occasion.
rct <- subset(x = dat, 
              select = c("id", "occasion", "intervention",
                         "ahiTotal", "cesdTotal"))
dim(rct)
head(rct)

# Create a factor for the intervention group
# variable. From the codebook:
#  Intervention group to which the participant was 
#  randomly assigned. The variable is integer-valued
#  with allowable values from 1-4 inclusive.
#   1 = "Using Signature Strengths"
#   2 = "Three Good Things"
#   3 = "Gratitude Visit"
#   4 = "Recording early memories" 
#   (used as the control condition)
rct$int_fact <- factor(x = rct$intervention, 
                       levels = 1:4,
                       labels = c("SS", "TGT", "GV", "REM"))
str(rct)

# From the code book:
#  Measurement occasion. The variable is 
#  integer-valued with an allowable range 
#  of 0-5 inclusive. 
#    0 = Pretest, i.e. , at enrolment
#    1 = Posttest, i.e. , 7 days after pretest
#    2 = 1-week follow-up, i.e. , 14 days after pretest (7 days after posttest).
#    3 = 1-month follow-up, i.e. , 38 days after pretest (31 days after posttest)
#    4 = 3-month follow-up, i.e. , 98 days after pretest (91 days after posttest)
#    5 = 6-month follow-up, i.e. , 189 days after pretest (182 days after posttest).

# Examine response occasion sample sizes
table(rct$occasion)

# We will study methods for repeated meausures
# eventually but for now we will look at "gain
# scores" by taking post - pre differences for 
# the primary outcomes.

# First, need to put the data into wide format.
# Use the reshape() function.
?reshape

# idvar is the ID for each participant.
# timevar is the variable in long format that
#  differentiations multiple records from the 
#  same participant.
# v.names specifies the names of variables that
#  should be spread out in wide format.
rct_wide <- reshape(data = rct, 
                    idvar = "id",
                    timevar = "occasion",
                    v.names = c("ahiTotal", "cesdTotal"),
                    direction = "wide")
dim(rct_wide)
head(rct_wide)

# Calculate gain scores
rct_wide$ahiGS <- rct_wide$ahiTotal.3 - rct_wide$ahiTotal.0
rct_wide$cesdGS <- rct_wide$cesdTotal.3 - rct_wide$cesdTotal.0

# Eliminate cases with missing data at one month follow-up
which(is.na(rct_wide$ahiGS))
rct_wide <- rct_wide[-which(is.na(rct_wide$ahiGS)),]
dim(rct_wide)

# Fitting the full model
head(rct_wide)
lm1 <- lm(ahiGS ~ int_fact, data = rct_wide)
summary(lm1)


## Descriptive statistics

# Group samples sizes:
table(rct_wide$int_fact)

# Calculating 95% C.I. for group mean
qt(p = .975, df = 135, lower.tail = TRUE)

6.31 + 1.98*2.01
6.31 - 1.98*2.01

# Using emmeans() to get C.I.
library(emmeans)
emm1 <- emmeans(object = lm1,
                specs = ~ int_fact)
emm1


## Testing a Single Factor Level Mean
# Calculating the p-value
(6.31 - 0)/2.01

pt(q = 3.14, df = 135, lower.tail = FALSE)

# Using the test() function
test(object = emm1, side = ">") # right-tailed


## Testing Pairwise Comparisons (SS vs TGT)
# Calculating the p-value
(6.31 - 3.16)/sqrt(116.79*(1/29 + 1/43))

2*pt(q = 1.21, df = 135, lower.tail = FALSE)

# Using the pairs() function
pairs(emm1, adjust = "none") # two-tailed


## Testing Linear Contrasts
# Calculate the p-value
((6.31 + 3.16 + 5.17)/3 - 3)/sqrt(116.79 * ((1/3)^2/29 + (1/3)^2/43 + (1/3)^2/30 + (-1)^2/37))

pt(q = 0.9, df = 135, lower.tail = FALSE)

# Using the contrast() function
contrast(emm1, method = list(c(1/3, 1/3, 1/3, -1))) # two-tailed

