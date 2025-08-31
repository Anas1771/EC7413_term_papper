#----#----#----#----#----#----#----#----#----#----#----#----#----#
#----#----#----#----#----#----#----#----#----#----#----#----#----#

#----#----#----#------## Term paper - EC7413 ##----#----#----#----#
#----#----#----#----#----## Anas Haouat ##----#----#----#----#----#

## RQ - How do monetary policy shocks, represented by the Riksbank policy rate,
## transmit to household mortgage rates and then to Swedish house prices

### Summary of the code:

# 0. Preparations:

# I. Data wrangling:
### 1. Variable 1 - Riksbank policy rate (rt)
### 2. Variable 2 - STIBOR 3M as proxy for mortgage rates (mt)
### 3. Variable 3 - House prices (ht)
### 4. Variable 4 - GDP growth (gdpt)
### 5. Visualisation
##### a. Policy rate
##### b. Mortgage rate
##### c. House prices
##### d. GDP growth
### 6. Housekeeping

# II. Analysis:
## 1. ADF tests
## 2. ACF graphs (seasonality):
## 3. Engle-Granger test
## 4. VAR in levels
## 5. IRF
## 6. FEVD

# III. Sensitivity analysis:
## 1. Cholesky ordering robustness change
## 2. Shock-size sensitivity for HPI's IRFs


#----#----#----#----#----#----#

# 0. Preparations:

## Relevant libraries:
library(pxweb) # pxweb_get()
library(zoo) # zoo()
library(readxl) # read_excel()
library(ggplot2) # ggplot()
library(urca) # ur.diff()
library(vars) # VAR()
library(scales)

##### Create a folder where to store relevant files:
dir.create("output", showWarnings = FALSE)

#----#----#----#----#----#----#

# I. Data wrangling:
## In this part, I will process raw data and produce clean time series for
## the four variables I will use in this study.

#----#----#----#----#----#----#

# 1. Variable 1 - Riksbank policy rate:

# Read the data - notice that the separator is ";" while the decimals are in ","
rt_df <- read.csv2("raw_data/Riksbank Quarterly Policy Rate.csv")

# Quarterly interest rates set by central bank
rt <- ts(rt_df$Average, start = c(1994,3), frequency = 4) # Convert to TS

# Sanity check:
frequency(rt)  # Should be 4
start(rt)      # Should be 1994 Q3
end(rt)        # Should be 2025 Q2


#----#----#----#----#----#----#

# 2. Variable 2 - STIBOR:

# Read the data:
stibor3m_m <- read.csv("raw_data/STIBOR 3M (ECB).csv")
names(stibor3m_m)[3] <- "rate" # Shorten column name for referencing
names(stibor3m_m)[1] <- "date" # same
stibor3m_m <- stibor3m_m[, c("date", "rate")] # Get rid of the superfluous columns


stibor3m_q <- with(stibor3m_m,as.ts(aggregate(zoo(rate, # Convert to a ts
                  as.Date(date)), as.yearqtr, mean) ))

mt <- stibor3m_q # Rename to match the rest of the variable, and a reminder that it is a proxy for mortgage rates

# Sanity check:
frequency(stibor3m_q)


#----#----#----#----#----#----#

# 3. Variable 3 - house prices (ht):

# First, I need the nominal house price index:
HPI <- read.csv("raw_data/BIS RPPI Housing Prices.csv") #Quarterly house prices indexed at 2010
HPIts <- ts(HPI$QSEN628BIS, start = c(1970, 1), frequency = 4) #Convert to a TS
plot(HPIts, main = "House Price Index", ylab = "Index", xlab = "Year") #Plot for visual inspection

# I also need the general price level index to deflate house prices by overall inflation:
# I use CPIF (KPIF in Swedish - consumer prices with fixed interest mortgage).

# Use the pxweb package to download the CPIF data from SCB:
pxweb_query_list <- 
  list("ContentsCode"=c("000005HR"),
       "Tid" = c("*"))
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101G/KPIF",
            query = pxweb_query_list)

CPIF_df <- as.data.frame(px_data) #Convert to a DF
CPIF <- as.numeric(CPIF_df$"KPIF, index, 1987=100") #Get the column I need and convert to a numeric vector
CPIFts <- ts(CPIF, start = c(1987,1), frequency = 12) #Convert to a TS

#Rebase to 2010 (not essential)
base_value <- mean(window(CPIFts, start = c(2010,1), end = c(2010,12))) #Compute the mean of the index year
CPIFts_rebased <- (CPIFts / base_value) * 100 #New index at = 2010 (instead of 1987)

# Sanity check if rebasing worked:
plot(CPIFts_rebased, main = "CPIF rebased to 2010=100")
abline(h = 100, col = "red", lty = 2)   # Reference line at 2010

# Converting the TS needs to quarterly from monthly:
CPIF_zoo <- aggregate(zoo(CPIFts_rebased), as.yearqtr, mean)   
CPIFts <- ts(CPIF_zoo, start = c(1987,1), frequency = 4) #Convert back to TS from zoo
frequency(CPIFts) # Sanity check if indeed quarterly

# Finally, I will compute the real house prices variable by deflating:
real_ht <- (HPIts/CPIFts)*100

# Then introducing the natural log:
ht <- log(real_ht)

# Sanity check - at 2010, I expect real_ht = 100, hence log(real_ht) = 4.605054
window(ht, start = c(2010,1), end = c(2010,4)) #Check all 2010 quarters
mean(window(ht, start = c(2010,1), end = c(2010,4))) #Check the mean over all of 2010

# Plot for visual confirmation:
mean(window(ht, start = c(2010,1), end = c(2010,4)))
plot(ht, main = "Log Real HPI", ylab = "Index(logged)")


#----#----#----#----#----#----#

# 4. Variable 4 - Swedish GDP (quarterly, real):

# Extract manually the data I need from the excel file - QoQ growth from 1993Q2 to 2025Q1)
raw <- read_excel("raw_data/GDP_1993_qoq_sa.xlsx",
                  sheet = "Data", range = "K7:K135",
                  col_names = FALSE)[[1]]

# Transform the raw numeric, drop leading NA if present:
gdp_qoq <- as.numeric(gsub("%", "", gsub(",", ".", trimws(as.character(raw)))))
gdp_qoq <- gdp_qoq[!is.na(gdp_qoq)]

# Create the relevant ts: 1993Q2-2025Q1
gdpt <- ts(gdp_qoq, start = c(1993, 2), frequency = 4)

# Sanity check:
head(gdpt, 8)  #Compare with values from Excel
length(gdpt)   #Should be 128


#----#----#----#----#----#----#

# 5. Visualisation:

# I will plot the 5 variables and export them as a LaTeX friendly format.

#----#----#----#
# a. Interest (policy) rates is already available as a df, but needs some processing for
#    optimal visualisation on LaTeX:

# Parse "1994 Quarter 3"
rt_df$date <- as.Date(as.yearqtr(sub(" Quarter ", " Q", rt_df$Period), format = "%Y Q%q"))

# Clean df for plotting:
rt_plot <- rt_df[, c("date", "Average")]

# Plot:
p_rt <- ggplot(rt_plot, aes(date, Average)) +
  geom_line(linewidth = 0.5, colour = "black") +
  labs(x = NULL, y = "Policy rate (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # rotate labels
    plot.margin = margin(5, 10, 5, 10)
  )

# Save as LaTeX friendly PDF:
ggsave("output/1_plot_policy_rate.pdf", p_rt, width = 6.5, height = 3.2, device = "pdf")

#----#----#----#
# b. Mortgage rate, which needs to be converted back from a ts to a df:

# Convert ts to df:
mt_df <- data.frame(
  date  = as.Date(as.yearqtr(time(mt))),
  value = as.numeric(mt)
)

# Plot:
p_mt <- ggplot(mt_df, aes(date, value)) +
  geom_line(linewidth = 0.5, colour = "black") +
  labs(x = NULL, y = "STIBOR 3M (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 10, 5, 10)
  )

# Save:
ggsave("output/2_plot_mortgage_rate.pdf", p_mt, width = 6.5, height = 3.2, device = "pdf")

#----#----#----#
# c. House prices, also needs to be converted from a ts to a df:

# Convert the ts to a df:
ht_df <- data.frame(
  date  = as.Date(as.yearqtr(time(ht))),
  value = as.numeric(ht)
)

# Plot:
p_ht <- ggplot(ht_df, aes(date, value)) +
  geom_line(linewidth = 0.5, colour = "black") +
  labs(x = NULL, y = "Real house prices (log, 2010 = 100)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 10, 5, 10)
  )

# Save:
ggsave("output/3_plot_house_prices.pdf", p_ht, width = 6.5, height = 3.2, device = "pdf")

#----#----#----#
# d. Quarterly GDP growth in % (already deseasonalised and detrended from the Riksbank):

# gdpt_growth is the ts starting 1993Q2
gdp_growth_df <- data.frame(
  date  = as.Date(as.yearqtr(time(gdpt))),
  value = as.numeric(gdpt)
)

p_gdp_growth <- ggplot(gdp_growth_df, aes(date, value)) +
  geom_line(linewidth = 0.5, colour = "black") +
  labs(x = NULL, y = "Quarterly real GDP growth (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 10, 5, 10)
  )

# Get a tighter y-range:
p_gdp_growth <- p_gdp_growth + coord_cartesian(ylim = c(-5, 5))

# Save
ggsave("output/4_plot_gdp_growth.pdf", p_gdp_growth, width = 6.5, height = 3.2, device = "pdf")


#----#----#----#----#----#----#

# 6. Housekeeping:

# Delete all the redundant variables, keeping only the 4 series I created:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt", "ts")))

# Combine the three main variables in one time series (NB - keeps only rows where all 4 exist)
ts <- na.omit(cbind(rt = rt, mt = mt, ht = ht, gdpt = gdpt))

# By extracting again, I ensure that all variables have the same numbers of observations
# This will be important for example when I run OLS for the Engle-Granger test:
ht <- ts[,"ht"]
mt <- ts[,"mt"]
rt <- ts[,"rt"]
gdpt <- ts[,"gdpt"]


#----#----#----#----#----#----#

# II. Analysis

#----#----#----#----#----#----#

# 1. ADF test for unit roots (stationarity test):

## Intercept only for interest/mortgage rates, and both trend + intercept for house prices.
summary(adf_rt_L <- ur.df(rt, type = "drift", selectlags = "AIC", lags = 12)) # Fail to reject 5%
summary(adf_mt_L <- ur.df(mt, type = "drift", selectlags = "AIC", lags = 12)) # Fail to reject 5%
summary(adf_gdpt_L <- ur.df(gdpt, type = "drift", selectlags = "AIC", lags = 12)) # Reject at 5% - I(0)
summary(adf_ht_L <- ur.df(ht, type = "trend", selectlags = "AIC", lags = 12)) # Fail to reject 5%

## I run the ADF test on the first diff and ensure they are stationary in differences:
summary(adf_d_rt <- ur.df(diff(rt), type = "drift", selectlags = "AIC", lags = 12)) # Reject at 5% - Δrt is I(0) - rt is I(1)
summary(adf_d_mt <- ur.df(diff(mt), type = "drift", selectlags = "AIC", lags = 12)) # Reject at 5% - Δmt is I(0) - mt is I(1)
summary(adf_d_ht <- ur.df(diff(ht), type = "drift", selectlags = "AIC", lags = 12)) # Reject at 5% - Δht is I(0) - ht is I(1)

## A bit of housekeeping again:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt", "ts")))   
   
#----#----#----#

# 2. Plot ACFs (seasonality):

par(mfrow=c(1,3))

pdf("output/5_acf_plots.pdf", width = 7, height = 3.5)
par(mfrow = c(1, 3)) # 1 row, 3 plots

acf(diff(mt), lag.max=16, main = "ACF delta_mt") #I choose 16 lags = 4 years of past information
acf(diff(ht), lag.max=16, main = "ACF delta_ht")
acf(diff(rt), lag.max=16, main = "ACF delta_rt")
# Skip GDP because data is already deseasonalised

par(mfrow = c(1, 1)) #Reset layout
dev.off()            #Close PDF saving process

#----#----#----#

# 3. Engle-Granger co-integration test between mt and ht:

## a. Step 1 - OLS:
### Start with the long-run relationship using OLS:
eg_lr <- lm(ht ~ mt)
eg_res  <- resid(eg_lr) #Save residuals

## b. Step 2 - ADF on residuals (eg_res):
summary(eg_adf <- ur.df(eg_res, type = "none", selectlags = "AIC", lags = 12))
### Residuals are stationary, so we can conclude that ht and mt are cointegrated.

### ADF on residuals (no constant) rejects a unit root, ht and mt are cointegrated.
### I will proceed with a VAR in levels thus”

# More housekeeping:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt", "ts")))   

#----#----#----#

# 4. VAR-in-levels analysis:

## a. Stack the variables (in levels):
tmp <- ts.intersect(
  rt   = ts[,"rt"],   # policy rate (level, %)
  mt   = ts[,"mt"],   # STIBOR 3M (level, %)
  gdpt = ts[,"gdpt"], # GDP growth I(0))
  ht   = ts[,"ht"]   # log real HPI (level, log)
)

Z <- tmp[, c("rt","mt","gdpt","ht")]# endogenous block (order = Cholesky order)

## b. Lag selection using a Ljung–Box guard:
lag.max  <- 8    # Upper limit at 2 years
lb_lags  <- 14   # Test up to 3.5 years
alpha    <- 0.05 # 5% significance level

### Compute Ljung–Box p-values from 1 to lag.max = 8:
lb_p <- sapply(1:lag.max, function(pp) {
  vm <- VAR(Z, p = pp, type = "const")
  serial.test(vm, lags.pt = lb_lags, type = "PT.adjusted")$serial$p.value
})

### Choose the first p that passes at 5%
### if none pass, take the max p-value
first_ok <- which(lb_p > alpha)[1]
p <- if (is.na(first_ok)) which.max(lb_p) else first_ok

var_lev <- VAR(Z, p = p, type = "const")
cat(sprintf("Chosen p = %d | Ljung–Box(h=%d) p = %.3f\n",
            p, lb_lags, lb_p[p]))


### The test fail to reject the Ljung–Box null - no serial correlation up to lag 14.
### NB - using lb_lags = 12 (3 years), the test rejected H0.
### NB2 - Caveat: p = 5 for quarterly data is far from ideal

## c. Summary diagnostics:
### i Residual serial correlation at the chosen lag horizon (already done in selection, but re-state)
lb_final <- serial.test(var_lev, lags.pt = 14, type = "PT.adjusted")$serial$p.value
cat(sprintf("Final Ljung–Box (h=14) p = %.3f\n", lb_final))
### No residual autocorrelation

### .ii. Jarque–Bera (Normality):
jb <- normality.test(var_lev, multivariate.only = TRUE)
print(jb$jb.mul)
### Residuals non-normal, which is expected from the data.

### iii. Conditional heteroskedasticity (ARCH LM, multivariate)
arch <- arch.test(var_lev, lags.multi = 12, multivariate.only = TRUE)
print(arch$arch.mul)
### No evidence of ARCH effects.

## d. Lag selection note:
# I tested p = 1..8 using the multivariate Ljung–Box test on 14 residual lags
# (3.5 years). The rule is to choose the smallest p that passes at 5%.
# Result - no parsimonious model (p ≤ 4) passed; p = 5 is the smallest
# that clears the guardrail with p = 0.078).

# More housekeeping:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt", "p", # Add var_lev and p
                                 "ts", "var_lev", "Z")))

#----#----#----#

# 5. IRFs (expansionary policy rate cut of -25bp):

set.seed(123) # Ensure IRF reproducibility
irf_r <- irf(var_lev, impulse = "rt", response = c("mt","gdpt","ht"),
             n.ahead = 24, boot = TRUE, runs = 2000, ci = 0.95, ortho = TRUE) #2000 runs instead of the default 10

# scale to a -25bp policy move
sd_u_rt <- sd(residuals(var_lev)[,"rt"], na.rm = TRUE)
scale_r <- (-25/100) / sd_u_rt
scale_irf <- function(o, s){ o$irf<-lapply(o$irf,`*`,s); o$Lower<-lapply(o$Lower,`*`,s); o$Upper<-lapply(o$Upper,`*`,s); o }
irf_r_25 <- scale_irf(irf_r, scale_r)

# tidy for ggplot (2 panels: mt and ht; add gdpt later if you want 3 panels)
H <- nrow(irf_r_25$irf$rt); h <- 0:(H-1)
df_pol <- rbind(
  data.frame(h=h, response="mt",
             irf=irf_r_25$irf$rt[,"mt"],   lo=irf_r_25$Lower$rt[,"mt"],   hi=irf_r_25$Upper$rt[,"mt"]),
  data.frame(h=h, response="ht",
             irf=irf_r_25$irf$rt[,"ht"],   lo=irf_r_25$Lower$rt[,"ht"],   hi=irf_r_25$Upper$rt[,"ht"])
)

h_breaks <- seq(0, max(h), by = 4)  # show yearly ticks
p_pol <- ggplot(df_pol, aes(h, irf)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.5, colour = "black") +
  facet_wrap(~ response, ncol = 2, scales = "free_y",
             labeller = as_labeller(c(mt="Mortgage rate (pp)", ht="Real HPI (log level)"))) +
  scale_x_continuous(breaks = h_breaks, limits = c(0, max(h_breaks)),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Horizon (quarters)", y = "Response") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("output/6_irf_policy_cut.pdf", p_pol, width = 6.5, height = 3.2, device = "pdf")

# Save the IRF table to cite in the paper
key <- c(4,8,12,20)
tab_irf <- data.frame(
  horizon_q = key,
  mt_pp  = round(irf_r_25$irf$rt[key, "mt"],  3),
  ht_log = round(irf_r_25$irf$rt[key, "ht"],  3)
)
write.csv(tab_irf, "output/10_irf_key_horizons.csv", row.names = FALSE)

# Housekeeping:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt",
                      "p", "ts", "var_lev", "Z")))

#----#----#----#

# 6. FEVD for ht and mt:

vd <- fevd(var_lev, n.ahead = 20)
Hsel <- c(4, 8, 12, 20)                                   # 1y, 2y, 3y, 5y
tab_fevd_ht <- round(vd$ht[Hsel, c("rt","mt","gdpt","ht")], 3)
print(tab_fevd_ht)
write.csv(cbind(h = Hsel, tab_fevd_ht), "output/11_fevd_ht_table.csv", row.names = FALSE)

## Stacked shares:
fevd_df <- data.frame(h = factor(Hsel,
                                 levels = Hsel,
                                 labels = c("4q (1y)", "8q (2y)", "12q (3y)", "20q (5y)")),
                      tab_fevd_ht)

long <- transform(stack(fevd_df[ , -1]),
                  h = rep(fevd_df$h, times = ncol(fevd_df) - 1))
names(long) <- c("share", "shock", "h")

## Legend labels:
long$shock <- factor(long$shock,
                     levels = c("rt","mt","gdpt","ht"),
                     labels = c("Policy rate (rt)",
                                "Mortgage (mt)",
                                "GDP growth (gdpt)",
                                "Own: house prices (ht)"))


p_fevd <- ggplot(long, aes(x = h, y = share, fill = shock)) +
## Add white outlines separate the segments:
  geom_col(position = "fill", width = 0.72, colour = "white", linewidth = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_grey(start = 0.15, end = 0.85, name = NULL) +
  labs(x = NULL, y = "Variance share in real HPI (ht)",
       title = "FEVD of real HPI (ht): contributions of shocks") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("output/7_fevd_ht_shares.pdf", p_fevd, width = 6.5, height = 3.6, device = "pdf")

# Housekeeping:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt",
                          "p", "ts", "var_lev", "Z")))

#----#----#----#----#----#----#

# III. Sensitivity analysis:

#----#----#----#----#----#----#

# 1. Cholesky ordering robustness change: rt -> gdpt -> mt -> ht (i.e., swapping mt and gdpt)

# Rebuild VAR with the alternative Cholesky order
Z_ord  <- na.omit(cbind(rt = ts[,"rt"], gdpt = ts[,"gdpt"], mt = ts[,"mt"], ht = ts[,"ht"]))
var_ord <- VAR(Z_ord, p = p, type = "const")

# IRF to a policy-rate shock, then scale to a -25bp move
irf_ord <- irf(var_ord, impulse = "rt", response = c("mt","ht"),
               n.ahead = 24, boot = TRUE, runs = 2000, ci = 0.95, ortho = TRUE)
scale25 <- (-25/100) / sd(residuals(var_ord)[,"rt"], na.rm = TRUE)
irf_ord$irf   <- lapply(irf_ord$irf,   `*`, scale25)
irf_ord$Lower <- lapply(irf_ord$Lower, `*`, scale25)
irf_ord$Upper <- lapply(irf_ord$Upper, `*`, scale25)

# Plot with 2 panels (mt and ht)
H <- nrow(irf_ord$irf$rt); h <- 0:(H-1)
df_ord <- rbind(
  data.frame(h=h, response="mt",
             irf=irf_ord$irf$rt[,"mt"], lo=irf_ord$Lower$rt[,"mt"], hi=irf_ord$Upper$rt[,"mt"]),
  data.frame(h=h, response="ht",
             irf=irf_ord$irf$rt[,"ht"], lo=irf_ord$Lower$rt[,"ht"], hi=irf_ord$Upper$rt[,"ht"])
)

p_ord <- ggplot(df_ord, aes(h, irf)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.20) +
  geom_line(linewidth = 0.5, colour = "black") +
  facet_wrap(~ response, ncol = 2, scales = "free_y",
             labeller = as_labeller(c(mt="Mortgage rate (pp)", ht="Real HPI (log level)"))) +
  scale_x_continuous(breaks = seq(0, max(h), by = 4),
                     limits = c(0, max(seq(0, max(h), by = 4))),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Horizon (quarters)", y = "Response",
       title = "IRFs (robustness): ordering rt → gdpt → mt → ht") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("output/8_robust_irf_order_swap.pdf", p_ord, width = 6.5, height = 3.2, device = "pdf")


#----#----#----#

# 2. Shock-size sensitivity for HPI IRFs:

### Choose the shock sizes (bp)
s_cs <- c(-25, -50, -100)

## Base IRF for a 1-s.d. policy change
base_ir <- irf(var_lev, impulse = "rt", response = "ht",
               n.ahead = 24, boot = TRUE, runs = 1000, ci = 0.95, ortho = TRUE)

## Scale factor bp -> model units
sd_rt <- sd(residuals(var_lev)[,"rt"], na.rm = TRUE)

## Tidy IRFs for each shock size
df_sz <- do.call(rbind, lapply(s_cs, function(bp){
  s <- (bp/100) / sd_rt
  data.frame(h = 0:(nrow(base_ir$irf$rt)-1),
             irf = as.numeric(base_ir$irf$rt[,"ht"]) * s,
             shock = bp)
}))

## Order legend by |shock| ascending: -25 → -50 → -100
lev_abs <- unique(s_cs)[order(abs(unique(s_cs)))]
df_sz$shock <- factor(df_sz$shock, levels = lev_abs)

p_sens <- ggplot(df_sz, aes(h, irf, linetype = shock, group = shock)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_line(linewidth = 0.8, color = "black") +
  scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
  scale_linetype_manual(
    name   = "Shock size",
    labels = paste0(lev_abs, " bp"),
    values = c("solid", "longdash", "dotted")[seq_along(lev_abs)]
  ) +
  labs(x = "Horizon (quarters)", y = "Response (log level)",
       title = "Sensitivity: real HPI IRFs to different policy shock sizes") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank())

ggsave("output/9_sens_irf_ht_shocksize.pdf", p_sens, width = 6.5, height = 3.2, device = "pdf")


# Final Housekeeping:
rm(list = setdiff(ls(), c("rt", "mt", "ht", "gdpt", "ts","var_lev")))


#----#----#----#----#----#----#----#----#----#----#----#----#
#----#----#----#----#-----## End ##-----#----#----#----#----#
#----#----#----#----#----#----#----#----#----#----#----#----#