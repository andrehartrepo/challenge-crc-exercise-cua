# Parametric Survival Model Selection Analysis

Source: Guyot-reconstructed pseudo-IPD from CHALLENGE trial KM curves
(Courneya et al. 2025, NEJM)

Method: NICE DSU TSD 14 (Latimer 2013) selection framework
(supplementary — DMP/NOMA has no equivalent parametric fitting methodology)

Pipeline: WebPlotDigitizer manual digitisation → IPDfromKM → flexsurv

Date: 2026-04-04 (Session 9)

Note: IPD time is in YEARS (converted from months during reconstruction in 01-digitize-km.R).
Flexsurv models are parameterised in years, matching the PSM time grid (cycle_length = 1/12 years).
AIC/BIC absolute values differ from month-based parameterisation but delta-AIC/BIC and survival
predictions are identical (time unit transformation is monotonic and applies equally to all distributions).

---

## 1. Statistical Fit (Control Arm — Model Selection Basis)

### DFS Control

| Rank | Distribution | nPar | AIC | BIC | dAIC | dBIC | Hazard Family |
|------|-------------|------|------|------|------|------|---------------|
| 1 | Gen. Gamma | 3 | 980.4 | 992.7 | 0.0 | 0.0 | AFT |
| 2 | Log-normal | 2 | 986.6 | 994.8 | 6.3 | 2.2 | AFT |
| 3 | Gompertz | 2 | 993.5 | 1001.7 | 13.2 | 9.1 | PH |
| 4 | Log-logistic | 2 | 996.0 | 1004.2 | 15.6 | 11.5 | AFT |
| 5 | Weibull | 2 | 1000.0 | 1008.2 | 19.7 | 15.6 | PH/AFT |
| 6 | Exponential | 1 | 1022.7 | 1026.8 | 42.3 | 34.1 | PH/AFT |

### OS Control

| Rank | Distribution | nPar | AIC | BIC | dAIC | dBIC | Hazard Family |
|------|-------------|------|------|------|------|------|---------------|
| 1 | Gen. Gamma | 3 | 603.6 | 615.9 | 0.0 | 3.9 | AFT |
| 2 | Log-normal | 2 | 603.8 | 612.0 | 0.2 | 0.0 | AFT |
| 3 | Log-logistic | 2 | 607.5 | 615.7 | 3.9 | 3.7 | AFT |
| 4 | Weibull | 2 | 608.4 | 616.6 | 4.8 | 4.6 | PH/AFT |
| 5 | Gompertz | 2 | 614.8 | 623.0 | 11.2 | 11.0 | PH |
| 6 | Exponential | 1 | 620.2 | 624.3 | 16.6 | 12.3 | PH/AFT |


## 2. Predicted Survival at Key Timepoints (Control Arm)

### DFS Control

| Distribution | S(5yr) | S(10yr) | S(20yr) | S(40yr) |
|-------------|--------|---------|---------|---------|
| Gen. Gamma | 0.7237 | 0.6466 | 0.5726 | 0.5041 |
| Log-normal | 0.7309 | 0.6248 | 0.5083 | 0.3910 |
| Gompertz | 0.7203 | 0.6425 | 0.6090 | 0.6046 |
| Log-logistic | 0.7341 | 0.6186 | 0.4878 | 0.3587 |
| Weibull | 0.7417 | 0.6170 | 0.4581 | 0.2832 |
| Exponential | 0.7586 | 0.5755 | 0.3311 | 0.1097 |

Published 5-year DFS (control): 73.9% (Courneya et al. 2025)
All distributions predict 5-year DFS within 72.0-75.9% (reasonable).

### OS Control

| Distribution | S(5yr) | S(10yr) | S(20yr) | S(40yr) |
|-------------|--------|---------|---------|---------|
| Gen. Gamma | 0.8951 | 0.7802 | 0.6493 | 0.5242 |
| Log-normal | 0.9039 | 0.7660 | 0.5585 | 0.3331 |
| Log-logistic | 0.9085 | 0.7589 | 0.4995 | 0.2404 |
| Weibull | 0.9104 | 0.7566 | 0.4368 | 0.0854 |
| Gompertz | 0.9091 | 0.7552 | 0.2609 | 0.0000 |
| Exponential | 0.8897 | 0.7915 | 0.6265 | 0.3925 |

Published 8-year OS (control): 83.2% (Courneya et al. 2025)
All distributions predict 8-year OS (96 months) within 81.9-82.9%
(exact parametric evaluation). Log-normal: 81.9%. Published: 83.2%.
Differences of 0.3-1.3pp reflect parametric smoothing of the KM curve.


## 3. Visual Fit Assessment

### DFS Control (dfs_ctrl_survival_overlay.pdf)
- All distributions fit KM well within the observed 120-month period
- Exponential overestimates survival 0-30 months, then underestimates
- Gen. gamma and log-normal track the KM most closely
- The KM shows a clear pattern: rapid early decline (0-24 months) then gradual flattening

### OS Control (os_ctrl_survival_overlay.pdf)
- All distributions fit well (OS curve is smoother with fewer events)
- Less discrimination between distributions in the observed period
- All curves remain within the KM confidence band


## 4. Hazard Function Assessment

### DFS Control (dfs_ctrl_hazard.pdf)
- Most distributions show declining hazard after initial period (clinically correct: most DFS events occur in first 3-5 years post-adjuvant chemotherapy)
- Exponential: constant hazard (clinically implausible)
- Gen. gamma: distinctive hump (low initially, peaks ~5 months, then declines). Could reflect: near-zero recurrence risk immediately post-randomisation, rising as patients enter surveillance, then declining as survivors enter lower-risk follow-up
- Log-normal: brief initial rise then monotonic decline (mathematically, lognormal hazard always rises from zero to a peak then decreases)
- Log-logistic, Weibull, Gompertz: monotonically decreasing from high initial hazard (also clinically plausible)

### OS Control (os_ctrl_hazard.pdf)
- Gompertz: monotonically increasing hazard (most clinically plausible for mortality — risk increases with age)
- Weibull: monotonically increasing (also plausible)
- Gen. gamma: hump then decline (declining mortality hazard is clinically implausible beyond observed data)
- Log-normal: increases then plateaus/slight decline (partial plausibility)
- Exponential: constant (implausible for mortality)


## 5. Long-Term Extrapolation Assessment

### DFS Control (dfs_ctrl_extrapolation.pdf)
- Gompertz: plateaus at ~60% at 40 years (age 101). Clinically implausible without gen pop cap.
- Gen. gamma: plateaus at ~50%. Same issue, less extreme.
- Log-normal: reaches ~39%. More conservative, still implausible at age 101.
- Log-logistic: reaches ~36%.
- Weibull: reaches ~28%. Continuous decline, reasonable.
- Exponential: reaches ~11%. Most aggressive decline.

Note: Model structure includes general population mortality cap (assumption A4). This corrects all implausible long-term survival predictions.

### OS Control (os_ctrl_extrapolation.pdf)
- Gompertz: reaches 0% by ~350 months (age ~90). Possibly too aggressive.
- Weibull: reaches ~9% at 40 years. Closest to realistic at age 101.
- Log-logistic: 24% at 40 years. Between Weibull and log-normal.
- Log-normal: 33% at 40 years. Implausible, corrected by gen pop cap.
- Gen. gamma: 52% at 40 years. Most implausible, corrected by gen pop cap.
- Exponential: 39% at 40 years. Implausible (constant hazard overestimates long-term survival).


## 6. TSD 14 Selection Recommendation

### Base Case: Log-normal (both DFS and OS)

**DFS rationale:**
1. Best BIC among 2-parameter distributions (dBIC = 2.2 from gengamma; per Kass and Raftery 1995, dBIC 2-6 = "positive" evidence, i.e. marginal BIC preference for gengamma offset by parsimony)
2. AIC delta of 6.3 to gengamma indicates considerably less support (Burnham and Anderson 2002: dAIC 4-7), offset by parsimony (2 vs 3 parameters)
3. Clinically plausible decreasing hazard for DFS (post-adjuvant cancer recurrence risk declines over time)
4. More conservative long-term extrapolation than gengamma (39% vs 50% at 40 years); both corrected by gen pop cap
5. Well-established distribution in cancer HTA submissions

**OS rationale:**
1. Best BIC (dBIC = 0.0). Near-best AIC (dAIC = 0.2 from gengamma, trivial)
2. More parsimonious than gengamma (2 vs 3 parameters)
3. Increasing hazard in observed period matches clinical expectation
4. Long-term hazard plateau/decline is less plausible for mortality, but corrected by gen pop cap
5. Consistent with DFS distribution choice (same parametric family for both endpoints simplifies interpretation)

### Sensitivity Analyses

| Analysis | DFS | OS | Rationale |
|----------|-----|----| ----------|
| Base case | Log-normal | Log-normal | Best BIC, parsimonious, clinically plausible |
| SA-1 | Gen. gamma | Gen. gamma | Best AIC, 3-parameter flexibility |
| SA-2 | Weibull | Weibull | PH/AFT family (exact HR application, no approximation) |
| SA-3 | Gompertz | Gompertz | Increasing OS hazard (most clinically plausible for mortality) |

### Treatment Effect Application Note

Log-normal is an AFT (accelerated failure time) distribution. Applying the published HR via S_int(t) = S_ctrl(t)^HR is exact only for PH distributions (exponential, Weibull, Gompertz). For AFT distributions, it is an approximation where the instantaneous HR varies over time.

This is acknowledged as standard practice in cancer HTA (TSD 14, Latimer 2013, Section 4.4) and will be discussed in the thesis Methods section. The Weibull SA (SA-2) tests sensitivity to this approximation.
