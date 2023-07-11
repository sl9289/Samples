## 1-cross sectional data
## Topic: The Academic Performance of Rural Left-behind Children and Its Influencing Factors—Evidence from Henan Province of China

# Merge data sets and keep the data of Henan province in 2017
install.packages("haven")
library(haven)
School_2017 <- read_dta("D:/surveydata/append/raw/school2017.dta")
View(School_2017)
School_2017$id <- as.numeric(School_2017$id)
library("dplyr")
School_2017 <- School_2017 %>% rename("school1" = "id")
School_2017 <- subset(School_2017, select = c(school1,s1_c,s4_c,s5,s7_c,s9_c,s13_c,s11_c,s19,s20,s23_c))
save(School_2017, file = "School_2017.Rda")  

Df_fi <- read_dta("D:/surveydata/append/dtanew/FINAL.dta")
Df_fi <- Df_fi[Df_fi$year == 2017,]
Df_fi$id <- as.character(Df_fi$id)
Df_fi$school1 <- substr(Df_fi$id, 1, 4) 
Df_fi$school1 <- as.numeric(Df_fi$school1)
save(Df_fi, file = "Df_fi.Rda") 

joint2017 <- merge(Df_fi, School_2017, by = 'school1', all.x = TRUE)
joint2017 <- joint2017[joint2017$pro == 4,] # keep the data of Henan province 
joint2017 <- joint2017[joint2017$g7_c1 == 0,] # keep the data of left-behind chirldren-2541
save(joint2017, file = "joint2017.Rda") 

load("joint2017.Rda") 
joint2017$age <- 2017-joint2017$g2_c # add a new variable which calculates age
joint2017$g11_c1 <- case_when(joint2017$g11_c == 1 ~ 0, 
                              joint2017$g11_c == 2 ~ 6, 
                              joint2017$g11_c == 3 ~ 9,
                              joint2017$g11_c == 4 ~ 12,
                              joint2017$g11_c == 5 | joint2017$g11_c == 6 ~ 16
                              ) # add a new variable which calculates the father's years of education
joint2017$g13_c1 <- case_when(joint2017$g13_c == 1 ~ 0, 
                              joint2017$g13_c == 2 ~ 6, 
                              joint2017$g13_c == 3 ~ 9,
                              joint2017$g13_c == 4 ~ 12,
                              joint2017$g13_c == 5 | joint2017$g13_c == 6 ~ 16
                              ) # add a new variable which calculates the mother's years of education
joint2017$outtype <- case_when(joint2017$g7_c == "1" ~ 1, 
                               joint2017$g7_c == "2" ~ 2, 
                               ) # add a new variable which calculates the type of parents working out of home
joint2017 <- subset(joint2017, complete.cases(joint2017$mathscore,joint2017$Psdv1,joint2017$Pmathscore))
final2017 <- subset(joint2017, select = c(id,outtype,pro,mathscore,sde1,sdn1,sdo1,sdg1,sdv1,Psdv1,Psdo1,Psdn1,
                                          g1_c,age,g3_c,g4_c,g26_c,grade,g30_c,g7_c2,g11_c1,g13_c1,g14_c,
                                          g15_c,g18_c,g34_c,g29_c,Pmathscore,g24new,Pg24new,g24anew,g25new,
                                          Pg24anew,Pg25new,s1_c.x,s4_c.x,s5.x,s7_c.x,s9_c.x,s13_c.x,
                                          s11_c.x,s19.x,s20.x,s23_c.x))
final2017 <- final2017 %>% rename("gender" = "g1_c","boarder" = "g3_c","preschool" = "g4_c","leader" = "g26_c",
                                  "repeater" = "g30_c","families" = "g7_c2","fa_edu" = "g11_c1","mo_edu" = "g13_c1",
                                  "house" = "g14_c","tv" = "g15_c","computer" = "g18_c","internet" = "g34_c",
                                  "money" = "g29_c","school_establish" = "s1_c.x","class_scale" = "s4_c.x",
                                  "teacher_amount" = "s5.x","teacher_excellent" = "s7_c.x","teacher_super" = "s9_c.x",
                                  "teacher_1" = "s11_c.x","teacher_2" = "s13_c.x","library" = "s19.x",
                                  "computer_sch" = "s20.x","dininghall" = "s23_c.x")
save(final2017, file = "final2017.Rda") 

# Summary descriptive statistics and plot distributions
install.packages("stargazer")
library(stargazer)
stargazer(final2017[c("boarder","preschool","leader","grade","repeater")], type = "text", title = "Summary Statistics-induvidual personalities1", out = "table1.txt")
stargazer(final2017[c("gender","sde1","sdn1","sdo1","sdg1","sdv1")], type = "text", title = "Summary Statistics-individual personalities2", out = "table1.txt") 
stargazer(final2017[c("families","fa_edu","mo_edu")], type = "text", title= "Summary Statistics-household characteristics1", out = "table1.txt") 
stargazer(final2017[c("house","tv","computer","internet","money")], type = "text", title = "Summary Statistics-household characteristics2", out = "table1.txt") 
stargazer(final2017[c("library","computer_sch","dininghall")],type="text",title="Summary Statistics-school characteristics1", out="table1.txt")
stargazer(final2017[c("school_establish","class_scale","teacher_amount")], type = "text", title = "Summary Statistics-school characteristics2", out = "table1.txt")

stargazer(final2017[c("g24new","g24anew","g25new")], type = "text", title = "final exam results", out = "table1.txt")
kd_math <- density(na.omit(final2017$g24new))
kd_chi <- density(na.omit(final2017$g24anew))
kd_eng <- density(na.omit(final2017$g25new))
plot(kd_chi,col = "blue", xlab ="final scores", main = "Distribution of final exam results for three Subjects", lwd = 2)
lines(kd_math,col = "red")
lines(kd_eng,col = "yellow")

final2017_3 <- final2017[final2017$grade == 3,]
kd_3 <- density(na.omit(final2017_3$mathscore))
final2017_4 <- final2017[final2017$grade == 4,]
kd_4 <- density(na.omit(final2017_4$mathscore))
plot(kd_4, col = "blue", xlab = "final scores", main = "Distribution of the TIMSS results of Left-behind Children by Grade", lwd = 2)
lines(kd_3, col = "red")
stargazer(final2017_3[c("mathscore")], type = "text", title = "TIMSS results of grade3", out = "table1.txt")
stargazer(final2017_4[c("mathscore")], type = "text", title = "TIMSS results of grade4", out = "table1.txt")

# Plot Correlograms
corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  install.packages("corrplot")
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag)
}

corrplot2(
  data = final2017,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# Diagnostic tests for 2SLS (because mathscore and Pmathscore have simultaneous causal effect which will cause endogeneity problem, I choose Psdv1 as an instrumental variable of Pmathscore and follow 2sls estimation) )
install.packages("remotes")
library("remots")
remotes::install_github("https://github.com/john-d-fox/ivreg/")
library("ivreg")
model_2sls <- ivreg(mathscore ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                    families + fa_edu + mo_edu + house + tv + computer + internet + money +
                    Pmathscore + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                    families + fa_edu + mo_edu + house + tv + computer + internet + money +
                    Psdv1 + library + teacher_2, 
                    data=final2017)
summary(model_2sls, Pmathscore = Psdv1, diagnostics = TRUE) # the results show that the instrument variable is not weak, and endogeneity does exist. But because the amount of instruments is equal to the number of endogenous regressors, I cannot test the instrument exogeneity using overidentifying restrictions

# Test for multicollinearity
install.packages("car")
library(car)
vif(model_2sls) # Since each of the VIF values for the predictor variables in the model are close to 1, multicollinearity is not a problem in this model

# Specification test
resettest(mathscore ~ sde1 + sdn1 + sdo1 +sdg1+sdv1+gender+age+boarder+preschool+leader+grade+repeater+
            families+fa_edu+mo_edu+house+tv+computer+internet+money+Pmathscore+library+teacher_2, 
          power = 2:3, type = c("fitted", "regressor","princomp"), data = final2017) # this model's functional form is correct

# Test for Heteroscedasticity
par(mfrow=c(2,2)) 
plot(model_2sls) # there is no a completely random, equal distribution of points throughout the range of X axis and no flat red line, so heteroskedasticity is detected
install.packages("lmtest")
library(lmtest)
bptest(model_2sls) # reject null hypothesis

# Resolve heteroskedasticity by 2sls with robust standard errors
model_2slsmm <- update(model_2sls, method = "MM")
summary(model_2slsmm) # adopted result
compareCoefs(model_2sls, model_2slsmm)

# Robustness check (substitute independent variable)
install.packages("mosaic")
library(mosaic)
final2017$stdmath <- scale(final2017$g24new)
final2017$stdPmath <- scale(final2017$Pg24new)
final2017$stdmath <- sd(final2017$g24new, na.rm = TRUE)
final2017$stdPmath <- sd(final2017$Pg24new, na.rm = TRUE)
model_2sls_r <- ivreg(stdmath ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                      families + fa_edu + mo_edu + house + tv + computer + internet + money +
                      stdPmath + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                      families + fa_edu + mo_edu + house + tv + computer + internet + money +
                      Psdv1 + library + teacher_2, 
                      data=final2017)
model_2sls_rmm <- update(model_2sls_r, method = "MM")
summary(model_2sls_rmm)

# Heterogeneity analyses-explore the different factors affecting the academic performance of left-behind children across subjects and genders
final2017 <- final2017 %>% rename("chinese" = "g24anew", "Pchinese" = "Pg24anew",
                                  "english" = "g25new", "Penglish" = "Pg25new")
final2017_c <- subset(final2017, complete.cases(final2017$chinese,final2017$Pchinese))
save(final2017_c, file = "final2017_C.Rda") 
model_chi <- ivreg(chinese ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                   families + fa_edu + mo_edu + house + tv + computer + internet + money +
                   Pchinese + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                   families + fa_edu + mo_edu + house + tv + computer + internet + money +
                   Psdv1 + library + teacher_2, 
                   data=final2017_c)
model_chi_rmm <- update(model_chi, method = "MM")
summary(model_chi_rmm) # robust 2sls result of using Chinese final score as the independent variable

final2017_e <- subset(final2017, complete.cases(final2017$english,final2017$Penglish))
save(final2017_e, file = "final2017_e.Rda") 
model_eng <- ivreg(english ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                   families + fa_edu + mo_edu + house + tv + computer + internet + money +
                   Penglish + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                   families + fa_edu + mo_edu + house + tv + computer + internet + money +
                   Psdv1 + library + teacher_2, 
                   data=final2017_e)
model_eng_rmm <- update(model_eng, method = "MM")
summary(model_eng_rmm) # robust 2sls result of using English final score as the independent variable

final2017_1 <- final2017[final2017$gender == 1,]
save(final2017_1, file = "final2017_1.Rda") 
model_1 <- ivreg(mathscore ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                    families + fa_edu + mo_edu + house + tv + computer + internet + money +
                    Pmathscore + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                    families + fa_edu + mo_edu + house + tv + computer + internet + money +
                    Psdv1 + library + teacher_2, 
                    data=final2017_1)
model_1_rmm <- update(model_1, method = "MM")
summary(model_1_rmm) # robust 2sls result only based on boys' data

final2017_2 <- final2017[final2017$gender == 2,]
save(final2017_2, file = "final2017_2.Rda") 
model_2 <- ivreg(mathscore ~ sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                 families + fa_edu + mo_edu + house + tv + computer + internet + money +
                 Pmathscore + library + teacher_2 | sde1 + sdn1 + sdo1 + sdg1 + sdv1 + gender + age + boarder + preschool + leader + grade + repeater +
                 families + fa_edu + mo_edu + house + tv + computer + internet + money +
                 Psdv1 + library + teacher_2, 
                 data=final2017_2)
model_2_rmm <- update(model_2, method = "MM")
summary(model_2_rmm) # robust 2sls result only based on girls' data


## 2-time series data
## Topic: Fit Hamilton’s switching regime model of real home prices from 1953-2021

library(readxl)
realhomeprices <- read_excel("C:/Users/lenovo/Downloads/realhomeprices.xls")
View(realhomeprices)

# Transform the data into a monthly percent change series
library(dplyr)
MPC <- realhomeprices%>%
  dplyr::mutate(Previous=lag(Index),
                change=Index-Previous,
                percentchange=(change/Previous)*100)

# Fit Hamilton’s switching regime model
install.packages("MSwM")
library(MSwM)
npercentchange<-length(MPC$percentchange)
pc<-MPC$percentchange
y0<-pc[2:npercentchange]
y1<-pc[1:(npercentchange-1)] 
df<-data.frame(y=y0,y1=y1)
lrm<-lm(y~y1,df) 
k<-2  
mv<-3 
rsm<-msmFit(lrm, k=2, p=0, sw=rep(TRUE, mv),
            control=list(parallel=FALSE))
x11(); plotProb(rsm,which=1) 
x11(); plotProb(rsm,which=2) 
x11(); plotProb(rsm,which=3) 

hamilton.filter<-function(param,x,y){
  nobs = length(y) 
  const0 <- param[1]; const1 <- param[2] 
  beta0  <- param[3]; beta1  <- param[4]
  sigma0 <- param[5]; sigma1 <- param[6]
  p00 <- 1/(1+exp(-param[7])) 
  p11 <- 1/(1+exp(-param[8]))
  p01 <- 1-p00; p10 <- 1-p11
  ps_i0 <- us_j0 <- ps_i1 <- us_j1 <- NULL
  sspr1 <- (1-p00)/(2-p11-p00)
  sspr0 <- 1-sspr1
  us_j0 <- sspr0
  us_j1 <- sspr1
  ps <- fp <- sp <- matrix(0,nrow = length(y),ncol = 2)
  loglike <- 0
  for(t in 1:nobs){
    ps_i0 <- us_j0
    ps_i1 <- us_j1
    er0 <- y[t] - const0 - beta0*x[t]
    er1 <- y[t] - const1 - beta1*x[t]
    eta_j0 <- (1/(sqrt(2*pi*sigma0^2)))*exp(-(er0^2)/(2*(sigma0^2)))
    eta_j1 <- (1/(sqrt(2*pi*sigma1^2)))*exp(-(er1^2)/(2*(sigma1^2)))
    f_yt <- ps_i0*p00*eta_j0 + 
      ps_i0*p01*eta_j1 + 
      ps_i1*p10*eta_j0 + 
      ps_i1*p11*eta_j1   
    if( f_yt < 0 || is.na(f_yt)) { 
      loglike <- -100000000; break 
    }
    us_j0 <- (ps_i0*p00*eta_j0+ps_i1*p10*eta_j0)/f_yt
    us_j1 <- (ps_i0*p01*eta_j1+ps_i1*p11*eta_j1)/f_yt
    ps[t,] <- c(ps_i0, ps_i0) # predicted states
    fp[t,] <- c(us_j0, us_j1) # filtered probability
    loglike <- loglike + log(f_yt)
  }
  return(list(loglike = -loglike, fp = fp, ps = ps ))
}
rs.est <- function(param,x,y){
  return(hamilton.filter(param,x,y)$loglike)
}

init_guess <- c(lrm$coefficients[1], lrm$coefficients[1], 
                lrm$coefficients[2], lrm$coefficients[2], 
                summary(lrm)$sigma, summary(lrm)$sigma, 3, 2.9)
mle <- optim(init_guess, rs.est,
             control = list(maxit=50000, trace=2),
             method=c("BFGS"), x = y1, y = y0)
mle <- optim(mle$par, rs.est,
             control = list(maxit=50000, trace=2),
             method=c("Nelder-Mead"), x = y1, y = y0)

hf <- hamilton.filter(mle$par,x = y1, y = y0)

x11(); par(mfrow=c(2,1))
matplot(cbind(rsm@Fit@filtProb[,1],hf$fp[,2]), 
        main = "Regime 1", type="l", cex.main=1)
matplot(cbind(rsm@Fit@filtProb[,2],hf$fp[,1]), 
        main = "Regime 2", type="l", cex.main=1)
