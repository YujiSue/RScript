# RScript (English | Japanese)
R scripts used for research, teaching, and other administrative tasks.  

これまで研究や教育、その他雑務で使用したRのスクリプト置場です。

## Statistical test | 統計検定関連
### Content | 実行可能な検定
- Fisher exact test   
- McNemar test  
- Normality test  
 &nbsp;&nbsp;&nbsp;&nbsp;(KS, SH, AD)  
- $\chi^2$ test  
- F test  
- Bartlett test  
- t test (Student, Welch, Paired)  
- U test  
- ANOVA  
- Kluscal-Wallis test  
- Friedman test  
- MANOVA  
- Multiple comparison test  
 &nbsp;&nbsp;&nbsp;&nbsp;(Dunnett, TukeyHSD, Scheffe, Steel, Steel-DWass)  
- Survival rate test  
  &nbsp;&nbsp;&nbsp;&nbsp;(Log-rank, Generalized Wilcoxon)  
- Likelihood ratio test  
 
### Usage | 使用方法
1. Launch your R app.   

2. Copy the [loadLibs.R]() and [ysrstat.R]() into the workspace of R app.

3. Load functions
```R
source("/content/loadLibs.R")
source("/content/ysrstat.R")
```


