Self-esteem across the menstrual cycle
================
2023

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.2

``` r
library("plotrix")
```

``` r
brutos = data$`Dados_brutos (3)`
head(brutos)
```

    ##   participant LINE BLOCO        rt correct condition PRIME_VAL PRIME_TYPE
    ## 1       AA556    0     1 0.4174751       0         2  positive      OTHER
    ## 2       AA556    1     1 0.1681083       1         2  positive      OTHER
    ## 3       AA556    2     1 0.5012267       0         2  negative      OTHER
    ## 4       AA556    3     1 0.4864544       1         2  negative      OTHER
    ## 5       AA556    4     1 0.4847672       0         2  negative      OTHER
    ## 6       AA556    5     1 0.5007748       1         2  positive      OTHER
    ##        PICTURE TARGET        PHASE
    ## 1    VPN07.jpg  linda menstruation
    ## 2    VPN09.jpg  capaz menstruation
    ## 3    VPN07.jpg inútil menstruation
    ## 4    VPN04.jpg   feia menstruation
    ## 5    VPN02.jpg triste menstruation
    ## 6 femcon_2.jpg genial menstruation

\#parse target

``` r
for (i in 1:nrow(brutos)){
  if(substr(brutos$TARGET[i],nchar(brutos$TARGET[i])-2,nchar(brutos$TARGET[i])) == "til"){
    brutos$TARGET[i] = "inútil"  
  }
}
```

\#check

``` r
brutos %>%
  mutate(new = case_when(TARGET == "alegre" ~ "positive",
                         TARGET =="capaz" ~ "positive",
                         TARGET == "confiante" ~ "positive",
                         TARGET == "genial" ~ "positive",
                         TARGET == "linda" ~ "positive"
                                ,TRUE ~ 'negative'
                                )) %>% pull(new) -> new

a = new == brutos$PRIME_VAL 
soma = sum(a, na.rm = TRUE) # best way to count TRUE values
soma == length(a)
```

    ## [1] TRUE

\#check participant’s id

``` r
summary(factor(brutos$participant))
```

    ## AA556 AA696 ab116 AF841 av734 BF401 BJ960 CA306 CC522 CJ335 cj967 CJ967 EG406 
    ##   452   513   446   526   285   491   508   359   101   479   242   261   520 
    ## GA454 GJ487 IJ887 JJ997 Lf380 LF380 MA007 MA565 ma889 MA889 MA929 MA969 mb968 
    ##   458   492   433   385   224   207   511   328   145   175   312   472   444 
    ## ME324 mj243 MJ690 MJ803 MM350 MM533 MM586 MM714 MP177 MR971 NA210 nj182 NJ182 
    ##   471   436   137   412   425   363   459   437   536   331   481   166   146 
    ## nj267 NJ267 OR883 PF580 rf539 RF539 RJ673 RR863 SJ292 SJ302 sj313 SJ313 SJ513 
    ##   209   160   489   505   228   278   441   461   364   451   269   272   444 
    ## SJ874 sm247 sm370 sm416 SM416 SP123 SR421 tp158 TP158 VA606 
    ##   480   411   473   144   169   424   380   226   239   315

``` r
#to lower case all cols
names(brutos) <- tolower(names(brutos))

brutos$participant = tolower(brutos$participant)

#brutos %>% group_by(participant) %>% count()
```

#### Apa theme

``` r
#apa
theme_apa <- function(base_size = 12, base_family = "", box = FALSE) {
  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0), hjust = 0.5)
      , plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8), margin = ggplot2::margin(ggplot2::rel(-7), 0, ggplot2::rel(14), 0), hjust = 0.5)

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.x.top = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(12), 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(12)))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(0.9))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.text.y.right = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(6)))
      , axis.line = ggplot2::element_line()
      # , axis.line.x = ggplot2::element_line()
      # , axis.line.y = ggplot2::element_line()

      , legend.title = ggplot2::element_text()
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_blank()
      , panel.grid.minor.x = ggplot2::element_blank()
      , panel.grid.major.y = ggplot2::element_blank()
      , panel.grid.minor.y = ggplot2::element_blank()

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, ggplot2::rel(10), 0))
      , strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(10)))
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}
```

``` r
plot = ggplot(data = brutos, aes(x = prime_type, y = rt, fill = interaction(correct,prime_type))) + geom_boxplot() + facet_wrap(~phase) + theme_apa()

plot
```

![](self_esteem_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
brutos2 = brutos %>% group_by(phase, prime_type, prime_val,target) %>% summarize(soma = sum(correct)) 
```

    ## `summarise()` has grouped output by 'phase', 'prime_type', 'prime_val'. You can
    ## override using the `.groups` argument.

``` r
for (i in 1:nrow(brutos2)){
  if(brutos2$prime_type[i] == "OTHER"){
    brutos2$soma[i] = brutos2$soma[i]/5
  }
}

brutos2
```

    ## # A tibble: 40 × 5
    ## # Groups:   phase, prime_type, prime_val [8]
    ##    phase        prime_type prime_val target     soma
    ##    <chr>        <chr>      <chr>     <chr>     <dbl>
    ##  1 menstruation OTHER      negative  falhada    116.
    ##  2 menstruation OTHER      negative  feia       117.
    ##  3 menstruation OTHER      negative  insegura   121.
    ##  4 menstruation OTHER      negative  inútil     132 
    ##  5 menstruation OTHER      negative  triste     123.
    ##  6 menstruation OTHER      positive  alegre     150.
    ##  7 menstruation OTHER      positive  capaz      138.
    ##  8 menstruation OTHER      positive  confiante  141.
    ##  9 menstruation OTHER      positive  genial     134 
    ## 10 menstruation OTHER      positive  linda      166 
    ## # … with 30 more rows

``` r
brutos2%>% 
  ggplot(aes(x = target, y = soma, fill = prime_val)) +
    geom_bar(position="dodge",stat = "identity")+
    #scale_fill_manual(values = c ('royalblue1', 'grey2', 'yellow1'))+
    ylab("Values")+
    xlab("")+
    facet_grid(prime_type ~ phase) + theme_apa()+
  theme(axis.text.x = element_text(angle = 95, vjust = 0.5,hjust = 1)) -> plot

plot
```

![](self_esteem_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \#filter
by beautiful and ugly

``` r
brutos2 %>% subset(target %in% c("linda","feia")) %>% 
  ggplot(aes(x = target, y = soma, fill = prime_val)) +
    geom_bar(position="dodge",stat = "identity")+
    #scale_fill_manual(values = c ('royalblue1', 'grey2', 'yellow1'))+
    ylab("Values")+
    xlab("")+
    facet_grid(prime_type ~ phase) + theme_apa()+
  theme(axis.text.x = element_text(angle = 95, vjust = 0.5,hjust = 1)) 
```

![](self_esteem_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#brutos = brutos %>% subset(target %in% c("linda","feia")) 
```

``` r
brutos %>% group_by(participant, phase) %>% summarize(soma = sum(correct)) %>% arrange(desc(soma)) ->grafico
```

    ## `summarise()` has grouped output by 'participant'. You can override using the
    ## `.groups` argument.

``` r
ggplot(grafico, aes(x = reorder(participant, -soma), y = soma)) + geom_bar(position="dodge",stat = "identity") +  coord_flip()+ facet_wrap(~phase) #+ geom_hline(yintercept=, linetype="dashed", 
```

![](self_esteem_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
      #          color = "red", size=2)
```

``` r
#brutos %>% group_by(participant, phase) %>% tally() %>% arrange(n)
```

``` r
#brutos %>% group_by(participant, phase) %>% summarise_at(vars(rt), funs(rt_mean = mean(., na.rm = T))) %>% arrange(desc(rt_mean))
```

\#remove participants mj690 & cc522 *few responses *few corrects \*high
rt

``` r
brutos = brutos %>% subset(!(participant %in% c("mj690","cc522","sm247")))
```

\#ROC-CURVE

``` r
head(brutos)
```

    ##   participant line bloco        rt correct condition prime_val prime_type
    ## 1       aa556    0     1 0.4174751       0         2  positive      OTHER
    ## 2       aa556    1     1 0.1681083       1         2  positive      OTHER
    ## 3       aa556    2     1 0.5012267       0         2  negative      OTHER
    ## 4       aa556    3     1 0.4864544       1         2  negative      OTHER
    ## 5       aa556    4     1 0.4847672       0         2  negative      OTHER
    ## 6       aa556    5     1 0.5007748       1         2  positive      OTHER
    ##        picture target        phase
    ## 1    VPN07.jpg  linda menstruation
    ## 2    VPN09.jpg  capaz menstruation
    ## 3    VPN07.jpg inútil menstruation
    ## 4    VPN04.jpg   feia menstruation
    ## 5    VPN02.jpg triste menstruation
    ## 6 femcon_2.jpg genial menstruation

``` r
response = c()
for (i in 1:nrow(brutos)){
  if (brutos$prime_val[i] == "positive"){
    if (brutos$correct[i] == 1){
      response = c(response, 1)
    }
    else{
      response = c(response,0)
    }
  }
  else{
    if(brutos$correct[i] == 1){
      response = c(response,0)
    }
    else{
      response = c(response,1)
    }
    
  }
}

expected = response

library(caTools)

library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
response = c()
for (i in 1:nrow(brutos)){
  if(brutos$prime_val[i]=="positive"){
    response = c(response,1)
  }
  else{
    response = c(response,0)
  }
}



test_roc = roc(expected ~ response, plot = TRUE, print.auc = TRUE)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](self_esteem_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
roc_df = data.frame(brutos$phase,brutos$prime_type,expected,response)


self_ovulation_df = roc_df %>% subset(brutos.phase == "ovulation" & brutos.prime_type=="SELF")
self_menstruation_df = roc_df %>% subset(brutos.phase == "menstruation" & brutos.prime_type=="SELF")

other_ovulation_df = roc_df %>% subset(brutos.phase == "ovulation" & brutos.prime_type=="OTHER")
other_menstruation_df = roc_df %>% subset(brutos.phase == "menstruation" & brutos.prime_type=="OTHER")



# create ROC curves for each condition
roc1 <- roc(self_ovulation_df$expected,self_ovulation_df$response)
```

    ## Setting levels: control = 0, case = 1
    ## Setting direction: controls < cases

``` r
roc2 <- roc(self_menstruation_df$expected,self_menstruation_df$response)
```

    ## Setting levels: control = 0, case = 1
    ## Setting direction: controls < cases

``` r
roc3 = roc(other_ovulation_df$expected,other_ovulation_df$response)
```

    ## Setting levels: control = 0, case = 1
    ## Setting direction: controls < cases

``` r
roc4 = roc(other_menstruation_df$expected,other_menstruation_df$response)
```

    ## Setting levels: control = 0, case = 1
    ## Setting direction: controls < cases

``` r
ggroc(list(self_ovulation = roc1, self_menstruation = roc2, other_ovulation = roc3, other_menstruation = roc4 )) +
   scale_color_manual(values = c("self_ovulation"="brown2","self_menstruation" = "Tan","other_ovulation" = "blue","other_menstruation" ="green"),
                      labels = c(paste0("self ovulation ",'ROC Curve ', '(AUC = ', .74, ')')
                                 ,paste0("self menstruation ",'ROC Curve ', '(AUC = ',.71, ')')
                                 ,paste0("other ovulation ",'ROC Curve ', '(AUC = ', .73, ')')
                                 ,paste0("other menstruation ",'ROC Curve ', '(AUC = ', .73, ')'))) + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

\#missing responses-add to dataframe

``` r
codes = c("ma889","ma929","mr971","na210","nj267","va606")

p1=list("ma889",0,1,0.42,0,1,"negative","SELF","x","x","menstruation")
p2=list("ma929",0,4,0.42,0,2,"negative","SELF","x","x","menstruation")
p3=list("mr971",0,4,0.42,0,2,"negative","SELF","x","x","ovulation")
p4=list("na210",0,2,0.42,0,2,"positive","SELF","x","x","menstruation")
p5=list("nj267",0,3,0.42,0,1,"negative","SELF","x","x","menstruation")
p6=list("va606",0,1,0.42,0,1,"positive","SELF","x","x","menstruation")

nested = list(p1,p2,p3,p4,p5,p6)
df_nested <-  as.data.frame(do.call(rbind, nested))

colnames(df_nested) = colnames(brutos)

library(tidyr)
unnested = df_nested %>% unnest(colnames(df_nested))
unnested = data.frame(unnested)
colnames(unnested) = colnames(brutos)

brutos = rbind(brutos,unnested)
```

# POSITIVITY INDEX - SELF

``` r
brutos %>% group_by(participant,phase,bloco, prime_val, prime_type) %>% summarize(soma = sum(correct)) -> df
```

    ## `summarise()` has grouped output by 'participant', 'phase', 'bloco',
    ## 'prime_val'. You can override using the `.groups` argument.

``` r
taxa = c()
x = 25
y = 5
  
for (i in 1:nrow(df)){
  if(df$prime_type[i] == "OTHER"){
    taxa = c(taxa,(df$soma[i]/x))
  } 
  else{
    taxa = c(taxa,(df$soma[i]/y))
  }
}

df$taxa = taxa
```

\#don’t

``` r
data= data.frame(matrix(ncol=7,nrow = 0))
colnames(data) = colnames(df)

blocks = c(1,2,3,4,5)

#for each participant
for (participant in levels(factor(df$participant))){
  #for each phase
  for(phase in phases){
    #for each block
    for (block in blocks){
      
            data[nrow(data) + 1,] =list(participant,phase,block, "negative","OTHER",0,0)
            
            data[nrow(data) + 1,] =list(participant,phase,block, "negative","SELF",0,0)

            data[nrow(data) + 1,] =list(participant,phase,block, "positive","OTHER",0,0)
            data[nrow(data) + 1,] =list(participant,phase,block, "positive","SELF",0,0)
    
    }
  }
}
```

\#don’t

``` r
df = rbind(df,data)
df%>% group_by(participant,phase,bloco, prime_val, prime_type) %>% summarize(taxa = sum(taxa))->df
```

``` r
index = c()
bloco = 1
counter = 1

for(i in 1:(nrow(df)-2)){
  if (df$bloco[i+2] == bloco & counter < 3){
    index = c(index,df$taxa[i]-df$taxa[i+2])
  }
  else {
    bloco = df$bloco[i+2]
  }
  if (counter == 4){
    counter = 0
  }
  counter = counter+1
}
```

\#create dataframe

``` r
participants = levels(factor(df$participant))
participants = rep(participants, each = 20)


phase = rep(c("menstruation","ovulation"), each = 10, times = 10)

bloco = rep(c(1,2,3,4,5),each = 2,times = 100)

condition = rep(c("OTHER","SELF"),times = 500)

colunas = c("participant","phase","bloco","condition","index")

df_index = data.frame(participants,phase,bloco,condition, index) 
colnames(df_index) = colunas
```

\#remove participants

``` r
df %>% subset(!(participant %in% c("sp123","sm247","mj690","cc522"))) -> df
```

``` r
df_index %>% group_by(phase,condition) %>% summarise_at(vars(index), funs(rt_mean = mean(., na.rm = T))) 
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

    ## # A tibble: 4 × 3
    ## # Groups:   phase [2]
    ##   phase        condition rt_mean
    ##   <chr>        <chr>       <dbl>
    ## 1 menstruation OTHER     -0.0976
    ## 2 menstruation SELF      -0.119 
    ## 3 ovulation    OTHER     -0.0859
    ## 4 ovulation    SELF      -0.116

\#symmetric index

``` r
df_index$index = -df_index$index
```

``` r
ggplot(data = df_index, aes(x = condition, y = index, color = phase)) + geom_boxplot() + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
#head(df,20)
```

``` r
#df_index %>% group_by(participant,phase,condition) %>% summarise_at(vars(index), funs(rt_mean = mean(., na.rm = T))) %>% arrange(rt_mean)
```

\#remove outlier

``` r
df_index = df_index %>% subset(participant!="ij887" & participant != "mr971") 
```

``` r
hist(df_index$index)
```

![](self_esteem_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
df_index %>% group_by(phase,condition) %>% summarise_at(vars(index), funs(index = mean(., na.rm = T)))-> df_index2 
df_index %>% group_by(phase,condition) %>% summarise_at(vars(index), funs(se = std.error(., na.rm = T))) %>% pull(se) ->se

df_index2$se = se

# Create bar plot with error bars
ggplot(df_index2, aes(x = condition, y = index, fill = phase)) +
  geom_bar(stat = "identity",position = position_dodge(0.9)) +   scale_fill_manual(values = c("brown2", "Tan1")) +
  geom_errorbar(aes(ymin = index - se, ymax = index + se), width = 0.2,position = position_dodge(0.9)) + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
df_index %>% group_by(participant,phase,condition) %>% summarise_at(vars(index), funs(index = mean(., na.rm = T))) %>% ggplot(aes(x = condition, y = index, color = phase)) + geom_boxplot() +theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 4.2.2

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## 
    ## Attaching package: 'lme4'

    ## The following object is masked from 'package:rio':
    ## 
    ##     factorize

``` r
library(afex)
```

    ## Warning: package 'afex' was built under R version 4.2.2

    ## ************
    ## Welcome to afex. For support visit: http://afex.singmann.science/

    ## - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    ## - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    ## - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    ## - NEWS: emmeans() for ANOVA models now uses model = 'multivariate' as default.
    ## - Get and set global package options with: afex_options()
    ## - Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()
    ## - For example analyses see: browseVignettes("afex")
    ## ************

    ## 
    ## Attaching package: 'afex'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

``` r
df_index$participant = factor(df_index$participant)
df_index$phase = factor(df_index$phase)
df_index$bloco = factor(df_index$bloco)
df_index$condition = factor(df_index$condition)
```

### check contrabalanceamento

``` r
contrabalanceamento = read.csv("contrabalanceamento.csv")
```

``` r
#to lower case all cols
names(contrabalanceamento) <- tolower(names(contrabalanceamento))

contrabalanceamento$participant = tolower(contrabalanceamento$participant)
```

\#left-join

``` r
df_index = df_index %>% left_join(contrabalanceamento)
```

    ## Joining, by = "participant"

``` r
model = lmer(index~condition*phase + tecla+contrabalanceamento+fase_start +(1|participant), df_index)

summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## index ~ condition * phase + tecla + contrabalanceamento + fase_start +  
    ##     (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 129.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6502 -0.5928 -0.0004  0.5751  3.5244 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.004787 0.06919 
    ##  Residual                0.061341 0.24767 
    ## Number of obs: 940, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                    0.084300   0.046906  49.378215   1.797   0.0784
    ## conditionSELF                 -0.005106   0.022848 890.000001  -0.223   0.8232
    ## phaseovulation                -0.017191   0.022848 890.000001  -0.752   0.4520
    ## tecla                          0.012207   0.026058  40.999999   0.468   0.6419
    ## contrabalanceamentoboth_imp    0.009592   0.035273  40.999999   0.272   0.7870
    ## contrabalanceamentoexp_imp    -0.020655   0.039373  40.999999  -0.525   0.6027
    ## contrabalanceamentoimp_exp    -0.030160   0.035389  40.999999  -0.852   0.3990
    ## fase_startovulation            0.019235   0.027000  40.999999   0.712   0.4803
    ## conditionSELF:phaseovulation   0.031660   0.032312 890.000001   0.980   0.3275
    ##                               
    ## (Intercept)                  .
    ## conditionSELF                 
    ## phaseovulation                
    ## tecla                         
    ## contrabalanceamentoboth_imp   
    ## contrabalanceamentoexp_imp    
    ## contrabalanceamentoimp_exp    
    ## fase_startovulation           
    ## conditionSELF:phaseovulation  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) cnSELF phsvlt tecla  cntrblncmntb_ cntrblncmntx_
    ## conditnSELF   -0.244                                                 
    ## phaseovultn   -0.244  0.500                                          
    ## tecla         -0.793  0.000  0.000                                   
    ## cntrblncmntb_ -0.232  0.000  0.000 -0.114                            
    ## cntrblncmntx_ -0.195  0.000  0.000 -0.083  0.440                     
    ## cntrblncmntm_ -0.270  0.000  0.000 -0.052  0.472         0.450       
    ## fs_strtvltn   -0.162  0.000  0.000 -0.003 -0.100        -0.261       
    ## cndtnSELF:p    0.172 -0.707 -0.707  0.000  0.000         0.000       
    ##               cntrblncmntm_ fs_str
    ## conditnSELF                       
    ## phaseovultn                       
    ## tecla                             
    ## cntrblncmntb_                     
    ## cntrblncmntx_                     
    ## cntrblncmntm_                     
    ## fs_strtvltn   -0.163              
    ## cndtnSELF:p    0.000         0.000

``` r
model = lmer(index~condition*phase +(1|participant), df_index)

summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: index ~ condition * phase + (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 96.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6152 -0.5896  0.0043  0.5482  3.5911 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.004328 0.06579 
    ##  Residual                0.060610 0.24619 
    ## Number of obs: 960, groups:  participant, 48
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                   1.023e-01  1.851e-02  2.177e+02   5.528 9.23e-08
    ## conditionSELF                 1.667e-04  2.247e-02  9.090e+02   0.007    0.994
    ## phaseovulation               -1.567e-02  2.247e-02  9.090e+02  -0.697    0.486
    ## conditionSELF:phaseovulation  2.483e-02  3.178e-02  9.090e+02   0.781    0.435
    ##                                 
    ## (Intercept)                  ***
    ## conditionSELF                   
    ## phaseovulation                  
    ## conditionSELF:phaseovulation    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) cnSELF phsvlt
    ## conditnSELF -0.607              
    ## phaseovultn -0.607  0.500       
    ## cndtnSELF:p  0.429 -0.707 -0.707

``` r
library(emmeans)
noise <- emmeans(model,~condition*phase)


x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  phase        condition contrast                  estimate     SE  df t.ratio
    ##  menstruation .         OTHER - SELF             -0.000167 0.0225 909  -0.007
    ##  ovulation    .         OTHER - SELF             -0.025000 0.0225 909  -1.112
    ##  .            OTHER     menstruation - ovulation  0.015667 0.0225 909   0.697
    ##  .            SELF      menstruation - ovulation -0.009167 0.0225 909  -0.408
    ##  p.value
    ##   1.0000
    ##   1.0000
    ##   1.0000
    ##   1.0000
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## P value adjustment: bonferroni method for 4 tests

### ESCALAS

``` r
rosenberg = data$Rosenberg
hp = data$HP
```

``` r
rosenberg %>% dplyr::select(CODE,M_Ros_F1,M_Ros_F2) ->rosenberg
hp %>% dplyr::select(CODE, M_HP_P_1,M_HP_P_2,M_HP_S_1,M_HP_S_2,M_HP_A_1,M_HP_A_2)->hp
```

``` r
head(hp)
```

    ##    CODE M_HP_P_1 M_HP_P_2 M_HP_S_1 M_HP_S_2 M_HP_A_1 M_HP_A_2
    ## 1 MJ960 4.000000 4.428571      5.0      3.2      3.8      3.6
    ## 2 ms416 4.142857 2.571429      4.6      3.2      5.4      5.4
    ## 3 SP513 4.000000 4.857143      5.8      6.2      6.6      5.6
    ## 4 MJ714 4.714286 4.142857      5.6      3.6      4.4      4.4
    ## 5 fb968 4.428571 4.428571      7.0      7.0      5.0      5.2
    ## 6 MA969 2.714286 3.714286      5.4      5.2      2.2      3.8

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
# Melt the data frame to long format using two conditions
hp = hp %>%
  pivot_longer(cols = starts_with("M_HP_"), 
               names_to = c("condition", "phase"), 
               names_pattern = c("(M_HP_[PSA]_)([12])")) 

colnames(hp)[1] = "participant"

hp$participant = tolower(hp$participant)

hp <- hp %>%
  mutate(phase = if_else(phase == "1", "ovulation", "menstruation"))
```

\#rename participants’ codes

``` r
hp %>%
  mutate(participant = case_when(participant == "av116"~"ab116",
                         participant == "mj960"~"bj960",
                         participant ==  "sp513"~"sj513",
                         participant ==  "mj714"~"mm714",
                         participant == "ms416"~"sm416" ,
                         participant == "af8411"~ "af841",
                         participant == "fb968" ~"mb968",
                         participant == "rj073" ~"rj673",
                        TRUE ~ participant
                                ))-> hp
```

\#left join

``` r
length(levels(factor(df_index$participant)))
```

    ## [1] 48

``` r
df_merged <- df_index %>%
  left_join(hp, by = c("participant","phase"))

df_merged %>%
  mutate(condition.y = case_when(condition.y == "M_HP_P_"~"Performance",
                         condition.y == "M_HP_S_"~"Social",
                         condition.y ==  "M_HP_A_"~"Attractiveness"
                                ))-> df_merged

df_merged$participant = factor(df_merged$participant)
```

``` r
ggplot(data = df_merged, aes(x = condition.y, y = value, color = phase)) + geom_boxplot() + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
df_merged %>% group_by(phase,condition.y) %>% summarise_at(vars(value), funs(value = mean(., na.rm = T)))-> df_merged2 

df_merged %>% group_by(phase,condition.y) %>% summarise_at(vars(value), funs(se = std.error(., na.rm = T))) %>% pull(se) ->se

df_merged2$se = se

# Create bar plot with error bars
ggplot(df_merged2, aes(x = condition.y, y = value, fill = phase)) +
  geom_bar(stat = "identity",position = position_dodge(0.9)) +   scale_fill_manual(values = c("brown2", "Tan1")) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2,position = position_dodge(0.9)) + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

\#using hp scale as predictor of index \#wide format

``` r
df_wide <- df_merged %>%
  pivot_wider(id_cols = c(participant, phase, bloco, condition.x, index),
              names_from = condition.y,
              values_from = value)
```

\#subtract index of self-other

``` r
index = c()
for (i in 1:nrow(df_wide)-1){
  if(i%%2!=0){
  index = c(index,df_wide$index[i+1]-df_wide$index[i])
  }
}  
length(index)*2
```

    ## [1] 960

``` r
#create dataframe
participants = levels(factor(df_wide$participant))
participants = rep(participants, each = 10)


phase = rep(c("menstruation","ovulation"), each = 5, times = 48)

bloco = rep(c(1,2,3,4,5),each = ,times = 96)

#condition = rep(c("OTHER","SELF"),times = 500)

colunas = c("participant","phase","bloco","index_global")

df_index = data.frame(participants,phase,bloco, index) 
colnames(df_index) = colunas


df_wide %>% group_by(participant,phase,bloco) %>% summarize(Attractiveness = mean(Attractiveness)) %>% pull(Attractiveness) -> Attractiveness
```

    ## `summarise()` has grouped output by 'participant', 'phase'. You can override
    ## using the `.groups` argument.

``` r
df_wide %>% group_by(participant,phase,bloco) %>% summarize(Social = mean(Social)) %>% pull(Social) -> Social
```

    ## `summarise()` has grouped output by 'participant', 'phase'. You can override
    ## using the `.groups` argument.

``` r
df_wide %>% group_by(participant,phase,bloco) %>% summarize(Performance = mean(Performance)) %>% pull(Performance) -> Performance
```

    ## `summarise()` has grouped output by 'participant', 'phase'. You can override
    ## using the `.groups` argument.

``` r
df_index$Performance = Performance
df_index$Attractiveness = Attractiveness
df_index$Social = Social
```

``` r
df_index = df_index %>% left_join(contrabalanceamento)
```

    ## Joining, by = "participant"

``` r
df_index$bloco = factor(df_index$bloco)


model = lmer(Attractiveness~phase + tecla + contrabalanceamento+ fase_start+(1|participant), df_index)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Attractiveness ~ phase + tecla + contrabalanceamento + fase_start +  
    ##     (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 587.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.69373 -0.44167  0.00497  0.47217  2.63715 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 1.8182   1.3484  
    ##  Residual                0.1245   0.3528  
    ## Number of obs: 470, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                              Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)                   4.60388    0.68370  41.04649   6.734  3.9e-08 ***
    ## phaseovulation                0.28085    0.03255 422.00000   8.629  < 2e-16 ***
    ## tecla                        -0.14342    0.39782  40.99999  -0.361    0.720    
    ## contrabalanceamentoboth_imp   0.61526    0.53851  41.00000   1.143    0.260    
    ## contrabalanceamentoexp_imp    0.14855    0.60110  41.00000   0.247    0.806    
    ## contrabalanceamentoimp_exp    0.81481    0.54028  41.00000   1.508    0.139    
    ## fase_startovulation          -0.13798    0.41221  41.00000  -0.335    0.740    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) phsvlt tecla  cntrblncmntb_ cntrblncmntx_ cntrblncmntm_
    ## phaseovultn   -0.024                                                        
    ## tecla         -0.831  0.000                                                 
    ## cntrblncmntb_ -0.243  0.000 -0.114                                          
    ## cntrblncmntx_ -0.204  0.000 -0.083  0.440                                   
    ## cntrblncmntm_ -0.283  0.000 -0.052  0.472         0.450                     
    ## fs_strtvltn   -0.170  0.000 -0.003 -0.100        -0.261        -0.163

### Attractiveness simple model

``` r
model = lmer(Attractiveness~phase+(1|participant), df_index)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Attractiveness ~ phase + (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 617.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.63076 -0.47912  0.00467  0.50195  2.55294 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 1.7146   1.3094  
    ##  Residual                0.1285   0.3585  
    ## Number of obs: 480, groups:  participant, 48
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)      4.74583    0.19041  47.70166  24.924  < 2e-16 ***
    ## phaseovulation   0.25833    0.03273 431.00000   7.894 2.45e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## phaseovultn -0.086

``` r
library(emmeans)
noise <- emmeans(model,~phase)


x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  contrast                 estimate     SE  df t.ratio p.value
    ##  menstruation - ovulation   -0.258 0.0327 431  -7.894  <.0001
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
eff_size(noise, sigma=sigma(model), edf = df.residual(model))
```

    ##  contrast                 effect.size     SE   df lower.CL upper.CL
    ##  menstruation - ovulation      -0.721 0.0942 47.7    -0.91   -0.531
    ## 
    ## sigma used for effect sizes: 0.3585 
    ## Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
    ## Confidence level used: 0.95

``` r
model = lmer(Social~phase + tecla + contrabalanceamento*phase+ fase_start+(1|participant), df_index)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: Social ~ phase + tecla + contrabalanceamento * phase + fase_start +  
    ##     (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 1008.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.89047 -0.53019  0.00547  0.50825  2.82857 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 1.908    1.381   
    ##  Residual                0.333    0.577   
    ## Number of obs: 470, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error        df
    ## (Intercept)                                  4.96291    0.70563  41.39440
    ## phaseovulation                               0.18571    0.09753 419.00000
    ## tecla                                       -0.44677    0.40971  41.00000
    ## contrabalanceamentoboth_imp                  0.52212    0.55924  42.38400
    ## contrabalanceamentoexp_imp                   0.67852    0.62396  42.30964
    ## contrabalanceamentoimp_exp                   1.88375    0.56105  42.37488
    ## fase_startovulation                         -0.18636    0.42453  41.00000
    ## phaseovulation:contrabalanceamentoboth_imp   0.51429    0.14357 419.00000
    ## phaseovulation:contrabalanceamentoexp_imp   -0.27460    0.15592 419.00000
    ## phaseovulation:contrabalanceamentoimp_exp   -0.50238    0.14357 419.00000
    ##                                            t value Pr(>|t|)    
    ## (Intercept)                                  7.033 1.41e-08 ***
    ## phaseovulation                               1.904 0.057582 .  
    ## tecla                                       -1.090 0.281887    
    ## contrabalanceamentoboth_imp                  0.934 0.355787    
    ## contrabalanceamentoexp_imp                   1.087 0.282999    
    ## contrabalanceamentoimp_exp                   3.358 0.001670 ** 
    ## fase_startovulation                         -0.439 0.662977    
    ## phaseovulation:contrabalanceamentoboth_imp   3.582 0.000381 ***
    ## phaseovulation:contrabalanceamentoexp_imp   -1.761 0.078936 .  
    ## phaseovulation:contrabalanceamentoimp_exp   -3.499 0.000516 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) phsvlt tecla  cntrblncmntb_ cntrblncmntx_
    ## phaseovultn           -0.069                                          
    ## tecla                 -0.829  0.000                                   
    ## cntrblncmntb_         -0.247  0.087 -0.113                            
    ## cntrblncmntx_         -0.207  0.078 -0.083  0.439                     
    ## cntrblncmntm_         -0.286  0.087 -0.052  0.472         0.449       
    ## fs_strtvltn           -0.169  0.000 -0.003 -0.099        -0.259       
    ## phsvltn:cntrblncmntb_  0.047 -0.679  0.000 -0.128        -0.053       
    ## phsvltn:cntrblncmntx_  0.043 -0.626  0.000 -0.055        -0.125       
    ## phsvltn:cntrblncmntm_  0.047 -0.679  0.000 -0.059        -0.053       
    ##                       cntrblncmntm_ fs_str phsvltn:cntrblncmntb_
    ## phaseovultn                                                     
    ## tecla                                                           
    ## cntrblncmntb_                                                   
    ## cntrblncmntx_                                                   
    ## cntrblncmntm_                                                   
    ## fs_strtvltn           -0.162                                    
    ## phsvltn:cntrblncmntb_ -0.059         0.000                      
    ## phsvltn:cntrblncmntx_ -0.054         0.000  0.425               
    ## phsvltn:cntrblncmntm_ -0.128         0.000  0.462               
    ##                       phsvltn:cntrblncmntx_
    ## phaseovultn                                
    ## tecla                                      
    ## cntrblncmntb_                              
    ## cntrblncmntx_                              
    ## cntrblncmntm_                              
    ## fs_strtvltn                                
    ## phsvltn:cntrblncmntb_                      
    ## phsvltn:cntrblncmntx_                      
    ## phsvltn:cntrblncmntm_  0.425

``` r
library(sjPlot)
```

    ## Warning: package 'sjPlot' was built under R version 4.2.3

``` r
plot_model(model, type = "pred", terms = c("contrabalanceamento"))
```

![](self_esteem_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

``` r
library(sjPlot)
plot_model(model, type = "pred", terms = c("phase","contrabalanceamento"))
```

![](self_esteem_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r
library(emmeans)
noise <- emmeans(model,~phase)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  contrast                 estimate     SE  df t.ratio p.value
    ##  menstruation - ovulation    -0.12 0.0539 419  -2.227  0.0265
    ## 
    ## Results are averaged over some or all of the levels of: tecla, contrabalanceamento, fase_start 
    ## Degrees-of-freedom method: kenward-roger

``` r
eff_size(noise, sigma=sigma(model), edf = df.residual(model))
```

    ##  contrast                 effect.size     SE   df lower.CL upper.CL
    ##  menstruation - ovulation      -0.208 0.0937 42.4   -0.397   -0.019
    ## 
    ## Results are averaged over the levels of: tecla, contrabalanceamento, fase_start 
    ## sigma used for effect sizes: 0.577 
    ## Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
    ## Confidence level used: 0.95

``` r
model = lmer(Performance~phase + tecla + contrabalanceamento*phase+ fase_start+ (1|participant), df_index)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## Performance ~ phase + tecla + contrabalanceamento * phase + fase_start +  
    ##     (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 330.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.44017 -0.63643 -0.00295  0.61111  2.25906 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.30274  0.5502  
    ##  Residual                0.07907  0.2812  
    ## Number of obs: 470, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error        df
    ## (Intercept)                                  3.95454    0.28257  41.58608
    ## phaseovulation                               0.42857    0.04753 419.00000
    ## tecla                                        0.06231    0.16388  41.00000
    ## contrabalanceamentoboth_imp                  0.21567    0.22458  43.06190
    ## contrabalanceamentoexp_imp                   0.02634    0.25052  42.95075
    ## contrabalanceamentoimp_exp                   0.70083    0.22530  43.04826
    ## fase_startovulation                         -0.18813    0.16981  41.00000
    ## phaseovulation:contrabalanceamentoboth_imp  -0.17857    0.06996 419.00000
    ## phaseovulation:contrabalanceamentoexp_imp   -0.34921    0.07598 419.00000
    ## phaseovulation:contrabalanceamentoimp_exp   -0.41667    0.06996 419.00000
    ##                                            t value Pr(>|t|)    
    ## (Intercept)                                 13.995  < 2e-16 ***
    ## phaseovulation                               9.017  < 2e-16 ***
    ## tecla                                        0.380  0.70576    
    ## contrabalanceamentoboth_imp                  0.960  0.34224    
    ## contrabalanceamentoexp_imp                   0.105  0.91675    
    ## contrabalanceamentoimp_exp                   3.111  0.00331 ** 
    ## fase_startovulation                         -1.108  0.27436    
    ## phaseovulation:contrabalanceamentoboth_imp  -2.552  0.01106 *  
    ## phaseovulation:contrabalanceamentoexp_imp   -4.596 5.72e-06 ***
    ## phaseovulation:contrabalanceamentoimp_exp   -5.955 5.50e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) phsvlt tecla  cntrblncmntb_ cntrblncmntx_
    ## phaseovultn           -0.084                                          
    ## tecla                 -0.828  0.000                                   
    ## cntrblncmntb_         -0.248  0.106 -0.113                            
    ## cntrblncmntx_         -0.209  0.095 -0.082  0.439                     
    ## cntrblncmntm_         -0.287  0.105 -0.051  0.472         0.449       
    ## fs_strtvltn           -0.169  0.000 -0.003 -0.099        -0.258       
    ## phsvltn:cntrblncmntb_  0.057 -0.679  0.000 -0.156        -0.064       
    ## phsvltn:cntrblncmntx_  0.053 -0.626  0.000 -0.066        -0.152       
    ## phsvltn:cntrblncmntm_  0.057 -0.679  0.000 -0.072        -0.064       
    ##                       cntrblncmntm_ fs_str phsvltn:cntrblncmntb_
    ## phaseovultn                                                     
    ## tecla                                                           
    ## cntrblncmntb_                                                   
    ## cntrblncmntx_                                                   
    ## cntrblncmntm_                                                   
    ## fs_strtvltn           -0.161                                    
    ## phsvltn:cntrblncmntb_ -0.072         0.000                      
    ## phsvltn:cntrblncmntx_ -0.066         0.000  0.425               
    ## phsvltn:cntrblncmntm_ -0.155         0.000  0.462               
    ##                       phsvltn:cntrblncmntx_
    ## phaseovultn                                
    ## tecla                                      
    ## cntrblncmntb_                              
    ## cntrblncmntx_                              
    ## cntrblncmntm_                              
    ## fs_strtvltn                                
    ## phsvltn:cntrblncmntb_                      
    ## phsvltn:cntrblncmntx_                      
    ## phsvltn:cntrblncmntm_  0.425

``` r
plot_model(model, type = "pred", terms = c("phase","contrabalanceamento"))
```

![](self_esteem_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
library(emmeans)
noise <- emmeans(model,~phase)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  contrast                 estimate     SE  df t.ratio p.value
    ##  menstruation - ovulation   -0.192 0.0263 419  -7.325  <.0001
    ## 
    ## Results are averaged over some or all of the levels of: tecla, contrabalanceamento, fase_start 
    ## Degrees-of-freedom method: kenward-roger

``` r
eff_size(noise, sigma=sigma(model), edf = df.residual(model))
```

    ##  contrast                 effect.size     SE   df lower.CL upper.CL
    ##  menstruation - ovulation      -0.684 0.0961 43.1   -0.878   -0.491
    ## 
    ## Results are averaged over the levels of: tecla, contrabalanceamento, fase_start 
    ## sigma used for effect sizes: 0.2812 
    ## Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
    ## Confidence level used: 0.95

``` r
ggplot(data = df_index, aes(x = phase, y = index_global)) + geom_boxplot() + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
df_index %>% group_by(phase) %>% summarise_at(vars(index_global), funs(value = mean(., na.rm = T)))-> df_index2 

df_index %>% group_by(phase) %>% summarise_at(vars(index_global), funs(se = std.error(., na.rm = T))) %>% pull(se) ->se

df_index2$se = se

# Create bar plot with error bars
ggplot(df_index2, aes(x = phase, y = value, fill = phase)) +
  geom_bar(stat = "identity",position = position_dodge(0.9)) +   scale_fill_manual(values = c("brown2", "Tan1")) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2,position = position_dodge(0.9)) + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
ggplot(df_index, aes(x = phase, y = index_global)) + geom_boxplot() + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-67-2.png)<!-- -->

``` r
model = lmer(index_global~phase + + tecla + contrabalanceamento+ fase_start+(1|participant), df_index)

summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: index_global ~ phase + +tecla + contrabalanceamento + fase_start +  
    ##     (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 402.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4356 -0.6666 -0.0025  0.6588  3.0694 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.01647  0.1283  
    ##  Residual                0.12043  0.3470  
    ## Number of obs: 470, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                   0.018375   0.086786  43.936125   0.212    0.833
    ## phaseovulation                0.031660   0.032015 421.998146   0.989    0.323
    ## tecla                         0.002595   0.049645  41.002429   0.052    0.959
    ## contrabalanceamentoboth_imp  -0.094859   0.067203  41.002429  -1.412    0.166
    ## contrabalanceamentoexp_imp   -0.046669   0.075014  41.002429  -0.622    0.537
    ## contrabalanceamentoimp_exp    0.006249   0.067424  41.002429   0.093    0.927
    ## fase_startovulation           0.009309   0.051441  41.002429   0.181    0.857
    ## 
    ## Correlation of Fixed Effects:
    ##               (Intr) phsvlt tecla  cntrblncmntb_ cntrblncmntx_ cntrblncmntm_
    ## phaseovultn   -0.184                                                        
    ## tecla         -0.817  0.000                                                 
    ## cntrblncmntb_ -0.239  0.000 -0.114                                          
    ## cntrblncmntx_ -0.201  0.000 -0.083  0.440                                   
    ## cntrblncmntm_ -0.278  0.000 -0.052  0.472         0.450                     
    ## fs_strtvltn   -0.167  0.000 -0.003 -0.100        -0.261        -0.163

``` r
noise <- emmeans(model,~phase)


x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  contrast                 estimate    SE  df t.ratio p.value
    ##  menstruation - ovulation  -0.0317 0.032 422  -0.989  0.3233
    ## 
    ## Results are averaged over some or all of the levels of: tecla, contrabalanceamento, fase_start 
    ## Degrees-of-freedom method: kenward-roger

``` r
eff_size(noise, sigma=sigma(model), edf = df.residual(model))
```

    ##  contrast                 effect.size     SE   df lower.CL upper.CL
    ##  menstruation - ovulation     -0.0912 0.0923 80.2   -0.275   0.0924
    ## 
    ## Results are averaged over the levels of: tecla, contrabalanceamento, fase_start 
    ## sigma used for effect sizes: 0.347 
    ## Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
    ## Confidence level used: 0.95

\#Predicting global index using Attractiveness/phase

``` r
model = lmer(index_global~Attractiveness*phase + Social*phase + Performance*phase + (1|participant), df_index)

summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## index_global ~ Attractiveness * phase + Social * phase + Performance *  
    ##     phase + (1 | participant)
    ##    Data: df_index
    ## 
    ## REML criterion at convergence: 414.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6912 -0.6713  0.0045  0.6620  2.9754 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.01353  0.1163  
    ##  Residual                0.11969  0.3460  
    ## Number of obs: 480, groups:  participant, 48
    ## 
    ## Fixed effects:
    ##                                 Estimate Std. Error         df t value Pr(>|t|)
    ## (Intercept)                    -0.187988   0.184795 171.250626  -1.017    0.310
    ## Attractiveness                  0.029189   0.024779 134.363077   1.178    0.241
    ## phaseovulation                 -0.160423   0.239074 463.655729  -0.671    0.503
    ## Social                         -0.012660   0.020648 198.274897  -0.613    0.541
    ## Performance                     0.026496   0.052717 242.762166   0.503    0.616
    ## Attractiveness:phaseovulation  -0.044186   0.031157 469.313542  -1.418    0.157
    ## phaseovulation:Social           0.006665   0.025134 467.872508   0.265    0.791
    ## phaseovulation:Performance      0.082330   0.072956 434.999073   1.128    0.260
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Attrct phsvlt Social Prfrmn Attrc: phsv:S
    ## Attractvnss -0.222                                          
    ## phaseovultn -0.596  0.145                                   
    ## Social       0.192 -0.239 -0.200                            
    ## Performance -0.784 -0.236  0.501 -0.488                     
    ## Attrctvnss:  0.130 -0.557  0.096  0.142  0.122              
    ## phsvltn:Scl -0.197  0.161  0.071 -0.661  0.379 -0.195       
    ## phsvltn:Prf  0.477  0.089 -0.823  0.351 -0.604 -0.474 -0.361

``` r
#library("interactions")
#interact_plot(model, pred = Attractiveness, modx = phase) + theme_apa()
```

``` r
head(rosenberg)
```

    ##    CODE M_Ros_F1 M_Ros_F2
    ## 1 MJ960      2.7      2.7
    ## 2 ms416      3.0      2.9
    ## 3 SP513      3.7      3.8
    ## 4 MJ714      2.7      2.5
    ## 5 fb968      2.7      2.7
    ## 6 MA969      2.6      2.5

``` r
# Melt the data frame to long format using two conditions
rosenberg %>%
  pivot_longer(cols = starts_with("M_"), 
               names_to = c("condition", "phase"), 
               names_pattern = c("(M_HP_[PSA]_)([12])")) -> rosenberg2


rosenberg2$participant = tolower(rosenberg2$CODE)

phase = rep(c("menstruation","ovulation"), each = 1, times = 53)

rosenberg2$phase = phase
rosenberg2$rosenberg = rosenberg2$value

rosenberg = rosenberg2 %>% select (participant,phase,rosenberg)
```

\#rename participants’ codes

``` r
rosenberg %>%
  mutate(participant = case_when(participant == "av116"~"ab116",
                         participant == "mj960"~"bj960",
                         participant ==  "sp513"~"sj513",
                         participant ==  "mj714"~"mm714",
                         participant == "ms416"~"sm416" ,
                         participant == "af8411"~ "af841",
                         participant == "fb968" ~"mb968",
                         participant == "rj073" ~"rj673",
                        TRUE ~ participant
                                ))-> rosenberg
```

``` r
df_merged3 <- df_index %>%
  left_join(rosenberg, by = c("participant","phase"))
```

``` r
ggplot(data = df_merged3, aes(x = phase, y = rosenberg)) + geom_boxplot() + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-77-1.png)<!-- -->

``` r
rosenberg %>% group_by(phase) %>% summarise_at(vars(rosenberg), funs(value = mean(., na.rm = T)))-> df_index2 

rosenberg %>% group_by(phase) %>% summarise_at(vars(rosenberg), funs(se = std.error(., na.rm = T))) %>% pull(se) ->se

df_index2$se = se

# Create bar plot with error bars
ggplot(df_index2, aes(x = phase, y = value, fill = phase)) +
  geom_bar(stat = "identity",position = position_dodge(0.2), width = 0.5) +   scale_fill_manual(values = c("brown2", "Tan1")) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.2,position = position_dodge(0.9)) + theme_apa()
```

![](self_esteem_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

``` r
#ggplot(df_index, aes(x = phase, y = index_global)) + geom_boxplot() + theme_apa()
```

``` r
#standardize

model = lmer(rosenberg ~ phase + tecla+ contrabalanceamento*phase+fase_start+(1|participant), df_merged3)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## rosenberg ~ phase + tecla + contrabalanceamento * phase + fase_start +  
    ##     (1 | participant)
    ##    Data: df_merged3
    ## 
    ## REML criterion at convergence: -563.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.95095 -0.56457  0.00857  0.56499  2.93519 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.252840 0.50283 
    ##  Residual                0.009543 0.09769 
    ## Number of obs: 470, groups:  participant, 47
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error        df
    ## (Intercept)                                  2.94011    0.25463  41.08634
    ## phaseovulation                              -0.03571    0.01651 419.00000
    ## tecla                                        0.05003    0.14812  41.00000
    ## contrabalanceamentoboth_imp                  0.24131    0.20088  41.30171
    ## contrabalanceamentoexp_imp                  -0.17425    0.22420  41.28559
    ## contrabalanceamentoimp_exp                   0.39261    0.20154  41.29973
    ## fase_startovulation                          0.03448    0.15348  41.00000
    ## phaseovulation:contrabalanceamentoboth_imp  -0.07262    0.02431 419.00000
    ## phaseovulation:contrabalanceamentoexp_imp    0.11349    0.02640 419.00000
    ## phaseovulation:contrabalanceamentoimp_exp    0.01071    0.02431 419.00000
    ##                                            t value Pr(>|t|)    
    ## (Intercept)                                 11.547 1.78e-14 ***
    ## phaseovulation                              -2.163  0.03111 *  
    ## tecla                                        0.338  0.73726    
    ## contrabalanceamentoboth_imp                  1.201  0.23648    
    ## contrabalanceamentoexp_imp                  -0.777  0.44148    
    ## contrabalanceamentoimp_exp                   1.948  0.05822 .  
    ## fase_startovulation                          0.225  0.82339    
    ## phaseovulation:contrabalanceamentoboth_imp  -2.988  0.00298 ** 
    ## phaseovulation:contrabalanceamentoexp_imp    4.300 2.13e-05 ***
    ## phaseovulation:contrabalanceamentoimp_exp    0.441  0.65957    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                       (Intr) phsvlt tecla  cntrblncmntb_ cntrblncmntx_
    ## phaseovultn           -0.032                                          
    ## tecla                 -0.830  0.000                                   
    ## cntrblncmntb_         -0.244  0.041 -0.114                            
    ## cntrblncmntx_         -0.205  0.037 -0.083  0.440                     
    ## cntrblncmntm_         -0.283  0.041 -0.052  0.472         0.450       
    ## fs_strtvltn           -0.170  0.000 -0.003 -0.100        -0.261       
    ## phsvltn:cntrblncmntb_  0.022 -0.679  0.000 -0.060        -0.025       
    ## phsvltn:cntrblncmntx_  0.020 -0.626  0.000 -0.026        -0.059       
    ## phsvltn:cntrblncmntm_  0.022 -0.679  0.000 -0.028        -0.025       
    ##                       cntrblncmntm_ fs_str phsvltn:cntrblncmntb_
    ## phaseovultn                                                     
    ## tecla                                                           
    ## cntrblncmntb_                                                   
    ## cntrblncmntx_                                                   
    ## cntrblncmntm_                                                   
    ## fs_strtvltn           -0.163                                    
    ## phsvltn:cntrblncmntb_ -0.028         0.000                      
    ## phsvltn:cntrblncmntx_ -0.026         0.000  0.425               
    ## phsvltn:cntrblncmntm_ -0.060         0.000  0.462               
    ##                       phsvltn:cntrblncmntx_
    ## phaseovultn                                
    ## tecla                                      
    ## cntrblncmntb_                              
    ## cntrblncmntx_                              
    ## cntrblncmntm_                              
    ## fs_strtvltn                                
    ## phsvltn:cntrblncmntb_                      
    ## phsvltn:cntrblncmntx_                      
    ## phsvltn:cntrblncmntm_  0.425

``` r
plot_model(model, type = "pred", terms = c("phase","contrabalanceamento"))
```

![](self_esteem_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

``` r
model = lmer(rosenberg ~ phase +(1|participant), df_merged3)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: rosenberg ~ phase + (1 | participant)
    ##    Data: df_merged3
    ## 
    ## REML criterion at convergence: -557
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.81066 -0.61618  0.00266  0.61587  2.81223 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.26948  0.5191  
    ##  Residual                0.01032  0.1016  
    ## Number of obs: 480, groups:  participant, 48
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error         df t value Pr(>|t|)    
    ## (Intercept)      3.143750   0.075214  47.359211  41.797  < 2e-16 ***
    ## phaseovulation  -0.029167   0.009274 431.000001  -3.145  0.00178 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## phaseovultn -0.062

``` r
library(emmeans)
noise <- emmeans(model,~phase)


x = contrast(noise, "pairwise", simple = "each", combine = TRUE, adjust="bonf")
x
```

    ##  contrast                 estimate      SE  df t.ratio p.value
    ##  menstruation - ovulation   0.0292 0.00927 431   3.145  0.0018
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
eff_size(noise, sigma=sigma(model), edf = df.residual(model))
```

    ##  contrast                 effect.size     SE   df lower.CL upper.CL
    ##  menstruation - ovulation       0.287 0.0918 47.4    0.103    0.472
    ## 
    ## sigma used for effect sizes: 0.1016 
    ## Degrees-of-freedom method: inherited from kenward-roger when re-gridding 
    ## Confidence level used: 0.95

``` r
plot(df_merged3$index_global,df_merged3$rosenberg)
```

![](self_esteem_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
library("lme4")
model = lmer(index_global~phase*rosenberg + (1|participant),df_merged3)
summary(model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: index_global ~ phase * rosenberg + (1 | participant)
    ##    Data: df_merged3
    ## 
    ## REML criterion at convergence: 392.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6168 -0.6706  0.0140  0.6569  3.0871 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.01394  0.1181  
    ##  Residual                0.11893  0.3449  
    ## Number of obs: 480, groups:  participant, 48
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error        df t value Pr(>|t|)
    ## (Intercept)               -0.20821    0.17193 111.28150  -1.211    0.228
    ## phaseovulation            -0.07356    0.19266 443.08600  -0.382    0.703
    ## rosenberg                  0.06628    0.05396 111.74325   1.228    0.222
    ## phaseovulation:rosenberg   0.03221    0.06072 443.16032   0.530    0.596
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) phsvlt rsnbrg
    ## phaseovultn -0.597              
    ## rosenberg   -0.987  0.590       
    ## phsvltn:rsn  0.585 -0.987 -0.593

\#check prime_val accuracy

``` r
model = glmer(correct~prime_val*phase+(1|participant),brutos)
```

    ## Warning in glmer(correct ~ prime_val * phase + (1 | participant), brutos):
    ## calling glmer() with family=gaussian (identity link) as a shortcut to lmer() is
    ## deprecated; please call lmer() directly

``` r
summary(model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: correct ~ prime_val * phase + (1 | participant)
    ##    Data: brutos
    ## 
    ## REML criterion at convergence: 25293.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2475 -1.2020  0.4588  0.6925  1.3767 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.01014  0.1007  
    ##  Residual                0.18564  0.4309  
    ## Number of obs: 21758, groups:  participant, 50
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error t value
    ## (Intercept)                       0.669631   0.015449  43.343
    ## prime_valpositive                 0.102470   0.008325  12.309
    ## phaseovulation                    0.022741   0.008410   2.704
    ## prime_valpositive:phaseovulation -0.020967   0.011695  -1.793
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prm_vl phsvlt
    ## prim_vlpstv -0.278              
    ## phaseovultn -0.276  0.510       
    ## prm_vlpstv:  0.197 -0.712 -0.716

\#check prime_val accuracy

``` r
model = glmer(rt~prime_val*phase+(1|participant),brutos)
```

    ## Warning in glmer(rt ~ prime_val * phase + (1 | participant), brutos): calling
    ## glmer() with family=gaussian (identity link) as a shortcut to lmer() is
    ## deprecated; please call lmer() directly

``` r
summary(model)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: rt ~ prime_val * phase + (1 | participant)
    ##    Data: brutos
    ## 
    ## REML criterion at convergence: -50344.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.7283 -0.5764  0.0699  0.6572  4.3999 
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.003013 0.05489 
    ##  Residual                0.005708 0.07555 
    ## Number of obs: 21758, groups:  participant, 50
    ## 
    ## Fixed effects:
    ##                                   Estimate Std. Error t value
    ## (Intercept)                       0.424001   0.007834  54.123
    ## prime_valpositive                -0.007570   0.001460  -5.186
    ## phaseovulation                    0.010763   0.001475   7.298
    ## prime_valpositive:phaseovulation -0.003425   0.002051  -1.670
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) prm_vl phsvlt
    ## prim_vlpstv -0.096              
    ## phaseovultn -0.095  0.510       
    ## prm_vlpstv:  0.068 -0.712 -0.716

``` r
brutos %>% group_by(phase, prime_val) %>% summarize(n())  
```

    ## `summarise()` has grouped output by 'phase'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 4 × 3
    ## # Groups:   phase [2]
    ##   phase        prime_val `n()`
    ##   <chr>        <chr>     <int>
    ## 1 menstruation negative   5220
    ## 2 menstruation positive   5518
    ## 3 ovulation    negative   5362
    ## 4 ovulation    positive   5658

``` r
observed_table <- matrix(c(5220,5518,5362,5658), nrow = 2, ncol = 2, byrow = F)
rownames(observed_table) <- c('negative',"positive")
colnames(observed_table) <- c('menstruation', 'ovulation')
observed_table
```

    ##          menstruation ovulation
    ## negative         5220      5362
    ## positive         5518      5658

``` r
X <- chisq.test(observed_table)
X
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  observed_table
    ## X-squared = 0.0027265, df = 1, p-value = 0.9584
