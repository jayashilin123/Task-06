Car price model Linear regression
================

Predict the price of the car model with Linear regression

library(stringr)

library(MASS)

library(car)

Load the data

``` r
load("rda/carprice.rda")
str(carprice)
```

    ## 'data.frame':    205 obs. of  26 variables:
    ##  $ car_ID          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ symboling       : int  3 3 1 2 2 2 1 1 1 0 ...
    ##  $ CarName         : chr  "alfa-romero giulia" "alfa-romero stelvio" "alfa-romero Quadrifoglio" "audi 100 ls" ...
    ##  $ fueltype        : chr  "gas" "gas" "gas" "gas" ...
    ##  $ aspiration      : chr  "std" "std" "std" "std" ...
    ##  $ doornumber      : chr  "two" "two" "two" "four" ...
    ##  $ carbody         : chr  "convertible" "convertible" "hatchback" "sedan" ...
    ##  $ drivewheel      : chr  "rwd" "rwd" "rwd" "fwd" ...
    ##  $ enginelocation  : chr  "front" "front" "front" "front" ...
    ##  $ wheelbase       : num  88.6 88.6 94.5 99.8 99.4 ...
    ##  $ carlength       : num  169 169 171 177 177 ...
    ##  $ carwidth        : num  64.1 64.1 65.5 66.2 66.4 66.3 71.4 71.4 71.4 67.9 ...
    ##  $ carheight       : num  48.8 48.8 52.4 54.3 54.3 53.1 55.7 55.7 55.9 52 ...
    ##  $ curbweight      : int  2548 2548 2823 2337 2824 2507 2844 2954 3086 3053 ...
    ##  $ enginetype      : chr  "dohc" "dohc" "ohcv" "ohc" ...
    ##  $ cylindernumber  : chr  "four" "four" "six" "four" ...
    ##  $ enginesize      : int  130 130 152 109 136 136 136 136 131 131 ...
    ##  $ fuelsystem      : chr  "mpfi" "mpfi" "mpfi" "mpfi" ...
    ##  $ boreratio       : num  3.47 3.47 2.68 3.19 3.19 3.19 3.19 3.19 3.13 3.13 ...
    ##  $ stroke          : num  2.68 2.68 3.47 3.4 3.4 3.4 3.4 3.4 3.4 3.4 ...
    ##  $ compressionratio: num  9 9 9 10 8 8.5 8.5 8.5 8.3 7 ...
    ##  $ horsepower      : int  111 111 154 102 115 110 110 110 140 160 ...
    ##  $ peakrpm         : int  5000 5000 5000 5500 5500 5500 5500 5500 5500 5500 ...
    ##  $ citympg         : int  21 21 19 24 18 19 19 19 17 16 ...
    ##  $ highwaympg      : int  27 27 26 30 22 25 25 25 20 22 ...
    ##  $ price           : num  13495 16500 16500 13950 17450 ...

Convert categorical variables to factors

``` r
carprice$symboling <-as.factor(carprice$symboling)
carprice$cylindernumber<-as.factor(carprice$cylindernumber)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$fuelsystem<-as.factor(carprice$fuelsystem)
carprice$fueltype<-as.factor(carprice$fueltype)
carprice$aspiration<-as.factor(carprice$aspiration)
carprice$doornumber<-as.factor(carprice$doornumber)
carprice$carbody <-as.factor(carprice$carbody)
carprice$drivewheel<-as.factor(carprice$drivewheel)
carprice$enginelocation <- as.factor(carprice$enginelocation)
```

Working on variable "carprice$CarName"

``` r
summary(as.factor(carprice$CarName))
```

    ##                     peugeot 504                  toyota corolla 
    ##                               6                               6 
    ##                   toyota corona                       subaru dl 
    ##                               6                               4 
    ##                     honda civic                       mazda 626 
    ##                               3                               3 
    ##                   mitsubishi g4            mitsubishi mirage g4 
    ##                               3                               3 
    ##            mitsubishi outlander                  toyota mark ii 
    ##                               3                               3 
    ##                      audi 100ls                        bmw 320i 
    ##                               2                               2 
    ##                          bmw x3                    honda accord 
    ##                               2                               2 
    ##                honda civic cvcc                    isuzu D-Max  
    ##                               2                               2 
    ##                       mazda glc                mazda glc deluxe 
    ##                               2                               2 
    ##                      mazda rx-4                   mazda rx-7 gs 
    ##                               2                               2 
    ##                  nissan clipper                    nissan latio 
    ##                               2                               2 
    ##                    nissan rogue                   peugeot 604sl 
    ##                               2                               2 
    ##               plymouth fury iii                 porsche cayenne 
    ##                               2                               2 
    ##                        saab 99e                      saab 99gle 
    ##                               2                               2 
    ##                       saab 99le                          subaru 
    ##                               2                               2 
    ##             toyota corolla 1200         toyota corolla liftback 
    ##                               2                               2 
    ##                  toyota starlet               volkswagen dasher 
    ##                               2                               2 
    ##                     volvo 144ea                 volvo 145e (sw) 
    ##                               2                               2 
    ##                     volvo 244dl                     volvo 264gl 
    ##                               2                               2 
    ##              alfa-romero giulia        alfa-romero Quadrifoglio 
    ##                               1                               1 
    ##             alfa-romero stelvio                     audi 100 ls 
    ##                               1                               1 
    ##                       audi 4000                       audi 5000 
    ##                               1                               1 
    ##             audi 5000s (diesel)                        audi fox 
    ##                               1                               1 
    ##                          bmw x1                          bmw x4 
    ##                               1                               1 
    ##                          bmw x5                          bmw z4 
    ##                               1                               1 
    ##                   buick century        buick century luxus (sw) 
    ##                               1                               1 
    ##           buick century special        buick electra 225 custom 
    ##                               1                               1 
    ##         buick opel isuzu deluxe buick regal sport coupe (turbo) 
    ##                               1                               1 
    ##                   buick skyhawk                   buick skylark 
    ##                               1                               1 
    ##                chevrolet impala           chevrolet monte carlo 
    ##                               1                               1 
    ##             chevrolet vega 2300             dodge challenger se 
    ##                               1                               1 
    ##                 dodge colt (sw)              dodge colt hardtop 
    ##                               1                               1 
    ##            dodge coronet custom       dodge coronet custom (sw) 
    ##                               1                               1 
    ##                      dodge d200               dodge dart custom 
    ##                               1                               1 
    ##               dodge monaco (sw)                   dodge rampage 
    ##                               1                               1 
    ##               honda accord cvcc                 honda accord lx 
    ##                               1                               1 
    ##              honda civic (auto)                honda civic 1300 
    ##                               1                               1 
    ##             honda civic 1500 gl                   honda prelude 
    ##                               1                               1 
    ##             isuzu D-Max V-Cross                      isuzu MU-X 
    ##                               1                               1 
    ##                       jaguar xf                       jaguar xj 
    ##                               1                               1 
    ##                       jaguar xk                maxda glc deluxe 
    ##                               1                               1 
    ##                       maxda rx3                     mazda glc 4 
    ##                               1                               1 
    ##                mazda glc custom              mazda glc custom l 
    ##                               1                               1 
    ##                 mazda rx2 coupe                  mercury cougar 
    ##                               1                               1 
    ##               mitsubishi lancer               mitsubishi mirage 
    ##                               1                               1 
    ##              mitsubishi montero               mitsubishi pajero 
    ##                               1                               1 
    ##                     nissan dayz                     nissan fuga 
    ##                               1                               1 
    ##                     nissan gt-r                     nissan juke 
    ##                               1                               1 
    ##                    nissan kicks                     nissan leaf 
    ##                               1                               1 
    ##                     nissan note                         (Other) 
    ##                               1                              48

There are multiple levels in CarName. Reduce the variables by taking only the carCompany

``` r
carprice$carCompany <-gsub("\\ .*", "", carprice$CarName)
str(carprice$carCompany)
```

    ##  chr [1:205] "alfa-romero" "alfa-romero" "alfa-romero" "audi" "audi" ...

``` r
carprice$carCompany <- as.factor(carprice$carCompany)
summary(carprice$carCompany)
```

    ## alfa-romero        audi         bmw       buick   chevrolet       dodge 
    ##           3           7           8           8           3           9 
    ##       honda       isuzu      jaguar       maxda       mazda     mercury 
    ##          13           4           3           2          15           1 
    ##  mitsubishi      nissan      Nissan     peugeot    plymouth    porcshce 
    ##          13          17           1          11           7           1 
    ##     porsche     renault        saab      subaru      toyota     toyouta 
    ##           4           2           6          12          31           1 
    ##   vokswagen  volkswagen       volvo          vw 
    ##           1           9          11           2

``` r
levels(carprice$carCompany)
```

    ##  [1] "alfa-romero" "audi"        "bmw"         "buick"       "chevrolet"  
    ##  [6] "dodge"       "honda"       "isuzu"       "jaguar"      "maxda"      
    ## [11] "mazda"       "mercury"     "mitsubishi"  "nissan"      "Nissan"     
    ## [16] "peugeot"     "plymouth"    "porcshce"    "porsche"     "renault"    
    ## [21] "saab"        "subaru"      "toyota"      "toyouta"     "vokswagen"  
    ## [26] "volkswagen"  "volvo"       "vw"

``` r
levels(carprice$carCompany)[10] <- "mazda"
levels(carprice$carCompany)[14] <- "nissan"
levels(carprice$carCompany)[16] <- "porsche"
levels(carprice$carCompany)[21] <- "toyota"
levels(carprice$carCompany)[21] <- "volkswagen"
levels(carprice$carCompany)[23] <- "volkswagen"
levels(carprice$carCompany)
```

    ##  [1] "alfa-romero" "audi"        "bmw"         "buick"       "chevrolet"  
    ##  [6] "dodge"       "honda"       "isuzu"       "jaguar"      "mazda"      
    ## [11] "mercury"     "mitsubishi"  "nissan"      "peugeot"     "plymouth"   
    ## [16] "porsche"     "renault"     "saab"        "subaru"      "toyota"     
    ## [21] "volkswagen"  "volvo"

Check for missing values

``` r
sum(is.na(carprice))
```

    ## [1] 0

Check for duplicated data

``` r
which(duplicated(carprice))
```

    ## integer(0)

Create the dummy variables

``` r
# For carCompany
dummy_1 <- data.frame(model.matrix( ~carCompany, data = carprice))
dummy_1<-dummy_1[,-1]

# For carbody
dummy_2 <- data.frame(model.matrix( ~carbody, data = carprice))
dummy_2<-dummy_2[,-1]

# Drivewheel 
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carprice))
dummy_3<-dummy_3[,-1]

#Engine type
dummy_4 <- data.frame(model.matrix( ~enginetype, data = carprice))
dummy_4<-dummy_4[,-1]

#cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = carprice))
dummy_5<-dummy_5[,-1]

# Fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = carprice))
dummy_6<-dummy_6[,-1]

# Symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = carprice))
dummy_7<-dummy_7[,-1]
```

Variable having 2 levels are replaced to 0&1 and converted to numeric

``` r
# for fueltype
levels(carprice$fueltype)<-c(1,0)
# assigning 1 to diesel and 0 to gas
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

# for aspiration
levels(carprice$aspiration)<-c(1,0)
# Assigning 1 to "std" and 0 to "turbo"
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

# For doornumber
levels(carprice$doornumber)<-c(1,0)
# Assigning 1 if the number of doors is 4, and 0 if the number of doors is 2.
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

# Enginelocation
levels(carprice$enginelocation)<-c(1,0)
# Assigning 1 if the engine is front and 0 if in rear
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]
```

Combine the dummy variables and the numeric columns of carprice dataset

``` r
carprice_1 <- cbind(carprice[ , c(1,4:6,9:14,17,19:26)], dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6, dummy_7)
```

head(carprice\_1)

Modeling

``` r
# Divide you data in 70:30 and create test and train datasets

set.seed(100)
indices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))

train=carprice_1[indices,]
test = carprice_1[-indices,]
```

``` r
model_1 <-lm(price~.,data=train[,-1])
summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = price ~ ., data = train[, -1])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2951.16  -708.57    10.27   719.33  3038.24 
    ## 
    ## Coefficients: (9 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.733e+04  1.585e+04  -2.355 0.020866 *  
    ## fueltype              5.414e+03  7.339e+03   0.738 0.462780    
    ## aspiration           -3.318e+03  9.167e+02  -3.619 0.000507 ***
    ## doornumber            1.790e+02  5.336e+02   0.336 0.738084    
    ## enginelocation       -7.450e+03  4.544e+03  -1.640 0.104854    
    ## wheelbase            -2.704e+01  1.171e+02  -0.231 0.817972    
    ## carlength             3.767e+01  6.080e+01   0.620 0.537241    
    ## carwidth              6.603e+02  2.448e+02   2.697 0.008464 ** 
    ## carheight             2.965e+01  1.605e+02   0.185 0.853912    
    ## curbweight            3.691e+00  1.713e+00   2.155 0.034097 *  
    ## enginesize            1.066e+02  2.753e+01   3.873 0.000214 ***
    ## boreratio            -1.073e+03  1.954e+03  -0.549 0.584162    
    ## stroke               -2.310e+03  1.112e+03  -2.077 0.040938 *  
    ## compressionratio     -3.947e+02  5.434e+02  -0.726 0.469719    
    ## horsepower           -2.296e+01  2.534e+01  -0.906 0.367388    
    ## peakrpm               1.760e+00  8.673e-01   2.029 0.045691 *  
    ## citympg               6.859e+01  1.389e+02   0.494 0.622821    
    ## highwaympg            1.768e+01  1.256e+02   0.141 0.888342    
    ## carCompanyaudi       -6.043e+02  2.216e+03  -0.273 0.785736    
    ## carCompanybmw         6.609e+03  2.636e+03   2.507 0.014126 *  
    ## carCompanybuick       4.135e+03  2.512e+03   1.646 0.103463    
    ## carCompanychevrolet  -2.068e+02  5.060e+03  -0.041 0.967495    
    ## carCompanydodge      -5.515e+03  2.187e+03  -2.522 0.013575 *  
    ## carCompanyhonda      -2.104e+03  2.141e+03  -0.983 0.328660    
    ## carCompanyisuzu      -1.886e+03  2.426e+03  -0.777 0.439099    
    ## carCompanyjaguar      1.907e+03  2.850e+03   0.669 0.505306    
    ## carCompanymazda      -4.602e+03  1.798e+03  -2.560 0.012277 *  
    ## carCompanymercury    -3.024e+03  2.924e+03  -1.034 0.303982    
    ## carCompanymitsubishi -6.093e+03  2.031e+03  -3.001 0.003558 ** 
    ## carCompanynissan     -3.928e+03  1.827e+03  -2.150 0.034457 *  
    ## carCompanypeugeot    -5.380e+03  2.519e+03  -2.136 0.035628 *  
    ## carCompanyplymouth   -4.957e+03  2.041e+03  -2.429 0.017302 *  
    ## carCompanyporsche     5.574e+03  5.545e+03   1.005 0.317734    
    ## carCompanyrenault    -5.789e+03  2.258e+03  -2.563 0.012170 *  
    ## carCompanysaab       -3.090e+03  2.227e+03  -1.387 0.169029    
    ## carCompanysubaru     -6.656e+03  2.138e+03  -3.113 0.002538 ** 
    ## carCompanytoyota     -3.537e+03  1.668e+03  -2.120 0.036988 *  
    ## carCompanyvolkswagen -3.698e+03  1.851e+03  -1.998 0.049040 *  
    ## carCompanyvolvo      -1.313e+03  2.514e+03  -0.522 0.602875    
    ## carbodyhardtop       -2.198e+03  1.320e+03  -1.665 0.099739 .  
    ## carbodyhatchback     -2.622e+03  1.250e+03  -2.097 0.039043 *  
    ## carbodysedan         -2.756e+03  1.336e+03  -2.063 0.042208 *  
    ## carbodywagon         -3.273e+03  1.486e+03  -2.202 0.030454 *  
    ## drivewheelfwd         3.192e+02  1.048e+03   0.305 0.761389    
    ## drivewheelrwd        -1.881e+03  1.416e+03  -1.328 0.187654    
    ## enginetypedohcv              NA         NA      NA       NA    
    ## enginetypel                  NA         NA      NA       NA    
    ## enginetypeohc        -1.214e+03  1.354e+03  -0.897 0.372375    
    ## enginetypeohcf               NA         NA      NA       NA    
    ## enginetypeohcv       -7.004e+02  1.324e+03  -0.529 0.598286    
    ## enginetyperotor       1.064e+04  5.069e+03   2.098 0.038904 *  
    ## cylindernumberfive   -1.294e+03  3.376e+03  -0.383 0.702564    
    ## cylindernumberfour    4.432e+02  3.959e+03   0.112 0.911142    
    ## cylindernumbersix     4.432e+02  2.928e+03   0.151 0.880056    
    ## cylindernumberthree          NA         NA      NA       NA    
    ## cylindernumbertwelve         NA         NA      NA       NA    
    ## cylindernumbertwo            NA         NA      NA       NA    
    ## fuelsystem2bbl        1.793e+03  1.334e+03   1.344 0.182552    
    ## fuelsystem4bbl               NA         NA      NA       NA    
    ## fuelsystemidi                NA         NA      NA       NA    
    ## fuelsystemmfi        -2.391e+02  2.473e+03  -0.097 0.923213    
    ## fuelsystemmpfi        1.624e+03  1.420e+03   1.143 0.256217    
    ## fuelsystemspdi        7.241e+02  1.692e+03   0.428 0.669868    
    ## fuelsystemspfi               NA         NA      NA       NA    
    ## symboling.1          -9.376e+02  1.811e+03  -0.518 0.606109    
    ## symboling0           -9.746e+02  2.043e+03  -0.477 0.634615    
    ## symboling1           -2.648e+02  2.127e+03  -0.124 0.901252    
    ## symboling2           -5.110e+02  2.237e+03  -0.228 0.819847    
    ## symboling3           -1.195e+03  2.214e+03  -0.540 0.590981    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1436 on 83 degrees of freedom
    ## Multiple R-squared:  0.9819, Adjusted R-squared:  0.9691 
    ## F-statistic: 76.48 on 59 and 83 DF,  p-value: < 2.2e-16

Applying stepwise approach with step &lt;- stepAIC(model\_1, direction="both")

``` r
step
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carlength + 
    ##     carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
    ##     carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymercury + 
    ##     carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
    ##     carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Coefficients:
    ##          (Intercept)            aspiration        enginelocation  
    ##           -33403.736             -2925.410             -9358.366  
    ##            carlength              carwidth            curbweight  
    ##               42.692               582.711                 3.160  
    ##           enginesize                stroke               peakrpm  
    ##               88.826             -2616.977                 1.245  
    ##              citympg         carCompanybmw       carCompanybuick  
    ##               64.759              7982.278              5641.473  
    ##      carCompanydodge       carCompanyhonda      carCompanyjaguar  
    ##            -4340.035             -1370.777              4514.841  
    ##      carCompanymazda     carCompanymercury  carCompanymitsubishi  
    ##            -3463.008             -2615.729             -4795.527  
    ##     carCompanynissan     carCompanypeugeot    carCompanyplymouth  
    ##            -2946.466             -3776.672             -3497.563  
    ##    carCompanyporsche     carCompanyrenault        carCompanysaab  
    ##             3716.283             -4410.374             -2823.918  
    ##     carCompanysubaru      carCompanytoyota  carCompanyvolkswagen  
    ##            -6542.488             -2505.288             -2840.989  
    ##       carbodyhardtop      carbodyhatchback          carbodysedan  
    ##            -2762.398             -3026.601             -3061.322  
    ##         carbodywagon         drivewheelrwd         enginetypeohc  
    ##            -3442.526             -2765.527             -1232.796  
    ##      enginetyperotor    cylindernumberfive        fuelsystem2bbl  
    ##             8717.263             -1071.502              1006.425  
    ##       fuelsystemmpfi           symboling.1            symboling0  
    ##              741.423              -851.155              -660.364  
    ##           symboling3  
    ##             -927.210

``` r
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_2)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carlength + 
    ##     carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
    ##     carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymercury + 
    ##     carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
    ##     carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2998.1  -733.7     0.0   640.7  3373.7 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.340e+04  8.952e+03  -3.732 0.000312 ***
    ## aspiration           -2.925e+03  5.153e+02  -5.677 1.27e-07 ***
    ## enginelocation       -9.358e+03  2.069e+03  -4.523 1.63e-05 ***
    ## carlength             4.269e+01  3.475e+01   1.229 0.222052    
    ## carwidth              5.827e+02  1.803e+02   3.232 0.001650 ** 
    ## curbweight            3.160e+00  1.131e+00   2.794 0.006208 ** 
    ## enginesize            8.883e+01  1.323e+01   6.715 1.05e-09 ***
    ## stroke               -2.617e+03  7.484e+02  -3.497 0.000696 ***
    ## peakrpm               1.245e+00  5.334e-01   2.334 0.021516 *  
    ## citympg               6.476e+01  4.795e+01   1.351 0.179809    
    ## carCompanybmw         7.982e+03  9.895e+02   8.067 1.39e-12 ***
    ## carCompanybuick       5.641e+03  1.050e+03   5.375 4.80e-07 ***
    ## carCompanydodge      -4.340e+03  9.625e+02  -4.509 1.73e-05 ***
    ## carCompanyhonda      -1.371e+03  9.174e+02  -1.494 0.138197    
    ## carCompanyjaguar      4.515e+03  1.437e+03   3.141 0.002198 ** 
    ## carCompanymazda      -3.463e+03  7.198e+02  -4.811 5.16e-06 ***
    ## carCompanymercury    -2.616e+03  1.472e+03  -1.777 0.078511 .  
    ## carCompanymitsubishi -4.796e+03  8.240e+02  -5.820 6.71e-08 ***
    ## carCompanynissan     -2.946e+03  6.492e+02  -4.538 1.54e-05 ***
    ## carCompanypeugeot    -3.777e+03  1.153e+03  -3.277 0.001431 ** 
    ## carCompanyplymouth   -3.498e+03  8.477e+02  -4.126 7.50e-05 ***
    ## carCompanyporsche     3.716e+03  1.720e+03   2.161 0.033001 *  
    ## carCompanyrenault    -4.410e+03  1.250e+03  -3.530 0.000624 ***
    ## carCompanysaab       -2.824e+03  1.006e+03  -2.808 0.005966 ** 
    ## carCompanysubaru     -6.542e+03  1.003e+03  -6.523 2.62e-09 ***
    ## carCompanytoyota     -2.505e+03  5.230e+02  -4.790 5.61e-06 ***
    ## carCompanyvolkswagen -2.841e+03  7.017e+02  -4.049 9.99e-05 ***
    ## carbodyhardtop       -2.762e+03  1.017e+03  -2.716 0.007754 ** 
    ## carbodyhatchback     -3.027e+03  9.248e+02  -3.273 0.001450 ** 
    ## carbodysedan         -3.061e+03  1.004e+03  -3.048 0.002925 ** 
    ## carbodywagon         -3.443e+03  1.072e+03  -3.212 0.001761 ** 
    ## drivewheelrwd        -2.766e+03  5.529e+02  -5.002 2.34e-06 ***
    ## enginetypeohc        -1.233e+03  6.028e+02  -2.045 0.043390 *  
    ## enginetyperotor       8.717e+03  1.622e+03   5.373 4.83e-07 ***
    ## cylindernumberfive   -1.072e+03  7.366e+02  -1.455 0.148805    
    ## fuelsystem2bbl        1.006e+03  5.521e+02   1.823 0.071227 .  
    ## fuelsystemmpfi        7.414e+02  6.127e+02   1.210 0.228988    
    ## symboling.1          -8.512e+02  5.374e+02  -1.584 0.116328    
    ## symboling0           -6.604e+02  3.962e+02  -1.667 0.098641 .  
    ## symboling3           -9.272e+02  5.943e+02  -1.560 0.121790    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1321 on 103 degrees of freedom
    ## Multiple R-squared:  0.981,  Adjusted R-squared:  0.9739 
    ## F-statistic: 136.6 on 39 and 103 DF,  p-value: < 2.2e-16

``` r
vif(model_2)
```

    ##           aspiration       enginelocation            carlength 
    ##             3.333014             7.203632            15.066604 
    ##             carwidth           curbweight           enginesize 
    ##            12.633480            28.955910            24.853602 
    ##               stroke              peakrpm              citympg 
    ##             4.434695             5.465165             8.296221 
    ##        carCompanybmw      carCompanybuick      carCompanydodge 
    ##             1.647827             4.202912             2.064227 
    ##      carCompanyhonda     carCompanyjaguar      carCompanymazda 
    ##             5.302090             2.334633             3.264087 
    ##    carCompanymercury carCompanymitsubishi     carCompanynissan 
    ##             1.232863             2.938211             2.854363 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyporsche 
    ##             4.375441             2.366835             6.588183 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.764361             2.796616             3.313775 
    ##     carCompanytoyota carCompanyvolkswagen       carbodyhardtop 
    ##             3.433252             2.379464             4.477460 
    ##     carbodyhatchback         carbodysedan         carbodywagon 
    ##            16.080081            20.215983            10.847212 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             5.842352             5.905741             4.429404 
    ##   cylindernumberfive       fuelsystem2bbl       fuelsystemmpfi 
    ##             2.622136             5.450440             7.553806 
    ##          symboling.1           symboling0           symboling3 
    ##             2.352004             2.668943             3.334540

carlength has a high VIF and is insignificant. Removing carlength

``` r
model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_3)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + citympg + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyporsche + carCompanyrenault + carCompanysaab + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2778.8  -750.5     0.0   713.1  3373.8 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.299e+04  8.967e+03  -3.679 0.000373 ***
    ## aspiration           -2.822e+03  5.096e+02  -5.537 2.32e-07 ***
    ## enginelocation       -9.915e+03  2.024e+03  -4.899 3.55e-06 ***
    ## carwidth              6.703e+02  1.660e+02   4.039 0.000103 ***
    ## curbweight            3.505e+00  1.098e+00   3.191 0.001874 ** 
    ## enginesize            8.976e+01  1.324e+01   6.781 7.45e-10 ***
    ## stroke               -2.530e+03  7.468e+02  -3.387 0.000998 ***
    ## peakrpm               1.283e+00  5.338e-01   2.403 0.018014 *  
    ## citympg               5.680e+01  4.763e+01   1.193 0.235730    
    ## carCompanybmw         7.791e+03  9.795e+02   7.954 2.32e-12 ***
    ## carCompanybuick       5.523e+03  1.048e+03   5.271 7.38e-07 ***
    ## carCompanydodge      -4.438e+03  9.616e+02  -4.615 1.13e-05 ***
    ## carCompanyhonda      -1.676e+03  8.853e+02  -1.893 0.061077 .  
    ## carCompanyjaguar      4.287e+03  1.429e+03   3.000 0.003375 ** 
    ## carCompanymazda      -3.460e+03  7.216e+02  -4.795 5.44e-06 ***
    ## carCompanymercury    -2.656e+03  1.475e+03  -1.800 0.074701 .  
    ## carCompanymitsubishi -4.877e+03  8.233e+02  -5.924 4.12e-08 ***
    ## carCompanynissan     -3.015e+03  6.484e+02  -4.651 9.76e-06 ***
    ## carCompanypeugeot    -3.602e+03  1.146e+03  -3.141 0.002190 ** 
    ## carCompanyplymouth   -3.604e+03  8.453e+02  -4.263 4.44e-05 ***
    ## carCompanyporsche     2.952e+03  1.607e+03   1.837 0.069072 .  
    ## carCompanyrenault    -4.370e+03  1.252e+03  -3.490 0.000709 ***
    ## carCompanysaab       -2.339e+03  9.273e+02  -2.522 0.013168 *  
    ## carCompanysubaru     -6.485e+03  1.004e+03  -6.457 3.48e-09 ***
    ## carCompanytoyota     -2.516e+03  5.242e+02  -4.800 5.34e-06 ***
    ## carCompanyvolkswagen -2.833e+03  7.034e+02  -4.027 0.000107 ***
    ## carbodyhardtop       -2.528e+03  1.001e+03  -2.524 0.013117 *  
    ## carbodyhatchback     -2.875e+03  9.188e+02  -3.129 0.002273 ** 
    ## carbodysedan         -2.668e+03  9.541e+02  -2.796 0.006165 ** 
    ## carbodywagon         -3.016e+03  1.016e+03  -2.967 0.003734 ** 
    ## drivewheelrwd        -2.648e+03  5.458e+02  -4.851 4.33e-06 ***
    ## enginetypeohc        -1.102e+03  5.947e+02  -1.853 0.066779 .  
    ## enginetyperotor       8.669e+03  1.626e+03   5.332 5.69e-07 ***
    ## cylindernumberfive   -1.197e+03  7.313e+02  -1.637 0.104646    
    ## fuelsystem2bbl        1.031e+03  5.531e+02   1.863 0.065244 .  
    ## fuelsystemmpfi        7.403e+02  6.142e+02   1.205 0.230768    
    ## symboling.1          -7.696e+02  5.346e+02  -1.439 0.153020    
    ## symboling0           -5.458e+02  3.861e+02  -1.414 0.160391    
    ## symboling3           -9.130e+02  5.956e+02  -1.533 0.128365    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1324 on 104 degrees of freedom
    ## Multiple R-squared:  0.9808, Adjusted R-squared:  0.9737 
    ## F-statistic: 139.5 on 38 and 104 DF,  p-value: < 2.2e-16

``` r
vif(model_3)
```

    ##           aspiration       enginelocation             carwidth 
    ##             3.243909             6.858677            10.656945 
    ##           curbweight           enginesize               stroke 
    ##            27.172615            24.771252             4.394755 
    ##              peakrpm              citympg        carCompanybmw 
    ##             5.446973             8.144869             1.606823 
    ##      carCompanybuick      carCompanydodge      carCompanyhonda 
    ##             4.167609             2.050091             4.912713 
    ##     carCompanyjaguar      carCompanymazda    carCompanymercury 
    ##             2.295814             3.264051             1.232255 
    ## carCompanymitsubishi     carCompanynissan    carCompanypeugeot 
    ##             2.919231             2.833047             4.308601 
    ##   carCompanyplymouth    carCompanyporsche    carCompanyrenault 
    ##             2.342227             5.726112             1.763154 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             2.366164             3.306669             3.432272 
    ## carCompanyvolkswagen       carbodyhardtop     carbodyhatchback 
    ##             2.379248             4.319494            15.794827 
    ##         carbodysedan         carbodywagon        drivewheelrwd 
    ##            18.156918             9.707562             5.667049 
    ##        enginetypeohc      enginetyperotor   cylindernumberfive 
    ##             5.720919             4.426776             2.571630 
    ##       fuelsystem2bbl       fuelsystemmpfi          symboling.1 
    ##             5.443511             7.553791             2.316114 
    ##           symboling0           symboling3 
    ##             2.521217             3.333276

Remove citympg

``` r
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_4)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyporsche + carCompanyrenault + carCompanysaab + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2676.1  -771.3     0.0   685.8  3450.7 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.096e+04  8.822e+03  -3.510 0.000662 ***
    ## aspiration           -2.604e+03  4.768e+02  -5.462 3.18e-07 ***
    ## enginelocation       -1.017e+04  2.016e+03  -5.045 1.91e-06 ***
    ## carwidth              7.214e+02  1.607e+02   4.490 1.83e-05 ***
    ## curbweight            3.029e+00  1.025e+00   2.954 0.003869 ** 
    ## enginesize            8.372e+01  1.225e+01   6.832 5.64e-10 ***
    ## stroke               -2.526e+03  7.483e+02  -3.375 0.001036 ** 
    ## peakrpm               9.937e-01  4.765e-01   2.085 0.039450 *  
    ## carCompanybmw         7.815e+03  9.812e+02   7.964 2.10e-12 ***
    ## carCompanybuick       5.796e+03  1.025e+03   5.656 1.35e-07 ***
    ## carCompanydodge      -4.384e+03  9.625e+02  -4.555 1.42e-05 ***
    ## carCompanyhonda      -1.749e+03  8.849e+02  -1.977 0.050707 .  
    ## carCompanyjaguar      4.685e+03  1.392e+03   3.365 0.001070 ** 
    ## carCompanymazda      -3.594e+03  7.143e+02  -5.031 2.03e-06 ***
    ## carCompanymercury    -2.694e+03  1.478e+03  -1.823 0.071099 .  
    ## carCompanymitsubishi -4.938e+03  8.234e+02  -5.997 2.89e-08 ***
    ## carCompanynissan     -2.967e+03  6.484e+02  -4.576 1.31e-05 ***
    ## carCompanypeugeot    -3.935e+03  1.114e+03  -3.531 0.000615 ***
    ## carCompanyplymouth   -3.612e+03  8.470e+02  -4.265 4.39e-05 ***
    ## carCompanyporsche     2.926e+03  1.610e+03   1.817 0.071997 .  
    ## carCompanyrenault    -4.484e+03  1.251e+03  -3.584 0.000514 ***
    ## carCompanysaab       -2.433e+03  9.258e+02  -2.628 0.009863 ** 
    ## carCompanysubaru     -6.836e+03  9.624e+02  -7.103 1.51e-10 ***
    ## carCompanytoyota     -2.595e+03  5.211e+02  -4.979 2.52e-06 ***
    ## carCompanyvolkswagen -2.812e+03  7.046e+02  -3.992 0.000122 ***
    ## carbodyhardtop       -2.451e+03  1.001e+03  -2.447 0.016058 *  
    ## carbodyhatchback     -2.781e+03  9.172e+02  -3.032 0.003060 ** 
    ## carbodysedan         -2.602e+03  9.544e+02  -2.726 0.007508 ** 
    ## carbodywagon         -2.938e+03  1.016e+03  -2.890 0.004676 ** 
    ## drivewheelrwd        -2.606e+03  5.458e+02  -4.774 5.86e-06 ***
    ## enginetypeohc        -1.353e+03  5.573e+02  -2.428 0.016903 *  
    ## enginetyperotor       7.699e+03  1.411e+03   5.458 3.24e-07 ***
    ## cylindernumberfive   -1.347e+03  7.218e+02  -1.866 0.064793 .  
    ## fuelsystem2bbl        7.256e+02  4.914e+02   1.477 0.142787    
    ## fuelsystemmpfi        3.920e+02  5.414e+02   0.724 0.470599    
    ## symboling.1          -7.842e+02  5.356e+02  -1.464 0.146129    
    ## symboling0           -5.442e+02  3.868e+02  -1.407 0.162445    
    ## symboling3           -1.047e+03  5.861e+02  -1.787 0.076881 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1327 on 105 degrees of freedom
    ## Multiple R-squared:  0.9805, Adjusted R-squared:  0.9736 
    ## F-statistic: 142.6 on 37 and 105 DF,  p-value: < 2.2e-16

``` r
vif(model_4)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.828301             6.781210             9.946551 
    ##           curbweight           enginesize               stroke 
    ##            23.582928            21.139757             4.394659 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             4.321957             1.606128             3.969769 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             2.045625             4.889259             2.170787 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             3.185472             1.231662             2.908024 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.821870             4.052975             2.342068 
    ##    carCompanyporsche    carCompanyrenault       carCompanysaab 
    ##             5.725081             1.752894             2.348958 
    ##     carCompanysubaru     carCompanytoyota carCompanyvolkswagen 
    ##             3.023734             3.378125             2.377852 
    ##       carbodyhardtop     carbodyhatchback         carbodysedan 
    ##             4.301497            15.678040            18.096611 
    ##         carbodywagon        drivewheelrwd        enginetypeohc 
    ##             9.667587             5.643284             5.003713 
    ##      enginetyperotor   cylindernumberfive       fuelsystem2bbl 
    ##             3.319013             2.495556             4.280039 
    ##       fuelsystemmpfi          symboling.1           symboling0 
    ##             5.845478             2.314902             2.521185 
    ##           symboling3 
    ##             3.214394

Remove fuelsystemmpfi

``` r
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_5)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyporsche + carCompanyrenault + carCompanysaab + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     fuelsystem2bbl + symboling.1 + symboling0 + symboling3, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2772.2  -808.7    -4.7   650.0  3326.9 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.184e+04  8.719e+03  -3.652 0.000407 ***
    ## aspiration           -2.503e+03  4.547e+02  -5.504 2.60e-07 ***
    ## enginelocation       -1.021e+04  2.011e+03  -5.075 1.66e-06 ***
    ## carwidth              7.280e+02  1.601e+02   4.548 1.45e-05 ***
    ## curbweight            3.110e+00  1.017e+00   3.059 0.002815 ** 
    ## enginesize            8.513e+01  1.207e+01   7.053 1.87e-10 ***
    ## stroke               -2.597e+03  7.401e+02  -3.509 0.000661 ***
    ## peakrpm               1.127e+00  4.384e-01   2.571 0.011543 *  
    ## carCompanybmw         7.813e+03  9.790e+02   7.981 1.84e-12 ***
    ## carCompanybuick       5.505e+03  9.404e+02   5.853 5.45e-08 ***
    ## carCompanydodge      -4.448e+03  9.563e+02  -4.651 9.57e-06 ***
    ## carCompanyhonda      -2.010e+03  8.065e+02  -2.492 0.014238 *  
    ## carCompanyjaguar      4.563e+03  1.379e+03   3.309 0.001279 ** 
    ## carCompanymazda      -3.675e+03  7.038e+02  -5.222 8.90e-07 ***
    ## carCompanymercury    -2.605e+03  1.469e+03  -1.773 0.079132 .  
    ## carCompanymitsubishi -5.091e+03  7.938e+02  -6.414 4.04e-09 ***
    ## carCompanynissan     -3.050e+03  6.366e+02  -4.792 5.40e-06 ***
    ## carCompanypeugeot    -3.995e+03  1.109e+03  -3.604 0.000479 ***
    ## carCompanyplymouth   -3.759e+03  8.205e+02  -4.582 1.26e-05 ***
    ## carCompanyporsche     2.663e+03  1.565e+03   1.702 0.091754 .  
    ## carCompanyrenault    -4.373e+03  1.239e+03  -3.530 0.000616 ***
    ## carCompanysaab       -2.401e+03  9.226e+02  -2.602 0.010588 *  
    ## carCompanysubaru     -6.832e+03  9.602e+02  -7.115 1.38e-10 ***
    ## carCompanytoyota     -2.618e+03  5.189e+02  -5.046 1.88e-06 ***
    ## carCompanyvolkswagen -2.850e+03  7.010e+02  -4.066 9.21e-05 ***
    ## carbodyhardtop       -2.492e+03  9.975e+02  -2.499 0.014003 *  
    ## carbodyhatchback     -2.890e+03  9.027e+02  -3.202 0.001802 ** 
    ## carbodysedan         -2.719e+03  9.385e+02  -2.897 0.004576 ** 
    ## carbodywagon         -3.068e+03  9.983e+02  -3.073 0.002695 ** 
    ## drivewheelrwd        -2.609e+03  5.445e+02  -4.791 5.42e-06 ***
    ## enginetypeohc        -1.349e+03  5.561e+02  -2.427 0.016921 *  
    ## enginetyperotor       7.456e+03  1.367e+03   5.454 3.24e-07 ***
    ## cylindernumberfive   -1.355e+03  7.201e+02  -1.882 0.062622 .  
    ## fuelsystem2bbl        5.534e+02  4.290e+02   1.290 0.199946    
    ## symboling.1          -7.745e+02  5.342e+02  -1.450 0.150040    
    ## symboling0           -5.857e+02  3.817e+02  -1.534 0.127920    
    ## symboling3           -1.090e+03  5.818e+02  -1.873 0.063758 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1324 on 106 degrees of freedom
    ## Multiple R-squared:  0.9804, Adjusted R-squared:  0.9737 
    ## F-statistic: 147.3 on 36 and 106 DF,  p-value: < 2.2e-16

``` r
vif(model_5)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.583427             6.777294             9.915124 
    ##           curbweight           enginesize               stroke 
    ##            23.300688            20.603732             4.317705 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.676210             1.606120             3.359181 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             2.028633             4.079042             2.139191 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             3.106592             1.223000             2.714767 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.732328             4.030093             2.207616 
    ##    carCompanyporsche    carCompanyrenault       carCompanysaab 
    ##             5.433246             1.726646             2.343399 
    ##     carCompanysubaru     carCompanytoyota carCompanyvolkswagen 
    ##             3.023629             3.364822             2.364657 
    ##       carbodyhardtop     carbodyhatchback         carbodysedan 
    ##             4.287219            15.253109            17.577836 
    ##         carbodywagon        drivewheelrwd        enginetypeohc 
    ##             9.367418             5.642890             5.003331 
    ##      enginetyperotor   cylindernumberfive       fuelsystem2bbl 
    ##             3.131250             2.494983             3.276881 
    ##          symboling.1           symboling0           symboling3 
    ##             2.313472             2.465881             3.181658

Remove carcompanyporsche

``` r
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 fuelsystem2bbl + symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_6)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
    ##     carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
    ##     enginetyperotor + cylindernumberfive + fuelsystem2bbl + symboling.1 + 
    ##     symboling0 + symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2760.61  -830.83    -0.83   685.31  3078.30 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.429e+04  8.675e+03  -3.953 0.000139 ***
    ## aspiration           -2.493e+03  4.587e+02  -5.435 3.47e-07 ***
    ## enginelocation       -1.294e+04  1.223e+03 -10.575  < 2e-16 ***
    ## carwidth              8.157e+02  1.529e+02   5.336 5.36e-07 ***
    ## curbweight            3.062e+00  1.026e+00   2.986 0.003500 ** 
    ## enginesize            8.420e+01  1.216e+01   6.922 3.42e-10 ***
    ## stroke               -2.604e+03  7.466e+02  -3.487 0.000709 ***
    ## peakrpm               1.124e+00  4.423e-01   2.542 0.012454 *  
    ## carCompanybmw         7.674e+03  9.842e+02   7.797 4.43e-12 ***
    ## carCompanybuick       5.202e+03  9.317e+02   5.584 1.80e-07 ***
    ## carCompanydodge      -4.470e+03  9.647e+02  -4.634 1.02e-05 ***
    ## carCompanyhonda      -2.066e+03  8.129e+02  -2.542 0.012456 *  
    ## carCompanyjaguar      4.217e+03  1.376e+03   3.065 0.002754 ** 
    ## carCompanymazda      -3.854e+03  7.020e+02  -5.490 2.72e-07 ***
    ## carCompanymercury    -2.941e+03  1.469e+03  -2.003 0.047749 *  
    ## carCompanymitsubishi -5.185e+03  7.988e+02  -6.491 2.73e-09 ***
    ## carCompanynissan     -3.165e+03  6.386e+02  -4.956 2.71e-06 ***
    ## carCompanypeugeot    -4.408e+03  1.091e+03  -4.040 0.000101 ***
    ## carCompanyplymouth   -3.804e+03  8.273e+02  -4.598 1.17e-05 ***
    ## carCompanyrenault    -4.582e+03  1.244e+03  -3.684 0.000362 ***
    ## carCompanysaab       -2.554e+03  9.264e+02  -2.757 0.006869 ** 
    ## carCompanysubaru     -7.069e+03  9.584e+02  -7.376 3.66e-11 ***
    ## carCompanytoyota     -2.713e+03  5.205e+02  -5.212 9.14e-07 ***
    ## carCompanyvolkswagen -2.933e+03  7.055e+02  -4.157 6.51e-05 ***
    ## carbodyhardtop       -2.598e+03  1.004e+03  -2.586 0.011044 *  
    ## carbodyhatchback     -2.924e+03  9.104e+02  -3.212 0.001743 ** 
    ## carbodysedan         -2.835e+03  9.443e+02  -3.002 0.003337 ** 
    ## carbodywagon         -3.148e+03  1.006e+03  -3.129 0.002261 ** 
    ## drivewheelrwd        -2.603e+03  5.493e+02  -4.738 6.67e-06 ***
    ## enginetypeohc        -1.466e+03  5.567e+02  -2.633 0.009728 ** 
    ## enginetyperotor       7.473e+03  1.379e+03   5.419 3.72e-07 ***
    ## cylindernumberfive   -1.494e+03  7.218e+02  -2.070 0.040852 *  
    ## fuelsystem2bbl        6.220e+02  4.309e+02   1.443 0.151827    
    ## symboling.1          -9.305e+02  5.309e+02  -1.753 0.082525 .  
    ## symboling0           -5.895e+02  3.851e+02  -1.531 0.128748    
    ## symboling3           -1.275e+03  5.765e+02  -2.212 0.029082 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1336 on 107 degrees of freedom
    ## Multiple R-squared:  0.9799, Adjusted R-squared:  0.9733 
    ## F-statistic: 148.7 on 35 and 107 DF,  p-value: < 2.2e-16

``` r
vif(model_6)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.583026             2.463765             8.886841 
    ##           curbweight           enginesize               stroke 
    ##            23.282859            20.561763             4.317593 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.676162             1.594854             3.239300 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             2.028246             4.072171             2.092718 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             3.036998             1.200838             2.701605 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.701756             3.837024             2.205325 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.709774             2.321236             2.959799 
    ##     carCompanytoyota carCompanyvolkswagen       carbodyhardtop 
    ##             3.326174             2.353312             4.270744 
    ##     carbodyhatchback         carbodysedan         carbodywagon 
    ##            15.245785            17.485230             9.346620 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             5.642624             4.927904             3.131075 
    ##   cylindernumberfive       fuelsystem2bbl          symboling.1 
    ##             2.462835             3.247930             2.245353 
    ##           symboling0           symboling3 
    ##             2.465796             3.070059

Remove fuelsystem2bbl

``` r
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling.1 + symboling0 + 
                 symboling3, data = train[, -1])
summary(model_7)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
    ##     carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
    ##     enginetyperotor + cylindernumberfive + symboling.1 + symboling0 + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2668.4  -748.2   -27.4   697.1  3280.8 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.195e+04  8.565e+03  -3.730 0.000307 ***
    ## aspiration           -2.280e+03  4.364e+02  -5.224 8.58e-07 ***
    ## enginelocation       -1.308e+04  1.225e+03 -10.671  < 2e-16 ***
    ## carwidth              8.076e+02  1.535e+02   5.260 7.34e-07 ***
    ## curbweight            3.012e+00  1.030e+00   2.925 0.004203 ** 
    ## enginesize            8.169e+01  1.210e+01   6.752 7.60e-10 ***
    ## stroke               -2.783e+03  7.399e+02  -3.761 0.000275 ***
    ## peakrpm               9.781e-01  4.327e-01   2.260 0.025798 *  
    ## carCompanybmw         7.492e+03  9.810e+02   7.637 9.50e-12 ***
    ## carCompanybuick       5.484e+03  9.156e+02   5.989 2.81e-08 ***
    ## carCompanydodge      -4.275e+03  9.598e+02  -4.453 2.07e-05 ***
    ## carCompanyhonda      -2.275e+03  8.039e+02  -2.830 0.005548 ** 
    ## carCompanyjaguar      4.500e+03  1.369e+03   3.288 0.001361 ** 
    ## carCompanymazda      -3.687e+03  6.958e+02  -5.298 6.23e-07 ***
    ## carCompanymercury    -2.907e+03  1.476e+03  -1.970 0.051429 .  
    ## carCompanymitsubishi -4.960e+03  7.873e+02  -6.300 6.60e-09 ***
    ## carCompanynissan     -2.982e+03  6.291e+02  -4.741 6.53e-06 ***
    ## carCompanypeugeot    -4.593e+03  1.089e+03  -4.217 5.16e-05 ***
    ## carCompanyplymouth   -3.574e+03  8.158e+02  -4.381 2.75e-05 ***
    ## carCompanyrenault    -4.746e+03  1.245e+03  -3.813 0.000229 ***
    ## carCompanysaab       -2.745e+03  9.214e+02  -2.979 0.003569 ** 
    ## carCompanysubaru     -7.148e+03  9.616e+02  -7.433 2.65e-11 ***
    ## carCompanytoyota     -2.747e+03  5.226e+02  -5.257 7.46e-07 ***
    ## carCompanyvolkswagen -3.178e+03  6.883e+02  -4.616 1.08e-05 ***
    ## carbodyhardtop       -2.534e+03  1.008e+03  -2.513 0.013459 *  
    ## carbodyhatchback     -2.792e+03  9.103e+02  -3.067 0.002736 ** 
    ## carbodysedan         -2.685e+03  9.433e+02  -2.847 0.005287 ** 
    ## carbodywagon         -2.967e+03  1.003e+03  -2.958 0.003809 ** 
    ## drivewheelrwd        -2.642e+03  5.514e+02  -4.792 5.29e-06 ***
    ## enginetypeohc        -1.456e+03  5.595e+02  -2.603 0.010542 *  
    ## enginetyperotor       7.109e+03  1.362e+03   5.218 8.83e-07 ***
    ## cylindernumberfive   -1.554e+03  7.242e+02  -2.146 0.034109 *  
    ## symboling.1          -9.885e+02  5.320e+02  -1.858 0.065907 .  
    ## symboling0           -5.406e+02  3.855e+02  -1.402 0.163678    
    ## symboling3           -1.299e+03  5.792e+02  -2.243 0.026969 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1342 on 108 degrees of freedom
    ## Multiple R-squared:  0.9795, Adjusted R-squared:  0.973 
    ## F-statistic: 151.5 on 34 and 108 DF,  p-value: < 2.2e-16

``` r
vif(model_7)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.314977             2.448182             8.874970 
    ##           curbweight           enginesize               stroke 
    ##            23.256249            20.140518             4.198321 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.483281             1.568853             3.097531 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.988126             3.943070             2.050213 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             2.953879             1.200526             2.598321 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.595688             3.784267             2.123415 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.695417             2.273612             2.950285 
    ##     carCompanytoyota carCompanyvolkswagen       carbodyhardtop 
    ##             3.319394             2.217698             4.262474 
    ##     carbodyhatchback         carbodysedan         carbodywagon 
    ##            15.090891            17.274618             9.201794 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             5.628406             4.927236             3.026156 
    ##   cylindernumberfive          symboling.1           symboling0 
    ##             2.454669             2.232503             2.446711 
    ##           symboling3 
    ##             3.067615

Remove symboling0

``` r
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling.1 + symboling3, data = train[, -1])
summary(model_8)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
    ##     carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
    ##     enginetyperotor + cylindernumberfive + symboling.1 + symboling3, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2724.5  -820.8     0.0   721.1  3357.7 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.389e+04  8.490e+03  -3.992 0.000119 ***
    ## aspiration           -2.262e+03  4.381e+02  -5.163 1.10e-06 ***
    ## enginelocation       -1.303e+04  1.230e+03 -10.587  < 2e-16 ***
    ## carwidth              8.450e+02  1.519e+02   5.564 1.90e-07 ***
    ## curbweight            2.546e+00  9.792e-01   2.600 0.010605 *  
    ## enginesize            8.223e+01  1.215e+01   6.770 6.77e-10 ***
    ## stroke               -2.675e+03  7.391e+02  -3.619 0.000450 ***
    ## peakrpm               9.940e-01  4.345e-01   2.288 0.024067 *  
    ## carCompanybmw         7.140e+03  9.526e+02   7.496 1.86e-11 ***
    ## carCompanybuick       5.433e+03  9.189e+02   5.912 3.93e-08 ***
    ## carCompanydodge      -4.182e+03  9.618e+02  -4.348 3.10e-05 ***
    ## carCompanyhonda      -2.448e+03  7.980e+02  -3.067 0.002723 ** 
    ## carCompanyjaguar      4.370e+03  1.372e+03   3.186 0.001880 ** 
    ## carCompanymazda      -3.739e+03  6.979e+02  -5.357 4.75e-07 ***
    ## carCompanymercury    -2.835e+03  1.482e+03  -1.914 0.058291 .  
    ## carCompanymitsubishi -4.801e+03  7.826e+02  -6.135 1.40e-08 ***
    ## carCompanynissan     -2.954e+03  6.315e+02  -4.677 8.40e-06 ***
    ## carCompanypeugeot    -4.890e+03  1.073e+03  -4.557 1.36e-05 ***
    ## carCompanyplymouth   -3.517e+03  8.184e+02  -4.297 3.77e-05 ***
    ## carCompanyrenault    -4.865e+03  1.247e+03  -3.901 0.000166 ***
    ## carCompanysaab       -2.379e+03  8.877e+02  -2.681 0.008490 ** 
    ## carCompanysubaru     -7.290e+03  9.605e+02  -7.590 1.15e-11 ***
    ## carCompanytoyota     -2.862e+03  5.184e+02  -5.520 2.32e-07 ***
    ## carCompanyvolkswagen -3.084e+03  6.881e+02  -4.481 1.84e-05 ***
    ## carbodyhardtop       -2.582e+03  1.012e+03  -2.550 0.012146 *  
    ## carbodyhatchback     -2.846e+03  9.135e+02  -3.116 0.002347 ** 
    ## carbodysedan         -2.855e+03  9.397e+02  -3.038 0.002981 ** 
    ## carbodywagon         -3.113e+03  1.002e+03  -3.106 0.002418 ** 
    ## drivewheelrwd        -2.382e+03  5.215e+02  -4.568 1.30e-05 ***
    ## enginetypeohc        -1.522e+03  5.600e+02  -2.717 0.007661 ** 
    ## enginetyperotor       6.951e+03  1.364e+03   5.097 1.46e-06 ***
    ## cylindernumberfive   -1.492e+03  7.260e+02  -2.055 0.042284 *  
    ## symboling.1          -7.093e+02  4.956e+02  -1.431 0.155224    
    ## symboling3           -1.254e+03  5.809e+02  -2.158 0.033102 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1348 on 109 degrees of freedom
    ## Multiple R-squared:  0.9791, Adjusted R-squared:  0.9728 
    ## F-statistic: 154.7 on 33 and 109 DF,  p-value: < 2.2e-16

``` r
vif(model_8)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.313046             2.446024             8.606988 
    ##           curbweight           enginesize               stroke 
    ##            20.833100            20.120423             4.152913 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.480874             1.466216             3.092743 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.978688             3.850808             2.040780 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             2.945464             1.199072             2.544657 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.592912             3.641135             2.118170 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.687500             2.091558             2.917204 
    ##     carCompanytoyota carCompanyvolkswagen       carbodyhardtop 
    ##             3.238072             2.196699             4.257563 
    ##     carbodyhatchback         carbodysedan         carbodywagon 
    ##            15.063349            16.991473             9.103066 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             4.990169             4.893076             3.005619 
    ##   cylindernumberfive          symboling.1           symboling3 
    ##             2.445441             1.919858             3.058100

Remove symboling.1

``` r
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda + carCompanymercury + 
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling3, data = train[, -1])
summary(model_9)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymercury + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
    ##     carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
    ##     enginetyperotor + cylindernumberfive + symboling3, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3013.5  -780.8   -21.0   712.7  3446.5 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.149e+04  8.361e+03  -3.766 0.000268 ***
    ## aspiration           -2.253e+03  4.402e+02  -5.117 1.32e-06 ***
    ## enginelocation       -1.270e+04  1.215e+03 -10.454  < 2e-16 ***
    ## carwidth              7.906e+02  1.477e+02   5.352 4.80e-07 ***
    ## curbweight            2.422e+00  9.799e-01   2.471 0.014996 *  
    ## enginesize            8.607e+01  1.190e+01   7.231 6.74e-11 ***
    ## stroke               -2.720e+03  7.420e+02  -3.666 0.000381 ***
    ## peakrpm               1.130e+00  4.259e-01   2.654 0.009138 ** 
    ## carCompanybmw         7.394e+03  9.405e+02   7.862 2.79e-12 ***
    ## carCompanybuick       5.463e+03  9.231e+02   5.918 3.75e-08 ***
    ## carCompanydodge      -4.339e+03  9.600e+02  -4.520 1.57e-05 ***
    ## carCompanyhonda      -2.448e+03  8.018e+02  -3.053 0.002837 ** 
    ## carCompanyjaguar      4.687e+03  1.360e+03   3.446 0.000806 ***
    ## carCompanymazda      -3.603e+03  6.947e+02  -5.186 9.86e-07 ***
    ## carCompanymercury    -2.490e+03  1.469e+03  -1.695 0.092864 .  
    ## carCompanymitsubishi -4.767e+03  7.860e+02  -6.066 1.89e-08 ***
    ## carCompanynissan     -2.869e+03  6.317e+02  -4.541 1.44e-05 ***
    ## carCompanypeugeot    -4.244e+03  9.781e+02  -4.339 3.19e-05 ***
    ## carCompanyplymouth   -3.593e+03  8.206e+02  -4.379 2.73e-05 ***
    ## carCompanyrenault    -4.674e+03  1.246e+03  -3.752 0.000282 ***
    ## carCompanysaab       -2.258e+03  8.878e+02  -2.544 0.012357 *  
    ## carCompanysubaru     -7.109e+03  9.566e+02  -7.431 2.47e-11 ***
    ## carCompanytoyota     -2.820e+03  5.201e+02  -5.423 3.51e-07 ***
    ## carCompanyvolkswagen -2.914e+03  6.810e+02  -4.279 4.03e-05 ***
    ## carbodyhardtop       -2.444e+03  1.013e+03  -2.414 0.017423 *  
    ## carbodyhatchback     -2.827e+03  9.178e+02  -3.080 0.002613 ** 
    ## carbodysedan         -2.917e+03  9.431e+02  -3.093 0.002508 ** 
    ## carbodywagon         -3.263e+03  1.001e+03  -3.259 0.001490 ** 
    ## drivewheelrwd        -2.536e+03  5.127e+02  -4.947 2.73e-06 ***
    ## enginetypeohc        -1.482e+03  5.619e+02  -2.636 0.009589 ** 
    ## enginetyperotor       7.030e+03  1.369e+03   5.134 1.23e-06 ***
    ## cylindernumberfive   -1.389e+03  7.259e+02  -1.913 0.058296 .  
    ## symboling3           -1.145e+03  5.786e+02  -1.978 0.050397 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1355 on 110 degrees of freedom
    ## Multiple R-squared:  0.9787, Adjusted R-squared:  0.9725 
    ## F-statistic:   158 on 32 and 110 DF,  p-value: < 2.2e-16

``` r
vif(model_9)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.312504             2.361980             8.067728 
    ##           curbweight           enginesize               stroke 
    ##            20.668852            19.137258             4.145294 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.313737             1.415618             3.091135 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.952827             3.850808             1.987623 
    ##      carCompanymazda    carCompanymercury carCompanymitsubishi 
    ##             2.890985             1.167268             2.542324 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.570067             2.996540             2.109221 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.668091             2.072548             2.866260 
    ##     carCompanytoyota carCompanyvolkswagen       carbodyhardtop 
    ##             3.227986             2.131337             4.219219 
    ##     carbodyhatchback         carbodysedan         carbodywagon 
    ##            15.060096            16.954468             9.002900 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             4.777379             4.880879             3.000716 
    ##   cylindernumberfive           symboling3 
    ##             2.421433             3.005476

Remove carcompanymercury

``` r
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                  symboling3, data = train[, -1])
summary(model_10)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
    ##     symboling3, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2836.5  -793.4   -56.2   743.8  3470.5 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.131e+04  8.431e+03  -3.713 0.000322 ***
    ## aspiration           -2.141e+03  4.389e+02  -4.879 3.59e-06 ***
    ## enginelocation       -1.272e+04  1.225e+03 -10.385  < 2e-16 ***
    ## carwidth              7.806e+02  1.489e+02   5.244 7.57e-07 ***
    ## curbweight            2.472e+00  9.877e-01   2.502 0.013798 *  
    ## enginesize            8.523e+01  1.199e+01   7.107 1.21e-10 ***
    ## stroke               -2.627e+03  7.462e+02  -3.521 0.000626 ***
    ## peakrpm               1.139e+00  4.295e-01   2.652 0.009173 ** 
    ## carCompanybmw         7.570e+03  9.425e+02   8.032 1.11e-12 ***
    ## carCompanybuick       5.614e+03  9.265e+02   6.060 1.91e-08 ***
    ## carCompanydodge      -4.186e+03  9.638e+02  -4.343 3.12e-05 ***
    ## carCompanyhonda      -2.363e+03  8.069e+02  -2.928 0.004138 ** 
    ## carCompanyjaguar      4.694e+03  1.371e+03   3.423 0.000869 ***
    ## carCompanymazda      -3.487e+03  6.971e+02  -5.002 2.14e-06 ***
    ## carCompanymitsubishi -4.637e+03  7.887e+02  -5.879 4.42e-08 ***
    ## carCompanynissan     -2.785e+03  6.351e+02  -4.386 2.64e-05 ***
    ## carCompanypeugeot    -4.149e+03  9.847e+02  -4.214 5.13e-05 ***
    ## carCompanyplymouth   -3.447e+03  8.229e+02  -4.189 5.65e-05 ***
    ## carCompanyrenault    -4.585e+03  1.255e+03  -3.653 0.000397 ***
    ## carCompanysaab       -2.157e+03  8.933e+02  -2.415 0.017365 *  
    ## carCompanysubaru     -7.049e+03  9.640e+02  -7.313 4.32e-11 ***
    ## carCompanytoyota     -2.704e+03  5.198e+02  -5.201 9.14e-07 ***
    ## carCompanyvolkswagen -2.846e+03  6.855e+02  -4.152 6.50e-05 ***
    ## carbodyhardtop       -2.377e+03  1.020e+03  -2.330 0.021612 *  
    ## carbodyhatchback     -2.850e+03  9.254e+02  -3.079 0.002614 ** 
    ## carbodysedan         -2.851e+03  9.502e+02  -3.001 0.003327 ** 
    ## carbodywagon         -3.222e+03  1.009e+03  -3.191 0.001841 ** 
    ## drivewheelrwd        -2.601e+03  5.155e+02  -5.045 1.78e-06 ***
    ## enginetypeohc        -1.608e+03  5.617e+02  -2.863 0.005020 ** 
    ## enginetyperotor       6.892e+03  1.378e+03   5.000 2.16e-06 ***
    ## cylindernumberfive   -1.276e+03  7.289e+02  -1.750 0.082875 .  
    ## symboling3           -1.065e+03  5.815e+02  -1.831 0.069804 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1366 on 111 degrees of freedom
    ## Multiple R-squared:  0.9781, Adjusted R-squared:  0.972 
    ## F-statistic: 160.3 on 31 and 111 DF,  p-value: < 2.2e-16

``` r
vif(model_10)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.261249             2.361713             8.054941 
    ##           curbweight           enginesize               stroke 
    ##            20.650241            19.104138             4.122531 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.313261             1.398296             3.062263 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.935479             3.835586             1.987604 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.863003             2.517835             2.554517 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.986799             2.085853             1.665162 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             2.063200             2.862429             3.171562 
    ## carCompanyvolkswagen       carbodyhardtop     carbodyhatchback 
    ##             2.124016             4.212764            15.056891 
    ##         carbodysedan         carbodywagon        drivewheelrwd 
    ##            16.925446             8.997562             4.750811 
    ##        enginetypeohc      enginetyperotor   cylindernumberfive 
    ##             4.794914             2.990078             2.400885 
    ##           symboling3 
    ##             2.985509

Remove symboling3

``` r
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_11)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2902.9  -919.3   -50.4   694.9  3590.5 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.368e+04  8.418e+03  -4.001 0.000114 ***
    ## aspiration           -2.058e+03  4.411e+02  -4.666 8.58e-06 ***
    ## enginelocation       -1.213e+04  1.194e+03 -10.161  < 2e-16 ***
    ## carwidth              7.951e+02  1.502e+02   5.294 6.02e-07 ***
    ## curbweight            2.242e+00  9.900e-01   2.265 0.025434 *  
    ## enginesize            8.350e+01  1.208e+01   6.913 3.08e-10 ***
    ## stroke               -2.524e+03  7.518e+02  -3.357 0.001078 ** 
    ## peakrpm               1.216e+00  4.319e-01   2.815 0.005767 ** 
    ## carCompanybmw         7.528e+03  9.521e+02   7.906 2.03e-12 ***
    ## carCompanybuick       6.003e+03  9.113e+02   6.587 1.51e-09 ***
    ## carCompanydodge      -4.561e+03  9.516e+02  -4.793 5.09e-06 ***
    ## carCompanyhonda      -2.678e+03  7.965e+02  -3.362 0.001059 ** 
    ## carCompanyjaguar      5.115e+03  1.366e+03   3.744 0.000288 ***
    ## carCompanymazda      -3.717e+03  6.929e+02  -5.364 4.42e-07 ***
    ## carCompanymitsubishi -5.057e+03  7.625e+02  -6.632 1.22e-09 ***
    ## carCompanynissan     -3.113e+03  6.157e+02  -5.056 1.68e-06 ***
    ## carCompanypeugeot    -3.952e+03  9.890e+02  -3.996 0.000116 ***
    ## carCompanyplymouth   -3.791e+03  8.095e+02  -4.683 7.99e-06 ***
    ## carCompanyrenault    -4.771e+03  1.264e+03  -3.774 0.000259 ***
    ## carCompanysaab       -2.573e+03  8.729e+02  -2.948 0.003895 ** 
    ## carCompanysubaru     -7.031e+03  9.740e+02  -7.219 6.71e-11 ***
    ## carCompanytoyota     -2.853e+03  5.187e+02  -5.500 2.43e-07 ***
    ## carCompanyvolkswagen -3.129e+03  6.749e+02  -4.636 9.67e-06 ***
    ## carbodyhardtop       -1.583e+03  9.331e+02  -1.697 0.092542 .  
    ## carbodyhatchback     -2.155e+03  8.529e+02  -2.527 0.012901 *  
    ## carbodysedan         -1.974e+03  8.292e+02  -2.381 0.018959 *  
    ## carbodywagon         -2.348e+03  8.988e+02  -2.612 0.010232 *  
    ## drivewheelrwd        -2.631e+03  5.206e+02  -5.053 1.70e-06 ***
    ## enginetypeohc        -1.466e+03  5.621e+02  -2.608 0.010342 *  
    ## enginetyperotor       6.005e+03  1.304e+03   4.606 1.09e-05 ***
    ## cylindernumberfive   -1.401e+03  7.332e+02  -1.911 0.058591 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1380 on 112 degrees of freedom
    ## Multiple R-squared:  0.9775, Adjusted R-squared:  0.9715 
    ## F-statistic: 162.1 on 30 and 112 DF,  p-value: < 2.2e-16

``` r
vif(model_11)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.236902             2.196161             8.032145 
    ##           curbweight           enginesize               stroke 
    ##            20.318615            18.985644             4.098873 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.281710             1.397449             2.901656 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.848040             3.660810             1.931784 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.770061             2.304795             2.351581 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.951035             1.977016             1.654337 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             1.929759             2.862111             3.093509 
    ## carCompanyvolkswagen       carbodyhardtop     carbodyhatchback 
    ##             2.016207             3.451510            12.527444 
    ##         carbodysedan         carbodywagon        drivewheelrwd 
    ##            12.623698             6.985464             4.745992 
    ##        enginetypeohc      enginetyperotor   cylindernumberfive 
    ##             4.703619             2.620657             2.379662

Remove carbodyhardtop

``` r
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_12)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
    ##     enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2926.1  -916.5  -106.8   675.9  3669.3 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.272e+04  8.468e+03  -3.864 0.000187 ***
    ## aspiration           -1.990e+03  4.429e+02  -4.493 1.71e-05 ***
    ## enginelocation       -1.200e+04  1.201e+03  -9.991  < 2e-16 ***
    ## carwidth              7.671e+02  1.505e+02   5.096 1.40e-06 ***
    ## curbweight            2.599e+00  9.755e-01   2.664 0.008851 ** 
    ## enginesize            8.061e+01  1.206e+01   6.685 9.16e-10 ***
    ## stroke               -2.669e+03  7.531e+02  -3.544 0.000575 ***
    ## peakrpm               1.195e+00  4.353e-01   2.744 0.007053 ** 
    ## carCompanybmw         7.528e+03  9.600e+02   7.842 2.70e-12 ***
    ## carCompanybuick       6.098e+03  9.171e+02   6.650 1.09e-09 ***
    ## carCompanydodge      -4.600e+03  9.592e+02  -4.795 5.00e-06 ***
    ## carCompanyhonda      -2.723e+03  8.027e+02  -3.393 0.000955 ***
    ## carCompanyjaguar      5.029e+03  1.377e+03   3.654 0.000394 ***
    ## carCompanymazda      -3.788e+03  6.973e+02  -5.432 3.24e-07 ***
    ## carCompanymitsubishi -5.110e+03  7.682e+02  -6.652 1.08e-09 ***
    ## carCompanynissan     -3.218e+03  6.177e+02  -5.210 8.60e-07 ***
    ## carCompanypeugeot    -4.211e+03  9.852e+02  -4.275 4.01e-05 ***
    ## carCompanyplymouth   -3.827e+03  8.159e+02  -4.691 7.67e-06 ***
    ## carCompanyrenault    -4.733e+03  1.274e+03  -3.713 0.000319 ***
    ## carCompanysaab       -2.870e+03  8.622e+02  -3.329 0.001177 ** 
    ## carCompanysubaru     -7.387e+03  9.589e+02  -7.704 5.51e-12 ***
    ## carCompanytoyota     -3.023e+03  5.132e+02  -5.891 4.03e-08 ***
    ## carCompanyvolkswagen -3.079e+03  6.798e+02  -4.529 1.48e-05 ***
    ## carbodyhatchback     -1.108e+03  5.935e+02  -1.867 0.064494 .  
    ## carbodysedan         -9.473e+02  5.715e+02  -1.658 0.100152    
    ## carbodywagon         -1.386e+03  7.032e+02  -1.971 0.051193 .  
    ## drivewheelrwd        -2.769e+03  5.186e+02  -5.339 4.89e-07 ***
    ## enginetypeohc        -1.636e+03  5.577e+02  -2.934 0.004056 ** 
    ## enginetyperotor       5.810e+03  1.310e+03   4.437 2.13e-05 ***
    ## cylindernumberfive   -1.534e+03  7.350e+02  -2.087 0.039118 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1392 on 113 degrees of freedom
    ## Multiple R-squared:  0.9769, Adjusted R-squared:  0.971 
    ## F-statistic: 164.8 on 29 and 113 DF,  p-value: < 2.2e-16

``` r
vif(model_12)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.218366             2.187523             7.934825 
    ##           curbweight           enginesize               stroke 
    ##            19.404389            18.606778             4.045767 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.278989             1.397449             2.890627 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.846977             3.656707             1.929153 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.759994             2.300894             2.327964 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.880467             1.975626             1.653812 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             1.852007             2.728885             2.977796 
    ## carCompanyvolkswagen     carbodyhatchback         carbodysedan 
    ##             2.012361             5.966038             5.898064 
    ##         carbodywagon        drivewheelrwd        enginetypeohc 
    ##             4.206069             4.631101             4.554116 
    ##      enginetyperotor   cylindernumberfive 
    ##             2.600281             2.352393

Remove carbodyhatchback

``` r
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_13)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
    ##     enginetyperotor + cylindernumberfive, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3095.2  -911.6  -107.2   734.3  3685.2 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.100e+04  8.509e+03  -3.643 0.000408 ***
    ## aspiration           -1.920e+03  4.461e+02  -4.303 3.58e-05 ***
    ## enginelocation       -1.267e+04  1.159e+03 -10.927  < 2e-16 ***
    ## carwidth              7.200e+02  1.500e+02   4.800 4.86e-06 ***
    ## curbweight            2.555e+00  9.858e-01   2.592 0.010797 *  
    ## enginesize            8.398e+01  1.205e+01   6.969 2.19e-10 ***
    ## stroke               -2.589e+03  7.600e+02  -3.407 0.000909 ***
    ## peakrpm               1.264e+00  4.384e-01   2.884 0.004691 ** 
    ## carCompanybmw         7.328e+03  9.643e+02   7.599 9.06e-12 ***
    ## carCompanybuick       6.508e+03  9.000e+02   7.231 5.91e-11 ***
    ## carCompanydodge      -4.850e+03  9.601e+02  -5.052 1.68e-06 ***
    ## carCompanyhonda      -2.948e+03  8.022e+02  -3.675 0.000363 ***
    ## carCompanyjaguar      4.825e+03  1.387e+03   3.479 0.000714 ***
    ## carCompanymazda      -3.900e+03  7.023e+02  -5.553 1.86e-07 ***
    ## carCompanymitsubishi -5.303e+03  7.694e+02  -6.893 3.20e-10 ***
    ## carCompanynissan     -3.291e+03  6.231e+02  -5.282 6.21e-07 ***
    ## carCompanypeugeot    -3.990e+03  9.886e+02  -4.036 9.89e-05 ***
    ## carCompanyplymouth   -4.071e+03  8.141e+02  -5.001 2.09e-06 ***
    ## carCompanyrenault    -4.953e+03  1.283e+03  -3.861 0.000188 ***
    ## carCompanysaab       -2.834e+03  8.713e+02  -3.252 0.001506 ** 
    ## carCompanysubaru     -7.241e+03  9.661e+02  -7.495 1.54e-11 ***
    ## carCompanytoyota     -3.081e+03  5.178e+02  -5.950 3.02e-08 ***
    ## carCompanyvolkswagen -2.971e+03  6.847e+02  -4.339 3.11e-05 ***
    ## carbodysedan         -4.022e+01  3.041e+02  -0.132 0.895019    
    ## carbodywagon         -4.404e+02  4.932e+02  -0.893 0.373728    
    ## drivewheelrwd        -2.677e+03  5.218e+02  -5.130 1.20e-06 ***
    ## enginetypeohc        -1.437e+03  5.533e+02  -2.597 0.010651 *  
    ## enginetyperotor       5.880e+03  1.323e+03   4.444 2.06e-05 ***
    ## cylindernumberfive   -1.657e+03  7.400e+02  -2.240 0.027060 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1407 on 114 degrees of freedom
    ## Multiple R-squared:  0.9762, Adjusted R-squared:  0.9703 
    ## F-statistic:   167 on 28 and 114 DF,  p-value: < 2.2e-16

``` r
vif(model_13)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.202293             1.994361             7.711583 
    ##           curbweight           enginesize               stroke 
    ##            19.393186            18.187908             4.032834 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.254778             1.379961             2.724966 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.810861             3.574227             1.916987 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.739600             2.259048             2.318579 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.838584             1.925035             1.639630 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             1.851047             2.710699             2.967044 
    ## carCompanyvolkswagen         carbodysedan         carbodywagon 
    ##             1.997852             1.634720             2.024969 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             4.589746             4.387168             2.598157 
    ##   cylindernumberfive 
    ##             2.333459

Remove carbodysedan

``` r
model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                  carbodywagon + 
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_14)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     carbodywagon + drivewheelrwd + enginetypeohc + enginetyperotor + 
    ##     cylindernumberfive, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3088.8  -907.4  -112.0   729.4  3693.8 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.090e+04  8.439e+03  -3.661 0.000381 ***
    ## aspiration           -1.927e+03  4.408e+02  -4.372 2.72e-05 ***
    ## enginelocation       -1.267e+04  1.154e+03 -10.974  < 2e-16 ***
    ## carwidth              7.198e+02  1.493e+02   4.820 4.43e-06 ***
    ## curbweight            2.529e+00  9.615e-01   2.630 0.009710 ** 
    ## enginesize            8.427e+01  1.180e+01   7.138 9.11e-11 ***
    ## stroke               -2.607e+03  7.451e+02  -3.498 0.000667 ***
    ## peakrpm               1.260e+00  4.354e-01   2.895 0.004543 ** 
    ## carCompanybmw         7.310e+03  9.508e+02   7.688 5.52e-12 ***
    ## carCompanybuick       6.499e+03  8.939e+02   7.271 4.66e-11 ***
    ## carCompanydodge      -4.822e+03  9.331e+02  -5.168 1.01e-06 ***
    ## carCompanyhonda      -2.930e+03  7.860e+02  -3.727 0.000302 ***
    ## carCompanyjaguar      4.819e+03  1.380e+03   3.491 0.000683 ***
    ## carCompanymazda      -3.892e+03  6.966e+02  -5.587 1.57e-07 ***
    ## carCompanymitsubishi -5.284e+03  7.522e+02  -7.025 1.61e-10 ***
    ## carCompanynissan     -3.286e+03  6.194e+02  -5.305 5.52e-07 ***
    ## carCompanypeugeot    -3.998e+03  9.825e+02  -4.069 8.70e-05 ***
    ## carCompanyplymouth   -4.050e+03  7.954e+02  -5.092 1.40e-06 ***
    ## carCompanyrenault    -4.921e+03  1.255e+03  -3.922 0.000150 ***
    ## carCompanysaab       -2.827e+03  8.659e+02  -3.264 0.001446 ** 
    ## carCompanysubaru     -7.252e+03  9.585e+02  -7.566 1.04e-11 ***
    ## carCompanytoyota     -3.070e+03  5.086e+02  -6.035 1.99e-08 ***
    ## carCompanyvolkswagen -2.971e+03  6.818e+02  -4.358 2.87e-05 ***
    ## carbodywagon         -4.135e+02  4.474e+02  -0.924 0.357247    
    ## drivewheelrwd        -2.666e+03  5.130e+02  -5.197 8.89e-07 ***
    ## enginetypeohc        -1.445e+03  5.472e+02  -2.641 0.009409 ** 
    ## enginetyperotor       5.904e+03  1.305e+03   4.523 1.49e-05 ***
    ## cylindernumberfive   -1.642e+03  7.283e+02  -2.255 0.026014 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1401 on 115 degrees of freedom
    ## Multiple R-squared:  0.9762, Adjusted R-squared:  0.9706 
    ## F-statistic: 174.6 on 27 and 115 DF,  p-value: < 2.2e-16

``` r
vif(model_14)
```

    ##           aspiration       enginelocation             carwidth 
    ##             2.168533             1.994359             7.711049 
    ##           curbweight           enginesize               stroke 
    ##            18.609871            17.605716             3.909732 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.237968             1.353289             2.711104 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.725059             3.461444             1.914859 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.718220             2.177648             2.310841 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.827957             1.853211             1.582307 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             1.843872             2.691557             2.887489 
    ## carCompanyvolkswagen         carbodywagon        drivewheelrwd 
    ##             1.997840             1.680645             4.474458 
    ##        enginetypeohc      enginetyperotor   cylindernumberfive 
    ##             4.327909             2.549810             2.279573

Remove carbodywagon

``` r
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_15)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     curbweight + enginesize + stroke + peakrpm + carCompanybmw + 
    ##     carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
    ##     carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3315.3  -841.4  -241.4   701.0  3718.2 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.161e+04  8.398e+03  -3.764 0.000264 ***
    ## aspiration           -2.048e+03  4.207e+02  -4.867 3.61e-06 ***
    ## enginelocation       -1.257e+04  1.149e+03 -10.942  < 2e-16 ***
    ## carwidth              7.387e+02  1.479e+02   4.996 2.09e-06 ***
    ## curbweight            2.060e+00  8.167e-01   2.523 0.012996 *  
    ## enginesize            8.790e+01  1.112e+01   7.903 1.74e-12 ***
    ## stroke               -2.566e+03  7.433e+02  -3.451 0.000779 ***
    ## peakrpm               1.260e+00  4.351e-01   2.895 0.004534 ** 
    ## carCompanybmw         7.366e+03  9.483e+02   7.768 3.51e-12 ***
    ## carCompanybuick       6.432e+03  8.903e+02   7.224 5.73e-11 ***
    ## carCompanydodge      -4.916e+03  9.269e+02  -5.304 5.50e-07 ***
    ## carCompanyhonda      -2.939e+03  7.855e+02  -3.741 0.000286 ***
    ## carCompanyjaguar      4.955e+03  1.372e+03   3.613 0.000449 ***
    ## carCompanymazda      -3.874e+03  6.959e+02  -5.568 1.69e-07 ***
    ## carCompanymitsubishi -5.270e+03  7.515e+02  -7.012 1.67e-10 ***
    ## carCompanynissan     -3.348e+03  6.154e+02  -5.441 2.99e-07 ***
    ## carCompanypeugeot    -4.007e+03  9.819e+02  -4.081 8.28e-05 ***
    ## carCompanyplymouth   -4.115e+03  7.918e+02  -5.197 8.79e-07 ***
    ## carCompanyrenault    -5.082e+03  1.242e+03  -4.093 7.91e-05 ***
    ## carCompanysaab       -2.640e+03  8.415e+02  -3.137 0.002163 ** 
    ## carCompanysubaru     -7.325e+03  9.547e+02  -7.672 5.76e-12 ***
    ## carCompanytoyota     -3.064e+03  5.083e+02  -6.028 2.02e-08 ***
    ## carCompanyvolkswagen -2.910e+03  6.782e+02  -4.291 3.70e-05 ***
    ## drivewheelrwd        -2.580e+03  5.043e+02  -5.117 1.24e-06 ***
    ## enginetypeohc        -1.466e+03  5.464e+02  -2.684 0.008346 ** 
    ## enginetyperotor       6.030e+03  1.297e+03   4.648 8.93e-06 ***
    ## cylindernumberfive   -1.585e+03  7.252e+02  -2.186 0.030806 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1400 on 116 degrees of freedom
    ## Multiple R-squared:  0.976,  Adjusted R-squared:  0.9706 
    ## F-statistic: 181.5 on 26 and 116 DF,  p-value: < 2.2e-16

``` r
vif(model_15)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.978117             1.977608             7.566853 
    ##           curbweight           enginesize               stroke 
    ##            13.443512            15.651346             3.895722 
    ##              peakrpm        carCompanybmw      carCompanybuick 
    ##             3.237958             1.347744             2.692863 
    ##      carCompanydodge      carCompanyhonda     carCompanyjaguar 
    ##             1.704654             3.460913             1.893071 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             2.716273             2.176719             2.283744 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             2.827677             1.838942             1.551783 
    ##       carCompanysaab     carCompanysubaru     carCompanytoyota 
    ##             1.743672             2.673379             2.887044 
    ## carCompanyvolkswagen        drivewheelrwd        enginetypeohc 
    ##             1.979081             4.328286             4.320421 
    ##      enginetyperotor   cylindernumberfive 
    ##             2.522064             2.263287

All variables are significant now

Variable curbweight and enginesize have high VIFs. curbweight is very less significant as compared to enginesize. Remove curbweight.

``` r
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_16)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + peakrpm + carCompanybmw + carCompanybuick + 
    ##     carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
    ##     carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
    ##     carCompanyplymouth + carCompanyrenault + carCompanysaab + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2849.2  -915.4   -83.8   670.2  3786.6 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.785e+04  8.208e+03  -4.611 1.03e-05 ***
    ## aspiration           -2.390e+03  4.072e+02  -5.869 4.18e-08 ***
    ## enginelocation       -1.218e+04  1.164e+03 -10.464  < 2e-16 ***
    ## carwidth              8.885e+02  1.385e+02   6.416 3.09e-09 ***
    ## enginesize            1.004e+02  1.018e+01   9.871  < 2e-16 ***
    ## stroke               -2.625e+03  7.598e+02  -3.454 0.000769 ***
    ## peakrpm               1.252e+00  4.449e-01   2.815 0.005725 ** 
    ## carCompanybmw         7.341e+03  9.697e+02   7.570 9.39e-12 ***
    ## carCompanybuick       6.242e+03  9.073e+02   6.880 3.14e-10 ***
    ## carCompanydodge      -5.014e+03  9.471e+02  -5.295 5.66e-07 ***
    ## carCompanyhonda      -3.112e+03  8.002e+02  -3.888 0.000168 ***
    ## carCompanyjaguar      5.813e+03  1.359e+03   4.278 3.88e-05 ***
    ## carCompanymazda      -4.172e+03  7.013e+02  -5.950 2.87e-08 ***
    ## carCompanymitsubishi -5.420e+03  7.662e+02  -7.074 1.19e-10 ***
    ## carCompanynissan     -3.571e+03  6.229e+02  -5.733 7.85e-08 ***
    ## carCompanypeugeot    -3.361e+03  9.694e+02  -3.467 0.000736 ***
    ## carCompanyplymouth   -4.294e+03  8.065e+02  -5.325 4.96e-07 ***
    ## carCompanyrenault    -5.094e+03  1.270e+03  -4.011 0.000107 ***
    ## carCompanysaab       -2.186e+03  8.407e+02  -2.601 0.010508 *  
    ## carCompanysubaru     -7.377e+03  9.761e+02  -7.557 1.01e-11 ***
    ## carCompanytoyota     -3.025e+03  5.195e+02  -5.823 5.17e-08 ***
    ## carCompanyvolkswagen -2.976e+03  6.930e+02  -4.294 3.64e-05 ***
    ## drivewheelrwd        -2.251e+03  4.981e+02  -4.519 1.50e-05 ***
    ## enginetypeohc        -1.430e+03  5.586e+02  -2.560 0.011754 *  
    ## enginetyperotor       6.612e+03  1.305e+03   5.065 1.54e-06 ***
    ## cylindernumberfive   -1.207e+03  7.256e+02  -1.664 0.098864 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1432 on 117 degrees of freedom
    ## Multiple R-squared:  0.9747, Adjusted R-squared:  0.9693 
    ## F-statistic: 180.3 on 25 and 117 DF,  p-value: < 2.2e-16

``` r
vif(model_16)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.772295             1.942441             6.346526 
    ##           enginesize               stroke              peakrpm 
    ##            12.524310             3.891847             3.237826 
    ##        carCompanybmw      carCompanybuick      carCompanydodge 
    ##             1.347600             2.673693             1.701638 
    ##      carCompanyhonda     carCompanyjaguar      carCompanymazda 
    ##             3.434503             1.776751             2.637920 
    ## carCompanymitsubishi     carCompanynissan    carCompanypeugeot 
    ##             2.163048             2.236931             2.635740 
    ##   carCompanyplymouth    carCompanyrenault       carCompanysaab 
    ##             1.824119             1.551760             1.664055 
    ##     carCompanysubaru     carCompanytoyota carCompanyvolkswagen 
    ##             2.672141             2.884479             1.976176 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             4.037970             4.317371             2.442190 
    ##   cylindernumberfive 
    ##             2.166507

Remove cylindernumberfive

``` r
model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke + peakrpm + 
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_17)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + peakrpm + carCompanybmw + carCompanybuick + 
    ##     carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
    ##     carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
    ##     carCompanyplymouth + carCompanyrenault + carCompanysaab + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2825.4  -927.2  -101.8   700.9  3992.5 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -3.309e+04  7.752e+03  -4.269 3.99e-05 ***
    ## aspiration           -2.381e+03  4.102e+02  -5.803 5.59e-08 ***
    ## enginelocation       -1.201e+04  1.168e+03 -10.278  < 2e-16 ***
    ## carwidth              8.369e+02  1.360e+02   6.155 1.06e-08 ***
    ## enginesize            1.035e+02  1.008e+01  10.270  < 2e-16 ***
    ## stroke               -3.008e+03  7.294e+02  -4.124 6.95e-05 ***
    ## peakrpm               1.071e+00  4.346e-01   2.464 0.015164 *  
    ## carCompanybmw         7.448e+03  9.749e+02   7.640 6.31e-12 ***
    ## carCompanybuick       5.628e+03  8.349e+02   6.741 6.13e-10 ***
    ## carCompanydodge      -4.552e+03  9.122e+02  -4.990 2.10e-06 ***
    ## carCompanyhonda      -2.540e+03  7.280e+02  -3.489 0.000683 ***
    ## carCompanyjaguar      5.954e+03  1.366e+03   4.358 2.82e-05 ***
    ## carCompanymazda      -3.804e+03  6.704e+02  -5.674 1.01e-07 ***
    ## carCompanymitsubishi -4.949e+03  7.173e+02  -6.900 2.77e-10 ***
    ## carCompanynissan     -3.256e+03  5.979e+02  -5.446 2.85e-07 ***
    ## carCompanypeugeot    -3.309e+03  9.762e+02  -3.390 0.000951 ***
    ## carCompanyplymouth   -3.870e+03  7.708e+02  -5.021 1.85e-06 ***
    ## carCompanyrenault    -4.457e+03  1.220e+03  -3.654 0.000387 ***
    ## carCompanysaab       -1.918e+03  8.312e+02  -2.307 0.022805 *  
    ## carCompanysubaru     -7.437e+03  9.827e+02  -7.568 9.18e-12 ***
    ## carCompanytoyota     -2.824e+03  5.091e+02  -5.548 1.80e-07 ***
    ## carCompanyvolkswagen -2.636e+03  6.672e+02  -3.951 0.000133 ***
    ## drivewheelrwd        -2.080e+03  4.911e+02  -4.236 4.53e-05 ***
    ## enginetypeohc        -1.564e+03  5.568e+02  -2.809 0.005826 ** 
    ## enginetyperotor       6.639e+03  1.315e+03   5.048 1.64e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1442 on 118 degrees of freedom
    ## Multiple R-squared:  0.9741, Adjusted R-squared:  0.9688 
    ## F-statistic: 184.9 on 24 and 118 DF,  p-value: < 2.2e-16

``` r
vif(model_17)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.771953             1.926258             6.028944 
    ##           enginesize               stroke              peakrpm 
    ##            12.109147             3.533420             3.043062 
    ##        carCompanybmw      carCompanybuick      carCompanydodge 
    ##             1.341733             2.230630             1.555138 
    ##      carCompanyhonda     carCompanyjaguar      carCompanymazda 
    ##             2.800603             1.769801             2.374703 
    ## carCompanymitsubishi     carCompanynissan    carCompanypeugeot 
    ##             1.867738             2.031163             2.633006 
    ##   carCompanyplymouth    carCompanyrenault       carCompanysaab 
    ##             1.641870             1.410624             1.602571 
    ##     carCompanysubaru     carCompanytoyota carCompanyvolkswagen 
    ##             2.668441             2.728401             1.804476 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             3.867199             4.227312             2.441805

Remove peakrpm (higher VIF, higher p-value as compared to othe variables in model)

``` r
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_18)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyhonda + carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + drivewheelrwd + enginetypeohc + enginetyperotor, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2899.1  -918.0  -143.4   786.0  4310.6 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -32049.483   7903.750  -4.055 8.99e-05 ***
    ## aspiration            -2140.307    406.904  -5.260 6.45e-07 ***
    ## enginelocation       -12831.420   1142.596 -11.230  < 2e-16 ***
    ## carwidth                943.990    131.563   7.175 6.69e-11 ***
    ## enginesize               94.160      9.533   9.877  < 2e-16 ***
    ## stroke                -3121.302    743.286  -4.199 5.19e-05 ***
    ## carCompanybmw          7539.434    994.698   7.580 8.33e-12 ***
    ## carCompanybuick        5075.660    821.234   6.181 9.25e-09 ***
    ## carCompanydodge       -4142.394    915.815  -4.523 1.45e-05 ***
    ## carCompanyhonda       -1855.770    687.227  -2.700 0.007936 ** 
    ## carCompanyjaguar       5798.870   1393.711   4.161 6.02e-05 ***
    ## carCompanymazda       -4095.768    673.739  -6.079 1.50e-08 ***
    ## carCompanymitsubishi  -4642.958    721.334  -6.437 2.68e-09 ***
    ## carCompanynissan      -3233.061    610.482  -5.296 5.50e-07 ***
    ## carCompanypeugeot     -4657.536    825.533  -5.642 1.16e-07 ***
    ## carCompanyplymouth    -3512.379    773.028  -4.544 1.34e-05 ***
    ## carCompanyrenault     -4343.276   1244.708  -3.489 0.000680 ***
    ## carCompanysaab        -1876.023    848.581  -2.211 0.028963 *  
    ## carCompanysubaru      -8340.518    930.988  -8.959 5.43e-15 ***
    ## carCompanytoyota      -3163.316    500.480  -6.321 4.71e-09 ***
    ## carCompanyvolkswagen  -2703.636    680.662  -3.972 0.000122 ***
    ## drivewheelrwd         -1856.852    492.826  -3.768 0.000258 ***
    ## enginetypeohc         -2119.109    519.945  -4.076 8.32e-05 ***
    ## enginetyperotor        6667.315   1342.805   4.965 2.32e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1473 on 119 degrees of freedom
    ## Multiple R-squared:  0.9728, Adjusted R-squared:  0.9675 
    ## F-statistic: 184.8 on 23 and 119 DF,  p-value: < 2.2e-16

``` r
vif(model_18)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.671896             1.767822             5.413525 
    ##           enginesize               stroke        carCompanybmw 
    ##            10.387030             3.519448             1.339786 
    ##      carCompanybuick      carCompanydodge      carCompanyhonda 
    ##             2.070021             1.503467             2.393628 
    ##     carCompanyjaguar      carCompanymazda carCompanymitsubishi 
    ##             1.766031             2.300593             1.811758 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             2.030658             1.806109             1.583675 
    ##    carCompanyrenault       carCompanysaab     carCompanysubaru 
    ##             1.408600             1.601913             2.297016 
    ##     carCompanytoyota carCompanyvolkswagen        drivewheelrwd 
    ##             2.529287             1.801416             3.735162 
    ##        enginetypeohc      enginetyperotor 
    ##             3.535221             2.441623

Remove carCompanysaab

``` r
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_19)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyhonda + carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + drivewheelrwd + enginetypeohc + enginetyperotor, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3052.8  -898.0  -198.4   680.0  4296.0 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -33659.788   7996.579  -4.209 4.97e-05 ***
    ## aspiration            -1964.968    405.513  -4.846 3.81e-06 ***
    ## enginelocation       -13312.041   1139.749 -11.680  < 2e-16 ***
    ## carwidth                946.329    133.673   7.079 1.06e-10 ***
    ## enginesize               91.858      9.628   9.540  < 2e-16 ***
    ## stroke                -2676.425    727.026  -3.681 0.000349 ***
    ## carCompanybmw          7693.819   1008.188   7.631 6.14e-12 ***
    ## carCompanybuick        5374.795    823.027   6.531 1.65e-09 ***
    ## carCompanydodge       -3720.464    910.101  -4.088 7.90e-05 ***
    ## carCompanyhonda       -1543.519    683.364  -2.259 0.025707 *  
    ## carCompanyjaguar       5851.103   1415.903   4.132 6.68e-05 ***
    ## carCompanymazda       -3683.866    657.869  -5.600 1.38e-07 ***
    ## carCompanymitsubishi  -4226.349    707.472  -5.974 2.43e-08 ***
    ## carCompanynissan      -2870.669    597.513  -4.804 4.53e-06 ***
    ## carCompanypeugeot     -4359.018    827.502  -5.268 6.16e-07 ***
    ## carCompanyplymouth    -3170.356    769.559  -4.120 7.01e-05 ***
    ## carCompanyrenault     -4158.003   1261.839  -3.295 0.001293 ** 
    ## carCompanysubaru      -7456.438    854.237  -8.729 1.78e-14 ***
    ## carCompanytoyota      -2883.946    492.045  -5.861 4.13e-08 ***
    ## carCompanyvolkswagen  -2313.226    667.918  -3.463 0.000740 ***
    ## drivewheelrwd         -1417.349    458.197  -3.093 0.002463 ** 
    ## enginetypeohc         -1975.519    524.162  -3.769 0.000256 ***
    ## enginetyperotor        6290.949   1353.373   4.648 8.66e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1496 on 120 degrees of freedom
    ## Multiple R-squared:  0.9716, Adjusted R-squared:  0.9665 
    ## F-statistic: 186.9 on 22 and 120 DF,  p-value: < 2.2e-16

``` r
vif(model_19)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.608379             1.703824             5.413175 
    ##           enginesize               stroke        carCompanybmw 
    ##            10.263051             3.261488             1.333183 
    ##      carCompanybuick      carCompanydodge      carCompanyhonda 
    ##             2.013827             1.438173             2.292522 
    ##     carCompanyjaguar      carCompanymazda carCompanymitsubishi 
    ##             1.765524             2.124658             1.688108 
    ##     carCompanynissan    carCompanypeugeot   carCompanyplymouth 
    ##             1.884253             1.757789             1.520244 
    ##    carCompanyrenault     carCompanysubaru     carCompanytoyota 
    ##             1.402214             1.873208             2.368038 
    ## carCompanyvolkswagen        drivewheelrwd        enginetypeohc 
    ##             1.680160             3.127370             3.480056 
    ##      enginetyperotor 
    ##             2.402378

Remove carcompanyhonda

``` r
model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + carCompanyrenault + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_20)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanyrenault + carCompanysubaru + carCompanytoyota + 
    ##     carCompanyvolkswagen + drivewheelrwd + enginetypeohc + enginetyperotor, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2933.4  -952.8  -187.6   792.9  4174.5 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -35281.926   8098.129  -4.357 2.78e-05 ***
    ## aspiration            -2199.794    398.547  -5.520 1.97e-07 ***
    ## enginelocation       -13501.901   1155.750 -11.682  < 2e-16 ***
    ## carwidth               1000.668    133.700   7.484 1.28e-11 ***
    ## enginesize               93.965      9.744   9.643  < 2e-16 ***
    ## stroke                -3443.778    653.571  -5.269 6.06e-07 ***
    ## carCompanybmw          7961.943   1018.003   7.821 2.19e-12 ***
    ## carCompanybuick        5446.522    836.237   6.513 1.76e-09 ***
    ## carCompanydodge       -2933.398    854.881  -3.431 0.000822 ***
    ## carCompanyjaguar       6630.652   1396.277   4.749 5.67e-06 ***
    ## carCompanymazda       -2970.020    586.681  -5.062 1.50e-06 ***
    ## carCompanymitsubishi  -3468.288    633.267  -5.477 2.39e-07 ***
    ## carCompanynissan      -2156.855    515.622  -4.183 5.48e-05 ***
    ## carCompanypeugeot     -4127.830    834.949  -4.944 2.49e-06 ***
    ## carCompanyplymouth    -2407.045    703.009  -3.424 0.000843 ***
    ## carCompanyrenault     -3080.018   1187.736  -2.593 0.010682 *  
    ## carCompanysubaru      -7201.910    861.005  -8.365 1.21e-13 ***
    ## carCompanytoyota      -2264.826    415.494  -5.451 2.69e-07 ***
    ## carCompanyvolkswagen  -1569.708    590.936  -2.656 0.008965 ** 
    ## drivewheelrwd         -1276.020    461.533  -2.765 0.006589 ** 
    ## enginetypeohc         -1908.516    532.118  -3.587 0.000484 ***
    ## enginetyperotor        6262.679   1376.062   4.551 1.28e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1522 on 121 degrees of freedom
    ## Multiple R-squared:  0.9704, Adjusted R-squared:  0.9653 
    ## F-statistic: 189.2 on 21 and 121 DF,  p-value: < 2.2e-16

``` r
vif(model_20)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.502661             1.694557             5.237835 
    ##           enginesize               stroke        carCompanybmw 
    ##            10.166660             2.549313             1.314700 
    ##      carCompanybuick      carCompanydodge     carCompanyjaguar 
    ##             2.010829             1.227342             1.660624 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             1.634316             1.308208             1.357151 
    ##    carCompanypeugeot   carCompanyplymouth    carCompanyrenault 
    ##             1.730897             1.227079             1.201623 
    ##     carCompanysubaru     carCompanytoyota carCompanyvolkswagen 
    ##             1.840611             1.633170             1.272059 
    ##        drivewheelrwd        enginetypeohc      enginetyperotor 
    ##             3.069050             3.468910             2.402173

Remove carCompany renault

``` r
model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_21)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3260.4  -943.8  -142.3   799.1  4361.6 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -32607.217   8218.462  -3.968 0.000123 ***
    ## aspiration            -2329.440    404.569  -5.758 6.50e-08 ***
    ## enginelocation       -13410.162   1182.000 -11.345  < 2e-16 ***
    ## carwidth                981.760    136.598   7.187 5.76e-11 ***
    ## enginesize               94.554      9.967   9.486 2.52e-16 ***
    ## stroke                -3971.151    635.530  -6.249 6.30e-09 ***
    ## carCompanybmw          7996.221   1041.525   7.677 4.49e-12 ***
    ## carCompanybuick        5653.569    851.723   6.638 9.30e-10 ***
    ## carCompanydodge       -2686.434    869.264  -3.090 0.002476 ** 
    ## carCompanyjaguar       7258.001   1407.052   5.158 9.77e-07 ***
    ## carCompanymazda       -2765.734    594.851  -4.649 8.50e-06 ***
    ## carCompanymitsubishi  -3222.550    640.658  -5.030 1.71e-06 ***
    ## carCompanynissan      -1951.425    521.316  -3.743 0.000278 ***
    ## carCompanypeugeot     -4033.921    853.510  -4.726 6.19e-06 ***
    ## carCompanyplymouth    -2205.114    714.887  -3.085 0.002522 ** 
    ## carCompanysubaru      -7330.492    879.511  -8.335 1.36e-13 ***
    ## carCompanytoyota      -2122.316    421.396  -5.036 1.66e-06 ***
    ## carCompanyvolkswagen  -1311.350    595.986  -2.200 0.029669 *  
    ## drivewheelrwd         -1176.792    470.612  -2.501 0.013726 *  
    ## enginetypeohc         -1860.521    544.130  -3.419 0.000854 ***
    ## enginetyperotor        6240.063   1407.948   4.432 2.05e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1557 on 122 degrees of freedom
    ## Multiple R-squared:  0.9688, Adjusted R-squared:  0.9637 
    ## F-statistic: 189.4 on 20 and 122 DF,  p-value: < 2.2e-16

``` r
vif(model_21)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.479015             1.692969             5.222257 
    ##           enginesize               stroke        carCompanybmw 
    ##            10.161148             2.302478             1.314479 
    ##      carCompanybuick      carCompanydodge     carCompanyjaguar 
    ##             1.992498             1.212110             1.610772 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             1.604848             1.278913             1.325116 
    ##    carCompanypeugeot   carCompanyplymouth     carCompanysubaru 
    ##             1.727640             1.212024             1.834507 
    ##     carCompanytoyota carCompanyvolkswagen        drivewheelrwd 
    ##             1.604600             1.235901             3.047955 
    ##        enginetypeohc      enginetyperotor 
    ##             3.464714             2.402076

Remove drivewheelrwd

``` r
model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_22)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
    ##     enginetypeohc + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3290.2 -1040.3   -50.5   795.7  4644.7 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -34023.148   8372.171  -4.064 8.54e-05 ***
    ## aspiration            -2093.736    401.748  -5.212 7.65e-07 ***
    ## enginelocation       -13453.673   1206.845 -11.148  < 2e-16 ***
    ## carwidth                995.028    139.379   7.139 7.17e-11 ***
    ## enginesize               86.213      9.591   8.989 3.68e-15 ***
    ## stroke                -3709.415    640.096  -5.795 5.38e-08 ***
    ## carCompanybmw          7461.840   1040.904   7.169 6.15e-11 ***
    ## carCompanybuick        5740.883    868.988   6.606 1.06e-09 ***
    ## carCompanydodge       -2358.857    877.493  -2.688 0.008180 ** 
    ## carCompanyjaguar       7441.850   1434.820   5.187 8.54e-07 ***
    ## carCompanymazda       -2599.318    603.607  -4.306 3.36e-05 ***
    ## carCompanymitsubishi  -2935.592    643.614  -4.561 1.21e-05 ***
    ## carCompanynissan      -1873.953    531.391  -3.527 0.000593 ***
    ## carCompanypeugeot     -4476.869    852.567  -5.251 6.43e-07 ***
    ## carCompanyplymouth    -2158.112    729.740  -2.957 0.003721 ** 
    ## carCompanysubaru      -6720.095    862.811  -7.789 2.41e-12 ***
    ## carCompanytoyota      -2338.660    421.133  -5.553 1.65e-07 ***
    ## carCompanyvolkswagen  -1120.214    603.553  -1.856 0.065842 .  
    ## enginetypeohc         -1647.069    548.747  -3.002 0.003254 ** 
    ## enginetyperotor        4980.765   1342.580   3.710 0.000313 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1590 on 123 degrees of freedom
    ## Multiple R-squared:  0.9672, Adjusted R-squared:  0.9621 
    ## F-statistic: 190.9 on 19 and 123 DF,  p-value: < 2.2e-16

``` r
vif(model_22)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.398727             1.692602             5.214377 
    ##           enginesize               stroke        carCompanybmw 
    ##             9.023358             2.240021             1.259138 
    ##      carCompanybuick      carCompanydodge     carCompanyjaguar 
    ##             1.989149             1.184581             1.606374 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             1.584761             1.237879             1.320435 
    ##    carCompanypeugeot   carCompanyplymouth     carCompanysubaru 
    ##             1.653224             1.211186             1.693192 
    ##     carCompanytoyota carCompanyvolkswagen        enginetypeohc 
    ##             1.536960             1.215572             3.379445 
    ##      enginetyperotor 
    ##             2.094752

Remove carCompanyvolkswagen

``` r
model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + carCompanydodge + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_23)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanydodge + 
    ##     carCompanyjaguar + carCompanymazda + carCompanymitsubishi + 
    ##     carCompanynissan + carCompanypeugeot + carCompanyplymouth + 
    ##     carCompanysubaru + carCompanytoyota + enginetypeohc + enginetyperotor, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3194.3  -990.3   -44.3   845.0  4650.6 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -32932.564   8433.456  -3.905 0.000154 ***
    ## aspiration            -2117.376    405.486  -5.222 7.24e-07 ***
    ## enginelocation       -13437.207   1218.651 -11.026  < 2e-16 ***
    ## carwidth                979.846    140.503   6.974 1.63e-10 ***
    ## enginesize               88.246      9.622   9.171 1.27e-15 ***
    ## stroke                -3873.006    640.219  -6.050 1.59e-08 ***
    ## carCompanybmw          7587.674   1048.884   7.234 4.27e-11 ***
    ## carCompanybuick        5837.690    875.931   6.665 7.78e-10 ***
    ## carCompanydodge       -2129.022    877.234  -2.427 0.016663 *  
    ## carCompanyjaguar       7572.902   1447.140   5.233 6.89e-07 ***
    ## carCompanymazda       -2363.760    595.903  -3.967 0.000122 ***
    ## carCompanymitsubishi  -2699.971    637.160  -4.238 4.37e-05 ***
    ## carCompanynissan      -1666.466    524.595  -3.177 0.001880 ** 
    ## carCompanypeugeot     -4288.482    854.808  -5.017 1.77e-06 ***
    ## carCompanyplymouth    -1926.100    726.007  -2.653 0.009022 ** 
    ## carCompanysubaru      -6615.689    869.422  -7.609 6.00e-12 ***
    ## carCompanytoyota      -2140.679    411.396  -5.203 7.85e-07 ***
    ## enginetypeohc         -1656.027    554.109  -2.989 0.003379 ** 
    ## enginetyperotor        5049.474   1355.236   3.726 0.000294 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1605 on 124 degrees of freedom
    ## Multiple R-squared:  0.9663, Adjusted R-squared:  0.9614 
    ## F-statistic: 197.4 on 18 and 124 DF,  p-value: < 2.2e-16

``` r
vif(model_23)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.397321             1.692511             5.196418 
    ##           enginesize               stroke        carCompanybmw 
    ##             8.905730             2.197549             1.253797 
    ##      carCompanybuick      carCompanydodge     carCompanyjaguar 
    ##             1.981983             1.160991             1.602484 
    ##      carCompanymazda carCompanymitsubishi     carCompanynissan 
    ##             1.514699             1.189719             1.261997 
    ##    carCompanypeugeot   carCompanyplymouth     carCompanysubaru 
    ##             1.629792             1.175645             1.685995 
    ##     carCompanytoyota        enginetypeohc      enginetyperotor 
    ##             1.438355             3.379184             2.093159

Remove carCompanydodge

``` r
model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanyplymouth + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_24)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanyplymouth + carCompanysubaru + 
    ##     carCompanytoyota + enginetypeohc + enginetyperotor, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3170.7 -1107.2   -66.9   919.8  4531.1 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -37521.943   8377.942  -4.479 1.67e-05 ***
    ## aspiration            -1944.328    406.901  -4.778 4.87e-06 ***
    ## enginelocation       -13808.185   1232.449 -11.204  < 2e-16 ***
    ## carwidth               1063.829    138.814   7.664 4.35e-12 ***
    ## enginesize               84.142      9.656   8.714 1.49e-14 ***
    ## stroke                -3957.984    651.645  -6.074 1.39e-08 ***
    ## carCompanybmw          7810.793   1065.089   7.333 2.47e-11 ***
    ## carCompanybuick        6055.753    888.190   6.818 3.51e-10 ***
    ## carCompanyjaguar       7928.127   1467.611   5.402 3.20e-07 ***
    ## carCompanymazda       -2222.281    604.533  -3.676 0.000350 ***
    ## carCompanymitsubishi  -2425.414    639.185  -3.795 0.000229 ***
    ## carCompanynissan      -1448.612    526.871  -2.749 0.006855 ** 
    ## carCompanypeugeot     -4330.046    871.193  -4.970 2.15e-06 ***
    ## carCompanyplymouth    -1629.649    729.522  -2.234 0.027270 *  
    ## carCompanysubaru      -6582.790    886.158  -7.428 1.50e-11 ***
    ## carCompanytoyota      -1947.508    411.443  -4.733 5.88e-06 ***
    ## enginetypeohc         -1781.039    562.398  -3.167 0.001937 ** 
    ## enginetyperotor        4741.472   1375.420   3.447 0.000772 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1636 on 125 degrees of freedom
    ## Multiple R-squared:  0.9647, Adjusted R-squared:  0.9599 
    ## F-statistic: 200.8 on 17 and 125 DF,  p-value: < 2.2e-16

``` r
vif(model_24)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.354115             1.665883             4.881217 
    ##           enginesize               stroke        carCompanybmw 
    ##             8.630690             2.190976             1.244165 
    ##      carCompanybuick     carCompanyjaguar      carCompanymazda 
    ##             1.961129             1.586092             1.500204 
    ## carCompanymitsubishi     carCompanynissan    carCompanypeugeot 
    ##             1.152215             1.225047             1.629138 
    ##   carCompanyplymouth     carCompanysubaru     carCompanytoyota 
    ##             1.142366             1.685585             1.384516 
    ##        enginetypeohc      enginetyperotor 
    ##             3.349983             2.074804

Remove carCompanyplymouth

``` r
model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_25)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanynissan + 
    ##     carCompanypeugeot + carCompanysubaru + carCompanytoyota + 
    ##     enginetypeohc + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3151.4 -1203.0  -109.0   963.2  4393.7 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -41347.637   8329.874  -4.964 2.19e-06 ***
    ## aspiration            -1854.112    411.253  -4.508 1.47e-05 ***
    ## enginelocation       -14110.773   1244.228 -11.341  < 2e-16 ***
    ## carwidth               1130.425    137.705   8.209 2.25e-13 ***
    ## enginesize               81.580      9.738   8.377 9.00e-14 ***
    ## stroke                -3991.178    661.710  -6.032 1.68e-08 ***
    ## carCompanybmw          8004.138   1078.244   7.423 1.50e-11 ***
    ## carCompanybuick        6164.314    900.793   6.843 3.01e-10 ***
    ## carCompanyjaguar       8130.484   1487.826   5.465 2.38e-07 ***
    ## carCompanymazda       -2059.649    609.562  -3.379 0.000969 ***
    ## carCompanymitsubishi  -2187.838    640.177  -3.418 0.000851 ***
    ## carCompanynissan      -1240.498    526.716  -2.355 0.020059 *  
    ## carCompanypeugeot     -4347.788    884.843  -4.914 2.72e-06 ***
    ## carCompanysubaru      -6484.816    898.976  -7.214 4.48e-11 ***
    ## carCompanytoyota      -1753.507    408.491  -4.293 3.49e-05 ***
    ## enginetypeohc         -1872.611    569.714  -3.287 0.001313 ** 
    ## enginetyperotor        4528.934   1393.682   3.250 0.001482 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1662 on 126 degrees of freedom
    ## Multiple R-squared:  0.9633, Adjusted R-squared:  0.9586 
    ## F-statistic: 206.5 on 16 and 126 DF,  p-value: < 2.2e-16

``` r
vif(model_25)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.340775             1.645759             4.656081 
    ##           enginesize               stroke        carCompanybmw 
    ##             8.508947             2.189836             1.235949 
    ##      carCompanybuick     carCompanyjaguar      carCompanymazda 
    ##             1.955258             1.580049             1.478446 
    ## carCompanymitsubishi     carCompanynissan    carCompanypeugeot 
    ##             1.120316             1.186744             1.629002 
    ##     carCompanysubaru     carCompanytoyota        enginetypeohc 
    ##             1.681456             1.322831             3.332185 
    ##      enginetyperotor 
    ##             2.064876

Remove carCompanynissan

``` r
model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_26)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanypeugeot + 
    ##     carCompanysubaru + carCompanytoyota + enginetypeohc + enginetyperotor, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3632.7 -1174.6  -107.7   791.9  4546.9 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -44350.145   8377.796  -5.294 5.10e-07 ***
    ## aspiration            -1927.589    417.343  -4.619 9.34e-06 ***
    ## enginelocation       -14622.886   1246.817 -11.728  < 2e-16 ***
    ## carwidth               1186.918    138.005   8.601 2.53e-14 ***
    ## enginesize               78.988      9.847   8.021 5.97e-13 ***
    ## stroke                -4023.291    673.308  -5.975 2.16e-08 ***
    ## carCompanybmw          8279.034   1090.927   7.589 6.05e-12 ***
    ## carCompanybuick        6366.546    912.601   6.976 1.49e-10 ***
    ## carCompanyjaguar       8589.433   1501.181   5.722 7.18e-08 ***
    ## carCompanymazda       -1833.590    612.638  -2.993 0.003321 ** 
    ## carCompanymitsubishi  -1949.745    643.360  -3.031 0.002959 ** 
    ## carCompanypeugeot     -4205.598    898.445  -4.681 7.21e-06 ***
    ## carCompanysubaru      -6209.326    907.149  -6.845 2.92e-10 ***
    ## carCompanytoyota      -1471.167    397.433  -3.702 0.000318 ***
    ## enginetypeohc         -1818.968    579.359  -3.140 0.002104 ** 
    ## enginetyperotor        4465.370   1418.145   3.149 0.002044 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1692 on 127 degrees of freedom
    ## Multiple R-squared:  0.9617, Adjusted R-squared:  0.9571 
    ## F-statistic: 212.3 on 15 and 127 DF,  p-value: < 2.2e-16

``` r
vif(model_26)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.333059             1.595495             4.514801 
    ##           enginesize               stroke        carCompanybmw 
    ##             8.400220             2.188907             1.221466 
    ##      carCompanybuick     carCompanyjaguar      carCompanymazda 
    ##             1.937491             1.552943             1.441788 
    ## carCompanymitsubishi    carCompanypeugeot     carCompanysubaru 
    ##             1.092378             1.621419             1.652988 
    ##     carCompanytoyota        enginetypeohc      enginetyperotor 
    ##             1.208900             3.326859             2.064102

Remove enginetypeohc

``` r
model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanymazda +
                  carCompanymitsubishi + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetyperotor, data = train[, -1])
summary(model_27)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanymazda + carCompanymitsubishi + carCompanypeugeot + 
    ##     carCompanysubaru + carCompanytoyota + enginetyperotor, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3699.2 -1131.2  -162.9   915.1  4309.6 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -37839.098   8393.202  -4.508 1.46e-05 ***
    ## aspiration            -1978.273    431.218  -4.588 1.05e-05 ***
    ## enginelocation       -14392.902   1287.007 -11.183  < 2e-16 ***
    ## carwidth               1073.197    137.697   7.794 1.96e-12 ***
    ## enginesize               96.861      8.308  11.658  < 2e-16 ***
    ## stroke                -4897.732    633.857  -7.727 2.81e-12 ***
    ## carCompanybmw          7238.502   1074.725   6.735 4.99e-10 ***
    ## carCompanybuick        5692.717    917.181   6.207 6.95e-09 ***
    ## carCompanyjaguar       8849.231   1549.890   5.710 7.50e-08 ***
    ## carCompanymazda       -1950.716    632.304  -3.085 0.002494 ** 
    ## carCompanymitsubishi  -2109.746    663.157  -3.181 0.001840 ** 
    ## carCompanypeugeot     -2785.508    802.692  -3.470 0.000709 ***
    ## carCompanysubaru      -5172.607    873.660  -5.921 2.77e-08 ***
    ## carCompanytoyota      -1479.181    410.945  -3.599 0.000454 ***
    ## enginetyperotor        6958.797   1214.879   5.728 6.89e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1749 on 128 degrees of freedom
    ## Multiple R-squared:  0.9587, Adjusted R-squared:  0.9542 
    ## F-statistic: 212.1 on 14 and 128 DF,  p-value: < 2.2e-16

``` r
vif(model_27)
```

    ##           aspiration       enginelocation             carwidth 
    ##             1.331065             1.589988             4.203792 
    ##           enginesize               stroke        carCompanybmw 
    ##             5.592867             1.814360             1.108734 
    ##      carCompanybuick     carCompanyjaguar      carCompanymazda 
    ##             1.830333             1.548225             1.436441 
    ## carCompanymitsubishi    carCompanypeugeot     carCompanysubaru 
    ##             1.085524             1.210468             1.433969 
    ##     carCompanytoyota      enginetyperotor 
    ##             1.208850             1.416767

Remove carCompanymitsubishi

``` r
model_28 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanypeugeot + 
                  carCompanysubaru + carCompanytoyota + 
                  enginetyperotor, data = train[, -1])
summary(model_28)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanypeugeot + carCompanysubaru + carCompanytoyota + 
    ##     enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -3493  -1185   -300   1041   4782 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -37973.457   8843.792  -4.294 3.41e-05 ***
    ## aspiration         -2031.004    448.984  -4.524 1.36e-05 ***
    ## enginelocation    -14563.707   1362.738 -10.687  < 2e-16 ***
    ## carwidth            1076.077    144.690   7.437 1.24e-11 ***
    ## enginesize            99.135      8.721  11.368  < 2e-16 ***
    ## stroke             -5049.910    668.872  -7.550 6.78e-12 ***
    ## carCompanybmw       7515.094   1137.158   6.609 9.08e-10 ***
    ## carCompanybuick     5832.976    970.466   6.010 1.75e-08 ***
    ## carCompanyjaguar    9054.492   1639.318   5.523 1.74e-07 ***
    ## carCompanypeugeot  -2475.062    847.116  -2.922  0.00411 ** 
    ## carCompanysubaru   -4856.814    921.779  -5.269 5.54e-07 ***
    ## carCompanytoyota   -1088.677    424.457  -2.565  0.01146 *  
    ## enginetyperotor     5512.925   1176.424   4.686 6.92e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1854 on 130 degrees of freedom
    ## Multiple R-squared:  0.9528, Adjusted R-squared:  0.9485 
    ## F-statistic: 218.9 on 12 and 130 DF,  p-value: < 2.2e-16

``` r
vif(model_28)
```

    ##        aspiration    enginelocation          carwidth        enginesize 
    ##          1.284076          1.586287          4.130389          5.482968 
    ##            stroke     carCompanybmw   carCompanybuick  carCompanyjaguar 
    ##          1.797842          1.104583          1.823495          1.541285 
    ## carCompanypeugeot  carCompanysubaru  carCompanytoyota   enginetyperotor 
    ##          1.199678          1.420473          1.147616          1.182181

Remove carCompanytoyota

``` r
model_29 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + carCompanypeugeot + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_29)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanypeugeot + carCompanysubaru + enginetyperotor, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4037.3 -1179.5  -410.2   981.5  5153.4 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -41743.364   8904.545  -4.688 6.83e-06 ***
    ## aspiration         -2144.830    456.199  -4.702 6.45e-06 ***
    ## enginelocation    -15248.662   1364.472 -11.176  < 2e-16 ***
    ## carwidth            1137.555    145.697   7.808 1.64e-12 ***
    ## enginesize            95.322      8.774  10.864  < 2e-16 ***
    ## stroke             -4833.534    677.511  -7.134 5.94e-11 ***
    ## carCompanybmw       7909.907   1150.431   6.876 2.28e-10 ***
    ## carCompanybuick     6080.132    986.016   6.166 8.06e-09 ***
    ## carCompanyjaguar    9425.915   1667.315   5.653 9.41e-08 ***
    ## carCompanypeugeot  -2339.616    863.282  -2.710  0.00763 ** 
    ## carCompanysubaru   -4445.083    926.819  -4.796 4.33e-06 ***
    ## enginetyperotor     5605.158   1200.650   4.668 7.41e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1893 on 131 degrees of freedom
    ## Multiple R-squared:  0.9505, Adjusted R-squared:  0.9463 
    ## F-statistic: 228.4 on 11 and 131 DF,  p-value: < 2.2e-16

``` r
vif(model_29)
```

    ##        aspiration    enginelocation          carwidth        enginesize 
    ##          1.271531          1.525368          4.017039          5.323586 
    ##            stroke     carCompanybmw   carCompanybuick  carCompanyjaguar 
    ##          1.769243          1.084343          1.805517          1.529258 
    ## carCompanypeugeot  carCompanysubaru   enginetyperotor 
    ##          1.195016          1.377393          1.181077

Remove carcompanyPeugeot

``` r
model_30 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_30)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     enginesize + stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanysubaru + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4058.9 -1218.9  -394.2   942.5  4850.9 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -35259.18    8780.81  -4.015 9.90e-05 ***
    ## aspiration        -2039.60     465.34  -4.383 2.37e-05 ***
    ## enginelocation   -15139.97    1396.28 -10.843  < 2e-16 ***
    ## carwidth           1017.62     142.11   7.161 5.04e-11 ***
    ## enginesize           98.33       8.91  11.035  < 2e-16 ***
    ## stroke            -4618.45     688.83  -6.705 5.35e-10 ***
    ## carCompanybmw      8073.50    1176.13   6.864 2.36e-10 ***
    ## carCompanybuick    6528.61     995.12   6.561 1.11e-09 ***
    ## carCompanyjaguar   9403.51    1706.89   5.509 1.82e-07 ***
    ## carCompanysubaru  -4234.38     945.49  -4.479 1.61e-05 ***
    ## enginetyperotor    5876.60    1224.88   4.798 4.27e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1938 on 132 degrees of freedom
    ## Multiple R-squared:  0.9477, Adjusted R-squared:  0.9437 
    ## F-statistic: 239.1 on 10 and 132 DF,  p-value: < 2.2e-16

``` r
vif(model_30)
```

    ##       aspiration   enginelocation         carwidth       enginesize 
    ##         1.262320         1.524050         3.646413         5.238605 
    ##           stroke    carCompanybmw  carCompanybuick carCompanyjaguar 
    ##         1.744966         1.081358         1.754662         1.529220 
    ## carCompanysubaru  enginetyperotor 
    ##         1.367700         1.172857

adjusted R-squared = 0.9437. carwidth, enginesize have high VIF. Lets see what happens when these are removed.

Removing carwidth

``` r
model_31a <- lm(formula = price ~ aspiration + enginelocation + 
                  enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31a)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + enginesize + 
    ##     stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanysubaru + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4969.4 -1523.0  -224.8  1120.6  7614.4 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       24939.294   2976.563   8.379 6.66e-14 ***
    ## aspiration        -3183.758    513.046  -6.206 6.44e-09 ***
    ## enginelocation   -10932.549   1486.884  -7.353 1.78e-11 ***
    ## enginesize          145.450      7.052  20.626  < 2e-16 ***
    ## stroke            -5325.793    800.250  -6.655 6.76e-10 ***
    ## carCompanybmw      7478.539   1377.201   5.430 2.59e-07 ***
    ## carCompanybuick    6673.224   1167.919   5.714 6.92e-08 ***
    ## carCompanyjaguar   7759.518   1985.499   3.908 0.000147 ***
    ## carCompanysubaru  -4947.812   1103.719  -4.483 1.57e-05 ***
    ## enginetyperotor    8475.982   1373.283   6.172 7.60e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2275 on 133 degrees of freedom
    ## Multiple R-squared:  0.9273, Adjusted R-squared:  0.9224 
    ## F-statistic: 188.6 on 9 and 133 DF,  p-value: < 2.2e-16

``` r
vif(model_31a)
```

    ##       aspiration   enginelocation       enginesize           stroke 
    ##         1.113491         1.254169         2.381095         1.709081 
    ##    carCompanybmw  carCompanybuick carCompanyjaguar carCompanysubaru 
    ##         1.075961         1.753940         1.501555         1.352514 
    ##  enginetyperotor 
    ##         1.069848

Adjusted R-squared decreased from 0.9437 to 0.9224, a decline of 2 percent.

Removing enginesize

``` r
model_31b <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31b)
```

    ## 
    ## Call:
    ## lm(formula = price ~ aspiration + enginelocation + carwidth + 
    ##     stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanysubaru + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -6439  -1678   -300   1237   9693 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -97023.2     9345.6 -10.382  < 2e-16 ***
    ## aspiration         -882.5      626.3  -1.409  0.16113    
    ## enginelocation   -23643.8     1608.4 -14.701  < 2e-16 ***
    ## carwidth           2175.8      132.3  16.441  < 2e-16 ***
    ## stroke            -3101.5      932.4  -3.327  0.00114 ** 
    ## carCompanybmw     10799.8     1588.4   6.799 3.23e-10 ***
    ## carCompanybuick   10987.9     1256.1   8.747 8.46e-15 ***
    ## carCompanyjaguar  16945.9     2160.5   7.844 1.27e-12 ***
    ## carCompanysubaru  -3438.6     1302.2  -2.641  0.00927 ** 
    ## enginetyperotor     838.7     1570.0   0.534  0.59410    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2677 on 133 degrees of freedom
    ## Multiple R-squared:  0.8994, Adjusted R-squared:  0.8926 
    ## F-statistic: 132.1 on 9 and 133 DF,  p-value: < 2.2e-16

``` r
vif(model_31b)
```

    ##       aspiration   enginelocation         carwidth           stroke 
    ##         1.198221         1.059815         1.657398         1.675469 
    ##    carCompanybmw  carCompanybuick carCompanyjaguar carCompanysubaru 
    ##         1.033643         1.465304         1.284013         1.359744 
    ##  enginetyperotor 
    ##         1.009923

Adjusted R-squared decreases from 0.9437 to 0.8926. These are fairly large decreases.

Remove variables which are comparatively less significant

Remove aspiration

``` r
model_31 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  carCompanysubaru +  
                  enginetyperotor, data = train[, -1])
summary(model_31)
```

    ## 
    ## Call:
    ## lm(formula = price ~ enginelocation + carwidth + enginesize + 
    ##     stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     carCompanysubaru + enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4335.3 -1334.4  -314.6  1036.1  5267.6 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -51435.671   8495.497  -6.054 1.35e-08 ***
    ## enginelocation   -15816.183   1479.687 -10.689  < 2e-16 ***
    ## carwidth           1231.491    142.315   8.653 1.44e-14 ***
    ## enginesize           89.525      9.256   9.672  < 2e-16 ***
    ## stroke            -3948.668    716.172  -5.514 1.76e-07 ***
    ## carCompanybmw      7909.503   1253.437   6.310 3.84e-09 ***
    ## carCompanybuick    6981.284   1055.331   6.615 8.28e-10 ***
    ## carCompanyjaguar   8808.511   1814.240   4.855 3.32e-06 ***
    ## carCompanysubaru  -3735.916   1000.823  -3.733 0.000280 ***
    ## enginetyperotor    5074.062   1291.376   3.929 0.000136 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2067 on 133 degrees of freedom
    ## Multiple R-squared:  0.9401, Adjusted R-squared:  0.936 
    ## F-statistic: 231.8 on 9 and 133 DF,  p-value: < 2.2e-16

``` r
vif(model_31)
```

    ##   enginelocation         carwidth       enginesize           stroke 
    ##         1.505443         3.216497         4.972596         1.659088 
    ##    carCompanybmw  carCompanybuick carCompanyjaguar carCompanysubaru 
    ##         1.080263         1.735762         1.519548         1.347913 
    ##  enginetyperotor 
    ##         1.146649

Remove carcompanysubaru

``` r
model_32 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar + 
                  enginetyperotor, data = train[, -1])
summary(model_32)
```

    ## 
    ## Call:
    ## lm(formula = price ~ enginelocation + carwidth + enginesize + 
    ##     stroke + carCompanybmw + carCompanybuick + carCompanyjaguar + 
    ##     enginetyperotor, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4419.6 -1290.5  -336.6  1065.1  5677.7 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -57320.19    8741.56  -6.557 1.09e-09 ***
    ## enginelocation   -16635.15    1532.33 -10.856  < 2e-16 ***
    ## carwidth           1268.00     148.67   8.529 2.76e-14 ***
    ## enginesize           87.77       9.68   9.067 1.33e-15 ***
    ## stroke            -2620.28     650.80  -4.026 9.44e-05 ***
    ## carCompanybmw      8301.30    1307.93   6.347 3.14e-09 ***
    ## carCompanybuick    6866.99    1104.62   6.217 6.01e-09 ***
    ## carCompanyjaguar   7884.91    1882.03   4.190 5.04e-05 ***
    ## enginetyperotor    5178.09    1351.95   3.830 0.000196 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2164 on 134 degrees of freedom
    ## Multiple R-squared:  0.9338, Adjusted R-squared:  0.9298 
    ## F-statistic: 236.2 on 8 and 134 DF,  p-value: < 2.2e-16

``` r
vif(model_32)
```

    ##   enginelocation         carwidth       enginesize           stroke 
    ##         1.472347         3.201307         4.959746         1.249447 
    ##    carCompanybmw  carCompanybuick carCompanyjaguar  enginetyperotor 
    ##         1.072688         1.734301         1.491285         1.146115

Remove enginetyperotor

``` r
model_33 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + stroke +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar, data = train[, -1])
summary(model_33)
```

    ## 
    ## Call:
    ## lm(formula = price ~ enginelocation + carwidth + enginesize + 
    ##     stroke + carCompanybmw + carCompanybuick + carCompanyjaguar, 
    ##     data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4546.2 -1330.1  -416.0   944.1  5923.2 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -64964.608   8931.154  -7.274 2.57e-11 ***
    ## enginelocation   -17653.599   1583.640 -11.147  < 2e-16 ***
    ## carwidth           1419.483    150.396   9.438  < 2e-16 ***
    ## enginesize           74.659      9.502   7.857 1.10e-12 ***
    ## stroke            -2502.659    682.199  -3.669  0.00035 ***
    ## carCompanybmw      8574.015   1370.513   6.256 4.86e-09 ***
    ## carCompanybuick    7324.647   1152.400   6.356 2.95e-09 ***
    ## carCompanyjaguar   8896.152   1955.487   4.549 1.19e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2271 on 135 degrees of freedom
    ## Multiple R-squared:  0.9265, Adjusted R-squared:  0.9227 
    ## F-statistic: 243.2 on 7 and 135 DF,  p-value: < 2.2e-16

``` r
vif(model_33)
```

    ##   enginelocation         carwidth       enginesize           stroke 
    ##         1.428010         2.974744         4.339698         1.246665 
    ##    carCompanybmw  carCompanybuick carCompanyjaguar 
    ##         1.069509         1.714007         1.461935

Remove stroke

``` r
model_34 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize +  
                  carCompanybmw + carCompanybuick + 
                  carCompanyjaguar, data = train[, -1])
summary(model_34)
```

    ## 
    ## Call:
    ## lm(formula = price ~ enginelocation + carwidth + enginesize + 
    ##     carCompanybmw + carCompanybuick + carCompanyjaguar, data = train[, 
    ##     -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4628.0 -1528.1  -421.2  1209.1  6693.9 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -72460.740   9083.747  -7.977 5.51e-13 ***
    ## enginelocation   -18973.597   1611.310 -11.775  < 2e-16 ***
    ## carwidth           1438.277    157.042   9.159 7.09e-16 ***
    ## enginesize           69.591      9.823   7.085 6.82e-11 ***
    ## carCompanybmw      9121.668   1423.389   6.408 2.23e-09 ***
    ## carCompanybuick    7240.155   1203.785   6.014 1.57e-08 ***
    ## carCompanyjaguar   7275.670   1990.280   3.656 0.000366 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2373 on 136 degrees of freedom
    ## Multiple R-squared:  0.9192, Adjusted R-squared:  0.9156 
    ## F-statistic: 257.9 on 6 and 136 DF,  p-value: < 2.2e-16

``` r
vif(model_34)
```

    ##   enginelocation         carwidth       enginesize    carCompanybmw 
    ##         1.354290         2.971293         4.247978         1.056820 
    ##  carCompanybuick carCompanyjaguar 
    ##         1.713323         1.387338

Remove carcompany jaguar

``` r
model_35 <- lm(formula = price ~  enginelocation + 
                  carwidth + enginesize + 
                  carCompanybmw + carCompanybuick, data = train[, -1])
summary(model_35)
```

    ## 
    ## Call:
    ## lm(formula = price ~ enginelocation + carwidth + enginesize + 
    ##     carCompanybmw + carCompanybuick, data = train[, -1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5207.4 -1499.5  -322.4  1258.5  6821.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -69563.79    9448.61  -7.362 1.52e-11 ***
    ## enginelocation  -17519.74    1630.39 -10.746  < 2e-16 ***
    ## carwidth          1342.90     161.70   8.305 8.58e-14 ***
    ## enginesize          86.62       9.03   9.592  < 2e-16 ***
    ## carCompanybmw     8415.83    1472.49   5.715 6.55e-08 ***
    ## carCompanybuick   5820.80    1189.75   4.892 2.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2478 on 137 degrees of freedom
    ## Multiple R-squared:  0.9113, Adjusted R-squared:  0.908 
    ## F-statistic: 281.4 on 5 and 137 DF,  p-value: < 2.2e-16

``` r
vif(model_35)
```

    ##  enginelocation        carwidth      enginesize   carCompanybmw 
    ##        1.271785        2.889275        3.293096        1.037373 
    ## carCompanybuick 
    ##        1.535081

Now there are 5 variables in the model.

Test the model on test dataset

``` r
Predict_1 <- predict(model_35,test[,-c(1,20)])
```

Add a new column "test\_predict" into the test dataset

``` r
test$test_price <- Predict_1
```

Calculate the test R2

``` r
cor(test$price,test$test_price)
```

    ## [1] 0.9267725

``` r
cor(test$price,test$test_price)^2
```

    ## [1] 0.8589072
