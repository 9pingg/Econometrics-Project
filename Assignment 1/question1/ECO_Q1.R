# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.


path <- "/Users/vedan/Desktop/update_main.csv"
main<-read.csv(path)
path_tap <- "/Users/vedan/Desktop/tap.csv"
tap<-read.csv(path_tap)
#main['gdp'] = rep(NA, 70572)
#main['tap'] = rep(NA, 70572)
#main['beds']= rep(NA, 70572)

seq = 1:70572
seq2 = 1:35

for(i in seq){
  for(j in seq2){
    if(main[i,4] == tap[j,1] & main[i,6] == tap[j,2]){
        main[i,70] = tap[j,3]
    }
  }
}

path_a <- "/Users/vedan/Desktop/beds.csv"

beds<-read.csv(path_a)
main <- subset (main, select = -X)
df_11 = data.frame(main$index, main$year, main$district, main$v44, main$cropcategory,main$gdp, main$beds, main$tap)

for(i in seq){
  for(j in seq2){
    if(main[i,4] == beds[j,1])
      {
      main[i,72] = beds[j,2]
      }
  }
}

path_gdp <- "/Users/vedan/Desktop/gdp.csv"
gdp<-read.csv(path_gdp)

for(i in seq){
  for(j in seq2){
    if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2011)
    {
      main[i,71] = gdp[j,2]
    }
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2012)
    {
      main[i,71] = gdp[j,3]
    }
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2013)
    {
      main[i,71] = gdp[j,4]
    }
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2014)
    {
      main[i,71] = gdp[j,5]
    }
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2015)
    {
      main[i,71] = gdp[j,6]
    }
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2016)
    {
      main[i,71] = gdp[j,7]
    }
    
    else if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2017)
    {
      main[i,71] = gdp[j,8]
    }
    
    if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2018)
    {
      main[i,71] = gdp[j,9]
    }
    
    if(tolower(main[i,4]) == tolower(gdp[j,1]) &&  main[i,7] == 2019)
    {
      main[i,71] = gdp[j,10]
    }
  }
}

path <- "/Users/vedan/Desktop/update_main_1.csv"
write.csv(main, path)
main
