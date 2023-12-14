
source("00-Libraries.R")

# 7895 total rows between January 2019 and November 28th 2023

# can only query 1000 lines at a time.
skips <- c(0,1000,2000,3000,4000,5000,6000,7000)

data = data.frame()

# url
url = 'https://api.fda.gov/food/enforcement.json?api_key=8MkTRDAhQc4gR4qoqncPUppXfXgxEC0MTijsWp2t&search=report_date:[20190101+TO+20231128]&limit=1000&skip='

# Loop through data
for(i in 1:length(skips)) {
    print(i)
    response = GET(paste0(url,skips[i]))
    
    content = fromJSON(rawToChar(response$content))
    
    res = content$results
    
    output = res %>% 
      select(-openfda) %>%
      mutate_all(list(~ifelse(.=="",NA,.)))
    
    data = rbind(data,output)
    
}

# save data
#write.csv(data, "FDA_Recall_Data-11-28-2023.csv")
