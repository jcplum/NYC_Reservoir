library(rvest)
library(dplyr)

# J. Plummer
# 8/2/2019

date.now<- format(as.Date(Sys.Date()), "%B %d, %Y")

# get water levels page and scrape text
inpath<-"https://www1.nyc.gov/site/dep/water/reservoir-levels.page"
selector_names<-c("p")
doc.text <- inpath %>% read_html() %>% html_nodes(selector_names) %>% html_text()
# Parse text strings - only important lines have ":"
lvltab<-NULL
s<-gregexpr(":",doc.text)
date<-format(as.Date(doc.text[2], "%B %d, %Y"), "%m-%d-%Y")
for (i in 1:length(s)){
	if (!s[[i]][1]==-1){
		s2<-gregexpr("%",doc.text[i])
		name<-substr(doc.text[i],1,s[[i]][1])
		name<-sub(" Capacity:","",name)
		cap<-substr(doc.text[i],(s[[i]][1]+2),s2[[1]][1]-1)
		cur<-substr(doc.text[i],s[[i]][2]+2,nchar(doc.text[i]))	
		lvltab<-rbind(lvltab,c(date,name,cap,cur))
	}
}
# format output table
lvltab<-data.frame(lvltab)
lvltab[,3]<-sapply(lvltab[,3],function(x) gsub("B gallons","",x))
curcaps<-apply(lvltab[,c(3,4)],1,function(x) round(as.numeric(x[1])*(as.numeric(x[2])/100),1))
lvltab<-cbind(lvltab,curcaps)
colnames(lvltab)<-c("Date","Name","Cap.(B gals)","Level(Pct)","Level(B gals)")
lvltab[,3]<-as.numeric(as.character(lvltab[,3]))
lvltab[,4]<-as.numeric(as.character(lvltab[,4]))

# calculate total, current and % capacity
nycTcap<-sum(as.numeric(lvltab[,3]),na.rm=TRUE)
nycCcap<-sum(as.numeric(lvltab[,5]),na.rm=TRUE)
nycCcapPCT<-round((nycCcap/nycTcap)*100,1)

# subset the Delaware River Reservoirs and calulate total, current and % capacity
delres<-c("Cannonsville Reservoir","Neversink Reservoir","Pepacton Reservoir")
deltab<-filter(lvltab, Name %in% delres)
delTcap<-sum(as.numeric(deltab[,3]),na.rm=TRUE)
delCcap<-sum(as.numeric(deltab[,5]),na.rm=TRUE)
delCcapPCT<-round((delCcap/delTcap)*100,1)


# Reservoir Releases 

# get water releases page and scrape text
inpath<-"https://www1.nyc.gov/site/dep/water/release-levels.page"
selector_names<-c(".card-head",".card-mgd",".card-ntu")
reltab<-NULL
inpage<-inpath %>% read_html()
resd<-inpage %>% html_nodes("p") %>% html_text()
date<-format(as.Date(resd[1], "%B %d, %Y"), "%m-%d-%Y")
for (i in 1:length(selector_names)){
	res<-inpage %>% html_nodes(selector_names[i]) %>% html_text()
	if (i==3) res<-c(res[1],NA,NA,NA)
	if (i==4) res<-rep(res[1],dim(reltab)[2])
	reltab<-rbind(reltab,res)
}
# format output table
reltab<-t(reltab)
reltab<-cbind(reltab,rep(date,dim(reltab)[1]))
reltab<-data.frame(reltab[,c(4,1,2,3)])
colnames(reltab)<-c("Date","Name","Release(MGD)","Turbidity(ntu)")
reltab[,3]<-sapply(reltab[,3],function(x) gsub("mgd","",x))
reltab[,4]<-sapply(reltab[,4],function(x) gsub("Turbidity","",x))
reltab[,4]<-sapply(reltab[,4],function(x) gsub("ntu","",x))
reltab[,3]<-as.numeric(as.character(reltab[,3]))
reltab[,4]<-format(as.numeric(as.character(reltab[,4])),nsmall=2)

# reservoir levels table:
lvltab

# reservoir level totals
message(paste0("Total NYC reservoir system capacity (B gallons): ",nycTcap))
message(paste0("Current NYC Reservoir holdings (B gallons): ",nycCcap))
message(paste0("Current NYC Reservoir holdings: ",nycCcapPCT,"%"))

message(paste0("Delaware River Watershed includes Cannonsville, Pepacton, and Neversink Reservoirs. ")) 
message(paste0("Total Delaware River reservoir system capacity (B gallons): ",delTcap))
message(paste0("Current Delaware River reservoir holdings (B gallons): ",delCcap))
message(paste0("Current Delaware River reservoir holdings: ",delCcapPCT,"%"))

# reservoir levels table:
reltab


