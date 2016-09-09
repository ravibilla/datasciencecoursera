#####03_GETTING AND CLEANING DATA
####Week-2

###Reading from MySQL
library("RMySQL")
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show databases;"); dbDisconnect(ucscDb);
result

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

dbListFields(hg19, "affyU133Plus2")

dbGetQuery(hg19, "select count(*) from affyU133Plus2")

query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query); quantile(affyMis$misMatches)

affyMisSmall <- fetch(query, n=10); dbClearResult(query);
dim(affyMisSmall)
dbDisconnect(hg19)

###Reading from HDF5
##R HDF5 package
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
created = h5createFile("example.h5")
created

##Create groups
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")

##Write to groups
A = matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
B = array(seq(0.1,2.0,by=0.2), dim=c(5,2,2))
attr(B,"scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")

##Write a dataset
df = data.frame(1L:5L,seq(0,1,length.out=5), c("ab","cde","fghi","a","s"), stringsAsFactors = FALSE)
h5write(df,"example.h5","df")
h5ls("example.h5")

##Reading data
readA = h5read("example.h5","foo/A")
readB = h5read("example.h5","foo/foobaa/B")
readdf = h5read("example.h5","df")
readA
readB
readdf

##Writing and reading chunks
h5write(c(12,13,14), "example.h5","foo/A",index=list(1:3,1))
h5read("example.h5","foo/A")


###Reading from the Web
##Getting data off webpages - readLines()
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

##Parsing with XML
library("XML")
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes = T)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

##GET from the httr package
library(httr); html2 = GET(url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText = TRUE)
xpathSApply(parsedHtml, "//title",xmlValue)

##Accessing websites with passwords
pg1 = GET("http://httpbin.org/basic-auth/user/passwd")
pg1
pg2 = GET("http://httpbin.org/basic-auth/user/passwd", authenticate("user", "passwd"))
pg2
names(pg2)

##Using handles
google = handle("http://gopgle.com")
pg1 = GET(handle=google, path="/")
pg2 = GET(handle=google, path="search")

###Reading from APIs
##Accessing Twitter from R
myapp = oauth_app("twitter_api_app_ravibilla", key="xLdklySSQXPNCni6Ep0k1jIU9", secret="tuaaYGFh0C9gLZSSvUAdcmlthSP4gyaJrBOinVwNQoxMOlqDUq")
sig = sign_oauth1.0(myapp, token="8410402-QayN3DiFQLSZ52KcEKHFHMLqpTOUroZZOH1Kl4af0p", token_secret = "4vLwTGoCl8uO0UoixYtjc74Q6lI6dWca9NkWjsl5fh6lJ")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)

##Converting the json object
library(RJSONIO)
json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]


###Reading from Other Sources




















