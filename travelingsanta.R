travelingsanta <- function(maxcities=NULL)
{
    ptm <- proc.time() #start the timer
    
    library('TSP')
    #library('ggmap')
    #library('geosphere')
    #library('maptools')
    #library('maps')
    dfcities <- getcities()
    if(!is.null(maxcities))
    {
        dfcities <- dfcities[1:maxcities,]
    }
    distances <- getdistancematrix(dfcities)
    distances <- TSP::TSP(distances,labels=dfcities$cityname)
    methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion","arbitrary_insertion", "nn", "repetitive_nn", "two_opt")
    tours <- sapply(methods, FUN = function(m) TSP::solve_TSP(distances, method = m), simplify = FALSE)
    dotchart(sort(c(sapply(tours, tour_length))))
    
    shortestmethod <- names(sort(c(sapply(tours, tour_length))))[1]
    shortestdistance <- as.integer(sort(c(sapply(tours, tour_length))))[1]
    shortestpath <- unlist(tours[shortestmethod])
    
    dfplot <- data.frame()
    for (i in 1:length(shortestpath))
    {
        nextcityname <- gsub(paste0(shortestmethod,"."),"",names(shortestpath[shortestpath==i]))
        nextcitylat <- dfcities$latitude[dfcities$cityname == nextcityname]
        nextcitylong <- dfcities$longitude[dfcities$cityname == nextcityname]
        
        nextcity <- data.frame(city=nextcityname,
                               lat=nextcitylat,
                               long=nextcitylong)
        dfplot <- rbind(dfplot,nextcity)
    }
    nextcityname <- gsub(paste0(shortestmethod,"."),"",names(shortestpath[shortestpath==1]))
    nextcitylat <- dfcities$latitude[dfcities$cityname == nextcityname]
    nextcitylong <- dfcities$longitude[dfcities$cityname == nextcityname]
    
    nextcity <- data.frame(city=nextcityname,
                           lat=nextcitylat,
                           long=nextcitylong)
    dfplot <- rbind(dfplot,nextcity)    
    
    #map("world", fill=TRUE, col="white", bg="lightblue")
    #points(dfplot$long,dfplot$lat, col="red", pch=16)
    pushtodomo(df=dfplot,datasetname="Traveling Santa",createdataset=FALSE)

    print("Execution Time:")
    print(proc.time()-ptm)
    
    dfplot
}

getcities <- function()
{
    filepath <- 'http://gael-varoquaux.info/images/misc/cities.txt'
    df <- read.csv(filepath, sep="\t")
    names(df)[1] <- 'cityname'
    dfnorthpole <- data.frame(cityname="North Pole",longitude=0,latitude=80)
    df <- rbind(dfnorthpole, df)
    df <- df[!duplicated(df$cityname),]
    df
}

getdistancematrix <- function(dfcities)
{
    numcities <- nrow(dfcities)
    distances <- dist(dfcities$latitude)
    index <- 1
    for (i in 1:(numcities-1))
    {
        for (j in 1:(numcities-i))
        {
            p1 <- c(dfcities[i,"longitude"],dfcities[i,"latitude"])
            p2 <- c(dfcities[i+j,"longitude"],dfcities[i+j,"latitude"])
            distances[index] <- geosphere::distHaversine(p1,p2)
            index <- index + 1
        }
    }
    distances/1000
}

plot_path <- function(path)
{
    plot(as(USCA312_coords, "Spatial"), axes = TRUE)
    plot(USCA312_basemap, add = TRUE,  col = "gray")
    points(USCA312_coords, pch = 3, cex = 0.4,col = "red")
    
    path_line <- SpatialLines(list(Lines(list(Line(USCA312_coords[path,])),ID="1")))
    plot(path_line, add=TRUE,col="black")
    points(USCA312_coords[c(head(path,1), tail(pathj,1)),], pch=19, col = "black")
}

pushtodomo <- function(df, datasetname, api_key=NULL, organization="KonoAnalytics", createdataset=FALSE)
{
    #library("devtools")
    #install_github('konoanalytics/KonostdlibR')
    library("KonostdlibR")
    library("DomoR")
    
    if(is.null(api_key))
    {
        api_key <- as.character(KonostdlibR::getcredentials("Domo")$api_key)
    }
    DomoR::init(organization,api_key)
    if(createdataset)
    {
        DomoR::create(df, name=datasetname)
    }else
    {
        DataSetInfo <- DomoR::list_ds(name=datasetname)
        replace <- DomoR::replace_ds(DataSetInfo$id, df)
    }
}