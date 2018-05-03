nodes <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json")

edges <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json")
# devtools::install_github("datastorm-open/visNetwork")
library(visNetwork)

library(stringr)


data=read.csv("force_chol.txt",sep="\t",head=T,stringsAsFactors = F)

#To make a new Links and Nodes dataframe
#Nodes df
cancer=unique(data$cancer)
gene=unique(data$gene)
nodes.df=data.frame(name=c(cancer,gene),
                    group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                    size=10
)
links.df=data.frame(source=as.vector(data[,1]+length(cancer)),
                    target=as.vector(data[,2]),
                    value=as.vector(data[,7]),
                    color=as.vector(data[,9]))
#Change target to number
links.df$target=str_replace_all(links.df$target,'A','1')
links.df$target=str_replace_all(links.df$target,'B','2')
links.df$target=str_replace_all(links.df$target,'C','3')
links.df$target=str_replace_all(links.df$target,'D','4')
links.df$target=str_replace_all(links.df$target,'E','5')
links.df$target=str_replace_all(links.df$target,'F','6')
links.df$target=str_replace_all(links.df$target,'G','7')
#Target and source should be started with zero
#JS index is 0 like python | R is 1 started
links.df$target=as.integer(links.df$target)-1
links.df$source=links.df$source-1

#Use relation for specifying the edge color
table(data$relation)
links.df$color=str_replace_all(links.df$color,'AFFECTS','#EF476F')
links.df$color=str_replace_all(links.df$color,'ASSOCIATED_WITH','#06D6A0')
links.df$color=str_replace_all(links.df$color,'AUGMENTS','#BA5A31')
links.df$color=str_replace_all(links.df$color,'CAUSES','#118AB2')
links.df$color=str_replace_all(links.df$color,'DISRUPTS','#073B4C')

nodes.df$id=0:(nrow(nodes.df)-1)
colnames(nodes.df)=c('label','group','size','id')

colnames(links.df)=c("from" ,"to" ,"value" ,"color")

#Add label for edges in legend
ledges <- data.frame(color = unique(links.df$color),
                     label = unique(data$relation), 
                     arrows =rep("to", length(unique(links.df$color))))


visnetwork<-visNetwork(nodes.df, links.df, height = "1000px", width = "100%") %>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
visnetwork
visSave(visnetwork, file = "network.html")


#shiny::runApp(system.file("shiny", package = "visNetwork"))
