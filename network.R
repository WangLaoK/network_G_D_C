library(networkD3)
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
links.df$color=str_replace_all(links.df$color,'AFFECTS','red')
links.df$color=str_replace_all(links.df$color,'ASSOCIATED_WITH','blue')
links.df$color=str_replace_all(links.df$color,'AUGMENTS','green')
links.df$color=str_replace_all(links.df$color,'CAUSES','black')
links.df$color=str_replace_all(links.df$color,'DISRUPTS','yellow')

table(links.df$color)
network <- forceNetwork(Links = links.df,#线性质数据框  
             Nodes = nodes.df,#节点性质数据框  
             Source = "source",#连线的源变量  
             Target = "target",#连线的目标变量  
             Value = "value",#连线的粗细值  
             NodeID = "name",#节点名称  
             Group = "group",#节点的分组  
             Nodesize = "size",#节点大小，节点数据框中  
             ###美化部分  
             fontFamily="宋体",#字体设置如"华文行楷" 等  
             fontSize = 20, #节点文本标签的数字字体大小（以像素为单位）。  
             linkColour=links.df$color,#连线颜色,black,red,blue,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),#colourScale ,linkWidth,#节点颜色,red，蓝色blue,cyan,yellow等  
             charge = -100,#数值表示节点排斥强度（负值）或吸引力（正值）    
             opacity = 0.9,  
             legend=T,#显示节点分组的颜色标签  
             arrows=T,#是否带方向  
             bounded=F,#是否启用限制图像的边框#opacityNoHover=1.0,#当鼠标悬停在其上时，节点标签文本的不透明度比例的数值  
             zoom = T)#允许放缩，双击放大
network
saveNetwork(network, 'Network_Chol.html', selfcontained = TRUE)




# Load igraph
library(igraph)

# Use igraph to make the graph and find membership
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)

# Convert to object suitable for networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)

# Create force directed network plot
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')
#Change target to number
# data$target=str_replace_all(data$target,'A','101')
# data$target=str_replace_all(data$target,'B','102')
# data$target=str_replace_all(data$target,'C','103')
# data$target=str_replace_all(data$target,'D','104')
# data$target=str_replace_all(data$target,'E','105')
# data$target=str_replace_all(data$target,'F','106')
# data$target=str_replace_all(data$target,'G','107')
# data$target=as.integer(data$target)
# 
# MisLinks=data.frame(source=as.vector(data[,1]),target=as.vector(data[,2]),value=as.vector(data[,3]))  
# MisNodes=data.frame(name=as.vector(data[,10]),group=as.vector(data[,5]),size=as.vector(data[,6])) 
#Use demo data
# data(MisLinks)
# data(MisNodes)
# 
# forceNetwork(Links = MisLinks,#线性质数据框  
#              Nodes = MisNodes,#节点性质数据框  
#              Source = "source",#连线的源变量  
#              Target = "target",#连线的目标变量  
#              Value = "value",#连线的粗细值  
#              NodeID = "name",#节点名称  
#              Group = "group",#节点的分组  
#              Nodesize = "size" ,#节点大小，节点数据框中  
#              ###美化部分  
#              fontFamily="宋体",#字体设置如"华文行楷" 等  
#              fontSize = 20, #节点文本标签的数字字体大小（以像素为单位）。  
#              linkColour="value",#连线颜色,black,red,blue,#colourScale ,linkWidth,#节点颜色,red，蓝色blue,cyan,yellow等  
#              charge = -100,#数值表示节点排斥强度（负值）或吸引力（正值）    
#              opacity = 0.9,  
#              legend=T,#显示节点分组的颜色标签  
#              arrows=T,#是否带方向  
#              bounded=F,#是否启用限制图像的边框#opacityNoHover=1.0,#当鼠标悬停在其上时，节点标签文本的不透明度比例的数值  
#              zoom = T)#允许放缩，双击放大
# 
# data=read.csv("simple_chol.txt",sep="\t",head=T)
# simpleNetwork(data,  
#               fontFamily="宋体",#字体设置如"华文行楷" 等  
#               fontSize = 20, #节点文本标签的数字字体大小（以像素为单位）。  
#               linkColour="black",#连线颜色,black,red,blue,    
#               nodeColour="black",#节点颜色,red，蓝色blue,cyan,yellow等  
#               charge = -300,#数值表示节点排斥强度（负值）或吸引力（正值）    
#               opacity=0.9,#透明度,1及以上为不透明，0为完全透明    
#               zoom=TRUE #可缩放  
# )
# 
# for(i in 1:nrow(data)){
# 	out=paste('{source: "',as.vector(data[i,10]),'", target: "',as.vector(data[i,4]),'", type: "',as.vector(data[i,9]),'"},
# ',sep="")
# 	cat(out,file="network_chol.txt",append=T)
# 	//cat('\n',file="network_chol.txt",append=T)
# }
# 
# {source: "Microsoft", target: "Amazon", type: "licensing"},
