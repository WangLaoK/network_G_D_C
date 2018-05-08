library(visNetwork)
library(xlsx)
library(stringr)

library(ggraph)
library(igraph)
library(tidygraph)
library(tidyverse)
options(stringsAsFactors = F)




chol=read.xlsx('./semrep_result.xlsx',sheetName = 'chol')
coca=read.xlsx('./semrep_result.xlsx',sheetName = 'coca')
esca=read.xlsx('./semrep_result.xlsx',sheetName = 'esca')
lihc=read.xlsx('./semrep_result.xlsx',sheetName = 'lihc')
paad=read.xlsx('./semrep_result.xlsx',sheetName = 'paad')
stad=read.xlsx('./semrep_result.xlsx',sheetName = 'stad')

table(coca$P_name)


#Chol start
#Dynamic
#Nodes df
cancer=unique(chol$group)
gene=unique(chol$S_name)
chol_node=data.frame(name=c(cancer,gene),
                    group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                    size=10
)
chol_node$id=0:(nrow(chol_node)-1)
chol_node$label=chol_node$name
chol_node_dic=chol_node
rownames(chol_node_dic)=chol_node_dic$name
#Make edge
chol_edge <- chol
chol_edge$from <- chol_node_dic[chol$S_name,'id']
chol_edge$to <- chol_node_dic[chol$group,'id']
chol_edge$color <- chol_edge$P_name
chol_edge$value <- chol_edge$n
##Use relation for specifying the edge color
table(chol_edge$P_name)
chol_edge$color=str_replace_all(chol_edge$color,'AFFECTS','#EF476F')
chol_edge$color=str_replace_all(chol_edge$color,'ASSOCIATED_WITH','#06D6A0')
chol_edge$color=str_replace_all(chol_edge$color,'AUGMENTS','#BA5A31')
chol_edge$color=str_replace_all(chol_edge$color,'CAUSES','#118AB2')
chol_edge$color=str_replace_all(chol_edge$color,'DISRUPTS','#073B4C')
chol_edge$color=str_replace_all(chol_edge$color,'PREVENTS','#FAEA77')
chol_edge$color=str_replace_all(chol_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(chol_edge$color),
                     label = unique(chol_edge$P_name), 
                     arrows =rep("to", length(unique(chol_edge$color))))
#Make visnetwork to view
cancer_chol_network<-visNetwork(chol_node, chol_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
cancer_chol_network

visSave(cancer_chol_network, file = "chol_network.html")


#Static
# chol.st <- chol %>%
#   select(S_name,group,P_name,n) %>%
#   arrange(desc(n))
# mygraph <-  chol.st%>%
#   graph_from_data_frame()
# pdf('chol_network.pdf',height = 8.5 ,width = 11)
#  mygraph  %>%
#    ggraph(layout = "fr") +
#    geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
#    #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
#    #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
#    geom_node_point(size = 1) +
#    geom_node_text(aes(label = name))+
#    theme_void()
# dev.off()


chol_edge.df <- chol_edge %>%
  select(from,to,group,S_name,n,P_name)
colnames(chol_edge.df) <- c('from','to','cancer','gene','n','P_name')
chol_node.df <- chol_node %>%
  select(id,name,group)
mygraph <- graph_from_data_frame( chol_edge.df, vertices=chol_node.df)

mygraph_tidy <- mygraph %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(gene=node_is_source())%>%
  mutate(cancer=!node_is_source())%>%
  activate(edges) 
mygraph_tidy %>%
  ggraph(layout = 'kk') +
  geom_edge_link(aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_edge_fan() +
  geom_node_point(aes(filter=cancer),color='#F45B24',size =5)+
  geom_node_point(aes(filter=gene),color='#547B94',size =5)+
  #geom_node_point(aes(color = group),size =4) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"))+
  labs(title='Chol network')+
  #scale_color_identity()
  theme_graph()

#esca start
#Dynamic
#Nodes df
cancer=unique(esca$group)
gene=unique(esca$S_name)
esca_node=data.frame(name=c(cancer,gene),
                     group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                     size=10
)
esca_node$id=0:(nrow(esca_node)-1)
esca_node$label=esca_node$name
esca_node_dic=esca_node
rownames(esca_node_dic)=esca_node_dic$name
#Make edge
esca_edge <- esca
esca_edge$from <- esca_node_dic[esca$S_name,'id']
esca_edge$to <- esca_node_dic[esca$group,'id']
esca_edge$color <- esca_edge$P_name
esca_edge$value <- esca_edge$n
##Use relation for specifying the edge color
table(esca_edge$P_name)
esca_edge$color=str_replace_all(esca_edge$color,'AFFECTS','#EF476F')
esca_edge$color=str_replace_all(esca_edge$color,'ASSOCIATED_WITH','#06D6A0')
esca_edge$color=str_replace_all(esca_edge$color,'AUGMENTS','#BA5A31')
esca_edge$color=str_replace_all(esca_edge$color,'CAUSES','#118AB2')
esca_edge$color=str_replace_all(esca_edge$color,'DISRUPTS','#073B4C')
esca_edge$color=str_replace_all(esca_edge$color,'PREVENTS','#FAEA77')
esca_edge$color=str_replace_all(esca_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(esca_edge$color),
                     label = unique(esca_edge$P_name), 
                     arrows =rep("to", length(unique(esca_edge$color))))
#Make visnetwork to view
cancer_esca_network<-visNetwork(esca_node, esca_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
cancer_esca_network

visSave(cancer_esca_network, file = "esca_network.html")


#Static
esca.st <- esca %>%
  select(S_name,group,P_name,n) %>%
  arrange(desc(n))
mygraph <-  esca.st%>%
  graph_from_data_frame()
pdf('esca_network.pdf',height = 8.5 ,width = 11)
mygraph  %>%
  ggraph(layout = "fr") +
  geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
  #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name))+
  theme_void()
dev.off()


#coca start
#Dynamic
#Nodes df
cancer=unique(coca$group)
gene=unique(coca$S_name)
coca_node=data.frame(name=c(cancer,gene),
                     group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                     size=10
)
coca_node$id=0:(nrow(coca_node)-1)
coca_node$label=coca_node$name
coca_node_dic=coca_node
rownames(coca_node_dic)=coca_node_dic$name
#Make edge
coca_edge <- coca
coca_edge$from <- coca_node_dic[coca$S_name,'id']
coca_edge$to <- coca_node_dic[coca$group,'id']
coca_edge$color <- coca_edge$P_name
coca_edge$value <- coca_edge$n
##Use relation for specifying the edge color
table(coca_edge$P_name)
coca_edge$color=str_replace_all(coca_edge$color,'AFFECTS','#EF476F')
coca_edge$color=str_replace_all(coca_edge$color,'ASSOCIATED_WITH','#06D6A0')
coca_edge$color=str_replace_all(coca_edge$color,'AUGMENTS','#BA5A31')
coca_edge$color=str_replace_all(coca_edge$color,'CAUSES','#118AB2')
coca_edge$color=str_replace_all(coca_edge$color,'DISRUPTS','#073B4C')
coca_edge$color=str_replace_all(coca_edge$color,'PREVENTS','#FAEA77')
coca_edge$color=str_replace_all(coca_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(coca_edge$color),
                     label = unique(coca_edge$P_name), 
                     arrows =rep("to", length(unique(coca_edge$color))))
#Make visnetwork to view
cancer_coca_network<-visNetwork(coca_node, coca_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
cancer_coca_network

visSave(cancer_coca_network, file = "coca_network.html")


#Static
coca.st <- coca %>%
  select(S_name,group,P_name,n) %>%
  arrange(desc(n))
mygraph <-  coca.st%>%
  graph_from_data_frame()
pdf('coca_network.pdf',height = 8.5 ,width = 11)
mygraph  %>%
  ggraph(layout = "fr") +
  geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
  #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name))+
  theme_void()
dev.off()

#lihc start
#Dynamic
#Nodes df
cancer=unique(lihc$group)
gene=unique(lihc$S_name)
lihc_node=data.frame(name=c(cancer,gene),
                     group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                     size=10
)
lihc_node$id=0:(nrow(lihc_node)-1)
lihc_node$label=lihc_node$name
lihc_node_dic=lihc_node
rownames(lihc_node_dic)=lihc_node_dic$name
#Make edge
lihc_edge <- lihc
lihc_edge$from <- lihc_node_dic[lihc$S_name,'id']
lihc_edge$to <- lihc_node_dic[lihc$group,'id']
lihc_edge$color <- lihc_edge$P_name
lihc_edge$value <- lihc_edge$n
##Use relation for specifying the edge color
table(lihc_edge$P_name)
lihc_edge$color=str_replace_all(lihc_edge$color,'AFFECTS','#EF476F')
lihc_edge$color=str_replace_all(lihc_edge$color,'ASSOCIATED_WITH','#06D6A0')
lihc_edge$color=str_replace_all(lihc_edge$color,'AUGMENTS','#BA5A31')
lihc_edge$color=str_replace_all(lihc_edge$color,'CAUSES','#118AB2')
lihc_edge$color=str_replace_all(lihc_edge$color,'DISRUPTS','#073B4C')
lihc_edge$color=str_replace_all(lihc_edge$color,'PREVENTS','#FAEA77')
lihc_edge$color=str_replace_all(lihc_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(lihc_edge$color),
                     label = unique(lihc_edge$P_name), 
                     arrows =rep("to", length(unique(lihc_edge$color))))
#Make visnetwork to view
cancer_lihc_network<-visNetwork(lihc_node, lihc_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
#cancer_lihc_network

visSave(cancer_lihc_network, file = "lihc_network.html")


#Static
lihc.st <- lihc %>%
  select(S_name,group,P_name,n) %>%
  arrange(desc(n))
mygraph <-  lihc.st%>%
  graph_from_data_frame()
pdf('lihc_network.pdf',height = 8.5 ,width = 11)
mygraph  %>%
  ggraph(layout = "fr") +
  geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
  #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name))+
  theme_void()
dev.off()


#stad start
#Dynamic
#Nodes df
cancer=unique(stad$group)
gene=unique(stad$S_name)
stad_node=data.frame(name=c(cancer,gene),
                     group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                     size=10
)
stad_node$id=0:(nrow(stad_node)-1)
stad_node$label=stad_node$name
stad_node_dic=stad_node
rownames(stad_node_dic)=stad_node_dic$name
#Make edge
stad_edge <- stad
stad_edge$from <- stad_node_dic[stad$S_name,'id']
stad_edge$to <- stad_node_dic[stad$group,'id']
stad_edge$color <- stad_edge$P_name
stad_edge$value <- stad_edge$n
##Use relation for specifying the edge color
table(stad_edge$P_name)
stad_edge$color=str_replace_all(stad_edge$color,'AFFECTS','#EF476F')
stad_edge$color=str_replace_all(stad_edge$color,'ASSOCIATED_WITH','#06D6A0')
stad_edge$color=str_replace_all(stad_edge$color,'AUGMENTS','#BA5A31')
stad_edge$color=str_replace_all(stad_edge$color,'CAUSES','#118AB2')
stad_edge$color=str_replace_all(stad_edge$color,'DISRUPTS','#073B4C')
stad_edge$color=str_replace_all(stad_edge$color,'PREVENTS','#FAEA77')
stad_edge$color=str_replace_all(stad_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(stad_edge$color),
                     label = unique(stad_edge$P_name), 
                     arrows =rep("to", length(unique(stad_edge$color))))
#Make visnetwork to view
cancer_stad_network<-visNetwork(stad_node, stad_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
#cancer_stad_network

visSave(cancer_stad_network, file = "stad_network.html")


#Static
stad.st <- stad %>%
  select(S_name,group,P_name,n) %>%
  arrange(desc(n))
mygraph <-  stad.st%>%
  graph_from_data_frame()
pdf('stad_network.pdf',height = 8.5 ,width = 11)
mygraph  %>%
  ggraph(layout = "fr") +
  geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
  #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name))+
  theme_void()
dev.off()


#paad start
#Dynamic
#Nodes df
cancer=unique(paad$group)
gene=unique(paad$S_name)
paad_node=data.frame(name=c(cancer,gene),
                     group=c(rep('cancer',length(cancer)),rep('gene',length(gene))),
                     size=10
)
paad_node$id=0:(nrow(paad_node)-1)
paad_node$label=paad_node$name
paad_node_dic=paad_node
rownames(paad_node_dic)=paad_node_dic$name
#Make edge
paad_edge <- paad
paad_edge$from <- paad_node_dic[paad$S_name,'id']
paad_edge$to <- paad_node_dic[paad$group,'id']
paad_edge$color <- paad_edge$P_name
paad_edge$value <- paad_edge$n
##Use relation for specifying the edge color
table(paad_edge$P_name)
paad_edge$color=str_replace_all(paad_edge$color,'AFFECTS','#EF476F')
paad_edge$color=str_replace_all(paad_edge$color,'ASSOCIATED_WITH','#06D6A0')
paad_edge$color=str_replace_all(paad_edge$color,'AUGMENTS','#BA5A31')
paad_edge$color=str_replace_all(paad_edge$color,'CAUSES','#118AB2')
paad_edge$color=str_replace_all(paad_edge$color,'DISRUPTS','#073B4C')
paad_edge$color=str_replace_all(paad_edge$color,'PREVENTS','#FAEA77')
paad_edge$color=str_replace_all(paad_edge$color,'TREATS','#B7B3B3')

ledges <- data.frame(color = unique(paad_edge$color),
                     label = unique(paad_edge$P_name), 
                     arrows =rep("to", length(unique(paad_edge$color))))
#Make visnetwork to view
cancer_paad_network<-visNetwork(paad_node, paad_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "cancer", color = "#F45B24") %>%
  visGroups(groupname = "gene", color = "#547B94") %>%
  visPhysics(stabilization = T)%>%
  visLegend(addEdges = ledges)
#cancer_paad_network

visSave(cancer_paad_network, file = "paad_network.html")


#Static
paad.st <- paad %>%
  select(S_name,group,P_name,n) %>%
  arrange(desc(n))
mygraph <-  paad.st%>%
  graph_from_data_frame()
pdf('paad_network.pdf',height = 8.5 ,width = 11)
mygraph  %>%
  ggraph(layout = "fr") +
  geom_edge_link( aes(colour=P_name,edge_alpha = n, edge_width = n)) +
  #geom_node_point(aes(filter=cancer),color='#7F9E5A',size =5)+
  #geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
  geom_node_point(size = 1) +
  geom_node_text(aes(label = name))+
  theme_void()
dev.off()


# #make as tidyversion
# 
# chol_edge <- chol_edge %>%
#   select('from','to','n','P_name')
# mygraph <- graph_from_data_frame( chol_edge, vertices=chol_node)
# 
# 
# 
# pdf('chol_network.pdf')
# mygraph_tidy %>%
#   ggraph(layout = 'kk') +
#   geom_edge_link(edge_colour = "cyan4") +
#   #geom_edge_fan() +
#   geom_node_point(aes(filter=drug),color='#7F9E5A',size =5)+
#   geom_node_point(aes(filter=gene),color='#FAEA77',size =5)+
#   #geom_node_point(aes(color = group),size =4) +
#   geom_node_text(aes(label = name), repel = TRUE, 
#                  point.padding = unit(0.2, "lines"))+
#   #ggtitle('Drug and gene network')+
#   #scale_color_identity()
#   theme_graph()
# dev.off()
