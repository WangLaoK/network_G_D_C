#Library--------------
library(visNetwork)
options(stringsAsFactors = F)
#Demo data
# nodes <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json")
# 
# edges <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json")

#Chol---------------
chol_rdf <- read.delim('./drug/drug_chol.txt',sep = '\t')

#Make node
chol_node <- data.frame(name=c(unique(chol_rdf$drug),unique(chol_rdf$gene)),
                        group=c(rep('drug',length(unique(chol_rdf$drug))),
                                rep('gene',length(unique(chol_rdf$gene)))))
chol_node$id=0:(nrow(chol_node)-1)
chol_node$label=chol_node$name
chol_node_dic=chol_node
rownames(chol_node_dic)=chol_node_dic$name
#Make edge
chol_edge <- chol_rdf
chol_edge$from <- chol_node_dic[chol_edge$gene,'id']
chol_edge$to <- chol_node_dic[chol_edge$drug,'id']

#Make visnetwork to view
drug_chol_network<-visNetwork(chol_node, chol_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_chol_network
visSave(drug_chol_network, 
        file="drug_chol_network.html")
        #file = paste0(normalizePath('./drug_network/'),"drug_chol_network.html"))

#coca---------------

coca_rdf <- read.delim('./drug/drug_coca.txt',sep = '\t')

#Make node
coca_node <- data.frame(name=c(unique(coca_rdf$drug),unique(coca_rdf$gene)),
                        group=c(rep('drug',length(unique(coca_rdf$drug))),
                                rep('gene',length(unique(coca_rdf$gene)))))
coca_node$id=0:(nrow(coca_node)-1)
coca_node$label=coca_node$name
coca_node_dic=coca_node
rownames(coca_node_dic)=coca_node_dic$name
#Make edge
coca_edge <- coca_rdf
coca_edge$from <- coca_node_dic[coca_edge$gene,'id']
coca_edge$to <- coca_node_dic[coca_edge$drug,'id']

#Make visnetwork to view
drug_coca_network<-visNetwork(coca_node, coca_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_coca_network
visSave(drug_coca_network, 
        file = "drug_coca_network.html")

#esca---------------

esca_rdf <- read.delim('./drug/drug_esca.txt',sep = '\t')

#Make node
esca_node <- data.frame(name=c(unique(esca_rdf$drug),unique(esca_rdf$gene)),
                        group=c(rep('drug',length(unique(esca_rdf$drug))),
                                rep('gene',length(unique(esca_rdf$gene)))))
esca_node$id=0:(nrow(esca_node)-1)
esca_node$label=esca_node$name
esca_node_dic=esca_node
rownames(esca_node_dic)=esca_node_dic$name
#Make edge
esca_edge <- esca_rdf
esca_edge$from <- esca_node_dic[esca_edge$gene,'id']
esca_edge$to <- esca_node_dic[esca_edge$drug,'id']

#Make visnetwork to view
drug_esca_network<-visNetwork(esca_node, esca_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_esca_network
visSave(drug_esca_network, 
        file = "drug_esca_network.html")

#lihc---------------

lihc_rdf <- read.delim('./drug/drug_lihc.txt',sep = '\t')

#Make node
lihc_node <- data.frame(name=c(unique(lihc_rdf$drug),unique(lihc_rdf$gene)),
                        group=c(rep('drug',length(unique(lihc_rdf$drug))),
                                rep('gene',length(unique(lihc_rdf$gene)))))
lihc_node$id=0:(nrow(lihc_node)-1)
lihc_node$label=lihc_node$name
lihc_node_dic=lihc_node
rownames(lihc_node_dic)=lihc_node_dic$name
#Make edge
lihc_edge <- lihc_rdf
lihc_edge$from <- lihc_node_dic[lihc_edge$gene,'id']
lihc_edge$to <- lihc_node_dic[lihc_edge$drug,'id']

#Make visnetwork to view
drug_lihc_network<-visNetwork(lihc_node, lihc_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_lihc_network
visSave(drug_lihc_network, 
        file = "drug_lihc_network.html")


#paad---------------

paad_rdf <- read.delim('./drug/drug_paad.txt',sep = '\t')

#Make node
paad_node <- data.frame(name=c(unique(paad_rdf$drug),unique(paad_rdf$gene)),
                        group=c(rep('drug',length(unique(paad_rdf$drug))),
                                rep('gene',length(unique(paad_rdf$gene)))))
paad_node$id=0:(nrow(paad_node)-1)
paad_node$label=paad_node$name
paad_node_dic=paad_node
rownames(paad_node_dic)=paad_node_dic$name
#Make edge
paad_edge <- paad_rdf
paad_edge$from <- paad_node_dic[paad_edge$gene,'id']
paad_edge$to <- paad_node_dic[paad_edge$drug,'id']

#Make visnetwork to view
drug_paad_network<-visNetwork(paad_node, paad_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_paad_network
visSave(drug_paad_network, 
        file = "drug_paad_network.html")


#stad---------------

stad_rdf <- read.delim('./drug/drug_stad.txt',sep = '\t')

#Make node
stad_node <- data.frame(name=c(unique(stad_rdf$drug),unique(stad_rdf$gene)),
                        group=c(rep('drug',length(unique(stad_rdf$drug))),
                                rep('gene',length(unique(stad_rdf$gene)))))
stad_node$id=0:(nrow(stad_node)-1)
stad_node$label=stad_node$name
stad_node_dic=stad_node
rownames(stad_node_dic)=stad_node_dic$name
#Make edge
stad_edge <- stad_rdf
stad_edge$from <- stad_node_dic[stad_edge$gene,'id']
stad_edge$to <- stad_node_dic[stad_edge$drug,'id']

#Make visnetwork to view
drug_stad_network<-visNetwork(stad_node, stad_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_stad_network
visSave(drug_stad_network, 
        file = "drug_stad_network.html")

#all---------------
all_rdf <- read.delim('./drug/drug_all.txt',sep = '\t')

#Make node
all_node <- data.frame(name=c(unique(all_rdf$drug),unique(all_rdf$gene)),
                       group=c(rep('drug',length(unique(all_rdf$drug))),
                               rep('gene',length(unique(all_rdf$gene)))))
all_node$id=0:(nrow(all_node)-1)
all_node$label=all_node$name
all_node_dic=all_node
rownames(all_node_dic)=all_node_dic$name
#Make edge
all_edge <- all_rdf
all_edge$from <- all_node_dic[all_edge$gene,'id']
all_edge$to <- all_node_dic[all_edge$drug,'id']

#Make visnetwork to view
drug_all_network<-visNetwork(all_node, all_edge, height = "1000px", width = "100%")%>%
  #Add group with color
  visGroups(groupname = "drug", color = "#7F9E5A") %>%
  visGroups(groupname = "gene", color = "#FAEA77") %>%
  visPhysics(stabilization = T)%>%
  visLegend()
drug_all_network
visSave(drug_all_network, 
        file="drug_all_network.html")

