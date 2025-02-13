############# Author: Léa Enguehard/Stefan Kruse ############# 
########## Date: 24 January 2025 ##########
######## lea.enguehard@awi.de ########
##### Simple forward prediction


# 1:: individual tree information
# .. in individual files per transect
indifolder = "//data/Individual tree table/"
indi_files_all = list.files(indifolder,pattern="*.csv",full=TRUE)

# read all
indi_all = NULL
for(f_i in indi_files_all) {
  (transectname_i = gsub("Age_Predicted_labels_tree_","",gsub(indifolder,"",gsub("\\.csv","",f_i))))
  indi_file_i = read.csv(f_i)
  indi_all = rbind(indi_all, indi_file_i[,c("plotID","X","Y","Zmax","predicted_labels","age_est")])
}

# .. extract height~age relationship for later use to calculate growth per year
#with(indi_all, plot(Zmax~age_est)) 
# => not linear
# .. make lookup table
agelookup = NULL
for(group_i in 1:2) {
  agg_per_year = with(indi_all[indi_all$predicted_labels==group_i,], aggregate(Zmax, list(round(age_est,0)),mean))
  names(agg_per_year) = c("Age","Height")
  agg_per_year = cbind(agg_per_year,Group=group_i)
  str(agg_per_year)
  agelookup = rbind(agelookup, agg_per_year)
}
agelookup = agelookup[-which(is.infinite(agelookup$Age)),]

library(ggplot2)
ggplot(agelookup,aes(y=Height, x=Age, color=Group)) +
  geom_point()

# ..create gap free product for use
library(zoo)
# .. add NAs
ages1 = 1:max(agelookup[agelookup$Group==1,]$Age)
ages2 = 1:max(agelookup[agelookup$Group==2,]$Age)
agelookup = rbind(agelookup, data.frame(Age=ages1[which(!ages1 %in% agelookup[agelookup$Group==1,]$Age)], Height=NA,Group=1))
agelookup = rbind(agelookup, data.frame(Age=ages2[which(!ages2 %in% agelookup[agelookup$Group==2,]$Age)], Height=NA,Group=2))
agelookup = agelookup[order(agelookup$Age),]

# .. fill nas by simple linear interpolation
agelookup[agelookup$Group==1,]$Height <- na.approx(agelookup[agelookup$Group==1,]$Height,na.rm=FALSE)
agelookup[agelookup$Group==2,]$Height <- na.approx(agelookup[agelookup$Group==2,]$Height,na.rm=FALSE)




# 2:: patch coordinates and initial module assignment
# .. direct table with all
patch_all = read.csv("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/RD_0209/R_Lidar/results/classification/patch_table/abundance_patch_age/abundance_patch_age_all2.csv")
patch_all = patch_all[,c("plotID","Patch_No","Patch_Xcenter","Patch_Ycenter","Label1_below_5","Label1_5_12","Label1_above_12","Label2_below_5","Label2_5_12","Label2_above_12","modularity_class","Max_Age")]
# 3:: growth estimation and abundances per year
table(table(paste0(patch_all$plotID,"_",patch_all$Patch_No)))
patch_all$PatchID = paste0(patch_all$plotID,"_",patch_all$Patch_No)

# define reused static variables
max_agelookup_Age = c(max(agelookup[agelookup$Group==1,]$Age), max(agelookup[agelookup$Group==2,]$Age))

#Max_Age as comparison restriction

#### define apply function for each tree per df to use apply and parallel and/or make the for(patch_i loop parallel)
GROWTH = function(class=1, age=0,oldheight=1, year_addition=1) {
  if( max_agelookup_Age[class]>(age+year_addition) ) {
    newheight = agelookup[agelookup$Group==class & agelookup$Age==(age+year_addition),]$Height
    return(newheight)
  } else {
    print("tree reached maximum age function declaration!")
    return(oldheight)
  }
}

CALCABUNDANCE = function(allclassvector=c(1,1,1,1,1,1), allheightsvector=c(23,2,12,4,1,4)) {
  class_1 = allheightsvector[allclassvector==1]
  class_2 = allheightsvector[allclassvector==2]
  
  Label1_below_5 = length(which(class_1<5));
  Label1_5_12 = length(which((class_1>=5) & (class_1<=12)));
  Label1_above_12 = length(which(class_1>12));
  
  Label2_below_5 = length(which(class_2<5));
  Label2_5_12 = length(which((class_2>=5) & (class_2<=12)));
  Label2_above_12 = length(which(class_2>12));
  
  dfout = data.frame(Label1_below_5,Label1_5_12,Label1_above_12,Label2_below_5,Label2_5_12,Label2_above_12)
  if(sum(dfout)>0) {
    return(dfout/sum(dfout))
  } else {
    print("warning: no tree in data set")
    return(dfout)
  }
}



##### when parallel
library(foreach)
# install.packages(doParallel)
library(doParallel)
#Setup backend to use many processors
(totalCores = detectCores())

#Leave one core to avoid overload your computer
cluster <- makeCluster(totalCores[1]-5) 
registerDoParallel(cluster)

#stopCluster(cluster)

## warning takes long! parallelized ~3-6 hours
nextpatch_df_out_time = NULL  
nextpatch_df_out_time = foreach(patch_i = patch_all$PatchID, .combine=rbind) %dopar% {
  # for(patch_i in patch_all$PatchID) {
  # for(patch_i in patch_all$PatchID[1]) {
  print(round(100*which(patch_i==patch_all$PatchID)/length(patch_all$PatchID),2))
  
  # find patch bounding box
  ymin=patch_all[patch_all$PatchID==patch_i,]$Patch_Ycenter-10
  ymax=patch_all[patch_all$PatchID==patch_i,]$Patch_Ycenter+10
  xmin=patch_all[patch_all$PatchID==patch_i,]$Patch_Xcenter-10
  xmax=patch_all[patch_all$PatchID==patch_i,]$Patch_Xcenter+10
  # subset all trees
  indi_tree_patch = indi_all[indi_all$X>xmin & indi_all$X<=xmax & indi_all$Y>ymin & indi_all$Y<=ymax,] # did Lea used larger and/or equal
  print(paste0("Dimension individuals = ", dim(indi_tree_patch)[1]))
  
  dfout_print=NULL
  
  if(dim(indi_tree_patch)[1]>0) {
    # define max age
    max_age_patch = max(indi_tree_patch$age_est)
    
    # per patch inc functions
    year_addition = 0
    membership_switch_happend = FALSE
    while (membership_switch_happend == FALSE) {
      year_addition = year_addition + 10
      print(year_addition)
      # plus x per patch
      patch_i_new_height = apply(indi_tree_patch,1,function(x){
        #print(x);
        GROWTH(class=as.numeric(x["predicted_labels"]), age=round(as.numeric(x["age_est"]),0), oldheight=as.numeric(x["Zmax"]), year_addition=year_addition);
      } )
      # calc abundances and mean age
      patch_i_abund = CALCABUNDANCE(allclassvector = indi_tree_patch$predicted_labels, allheightsvector = patch_i_new_height)
      
      # find most closest patch at the + year and must be older (mean age) ... which is 95% similar if not then 
      # 4:: find closest matching abundance and if change of module record that per year
      nextpatch_i=NULL
      value_i=999
      if(length(which(patch_all[patch_all$PatchID!=patch_i,]$Max_Age>=(max_age_patch+year_addition)))>0) {#patchesolder are availble 
        compdf = rbind(patch_i_abund,patch_all[(patch_all$Max_Age>=(max_age_patch+year_addition)) & (patch_all$PatchID!=patch_i),names(patch_i_abund)])
        dists_i = as.matrix(dist(compdf,diag=FALSE,upper=TRUE))[1,]
        maxval_i_pos = names(which.min(dists_i[-1]))# exclude first newheight abund
        value_i = dists_i[maxval_i_pos]
        nextpatch_i = patch_all[row.names(patch_all) == maxval_i_pos,]
      } else {# only copy the same patch as it stays		
        nextpatch_i = patch_all[patch_all$PatchID==patch_i,]
      }
      # membership from
      #from_module = patch_all[patch_all$PatchID==patch_i,]$modularity_class
      # ... to
      #to_module = nextpatch_i$modularity_class
      
      
      # return PatchID
      dfout_print = rbind(dfout_print, data.frame(YearAdd=year_addition,FromPatch=patch_i,nextpatch_i,patch_i_abund,abundance_distance=value_i))
      
      if( year_addition >= 500 ) {
        membership_switch_happend=TRUE
        
        # record years to next step and name of next patch
      }
    } 
  } else {
    print("no trees present!")
  }
  
  # for parralel needs extra for rbind
  dfout_print
  
}
str(nextpatch_df_out_time)

#setwd("//dmawi/potsdam/data/bioing/user/lenguehard/Project 2/Prediction")
if(FALSE) {# if to not by accident run again!
  write.csv2(nextpatch_df_out_time,"nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.csv",row.names=FALSE)
}





# Visualization 
nextpatch_df_out_time = read.csv2("nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.csv")

vall = lapply(unique(nextpatch_df_out_time$FromPatch), function(patchid) {
  mintimeto2 = min(nextpatch_df_out_time[(nextpatch_df_out_time$FromPatch==patchid) & (nextpatch_df_out_time$modularity_class==2),]$YearAdd);
  return(mintimeto2)
})
pall = lapply(unique(nextpatch_df_out_time$FromPatch), function(patchid) {
  modclass = patch_all[patch_all$PatchID==patchid,]$modularity_class;
  return(modclass)
})
vall_v = unlist(vall)
pall_v = unlist(pall)
changedf = data.frame(PatchID = unique(nextpatch_df_out_time$FromPatch), TimeTo2=vall_v, StartModule=pall_v)
if(FALSE) {
  write.csv2(changedf, "nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.changedf.csv",row.names=FALSE)
} else {
  changedf= read.csv2("nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.changedf.csv")
}
str(changedf)

ggplot(changedf[changedf$StartModule!=2,], aes(x=TimeTo2,group=StartModule, col=as.factor(StartModule))) +
  geom_density(linewidth=2)
# this plot shows when patches transit over to the module class 2 (=="disturbed")


### sankey diagram
# simplify data frame
sankeyall = lapply(unique(nextpatch_df_out_time$FromPatch), function(patchid) {
  frommodule = patch_all[patch_all$PatchID==patchid,]$modularity_class
  tomodules = nextpatch_df_out_time[(nextpatch_df_out_time$FromPatch==patchid),]$modularity_class
  times = nextpatch_df_out_time[(nextpatch_df_out_time$FromPatch==patchid),]$YearAdd
  
  return(data.frame(PatchID=patchid,Time = c(0,times), Module=c(frommodule,tomodules)))
})
sankeyall_df <- do.call(rbind.data.frame, sankeyall)
if(FALSE) { 
  write.csv2(sankeyall_df, "nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.sankeyall_df.csv", row.names=FALSE)
} else {
  sankeyall_df = read.csv2("nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.sankeyall_df.csv")
}
str(sankeyall_df)

# transform and aggregate for plotting
starttime=seq(0,490,10)
edge_id_iter=0
sankeyli = lapply(starttime, function(timestart){
  #timestart=0
  from_i = sankeyall_df[sankeyall_df$Time==timestart,]
  to_i = sankeyall_df[sankeyall_df$Time==(timestart+10),]
  # tstart = as.matrix(table(factor(from_i$Module,levels=c(1:5))))
  df_ii = NULL
  for(i in unique(to_i$Module)){
    # i=1
    amount_i = dim(to_i[to_i$Module==i,])[1]
    fromtable_i = as.matrix(
      table(
        factor(from_i[from_i$PatchID%in%to_i[to_i$Module==i,]$PatchID,]$Module,levels=c(1:5)))
    )
    
    for(modulelistiter in 1:5){
      if(fromtable_i[modulelistiter,1]>0) {
        edge_id_iter <<- edge_id_iter+1
        df_ii = rbind(df_ii, 
                      data.frame(
                        edge_id=c(edge_id_iter,edge_id_iter),
                        RCSES=c(fromtable_i[modulelistiter,1],fromtable_i[modulelistiter,1]),
                        node=c(modulelistiter,i),
                        stage=c(timestart,timestart+10),
                        connector=c("from","to")
                      ))
      }
    }
    
  }
  return(df_ii)
})
sankeyli_df <- do.call(rbind.data.frame, sankeyli)
if(FALSE) { 
  write.csv2(sankeyli_df, "nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.sankeyli_df.csv", row.names=FALSE)
} else {
  sankeyli_df = read.csv2("nextpatch_df_out_time_10yrsteps_5percsimil_2025-01-20_recall_010-500_maxage.sankeyli_df.csv")
}
str(sankeyli_df)


# Plot figure 7: Sankey diagram
palette <- c("1" = "#BF6E5E","2" = "#81B29A","3" = "#F2CC8F" , "4" = "#6AABD2", "5" = "#52567A")

library(ggplot2)
# sankey diagram for group membership shifts
library(ggsankeyfier)

maxyear=120

sankeyli_df$node = factor(sankeyli_df$node,levels=1:5,ordered=TRUE)
sankeyli_df$stage = factor(sankeyli_df$stage,levels=seq(0,500,10),ordered=TRUE)

ggplot(sankeyli_df[as.numeric(as.character(sankeyli_df$stage))<=maxyear,],aes(x=stage, y=RCSES, group=node, connector=connector, edge_id=edge_id, fill=node)) +
  geom_sankeyedge(v_space = "auto") +
  geom_sankeynode(v_space = "auto") +
  labs(
    title = "",
    x = "Years",  
    y = "Number of forest patches", 
  ) +
  scale_fill_manual(values = palette, name = "Successional\nStage") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14 ),
    legend.title = element_text(size = 14 )
    #panel.grid = element_blank(),  # Remove gridlines
  ) 





