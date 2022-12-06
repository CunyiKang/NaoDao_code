#清除工作路径下的其他变量
rm(list = ls())
#加载pacman包，如无则下载
if (!require("pacman")) install.packages("pacman"); library(pacman)
#导入用到包
p_load(bruceR, dplyr)
#获取当前代码路径
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
#设置路径
setwd(curWD)
#打印当前路径
getwd()
#导入问卷数据
data_des=import("1668997467989/决策游戏实验_20221121102422_问卷调查.csv",encoding="UTF-8",header = TRUE)

#data_des表头更名为第一行
colnames(data_des)=data_des[1,]
#data_des删除第一行
data_des=data_des[-1,]
#data_des删除最后一列
data_des=data_des[,-80]
#data_des删除无用列
data_des<-subset(data_des,select = -c(序号,提交答卷时间,来源,来源详情,
                                      节点id,节点名称,节点id.1,节点名称.1,节点id.2,节点名称.2,
                                      节点id.3,节点名称.3,节点id.4,节点名称.4))

#排除0000数据
data_des<-filter(data_des, as.numeric(data_des$`1、请输入你的手机号码后4位？ （仅用于人员标记）`) !=0000 & 
                   as.numeric(data_des$`1、请输入你的手机号码后4位？ （仅用于人员标记）`) !=-999)


#筛选出答完数据
#筛选A组接纳数据
data_des_a<-filter(data_des,data_des$`1、我感到被排斥` !=-999 )
#筛选A组有效列
data_des_a=data_des_a[,1:36]
#增加A组的分组变量
data_des_a$group<-"A"
#筛选B组排斥数据
data_des_b<-filter(data_des,data_des$`1、我感到被排斥.1` !=-999 )
#筛选B组有效列
data_des_b=data_des_b[,-(4:32)]
#修改B组变量名
colnames(data_des_b)<-gsub(".1$","",colnames(data_des_b))

#交换B组前后得分数据，与A一致
a<-data_des_b[4:7]
data_des_b<-data_des_b[-c(4:7)]
data_des_b<-cbind(data_des_b,a)
#增加B组的分组变量
data_des_b$group<-"B"

#A组B组数据合并
data_total<-rbind(data_des_a,data_des_b)

#数据数值化
data_total[,c(4:31,35)]<-as.numeric(unlist(data_total[,c(4:31,35)]))

#反向计分反转
data_total[,c(4:6,11,17:21,25:27,29)]<-RECODE(unlist(data_total[,c(4:6,11,17:21,25:27,29)]),"1=5;2=4;3=3;4=2;5=1")

#剔除连续作答
# data_total$cons<-CONSEC(data_total,varrange ="1、我感到被排斥:26、我感到很悲伤" )
# data_total<-filter(data_total,data_total$cons<=7)

#信度分析
sink("output/Alpha.txt")
Alpha(data_total[,4:29],varrange = "1、我感到被排斥:26、我感到很悲伤")
sink()

#汇总分数
data_total$belonging<-rowMeans(data_total[,c(4:8)])
data_total$self_esteem<-rowMeans(data_total[,c(9:13)])
data_total$mean_exi<-rowMeans(data_total[,c(14:18)])
data_total$control<-rowMeans(data_total[,c(19:23)])

data_total$happy<-rowMeans(data_total[,c(24:25)])
data_total$hurt<-rowMeans(data_total[,c(26:27)])
data_total$sad<-rowMeans(data_total[,c(28:29)])

data_total$eff1<-data_total[,30]
data_total$eff2<-data_total[,31]
data_total$rate<-as.numeric(data_total[,32]) 

#变量重命名
colnames(data_total)[33]<-"phone"
colnames(data_total)[34]<-"sex"
colnames(data_total)[35]<-"age"
colnames(data_total)[36]<-"edu"

data_total<-data_total[,-c(4:32)]

#导入PsychoPy数据
#定位psychopy文件夹
files<-paste("1668997467989/PsychoPy/",dir("1668997467989/PsychoPy/"),sep="")
#创建表，存放psychopy汇总结果
data_psy_total<-data.frame(被试者记录ID=1:length(files))
#创建data_psy存放psychopy数据
data_psy<-list()


for(i in 1:length(files)){
  #将psychopy实验csv名称上的id提取到data表
  data_psy_total$被试者记录ID[i]=substr(files[i],31,48)
  #将数据导入data_psy
  data_psy<-c(data_psy,list(read.csv(files[[i]],encoding="UTF-8",header = TRUE)))
  #给data_psy数据绑定id
  data_psy[[i]]$id<-data_psy_total$被试者记录ID[i]
}

#循环处理psychopy数据
for(i in 1:length(files)) {
  #删除无用列和练习、启动行
  data_psy[[i]]<-data_psy[[i]][-c(31),-c(1:2,6,8:12,15:22)]
  #增加tf列
  data_psy[[i]]$tf<-data_psy[[i]]$popped
  #增加细化popped2列
  data_psy[[i]]$popped2<-data_psy[[i]]$popped
  #增加rep_swi列
  data_psy[[i]]$rep_swi<-data_psy[[i]]$popped
  #增加修改popped列
  for(j in 2:length(data_psy[[i]][,3])){
    if (data_psy[[i]]$popped[j]==data_psy[[i]]$popped[j-1]){
      data_psy[[i]]$rep_swi[j]<-"repeat"
    } else {
      data_psy[[i]]$rep_swi[j]<-"switch"
    }
  }
  #细化rep和swi
  for(j in 2:length(data_psy[[i]][,3])){
    if ((data_psy[[i]]$popped2[j]==TRUE) & (data_psy[[i]]$popped[j-1]==TRUE)){
      data_psy[[i]]$rep_swi2[j]<-"repeat_neg"
    } else if((data_psy[[i]]$popped2[j]==FALSE) & (data_psy[[i]]$popped[j-1]==FALSE)) {
      data_psy[[i]]$rep_swi2[j]<-"repeat_pos"
    } else if((data_psy[[i]]$popped2[j]==FALSE) & (data_psy[[i]]$popped[j-1]==TRUE)){
      data_psy[[i]]$rep_swi2[j]<-"switch_n2p"
    } else{
      data_psy[[i]]$rep_swi2[j]<-"switch_p2n"
    }
  }
  #第一个试次进行编辑
  data_psy[[i]]$rep_swi[1]<-"start"
  data_psy[[i]]$rep_swi2[1]<-"start"
  #删除popped列
  data_psy[[i]]<-data_psy[[i]][,-c(3)]
  
  #计算切换与否细化分组nPumps
  data_psy[[i]]$swi_p2n<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi2=="switch_p2n")$nPumps)
  data_psy[[i]]$swi_n2p<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi2=="switch_n2p")$nPumps)
  data_psy[[i]]$rep_pos<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi2=="repeat_pos")$nPumps)
  data_psy[[i]]$rep_neg<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi2=="repeat_neg")$nPumps)
  
  #计算气球未爆率
  data_psy[[i]]$rate_pos<-as.numeric(count(filter(data_psy[[i]],data_psy[[i]]$earnings !=0)))/30
  
  #计算平均按键次数
  data_psy[[i]]$n_Pumps<-mean(data_psy[[i]]$nPumps)
  
  #过滤非0数据
  data_psy_earn<-data_psy
  data_psy_earn[[i]]<-filter(data_psy[[i]],data_psy[[i]]$earnings !=0 )
  #计算风险倾向
  data_psy[[i]]$risk<-mean(data_psy_earn[[i]]$earnings)
  #计算总收益
  data_psy[[i]]$earn<-sum(data_psy_earn[[i]]$earnings)
  
  #计算气球颜色分组
  #修改ballonPic
  data_psy[[i]]$ballonPic<-gsub("Asset/stimuli/","",data_psy[[i]]$ballonPic)
  data_psy[[i]]$ballonPic<-gsub("Balloon.png","",data_psy[[i]]$ballonPic)
  #计算blue、red、green
  data_psy[[i]]$blue<-mean(filter(data_psy[[i]],data_psy[[i]]$ballonPic=="blue")$earnings)
  data_psy[[i]]$red<-mean(filter(data_psy[[i]],data_psy[[i]]$ballonPic=="red")$earnings)
  data_psy[[i]]$green<-mean(filter(data_psy[[i]],data_psy[[i]]$ballonPic=="green")$earnings)
  
  #计算切换与否分组
  data_psy[[i]]$swi<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi=="switch")$earnings)
  data_psy[[i]]$rep<-mean(filter(data_psy[[i]],data_psy[[i]]$rep_swi=="repeat")$earnings)
  
  #循环赋值
  for(j in 1:length(data_psy)){
  if (data_psy[[i]]$id[1]==data_psy_total$被试者记录ID[j]){
    #赋值risk给data_psy_total
    data_psy_total$risk[j]<-data_psy[[i]]$risk[1]
    #赋值earn给data_psy_total
    data_psy_total$earn[j]<-data_psy[[i]]$earn[1]
    
    #颜色
    #赋值blue、red、green
    data_psy_total$blue[j]<-data_psy[[i]]$blue[1]
    data_psy_total$red[j]<-data_psy[[i]]$red[1]
    data_psy_total$green[j]<-data_psy[[i]]$green[1]
    
    #切换与否
    data_psy_total$swi[j]<-data_psy[[i]]$swi[1]
    data_psy_total$rep[j]<-data_psy[[i]]$rep[1]
    
    #切换与否细化
    data_psy_total$swi_p2n[j]<-data_psy[[i]]$swi_p2n[1]
    data_psy_total$swi_n2p[j]<-data_psy[[i]]$swi_n2p[1]
    data_psy_total$rep_pos[j]<-data_psy[[i]]$rep_pos[1]
    data_psy_total$rep_neg[j]<-data_psy[[i]]$rep_neg[1]
    
    #气球未爆率
    data_psy_total$rate_pos[j]<-data_psy[[i]]$rate_pos[1]
    
    #平均按键次数
    data_psy_total$n_Pumps[j]<-data_psy[[i]]$n_Pumps[1]
    } 
  }   
}  

#合并psychpy数据和问卷数据
data_total$被试者记录ID<-gsub("\t","",data_total$被试者记录ID)
data_out<-merge(data_total,data_psy_total,by="被试者记录ID",
                #all=TRUE
                )
#性别变量赋值
data_out$sex[data_out$sex == '女']<- 0
data_out$sex[data_out$sex == '男']<- 1

#设置路径
setwd("output")

#剔除部分缺失值
data_out<-filter(data_out,!is.na(age))
data_out<-filter(data_out,data_out$risk != "NaN")

#导出数据
xlsx::write.xlsx(data_out,"data_out.xlsx")

#数理统计
#描述性统计
Describe(data_out[,c(6,9:31)], 
         plot=TRUE,
         upper.triangle = TRUE, 
         upper.smooth = "loess",
         file="desp.doc",
         plot.file = "desp.png",
         plot.width = 15,plot.height = 15,plot.dpi = 500
)

#分组输出
sink("des_sex.txt")
psych::describe.by(data_out[,c(6,9:31)],list(data_out$sex))
sink()
sink("des_group.txt")
psych::describe.by(data_out[,c(6,9:31)],list(data_out$group))
sink()



#频次统计
sink("Freq.txt")
Freq(data_out$sex)
Freq(data_out$edu)
Freq(data_out$group)
sink()

#相关分析
Corr(
  data_out[,c(6,9:31)],
  method = "pearson",
  p.adjust = "none",
  all.as.numeric = TRUE,
  digits = 2,
  nsmall = 2,
  file = "corr.doc",
  plot = TRUE,
  plot.range = c(-1, 1),
  plot.palette = NULL,
  plot.color.levels = 201,
  plot.file = "corr.png",
  plot.width = 8,
  plot.height = 6,
  plot.dpi = 500
)



#差异检验
#独立样本t检验
TTEST(data_out, y=c(colnames(data_out)[9:31]), x=c("group"),file="dep-t-group.doc")
TTEST(data_out, y=c(colnames(data_out)[9:31]), x=c("sex"),file="dep-t-sex.doc")

#配对样本t检验
TTEST(data_out, y=c("rep","swi"), paired=TRUE,file="pai-t-swirep.doc")
TTEST(data_out, y=c("swi_p2n","swi_n2p"), paired=TRUE,file="pai-t-swi.doc")
TTEST(data_out, y=c("rep_pos","rep_neg"), paired=TRUE,file="pai-t-rep.doc")
TTEST(data_out, y=c("blue","red"), paired=TRUE,file="pai-t-br.doc")
TTEST(data_out, y=c("blue","green"), paired=TRUE,file="pai-t-bg.doc")
TTEST(data_out, y=c("red","green"), paired=TRUE,file="pai-t-rg.doc")


#risk指标
#单因素方差分析
m=MANOVA(data_out, dv="risk", between=c("sex"),file="manova-sex_risk.doc")
m=MANOVA(data_out, dv="risk", between=c("group"),file="manova-group_risk.doc")
#多因素方差分析
m=MANOVA(data_out, dv="risk", between=c("group", "sex"),file="manova-group_sex_risk.doc")

png("group_risk.png")
emmip(m, ~ group | sex, CIs=TRUE)
dev.off()
png("sex_risk.png")
emmip(m, ~ sex | group, CIs=TRUE)
dev.off()
png("sex_group_risk.png")
emmip(m, sex ~ group, CIs=TRUE)
dev.off()
png("group_sex_risk.png")
emmip(m, group ~ sex, CIs=TRUE)
dev.off()

#n_Pumps指标
#单因素方差分析
m=MANOVA(data_out, dv="n_Pumps", between=c("sex"),file="manova-sex_nPumps.doc")
m=MANOVA(data_out, dv="n_Pumps", between=c("group"),file="manova-group_nPumps.doc")
#多因素方差分析
m=MANOVA(data_out, dv="n_Pumps", between=c("group", "sex"),file="manova-group_sex_nPumps.doc")

png("group_nPumps.png")
emmip(m, ~ group | sex, CIs=TRUE)
dev.off()
png("sex_nPumps.png")
emmip(m, ~ sex | group, CIs=TRUE)
dev.off()
png("sex_group_nPumps.png")
emmip(m, sex ~ group, CIs=TRUE)
dev.off()
png("group_sex_nPumps.png")
emmip(m, group ~ sex, CIs=TRUE)
dev.off()

#n_Pumps指标
#单因素方差分析
m=MANOVA(data_out, dv="rate_pos", between=c("sex"),file="manova-sex_rate.doc")
m=MANOVA(data_out, dv="rate_pos", between=c("group"),file="manova-group_rate.doc")

#多因素方差分析
m=MANOVA(data_out, dv="rate_pos", between=c("group", "sex"),file="manova-group_sex_rate.doc")


png("group_rate.png")
emmip(m, ~ group | sex, CIs=TRUE)
dev.off()
png("sex_rate.png")
emmip(m, ~ sex | group, CIs=TRUE)
dev.off()
png("sex_group_rate.png")
emmip(m, sex ~ group, CIs=TRUE)
dev.off()
png("group_sex_rate.png")
emmip(m, group ~ sex, CIs=TRUE)
dev.off()

#重复测量方差分析，变量重命名
colnames(data_out)[21]<-"Cblue"
colnames(data_out)[22]<-"Cred"
colnames(data_out)[23]<-"Cgreen"

colnames(data_out)[26]<-"Cswi_p2n"
colnames(data_out)[27]<-"Cswi_n2p"
colnames(data_out)[28]<-"Crep_pos"
colnames(data_out)[29]<-"Crep_neg"

m=MANOVA(data_out, dvs="Cswi_p2n:Crep_neg", dvs.pattern="C(.)",
       within=c("C"),file="manova-swirep.doc")

m=MANOVA(data_out, dvs="Cblue:Cgreen", dvs.pattern="C(.)",
         within=c("C"),file="manova-color.doc")


#回归分析
sink("regress.txt")
regress(risk ~ sex, data=data_out, robust=TRUE)
regress(risk ~ age, data=data_out, robust=TRUE)
regress(risk ~ group, data=data_out, robust=TRUE)
regress(risk ~ belonging, data=data_out, robust=TRUE)
regress(risk ~ self_esteem, data=data_out, robust=TRUE)
regress(risk ~ mean_exi, data=data_out, robust=TRUE)
regress(risk ~ control, data=data_out, robust=TRUE)
regress(risk ~ happy, data=data_out, robust=TRUE)
regress(risk ~ hurt, data=data_out, robust=TRUE)
regress(risk ~ sad, data=data_out, robust=TRUE)
regress(risk ~ eff1, data=data_out, robust=TRUE)
regress(risk ~ eff2, data=data_out, robust=TRUE)
regress(risk ~ rate, data=data_out, robust=TRUE)
sink()

#regress(risk ~ age+sex+belonging+hurt+self_esteem+mean_exi+control+happy+sad, data=data_out, robust=TRUE)




#todo
#反应时计算
#异常值筛选逻辑
#标准差极端值

#计算反应时
# for(j in 1:30){
#  for(m in 1:length(data_psy[[i]]$response.rt[j])){
#逗号拆分反应时
#    data_psy[[i]]$response.rt[j] <- as.character(unlist(strsplit(data_psy[[i]]$response.rt[j], split = ",")))
#删除开头[
#    data_psy[[i]]$response.rt[j][1]<- gsub("['[']","",data_psy[[i]]$response.rt[j][1])
#删除末尾]
#    data_psy[[i]]$response.rt[j][length(data_psy[[i]]$response.rt[j])]<- gsub("]","",data_psy[[i]]$response.rt[j][length(data_psy[[i]]$response.rt[j])])
#转化为数值型
#    data_psy[[i]]$response.rt[j]<-sum(as.numeric(rt[j]))
#    }

#}

#聚类
# #聚类分析
# #去处空值
# data_out <- na.omit(data_out)
# #数据标准化
# data_out <- scale(data_out[,-c(1:4,7:8)])
# 
# p_load(factoextra, cluster)
# #集群性评估，Hopkins<0.5高度聚合，n<sample即最大为23
# res <- get_clust_tendency(data_out, 90, graph = TRUE)
# res$hopkins_stat
# #0.66>0.5相对聚合
# 
# #可视化
# res$plot
# 
# #估计最优聚类数
# set.seed(123)
# ## Compute the gap statistic
# gap_stat <- clusGap(data_out, FUN = kmeans, nstart = 25, K.max = 10, B = 500) 
# # Plot the result
# fviz_gap_stat(gap_stat)
# #最优为3类
# 
# #kmeans聚类
# set.seed(123)
# km.res <- kmeans(data_out, 4, nstart = 25)
# head(km.res$cluster, 20)
# 
# #可视化
# fviz_cluster(km.res, data_out)
# 
# #检查聚类结果，sisi接近1聚类良好，接近-1表示不好
# sil <- silhouette(km.res$cluster, dist(data_out))
# rownames(sil) <- rownames(data_out)
# head(sil[, 1:3])
# #上一步结果如有负值结果不好，肉眼观察无，该步骤用于筛出负值
# neg_sil_index <- which(sil[, "sil_width"] < 0)
# sil[neg_sil_index, , drop = FALSE]
# 
# # 层次聚类
# res.hc <- eclust(data_out, "hclust") # compute hclust
# fviz_dend(res.hc, rect = TRUE) # dendrogam
# fviz_silhouette(res.hc) # silhouette plot
# fviz_cluster(res.hc) # scatter plot
# 
# fit_hc = hclust(dist(data))
# print(fit_hc)
# plot(fit_hc)
# 
# 
# # 确定聚类数
# fviz_gap_stat(res.km$gap_stat)





