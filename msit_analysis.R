## Analyze the behavioral results from the MSIT task
library(ggplot2)
library(reshape)

# read in the demo data
pt_dframe=read.table("iba_trt_demo.csv",header=TRUE,sep=";")

## message the data to get it into the correct format
# first lets melt down the reaction times

## set up session 1 data
# first lets pull out the session 1 data and get it to what we want
sess1_dframe=pt_dframe[c("pid", "age", "sex", "sess1_scanner", "sess2_days_between_scans", "sess1_msit1_con_err",
    "sess1_msit1_incon_err", "sess1_msit1_con_rt", "sess1_msit1_incon_rt", "sess1_msit2_con_err", "sess1_msit2_incon_err", 
	"sess1_msit2_con_rt", "sess1_msit2_incon_rt")]

# melt down the different measures	
sess1_dframe=melt(sess1_dframe,id=c("pid","age","sex","sess1_scanner","sess2_days_between_scans"))

# now the string in the "variable" column is formatted as session_task_contrast_measure, lets split
# those into three seperate columns using a temporary dframe
df <- data.frame(do.call('rbind', strsplit(as.character(sess1_dframe$variable),'_',fixed=TRUE)))

# now drop the old variable column and bind in the four new columns
sess1_dframe = cbind(subset(sess1_dframe, select=-c(variable)),df)

# clean up the column labels
sess1_dframe = rename(sess1_dframe,c(sess1_scanner="Scanner", sess2_days_between_scans="ScanDelta", X1="Session", X2="Task", 
    X3="Condition", X4="Measure"))

## set up session 2 data
# first lets pull out the session 1 data and get it to what we want
sess2_dframe=pt_dframe[c("pid", "age", "sex", "sess2_scanner", "sess2_days_between_scans", "sess2_msit1_con_err",
    "sess2_msit1_incon_err", "sess2_msit1_con_rt", "sess2_msit1_incon_rt", "sess2_msit2_con_err", "sess2_msit2_incon_err", 
	"sess2_msit2_con_rt", "sess2_msit2_incon_rt")]

# melt down the different measures	
sess2_dframe=melt(sess2_dframe,id=c("pid","age","sex","sess2_scanner","sess2_days_between_scans"))

# now the string in the "variable" column is formatted as session_task_contrast_measure, lets split
# those into three seperate columns using a temporary dframe
df <- data.frame(do.call('rbind', strsplit(as.character(sess2_dframe$variable),'_',fixed=TRUE)))

# now drop the old variable column and bind in the four new columns
sess2_dframe = cbind(subset(sess2_dframe, select=-c(variable)),df)

# clean up the column labels
sess2_dframe = rename(sess2_dframe,c(sess2_scanner="Scanner", sess2_days_between_scans="ScanDelta", X1="Session", X2="Task", 
    X3="Condition", X4="Measure"))

	
## Combine the two session dframes to re-make the full dframe
pt_dframe=rbind(sess1_dframe, sess2_dframe)

# caste the dframe to break err and rt into seperate columns
pt_dframe=cast(pt_dframe,pid+age+sex+Scanner+ScanDelta+Session+Task+Condition ~ Measure)

# set ScanDelta == 0 for session 1
pt_dframe$ScanDelta[pt_dframe$Session=="sess1"]=0

# remove the rows that are not complete
pt_dframe=pt_dframe[complete.cases(pt_dframe),]

# renumber the rows
rownames(pt_dframe)<-1:nrow(pt_dframe)

# clean up column names
pt_dframe=rename(pt_dframe,c(pid="PID",age="Age",sex="Sex",err="Errors",rt="RT"))

# clean up the labels
pt_dframe$Session=factor(pt_dframe$Session,levels=c("sess1","sess2"), labels=c("Session 1","Session 2"))
pt_dframe$Task=factor(pt_dframe$Task,levels=c("msit1","msit2"), labels=c("MSIT 1","MSIT 2"))
pt_dframe$Condition=factor(pt_dframe$Condition,levels=c("con","incon"), labels=c("Congruent","Incongruent"))


## now lets work on some plots
ggplot(subset(pt_dframe,Session=="Session 1"),aes(x=Condition,y=RT))+geom_boxplot()+facet_grid(".~Task")+theme_bw()
	
# get the list of subjects that are only in session 2
sess2pid=unique(pt_dframe$PID[pt_dframe$Session=="Session 2"])

xx$sessbytask<-with(xx,interaction(Session,Task,sep= "\n"))
factor(xx$sessbytask,c("Session 1\nMSIT 1","Session 1\nMSIT 2","Session 2\nMSIT 1", "Session 2\nMSIT 2"))

ggplot(subset(pt_dframe,PID %in% sess2pid),aes(x=Condition,y=RT))+geom_boxplot()+facet_grid(".~Session + Task")+theme_bw()
ggplot(xx,aes(x=Condition,y=RT,fill=Session,colour=Task))+geom_boxplot(position=position_dodge(width=.85))+scale_fill_manual(values=c("orange","white"))+scale_colour_manual(values=c("brown","blue"))+theme_bw()

ggplot(xx,aes(x=factor(xx$sessbytask,c("Session 1 MSIT 1","Session 1 MSIT 2","Session 2 MSIT 1", "Session 2 MSIT 2")),y=RT,fill=Condition))+geom_boxplot(position=position_dodge(width=.85))+scale_fill_manual(values=c("orange","white"))+scale_colour_manual(values=c("brown","blue"))+theme_bw()

ggplot(xx,aes(x=factor(xx$sessbytask,c("Session 1\nMSIT 1","Session 1\nMSIT 2","Session 2\nMSIT 1", "Session 2\nMSIT 2")),,y=RT,fill=Condition))+geom_violin(position=position_dodge(width=.85))+geom_boxplot(width=.45,position=position_dodge(width=.85))+scale_fill_grey(start=0.9,end=1)+scale_colour_manual(values=c("brown","blue"))+theme_bw()
