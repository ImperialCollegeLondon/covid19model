df$country=df$group
df$group=as.character(df$group)
df$group[df$group=="Denmark" & df$colour=="Scenario 1"] = "Abs: Swe -> Dmk"
df$group[df$group=="Denmark" & df$colour=="Scenario 2"] = "Abs: UK -> Dmk"
df$group[df$group=="Denmark" & df$colour=="Scenario 3"] = "Rel: Swe -> Dmk"
df$group[df$group=="Denmark" & df$colour=="Scenario 4"] = "Rel: UK -> Dmk"
df$group[df$group=="Denmark" & df$colour=="Original"] = "Fit Dmk"

df$group[df$group=="Sweden" & df$colour=="Scenario 1"] = "Abs: UK -> Swe"
df$group[df$group=="Sweden" & df$colour=="Scenario 2"] = "Abs: Dmk -> Swe"
df$group[df$group=="Sweden" & df$colour=="Scenario 3"] = "Rel: UK -> Swe"
df$group[df$group=="Sweden" & df$colour=="Scenario 4"] = "Rel: Dmk -> Swe"
df$group[df$group=="Sweden" & df$colour=="Original"] = "Fit Swe"

df$group[df$group=="United_Kingdom" & df$colour=="Scenario 1"] = "Abs: Swe -> UK"
df$group[df$group=="United_Kingdom" & df$colour=="Scenario 2"] = "Abs: Dmk -> UK"
df$group[df$group=="United_Kingdom" & df$colour=="Scenario 3"] = "Rel: Swe -> UK"
df$group[df$group=="United_Kingdom" & df$colour=="Scenario 4"] = "Rel: Dmk -> UK"
df$group[df$group=="United_Kingdom" & df$colour=="Original"] = "Fit UK"

ordering<-c( 
"Abs: Swe -> Dmk",
"Rel: Swe -> Dmk" , 
"Fit Dmk",  
"Abs: UK -> Dmk",  
"Rel: UK -> Dmk",
"Abs: UK -> Swe",
"Rel: UK -> Swe",
"Fit Swe",
"Abs: Dmk -> Swe",
"Rel: Dmk -> Swe",
"Abs: Swe -> UK",
"Rel: Swe -> UK",
"Fit UK",
"Abs: Dmk -> UK",
"Rel: Dmk -> UK"
)
df$group=factor(df$group,levels=ordering)


df_cf1$country=df_cf1$group
df_cf1$group=as.character(df_cf1$group)
df_cf1$group[df_cf1$group=="Denmark" & df_cf1$colour=="Scenario 1"] = "Abs: Swe -> Dmk"
df_cf1$group[df_cf1$group=="Denmark" & df_cf1$colour=="Scenario 2"] = "Abs: UK -> Dmk"
df_cf1$group[df_cf1$group=="Denmark" & df_cf1$colour=="Scenario 3"] = "Rel: Swe -> Dmk"
df_cf1$group[df_cf1$group=="Denmark" & df_cf1$colour=="Scenario 4"] = "Rel: UK -> Dmk"
df_cf1$group[df_cf1$group=="Denmark" & df_cf1$colour=="Original"] = "Fit Dmk"

df_cf1$group[df_cf1$group=="Sweden" & df_cf1$colour=="Scenario 1"] = "Abs: UK -> Swe"
df_cf1$group[df_cf1$group=="Sweden" & df_cf1$colour=="Scenario 2"] = "Abs: Dmk -> Swe"
df_cf1$group[df_cf1$group=="Sweden" & df_cf1$colour=="Scenario 3"] = "Rel: UK -> Swe"
df_cf1$group[df_cf1$group=="Sweden" & df_cf1$colour=="Scenario 4"] = "Rel: Dmk -> Swe"
df_cf1$group[df_cf1$group=="Sweden" & df_cf1$colour=="Original"] = "Fit Swe"

df_cf1$group[df_cf1$group=="United_Kingdom" & df_cf1$colour=="Scenario 1"] = "Abs: Swe -> UK"
df_cf1$group[df_cf1$group=="United_Kingdom" & df_cf1$colour=="Scenario 2"] = "Abs: Dmk -> UK"
df_cf1$group[df_cf1$group=="United_Kingdom" & df_cf1$colour=="Scenario 3"] = "Rel: Swe -> UK"
df_cf1$group[df_cf1$group=="United_Kingdom" & df_cf1$colour=="Scenario 4"] = "Rel: Dmk -> UK"
df_cf1$group[df_cf1$group=="United_Kingdom" & df_cf1$colour=="Original"] = "Fit UK"
df_cf1$group=factor(df_cf1$group,levels=ordering)

df_cf2$country=df_cf2$group
df_cf2$group=as.character(df_cf2$group)
df_cf2$group[df_cf2$group=="Denmark" & df_cf2$colour=="Scenario 1"] = "Abs: Swe -> Dmk"
df_cf2$group[df_cf2$group=="Denmark" & df_cf2$colour=="Scenario 2"] = "Abs: UK -> Dmk"
df_cf2$group[df_cf2$group=="Denmark" & df_cf2$colour=="Scenario 3"] = "Rel: Swe -> Dmk"
df_cf2$group[df_cf2$group=="Denmark" & df_cf2$colour=="Scenario 4"] = "Rel: UK -> Dmk"
df_cf2$group[df_cf2$group=="Denmark" & df_cf2$colour=="Original"] = "Fit Dmk"

df_cf2$group[df_cf2$group=="Sweden" & df_cf2$colour=="Scenario 1"] = "Abs: UK -> Swe"
df_cf2$group[df_cf2$group=="Sweden" & df_cf2$colour=="Scenario 2"] = "Abs: Dmk -> Swe"
df_cf2$group[df_cf2$group=="Sweden" & df_cf2$colour=="Scenario 3"] = "Rel: UK -> Swe"
df_cf2$group[df_cf2$group=="Sweden" & df_cf2$colour=="Scenario 4"] = "Rel: Dmk -> Swe"
df_cf2$group[df_cf2$group=="Sweden" & df_cf2$colour=="Original"] = "Fit Swe"

df_cf2$group[df_cf2$group=="United_Kingdom" & df_cf2$colour=="Scenario 1"] = "Abs: Swe -> UK"
df_cf2$group[df_cf2$group=="United_Kingdom" & df_cf2$colour=="Scenario 2"] = "Abs: Dmk -> UK"
df_cf2$group[df_cf2$group=="United_Kingdom" & df_cf2$colour=="Scenario 3"] = "Rel: Swe -> UK"
df_cf2$group[df_cf2$group=="United_Kingdom" & df_cf2$colour=="Scenario 4"] = "Rel: Dmk -> UK"
df_cf2$group[df_cf2$group=="United_Kingdom" & df_cf2$colour=="Original"] = "Fit UK"
df_cf2$group=factor(df_cf2$group,levels=ordering)

df_cf3$country=df_cf3$group
df_cf3$group=as.character(df_cf3$group)
df_cf3$group[df_cf3$group=="Denmark" & df_cf3$colour=="Scenario 1"] = "Abs: Swe -> Dmk"
df_cf3$group[df_cf3$group=="Denmark" & df_cf3$colour=="Scenario 2"] = "Abs: UK -> Dmk"
df_cf3$group[df_cf3$group=="Denmark" & df_cf3$colour=="Scenario 3"] = "Rel: Swe -> Dmk"
df_cf3$group[df_cf3$group=="Denmark" & df_cf3$colour=="Scenario 4"] = "Rel: UK -> Dmk"
df_cf3$group[df_cf3$group=="Denmark" & df_cf3$colour=="Original"] = "Fit Dmk"

df_cf3$group[df_cf3$group=="Sweden" & df_cf3$colour=="Scenario 1"] = "Abs: UK -> Swe"
df_cf3$group[df_cf3$group=="Sweden" & df_cf3$colour=="Scenario 2"] = "Abs: Dmk -> Swe"
df_cf3$group[df_cf3$group=="Sweden" & df_cf3$colour=="Scenario 3"] = "Rel: UK -> Swe"
df_cf3$group[df_cf3$group=="Sweden" & df_cf3$colour=="Scenario 4"] = "Rel: Dmk -> Swe"
df_cf3$group[df_cf3$group=="Sweden" & df_cf3$colour=="Original"] = "Fit Swe"

df_cf3$group[df_cf3$group=="United_Kingdom" & df_cf3$colour=="Scenario 1"] = "Abs: Swe -> UK"
df_cf3$group[df_cf3$group=="United_Kingdom" & df_cf3$colour=="Scenario 2"] = "Abs: Dmk -> UK"
df_cf3$group[df_cf3$group=="United_Kingdom" & df_cf3$colour=="Scenario 3"] = "Rel: Swe -> UK"
df_cf3$group[df_cf3$group=="United_Kingdom" & df_cf3$colour=="Scenario 4"] = "Rel: Dmk -> UK"
df_cf3$group[df_cf3$group=="United_Kingdom" & df_cf3$colour=="Original"] = "Fit UK"
df_cf3$group=factor(df_cf3$group,levels=ordering)

df_cf4$country=df_cf4$group
df_cf4$group=as.character(df_cf4$group)
df_cf4$group[df_cf4$group=="Denmark" & df_cf4$colour=="Scenario 1"] = "Abs: Swe -> Dmk"
df_cf4$group[df_cf4$group=="Denmark" & df_cf4$colour=="Scenario 2"] = "Abs: UK -> Dmk"
df_cf4$group[df_cf4$group=="Denmark" & df_cf4$colour=="Scenario 3"] = "Rel: Swe -> Dmk"
df_cf4$group[df_cf4$group=="Denmark" & df_cf4$colour=="Scenario 4"] = "Rel: UK -> Dmk"
df_cf4$group[df_cf4$group=="Denmark" & df_cf4$colour=="Original"] = "Fit Dmk"

df_cf4$group[df_cf4$group=="Sweden" & df_cf4$colour=="Scenario 1"] = "Abs: UK -> Swe"
df_cf4$group[df_cf4$group=="Sweden" & df_cf4$colour=="Scenario 2"] = "Abs: Dmk -> Swe"
df_cf4$group[df_cf4$group=="Sweden" & df_cf4$colour=="Scenario 3"] = "Rel: UK -> Swe"
df_cf4$group[df_cf4$group=="Sweden" & df_cf4$colour=="Scenario 4"] = "Rel: Dmk -> Swe"
df_cf4$group[df_cf4$group=="Sweden" & df_cf4$colour=="Original"] = "Fit Swe"

df_cf4$group[df_cf4$group=="United_Kingdom" & df_cf4$colour=="Scenario 1"] = "Abs: Swe -> UK"
df_cf4$group[df_cf4$group=="United_Kingdom" & df_cf4$colour=="Scenario 2"] = "Abs: Dmk -> UK"
df_cf4$group[df_cf4$group=="United_Kingdom" & df_cf4$colour=="Scenario 3"] = "Rel: Swe -> UK"
df_cf4$group[df_cf4$group=="United_Kingdom" & df_cf4$colour=="Scenario 4"] = "Rel: Dmk -> UK"
df_cf4$group[df_cf4$group=="United_Kingdom" & df_cf4$colour=="Original"] = "Fit UK"
df_cf4$group=factor(df_cf4$group,levels=ordering)


# dataframe for additional lines
df_originals=df
df_originals$group=as.character(df_originals$group)
df_originals = df_originals[!df_originals$group%in%c("Fit Dmk","Fit Swe","Fit UK"),]
un=unique(df_originals$group)
df_originals$median[grep("-> Dmk",df_originals$group)] = df$median[grep("^Fit Dmk$",df$group)]
df_originals$median[grep("-> Swe",df_originals$group)] = df$median[grep("^Fit Swe$",df$group)]
df_originals$median[grep("-> UK",df_originals$group)] = df$median[grep("^Fit UK$",df$group)]
df_originals$group=as.factor(df_originals$group)


mxs=c(
  max(df$df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[3]]),
  max(df$deaths_ui[df$group==ordering[4]],df$deaths_ui[df$group==ordering[5]]),
  max(df$deaths_ui[df$group==ordering[4]],df$deaths_ui[df$group==ordering[5]]),

  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[8]]),
  max(df$deaths_ui[df$group==ordering[9]],df$deaths_ui[df$group==ordering[10]]),
  max(df$deaths_ui[df$group==ordering[9]],df$deaths_ui[df$group==ordering[10]]),  

  max(df$deaths_ui[df$group==ordering[11]],df$deaths_ui[df$group==ordering[12]]),
  max(df$deaths_ui[df$group==ordering[11]],df$deaths_ui[df$group==ordering[12]]),
  max(df$deaths_ui[df$group==ordering[13]]),
  max(df$deaths_ui[df$group==ordering[14]],df$deaths_ui[df$group==ordering[15]]),
  max(df$deaths_ui[df$group==ordering[14]],df$deaths_ui[df$group==ordering[15]])    
)
df_aux <- data.frame(date = rep(as.Date("01/03/2020",format="%d/%m/%Y"),length(ordering)),
                     max=mxs,
                     group=ordering,
                     country=c(rep("Denmark",5),rep("Sweden",5),rep("United_Kingdom",5)))
                 