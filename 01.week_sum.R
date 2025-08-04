# ==============================================================================
# 功能：根据atimelogger导出的时间记录绘制每周分类时长图、时间轴，用于周复盘，适合A5打印
# 作者：咚咚

# 未解决的bug：1.纵式时间轴的备注信息如没有断点（空格、换行符,中文标点不算断点）则不能自动换行，
#              只能暂时把标点替换为空格，但还是不能解决长词组不能换行的问题
#             只能从源头处避免在备注中使用太长的词组 
#              2.如果时间文件中有重叠的记录，可能会影响排版
# ==============================================================================

library(tidyverse)
library(lubridate)
library(patchwork)
library(ggtext)
Sys.setlocale("LC_TIME", "English")  # 保证正常的星期为英文,若想显示中文星期，则不运行

# ===================================导入文件===================================
# 需填参数：atimelogger导出的文件名、目标颜色文件
df_rawdata <- read_csv("report-example.csv")
df_goalcolor <- read_csv("goalcolor-example.csv")

# ===================================数据清洗===================================
clean_rawdata <- function(){
  df_cleandata <- df_rawdata %>%
    set_names(c("type", "duration", "start", "end", "note", "tag")) %>%
    filter(end != "") %>% 
    mutate(
      start = str_replace_all(start,c("上午" = "AM", "下午" = "PM")),
      start = ymd_hms(start),  
      end = str_replace_all(end,c("上午" = "AM", "下午" = "PM")),
      end = ymd_hms(end))
  # 处理跨越两天的记录，生成时间，星期等
  df_ceiling <- df_cleandata %>%  
    filter(as.Date(start)!=as.Date(end)) %>%
    mutate(start = as.POSIXct(ceiling_date(start,"day")))
  df_floor <- df_cleandata %>%  
    filter(as.Date(start)!=as.Date(end)) %>% 
    mutate(end = as.POSIXct(floor_date(end,"day")-1))
  df_cleandata <- df_cleandata %>%
    filter(as.Date(start)==as.Date(end)) %>% 
    bind_rows(df_ceiling)  %>% 
    bind_rows(df_floor)  %>% 
    mutate(
      date = as.Date(start),
      day = wday(start,label = TRUE,abbr = TRUE,week_start = 1),
      duration = as.duration(end - start),
      weekstart = floor_date(date, "week", week_start = 1))%>% 
    arrange(start) 
  return(df_cleandata)
}
df_cleandata <- clean_rawdata()

# ===================================分类汇总===================================
cal_weeksum <- function(){
  df_weeksum <- df_cleandata %>%
    group_by(weekstart,type) %>%
    summarise(weeksum = as.duration(sum(duration)),.groups = "drop") 
  df_goalcolor_full <- df_goalcolor %>%
    cross_join(as_tibble(unique(df_weeksum$weekstart)))%>% # 取笛卡尔积
    rename(weekstart=value)  
  df_weeksum <- df_goalcolor_full %>%
    full_join(df_weeksum,by = c("type","weekstart"))%>%
    mutate(
      goal= goal*3600,
      weeksum = ifelse(is.na(weeksum), 0, weeksum),
      goalcolor = case_when(
        good_or_bad =="1" & weeksum >= goal ~"chartreuse4" , 
        good_or_bad =="-1" & weeksum < goal ~"chartreuse4" , 
        good_or_bad =="1" & weeksum < goal ~"red" , 
        good_or_bad =="-1" & weeksum >= goal ~"red" ,
        good_or_bad =="0"  ~"grey50"))%>%
    arrange(weekstart,type) 
  return(df_weeksum)
}
df_weeksum <- cal_weeksum()

# ============================周分类图函数定义==================================
draw_weekgroup_h <- function(startday){
  if (!startday %in% df_weeksum$weekstart ) {
    print(paste0(startday , "不是周一或不在时间范围内！"))
  } else {
    df_weekgroup <- df_weeksum %>% 
      filter(weekstart == startday) %>%
      group_by(weekstart,group) %>%
      summarise(weeksum = sum(weeksum),
                goal = sum(goal),
                .groups = "drop") %>%
      mutate(percent = paste0(round(weeksum/6048,1),"%"),
             text = paste0(round(as.numeric(weeksum)/3600,1),"\n",percent))%>%
      arrange(weekstart,group)
    # 分离 y 值最大的标签数据（仅 1 行）和其他标签数据
    max_label <- df_weekgroup %>% 
      slice_max(order_by = weeksum, n = 1)  
    other_labels <- df_weekgroup %>% 
      anti_join(max_label, by = "group")
    
    p_weekgroup <- ggplot(df_weekgroup, aes(x = group,y = weeksum)) + 
      geom_bar(stat='identity',alpha=0.25)+ 
      geom_errorbar(aes(ymin = weeksum, ymax = goal),color = "grey40",
                    width = 0.6,linewidth = 0.6) +
      geom_text(data=max_label,aes(y= pmin(weeksum,goal)-5000,label = text),
                fontface = "bold.italic",hjust = 1,color = "grey50",size = 2.5)+
      geom_text(data=other_labels,aes(y= pmax(weeksum,goal)+5000,label = text),
                fontface = "bold.italic",hjust = 0,color = "grey50",size = 2.5)+
      theme_light()+
      theme(axis.text.x = element_blank() ,
            axis.text.y=element_text(size=7,face = "bold",color="grey40"))+
      scale_x_discrete(limits=rev(df_weekgroup$group)) +
      scale_y_continuous(expand = c(0,500))+
      labs(x = NULL, y = NULL)+
      coord_flip()
    return(p_weekgroup)
  }
}  # group大类，用于搭配纵式时间轴
draw_weekgroup_v <- function(startday){
  if (!startday %in% df_weeksum$weekstart ) {
    print(paste0(startday , "不是周一或不在时间范围内！"))
  } else {
    df_weekgroup <- df_weeksum %>% 
      filter(weekstart == startday) %>%
      group_by(weekstart,group) %>%
      summarise(weeksum = sum(weeksum),
                goal = sum(goal),
                .groups = "drop") %>%
      mutate(percent = paste0(round(weeksum/6048,1),"%"),
             text = paste(round(as.numeric(weeksum)/3600,1),"(",percent,")"))%>%
      arrange(weekstart,group)
    # 分离 y 值最大的标签数据（仅 1 行）和其他标签数据
    max_label <- df_weekgroup %>% 
      slice_max(order_by = weeksum, n = 1)  
    other_labels <- df_weekgroup %>% 
      anti_join(max_label, by = "group")
    
    p_weekgroup <- ggplot(df_weekgroup, aes(x = group,y = weeksum)) + 
      geom_bar(stat='identity',alpha=0.25)+ 
      geom_errorbar(aes(ymin = weeksum, ymax = goal),color = "grey40",
                    width = 0.6,linewidth = 0.6) +
      geom_text(data=max_label,aes(y= pmin(weeksum,goal)-2000,label = text),
                fontface = "bold.italic",hjust = 1,color = "grey50",size = 2.5)+
      geom_text(data=other_labels,aes(y= pmax(weeksum,goal)+2000,label = text),
                fontface = "bold.italic",hjust = 0,color = "grey50",size = 2.5)+
      theme_light()+
      theme(axis.text.x = element_blank() ,
            axis.text.y=element_text(size=7,face = "bold",color="grey40"))+
      scale_x_discrete(limits=rev(df_weekgroup$group)) +
      scale_y_continuous(expand = c(0,500))+
      labs(x = NULL, y = NULL)+
      coord_flip()
    return(p_weekgroup)
  }
} # group大类，用于搭配纵式时间轴
draw_weektype <- function(startday) {
  if (!startday %in% df_weeksum$weekstart ) {
    print(paste0(startday , "不是周一或不在时间范围内！"))
  } else {
    df_weektype <- df_weeksum %>% 
      filter(weekstart == startday)%>%
      mutate(text = paste(round(weeksum/3600,1),"(",
                          round(goal/3600,1),")"))
    # 分离 y 值最大的标签数据（仅 1 行）和其他标签
    max_label <- df_weektype %>% 
      slice_max(order_by = weeksum, n = 1)  
    other_labels <- df_weektype %>% 
      anti_join(max_label, by = "type")  
    
    p_weektype <- ggplot(df_weektype, aes(x = type)) + 
      geom_bar(aes(y = weeksum,fill = color),stat='identity',alpha=0.7)+ 
      geom_errorbar(aes(ymin = weeksum, ymax = goal,color = goalcolor),
                    width = 0.6,linewidth = 0.4) +
      geom_text(data=max_label,aes(y= pmin(weeksum,goal)-2000,label = text),
                fontface = "bold.italic",hjust = 1,color = "grey50",size = 2.5)+
      geom_text(data=other_labels,aes(y= pmax(weeksum,goal)+2000,label = text),
                fontface = "bold.italic",hjust = 0,color = "grey50",size = 2.5)+
      theme_light()+
      theme(axis.text.x = element_blank(),
            axis.text.y=element_text(size=7,face = "bold",color="grey40"))+
      scale_x_discrete(limits=rev(df_weektype$type),) +
      scale_y_continuous(expand = c(0,500))+
      scale_fill_identity()+
      scale_color_identity()+
      labs(x = NULL, y = NULL)+
      coord_flip() 
    return(p_weektype)
  }
} # type小类

# ============================周时间轴数定义====================================
draw_weektimeline_h <- function(startday){
  df_weektimeline <- df_cleandata %>%
    filter(ymd(date) >= ymd(startday) & ymd(date) < ymd(startday) + 7) %>%
    left_join(df_goalcolor[c("type","color")],by = "type") %>%
    mutate(
      date=as.numeric(date),
      start = as.POSIXct(paste(startday,format(start,"%H:%M:%S"))),
      end = as.POSIXct(paste(startday,format(end,"%H:%M:%S"))))
  
  ggplot(df_weektimeline)+
    geom_linerange(aes(x= date, ymin = start +60,ymax = end - 60,
                       color = color),alpha=0.7,linewidth= 5.5)+
    scale_color_identity()+
    scale_x_reverse(
      labels = function(x) {format(as.Date(x, origin = "1970-01-01"), "%m-%d %a")},
      breaks = sort(unique(df_weektimeline$date), decreasing = TRUE)
    ) +
    scale_y_continuous(
      labels = function(y){hour(as.POSIXct(y))},
      breaks = seq(min(df_weektimeline$start), max(df_weektimeline$end)+1, by =3600),
      expand = expansion(mult = 0.002)) +
    scale_size_identity() + 
    theme_minimal()+
    theme(axis.text=element_text( face = "bold",color="grey40"))+
    labs(x = NULL, y = NULL)+
    coord_flip()
} # 周时间轴轴-横式
draw_weektimeline_v <- function(startday){
  df_weektimeline <- df_cleandata %>%
    filter(ymd(date) >= ymd(startday) & ymd(date) < ymd(startday) + 7) %>%
    left_join(df_goalcolor[c("type","color")],by = "type") %>%
    mutate(
      date= as.numeric(date),
      start = as.numeric(as.POSIXct(paste(startday,format(start,"%H:%M:%S")))),
      end = as.numeric(as.POSIXct(paste(startday,format(end,"%H:%M:%S")))),
      note = ifelse(is.na(note),"",note),
      event = paste(type,note),
      text = event %>%
        str_replace_all("[[:punct:]]|[\u3000-\u303F\uff00-\uffef]", " "), 
      height = end - start, 
      text = ifelse(height < 600,"",text), # 小于10分钟不显示
      text_size = pmin(2.5, (0.8 + height * 0.0004) / 1.2),
      text_size = ifelse(text_size < 0.1,0,text_size))
  
  ggplot(df_weektimeline)+
    geom_linerange(aes(x= date,
                       ymin = start+20,ymax = end-20,
                       color = color),alpha=0.7,linewidth= 20)+
    scale_color_identity()+
    scale_x_continuous(
      limits = (c(min(df_weektimeline$date)-0.5, max(df_weektimeline$date+0.5))),
      labels = function(x) {format(as.Date(x, origin = "1970-01-01"), "%m-%d %a")},
      expand = expansion(mult = 0.002),
      breaks = sort(unique(df_weektimeline$date))) +
    scale_y_continuous(
      labels = function(y){hour(as.POSIXct(y))},
      breaks = seq(min(df_weektimeline$start), max(df_weektimeline$end)+1, by = 3600),
      expand = expansion(mult = 0.002),trans = "reverse") +
    geom_textbox(aes(x = date-0.5, y = start-100,size = text_size,label = text),
                 #label = "备注备注",
                 box.padding = unit(1.5,"pt"), 
                 hjust = 0,   vjust = 1, width = 0.14,
                 lineheight = 1.2,fill = NA,
                 height = NULL,
                 box.colour =NA,
                 fontface = "bold",color = "white" ,
                 show.legend = FALSE)+
    scale_size_identity()+
    theme_minimal()+
    theme(axis.text=element_text( face = "bold",size= 8,color="grey40"))+
    labs(x = NULL, y = NULL)
} # 周时间轴-纵势 带标签

# ============================周组合图数定义====================================
draw_weekcombin_h <- function(startday){
  p1 <- draw_weektimeline_h(startday)
  p2 <- draw_weekgroup_h(startday)
  p3 <- draw_weektype(startday)
  pic_name <- paste(as.character(ymd(startday)),"~",as.character(ymd(startday) + 6),"周复盘")
  
  p_weekcombin <- p1/(p2 +p3 + plot_layout(widths = c(1,3)))+
    plot_layout(nrow=2,heights = c(0.7,2))+
    plot_annotation(title = pic_name,
                    theme = theme( plot.title = element_text(
                      size=14,hjust=0.5,face = "bold",color = "grey40")))
  
  ggsave(paste(pic_name,"_横式.png"), plot = p_weekcombin,
         width = 20/2.54, height = 14/2.54, dpi = 300, units = "in")
  print(paste(paste(pic_name,"_横式.png"),"已保存"))
  p_weekcombin
} # 纵式
draw_weekcombin_v <- function(startday){
  p1 <- draw_weektimeline_v(startday)
  p2 <- draw_weekgroup_v(startday)
  p3 <- draw_weektype(startday)
  pic_name <- paste(as.character(ymd(startday)),"~",as.character(ymd(startday) + 6),"周复盘")
  
  p_weekcombin <- (p2 +p3 + plot_layout(nrow=2,heights = c(1,3)))/p1+
    plot_layout(ncol=2,widths = c(1,2))+
    plot_annotation(title = pic_name,
                    theme = theme( plot.title = element_text(
                      size=14,hjust=0.5,face = "bold",color = "grey40")))
  
  ggsave(paste(pic_name,"_纵式.png"), plot = p_weekcombin,
         width = 20/2.54, height = 14/2.54, dpi = 300, units = "in")
  print(paste(paste(pic_name,"_纵式.png"),"已保存"))
  p_weekcombin
} # 横式

# ============================绘图及保存====================================
# 需填参数：需要分析的那周的周一日期
draw_weekcombin_h("2025-07-28")  
draw_weekcombin_v("2025-07-21")
