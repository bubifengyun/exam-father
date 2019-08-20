#!/usr/bin/env R
# _setup_mysql.R

## 连接数据库
source("_paperid.R")
library(RMariaDB)
library(stringr)
db = RMariaDB::dbConnect(RMariaDB::MariaDB(), 
    user = "litianci", 
    password = "litianci", 
    dbname = "db_blog", 
    unix.socket = "/opt/lampp/var/mysql/mysql.sock")
rs = dbSendQuery(db, sprintf("SELECT * FROM tbl_shijuan WHERE id = '%s'", paperid))
shijuan_info = dbFetch(rs, n = 1)
dbClearResult(rs) 

## 六大题型的编号 

tiankong = 1
xuanze = 2
panduan = 3
jianda = 4 
zongshu = 5 
duoxuan = 6 
ans_in_paper = TRUE 

## 填空题和答案的生成 

sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')",
    tiankong, 
    shijuan_info$career, 
    shijuan_info$person_type) 

tiankong_rs = dbSendQuery(db, sqlstring) 
tiankong_all = dbFetch(tiankong_rs, n = -1) 
dbClearResult(tiankong_rs) 

tiankong_space_count = shijuan_info$tiankong_count #填空，传过来的是空数不是题数 
tiankong_search_depth = 10 #因为空数可能不正好够，或多或少，需要反复检测 
if (length(tiankong_all$id) < shijuan_info$tiankong_count) { 
    shijuan_info$tiankong_count = length(tiankong_all$id) 
} 

#tiankong_show_ids = sample(tiankong_all$id, shijuan_info$tiankong_count, replace = FALSE) 
tiankong_show_ids = tiankong_all$id

has_found = FALSE 
has_found_notgood = FALSE 
has_enough_space = FALSE 
tiankong_notgood_ids = c() 
for (step in 1:tiankong_search_depth) { 
    space_count = 0 
    tiankong_current_step_ids = c() 
    tiankong_sample_ids = sample(tiankong_all$id, shijuan_info$tiankong_count, replace = FALSE) 
    for (id in tiankong_sample_ids) { 
        tiankong_current_step_ids = c(tiankong_current_step_ids,id) 
        space_count = space_count + str_count(tiankong_all$content[tiankong_all$id == id],"\\line") if (space_count == tiankong_space_count -1) { 
            tiankong_show_ids = tiankong_current_step_ids 
            has_found_notgood = TRUE 
        } else if (space_count == tiankong_space_count) { 
            tiankong_show_ids = tiankong_current_step_ids 
            has_found = TRUE 
            has_enough_space = TRUE 
            break 
        } else if (space_count > tiankong_space_count) { 
            tiankong_notgood_ids = tiankong_current_step_ids 
            has_enough_space = TRUE 
            break 
        } 
    } 
    if (has_found) { break } 
    if (step == tiankong_search_depth) { 
        if (has_found_notgood) { has_found = TRUE } 
    } 
} 

if (!has_found) { 
    if (has_enough_space) { 
        tiankong_show_ids = tiankong_notgood_ids 
    } else { 
        tiankong_space_count = space_count 
    } 
} 

shijuan_info$tiankong_count = length(tiankong_show_ids)

tiankong_show = "" 
tiankong_ans = c() 
for (id in tiankong_show_ids) { 
    tiankong_strings = tiankong_all$content[tiankong_all$id == id] 
    if (ans_in_paper) { 
        tiankong_content = sprintf("\\item\t%s【答】%s\n", 
            tiankong_strings, tiankong_all$ans_area[tiankong_all$id == id]) 
    } else { 
        tiankong_content = sprintf("\\item\t%s\n",tiankong_strings) 
    } 
    tiankong_show = sprintf("%s%s", tiankong_show, tiankong_content) 
    tiankong_ans = c(tiankong_ans, tiankong_all$ans_area[tiankong_all$id == id]) 
} 


## 选择题和答案的生成 

sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')", xuanze, shijuan_info$career, shijuan_info$person_type) 
xuanze_rs = dbSendQuery(db, sqlstring) 
xuanze_all = dbFetch(xuanze_rs, n = -1) 
dbClearResult(xuanze_rs)

if (length(xuanze_all$id) < shijuan_info$xuanze_count) { 
    shijuan_info$xuanze_count = length(xuanze_all$id) 
    } 
#xuanze_show_ids = sample(xuanze_all$id, shijuan_info$xuanze_count, replace = FALSE) 
xuanze_show_ids = xuanze_all$id 
xuanze_show = "" 
xuanze_ans = c() 
for (id in xuanze_show_ids) { 
    xuanze_strings = xuanze_all$content[xuanze_all$id == id] 
    xuanze_contents_split = strsplit(xuanze_strings, '\n')[[1]] 
    xuanze_contents_trim = stringr::str_trim(xuanze_contents_split, side = "both") 
    if (ans_in_paper) { 
        xuanze_content = sprintf("\\item\t%s\\xxans{%s}{%s}{%s}{%s}{%s}\n", 
            xuanze_contents_trim[1], 
            xuanze_contents_trim[2], 
            xuanze_contents_trim[3], 
            xuanze_contents_trim[4], 
            xuanze_contents_trim[5], 
            xuanze_all$ans_chars[xuanze_all$id == id]) 
    } else { 
        xuanze_content = sprintf("\\item\t%s\\xx{%s}{%s}{%s}{%s}\n", 
        xuanze_contents_trim[1], 
        xuanze_contents_trim[2], 
        xuanze_contents_trim[3], 
        xuanze_contents_trim[4], 
        xuanze_contents_trim[5]) 
    } 
    
    xuanze_show = sprintf("%s%s", xuanze_show, xuanze_content) 
    xuanze_ans = c(xuanze_ans, xuanze_all$ans_chars[xuanze_all$id == id]) 
}

## 判断题和答案的生成 

sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')", panduan, shijuan_info$career, shijuan_info$person_type) 
panduan_rs = dbSendQuery(db, sqlstring) 
panduan_all = dbFetch(panduan_rs, n = -1) 
dbClearResult(panduan_rs) 

if (length(panduan_all$id) < shijuan_info$panduan_count) { 
    shijuan_info$panduan_count = length(panduan_all$id) 
} 
#panduan_show_ids = sample(panduan_all$id, shijuan_info$panduan_count, replace = FALSE) 
panduan_show_ids = panduan_all$id 
panduan_show = "" 
panduan_ans = c() 
for (id in panduan_show_ids) { 
    panduan_strings = panduan_all$content[panduan_all$id == id] 
    panduan_contents_split = strsplit(panduan_strings, "。")[[1]] 
    panduan_contents_trim = stringr::str_trim(panduan_contents_split, side = "both") 
    if (ans_in_paper) { 
        panduan_content = sprintf("\\item %s。\\hspace*{\\fill}\\khans{%s}\n", 
            panduan_contents_trim[1],
            panduan_all$ans_chars[panduan_all$id == id]) 
    } else { 
        panduan_content = sprintf("\\item %s。\\hspace*{\\fill}\\kh\n", panduan_contents_trim[1]) 
    } 
    panduan_show = sprintf("%s%s", panduan_show, panduan_content) 
    panduan_ans = c(panduan_ans, panduan_all$ans_chars[panduan_all$id == id]) 
}

## 多选题和答案的生成 
sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')", duoxuan, shijuan_info$career, shijuan_info$person_type) 

duoxuan_rs = dbSendQuery(db, sqlstring) 
duoxuan_all = dbFetch(duoxuan_rs, n = -1) 
dbClearResult(duoxuan_rs) 

if (length(duoxuan_all$id) < shijuan_info$duoxuan_count) { 
    shijuan_info$duoxuan_count = length(duoxuan_all$id) 
} 
#duoxuan_show_ids = sample(duoxuan_all$id, shijuan_info$duoxuan_count, replace = FALSE) 
duoxuan_show_ids = duoxuan_all$id 
duoxuan_show = "" 
duoxuan_ans = c()

for (id in duoxuan_show_ids) { 
    duoxuan_strings = duoxuan_all$content[duoxuan_all$id == id] 
    duoxuan_contents_split = strsplit(duoxuan_strings, '\n')[[1]] 
    duoxuan_contents_trim = stringr::str_trim(duoxuan_contents_split, side = "both") 
    if (ans_in_paper) { 
        duoxuan_content = sprintf("\\item\t%s\\xxans{%s}{%s}{%s}{%s}{%s}\n", 
            duoxuan_contents_trim[1], 
            duoxuan_contents_trim[2], 
            duoxuan_contents_trim[3], 
            duoxuan_contents_trim[4], 
            duoxuan_contents_trim[5], 
            duoxuan_all$ans_chars[duoxuan_all$id == id]) 
    } else { 
        duoxuan_content = sprintf("\\item\t%s\\xx{%s}{%s}{%s}{%s}\n", 
            duoxuan_contents_trim[1], 
            duoxuan_contents_trim[2], 
            duoxuan_contents_trim[3], 
            duoxuan_contents_trim[4], 
            duoxuan_contents_trim[5]) 
    } 
    duoxuan_show = sprintf("%s%s", duoxuan_show, duoxuan_content) 
    duoxuan_ans = c(duoxuan_ans, duoxuan_all$ans_chars[duoxuan_all$id == id]) 
}

## 简答题和答案的生成 
sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')", jianda, shijuan_info$career, shijuan_info$person_type) 

jianda_rs = dbSendQuery(db, sqlstring) 
jianda_all = dbFetch(jianda_rs, n = -1) 
dbClearResult(jianda_rs) 

if (length(jianda_all$id) < shijuan_info$jianda_count) { 
    shijuan_info$jianda_count = length(jianda_all$id) 
} 

#jianda_show_ids = sample(jianda_all$id, shijuan_info$jianda_count, replace = FALSE) 
jianda_show_ids = jianda_all$id 
jianda_show = "" 
jianda_ans = c()

for (id in jianda_show_ids) { 
    if (length(jianda_all$id) > 1) { 
        jianda_strings = jianda_all$content[jianda_all$id == id] 
        jianda_ans_strings = jianda_all$ans_area[jianda_all$id == id] 
    } else { 
        jianda_strings = jianda_all$content 
        jianda_ans_strings = jianda_all$ans_area 
    } 
    if (ans_in_paper) { 
        jianda_content = sprintf("\\item\t%s\\\\【答】%s\n", 
            jianda_strings, 
            jianda_ans_strings) 
    } else { 
        jianda_content = sprintf("\\item\t%s\n\\ansarea", jianda_strings) 
    } 
    jianda_show = sprintf("%s%s", jianda_show, jianda_content) 
    jianda_ans = c(jianda_ans, jianda_ans_strings) 
}


## 综述题和答案的生成 

sqlstring = sprintf("SELECT * FROM tbl_faq_library WHERE test_type = '%s' AND career = '%s' AND ( person_type = '%s' OR person_type = '0')", zongshu, shijuan_info$career, shijuan_info$person_type) 

zongshu_rs = dbSendQuery(db, sqlstring) 
zongshu_all = dbFetch(zongshu_rs, n = -1) 
dbClearResult(zongshu_rs) 

if (length(zongshu_all$id) < shijuan_info$zongshu_count) { 
    shijuan_info$zongshu_count = length(zongshu_all$id) 
} 
#zongshu_show_ids = sample(zongshu_all$id, shijuan_info$zongshu_count, replace = FALSE) 
zongshu_show_ids = zongshu_all$id zongshu_show = "" 
zongshu_ans = c() 
for (id in zongshu_show_ids) { 
    if (length(zongshu_all$id) > 1) { 
        zongshu_strings = zongshu_all$content[zongshu_all$id == id] 
        zongshu_ans_strings = zongshu_all$ans_area[zongshu_all$id == id] 
    } else { 
        zongshu_strings = zongshu_all$content 
        zongshu_ans_strings = zongshu_all$ans_area 
    }
    if (ans_in_paper) { 
        zongshu_content = sprintf("\\item\t%s\\\\【答】%s\n", 
            zongshu_strings, 
            zongshu_ans_strings) 
    } else { 
        zongshu_content = sprintf("\\item\t%s\n\\ansarea", zongshu_strings) 
    } 
    zongshu_show = sprintf("%s%s", zongshu_show, zongshu_content) 
    zongshu_ans = c(zongshu_ans, zongshu_ans_strings) 
} 

## 填空题答案的处理 

i = 0 
tiankong_ans_show = "\\item[] " 
while (i < shijuan_info$tiankong_count) { 
    i = i+1 
    tiankong_ans_show = sprintf("%s\\textbf{%s}、%s\\quad ",
        tiankong_ans_show,i,tiankong_ans[i]) 
} 

## 选择题答案的处理

formatans <- function(ans, count, off_set=0) { 
    i = 1 
    ans_show = "" 
    while (i < count) { 
        last_item = i + 4 
        if (last_item > count) { 
            last_item = count 
        } 
        if ((last_item %% 30) == 0) { 
            ans_show = sprintf("%s\n",ans_show) 
        } else if ((i %% 30) == 1) { 
            ans_show = sprintf("%s\\item[]",ans_show) 
        } 
        current_ans = ans[i:last_item] 
        ans_show = sprintf("%s%02d-%02d、%s\\quad ", 
            ans_show,i+off_set,last_item+off_set, 
            stringr::str_c(current_ans, collapse = "")) 
        i = i + 5 
    } 
    return(ans_show) 
} 

off_set = shijuan_info$tiankong_count 
xuanze_ans_show = formatans(xuanze_ans, shijuan_info$xuanze_count, off_set = off_set) 

## 判断题答案的处理

off_set = shijuan_info$xuanze_count + off_set 
panduan_ans_show = formatans(panduan_ans, shijuan_info$panduan_count, off_set = off_set) 
#替换AB为对错 
panduan_ans_show = gsub("A","√",panduan_ans_show) 
panduan_ans_show = gsub("B","×",panduan_ans_show) 
panduan_ans_show = gsub("V","√",panduan_ans_show) 
panduan_ans_show = gsub("X","×",panduan_ans_show)

 ## 多选题答案的处理 
 
 off_set = shijuan_info$panduan_count + off_set 
 i = 0 
 duoxuan_ans_show = "\\item[] " 
 while (i < shijuan_info$duoxuan_count) { 
     i = i+1 
     duoxuan_ans_show = sprintf("%s\\textbf{%s}、%s\\quad ",
        duoxuan_ans_show,i + off_set,duoxuan_ans[i]) 
} 
     
## 简答题答案的处理

formatarea_ans <- function(ans, count, off_set=0) { 
    i = 0 
    ans_show = "" 
    while (i < count) { 
        i = i+1 
        ans_show = sprintf("%s\\item[]%s、答：%s\n ",ans_show,i + off_set,ans[i]) 
    } 
    return(ans_show) 
} 

off_set = shijuan_info$duoxuan_count + off_set 
jianda_ans_show = formatarea_ans(jianda_ans, shijuan_info$jianda_count, off_set) 

## 综述题答案的处理 

off_set = shijuan_info$jianda_count + off_set 
zongshu_ans_show = formatarea_ans(zongshu_ans, shijuan_info$zongshu_count, off_set) 

## 总分栏的制作

total_tixing = 0 
if (shijuan_info$tiankong_count > 0) 
    total_tixing = total_tixing + 1 
if (shijuan_info$xuanze_count > 0) 
    total_tixing = total_tixing + 1 
if (shijuan_info$panduan_count > 0)
    total_tixing = total_tixing + 1 
if (shijuan_info$duoxuan_count > 0) 
    total_tixing = total_tixing + 1 
if (shijuan_info$jianda_count > 0) 
    total_tixing = total_tixing + 1 
if (shijuan_info$zongshu_count > 0) 
    total_tixing = total_tixing + 1 

zongfenlan = sprintf( 
    "\\newcounter{tixingcounter}\n\\renewcommand{
    \\zongfenlan}{\n\\begin{center}\\setlength{\\tabcolsep}{4mm} 
    \\renewcommand{\\arraystretch}{1.5} 
    \\begin{tabular}{*{%s}{|c}|} \\hline\n 题号",total_tixing+4) 

zongfenlan = sprintf("%s%s", zongfenlan, paste(rep(
    "& \\stepcounter{tixingcounter}\\chinese{tixingcounter}", total_tixing),collapse = " ")) 
zongfenlan = sprintf("%s &总分&总分人&复核人\\\\\\hline\n 分数 %s", zongfenlan, 
    paste(rep("&", total_tixing + 3),collapse = " ")) 
zongfenlan = sprintf("%s \\\\\n\\hline\\end{tabular}\\end{center}\\vskip-3mm}" , zongfenlan) 
zongfenlan = sprintf("%s \n \\zongfenlan", zongfenlan) 

## 关闭数据库 

dbDisconnect(db)