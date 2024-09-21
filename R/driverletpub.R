# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

driverletpub <- function(KeyWord,binary = '/Program Files/Mozilla Firefox/firefox.exe'){
  random_ints = sample(1:10000, 1, replace = TRUE)
  library(httr)
  library(tidyverse)
  library(rvest)
  library(RSelenium)
  library(wdman)
  library(XML)
  library(data.table)
  library(stringr)
  library(magrittr)
  rD = rsDriver(
    browser = "firefox",
    port = random_ints,
    chromever = NULL,
    geckover = "0.33.0",
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        binary = binary,
        args = list("--headless")
      ),
      "browser.startup.homepage" = "about:blank",
      "browser.privatebrowsing.autostart" = TRUE,
      "browser.download.manager.showWhenStarting" = FALSE,
      "browser.helperApps.neverAsk.saveToDisk" = "application/octet-stream"
    ),
    check = F,
    verbose = F
  )
  driver = rD$client
  driver$open()

  url = "https://www.letpub.com.cn/index.php?page=login"
  driver$navigate(url)

  driver$maxWindowSize()
  driver$screenshot(display = TRUE)

  driver$findElement(using='xpath',value="//input[@id='email']")$sendKeysToElement(list('repatriatekr7712@outlook.com'))
  driver$findElement(using='xpath',value="//input[@id='password']")$sendKeysToElement(list('123456789'))
  driver$findElement(using='xpath',value="//img[@src='images/userlogin.jpg']")$clickElement()

  driver$navigate('http://www.letpub.com.cn/index.php?page=grant')
  driver$screenshot(display = TRUE)
  Sys.sleep(5)

  table_final_all = data.frame()
  for (variable2 in 1:32) {
    driver$navigate('http://www.letpub.com.cn/index.php?page=grant')
    driver$screenshot(display = TRUE)
    Sys.sleep(10)

    driver$findElement(using='xpath',value="//input[@id='name']")$sendKeysToElement(list(KeyWord))
    driver$findElement(using='xpath',value="//select[@id='startTime']/option[4]")$clickElement()
    driver$findElement(using='xpath',value="//select[@id='endTime']/option[1]")$clickElement()
    driver$findElement(using='xpath',value="//select[@id='addcomment_s1_advanced']/option[9]")$clickElement()
    driver$findElement(using='xpath',value=paste0("//select[@id='province_main']/option[",variable2,"]"))$clickElement()
    driver$screenshot(display = TRUE)

    driver$findElement(using='xpath',value="//input[@id='submit']")$clickElement()
    Sys.sleep(10)

    tryCatch({
      tpage = driver$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% htmlParse(encoding='UTF-8')
      ttabl = xpathSApply(tpage,"//table[@class='table_yjfx']")[[1]] %>% readHTMLTable(header=T) %>% set_colnames(.[1,]) %>% data.table() %>% .[c(-1,-.N),-7]

      mmm = driver$getPageSource()[[1]] %>%read_html(encoding ="UTF-8") %>% html_node('table.table_yjfx') %>% html_node('form') %>% html_text()
      total_p = strsplit(mmm,"/")[[1]][2]
      total_p = as.numeric(gsub('[页) ]',"",total_p))

      for (i in 1:(total_p-1)) {
        driver$findElement("link text", "下一页")$clickElement()
        Sys.sleep(10)
        tpage = driver$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% htmlParse(encoding='UTF-8')
        ttabl1 = xpathSApply(tpage,"//table[@class='table_yjfx']")[[1]] %>% readHTMLTable(header=T) %>% set_colnames(.[1,]) %>% data.table() %>% .[c(-1,-.N),-7]
        ttabl = rbind(ttabl,ttabl1)
      }

      ttabl = ttabl[-which( ttabl$负责人 %in% c("学科代码","执行时间")),]
      tab_name = ttabl[which(ttabl$项目编号!= "NA"), ]
      tab_title = ttabl[which(ttabl$负责人== "题目"), ][,2]
      names(tab_title) = c("题目")
      tab_cat = ttabl[which(ttabl$负责人== "学科分类"), ][,2]
      names(tab_cat) = c("学科分类")
      table_final = cbind(tab_name,tab_title)
      table_final = cbind(table_final,tab_cat)
      table_final_all = rbind(table_final_all,table_final)
      rm(table_final)
    }, error = function(e) {
      cat("Error encountered with item:",variable2, "\n")
    })
  }

  write.table(table_final_all, paste0("国自然_",KeyWord,".txt"), sep = "\t", row.names=F,quote = F,fileEncoding = "utf8")
  driver$closeWindow()
  rD = rD$server$stop()
}

