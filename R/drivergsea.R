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

drivergsea <- function(GeneSet,binary = '/Program Files/Mozilla Firefox/firefox.exe'){
  random_ints = sample(1:10000, 1, replace = TRUE)
  library(httr)
  library(tidyverse)
  library(rvest)
  library(RSelenium)
  library(wdman)
  library(DT)
  library(htmlwidgets)
  library(XML)
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
  url = "https://www.gsea-msigdb.org/gsea/login.jsp"
  driver$navigate(url)

  driver$maxWindowSize()
  driver$screenshot(display = TRUE)

  #登录Letpub-----------------------------------------------------------------
  driver$findElement(using='xpath',value="//input[@id='email']")$sendKeysToElement(list('858852776@qq.com'))
  driver$findElement(using='xpath',value="//input[@type='button']")$clickElement()
  driver$screenshot(display = TRUE)
  Sys.sleep(5)

  #搜索基因集-----------------------------------------------------------------
  driver$findElement(using = 'css selector', "a[href='msigdb/index.jsp']")$clickElement()
  Sys.sleep(5)
  driver$findElement(using = 'css selector', ".navItem[href*='genesets.jsp']")$clickElement()
  Sys.sleep(5)
  driver$screenshot(display = TRUE)
  #定位输入框
  driver$findElement(using = 'id', 'searchterm')$sendKeysToElement(list(GeneSet))
  driver$screenshot(display = TRUE)

  # 定位搜索按钮
  driver$findElement(using = 'id', 'Search')$clickElement()
  Sys.sleep(5)
  driver$screenshot(display = TRUE)

  tpage = driver$getPageSource()[[1]] %>% read_html(encoding ="UTF-8")# %>% htmlParse(encoding='UTF-8')

  # 提取所有<a>标签的href属性
  links = tpage %>%
    html_nodes("a") %>%  # 查找所有的<a>标签
    html_attr("href") %>% .[106:length(.)]   # 提取href属性

  links = paste0("https://www.gsea-msigdb.org/gsea/",links)


  for (variable in links) {
    driver$navigate(variable)
    tpage = driver$getPageSource()[[1]] %>% read_html(encoding ="UTF-8")
    gmt_link = tpage %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      grep("fileType=gmt", ., value = TRUE)
    download.file(paste0("https://www.gsea-msigdb.org/gsea/",gmt_link),
                  destfile = paste0(substr(gmt_link,47,(nchar(gmt_link)-13)),".gmt"))
  }
  driver$closeWindow()
  rD = rD$server$stop()
}

