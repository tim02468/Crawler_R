library(rvest)
library(httr)
library(xml2)

#Create empty vector
fishname = c()
fish1 = c()
fish2 = c()
year = c()
plan = c()


#Let's get started
name = c("Acrossocheilus paradoxus", "Ambassis miops")
for (i in 1:length(name)) {
  
  a = strsplit(paste0(name[i]), " ")
  a = unlist(a)
  url = paste0("http://fishdb.sinica.edu.tw/chi/fishdistri_both.php?gen=",a[1],"&spe=",a[2],"&PointType2=Y")
  x = GET(url, add_headers('user-agent' = 'r'))
  readed = read_html(x)
  findhref = xml_find_all(readed, "//form/a")
  findhref2 = xml_attr(findhref, "href")
  findhref2 = findhref2[1:length(findhref2)-1]
  
  for (i in 1:length(findhref2)) {
    url2 = read_html(paste0("http://fishdb.sinica.edu.tw/chi/", findhref2[i]))
    findid = xml_find_all(url2, "//tr/td[@align='center']/a")
    idfile = xml_text(findid)
    idfile = idfile[idfile != ""]
    for (i in 1:length(idfile)) {
      
      if(length(fish1) %% 50 == 0){
        Sys.sleep(180)
      }
      fish = read_html(paste0("http://fishdb.sinica.edu.tw/chi/observation_detail.php?id=",idfile[i]))
      findall = xml_find_all(fish, "//tr/td")
      findallText = xml_text(findall)
      fishname = c(fishname, findallText[8])
      fish1 = c(fish1, findallText[14])
      fish2 = c(fish2, findallText[18])
      year = c(year, findallText[44])
      plan = c(plan, findallText[42])
    }
    
  }
  
}

#Dataframe you need
test = data.frame("Species" = fishname, "緯度" = fish1, "經度" = fish2, "年份" = year, "計畫名稱" = plan)

#Well Done!




a = strsplit("Caesio caerulaurea", " ")
a = unlist(a)
url = paste0("http://fishdb.sinica.edu.tw/chi/fishdistri_both.php?gen=Ambassis&spe=miops&PointType2=Y")
readed = read_html(url)
findhref = xml_find_all(readed, "//form/a")
findhref2 = xml_attr(findhref, "href")
findhref2 = findhref2[1:length(findhref2)-1]

id = c()
  
for (i in 1:length(findhref2)) {
  url2 = read_html(paste0("http://fishdb.sinica.edu.tw/chi/", findhref2[i]))
  findid = xml_find_all(url2, "//tr/td[@align='center']/a")
  idfile = xml_text(findid)
  idfile = idfile[idfile != ""]
  id = c(id, idfile)
  
}
  



for (i in 1:length(idfile)) {
  fish = read_html(paste0("http://fishdb.sinica.edu.tw/chi/observation_detail.php?id=",idfile[1]))
  findall = xml_find_all(fish, "//tr/td")
  findallText = xml_text(findall)
  fishname = c(fishname, findallText[8])
  fish1 = c(fish1, findallText[14])
  fish2 = c(fish2, findallText[18])
  year = c(year, findallText[44])
  plan = c(plan, findallText[42])
}



url = read_html("http://fishdb.sinica.edu.tw/chi/fishdistri_both.php?gen=Caesio&spe=caerulaurea&PointType2=Y")
test = url %>% html_nodes("div #main") 

test1 = xml_find_all(url, "//form/a")
test2 = xml_attr(test, "href")[1:length(test2)-1]

url2 = read_html(paste0("http://fishdb.sinica.edu.tw/chi/",test2[1]))
a = xml_find_all(url2, "//tr/td[@align='center']/a")
textfile = xml_text(a)
textfile = textfile[textfile !=""]

fish = read_html(paste0("http://fishdb.sinica.edu.tw/chi/observation_detail.php?id=",textfile[1]))
findall = xml_find_all(fish, "//tr/td")
findallText = xml_text(findall)
fishname = c(fishname, findallText[8])
fish1 = c(fish1, findallText[14])
fish2 = c(fish2, findallText[18])




#markdown
get_href = function(name){
  
  divided = unlist(strsplit(name," "))
  
  if(length(divided) == 2){
    url = paste0("http://fishdb.sinica.edu.tw/chi/fishdistri_both.php?gen=",divided[1],"&spe=",divided[2],"&PointType2=Y")
  }else if(length(divided) == 3){
    url = paste0("http://fishdb.sinica.edu.tw/chi/fishdistri_both.php?gen=",divided[1],"&spe=",divided[2],"%20",divided[3],"&PointType2=Y")
  }
  
  readed = read_html(url)
  findhref = xml_find_all(readed, "//form/a")
  findhref2 = xml_attr(findhref, "href")
  
  check_href = function(link){
    key = substr(link, 1, 11)
    if(!"observation" %in% substr(link, 1, 11)){
      return(" ")
    }else{
      return(link)
    }
  }
  
  test = sapply(findhref2, check_href)
  test = test[test!=" "]
  test = paste0("http://fishdb.sinica.edu.tw/chi/", test)
  return(test)
}

name = list("Caesio caerulaurea","Carassius auratus auratus")
test = lapply(name, get_href)

get_id = function(link){
  url = read_html(link)
  findid = xml_find_all(url2, "//tr/td[@align='center']/a")
  idfile = xml_text(findid)
  idfile = idfile[idfile != ""]
  return(idfile)
}


test2 = lapply(unlist(test), get_id)


