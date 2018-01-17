library(rvest)
library('ggplot2')

myurl='https://www.amazon.in/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=laptops'
webpage<-read_html(myurl)
name_html_data<-html_nodes(webpage,'.sx-line-clamp-4')
name_data<-html_text(name_html_data)
price_html_data<-html_nodes(webpage,'.s-price')
price_data<-html_text(price_html_data)
price_data<-gsub(",","",price_data)
price_data<-substr(price_data,3,nchar(price_data))
price_data<-as.numeric(price_data)
price_data<-price_data[1:24]
name_data<-gsub("\n","",name_data)
#name_data<-gsub(" ","",name_data)
#name_data<-gsub(",.*","",name_data)
name_data<-strsplit(name_data," ")
product_name<-c()
for(i in c(1:24))
{
  product_name<-c(product_name,name_data[[i]][1])  
}
#product_name
for(i in c(20:24))
  product_name[i]=substr(product_name[i],12,nchar(product_name[i]))
for(i in c(2:150))
{
  strings<-"https://www.amazon.in/s/ref=sr_pg_"
  strings<-paste0(strings,as.character(i))
  strings<-paste0(strings,"?fst=as%3Aon&rh=n%3A976392031%2Cn%3A1375424031%2Ck%3Alaptops&page=")
  strings<-paste0(strings,as.character(i))
  strings<-paste0(strings,"&keywords=laptops&ie=UTF8&qid=1516180902")
  myurl=strings
  webpage<-read_html(myurl)
  name_tmp_html<-html_nodes(webpage,'.s-access-title')
  name_tmp<-html_text(name_tmp_html)
  name_tmp<-name_tmp[1:24]
  name_tmp<-gsub("\n","",name_tmp)
  name_tmp<-strsplit(name_tmp," ")
  tmp_product<-c()
  for(j in c(1:24))
    tmp_product<-c(tmp_product,name_tmp[[j]][1])
  for(j in c(1:3))
    tmp_product[j]<-substr(tmp_product[j],12,nchar(tmp_product[j]))
  #for(j in c(22:24))
    #tmp_product[j]<-substr(tmp_product[j],12,nchar(tmp_product[j]))
  product_name<-c(product_name,tmp_product)
  price_tmp_html<-html_nodes(webpage,'.s-price')
  price_tmp<-html_text(price_tmp_html)
  price_tmp<-price_tmp[1:24]
  price_tmp<-gsub(",","",price_tmp)
  price_tmp<-substr(price_tmp,3,nchar(price_tmp))
  price_tmp<-as.numeric(price_tmp)
  #print(price_tmp)
  price_data<-c(price_data,price_tmp)
}
product_df<-data.frame(Price=price_data,Name=product_name)
qplot(data=product_df,Price,fill=Name,bins=10)
write.table(product_df,"LAPTOPS.txt",sep="\t",row.names = FALSE)