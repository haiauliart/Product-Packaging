##Project Machine Learning for Retail with R : Product Packaging

library(arules)
transaksi_tabular <- read.transactions(file="https://storage.googleapis.com/dqlab-dataset/transaksi_dqlab_retail.tsv", format="single", sep="\t", cols=c(1,2), skip=1)
write(transaksi_tabular, file="test_project_retail_1.txt", sep=",")

#mendapatkan insight top 10 dari produk yang terjual 
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.csv", format="single", sep="\t", cols=c(1,2), skip=1)
top10_item <- itemFrequency(transaksi_tabular, type="absolute")
top10_item <- sort(top10_item, decreasing=TRUE)
top10_item <- top10_item [1:10]
data_item <- data.frame("Nama Produk"=names(top10_item), "Jumlah"=top10_item, row.names=NULL)
write.csv(data_item, file="top10_item_retail.txt")

#mendapatkan insight bottom 10 dari produk yang terjual
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.csv", format="single", sep="\t", cols=c(1,2), skip=1)
bottom10 <- itemFrequency(transaksi_tabular, type="absolute")
bottom10 <- sort(bottom10, decreasing=FALSE)
bottom10 <- bottom10[1:10]
bottom10 <- data.frame("Nama.Produk"=names(bottom10), "Jumlah"=bottom10, row.names=NULL)
write.csv(bottom10, file="bottom10_item_retail.txt")

#mendapatkan daftar seluruh kombinasi paket produk yang menarik
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.csv", format="single", sep="\t", cols=c(1,2), skip=1)
rules <- apriori(transaksi_tabular, parameter = list(supp=10/length(transaksi_tabular), conf=0.5, minlen=2, maxlen=3))
apriori_rules <- c(head(sort(rules, by="lift"), n=10))
write(apriori_rules, file="kombinasi_retail.txt")

#mendapatkan daftar seluruh kombinasi paket produk dengan item tertentu
library(arules)
transaksi_tabular <- read.transactions(file="transaksi_dqlab_retail.csv", format="single", sep="\t", cols=c(1,2), skip=1)
rules <- apriori(transaksi_tabular, parameter=list(supp=10/length(transaksi_tabular), conf=0.1, minlen=2, maxlen=3))
A <- subset(rules, rhs %in% "Tas Makeup")
B <- subset(rules, rhs %in% "Baju Renang Pria Anak-anak")
A <- head(sort(A, by="lift", decreasing=TRUE), n=3L)
B <- head(sort(B, by="lift", decreasing=TRUE), n=3L)
AB <- c(A,B)
inspect(AB)
write(AB, file="kombinasi_retail_slow_moving.txt")



