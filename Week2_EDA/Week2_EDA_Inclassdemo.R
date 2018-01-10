url = "https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/Week2_EDA"
Rays.url = paste(url, "Rays_starter_1998_2015.csv", sep="/")
Rays = read.table(Rays.url, sep=",", header=TRUE)

Rays1=read.csv("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/Week2_EDA/Rays_starter_1998_2015.csv",
               sep=",", header=TRUE)

Rays2 = read.table("D:/Courses/UM/2016_WN/NRE538_GSRA/Labs/NRE538_Lab1/Rays_starter_1998_2015.csv",sep=",", header=T)

identical(Rays2, Rays1)
