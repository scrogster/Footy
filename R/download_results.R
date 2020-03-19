
#Code to download historic AFL results and details of future games.
download.file("https://api.squiggle.com.au/?q=games;year=2016;format=csv",
							"Data/Results2016.csv")
download.file("https://api.squiggle.com.au/?q=games;year=2017;format=csv",
							"Data/Results2017.csv")
download.file("https://api.squiggle.com.au/?q=games;year=2018;format=csv",
							"Data/Results2018.csv")
download.file("https://api.squiggle.com.au/?q=games;year=2019;format=csv",
							"Data/Results2019.csv")
download.file("https://api.squiggle.com.au/?q=games;year=2020;format=csv",
							"Data/Results2020.csv")

