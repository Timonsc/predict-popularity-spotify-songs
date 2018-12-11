# Timon Schneider, José Diaz Mendoza and Carlos Callejo.
> 18 May 2018
> https://www.kaggle.com/nadintamer/top-tracks-of-2017/home

#### Abstract
This was a project for a course called Data Valorization. We participated in a Kaggle Challenge where we aimed to predict the popularity of the songs based on features generated by spotify. This was part of a business case where a producer could test a album on the most popular songs for optimal marketing of the album. I wrote a script to mine more data, particularly unpopular songs from the artists in the dataset such that we can more specifically find out what features make the difference between a popular song and unpopular song.

In this project we made a lot of mistakes and learned a lot from them. For example, the assumption that a song is popular (1) or unpopular (0) is quite risky. This project needed a lot of hacking and data strangling, the Spotify API did not deliver very consistent data. Ultimately, the data set was very small and we should have considered that more in out choice of predictive methods.

#### How to run:
1) Set the correct file locations to the data sets in Workbook.R
2) If you want to use the dataretrieval.R you should sign up for the spotify API (the retrieved data is included in /input/)