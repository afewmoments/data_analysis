# Wine ratings

This dataset is a wine-enthusiast ratings dataset from [Kaggle](https://www.kaggle.com/zynicide/wine-reviews).

# Get the data!

```
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
```

# Data Dictionary


### `winemag-data-130k-v2.csv`

|variable              |class     |description |
|:---|:---|:-----------|
|country               |character | Country of origin |
|description           |character | Flavors and taste profile as written by reviewer |
|designation | character | The vineyard within the winery where the grapes that made the wine are from |
|points                |double    | The number of points WineEnthusiast rated the wine on a scale of 1-100 (though they say they only post reviews for wines that score >=80) |
|price                 |double    | The cost for a bottle of the wine |
|province              |character | The province or state that the wine is from|
|region_1              |character | The wine growing area in a province or state (ie Napa) |
|taster_name           |character | The taster/reviewer |
|title                 |character | The title of the wine review, which often contains the vintage (year) |
|variety               |character | Grape type |
|winery                |character | The winery that made the wine |
