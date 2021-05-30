# Анализ заездов в игру Марио

Игра состоит из 16 отдельных треков, и мировые рекорды могут быть достигнуты как на самом быстром одном круге, так и на самой быстрой завершенной гонке (три круга). Кроме того, на протяжении многих лет игроки открывали короткие пути на многих треках.

[Подробнее о данных рассказано здесь](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md)

## Data Dictionary

### `records`

| variable         | class     | description                     |
|:-----------------|:----------|:--------------------------------|
| track            | character | Track name                      |
| type             | factor    | Single or three lap record      |
| shortcut         | factor    | Shortcut or non-shortcut record |
| player           | character | Player’s name                   |
| system_played   | character | Used system (NTSC or PAL)       |
| date             | date      | World record date               |
| time_period     | period    | Time as `hms` period            |
| time             | double    | Time in seconds                 |
| record_duration | double    | Record duration in days         |

### `drivers`

| variable | class     | description                            |
|:---------|:----------|:---------------------------------------|
| position | integer   | Player’s current leader board position |
| player   | character | Player’s name                          |
| total    | integer   | Total world records                    |
| year     | double    | Year                                   |
| records  | integer   | Number of world records                |
| nation   | character | Player’s nationality                   |