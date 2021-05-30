# Анализ заездов в игру Марио

## Data Dictionary

### `world-records`

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