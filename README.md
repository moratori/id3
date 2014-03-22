ID3
=====

CSVファイルとかからID3で決定木を生成します。

![animal-csv](src/data/test4.csv)

![animal-tree](src/data/test4.png)

Usage
=====

```
* (ql:quickload :id3)
* (id3.out::decision-tree "filename.csv" "dotsource.dot")
```
