
(ql:quickload :id3)


(import '(id3.out:decision-tree))


(decision-tree "data/test1.csv" "data/test1.dot")
