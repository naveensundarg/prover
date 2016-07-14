
"begin" "wise man puzzle n =2"
"assumption" (Common! now (not (Knows! a1 now (Marked a1))))
"assumption" (Common! now (iff (not (Marked a2)) (Marked a1)))
"assumption" (Common! now (if (not (Marked a2)) (Knows! a1 now (not (Marked a2)))))
"goal" (Marked a2)
"end"