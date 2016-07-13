"begin" "wise man puzzle n =2"
"assumption" (Common! now (not (Knows! a1 now (Marked a1))))
"assumption" (Common! now (if (not (Marked a2)) (Marked a1)))
"assumption" (Common! now (if (not (Marked a1)) (Marked a2)))
"assumption" (Common! now (if (Marked a1) (not (Marked a2))))
"assumption" (Common! now (if (Marked a2) (not (Marked a1))))
"assumption" (Common! now (if (not (Marked a2)) (Knows! a1 now (not (Marked a2)))))
"goal" (and (Marked a2) (not (Marked a1)))
"end"