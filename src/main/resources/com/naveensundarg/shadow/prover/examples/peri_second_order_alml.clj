{:name        "Tantalizer” PERI.2 Demo "
 :description
 "PERI, hi, does nao_south know that nao_north knows there are three objects, each north of a dodecahedron and possessed of a certain property?"
 :assumptions {:given (Knows! peri
                              (Knows! nao-north
                                      (Knows! nao-south
                                              (exists [x? y? z? u?]
                                                      (and (not (= x? y?)) (not (= y? z?)) (not (= z? u?)) (Dodec u?)
                                                           (N x? u?) (N y? u?) (N z? u?)
                                                           (Red x?)
                                                           ;;;⚡💥⚡;;;
                                                           (Red z?))))))}
 :goal    (Knows! peri
                  (Knows! nao-north
                          (Knows! nao-south
                                  (exists [x? y? z? u? $X?]
                                          (and (not (= x? y?)) (not (= y? z?)) (not (= z? u?)) (Dodec u?)
                                               (N x? u?) (N y? u?) (N z? u?)
                                               ($X? x?) ($X? y?) ($X? z?))))))}


