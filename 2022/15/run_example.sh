#!/bin/bash

gnatmake advent_15_z3
./advent_15_z3 < example > example.smt
cat >> example.smt << EOF
(assert (<= x 20))
(assert (<= y 20))
(check-sat)
(get-value (x y))
EOF
z3 example.smt
