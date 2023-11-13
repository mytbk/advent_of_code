#!/bin/bash

gnatmake advent_15_z3
./advent_15_z3 < /tmp/input > part2.smt
cat >> part2.smt << EOF
(assert (<= x 4000000))
(assert (<= y 4000000))
(check-sat)
(get-value (x y))
EOF
z3 part2.smt
