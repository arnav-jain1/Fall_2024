NFA
![[Pasted image 20240917141359.png]]
NFA table

| state | 0     | 1    |
| ----- | ----- | ---- |
| q0    | q0,q1 | q0q1 |
| q1    | q2    | q2   |
| +q2   | q2    | q2   |
DFA table

| state     | 0        | 1        |
| --------- | -------- | -------- |
| q0        | q0,q1,q2 | q0,q1,q2 |
| +q0,q1,q2 | q0,q1,q2 | q0,q1,q2 |

DFA
![[Pasted image 20240917141438.png]]