Parsing and reduction of untyped lambda calculus.

There are two types of divergence: 
1. Cycles
2. Infinite expansion

My reduction prevents cycles. I.e., reducing `omega` gives `omega` back. However, infinite expansion is not caught.