## Conjecture
### Statement
*Every integer larger than 4 can be the second-largest denominator in an Egyptian fraction representation of 1.*

This conjecture has been verified for integers up to 6000. Now we describe our strategy for looking for witnesses, i.e., representations that verify the conjecture for certain integers. Given an integer d that is larger than 4, we look for representations where the largest denominator is of the form d\*c for some c. For d ≤ 20, we choose d by hand. For d > 20,  we choose a candidate c within a range, say, 1 < c ≤ 10000, so that the largest prime power of the denominator of 1-1/d-1/(d\*c) can be minimized. 

#### Examples
##### How we choose a desired c
Suppose d = 54. We run `(sld-choose-c 54 10)` to get a c that is smaller than 10. It returns 2. We have 1-1/54-1/(54 \* 2) = 35/36, and the largest prime power in 36 = 4 \* 9 is 9. If we run `(sld-choose-c 54 100)`, we will get 35 instead. We have 1-1/54-1/(54 \* 35) = 103/105. This time, 105 = 3 \* 5 \* 7. Since 7 is smaller than 9, it is considered a better choice when we enlarge our range to 100.

##### Different values of c lead to different results
Continuing our previous example, `(sld 54 2)` gives us  
```(2 3 9 36 54 108)```,  
that is, 1/2+1/3+1/9+1/36+1/54+1/108=1. Here `sld` stands for second largest denominator. If we run `(sld 54 35)`, we will have  
```(2 5 7 14 15 54 1890)```  
instead. Both verifies the conjecture for d=54.

##### A properly chosen c can drastically improve the performance
Consider the case when d = 1453. `(sld-choose-c 1453 10000)` yields 2905, while `(sld-choose-c 1453 100000)` returns 24700. If we use c = 2905 and run `(sld 1453 2905)`, it returns  
```(2 4 7 14 83 105 166 332 415 498 1453 4220965)```  
in around 65 seconds in our test. However, running `(sld 1453 24700)` returns  
```(2 4 13 19 20 25 52 95 1453 35889100)```  
in 0.002 seconds.

Therefore, if the program runs too slow or uses too much memory, we may increase the bound so that there can be a better choice of c.
#### Finding witnesses
The following script prints witnesses for 21 ≤ d ≤ 1000.

```scheme
(for-each (lambda (d)
            (display (sld d (sld-choose-c d 10000)))
            (newline))
	  (range 21 1000))
```
The following script prints witnesses for 1001 ≤ d ≤ 4000.

```scheme
(for-each (lambda (d)
            (display (sld d (sld-choose-c d 100000)))
            (newline))
	  (range 1001 4000))
```

Equivalently, the above scripts are encapsulated as follows.

```scheme
(sld-witness 21 1000 10000)
(sld-witness 1001 4000 100000)
```
#### Results
The table below lists witnesses for d ≤ 100. See [this table](conjecture_witnesses.md) for complete results and also [raw outputs](./conjecture_raw_outputs.txt). 

| d   | c   | denominators                        |
|-----|-----|-------------------------------------|
| 5   | 4   | 2 4 5 20                            |
| 6   | 2   | 2 4 6 12                            |
| 7   | 6   | 2 3 7 42                            |
| 8   | 3   | 2 3 8 24                            |
| 9   | 2   | 2 3 9 18                            |
| 10  | 3   | 2 5 6 10 30                         |
| 11  | 120 | 3 4 5 8 11 1320                     |
| 12  | 2   | 2 4 8 12 24                         |
| 13  | 12  | 2 4 6 13 156                        |
| 14  | 6   | 2 4 6 14 84                         |
| 15  | 4   | 2 4 6 15 60                         |
| 16  | 3   | 2 4 6 16 48                         |
| 17  | 16  | 2 4 8 16 17 272                     |
| 18  | 2   | 2 4 6 18 36                         |
| 19  | 56  | 2 4 8 14 19 1064                    |
| 20  | 3   | 3 4 5 12 15 20 60                   |
| 21  | 20  | 2 4 5 21 420                        |
| 22  | 10  | 2 4 5 22 220                        |
| 23  | 45  | 2 3 15 18 23 1035                   |
| 24  | 5   | 2 4 5 24 120                        |
| 25  | 4   | 2 4 5 25 100                        |
| 26  | 12  | 2 3 8 26 312                        |
| 27  | 35  | 2 5 7 14 21 27 945                  |
| 28  | 3   | 2 3 14 21 28 84                     |
| 29  | 28  | 2 4 7 14 29 812                     |
| 30  | 2   | 2 4 5 30 60                         |
| 31  | 30  | 2 5 6 10 31 930                     |
| 32  | 15  | 2 5 6 10 32 480                     |
| 33  | 10  | 2 5 6 10 33 330                     |
| 34  | 84  | 2 7 8 11 22 28 33 34 2856           |
| 35  | 6   | 2 5 6 10 35 210                     |
| 36  | 5   | 2 5 6 10 36 180                     |
| 37  | 36  | 2 3 9 36 37 1332                    |
| 38  | 18  | 2 3 9 36 38 684                     |
| 39  | 12  | 2 3 9 36 39 468                     |
| 40  | 3   | 2 5 6 10 40 120                     |
| 41  | 40  | 2 4 8 10 41 1640                    |
| 42  | 2   | 2 4 7 14 42 84                      |
| 43  | 42  | 2 3 7 43 1806                       |
| 44  | 21  | 2 3 7 44 924                        |
| 45  | 2   | 3 4 5 10 12 45 90                   |
| 46  | 45  | 3 4 5 9 12 46 2070                  |
| 47  | 140 | 2 5 7 10 28 47 6580                 |
| 48  | 7   | 2 3 7 48 336                        |
| 49  | 6   | 2 3 7 49 294                        |
| 50  | 14  | 2 5 7 10 28 50 700                  |
| 51  | 84  | 2 3 9 28 51 4284                    |
| 52  | 90  | 4 5 6 8 9 10 36 52 4680             |
| 53  | 105 | 2 5 7 14 15 53 5565                 |
| 54  | 35  | 2 5 7 14 15 54 1890                 |
| 55  | 21  | 2 5 7 14 15 55 1155                 |
| 56  | 3   | 2 3 7 56 168                        |
| 57  | 56  | 3 4 7 8 12 21 57 3192               |
| 58  | 28  | 3 4 7 8 12 21 58 1624               |
| 59  | 117 | 2 3 9 26 59 6903                    |
| 60  | 7   | 2 5 7 14 15 60 420                  |
| 61  | 60  | 2 4 5 30 61 3660                    |
| 62  | 30  | 2 4 5 30 62 1860                    |
| 63  | 20  | 2 4 5 30 63 1260                    |
| 64  | 15  | 2 4 5 30 64 960                     |
| 65  | 12  | 2 4 5 30 65 780                     |
| 66  | 10  | 2 4 5 30 66 660                     |
| 67  | 66  | 2 3 11 22 66 67 4422                |
| 68  | 33  | 2 3 11 22 66 68 2244                |
| 69  | 22  | 2 3 11 22 66 69 1518                |
| 70  | 6   | 2 4 5 30 70 420                     |
| 71  | 70  | 2 5 7 8 56 71 4970                  |
| 72  | 5   | 2 4 5 30 72 360                     |
| 73  | 72  | 2 4 8 9 73 5256                     |
| 74  | 36  | 2 4 8 9 74 2664                     |
| 75  | 4   | 2 4 5 30 75 300                     |
| 76  | 18  | 2 4 8 9 76 1368                     |
| 77  | 10  | 2 4 5 28 77 770                     |
| 78  | 12  | 2 4 8 9 78 936                      |
| 79  | 315 | 2 5 7 9 30 79 24885                 |
| 80  | 3   | 2 4 5 30 80 240                     |
| 81  | 8   | 2 4 8 9 81 648                      |
| 82  | 286 | 2 5 11 13 15 44 65 66 82 23452      |
| 83  | 165 | 3 4 5 11 12 33 83 13695             |
| 84  | 5   | 2 4 5 28 84 420                     |
| 85  | 84  | 3 4 5 7 30 35 85 7140               |
| 86  | 42  | 3 4 5 7 30 35 86 3612               |
| 87  | 28  | 3 4 5 7 30 35 87 2436               |
| 88  | 21  | 3 4 5 7 30 35 88 1848               |
| 89  | 88  | 2 4 8 11 44 89 7832                 |
| 90  | 2   | 2 4 5 30 90 180                     |
| 91  | 12  | 3 4 5 7 30 35 91 1092               |
| 92  | 45  | 3 4 5 9 15 36 92 4140               |
| 93  | 30  | 3 4 5 9 15 36 93 2790               |
| 94  | 140 | 4 5 6 7 8 14 30 94 13160            |
| 95  | 56  | 4 5 6 7 8 14 30 95 5320             |
| 96  | 7   | 3 4 5 7 30 35 96 672                |
| 97  | 969 | 3 4 5 15 17 38 57 68 85 95 97 93993 |
| 98  | 6   | 3 4 5 7 30 35 98 588                |
| 99  | 10  | 3 4 5 9 15 36 99 990                |
| 100 | 14  | 4 5 6 7 8 14 30 100 1400            |
