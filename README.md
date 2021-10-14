# Dense-Egyptian-Fractions
An algorithm for computing <cite>[unit fraction representations][4]</cite>, hence also <cite>[Egyptian fraction representations][5]</cite>.
<cite>[![DOI](https://zenodo.org/badge/104223004.svg)][6]</cite>

This algorithm can possibly be used to find the densest Egyptian fraction representations, or verify related conjectures. For example, Greg Martin conjectured that every integer greater than 4 can be the second largest denominator in an Egyptian fraction representation of 1. We have verified this conjecture for integers up to 6000. See [conjecture.md](./conjecture.md) for details.

## Table of Contents
- [Description](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#description)
- [Installation](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#installation)
    - [Install Chez Scheme](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#install-chez-scheme)
    - [Install Dense-Egyptian-Fractions](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#install-dense-egyptian-fractions)
    - [Install Python 3 (optional)](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#install-python-3-optional)
- [Usage](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#usage)
    - [Chez Scheme Interface](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#chez-scheme-interface)
        - [Basic Examples](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#basic-examples)
        - [Advanced Examples](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#advanced-examples)
    - [Python Interface](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#python-interface)
        - [Examples](https://github.com/ChlorophyII/Dense-Egyptian-Fractions#examples)
- [Conjecture](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#conjecture)
    - [Statement](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#statement)
    - [Examples](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#examples)
        - [The way to choose a desired c](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#the-way-to-choose-a-desired-c)
        - [Different values of c leading to different results](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#different-values-of-c-leading-to-different-results)
        - [Better c leading to better performance](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#better-c-leading-to-better-performance)
    - [Finding Witnesses](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#finding-witnesses)
    - [Results](https://github.com/ChlorophyII/Dense-Egyptian-Fractions/blob/master/conjecture.md#results)
        
## Description
The program computes unit fraction representations of a given positive rational number within a given set. The idea is from <cite>[Dense Egyptian Fractions][1]</cite>.

## Installation
This program is implemented in <cite>[Chez Scheme][2]</cite>, so one must install Chez Scheme first. 

### Install Chez Scheme
- For a Mac with an Intel CPU, the easiest way to install Chez Scheme is by using <cite>[Homebrew][3]</cite>, a great package manager.

	1. Paste the following in a macOS Terminal prompt.
    ```sh
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ```
	2. Then run ```brew install chezscheme```

- For Apple Silicon or other ARM platforms, say, a Raspberry Pi, follow the instructions on <cite>[Chez Scheme forked by racket][12]</cite>. <cite>[This file][13]</cite> contains detailed instructions. Note that the binary installed in this way is named `scheme` rather than `chez`. One may add the line `alias chez="scheme"` to their .zshrc or .bash_profile. Alternatively, one can replace any `chez` appeared in code in the following tutorial by `scheme`.

- For Windows or Linux, follow the instructions on <cite>[Chez Scheme][2]</cite>. <cite>[This file][7]</cite> contains detailed instructions.

### Install Dense-Egyptian-Fractions
1. Clone or download this repository and unzip it.
2. In terminal, change directory to Dense-Egyptian-Fractions.
3. Before the first use, execute

    ```sh
    echo '(load "ufrac-compile.scm")' | chez -q
    ```
    
    to compile files. This is required by the Python interface and also accelerates the program. 

### Install Python 3 (optional)
If you do not want to write scheme code, we also provide a Python interface as an option, which requires Python 3. It has all the major functionalities of the scheme version except for `efrac-es-progress`. If not installed, `brew install python3` on macOS or download from <cite>[the official website][11]</cite>. 

## Usage
The program mainly provides two procedures, `ufrac` and `ufrac-es`. The first procedure `ufrac` computes all unit fraction representations of the given rational in the given list of integers. The second procedure `ufrac-es` determines whether there exists a representation of the given rational in the list of integers and returns one if there exists one. The postfix "-es" stands for early-stopping.

There is another procedure, `ufrac-es-progress`, which is the same as `ufrac-es` except that it prints the progress of computing when verbose mode is turned on. 
### Chez Scheme Interface

1. Run `chez` in terminal to enter Chez Scheme interpreter.
2. Run `(load "ufrac.so")` in the interpreter to load the program.

#### Basic Examples  

```scheme
> (ufrac '(2 2 4 4) 1) ; a single quote creates a list
((2 4 4) (2 2))
> (ufrac (range 1 10) 3/2)
((1 2) (1 3 6))
> (ufrac (range 1 64) 4)
()
> (ufrac-es (range 1 64) 4)
#f
> (ufrac-es (range 1 184) 5)
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
 24 25 26 27 28 29 30 32 33 34 35 36 38 39 40 42 44 45 46 48
 50 51 52 54 55 56 60 62 63 65 66 68 70 72 75 76 77 78 80 81
 84 85 88 90 91 92 93 95 96 99 100 102 104 105 108 110 112
 114 115 116 117 120 126 130 133 136 140 143 144 145 152 153
 154 155 156 161 162 165 170 171 176 180 184)
> (rec-sum '(2 3 4)) ; rec-sum stands for sum of reciprocals
13/12
> (rec-sum (ufrac-es (range 1 65) 4))
4
> (rec-sum '())
0
>
```
#### Advanced Examples
Many set operations are implemented in "ufrac-math.scm" and they should be self-explanetory. As for referece of scheme, <cite>[The Scheme Programming Language][10]</cite> by R. Kent Dybvig, the principal developer of Chez Scheme, is a good start, and <cite>[Chez Scheme Version 9 User’s Guide][9]</cite> provides more Chez specific information. A more pedagogical and informative book is the Wizard book, <cite>[Structure and Interpretation of Computer Programs][8]</cite>, which is the holy bible of scheme. 

Notice that `set!` is a built-in procedure of scheme and "set-" as a prefix means set operation.  

```scheme
> (set! reps-5 (ufrac (range 1 184) 5))
> (length reps-5)
16
> (set-intersection '(1 2 3) '(2 3 4))
(2 3)
> (sets-intersection '((1 2 3) (2 3 4) (3 4 5)))
(3)
> (sets-intersection reps-5)
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
 24 25 26 27 28 30 32 33 34 35 36 38 39 40 42 44 45 48 50 51
 52 54 55 56 60 62 63 65 66 68 70 72 75 76 77 78 80 81 84 85
 88 90 91 92 93 99 102 104 105 108 110 112 115 116 117 126
 130 133 136 140 143 144 145 153 154 155 156 161 162 170 171
 180 184)
 > (map length reps-5)
(109 108 110 109 107 106 108 107 107 108 109 110 109 108 107
     106)
 > (set! rep-4 (car (ufrac (range 1 65) 4)))
 > (set-difference (sets-intersection reps-5) rep-4)
(17 19 21 23 25 32 34 38 39 44 50 51 55 62 66 68 70 72 75 76
 77 78 80 81 84 85 88 90 91 92 93 99 102 104 105 108 110 112
 115 116 117 126 130 133 136 140 143 144 145 153 154 155 156
 161 162 170 171 180 184)
> (set-symmetric-difference (sets-intersection reps-5) rep-4)
(17 19 21 23 25 32 34 38 39 44 50 51 55 62 66 68 70 72 75 76
 77 78 80 81 84 85 88 90 91 92 93 99 102 104 105 108 110 112
 115 116 117 126 130 133 136 140 143 144 145 153 154 155 156
 161 162 170 171 180 184)
> (set-dedupe '(1 2 2 3 4 3 5))
(1 2 4 3 5)
> (sort < '(1 2 4 3 5))
(1 2 3 4 5)
>
```

To use `ufrac-es-progress`, we need to turn on verbose mode. The algorithm has a tree structure, actually, more like a mine.  

```
> (set! verbose? #t)
> (ufrac-es-progress (range 1 184) 5)
progress:   6.2500000000%  killed branches: 42            #<date Sun Jul 11 12:34:39 2021> (2 2 4)
progress:  12.5000000000%  killed branches: 44            #<date Sun Jul 11 12:34:39 2021> (2 2 4)
progress:  14.0625000000%  killed branches: 48            #<date Sun Jul 11 12:34:39 2021> (2 2 2 2 4)
progress:  15.6250000000%  killed branches: 49            #<date Sun Jul 11 12:34:39 2021> (2 2 2 2 4)
progress:  16.6666666667%  killed branches: 51            #<date Sun Jul 11 12:34:39 2021> (3 2 2 2 4)
...
progress:  44.7791666667%  killed branches: 231           #<date Sun Jul 11 12:34:40 2021> (5 6 10 5 2 3 2 4)
progress:  44.7805555556%  killed branches: 233           #<date Sun Jul 11 12:34:40 2021> (5 6 10 5 2 3 2 4)
progress:  44.7819444444%  killed branches: 235           #<date Sun Jul 11 12:34:40 2021> (5 6 10 5 2 3 2 4)
progress:  44.7833333333%  killed branches: 236           #<date Sun Jul 11 12:34:40 2021> (5 6 10 5 2 3 2 4)
progress:  44.7837962963%  killed branches: 238           #<date Sun Jul 11 12:34:40 2021> (3 5 6 10 5 2 3 2 4)
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
 24 25 26 27 28 29 30 32 33 34 35 36 38 39 40 42 44 45 46 48
 50 51 52 54 55 56 60 62 63 65 66 68 70 72 75 76 77 78 80 81
 84 85 88 90 91 92 93 95 96 99 100 102 104 105 108 110 112
 114 115 116 117 120 126 130 133 136 140 143 144 145 152 153
 154 155 156 161 162 165 170 171 176 180 184)
>
```

Combining progress and the list at the end of each line, which we call treasure-map in the code, we can recover the location of the branch that we are exploring. Top levels with only one branch are ommitted from the treasure-map. Treasure-map tells us how the path are connected when branching starts. For example, 6.25% and (2 2 4) together indicates:

```
Level n                         (A)              
Level n+1           (A->a)             ~             (A->b) ~ (A->c) ~ (A->d)
Level n+2   (A->a->i)         ~         (A->a->ii)   | *** unknown world ***
Level n+3 {A->a->i->1} ~ (A->a->i->2)   | *** unknown world ***
    
(A->a): a branch, whose parent is (A) and siblings are (A->b), (A->c) and (A->d)
(A->a) ~ (A->b): (A->a) and (A->b) are siblings
{A->a->i->1}: The branch {A->a->i->1} has been explored 
```

Since level n has 1 branch, we assign weight 1 to `(A)`. Since there are 4 children of `(A)`, we assign weight 1/4 to each of them as children of `(A)`. Similarly, `(A->a->i)` and `{A->a->i->1}` have weight 1/2 as children of their parents respectively. Therefore, completing exploration of `{A->a->i->1}` indicates we have finished 1/4 * 1/2 * 1/2 = 1/16 = 6.25%.
    
This way of computing progress only gives us a way to locate the branch. It has nothing to do with time. For example, it takes "forever" to compute `(ufrac-es-progress (range 1 1230) 7)`. The number of killed branches keeps increasing at a constant speed, but the progress may come to temporary stop, because the branches being investigated are too deep and deep branches have insignificant weights.


### Python Interface
Scheme code must be compiled before the first use with

```sh
echo '(load "ufrac-compile.scm")' | chez -q
```
    
1. Use `python3 -i u_frac.py` or `python3` together with `from u_frac import *` to load the program. The underscore in `u_frac.py` is added to avoid conflict with `ufrac.scm`.

2. `ufrac` and `ufrac_es` are well-commented in Python.

    ```python
    >>> help(ufrac)
    >>> help(ufrac_es)
    ```
3. To create a list of numbers, use either `[1,2,3]` or `list(range(1, 4))`. Notice that `list(range(a,b))` creates a list [a, a+1, ..., b-1]. To create a rational number, or "Fraction" in Python, `Fraction(3)` gives 3 and `Fraction(3,7)` gives 3/7. Do not enter with slash, as `Fraction(1/3)=Fraction(6004799503160661, 18014398509481984)`, which is not the same as 'Fraction(1, 3)'.

#### Examples:

```python
>>> ufrac(list(range(2, 25)), Fraction(2))
[[2, 3, 4, 5, 6, 8, 9, 10, 15, 18, 20, 24]]
>>> ufrac([2, 2, 3, 3, 4, 5, 6, 6, 7, 8, 12], Fraction(3, 2))
[[2, 3, 4, 6, 6, 12], [2, 3, 3, 6, 6], [2, 3, 3, 4, 12], [2, 2, 3, 6], [2, 2, 4, 6, 12]]
>>> ufrac(list(range(1, 184)), Fraction(5))
[]
>>> reps_5 = ufrac(list(range(1, 185)), Fraction(5))
>>> len(reps_5)
16
>>> [max(rep) for rep in reps_5]
[184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184]
>>> sum(Fraction(1, d) for d in [1,2,3])
Fraction(11, 6)
>>> [sum(Fraction(1, d) for d in rep) for rep in reps_5]
[Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1), Fraction(5, 1)]
>>> ufrac_es(list(range(1, 184)), Fraction(5))
False
>>> ufrac_es(list(range(1, 185)), Fraction(5))
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 50, 51, 52, 54, 55, 56, 60, 62, 63, 65, 66, 68, 70, 72, 75, 76, 77, 78, 80, 81, 84, 85, 88, 90, 91, 92, 93, 95, 96, 99, 100, 102, 104, 105, 108, 110, 112, 114, 115, 116, 117, 120, 126, 130, 133, 136, 140, 143, 144, 145, 152, 153, 154, 155, 156, 161, 162, 165, 170, 171, 176, 180, 184]
```
    
    
[1]:http://www.ams.org/journals/tran/1999-351-09/S0002-9947-99-02327-2/S0002-9947-99-02327-2.pdf
[2]:https://github.com/cisco/ChezScheme
[3]:https://brew.sh
[4]:https://en.wikipedia.org/wiki/Unit_fraction#Finite_sums_of_unit_fractions
[5]:https://en.wikipedia.org/wiki/Egyptian_fraction
[6]:https://zenodo.org/badge/latestdoi/104223004
[7]:https://raw.githubusercontent.com/cisco/ChezScheme/master/BUILDING
[8]:https://web.mit.edu/alexmv/6.037/sicp.pdf
[9]:https://cisco.github.io/ChezScheme/csug9.5/csug9_5.pdf
[10]:https://www.scheme.com/tspl4/
[11]:https://www.python.org/downloads/
[12]:https://github.com/racket/ChezScheme
[13]:https://github.com/racket/ChezScheme/blob/master/BUILDING
