# Dense-Egyptian-Fractions
An algorithm for computing <cite>[unit fraction representations][4]</cite>, hence also <cite>[Egyptian fraction representations][5]</cite>.
<cite>[![DOI](https://zenodo.org/badge/104223004.svg)][6]</cite>

## Description
The program computes unit fraction representations of a given positive rational number within a given set. The idea is from <cite>[Dense Egyptian Fractions][1]</cite>.

## Dependency
This program is implemented in <cite>[Chez Scheme][2]</cite>, so one must install Chez Scheme. 

If you do not want to write scheme code, we also provide a Python interface as an option, which requires Python 3. It has all the major functionalities of the scheme version except for `efrac-dfss`.

### Install Chez Scheme
On macOS, the easiest way to install Chez Scheme is by using <cite>[Homebrew][3]</cite>, a great package manager.

1. Paste the following in a macOS Terminal prompt.

    ```sh
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ```
2. Then run ```brew install chezscheme```

If the two-line method fails or if you are using another operating system, follow the instructions on <cite>[Chez Scheme][2]</cite>. <cite>[This][7]</cite> file contains detailed instructions.

### Install Python 3
If not installed, `brew install python3` on macOS or download from <cite>[here][11]</cite>. 

## Usage
1. In terminal, change directory to Dense-Egyptian-Fractions.
2. Before the first use, execute

    ```sh
    echo '(load "ufrac-compile.scm")' | chez -q
    ```
    
    to compile files. This is required by the Python interface and also accelerates the program. 

### Chez Scheme Interface
The program mainly provides two procedures, `ufrac` and `ufrac-dfs`. The first computes all unit fraction representations of the given number in the given list, while the second determines whether there exists one and returns one if there exists. `ufrac-dfss` is the same as `ufrac-dfs` except that it tells the progress of computing in verbose mode. 

1. Run `chez` in terminal to enter Chez Scheme interpreter.
2. `(load "ufrac.so")` if you have compiled the files, or `(load "ufrac.scm")` if not.
3. The following provides some basic examples. 

    ```scheme
    > (ufrac '(2 2 4 4) 1)
    ((2 4 4) (2 2))
    > (ufrac (range 1 10) 3/2)
    ((1 2) (1 3 6))
    > (ufrac (range 1 64) 4)
    ()
    > (ufrac-dfs (range 1 64) 4)
    #f
    > (ufrac-dfs (range 1 184) 5)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
     24 25 26 27 28 29 30 32 33 34 35 36 38 39 40 42 44 45 46 48
     50 51 52 54 55 56 60 62 63 65 66 68 70 72 75 76 77 78 80 81
     84 85 88 90 91 92 93 95 96 99 100 102 104 105 108 110 112
     114 115 116 117 120 126 130 133 136 140 143 144 145 152 153
     154 155 156 161 162 165 170 171 176 180 184)
    > (rec-sum '(2 3 4))
    13/12
    > (rec-sum (ufrac-dfs (range 1 65) 4))
    4
    > (rec-sum '())
    0
    >
    ```

    Below is more advanced. More set operations are implemented in "ufrac-math.scm" and they should be self-explanetory. As for referece of scheme, <cite>[The Scheme Programming Language][10]</cite> by R. Kent Dybvig, the principal developer of Chez Scheme, is a good start, and <cite>[Chez Scheme Version 9 User’s Guide][9]</cite> provides more Chez specific information. A more pedagogical and informative book is the Wizard book, <cite>[Structure and Interpretation of Computer Programs][8]</cite>, which is the holy bible of scheme. 

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
4. To use `ufrac-dfss`, we need to turn on verbose mode. The algorithm has a tree structure, actually more like a mine.

    ```
    > (set! verbose? #t)
    > (ufrac-dfss (range 1 184) 5)
    progress:   6.25000000%  #<date Mon Jan 20 00:00:56 2020> (2 2 4)
    progress:  12.50000000%  #<date Mon Jan 20 00:00:56 2020> (2 2 4)
    progress:  14.06250000%  #<date Mon Jan 20 00:00:56 2020> (2 2 2 2 4)
    progress:  15.62500000%  #<date Mon Jan 20 00:00:56 2020> (2 2 2 2 4)
    progress:  16.66666667%  #<date Mon Jan 20 00:00:56 2020> (3 2 2 2 4)
    progress:  16.92708333%  #<date Mon Jan 20 00:00:56 2020> (4 3 2 2 2 4)
    ...
    progress:  44.81547619%  #<date Mon Jan 20 00:00:56 2020> (5 7 12 5 2 3 2 4)
    progress:  44.81572421%  #<date Mon Jan 20 00:00:56 2020> (4 5 7 12 5 2 3 2 4)
    progress:  44.81597222%  #<date Mon Jan 20 00:00:56 2020> (4 5 7 12 5 2 3 2 4)
    progress:  44.81609623%  #<date Mon Jan 20 00:00:56 2020> (2 4 5 7 12 5 2 3 2 4)
    progress:  44.81615823%  #<date Mon Jan 20 00:00:56 2020> (2 2 4 5 7 12 5 2 3 2 4)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
     24 25 26 27 28 29 30 32 33 34 35 36 38 39 40 42 44 45 46 48
     50 51 52 54 55 56 60 62 63 65 66 68 70 72 75 76 77 78 80 81
     84 85 88 90 91 92 93 95 96 99 100 102 104 105 108 110 112
     114 115 116 117 120 126 130 133 136 140 143 144 145 152 153
     154 155 156 161 162 165 170 171 176 180 184)
    >
    ```
	Combining progress and the list in the end, which we call treasure-map in the code, we can recover the location of the branch that we are exploring. Top levels with only one branch are ommitted from the treasure-map. Treasure-map tells us how the path are connected when branching starts. For example, 6.25% and (2 2 4) together indicates:

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
    
	This way of computing progress only gives us a way to locate the branch. It has nothing to do with time. For example, it takes forever to compute `(ufrac-dfss (range 1 500) 6)` and the progress gradually stops increasing at some point, because it is too deep and deep branches have small weights.


### Python Interface
Scheme code must be compiled before the first use with

```sh
echo '(load "ufrac-compile.scm")' | chez -q
```
    
1. `python3 -i ufrac.py` or `python3` and then `from u_frac import *`

2. `ufrac` and `ufrac_dfs` are well-commented in Python.

    ```python
    >>> help(ufrac)
    >>> help(ufrac_dfs)
    ```
3. To create a list of numbers, either `[1,2,3]` or `list(range(1, 4))`. Notice that `list(range(a,b))` creates a list [a, a+1, ..., b-1]. To create a rational number, or "Fraction" in Python, `Fraction(3)` gives 3 and `Fraction(3,7)` gives 3/7. Do not enter with slash, as `Fraction(1/3)=Fraction(6004799503160661, 18014398509481984)`, not equal to 'Fraction(1, 3)'.
4. Examples:

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
    >>> ufrac_dfs(list(range(1, 184)), Fraction(5))
    False
    >>> ufrac_dfs(list(range(1, 185)), Fraction(5))
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
