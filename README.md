# Dense-Egyptian-Fractions
An algorithm for dense Egyptian fractions
[![DOI](https://zenodo.org/badge/104223004.svg)](https://zenodo.org/badge/latestdoi/104223004)

### Description
This program is designed to find dense Egyptian fraction representations of a given positive rational number, where the denominators in the representation are chosen from a given set. The idea is from <cite>[Dense Egyptian Fractions][1]</cite>.

### Dependency
This program is implemented in <cite>[Chez Scheme][2]</cite>. So one has to install Chez Scheme first.

On OSX, the easiest way to install Chez Scheme is using <cite>[Homebrew][3]</cite>, a great package manager.

1. Paste the following at a Terminal prompt

```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
2. Then simply run ```brew install chezscheme```

If the two-line method fails, or if you are using another system, follow the instructions on <cite>[Chez Scheme][2]</cite>.

### Usage
The *.so* files are binary files compiled on OSX. On other platforms, one can compile the files by `(compile-file "<filename>.scm")`. Note: replace `<filename>` for each file.

The file can be used by `(load "<filename>.so")`. Our program only requires to `(load "efrac.so")`. If you cannot use the binary files, then simply replace the first three lines in "efrac.scm" by

```
(load "math.scm")
(load "ext.scm")
(load "efrac-branch.scm")
```
Then `(load "efrac.scm")`. Everything will be the same, except that uncompiled files are a little bit slower.

Here are a few samples. More detailed explanation is in **efrac** section in "Documentation.md".

```
> (efrac (range 1 10) 3/2)
((1 2) (1 3 6))
> (efrac (range 1 64) 4)
()
> (efrac (range 1 65) 4)
((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 20 22 24 26 27
  28 30 33 35 36 40 42 45 48 52 54 56 60 63 65))
```

[1]:http://www.ams.org/journals/tran/1999-351-09/S0002-9947-99-02327-2/S0002-9947-99-02327-2.pdf
[2]:https://github.com/cisco/ChezScheme
[3]:https://brew.sh

