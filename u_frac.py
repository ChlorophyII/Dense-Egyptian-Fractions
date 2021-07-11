import subprocess
import ast
from fractions import Fraction

class ChezSchemeError(Exception):
    """Exception raised by ChezScheme."""
    def __init__(self, message):
        self.message = message
        
def ufrac(D, r):
    """
    "ufrac" takes D, a list of positive integers and r, a nonnegative fraction
    as input and returns all unit fraction representations of r in D, 
    as a list of lists.

    It depends on "efrac.so", compiled ChezScheme code. Execute the following
    in shell to compile before the first use:

    echo '(load "ufrac-compile.scm")' | chez -q

    Examples: 
    >>> ufrac(list(range(1, 7)), Fraction(1))
    [[2, 3, 6], [1]] 
    >>> ufrac(list(range(1, 6)), Fraction(2))
    []
    >>> ufrac([2, 3, 4, 12], Fraction(1, 3))
    [[4, 12], [3]]
    >>> ufrac([2, 2, 4, 4], Fraction(1))
    [[2, 4, 4], [2, 2]]
    """
    if not(isinstance(D, list)):
        raise ValueError("D should be a list.")
    if not(all(d > 0 and isinstance(d, int) for d in D)):
        raise ValueError("Elements in D should be positive integers.")
    if not(isinstance(r, Fraction) and r >= 0):
        raise ValueError("r should be a nonnegative number of class Fraction.")

    prefix = 'echo \'(load "ufrac.so") (ufrac '
    D_str = '(list ' + ' '.join([str(d) for d in D]) + ')'
    r_str = str(r.numerator) + '/' + str(r.denominator)
    postfix = ')\' | chez -q'
    command = prefix + D_str + ' ' + r_str + postfix
    
    output = subprocess.getoutput(command)
    
    if "Exception" in output or "<procedure-" in output:
        if len(output) > 400:
            output = output[:400]+" ..."
        raise ChezSchemeError(output)

    output = ", ".join(output.split())
    output = output.replace("(","[").replace(")","]")
    
    solutions = ast.literal_eval(output)
    return solutions

def ufrac_es(D, r):
    """
    "ufrac_es" takes D, a list of positive integers and r, a nonnegative 
    fraction as input and returns a unit fraction representation of r in D as a
    list , if there is any, or False if there is none.

    It depends on "efrac.so", compiled ChezScheme code. Execute the following
    in shell to compile before the first use:

    echo '(load "ufrac-compile.scm")' | chez -q

    Examples: 
    >>> ufrac_es(list(range(1, 7)), Fraction(1))
    [1]
    >>> ufrac_es(list(range(1, 6)), Fraction(2))
    False
    >>> ufrac_es([2, 3, 4, 12], Fraction(1, 3))
    [3]
    >>> ufrac_es([2, 2, 4, 4], Fraction(1))
    [[2, 4, 4], [2, 2]]
    """
    if not(isinstance(D, list)):
        raise ValueError("D should be a list.")
    if not(all(d > 0 and isinstance(d, int) for d in D)):
        raise ValueError("Elements in D should be positive integers.")
    if not(isinstance(r, Fraction) and r >= 0):
        raise ValueError("r should be a nonnegative number of class Fraction.")

    prefix = 'echo \'(load "ufrac.so") (ufrac-es ' # different from ufrac
    D_str = '(list ' + ' '.join([str(d) for d in D]) + ')'
    r_str = str(r.numerator) + '/' + str(r.denominator)
    postfix = ')\' | chez -q'
    command = prefix + D_str + ' ' + r_str + postfix

    output = subprocess.getoutput(command)

    if "Exception" in output or "<procedure-" in output:
        if len(output) > 400:
            output = output[:400]+" ..."
        raise ChezSchemeError(output)
    if "#f" in output:
        return False

    output = ", ".join(output.split())
    output = output.replace("(","[").replace(")","]")

    solution = ast.literal_eval(output)
    return solution
