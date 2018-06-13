# CODE SNIPPETS FOR TESTING PERLTIDY 

This directory contains some snippets of code to do simple checks of perltidy.
These are used to create the test file "snippets.t" in the source distribution.

The tests are intended to give a good overall check that perltidy is working
correctly at installation but they are by no means exhaustive. Thorough testing
of perltidy must be done against a very large body of perl code.

Run 'make' after any changes or additions to see if recent code changes have changed the perltidy formatting.

Folder 'tmp' contains the the most recent formatting results.
Folder 'expect' contains the previous expected output of perltidy.
The program run by make will give instructions for what to do if there are differences.

There are two types of files:
 - scripts (extension ".in") and 
 - parameters (extension ".par"). 

The scripts are simple code snippets, and the parameters are 
little .perltidyrc files.

## Name Matching Rules

Decisions about which snippets are run with which parameters are made based
on the file names.  Two rules are:

 - Rule 1: all scripts are run with the default parameters ("def.par")
 - Rule 2: if there is a parameter file with the same base name as the base name of the script file, then the script is also run with that parameter file. 
then that script is 

For example, consider the source file "rt20421.in".  The base name is 'rt20421'.
It will be run with the default parameters.  If a parameter file named "rt20421.par" 
exists, then it will also be run with this parameter file.

Incidentally, the numbered rt files correspond to the list at
(rt)[https://rt.cpan.org/Dist/Display.html?Status=Resolved;Queue=Perl-Tidy]

Besides these two rules, there are special naming rules for running a single
script with an arbitrary number of parameter files, and a single parameter file
with an arbitrary number of scripts.  To describe these we need to define
a "root name". The root name of a file is all characters of the file name
up to the first digit.  So for example, the root name of "rt20421" is just "rt".

The additional special rules are:

- Rule 3: For a given snippet file, if there is a parameter file whose base
  name equals the root name if the snippet, then the combination will be used.

- Rule 4: For a given parameter file, if there is a snippet file whose base
  name equals the root name if the parameter file, then the combination will be
used.

For example:

- Snippets 'rt20421.in' and 'rt34935.in' will be run against 'rt.par' if it
  exists. We probably do not want this because the 'rt..." files illustrate
specific issues discussed at rt.cpan.org. So we have to be careful when
creating new names.

- Parameter files 'style1.par' and 'style35.par' will be run against 'style.in' if it exists.

It is best to avoid file names which are pure digits because they can be difficult to search for. But leading digits followed by some non-digits, would be okay.

## How to name a new snippet and parameter file, if any:

- Give it a new base name with extension ".in".  End the name with some digits
  to avoid accidentally invoking unexpected parameter combinations. If you just
want to format with default parameters, skip to the the run 'make' step.

- All snippets are run with default parameters. If the new snippet is to also
  be run with special parameters, put them in a file with the same base name
but extension ".par". 

- To add are multiple snippets of the same class, give the ".in" files trailing
  digits but keep the special parameter file without the digits. For example,
to use a parameter file named "ce.par" against a number of source files, name
the source files "ce1.in", "ce2.in", and so on.  The values of the numbers are
not significant but normally they would start with 1 and increase.

- To add multiple parameter sets to a single source file, the source file a
  name without trailing digits and give each parameter file the same root name
as the source but with different trailing digits.  For example, source file
"spacetest.in" could be run with parameter sets "spacetest1.par",
"spacetest2.par", etc;

- To run a matrix of multiple sources and multiple parameters the easiest thing
  to do is combine the sources into a single source and use the previous
method. 

## How to recreate the snippet\*.t files

- In the snippets directory, run 'make'. This runs 'make\_expect.pl'.  This
program runs perltidy on the snippets. The output are in the tmp directory.
It also creates a file named 'RUNME.sh' which you can run 
if everything looks good. 

- ./RUNME.sh

- This re-creates the 'snippet#.t' files in the upper directory.
- Verify that everything is ok by running perl on the new '.t' files or by
going to the top directory and doing 

```
perl Makefile.PL
make
make test
```

## How to clean up a .par file

The '.par' parameter files are just .perltidyrc files, and they can be quite
lengthy.  To keep the snippets compact, I prefer to remove all comments and
default parameters, and to write the parameters with the short abbreviations.
The following command will do this.

```
  ../../examples/perltidyrc_dump.pl -s -q -d oldfile.par >newfile.par
```

If the output file 'newfile.par' looks ok then it can replace 'messy.par'.  You
could then add a single short comment to the new file if it would be helpful.

## Coverage

To update the list of covered parameters, run

```
  make_coverage_report.pl
```
