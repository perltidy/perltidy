# Running Random Tests with Perltidy

The tests which ship with perltidy check that its basic functionality is
correct.  Perltidy has well over 100 input parameters, and most of them are
checked in a simple way with these tests.  But when we consider that the
parameters do not each operate independently but rather interact with each
other, these tests barely "scratch the surface" in testing perltidy. It would
require an impossible number of tests to really do an in-depth check for
problems.  To illustrate, suppose we had just 100 input parameters and they
were simple binary switches, we would need 2^100, or about 10^30, tests to test
all possible combinations. But many of the flags can take more than 2 settings,
so the number of combinations in far greater.  For example, the line length
parameter takes any integer as value.  And the problem becomes even more
complex when we consider that problems among interacting parameters may only
occur on some particular sequence of input text.

What to do?  Several strategies have been developed to handle this problem. One
of these which has always been used is to check all source code changes
on a very large body of perl code with a variety of parameter settings.  This requires
several cpu hours of runs for each coding change.

Another approach, described here, is random testing.  The basic idea is
to try to cause perltidy to fail in some way by running it on a large number of
random input parameters and files.  The runs which do this work can run for
many hours, processing hundreds of test files, each with many thousands of
random parameter combinations.  The results can be checked at any time, and the
scripts are written so that they can stop and restart any time.  That's okay,
computer time is cheap.

This type of testing has been extremely helpful in making perltidy more robust.
Over 1300 issues have been identified with this method so far and have been
corrected.  All of these are extremely rare edge cases that would have been
difficult to find by any other method.  Perltidy has become very robust as a
result of this work, and the mean time for the discovery of a new convergence
issue is now about 50 cpu hours.

Another strategy which is employed at the same time is to turn on the
``DEVEL_MODE`` flags in perltidy which cause it to do numerous self-checks as
it runs.  These slow perltidy down, so they are only turned on during testing.

There are currently a set of three scripts for this work.

 -  random\_file\_generator.pl
 -  perltidy\_random\_setup.pl
 -  perltidy\_random\_run.pl

The method for using them is as follows on a linux system.  The three scripts
need to be in the PATH.  I do this by creating a symlink to them in my
personal bin directory.

## Prepare a temporary directory

First collect a large number of arbitrary perl scripts in a single directory.
You may also include other arbitrary files, such as text or html files.

Then create a temporary sub directory and enter it, say

```
mkdir random_junk ; 
cd random_junk
```

This directory will be deleted when testing is complete.

You can either test whatever version of perltidy is in your path, or test a
special version.  I prefer to test a special version for a couple of reasons.
The main one is that I will not cause a problem with the running process if I
am simultaneously updating the development version.  Also, this allows me to
turn on some flags in the code for extra checking.  To make a special version,
go back to the git home or to the latest distribution files and enter

```
./pm2pl
```

This will create a stand-alone version of perltidy which you should copy into your temporary
directory and rename ```perltidy.pl```.  You can also edit it and change all statements ```DEVEL_MODE => 0```
to be ```DEVEL_MODE => 1```.

## Prepare random files if desired

You have the option of either (1) processing any or all of the files in the
upper directory, or (2) processing a bunch of random files.  Both are useful,
but processing completely random files has been less effective in finding bugs
compared to processing actual perl scripts.  One reason is that with actual
scripts you can allow syntax checking by perl.  If you wish to create a bunch
of random files, enter a command similar to

```
random_file_generator.pl ../* 100
```

The first parameter(s) are some files to use to get started, and the last number is the number of files.
The files will have names ```ranfile.1```, ```ranfile.2```, and so on.

It is best to pass it at least 10 file names.  If you pass it more it will select what it wants to use.

## Run the setup program

Start the setup program and answer the questions

```
perltidy_random_setup.pl
```

You can usually hit enter to get default suggestions until you reach the main
menu.  You will be generating profiles here (a profile being a file with a
random set of perltidy parameters). The number of random profiles is set to 50
but I sometimes increase it to 100 or 200.  Every input file will be run
against every random profile, so this can significantly increase the total run
time. The main menu looks like this: 

```
R   - Read a config file
F   - Files:    FILES.txt
    Number of Files: 85
    First file     : ../ofile.4.12
    Last file      : ../nohup.my
P   - Profiles: 
    Number of Files: 49
    First profile     : profile.1
    Last profile      : profile.50
C   - Chain mode               : 2
D   - Delete good output?      : 1
S   - Syntax check?            : 0
V   - perltidy Version         : [default]
Q   - Quit without saving config file
W   - Write config, FILES.txt, PROFILES.txt, GO.sh and eXit
```

You should only turn on the syntax check if you are processing trusted files.
The reason is that this will cause BEGIN blocks to be executed.

The ```Chain Mode``` flag should normally be 0 or 2.  If it is 1 then every run
starts with the output of the previous run. This gives perltidy a good workout
but it can lead to a large number of intermediate files being saved and it be
hard to recreate a problem if intermediate files are not saved.

Use the menu to make changes, then when everything looks ok enter ```W```. This
will write a configuation file ```config.txt``` with this information.  You can
edit this config file to make more changes, or you can rerun the setup script
which will find it and read it.

## Start the run

```
./GO.sh
```

## Stopping and restarting

You can use control-z to interrupt the program and look at the ```nohup.my``` output file.
To actually stop a run, create a file ```stop.now```:

```
touch stop.now
```

and the program will stop and write a new script ```GO.sh```.  To restart the program later:

```
./GO.sh
``` 

## Examining the results

This is the time consuming part of the work because it is hard to automate.  To
look for problems or issues you need to search through the ```nohup.my``` file
which was written.  

Start by looking at the summary at the end which will summarize suspicious
results.  The program will have saved some intermediate suspicious results
which may be helpful.  But most problems and issues identified by the script
are not actual problems with perltidy, so going through these results can take
some time.

For example, you might have a random script which has a line beginning with
```=txt```, so perl will take that as the start of pod.  Then if the flag to
delete pod is set, the output file will be truncated and this could trigger a
warning that the output file looks too short.

Also run

```
./RUNME.sh
```

which will help scan the ```nohup.my``` file for certain keywords.  

## Utility for running convergence tests

Most of the problems discovered with random testing are cases where perltidy did not converge under unusual parameter settings.  In most cases the issues involved can be boiled down to a very short script with just a few parameters.  All of these issues have been fixed, but perltidy should be re-tested on these after every coding change.  A script to do this is

 -  run_convergence_tests.pl

The usage is simply

```
./run_convergence_tests.pl
```

It reads its database, ```run_convergence_tests.pl.data```, and runs the latest version of perltidy on each case.  This takes a little time because there are hundreds of cases in the database.  The last line of the output will show "OK" if there are no problems.

## Utility for running tokenizer tests

A similar script for running tests which test the tokenizer is

 -  run_tokenizer_tests.pl

The usage is 

```
./run_tokenizer_tests.pl
```

It reads its database, ```run_tokenizer_tests.pl.data```, and runs the latest version of perltidy on each case.  The last line of the output will show "OK" if there are no problems.

## Utility for stress testing with side comments

Another type of test which has been useful is a side comment test.  A script to do this type of test is

 - side_comment_test.pl

It works by taking an arbitrary perl script and first 'extruding' it into as many lines as possible. Then it adds side comments to each line (except where it does not make sense, such as here-doc targets and __END__ lines).  If this introduces a new error message from perltidy then something may be wrong and needs to be investigated.  Instructions for use are in the comments.  This utility has helped locate several problems in the Tokenizer module.

## Additional scripts

The files of parameters which are automatically are long and contain
many parameters which are on by default or which are not relevant, such
as the various flags for controlling html.  A script which removes
these to assist in locating a problem is

 -  perltidy\_minimal\_flags.pl
