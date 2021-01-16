# Running Random Tests with Perltidy

The basic idea is to try to cause perltidy to fail in some way by running it on
a large number of random input parameters and files.  The runs which do this
work can run for many hours, processing hundreds of test files, each with
perhaps hundreds of parameter combinations.  The results can be checked at any
time, and the scripts are written so that they can stop and restart any time.
That's okay, computer time is cheap.  The hard part will be at the end, sifting
through the results.

When this type of testing was begun, several dozen problems were quickly
identified and fixed.  The most common problem was that an uninitialized
variable was referenced in some way.  It has been some time since a new problem
was detected with these scripts, but it is important to run these
tests periodically, and always before a release, because new coding and new
parameters may introduce bugs.

There are currently a set of three scripts for this work.

 -  random\_file\_generator.pl
 -  perltidy\_random\_setup.pl
 -  perltidy\_random\_run.pl

The method for using them is as follows on a linux system.  The three scripts
need to be in the PATH.  I do this by creating a symlink to them in my
personal bin directory.

## Prepare a temporary directory

First collect a large number (say 50 or more) of arbitrary perl scripts in a
single directory.  You may also include other arbitrary files, such as
text or html files.

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

For example, you might have a random script which has a line begining with
```=txt```, so perl will take that as the start of pod.  Then if the flag to
delete pod is set, the output file will be truncated and this could trigger a
warning that the output file looks too short.

Also run

```
./RUNME.sh
```

which will help scan the ```nohup.my``` file for certain keywords.  

## Additional scripts

The files of parameters which are automatically are long and contain
many parameters which are on by default or which are not relevant, such
as the various flags for controlling html.  A script which removes
these to assist in locating a problem is

 -  perltidy\_minimal\_flags.pl
