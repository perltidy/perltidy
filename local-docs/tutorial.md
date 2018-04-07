# A Brief Perltidy Tutorial

Perltidy can save you a lot of tedious editing if you spend a few
minutes learning to use it effectively.  Perltidy is highly
configurable, but for many programmers the default parameter set will be
satisfactory, with perhaps a few additional parameters to account for
style preferences.

This tutorial assumes that perltidy has been installed on your system.
Installation instructions accompany the package.  To follow along with
this tutorial, please find a small Perl script and place a copy in a
temporary directory.  For example, here is a small (and silly) script:

    print "Help Desk -- What Editor do you use?";
    chomp($editor = <STDIN>);
    if ($editor =~ /emacs/i) {
      print "Why aren't you using vi?\n";
    } elsif ($editor =~ /vi/i) {
      print "Why aren't you using emacs?\n";
    } else {
      print "I think that's the problem\n";
    }

It is included in the `docs` section of the distribution.

## A First Test

Assume that the name of your script is `testfile.pl`.  You can reformat it
with the default options to use the style recommended in the perlstyle man
pages with the command:

    perltidy testfile.pl

For safety, perltidy never overwrites your original file.  In this case,
its output will go to a file named `testfile.pl.tdy`, which you should
examine now with your editor.  Here is what the above file looks like
with the default options:

    print "Help Desk -- What Editor do you use?";
    chomp( $editor = <STDIN> );
    if ( $editor =~ /emacs/i ) {
        print "Why aren't you using vi?\n";
    }
    elsif ( $editor =~ /vi/i ) {
        print "Why aren't you using emacs?\n";
    }
    else {
        print "I think that's the problem\n";
    }

You'll notice an immediate style change from the "cuddled-else" style of
the original to the default "non-cuddled-else" style.  This is because
perltidy has to make some kind of default selection of formatting
options, and this default tries to follow the suggestions in the
perlstyle man pages.  

If you prefer the original "cuddled-else" style, don't worry, you can
indicate that with a **-ce** flag.  So if you rerun with that flag

    perltidy -ce testfile.pl

you will see a return to the original "cuddled-else" style.  There are
many more parameters for controlling style, and some of the most useful
of these are discussed below.  

## Indentation

Another noticeable difference between the original and the reformatted
file is that the indentation has been changed from 2 spaces to 4 spaces.
That's because 4 spaces is the default.  You may change this to be a
different number with **-i=n**.

To get some practice, try these examples, and examine the resulting
`testfile.pl.tdy` file:

    perltidy -i=8 testfile.pl

This changes the default of 4 spaces per indentation level to be 8.  Now
just to emphasize the point, try this and examine the result:

    perltidy -i=0 testfile.pl

There will be no indentation at all in this case.

## Input Flags

This is a good place to mention a few points regarding the input flags.
First, for each option, there are two forms, a long form and a short
form, and either may be used.  

For example, if you want to change the number of columns corresponding to one
indentation level to 3 (from the default of 4) you may use either

    -i=3   or  --indent-columns=3

The short forms are convenient for entering parameters by hand, whereas
the long forms, though often ridiculously long, are self-documenting and
therefore useful in configuration scripts.  You may use either one or
two dashes ahead of the parameters.  Also, the '=' sign is optional, 
and may be a single space instead.  However, the value of a parameter
must NOT be adjacent to the flag, like this **-i3** (WRONG).  Also,
flags must be input separately, never bundled together.

## Line Length and Continuation Indentation.

If you change the indentation spaces you will probably also need to
change the continuation indentation spaces with the parameter **-ci=n**.
The continuation indentation is the extra indentation -- 2 spaces by
default -- given to that portion of a long line which has been placed
below the start of a statement.  For example:

    croak "Couldn't pop genome file"
      unless sysread( $impl->{file}, $element, $impl->{group} )
      and truncate( $impl->{file}, $new_end );

There is no fixed rule for setting the value for **-ci=n**, but it should
probably not exceed one-half of the number of spaces of a full
indentation level.

In the above snippet, the statement was broken into three lines.  The
actual number is governed by a parameter, the maximum line length, as
well as by what perltidy considers to be good break points.  The maximum
line length is 80 characters by default.  You can change this to be any
number **n** with the **-l=n** flag.  Perltidy tries to produce lines
which do not exceed this length, and it does this by finding good break
points.  For example, the above snippet would look like this with
**perltidy -l=40**:

    croak "Couldn't pop genome file"
      unless
      sysread( $impl->{file}, $element,
        $impl->{group} )
      and
      truncate( $impl->{file}, $new_end );

You may be wondering what would happen with, say, **-l=1**.  Go 
ahead and try it.

## Tabs or Spaces?

With indentation, there is always a tab issue to resolve.  By default,
perltidy will use leading ascii space characters instead of tabs.  The
reason is that this will be displayed correctly by virtually all
editors, and in the long run, will avoid maintenance problems.  

However, if you prefer, you may have perltidy entab the leading
whitespace of a line with the command **-et=n**, where **n** is the number
of spaces which will be represented by one tab.  But note that your text
will not be displayed properly unless viewed with software that is
configured to display **n** spaces per tab.

## Input/Output Control

In the first example, we saw that if we pass perltidy the name
of a file on the command line, it reformats it and creates a
new filename by appending an extension, `.tdy`.  This is the
default behavior, but there are several other options.

On most systems, you may use wildcards to reformat a whole batch of
files at once, like this for example:

    perltidy *.pl

and in this case, each of the output files will be have a name equal to
the input file with the extension `.tdy` appended.  If you decide that
the formatting is acceptable, you will want to backup your originals and
then remove the `.tdy` extensions from the reformatted files.  There is
an powerful perl script called `rename` that can be used for this
purpose; if you don't have it, you can find it for example in **The Perl
Cookbook**.

If you find that the formatting done by perltidy is usually acceptable,
you may want to save some effort by letting perltidy do a simple backup
of the original files and then reformat them in place.  You specify this
with a **-b** flag.  For example, the command

    perltidy -b *.pl

will rename the original files by appending a `.bak` extension, and then
create reformatted files with the same names as the originals.  (If you don't
like the default backup extension choice `.bak`, the manual tells how to
change it).  Each time you run perltidy with the **-b** option, the previous
`.bak` files will be overwritten, so please make regular separate backups.

If there is no input filename specified on the command line, then input
is assumed to come from standard input and output will go to standard
output.  On systems with a Unix-like interface, you can use perltidy as
a filter, like this:

    perltidy <somefile.pl >newfile.pl

What happens in this case is that the shell takes care of the redirected
input files, '&lt;somefile.pl', and so perltidy never sees the filename.
Therefore, it knows to use the standard input and standard output
channels.

If you are executing perltidy on a file and want to force the output
to standard output, rather than create a `.tdy` file, you can
indicate this with the flag **-st**, like this:

    perltidy somefile.pl -st >otherfile.pl

You can also control the name of the output file with the **-o** flag,
like this:

    perltidy testfile.pl -o=testfile.new.pl

## Style Variations

Perltidy has to make some kind of default selection of formatting
options, and its choice is to try to follow the suggestions in the
perlstyle man pages.  Many programmers more or less follow these
suggestions with a few exceptions.  In this section we will
look at just a few of the most commonly used style parameters.  Later,
you may want to systematically develop a set of style
parameters with the help of
the perltidy **stylekey** web page at
http://perltidy.sourceforge.net/stylekey.html

- **-ce**, cuddled elses

    If you prefer cuddled elses, use the **-ce** flag.  

- **-bl**, braces left

    Here is what the `if` block in the above script looks like with **-bl**:

        if ( $editor =~ /emacs/i )
        {
            print "Why aren't you using vi?\n";
        }
        elsif ( $editor =~ /vi/i )
        {
            print "Why aren't you using emacs?\n";
        }
        else
        {
            print "I think that's the problem\n";
        }

- **-lp**, Lining up with parentheses

    The **-lp** parameter can enhance the readability of lists by adding
    extra indentation.  Consider:

            %romanNumerals = (
                one   => 'I',
                two   => 'II',
                three => 'III',
                four  => 'IV',
                five  => 'V',
                six   => 'VI',
                seven => 'VII',
                eight => 'VIII',
                nine  => 'IX',
                ten   => 'X'
            );

    With the **-lp** flag, this is formatted as:

            %romanNumerals = (
                               one   => 'I',
                               two   => 'II',
                               three => 'III',
                               four  => 'IV',
                               five  => 'V',
                               six   => 'VI',
                               seven => 'VII',
                               eight => 'VIII',
                               nine  => 'IX',
                               ten   => 'X'
                             );

    which is preferred by some.  (I've actually used **-lp** and **-cti=1** to
    format this block.  The **-cti=1** flag causes the closing paren to align
    vertically with the opening paren, which works well with the **-lp**
    indentation style).  An advantage of **-lp** indentation are that it
    displays lists nicely.  A disadvantage is that deeply nested lists can
    require a long line length.

- **-bt**,**-pt**,**-sbt**:  Container tightness

    These are parameters for controlling the amount of space within
    containing parentheses, braces, and square brackets.  The example below
    shows the effect of the three possible values, 0, 1, and 2, for the case
    of parentheses:

        if ( ( my $len_tab = length( $tabstr ) ) > 0 ) {  # -pt=0
        if ( ( my $len_tab = length($tabstr) ) > 0 ) {    # -pt=1 (default)
        if ((my $len_tab = length($tabstr)) > 0) {        # -pt=2

    A value of 0 causes all parens to be padded on the inside with a space,
    and a value of 2 causes this never to happen.  With a value of 1, spaces
    will be introduced if the item within is more than a single token.

## Configuration Files

While style preferences vary, most people would agree that it is
important to maintain a uniform style within a script, and this is a
major benefit provided by perltidy.  Once you have decided on which, if
any, special options you prefer, you may want to avoid having to enter
them each time you run it.  You can do this by creating a special file
named `.perltidyrc` in either your home directory, your current
directory, or certain system-dependent locations.  (Note the leading "."
in the file name).  

A handy command to know when you start using a configuration file is

    perltidy -dpro

which will dump to standard output the search that perltidy makes when
looking for a configuration file, and the contents of the one that it
selects, if any.  This is one of a number of useful "dump and die"
commands, in which perltidy will dump some information to standard
output and then immediately exit.  Others include **-h**, which dumps
help information, and **-v**, which dumps the version number.

Another useful command when working with configuration files is

    perltidy -pro=file

which causes the contents of `file` to be used as the configuration
file instead of a `.perltidyrc` file.  With this command, you can
easily switch among several different candidate configuration files
during testing.

This `.perltidyrc` file is free format.  It is simply a list of
parameters, just as they would be entered on a command line.  Any number
of lines may be used, with any number of parameters per line, although
it may be easiest to read with one parameter per line.  Blank lines are
ignored, and text after a '#' is ignored to the end of a line.

Here is an example of a `.perltidyrc` file:

    # This is a simple of a .perltidyrc configuration file
    # This implements a highly spaced style
    -bl    # braces on new lines
    -pt=0  # parens not tight at all
    -bt=0  # braces not tight
    -sbt=0 # square brackets not tight

If you experiment with this file, remember that it is in your directory,
since if you are running on a Unix system, files beginning with a "."
are normally hidden.  

If you have a `.perltidyrc` file, and want perltidy to ignore it,
use the **-npro** flag on the command line.

## Error Reporting

Let's run through a 'fire drill' to see how perltidy reports errors.  Try
introducing an extra opening brace somewhere in a test file.  For example,
introducing an extra brace in the file listed above produces the following
message on the terminal (or standard error output):

    ## Please see file testfile.pl.ERR!

Here is what `testfile.pl.ERR` contains:

    10:    final indentation level: 1
    
    Final nesting depth of '{'s is 1
    The most recent un-matched '{' is on line 6
    6: } elsif ($temperature < 68) {{
                                   ^

This shows how perltidy will, by default, write error messages to a file
with the extension `.ERR`, and it will write a note that it did so to
the standard error device.  If you would prefer to have the error
messages sent to standard output, instead of to a `.ERR` file, use the
**-se** flag.

Almost every programmer would want to see error messages of this type,
but there are a number of messages which, if reported, would be
annoying.  To manage this problem, perltidy puts its messages into two
categories: errors and warnings.  The default is to just report the
errors, but you can control this with input flags, as follows:

    flag  what this does
    ----  --------------
          default: report errors but not warnings
    -w    report all errors and warnings
    -q    quiet! do not report either errors or warnings

The default is generally a good choice, but it's not a bad idea to check
programs with **-w** occasionally, especially if your are looking for a
bug.  For example, it will ask if you really want '=' instead of '=~' in
this line:

    $line = s/^\s*//;

This kind of error can otherwise be hard to find.

## The Log File

One last topic that needs to be touched upon concerns the `.LOG` file.
This is where perltidy records messages that are not normally of any
interest, but which just might occasionally be useful.  This file is not
saved, though, unless perltidy detects that it has made a mistake or you
ask for it to be saved.

There are a couple of ways to ask perltidy to save a log file.  To
create a relatively sparse log file, use

    perltidy -log testfile.pl

and for a verbose log file, use

    perltidy -g testfile.pl

The difference is that the first form only saves detailed information at
least every 50th line, while the second form saves detailed information
about every line.

So returning to our example, lets force perltidy to save a
verbose log file by issuing the following command

    perltidy -g testfile.pl

You will find that a file named `testfile.pl.LOG` has been
created in your directory.  

If you open this file, you will see that it is a text file with a
combination of warning messages and informative messages.  All you need
to know for now is that it exists; someday it may be useful.

## Using Perltidy as a Filter on Selected Text from an Editor

Most programmer's editors allow a selected group of lines to be passed
through an external filter.  Perltidy has been designed to work well as
a filter, and it is well worthwhile learning the appropriate commands to
do this with your editor.  This means that you can enter a few
keystrokes and watch a block of text get reformatted.  If you are not
doing this, you are missing out of a lot of fun!  You may want to supply
the **-q** flag to prevent error messages regarding incorrect syntax,
since errors may be obvious in the indentation of the reformatted text.
This is entirely optional, but if you do not use the **-q** flag, you
will need to use the undo keys in case an error message appears on the
screen. 

For example, within the **vim** editor it is only necessary to select the
text by any of the text selection methods, and then issue the command
!perltidy in command mode.  Thus, an entire file can be formatted using

    :%!perltidy -q

or, without the **-q** flag, just

    :%!perltidy

It isn't necessary to format an entire file, however.  Perltidy will
probably work well as long as you select blocks of text whose braces,
parentheses, and square brackets are properly balanced.  You can
even format an `elsif` block without the leading `if` block, as
long as the text you select has all braces balanced.

For the **emacs** editor, first mark a region and then pipe it through
perltidy.  For example, to format an entire file, select it with `C-x h` 
and then pipe it with `M-1 M-|` and then `perltidy`.  The numeric
argument, `M-1` causes the output from perltidy to replace the marked
text.  See "GNU Emacs Manual" for more information,
http://www.gnu.org/manual/emacs-20.3/html\_node/emacs\_toc.html

If you have difficulty with an editor, try the **-st** flag, which will
force perltidy to send output to standard output.  This might be needed,
for example, if the editor passes text to perltidy as temporary filename
instead of through the standard input.  If this works, you might put the
**-st** flag in your `.perltidyrc` file.

If you have some tips for making perltidy work with your editor, and
are willing to share them, please email me (see below) and I'll try to
incorporate them in this document or put up a link to them.

After you get your editor and perltidy successfully talking to each
other, try formatting a snippet of code with a brace error to see what
happens.  (Do not use the quiet flag, **-q**, for this test).  Perltidy
will send one line starting with `##` to standard error output.  Your
editor may either display it at the top of the reformatted text or at
the bottom (or even midstream!).  You probably cannot control this, and
perltidy can't, but you need to know where to look when an actual error
is detected.

## Writing an HTML File

Perltidy can switch between two different output modes.  We have been
discussing what might be called its "beautifier" mode, but it can also
output in HTML.  To do this, use the **-html** flag, like this:

    perltidy -html testfile.pl

which will produce a file `testfile.pl.html`.  There are many
parameters available for adjusting the appearance of an HTML file, but a
very easy way is to just write the HTML file with this simple command
and then edit the stylesheet which is embedded at its top.

One important thing to know about the **-html** flag is that perltidy can
either send its output to its beautifier or to its HTML writer, but
(unfortunately) not both in a single run.  So the situation can be
represented like this:

                     ------------
                     |          |     --->beautifier--> testfile.pl.tdy
    testfile.pl -->  | perltidy | -->
                     |          |     --->HTML -------> testfile.pl.html
                     ------------

And in the future, there may be more output filters.  So if you would
like to both beautify a script and write it to HTML, you need to do it
in two steps.

## Summary

That's enough to get started using perltidy.  
When you are ready to create a `.perltidyrc` file, you may find it
helpful to use the `stylekey` page as a guide at
http://perltidy.sourceforge.net/stylekey.html

Many additional special
features and capabilities can be found in the manual pages for perltidy
at
http://perltidy.sourceforge.net/perltidy.html

We hope that perltidy makes perl programming a little more fun.
Please check the perltidy
web site http://perltidy.sourceforge.net occasionally
for updates.

The author may be contacted at perltidy at users.sourceforge.net.
