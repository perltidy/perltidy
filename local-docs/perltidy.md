# NAME

perltidy - a perl script indenter and reformatter

# SYNOPSIS

    perltidy [ options ] file1 file2 file3 ...
            (output goes to file1.tdy, file2.tdy, file3.tdy, ...)
    perltidy [ options ] file1 -o outfile
    perltidy [ options ] file1 -st >outfile
    perltidy [ options ] <infile >outfile

# DESCRIPTION

Perltidy reads a perl script and writes an indented, reformatted script.

Many users will find enough information in ["EXAMPLES"](#examples) to get 
started.  New users may benefit from the short tutorial 
which can be found at
http://perltidy.sourceforge.net/tutorial.html

A convenient aid to systematically defining a set of style parameters
can be found at
http://perltidy.sourceforge.net/stylekey.html

Perltidy can produce output on either of two modes, depending on the
existence of an **-html** flag.  Without this flag, the output is passed
through a formatter.  The default formatting tries to follow the
recommendations in perlstyle(1), but it can be controlled in detail with
numerous input parameters, which are described in ["FORMATTING
OPTIONS"](#formatting-options).  

When the **-html** flag is given, the output is passed through an HTML
formatter which is described in ["HTML OPTIONS"](#html-options).  

# EXAMPLES

    perltidy somefile.pl

This will produce a file `somefile.pl.tdy` containing the script reformatted
using the default options, which approximate the style suggested in 
perlstyle(1).  The source file `somefile.pl` is unchanged.

    perltidy *.pl

Execute perltidy on all `.pl` files in the current directory with the
default options.  The output will be in files with an appended `.tdy`
extension.  For any file with an error, there will be a file with extension
`.ERR`.

    perltidy -b file1.pl file2.pl

Modify `file1.pl` and `file2.pl` in place, and backup the originals to
`file1.pl.bak` and `file2.pl.bak`.  If `file1.pl.bak` and/or `file2.pl.bak`
already exist, they will be overwritten.

    perltidy -b -bext='/' file1.pl file2.pl

Same as the previous example except that the backup files `file1.pl.bak` and `file2.pl.bak` will be deleted if there are no errors.

    perltidy -gnu somefile.pl

Execute perltidy on file `somefile.pl` with a style which approximates the
GNU Coding Standards for C programs.  The output will be `somefile.pl.tdy`.

    perltidy -i=3 somefile.pl

Execute perltidy on file `somefile.pl`, with 3 columns for each level of
indentation (**-i=3**) instead of the default 4 columns.  There will not be any
tabs in the reformatted script, except for any which already exist in comments,
pod documents, quotes, and here documents.  Output will be `somefile.pl.tdy`. 

    perltidy -i=3 -et=8 somefile.pl

Same as the previous example, except that leading whitespace will
be entabbed with one tab character per 8 spaces.

    perltidy -ce -l=72 somefile.pl

Execute perltidy on file `somefile.pl` with all defaults except use "cuddled
elses" (**-ce**) and a maximum line length of 72 columns (**-l=72**) instead of
the default 80 columns.  

    perltidy -g somefile.pl

Execute perltidy on file `somefile.pl` and save a log file `somefile.pl.LOG`
which shows the nesting of braces, parentheses, and square brackets at
the start of every line.

    perltidy -html somefile.pl

This will produce a file `somefile.pl.html` containing the script with
html markup.  The output file will contain an embedded style sheet in
the <HEAD> section which may be edited to change the appearance.

    perltidy -html -css=mystyle.css somefile.pl

This will produce a file `somefile.pl.html` containing the script with
html markup.  This output file will contain a link to a separate style
sheet file `mystyle.css`.  If the file `mystyle.css` does not exist,
it will be created.  If it exists, it will not be overwritten.

    perltidy -html -pre somefile.pl

Write an html snippet with only the PRE section to `somefile.pl.html`.
This is useful when code snippets are being formatted for inclusion in a
larger web page.  No style sheet will be written in this case.  

    perltidy -html -ss >mystyle.css

Write a style sheet to `mystyle.css` and exit.

    perltidy -html -frm mymodule.pm

Write html with a frame holding a table of contents and the source code.  The
output files will be `mymodule.pm.html` (the frame), `mymodule.pm.toc.html`
(the table of contents), and `mymodule.pm.src.html` (the source code).

# OPTIONS - OVERVIEW

The entire command line is scanned for options, and they are processed
before any files are processed.  As a result, it does not matter
whether flags are before or after any filenames.  However, the relative
order of parameters is important, with later parameters overriding the
values of earlier parameters.  

For each parameter, there is a long name and a short name.  The short
names are convenient for keyboard input, while the long names are
self-documenting and therefore useful in scripts.  It is customary to
use two leading dashes for long names, but one may be used.

Most parameters which serve as on/off flags can be negated with a
leading "n" (for the short name) or a leading "no" or "no-" (for the
long name).  For example, the flag to outdent long quotes is **-olq**
or **--outdent-long-quotes**.  The flag to skip this is **-nolq**
or **--nooutdent-long-quotes** or **--no-outdent-long-quotes**.

Options may not be bundled together.  In other words, options **-q** and
**-g** may NOT be entered as **-qg**.

Option names may be terminated early as long as they are uniquely identified.
For example, instead of **--dump-token-types**, it would be sufficient to enter
**--dump-tok**, or even **--dump-t**, to uniquely identify this command.

## I/O control

The following parameters concern the files which are read and written.

- **-h**,    **--help** 

    Show summary of usage and exit.

- **-o**=filename,    **--outfile**=filename  

    Name of the output file (only if a single input file is being
    processed).  If no output file is specified, and output is not
    redirected to the standard output, the output will go to `filename.tdy`.

- **-st**,    **--standard-output**

    Perltidy must be able to operate on an arbitrarily large number of files
    in a single run, with each output being directed to a different output
    file.  Obviously this would conflict with outputting to the single
    standard output device, so a special flag, **-st**, is required to
    request outputting to the standard output.  For example,

        perltidy somefile.pl -st >somefile.new.pl

    This option may only be used if there is just a single input file.  
    The default is **-nst** or **--nostandard-output**.

- **-se**,    **--standard-error-output**

    If perltidy detects an error when processing file `somefile.pl`, its
    default behavior is to write error messages to file `somefile.pl.ERR`.
    Use **-se** to cause all error messages to be sent to the standard error
    output stream instead.  This directive may be negated with **-nse**.
    Thus, you may place **-se** in a `.perltidyrc` and override it when
    desired with **-nse** on the command line.

- **-oext**=ext,    **--output-file-extension**=ext  

    Change the extension of the output file to be `ext` instead of the
    default `tdy` (or `html` in case the -**-html** option is used).
    See ["Specifying File Extensions"](#specifying-file-extensions).

- **-opath**=path,    **--output-path**=path  

    When perltidy creates a filename for an output file, by default it merely
    appends an extension to the path and basename of the input file.  This
    parameter causes the path to be changed to `path` instead.

    The path should end in a valid path separator character, but perltidy will try
    to add one if it is missing.

    For example

        perltidy somefile.pl -opath=/tmp/

    will produce `/tmp/somefile.pl.tdy`.  Otherwise, `somefile.pl.tdy` will
    appear in whatever directory contains `somefile.pl`.

    If the path contains spaces, it should be placed in quotes.

    This parameter will be ignored if output is being directed to standard output,
    or if it is being specified explicitly with the **-o=s** parameter.

- **-b**,    **--backup-and-modify-in-place**

    Modify the input file or files in-place and save the original with the
    extension `.bak`.  Any existing `.bak` file will be deleted.  See next
    item for changing the default backup extension, and for eliminating the
    backup file altogether.  

    A **-b** flag will be ignored if input is from standard input or goes to
    standard output, or if the **-html** flag is set.  

    In particular, if you want to use both the **-b** flag and the **-pbp**
    (--perl-best-practices) flag, then you must put a **-nst** flag after the
    **-pbp** flag because it contains a **-st** flag as one of its components,
    which means that output will go to the standard output stream.

- **-bext**=ext,    **--backup-file-extension**=ext  

    This parameter serves two purposes: (1) to change the extension of the backup
    file to be something other than the default `.bak`, and (2) to indicate
    that no backup file should be saved.

    To change the default extension to something other than `.bak` see
    ["Specifying File Extensions"](#specifying-file-extensions).

    A backup file of the source is always written, but you can request that it
    be deleted at the end of processing if there were no errors.  This is risky
    unless the source code is being maintained with a source code control
    system.  

    To indicate that the backup should be deleted include one forward slash,
    **/**, in the extension.  If any text remains after the slash is removed
    it will be used to define the backup file extension (which is always
    created and only deleted if there were no errors).

    Here are some examples:

        Parameter           Extension          Backup File Treatment
        <-bext=bak>         F<.bak>            Keep (same as the default behavior)
        <-bext='/'>         F<.bak>            Delete if no errors
        <-bext='/backup'>   F<.backup>         Delete if no errors
        <-bext='original/'> F<.original>       Delete if no errors

- **-w**,    **--warning-output**             

    Setting **-w** causes any non-critical warning
    messages to be reported as errors.  These include messages
    about possible pod problems, possibly bad starting indentation level,
    and cautions about indirect object usage.  The default, **-nw** or
    **--nowarning-output**, is not to include these warnings.

- **-q**,    **--quiet**             

    Deactivate error messages and syntax checking (for running under
    an editor). 

    For example, if you use a vi-style editor, such as vim, you may execute
    perltidy as a filter from within the editor using something like

        :n1,n2!perltidy -q

    where `n1,n2` represents the selected text.  Without the **-q** flag,
    any error message may mess up your screen, so be prepared to use your
    "undo" key.

- **-log**,    **--logfile**           

    Save the `.LOG` file, which has many useful diagnostics.  Perltidy always
    creates a `.LOG` file, but by default it is deleted unless a program bug is
    suspected.  Setting the **-log** flag forces the log file to be saved.

- **-g=n**, **--logfile-gap=n**

    Set maximum interval between input code lines in the logfile.  This purpose of
    this flag is to assist in debugging nesting errors.  The value of `n` is
    optional.  If you set the flag **-g** without the value of `n`, it will be
    taken to be 1, meaning that every line will be written to the log file.  This
    can be helpful if you are looking for a brace, paren, or bracket nesting error. 

    Setting **-g** also causes the logfile to be saved, so it is not necessary to
    also include **-log**. 

    If no **-g** flag is given, a value of 50 will be used, meaning that at least
    every 50th line will be recorded in the logfile.  This helps prevent
    excessively long log files.  

    Setting a negative value of `n` is the same as not setting **-g** at all.

- **-npro**  **--noprofile**    

    Ignore any `.perltidyrc` command file.  Normally, perltidy looks first in
    your current directory for a `.perltidyrc` file of parameters.  (The format
    is described below).  If it finds one, it applies those options to the
    initial default values, and then it applies any that have been defined
    on the command line.  If no `.perltidyrc` file is found, it looks for one
    in your home directory.

    If you set the **-npro** flag, perltidy will not look for this file.

- **-pro=filename** or  **--profile=filename**    

    To simplify testing and switching .perltidyrc files, this command may be
    used to specify a configuration file which will override the default
    name of .perltidyrc.  There must not be a space on either side of the
    '=' sign.  For example, the line

        perltidy -pro=testcfg

    would cause file `testcfg` to be used instead of the 
    default `.perltidyrc`.

    A pathname begins with three dots, e.g. ".../.perltidyrc", indicates that
    the file should be searched for starting in the current directory and
    working upwards. This makes it easier to have multiple projects each with
    their own .perltidyrc in their root directories.

- **-opt**,   **--show-options**      

    Write a list of all options used to the `.LOG` file.  
    Please see **--dump-options** for a simpler way to do this.

- **-f**,   **--force-read-binary**      

    Force perltidy to process binary files.  To avoid producing excessive
    error messages, perltidy skips files identified by the system as non-text.
    However, valid perl scripts containing binary data may sometimes be identified
    as non-text, and this flag forces perltidy to process them.

# FORMATTING OPTIONS

## Basic Options

- **--notidy**

    This flag disables all formatting and causes the input to be copied unchanged
    to the output except for possible changes in line ending characters and any
    pre- and post-filters.  This can be useful in conjunction with a hierarchical
    set of `.perltidyrc` files to avoid unwanted code tidying.  See also
    ["Skipping Selected Sections of Code"](#skipping-selected-sections-of-code) for a way to avoid tidying specific
    sections of code.

- **-i=n**,  **--indent-columns=n**  

    Use n columns per indentation level (default n=4).

- **-l=n**, **--maximum-line-length=n**

    The default maximum line length is n=80 characters.  Perltidy will try
    to find line break points to keep lines below this length. However, long
    quotes and side comments may cause lines to exceed this length. 
    Setting **-l=0** is equivalent to setting **-l=(a large number)**. 

- **-vmll**, **--variable-maximum-line-length**

    A problem arises using a fixed maximum line length with very deeply nested code
    and data structures because eventually the amount of leading whitespace used
    for indicating indentation takes up most or all of the available line width,
    leaving little or no space for the actual code or data.  One solution is to use
    a vary long line length.  Another solution is to use the **-vmll** flag, which
    basically tells perltidy to ignore leading whitespace when measuring the line
    length.  

    To be precise, when the **-vmll** parameter is set, the maximum line length of a
    line of code will be M+L\*I, where

          M is the value of --maximum-line-length=M (-l=M), default 80,
          I is the value of --indent-columns=I (-i=I), default 4,
          L is the indentation level of the line of code

    When this flag is set, the choice of breakpoints for a block of code should be
    essentially independent of its nesting depth.  However, the absolute line
    lengths, including leading whitespace, can still be arbitrarily large.  This
    problem can be avoided by including the next parameter.  

    The default is not to do this (**-nvmll**).

- **-wc=n**, **--whitespace-cycle=n**

    This flag also addresses problems with very deeply nested code and data
    structures.  When the nesting depth exceeds the value **n** the leading
    whitespace will be reduced and start at a depth of 1 again.  The result is that
    blocks of code will shift back to the left rather than moving arbitrarily far
    to the right.  This occurs cyclically to any depth.  

    For example if one level of indentation equals 4 spaces (**-i=4**, the default),
    and one uses **-wc=15**, then if the leading whitespace on a line exceeds about
    4\*15=60 spaces it will be reduced back to 4\*1=4 spaces and continue increasing
    from there.  If the whitespace never exceeds this limit the formatting remains
    unchanged.

    The combination of **-vmll** and **-wc=n** provides a solution to the problem of
    displaying arbitrarily deep data structures and code in a finite window,
    although **-wc=n** may of course be used without **-vmll**.

    The default is not to use this, which can also be indicated using **-wc=0**.

- tabs

    Using tab characters will almost certainly lead to future portability
    and maintenance problems, so the default and recommendation is not to
    use them.  For those who prefer tabs, however, there are two different
    options.  

    Except for possibly introducing tab indentation characters, as outlined
    below, perltidy does not introduce any tab characters into your file,
    and it removes any tabs from the code (unless requested not to do so
    with **-fws**).  If you have any tabs in your comments, quotes, or
    here-documents, they will remain.

    - **-et=n**,   **--entab-leading-whitespace**

        This flag causes each **n** initial space characters to be replaced by
        one tab character.  Note that the integer **n** is completely independent
        of the integer specified for indentation parameter, **-i=n**.

    - **-t**,   **--tabs**

        This flag causes one leading tab character to be inserted for each level
        of indentation.  Certain other features are incompatible with this
        option, and if these options are also given, then a warning message will
        be issued and this flag will be unset.  One example is the **-lp**
        option.

    - **-dt=n**,   **--default-tabsize=n**

        If the first line of code passed to perltidy contains leading tabs but no
        tab scheme is specified for the output stream then perltidy must guess how many
        spaces correspond to each leading tab.  This number of spaces **n**
        corresponding to each leading tab of the input stream may be specified with
        **-dt=n**.  The default is **n=8**.  

        This flag has no effect if a tab scheme is specified for the output stream,
        because then the input stream is assumed to use the same tab scheme and
        indentation spaces as for the output stream (any other assumption would lead to
        unstable editing).

- **-syn**,   **--check-syntax**      

    This flag is now ignored for safety, but the following documentation
    has been retained for reference.

    This flag causes perltidy to run `perl -c -T` to check syntax of input
    and output.  (To change the flags passed to perl, see the next
    item, **-pscf**).  The results are written to the `.LOG` file, which
    will be saved if an error is detected in the output script.  The output
    script is not checked if the input script has a syntax error.  Perltidy
    does its own checking, but this option employs perl to get a "second
    opinion".

    If perl reports errors in the input file, they will not be reported in
    the error output unless the **--warning-output** flag is given. 

    The default is **NOT** to do this type of syntax checking (although
    perltidy will still do as much self-checking as possible).  The reason
    is that it causes all code in BEGIN blocks to be executed, for all
    modules being used, and this opens the door to security issues and
    infinite loops when running perltidy.

- **-pscf=s**, **-perl-syntax-check-flags=s**

    When perl is invoked to check syntax, the normal flags are `-c -T`.  In
    addition, if the **-x** flag is given to perltidy, then perl will also be
    passed a **-x** flag.  It should not normally be necessary to change
    these flags, but it can be done with the **-pscf=s** flag.  For example,
    if the taint flag, `-T`, is not wanted, the flag could be set to be just
    **-pscf=-c**.  

    Perltidy will pass your string to perl with the exception that it will
    add a **-c** and **-x** if appropriate.  The `.LOG` file will show
    exactly what flags were passed to perl.

- **-xs**,   **--extended-syntax**      

    A problem with formatting Perl code is that some modules can introduce new
    syntax.  This flag allows perltidy to handle certain common extensions
    to the standard syntax without complaint.  

    For example, without this flag a structure such as the following would generate
    a syntax error and the braces would not be balanced:

        method deposit( Num $amount) {
            $self->balance( $self->balance + $amount );
        }

    This flag is enabled by default but it can be deactivated with **-nxs**.
    Probably the only reason to deactivate this flag is to generate more diagnostic
    messages when debugging a script.

- **-io**,   **--indent-only**       

    This flag is used to deactivate all whitespace and line break changes
    within non-blank lines of code.
    When it is in effect, the only change to the script will be
    to the indentation and to the number of blank lines.
    And any flags controlling whitespace and newlines will be ignored.  You
    might want to use this if you are perfectly happy with your whitespace
    and line breaks, and merely want perltidy to handle the indentation.
    (This also speeds up perltidy by well over a factor of two, so it might be
    useful when perltidy is merely being used to help find a brace error in
    a large script).

    Setting this flag is equivalent to setting **--freeze-newlines** and
    **--freeze-whitespace**.  

    If you also want to keep your existing blank lines exactly
    as they are, you can add **--freeze-blank-lines**. 

    With this option perltidy is still free to modify the indenting (and
    outdenting) of code and comments as it normally would.  If you also want to
    prevent long comment lines from being outdented, you can add either **-noll** or
    **-l=0**.

    Setting this flag will prevent perltidy from doing any special operations on
    closing side comments.  You may still delete all side comments however when
    this flag is in effect.

- **-enc=s**,  **--character-encoding=s**

    where **s**=**none** or **utf8**.  This flag tells perltidy the character encoding
    of both the input and output character streams.  The value **utf8** causes the
    stream to be read and written as UTF-8.  The value **none** causes the stream to
    be processed without special encoding assumptions.  At present there is no
    automatic detection of character encoding (even if there is a `'use utf8'`
    statement in your code) so this flag must be set for streams encoded in UTF-8.
    Incorrectly setting this parameter can cause data corruption, so please
    carefully check the output.

    The default is **none**.  

    The abbreviations **-utf8** or **-UTF8** are equivalent to **-enc=utf8**.
    So to process a file named **file.pl** which is encoded in UTF-8 you can use:

        perltidy -utf8 file.pl

- **-ole=s**,  **--output-line-ending=s**

    where s=`win`, `dos`, `unix`, or `mac`.  This flag tells perltidy
    to output line endings for a specific system.  Normally,
    perltidy writes files with the line separator character of the host
    system.  The `win` and `dos` flags have an identical result.

- **-ple**,  **--preserve-line-endings**

    This flag tells perltidy to write its output files with the same line
    endings as the input file, if possible.  It should work for
    **dos**, **unix**, and **mac** line endings.  It will only work if perltidy
    input comes from a filename (rather than stdin, for example).  If
    perltidy has trouble determining the input file line ending, it will
    revert to the default behavior of using the line ending of the host system.

- **-it=n**,   **--iterations=n**

    This flag causes perltidy to do **n** complete iterations.  The reason for this
    flag is that code beautification is an iterative process and in some
    cases the output from perltidy can be different if it is applied a second time.
    For most purposes the default of **n=1** should be satisfactory.  However **n=2**
    can be useful when a major style change is being made, or when code is being
    beautified on check-in to a source code control system.  It has been found to
    be extremely rare for the output to change after 2 iterations.  If a value
    **n** is greater than 2 is input then a convergence test will be used to stop
    the iterations as soon as possible, almost always after 2 iterations.  See
    the next item for a simplified iteration control.

    This flag has no effect when perltidy is used to generate html.

- **-conv**,   **--converge**

    This flag is equivalent to **-it=4** and is included to simplify iteration
    control.  For all practical purposes one either does or does not want to be
    sure that the output is converged, and there is no penalty to using a large
    iteration limit since perltidy will check for convergence and stop iterating as
    soon as possible.  The default is **-nconv** (no convergence check).  Using
    **-conv** will approximately double run time since normally one extra iteration
    is required to verify convergence.

## Code Indentation Control

- **-ci=n**, **--continuation-indentation=n**

    Continuation indentation is extra indentation spaces applied when
    a long line is broken.  The default is n=2, illustrated here:

        my $level =   # -ci=2      
          ( $max_index_to_go >= 0 ) ? $levels_to_go[0] : $last_output_level;

    The same example, with n=0, is a little harder to read:

        my $level =   # -ci=0    
        ( $max_index_to_go >= 0 ) ? $levels_to_go[0] : $last_output_level;

    The value given to **-ci** is also used by some commands when a small
    space is required.  Examples are commands for outdenting labels,
    **-ola**, and control keywords, **-okw**.  

    When default values are not used, it is suggested that the value **n**
    given with **-ci=n** be no more than about one-half of the number of
    spaces assigned to a full indentation level on the **-i=n** command.

- **-sil=n** **--starting-indentation-level=n**   

    By default, perltidy examines the input file and tries to determine the
    starting indentation level.  While it is often zero, it may not be
    zero for a code snippet being sent from an editing session.  

    To guess the starting indentation level perltidy simply assumes that
    indentation scheme used to create the code snippet is the same as is being used
    for the current perltidy process.  This is the only sensible guess that can be
    made.  It should be correct if this is true, but otherwise it probably won't.
    For example, if the input script was written with -i=2 and the current peltidy
    flags have -i=4, the wrong initial indentation will be guessed for a code
    snippet which has non-zero initial indentation. Likewise, if an entabbing
    scheme is used in the input script and not in the current process then the
    guessed indentation will be wrong.

    If the default method does not work correctly, or you want to change the
    starting level, use **-sil=n**, to force the starting level to be n.

- List indentation using **-lp**, **--line-up-parentheses**

    By default, perltidy indents lists with 4 spaces, or whatever value
    is specified with **-i=n**.  Here is a small list formatted in this way:

        # perltidy (default)
        @month_of_year = (
            'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
        );

    Use the **-lp** flag to add extra indentation to cause the data to begin
    past the opening parentheses of a sub call or list, or opening square
    bracket of an anonymous array, or opening curly brace of an anonymous
    hash.  With this option, the above list would become:

        # perltidy -lp
        @month_of_year = (
                           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
        );

    If the available line length (see **-l=n** ) does not permit this much 
    space, perltidy will use less.   For alternate placement of the
    closing paren, see the next section.

    This option has no effect on code BLOCKS, such as if/then/else blocks,
    which always use whatever is specified with **-i=n**.  Also, the
    existence of line breaks and/or block comments between the opening and
    closing parens may cause perltidy to temporarily revert to its default
    method.

    Note: The **-lp** option may not be used together with the **-t** tabs option.
    It may, however, be used with the **-et=n** tab method.

    In addition, any parameter which significantly restricts the ability of
    perltidy to choose newlines will conflict with **-lp** and will cause
    **-lp** to be deactivated.  These include **-io**, **-fnl**, **-nanl**, and
    **-ndnl**.  The reason is that the **-lp** indentation style can require
    the careful coordination of an arbitrary number of break points in
    hierarchical lists, and these flags may prevent that.

- **-cti=n**, **--closing-token-indentation**

    The **-cti=n** flag controls the indentation of a line beginning with 
    a `)`, `]`, or a non-block `}`.  Such a line receives:

        -cti = 0 no extra indentation (default)
        -cti = 1 extra indentation such that the closing token
               aligns with its opening token.
        -cti = 2 one extra indentation level if the line looks like:
               );  or  ];  or  };
        -cti = 3 one extra indentation level always

    The flags **-cti=1** and **-cti=2** work well with the **-lp** flag (previous
    section).

        # perltidy -lp -cti=1
        @month_of_year = (
                           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
                         );

        # perltidy -lp -cti=2
        @month_of_year = (
                           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
                           );

    These flags are merely hints to the formatter and they may not always be
    followed.  In particular, if -lp is not being used, the indentation for
    **cti=1** is constrained to be no more than one indentation level.

    If desired, this control can be applied independently to each of the
    closing container token types.  In fact, **-cti=n** is merely an
    abbreviation for **-cpi=n -csbi=n -cbi=n**, where:  
    **-cpi** or **--closing-paren-indentation** controls **)**'s,
    **-csbi** or **--closing-square-bracket-indentation** controls **\]**'s, 
    **-cbi** or **--closing-brace-indentation** controls non-block **}**'s. 

- **-icp**, **--indent-closing-paren**

    The **-icp** flag is equivalent to
    **-cti=2**, described in the previous section.  The **-nicp** flag is
    equivalent **-cti=0**.  They are included for backwards compatibility.

- **-icb**, **--indent-closing-brace**

    The **-icb** option gives one extra level of indentation to a brace which
    terminates a code block .  For example,

            if ($task) {
                yyy();
                }    # -icb
            else {
                zzz();
                }

    The default is not to do this, indicated by **-nicb**.

- **-olq**, **--outdent-long-quotes**

    When **-olq** is set, lines which is a quoted string longer than the
    value **maximum-line-length** will have their indentation removed to make
    them more readable.  This is the default.  To prevent such out-denting,
    use **-nolq** or **--nooutdent-long-lines**.

- **-oll**, **--outdent-long-lines**

    This command is equivalent to **--outdent-long-quotes** and
    **--outdent-long-comments**, and it is included for compatibility with previous
    versions of perltidy.  The negation of this also works, **-noll** or
    **--nooutdent-long-lines**, and is equivalent to setting **-nolq** and **-nolc**.

- Outdenting Labels: **-ola**,  **--outdent-labels**

    This command will cause labels to be outdented by 2 spaces (or whatever **-ci**
    has been set to), if possible.  This is the default.  For example:

            my $i;
          LOOP: while ( $i = <FOTOS> ) {
                chomp($i);
                next unless $i;
                fixit($i);
            }

    Use **-nola** to not outdent labels. 

- Outdenting Keywords
    - **-okw**,  **--outdent-keywords**

        The command **-okw** will cause certain leading control keywords to
        be outdented by 2 spaces (or whatever **-ci** has been set to), if
        possible.  By default, these keywords are `redo`, `next`, `last`,
        `goto`, and `return`.  The intention is to make these control keywords
        easier to see.  To change this list of keywords being outdented, see
        the next section.

        For example, using `perltidy -okw` on the previous example gives:

                my $i;
              LOOP: while ( $i = <FOTOS> ) {
                    chomp($i);
                  next unless $i;
                    fixit($i);
                }

        The default is not to do this.  

    - Specifying Outdented Keywords: **-okwl=string**,  **--outdent-keyword-list=string**

        This command can be used to change the keywords which are outdented with
        the **-okw** command.  The parameter **string** is a required list of perl
        keywords, which should be placed in quotes if there are more than one.
        By itself, it does not cause any outdenting to occur, so the **-okw**
        command is still required.

        For example, the commands `-okwl="next last redo goto" -okw` will cause
        those four keywords to be outdented.  It is probably simplest to place
        any **-okwl** command in a `.perltidyrc` file.

## Whitespace Control

Whitespace refers to the blank space between variables, operators,
and other code tokens.

- **-fws**,  **--freeze-whitespace**

    This flag causes your original whitespace to remain unchanged, and
    causes the rest of the whitespace commands in this section, the
    Code Indentation section, and
    the Comment Control section to be ignored.

- Tightness of curly braces, parentheses, and square brackets.

    Here the term "tightness" will mean the closeness with which
    pairs of enclosing tokens, such as parentheses, contain the quantities
    within.  A numerical value of 0, 1, or 2 defines the tightness, with
    0 being least tight and 2 being most tight.  Spaces within containers
    are always symmetric, so if there is a space after a `(` then there
    will be a space before the corresponding `)`.

    The **-pt=n** or **--paren-tightness=n** parameter controls the space within
    parens.  The example below shows the effect of the three possible
    values, 0, 1, and 2:

        if ( ( my $len_tab = length( $tabstr ) ) > 0 ) {  # -pt=0
        if ( ( my $len_tab = length($tabstr) ) > 0 ) {    # -pt=1 (default)
        if ((my $len_tab = length($tabstr)) > 0) {        # -pt=2

    When n is 0, there is always a space to the right of a '(' and to the left
    of a ')'.  For n=2 there is never a space.  For n=1, the default, there
    is a space unless the quantity within the parens is a single token, such
    as an identifier or quoted string.  

    Likewise, the parameter **-sbt=n** or **--square-bracket-tightness=n**
    controls the space within square brackets, as illustrated below.

        $width = $col[ $j + $k ] - $col[ $j ];  # -sbt=0
        $width = $col[ $j + $k ] - $col[$j];    # -sbt=1 (default)
        $width = $col[$j + $k] - $col[$j];      # -sbt=2 

    Curly braces which do not contain code blocks are controlled by
    the parameter **-bt=n** or **--brace-tightness=n**. 

        $obj->{ $parsed_sql->{ 'table' }[0] };    # -bt=0
        $obj->{ $parsed_sql->{'table'}[0] };      # -bt=1 (default)
        $obj->{$parsed_sql->{'table'}[0]};        # -bt=2

    And finally, curly braces which contain blocks of code are controlled by the
    parameter **-bbt=n** or **--block-brace-tightness=n** as illustrated in the
    example below.   

        %bf = map { $_ => -M $_ } grep { /\.deb$/ } dirents '.'; # -bbt=0 (default)
        %bf = map { $_ => -M $_ } grep {/\.deb$/} dirents '.';   # -bbt=1
        %bf = map {$_ => -M $_} grep {/\.deb$/} dirents '.';     # -bbt=2

    To simplify input in the case that all of the tightness flags have the same
    value &lt;n>, the parameter <-act=n> or **--all-containers-tightness=n** is an
    abbreviation for the combination <-pt=n -sbt=n -bt=n -bbt=n>.

- **-tso**,   **--tight-secret-operators**

    The flag **-tso** causes certain perl token sequences (secret operators)
    which might be considered to be a single operator to be formatted "tightly"
    (without spaces).  The operators currently modified by this flag are: 

         0+  +0  ()x!! ~~<>  ,=>   =( )=  

    For example the sequence **0 +**,  which converts a string to a number,
    would be formatted without a space: **0+** when the **-tso** flag is set.  This
    flag is off by default.

- **-sts**,   **--space-terminal-semicolon**

    Some programmers prefer a space before all terminal semicolons.  The
    default is for no such space, and is indicated with **-nsts** or
    **--nospace-terminal-semicolon**.

            $i = 1 ;     #  -sts
            $i = 1;      #  -nsts   (default)

- **-sfs**,   **--space-for-semicolon**

    Semicolons within **for** loops may sometimes be hard to see,
    particularly when commas are also present.  This option places spaces on
    both sides of these special semicolons, and is the default.  Use
    **-nsfs** or **--nospace-for-semicolon** to deactivate it.

        for ( @a = @$ap, $u = shift @a ; @a ; $u = $v ) {  # -sfs (default)
        for ( @a = @$ap, $u = shift @a; @a; $u = $v ) {    # -nsfs

- **-asc**,  **--add-semicolons**

    Setting **-asc** allows perltidy to add any missing optional semicolon at the end 
    of a line which is followed by a closing curly brace on the next line.  This
    is the default, and may be deactivated with **-nasc** or **--noadd-semicolons**.

- **-dsm**,  **--delete-semicolons**

    Setting **-dsm** allows perltidy to delete extra semicolons which are
    simply empty statements.  This is the default, and may be deactivated
    with **-ndsm** or **--nodelete-semicolons**.  (Such semicolons are not
    deleted, however, if they would promote a side comment to a block
    comment).

- **-aws**,  **--add-whitespace**

    Setting this option allows perltidy to add certain whitespace improve
    code readability.  This is the default. If you do not want any
    whitespace added, but are willing to have some whitespace deleted, use
    **-naws**.  (Use **-fws** to leave whitespace completely unchanged).

- **-dws**,  **--delete-old-whitespace**

    Setting this option allows perltidy to remove some old whitespace
    between characters, if necessary.  This is the default.  If you
    do not want any old whitespace removed, use **-ndws** or
    **--nodelete-old-whitespace**.

- Detailed whitespace controls around tokens

    For those who want more detailed control over the whitespace around
    tokens, there are four parameters which can directly modify the default
    whitespace rules built into perltidy for any token.  They are:

    **-wls=s** or **--want-left-space=s**,

    **-nwls=s** or **--nowant-left-space=s**,

    **-wrs=s** or **--want-right-space=s**,

    **-nwrs=s** or **--nowant-right-space=s**.

    These parameters are each followed by a quoted string, **s**, containing a
    list of token types.  No more than one of each of these parameters
    should be specified, because repeating a command-line parameter
    always overwrites the previous one before perltidy ever sees it.

    To illustrate how these are used, suppose it is desired that there be no
    space on either side of the token types **= + - / \***.  The following two
    parameters would specify this desire:

        -nwls="= + - / *"    -nwrs="= + - / *"

    (Note that the token types are in quotes, and that they are separated by
    spaces).  With these modified whitespace rules, the following line of math:

        $root = -$b + sqrt( $b * $b - 4. * $a * $c ) / ( 2. * $a );

    becomes this:

        $root=-$b+sqrt( $b*$b-4.*$a*$c )/( 2.*$a );

    These parameters should be considered to be hints to perltidy rather
    than fixed rules, because perltidy must try to resolve conflicts that
    arise between them and all of the other rules that it uses.  One
    conflict that can arise is if, between two tokens, the left token wants
    a space and the right one doesn't.  In this case, the token not wanting
    a space takes priority.  

    It is necessary to have a list of all token types in order to create
    this type of input.  Such a list can be obtained by the command
    **--dump-token-types**.  Also try the **-D** flag on a short snippet of code
    and look at the .DEBUG file to see the tokenization. 

    **WARNING** Be sure to put these tokens in quotes to avoid having them
    misinterpreted by your command shell.

- Space between specific keywords and opening paren

    When an opening paren follows a Perl keyword, no space is introduced after the
    keyword, unless it is (by default) one of these:

        my local our and or eq ne if else elsif until unless 
        while for foreach return switch case given when

    These defaults can be modified with two commands:

    **-sak=s**  or **--space-after-keyword=s**  adds keywords.

    **-nsak=s**  or **--nospace-after-keyword=s**  removes keywords.

    where **s** is a list of keywords (in quotes if necessary).  For example, 

        my ( $a, $b, $c ) = @_;    # default
        my( $a, $b, $c ) = @_;     # -nsak="my local our"

    The abbreviation **-nsak='\*'** is equivalent to including all of the
    keywords in the above list.

    When both **-nsak=s** and **-sak=s** commands are included, the **-nsak=s**
    command is executed first.  For example, to have space after only the
    keywords (my, local, our) you could use **-nsak="\*" -sak="my local our"**.

    To put a space after all keywords, see the next item.

- Space between all keywords and opening parens

    When an opening paren follows a function or keyword, no space is introduced
    after the keyword except for the keywords noted in the previous item.  To
    always put a space between a function or keyword and its opening paren,
    use the command:

    **-skp**  or **--space-keyword-paren**

    You will probably also want to use the flag **-sfp** (next item) too.

- Space between all function names and opening parens

    When an opening paren follows a function the default is not to introduce
    a space.  To cause a space to be introduced use:

    **-sfp**  or **--space-function-paren**

        myfunc( $a, $b, $c );    # default 
        myfunc ( $a, $b, $c );   # -sfp

    You will probably also want to use the flag **-skp** (previous item) too.

- Trimming whitespace around `qw` quotes

    **-tqw** or **--trim-qw** provide the default behavior of trimming
    spaces around multi-line `qw` quotes and indenting them appropriately.

    **-ntqw** or **--notrim-qw** cause leading and trailing whitespace around
    multi-line `qw` quotes to be left unchanged.  This option will not
    normally be necessary, but was added for testing purposes, because in
    some versions of perl, trimming `qw` quotes changes the syntax tree.

- **-sbq=n**  or **--space-backslash-quote=n**

    Lines like

           $str1=\"string1";
           $str2=\'string2';

    can confuse syntax highlighters unless a space is included between the backslash and the single or double quotation mark.

    This can be controlled with the value of **n** as follows:

        -sbq=0 means no space between the backslash and quote
        -sbq=1 means follow the example of the source code
        -sbq=2 means always put a space between the backslash and quote

    The default is **-sbq=1**, meaning that a space will be used 0if there is one in the source code.

- Trimming trailing whitespace from lines of POD

    **-trp** or **--trim-pod** will remove trailing whitespace from lines of POD.
    The default is not to do this.

## Comment Controls

Perltidy has a number of ways to control the appearance of both block comments
and side comments.  The term **block comment** here refers to a full-line
comment, whereas **side comment** will refer to a comment which appears on a
line to the right of some code.

- **-ibc**,  **--indent-block-comments**

    Block comments normally look best when they are indented to the same
    level as the code which follows them.  This is the default behavior, but
    you may use **-nibc** to keep block comments left-justified.  Here is an
    example:

                 # this comment is indented      (-ibc, default)
                 if ($task) { yyy(); }

    The alternative is **-nibc**:

        # this comment is not indented              (-nibc)
                    if ($task) { yyy(); }

    See also the next item, **-isbc**, as well as **-sbc**, for other ways to
    have some indented and some outdented block comments.

- **-isbc**,  **--indent-spaced-block-comments**

    If there is no leading space on the line, then the comment will not be
    indented, and otherwise it may be.

    If both **-ibc** and **-isbc** are set, then **-isbc** takes priority.

- **-olc**, **--outdent-long-comments**

    When **-olc** is set, lines which are full-line (block) comments longer
    than the value **maximum-line-length** will have their indentation
    removed.  This is the default; use **-nolc** to prevent outdenting.

- **-msc=n**,  **--minimum-space-to-comment=n**

    Side comments look best when lined up several spaces to the right of
    code.  Perltidy will try to keep comments at least n spaces to the
    right.  The default is n=4 spaces.

- **-fpsc=n**,  **--fixed-position-side-comment=n**

    This parameter tells perltidy to line up side comments in column number **n**
    whenever possible.  The default, n=0, will not do this.

- **-iscl**,  **--ignore-side-comment-lengths**

    This parameter causes perltidy to ignore the length of side comments when
    setting line breaks.  The default, **-niscl**, is to include the length of 
    side comments when breaking lines to stay within the length prescribed
    by the **-l=n** maximum line length parameter.  For example, the following
    long single line would remain intact with -l=80 and -iscl:

         perltidy -l=80 -iscl
            $vmsfile =~ s/;[\d\-]*$//; # Clip off version number; we can use a newer version as well

    whereas without the -iscl flag the line will be broken:

          perltidy -l=80
             $vmsfile =~ s/;[\d\-]*$//
               ;    # Clip off version number; we can use a newer version as well
        

- **-hsc**, **--hanging-side-comments**

    By default, perltidy tries to identify and align "hanging side
    comments", which are something like this:

            my $IGNORE = 0;    # This is a side comment
                               # This is a hanging side comment
                               # And so is this

    A comment is considered to be a hanging side comment if (1) it immediately
    follows a line with a side comment, or another hanging side comment, and
    (2) there is some leading whitespace on the line.
    To deactivate this feature, use **-nhsc** or **--nohanging-side-comments**.  
    If block comments are preceded by a blank line, or have no leading
    whitespace, they will not be mistaken as hanging side comments.

- Closing Side Comments

    A closing side comment is a special comment which perltidy can
    automatically create and place after the closing brace of a code block.
    They can be useful for code maintenance and debugging.  The command
    **-csc** (or **--closing-side-comments**) adds or updates closing side
    comments.  For example, here is a small code snippet

            sub message {
                if ( !defined( $_[0] ) ) {
                    print("Hello, World\n");
                }
                else {
                    print( $_[0], "\n" );
                }
            }

    And here is the result of processing with `perltidy -csc`:

            sub message {
                if ( !defined( $_[0] ) ) {
                    print("Hello, World\n");
                }
                else {
                    print( $_[0], "\n" );
                }
            } ## end sub message

    A closing side comment was added for `sub message` in this case, but not
    for the `if` and `else` blocks, because they were below the 6 line
    cutoff limit for adding closing side comments.  This limit may be
    changed with the **-csci** command, described below.

    The command **-dcsc** (or **--delete-closing-side-comments**) reverses this 
    process and removes these comments.

    Several commands are available to modify the behavior of these two basic
    commands, **-csc** and **-dcsc**:

    - **-csci=n**, or **--closing-side-comment-interval=n** 

        where `n` is the minimum number of lines that a block must have in
        order for a closing side comment to be added.  The default value is
        `n=6`.  To illustrate:

                # perltidy -csci=2 -csc
                sub message {
                    if ( !defined( $_[0] ) ) {
                        print("Hello, World\n");
                    } ## end if ( !defined( $_[0] ))
                    else {
                        print( $_[0], "\n" );
                    } ## end else [ if ( !defined( $_[0] ))
                } ## end sub message

        Now the `if` and `else` blocks are commented.  However, now this has
        become very cluttered.

    - **-cscp=string**, or **--closing-side-comment-prefix=string** 

        where string is the prefix used before the name of the block type.  The
        default prefix, shown above, is `## end`.  This string will be added to
        closing side comments, and it will also be used to recognize them in
        order to update, delete, and format them.  Any comment identified as a
        closing side comment will be placed just a single space to the right of
        its closing brace.

    - **-cscl=string**, or **--closing-side-comment-list** 

        where `string` is a list of block types to be tagged with closing side
        comments.  By default, all code block types preceded by a keyword or
        label (such as `if`, `sub`, and so on) will be tagged.  The **-cscl**
        command changes the default list to be any selected block types; see
        ["Specifying Block Types"](#specifying-block-types).
        For example, the following command
        requests that only `sub`'s, labels, `BEGIN`, and `END` blocks be
        affected by any **-csc** or **-dcsc** operation:

            -cscl="sub : BEGIN END"

    - **-csct=n**, or **--closing-side-comment-maximum-text=n** 

        The text appended to certain block types, such as an `if` block, is
        whatever lies between the keyword introducing the block, such as `if`,
        and the opening brace.  Since this might be too much text for a side
        comment, there needs to be a limit, and that is the purpose of this
        parameter.  The default value is `n=20`, meaning that no additional
        tokens will be appended to this text after its length reaches 20
        characters.  Omitted text is indicated with `...`.  (Tokens, including
        sub names, are never truncated, however, so actual lengths may exceed
        this).  To illustrate, in the above example, the appended text of the
        first block is ` ( !defined( $_[0] )...`.  The existing limit of
        `n=20` caused this text to be truncated, as indicated by the `...`.  See
        the next flag for additional control of the abbreviated text.

    - **-cscb**, or **--closing-side-comments-balanced** 

        As discussed in the previous item, when the
        closing-side-comment-maximum-text limit is exceeded the comment text must
        be truncated.  Older versions of perltidy terminated with three dots, and this
        can still be achieved with -ncscb:

            perltidy -csc -ncscb
            } ## end foreach my $foo (sort { $b cmp $a ...

        However this causes a problem with editors which cannot recognize
        comments or are not configured to do so because they cannot "bounce" around in
        the text correctly.  The **-cscb** flag has been added to
        help them by appending appropriate balancing structure:

            perltidy -csc -cscb
            } ## end foreach my $foo (sort { $b cmp $a ... })

        The default is **-cscb**.

    - **-csce=n**, or **--closing-side-comment-else-flag=n** 

        The default, **n=0**, places the text of the opening `if` statement after any
        terminal `else`.

        If **n=2** is used, then each `elsif` is also given the text of the opening
        `if` statement.  Also, an `else` will include the text of a preceding
        `elsif` statement.  Note that this may result some long closing
        side comments.

        If **n=1** is used, the results will be the same as **n=2** whenever the
        resulting line length is less than the maximum allowed.

    - **-cscb**, or **--closing-side-comments-balanced** 

        When using closing-side-comments, and the closing-side-comment-maximum-text
        limit is exceeded, then the comment text must be abbreviated.  
        It is terminated with three dots if the **-cscb** flag is negated:

            perltidy -csc -ncscb
            } ## end foreach my $foo (sort { $b cmp $a ...

        This causes a problem with older editors which do not recognize comments
        because they cannot "bounce" around in the text correctly.  The **-cscb**
        flag tries to help them by appending appropriate terminal balancing structures:

            perltidy -csc -cscb
            } ## end foreach my $foo (sort { $b cmp $a ... })

        The default is **-cscb**.  

    - **-cscw**, or **--closing-side-comment-warnings** 

        This parameter is intended to help make the initial transition to the use of
        closing side comments.  
        It causes two
        things to happen if a closing side comment replaces an existing, different
        closing side comment:  first, an error message will be issued, and second, the
        original side comment will be placed alone on a new specially marked comment
        line for later attention. 

        The intent is to avoid clobbering existing hand-written side comments
        which happen to match the pattern of closing side comments. This flag
        should only be needed on the first run with **-csc**.

    **Important Notes on Closing Side Comments:** 

    - Closing side comments are only placed on lines terminated with a closing
    brace.  Certain closing styles, such as the use of cuddled elses
    (**-ce**), preclude the generation of some closing side comments.
    - Please note that adding or deleting of closing side comments takes
    place only through the commands **-csc** or **-dcsc**.  The other commands,
    if used, merely modify the behavior of these two commands.  
    - It is recommended that the **-cscw** flag be used along with **-csc** on
    the first use of perltidy on a given file.  This will prevent loss of
    any existing side comment data which happens to have the csc prefix.
    - Once you use **-csc**, you should continue to use it so that any
    closing side comments remain correct as code changes.  Otherwise, these
    comments will become incorrect as the code is updated.
    - If you edit the closing side comments generated by perltidy, you must also
    change the prefix to be different from the closing side comment prefix.
    Otherwise, your edits will be lost when you rerun perltidy with **-csc**.   For
    example, you could simply change `## end` to be `## End`, since the test is
    case sensitive.  You may also want to use the **-ssc** flag to keep these
    modified closing side comments spaced the same as actual closing side comments.
    - Temporarily generating closing side comments is a useful technique for
    exploring and/or debugging a perl script, especially one written by someone
    else.  You can always remove them with **-dcsc**.

- Static Block Comments

    Static block comments are block comments with a special leading pattern,
    `##` by default, which will be treated slightly differently from other
    block comments.  They effectively behave as if they had glue along their
    left and top edges, because they stick to the left edge and previous line
    when there is no blank spaces in those places.  This option is
    particularly useful for controlling how commented code is displayed.

    - **-sbc**, **--static-block-comments**

        When **-sbc** is used, a block comment with a special leading pattern, `##` by
        default, will be treated specially. 

        Comments so identified  are treated as follows: 

        - If there is no leading space on the line, then the comment will not
        be indented, and otherwise it may be,
        - no new blank line will be
        inserted before such a comment, and 
        - such a comment will never become
        a hanging side comment.  

        For example, assuming `@month_of_year` is
        left-adjusted:

            @month_of_year = (    # -sbc (default)
                'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
            ##  'Dec', 'Nov'
                'Nov', 'Dec');

        Without this convention, the above code would become

              @month_of_year = (   # -nsbc
                  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
            
                  ##  'Dec', 'Nov'
                  'Nov', 'Dec'
              );

        which is not as clear.
        The default is to use **-sbc**.  This may be deactivated with **-nsbc**.

    - **-sbcp=string**, **--static-block-comment-prefix=string**

        This parameter defines the prefix used to identify static block comments
        when the **-sbc** parameter is set.  The default prefix is `##`,
        corresponding to `-sbcp=##`.  The prefix is actually part of a perl 
        pattern used to match lines and it must either begin with `#` or `^#`.  
        In the first case a prefix ^\\s\* will be added to match any leading
        whitespace, while in the second case the pattern will match only
        comments with no leading whitespace.  For example, to
        identify all comments as static block comments, one would use `-sbcp=#`.
        To identify all left-adjusted comments as static block comments, use `-sbcp='^#'`.

        Please note that **-sbcp** merely defines the pattern used to identify static
        block comments; it will not be used unless the switch **-sbc** is set.  Also,
        please be aware that since this string is used in a perl regular expression
        which identifies these comments, it must enable a valid regular expression to
        be formed.

        A pattern which can be useful is:

            -sbcp=^#{2,}[^\s#] 

        This pattern requires a static block comment to have at least one character
        which is neither a # nor a space.  It allows a line containing only '#'
        characters to be rejected as a static block comment.  Such lines are often used
        at the start and end of header information in subroutines and should not be
        separated from the intervening comments, which typically begin with just a
        single '#'.

    - **-osbc**, **--outdent-static-block-comments**

        The command **-osbc** will cause static block comments to be outdented by 2
        spaces (or whatever **-ci=n** has been set to), if possible.

- Static Side Comments

    Static side comments are side comments with a special leading pattern.
    This option can be useful for controlling how commented code is displayed
    when it is a side comment.

    - **-ssc**, **--static-side-comments**

        When **-ssc** is used, a side comment with a static leading pattern, which is
        `##` by default, will be spaced only a single space from previous
        character, and it will not be vertically aligned with other side comments.

        The default is **-nssc**.

    - **-sscp=string**, **--static-side-comment-prefix=string**

        This parameter defines the prefix used to identify static side comments
        when the **-ssc** parameter is set.  The default prefix is `##`,
        corresponding to `-sscp=##`.  

        Please note that **-sscp** merely defines the pattern used to identify
        static side comments; it will not be used unless the switch **-ssc** is
        set.  Also, note that this string is used in a perl regular expression
        which identifies these comments, so it must enable a valid regular
        expression to be formed.

## Skipping Selected Sections of Code

Selected lines of code may be passed verbatim to the output without any
formatting.  This feature is enabled by default but can be disabled with
the **--noformat-skipping** or **-nfs** flag.  It should be used sparingly to
avoid littering code with markers, but it might be helpful for working
around occasional problems.  For example it might be useful for keeping
the indentation of old commented code unchanged, keeping indentation of
long blocks of aligned comments unchanged, keeping certain list
formatting unchanged, or working around a glitch in perltidy.

- **-fs**,  **--format-skipping**

    This flag, which is enabled by default, causes any code between
    special beginning and ending comment markers to be passed to the
    output without formatting.  The default beginning marker is #<<<
    and the default ending marker is #>>> but they
    may be changed (see next items below).  Additional text may appear on
    these special comment lines provided that it is separated from the
    marker by at least one space.  For example

        #<<<  do not let perltidy touch this
           my @list = (1,
                       1, 1,
                       1, 2, 1,
                       1, 3, 3, 1,
                       1, 4, 6, 4, 1,);
        #>>>

    The comment markers may be placed at any location that a block comment may
    appear.  If they do not appear to be working, use the -log flag and examine the
    `.LOG` file.  Use **-nfs** to disable this feature.

- **-fsb=string**,  **--format-skipping-begin=string**

    The **-fsb=string** parameter may be used to change the beginning marker for
    format skipping.  The default is equivalent to -fsb='#<<<'.  The string that
    you enter must begin with a # and should be in quotes as necessary to get past
    the command shell of your system.  It is actually the leading text of a pattern
    that is constructed by appending a '\\s', so you must also include backslashes
    for characters to be taken literally rather than as patterns.  

    Some examples show how example strings become patterns:

        -fsb='#\{\{\{' becomes /^#\{\{\{\s/  which matches  #{{{ but not #{{{{
        -fsb='#\*\*'   becomes /^#\*\*\s/    which matches  #** but not #***
        -fsb='#\*{2,}' becomes /^#\*{2,}\s/  which matches  #** and #***** 

- **-fse=string**,  **--format-skipping-end=string**

    The **-fsb=string** is the corresponding parameter used to change the
    ending marker for format skipping.  The default is equivalent to
    \-fse='#<<<'.  

## Line Break Control

The parameters in this section control breaks after
non-blank lines of code.  Blank lines are controlled
separately by parameters in the section ["Blank Line
Control"](#blank-line-control).

- **-fnl**,  **--freeze-newlines**

    If you do not want any changes to the line breaks within
    lines of code in your script, set
    **-fnl**, and they will remain fixed, and the rest of the commands in
    this section and sections 
    ["Controlling List Formatting"](#controlling-list-formatting),
    ["Retaining or Ignoring Existing Line Breaks"](#retaining-or-ignoring-existing-line-breaks). 
    You may want to use **-noll** with this.

    Note: If you also want to keep your blank lines exactly
    as they are, you can use the **-fbl** flag which is described
    in the section ["Blank Line Control"](#blank-line-control).

- **-ce**,   **--cuddled-else**

    Enable the "cuddled else" style, in which `else` and `elsif` are
    follow immediately after the curly brace closing the previous block.
    The default is not to use cuddled elses, and is indicated with the flag
    **-nce** or **--nocuddled-else**.  Here is a comparison of the
    alternatives:

        # -ce
        if ($task) {
            yyy();
        } else {    
            zzz();
        }

        # -nce (default)
        if ($task) {
              yyy();
        }
        else {    
              zzz();
        }

    In this example the keyword **else** is placed on the same line which begins with
    the preceding closing block brace and is followed by its own opening block brace
    on the same line.  Other keywords and function names which are formatted with
    this "cuddled" style are **elsif**, **continue**, **catch**, **finally**.

    Other block types can be formatted by specifying their names on a 
    separate parameter **-cbl**, described in a later section.  

    Cuddling between a pair of code blocks requires that the closing brace of the
    first block start a new line.  If this block is entirely on one line in the
    input file, it is necessary to decide if it should be broken to allow cuddling.
    This decision is controlled by the flag **-cbo=n** discussed below.  The default
    and recommended value of **-cbo=1** bases this decision on the first block in
    the chain.  If it spans multiple lines then cuddling is made and continues
    along the chain, regardless of the sizes of subsequent blocks. Otherwise, short
    lines remain intact.

    So for example, the **-ce** flag would not have any effect if the above snippet
    is rewritten as

        if ($task) { yyy() }
        else {    zzz() }

    If the first block spans multiple lines, then cuddling can be done and will
    continue for the subsequent blocks in the chain, as illustrated in the previous
    snippet.

    If there are blank lines between cuddled blocks they will be eliminated.  If
    there are comments after the closing brace where cuddling would occur then
    cuddling will be prevented.  If this occurs, cuddling will restart later in the
    chain if possible.  

- **-cb**,   **--cuddled-blocks**

    This flag is equivalent to **-ce**. 

- **-cbl**,    **--cuddled-block-list**     

    The built-in default cuddled block types are **else, elsif, continue, catch, finally**.

    Additional block types to which the **-cuddled-blocks** style applies can be defined by
    this parameter.  This parameter is a character string, giving a list of
    block types separated by commas or spaces.  For example, to cuddle code blocks
    of type sort, map and grep, in addition to the default types, the string could
    be set to

        -cbl="sort map grep"

    or equivalently

        -cbl=sort,map,grep 

    Note however that these particular block types are typically short so there might not be much
    opportunity for the cuddled format style.

    Using commas avoids the need to protect spaces with quotes.

    As a diagnostic check, the flag **--dump-cuddled-block-list** or **-dcbl** can be
    used to view the hash of values that are generated by this flag. 

    Finally, note that the **-cbl** flag by itself merely specifies which blocks are formatted
    with the cuddled format. It has no effect unless this formatting style is activated with
    **-ce**.

- **-cblx**,    **--cuddled-block-list-exclusive**     

    When cuddled else formatting is selected with **-ce**, setting this flag causes
    perltidy to ignore its built-in defaults and rely exclusively on the block types
    specified on the **-cbl** flag described in the previous section.  For example,
    to avoid using cuddled **catch** and **finally**, which among in the defaults, the
    following set of parameters could be used:

        perltidy -ce -cbl='else elsif continue' -cblx

- **-cbo=n**,   **--cuddled-break-option=n**

    Cuddled formatting is only possible between a pair of code blocks if the
    closing brace of the first block starts a new line. If a block is encountered
    which is entirely on a single line, and cuddled formatting is selected, it is
    necessary to make a decision as to whether or not to "break" the block, meaning
    to cause it to span multiple lines.  This parameter controls that decision. The
    options are:

        cbo=0  Never force a short block to break.
        cbo=1  If the first of a pair of blocks is broken in the input file, 
               then break the second [DEFAULT].
        cbo=2  Break open all blocks for maximal cuddled formatting.

    The default and recommended value is **cbo=1**.  With this value, if the starting
    block of a chain spans multiple lines, then a cascade of breaks will occur for
    remaining blocks causing the entire chain to be cuddled.

    The option **cbo=0** can produce erratic cuddling if there are numerous one-line
    blocks.

    The option **cbo=2** produces maximal cuddling but will not allow any short blocks.

- **-bl**,    **--opening-brace-on-new-line**     

    Use the flag **-bl** to place the opening brace on a new line:

        if ( $input_file eq '-' )    # -bl 
        {                          
            important_function();
        }

    This flag applies to all structural blocks, including named sub's (unless
    the **-sbl** flag is set -- see next item).

    The default style, **-nbl**, places an opening brace on the same line as
    the keyword introducing it.  For example,

        if ( $input_file eq '-' ) {   # -nbl (default)

- **-sbl**,    **--opening-sub-brace-on-new-line**     

    The flag **-sbl** can be used to override the value of **-bl** for
    the opening braces of named sub's.  For example, 

        perltidy -sbl

    produces this result:

        sub message
        {
           if (!defined($_[0])) {
               print("Hello, World\n");
           }
           else {
               print($_[0], "\n");
           }
        }

    This flag is negated with **-nsbl**.  If **-sbl** is not specified,
    the value of **-bl** is used.

- **-asbl**,    **--opening-anonymous-sub-brace-on-new-line**     

    The flag **-asbl** is like the **-sbl** flag except that it applies
    to anonymous sub's instead of named subs. For example

        perltidy -asbl

    produces this result:

        $a = sub
        {
            if ( !defined( $_[0] ) ) {
                print("Hello, World\n");
            }
            else {
                print( $_[0], "\n" );
            }
        };

    This flag is negated with **-nasbl**, and the default is **-nasbl**.

- **-bli**,    **--brace-left-and-indent**     

    The flag **-bli** is the same as **-bl** but in addition it causes one 
    unit of continuation indentation ( see **-ci** ) to be placed before 
    an opening and closing block braces.

    For example,

            if ( $input_file eq '-' )    # -bli
              {
                important_function();
              }

    By default, this extra indentation occurs for blocks of type:
    **if**, **elsif**, **else**, **unless**, **for**, **foreach**, **sub**, 
    **while**, **until**, and also with a preceding label.  The next item
    shows how to change this.

- **-blil=s**,    **--brace-left-and-indent-list=s**     

    Use this parameter to change the types of block braces for which the
    **-bli** flag applies; see ["Specifying Block Types"](#specifying-block-types).  For example,
    **-blil='if elsif else'** would apply it to only `if/elsif/else` blocks.

- **-bar**,    **--opening-brace-always-on-right**     

    The default style, **-nbl** places the opening code block brace on a new
    line if it does not fit on the same line as the opening keyword, like
    this:

            if ( $bigwasteofspace1 && $bigwasteofspace2
              || $bigwasteofspace3 && $bigwasteofspace4 )
            {
                big_waste_of_time();
            }

    To force the opening brace to always be on the right, use the **-bar**
    flag.  In this case, the above example becomes

            if ( $bigwasteofspace1 && $bigwasteofspace2
              || $bigwasteofspace3 && $bigwasteofspace4 ) {
                big_waste_of_time();
            }

    A conflict occurs if both **-bl** and **-bar** are specified.

- **-otr**,  **--opening-token-right** and related flags

    The **-otr** flag is a hint that perltidy should not place a break between a
    comma and an opening token.  For example:

        # default formatting
        push @{ $self->{$module}{$key} },
          {
            accno       => $ref->{accno},
            description => $ref->{description}
          };

        # perltidy -otr
        push @{ $self->{$module}{$key} }, {
            accno       => $ref->{accno},
            description => $ref->{description}
          };

    The flag **-otr** is actually an abbreviation for three other flags
    which can be used to control parens, hash braces, and square brackets
    separately if desired:

        -opr  or --opening-paren-right
        -ohbr or --opening-hash-brace-right
        -osbr or --opening-square-bracket-right

- **-wn**,  **--weld-nested-containers** 

    The **-wn** flag causes closely nested pairs of opening and closing container
    symbols (curly braces, brackets, or parens) to be "welded" together, meaning
    that they are treated as if combined into a single unit, with the indentation
    of the innermost code reduced to be as if there were just a single container
    symbol.

    For example:

            # default formatting
            do {
                {
                    next if $x == $y;    
                }
            } until $x++ > $z;

            # perltidy -wn
            do { {
                next if $x == $y;
            } } until $x++ > $z;

    When this flag is set perltidy makes a preliminary pass through the file and
    identifies all nested pairs of containers.  To qualify as a nested pair, the
    closing container symbols must be immediately adjacent. The opening symbols
    must either be adjacent, or, if the outer opening symbol is an opening
    paren, they may be separated by any single non-container symbol or something
    that looks like a function evaluation.  

    Any container symbol may serve as both the inner container of one pair and as
    the outer container of an adjacent pair. Consequently, any number of adjacent
    opening or closing symbols may join together in weld.  For example, here are
    three levels of wrapped function calls:

            # default formatting
            my (@date_time) = Localtime(
                Date_to_Time(
                    Add_Delta_DHMS(
                        $year, $month,  $day, $hour, $minute, $second,
                        '0',   $offset, '0',  '0'
                    )
                )
            );

            # perltidy -wn
            my (@date_time) = Localtime( Date_to_Time( Add_Delta_DHMS(
                $year, $month,  $day, $hour, $minute, $second,
                '0',   $offset, '0',  '0'
            ) ) );

    Notice how the indentation of the inner lines are reduced by two levels in this
    case.  This example also shows the typical result of this formatting, namely it
    is a sandwich consisting of an initial opening layer, a central section of any
    complexity forming the "meat" of the sandwich, and a final closing layer.  This
    predictable structure helps keep the compacted structure readable.

    The inner sandwich layer is required to be at least one line thick.  If this
    cannot be achieved, welding does not occur.  This constraint can cause
    formatting to take a couple of iterations to stabilize when it is first applied
    to a script. The **-conv** flag can be used to insure that the final format is
    achieved in a single run.

    Here is an example illustrating a welded container within a welded containers:

            # default formatting
            $x->badd(
                bmul(
                    $class->new(
                        abs(
                            $sx * int( $xr->numify() ) & $sy * int( $yr->numify() )
                        )
                    ),
                    $m
                )
            );

            # perltidy -wn
            $x->badd( bmul(
                $class->new( abs(
                    $sx * int( $xr->numify() ) & $sy * int( $yr->numify() )
                ) ),
                $m
            ) );

    This format option is quite general but there are some limitations.  

    One limitiation is that any line length limit still applies and can cause long
    welded sections to be broken into multiple lines.  

    Another limitation is that an opening symbol which delimits quoted text cannot
    be included in a welded pair.  This is because quote delimiters are treated
    specially in perltidy.  

    Finally, the stacking of containers defined by this flag have priority over
    any other container stacking flags.  This is because any welding is done first.

- **Vertical tightness** of non-block curly braces, parentheses, and square brackets.

    These parameters control what shall be called vertical tightness.  Here are the
    main points:

    - Opening tokens (except for block braces) are controlled by **-vt=n**, or
    **--vertical-tightness=n**, where

            -vt=0 always break a line after opening token (default). 
            -vt=1 do not break unless this would produce more than one 
                    step in indentation in a line.
            -vt=2 never break a line after opening token

    - You must also use the **-lp** flag when you use the **-vt** flag; the
    reason is explained below.
    - Closing tokens (except for block braces) are controlled by **-vtc=n**, or
    **--vertical-tightness-closing=n**, where

            -vtc=0 always break a line before a closing token (default), 
            -vtc=1 do not break before a closing token which is followed 
                   by a semicolon or another closing token, and is not in 
                   a list environment.
            -vtc=2 never break before a closing token.

        The rules for **-vtc=1** are designed to maintain a reasonable balance
        between tightness and readability in complex lists.

    - Different controls may be applied to different token types,
    and it is also possible to control block braces; see below.
    - Finally, please note that these vertical tightness flags are merely
    hints to the formatter, and it cannot always follow them.  Things which
    make it difficult or impossible include comments, blank lines, blocks of
    code within a list, and possibly the lack of the **-lp** parameter.
    Also, these flags may be ignored for very small lists (2 or 3 lines in
    length).

    Here are some examples: 

        # perltidy -lp -vt=0 -vtc=0
        %romanNumerals = (
                           one   => 'I',
                           two   => 'II',
                           three => 'III',
                           four  => 'IV',
        );

        # perltidy -lp -vt=1 -vtc=0
        %romanNumerals = ( one   => 'I',
                           two   => 'II',
                           three => 'III',
                           four  => 'IV',
        );

        # perltidy -lp -vt=1 -vtc=1
        %romanNumerals = ( one   => 'I',
                           two   => 'II',
                           three => 'III',
                           four  => 'IV', );

    The difference between **-vt=1** and **-vt=2** is shown here:

        # perltidy -lp -vt=1 
        $init->add(
                    mysprintf( "(void)find_threadsv(%s);",
                               cstring( $threadsv_names[ $op->targ ] )
                    )
        );

        # perltidy -lp -vt=2 
        $init->add( mysprintf( "(void)find_threadsv(%s);",
                               cstring( $threadsv_names[ $op->targ ] )
                    )
        );

    With **-vt=1**, the line ending in `add(` does not combine with the next
    line because the next line is not balanced.  This can help with
    readability, but **-vt=2** can be used to ignore this rule.

    The tightest, and least readable, code is produced with both `-vt=2` and
    `-vtc=2`:

        # perltidy -lp -vt=2 -vtc=2
        $init->add( mysprintf( "(void)find_threadsv(%s);",
                               cstring( $threadsv_names[ $op->targ ] ) ) );

    Notice how the code in all of these examples collapses vertically as
    **-vt** increases, but the indentation remains unchanged.  This is
    because perltidy implements the **-vt** parameter by first formatting as
    if **-vt=0**, and then simply overwriting one output line on top of the
    next, if possible, to achieve the desired vertical tightness.  The
    **-lp** indentation style has been designed to allow this vertical
    collapse to occur, which is why it is required for the **-vt** parameter.

    The **-vt=n** and **-vtc=n** parameters apply to each type of container
    token.  If desired, vertical tightness controls can be applied
    independently to each of the closing container token types.

    The parameters for controlling parentheses are **-pvt=n** or
    **--paren-vertical-tightness=n**, and **-pcvt=n** or
    **--paren-vertical-tightness-closing=n**.

    Likewise, the parameters for square brackets are **-sbvt=n** or
    **--square-bracket-vertical-tightness=n**, and **-sbcvt=n** or
    **--square-bracket-vertical-tightness-closing=n**.

    Finally, the parameters for controlling non-code block braces are
    **-bvt=n** or **--brace-vertical-tightness=n**, and **-bcvt=n** or
    **--brace-vertical-tightness-closing=n**.

    In fact, the parameter **-vt=n** is actually just an abbreviation for
    **-pvt=n -bvt=n sbvt=n**, and likewise **-vtc=n** is an abbreviation
    for **-pvtc=n -bvtc=n sbvtc=n**.

- **-bbvt=n** or **--block-brace-vertical-tightness=n**

    The **-bbvt=n** flag is just like the **-vt=n** flag but applies
    to opening code block braces.

        -bbvt=0 break after opening block brace (default). 
        -bbvt=1 do not break unless this would produce more than one 
                step in indentation in a line.
        -bbvt=2 do not break after opening block brace.

    It is necessary to also use either **-bl** or **-bli** for this to work,
    because, as with other vertical tightness controls, it is implemented by
    simply overwriting a line ending with an opening block brace with the
    subsequent line.  For example:

        # perltidy -bli -bbvt=0
        if ( open( FILE, "< $File" ) )
          {
            while ( $File = <FILE> )
              {
                $In .= $File;
                $count++;
              }
            close(FILE);
          }

        # perltidy -bli -bbvt=1
        if ( open( FILE, "< $File" ) )
          { while ( $File = <FILE> )
              { $In .= $File;
                $count++;
              }
            close(FILE);
          }

    By default this applies to blocks associated with keywords **if**,
    **elsif**, **else**, **unless**, **for**, **foreach**, **sub**, **while**,
    **until**, and also with a preceding label.  This can be changed with
    the parameter **-bbvtl=string**, or
    **--block-brace-vertical-tightness-list=string**, where **string** is a
    space-separated list of block types.  For more information on the
    possible values of this string, see ["Specifying Block Types"](#specifying-block-types)

    For example, if we want to just apply this style to `if`,
    `elsif`, and `else` blocks, we could use 
    `perltidy -bli -bbvt=1 -bbvtl='if elsif else'`.

    There is no vertical tightness control for closing block braces; with
    one exception they will be placed on separate lines.
    The exception is that a cascade of closing block braces may
    be stacked on a single line.  See **-scbb**.

- **-sot**,  **--stack-opening-tokens** and related flags

    The **-sot** flag tells perltidy to "stack" opening tokens
    when possible to avoid lines with isolated opening tokens.

    For example:

        # default
        $opt_c = Text::CSV_XS->new(
            {
                binary       => 1,
                sep_char     => $opt_c,
                always_quote => 1,
            }
        );

        # -sot
        $opt_c = Text::CSV_XS->new( {
                binary       => 1,
                sep_char     => $opt_c,
                always_quote => 1,
            }
        );

    For detailed control of individual closing tokens the following
    controls can be used:

        -sop  or --stack-opening-paren
        -sohb or --stack-opening-hash-brace
        -sosb or --stack-opening-square-bracket
        -sobb or --stack-opening-block-brace

    The flag **-sot** is an abbreviation for **-sop -sohb -sosb**.

    The flag **-sobb** is a abbreviation for **-bbvt=2 -bbvtl='\*'**.  This
    will case a cascade of opening block braces to appear on a single line,
    although this an uncommon occurrence except in test scripts. 

- **-sct**,  **--stack-closing-tokens** and related flags

    The **-sct** flag tells perltidy to "stack" closing tokens
    when possible to avoid lines with isolated closing tokens.

    For example:

        # default
        $opt_c = Text::CSV_XS->new(
            {
                binary       => 1,
                sep_char     => $opt_c,
                always_quote => 1,
            }
        );

        # -sct
        $opt_c = Text::CSV_XS->new(
            {
                binary       => 1,
                sep_char     => $opt_c,
                always_quote => 1,
            } );

    The **-sct** flag is somewhat similar to the **-vtc** flags, and in some
    cases it can give a similar result.  The difference is that the **-vtc**
    flags try to avoid lines with leading opening tokens by "hiding" them at
    the end of a previous line, whereas the **-sct** flag merely tries to
    reduce the number of lines with isolated closing tokens by stacking them
    but does not try to hide them.  For example:

        # -vtc=2
        $opt_c = Text::CSV_XS->new(
            {
                binary       => 1,
                sep_char     => $opt_c,
                always_quote => 1, } );

    For detailed control of the stacking of individual closing tokens the
    following controls can be used:

        -scp  or --stack-closing-paren
        -schb or --stack-closing-hash-brace
        -scsb or --stack-closing-square-bracket
        -scbb or --stack-closing-block-brace

    The flag **-sct** is an abbreviation for stacking the non-block closing
    tokens, **-scp -schb -scsb**. 

    Stacking of closing block braces, **-scbb**, causes a cascade of isolated
    closing block braces to be combined into a single line as in the following
    example:

        # -scbb:
        for $w1 (@w1) {
            for $w2 (@w2) {
                for $w3 (@w3) {
                    for $w4 (@w4) {
                        push( @lines, "$w1 $w2 $w3 $w4\n" );
                    } } } }

    To simplify input even further for the case in which both opening and closing
    non-block containers are stacked, the flag **-sac** or **--stack-all-containers**
    is an abbreviation for **-sot -sot**.

- **-dnl**,  **--delete-old-newlines**

    By default, perltidy first deletes all old line break locations, and then it
    looks for good break points to match the desired line length.  Use **-ndnl**
    or  **--nodelete-old-newlines** to force perltidy to retain all old line break
    points.  

- **-anl**,  **--add-newlines**

    By default, perltidy will add line breaks when necessary to create
    continuations of long lines and to improve the script appearance.  Use
    **-nanl** or **--noadd-newlines** to prevent any new line breaks.  

    This flag does not prevent perltidy from eliminating existing line
    breaks; see **--freeze-newlines** to completely prevent changes to line
    break points.

- Controlling whether perltidy breaks before or after operators

    Four command line parameters provide some control over whether
    a line break should be before or after specific token types.
    Two parameters give detailed control:

    **-wba=s** or **--want-break-after=s**, and

    **-wbb=s** or **--want-break-before=s**.

    These parameters are each followed by a quoted string, **s**, containing
    a list of token types (separated only by spaces).  No more than one of each
    of these parameters should be specified, because repeating a
    command-line parameter always overwrites the previous one before
    perltidy ever sees it.

    By default, perltidy breaks **after** these token types:
      % + - \* / x != == >= <= =~ !~ < >  | & 
      = \*\*= += \*= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=

    And perltidy breaks **before** these token types by default:
      . << >> -> && || //

    To illustrate, to cause a break after a concatenation operator, `'.'`,
    rather than before it, the command line would be

        -wba="."

    As another example, the following command would cause a break before 
    math operators `'+'`, `'-'`, `'/'`, and `'*'`:

        -wbb="+ - / *"

    These commands should work well for most of the token types that perltidy uses
    (use **--dump-token-types** for a list).  Also try the **-D** flag on a short
    snippet of code and look at the .DEBUG file to see the tokenization.  However,
    for a few token types there may be conflicts with hardwired logic which cause
    unexpected results.  One example is curly braces, which should be controlled
    with the parameter **bl** provided for that purpose.

    **WARNING** Be sure to put these tokens in quotes to avoid having them
    misinterpreted by your command shell.

    Two additional parameters are available which, though they provide no further
    capability, can simplify input are:

    **-baao** or **--break-after-all-operators**,

    **-bbao** or **--break-before-all-operators**.

    The -baao sets the default to be to break after all of the following operators:

        % + - * / x != == >= <= =~ !~ < > | & 
        = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
        . : ? && || and or err xor

    and the **-bbao** flag sets the default to break before all of these operators.
    These can be used to define an initial break preference which can be fine-tuned
    with the **-wba** and **-wbb** flags.  For example, to break before all operators
    except an **=** one could use --bbao -wba='=' rather than listing every
    single perl operator except **=** on a -wbb flag.

## Controlling List Formatting

Perltidy attempts to place comma-separated arrays of values in tables
which look good.  Its default algorithms usually work well, and they
have been improving with each release, but several parameters are
available to control list formatting.

- **-boc**,  **--break-at-old-comma-breakpoints**

    This flag tells perltidy to try to break at all old commas.  This is not
    the default.  Normally, perltidy makes a best guess at list formatting,
    and seldom uses old comma breakpoints.  Usually this works well,
    but consider:

        my @list = (1,
                    1, 1,
                    1, 2, 1,
                    1, 3, 3, 1,
                    1, 4, 6, 4, 1,);

    The default formatting will flatten this down to one line:

        # perltidy (default)
        my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

    which hides the structure. Using **-boc**, plus additional flags
    to retain the original style, yields

        # perltidy -boc -lp -pt=2 -vt=1 -vtc=1
        my @list = (1,
                    1, 1,
                    1, 2, 1,
                    1, 3, 3, 1,
                    1, 4, 6, 4, 1,);

    A disadvantage of this flag is that all tables in the file
    must already be nicely formatted.  For another possibility see
    the -fs flag in ["Skipping Selected Sections of Code"](#skipping-selected-sections-of-code).

- **-mft=n**,  **--maximum-fields-per-table=n**

    If the computed number of fields for any table exceeds **n**, then it
    will be reduced to **n**.  The default value for **n** is a large number,
    40\.  While this value should probably be left unchanged as a general
    rule, it might be used on a small section of code to force a list to
    have a particular number of fields per line, and then either the **-boc**
    flag could be used to retain this formatting, or a single comment could
    be introduced somewhere to freeze the formatting in future applications
    of perltidy.

        # perltidy -mft=2
        @month_of_year = (    
            'Jan', 'Feb',
            'Mar', 'Apr',
            'May', 'Jun',
            'Jul', 'Aug',
            'Sep', 'Oct',
            'Nov', 'Dec'
        );

- **-cab=n**,  **--comma-arrow-breakpoints=n**

    A comma which follows a comma arrow, '=>', is given special
    consideration.  In a long list, it is common to break at all such
    commas.  This parameter can be used to control how perltidy breaks at
    these commas.  (However, it will have no effect if old comma breaks are
    being forced because **-boc** is used).  The possible values of **n** are:

        n=0 break at all commas after =>  
        n=1 stable: break at all commas after => if container is open,
            EXCEPT FOR one-line containers
        n=2 break at all commas after =>, BUT try to form the maximum
            maximum one-line container lengths
        n=3 do not treat commas after => specially at all 
        n=4 break everything: like n=0 but ALSO break a short container with
            a => not followed by a comma when -vt=0 is used
        n=5 stable: like n=1 but ALSO break at open one-line containers when
            -vt=0 is used (default)

    For example, given the following single line, perltidy by default will
    not add any line breaks because it would break the existing one-line
    container:

        bless { B => $B, Root => $Root } => $package;

    Using **-cab=0** will force a break after each comma-arrow item:

        # perltidy -cab=0:
        bless {
            B    => $B,
            Root => $Root
        } => $package;

    If perltidy is subsequently run with this container broken, then by
    default it will break after each '=>' because the container is now
    broken.  To reform a one-line container, the parameter **-cab=2** could
    be used.

    The flag **-cab=3** can be used to prevent these commas from being
    treated specially.  In this case, an item such as "01" => 31 is
    treated as a single item in a table.  The number of fields in this table
    will be determined by the same rules that are used for any other table.
    Here is an example.

        # perltidy -cab=3
        my %last_day = (
            "01" => 31, "02" => 29, "03" => 31, "04" => 30,
            "05" => 31, "06" => 30, "07" => 31, "08" => 31,
            "09" => 30, "10" => 31, "11" => 30, "12" => 31
        );

## Retaining or Ignoring Existing Line Breaks

Several additional parameters are available for controlling the extent
to which line breaks in the input script influence the output script.
In most cases, the default parameter values are set so that, if a choice
is possible, the output style follows the input style.  For example, if
a short logical container is broken in the input script, then the
default behavior is for it to remain broken in the output script.

Most of the parameters in this section would only be required for a
one-time conversion of a script from short container lengths to longer
container lengths.  The opposite effect, of converting long container
lengths to shorter lengths, can be obtained by temporarily using a short
maximum line length.

- **-bol**,  **--break-at-old-logical-breakpoints**

    By default, if a logical expression is broken at a `&&`, `||`, `and`,
    or `or`, then the container will remain broken.  Also, breaks
    at internal keywords `if` and `unless` will normally be retained.
    To prevent this, and thus form longer lines, use **-nbol**.

- **-bok**,  **--break-at-old-keyword-breakpoints**

    By default, perltidy will retain a breakpoint before keywords which may
    return lists, such as `sort` and &lt;map>.  This allows chains of these
    operators to be displayed one per line.  Use **-nbok** to prevent
    retaining these breakpoints.

- **-bot**,  **--break-at-old-ternary-breakpoints**

    By default, if a conditional (ternary) operator is broken at a `:`,
    then it will remain broken.  To prevent this, and thereby
    form longer lines, use **-nbot**.

- **-boa**,  **--break-at-old-attribute-breakpoints**

    By default, if an attribute list is broken at a `:` in the source file, then
    it will remain broken.  For example, given the following code, the line breaks
    at the ':'s will be retained:

                        my @field
                          : field
                          : Default(1)
                          : Get('Name' => 'foo') : Set('Name');

    If the attributes are on a single line in the source code then they will remain
    on a single line if possible.

    To prevent this, and thereby always form longer lines, use **-nboa**.  

- **-iob**,  **--ignore-old-breakpoints**

    Use this flag to tell perltidy to ignore existing line breaks to the
    maximum extent possible.  This will tend to produce the longest possible
    containers, regardless of type, which do not exceed the line length
    limit.

- **-kis**,  **--keep-interior-semicolons**

    Use the **-kis** flag to prevent breaking at a semicolon if
    there was no break there in the input file.  Normally
    perltidy places a newline after each semicolon which
    terminates a statement unless several statements are
    contained within a one-line brace block.  To illustrate,
    consider the following input lines:

        dbmclose(%verb_delim); undef %verb_delim;
        dbmclose(%expanded); undef %expanded;

    The default is to break after each statement, giving

        dbmclose(%verb_delim);
        undef %verb_delim;
        dbmclose(%expanded);
        undef %expanded;

    With **perltidy -kis** the multiple statements are retained:

        dbmclose(%verb_delim); undef %verb_delim;
        dbmclose(%expanded);   undef %expanded;

    The statements are still subject to the specified value
    of **maximum-line-length** and will be broken if this 
    maximum is exceeded.

## Blank Line Control

Blank lines can improve the readability of a script if they are carefully
placed.  Perltidy has several commands for controlling the insertion,
retention, and removal of blank lines.  

- **-fbl**,  **--freeze-blank-lines**

    Set **-fbl** if you want to the blank lines in your script to
    remain exactly as they are.  The rest of the parameters in
    this section may then be ignored.  (Note: setting the **-fbl** flag
    is equivalent to setting **-mbl=0** and **-kbl=2**).

- **-bbc**,  **--blanks-before-comments**

    A blank line will be introduced before a full-line comment.  This is the
    default.  Use **-nbbc** or  **--noblanks-before-comments** to prevent
    such blank lines from being introduced.

- **-blbs=n**,  **--blank-lines-before-subs=n**

    The parameter **-blbs=n** requests that least **n** blank lines precede a sub
    definition which does not follow a comment and which is more than one-line
    long.  The default is <-blbs=1>.  **BEGIN** and **END** blocks are included.

    The requested number of blanks statement will be inserted regardless of the
    value of **--maximum-consecutive-blank-lines=n** (**-mbl=n**) with the exception
    that if **-mbl=0** then no blanks will be output.

    This parameter interacts with the value **k** of the parameter **--maximum-consecutive-blank-lines=k** (**-mbl=k**) as follows:

    1\. If **-mbl=0** then no blanks will be output.  This allows all blanks to be suppressed with a single parameter.  Otherwise,

    2\. If the number of old blank lines in the script is less than **n** then
    additional blanks will be inserted to make the total **n** regardless of the
    value of **-mbl=k**.  

    3\. If the number of old blank lines in the script equals or exceeds **n** then
    this parameter has no effect, however the total will not exceed
    value specified on the **-mbl=k** flag.

- **-blbp=n**,  **--blank-lines-before-packages=n**

    The parameter **-blbp=n** requests that least **n** blank lines precede a package
    which does not follow a comment.  The default is **-blbp=1**.  

    This parameter interacts with the value **k** of the parameter
    **--maximum-consecutive-blank-lines=k** (**-mbl=k**) in the same way as described
    for the previous item **-blbs=n**.

- **-bbs**,  **--blanks-before-subs**

    For compatibility with previous versions, **-bbs** or **--blanks-before-subs**
    is equivalent to `-blbp=1` and `-blbs=1`.  

    Likewise, **-nbbs** or **--noblanks-before-subs** 
    is equivalent to `-blbp=0` and `-blbs=0`.  

- **-bbb**,  **--blanks-before-blocks**

    A blank line will be introduced before blocks of coding delimited by
    **for**, **foreach**, **while**, **until**, and **if**, **unless**, in the following
    circumstances:

    - The block is not preceded by a comment.
    - The block is not a one-line block.
    - The number of consecutive non-blank lines at the current indentation depth is at least **-lbl**
    (see next section).

    This is the default.  The intention of this option is to introduce
    some space within dense coding.
    This is negated with **-nbbb** or  **--noblanks-before-blocks**.

- **-lbl=n** **--long-block-line-count=n**

    This controls how often perltidy is allowed to add blank lines before 
    certain block types (see previous section).  The default is 8.  Entering
    a value of **0** is equivalent to entering a very large number.

- **-blao=i** or **--blank-lines-after-opening-block=i**

    This control places a minimum of **i** blank lines **after** a line which **ends**
    with an opening block brace of a specified type.  By default, this only applies
    to the block of a named **sub**, but this can be changed (see **-blaol** below).
    The default is not to do this (**i=0**).

    Please see the note below on using the **-blao** and **-blbc** options.

- **-blbc=i** or **--blank-lines-before-closing-block=i**

    This control places a minimum of **i** blank lines **before** a line which
    **begins** with a closing block brace of a specified type.  By default, this
    only applies to the block of a named **sub**, but this can be changed (see
    **-blbcl** below).  The default is not to do this (**i=0**).

- **-blaol=s** or **--blank-lines-after-opening-block-list=s**

    The parameter **s** is a list of block type keywords to which the flag **-blao**
    should apply.  The section ["Specifying Block Types"](#specifying-block-types) explains how to list
    block types.

- **-blbcl=s** or **--blank-lines-before-closing-block-list=s**

    This parameter is a list of block type keywords to which the flag **-blbc**
    should apply.  The section ["Specifying Block Types"](#specifying-block-types) explains how to list
    block types.

- Note on using the **-blao** and **-blbc** options.

    These blank line controls introduce a certain minimum number of blank lines in
    the text, but the final number of blank lines may be greater, depending on
    values of the other blank line controls and the number of old blank lines.  A
    consequence is that introducing blank lines with these and other controls
    cannot be exactly undone, so some experimentation with these controls is
    recommended before using them.

    For example, suppose that for some reason we decide to introduce one blank
    space at the beginning and ending of all blocks.  We could do
    this using

        perltidy -blao=2 -blbc=2 -blaol='*' -blbcl='*' filename

    Now suppose the script continues to be developed, but at some later date we
    decide we don't want these spaces after all. we might expect that running with
    the flags **-blao=0** and **-blbc=0** will undo them.  However, by default
    perltidy retains single blank lines, so the blank lines remain.  

    We can easily fix this by telling perltidy to ignore old blank lines by
    including the added parameter **-kbl=0** and rerunning. Then the unwanted blank
    lines will be gone.  However, this will cause all old blank lines to be
    ignored, perhaps even some that were added by hand to improve formatting. So
    please be cautious when using these parameters.

- **-mbl=n** **--maximum-consecutive-blank-lines=n**   

    This parameter specifies the maximum number of consecutive blank lines which
    will be output within code sections of a script.  The default is n=1.  If the
    input file has more than n consecutive blank lines, the number will be reduced
    to n except as noted above for the **-blbp** and **-blbs** parameters.  If **n=0**
    then no blank lines will be output (unless all old blank lines are retained
    with the **-kbl=2** flag of the next section).

    This flag obviously does not apply to pod sections,
    here-documents, and quotes.  

- **-kbl=n**,  **--keep-old-blank-lines=n**

    The **-kbl=n** flag gives you control over how your existing blank lines are
    treated.  

    The possible values of **n** are:

        n=0 ignore all old blank lines
        n=1 stable: keep old blanks, but limited by the value of the B<-mbl=n> flag
        n=2 keep all old blank lines, regardless of the value of the B<-mbl=n> flag

    The default is **n=1**.  

- **-sob**,  **--swallow-optional-blank-lines**

    This is equivalent to **kbl=0** and is included for compatibility with
    previous versions.

- **-nsob**,  **--noswallow-optional-blank-lines**

    This is equivalent to **kbl=1** and is included for compatibility with
    previous versions.

## Styles

A style refers to a convenient collection of existing parameters.

- **-gnu**, **--gnu-style**

    **-gnu** gives an approximation to the GNU Coding Standards (which do
    not apply to perl) as they are sometimes implemented.  At present, this
    style overrides the default style with the following parameters:

        -lp -bl -noll -pt=2 -bt=2 -sbt=2 -icp

- **-pbp**, **--perl-best-practices**

    **-pbp** is an abbreviation for the parameters in the book **Perl Best Practices**
    by Damian Conway:

        -l=78 -i=4 -ci=4 -st -se -vt=2 -cti=0 -pt=1 -bt=1 -sbt=1 -bbt=1 -nsfs -nolq
        -wbb="% + - * / x != == >= <= =~ !~ < > | & = 
              **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x="

    Please note that this parameter set includes -st and -se flags, which make
    perltidy act as a filter on one file only.  These can be overridden by placing
    **-nst** and/or **-nse** after the -pbp parameter. 

    Also note that the value of continuation indentation, -ci=4, is equal to the
    value of the full indentation, -i=4.  In some complex statements perltidy will
    produce nicer results with -ci=2. This can be implemented by including -ci=2
    after the -pbp parameter.  For example, 

        # perltidy -pbp
        $self->{_text} = (
             !$section        ? ''
            : $type eq 'item' ? "the $section entry"
            :                   "the section on $section"
            )
            . (
            $page
            ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
            : ' elsewhere in this document'
            );

        # perltidy -pbp -ci=2
        $self->{_text} = (
             !$section        ? ''
            : $type eq 'item' ? "the $section entry"
            :                   "the section on $section"
          )
          . (
            $page
            ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
            : ' elsewhere in this document'
          );

## Controlling Vertical Alignment

Vertical alignment refers to lining up certain symbols in list of consecutive
similar lines to improve readability.  For example, the "fat commas" are
aligned in the following statement:

        $data = $pkg->new(
            PeerAddr => join( ".", @port[ 0 .. 3 ] ),   
            PeerPort => $port[4] * 256 + $port[5],
            Proto    => 'tcp'
        );

The only explicit control on vertical alignment is to turn it off using
**-novalign**, a flag mainly intended for debugging.  However, vertical
alignment can be forced to stop and restart by selectively introducing blank
lines.  For example, a blank has been inserted in the following code
to keep somewhat similar things aligned.

    %option_range = (
        'format'             => [ 'tidy', 'html', 'user' ],
        'output-line-ending' => [ 'dos',  'win',  'mac', 'unix' ],
        'character-encoding' => [ 'none', 'utf8' ],

        'block-brace-tightness'    => [ 0, 2 ],
        'brace-tightness'          => [ 0, 2 ],
        'paren-tightness'          => [ 0, 2 ],
        'square-bracket-tightness' => [ 0, 2 ],
    );

## Other Controls

- Deleting selected text 

    Perltidy can selectively delete comments and/or pod documentation.  The
    command **-dac** or  **--delete-all-comments** will delete all comments
    **and** all pod documentation, leaving just code and any leading system
    control lines.

    The command **-dp** or **--delete-pod** will remove all pod documentation
    (but not comments).

    Two commands which remove comments (but not pod) are: **-dbc** or
    **--delete-block-comments** and **-dsc** or  **--delete-side-comments**.
    (Hanging side comments will be deleted with block comments here.)

    The negatives of these commands also work, and are the defaults.  When
    block comments are deleted, any leading 'hash-bang' will be retained.
    Also, if the **-x** flag is used, any system commands before a leading
    hash-bang will be retained (even if they are in the form of comments).

- Writing selected text to a file

    When perltidy writes a formatted text file, it has the ability to also
    send selected text to a file with a `.TEE` extension.  This text can
    include comments and pod documentation.  

    The command **-tac** or  **--tee-all-comments** will write all comments
    **and** all pod documentation.

    The command **-tp** or **--tee-pod** will write all pod documentation (but
    not comments).

    The commands which write comments (but not pod) are: **-tbc** or
    **--tee-block-comments** and **-tsc** or  **--tee-side-comments**.
    (Hanging side comments will be written with block comments here.)

    The negatives of these commands also work, and are the defaults.  

- Using a `.perltidyrc` command file

    If you use perltidy frequently, you probably won't be happy until you
    create a `.perltidyrc` file to avoid typing commonly-used parameters.
    Perltidy will first look in your current directory for a command file
    named `.perltidyrc`.  If it does not find one, it will continue looking
    for one in other standard locations.  

    These other locations are system-dependent, and may be displayed with
    the command `perltidy -dpro`.  Under Unix systems, it will first look
    for an environment variable **PERLTIDY**.  Then it will look for a
    `.perltidyrc` file in the home directory, and then for a system-wide
    file `/usr/local/etc/perltidyrc`, and then it will look for
    `/etc/perltidyrc`.  Note that these last two system-wide files do not
    have a leading dot.  Further system-dependent information will be found
    in the INSTALL file distributed with perltidy.

    Under Windows, perltidy will also search for a configuration file named perltidy.ini since Windows does not allow files with a leading period (.).
    Use `perltidy -dpro` to see the possible locations for your system.
    An example might be `C:\Documents and Settings\All Users\perltidy.ini`.

    Another option is the use of the PERLTIDY environment variable.
    The method for setting environment variables depends upon the version of
    Windows that you are using.  Instructions for Windows 95 and later versions can
    be found here:

    http://www.netmanage.com/000/20021101\_005\_tcm21-6336.pdf

    Under Windows NT / 2000 / XP the PERLTIDY environment variable can be placed in
    either the user section or the system section.  The later makes the
    configuration file common to all users on the machine.  Be sure to enter the
    full path of the configuration file in the value of the environment variable.
    Ex.  PERLTIDY=C:\\Documents and Settings\\perltidy.ini

    The configuration file is free format, and simply a list of parameters, just as
    they would be entered on a command line.  Any number of lines may be used, with
    any number of parameters per line, although it may be easiest to read with one
    parameter per line.  Comment text begins with a #, and there must
    also be a space before the # for side comments.  It is a good idea to
    put complex parameters in either single or double quotes.

    Here is an example of a `.perltidyrc` file:

        # This is a simple of a .perltidyrc configuration file
        # This implements a highly spaced style
        -se    # errors to standard error output
        -w     # show all warnings
        -bl    # braces on new lines
        -pt=0  # parens not tight at all
        -bt=0  # braces not tight
        -sbt=0 # square brackets not tight

    The parameters in the `.perltidyrc` file are installed first, so any
    parameters given on the command line will have priority over them.  

    To avoid confusion, perltidy ignores any command in the .perltidyrc
    file which would cause some kind of dump and an exit.  These are:

        -h -v -ddf -dln -dop -dsn -dtt -dwls -dwrs -ss

    There are several options may be helpful in debugging a `.perltidyrc`
    file:  

    - A very helpful command is **--dump-profile** or **-dpro**.  It writes a
    list of all configuration filenames tested to standard output, and 
    if a file is found, it dumps the content to standard output before
    exiting.  So, to find out where perltidy looks for its configuration
    files, and which one if any it selects, just enter 

            perltidy -dpro

    - It may be simplest to develop and test configuration files with
    alternative names, and invoke them with **-pro=filename** on the command
    line.  Then rename the desired file to `.perltidyrc` when finished.
    - The parameters in the `.perltidyrc` file can be switched off with 
    the **-npro** option.
    - The commands **--dump-options**, **--dump-defaults**, **--dump-long-names**,
    and **--dump-short-names**, all described below, may all be helpful.

- Creating a new abbreviation

    A special notation is available for use in a `.perltidyrc` file
    for creating an abbreviation for a group
    of options.  This can be used to create a
    shorthand for one or more styles which are frequently, but not always,
    used.  The notation is to group the options within curly braces which
    are preceded by the name of the alias (without leading dashes), like this:

            newword {
            -opt1
            -opt2
            }

    where **newword** is the abbreviation, and **opt1**, etc, are existing parameters
    _or other abbreviations_.  The main syntax requirement is that the new
    abbreviation along with its opening curly brace must begin on a new line.
    Space before and after the curly braces is optional.
    For a
    specific example, the following line

            airy {-bl -pt=0 -bt=0 -sbt=0}

    could be placed in a `.perltidyrc` file, and then invoked at will with

            perltidy -airy somefile.pl

    (Either `-airy` or `--airy` may be used).

- Skipping leading non-perl commands with **-x** or **--look-for-hash-bang**

    If your script has leading lines of system commands or other text which
    are not valid perl code, and which are separated from the start of the
    perl code by a "hash-bang" line, ( a line of the form `#!...perl` ),
    you must use the **-x** flag to tell perltidy not to parse and format any
    lines before the "hash-bang" line.  This option also invokes perl with a
    \-x flag when checking the syntax.  This option was originally added to
    allow perltidy to parse interactive VMS scripts, but it should be used
    for any script which is normally invoked with `perl -x`.

- Making a file unreadable

    The goal of perltidy is to improve the readability of files, but there
    are two commands which have the opposite effect, **--mangle** and
    **--extrude**.  They are actually
    merely aliases for combinations of other parameters.  Both of these
    strip all possible whitespace, but leave comments and pod documents,
    so that they are essentially reversible.  The
    difference between these is that **--mangle** puts the fewest possible
    line breaks in a script while **--extrude** puts the maximum possible.
    Note that these options do not provided any meaningful obfuscation, because
    perltidy can be used to reformat the files.  They were originally
    developed to help test the tokenization logic of perltidy, but they
    have other uses.
    One use for **--mangle** is the following:

        perltidy --mangle myfile.pl -st | perltidy -o myfile.pl.new

    This will form the maximum possible number of one-line blocks (see next
    section), and can sometimes help clean up a badly formatted script.

    A similar technique can be used with **--extrude** instead of **--mangle**
    to make the minimum number of one-line blocks.

    Another use for **--mangle** is to combine it with **-dac** to reduce
    the file size of a perl script.

- One-line blocks 

    There are a few points to note regarding one-line blocks.  A one-line
    block is something like this,

            if ($x > 0) { $y = 1 / $x }  

    where the contents within the curly braces is short enough to fit
    on a single line.

    With few exceptions, perltidy retains existing one-line blocks, if it
    is possible within the line-length constraint, but it does not attempt
    to form new ones.  In other words, perltidy will try to follow the
    one-line block style of the input file.

    If an existing one-line block is longer than the maximum line length,
    however, it will be broken into multiple lines.  When this happens, perltidy
    checks for and adds any optional terminating semicolon (unless the **-nasc**
    option is used) if the block is a code block.  

    The main exception is that perltidy will attempt to form new one-line
    blocks following the keywords `map`, `eval`, and `sort`, because
    these code blocks are often small and most clearly displayed in a single
    line.

    One-line block rules can conflict with the cuddled-else option.  When
    the cuddled-else option is used, perltidy retains existing one-line
    blocks, even if they do not obey cuddled-else formatting.

    Occasionally, when one-line blocks get broken because they exceed the
    available line length, the formatting will violate the requested brace style.
    If this happens, reformatting the script a second time should correct
    the problem.

- Debugging 

    The following flags are available for debugging:

    **--dump-cuddled-block-list** or **-dcbl** will dump to standard output the
    internal hash of cuddled block types created by a **-cuddled-block-list** input
    string.

    **--dump-defaults** or **-ddf** will write the default option set to standard output and quit

    **--dump-profile** or **-dpro**  will write the name of the current 
    configuration file and its contents to standard output and quit.

    **--dump-options** or **-dop**  will write current option set to standard
    output and quit.  

    **--dump-long-names** or **-dln**  will write all command line long names (passed 
    to Get\_options) to standard output and quit.

    **--dump-short-names**  or **-dsn** will write all command line short names 
    to standard output and quit.

    **--dump-token-types** or **-dtt**  will write a list of all token types 
    to standard output and quit.

    **--dump-want-left-space** or **-dwls**  will write the hash %want\_left\_space
    to standard output and quit.  See the section on controlling whitespace
    around tokens.

    **--dump-want-right-space** or **-dwrs**  will write the hash %want\_right\_space
    to standard output and quit.  See the section on controlling whitespace
    around tokens.

    **--no-memoize** or **-nmem**  will turn of memoizing.
    Memoization can reduce run time when running perltidy repeatedly in a 
    single process.  It is on by default but can be deactivated for
    testing with **-nmem**.

    **--no-timestamp** or **-nts** will eliminate any time stamps in output files to prevent
    differences in dates from causing test installation scripts to fail. There are just
    a couple of places where timestamps normally occur. One is in the headers of
    html files, and another is when the **-cscw** option is selected. The default is
    to allow timestamps (**--timestamp** or **-ts**).

    **--file-size-order** or **-fso** will cause files to be processed in order of
    increasing size, when multiple files are being processed.  This is useful
    during program development, when large numbers of files with varying sizes are
    processed, because it can reduce virtual memory usage. 

    **-DEBUG**  will write a file with extension `.DEBUG` for each input file 
    showing the tokenization of all lines of code.

- Working with MakeMaker, AutoLoader and SelfLoader

    The first $VERSION line of a file which might be eval'd by MakeMaker
    is passed through unchanged except for indentation.  
    Use **--nopass-version-line**, or **-npvl**, to deactivate this feature.

    If the AutoLoader module is used, perltidy will continue formatting
    code after seeing an \_\_END\_\_ line.
    Use **--nolook-for-autoloader**, or **-nlal**, to deactivate this feature.

    Likewise, if the SelfLoader module is used, perltidy will continue formatting
    code after seeing a \_\_DATA\_\_ line.
    Use **--nolook-for-selfloader**, or **-nlsl**, to deactivate this feature.

- Working around problems with older version of Perl 

    Perltidy contains a number of rules which help avoid known subtleties
    and problems with older versions of perl, and these rules always
    take priority over whatever formatting flags have been set.  For example,
    perltidy will usually avoid starting a new line with a bareword, because
    this might cause problems if `use strict` is active.

    There is no way to override these rules.

# HTML OPTIONS

- The **-html** master switch

    The flag **-html** causes perltidy to write an html file with extension
    `.html`.  So, for example, the following command

            perltidy -html somefile.pl

    will produce a syntax-colored html file named `somefile.pl.html`
    which may be viewed with a browser.

    **Please Note**: In this case, perltidy does not do any formatting to the
    input file, and it does not write a formatted file with extension
    `.tdy`.  This means that two perltidy runs are required to create a
    fully reformatted, html copy of a script.  

- The **-pre** flag for code snippets

    When the **-pre** flag is given, only the pre-formatted section, within
    the <PRE> and &lt;/PRE> tags, will be output.  This simplifies inclusion
    of the output in other files.  The default is to output a complete
    web page.

- The **-nnn** flag for line numbering

    When the **-nnn** flag is given, the output lines will be numbered.

- The **-toc**, or **--html-table-of-contents** flag

    By default, a table of contents to packages and subroutines will be
    written at the start of html output.  Use **-ntoc** to prevent this.
    This might be useful, for example, for a pod document which contains a
    number of unrelated code snippets.  This flag only influences the code
    table of contents; it has no effect on any table of contents produced by
    pod2html (see next item).

- The **-pod**, or **--pod2html** flag

    There are two options for formatting pod documentation.  The default is
    to pass the pod through the Pod::Html module (which forms the basis of
    the pod2html utility).  Any code sections are formatted by perltidy, and
    the results then merged.  Note: perltidy creates a temporary file when
    Pod::Html is used; see ["FILES"](#files).  Also, Pod::Html creates temporary
    files for its cache.

    NOTE: Perltidy counts the number of `=cut` lines, and either moves the
    pod text to the top of the html file if there is one `=cut`, or leaves
    the pod text in its original order (interleaved with code) otherwise.

    Most of the flags accepted by pod2html may be included in the perltidy
    command line, and they will be passed to pod2html.  In some cases,
    the flags have a prefix `pod` to emphasize that they are for the
    pod2html, and this prefix will be removed before they are passed to
    pod2html.  The flags which have the additional `pod` prefix are:

        --[no]podheader --[no]podindex --[no]podrecurse --[no]podquiet 
        --[no]podverbose --podflush

    The flags which are unchanged from their use in pod2html are:

        --backlink=s --cachedir=s --htmlroot=s --libpods=s --title=s
        --podpath=s --podroot=s 

    where 's' is an appropriate character string.  Not all of these flags are
    available in older versions of Pod::Html.  See your Pod::Html documentation for
    more information.

    The alternative, indicated with **-npod**, is not to use Pod::Html, but
    rather to format pod text in italics (or whatever the stylesheet
    indicates), without special html markup.  This is useful, for example,
    if pod is being used as an alternative way to write comments.

- The **-frm**, or **--frames** flag

    By default, a single html output file is produced.  This can be changed
    with the **-frm** option, which creates a frame holding a table of
    contents in the left panel and the source code in the right side. This
    simplifies code browsing.  Assume, for example, that the input file is
    `MyModule.pm`.  Then, for default file extension choices, these three
    files will be created:

        MyModule.pm.html      - the frame
        MyModule.pm.toc.html  - the table of contents
        MyModule.pm.src.html  - the formatted source code

    Obviously this file naming scheme requires that output be directed to a real
    file (as opposed to, say, standard output).  If this is not the
    case, or if the file extension is unknown, the **-frm** option will be
    ignored.

- The **-text=s**, or **--html-toc-extension** flag

    Use this flag to specify the extra file extension of the table of contents file
    when html frames are used.  The default is "toc".
    See ["Specifying File Extensions"](#specifying-file-extensions).

- The **-sext=s**, or **--html-src-extension** flag

    Use this flag to specify the extra file extension of the content file when html
    frames are used.  The default is "src".
    See ["Specifying File Extensions"](#specifying-file-extensions).

- The **-hent**, or **--html-entities** flag

    This flag controls the use of Html::Entities for html formatting.  By
    default, the module Html::Entities is used to encode special symbols.
    This may not be the right thing for some browser/language
    combinations.  Use --nohtml-entities or -nhent to prevent this.

- Style Sheets

    Style sheets make it very convenient to control and adjust the
    appearance of html pages.  The default behavior is to write a page of
    html with an embedded style sheet.

    An alternative to an embedded style sheet is to create a page with a
    link to an external style sheet.  This is indicated with the
    **-css=filename**,  where the external style sheet is `filename`.  The
    external style sheet `filename` will be created if and only if it does
    not exist.  This option is useful for controlling multiple pages from a
    single style sheet.

    To cause perltidy to write a style sheet to standard output and exit,
    use the **-ss**, or **--stylesheet**, flag.  This is useful if the style
    sheet could not be written for some reason, such as if the **-pre** flag
    was used.  Thus, for example,

        perltidy -html -ss >mystyle.css

    will write a style sheet with the default properties to file
    `mystyle.css`.

    The use of style sheets is encouraged, but a web page without a style
    sheets can be created with the flag **-nss**.  Use this option if you
    must to be sure that older browsers (roughly speaking, versions prior to
    4.0 of Netscape Navigator and Internet Explorer) can display the
    syntax-coloring of the html files.

- Controlling HTML properties

    Note: It is usually more convenient to accept the default properties
    and then edit the stylesheet which is produced.  However, this section
    shows how to control the properties with flags to perltidy.

    Syntax colors may be changed from their default values by flags of the either
    the long form, **-html-color-xxxxxx=n**, or more conveniently the short form,
    **-hcx=n**, where **xxxxxx** is one of the following words, and **x** is the
    corresponding abbreviation:

          Token Type             xxxxxx           x 
          ----------             --------         --
          comment                comment          c
          number                 numeric          n
          identifier             identifier       i
          bareword, function     bareword         w
          keyword                keyword          k
          quite, pattern         quote            q
          here doc text          here-doc-text    h
          here doc target        here-doc-target  hh
          punctuation            punctuation      pu
          parentheses            paren            p
          structural braces      structure        s
          semicolon              semicolon        sc
          colon                  colon            co
          comma                  comma            cm
          label                  label            j
          sub definition name    subroutine       m
          pod text               pod-text         pd

    A default set of colors has been defined, but they may be changed by providing
    values to any of the following parameters, where **n** is either a 6 digit 
    hex RGB color value or an ascii name for a color, such as 'red'.

    To illustrate, the following command will produce an html 
    file `somefile.pl.html` with "aqua" keywords:

            perltidy -html -hck=00ffff somefile.pl

    and this should be equivalent for most browsers:

            perltidy -html -hck=aqua somefile.pl

    Perltidy merely writes any non-hex names that it sees in the html file.
    The following 16 color names are defined in the HTML 3.2 standard:

            black   => 000000,
            silver  => c0c0c0,
            gray    => 808080,
            white   => ffffff,
            maroon  => 800000,
            red     => ff0000,
            purple  => 800080,
            fuchsia => ff00ff,
            green   => 008000,
            lime    => 00ff00,
            olive   => 808000,
            yellow  => ffff00
            navy    => 000080,
            blue    => 0000ff,
            teal    => 008080,
            aqua    => 00ffff,

    Many more names are supported in specific browsers, but it is safest
    to use the hex codes for other colors.  Helpful color tables can be
    located with an internet search for "HTML color tables". 

    Besides color, two other character attributes may be set: bold, and italics.
    To set a token type to use bold, use the flag
    **--html-bold-xxxxxx** or **-hbx**, where **xxxxxx** or **x** are the long
    or short names from the above table.  Conversely, to set a token type to 
    NOT use bold, use **--nohtml-bold-xxxxxx** or **-nhbx**.

    Likewise, to set a token type to use an italic font, use the flag
    **--html-italic-xxxxxx** or **-hix**, where again **xxxxxx** or **x** are the
    long or short names from the above table.  And to set a token type to
    NOT use italics, use **--nohtml-italic-xxxxxx** or **-nhix**.

    For example, to use bold braces and lime color, non-bold, italics keywords the
    following command would be used:

            perltidy -html -hbs -hck=00FF00 -nhbk -hik somefile.pl

    The background color can be specified with **--html-color-background=n**,
    or **-hcbg=n** for short, where n is a 6 character hex RGB value.  The
    default color of text is the value given to **punctuation**, which is
    black as a default.

    Here are some notes and hints:

    1\. If you find a preferred set of these parameters, you may want
    to create a `.perltidyrc` file containing them.  See the perltidy man
    page for an explanation.

    2\. Rather than specifying values for these parameters, it is probably
    easier to accept the defaults and then edit a style sheet.  The style
    sheet contains comments which should make this easy.

    3\. The syntax-colored html files can be very large, so it may be best to
    split large files into smaller pieces to improve download times.

# SOME COMMON INPUT CONVENTIONS

## Specifying Block Types

Several parameters which refer to code block types may be customized by also
specifying an associated list of block types.  The type of a block is the name
of the keyword which introduces that block, such as **if**, **else**, or **sub**.
An exception is a labeled block, which has no keyword, and should be specified
with just a colon.  To specify all blocks use **'\*'**.

The keyword **sub** indicates a named sub.  For anonymous subs, use the special
keyword **asub**.

For example, the following parameter specifies `sub`, labels, `BEGIN`, and
`END` blocks:

    -cscl="sub : BEGIN END"

(the meaning of the -cscl parameter is described above.)  Note that
quotes are required around the list of block types because of the
spaces.  For another example, the following list specifies all block types
for vertical tightness:

    -bbvtl='*'

## Specifying File Extensions

Several parameters allow default file extensions to be overridden.  For
example, a backup file extension may be specified with **-bext=ext**,
where **ext** is some new extension.  In order to provides the user some
flexibility, the following convention is used in all cases to decide if
a leading '.' should be used.  If the extension `ext` begins with
`A-Z`, `a-z`, or `0-9`, then it will be appended to the filename with
an intermediate '.' (or perhaps an '\_' on VMS systems).  Otherwise, it
will be appended directly.  

For example, suppose the file is `somefile.pl`.  For `-bext=old`, a '.' is
added to give `somefile.pl.old`.  For `-bext=.old`, no additional '.' is
added, so again the backup file is `somefile.pl.old`.  For `-bext=~`, then no
dot is added, and the backup file will be `somefile.pl~`  .  

# SWITCHES WHICH MAY BE NEGATED

The following list shows all short parameter names which allow a prefix
'n' to produce the negated form:

    D    anl asc  aws  b    bbb bbc bbs  bl   bli  boc bok  bol  bot  ce
    csc  dac dbc  dcsc ddf  dln dnl dop  dp   dpro dsc dsm  dsn  dtt  dwls
    dwrs dws f    fll  frm  fs  hsc html ibc  icb  icp iob  isbc lal  log
    lp   lsl ohbr okw  ola  oll opr opt  osbr otr  ple  pod  pvl  q
    sbc  sbl schb scp  scsb sct se  sfp  sfs  skp  sob sohb sop  sosb sot
    ssc  st  sts  syn  t    tac tbc toc  tp   tqw  tsc w    x    bar  kis

Equivalently, the prefix 'no' or 'no-' on the corresponding long names may be
used.

# LIMITATIONS

- Parsing Limitations

    Perltidy should work properly on most perl scripts.  It does a lot of
    self-checking, but still, it is possible that an error could be
    introduced and go undetected.  Therefore, it is essential to make
    careful backups and to test reformatted scripts.

    The main current limitation is that perltidy does not scan modules
    included with 'use' statements.  This makes it necessary to guess the
    context of any bare words introduced by such modules.  Perltidy has good
    guessing algorithms, but they are not infallible.  When it must guess,
    it leaves a message in the log file.

    If you encounter a bug, please report it.

- What perltidy does not parse and format

    Perltidy indents but does not reformat comments and `qw` quotes. 
    Perltidy does not in any way modify the contents of here documents or
    quoted text, even if they contain source code.  (You could, however,
    reformat them separately).  Perltidy does not format 'format' sections
    in any way.  And, of course, it does not modify pod documents.

# FILES

- Temporary files

    Under the -html option with the default --pod2html flag, a temporary file is
    required to pass text to Pod::Html.  Unix systems will try to use the POSIX
    tmpnam() function.  Otherwise the file `perltidy.TMP` will be temporarily
    created in the current working directory.

- Special files when standard input is used

    When standard input is used, the log file, if saved, is `perltidy.LOG`,
    and any errors are written to `perltidy.ERR` unless the **-se** flag is
    set.  These are saved in the current working directory.  

- Files overwritten

    The following file extensions are used by perltidy, and files with these
    extensions may be overwritten or deleted: `.ERR`, `.LOG`, `.TEE`,
    and/or `.tdy`, `.html`, and `.bak`, depending on the run type and
    settings.

- Files extensions limitations

    Perltidy does not operate on files for which the run could produce a file with
    a duplicated file extension.  These extensions include `.LOG`, `.ERR`,
    `.TEE`, and perhaps `.tdy` and `.bak`, depending on the run type.  The
    purpose of this rule is to prevent generating confusing filenames such as
    `somefile.tdy.tdy.tdy`.

# SEE ALSO

perlstyle(1), Perl::Tidy(3)

# VERSION

This man page documents perltidy version 20181118

# BUG REPORTS

A list of current bugs and issues can be found at the CPAN site [https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)

To report a new bug or problem, use the link on this page.  

The source code repository is at [https://github.com/perltidy/perltidy](https://github.com/perltidy/perltidy).

# COPYRIGHT

Copyright (c) 2000-2018 by Steve Hancock

# LICENSE

This package is free software; you can redistribute it and/or modify it
under the terms of the "GNU General Public License".

Please refer to the file "COPYING" for details.

# DISCLAIMER

This package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the "GNU General Public License" for more details.
