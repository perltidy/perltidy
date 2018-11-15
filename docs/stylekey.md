# Perltidy Style Key

When perltidy was first developed, the main parameter choices were the number
of indentation spaces and if the user liked cuddled else's.  As the number of
users has grown so has the number of parameters.  Now there are so many that it
can be difficult for a new user to find a good initial set.  This document is
one attempt to help with this problem, and some other suggestions are given at
the end.

Use this document to methodically find a starting set of perltidy parameters to
approximate your style.  We will be working on just one aspect of formatting at
a time.  Just read each question and select the best answer.  Enter your
parameters in a file named `.perltidyrc` (examples are listed at the end).
Then move it to one of the places where perltidy will find it.  You can run
perltidy with the parameter **-dpro** to see where these places are for your
system.

## Before You Start

Before you begin, experiment using just `perltidy filename.pl` on some
of your files.  From the results (which you will find in files with a
`.tdy` extension), you will get a sense of what formatting changes, if
any, you'd like to make.  If the default formatting is acceptable, you
do not need a `.perltidyrc` file.

## Use as Filter?

Do you almost always want to run perltidy as a standard filter on just
one input file?  If yes, use **-st** and **-se**.  

## Line Length Setting

Perltidy will set line breaks to prevent lines from exceeding the
maximum line length.  

Do you want the maximum line length to be 80 columns?  If no, use
**-l=n**, where **n** is the number of columns you prefer.

## Indentation in Code Blocks

In the block below, the variable `$anchor` is one indentation level deep
and is indented by 4 spaces as shown here: 

    if ( $flag eq "a" ) {
        $anchor = $header;
    }  

If you want to change this to be a different number **n** of spaces
per indentation level, use **-i=n**.

## Continuation Indentation

Look at the statement beginning with `$anchor`:

    if ( $flag eq "a" ) {
        $anchor =
          substr( $header, 0, 6 )
          . substr( $char_list, $place_1, 1 )
          . substr( $char_list, $place_2, 1 );
    }

The statement is too long for the line length (80 characters by default), so it
has been broken into 4 lines.  The second and later lines have some extra
"continuation indentation" to help make the start of the statement easy to
find.  The default number of extra spaces is 2.  If you prefer a number n
different from 2, you may specify this with **-ci=n**.  It is probably best if
it does not exceed the value of the primary indentation.

## Tabs

The default, and recommendation, is to represent leading whitespace
with actual space characters.  However, if you prefer to entab
leading whitespace with one tab character for each **n** spaces,
use **-et=n**.  Typically, **n** would be 8.  

## Opening Block Brace Right or Left?

Opening and closing curly braces, parentheses, and square brackets are divided
into two separate categories and controlled separately in most cases.  The two
categories are (1) code block curly braces, which contain perl code, and (2)
everything else.  Basically, a code block brace is one which could contain
semicolon-terminated lines of perl code.  We will first work on the scheme for
code block curly braces.  

Decide which of the following opening brace styles you prefer for most blocks
of code (with the possible exception of a **sub block brace** which will
be covered later):

If you like opening braces on the right, like this, go to 
["Opening Braces Right"](#opening-braces-right).

    if ( $flag eq "h" ) {
        $headers = 0;
    }  

If you like opening braces on the left, like this, go to 
["Opening Braces Left"](#opening-braces-left).

    if ( $flag eq "h" )
    {
        $headers = 0;
    }

## Opening Braces Right

In a multi-line **if** test expression, the default is to place
the opening brace on the left, like this:

    if ( $bigwasteofspace1 && $bigwasteofspace2
        || $bigwasteofspace3 && $bigwasteofspace4 )
    {
        big_waste_of_time();
    }

This helps to visually separate the block contents from the test
expression.  

An alternative is to keep the brace on the right even for
multiple-line test expressions, like this:

    if ( $bigwasteofspace1 && $bigwasteofspace2
        || $bigwasteofspace3 && $bigwasteofspace4 ) {
        big_waste_of_time();
    }

If you prefer this alternative, use **-bar**.

## Cuddled Else?

Do you prefer this **Cuddled Else** style

    if ( $flag eq "h" ) {
        $headers = 0;
    } elsif ( $flag eq "f" ) {
        $sectiontype = 3;
    } else {
        print "invalid option: " . substr( $arg, $i, 1 ) . "\n";
        dohelp();
    }

instead of this default style?

    if ( $flag eq "h" ) {
        $headers = 0;
    }  
    elsif ( $flag eq "f" ) {
        $sectiontype = 3;
    } 
    else {    
        print "invalid option: " . substr( $arg, $i, 1 ) . "\n";
        dohelp();
    }

If yes, you should use **-ce**.
Now skip ahead to ["Opening Sub Braces"](#opening-sub-braces).

## Opening Braces Left

Use **-bl** if you prefer this style:

    if ( $flag eq "h" )
    {
        $headers = 0;
    }

Use **-bli** if you prefer this indented-brace style:

    if ( $flag eq "h" )
      {
        $headers = 0;
      }

The number of spaces of extra indentation will be the value specified
for continuation indentation with the **-ci=n** parameter (2 by default).

## Opening Sub Braces

By default, the opening brace of a sub block will be treated
the same as other code blocks.  If this is okay, skip ahead
to ["Block Brace Vertical Tightness"](#block-brace-vertical-tightness).

If you prefer an opening sub brace to be on a new line,
like this: 

    sub message
    {
        # -sbl
    }

use **-sbl**.  If you prefer the sub brace on the right like this

    sub message {

        # -nsbl
    }

use **-nsbl**.

If you wish to give this opening sub brace some indentation you can do
that with the parameters **-bli** and **-blil** which are described in the
manual.

## Block Brace Vertical Tightness

If you chose to put opening block braces of all types to the right, skip
ahead to ["Closing Block Brace Indentation"](#closing-block-brace-indentation).

If you chose to put braces of any type on the left, the default is to leave the
opening brace on a line by itself, like this (shown for **-bli**, but also true
for **-bl**):

    if ( $flag eq "h" )
      {
        $headers = 0;
      }

But you may also use this more compressed style if you wish:

    if ( $flag eq "h" )
      { $headers = 0;
      }

If you do not prefer this more compressed form, go to 
["Opening Sub Braces"](#opening-sub-braces).

Otherwise use parameter **-bbvt=n**, where n=1 or n=2.  To decide,
look at this snippet:

    # -bli -bbvt=1
    sub _directives
      {
        {
            'ENDIF' => \&_endif,
               'IF' => \&_if,
        };
      }

    # -bli -bbvt=2
    sub _directives
    {   {
            'ENDIF' => \&_endif,
            'IF'    => \&_if,
        };
    }

The difference is that **-bbvt=1** breaks after an opening brace if
the next line is unbalanced, whereas **-bbvt=2** never breaks.  

If you were expecting the 'ENDIF' word to move up vertically here, note that
the second opening brace in the above example is not a code block brace (it is
a hash brace), so the **-bbvt** does not apply to it (another parameter will).

## Closing Block Brace Indentation

The default is to place closing braces at the same indentation as the
opening keyword or brace of that code block, as shown here:

        if ($task) {
            yyy();
        }            # default

If you chose the **-bli** style, however, the default closing braces will be
indented one continuation indentation like the opening brace:

        if ($task)
          {
            yyy();
          }    # -bli

If you prefer to give closing block braces one full level of
indentation, independently of how the opening brace is treated,
for example like this:

        if ($task) {
            yyy();
            }          # -icb

use **-icb**.

This completes the definition of the placement of code block braces.

## Indentation Style for Other Containers

You have a choice of two basic indentation schemes for non-block containers.
The default is to use a fixed number of spaces per indentation level (the same
number of spaces used for code blocks, which is 4 by default).  Here is an
example of the default:

    $dbh = DBI->connect(
        undef, undef, undef,
        {
            PrintError => 0,
            RaiseError => 1
        }
    );

In this default indentation scheme, a simple formula is used to find the
indentation of every line.  Notice how the first 'undef' is indented 4
spaces (one level) to the right, and how 'PrintError' is indented 4 more
speces (one more level) to the right.  

The alternate is to let the location of the opening paren (or square
bracket, or curly brace) define the indentation, like this:

    $dbh = DBI->connect(
                         undef, undef, undef,
                         {
                           PrintError => 0,
                           RaiseError => 1
                         }
    );

The first scheme is completely robust.  The second scheme often looks a little
nicer, but be aware that deeply nested structures it can be spoiled if the line
length limit is exceeded.  Also, if there are comments or blank lines within a
complex structure perltidy will temporarily fall back on the default
indentation scheme.  You may want to try both on large sections of code to see
which works best.

If you prefer the first (default) scheme, no parameter is needed.

If you prefer the latter scheme, use **-lp**. 

## Opening Vertical Tightness

The information in this section applies mainly to the **-lp**
style but it also applies in some cases to the default style.
It will be illustrated for the **-lp** indentation style.

The default **-lp** indentation style ends a line at the
opening tokens, like this:

    $dbh = DBI->connect(
                         undef, undef, undef,
                         {
                           PrintError => 0,
                           RaiseError => 1
                         }
    );

Here is a tighter alternative, which does not end a line
with the opening tokens:

    $dbh = DBI->connect( undef, undef, undef,
                         { PrintError => 0,
                           RaiseError => 1
                         }
    );

The difference is that the lines have been compressed vertically without
any changes to the indentation.  This can almost always be done with the
**-lp** indentation style, but only in limited cases for the default
indentation style. 

If you prefer the default, skip ahead to ["Closing Token Placement"](#closing-token-placement).

Otherwise, use **-vt=n**, where **n** should be either 1 or 2.  To help
decide, observe the first three opening parens in the following snippet
and choose the value of n you prefer.  Here it is with **-lp -vt=1**:

    if (
         !defined(
                   start_slip( $DEVICE, $PHONE,  $ACCOUNT, $PASSWORD,
                               $LOCAL,  $REMOTE, $NETMASK, $MTU
                   )
         )
         && $continuation_flag
      )
    {
        do_something_about_it();
    }

And here it is again formatted with **-lp -vt=2**:

    if ( !defined( start_slip( $DEVICE, $PHONE,  $ACCOUNT, $PASSWORD,
                               $LOCAL,  $REMOTE, $NETMASK, $MTU
                   )
         )
         && $continuation_flag
      )
    {
        do_something_about_it();
    }

The **-vt=1** style tries to display the structure by preventing more
than one step in indentation per line. In this example, the first two
opening parens were not followed by balanced lines, so **-vt=1** broke
after them.  

The **-vt=2** style does not limit itself to a single indentation step
per line.

Note that in the above example the function 'do\_sumething\_about\_it'
started on a new line. This is because it follows an opening code
block brace and is governed by the flag previously set in 
["Block Brace Vertical Tightness"](#block-brace-vertical-tightness).

## Closing Token Placement

You have several options for dealing with the terminal closing tokens of
non-blocks.  In the following examples, a closing parenthesis is shown, but
these parameters apply to closing square brackets and non-block curly braces as
well.  

The default behavior for parenthesized relatively large lists is to place the
closing paren on a separate new line.  The flag **-cti=n** controls the amount
of indentation of such a closing paren.

The default, **-cti=0**, for a line beginning with a closing paren, is to use
the indentation defined by the next (lower) indentation level.  This works
well for the default indentation scheme:

    # perltidy
    @month_of_year = (
        'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
    );

but it may not look very good with the **-lp** indentation scheme:

    # perltidy -lp
    @month_of_year = (
                       'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
    );

An alternative which works well with **-lp** indentation is **-cti=1**,
which aligns the closing paren vertically with its
opening paren, if possible:  

    # perltidy -lp -cti=1
    @month_of_year = (
                       'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
                     );

Another alternative, **-cti=3**, indents a line with leading closing
paren one full indentation level:

    # perltidy -lp -cti=3
    @month_of_year = (
                       'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
                       );

If you prefer the closing paren on a separate line like this, 
note the value of **-cti=n** that you prefer and skip ahead to 
["Define Horizontal Tightness"](#define-horizontal-tightness). 

Finally, the question of paren indentation can be avoided by placing it
at the end of the previous line, like this:

    @month_of_year = (
        'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' );

Perltidy will automatically do this to save space for very short lists but not
for longer lists.

Use **-vtc=n** if you prefer to usually do this, where **n** is either 1 or 2. To
determine **n**, we have to look at something more complex.  Observe the
behavior of the closing tokens in the following snippet:

Here is **-lp -vtc=1**:

    $srec->{'ACTION'} = [
                          $self->read_value(
                                             $lookup->{'VFMT'},
                                             $loc, $lookup, $fh
                          ),
                          $self->read_value(
                                             $lookup->{'VFMT2'},
                                             $loc, $lookup, $fh
                          ) ];

Here is **-lp -vtc=2**:

    $srec->{'ACTION'} = [
                          $self->read_value(
                                             $lookup->{'VFMT'},
                                             $loc, $lookup, $fh ),
                          $self->read_value(
                                             $lookup->{'VFMT2'},
                                             $loc, $lookup, $fh ) ];

Choose the one that you prefer.  The difference is that **-vtc=1** leaves
closing tokens at the start of a line within a list, which can assist in
keeping hierarchical lists readable.  The **-vtc=2** style always tries
to move closing tokens to the end of a line.  

If you choose **-vtc=1**,
you may also want to specify a value of **-cti=n** (previous section) to
handle cases where a line begins with a closing paren.

## Stack Opening Tokens

In the following snippet the opening hash brace has been placed
alone on a new line.  

    $opt_c = Text::CSV_XS->new(
        {
            binary       => 1,
            sep_char     => $opt_c,
            always_quote => 1,
        }
    );

If you prefer to avoid isolated opening tokens by
"stacking" them together with other opening tokens like this:

    $opt_c = Text::CSV_XS->new( {
            binary       => 1,
            sep_char     => $opt_c,
            always_quote => 1,
        }
    );

use **-sot**.

## Stack Closing Tokens

Likewise, in the same snippet the default formatting leaves
the closing paren on a line by itself here:

    $opt_c = Text::CSV_XS->new(
        {
            binary       => 1,
            sep_char     => $opt_c,
            always_quote => 1,
        }
    );

If you would like to avoid leaving isolated closing tokens by
stacking them with other closing tokens, like this:

    $opt_c = Text::CSV_XS->new(
        {
            binary       => 1,
            sep_char     => $opt_c,
            always_quote => 1,
        } );

use **-sct**.

The **-sct** flag is somewhat similar to the **-vtc** flags, and in some cases it
can give a similar result.  The difference is that the **-vtc** flags try to
avoid lines with leading opening tokens by "hiding" them at the end of a
previous line, whereas the **-sct** flag merely tries to reduce the number of
lines with isolated closing tokens by stacking multiple closing tokens
together, but it does not try to hide them.  

The manual shows how all of these vertical tightness controls may be applied
independently to each type of non-block opening and opening token.

## Define Horizontal Tightness

Horizontal tightness parameters define how much space is included
within a set of container tokens.

For parentheses, decide which of the following values of **-pt=n**
you prefer: 

    if ( ( my $len_tab = length( $tabstr ) ) > 0 ) {  # -pt=0
    if ( ( my $len_tab = length($tabstr) ) > 0 ) {    # -pt=1 (default)
    if ((my $len_tab = length($tabstr)) > 0) {        # -pt=2

For n=0, space is always used, and for n=2, space is never used.  For
the default n=1, space is used if the parentheses contain more than
one token.

For square brackets, decide which of the following values of **-sbt=n**
you prefer:

    $width = $col[ $j + $k ] - $col[ $j ];  # -sbt=0
    $width = $col[ $j + $k ] - $col[$j];    # -sbt=1 (default)
    $width = $col[$j + $k] - $col[$j];      # -sbt=2 

For curly braces, decide which of the following values of **-bt=n**
you prefer:

    $obj->{ $parsed_sql->{ 'table' }[0] };    # -bt=0
    $obj->{ $parsed_sql->{'table'}[0] };      # -bt=1 (default)
    $obj->{$parsed_sql->{'table'}[0]};        # -bt=2

For code block curly braces, decide which of the following values of
**-bbt=n** you prefer: 

    %bf = map { $_ => -M $_ } grep { /\.deb$/ } dirents '.'; # -bbt=0 (default)
    %bf = map { $_ => -M $_ } grep {/\.deb$/} dirents '.';   # -bbt=1
    %bf = map {$_ => -M $_} grep {/\.deb$/} dirents '.';     # -bbt=2

## Spaces between function names and opening parens

The default is not to place a space after a function call:

    myfunc( $a, $b, $c );    # default 

If you prefer a space:

    myfunc ( $a, $b, $c );   # -sfp

use **-sfp**.

## Spaces between Perl keywords and parens

The default is to place a space between only these keywords
and an opening paren:

    my local our and or eq ne if else elsif until unless 
    while for foreach return switch case given when

but no others. For example, the default is:

    $aa = pop(@bb);

If you want a space between all Perl keywords and an opening paren,

    $aa = pop (@bb);

use **-skp**.  For detailed control of individual keywords, see the manual.

## Statement Termination Semicolon Spaces

The default is not to put a space before a statement termination
semicolon, like this:

    $i = 1;

If you prefer a space, like this:

    $i = 1 ; 

enter **-sts**.

## For Loop Semicolon Spaces

The default is to place a space before a semicolon in a for statement,
like this:

    for ( @a = @$ap, $u = shift @a ; @a ; $u = $v ) {  # -sfs (default)

If you prefer no such space, like this:

    for ( @a = @$ap, $u = shift @a; @a; $u = $v ) {    # -nsfs

enter **-nsfs**.

## Block Comment Indentation

Block comments are comments which occupy a full line, as opposed to side
comments.  The default is to indent block comments with the same
indentation as the code block that contains them (even though this
will allow long comments to exceed the maximum line length). 

If you would like block comments indented except when this would cause
the maximum line length to be exceeded, use **-olc**.  This will cause a
group of consecutive block comments to be outdented by the amount needed
to prevent any one from exceeding the maximum line length. 

If you never want block comments indented, use **-nibc**.

If block comments may only be indented if they have some space
characters before the leading `#` character in the input file, use
**-isbc**.

The manual shows many other options for controlling comments.

## Outdenting Long Quotes

Long quoted strings may exceed the specified line length limit.  The
default, when this happens, is to outdent them to the first column.
Here is an example of an outdented long quote:

           if ($source_stream) {
               if ( @ARGV > 0 ) {
                   die
    "You may not specify any filenames when a source array is given\n";
               }
           }

The effect is not too different from using a here document to represent
the quote.  If you prefer to leave the quote indented, like this:

        if ($source_stream) {
            if ( @ARGV > 0 ) {
                die
                  "You may not specify any filenames when a source array is given\n";
            }
        }

use **-nolq**.

## Many Other Parameters

This document has only covered the most popular parameters.  The manual
contains many more and should be consulted if you did not find what you need
here.

## Example `.perltidyrc` files

Now gather together all of the parameters you prefer and enter them
in a file called `.perltidyrc`.

Here are some example `.perltidyrc` files and the corresponding style.

Here is a little test snippet, shown the way it would appear with
the default style.

    for (@methods) {
        push (
            @results,
            {
                name => $_->name,
                help => $_->help,
            }
        );
    }

You do not need a `.perltidyrc` file for this style.

Here is the same snippet

    for (@methods)
    {
        push(@results,
             {  name => $_->name,
                help => $_->help,
             }
            );
    }

for a `.perltidyrc` file containing these parameters:

    -bl
    -lp
    -cti=1
    -vt=1
    -pt=2

You do not need to place just one parameter per line, but this may be
convenient for long lists.  You may then hide any parameter by placing
a `#` symbol before it.

And here is the snippet

    for (@methods) {
        push ( @results,
               { name => $_->name,
                 help => $_->help,
               } );
    }

for a `.perltidyrc` file containing these parameters:

    -lp
    -vt=1
    -vtc=1

## Tidyview

There is a graphical program called **tidyview** which you can use to read a
preliminary `.perltidyrc` file, make trial adjustments and immediately see
their effect on a test file, and then write a new `.perltidyrc`.  You can
download a copy at

http://sourceforge.net/projects/tidyview

## Additional Information

This document has covered the main parameters.  Many more parameters are
available for special purposes and for fine-tuning a style.  For complete
information see the perltidy manual
http://perltidy.sourceforge.net/perltidy.html

For an introduction to using perltidy, see the tutorial 
http://perltidy.sourceforge.net/tutorial.html

Suggestions for improving this document are welcome and may be sent to
perltidy at users.sourceforge.net
