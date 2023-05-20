# An update to the basic Perl::TIdy continuation indentation model

The next release after Perl::Tidy versison 20230309 has several changes in the
basic method for computing "continuation indentation".  The changes mainly
apply to some unusual situations, and most programs will remain unchanged.
This note explains what the changes are and why they are needed.

To briefly review, the indentation of a line is the sum of two parts: 
(**1**) structural indentation, and (**2**) continuation indentation.

These are occasionally called primary and secondary indentation.

**Structural indentation** is introduced by opening container tokens
**{**, **(**, or **[**.  Default structural indentation is 4 characters by
default but can be changed with the **-i=n** parameter. The total structural
indentation is easily determined by keeping a stack of the opening tokens which
contain a given line.
 
**Continuation indentation** is introduced whenever a statement in a code block
is so long that it must have an internal line break before its terminating
semicolon, or when a long list item is broken before its terminating comma.  In
this case, any continuation lines receive the additional continuation
indentation to contrast them with the start of subsequent statements or list
items.  In complex situations, such as a mixture of ternary statements and list
items which contain function calls and logical expressions, it can be
difficult to design rules for the continuation indentation.

The default continuation indentation is 2 characters but this can be changed
with the **-ci=n** parameter.

The original coding for continuation indentation operated in the Tokenizer
module during a single pass through the file, and this placed some limits on
what it could do.  This update moves this coding downstream into the Formatter
module, where the entire file is accessible with complete data structures, and
this allows the following improvements to be made.

## Block comment indentation changes before closing braces, brackets and parens

The indentation of one-line comments, also called block comments, which
appear near the end of a containing structure are now independent of the
existance of any optional trailing comma or semicolon.

To illustrate the issue, consider the following example where the last
statement is terminated with a semicolon:

```
if ( $name =~ m/^$AccentTokens$/ ) {
    print OUTFILE "$pre_space\n", '\i ', $name, '{';

    # block comment
}
```

The comment has the basic indentation of the block but now continuation
indentation.  But if the terminal semicolon is removed in this line then the
old default formatting would add continuation indentation to the comment:

```
if ( $name =~ m/^$AccentTokens$/ ) {
    print OUTFILE "$pre_space\n", '\i ', $name, '{'

      # block comment
}
```

The same issue occurs in lists regarding the existance of an optional trailing
comma.  This change is undesirable and has been removed in the current version.
So the new indentation for this example is independent of the final semicolon:

```
if ( $name =~ m/^$AccentTokens$/ ) {
    print OUTFILE "$pre_space\n", '\i ', $name, '{'

    # block comment
}
```

## Block comment indentation changes in ternary statements

Another change is that the indentation of block comments within ternary
statements is improved. For example, here is the old default formatting of
a complex ternary with lots of comments:

```
    # a) under an interactive shell?
    my $rl_avail = ( !$term->isa('CPANPLUS::Shell::_Faked') )

      # b) do we have a tty terminal?
      ? ( -t STDIN )

      # c) should we enable the term?
          ? ( !$self->__is_bad_terminal($term) )

        # d) external modules available?
              ? ( $term->ReadLine ne "Term::ReadLine::Stub" )

              # a+b+c+d => "Smart" terminal
                  ? loc("enabled")

                      # a+b+c => "Stub" terminal
                  : loc("available (try 'i Term::ReadLine::Perl')")

                      # a+b => "Bad" terminal
              : loc("disabled")

                # a => "Dumb" terminal
          : loc("suppressed")

          # none    => "Faked" terminal
      : loc("suppressed in batch mode");
```

The comment indentation is rather poor. Here is the new improved formatting:

```
    # a) under an interactive shell?
    my $rl_avail = ( !$term->isa('CPANPLUS::Shell::_Faked') )

      # b) do we have a tty terminal?
      ? ( -t STDIN )

          # c) should we enable the term?
          ? ( !$self->__is_bad_terminal($term) )

              # d) external modules available?
              ? ( $term->ReadLine ne "Term::ReadLine::Stub" )

                  # a+b+c+d => "Smart" terminal
                  ? loc("enabled")

                  # a+b+c => "Stub" terminal
                  : loc("available (try 'i Term::ReadLine::Perl')")

              # a+b => "Bad" terminal
              : loc("disabled")

          # a => "Dumb" terminal
          : loc("suppressed")

      # none    => "Faked" terminal
      : loc("suppressed in batch mode");
```

## Improved indentation for some nested welds.

An issue has been fixed involving cases where the **--weld-nested**, or **-wn**
parameter was used on comma-separated lists of items at block level (paren-less
lists).  For example, here is the old default formatting with the **-wn**
parameter.

```
is_deeply $fixer->fix( {
    demo => { nl => 'Tuin der lusten', en => 'The Garden of Earthly Delights' }
} ),
  {
    demo => { NL => 'TUIN DER LUSTEN', en => 'The Garden of Earthly Delights' },
    titles => ['The Garden of Earthly Delights']
  },
  'specific testing';
```

The closing '} )' is not indented correctly.  The new default formatting is 

```
is_deeply $fixer->fix( {
    demo => { nl => 'Tuin der lusten', en => 'The Garden of Earthly Delights' }
  } ),
  {
    demo => { NL => 'TUIN DER LUSTEN', en => 'The Garden of Earthly Delights' },
    titles => ['The Garden of Earthly Delights']
  },
  'specific testing';
```

## Problems with excess continuation indentation

A very rare problem has been fixed in which excess indentation could occur.
This is illustrated in the following example which is run with **-ci=4** to
emphasize the problem:

```
                ( $foo, $dayC[$cnt], $foo ) = split /;/,
                    $slist[
                    &UnixDate(
                        &ParseDate(
                                  $week_name[ $cnt - 1 ]
                                . " week "
                                . $uweek . " "
                                . $this_year
                        ),
                        "%j"
                    ) - 1
                    ];
```

The problem is that the lines with leading dots have twice the
amount of indentation that they should.  The new version fixes this:

```
                ( $foo, $dayC[$cnt], $foo ) = split /;/,
                    $slist[
                    &UnixDate(
                        &ParseDate(
                              $week_name[ $cnt - 1 ]
                            . " week "
                            . $uweek . " "
                            . $this_year
                        ),
                        "%j"
                    ) - 1
                    ];
```

Here is another example, also run with **-ci=4** for emphasis:

```
    $a
        ? $b
            ? $c
                ? $d
                        ? $e
                        : $f
                : $g
            : $h
        : print "hello\n";
```

Note how $e and $f have excess indenation. The update version is:

```
    $a
        ? $b
            ? $c
                ? $d
                    ? $e
                    : $f
                : $g
            : $h
        : print "hello\n";
```


