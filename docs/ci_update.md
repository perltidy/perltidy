# An update to the basic Perl::Tidy continuation indentation model

Perl::Tidy version 20230701 has several changes in the basic method for
computing "continuation indentation".  This has been on the TODO list
for a long time. The changes mainly apply to some unusual situations,
and most programs will remain unchanged. This note explains what the changes
are and why they are needed.

To briefly review, the indentation of a line is the sum of two parts:
(1) **structural indentation**, and (2) **continuation indentation**.

These are occasionally called primary and secondary indentation.

**Structural indentation** is introduced by opening container tokens
**{**, **(**, or **[**.  Default structural indentation is 4 characters by
default but can be changed with the **-i=n** parameter. The total structural
indentation is easily determined by keeping a stack of the opening tokens which
contain a given line.

**Continuation indentation** is introduced to help show structure in multi-line
statements, list items, and logical expressions. The first line of such long
lines usually starts with the basic structural indentation. Subsequent
lines are given the additional continuation indentation to emphasize that
they are a continuation of the statement.

The default continuation indentation is 2 characters but this can be changed
with the **-ci=n** parameter.

Previously, computation of continuation indentation was done in the
initial pass through a file, and this placed some limits on what it could do.
This computation has been moved downstream in the processing pipeline, where the
entire file is accessible with full data structures, and this allows several
improvements to be made.  These mainly involve (1) the continuation
indentation assigned to comments in unusual circumstances, or (2) the
indentation of complex ternary expressions, or (3) the indentation of
chains of ``sort/map/grep`` blocks.  Some examples are as follows.

## Block comment indentation changes before closing braces, brackets and parens

The indentation of one-line comments, also called block comments, which
appear near the end of a containing structure are now independent of the
existence of any optional trailing comma or semicolon.

To illustrate the issue, consider the following example, in which the last
statement is not terminated with a semicolon.  Previously, the subsequent
comments would have continuation indentation, since the statement is not
terminated:

```
BEGIN {

    $my_hash{'word1'} = 1;
    $my_hash{'word2'} = 1

      # comment
      # ...
}
```

In the updated version, since the final semicolon is optional, the
comments do not have the continuation indentation:

```
BEGIN {

    $my_hash{'word1'} = 1;
    $my_hash{'word2'} = 1

    # comment
    # ...
}
```

This makes the comments have the same indentation as if there were a
terminal semicolon. This update keeps large blocks of comments from shifting
when an optional trailing semicolon or comma is added or removed.

## Closing brace indentation changes

A related issue which has been fixed is illustrated with the following
example which shows the previous formatting:

```
        if ( $term->ReadLine eq "Term::ReadLine::Gnu" ) {
            my $attribs = $term->Attribs;
            $attribs->{attempted_completion_function} = sub {
                &CPAN::Complete::gnu_cpl;
              }

              # comment
              # comment
        }
```

Here again, an optional terminal semicolon is missing after the closing sub
brace, and there are some comments before the closing ``if`` block brace. The
previous logic had a limited look-ahead ability, and in this case the
continuation indentation of the closing sub brace was not removed.

The updated logic fixes this problem:

```
        if ( $term->ReadLine eq "Term::ReadLine::Gnu" ) {
            my $attribs = $term->Attribs;
            $attribs->{attempted_completion_function} = sub {
                &CPAN::Complete::gnu_cpl;
            }

            # comment
            # comment
        }
```

## Block comment indentation changes in ternary statements

Another change is that the indentation of block comments within ternary
statements is improved. These can be difficult to format. For example,
here is the old default formatting of a complex ternary with lots of comments:

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

The comment indentation is very poor here. Here is the new formatting:

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

The closing '} )' is missing some continuation indentation.  The new default
formatting is

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

Note how $e and $f have excess indentation. The updated version is:

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

## Some problems with indentation in ternary expressions

The continuation indentation in some complex ternary statements has been
improved.  For example, in the following old formatting the lines beginning
with ``&&`` lack continuation indentation:

```
    if (
          $file eq '-'      ? open(PHONES, '<&STDIN')
        : $file =~ /\.Z$/   ? open(PHONES, "zcat '$file' 2>/dev/null |")
        : $file =~ /\.pgp$/ ? $usepgp
        && length($ENV{PGPPASS})
        && open(PHONES, "pgp -fd <'$file' |")
        : open(PHONES, "< $file\0")
       )
    {
    }
```

The updated version adds indentation to these lines to help indicate that
they are a continuation of the previous line.

```
    if (
          $file eq '-'      ? open(PHONES, '<&STDIN')
        : $file =~ /\.Z$/   ? open(PHONES, "zcat '$file' 2>/dev/null |")
        : $file =~ /\.pgp$/ ? $usepgp
          && length($ENV{PGPPASS})
          && open(PHONES, "pgp -fd <'$file' |")
        : open(PHONES, "< $file\0")
       )
    {
    }
```

## Some improved indentation of filter block chains

The lines of an isolated chain of ``sort/map/grep`` blocks are normally all
given the same indentation.  For example

```
            @new_in_dir = (
                grep { not $seen{$_} }
                map  { $dir . "/" . $_ }
                grep { not ignore_file($_) }
                grep { not $skip{$_} } readdir(D)
            );
```

Previously, there were a a number of situations where this could not be
achieved. As an example, if the above example had side comments then the
formatting would be

```
            @new_in_dir = (
                grep   { not $seen{$_} }          # files not yet processed
                  map  { $dir . "/" . $_ }        # map from file to dir/file
                  grep { not ignore_file($_) }    # ignore files in cvsignore
                  grep { not $skip{$_} }          # skip files to be ignored
                  readdir(D)
            );
```

The first line now has a different indentation from the rest, and this is
undesirable because ideally indentation should be independent of the existence
of side comments.  The new version handles this correctly:

```
            @new_in_dir = (
                grep { not $seen{$_} }          # files not yet processed
                map  { $dir . "/" . $_ }        # map from file to dir/file
                grep { not ignore_file($_) }    # ignore files in cvsignore
                grep { not $skip{$_} }          # skip files to be ignored
                  readdir(D)
            );
```

A related change is that some undesirable alignments across changes in
continuation indentation have been removed.  For example, here is an
example of this issue as previously formatted:

```
        print $fh map { $_->[0] }
          sort        { $a->[1] cmp $b->[1] || $a->[0] cmp $b->[0] }
          map { my $f = lc $_; $f =~ s/[^a-z0-9\s]//g; [ $_, $f ] } @manifest;
```

The alignment of the ``map`` and ``sort`` braces produces an undesirable
gap. The revised formatting avoids this:

```
        print $fh map { $_->[0] }
          sort { $a->[1] cmp $b->[1] || $a->[0] cmp $b->[0] }
          map { my $f = lc $_; $f =~ s/[^a-z0-9\s]//g; [ $_, $f ] } @manifest;
```
