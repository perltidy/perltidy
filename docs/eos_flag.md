# The --encode-output-strings Flag

## What's this about?

This is about making perltidy work better as a filter when called from other
Perl scripts.  For example, in the following example a reference to a
string `$source` is passed to perltidy and it stores the formatted result in
a string named `$output`:

```
    my $output;
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        argv        => $argv,
    );
```

For a filtering operation we expect to be able to directly compare the source and output strings, like this:

```
    if ($output eq $source) {print "Your formatting is unchanged\n";}
```

Or we might want to optionally skip the filtering step and pass the source
directly on to the next stage of processing.  This requires that the source
and output strings be in the same storage mode.

The problem is that in versions of perltidy prior to 2022 there was a use case
where this was not possible.  That case was when perltidy received an encoded
source and decoded it from a utf8 but did not re-encode it before storing it in
the output string.  So the source string was in a different storage mode than
the output string, and a direct comparison was not meaningful.

This problem is an unintentional result of the historical evolution of perltidy.

The same problem occurs if the destination is an array rather than a string,
so for simplicity we can limit this discussion to string destinations, which
are more common.

## How has the problem been fixed?

A fix was phased in over a couple of steps. The first step was to
introduce a new flag in in version 20220217.  The new flag is
**--encode-output-strings**, or **-eos**.  When this is set, perltidy will fix
the specific problem mentioned above by doing an encoding before returning.
So perltidy will behave well as a filter when **-eos** is set.

To illustrate using this flag in the above example, we could write

```
    my $output;

    # Make perltidy versions after 2022 behave well as a filter
    $argv .= " -eos" if ($Perl::Tidy::VERSION > 20220101);
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        argv        => $argv,
    );
```

With this modification we can make a meaningful direct comparison of `$source` and `$output`. The test on `$VERSION` allows this to work with older versions of perltidy (which would not recognize the flag -eos).

In the second step, introduced in version 20220613, the new **-eos** flag became the default.

## What can go wrong?

The first step is safe because the default behavior is unchanged.  But the programmer has to set **-eos** for the corrected behavior to go into effect.

The second step, in which **-eos** becomes the default, will have no effect on programs which do not require perltidy to decode strings, and it will make some programs start processing encoded strings correctly.  But there is also the possibility of  **double encoding** of the output, or in other words data corruption, in some cases.  This could happen if an existing program already has already worked around this issue by encoding the output that it receives back from perltidy.  It is important to check for this.

To see how common this problem might be, all programs on CPAN which use Perl::Tidy as a filter were examined.  Of a total of 45 programs located, one was identified for which the change in default would definitely cause double encoding, and in one program it was difficult to determine.  It looked like the rest of the programs would either not be affected or would start working correctly when processing encoded files.  Here is a slightly revised version of the code for the program which would have a problem with double encoding with the new default:

```
    my $output;
    Perl::Tidy::perltidy(
        source      => \$self->{data},
        destination => \$output,
        stderr      => \$perltidy_err,
        errorfile   => \$perltidy_err,
        logfile     => \$perltidy_log,
        argv        => $perltidy_argv,
    );

    # convert source back to raw
    encode_utf8 $output;
```

The problem is in the last line where encoding is done after the call to perltidy.  This
encoding operation was added by the module author to compensate for the lack of an
encoding step with the old default behavior.  But if we run this code with
**-eos**, which is the planned new default, encoding will also be done by perltidy before
it returns, with the result that `$output` gets double encoding.  This must be avoided. Here
is one way to modify the above code to avoid double encoding:

```
    my $has_eos_flag = $Perl::Tidy::VERSION > 20220101;
    $perltidy_argv .= ' -eos' if $has_eos_flag;

    Perl::Tidy::perltidy(
        source      => \$self->{data},
        destination => \$output,
        stderr      => \$perltidy_err,
        errorfile   => \$perltidy_err,
        logfile     => \$perltidy_log,
        argv        => $perltidy_argv,
    );

    # convert source back to raw if perltidy did not do it
    encode_utf8($output) if ( !$has_eos_flag );
```

A related problem is if an update of Perl::Tidy is made without also updating
a corrected version of a module such as the above.  To help reduce the chance
that this will occur the Change Log for perltidy will contain a warning to be
alert for the double encoding problem, and how to reset the default if
necessary.  This is also the reason for waiting some time before the second step was made.

If double encoding does appear to be occurring with the change in the default for some program which calls Perl::Tidy, then a quick emergency fix can be made by the program user by setting **-neos** to revert to the old default.  A better fix can eventually be made by the program author by removing the second encoding using a technique such as illustrated above.

## Summary

A new flag, **-eos**, has been added to cause Perl::Tidy to behave better as a
filter when called from other Perl scripts.  This flag is the default setting
in the current release.

## Reference

This flag was originally introduced to fix a problem with the widely-used **tidyall** program (see https://github.com/houseabsolute/perl-code-tidyall/issues/84).

## Appendix, a little closer look

A string of text (here, a Perl script) can be stored by Perl in one of two
internal storage formats.  For simplicity let's call them 'B' mode (for 'Byte')
mode and 'C' mode (for 'Character').  The 'B' mode can be used for text with
single-byte characters or for storing an encoded string of multi-byte
characters.  The 'C' mode is needed for actually working with multi-byte
characters.  Thinking of a Perl script as a single long string of text, we can
look at the mode of the text of a source script as it is processed by perltidy
at three points:

    - when it enters as a source
    - at the intermediate stage as it is processed
    - when it is leaves to its destination

Since 'C' mode only has meaning within Perl scripts, a rule is that outside of
the realm of Perl the text must be stored in 'B' mode.

The source can only be in 'C' mode if it arrives by a call from another Perl
program, and the destination can only be in 'C' mode if the destination is a
Perl program.  Otherwise, if the destination is a file, or object with a print
method, then it will be assumed to be ending its existence as a Perl string and
will be placed in an end state which is 'B' mode.

Transition from a starting 'B' mode to 'C' mode is done by a decoding operation
according to the user settings. A transition from an intermediate 'C' mode to
an ending 'B' mode is done by an encoding operation.  It never makes sense to
transition from a starting 'C' mode to a 'B' mode, or from an intermediate 'B'
mode to an ending 'C' mode.

Let us make a list of all possible sets of string storage modes to be sure that
all cases are covered.  If each of the three stages list above (entry,
intermediate, and exit) could be in 'B' or 'C' mode then we would have a total
of 2 x 2 x 2 = 8 combinations of states.  Each end point may either be a file
or a string reference. Here is a list of them, with a note indicating which
ones are possible, and when:

    #   modes    when
    1 - B->B->B  always ok: (file or string )->(file or string)
    2 - B->B->C  never (trailing B->C never done)
    3 - B->C->B  ok if destination is a file or -eos is set     [NEW DEFAULT]
    4 - B->C->C  ok if destination is a string and -neos is set [OLD DEFAULT]
    5 - C->B->B  never (leading C->B never done)
    6 - C->B->C  never (leading C->B and trailing B->C never done)
    7 - C->C->B  only for string-to-file
    8 - C->C->C  only for string-to-string

So three of these cases (2, 5, and 6) cannot occur and the other five can
occur.  Of these five possible cases, only four are possible when the
destination is a string:

    1 - B->B->B  ok
    3 - B->C->B  ok if -eos is set  [NEW DEFAULt]
    4 - B->C->C  ok if -neos is set [OLD DEFAULT]
    8 - C->C->C  string-to-string only

The first three of these may start at either a file or a string, and the last one only starts at a string.

From this we can see that, if **-eos** is set, then only cases 1, 3, and 8 can occur.  In that case the starting and ending states have the same storage mode for all routes through perltidy which end at a string.  This verifies that perltidy will work well as a filter in all cases when the **-eos** flag is set, which is the goal here.

The last case in this table, the C->C->C route, corresponds to programs which
pass decoded strings to perltidy. This is a common usage pattern, and this
route is not influenced by the **-eos** flag setting, since it only applies to
strings that have been decoded by perltidy itself.

Incidentally, the full name of the flag, **--encode-output-strings**, is not
the best because it does not describe what happens in this case.  It was
difficult to find a concise name for this flag.  A more correct name would have
been **--encode-output-strings-that-you-decode**, but that is rather long.  A
more intuitive name for the flag might have been **--be-a-nice-filter**.

Finally, note that case 7 in the full table, the C->C->B route, is an unusual
but possible situation involving a source string being sent directly to a file.
It is the only situation in which perltidy does an encoding without having done
a corresponding previous decoding.
