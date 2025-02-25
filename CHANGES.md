# Perltidy Change Log

## 2025 02 14.02

    - Fig git #32, a tokenization error.

    - A new option --dump-similar-keys will dump hash keys which are
      similar but slightly different to standard output and then exit.
      A related option --warn-similar-keys will report keys with are similar
      to the error output while formatting. Both of these can be controlled
      by parameters which are described in the input manual.

    - A new option --dump-hash-keys will dump all hash keys found by
      perltidy to standard output.

    - The output table for --dump-block-summary has an additional field.
      It is an alternate McCabe complexity count which is the same as
      the previous count except for subs. For subs, the complexity number
      is reduced by the values for any contained anonymous subs.

    - Fix git #181, remove continuation indentation from closing brace
      of an anonymous sub which terminates an input stream.

## 2025 02 14

    - A new option --keep-old-blank-lines-exceptions=s, or --kblx=s,
      allows selected blank lines to be ignored when an input stream
      is read. The parameter s is used to select the blank lines to
      be ignored. This option provides an inverse to other blank line
      parameters. The manual has details. See discussion git #180.

    - A new option --warn-unused-keys, or -wuk, has been added which will
      produce warnings for unused hash keys during formatting. This is similar
      to --dump-unused-keys, which just exits and does not continue formatting.
      A related new parameter --warn-unused-keys-cutoff=N provides control
      over a filter which prevents warnings for keys which appear to be
      members of large hashes used to communicate with external packages.
      The manual has details.  See also git #177.

    - A new option --pack-opening-types='->' has been added to provide more
      control over breaks of method call chains.  It tells perltidy not to
      break at every method call when a chain of calls spans multiple lines.
      This was the behavior in versions prior to 20250105. The default
      starting with 20250105 is to break at each call of a method call chain
      which spans multiple lines. The manual has details. See also git #171.

## 2025 01 05

    - If a file consists only of comments, then the starting indentation will
      be guessed from the indentation of the first comment. Previously it would
      be guessed to be zero. Parameter --starting-indentation-level=n can be
      used to specify an indentation and avoid a guess. This issue can
      arise when formatting a block of comments from within an editor.

    - Added missing 'use File::Temp' for -html option. This was causing the
      message: "Undefined subroutine &File::Temp::tempfile called at ..."
      See git #176.

    - A new parameter --dump-unique-keys, or -duk, dumps a list of hash keys
      which appear to be used just once, and do not appear among the quoted
      strings in a file. For example:

         perltidy -duk File.pm >output.txt

      This can help locate misspelled hash keys.

    - Line breaks at long chains of method calls now break at all calls
      with args in parens, as in this example from git #171

        # Old default
        sub bla_p( $value = 42 ) {
            return Mojo::Promise->resolve($value)->then( sub { shift() / 2 } )
              ->then( sub { shift() + 6 } )->then( sub { shift() / 2 } )
              ->catch( sub { warn shift } );
        }

        # New default
        sub bla_p( $value = 42 ) {
            return Mojo::Promise->resolve($value)
              ->then( sub { shift() / 2 } )
              ->then( sub { shift() + 6 } )
              ->then( sub { shift() / 2 } )
              ->catch( sub { warn shift } );
        }

    - Parameter --break-at-old-method-breakpoints, or -bom, has been
    updated to insure that it only applies to lines beginning with
    method calls, as intended.  Line breaks for all lines beginning with
    '->', even non-method calls, can be retained by using
    --keep-old-breakpoints-before='->'.

    - Added parameter --multiple-token-tightness=s, or -mutt=s.
    The default value --paren-tightness=1 adds space within the parens
    if, and only if, the container holds multiple tokens.  Some perltidy
    tokens may be rather long, and it can be preferable to also space some of
    them as if they were multiple tokens.  This can be done with this parameter,
    and it applies to parens as well as square brackets and curly braces.
    For example, the default below has no space within the square brackets:

        # perltidy
        my $rlist = [qw( alpha beta gamma )];

    Spaces can be obtained with:

        # perltidy -mutt='q*'
        my $rlist = [ qw( alpha beta gamma ) ];

    The parameter -mutt='q*' means treat qw and similar quote operators as
    multiple tokens.  The manual has details; git #120 has another example.

    - Added parameter --indent-leading-semicolon, -ils; see git #171. When
    this is negated, a line with a leading semicolon does not get the extra
    leading continuation indentation spaces (defined with -ci=n).

    - Space around here doc delimiters follow spacing controls better. For
    example, a space is now added before the closing paren here:

       OLD: (without the here doc):
       push( @script, <<'EOT');

       NEW:
       push( @script, <<'EOT' );

    Also, any spaces between the '<<' and here target are removed (git #174):

       OLD:
       push( @script, <<  'EOT');

       NEW:
       push( @script, <<'EOT' );

    - Added parameter --break-at-trailing-comma-types=s, or -btct=s, where
    s is a string which selects trailing commas.  For example, -btct='f(b'
    places a line break after all bare trailing commas in function calls.
    The manual has details.

    - Fix git #165, strings beginning with v before => gave an incorrect error
    message.

    - The parameter --add-lone-trailing-commas, -altc, is now on by default.
    This will simplify input for trailing comma operations. Use
    --noadd-lone-trailing-commas, or -naltc to turn it off.

    - More edge cases for adding and deleting trailing commas are now handled
    (git #156).

    - A problem has been fixed in which the addition or deletion of trailing
    commas with the -atc or -dtc flags did not occur due to early convergence
    when the -conv flag was set (git #143).

    - Added parameter --qw-as-function, or -qwaf, discussed in git #164.
    When this parameter is set, a qw list which begins with 'qw(' is
    formatted as if it were a function call with call args being a list
    of comma-separated quoted items. For example, given this input:

    @fields = qw( $st_dev	   $st_ino    $st_mode $st_nlink   $st_uid
      $st_gid $st_rdev    $st_size $st_atime   $st_mtime  $st_ctime
      $st_blksize $st_blocks);

    # perltidy -qwaf
    @fields = qw(
        $st_dev   $st_ino   $st_mode  $st_nlink
        $st_uid   $st_gid   $st_rdev  $st_size
        $st_atime $st_mtime $st_ctime $st_blksize
        $st_blocks
    );

## 2024 09 03

    - Add partial support for Syntax::Operator::In and Syntax::Keyword::Match
      (see git #162).

    - Add --timeout-in-seconds=n, or -tos=n.  When the standard input supplies
      the input stream, and the input has not been received within n seconds,
      perltidy will end with a timeout message.  The intention is to catch
      a situation where perltidy is accidentally invoked without a file to
      process and therefore waits for input from the system standard input
      (stdin), which never arrives.  The default is n=10.
      This check can be turned off with -tos=0.

    - Add parameter --closing-side-comment-exclusion-list=string, or
      -cscxl=string, where string is a list of block types to exclude
      for closing side comment operations.  Also, closing side comments
      now work for anonymous subs if a --closing-side-comment-list (-cscl)
      is not specified, and when 'asub' is requested with -cscl=asub.
      Use -cscxl=asub to prevent this.

    - Include check for unused constants in --dump-unusual-variables and
      --warn-variable-types (new issue type 'c'). Also expand checks to
      cover variables introduced with 'use vars'.

    - Include signature variables in --dump-unusual-variables and
      --warn-variable-types; see git #158.

    - Add logical xor operator ^^ available in perl version 5.40, as
      noted in git #157.

    - Keyword 'state' now has default space before a paren, like 'my'.
      Previously there was no space and no control.  So the default
      is now "state ($x)". This space can be removed with -nsak='state'.

    - Add options --add-lone-trailing-commas, -altc and
      --delete-lone-trailing-commas, -dltc, to provide control over adding
      and deleting the only comma in a list.  See discussion in git #143
      and the updated manual.

    - Add options --dump-mismatched-returns (or -dmr) and
      --warn-mismatched-returns (or -wmr).  These options report function
      calls where the number of values requested may disagree with sub
      return statements.  The -dump version writes the results for a single
      file to standard output and exits:

         perltidy -dmr somefile.pl >results.txt

      The -warn version formats as normal but reports any issues as warnings in
      the error file:

         perltidy -wmr somefile.pl

      The -warn version may be customized with the following additional
      parameters if necessary to avoid needless warnings:

      --warn-mismatched-return-types=s (or -wmrt=s),
      --warn-mismatched-return-exclusion-list=s (or -wmrxl=s)

      where 's' is a control string. These are explained in the manual.

    - Updates for issue git #151:
      (1) --warn-variable-types=u is now okay if a named file is processed.
      (2) --warn-variable-exclusion-list=s now allows leading and/or
      trailing * on variable names to allow a wildcard match. For example
      -wvxl='*_unused' is okay and would match $var1_unused and $var2_unused.
      (3) --dump-unusual-variables now outputs the filename.

    - A option was added to filter unimplemented parameters from perltidy
      configuration files, suggested in git #146.  It works like this: if
      a line in the config file begins with three dashes followed by a
      parameter name (rather than two dashes), then the line will be removed
      if the parameter is unknown. Otherwise, a dash will be removed to make
      the line valid.

    - Parameters --dump-mismatched-args (or -dma) and
      --warn-mismatched-args (or -wma) have been updated to catch more
      arg count issues.

    - Fixed issue git #143, extend -add-trailing-commas to apply to a list
      with just a fat comma.

    - The minimum perl version is 5.8.1. Previously it was 5.8.0, which was
      not correct because of the use of utf8::is_utf8.

    - Fixed issue git #142, test failure installing on perl versions before
      version 5.10.  The error caused the new parameter
      -interbracket-arrow-style=s not to work. Except for this limitation,
      Version 20240511 will work on older perl versions.

## 2024 05 11

    - The option --valign-signed-numbers, or -vsn is now the default. It
      was introduced in the previous release has been found to significantly
      improve the overall appearance of columns of signed and unsigned
      numbers.  See the previous Change Log entry for an example.
      This will change the formatting in scripts with columns
      of vertically aligned signed and unsigned numbers.
      Use -nvsn to turn this option off and avoid this change.

    - The option --delete-repeated-commas is now the default.

      It makes the following checks and changes:
      - Repeated commas like ',,' are removed with a warning
      - Repeated fat commas like '=> =>' are removed with a warning
      - The combination '=>,' produces a warning but is not changed
      These warnings are only output if --warning-output, or -w, is set.

      Use --nodelete-repeated-commas, or -ndrc, to retain repeated commas.

    - Previously, a line break was always made before a concatenated
      quoted string, such as "\n", if the previous line had a greater
      starting indentation. An exception is now made for a short concatenated
      terminal quote.  This keeps code a little more compact. For example:

    # basic rule: break before "\n" here because '$name' has more indentation:
    my $html = $this->SUPER::genObject( $query, $bindNode, $field . ":$var",
        $name, "remove", "UNCHECKED" )
      . "\n";

    # modified rule: make an exception for a short terminal quote like "\n"
    my $html = $this->SUPER::genObject( $query, $bindNode, $field . ":$var",
        $name, "remove", "UNCHECKED" ) . "\n";

    - The operator ``**=`` now has spaces on both sides by default. Previously,
      there was no space on the left.  This change makes its spacing the same
      as all other assignment operators. The previous behavior can be obtained
      with the parameter setting -nwls='**='.

    - The option --file-size-order, or -fso is now the default. When
      perltidy is given a list of multiple filenames to process, they
      are sorted by size and processed in order of increasing size.
      This can significantly reduce memory usage by Perl.  This
      option has always been used in testing, where typically several
      jobs each operating on thousands of filenames are running at the
      same time and competing for system resources.  If this option
      is not wanted for some reason, it can be deactivated with -nfso.

    - In the option --dump-block-summary, the number of sub arguments indicated
      for each sub now includes any leading object variable passed with
      an arrow-operator call.  Previously the count would have been decreased
      by one in this case. This change is needed for compatibility with future
      updates.

    - Fix issue git #138 involving -xlp (--extended-line-up-parentheses).
      When multiple-line quotes and regexes have long secondary lines, these
      line lengths could influencing some spacing and indentation, but they
      should not have since perltidy has no control over their indentation.
      This has been fixed. This will mainly influence code which uses -xlp
      and has long multi-line quotes.

    - Add option --minimize-continuation-indentation, -mci (see git #137).
      This flag allows perltidy to remove continuation indentation in some
      special cases where it is not really unnecessary. For a simple example,
      the default formatting for the following snippet is:

        # perltidy -nmci
        $self->blurt( "Error: No INPUT definition for type '$type', typekind '"
              . $type->xstype
              . "' found" );

      The second and third lines are one level deep in a container, and
      are also statement continuations, so they get indented by the sum
      of the -i value and the -ci value.  If this flag is set, the
      indentation is reduced by -ci spaces, giving

        # perltidy -mci
        $self->blurt( "Error: No INPUT definition for type '$type', typekind '"
            . $type->xstype
            . "' found" );

      This situation is relatively rare except in code which has long
      quoted strings and the -nolq flag is also set.  This flag is currently
      off by default, but it could become the default in a future version.

    - Add options --dump-mismatched-args (or -dma) and
      --warn-mismatched-args (or -wma).  These options look
      for and report instances where the number of args expected by a
      sub appear to differ from the number passed to the sub.  The -dump
      version writes the results for a single file to standard output
      and exits:

         perltidy -dma somefile.pl >results.txt

      The -warn version formats as normal but reports any issues as warnings in
      the error file:

         perltidy -wma somefile.pl

      The -warn version may be customized with the following additional parameters
      if necessary to avoid needless warnings:

      --warn-mismatched-arg-types=s (or -wmat=s),
      --warn-mismatched-arg-exclusion-list=s (or -wmaxl=s), and
      --warn-mismatched-arg-undercount-cutoff=n (or -wmauc=n).
      --warn-mismatched-arg-overcount-cutoff=n (or -wmaoc=n).

      These are explained in the manual.

    - Add option --valign-wide-equals, or -vwe, for issue git #135.
      Setting this parameter causes the following assignment operators

         = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=

      to be aligned vertically with the ending = all aligned. For example,
      here is the default formatting of a snippet of code:

            $str .= SPACE x $total_pad_count;
            $str_len += $total_pad_count;
            $total_pad_count = 0;
            $str .= $rfields->[$j];
            $str_len += $rfield_lengths->[$j];

      And here is the same code formatted with -vwe:

            # perltidy -vwe
            $str             .= SPACE x $total_pad_count;
            $str_len         += $total_pad_count;
            $total_pad_count  = 0;
            $str             .= $rfields->[$j];
            $str_len         += $rfield_lengths->[$j];

      This option currently is off by default to avoid changing existing
      formatting.

    - Added control --delete-interbracket-arrows, or -dia, to delete optional
      hash ref and array ref arrows between brackets as in the following
      expression (see git #131)

        return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};

        # perltidy -dia gives:
        return $self->{'commandline'}{'arg_list'}[0][0]{'hostgroups'};

      Added the opposite control --aia-interbracket-arrows, or -aia, to
      add arrows. So applied to the previous line the arrows are restored:

        # perltidy -aia
        return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};

     The manual describes additional controls for adding and deleting
     just selected interbracket arrows.

## 2024 02 02

    - Added --valign-signed-numbers, or -vsn. This improves the appearance
      of columns of numbers by aligning leading algebraic signs.  For example:

            # perltidy -vsn
            my $xyz_shield = [
                [ -0.060,  -0.060,  0. ],
                [  0.060,  -0.060,  0. ],
                [  0.060,   0.060,  0. ],
                [ -0.060,   0.060,  0. ],
                [ -0.0925, -0.0925, 0.092 ],
                [  0.0925, -0.0925, 0.092 ],
                [  0.0925,  0.0925, 0.092 ],
                [ -0.0925,  0.0925, 0.092 ],
            ];

            # perltidy -nvsn (current DEFAULT)
            my $xyz_shield = [
                [ -0.060,  -0.060,  0. ],
                [ 0.060,   -0.060,  0. ],
                [ 0.060,   0.060,   0. ],
                [ -0.060,  0.060,   0. ],
                [ -0.0925, -0.0925, 0.092 ],
                [ 0.0925,  -0.0925, 0.092 ],
                [ 0.0925,  0.0925,  0.092 ],
                [ -0.0925, 0.0925,  0.092 ],
            ];

      This new option works well but is currently OFF to allow more testing
      and fine-tuning. It is expected to be activated in a future release.

    - Added --dump-mixed-call-parens (-dmcp ) which will dump a list of
      operators which are sometimes followed by parens and sometimes not.
      This can be useful for developing a uniform style for selected operators.
      Issue git #128. For example

         perltidy -dmcp somefile.pl >out.txt

      produces lines like this, where the first number is the count of
      uses with parens, and the second number is the count without parens.

        k:caller:2:1
        k:chomp:3:4
        k:close:7:4

    - Added --want-call-parens=s (-wcp=s) and --nowant-call-parens=s (-nwcp=s)
      options which will warn of paren uses which do not match a selected
      style. The manual has details. But for example,

        perltidy -wcp='&' somefile.pl

      will format as normal but warn if any user subs are called without parens.

    - Added --dump-unusual-variables (-duv) option to dump a list of
      variables with certain properties of interest.  For example

         perltidy -duv somefile.pl >vars.txt

      produces a file with lines which look something like

         1778:u: my $input_file
         6089:r: my $j: reused - see line 6076

      The values on the line which are separated by colons are:

        line number   - the number of the line of the input file
        issue         - a single letter indicating the issue, see below
        variable name - the name of the variable, preceded by a keyword
        note          - an optional note referring to another line

      The issue is indicated by a letter which may be one of:

        r: reused variable name
        s: sigil change but reused bareword
        p: lexical variable with scope in multiple packages
        u: unused variable

      This is very useful for locating problem areas and bugs in code.

    - Added a related flag --warn-variable-types=string (-wvt=string) option
      to warn if certain types of variables are found in a script. The types
      are a space-separated string which may include 'r', 's', and 'p' but
      not 'u'. For example

        perltidy -wvt='r s' somefile.pl

      will check for and warn if any variabls of type 'r', or 's' are seen,
      but not 'p'. All possible checks may be indicated with a '*' or '1':

        perltidy -wvt='*' somefile.pl

      The manual has further details.

    - All parameters taking integer values are now checked for
      out-of-range values before processing starts. When a maximum or
      maximum range is exceeded, the new default behavior is to write a
      warning message, reset the value to its default setting, and continue.
      This default behavior can be changed with the new parameter
      --integer-range-check=n, or -irc=n, as follows:

        n=0  skip check completely (for stress-testing perltidy only)
        n=1  reset bad values to defaults but do not issue a warning
        n=2  reset bad values to defaults and issue a warning [DEFAULT]
        n=3  stop immediately if any values are out of bounds

      The settings n=0 and n=1 are mainly useful for testing purposes.

    - The --dump-block-summary (-dbs) option now includes the number of sub
      args in the 'type' column. For example, 'sub(9)' indicates a sub
      with 9 args.  Subs whose arg count cannot easily be determined are
      indicated as 'sub(*)'. The count does not include a leading '$self'
      or '$class' arg.

    - Added flag --space-signature-paren=n, or -ssp=n (issue git #125).
      This flag works the same as the existing flag --space-prototype-paren=n
      except that it applies to the space before the opening paren of a sub
      signature instead of a sub prototype.  Previously, there was no control
      over this (a space always occurred). For example, given the following
      line:

        sub circle( $xc, $yc, $rad );

      The following results can now be obtained, according to the value of n:

        sub circle( $xc, $yc, $rad );   # n=0 [no space]
        sub circle( $xc, $yc, $rad );   # n=1 [default; same as input]
        sub circle ( $xc, $yc, $rad );  # n=2 [space]

      The spacing in previous versions of perltidy corresponded to n=2 (always
      a space). The new default value, n=1, will produce a space if and only
      if there was a space in the input text.

    - The --dump-block-summary option can report an if-elsif-elsif-.. chain
      as a single line item with the notation -dbt='elsif3', for example,
      where the '3' is an integer which specifies the minimum number of elsif
      blocks required for a chain to be reported. The manual has details.

    - Fix problem c269, in which the new -ame parameter could incorrectly
      emit an else block when two elsif blocks were separated by a hanging
      side comment (a very rare situation).

    - When braces are detected to be unbalanced, an attempt is made to
      localize the error by comparing the indentation at closing braces
      with their actual nesting levels. This can be useful for files which
      have previously been formatted by perltidy. To illustrate, a test was
      made in which the closing brace at line 30644 was commented out in
      a file with a total of over 62000 lines.  The new error message is

        Final nesting depth of '{'s is 1
        The most recent un-matched '{' is on line 6858
        ...
        Table of nesting level differences at closing braces.
        This might help localize brace errors if the file was previously formatted.
        line:  (brace level) - (level expected from old indentation)
        30643: 0
        30645: 1

      Previously, the error file only indicated that the error in this case
      was somewhere after line 6858, so the new table is very helpful. Closing
      brace indentation is checked because it is unambiguous and can be done
      very efficiently.

    - The -DEBUG option no longer automatically also writes a .LOG file.
      Use --show-options if the .LOG file is needed.

    - The run time of this version with all new options in use is no greater
      than that of the previous version thanks to optimization work.

## 2023 09 12

    - Fix for git #124: remove a syntax error check which could cause
      an incorrect error message when List::Gather::gather was used.

## 2023 09 09

    - Added new parameters -wme, or --warn-missing-else, and -ame,
      or --add-missing else.  The parameter -wme tells perltidy to issue
      a warning if an if-elsif-... chain does not end in an else block.
      The parameter -ame tells perltidy to insert an else block at the
      end of such a chain if there is none.

      For example, given the following snippet:

        if    ( $level == 3 ) { $val = $global{'section'} }
        elsif ( $level == 2 ) { $val = $global{'chapter'} }

      # perltidy -ame
        if    ( $level == 3 ) { $val = $global{'section'} }
        elsif ( $level == 2 ) { $val = $global{'chapter'} }
        else {
            ##FIXME - added with perltidy -ame
        }

      The resulting code should be carefully reviewed, and the ##FIXME comment
      should be updated as appropriate.  The text of the ##FIXME comment can be
      changed with parameter -amec=s, where 's' is the comment to mark the new
      else block. The man pages have more details.

    - The syntax of the parameter --use-feature=class, or -uf=class, which
      new in the previous release, has been changed slightly for clarity.
      The default behavior, which occurs if this flag is not entered, is
      to automatically try to handle both old and new uses of the keywords
      'class', 'method', 'field', and 'ADJUST'.
      To force these keywords to only follow the -use feature 'class' syntax,
      enter --use-feature=class.
      To force perltidy to ignore the -use feature 'class' syntax, enter
      --use-feature=noclass.

    - Issue git #122. Added parameter -lrt=n1:n2, or --line-range-tidy=n1:n2
      to limit tidy operations to a limited line range.  Line numbers start
      with 1. This parameter is mainly of interest to editing programs which
      drive perltidy. The man pages have details.

    - Some fairly rare instances of incorrect spacing have been fixed.  The
      problem was that the tokenizer being overly conservative in marking
      terms as possible filehandles or indirect objects. This causes the space
      after the possible filehandle to be frozen to its input value in order not
      to introduce an error in case Perl had to guess.  The problem was fixed
      by having the tokenizer look ahead for operators which can eliminate the
      uncertainty.  To illustrate, in the following line the term ``$d`` was
      previously marked as a possible filehandle, so no space was added after it.

          print $d== 1 ? " [ON]\n" : $d ? " [$d]\n" : "\n";
                  ^

      In the current version, the next token is seen to be an equality, so
      ``$d`` is marked as an ordinary identifier and normal spacing rules
      can apply:

          print $d == 1 ? " [ON]\n" : $d ? " [$d]\n" : "\n";
                  ^

    - This version runs 7 to 10 percent faster than the previous release on
      large files, depending on options and file type. Much of the gain comes
      from streamlined I/O operations.

    - This version was stress-tested for many cpu hours with random
      input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.


## 2023 07 01

    - Issue git #121. Added parameters -xbt, or --extended-block-tightness,
      and -xbtl=s, or --extended-block-tightness-list=s, to allow
      certain small code blocks to have internal spacing controlled by
      -bbt=n rather than -bt=n. The man pages have details.

    - Issue git #118. A warning will be issued if a duplicate format-skipping
      starting marker is seen within a format-skipping section. The same
      applies to duplicate code-skipping starting markers within code-skipping
      sections.

    - Issue git #116. A new flag --valign-if-unless, -viu, was added to
      allow postfix 'unless' terms to align with postfix 'if' terms.  The
      default remains not to do this.

    - Fixed git #115. In the two most recent CPAN releases, when the
      Perl::Tidy module was called with the source pointing to a file,
      but no destination specified, the output went to the standard
      output instead of to a file with extension ``.tdy``, as it should
      have.  This has been fixed.

    - Fixed git #110, add missing documentation for new options
      -cpb and -bfvt=n. These work in version 20230309 but the pod
      documentation was missing and has been added.

    - Fixed an undefined reference message when running with
      --dump-block-summary on a file without any subs or other
      selected block types.

    - Add parameter -ipc, or --ignore-perlcritic-comments.  Perltidy, by
      default, will look for side comments beginning with ``## no critic`` and
      ignore their lengths when making line break decisions, even if the user
      has not set ``-iscl``.  The reason is that an unwanted line break can
      make these special comments ineffective in controlling ``perlcritic``.
      The parameter -ipc can be set if, for some reason, this is not wanted.

    - Some minor issues with continuation indentation have been fixed.
      Most scripts will remain unchanged.  The main change is that block
      comments which occur just before a closing brace, bracket or paren
      now have an indentation which is independent of the existence of
      an optional comma or semicolon.  Previously, adding or deleting
      an optional trailing comma could cause their indentation to jump.
      Also, indentation of comments within ternary statements has been
      improved. For additional details see:

      https://github.com/perltidy/perltidy/blob/master/docs/ci_update.md

    - This version was stress-tested for many cpu hours with random
      input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.

    - This version runs several percent faster than the previous release
      on large files.

## 2023 03 09

    - No significant bugs have been found since the last release to CPAN.
      Several minor issues have been fixed, and some new parameters have been
      added, as follows:

    - Added parameter --one-line-block-exclusion-list=s, or -olbxl=s, where
      s is a list of block types which should not automatically be turned
      into one-line blocks.  This implements the issue raised in PR #111.
      The list s may include any of the words 'sort map grep eval', or
      it may be '*' to indicate all of these.  So for example to prevent
      multi-line 'eval' blocks from becoming one-line blocks, the command
      would be -olbxl='eval'.

    - For the -b (--backup-and-modify-in-place) option, the file timestamps
      are changing (git #113, rt#145999).  First, if there are no formatting
      changes to an input file, it will keep its original modification time.
      Second, any backup file will keep its original modification time.  This
      was previously true for --backup-method=move but not for the default
      --backup-method=copy.  The purpose of these changes is to avoid
      triggering Makefile operations when there are no actual file changes.
      If this causes a problem please open an issue for discussion on github.

    - A change was made to the way line breaks are made at the '.'
      operator when the user sets -wba='.' to requests breaks after a '.'
      ( this setting is not recommended because it can be hard to read ).
      The goal of the change is to make switching from breaks before '.'s
      to breaks after '.'s just move the dots from the end of
      lines to the beginning of lines.  For example:

            # default and recommended (--want-break-before='.'):
            $output_rules .=
              (     'class'
                  . $dir
                  . '.stamp: $('
                  . $dir
                  . '_JAVA)' . "\n" . "\t"
                  . '$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) '
                  . '$(JAVACFLAGS) $?' . "\n" . "\t"
                  . 'echo timestamp > class'
                  . $dir
                  . '.stamp'
                  . "\n" );

            # perltidy --want-break-after='.'
            $output_rules .=
              ( 'class' .
                  $dir .
                  '.stamp: $(' .
                  $dir .
                  '_JAVA)' . "\n" . "\t" .
                  '$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) ' .
                  '$(JAVACFLAGS) $?' . "\n" . "\t" .
                  'echo timestamp > class' .
                  $dir .
                  '.stamp' .
                  "\n" );

      For existing code formatted with -wba='.', this may cause some
      changes in the formatting of code with long concatenation chains.

    - Added option --use-feature=class, or -uf=class, for issue rt #145706.
      This adds keywords 'class', 'method', 'field', and 'ADJUST' in support of
      this feature which is being tested for future inclusion in Perl.
      An effort has been made to avoid conflicts with past uses of these
      words, especially 'method' and 'class'. The default setting
      is --use-feature=class. If this causes a conflict, this option can
      be turned off by entering -uf=' '.

      In other words, perltidy should work for both old and new uses of
      these keywords with the default settings, but this flag is available
      if a conflict arises.

    - Added option -bfvt=n, or --brace-follower-vertical-tightness=n,
      for part of issue git #110.  For n=2, this option looks for lines
      which would otherwise be, by default,

      }
        or ..

      and joins them into a single line

      } or ..

      where the or can be one of a number of logical operators or if unless.
      The default is not to do this and can be indicated with n=1.

    - Added option -cpb, or --cuddled-paren-brace, for issue git #110.
      This option will cause perltidy to join two lines which
      otherwise would be, by default,

        )
      {

      into a single line

      ) {

    - Some minor changes to existing formatted output may occur as a result
      of fixing minor formatting issues with edge cases.  This is especially
      true for code which uses the -lp or -xlp styles.

    - Added option -dbs, or --dump-block-summary, to dump summary
      information about code blocks in a file to standard output.
      The basic command is:

          perltidy -dbs somefile.pl >blocks.csv

      Instead of formatting ``somefile.pl``, this dumps the following
      comma-separated items describing its blocks to the standard output:

       filename     - the name of the file
       line         - the line number of the opening brace of this block
       line_count   - the number of lines between opening and closing braces
       code_lines   - the number of lines excluding blanks, comments, and pod
       type         - the block type (sub, for, foreach, ...)
       name         - the block name if applicable (sub name, label, asub name)
       depth        - the nesting depth of the opening block brace
       max_change   - the change in depth to the most deeply nested code block
       block_count  - the total number of code blocks nested in this block
       mccabe_count - the McCabe complexity measure of this code block

      This can be useful for code restructuring. The man page for perltidy
      has more information and describes controls for selecting block types.

    - This version was stress-tested for over 100 cpu hours with random
      input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.

    - This version runs a few percent faster than the previous release on
      large files due to optimizations made with the help of Devel::NYTProf.

## 2022 11 12

    - Fix rt #145095, undef warning in Perl before 5.12. Version 20221112 is
      identical to 2022111 except for this fix for older versions of Perl.

    - No significant bugs have been found since the last release to CPAN.
      Several minor issues have been fixed, and some new parameters have been
      added, as follows:

    - Fixed rare problem with irregular indentation involving --cuddled-else,
      usually also with the combination -xci and -lp.  Reported in rt #144979.

    - Add option --weld-fat-comma (-wfc) for issue git #108. When -wfc
      is set, along with -wn, perltidy is allowed to weld an opening paren
      to an inner opening container when they are separated by a hash key
      and fat comma (=>).  For example:

        # perltidy -wn
        elf->call_method(
            method_name_foo => {
                some_arg1       => $foo,
                some_other_arg3 => $bar->{'baz'},
            }
        );

        # perltidy -wn -wfc
        elf->call_method( method_name_foo => {
            some_arg1       => $foo,
            some_other_arg3 => $bar->{'baz'},
        } );

      This flag is off by default.

    - Fix issue git #106. This fixes some edge cases of formatting with the
      combination -xlp -pt=2, mainly for two-line lists with short function
      names. One indentation space is removed to improve alignment:

        # OLD: perltidy -xlp -pt=2
        is($module->VERSION, $expected,
            "$main_module->VERSION matches $module->VERSION ($expected)");

        # NEW: perltidy -xlp -pt=2
        is($module->VERSION, $expected,
           "$main_module->VERSION matches $module->VERSION ($expected)");

    - Fix for issue git #105, incorrect formatting with 5.36 experimental
      for_list feature.

    - Fix for issue git #103. For parameter -b, or --backup-and-modify-in-place,
      the default backup method has been changed to preserve the inode value
      of the file being formatted.  If this causes a problem, the previous
      method is available and can be used by setting -backup-mode='move', or
      -bm='move'.  The new default corresponds to -bm='copy'.  The difference
      between the two methods is as follows.  For the older method,
      -bm='move', the input file was moved to the backup, and a new file was
      created for the formatted output.  This caused the inode to change.  For
      the new default method, -bm='copy', the input is copied to the backup
      and then the input file is reopened and rewritten. This preserves the
      file inode.  Tests have not produced any problems with this change, but
      before using the --backup-and-modify-in-place parameter please verify
      that it works correctly in your environment and operating system. The
      initial update for this had an error which was caught and fixed
      in git #109.

    - Fix undefined value message when perltidy -D is used (git #104)

    - Fixed an inconsistency in html colors near pointers when -html is used.
      Previously, a '->' at the end of a line got the 'punctuation color', black
      by default but a '->' before an identifier got the color of the following
      identifier. Now all pointers get the same color, which is black by default.
      Also, previously a word following a '->' was given the color of a bareword,
      black by default, but now it is given the color of an identifier.

    - Fixed incorrect indentation of any function named 'err'.  This was
      due to some old code from when "use feature 'err'" was valid.

            # OLD:
            my ($curr) = current();
              err (@_);

            # NEW:
            my ($curr) = current();
            err(@_);

    - Added parameter --delete-repeated-commas (-drc) to delete repeated
      commas. This is off by default. For example, given:

            ignoreSpec( $file, "file",, \%spec, \%Rspec );

      # perltidy -drc:
            ignoreSpec( $file, "file", \%spec, \%Rspec );

    - Add continuation indentation to long C-style 'for' terms; i.e.

            # OLD
            for (
                $j = $i - $shell ;
                $j >= 0
                && ++$ncomp
                && $array->[$j] gt $array->[ $j + $shell ] ;
                $j -= $shell
              )

            # NEW
            for (
                $j = $i - $shell ;
                $j >= 0
                  && ++$ncomp
                  && $array->[$j] gt $array->[ $j + $shell ] ;
                $j -= $shell
              )

      This will change some existing formatting with very long 'for' terms.

    - The following new parameters are available for manipulating
      trailing commas of lists. They are described in the manual.

           --want-trailing-commas=s, -wtc=s
           --add-trailing-commas,    -atc
           --delete-trailing-commas, -dtc
           --delete-weld-interfering-commas, -dwic

    - Files with errors due to missing, extra or misplaced parens, braces,
      or square brackets are now written back out verbatim, without any
      attempt at formatting.

    - This version runs 10 to 15 percent faster than the previous
      release on large files due to optimizations made with the help of
      Devel::NYTProf.

    - This version was stress-tested for over 200 cpu hours with random
      input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.

## 2022 06 13

    - No significant bugs have been found since the last release but users
      of programs which call the Perl::Tidy module should note the first
      item below, which changes a default setting.  The main change to
      existing formatting is the second item below, which adds vertical
      alignment to 'use' statements.

    - The flag --encode-output-strings, or -eos, is now set 'on' by default.
      This has no effect on the use of the 'perltidy' binary script, but could
      change the behavior of some programs which use the Perl::Tidy module on
      files encoded in UTF-8.  If any problems are noticed, an emergency fix
      can be made by reverting to the old default by setting -neos.  For
      an explanation of why this change needs to be made see:

      https://github.com/perltidy/perltidy/issues/92

      https://github.com/perltidy/perltidy/blob/master/docs/eos_flag.md

    - Added vertical alignment for qw quotes and empty parens in 'use'
      statements (see issue #git 93).  This new alignment is 'on' by default
      and will change formatting as shown below. If this is not wanted it can
      be turned off with the parameter -vxl='q' (--valign-exclusion-list='q').

        # old default, or -vxl='q'
        use Getopt::Long qw(GetOptions);
        use Fcntl qw(O_RDONLY O_WRONLY O_EXCL O_CREAT);
        use Symbol qw(gensym);
        use Exporter ();

        # new default
        use Getopt::Long qw(GetOptions);
        use Fcntl        qw(O_RDONLY O_WRONLY O_EXCL O_CREAT);
        use Symbol       qw(gensym);
        use Exporter     ();

    - The parameter -kbb (--keep-break-before) now ignores a request to break
      before an opening token, such as '('.  Likewise, -kba (--keep-break-after)
      now ignores a request to break after a closing token, such as ')'. This
      change was made to avoid a rare instability discovered in random testing.

    - Previously, if a -dsc command was used to delete all side comments,
      then any special side comments for controlling non-indenting braces got
      deleted too. Now, these control side comments are retained when -dsc is
      set unless a -nnib (--nonon-indenting-braces) flag is also set to
      deactivate them.

    - This version runs about 10 percent faster on large files than the previous
      release due to optimizations made with the help of Devel::NYTProf.  Much
      of the gain came from faster processing of blank tokens and comments.

    - This version of perltidy was stress-tested for many cpu hours with
      random input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.

## 2022 02 17

    - A new flag, --encode-output-strings, or -eos, has been added to resolve
      issue git #83. This issue involves the interface between Perl::Tidy and
      calling programs, and Code::TidyAll (tidyall) in particular.  The problem
      is that perltidy by default returns decoded character strings, but
      tidyall expects encoded strings.  This flag provides a fix for that.

      So, tidyall users who process encoded (utf8) files should update to this
      version of Perl::Tidy and use -eos for tidyall.  For further info see:

      https://github.com/houseabsolute/perl-code-tidyall/issues/84, and
      https://github.com/perltidy/perltidy/issues/83

      If there are other applications having utf8 problems at the interface
      with Perl::Tidy, this flag probably may need to be set.

    - The default value of the new flag, --encode-output-strings, -eos, is currently
      -neos BUT THIS MAY CHANGE in a future release because the current
      default is inconvenient.  So authors of programs which receive character
      strings back from Perl::Tidy should set this flag, if necessary,
      to avoid any problems when the default changes.  For more information see the
      above links and the Perl::Tidy man pages for example coding.

    - The possible values of the string 's' for the flag '--character-encoding=s'
      have been limited to 'utf8' (or UTF-8), 'none', or 'guess'.  Previously an
      arbitrary encoding could also be specified, but as a result of discussions
      regarding git #83 it became clear that this could cause trouble
      since the output encoding was still restricted to UTF-8. Users
      who need to work in other encodings can write a short program calling
      Perl::Tidy with pre- and post-processing to handle encoding/decoding.

    - A new flag --break-after-labels=i, or -bal=i, was added for git #86.  This
      controls line breaks after labels, to provide a uniform style, as follows:

            -bal=0 follows the input line breaks [DEFAULT]
            -bal=1 always break after a label
            -bal=2 never break after a label

      For example:

          # perltidy -bal=1
          INIT:
            {
                $xx = 1.234;
            }

          # perltidy -bal=2
          INIT: {
                $xx = 1.234;
            }

    - Fix issue git #82, an error handling something like ${bareword} in a
      possible indirect object location. Perl allows this, now perltidy does too.

    - The flags -kbb=s or --keep-old-breakpoints-before=s, and its counterpart
      -kba=s or --keep-old-breakpoints-after=s have expanded functionality
      for the container tokens: { [ ( } ] ).  The updated man pages have
      details.

    - Two new flags have been added to provide finer vertical alignment control,
      --valign-exclusion-list=s (-vxl=s) and  --valign-inclusion-list=s (-vil=s).
      This has been requested several times, most recently in git #79, and it
      finally got done.  For example, -vil='=>' means just align on '=>'.

    - A new flag -gal=s, --grep-alias-list=s, has been added as suggested in
      git #77.  This allows code blocks passed to list operator functions to
      be formatted in the same way as a code block passed to grep, map, or sort.
      By default, the following list operators in List::Util are included:

        all any first none notall reduce reductions

      They can be changed with the flag -gaxl=s, -grep-alias-exclusion-list=s

    - A new flag -xlp has been added which can be set to avoid most of the
      limitations of the -lp flag regarding side comments, blank lines, and
      code blocks.  See the man pages for more info. This fixes git #64 and git #74.
      The older -lp flag still works.

    - A new flag -lpil=s, --line-up-parentheses-inclusion-list=s, has been added
      as an alternative to -lpxl=s, --line-up-parentheses-exclusion-list=s.
      It supplies equivalent information but is much easier to describe and use.
      It works for both the older -lp version and the newer -xlp.

    - The coding for the older -lp flag has been updated to avoid some problems
      and limitations.  The new coding allows the -lp indentation style to
      mix smoothly with the standard indentation in a single file.  Some problems
      where -lp and -xci flags were not working well together have been fixed, such
      as happened in issue rt140025.  As a result of these updates some minor
      changes in existing code using the -lp style may occur.

    - This version of perltidy was stress-tested for many cpu hours with
      random input parameters. No failures to converge, internal fault checks,
      undefined variable references or other irregularities were seen.

    - Numerous minor fixes have been made, mostly very rare formatting
      instabilities found in random testing.

## 2021 10 29

    - No significant bugs have been found since the last release, but several
      minor issues have been fixed.  Vertical alignment has been improved for
      lists of call args which are not contained within parens (next item).

    - Vertical alignment of function calls without parens has been improved with
      the goal of making vertical alignment essentially the same with or
      without parens around the call args.  Some examples:

        # OLD
        mkTextConfig $c, $x, $y, -anchor => 'se', $color;
        mkTextConfig $c, $x + 30, $y, -anchor => 's',  $color;
        mkTextConfig $c, $x + 60, $y, -anchor => 'sw', $color;
        mkTextConfig $c, $x, $y + 30, -anchor => 'e', $color;

        # NEW
        mkTextConfig $c, $x,      $y,      -anchor => 'se', $color;
        mkTextConfig $c, $x + 30, $y,      -anchor => 's',  $color;
        mkTextConfig $c, $x + 60, $y,      -anchor => 'sw', $color;
        mkTextConfig $c, $x,      $y + 30, -anchor => 'e',  $color;

        # OLD
        is id_2obj($id), undef, "unregistered object not retrieved";
        is scalar keys %$ob_reg, 0, "object registry empty";
        is register($obj), $obj, "object returned by register";
        is scalar keys %$ob_reg, 1, "object registry nonempty";
        is id_2obj($id), $obj, "registered object retrieved";

        # NEW
        is id_2obj($id),         undef, "unregistered object not retrieved";
        is scalar keys %$ob_reg, 0,     "object registry empty";
        is register($obj),       $obj,  "object returned by register";
        is scalar keys %$ob_reg, 1,     "object registry nonempty";
        is id_2obj($id),         $obj,  "registered object retrieved";

      This will cause some changes in alignment, hopefully for the better,
      particularly in test code which often uses numerous parenless function
      calls with functions like 'ok', 'is', 'is_deeply', ....

    - Two new parameters were added to control the block types to which the
      -bl (--opening-brace-on-new-line) flag applies.  The new parameters are
      -block-left-list=s, or -bll=s, and --block-left-exclusion-list=s,
      or -blxl=s.  Previously the -bl flag was 'hardwired' to apply to
      nearly all blocks. The default values of the new parameters
      retain the the old default behavior but allow it to be changed.

    - The default behavior of the -bli (-brace-left-and-indent) flag has changed
      slightly.  Previously, if you set -bli, then the -bl flag would also
      automatically be set.  Consequently, block types which were not included
      in the default list for -bli would get -bl formatting.  This is no longer done,
      and these two styles are now controlled independently.  The manual describes
      the controls.  If you want to recover the exact previous default behavior of
      the -bli then add the -bl flag.

    - A partial fix was made for issue for git #74. The -lp formatting style was
      being lost when a one-line anonymous sub was followed by a closing brace.

    - Fixed issue git #73, in which the -nfpva flag was not working correctly.
      Some unwanted vertical alignments of spaced function perens
      were being made.

    - Updated the man pages to clarify the flags -valign and -novalign
      for turning vertical alignment on and off (issue git #72).
      Added parameters -vc -vsc -vbc for separately turning off vertical
      alignment of code, side comments and block comments.

    - Fixed issue git #68, where a blank line following a closing code-skipping
      comment, '#>>V', could be lost.

    - This version runs 10 to 15 percent faster on large files than the
      previous release due to optimizations made with the help of NYTProf.

    - This version of perltidy was stress-tested for many cpu hours with
      random input parameters. No instabilities,  internal fault checks,
      undefined variable references or other irregularities were seen.

    - Numerous minor fixes have been made, mostly very rare formatting instabilities
      found in random testing. An effort has been made to minimize changes to
      existing formatting that these fixes produce, but occasional changes
      may occur. Many of these updates are listed at:

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2021 07 17

    - This release is being made mainly because of the next item, in which an
      error message about an uninitialized value error message could be produced
      in certain cases when format-skipping is used.  The error message was
      annoying but harmless to formatting.

    - Fixed an undefined variable message, see git #67. When a format skipping
      comment '#<<' is placed before the first line of code in a script, a
      message 'Use of uninitialized value $Ktoken_vars in numeric ...' can
      occur.

    - A warning will no longer be given if a script has an opening code-skipping
      comment '#<<V' which is not terminated with a closing comment '#>>V'. This
      makes code-skipping and format-skipping behave in a similar way: an
      opening comment without a corresponding closing comment will cause
      the rest of a file to be skipped.  If there is a question about which lines
      are skipped, a .LOG file can be produced with the -g flag and it will have
      this information.

    - Removed the limit on -ci=n when -xci is set, reference: rt #136415.
      This update removes a limit in the previous two versions in which the
      value of -ci=n was limited to the value of -i=n when -xci was set.
      This limit had been placed to avoid some formatting instabilities,
      but recent coding improvements allow the limit to be removed.

    - The -wn and -bbxx=n flags were not working together correctly. This has
      been fixed.

    - This version may produce occasional differences in formatting compared to
      previous versions, mainly for lines which are near the specified line
      length limit.  This is due to ongoing efforts to eliminate edge cases of
      formatting instability.

    - Numerous minor fixes have been made. A complete list is at:

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2021 06 25

    - This release adds several new requested parameters.  No significant bugs have
      been found since the last release, but a number of minor problems have been
      corrected.

    - Added a new option '--code-skipping', requested in git #65, in which code
      between comment lines '#<<V' and '#>>V' is passed verbatim to the output
      stream without error checking.  It is similar to --format-skipping
      but there is no error checking of the skipped code. This can be useful for
      skipping past code which employs an extended syntax.

    - Added a new option for closing paren placement, -vtc=3, requested in rt #136417.

    - Added flag -atnl, --add-terminal-newline, to help issue git #58.
      This flag tells perltidy to terminate the last line of the output stream
      with a newline character, regardless of whether or not the input stream
      was terminated with a newline character.  This is the default.
      If this flag is negated, with -natnl, then perltidy will add a terminal
      newline character to the the output stream only if the input
      stream is terminated with a newline.

    - Some nested structures formatted with the -lp indentation option may have
      some changes in indentation.  This is due to updates which were made to
      prevent formatting instability when line lengths are limited by the maximum line
      length. Most scripts will not be affected. If this causes unwanted formatting
      changes, try increasing the --maximum-line-length by a few characters.

    - Numerous minor fixes have been made. A complete list is at:

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2021 04 02

    - This release fixes several non-critical bugs which have been found since the last
    release.  An effort has been made to keep existing formatting unchanged.

    - Fixed issue git #57 regarding uninitialized warning flag.

    - Added experimental flag -lpxl=s requested in issue git #56 to provide some
    control over which containers get -lp indentation.

    - Fixed issue git #55 regarding lack of coordination of the --break-before-xxx
    flags and the --line-up-parens flag.

    - Fixed issue git #54 regarding irregular application of the --break-before-paren
    and similar --break-before-xxx flags, in which lists without commas were not
    being formatted according to these flags.

    - Fixed issue git #53. A flag was added to turn off alignment of spaced function
    parens.  If the --space-function-paren, -sfp flag is set, a side-effect is that the
    spaced function parens may get vertically aligned.  This can be undesirable,
    so a new parameter '--function-paren-vertical-alignment', or '-fpva', has been
    added to turn this vertical alignment off. The default is '-fpva', so that
    existing formatting is not changed.  Use '-nfpva' to turn off unwanted
    vertical alignment.  To illustrate the possibilities:

        # perltidy [default]
        myfun( $aaa, $b, $cc );
        mylongfun( $a, $b, $c );

        # perltidy -sfp
        myfun     ( $aaa, $b, $cc );
        mylongfun ( $a, $b, $c );

        # perltidy -sfp -nfpva
        myfun ( $aaa, $b, $cc );
        mylongfun ( $a, $b, $c );

    - Fixed issue git #51, a closing qw bare paren was not being outdented when
    the -nodelete-old-newlines flag was set.

    - Fixed numerous edge cases involving unusual parameter combinations which
      could cause alternating output states.  Most scripts will not be
      changed by these fixes.

    - A more complete list of updates is at

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2021 01 11

    - Fixed issue git #49, -se breaks warnings exit status behavior.
    The exit status flag was not always being set when the -se flag was set.

    - Some improvements have been made in the method for aligning side comments.
    One of the problems that was fixed is that there was a tendency for side comment
    placement to drift to the right in long scripts.  Programs with side comments
    may have a few changes.

    - Some improvements have been made in formatting qw quoted lists.  This
    fixes issue git #51, in which closing qw pattern delimiters not always
    following the settings specified by the --closing-token-indentation=n settings.
    Now qw closing delimiters ')', '}' and ']' follow these flags, and the
    delimiter '>' follows the flag for ')'.  Other qw pattern delimiters remain
    indented as the are now.  This change will cause some small formatting changes
    in some existing programs.

    - Another change involving qw lists is that they get full indentation,
    rather than just continuation indentation, if

         (1) the closing delimiter is one of } ) ] > and is on a separate line,
         (2) the opening delimiter  (i.e. 'qw{' ) is also on a separate line, and
         (3) the -xci flag (--extended-continuation-indentation) is set.

    This improves formatting when qw lists are contained in other lists. For example,

            # OLD: perltidy
            foreach $color (
                qw(
                AntiqueWhite3 Bisque1 Bisque2 Bisque3 Bisque4
                SlateBlue3 RoyalBlue1 SteelBlue2 DeepSkyBlue3
                ),
                qw(
                LightBlue1 DarkSlateGray1 Aquamarine2 DarkSeaGreen2
                SeaGreen1 Yellow1 IndianRed1 IndianRed2 Tan1 Tan4
                )
              )

            # NEW, perltidy -xci
            foreach $color (
                qw(
                    AntiqueWhite3 Bisque1 Bisque2 Bisque3 Bisque4
                    SlateBlue3 RoyalBlue1 SteelBlue2 DeepSkyBlue3
                ),
                qw(
                    LightBlue1 DarkSlateGray1 Aquamarine2 DarkSeaGreen2
                    SeaGreen1 Yellow1 IndianRed1 IndianRed2 Tan1 Tan4
                )
              )

    - Some minor improvements have been made to the rules for formatting
    some edge vertical alignment cases, usually involving two dissimilar lines.

    - A more complete list of updates is at

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2020 12 07

    - Fixed issue git #47, incorrect welding of anonymous subs.
      An incorrect weld format was being made when the --weld-nested-containers option
      (-wn) was used in to format a function which returns a list of anonymous subs.
      For example, the following snippet was incorrectly being welded.

    $promises[$i]->then(
        sub { $all->resolve(@_); () },
        sub {
            $results->[$i] = [@_];
            $all->reject(@$results) if --$remaining <= 0;
            return ();
        }
    );

    This was due to an error introduced in v20201201 related to parsing sub
    signatures.  Reformatting with the current version will fix the problem.

## 2020 12 01

    - This release is being made primarily to make available a several new formatting
      parameters, in particular -xci, -kbb=s, -kba=s, and -wnxl=s. No significant
      bugs have been found since the previous release, but numerous minor issues have
      been found and fixed as listed below.

    - This version is about 20% faster than the previous version due to optimizations
      made with the help of Devel::NYTProf.

    - Added flag -wnxl=s, --weld-nested-exclusion-list=s, to provide control which containers
      are welded with the --weld-nested-containers parameter.  This is related to issue git #45.

    - Merged pull request git #46 which fixes the docs regarding the -fse flag.

    - Fixed issue git #45, -vtc=n flag was ignored when -wn was set.

    - implement request RT #133649, delete-old-newlines selectively. Two parameters,

      -kbb=s or --keep-old-breakpoints-before=s, and
      -kba=s or --keep-old-breakpoints-after=s

      were added to request that old breakpoints be kept before or after
      selected token types.  For example, -kbb='=>' means that newlines before
      fat commas should be kept.

    - Fix git #44, fix exit status for assert-tidy/untidy.  The exit status was
      always 0 for --assert-tidy if the user had turned off all error messages with
      the -quiet flag.  This has been fixed.

    - Add flag -maxfs=n, --maximum-file-size-mb=n.  This parameter is provided to
      avoid causing system problems by accidentally attempting to format an
      extremely large data file. The default is n=10.  The command to increase
      the limit to 20 MB for example would be  -mfs=20.  This only applies to
      files specified by filename on the command line.

    - Skip formatting if there are too many indentation level errors.  This is
      controlled with -maxle=n, --maximum-level-errors=n.  This means that if
      the ending indentation differs from the starting indentation by more than
      n levels, the file will be output verbatim. The default is n=1.
      To skip this check, set n=-1 or set n to a large number.

    - A related new flag, --maximum-unexpected-errors=n, or -maxue=n, is available
      but is off by default.

    - Add flag -xci, --extended-continuation-indentation, regarding issue git #28
      This flag causes continuation indentation to "extend" deeper into structures.
      Since this is a fairly new flag, the default is -nxci to avoid disturbing
      existing formatting.  BUT you will probably see some improved formatting
      in complex data structures by setting this flag if you currently use -ci=n
      and -i=n with the same value of 'n' (as is the case if you use -pbp,
      --perl-best-practices, where n=4).

    - Fix issue git #42, clarify how --break-at-old-logical-breakpoints works.
      The man page was updated to note that it does not cause all logical breakpoints
      to be replicated in the output file.

    - Fix issue git #41, typo in manual regarding -fsb.

    - Fix issue git #40: when using the -bli option, a closing brace followed by
      a semicolon was not being indented.  This applies to braces which require
      semicolons, such as a 'do' block.

    - Added 'state' as a keyword.

    - A better test for convergence has been added. When iterations are requested,
      the new test will stop after the first pass if no changes in line break
      locations are made.  Previously, file checksums were used and required at least two
      passes to verify convergence unless no formatting changes were made.  With the new test,
      only a single pass is needed when formatting changes are limited to adjustments of
      indentation and whitespace on the lines of code.  Extensive testing has been made to
      verify the correctness of the new convergence test.

    - Line breaks are now automatically placed after 'use overload' to
      improve formatting when there are numerous overloaded operators.  For
      example

        use overload
          '+' => sub {
          ...

    - A number of minor problems with parsing signatures and prototypes have
      been corrected, particularly multi-line signatures. Some signatures
      had previously been parsed as if they were prototypes, which meant the
      normal spacing rules were not applied.  For example

      OLD:
        sub echo ($message= 'Hello World!' ) {
            ...;
        }

      NEW:
        sub echo ( $message = 'Hello World!' ) {
            ...;
        }

    - Numerous minor issues that the average user would not encounter were found
      and fixed. They can be seen in the more complete list of updates at

           https://github.com/perltidy/perltidy/blob/master/local-docs/BugLog.pod

## 2020 10 01

    - Robustness of perltidy has been significantly improved.  Updating is recommended. Continual
      automated testing runs began about 1 Sep 2020 and numerous issues have been found and fixed.
      Many involve references to uninitialized variables when perltidy is fed random text and random
      control parameters.

    - Added the token '->' to the list of alignment tokens, as suggested in git
      #39, so that it can be vertically aligned if a space is placed before them with -wls='->'.

    - Added parameters -bbhb=n (--break-before-hash-brace=n), -bbsb=n (--break-before-square-bracket=n),
      and -bbp=n (--break-before-paren=n) suggested in git #38.  These provide control over the
      opening container token of a multiple-line list.  Related new parameters -bbhbi=n, -bbsbi=n, -bbpi=n
      control indentation of these tokens.

    - Added keyword 'isa'.

## 2020 09 07

    - Fixed bug git #37, an error when the combination -scbb -csc was used.
      It occurs in perltidy versions 20200110, 20200619, and 20200822.  What happens is
      that when two consecutive lines with isolated closing braces had new side
      comments generated by the -csc parameter, a separating newline was missing.
      The resulting script will not then run, but worse, if it is reformatted with
      the same parameters then closing side comments could be overwritten and data
      lost.

      This problem was found during automated random testing.  The parameter
      -scbb is rarely used, which is probably why this has not been reported.  Please
      upgrade your version.

    - Added parameter --non-indenting-braces, or -nib, which prevents
      code from indenting one level if it follows an opening brace marked
      with a special side comment, '#<<<'.  For example,

                    { #<<<   a closure to contain lexical vars

                    my $var;  # this line does not indent

                    }

                    # this line cannot 'see' $var;

      This is on by default.  If your code happens to have some
      opening braces followed by '#<<<', and you
      don't want this, you can use -nnib to deactivate it.

    - Side comment locations reset at a line ending in a level 0 open
      block, such as when a new multi-line sub begins.  This is intended to
      help keep side comments from drifting to far to the right.

## 2020 08 22

    - Fix RT #133166, encoding not set for -st.  Also reported as RT #133171
      and git #35.

      This is a significant bug in version 20200616 which can corrupt data if
      perltidy is run as a filter on encoded text.
**Please upgrade**

    - Fix issue RT #133161, perltidy -html was not working on pod

    - Fix issue git #33, allow control of space after '->'

    - Vertical alignment has been improved. Numerous minor issues have
      been fixed.

    - Formatting with the -lp option is improved.

    - Fixed issue git #32, misparse of bare 'ref' in ternary

    - When --assert-tidy is used and triggers an error, the first difference
      between input and output files is shown in the error output. This is
      a partial response to issue git #30.

## 2020 06 19

    - Added support for Switch::Plain syntax, issue git #31.

    - Fixed minor problem where trailing 'unless' clauses were not
      getting vertically aligned.

    - Added a parameter --logical-padding or -lop to allow logical padding
      to be turned off.  Requested by git #29. This flag is on by default.
      The man pages have examples.

    - Added a parameter -kpit=n to control spaces inside of parens following
      certain keywords, requested in git#26. This flag is off by default.

    - Added fix for git#25, improve vertical alignment for long lists with
      varying numbers of items per line.

    - calls to the module Perl::Tidy can now capture any output produced
      by a debug flag or one of the 'tee' flags through the new 'debugfile' and
      'teefile' call parameters.  These output streams are rarely used but
      they are now treated the same as any 'logfile' stream.

    - add option --break-at-old-semicolon-breakpoints', -bos, requested
      in RT#131644.  This flag will keep lines beginning with a semicolon.

    - Added --use-unicode-gcstring to control use of Unicode::GCString for
      evaluating character widths of encoded data.  The default is
      not to use this (--nouse-unicode-gcstring). If this flag is set,
      perltidy will look for Unicode::GCString and, if found, will use it
      to evaluate character display widths.  This can improve displayed
      vertical alignment for files with wide characters.  It is a nice
      feature but it is off by default to avoid conflicting formatting
      when there are multiple developers.  Perltidy installation does not
      require Unicode::GCString, so users wanting to use this feature need
      set this flag and also to install Unicode::GCString separately.

    - Added --character-encoding=guess or -guess to have perltidy guess
      if a file (or other input stream) is encoded as -utf8 or some
      other single-byte encoding. This is useful when processing a mixture
      of file types, such as utf8 and latin-1.

      Please Note: The default encoding has been set to be 'guess'
      instead of 'none'. This seems like the best default, since
      it allows perltidy work properly with both
      utf8 files and older latin-1 files.  The guess mode uses Encode::Guess,
      which is included in standard perl distributions, and only tries to
      guess if a file is utf8 or not, never any other encoding.  If the guess is
      utf8, and if the file successfully decodes as utf8, then it the encoding
      is assumed to be utf8.  Otherwise, no encoding is assumed.
      If you do not want to use this new default guess mode, or have a
      problem with it, you can set --character-encoding=none (the previous
      default) or --character-encoding=utf8 (if you deal with utf8 files).

    - Specific encodings of input files other than utf8 may now be given, for
      example --character-encoding=euc-jp.

    - Fix for git#22, Preserve function signature on a single line. An
      unwanted line break was being introduced when a closing signature paren
      followed a closing do brace.

    - Fix RT#132059, the -dac parameter was not working and caused an error exit

    - When -utf8 is used, any error output is encoded as utf8

    - Fix for git#19, adjust line break around an 'xor'

    - Fix for git#18, added warning for missing comma before unknown bare word.

## 2020 01 10

    - This release adds a flag to control the feature RT#130394 (allow short nested blocks)
      introduced in the previous release.  Unfortunately that feature breaks
      RPerl installations, so a control flag has been introduced and that feature is now
      off by default.  The flag is:

      --one-line-block-nesting=n, or -olbn=n, where n is an integer as follows:

      -olbn=0 break nested one-line blocks into multiple lines [new DEFAULT]
      -olbn=1 stable; keep existing nested-one line blocks intact [previous DEFAULT]

      For example, consider this input line:

        foreach (@list) { if ($_ eq $asked_for) { last } ++$found }

      The new default behavior (-olbn=0), and behavior prior to version 20191203, is to break it into multiple lines:

        foreach (@list) {
            if ( $_ eq $asked_for ) { last }
            ++$found;
        }

      To keep nested one-line blocks such as this on a single line you can add the parameter -olbn=1.

    - Fixed issue RT#131288: parse error for un-prototyped constant function without parenthesized
      call parameters followed by ternary.

    - Fixed issue RT#131360, installation documentation.  Added a note that the binary
      'perltidy' comes with the Perl::Tidy module. They can both normally be installed with
      'cpanm Perl::Tidy'


## 2019 12 03

    - Fixed issue RT#131115: -bli option not working correctly.
      Closing braces were not indented in some cases due to a glitch
      introduced in version 20181120.

    - Fixed issue RT#130394: Allow short nested blocks.  Given the following

        $factorial = sub { reduce { $a * $b } 1 .. 11 };

      Previous versions would always break the sub block because it
      contains another block (the reduce block).  The fix keeps
      short one-line blocks such as this intact.

    - Implement issue RT#130640: Allow different subroutine keywords.
      Added a flag --sub-alias-list=s or -sal=s, where s is a string with
      one or more aliases for 'sub', separated by spaces or commas.
      For example,

        perltidy -sal='method fun'

      will cause the perltidy to treat the words 'method' and 'fun' to be
      treated the same as if they were 'sub'.

    - Added flag --space-prototype-paren=i, or -spp=i, to control spacing
      before the opening paren of a prototype, where i=0, 1, or 2:
      i=0 no space
      i=1 follow input [current and default]
      i=2 always space

      Previously, perltidy always followed the input.
      For example, given the following input

         sub usage();

      The result will be:
        sub usage();    # i=0 [no space]
        sub usage();    # i=1 [default; follows input]
        sub usage ();   # i=2 [space]

    - Fixed issue git#16, minor vertical alignment issue.

    - Fixed issue git#10, minor conflict of -wn and -ce

    - Improved some vertical alignments involving two lines.


## 2019 09 15

    - fixed issue RT#130344: false warning "operator in print statement"
      for "use lib".

    - fixed issue RT#130304: standard error output should include filename.
      When perltidy error messages are directed to the standard error output
      with -se or --standard-error-output, the message lines now have a prefix
      'filename:' for clarification in case multiple files
      are processed, where 'filename' is the name of the input file.  If
      input is from the standard input the displayed filename is '<stdin>',
      and if it is from a data structure then displayed filename
      is '<source_stream>'.

    - implement issue RT#130425: check mode.  A new flag '--assert-tidy'
      will cause an error message if the output script is not identical to
      the input script. For completeness, the opposite flag '--assert-untidy'
      has also been added.  The next item, RT#130297, insures that the script
      will exit with a non-zero exit flag if the assertion fails.

    - fixed issue RT#130297; the perltidy script now exits with a nonzero exit
      status if it wrote to the standard error output. Previously only fatal
      run errors produced a non-zero exit flag. Now, even non-fatal messages
      requested with the -w flag will cause a non-zero exit flag.  The exit
      flag now has these values:

         0 = no errors
         1 = perltidy could not run to completion due to errors
         2 = perltidy ran to completion with error messages

    - added warning message for RT#130008, which warns of conflicting input
      parameters -iob and -bom or -boc.

    - fixed RT#129850; concerning a space between a closing block brace and
      opening bracket or brace, as occurs before the '[' in this line:

       my @addunix = map { File::Spec::Unix->catfile( @ROOT, @$_ ) } ['b'];

      Formerly, any space was removed. Now it is optional, and the output will
      follow the input.

    - fixed issue git#13, needless trailing whitespace in error message

    - fixed issue git#9: if the -ce (--cuddled-else) flag is used,
      do not try to form new one line blocks for a block type
      specified with -cbl, particularly map, sort, grep

    - iteration speedup for unchanged code.  Previously, when iterations were
      requested, at least two formatting passes were made. Now just a single pass
      is made if the formatted code is identical to the input code.

    - some improved vertical alignments

## 2019 06 01

    - rt #128477: Prevent inconsistent owner/group and setuid/setgid bits.
      In the -b (--backup-and-modify-in-place) mode, an attempt is made to set ownership
      of the output file equal to the input file, if they differ.
      In all cases, if the final output file ownership differs from input file, any setuid/setgid bits are cleared.

    - Added option -bom  (--break-at-old-method-breakpoints) by
      merrillymeredith which preserves breakpoints of method chains. Modified to also handle a cuddled call style.

    - Merged patch to fix Windows EOL translation error with UTF-8 written by
      Ron Ivy. This update prevents automatic conversion to 'DOS' CRLF line
      endings.  Also, Windows system testing at the appveyor site is working again.

    - RT #128280, added flag --one-line-block-semicolons=n (-olbs=n) 
      to control semicolons in one-line blocks.  The values of n are:
        n=0 means no semicolons termininating simple one-line blocks
        n=1 means stable; do not change from input file [DEFAULT and current]
        n=2 means always add semicolons in one-line blocks
      The current behavior corresponds to the default n=1.

    - RT #128216, Minor update to prevent inserting unwanted blank line at
      indentation level change.  This should not change existing scripts.

    - RT #81852: Improved indentation when quoted word (qw) lists are 
      nested within other containers using the --weld-nested (-wn) flag.
      The example given previously (below) is now closer to what it would
      be with a simple list instead of qw:

      # perltidy -wn
      use_all_ok( qw{
          PPI
          PPI::Tokenizer
          PPI::Lexer
          PPI::Dumper
          PPI::Find
          PPI::Normal
          PPI::Util
          PPI::Cache
      } );

    - RT#12764, introduced new feature allowing placement of blanks around
      sequences of selected keywords. This can be activated with the -kgb* 
      series of parameters described in the manual.

    - Rewrote vertical algnment module.  It is better at finding
      patterns in complex code. For example,

	OLD:
           /^-std$/ && do { $std       = 1;     next; };
           /^--$/   && do { @link_args = @argv; last; };
           /^-I(.*)/ && do { $path = $1 || shift @argv; next; };

	NEW:
           /^-std$/  && do { $std       = 1;                 next; };
           /^--$/    && do { @link_args = @argv;             last; };
           /^-I(.*)/ && do { $path      = $1 || shift @argv; next; };

    - Add repository URLs to META files 

    - RT #118553, "leave only one newline at end of file". This option was not 
      added because of undesirable side effects, but a new filter script
      was added which can do this, "examples/delete_ending_blank_lines.pl".

## 2018 11 20

    - fix RT#127736 Perl-Tidy-20181119 has the EXE_FILES entry commented out in
      Makefile.PL so it doesn't install the perltidy script or its manpage.


## 2018 11 19

    - Removed test case 'filter_example.t' which was causing a failure on a
      Windows installation for unknown reasons, possibly due to an unexpected
      perltidyrc being read by the test script.  Added VERSION numbers to all
      new modules.

## 2018 11 17

    - Fixed RT #126965, in which a ternary operator was misparsed if immediately
      following a function call without arguments, such as: 
        my $restrict_customer = shift ? 1 : 0;

    - Fixed RT #125012: bug in -mangle --delete-all-comments
      A needed blank space before bareword tokens was being removed when comments 
      were deleted

    - Fixed RT #81852: Stacked containers and quoting operators. Quoted words
      (qw) delimited by container tokens ('{', '[', '(', '<') are now included in
      the --weld-nested (-wn) flag:

          # perltidy -wn
          use_all_ok( qw{
                PPI
                PPI::Tokenizer
                PPI::Lexer
                PPI::Dumper
                PPI::Find
                PPI::Normal
                PPI::Util
                PPI::Cache
                } );

    - The cuddled-else (-ce) coding was merged with the new cuddled-block (-cb)
      coding.  The change is backward compatible and simplifies input.  
      The --cuddled-block-option=n (-cbo=n) flag now applies to both -ce and -cb 
      formatting.  In fact the -cb flag is just an alias for -ce now.

    - Fixed RT #124594, license text desc. changed from 'GPL-2.0+' to 'gpl_2'

    - Fixed bug in which a warning about a possible code bug was issued in a
      script with brace errors. 

    - added option --notimestamp or -nts to eliminate any time stamps in output 
      files.  This is used to prevent differences in test scripts from causing
      failure at installation. For example, the -cscw option will put a date
      stamp on certain closing side comments. We need to avoid this in order
      to test this feature in an installation test.

    - Fixed bug with the entab option, -et=8, in which the leading space of
      some lines was was not entabbed.  This happened in code which was adjusted
      for vertical alignment and in hanging side comments. Thanks to Glenn.

    - Fixed RT #127633, undesirable line break after return when -baao flag is set

    - Fixed RT #127035, vertical alignment. Vertical alignment has been improved 
      in several ways.  Thanks especially to Michael Wardman and Glenn for sending 
      helpful snippets. 

      - Alignment of the =~ operators has been reactivated.  

          OLD:
          $service_profile =~ s/^\s+|\s+$//g;
          $host_profile =~ s/^\s+|\s+$//g;
      
          NEW:
          $service_profile =~ s/^\s+|\s+$//g;
          $host_profile    =~ s/^\s+|\s+$//g;

      - Alignment of the // operator has been reactivated.  

          OLD:
          is( pop // 7,       7, 'pop // ... works' );
          is( pop() // 7,     0, 'pop() // ... works' );
          is( pop @ARGV // 7, 3, 'pop @array // ... works' );
          
          NEW:
          is( pop       // 7, 7, 'pop // ... works' );
          is( pop()     // 7, 0, 'pop() // ... works' );
          is( pop @ARGV // 7, 3, 'pop @array // ... works' );

      - The rules for alignment of just two lines have been adjusted,
        hopefully to be a little better overall.  In some cases, two 
        lines which were previously unaligned are now aligned, and vice-versa.

          OLD:
          $expect = "1$expect" if $expect =~ /^e/i;
          $p = "1$p" if defined $p and $p =~ /^e/i;
      
          NEW:
          $expect = "1$expect" if $expect =~ /^e/i;
          $p      = "1$p"      if defined $p and $p =~ /^e/i;


    - RT #106493; source code repository location has been added to docs; it is 
         https://github.com/perltidy/perltidy

    - The packaging for this version has changed. The Tidy.pm module is much 
      smaller.  Supporting modules have been split out from it and placed below 
      it in the path Perl/Tidy/*.

    - A number of new installation test cases have been added. Updates are now
      continuously tested at Travis CI against versions back to Perl 5.08.

## 2018 02 20

    - RT #124469, #124494, perltidy often making empty files.  The previous had
      an index error causing it to fail, particularly in version 5.18 of Perl.

      Please avoid version 20180219.

## 2018 02 19

    - RT #79947, cuddled-else generalization. A new flag -cb provides
      'cuddled-else' type formatting for an arbitrary type of block chain. The
      default is try-catch-finally, but this can be modified with the 
      parameter -cbl. 

    - Fixed RT #124298: add space after ! operator without breaking !! secret 
      operator

    - RT #123749: numerous minor improvements to the -wn flag were made.  

    - Fixed a problem with convergence tests in which iterations were stopping 
      prematurely. 

    - Here doc targets for <<~ type here-docs may now have leading whitespace.

    - Fixed RT #124354. The '-indent-only' flag was not working correctly in the 
      previous release. A bug in version 20180101 caused extra blank lines 
      to be output.

    - Issue RT #124114. Some improvements were made in vertical alignment
      involving 'fat commas'.

## 2018 01 01

    - Added new flag -wn (--weld-nested-containers) which addresses these issues:
      RT #123749: Problem with promises; 
      RT #119970: opening token stacking strange behavior;
      RT #81853: Can't stack block braces

      This option causes closely nested pairs of opening and closing containers
      to be "welded" together and essentially be formatted as a single unit,
      with just one level of indentation.

      Since this is a new flag it is set to be "off" by default but it has given 
      excellent results in testing. 

      EXAMPLE 1, multiple blocks, default formatting:
          do {
              {
                  next if $x == $y;    # do something here
              }
          } until $x++ > $z;

      perltidy -wn
          do { {
              next if $x == $y;
          } } until $x++ > $z;

       EXAMPLE 2, three levels of wrapped function calls, default formatting:
              p(
                  em(
                      conjug(
                          translate( param('verb') ), param('tense'),
                          param('person')
                      )
                  )
              );

          # perltidy -wn
              p( em( conjug(
                  translate( param('verb') ),
                  param('tense'), param('person')
              ) ) );

          # EXAMPLE 3, chained method calls, default formatting:
          get('http://mojolicious.org')->then(
              sub {
                  my $mojo = shift;
                  say $mojo->res->code;
                  return get('http://metacpan.org');
              }
          )->then(
              sub {
                  my $cpan = shift;
                  say $cpan->res->code;
              }
          )->catch(
              sub {
                  my $err = shift;
                  warn "Something went wrong: $err";
              }
          )->wait;

          # perltidy -wn
          get('http://mojolicious.org')->then( sub {
              my $mojo = shift;
              say $mojo->res->code;
              return get('http://metacpan.org');
          } )->then( sub {
              my $cpan = shift;
              say $cpan->res->code;
          } )->catch( sub {
              my $err = shift;
              warn "Something went wrong: $err";
          } )->wait;


    - Fixed RT #114359: Missparsing of "print $x ** 0.5;

    - Deactivated the --check-syntax flag for better security.  It will be
      ignored if set.  

    - Corrected minimum perl version from 5.004 to 5.008 based on perlver
      report.  The change is required for coding involving wide characters.

    - For certain severe errors, the source file will be copied directly to the
      output without formatting. These include ending in a quote, ending in a
      here doc, and encountering an unidentified character.

## 2017 12 14

    - RT #123749, partial fix.  "Continuation indentation" is removed from lines 
      with leading closing parens which are part of a call chain. 
      For example, the call to pack() is is now outdented to the starting 
      indentation in the following expression:

          # OLD
          $mw->Button(
              -text    => "New Document",
              -command => \&new_document
            )->pack(
              -side   => 'bottom',
              -anchor => 'e'
            );

          # NEW
          $mw->Button(
              -text    => "New Document",
              -command => \&new_document
          )->pack(
              -side   => 'bottom',
              -anchor => 'e'
          );

      This modification improves readability of complex expressions, especially
      when the user uses the same value for continuation indentation (-ci=n) and 
      normal indentation (-i=n).  Perltidy was already programmed to
      do this but a minor bug was preventing it.

    - RT #123774, added flag to control space between a backslash and a single or
      double quote, requested by Robert Rothenberg.  The issue is that lines like

         $str1=\"string1";
         $str2=\'string2';

      confuse syntax highlighters unless a space is left between the backslash and
      the quote.

      The new flag to control this is -sbq=n (--space-backslash-quote=n), 
      where n=0 means no space, n=1 means follow existing code, n=2 means always
      space.  The default is n=1, meaning that a space will be retained if there
      is one in the source code.

    - Fixed RT #123492, support added for indented here doc operator <<~ added 
      in v5.26.  Thanks to Chris Weyl for the report.

    - Fixed docs; --closing-side-comment-list-string should have been just
      --closing-side-comment-list.  Thanks to F.Li.

    - Added patch RT #122030] Perl::Tidy sometimes does not call binmode.
      Thanks to Irilis Aelae.

    - Fixed RT #121959, PERLTIDY doesn't honor the 'three dot' notation for 
      locating a config file using environment variables.  Thanks to John 
      Wittkowski.

    - Minor improvements to formatting, in which some additional vertical
      aligmnemt is done. Thanks to Keith Neargarder.

    - RT #119588.  Vertical alignment is no longer done for // operator.

## 2017 05 21

    - Fixed debian #862667: failure to check for perltidy.ERR deletion can lead 
      to overwriting arbitrary files by symlink attack. Perltidy was continuing
      to write files after an unlink failure.  Thanks to Don Armstrong 
      for a patch.

    - Fixed RT #116344, perltidy fails on certain anonymous hash references:
      in the following code snippet the '?' was misparsed as a pattern 
      delimiter rather than a ternary operator.
          return ref {} ? 1 : 0;

    - Fixed RT #113792: misparsing of a fat comma (=>) right after 
      the __END__ or __DATA__ tokens.  These keywords were getting
      incorrectly quoted by the following => operator.

    - Fixed RT #118558. Custom Getopt::Long configuration breaks parsing 
      of perltidyrc.  Perltidy was resetting the users configuration too soon.

    - Fixed RT #119140, failure to parse double diamond operator.  Code to
      handle this new operator has been added.

    - Fixed RT #120968.  Fixed problem where -enc=utf8 didn't work 
      with --backup-and-modify-in-place. Thanks to Heinz Knutzen for this patch.

    - Fixed minor formatting issue where one-line blocks for subs with signatures 
      were unnecessarily broken

    - RT #32905, patch to fix utf-8 error when output was STDOUT. 

    - RT #79947, improved spacing of try/catch/finally blocks. Thanks to qsimpleq
      for a patch.

    - Fixed #114909, Anonymous subs with signatures and prototypes misparsed as
      broken ternaries, in which a statement such as this was not being parsed
      correctly:
          return sub ( $fh, $out ) : prototype(*$) { ... }

    - Implemented RT #113689, option to introduces spaces after an opening block
      brace and before a closing block brace. Four new optional controls are
      added. The first two define the minimum number of blank lines to be
      inserted 

       -blao=i or --blank-lines-after-opening-block=i
       -blbc=i or --blank-lines-before-closing-block=i

      where i is an integer, the number of lines (the default is 0).  

      The second two define the types of blocks to which the first two apply 

       -blaol=s or --blank-lines-after-opening-block-list=s
       -blbcl=s or --blank-lines-before-closing-block-list=s
      
      where s is a string of possible block keywords (default is just 'sub',
      meaning a named subroutine).

      For more information please see the documentation.

    - The method for specifying block types for certain input parameters has
      been generalized to distinguish between normal named subroutines and
      anonymous subs.  The keyword for normal subroutines remains 'sub', and
      the new keyword for anonymous subs is 'asub'. 

    - Minor documentation changes. The BUGS sections now have a link
      to CPAN where most open bugs and issues can be reviewed and bug reports
      can be submitted.  The information in the AUTHOR and CREDITS sections of
      the man pages have been removed from the man pages to streamline the
      documentation. This information is still in the source code.

## 2016 03 02

    - RT #112534. Corrected a minor problem in which an unwanted newline
      was placed before the closing brace of an anonymous sub with 
      a signature, if it was in a list.  Thanks to Dmytro Zagashev.

    - Corrected a minor problem in which occasional extra indentation was
      given to the closing brace of an anonymous sub in a list when the -lp 
      parameter was set.

## 2016 03 01

     - RT #104427. Added support for signatures.

     - RT #111512.  Changed global warning flag $^W = 1 to use warnings;
       Thanks to Dmytro Zagashev.

     - RT #110297, added support for new regexp modifier /n
       Thanks to Dmytro Zagashev.

     - RT #111519.  The -io (--indent-only) and -dac (--delete-all-comments)
       can now both be used in one pass. Thanks to Dmitry Veltishev.

     - Patch to avoid error message with 'catch' used by TryCatch, as in
          catch($err){
             # do something
          }
       Thanks to Nick Tonkin.

     - RT #32905, UTF-8 coding is now more robust. Thanks to qsimpleq
       and Dmytro for patches.

     - RT #106885. Added string bitwise operators ^. &. |. ~. ^.= &.= |.=
    
     - Fixed RT #107832 and #106492, lack of vertical alignment of two lines
       when -boc flag (break at old commas) is set.  This bug was 
       inadvertently introduced in previous bug fix RT #98902. 

     - Some common extensions to Perl syntax are handled better.
       In particular, the following snippet is now foratted cleanly:

         method deposit( Num $amount) {
             $self->balance( $self->balance + $amount );
         }

       A new flag -xs (--extended-syntax) was added to enable this, and the default
       is to use -xs. 

       In previous versions, and now only when -nxs is set, this snippet of code
       generates the following error message:

       "syntax error at ') {', didn't see one of: case elsif for foreach given if switch unless until when while"

## 2015 08 15

     - Fixed RT# 105484, Invalid warning about 'else' in 'switch' statement.  The
       warning happened if a 'case' statement did not use parens.

     - Fixed RT# 101547, misparse of // caused error message.  Also..

     - Fixed RT# 102371, misparse of // caused unwated space in //=

     - Fixed RT# 100871, "silent failure of HTML Output on Windows". 
       Changed calls to tempfile() from:
         my ( $fh_tmp, $tmpfile ) = tempfile();
       to have the full path name:
         my ( $fh_tmp, $tmpfile ) = File::Temp::tempfile()
       because of problems in the Windows version reported by Dean Pearce.

     - Fixed RT# 99514, calling the perltidy module multiple times with 
       a .perltidyrc file containing the parameter --output-line-ending 
       caused a crash.  This was a glitch in the memoization logic. 

     - Fixed RT#99961, multiple lines inside a cast block caused unwanted
       continuation indentation.  

     - RT# 32905, broken handling of UTF-8 strings. 
       A new flag -utf8 causes perltidy assume UTF-8 encoding for input and 
       output of an io stream.  Thanks to Sebastian Podjasek for a patch.  
       This feature may not work correctly in older versions of Perl. 
       It worked in a linux version 5.10.1 but not in a Windows version 5.8.3 (but
       otherwise perltidy ran correctly).

     - Warning files now report perltidy VERSION. Suggested by John Karr.
    
     - Fixed long flag --nostack-closing-tokens (-nsct has always worked though). 
       This was due to a typo.  This also fixed --nostack-opening-tokens to 
       behave correctly.  Thanks to Rob Dixon.

## 2014 07 11

    - Fixed RT #94902: abbreviation parsing in .perltidyrc files was not
      working for multi-line abbreviations.  Thanks to Eric Fung for 
      supplying a patch. 
    
    - Fixed RT #95708, misparsing of a hash when the first key was a perl
      keyword, causing a semicolon to be incorrectly added.

    - Fixed RT #94338 for-loop in a parenthesized block-map.  A code block within
      parentheses of a map, sort, or grep function was being mistokenized.  In 
      rare cases this could produce in an incorrect error message.  The fix will
      produce some minor formatting changes.  Thanks to Daniel Trizen 
      discovering and documenting this.

    - Fixed RT #94354, excess indentation for stacked tokens.  Thanks to 
      Colin Williams for supplying a patch.

    - Added support for experimental postfix dereferencing notation introduced in
      perl 5.20. RT #96021.

    - Updated documentation to clarify the behavior of the -io flag
      in response to RT #95709.  You can add -noll or -l=0 to prevent 
      long comments from being outdented when -io is used.

    - Added a check to prevent a problem reported in RT #81866, where large
      scripts which had been compressed to a single line could not be formatted
      because of a check for VERSION for MakeMaker. The workaround was to 
      use -nvpl, but this shouldn't be necessary now.

    - Fixed RT #96101; Closing brace of anonymous sub in a list was being
      indented.  For example, the closing brace of the anonymous sub below 
      will now be lined up with the word 'callback'.  This problem 
      occurred if there was no comma after the closing brace of the anonymous sub.
      This update may cause minor changes to formatting of code with lists 
      of anonymous subs, especially TK code.
      
      # OLD
      my @menu_items = (

          #...
          {
              path     => '/_Operate/Transcode and split',
              callback => sub {
                  return 1 if not $self->project_opened;
                  $self->comp('project')->transcode( split => 1 );
                }
          }
      );

      # NEW
      my @menu_items = (

          #...
          {
              path     => '/_Operate/Transcode and split',
              callback => sub {
                  return 1 if not $self->project_opened;
                  $self->comp('project')->transcode( split => 1 );
              }
          }
      );

## 2014 03 28

    - Fixed RT #94190 and debian Bug #742004: perltidy.LOG file left behind.
      Thanks to George Hartzell for debugging this.  The problem was
      caused by the memoization speedup patch in version 20121207.  An
      unwanted flag was being set which caused a LOG to be written if 
      perltidy was called multiple times.

    - New default behavior for LOG files: If the source is from an array or 
      string (through a call to the perltidy module) then a LOG output is only
      possible if a logfile stream is specified.  This is to prevent 
      unexpected perltidy.LOG files. 

    - Fixed debian Bug #740670, insecure temporary file usage.  File::Temp is now
      used to get a temporary file.  Thanks to Don Anderson for a patch.
    
    - Any -b (--backup-and-modify-in-place) flag is silently ignored when a 
      source stream, destination stream, or standard output is used.  
      This is because the -b flag may have been in a .perltidyrc file and 
      warnings break Test::NoWarnings.  Thanks to Marijn Brand. 

## 2013 09 22

    - Fixed RT #88020. --converge was not working with wide characters.

    - Fixed RT #78156. package NAMESPACE VERSION syntax not accepted.

    - First attempt to fix RT #88588.  INDEX END tag change in pod2html breaks 
      perltidy -html. I put in a patch which should work but I don't yet have
      a way of testing it.

## 2013 08 06

    - Fixed RT #87107, spelling

## 2013 08 05

    - Fixed RT #87502, incorrect of parsing of smartmatch before hash brace
    
    - Added feature request RT #87330, trim whitespace after POD.
      The flag -trp (--trim-pod) will trim trailing whitespace from lines of POD

## 2013 07 17

    - Fixed RT #86929, #86930, missing lhs of assignment.

    - Fixed RT #84922, moved pod from Tidy.pm into Tidy.pod

## 2012 12 07

    - The flag -cab=n or --comma-arrow-breakpoints=n has been generalized
      to give better control over breaking open short containers.  The
      possible values are now:

        n=0 break at all commas after =>  
        n=1 stable: break at all commas after => if container is open,
            EXCEPT FOR one-line containers
        n=2 break at all commas after =>, BUT try to form the maximum
            maximum one-line container lengths
        n=3 do not treat commas after => specially at all 
        n=4 break everything: like n=0 but also break a short container with
            a => not followed by a comma
        n=5 stable: like n=1 but ALSO break at open one-line containers (default)

      New values n=4 and n=5 have been added to allow short blocks to be
      broken open.  The new default is n=5, stable.  It should more closely
      follow the breaks in the input file, and previously formatted code
      should remain unchanged.  If this causes problems use -cab=1 to recover 
      the former behavior.  Thanks to Tony Maszeroski for the suggestion.

      To illustrate the need for the new options, if perltidy is given
      the following code, then the old default (-cab=1) was to close up 
      the 'index' container even if it was open in the source.  The new 
      default (-cab=5) will keep it open if it was open in the source.

       our $fancypkg = {
           'ALL' => {
               'index' => {
                   'key' => 'value',
               },
               'alpine' => {
                   'one'   => '+',
                   'two'   => '+',
                   'three' => '+',
               },
           }
       };

    - New debug flag --memoize (-mem).  This version contains a 
      patch supplied by Jonathan Swartz which can significantly speed up
      repeated calls to Perl::Tidy::perltidy in a single process by caching
      the result of parsing the formatting parameters.  A factor of up to 10
      speedup was achieved for masontidy (https://metacpan.org/module/masontidy).
      The memoization patch is on by default but can be deactivated for 
      testing with -nmem (or --no-memoize).

    - New flag -tso (--tight-secret-operators) causes certain perl operator
      sequences (secret operators) to be formatted "tightly" (without spaces).  
      The most common of these are 0 +  and + 0 which become 0+ and +0.  The
      operators currently modified by this flag are: 
           =( )=  0+  +0  ()x!! ~~<>  ,=>
      Suggested by by Philippe Bruhat. See https://metacpan.org/module/perlsecret
      This flag is off by default.
      
    - New flag -vmll (--variable-maximum-line-length) makes the maximum
      line length increase with the nesting depth of a line of code.  
      Basically, it causes the length of leading whitespace to be ignored when
      setting line breaks, so the formatting of a block of code is independent
      of its nesting depth.  Try this option if you have deeply nested 
      code or data structures, perhaps in conjunction with the -wc flag
      described next.  The default is not todo this.
    
    - New flag -wc=n (--whitespace-cycle=n) also addresses problems with
      very deeply nested code and data structures.  When this parameter is
      used and the nesting depth exceeds the value n, the leading whitespace 
      will be reduced and start at 1 again.  The result is that deeply
      nested blocks of code will shift back to the left. This occurs cyclically 
      to any nesting depth.  This flag may be used either with or without -vmll.
      The default is not to use this (-wc=0).

    - Fixed RT #78764, error parsing smartmatch operator followed by anonymous
      hash or array and then a ternary operator; two examples:

       qr/3/ ~~ ['1234'] ? 1 : 0;
       map { $_ ~~ [ '0', '1' ] ? 'x' : 'o' } @a;

    - Fixed problem with specifying spaces around arrows using -wls='->'
      and -wrs='->'.  Thanks to Alain Valleton for documenting this problem. 

    - Implemented RT #53183, wishlist, lines of code with the same indentation
      level which are contained with multiple stacked opening and closing tokens
      (requested with flags -sot -sct) now have reduced indentation.  

       # Default
       $sender->MailMsg(
           {
               to      => $addr,
               subject => $subject,
               msg     => $body
           }
       );

       # OLD: perltidy -sot -sct 
       $sender->MailMsg( {
               to      => $addr,
               subject => $subject,
               msg     => $body
       } );

       # NEW: perltidy -sot -sct 
       $sender->MailMsg( {
           to      => $addr,
           subject => $subject,
           msg     => $body
       } );

    - New flag -act=n (--all-containers-tightness=n) is an abbreviation for
      -pt=n -sbt=n -bt=n -bbt=n, where n=0,1, or 2.  It simplifies input when all
      containers have the same tightness. Using the same example:

       # NEW: perltidy -sot -sct -act=2
       $sender->MailMsg({
           to      => $addr,
           subject => $subject,
           msg     => $body
       });

    - New flag -sac (--stack-all-containers) is an abbreviation for -sot -sct
      This is part of wishlist item RT #53183. Using the same example again:

       # NEW: perltidy -sac -act=2
       $sender->MailMsg({
           to      => $addr,
           subject => $subject,
           msg     => $body
       });

     - new flag -scbb (--stack-closing-block-brace) causes isolated closing 
       block braces to stack as in the following example. (Wishlist item RT#73788)

       DEFAULT:
       for $w1 (@w1) {
           for $w2 (@w2) {
               for $w3 (@w3) {
                   for $w4 (@w4) {
                       push( @lines, "$w1 $w2 $w3 $w4\n" );
                   }
               }
           }
       }

       perltidy -scbb:
       for $w1 (@w1) {
           for $w2 (@w2) {
               for $w3 (@w3) {
                   for $w4 (@w4) {
                       push( @lines, "$w1 $w2 $w3 $w4\n" );
                   } } } }

      There is, at present, no flag to place these closing braces at the end
      of the previous line. It seems difficult to develop good rules for 
      doing this for a wide variety of code and data structures.

    - Parameters defining block types may use a wildcard '*' to indicate
      all block types.  Previously it was not possible to include bare blocks.
    
    - A flag -sobb (--stack-opening-block-brace) has been introduced as an
      alias for -bbvt=2 -bbvtl='*'.  So for example the following test code:

      {{{{{{{ $testing }}}}}}}

      cannot be formatted as above but can at least be kept vertically compact 
      using perltidy -sobb -scbb

      {   {   {   {   {   {   {   $testing
                              } } } } } } }

      Or even, perltidy -sobb -scbb -i=1 -bbt=2
      {{{{{{{$testing
            }}}}}}}


    - Error message improved for conflicts due to -pbp; thanks to Djun Kim.
     
    - Fixed RT #80645, error parsing special array name '@$' when used as 
      @{$} or $#{$}
    
    - Eliminated the -chk debug flag which was included in version 20010406 to
      do a one-time check for a bug with multi-line quotes.  It has not been
      needed since then.

    - Numerous other minor formatting improvements.

## 2012 07 14

    - Added flag -iscl (--ignore-side-comment-lengths) which causes perltidy 
      to ignore the length of side comments when setting line breaks, 
      RT #71848.  The default is to include the length of side comments when
      breaking lines to stay within the length prescribed by the -l=n
      maximum line length parameter.  For example,

        Default behavior on a single line with long side comment:
           $vmsfile =~ s/;[\d\-]*$//
             ;    # Clip off version number; we can use a newer version as well
      
        perltidy -iscl leaves the line intact:

           $vmsfile =~ s/;[\d\-]*$//; # Clip off version number; we can use a newer version as well

    - Fixed RT #78182, side effects with STDERR.  Error handling has been
      revised and the documentation has been updated.  STDERR can now be 
      redirected to a string reference, and perltidy now returns an 
      error flag instead of calling die when input errors are detected. 
      If the error flag is set then no tidied output was produced.
      See man Perl::Tidy for an example.

    - Fixed RT #78156, erroneous warning message for package VERSION syntax.

    - Added abbreviations -conv (--converge) to simplify iteration control.
      -conv is equivalent to -it=4 and will insure that the tidied code is
      converged to its final state with the minimum number of iterations.

    - Minor formatting modifications have been made to insure convergence.

    - Simplified and hopefully improved the method for guessing the starting 
      indentation level of entabbed code.  Added flag -dt=n (--default_tabsize=n) 
      which might be helpful if the guessing method does not work well for
      some editors.

    - Added support for stacked labels, upper case X/B in hex and binary, and
      CORE:: namespace.

    - Eliminated warning messages for using keyword names as constants.

## 2012 07 01

    - Corrected problem introduced by using a chomp on scalar references, RT #77978

    - Added support for Perl 5.14 package block syntax, RT #78114.

    - A convergence test is made if three or more iterations are requested with
      the -it=n parameter to avoid wasting computer time.  Several hundred Mb of
      code gleaned from the internet were searched with the results that: 
       - It is unusual for two iterations to be required unless a major 
         style change is being made. 
       - Only one case has been found where three iterations were required.  
       - No cases requiring four iterations have been found with this version.
      For the previous version several cases where found the results could
      oscillate between two semi-stable states. This version corrects this.

      So if it is important that the code be converged it is okay to set -it=4
      with this version and it will probably stop after the second iteration.

    - Improved ability to identify and retain good line break points in the
      input stream, such as at commas and equals. You can always tell 
      perltidy to ignore old breakpoints with -iob.  

    - Fixed glitch in which a terminal closing hash brace followed by semicolon
      was not outdented back to the leading line depth like other closing
      tokens.  Thanks to Keith Neargarder for noting this.

        OLD:
           my ( $pre, $post ) = @{
               {
                   "pp_anonlist" => [ "[", "]" ],
                   "pp_anonhash" => [ "{", "}" ]
               }->{ $kid->ppaddr }
             };   # terminal brace

        NEW:
           my ( $pre, $post ) = @{
               {
                   "pp_anonlist" => [ "[", "]" ],
                   "pp_anonhash" => [ "{", "}" ]
               }->{ $kid->ppaddr }
           };    # terminal brace

    - Removed extra indentation given to trailing 'if' and 'unless' clauses 
      without parentheses because this occasionally produced undesirable 
      results.  This only applies where parens are not used after the if or
      unless.

       OLD:
           return undef
             unless my ( $who, $actions ) =
                 $clause =~ /^($who_re)((?:$action_re)+)$/o; 
       
       NEW:
           return undef
             unless my ( $who, $actions ) =
             $clause =~ /^($who_re)((?:$action_re)+)$/o; 

## 2012 06 19

    - Updated perltidy to handle all quote modifiers defined for perl 5 version 16.

    - Side comment text in perltidyrc configuration files must now begin with
      at least one space before the #.  Thus:

      OK:
        -l=78 # Max line width is 78 cols
      BAD: 
        -l=78# Max line width is 78 cols

      This is probably true of almost all existing perltidyrc files, 
      but if you get an error message about bad parameters
      involving a '#' the first time you run this version, please check the side
      comments in your perltidyrc file, and add a space before the # if necessary.
      You can quickly see the contents your perltidyrc file, if any, with the
      command:

        perltidy -dpro

      The reason for this change is that some parameters naturally involve
      the # symbol, and this can get interpreted as a side comment unless the
      parameter is quoted.  For example, to define -sphb=# it used to be necessary
      to write
        -sbcp='#'
      to keep the # from becoming part of a comment.  This was causing 
      trouble for new users.  Now it can also be written without quotes: 
        -sbcp=#

    - Fixed bug in processing some .perltidyrc files containing parameters with
      an opening brace character, '{'.  For example the following was
      incorrectly processed:
         --static-block-comment-prefix="^#{2,}[^\s#]"
      Thanks to pdagosto.

    - Added flag -boa (--break-at-old-attribute-breakpoints) which retains
      any existing line breaks at attribute separation ':'. This is now the
      default, use -nboa to deactivate.  Thanks to Daphne Phister for the patch.  
      For example, given the following code, the line breaks at the ':'s will be
      retained:
          
                       my @field
                         : field
                         : Default(1)
                         : Get('Name' => 'foo') : Set('Name');

      whereas the previous version would have output a single line.  If
      the attributes are on a single line then they will remain on a single line.
    
    - Added new flags --blank-lines-before-subs=n (-blbs=n) and
      --blank-lines-before-packages=n (-blbp=n) to put n blank lines before
      subs and packages.  The old flag -bbs is now equivalent to -blbs=1 -blbp=1.
      and -nbbs is equivalent to -blbs=0 -blbp=0. Requested by M. Schwern and
      several others.

    - Added feature -nsak='*' meaning no space between any keyword and opening 
      paren.  This avoids listing entering a long list of keywords.  Requested
      by M. Schwern.

    - Added option to delete a backup of original file with in-place-modify (-b)
      if there were no errors.  This can be requested with the flag -bext='/'.  
      See documentation for details.  Requested by M. Schwern and others.

    - Fixed bug where the module postfilter parameter was not applied when -b 
      flag was used.  This was discovered during testing.

    - Fixed in-place-modify (-b) to work with symbolic links to source files.
      Thanks to Ted Johnson.

    - Fixed bug where the Perl::Tidy module did not allow -b to be used 
      in some cases.

    - No extra blank line is added before a comment which follows
      a short line ending in an opening token, for example like this:
       OLD:
               if (

                   # unless we follow a blank or comment line
                   $last_line_leading_type !~ /^[#b]$/
                   ...

       NEW:
               if (
                   # unless we follow a blank or comment line
                   $last_line_leading_type !~ /^[#b]$/
                   ...

       The blank is not needed for readability in these cases because there
       already is already space above the comment.  If a blank already 
       exists there it will not be removed, so this change should not 
       change code which has previously been formatted with perltidy. 
       Thanks to R.W.Stauner.

    - Likewise, no extra blank line is added above a comment consisting of a
      single #, since nothing is gained in readability.

    - Fixed error in which a blank line was removed after a #>>> directive. 
      Thanks to Ricky Morse.

    - Unnecessary semicolons after given/when/default blocks are now removed.

    - Fixed bug where an unwanted blank line could be added before
      pod text in __DATA__ or __END__ section.  Thanks to jidani.

    - Changed exit flags from 1 to 0 to indicate success for -help, -version, 
      and all -dump commands.  Also added -? as another way to dump the help.
      Requested by Keith Neargarder.

    - Fixed bug where .ERR and .LOG files were not written except for -it=2 or more

    - Fixed bug where trailing blank lines at the end of a file were dropped when
      -it>1.

    - Fixed bug where a line occasionally ended with an extra space. This reduces
      the number of instances where a second iteration gives a result different
      from the first. 

    - Updated documentation to note that the Tidy.pm module <stderr> parameter may
      not be a reference to SCALAR or ARRAY; it must be a file.
    
    - Syntax check with perl now work when the Tidy.pm module is processing
      references to arrays and strings.  Thanks to Charles Alderman.

    - Zero-length files are no longer processed due to concerns for data loss
      due to side effects in some scenarios.

    - block labels, if any, are now included in closing side comment text
      when the -csc flag is used.  Suggested by Aaron.  For example, 
      the label L102 in the following block is now included in the -csc text:

         L102: for my $i ( 1 .. 10 ) {
           ...
         } ## end L102: for my $i ( 1 .. 10 )

## 2010 12 17

    - added new flag -it=n or --iterations=n
      This flag causes perltidy to do n complete iterations.  
      For most purposes the default of n=1 should be satisfactory.  However n=2
      can be useful when a major style change is being made, or when code is being
      beautified on check-in to a source code control system.  The run time will be
      approximately proportional to n, and it should seldom be necessary to use a
      value greater than n=2.  Thanks to Jonathan Swartz

    - A configuration file pathname begins with three dots, e.g.
      ".../.perltidyrc", indicates that the file should be searched for starting
      in the current directory and working upwards. This makes it easier to have
      multiple projects each with their own .perltidyrc in their root directories.
      Thanks to Jonathan Swartz for this patch.

    - Added flag --notidy which disables all formatting and causes the input to be
      copied unchanged.  This can be useful in conjunction with hierarchical
      F<.perltidyrc> files to prevent unwanted tidying.
      Thanks to Jonathan Swartz for this patch.

    - Added prefilters and postfilters in the call to the Tidy.pm module.
      Prefilters and postfilters. The prefilter is a code reference that 
      will be applied to the source before tidying, and the postfilter 
      is a code reference to the result before outputting.  

      Thanks to Jonathan Swartz for this patch.  He writes:
      This is useful for all manner of customizations. For example, I use
      it to convert the 'method' keyword to 'sub' so that perltidy will work for
      Method::Signature::Simple code:

      Perl::Tidy::perltidy(
         prefilter => sub { $_ = $_[0]; s/^method (.*)/sub $1 \#__METHOD/gm; return $_ },
         postfilter => sub { $_ = $_[0]; s/^sub (.*?)\s* \#__METHOD/method $1/gm; return $_ }
      );

    - The starting indentation level of sections of code entabbed with -et=n
      is correctly guessed if it was also produced with the same -et=n flag.  This
      keeps the indentation stable on repeated formatting passes within an editor.
      Thanks to Sam Kington and Glenn.

    - Functions with prototype '&' had a space between the function and opening
      peren.  This space now only occurs if the flag --space-function-paren (-sfp)
      is set.  Thanks to Zrajm Akfohg.

    - Patch to never put spaces around a bare word in braces beginning with ^ as in:
        my $before = ${^PREMATCH};
      even if requested with the -bt=0 flag because any spaces cause a syntax error in perl.
      Thanks to Fabrice Dulanoy.

## 2009 06 16

    - Allow configuration file to be 'perltidy.ini' for Windows systems.
      i.e. C:\Documents and Settings\User\perltidy.ini
      and added documentation for setting configuration file under Windows in man
      page.  Thanks to Stuart Clark.

    - Corrected problem of unwanted semicolons in hash ref within given/when code.
     Thanks to Nelo Onyiah.

    - added new flag -cscb or --closing-side-comments-balanced
     When using closing-side-comments, and the closing-side-comment-maximum-text
     limit is exceeded, then the comment text must be truncated.  Previous
     versions of perltidy terminate with three dots, and this can still be
     achieved with -ncscb:
      
      perltidy -csc -ncscb

      } ## end foreach my $foo (sort { $b cmp $a ...
      
     However this causes a problem with older editors which cannot recognize
     comments or are not configured to doso because they cannot "bounce" around in
     the text correctly.  The B<-cscb> flag tries to help them by 
     appending appropriate terminal balancing structure:
      
      perltidy -csc -cscb

      } ## end foreach my $foo (sort { $b cmp $a ... })
      
     Since there is much to be gained and little to be lost by doing this,
     the default is B<-cscb>.  Use B<-ncscb> if you do not want this.

     Thanks to Daniel Becker for suggesting this option.

    - After an isolated closing eval block the continuation indentation will be
      removed so that the braces line up more like other blocks.  Thanks to Yves Orton.

    OLD:
       eval {
           #STUFF;
           1;    # return true
         }  
         or do {
           #handle error
         };

    NEW:
       eval {
           #STUFF;
           1;    # return true
       } or do {
           #handle error
       };

    -A new flag -asbl (or --opening-anonymous-sub-brace-on-new-line) has
     been added to put the opening brace of anonymous sub's on a new line,
     as in the following snippet:

       my $code = sub
       {
           my $arg = shift;
           return $arg->(@_);
       };

     This was not possible before because the -sbl flag only applies to named
     subs. Thanks to Benjamin Krupp.

    -Fix tokenization bug with the following snippet
      print 'hi' if { x => 1, }->{x};
     which resulted in a semicolon being added after the comma.  The workaround
     was to use -nasc, but this is no longer necessary.  Thanks to Brian Duggan. 

    -Fixed problem in which an incorrect error message could be triggered
    by the (unusual) combination of parameters  -lp -i=0 -l=2 -ci=0 for
    example.  Thanks to Richard Jelinek.

    -A new flag --keep-old-blank-lines=n has been added to
    give more control over the treatment of old blank lines in
    a script.  The manual has been revised to discuss the new
    flag and clarify the treatment of old blank lines.  Thanks
    to Oliver Schaefer.

## 2007 12 05

    -Improved support for perl 5.10: New quote modifier 'p', new block type UNITCHECK, 
    new keyword break, improved formatting of given/when.

    -Corrected tokenization bug of something like $var{-q}.

    -Numerous minor formatting improvements.

    -Corrected list of operators controlled by -baao -bbao to include
      . : ? && || and or err xor

    -Corrected very minor error in log file involving incorrect comment
    regarding need for upper case of labels.  

    -Fixed problem where perltidy could run for a very long time
    when given certain non-perl text files.

    -Line breaks in un-parenthesized lists now try to follow
    line breaks in the input file rather than trying to fill
    lines.  This usually works better, but if this causes
    trouble you can use -iob to ignore any old line breaks.
    Example for the following input snippet:

       print
       "conformability (Not the same dimension)\n",
       "\t", $have, " is ", text_unit($hu), "\n",
       "\t", $want, " is ", text_unit($wu), "\n",
       ;

     OLD:
       print "conformability (Not the same dimension)\n", "\t", $have, " is ",
         text_unit($hu), "\n", "\t", $want, " is ", text_unit($wu), "\n",;

     NEW:
       print "conformability (Not the same dimension)\n",
         "\t", $have, " is ", text_unit($hu), "\n",
         "\t", $want, " is ", text_unit($wu), "\n",
         ;

## 2007 08 01

    -Added -fpsc option (--fixed-position-side-comment). Thanks to Ueli Hugenschmidt. 
    For example -fpsc=40 tells perltidy to put side comments in column 40
    if possible.  

    -Added -bbao and -baao options (--break-before-all-operators and
    --break-after-all-operators) to simplify command lines and configuration
    files.  These define an initial preference for breaking at operators which can
    be modified with -wba and -wbb flags.  For example to break before all operators
    except an = one could use --bbao -wba='=' rather than listing every
    single perl operator (except =) on a -wbb flag.

    -Added -kis option (--keep-interior-semicolons).  Use the B<-kis> flag
    to prevent breaking at a semicolon if there was no break there in the
    input file.  To illustrate, consider the following input lines:

       dbmclose(%verb_delim); undef %verb_delim;
       dbmclose(%expanded); undef %expanded;
       dbmclose(%global); undef %global;

    Normally these would be broken into six lines, but 
    perltidy -kis gives:

       dbmclose(%verb_delim); undef %verb_delim;
       dbmclose(%expanded);   undef %expanded;
       dbmclose(%global);     undef %global;
    
    -Improved formatting of complex ternary statements, with indentation
    of nested statements.  
     OLD:
       return defined( $cw->{Selected} )
         ? (wantarray)
         ? @{ $cw->{Selected} }
         : $cw->{Selected}[0]
         : undef;

     NEW:
       return defined( $cw->{Selected} )
         ? (wantarray)
             ? @{ $cw->{Selected} }
             : $cw->{Selected}[0]
         : undef;

    -Text following un-parenthesized if/unless/while/until statements get a
    full level of indentation.  Suggested by Jeff Armstrong and others.
    OLD:
       return $ship->chargeWeapons("phaser-canon")
         if $encounter->description eq 'klingon'
         and $ship->firepower >= $encounter->firepower
         and $location->status ne 'neutral';
    NEW:
       return $ship->chargeWeapons("phaser-canon")
         if $encounter->description eq 'klingon'
             and $ship->firepower >= $encounter->firepower
             and $location->status ne 'neutral';

## 2007 05 08

    -Fixed bug where #line directives were being indented.  Thanks to
    Philippe Bruhat.

## 2007 05 04

    -Fixed problem where an extra blank line was added after an =cut when either
    (a) the =cut started (not stopped) a POD section, or (b) -mbl > 1. 
    Thanks to J. Robert Ray and Bill Moseley.

## 2007 04 24

    -ole (--output-line-ending) and -ple (--preserve-line-endings) should
    now work on all systems rather than just unix systems. Thanks to Dan
    Tyrell.

    -Fixed problem of a warning issued for multiple subs for BEGIN subs
    and other control subs. Thanks to Heiko Eissfeldt.
    
    -Fixed problem where no space was introduced between a keyword or
    bareword and a colon, such as:

    ( ref($result) eq 'HASH' && !%$result ) ? undef: $result;

    Thanks to Niek.

    -Added a utility program 'break_long_quotes.pl' to the examples directory of
    the distribution.  It breaks long quoted strings into a chain of concatenated
    sub strings no longer than a selected length.  Suggested by Michael Renner as
    a perltidy feature but was judged to be best done in a separate program.

    -Updated docs to remove extra < and >= from list of tokens 
    after which breaks are made by default.  Thanks to Bob Kleemann.

    -Removed improper uses of $_ to avoid conflicts with external calls, giving
    error message similar to:
       Modification of a read-only value attempted at 
       /usr/share/perl5/Perl/Tidy.pm line 6907.
    Thanks to Michael Renner.

    -Fixed problem when errorfile was not a plain filename or filehandle
    in a call to Tidy.pm.  The call
    perltidy(source => \$input, destination => \$output, errorfile => \$err);
    gave the following error message:
     Not a GLOB reference at /usr/share/perl5/Perl/Tidy.pm line 3827.
    Thanks to Michael Renner and Phillipe Bruhat.

    -Fixed problem where -sot would not stack an opening token followed by
    a side comment.  Thanks to Jens Schicke.

    -improved breakpoints in complex math and other long statements. Example:
    OLD:
       return
         log($n) + 0.577215664901532 + ( 1 / ( 2 * $n ) ) -
         ( 1 / ( 12 * ( $n**2 ) ) ) + ( 1 / ( 120 * ( $n**4 ) ) );
    NEW:
       return
         log($n) + 0.577215664901532 +
         ( 1 / ( 2 * $n ) ) -
         ( 1 / ( 12 * ( $n**2 ) ) ) +
         ( 1 / ( 120 * ( $n**4 ) ) );

    -more robust vertical alignment of complex terminal else blocks and ternary
    statements.

## 2006 07 19

    -Eliminated bug where a here-doc invoked through an 'e' modifier on a pattern
    replacement text was not recognized.  The tokenizer now recursively scans
    replacement text (but does not reformat it).

    -improved vertical alignment of terminal else blocks and ternary statements.
     Thanks to Chris for the suggestion. 

     OLD:
       if    ( IsBitmap() ) { return GetBitmap(); }
       elsif ( IsFiles() )  { return GetFiles(); }
       else { return GetText(); }

     NEW:
       if    ( IsBitmap() ) { return GetBitmap(); }
       elsif ( IsFiles() )  { return GetFiles(); }
       else                 { return GetText(); }

     OLD:
       $which_search =
           $opts{"t"} ? 'title'
         : $opts{"s"} ? 'subject'
         : $opts{"a"} ? 'author'
         : 'title';

     NEW:
       $which_search =
           $opts{"t"} ? 'title'
         : $opts{"s"} ? 'subject'
         : $opts{"a"} ? 'author'
         :              'title';

    -improved indentation of try/catch blocks and other externally defined
    functions accepting a block argument.  Thanks to jae.

    -Added support for Perl 5.10 features say and smartmatch.

    -Added flag -pbp (--perl-best-practices) as an abbreviation for parameters
    suggested in Damian Conway's "Perl Best Practices".  -pbp is the same as:

       -l=78 -i=4 -ci=4 -st -se -vt=2 -cti=0 -pt=1 -bt=1 -sbt=1 -bbt=1 -nsfs -nolq
       -wbb="% + - * / x != == >= <= =~ !~ < > | & >= < = 
             **= += *= &= <<= &&= -= /= |= >>= ||= .= %= ^= x="

     Please note that the -st here restricts input to standard input; use
     -nst if necessary to override.

    -Eliminated some needless breaks at equals signs in -lp indentation.

       OLD:
           $c =
             Math::Complex->make(LEFT + $x * (RIGHT - LEFT) / SIZE,
                                 TOP + $y * (BOTTOM - TOP) / SIZE);
       NEW:
           $c = Math::Complex->make(LEFT + $x * (RIGHT - LEFT) / SIZE,
                                    TOP + $y * (BOTTOM - TOP) / SIZE);

    A break at an equals is sometimes useful for preventing complex statements 
    from hitting the line length limit.  The decision to do this was 
    over-eager in some cases and has been improved.  Thanks to Royce Reece.

    -qw quotes contained in braces, square brackets, and parens are being
    treated more like those containers as far as stacking of tokens.  Also
    stack of closing tokens ending ');' will outdent to where the ');' would
    have outdented if the closing stack is matched with a similar opening stack.

     OLD: perltidy -soc -sct
       __PACKAGE__->load_components(
           qw(
             PK::Auto
             Core
             )
       );
     NEW: perltidy -soc -sct
       __PACKAGE__->load_components( qw(
             PK::Auto
             Core
       ) );
     Thanks to Aran Deltac

    -Eliminated some undesirable or marginally desirable vertical alignments.
    These include terminal colons, opening braces, and equals, and particularly
    when just two lines would be aligned.

    OLD:
       my $accurate_timestamps = $Stamps{lnk};
       my $has_link            = 
           ...
    NEW:
       my $accurate_timestamps = $Stamps{lnk};
       my $has_link =

    -Corrected a problem with -mangle in which a space would be removed
    between a keyword and variable beginning with ::.

## 2006 06 14

    -Attribute argument lists are now correctly treated as quoted strings
    and not formatted.  This is the most important update in this version.
    Thanks to Borris Zentner, Greg Ferguson, Steve Kirkup.

    -Updated to recognize the defined or operator, //, to be released in Perl 10.
    Thanks to Sebastien Aperghis-Tramoni.

    -A useful utility perltidyrc_dump.pl is included in the examples section.  It
    will read any perltidyrc file and write it back out in a standard format
    (though comments are lost).

    -Added option to have perltidy read and return a hash with the contents of a
    perltidyrc file.  This may be used by Leif Eriksen's tidyview code.  This
    feature is used by the demonstration program 'perltidyrc_dump.pl' in the
    examples directory.

    -Improved error checking in perltidyrc files.  Unknown bare words were not
    being caught.

    -The --dump-options parameter now dumps parameters in the format required by a
    perltidyrc file.

    -V-Strings with underscores are now recognized.
    For example: $v = v1.2_3; 

    -cti=3 option added which gives one extra indentation level to closing 
    tokens always.  This provides more predictable closing token placement
    than cti=2.  If you are using cti=2 you might want to try cti=3.

    -To identify all left-adjusted comments as static block comments, use C<-sbcp='^#'>.

    -New parameters -fs, -fsb, -fse added to allow sections of code between #<<<
    and #>>> to be passed through verbatim. This is enabled by default and turned
    off by -nfs.  Flags -fsb and -fse allow other beginning and ending markers.
    Thanks to Wolfgang Werner and Marion Berryman for suggesting this.  

    -added flag -skp to put a space between all Perl keywords and following paren.
    The default is to only do this for certain keywords.  Suggested by
    H.Merijn Brand.

    -added flag -sfp to put a space between a function name and following paren.
    The default is not to do this.  Suggested by H.Merijn Brand.

    -Added patch to avoid breaking GetOpt::Long::Configure set by calling program. 
    Thanks to Philippe Bruhat.

    -An error was fixed in which certain parameters in a .perltidyrc file given
    without the equals sign were not recognized.  That is,
    '--brace-tightness 0' gave an error but '--brace-tightness=0' worked
    ok.  Thanks to Zac Hansen.

    -An error preventing the -nwrs flag from working was corrected. Thanks to
     Greg Ferguson.

    -Corrected some alignment problems with entab option.

    -A bug with the combination of -lp and -extrude was fixed (though this
    combination doesn't really make sense).  The bug was that a line with
    a single zero would be dropped.  Thanks to Cameron Hayne.

    -Updated Windows detection code to avoid an undefined variable.
    Thanks to Joe Yates and Russ Jones.

    -Improved formatting for short trailing statements following a closing paren.
    Thanks to Joe Matarazzo.

    -The handling of the -icb (indent closing block braces) flag has been changed
    slightly to provide more consistent and predictable formatting of complex
    structures.  Instead of giving a closing block brace the indentation of the
    previous line, it is now given one extra indentation level.  The two methods
    give the same result if the previous line was a complete statement, as in this
    example:

           if ($task) {
               yyy();
               }    # -icb
           else {
               zzz();
               }
    The change also fixes a problem with empty blocks such as:

       OLD, -icb:
       elsif ($debug) {
       }

       NEW, -icb:
       elsif ($debug) {
           }

    -A problem with -icb was fixed in which a closing brace was misplaced when
    it followed a quote which spanned multiple lines.

    -Some improved breakpoints for -wba='&& || and or'

    -Fixed problem with misaligned cuddled else in complex statements
    when the -bar flag was also used.  Thanks to Alex and Royce Reese.

    -Corrected documentation to show that --outdent-long-comments is the default.
    Thanks to Mario Lia.

    -New flag -otr (opening-token-right) is similar to -bar (braces-always-right)
    but applies to non-structural opening tokens.

    -new flags -sot (stack-opening-token), -sct (stack-closing-token).
    Suggested by Tony.

## 2003 10 21

    -The default has been changed to not do syntax checking with perl.  
      Use -syn if you want it.  Perltidy is very robust now, and the -syn
      flag now causes more problems than it's worth because of BEGIN blocks
      (which get executed with perl -c).  For example, perltidy will never
      return when trying to beautify this code if -syn is used:

           BEGIN { 1 while { }; }

     Although this is an obvious error, perltidy is often run on untested
     code which is more likely to have this sort of problem.  A more subtle
     example is:

           BEGIN { use FindBin; }

     which may hang on some systems using -syn if a shared file system is
     unavailable.

    -Changed style -gnu to use -cti=1 instead of -cti=2 (see next item).
     In most cases it looks better.  To recover the previous format, use
     '-gnu -cti=2'

    -Added flags -cti=n for finer control of closing token indentation.
      -cti = 0 no extra indentation (default; same as -nicp)
      -cti = 1 enough indentation so that the closing token
           aligns with its opening token.
      -cti = 2 one extra indentation level if the line has the form 
             );   ];   or   };     (same as -icp).

      The new option -cti=1 works well with -lp:

      EXAMPLES:

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
     This is backwards compatible with -icp. See revised manual for
     details.  Suggested by Mike Pennington.
     
    -Added flag '--preserve-line-endings' or '-ple' to cause the output
     line ending to be the same as in the input file, for unix, dos, 
     or mac line endings.  Only works under unix. Suggested by 
     Rainer Hochschild.

    -Added flag '--output-line-ending=s' or '-ole=s' where s=dos or win,
     unix, or mac.  Only works under unix.

    -Files with Mac line endings should now be handled properly under unix
     and dos without being passed through a converter.

    -You may now include 'and', 'or', and 'xor' in the list following
     '--want-break-after' to get line breaks after those keywords rather than
     before them.  Suggested by Rainer Hochschild.

    -Corrected problem with command line option for -vtc=n and -vt=n. The
     equals sign was being eaten up by the Windows shell so perltidy didn't
     see it.

## 2003 07 26

    -Corrected cause of warning message with recent versions of Perl:
       "Possible precedence problem on bitwise & operator at ..."
     Thanks to Jim Files.

    -fixed bug with -html with '=for pod2html' sections, in which code/pod
    output order was incorrect.  Thanks to Tassilo von Parseval.

    -fixed bug when the -html flag is used, in which the following error
    message, plus others, appear:
        did not see <body> in pod2html output
    This was caused by a change in the format of html output by pod2html
    VERSION 1.04 (included with perl 5.8).  Thanks to Tassilo von Parseval.

    -Fixed bug where an __END__ statement would be mistaken for a label
    if it is immediately followed by a line with a leading colon. Thanks
    to John Bayes.
    
    -Implemented guessing logic for brace types when it is ambiguous.  This
    has been on the TODO list a long time.  Thanks to Boris Zentner for
    an example.

    -Long options may now be negated either as '--nolong-option' 
    or '--no-long-option'.  Thanks to Philip Newton for the suggestion.

    -added flag --html-entities or -hent which controls the use of
    Html::Entities for html formatting.  Use --nohtml-entities or -nhent to
    prevent the use of Html::Entities to encode special symbols.  The
    default is -hent.  Html::Entities when formatting perl text to escape
    special symbols.  This may or may not be the right thing to do,
    depending on browser/language combinations.  Thanks to Burak Gursoy for
    this suggestion.

    -Bareword strings with leading '-', like, '-foo' now count as 1 token
    for horizontal tightness.  This way $a{'-foo'}, $a{foo}, and $a{-foo}
    are now all treated similarly.  Thus, by default, OLD: $a{ -foo } will
    now be NEW: $a{-foo}.  Suggested by Mark Olesen.

    -added 2 new flags to control spaces between keywords and opening parens:
      -sak=s  or --space-after-keyword=s,  and
      -nsak=s or --nospace-after-keyword=s, where 's' is a list of keywords.

    The new default list of keywords which get a space is:

      "my local our and or eq ne if else elsif until unless while for foreach
        return switch case given when"

    Use -sak=s and -nsak=s to add and remove keywords from this list,
       respectively.

    Explanation: Stephen Hildrey noted that perltidy was being inconsistent
    in placing spaces between keywords and opening parens, and sent a patch
    to give user control over this.  The above list was selected as being
    a reasonable default keyword list.  Previously, perltidy
    had a hardwired list which also included these keywords:

           push pop shift unshift join split die

    but did not have 'our'.  Example: if you prefer to make perltidy behave
    exactly as before, you can include the following two lines in your
    .perltidyrc file: 

      -sak="push pop local shift unshift join split die"
      -nsak="our"

    -Corrected html error in .toc file when -frm -html is used (extra ");
     browsers were tolerant of it.

    -Improved alignment of chains of binary and ?/: operators. Example:
     OLD:
       $leapyear =
         $year % 4     ? 0
         : $year % 100 ? 1
         : $year % 400 ? 0
         : 1;
     NEW:
       $leapyear =
           $year % 4   ? 0
         : $year % 100 ? 1
         : $year % 400 ? 0
         : 1;

    -improved breakpoint choices involving '->'

    -Corrected tokenization of things like ${#}. For example,
     ${#} is valid, but ${# } is a syntax error.

    -Corrected minor tokenization errors with indirect object notation.
     For example, 'new A::()' works now.

    -Minor tokenization improvements; all perl code distributed with perl 5.8 
     seems to be parsed correctly except for one instance (lextest.t) 
     of the known bug.

## 2002 11 30

    -Implemented scalar attributes.  Thanks to Sean Tobin for noting this.

    -Fixed glitch introduced in previous release where -pre option
    was not outputting a leading html <pre> tag.

    -Numerous minor improvements in vertical alignment, including the following:

    -Improved alignment of opening braces in many cases.  Needed for improved
    switch/case formatting, and also suggested by Mark Olesen for sort/map/grep
    formatting.  For example:

     OLD:
       @modified =
         map { $_->[0] }
         sort { $a->[1] <=> $b->[1] }
         map { [ $_, -M ] } @filenames;

     NEW:
       @modified =
         map  { $_->[0] }
         sort { $a->[1] <=> $b->[1] }
         map  { [ $_, -M ] } @filenames;

    -Eliminated alignments across unrelated statements. Example:
     OLD:
       $borrowerinfo->configure( -state => 'disabled' );
       $borrowerinfo->grid( -col        => 1, -row => 0, -sticky => 'w' );

     NEW:  
       $borrowerinfo->configure( -state => 'disabled' );
       $borrowerinfo->grid( -col => 1, -row => 0, -sticky => 'w' );

     Thanks to Mark Olesen for suggesting this.

    -Improved alignment of '='s in certain cases.
     Thanks to Norbert Gruener for sending an example.

    -Outdent-long-comments (-olc) has been re-instated as a default, since
     it works much better now.  Use -nolc if you want to prevent it.

    -Added check for 'perltidy file.pl -o file.pl', which causes file.pl
    to be lost. (The -b option should be used instead). Thanks to mreister
    for reporting this problem.

## 2002 11 06

    -Switch/case or given/when syntax is now recognized.  Its vertical alignment
    is not great yet, but it parses ok.  The words 'switch', 'case', 'given',
    and 'when' are now treated as keywords.  If this causes trouble with older
    code, we could introduce a switch to deactivate it.  Thanks to Stan Brown
    and Jochen Schneider for recommending this.

    -Corrected error parsing sub attributes with call parameters.
    Thanks to Marc Kerr for catching this.

    -Sub prototypes no longer need to be on the same line as sub names.  

    -a new flag -frm or --frames will cause html output to be in a
    frame, with table of contents in the left panel and formatted source
    in the right panel.  Try 'perltidy -html -frm somemodule.pm' for example.

    -The new default for -html formatting is to pass the pod through Pod::Html.
    The result is syntax colored code within your pod documents. This can be
    deactivated with -npod.  Thanks to those who have written to discuss this,
    particularly Mark Olesen and Hugh Myers.

    -the -olc (--outdent-long-comments) option works much better.  It now outdents
    groups of consecutive comments together, and by just the amount needed to
    avoid having any one line exceeding the maximum line length.

    -block comments are now trimmed of trailing whitespace.

    -if a directory specified with -opath does not exist, it will be created.

    -a table of contents to packages and subs is output when -html is used.
    Use -ntoc to prevent this. 

    -fixed an unusual bug in which a 'for' statement following a 'format'
    statement was not correctly tokenized.  Thanks to Boris Zentner for
    catching this.

    -Tidy.pm is no longer dependent on modules IO::Scalar and IO::ScalarArray.  
    There were some speed issues.  Suggested by Joerg Walter.

    -The treatment of quoted wildcards (file globs) is now system-independent. 
    For example

       perltidy 'b*x.p[lm]'

    would match box.pl, box.pm, brinx.pm under any operating system.  Of
    course, anything unquoted will be subject to expansion by any shell.

    -default color for keywords under -html changed from 
    SaddleBrown (#8B4513) to magenta4 (#8B008B).

    -fixed an arg parsing glitch in which something like:
      perltidy quick-help
    would trigger the help message and exit, rather than operate on the
    file 'quick-help'.

## 2002 09 22

    -New option '-b' or '--backup-and-modify-in-place' will cause perltidy to
    overwrite the original file with the tidied output file.  The original
    file will be saved with a '.bak' extension (which can be changed with
    -bext=s).  Thanks to Rudi Farkas for the suggestion.

    -An index to all subs is included at the top of -html output, unless
    only the <pre> section is written.

    -Anchor lines of the form <a name="mysub"></a> are now inserted at key points
    in html output, such as before sub definitions, for the convenience of
    postprocessing scripts.  Suggested by Howard Owen.

    -The cuddled-else (-ce) flag now also makes cuddled continues, like
    this:

       while ( ( $pack, $file, $line ) = caller( $i++ ) ) {
          # bla bla
       } continue {
           $prevpack = $pack;
       }

    Suggested by Simon Perreault.  

    -Fixed bug in which an extra blank line was added before an =head or 
    similar pod line after an __END__ or __DATA__ line each time 
    perltidy was run.  Also, an extra blank was being added after
    a terminal =cut.  Thanks to Mike Birdsall for reporting this.

## 2002 08 26

    -Fixed bug in which space was inserted in a hyphenated hash key:
       my $val = $myhash{USER-NAME};
     was converted to:
       my $val = $myhash{USER -NAME}; 
     Thanks to an anonymous bug reporter at sourceforge.

    -Fixed problem with the '-io' ('--indent-only') where all lines 
     were double spaced.  Thanks to Nick Andrew for reporting this bug.

    -Fixed tokenization error in which something like '-e1' was 
     parsed as a number. 

    -Corrected a rare problem involving older perl versions, in which 
     a line break before a bareword caused problems with 'use strict'.
     Thanks to Wolfgang Weisselberg for noting this.

    -More syntax error checking added.

    -Outdenting labels (-ola) has been made the default, in order to follow the
     perlstyle guidelines better.  It's probably a good idea in general, but
     if you do not want this, use -nola in your .perltidyrc file.
     
    -Updated rules for padding logical expressions to include more cases.
     Thanks to Wolfgang Weisselberg for helpful discussions.

    -Added new flag -osbc (--outdent-static-block-comments) which will
     outdent static block comments by 2 spaces (or whatever -ci equals).
     Requested by Jon Robison.

## 2002 04 25

    -Corrected a bug, introduced in the previous release, in which some
     closing side comments (-csc) could have incorrect text.  This is
     annoying but will be correct the next time perltidy is run with -csc.

    -Fixed bug where whitespace was being removed between 'Bar' and '()' 
     in a use statement like:

          use Foo::Bar ();

    -Whenever possible, if a logical expression is broken with leading
     '&&', '||', 'and', or 'or', then the leading line will be padded
     with additional space to produce alignment.  This has been on the
     todo list for a long time; thanks to Frank Steinhauer for reminding
     me to do it.  Notice the first line after the open parens here:

           OLD: perltidy -lp
           if (
                !param("rules.to.$linecount")
                && !param("rules.from.$linecount")
                && !param("rules.subject.$linecount")
                && !(
                      param("rules.fieldname.$linecount")
                      && param("rules.fieldval.$linecount")
                )
                && !param("rules.size.$linecount")
                && !param("rules.custom.$linecount")
             )

           NEW: perltidy -lp
           if (
                   !param("rules.to.$linecount")
                && !param("rules.from.$linecount")
                && !param("rules.subject.$linecount")
                && !(
                         param("rules.fieldname.$linecount")
                      && param("rules.fieldval.$linecount")
                )
                && !param("rules.size.$linecount")
                && !param("rules.custom.$linecount")
             )

## 2002 04 16

    -Corrected a mistokenization of variables for a package with a name
     equal to a perl keyword.  For example: 

        my::qx();
        package my;
        sub qx{print "Hello from my::qx\n";}

     In this case, the leading 'my' was mistokenized as a keyword, and a
     space was being place between 'my' and '::'.  This has been
     corrected.  Thanks to Martin Sluka for discovering this. 

    -A new flag -bol (--break-at-old-logic-breakpoints)
     has been added to control whether containers with logical expressions
     should be broken open.  This is the default.

    -A new flag -bok (--break-at-old-keyword-breakpoints)
     has been added to follow breaks at old keywords which return lists,
     such as sort and map.  This is the default.

    -A new flag -bot (--break-at-old-trinary-breakpoints) has been added to
     follow breaks at trinary (conditional) operators.  This is the default.

    -A new flag -cab=n has been added to control breaks at commas after
     '=>' tokens.  The default is n=1, meaning break unless this breaks
     open an existing on-line container.

    -A new flag -boc has been added to allow existing list formatting
     to be retained.  (--break-at-old-comma-breakpoints).  See updated manual.

    -A new flag -iob (--ignore-old-breakpoints) has been added to
     prevent the locations of old breakpoints from influencing the output
     format.

    -Corrected problem where nested parentheses were not getting full
     indentation.  This has been on the todo list for some time; thanks 
     to Axel Rose for a snippet demonstrating this issue.

               OLD: inner list is not indented
               $this->sendnumeric(
                   $this->server,
                   (
                     $ret->name,        $user->username, $user->host,
                   $user->server->name, $user->nick,     "H"
                   ),
               );

               NEW:
               $this->sendnumeric(
                   $this->server,
                   (
                       $ret->name,          $user->username, $user->host,
                       $user->server->name, $user->nick,     "H"
                   ),
               );

    -Code cleaned up by removing the following unused, undocumented flags.
     They should not be in any .perltidyrc files because they were just
     experimental flags which were never documented.  Most of them placed
     artificial limits on spaces, and Wolfgang Weisselberg convinced me that
     most of them they do more harm than good by causing unexpected results.

     --maximum-continuation-indentation (-mci)
     --maximum-whitespace-columns
     --maximum-space-to-comment (-xsc)
     --big-space-jump (-bsj)

    -Pod file 'perltidy.pod' has been appended to the script 'perltidy', and
     Tidy.pod has been append to the module 'Tidy.pm'.  Older MakeMaker's
     were having trouble.
    
    -A new flag -isbc has been added for more control on comments. This flag
     has the effect that if there is no leading space on the line, then the
     comment will not be indented, and otherwise it may be.  If both -ibc and
     -isbc are set, then -isbc takes priority.  Thanks to Frank Steinhauer
     for suggesting this.

    -A new document 'stylekey.pod' has been created to quickly guide new users
     through the maze of perltidy style parameters.  An html version is 
     on the perltidy web page.  Take a look! It should be very helpful.

    -Parameters for controlling 'vertical tightness' have been added:
     -vt and -vtc are the main controls, but finer control is provided
     with -pvt, -pcvt, -bvt, -bcvt, -sbvt, -sbcvt.  Block brace vertical
     tightness controls have also been added.
     See updated manual and also see 'stylekey.pod'. Simple examples:

       # perltidy -lp -vt=1 -vtc=1
       @month_of_year = ( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' );

       # perltidy -lp -vt=1 -vtc=0
       @month_of_year = ( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
       );

    -Lists which do not format well in uniform columns are now better
     identified and formatted.

       OLD:
       return $c->create( 'polygon', $x, $y, $x + $ruler_info{'size'},
           $y + $ruler_info{'size'}, $x - $ruler_info{'size'},
           $y + $ruler_info{'size'} );

       NEW:
       return $c->create(
           'polygon', $x, $y,
           $x + $ruler_info{'size'},
           $y + $ruler_info{'size'},
           $x - $ruler_info{'size'},
           $y + $ruler_info{'size'}
       );

       OLD:
         radlablist($f1, pad('Initial', $p), $b->{Init}->get_panel_ref, 'None ',
                    'None', 'Default', 'Default', 'Simple', 'Simple');
       NEW:
         radlablist($f1,
                    pad('Initial', $p),
                    $b->{Init}->get_panel_ref,
                    'None ', 'None', 'Default', 'Default', 'Simple', 'Simple');

    -Corrected problem where an incorrect html filename was generated for 
     external calls to Tidy.pm module.  Fixed incorrect html title when
     Tidy.pm is called with IO::Scalar or IO::Array source.

    -Output file permissions are now set as follows.  An output script file
     gets the same permission as the input file, except that owner
     read/write permission is added (otherwise, perltidy could not be
     rerun).  Html output files use system defaults.  Previously chmod 0755
     was used in all cases.  Thanks to Mark Olesen for bringing this up.

    -Missing semicolons will not be added in multi-line blocks of type
     sort, map, or grep.  This brings perltidy into closer agreement
     with common practice.  Of course, you can still put semicolons 
     there if you like.  Thanks to Simon Perreault for a discussion of this.

    -Most instances of extra semicolons are now deleted.  This is
     particularly important if the -csc option is used.  Thanks to Wolfgang
     Weisselberg for noting this.  For example, the following line
     (produced by 'h2xs' :) has an extra semicolon which will now be
     removed:

        BEGIN { plan tests => 1 };

    -New parameter -csce (--closing-side-comment-else-flag) can be used
     to control what text is appended to 'else' and 'elsif' blocks.
     Default is to just add leading 'if' text to an 'else'.  See manual.

    -The -csc option now labels 'else' blocks with additional information
     from the opening if statement and elsif statements, if space.
     Thanks to Wolfgang Weisselberg for suggesting this.

    -The -csc option will now remove any old closing side comments
     below the line interval threshold. Thanks to Wolfgang Weisselberg for
     suggesting this.

    -The abbreviation feature, which was broken in the previous version,
     is now fixed.  Thanks to Michael Cartmell for noting this.

    -Vertical alignment is now done for '||='  .. somehow this was 
     overlooked.

## 2002 02 25

    -This version uses modules for the first time, and a standard perl
     Makefile.PL has been supplied.  However, perltidy may still be
     installed as a single script, without modules.  See INSTALL for
     details.

    -The man page 'perl2web' has been merged back into the main 'perltidy'
     man page to simplify installation.  So you may remove that man page
     if you have an older installation.

    -Added patch from Axel Rose for MacPerl.  The patch prompts the user
     for command line arguments before calling the module 
     Perl::Tidy::perltidy.

    -Corrected bug with '-bar' which was introduced in the previous
     version.  A closing block brace was being indented.  Thanks to
     Alexandros M Manoussakis for reporting this.

    -New parameter '--entab-leading-whitespace=n', or '-et=n', has been
     added for those who prefer tabs.  This behaves different from the
     existing '-t' parameter; see updated man page.  Suggested by Mark
     Olesen.

    -New parameter '--perl-syntax-check-flags=s'  or '-pcsf=s' can be
     used to change the flags passed to perltidy in a syntax check.
     See updated man page.  Suggested by Mark Olesen. 

    -New parameter '--output-path=s'  or '-opath=s' will cause output
     files to be placed in directory s.  See updated man page.  Thanks for
     Mark Olesen for suggesting this.

    -New parameter --dump-profile (or -dpro) will dump to
     standard output information about the search for a
     configuration file, the name of whatever configuration file
     is selected, and its contents.  This should help debugging
     config files, especially on different Windows systems.

    -The -w parameter now notes possible errors of the form:

           $comment = s/^\s*(\S+)\..*/$1/;   # trim whitespace

    -Corrections added for a leading ':' and for leaving a leading 'tcsh'
     line untouched.  Mark Olesen reported that lines of this form were
     accepted by perl but not by perltidy:

           : # use -*- perl -*-
           eval 'exec perl -wS $0 "$@"'  # shell should exec 'perl'
           unless 1;                     # but Perl should skip this one

     Perl will silently swallow a leading colon on line 1 of a
     script, and now perltidy will do likewise.  For example,
     this is a valid script, provided that it is the first line,
     but not otherwise:

           : print "Hello World\n";
     
     Also, perltidy will now mark a first line with leading ':' followed by
     '#' as type SYSTEM (just as a #!  line), not to be formatted.

    -List formatting improved for certain lists with special
     initial terms, such as occur with 'printf', 'sprintf',
     'push', 'pack', 'join', 'chmod'.  The special initial term is
     now placed on a line by itself.  For example, perltidy -gnu

        OLD:
           $Addr = pack(
                        "C4",                hex($SourceAddr[0]),
                        hex($SourceAddr[1]), hex($SourceAddr[2]),
                        hex($SourceAddr[3])
                        );

        NEW:
           $Addr = pack("C4",
                        hex($SourceAddr[0]), hex($SourceAddr[1]),
                        hex($SourceAddr[2]), hex($SourceAddr[3]));

         OLD:
               push (
                     @{$$self{states}}, '64', '66', '68',
                     '70',              '72', '74', '76',
                     '78',              '80', '82', '84',
                     '86',              '88', '90', '92',
                     '94',              '96', '98', '100',
                     '102',             '104'
                     );

         NEW:
               push (
                     @{$$self{states}},
                     '64', '66', '68', '70', '72',  '74',  '76',
                     '78', '80', '82', '84', '86',  '88',  '90',
                     '92', '94', '96', '98', '100', '102', '104'
                     );

    -Lists of complex items, such as matrices, are now detected
     and displayed with just one item per row:

       OLD:
       $this->{'CURRENT'}{'gfx'}{'MatrixSkew'} = Text::PDF::API::Matrix->new(
           [ 1, tan( deg2rad($a) ), 0 ], [ tan( deg2rad($b) ), 1, 0 ],
           [ 0, 0, 1 ]
       );

       NEW:
       $this->{'CURRENT'}{'gfx'}{'MatrixSkew'} = Text::PDF::API::Matrix->new(
           [ 1,                  tan( deg2rad($a) ), 0 ],
           [ tan( deg2rad($b) ), 1,                  0 ],
           [ 0,                  0,                  1 ]
       );

    -The perl syntax check will be turned off for now when input is from
     standard input or standard output.  The reason is that this requires
     temporary files, which has produced far too many problems during
     Windows testing.  For example, the POSIX module under Windows XP/2000
     creates temporary names in the root directory, to which only the
     administrator should have permission to write.

    -Merged patch sent by Yves Orton to handle appropriate
     configuration file locations for different Windows varieties
     (2000, NT, Me, XP, 95, 98).

    -Added patch to properly handle a for/foreach loop without
     parens around a list represented as a qw.  I didn't know this
     was possible until Wolfgang Weisselberg pointed it out:

           foreach my $key qw\Uno Due Tres Quadro\ {
               print "Set $key\n";
           }

     But Perl will give a syntax error without the $ variable; ie this will
     not work:

           foreach qw\Uno Due Tres Quadro\ {
               print "Set $_\n";
           }

    -Merged Windows version detection code sent by Yves Orton.  Perltidy
     now automatically turns off syntax checking for Win 9x/ME versions,
     and this has solved a lot of robustness problems.  These systems 
     cannot reliably handle backtick operators.  See man page for
     details.
     
    -Merged VMS filename handling patch sent by Michael Cartmell.  (Invalid
     output filenames were being created in some cases). 

    -Numerous minor improvements have been made for -lp style indentation.

    -Long C-style 'for' expressions will be broken after each ';'.   

     'perltidy -gnu' gives:

       OLD:
       for ($status = $db->seq($key, $value, R_CURSOR()) ; $status == 0
            and $key eq $origkey ; $status = $db->seq($key, $value, R_NEXT())) 

       NEW:
       for ($status = $db->seq($key, $value, R_CURSOR()) ;
            $status == 0 and $key eq $origkey ;
            $status = $db->seq($key, $value, R_NEXT()))

    -For the -lp option, a single long term within parens
     (without commas) now has better alignment.  For example,
     perltidy -gnu

               OLD:
               $self->throw("Must specify a known host, not $location,"
                     . " possible values ("
                     . join (",", sort keys %hosts) . ")");

               NEW:
               $self->throw("Must specify a known host, not $location,"
                            . " possible values ("
                            . join (",", sort keys %hosts) . ")");

## 2001 12 31

    -This version is about 20 percent faster than the previous
     version as a result of optimization work.  The largest gain
     came from switching to a dispatch hash table in the
     tokenizer.

    -perltidy -html will check to see if HTML::Entities is
     installed, and if so, it will use it to encode unsafe
     characters.

    -Added flag -oext=ext to change the output file extension to
     be different from the default ('tdy' or 'html').  For
     example:

       perltidy -html -oext=htm filename

    will produce filename.htm

    -Added flag -cscw to issue warnings if a closing side comment would replace
    an existing, different side comments.  See the man page for details.
    Thanks to Peter Masiar for helpful discussions.

    -Corrected tokenization error of signed hex/octal/binary numbers. For
    example, the first hex number below would have been parsed correctly
    but the second one was not:
       if ( ( $tmp >= 0x80_00_00 ) || ( $tmp < -0x80_00_00 ) ) { }

    -'**=' was incorrectly tokenized as '**' and '='.  This only
        caused a problem with the -extrude option.

    -Corrected a divide by zero when -extrude option is used

    -The flag -w will now contain all errors reported by 'perl -c' on the
    input file, but otherwise they are not reported.  The reason is that
    perl will report lots of problems and syntax errors which are not of
    interest when only a small snippet is being formatted (such as missing
    modules and unknown bare words).  Perltidy will always report all
    significant syntax errors that it finds, such as unbalanced braces,
    unless the -q (quiet) flag is set.

    -Merged modifications created by Hugh Myers into perltidy.
     These include a 'streamhandle' routine which allows perltidy
     as a module to operate on input and output arrays and strings
     in addition to files.  Documentation and new packaging as a
     module should be ready early next year; This is an elegant,
     powerful update; many thanks to Hugh for contributing it.

## 2001 11 28

    -added a tentative patch which tries to keep any existing breakpoints
    at lines with leading keywords map,sort,eval,grep. The idea is to
    improve formatting of sequences of list operations, as in a schwartzian
    transform.  Example:

       INPUT:
       my @sorted = map { $_->[0] }
                    sort { $a->[1] <=> $b->[1] }
                    map { [ $_, rand ] } @list;

       OLD:
       my @sorted =
         map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;

       NEW:
       my @sorted = map { $_->[0] }
         sort { $a->[1] <=> $b->[1] }
         map { [ $_, rand ] } @list;

     The new alignment is not as nice as the input, but this is an improvement.
     Thanks to Yves Orton for this suggestion.

    -modified indentation logic so that a line with leading opening paren,
    brace, or square bracket will never have less indentation than the
    line with the corresponding opening token.  Here's a simple example:

       OLD:
           $mw->Button(
               -text    => "New Document",
               -command => \&new_document
             )->pack(
               -side   => 'bottom',
               -anchor => 'e'
           );

       Note how the closing ');' is lined up with the first line, even
       though it closes a paren in the 'pack' line.  That seems wrong.
    
       NEW:
           $mw->Button(
               -text    => "New Document",
               -command => \&new_document
             )->pack(
               -side   => 'bottom',
               -anchor => 'e'
             );

      This seems nicer: you can up-arrow with an editor and arrive at the
      opening 'pack' line.
    
    -corrected minor glitch in which cuddled else (-ce) did not get applied
    to an 'unless' block, which should look like this:

           unless ($test) {

           } else {

           }

     Thanks to Jeremy Mates for reporting this.

    -The man page has been reorganized to parameters easier to find.
    
    -Added check for multiple definitions of same subroutine.  It is easy
     to introduce this problem when cutting and pasting. Perl does not
     complain about it, but it can lead to disaster.

    -The command -pro=filename  or -profile=filename may be used to specify a
     configuration file which will override the default name of .perltidyrc.
     There must not be a space on either side of the '=' sign.  I needed
     this to be able to easily test perltidy with a variety of different
     configuration files.

    -Side comment alignment has been improved somewhat across frequent level
     changes, as in short if/else blocks.  Thanks to Wolfgang Weisselberg 
     for pointing out this problem.  For example:
       
       OLD:
       if ( ref $self ) {    # Called as a method
           $format = shift;
       }
       else {    # Regular procedure call
           $format = $self;
           undef $self;
       }

       NEW:
       if ( ref $self ) {    # Called as a method
           $format = shift;
       }
       else {                # Regular procedure call
           $format = $self;
           undef $self;
       }

    -New command -ssc (--static-side-comment) and related command allows
     side comments to be spaced close to preceding character.  This is
     useful for displaying commented code as side comments.

    -New command -csc (--closing-side-comment) and several related
     commands allow comments to be added to (and deleted from) any or all
     closing block braces.  This can be useful if you have to maintain large
     programs, especially those that you didn't write.  See updated man page.
     Thanks to Peter Masiar for this suggestion.  For a simple example:

           perltidy -csc

           sub foo {
               if ( !defined( $_[0] ) ) {
                   print("Hello, World\n");
               }
               else {
                   print( $_[0], "\n" );
               }
           } ## end sub foo

     This added '## end sub foo' to the closing brace.  
     To remove it, perltidy -ncsc.

    -New commands -ola, for outdenting labels, and -okw, for outdenting
     selected control keywords, were implemented.  See the perltidy man
     page for details.  Thanks to Peter Masiar for this suggestion.

    -Hanging side comment change: a comment will not be considered to be a
     hanging side comment if there is no leading whitespace on the line.
     This should improve the reliability of identifying hanging side comments.
     Thanks to Peter Masiar for this suggestion.

    -Two new commands for outdenting, -olq (outdent-long-quotes) and -olc
     (outdent-long-comments), have been added.  The original -oll
     (outdent-long-lines) remains, and now is an abbreviation for -olq and -olc.
     The new default is just -olq.  This was necessary to avoid inconsistency with
     the new static block comment option.

    -Static block comments:  to provide a way to display commented code
     better, the convention is used that comments with a leading '##' should
     not be formatted as usual.  Please see '-sbc' (or '--static-block-comment')
     for documentation.  It can be deactivated with with -nsbc, but
     should not normally be necessary. Thanks to Peter Masiar for this 
     suggestion.

    -Two changes were made to help show structure of complex lists:
     (1) breakpoints are forced after every ',' in a list where any of
     the list items spans multiple lines, and
     (2) List items which span multiple lines now get continuation indentation.

     The following example illustrates both of these points.  Many thanks to
     Wolfgang Weisselberg for this snippet and a discussion of it; this is a
     significant formatting improvement. Note how it is easier to see the call
     parameters in the NEW version:

       OLD:
       assert( __LINE__, ( not defined $check )
           or ref $check
           or $check eq "new"
           or $check eq "old", "Error in parameters",
           defined $old_new ? ( ref $old_new ? ref $old_new : $old_new ) : "undef",
           defined $db_new  ? ( ref $db_new  ? ref $db_new  : $db_new )  : "undef",
           defined $old_db ? ( ref $old_db ? ref $old_db : $old_db ) : "undef" );

       NEW: 
       assert(
           __LINE__,
           ( not defined $check )
             or ref $check
             or $check eq "new"
             or $check eq "old",
           "Error in parameters",
           defined $old_new ? ( ref $old_new ? ref $old_new : $old_new ) : "undef",
           defined $db_new  ? ( ref $db_new  ? ref $db_new  : $db_new )  : "undef",
           defined $old_db  ? ( ref $old_db  ? ref $old_db  : $old_db )  : "undef"
       );

       Another example shows how this helps displaying lists:

       OLD:
       %{ $self->{COMPONENTS} } = (
           fname =>
           { type => 'name', adj => 'yes', font => 'Helvetica', 'index' => 0 },
           street =>
           { type => 'road', adj => 'yes', font => 'Helvetica', 'index' => 2 },
       );

       The structure is clearer with the added indentation:
       
       NEW:
       %{ $self->{COMPONENTS} } = (
           fname =>
             { type => 'name', adj => 'yes', font => 'Helvetica', 'index' => 0 },
           street =>
             { type => 'road', adj => 'yes', font => 'Helvetica', 'index' => 2 },
       );

       -The structure of nested logical expressions is now displayed better.
       Thanks to Wolfgang Weisselberg for helpful discussions.  For example,
       note how the status of the final 'or' is displayed in the following:

       OLD:
       return ( !null($op)
             and null( $op->sibling )
             and $op->ppaddr eq "pp_null"
             and class($op) eq "UNOP"
             and ( ( $op->first->ppaddr =~ /^pp_(and|or)$/
               and $op->first->first->sibling->ppaddr eq "pp_lineseq" )
               or ( $op->first->ppaddr eq "pp_lineseq"
                   and not null $op->first->first->sibling
                   and $op->first->first->sibling->ppaddr eq "pp_unstack" ) ) );

       NEW:
       return (
           !null($op)
             and null( $op->sibling )
             and $op->ppaddr eq "pp_null"
             and class($op) eq "UNOP"
             and (
               (
                   $op->first->ppaddr =~ /^pp_(and|or)$/
                   and $op->first->first->sibling->ppaddr eq "pp_lineseq"
               )
               or ( $op->first->ppaddr eq "pp_lineseq"
                   and not null $op->first->first->sibling
                   and $op->first->first->sibling->ppaddr eq "pp_unstack" )
             )
       );

      -A break will always be put before a list item containing a comma-arrow.
      This will improve formatting of mixed lists of this form:

           OLD:
           $c->create(
               'text', 225, 20, -text => 'A Simple Plot',
               -font => $font,
               -fill => 'brown'
           );

           NEW:
           $c->create(
               'text', 225, 20,
               -text => 'A Simple Plot',
               -font => $font,
               -fill => 'brown'
           );

     -For convenience, the command -dac (--delete-all-comments) now also
     deletes pod.  Likewise, -tac (--tee-all-comments) now also sends pod
     to a '.TEE' file.  Complete control over the treatment of pod and
     comments is still possible, as described in the updated help message 
     and man page.

     -The logic which breaks open 'containers' has been rewritten to be completely
     symmetric in the following sense: if a line break is placed after an opening
     {, [, or (, then a break will be placed before the corresponding closing
     token.  Thus, a container either remains closed or is completely cracked
     open.

     -Improved indentation of parenthesized lists.  For example, 

               OLD:
               $GPSCompCourse =
                 int(
                 atan2( $GPSTempCompLong - $GPSLongitude,
                 $GPSLatitude - $GPSTempCompLat ) * 180 / 3.14159265 );

               NEW:
               $GPSCompCourse = int(
                   atan2(
                       $GPSTempCompLong - $GPSLongitude,
                       $GPSLatitude - $GPSTempCompLat
                     ) * 180 / 3.14159265
               );

      Further improvements will be made in future releases.

     -Some improvements were made in formatting small lists.

     -Correspondence between Input and Output line numbers reported in a 
      .LOG file should now be exact.  They were sometimes off due to the size
      of intermediate buffers.

     -Corrected minor tokenization error in which a ';' in a foreach loop
      control was tokenized as a statement termination, which forced a 
      line break:

           OLD:
           foreach ( $i = 0;
               $i <= 10;
               $i += 2
             )
           {
               print "$i ";
           }

           NEW:
           foreach ( $i = 0 ; $i <= 10 ; $i += 2 ) {
               print "$i ";
           }

     -Corrected a problem with reading config files, in which quote marks were not
      stripped.  As a result, something like -wba="&& . || " would have the leading
      quote attached to the && and not work correctly.  A workaround for older
      versions is to place a space around all tokens within the quotes, like this:
      -wba=" && . || "

     -Removed any existing space between a label and its ':'
       OLD    : { }
       NEW: { }
      This was necessary because the label and its colon are a single token.

     -Corrected tokenization error for the following (highly non-recommended) 
      construct:
       $user = @vars[1] / 100;
    
     -Resolved cause of a difference between perltidy under perl v5.6.1 and
     5.005_03; the problem was different behavior of \G regex position
     marker(!)

## 2001 10 20

    -Corrected a bug in which a break was not being made after a full-line
    comment within a short eval/sort/map/grep block.  A flag was not being
    zeroed.  The syntax error check catches this.  Here is a snippet which
    illustrates the bug:

           eval {
               #open Socket to Dispatcher
               $sock = &OpenSocket;
           };

    The formatter mistakenly thought that it had found the following 
    one-line block:
    
           eval {#open Socket to Dispatcher$sock = &OpenSocket; };

    The patch fixes this. Many thanks to Henry Story for reporting this bug.

    -Changes were made to help diagnose and resolve problems in a
    .perltidyrc file: 
      (1) processing of command parameters has been into two separate
      batches so that any errors in a .perltidyrc file can be localized.  
      (2) commands --help, --version, and as many of the --dump-xxx
      commands are handled immediately, without any command line processing
      at all.  
      (3) Perltidy will ignore any commands in the .perltidyrc file which
      cause immediate exit.  These are:  -h -v -ddf -dln -dop -dsn -dtt
      -dwls -dwrs -ss.  Thanks to Wolfgang Weisselberg for helpful
      suggestions regarding these updates.

    -Syntax check has been reinstated as default for MSWin32 systems.  This
    way Windows 2000 users will get syntax check by default, which seems
    like a better idea, since the number of Win 95/98 systems will be
    decreasing over time.  Documentation revised to warn Windows 95/98
    users about the problem with empty '&1'.  Too bad these systems
    all report themselves as MSWin32.

## 2001 10 16

    -Fixed tokenization error in which a method call of the form

       Module::->new();
    
     got a space before the '::' like this:

       Module ::->new();

     Thanks to David Holden for reporting this.
    
    -Added -html control over pod text, using a new abbreviation 'pd'.  See
    updated perl2web man page. The default is to use the color of a comment,
    but italicized.  Old .css style sheets will need a new line for
    .pd to use this.  The old color was the color of a string, and there
    was no control.  
    
    -.css lines are now printed in sorted order.

    -Fixed interpolation problem where html files had '$input_file' as title
    instead of actual input file name.  Thanks to Simon Perreault for finding
    this and sending a patch, and also to Tobias Weber.

    -Breaks will now have the ':' placed at the start of a line, 
    one per line by default because this shows logical structure
    more clearly. This coding has been completely redone. Some 
    examples of new ?/: formatting:

          OLD:
               wantarray ? map( $dir::cwd->lookup($_)->path, @_ ) :
                 $dir::cwd->lookup( $_[0] )->path;

          NEW:
               wantarray 
                 ? map( $dir::cwd->lookup($_)->path, @_ )
                 : $dir::cwd->lookup( $_[0] )->path;

          OLD:
                   $a = ( $b > 0 ) ? {
                       a => 1,
                       b => 2
                   } : { a => 6, b => 8 };

          NEW:
                   $a = ( $b > 0 )
                     ? {
                       a => 1,
                       b => 2
                     }
                     : { a => 6, b => 8 };

       OLD: (-gnu):
       $self->note($self->{skip} ? "Hunk #$self->{hunk} ignored at 1.\n" :
                   "Hunk #$self->{hunk} failed--$@");

       NEW: (-gnu):
       $self->note($self->{skip} 
                   ? "Hunk #$self->{hunk} ignored at 1.\n"
                   : "Hunk #$self->{hunk} failed--$@");

       OLD:
           $which_search =
             $opts{"t"} ? 'title'   :
             $opts{"s"} ? 'subject' : $opts{"a"} ? 'author' : 'title';

       NEW:
           $which_search =
             $opts{"t"} ? 'title'
             : $opts{"s"} ? 'subject'
             : $opts{"a"} ? 'author'
             : 'title';
    
    You can use -wba=':' to recover the previous default which placed ':'
    at the end of a line.  Thanks to Michael Cartmell for helpful
    discussions and examples.  

    -Tokenizer updated to do syntax checking for matched ?/: pairs.  Also,
    the tokenizer now outputs a unique serial number for every balanced
    pair of brace types and ?/: pairs.  This greatly simplifies the
    formatter.

    -Long lines with repeated 'and', 'or', '&&', '||'  will now have
    one such item per line.  For example:

       OLD:
           if ( $opt_d || $opt_m || $opt_p || $opt_t || $opt_x
               || ( -e $archive && $opt_r ) )
           {
               ( $pAr, $pNames ) = readAr($archive);
           }

       NEW:
           if ( $opt_d
               || $opt_m
               || $opt_p
               || $opt_t
               || $opt_x
               || ( -e $archive && $opt_r ) )
           {
               ( $pAr, $pNames ) = readAr($archive);
           }

      OLD:
           if ( $vp->{X0} + 4 <= $x && $vp->{X0} + $vp->{W} - 4 >= $x
               && $vp->{Y0} + 4 <= $y && $vp->{Y0} + $vp->{H} - 4 >= $y ) 

      NEW:
           if ( $vp->{X0} + 4 <= $x
               && $vp->{X0} + $vp->{W} - 4 >= $x
               && $vp->{Y0} + 4 <= $y
               && $vp->{Y0} + $vp->{H} - 4 >= $y )

    -Long lines with multiple concatenated tokens will have concatenated
    terms (see below) placed one per line, except for short items.  For
    example:

      OLD:
           $report .=
             "Device type:" . $ib->family . "  ID:" . $ib->serial . "  CRC:"
             . $ib->crc . ": " . $ib->model() . "\n";

      NEW:
           $report .= "Device type:"
             . $ib->family . "  ID:"
             . $ib->serial . "  CRC:"
             . $ib->model()
             . $ib->crc . ": " . "\n";

    NOTE: at present 'short' means 8 characters or less.  There is a
    tentative flag to change this (-scl), but it is undocumented and
    is likely to be changed or removed later, so only use it for testing.  
    In the above example, the tokens "  ID:", "  CRC:", and "\n" are below
    this limit.  

    -If a line which is short enough to fit on a single line was
    nevertheless broken in the input file at a 'good' location (see below), 
    perltidy will try to retain a break.  For example, the following line
    will be formatted as:
    
       open SUM, "<$file"
         or die "Cannot open $file ($!)";
    
    if it was broken in the input file, and like this if not:

       open SUM, "<$file" or die "Cannot open $file ($!)";

    GOOD: 'good' location means before 'and','or','if','unless','&&','||'

    The reason perltidy does not just always break at these points is that if
    there are multiple, similar statements, this would preclude alignment.  So
    rather than check for this, perltidy just tries to follow the input style,
    in the hopes that the author made a good choice. Here is an example where 
    we might not want to break before each 'if':

       ($Locale, @Locale) = ($English, @English) if (@English > @Locale);
       ($Locale, @Locale) = ($German,  @German)  if (@German > @Locale);
       ($Locale, @Locale) = ($French,  @French)  if (@French > @Locale);
       ($Locale, @Locale) = ($Spanish, @Spanish) if (@Spanish > @Locale);

    -Added wildcard file expansion for systems with shells which lack this.
    Now 'perltidy *.pl' should work under MSDOS/Windows.  Thanks to Hugh Myers 
    for suggesting this.  This uses builtin glob() for now; I may change that.

    -Added new flag -sbl which, if specified, overrides the value of -bl
    for opening sub braces.  This allows formatting of this type:

    perltidy -sbl 

    sub foo
    {
       if (!defined($_[0])) {
           print("Hello, World\n");
       }
       else {
           print($_[0], "\n");
       }
    }
    Requested by Don Alexander.

    -Fixed minor parsing error which prevented a space after a $$ variable
    (pid) in some cases.  Thanks to Michael Cartmell for noting this.
    For example, 
      old: $$< 700 
      new: $$ < 700

    -Improved line break choices 'and' and 'or' to display logic better.
    For example:

       OLD:
           exists $self->{'build_dir'} and push @e,
             "Unwrapped into directory $self->{'build_dir'}";

       NEW:
           exists $self->{'build_dir'}
             and push @e, "Unwrapped into directory $self->{'build_dir'}";

    -Fixed error of multiple use of abbreviatioin '-dsc'.  -dsc remains 
    abbreviation for delete-side-comments; -dsm is new abbreviation for 
    delete-semicolons.

    -Corrected and updated 'usage' help routine.  Thanks to Slaven Rezic for 
    noting an error.

    -The default for Windows is, for now, not to do a 'perl -c' syntax
    check (but -syn will activate it).  This is because of problems with
    command.com.  James Freeman sent me a patch which tries to get around
    the problems, and it works in many cases, but testing revealed several
    issues that still need to be resolved.  So for now, the default is no
    syntax check for Windows.

    -I added a -T flag when doing perl -c syntax check.
    This is because I test it on a large number of scripts from sources
    unknown, and who knows what might be hidden in initialization blocks?
    Also, deactivated the syntax check if perltidy is run as root.  As a
    benign example, running the previous version of perltidy on the
    following file would cause it to disappear:

           BEGIN{
                   print "Bye, bye baby!\n";
                   unlink $0;
           }
           
    The new version will not let that happen.

    -I am contemplating (but have not yet implemented) making '-lp' the
    default indentation, because it is stable now and may be closer to how
    perl is commonly formatted.  This could be in the next release.  The
    reason that '-lp' was not the original default is that the coding for
    it was complex and not ready for the initial release of perltidy.  If
    anyone has any strong feelings about this, I'd like to hear.  The
    current default could always be recovered with the '-nlp' flag.  

## 2001 09 03 

    -html updates:
        - sub definition names are now specially colored, red by default.  
          The letter 'm' is used to identify them.
        - keyword 'sub' now has color of other keywords.
        - restored html keyword color to __END__ and __DATA__, which was 
          accidentally removed in the previous version.

    -A new -se (--standard-error-output) flag has been implemented and
    documented which causes all errors to be written to standard output
    instead of a .ERR file.

    -A new -w (--warning-output) flag has been implemented and documented
     which causes perltidy to output certain non-critical messages to the
     error output file, .ERR.  These include complaints about pod usage,
     for example.  The default is to not include these.

     NOTE: This replaces an undocumented -w=0 or --warning-level flag
     which was tentatively introduced in the previous version to avoid some
     unwanted messages.  The new default is the same as the old -w=0, so
     that is no longer needed. 

     -Improved syntax checking and corrected tokenization of functions such
     as rand, srand, sqrt, ...  These can accept either an operator or a term
     to their right.  This has been corrected.
    
    -Corrected tokenization of semicolon: testing of the previous update showed 
    that the semicolon in the following statement was being mis-tokenized.  That
    did no harm, other than adding an extra blank space, but has been corrected.

             for (sort {strcoll($a,$b);} keys %investments) {
                ...
             }

    -New syntax check: after wasting 5 minutes trying to resolve a syntax
     error in which I had an extra terminal ';' in a complex for (;;) statement, 
     I spent a few more minutes adding a check for this in perltidy so it won't
     happen again.

    -The behavior of --break-before-subs (-bbs) and --break-before-blocks
    (-bbb) has been modified.  Also, a new control parameter,
    --long-block-line-count=n (-lbl=n) has been introduced to give more
    control on -bbb.  This was previously a hardwired value.  The reason
    for the change is to reduce the number of unwanted blank lines that
    perltidy introduces, and make it less erratic.  It's annoying to remove
    an unwanted blank line and have perltidy put it back.  The goal is to
    be able to sprinkle a few blank lines in that dense script you
    inherited from Bubba.  I did a lot of experimenting with different
    schemes for introducing blank lines before and after code blocks, and
    decided that there is no really good way to do it.  But I think the new
    scheme is an improvement.  You can always deactivate this with -nbbb.
    I've been meaning to work on this; thanks to Erik Thaysen for bringing
    it to my attention.

    -The .LOG file is seldom needed, and I get tired of deleting them, so
     they will now only be automatically saved if perltidy thinks that it
     made an error, which is almost never.  You can still force the logfile
     to be saved with -log or -g.

    -Improved method for computing number of columns in a table.  The old
    method always tried for an even number.  The new method allows odd
    numbers when it is obvious that a list is not a hash initialization
    list.

      old: my (
                $name,       $xsargs, $parobjs, $optypes,
                $hasp2child, $pmcode, $hdrcode, $inplacecode,
                $globalnew,  $callcopy
             )
             = @_;

      new: my (
                $name,   $xsargs,  $parobjs,     $optypes,   $hasp2child,
                $pmcode, $hdrcode, $inplacecode, $globalnew, $callcopy
             )
             = @_;

    -I fiddled with the list threshold adjustment, and some small lists
    look better now.  Here is the change for one of the lists in test file
    'sparse.t':
    old:
      %units =
        ("in", "in", "pt", "pt", "pc", "pi", "mm", "mm", "cm", "cm", "\\hsize", "%",
          "\\vsize", "%", "\\textwidth", "%", "\\textheight", "%");

    new:
      %units = (
                 "in",      "in", "pt",          "pt", "pc",           "pi",
                 "mm",      "mm", "cm",          "cm", "\\hsize",      "%",
                 "\\vsize", "%",  "\\textwidth", "%",  "\\textheight", "%"
                 );

    -Improved -lp formatting at '=' sign.  A break was always being added after
    the '=' sign in a statement such as this, (to be sure there was enough room
    for the parameters):

    old: my $fee =
           CalcReserveFee(
                           $env,          $borrnum,
                           $biblionumber, $constraint,
                           $bibitems
                           );
    
    The updated version doesn't do this unless the space is really needed:

    new: my $fee = CalcReserveFee(
                                  $env,          $borrnum,
                                  $biblionumber, $constraint,
                                  $bibitems
                                  );

    -I updated the tokenizer to allow $#+ and $#-, which seem to be new to
    Perl 5.6.  Some experimenting with a recent version of Perl indicated
    that it allows these non-alphanumeric '$#' array maximum index
    variables: $#: $#- $#+ so I updated the parser accordingly.  Only $#:
    seems to be valid in older versions of Perl.

    -Fixed a rare formatting problem with -lp (and -gnu) which caused
    excessive indentation.

    -Many additional syntax checks have been added.

    -Revised method for testing here-doc target strings; the following
    was causing trouble with a regex test because of the '*' characters:
     print <<"*EOF*";
     bla bla
     *EOF*
    Perl seems to allow almost anything to be a here doc target, so an
    exact string comparison is now used.

    -Made update to allow underscores in binary numbers, like '0b1100_0000'.

    -Corrected problem with scanning certain module names; a blank space was 
    being inserted after 'warnings' in the following:
       use warnings::register;
    The problem was that warnings (and a couple of other key modules) were 
    being tokenized as keywords.  They should have just been identifiers.

    -Corrected tokenization of indirect objects after sort, system, and exec,
    after testing produced an incorrect error message for the following
    line of code:
       print sort $sortsubref @list;

    -Corrected minor problem where a line after a format had unwanted
    extra continuation indentation.  

    -Delete-block-comments (and -dac) now retain any leading hash-bang line

    -Update for -lp (and -gnu) to not align the leading '=' of a list
    with a previous '=', since this interferes with alignment of parameters.

     old:  my $hireDay = new Date;
           my $self    = {
                        firstName => undef,
                        lastName  => undef,
                        hireDay   => $hireDay
                        };
       
     new:  my $hireDay = new Date;
           my $self = {
                        firstName => undef,
                        lastName  => undef,
                        hireDay   => $hireDay
                        };

    -Modifications made to display tables more compactly when possible,
     without adding lines. For example,
     old:
                   '1', "I", '2', "II", '3', "III", '4', "IV",
                   '5', "V", '6', "VI", '7', "VII", '8', "VIII",
                   '9', "IX"
     new:
                   '1', "I",   '2', "II",   '3', "III",
                   '4', "IV",  '5', "V",    '6', "VI",
                   '7', "VII", '8', "VIII", '9', "IX"

    -Corrected minor bug in which -pt=2 did not keep the right paren tight
    around a '++' or '--' token, like this:

               for ($i = 0 ; $i < length $key ; $i++ )

    The formatting for this should be, and now is: 

               for ($i = 0 ; $i < length $key ; $i++)

    Thanks to Erik Thaysen for noting this.

    -Discovered a new bug involving here-docs during testing!  See BUGS.html.  

    -Finally fixed parsing of subroutine attributes (A Perl 5.6 feature).
    However, the attributes and prototypes must still be on the same line
    as the sub name.

## 2001 07 31 

    -Corrected minor, uncommon bug found during routine testing, in which a
    blank got inserted between a function name and its opening paren after
    a file test operator, but only in the case that the function had not
    been previously seen.  Perl uses the existence (or lack thereof) of 
    the blank to guess if it is a function call.  That is,
       if (-l pid_filename()) {
    became
       if (-l pid_filename ()) {
    which is a syntax error if pid_filename has not been seen by perl.

    -If the AutoLoader module is used, perltidy will continue formatting
    code after seeing an __END__ line.  Use -nlal to deactivate this feature.  
    Likewise, if the SelfLoader module is used, perltidy will continue 
    formatting code after seeing a __DATA__ line.  Use -nlsl to
    deactivate this feature.  Thanks to Slaven Rezic for this suggestion.

    -pod text after __END__ and __DATA__ is now identified by perltidy
    so that -dp works correctly.  Thanks to Slaven Rezic for this suggestion.

    -The first $VERSION line which might be eval'd by MakeMaker
    is now passed through unchanged.  Use -npvl to deactivate this feature.
    Thanks to Manfred Winter for this suggestion.

    -Improved indentation of nested parenthesized expressions.  Tests have
    given favorable results.  Thanks to Wolfgang Weisselberg for helpful
    examples.

## 2001 07 23 

    -Fixed a very rare problem in which an unwanted semicolon was inserted
    due to misidentification of anonymous hash reference curly as a code
    block curly.  (No instances of this have been reported; I discovered it
    during testing).  A workaround for older versions of perltidy is to use
    -nasc.

    -Added -icb (-indent-closing-brace) parameter to indent a brace which
    terminates a code block to the same level as the previous line.
    Suggested by Andrew Cutler.  For example, 

           if ($task) {
               yyy();
               }    # -icb
           else {
               zzz();
               }

    -Rewrote error message triggered by an unknown bareword in a print or
    printf filehandle position, and added flag -w=0 to prevent issuing this
    error message.  Suggested by Byron Jones.

    -Added modification to align a one-line 'if' block with similar
    following 'elsif' one-line blocks, like this:
         if    ( $something eq "simple" )  { &handle_simple }
         elsif ( $something eq "hard" )    { &handle_hard }
    (Suggested by  Wolfgang Weisselberg).

## 2001 07 02 

    -Eliminated all constants with leading underscores because perl 5.005_03
    does not support that.  For example, _SPACES changed to XX_SPACES.
    Thanks to kromJx for this update.

## 2001 07 01 

    -the directory of test files has been moved to a separate distribution
    file because it is getting large but is of little interest to most users.
    For the current distribution:
      perltidy-20010701.tgz        contains the source and docs for perltidy
      perltidy-20010701-test.tgz   contains the test files

    -fixed bug where temporary file perltidy.TMPI was not being deleted 
    when input was from stdin.

    -adjusted line break logic to not break after closing brace of an
    eval block (suggested by Boris Zentner).

    -added flag -gnu (--gnu-style) to give an approximation to the GNU
    style as sometimes applied to perl.  The programming style in GNU
    'automake' was used as a guide in setting the parameters; these
    parameters will probably be adjusted over time.

    -an empty code block now has one space for emphasis:
      if ( $cmd eq "bg_untested" ) {}    # old
      if ( $cmd eq "bg_untested" ) { }   # new
    If this bothers anyone, we could create a parameter.

    -the -bt (--brace-tightness) parameter has been split into two
    parameters to give more control. -bt now applies only to non-BLOCK
    braces, while a new parameter -bbt (block-brace-tightness) applies to
    curly braces which contain code BLOCKS. The default value is -bbt=0.

    -added flag -icp (--indent-closing-paren) which leaves a statement
    termination of the form );, };, or ]; indented with the same
    indentation as the previous line.  For example,

       @month_of_year = (          # default, or -nicp
           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
           'Nov', 'Dec'
       );

       @month_of_year = (          # -icp
           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
           'Nov', 'Dec'
           );

    -Vertical alignment updated to synchronize with tokens &&, ||,
    and, or, if, unless.  Allowable space before forcing
    resynchronization has been increased.  (Suggested by  Wolfgang
    Weisselberg).

    -html corrected to use -nohtml-bold-xxxxxxx or -nhbx to negate bold,
    and likewise -nohtml-italic-xxxxxxx or -nhbi to negate italic.  There
    was no way to negate these previously.  html documentation updated and
    corrected.  (Suggested by  Wolfgang Weisselberg).

    -Some modifications have been made which improve the -lp formatting in
    a few cases.

    -Perltidy now retains or creates a blank line after an =cut to keep
    podchecker happy (Suggested by Manfred H. Winter).  This appears to be
    a glitch in podchecker, but it was annoying.

## 2001 06 17  

    -Added -bli flag to give continuation indentation to braces, like this

           if ($bli_flag)
             {
               extra_indentation();
             }

    -Corrected an error with the tab (-t) option which caused the last line
    of a multi-line quote to receive a leading tab.  This error was in
    version 2001 06 08  but not 2001 04 06.  If you formatted a script
    with -t with this version, please check it by running once with the
    -chk flag and perltidy will scan for this possible error.

    -Corrected an invalid pattern (\R should have been just R), changed
    $^W =1 to BEGIN {$^W=1} to use warnings in compile phase, and corrected
    several unnecessary 'my' declarations. Many thanks to Wolfgang Weisselberg,
    2001-06-12, for catching these errors.
    
    -A '-bar' flag has been added to require braces to always be on the
    right, even for multi-line if and foreach statements.  For example,
    the default formatting of a long if statement would be:

           if ($bigwasteofspace1 && $bigwasteofspace2
             || $bigwasteofspace3 && $bigwasteofspace4)
           {
               bigwastoftime();
           }

    With -bar, the formatting is:

           if ($bigwasteofspace1 && $bigwasteofspace2
             || $bigwasteofspace3 && $bigwasteofspace4) {
               bigwastoftime();
           }
    Suggested by Eli Fidler 2001-06-11.

    -Uploaded perltidy to sourceforge cvs 2001-06-10.

    -An '-lp' flag (--line-up-parentheses) has been added which causes lists
    to be indented with extra indentation in the manner sometimes
    associated with emacs or the GNU suggestions.  Thanks to Ian Stuart for
    this suggestion and for extensive help in testing it. 

    -Subroutine call parameter lists are now formatted as other lists.
    This should improve formatting of tables being passed via subroutine
    calls.  This will also cause full indentation ('-i=n, default n= 4) of
    continued parameter list lines rather than just the number of spaces
    given with -ci=n, default n=2.
    
    -Added support for hanging side comments.  Perltidy identifies a hanging
    side comment as a comment immediately following a line with a side
    comment or another hanging side comment.  This should work in most
    cases.  It can be deactivated with --no-hanging-side-comments (-nhsc).
    The manual has been updated to discuss this.  Suggested by Brad
    Eisenberg some time ago, and finally implemented.

## 2001 06 08  

    -fixed problem with parsing command parameters containing quoted
    strings in .perltidyrc files. (Reported by Roger Espel Llima 2001-06-07).

    -added two command line flags, --want-break-after and 
    --want-break-before, which allow changing whether perltidy
    breaks lines before or after any operators.  Please see the revised 
    man pages for details.

    -added system-wide configuration file capability.
    If perltidy does not find a .perltidyrc command line file in
    the current directory, nor in the home directory, it now looks
    for '/usr/local/etc/perltidyrc' and then for '/etc/perltidyrc'.
    (Suggested by Roger Espel Llima 2001-05-31).

    -fixed problem in which spaces were trimmed from lines of a multi-line
    quote. (Reported by Roger Espel Llima 2001-05-30).  This is an 
    uncommon situation, but serious, because it could conceivably change
    the proper function of a script.

    -fixed problem in which a semicolon was incorrectly added within 
    an anonymous hash.  (Reported by A.C. Yardley, 2001-5-23).
    (You would know if this happened, because perl would give a syntax
    error for the resulting script).

    -fixed problem in which an incorrect error message was produced
     after a version number on a 'use' line, like this ( Reported 
     by Andres Kroonmaa, 2001-5-14):

                 use CGI 2.42 qw(fatalsToBrowser);

     Other than the extraneous error message, this bug was harmless.

## 2001 04 06 

    -fixed serious bug in which the last line of some multi-line quotes or
     patterns was given continuation indentation spaces.  This may make
     a pattern incorrect unless it uses the /x modifier.  To find
     instances of this error in scripts which have been formatted with
     earlier versions of perltidy, run with the -chk flag, which has
     been added for this purpose (SLH, 2001-04-05).

     ** So, please check previously formatted scripts by running with -chk
     at least once **

    -continuation indentation has been reprogrammed to be hierarchical, 
     which improves deeply nested structures.

    -fixed problem with undefined value in list formatting (reported by Michael
     Langner 2001-04-05)

    -Switched to graphical display of nesting in .LOG files.  If an
     old format string was "(1 [0 {2", the new string is "{{(".  This
     is easier to read and also shows the order of nesting.

    -added outdenting of cuddled paren structures, like  ")->pack(".

    -added line break and outdenting of ')->' so that instead of

           $mw->Label(
             -text   => "perltidy",
             -relief => 'ridge')->pack;
    
     the current default is:

           $mw->Label(
             -text   => "perltidy",
             -relief => 'ridge'
           )->pack;

     (requested by Michael Langner 2001-03-31; in the future this could 
     be controlled by a command-line parameter).

    -revised list indentation logic, so that lists following an assignment
     operator get one full indentation level, rather than just continuation 
     indentation.  Also corrected some minor glitches in the continuation 
     indentation logic. 

    -Fixed problem with unwanted continuation indentation after a blank line 
    (reported by Erik Thaysen 2001-03-28):

    -minor update to avoid stranding a single '(' on one line

## 2001 03 28:

    -corrected serious error tokenizing filehandles, in which a sub call 
    after a print or printf, like this:
       print usage() and exit;
    became this:
       print usage () and exit;
    Unfortunately, this converts 'usage' to a filehandle.  To fix this, rerun
    perltidy; it will look for this situation and issue a warning. 

    -fixed another cuddled-else formatting bug (Reported by Craig Bourne)

    -added several diagnostic --dump routines
    
    -added token-level whitespace controls (suggested by Hans Ecke)

## 2001 03 23:

    -added support for special variables of the form ${^WANT_BITS}

    -space added between scalar and left paren in 'for' and 'foreach' loops,
     (suggestion by Michael Cartmell):

       for $i( 1 .. 20 )   # old
       for $i ( 1 .. 20 )   # new

    -html now outputs cascading style sheets (thanks to suggestion from
     Hans Ecke)

    -flags -o and -st now work with -html

    -added missing -html documentation for comments (noted by Alex Izvorski)

    -support for VMS added (thanks to Michael Cartmell for code patches and 
      testing)

    -v-strings implemented (noted by Hans Ecke and Michael Cartmell; extensive
      testing by Michael Cartmell)

    -fixed problem where operand may be empty at line 3970 
     (\b should be just b in lines 3970, 3973) (Thanks to Erik Thaysen, 
     Keith Marshall for bug reports)

    -fixed -ce bug (cuddled else), where lines like '} else {' were indented
     (Thanks to Shawn Stepper and Rick Measham for reporting this)

## 2001 03 04:

    -fixed undefined value in line 153 (only worked with -I set)
    (Thanks to Mike Stok, Phantom of the Opcodes, Ian Ehrenwald, and others)

    -fixed undefined value in line 1069 (filehandle problem with perl versions <
    5.6) (Thanks to Yuri Leikind, Mike Stok, Michael Holve, Jeff Kolber)

## 2001 03 03:

    -Initial announcement at freshmeat.net; started Change Log
    (Unfortunately this version was DOA, but it was fixed the next day)
