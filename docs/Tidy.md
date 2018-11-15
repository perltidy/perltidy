# NAME

Perl::Tidy - Parses and beautifies perl source

# SYNOPSIS

    use Perl::Tidy;

    my $error_flag = Perl::Tidy::perltidy(
        source            => $source,
        destination       => $destination,
        stderr            => $stderr,
        argv              => $argv,
        perltidyrc        => $perltidyrc,
        logfile           => $logfile,
        errorfile         => $errorfile,
        formatter         => $formatter,           # callback object (see below)
        dump_options      => $dump_options,
        dump_options_type => $dump_options_type,
        prefilter         => $prefilter_coderef,
        postfilter        => $postfilter_coderef,
    );

# DESCRIPTION

This module makes the functionality of the perltidy utility available to perl
scripts.  Any or all of the input parameters may be omitted, in which case the
@ARGV array will be used to provide input parameters as described
in the perltidy(1) man page.

For example, the perltidy script is basically just this:

    use Perl::Tidy;
    Perl::Tidy::perltidy();

The call to **perltidy** returns a scalar **$error\_flag** which is TRUE if an
error caused premature termination, and FALSE if the process ran to normal
completion.  Additional discuss of errors is contained below in the [ERROR
HANDLING](https://metacpan.org/pod/ERROR
HANDLING) section.

The module accepts input and output streams by a variety of methods.
The following list of parameters may be any of the following: a
filename, an ARRAY reference, a SCALAR reference, or an object with
either a **getline** or **print** method, as appropriate.

        source            - the source of the script to be formatted
        destination       - the destination of the formatted output
        stderr            - standard error output
        perltidyrc        - the .perltidyrc file
        logfile           - the .LOG file stream, if any 
        errorfile         - the .ERR file stream, if any
        dump_options      - ref to a hash to receive parameters (see below), 
        dump_options_type - controls contents of dump_options
        dump_getopt_flags - ref to a hash to receive Getopt flags
        dump_options_category - ref to a hash giving category of options
        dump_abbreviations    - ref to a hash giving all abbreviations

The following chart illustrates the logic used to decide how to
treat a parameter.

    ref($param)  $param is assumed to be:
    -----------  ---------------------
    undef        a filename
    SCALAR       ref to string
    ARRAY        ref to array
    (other)      object with getline (if source) or print method

If the parameter is an object, and the object has a **close** method, that
close method will be called at the end of the stream.

- source

    If the **source** parameter is given, it defines the source of the input stream.
    If an input stream is defined with the **source** parameter then no other source
    filenames may be specified in the @ARGV array or **argv** parameter.

- destination

    If the **destination** parameter is given, it will be used to define the
    file or memory location to receive output of perltidy.  

- stderr

    The **stderr** parameter allows the calling program to redirect the stream that
    would otherwise go to the standard error output device to any of the stream
    types listed above.  This stream contains important warnings and errors 
    related to the parameters passed to perltidy.

- perltidyrc

    If the **perltidyrc** file is given, it will be used instead of any
    `.perltidyrc` configuration file that would otherwise be used. 

- errorfile

    The **errorfile** parameter allows the calling program to capture
    the stream that would otherwise go to either a .ERR file.  This
    stream contains warnings or errors related to the contents of one
    source file or stream. 

    The reason that this is different from the stderr stream is that when perltidy
    is called to process multiple files there will be up to one .ERR file created
    for each file and it would be very confusing if they were combined.  

    However if perltidy is called to process just a single perl script then it may
    be more convenient to combine the **errorfile** stream with the **stderr**
    stream.  This can be done by setting the **-se** parameter, in which case this
    parameter is ignored.

- logfile

    The **logfile** parameter allows the calling program to capture the log stream.
    This stream is only created if requested with a **-g** parameter.  It contains
    detailed diagnostic information about a script which may be useful for
    debugging.

- argv

    If the **argv** parameter is given, it will be used instead of the
    **@ARGV** array.  The **argv** parameter may be a string, a reference to a
    string, or a reference to an array.  If it is a string or reference to a
    string, it will be parsed into an array of items just as if it were a
    command line string.

- dump\_options

    If the **dump\_options** parameter is given, it must be the reference to a hash.
    In this case, the parameters contained in any perltidyrc configuration file
    will be placed in this hash and perltidy will return immediately.  This is
    equivalent to running perltidy with --dump-options, except that the perameters
    are returned in a hash rather than dumped to standard output.  Also, by default
    only the parameters in the perltidyrc file are returned, but this can be
    changed (see the next parameter).  This parameter provides a convenient method
    for external programs to read a perltidyrc file.  An example program using
    this feature, `perltidyrc_dump.pl`, is included in the distribution.

    Any combination of the **dump\_** parameters may be used together.

- dump\_options\_type

    This parameter is a string which can be used to control the parameters placed
    in the hash reference supplied by **dump\_options**.  The possible values are
    'perltidyrc' (default) and 'full'.  The 'full' parameter causes both the
    default options plus any options found in a perltidyrc file to be returned.

- dump\_getopt\_flags

    If the **dump\_getopt\_flags** parameter is given, it must be the reference to a
    hash.  This hash will receive all of the parameters that perltidy understands
    and flags that are passed to Getopt::Long.  This parameter may be
    used alone or with the **dump\_options** flag.  Perltidy will
    exit immediately after filling this hash.  See the demo program
    `perltidyrc_dump.pl` for example usage.

- dump\_options\_category

    If the **dump\_options\_category** parameter is given, it must be the reference to a
    hash.  This hash will receive a hash with keys equal to all long parameter names
    and values equal to the title of the corresponding section of the perltidy manual.
    See the demo program `perltidyrc_dump.pl` for example usage.

- dump\_abbreviations

    If the **dump\_abbreviations** parameter is given, it must be the reference to a
    hash.  This hash will receive all abbreviations used by Perl::Tidy.  See the
    demo program `perltidyrc_dump.pl` for example usage.

- prefilter

    A code reference that will be applied to the source before tidying. It is
    expected to take the full content as a string in its input, and output the
    transformed content.

- postfilter

    A code reference that will be applied to the tidied result before outputting.
    It is expected to take the full content as a string in its input, and output
    the transformed content.

    Note: A convenient way to check the function of your custom prefilter and
    postfilter code is to use the --notidy option, first with just the prefilter
    and then with both the prefilter and postfilter.  See also the file
    **filter\_example.pl** in the perltidy distribution.

# ERROR HANDLING

Perltidy will return with an error flag indicating if the process had to be
terminated early due to errors in the input parameters.  This can happen for
example if a parameter is misspelled or given an invalid value.  The calling
program should check this flag because if it is set the destination stream will
be empty or incomplete and should be ignored.  Error messages in the **stderr**
stream will indicate the cause of any problem.  

If the error flag is not set then perltidy ran to completion.   However there
may still be warning messages in the **stderr** stream related to control
parameters, and there may be warning messages in the **errorfile** stream
relating to possible syntax errors in the source code being tidied.  

In the event of a catastrophic error for which recovery is not possible
**perltidy** terminates by making calls to **croak** or **confess** to help the
programmer localize the problem.  These should normally only occur during
program development.  

# NOTES ON FORMATTING PARAMETERS

Parameters which control formatting may be passed in several ways: in a
`.perltidyrc` configuration file, in the **perltidyrc** parameter, and in the
**argv** parameter.

The **-syn** (**--check-syntax**) flag may be used with all source and
destination streams except for standard input and output.  However 
data streams which are not associated with a filename will 
be copied to a temporary file before being be passed to Perl.  This
use of temporary files can cause somewhat confusing output from Perl.

If the **-pbp** style is used it will typically be necessary to also
specify a **-nst** flag.  This is necessary to turn off the **-st** flag
contained in the **-pbp** parameter set which otherwise would direct
the output stream to the standard output.  

# EXAMPLES

The following example uses string references to hold the input and output
code and error streams, and illustrates checking for errors.

    use Perl::Tidy;
    
    my $source_string = <<'EOT';
    my$error=Perl::Tidy::perltidy(argv=>$argv,source=>\$source_string,
      destination=>\$dest_string,stderr=>\$stderr_string,
    errorfile=>\$errorfile_string,);
    EOT
    
    my $dest_string;
    my $stderr_string;
    my $errorfile_string;
    my $argv = "-npro";   # Ignore any .perltidyrc at this site
    $argv .= " -pbp";     # Format according to perl best practices
    $argv .= " -nst";     # Must turn off -st in case -pbp is specified
    $argv .= " -se";      # -se appends the errorfile to stderr
    ## $argv .= " --spell-check";  # uncomment to trigger an error
    
    print "<<RAW SOURCE>>\n$source_string\n";
    
    my $error = Perl::Tidy::perltidy(
        argv        => $argv,
        source      => \$source_string,
        destination => \$dest_string,
        stderr      => \$stderr_string,
        errorfile   => \$errorfile_string,    # ignored when -se flag is set
        ##phasers   => 'stun',                # uncomment to trigger an error
    );
    
    if ($error) {
    
        # serious error in input parameters, no tidied output
        print "<<STDERR>>\n$stderr_string\n";
        die "Exiting because of serious errors\n";
    }
    
    if ($dest_string)      { print "<<TIDIED SOURCE>>\n$dest_string\n" }
    if ($stderr_string)    { print "<<STDERR>>\n$stderr_string\n" }
    if ($errorfile_string) { print "<<.ERR file>>\n$errorfile_string\n" }

Additional examples are given in examples section of the perltidy distribution.  

# Using the **formatter** Callback Object

The **formatter** parameter is an optional callback object which allows
the calling program to receive tokenized lines directly from perltidy for
further specialized processing.  When this parameter is used, the two
formatting options which are built into perltidy (beautification or
html) are ignored.  The following diagram illustrates the logical flow:

                      |-- (normal route)   -> code beautification
    caller->perltidy->|-- (-html flag )    -> create html 
                      |-- (formatter given)-> callback to write_line

This can be useful for processing perl scripts in some way.  The 
parameter `$formatter` in the perltidy call,

        formatter   => $formatter,  

is an object created by the caller with a `write_line` method which
will accept and process tokenized lines, one line per call.  Here is
a simple example of a `write_line` which merely prints the line number,
the line type (as determined by perltidy), and the text of the line:

    sub write_line {
    
        # This is called from perltidy line-by-line
        my $self              = shift;
        my $line_of_tokens    = shift;
        my $line_type         = $line_of_tokens->{_line_type};
        my $input_line_number = $line_of_tokens->{_line_number};
        my $input_line        = $line_of_tokens->{_line_text};
        print "$input_line_number:$line_type:$input_line";
    }

The complete program, **perllinetype**, is contained in the examples section of
the source distribution.  As this example shows, the callback method
receives a parameter **$line\_of\_tokens**, which is a reference to a hash
of other useful information.  This example uses these hash entries:

    $line_of_tokens->{_line_number} - the line number (1,2,...)
    $line_of_tokens->{_line_text}   - the text of the line
    $line_of_tokens->{_line_type}   - the type of the line, one of:

       SYSTEM         - system-specific code before hash-bang line
       CODE           - line of perl code (including comments)
       POD_START      - line starting pod, such as '=head'
       POD            - pod documentation text
       POD_END        - last line of pod section, '=cut'
       HERE           - text of here-document
       HERE_END       - last line of here-doc (target word)
       FORMAT         - format section
       FORMAT_END     - last line of format section, '.'
       DATA_START     - __DATA__ line
       DATA           - unidentified text following __DATA__
       END_START      - __END__ line
       END            - unidentified text following __END__
       ERROR          - we are in big trouble, probably not a perl script

Most applications will be only interested in lines of type **CODE**.  For
another example, let's write a program which checks for one of the
so-called _naughty matching variables_ `` &` ``, `$&`, and `$'`, which
can slow down processing.  Here is a **write\_line**, from the example
program **find\_naughty.pl**, which does that:

    sub write_line {
    
        # This is called back from perltidy line-by-line
        # We're looking for $`, $&, and $'
        my ( $self, $line_of_tokens ) = @_;
    
        # pull out some stuff we might need
        my $line_type         = $line_of_tokens->{_line_type};
        my $input_line_number = $line_of_tokens->{_line_number};
        my $input_line        = $line_of_tokens->{_line_text};
        my $rtoken_type       = $line_of_tokens->{_rtoken_type};
        my $rtokens           = $line_of_tokens->{_rtokens};
        chomp $input_line;
    
        # skip comments, pod, etc
        return if ( $line_type ne 'CODE' );
    
        # loop over tokens looking for $`, $&, and $'
        for ( my $j = 0 ; $j < @$rtoken_type ; $j++ ) {
    
            # we only want to examine token types 'i' (identifier)
            next unless $$rtoken_type[$j] eq 'i';
    
            # pull out the actual token text
            my $token = $$rtokens[$j];
    
            # and check it
            if ( $token =~ /^\$[\`\&\']$/ ) {
                print STDERR
                  "$input_line_number: $token\n";
            }
        }
    }

This example pulls out these tokenization variables from the $line\_of\_tokens
hash reference:

     $rtoken_type = $line_of_tokens->{_rtoken_type};
     $rtokens     = $line_of_tokens->{_rtokens};

The variable `$rtoken_type` is a reference to an array of token type codes,
and `$rtokens` is a reference to a corresponding array of token text.
These are obviously only defined for lines of type **CODE**.
Perltidy classifies tokens into types, and has a brief code for each type.
You can get a complete list at any time by running perltidy from the
command line with

     perltidy --dump-token-types

In the present example, we are only looking for tokens of type **i**
(identifiers), so the for loop skips past all other types.  When an
identifier is found, its actual text is checked to see if it is one
being sought.  If so, the above write\_line prints the token and its
line number.

The **formatter** feature is relatively new in perltidy, and further
documentation needs to be written to complete its description.  However,
several example programs have been written and can be found in the
**examples** section of the source distribution.  Probably the best way
to get started is to find one of the examples which most closely matches
your application and start modifying it.

For help with perltidy's peculiar way of breaking lines into tokens, you
might run, from the command line, 

    perltidy -D filename

where `filename` is a short script of interest.  This will produce
`filename.DEBUG` with interleaved lines of text and their token types.
The **-D** flag has been in perltidy from the beginning for this purpose.
If you want to see the code which creates this file, it is
`write_debug_entry` in Tidy.pm.

# EXPORT

    &perltidy

# VERSION

This man page documents Perl::Tidy version 20181116

# LICENSE

This package is free software; you can redistribute it and/or modify it
under the terms of the "GNU General Public License".

Please refer to the file "COPYING" for details.

# BUG REPORTS

A list of current bugs and issues can be found at the CPAN site [https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)

To report a new bug or problem, use the link on this page.  

The source code repository is at [https://github.com/perltidy/perltidy](https://github.com/perltidy/perltidy).

# SEE ALSO

The perltidy(1) man page describes all of the features of perltidy.  It
can be found at http://perltidy.sourceforge.net.
