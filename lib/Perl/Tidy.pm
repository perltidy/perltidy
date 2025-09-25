#
###########################################################
#
#    perltidy - a perl script indenter and formatter
#
#    Copyright (c) 2000-2025 by Steve Hancock
#    Distributed under the GPL license agreement; see file COPYING
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License along
#    with this program; if not, write to the Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
#    For brief instructions, try 'perltidy -h'.
#    For more complete documentation, try 'man perltidy'
#    or visit the GitHub site https://perltidy.github.io/perltidy/
#
#    This script is an example of the default style.  It was formatted with:
#
#      perltidy Tidy.pm
#
#    Code Contributions: See ChangeLog.html for a complete history.
#      Michael Cartmell supplied code for adaptation to VMS and helped with
#        v-strings.
#      Hugh S. Myers supplied sub streamhandle and the supporting code to
#        create a Perl::Tidy module which can operate on strings, arrays, etc.
#      Yves Orton supplied coding to help detect Windows versions.
#      Axel Rose supplied a patch for MacPerl.
#      Sebastien Aperghis-Tramoni supplied a patch for the defined or operator.
#      Dan Tyrell contributed a patch for binary I/O.
#      Ueli Hugenschmidt contributed a patch for -fpsc
#      Sam Kington supplied a patch to identify the initial indentation of
#      entabbed code.
#      jonathan swartz supplied patches for:
#      * .../ pattern, which looks upwards from directory
#      * --notidy, to be used in directories where we want to avoid
#        accidentally tidying
#      * prefilter and postfilter
#      * iterations option
#
#      Many others have supplied key ideas, suggestions, and bug reports;
#        see the CHANGES file.
#
############################################################

package Perl::Tidy;

# perlver reports minimum version needed is 5.8.1
# 5.004 needed for IO::File
# 5.008 needed for wide characters
# 5.008001 needed for utf8::is_utf8
# 5.008001 needed for Scalar::Util::refaddr
use 5.008001;
use warnings;
use strict;
use Exporter;
use Carp;
use English     qw( -no_match_vars );
use Digest::MD5 qw(md5_hex);
use Perl::Tidy::Debugger;
use Perl::Tidy::Diagnostics;
use Perl::Tidy::FileWriter;
use Perl::Tidy::Formatter;
use Perl::Tidy::HtmlWriter;
use Perl::Tidy::IOScalar;
use Perl::Tidy::IOScalarArray;
use Perl::Tidy::IndentationItem;
use Perl::Tidy::Logger;
use Perl::Tidy::Tokenizer;
use Perl::Tidy::VerticalAligner;
local $OUTPUT_AUTOFLUSH = 1;

# DEVEL_MODE can be turned on for extra checking during development
use constant DEVEL_MODE   => 0;
use constant DIAGNOSTICS  => 0;
use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };
use constant CONST_1024   => 1024;    # bytes per kb; 2**10

use vars qw{
  $VERSION
  @ISA
  @EXPORT
};

@ISA    = qw( Exporter );
@EXPORT = qw( &perltidy );

use Cwd;
use Encode ();
use Encode::Guess;
use IO::File;
use File::Basename;
use File::Copy;
use File::Spec ();

# perl stat function index names, based on
#    https://perldoc.perl.org/functions/stat
use constant {

    _mode_  => 2,    # file mode  (type and permissions)
    _uid_   => 4,    # numeric user ID of file's owner
    _gid_   => 5,    # numeric group ID of file's owner
    _atime_ => 8,    # last access time in seconds since the epoch
    _mtime_ => 9,    # last modify time in seconds since the epoch

##  _dev_     => 0,     # device number of filesystem
##  _ino_     => 1,     # inode number
##  _nlink_   => 3,     # number of (hard) links to the file
##  _rdev_    => 6,     # the device identifier (special files only)
##  _size_    => 7,     # total size of file, in bytes
##  _ctime_   => 10,    # inode change time in seconds since the epoch (*)
##  _blksize_ => 11,    # preferred I/O size in bytes for interacting with
##                      # the file (may vary from file to file)
##  _blocks_  => 12,    # actual number of system-specific blocks allocated
##                      # on disk (often, but not always, 512 bytes each)
};

BEGIN {

    # Release version is the approximate YYYYMMDD of the release.
    # Development version is (Last Release).(Development Number)

    # To make the number continually increasing, the Development Number is a 2
    # digit number starting at 01 after a release. It is continually bumped
    # along at significant points during development. If it ever reaches 99
    # then the Release version must be bumped, and it is probably past time for
    # a release anyway.

    $VERSION = '20250912.01';
} ## end BEGIN

{
    # List of hash keys to prevent -duk from listing them.
    my @unique_hash_keys_uu = qw( html-toc-extension html-src-extension * );
}

sub DESTROY {
    my $self = shift;

    # required to avoid call to AUTOLOAD in some versions of perl
    return;
} ## end sub DESTROY

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    print {*STDERR} <<EOM;
======================================================================
Unexpected call to Autoload looking for sub $AUTOLOAD
Called from package: '$pkg'
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
} ## end sub AUTOLOAD

sub streamhandle {

    my ( $filename, $mode, ($is_encoded_data) ) = @_;

    # Given:
    #   $filename
    #   $mode = 'r' or 'w' (only 'w' is used now, see note below)
    #   $is_encoded_data (optional flag)

    # Create an object which:
    #   has a 'getline' method if mode='r', and
    #   has a 'print' method if mode='w'.
    # The objects also need a 'close' method.
    #
    # How the object is made:
    #
    # if $filename is:     Make object using:
    # ----------------     -----------------
    # '-'                  (STDIN if mode = 'r', STDOUT if mode='w')
    # string               IO::File
    # ARRAY  ref           Perl::Tidy::IOScalarArray (formerly IO::ScalarArray)
    # STRING ref           Perl::Tidy::IOScalar      (formerly IO::Scalar)
    # object               object
    #                      (check for 'print' method for 'w' mode)
    #                      (check for 'getline' method for 'r' mode)

    # An optional flag '$is_encoded_data' may be given, as follows:
    # - true: encoded data is being transferred,
    #    set encoding to be utf8 for files and for stdin.
    # - false: unencoded binary data is being transferred,
    #    set binary mode for files and for stdin.

    # NOTE: mode 'r' works but is no longer used.
    # Use sub stream_slurp instead for mode 'r', for efficiency.
    $mode = lc($mode);
    if ( $mode ne 'w' ) {
        if ( DEVEL_MODE || $mode ne 'r' ) {
            Fault("streamhandle called in unexpected mode '$mode'\n");
        }
    }

    my $ref = ref($filename);
    my $New;
    my $fh;

    #-------------------
    # handle a reference
    #-------------------
    if ($ref) {
        if ( $ref eq 'ARRAY' ) {
            $New = sub { Perl::Tidy::IOScalarArray->new( $filename, $mode ) };
        }
        elsif ( $ref eq 'SCALAR' ) {
            $New = sub { Perl::Tidy::IOScalar->new( $filename, $mode ) };
        }
        else {

            # Accept an object with a getline method for reading. Note:
            # IO::File is built-in and does not respond to the defined
            # operator.  If this causes trouble, the check can be
            # skipped and we can just let it crash if there is no
            # getline.
            if ( $mode eq 'r' ) {

                # RT#97159; part 1 of 2: updated to use 'can'
                if ( $ref->can('getline') ) {
                    $New = sub { $filename };
                }
                else {
                    $New = sub { undef };
                    confess <<EOM;
------------------------------------------------------------------------
No 'getline' method is defined for object of class '$ref'
Please check your call to Perl::Tidy::perltidy.  Trace follows.
------------------------------------------------------------------------
EOM
                }
            }

            # Accept an object with a print method for writing.
            # See note above about IO::File
            if ( $mode eq 'w' ) {

                # RT#97159; part 2 of 2: updated to use 'can'
                if ( $ref->can('print') ) {
                    $New = sub { $filename };
                }
                else {
                    $New = sub { undef };
                    confess <<EOM;
------------------------------------------------------------------------
No 'print' method is defined for object of class '$ref'
Please check your call to Perl::Tidy::perltidy. Trace follows.
------------------------------------------------------------------------
EOM
                }
            }
        }
    }

    #----------------
    # handle a string
    #----------------
    else {
        if ( $filename eq '-' ) {
            $New = sub { $mode eq 'w' ? *STDOUT : *STDIN }
        }
        else {
            $New = sub { IO::File->new( $filename, $mode ) };
        }
    }

    #--------------
    # Open the file
    #--------------
    $fh = $New->( $filename, $mode );

    if ( !$fh ) {
        Warn("Couldn't open file:'$filename' in mode:$mode : $OS_ERROR\n");
    }

    #------------
    # Set binmode
    #------------
    else {
        if ( ref($fh) eq 'IO::File' ) {
            ## binmode object call not available in older perl versions
            ## $fh->binmode(":raw:encoding(UTF-8)");
            if ($is_encoded_data) { binmode $fh, ":raw:encoding(UTF-8)"; }
            else                  { binmode $fh }
        }
        elsif ( $filename eq '-' ) {
            if ($is_encoded_data) { binmode STDOUT, ":raw:encoding(UTF-8)"; }
            else                  { binmode STDOUT }
        }
        else {

            # shouldn't get here
            if (DEVEL_MODE) {
                my $ref_fh = ref($fh);
                Fault(<<EOM);
unexpected streamhandle state for file='$filename' mode='$mode' ref(fh)=$ref_fh
EOM
            }
        }
    }

    return $fh;
} ## end sub streamhandle

sub stream_slurp {

    my ( $filename, ($timeout_in_seconds) ) = @_;

    # Given:
    #   $filename
    #   $timeout_in_seconds (optional timeout, in seconds)

    # Read the text in $filename and
    # return:
    #    undef if read error, or
    #    $rinput_string = ref to string of text

    # if $filename is:     Read
    # ----------------     -----------------
    # ARRAY  ref           array ref
    # SCALAR ref           string ref
    # object ref           object with 'getline' method (exit if no 'getline')
    # '-'                  STDIN
    # string               file named $filename

    # Note that any decoding from utf8 must be done by the caller

    my $ref = ref($filename);
    my $rinput_string;

    # handle a reference
    if ($ref) {
        if ( $ref eq 'ARRAY' ) {
            my $buf = join EMPTY_STRING, @{$filename};
            $rinput_string = \$buf;
        }
        elsif ( $ref eq 'SCALAR' ) {
            $rinput_string = $filename;
        }
        else {
            if ( $ref->can('getline') ) {
                my $buf = EMPTY_STRING;
                while ( defined( my $line = $filename->getline() ) ) {
                    $buf .= $line;
                }
                $rinput_string = \$buf;
            }
            else {
                confess <<EOM;
------------------------------------------------------------------------
No 'getline' method is defined for object of class '$ref'
Please check your call to Perl::Tidy::perltidy.  Trace follows.
------------------------------------------------------------------------
EOM
            }
        }
    }

    # handle a string
    else {
        if ( $filename eq '-' ) {
            local $INPUT_RECORD_SEPARATOR = undef;
            my $buf;
            if ( $timeout_in_seconds && $timeout_in_seconds > 0 ) {
                eval {
                    local $SIG{ALRM} = sub { die "alarm\n" };
                    alarm($timeout_in_seconds);
                    $buf = <>;
                    alarm(0);
                    1;
                }
                  or Die(
"Timeout reading stdin using -tos=$timeout_in_seconds seconds. Use -tos=0 to skip timeout check.\n"
                  );
            }
            else {
                $buf = <>;
            }
            $rinput_string = \$buf;
        }
        else {
            if ( open( my $fh, '<', $filename ) ) {
                local $INPUT_RECORD_SEPARATOR = undef;
                my $buf = <$fh>;
                $fh->close() or Warn("Cannot close $filename\n");
                $rinput_string = \$buf;
            }
            else {
                Warn("Cannot open $filename: $OS_ERROR\n");
                return;
            }
        }
    }

    return $rinput_string;
} ## end sub stream_slurp

# Here is a map of the flow of data from the input source to the output
# line sink:
#
#             -->Tokenizer-->Formatter-->VerticalAligner-->FileWriter-->
#       input                         groups                 output
#       lines   tokens      lines       of          lines    lines
#                                      lines
#
# The names correspond to the package names responsible for the unit processes.
#
# The overall process is controlled by the "main" package.
#
# Tokenizer analyzes a line and breaks it into tokens, peeking ahead
# if necessary.  A token is any section of the input line which should be
# manipulated as a single entity during formatting.  For example, a single
# ',' character is a token, and so is an entire side comment.  It handles
# the complexities of Perl syntax, such as distinguishing between '<<' as
# a shift operator and as a here-document, or distinguishing between '/'
# as a divide symbol and as a pattern delimiter.
#
# Formatter inserts and deletes whitespace between tokens, and breaks
# sequences of tokens at appropriate points as output lines.  It bases its
# decisions on the default rules as modified by any command-line options.
#
# VerticalAligner collects groups of lines together and tries to line up
# certain tokens, such as '=>', '#', and '=' by adding whitespace.
#
# FileWriter simply writes lines to the output stream.
#
# The Logger package, not shown, records significant events and warning
# messages.  It writes a .LOG file, which may be saved with a
# '-log' or a '-g' flag.

{ #<<<  (this side comment avoids excessive indentation in a closure)

my $Warn_count;
my $fh_stderr;
my $loaded_unicode_gcstring;
my $rstatus;

# Bump Warn_count only: it is essential to bump the count on all warnings, even
# if no message goes out, so that the correct exit status is set.
sub Warn_count_bump { $Warn_count++; return }

# Output Warn message only
sub Warn_msg { my $msg = shift; $fh_stderr->print($msg); return }

# Output Warn message and bump Warn count
sub Warn { my $msg = shift; $fh_stderr->print($msg); $Warn_count++; return }

sub Exit {
    my $flag = shift;
    if   ($flag) { goto ERROR_EXIT }
    else         { goto NORMAL_EXIT }
    croak "unexpected return to sub Exit";
} ## end sub Exit

sub Die {
    my $msg = shift;
    Warn($msg);
    Exit(1);
    croak "unexpected return from sub Exit";
} ## end sub Die

sub Fault {
    my ($msg) = @_;

    # This routine is called for errors that really should not occur
    # except if there has been a bug introduced by a recent program change.
    # Please add comments at calls to Fault to explain why the call
    # should not occur, and where to look to fix it.
    my ( $package0_uu, $filename0_uu, $line0,    $subroutine0_uu ) = caller(0);
    my ( $package1_uu, $filename1,    $line1,    $subroutine1 )    = caller(1);
    my ( $package2_uu, $filename2_uu, $line2_uu, $subroutine2 )    = caller(2);
    my $pkg = __PACKAGE__;

    my $input_stream_name = $rstatus->{'input_name'};
    $input_stream_name = '(unknown)' unless ($input_stream_name);
    Die(<<EOM);
==============================================================================
While operating on input stream with name: '$input_stream_name'
A fault was detected at line $line0 of sub '$subroutine1'
in file '$filename1'
which was called from line $line1 of sub '$subroutine2'
Message: '$msg'
This is probably an error introduced by a recent programming change.
$pkg reports VERSION='$VERSION'.
==============================================================================
EOM

    croak "unexpected return from sub Die";
} ## end sub Fault

sub is_char_mode {

    my ($string) = @_;

    # Returns:
    #   true  if $string is in Perl's internal character mode
    #         (also called the 'upgraded form', or UTF8=1)
    #   false if $string is in Perl's internal byte mode

    # This function isolates the call to Perl's internal function
    # utf8::is_utf8() which is true for strings represented in an 'upgraded
    # form'. It is available AFTER Perl version 5.8.
    # See https://perldoc.perl.org/Encode.
    # See also comments in Carp.pm and other modules using this function

    return 1 if ( utf8::is_utf8($string) );
    return;
} ## end sub is_char_mode

my $md5_hex = sub {
    my ($buf) = @_;

    # Evaluate the MD5 sum for a string:
    # Given:
    #   $buf = a string
    # Return:
    #   $digest = its MD5 sum
    my $octets = Encode::encode( "utf8", $buf );
    my $digest = md5_hex($octets);
    return $digest;
}; ## end $md5_hex = sub

sub get_iteration_count {
    return $rstatus->{iteration_count};
}

my %is_known_markup_word;

BEGIN {
    my @q = qw( ?xml !doctype !-- html meta );
    @is_known_markup_word{@q} = (1) x scalar(@q);
}

sub is_not_perl {

    my ( $rinput_string, $input_file, $is_named_file ) = @_;

    # Given:
    #  $rinput_string = ref to the string of text being processed
    #  $input_file    = the name of the input stream
    #  $is_named_file = true if $input_file is a named file
    # Return:
    #  true if this is clearly not a perl script
    #  false otherwise
    # Note:
    #  This sub is called when the first character of a file is '<' in order
    #  to catch the obvious cases of some kind of markup language.
    #  See also some related code in 'find_angle_operator_termination which
    #  will catch some markup syntax which gets past this preliminary check.

    my $text;
    if ( ${$rinput_string} =~ m/\s*\<\s*([\?\!]?[\-\w]+)/ ) { $text = $1 }
    else                                                    { return }

    return 1 if ( $is_known_markup_word{ lc($text) } );

    # require a named file with known extension for other markup words
    return if ( !$is_named_file );

    # check filename
    return 1 if ( $input_file =~ /html?$/i );

    return;
} ## end sub is_not_perl

BEGIN {

    # Array index names for $self.
    # Do not combine with other BEGIN blocks (c101).
    my $i = 0;
    use constant {
        _actual_output_extension_  => $i++,
        _debugfile_stream_         => $i++,
        _decoded_input_as_         => $i++,
        _destination_stream_       => $i++,
        _diagnostics_object_       => $i++,
        _display_name_             => $i++,
        _file_extension_separator_ => $i++,
        _fileroot_                 => $i++,
        _is_encoded_data_          => $i++,
        _length_function_          => $i++,
        _line_separator_default_   => $i++,
        _line_separator_           => $i++,
        _line_tidy_begin_          => $i++,
        _line_tidy_end_            => $i++,
        _logger_object_            => $i++,
        _output_file_              => $i++,
        _postfilter_               => $i++,
        _prefilter_                => $i++,
        _rOpts_                    => $i++,
        _saw_pbp_                  => $i++,
        _teefile_stream_           => $i++,
        _user_formatter_           => $i++,
        _input_copied_verbatim_    => $i++,
        _input_output_difference_  => $i++,
        _dump_to_stdout_           => $i++,
    };
} ## end BEGIN

sub perltidy {

    my %input_hash = @_;

    # This is the main perltidy routine

    my %defaults = (
        argv                  => undef,
        destination           => undef,
        formatter             => undef,
        logfile               => undef,
        errorfile             => undef,
        teefile               => undef,
        debugfile             => undef,
        perltidyrc            => undef,
        source                => undef,
        stderr                => undef,
        dump_options          => undef,
        dump_options_type     => undef,
        dump_getopt_flags     => undef,
        dump_options_category => undef,
        dump_abbreviations    => undef,
        prefilter             => undef,
        postfilter            => undef,
    );

    # Status information which can be returned for diagnostic purposes.
    # NOTE: This is intended only for testing and subject to change.

    # List of "key => value" hash entries:

    # Some relevant user input parameters for convenience:
    # opt_format         => value of --format: 'tidy', 'html', or 'user'
    # opt_encoding       => value of -enc flag: 'utf8', 'none', or 'guess'
    # opt_encode_output  => value of -eos flag: 'eos' or 'neos'
    # opt_max_iterations => value of --iterations=n

    # file_count         => number of files processed in this call

    # If multiple files are processed, then the following values will be for
    # the last file only:

    # input_name         => name of the input stream
    # output_name        => name of the output stream

    # The following two variables refer to Perl's two internal string modes,
    # and have the values 0 for 'byte' mode and 1 for 'char' mode:
    # char_mode_source   => true if source is in 'char' mode. Will be false
    #      unless we received a source string ref with utf8::is_utf8() set.
    # char_mode_used     => true if text processed by perltidy in 'char' mode.
    #      Normally true for text identified as utf8, otherwise false.

    # This tells if Unicode::GCString was used
    # gcs_used           => true if -gcs and Unicode::GCString found & used

    # These variables tell what utf8 decoding/encoding was done:
    # input_decoded_as   => non-blank if perltidy decoded the source text
    # output_encoded_as  => non-blank if perltidy encoded before return

    # These variables are related to iterations and convergence testing:
    # iteration_count    => number of iterations done
    #                       ( can be from 1 to opt_max_iterations )
    # converged          => true if stopped on convergence
    #                       ( can only happen if opt_max_iterations > 1 )
    # blinking           => true if stopped on blinking states
    #                       ( i.e., unstable formatting, should not happen )

    $rstatus = {

        file_count         => 0,
        opt_format         => EMPTY_STRING,
        opt_encoding       => EMPTY_STRING,
        opt_encode_output  => EMPTY_STRING,
        opt_max_iterations => EMPTY_STRING,

        input_name        => EMPTY_STRING,
        output_name       => EMPTY_STRING,
        char_mode_source  => 0,
        char_mode_used    => 0,
        input_decoded_as  => EMPTY_STRING,
        output_encoded_as => EMPTY_STRING,
        gcs_used          => 0,
        iteration_count   => 0,
        converged         => 0,
        blinking          => 0,
    };

    # Fix for issue git #57
    $Warn_count = 0;

    # don't overwrite callers ARGV
    # Localization of @ARGV could be avoided by calling GetOptionsFromArray
    # instead of GetOptions, but that is not available before perl 5.10
    local @ARGV   = @ARGV;
    local *STDERR = *STDERR;

    if ( my @bad_keys = grep { !exists $defaults{$_} } keys %input_hash ) {
        local $LIST_SEPARATOR = ')(';
        my @good_keys = sort keys %defaults;
        @bad_keys = sort @bad_keys;
        confess <<EOM;
------------------------------------------------------------------------
Unknown perltidy parameter : (@bad_keys)
perltidy only understands : (@good_keys)
------------------------------------------------------------------------

EOM
    }

    my $get_hash_ref = sub {

        my ($key) = @_;

        # Get and check a parameter from the input hash

        my $hash_ref = $input_hash{$key};
        if ( defined($hash_ref) ) {
            if ( ref($hash_ref) ne 'HASH' ) {
                my $what = ref($hash_ref);
                my $but_is =
                  $what ? "but is ref to $what" : "but is not a reference";
                croak <<EOM;
------------------------------------------------------------------------
error in call to perltidy:
-$key must be reference to HASH $but_is
------------------------------------------------------------------------
EOM
            }
        }
        return $hash_ref;
    }; ## end $get_hash_ref = sub

    %input_hash = ( %defaults, %input_hash );
    my $argv               = $input_hash{'argv'};
    my $destination_stream = $input_hash{'destination'};
    my $perltidyrc_stream  = $input_hash{'perltidyrc'};
    my $source_stream      = $input_hash{'source'};
    my $stderr_stream      = $input_hash{'stderr'};
    my $user_formatter     = $input_hash{'formatter'};
    my $prefilter          = $input_hash{'prefilter'};
    my $postfilter         = $input_hash{'postfilter'};

    if ($stderr_stream) {
        $fh_stderr = Perl::Tidy::streamhandle( $stderr_stream, 'w' );
        if ( !$fh_stderr ) {
            croak <<EOM;
------------------------------------------------------------------------
Unable to redirect STDERR to $stderr_stream
Please check value of -stderr in call to perltidy
------------------------------------------------------------------------
EOM
        }
    }
    else {
        $fh_stderr = *STDERR;
    }

    my $self = [];
    bless $self, __PACKAGE__;

    # extract various dump parameters
    my $dump_options_type     = $input_hash{'dump_options_type'};
    my $dump_options          = $get_hash_ref->('dump_options');
    my $dump_getopt_flags     = $get_hash_ref->('dump_getopt_flags');
    my $dump_options_category = $get_hash_ref->('dump_options_category');
    my $dump_abbreviations    = $get_hash_ref->('dump_abbreviations');

    # validate dump_options_type
    if ( defined($dump_options) ) {
        if ( !defined($dump_options_type) ) {
            $dump_options_type = 'perltidyrc';
        }
        if (   $dump_options_type ne 'perltidyrc'
            && $dump_options_type ne 'full' )
        {
            croak <<EOM;
------------------------------------------------------------------------
Please check value of -dump_options_type in call to perltidy;
saw: '$dump_options_type'
expecting: 'perltidyrc' or 'full'
------------------------------------------------------------------------
EOM

        }
    }
    else {
        $dump_options_type = EMPTY_STRING;
    }

    if ($user_formatter) {

        # if the user defines a formatter, there is no output stream,
        # but we need a null stream to keep coding simple
        $destination_stream = \my $tmp;
    }

    # see if ARGV is overridden
    if ( defined($argv) ) {

        my $rargv = ref($argv);
        if ( $rargv eq 'SCALAR' ) { $argv = ${$argv}; $rargv = undef }

        # ref to ARRAY
        if ($rargv) {
            if ( $rargv eq 'ARRAY' ) {
                @ARGV = @{$argv};
            }
            else {
                croak <<EOM;
------------------------------------------------------------------------
Please check value of -argv in call to perltidy;
it must be a string or ref to ARRAY but is: $rargv
------------------------------------------------------------------------
EOM
            }
        }

        # string
        else {
            my ( $rargv_str, $msg ) = parse_args($argv);
            if ($msg) {
                Die(<<EOM);
Error parsing this string passed to perltidy with 'argv':
$msg
EOM
            }
            @ARGV = @{$rargv_str};
        }
    }

    # These string refs will hold any warnings and error messages to be written
    # to the logfile object when it eventually gets created.
    my $rpending_complaint;
    ${$rpending_complaint} = EMPTY_STRING;

    my $rpending_logfile_message;
    ${$rpending_logfile_message} = EMPTY_STRING;

    my ( $is_Windows, $Windows_type ) = look_for_Windows($rpending_complaint);

    # VMS file names are restricted to a 40.40 format, so we append _tdy
    # instead of .tdy, etc. (but see also sub check_vms_filename)
    my $dot;
    my $dot_pattern;
    if ( $OSNAME eq 'VMS' ) {
        $dot         = '_';
        $dot_pattern = '_';
    }
    else {
        $dot         = '.';
        $dot_pattern = '\.';    # must escape for use in regex
    }
    $self->[_file_extension_separator_] = $dot;

    # save a copy of the last two input args for error checking later
    my @ARGV_saved;
    if ( @ARGV > 1 ) {
        @ARGV_saved = ( $ARGV[-2], $ARGV[-1] );
    }

    # see if -wvt was entered on the command line before @ARGV is changed
    my $wvt_in_args = grep { /-(wvt|warn-variable-types)=/ } @ARGV;

    #-------------------------
    # get command line options
    #-------------------------
    my ( $rOpts, $config_file, $rraw_options, $roption_string,
        $rexpansion, $roption_category, $rinteger_option_range )
      = process_command_line(
        $perltidyrc_stream,  $is_Windows, $Windows_type,
        $rpending_complaint, $dump_options_type,
      );

    # Only filenames should remain in @ARGV
    my @Arg_files = @ARGV;

    $self->[_rOpts_] = $rOpts;

    my $saw_pbp =
      grep { $_ eq '-pbp' || $_ eq '-perl-best-practices' } @{$rraw_options};
    $self->[_saw_pbp_] = $saw_pbp;

    #------------------------------------
    # Handle requests to dump information
    #------------------------------------

    # return or exit immediately after all dumps
    my $quit_now = 0;

    # Getopt parameters and their flags
    if ( defined($dump_getopt_flags) ) {
        $quit_now = 1;
        foreach my $op ( @{$roption_string} ) {
            my $opt  = $op;
            my $flag = EMPTY_STRING;

            # Examples:
            #  some-option=s
            #  some-option=i
            #  some-option:i
            #  some-option!
            if ( $opt =~ /(.*)(!|=.*|:.*)$/ ) {
                $opt  = $1;
                $flag = $2;
            }
            $dump_getopt_flags->{$opt} = $flag;
        }
    }

    if ( defined($dump_options_category) ) {
        $quit_now = 1;
        %{$dump_options_category} = %{$roption_category};
    }

    if ( defined($dump_abbreviations) ) {
        $quit_now = 1;
        %{$dump_abbreviations} = %{$rexpansion};
    }

    if ( defined($dump_options) ) {
        $quit_now = 1;
        %{$dump_options} = %{$rOpts};
    }

    Exit(0) if ($quit_now);

    # make printable string of options for this run as possible diagnostic
    my $readable_options = readable_options( $rOpts, $roption_string );

    # dump from command line
    if ( $rOpts->{'dump-options'} ) {
        print {*STDOUT} $readable_options;
        Exit(0);
    }

    # some dump options require one filename in the arg list.  This is a safety
    # precaution in case a user accidentally adds such an option to the command
    # line parameters and is expecting formatted output to stdout.  Another
    # precaution, added elsewhere, is to ignore these in a .perltidyrc
    my $num_files = @Arg_files;
    foreach my $opt_name (
        qw(
        dump-block-summary
        dump-unusual-variables
        dump-mixed-call-parens
        dump-mismatched-args
        dump-mismatched-returns
        dump-unique-keys
        dump-hash-keys
        dump-similar-keys
        )
      )
    {

        if ( $rOpts->{$opt_name} ) {
            $self->[_dump_to_stdout_] = 1;
            if ( $num_files != 1 ) {
                Die(<<EOM);
--$opt_name expects 1 filename in the arg list but saw $num_files filenames
EOM
            }
        }
    }

    #----------------------------------------
    # check parameters and their interactions
    #----------------------------------------
    $self->check_options( $num_files, $rinteger_option_range );

    if ($user_formatter) {
        $rOpts->{'format'} = 'user';
    }

    # there must be one entry here for every possible format
    my %default_file_extension = (
        tidy => 'tdy',
        html => 'html',
        user => EMPTY_STRING,
    );

    $rstatus->{'opt_format'}         = $rOpts->{'format'};
    $rstatus->{'opt_max_iterations'} = $rOpts->{'iterations'};
    $rstatus->{'opt_encode_output'} =
      $rOpts->{'encode-output-strings'} ? 'eos' : 'neos';

    # be sure we have a valid output format
    if ( !exists $default_file_extension{ $rOpts->{'format'} } ) {
        my $formats = join SPACE,
          sort map { "'" . $_ . "'" } keys %default_file_extension;
        my $fmt = $rOpts->{'format'};
        Die("-format='$fmt' but must be one of: $formats\n");
    }

    my $output_extension =
      $self->make_file_extension( $rOpts->{'output-file-extension'},
        $default_file_extension{ $rOpts->{'format'} } );

    # get parameters associated with the -b option
    my ( $in_place_modify, $backup_extension, $delete_backup ) =
      $self->check_in_place_modify( $source_stream, $destination_stream );

    my $line_range_clipped = $rOpts->{'line-range-tidy'}
      && ( $self->[_line_tidy_begin_] > 1
        || defined( $self->[_line_tidy_end_] ) );

    Perl::Tidy::Formatter::check_options( $rOpts, $wvt_in_args, $num_files,
        $line_range_clipped );
    Perl::Tidy::Tokenizer::check_options($rOpts);
    Perl::Tidy::VerticalAligner::check_options($rOpts);
    if ( $rOpts->{'format'} eq 'html' ) {
        Perl::Tidy::HtmlWriter->check_options($rOpts);
    }

    # Try to catch an unusual missing string parameter error, like this:
    #    perltidy -wvt perltidy.pl
    # The problem is that -wvt wants a string, so it grabs 'perltidy.pl'.
    # Then there is no output filename, so input is assumed to be stdin.
    # This make perltidy unexpectedly wait for input. To the user, it
    # appears that perltidy has gone into an infinite loop. Issue c312.
    # To avoid getting this far, it is best for parameters which take a
    # string to check the strings in one of the 'check_options' subs, and
    # exit if there is an obvious error. This has been done for -wvt,
    # but are undoubtedly other parameters where this problem might occur.
    if ( !$num_files && @ARGV_saved > 1 ) {
        my $opt_test  = $ARGV_saved[-2];
        my $file_test = $ARGV_saved[-1];
        if (   $opt_test =~ s/^[-]+//
            && $file_test !~ /^[-]/
            && $file_test !~ /^\d+$/
            && -e $file_test )
        {

            # These options can take filenames, so we will ignore them here
            my %is_option_with_file_parameter;
            my @qf = qw( outfile profile );
            @is_option_with_file_parameter{@qf} = (1) x scalar(@qf);

            # Expand an abbreviation into a long name
            my $long_name;
            my $exp = $rexpansion->{$opt_test};
            if    ( !$exp )        { $long_name = $opt_test }
            elsif ( @{$exp} == 1 ) { $long_name = $exp->[0] }
            else                   { }

            # If this arg grabbed the file, then it must take a string arg
            if (   $long_name
                && defined( $rOpts->{$long_name} )
                && $rOpts->{$long_name} eq $file_test
                && !$is_option_with_file_parameter{$long_name} )
            {
                Die(<<EOM);
Stopping on possible missing string parameter for '-$opt_test':
This parameter takes a string and has been set equal to file '$file_test',
and formatted output will go to standard output. If this is actually correct,
you can skip this message by entering this as '-$opt_test=$file_test'.
EOM
            }
        }
    }

    # make the pattern of file extensions that we shouldn't touch
    my $forbidden_file_extensions = "(($dot_pattern)(LOG|DEBUG|ERR|TEE)";
    if ($output_extension) {
        my $ext = quotemeta($output_extension);
        $forbidden_file_extensions .= "|$ext";
    }
    if ( $in_place_modify && $backup_extension ) {
        my $ext = quotemeta($backup_extension);
        $forbidden_file_extensions .= "|$ext";
    }
    $forbidden_file_extensions .= ')$';

    # Create a diagnostics object if requested;
    # This is only useful for code development
    my $diagnostics_object = undef;
    if (DIAGNOSTICS) {
        $diagnostics_object = Perl::Tidy::Diagnostics->new();
    }

    # no filenames should be given if input is from an array
    if ($source_stream) {
        if ( @Arg_files > 0 ) {
            Die(
"You may not specify any filenames when a source array is given\n"
            );
        }

        # we'll stuff the source array into Arg_files
        unshift( @Arg_files, $source_stream );

        # No special treatment for source stream which is a filename.
        # This will enable checks for binary files and other bad stuff.
        $source_stream = undef unless ( ref($source_stream) );
    }

    # use stdin by default if no source array and no args
    elsif ( !@Arg_files ) {
        unshift( @Arg_files, '-' );
    }

    # check file existence and expand any globs
    else {
        my @updated_files;
        foreach my $input_file (@Arg_files) {
            if ( -e $input_file ) {
                push @updated_files, $input_file;
            }
            else {

                # file doesn't exist - check for a file glob
                if ( $input_file =~ /([\?\*\[\{])/ ) {

                    # Windows shell may not remove quotes, so do it
                    my $ifile = $input_file;
                    if ( $ifile =~ /^\'(.+)\'$/ ) { $ifile = $1 }
                    if ( $ifile =~ /^\"(.+)\"$/ ) { $ifile = $1 }
                    my $pattern = fileglob_to_re($ifile);
                    my $dh;
                    if ( opendir( $dh, './' ) ) {
                        my @files =
                          grep { /$pattern/ && !-d } readdir($dh);
                        closedir($dh);
                        next unless (@files);
                        push @updated_files, @files;
                        next;
                    }
                }
                Warn("skipping file: '$input_file': no matches found\n");
                next;
            }
        } ## end loop over input filenames

        @Arg_files = @updated_files;
        if ( !@Arg_files ) {
            Die("no matching input files found\n");
        }
    }

    # Flag for loading module Unicode::GCString for evaluating text width:
    #   undef = ok to use but not yet loaded
    #       0 = do not use; failed to load or not wanted
    #       1 = successfully loaded and ok to use
    # The module is not actually loaded unless/until it is needed
    if ( !$rOpts->{'use-unicode-gcstring'} ) {
        $loaded_unicode_gcstring = 0;
    }

    # Remove duplicate filenames.  Otherwise, for example if the user entered
    #     perltidy -b myfile.pl myfile.pl
    # the backup version of the original would be lost.
    if ( @Arg_files > 1 ) {
        my %seen = ();
        @Arg_files = grep { !$seen{$_}++ } @Arg_files;
    }

    # If requested, process in order of increasing file size
    # This can significantly reduce perl's virtual memory usage during testing.
    if ( @Arg_files > 1 && $rOpts->{'file-size-order'} ) {
        @Arg_files =
          map  { $_->[0] }
          sort { $a->[1] <=> $b->[1] }
          map  { [ $_, -e $_ ? -s $_ : 0 ] } @Arg_files;
    }

    my $logfile_header = make_logfile_header( $rOpts, $config_file,
        $rraw_options, $Windows_type, $readable_options );

    # Store some values needed by lower level routines
    $self->[_diagnostics_object_] = $diagnostics_object;
    $self->[_postfilter_]         = $postfilter;
    $self->[_prefilter_]          = $prefilter;
    $self->[_user_formatter_]     = $user_formatter;

    #--------------------------
    # loop to process all files
    #--------------------------
    $self->process_all_files(
        {
            rinput_hash        => \%input_hash,
            rfiles             => \@Arg_files,
            line_range_clipped => $line_range_clipped,

            # filename stuff...
            source_stream             => $source_stream,
            output_extension          => $output_extension,
            forbidden_file_extensions => $forbidden_file_extensions,
            in_place_modify           => $in_place_modify,
            backup_extension          => $backup_extension,
            delete_backup             => $delete_backup,

            # logfile stuff...
            logfile_header           => $logfile_header,
            rpending_complaint       => $rpending_complaint,
            rpending_logfile_message => $rpending_logfile_message,
        }
    );

    #-----
    # Exit
    #-----

    # Fix for RT #130297: return a true value if anything was written to the
    # standard error output, even non-fatal warning messages, otherwise return
    # false.

    # These exit codes are returned:
    #  0 = perltidy ran to completion with no errors
    #  1 = perltidy could not run to completion due to errors
    #  2 = perltidy ran to completion with error messages

    # Note that if perltidy is run with multiple files, any single file with
    # errors or warnings will write a line like
    #        '## Please see file testing.t.ERR'
    # to standard output for each file with errors, so the flag will be true,
    # even if only some of the multiple files may have had errors.

  NORMAL_EXIT:
    my $ret = $Warn_count ? 2 : 0;
    return wantarray ? ( $ret, $rstatus ) : $ret;

  ERROR_EXIT:
    return wantarray ? ( 1, $rstatus ) : 1;

} ## end sub perltidy

sub make_file_extension {

    # Make a file extension, adding any leading '.' if necessary.
    # (the '.' may actually be an '_' under VMS).
    my ( $self, $extension, ($default) ) = @_;

    # Given:
    #  $extension = the first choice (usually a user entry)
    #  $default   = an optional backup extension
    # Return:
    #  $extension = the actual file extension

    $extension = EMPTY_STRING unless ( defined($extension) );
    $extension =~ s/^\s+//;
    $extension =~ s/\s+$//;

    # Use default extension if nothing remains of the first choice
    if ( length($extension) == 0 ) {
        $extension = $default;
        $extension = EMPTY_STRING unless ( defined($extension) );
        $extension =~ s/^\s+//;
        $extension =~ s/\s+$//;
    }

    # Only extensions with these leading characters get a '.'
    # This rule gives the user some freedom.
    if ( $extension =~ /^[a-zA-Z0-9]/ ) {
        my $dot = $self->[_file_extension_separator_];
        $extension = $dot . $extension;
    }
    return $extension;
} ## end sub make_file_extension

sub check_in_place_modify {

    my ( $self, $source_stream, $destination_stream ) = @_;

    # See if --backup-and-modify-in-place (-b) is set, and if so,
    # return its associated parameters
    my $rOpts = $self->[_rOpts_];

    # check for -b option;
    # silently ignore unless beautify mode
    my $in_place_modify = $rOpts->{'backup-and-modify-in-place'}
      && $rOpts->{'format'} eq 'tidy';

    my ( $backup_extension, $delete_backup );

    # Turn off -b with warnings in case of conflicts with other options.
    # NOTE: Do this silently, without warnings, if there is a source or
    # destination stream, or standard output is used.  This is because the -b
    # flag may have been in a .perltidyrc file and warnings break
    # Test::NoWarnings.  See email discussion with Merijn Brand 26 Feb 2014.
    if ($in_place_modify) {
        if (   $rOpts->{'standard-output'}
            || $destination_stream
            || ref($source_stream)
            || $rOpts->{'outfile'}
            || defined( $rOpts->{'output-path'} ) )
        {
            $in_place_modify = 0;
        }
    }

    if ($in_place_modify) {

        # If the backup extension contains a / character then the backup should
        # be deleted when the -b option is used.   On older versions of
        # perltidy this will generate an error message due to an illegal
        # file name.
        #
        # A backup file will still be generated but will be deleted
        # at the end.  If -bext='/' then this extension will be
        # the default 'bak'.  Otherwise it will be whatever characters
        # remains after all '/' characters are removed.  For example:
        # -bext         extension     slashes
        #  '/'          bak           1
        #  '/delete'    delete        1
        #  'delete/'    delete        1
        #  '/dev/null'  devnull       2    (Currently not allowed)
        my $bext = $rOpts->{'backup-file-extension'};
        $delete_backup = ( $rOpts->{'backup-file-extension'} =~ s/\///g );

        # At present only one forward slash is allowed.  In the future multiple
        # slashes may be allowed to allow for other options
        if ( $delete_backup > 1 ) {
            Die("-bext=$bext contains more than one '/'\n");
        }

        $backup_extension =
          $self->make_file_extension( $rOpts->{'backup-file-extension'},
            'bak' );
    }

    my $backup_method = $rOpts->{'backup-method'};
    if (   defined($backup_method)
        && $backup_method ne 'copy'
        && $backup_method ne 'move' )
    {
        Die(
"Unexpected --backup-method='$backup_method'; must be one of: 'move', 'copy'\n"
        );
    }

    return ( $in_place_modify, $backup_extension, $delete_backup );
} ## end sub check_in_place_modify

sub backup_method_copy {

    my ( $self, $input_file, $routput_string, $backup_extension,
        $delete_backup )
      = @_;

    # Handle the -b (--backup-and-modify-in-place) option with -bm='copy':
    # - First copy $input file to $backup_name.
    # - Then open input file and rewrite with contents of $routput_string
    # - Then delete the backup if requested

    # NOTES:
    # - Die immediately on any error.
    # - $routput_string is a SCALAR ref

    my $backup_file = $input_file . $backup_extension;

    if ( !-f $input_file ) {

        # no real file to backup ..
        # This shouldn't happen because of numerous preliminary checks
        Die(
            "problem with -b backing up input file '$input_file': not a file\n"
        );
    }

    if ( -f $backup_file ) {
        unlink($backup_file)
          or Die(
"unable to remove previous '$backup_file' for -b option; check permissions: $OS_ERROR\n"
          );
    }

    # Copy input file to backup
    File::Copy::copy( $input_file, $backup_file )
      or Die("File::Copy failed trying to backup source: $OS_ERROR");

    # set permissions of the backup file to match the input file
    my @input_file_stat = stat($input_file);
    my $in_place_modify = 1;
    $self->set_output_file_permissions( $backup_file, \@input_file_stat,
        $in_place_modify );

    # set the modification time of the copy to the original value (rt#145999)
    my ( $read_time, $write_time ) = @input_file_stat[ _atime_, _mtime_ ];
    if ( defined($write_time) ) {
        utime( $read_time, $write_time, $backup_file )
          || Warn("error setting times for backup file '$backup_file'\n");
    }

    # Open the original input file for writing ... opening with ">" will
    # truncate the existing data.
    open( my $fout, ">", $input_file )
      or Die(
"problem re-opening $input_file for write for -b option; check file and directory permissions: $OS_ERROR\n"
      );

    if ( $self->[_is_encoded_data_] ) { binmode $fout, ":raw:encoding(UTF-8)" }
    else                              { binmode $fout }

    # Now copy the formatted output to it..
    # output must be SCALAR ref..
    if ( ref($routput_string) eq 'SCALAR' ) {
        $fout->print( ${$routput_string} )
          or Die("cannot print to '$input_file' with -b option: $OS_ERROR\n");
    }

    # Error if anything else ...
    else {
        my $ref = ref($routput_string);
        Die(<<EOM);
Programming error: unable to print to '$input_file' with -b option:
unexpected ref type '$ref'; expecting 'ARRAY' or 'SCALAR'
EOM
    }

    $fout->close()
      or Die("cannot close '$input_file' with -b option: $OS_ERROR\n");

    # Set permissions of the output file to match the input file. This is
    # necessary even if the inode remains unchanged because suid/sgid bits may
    # have been reset.
    $self->set_output_file_permissions( $input_file, \@input_file_stat,
        $in_place_modify );

    # Keep original modification time if no change (rt#145999)
    if ( !$self->[_input_output_difference_] && defined($write_time) ) {
        utime( $read_time, $write_time, $input_file )
          || Warn("error setting times for '$input_file'\n");
    }

    #---------------------------------------------------------
    # remove the original file for in-place modify as follows:
    #   $delete_backup=0 never
    #   $delete_backup=1 only if no errors
    #   $delete_backup>1 always  : NOT ALLOWED, too risky
    #---------------------------------------------------------
    if ( $delete_backup && -f $backup_file ) {

        # Currently, $delete_backup may only be 1. But if a future update
        # allows a value > 1, then reduce it to 1 if there were warnings.
        if (   $delete_backup > 1
            && $self->[_logger_object_]->get_warning_count() )
        {
            $delete_backup = 1;
        }

        # As an added safety precaution, do not delete the source file
        # if its size has dropped from positive to zero, since this
        # could indicate a disaster of some kind, including a hardware
        # failure.  Actually, this could happen if you had a file of
        # all comments (or pod) and deleted everything with -dac (-dap)
        # for some reason.
        if ( !-s $input_file && -s $backup_file && $delete_backup == 1 ) {
            Warn(
"output file '$input_file' missing or zero length; original '$backup_file' not deleted\n"
            );
        }
        else {
            unlink($backup_file)
              or Die(
"unable to remove backup file '$backup_file' for -b option; check permissions: $OS_ERROR\n"
              );
        }
    }

    # Verify that inode is unchanged during development
    if (DEVEL_MODE) {
        my @output_file_stat = stat($input_file);
        my $inode_input      = $input_file_stat[1];
        my $inode_output     = $output_file_stat[1];
        if ( $inode_input != $inode_output ) {
            Fault(<<EOM);
inode changed with -bm=copy for file '$input_file': inode_input=$inode_input inode_output=$inode_output
EOM
        }
    }

    return;
} ## end sub backup_method_copy

sub backup_method_move {

    my ( $self, $input_file, $routput_string, $backup_extension,
        $delete_backup )
      = @_;

    # Handle the -b (--backup-and-modify-in-place) option with -bm='move':
    # - First move $input file to $backup_name.
    # - Then copy $routput_string to $input_file.
    # - Then delete the backup if requested

    # NOTES:
    # - Die immediately on any error.
    # - $routput_string is a SCALAR ref
    # - $input_file permissions will be set by sub set_output_file_permissions

    my $backup_name = $input_file . $backup_extension;

    if ( !-f $input_file ) {

        # oh, oh, no real file to backup ..
        # shouldn't happen because of numerous preliminary checks
        Die(
            "problem with -b backing up input file '$input_file': not a file\n"
        );
    }
    if ( -f $backup_name ) {
        unlink($backup_name)
          or Die(
"unable to remove previous '$backup_name' for -b option; check permissions: $OS_ERROR\n"
          );
    }

    my @input_file_stat = stat($input_file);

    # backup the input file
    # we use copy for symlinks, move for regular files
    if ( -l $input_file ) {
        File::Copy::copy( $input_file, $backup_name )
          or Die("File::Copy failed trying to backup source: $OS_ERROR");
    }
    else {
        rename( $input_file, $backup_name )
          or Die(
"problem renaming $input_file to $backup_name for -b option: $OS_ERROR\n"
          );
    }

    # Open a file with the original input file name for writing ...
    open( my $fout, ">", $input_file )
      or Die(
"problem re-opening $input_file for write for -b option; check file and directory permissions: $OS_ERROR\n"
      );

    if ( $self->[_is_encoded_data_] ) { binmode $fout, ":raw:encoding(UTF-8)" }
    else                              { binmode $fout }

    # Now copy the formatted output to it..
    # output must be SCALAR ref..
    if ( ref($routput_string) eq 'SCALAR' ) {
        $fout->print( ${$routput_string} )
          or Die("cannot print to '$input_file' with -b option: $OS_ERROR\n");
    }

    # Error if anything else ...
    else {
        my $ref = ref($routput_string);
        Die(<<EOM);
Programming error: unable to print to '$input_file' with -b option:
unexpected ref type '$ref'; expecting 'ARRAY' or 'SCALAR'
EOM
    }

    $fout->close()
      or Die("cannot close '$input_file' with -b option: $OS_ERROR\n");

    # set permissions of the output file to match the input file
    my $in_place_modify = 1;
    $self->set_output_file_permissions( $input_file, \@input_file_stat,
        $in_place_modify );

    # Keep original modification time if no change (rt#145999)
    my ( $read_time, $write_time ) = @input_file_stat[ _atime_, _mtime_ ];
    if ( !$self->[_input_output_difference_] && defined($write_time) ) {
        utime( $read_time, $write_time, $input_file )
          || Warn("error setting times for '$input_file'\n");
    }

    #---------------------------------------------------------
    # remove the original file for in-place modify as follows:
    #   $delete_backup=0 never
    #   $delete_backup=1 only if no errors
    #   $delete_backup>1 always  : NOT ALLOWED, too risky
    #---------------------------------------------------------
    if ( $delete_backup && -f $backup_name ) {

        # Currently, $delete_backup may only be 1. But if a future update
        # allows a value > 1, then reduce it to 1 if there were warnings.
        if (   $delete_backup > 1
            && $self->[_logger_object_]->get_warning_count() )
        {
            $delete_backup = 1;
        }

        # As an added safety precaution, do not delete the source file
        # if its size has dropped from positive to zero, since this
        # could indicate a disaster of some kind, including a hardware
        # failure.  Actually, this could happen if you had a file of
        # all comments (or pod) and deleted everything with -dac (-dap)
        # for some reason.
        if ( !-s $input_file && -s $backup_name && $delete_backup == 1 ) {
            Warn(
"output file '$input_file' missing or zero length; original '$backup_name' not deleted\n"
            );
        }
        else {
            unlink($backup_name)
              or Die(
"unable to remove previous '$backup_name' for -b option; check permissions: $OS_ERROR\n"
              );
        }
    }

    return;

} ## end sub backup_method_move

# masks for file permissions
use constant OCT_777  => oct(777);     # All users (O+G+W) + r/w/x bits
use constant OCT_7777 => oct(7777);    # Same + suid/sgid/sbit
use constant OCT_600  => oct(600);     # Owner RW permission

sub set_output_file_permissions {

    my ( $self, $output_file, $rinput_file_stat, $in_place_modify ) = @_;

    # Set the permissions for the output file

    # Given:
    #  $output_file      = the file whose permissions we will set
    #  $rinput_file_stat = the result of stat($input_file)
    #  $in_place_modify  = true if --backup-and-modify-in-place is set

    my ( $mode_i, $uid_i, $gid_i ) =
      @{$rinput_file_stat}[ _mode_, _uid_, _gid_ ];
    my ( $uid_o, $gid_o ) = ( stat($output_file) )[ _uid_, _gid_ ];
    my $input_file_permissions  = $mode_i & OCT_7777;
    my $output_file_permissions = $input_file_permissions;

    #rt128477: avoid inconsistent owner/group and suid/sgid
    if ( $uid_i != $uid_o || $gid_i != $gid_o ) {

        # try to change owner and group to match input file if
        # in -b mode.  Note: chown returns number of files
        # successfully changed.
        if ( $in_place_modify
            && chown( $uid_i, $gid_i, $output_file ) )
        {
            # owner/group successfully changed
        }
        else {

            # owner or group differ: do not copy suid and sgid
            $output_file_permissions = $mode_i & OCT_777;
            if ( $input_file_permissions != $output_file_permissions ) {
                Warn(
"Unable to copy setuid and/or setgid bits for output file '$output_file'\n"
                );
            }
        }
    }

    # Mark the output file for rw unless we are in -b mode.
    # Explanation: perltidy does not unlink existing output
    # files before writing to them, for safety.  If a
    # designated output file exists and is not writable,
    # perltidy will halt.  This can prevent a data loss if a
    # user accidentally enters "perltidy infile -o
    # important_ro_file", or "perltidy infile -st
    # >important_ro_file". But it also means that perltidy can
    # get locked out of rerunning unless it marks its own
    # output files writable. The alternative, of always
    # unlinking the designated output file, is less safe and
    # not always possible, except in -b mode, where there is an
    # assumption that a previous backup can be unlinked even if
    # not writable.
    if ( !$in_place_modify ) {
        $output_file_permissions |= OCT_600;
    }

    if ( !chmod( $output_file_permissions, $output_file ) ) {

        # couldn't change file permissions
        my $operm = sprintf( "%04o", $output_file_permissions );
        Warn(
"Unable to set permissions for output file '$output_file' to $operm\n"
        );
    }
    return;
} ## end sub set_output_file_permissions

sub get_decoded_string_buffer {

    my ( $self, $input_file, $display_name ) = @_;

    # Decode the input buffer from utf8 if necessary or requested

    # Given:
    #   $input_file   = the input file or stream
    #   $display_name = its name to use in error messages

    # Set $self->[_line_separator_], and

    # Return:
    #   $rinput_string = ref to input string, decoded from utf8 if necessary
    #   $is_encoded_data  = true if $buf is decoded from utf8
    #   $decoded_input_as = true if perltidy decoded input buf
    #   $encoding_log_message = messages for log file,
    #   $length_function  = function to use for measuring string width

    # Return nothing on any error; this is a signal to skip this file

    my $rOpts = $self->[_rOpts_];

    my $rinput_string =
      stream_slurp( $input_file, $rOpts->{'timeout-in-seconds'} );
    return unless ( defined($rinput_string) );

    # Note that we could have a zero size input string here if it
    # arrived from standard input or from a string ref. For example
    # 'perltidy <null.pl'.  If we issue a warning and stop, as we would
    # for a zero length file ('perltidy null.pl'), then we could cause
    # a call to the perltidy module to misbehave as a filter. So we will
    # process this as any other file in this case without any warning (c286).
    if ( !length( ${$rinput_string} ) ) {

        # zero length, but keep going
    }

    # Check size of strings arriving from the standard input. These
    # could not be checked until now.
    if ( $input_file eq '-' ) {
        my $size_in_mb =
          length( ${$rinput_string} ) / ( CONST_1024 * CONST_1024 );
        my $maximum_file_size_mb = $rOpts->{'maximum-file-size-mb'};
        if ( $size_in_mb > $maximum_file_size_mb ) {
            $size_in_mb = sprintf( "%0.1f", $size_in_mb );
            Warn(
"skipping file: <stdin>: size $size_in_mb MB exceeds limit $maximum_file_size_mb; use -maxfs=i to change\n"
            );
            return;
        }
    }

    $rinput_string = $self->set_line_separator($rinput_string);

    my $encoding_in              = EMPTY_STRING;
    my $rOpts_character_encoding = $rOpts->{'character-encoding'};
    my $encoding_log_message;
    my $decoded_input_as = EMPTY_STRING;
    $rstatus->{'char_mode_source'} = 0;

    # Case 1: If Perl is already in a character-oriented mode for this
    # string rather than a byte-oriented mode.  Normally, this happens if
    # the caller has decoded a utf8 string before calling perltidy.  But it
    # could also happen if the user has done some unusual manipulations of
    # the source.  In any case, we will not attempt to decode it because
    # that could result in an output string in a different mode.
    if ( is_char_mode( ${$rinput_string} ) ) {
        $encoding_in = "utf8";
        $rstatus->{'char_mode_source'} = 1;
    }

    # Case 2. No input stream encoding requested.  This is appropriate
    # for single-byte encodings like ascii, latin-1, etc
    elsif ( !$rOpts_character_encoding
        || $rOpts_character_encoding eq 'none' )
    {

        # nothing to do
    }

    # Case 3. guess input stream encoding if requested
    elsif ( lc($rOpts_character_encoding) eq 'guess' ) {

        # The guessing strategy is simple: use Encode::Guess to guess
        # an encoding.  If and only if the guess is utf8, try decoding and
        # use it if successful.  Otherwise, we proceed assuming the
        # characters are encoded as single bytes (same as if 'none' had
        # been specified as the encoding).

        # In testing I have found that including additional guess 'suspect'
        # encodings sometimes works but can sometimes lead to disaster by
        # using an incorrect decoding.

        my $decoder;
        if ( ${$rinput_string} =~ /[^[:ascii:]]/ ) {
            $decoder = guess_encoding( ${$rinput_string}, 'utf8' );
        }
        if ( $decoder && ref($decoder) ) {
            $encoding_in = $decoder->name;
            if ( $encoding_in ne 'UTF-8' && $encoding_in ne 'utf8' ) {
                $encoding_in = EMPTY_STRING;
                $encoding_log_message .= <<EOM;
Guessed encoding '$encoding_in' is not utf8; no encoding will be used
EOM
            }
            else {

                my $buf;
                if ( !eval { $buf = $decoder->decode( ${$rinput_string} ); 1 } )
                {

                    $encoding_log_message .= <<EOM;
Guessed encoding '$encoding_in' but decoding was unsuccessful; no encoding is used
EOM

                    # Note that a guess failed, but keep going
                    # This warning can eventually be removed
                    Warn(
"file: $display_name: bad guess to decode source as $encoding_in\n"
                    );
                    $encoding_in = EMPTY_STRING;
                }
                else {
                    $encoding_log_message .= <<EOM;
Guessed encoding '$encoding_in' successfully decoded
EOM
                    $decoded_input_as = $encoding_in;
                    $rinput_string    = \$buf;
                }
            }
        }
        else {
            $encoding_log_message .= <<EOM;
Does not look like utf8 encoded text so processing as raw bytes
EOM
        }
    }

    # Case 4. Decode with a specific encoding
    else {
        $encoding_in = $rOpts_character_encoding;
        my $buf;
        if (
            !eval {
                $buf = Encode::decode( $encoding_in, ${$rinput_string},
                    Encode::FB_CROAK | Encode::LEAVE_SRC );
                1;
            }
          )
        {

            # Quit if we cannot decode by the requested encoding;
            # Something is not right.
            Warn(
"skipping file: $display_name: Unable to decode source as $encoding_in\n"
            );

            # return nothing on error
            return;
        }
        else {
            $encoding_log_message .= <<EOM;
Specified encoding '$encoding_in' successfully decoded
EOM
            $decoded_input_as = $encoding_in;
            $rinput_string    = \$buf;
        }
    }

    # Set the encoding to be used for all further i/o: If we have
    # decoded the data with any format, then we must continue to
    # read and write it as encoded data, and we will normalize these
    # operations with utf8.  If we have not decoded the data, then
    # we must not treat it as encoded data.
    my $is_encoded_data = $encoding_in ? 'utf8' : EMPTY_STRING;
    $self->[_is_encoded_data_] = $is_encoded_data;

    # Delete any Byte Order Mark (BOM), which can cause trouble
    if ($is_encoded_data) {
        ${$rinput_string} =~ s/^\x{FEFF}//;
    }

    $rstatus->{'input_name'}       = $display_name;
    $rstatus->{'opt_encoding'}     = $rOpts_character_encoding;
    $rstatus->{'char_mode_used'}   = $encoding_in ? 1 : 0;
    $rstatus->{'input_decoded_as'} = $decoded_input_as;

    # Define the function to determine the display width of character
    # strings
    my $length_function;
    if ($is_encoded_data) {

        # Try to load Unicode::GCString for defining text display width, if
        # requested, when the first encoded file is encountered
        if ( !defined($loaded_unicode_gcstring) ) {
            if ( eval { require Unicode::GCString; 1 } ) {
                $loaded_unicode_gcstring = 1;
            }
            else {
                $loaded_unicode_gcstring = 0;
                if ( $rOpts->{'use-unicode-gcstring'} ) {
                    Warn(<<EOM);
----------------------
Unable to load Unicode::GCString: $EVAL_ERROR
Processing continues but some vertical alignment may be poor
To prevent this warning message, you can either:
- install module Unicode::GCString, or
- remove '--use-unicode-gcstring' or '-gcs' from your perltidyrc or command line
----------------------
EOM
                }
            }
        }
        if ($loaded_unicode_gcstring) {
            $length_function = sub {
                return Unicode::GCString->new( $_[0] )->columns;
            };
            $encoding_log_message .= <<EOM;
Using 'Unicode::GCString' to measure horizontal character widths
EOM
            $rstatus->{'gcs_used'} = 1;
        }
    }
    return (
        $rinput_string,
        $is_encoded_data,
        $decoded_input_as,
        $encoding_log_message,
        $length_function,

    );
} ## end sub get_decoded_string_buffer

{ #<<<

my $LF;
my $CR;
my $CRLF;

BEGIN {
    $LF   = chr(10);
    $CR   = chr(13);
    $CRLF = $CR . $LF;
}

sub get_line_separator_default {

    my ($rOpts) = @_;

    # Get the line separator that will apply unless overridden by a
    # --preserve-line-endings flag for a specific file

    my $line_separator_default = "\n";

    my $ole = $rOpts->{'output-line-ending'};
    if ($ole) {
        my %endings = (
            dos  => $CRLF,
            win  => $CRLF,
            mac  => $CR,
            unix => $LF,
        );

        $line_separator_default = $endings{ lc($ole) };

        if ( !$line_separator_default ) {
            my $str = join SPACE, keys %endings;
            Die(<<EOM);
Unrecognized line ending '$ole'; expecting one of: $str
EOM
        }

        # Check for conflict with -ple
        if ( $rOpts->{'preserve-line-endings'} ) {
            Warn("Ignoring -ple; conflicts with -ole\n");
            $rOpts->{'preserve-line-endings'} = undef;
        }
    }

    return $line_separator_default;

} ## end sub get_line_separator_default

sub set_line_separator {

    my ( $self, $rinput_string ) = @_;

    # Set the (output) line separator as requested or necessary

    my $rOpts = $self->[_rOpts_];

    # Start with the default (output) line separator
    my $line_separator = $self->[_line_separator_default_];

    # First try to find the line separator of the input stream
    my $input_line_separator;

    # Limit the search to a reasonable number of characters, in case we
    # have a weird file
    my $str = substr( ${$rinput_string}, 0, CONST_1024 );
    if ($str) {

        if ( $str =~ m/(($CR|$LF)+)/ ) {

            my $test = $1;

            # dos
            if ( $test =~ /^($CRLF)+\z/ ) {
                $input_line_separator = $CRLF;
            }

            # mac
            elsif ( $test =~ /^($CR)+\z/ ) {
                $input_line_separator = $CR;
            }

            # unix
            elsif ( $test =~ /^($LF)+\z/ ) {
                $input_line_separator = $LF;
            }

            # unknown
            else { }
        }

        # no ending seen
        else { }
    }

    if ( defined($input_line_separator) ) {

        # Remember the input line separator if needed
        if ( $rOpts->{'preserve-line-endings'} ) {
            $line_separator = $input_line_separator;
        }

        # Convert line endings to "\n" for processing if necessary.
        if ( $input_line_separator ne "\n" ) {
            my @lines = split /^/, ${$rinput_string};

            # try to convert CR to \n
            if ( $input_line_separator eq $CR ) {

                # if this file is currently a single line ..
                if ( @lines == 1 ) {

                    # and becomes multiple lines with the change ..
                    @lines = map { $_ . "\n" } split /$CR/, ${$rinput_string};
                    if ( @lines > 1 ) {

                        # then make the change
                        my $buf = join EMPTY_STRING, @lines;
                        $rinput_string = \$buf;
                    }
                }
            }

            # convert CR-LF to LF
            elsif ( ( $input_line_separator eq $CRLF ) && ( "\n" eq $LF ) ) {
                foreach my $line (@lines) { $line =~ s/$CRLF$/\n/ }
                my $buf = join EMPTY_STRING, @lines;
                $rinput_string = \$buf;
            }

            # unknown line ending scheme - leave it alone and let the tokenizer
            # deal with it
            else {
            }
        }
    }

    $self->[_line_separator_] = $line_separator;
    return $rinput_string;
} ## end sub set_line_separator
}

sub process_all_files {

    my ( $self, $rcall_hash ) = @_;

    # This routine is the main loop to process all files.
    # Total formatting is done with these layers of subroutines:
    #   perltidy                - main routine; checks run parameters
    #  *process_all_files       - main loop to process all files; *THIS LAYER
    #   process_filter_layer    - do any pre and post processing;
    #   process_iteration_layer - handle any iterations on formatting
    #   process_single_case     - solves one formatting problem

    my $rinput_hash               = $rcall_hash->{rinput_hash};
    my $rfiles                    = $rcall_hash->{rfiles};
    my $line_range_clipped        = $rcall_hash->{line_range_clipped};
    my $source_stream             = $rcall_hash->{source_stream};
    my $output_extension          = $rcall_hash->{output_extension};
    my $forbidden_file_extensions = $rcall_hash->{forbidden_file_extensions};
    my $in_place_modify           = $rcall_hash->{in_place_modify};
    my $backup_extension          = $rcall_hash->{backup_extension};
    my $delete_backup             = $rcall_hash->{delete_backup};
    my $logfile_header            = $rcall_hash->{logfile_header};
    my $rpending_complaint        = $rcall_hash->{rpending_complaint};
    my $rpending_logfile_message  = $rcall_hash->{rpending_logfile_message};

    my $rOpts              = $self->[_rOpts_];
    my $dot                = $self->[_file_extension_separator_];
    my $diagnostics_object = $self->[_diagnostics_object_];

    my $destination_stream = $rinput_hash->{'destination'};
    my $errorfile_stream   = $rinput_hash->{'errorfile'};
    my $logfile_stream     = $rinput_hash->{'logfile'};
    my $teefile_stream     = $rinput_hash->{'teefile'};
    my $debugfile_stream   = $rinput_hash->{'debugfile'};

    my $number_of_files = @{$rfiles};
    foreach my $input_file ( @{$rfiles} ) {
        my $fileroot;
        my @input_file_stat;
        my $display_name;

        #--------------------------
        # prepare this input stream
        #--------------------------
        if ($source_stream) {
            $fileroot     = "perltidy";
            $display_name = "<source_stream>";

            # If the source is from an array or string, then .LOG output
            # is only possible if a logfile stream is specified.  This prevents
            # unexpected perltidy.LOG files.  If the stream is not defined
            # then we will capture it in a string ref but it will not be
            # accessible. Previously by Perl::Tidy::DevNull (fix c255);
            if ( !defined($logfile_stream) ) {
                $logfile_stream = \my $tmp;

                # Likewise for .TEE and .DEBUG output
            }
            if ( !defined($teefile_stream) ) {
                $teefile_stream = \my $tmp;
            }
            if ( !defined($debugfile_stream) ) {
                $debugfile_stream = \my $tmp;
            }
        }
        elsif ( $input_file eq '-' ) {    # '-' indicates input from STDIN
            $fileroot     = "perltidy";   # root name to use for .ERR, .LOG, etc
            $display_name = "<stdin>";
            $in_place_modify = 0;
        }
        else {
            $fileroot     = $input_file;
            $display_name = $input_file;
            if ( !-e $input_file ) {
                Warn("skipping file: '$input_file': no matches found\n");
                next;
            }

            if ( !-f $input_file ) {
                Warn("skipping file: $input_file: not a regular file\n");
                next;
            }

            # As a safety precaution, skip zero length files.
            # If for example a source file got clobbered somehow,
            # the old .tdy or .bak files might still exist so we
            # shouldn't overwrite them with zero length files.
            if ( !-s $input_file ) {
                Warn("skipping file: $input_file: Zero size\n");
                next;
            }

            # And avoid formatting extremely large files. Since perltidy reads
            # files into memory, trying to process an extremely large file
            # could cause system problems.
            my $size_in_mb = ( -s $input_file ) / ( CONST_1024 * CONST_1024 );
            my $maximum_file_size_mb = $rOpts->{'maximum-file-size-mb'};
            if ( $size_in_mb > $maximum_file_size_mb ) {
                $size_in_mb = sprintf( "%0.1f", $size_in_mb );
                Warn(
"skipping file: $input_file: size $size_in_mb MB exceeds limit $maximum_file_size_mb; use -maxfs=i to change\n"
                );
                next;
            }

            if ( !-T $input_file && !$rOpts->{'force-read-binary'} ) {
                Warn("skipping file: $input_file: Non-text (override with -f)\n"
                );
                next;
            }

            # Input file must be writable for -b -bm='copy'.  We must catch
            # this early to prevent encountering trouble after unlinking the
            # previous backup.
            if ( $in_place_modify && !-w $input_file ) {
                my $backup_method = $rOpts->{'backup-method'};
                if ( defined($backup_method) && $backup_method eq 'copy' ) {
                    Warn(
"skipping file '$input_file' for -b option: file reported as non-writable\n"
                    );
                    next;
                }
            }

            # we should have a valid filename now
            $fileroot        = $input_file;
            @input_file_stat = stat($input_file);

            if ( $OSNAME eq 'VMS' ) {
                ( $fileroot, $dot ) = check_vms_filename($fileroot);
                $self->[_file_extension_separator_] = $dot;
            }

            # add option to change path here
            if ( defined( $rOpts->{'output-path'} ) ) {

                my ( $base, $old_path_uu ) = fileparse($fileroot);
                my $new_path = $rOpts->{'output-path'};
                if ( !-d $new_path ) {
                    mkdir($new_path)    # Default MODE is 0777
                      or
                      Die("unable to create directory $new_path: $OS_ERROR\n");
                }
                my $path = $new_path;
                $fileroot = File::Spec->catfile( $path, $base );
                if ( !$fileroot ) {
                    Die(<<EOM);
------------------------------------------------------------------------
Problem combining $new_path and $base to make a filename; check -opath
------------------------------------------------------------------------
EOM
                }
            }
        }

        # Skip files with same extension as the output files because
        # this can lead to a messy situation with files like
        # script.tdy.tdy.tdy ... or worse problems ...  when you
        # rerun perltidy over and over with wildcard input.
        if (
            !$source_stream
            && (   $input_file =~ /$forbidden_file_extensions/
                || $input_file eq 'DIAGNOSTICS' )
          )
        {
            Warn("skipping file: $input_file: wrong extension\n");
            next;
        }

        # copy source to a string buffer, decoding from utf8 if necessary
        my (
            $rinput_string,
            $is_encoded_data,
            $decoded_input_as,
            $encoding_log_message,
            $length_function,

        ) = $self->get_decoded_string_buffer( $input_file, $display_name );

        # Skip this file on any error
        next if ( !defined($rinput_string) );

        # If we are formatting a named file, skip if it looks like a markup
        # language.  Do not do this for a non-named file (it could be a glob
        # from an editor).
        # Examples of valid perl starting text: '<<END' '<>' '<<>>'
        if ( ${$rinput_string} =~ /^\s*<[^<>]/ ) {
            my $is_named_file = $number_of_files > 0 && !$line_range_clipped;
            if ( is_not_perl( $rinput_string, $input_file, $is_named_file ) ) {
                Warn(
                    "skipping file: $input_file: does not look like Perl code\n"
                );
                next;
            }
        }

        # Register this file name with the Diagnostics package, if any.
        $diagnostics_object->set_input_file($input_file)
          if ($diagnostics_object);

        # The (possibly decoded) input is now in string ref $rinput_string.
        # Now prepare the output stream and error logger.

        #--------------------------
        # prepare the output stream
        #--------------------------
        my $output_file;
        my $output_name = EMPTY_STRING;
        my $actual_output_extension;

        if ( $rOpts->{'outfile'} ) {

            if ( $number_of_files <= 1 ) {

                if ( $rOpts->{'standard-output'} ) {
                    my $saw_pbp = $self->[_saw_pbp_];
                    my $msg     = "You may not use -o and -st together";
                    $msg .= " (-pbp contains -st; see manual)" if ($saw_pbp);
                    Die("$msg\n");
                }

                if ($destination_stream) {
                    Die(
"You may not specify a destination array and -o together\n"
                    );
                }

                if ( defined( $rOpts->{'output-path'} ) ) {
                    Die("You may not specify -o and -opath together\n");
                }

                if ( defined( $rOpts->{'output-file-extension'} ) ) {
                    Die("You may not specify -o and -oext together\n");
                }

                $output_file = $rOpts->{outfile};
                $output_name = $output_file;

                # make sure user gives a file name after -o
                if ( $output_file =~ /^-/ ) {
                    Die("You must specify a valid filename after -o\n");
                }

                # do not overwrite input file with -o
                if ( @input_file_stat && ( $output_file eq $input_file ) ) {
                    Die("Use 'perltidy -b $input_file' to modify in-place\n");
                }
            }
            else {
                Die("You may not use -o with more than one input file\n");
            }
        }
        elsif ( $rOpts->{'standard-output'} ) {
            if ($destination_stream) {
                my $saw_pbp = $self->[_saw_pbp_];
                my $msg =
                  "You may not specify a destination array and -st together\n";
                $msg .= " (-pbp contains -st; see manual)" if ($saw_pbp);
                Die("$msg\n");
            }
            $output_file = '-';
            $output_name = "<stdout>";

            if ( $number_of_files <= 1 ) {
            }
            else {
                Die("You may not use -st with more than one input file\n");
            }
        }
        elsif ($destination_stream) {

            $output_file = $destination_stream;
            $output_name = "<destination_stream>";
        }
        elsif ($source_stream) {    # source but no destination goes to stdout
            $output_file = '-';
            $output_name = "<stdout>";
        }
        elsif ( $input_file eq '-' ) {
            $output_file = '-';
            $output_name = "<stdout>";
        }
        else {
            if ($in_place_modify) {
                $output_name = $display_name;
            }
            else {
                $actual_output_extension = $output_extension;
                $output_file             = $fileroot . $output_extension;
                $output_name             = $output_file;
            }
        }

        # prepare standard output in case of a dump to stdout
        if ( $is_encoded_data && $self->[_dump_to_stdout_] ) {
            binmode *STDOUT, ':encoding(UTF-8)';
        }

        $rstatus->{'file_count'} += 1;
        $rstatus->{'output_name'}     = $output_name;
        $rstatus->{'iteration_count'} = 0;
        $rstatus->{'converged'}       = 0;

        #------------------------------------------
        # initialize the error logger for this file
        #------------------------------------------
        my $warning_file = $fileroot . $dot . "ERR";
        if ($errorfile_stream) { $warning_file = $errorfile_stream }
        my $log_file = $fileroot . $dot . "LOG";
        if ($logfile_stream) { $log_file = $logfile_stream }

        # The logger object handles warning messages, logfile messages,
        # and can supply basic run information to lower level routines.
        my $logger_object = Perl::Tidy::Logger->new(
            rOpts           => $rOpts,
            log_file        => $log_file,
            warning_file    => $warning_file,
            fh_stderr       => $fh_stderr,
            display_name    => $display_name,
            is_encoded_data => $is_encoded_data,
        );
        $logger_object->write_logfile_entry($logfile_header);
        $logger_object->write_logfile_entry($encoding_log_message)
          if ($encoding_log_message);

        # Now we can add any pending messages to the log
        if ( ${$rpending_logfile_message} ) {
            $logger_object->write_logfile_entry( ${$rpending_logfile_message} );
        }
        if ( ${$rpending_complaint} ) {
            $logger_object->complain( ${$rpending_complaint} );
        }

        # additional parameters needed by lower level routines
        $self->[_actual_output_extension_] = $actual_output_extension;
        $self->[_debugfile_stream_]        = $debugfile_stream;
        $self->[_decoded_input_as_]        = $decoded_input_as;
        $self->[_destination_stream_]      = $destination_stream;
        $self->[_display_name_]            = $display_name;
        $self->[_fileroot_]                = $fileroot;
        $self->[_is_encoded_data_]         = $is_encoded_data;
        $self->[_length_function_]         = $length_function;
        $self->[_logger_object_]           = $logger_object;
        $self->[_output_file_]             = $output_file;
        $self->[_teefile_stream_]          = $teefile_stream;
        $self->[_input_copied_verbatim_]   = 0;
        $self->[_input_output_difference_] = 1;    ## updated later if -b used

        #--------------------
        # process this buffer
        #--------------------
        my $routput_string = $self->process_filter_layer($rinput_string);

        #------------------------------------------------
        # send the tidied output to its final destination
        #------------------------------------------------
        if ( $rOpts->{'format'} eq 'tidy' && defined($routput_string) ) {

            $self->write_tidy_output(
                {
                    routput_string   => $routput_string,
                    rinput_file_stat => \@input_file_stat,
                    in_place_modify  => $in_place_modify,
                    input_file       => $input_file,
                    backup_extension => $backup_extension,
                    delete_backup    => $delete_backup,
                }
            );
        }

        $logger_object->finish()
          if ($logger_object);
    } ## end loop over files

    return;
} ## end sub process_all_files

sub write_tidy_output {

    my ( $self, $rcall_hash ) = @_;

    # Write tidied output in '$routput_string' to its final destination

    my $routput_string   = $rcall_hash->{routput_string};
    my $rinput_file_stat = $rcall_hash->{rinput_file_stat};
    my $in_place_modify  = $rcall_hash->{in_place_modify};
    my $input_file       = $rcall_hash->{input_file};
    my $backup_extension = $rcall_hash->{backup_extension};
    my $delete_backup    = $rcall_hash->{delete_backup};

    my $rOpts           = $self->[_rOpts_];
    my $is_encoded_data = $self->[_is_encoded_data_];
    my $output_file     = $self->[_output_file_];

    # There are three main output paths:

    #-------------------------------------------------------------------------
    # PATH 1: $output_file is not defined: --backup and modify in-place option
    #-------------------------------------------------------------------------
    if ($in_place_modify) {

        # For -b option, leave the file unchanged if a severe error caused
        # formatting to be skipped. Otherwise we will overwrite any backup.
        if ( !$self->[_input_copied_verbatim_] ) {

            my $backup_method = $rOpts->{'backup-method'};

            #-------------------------------------------------------------
            # PATH 1a: -bm='copy': uses newer version in which original is
            # copied to the backup and rewritten; see git #103.
            #-------------------------------------------------------------
            if ( defined($backup_method) && $backup_method eq 'copy' ) {
                $self->backup_method_copy(
                    $input_file,       $routput_string,
                    $backup_extension, $delete_backup
                );
            }

            #-------------------------------------------------------------
            # PATH 1b: -bm='move': uses older version, where original is
            # moved to the backup and formatted output goes to a new file.
            #-------------------------------------------------------------
            else {
                $self->backup_method_move(
                    $input_file,       $routput_string,
                    $backup_extension, $delete_backup
                );
            }
        }
    }

    #--------------------------------------------------------------------------
    # PATH 2: $output_file is a reference (=destination_stream): send output to
    # a destination stream ref received from an external perl program. We use
    # a sub to do this because the encoding rules are a bit tricky.
    #--------------------------------------------------------------------------
    elsif ( ref($output_file) ) {
        $self->copy_buffer_to_external_ref( $routput_string, $output_file );
    }

    #--------------------------------------------------------------------------
    # PATH 3: $output_file is named file or '-'; send output to the file system
    #--------------------------------------------------------------------------
    else {

        #--------------------------
        # PATH 3a: output to STDOUT
        #--------------------------
        if ( $output_file eq '-' ) {
            my $fh = *STDOUT;
            if ($is_encoded_data) { binmode $fh, ":raw:encoding(UTF-8)" }
            else                  { binmode $fh }
            $fh->print( ${$routput_string} );
        }

        #--------------------------------
        # PATH 3b: output to a named file
        #--------------------------------
        else {
            if ( open( my $fh, '>', $output_file ) ) {
                if ($is_encoded_data) { binmode $fh, ":raw:encoding(UTF-8)" }
                else                  { binmode $fh }
                $fh->print( ${$routput_string} );
                $fh->close() or Die("Cannot close '$output_file': $OS_ERROR\n");
            }
            else {
                Die("Cannot open $output_file to write: $OS_ERROR\n");
            }

            # set output file ownership and permissions if appropriate
            if ( $output_file && -f $output_file && !-l $output_file ) {
                if ( @{$rinput_file_stat} ) {
                    $self->set_output_file_permissions( $output_file,
                        \@{$rinput_file_stat}, $in_place_modify );
                }
            }
        }

        # Save diagnostic info
        if ($is_encoded_data) {
            $rstatus->{'output_encoded_as'} = 'UTF-8';
        }
    }

    return;

} ## end sub write_tidy_output

sub process_filter_layer {

    my ( $self, $rinput_string ) = @_;

    # This is the filter layer of processing.
    # Do all requested formatting on the string ref '$rinput_string', including
    # any pre- and post-processing with filters.
    # Returns:
    #   $routput_string = ref to tidied output if in 'tidy' mode
    #   (nothing) if not in 'tidy' mode [these modes handle output separately]

    # Total formatting is done with these layers of subroutines:
    #   perltidy                - main routine; checks run parameters
    #   process_all_files       - main loop to process all files;
    #  *process_filter_layer    - do any pre and post processing; *THIS LAYER
    #   process_iteration_layer - handle any iterations on formatting
    #   process_single_case     - solves one formatting problem

    # Data Flow in this layer:
    #  $rinput_string
    #   -> optional prefilter operations
    #     -> [ formatting by sub process_iteration_layer ]
    #       -> return if not in 'tidy' mode
    #         -> optional postfilter operations
    #           -> $routput_string

    # What is done based on format type:
    #  utf8 decoding is done for all format types
    #  prefiltering is applied to all format types
    #   - because it may be needed to get through the tokenizer
    #  postfiltering is only done for format='tidy'
    #   - not appropriate for html text, which has already been output
    #  encoding of decoded output is only done for format='tidy'
    #   - because html does its own encoding; user formatter does what it wants

    # Be sure the string we received is defined
    if ( !defined($rinput_string) ) {
        Fault("bad call: the source string ref \$rinput_string is undefined\n");
    }
    if ( ref($rinput_string) ne 'SCALAR' ) {
        Fault("bad call: the source string ref is not SCALAR\n");
    }

    my $rOpts         = $self->[_rOpts_];
    my $logger_object = $self->[_logger_object_];

    # vars for --line-range-tidy filter, if needed
    my @input_lines_pre;
    my @input_lines_post;

    # vars for checking assertions, if needed
    my $digest_input;
    my $saved_input_buf;

    # var for checking --noadd-terminal-newline
    my $chomp_terminal_newline;

    # Setup post-filter vars; these apply to 'tidy' mode only
    if ( $rOpts->{'format'} eq 'tidy' ) {

        #---------------------------------------------------------------------
        # for --line-range-tidy, clip '$rinput_string' to a limited line range
        #---------------------------------------------------------------------
        my $line_tidy_begin = $self->[_line_tidy_begin_];
        if ($line_tidy_begin) {

            my @input_lines = split /^/, ${$rinput_string};

            my $num = @input_lines;
            if ( $line_tidy_begin > $num ) {
                Die(<<EOM);
#--line-range-tidy=n1:n2 has n1=$line_tidy_begin which exceeds max line number of $num
EOM
            }
            else {
                my $line_tidy_end = $self->[_line_tidy_end_];
                if ( !defined($line_tidy_end) || $line_tidy_end > $num ) {
                    $line_tidy_end = $num;
                }
                my $input_string = join EMPTY_STRING,
                  @input_lines[ $line_tidy_begin - 1 .. $line_tidy_end - 1 ];
                $rinput_string = \$input_string;

                @input_lines_pre  = @input_lines[ 0 .. $line_tidy_begin - 2 ];
                @input_lines_post = @input_lines[ $line_tidy_end .. $num - 1 ];
            }
        }

        #------------------------------------------
        # evaluate MD5 sum of input file, if needed
        #------------------------------------------
        if (   $rOpts->{'assert-tidy'}
            || $rOpts->{'assert-untidy'}
            || $rOpts->{'backup-and-modify-in-place'} )
        {
            $digest_input    = $md5_hex->( ${$rinput_string} );
            $saved_input_buf = ${$rinput_string};
        }

        # When -noadd-terminal-newline is set, and the input does not
        # have a newline, then we remove the final newline of the output
        $chomp_terminal_newline = !$rOpts->{'add-terminal-newline'}
          && substr( ${$rinput_string}, -1, 1 ) !~ /\n/;

    }

    #-----------------------------------------------------------------------
    # Apply any prefilter. The prefilter is a code reference that will be
    # applied to the source before tokenizing.  Note that we are doing this
    # for all format types ('tidy', 'html', 'user') because it may be needed
    # to avoid tokenization errors.
    #-----------------------------------------------------------------------
    my $prefilter = $self->[_prefilter_];
    if ($prefilter) {
        my $input_string = $prefilter->( ${$rinput_string} );
        $rinput_string = \$input_string;
    }

    #-------------------------------------------
    # Format contents of string '$rinput_string'
    #-------------------------------------------
    my $routput_string = $self->process_iteration_layer($rinput_string);

    #-------------------------------
    # All done if not in 'tidy' mode
    #-------------------------------
    if ( $rOpts->{'format'} ne 'tidy' ) {
        return;
    }

    #---------------------
    # apply any postfilter
    #---------------------
    my $postfilter = $self->[_postfilter_];
    if ($postfilter) {
        my $output_string = $postfilter->( ${$routput_string} );
        $routput_string = \$output_string;
    }

    if ( defined($digest_input) ) {
        my $digest_output = $md5_hex->( ${$routput_string} );
        $self->[_input_output_difference_] = $digest_output ne $digest_input;
    }

    #-----------------------------------------------------
    # check for changes if requested by 'assert-...' flags
    #-----------------------------------------------------
    if ( $rOpts->{'assert-tidy'} ) {
        if ( $self->[_input_output_difference_] ) {
            my $diff_msg =
              compare_string_buffers( \$saved_input_buf, $routput_string );
            $logger_object->warning(<<EOM);
assertion failure: '--assert-tidy' is set but output differs from input
EOM
            $logger_object->interrupt_logfile();
            $logger_object->warning( $diff_msg . "\n" );
            $logger_object->resume_logfile();
        }
    }

    if ( $rOpts->{'assert-untidy'} ) {
        if ( !$self->[_input_output_difference_] ) {
            $logger_object->warning(
"assertion failure: '--assert-untidy' is set but output equals input\n"
            );
        }
    }

    #----------------------------------------
    # do --line-range-tidy line recombination
    #----------------------------------------
    if ( @input_lines_pre || @input_lines_post ) {
        my $str_pre       = join EMPTY_STRING, @input_lines_pre;
        my $str_post      = join EMPTY_STRING, @input_lines_post;
        my $output_string = $str_pre . ${$routput_string} . $str_post;
        $routput_string = \$output_string;
    }

    #-----------------------------------------
    # handle a '--noadd-terminal-newline' flag
    #-----------------------------------------
    if ($chomp_terminal_newline) {
        chomp ${$routput_string};
    }

    #-------------------------------------------------------------
    # handle --preserve-line-endings or -output-line-endings flags
    #-------------------------------------------------------------
    # The native line separator has been used in all intermediate
    # iterations and filter operations until here so that string
    # operations work ok.
    if ( $self->[_line_separator_] ne "\n" ) {
        my $line_separator = $self->[_line_separator_];
        my @output_lines   = split /^/, ${$routput_string};
        foreach my $line (@output_lines) {

            # must check chomp because last line might not have a newline
            # if --noadd-terminal-newline is also set (c283)
            if ( chomp $line ) {
                $line .= $line_separator;
            }
        }
        my $output_string = join EMPTY_STRING, @output_lines;
        $routput_string = \$output_string;
    }

    return $routput_string;
} ## end sub process_filter_layer

# For safety, set an upper bound on number of iterations before stopping.
# The average number of iterations is 2. No known cases exceed 5.
use constant ITERATION_LIMIT => 6;

sub process_iteration_layer {

    my ( $self, $rinput_string ) = @_;

    # This is the iteration layer of processing.
    # Do all formatting, iterating if requested, on the source $rinput_string
    # Output depends on format type:
    #   For 'tidy' formatting, output goes to sink object
    #   For 'html' formatting, output goes to the ultimate destination
    #   For 'user' formatting, user formatter handles output

    # Total formatting is done with these layers of subroutines:
    #   perltidy                - main routine; checks run parameters
    #   process_all_files       - main loop to process all files;
    #   process_filter_layer    - do any pre and post processing
    #  *process_iteration_layer - do any iterations on formatting; *THIS LAYER
    #   process_single_case     - solves one formatting problem

    # Data Flow in this layer:
    #      $rinput_string -> [ loop over iterations ] -> $routput_string

    my $diagnostics_object = $self->[_diagnostics_object_];
    my $display_name       = $self->[_display_name_];
    my $fileroot           = $self->[_fileroot_];
    my $is_encoded_data    = $self->[_is_encoded_data_];
    my $length_function    = $self->[_length_function_];
    my $logger_object      = $self->[_logger_object_];
    my $rOpts              = $self->[_rOpts_];
    my $user_formatter     = $self->[_user_formatter_];

    # make a debugger object if requested
    my $debugger_object;
    if ( $rOpts->{DEBUG} ) {
        my $debug_file = $self->[_debugfile_stream_]
          || $fileroot . $self->make_file_extension('DEBUG');
        $debugger_object =
          Perl::Tidy::Debugger->new( $debug_file, $is_encoded_data );
    }

    # make a tee file handle if requested
    my $fh_tee;
    my $tee_file;
    if (   $rOpts->{'tee-pod'}
        || $rOpts->{'tee-block-comments'}
        || $rOpts->{'tee-side-comments'} )
    {
        $tee_file = $self->[_teefile_stream_]
          || $fileroot . $self->make_file_extension('TEE');
        $fh_tee = Perl::Tidy::streamhandle( $tee_file, 'w', $is_encoded_data );
        if ( !$fh_tee ) {
            Warn("couldn't open TEE file $tee_file: $OS_ERROR\n");
        }
    }

    # vars for iterations and convergence test
    my $max_iterations = 1;
    my $convergence_log_message;
    my %saw_md5;

    # Only 'tidy' formatting can use multiple iterations
    if ( $rOpts->{'format'} eq 'tidy' ) {

        # check iteration count and quietly fix if necessary:
        # - iterations option only applies to code beautification mode
        # - the convergence check should stop most runs on iteration 2, and
        #   virtually all on iteration 3.  We allow up to ITERATION_LIMIT.
        $max_iterations = $rOpts->{'iterations'};
        if ( !defined($max_iterations)
            || $max_iterations <= 0 )
        {
            $max_iterations = 1;
        }

        if ( $max_iterations > ITERATION_LIMIT ) {
            $max_iterations = ITERATION_LIMIT;
        }

        # get starting MD5 sum for convergence test
        if ( $max_iterations > 1 ) {
            my $digest = $md5_hex->( ${$rinput_string} );
            $saw_md5{$digest} = 0;
        }
    }

    # save objects to allow redirecting output during iterations
    my $logger_object_final = $logger_object;
    my $iteration_of_formatter_convergence;
    my $routput_string;

    #---------------------
    # Loop over iterations
    #---------------------
    foreach my $iter ( 1 .. $max_iterations ) {

        $rstatus->{'iteration_count'} += 1;

        # create a string to capture the output
        my $sink_buffer = EMPTY_STRING;
        $routput_string = \$sink_buffer;

        # Save logger, debugger and tee output only on pass 1 because:
        # (1) line number references must be to the starting
        # source, not an intermediate result, and
        # (2) we need to know if there are errors so we can stop the
        # iterations early if necessary.
        # (3) the tee option only works on first pass if comments are also
        # being deleted.
        if ( $iter > 1 ) {

            $debugger_object->close_debug_file()
              if ($debugger_object);

            if (   $fh_tee
                && $fh_tee->can('close')
                && !ref($tee_file)
                && $tee_file ne '-' )
            {
                $fh_tee->close()
                  or Warn("couldn't close TEE file $tee_file: $OS_ERROR\n");
            }

            $debugger_object = undef;
            $logger_object   = undef;
            $fh_tee          = undef;
        }

        #---------------------------------
        # create a formatter for this file
        #---------------------------------

        my $formatter;

        if ($user_formatter) {
            $formatter = $user_formatter;
        }
        elsif ( $rOpts->{'format'} eq 'html' ) {

            my $html_toc_extension =
              $self->make_file_extension( $rOpts->{'html-toc-extension'},
                'toc' );

            my $html_src_extension =
              $self->make_file_extension( $rOpts->{'html-src-extension'},
                'src' );

            $formatter = Perl::Tidy::HtmlWriter->new(
                input_file         => $fileroot,
                html_file          => $self->[_output_file_],
                extension          => $self->[_actual_output_extension_],
                html_toc_extension => $html_toc_extension,
                html_src_extension => $html_src_extension,
            );
        }
        elsif ( $rOpts->{'format'} eq 'tidy' ) {
            $formatter = Perl::Tidy::Formatter->new(
                logger_object      => $logger_object,
                diagnostics_object => $diagnostics_object,
                sink_object        => $routput_string,
                length_function    => $length_function,
                is_encoded_data    => $is_encoded_data,
                fh_tee             => $fh_tee,
            );
        }
        else {
            Die("I don't know how to do -format=$rOpts->{'format'}\n");
        }

        if ( !$formatter ) {
            Die("Unable to continue with $rOpts->{'format'} formatting\n");
        }

        #-----------------------------------
        # create the tokenizer for this file
        #-----------------------------------
        my $tokenizer = Perl::Tidy::Tokenizer->new(
            source_object      => $rinput_string,
            logger_object      => $logger_object,
            debugger_object    => $debugger_object,
            diagnostics_object => $diagnostics_object,
            rOpts              => $rOpts,
            starting_level     => $rOpts->{'starting-indentation-level'},
        );

        #---------------------------------
        # do processing for this iteration
        #---------------------------------
        $self->process_single_case( $tokenizer, $formatter );

        #--------------
        # report errors
        #--------------

        # see if the formatter is converged
        if (   $max_iterations > 1
            && !defined($iteration_of_formatter_convergence)
            && $formatter->can('get_convergence_check') )
        {
            if ( $formatter->get_convergence_check() ) {
                $iteration_of_formatter_convergence = $iter;
                $rstatus->{'converged'} = 1;
            }
        }

        # line source for next iteration (if any) comes from the current
        # temporary output buffer
        if ( $iter < $max_iterations ) {

            $rinput_string = \$sink_buffer;

            # stop iterations if errors or converged
            my $stop_now = $self->[_input_copied_verbatim_];
            $stop_now ||= $tokenizer->get_unexpected_error_count();
            my $stopping_on_error = $stop_now;
            if ($stop_now) {
                $convergence_log_message = <<EOM;
Stopping iterations because of severe errors.
EOM
            }

            # or do convergence test
            else {

                # stop if the formatter has converged
                $stop_now ||= defined($iteration_of_formatter_convergence);

                my $digest = $md5_hex->($sink_buffer);
                if ( !defined( $saw_md5{$digest} ) ) {
                    $saw_md5{$digest} = $iter;
                }

                # do a second iteration if all ok and requested by formatter
                # to allow delayed adding/deleting of commas (git156, git143)
                elsif ( $iter == 1
                    && !$stop_now
                    && $formatter->can('want_second_iteration')
                    && $formatter->want_second_iteration() )
                {
                    ## deja vu, but do not set $stop_now
                    $saw_md5{$digest} = $iter;
                }
                else {

                    # Deja vu, stop iterating

                    $stop_now = 1;
                    my $iterm = $iter - 1;
                    if ( $saw_md5{$digest} != $iterm ) {

                        # Blinking (oscillating) between two or more stable
                        # end states.  This is unlikely to occur with normal
                        # parameters, but it can occur in stress testing
                        # with extreme parameter values, such as very short
                        # maximum line lengths.  We want to catch and fix
                        # them when they happen.
                        $rstatus->{'blinking'} = 1;
                        $convergence_log_message = <<EOM;
BLINKER. Output for iteration $iter same as for $saw_md5{$digest}.
EOM
                        $stopping_on_error ||= $convergence_log_message;
                        DEVEL_MODE
                          && print {*STDERR} $convergence_log_message;
                        $diagnostics_object->write_diagnostics(
                            $convergence_log_message)
                          if ($diagnostics_object);

# Uncomment to search for blinking states
# Warn( "$display_name: blinking; iter $iter same as for $saw_md5{$digest}\n" );

                    }
                    else {
                        $convergence_log_message = <<EOM;
Converged.  Output for iteration $iter same as for iter $iterm.
EOM
                        $diagnostics_object->write_diagnostics(
                            $convergence_log_message)
                          if ( $diagnostics_object && $iterm > 2 );
                        $rstatus->{'converged'} = 1;
                    }
                }
            }

            if ($stop_now) {

                if (DEVEL_MODE) {

                    if ( defined($iteration_of_formatter_convergence) ) {

                        # This message cannot appear unless the formatter
                        # convergence test above is temporarily skipped for
                        # testing.
                        if ( $iteration_of_formatter_convergence < $iter - 1 ) {
                            print {*STDERR}
"STRANGE Early conv in $display_name: Stopping on it=$iter, converged in formatter on $iteration_of_formatter_convergence\n";
                        }
                    }
                    elsif ( !$stopping_on_error ) {

                        # The md5 sum implies convergence but the convergence
                        # was not detected by the Formatter.  This is not
                        # critical but should be investigated.  It happened
                        # once when a line break was placed before a phantom
                        # comma under -qwaf, and a semicolon under -nasc,
                        # and has been fixed (search for 'STRANGE').
                        print {*STDERR}
"STRANGE no conv in $display_name: stopping on it=$iter, but not converged in formatter\n";
                    }
                    else {
                        ## looks ok
                    }
                }

                # we are stopping the iterations early;
                last;
            }
        } ## end if ( $iter < $max_iterations)
    } ## end loop over iterations for one source file

    $debugger_object->close_debug_file()
      if ($debugger_object);

    if (   $fh_tee
        && $fh_tee->can('close')
        && !ref($tee_file)
        && $tee_file ne '-' )
    {
        $fh_tee->close()
          or Warn("couldn't close TEE file $tee_file: $OS_ERROR\n");
    }

    # leave logger object open for additional messages
    $logger_object = $logger_object_final;
    $logger_object->write_logfile_entry($convergence_log_message)
      if ($convergence_log_message);

    return $routput_string;

} ## end sub process_iteration_layer

sub process_single_case {

    my ( $self, $tokenizer, $formatter ) = @_;

    # Run the formatter on a single defined case

    # Total formatting is done with these layers of subroutines:
    #   perltidy                - main routine; checks run parameters
    #   process_all_files       - main loop to process all files;
    #   process_filter_layer    - do any pre and post processing;
    #   process_iteration_layer - do any iterations on formatting
    #  *process_single_case     - solve one formatting problem; *THIS LAYER

    while ( my $line = $tokenizer->get_line() ) {
        $formatter->write_line($line);
    }

    # user-defined formatters are possible, and may not have a
    # sub 'finish_formatting', so we have to check
    if ( $formatter->can('finish_formatting') ) {
        my $rtok_report = $tokenizer->report_tokenization_errors();
        my $verbatim    = $formatter->finish_formatting($rtok_report);
        $self->[_input_copied_verbatim_] = $verbatim;
    }

    return;
} ## end sub process_single_case

sub copy_buffer_to_external_ref {

    my ( $self, $routput, $destination_stream ) = @_;

    # Copy $routput to the final $destination_stream,
    # encoding if the flag $encode_destination_buffer is true.

    # Data Flow:
    #    $destination_buffer -> [ encode? ] -> $destination_stream

    my $destination_buffer = EMPTY_STRING;
    if ( ref($routput) eq 'ARRAY' ) {
        $destination_buffer = join EMPTY_STRING, @{$routput};
    }
    elsif ( ref($routput) eq 'SCALAR' ) {
        $destination_buffer = ${$routput};
    }
    else {
        Fault(
            "'copy_buffer_to_external_ref' expecting ref to ARRAY or SCALAR\n");
    }

    $rstatus->{'output_encoded_as'} = EMPTY_STRING;
    my $ref_destination_stream = ref($destination_stream);

    # Encode output? Strings and arrays use special encoding rules; see:
    #   https://github.com/perltidy/perltidy/blob/master/docs/eos_flag.md
    my $encode_destination_buffer;
    if (   $ref_destination_stream eq 'SCALAR'
        || $ref_destination_stream eq 'ARRAY' )
    {
        my $rOpts = $self->[_rOpts_];
        $encode_destination_buffer =
          $rOpts->{'encode-output-strings'} && $self->[_decoded_input_as_];
    }

    # An object with a print method will use file encoding rules
    elsif ( $ref_destination_stream->can('print') ) {
        $encode_destination_buffer = $self->[_is_encoded_data_];
    }
    else {
        confess <<EOM;
------------------------------------------------------------------------
No 'print' method is defined for object of class '$ref_destination_stream'
Please check your call to Perl::Tidy::perltidy. Trace follows.
------------------------------------------------------------------------
EOM
    }

    if ($encode_destination_buffer) {
        my $encoded_buffer;
        if (
            !eval {
                $encoded_buffer =
                  Encode::encode( "UTF-8", $destination_buffer,
                    Encode::FB_CROAK | Encode::LEAVE_SRC );
                1;
            }
          )
        {

            Warn(
"Error attempting to encode output string ref; encoding not done\n"
            );
        }
        else {
            $destination_buffer = $encoded_buffer;
            $rstatus->{'output_encoded_as'} = 'UTF-8';
        }
    }

    # Send data for SCALAR, ARRAY & OBJ refs to its final destination
    if ( $ref_destination_stream eq 'SCALAR' ) {
        ${$destination_stream} = $destination_buffer;
    }
    elsif ( defined($destination_buffer) ) {
        my @lines = split /^/, $destination_buffer;
        if ( $ref_destination_stream eq 'ARRAY' ) {
            @{$destination_stream} = @lines;
        }

        # destination stream must be an object with print method
        else {
            foreach my $line (@lines) {
                $destination_stream->print($line);
            }
            if ( $ref_destination_stream->can('close') ) {
                $destination_stream->close();
            }
        }
    }
    else {

        # Empty destination buffer not going to a string ... could
        # happen for example if user deleted all pod or comments
    }
    return;
} ## end sub copy_buffer_to_external_ref

} ## end of closure for sub perltidy

sub line_diff {

    my ( $s1, $s2 ) = @_;

    # Given two strings, Return
    #  $diff_marker = a string with caret (^) symbols indicating differences
    #  $pos1 = character position of first difference; pos1=-1 if no difference

    # Form exclusive or of the strings, which has null characters where strings
    # have same common characters so non-null characters indicate character
    # differences.
    my $diff_marker = EMPTY_STRING;
    my $pos         = -1;
    my $pos1        = -1;
    if ( defined($s1) && defined($s2) ) {
        my $mask = $s1 ^ $s2;

        while ( $mask =~ /[^\0]/g ) {
            my $pos_last = $pos;
            $pos = $LAST_MATCH_START[0];
            if ( $pos1 < 0 ) { $pos1 = $pos; }
            $diff_marker .= SPACE x ( $pos - $pos_last - 1 ) . '^';

            # we could continue to mark all differences, but there is no point
            last;
        } ## end while ( $mask =~ /[^\0]/g)
    }
    return ( $diff_marker, $pos1 );
} ## end sub line_diff

sub compare_string_buffers {

    my ( $string_i, $string_o, ($max_diff_count) ) = @_;

    # Compare input and output string buffers and return a brief text
    # description of the first difference.

    # Given:
    #   $string_i = input string, or ref to input string
    #   $string_o = output string, or ref to output string
    #   $max_diff_count = optional maximum number of differences to show,
    #       default=1
    # Return:
    #   a string showing differences

    my $rbufi = ref($string_i) ? $string_i : \$string_i;
    my $rbufo = ref($string_o) ? $string_o : \$string_o;

    if ( !defined($max_diff_count) ) { $max_diff_count = 1 }

    my ( @aryi, @aryo );
    my ( $leni, $leno ) = ( 0, 0 );
    if ( defined($rbufi) ) {
        $leni = length( ${$rbufi} );
        @aryi = split /^/, ${$rbufi};
    }
    if ( defined($rbufo) ) {
        $leno = length( ${$rbufo} );
        @aryo = split /^/, ${$rbufo};
    }
    my $nlines_i = @aryi;
    my $nlines_o = @aryo;
    my $msg      = <<EOM;
Input   file length has $leni chars in $nlines_i lines
Output  file length has $leno chars in $nlines_o lines
EOM
    return $msg unless ( $leni && $leno );

    my $truncate = sub {
        my ( $str, $lenmax ) = @_;
        if ( length($str) > $lenmax ) {
            $str = substr( $str, 0, $lenmax ) . "...";
        }
        return $str;
    }; ## end $truncate = sub

    my $last_nonblank_line  = EMPTY_STRING;
    my $last_nonblank_count = 0;

    # loop over lines until we find a difference
    my $count      = 0;
    my $diff_count = 0;
    while ( @aryi && @aryo ) {
        $count++;
        my $linei = shift @aryi;
        my $lineo = shift @aryo;
        chomp $linei;
        chomp $lineo;
        if ( $linei eq $lineo ) {
            if ( length($linei) ) {
                $last_nonblank_line  = $linei;
                $last_nonblank_count = $count;
            }
            next;
        }

        #---------------------------
        # lines differ ... finish up
        #---------------------------
        my ( $line_diff, $pos1 ) = line_diff( $linei, $lineo );
        my $ch1    = $pos1 + 1;
        my $reason = "Files first differ at character $ch1 of line $count";

        my ( $leading_ws_i, $leading_ws_o ) = ( EMPTY_STRING, EMPTY_STRING );
        if ( $linei =~ /^(\s+)/ ) { $leading_ws_i = $1; }
        if ( $lineo =~ /^(\s+)/ ) { $leading_ws_o = $1; }
        if ( $leading_ws_i ne $leading_ws_o ) {
            $reason .= "; leading whitespace differs";
            if ( $leading_ws_i =~ /\t/ ) {
                $reason .= "; input has tab char";
            }
        }
        else {
            my ( $trailing_ws_i, $trailing_ws_o ) =
              ( EMPTY_STRING, EMPTY_STRING );
            if ( $linei =~ /(\s+)$/ ) { $trailing_ws_i = $1; }
            if ( $lineo =~ /(\s+)$/ ) { $trailing_ws_o = $1; }
            if ( $trailing_ws_i ne $trailing_ws_o ) {
                $reason .= "; trailing whitespace differs";
            }
        }
        $msg .= $reason . "\n";

        # limit string display length
        if ( $pos1 > 60 ) {
            my $drop = $pos1 - 40;
            $linei     = "..." . substr( $linei, $drop );
            $lineo     = "..." . substr( $lineo, $drop );
            $line_diff = SPACE x 3 . substr( $line_diff, $drop );
        }
        $linei              = $truncate->( $linei,              72 );
        $lineo              = $truncate->( $lineo,              72 );
        $last_nonblank_line = $truncate->( $last_nonblank_line, 72 );

        if ($last_nonblank_line) {
            $msg .= <<EOM;
 $last_nonblank_count:$last_nonblank_line
EOM
        }
        $line_diff = SPACE x ( 2 + length($count) ) . $line_diff;
        $msg .= <<EOM;
<$count:$linei
>$count:$lineo
$line_diff
EOM
        $diff_count++;
        last if ( $diff_count >= $max_diff_count );
    } ## end while ( @aryi && @aryo )

    if ($diff_count) { return $msg }

    #------------------------------------------------------
    # no differences found, see if one file has fewer lines
    #------------------------------------------------------
    if ( $nlines_i > $nlines_o ) {
        $msg .= <<EOM;
Files initially match file but output file has fewer lines
EOM
    }
    elsif ( $nlines_i < $nlines_o ) {
        $msg .= <<EOM;
Files initially match file but input file has fewer lines
EOM
    }
    else {
        $msg .= <<EOM;
Text in lines of file match but checksums differ. Perhaps line endings differ.
EOM
    }
    return $msg;
} ## end sub compare_string_buffers

sub fileglob_to_re {

    # modified (corrected) from version in find2perl
    my $x = shift;
    $x =~ s/([.\/^\$()])/\\$1/g;    # escape special characters
    $x =~ s/\*/.*/g;                # '*' -> '.*'
    $x =~ s/\?/./g;                 # '?' -> '.'
    return "^$x\\z";                # match whole word
} ## end sub fileglob_to_re

sub make_logfile_header {
    my ( $rOpts, $config_file, $rraw_options, $Windows_type, $readable_options )
      = @_;

    # Note: the punctuation variable '$]' is not in older versions of
    # English.pm so leave it as is to avoid failing installation tests.
    my $msg =
"perltidy version $VERSION log file on a $OSNAME system, OLD_PERL_VERSION=$]\n";
    if ($Windows_type) {
        $msg .= "Windows type is $Windows_type\n";
    }
    my $options_string = join( SPACE, @{$rraw_options} );

    if ( defined($config_file) ) {
        $msg .= "Found Configuration File >>> $config_file \n";
    }
    $msg .= "Configuration and command line parameters for this run:\n";
    $msg .= "$options_string\n";

    if ( $rOpts->{'show-options'} ) {
        $rOpts->{'logfile'} = 1;    # force logfile to be saved
        $msg .= "Final parameter set for this run\n";
        $msg .= "------------------------------------\n";

        $msg .= $readable_options;

        $msg .= "------------------------------------\n";
    }
    $msg .= "To find error messages search for 'WARNING' with your editor\n";
    return $msg;
} ## end sub make_logfile_header

sub generate_options {

    ######################################################################
    # Generate and return references to:
    #  @option_string - the list of options to be passed to Getopt::Long
    #  @defaults - the list of default options
    #  %expansion - a hash showing how all abbreviations are expanded
    #  %category - a hash giving the general category of each option
    #  %integer_option_range - valid ranges of certain options

    # Note: a few options are not documented in the man page and usage
    # message. This is because these are deprecated, experimental or debug
    # options and may or may not be retained in future versions:

    # These undocumented flags are accepted but not used:
    # --check-syntax
    # --fuzzy-line-length
    #
    # These undocumented flags are for debugging:
    # --recombine                           # used to debug line breaks
    # --short-concatenation-item-length     # used to break a '.' chain
    #
    ######################################################################

    # here is a summary of the Getopt codes:
    # <none> does not take an argument
    # =s takes a mandatory string
    # :s takes an optional string  (DO NOT USE - filenames will get eaten up)
    # =i takes a mandatory integer
    # :i takes an optional integer (NOT RECOMMENDED - can cause trouble)
    # ! does not take an argument and may be negated
    #  i.e., -foo and -nofoo are allowed
    # a double dash signals the end of the options list
    #
    #-----------------------------------------------
    # Define the option string passed to GetOptions.
    #-----------------------------------------------

    my @option_string   = ();
    my %expansion       = ();
    my %option_category = ();
    my %integer_option_range;

    # names of categories in manual
    # leading integers will allow sorting
    my @category_name = (
        '0. I/O control',
        '1. Basic formatting options',
        '2. Code indentation control',
        '3. Whitespace control',
        '4. Comment controls',
        '5. Linebreak controls',
        '6. Controlling list formatting',
        '7. Retaining or ignoring existing line breaks',
        '8. Blank line control',
        '9. Other controls',
        '10. HTML options',
        '11. pod2html options',
        '12. Controlling HTML properties',
        '13. Debugging',
    );

    #  These options are parsed directly by perltidy:
    #    help h
    #    version v
    #  However, they are included in the option set so that they will
    #  be seen in the options dump.

    # These long option names have no abbreviations or are treated specially
    @option_string = qw(
      html!
      noprofile
      nopro
      no-profile
      npro
      recombine!
      notidy
    );

    my $category = 13;    # Debugging
    foreach (@option_string) {
        my $opt = $_;     # must avoid changing the actual flag
        $opt =~ s/!$//;
        $option_category{$opt} = $category_name[$category];
    }

    $category = 11;                                       # HTML
    $option_category{html} = $category_name[$category];

    # Routine to install and check options
    my $add_option = sub {

        my ( $long_name, $short_name, $flag ) = @_;

        # Given:
        #   $long_name  = the full option name, such as 'backup-method'
        #   $short_name = the abbreviation, such as 'bm'
        #   $flag       = the Getopt code, such as '=s', see above list

        push @option_string, $long_name . $flag;
        $option_category{$long_name} = $category_name[$category];
        if ($short_name) {
            if ( $expansion{$short_name} ) {
                my $existing_name = $expansion{$short_name}->[0];
                Die(
"redefining abbreviation $short_name for $long_name; already used for $existing_name\n"
                );
            }
            $expansion{$short_name} = [$long_name];
            if ( $flag eq '!' ) {
                my $nshort_name = 'n' . $short_name;
                my $nolong_name = 'no' . $long_name;
                if ( $expansion{$nshort_name} ) {
                    my $existing_name = $expansion{$nshort_name}->[0];
                    Die(
"attempting to redefine abbreviation $nshort_name for $nolong_name; already used for $existing_name\n"
                    );
                }
                $expansion{$nshort_name} = [$nolong_name];
            }
        }
        return;
    }; ## end $add_option = sub

    # Install long option names which have a simple abbreviation.
    # Options with code '!' get standard negation ('no' for long names,
    # 'n' for abbreviations).  Categories follow the manual.

    ###########################
    $category = 0;    # I/O_Control
    ###########################
    $add_option->( 'backup-and-modify-in-place', 'b',     '!' );
    $add_option->( 'backup-file-extension',      'bext',  '=s' );
    $add_option->( 'backup-method',              'bm',    '=s' );
    $add_option->( 'character-encoding',         'enc',   '=s' );
    $add_option->( 'force-read-binary',          'f',     '!' );
    $add_option->( 'format',                     'fmt',   '=s' );
    $add_option->( 'iterations',                 'it',    '=i' );
    $add_option->( 'logfile',                    'log',   '!' );
    $add_option->( 'logfile-gap',                'g',     ':i' );
    $add_option->( 'outfile',                    'o',     '=s' );
    $add_option->( 'output-file-extension',      'oext',  '=s' );
    $add_option->( 'output-path',                'opath', '=s' );
    $add_option->( 'profile',                    'pro',   '=s' );
    $add_option->( 'quiet',                      'q',     '!' );
    $add_option->( 'standard-error-output',      'se',    '!' );
    $add_option->( 'standard-output',            'st',    '!' );
    $add_option->( 'use-unicode-gcstring',       'gcs',   '!' );
    $add_option->( 'warning-output',             'w',     '!' );
    $add_option->( 'add-terminal-newline',       'atnl',  '!' );
    $add_option->( 'line-range-tidy',            'lrt',   '=s' );
    $add_option->( 'timeout-in-seconds',         'tos',   '=i' );

    # options which are both toggle switches and values moved here
    # to hide from tidyview (which does not show category 0 flags):
    # -ole moved here from category 1
    # -sil moved here from category 2
    $add_option->( 'output-line-ending',         'ole', '=s' );
    $add_option->( 'starting-indentation-level', 'sil', '=i' );

    ########################################
    $category = 1;    # Basic formatting options
    ########################################
    $add_option->( 'check-syntax',                 'syn',  '!' );
    $add_option->( 'entab-leading-whitespace',     'et',   '=i' );
    $add_option->( 'indent-columns',               'i',    '=i' );
    $add_option->( 'maximum-line-length',          'l',    '=i' );
    $add_option->( 'variable-maximum-line-length', 'vmll', '!' );
    $add_option->( 'whitespace-cycle',             'wc',   '=i' );
    $add_option->( 'perl-syntax-check-flags',      'pscf', '=s' );
    $add_option->( 'preserve-line-endings',        'ple',  '!' );
    $add_option->( 'tabs',                         't',    '!' );
    $add_option->( 'default-tabsize',              'dt',   '=i' );
    $add_option->( 'extended-syntax',              'xs',   '!' );
    $add_option->( 'assert-tidy',                  'ast',  '!' );
    $add_option->( 'assert-untidy',                'asu',  '!' );
    $add_option->( 'encode-output-strings',        'eos',  '!' );
    $add_option->( 'sub-alias-list',               'sal',  '=s' );
    $add_option->( 'grep-alias-list',              'gal',  '=s' );
    $add_option->( 'grep-alias-exclusion-list',    'gaxl', '=s' );
    $add_option->( 'use-feature',                  'uf',   '=s' );

    ########################################
    $category = 2;    # Code indentation control
    ########################################
    $add_option->( 'continuation-indentation',             'ci',    '=i' );
    $add_option->( 'extended-continuation-indentation',    'xci',   '!' );
    $add_option->( 'minimize-continuation-indentation',    'mci',   '!' );
    $add_option->( 'line-up-parentheses',                  'lp',    '!' );
    $add_option->( 'extended-line-up-parentheses',         'xlp',   '!' );
    $add_option->( 'line-up-parentheses-exclusion-list',   'lpxl',  '=s' );
    $add_option->( 'line-up-parentheses-inclusion-list',   'lpil',  '=s' );
    $add_option->( 'outdent-keyword-list',                 'okwl',  '=s' );
    $add_option->( 'outdent-keywords',                     'okw',   '!' );
    $add_option->( 'outdent-labels',                       'ola',   '!' );
    $add_option->( 'outdent-long-quotes',                  'olq',   '!' );
    $add_option->( 'indent-closing-brace',                 'icb',   '!' );
    $add_option->( 'indent-leading-semicolon',             'ils',   '!' );
    $add_option->( 'closing-token-indentation',            'cti',   '=i' );
    $add_option->( 'closing-paren-indentation',            'cpi',   '=i' );
    $add_option->( 'closing-brace-indentation',            'cbi',   '=i' );
    $add_option->( 'closing-square-bracket-indentation',   'csbi',  '=i' );
    $add_option->( 'brace-left-and-indent',                'bli',   '!' );
    $add_option->( 'brace-left-and-indent-list',           'blil',  '=s' );
    $add_option->( 'brace-left-and-indent-exclusion-list', 'blixl', '=s' );

    ########################################
    $category = 3;    # Whitespace control
    ########################################
    $add_option->( 'add-trailing-commas',                       'atc',   '!' );
    $add_option->( 'add-lone-trailing-commas',                  'altc',  '!' );
    $add_option->( 'add-semicolons',                            'asc',   '!' );
    $add_option->( 'add-whitespace',                            'aws',   '!' );
    $add_option->( 'block-brace-tightness',                     'bbt',   '=i' );
    $add_option->( 'brace-tightness',                           'bt',    '=i' );
    $add_option->( 'delete-old-whitespace',                     'dws',   '!' );
    $add_option->( 'delete-repeated-commas',                    'drc',   '!' );
    $add_option->( 'delete-trailing-commas',                    'dtc',   '!' );
    $add_option->( 'delete-lone-trailing-commas',               'dltc',  '!' );
    $add_option->( 'delete-weld-interfering-commas',            'dwic',  '!' );
    $add_option->( 'delete-semicolons',                         'dsm',   '!' );
    $add_option->( 'function-paren-vertical-alignment',         'fpva',  '!' );
    $add_option->( 'delay-trailing-comma-operations',           'dtco',  '!' );
    $add_option->( 'keyword-paren-inner-tightness',             'kpit',  '=i' );
    $add_option->( 'keyword-paren-inner-tightness-list',        'kpitl', '=s' );
    $add_option->( 'logical-padding',                           'lop',   '!' );
    $add_option->( 'multiple-token-tightness',                  'mutt',  '=s' );
    $add_option->( 'nospace-after-keyword',                     'nsak',  '=s' );
    $add_option->( 'nowant-left-space',                         'nwls',  '=s' );
    $add_option->( 'nowant-right-space',                        'nwrs',  '=s' );
    $add_option->( 'paren-tightness',                           'pt',    '=i' );
    $add_option->( 'space-after-keyword',                       'sak',   '=s' );
    $add_option->( 'space-for-semicolon',                       'sfs',   '!' );
    $add_option->( 'space-function-paren',                      'sfp',   '!' );
    $add_option->( 'space-keyword-paren',                       'skp',   '!' );
    $add_option->( 'space-terminal-semicolon',                  'sts',   '!' );
    $add_option->( 'square-bracket-tightness',                  'sbt',   '=i' );
    $add_option->( 'square-bracket-vertical-tightness',         'sbvt',  '=i' );
    $add_option->( 'square-bracket-vertical-tightness-closing', 'sbvtc', '=i' );
    $add_option->( 'tight-secret-operators',                    'tso',   '!' );
    $add_option->( 'trim-qw',                                   'tqw',   '!' );
    $add_option->( 'trim-pod',                                  'trp',   '!' );
    $add_option->( 'want-left-space',                           'wls',   '=s' );
    $add_option->( 'want-right-space',                          'wrs',   '=s' );
    $add_option->( 'want-trailing-commas',                      'wtc',   '=s' );
    $add_option->( 'space-prototype-paren',                     'spp',   '=i' );
    $add_option->( 'space-signature-paren',                     'ssp',   '=i' );
    $add_option->( 'valign-code',                               'vc',    '!' );
    $add_option->( 'valign-block-comments',                     'vbc',   '!' );
    $add_option->( 'valign-side-comments',                      'vsc',   '!' );
    $add_option->( 'valign-exclusion-list',                     'vxl',   '=s' );
    $add_option->( 'valign-inclusion-list',                     'vil',   '=s' );
    $add_option->( 'valign-if-unless',                          'viu',   '!' );
    $add_option->( 'valign-signed-numbers',                     'vsn',   '!' );
    $add_option->( 'valign-signed-numbers-limit',               'vsnl',  '=i' );
    $add_option->( 'valign-wide-equals',                        'vwe',   '!' );
    $add_option->( 'extended-block-tightness',                  'xbt',   '!' );
    $add_option->( 'extended-block-tightness-list',             'xbtl',  '=s' );
    $add_option->( 'qw-as-function',                            'qwaf',  '!' );

    ########################################
    $category = 4;    # Comment controls
    ########################################
    $add_option->( 'closing-side-comment-else-flag',      'csce',  '=i' );
    $add_option->( 'closing-side-comment-interval',       'csci',  '=i' );
    $add_option->( 'closing-side-comment-list',           'cscl',  '=s' );
    $add_option->( 'closing-side-comment-exclusion-list', 'cscxl', '=s' );
    $add_option->( 'closing-side-comment-maximum-text',   'csct',  '=i' );
    $add_option->( 'closing-side-comment-prefix',         'cscp',  '=s' );
    $add_option->( 'closing-side-comment-warnings',       'cscw',  '!' );
    $add_option->( 'closing-side-comments',               'csc',   '!' );
    $add_option->( 'closing-side-comments-balanced',      'cscb',  '!' );
    $add_option->( 'code-skipping',                       'cs',    '!' );
    $add_option->( 'code-skipping-begin',                 'csb',   '=s' );
    $add_option->( 'code-skipping-end',                   'cse',   '=s' );
    $add_option->( 'code-skipping-from-start',            'csfs',  '!' );
    $add_option->( 'format-skipping',                     'fs',    '!' );
    $add_option->( 'format-skipping-begin',               'fsb',   '=s' );
    $add_option->( 'format-skipping-end',                 'fse',   '=s' );
    $add_option->( 'detect-format-skipping-from-start',   'dfsfs', '!' );
    $add_option->( 'hanging-side-comments',               'hsc',   '!' );
    $add_option->( 'indent-block-comments',               'ibc',   '!' );
    $add_option->( 'indent-spaced-block-comments',        'isbc',  '!' );
    $add_option->( 'fixed-position-side-comment',         'fpsc',  '=i' );
    $add_option->( 'minimum-space-to-comment',            'msc',   '=i' );
    $add_option->( 'non-indenting-braces',                'nib',   '!' );
    $add_option->( 'non-indenting-brace-prefix',          'nibp',  '=s' );
    $add_option->( 'outdent-long-comments',               'olc',   '!' );
    $add_option->( 'outdent-static-block-comments',       'osbc',  '!' );
    $add_option->( 'skip-formatting-except-id',           'sfei',  '=s' );
    $add_option->( 'static-block-comment-prefix',         'sbcp',  '=s' );
    $add_option->( 'static-block-comments',               'sbc',   '!' );
    $add_option->( 'static-side-comment-prefix',          'sscp',  '=s' );
    $add_option->( 'static-side-comments',                'ssc',   '!' );
    $add_option->( 'ignore-side-comment-lengths',         'iscl',  '!' );
    $add_option->( 'ignore-perlcritic-comments',          'ipc',   '!' );

    ########################################
    $category = 5;    # Linebreak controls
    ########################################
    $add_option->( 'add-newlines',                            'anl',   '!' );
    $add_option->( 'block-brace-vertical-tightness',          'bbvt',  '=i' );
    $add_option->( 'block-brace-vertical-tightness-list',     'bbvtl', '=s' );
    $add_option->( 'brace-follower-vertical-tightness',       'bfvt',  '=i' );
    $add_option->( 'brace-vertical-tightness',                'bvt',   '=i' );
    $add_option->( 'brace-vertical-tightness-closing',        'bvtc',  '=i' );
    $add_option->( 'cuddled-else',                            'ce',    '!' );
    $add_option->( 'cuddled-block-list',                      'cbl',   '=s' );
    $add_option->( 'cuddled-block-list-exclusive',            'cblx',  '!' );
    $add_option->( 'cuddled-break-option',                    'cbo',   '=i' );
    $add_option->( 'cuddled-paren-brace',                     'cpb',   '!' );
    $add_option->( 'cuddled-paren-brace-weld',                'cpbw',  '!' );
    $add_option->( 'delete-old-newlines',                     'dnl',   '!' );
    $add_option->( 'opening-brace-always-on-right',           'bar',   '!' );
    $add_option->( 'opening-brace-on-new-line',               'bl',    '!' );
    $add_option->( 'opening-hash-brace-right',                'ohbr',  '!' );
    $add_option->( 'opening-paren-right',                     'opr',   '!' );
    $add_option->( 'opening-square-bracket-right',            'osbr',  '!' );
    $add_option->( 'opening-anonymous-sub-brace-on-new-line', 'asbl',  '!' );
    $add_option->( 'opening-sub-brace-on-new-line',           'sbl',   '!' );
    $add_option->( 'paren-vertical-tightness',                'pvt',   '=i' );
    $add_option->( 'paren-vertical-tightness-closing',        'pvtc',  '=i' );
    $add_option->( 'weld-nested-containers',                  'wn',    '!' );
    $add_option->( 'weld-nested-exclusion-list',              'wnxl',  '=s' );
    $add_option->( 'weld-fat-comma',                          'wfc',   '!' );
    $add_option->( 'space-backslash-quote',                   'sbq',   '=i' );
    $add_option->( 'stack-closing-block-brace',               'scbb',  '!' );
    $add_option->( 'stack-closing-hash-brace',                'schb',  '!' );
    $add_option->( 'stack-closing-paren',                     'scp',   '!' );
    $add_option->( 'stack-closing-square-bracket',            'scsb',  '!' );
    $add_option->( 'stack-opening-hash-brace',                'sohb',  '!' );
    $add_option->( 'stack-opening-paren',                     'sop',   '!' );
    $add_option->( 'stack-opening-square-bracket',            'sosb',  '!' );

    # NOTE: --vt and --vtc are actually expansions now, so these two lines
    # might eventually be removed. But search for 'msdos' to see notes about
    # an issue with 'msdos' that could be a problem if msdos is still used.
    $add_option->( 'vertical-tightness',         'vt',  '=i' );
    $add_option->( 'vertical-tightness-closing', 'vtc', '=i' );

    $add_option->( 'want-break-after',                       'wba',   '=s' );
    $add_option->( 'want-break-before',                      'wbb',   '=s' );
    $add_option->( 'break-after-all-operators',              'baao',  '!' );
    $add_option->( 'break-before-all-operators',             'bbao',  '!' );
    $add_option->( 'keep-interior-semicolons',               'kis',   '!' );
    $add_option->( 'one-line-block-semicolons',              'olbs',  '=i' );
    $add_option->( 'one-line-block-nesting',                 'olbn',  '=i' );
    $add_option->( 'one-line-block-exclusion-list',          'olbxl', '=s' );
    $add_option->( 'break-before-hash-brace',                'bbhb',  '=i' );
    $add_option->( 'break-before-hash-brace-and-indent',     'bbhbi', '=i' );
    $add_option->( 'break-before-square-bracket',            'bbsb',  '=i' );
    $add_option->( 'break-before-square-bracket-and-indent', 'bbsbi', '=i' );
    $add_option->( 'break-before-paren',                     'bbp',   '=i' );
    $add_option->( 'break-before-paren-and-indent',          'bbpi',  '=i' );
    $add_option->( 'brace-left-list',                        'bll',   '=s' );
    $add_option->( 'brace-left-exclusion-list',              'blxl',  '=s' );
    $add_option->( 'break-after-labels',                     'bal',   '=i' );
    $add_option->( 'pack-operator-types',                    'pot',   '=s' );

    # This was an experiment mentioned in git #78, originally named -bopl.
    $add_option->( 'break-open-compact-parens', 'bocp', '=s' );

    ########################################
    $category = 6;    # Controlling list formatting
    ########################################
    $add_option->( 'break-at-old-comma-breakpoints', 'boc',  '!' );
    $add_option->( 'break-at-old-comma-types',       'boct', '=s' );
    $add_option->( 'break-at-trailing-comma-types',  'btct', '=s' );
    $add_option->( 'comma-arrow-breakpoints',        'cab',  '=i' );
    $add_option->( 'maximum-fields-per-table',       'mft',  '=s' );

    ########################################
    $category = 7;    # Retaining or ignoring existing line breaks
    ########################################
    $add_option->( 'break-at-old-keyword-breakpoints',   'bok', '!' );
    $add_option->( 'break-at-old-logical-breakpoints',   'bol', '!' );
    $add_option->( 'break-at-old-method-breakpoints',    'bom', '!' );
    $add_option->( 'break-at-old-semicolon-breakpoints', 'bos', '!' );
    $add_option->( 'break-at-old-ternary-breakpoints',   'bot', '!' );
    $add_option->( 'break-at-old-attribute-breakpoints', 'boa', '!' );
    $add_option->( 'keep-old-breakpoints-before',        'kbb', '=s' );
    $add_option->( 'keep-old-breakpoints-after',         'kba', '=s' );
    $add_option->( 'ignore-old-breakpoints',             'iob', '!' );

    ########################################
    $category = 8;    # Blank line control
    ########################################
    $add_option->( 'blanks-before-blocks',            'bbb',  '!' );
    $add_option->( 'blanks-before-comments',          'bbc',  '!' );
    $add_option->( 'blank-lines-before-subs',         'blbs', '=i' );
    $add_option->( 'blank-lines-before-packages',     'blbp', '=i' );
    $add_option->( 'long-block-line-count',           'lbl',  '=i' );
    $add_option->( 'maximum-consecutive-blank-lines', 'mbl',  '=i' );
    $add_option->( 'keep-old-blank-lines',            'kbl',  '=i' );
    $add_option->( 'keep-old-blank-lines-exceptions', 'kblx', '=s' );

    $add_option->( 'keyword-group-blanks-list',         'kgbl', '=s' );
    $add_option->( 'keyword-group-blanks-size',         'kgbs', '=s' );
    $add_option->( 'keyword-group-blanks-repeat-count', 'kgbr', '=i' );
    $add_option->( 'keyword-group-blanks-before',       'kgbb', '=i' );
    $add_option->( 'keyword-group-blanks-after',        'kgba', '=i' );
    $add_option->( 'keyword-group-blanks-inside',       'kgbi', '!' );
    $add_option->( 'keyword-group-blanks-delete',       'kgbd', '!' );

    $add_option->( 'blank-lines-after-opening-block',       'blao',  '=i' );
    $add_option->( 'blank-lines-before-closing-block',      'blbc',  '=i' );
    $add_option->( 'blank-lines-after-opening-block-list',  'blaol', '=s' );
    $add_option->( 'blank-lines-before-closing-block-list', 'blbcl', '=s' );

    ########################################
    $category = 9;    # Other controls
    ########################################
    $add_option->( 'dump-nested-ternaries',        'dnt',  '!' );
    $add_option->( 'warn-nested-ternaries',        'wnt',  '!' );
    $add_option->( 'nested-ternary-maximum-depth', 'ntmd', '=i' );
    $add_option->( 'warn-missing-else',            'wme',  '!' );
    $add_option->( 'add-missing-else',             'ame',  '!' );
    $add_option->( 'add-missing-else-comment',     'amec', '=s' );
    $add_option->( 'delete-block-comments',        'dbc',  '!' );
    $add_option->( 'delete-closing-side-comments', 'dcsc', '!' );
    $add_option->( 'delete-pod',                   'dp',   '!' );
    $add_option->( 'delete-side-comments',         'dsc',  '!' );
    $add_option->( 'tee-block-comments',           'tbc',  '!' );
    $add_option->( 'tee-pod',                      'tp',   '!' );
    $add_option->( 'tee-side-comments',            'tsc',  '!' );
    $add_option->( 'look-for-autoloader',          'lal',  '!' );
    $add_option->( 'look-for-hash-bang',           'x',    '!' );
    $add_option->( 'look-for-selfloader',          'lsl',  '!' );
    $add_option->( 'pass-version-line',            'pvl',  '!' );
    $add_option->( 'warn-variable-types',          'wvt',  '=s' );
    $add_option->( 'warn-variable-exclusion-list', 'wvxl', '=s' );
    $add_option->( 'want-call-parens',             'wcp',  '=s' );
    $add_option->( 'nowant-call-parens',           'nwcp', '=s' );

    $add_option->( 'warn-unique-keys',                      'wuk',   '!' );
    $add_option->( 'warn-unique-keys-cutoff',               'wukc',  '=i' );
    $add_option->( 'warn-mismatched-args',                  'wma',   '!' );
    $add_option->( 'warn-mismatched-arg-types',             'wmat',  '=s' );
    $add_option->( 'warn-mismatched-arg-undercount-cutoff', 'wmauc', '=i' );
    $add_option->( 'warn-mismatched-arg-overcount-cutoff',  'wmaoc', '=i' );
    $add_option->( 'warn-mismatched-arg-exclusion-list',    'wmaxl', '=s' );
    $add_option->( 'warn-mismatched-returns',               'wmr',   '!' );
    $add_option->( 'warn-mismatched-return-types',          'wmrt',  '=s' );
    $add_option->( 'warn-mismatched-return-exclusion-list', 'wmrxl', '=s' );
    $add_option->( 'warn-similar-keys',                     'wsk',   '!' );

    $add_option->( 'add-interbracket-arrows',       'aia', '!' );
    $add_option->( 'delete-interbracket-arrows',    'dia', '!' );
    $add_option->( 'warn-interbracket-arrows',      'wia', '!' );
    $add_option->( 'interbracket-arrow-style',      'ias', '=s' );
    $add_option->( 'interbracket-arrow-complexity', 'iac', '=i' );

    ########################################
    $category = 13;    # Debugging
    ########################################
    $add_option->( 'DEBUG',                           'D',     '!' );
    $add_option->( 'dump-block-summary',              'dbs',   '!' );
    $add_option->( 'dump-block-minimum-lines',        'dbl',   '=i' );
    $add_option->( 'dump-block-types',                'dbt',   '=s' );
    $add_option->( 'dump-cuddled-block-list',         'dcbl',  '!' );
    $add_option->( 'dump-defaults',                   'ddf',   '!' );
    $add_option->( 'dump-hash-keys',                  'dhk',   '!' );
    $add_option->( 'dump-integer-option-range',       'dior',  '!' );
    $add_option->( 'dump-long-names',                 'dln',   '!' );
    $add_option->( 'dump-mismatched-args',            'dma',   '!' );
    $add_option->( 'dump-mismatched-returns',         'dmr',   '!' );
    $add_option->( 'dump-mixed-call-parens',          'dmcp',  '!' );
    $add_option->( 'dump-options',                    'dop',   '!' );
    $add_option->( 'dump-profile',                    'dpro',  '!' );
    $add_option->( 'dump-short-names',                'dsn',   '!' );
    $add_option->( 'dump-similar-keys',               'dsk',   '!' );
    $add_option->( 'dump-token-types',                'dtt',   '!' );
    $add_option->( 'dump-unusual-variables',          'duv',   '!' );
    $add_option->( 'dump-unique-keys',                'duk',   '!' );
    $add_option->( 'dump-want-left-space',            'dwls',  '!' );
    $add_option->( 'dump-want-right-space',           'dwrs',  '!' );
    $add_option->( 'fuzzy-line-length',               'fll',   '!' );
    $add_option->( 'help',                            'h',     EMPTY_STRING );
    $add_option->( 'short-concatenation-item-length', 'scl',   '=i' );
    $add_option->( 'show-options',                    'opt',   '!' );
    $add_option->( 'timestamp',                       'ts',    '!' );
    $add_option->( 'version',                         'v',     EMPTY_STRING );
    $add_option->( 'memoize',                         'mem',   '!' );
    $add_option->( 'file-size-order',                 'fso',   '!' );
    $add_option->( 'maximum-file-size-mb',            'maxfs', '=i' );
    $add_option->( 'maximum-level-errors',            'maxle', '=i' );
    $add_option->( 'maximum-unexpected-errors',       'maxue', '=i' );
    $add_option->( 'integer-range-check',             'irc',   '=i' );

    $add_option->( 'similar-keys-maximum-difference', 'skmd', '=i' );
    $add_option->( 'similar-keys-minimum-length',     'skml', '=i' );
    $add_option->( 'similar-keys-maximum-pairs',      'skmp', '=i' );

    #---------------------------------------------------------------------

    # The Perl::Tidy::HtmlWriter will add its own options to the string
    Perl::Tidy::HtmlWriter->make_getopt_long_names( \@option_string );

    ########################################
    # Set categories 10, 11, 12
    ########################################
    # Based on their known order
    $category = 12;    # HTML properties
    foreach my $opt (@option_string) {
        my $long_name = $opt;
        $long_name =~ s/(!|=.*|:.*)$//;
        if ( !defined( $option_category{$long_name} ) ) {
            if ( $long_name =~ /^html-linked/ ) {
                $category = 10;    # HTML options
            }
            elsif ( $long_name =~ /^pod2html/ ) {
                $category = 11;    # Pod2html
            }
            else {
                $category = 12;    # HTML properties
            }
            $option_category{$long_name} = $category_name[$category];
        }
    }

    #----------------------------------------------------------------------
    # NON-INTEGER DEFAULTS: Assign default values to the above options here
    # except for integers, 'outfile' and 'help'
    # NOTES:
    # - Enter integer options in %integer_option_range, NOT HERE
    # - 'keyword-group-blanks-size=5' is ok here: the arg is a string
    # - These settings should approximate the perlstyle(1) suggestions.
    #----------------------------------------------------------------------
    my @defaults = qw(
      add-lone-trailing-commas
      add-newlines
      add-terminal-newline
      add-semicolons
      add-whitespace
      blanks-before-blocks
      blanks-before-comments

      keyword-group-blanks-size=5
      nokeyword-group-blanks-inside
      nokeyword-group-blanks-delete

      break-at-old-logical-breakpoints
      break-at-old-ternary-breakpoints
      break-at-old-attribute-breakpoints
      break-at-old-keyword-breakpoints
      nocheck-syntax
      character-encoding=guess
      closing-side-comments-balanced
      noextended-continuation-indentation
      delete-old-newlines
      delete-repeated-commas
      delete-lone-trailing-commas
      delete-semicolons
      dump-block-types=sub
      extended-syntax
      encode-output-strings
      file-size-order
      function-paren-vertical-alignment
      fuzzy-line-length
      hanging-side-comments
      indent-block-comments
      indent-leading-semicolon
      logical-padding
      look-for-autoloader
      look-for-selfloader
      memoize
      nobrace-left-and-indent
      nocuddled-else
      nodelete-old-whitespace
      nohtml
      nologfile
      non-indenting-braces
      noquiet
      noshow-options
      nostatic-side-comments
      notabs
      nowarning-output
      outdent-labels
      outdent-long-quotes
      outdent-long-comments
      pass-version-line
      noweld-nested-containers
      recombine
      nouse-unicode-gcstring
      valign-code
      valign-block-comments
      valign-side-comments
      valign-signed-numbers
      space-for-semicolon
      static-block-comments
      timestamp
      trim-qw
      format=tidy
      backup-method=copy
      backup-file-extension=bak
      code-skipping
      format-skipping
      detect-format-skipping-from-start

      pod2html
      html-table-of-contents
      html-entities
    );

    #------------------------------------------------------------
    # Set Ranges and defaults of all integer options (type '=i').
    #------------------------------------------------------------
    # NOTES:
    # 1. All integer options must be in this table, not in @defaults
    # 2. 'closing-token-indentation' (cti), 'vertical-tightness' (vt),
    #   and 'vertical-tightness-closing' (vtc) are aliases which are included
    #   to work around an old problem with msdos (see note in check_options).
    # 3. Use -dior to dump this table.

    # 'option-name' => [min, max, default]
    %integer_option_range = (
        'blank-lines-after-opening-block'           => [ 0, undef, 0 ],
        'blank-lines-before-closing-block'          => [ 0, undef, 0 ],
        'blank-lines-before-packages'               => [ 0, undef, 1 ],
        'blank-lines-before-subs'                   => [ 0, undef, 1 ],
        'block-brace-tightness'                     => [ 0, 2,     0 ],
        'block-brace-vertical-tightness'            => [ 0, 2,     0 ],
        'brace-follower-vertical-tightness'         => [ 0, 2,     1 ],
        'brace-tightness'                           => [ 0, 2,     1 ],
        'brace-vertical-tightness'                  => [ 0, 2,     0 ],
        'brace-vertical-tightness-closing'          => [ 0, 3,     0 ],
        'break-after-labels'                        => [ 0, 2,     0 ],
        'break-before-hash-brace'                   => [ 0, 3,     0 ],
        'break-before-hash-brace-and-indent'        => [ 0, 2,     0 ],
        'break-before-paren'                        => [ 0, 3,     0 ],
        'break-before-paren-and-indent'             => [ 0, 2,     0 ],
        'break-before-square-bracket'               => [ 0, 3,     0 ],
        'break-before-square-bracket-and-indent'    => [ 0, 2,     0 ],
        'closing-brace-indentation'                 => [ 0, 3,     0 ],
        'closing-paren-indentation'                 => [ 0, 3,     0 ],
        'closing-side-comment-else-flag'            => [ 0, 2,     0 ],
        'closing-side-comment-interval'             => [ 0, undef, 6 ],
        'closing-side-comment-maximum-text'         => [ 0, undef, 20 ],
        'closing-square-bracket-indentation'        => [ 0, 3,     0 ],
        'closing-token-indentation'                 => [ 0, 3,     undef ],
        'comma-arrow-breakpoints'                   => [ 0, 5,     5 ],
        'continuation-indentation'                  => [ 0, undef, 2 ],
        'cuddled-break-option'                      => [ 0, 2,     1 ],
        'default-tabsize'                           => [ 0, undef, 8 ],
        'dump-block-minimum-lines'                  => [ 0, undef, 20 ],
        'entab-leading-whitespace'                  => [ 0, undef, 0 ],
        'fixed-position-side-comment'               => [ 0, undef, undef ],
        'indent-columns'                            => [ 0, undef, 4 ],
        'integer-range-check'                       => [ 1, 3,     2 ],
        'interbracket-arrow-complexity'             => [ 0, 2,     1 ],
        'iterations'                                => [ 0, undef, 1 ],
        'keep-old-blank-lines'                      => [ 0, 2,     1 ],
        'keyword-group-blanks-after'                => [ 0, 2,     1 ],
        'keyword-group-blanks-before'               => [ 0, 2,     1 ],
        'keyword-group-blanks-repeat-count'         => [ 0, undef, 0 ],
        'keyword-paren-inner-tightness'             => [ 0, 2,     1 ],
        'long-block-line-count'                     => [ 0, undef, 8 ],
        'maximum-consecutive-blank-lines'           => [ 0, undef, 1 ],
        'maximum-file-size-mb'                      => [ 0, undef, 10 ],
        'maximum-level-errors'                      => [ 0, undef, 1 ],
        'maximum-line-length'                       => [ 0, undef, 80 ],
        'maximum-unexpected-errors'                 => [ 0, undef, 0 ],
        'minimum-space-to-comment'                  => [ 0, undef, 4 ],
        'one-line-block-nesting'                    => [ 0, 1,     0 ],
        'one-line-block-semicolons'                 => [ 0, 2,     1 ],
        'paren-tightness'                           => [ 0, 2,     1 ],
        'paren-vertical-tightness'                  => [ 0, 2,     0 ],
        'paren-vertical-tightness-closing'          => [ 0, 3,     0 ],
        'short-concatenation-item-length'           => [ 0, undef, 8 ],
        'similar-keys-maximum-difference'           => [ 1, undef, 1 ],
        'similar-keys-maximum-pairs'                => [ 1, undef, 25 ],
        'similar-keys-minimum-length'               => [ 1, undef, 4 ],
        'space-backslash-quote'                     => [ 0, 2,     1 ],
        'space-prototype-paren'                     => [ 0, 2,     1 ],
        'space-signature-paren'                     => [ 0, 2,     1 ],
        'square-bracket-tightness'                  => [ 0, 2,     1 ],
        'square-bracket-vertical-tightness'         => [ 0, 2,     0 ],
        'square-bracket-vertical-tightness-closing' => [ 0, 3,     0 ],
        'starting-indentation-level'                => [ 0, undef, undef ],
        'timeout-in-seconds'                        => [ 0, undef, 10 ],
        'valign-signed-numbers-limit'               => [ 0, undef, 20 ],
        'vertical-tightness'                        => [ 0, 2,     undef ],
        'vertical-tightness-closing'                => [ 0, 3,     undef ],
        'warn-mismatched-arg-overcount-cutoff'      => [ 0, undef, 1 ],
        'warn-mismatched-arg-undercount-cutoff'     => [ 0, undef, 4 ],
        'nested-ternary-maximum-depth'              => [ 0, undef, 0 ],
        'warn-unique-keys-cutoff'                   => [ 1, undef, 1 ],
        'whitespace-cycle'                          => [ 0, undef, 0 ],
    );

    foreach my $key ( keys %integer_option_range ) {
        my $val = $integer_option_range{$key}->[2];
        if ( defined($val) ) {
            push @defaults, "$key=$val";
        }
    }

    #------------------------------------
    # Locate strings options of type '=s'
    #------------------------------------
    my %is_string_option;
    foreach my $opt (@option_string) {
        next if ( substr( $opt, -2, 2 ) ne '=s' );
        my $key = substr( $opt, 0, -2 );
        $is_string_option{$key} = 1;
    }

    # Verify that only integers of type =i are in the above list during
    # development. This will guard against spelling errors.
    if (DEVEL_MODE) {
        my %option_flag;
        my $msg = EMPTY_STRING;
        foreach my $opt (@option_string) {
            my $key  = $opt;
            my $flag = EMPTY_STRING;
            if ( $key =~ /(.*)(!|=.*|:.*)$/ ) {
                $key  = $1;
                $flag = $2;
            }
            $option_flag{$key} = $flag;
        }

        # Be sure all keys of %integer_option_range have option type '=i'
        foreach my $opt ( keys %integer_option_range ) {
            my $flag = $option_flag{$opt};
            if ( !defined($flag) ) { $flag = EMPTY_STRING }
            if ( $flag ne '=i' ) {

                # If this fault occurs, one of the items in the previous hash
                # is not type =i, possibly due to incorrect spelling.
                $msg .=
"Option '$opt' has an entry in '%integer_option_range' but is not an integer\n";
            }
        }

        # Be sure all '=i' options are in %integer_option_range. This is not
        # strictly necessary but helps insure that nothing was missed.
        foreach my $opt ( keys %option_flag ) {
            my $flag = $option_flag{$opt};
            next if ( $flag ne '=i' );
            if ( !defined( $integer_option_range{$opt} ) ) {
                $msg .=
"Integer option '$opt' is needs an entry in '%integer_option_range'\n";
            }
        }

        # look for integer options without default values
        foreach my $opt ( keys %integer_option_range ) {
            if ( @{ $integer_option_range{$opt} } < 3 ) {
                $msg .= "Integer option '$opt' does not have a default value\n";
            }
        }

        if ($msg) {
            Fault($msg);
        }
    }

    #-----------------------------------------------------------------------
    # Define abbreviations which will be expanded into the above primitives.
    # These may be defined recursively.
    #-----------------------------------------------------------------------
    %expansion = (
        %expansion,
        'freeze-newlines'    => [qw(noadd-newlines nodelete-old-newlines)],
        'fnl'                => [qw(freeze-newlines)],
        'freeze-whitespace'  => [qw(noadd-whitespace nodelete-old-whitespace)],
        'fws'                => [qw(freeze-whitespace)],
        'freeze-blank-lines' =>
          [qw(maximum-consecutive-blank-lines=0 keep-old-blank-lines=2)],
        'fbl'                => [qw(freeze-blank-lines)],
        'indent-only'        => [qw(freeze-newlines freeze-whitespace)],
        'outdent-long-lines' => [qw(outdent-long-quotes outdent-long-comments)],
        'nooutdent-long-lines' =>
          [qw(nooutdent-long-quotes nooutdent-long-comments)],
        'oll'                 => [qw(outdent-long-lines)],
        'noll'                => [qw(nooutdent-long-lines)],
        'io'                  => [qw(indent-only)],
        'delete-all-comments' =>
          [qw(delete-block-comments delete-side-comments delete-pod)],
        'nodelete-all-comments' =>
          [qw(nodelete-block-comments nodelete-side-comments nodelete-pod)],
        'dac'              => [qw(delete-all-comments)],
        'ndac'             => [qw(nodelete-all-comments)],
        'gnu'              => [qw(gnu-style)],
        'pbp'              => [qw(perl-best-practices)],
        'tee-all-comments' =>
          [qw(tee-block-comments tee-side-comments tee-pod)],
        'notee-all-comments' =>
          [qw(notee-block-comments notee-side-comments notee-pod)],
        'tac'   => [qw(tee-all-comments)],
        'ntac'  => [qw(notee-all-comments)],
        'html'  => [qw(format=html)],
        'nhtml' => [qw(format=tidy)],
        'tidy'  => [qw(format=tidy)],

        'brace-left' => [qw(opening-brace-on-new-line)],

        # -cb is now a synonym for -ce
        'cb'             => [qw(cuddled-else)],
        'cuddled-blocks' => [qw(cuddled-else)],

        'utf8'  => [qw(character-encoding=utf8)],
        'UTF8'  => [qw(character-encoding=utf8)],
        'guess' => [qw(character-encoding=guess)],

        'swallow-optional-blank-lines'   => [qw(kbl=0)],
        'noswallow-optional-blank-lines' => [qw(kbl=1)],
        'sob'                            => [qw(kbl=0)],
        'nsob'                           => [qw(kbl=1)],

        'break-after-comma-arrows'   => [qw(cab=0)],
        'nobreak-after-comma-arrows' => [qw(cab=1)],
        'baa'                        => [qw(cab=0)],
        'nbaa'                       => [qw(cab=1)],

        'blanks-before-subs'   => [qw(blbs=1 blbp=1)],
        'bbs'                  => [qw(blbs=1 blbp=1)],
        'noblanks-before-subs' => [qw(blbs=0 blbp=0)],
        'nbbs'                 => [qw(blbs=0 blbp=0)],

        'keyword-group-blanks'   => [qw(kgbb=2 kgbi kgba=2)],
        'kgb'                    => [qw(kgbb=2 kgbi kgba=2)],
        'nokeyword-group-blanks' => [qw(kgbb=1 nkgbi kgba=1)],
        'nkgb'                   => [qw(kgbb=1 nkgbi kgba=1)],

        # allow spelling error 'trinary' vs 'ternary'
        'break-at-old-trinary-breakpoints' => [qw(bot)],

        'cti=0' => [qw(cpi=0 cbi=0 csbi=0)],
        'cti=1' => [qw(cpi=1 cbi=1 csbi=1)],
        'cti=2' => [qw(cpi=2 cbi=2 csbi=2)],
        'icp'   => [qw(cpi=2 cbi=2 csbi=2)],
        'nicp'  => [qw(cpi=0 cbi=0 csbi=0)],

        'closing-token-indentation=0' => [qw(cpi=0 cbi=0 csbi=0)],
        'closing-token-indentation=1' => [qw(cpi=1 cbi=1 csbi=1)],
        'closing-token-indentation=2' => [qw(cpi=2 cbi=2 csbi=2)],
        'indent-closing-paren'        => [qw(cpi=2 cbi=2 csbi=2)],
        'noindent-closing-paren'      => [qw(cpi=0 cbi=0 csbi=0)],

        'vt=0' => [qw(pvt=0 bvt=0 sbvt=0)],
        'vt=1' => [qw(pvt=1 bvt=1 sbvt=1)],
        'vt=2' => [qw(pvt=2 bvt=2 sbvt=2)],

        'vertical-tightness=0' => [qw(pvt=0 bvt=0 sbvt=0)],
        'vertical-tightness=1' => [qw(pvt=1 bvt=1 sbvt=1)],
        'vertical-tightness=2' => [qw(pvt=2 bvt=2 sbvt=2)],

        'vtc=0' => [qw(pvtc=0 bvtc=0 sbvtc=0)],
        'vtc=1' => [qw(pvtc=1 bvtc=1 sbvtc=1)],
        'vtc=2' => [qw(pvtc=2 bvtc=2 sbvtc=2)],
        'vtc=3' => [qw(pvtc=3 bvtc=3 sbvtc=3)],

        'vertical-tightness-closing=0' => [qw(pvtc=0 bvtc=0 sbvtc=0)],
        'vertical-tightness-closing=1' => [qw(pvtc=1 bvtc=1 sbvtc=1)],
        'vertical-tightness-closing=2' => [qw(pvtc=2 bvtc=2 sbvtc=2)],
        'vertical-tightness-closing=3' => [qw(pvtc=3 bvtc=3 sbvtc=3)],

        'otr'                   => [qw(opr ohbr osbr)],
        'opening-token-right'   => [qw(opr ohbr osbr)],
        'notr'                  => [qw(nopr nohbr nosbr)],
        'noopening-token-right' => [qw(nopr nohbr nosbr)],

        'sot'                    => [qw(sop sohb sosb)],
        'nsot'                   => [qw(nsop nsohb nsosb)],
        'stack-opening-tokens'   => [qw(sop sohb sosb)],
        'nostack-opening-tokens' => [qw(nsop nsohb nsosb)],

        'sct'                    => [qw(scp schb scsb)],
        'stack-closing-tokens'   => [qw(scp schb scsb)],
        'nsct'                   => [qw(nscp nschb nscsb)],
        'nostack-closing-tokens' => [qw(nscp nschb nscsb)],

        'sac'                    => [qw(sot sct)],
        'nsac'                   => [qw(nsot nsct)],
        'stack-all-containers'   => [qw(sot sct)],
        'nostack-all-containers' => [qw(nsot nsct)],

        'act=0'                      => [qw(pt=0 sbt=0 bt=0 bbt=0)],
        'act=1'                      => [qw(pt=1 sbt=1 bt=1 bbt=1)],
        'act=2'                      => [qw(pt=2 sbt=2 bt=2 bbt=2)],
        'all-containers-tightness=0' => [qw(pt=0 sbt=0 bt=0 bbt=0)],
        'all-containers-tightness=1' => [qw(pt=1 sbt=1 bt=1 bbt=1)],
        'all-containers-tightness=2' => [qw(pt=2 sbt=2 bt=2 bbt=2)],

        'stack-opening-block-brace'   => [qw(bbvt=2 bbvtl=*)],
        'sobb'                        => [qw(bbvt=2 bbvtl=*)],
        'nostack-opening-block-brace' => [qw(bbvt=0)],
        'nsobb'                       => [qw(bbvt=0)],

        'converge'   => [qw(it=4)],
        'noconverge' => [qw(it=1)],
        'conv'       => [qw(it=4)],
        'nconv'      => [qw(it=1)],

        'valign'   => [qw(vc vsc vbc)],
        'novalign' => [qw(nvc nvsc nvbc)],

        # NOTE: This is a possible future shortcut.  But it will remain
        # deactivated until the -lpxl flag is no longer experimental.
        # 'line-up-function-parentheses' => [ qw(lp), q#lpxl=[ { F(2# ],
        # 'lfp'                          => [qw(line-up-function-parentheses)],

        # 'mangle' originally deleted pod and comments, but to keep it
        # reversible, it no longer does.  But if you really want to
        # delete them, just use:
        #   -mangle -dac

        # An interesting use for 'mangle' is to do this:
        #    perltidy -mangle myfile.pl -st | perltidy -o myfile.pl.new
        # which will form as many one-line blocks as possible

        'mangle' => [
            qw(
              keep-old-blank-lines=0
              delete-old-newlines
              delete-old-whitespace
              delete-semicolons
              indent-columns=0
              maximum-consecutive-blank-lines=0
              maximum-line-length=100000
              noadd-newlines
              noadd-semicolons
              noadd-whitespace
              noblanks-before-blocks
              blank-lines-before-subs=0
              blank-lines-before-packages=0
              notabs
            )
        ],

        # 'extrude' originally deleted pod and comments, but to keep it
        # reversible, it no longer does.  But if you really want to
        # delete them, just use
        #   extrude -dac
        #
        # An interesting use for 'extrude' is to do this:
        #    perltidy -extrude myfile.pl -st | perltidy -o myfile.pl.new
        # which will break up all one-line blocks.
        'extrude' => [
            qw(
              ci=0
              delete-old-newlines
              delete-old-whitespace
              delete-semicolons
              indent-columns=0
              maximum-consecutive-blank-lines=0
              maximum-line-length=1
              noadd-semicolons
              noadd-whitespace
              noblanks-before-blocks
              blank-lines-before-subs=0
              blank-lines-before-packages=0
              nofuzzy-line-length
              notabs
              norecombine
            )
        ],

        # this style tries to follow the GNU Coding Standards (which do
        # not really apply to perl but which are followed by some perl
        # programmers).
        'gnu-style' => [
            qw(
              lp bl noll pt=2 bt=2 sbt=2 cpi=1 csbi=1 cbi=1
            )
        ],

        # Style suggested in Damian Conway's Perl Best Practices
        'perl-best-practices' => [
            qw(l=78 i=4 ci=4 st se vt=2 cti=0 pt=1 bt=1 sbt=1 bbt=1 nsfs nolq),
q(wbb=% + - * / x != == >= <= =~ !~ < > | & = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=)
        ],

        # Additional styles can be added here
    );

    Perl::Tidy::HtmlWriter->make_abbreviated_names( \%expansion );

    # Uncomment next line to dump all expansions for debugging:
    # dump_short_names(\%expansion);
    return ( \@option_string, \@defaults, \%expansion, \%option_category,
        \%integer_option_range, \%is_string_option );

} ## end sub generate_options

{ #<<< closure process_command_line

# Memoize process_command_line. Given same @ARGV passed in, return same
# values and same @ARGV back.
# This patch was supplied by Jonathan Swartz Nov 2012 and significantly speeds
# up masontidy (https://metacpan.org/module/masontidy)

my %process_command_line_cache;

sub process_command_line {

    my @q = @_;
    my (
        $perltidyrc_stream,     $is_Windows_uu, $Windows_type_uu,
        $rpending_complaint_uu, $dump_options_type
    ) = @q;

    # This is the outer sub which handles memoization

    my $use_cache = !defined($perltidyrc_stream) && !$dump_options_type;
    if ($use_cache) {
        my $cache_key = join( chr(28), @ARGV );
        if ( my $result = $process_command_line_cache{$cache_key} ) {
            my ( $argv, @retvals ) = @{$result};
            @ARGV = @{$argv};
            return @retvals;
        }
        else {
            my @retvals = _process_command_line(@q);
            $process_command_line_cache{$cache_key} = [ \@ARGV, @retvals ]
              if ( $retvals[0]->{'memoize'} );
            return @retvals;
        }
    }
    else {
        return _process_command_line(@q);
    }
} ## end sub process_command_line
} ## end closure process_command_line

# (note the underscore here)
sub _process_command_line {

    my (
        $perltidyrc_stream,  $is_Windows, $Windows_type,
        $rpending_complaint, $dump_options_type
    ) = @_;

    # This is the inner sub which actually processes the command line

    use Getopt::Long;

    # Save any current Getopt::Long configuration
    # and set to Getopt::Long defaults.  Use eval to avoid
    # breaking old versions of Perl without these routines.
    # Previous configuration is reset at the exit of this routine.
    my $glc;
    if ( eval { $glc = Getopt::Long::Configure(); 1 } ) {
        my $ok = eval { Getopt::Long::ConfigDefaults(); 1 };
        if ( !$ok && DEVEL_MODE ) {
            Fault("Failed call to Getopt::Long::ConfigDefaults: $EVAL_ERROR\n");
        }
    }
    else { $glc = undef }

    my ( $roption_string, $rdefaults, $rexpansion,
        $roption_category, $rinteger_option_range, $ris_string_option )
      = generate_options();

    #--------------------------------------------------------------
    # set the defaults by passing the above list through GetOptions
    #--------------------------------------------------------------
    my %Opts = ();
    {
        local @ARGV = ();

        # do not load the defaults if we are just dumping perltidyrc
        if ( $dump_options_type ne 'perltidyrc' ) {
            for my $i ( @{$rdefaults} ) { push @ARGV, "--" . $i }
        }
        if ( !GetOptions( \%Opts, @{$roption_string} ) ) {
            Die(
"Programming Bug reported by 'GetOptions': error in setting default options"
            );
        }
    }

    my @raw_options        = ();
    my $saw_ignore_profile = 0;
    my $saw_dump_profile   = 0;
    my $config_file;

    #--------------------------------------------------------------
    # Take a first look at the command-line parameters.  Do as many
    # immediate dumps as possible, which can avoid confusion if the
    # perltidyrc file has an error.
    #--------------------------------------------------------------
    foreach my $i (@ARGV) {

        $i =~ s/^--/-/;
        if ( $i =~ /^-(npro|noprofile|nopro|no-profile)$/ ) {
            $saw_ignore_profile = 1;
        }

        # note: this must come before -pro and -profile, below:
        elsif ( $i =~ /^-(dump-profile|dpro)$/ ) {
            $saw_dump_profile = 1;
        }
        elsif ( $i =~ /^-(pro|profile)=(.+)/ ) {
            if ( defined($config_file) ) {
                Warn(
"Only one -pro=filename allowed, using '$2' instead of '$config_file'\n"
                );
            }
            $config_file = $2;

            # resolve <dir>/.../<file>, meaning look upwards from directory
            if ( defined($config_file) ) {
                if ( my ( $start_dir, $search_file ) =
                    ( $config_file =~ m{^(.*)\.\.\./(.*)$} ) )
                {
                    $start_dir = '.' if ( !$start_dir );
                    $start_dir = Cwd::realpath($start_dir);
                    my $found_file =
                      find_file_upwards( $start_dir, $search_file );
                    if ( defined($found_file) ) {
                        $config_file = $found_file;
                    }
                }
            }
            if ( !-e $config_file ) {
                Die(
                    "cannot find file given with -pro=$config_file: $OS_ERROR\n"
                );
            }
        }
        elsif ( $i =~ /^-(pro|profile)=?$/ ) {
            Die("usage: -pro=filename or --profile=filename, no spaces\n");
        }
        elsif ( $i =~ /^-(?: help | [ h \? ] )$/xi ) {
            usage();
            Exit(0);
        }
        elsif ( $i =~ /^-(version|v)$/ ) {
            show_version();
            Exit(0);
        }
        elsif ( $i =~ /^-(dump-defaults|ddf)$/ ) {
            dump_defaults( @{$rdefaults} );
            Exit(0);
        }
        elsif ( $i =~ /^-(dump-integer-option-range|dior)$/ ) {
            dump_integer_option_range($rinteger_option_range);
            Exit(0);
        }
        elsif ( $i =~ /^-(dump-long-names|dln)$/ ) {
            dump_long_names( @{$roption_string} );
            Exit(0);
        }
        elsif ( $i =~ /^-(dump-short-names|dsn)$/ ) {
            dump_short_names($rexpansion);
            Exit(0);
        }
        elsif ( $i =~ /^-(dump-token-types|dtt)$/ ) {
            Perl::Tidy::Tokenizer->dump_token_types(*STDOUT);
            Exit(0);
        }
        else {
            ## no more special cases
        }
    }

    # The above commands processed before disambiguation and then Exited.  So
    # we need to check below to see if the user entered something like
    # '-dump-t' or '-he'. This will slip past here and not get processed.
    my %early_exit_commands = (
        'help'                      => 'h',
        'version'                   => 'v',
        'dump-defaults'             => 'ddf',
        'dump-integer-option-range' => 'dior',
        'dump-long-names'           => 'dln',
        'dump-short-names'          => 'dsn',
        'dump-token-types'          => 'dtt',
    );

    if ( $saw_dump_profile && $saw_ignore_profile ) {
        Warn("No profile to dump because of -npro\n");
        Exit(1);
    }

    #----------------------------------------
    # read any .perltidyrc configuration file
    #----------------------------------------
    if ( !$saw_ignore_profile ) {

        # resolve possible conflict between $perltidyrc_stream passed
        # as call parameter to perltidy and -pro=filename on command
        # line.
        if ($perltidyrc_stream) {
            if ( defined($config_file) ) {
                Warn(<<EOM);
 Conflict: a perltidyrc configuration file was specified both as this
 perltidy call parameter: $perltidyrc_stream
 and with this -profile=$config_file.
 Using -profile=$config_file.
EOM
            }
            else {
                $config_file = $perltidyrc_stream;
            }
        }

        # look for a config file if we don't have one yet
        my $rconfig_file_chatter;
        ${$rconfig_file_chatter} = EMPTY_STRING;
        if ( !defined($config_file) ) {
            $config_file =
              find_config_file( $is_Windows, $Windows_type,
                $rconfig_file_chatter, $rpending_complaint );
        }

        # open any config file
        my $rconfig_string;
        if ( defined($config_file) ) {
            $rconfig_string = stream_slurp($config_file);
            if ( !defined($rconfig_string) ) {
                Die(
"exiting because profile '$config_file' could not be opened\n"
                );
            }
            filter_unknown_options(
                $rconfig_string, $roption_category,
                $rexpansion,     $rconfig_file_chatter
            );
        }
        if ($saw_dump_profile) {
            dump_config_file( $rconfig_string, $config_file,
                $rconfig_file_chatter );
            Exit(0);
        }

        if ( defined($rconfig_string) ) {

            my ( $rconfig_list, $death_message ) =
              read_config_file( $rconfig_string, $config_file, $rexpansion );
            Die($death_message) if ($death_message);

            # process any .perltidyrc parameters right now so we can
            # localize errors
            if ( @{$rconfig_list} ) {
                local @ARGV = @{$rconfig_list};

                expand_command_abbreviations( $rexpansion, \@raw_options,
                    $config_file );

                check_for_missing_string_options( $ris_string_option,
                    $config_file );

                if ( !GetOptions( \%Opts, @{$roption_string} ) ) {
                    Die(
"Error in this config file: $config_file  \nUse -npro to ignore this file, -dpro to dump it, -h for help'\n"
                    );
                }

                # Anything left in this local @ARGV is an error and must be
                # invalid bare words from the configuration file.  We cannot
                # check this earlier because bare words may have been valid
                # values for parameters.  We had to wait for GetOptions to have
                # a look at @ARGV.
                if (@ARGV) {
                    my $count = @ARGV;
                    my $str   = EMPTY_STRING;
                    foreach my $param (@ARGV) {
                        if ( length($str) < 70 ) {
                            if ($str) { $str .= ', ' }
                            $str .= "'$param'";
                        }
                        else {
                            $str .= ", ...";
                            last;
                        }
                    }
                    Die(<<EOM);
There are $count unrecognized values in the configuration file '$config_file':
$str
Use leading dashes for parameters.  Use -npro to ignore this file.
EOM
                }

                # Undo any options which cause premature exit.  They are not
                # appropriate for a config file, and it could be hard to
                # diagnose the cause of the premature exit.

                # These are options include dump switches of the form
                # '--dump-xxx-xxx!'.
                my @dump_commands =
                  grep { /^(dump-.*)!$/ } @{$roption_string};
                foreach (@dump_commands) { s/!$// }

                # Here is a current list of these @dump_commands:
                #  dump-block-summary
                #  dump-cuddled-block-list
                #  dump-defaults
                #  dump-integer-option-range
                #  dump-long-names
                #  dump-mismatched-args
                #  dump-mismatched-returns
                #  dump-mixed-call-parens
                #  dump-options
                #  dump-profile
                #  dump-short-names
                #  dump-token-types
                #  dump-unusual-variables
                #  dump-want-left-space
                #  dump-want-right-space

                # The following two dump configuration parameters which
                # take =i or =s would still be allowed:
                #  dump-block-minimum-lines',        'dbl',   '=i' );
                #  dump-block-types',                'dbt',   '=s' );

                foreach my $cmd (
                    @dump_commands,
                    qw{
                    help
                    stylesheet
                    version
                    }
                  )
                {
                    if ( defined( $Opts{$cmd} ) ) {
                        delete $Opts{$cmd};
                        Warn("ignoring --$cmd in config file: $config_file\n");
                    }
                }
            }
        }
    }

    #----------------------------------------
    # now process the command line parameters
    #----------------------------------------
    expand_command_abbreviations( $rexpansion, \@raw_options, $config_file );

    check_for_missing_string_options($ris_string_option);

    local $SIG{'__WARN__'} = sub { Warn( $_[0] ) };
    if ( !GetOptions( \%Opts, @{$roption_string} ) ) {
        Die("Error on command line; for help try 'perltidy -h'\n");
    }

    # Catch ambiguous entries which should have exited above (c333)
    foreach my $long_name ( keys %early_exit_commands ) {
        if ( $Opts{$long_name} ) {
            my $short_name = $early_exit_commands{$long_name};
            Die(<<EOM);
Ambiguous entry; please enter '--$long_name' or '-$short_name'
EOM
        }
    }

    # reset Getopt::Long configuration back to its previous value
    if ( defined($glc) ) {
        my $ok = eval { Getopt::Long::Configure($glc); 1 };
        if ( !$ok && DEVEL_MODE ) {
            Fault("Could not reset Getopt::Long configuration: $EVAL_ERROR\n");
        }
    }

    return ( \%Opts, $config_file, \@raw_options, $roption_string,
        $rexpansion, $roption_category, $rinteger_option_range );
} ## end sub _process_command_line

sub make_grep_alias_string {

    my ($rOpts) = @_;

    # pre-process the --grep-alias-list parameter

    # Defaults: list operators in List::Util
    # Possible future additions:  pairfirst pairgrep pairmap
    my $default_string = join SPACE,
      qw( all any first none notall reduce reductions );

    # make a hash of any excluded words
    my %is_excluded_word;
    my $exclude_string = $rOpts->{'grep-alias-exclusion-list'};
    if ($exclude_string) {
        $exclude_string =~ s/,/ /g;    # allow commas
        $exclude_string =~ s/^\s+//;
        $exclude_string =~ s/\s+$//;
        my @q = split /\s+/, $exclude_string;
        @is_excluded_word{@q} = (1) x scalar(@q);
        foreach my $word (@q) {
            next if ( $word eq '*' );
            if ( $word =~ /^\d/ || $word =~ /[^\w]/ ) {
                my $opt_name = 'grep-alias-exclusion-list';
                Die("unexpected word in --$opt_name: '$word'\n");
            }
        }
    }

    # The special option -gaxl='*' removes all defaults
    if ( $is_excluded_word{'*'} ) { $default_string = EMPTY_STRING }

    # combine the defaults and any input list
    my $input_string = $rOpts->{'grep-alias-list'};
    if ($input_string) { $input_string .= SPACE . $default_string }
    else               { $input_string = $default_string }

    # Now make the final list of unique grep alias words
    $input_string =~ s/,/ /g;    # allow commas
    $input_string =~ s/^\s+//;
    $input_string =~ s/\s+$//;
    my @word_list = split /\s+/, $input_string;
    my @filtered_word_list;
    my %seen;

    foreach my $word (@word_list) {
        if ($word) {
            if ( $word =~ /^\d/ || $word =~ /[^\w]/ ) {
                my $opt_name = 'grep-alias-list';
                Die("unexpected word in --$opt_name: '$word'\n");
            }
            if ( !$seen{$word} && !$is_excluded_word{$word} ) {
                $seen{$word}++;
                push @filtered_word_list, $word;
            }
        }
    }
    my $joined_words = join SPACE, @filtered_word_list;
    $rOpts->{'grep-alias-list'} = $joined_words;
    return;
} ## end sub make_grep_alias_string

sub cleanup_word_list {

    my ( $rOpts, $option_name, $rforced_words ) = @_;

    # Clean up the list of words in a user option to simplify use by
    # later routines (delete repeats, replace commas with single space,
    # remove non-words)

    # Given:
    #   $rOpts - the global option hash
    #   $option_name - hash key of this option
    #   $rforced_words - ref to list of any words to be added

    # Returns:
    #   \%seen - hash of the final list of words

    my %seen;
    my @input_list;

    my $input_string = $rOpts->{$option_name};
    if ( defined($input_string) && length($input_string) ) {
        $input_string =~ s/,/ /g;    # allow commas
        $input_string =~ s/^\s+//;
        $input_string =~ s/\s+$//;
        @input_list = split /\s+/, $input_string;
    }

    if ($rforced_words) {
        push @input_list, @{$rforced_words};
    }

    my @filtered_word_list;
    my @bad_words;
    foreach my $word (@input_list) {
        if ($word) {
            if ( !$seen{$word} ) {
                $seen{$word}++;

                # look for obviously bad words
                if ( $word =~ /^\d/ || $word !~ /^\w+$/ ) {
                    push @bad_words, $word;
                }
                else {
                    push @filtered_word_list, $word;
                }
            }
        }
    }
    if (@bad_words) {
        my $num = @bad_words;
        local $LIST_SEPARATOR = SPACE;
        my $msg = <<EOM;
$num unrecognized words were input with $option_name :
 @bad_words
EOM
        Die($msg);
    }
    $rOpts->{$option_name} = join SPACE, @filtered_word_list;
    return \%seen;
} ## end sub cleanup_word_list

sub check_options {

    my ( $self, $num_files, $rinteger_option_range ) = @_;

    # Check options at a high level. Note that other modules have their
    # own sub 'check_options' for lower level checking.

    # Input parameters:
    #  $num_files = the number of files to be processed in this call to
    #     perltidy, needed for error checks.
    #  $rinteger_option-range = hash with valid ranges of parameters which
    #     take an integer

    my $rOpts = $self->[_rOpts_];

    #------------------------------------------------------------
    # check and handle any interactions among the basic options..
    #------------------------------------------------------------

    # Since perltidy only encodes in utf8, problems can occur if we let it
    # decode anything else.  See discussions for issue git #83.
    my $encoding = $rOpts->{'character-encoding'};
    if ( $encoding !~ /^\s*(?:guess|none|utf8|utf-8)\s*$/i ) {
        Die(<<EOM);
--character-encoding = '$encoding' is not allowed; the options are: 'none', 'guess', 'utf8'
EOM
    }

    # Check for integer values out of bounds as follows:
    #  $integer_range_check=
    #    1 => quietly reset bad values to defaults
    #    2 => issue warning and reset bad values to defaults [DEFAULT]
    #    3 => stop if any values are out of bounds
    # Note: Previously a value of 0 meant to skip this check. This provided a
    # workaround in case this logic caused a problem. This is no longer needed.
    my $integer_range_check = $rOpts->{'integer-range-check'};
    if (   !defined($integer_range_check)
        || $integer_range_check <= 0
        || $integer_range_check > 3 )
    {
        $integer_range_check = 2;
    }

    if ($integer_range_check) {
        my $Error_message;
        foreach my $opt ( keys %{$rinteger_option_range} ) {
            my $val = $rOpts->{$opt};
            next unless ( defined($val) );
            my $range = $rinteger_option_range->{$opt};
            next unless ( defined($range) );
            my ( $min, $max, $default ) = @{$range};

            if ( defined($min) && $val < $min ) {
                $Error_message .= "--$opt=$val but should be >= $min";
                if ( $integer_range_check < 3 ) {
                    $rOpts->{$opt} = $default;
                    my $def = defined($default) ? $default : 'undef';
                    $Error_message .= "; using default $def";
                }
                $Error_message .= "\n";
            }
            if ( defined($max) && $val > $max ) {
                $Error_message .= "--$opt=$val but should be <= $max";
                if ( $integer_range_check < 3 ) {
                    $rOpts->{$opt} = $default;
                    my $def = defined($default) ? $default : 'undef';
                    $Error_message .= "; using default $def";
                }
                $Error_message .= "\n";
            }
        }
        if ($Error_message) {
            if ( $integer_range_check == 1 ) {
                ## no warning
            }
            elsif ( $integer_range_check == 2 ) {
                Warn($Error_message);
            }
            else {
                Die($Error_message);
            }
        }
    }

    # Note that -vt, -vtc, and -cti are abbreviations. But under
    # msdos, an unquoted input parameter like vtc=1 will be
    # seen as 2 parameters, vtc and 1, so the abbreviations
    # won't be seen.  Therefore, we will catch them here if
    # they get through.
    if ( defined( $rOpts->{'vertical-tightness'} ) ) {
        my $vt = $rOpts->{'vertical-tightness'};
        $rOpts->{'paren-vertical-tightness'}          = $vt;
        $rOpts->{'square-bracket-vertical-tightness'} = $vt;
        $rOpts->{'brace-vertical-tightness'}          = $vt;
    }

    if ( defined( $rOpts->{'vertical-tightness-closing'} ) ) {
        my $vtc = $rOpts->{'vertical-tightness-closing'};
        $rOpts->{'paren-vertical-tightness-closing'}          = $vtc;
        $rOpts->{'square-bracket-vertical-tightness-closing'} = $vtc;
        $rOpts->{'brace-vertical-tightness-closing'}          = $vtc;
    }

    if ( defined( $rOpts->{'closing-token-indentation'} ) ) {
        my $cti = $rOpts->{'closing-token-indentation'};
        $rOpts->{'closing-square-bracket-indentation'} = $cti;
        $rOpts->{'closing-brace-indentation'}          = $cti;
        $rOpts->{'closing-paren-indentation'}          = $cti;
    }

    # Syntax checking is no longer supported due to concerns about executing
    # code in BEGIN blocks.  The flag is still accepted for backwards
    # compatibility but is ignored if set.
    $rOpts->{'check-syntax'} = 0;

    my $check_blank_count = sub {
        my ( $key, $abbrev ) = @_;
        if ( $rOpts->{$key} ) {
            if ( $rOpts->{$key} < 0 ) {
                $rOpts->{$key} = 0;
                Warn("negative value of $abbrev, setting 0\n");
            }
            if ( $rOpts->{$key} > 100 ) {
                Warn("unreasonably large value of $abbrev, reducing\n");
                $rOpts->{$key} = 100;
            }
        }
        return;
    }; ## end $check_blank_count = sub

    # check for reasonable number of blank lines and fix to avoid problems
    $check_blank_count->( 'blank-lines-before-subs',          '-blbs' );
    $check_blank_count->( 'blank-lines-before-packages',      '-blbp' );
    $check_blank_count->( 'blank-lines-after-block-opening',  '-blao' );
    $check_blank_count->( 'blank-lines-before-block-closing', '-blbc' );

    # setting a non-negative logfile gap causes logfile to be saved
    if ( defined( $rOpts->{'logfile-gap'} ) && $rOpts->{'logfile-gap'} >= 0 ) {
        $rOpts->{'logfile'} = 1;
    }

    # set short-cut flag when only indentation is to be done.
    # Note that the user may or may not have already set the
    # indent-only flag.
    if (   !$rOpts->{'add-whitespace'}
        && !$rOpts->{'delete-old-whitespace'}
        && !$rOpts->{'add-newlines'}
        && !$rOpts->{'delete-old-newlines'} )
    {
        $rOpts->{'indent-only'} = 1;
    }

    # -isbc implies -ibc
    if ( $rOpts->{'indent-spaced-block-comments'} ) {
        $rOpts->{'indent-block-comments'} = 1;
    }

    # -bar cannot be used with -bl or -bli; arbitrarily keep -bar
    if ( $rOpts->{'opening-brace-always-on-right'} ) {

        if ( $rOpts->{'opening-brace-on-new-line'} ) {
            Warn(<<EOM);
 Conflict: you specified both 'opening-brace-always-on-right' (-bar) and
  'opening-brace-on-new-line' (-bl).  Ignoring -bl.
EOM
            $rOpts->{'opening-brace-on-new-line'} = 0;
        }
        if ( $rOpts->{'brace-left-and-indent'} ) {
            Warn(<<EOM);
 Conflict: you specified both 'opening-brace-always-on-right' (-bar) and
  '--brace-left-and-indent' (-bli).  Ignoring -bli.
EOM
            $rOpts->{'brace-left-and-indent'} = 0;
        }
    }

    # it simplifies things if -bl is 0 rather than undefined
    if ( !defined( $rOpts->{'opening-brace-on-new-line'} ) ) {
        $rOpts->{'opening-brace-on-new-line'} = 0;
    }

    if ( $rOpts->{'entab-leading-whitespace'} ) {
        if ( $rOpts->{'entab-leading-whitespace'} < 0 ) {
            Warn("-et=n must use a positive integer; ignoring -et\n");
            $rOpts->{'entab-leading-whitespace'} = undef;
        }

        # entab leading whitespace has priority over the older 'tabs' option
        if ( $rOpts->{'tabs'} ) {

            # The following warning could be added but would annoy a lot of
            # users who have a perltidyrc with both -t and -et=n.  So instead
            # there is a note in the manual that -et overrides -t.
            ##Warn("-tabs and -et=n conflict; ignoring -tabs\n");
            $rOpts->{'tabs'} = 0;
        }
    }

    # set a default tabsize to be used in guessing the starting indentation
    # level if and only if this run does not use tabs and the old code does
    # use tabs
    if ( $rOpts->{'default-tabsize'} ) {
        if ( $rOpts->{'default-tabsize'} < 0 ) {
            Warn("negative value of -dt, setting 0\n");
            $rOpts->{'default-tabsize'} = 0;
        }
        if ( $rOpts->{'default-tabsize'} > 20 ) {
            Warn("unreasonably large value of -dt, reducing\n");
            $rOpts->{'default-tabsize'} = 20;
        }
    }
    else {
        $rOpts->{'default-tabsize'} = 8;
    }

    # Check and clean up any sub-alias-list
    if ( defined( $rOpts->{'sub-alias-list'} )
        && length( $rOpts->{'sub-alias-list'} ) )
    {
        my @forced_words;

        # include 'sub' for convenience if this option is used
        push @forced_words, 'sub';

        cleanup_word_list( $rOpts, 'sub-alias-list', \@forced_words );
    }

    make_grep_alias_string($rOpts);

    # Turn on fuzzy-line-length unless this is an extrude run, as determined
    # by the -i and -ci settings. Otherwise blinkers can form (case b935).
    # This is an undocumented parameter used only for stress-testing when
    # --extrude is set.
    if ( !$rOpts->{'fuzzy-line-length'} ) {
        if (   $rOpts->{'maximum-line-length'} != 1
            || $rOpts->{'continuation-indentation'} != 0 )
        {
            $rOpts->{'fuzzy-line-length'} = 1;
        }
    }

    # Large values of -scl can cause convergence problems, issue c167
    if ( $rOpts->{'short-concatenation-item-length'} > 12 ) {
        $rOpts->{'short-concatenation-item-length'} = 12;
    }

    # The freeze-whitespace option is currently a derived option which has its
    # own key
    $rOpts->{'freeze-whitespace'} = !$rOpts->{'add-whitespace'}
      && !$rOpts->{'delete-old-whitespace'};

    # Turn off certain options if whitespace is frozen
    # Note: vertical alignment will be automatically shut off
    if ( $rOpts->{'freeze-whitespace'} ) {
        $rOpts->{'logical-padding'} = 0;
    }

    # Define the default line ending, before any -ple option is applied
    $self->[_line_separator_default_] = get_line_separator_default($rOpts);

    $self->[_line_tidy_begin_] = undef;
    $self->[_line_tidy_end_]   = undef;
    my $line_range_tidy = $rOpts->{'line-range-tidy'};
    if ($line_range_tidy) {

        if ( $num_files > 1 ) {
            Die(<<EOM);
--line-range-tidy expects no more than 1 filename in the arg list but saw $num_files filenames
EOM
        }

        $line_range_tidy =~ s/\s+//g;
        if ( $line_range_tidy =~ /^(\d+):(\d+)?$/ ) {
            my $n1 = $1;
            my $n2 = $2;
            if ( $n1 < 1 ) {
                Die(<<EOM);
--line-range-tidy=n1:n2 expects starting line number n1>=1 but n1=$n1
EOM
            }
            if ( defined($n2) && $n2 < $n1 ) {
                Die(<<EOM);
--line-range-tidy=n1:n2 expects ending line number n2>=n1 but n1=$n1 and n2=$n2
EOM
            }
            $self->[_line_tidy_begin_] = $n1;
            $self->[_line_tidy_end_]   = $n2;
        }
        else {
            Die(
"unrecognized 'line-range-tidy'; expecting format '-lrt=n1:n2'\n"
            );
        }
    }

    return;
} ## end sub check_options

sub find_file_upwards {

    my ( $search_dir, $search_file ) = @_;

    # This implements the ... upward search for a file

    $search_dir  =~ s{/+$}{};
    $search_file =~ s{^/+}{};

    while (1) {
        my $try_path = "$search_dir/$search_file";
        if ( -f $try_path ) {
            return $try_path;
        }
        elsif ( $search_dir eq '/' ) {
            return;
        }
        else {
            $search_dir = dirname($search_dir);
        }
    } ## end while (1)

    # This return is for Perl-Critic.
    # We shouldn't get out of the while loop without a return
    return;
} ## end sub find_file_upwards

sub expand_command_abbreviations {

    my ( $rexpansion, $rraw_options, $config_file ) = @_;

    # Go through @ARGV and expand any abbreviations
    # Note that @ARGV has been localized

    # Set a pass limit to prevent an infinite loop;
    # 10 should be plenty, but it may be increased to allow deeply
    # nested expansions.
    my $max_passes = 10;

    # keep looping until all expansions have been converted into actual
    # dash parameters..
    foreach my $pass_count ( 0 .. $max_passes ) {
        my @new_argv     = ();
        my $abbrev_count = 0;

        # loop over each item in @ARGV..
        foreach my $word (@ARGV) {

            # convert any leading 'no-' to just 'no'
            if ( $word =~ /^(-[-]?no)-(.*)/ ) { $word = $1 . $2 }

            # if it is a dash flag (instead of a file name)..
            if ( $word =~ /^-[-]?([\w\-]+)(.*)/ ) {

                my $abr   = $1;
                my $flags = $2;

                # save the raw input for debug output in case of circular refs
                if ( $pass_count == 0 ) {
                    push( @{$rraw_options}, $word );
                }

                # recombine abbreviation and flag, if necessary,
                # to allow abbreviations with arguments such as '-vt=1'
                if ( $rexpansion->{ $abr . $flags } ) {
                    $abr   = $abr . $flags;
                    $flags = EMPTY_STRING;
                }

                # if we see this dash item in the expansion hash..
                if ( $rexpansion->{$abr} ) {
                    $abbrev_count++;

                    # stuff all of the words that it expands to into the
                    # new arg list for the next pass
                    foreach my $abbrev ( @{ $rexpansion->{$abr} } ) {
                        next unless ($abbrev);    # for safety; shouldn't happen
                        push( @new_argv, '--' . $abbrev . $flags );
                    }
                }

                # not in expansion hash, must be actual long name
                else {
                    push( @new_argv, $word );
                }
            }

            # not a dash item, so just save it for the next pass
            else {
                push( @new_argv, $word );
            }
        } ## end of this pass

        # update parameter list @ARGV to the new one
        @ARGV = @new_argv;
        last if ( !$abbrev_count );

        # make sure we are not in an infinite loop
        if ( $pass_count == $max_passes ) {
            local $LIST_SEPARATOR = ')(';
            Warn(<<EOM);
I'm tired. We seem to be in an infinite loop trying to expand aliases.
Here are the raw options;
(rraw_options)
EOM
            my $num = @new_argv;
            if ( $num < 50 ) {
                Warn(<<EOM);
After $max_passes passes here is ARGV
(@new_argv)
EOM
            }
            else {
                Warn(<<EOM);
After $max_passes passes ARGV has $num entries
EOM
            }

            if ( defined($config_file) ) {
                Die(<<"DIE");
Please check your configuration file $config_file for circular-references.
To deactivate it, use -npro.
DIE
            }
            else {
                Die(<<'DIE');
Program bug - circular-references in the %expansion hash, probably due to
a recent program change.
DIE
            }
        } ## end of check for circular references
    } ## end of loop over all passes
    return;
} ## end sub expand_command_abbreviations

sub check_for_missing_string_options {
    my ( $ris_string_option, ($config_file) ) = @_;

    # Given:
    #  $ris_string_option = hash with keys are options of type '=s'
    #  ($config_file) = optional parameter:
    #     - name of config file if processing config file
    #     - undef if processing command line args

    # Task:
    # Look through @ARGV for string options which are not immediately followed
    # by '=string'.  If the next word looks like another --option, then it may
    # get gobbled up as the string arg. In that case, exit with an error
    # message. The user can always force a string arg which looks like an
    # option by using the '=string' input form.

    # Example of the type of error this sub checks for:

    #   perltidy -lpil -l=9 filename

    # In this sub, any short option forms have already been expanded into their
    # long forms, so this will appear here in the local copy of @ARGV as three
    # list items:

    #   @ARGV = qw(
    #     --line-up-parentheses-inclusion-list
    #     --maximum-line-length=9
    #     filename
    #   );

    # Then, since -lpil wants a string value, it will be set equal to
    # '--line-up-parentheses=9' by sub GetOptions, which is probably not the
    # desired value.

    # This sub will catch most errors of this type at the earliest possible
    # stage. One exception is if the user enters just part of an option name
    # and relies on name completion by sub GetOptions.  Another exception is if
    # a filename follows the missing string option on the command line. In
    # those cases we have to rely on later checks.

    my $arg_seeking_string_last;
    my $error_message = EMPTY_STRING;
    foreach my $arg (@ARGV) {

        my $arg_seeking_string;

        # something like --option ?
        if ( substr( $arg, 0, 2 ) eq '--' && length($arg) > 2 ) {

            # Will the previous string without arg try to grab this option?
            if ( $arg_seeking_string_last && $arg =~ /^\-\-[A-Za-z]/ ) {
                $error_message .= <<EOM;
  '$arg_seeking_string_last' may be missing its string parameter.
EOM
            }

            # Is this a string option without a following '=value' ?
            if ( index( $arg, '=' ) < 0
                && $ris_string_option->{ substr( $arg, 2 ) } )
            {
                $arg_seeking_string = $arg;
            }

        }
        $arg_seeking_string_last = $arg_seeking_string;
    }

    if ($error_message) {
        my $pre_note = "Possible error ";
        $pre_note .=
          defined($config_file)
          ? "in config file '$config_file':\n"
          : "on the command line:\n";
        my $post_note =
          "Use the equals form '--option=string' to avoid this message.\n";
        Die( $pre_note . $error_message . $post_note );
    }
    return;
} ## end sub check_for_missing_string_options

sub dump_short_names {

    my $rexpansion = shift;

    # do --dump-short-names (-dsn)
    # Debug routine -- this will dump the expansion hash

    print {*STDOUT} <<EOM;
List of short names.  This list shows how all abbreviations are
translated into other abbreviations and, eventually, into long names.
New abbreviations may be defined in a .perltidyrc file.
For a list of all long names, use perltidy --dump-long-names (-dln).
--------------------------------------------------------------------------
EOM
    foreach my $abbrev ( sort keys %{$rexpansion} ) {
        my @list = @{ $rexpansion->{$abbrev} };
        print {*STDOUT} "$abbrev --> @list\n";
    }
    return;
} ## end sub dump_short_names

sub check_vms_filename {

    my $filename = shift;

    # Given a valid filename (the perltidy input file)
    # create a modified filename and separator character
    # suitable for VMS.
    #
    # Contributed by Michael Cartmell
    #
    my ( $base, $path ) = fileparse($filename);

    # remove explicit ; version
    $base =~ s/;-?\d*$//

      # remove explicit . version, i.e. two dots in filename NB ^ escapes a dot
      or $base =~ s{(          # begin capture $1
                  (?:^|[^^])\. # match a dot not preceded by a caret
                  (?:          # followed by nothing
                    |          # or
                    .*[^^]     # anything ending in a non caret
                  )
                )              # end capture $1
                \.-?\d*$       # match . version number
              }{$1}x;

    # normalize filename, if there are no unescaped dots then append one
    $base .= '.' unless ( $base =~ /(?:^|[^^])\./ );

    # if we don't already have an extension then we just append the extension
    my $separator = ( $base =~ /\.$/ ) ? EMPTY_STRING : "_";
    return ( $path . $base, $separator );
} ## end sub check_vms_filename

sub Win_OS_Type {

    my $rpending_complaint = shift;

    # Returns a string that determines what MS OS we are on.
    # Returns win32s,95,98,Me,NT3.51,NT4,2000,XP/.Net,Win2003
    # Returns blank string if not an MS system.
    # Original code contributed by: Yves Orton
    # We need to know this to decide where to look for config files

    # TODO: are these more standard names?
    # Win32s Win95 Win98 WinMe WinNT3.51 WinNT4 Win2000 WinXP/.Net Win2003

    my $os = EMPTY_STRING;
    return $os unless ( $OSNAME =~ /win32|dos/i );    # is it a MS box?

    # Systems built from Perl source may not have Win32.pm
    # But probably have Win32::GetOSVersion() anyway so the
    # following line is not 'required':
    # return $os unless eval('require Win32');

    # Use the standard API call to determine the version
    my ( $undef, $major, $minor, $build, $id );
    my $ok = eval {
        ( $undef, $major, $minor, $build, $id ) = Win32::GetOSVersion();
        1;
    };
    if ( !$ok && DEVEL_MODE ) {
        Fault("Could not cal Win32::GetOSVersion(): $EVAL_ERROR\n");
    }

    #
    #    NAME                   ID   MAJOR  MINOR
    #    Windows NT 4           2      4       0
    #    Windows 2000           2      5       0
    #    Windows XP             2      5       1
    #    Windows Server 2003    2      5       2

    return "win32s" unless ($id);    # If id==0 then its a win32s box.
    $os = {                          # Magic numbers from MSDN
                                     # documentation of GetOSVersion
        1 => {
            0  => "95",
            10 => "98",
            90 => "Me",
        },
        2 => {
            0  => "2000",      # or NT 4, see below
            1  => "XP/.Net",
            2  => "Win2003",
            51 => "NT3.51",
        },
    }->{$id}->{$minor};

    # If $os is undefined, the above code is out of date.  Suggested updates
    # are welcome.
    if ( !defined($os) ) {
        $os = EMPTY_STRING;

        # Deactivated this message 20180322 because it was needlessly
        # causing some test scripts to fail.  Need help from someone
        # with expertise in Windows to decide what is possible with windows.
        ${$rpending_complaint} .= <<EOS if (0);
Error trying to discover Win_OS_Type: $id:$major:$minor Has no name of record!
We won't be able to look for a system-wide config file.
EOS
    }

    # Unfortunately the logic used for the various versions isn't so clever..
    # so we have to handle an outside case.
    return ( $os eq "2000" && $major != 5 ) ? "NT4" : $os;
} ## end sub Win_OS_Type

sub look_for_Windows {

    my $rpending_complaint = shift;

    # Determine Windows sub-type and location of
    # system-wide configuration files
    my $is_Windows = ( $OSNAME =~ /win32|dos/i );
    my $Windows_type;
    $Windows_type = Win_OS_Type($rpending_complaint) if ($is_Windows);
    return ( $is_Windows, $Windows_type );
} ## end sub look_for_Windows

sub find_config_file {

    my ( $is_Windows, $Windows_type, $rconfig_file_chatter,
        $rpending_complaint )
      = @_;

    # Look for a .perltidyrc configuration file
    # For Windows also look for a file named perltidy.ini

    ${$rconfig_file_chatter} .= "# Config file search...system reported as:";
    if ($is_Windows) {
        ${$rconfig_file_chatter} .= "Windows $Windows_type\n";
    }
    else {
        ${$rconfig_file_chatter} .= " $OSNAME\n";
    }

    # sub to check file existence and record all tests
    my $exists_config_file = sub {
        my $config_file = shift;
        return 0 unless ( defined($config_file) );
        ${$rconfig_file_chatter} .= "# Testing: $config_file\n";
        return -f $config_file;
    }; ## end $exists_config_file = sub

    # Sub to search upward for config file
    my $resolve_config_file = sub {

        # resolve <dir>/.../<file>, meaning look upwards from directory
        my $config_file = shift;
        if ( defined($config_file) ) {
            if ( my ( $start_dir, $search_file ) =
                ( $config_file =~ m{^(.*)\.\.\./(.*)$} ) )
            {
                ${$rconfig_file_chatter} .=
                  "# Searching Upward: $config_file\n";
                $start_dir = '.' if ( !$start_dir );
                $start_dir = Cwd::realpath($start_dir);
                my $found_file = find_file_upwards( $start_dir, $search_file );
                if ( defined($found_file) ) {
                    $config_file = $found_file;
                    ${$rconfig_file_chatter} .= "# Found: $config_file\n";
                }
            }
        }
        return $config_file;
    }; ## end $resolve_config_file = sub

    my $config_file;

    # look in current directory first
    $config_file = ".perltidyrc";
    return $config_file if ( $exists_config_file->($config_file) );
    if ($is_Windows) {
        $config_file = "perltidy.ini";
        return $config_file if ( $exists_config_file->($config_file) );
    }

    # Default environment vars.
    my @envs = qw( PERLTIDY HOME );

    # Check the NT/2k/XP locations, first a local machine def, then a
    # network def
    push @envs, qw( USERPROFILE HOMESHARE ) if ( $OSNAME =~ /win32/i );

    # Now go through the environment ...
    foreach my $var (@envs) {
        ${$rconfig_file_chatter} .= "# Examining: \$ENV{$var}";
        if ( defined( $ENV{$var} ) ) {
            ${$rconfig_file_chatter} .= " = $ENV{$var}\n";

            # test ENV{ PERLTIDY } as file:
            if ( $var eq 'PERLTIDY' ) {
                $config_file = "$ENV{$var}";
                $config_file = $resolve_config_file->($config_file);
                return $config_file if ( $exists_config_file->($config_file) );
            }

            # test ENV as directory:
            $config_file = File::Spec->catfile( $ENV{$var}, ".perltidyrc" );
            $config_file = $resolve_config_file->($config_file);
            return $config_file if ( $exists_config_file->($config_file) );

            if ($is_Windows) {
                $config_file =
                  File::Spec->catfile( $ENV{$var}, "perltidy.ini" );
                $config_file = $resolve_config_file->($config_file);
                return $config_file if ( $exists_config_file->($config_file) );
            }
        }
        else {
            ${$rconfig_file_chatter} .= "\n";
        }
    }

    # then look for a system-wide definition
    # where to look varies with OS
    if ($is_Windows) {

        if ($Windows_type) {
            my ( $os_uu, $system, $allusers ) =
              Win_Config_Locs( $rpending_complaint, $Windows_type );

            # Check All Users directory, if there is one.
            # i.e. C:\Documents and Settings\User\perltidy.ini
            if ($allusers) {

                $config_file = File::Spec->catfile( $allusers, ".perltidyrc" );
                return $config_file if ( $exists_config_file->($config_file) );

                $config_file = File::Spec->catfile( $allusers, "perltidy.ini" );
                return $config_file if ( $exists_config_file->($config_file) );
            }

            # Check system directory.
            # retain old code in case someone has been able to create
            # a file with a leading period.
            $config_file = File::Spec->catfile( $system, ".perltidyrc" );
            return $config_file if ( $exists_config_file->($config_file) );

            $config_file = File::Spec->catfile( $system, "perltidy.ini" );
            return $config_file if ( $exists_config_file->($config_file) );
        }
    }

    # Place to add customization code for other systems
    elsif ( $OSNAME eq 'OS2' ) {
    }
    elsif ( $OSNAME eq 'MacOS' ) {
    }
    elsif ( $OSNAME eq 'VMS' ) {
    }

    # Assume some kind of Unix
    else {

        $config_file = "/usr/local/etc/perltidyrc";
        return $config_file if ( $exists_config_file->($config_file) );

        $config_file = "/etc/perltidyrc";
        return $config_file if ( $exists_config_file->($config_file) );
    }

    # Couldn't find a config file
    return;
} ## end sub find_config_file

sub Win_Config_Locs {

    my ( $rpending_complaint, $os ) = @_;

    # In scalar context returns the OS name (95 98 ME NT3.51 NT4 2000 XP),
    # or undef if its not a win32 OS.  In list context returns OS, System
    # Directory, and All Users Directory.  All Users will be empty on a
    # 9x/Me box.  Contributed by: Yves Orton.

    if ( !$os ) { $os = Win_OS_Type($rpending_complaint) }

    return unless ($os);

    my $system   = EMPTY_STRING;
    my $allusers = EMPTY_STRING;

    if ( $os =~ /9[58]|Me/ ) {
        $system = "C:/Windows";
    }
    elsif ( $os =~ /NT|XP|200?/ ) {
        $system = ( $os =~ /XP/ ) ? "C:/Windows/" : "C:/WinNT/";
        $allusers =
          ( $os =~ /NT/ )
          ? "C:/WinNT/profiles/All Users/"
          : "C:/Documents and Settings/All Users/";
    }
    else {

        # This currently would only happen on a win32s computer.  I don't have
        # one to test, so I am unsure how to proceed.  Suggestions welcome!
        ${$rpending_complaint} .=
"I don't know a sensible place to look for config files on an $os system.\n";
        return;
    }
    return ( $os, $system, $allusers );
} ## end sub Win_Config_Locs

sub dump_config_file {

    my ( $rconfig_string, $config_file, $rconfig_file_chatter ) = @_;

    # do --dump-profile (-dpro)

    print {*STDOUT} "${$rconfig_file_chatter}";
    if ($rconfig_string) {
        my @lines = split /^/, ${$rconfig_string};
        print {*STDOUT} "# Dump of file: '$config_file'\n";
        foreach my $line (@lines) { print {*STDOUT} $line }
    }
    else {
        print {*STDOUT} "# ...no config file found\n";
    }
    return;
} ## end sub dump_config_file

sub filter_unknown_options {

    my (
        $rconfig_string, $roption_category,
        $rexpansion,     $rconfig_file_chatter
    ) = @_;

    # Look through the configuration file for lines beginning with '---' and
    # - remove the line if the option is unknown, or
    # - remove the extra dash if the option is known
    # See git #146 for discussion

    # Given:
    #   $rconfig_string = string ref to a .perltidyrc configuration file
    #   $roption_category = ref to hash with long_names as key
    #   $rexpansion = ref to hash with abbreviations as key
    #   $rconfig_file_chatter = messages displayed in --dump-profile
    #
    # Update:
    #   $rconfig_string and $rconfig_file_chatter

    # quick check to skip most files
    if ( ${$rconfig_string} !~ /^\s*---\w/m ) { return }

    my $new_config_string;
    my $change_notices = EMPTY_STRING;
    my @lines          = split /^/, ${$rconfig_string};
    foreach my $line (@lines) {
        chomp $line;

        # look for lines beginning with '---'
        if ( $line && $line =~ /^\s*---(\w[\w-]*)/ ) {
            my $word = $1;

            # first look for a long name or an abbreviation
            my $is_known = $roption_category->{$word} || $rexpansion->{$word};

            # then look for prefix 'no' or 'no-' on a long name
            if ( !$is_known && $word =~ s/^no-?// ) {
                $is_known = $roption_category->{$word};
            }

            if ( !$is_known ) {
                $change_notices .= "#  removing unknown option line $line\n";
                next;
            }
            else {
                $change_notices .= "#  accepting and fixing line $line\n";
                $line =~ s/-//;
            }
        }
        $new_config_string .= $line . "\n";
    }

    if ($change_notices) {
        ${$rconfig_file_chatter} .= "# Filter operations:\n" . $change_notices;
        ${$rconfig_string} = $new_config_string;
    }
    return;
} ## end sub filter_unknown_options

sub read_config_file {

    my ( $rconfig_string, $config_file, $rexpansion ) = @_;

    # Read and process the contents of a perltidyrc command file

    # Given:
    #   $rconfig_string = ref to the file as a string
    #   $config_file = name of the file, for error reporting
    #   $rexpansion = ref to hash of abbreviations; if this config file defines
    #                 any abbreviations they will be added to it

    # Return:
    #   \@config_list = ref to final parameters and values which will be
    #     placed in @ARGV for processing by GetOptions
    #   $death_message = error message returned if a fatal error occurs
    my @config_list = ();

    # remove side comments and join multiline quotes
    my ( $rline_hash, $death_message ) =
      strip_comments_and_join_quotes( $rconfig_string, $config_file );

    # file is bad if non-empty $death_message is returned
    if ($death_message) {
        return ( \@config_list, $death_message );
    }

    my $name = undef;
    my $opening_brace_line;
    foreach my $item ( @{$rline_hash} ) {
        my $line    = $item->{line};
        my $line_no = $item->{line_no};
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        next unless ( length($line) );

        my $body = $line;

        # Look for complete or partial abbreviation definition of the form
        #     name { body }   or  name {   or    name { body
        # See rules in perltidy's perldoc page
        # Section: Other Controls - Creating a new abbreviation
        if ( $line =~ /^(?: (\w+) \s* \{ ) (.*)? $/x ) {
            ( $name, $body ) = ( $1, $2 );

            # Cannot start new abbreviation unless old abbreviation is complete
            last if ($opening_brace_line);

            $opening_brace_line = $line_no unless ( $body && $body =~ s/\}$// );

            # handle a new alias definition
            if ( $rexpansion->{$name} ) {
                local $LIST_SEPARATOR = ')(';
                my @names = sort keys %{$rexpansion};
                $death_message =
                    "Here is a list of all installed aliases\n(@names)\n"
                  . "Attempting to redefine alias ($name) in config file $config_file line $INPUT_LINE_NUMBER\n";
                last;
            }
            $rexpansion->{$name} = [];
        }

        # leading opening braces not allowed
        elsif ( $line =~ /^{/ ) {
            $opening_brace_line = undef;
            $death_message =
              "Unexpected '{' at line $line_no in config file '$config_file'\n";
            last;
        }

        # Look for abbreviation closing:    body }   or    }
        elsif ( $line =~ /^(.*)?\}$/ ) {
            $body = $1;
            if ($opening_brace_line) {
                $opening_brace_line = undef;
            }
            else {
                $death_message =
"Unexpected '}' at line $line_no in config file '$config_file'\n";
                last;
            }
        }
        else {
            # no abbreviations to untangle
        }

        # Now store any parameters
        if ($body) {

            my ( $rbody_parts, $msg ) = parse_args($body);
            if ($msg) {
                $death_message = <<EOM;
Error reading file '$config_file' at line number $line_no.
$msg
Please fix this line or use -npro to avoid reading this file
EOM
                last;
            }

            if ($name) {

                # remove leading dashes if this is an alias
                foreach ( @{$rbody_parts} ) { s/^\-+//; }
                push @{ $rexpansion->{$name} }, @{$rbody_parts};
            }
            else {
                push( @config_list, @{$rbody_parts} );
            }
        }
    }

    if ($opening_brace_line) {
        $death_message =
"Didn't see a '}' to match the '{' at line $opening_brace_line in config file '$config_file'\n";
    }
    return ( \@config_list, $death_message );
} ## end sub read_config_file

sub strip_comments_and_join_quotes {

    my ( $rconfig_string, $config_file ) = @_;

    # Tasks:
    # 1. Strip comments from .perltidyrc lines
    # 2. Join lines which are spanned by a quote

    # Given:
    #   $rconfig_string = the configuration file
    #   $config_file = filename, for error messages
    # Return:
    #   $rline_hash = hash with modified lines and their input numbers
    #   $msg = any error message; code will die on any message.

    # return variables
    my $msg        = EMPTY_STRING;
    my $rline_hash = [];

    # quote state variables
    my $quote_char          = EMPTY_STRING;
    my $quote_start_line    = EMPTY_STRING;
    my $quote_start_line_no = -1;
    my $in_string           = EMPTY_STRING;
    my $out_string          = EMPTY_STRING;

    my @lines   = split /^/, ${$rconfig_string};
    my $line_no = 0;

    # loop over lines
    foreach my $line (@lines) {
        $line_no++;
        $line =~ s/^\s+//;
        $line =~ s/\s+$//;
        next unless ( length($line) );

        if ( !$quote_char ) {

            # skip a full-line comment
            if ( substr( $line, 0, 1 ) eq '#' ) {
                next;
            }
            $in_string  = $line;
            $out_string = EMPTY_STRING;
        }
        else {

            # treat previous newline as a space
            $in_string = SPACE . $line;
        }

        # loop over string characters
        #  $in_string  = the input string
        #  $out_string = the output string
        #  $quote_char = quote character being sought
        while (1) {

            # accumulating characters not in quote
            if ( !$quote_char ) {

                if ( $in_string =~ /\G([\"\'])/gc ) {

                    # starting new quote..
                    $out_string .= $1;
                    $quote_char          = $1;
                    $quote_start_line_no = $line_no;
                    $quote_start_line    = $line;
                }
                elsif ( $in_string =~ /\G#/gc ) {

                    # A space is required before the # of a side comment
                    # This allows something like:
                    #    -sbcp=#
                    # Otherwise, it would have to be quoted:
                    #    -sbcp='#'
                    if ( !length($out_string) || $out_string =~ s/\s+$// ) {
                        last;
                    }
                    $out_string .= '#';
                }
                elsif ( $in_string =~ /\G([^\#\'\"]+)/gc ) {

                    # neither quote nor side comment
                    $out_string .= $1;
                }
                else {

                    # end of line
                    last;
                }
            }

            # looking for ending quote character
            else {
                if ( $in_string =~ /\G($quote_char)/gc ) {

                    # end of quote
                    $out_string .= $1;
                    $quote_char = EMPTY_STRING;
                }
                elsif ( $in_string =~ /\G([^$quote_char]+)/gc ) {

                    # accumulate quoted text
                    $out_string .= $1;
                }
                else {

                    # end of line
                    last;
                }
            }
        } ## end while (1)

        if ( !$quote_char ) {
            push @{$rline_hash},
              {
                line    => $out_string,
                line_no => $line_no,
              };
        }

    } ## end loop over lines

    if ($quote_char) {
        my $max_len = 80;
        if ( length($quote_start_line) > $max_len ) {
            $quote_start_line =
              substr( $quote_start_line, 0, $max_len - 3 ) . '...';
        }
        $msg = <<EOM;
Error: hit EOF reading file '$config_file' looking for end of quoted text
which started at line $quote_start_line_no with quote character <$quote_char>:
$quote_start_line
Please fix or use -npro to avoid reading this file
EOM
    }
    return ( $rline_hash, $msg );
} ## end sub strip_comments_and_join_quotes

sub parse_args {

    my ($body) = @_;

    # Parse a command string $body containing multiple string with possible
    # quotes, into individual commands.  It might look like this, for example:
    #
    #    -wba=" + - "  -some-thing -wbb='. && ||'
    #
    # There is no need, at present, to handle escaped quote characters.
    # (They are not perltidy tokens, so needn't be in strings).

    my @body_parts = ();
    my $quote_char = EMPTY_STRING;
    my $part       = EMPTY_STRING;
    my $msg        = EMPTY_STRING;

    # Check for external call with undefined $body - added to fix
    # github issue Perl-Tidy-Sweetened issue #23
    if ( !defined($body) ) { $body = EMPTY_STRING }

    while (1) {

        # looking for ending quote character
        if ($quote_char) {
            if ( $body =~ /\G($quote_char)/gc ) {
                $quote_char = EMPTY_STRING;
            }
            elsif ( $body =~ /\G(.)/gc ) {
                $part .= $1;
            }

            # error..we reached the end without seeing the ending quote char
            else {
                if ( length($part) ) { push @body_parts, $part; }
                $msg = <<EOM;
Did not see ending quote character <$quote_char> in this text:
$body
EOM
                last;
            }
        }

        # accumulating characters and looking for start of a quoted string
        else {
            if ( $body =~ /\G([\"\'])/gc ) {
                $quote_char = $1;
            }
            elsif ( $body =~ /\G(\s+)/gc ) {
                if ( length($part) ) { push @body_parts, $part; }
                $part = EMPTY_STRING;
            }
            elsif ( $body =~ /\G(.)/gc ) {
                $part .= $1;
            }
            else {
                if ( length($part) ) { push @body_parts, $part; }
                last;
            }
        }
    } ## end while (1)
    return ( \@body_parts, $msg );
} ## end sub parse_args

sub dump_long_names {

    my @names = @_;

    # do --dump-long-names (-dln)

    print {*STDOUT} <<EOM;
# Command line long names (passed to GetOptions)
#--------------------------------------------------
# here is a summary of the Getopt codes:
# <none> does not take an argument
# =s takes a mandatory string
# :s takes an optional string
# =i takes a mandatory integer
# :i takes an optional integer
# ! does not take an argument and may be negated
#  i.e., -foo and -nofoo are allowed
# a double dash signals the end of the options list
#
#--------------------------------------------------
EOM

    foreach my $name ( sort @names ) { print {*STDOUT} "$name\n" }
    return;
} ## end sub dump_long_names

sub dump_integer_option_range {

    my ($rinteger_option_range) = @_;

    # do --dump-integer-option-range (-dior)

    print {*STDOUT} "Option, min, max, default\n";
    foreach my $key ( sort keys %{$rinteger_option_range} ) {
        my ( $min, $max, $default ) = @{ $rinteger_option_range->{$key} };
        foreach ( $min, $max, $default ) {
            $_ = 'undef' unless ( defined($_) );
        }
        print {*STDOUT} "$key, $min, $max, $default\n";
    }
    return;
} ## end sub dump_integer_option_range

sub dump_defaults {

    my @defaults = @_;

    #  do --dump-defaults (-ddf)
    print {*STDOUT} "Default command line options:\n";
    foreach my $line ( sort @defaults ) { print {*STDOUT} "$line\n" }
    return;
} ## end sub dump_defaults

sub readable_options {

    my ( $rOpts, $roption_string ) = @_;

    # return options for this run as a string which could be
    # put in a perltidyrc file
    my %Getopt_flags;
    my $rGetopt_flags    = \%Getopt_flags;
    my $readable_options = "# Final parameter set for this run.\n";
    $readable_options .=
      "# See utility 'perltidyrc_dump.pl' for nicer formatting.\n";
    foreach my $opt ( @{$roption_string} ) {
        my $flag = EMPTY_STRING;
        if ( $opt =~ /(.*)(!|=.*)$/ ) {
            $opt  = $1;
            $flag = $2;
        }
        if ( defined( $rOpts->{$opt} ) ) {
            $rGetopt_flags->{$opt} = $flag;
        }
    }
    foreach my $key ( sort keys %{$rOpts} ) {
        my $flag   = $rGetopt_flags->{$key};
        my $value  = $rOpts->{$key};
        my $prefix = '--';
        my $suffix = EMPTY_STRING;
        if ($flag) {
            if ( $flag =~ /^=/ ) {
                if ( $value !~ /^\d+$/ ) { $value = '"' . $value . '"' }
                $suffix = "=" . $value;
            }
            elsif ( $flag =~ /^!/ ) {
                $prefix .= "no" unless ($value);
            }
            else {

                # shouldn't happen
                $readable_options .=
                  "# ERROR in dump_options: unrecognized flag $flag for $key\n";
            }
        }
        $readable_options .= $prefix . $key . $suffix . "\n";
    }
    return $readable_options;
} ## end sub readable_options

sub show_version {
    print {*STDOUT} <<"EOM";
This is perltidy, v$VERSION

Copyright 2000-2025 by Steve Hancock

Perltidy is free software and may be copied under the terms of the GNU
General Public License, which is included in the distribution files.

Documentation can be found using 'man perltidy'
or at GitHub      https://perltidy.github.io/perltidy/
or at metacpan    https://metacpan.org/pod/distribution/Perl-Tidy/bin/perltidy
or at Sourceforge https://perltidy.sourceforge.net
EOM
    return;
} ## end sub show_version

sub usage {

    # Dump brief usage message if arg is -help or -h, or on certain errors

    print {*STDOUT} <<EOF;
This is perltidy version $VERSION, a perl script indenter.  Usage:

    perltidy [ options ] file1 file2 file3 ...
            (output goes to file1.tdy, file2.tdy, file3.tdy, ...)
    perltidy [ options ] file1 -o outfile
    perltidy [ options ] file1 -st >outfile
    perltidy [ options ] <infile >outfile

Options have short and long forms. Short forms are shown; see
man pages for long forms.  Note: '=s' indicates a required string,
and '=n' indicates a required integer.

I/O control
 -h      show this help
 -o=file name of the output file (only if single input file)
 -oext=s change output extension from 'tdy' to s
 -opath=path  change path to be 'path' for output files
 -b      backup original to .bak and modify file in-place
 -bext=s change default backup extension from 'bak' to s
 -q      deactivate error messages (for running under editor)
 -w      include non-critical warning messages in the .ERR error output
 -log    save .LOG file, which has useful diagnostics
 -f      force perltidy to read a binary file
 -g      like -log but writes more detailed .LOG file, for debugging scripts
 -opt    write the set of options actually used to a .LOG file
 -npro   ignore .perltidyrc configuration command file
 -pro=file   read configuration commands from file instead of .perltidyrc
 -st     send output to standard output, STDOUT
 -se     send all error output to standard error output, STDERR
 -v      display version number to standard output and quit

Basic Options:
 -i=n    use n columns per indentation level (default n=4)
 -t      tabs: use one tab character per indentation level, not recommended
 -nt     no tabs: use n spaces per indentation level (default)
 -et=n   entab leading whitespace n spaces per tab; not recommended
 -io     "indent only": just do indentation, no other formatting.
 -sil=n  set starting indentation level to n;  use if auto detection fails
 -ole=s  specify output line ending (s=dos or win, mac, unix)
 -ple    keep output line endings same as input (input must be filename)

Whitespace Control
 -fws    freeze whitespace; this disables all whitespace changes
           and disables the following switches:
 -bt=n   sets brace tightness,  n= (0 = loose, 1=default, 2 = tight)
 -bbt    same as -bt but for code block braces; same as -bt if not given
 -bbvt   block braces vertically tight; use with -bl or -bli
 -bbvtl=s  make -bbvt to apply to selected list of block types
 -pt=n   paren tightness (n=0, 1 or 2)
 -sbt=n  square bracket tightness (n=0, 1, or 2)
 -bvt=n  brace vertical tightness,
         n=(0=open, 1=close unless multiple steps on a line, 2=always close)
 -pvt=n  paren vertical tightness (see -bvt for n)
 -sbvt=n square bracket vertical tightness (see -bvt for n)
 -bvtc=n closing brace vertical tightness:
         n=(0=open, 1=sometimes close, 2=always close)
 -pvtc=n closing paren vertical tightness, see -bvtc for n.
 -sbvtc=n closing square bracket vertical tightness, see -bvtc for n.
 -ci=n   sets continuation indentation=n,  default is n=2 spaces
 -lp     line up parentheses, brackets, and non-BLOCK braces
 -sfs    add space before semicolon in for( ; ; )
 -aws    allow perltidy to add whitespace (default)
 -dws    delete all old non-essential whitespace
 -icb    indent closing brace of a code block
 -cti=n  closing indentation of paren, square bracket, or non-block brace:
         n=0 none, =1 align with opening, =2 one full indentation level
 -icp    equivalent to -cti=2
 -wls=s  want space left of tokens in string; i.e. -nwls='+ - * /'
 -wrs=s  want space right of tokens in string;
 -sts    put space before terminal semicolon of a statement
 -sak=s  put space between keywords given in s and '(';
 -nsak=s no space between keywords in s and '('; i.e. -nsak='my our local'

Line Break Control
 -fnl    freeze newlines; this disables all line break changes
            and disables the following switches:
 -anl    add newlines;  ok to introduce new line breaks
 -bbs    add blank line before subs and packages
 -bbc    add blank line before block comments
 -bbb    add blank line between major blocks
 -kbl=n  keep old blank lines? 0=no, 1=some, 2=all
 -mbl=n  maximum consecutive blank lines to output (default=1)
 -ce     cuddled else; use this style: '} else {'
 -cb     cuddled blocks (other than 'if-elsif-else')
 -cbl=s  list of blocks to cuddled, default 'try-catch-finally'
 -dnl    delete old newlines (default)
 -l=n    maximum line length;  default n=80
 -bl     opening brace on new line
 -sbl    opening sub brace on new line.  value of -bl is used if not given.
 -bli    opening brace on new line and indented
 -bar    opening brace always on right, even for long clauses
 -vt=n   vertical tightness (requires -lp); n controls break after opening
         token: 0=never  1=no break if next line balanced   2=no break
 -vtc=n  vertical tightness of closing container; n controls if closing
         token starts new line: 0=always  1=not unless list  1=never
 -wba=s  want break after tokens in string; i.e. wba=': .'
 -wbb=s  want break before tokens in string
 -wn     weld nested: combines opening and closing tokens when both are adjacent
 -wnxl=s weld nested exclusion list: provides some control over the types of
         containers which can be welded

Following Old Breakpoints
 -kis    keep interior semicolons.  Allows multiple statements per line.
 -boc    break at old comma breaks: turns off all automatic list formatting
 -bol    break at old logical breakpoints: or, and, ||, && (default)
 -bom    break at old method call breakpoints: ->
 -bok    break at old list keyword breakpoints such as map, sort (default)
 -bot    break at old conditional (ternary ?:) operator breakpoints (default)
 -boa    break at old attribute breakpoints
 -cab=n  break at commas after a comma-arrow (=>):
         n=0 break at all commas after =>
         n=1 stable: break unless this breaks an existing one-line container
         n=2 break only if a one-line container cannot be formed
         n=3 do not treat commas after => specially at all

Comment controls
 -ibc    indent block comments (default)
 -isbc   indent spaced block comments; may indent unless no leading space
 -msc=n  minimum desired spaces to side comment, default 4
 -fpsc=n fix position for side comments; default 0;
 -csc    add or update closing side comments after closing BLOCK brace
 -dcsc   delete closing side comments created by a -csc command
 -cscp=s change closing side comment prefix to be other than '## end'
 -cscl=s change closing side comment to apply to selected list of blocks
 -csci=n minimum number of lines needed to apply a -csc tag, default n=6
 -csct=n maximum number of columns of appended text, default n=20
 -cscw   causes warning if old side comment is overwritten with -csc

 -sbc    use 'static block comments' identified by leading '##' (default)
 -sbcp=s change static block comment identifier to be other than '##'
 -osbc   outdent static block comments

 -ssc    use 'static side comments' identified by leading '##' (default)
 -sscp=s change static side comment identifier to be other than '##'

Delete selected text
 -dac    delete all comments AND pod
 -dbc    delete block comments
 -dsc    delete side comments
 -dp     delete pod

Send selected text to a '.TEE' file
 -tac    tee all comments AND pod
 -tbc    tee block comments
 -tsc    tee side comments
 -tp     tee pod

Outdenting
 -olq    outdent long quoted strings (default)
 -olc    outdent a long block comment line
 -ola    outdent statement labels
 -okw    outdent control keywords (redo, next, last, goto, return)
 -okwl=s specify alternative keywords for -okw command

Other controls
 -mft=n  maximum fields per table; default n=0 (no limit)
 -x      do not format lines before hash-bang line (i.e., for VMS)
 -asc    allows perltidy to add a ';' when missing (default)
 -dsm    allows perltidy to delete an unnecessary ';'  (default)

Combinations of other parameters
 -gnu     attempt to follow GNU Coding Standards as applied to perl
 -mangle  remove as many newlines as possible (but keep comments and pods)
 -extrude  insert as many newlines as possible

Dump and die, debugging
 -dop    dump options used in this run to standard output and quit
 -ddf    dump default options to standard output and quit
 -dsn    dump all option short names to standard output and quit
 -dln    dump option long names to standard output and quit
 -dpro   dump whatever configuration file is in effect to standard output
 -dtt    dump all token types to standard output and quit

HTML
 -html write an html file (see 'man perl2web' for many options)
       Note: when -html is used, no indentation or formatting are done.
       Hint: try perltidy -html -css=mystyle.css filename.pl
       and edit mystyle.css to change the appearance of filename.html.
       -nnn gives line numbers
       -pre only writes out <pre>..</pre> code section
       -toc places a table of contents to subs at the top (default)
       -pod passes pod text through pod2html (default)
       -frm write html as a frame (3 files)
       -text=s extra extension for table of contents if -frm, default='toc'
       -sext=s extra extension for file content if -frm, default='src'

A prefix of "n" negates short form toggle switches, and a prefix of "no"
negates the long forms.  For example, -nasc means don't add missing
semicolons.

If you are unable to see this entire text, try "perltidy -h | more"
For more detailed information, and additional options, try "man perltidy",
or see https://metacpan.org/pod/distribution/Perl-Tidy/bin/perltidy
EOF

    return;
} ## end sub usage

1;
