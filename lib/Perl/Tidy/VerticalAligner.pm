package Perl::Tidy::VerticalAligner;
use strict;
use warnings;
use Carp;

{ #<<< A non-indenting brace to contain all lexical variables

our $VERSION = '20240511';
use English qw( -no_match_vars );
use Scalar::Util 'refaddr';
use Perl::Tidy::VerticalAligner::Alignment;
use Perl::Tidy::VerticalAligner::Line;

use constant DEVEL_MODE   => 0;
use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };

# The Perl::Tidy::VerticalAligner package collects output lines and
# attempts to line up certain common tokens, such as => and #, which are
# identified by the calling routine.
#
# Usage:
#   - Initiate an object with a call to new().
#   - Write lines one-by-one with calls to valign_input().
#   - Make a final call to flush() to empty the pipeline.
#
# The sub valign_input collects lines into groups.  When a group reaches
# the maximum possible size it is processed for alignment and output.
# The maximum group size is reached whenever there is a change in indentation
# level, a blank line, a block comment, or an external flush call.  The calling
# routine may also force a break in alignment at any time.
#
# If the calling routine needs to interrupt the output and send other text to
# the output, it must first call flush() to empty the output pipeline.  This
# might occur for example if a block of pod text needs to be sent to the output
# between blocks of code.

# It is essential that a final call to flush() be made. Otherwise some
# final lines of text will be lost.

# Index...
# CODE SECTION 1: Preliminary code, global definitions and sub new
#                 sub new
# CODE SECTION 2: Some Basic Utilities
# CODE SECTION 3: Code to accept input and form groups
#                 sub valign_input
# CODE SECTION 4: Code to process comment lines
#                 sub _flush_comment_lines
# CODE SECTION 5: Code to process groups of code lines
#                 sub _flush_group_lines
# CODE SECTION 6: Pad Signed Number Columns
#                 sub pad_signed_number_columns
# CODE SECTION 7: Pad Wide Equals Columns
#                 sub pad_wide_equals_columns
# CODE SECTION 8: Output Step A
#                 sub valign_output_step_A
# CODE SECTION 9: Output Step B
#                 sub valign_output_step_B
# CODE SECTION 10: Output Step C
#                 sub valign_output_step_C
# CODE SECTION 11: Output Step D
#                 sub valign_output_step_D
# CODE SECTION 12: Summary
#                 sub report_anything_unusual

##################################################################
# CODE SECTION 1: Preliminary code, global definitions and sub new
##################################################################

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    my $my_package = __PACKAGE__;
    print {*STDERR} <<EOM;
======================================================================
Error detected in package '$my_package', version $VERSION
Received unexpected AUTOLOAD call for sub '$AUTOLOAD'
Called from package: '$pkg'
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
} ## end sub AUTOLOAD

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub Die {
    my ($msg) = @_;
    Perl::Tidy::Die($msg);
    croak "unexpected return from Perl::Tidy::Die";
}

sub Fault {
    my ($msg) = @_;

    # This routine is called for errors that really should not occur
    # except if there has been a bug introduced by a recent program change.
    # Please add comments at calls to Fault to explain why the call
    # should not occur, and where to look to fix it.
    my ( $package0, $filename0, $line0, $subroutine0 ) = caller(0);
    my ( $package1, $filename1, $line1, $subroutine1 ) = caller(1);
    my ( $package2, $filename2, $line2, $subroutine2 ) = caller(2);
    my $pkg = __PACKAGE__;

    my $input_stream_name = get_input_stream_name();

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

    # We shouldn't get here, but this return is to keep Perl-Critic from
    # complaining.
    return;
} ## end sub Fault

my %valid_LINE_keys;

BEGIN {

    # define valid keys in a line object
    my @q = qw(
      jmax
      rtokens
      rfields
      rfield_lengths
      rpatterns
      indentation
      leading_space_count
      outdent_long_lines
      list_type
      list_seqno
      is_hanging_side_comment
      maximum_line_length
      rvertical_tightness_flags
      is_terminal_ternary
      j_terminal_match
      end_group
      Kend
      ci_level
      level
      level_end
      imax_pair

      ralignments
    );

    @valid_LINE_keys{@q} = (1) x scalar(@q);
} ## end BEGIN

BEGIN {

    # Define the fixed indexes for variables in $self, which is an array
    # reference.  Note the convention of leading and trailing underscores to
    # keep them unique.
    # Do not combine with other BEGIN blocks (c101).
    my $i = 0;
    use constant {
        _file_writer_object_ => $i++,
        _logger_object_      => $i++,
        _diagnostics_object_ => $i++,

        _rOpts_ => $i++,

        _last_level_written_            => $i++,
        _last_side_comment_column_      => $i++,
        _last_side_comment_line_number_ => $i++,
        _last_side_comment_length_      => $i++,
        _last_side_comment_level_       => $i++,
        _outdented_line_count_          => $i++,
        _first_outdented_line_at_       => $i++,
        _last_outdented_line_at_        => $i++,
        _consecutive_block_comments_    => $i++,

        _rgroup_lines_                => $i++,
        _group_level_                 => $i++,
        _group_type_                  => $i++,
        _group_maximum_line_length_   => $i++,
        _zero_count_                  => $i++,
        _last_leading_space_count_    => $i++,
        _comment_leading_space_count_ => $i++,
    };

    # Debug flag. This is a relic from the original program development
    # looking for problems with tab characters.  Caution: this debug flag can
    # produce a lot of output It should be 0 except when debugging small
    # scripts.

    use constant DEBUG_TABS => 0;

    my $debug_warning = sub {
        my $msg = shift;
        print {*STDOUT} "VALIGN_DEBUGGING with key $msg\n";
        return;
    };

    DEBUG_TABS && $debug_warning->('TABS');
} ## end BEGIN

# GLOBAL variables
my (

    %valign_control_hash,
    $valign_control_default,

    $rOpts_indent_columns,
    $rOpts_tabs,
    $rOpts_entab_leading_whitespace,
    $rOpts_fixed_position_side_comment,
    $rOpts_maximum_line_length,
    $rOpts_minimum_space_to_comment,
    $rOpts_valign_code,
    $rOpts_valign_block_comments,
    $rOpts_valign_side_comments,
    $rOpts_valign_signed_numbers,
    $rOpts_valign_signed_numbers_limit,
    $rOpts_valign_wide_equals,

    $require_tabs,

);

sub check_options {

    # This routine is called to check the user-supplied run parameters
    # and to configure the control hashes to them.
    my ($rOpts) = @_;

    # All alignments are done by default
    %valign_control_hash    = ();
    $valign_control_default = 1;

    # If -vil=s is entered without -vxl, assume -vxl='*'
    if (  !$rOpts->{'valign-exclusion-list'}
        && $rOpts->{'valign-inclusion-list'} )
    {
        $rOpts->{'valign-exclusion-list'} = '*';
    }

    # See if the user wants to exclude any alignment types ...
    if ( $rOpts->{'valign-exclusion-list'} ) {

        # The inclusion list is only relevant if there is an exclusion list
        if ( $rOpts->{'valign-inclusion-list'} ) {
            my @vil = split /\s+/, $rOpts->{'valign-inclusion-list'};
            @valign_control_hash{@vil} = (1) x scalar(@vil);
        }

        # Note that the -vxl list is done after -vil, so -vxl has priority
        # in the event of duplicate entries.
        my @vxl = split /\s+/, $rOpts->{'valign-exclusion-list'};
        @valign_control_hash{@vxl} = (0) x scalar(@vxl);

        # Optimization: revert to defaults if no exclusions.
        # This could happen with -vxl='  ' and any -vil list
        if ( !@vxl ) {
            %valign_control_hash = ();
        }

        # '$valign_control_default' applies to types not in the hash:
        # - If a '*' was entered then set it to be that default type
        # - Otherwise, leave it set it to 1
        if ( defined( $valign_control_hash{'*'} ) ) {
            $valign_control_default = $valign_control_hash{'*'};
        }

        # Side comments are controlled separately and must be removed
        # if given in a list.
        if (%valign_control_hash) {
            $valign_control_hash{'#'} = 1;
        }
    }

    # Initialize some global options
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_tabs                     = $rOpts->{'tabs'};
    $rOpts_entab_leading_whitespace = $rOpts->{'entab-leading-whitespace'};
    $require_tabs = ( $rOpts_tabs || $rOpts_entab_leading_whitespace )
      && $rOpts_indent_columns > 0;

    $rOpts_fixed_position_side_comment =
      $rOpts->{'fixed-position-side-comment'};

    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};
    $rOpts_minimum_space_to_comment = $rOpts->{'minimum-space-to-comment'};
    $rOpts_valign_code              = $rOpts->{'valign-code'};
    $rOpts_valign_block_comments    = $rOpts->{'valign-block-comments'};
    $rOpts_valign_side_comments     = $rOpts->{'valign-side-comments'};
    $rOpts_valign_signed_numbers    = $rOpts->{'valign-signed-numbers'};
    $rOpts_valign_signed_numbers_limit =
      $rOpts->{'valign-signed-numbers-limit'};
    $rOpts_valign_wide_equals = $rOpts->{'valign-wide-equals'};

    return;
} ## end sub check_options

sub check_keys {
    my ( $rtest, $rvalid, $msg, $exact_match ) = @_;

    # Check the keys of a hash:
    # $rtest   = ref to hash to test
    # $rvalid  = ref to hash with valid keys

    # $msg = a message to write in case of error
    # $exact_match defines the type of check:
    #     = false: test hash must not have unknown key
    #     = true:  test hash must have exactly same keys as known hash
    my @unknown_keys =
      grep { !exists $rvalid->{$_} } keys %{$rtest};
    my @missing_keys =
      grep { !exists $rtest->{$_} } keys %{$rvalid};
    my $error = @unknown_keys;
    if ($exact_match) { $error ||= @missing_keys }
    if ($error) {
        local $LIST_SEPARATOR = ')(';
        my @expected_keys = sort keys %{$rvalid};
        @unknown_keys = sort @unknown_keys;
        Fault(<<EOM);
------------------------------------------------------------------------
Program error detected checking hash keys
Message is: '$msg'
Expected keys: (@expected_keys)
Unknown key(s): (@unknown_keys)
Missing key(s): (@missing_keys)
------------------------------------------------------------------------
EOM
    }
    return;
} ## end sub check_keys

sub new {

    my ( $class, @arglist ) = @_;
    if ( @arglist % 2 ) { croak "Odd number of items in arg hash list\n" }

    my %defaults = (
        rOpts              => undef,
        file_writer_object => undef,
        logger_object      => undef,
        diagnostics_object => undef,
    );
    my %args = ( %defaults, @arglist );

    # Initialize other caches and buffers
    initialize_step_B_cache();
    initialize_valign_buffer();
    initialize_decode();
    set_logger_object( $args{logger_object} );

    # Initialize all variables in $self.
    # To add an item to $self, first define a new constant index in the BEGIN
    # section.
    my $self = [];

    # objects
    $self->[_file_writer_object_] = $args{file_writer_object};
    $self->[_logger_object_]      = $args{logger_object};
    $self->[_diagnostics_object_] = $args{diagnostics_object};

    # shortcut to user options
    my $rOpts = $args{rOpts};
    $self->[_rOpts_] = $rOpts;

    # Batch of lines being collected
    $self->[_rgroup_lines_]                = [];
    $self->[_group_level_]                 = 0;
    $self->[_group_type_]                  = EMPTY_STRING;
    $self->[_group_maximum_line_length_]   = undef;
    $self->[_zero_count_]                  = 0;
    $self->[_comment_leading_space_count_] = 0;
    $self->[_last_leading_space_count_]    = 0;

    # Memory of what has been processed
    $self->[_last_level_written_]            = -1;
    $self->[_last_side_comment_column_]      = 0;
    $self->[_last_side_comment_line_number_] = 0;
    $self->[_last_side_comment_length_]      = 0;
    $self->[_last_side_comment_level_]       = -1;
    $self->[_outdented_line_count_]          = 0;
    $self->[_first_outdented_line_at_]       = 0;
    $self->[_last_outdented_line_at_]        = 0;
    $self->[_consecutive_block_comments_]    = 0;

    bless $self, $class;
    return $self;
} ## end sub new

#################################
# CODE SECTION 2: Basic Utilities
#################################

sub flush {

    # flush() is the external call to completely empty the pipeline.
    my ($self) = @_;

    # push things out the pipeline...

    # push out any current group lines
    $self->_flush_group_lines()
      if ( @{ $self->[_rgroup_lines_] } );

    # then anything left in the cache of step_B
    $self->_flush_step_B_cache();

    # then anything left in the buffer of step_C
    $self->dump_valign_buffer();

    return;
} ## end sub flush

sub initialize_for_new_group {
    my ($self) = @_;

    $self->[_rgroup_lines_]                = [];
    $self->[_group_type_]                  = EMPTY_STRING;
    $self->[_zero_count_]                  = 0;
    $self->[_comment_leading_space_count_] = 0;
    $self->[_last_leading_space_count_]    = 0;
    $self->[_group_maximum_line_length_]   = undef;

    # Note that the value for _group_level_ is
    # handled separately in sub valign_input
    return;
} ## end sub initialize_for_new_group

sub group_line_count {
    my $self = shift;
    return +@{ $self->[_rgroup_lines_] };
}

# interface to Perl::Tidy::Diagnostics routines
# For debugging; not currently used
sub write_diagnostics {
    my ( $self, $msg ) = @_;
    my $diagnostics_object = $self->[_diagnostics_object_];
    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics($msg);
    }
    return;
} ## end sub write_diagnostics

{    ## begin closure for logger routines
    my $logger_object;

    # Called once per file to initialize the logger object
    sub set_logger_object {
        $logger_object = shift;
        return;
    }

    sub get_input_stream_name {
        my $input_stream_name = EMPTY_STRING;
        if ($logger_object) {
            $input_stream_name = $logger_object->get_input_stream_name();
        }
        return $input_stream_name;
    } ## end sub get_input_stream_name

    sub warning {
        my ($msg) = @_;
        if ($logger_object) {
            $logger_object->warning($msg);
        }
        return;
    } ## end sub warning

    sub write_logfile_entry {
        my ($msg) = @_;
        if ($logger_object) {
            $logger_object->write_logfile_entry($msg);
        }
        return;
    } ## end sub write_logfile_entry
}

sub get_cached_line_count {
    my $self = shift;
    return $self->group_line_count() + ( get_cached_line_type() ? 1 : 0 );
}

sub get_recoverable_spaces {

    # return the number of spaces (+ means shift right, - means shift left)
    # that we would like to shift a group of lines with the same indentation
    # to get them to line up with their opening parens
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_recoverable_spaces() : 0;
} ## end sub get_recoverable_spaces

######################################################
# CODE SECTION 3: Code to accept input and form groups
######################################################

use constant DEBUG_VALIGN      => 0;
use constant SC_LONG_LINE_DIFF => 12;

my %is_closing_token;

BEGIN {
    my @q = qw< } ) ] >;
    @is_closing_token{@q} = (1) x scalar(@q);
}

#--------------------------------------------
# VTFLAGS: Vertical tightness types and flags
#--------------------------------------------
# Vertical tightness is controlled by a 'type' and associated 'flags' for each
# line.  These values are set by sub Formatter::set_vertical_tightness_flags.
# These are defined as follows:

# Vertical Tightness Line Type Codes:
# Type 0, no vertical tightness condition
# Type 1, last token of this line is a non-block opening token
# Type 2, first token of next line is a non-block closing
# Type 3, isolated opening block brace
# type 4, isolated closing block brace

# Opening token flag values are the vertical tightness flags
# 0 do not join with next line
# 1 just one join per line
# 2 any number of joins

# Closing token flag values indicate spacing:
# 0 = no space added before closing token
# 1 = single space added before closing token

sub valign_input {

    #---------------------------------------------------------------------
    # This is the front door of the vertical aligner.  On each call
    # we receive one line of specially marked text for vertical alignment.
    # We compare the line with the current group, and either:
    # - the line joins the current group if alignments match, or
    # - the current group is flushed and a new group is started
    #---------------------------------------------------------------------
    #
    # The key input parameters describing each line are:
    #     $level          = indentation level of this line
    #     $rfields        = ref to array of fields
    #     $rpatterns      = ref to array of patterns, one per field
    #     $rtokens        = ref to array of tokens starting fields 1,2,..
    #     $rfield_lengths = ref to array of field display widths
    #
    # Here is an example of what this package does.  In this example,
    # we are trying to line up both the '=>' and the '#'.
    #
    #         '18' => 'grave',    #   \`
    #         '19' => 'acute',    #   `'
    #         '20' => 'caron',    #   \v
    # <-tabs-><f1-><--field 2 ---><-f3->
    # |            |              |    |
    # |            |              |    |
    # col1        col2         col3 col4
    #
    # The calling routine has already broken the entire line into 3 fields as
    # indicated.  (So the work of identifying promising common tokens has
    # already been done).
    #
    # In this example, there will be 2 tokens being matched: '=>' and '#'.
    # They are the leading parts of fields 2 and 3, but we do need to know
    # what they are so that we can dump a group of lines when these tokens
    # change.
    #
    # The fields contain the actual characters of each field.  The patterns
    # are like the fields, but they contain mainly token types instead
    # of tokens, so they have fewer characters.  They are used to be
    # sure we are matching fields of similar type.
    #
    # In this example, there will be 4 column indexes being adjusted.  The
    # first one is always at zero.  The interior columns are at the start of
    # the matching tokens, and the last one tracks the maximum line length.
    #
    # Each time a new line comes in, it joins the current vertical
    # group if possible.  Otherwise it causes the current group to be flushed
    # and a new group is started.
    #
    # For each new group member, the column locations are increased, as
    # necessary, to make room for the new fields.  When the group is finally
    # output, these column numbers are used to compute the amount of spaces of
    # padding needed for each field.
    #
    # Programming note: the fields are assumed not to have any tab characters.
    # Tabs have been previously removed except for tabs in quoted strings and
    # side comments.  Tabs in these fields can mess up the column counting.
    # The log file warns the user if there are any such tabs.

    my ( $self, $rcall_hash ) = @_;

    # Unpack the call args. This form is significantly faster than getting them
    # one-by-one.
    my (

        $Kend,
        $break_alignment_after,
        $break_alignment_before,
        $ci_level,
        $forget_side_comment,
        $indentation,
        $is_terminal_ternary,
        $level,
        $level_end,
        $list_seqno,
        $maximum_line_length,
        $outdent_long_lines,
        $rline_alignment,
        $rvertical_tightness_flags,

      ) =

      @{$rcall_hash}{
        qw(
          Kend
          break_alignment_after
          break_alignment_before
          ci_level
          forget_side_comment
          indentation
          is_terminal_ternary
          level
          level_end
          list_seqno
          maximum_line_length
          outdent_long_lines
          rline_alignment
          rvertical_tightness_flags
        )
      };

    my ( $rtokens, $rfields, $rpatterns, $rfield_lengths ) =
      @{$rline_alignment};

    # The index '$Kend' is a value which passed along with the line text to sub
    # 'write_code_line' for a convergence check.

    # number of fields is $jmax
    # number of tokens between fields is $jmax-1
    my $jmax = @{$rfields} - 1;

    my $leading_space_count =
      ref($indentation) ? $indentation->get_spaces() : $indentation;

    # set outdented flag to be sure we either align within statements or
    # across statement boundaries, but not both.
    my $is_outdented =
      $self->[_last_leading_space_count_] > $leading_space_count;
    $self->[_last_leading_space_count_] = $leading_space_count;

    # Identify a hanging side comment.  Hanging side comments have an empty
    # initial field.
    my $is_hanging_side_comment =
      ( $jmax == 1 && $rtokens->[0] eq '#' && $rfields->[0] =~ /^\s*$/ );

    # Undo outdented flag for a hanging side comment
    $is_outdented = 0 if $is_hanging_side_comment;

    # Identify a block comment.
    my $is_block_comment = $jmax == 0 && substr( $rfields->[0], 0, 1 ) eq '#';

    # Block comment .. update count
    if ($is_block_comment) {
        $self->[_consecutive_block_comments_]++;
    }

    # Not a block comment ..
    # Forget side comment column if we saw 2 or more block comments,
    # and reset the count
    else {

        if ( $self->[_consecutive_block_comments_] > 1 ) {
            $self->forget_side_comment();
        }
        $self->[_consecutive_block_comments_] = 0;
    }

    # Reset side comment location if we are entering a new block from level 0.
    # This is intended to keep them from drifting too far to the right.
    if ($forget_side_comment) {
        $self->forget_side_comment();
    }

    my $is_balanced_line = $level_end == $level;

    my $group_level               = $self->[_group_level_];
    my $group_maximum_line_length = $self->[_group_maximum_line_length_];

    DEBUG_VALIGN && do {
        my $nlines = $self->group_line_count();
        print {*STDOUT}
"Entering valign_input: lines=$nlines new #fields= $jmax, leading_count=$leading_space_count, level=$level, group_level=$group_level, level_end=$level_end\n";
    };

    # Validate cached line if necessary: If we can produce a container
    # with just 2 lines total by combining an existing cached opening
    # token with the closing token to follow, then we will mark both
    # cached flags as valid.
    my $cached_line_type = get_cached_line_type();
    if ($cached_line_type) {
        my $cached_line_opening_flag = get_cached_line_opening_flag();
        if ($rvertical_tightness_flags) {
            my $cached_seqno = get_cached_seqno();
            if (   $cached_seqno
                && $rvertical_tightness_flags->{_vt_seqno}
                && $rvertical_tightness_flags->{_vt_seqno} == $cached_seqno )
            {

                # Fix for b1187 and b1188: Normally this step is only done
                # if the number of existing lines is 0 or 1.  But to prevent
                # blinking, this range can be controlled by the caller.
                # If zero values are given we fall back on the range 0 to 1.
                my $line_count = $self->group_line_count();
                my $min_lines  = $rvertical_tightness_flags->{_vt_min_lines};
                my $max_lines  = $rvertical_tightness_flags->{_vt_max_lines};
                $min_lines = 0 if ( !$min_lines );
                $max_lines = 1 if ( !$max_lines );
                if (   ( $line_count >= $min_lines )
                    && ( $line_count <= $max_lines ) )
                {
                    $rvertical_tightness_flags->{_vt_valid_flag} ||= 1;
                    set_cached_line_valid(1);
                }
            }
        }

        # do not join an opening block brace (type 3, see VTFLAGS)
        # with an unbalanced line unless requested with a flag value of 2
        if (   $cached_line_type == 3
            && !$self->group_line_count()
            && $cached_line_opening_flag < 2
            && !$is_balanced_line )
        {
            set_cached_line_valid(0);
        }
    }

    # shouldn't happen:
    if ( $level < 0 ) { $level = 0 }

    # do not align code across indentation level changes
    # or changes in the maximum line length
    # or if vertical alignment is turned off
    if (
        $level != $group_level
        || (   $group_maximum_line_length
            && $maximum_line_length != $group_maximum_line_length )
        || $is_outdented
        || ( $is_block_comment && !$rOpts_valign_block_comments )
        || (   !$is_block_comment
            && !$rOpts_valign_side_comments
            && !$rOpts_valign_code )
      )
    {

        $self->_flush_group_lines( $level - $group_level )
          if ( @{ $self->[_rgroup_lines_] } );

        $group_level                         = $level;
        $self->[_group_level_]               = $group_level;
        $self->[_group_maximum_line_length_] = $maximum_line_length;

        # Update leading spaces after the above flush because the leading space
        # count may have been changed if the -icp flag is in effect
        $leading_space_count =
          ref($indentation) ? $indentation->get_spaces() : $indentation;
    }

    # --------------------------------------------------------------------
    # Collect outdentable block COMMENTS
    # --------------------------------------------------------------------
    if ( $self->[_group_type_] eq 'COMMENT' ) {
        if (   $is_block_comment
            && $outdent_long_lines
            && $leading_space_count == $self->[_comment_leading_space_count_] )
        {

            # Note that for a comment group we are not storing a line
            # but rather just the text and its length.
            push @{ $self->[_rgroup_lines_] },
              [ $rfields->[0], $rfield_lengths->[0], $Kend ];
            return;
        }
        else {
            $self->_flush_group_lines()
              if ( @{ $self->[_rgroup_lines_] } );
        }
    }

    my $rgroup_lines = $self->[_rgroup_lines_];
    if ( $break_alignment_before && @{$rgroup_lines} ) {
        $rgroup_lines->[-1]->{'end_group'} = 1;
    }

    # --------------------------------------------------------------------
    # add dummy fields for terminal ternary
    # --------------------------------------------------------------------
    my $j_terminal_match;

    if ( $is_terminal_ternary && @{$rgroup_lines} ) {
        $j_terminal_match = fix_terminal_ternary(
            {
                old_line       => $rgroup_lines->[-1],
                rfields        => $rfields,
                rtokens        => $rtokens,
                rpatterns      => $rpatterns,
                rfield_lengths => $rfield_lengths,
                group_level    => $group_level,
            }
        );
        $jmax = @{$rfields} - 1;
    }

    # --------------------------------------------------------------------
    # add dummy fields for else statement
    # --------------------------------------------------------------------

    # Note the trailing space after 'else' here. If there were no space between
    # the else and the next '{' then we would not be able to do vertical
    # alignment of the '{'.
    if (   $rfields->[0] eq 'else '
        && @{$rgroup_lines}
        && $is_balanced_line )
    {
        $j_terminal_match = fix_terminal_else(
            {
                old_line       => $rgroup_lines->[-1],
                rfields        => $rfields,
                rtokens        => $rtokens,
                rpatterns      => $rpatterns,
                rfield_lengths => $rfield_lengths,
            }
        );
        $jmax = @{$rfields} - 1;
    }

    # --------------------------------------------------------------------
    # Handle simple line of code with no fields to match.
    # --------------------------------------------------------------------
    if ( $jmax <= 0 ) {
        $self->[_zero_count_]++;

        # VSN PATCH for a single number, part 1.
        my $is_numeric =
          $rOpts_valign_signed_numbers && $rpatterns->[0] eq 'n,';

        if (   !$is_numeric
            && @{$rgroup_lines}
            && !get_recoverable_spaces( $rgroup_lines->[0]->{'indentation'} ) )
        {

            # flush the current group if it has some aligned columns..
            # or we haven't seen a comment lately
            if (   $rgroup_lines->[0]->{'jmax'} > 1
                || $self->[_zero_count_] > 3 )
            {
                $self->_flush_group_lines()
                  if ( @{ $self->[_rgroup_lines_] } );

                # Update '$rgroup_lines' - it will become a ref to empty array.
                # This allows avoiding a call to get_group_line_count below.
                $rgroup_lines = $self->[_rgroup_lines_];
            }
        }

        # start new COMMENT group if this comment may be outdented
        if (   $is_block_comment
            && $outdent_long_lines
            && !@{$rgroup_lines} )
        {
            $self->[_group_type_]                  = 'COMMENT';
            $self->[_comment_leading_space_count_] = $leading_space_count;
            $self->[_group_maximum_line_length_]   = $maximum_line_length;
            push @{$rgroup_lines},
              [ $rfields->[0], $rfield_lengths->[0], $Kend ];
            return;
        }

        # just write this line directly if no current group, no side comment,
        # and no space recovery is needed,
        # and not numeric - VSN PATCH for a single number, part 4.
        if (   !@{$rgroup_lines}
            && !$is_numeric
            && !get_recoverable_spaces($indentation) )
        {

            $self->valign_output_step_B(
                {
                    leading_space_count       => $leading_space_count,
                    line                      => $rfields->[0],
                    line_length               => $rfield_lengths->[0],
                    side_comment_length       => 0,
                    outdent_long_lines        => $outdent_long_lines,
                    rvertical_tightness_flags => $rvertical_tightness_flags,
                    level                     => $level,
                    level_end                 => $level_end,
                    Kend                      => $Kend,
                    maximum_line_length       => $maximum_line_length,
                }
            );
            return;
        }
    }
    else {
        $self->[_zero_count_] = 0;
    }

    # --------------------------------------------------------------------
    # It simplifies things to create a zero length side comment
    # if none exists.
    # --------------------------------------------------------------------
    if ( ( $jmax == 0 ) || ( $rtokens->[ $jmax - 1 ] ne '#' ) ) {
        $jmax += 1;
        $rtokens->[ $jmax - 1 ]  = '#';
        $rfields->[$jmax]        = EMPTY_STRING;
        $rfield_lengths->[$jmax] = 0;
        $rpatterns->[$jmax]      = '#';
    }

    # --------------------------------------------------------------------
    # create an object to hold this line
    # --------------------------------------------------------------------

    # The hash keys below must match the list of keys in %valid_LINE_keys.
    # Values in this hash are accessed directly, except for 'ralignments',
    # rather than with get/set calls for efficiency.
    my $new_line = Perl::Tidy::VerticalAligner::Line->new(
        {
            jmax                      => $jmax,
            rtokens                   => $rtokens,
            rfields                   => $rfields,
            rpatterns                 => $rpatterns,
            rfield_lengths            => $rfield_lengths,
            indentation               => $indentation,
            leading_space_count       => $leading_space_count,
            outdent_long_lines        => $outdent_long_lines,
            list_seqno                => $list_seqno,
            list_type                 => EMPTY_STRING,
            is_hanging_side_comment   => $is_hanging_side_comment,
            rvertical_tightness_flags => $rvertical_tightness_flags,
            is_terminal_ternary       => $is_terminal_ternary,
            j_terminal_match          => $j_terminal_match,
            end_group                 => $break_alignment_after,
            Kend                      => $Kend,
            ci_level                  => $ci_level,
            level                     => $level,
            level_end                 => $level_end,
            imax_pair                 => -1,
            maximum_line_length       => $maximum_line_length,

            ralignments => [],
        }
    );

    DEVEL_MODE
      && check_keys( $new_line, \%valid_LINE_keys,
        "Checking line keys at line definition", 1 );

    # --------------------------------------------------------------------
    # Decide if this is a simple list of items.
    # We use this to be less restrictive in deciding what to align.
    # --------------------------------------------------------------------
    decide_if_list($new_line) if ($list_seqno);

    # --------------------------------------------------------------------
    # Append this line to the current group (or start new group)
    # --------------------------------------------------------------------

    push @{ $self->[_rgroup_lines_] }, $new_line;
    $self->[_group_maximum_line_length_] = $maximum_line_length;

    # output this group if it ends in a terminal else or ternary line
    if ( defined($j_terminal_match) ) {
        $self->_flush_group_lines()
          if ( @{ $self->[_rgroup_lines_] } );
    }

    # Force break after jump to lower level
    elsif ($level_end < $level
        || $is_closing_token{ substr( $rfields->[0], 0, 1 ) } )
    {
        $self->_flush_group_lines(-1)
          if ( @{ $self->[_rgroup_lines_] } );
    }

    else {
        ##ok: no output needed
    }

    # --------------------------------------------------------------------
    # Some old debugging stuff
    # --------------------------------------------------------------------
    DEBUG_VALIGN && do {
        print {*STDOUT} "exiting valign_input fields:";
        dump_array( @{$rfields} );
        print {*STDOUT} "exiting valign_input tokens:";
        dump_array( @{$rtokens} );
        print {*STDOUT} "exiting valign_input patterns:";
        dump_array( @{$rpatterns} );
    };

    return;
} ## end sub valign_input

sub join_hanging_comment {

    # Add dummy fields to a hanging side comment to make it look
    # like the first line in its potential group.  This simplifies
    # the coding.
    my ( $new_line, $old_line ) = @_;

    my $jmax = $new_line->{'jmax'};

    # must be 2 fields
    return 0 unless ( $jmax == 1 );
    my $rtokens = $new_line->{'rtokens'};

    # the second field must be a comment
    return 0 unless ( $rtokens->[0] eq '#' );
    my $rfields = $new_line->{'rfields'};

    # the first field must be empty
    return 0 if ( $rfields->[0] !~ /^\s*$/ );

    # the current line must have fewer fields
    my $maximum_field_index = $old_line->{'jmax'};
    return 0
      if ( $maximum_field_index <= $jmax );

    # looks ok..
    my $rpatterns      = $new_line->{'rpatterns'};
    my $rfield_lengths = $new_line->{'rfield_lengths'};

    $new_line->{'is_hanging_side_comment'} = 1;

    $jmax                     = $maximum_field_index;
    $new_line->{'jmax'}       = $jmax;
    $rfields->[$jmax]         = $rfields->[1];
    $rfield_lengths->[$jmax]  = $rfield_lengths->[1];
    $rtokens->[ $jmax - 1 ]   = $rtokens->[0];
    $rpatterns->[ $jmax - 1 ] = $rpatterns->[0];

    foreach my $j ( 1 .. $jmax - 1 ) {
        $rfields->[$j]         = EMPTY_STRING;
        $rfield_lengths->[$j]  = 0;
        $rtokens->[ $j - 1 ]   = EMPTY_STRING;
        $rpatterns->[ $j - 1 ] = EMPTY_STRING;
    }
    return 1;
} ## end sub join_hanging_comment

{    ## closure for sub decide_if_list

    my %is_comma_token;

    BEGIN {

        my @q = qw( => );
        push @q, ',';
        @is_comma_token{@q} = (1) x scalar(@q);
    } ## end BEGIN

    sub decide_if_list {

        my $line = shift;

        # A list will be taken to be a line with a forced break in which all
        # of the field separators are commas or comma-arrows (except for the
        # trailing #)

        my $rtokens    = $line->{'rtokens'};
        my $test_token = $rtokens->[0];
        my ( $raw_tok, $lev, $tag, $tok_count ) =
          decode_alignment_token($test_token);
        if ( $is_comma_token{$raw_tok} ) {
            my $list_type = $test_token;
            my $jmax      = $line->{'jmax'};

            foreach ( 1 .. $jmax - 2 ) {
                ( $raw_tok, $lev, $tag, $tok_count ) =
                  decode_alignment_token( $rtokens->[$_] );
                if ( !$is_comma_token{$raw_tok} ) {
                    $list_type = EMPTY_STRING;
                    last;
                }
            }
            $line->{'list_type'} = $list_type;
        }
        return;
    } ## end sub decide_if_list
}

sub fix_terminal_ternary {

    # Add empty fields as necessary to align a ternary term
    # like this:
    #
    #  my $leapyear =
    #      $year % 4   ? 0
    #    : $year % 100 ? 1
    #    : $year % 400 ? 0
    #    :               1;
    #
    # returns the index of the terminal question token, if any

    my ($rcall_hash) = @_;

    my $old_line       = $rcall_hash->{old_line};
    my $rfields        = $rcall_hash->{rfields};
    my $rtokens        = $rcall_hash->{rtokens};
    my $rpatterns      = $rcall_hash->{rpatterns};
    my $rfield_lengths = $rcall_hash->{rfield_lengths};
    my $group_level    = $rcall_hash->{group_level};

    return if ( !$old_line );
    use constant EXPLAIN_TERNARY => 0;

    if (%valign_control_hash) {
        my $align_ok = $valign_control_hash{'?'};
        $align_ok = $valign_control_default unless defined($align_ok);
        return if ( !$align_ok );
    }

    my $jmax        = @{$rfields} - 1;
    my $rfields_old = $old_line->{'rfields'};

    my $rpatterns_old       = $old_line->{'rpatterns'};
    my $rtokens_old         = $old_line->{'rtokens'};
    my $maximum_field_index = $old_line->{'jmax'};

    # look for the question mark after the :
    my ($jquestion);
    my $depth_question;
    my $pad        = EMPTY_STRING;
    my $pad_length = 0;
    foreach my $j ( 0 .. $maximum_field_index - 1 ) {
        my $tok = $rtokens_old->[$j];
        my ( $raw_tok, $lev, $tag, $tok_count ) = decode_alignment_token($tok);
        if ( $raw_tok eq '?' ) {
            $depth_question = $lev;

            # depth must be correct
            next if ( $depth_question ne $group_level );

            $jquestion = $j;
            if ( $rfields_old->[ $j + 1 ] =~ /^(\?\s*)/ ) {
                $pad_length = length($1);
                $pad        = SPACE x $pad_length;
            }
            else {
                return;    # shouldn't happen
            }
            last;
        }
    }
    return if ( !defined($jquestion) );    # shouldn't happen

    # Now splice the tokens and patterns of the previous line
    # into the else line to insure a match.  Add empty fields
    # as necessary.
    my $jadd = $jquestion;

    # Work on copies of the actual arrays in case we have
    # to return due to an error
    my @fields        = @{$rfields};
    my @patterns      = @{$rpatterns};
    my @tokens        = @{$rtokens};
    my @field_lengths = @{$rfield_lengths};

    EXPLAIN_TERNARY && do {
        local $LIST_SEPARATOR = '><';
        print {*STDOUT} "CURRENT FIELDS=<@{$rfields_old}>\n";
        print {*STDOUT} "CURRENT TOKENS=<@{$rtokens_old}>\n";
        print {*STDOUT} "CURRENT PATTERNS=<@{$rpatterns_old}>\n";
        print {*STDOUT} "UNMODIFIED FIELDS=<@{$rfields}>\n";
        print {*STDOUT} "UNMODIFIED TOKENS=<@{$rtokens}>\n";
        print {*STDOUT} "UNMODIFIED PATTERNS=<@{$rpatterns}>\n";
    };

    # handle cases of leading colon on this line
    if ( $fields[0] =~ /^(:\s*)(.*)$/ ) {

        my ( $colon, $therest ) = ( $1, $2 );

        # Handle sub-case of first field with leading colon plus additional code
        # This is the usual situation as at the '1' below:
        #  ...
        #  : $year % 400 ? 0
        #  :               1;
        if ($therest) {

            # Split the first field after the leading colon and insert padding.
            # Note that this padding will remain even if the terminal value goes
            # out on a separate line.  This does not seem to look to bad, so no
            # mechanism has been included to undo it.
            my $field1        = shift @fields;
            my $field_length1 = shift @field_lengths;
            my $len_colon     = length($colon);
            unshift @fields, ( $colon, $pad . $therest );
            unshift @field_lengths,
              ( $len_colon, $pad_length + $field_length1 - $len_colon );

            # change the leading pattern from : to ?
            return if ( $patterns[0] !~ s/^\:/?/ );

            # install leading tokens and patterns of existing line
            unshift( @tokens,   @{$rtokens_old}[ 0 .. $jquestion ] );
            unshift( @patterns, @{$rpatterns_old}[ 0 .. $jquestion ] );

            # insert appropriate number of empty fields
            splice( @fields,        1, 0, (EMPTY_STRING) x $jadd ) if $jadd;
            splice( @field_lengths, 1, 0, (0) x $jadd )            if $jadd;
        }

        # handle sub-case of first field just equal to leading colon.
        # This can happen for example in the example below where
        # the leading '(' would create a new alignment token
        # : ( $name =~ /[]}]$/ ) ? ( $mname = $name )
        # :                        ( $mname = $name . '->' );
        else {

            return if ( $jmax <= 0 || $tokens[0] eq '#' );    # shouldn't happen

            # prepend a leading ? onto the second pattern
            $patterns[1] = "?b" . $patterns[1];

            # pad the second field
            $fields[1]        = $pad . $fields[1];
            $field_lengths[1] = $pad_length + $field_lengths[1];

            # install leading tokens and patterns of existing line, replacing
            # leading token and inserting appropriate number of empty fields
            splice( @tokens,   0, 1, @{$rtokens_old}[ 0 .. $jquestion ] );
            splice( @patterns, 1, 0, @{$rpatterns_old}[ 1 .. $jquestion ] );
            splice( @fields,        1, 0, (EMPTY_STRING) x $jadd ) if $jadd;
            splice( @field_lengths, 1, 0, (0) x $jadd )            if $jadd;
        }
    }

    # Handle case of no leading colon on this line.  This will
    # be the case when -wba=':' is used.  For example,
    #  $year % 400 ? 0 :
    #                1;
    else {

        # install leading tokens and patterns of existing line
        $patterns[0] = '?' . 'b' . $patterns[0];
        unshift( @tokens,   @{$rtokens_old}[ 0 .. $jquestion ] );
        unshift( @patterns, @{$rpatterns_old}[ 0 .. $jquestion ] );

        # insert appropriate number of empty fields
        $jadd             = $jquestion + 1;
        $fields[0]        = $pad . $fields[0];
        $field_lengths[0] = $pad_length + $field_lengths[0];
        splice( @fields,        0, 0, (EMPTY_STRING) x $jadd ) if $jadd;
        splice( @field_lengths, 0, 0, (0) x $jadd )            if $jadd;
    }

    EXPLAIN_TERNARY && do {
        local $LIST_SEPARATOR = '><';
        print {*STDOUT} "MODIFIED TOKENS=<@tokens>\n";
        print {*STDOUT} "MODIFIED PATTERNS=<@patterns>\n";
        print {*STDOUT} "MODIFIED FIELDS=<@fields>\n";
    };

    # all ok .. update the arrays
    @{$rfields}        = @fields;
    @{$rtokens}        = @tokens;
    @{$rpatterns}      = @patterns;
    @{$rfield_lengths} = @field_lengths;

    # force a flush after this line
    return $jquestion;
} ## end sub fix_terminal_ternary

sub fix_terminal_else {

    # Add empty fields as necessary to align a balanced terminal
    # else block to a previous if/elsif/unless block,
    # like this:
    #
    #  if   ( 1 || $x ) { print "ok 13\n"; }
    #  else             { print "not ok 13\n"; }
    #
    # returns a positive value if the else block should be indented
    #
    my ($rcall_hash) = @_;

    my $old_line       = $rcall_hash->{old_line};
    my $rfields        = $rcall_hash->{rfields};
    my $rtokens        = $rcall_hash->{rtokens};
    my $rpatterns      = $rcall_hash->{rpatterns};
    my $rfield_lengths = $rcall_hash->{rfield_lengths};

    return if ( !$old_line );
    my $jmax = @{$rfields} - 1;
    return if ( $jmax <= 0 );

    if (%valign_control_hash) {
        my $align_ok = $valign_control_hash{'{'};
        $align_ok = $valign_control_default unless defined($align_ok);
        return if ( !$align_ok );
    }

    # check for balanced else block following if/elsif/unless
    my $rfields_old = $old_line->{'rfields'};

    # TBD: add handling for 'case'
    return if ( $rfields_old->[0] !~ /^(?:if|elsif|unless)\s*$/ );

    # look for the opening brace after the else, and extract the depth
    my $tok_brace = $rtokens->[0];
    my $depth_brace;
    if ( $tok_brace =~ /^\{(\d+)/ ) { $depth_brace = $1; }

    # probably:  "else # side_comment"
    else { return }

    my $rpatterns_old       = $old_line->{'rpatterns'};
    my $rtokens_old         = $old_line->{'rtokens'};
    my $maximum_field_index = $old_line->{'jmax'};

    # be sure the previous if/elsif is followed by an opening paren
    my $jparen    = 0;
    my $tok_paren = '(' . $depth_brace;
    my $tok_test  = $rtokens_old->[$jparen];
    return if ( $tok_test ne $tok_paren );    # shouldn't happen

    # Now find the opening block brace
    my ($jbrace);
    foreach my $j ( 1 .. $maximum_field_index - 1 ) {
        my $tok = $rtokens_old->[$j];
        if ( $tok eq $tok_brace ) {
            $jbrace = $j;
            last;
        }
    }
    return if ( !defined($jbrace) );          # shouldn't happen

    # Now splice the tokens and patterns of the previous line
    # into the else line to insure a match.  Add empty fields
    # as necessary.
    my $jadd = $jbrace - $jparen;
    splice( @{$rtokens},   0, 0, @{$rtokens_old}[ $jparen .. $jbrace - 1 ] );
    splice( @{$rpatterns}, 1, 0, @{$rpatterns_old}[ $jparen + 1 .. $jbrace ] );
    splice( @{$rfields},        1, 0, (EMPTY_STRING) x $jadd );
    splice( @{$rfield_lengths}, 1, 0, (0) x $jadd );

    # force a flush after this line if it does not follow a case
    if   ( $rfields_old->[0] =~ /^case\s*$/ ) { return }
    else                                      { return $jbrace }
} ## end sub fix_terminal_else

my %is_closing_block_type;

BEGIN {
    my @q = qw< } ] >;
    @is_closing_block_type{@q} = (1) x scalar(@q);
}

# This is a flag for testing alignment by sub sweep_left_to_right only.
# This test can help find problems with the alignment logic.
# This flag should normally be zero.
use constant TEST_SWEEP_ONLY => 0;

use constant EXPLAIN_CHECK_MATCH => 0;

sub check_match {

    # See if the current line matches the current vertical alignment group.

    my ( $self, $new_line, $base_line, $prev_line, $group_line_count ) = @_;

    # Given:
    #  $new_line  = the line being considered for group inclusion
    #  $base_line = the first line of the current group
    #  $prev_line = the line just before $new_line
    #  $group_line_count = number of lines in the current group

    # returns a flag and a value as follows:
    #    return (0, $imax_align)   if the line does not match
    #    return (1, $imax_align)   if the line matches but does not fit
    #    return (2, $imax_align)   if the line matches and fits

    use constant NO_MATCH      => 0;
    use constant MATCH_NO_FIT  => 1;
    use constant MATCH_AND_FIT => 2;

    my $return_value;

    # Returns '$imax_align' which is the index of the maximum matching token.
    # It will be used in the subsequent left-to-right sweep to align as many
    # tokens as possible for lines which partially match.
    my $imax_align = -1;

    # variable $GoToMsg explains reason for no match, for debugging
    my $GoToMsg = EMPTY_STRING;

    my $jmax                = $new_line->{'jmax'};
    my $maximum_field_index = $base_line->{'jmax'};

    my $jlimit = $jmax - 2;
    if ( $jmax > $maximum_field_index ) {
        $jlimit = $maximum_field_index - 2;
    }

    if ( $new_line->{'is_hanging_side_comment'} ) {

        # HSC's can join the group if they fit
    }

    # Everything else
    else {

        # A group with hanging side comments ends with the first non hanging
        # side comment.
        if ( $base_line->{'is_hanging_side_comment'} ) {
            $GoToMsg      = "end of hanging side comments";
            $return_value = NO_MATCH;
        }
        else {

            # The number of tokens that this line shares with the previous
            # line has been stored with the previous line.  This value was
            # calculated and stored by sub 'match_line_pair'.
            $imax_align = $prev_line->{'imax_pair'};

            # Only the following ci sequences are accepted (issue c225):
            #   0 0 0 ...  OK
            #   0 1 1 ...  OK but marginal*
            #   1 1 1 ...  OK
            # This check is rarely activated, but for example we want
            # to avoid something like this 'tail wag dog' situation:
            #  $tag        =~ s/\b([a-z]+)/\L\u$1/gio;
            #  $tag        =~ s/\b([b-df-hj-np-tv-z]+)\b/\U$1/gio
            #      if $tag =~ /-/;
            # *Note: we could set a flag for the 0 1 marginal case and
            # use it to prevent alignment of selected token types.
            my $ci_prev = $prev_line->{'ci_level'};
            my $ci_new  = $new_line->{'ci_level'};
            if (   $ci_prev != $ci_new
                && $imax_align >= 0
                && ( $ci_new == 0 || $group_line_count > 1 ) )
            {
                $imax_align = -1;
                $GoToMsg =
"Rejected ci: ci_prev=$ci_prev ci_new=$ci_new num=$group_line_count\n";
                $return_value = NO_MATCH;
            }
            elsif ( $imax_align != $jlimit ) {
                $GoToMsg = "Not all tokens match: $imax_align != $jlimit\n";
                $return_value = NO_MATCH;
            }
            else {
                ##ok: continue
            }
        }
    }

    if ( !defined($return_value) ) {

        # The tokens match, but the lines must have identical number of
        # tokens to join the group.
        if ( $maximum_field_index != $jmax ) {
            $GoToMsg      = "token count differs";
            $return_value = NO_MATCH;
        }

        # The tokens match. Now See if there is space for this line in the
        # current group.
        elsif ( $self->check_fit( $new_line, $base_line ) && !TEST_SWEEP_ONLY )
        {

            $GoToMsg = "match and fit, imax_align=$imax_align, jmax=$jmax\n";
            $return_value = MATCH_AND_FIT;
            $imax_align   = $jlimit;
        }
        else {
            $GoToMsg = "match but no fit, imax_align=$imax_align, jmax=$jmax\n";
            $return_value = MATCH_NO_FIT;
            $imax_align   = $jlimit;
        }
    }

    EXPLAIN_CHECK_MATCH
      && print
"returning $return_value because $GoToMsg, max match index =i $imax_align, jmax=$jmax\n";

    return ( $return_value, $imax_align );
} ## end sub check_match

sub check_fit {

    my ( $self, $new_line, $old_line ) = @_;

    # The new line has alignments identical to the current group. Now we have
    # to fit the new line into the group without causing a field to exceed the
    # line length limit.
    #   return true if successful
    #   return false if not successful

    my $jmax                = $new_line->{'jmax'};
    my $leading_space_count = $new_line->{'leading_space_count'};
    my $rfield_lengths      = $new_line->{'rfield_lengths'};
    my $padding_available   = $old_line->get_available_space_on_right();
    my $jmax_old            = $old_line->{'jmax'};

    # Safety check ... only lines with equal array sizes should arrive here
    # from sub check_match.  So if this error occurs, look at recent changes in
    # sub check_match.  It is only supposed to check the fit of lines with
    # identical numbers of alignment tokens.
    if ( $jmax_old ne $jmax ) {

        warning(<<EOM);
Program bug detected in Perl::Tidy::VerticalAligner sub check_fit
unexpected difference in array lengths: $jmax != $jmax_old
EOM
        return;
    }

    # Save current columns in case this line does not fit.
    my @alignments = @{ $old_line->{'ralignments'} };
    foreach my $alignment (@alignments) {
        $alignment->save_column();
    }

    # Loop over all alignments ...
    for my $j ( 0 .. $jmax ) {

        my $pad = $rfield_lengths->[$j] - $old_line->current_field_width($j);

        if ( $j == 0 ) {
            $pad += $leading_space_count;
        }

        # Keep going if this field does not need any space.
        next if ( $pad < 0 );

        # Revert to the starting state if does not fit
        if ( $pad > $padding_available ) {

            #----------------------------------------------
            # Line does not fit -- revert to starting state
            #----------------------------------------------
            foreach my $alignment (@alignments) {
                $alignment->restore_column();
            }
            return;
        }

        # make room for this field
        $old_line->increase_field_width( $j, $pad );
        $padding_available -= $pad;
    }

    #-------------------------------------
    # The line fits, the match is accepted
    #-------------------------------------
    return 1;

} ## end sub check_fit

sub install_new_alignments {

    my ($new_line) = @_;

    my $jmax           = $new_line->{'jmax'};
    my $rfield_lengths = $new_line->{'rfield_lengths'};
    my $col            = $new_line->{'leading_space_count'};

    my @alignments;
    for my $j ( 0 .. $jmax ) {
        $col += $rfield_lengths->[$j];

        # create initial alignments for the new group
        my $alignment =
          Perl::Tidy::VerticalAligner::Alignment->new( { column => $col } );
        push @alignments, $alignment;
    }
    $new_line->{'ralignments'} = \@alignments;
    return;
} ## end sub install_new_alignments

sub copy_old_alignments {
    my ( $new_line, $old_line ) = @_;
    my @new_alignments = @{ $old_line->{'ralignments'} };
    $new_line->{'ralignments'} = \@new_alignments;
    return;
} ## end sub copy_old_alignments

sub dump_array {

    # debug routine to dump array contents
    local $LIST_SEPARATOR = ')(';
    print {*STDOUT} "(@_)\n";
    return;
} ## end sub dump_array

sub level_change {

    # compute decrease in level when we remove $diff spaces from the
    # leading spaces
    my ( $self, $leading_space_count, $diff, $level ) = @_;

    if ($rOpts_indent_columns) {
        my $olev =
          int( ( $leading_space_count + $diff ) / $rOpts_indent_columns );
        my $nlev = int( $leading_space_count / $rOpts_indent_columns );
        $level -= ( $olev - $nlev );
        if ( $level < 0 ) { $level = 0 }
    }
    return $level;
} ## end sub level_change

###############################################
# CODE SECTION 4: Code to process comment lines
###############################################

sub _flush_comment_lines {

    # Output a group consisting of COMMENT lines

    my ($self) = @_;
    my $rgroup_lines = $self->[_rgroup_lines_];
    return if ( !@{$rgroup_lines} );
    my $group_level               = $self->[_group_level_];
    my $group_maximum_line_length = $self->[_group_maximum_line_length_];
    my $leading_space_count       = $self->[_comment_leading_space_count_];

    # look for excessively long lines
    my $max_excess = 0;
    foreach my $item ( @{$rgroup_lines} ) {
        my ( $str, $str_len ) = @{$item};
        my $excess =
          $str_len + $leading_space_count - $group_maximum_line_length;
        if ( $excess > $max_excess ) {
            $max_excess = $excess;
        }
    }

    # zero leading space count if any lines are too long
    if ( $max_excess > 0 ) {
        $leading_space_count -= $max_excess;
        if ( $leading_space_count < 0 ) { $leading_space_count = 0 }
        my $file_writer_object = $self->[_file_writer_object_];
        my $last_outdented_line_at =
          $file_writer_object->get_output_line_number();
        my $nlines = @{$rgroup_lines};
        $self->[_last_outdented_line_at_] =
          $last_outdented_line_at + $nlines - 1;
        my $outdented_line_count = $self->[_outdented_line_count_];
        if ( !$outdented_line_count ) {
            $self->[_first_outdented_line_at_] = $last_outdented_line_at;
        }
        $outdented_line_count += $nlines;
        $self->[_outdented_line_count_] = $outdented_line_count;
    }

    # write the lines
    my $outdent_long_lines = 0;

    foreach my $item ( @{$rgroup_lines} ) {
        my ( $str, $str_len, $Kend ) = @{$item};
        $self->valign_output_step_B(
            {
                leading_space_count       => $leading_space_count,
                line                      => $str,
                line_length               => $str_len,
                side_comment_length       => 0,
                outdent_long_lines        => $outdent_long_lines,
                rvertical_tightness_flags => undef,
                level                     => $group_level,
                level_end                 => $group_level,
                Kend                      => $Kend,
                maximum_line_length       => $group_maximum_line_length,
            }
        );
    }

    $self->initialize_for_new_group();
    return;
} ## end sub _flush_comment_lines

######################################################
# CODE SECTION 5: Code to process groups of code lines
######################################################

sub _flush_group_lines {

    # This is the vertical aligner internal flush, which leaves the cache
    # intact
    my ( $self, $level_jump ) = @_;

    # $level_jump = $next_level-$group_level, if known
    #             = undef if not known
    # Note: only the sign of the jump is needed

    my $rgroup_lines = $self->[_rgroup_lines_];
    return if ( !@{$rgroup_lines} );
    my $group_type  = $self->[_group_type_];
    my $group_level = $self->[_group_level_];

    # Debug
    0 && do {
        my ( $a, $b, $c ) = caller();
        my $nlines = @{$rgroup_lines};
        print {*STDOUT}
"APPEND0: _flush_group_lines called from $a $b $c lines=$nlines, type=$group_type \n";
    };

    #-------------------------------------------
    # Section 1: Handle a group of COMMENT lines
    #-------------------------------------------
    if ( $group_type eq 'COMMENT' ) {
        $self->_flush_comment_lines();
        return;
    }

    #------------------------------------------------------------------------
    # Section 2: Handle line(s) of CODE.  Most of the actual work of vertical
    # aligning happens here in the following steps:
    #------------------------------------------------------------------------

    # STEP 1: Remove most unmatched tokens. They block good alignments.
    my ( $max_lev_diff, $saw_side_comment ) =
      delete_unmatched_tokens( $rgroup_lines, $group_level );

    # STEP 2: Sweep top to bottom, forming subgroups of lines with exactly
    # matching common alignments.  The indexes of these subgroups are in the
    # return variable.
    my $rgroups = $self->sweep_top_down( $rgroup_lines, $group_level );

    # STEP 3: Sweep left to right through the lines, looking for leading
    # alignment tokens shared by groups.
    sweep_left_to_right( $rgroup_lines, $rgroups, $group_level )
      if ( @{$rgroups} > 1 );

    # STEP 4: Move side comments to a common column if possible.
    if ($saw_side_comment) {
        $self->align_side_comments( $rgroup_lines, $rgroups );
    }

    # STEP 5: For the -lp option, increase the indentation of lists
    # to the desired amount, but do not exceed the line length limit.

    # We are allowed to shift a group of lines to the right if:
    #  (1) its level is greater than the level of the previous group, and
    #  (2) its level is greater than the level of the next line to be written.

    my $extra_indent_ok;
    if ( $group_level > $self->[_last_level_written_] ) {

        # Use the level jump to next line to come, if given
        if ( defined($level_jump) ) {
            $extra_indent_ok = $level_jump < 0;
        }

        # Otherwise, assume the next line has the level of the end of last line.
        # This fixes case c008.
        else {
            my $level_end = $rgroup_lines->[-1]->{'level_end'};
            $extra_indent_ok = $group_level > $level_end;
        }
    }

    my $extra_leading_spaces =
      $extra_indent_ok
      ? get_extra_leading_spaces( $rgroup_lines, $rgroups )
      : 0;

    # STEP 6: add sign padding to columns numbers if needed
    pad_signed_number_columns($rgroup_lines)
      if ($rOpts_valign_signed_numbers);

    # STEP 7: pad wide equals
    pad_wide_equals_columns($rgroup_lines)
      if ($rOpts_valign_wide_equals);

    # STEP 8: Output the lines.
    # All lines in this group have the same leading spacing and maximum line
    # length
    my $group_leader_length       = $rgroup_lines->[0]->{'leading_space_count'};
    my $group_maximum_line_length = $rgroup_lines->[0]->{'maximum_line_length'};

    foreach my $line ( @{$rgroup_lines} ) {
        $self->valign_output_step_A(
            {
                line                 => $line,
                min_ci_gap           => 0,
                do_not_align         => 0,
                group_leader_length  => $group_leader_length,
                extra_leading_spaces => $extra_leading_spaces,
                level                => $group_level,
                maximum_line_length  => $group_maximum_line_length,
            }
        );
    }

    # Let the formatter know that this object has been processed and any
    # recoverable spaces have been handled.  This is needed for setting the
    # closing paren location in -lp mode.
    my $object = $rgroup_lines->[0]->{'indentation'};
    if ( ref($object) ) { $object->set_recoverable_spaces(0) }

    $self->initialize_for_new_group();
    return;
} ## end sub _flush_group_lines

{    ## closure for sub sweep_top_down

    my $rall_lines;         # all of the lines
    my $grp_level;          # level of all lines
    my $rgroups;            # describes the partition of lines we will make here
    my $group_line_count;   # number of lines in current partition

    BEGIN { $rgroups = [] }

    sub initialize_for_new_rgroup {
        $group_line_count = 0;
        return;
    }

    sub add_to_rgroup {

        my ($jend) = @_;
        my $rline = $rall_lines->[$jend];

        my $jbeg = $jend;
        if ( $group_line_count == 0 ) {
            install_new_alignments($rline);
        }
        else {
            my $rvals = pop @{$rgroups};
            $jbeg = $rvals->[0];
            copy_old_alignments( $rline, $rall_lines->[$jbeg] );
        }
        push @{$rgroups}, [ $jbeg, $jend, undef ];
        $group_line_count++;
        return;
    } ## end sub add_to_rgroup

    sub get_rgroup_jrange {

        return if ( !@{$rgroups} );
        return if ( $group_line_count <= 0 );
        my ( $jbeg, $jend ) = @{ $rgroups->[-1] };
        return ( $jbeg, $jend );
    } ## end sub get_rgroup_jrange

    sub end_rgroup {

        my ($imax_align) = @_;
        return if ( !@{$rgroups} );
        return if ( $group_line_count <= 0 );

        my ( $jbeg, $jend ) = @{ pop @{$rgroups} };
        push @{$rgroups}, [ $jbeg, $jend, $imax_align ];

        # Undo some alignments of poor two-line combinations.
        # We had to wait until now to know the line count.
        if ( $jend - $jbeg == 1 ) {
            my $line_0 = $rall_lines->[$jbeg];
            my $line_1 = $rall_lines->[$jend];

            my $imax_pair = $line_1->{'imax_pair'};
            if ( $imax_pair > $imax_align ) { $imax_align = $imax_pair }

            ## flag for possible future use:
            ## my $is_isolated_pair = $imax_pair < 0
            ##  && ( $jbeg == 0
            ##    || $rall_lines->[ $jbeg - 1 ]->{'imax_pair'} < 0 );

            my $imax_prev =
              $jbeg > 0 ? $rall_lines->[ $jbeg - 1 ]->{'imax_pair'} : -1;

            my ( $is_marginal, $imax_align_fix ) =
              is_marginal_match( $line_0, $line_1, $grp_level, $imax_align,
                $imax_prev );
            if ($is_marginal) {
                combine_fields( $line_0, $line_1, $imax_align_fix );
            }
        }

        initialize_for_new_rgroup();
        return;
    } ## end sub end_rgroup

    sub block_penultimate_match {

        # emergency reset to prevent sweep_left_to_right from trying to match a
        # failed terminal else match
        return if ( @{$rgroups} <= 1 );
        $rgroups->[-2]->[2] = -1;
        return;
    } ## end sub block_penultimate_match

    sub sweep_top_down {
        my ( $self, $rlines, $group_level ) = @_;

        # Partition the set of lines into final alignment subgroups
        # and store the alignments with the lines.

        # The alignment subgroups we are making here are groups of consecutive
        # lines which have (1) identical alignment tokens and (2) do not
        # exceed the allowable maximum line length.  A later sweep from
        # left-to-right ('sweep_lr') will handle additional alignments.

        # transfer args to closure variables
        $rall_lines = $rlines;
        $grp_level  = $group_level;
        $rgroups    = [];
        initialize_for_new_rgroup();
        return unless @{$rlines};    # shouldn't happen

        # Unset the _end_group flag for the last line if it it set because it
        # is not needed and can causes problems for -lp formatting
        $rall_lines->[-1]->{'end_group'} = 0;

        # Loop over all lines ...
        my $jline = -1;
        foreach my $new_line ( @{$rall_lines} ) {
            $jline++;

            # Start a new subgroup if necessary
            if ( !$group_line_count ) {
                add_to_rgroup($jline);
                if ( $new_line->{'end_group'} ) {
                    end_rgroup(-1);
                }
                next;
            }

            my $j_terminal_match = $new_line->{'j_terminal_match'};
            my ( $jbeg, $jend ) = get_rgroup_jrange();
            if ( !defined($jbeg) ) {

                # safety check, shouldn't happen
                warning(<<EOM);
Program bug detected in Perl::Tidy::VerticalAligner sub sweep_top_down
undefined index for group line count $group_line_count
EOM
                $jbeg = $jline;
            }
            my $base_line = $rall_lines->[$jbeg];

            # Initialize a global flag saying if the last line of the group
            # should match end of group and also terminate the group.  There
            # should be no returns between here and where the flag is handled
            # at the bottom.
            my $col_matching_terminal = 0;
            if ( defined($j_terminal_match) ) {

                # remember the column of the terminal ? or { to match with
                $col_matching_terminal =
                  $base_line->get_column($j_terminal_match);

                # Ignore an undefined value as a defensive step; shouldn't
                # normally happen.
                $col_matching_terminal = 0
                  unless defined($col_matching_terminal);
            }

            # -------------------------------------------------------------
            # Allow hanging side comment to join current group, if any.  The
            # only advantage is to keep the other tokens in the same group. For
            # example, this would make the '=' align here:
            #  $ax         = 1;           # side comment
            #                             # hanging side comment
            #  $boondoggle = 5;           # side comment
            #  $beetle     = 5;           # side comment

            # here is another example..

            #  _rtoc_name_count   => {},                   # hash to track ..
            #  _rpackage_stack    => [],                   # stack to check ..
            #                                              # name changes
            #  _rlast_level       => \$last_level,         # brace indentation
            #
            #
            # If this were not desired, the next step could be skipped.
            # -------------------------------------------------------------
            if ( $new_line->{'is_hanging_side_comment'} ) {
                join_hanging_comment( $new_line, $base_line );
            }

            # If this line has no matching tokens, then flush out the lines
            # BEFORE this line unless both it and the previous line have side
            # comments.  This prevents this line from pushing side comments out
            # to the right.
            elsif ( $new_line->{'jmax'} == 1 ) {

                # There are no matching tokens, so now check side comments.
                # Programming note: accessing arrays with index -1 is
                # risky in Perl, but we have verified there is at least one
                # line in the group and that there is at least one field,
                my $prev_comment =
                  $rall_lines->[ $jline - 1 ]->{'rfields'}->[-1];
                my $side_comment = $new_line->{'rfields'}->[-1];

                # do not end group if both lines have side comments
                if ( !$side_comment || !$prev_comment ) {

                    # Otherwise - VSN PATCH for a single number:
                    # - do not end group if numeric and no side comment, or
                    # - end if !numeric or side comment
                    my $pat        = $new_line->{'rpatterns'}->[0];
                    my $is_numeric = $rOpts_valign_signed_numbers
                      && ( $pat eq 'n,'
                        || $pat eq 'n,b' );
                    end_rgroup(-1) if ( !$is_numeric || $side_comment );
                }
            }
            else {
                ##ok: continue
            }

            # See if the new line matches and fits the current group,
            # if it still exists. Flush the current group if not.
            my $match_code;
            if ($group_line_count) {
                ( $match_code, my $imax_align ) =
                  $self->check_match( $new_line, $base_line,
                    $rall_lines->[ $jline - 1 ],
                    $group_line_count );
                if ( $match_code != 2 ) { end_rgroup($imax_align) }
            }

            # Store the new line
            add_to_rgroup($jline);

            if ( defined($j_terminal_match) ) {

                # Decide if we should fix a terminal match. We can either:
                # 1. fix it and prevent the sweep_lr from changing it, or
                # 2. leave it alone and let sweep_lr try to fix it.

                # The current logic is to fix it if:
                # -it has not joined to previous lines,
                # -and either the previous subgroup has just 1 line, or
                # -this line matched but did not fit (so sweep won't work)
                my $fixit;
                if ( $group_line_count == 1 ) {
                    $fixit ||= $match_code;
                    if ( !$fixit ) {
                        if ( @{$rgroups} > 1 ) {
                            my ( $jbegx, $jendx ) = @{ $rgroups->[-2] };
                            my $nlines = $jendx - $jbegx + 1;
                            $fixit ||= $nlines <= 1;
                        }
                    }
                }

                if ($fixit) {
                    $base_line = $new_line;
                    my $col_now = $base_line->get_column($j_terminal_match);

                    # Ignore an undefined value as a defensive step; shouldn't
                    # normally happen.
                    $col_now = 0 unless defined($col_now);

                    my $pad = $col_matching_terminal - $col_now;
                    my $padding_available =
                      $base_line->get_available_space_on_right();
                    if ( $col_now && $pad > 0 && $pad <= $padding_available ) {
                        $base_line->increase_field_width( $j_terminal_match,
                            $pad );
                    }

                    # do not let sweep_left_to_right change an isolated 'else'
                    if ( !$new_line->{'is_terminal_ternary'} ) {
                        block_penultimate_match();
                    }
                }
                end_rgroup(-1);
            }

            # end the group if we know we cannot match next line.
            elsif ( $new_line->{'end_group'} ) {
                end_rgroup(-1);
            }

            else {
                ##ok: continue
            }
        } ## end loop over lines

        end_rgroup(-1);
        return ($rgroups);
    } ## end sub sweep_top_down
}

sub two_line_pad {

    my ( $line_m, $line, $imax_min ) = @_;

    # Given:
    #  two isolated (list) lines
    #  imax_min = number of common alignment tokens
    # Return:
    #  $pad_max = maximum suggested pad distance
    #           = 0 if alignment not recommended
    # Note that this is only for two lines which do not have alignment tokens
    # in common with any other lines.  It is intended for lists, but it might
    # also be used for two non-list lines with a common leading '='.

    # Allow alignment if the difference in the two unpadded line lengths
    # is not more than either line length.  The idea is to avoid
    # aligning lines with very different field lengths, like these two:

    #   [
    #       'VARCHAR', DBI::SQL_VARCHAR, undef, "'", "'", undef, 0, 1,
    #       1, 0, 0, 0, undef, 0, 0
    #   ];
    my $rfield_lengths   = $line->{'rfield_lengths'};
    my $rfield_lengths_m = $line_m->{'rfield_lengths'};

    # Safety check - shouldn't happen
    return 0
      if ( $imax_min >= @{$rfield_lengths}
        || $imax_min >= @{$rfield_lengths_m} );

    my $lensum_m = 0;
    my $lensum   = 0;
    foreach my $i ( 0 .. $imax_min ) {
        $lensum_m += $rfield_lengths_m->[$i];
        $lensum   += $rfield_lengths->[$i];
    }

    my ( $lenmin, $lenmax ) =
      $lensum >= $lensum_m ? ( $lensum_m, $lensum ) : ( $lensum, $lensum_m );

    my $patterns_match;
    if ( $line_m->{'list_type'} && $line->{'list_type'} ) {
        $patterns_match = 1;
        my $rpatterns_m = $line_m->{'rpatterns'};
        my $rpatterns   = $line->{'rpatterns'};
        foreach my $i ( 0 .. $imax_min ) {
            my $pat   = $rpatterns->[$i];
            my $pat_m = $rpatterns_m->[$i];

            # VSN PATCH: allow numbers to match quotes
            if ( $pat_m ne $pat && length($pat_m) eq length($pat) ) {
                $pat   =~ tr/n/Q/;
                $pat_m =~ tr/n/Q/;
            }

            if ( $pat ne $pat_m ) { $patterns_match = 0; last; }
        }
    }

    my $pad_max = $lenmax;
    if ( !$patterns_match && $lenmax > 2 * $lenmin ) { $pad_max = 0 }

    return $pad_max;
} ## end sub two_line_pad

sub sweep_left_to_right {

    my ( $rlines, $rgroups, $group_level ) = @_;

    # So far we have divided the lines into groups having an equal number of
    # identical alignments.  Here we are going to look for common leading
    # alignments between the different groups and align them when possible.
    # For example, the three lines below are in three groups because each line
    # has a different number of commas.  In this routine we will sweep from
    # left to right, aligning the leading commas as we go, but stopping if we
    # hit the line length limit.

    #  my ( $num, $numi, $numj,  $xyza, $ka,   $xyzb, $kb, $aff, $error );
    #  my ( $i,   $j,    $error, $aff,  $asum, $avec );
    #  my ( $km,  $area, $varea );

    # nothing to do if just one group
    my $ng_max = @{$rgroups} - 1;
    return if ( $ng_max <= 0 );

    #---------------------------------------------------------------------
    # Step 1: Loop over groups to find all common leading alignment tokens
    #---------------------------------------------------------------------

    my $line;
    my $rtokens;
    my $imax;     # index of maximum non-side-comment alignment token
    my $istop;    # an optional stopping index
    my $jbeg;     # starting line index
    my $jend;     # ending line index

    my $line_m;
    my $rtokens_m;
    my $imax_m;
    my $istop_m;
    my $jbeg_m;
    my $jend_m;

    my $istop_mm;

    # Look at neighboring pairs of groups and form a simple list
    # of all common leading alignment tokens. Foreach such match we
    # store [$i, $ng], where
    #  $i = index of the token in the line (0,1,...)
    #  $ng is the second of the two groups with this common token
    my @icommon;

    # Hash to hold the maximum alignment change for any group
    my %max_move;

    # a small number of columns
    my $short_pad = 4;

    my $ng = -1;
    foreach my $item ( @{$rgroups} ) {
        $ng++;

        $istop_mm = $istop_m;

        # save _m values of previous group
        $line_m    = $line;
        $rtokens_m = $rtokens;
        $imax_m    = $imax;
        $istop_m   = $istop;
        $jbeg_m    = $jbeg;
        $jend_m    = $jend;

        # Get values for this group. Note that we just have to use values for
        # one of the lines of the group since all members have the same
        # alignments.
        ( $jbeg, $jend, $istop ) = @{$item};

        $line    = $rlines->[$jbeg];
        $rtokens = $line->{'rtokens'};
        $imax    = $line->{'jmax'} - 2;
        $istop   = -1    if ( !defined($istop) );
        $istop   = $imax if ( $istop > $imax );

        # Initialize on first group
        next if ( $ng == 0 );

        # Use the minimum index limit of the two groups
        my $imax_min = $imax > $imax_m ? $imax_m : $imax;

        # Also impose a limit if given.
        if ( $istop_m < $imax_min ) {
            $imax_min = $istop_m;
        }

        # Special treatment of two one-line groups isolated from other lines,
        # unless they form a simple list or a terminal match.  Otherwise the
        # alignment can look strange in some cases.
        my $list_type = $rlines->[$jbeg]->{'list_type'};
        if (
               $jend == $jbeg
            && $jend_m == $jbeg_m
            && ( $ng == 1 || $istop_mm < 0 )
            && ( $ng == $ng_max || $istop < 0 )
            && !$line->{'j_terminal_match'}

            # Only do this for imperfect matches. This is normally true except
            # when two perfect matches cannot form a group because the line
            # length limit would be exceeded. In that case we can still try
            # to match as many alignments as possible.
            && ( $imax != $imax_m || $istop_m != $imax_m )
          )
        {

            # We will just align assignments and simple lists
            next if ( $imax_min < 0 );
            next
              if ( $rtokens->[0] !~ /^=\d/
                && !$list_type );

            # In this case we will limit padding to a short distance.  This
            # is a compromise to keep some vertical alignment but prevent large
            # gaps, which do not look good for just two lines.
            my $pad_max =
              two_line_pad( $rlines->[$jbeg], $rlines->[$jbeg_m], $imax_min );
            next if ( !$pad_max );
            my $ng_m = $ng - 1;
            $max_move{"$ng_m"} = $pad_max;
            $max_move{"$ng"}   = $pad_max;
        }

        # Loop to find all common leading tokens.
        if ( $imax_min >= 0 ) {
            foreach my $i ( 0 .. $imax_min ) {
                my $tok   = $rtokens->[$i];
                my $tok_m = $rtokens_m->[$i];
                last if ( $tok ne $tok_m );
                push @icommon, [ $i, $ng, $tok ];
            }
        }
    }
    return unless @icommon;

    #----------------------------------------------------------
    # Step 2: Reorder and consolidate the list into a task list
    #----------------------------------------------------------

    # We have to work first from lowest token index to highest, then by group,
    # sort our list first on token index then group number
    @icommon = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @icommon;

    # Make a task list of the form
    #   [$i, ng_beg, $ng_end, $tok], ..
    # where
    #   $i is the index of the token to be aligned
    #   $ng_beg..$ng_end is the group range for this action
    my @todo;
    my ( $i, $ng_end, $tok );
    foreach my $item (@icommon) {
        my $ng_last = $ng_end;
        my $i_last  = $i;
        ( $i, $ng_end, $tok ) = @{$item};
        my $ng_beg = $ng_end - 1;
        if ( defined($ng_last) && $ng_beg == $ng_last && $i == $i_last ) {
            my $var = pop(@todo);
            $ng_beg = $var->[1];
        }
        my ( $raw_tok, $lev, $tag, $tok_count ) = decode_alignment_token($tok);
        push @todo, [ $i, $ng_beg, $ng_end, $raw_tok, $lev ];
    }

    #------------------------------
    # Step 3: Execute the task list
    #------------------------------
    do_left_to_right_sweep(
        {
            rlines      => $rlines,
            rgroups     => $rgroups,
            rtodo       => \@todo,
            rmax_move   => \%max_move,
            short_pad   => $short_pad,
            group_level => $group_level,
        }
    );
    return;
} ## end sub sweep_left_to_right

{    ## closure for sub do_left_to_right_sweep

    my %is_good_alignment_token;

    BEGIN {

        # One of the most difficult aspects of vertical alignment is knowing
        # when not to align.  Alignment can go from looking very nice to very
        # bad when overdone.  In the sweep algorithm there are two special
        # cases where we may need to limit padding to a '$short_pad' distance
        # to avoid some very ugly formatting:

        # 1. Two isolated lines with partial alignment
        # 2. A 'tail-wag-dog' situation, in which a single terminal
        #    line with partial alignment could cause a significant pad
        #    increase in many previous lines if allowed to join the alignment.

        # For most alignment tokens, we will allow only a small pad to be
        # introduced (the hardwired $short_pad variable) . But for some 'good'
        # alignments we can be less restrictive.

        # These are 'good' alignments, which are allowed more padding:
        my @q = qw(
          => = ? if unless or || {
        );
        push @q, ',';
        @is_good_alignment_token{@q} = (0) x scalar(@q);

        # Promote a few of these to 'best', with essentially no pad limit:
        $is_good_alignment_token{'='}      = 1;
        $is_good_alignment_token{'if'}     = 1;
        $is_good_alignment_token{'unless'} = 1;
        $is_good_alignment_token{'=>'}     = 1;

        # Note the hash values are set so that:
        #         if ($is_good_alignment_token{$raw_tok}) => best
        # if defined ($is_good_alignment_token{$raw_tok}) => good or best

    } ## end BEGIN

    sub move_to_common_column {

        # This is a sub called by sub do_left_to_right_sweep to
        # move the alignment column of token $itok to $col_want for a
        # sequence of groups.
        my ($rcall_hash) = @_;

        my $rlines    = $rcall_hash->{rlines};
        my $rgroups   = $rcall_hash->{rgroups};
        my $rmax_move = $rcall_hash->{rmax_move};
        my $ngb       = $rcall_hash->{ngb};
        my $nge       = $rcall_hash->{nge};
        my $itok      = $rcall_hash->{itok};
        my $col_want  = $rcall_hash->{col_want};
        my $raw_tok   = $rcall_hash->{raw_tok};

        return if ( !defined($ngb) || $nge <= $ngb );
        foreach my $ng ( $ngb .. $nge ) {

            my ( $jbeg, $jend ) = @{ $rgroups->[$ng] };
            my $line = $rlines->[$jbeg];
            my $col  = $line->get_column($itok);
            my $move = $col_want - $col;
            if ( $move > 0 ) {

                # limit padding increase in isolated two lines
                next
                  if ( defined( $rmax_move->{$ng} )
                    && $move > $rmax_move->{$ng}
                    && !$is_good_alignment_token{$raw_tok} );

                $line->increase_field_width( $itok, $move );
            }
            elsif ( $move < 0 ) {

                # spot to take special action on failure to move
            }
            else {
                ##ok: (move==0)
            }
        }
        return;
    } ## end sub move_to_common_column

    sub do_left_to_right_sweep {

        my ($rcall_hash) = @_;

        # This is the worker routine for sub 'sweep_left_to_right'. Make
        # vertical alignments by sweeping from left to right over groups
        # of lines which have been located and prepared by the caller.

        my $rlines      = $rcall_hash->{rlines};
        my $rgroups     = $rcall_hash->{rgroups};
        my $rtodo       = $rcall_hash->{rtodo};
        my $rmax_move   = $rcall_hash->{rmax_move};
        my $short_pad   = $rcall_hash->{short_pad};
        my $group_level = $rcall_hash->{group_level};

        # $blocking_level[$nj is the level at a match failure between groups
        # $ng-1 and $ng
        my @blocking_level;
        my $group_list_type = $rlines->[0]->{'list_type'};

        foreach my $task ( @{$rtodo} ) {
            my ( $itok, $ng_beg, $ng_end, $raw_tok, $lev ) = @{$task};

            # Nothing to do for a single group
            next if ( $ng_end <= $ng_beg );

            my $ng_first;  # index of the first group of a continuous sequence
            my $col_want;  # the common alignment column of a sequence of groups
            my $col_limit; # maximum column before bumping into max line length
            my $line_count_ng_m = 0;
            my $jmax_m;
            my $it_stop_m;

            # Loop over the groups
            # 'ix_' = index in the array of lines
            # 'ng_' = index in the array of groups
            # 'it_' = index in the array of tokens
            my $ix_min      = $rgroups->[$ng_beg]->[0];
            my $ix_max      = $rgroups->[$ng_end]->[1];
            my $lines_total = $ix_max - $ix_min + 1;
            foreach my $ng ( $ng_beg .. $ng_end ) {
                my ( $ix_beg, $ix_end, $it_stop ) = @{ $rgroups->[$ng] };
                my $line_count_ng = $ix_end - $ix_beg + 1;

                # Important: note that since all lines in a group have a common
                # alignments object, we just have to work on one of the lines
                # (the first line).  All of the rest will be changed
                # automatically.
                my $line = $rlines->[$ix_beg];
                my $jmax = $line->{'jmax'};

                # the maximum space without exceeding the line length:
                my $avail   = $line->get_available_space_on_right();
                my $col     = $line->get_column($itok);
                my $col_max = $col + $avail;

                # Initialize on first group
                if ( !defined($col_want) ) {
                    $ng_first        = $ng;
                    $col_want        = $col;
                    $col_limit       = $col_max;
                    $line_count_ng_m = $line_count_ng;
                    $jmax_m          = $jmax;
                    $it_stop_m       = $it_stop;
                    next;
                }

                # RULE: Throw a blocking flag upon encountering a token level
                # different from the level of the first blocking token.  For
                # example, in the following example, if the = matches get
                # blocked between two groups as shown, then we want to start
                # blocking matches at the commas, which are at deeper level, so
                # that we do not get the big gaps shown here:

                #  my $unknown3 = pack( "v",          -2 );
                #  my $unknown4 = pack( "v",          0x09 );
                #  my $unknown5 = pack( "VVV",        0x06, 0x00, 0x00 );
                #  my $num_bbd_blocks  = pack( "V",   $num_lists );
                #  my $root_startblock = pack( "V",   $root_start );
                #  my $unknown6        = pack( "VV",  0x00, 0x1000 );

                # On the other hand, it is okay to keep matching at the same
                # level such as in a simple list of commas and/or fat commas.

                my $is_blocked = defined( $blocking_level[$ng] )
                  && $lev > $blocking_level[$ng];

                # TAIL-WAG-DOG RULE: prevent a 'tail-wag-dog' syndrom, meaning:
                # Do not let one or two lines with a **different number of
                # alignments** open up a big gap in a large block.  For
                # example, we will prevent something like this, where the first
                # line pries open the rest:

            #  $worksheet->write( "B7", "http://www.perl.com", undef, $format );
            #  $worksheet->write( "C7", "",                    $format );
            #  $worksheet->write( "D7", "",                    $format );
            #  $worksheet->write( "D8", "",                    $format );
            #  $worksheet->write( "D8", "",                    $format );

                # We should exclude from consideration two groups which are
                # effectively the same but separated because one does not
                # fit in the maximum allowed line length.
                my $is_same_group =
                  $jmax == $jmax_m && $it_stop_m == $jmax_m - 2;

                my $lines_above = $ix_beg - $ix_min;
                my $lines_below = $lines_total - $lines_above;

                # Increase the tolerable gap for certain favorable factors
                my $factor    = 1;
                my $top_level = $lev == $group_level;

                # Align best top level alignment tokens like '=', 'if', ...
                # A factor of 10 allows a gap of up to 40 spaces
                if ( $top_level && $is_good_alignment_token{$raw_tok} ) {
                    $factor = 10;
                }

                # Otherwise allow some minimal padding of good alignments
                else {

                    if (

                        defined( $is_good_alignment_token{$raw_tok} )

                        # We have to be careful if there are just 2 lines.
                        # This two-line factor allows large gaps only for 2
                        # lines which are simple lists with fewer items on the
                        # second line. It gives results similar to previous
                        # versions of perltidy.
                        && (
                            $lines_total > 2
                            || (   $group_list_type
                                && $jmax < $jmax_m
                                && $top_level )
                        )
                      )
                    {
                        $factor += 1;
                        if ($top_level) {
                            $factor += 1;
                        }
                    }
                }

                my $is_big_gap;
                if ( !$is_same_group ) {
                    $is_big_gap ||=
                      (      $lines_above == 1
                          || $lines_above == 2 && $lines_below >= 4 )
                      && $col_want > $col + $short_pad * $factor;
                    $is_big_gap ||=
                      (      $lines_below == 1
                          || $lines_below == 2 && $lines_above >= 4 )
                      && $col > $col_want + $short_pad * $factor;
                }

                # if match is limited by gap size, stop aligning at this level
                if ($is_big_gap) {
                    $blocking_level[$ng] = $lev - 1;
                }

                # quit and restart if it cannot join this batch
                if (   $col_want > $col_max
                    || $col > $col_limit
                    || $is_big_gap
                    || $is_blocked )
                {

                    # remember the level of the first blocking token
                    if ( !defined( $blocking_level[$ng] ) ) {
                        $blocking_level[$ng] = $lev;
                    }

                    move_to_common_column(
                        {
                            rlines    => $rlines,
                            rgroups   => $rgroups,
                            rmax_move => $rmax_move,
                            ngb       => $ng_first,
                            nge       => $ng - 1,
                            itok      => $itok,
                            col_want  => $col_want,
                            raw_tok   => $raw_tok,
                        }
                    );
                    $ng_first        = $ng;
                    $col_want        = $col;
                    $col_limit       = $col_max;
                    $line_count_ng_m = $line_count_ng;
                    $jmax_m          = $jmax;
                    $it_stop_m       = $it_stop;
                    next;
                }

                $line_count_ng_m += $line_count_ng;

                # update the common column and limit
                if ( $col > $col_want )      { $col_want  = $col }
                if ( $col_max < $col_limit ) { $col_limit = $col_max }

            } ## end loop over groups

            if ( $ng_end > $ng_first ) {
                move_to_common_column(
                    {
                        rlines    => $rlines,
                        rgroups   => $rgroups,
                        rmax_move => $rmax_move,
                        ngb       => $ng_first,
                        nge       => $ng_end,
                        itok      => $itok,
                        col_want  => $col_want,
                        raw_tok   => $raw_tok,
                    }
                );
            }
        } ## end loop over tasks

        return;
    } ## end sub do_left_to_right_sweep
}

sub delete_selected_tokens {

    my ( $line_obj, $ridel ) = @_;

    # $line_obj    is the line to be modified
    # $ridel       is a ref to list of indexes to be deleted

    # remove an unused alignment token(s) to improve alignment chances

    return if ( !defined($line_obj) || !defined($ridel) || !@{$ridel} );

    my $jmax_old           = $line_obj->{'jmax'};
    my $rfields_old        = $line_obj->{'rfields'};
    my $rfield_lengths_old = $line_obj->{'rfield_lengths'};
    my $rpatterns_old      = $line_obj->{'rpatterns'};
    my $rtokens_old        = $line_obj->{'rtokens'};
    my $j_terminal_match   = $line_obj->{'j_terminal_match'};

    use constant EXPLAIN_DELETE_SELECTED => 0;

    local $LIST_SEPARATOR = '> <';
    EXPLAIN_DELETE_SELECTED && print <<EOM;
delete indexes: <@{$ridel}>
old jmax: $jmax_old
old tokens: <@{$rtokens_old}>
old patterns: <@{$rpatterns_old}>
old fields: <@{$rfields_old}>
old field_lengths: <@{$rfield_lengths_old}>
EOM

    my $rfields_new        = [];
    my $rpatterns_new      = [];
    my $rtokens_new        = [];
    my $rfield_lengths_new = [];

    # Convert deletion list to a hash to allow any order, multiple entries,
    # and avoid problems with index values out of range
    my %delete_me;
    @delete_me{ @{$ridel} } = (1) x scalar( @{$ridel} );

    my $pattern_0      = $rpatterns_old->[0];
    my $field_0        = $rfields_old->[0];
    my $field_length_0 = $rfield_lengths_old->[0];
    push @{$rfields_new},        $field_0;
    push @{$rfield_lengths_new}, $field_length_0;
    push @{$rpatterns_new},      $pattern_0;

    # Loop to either copy items or concatenate fields and patterns
    my $jmin_del;
    foreach my $j ( 0 .. $jmax_old - 1 ) {
        my $token        = $rtokens_old->[$j];
        my $field        = $rfields_old->[ $j + 1 ];
        my $field_length = $rfield_lengths_old->[ $j + 1 ];
        my $pattern      = $rpatterns_old->[ $j + 1 ];
        if ( !$delete_me{$j} ) {
            push @{$rtokens_new},        $token;
            push @{$rfields_new},        $field;
            push @{$rpatterns_new},      $pattern;
            push @{$rfield_lengths_new}, $field_length;
        }
        else {
            if ( !defined($jmin_del) ) { $jmin_del = $j }
            $rfields_new->[-1] .= $field;
            $rfield_lengths_new->[-1] += $field_length;
            $rpatterns_new->[-1] .= $pattern;
        }
    }

    # ----- x ------ x ------ x ------
    #t      0        1        2        <- token indexing
    #f   0      1        2        3    <- field and pattern

    my $jmax_new = @{$rfields_new} - 1;
    $line_obj->{'rtokens'}        = $rtokens_new;
    $line_obj->{'rpatterns'}      = $rpatterns_new;
    $line_obj->{'rfields'}        = $rfields_new;
    $line_obj->{'rfield_lengths'} = $rfield_lengths_new;
    $line_obj->{'jmax'}           = $jmax_new;

    # The value of j_terminal_match will be incorrect if we delete tokens prior
    # to it. We will have to give up on aligning the terminal tokens if this
    # happens.
    if ( defined($j_terminal_match) && $jmin_del <= $j_terminal_match ) {
        $line_obj->{'j_terminal_match'} = undef;
    }

    # update list type -
    if ( $line_obj->{'list_seqno'} ) {

        ## This works, but for efficiency see if we need to make a change:
        ## decide_if_list($line_obj);

        # An existing list will still be a list but with possibly different
        # leading token
        my $old_list_type = $line_obj->{'list_type'};
        my $new_list_type = EMPTY_STRING;
        if ( $rtokens_new->[0] =~ /^(=>|,)/ ) {
            $new_list_type = $rtokens_new->[0];
        }
        if ( !$old_list_type || $old_list_type ne $new_list_type ) {
            decide_if_list($line_obj);
        }
    }

    EXPLAIN_DELETE_SELECTED && print <<EOM;

new jmax: $jmax_new
new tokens: <@{$rtokens_new}>
new patterns: <@{$rpatterns_new}>
new fields: <@{$rfields_new}>
EOM
    return;
} ## end sub delete_selected_tokens

{    ## closure for sub decode_alignment_token

    # This routine is called repeatedly for each token, so it needs to be
    # efficient.  We can speed things up by remembering the inputs and outputs
    # in a hash.
    my %decoded_token;

    sub initialize_decode {

        # We will re-initialize the hash for each file. Otherwise, there is
        # a danger that the hash can become arbitrarily large if a very large
        # number of files is processed at once.
        %decoded_token = ();
        return;
    } ## end sub initialize_decode

    sub decode_alignment_token {

        # Unpack the values packed in an alignment token
        #
        # Usage:
        #        my ( $raw_tok, $lev, $tag, $tok_count ) =
        #          decode_alignment_token($token);

        # Alignment tokens have a trailing decimal level and optional tag (for
        # commas):
        # For example, the first comma in the following line
        #     sub banner  { crlf; report( shift, '/', shift ); crlf }
        # is decorated as follows:
        #    ,2+report-6  => (tok,lev,tag) =qw( ,   2   +report-6)

        # An optional token count may be appended with a leading dot.
        # Currently this is only done for '=' tokens but this could change.
        # For example, consider the following line:
        #   $nport   = $port = shift || $name;
        # The first '=' may either be '=0' or '=0.1' [level 0, first equals]
        # The second '=' will be '=0.2' [level 0, second equals]
        my ($tok) = @_;

        if ( defined( $decoded_token{$tok} ) ) {
            return @{ $decoded_token{$tok} };
        }

        my ( $raw_tok, $lev, $tag, $tok_count ) = ( $tok, 0, EMPTY_STRING, 1 );
        if ( $tok =~ /^(\D+)(\d+)([^\.]*)(\.(\d+))?$/ ) {
            $raw_tok   = $1;
            $lev       = $2;
            $tag       = $3 if ($3);
            $tok_count = $5 if ($5);
        }
        my @vals = ( $raw_tok, $lev, $tag, $tok_count );
        $decoded_token{$tok} = \@vals;
        return @vals;
    } ## end sub decode_alignment_token
}

{    ## closure for sub delete_unmatched_tokens

    my %is_assignment;
    my %keep_after_deleted_assignment;

    BEGIN {
        my @q;

        @q = qw(
          = **= += *= &= <<= &&=
          -= /= |= >>= ||= //=
          .= %= ^=
          x=
        );
        @is_assignment{@q} = (1) x scalar(@q);

        # These tokens may be kept following an = deletion
        @q = qw(
          if unless or ||
        );
        @keep_after_deleted_assignment{@q} = (1) x scalar(@q);

    } ## end BEGIN

    sub delete_unmatched_tokens {
        my ( $rlines, $group_level ) = @_;

        # This is a important first step in vertical alignment in which
        # we remove as many obviously un-needed alignment tokens as possible.
        # This will prevent them from interfering with the final alignment.

        # Returns:
        my $max_lev_diff     = 0;    # used to avoid a call to prune_tree
        my $saw_side_comment = 0;    # used to avoid a call for side comments

        # Handle no lines -- shouldn't happen
        return unless @{$rlines};

        # Handle a single line
        if ( @{$rlines} == 1 ) {
            my $line   = $rlines->[0];
            my $jmax   = $line->{'jmax'};
            my $length = $line->{'rfield_lengths'}->[$jmax];
            $saw_side_comment = $length > 0;
            return ( $max_lev_diff, $saw_side_comment );
        }

        # ignore hanging side comments in these operations
        my @filtered   = grep { !$_->{'is_hanging_side_comment'} } @{$rlines};
        my $rnew_lines = \@filtered;

        $saw_side_comment = @filtered != @{$rlines};
        $max_lev_diff     = 0;

        # nothing to do if all lines were hanging side comments
        my $jmax = @{$rnew_lines} - 1;
        return ( $max_lev_diff, $saw_side_comment ) if ( $jmax < 0 );

        #----------------------------------------------------
        # Create a hash of alignment token info for each line
        #----------------------------------------------------
        ( my $rline_hashes, my $requals_info, $saw_side_comment, $max_lev_diff )
          = make_alignment_info( $group_level, $rnew_lines, $saw_side_comment );

        #------------------------------------------------------------
        # Find independent subgroups of lines.  Neighboring subgroups
        # do not have a common alignment token.
        #------------------------------------------------------------
        my @subgroups;
        push @subgroups, [ 0, $jmax ];
        foreach my $jl ( 0 .. $jmax - 1 ) {
            if ( $rnew_lines->[$jl]->{'end_group'} ) {
                $subgroups[-1]->[1] = $jl;
                push @subgroups, [ $jl + 1, $jmax ];
            }
        }

        #-----------------------------------------------------------
        # PASS 1 over subgroups to remove unmatched alignment tokens
        #-----------------------------------------------------------
        delete_unmatched_tokens_main_loop(
            $group_level,  $rnew_lines, \@subgroups,
            $rline_hashes, $requals_info
        );

        #----------------------------------------------------------------
        # PASS 2: Construct a tree of matched lines and delete some small
        # deeper levels of tokens.  They also block good alignments.
        #----------------------------------------------------------------
        prune_alignment_tree($rnew_lines) if ($max_lev_diff);

        #--------------------------------------------
        # PASS 3: compare all lines for common tokens
        #--------------------------------------------
        match_line_pairs( $rlines, $rnew_lines, \@subgroups, $group_level );

        return ( $max_lev_diff, $saw_side_comment );
    } ## end sub delete_unmatched_tokens

    sub make_alignment_info {

        my ( $group_level, $rnew_lines, $saw_side_comment ) = @_;

        #------------------------------------------------------------
        # Loop to create a hash of alignment token info for each line
        #------------------------------------------------------------
        my $rline_hashes = [];
        my @equals_info;
        my @line_info;    # no longer used
        my $jmax         = @{$rnew_lines} - 1;
        my $max_lev_diff = 0;
        foreach my $line ( @{$rnew_lines} ) {
            my $rhash     = {};
            my $rtokens   = $line->{'rtokens'};
            my $rpatterns = $line->{'rpatterns'};
            my $i         = 0;
            my ( $i_eq, $tok_eq, $pat_eq );
            my ( $lev_min, $lev_max );
            foreach my $tok ( @{$rtokens} ) {
                my ( $raw_tok, $lev, $tag, $tok_count ) =
                  decode_alignment_token($tok);

                if ( $tok ne '#' ) {
                    if ( !defined($lev_min) ) {
                        $lev_min = $lev;
                        $lev_max = $lev;
                    }
                    else {
                        if ( $lev < $lev_min ) { $lev_min = $lev }
                        if ( $lev > $lev_max ) { $lev_max = $lev }
                    }
                }
                else {
                    if ( !$saw_side_comment ) {
                        my $length = $line->{'rfield_lengths'}->[ $i + 1 ];
                        $saw_side_comment ||= $length;
                    }
                }

                # Possible future upgrade: for multiple matches,
                # record [$i1, $i2, ..] instead of $i
                $rhash->{$tok} =
                  [ $i, undef, undef, $raw_tok, $lev, $tag, $tok_count ];

                # remember the first equals at line level
                if ( !defined($i_eq) && $raw_tok eq '=' ) {

                    if ( $lev eq $group_level ) {
                        $i_eq   = $i;
                        $tok_eq = $tok;
                        $pat_eq = $rpatterns->[$i];
                    }
                }
                $i++;
            }
            push @{$rline_hashes}, $rhash;
            push @equals_info,     [ $i_eq,    $tok_eq, $pat_eq ];
            push @line_info,       [ $lev_min, $lev_max ];
            if ( defined($lev_min) ) {
                my $lev_diff = $lev_max - $lev_min;
                if ( $lev_diff > $max_lev_diff ) { $max_lev_diff = $lev_diff }
            }
        }

        #----------------------------------------------------
        # Loop to compare each line pair and remember matches
        #----------------------------------------------------
        my $rtok_hash = {};
        my $nr        = 0;
        foreach my $jl ( 0 .. $jmax - 1 ) {
            my $nl = $nr;
            $nr = 0;
            my $jr      = $jl + 1;
            my $rhash_l = $rline_hashes->[$jl];
            my $rhash_r = $rline_hashes->[$jr];
            foreach my $tok ( keys %{$rhash_l} ) {
                if ( defined( $rhash_r->{$tok} ) ) {
                    my $il = $rhash_l->{$tok}->[0];
                    my $ir = $rhash_r->{$tok}->[0];
                    $rhash_l->{$tok}->[2] = $ir;
                    $rhash_r->{$tok}->[1] = $il;
                    if ( $tok ne '#' ) {
                        push @{ $rtok_hash->{$tok} }, ( $jl, $jr );
                        $nr++;
                    }
                }
            }

            # Set a line break if no matching tokens between these lines
            # (this is not strictly necessary now but does not hurt)
            if ( $nr == 0 && $nl > 0 ) {
                $rnew_lines->[$jl]->{'end_group'} = 1;
            }

            # Also set a line break if both lines have simple equals but with
            # different leading characters in patterns.  This check is similar
            # to one in sub check_match, and will prevent sub
            # prune_alignment_tree from removing alignments which otherwise
            # should be kept. This fix is rarely needed, but it can
            # occasionally improve formatting.
            # For example:
            #     my $name = $this->{Name};
            #     $type = $this->ctype($genlooptype) if defined $genlooptype;
            #     my $declini = ( $asgnonly ? ""          : "\t$type *" );
            #     my $cast    = ( $type     ? "($type *)" : "" );
            # The last two lines start with 'my' and will not match the
            # previous line starting with $type, so we do not want
            # prune_alignment tree to delete their ? : alignments at a deeper
            # level.
            my ( $i_eq_l, $tok_eq_l, $pat_eq_l ) = @{ $equals_info[$jl] };
            my ( $i_eq_r, $tok_eq_r, $pat_eq_r ) = @{ $equals_info[$jr] };
            if ( defined($i_eq_l) && defined($i_eq_r) ) {

                # Also, do not align equals across a change in ci level
                my $ci_jump = $rnew_lines->[$jl]->{'ci_level'} !=
                  $rnew_lines->[$jr]->{'ci_level'};

                if (
                       $tok_eq_l eq $tok_eq_r
                    && $i_eq_l == 0
                    && $i_eq_r == 0
                    && ( substr( $pat_eq_l, 0, 1 ) ne substr( $pat_eq_r, 0, 1 )
                        || $ci_jump )
                  )
                {
                    $rnew_lines->[$jl]->{'end_group'} = 1;
                }
            }
        }
        return ( $rline_hashes, \@equals_info, $saw_side_comment,
            $max_lev_diff );
    } ## end sub make_alignment_info

    sub delete_unmatched_tokens_main_loop {

        my (
            $group_level,  $rnew_lines, $rsubgroups,
            $rline_hashes, $requals_info
        ) = @_;

        #--------------------------------------------------------------
        # Main loop over subgroups to remove unmatched alignment tokens
        #--------------------------------------------------------------

        # flag to allow skipping pass 2 - not currently used
        my $saw_large_group;

        my $has_terminal_match = $rnew_lines->[-1]->{'j_terminal_match'};

        foreach my $item ( @{$rsubgroups} ) {
            my ( $jbeg, $jend ) = @{$item};

            my $nlines = $jend - $jbeg + 1;

            #---------------------------------------------------
            # Look for complete if/elsif/else and ternary blocks
            #---------------------------------------------------

            # We are looking for a common '$dividing_token' like these:

            #    if    ( $b and $s ) { $p->{'type'} = 'a'; }
            #    elsif ($b)          { $p->{'type'} = 'b'; }
            #    elsif ($s)          { $p->{'type'} = 's'; }
            #    else                { $p->{'type'} = ''; }
            #                        ^----------- dividing_token

            #   my $severity =
            #      !$routine                     ? '[PFX]'
            #     : $routine =~ /warn.*_d\z/     ? '[DS]'
            #     : $routine =~ /ck_warn/        ? 'W'
            #     : $routine =~ /ckWARN\d*reg_d/ ? 'S'
            #     : $routine =~ /ckWARN\d*reg/   ? 'W'
            #     : $routine =~ /vWARN\d/        ? '[WDS]'
            #     :                                '[PFX]';
            #                                    ^----------- dividing_token

            # Only look for groups which are more than 2 lines long.  Two lines
            # can get messed up doing this, probably due to the various
            # two-line rules.

            my $dividing_token;
            my %token_line_count;
            if ( $nlines > 2 ) {

                foreach my $jj ( $jbeg .. $jend ) {
                    my %seen;
                    my $line    = $rnew_lines->[$jj];
                    my $rtokens = $line->{'rtokens'};
                    foreach my $tok ( @{$rtokens} ) {
                        if ( !$seen{$tok} ) {
                            $seen{$tok}++;
                            $token_line_count{$tok}++;
                        }
                    }
                }

                foreach my $tok ( keys %token_line_count ) {
                    if ( $token_line_count{$tok} == $nlines ) {
                        if (   substr( $tok, 0, 1 ) eq '?'
                            || substr( $tok, 0, 1 ) eq '{'
                            && $tok =~ /^\{\d+if/ )
                        {
                            $dividing_token = $tok;
                            last;
                        }
                    }
                }
            }

            #-------------------------------------------------------------
            # Loop over subgroup lines to remove unwanted alignment tokens
            #-------------------------------------------------------------
            foreach my $jj ( $jbeg .. $jend ) {
                my $line    = $rnew_lines->[$jj];
                my $rtokens = $line->{'rtokens'};
                my $rhash   = $rline_hashes->[$jj];
                my $i_eq    = $requals_info->[$jj]->[0];
                my @idel;
                my $imax = @{$rtokens} - 2;
                my $delete_above_level;
                my $deleted_assignment_token;

                my $saw_dividing_token = EMPTY_STRING;
                $saw_large_group ||= $nlines > 2 && $imax > 1;

                # Loop over all alignment tokens
                foreach my $i ( 0 .. $imax ) {
                    my $tok = $rtokens->[$i];
                    next if ( $tok eq '#' );    # shouldn't happen
                    my ( $iii, $il, $ir, $raw_tok, $lev, $tag, $tok_count ) =
                      @{ $rhash->{$tok} };

                    #------------------------------------------------------
                    # Here is the basic RULE: remove an unmatched alignment
                    # which does not occur in the surrounding lines.
                    #------------------------------------------------------
                    my $delete_me = !defined($il) && !defined($ir);

                    # Apply any user controls. Note that not all lines pass
                    # this way so they have to be applied elsewhere too.
                    my $align_ok = 1;
                    if (%valign_control_hash) {
                        $align_ok = $valign_control_hash{$raw_tok};
                        $align_ok = $valign_control_default
                          unless defined($align_ok);
                        $delete_me ||= !$align_ok;
                    }

                    # But now we modify this with exceptions...

                    # EXCEPTION 1: If we are in a complete ternary or
                    # if/elsif/else group, and this token is not on every line
                    # of the group, should we delete it to preserve overall
                    # alignment?
                    if ($dividing_token) {
                        if ( $token_line_count{$tok} >= $nlines ) {
                            $saw_dividing_token ||= $tok eq $dividing_token;
                        }
                        else {

                            # For shorter runs, delete toks to save alignment.
                            # For longer runs, keep toks after the '{' or '?'
                            # to allow sub-alignments within braces.  The
                            # number 5 lines is arbitrary but seems to work ok.
                            $delete_me ||=
                              ( $nlines < 5 || !$saw_dividing_token );
                        }
                    }

                    # EXCEPTION 2: Remove all tokens above a certain level
                    # following a previous deletion.  For example, we have to
                    # remove tagged higher level alignment tokens following a
                    # '=>' deletion because the tags of higher level tokens
                    # will now be incorrect. For example, this will prevent
                    # aligning commas as follows after deleting the second '=>'
                    #    $w->insert(
                    #         ListBox => origin => [ 270, 160 ],
                    #         size    => [ 200,           55 ],
                    #    );
                    if ( defined($delete_above_level) ) {
                        if ( $lev > $delete_above_level ) {
                            $delete_me ||= 1;
                        }
                        else { $delete_above_level = undef }
                    }

                    # EXCEPTION 3: Remove all but certain tokens after an
                    # assignment deletion.
                    if (
                        $deleted_assignment_token
                        && ( $lev > $group_level
                            || !$keep_after_deleted_assignment{$raw_tok} )
                      )
                    {
                        $delete_me ||= 1;
                    }

                    # EXCEPTION 4: Do not touch the first line of a 2 line
                    # terminal match, such as below, because j_terminal has
                    # already been set.
                    #    if ($tag) { $tago = "<$tag>"; $tagc = "</$tag>"; }
                    #    else      { $tago = $tagc = ''; }
                    # But see snippets 'else1.t' and 'else2.t'
                    $delete_me = 0
                      if ( $jj == $jbeg
                        && $has_terminal_match
                        && $nlines == 2 );

                    # EXCEPTION 5: misc additional rules for commas and equals
                    if ( $delete_me && $tok_count == 1 ) {

                        # okay to delete second and higher copies of a token

                        # for a comma...
                        if ( $raw_tok eq ',' ) {

                            # Do not delete commas before an equals
                            $delete_me = 0
                              if ( defined($i_eq) && $i < $i_eq );

                            # Do not delete line-level commas
                            $delete_me = 0 if ( $lev <= $group_level );
                        }

                        # For an assignment at group level..
                        if (   $is_assignment{$raw_tok}
                            && $lev == $group_level )
                        {

                            # Do not delete if it is the last alignment of
                            # multiple tokens; this will prevent some
                            # undesirable alignments
                            if ( $imax > 0 && $i == $imax ) {
                                $delete_me = 0;
                            }

                            # Otherwise, set a flag to delete most
                            # remaining tokens
                            else { $deleted_assignment_token = $raw_tok }
                        }
                    }

                    # Do not let a user exclusion be reactivated by above rules
                    $delete_me ||= !$align_ok;

                    #------------------------------------
                    # Add this token to the deletion list
                    #------------------------------------
                    if ($delete_me) {
                        push @idel, $i;

                        # update deletion propagation flags
                        if ( !defined($delete_above_level)
                            || $lev < $delete_above_level )
                        {

                            # delete all following higher level alignments
                            $delete_above_level = $lev;

                            # but keep deleting after => to next lower level
                            # to avoid some bizarre alignments
                            if ( $raw_tok eq '=>' ) {
                                $delete_above_level = $lev - 1;
                            }
                        }
                    }
                }    # End loop over alignment tokens

                # Process all deletion requests for this line
                if (@idel) {
                    delete_selected_tokens( $line, \@idel );
                }
            }    # End loop over lines
        } ## end main loop over subgroups

        return;
    } ## end sub delete_unmatched_tokens_main_loop
}

sub match_line_pairs {
    my ( $rlines, $rnew_lines, $rsubgroups, $group_level ) = @_;

    # Compare each pair of lines and save information about common matches
    # $rlines     = list of lines including hanging side comments
    # $rnew_lines = list of lines without any hanging side comments
    # $rsubgroups = list of subgroups of the new lines

    # TODO:
    # Maybe change: imax_pair => pair_match_info = ref to array
    #  = [$imax_align, $rMsg, ... ]
    #  This may eventually have multi-level match info

    # Previous line vars
    my ( $line_m, $rtokens_m, $rpatterns_m, $rfield_lengths_m, $imax_m,
        $list_type_m, $ci_level_m );

    # Current line vars
    my ( $line, $rtokens, $rpatterns, $rfield_lengths, $imax, $list_type,
        $ci_level );

    # loop over subgroups
    foreach my $item ( @{$rsubgroups} ) {
        my ( $jbeg, $jend ) = @{$item};
        my $nlines = $jend - $jbeg + 1;
        next if ( $nlines <= 1 );

        # loop over lines in a subgroup
        foreach my $jj ( $jbeg .. $jend ) {

            $line_m           = $line;
            $rtokens_m        = $rtokens;
            $rpatterns_m      = $rpatterns;
            $rfield_lengths_m = $rfield_lengths;
            $imax_m           = $imax;
            $list_type_m      = $list_type;
            $ci_level_m       = $ci_level;

            $line           = $rnew_lines->[$jj];
            $rtokens        = $line->{'rtokens'};
            $rpatterns      = $line->{'rpatterns'};
            $rfield_lengths = $line->{'rfield_lengths'};
            $imax           = @{$rtokens} - 2;
            $list_type      = $line->{'list_type'};
            $ci_level       = $line->{'ci_level'};

            # nothing to do for first line
            next if ( $jj == $jbeg );

            my $ci_jump = $ci_level - $ci_level_m;

            my $imax_min = $imax_m < $imax ? $imax_m : $imax;

            my $imax_align = -1;

            # find number of leading common tokens

            #---------------------------------
            # No match to hanging side comment
            #---------------------------------
            if ( $line->{'is_hanging_side_comment'} ) {

                # Should not get here; HSC's have been filtered out
                $imax_align = -1;
            }

            #-----------------------------
            # Handle comma-separated lists
            #-----------------------------
            elsif ( $list_type && $list_type eq $list_type_m ) {

                # do not align lists across a ci jump with new list method
                if ($ci_jump) { $imax_min = -1 }

                my $i_nomatch = $imax_min + 1;
                foreach my $i ( 0 .. $imax_min ) {
                    my $tok   = $rtokens->[$i];
                    my $tok_m = $rtokens_m->[$i];
                    if ( $tok ne $tok_m ) {
                        $i_nomatch = $i;
                        last;
                    }
                }

                $imax_align = $i_nomatch - 1;
            }

            #-----------------
            # Handle non-lists
            #-----------------
            else {
                my $i_nomatch = $imax_min + 1;
                foreach my $i ( 0 .. $imax_min ) {
                    my $tok   = $rtokens->[$i];
                    my $tok_m = $rtokens_m->[$i];
                    if ( $tok ne $tok_m ) {
                        $i_nomatch = $i;
                        last;
                    }

                    my $pat   = $rpatterns->[$i];
                    my $pat_m = $rpatterns_m->[$i];

                    # VSN PATCH: allow numbers to match quotes
                    if ( $pat_m ne $pat ) {
                        $pat   =~ tr/n/Q/;
                        $pat_m =~ tr/n/Q/;
                    }

                    # If patterns don't match, we have to be careful...
                    if ( $pat_m ne $pat ) {
                        my $pad =
                          $rfield_lengths->[$i] - $rfield_lengths_m->[$i];
                        my $match_code = compare_patterns(
                            {
                                group_level => $group_level,
                                tok         => $tok,
                                tok_m       => $tok_m,
                                pat         => $pat,
                                pat_m       => $pat_m,
                                pad         => $pad,
                            }
                        );
                        if ($match_code) {
                            if    ( $match_code == 1 ) { $i_nomatch = $i }
                            elsif ( $match_code == 2 ) { $i_nomatch = 0 }
                            else                       { }                  ##ok
                            last;
                        }
                    }
                }
                $imax_align = $i_nomatch - 1;
            }

            $line_m->{'imax_pair'} = $imax_align;

        } ## end loop over lines

        # Put fence at end of subgroup
        $line->{'imax_pair'} = -1;

    } ## end loop over subgroups

    # if there are hanging side comments, propagate the pair info down to them
    # so that lines can just look back one line for their pair info.
    if ( @{$rlines} > @{$rnew_lines} ) {
        my $last_pair_info = -1;
        foreach my $line_t ( @{$rlines} ) {
            if ( $line_t->{'is_hanging_side_comment'} ) {
                $line_t->{'imax_pair'} = $last_pair_info;
            }
            else {
                $last_pair_info = $line_t->{'imax_pair'};
            }
        }
    }
    return;
} ## end sub match_line_pairs

sub compare_patterns {

    my ($rcall_hash) = @_;

    my $group_level = $rcall_hash->{group_level};
    my $tok         = $rcall_hash->{tok};
    my $tok_m       = $rcall_hash->{tok_m};
    my $pat         = $rcall_hash->{pat};
    my $pat_m       = $rcall_hash->{pat_m};
    my $pad         = $rcall_hash->{pad};

    # helper routine for sub match_line_pairs to decide if patterns in two
    # lines match well enough..Given
    #   $tok_m, $pat_m = token and pattern of first line
    #   $tok, $pat     = token and pattern of second line
    #   $pad           = 0 if no padding is needed, !=0 otherwise
    # return code:
    #   0 = patterns match, continue
    #   1 = no match
    #   2 = no match, and lines do not match at all

    my $GoToMsg     = EMPTY_STRING;
    my $return_code = 0;

    use constant EXPLAIN_COMPARE_PATTERNS => 0;

    my ( $alignment_token, $lev, $tag, $tok_count ) =
      decode_alignment_token($tok);

    # We have to be very careful about aligning commas
    # when the pattern's don't match, because it can be
    # worse to create an alignment where none is needed
    # than to omit one.  Here's an example where the ','s
    # are not in named containers.  The first line below
    # should not match the next two:
    #   ( $a, $b ) = ( $b, $r );
    #   ( $x1, $x2 ) = ( $x2 - $q * $x1, $x1 );
    #   ( $y1, $y2 ) = ( $y2 - $q * $y1, $y1 );
    if ( $alignment_token eq ',' ) {

        # do not align commas unless they are in named
        # containers
        if ( $tok !~ /[A-Za-z]/ ) {
            $return_code = 1;
            $GoToMsg     = "do not align commas in unnamed containers";
        }
        else {
            $return_code = 0;
        }
    }

    # do not align parens unless patterns match;
    # large ugly spaces can occur in math expressions.
    elsif ( $alignment_token eq '(' ) {

        # But we can allow a match if the parens don't
        # require any padding.
        if ( $pad != 0 ) {
            $return_code = 1;
            $GoToMsg     = "do not align '(' unless patterns match or pad=0";
        }
        else {
            $return_code = 0;
        }
    }

    # Handle an '=' alignment with different patterns to
    # the left.
    elsif ( $alignment_token eq '=' ) {

        # It is best to be a little restrictive when
        # aligning '=' tokens.  Here is an example of
        # two lines that we will not align:
        #       my $variable=6;
        #       $bb=4;
        # The problem is that one is a 'my' declaration,
        # and the other isn't, so they're not very similar.
        # We will filter these out by comparing the first
        # letter of the pattern.  This is crude, but works
        # well enough.
        if ( substr( $pat_m, 0, 1 ) ne substr( $pat, 0, 1 ) ) {
            $GoToMsg     = "first character before equals differ";
            $return_code = 1;
        }

        # The introduction of sub 'prune_alignment_tree'
        # enabled alignment of lists left of the equals with
        # other scalar variables. For example:
        # my ( $D, $s, $e ) = @_;
        # my $d             = length $D;
        # my $c             = $e - $s - $d;

        # But this would change formatting of a lot of scripts,
        # so for now we prevent alignment of comma lists on the
        # left with scalars on the left.  We will also prevent
        # any partial alignments.

        # set return code 2 if the = is at line level, but
        # set return code 1 if the = is below line level, i.e.
        #  sub new { my ( $p, $v ) = @_; bless \$v, $p }
        #  sub iter { my ($x) = @_; return undef if $$x < 0; return $$x--; }

        elsif ( ( index( $pat_m, ',' ) >= 0 ) ne ( index( $pat, ',' ) >= 0 ) ) {
            $GoToMsg     = "mixed commas/no-commas before equals";
            $return_code = 1;
            if ( $lev eq $group_level ) {
                $return_code = 2;
            }
        }
        else {
            $return_code = 0;
        }
    }
    else {
        $return_code = 0;
    }

    EXPLAIN_COMPARE_PATTERNS
      && $return_code
      && print {*STDOUT} "no match because $GoToMsg\n";

    return $return_code;

} ## end sub compare_patterns

sub fat_comma_to_comma {
    my ($str) = @_;

    # We are changing '=>' to ',' and removing any trailing decimal count
    # because currently fat commas have a count and commas do not.
    # For example, we will change '=>2+{-3.2' into ',2+{-3'
    if ( $str =~ /^=>([^\.]*)/ ) { $str = ',' . $1 }
    return $str;
} ## end sub fat_comma_to_comma

sub get_line_token_info {

    # scan lines of tokens and return summary information about the range of
    # levels and patterns.
    my ($rlines) = @_;

    # First scan to check monotonicity. Here is an example of several
    # lines which are monotonic. The = is the lowest level, and
    # the commas are all one level deeper. So this is not nonmonotonic.
    #  $$d{"weeks"}   = [ "w",  "wk",  "wks", "week", "weeks" ];
    #  $$d{"days"}    = [ "d",  "day", "days" ];
    #  $$d{"hours"}   = [ "h",  "hr",  "hrs", "hour", "hours" ];
    my @all_token_info;
    my $all_monotonic = 1;
    foreach my $jj ( 0 .. @{$rlines} - 1 ) {
        my ($line) = $rlines->[$jj];
        my $rtokens = $line->{'rtokens'};
        my $last_lev;
        my $is_monotonic = 1;
        my $i            = -1;
        foreach my $tok ( @{$rtokens} ) {
            $i++;
            my ( $raw_tok, $lev, $tag, $tok_count ) =
              decode_alignment_token($tok);
            push @{ $all_token_info[$jj] },
              [ $raw_tok, $lev, $tag, $tok_count ];
            last if ( $tok eq '#' );
            if ( $i > 0 && $lev < $last_lev ) { $is_monotonic = 0 }
            $last_lev = $lev;
        }
        if ( !$is_monotonic ) { $all_monotonic = 0 }
    }

    my $rline_values = [];
    foreach my $jj ( 0 .. @{$rlines} - 1 ) {
        my ($line) = $rlines->[$jj];

        my $rtokens = $line->{'rtokens'};
        my $i       = -1;
        my ( $lev_min, $lev_max );
        my $token_pattern_max = EMPTY_STRING;
        my %saw_level;
        my $is_monotonic = 1;

        # find the index of the last token before the side comment
        my $imax      = @{$rtokens} - 2;
        my $imax_true = $imax;

        # If the entire group is monotonic, and the line ends in a comma list,
        # walk it back to the first such comma. this will have the effect of
        # making all trailing ragged comma lists match in the prune tree
        # routine.  these trailing comma lists can better be handled by later
        # alignment rules.

        # Treat fat commas the same as commas here by converting them to
        # commas.  This will improve the chance of aligning the leading parts
        # of ragged lists.

        my $tok_end = fat_comma_to_comma( $rtokens->[$imax] );
        if ( $all_monotonic && $tok_end =~ /^,/ ) {
            my $ii = $imax - 1;
            while ( $ii >= 0
                && fat_comma_to_comma( $rtokens->[$ii] ) eq $tok_end )
            {
                $imax = $ii;
                $ii--;
            }
        }

        # make a first pass to find level range
        my $last_lev;
        foreach my $tok ( @{$rtokens} ) {
            $i++;
            last if ( $i > $imax );
            last if ( $tok eq '#' );
            my ( $raw_tok, $lev, $tag, $tok_count ) =
              @{ $all_token_info[$jj]->[$i] };

            last if ( $tok eq '#' );
            $token_pattern_max .= $tok;
            $saw_level{$lev}++;
            if ( !defined($lev_min) ) {
                $lev_min = $lev;
                $lev_max = $lev;
            }
            else {
                if ( $lev < $lev_min )  { $lev_min      = $lev; }
                if ( $lev > $lev_max )  { $lev_max      = $lev; }
                if ( $lev < $last_lev ) { $is_monotonic = 0 }
            }
            $last_lev = $lev;
        }

        # handle no levels
        my $rtoken_patterns = {};
        my $rtoken_indexes  = {};
        my @levs            = sort { $a <=> $b } keys %saw_level;
        if ( !defined($lev_min) ) {
            $lev_min                     = -1;
            $lev_max                     = -1;
            $levs[0]                     = -1;
            $rtoken_patterns->{$lev_min} = EMPTY_STRING;
            $rtoken_indexes->{$lev_min}  = [];
        }

        # handle one level
        elsif ( $lev_max == $lev_min ) {
            $rtoken_patterns->{$lev_max} = $token_pattern_max;
            $rtoken_indexes->{$lev_max}  = [ ( 0 .. $imax ) ];
        }

        # handle multiple levels
        else {
            $rtoken_patterns->{$lev_max} = $token_pattern_max;
            $rtoken_indexes->{$lev_max}  = [ ( 0 .. $imax ) ];

            my $lev_top = pop @levs;    # already did max level
            my $itok    = -1;
            foreach my $tok ( @{$rtokens} ) {
                $itok++;
                last if ( $itok > $imax );
                my ( $raw_tok, $lev, $tag, $tok_count ) =
                  @{ $all_token_info[$jj]->[$itok] };
                last if ( $raw_tok eq '#' );
                foreach my $lev_test (@levs) {
                    next if ( $lev > $lev_test );
                    $rtoken_patterns->{$lev_test} .= $tok;
                    push @{ $rtoken_indexes->{$lev_test} }, $itok;
                }
            }
            push @levs, $lev_top;
        }

        push @{$rline_values},
          [
            $lev_min,        $lev_max,      $rtoken_patterns, \@levs,
            $rtoken_indexes, $is_monotonic, $imax_true,       $imax,
          ];

        # debug
        0 && do {
            local $LIST_SEPARATOR = ')(';
            print "lev_min=$lev_min, lev_max=$lev_max, levels=(@levs)\n";
            foreach my $key ( sort keys %{$rtoken_patterns} ) {
                print "$key => $rtoken_patterns->{$key}\n";
                print "$key => @{$rtoken_indexes->{$key}}\n";
            }
        };
    } ## end loop over lines
    return ( $rline_values, $all_monotonic );
} ## end sub get_line_token_info

sub prune_alignment_tree {
    my ($rlines) = @_;
    my $jmax = @{$rlines} - 1;
    return if ( $jmax <= 0 );

    # Vertical alignment in perltidy is done as an iterative process.  The
    # starting point is to mark all possible alignment tokens ('=', ',', '=>',
    # etc) for vertical alignment.  Then we have to delete all alignments
    # which, if actually made, would detract from overall alignment.  This
    # is done in several phases of which this is one.

    # In this routine we look at the alignments of a group of lines as a
    # hierarchical tree.  We will 'prune' the tree to limited depths if that
    # will improve overall alignment at the lower depths.
    # For each line we will be looking at its alignment patterns down to
    # different fixed depths. For each depth, we include all lower depths and
    # ignore all higher depths.  We want to see if we can get alignment of a
    # larger group of lines if we ignore alignments at some lower depth.
    # Here is an # example:

    # for (
    #     [ '$var',     sub { join $_, "bar" },            0, "bar" ],
    #     [ 'CONSTANT', sub { join "foo", "bar" },         0, "bar" ],
    #     [ 'CONSTANT', sub { join "foo", "bar", 3 },      1, "barfoo3" ],
    #     [ '$myvar',   sub { my $var; join $var, "bar" }, 0, "bar" ],
    # );

    # In the above example, all lines have three commas at the lowest depth
    # (zero), so if there were no other alignments, these lines would all
    # align considering only the zero depth alignment token.  But some lines
    # have additional comma alignments at the next depth, so we need to decide
    # if we should drop those to keep the top level alignments, or keep those
    # for some additional low level alignments at the expense losing some top
    # level alignments.  In this case we will drop the deeper level commas to
    # keep the entire collection aligned.  But in some cases the decision could
    # go the other way.

    # The tree for this example at the zero depth has one node containing
    # all four lines, since they are identical at zero level (three commas).
    # At depth one, there are three 'children' nodes, namely:
    # - lines 1 and 2, which have a single comma in the 'sub' at depth 1
    # - line 3, which has 2 commas at depth 1
    # - line4, which has a ';' and a ',' at depth 1
    # There are no deeper alignments in this example.
    # so the tree structure for this example is:
    #
    #    depth 0         depth 1      depth 2
    #    [lines 1-4] --  [line 1-2] -  (empty)
    #                 |  [line 3]   -  (empty)
    #                 |  [line 4]   -  (empty)

    # We can carry this to any depth, but it is not really useful to go below
    # depth 2. To cleanly stop there, we will consider depth 2 to contain all
    # alignments at depth >=2.

    use constant EXPLAIN_PRUNE => 0;

    #-------------------------------------------------------------------
    # Prune Tree Step 1. Start by scanning the lines and collecting info
    #-------------------------------------------------------------------

    # Note that the caller had this info but we have to redo this now because
    # alignment tokens may have been deleted.
    my ( $rline_values, $all_monotonic ) = get_line_token_info($rlines);

    # If all the lines have levels which increase monotonically from left to
    # right, then the sweep-left-to-right pass can do a better job of alignment
    # than pruning, and without deleting alignments.
    return if ($all_monotonic);

    # Contents of $rline_values
    #   [
    #     $lev_min,        $lev_max,      $rtoken_patterns, \@levs,
    #     $rtoken_indexes, $is_monotonic, $imax_true,       $imax,
    #   ];

    # We can work to any depth, but there is little advantage to working
    # to a a depth greater than 2
    my $MAX_DEPTH = 2;

    # This arrays will hold the tree of alignment tokens at different depths
    # for these lines.
    my @match_tree;

    # Tree nodes contain these values:
    # $match_tree[$depth] = [$jbeg, $jend, $n_parent, $level, $pattern,
    #                        $nc_beg_p, $nc_end_p, $rindexes];
    # where
    #      $depth = 0,1,2 = index of depth of the match

    #  $jbeg beginning index j of the range of lines in this match
    #  $jend ending index j of the range of lines in this match
    #  $n_parent = index of the containing group at $depth-1, if it exists
    #  $level = actual level of code being matched in this group
    #  $pattern = alignment pattern being matched
    #  $nc_beg_p = first child
    #  $nc_end_p = last child
    #  $rindexes = ref to token indexes

    # the patterns and levels of the current group being formed at each depth
    my ( @token_patterns_current, @levels_current, @token_indexes_current );

    # the patterns and levels of the next line being tested at each depth
    my ( @token_patterns_next, @levels_next, @token_indexes_next );

    #-----------------------------------------------------------
    # define a recursive worker subroutine for tree construction
    #-----------------------------------------------------------

    # This is a recursive routine which is called if a match condition changes
    # at any depth when a new line is encountered.  It ends the match node
    # which changed plus all deeper nodes attached to it.
    my $end_node;
    $end_node = sub {
        my ( $depth, $jl, $n_parent ) = @_;

        # $depth is the tree depth
        # $jl is the  index of the line
        # $n_parent is index of the parent node of this node

        return if ( $depth > $MAX_DEPTH );

        # end any current group at this depth
        if (   $jl >= 0
            && defined( $match_tree[$depth] )
            && @{ $match_tree[$depth] }
            && defined( $levels_current[$depth] ) )
        {
            $match_tree[$depth]->[-1]->[1] = $jl;
        }

        # Define the index of the node we will create below
        my $ng_self = 0;
        if ( defined( $match_tree[$depth] ) ) {
            $ng_self = @{ $match_tree[$depth] };
        }

        # end any next deeper child node(s)
        $end_node->( $depth + 1, $jl, $ng_self );

        # update the levels being matched
        $token_patterns_current[$depth] = $token_patterns_next[$depth];
        $token_indexes_current[$depth]  = $token_indexes_next[$depth];
        $levels_current[$depth]         = $levels_next[$depth];

        # Do not start a new group at this level if it is not being used
        if ( !defined( $levels_next[$depth] )
            || $depth > 0
            && $levels_next[$depth] <= $levels_next[ $depth - 1 ] )
        {
            return;
        }

        # Create a node for the next group at this depth. We initially assume
        # that it will continue to $jmax, and correct that later if the node
        # ends earlier.
        push @{ $match_tree[$depth] },
          [
            $jl + 1, $jmax, $n_parent, $levels_current[$depth],
            $token_patterns_current[$depth],
            undef, undef, $token_indexes_current[$depth],
          ];

        return;
    };    ## end sub end_node

    #-----------------------------------------------------
    # Prune Tree Step 2. Loop to form the tree of matches.
    #-----------------------------------------------------
    foreach my $jp ( 0 .. $jmax ) {

        # working with two adjacent line indexes, 'm'=minus, 'p'=plus
        my $jm = $jp - 1;

        # Pull out needed values for the next line
        my ( $lev_min, $lev_max, $rtoken_patterns, $rlevs, $rtoken_indexes,
            $is_monotonic, $imax_true, $imax )
          = @{ $rline_values->[$jp] };

        # Transfer levels and patterns for this line to the working arrays.
        # If the number of levels differs from our chosen MAX_DEPTH ...
        # if fewer than MAX_DEPTH: leave levels at missing depths undefined
        # if more than MAX_DEPTH: set the MAX_DEPTH level to be the maximum
        @levels_next = @{$rlevs}[ 0 .. $MAX_DEPTH ];
        if ( @{$rlevs} > $MAX_DEPTH ) {
            $levels_next[$MAX_DEPTH] = $rlevs->[-1];
        }
        my $depth = 0;
        foreach my $item (@levels_next) {
            $token_patterns_next[$depth] =
              defined($item) ? $rtoken_patterns->{$item} : undef;
            $token_indexes_next[$depth] =
              defined($item) ? $rtoken_indexes->{$item} : undef;
            $depth++;
        }

        # Look for a change in match groups...

        # Initialize on the first line
        if ( $jp == 0 ) {
            my $n_parent;
            $end_node->( 0, $jm, $n_parent );
        }

        # End groups if a hard flag has been set
        elsif ( $rlines->[$jm]->{'end_group'} ) {
            my $n_parent;
            $end_node->( 0, $jm, $n_parent );
        }

        # Continue at hanging side comment
        elsif ( $rlines->[$jp]->{'is_hanging_side_comment'} ) {
            next;
        }

        # Otherwise see if anything changed and update the tree if so
        else {
            foreach my $dep ( 0 .. $MAX_DEPTH ) {

                my $def_current = defined( $token_patterns_current[$dep] );
                my $def_next    = defined( $token_patterns_next[$dep] );
                last if ( !$def_current && !$def_next );
                if (   !$def_current
                    || !$def_next
                    || $token_patterns_current[$dep] ne
                    $token_patterns_next[$dep] )
                {
                    my $n_parent;
                    if ( $dep > 0 && defined( $match_tree[ $dep - 1 ] ) ) {
                        $n_parent = @{ $match_tree[ $dep - 1 ] } - 1;
                    }
                    $end_node->( $dep, $jm, $n_parent );
                    last;
                }
            }
        }
    } ## end loop to form tree of matches

    #---------------------------------------------------------
    # Prune Tree Step 3. Make links from parent to child nodes
    #---------------------------------------------------------

    # It seemed cleaner to do this as a separate step rather than during tree
    # construction.  The children nodes have links up to the parent node which
    # created them.  Now make links in the opposite direction, so the parents
    # can find the children.  We store the range of children nodes ($nc_beg,
    # $nc_end) of each parent with two additional indexes in the original array.
    # These will be undef if no children.
    foreach my $depth ( reverse( 1 .. $MAX_DEPTH ) ) {
        next unless defined( $match_tree[$depth] );
        my $nc_max = @{ $match_tree[$depth] } - 1;
        my $np_now;
        foreach my $nc ( 0 .. $nc_max ) {
            my $np = $match_tree[$depth]->[$nc]->[2];
            if ( !defined($np) ) {

                # shouldn't happen
                #print STDERR "lost child $np at depth $depth\n";
                next;
            }
            if ( !defined($np_now) || $np != $np_now ) {
                $np_now = $np;
                $match_tree[ $depth - 1 ]->[$np]->[5] = $nc;
            }
            $match_tree[ $depth - 1 ]->[$np]->[6] = $nc;
        }
    } ## end loop to make links down to the child nodes

    EXPLAIN_PRUNE > 0 && do {
        print "Tree complete. Found these groups:\n";
        foreach my $depth ( 0 .. $MAX_DEPTH ) {
            Dump_tree_groups( \@{ $match_tree[$depth] }, "depth=$depth" );
        }
    };

    #------------------------------------------------------
    # Prune Tree Step 4. Make a list of nodes to be deleted
    #------------------------------------------------------

    #  list of lines with tokens to be deleted:
    #  [$jbeg, $jend, $level_keep]
    #  $jbeg..$jend is the range of line indexes,
    #  $level_keep is the minimum level to keep
    my @delete_list;

    # We work with a list of nodes to visit at the next deeper depth.
    my @todo_list;
    if ( defined( $match_tree[0] ) ) {
        @todo_list = ( 0 .. @{ $match_tree[0] } - 1 );
    }

    foreach my $depth ( 0 .. $MAX_DEPTH ) {
        last if ( !@todo_list );
        my @todo_next;
        foreach my $np (@todo_list) {
            my ( $jbeg_p, $jend_p, $np_p, $lev_p, $pat_p, $nc_beg_p, $nc_end_p,
                $rindexes_p )
              = @{ $match_tree[$depth]->[$np] };
            my $nlines_p = $jend_p - $jbeg_p + 1;

            # nothing to do if no children
            next unless defined($nc_beg_p);

            # Define the number of lines to either keep or delete a child node.
            # This is the key decision we have to make.  We want to delete
            # short runs of matched lines, and keep long runs.  It seems easier
            # for the eye to follow breaks in monotonic level changes than
            # non-monotonic level changes.  For example, the following looks
            # best if we delete the lower level alignments:

            #  [1]                  ~~ [];
            #  [ ["foo"], ["bar"] ] ~~ [ qr/o/, qr/a/ ];
            #  [ qr/o/, qr/a/ ]     ~~ [ ["foo"], ["bar"] ];
            #  [ "foo", "bar" ]     ~~ [ qr/o/, qr/a/ ];
            #  [ qr/o/, qr/a/ ]     ~~ [ "foo", "bar" ];
            #  $deep1               ~~ $deep1;

            # So we will use two thresholds.
            my $nmin_mono     = $depth + 2;
            my $nmin_non_mono = $depth + 6;
            if ( $nmin_mono > $nlines_p - 1 ) {
                $nmin_mono = $nlines_p - 1;
            }
            if ( $nmin_non_mono > $nlines_p - 1 ) {
                $nmin_non_mono = $nlines_p - 1;
            }

            # loop to keep or delete each child node
            foreach my $nc ( $nc_beg_p .. $nc_end_p ) {
                my ( $jbeg_c, $jend_c, $np_c, $lev_c, $pat_c, $nc_beg_c,
                    $nc_end_c )
                  = @{ $match_tree[ $depth + 1 ]->[$nc] };
                my $nlines_c     = $jend_c - $jbeg_c + 1;
                my $is_monotonic = $rline_values->[$jbeg_c]->[5];
                my $nmin         = $is_monotonic ? $nmin_mono : $nmin_non_mono;
                if ( $nlines_c < $nmin ) {
##print "deleting child, nlines=$nlines_c, nmin=$nmin\n";
                    push @delete_list, [ $jbeg_c, $jend_c, $lev_p ];
                }
                else {
##print "keeping child, nlines=$nlines_c, nmin=$nmin\n";
                    push @todo_next, $nc;
                }
            }
        }
        @todo_list = @todo_next;
    } ## end loop to mark nodes to delete

    #------------------------------------------------------------
    # Prune Tree Step 5. Loop to delete selected alignment tokens
    #------------------------------------------------------------
    foreach my $item (@delete_list) {
        my ( $jbeg, $jend, $level_keep ) = @{$item};
        foreach my $jj ( $jbeg .. $jend ) {
            my $line = $rlines->[$jj];
            my @idel;
            my $rtokens = $line->{'rtokens'};
            my $imax    = @{$rtokens} - 2;
            foreach my $i ( 0 .. $imax ) {
                my $tok = $rtokens->[$i];
                my ( $raw_tok, $lev, $tag, $tok_count ) =
                  decode_alignment_token($tok);
                if ( $lev > $level_keep ) {
                    push @idel, $i;
                }
            }
            if (@idel) {
                delete_selected_tokens( $line, \@idel );
            }
        }
    } ## end loop to delete selected alignment tokens

    return;
} ## end sub prune_alignment_tree

sub Dump_tree_groups {
    my ( $rgroup, $msg ) = @_;

    # Debug routine
    print "$msg\n";
    local $LIST_SEPARATOR = ')(';
    foreach my $item ( @{$rgroup} ) {
        my @fix = @{$item};
        foreach my $val (@fix) { $val = "undef" unless defined($val); }
        $fix[4] = "...";
        print "(@fix)\n";
    }
    return;
} ## end sub Dump_tree_groups

{    ## closure for sub is_marginal_match

    my %is_if_or;
    my %is_assignment;
    my %is_good_alignment;

    # This test did not give sufficiently better results to use as an update,
    # but the flag is worth keeping as a starting point for future testing.
    use constant TEST_MARGINAL_EQ_ALIGNMENT => 0;

    BEGIN {

        my @q = qw(
          if unless or ||
        );
        @is_if_or{@q} = (1) x scalar(@q);

        @q = qw(
          = **= += *= &= <<= &&=
          -= /= |= >>= ||= //=
          .= %= ^=
          x=
        );
        @is_assignment{@q} = (1) x scalar(@q);

        # Vertically aligning on certain "good" tokens is usually okay
        # so we can be less restrictive in marginal cases.
        @q = qw( { ? => = );
        push @q, (',');
        @is_good_alignment{@q} = (1) x scalar(@q);
    } ## end BEGIN

    sub is_marginal_match {

        my ( $line_0, $line_1, $group_level, $imax_align, $imax_prev ) = @_;

        # Decide if we should undo some or all of the common alignments of a
        # group of just two lines.

        # Given:
        #   $line_0 and $line_1 - the two lines
        #   $group_level = the indentation level of the group being processed
        #   $imax_align = the maximum index of the common alignment tokens
        #                 of the two lines
        #   $imax_prev  = the maximum index of the common alignment tokens
        #                 with the line before $line_0 (=-1 of does not exist)

        # Return:
        #   $is_marginal = true if the two lines should NOT be fully aligned
        #                = false if the two lines can remain fully aligned
        #   $imax_align  = the index of the highest alignment token shared by
        #                  these two lines to keep if the match is marginal.

        # When we have an alignment group of just two lines like this, we are
        # working in the twilight zone of what looks good and what looks bad.
        # This routine is a collection of rules which work have been found to
        # work fairly well, but it will need to be updated from time to time.

        my $is_marginal = 0;

        #---------------------------------------
        # Always align certain special cases ...
        #---------------------------------------
        if (

            # always keep alignments of a terminal else or ternary
            defined( $line_1->{'j_terminal_match'} )

            # always align lists
            || $line_0->{'list_type'}

            # always align hanging side comments
            || $line_1->{'is_hanging_side_comment'}

          )
        {
            return ( $is_marginal, $imax_align );
        }

        my $jmax_0           = $line_0->{'jmax'};
        my $jmax_1           = $line_1->{'jmax'};
        my $rtokens_1        = $line_1->{'rtokens'};
        my $rtokens_0        = $line_0->{'rtokens'};
        my $rfield_lengths_0 = $line_0->{'rfield_lengths'};
        my $rfield_lengths_1 = $line_1->{'rfield_lengths'};
        my $rpatterns_0      = $line_0->{'rpatterns'};
        my $rpatterns_1      = $line_1->{'rpatterns'};
        my $imax_next        = $line_1->{'imax_pair'};

        # We will scan the alignment tokens and set a flag '$is_marginal' if
        # it seems that the an alignment would look bad.
        my $max_pad            = 0;
        my $saw_good_alignment = 0;
        my $saw_if_or;                # if we saw an 'if' or 'or' at group level
        my $raw_tokb = EMPTY_STRING;  # first token seen at group level
        my $jfirst_bad;
        my $line_ending_fat_comma;    # is last token just a '=>' ?
        my $j0_eq_pad;
        my $j0_max_pad = 0;

        foreach my $j ( 0 .. $jmax_1 - 2 ) {
            my ( $raw_tok, $lev, $tag, $tok_count ) =
              decode_alignment_token( $rtokens_1->[$j] );
            if ( $raw_tok && $lev == $group_level ) {
                if ( !$raw_tokb ) { $raw_tokb = $raw_tok }
                $saw_if_or ||= $is_if_or{$raw_tok};
            }

            # When the first of the two lines ends in a bare '=>' this will
            # probably be marginal match.  (For a bare =>, the next field length
            # will be 2 or 3, depending on side comment)
            $line_ending_fat_comma =
                 $j == $jmax_1 - 2
              && $raw_tok eq '=>'
              && $rfield_lengths_0->[ $j + 1 ] <= 3;

            my $pad = $rfield_lengths_1->[$j] - $rfield_lengths_0->[$j];
            if ( $j == 0 ) {
                $pad += $line_1->{'leading_space_count'} -
                  $line_0->{'leading_space_count'};

                # Remember the pad at a leading equals
                if ( $raw_tok eq '=' && $lev == $group_level ) {
                    $j0_eq_pad = $pad;
                    $j0_max_pad =
                      0.5 * ( $rfield_lengths_1->[0] + $rfield_lengths_0->[0] );
                    $j0_max_pad = 4 if ( $j0_max_pad < 4 );
                }
            }

            if ( $pad < 0 )        { $pad     = -$pad }
            if ( $pad > $max_pad ) { $max_pad = $pad }
            if ( $is_good_alignment{$raw_tok} && !$line_ending_fat_comma ) {
                $saw_good_alignment = 1;
            }
            else {
                $jfirst_bad = $j unless defined($jfirst_bad);
            }
            my $pat_0 = $rpatterns_0->[$j];
            my $pat_1 = $rpatterns_1->[$j];
            if ( $pat_0 ne $pat_1 && length($pat_0) eq length($pat_1) ) {
                $pat_0 =~ tr/n/Q/;
                $pat_1 =~ tr/n/Q/;
            }
            if ( $pat_0 ne $pat_1 ) {

                # Flag this as a marginal match since patterns differ.
                # Normally, we will not allow just two lines to match if
                # marginal. But we can allow matching in some specific cases.

                $jfirst_bad  = $j if ( !defined($jfirst_bad) );
                $is_marginal = 1  if ( $is_marginal == 0 );
                if ( $raw_tok eq '=' ) {

                    # Here is an example of a marginal match:
                    #       $done{$$op} = 1;
                    #       $op         = compile_bblock($op);
                    # The left tokens are both identifiers, but
                    # one accesses a hash and the other doesn't.
                    # We'll let this be a tentative match and undo
                    # it later if we don't find more than 2 lines
                    # in the group.
                    $is_marginal = 2;
                }
            }
        }

        $is_marginal = 1 if ( $is_marginal == 0 && $line_ending_fat_comma );

        # Turn off the "marginal match" flag in some cases...
        # A "marginal match" occurs when the alignment tokens agree
        # but there are differences in the other tokens (patterns).
        # If we leave the marginal match flag set, then the rule is that we
        # will align only if there are more than two lines in the group.
        # We will turn of the flag if we almost have a match
        # and either we have seen a good alignment token or we
        # just need a small pad (2 spaces) to fit.  These rules are
        # the result of experimentation.  Tokens which misaligned by just
        # one or two characters are annoying.  On the other hand,
        # large gaps to less important alignment tokens are also annoying.
        if ( $is_marginal == 1
            && ( $saw_good_alignment || $max_pad < 3 ) )
        {
            $is_marginal = 0;
        }

        # We will use the line endings to help decide on alignments...
        # See if the lines end with semicolons...
        my $sc_term0;
        my $sc_term1;
        if ( $jmax_0 < 1 || $jmax_1 < 1 ) {

            # shouldn't happen
        }
        else {
            my $pat0 = $rpatterns_0->[ $jmax_0 - 1 ];
            my $pat1 = $rpatterns_1->[ $jmax_1 - 1 ];
            $sc_term0 = $pat0 =~ /;b?$/;
            $sc_term1 = $pat1 =~ /;b?$/;
        }

        if ( !$is_marginal && !$sc_term0 ) {

            # First line of assignment should be semicolon terminated.
            # For example, do not align here:
            #  $$href{-NUM_TEXT_FILES} = $$href{-NUM_BINARY_FILES} =
            #    $$href{-NUM_DIRS} = 0;
            if ( $is_assignment{$raw_tokb} ) {
                $is_marginal = 1;
            }
        }

        # Try to avoid some undesirable alignments of opening tokens
        # for example, the space between grep and { here:
        #  return map { ( $_ => $_ ) }
        #    grep     { /$handles/ } $self->_get_delegate_method_list;
        $is_marginal ||=
             ( $raw_tokb eq '(' || $raw_tokb eq '{' )
          && $jmax_1 == 2
          && $sc_term0 ne $sc_term1;

        #---------------------------------------
        # return if this is not a marginal match
        #---------------------------------------
        if ( !$is_marginal ) {
            return ( $is_marginal, $imax_align );
        }

        # Undo the marginal match flag in certain cases,

        # Two lines with a leading equals-like operator are allowed to
        # align if the patterns to the left of the equals are the same.
        # For example the following two lines are a marginal match but have
        # the same left side patterns, so we will align the equals.
        #     my $orig = my $format = "^<<<<< ~~\n";
        #     my $abc  = "abc";
        # But these have a different left pattern so they will not be
        # aligned
        #     $xmldoc .= $`;
        #     $self->{'leftovers'} .= "<bx-seq:seq" . $';

        # First line semicolon terminated but second not, usually ok:
        #               my $want = "'ab', 'a', 'b'";
        #               my $got  = join( ", ",
        #                    map { defined($_) ? "'$_'" : "undef" }
        #                          @got );
        #  First line not semicolon terminated, Not OK to match:
        #   $$href{-NUM_TEXT_FILES} = $$href{-NUM_BINARY_FILES} =
        #      $$href{-NUM_DIRS} = 0;
        my $pat0 = $rpatterns_0->[0];
        my $pat1 = $rpatterns_1->[0];

        #---------------------------------------------------------
        # Turn off the marginal flag for some types of assignments
        #---------------------------------------------------------
        if ( $is_assignment{$raw_tokb} ) {

            # undo marginal flag if first line is semicolon terminated
            # and leading patters match
            if ($sc_term0) {    # && $sc_term1) {
                $is_marginal = $pat0 ne $pat1;
            }
        }
        elsif ( $raw_tokb eq '=>' ) {

            # undo marginal flag if patterns match
            $is_marginal = $pat0 ne $pat1 || $line_ending_fat_comma;
        }
        elsif ( $raw_tokb eq '=~' ) {

            # undo marginal flag if both lines are semicolon terminated
            # and leading patters match
            if ( $sc_term1 && $sc_term0 ) {
                $is_marginal = $pat0 ne $pat1;
            }
        }
        else {
            ##ok: (none of the above)
        }

        #-----------------------------------------------------
        # Turn off the marginal flag if we saw an 'if' or 'or'
        #-----------------------------------------------------

        # A trailing 'if' and 'or' often gives a good alignment
        # For example, we can align these:
        #  return -1     if $_[0] =~ m/^CHAPT|APPENDIX/;
        #  return $1 + 0 if $_[0] =~ m/^SECT(\d*)$/;

        # or
        #  $d_in_m[2] = 29          if ( &Date_LeapYear($y) );
        #  $d         = $d_in_m[$m] if ( $d > $d_in_m[$m] );

        if ($saw_if_or) {

            # undo marginal flag if both lines are semicolon terminated
            if ( $sc_term0 && $sc_term1 ) {
                $is_marginal = 0;
            }
        }

        # For a marginal match, only keep matches before the first 'bad' match
        if (   $is_marginal
            && defined($jfirst_bad)
            && $imax_align > $jfirst_bad - 1 )
        {
            $imax_align = $jfirst_bad - 1;
        }

        #----------------------------------------------------------
        # Allow sweep to match lines with leading '=' in some cases
        #----------------------------------------------------------
        if ( $imax_align < 0 && defined($j0_eq_pad) ) {

            if (

                # If there is a following line with leading equals, or
                # preceding line with leading equals, then let the sweep align
                # them without restriction.  For example, the first two lines
                # here are a marginal match, but they are followed by a line
                # with leading equals, so the sweep-lr logic can align all of
                # the lines:

                #  $date[1] = $month_to_num{ $date[1] };            # <--line_0
                #  @xdate   = split( /[:\/\s]/, $log->field('t') ); # <--line_1
                #  $day     = sprintf( "%04d/%02d/%02d", @date[ 2, 1, 0 ] );
                #  $time    = sprintf( "%02d:%02d:%02d", @date[ 3 .. 5 ] );

                # Likewise, if we reverse the two pairs we want the same result

                #  $day     = sprintf( "%04d/%02d/%02d", @date[ 2, 1, 0 ] );
                #  $time    = sprintf( "%02d:%02d:%02d", @date[ 3 .. 5 ] );
                #  $date[1] = $month_to_num{ $date[1] };            # <--line_0
                #  @xdate   = split( /[:\/\s]/, $log->field('t') ); # <--line_1

                (
                       $imax_next >= 0
                    || $imax_prev >= 0
                    || TEST_MARGINAL_EQ_ALIGNMENT
                )
                && $j0_eq_pad >= -$j0_max_pad
                && $j0_eq_pad <= $j0_max_pad
              )
            {

                # But do not do this if there is a comma before the '='.
                # For example, the first two lines below have commas and
                # therefore are not allowed to align with lines 3 & 4:

                # my ( $x, $y ) = $self->Size();                      #<--line_0
                # my ( $left, $top, $right, $bottom ) = $self->Window(); #<--l_1
                # my $vx = $right - $left;
                # my $vy = $bottom - $top;

                if ( $rpatterns_0->[0] !~ /,/ && $rpatterns_1->[0] !~ /,/ ) {
                    $imax_align = 0;
                }
            }
        }

        return ( $is_marginal, $imax_align );
    } ## end sub is_marginal_match
} ## end closure for sub is_marginal_match

sub get_extra_leading_spaces {

    my ( $rlines, $rgroups ) = @_;

    #----------------------------------------------------------
    # Define any extra indentation space (for the -lp option).
    # Here is why:
    # If a list has side comments, sub scan_list must dump the
    # list before it sees everything.  When this happens, it sets
    # the indentation to the standard scheme, but notes how
    # many spaces it would have liked to use.  We may be able
    # to recover that space here in the event that all of the
    # lines of a list are back together again.
    #----------------------------------------------------------

    return 0 if ( !@{$rlines} || !@{$rgroups} );

    my $object = $rlines->[0]->{'indentation'};
    return 0 if ( !ref($object) );
    my $extra_leading_spaces            = 0;
    my $extra_indentation_spaces_wanted = get_recoverable_spaces($object);
    return ($extra_leading_spaces) if ( !$extra_indentation_spaces_wanted );

    my $min_spaces = $extra_indentation_spaces_wanted;
    if ( $min_spaces > 0 ) { $min_spaces = 0 }

    # loop over all groups
    my $ng      = -1;
    my $ngroups = @{$rgroups};
    foreach my $item ( @{$rgroups} ) {
        $ng++;
        my ( $jbeg, $jend ) = @{$item};
        foreach my $j ( $jbeg .. $jend ) {
            next if ( $j == 0 );

            # all indentation objects must be the same
            if ( $object != $rlines->[$j]->{'indentation'} ) {
                return 0;
            }
        }

       # find the maximum space without exceeding the line length for this group
        my $avail = $rlines->[$jbeg]->get_available_space_on_right();
        my $spaces =
          ( $avail > $extra_indentation_spaces_wanted )
          ? $extra_indentation_spaces_wanted
          : $avail;

        #--------------------------------------------------------
        # Note: min spaces can be negative; for example with -gnu
        # f(
        #   do { 1; !!(my $x = bless []); }
        #  );
        #--------------------------------------------------------
        # The following rule is needed to match older formatting:
        # For multiple groups, we will keep spaces non-negative.
        # For a single group, we will allow a negative space.
        if ( $ngroups > 1 && $spaces < 0 ) { $spaces = 0 }

        # update the minimum spacing
        if ( $ng == 0 || $spaces < $extra_leading_spaces ) {
            $extra_leading_spaces = $spaces;
        }
    }

    # update the indentation object because with -icp the terminal
    # ');' will use the same adjustment.
    $object->permanently_decrease_available_spaces( -$extra_leading_spaces );
    return $extra_leading_spaces;
} ## end sub get_extra_leading_spaces

sub forget_side_comment {
    my ($self) = @_;
    $self->[_last_side_comment_column_] = 0;
    return;
}

sub is_good_side_comment_column {
    my ( $self, $line, $line_number, $level, $num5 ) = @_;

    # Upon encountering the first side comment of a group, decide if
    # a previous side comment should be forgotten.  This involves
    # checking several rules.

    # Return true to KEEP old comment location
    # Return false to FORGET old comment location
    my $KEEP   = 1;
    my $FORGET = 0;

    my $rfields                 = $line->{'rfields'};
    my $is_hanging_side_comment = $line->{'is_hanging_side_comment'};

    # RULE1: Never forget comment before a hanging side comment
    return $KEEP if ($is_hanging_side_comment);

    # RULE2: Forget a side comment after a short line difference,
    # where 'short line difference' is computed from a formula.
    # Using a smooth formula helps minimize sudden large changes.
    my $line_diff = $line_number - $self->[_last_side_comment_line_number_];
    my $alev_diff = abs( $level - $self->[_last_side_comment_level_] );

    # '$num5' is the number of comments in the first 5 lines after the first
    # comment.  It is needed to keep a compact group of side comments from
    # being influenced by a more distant side comment.
    $num5 = 1 if ( !$num5 );

    # Some values:

    #        $adiff  $num5   $short_diff
    #        0       *       12
    #        1       1       6
    #        1       2       4
    #        1       3       3
    #        1       4       2
    #        2       1       4
    #        2       2       2
    #        2       3       1
    #        3       1       3
    #        3       2       1

    my $short_diff = SC_LONG_LINE_DIFF / ( 1 + $alev_diff * $num5 );

    return $FORGET
      if ( $line_diff > $short_diff
        || !$rOpts_valign_side_comments );

    # RULE3: Forget a side comment if this line is at lower level and
    # ends a block
    my $last_sc_level = $self->[_last_side_comment_level_];
    return $FORGET
      if ( $level < $last_sc_level
        && $is_closing_block_type{ substr( $rfields->[0], 0, 1 ) } );

    # RULE 4: Forget the last side comment if this comment might join a cached
    # line ...
    if ( my $cached_line_type = get_cached_line_type() ) {

        # ... otherwise side comment alignment will get messed up.
        # For example, in the following test script
        # with using 'perltidy -sct -act=2', the last comment would try to
        # align with the previous and then be in the wrong column when
        # the lines are combined:

        # foreach $line (
        #    [0, 1, 2], [3, 4, 5], [6, 7, 8],    # rows
        #    [0, 3, 6], [1, 4, 7], [2, 5, 8],    # columns
        #    [0, 4, 8], [2, 4, 6]
        #  )                                     # diagonals
        return $FORGET
          if ( $cached_line_type == 2 || $cached_line_type == 4 );
    }

    # Otherwise, keep it alive
    return $KEEP;
} ## end sub is_good_side_comment_column

sub align_side_comments {

    my ( $self, $rlines, $rgroups ) = @_;

    # Align any side comments in this batch of lines

    # Given:
    #  $rlines  - the lines
    #  $rgroups - the partition of the lines into groups
    #
    # We will be working group-by-group because all side comments
    # (real or fake) in each group are already aligned. So we just have
    # to make alignments between groups wherever possible.

    # An unusual aspect is that within each group we have aligned both real
    # and fake side comments.  This has the consequence that the lengths of
    # long lines without real side comments can cause 'push' all side comments
    # to the right.  This seems unusual, but testing with and without this
    # feature shows that it is usually better this way.  Otherwise, side
    # comments can be hidden between long lines without side comments and
    # thus be harder to read.

    my $group_level        = $self->[_group_level_];
    my $continuing_sc_flow = $self->[_last_side_comment_length_] > 0
      && $group_level == $self->[_last_level_written_];

    # Find groups with side comments, and remember the first nonblank comment
    my $j_sc_beg;
    my @todo;
    my $ng = -1;
    foreach my $item ( @{$rgroups} ) {
        $ng++;
        my ( $jbeg, $jend ) = @{$item};
        foreach my $j ( $jbeg .. $jend ) {
            my $line = $rlines->[$j];
            my $jmax = $line->{'jmax'};
            if ( $line->{'rfield_lengths'}->[$jmax] ) {

                # this group has a line with a side comment
                push @todo, $ng;
                if ( !defined($j_sc_beg) ) {
                    $j_sc_beg = $j;
                }
                last;
            }
        }
    }

    # done if no groups with side comments
    return unless @todo;

    # Count $num5 = number of comments in the 5 lines after the first comment
    # This is an important factor in a decision formula
    my $num5 = 1;
    foreach my $jj ( $j_sc_beg + 1 .. @{$rlines} - 1 ) {
        my $ldiff = $jj - $j_sc_beg;
        last if ( $ldiff > 5 );
        my $line   = $rlines->[$jj];
        my $jmax   = $line->{'jmax'};
        my $sc_len = $line->{'rfield_lengths'}->[$jmax];
        next if ( !$sc_len );
        $num5++;
    }

    # Forget the old side comment location if necessary
    my $line_0 = $rlines->[$j_sc_beg];
    my $lnum =
      $j_sc_beg + $self->[_file_writer_object_]->get_output_line_number();
    my $keep_it =
      $self->is_good_side_comment_column( $line_0, $lnum, $group_level, $num5 );
    my $last_side_comment_column =
      $keep_it ? $self->[_last_side_comment_column_] : 0;

    # If there are multiple groups we will do two passes
    # so that we can find a common alignment for all groups.
    my $MAX_PASS = @todo > 1 ? 2 : 1;

    # Loop over passes
    my $max_comment_column = $last_side_comment_column;
    foreach my $PASS ( 1 .. $MAX_PASS ) {

        # If there are two passes, then on the last pass make the old column
        # equal to the largest of the group.  This will result in the comments
        # being aligned if possible.
        if ( $PASS == $MAX_PASS ) {
            $last_side_comment_column = $max_comment_column;
        }

        # Loop over the groups with side comments
        my $column_limit;
        foreach my $ngr (@todo) {
            my ( $jbeg, $jend ) = @{ $rgroups->[$ngr] };

            # Note that since all lines in a group have common alignments, we
            # just have to work on one of the lines (the first line).
            my $line                    = $rlines->[$jbeg];
            my $jmax                    = $line->{'jmax'};
            my $is_hanging_side_comment = $line->{'is_hanging_side_comment'};
            last
              if ( $PASS < $MAX_PASS && $is_hanging_side_comment );

            # the maximum space without exceeding the line length:
            my $avail = $line->get_available_space_on_right();

            # try to use the previous comment column
            my $side_comment_column = $line->get_column( $jmax - 1 );
            my $move = $last_side_comment_column - $side_comment_column;

            # Remember the maximum possible column of the first line with
            # side comment
            if ( !defined($column_limit) ) {
                $column_limit = $side_comment_column + $avail;
            }

            next if ( $jmax <= 0 );

            # but if this doesn't work, give up and use the minimum space
            my $min_move = $rOpts_minimum_space_to_comment - 1;
            if ( $move > $avail ) {
                $move = $min_move;
            }

            # but we want some minimum space to the comment
            if (   $move >= 0
                && $j_sc_beg == 0
                && $continuing_sc_flow )
            {
                $min_move = 0;
            }

            # remove constraints on hanging side comments
            if ($is_hanging_side_comment) { $min_move = 0 }

            if ( $move < $min_move ) {
                $move = $min_move;
            }

            # don't exceed the available space
            if ( $move > $avail ) { $move = $avail }

            # We can only increase space, never decrease.
            if ( $move < 0 ) { $move = 0 }

            # Discover the largest column on the preliminary  pass
            if ( $PASS < $MAX_PASS ) {
                my $col = $line->get_column( $jmax - 1 ) + $move;

                # but ignore columns too large for the starting line
                if ( $col > $max_comment_column && $col < $column_limit ) {
                    $max_comment_column = $col;
                }
            }

            # Make the changes on the final pass
            else {
                $line->increase_field_width( $jmax - 1, $move );

                # remember this column for the next group
                $last_side_comment_column = $line->get_column( $jmax - 1 );
            }
        } ## end loop over groups
    } ## end loop over passes

    # Find the last side comment
    my $j_sc_last;
    my $ng_last = $todo[-1];
    my ( $jbeg, $jend ) = @{ $rgroups->[$ng_last] };
    foreach my $jj ( reverse( $jbeg .. $jend ) ) {
        my $line = $rlines->[$jj];
        my $jmax = $line->{'jmax'};
        if ( $line->{'rfield_lengths'}->[$jmax] ) {
            $j_sc_last = $jj;
            last;
        }
    }

    # Save final side comment info for possible use by the next batch
    if ( defined($j_sc_last) ) {
        my $line_number =
          $self->[_file_writer_object_]->get_output_line_number() + $j_sc_last;
        $self->[_last_side_comment_column_]      = $last_side_comment_column;
        $self->[_last_side_comment_line_number_] = $line_number;
        $self->[_last_side_comment_level_]       = $group_level;
    }
    return;
} ## end sub align_side_comments

###########################################
# CODE SECTION 6: Pad Signed Number Columns
###########################################

use constant DEBUG_VSN => 0;

my %is_digit_char;
my %is_plus_or_minus;
my %is_leading_sign_pattern;
my %is_opening_token;

BEGIN {

    # PATTERNS: A pattern is basically the concatenation of all token types in
    # the field, with keywords converted to their actual text.  The formatter
    # has changed things like 'print' to 'priNt' so that all 'n's are numbers.
    # The following patterns 'n' can match a signed number of interest.
    # Thus 'n'=a signed or unsigned number, 'b'=a space, '}'=one of ) ] }
    my @q = ( 'n,', 'n,b', 'nb', 'nb}', 'nb},', 'n},', 'n};' );

    @is_leading_sign_pattern{@q} = (1) x scalar(@q);

    @q = qw( 0 1 2 3 4 5 6 7 8 9 );
    @is_digit_char{@q} = (1) x scalar(@q);

    @q = qw( + - );
    @is_plus_or_minus{@q} = (1) x scalar(@q);

    @q = qw< { ( [ >;
    @is_opening_token{@q} = (1) x scalar(@q);
}

sub min_max_median {
    my ($rvalues) = @_;

    # Given:  $rvalues = ref to an array of numbers
    # Return: the min, max, and median
    my $num = @{$rvalues};
    return unless ($num);

    my @sorted = sort { $a <=> $b } @{$rvalues};

    my $min  = $sorted[0];
    my $max  = $sorted[-1];
    my $imid = int $num / 2;
    my $median =
        @sorted % 2
      ? $sorted[$imid]
      : ( $sorted[ $imid - 1 ] + $sorted[$imid] ) / 2;

    return ( $min, $max, $median );
} ## end sub min_max_median

sub end_signed_number_column {
    my ( $rgroup_lines, $rcol_hash, $ix_last ) = @_;

    # Finish formatting a column of unsigned numbers
    # Given:
    #   $rgroup_lines - the current vertical aligment group of lines
    #   $rcol_hash    - a hash of information about this vertical column
    #   $ix_last      - index of the last line of this vertical column
    # Task:
    #   If this is a mixture of signed and unsigned numbers, then add a
    #   single space before the unsigned numbers to improve appearance.
    return unless ($rcol_hash);
    my $jcol          = $rcol_hash->{jcol};
    my $unsigned      = $rcol_hash->{unsigned_count};
    my $signed        = $rcol_hash->{signed_count};
    my $rsigned_lines = $rcol_hash->{rsigned_lines};

    if ( !$signed && $unsigned ) {
        DEVEL_MODE
          && Fault("avoid calling without mixed signed and unsigned\n");
        return;
    }

    my $pos_start_number = $rcol_hash->{pos_start_number};
    my $char_end_part1   = $rcol_hash->{char_end_part1};
    my $ix_first         = $rcol_hash->{ix_first};
    my $nlines           = $ix_last - $ix_first + 1;

    # check for skipped lines, shouldn't happen
    if ( $signed + $unsigned != $nlines ) {
        my $line    = $rgroup_lines->[$ix_last];
        my $rfields = $line->{'rfields'};
        my $text    = join EMPTY_STRING, @{$rfields};
        DEVEL_MODE && Fault(<<EOM);
We seem to have miscounted lines, please check:
signed=$signed
j=$jcol
unsigned=$unsigned
ix_first=$ix_first
ix_last=$ix_last
nlines=$nlines
text=$text
EOM
        return;
    }

    #-----------------------------------------------------------------
    # Form groups of unsigned numbers from the list of signed numbers.
    #-----------------------------------------------------------------
    my @unsigned_subgroups;
    my $ix_last_negative = $ix_first - 1;
    my %is_signed;
    foreach my $ix ( @{$rsigned_lines} ) {
        $is_signed{$ix} = 1;
        my $Nu = $ix - $ix_last_negative - 1;
        if ( $Nu > 0 && $Nu <= $rOpts_valign_signed_numbers_limit ) {
            push @unsigned_subgroups, [ $ix_last_negative + 1, $ix - 1 ];
        }
        $ix_last_negative = $ix;
    }

    # Exclude groups with more than about 20 consecutive numbers.  Little
    # visual improvement is gained by padding more than this, and this avoids
    # large numbers of differences in a file when a single line is changed.
    my $Nu = $ix_last - $ix_last_negative;
    if ( $Nu > 0 && $Nu <= $rOpts_valign_signed_numbers_limit ) {
        push @unsigned_subgroups, [ $ix_last_negative + 1, $ix_last ];
    }

    if ( !@unsigned_subgroups ) { return }    # shouldn't happen

    #--------------------------------------------
    # Find number lengths for irregularity checks
    #--------------------------------------------
    # Padding signed numbers looks best when the numbers, excluding signs,
    # all have about the same length. When the lengths are irregular, with
    # mostly longer unsigned numbers, it doesn't look good to do this. So
    # we need to filter out these bad-looking cases.

    # The 'field_lengths' are unreliable because they may include some
    # arbitrary trailing text; see 'substr.t' So we must look for the end of
    # the number at a space, comma, or closing container token. Note that these
    # lengths include the length of any signs.
    my @len_unsigned;
    my @len_signed;
    my @lengths;
    foreach my $ix ( $ix_first .. $ix_last ) {
        my $line   = $rgroup_lines->[$ix];
        my $rfield = $line->{'rfields'};
        my $str    = substr( $rfield->[$jcol], $pos_start_number );
        if ( $str =~ /^([^\s\,\)\]\}]*)/ ) { $str = $1 }
        my $len = length($str);
        if   ( $is_signed{$ix} ) { push @len_signed,   $len }
        else                     { push @len_unsigned, $len }
        push @lengths, [ $len, $ix ];
    }

    my ( $min_unsigned_length, $max_unsigned_length, $median_unsigned_length )
      = min_max_median( \@len_unsigned );
    my ( $min_signed_length, $max_signed_length, $median_signed_length ) =
      min_max_median( \@len_signed );

    # Skip padding if no signed numbers exceed unsigned numbers in length
    if ( $max_signed_length <= $min_unsigned_length ) {
        return;
    }

    # If max signed length is greatest - all unsigned values can be padded
    elsif ( $max_signed_length > $max_unsigned_length ) {

        # Example:
        #    %wind_dir = (
        #        'n'  => [  1,  0 ],
        #        'ne' => [  1,  1 ],
        #        'e'  => [  0,  1 ],
        #        'se' => [ -1,  1 ],
        #        's'  => [ -1,  0 ],
        #        'sw' => [ -1, -1 ],
        #        'w'  => [  0, -1 ],
        #        'nw' => [  1, -1 ],
        #        ''   => [  0,  0 ],
        #    );

        # This is the ideal case - ok to continue and pad
    }

    # intermediate case: some signed numbers cannot be padded ...
    else {

        # We have to take a closer look.
        # Here is an example which looks bad if we do padding like this:
        #    my %hash = (
        #        X0 => -12867.098241163,
        #        X1 =>  2.31694338671684,       # unsigned w/   excess>0
        #        X2 => 0.0597726714860419,      # max length => excess=0
        #        Y0 =>  30043.1335503155,       # unsigned w/   excess>0
        #        Y1 => 0.0525784981597044,      # max length => excess=0
        #        Y2 => -2.32447131600783,
        #    );

        # To decide what looks okay, we count 'good' and 'bad' line interfaces:
        #    X0 - X1 = good (X0 is signed and X1 can move)
        #    X1 - X2 = bad  (x1 can move but x2 cannot)
        #    X2 - Y0 = bad  (x2 cannot move but Y0 can move)
        #    Y0 - Y1 = bad  (Y0 can move but Y1 cannot move)
        #    Y1 - Y2 = bad  (Y1 cannot move and Y2 is signed)
        # Result: 4 bad interfaces and 1 good => so we will skip this
        my $good_count = 0;
        my $bad_count  = 0;
        foreach my $item (@lengths) {
            $item->[0] = $max_unsigned_length - $item->[0];
        }
        my $item0 = shift @lengths;
        my ( $excess, $ix ) = @{$item0};
        my $immobile_count = $excess ? 0 : 1;
        foreach my $item (@lengths) {
            my $excess_m = $excess;
            my $ix_m     = $ix;
            ( $excess, $ix ) = @{$item};
            if ( !$excess ) { $immobile_count++ }

            if ( $is_signed{$ix_m} ) {

                # signed-unsigned interface
                if ( !$is_signed{$ix} ) {
                    if   ($excess) { $good_count++ }
                    else           { $bad_count++ }
                }

                # signed-signed: ok, not good or bad
            }
            else {

                # unsigned-signed interface
                if ( $is_signed{$ix} ) {
                    if   ($excess_m) { $good_count++ }
                    else             { $bad_count++ }
                }

                # unsigned-unsigned: bad if different
                else {
                    if ( $excess_m xor $excess ) {
                        $bad_count++;
                    }
                }
            }
        }

        # Filter 1: skip if more interfaces are 'bad' than 'good'
        if ( $bad_count > $good_count ) {
            return;
        }

        # Filter 2: skip in a table with multiple 'bad' interfaces and where
        # 'most' of the unsigned lengths are shorter than the signed lengths.
        # Using the median value makes this insensitive to small changes.
        if (   $median_unsigned_length >= $median_signed_length
            && $bad_count > 1
            && $immobile_count > 1 )
        {
            return;
        }

        # Anything that gets past these filters should look ok if padded
    }

    #---------------------------------------------
    # Compute actual available space for each line
    #---------------------------------------------
    my %excess_space;
    my $movable_count = 0;
    foreach my $item (@unsigned_subgroups) {
        my ( $ix_min, $ix_max ) = @{$item};
        foreach my $ix ( $ix_min .. $ix_max ) {
            my $line                = $rgroup_lines->[$ix];
            my $leading_space_count = $line->{'leading_space_count'};
            my $jmax                = $line->{'jmax'};
            my $rfield_lengths      = $line->{'rfield_lengths'};
            if ( $jcol >= $jmax ) {

                # shouldn't happen
                DEVEL_MODE && Fault("jcol=$jcol >= jmax=$jmax\n");
                return;
            }
            my @alignments = @{ $line->{'ralignments'} };
            my $col        = $alignments[$jcol]->{'column'};
            my $col_start =
                $jcol == 0
              ? $leading_space_count
              : $alignments[ $jcol - 1 ]->{'column'};
            my $avail        = $col - $col_start;
            my $field_length = $rfield_lengths->[$jcol];
            my $excess       = $avail - $field_length;
            $excess_space{$ix} = $excess;
            if ( $excess > 0 ) { $movable_count++ }
        }
    }

    return unless ($movable_count);

    # Count the number of signed-unsigned interfaces that would change
    # if we do the padding
    my $Nc = 0;
    foreach my $item (@unsigned_subgroups) {
        my ( $ix_min, $ix_max ) = @{$item};
        $Nc++ if ( $excess_space{$ix_min} > 0 && $ix_min != $ix_first );
        $Nc++ if ( $excess_space{$ix_max} > 0 && $ix_max != $ix_last );
    }

    #--------------------------------------------------------------------
    # Sparsity check:
    # Give up if the number of interface changes will be below the cutoff
    #--------------------------------------------------------------------
    if ( $unsigned > $Nc * $rOpts_valign_signed_numbers_limit ) {
        return;
    }

    #------------------------------------------------------------------------
    # Insert an extra space before the unsigned numbers if space is available
    #------------------------------------------------------------------------
    foreach my $item (@unsigned_subgroups) {
        my ( $ix_min, $ix_max ) = @{$item};

        foreach my $ix ( $ix_min .. $ix_max ) {
            next if ( $excess_space{$ix} <= 0 );
            my $line           = $rgroup_lines->[$ix];
            my $rfields        = $line->{'rfields'};
            my $rfield_lengths = $line->{'rfield_lengths'};
            pad_signed_field(
                \$rfields->[$jcol], \$rfield_lengths->[$jcol],
                $pos_start_number,  $char_end_part1
            );
        }
    }
    return;
} ## end sub end_signed_number_column

sub pad_signed_field {
    my ( $rstr, $rstr_len, $pos_start_number, $char_end_part1 ) = @_;

    # Insert an extra space before a number to highlight algebraic signs
    # in a column of numbers.
    # Given:
    #  $rstr     = ref to string
    #  $rstr_len = ref to display width of string (could include wide chars)
    #  $pos_start_number = string position of the leading digit
    #  $char_end_part1 = character at $pos_start_number - 1
    # Task: update $rstr and $rstr_len with a single space

    # First partition the string into $part1 and $part2, so that the
    # number starts at the beginning of part2.
    my $part1 = EMPTY_STRING;
    my $part2 = ${$rstr};
    my $str   = ${$rstr};
    if ( $pos_start_number > 0 ) {
        my $len = length($str);
        if ( $pos_start_number >= $len ) {
            DEVEL_MODE && Fault(<<EOM);
Expection position '$pos_start_number' < length $len of string '$str'
EOM
            return;
        }
        $part1 = substr( $str, 0, $pos_start_number );
        $part2 = substr( $str, $pos_start_number );

        # VERIFY that we are inserting a new space after either
        #   (1) an existing space or
        #   (2) an opening token.
        # Otherwise disaster can occur. An error here implies a programming
        # error in defining '$pos_start_number'.

        my $test_char1 = substr( $part1, -1, 1 );
        if ( $test_char1 ne $char_end_part1 ) {
            DEVEL_MODE && Fault(<<EOM);
Expecting '$char_end_part1' but saw '$test_char1' in string '$str'
Probably bad position '$pos_start_number'
EOM
            return;
        }
    }

    # VERIFY we are inserting a space before a digit character
    my $test_char2 = substr( $part2, 0, 1 );
    if ( $is_digit_char{$test_char2} ) {
        ${$rstr} = $part1 . SPACE . $part2;
        ${$rstr_len} += 1;
    }
    else {
        DEVEL_MODE && Fault(<<EOM);
Expecting test char2 as leading digit but saw '$test_char2' in string '$str'
May be bad position '$pos_start_number'
EOM
    }
    return;
} ## end sub pad_signed_field

sub split_field {
    my ( $pat1, $field, $pattern ) = @_;

    # Given;
    #   $pat1    = first part of a pattern before a numeric type 'n'
    #   $field   = corresponding text field
    #   $pattern = full pattern
    # Return:
    #   $pos_start_number = positiion in $field where the number should start
    #                     = 0 if cannot find
    #   $char_end_part1 = the character preceding $pos_start_number
    #   $ch_opening     = the preceding opening container character, if any

    # We have to find where the possible number starts in this field.
    # The safe thing to do is return @fail if anything does not look right.

    my $pos_start_number = 0;
    my $char_end_part1   = EMPTY_STRING;
    my $ch_opening       = EMPTY_STRING;
    my @fail             = ( $pos_start_number, $char_end_part1, $ch_opening );

    # Be sure there is just 'n' in the pattern. Multiple terms can occur
    # when fields are joined, but since we are jumping into the middle
    # of a field it is safest not to try to handle them.
    my $n_count = ( $pattern =~ tr/n/n/ );
    if ( $n_count && $n_count > 1 ) {
        return @fail;
    }

    # Same thing for commas
    my $comma_count = ( $pattern =~ tr/,/,/ );
    if ( $comma_count && $comma_count > 1 ) {
        return @fail;
    }

    # Require 0 or 1 braces
    my $len_field = length($field);
    my $len_pat1  = length($pat1);
    return @fail unless ( $len_pat1 && $len_field );

    # Look at the pattern ending
    my $ending_b = 0;
    my $ch       = substr( $pat1, -1, 1 );
    if ( $ch eq 'b' ) {
        $ending_b       = 1;
        $ch             = substr( $pat1, -2, 1 );
        $char_end_part1 = SPACE;
    }

    # handle either '{b' or '{'
    if ( $ch eq '{' ) {

        # Only one brace
        my $brace_count = ( $pat1 =~ tr/\{/\{/ );
        return @fail if ( $brace_count != 1 );

        my $i_paren   = index( $field, '(' );
        my $i_bracket = index( $field, '[' );
        my $i_brace   = index( $field, '{' );
        my $i_opening = length($field);
        if ( $i_paren >= 0 ) {
            $i_opening  = $i_paren;
            $ch_opening = '(';
        }
        if (   $i_bracket >= 0
            && $i_bracket < $i_opening )
        {
            $i_opening  = $i_bracket;
            $ch_opening = '[';
        }
        if ( $i_brace >= 0 && $i_brace < $i_opening ) {
            $i_opening  = $i_brace;
            $ch_opening = '{';
        }
        if (   $i_opening >= 0
            && $i_opening < length($field) - 1 )
        {
            $pos_start_number = $i_opening + 1 + $ending_b;
            $char_end_part1   = $ch_opening
              if ( !$ending_b );
        }
        else {
            # strange - could not find the opening token
        }
    }

    # no braces: maybe '=>b'
    else {

        # looking for patterns ending in '=b' or '=>b'
        if ( !$ending_b ) { return @fail }

        # find the = in the text
        my $pos_equals = index( $field, '=' );
        return @fail if ( $pos_equals < 0 );

        # be sure there are no other '=' in the pattern
        my $equals_count = ( $pat1 =~ tr/=/=/ );
        return @fail if ( $equals_count != 1 );

        if ( $len_pat1 >= 2 && substr( $pat1, -2, 2 ) eq '=b' ) {
            $pos_start_number = $pos_equals + 2;
        }
        elsif ( $len_pat1 >= 3 && substr( $pat1, -3, 3 ) eq '=>b' ) {
            $pos_start_number = $pos_equals + 3;
        }
        else {

            # cannot handle this pattern
            return @fail;
        }
    }

    if ( $pos_start_number <= 0 || $pos_start_number >= $len_field ) {
        return @fail;
    }

    return ( $pos_start_number, $char_end_part1, $ch_opening );
} ## end sub split_field

sub field_matches_end_pattern {
    my ( $field2, $pat2 ) = @_;

    # Check that a possible numeric field matches the ending pattern

    # Given:
    #  $field2 = the rest of the field after removing any sign
    #  $pat2   = the end pattern of this field
    # Return:
    #  false if field is definitely non-numeric
    #  true otherwise

    my $next_char   = substr( $pat2, 1, 1 );
    my $field2_trim = EMPTY_STRING;

    # if pattern is one of: 'n,', 'n,b'
    if ( $next_char eq ',' ) {
        my $icomma = index( $field2, ',' );
        if ( $icomma >= 0 ) {
            $field2_trim = substr( $field2, 0, $icomma );
        }
    }

    # if pattern is one of: 'nb', 'nb}', 'nb},'
    elsif ( $next_char eq 'b' ) {
        my $ispace = index( $field2, SPACE );
        if ( $ispace >= 0 ) {
            $field2_trim = substr( $field2, 0, $ispace );
        }
    }

    # if pattern is one of 'n},', 'n};'
    elsif ( $next_char eq '}' ) {
        if ( $field2 =~ /^([^\)\}\]]+)/ ) {
            $field2_trim = $1;
        }
    }

    # unrecognized pattern
    else {
        DEVEL_MODE && Fault(<<EOM);
Unexpected ending pattern '$pat2' next='$next_char' field2='$field2'
The hash 'is_leading_sign_pattern' seems to have changed but the code
has not been updated to handle it. Please fix.
EOM
        return;
    }

    if ( !length($field2_trim) ) {
        DEVEL_MODE
          && Fault(
            "STRANGE: cannot find end of field=$field2 for pat=$pat2 \n");
        return;
    }

    # Reject obviously non-numeric fields just to be sure we did not
    # jump into a quote of some kind
    if ( $field2_trim !~ /^[\d\.\+\-abcdefpx_]+$/i ) {
        DEBUG_VSN
          && print {*STDERR}
"Rejecting match to pat2='$pat2' with next=$next_char field2=$field2 trimmed='$field2_trim'\n";
        return;
    }
    return 1;
} ## end sub field_matches_end_pattern

sub pad_signed_number_columns {
    my ($rgroup_lines) = @_;

    # Given:
    #   $rgroup_lines = the current vertical alignment group of lines
    # Task:
    #   Look for columns of aligned numeric values, some of whose numbers
    #   have algebraic signs. Add a leading space to the unsigned
    #   numbers, if possible, so that the just the signs appear as the first
    #   character. Example of what we want to do:

    #    my @correct = (
    #        [  123456.79,   86753090000.868,  11 ],
    #        [ -123456.79,  -86753090000.868, -11 ],
    #        [  123456.001,  80.080,           10 ],
    #        [ -123456.001, -80.080,           0 ],
    #        [  10.9,        10.9,             11 ],
    #    );

    # The logic here is complex because we are working with bits of text
    # which have been broken into patterns which are convenient for the
    # vertical aligner, but we no longer have the original tokenization
    # which would have indicated the precise bounds of numbers.  So we
    # have to procede very carefully with lots of checks. There are
    # more checks than really necessary now because originally numbers
    # and quotes were both indicated with pattern 'Q'. But now numbers are
    # uniquely marked as pattern 'n', so there is less risk of an error.
    # The extra checks take very little time so they are retained.

    return unless ($rOpts_valign_signed_numbers);

    my %column_info;
    my @columns;

    #----------------
    # loop over lines
    #----------------
    my $ix_line = -1;
    my $jmax    = -1;
    foreach my $line ( @{$rgroup_lines} ) {
        $ix_line++;
        my $jmax_last = $jmax;
        $jmax = $line->{'jmax'};
        my $jmax_change = $jmax ne $jmax_last;

        my @alignments = @{ $line->{'ralignments'} };
        my $rfields    = $line->{'rfields'};
        my $rpatterns  = $line->{'rpatterns'};
        my $rtokens    = $line->{'rtokens'};

        #-----------------------------------------------
        # Check for a reduction in the number of columns
        #-----------------------------------------------
        if ( $jmax < $jmax_last ) {

            foreach my $jcol ( keys %column_info ) {

                # end any stranded columns on the right
                next if ( $jcol < $jmax );
                my $rcol_hash = $column_info{$jcol};
                next unless ($rcol_hash);
                if (   $rcol_hash->{signed_count}
                    && $rcol_hash->{unsigned_count} )
                {
                    end_signed_number_column( $rgroup_lines, $rcol_hash,
                        $ix_line - 1 );
                }
                delete $column_info{$jcol};
            }

            # Try to keep the end data column running; test case 'rfc.in'
            # The last item in a list will still need a trailing comma.
            my $jcol = $jmax - 1;
            if ( $jcol >= 0 && $column_info{$jcol} ) {
                my $alignment = $alignments[$jcol];
                my $old_col   = $columns[$jcol];
                my $col       = $alignment->{column};

                # only do this if the text has a leading digit
                if (   $col < $old_col
                    && $rfields->[$jcol] =~ /^[+-]?\d/ )
                {
                    my $spaces_needed = $old_col - $col;
                    my $spaces_available =
                      $line->get_available_space_on_right();
                    if ( $spaces_available >= $spaces_needed ) {
                        $line->increase_field_width( $jcol, $spaces_needed );
                    }
                }
            }
        }

        #--------------------------------------------
        # Loop over fields except last (side comment)
        #--------------------------------------------
        for my $jcol ( 0 .. $jmax - 1 ) {

            #-----------------------------------------
            # Decide if this is a new alignment column
            #-----------------------------------------
            my $alignment = $alignments[$jcol];
            my $old_col   = $columns[$jcol];
            my $col       = $alignment->{column};
            $columns[$jcol] = $col;
            if ( defined($old_col) && $old_col != $col ) {
                foreach my $jcol_old ( keys %column_info ) {
                    next if ( $jcol_old < $jcol );
                    my $rcol_hash = $column_info{$jcol_old};
                    if (   $rcol_hash->{signed_count}
                        && $rcol_hash->{unsigned_count} )
                    {
                        end_signed_number_column( $rgroup_lines, $rcol_hash,
                            $ix_line - 1 );
                    }
                    delete $column_info{$jcol_old};
                }
            }

            # A new padded sign column can only start at an alignment change
            my $rcol_hash = $column_info{$jcol};

            #------------------------------------------------------------
            # Examine this field, looking for signed and unsigned numbers
            #------------------------------------------------------------
            my $field   = $rfields->[$jcol];
            my $pattern = $rpatterns->[$jcol];

            my $is_signed_number   = 0;
            my $is_unsigned_number = 0;

            #--------------------------------------------------------
            # set $pos_start_number = index in field of digit or sign
            #--------------------------------------------------------
            my $pos_start_number = 0;
            my $char_end_part1   = EMPTY_STRING;
            my $ch_opening       = EMPTY_STRING;

            # Set $field_ok to false on encountering any problem
            # Do not pad signed and unsigned hash keys
            my $field_ok = length($field) > 0
              && substr( $rtokens->[$jcol], 0, 2 ) ne '=>';

            if ( $field_ok && $pattern ) {

                # Split the pattern at the first 'n'
                # $pat1 = pattern before the 'n' (if any)
                # $pat2 = pattern starting at the 'n'
                my ( $pat1, $pat2 );
                my $posq = index( $pattern, 'n' );
                if ( $posq < 0 ) {
                    $field_ok = 0;
                }
                else {
                    # Just look at up to 3 of the pattern characters
                    # We require $pat2 to have one of the known patterns
                    $pat1     = substr( $pattern, 0,     $posq );
                    $pat2     = substr( $pattern, $posq, 3 );
                    $field_ok = $is_leading_sign_pattern{$pat2};
                }

                if ($field_ok) {

                    # If the number starts within the field then we must
                    # find its offset position.
                    if ($pat1) {

                        # Note: an optimization would be to remember previous
                        # calls for each column and use them if possible, but
                        # benchmarking shows that this is not necessary.
                        # See .ba54 for example coding.
                        ( $pos_start_number, $char_end_part1, $ch_opening ) =
                          split_field( $pat1, $field, $pattern );

                        $field_ok ||= $pos_start_number;
                    }

                    if ($field_ok) {

                        # look for an optional + or - sign
                        my $test_char = substr( $field, $pos_start_number, 1 );
                        my $sign;
                        if ( $is_plus_or_minus{$test_char} ) {
                            $sign = $test_char;
                            $test_char =
                              substr( $field, $pos_start_number + 1, 1 );
                        }

                        # and a digit
                        if ( $is_digit_char{$test_char} ) {
                            my $field2;
                            if ($sign) {
                                $is_signed_number = 1;
                                $field2 =
                                  substr( $field, $pos_start_number + 1 );
                            }
                            else {
                                $is_unsigned_number = 1;
                                $field2 =
                                  $pos_start_number
                                  ? substr( $field, $pos_start_number )
                                  : $field;
                            }

                            # Check for match to ending pattern
                            $field_ok =
                              field_matches_end_pattern( $field2, $pat2 );
                        }
                        else {
                            $field_ok = 0;
                        }
                    }
                }
            }

            #----------------------
            # Figure out what to do
            #----------------------

            # we require a signed or unsigned number field
            # which is not a hash key
            $field_ok &&= ( $is_signed_number || $is_unsigned_number );

            # if a column has not started..
            if ( !$rcol_hash ) {

                # give up if this is cannot start a new column
                next if ( !$field_ok );

                # otherwise continue on to start a new column

            }

            # if a column has been started...
            else {

                # and this cannot be added to it
                if (  !$field_ok
                    || $rcol_hash->{pos_start_number} ne $pos_start_number
                    || $rcol_hash->{char_end_part1} ne $char_end_part1
                    || $rcol_hash->{col} ne $col )
                {

                    # then end the current column and start over
                    if (   $rcol_hash->{signed_count}
                        && $rcol_hash->{unsigned_count} )
                    {
                        end_signed_number_column( $rgroup_lines, $rcol_hash,
                            $ix_line - 1 );
                    }
                    delete $column_info{$jcol};
                    $rcol_hash = undef;
                }
            }

            if (DEBUG_VSN) {
                my $exists = defined($rcol_hash);
                print
"VSN: line=$ix_line change=$jmax_change jcol=$jcol field=$field exists?=$exists unsigned?=$is_unsigned_number signed?=$is_signed_number\n";
            }

            #---------------------------------------
            # Either start a new column, if possible
            #---------------------------------------
            if ( !defined($rcol_hash) ) {

                next if ( !$field_ok );

                my $rsigned_lines = $is_signed_number ? [$ix_line] : [];
                $column_info{$jcol} = {
                    unsigned_count   => $is_unsigned_number,
                    signed_count     => $is_signed_number,
                    pos_start_number => $pos_start_number,
                    char_end_part1   => $char_end_part1,
                    ix_first         => $ix_line,
                    col              => $col,
                    jcol             => $jcol,
                    rsigned_lines    => $rsigned_lines,
                };
            }

            #------------------------------
            # or extend the existing column
            #------------------------------
            else {
                $rcol_hash->{unsigned_count} += $is_unsigned_number;
                $rcol_hash->{signed_count}   += $is_signed_number;
                if ($is_signed_number) {
                    push @{ $rcol_hash->{rsigned_lines} }, $ix_line;
                }
            }
        }
    }

    #-------------------------------------
    # Loop to finish any remaining columns
    #-------------------------------------
    foreach my $jcol ( keys %column_info ) {
        my $rcol_hash = $column_info{$jcol};
        if ( $rcol_hash->{signed_count} && $rcol_hash->{unsigned_count} ) {
            end_signed_number_column( $rgroup_lines, $rcol_hash, $ix_line );
        }
    }
    return;
} ## end sub pad_signed_number_columns

#########################################
# CODE SECTION 7: Pad Wide Equals Columns
#########################################

use constant DEBUG_WEC => 0;

sub end_wide_equals_column {
    my ( $rgroup_lines, $rcol_hash, $ix_last ) = @_;

    # Finish formatting a column of wide equals
    # Given:
    #   $rgroup_lines - the current vertical aligment group of lines
    #   $rcol_hash    - a hash of information about this vertical column
    #   $ix_last      - index of the last line of this vertical column

    return unless ($rcol_hash);
    my $jcol      = $rcol_hash->{jcol};
    my $col       = $rcol_hash->{col};
    my $min_width = $rcol_hash->{min_width};
    my $max_width = $rcol_hash->{max_width};
    my $rwidths   = $rcol_hash->{rwidths};
    my $ix_first  = $rcol_hash->{ix_first};

    # check for skipped lines, shouldn't happen
    my $nlines = $ix_last - $ix_first + 1;
    my $num    = @{$rwidths};
    if ( $num != $nlines ) {
        my $line    = $rgroup_lines->[$ix_last];
        my $rfields = $line->{'rfields'};
        my $text    = join EMPTY_STRING, @{$rfields};
        DEVEL_MODE && Fault(<<EOM);
We seem to have miscounted lines, please check:
nlines=$nlines
num saved=$num
min width=$min_width
max width=$max_width
j=$jcol
ix_first=$ix_first
ix_last=$ix_last
text=$text
EOM
        return;
    }

    #------------------------------------------------------
    # loop over all lines of this vertical alignment column
    #------------------------------------------------------

    my (
        $current_alignment, $starting_colp,
        $current_line,      @previous_linked_lines
    );
    foreach my $item ( @{$rwidths} ) {
        my ( $ix, $width ) = @{$item};
        my $line = $rgroup_lines->[$ix];

        # add leading spaces to the shorter equality tokens to get
        # vertical alignment of the '=' signs
        my $jmax  = $line->{'jmax'};
        my $jcolp = $jcol + 1;

        my @alignments = @{ $line->{'ralignments'} };
        my $alignment  = $alignments[$jcolp];
        my $colp       = $alignment->{column};

        #------------------------------------------------------------
        # Transfer column width changes between equivalent alignments
        #------------------------------------------------------------

        # This step keeps alignments to the right correct in case the
        # alignment object changes but the actual alignment col does not.
        # It is extremely rare for this to occur. Issue c353.

        # nothing to do if no more real alignments on right
        if ( $jcolp >= $jmax - 1 ) {
            $current_alignment     = undef;
            $current_line          = undef;
            @previous_linked_lines = ();
        }

        # handle new rhs alignment
        elsif ( !$current_alignment ) {
            $current_alignment     = $alignment;
            $current_line          = $line;
            $starting_colp         = $colp;
            @previous_linked_lines = ();
        }

        # handle change in existing alignment
        elsif ( refaddr($alignment) != refaddr($current_alignment) ) {

            # change rhs alignment column - new vertical group on right
            if ( $starting_colp != $colp ) {
                $starting_colp         = $colp;
                @previous_linked_lines = ();
            }
            else {

                # Same starting alignment col on right, but different alignment
                # object. See if we must increase width of this new alignment
                # object.
                my $current_colp = $current_alignment->{column};
                if ( $current_colp > $colp ) {
                    my $excess = $current_colp - $colp;
                    my $padding_available =
                      $line->get_available_space_on_right();
                    if ( $excess <= $padding_available ) {
                        $line->increase_field_width( $jcolp, $excess );
                        $colp = $alignment->{column};
                    }
                }

                # remember the previous line in case we have to go back and
                # increasse its width
                push @previous_linked_lines, $current_line;
            }
            $current_alignment = $alignment;
            $current_line      = $line;
        }
        else {
            # continuing with same alignment
        }

        #-----------------------
        # add any needed padding
        #-----------------------
        my $pad = $max_width - $width;
        if ( $pad > 0 ) {

            my $rfields        = $line->{'rfields'};
            my $rfield_lengths = $line->{'rfield_lengths'};

            my $lenp   = $rfield_lengths->[$jcolp];
            my $avail  = $colp - $col;
            my $excess = $lenp + $pad - $avail;

            if ( $excess > 0 ) {

                my $padding_available = $line->get_available_space_on_right();
                if ( $excess <= $padding_available ) {
                    $line->increase_field_width( $jcolp, $excess );

                    # Increase space of any previous linked lines
                    foreach my $line_prev (@previous_linked_lines) {
                        $padding_available =
                          $line_prev->get_available_space_on_right();
                        if ( $excess <= $padding_available ) {
                            $line_prev->increase_field_width( $jcolp, $excess );
                        }
                        else {
                            last;
                        }
                    }
                }
                else {
                    $pad = 0;
                }

            }

            # Add spaces
            $rfields->[$jcolp] = ( SPACE x $pad ) . $rfields->[$jcolp];
            $rfield_lengths->[$jcolp] += $pad;
        }
    }
    return;
} ## end sub end_wide_equals_column

sub pad_wide_equals_columns {
    my ($rgroup_lines) = @_;

    # Given:
    #   $rgroup_lines = the current vertical alignment group of lines
    # Task:
    #   Look for columns of aligned equals tokens, some of which may be
    #   things like '-=', '&&=', etc.  Increase the field length of the
    #   previous field by 1 or 2 spaces where necessary and possible so
    #   that alignment of all '=' occurs.  For example, given

    #       $j    /= 2;
    #       $pow2 = $pow2 * $pow2;

    # In this case we want to add a leading space '=' term to get
    #       $j    /= 2;
    #       $pow2  = $pow2 * $pow2;

    # The logic here is somewhat similar to sub pad_signed_number_columns

    return unless ($rOpts_valign_wide_equals);

    my %column_info;
    my @columns;

    #----------------
    # loop over lines
    #----------------
    my $ix_line = -1;
    my $jmax    = -1;
    foreach my $line ( @{$rgroup_lines} ) {
        $ix_line++;
        my $jmax_last = $jmax;
        $jmax = $line->{'jmax'};
        my $jmax_change = $jmax ne $jmax_last;

        my @alignments = @{ $line->{'ralignments'} };
        my $rfields    = $line->{'rfields'};
        my $rtokens    = $line->{'rtokens'};

        #-----------------------------------------------
        # Check for a reduction in the number of columns
        #-----------------------------------------------
        if ( $jmax < $jmax_last ) {

            foreach my $jcol ( keys %column_info ) {

                # end any stranded columns on the right
                next if ( $jcol < $jmax );
                my $rcol_hash = $column_info{$jcol};
                next unless ($rcol_hash);
                if ( $rcol_hash->{max_width} > $rcol_hash->{min_width} ) {
                    end_wide_equals_column( $rgroup_lines, $rcol_hash,
                        $ix_line - 1 );
                }
                delete $column_info{$jcol};
            }
        }

        #--------------------------------------------------
        # Loop over fields except last field (side comment)
        #--------------------------------------------------
        for my $jcol ( 0 .. $jmax - 1 ) {

            #-----------------------------------------
            # Decide if this is a new alignment column
            #-----------------------------------------
            my $alignment = $alignments[$jcol];
            my $old_col   = $columns[$jcol];
            my $col       = $alignment->{column};
            $columns[$jcol] = $col;
            if ( defined($old_col) && $old_col != $col ) {
                foreach my $jcol_old ( keys %column_info ) {
                    next if ( $jcol_old < $jcol );
                    my $rcol_hash = $column_info{$jcol_old};
                    if ( $rcol_hash->{max_width} > $rcol_hash->{min_width} ) {
                        end_wide_equals_column( $rgroup_lines, $rcol_hash,
                            $ix_line - 1 );
                    }
                    delete $column_info{$jcol_old};
                }
            }

            # A new wide equals column can only start at an alignment change
            my $rcol_hash = $column_info{$jcol};

            #------------------------------------------------------
            # Examine this field, looking for equals or wide equals
            #------------------------------------------------------
            my $field_next = $rfields->[ $jcol + 1 ];
            my $token      = $rtokens->[$jcol];

            # See if this is an equals alignment group;
            # indicated by alignment token of '=' followed by a digit
            my $len_equals_symbol = 0;
            if (   length($token) > 1
                && substr( $token, 0, 1 ) eq '='
                && $is_digit_char{ substr( $token, 1, 1 ) } )
            {

                # find the actual equality symbol which starts the next field
                # i.e. '=' or '**=' or '-=' etc. We just need its length.
                my $pos = index( $field_next, '=' );
                if ( $pos >= 0 && $pos <= 2 ) {
                    $len_equals_symbol = $pos + 1;
                }
            }

            # if a column has not started..
            if ( !$rcol_hash ) {

                # give up if this is cannot start a new column
                next if ( !$len_equals_symbol );

                # otherwise continue on to start a new column

            }

            # if a column has been started...
            else {

                # and this cannot be added to it
                if ( !$len_equals_symbol || $rcol_hash->{col} ne $col ) {

                    # then end the current column and start over
                    if ( $rcol_hash->{max_width} > $rcol_hash->{min_width} ) {
                        end_wide_equals_column( $rgroup_lines, $rcol_hash,
                            $ix_line - 1 );
                    }
                    delete $column_info{$jcol};
                    $rcol_hash = undef;
                }
            }

            if (DEBUG_WEC) {
                my $exists = defined($rcol_hash);
                print
"WEA: line=$ix_line change=$jmax_change jcol=$jcol field=$field_next exists?=$exists equals?=$len_equals_symbol\n";
            }

            #---------------------------------------
            # Either start a new column, if possible
            #---------------------------------------
            if ( !defined($rcol_hash) ) {

                next if ( !$len_equals_symbol );

                $column_info{$jcol} = {
                    ix_first  => $ix_line,
                    col       => $col,
                    jcol      => $jcol,
                    min_width => $len_equals_symbol,
                    max_width => $len_equals_symbol,
                    rwidths   => [ [ $ix_line, $len_equals_symbol ] ],
                };
            }

            #------------------------------
            # or extend the existing column
            #------------------------------
            else {
                if ( $len_equals_symbol > $rcol_hash->{max_width} ) {
                    $rcol_hash->{max_width} = $len_equals_symbol;
                }
                if ( $len_equals_symbol < $rcol_hash->{min_width} ) {
                    $rcol_hash->{min_width} = $len_equals_symbol;
                }
                push @{ $rcol_hash->{rwidths} },
                  [ $ix_line, $len_equals_symbol ];
            }
        }
    }

    #-------------------------------------
    # Loop to finish any remaining columns
    #-------------------------------------
    foreach my $jcol ( keys %column_info ) {
        my $rcol_hash = $column_info{$jcol};
        if ( $rcol_hash->{max_width} > $rcol_hash->{min_width} ) {
            end_wide_equals_column( $rgroup_lines, $rcol_hash, $ix_line );
        }
    }
    return;
} ## end sub pad_wide_equals_columns

###############################
# CODE SECTION 8: Output Step A
###############################

sub valign_output_step_A {

    #------------------------------------------------------------
    # This is Step A in writing vertically aligned lines.
    # The line is prepared according to the alignments which have
    # been found. Then it is shipped to the next step.
    #------------------------------------------------------------

    my ( $self, $rinput_hash ) = @_;

    my $line                 = $rinput_hash->{line};
    my $min_ci_gap           = $rinput_hash->{min_ci_gap};
    my $do_not_align         = $rinput_hash->{do_not_align};
    my $group_leader_length  = $rinput_hash->{group_leader_length};
    my $extra_leading_spaces = $rinput_hash->{extra_leading_spaces};
    my $level                = $rinput_hash->{level};
    my $maximum_line_length  = $rinput_hash->{maximum_line_length};

    my $rfields                   = $line->{'rfields'};
    my $rfield_lengths            = $line->{'rfield_lengths'};
    my $leading_space_count       = $line->{'leading_space_count'};
    my $outdent_long_lines        = $line->{'outdent_long_lines'};
    my $maximum_field_index       = $line->{'jmax'};
    my $rvertical_tightness_flags = $line->{'rvertical_tightness_flags'};
    my $Kend                      = $line->{'Kend'};
    my $level_end                 = $line->{'level_end'};

    # Check for valid hash keys at end of lifetime of $line during development
    DEVEL_MODE
      && check_keys( $line, \%valid_LINE_keys,
        "Checking line keys at valign_output_step_A", 1 );

    # add any extra spaces
    if ( $leading_space_count > $group_leader_length ) {
        $leading_space_count += $min_ci_gap;
    }

    my $str     = $rfields->[0];
    my $str_len = $rfield_lengths->[0];

    my @alignments = @{ $line->{'ralignments'} };
    if ( @alignments != $maximum_field_index + 1 ) {

        # Shouldn't happen: sub install_new_alignments makes jmax alignments
        my $jmax_alignments = @alignments - 1;
        if (DEVEL_MODE) {
            Fault(
"alignment jmax=$jmax_alignments should equal $maximum_field_index\n"
            );
        }
        $do_not_align = 1;
    }

    # loop to concatenate all fields of this line and needed padding
    my $total_pad_count = 0;
    for my $j ( 1 .. $maximum_field_index ) {

        # skip zero-length side comments
        last
          if (
            ( $j == $maximum_field_index )
            && ( !defined( $rfields->[$j] )
                || ( $rfield_lengths->[$j] == 0 ) )
          );

        # compute spaces of padding before this field
        my $col = $alignments[ $j - 1 ]->{'column'};
        my $pad = $col - ( $str_len + $leading_space_count );

        if ($do_not_align) {
            $pad =
              ( $j < $maximum_field_index )
              ? 0
              : $rOpts_minimum_space_to_comment - 1;
        }

        # if the -fpsc flag is set, move the side comment to the selected
        # column if and only if it is possible, ignoring constraints on
        # line length and minimum space to comment
        if (   $rOpts_fixed_position_side_comment
            && $j == $maximum_field_index )
        {
            my $newpad = $pad + $rOpts_fixed_position_side_comment - $col - 1;
            if ( $newpad >= 0 ) { $pad = $newpad; }
        }

        # accumulate the padding
        if ( $pad > 0 ) { $total_pad_count += $pad; }

        # only add padding when we have a finite field;
        # this avoids extra terminal spaces if we have empty fields
        if ( $rfield_lengths->[$j] > 0 ) {
            $str .= SPACE x $total_pad_count;
            $str_len += $total_pad_count;
            $total_pad_count = 0;
            $str .= $rfields->[$j];
            $str_len += $rfield_lengths->[$j];
        }
        else {
            $total_pad_count = 0;
        }
    }

    my $side_comment_length = $rfield_lengths->[$maximum_field_index];

    # ship this line off
    $self->valign_output_step_B(
        {
            leading_space_count => $leading_space_count + $extra_leading_spaces,
            line                => $str,
            line_length         => $str_len,
            side_comment_length => $side_comment_length,
            outdent_long_lines  => $outdent_long_lines,
            rvertical_tightness_flags => $rvertical_tightness_flags,
            level                     => $level,
            level_end                 => $level_end,
            Kend                      => $Kend,
            maximum_line_length       => $maximum_line_length,
        }
    );
    return;
} ## end sub valign_output_step_A

sub combine_fields {

    # We have a group of two lines for which we do not want to align tokens
    # between index $imax_align and the side comment.  So we will delete fields
    # between $imax_align and the side comment.  Alignments have already
    # been set so we have to adjust them.

    my ( $line_0, $line_1, $imax_align ) = @_;

    if ( !defined($imax_align) ) { $imax_align = -1 }

    # First delete the unwanted tokens
    my $jmax_old = $line_0->{'jmax'};
    my @idel     = ( $imax_align + 1 .. $jmax_old - 2 );
    return if ( !@idel );

    # Get old alignments before any changes are made
    my @old_alignments = @{ $line_0->{'ralignments'} };

    foreach my $line ( $line_0, $line_1 ) {
        delete_selected_tokens( $line, \@idel );
    }

    # Now adjust the alignments.  Note that the side comment alignment
    # is always at jmax-1, and there is an ending alignment at jmax.
    my @new_alignments;
    if ( $imax_align >= 0 ) {
        @new_alignments[ 0 .. $imax_align ] =
          @old_alignments[ 0 .. $imax_align ];
    }

    my $jmax_new = $line_0->{'jmax'};

    $new_alignments[ $jmax_new - 1 ] = $old_alignments[ $jmax_old - 1 ];
    $new_alignments[$jmax_new]       = $old_alignments[$jmax_old];
    $line_0->{'ralignments'}         = \@new_alignments;
    $line_1->{'ralignments'}         = \@new_alignments;
    return;
} ## end sub combine_fields

sub get_output_line_number {

    # Return the output line number to external modules.
    # The output line number reported to a caller =
    # the number of items still in the buffer +
    # the number of items written.
    my $self = shift;
    return $self->group_line_count() +
      $self->[_file_writer_object_]->get_output_line_number();
} ## end sub get_output_line_number

###############################
# CODE SECTION 9: Output Step B
###############################

{    ## closure for sub valign_output_step_B

    # These are values for a cache used by valign_output_step_B.
    my $cached_line_text;
    my $cached_line_text_length;
    my $cached_line_type;
    my $cached_line_opening_flag;
    my $cached_line_closing_flag;
    my $cached_seqno;
    my $cached_line_valid;
    my $cached_line_leading_space_count;
    my $cached_seqno_string;
    my $cached_line_Kend;
    my $cached_line_maximum_length;

    # These are passed to step_C:
    my $seqno_string;
    my $last_nonblank_seqno_string;

    sub set_last_nonblank_seqno_string {
        my ($val) = @_;
        $last_nonblank_seqno_string = $val;
        return;
    }

    sub get_cached_line_opening_flag {
        return $cached_line_opening_flag;
    }

    sub get_cached_line_type {
        return $cached_line_type;
    }

    sub set_cached_line_valid {
        my ($val) = @_;
        $cached_line_valid = $val;
        return;
    }

    sub get_cached_seqno {
        return $cached_seqno;
    }

    sub initialize_step_B_cache {

        # valign_output_step_B cache:
        $cached_line_text                = EMPTY_STRING;
        $cached_line_text_length         = 0;
        $cached_line_type                = 0;
        $cached_line_opening_flag        = 0;
        $cached_line_closing_flag        = 0;
        $cached_seqno                    = 0;
        $cached_line_valid               = 0;
        $cached_line_leading_space_count = 0;
        $cached_seqno_string             = EMPTY_STRING;
        $cached_line_Kend                = undef;
        $cached_line_maximum_length      = undef;

        # These vars hold a string of sequence numbers joined together used by
        # the cache
        $seqno_string               = EMPTY_STRING;
        $last_nonblank_seqno_string = EMPTY_STRING;
        return;
    } ## end sub initialize_step_B_cache

    sub _flush_step_B_cache {
        my ($self) = @_;

        # Send any text in the step_B cache on to step_C
        if ($cached_line_type) {
            $seqno_string = $cached_seqno_string;
            $self->valign_output_step_C(
                $seqno_string,
                $last_nonblank_seqno_string,

                $cached_line_text,
                $cached_line_leading_space_count,
                $self->[_last_level_written_],
                $cached_line_Kend,
            );
            $cached_line_type           = 0;
            $cached_line_text           = EMPTY_STRING;
            $cached_line_text_length    = 0;
            $cached_seqno_string        = EMPTY_STRING;
            $cached_line_Kend           = undef;
            $cached_line_maximum_length = undef;
        }
        return;
    } ## end sub _flush_step_B_cache

    sub handle_cached_line {

        my ( $self, $rinput, $leading_string, $leading_string_length ) = @_;

        # The cached line will either be:
        # - passed along to step_C, or
        # - or combined with the current line

        my $last_level_written = $self->[_last_level_written_];

        my $leading_space_count       = $rinput->{leading_space_count};
        my $str                       = $rinput->{line};
        my $str_length                = $rinput->{line_length};
        my $rvertical_tightness_flags = $rinput->{rvertical_tightness_flags};
        my $level                     = $rinput->{level};
        my $level_end                 = $rinput->{level_end};
        my $maximum_line_length       = $rinput->{maximum_line_length};

        my ( $open_or_close, $seqno_beg );
        if ($rvertical_tightness_flags) {

            $open_or_close = $rvertical_tightness_flags->{_vt_type};
            $seqno_beg     = $rvertical_tightness_flags->{_vt_seqno_beg};
        }

        # Dump an invalid cached line
        if ( !$cached_line_valid ) {
            $self->valign_output_step_C(
                $seqno_string,
                $last_nonblank_seqno_string,

                $cached_line_text,
                $cached_line_leading_space_count,
                $last_level_written,
                $cached_line_Kend,
            );
        }

        # Handle cached line ending in OPENING tokens
        elsif ( $cached_line_type == 1 || $cached_line_type == 3 ) {

            my $gap = $leading_space_count - $cached_line_text_length;

            # handle option of just one tight opening per line:
            if ( $cached_line_opening_flag == 1 ) {
                if ( defined($open_or_close) && $open_or_close == 1 ) {
                    $gap = -1;
                }
            }

            # Do not join the lines if this might produce a one-line
            # container which exceeds the maximum line length.  This is
            # necessary prevent blinking, particularly with the combination
            # -xci -pvt=2.  In that case a one-line block alternately forms
            # and breaks, causing -xci to alternately turn on and off (case
            # b765).
            # Patched to fix cases b656 b862 b971 b972: always do the check
            # if the maximum line length changes (due to -vmll).
            if (
                $gap >= 0
                && ( $maximum_line_length != $cached_line_maximum_length
                    || ( defined($level_end) && $level > $level_end ) )
              )
            {
                my $test_line_length =
                  $cached_line_text_length + $gap + $str_length;

                # Add a small tolerance in the length test (fixes case b862)
                if ( $test_line_length > $cached_line_maximum_length - 2 ) {
                    $gap = -1;
                }
            }

            if ( $gap >= 0 && defined($seqno_beg) ) {
                $maximum_line_length   = $cached_line_maximum_length;
                $leading_string        = $cached_line_text . SPACE x $gap;
                $leading_string_length = $cached_line_text_length + $gap;
                $leading_space_count   = $cached_line_leading_space_count;
                $seqno_string = $cached_seqno_string . ':' . $seqno_beg;
                $level        = $last_level_written;
            }
            else {
                $self->valign_output_step_C(
                    $seqno_string,
                    $last_nonblank_seqno_string,

                    $cached_line_text,
                    $cached_line_leading_space_count,
                    $last_level_written,
                    $cached_line_Kend,
                );
            }
        }

        # Handle cached line ending in CLOSING tokens
        else {
            my $test_line =
              $cached_line_text . SPACE x $cached_line_closing_flag . $str;
            my $test_line_length =
              $cached_line_text_length +
              $cached_line_closing_flag +
              $str_length;
            if (

                # The new line must start with container
                $seqno_beg

                # The container combination must be okay..
                && (

                    # okay to combine like types
                    ( $open_or_close == $cached_line_type )

                    # closing block brace may append to non-block
                    || ( $cached_line_type == 2 && $open_or_close == 4 )

                    # something like ');'
                    || ( !$open_or_close && $cached_line_type == 2 )

                )

                # The combined line must fit
                && ( $test_line_length <= $cached_line_maximum_length )
              )
            {

                $seqno_string = $cached_seqno_string . ':' . $seqno_beg;

                # Patch to outdent closing tokens ending # in ');' If we
                # are joining a line like ');' to a previous stacked set of
                # closing tokens, then decide if we may outdent the
                # combined stack to the indentation of the ');'.  Since we
                # should not normally outdent any of the other tokens more
                # than the indentation of the lines that contained them, we
                # will only do this if all of the corresponding opening
                # tokens were on the same line.  This can happen with -sot
                # and -sct.

                # For example, it is ok here:
                #   __PACKAGE__->load_components( qw(
                #         PK::Auto
                #         Core
                #   ));
                #
                # But, for example, we do not outdent in this example
                # because that would put the closing sub brace out farther
                # than the opening sub brace:
                #
                #   perltidy -sot -sct
                #   $c->Tk::bind(
                #       '<Control-f>' => sub {
                #           my ($c) = @_;
                #           my $e = $c->XEvent;
                #           itemsUnderArea $c;
                #       } );
                #
                if (   $str =~ /^\);/
                    && $cached_line_text =~ /^[\)\}\]\s]*$/ )
                {

                    # The way to tell this is if the stacked sequence
                    # numbers of this output line are the reverse of the
                    # stacked sequence numbers of the previous non-blank
                    # line of sequence numbers.  So we can join if the
                    # previous nonblank string of tokens is the mirror
                    # image.  For example if stack )}] is 13:8:6 then we
                    # are looking for a leading stack like [{( which
                    # is 6:8:13. We only need to check the two ends,
                    # because the intermediate tokens must fall in order.
                    # Note on speed: having to split on colons and
                    # eliminate multiple colons might appear to be slow,
                    # but it's not an issue because we almost never come
                    # through here.  In a typical file we don't.

                    $seqno_string               =~ s/^:+//;
                    $last_nonblank_seqno_string =~ s/^:+//;
                    $seqno_string               =~ s/:+/:/g;
                    $last_nonblank_seqno_string =~ s/:+/:/g;

                    # how many spaces can we outdent?
                    my $diff =
                      $cached_line_leading_space_count - $leading_space_count;
                    if (   $diff > 0
                        && length($seqno_string)
                        && length($last_nonblank_seqno_string) ==
                        length($seqno_string) )
                    {
                        my @seqno_last =
                          ( split /:/, $last_nonblank_seqno_string );
                        my @seqno_now = ( split /:/, $seqno_string );
                        if (   @seqno_now
                            && @seqno_last
                            && $seqno_now[-1] == $seqno_last[0]
                            && $seqno_now[0] == $seqno_last[-1] )
                        {

                            # OK to outdent ..
                            # for absolute safety, be sure we only remove
                            # whitespace
                            my $ws = substr( $test_line, 0, $diff );
                            if ( ( length($ws) == $diff )
                                && $ws =~ /^\s+$/ )
                            {

                                $test_line = substr( $test_line, $diff );
                                $cached_line_leading_space_count -= $diff;
                                $last_level_written =
                                  $self->level_change(
                                    $cached_line_leading_space_count,
                                    $diff, $last_level_written );
                                $self->reduce_valign_buffer_indentation($diff);
                            }

                            # shouldn't happen, but not critical:
                            ##else {
                            ## ERROR transferring indentation here
                            ##}
                        }
                    }
                }

                # Change the args to look like we received the combined line
                $str                   = $test_line;
                $str_length            = $test_line_length;
                $leading_string        = EMPTY_STRING;
                $leading_string_length = 0;
                $leading_space_count   = $cached_line_leading_space_count;
                $level                 = $last_level_written;
                $maximum_line_length   = $cached_line_maximum_length;
            }
            else {
                $self->valign_output_step_C(
                    $seqno_string,
                    $last_nonblank_seqno_string,

                    $cached_line_text,
                    $cached_line_leading_space_count,
                    $last_level_written,
                    $cached_line_Kend,
                );
            }
        }
        return ( $str, $str_length, $leading_string, $leading_string_length,
            $leading_space_count, $level, $maximum_line_length );

    } ## end sub handle_cached_line

    sub valign_output_step_B {

        #---------------------------------------------------------
        # This is Step B in writing vertically aligned lines.
        # Vertical tightness is applied according to preset flags.
        # In particular this routine handles stacking of opening
        # and closing tokens.
        #---------------------------------------------------------

        my ( $self, $rinput ) = @_;

        my $leading_space_count       = $rinput->{leading_space_count};
        my $str                       = $rinput->{line};
        my $str_length                = $rinput->{line_length};
        my $side_comment_length       = $rinput->{side_comment_length};
        my $outdent_long_lines        = $rinput->{outdent_long_lines};
        my $rvertical_tightness_flags = $rinput->{rvertical_tightness_flags};
        my $level                     = $rinput->{level};
        my $level_end                 = $rinput->{level_end};
        my $Kend                      = $rinput->{Kend};
        my $maximum_line_length       = $rinput->{maximum_line_length};

        # Useful -gcs test cases for wide characters are
        # perl527/(method.t.2, reg_mesg.t, mime-header.t)

        # handle outdenting of long lines:
        my $is_outdented_line;
        if ($outdent_long_lines) {
            my $excess =
              $str_length -
              $side_comment_length +
              $leading_space_count -
              $maximum_line_length;
            if ( $excess > 0 ) {
                $leading_space_count = 0;
                my $file_writer_object = $self->[_file_writer_object_];
                my $last_outdented_line_at =
                  $file_writer_object->get_output_line_number();
                $self->[_last_outdented_line_at_] = $last_outdented_line_at;

                my $outdented_line_count = $self->[_outdented_line_count_];
                if ( !$outdented_line_count ) {
                    $self->[_first_outdented_line_at_] =
                      $last_outdented_line_at;
                }
                $outdented_line_count++;
                $self->[_outdented_line_count_] = $outdented_line_count;
                $is_outdented_line = 1;
            }
        }

        # Make preliminary leading whitespace.  It could get changed
        # later by entabbing, so we have to keep track of any changes
        # to the leading_space_count from here on.
        my $leading_string =
          $leading_space_count > 0
          ? ( SPACE x $leading_space_count )
          : EMPTY_STRING;
        my $leading_string_length = length($leading_string);

        # Unpack any recombination data; it was packed by
        # sub 'Formatter::set_vertical_tightness_flags'

        # old   hash              Meaning
        # index key
        #
        # 0   _vt_type:           1=opening non-block    2=closing non-block
        #                         3=opening block brace  4=closing block brace
        #
        # 1a  _vt_opening_flag:  1=no multiple steps, 2=multiple steps ok
        # 1b  _vt_closing_flag:    spaces of padding to use if closing
        # 2   _vt_seqno:          sequence number of container
        # 3   _vt_valid flag:     do not append if this flag is false. Will be
        #           true if appropriate -vt flag is set.  Otherwise, Will be
        #           made true only for 2 line container in parens with -lp
        # 4   _vt_seqno_beg:      sequence number of first token of line
        # 5   _vt_seqno_end:      sequence number of last token of line
        # 6   _vt_min_lines:      min number of lines for joining opening cache,
        #                           0=no constraint
        # 7   _vt_max_lines:      max number of lines for joining opening cache,
        #                           0=no constraint

        my ( $open_or_close, $opening_flag, $closing_flag, $seqno, $valid,
            $seqno_beg, $seqno_end );
        if ($rvertical_tightness_flags) {

            $open_or_close = $rvertical_tightness_flags->{_vt_type};
            $opening_flag  = $rvertical_tightness_flags->{_vt_opening_flag};
            $closing_flag  = $rvertical_tightness_flags->{_vt_closing_flag};
            $seqno         = $rvertical_tightness_flags->{_vt_seqno};
            $valid         = $rvertical_tightness_flags->{_vt_valid_flag};
            $seqno_beg     = $rvertical_tightness_flags->{_vt_seqno_beg};
            $seqno_end     = $rvertical_tightness_flags->{_vt_seqno_end};
        }

        $seqno_string = $seqno_end;

        # handle any cached line ..
        # either append this line to it or write it out
        # Note: the function length() is used in this next test out of caution.
        # All testing has shown that the variable $cached_line_text_length is
        # correct, but its calculation is complex and a loss of cached text
        # would be a disaster.
        if ( length($cached_line_text) ) {

            (
                $str,
                $str_length,
                $leading_string,
                $leading_string_length,
                $leading_space_count,
                $level,
                $maximum_line_length

            ) = $self->handle_cached_line( $rinput, $leading_string,
                $leading_string_length );

            $cached_line_type           = 0;
            $cached_line_text           = EMPTY_STRING;
            $cached_line_text_length    = 0;
            $cached_line_Kend           = undef;
            $cached_line_maximum_length = undef;

        }

        # make the line to be written
        my $line        = $leading_string . $str;
        my $line_length = $leading_string_length + $str_length;

        # Safety check: be sure that a line to be cached as a stacked block
        # brace line ends in the appropriate opening or closing block brace.
        # This should always be the case if the caller set flags correctly.
        # Code '3' is for -sobb, code '4' is for -scbb.
        if ($open_or_close) {
            if (   $open_or_close == 3 && $line !~ /\{\s*$/
                || $open_or_close == 4 && $line !~ /\}\s*$/ )
            {
                $open_or_close = 0;
            }
        }

        # write or cache this line ...
        # fix for case b999: do not cache an outdented line
        # fix for b1378: do not cache an empty line
        if (  !$open_or_close
            || $side_comment_length > 0
            || $is_outdented_line
            || !$line_length )
        {
            $self->valign_output_step_C(
                $seqno_string,
                $last_nonblank_seqno_string,

                $line,
                $leading_space_count,
                $level,
                $Kend,
            );
        }
        else {
            $cached_line_text                = $line;
            $cached_line_text_length         = $line_length;
            $cached_line_type                = $open_or_close;
            $cached_line_opening_flag        = $opening_flag;
            $cached_line_closing_flag        = $closing_flag;
            $cached_seqno                    = $seqno;
            $cached_line_valid               = $valid;
            $cached_line_leading_space_count = $leading_space_count;
            $cached_seqno_string             = $seqno_string;
            $cached_line_Kend                = $Kend;
            $cached_line_maximum_length      = $maximum_line_length;
        }

        $self->[_last_level_written_]       = $level;
        $self->[_last_side_comment_length_] = $side_comment_length;
        return;
    } ## end sub valign_output_step_B
}

################################
# CODE SECTION 10: Output Step C
################################

{    ## closure for sub valign_output_step_C

    # Vertical alignment buffer used by valign_output_step_C
    my $valign_buffer_filling;
    my @valign_buffer;

    sub initialize_valign_buffer {
        @valign_buffer         = ();
        $valign_buffer_filling = EMPTY_STRING;
        return;
    }

    sub dump_valign_buffer {
        my ($self) = @_;

        # Send all lines in the current buffer on to step_D
        if (@valign_buffer) {
            foreach (@valign_buffer) {
                $self->valign_output_step_D( @{$_} );
            }
            @valign_buffer = ();
        }
        $valign_buffer_filling = EMPTY_STRING;
        return;
    } ## end sub dump_valign_buffer

    sub reduce_valign_buffer_indentation {

        my ( $self, $diff ) = @_;

        # Reduce the leading indentation of lines in the current
        # buffer by $diff spaces
        if ( $valign_buffer_filling && $diff ) {
            my $max_valign_buffer = @valign_buffer;
            foreach my $i ( 0 .. $max_valign_buffer - 1 ) {
                my ( $line, $leading_space_count, $level, $Kend ) =
                  @{ $valign_buffer[$i] };
                my $ws = substr( $line, 0, $diff );
                if ( ( length($ws) == $diff ) && $ws =~ /^\s+$/ ) {
                    $line = substr( $line, $diff );
                }
                if ( $leading_space_count >= $diff ) {
                    $leading_space_count -= $diff;
                    $level =
                      $self->level_change( $leading_space_count, $diff,
                        $level );
                }
                $valign_buffer[$i] =
                  [ $line, $leading_space_count, $level, $Kend ];
            }
        }
        return;
    } ## end sub reduce_valign_buffer_indentation

    sub valign_output_step_C {

        #-----------------------------------------------------------------------
        # This is Step C in writing vertically aligned lines.
        # Lines are either stored in a buffer or passed along to the next step.
        # The reason for storing lines is that we may later want to reduce their
        # indentation when -sot and -sct are both used.
        #-----------------------------------------------------------------------
        my (
            $self,
            $seqno_string,
            $last_nonblank_seqno_string,

            @args_to_D,
        ) = @_;

        # Dump any saved lines if we see a line with an unbalanced opening or
        # closing token.
        $self->dump_valign_buffer()
          if ( $seqno_string && $valign_buffer_filling );

        # Either store or write this line
        if ($valign_buffer_filling) {
            push @valign_buffer, [@args_to_D];
        }
        else {
            $self->valign_output_step_D(@args_to_D);
        }

        # For lines starting or ending with opening or closing tokens..
        if ($seqno_string) {
            $last_nonblank_seqno_string = $seqno_string;
            set_last_nonblank_seqno_string($seqno_string);

            # Start storing lines when we see a line with multiple stacked
            # opening tokens.
            # patch for RT #94354, requested by Colin Williams
            if (   index( $seqno_string, ':' ) >= 0
                && $seqno_string =~ /^\d+(\:+\d+)+$/
                && $args_to_D[0] !~ /^[\}\)\]\:\?]/ )
            {

                # This test is efficient but a little subtle: The first test
                # says that we have multiple sequence numbers and hence
                # multiple opening or closing tokens in this line.  The second
                # part of the test rejects stacked closing and ternary tokens.
                # So if we get here then we should have stacked unbalanced
                # opening tokens.

                # Here is a complex example:

                # Foo($Bar[0], {  # (side comment)
                #     baz => 1,
                # });

                # The first line has sequence 6::4.  It does not begin with
                # a closing token or ternary, so it passes the test and must be
                # stacked opening tokens.

                # The last line has sequence 4:6 but is a stack of closing
                # tokens, so it gets rejected.

                # Note that the sequence number of an opening token for a qw
                # quote is a negative number and will be rejected.  For
                # example, for the following line: skip_symbols([qw(
                # $seqno_string='10:5:-1'.  It would be okay to accept it but I
                # decided not to do this after testing.

                $valign_buffer_filling = $seqno_string;

            }
        }
        return;
    } ## end sub valign_output_step_C
}

###############################
# CODE SECTION 11: Output Step D
###############################

sub add_leading_tabs {

    my ( $line, $leading_space_count, $level ) = @_;

    # Convert leading whitespace to use tabs if -et or -t are set

    # Given:
    #   $line = the line of text to be written, without any tabs
    #   $leading_whitespace = expected number of leading blank spaces
    #   $level = indentation level (needed for -t)

    # Return:
    #   $line = the line with possible leading tabs

    my $trimmed_line = $line;
    $trimmed_line =~ s/^ [^\S\n]+ //gxm;

    # Check for discrepancy in actual leading white spaces with estimate
    if ( length($line) != length($trimmed_line) + $leading_space_count ) {

        # If $leading_space_count is zero, then this routine must not
        # be called because we might be in a quote of some kind
        if ( $leading_space_count <= 0 ) {
            DEVEL_MODE && Fault(<<EOM);
should not be here with leading space count = $leading_space_count
EOM
            return $line;
        }

        my $leading_space_count_test = length($line) - length($trimmed_line);

        # Skip tabbing if actual whitespace is less than expected
        if ( $leading_space_count_test < $leading_space_count ) {
            DEBUG_TABS
              && warning(<<EOM);
Error entabbing: expected count=$leading_space_count but only found $leading_space_count_test for line:
'$line'
EOM
            return $line;
        }

        # Use actual whitespace if it exceeds prediction. This mainly
        # occurs at hanging side comments.
        $leading_space_count = $leading_space_count_test;
    }

    #----------------------------------
    # Handle --entab-leading-whitespace
    #----------------------------------
    if ($rOpts_entab_leading_whitespace) {

        my $space_count =
          $leading_space_count % $rOpts_entab_leading_whitespace;
        my $tab_count =
          int( $leading_space_count / $rOpts_entab_leading_whitespace );
        my $leading_string = "\t" x $tab_count . SPACE x $space_count;
        $line = $leading_string . $trimmed_line;
    }

    #-----------------------------------------------
    # Handle -t (one tab per level; not recommended)
    #-----------------------------------------------
    elsif ( $rOpts_tabs && $level ) {

        my $leading_string = ( "\t" x $level );
        my $space_count = $leading_space_count - $level * $rOpts_indent_columns;

        # shouldn't happen:
        if ( $space_count < 0 ) {

            # But it could be an outdented comment
            if ( $line !~ /^\s*#/ ) {
                DEBUG_TABS
                  && warning(
"Error entabbing in valign_output_step_D: for level=$level count=$leading_space_count\n"
                  );
            }
            $leading_string = ( SPACE x $leading_space_count );
        }
        else {
            $leading_string .= ( SPACE x $space_count );
        }
        $line = $leading_string . $trimmed_line;
    }

    # nothing to do; we should have skipped a call to this sub
    else {
        if (DEVEL_MODE) {
            Fault(
"in tab sub but neither -t nor -et set: check flag 'require_tabs'\n"
            );
        }
    }
    return $line;
} ## end sub add_leading_tabs

sub valign_output_step_D {

    #----------------------------------------------------------------
    # This is Step D in writing vertically aligned lines.
    # It is the end of the vertical alignment pipeline.
    # Write one vertically aligned line of code to the output object.
    #----------------------------------------------------------------

    my ( $self, $line, $leading_space_count, $level, $Kend ) = @_;

    # Convert leading whitespace to use tabs if requested.
    if ( $require_tabs && $leading_space_count > 0 ) {
        $line = add_leading_tabs( $line, $leading_space_count, $level );
    }

    my $file_writer_object = $self->[_file_writer_object_];
    $file_writer_object->write_code_line( $line . "\n", $Kend );

    return;
} ## end sub valign_output_step_D

##########################
# CODE SECTION 12: Summary
##########################

sub report_anything_unusual {
    my $self = shift;

    my $outdented_line_count = $self->[_outdented_line_count_];
    if ( $outdented_line_count > 0 ) {
        write_logfile_entry(
            "$outdented_line_count long lines were outdented:\n");
        my $first_outdented_line_at = $self->[_first_outdented_line_at_];
        write_logfile_entry(
            "  First at output line $first_outdented_line_at\n");

        if ( $outdented_line_count > 1 ) {
            my $last_outdented_line_at = $self->[_last_outdented_line_at_];
            write_logfile_entry(
                "   Last at output line $last_outdented_line_at\n");
        }
        write_logfile_entry(
            "  use -noll to prevent outdenting, -l=n to increase line length\n"
        );
        write_logfile_entry("\n");
    }
    return;
} ## end sub report_anything_unusual

} ## end package Perl::Tidy::VerticalAligner
1;
