#####################################################################
#
# The Perl::Tidy::Formatter package adds indentation, whitespace, and
# line breaks to the token stream
#
#####################################################################

# Index...
# CODE SECTION 1: Preliminary code, global definitions and sub new
#                 sub new
# CODE SECTION 2: Some Basic Utilities
# CODE SECTION 3: Check and process options
#                 sub check_options
# CODE SECTION 4: Receive lines from the tokenizer
#                 sub write_line
# CODE SECTION 5: Pre-process the entire file
#                 sub finish_formatting
# CODE SECTION 6: Process line-by-line
#                 sub process_all_lines
# CODE SECTION 7: Process lines of code
#                 process_line_of_CODE
# CODE SECTION 8: Utilities for setting breakpoints
#                 sub set_forced_breakpoint
# CODE SECTION 9: Process batches of code
#                 sub grind_batch_of_CODE
# CODE SECTION 10: Code to break long statments
#                  sub set_continuation_breaks
# CODE SECTION 11: Code to break long lists
#                  sub scan_list
# CODE SECTION 12: Code for setting indentation
# CODE SECTION 13: Preparing batches for vertical alignment
#                  sub send_lines_to_vertical_aligner
# CODE SECTION 14: Code for creating closing side comments
#                  sub add_closing_side_comment
# CODE SECTION 15: Summarize
#                  sub wrapup

#######################################################################
# CODE SECTION 1: Preliminary code and global definitions up to sub new
#######################################################################

package Perl::Tidy::Formatter;
use strict;
use warnings;

# this can be turned on for extra checking during development
use constant DEVEL_MODE => 0;

{ #<<< A non-indenting brace to contain all lexical variables

use Carp;
our $VERSION = '20210111';

# The Tokenizer will be loaded with the Formatter
##use Perl::Tidy::Tokenizer;    # for is_keyword()

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD =~ /\bDESTROY$/ );
    my ( $pkg, $fname, $lno ) = caller();
    my $my_package = __PACKAGE__;
    print STDERR <<EOM;
======================================================================
Error detected in package '$my_package', version $VERSION
Received unexpected AUTOLOAD call for sub '$AUTOLOAD'
Called from package: '$pkg'  
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
}

sub DESTROY {
    my $self = shift;
    $self->_decrement_count();
    return;
}

sub Die {
    my ($msg) = @_;
    Perl::Tidy::Die($msg);
    croak "unexpected return from Perl::Tidy::Die";
}

sub Warn {
    my ($msg) = @_;
    Perl::Tidy::Warn($msg);
    return;
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
    my $input_stream_name = get_input_stream_name();

    Die(<<EOM);
==============================================================================
While operating on input stream with name: '$input_stream_name'
A fault was detected at line $line0 of sub '$subroutine1'
in file '$filename1'
which was called from line $line1 of sub '$subroutine2'
Message: '$msg'
This is probably an error introduced by a recent programming change. 
Perl::Tidy::Formatter.pm reports VERSION='$VERSION'.
==============================================================================
EOM

    # We shouldn't get here, but this return is to keep Perl-Critic from
    # complaining.
    return;
}

sub Exit {
    my ($msg) = @_;
    Perl::Tidy::Exit($msg);
    croak "unexpected return from Perl::Tidy::Exit";
}

# Global variables ...
my (

    ##################################################################
    # Section 1: Global variables which are either always constant or
    # are constant after being configured by user-supplied
    # parameters.  They remain constant as a file is being processed.
    ##################################################################

    # user parameters and shortcuts
    $rOpts,
    $rOpts_closing_side_comment_maximum_text,
    $rOpts_continuation_indentation,
    $rOpts_indent_columns,
    $rOpts_line_up_parentheses,
    $rOpts_maximum_line_length,
    $rOpts_variable_maximum_line_length,
    $rOpts_block_brace_tightness,
    $rOpts_block_brace_vertical_tightness,
    $rOpts_stack_closing_block_brace,
    $rOpts_maximum_consecutive_blank_lines,

    $rOpts_recombine,
    $rOpts_add_newlines,
    $rOpts_break_at_old_comma_breakpoints,
    $rOpts_ignore_old_breakpoints,

    $rOpts_keep_interior_semicolons,
    $rOpts_comma_arrow_breakpoints,
    $rOpts_maximum_fields_per_table,
    $rOpts_one_line_block_semicolons,
    $rOpts_break_at_old_semicolon_breakpoints,

    $rOpts_tee_side_comments,
    $rOpts_tee_block_comments,
    $rOpts_tee_pod,
    $rOpts_delete_side_comments,
    $rOpts_delete_closing_side_comments,
    $rOpts_format_skipping,
    $rOpts_indent_only,
    $rOpts_static_block_comments,

    # Static hashes initialized in a BEGIN block
    %is_assignment,
    %is_keyword_returning_list,
    %is_if_unless_and_or_last_next_redo_return,
    %is_if_elsif_else_unless_while_until_for_foreach,
    %is_if_unless_while_until_for,
    %is_last_next_redo_return,
    %is_sort_map_grep,
    %is_sort_map_grep_eval,
    %is_if_unless,
    %is_and_or,
    %is_chain_operator,
    %is_block_without_semicolon,
    %ok_to_add_semicolon_for_block_type,
    %is_opening_type,
    %is_closing_type,
    %is_opening_token,
    %is_closing_token,
    %is_equal_or_fat_comma,
    %is_block_with_ci,
    %is_counted_type,
    %is_opening_sequence_token,
    %is_closing_sequence_token,
    %is_container_label_type,

    # Initialized in check_options. These are constants and could
    # just as well be initialized in a BEGIN block.
    %is_do_follower,
    %is_if_brace_follower,
    %is_else_brace_follower,
    %is_anon_sub_brace_follower,
    %is_anon_sub_1_brace_follower,
    %is_other_brace_follower,

    # Initialized in sub initialize_whitespace_hashes;
    # Some can be modified according to user parameters.
    %binary_ws_rules,
    %want_left_space,
    %want_right_space,

    # Configured in sub initialize_bond_strength_hashes
    %right_bond_strength,
    %left_bond_strength,

    # Hashes for -kbb=s and -kba=s
    %keep_break_before_type,
    %keep_break_after_type,

    # Initialized in check_options, modified by prepare_cuddled_block_types:
    %want_one_line_block,

    # Initialized in sub prepare_cuddled_block_types
    $rcuddled_block_types,

    # Initialized and configured in check_optioms
    %outdent_keyword,
    %keyword_paren_inner_tightness,

    %want_break_before,

    %break_before_container_types,
    %container_indentation_options,

    %space_after_keyword,

    %tightness,
    %matching_token,

    %opening_vertical_tightness,
    %closing_vertical_tightness,
    %closing_token_indentation,
    $some_closing_token_indentation,

    %opening_token_right,
    %stack_opening_token,
    %stack_closing_token,

    %weld_nested_exclusion_rules,

    # regex patterns for text identification.
    # Most are initialized in a sub make_**_pattern during configuration.
    # Most can be configured by user parameters.
    $SUB_PATTERN,
    $ASUB_PATTERN,
    $ANYSUB_PATTERN,
    $static_block_comment_pattern,
    $static_side_comment_pattern,
    $format_skipping_pattern_begin,
    $format_skipping_pattern_end,
    $non_indenting_brace_pattern,
    $bli_pattern,
    $block_brace_vertical_tightness_pattern,
    $blank_lines_after_opening_block_pattern,
    $blank_lines_before_closing_block_pattern,
    $keyword_group_list_pattern,
    $keyword_group_list_comment_pattern,
    $closing_side_comment_prefix_pattern,
    $closing_side_comment_list_pattern,

    # Table to efficiently find indentation and max line length
    # from level.  Initialized in sub 'find_nested_pairs'
    @maximum_line_length,

    # Total number of sequence items in a weld, for quick checks
    $total_weld_count,

    #########################################################
    # Section 2: Work arrays for the current batch of tokens.
    #########################################################

    # These are re-initialized for each batch of code
    # in sub initialize_batch_variables.
    $max_index_to_go,
    @block_type_to_go,
    @type_sequence_to_go,
    @container_environment_to_go,
    @bond_strength_to_go,
    @forced_breakpoint_to_go,
    @token_lengths_to_go,
    @summed_lengths_to_go,
    @levels_to_go,
    @leading_spaces_to_go,
    @reduced_spaces_to_go,
    @mate_index_to_go,
    @ci_levels_to_go,
    @nesting_depth_to_go,
    @nobreak_to_go,
    @old_breakpoint_to_go,
    @tokens_to_go,
    @K_to_go,
    @types_to_go,
    @inext_to_go,
    @iprev_to_go,

);

BEGIN {

    # Initialize constants...

    # Array index names for token variables
    my $i = 0;
    use constant {
        _BLOCK_TYPE_            => $i++,
        _CI_LEVEL_              => $i++,
        _CONTAINER_ENVIRONMENT_ => $i++,
        _CUMULATIVE_LENGTH_     => $i++,
        _LINE_INDEX_            => $i++,
        _KNEXT_SEQ_ITEM_        => $i++,
        _LEVEL_                 => $i++,
        _LEVEL_TRUE_            => $i++,
        _SLEVEL_                => $i++,
        _TOKEN_                 => $i++,
        _TOKEN_LENGTH_          => $i++,
        _TYPE_                  => $i++,
        _TYPE_SEQUENCE_         => $i++,

        # Number of token variables; must be last in list:
        _NVARS => $i++,
    };

    # Array index names for $self (which is an array ref)
    $i = 0;
    use constant {
        _rlines_                  => $i++,
        _rlines_new_              => $i++,
        _rLL_                     => $i++,
        _Klimit_                  => $i++,
        _K_opening_container_     => $i++,
        _K_closing_container_     => $i++,
        _K_opening_ternary_       => $i++,
        _K_closing_ternary_       => $i++,
        _K_first_seq_item_        => $i++,
        _rK_phantom_semicolons_   => $i++,
        _rtype_count_by_seqno_    => $i++,
        _ris_broken_container_    => $i++,
        _rhas_broken_container_   => $i++,
        _ris_bli_container_       => $i++,
        _rparent_of_seqno_        => $i++,
        _rchildren_of_seqno_      => $i++,
        _ris_list_by_seqno_       => $i++,
        _rbreak_container_        => $i++,
        _rshort_nested_           => $i++,
        _length_function_         => $i++,
        _is_encoded_data_         => $i++,
        _fh_tee_                  => $i++,
        _sink_object_             => $i++,
        _file_writer_object_      => $i++,
        _vertical_aligner_object_ => $i++,
        _logger_object_           => $i++,
        _radjusted_levels_        => $i++,
        _this_batch_              => $i++,

        _last_output_short_opening_token_ => $i++,

        _last_line_leading_type_       => $i++,
        _last_line_leading_level_      => $i++,
        _last_last_line_leading_level_ => $i++,

        _added_semicolon_count_    => $i++,
        _first_added_semicolon_at_ => $i++,
        _last_added_semicolon_at_  => $i++,

        _deleted_semicolon_count_    => $i++,
        _first_deleted_semicolon_at_ => $i++,
        _last_deleted_semicolon_at_  => $i++,

        _embedded_tab_count_    => $i++,
        _first_embedded_tab_at_ => $i++,
        _last_embedded_tab_at_  => $i++,

        _first_tabbing_disagreement_       => $i++,
        _last_tabbing_disagreement_        => $i++,
        _tabbing_disagreement_count_       => $i++,
        _in_tabbing_disagreement_          => $i++,
        _first_brace_tabbing_disagreement_ => $i++,
        _in_brace_tabbing_disagreement_    => $i++,

        _saw_VERSION_in_this_file_ => $i++,
        _saw_END_or_DATA_          => $i++,

        _rweld_len_left_closing_  => $i++,
        _rweld_len_right_closing_ => $i++,
        _rweld_len_left_opening_  => $i++,
        _rweld_len_right_opening_ => $i++,
        _ris_welded_seqno_        => $i++,

        _rspecial_side_comment_type_ => $i++,

        _rseqno_controlling_my_ci_ => $i++,
        _ris_seqno_controlling_ci_ => $i++,
        _save_logfile_             => $i++,
        _maximum_level_            => $i++,

        _rKrange_code_without_comments_ => $i++,
        _rbreak_before_Kfirst_          => $i++,
        _rbreak_after_Klast_            => $i++,
        _converged_                     => $i++,

        _rstarting_multiline_qw_seqno_by_K_ => $i++,
        _rending_multiline_qw_seqno_by_K_   => $i++,
        _rKrange_multiline_qw_by_seqno_     => $i++,
        _rcontains_multiline_qw_by_seqno_   => $i++,
        _rmultiline_qw_has_extra_level_     => $i++,
    };

    # Array index names for _this_batch_ (in above list)
    # So _this_batch_ is a sub-array of $self for
    # holding the batches of tokens being processed.
    $i = 0;
    use constant {
        _starting_in_quote_        => $i++,
        _ending_in_quote_          => $i++,
        _is_static_block_comment_  => $i++,
        _rlines_K_                 => $i++,
        _do_not_pad_               => $i++,
        _ibeg0_                    => $i++,
        _peak_batch_size_          => $i++,
        _max_index_to_go_          => $i++,
        _rK_to_go_                 => $i++,
        _batch_count_              => $i++,
        _rix_seqno_controlling_ci_ => $i++,
        _batch_CODE_type_          => $i++,
    };

    # Sequence number assigned to the root of sequence tree.
    # The minimum of the actual sequences numbers is 4, so we can use 1
    use constant SEQ_ROOT => 1;

    # Codes for insertion and deletion of blanks
    use constant DELETE => 0;
    use constant STABLE => 1;
    use constant INSERT => 2;

    # whitespace codes
    use constant WS_YES      => 1;
    use constant WS_OPTIONAL => 0;
    use constant WS_NO       => -1;

    # Token bond strengths.
    use constant NO_BREAK    => 10000;
    use constant VERY_STRONG => 100;
    use constant STRONG      => 2.1;
    use constant NOMINAL     => 1.1;
    use constant WEAK        => 0.8;
    use constant VERY_WEAK   => 0.55;

    # values for testing indexes in output array
    use constant UNDEFINED_INDEX => -1;

    # Maximum number of little messages; probably need not be changed.
    use constant MAX_NAG_MESSAGES => 6;

    # increment between sequence numbers for each type
    # For example, ?: pairs might have numbers 7,11,15,...
    use constant TYPE_SEQUENCE_INCREMENT => 4;

    # Initialize constant hashes ...
    my @q;

    @q = qw(
      = **= += *= &= <<= &&=
      -= /= |= >>= ||= //=
      .= %= ^=
      x=
    );
    @is_assignment{@q} = (1) x scalar(@q);

    @q = qw(
      grep
      keys
      map
      reverse
      sort
      split
    );
    @is_keyword_returning_list{@q} = (1) x scalar(@q);

    @q = qw(is if unless and or err last next redo return);
    @is_if_unless_and_or_last_next_redo_return{@q} = (1) x scalar(@q);

    # These block types may have text between the keyword and opening
    # curly.  Note: 'else' does not, but must be included to allow trailing
    # if/elsif text to be appended.
    # patch for SWITCH/CASE: added 'case' and 'when'
    @q = qw(if elsif else unless while until for foreach case when catch);
    @is_if_elsif_else_unless_while_until_for_foreach{@q} =
      (1) x scalar(@q);

    @q = qw(if unless while until for);
    @is_if_unless_while_until_for{@q} =
      (1) x scalar(@q);

    @q = qw(last next redo return);
    @is_last_next_redo_return{@q} = (1) x scalar(@q);

    @q = qw(sort map grep);
    @is_sort_map_grep{@q} = (1) x scalar(@q);

    @q = qw(sort map grep eval);
    @is_sort_map_grep_eval{@q} = (1) x scalar(@q);

    @q = qw(if unless);
    @is_if_unless{@q} = (1) x scalar(@q);

    @q = qw(and or err);
    @is_and_or{@q} = (1) x scalar(@q);

    # Identify certain operators which often occur in chains.
    # Note: the minus (-) causes a side effect of padding of the first line in
    # something like this (by sub set_logical_padding):
    #    Checkbutton => 'Transmission checked',
    #   -variable    => \$TRANS
    # This usually improves appearance so it seems ok.
    @q = qw(&& || and or : ? . + - * /);
    @is_chain_operator{@q} = (1) x scalar(@q);

    # We can remove semicolons after blocks preceded by these keywords
    @q =
      qw(BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue if elsif else
      unless while until for foreach given when default);
    @is_block_without_semicolon{@q} = (1) x scalar(@q);

    # We will allow semicolons to be added within these block types
    # as well as sub and package blocks.
    # NOTES:
    # 1. Note that these keywords are omitted:
    #     switch case given when default sort map grep
    # 2. It is also ok to add for sub and package blocks and a labeled block
    # 3. But not okay for other perltidy types including:
    #     { } ; G t
    # 4. Test files: blktype.t, blktype1.t, semicolon.t
    @q =
      qw( BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue if elsif else
      unless do while until eval for foreach );
    @ok_to_add_semicolon_for_block_type{@q} = (1) x scalar(@q);

    # 'L' is token for opening { at hash key
    @q = qw< L { ( [ >;
    @is_opening_type{@q} = (1) x scalar(@q);

    # 'R' is token for closing } at hash key
    @q = qw< R } ) ] >;
    @is_closing_type{@q} = (1) x scalar(@q);

    @q = qw< { ( [ >;
    @is_opening_token{@q} = (1) x scalar(@q);

    @q = qw< } ) ] >;
    @is_closing_token{@q} = (1) x scalar(@q);

    @q = qw< { ( [ ? >;
    @is_opening_sequence_token{@q} = (1) x scalar(@q);

    @q = qw< } ) ] : >;
    @is_closing_sequence_token{@q} = (1) x scalar(@q);

    # a hash needed by sub scan_list for labeling containers
    @q = qw( k => && || ? : . );
    @is_container_label_type{@q} = (1) x scalar(@q);

    # Braces -bbht etc must follow these. Note: experimentation with
    # including a simple comma shows that it adds little and can lead
    # to poor formatting in complex lists.
    @q = qw( = => );
    @is_equal_or_fat_comma{@q} = (1) x scalar(@q);

    @q = qw( => ; );
    push @q, ',';
    @is_counted_type{@q} = (1) x scalar(@q);

    # These block types can take ci.  This is used by the -xci option.
    # Note that the 'sub' in this list is an anonymous sub.  To be more correct
    # we could remove sub and use ASUB pattern to also handle a
    # prototype/signature.  But that would slow things down and would probably
    # never be useful.
    @q = qw( do sub eval sort map grep );
    @is_block_with_ci{@q} = (1) x scalar(@q);

}

{    ## begin closure to count instanes

    # methods to count instances
    my $_count = 0;
    sub get_count        { return $_count; }
    sub _increment_count { return ++$_count }
    sub _decrement_count { return --$_count }
} ## end closure to count instanes

sub new {

    my ( $class, @args ) = @_;

    # we are given an object with a write_line() method to take lines
    my %defaults = (
        sink_object        => undef,
        diagnostics_object => undef,
        logger_object      => undef,
        length_function    => sub { return length( $_[0] ) },
        is_encoded_data    => "",
        fh_tee             => undef,
    );
    my %args = ( %defaults, @args );

    my $length_function    = $args{length_function};
    my $is_encoded_data    = $args{is_encoded_data};
    my $fh_tee             = $args{fh_tee};
    my $logger_object      = $args{logger_object};
    my $diagnostics_object = $args{diagnostics_object};

    # we create another object with a get_line() and peek_ahead() method
    my $sink_object = $args{sink_object};
    my $file_writer_object =
      Perl::Tidy::FileWriter->new( $sink_object, $rOpts, $logger_object );

    # initialize closure variables...
    set_logger_object($logger_object);
    set_diagnostics_object($diagnostics_object);
    initialize_gnu_vars();
    initialize_csc_vars();
    initialize_scan_list();
    initialize_saved_opening_indentation();
    initialize_undo_ci();
    initialize_process_line_of_CODE();
    initialize_grind_batch_of_CODE();
    initialize_adjusted_indentation();
    initialize_postponed_breakpoint();
    initialize_batch_variables();
    initialize_forced_breakpoint_vars();
    initialize_gnu_batch_vars();
    initialize_write_line();

    my $vertical_aligner_object = Perl::Tidy::VerticalAligner->new(
        rOpts              => $rOpts,
        file_writer_object => $file_writer_object,
        logger_object      => $logger_object,
        diagnostics_object => $diagnostics_object,
        length_function    => $length_function
    );

    if ( $rOpts->{'entab-leading-whitespace'} ) {
        write_logfile_entry(
"Leading whitespace will be entabbed with $rOpts->{'entab-leading-whitespace'} spaces per tab\n"
        );
    }
    elsif ( $rOpts->{'tabs'} ) {
        write_logfile_entry("Indentation will be with a tab character\n");
    }
    else {
        write_logfile_entry(
            "Indentation will be with $rOpts->{'indent-columns'} spaces\n");
    }

    # Initialize the $self array reference.
    # To add an item, first add a constant index in the BEGIN block above.
    my $self = [];

    # Basic data structures...
    $self->[_rlines_]     = [];       # = ref to array of lines of the file
    $self->[_rlines_new_] = [];       # = ref to array of output lines
                                      #   (FOR FUTURE DEVELOPMENT)
    $self->[_rLL_]        = [];       # = ref to array with all tokens
                                      # in the file. LL originally meant
                                      # 'Linked List'. Linked lists were a
                                      # bad idea but LL is easy to type.
    $self->[_Klimit_]     = undef;    # = maximum K index for rLL.
    $self->[_K_opening_container_] = {};    # for quickly traversing structure
    $self->[_K_closing_container_] = {};    # for quickly traversing structure
    $self->[_K_opening_ternary_]   = {};    # for quickly traversing structure
    $self->[_K_closing_ternary_]   = {};    # for quickly traversing structure
    $self->[_K_first_seq_item_]    = undef; # K of first token with a sequence #
    $self->[_rK_phantom_semicolons_] =
      undef;    # for undoing phantom semicolons if iterating
    $self->[_rtype_count_by_seqno_]  = {};
    $self->[_ris_broken_container_]  = {};
    $self->[_rhas_broken_container_] = {};
    $self->[_ris_bli_container_]     = {};
    $self->[_rparent_of_seqno_]      = {};
    $self->[_rchildren_of_seqno_]    = {};
    $self->[_ris_list_by_seqno_]     = {};
    $self->[_rbreak_container_]      = {};    # prevent one-line blocks
    $self->[_rshort_nested_]         = {};    # blocks not forced open
    $self->[_length_function_]       = $length_function;
    $self->[_is_encoded_data_]       = $is_encoded_data;

    # Some objects...
    $self->[_fh_tee_]                  = $fh_tee;
    $self->[_sink_object_]             = $sink_object;
    $self->[_file_writer_object_]      = $file_writer_object;
    $self->[_vertical_aligner_object_] = $vertical_aligner_object;
    $self->[_logger_object_]           = $logger_object;

    # Reference to the batch being processed
    $self->[_this_batch_] = [];

    # Memory of processed text...
    $self->[_last_last_line_leading_level_]    = 0;
    $self->[_last_line_leading_level_]         = 0;
    $self->[_last_line_leading_type_]          = '#';
    $self->[_last_output_short_opening_token_] = 0;
    $self->[_added_semicolon_count_]           = 0;
    $self->[_first_added_semicolon_at_]        = 0;
    $self->[_last_added_semicolon_at_]         = 0;
    $self->[_deleted_semicolon_count_]         = 0;
    $self->[_first_deleted_semicolon_at_]      = 0;
    $self->[_last_deleted_semicolon_at_]       = 0;
    $self->[_embedded_tab_count_]              = 0;
    $self->[_first_embedded_tab_at_]           = 0;
    $self->[_last_embedded_tab_at_]            = 0;
    $self->[_first_tabbing_disagreement_]      = 0;
    $self->[_last_tabbing_disagreement_]       = 0;
    $self->[_tabbing_disagreement_count_]      = 0;
    $self->[_in_tabbing_disagreement_]         = 0;
    $self->[_saw_VERSION_in_this_file_]        = !$rOpts->{'pass-version-line'};
    $self->[_saw_END_or_DATA_]                 = 0;

    # Hashes related to container welding...
    $self->[_radjusted_levels_]        = [];
    $self->[_rweld_len_left_closing_]  = {};
    $self->[_rweld_len_right_closing_] = {};
    $self->[_rweld_len_left_opening_]  = {};
    $self->[_rweld_len_right_opening_] = {};
    $self->[_ris_welded_seqno_]        = {};

    $self->[_rseqno_controlling_my_ci_] = {};
    $self->[_ris_seqno_controlling_ci_] = {};

    $self->[_rspecial_side_comment_type_] = {};
    $self->[_maximum_level_]              = 0;

    $self->[_rKrange_code_without_comments_] = [];
    $self->[_rbreak_before_Kfirst_]          = {};
    $self->[_rbreak_after_Klast_]            = {};
    $self->[_converged_]                     = 0;

    $self->[_rstarting_multiline_qw_seqno_by_K_] = {};
    $self->[_rending_multiline_qw_seqno_by_K_]   = {};
    $self->[_rKrange_multiline_qw_by_seqno_]     = {};
    $self->[_rcontains_multiline_qw_by_seqno_]   = {};
    $self->[_rmultiline_qw_has_extra_level_]     = {};

    # This flag will be updated later by a call to get_save_logfile()
    $self->[_save_logfile_] = defined($logger_object);

    bless $self, $class;

    # Safety check..this is not a class yet
    if ( _increment_count() > 1 ) {
        confess
"Attempt to create more than 1 object in $class, which is not a true class yet\n";
    }
    return $self;
}

######################################
# CODE SECTION 2: Some Basic Utilities
######################################

{    ## begin closure for logger routines
    my $logger_object;

    # Called once per file to initialize the logger object
    sub set_logger_object {
        $logger_object = shift;
        return;
    }

    sub get_logger_object {
        return $logger_object;
    }

    sub get_input_stream_name {
        my $input_stream_name = "";
        if ($logger_object) {
            $input_stream_name = $logger_object->get_input_stream_name();
        }
        return $input_stream_name;
    }

    # interface to Perl::Tidy::Logger routines
    sub warning {
        my ($msg) = @_;
        if ($logger_object) { $logger_object->warning($msg); }
        return;
    }

    sub complain {
        my ($msg) = @_;
        if ($logger_object) {
            $logger_object->complain($msg);
        }
        return;
    }

    sub write_logfile_entry {
        my @msg = @_;
        if ($logger_object) {
            $logger_object->write_logfile_entry(@msg);
        }
        return;
    }

    sub report_definite_bug {
        if ($logger_object) {
            $logger_object->report_definite_bug();
        }
        return;
    }

    sub get_saw_brace_error {
        if ($logger_object) {
            return $logger_object->get_saw_brace_error();
        }
        return;
    }

    sub we_are_at_the_last_line {
        if ($logger_object) {
            $logger_object->we_are_at_the_last_line();
        }
        return;
    }

} ## end closure for logger routines

{    ## begin closure for diagnostics routines
    my $diagnostics_object;

    # Called once per file to initialize the diagnostics object
    sub set_diagnostics_object {
        $diagnostics_object = shift;
        return;
    }

    sub write_diagnostics {
        my ($msg) = @_;
        if ($diagnostics_object) {
            $diagnostics_object->write_diagnostics($msg);
        }
        return;
    }
} ## end closure for diagnostics routines

sub get_convergence_check {
    my ($self) = @_;
    return $self->[_converged_];
}

sub get_added_semicolon_count {
    my $self = shift;
    return $self->[_added_semicolon_count_];
}

sub get_output_line_number {
    my ($self) = @_;
    my $vao = $self->[_vertical_aligner_object_];
    return $vao->get_output_line_number();
}

sub check_token_array {
    my $self = shift;

    # Check for errors in the array of tokens. This is only called now
    # when the DEVEL_MODE flag is set, so this Fault will only occur
    # during code development.
    my $rLL = $self->[_rLL_];
    for ( my $KK = 0 ; $KK < @{$rLL} ; $KK++ ) {
        my $nvars = @{ $rLL->[$KK] };
        if ( $nvars != _NVARS ) {
            my $NVARS = _NVARS;
            my $type  = $rLL->[$KK]->[_TYPE_];
            $type = '*' unless defined($type);

            # The number of variables per token node is _NVARS and was set when
            # the array indexes were generated. So if the number of variables
            # is different we have done something wrong, like not store all of
            # them in sub 'write_line' when they were received from the
            # tokenizer.
            Fault(
"number of vars for node $KK, type '$type', is $nvars but should be $NVARS"
            );
        }
        foreach my $var ( _TOKEN_, _TYPE_ ) {
            if ( !defined( $rLL->[$KK]->[$var] ) ) {
                my $iline = $rLL->[$KK]->[_LINE_INDEX_];

                # This is a simple check that each token has some basic
                # variables.  In other words, that there are no holes in the
                # array of tokens.  Sub 'write_line' pushes tokens into the
                # $rLL array, so this should guarantee no gaps.
                Fault("Undefined variable $var for K=$KK, line=$iline\n");
            }
        }
    }
    return;
}

sub want_blank_line {
    my $self = shift;
    $self->flush();
    my $file_writer_object = $self->[_file_writer_object_];
    $file_writer_object->want_blank_line();
    return;
}

sub write_unindented_line {
    my ( $self, $line ) = @_;
    $self->flush();
    my $file_writer_object = $self->[_file_writer_object_];
    $file_writer_object->write_line($line);
    return;
}

sub consecutive_nonblank_lines {
    my ($self)             = @_;
    my $file_writer_object = $self->[_file_writer_object_];
    my $vao                = $self->[_vertical_aligner_object_];
    return $file_writer_object->get_consecutive_nonblank_lines() +
      $vao->get_cached_line_count();
}

sub trim {

    # trim leading and trailing whitespace from a string
    my $str = shift;
    $str =~ s/\s+$//;
    $str =~ s/^\s+//;
    return $str;
}

sub max {
    my (@vals) = @_;
    my $max = shift @vals;
    for (@vals) { $max = $_ > $max ? $_ : $max }
    return $max;
}

sub min {
    my (@vals) = @_;
    my $min = shift @vals;
    for (@vals) { $min = $_ < $min ? $_ : $min }
    return $min;
}

sub split_words {

    # given a string containing words separated by whitespace,
    # return the list of words
    my ($str) = @_;
    return unless $str;
    $str =~ s/\s+$//;
    $str =~ s/^\s+//;
    return split( /\s+/, $str );
}

###########################################
# CODE SECTION 3: Check and process options
###########################################

sub check_options {

    # This routine is called to check the user-supplied run parameters
    # and to configure the control hashes to them.
    $rOpts = shift;

    initialize_whitespace_hashes();
    initialize_bond_strength_hashes();

    # Make needed regex patterns for matching text.
    # NOTE: sub_matching_patterns must be made first because later patterns use
    # them; see RT #133130.
    make_sub_matching_pattern();
    make_static_block_comment_pattern();
    make_static_side_comment_pattern();
    make_closing_side_comment_prefix();
    make_closing_side_comment_list_pattern();
    $format_skipping_pattern_begin =
      make_format_skipping_pattern( 'format-skipping-begin', '#<<<' );
    $format_skipping_pattern_end =
      make_format_skipping_pattern( 'format-skipping-end', '#>>>' );
    make_non_indenting_brace_pattern();

    # If closing side comments ARE selected, then we can safely
    # delete old closing side comments unless closing side comment
    # warnings are requested.  This is a good idea because it will
    # eliminate any old csc's which fall below the line count threshold.
    # We cannot do this if warnings are turned on, though, because we
    # might delete some text which has been added.  So that must
    # be handled when comments are created.  And we cannot do this
    # with -io because -csc will be skipped altogether.
    if ( $rOpts->{'closing-side-comments'} ) {
        if (   !$rOpts->{'closing-side-comment-warnings'}
            && !$rOpts->{'indent-only'} )
        {
            $rOpts->{'delete-closing-side-comments'} = 1;
        }
    }

    # If closing side comments ARE NOT selected, but warnings ARE
    # selected and we ARE DELETING csc's, then we will pretend to be
    # adding with a huge interval.  This will force the comments to be
    # generated for comparison with the old comments, but not added.
    elsif ( $rOpts->{'closing-side-comment-warnings'} ) {
        if ( $rOpts->{'delete-closing-side-comments'} ) {
            $rOpts->{'delete-closing-side-comments'}  = 0;
            $rOpts->{'closing-side-comments'}         = 1;
            $rOpts->{'closing-side-comment-interval'} = 100000000;
        }
    }

    make_bli_pattern();
    make_block_brace_vertical_tightness_pattern();
    make_blank_line_pattern();
    make_keyword_group_list_pattern();

    # Make initial list of desired one line block types
    # They will be modified by 'prepare_cuddled_block_types'
    %want_one_line_block = %is_sort_map_grep_eval;

    prepare_cuddled_block_types();
    if ( $rOpts->{'dump-cuddled-block-list'} ) {
        dump_cuddled_block_list(*STDOUT);
        Exit(0);
    }

    if ( $rOpts->{'line-up-parentheses'} ) {

        if (   $rOpts->{'indent-only'}
            || !$rOpts->{'add-newlines'}
            || !$rOpts->{'delete-old-newlines'} )
        {
            Warn(<<EOM);
-----------------------------------------------------------------------
Conflict: -lp  conflicts with -io, -fnl, -nanl, or -ndnl; ignoring -lp
    
The -lp indentation logic requires that perltidy be able to coordinate
arbitrarily large numbers of line breakpoints.  This isn't possible
with these flags.
-----------------------------------------------------------------------
EOM
            $rOpts->{'line-up-parentheses'} = 0;
        }

        if ( $rOpts->{'whitespace-cycle'} ) {
            Warn(<<EOM);
Conflict: -wc cannot currently be used with the -lp option; ignoring -wc
EOM
            $rOpts->{'whitespace-cycle'} = 0;
        }
    }

    # At present, tabs are not compatible with the line-up-parentheses style
    # (it would be possible to entab the total leading whitespace
    # just prior to writing the line, if desired).
    if ( $rOpts->{'line-up-parentheses'} && $rOpts->{'tabs'} ) {
        Warn(<<EOM);
Conflict: -t (tabs) cannot be used with the -lp  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    # Likewise, tabs are not compatible with outdenting..
    if ( $rOpts->{'outdent-keywords'} && $rOpts->{'tabs'} ) {
        Warn(<<EOM);
Conflict: -t (tabs) cannot be used with the -okw options; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( $rOpts->{'outdent-labels'} && $rOpts->{'tabs'} ) {
        Warn(<<EOM);
Conflict: -t (tabs) cannot be used with the -ola  option; ignoring -t; see -et.
EOM
        $rOpts->{'tabs'} = 0;
    }

    if ( !$rOpts->{'space-for-semicolon'} ) {
        $want_left_space{'f'} = -1;
    }

    if ( $rOpts->{'space-terminal-semicolon'} ) {
        $want_left_space{';'} = 1;
    }

    # We should put an upper bound on any -sil=n value. Otherwise enormous
    # files could be created by mistake.
    for ( $rOpts->{'starting-indentation-level'} ) {
        if ( $_ && $_ > 100 ) {
            Warn(<<EOM);
The value --starting-indentation-level=$_ is very large; a mistake? resetting to 0;
EOM
            $_ = 0;
        }
    }

    # implement outdenting preferences for keywords
    %outdent_keyword = ();
    my @okw = split_words( $rOpts->{'outdent-keyword-list'} );
    unless (@okw) {
        @okw = qw(next last redo goto return);    # defaults
    }

    # FUTURE: if not a keyword, assume that it is an identifier
    foreach (@okw) {
        if ( $Perl::Tidy::Tokenizer::is_keyword{$_} ) {
            $outdent_keyword{$_} = 1;
        }
        else {
            Warn("ignoring '$_' in -okwl list; not a perl keyword");
        }
    }

    # setup hash for -kpit option
    %keyword_paren_inner_tightness = ();
    my $kpit_value = $rOpts->{'keyword-paren-inner-tightness'};
    if ( defined($kpit_value) && $kpit_value != 1 ) {
        my @kpit =
          split_words( $rOpts->{'keyword-paren-inner-tightness-list'} );
        unless (@kpit) {
            @kpit = qw(if elsif unless while until for foreach);    # defaults
        }

        # we will allow keywords and user-defined identifiers
        foreach (@kpit) {
            $keyword_paren_inner_tightness{$_} = $kpit_value;
        }
    }

    # implement user whitespace preferences
    if ( my @q = split_words( $rOpts->{'want-left-space'} ) ) {
        @want_left_space{@q} = (1) x scalar(@q);
    }

    if ( my @q = split_words( $rOpts->{'want-right-space'} ) ) {
        @want_right_space{@q} = (1) x scalar(@q);
    }

    if ( my @q = split_words( $rOpts->{'nowant-left-space'} ) ) {
        @want_left_space{@q} = (-1) x scalar(@q);
    }

    if ( my @q = split_words( $rOpts->{'nowant-right-space'} ) ) {
        @want_right_space{@q} = (-1) x scalar(@q);
    }
    if ( $rOpts->{'dump-want-left-space'} ) {
        dump_want_left_space(*STDOUT);
        Exit(0);
    }

    if ( $rOpts->{'dump-want-right-space'} ) {
        dump_want_right_space(*STDOUT);
        Exit(0);
    }

    # default keywords for which space is introduced before an opening paren
    # (at present, including them messes up vertical alignment)
    my @sak = qw(my local our and or xor err eq ne if else elsif until
      unless while for foreach return switch case given when catch);
    %space_after_keyword = map { $_ => 1 } @sak;

    # first remove any or all of these if desired
    if ( my @q = split_words( $rOpts->{'nospace-after-keyword'} ) ) {

        # -nsak='*' selects all the above keywords
        if ( @q == 1 && $q[0] eq '*' ) { @q = keys(%space_after_keyword) }
        @space_after_keyword{@q} = (0) x scalar(@q);
    }

    # then allow user to add to these defaults
    if ( my @q = split_words( $rOpts->{'space-after-keyword'} ) ) {
        @space_after_keyword{@q} = (1) x scalar(@q);
    }

    # implement user break preferences
    my @all_operators = qw(% + - * / x != == >= <= =~ !~ < > | &
      = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
      . : ? && || and or err xor
    );

    my $break_after = sub {
        my @toks = @_;
        foreach my $tok (@toks) {
            if ( $tok eq '?' ) { $tok = ':' }    # patch to coordinate ?/:
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $lbs < $rbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    };

    my $break_before = sub {
        my @toks = @_;
        foreach my $tok (@toks) {
            my $lbs = $left_bond_strength{$tok};
            my $rbs = $right_bond_strength{$tok};
            if ( defined($lbs) && defined($rbs) && $rbs < $lbs ) {
                ( $right_bond_strength{$tok}, $left_bond_strength{$tok} ) =
                  ( $lbs, $rbs );
            }
        }
    };

    $break_after->(@all_operators) if ( $rOpts->{'break-after-all-operators'} );
    $break_before->(@all_operators)
      if ( $rOpts->{'break-before-all-operators'} );

    $break_after->( split_words( $rOpts->{'want-break-after'} ) );
    $break_before->( split_words( $rOpts->{'want-break-before'} ) );

    # make note if breaks are before certain key types
    %want_break_before = ();
    foreach my $tok ( @all_operators, ',' ) {
        $want_break_before{$tok} =
          $left_bond_strength{$tok} < $right_bond_strength{$tok};
    }

    # Coordinate ?/: breaks, which must be similar
    if ( !$want_break_before{':'} ) {
        $want_break_before{'?'}   = $want_break_before{':'};
        $right_bond_strength{'?'} = $right_bond_strength{':'} + 0.01;
        $left_bond_strength{'?'}  = NO_BREAK;
    }

    # Only make a hash entry for the next parameters if values are defined.
    # That allows a quick check to be made later.
    %break_before_container_types = ();
    for ( $rOpts->{'break-before-hash-brace'} ) {
        $break_before_container_types{'{'} = $_ if $_ && $_ > 0;
    }
    for ( $rOpts->{'break-before-square-bracket'} ) {
        $break_before_container_types{'['} = $_ if $_ && $_ > 0;
    }
    for ( $rOpts->{'break-before-paren'} ) {
        $break_before_container_types{'('} = $_ if $_ && $_ > 0;
    }

    %container_indentation_options = ();
    for ( $rOpts->{'break-before-hash-brace-and-indent'} ) {
        my $tok = '{';
        if ( defined($_) && $_ > 0 && $break_before_container_types{$tok} ) {
            $container_indentation_options{$tok} = $_;
        }
    }
    for ( $rOpts->{'break-before-square-bracket-and-indent'} ) {
        my $tok = '[';
        if ( defined($_) && $_ > 0 && $break_before_container_types{$tok} ) {
            $container_indentation_options{$tok} = $_;
        }
    }
    for ( $rOpts->{'break-before-paren-and-indent'} ) {
        my $tok = '(';
        if ( defined($_) && $_ > 0 && $break_before_container_types{$tok} ) {
            $container_indentation_options{$tok} = $_;
        }
    }

    # Define here tokens which may follow the closing brace of a do statement
    # on the same line, as in:
    #   } while ( $something);
    my @dof = qw(until while unless if ; : );
    push @dof, ',';
    @is_do_follower{@dof} = (1) x scalar(@dof);

    # What tokens may follow the closing brace of an if or elsif block?
    # Not used. Previously used for cuddled else, but no longer needed.
    %is_if_brace_follower = ();

    # nothing can follow the closing curly of an else { } block:
    %is_else_brace_follower = ();

    # what can follow a multi-line anonymous sub definition closing curly:
    my @asf = qw# ; : => or and  && || ~~ !~~ ) #;
    push @asf, ',';
    @is_anon_sub_brace_follower{@asf} = (1) x scalar(@asf);

    # what can follow a one-line anonymous sub closing curly:
    # one-line anonymous subs also have ']' here...
    # see tk3.t and PP.pm
    my @asf1 = qw#  ; : => or and  && || ) ] ~~ !~~ #;
    push @asf1, ',';
    @is_anon_sub_1_brace_follower{@asf1} = (1) x scalar(@asf1);

    # What can follow a closing curly of a block
    # which is not an if/elsif/else/do/sort/map/grep/eval/sub
    # Testfiles: 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl'
    my @obf = qw#  ; : => or and  && || ) #;
    push @obf, ',';
    @is_other_brace_follower{@obf} = (1) x scalar(@obf);

    $right_bond_strength{'{'} = WEAK;
    $left_bond_strength{'{'}  = VERY_STRONG;

    # make -l=0  equal to -l=infinite
    if ( !$rOpts->{'maximum-line-length'} ) {
        $rOpts->{'maximum-line-length'} = 1000000;
    }

    # make -lbl=0  equal to -lbl=infinite
    if ( !$rOpts->{'long-block-line-count'} ) {
        $rOpts->{'long-block-line-count'} = 1000000;
    }

    my $ole = $rOpts->{'output-line-ending'};
    if ($ole) {
        my %endings = (
            dos  => "\015\012",
            win  => "\015\012",
            mac  => "\015",
            unix => "\012",
        );

        # Patch for RT #99514, a memoization issue.
        # Normally, the user enters one of 'dos', 'win', etc, and we change the
        # value in the options parameter to be the corresponding line ending
        # character.  But, if we are using memoization, on later passes through
        # here the option parameter will already have the desired ending
        # character rather than the keyword 'dos', 'win', etc.  So
        # we must check to see if conversion has already been done and, if so,
        # bypass the conversion step.
        my %endings_inverted = (
            "\015\012" => 'dos',
            "\015\012" => 'win',
            "\015"     => 'mac',
            "\012"     => 'unix',
        );

        if ( defined( $endings_inverted{$ole} ) ) {

            # we already have valid line ending, nothing more to do
        }
        else {
            $ole = lc $ole;
            unless ( $rOpts->{'output-line-ending'} = $endings{$ole} ) {
                my $str = join " ", keys %endings;
                Die(<<EOM);
Unrecognized line ending '$ole'; expecting one of: $str
EOM
            }
            if ( $rOpts->{'preserve-line-endings'} ) {
                Warn("Ignoring -ple; conflicts with -ole\n");
                $rOpts->{'preserve-line-endings'} = undef;
            }
        }
    }

    # hashes used to simplify setting whitespace
    %tightness = (
        '{' => $rOpts->{'brace-tightness'},
        '}' => $rOpts->{'brace-tightness'},
        '(' => $rOpts->{'paren-tightness'},
        ')' => $rOpts->{'paren-tightness'},
        '[' => $rOpts->{'square-bracket-tightness'},
        ']' => $rOpts->{'square-bracket-tightness'},
    );
    %matching_token = (
        '{' => '}',
        '(' => ')',
        '[' => ']',
        '?' => ':',
    );

    # note any requested old line breaks to keep
    %keep_break_before_type = ();
    %keep_break_after_type  = ();
    if ( !$rOpts->{'ignore-old-breakpoints'} ) {

        # FIXME: could check for valid types here.
        # Invalid types are harmless but probably not intended.
        my @types;
        @types = ( split_words( $rOpts->{'keep-old-breakpoints-before'} ) );
        @keep_break_before_type{@types} = (1) x scalar(@types);
        @types = ( split_words( $rOpts->{'keep-old-breakpoints-after'} ) );
        @keep_break_after_type{@types} = (1) x scalar(@types);
    }
    else {
        if ( $rOpts->{'break-at-old-method-breakpoints'} ) {
            Warn("Conflicting parameters: -iob and -bom; -bom will be ignored\n"
            );
        }
        if ( $rOpts->{'break-at-old-comma-breakpoints'} ) {
            Warn("Conflicting parameters: -iob and -boc; -boc will be ignored\n"
            );
        }
        if ( $rOpts->{'break-at-old-semicolon-breakpoints'} ) {
            Warn("Conflicting parameters: -iob and -bos; -bos will be ignored\n"
            );
        }
        if ( $rOpts->{'keep-old-breakpoints-before'} ) {
            Warn("Conflicting parameters: -iob and -kbb; -kbb will be ignored\n"
            );
        }
        if ( $rOpts->{'keep-old-breakpoints-after'} ) {
            Warn("Conflicting parameters: -iob and -kba; -kba will be ignored\n"
            );
        }

        # Note: there are additional parameters that can be made inactive by
        # -iob, but they are on by default so we would generate excessive
        # warnings if we noted them. They are:
        # $rOpts->{'break-at-old-keyword-breakpoints'}
        # $rOpts->{'break-at-old-logical-breakpoints'}
        # $rOpts->{'break-at-old-ternary-breakpoints'}
        # $rOpts->{'break-at-old-attribute-breakpoints'}
    }

    # very frequently used parameters made global for efficiency
    $rOpts_closing_side_comment_maximum_text =
      $rOpts->{'closing-side-comment-maximum-text'};
    $rOpts_continuation_indentation = $rOpts->{'continuation-indentation'};
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_line_up_parentheses      = $rOpts->{'line-up-parentheses'};
    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};
    $rOpts_variable_maximum_line_length =
      $rOpts->{'variable-maximum-line-length'};
    $rOpts_block_brace_tightness = $rOpts->{'block-brace-tightness'};
    $rOpts_block_brace_vertical_tightness =
      $rOpts->{'block-brace-vertical-tightness'};
    $rOpts_stack_closing_block_brace = $rOpts->{'stack-closing-block-brace'};
    $rOpts_maximum_consecutive_blank_lines =
      $rOpts->{'maximum-consecutive-blank-lines'};
    $rOpts_recombine    = $rOpts->{'recombine'};
    $rOpts_add_newlines = $rOpts->{'add-newlines'};
    $rOpts_break_at_old_comma_breakpoints =
      $rOpts->{'break-at-old-comma-breakpoints'};
    $rOpts_ignore_old_breakpoints    = $rOpts->{'ignore-old-breakpoints'};
    $rOpts_keep_interior_semicolons  = $rOpts->{'keep-interior-semicolons'};
    $rOpts_comma_arrow_breakpoints   = $rOpts->{'comma-arrow-breakpoints'};
    $rOpts_maximum_fields_per_table  = $rOpts->{'maximum-fields-per-table'};
    $rOpts_one_line_block_semicolons = $rOpts->{'one-line-block-semicolons'};
    $rOpts_break_at_old_semicolon_breakpoints =
      $rOpts->{'break-at-old-semicolon-breakpoints'};

    $rOpts_tee_side_comments    = $rOpts->{'tee-side-comments'};
    $rOpts_tee_block_comments   = $rOpts->{'tee-block-comments'};
    $rOpts_tee_pod              = $rOpts->{'tee-pod'};
    $rOpts_delete_side_comments = $rOpts->{'delete-side-comments'};
    $rOpts_delete_closing_side_comments =
      $rOpts->{'delete-closing-side-comments'};
    $rOpts_format_skipping       = $rOpts->{'format-skipping'};
    $rOpts_indent_only           = $rOpts->{'indent-only'};
    $rOpts_static_block_comments = $rOpts->{'static-block-comments'};

    # Note that both opening and closing tokens can access the opening
    # and closing flags of their container types.
    %opening_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness'},
        '{' => $rOpts->{'brace-vertical-tightness'},
        '[' => $rOpts->{'square-bracket-vertical-tightness'},
        ')' => $rOpts->{'paren-vertical-tightness'},
        '}' => $rOpts->{'brace-vertical-tightness'},
        ']' => $rOpts->{'square-bracket-vertical-tightness'},
    );

    %closing_vertical_tightness = (
        '(' => $rOpts->{'paren-vertical-tightness-closing'},
        '{' => $rOpts->{'brace-vertical-tightness-closing'},
        '[' => $rOpts->{'square-bracket-vertical-tightness-closing'},
        ')' => $rOpts->{'paren-vertical-tightness-closing'},
        '}' => $rOpts->{'brace-vertical-tightness-closing'},
        ']' => $rOpts->{'square-bracket-vertical-tightness-closing'},
    );

    # assume flag for '>' same as ')' for closing qw quotes
    %closing_token_indentation = (
        ')' => $rOpts->{'closing-paren-indentation'},
        '}' => $rOpts->{'closing-brace-indentation'},
        ']' => $rOpts->{'closing-square-bracket-indentation'},
        '>' => $rOpts->{'closing-paren-indentation'},
    );

    # flag indicating if any closing tokens are indented
    $some_closing_token_indentation =
         $rOpts->{'closing-paren-indentation'}
      || $rOpts->{'closing-brace-indentation'}
      || $rOpts->{'closing-square-bracket-indentation'}
      || $rOpts->{'indent-closing-brace'};

    %opening_token_right = (
        '(' => $rOpts->{'opening-paren-right'},
        '{' => $rOpts->{'opening-hash-brace-right'},
        '[' => $rOpts->{'opening-square-bracket-right'},
    );

    %stack_opening_token = (
        '(' => $rOpts->{'stack-opening-paren'},
        '{' => $rOpts->{'stack-opening-hash-brace'},
        '[' => $rOpts->{'stack-opening-square-bracket'},
    );

    %stack_closing_token = (
        ')' => $rOpts->{'stack-closing-paren'},
        '}' => $rOpts->{'stack-closing-hash-brace'},
        ']' => $rOpts->{'stack-closing-square-bracket'},
    );

    # Create a table of maximum line length vs level for later efficient use.
    # This avoids continually checking the -vmll flag. We will make the
    # table very long to be sure it will not be exceeded.  But we have to
    # choose a fixed length.  A check will be made at the start of sub
    # 'finish_formatting' to be sure it is not exceeded.  Note, some
    # of my standard test problems have indentation levels of about 150,
    # so this should be fairly large.
    my $level_max = 1000;
    foreach my $level ( 0 .. $level_max ) {
        $maximum_line_length[$level] = $rOpts_maximum_line_length;
    }
    if ($rOpts_variable_maximum_line_length) {
        foreach my $level ( 0 .. $level_max ) {
            $maximum_line_length[$level] += $level * $rOpts_indent_columns;
        }
    }

    initialize_weld_nested_exclusion_rules($rOpts);
    return;
}

sub initialize_weld_nested_exclusion_rules {
    my ($rOpts) = @_;
    %weld_nested_exclusion_rules = ();

    my $opt_name = 'weld-nested-exclusion-list';
    my $str      = $rOpts->{$opt_name};
    return unless ($str);
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    return unless ($str);

    # There are four container tokens.
    my %token_keys = (
        '(' => '(',
        '[' => '[',
        '{' => '{',
        'q' => 'q',
    );

    # We are parsing an exclusion list for nested welds. The list is a string
    # with spaces separating any number of items.  Each item consists of three
    # pieces of information:
    # <optional position> <optional type> <type of container>
    # <     ^ or .      > <    k or K   > <     ( [ {       >

    # The last character is the required container type and must be one of:
    # ( = paren
    # [ = square bracket
    # { = brace

    # An optional leading position indicator:
    # ^ means the leading token position in the weld
    # . means a secondary token position in the weld
    #   no position indicator means all positions match

    # An optional alphanumeric character between the position and container
    # token selects to which the rule applies:
    # k = any keyword
    # K = any non-keyword
    # f = function call
    # F = not a function call
    # w = function or keyword
    # W = not a function or keyword
    #     no letter means any preceding type matches

    # Examples:
    # ^(  - the weld must not start with a paren
    # .(  - the second and later tokens may not be parens
    # (   - no parens in weld
    # ^K(  - exclude a leading paren not preceded by a keyword
    # .k(  - exclude a secondary paren preceded by a keyword
    # [ {  - exclude all brackets and braces

    my @items = split /\s+/, $str;
    my $msg1;
    my $msg2;
    foreach my $item (@items) {
        my $item_save = $item;
        my $tok       = chop($item);
        my $key       = $token_keys{$tok};
        if ( !defined($key) ) {
            $msg1 .= " '$item_save'";
            next;
        }
        if ( !defined( $weld_nested_exclusion_rules{$key} ) ) {
            $weld_nested_exclusion_rules{$key} = [];
        }
        my $rflags = $weld_nested_exclusion_rules{$key};

        # A 'q' means do not weld quotes
        if ( $tok eq 'q' ) {
            $rflags->[0] = '*';
            $rflags->[1] = '*';
            next;
        }

        my $pos    = '*';
        my $select = '*';
        if ($item) {
            if ( $item =~ /^([\^\.])?([kKfFwW])?$/ ) {
                $pos    = $1 if ($1);
                $select = $2 if ($2);
            }
            else {
                $msg1 .= " '$item_save'";
                next;
            }
        }
        if ( $pos eq '^' || $pos eq '*' ) {
            if ( defined( $rflags->[0] ) && $rflags ne $select ) {
                $msg1 .= " '$item_save'";
            }
            $rflags->[0] = $select;
        }
        if ( $pos eq '.' || $pos eq '*' ) {
            if ( defined( $rflags->[1] ) && $rflags ne $select ) {
                $msg1 .= " '$item_save'";
            }
            $rflags->[1] = $select;
        }
    }
    if ($msg1) {
        Warn(<<EOM);
Unexpecting symbol(s) encountered in --$opt_name will be ignored:
$msg1
EOM
    }
    if ($msg2) {
        Warn(<<EOM);
Multiple specifications were encountered in the --weld-nested-exclusion-list for:
$msg2
Only the last will be used.
EOM
    }
    return;
}

sub initialize_whitespace_hashes {

    # This is called once before formatting begins to initialize these global
    # hashes, which control the use of whitespace around tokens:
    #
    # %binary_ws_rules
    # %want_left_space
    # %want_right_space
    # %space_after_keyword
    #
    # Many token types are identical to the tokens themselves.
    # See the tokenizer for a complete list. Here are some special types:
    #   k = perl keyword
    #   f = semicolon in for statement
    #   m = unary minus
    #   p = unary plus
    # Note that :: is excluded since it should be contained in an identifier
    # Note that '->' is excluded because it never gets space
    # parentheses and brackets are excluded since they are handled specially
    # curly braces are included but may be overridden by logic, such as
    # newline logic.

    # NEW_TOKENS: create a whitespace rule here.  This can be as
    # simple as adding your new letter to @spaces_both_sides, for
    # example.

    my @opening_type = qw< L { ( [ >;
    @is_opening_type{@opening_type} = (1) x scalar(@opening_type);

    my @closing_type = qw< R } ) ] >;
    @is_closing_type{@closing_type} = (1) x scalar(@closing_type);

    my @spaces_both_sides = qw#
      + - * / % ? = . : x < > | & ^ .. << >> ** && .. || // => += -=
      .= %= x= &= |= ^= *= <> <= >= == =~ !~ /= != ... <<= >>= ~~ !~~
      &&= ||= //= <=> A k f w F n C Y U G v
      #;

    my @spaces_left_side = qw<
      t ! ~ m p { \ h pp mm Z j
    >;
    push( @spaces_left_side, '#' );    # avoids warning message

    my @spaces_right_side = qw<
      ; } ) ] R J ++ -- **=
    >;
    push( @spaces_right_side, ',' );    # avoids warning message

    %want_left_space  = ();
    %want_right_space = ();
    %binary_ws_rules  = ();

    # Note that we setting defaults here.  Later in processing
    # the values of %want_left_space and  %want_right_space
    # may be overridden by any user settings specified by the
    # -wls and -wrs parameters.  However the binary_whitespace_rules
    # are hardwired and have priority.
    @want_left_space{@spaces_both_sides} =
      (1) x scalar(@spaces_both_sides);
    @want_right_space{@spaces_both_sides} =
      (1) x scalar(@spaces_both_sides);
    @want_left_space{@spaces_left_side} =
      (1) x scalar(@spaces_left_side);
    @want_right_space{@spaces_left_side} =
      (-1) x scalar(@spaces_left_side);
    @want_left_space{@spaces_right_side} =
      (-1) x scalar(@spaces_right_side);
    @want_right_space{@spaces_right_side} =
      (1) x scalar(@spaces_right_side);
    $want_left_space{'->'}      = WS_NO;
    $want_right_space{'->'}     = WS_NO;
    $want_left_space{'**'}      = WS_NO;
    $want_right_space{'**'}     = WS_NO;
    $want_right_space{'CORE::'} = WS_NO;

    # These binary_ws_rules are hardwired and have priority over the above
    # settings.  It would be nice to allow adjustment by the user,
    # but it would be complicated to specify.
    #
    # hash type information must stay tightly bound
    # as in :  ${xxxx}
    $binary_ws_rules{'i'}{'L'} = WS_NO;
    $binary_ws_rules{'i'}{'{'} = WS_YES;
    $binary_ws_rules{'k'}{'{'} = WS_YES;
    $binary_ws_rules{'U'}{'{'} = WS_YES;
    $binary_ws_rules{'i'}{'['} = WS_NO;
    $binary_ws_rules{'R'}{'L'} = WS_NO;
    $binary_ws_rules{'R'}{'{'} = WS_NO;
    $binary_ws_rules{'t'}{'L'} = WS_NO;
    $binary_ws_rules{'t'}{'{'} = WS_NO;
    $binary_ws_rules{'}'}{'L'} = WS_NO;
    $binary_ws_rules{'}'}{'{'} = WS_OPTIONAL;    # RT#129850; was WS_NO
    $binary_ws_rules{'$'}{'L'} = WS_NO;
    $binary_ws_rules{'$'}{'{'} = WS_NO;
    $binary_ws_rules{'@'}{'L'} = WS_NO;
    $binary_ws_rules{'@'}{'{'} = WS_NO;
    $binary_ws_rules{'='}{'L'} = WS_YES;
    $binary_ws_rules{'J'}{'J'} = WS_YES;

    # the following includes ') {'
    # as in :    if ( xxx ) { yyy }
    $binary_ws_rules{']'}{'L'} = WS_NO;
    $binary_ws_rules{']'}{'{'} = WS_NO;
    $binary_ws_rules{')'}{'{'} = WS_YES;
    $binary_ws_rules{')'}{'['} = WS_NO;
    $binary_ws_rules{']'}{'['} = WS_NO;
    $binary_ws_rules{']'}{'{'} = WS_NO;
    $binary_ws_rules{'}'}{'['} = WS_NO;
    $binary_ws_rules{'R'}{'['} = WS_NO;

    $binary_ws_rules{']'}{'++'} = WS_NO;
    $binary_ws_rules{']'}{'--'} = WS_NO;
    $binary_ws_rules{')'}{'++'} = WS_NO;
    $binary_ws_rules{')'}{'--'} = WS_NO;

    $binary_ws_rules{'R'}{'++'} = WS_NO;
    $binary_ws_rules{'R'}{'--'} = WS_NO;

    $binary_ws_rules{'i'}{'Q'} = WS_YES;
    $binary_ws_rules{'n'}{'('} = WS_YES;    # occurs in 'use package n ()'

    # FIXME: we could to split 'i' into variables and functions
    # and have no space for functions but space for variables.  For now,
    # I have a special patch in the special rules below
    $binary_ws_rules{'i'}{'('} = WS_NO;

    $binary_ws_rules{'w'}{'('} = WS_NO;
    $binary_ws_rules{'w'}{'{'} = WS_YES;
    return;

} ## end initialize_whitespace_hashes

sub set_whitespace_flags {

    # This routine is called once per file to set whitespace flags for that
    # file.  This routine examines each pair of nonblank tokens and sets a flag
    # indicating if white space is needed.
    #
    # $rwhitespace_flags->[$j] is a flag indicating whether a white space
    # BEFORE token $j is needed, with the following values:
    #
    #             WS_NO      = -1 do not want a space BEFORE token $j
    #             WS_OPTIONAL=  0 optional space or $j is a whitespace
    #             WS_YES     =  1 want a space BEFORE token $j
    #

    my $self = shift;
    my $rLL  = $self->[_rLL_];
    use constant DEBUG_WHITE => 0;

    my $rOpts_space_keyword_paren   = $rOpts->{'space-keyword-paren'};
    my $rOpts_space_backslash_quote = $rOpts->{'space-backslash-quote'};
    my $rOpts_space_function_paren  = $rOpts->{'space-function-paren'};

    my $rwhitespace_flags = [];

    my %is_for_foreach = ( 'for' => 1, 'foreach' => 1 );

    my ( $token, $type, $block_type, $seqno, $input_line_no );
    my (
        $last_token, $last_type, $last_block_type,
        $last_seqno, $last_input_line_no
    );

    my $j_tight_closing_paren = -1;

    $token              = ' ';
    $type               = 'b';
    $block_type         = '';
    $seqno              = '';
    $input_line_no      = 0;
    $last_token         = ' ';
    $last_type          = 'b';
    $last_block_type    = '';
    $last_seqno         = '';
    $last_input_line_no = 0;

    my $jmax = @{$rLL} - 1;

    my ($ws);

    # This is some logic moved to a sub to avoid deep nesting of if stmts
    my $ws_in_container = sub {

        my ($j) = @_;
        my $ws = WS_YES;
        if ( $j + 1 > $jmax ) { return (WS_NO) }

        # Patch to count '-foo' as single token so that
        # each of  $a{-foo} and $a{foo} and $a{'foo'} do
        # not get spaces with default formatting.
        my $j_here = $j;
        ++$j_here
          if ( $token eq '-'
            && $last_token eq '{'
            && $rLL->[ $j + 1 ]->[_TYPE_] eq 'w' );

        # Patch to count a sign separated from a number as a single token, as
        # in the following line. Otherwise, it takes two steps to converge:
        #    deg2rad(-  0.5)
        if (   ( $type eq 'm' || $type eq 'p' )
            && $j < $jmax + 1
            && $rLL->[ $j + 1 ]->[_TYPE_] eq 'b'
            && $rLL->[ $j + 2 ]->[_TYPE_] eq 'n'
            && $rLL->[ $j + 2 ]->[_TOKEN_] =~ /^\d/ )
        {
            $j_here = $j + 2;
        }

        # $j_next is where a closing token should be if
        # the container has a single token
        if ( $j_here + 1 > $jmax ) { return (WS_NO) }
        my $j_next =
          ( $rLL->[ $j_here + 1 ]->[_TYPE_] eq 'b' )
          ? $j_here + 2
          : $j_here + 1;

        if ( $j_next > $jmax ) { return WS_NO }
        my $tok_next  = $rLL->[$j_next]->[_TOKEN_];
        my $type_next = $rLL->[$j_next]->[_TYPE_];

        # for tightness = 1, if there is just one token
        # within the matching pair, we will keep it tight
        if (
            $tok_next eq $matching_token{$last_token}

            # but watch out for this: [ [ ]    (misc.t)
            && $last_token ne $token

            # double diamond is usually spaced
            && $token ne '<<>>'

          )
        {

            # remember where to put the space for the closing paren
            $j_tight_closing_paren = $j_next;
            return (WS_NO);
        }
        return (WS_YES);
    };

    # Local hashes to set spaces around container tokens according to their
    # sequence numbers.  These are set as keywords are examined.
    # They are controlled by the -kpit and -kpitl flags.
    my %opening_container_inside_ws;
    my %closing_container_inside_ws;
    my $set_container_ws_by_keyword = sub {

        return unless (%keyword_paren_inner_tightness);

        my ( $word, $sequence_number ) = @_;

        # We just saw a keyword (or other function name) followed by an opening
        # paren. Now check to see if the following paren should have special
        # treatment for its inside space.  If so we set a hash value using the
        # sequence number as key.
        if ( $word && $sequence_number ) {
            my $tightness = $keyword_paren_inner_tightness{$word};
            if ( defined($tightness) && $tightness != 1 ) {
                my $ws_flag = $tightness == 0 ? WS_YES : WS_NO;
                $opening_container_inside_ws{$sequence_number} = $ws_flag;
                $closing_container_inside_ws{$sequence_number} = $ws_flag;
            }
        }
    };

    my $ws_opening_container_override = sub {
        my ( $ws, $sequence_number ) = @_;
        return $ws unless (%opening_container_inside_ws);
        if ($sequence_number) {
            my $ws_override = $opening_container_inside_ws{$sequence_number};
            if ($ws_override) { $ws = $ws_override }
        }
        return $ws;
    };

    my $ws_closing_container_override = sub {
        my ( $ws, $sequence_number ) = @_;
        return $ws unless (%closing_container_inside_ws);
        if ($sequence_number) {
            my $ws_override = $closing_container_inside_ws{$sequence_number};
            if ($ws_override) { $ws = $ws_override }
        }
        return $ws;
    };

    # main loop over all tokens to define the whitespace flags
    for ( my $j = 0 ; $j <= $jmax ; $j++ ) {

        my $rtokh = $rLL->[$j];

        # Set a default
        $rwhitespace_flags->[$j] = WS_OPTIONAL;

        if ( $rtokh->[_TYPE_] eq 'b' ) {
            next;
        }

        # set a default value, to be changed as needed
        $ws                 = undef;
        $last_token         = $token;
        $last_type          = $type;
        $last_block_type    = $block_type;
        $last_seqno         = $seqno;
        $last_input_line_no = $input_line_no;
        $token              = $rtokh->[_TOKEN_];
        $type               = $rtokh->[_TYPE_];
        $block_type         = $rtokh->[_BLOCK_TYPE_];
        $seqno              = $rtokh->[_TYPE_SEQUENCE_];
        $input_line_no      = $rtokh->[_LINE_INDEX_];

        #---------------------------------------------------------------
        # Whitespace Rules Section 1:
        # Handle space on the inside of opening braces.
        #---------------------------------------------------------------

        #    /^[L\{\(\[]$/
        if ( $is_opening_type{$last_type} ) {

            $j_tight_closing_paren = -1;

            # let us keep empty matched braces together: () {} []
            # except for BLOCKS
            if ( $token eq $matching_token{$last_token} ) {
                if ($block_type) {
                    $ws = WS_YES;
                }
                else {
                    $ws = WS_NO;
                }
            }
            else {

                # we're considering the right of an opening brace
                # tightness = 0 means always pad inside with space
                # tightness = 1 means pad inside if "complex"
                # tightness = 2 means never pad inside with space

                my $tightness;
                if (   $last_type eq '{'
                    && $last_token eq '{'
                    && $last_block_type )
                {
                    $tightness = $rOpts_block_brace_tightness;
                }
                else { $tightness = $tightness{$last_token} }

               #=============================================================
               # Patch for test problem <<snippets/fabrice_bug.in>>
               # We must always avoid spaces around a bare word beginning
               # with ^ as in:
               #    my $before = ${^PREMATCH};
               # Because all of the following cause an error in perl:
               #    my $before = ${ ^PREMATCH };
               #    my $before = ${ ^PREMATCH};
               #    my $before = ${^PREMATCH };
               # So if brace tightness flag is -bt=0 we must temporarily reset
               # to bt=1.  Note that here we must set tightness=1 and not 2 so
               # that the closing space
               # is also avoided (via the $j_tight_closing_paren flag in coding)
                if ( $type eq 'w' && $token =~ /^\^/ ) { $tightness = 1 }

                #=============================================================

                if ( $tightness <= 0 ) {
                    $ws = WS_YES;
                }
                elsif ( $tightness > 1 ) {
                    $ws = WS_NO;
                }
                else {
                    $ws = $ws_in_container->($j);
                }
            }

            # check for special cases which override the above rules
            $ws = $ws_opening_container_override->( $ws, $last_seqno );

        }    # end setting space flag inside opening tokens
        my $ws_1;
        $ws_1 = $ws
          if DEBUG_WHITE;

        #---------------------------------------------------------------
        # Whitespace Rules Section 2:
        # Handle space on inside of closing brace pairs.
        #---------------------------------------------------------------

        #   /[\}\)\]R]/
        if ( $is_closing_type{$type} ) {

            if ( $j == $j_tight_closing_paren ) {

                $j_tight_closing_paren = -1;
                $ws                    = WS_NO;
            }
            else {

                if ( !defined($ws) ) {

                    my $tightness;
                    if ( $type eq '}' && $token eq '}' && $block_type ) {
                        $tightness = $rOpts_block_brace_tightness;
                    }
                    else { $tightness = $tightness{$token} }

                    $ws = ( $tightness > 1 ) ? WS_NO : WS_YES;
                }
            }

            # check for special cases which override the above rules
            $ws = $ws_closing_container_override->( $ws, $seqno );

        }    # end setting space flag inside closing tokens

        my $ws_2;
        $ws_2 = $ws
          if DEBUG_WHITE;

        #---------------------------------------------------------------
        # Whitespace Rules Section 3:
        # Use the binary rule table.
        #---------------------------------------------------------------
        if ( !defined($ws) ) {
            $ws = $binary_ws_rules{$last_type}{$type};
        }
        my $ws_3;
        $ws_3 = $ws
          if DEBUG_WHITE;

        #---------------------------------------------------------------
        # Whitespace Rules Section 4:
        # Handle some special cases.
        #---------------------------------------------------------------
        if ( $token eq '(' ) {

            # This will have to be tweaked as tokenization changes.
            # We usually want a space at '} (', for example:
            # <<snippets/space1.in>>
            #     map { 1 * $_; } ( $y, $M, $w, $d, $h, $m, $s );
            #
            # But not others:
            #     &{ $_->[1] }( delete $_[$#_]{ $_->[0] } );
            # At present, the above & block is marked as type L/R so this case
            # won't go through here.
            if ( $last_type eq '}' ) { $ws = WS_YES }

            # NOTE: some older versions of Perl had occasional problems if
            # spaces are introduced between keywords or functions and opening
            # parens.  So the default is not to do this except is certain
            # cases.  The current Perl seems to tolerate spaces.

            # Space between keyword and '('
            elsif ( $last_type eq 'k' ) {
                $ws = WS_NO
                  unless ( $rOpts_space_keyword_paren
                    || $space_after_keyword{$last_token} );

                # Set inside space flag if requested
                $set_container_ws_by_keyword->( $last_token, $seqno );
            }

            # Space between function and '('
            # -----------------------------------------------------
            # 'w' and 'i' checks for something like:
            #   myfun(    &myfun(   ->myfun(
            # -----------------------------------------------------
            elsif (( $last_type =~ /^[wUG]$/ )
                || ( $last_type =~ /^[wi]$/ && $last_token =~ /^(\&|->)/ ) )
            {
                $ws = WS_NO unless ($rOpts_space_function_paren);
                $set_container_ws_by_keyword->( $last_token, $seqno );
            }

            # space between something like $i and ( in <<snippets/space2.in>>
            # for $i ( 0 .. 20 ) {
            # FIXME: eventually, type 'i' needs to be split into multiple
            # token types so this can be a hardwired rule.
            elsif ( $last_type eq 'i' && $last_token =~ /^[\$\%\@]/ ) {
                $ws = WS_YES;
            }

            # allow constant function followed by '()' to retain no space
            elsif ($last_type eq 'C'
                && $rLL->[ $j + 1 ]->[_TOKEN_] eq ')' )
            {
                $ws = WS_NO;
            }
        }

        # patch for SWITCH/CASE: make space at ']{' optional
        # since the '{' might begin a case or when block
        elsif ( ( $token eq '{' && $type ne 'L' ) && $last_token eq ']' ) {
            $ws = WS_OPTIONAL;
        }

        # keep space between 'sub' and '{' for anonymous sub definition
        if ( $type eq '{' ) {
            if ( $last_token eq 'sub' ) {
                $ws = WS_YES;
            }

            # this is needed to avoid no space in '){'
            if ( $last_token eq ')' && $token eq '{' ) { $ws = WS_YES }

            # avoid any space before the brace or bracket in something like
            #  @opts{'a','b',...}
            if ( $last_type eq 'i' && $last_token =~ /^\@/ ) {
                $ws = WS_NO;
            }
        }

        elsif ( $type eq 'i' ) {

            # never a space before ->
            if ( substr( $token, 0, 2 ) eq '->' ) {
                $ws = WS_NO;
            }
        }

        # retain any space between '-' and bare word
        elsif ( $type eq 'w' || $type eq 'C' ) {
            $ws = WS_OPTIONAL if $last_type eq '-';

            # never a space before ->
            if ( substr( $token, 0, 2 ) eq '->' ) {
                $ws = WS_NO;
            }
        }

        # retain any space between '-' and bare word; for example
        # avoid space between 'USER' and '-' here: <<snippets/space2.in>>
        #   $myhash{USER-NAME}='steve';
        elsif ( $type eq 'm' || $type eq '-' ) {
            $ws = WS_OPTIONAL if ( $last_type eq 'w' );
        }

        # always space before side comment
        elsif ( $type eq '#' ) { $ws = WS_YES if $j > 0 }

        # always preserver whatever space was used after a possible
        # filehandle (except _) or here doc operator
        if (
            $type ne '#'
            && ( ( $last_type eq 'Z' && $last_token ne '_' )
                || $last_type eq 'h' )
          )
        {
            $ws = WS_OPTIONAL;
        }

        # space_backslash_quote; RT #123774  <<snippets/rt123774.in>>
        # allow a space between a backslash and single or double quote
        # to avoid fooling html formatters
        elsif ( $last_type eq '\\' && $type eq 'Q' && $token =~ /^[\"\']/ ) {
            if ($rOpts_space_backslash_quote) {
                if ( $rOpts_space_backslash_quote == 1 ) {
                    $ws = WS_OPTIONAL;
                }
                elsif ( $rOpts_space_backslash_quote == 2 ) { $ws = WS_YES }
                else { }    # shouldnt happen
            }
            else {
                $ws = WS_NO;
            }
        }
        elsif ( $type eq 'k' ) {

            # Keywords 'for', 'foreach' are special cases for -kpit since the
            # opening paren does not always immediately follow the keyword. So
            # we have to search forward for the paren in this case.  I have
            # limited the search to 10 tokens ahead, just in case somebody
            # has a big file and no opening paren.  This should be enough for
            # all normal code.
            if (   $is_for_foreach{$token}
                && %keyword_paren_inner_tightness
                && defined( $keyword_paren_inner_tightness{$token} )
                && $j < $jmax )
            {
                my $jp = $j;
                for ( my $inc = 1 ; $inc < 10 ; $inc++ ) {
                    $jp++;
                    last if ( $jp > $jmax );
                    next unless ( $rLL->[$jp]->[_TOKEN_] eq '(' );
                    my $seqno = $rLL->[$jp]->[_TYPE_SEQUENCE_];
                    $set_container_ws_by_keyword->( $token, $seqno );
                    last;
                }
            }
        }

        my $ws_4;
        $ws_4 = $ws
          if DEBUG_WHITE;

        #---------------------------------------------------------------
        # Whitespace Rules Section 5:
        # Apply default rules not covered above.
        #---------------------------------------------------------------

        # If we fall through to here, look at the pre-defined hash tables for
        # the two tokens, and:
        #  if (they are equal) use the common value
        #  if (either is zero or undef) use the other
        #  if (either is -1) use it
        # That is,
        # left  vs right
        #  1    vs    1     -->  1
        #  0    vs    0     -->  0
        # -1    vs   -1     --> -1
        #
        #  0    vs   -1     --> -1
        #  0    vs    1     -->  1
        #  1    vs    0     -->  1
        # -1    vs    0     --> -1
        #
        # -1    vs    1     --> -1
        #  1    vs   -1     --> -1
        if ( !defined($ws) ) {
            my $wl = $want_left_space{$type};
            my $wr = $want_right_space{$last_type};
            if ( !defined($wl) ) { $wl = 0 }
            if ( !defined($wr) ) { $wr = 0 }
            $ws = ( ( $wl == $wr ) || ( $wl == -1 ) || !$wr ) ? $wl : $wr;
        }

        if ( !defined($ws) ) {
            $ws = 0;
            write_diagnostics(
                "WS flag is undefined for tokens $last_token $token\n");
        }

        # Treat newline as a whitespace. Otherwise, we might combine
        # 'Send' and '-recipients' here according to the above rules:
        # <<snippets/space3.in>>
        #    my $msg = new Fax::Send
        #      -recipients => $to,
        #      -data => $data;
        if ( $ws == 0 && $input_line_no != $last_input_line_no ) { $ws = 1 }

        $rwhitespace_flags->[$j] = $ws;

        DEBUG_WHITE && do {
            my $str = substr( $last_token, 0, 15 );
            $str .= ' ' x ( 16 - length($str) );
            if ( !defined($ws_1) ) { $ws_1 = "*" }
            if ( !defined($ws_2) ) { $ws_2 = "*" }
            if ( !defined($ws_3) ) { $ws_3 = "*" }
            if ( !defined($ws_4) ) { $ws_4 = "*" }
            print STDOUT
"NEW WHITE:  i=$j $str $last_type $type $ws_1 : $ws_2 : $ws_3 : $ws_4 : $ws \n";
        };
    } ## end main loop

    if ( $rOpts->{'tight-secret-operators'} ) {
        new_secret_operator_whitespace( $rLL, $rwhitespace_flags );
    }
    return $rwhitespace_flags;

} ## end sub set_whitespace_flags

sub dump_want_left_space {
    my $fh = shift;
    local $" = "\n";
    $fh->print(<<EOM);
These values are the main control of whitespace to the left of a token type;
They may be altered with the -wls parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its left
-1 means the token does not want a space to its left
------------------------------------------------------------------------
EOM
    foreach my $key ( sort keys %want_left_space ) {
        $fh->print("$key\t$want_left_space{$key}\n");
    }
    return;
}

sub dump_want_right_space {
    my $fh = shift;
    local $" = "\n";
    $fh->print(<<EOM);
These values are the main control of whitespace to the right of a token type;
They may be altered with the -wrs parameter.
For a list of token types, use perltidy --dump-token-types (-dtt)
 1 means the token wants a space to its right
-1 means the token does not want a space to its right
------------------------------------------------------------------------
EOM
    foreach my $key ( sort keys %want_right_space ) {
        $fh->print("$key\t$want_right_space{$key}\n");
    }
    return;
}

{    ## begin closure is_essential_whitespace

    my %is_sort_grep_map;
    my %is_for_foreach;
    my %is_digraph;
    my %is_trigraph;
    my %essential_whitespace_filter_l1;
    my %essential_whitespace_filter_r1;
    my %essential_whitespace_filter_l2;
    my %essential_whitespace_filter_r2;

    BEGIN {

        my @q;
        @q = qw(sort grep map);
        @is_sort_grep_map{@q} = (1) x scalar(@q);

        @q = qw(for foreach);
        @is_for_foreach{@q} = (1) x scalar(@q);

        @q = qw(
          .. :: << >> ** && .. || // -> => += -= .= %= &= |= ^= *= <>
          <= >= == =~ !~ != ++ -- /= x= ~~ ~. |. &. ^.
        );
        @is_digraph{@q} = (1) x scalar(@q);

        @q = qw( ... **= <<= >>= &&= ||= //= <=> !~~ &.= |.= ^.= <<~);
        @is_trigraph{@q} = (1) x scalar(@q);

        # These are used as a speedup filters for sub is_essential_whitespace.

        # Filter 1:
        # These left side token types USUALLY do not require a space:
        @q = qw( ; { } [ ] L R );
        push @q, ',';
        push @q, ')';
        push @q, '(';
        @essential_whitespace_filter_l1{@q} = (1) x scalar(@q);

        # BUT some might if followed by these right token types
        @q = qw( pp mm << <<= h );
        @essential_whitespace_filter_r1{@q} = (1) x scalar(@q);

        # Filter 2:
        # These right side filters usually do not require a space
        @q = qw( ; ] R } );
        push @q, ',';
        push @q, ')';
        @essential_whitespace_filter_r2{@q} = (1) x scalar(@q);

        # BUT some might if followed by these left token types
        @q = qw( h Z );
        @essential_whitespace_filter_l2{@q} = (1) x scalar(@q);

    }

    sub is_essential_whitespace {

        # Essential whitespace means whitespace which cannot be safely deleted
        # without risking the introduction of a syntax error.
        # We are given three tokens and their types:
        # ($tokenl, $typel) is the token to the left of the space in question
        # ($tokenr, $typer) is the token to the right of the space in question
        # ($tokenll, $typell) is previous nonblank token to the left of $tokenl
        #
        # Note1: This routine should almost never need to be changed.  It is
        # for avoiding syntax problems rather than for formatting.

        # Note2: The -mangle option causes large numbers of calls to this
        # routine and therefore is a good test. So if a change is made, be sure
        # to run a large number of files with the -mangle option and check for
        # differences.

        my ( $tokenll, $typell, $tokenl, $typel, $tokenr, $typer ) = @_;

        # This is potentially a very slow routine but the following quick
        # filters typically catch and handle over 90% of the calls.

        # Filter 1: usually no space required after common types ; , [ ] { } ( )
        return
          if ( $essential_whitespace_filter_l1{$typel}
            && !$essential_whitespace_filter_r1{$typer} );

        # Filter 2: usually no space before common types ; ,
        return
          if ( $essential_whitespace_filter_r2{$typer}
            && !$essential_whitespace_filter_l2{$typel} );

        # Filter 3: Handle side comments: a space is only essential if the left
        # token ends in '$' For example, we do not want to create $#foo below:

        #   sub t086
        #       ( #foo)))
        #       $ #foo)))
        #       a #foo)))
        #       ) #foo)))
        #       { ... }

        # Also, I prefer not to put a ? and # together because ? used to be
        # a pattern delmiter and spacing was used if guessing was needed.

        if ( $typer eq '#' ) {

            return 1
              if ( $tokenl
                && ( $typel eq '?' || substr( $tokenl, -1 ) eq '$' ) );
            return;
        }

        my $tokenr_is_bareword   = $tokenr =~ /^\w/ && $tokenr !~ /^\d/;
        my $tokenr_is_open_paren = $tokenr eq '(';
        my $token_joined         = $tokenl . $tokenr;
        my $tokenl_is_dash       = $tokenl eq '-';

        my $result =

          # never combine two bare words or numbers
          # examples:  and ::ok(1)
          #            return ::spw(...)
          #            for bla::bla:: abc
          # example is "%overload:: and" in files Dumpvalue.pm or colonbug.pl
          #            $input eq"quit" to make $inputeq"quit"
          #            my $size=-s::SINK if $file;  <==OK but we won't do it
          # don't join something like: for bla::bla:: abc
          # example is "%overload:: and" in files Dumpvalue.pm or colonbug.pl
          (      ( $tokenl =~ /([\'\w]|\:\:)$/ && $typel ne 'CORE::' )
              && ( $tokenr =~ /^([\'\w]|\:\:)/ ) )

          # do not combine a number with a concatenation dot
          # example: pom.caputo:
          # $vt100_compatible ? "\e[0;0H" : ('-' x 78 . "\n");
          || $typel eq 'n' && $tokenr eq '.'
          || $typer eq 'n'
          && $tokenl eq '.'

          # cases of a space before a bareword...
          || (
            $tokenr_is_bareword && (

                # do not join a minus with a bare word, because you might form
                # a file test operator.  Example from Complex.pm:
                # if (CORE::abs($z - i) < $eps);
                # "z-i" would be taken as a file test.
                $tokenl_is_dash && length($tokenr) == 1

                # and something like this could become ambiguous without space
                # after the '-':
                #   use constant III=>1;
                #   $a = $b - III;
                # and even this:
                #   $a = - III;
                || $tokenl_is_dash && $typer =~ /^[wC]$/

                # keep a space between a quote and a bareword to prevent the
                # bareword from becoming a quote modifier.
                || $typel eq 'Q'

                # keep a space between a token ending in '$' and any word;
                # this caused trouble:  "die @$ if $@"
                || $typel eq 'i' && $tokenl =~ /\$$/

               # do not remove space between an '&' and a bare word because
               # it may turn into a function evaluation, like here
               # between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
               #    $opts{rdonly} = (($opts{mode} & O_ACCMODE) == O_RDONLY);
                || $typel eq '&'

                # don't combine $$ or $# with any alphanumeric
                # (testfile mangle.t with --mangle)
                || $tokenl =~ /^\$[\$\#]$/

            )
          )    ## end $tokenr_is_bareword

          # OLD, not used
          # '= -' should not become =- or you will get a warning
          # about reversed -=
          # || ($tokenr eq '-')

          # do not join a bare word with a minus, like between 'Send' and
          # '-recipients' here <<snippets/space3.in>>
          #   my $msg = new Fax::Send
          #     -recipients => $to,
          #     -data => $data;
          # This is the safest thing to do. If we had the token to the right of
          # the minus we could do a better check.
          #
          # And do not combine a bareword and a quote, like this:
          #    oops "Your login, $Bad_Login, is not valid";
          # It can cause a syntax error if oops is a sub
          || $typel eq 'w' && ( $tokenr eq '-' || $typer eq 'Q' )

          # perl is very fussy about spaces before <<
          || $tokenr =~ /^\<\</

          # avoid combining tokens to create new meanings. Example:
          #     $a+ +$b must not become $a++$b
          || ( $is_digraph{$token_joined} )
          || $is_trigraph{$token_joined}

          # another example: do not combine these two &'s:
          #     allow_options & &OPT_EXECCGI
          || $is_digraph{ $tokenl . substr( $tokenr, 0, 1 ) }

          # retain any space after possible filehandle
          # (testfiles prnterr1.t with --extrude and mangle.t with --mangle)
          || $typel eq 'Z'

          # Perl is sensitive to whitespace after the + here:
          #  $b = xvals $a + 0.1 * yvals $a;
          || $typell eq 'Z' && $typel =~ /^[\/\?\+\-\*]$/

          || (
            $tokenr_is_open_paren && (

                # keep paren separate in 'use Foo::Bar ()'
                ( $typel eq 'w' && $typell eq 'k' && $tokenll eq 'use' )

                # keep any space between filehandle and paren:
                # file mangle.t with --mangle:
                || $typel eq 'Y'

                # must have space between grep and left paren; "grep(" will fail
                || $is_sort_grep_map{$tokenl}

                # don't stick numbers next to left parens, as in:
                #use Mail::Internet 1.28 (); (see Entity.pm, Head.pm, Test.pm)
                || $typel eq 'n'
            )
          )    ## end $tokenr_is_open_paren

          # retain any space after here doc operator ( hereerr.t)
          || $typel eq 'h'

          # be careful with a space around ++ and --, to avoid ambiguity as to
          # which token it applies
          || $typer  =~ /^(pp|mm)$/ && $tokenl !~ /^[\;\{\(\[]/
          || $typel  =~ /^(\+\+|\-\-)$/
          && $tokenr !~ /^[\;\}\)\]]/

          # need space after foreach my; for example, this will fail in
          # older versions of Perl:
          # foreach my$ft(@filetypes)...
          || (
            $tokenl eq 'my'

            #  /^(for|foreach)$/
            && $is_for_foreach{$tokenll}
            && $tokenr =~ /^\$/
          )

          # We must be sure that a space between a ? and a quoted string
          # remains if the space before the ? remains.  [Loca.pm, lockarea]
          # ie,
          #    $b=join $comma ? ',' : ':', @_;  # ok
          #    $b=join $comma?',' : ':', @_;    # ok!
          #    $b=join $comma ?',' : ':', @_;   # error!
          # Not really required:
          ## || ( ( $typel eq '?' ) && ( $typer eq 'Q' ) )

          # space stacked labels  (TODO: check if really necessary)
          || $typel eq 'J' && $typer eq 'J'

          ;    # the value of this long logic sequence is the result we want
        return $result;
    }
} ## end closure is_essential_whitespace

{    ## begin closure new_secret_operator_whitespace

    my %secret_operators;
    my %is_leading_secret_token;

    BEGIN {

        # token lists for perl secret operators as compiled by Philippe Bruhat
        # at: https://metacpan.org/module/perlsecret
        %secret_operators = (
            'Goatse'             => [qw#= ( ) =#],        #=( )=
            'Venus1'             => [qw#0 +#],            # 0+
            'Venus2'             => [qw#+ 0#],            # +0
            'Enterprise'         => [qw#) x ! !#],        # ()x!!
            'Kite1'              => [qw#~ ~ <>#],         # ~~<>
            'Kite2'              => [qw#~~ <>#],          # ~~<>
            'Winking Fat Comma'  => [ ( ',', '=>' ) ],    # ,=>
            'Bang bang         ' => [qw#! !#],            # !!
        );

        # The following operators and constants are not included because they
        # are normally kept tight by perltidy:
        # ~~ <~>
        #

        # Make a lookup table indexed by the first token of each operator:
        # first token => [list, list, ...]
        foreach my $value ( values(%secret_operators) ) {
            my $tok = $value->[0];
            push @{ $is_leading_secret_token{$tok} }, $value;
        }
    }

    sub new_secret_operator_whitespace {

        my ( $rlong_array, $rwhitespace_flags ) = @_;

        # Loop over all tokens in this line
        my ( $token, $type );
        my $jmax = @{$rlong_array} - 1;
        foreach my $j ( 0 .. $jmax ) {

            $token = $rlong_array->[$j]->[_TOKEN_];
            $type  = $rlong_array->[$j]->[_TYPE_];

            # Skip unless this token might start a secret operator
            next if ( $type eq 'b' );
            next unless ( $is_leading_secret_token{$token} );

            #      Loop over all secret operators with this leading token
            foreach my $rpattern ( @{ $is_leading_secret_token{$token} } ) {
                my $jend = $j - 1;
                foreach my $tok ( @{$rpattern} ) {
                    $jend++;
                    $jend++

                      if ( $jend <= $jmax
                        && $rlong_array->[$jend]->[_TYPE_] eq 'b' );
                    if (   $jend > $jmax
                        || $tok ne $rlong_array->[$jend]->[_TOKEN_] )
                    {
                        $jend = undef;
                        last;
                    }
                }

                if ($jend) {

                    # set flags to prevent spaces within this operator
                    foreach my $jj ( $j + 1 .. $jend ) {
                        $rwhitespace_flags->[$jj] = WS_NO;
                    }
                    $j = $jend;
                    last;
                }
            }    ##      End Loop over all operators
        }    ## End loop over all tokens
        return;
    }    # End sub
} ## end closure new_secret_operator_whitespace

{    ## begin closure set_bond_strengths

    # These routines and variables are involved in deciding where to break very
    # long lines.

    my %is_good_keyword_breakpoint;
    my %is_lt_gt_le_ge;
    my %is_container_token;

    my %binary_bond_strength_nospace;
    my %binary_bond_strength;
    my %nobreak_lhs;
    my %nobreak_rhs;

    my @bias_tokens;
    my %bias_hash;
    my %bias;
    my $delta_bias;

    sub initialize_bond_strength_hashes {

        my @q;
        @q = qw(if unless while until for foreach);
        @is_good_keyword_breakpoint{@q} = (1) x scalar(@q);

        @q = qw(lt gt le ge);
        @is_lt_gt_le_ge{@q} = (1) x scalar(@q);

        @q = qw/ ( [ { } ] ) /;
        @is_container_token{@q} = (1) x scalar(@q);

        # The decision about where to break a line depends upon a "bond
        # strength" between tokens.  The LOWER the bond strength, the MORE
        # likely a break.  A bond strength may be any value but to simplify
        # things there are several pre-defined strength levels:

        #    NO_BREAK    => 10000;
        #    VERY_STRONG => 100;
        #    STRONG      => 2.1;
        #    NOMINAL     => 1.1;
        #    WEAK        => 0.8;
        #    VERY_WEAK   => 0.55;

        # The strength values are based on trial-and-error, and need to be
        # tweaked occasionally to get desired results.  Some comments:
        #
        #   1. Only relative strengths are important.  small differences
        #      in strengths can make big formatting differences.
        #   2. Each indentation level adds one unit of bond strength.
        #   3. A value of NO_BREAK makes an unbreakable bond
        #   4. A value of VERY_WEAK is the strength of a ','
        #   5. Values below NOMINAL are considered ok break points.
        #   6. Values above NOMINAL are considered poor break points.
        #
        # The bond strengths should roughly follow precedence order where
        # possible.  If you make changes, please check the results very
        # carefully on a variety of scripts.  Testing with the -extrude
        # options is particularly helpful in exercising all of the rules.

        # Wherever possible, bond strengths are defined in the following
        # tables.  There are two main stages to setting bond strengths and
        # two types of tables:
        #
        # The first stage involves looking at each token individually and
        # defining left and right bond strengths, according to if we want
        # to break to the left or right side, and how good a break point it
        # is.  For example tokens like =, ||, && make good break points and
        # will have low strengths, but one might want to break on either
        # side to put them at the end of one line or beginning of the next.
        #
        # The second stage involves looking at certain pairs of tokens and
        # defining a bond strength for that particular pair.  This second
        # stage has priority.

        #---------------------------------------------------------------
        # Bond Strength BEGIN Section 1.
        # Set left and right bond strengths of individual tokens.
        #---------------------------------------------------------------

        # NOTE: NO_BREAK's set in this section first are HINTS which will
        # probably not be honored. Essential NO_BREAKS's should be set in
        # BEGIN Section 2 or hardwired in the NO_BREAK coding near the end
        # of this subroutine.

        # Note that we are setting defaults in this section.  The user
        # cannot change bond strengths but can cause the left and right
        # bond strengths of any token type to be swapped through the use of
        # the -wba and -wbb flags. In this way the user can determine if a
        # breakpoint token should appear at the end of one line or the
        # beginning of the next line.

        %right_bond_strength          = ();
        %left_bond_strength           = ();
        %binary_bond_strength_nospace = ();
        %binary_bond_strength         = ();
        %nobreak_lhs                  = ();
        %nobreak_rhs                  = ();

        # The hash keys in this section are token types, plus the text of
        # certain keywords like 'or', 'and'.

        # no break around possible filehandle
        $left_bond_strength{'Z'}  = NO_BREAK;
        $right_bond_strength{'Z'} = NO_BREAK;

        # never put a bare word on a new line:
        # example print (STDERR, "bla"); will fail with break after (
        $left_bond_strength{'w'} = NO_BREAK;

        # blanks always have infinite strength to force breaks after
        # real tokens
        $right_bond_strength{'b'} = NO_BREAK;

        # try not to break on exponentation
        @q                       = qw# ** .. ... <=> #;
        @left_bond_strength{@q}  = (STRONG) x scalar(@q);
        @right_bond_strength{@q} = (STRONG) x scalar(@q);

        # The comma-arrow has very low precedence but not a good break point
        $left_bond_strength{'=>'}  = NO_BREAK;
        $right_bond_strength{'=>'} = NOMINAL;

        # ok to break after label
        $left_bond_strength{'J'}  = NO_BREAK;
        $right_bond_strength{'J'} = NOMINAL;
        $left_bond_strength{'j'}  = STRONG;
        $right_bond_strength{'j'} = STRONG;
        $left_bond_strength{'A'}  = STRONG;
        $right_bond_strength{'A'} = STRONG;

        $left_bond_strength{'->'}  = STRONG;
        $right_bond_strength{'->'} = VERY_STRONG;

        $left_bond_strength{'CORE::'}  = NOMINAL;
        $right_bond_strength{'CORE::'} = NO_BREAK;

        # breaking AFTER modulus operator is ok:
        @q = qw< % >;
        @left_bond_strength{@q} = (STRONG) x scalar(@q);
        @right_bond_strength{@q} =
          ( 0.1 * NOMINAL + 0.9 * STRONG ) x scalar(@q);

        # Break AFTER math operators * and /
        @q                       = qw< * / x  >;
        @left_bond_strength{@q}  = (STRONG) x scalar(@q);
        @right_bond_strength{@q} = (NOMINAL) x scalar(@q);

        # Break AFTER weakest math operators + and -
        # Make them weaker than * but a bit stronger than '.'
        @q = qw< + - >;
        @left_bond_strength{@q} = (STRONG) x scalar(@q);
        @right_bond_strength{@q} =
          ( 0.91 * NOMINAL + 0.09 * WEAK ) x scalar(@q);

        # breaking BEFORE these is just ok:
        @q                       = qw# >> << #;
        @right_bond_strength{@q} = (STRONG) x scalar(@q);
        @left_bond_strength{@q}  = (NOMINAL) x scalar(@q);

        # breaking before the string concatenation operator seems best
        # because it can be hard to see at the end of a line
        $right_bond_strength{'.'} = STRONG;
        $left_bond_strength{'.'}  = 0.9 * NOMINAL + 0.1 * WEAK;

        @q                       = qw< } ] ) R >;
        @left_bond_strength{@q}  = (STRONG) x scalar(@q);
        @right_bond_strength{@q} = (NOMINAL) x scalar(@q);

        # make these a little weaker than nominal so that they get
        # favored for end-of-line characters
        @q = qw< != == =~ !~ ~~ !~~ >;
        @left_bond_strength{@q} = (STRONG) x scalar(@q);
        @right_bond_strength{@q} =
          ( 0.9 * NOMINAL + 0.1 * WEAK ) x scalar(@q);

        # break AFTER these
        @q = qw# < >  | & >= <= #;
        @left_bond_strength{@q} = (VERY_STRONG) x scalar(@q);
        @right_bond_strength{@q} =
          ( 0.8 * NOMINAL + 0.2 * WEAK ) x scalar(@q);

        # breaking either before or after a quote is ok
        # but bias for breaking before a quote
        $left_bond_strength{'Q'}  = NOMINAL;
        $right_bond_strength{'Q'} = NOMINAL + 0.02;
        $left_bond_strength{'q'}  = NOMINAL;
        $right_bond_strength{'q'} = NOMINAL;

        # starting a line with a keyword is usually ok
        $left_bond_strength{'k'} = NOMINAL;

        # we usually want to bond a keyword strongly to what immediately
        # follows, rather than leaving it stranded at the end of a line
        $right_bond_strength{'k'} = STRONG;

        $left_bond_strength{'G'}  = NOMINAL;
        $right_bond_strength{'G'} = STRONG;

        # assignment operators
        @q = qw(
          = **= += *= &= <<= &&=
          -= /= |= >>= ||= //=
          .= %= ^=
          x=
        );

        # Default is to break AFTER various assignment operators
        @left_bond_strength{@q} = (STRONG) x scalar(@q);
        @right_bond_strength{@q} =
          ( 0.4 * WEAK + 0.6 * VERY_WEAK ) x scalar(@q);

        # Default is to break BEFORE '&&' and '||' and '//'
        # set strength of '||' to same as '=' so that chains like
        # $a = $b || $c || $d   will break before the first '||'
        $right_bond_strength{'||'} = NOMINAL;
        $left_bond_strength{'||'}  = $right_bond_strength{'='};

        # same thing for '//'
        $right_bond_strength{'//'} = NOMINAL;
        $left_bond_strength{'//'}  = $right_bond_strength{'='};

        # set strength of && a little higher than ||
        $right_bond_strength{'&&'} = NOMINAL;
        $left_bond_strength{'&&'}  = $left_bond_strength{'||'} + 0.1;

        $left_bond_strength{';'}  = VERY_STRONG;
        $right_bond_strength{';'} = VERY_WEAK;
        $left_bond_strength{'f'}  = VERY_STRONG;

        # make right strength of for ';' a little less than '='
        # to make for contents break after the ';' to avoid this:
        #   for ( $j = $number_of_fields - 1 ; $j < $item_count ; $j +=
        #     $number_of_fields )
        # and make it weaker than ',' and 'and' too
        $right_bond_strength{'f'} = VERY_WEAK - 0.03;

        # The strengths of ?/: should be somewhere between
        # an '=' and a quote (NOMINAL),
        # make strength of ':' slightly less than '?' to help
        # break long chains of ? : after the colons
        $left_bond_strength{':'}  = 0.4 * WEAK + 0.6 * NOMINAL;
        $right_bond_strength{':'} = NO_BREAK;
        $left_bond_strength{'?'}  = $left_bond_strength{':'} + 0.01;
        $right_bond_strength{'?'} = NO_BREAK;

        $left_bond_strength{','}  = VERY_STRONG;
        $right_bond_strength{','} = VERY_WEAK;

        # remaining digraphs and trigraphs not defined above
        @q                       = qw( :: <> ++ --);
        @left_bond_strength{@q}  = (WEAK) x scalar(@q);
        @right_bond_strength{@q} = (STRONG) x scalar(@q);

        # Set bond strengths of certain keywords
        # make 'or', 'err', 'and' slightly weaker than a ','
        $left_bond_strength{'and'}  = VERY_WEAK - 0.01;
        $left_bond_strength{'or'}   = VERY_WEAK - 0.02;
        $left_bond_strength{'err'}  = VERY_WEAK - 0.02;
        $left_bond_strength{'xor'}  = VERY_WEAK - 0.01;
        $right_bond_strength{'and'} = NOMINAL;
        $right_bond_strength{'or'}  = NOMINAL;
        $right_bond_strength{'err'} = NOMINAL;
        $right_bond_strength{'xor'} = NOMINAL;

        #---------------------------------------------------------------
        # Bond Strength BEGIN Section 2.
        # Set binary rules for bond strengths between certain token types.
        #---------------------------------------------------------------

        #  We have a little problem making tables which apply to the
        #  container tokens.  Here is a list of container tokens and
        #  their types:
        #
        #   type    tokens // meaning
        #      {    {, [, ( // indent
        #      }    }, ], ) // outdent
        #      [    [ // left non-structural [ (enclosing an array index)
        #      ]    ] // right non-structural square bracket
        #      (    ( // left non-structural paren
        #      )    ) // right non-structural paren
        #      L    { // left non-structural curly brace (enclosing a key)
        #      R    } // right non-structural curly brace
        #
        #  Some rules apply to token types and some to just the token
        #  itself.  We solve the problem by combining type and token into a
        #  new hash key for the container types.
        #
        #  If a rule applies to a token 'type' then we need to make rules
        #  for each of these 'type.token' combinations:
        #  Type    Type.Token
        #  {       {{, {[, {(
        #  [       [[
        #  (       ((
        #  L       L{
        #  }       }}, }], })
        #  ]       ]]
        #  )       ))
        #  R       R}
        #
        #  If a rule applies to a token then we need to make rules for
        #  these 'type.token' combinations:
        #  Token   Type.Token
        #  {       {{, L{
        #  [       {[, [[
        #  (       {(, ((
        #  }       }}, R}
        #  ]       }], ]]
        #  )       }), ))

        # allow long lines before final { in an if statement, as in:
        #    if (..........
        #      ..........)
        #    {
        #
        # Otherwise, the line before the { tends to be too short.

        $binary_bond_strength{'))'}{'{{'} = VERY_WEAK + 0.03;
        $binary_bond_strength{'(('}{'{{'} = NOMINAL;

        # break on something like '} (', but keep this stronger than a ','
        # example is in 'howe.pl'
        $binary_bond_strength{'R}'}{'(('} = 0.8 * VERY_WEAK + 0.2 * WEAK;
        $binary_bond_strength{'}}'}{'(('} = 0.8 * VERY_WEAK + 0.2 * WEAK;

        # keep matrix and hash indices together
        # but make them a little below STRONG to allow breaking open
        # something like {'some-word'}{'some-very-long-word'} at the }{
        # (bracebrk.t)
        $binary_bond_strength{']]'}{'[['} = 0.9 * STRONG + 0.1 * NOMINAL;
        $binary_bond_strength{']]'}{'L{'} = 0.9 * STRONG + 0.1 * NOMINAL;
        $binary_bond_strength{'R}'}{'[['} = 0.9 * STRONG + 0.1 * NOMINAL;
        $binary_bond_strength{'R}'}{'L{'} = 0.9 * STRONG + 0.1 * NOMINAL;

        # increase strength to the point where a break in the following
        # will be after the opening paren rather than at the arrow:
        #    $a->$b($c);
        $binary_bond_strength{'i'}{'->'} = 1.45 * STRONG;

    # Note that the following alternative strength would make the break at the
    # '->' rather than opening the '('.  Both have advantages and disadvantages.
    # $binary_bond_strength{'i'}{'->'} = 0.5*STRONG + 0.5 * NOMINAL; #

        $binary_bond_strength{'))'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;
        $binary_bond_strength{']]'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;
        $binary_bond_strength{'})'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;
        $binary_bond_strength{'}]'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;
        $binary_bond_strength{'}}'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;
        $binary_bond_strength{'R}'}{'->'} = 0.1 * STRONG + 0.9 * NOMINAL;

        $binary_bond_strength{'))'}{'[['} = 0.2 * STRONG + 0.8 * NOMINAL;
        $binary_bond_strength{'})'}{'[['} = 0.2 * STRONG + 0.8 * NOMINAL;
        $binary_bond_strength{'))'}{'{['} = 0.2 * STRONG + 0.8 * NOMINAL;
        $binary_bond_strength{'})'}{'{['} = 0.2 * STRONG + 0.8 * NOMINAL;

        #---------------------------------------------------------------
        # Binary NO_BREAK rules
        #---------------------------------------------------------------

        # use strict requires that bare word and => not be separated
        $binary_bond_strength{'C'}{'=>'} = NO_BREAK;
        $binary_bond_strength{'U'}{'=>'} = NO_BREAK;

        # Never break between a bareword and a following paren because
        # perl may give an error.  For example, if a break is placed
        # between 'to_filehandle' and its '(' the following line will
        # give a syntax error [Carp.pm]: my( $no) =fileno(
        # to_filehandle( $in)) ;
        $binary_bond_strength{'C'}{'(('} = NO_BREAK;
        $binary_bond_strength{'C'}{'{('} = NO_BREAK;
        $binary_bond_strength{'U'}{'(('} = NO_BREAK;
        $binary_bond_strength{'U'}{'{('} = NO_BREAK;

        # use strict requires that bare word within braces not start new
        # line
        $binary_bond_strength{'L{'}{'w'} = NO_BREAK;

        $binary_bond_strength{'w'}{'R}'} = NO_BREAK;

        # The following two rules prevent a syntax error caused by breaking up
        # a construction like '{-y}'.  The '-' quotes the 'y' and prevents
        # it from being taken as a transliteration. We have to keep
        # token types 'L m w' together to prevent this error.
        $binary_bond_strength{'L{'}{'m'}        = NO_BREAK;
        $binary_bond_strength_nospace{'m'}{'w'} = NO_BREAK;

        # keep 'bareword-' together, but only if there is no space between
        # the word and dash. Do not keep together if there is a space.
        # example 'use perl6-alpha'
        $binary_bond_strength_nospace{'w'}{'m'} = NO_BREAK;

        # use strict requires that bare word and => not be separated
        $binary_bond_strength{'w'}{'=>'} = NO_BREAK;

        # use strict does not allow separating type info from trailing { }
        # testfile is readmail.pl
        $binary_bond_strength{'t'}{'L{'} = NO_BREAK;
        $binary_bond_strength{'i'}{'L{'} = NO_BREAK;

        # As a defensive measure, do not break between a '(' and a
        # filehandle.  In some cases, this can cause an error.  For
        # example, the following program works:
        #    my $msg="hi!\n";
        #    print
        #    ( STDOUT
        #    $msg
        #    );
        #
        # But this program fails:
        #    my $msg="hi!\n";
        #    print
        #    (
        #    STDOUT
        #    $msg
        #    );
        #
        # This is normally only a problem with the 'extrude' option
        $binary_bond_strength{'(('}{'Y'} = NO_BREAK;
        $binary_bond_strength{'{('}{'Y'} = NO_BREAK;

        # never break between sub name and opening paren
        $binary_bond_strength{'w'}{'(('} = NO_BREAK;
        $binary_bond_strength{'w'}{'{('} = NO_BREAK;

        # keep '}' together with ';'
        $binary_bond_strength{'}}'}{';'} = NO_BREAK;

        # Breaking before a ++ can cause perl to guess wrong. For
        # example the following line will cause a syntax error
        # with -extrude if we break between '$i' and '++' [fixstyle2]
        #   print( ( $i++ & 1 ) ? $_ : ( $change{$_} || $_ ) );
        $nobreak_lhs{'++'} = NO_BREAK;

        # Do not break before a possible file handle
        $nobreak_lhs{'Z'} = NO_BREAK;

        # use strict hates bare words on any new line.  For
        # example, a break before the underscore here provokes the
        # wrath of use strict:
        # if ( -r $fn && ( -s _ || $AllowZeroFilesize)) {
        $nobreak_rhs{'F'}      = NO_BREAK;
        $nobreak_rhs{'CORE::'} = NO_BREAK;

        #---------------------------------------------------------------
        # Bond Strength BEGIN Section 3.
        # Define tables and values for applying a small bias to the above
        # values.
        #---------------------------------------------------------------
        # Adding a small 'bias' to strengths is a simple way to make a line
        # break at the first of a sequence of identical terms.  For
        # example, to force long string of conditional operators to break
        # with each line ending in a ':', we can add a small number to the
        # bond strength of each ':' (colon.t)
        @bias_tokens = qw( : && || f and or . );       # tokens which get bias
        %bias_hash   = map { $_ => 0 } @bias_tokens;
        $delta_bias  = 0.0001;    # a very small strength level
        return;

    } ## end sub initialize_bond_strength_hashes

    use constant DEBUG_BOND => 0;

    sub set_bond_strengths {

        my ($self) = @_;

        # patch-its always ok to break at end of line
        $nobreak_to_go[$max_index_to_go] = 0;

        my $rOpts_short_concatenation_item_length =
          $rOpts->{'short-concatenation-item-length'};

        # we start a new set of bias values for each line
        %bias = %bias_hash;

        my $code_bias = -.01;    # bias for closing block braces

        my $type         = 'b';
        my $token        = ' ';
        my $token_length = 1;
        my $last_type;
        my $last_nonblank_type  = $type;
        my $last_nonblank_token = $token;
        my $list_str            = $left_bond_strength{'?'};

        my ( $block_type, $i_next, $i_next_nonblank, $next_nonblank_token,
            $next_nonblank_type, $next_token, $next_type, $total_nesting_depth,
        );

        # main loop to compute bond strengths between each pair of tokens
        foreach my $i ( 0 .. $max_index_to_go ) {
            $last_type = $type;
            if ( $type ne 'b' ) {
                $last_nonblank_type  = $type;
                $last_nonblank_token = $token;
            }
            $type = $types_to_go[$i];

            # strength on both sides of a blank is the same
            if ( $type eq 'b' && $last_type ne 'b' ) {
                $bond_strength_to_go[$i] = $bond_strength_to_go[ $i - 1 ];
                next;
            }

            $token               = $tokens_to_go[$i];
            $token_length        = $token_lengths_to_go[$i];
            $block_type          = $block_type_to_go[$i];
            $i_next              = $i + 1;
            $next_type           = $types_to_go[$i_next];
            $next_token          = $tokens_to_go[$i_next];
            $total_nesting_depth = $nesting_depth_to_go[$i_next];
            $i_next_nonblank     = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
            $next_nonblank_type  = $types_to_go[$i_next_nonblank];
            $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

            my $seqno               = $type_sequence_to_go[$i];
            my $next_nonblank_seqno = $type_sequence_to_go[$i_next_nonblank];

            # We are computing the strength of the bond between the current
            # token and the NEXT token.

            #---------------------------------------------------------------
            # Bond Strength Section 1:
            # First Approximation.
            # Use minimum of individual left and right tabulated bond
            # strengths.
            #---------------------------------------------------------------
            my $bsr = $right_bond_strength{$type};
            my $bsl = $left_bond_strength{$next_nonblank_type};

            # define right bond strengths of certain keywords
            if ( $type eq 'k' && defined( $right_bond_strength{$token} ) ) {
                $bsr = $right_bond_strength{$token};
            }
            elsif ( $token eq 'ne' or $token eq 'eq' ) {
                $bsr = NOMINAL;
            }

            # set terminal bond strength to the nominal value
            # this will cause good preceding breaks to be retained
            if ( $i_next_nonblank > $max_index_to_go ) {
                $bsl = NOMINAL;
            }

            # define right bond strengths of certain keywords
            if ( $next_nonblank_type eq 'k'
                && defined( $left_bond_strength{$next_nonblank_token} ) )
            {
                $bsl = $left_bond_strength{$next_nonblank_token};
            }
            elsif ($next_nonblank_token eq 'ne'
                or $next_nonblank_token eq 'eq' )
            {
                $bsl = NOMINAL;
            }
            elsif ( $is_lt_gt_le_ge{$next_nonblank_token} ) {
                $bsl = 0.9 * NOMINAL + 0.1 * STRONG;
            }

            # Use the minimum of the left and right strengths.  Note: it might
            # seem that we would want to keep a NO_BREAK if either token has
            # this value.  This didn't work, for example because in an arrow
            # list, it prevents the comma from separating from the following
            # bare word (which is probably quoted by its arrow).  So necessary
            # NO_BREAK's have to be handled as special cases in the final
            # section.
            if ( !defined($bsr) ) { $bsr = VERY_STRONG }
            if ( !defined($bsl) ) { $bsl = VERY_STRONG }
            my $bond_str   = ( $bsr < $bsl ) ? $bsr : $bsl;
            my $bond_str_1 = $bond_str;

            #---------------------------------------------------------------
            # Bond Strength Section 2:
            # Apply hardwired rules..
            #---------------------------------------------------------------

            # Patch to put terminal or clauses on a new line: Weaken the bond
            # at an || followed by die or similar keyword to make the terminal
            # or clause fall on a new line, like this:
            #
            #   my $class = shift
            #     || die "Cannot add broadcast:  No class identifier found";
            #
            # Otherwise the break will be at the previous '=' since the || and
            # = have the same starting strength and the or is biased, like
            # this:
            #
            # my $class =
            #   shift || die "Cannot add broadcast:  No class identifier found";
            #
            # In any case if the user places a break at either the = or the ||
            # it should remain there.
            if ( $type eq '||' || $type eq 'k' && $token eq 'or' ) {
                if ( $next_nonblank_token =~ /^(die|confess|croak|warn)$/ ) {
                    if ( $want_break_before{$token} && $i > 0 ) {
                        $bond_strength_to_go[ $i - 1 ] -= $delta_bias;

                        # keep bond strength of a token and its following blank
                        # the same
                        if ( $types_to_go[ $i - 1 ] eq 'b' && $i > 2 ) {
                            $bond_strength_to_go[ $i - 2 ] -= $delta_bias;
                        }
                    }
                    else {
                        $bond_str -= $delta_bias;
                    }
                }
            }

            # good to break after end of code blocks
            if ( $type eq '}' && $block_type && $next_nonblank_type ne ';' ) {

                $bond_str = 0.5 * WEAK + 0.5 * VERY_WEAK + $code_bias;
                $code_bias += $delta_bias;
            }

            if ( $type eq 'k' ) {

                # allow certain control keywords to stand out
                if (   $next_nonblank_type eq 'k'
                    && $is_last_next_redo_return{$token} )
                {
                    $bond_str = 0.45 * WEAK + 0.55 * VERY_WEAK;
                }

                # Don't break after keyword my.  This is a quick fix for a
                # rare problem with perl. An example is this line from file
                # Container.pm:

                # foreach my $question( Debian::DebConf::ConfigDb::gettree(
                # $this->{'question'} ) )

                if ( $token eq 'my' ) {
                    $bond_str = NO_BREAK;
                }

            }

            # good to break before 'if', 'unless', etc
            if ( $is_if_brace_follower{$next_nonblank_token} ) {
                $bond_str = VERY_WEAK;
            }

            if ( $next_nonblank_type eq 'k' && $type ne 'CORE::' ) {

                # FIXME: needs more testing
                if ( $is_keyword_returning_list{$next_nonblank_token} ) {
                    $bond_str = $list_str if ( $bond_str > $list_str );
                }

                # keywords like 'unless', 'if', etc, within statements
                # make good breaks
                if ( $is_good_keyword_breakpoint{$next_nonblank_token} ) {
                    $bond_str = VERY_WEAK / 1.05;
                }
            }

            # try not to break before a comma-arrow
            elsif ( $next_nonblank_type eq '=>' ) {
                if ( $bond_str < STRONG ) { $bond_str = STRONG }
            }

            #---------------------------------------------------------------
            # Additional hardwired NOBREAK rules
            #---------------------------------------------------------------

            # map1.t -- correct for a quirk in perl
            if (   $token eq '('
                && $next_nonblank_type eq 'i'
                && $last_nonblank_type eq 'k'
                && $is_sort_map_grep{$last_nonblank_token} )

              #     /^(sort|map|grep)$/ )
            {
                $bond_str = NO_BREAK;
            }

            # extrude.t: do not break before paren at:
            #    -l pid_filename(
            if ( $last_nonblank_type eq 'F' && $next_nonblank_token eq '(' ) {
                $bond_str = NO_BREAK;
            }

            # in older version of perl, use strict can cause problems with
            # breaks before bare words following opening parens.  For example,
            # this will fail under older versions if a break is made between
            # '(' and 'MAIL': use strict; open( MAIL, "a long filename or
            # command"); close MAIL;
            if ( $type eq '{' ) {

                if ( $token eq '(' && $next_nonblank_type eq 'w' ) {

                    # but it's fine to break if the word is followed by a '=>'
                    # or if it is obviously a sub call
                    my $i_next_next_nonblank = $i_next_nonblank + 1;
                    my $next_next_type = $types_to_go[$i_next_next_nonblank];
                    if (   $next_next_type eq 'b'
                        && $i_next_nonblank < $max_index_to_go )
                    {
                        $i_next_next_nonblank++;
                        $next_next_type = $types_to_go[$i_next_next_nonblank];
                    }

                    # We'll check for an old breakpoint and keep a leading
                    # bareword if it was that way in the input file.
                    # Presumably it was ok that way.  For example, the
                    # following would remain unchanged:
                    #
                    # @months = (
                    #   January,   February, March,    April,
                    #   May,       June,     July,     August,
                    #   September, October,  November, December,
                    # );
                    #
                    # This should be sufficient:
                    if (
                        !$old_breakpoint_to_go[$i]
                        && (   $next_next_type eq ','
                            || $next_next_type eq '}' )
                      )
                    {
                        $bond_str = NO_BREAK;
                    }
                }
            }

            # Do not break between a possible filehandle and a ? or / and do
            # not introduce a break after it if there is no blank
            # (extrude.t)
            elsif ( $type eq 'Z' ) {

                # don't break..
                if (

                    # if there is no blank and we do not want one. Examples:
                    #    print $x++    # do not break after $x
                    #    print HTML"HELLO"   # break ok after HTML
                    (
                           $next_type ne 'b'
                        && defined( $want_left_space{$next_type} )
                        && $want_left_space{$next_type} == WS_NO
                    )

                    # or we might be followed by the start of a quote
                    || $next_nonblank_type =~ /^[\/\?]$/
                  )
                {
                    $bond_str = NO_BREAK;
                }
            }

            # Breaking before a ? before a quote can cause trouble if
            # they are not separated by a blank.
            # Example: a syntax error occurs if you break before the ? here
            #  my$logic=join$all?' && ':' || ',@regexps;
            # From: Professional_Perl_Programming_Code/multifind.pl
            if ( $next_nonblank_type eq '?' ) {
                $bond_str = NO_BREAK
                  if ( $types_to_go[ $i_next_nonblank + 1 ] eq 'Q' );
            }

            # Breaking before a . followed by a number
            # can cause trouble if there is no intervening space
            # Example: a syntax error occurs if you break before the .2 here
            #  $str .= pack($endian.2, ensurrogate($ord));
            # From: perl58/Unicode.pm
            elsif ( $next_nonblank_type eq '.' ) {
                $bond_str = NO_BREAK
                  if ( $types_to_go[ $i_next_nonblank + 1 ] eq 'n' );
            }

            my $bond_str_2 = $bond_str;

            #---------------------------------------------------------------
            # End of hardwired rules
            #---------------------------------------------------------------

            #---------------------------------------------------------------
            # Bond Strength Section 3:
            # Apply table rules. These have priority over the above
            # hardwired rules.
            #---------------------------------------------------------------

            my $tabulated_bond_str;
            my $ltype = $type;
            my $rtype = $next_nonblank_type;
            if ( $seqno && $is_container_token{$token} ) {
                $ltype = $type . $token;
            }

            if (   $next_nonblank_seqno
                && $is_container_token{$next_nonblank_token} )
            {
                $rtype = $next_nonblank_type . $next_nonblank_token;
            }

            # apply binary rules which apply regardless of space between tokens
            if ( $binary_bond_strength{$ltype}{$rtype} ) {
                $bond_str           = $binary_bond_strength{$ltype}{$rtype};
                $tabulated_bond_str = $bond_str;
            }

            # apply binary rules which apply only if no space between tokens
            if ( $binary_bond_strength_nospace{$ltype}{$next_type} ) {
                $bond_str           = $binary_bond_strength{$ltype}{$next_type};
                $tabulated_bond_str = $bond_str;
            }

            if ( $nobreak_rhs{$ltype} || $nobreak_lhs{$rtype} ) {
                $bond_str           = NO_BREAK;
                $tabulated_bond_str = $bond_str;
            }
            my $bond_str_3 = $bond_str;

            # If the hardwired rules conflict with the tabulated bond
            # strength then there is an inconsistency that should be fixed
            DEBUG_BOND
              && $tabulated_bond_str
              && $bond_str_1
              && $bond_str_1 != $bond_str_2
              && $bond_str_2 != $tabulated_bond_str
              && do {
                print STDERR
"BOND_TABLES: ltype=$ltype rtype=$rtype $bond_str_1->$bond_str_2->$bond_str_3\n";
              };

           #-----------------------------------------------------------------
           # Bond Strength Section 4:
           # Modify strengths of certain tokens which often occur in sequence
           # by adding a small bias to each one in turn so that the breaks
           # occur from left to right.
           #
           # Note that we only changing strengths by small amounts here,
           # and usually increasing, so we should not be altering any NO_BREAKs.
           # Other routines which check for NO_BREAKs will use a tolerance
           # of one to avoid any problem.
           #-----------------------------------------------------------------

            # The bias tables use special keys:
            #   $type - if not keyword
            #   $token - if keyword, but map some keywords together
            my $left_key =
              $type eq 'k' ? $token eq 'err' ? 'or' : $token : $type;
            my $right_key =
                $next_nonblank_type eq 'k'
              ? $next_nonblank_token eq 'err'
                  ? 'or'
                  : $next_nonblank_token
              : $next_nonblank_type;

            # add any bias set by sub scan_list at old comma break points.
            if ( $type eq ',' ) { $bond_str += $bond_strength_to_go[$i] }

            # bias left token
            elsif ( defined( $bias{$left_key} ) ) {
                if ( !$want_break_before{$left_key} ) {
                    $bias{$left_key} += $delta_bias;
                    $bond_str += $bias{$left_key};
                }
            }

            # bias right token
            if ( defined( $bias{$right_key} ) ) {
                if ( $want_break_before{$right_key} ) {

                    # for leading '.' align all but 'short' quotes; the idea
                    # is to not place something like "\n" on a single line.
                    if ( $right_key eq '.' ) {
                        unless (
                            $last_nonblank_type eq '.'
                            && ( $token_length <=
                                $rOpts_short_concatenation_item_length )
                            && ( !$is_closing_token{$token} )
                          )
                        {
                            $bias{$right_key} += $delta_bias;
                        }
                    }
                    else {
                        $bias{$right_key} += $delta_bias;
                    }
                    $bond_str += $bias{$right_key};
                }
            }
            my $bond_str_4 = $bond_str;

            #---------------------------------------------------------------
            # Bond Strength Section 5:
            # Fifth Approximation.
            # Take nesting depth into account by adding the nesting depth
            # to the bond strength.
            #---------------------------------------------------------------
            my $strength;

            if ( defined($bond_str) && !$nobreak_to_go[$i] ) {
                if ( $total_nesting_depth > 0 ) {
                    $strength = $bond_str + $total_nesting_depth;
                }
                else {
                    $strength = $bond_str;
                }
            }
            else {
                $strength = NO_BREAK;

                # For critical code such as lines with here targets we must
                # be absolutely sure that we do not allow a break.  So for
                # these the nobreak flag exceeds 1 as a signal. Otherwise we
                # can run into trouble when small tolerances are added.
                $strength += 1 if ( $nobreak_to_go[$i] > 1 );
            }

            #---------------------------------------------------------------
            # Bond Strength Section 6:
            # Sixth Approximation. Welds.
            #---------------------------------------------------------------

            # Do not allow a break within welds,
            if ( $seqno && $total_weld_count ) {
                if ( $self->weld_len_right( $seqno, $type ) ) {
                    $strength = NO_BREAK;
                }

                # But encourage breaking after opening welded tokens
                elsif ($is_opening_token{$token}
                    && $self->weld_len_left( $seqno, $type ) )
                {
                    $strength -= 1;
                }
            }

            # always break after side comment
            if ( $type eq '#' ) { $strength = 0 }

            $bond_strength_to_go[$i] = $strength;

            DEBUG_BOND && do {
                my $str = substr( $token, 0, 15 );
                $str .= ' ' x ( 16 - length($str) );
                print STDOUT
"BOND:  i=$i $str $type $next_nonblank_type depth=$total_nesting_depth strength=$bond_str_1 -> $bond_str_2 -> $bond_str_3 -> $bond_str_4 $bond_str -> $strength \n";
            };
        } ## end main loop
        return;
    } ## end sub set_bond_strengths
} ## end closure set_bond_strengths

sub bad_pattern {

    # See if a pattern will compile. We have to use a string eval here,
    # but it should be safe because the pattern has been constructed
    # by this program.
    my ($pattern) = @_;
    eval "'##'=~/$pattern/";
    return $@;
}

{    ## begin closure prepare_cuddled_block_types

    my %no_cuddle;

    # Add keywords here which really should not be cuddled
    BEGIN {
        my @q = qw(if unless for foreach while);
        @no_cuddle{@q} = (1) x scalar(@q);
    }

    sub prepare_cuddled_block_types {

        # the cuddled-else style, if used, is controlled by a hash that
        # we construct here

        # Include keywords here which should not be cuddled

        my $cuddled_string = "";
        if ( $rOpts->{'cuddled-else'} ) {

            # set the default
            $cuddled_string = 'elsif else continue catch finally'
              unless ( $rOpts->{'cuddled-block-list-exclusive'} );

            # This is the old equivalent but more complex version
            # $cuddled_string = 'if-elsif-else unless-elsif-else -continue ';

            # Add users other blocks to be cuddled
            my $cuddled_block_list = $rOpts->{'cuddled-block-list'};
            if ($cuddled_block_list) {
                $cuddled_string .= " " . $cuddled_block_list;
            }

        }

        # If we have a cuddled string of the form
        #  'try-catch-finally'

        # we want to prepare a hash of the form

        # $rcuddled_block_types = {
        #    'try' => {
        #        'catch'   => 1,
        #        'finally' => 1
        #    },
        # };

        # use -dcbl to dump this hash

        # Multiple such strings are input as a space or comma separated list

        # If we get two lists with the same leading type, such as
        #   -cbl = "-try-catch-finally  -try-catch-otherwise"
        # then they will get merged as follows:
        # $rcuddled_block_types = {
        #    'try' => {
        #        'catch'     => 1,
        #        'finally'   => 2,
        #        'otherwise' => 1,
        #    },
        # };
        # This will allow either type of chain to be followed.

        $cuddled_string =~ s/,/ /g;    # allow space or comma separated lists
        my @cuddled_strings = split /\s+/, $cuddled_string;

        $rcuddled_block_types = {};

        # process each dash-separated string...
        my $string_count = 0;
        foreach my $string (@cuddled_strings) {
            next unless $string;
            my @words = split /-+/, $string;    # allow multiple dashes

            # we could look for and report possible errors here...
            next unless ( @words > 0 );

           # allow either '-continue' or *-continue' for arbitrary starting type
            my $start = '*';

            # a single word without dashes is a secondary block type
            if ( @words > 1 ) {
                $start = shift @words;
            }

            # always make an entry for the leading word. If none follow, this
            # will still prevent a wildcard from matching this word.
            if ( !defined( $rcuddled_block_types->{$start} ) ) {
                $rcuddled_block_types->{$start} = {};
            }

            # The count gives the original word order in case we ever want it.
            $string_count++;
            my $word_count = 0;
            foreach my $word (@words) {
                next unless $word;
                if ( $no_cuddle{$word} ) {
                    Warn(
"## Ignoring keyword '$word' in -cbl; does not seem right\n"
                    );
                    next;
                }
                $word_count++;
                $rcuddled_block_types->{$start}->{$word} =
                  1;    #"$string_count.$word_count";

                # git#9: Remove this word from the list of desired one-line
                # blocks
                $want_one_line_block{$word} = 0;
            }
        }
        return;
    }
}    ## begin closure prepare_cuddled_block_types

sub dump_cuddled_block_list {
    my ($fh) = @_;

    # ORIGINAL METHOD: Here is the format of the cuddled block type hash
    # which controls this routine
    #    my $rcuddled_block_types = {
    #        'if' => {
    #            'else'  => 1,
    #            'elsif' => 1
    #        },
    #        'try' => {
    #            'catch'   => 1,
    #            'finally' => 1
    #        },
    #    };

    # SIMPLFIED METHOD: the simplified method uses a wildcard for
    # the starting block type and puts all cuddled blocks together:
    #    my $rcuddled_block_types = {
    #        '*' => {
    #            'else'  => 1,
    #            'elsif' => 1
    #            'catch'   => 1,
    #            'finally' => 1
    #        },
    #    };

    # Both methods work, but the simplified method has proven to be adequate and
    # easier to manage.

    my $cuddled_string = $rOpts->{'cuddled-block-list'};
    $cuddled_string = '' unless $cuddled_string;

    my $flags = "";
    $flags .= "-ce" if ( $rOpts->{'cuddled-else'} );
    $flags .= " -cbl='$cuddled_string'";

    unless ( $rOpts->{'cuddled-else'} ) {
        $flags .= "\nNote: You must specify -ce to generate a cuddled hash";
    }

    $fh->print(<<EOM);
------------------------------------------------------------------------
Hash of cuddled block types prepared for a run with these parameters:
  $flags
------------------------------------------------------------------------
EOM

    use Data::Dumper;
    $fh->print( Dumper($rcuddled_block_types) );

    $fh->print(<<EOM);
------------------------------------------------------------------------
EOM
    return;
}

sub make_static_block_comment_pattern {

    # create the pattern used to identify static block comments
    $static_block_comment_pattern = '^\s*##';

    # allow the user to change it
    if ( $rOpts->{'static-block-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-block-comment-prefix'};
        $prefix =~ s/^\s*//;
        my $pattern = $prefix;

        # user may give leading caret to force matching left comments only
        if ( $prefix !~ /^\^#/ ) {
            if ( $prefix !~ /^#/ ) {
                Die(
"ERROR: the -sbcp prefix is '$prefix' but must begin with '#' or '^#'\n"
                );
            }
            $pattern = '^\s*' . $prefix;
        }
        if ( bad_pattern($pattern) ) {
            Die(
"ERROR: the -sbc prefix '$prefix' causes the invalid regex '$pattern'\n"
            );
        }
        $static_block_comment_pattern = $pattern;
    }
    return;
}

sub make_format_skipping_pattern {
    my ( $opt_name, $default ) = @_;
    my $param = $rOpts->{$opt_name};
    unless ($param) { $param = $default }
    $param =~ s/^\s*//;
    if ( $param !~ /^#/ ) {
        Die("ERROR: the $opt_name parameter '$param' must begin with '#'\n");
    }
    my $pattern = '^' . $param . '\s';
    if ( bad_pattern($pattern) ) {
        Die(
"ERROR: the $opt_name parameter '$param' causes the invalid regex '$pattern'\n"
        );
    }
    return $pattern;
}

sub make_non_indenting_brace_pattern {

    # Create the pattern used to identify static side comments.
    # Note that we are ending the pattern in a \s. This will allow
    # the pattern to be followed by a space and some text, or a newline.
    # The pattern is used in sub 'non_indenting_braces'
    $non_indenting_brace_pattern = '^#<<<\s';

    # allow the user to change it
    if ( $rOpts->{'non-indenting-brace-prefix'} ) {
        my $prefix = $rOpts->{'non-indenting-brace-prefix'};
        $prefix =~ s/^\s*//;
        if ( $prefix !~ /^#/ ) {
            Die("ERROR: the -nibp parameter '$prefix' must begin with '#'\n");
        }
        my $pattern = '^' . $prefix . '\s';
        if ( bad_pattern($pattern) ) {
            Die(
"ERROR: the -nibp prefix '$prefix' causes the invalid regex '$pattern'\n"
            );
        }
        $non_indenting_brace_pattern = $pattern;
    }
    return;
}

sub make_closing_side_comment_list_pattern {

    # turn any input list into a regex for recognizing selected block types
    $closing_side_comment_list_pattern = '^\w+';
    if ( defined( $rOpts->{'closing-side-comment-list'} )
        && $rOpts->{'closing-side-comment-list'} )
    {
        $closing_side_comment_list_pattern =
          make_block_pattern( '-cscl', $rOpts->{'closing-side-comment-list'} );
    }
    return;
}

sub make_sub_matching_pattern {

    # Patterns for standardizing matches to block types for regular subs and
    # anonymous subs. Examples
    #  'sub process' is a named sub
    #  'sub ::m' is a named sub
    #  'sub' is an anonymous sub
    #  'sub:' is a label, not a sub
    #  'substr' is a keyword
    $SUB_PATTERN    = '^sub\s+(::|\w)';    # match normal sub
    $ASUB_PATTERN   = '^sub$';             # match anonymous sub
    $ANYSUB_PATTERN = '^sub\b';            # match either type of sub

    # Note (see also RT #133130): These patterns are used by
    # sub make_block_pattern, which is used for making most patterns.
    # So this sub needs to be called before other pattern-making routines.

    if ( $rOpts->{'sub-alias-list'} ) {

        # Note that any 'sub-alias-list' has been preprocessed to
        # be a trimmed, space-separated list which includes 'sub'
        # for example, it might be 'sub method fun'
        my $sub_alias_list = $rOpts->{'sub-alias-list'};
        $sub_alias_list =~ s/\s+/\|/g;
        $SUB_PATTERN    =~ s/sub/\($sub_alias_list\)/;
        $ASUB_PATTERN   =~ s/sub/\($sub_alias_list\)/;
        $ANYSUB_PATTERN =~ s/sub/\($sub_alias_list\)/;
    }
    return;
}

sub make_bli_pattern {

    # default list of block types for which -bli would apply
    my $bli_list_string = 'if else elsif unless while for foreach do : sub';

    if ( defined( $rOpts->{'brace-left-and-indent-list'} )
        && $rOpts->{'brace-left-and-indent-list'} )
    {
        $bli_list_string = $rOpts->{'brace-left-and-indent-list'};
    }

    $bli_pattern = make_block_pattern( '-blil', $bli_list_string );
    return;
}

sub make_keyword_group_list_pattern {

    # turn any input list into a regex for recognizing selected block types.
    # Here are the defaults:
    $keyword_group_list_pattern         = '^(our|local|my|use|require|)$';
    $keyword_group_list_comment_pattern = '';
    if ( defined( $rOpts->{'keyword-group-blanks-list'} )
        && $rOpts->{'keyword-group-blanks-list'} )
    {
        my @words = split /\s+/, $rOpts->{'keyword-group-blanks-list'};
        my @keyword_list;
        my @comment_list;
        foreach my $word (@words) {
            if ( $word =~ /^(BC|SBC)$/ ) {
                push @comment_list, $word;
                if ( $word eq 'SBC' ) { push @comment_list, 'SBCX' }
            }
            else {
                push @keyword_list, $word;
            }
        }
        $keyword_group_list_pattern =
          make_block_pattern( '-kgbl', $rOpts->{'keyword-group-blanks-list'} );
        $keyword_group_list_comment_pattern =
          make_block_pattern( '-kgbl', join( ' ', @comment_list ) );
    }
    return;
}

sub make_block_brace_vertical_tightness_pattern {

    # turn any input list into a regex for recognizing selected block types
    $block_brace_vertical_tightness_pattern =
      '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';
    if ( defined( $rOpts->{'block-brace-vertical-tightness-list'} )
        && $rOpts->{'block-brace-vertical-tightness-list'} )
    {
        $block_brace_vertical_tightness_pattern =
          make_block_pattern( '-bbvtl',
            $rOpts->{'block-brace-vertical-tightness-list'} );
    }
    return;
}

sub make_blank_line_pattern {

    $blank_lines_before_closing_block_pattern = $SUB_PATTERN;
    my $key = 'blank-lines-before-closing-block-list';
    if ( defined( $rOpts->{$key} ) && $rOpts->{$key} ) {
        $blank_lines_before_closing_block_pattern =
          make_block_pattern( '-blbcl', $rOpts->{$key} );
    }

    $blank_lines_after_opening_block_pattern = $SUB_PATTERN;
    $key = 'blank-lines-after-opening-block-list';
    if ( defined( $rOpts->{$key} ) && $rOpts->{$key} ) {
        $blank_lines_after_opening_block_pattern =
          make_block_pattern( '-blaol', $rOpts->{$key} );
    }
    return;
}

sub make_block_pattern {

    #  given a string of block-type keywords, return a regex to match them
    #  The only tricky part is that labels are indicated with a single ':'
    #  and the 'sub' token text may have additional text after it (name of
    #  sub).
    #
    #  Example:
    #
    #   input string: "if else elsif unless while for foreach do : sub";
    #   pattern:  '^((if|else|elsif|unless|while|for|foreach|do|\w+:)$|sub)';

    #  Minor Update:
    #
    #  To distinguish between anonymous subs and named subs, use 'sub' to
    #   indicate a named sub, and 'asub' to indicate an anonymous sub

    my ( $abbrev, $string ) = @_;
    my @list  = split_words($string);
    my @words = ();
    my %seen;
    for my $i (@list) {
        if ( $i eq '*' ) { my $pattern = '^.*'; return $pattern }
        next if $seen{$i};
        $seen{$i} = 1;
        if ( $i eq 'sub' ) {
        }
        elsif ( $i eq 'asub' ) {
        }
        elsif ( $i eq ';' ) {
            push @words, ';';
        }
        elsif ( $i eq '{' ) {
            push @words, '\{';
        }
        elsif ( $i eq ':' ) {
            push @words, '\w+:';
        }
        elsif ( $i =~ /^\w/ ) {
            push @words, $i;
        }
        else {
            Warn("unrecognized block type $i after $abbrev, ignoring\n");
        }
    }
    my $pattern      = '(' . join( '|', @words ) . ')$';
    my $sub_patterns = "";
    if ( $seen{'sub'} ) {
        $sub_patterns .= '|' . $SUB_PATTERN;
    }
    if ( $seen{'asub'} ) {
        $sub_patterns .= '|' . $ASUB_PATTERN;
    }
    if ($sub_patterns) {
        $pattern = '(' . $pattern . $sub_patterns . ')';
    }
    $pattern = '^' . $pattern;
    return $pattern;
}

sub make_static_side_comment_pattern {

    # create the pattern used to identify static side comments
    $static_side_comment_pattern = '^##';

    # allow the user to change it
    if ( $rOpts->{'static-side-comment-prefix'} ) {
        my $prefix = $rOpts->{'static-side-comment-prefix'};
        $prefix =~ s/^\s*//;
        my $pattern = '^' . $prefix;
        if ( bad_pattern($pattern) ) {
            Die(
"ERROR: the -sscp prefix '$prefix' causes the invalid regex '$pattern'\n"
            );
        }
        $static_side_comment_pattern = $pattern;
    }
    return;
}

sub make_closing_side_comment_prefix {

    # Be sure we have a valid closing side comment prefix
    my $csc_prefix = $rOpts->{'closing-side-comment-prefix'};
    my $csc_prefix_pattern;
    if ( !defined($csc_prefix) ) {
        $csc_prefix         = '## end';
        $csc_prefix_pattern = '^##\s+end';
    }
    else {
        my $test_csc_prefix = $csc_prefix;
        if ( $test_csc_prefix !~ /^#/ ) {
            $test_csc_prefix = '#' . $test_csc_prefix;
        }

        # make a regex to recognize the prefix
        my $test_csc_prefix_pattern = $test_csc_prefix;

        # escape any special characters
        $test_csc_prefix_pattern =~ s/([^#\s\w])/\\$1/g;

        $test_csc_prefix_pattern = '^' . $test_csc_prefix_pattern;

        # allow exact number of intermediate spaces to vary
        $test_csc_prefix_pattern =~ s/\s+/\\s\+/g;

        # make sure we have a good pattern
        # if we fail this we probably have an error in escaping
        # characters.

        if ( bad_pattern($test_csc_prefix_pattern) ) {

            # shouldn't happen..must have screwed up escaping, above
            report_definite_bug();
            Warn(
"Program Error: the -cscp prefix '$csc_prefix' caused the invalid regex '$csc_prefix_pattern'\n"
            );

            # just warn and keep going with defaults
            Warn("Please consider using a simpler -cscp prefix\n");
            Warn("Using default -cscp instead; please check output\n");
        }
        else {
            $csc_prefix         = $test_csc_prefix;
            $csc_prefix_pattern = $test_csc_prefix_pattern;
        }
    }
    $rOpts->{'closing-side-comment-prefix'} = $csc_prefix;
    $closing_side_comment_prefix_pattern = $csc_prefix_pattern;
    return;
}

##################################################
# CODE SECTION 4: receive lines from the tokenizer
##################################################

{    ## begin closure write_line

    my $Last_line_had_side_comment;
    my $In_format_skipping_section;
    my $Saw_VERSION_in_this_file;

    sub initialize_write_line {

        $Last_line_had_side_comment = 0;
        $In_format_skipping_section = 0;
        $Saw_VERSION_in_this_file   = 0;

        return;
    }

    sub write_line {

      # This routine originally received lines of code and immediately processed
      # them.  That was efficient when memory was limited, but now it just saves
      # the lines it receives.  They get processed all together after the last
      # line is received.

       # As tokenized lines are received they are converted to the format needed
       # for the final formatting.
        my ( $self, $line_of_tokens_old ) = @_;
        my $rLL           = $self->[_rLL_];
        my $Klimit        = $self->[_Klimit_];
        my $rlines_new    = $self->[_rlines_];
        my $maximum_level = $self->[_maximum_level_];

        my $Kfirst;
        my $line_of_tokens = {};
        foreach my $key (
            qw(
            _curly_brace_depth
            _ending_in_quote
            _guessed_indentation_level
            _line_number
            _line_text
            _line_type
            _paren_depth
            _quote_character
            _square_bracket_depth
            _starting_in_quote
            )
          )
        {
            $line_of_tokens->{$key} = $line_of_tokens_old->{$key};
        }

        # Data needed by Logger
        $line_of_tokens->{_level_0}          = 0;
        $line_of_tokens->{_ci_level_0}       = 0;
        $line_of_tokens->{_nesting_blocks_0} = "";
        $line_of_tokens->{_nesting_tokens_0} = "";

        # Needed to avoid trimming quotes
        $line_of_tokens->{_ended_in_blank_token} = undef;

        my $line_type     = $line_of_tokens_old->{_line_type};
        my $input_line_no = $line_of_tokens_old->{_line_number} - 1;
        my $CODE_type     = "";
        my $tee_output;

        # Handle line of non-code
        if ( $line_type ne 'CODE' ) {
            $tee_output ||= $rOpts_tee_pod
              && substr( $line_type, 0, 3 ) eq 'POD';
        }

        # Handle line of code
        else {

            my $rtokens         = $line_of_tokens_old->{_rtokens};
            my $rtoken_type     = $line_of_tokens_old->{_rtoken_type};
            my $rblock_type     = $line_of_tokens_old->{_rblock_type};
            my $rcontainer_type = $line_of_tokens_old->{_rcontainer_type};
            my $rcontainer_environment =
              $line_of_tokens_old->{_rcontainer_environment};
            my $rtype_sequence  = $line_of_tokens_old->{_rtype_sequence};
            my $rlevels         = $line_of_tokens_old->{_rlevels};
            my $rslevels        = $line_of_tokens_old->{_rslevels};
            my $rci_levels      = $line_of_tokens_old->{_rci_levels};
            my $rnesting_blocks = $line_of_tokens_old->{_rnesting_blocks};
            my $rnesting_tokens = $line_of_tokens_old->{_rnesting_tokens};

            my $jmax = @{$rtokens} - 1;
            if ( $jmax >= 0 ) {
                $Kfirst = defined($Klimit) ? $Klimit + 1 : 0;
                foreach my $j ( 0 .. $jmax ) {

                 # Clip negative nesting depths to zero to avoid problems.
                 # Negative values can occur in files with unbalanced containers
                    my $slevel = $rslevels->[$j];
                    if ( $slevel < 0 ) { $slevel = 0 }

                    if ( $rlevels->[$j] > $maximum_level ) {
                        $maximum_level = $rlevels->[$j];
                    }

                    # But do not clip the 'level' variable yet. We will do this
                    # later, in sub 'store_token_to_go'. The reason is that in
                    # files with level errors, the logic in 'weld_cuddled_else'
                    # uses a stack logic that will give bad welds if we clip
                    # levels here.
                    ## if ( $rlevels->[$j] < 0 ) { $rlevels->[$j] = 0 }

                    my @tokary;
                    @tokary[
                      _TOKEN_,         _TYPE_,
                      _BLOCK_TYPE_,    _CONTAINER_ENVIRONMENT_,
                      _TYPE_SEQUENCE_, _LEVEL_,
                      _LEVEL_TRUE_,    _SLEVEL_,
                      _CI_LEVEL_,      _LINE_INDEX_,
                      ]
                      = (
                        $rtokens->[$j],        $rtoken_type->[$j],
                        $rblock_type->[$j],    $rcontainer_environment->[$j],
                        $rtype_sequence->[$j], $rlevels->[$j],
                        $rlevels->[$j],        $slevel,
                        $rci_levels->[$j],     $input_line_no,
                      );
                    push @{$rLL}, \@tokary;
                } ## end foreach my $j ( 0 .. $jmax )

                $Klimit = @{$rLL} - 1;

                # Need to remember if we can trim the input line
                $line_of_tokens->{_ended_in_blank_token} =
                  $rtoken_type->[$jmax] eq 'b';

                $line_of_tokens->{_level_0}          = $rlevels->[0];
                $line_of_tokens->{_ci_level_0}       = $rci_levels->[0];
                $line_of_tokens->{_nesting_blocks_0} = $rnesting_blocks->[0];
                $line_of_tokens->{_nesting_tokens_0} = $rnesting_tokens->[0];
            } ## end if ( $jmax >= 0 )

            $CODE_type =
              $self->get_CODE_type( $line_of_tokens, $Kfirst, $Klimit );

            $tee_output ||=
                 $rOpts_tee_block_comments
              && $jmax == 0
              && $rLL->[$Kfirst]->[_TYPE_] eq '#';

            $tee_output ||=
                 $rOpts_tee_side_comments
              && defined($Kfirst)
              && $Klimit > $Kfirst
              && $rLL->[$Klimit]->[_TYPE_] eq '#';

            # Handle any requested side comment deletions. It is easier to get
            # this done here rather than farther down the pipeline because IO
            # lines take a different route, and because lines with deleted HSC
            # become BL lines.  An since we are deleting now, we have to also
            # handle any tee- requests before the side comments vanish.
            my $delete_side_comment =
                 $rOpts_delete_side_comments
              && defined($Kfirst)
              && $rLL->[$Klimit]->[_TYPE_] eq '#'
              && ( $Klimit > $Kfirst || $CODE_type eq 'HSC' )
              && ( !$CODE_type || $CODE_type eq 'HSC' || $CODE_type eq 'IO' );

            if (   $rOpts_delete_closing_side_comments
                && !$delete_side_comment
                && defined($Kfirst)
                && $Klimit > $Kfirst
                && $rLL->[$Klimit]->[_TYPE_] eq '#'
                && ( !$CODE_type || $CODE_type eq 'HSC' || $CODE_type eq 'IO' )
              )
            {
                my $token  = $rLL->[$Klimit]->[_TOKEN_];
                my $K_m    = $Klimit - 1;
                my $type_m = $rLL->[$K_m]->[_TYPE_];
                if ( $type_m eq 'b' && $K_m > $Kfirst ) { $K_m-- }
                my $last_nonblank_block_type = $rLL->[$K_m]->[_BLOCK_TYPE_];
                if (   $token =~ /$closing_side_comment_prefix_pattern/
                    && $last_nonblank_block_type =~
                    /$closing_side_comment_list_pattern/ )
                {
                    $delete_side_comment = 1;
                }
            } ## end if ( $rOpts_delete_closing_side_comments...)

            if ($delete_side_comment) {
                pop @{$rLL};
                $Klimit -= 1;
                if (   $Klimit > $Kfirst
                    && $rLL->[$Klimit]->[_TYPE_] eq 'b' )
                {
                    pop @{$rLL};
                    $Klimit -= 1;
                }

                # The -io option outputs the line text, so we have to update
                # the line text so that the comment does not reappear.
                if ( $CODE_type eq 'IO' ) {
                    my $line = "";
                    foreach my $KK ( $Kfirst .. $Klimit ) {
                        $line .= $rLL->[$KK]->[_TOKEN_];
                    }
                    $line_of_tokens->{_line_text} = $line . "\n";
                }

                # If we delete a hanging side comment the line becomes blank.
                if ( $CODE_type eq 'HSC' ) { $CODE_type = 'BL' }
            }

        } ## end if ( $line_type eq 'CODE')

        # Finish storing line variables
        if ($tee_output) {
            my $fh_tee    = $self->[_fh_tee_];
            my $line_text = $line_of_tokens_old->{_line_text};
            $fh_tee->print($line_text) if ($fh_tee);
        }

        $line_of_tokens->{_rK_range}  = [ $Kfirst, $Klimit ];
        $line_of_tokens->{_code_type} = $CODE_type;
        $self->[_Klimit_]             = $Klimit;
        $self->[_maximum_level_]      = $maximum_level;

        push @{$rlines_new}, $line_of_tokens;
        return;
    }

    sub get_CODE_type {
        my ( $self, $line_of_tokens, $Kfirst, $Klast ) = @_;

        # We are looking at a line of code and setting a flag to
        # describe any special processing that it requires

        # Possible CODE_types
        # 'VB'  = Verbatim - line goes out verbatim (a quote)
        # 'FS'  = Format Skipping - line goes out verbatim
        # 'BL'  = Blank Line
        # 'HSC' = Hanging Side Comment - fix this hanging side comment
        # 'SBCX'= Static Block Comment Without Leading Space
        # 'SBC' = Static Block Comment
        # 'BC'  = Block Comment - an ordinary full line comment
        # 'IO'  = Indent Only - line goes out unchanged except for indentation
        # 'NIN' = No Internal Newlines - line does not get broken
        # 'VER' = VERSION statement
        # ''    = ordinary line of code with no restructions

        my $rLL = $self->[_rLL_];

        my $CODE_type  = "";
        my $input_line = $line_of_tokens->{_line_text};
        my $jmax       = defined($Kfirst) ? $Klast - $Kfirst : -1;

        my $is_block_comment = 0;
        my $has_side_comment = 0;

        if ( $jmax >= 0 && $rLL->[$Klast]->[_TYPE_] eq '#' ) {
            if   ( $jmax == 0 ) { $is_block_comment = 1; }
            else                { $has_side_comment = 1 }
        }

        # Write line verbatim if we are in a formatting skip section
        if ($In_format_skipping_section) {

            # Note: extra space appended to comment simplifies pattern matching
            if ( $is_block_comment
                && ( $rLL->[$Kfirst]->[_TOKEN_] . " " ) =~
                /$format_skipping_pattern_end/ )
            {
                $In_format_skipping_section = 0;
                write_logfile_entry("Exiting formatting skip section\n");
            }
            $CODE_type = 'FS';
            goto RETURN;
        }

        # Check for a continued quote..
        if ( $line_of_tokens->{_starting_in_quote} ) {

            # A line which is entirely a quote or pattern must go out
            # verbatim.  Note: the \n is contained in $input_line.
            if ( $jmax <= 0 ) {
                if ( ( $input_line =~ "\t" ) ) {
                    my $input_line_number = $line_of_tokens->{_line_number};
                    $self->note_embedded_tab($input_line_number);
                }
                $CODE_type = 'VB';
                goto RETURN;
            }
        }

        # See if we are entering a formatting skip section
        if (   $rOpts_format_skipping
            && $is_block_comment
            && ( $rLL->[$Kfirst]->[_TOKEN_] . " " ) =~
            /$format_skipping_pattern_begin/ )
        {
            $In_format_skipping_section = 1;
            write_logfile_entry("Entering formatting skip section\n");
            $CODE_type = 'FS';
            goto RETURN;
        }

        # ignore trailing blank tokens (they will get deleted later)
        if ( $jmax > 0 && $rLL->[$Klast]->[_TYPE_] eq 'b' ) {
            $jmax--;
        }

        # blank line..
        if ( $jmax < 0 ) {
            $CODE_type = 'BL';
            goto RETURN;
        }

        # see if this is a static block comment (starts with ## by default)
        my $is_static_block_comment                       = 0;
        my $is_static_block_comment_without_leading_space = 0;
        if (   $is_block_comment
            && $rOpts->{'static-block-comments'}
            && $input_line =~ /$static_block_comment_pattern/ )
        {
            $is_static_block_comment = 1;
            $is_static_block_comment_without_leading_space =
              substr( $input_line, 0, 1 ) eq '#';
        }

        # Check for comments which are line directives
        # Treat exactly as static block comments without leading space
        # reference: perlsyn, near end, section Plain Old Comments (Not!)
        # example: '# line 42 "new_filename.plx"'
        if (
               $is_block_comment
            && $input_line =~ /^\#   \s*
                               line \s+ (\d+)   \s*
                               (?:\s("?)([^"]+)\2)? \s*
                               $/x
          )
        {
            $is_static_block_comment                       = 1;
            $is_static_block_comment_without_leading_space = 1;
        }

        # look for hanging side comment
        if (
               $is_block_comment
            && $Last_line_had_side_comment  # last line had side comment
            && $input_line =~ /^\s/         # there is some leading space
            && !$is_static_block_comment    # do not make static comment hanging
            && $rOpts->{'hanging-side-comments'}    # user is allowing
                                                    # hanging side comments
                                                    # like this
          )
        {
            $has_side_comment = 1;
            $CODE_type        = 'HSC';
            goto RETURN;
        }

        # Handle a block (full-line) comment..
        if ($is_block_comment) {

            if ($is_static_block_comment_without_leading_space) {
                $CODE_type = 'SBCX';
                goto RETURN;
            }
            elsif ($is_static_block_comment) {
                $CODE_type = 'SBC';
                goto RETURN;
            }
            elsif ($Last_line_had_side_comment
                && !$rOpts_maximum_consecutive_blank_lines
                && $rLL->[$Kfirst]->[_LEVEL_] > 0 )
            {
                # Emergency fix to keep a block comment from becoming a hanging
                # side comment.  This fix is for the case that blank lines
                # cannot be inserted.  There is related code in sub
                # 'process_line_of_CODE'
                $CODE_type = 'SBCX';
                goto RETURN;
            }
            else {
                $CODE_type = 'BC';
                goto RETURN;
            }
        }

        # End of comments. Handle a line of normal code:

        if ($rOpts_indent_only) {
            $CODE_type = 'IO';
            goto RETURN;
        }

        if ( !$rOpts_add_newlines ) {
            $CODE_type = 'NIN';
            goto RETURN;
        }

        #   Patch needed for MakeMaker.  Do not break a statement
        #   in which $VERSION may be calculated.  See MakeMaker.pm;
        #   this is based on the coding in it.
        #   The first line of a file that matches this will be eval'd:
        #       /([\$*])(([\w\:\']*)\bVERSION)\b.*\=/
        #   Examples:
        #     *VERSION = \'1.01';
        #     ( $VERSION ) = '$Revision: 1.74 $ ' =~ /\$Revision:\s+([^\s]+)/;
        #   We will pass such a line straight through without breaking
        #   it unless -npvl is used.

        #   Patch for problem reported in RT #81866, where files
        #   had been flattened into a single line and couldn't be
        #   tidied without -npvl.  There are two parts to this patch:
        #   First, it is not done for a really long line (80 tokens for now).
        #   Second, we will only allow up to one semicolon
        #   before the VERSION.  We need to allow at least one semicolon
        #   for statements like this:
        #      require Exporter;  our $VERSION = $Exporter::VERSION;
        #   where both statements must be on a single line for MakeMaker

        my $is_VERSION_statement = 0;
        if (  !$Saw_VERSION_in_this_file
            && $jmax < 80
            && $input_line =~
            /^[^;]*;?[^;]*([\$*])(([\w\:\']*)\bVERSION)\b.*\=/ )
        {
            $Saw_VERSION_in_this_file = 1;
            write_logfile_entry("passing VERSION line; -npvl deactivates\n");

            # This code type has lower priority than others
            $CODE_type = 'VER';
            goto RETURN;
        }

      RETURN:
        $Last_line_had_side_comment = $has_side_comment;
        return $CODE_type;
    }

} ## end closure write_line

#############################################
# CODE SECTION 5: Pre-process the entire file
#############################################

sub finish_formatting {

    my ( $self, $severe_error ) = @_;

    # The file has been tokenized and is ready to be formatted.
    # All of the relevant data is stored in $self, ready to go.

    # Check the maximum level. If it is extremely large we will
    # give up and output the file verbatim.
    my $maximum_level       = $self->[_maximum_level_];
    my $maximum_table_index = $#maximum_line_length;
    if ( !$severe_error && $maximum_level > $maximum_table_index ) {
        $severe_error ||= 1;
        Warn(<<EOM);
The maximum indentation level, $maximum_level, exceeds the builtin limit of $maximum_table_index.
Something may be wrong; formatting will be skipped. 
EOM
    }

    # output file verbatim if severe error or no formatting requested
    if ( $severe_error || $rOpts->{notidy} ) {
        $self->dump_verbatim();
        $self->wrapup();
        return;
    }

    # Update the 'save_logfile' flag based to include any tokenization errors.
    # We can save time by skipping logfile calls if it is not going to be saved.
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $self->[_save_logfile_] = $logger_object->get_save_logfile();
    }

    # Make a pass through all tokens, adding or deleting any whitespace as
    # required.  Also make any other changes, such as adding semicolons.
    # All token changes must be made here so that the token data structure
    # remains fixed for the rest of this iteration.
    $self->respace_tokens();

    $self->find_multiline_qw();

    $self->keep_old_line_breaks();

    # Implement any welding needed for the -wn or -cb options
    $self->weld_containers();

    # Locate small nested blocks which should not be broken
    $self->mark_short_nested_blocks();

    $self->adjust_indentation_levels();

    # Finishes formatting and write the result to the line sink.
    # Eventually this call should just change the 'rlines' data according to the
    # new line breaks and then return so that we can do an internal iteration
    # before continuing with the next stages of formatting.
    $self->process_all_lines();

    # A final routine to tie up any loose ends
    $self->wrapup();
    return;
}

sub dump_verbatim {
    my $self   = shift;
    my $rlines = $self->[_rlines_];
    foreach my $line ( @{$rlines} ) {
        my $input_line = $line->{_line_text};
        $self->write_unindented_line($input_line);
    }
    return;
}

sub respace_tokens {

    my $self = shift;
    return if $rOpts->{'indent-only'};

    # This routine is called once per file to do as much formatting as possible
    # before new line breaks are set.

    # This routine makes all necessary and possible changes to the tokenization
    # after the initial tokenization of the file. This is a tedious routine,
    # but basically it consists of inserting and deleting whitespace between
    # nonblank tokens according to the selected parameters. In a few cases
    # non-space characters are added, deleted or modified.

    # The goal of this routine is to create a new token array which only needs
    # the definition of new line breaks and padding to complete formatting.  In
    # a few cases we have to cheat a little to achieve this goal.  In
    # particular, we may not know if a semicolon will be needed, because it
    # depends on how the line breaks go.  To handle this, we include the
    # semicolon as a 'phantom' which can be displayed as normal or as an empty
    # string.

    # Method: The old tokens are copied one-by-one, with changes, from the old
    # linear storage array $rLL to a new array $rLL_new.

    my $rLL             = $self->[_rLL_];
    my $Klimit_old      = $self->[_Klimit_];
    my $rlines          = $self->[_rlines_];
    my $length_function = $self->[_length_function_];
    my $is_encoded_data = $self->[_is_encoded_data_];

    my $rLL_new = [];    # This is the new array
    my $KK      = 0;
    my $rtoken_vars;
    my $Kmax = @{$rLL} - 1;

    my $CODE_type = "";
    my $line_type = "";

    my $rOpts_add_whitespace        = $rOpts->{'add-whitespace'};
    my $rOpts_delete_old_whitespace = $rOpts->{'delete-old-whitespace'};
    my $rOpts_ignore_side_comment_lengths =
      $rOpts->{'ignore-side-comment-lengths'};

    # Set the whitespace flags, which indicate the token spacing preference.
    my $rwhitespace_flags = $self->set_whitespace_flags();

    # we will be setting token lengths as we go
    my $cumulative_length = 0;

    # We also define these hash indexes giving container token array indexes
    # as a function of the container sequence numbers.  For example,
    my $K_opening_container = {};    # opening [ { or (
    my $K_closing_container = {};    # closing ] } or )
    my $K_opening_ternary   = {};    # opening ? of ternary
    my $K_closing_ternary   = {};    # closing : of ternary

    # List of new K indexes of phantom semicolons
    # This will be needed if we want to undo them for iterations
    my $rK_phantom_semicolons = [];

    my %seqno_stack;
    my %KK_stack;                      # Note: old K index
    my %K_opening_by_seqno    = ();    # Note: old K index
    my $depth_next            = 0;
    my $depth_next_max        = 0;
    my $rtype_count_by_seqno  = {};
    my $ris_broken_container  = {};
    my $rhas_broken_container = {};
    my $rparent_of_seqno      = {};
    my $rchildren_of_seqno    = {};

    my $last_nonblank_type       = ';';
    my $last_nonblank_token      = ';';
    my $last_nonblank_block_type = '';
    my $nonblank_token_count     = 0;
    my $store_token              = sub {
        my ($item) = @_;

        # This will be the index of this item in the new array
        my $KK_new = @{$rLL_new};

        my $type     = $item->[_TYPE_];
        my $is_blank = $type eq 'b';

        # Do not output consecutive blanks. This should not happen, but
        # is worth checking because later routines make this assumption.
        if ( $is_blank && $KK_new && $rLL_new->[-1]->[_TYPE_] eq 'b' ) {
            return;
        }

        # check for a sequenced item (i.e., container or ?/:)
        my $type_sequence = $item->[_TYPE_SEQUENCE_];
        if ($type_sequence) {

            my $token = $item->[_TOKEN_];
            if ( $is_opening_token{$token} ) {

                $K_opening_container->{$type_sequence} = $KK_new;
            }
            elsif ( $is_closing_token{$token} ) {

                $K_closing_container->{$type_sequence} = $KK_new;
            }

            # These are not yet used but could be useful
            else {
                if ( $token eq '?' ) {
                    $K_opening_ternary->{$type_sequence} = $KK_new;
                }
                elsif ( $token eq ':' ) {
                    $K_closing_ternary->{$type_sequence} = $KK_new;
                }
                else {

                    # We really shouldn't arrive here, just being cautious:
                    # The only sequenced types output by the tokenizer are the
                    # opening & closing containers and the ternary types. Each
                    # of those was checked above. So we would only get here
                    # if the tokenizer has been changed to mark some other
                    # tokens with sequence numbers.
                    my $type = $item->[_TYPE_];
                    Fault(
"Unexpected token type with sequence number: type='$type', seqno='$type_sequence'"
                    );
                }
            }
        }

        # Find the length of this token.  Later it may be adjusted if phantom
        # or ignoring side comment lengths.
        my $token_length =
            $is_encoded_data
          ? $length_function->( $item->[_TOKEN_] )
          : length( $item->[_TOKEN_] );

        # handle comments
        my $is_comment = $type eq '#';
        if ($is_comment) {

            # trim comments if necessary
            if ( $item->[_TOKEN_] =~ s/\s+$// ) {
                $token_length = $length_function->( $item->[_TOKEN_] );
            }

           # Mark length of side comments as just 1 if their lengths are ignored
            if ( $rOpts_ignore_side_comment_lengths
                && ( !$CODE_type || $CODE_type eq 'HSC' ) )
            {
                $token_length = 1;
            }
        }

        $item->[_TOKEN_LENGTH_] = $token_length;

        # and update the cumulative length
        $cumulative_length += $token_length;

        # Save the length sum to just AFTER this token
        $item->[_CUMULATIVE_LENGTH_] = $cumulative_length;

        if ( !$is_blank && !$is_comment ) {
            $last_nonblank_type       = $type;
            $last_nonblank_token      = $item->[_TOKEN_];
            $last_nonblank_block_type = $item->[_BLOCK_TYPE_];
            $nonblank_token_count++;

            # count selected types
            if ( $is_counted_type{$type} ) {
                my $seqno = $seqno_stack{ $depth_next - 1 };
                if ( defined($seqno) ) {
                    $rtype_count_by_seqno->{$seqno}->{$type}++;
                }
            }
        }

        # For reference, here is how to get the parent sequence number.
        # This is not used because it is slower than finding it on the fly
        # in sub parent_seqno_by_K:

        # my $seqno_parent =
        #     $type_sequence && $is_opening_token{$token}
        #   ? $seqno_stack{ $depth_next - 2 }
        #   : $seqno_stack{ $depth_next - 1 };
        # my $KK = @{$rLL_new};
        # $rseqno_of_parent_by_K->{$KK} = $seqno_parent;

        # and finally, add this item to the new array
        push @{$rLL_new}, $item;
    };

    my $store_token_and_space = sub {
        my ( $item, $want_space ) = @_;

        # store a token with preceding space if requested and needed

        # First store the space
        if (   $want_space
            && @{$rLL_new}
            && $rLL_new->[-1]->[_TYPE_] ne 'b'
            && $rOpts_add_whitespace )
        {
            my $rcopy = copy_token_as_type( $item, 'b', ' ' );
            $rcopy->[_LINE_INDEX_] =
              $rLL_new->[-1]->[_LINE_INDEX_];
            $store_token->($rcopy);
        }

        # then the token
        $store_token->($item);
    };

    my $K_end_q = sub {
        my ($KK) = @_;
        my $K_end = $KK;

        my $Kn = $KK + 1;
        if ( $Kn <= $Kmax && $rLL->[$Kn]->[_TYPE_] eq 'b' ) { $Kn += 1 }

        while ( $Kn <= $Kmax && $rLL->[$Kn]->[_TYPE_] eq 'q' ) {
            $K_end = $Kn;

            $Kn += 1;
            if ( $Kn <= $Kmax && $rLL->[$Kn]->[_TYPE_] eq 'b' ) { $Kn += 1 }
        }

        return $K_end;
    };

    my $add_phantom_semicolon = sub {

        my ($KK) = @_;

        my $Kp = $self->K_previous_nonblank( undef, $rLL_new );
        return unless ( defined($Kp) );

        # we are only adding semicolons for certain block types
        my $block_type = $rLL->[$KK]->[_BLOCK_TYPE_];
        return
          unless ( $ok_to_add_semicolon_for_block_type{$block_type}
            || $block_type =~ /^(sub|package)/
            || $block_type =~ /^\w+\:$/ );

        my $type_sequence = $rLL->[$KK]->[_TYPE_SEQUENCE_];

        my $previous_nonblank_type  = $rLL_new->[$Kp]->[_TYPE_];
        my $previous_nonblank_token = $rLL_new->[$Kp]->[_TOKEN_];

        # Do not add a semicolon if...
        return
          if (

            # it would follow a comment (and be isolated)
            $previous_nonblank_type eq '#'

            # it follows a code block ( because they are not always wanted
            # there and may add clutter)
            || $rLL_new->[$Kp]->[_BLOCK_TYPE_]

            # it would follow a label
            || $previous_nonblank_type eq 'J'

            # it would be inside a 'format' statement (and cause syntax error)
            || (   $previous_nonblank_type eq 'k'
                && $previous_nonblank_token =~ /format/ )

          );

        # Do not add a semicolon if it would impede a weld with an immediately
        # following closing token...like this
        #   { ( some code ) }
        #                  ^--No semicolon can go here

        # look at the previous token... note use of the _NEW rLL array here,
        # but sequence numbers are invariant.
        my $seqno_inner = $rLL_new->[$Kp]->[_TYPE_SEQUENCE_];

        # If it is also a CLOSING token we have to look closer...
        if (
               $seqno_inner
            && $is_closing_token{$previous_nonblank_token}

            # we only need to look if there is just one inner container..
            && defined( $rchildren_of_seqno->{$type_sequence} )
            && @{ $rchildren_of_seqno->{$type_sequence} } == 1
          )
        {

            # Go back and see if the corresponding two OPENING tokens are also
            # together.  Note that we are using the OLD K indexing here:
            my $K_outer_opening = $K_opening_by_seqno{$type_sequence};
            if ( defined($K_outer_opening) ) {
                my $K_nxt = $self->K_next_nonblank($K_outer_opening);
                if ( defined($K_nxt) ) {
                    my $seqno_nxt = $rLL->[$K_nxt]->[_TYPE_SEQUENCE_];

                    # Is the next token after the outer opening the same as
                    # our inner closing (i.e. same sequence number)?
                    # If so, do not insert a semicolon here.
                    return if ( $seqno_nxt && $seqno_nxt == $seqno_inner );
                }
            }
        }

        # We will insert an empty semicolon here as a placeholder.  Later, if
        # it becomes the last token on a line, we will bring it to life.  The
        # advantage of doing this is that (1) we just have to check line
        # endings, and (2) the phantom semicolon has zero width and therefore
        # won't cause needless breaks of one-line blocks.
        my $Ktop = -1;
        if (   $rLL_new->[$Ktop]->[_TYPE_] eq 'b'
            && $want_left_space{';'} == WS_NO )
        {

            # convert the blank into a semicolon..
            # be careful: we are working on the new stack top
            # on a token which has been stored.
            my $rcopy = copy_token_as_type( $rLL_new->[$Ktop], 'b', ' ' );

            # Convert the existing blank to:
            #   a phantom semicolon for one_line_block option = 0 or 1
            #   a real semicolon    for one_line_block option = 2
            my $tok     = '';
            my $len_tok = 0;
            if ( $rOpts_one_line_block_semicolons == 2 ) {
                $tok     = ';';
                $len_tok = 1;
            }

            $rLL_new->[$Ktop]->[_TOKEN_]        = $tok;
            $rLL_new->[$Ktop]->[_TOKEN_LENGTH_] = $len_tok;
            $rLL_new->[$Ktop]->[_TYPE_]         = ';';
            $rLL_new->[$Ktop]->[_SLEVEL_] =
              $rLL->[$KK]->[_SLEVEL_];

            push @{$rK_phantom_semicolons}, @{$rLL_new} - 1;

            # Then store a new blank
            $store_token->($rcopy);
        }
        else {

            # insert a new token
            my $rcopy = copy_token_as_type( $rLL_new->[$Kp], ';', '' );
            $rcopy->[_SLEVEL_] = $rLL->[$KK]->[_SLEVEL_];
            $store_token->($rcopy);
            push @{$rK_phantom_semicolons}, @{$rLL_new} - 1;
        }
    };

    my $check_Q = sub {

        # Check that a quote looks okay
        # This sub works but needs to by sync'd with the log file output
        # before it can be used.
        my ( $KK, $Kfirst, $line_number ) = @_;
        my $token = $rLL->[$KK]->[_TOKEN_];
        $self->note_embedded_tab($line_number) if ( $token =~ "\t" );

        my $Kp = $self->K_previous_nonblank( undef, $rLL_new );
        return unless ( defined($Kp) );
        my $previous_nonblank_type  = $rLL_new->[$Kp]->[_TYPE_];
        my $previous_nonblank_token = $rLL_new->[$Kp]->[_TOKEN_];

        my $previous_nonblank_type_2  = 'b';
        my $previous_nonblank_token_2 = "";
        my $Kpp = $self->K_previous_nonblank( $Kp, $rLL_new );
        if ( defined($Kpp) ) {
            $previous_nonblank_type_2  = $rLL_new->[$Kpp]->[_TYPE_];
            $previous_nonblank_token_2 = $rLL_new->[$Kpp]->[_TOKEN_];
        }

        my $next_nonblank_token = "";
        my $Kn                  = $KK + 1;
        if ( $Kn <= $Kmax && $rLL->[$Kn]->[_TYPE_] eq 'b' ) { $Kn += 1 }
        if ( $Kn <= $Kmax ) {
            $next_nonblank_token = $rLL->[$Kn]->[_TOKEN_];
        }

        my $token_0 = $rLL->[$Kfirst]->[_TOKEN_];
        my $type_0  = $rLL->[$Kfirst]->[_TYPE_];

        # make note of something like '$var = s/xxx/yyy/;'
        # in case it should have been '$var =~ s/xxx/yyy/;'
        if (
               $token =~ /^(s|tr|y|m|\/)/
            && $previous_nonblank_token =~ /^(=|==|!=)$/

            # preceded by simple scalar
            && $previous_nonblank_type_2 eq 'i'
            && $previous_nonblank_token_2 =~ /^\$/

            # followed by some kind of termination
            # (but give complaint if we can not see far enough ahead)
            && $next_nonblank_token =~ /^[; \)\}]$/

            # scalar is not declared
            && !( $type_0 eq 'k' && $token_0 =~ /^(my|our|local)$/ )
          )
        {
            my $guess = substr( $last_nonblank_token, 0, 1 ) . '~';
            complain(
"Note: be sure you want '$previous_nonblank_token' instead of '$guess' here\n"
            );
        }
    };

    # Main loop over all lines of the file
    my $last_K_out;

    # Testing option to break qw.  Do not use; it can make a mess.
    my $ALLOW_BREAK_MULTILINE_QW = 0;
    my $in_multiline_qw;
    foreach my $line_of_tokens ( @{$rlines} ) {

        my $input_line_number = $line_of_tokens->{_line_number};
        my $last_line_type    = $line_type;
        $line_type = $line_of_tokens->{_line_type};
        next unless ( $line_type eq 'CODE' );
        my $last_CODE_type = $CODE_type;
        $CODE_type = $line_of_tokens->{_code_type};
        my $rK_range = $line_of_tokens->{_rK_range};
        my ( $Kfirst, $Klast ) = @{$rK_range};
        next unless defined($Kfirst);

        # Check for correct sequence of token indexes...
        # An error here means that sub write_line() did not correctly
        # package the tokenized lines as it received them.  If we
        # get a fault here it has not output a continuous sequence
        # of K values.  Or a line of CODE may have been mismarked as
        # something else.
        if ( defined($last_K_out) ) {
            if ( $Kfirst != $last_K_out + 1 ) {
                Fault(
                    "Program Bug: last K out was $last_K_out but Kfirst=$Kfirst"
                );
            }
        }
        else {

            # The first token should always have been given index 0 by sub
            # write_line()
            if ( $Kfirst != 0 ) {
                Fault("Program Bug: first K is $Kfirst but should be 0");
            }
        }
        $last_K_out = $Klast;

        # Handle special lines of code
        if ( $CODE_type && $CODE_type ne 'NIN' && $CODE_type ne 'VER' ) {

            # CODE_types are as follows.
            # 'BL' = Blank Line
            # 'VB' = Verbatim - line goes out verbatim
            # 'FS' = Format Skipping - line goes out verbatim, no blanks
            # 'IO' = Indent Only - only indentation may be changed
            # 'NIN' = No Internal Newlines - line does not get broken
            # 'HSC'=Hanging Side Comment - fix this hanging side comment
            # 'BC'=Block Comment - an ordinary full line comment
            # 'SBC'=Static Block Comment - a block comment which does not get
            #      indented
            # 'SBCX'=Static Block Comment Without Leading Space
            # 'VER'=VERSION statement
            # '' or (undefined) - no restructions

            # For a hanging side comment we insert an empty quote before
            # the comment so that it becomes a normal side comment and
            # will be aligned by the vertical aligner
            if ( $CODE_type eq 'HSC' ) {

                # Safety Check: This must be a line with one token (a comment)
                my $rtoken_vars = $rLL->[$Kfirst];
                if ( $Kfirst == $Klast && $rtoken_vars->[_TYPE_] eq '#' ) {

                    # Note that even if the flag 'noadd-whitespace' is set, we
                    # will make an exception here and allow a blank to be
                    # inserted to push the comment to the right.  We can think
                    # of this as an adjustment of indentation rather than
                    # whitespace between tokens. This will also prevent the
                    # hanging side comment from getting converted to a block
                    # comment if whitespace gets deleted, as for example with
                    # the -extrude and -mangle options.
                    my $rcopy = copy_token_as_type( $rtoken_vars, 'q', '' );
                    $store_token->($rcopy);
                    $rcopy = copy_token_as_type( $rtoken_vars, 'b', ' ' );
                    $store_token->($rcopy);
                    $store_token->($rtoken_vars);
                    next;
                }
                else {

                    # This line was mis-marked by sub scan_comment
                    Fault(
                        "Program bug. A hanging side comment has been mismarked"
                    );
                }
            }

            # Copy tokens unchanged
            foreach my $KK ( $Kfirst .. $Klast ) {
                $store_token->( $rLL->[$KK] );
            }
            next;
        }

        # Handle normal line..

        # Insert any essential whitespace between lines
        # if last line was normal CODE.
        # Patch for rt #125012: use K_previous_code rather than '_nonblank'
        # because comments may disappear.
        my $type_next  = $rLL->[$Kfirst]->[_TYPE_];
        my $token_next = $rLL->[$Kfirst]->[_TOKEN_];
        my $Kp         = $self->K_previous_code( undef, $rLL_new );
        if (   $last_line_type eq 'CODE'
            && $type_next ne 'b'
            && defined($Kp) )
        {
            my $token_p = $rLL_new->[$Kp]->[_TOKEN_];
            my $type_p  = $rLL_new->[$Kp]->[_TYPE_];

            my ( $token_pp, $type_pp );
            my $Kpp = $self->K_previous_code( $Kp, $rLL_new );
            if ( defined($Kpp) ) {
                $token_pp = $rLL_new->[$Kpp]->[_TOKEN_];
                $type_pp  = $rLL_new->[$Kpp]->[_TYPE_];
            }
            else {
                $token_pp = ";";
                $type_pp  = ';';
            }

            if (

                is_essential_whitespace(
                    $token_pp, $type_pp,    $token_p,
                    $type_p,   $token_next, $type_next,
                )
              )
            {

                # Copy this first token as blank, but use previous line number
                my $rcopy = copy_token_as_type( $rLL->[$Kfirst], 'b', ' ' );
                $rcopy->[_LINE_INDEX_] =
                  $rLL_new->[-1]->[_LINE_INDEX_];
                $store_token->($rcopy);
            }
        }

        # loop to copy all tokens on this line, with any changes
        my $type_sequence;
        for ( my $KK = $Kfirst ; $KK <= $Klast ; $KK++ ) {
            $rtoken_vars = $rLL->[$KK];
            my $token              = $rtoken_vars->[_TOKEN_];
            my $type               = $rtoken_vars->[_TYPE_];
            my $last_type_sequence = $type_sequence;
            $type_sequence = $rtoken_vars->[_TYPE_SEQUENCE_];

            # Handle a blank space ...
            if ( $type eq 'b' ) {

                # Delete it if not wanted by whitespace rules
                # or we are deleting all whitespace
                # Note that whitespace flag is a flag indicating whether a
                # white space BEFORE the token is needed
                next if ( $KK >= $Klast );    # skip terminal blank
                my $Knext = $KK + 1;
                my $ws    = $rwhitespace_flags->[$Knext];
                if (   $ws == -1
                    || $rOpts_delete_old_whitespace )
                {

                    # FIXME: maybe switch to using _new
                    my $Kp = $self->K_previous_nonblank($KK);
                    next unless defined($Kp);
                    my $token_p = $rLL->[$Kp]->[_TOKEN_];
                    my $type_p  = $rLL->[$Kp]->[_TYPE_];

                    my ( $token_pp, $type_pp );

                    my $Kpp = $self->K_previous_nonblank($Kp);
                    if ( defined($Kpp) ) {
                        $token_pp = $rLL->[$Kpp]->[_TOKEN_];
                        $type_pp  = $rLL->[$Kpp]->[_TYPE_];
                    }
                    else {
                        $token_pp = ";";
                        $type_pp  = ';';
                    }
                    my $token_next = $rLL->[$Knext]->[_TOKEN_];
                    my $type_next  = $rLL->[$Knext]->[_TYPE_];

                    my $do_not_delete = is_essential_whitespace(
                        $token_pp, $type_pp,    $token_p,
                        $type_p,   $token_next, $type_next,
                    );

                    next unless ($do_not_delete);
                }

                # make it just one character if allowed
                if ($rOpts_add_whitespace) {
                    $rtoken_vars->[_TOKEN_] = ' ';
                }
                $store_token->($rtoken_vars);
                next;
            }

            # Handle a nonblank token...

            if ($type_sequence) {

                if ( $is_opening_token{$token} ) {
                    my $seqno_parent = $seqno_stack{ $depth_next - 1 };
                    $seqno_parent = SEQ_ROOT unless defined($seqno_parent);
                    push @{ $rchildren_of_seqno->{$seqno_parent} },
                      $type_sequence;
                    $rparent_of_seqno->{$type_sequence} = $seqno_parent;
                    $seqno_stack{$depth_next}           = $type_sequence;
                    $KK_stack{$depth_next}              = $KK;
                    $K_opening_by_seqno{$type_sequence} = $KK;
                    $depth_next++;

                    if ( $depth_next > $depth_next_max ) {
                        $depth_next_max = $depth_next;
                    }
                }
                elsif ( $is_closing_token{$token} ) {

                    # Insert a tentative missing semicolon if the next token is
                    # a closing block brace
                    if (
                           $type eq '}'
                        && $token eq '}'

                        # not preceded by a ';'
                        && $last_nonblank_type ne ';'

                        # and this is not a VERSION stmt (is all one line, we
                        # are not inserting semicolons on one-line blocks)
                        && $CODE_type ne 'VER'

                        # and we are allowed to add semicolons
                        && $rOpts->{'add-semicolons'}
                      )
                    {
                        $add_phantom_semicolon->($KK);
                    }

                    # Update the stack...  Note that we do this after adding
                    # any phantom semicolons so that they will be counted in
                    # the correct container.
                    $depth_next--;

                    # keep track of broken lists for later formatting
                    my $seqno_test  = $seqno_stack{$depth_next};
                    my $KK_open     = $KK_stack{$depth_next};
                    my $seqno_outer = $seqno_stack{ $depth_next - 1 };
                    if (   defined($seqno_test)
                        && defined($KK_open)
                        && $seqno_test == $type_sequence )
                    {
                        my $lx_open  = $rLL->[$KK_open]->[_LINE_INDEX_];
                        my $lx_close = $rLL->[$KK]->[_LINE_INDEX_];
                        if ( $lx_open < $lx_close ) {
                            $ris_broken_container->{$type_sequence} =
                              $lx_close - $lx_open;
                            if ( defined($seqno_outer) ) {
                                $rhas_broken_container->{$seqno_outer} = 1;
                            }
                        }
                    }
                }
            }

            # Modify certain tokens here for whitespace
            # The following is not yet done, but could be:
            #   sub (x x x)
            elsif ( $type =~ /^[wit]$/ ) {

                # Examples: <<snippets/space1.in>>
                # change '$  var'  to '$var' etc
                # change '@    '   to '@'
                my ( $sigil, $word ) = split /\s+/, $token, 2;
                if ( length($sigil) == 1
                    && $sigil =~ /^[\$\&\%\*\@]$/ )
                {
                    $token = $sigil;
                    $token .= $word if ($word);
                    $rtoken_vars->[_TOKEN_] = $token;
                }

                # Split identifiers with leading arrows, inserting blanks if
                # necessary.  It is easier and safer here than in the
                # tokenizer.  For example '->new' becomes two tokens, '->' and
                # 'new' with a possible blank between.
                #
                # Note: there is a related patch in sub set_whitespace_flags
                if (   substr( $token, 0, 1 ) eq '-'
                    && $token =~ /^\-\>(.*)$/
                    && $1 )
                {

                    my $token_save = $1;
                    my $type_save  = $type;

                    # Change '-> new'  to '->new'
                    $token_save =~ s/^\s+//g;

                    # store a blank to left of arrow if necessary
                    my $Kprev = $self->K_previous_nonblank($KK);
                    if (   defined($Kprev)
                        && $rLL->[$Kprev]->[_TYPE_] ne 'b'
                        && $rOpts_add_whitespace
                        && $want_left_space{'->'} == WS_YES )
                    {
                        my $rcopy =
                          copy_token_as_type( $rtoken_vars, 'b', ' ' );
                        $store_token->($rcopy);
                    }

                    # then store the arrow
                    my $rcopy = copy_token_as_type( $rtoken_vars, '->', '->' );
                    $store_token->($rcopy);

                    # store a blank after the arrow if requested
                    # added for issue git #33
                    if ( $want_right_space{'->'} == WS_YES ) {
                        my $rcopy =
                          copy_token_as_type( $rtoken_vars, 'b', ' ' );
                        $store_token->($rcopy);
                    }

                    # then reset the current token to be the remainder,
                    # and reset the whitespace flag according to the arrow
                    $token = $rtoken_vars->[_TOKEN_] = $token_save;
                    $type  = $rtoken_vars->[_TYPE_]  = $type_save;
                    $store_token->($rtoken_vars);
                    next;
                }

                if ( $token =~ /$ANYSUB_PATTERN/ ) {

                    # -spp = 0 : no space before opening prototype paren
                    # -spp = 1 : stable (follow input spacing)
                    # -spp = 2 : always space before opening prototype paren
                    my $spp = $rOpts->{'space-prototype-paren'};
                    if ( defined($spp) ) {
                        if    ( $spp == 0 ) { $token =~ s/\s+\(/\(/; }
                        elsif ( $spp == 2 ) { $token =~ s/\(/ (/; }
                    }

                    # one space max, and no tabs
                    $token =~ s/\s+/ /g;
                    $rtoken_vars->[_TOKEN_] = $token;
                }

                # clean up spaces in package identifiers, like
                #   "package        Bob::Dog;"
                if ( $token =~ /^package\s/ ) {
                    $token =~ s/\s+/ /g;
                    $rtoken_vars->[_TOKEN_] = $token;
                }

                # trim identifiers of trailing blanks which can occur
                # under some unusual circumstances, such as if the
                # identifier 'witch' has trailing blanks on input here:
                #
                # sub
                # witch
                # ()   # prototype may be on new line ...
                # ...
                if ( $type eq 'i' ) {
                    $token =~ s/\s+$//g;
                    $rtoken_vars->[_TOKEN_] = $token;
                }
            }

            # handle semicolons
            elsif ( $type eq ';' ) {

                # Remove unnecessary semicolons, but not after bare
                # blocks, where it could be unsafe if the brace is
                # mistokenized.
                if (
                    $rOpts->{'delete-semicolons'}
                    && (
                        (
                            $last_nonblank_type eq '}'
                            && (
                                $is_block_without_semicolon{
                                    $last_nonblank_block_type}
                                || $last_nonblank_block_type =~ /$SUB_PATTERN/
                                || $last_nonblank_block_type =~ /^\w+:$/ )
                        )
                        || $last_nonblank_type eq ';'
                    )
                  )
                {

                    # This looks like a deletable semicolon, but even if a
                    # semicolon can be deleted it is necessarily best to do so.
                    # We apply these additional rules for deletion:
                    # - Always ok to delete a ';' at the end of a line
                    # - Never delete a ';' before a '#' because it would
                    #   promote it to a block comment.
                    # - If a semicolon is not at the end of line, then only
                    #   delete if it is followed by another semicolon or closing
                    #   token.  This includes the comment rule.  It may take
                    #   two passes to get to a final state, but it is a little
                    #   safer.  For example, keep the first semicolon here:
                    #      eval { sub bubba { ok(0) }; ok(0) } || ok(1);
                    #   It is not required but adds some clarity.
                    my $ok_to_delete = 1;
                    if ( $KK < $Klast ) {
                        my $Kn = $self->K_next_nonblank($KK);
                        if ( defined($Kn) && $Kn <= $Klast ) {
                            my $next_nonblank_token_type =
                              $rLL->[$Kn]->[_TYPE_];
                            $ok_to_delete = $next_nonblank_token_type eq ';'
                              || $next_nonblank_token_type eq '}';
                        }
                    }

                    # do not delete only nonblank token in a file
                    else {
                        my $Kn = $self->K_next_nonblank($KK);
                        $ok_to_delete = defined($Kn) || $nonblank_token_count;
                    }

                    if ($ok_to_delete) {
                        $self->note_deleted_semicolon($input_line_number);
                        next;
                    }
                    else {
                        write_logfile_entry("Extra ';'\n");
                    }
                }
            }

            # patch to add space to something like "x10"
            # This avoids having to split this token in the pre-tokenizer
            elsif ( $type eq 'n' ) {
                if ( $token =~ /^x\d+/ ) {
                    $token =~ s/x/x /;
                    $rtoken_vars->[_TOKEN_] = $token;
                }
            }

            # check for a qw quote
            elsif ( $type eq 'q' ) {

                # trim blanks from right of qw quotes
                # (To avoid trimming qw quotes use -ntqw; the tokenizer handles
                # this)
                $token =~ s/\s*$//;
                $rtoken_vars->[_TOKEN_] = $token;
                $self->note_embedded_tab($input_line_number)
                  if ( $token =~ "\t" );

                if ($in_multiline_qw) {

                    # If we are at the end of a multiline qw ..
                    if ( $in_multiline_qw == $KK ) {

                 # Split off the closing delimiter character
                 # so that the formatter can put a line break there if necessary
                        my $part1 = $token;
                        my $part2 = substr( $part1, -1, 1, "" );

                        if ($part1) {
                            my $rcopy =
                              copy_token_as_type( $rtoken_vars, 'q', $part1 );
                            $store_token->($rcopy);
                            $token = $part2;
                            $rtoken_vars->[_TOKEN_] = $token;

                        }
                        $in_multiline_qw = undef;

                        # store without preceding blank
                        $store_token->($rtoken_vars);
                        next;
                    }
                    else {
                        # continuing a multiline qw
                        $store_token->($rtoken_vars);
                        next;
                    }
                }

                else {

                    # we are encountered new qw token...see if multiline
                    if ($ALLOW_BREAK_MULTILINE_QW) {
                        my $K_end = $K_end_q->($KK);
                        if ( $K_end != $KK ) {

                            # Starting multiline qw...
                            # set flag equal to the ending K
                            $in_multiline_qw = $K_end;

                          # Split off the leading part so that the formatter can
                          # put a line break there if necessary
                            if ( $token =~ /^(qw\s*.)(.*)$/ ) {
                                my $part1 = $1;
                                my $part2 = $2;
                                if ($part2) {
                                    my $rcopy =
                                      copy_token_as_type( $rtoken_vars, 'q',
                                        $part1 );
                                    $store_token_and_space->(
                                        $rcopy,
                                        $rwhitespace_flags->[$KK] == WS_YES
                                    );
                                    $token = $part2;
                                    $rtoken_vars->[_TOKEN_] = $token;

                                   # Second part goes without intermediate blank
                                    $store_token->($rtoken_vars);
                                    next;
                                }
                            }
                        }
                    }
                    else {

                        # this is a new single token qw -
                        # store with possible preceding blank
                        $store_token_and_space->(
                            $rtoken_vars, $rwhitespace_flags->[$KK] == WS_YES
                        );
                        next;
                    }
                }
            } ## end if ( $type eq 'q' )

            # change 'LABEL   :'   to 'LABEL:'
            elsif ( $type eq 'J' ) {
                $token =~ s/\s+//g;
                $rtoken_vars->[_TOKEN_] = $token;
            }

            # check a quote for problems
            elsif ( $type eq 'Q' ) {
                $check_Q->( $KK, $Kfirst, $input_line_number );
            }

            # Store this token with possible previous blank
            $store_token_and_space->(
                $rtoken_vars, $rwhitespace_flags->[$KK] == WS_YES
            );

        }    # End token loop
    }    # End line loop

    # Walk backwards through the tokens, making forward links to sequence items.
    if ( @{$rLL_new} ) {
        my $KNEXT;
        for ( my $KK = @{$rLL_new} - 1 ; $KK >= 0 ; $KK-- ) {
            $rLL_new->[$KK]->[_KNEXT_SEQ_ITEM_] = $KNEXT;
            if ( $rLL_new->[$KK]->[_TYPE_SEQUENCE_] ) { $KNEXT = $KK }
        }
        $self->[_K_first_seq_item_] = $KNEXT;
    }

    # find and remember lists by sequence number
    # TODO: eventually this should hold a name for the list
    my $ris_list_by_seqno = {};
    foreach my $seqno ( keys %{$K_opening_container} ) {
        my $K_opening  = $K_opening_container->{$seqno};
        my $block_type = $rLL_new->[$K_opening]->[_BLOCK_TYPE_];
        next if ($block_type);
        my $rtype_count = $rtype_count_by_seqno->{$seqno};
        next unless ($rtype_count);
        my $fat_comma_count = $rtype_count->{'=>'};
        my $comma_count     = $rtype_count->{','};
        my $semicolon_count = $rtype_count->{';'};

        # This definition of a list is sufficient for our needs
        if ( ( $fat_comma_count || $comma_count ) && !$semicolon_count ) {
            $ris_list_by_seqno->{$seqno} = $seqno;
        }
    }

    # Reset memory to be the new array
    $self->[_rLL_] = $rLL_new;
    my $Klimit;
    if ( @{$rLL_new} ) { $Klimit = @{$rLL_new} - 1 }
    $self->[_Klimit_]                = $Klimit;
    $self->[_K_opening_container_]   = $K_opening_container;
    $self->[_K_closing_container_]   = $K_closing_container;
    $self->[_K_opening_ternary_]     = $K_opening_ternary;
    $self->[_K_closing_ternary_]     = $K_closing_ternary;
    $self->[_rK_phantom_semicolons_] = $rK_phantom_semicolons;
    $self->[_rtype_count_by_seqno_]  = $rtype_count_by_seqno;
    $self->[_ris_broken_container_]  = $ris_broken_container;
    $self->[_rhas_broken_container_] = $rhas_broken_container;
    $self->[_rparent_of_seqno_]      = $rparent_of_seqno;
    $self->[_rchildren_of_seqno_]    = $rchildren_of_seqno;
    $self->[_ris_list_by_seqno_]     = $ris_list_by_seqno;

    # DEBUG OPTION: make sure the new array looks okay.
    # This is no longer needed but should be retained for future development.
    DEVEL_MODE && $self->check_token_array();

    # reset the token limits of each line
    $self->resync_lines_and_tokens();

    return;
}

sub copy_token_as_type {

    # This provides a quick way to create a new token by
    # slightly modifying an existing token.
    my ( $rold_token, $type, $token ) = @_;
    if ( $type eq 'b' ) {
        $token = " " unless defined($token);
    }
    elsif ( $type eq 'q' ) {
        $token = '' unless defined($token);
    }
    elsif ( $type eq '->' ) {
        $token = '->' unless defined($token);
    }
    elsif ( $type eq ';' ) {
        $token = ';' unless defined($token);
    }
    else {

        # This sub assumes it will be called with just two types, 'b' or 'q'
        Fault(
"Programming error: copy_token_as has type $type but should be 'b' or 'q'"
        );
    }

    my @rnew_token = @{$rold_token};
    $rnew_token[_TYPE_]                  = $type;
    $rnew_token[_TOKEN_]                 = $token;
    $rnew_token[_BLOCK_TYPE_]            = '';
    $rnew_token[_CONTAINER_ENVIRONMENT_] = '';
    $rnew_token[_TYPE_SEQUENCE_]         = '';
    return \@rnew_token;
}

sub Debug_dump_tokens {

    # a debug routine, not normally used
    my ( $self, $msg ) = @_;
    my $rLL   = $self->[_rLL_];
    my $nvars = @{$rLL};
    print STDERR "$msg\n";
    print STDERR "ntokens=$nvars\n";
    print STDERR "K\t_TOKEN_\t_TYPE_\n";
    my $K = 0;

    foreach my $item ( @{$rLL} ) {
        print STDERR "$K\t$item->[_TOKEN_]\t$item->[_TYPE_]\n";
        $K++;
    }
    return;
}

sub K_next_code {
    my ( $self, $KK, $rLL ) = @_;

    # return the index K of the next nonblank, non-comment token
    return unless ( defined($KK) && $KK >= 0 );

    # use the standard array unless given otherwise
    $rLL = $self->[_rLL_] unless ( defined($rLL) );
    my $Num  = @{$rLL};
    my $Knnb = $KK + 1;
    while ( $Knnb < $Num ) {
        if ( !defined( $rLL->[$Knnb] ) ) {

            # We seem to have encountered a gap in our array.
            # This shouldn't happen because sub write_line() pushed
            # items into the $rLL array.
            Fault("Undefined entry for k=$Knnb");
        }
        if (   $rLL->[$Knnb]->[_TYPE_] ne 'b'
            && $rLL->[$Knnb]->[_TYPE_] ne '#' )
        {
            return $Knnb;
        }
        $Knnb++;
    }
    return;
}

sub K_next_nonblank {
    my ( $self, $KK, $rLL ) = @_;

    # return the index K of the next nonblank token, or
    # return undef if none
    return unless ( defined($KK) && $KK >= 0 );

    # The third arg allows this routine to be used on any array.  This is
    # useful in sub respace_tokens when we are copying tokens from an old $rLL
    # to a new $rLL array.  But usually the third arg will not be given and we
    # will just use the $rLL array in $self.
    $rLL = $self->[_rLL_] unless ( defined($rLL) );
    my $Num  = @{$rLL};
    my $Knnb = $KK + 1;
    return unless ( $Knnb < $Num );
    return $Knnb if ( $rLL->[$Knnb]->[_TYPE_] ne 'b' );
    return unless ( ++$Knnb < $Num );
    return $Knnb if ( $rLL->[$Knnb]->[_TYPE_] ne 'b' );

    # Backup loop. Very unlikely to get here; it means we have neighboring
    # blanks in the token stream.
    $Knnb++;
    while ( $Knnb < $Num ) {

        # Safety check, this fault shouldn't happen:  The $rLL array is the
        # main array of tokens, so all entries should be used.  It is
        # initialized in sub write_line, and then re-initialized by sub
        # $store_token() within sub respace_tokens.  Tokens are pushed on
        # so there shouldn't be any gaps.
        if ( !defined( $rLL->[$Knnb] ) ) {
            Fault("Undefined entry for k=$Knnb");
        }
        if ( $rLL->[$Knnb]->[_TYPE_] ne 'b' ) { return $Knnb }
        $Knnb++;
    }
    return;
}

sub K_previous_code {

    # return the index K of the previous nonblank, non-comment token
    # Call with $KK=undef to start search at the top of the array
    my ( $self, $KK, $rLL ) = @_;

    # use the standard array unless given otherwise
    $rLL = $self->[_rLL_] unless ( defined($rLL) );
    my $Num = @{$rLL};
    if    ( !defined($KK) ) { $KK = $Num }
    elsif ( $KK > $Num ) {

        # This fault can be caused by a programming error in which a bad $KK is
        # given.  The caller should make the first call with KK_new=undef to
        # avoid this error.
        Fault(
"Program Bug: K_previous_nonblank_new called with K=$KK which exceeds $Num"
        );
    }
    my $Kpnb = $KK - 1;
    while ( $Kpnb >= 0 ) {
        if (   $rLL->[$Kpnb]->[_TYPE_] ne 'b'
            && $rLL->[$Kpnb]->[_TYPE_] ne '#' )
        {
            return $Kpnb;
        }
        $Kpnb--;
    }
    return;
}

sub K_previous_nonblank {

    # return index of previous nonblank token before item K;
    # Call with $KK=undef to start search at the top of the array
    my ( $self, $KK, $rLL ) = @_;

    # use the standard array unless given otherwise
    $rLL = $self->[_rLL_] unless ( defined($rLL) );
    my $Num = @{$rLL};
    if    ( !defined($KK) ) { $KK = $Num }
    elsif ( $KK > $Num ) {

        # This fault can be caused by a programming error in which a bad $KK is
        # given.  The caller should make the first call with KK_new=undef to
        # avoid this error.
        Fault(
"Program Bug: K_previous_nonblank_new called with K=$KK which exceeds $Num"
        );
    }
    my $Kpnb = $KK - 1;
    return unless ( $Kpnb >= 0 );
    return $Kpnb if ( $rLL->[$Kpnb]->[_TYPE_] ne 'b' );
    return unless ( --$Kpnb >= 0 );
    return $Kpnb if ( $rLL->[$Kpnb]->[_TYPE_] ne 'b' );

    # Backup loop. We should not get here unless some routine
    # slipped repeated blanks into the token stream.
    return unless ( --$Kpnb >= 0 );
    while ( $Kpnb >= 0 ) {
        if ( $rLL->[$Kpnb]->[_TYPE_] ne 'b' ) { return $Kpnb }
        $Kpnb--;
    }
    return;
}

sub get_old_line_index {

    # return index of the original line that token K was on
    my ( $self, $K ) = @_;
    my $rLL = $self->[_rLL_];
    return 0 unless defined($K);
    return $rLL->[$K]->[_LINE_INDEX_];
}

sub get_old_line_count {

    # return number of input lines separating two tokens
    my ( $self, $Kbeg, $Kend ) = @_;
    my $rLL = $self->[_rLL_];
    return 0 unless defined($Kbeg);
    return 0 unless defined($Kend);
    return $rLL->[$Kend]->[_LINE_INDEX_] - $rLL->[$Kbeg]->[_LINE_INDEX_] + 1;
}

sub parent_seqno_by_K {

    # Return the sequence number of the parent container of token K, if any.

    my ( $self, $KK ) = @_;
    return unless defined($KK);

    # Note: This routine is relatively slow. I tried replacing it with a hash
    # which is easily created in sub respace_tokens. But the total time with a
    # hash was greater because this routine is called once per line whereas a
    # hash must be created token-by-token.

    my $rLL   = $self->[_rLL_];
    my $KNEXT = $KK;

    # For example, consider the following with seqno=5 of the '[' and ']'
    # being called with index K of the first token of each line:

    #                                              # result
    #    push @tests,                              # -
    #      [                                       # -
    #        sub { 99 },   'do {&{%s} for 1,2}',   # 5
    #        '(&{})(&{})', undef,                  # 5
    #        [ 2, 2, 0 ],  0                       # 5
    #      ];                                      # -

    my $parent_seqno;
    while ( defined($KNEXT) ) {
        my $Kt = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];
        my $rtoken_vars   = $rLL->[$Kt];
        my $type          = $rtoken_vars->[_TYPE_];
        my $type_sequence = $rtoken_vars->[_TYPE_SEQUENCE_];

        # if next container token is closing, it is the parent seqno
        if ( $is_closing_type{$type} ) {
            if ( $Kt > $KK ) {
                $parent_seqno = $type_sequence;
            }
            else {
                $parent_seqno = $self->[_rparent_of_seqno_]->{$type_sequence};
            }
            last;
        }

        # if next container token is opening, we want its parent container
        elsif ( $is_opening_type{$type} ) {
            $parent_seqno = $self->[_rparent_of_seqno_]->{$type_sequence};
            last;
        }

        # not a container - must be ternary - keep going
    }

    return $parent_seqno;
}

sub is_list_by_K {

    # Return true if token K is in a list
    my ( $self, $KK ) = @_;

    my $parent_seqno = $self->parent_seqno_by_K($KK);
    return unless defined($parent_seqno);
    return $self->[_ris_list_by_seqno_]->{$parent_seqno};
}

sub is_list_by_seqno {

    # Return true if the immediate contents of a container appears to be a
    # list.
    my ( $self, $seqno ) = @_;
    return unless defined($seqno);
    return $self->[_ris_list_by_seqno_]->{$seqno};
}

sub resync_lines_and_tokens {

    my $self   = shift;
    my $rLL    = $self->[_rLL_];
    my $Klimit = $self->[_Klimit_];
    my $rlines = $self->[_rlines_];
    my @Krange_code_without_comments;
    my @Klast_valign_code;

    # Re-construct the arrays of tokens associated with the original input lines
    # since they have probably changed due to inserting and deleting blanks
    # and a few other tokens.

    my $Kmax = -1;

    # This is the next token and its line index:
    my $Knext = 0;
    my $inext;
    if ( defined($rLL) && @{$rLL} ) {
        $Kmax  = @{$rLL} - 1;
        $inext = $rLL->[$Knext]->[_LINE_INDEX_];
    }

    # Remember the most recently output token index
    my $Klast_out;

    my $iline = -1;
    foreach my $line_of_tokens ( @{$rlines} ) {
        $iline++;
        my $line_type = $line_of_tokens->{_line_type};
        my $CODE_type = $line_of_tokens->{_code_type};
        if ( $line_type eq 'CODE' ) {

            my @K_array;
            my $rK_range;
            if ( $Knext <= $Kmax ) {
                $inext = $rLL->[$Knext]->[_LINE_INDEX_];
                while ( $inext <= $iline ) {
                    push @K_array, $Knext;
                    $Knext += 1;
                    if ( $Knext > $Kmax ) {
                        $inext = undef;
                        last;
                    }
                    $inext = $rLL->[$Knext]->[_LINE_INDEX_];
                }
            }

            # Delete any terminal blank token
            if (@K_array) {
                if ( $rLL->[ $K_array[-1] ]->[_TYPE_] eq 'b' ) {
                    pop @K_array;
                }
            }

            # Define the range of K indexes for the line:
            # $Kfirst = index of first token on line
            # $Klast_out = index of last token on line
            my ( $Kfirst, $Klast );
            if (@K_array) {
                $Kfirst    = $K_array[0];
                $Klast     = $K_array[-1];
                $Klast_out = $Klast;

                if ( defined($Kfirst) ) {

                    # Save ranges of non-comment code. This will be used by
                    # sub keep_old_line_breaks.
                    if ( $rLL->[$Kfirst]->[_TYPE_] ne '#' ) {
                        push @Krange_code_without_comments, [ $Kfirst, $Klast ];
                    }

                    # Only save ending K indexes of code types which are blank
                    # or 'VER'.  These will be used for a convergence check.
                    # See related code in sub 'send_lines_to_vertical_aligner'.
                    if (  !$CODE_type
                        || $CODE_type eq 'VER' )
                    {
                        push @Klast_valign_code, $Klast;
                    }
                }
            }

            # It is only safe to trim the actual line text if the input
            # line had a terminal blank token. Otherwise, we may be
            # in a quote.
            if ( $line_of_tokens->{_ended_in_blank_token} ) {
                $line_of_tokens->{_line_text} =~ s/\s+$//;
            }
            $line_of_tokens->{_rK_range} = [ $Kfirst, $Klast ];

            # Deleting semicolons can create new empty code lines
            # which should be marked as blank
            if ( !defined($Kfirst) ) {
                my $code_type = $line_of_tokens->{_code_type};
                if ( !$code_type ) {
                    $line_of_tokens->{_code_type} = 'BL';
                }
            }
        }
    }

    # There shouldn't be any nodes beyond the last one.  This routine is
    # relinking lines and tokens after the tokens have been respaced.  A fault
    # here indicates some kind of bug has been introduced into the above loops.
    if ( defined($inext) ) {

        Fault("unexpected tokens at end of file when reconstructing lines");
    }
    $self->[_rKrange_code_without_comments_] = \@Krange_code_without_comments;

    # Setup the convergence test in the FileWriter based on line-ending indexes
    my $file_writer_object = $self->[_file_writer_object_];
    $file_writer_object->setup_convergence_test( \@Klast_valign_code );

    return;
}

sub keep_old_line_breaks {

    # Called once per file to find and mark any old line breaks which
    # should be kept.  We will be translating the input hashes into
    # token indexes.
    my ($self) = @_;

    return unless ( %keep_break_before_type || %keep_break_after_type );

    my $rLL = $self->[_rLL_];

    my $rKrange_code_without_comments =
      $self->[_rKrange_code_without_comments_];
    my $rbreak_before_Kfirst = $self->[_rbreak_before_Kfirst_];
    my $rbreak_after_Klast   = $self->[_rbreak_after_Klast_];

    foreach my $item ( @{$rKrange_code_without_comments} ) {
        my ( $Kfirst, $Klast ) = @{$item};

        my $type_first = $rLL->[$Kfirst]->[_TYPE_];
        if ( $keep_break_before_type{$type_first} ) {
            $rbreak_before_Kfirst->{$Kfirst} = 1;
        }

        my $type_last = $rLL->[$Klast]->[_TYPE_];
        if ( $keep_break_after_type{$type_last} ) {
            $rbreak_after_Klast->{$Klast} = 1;
        }
    }
    return;
}

sub weld_containers {

    # Called once per file to do any welding operations requested by --weld*
    # flags.
    my ($self) = @_;

    return if ( $rOpts->{'indent-only'} );
    return unless ($rOpts_add_newlines);

    if ( $rOpts->{'weld-nested-containers'} ) {

        # if called, weld_nested_containers must be called before other weld
        # operations.  This is because weld_nested_containers could overwrite
        # hash values written by weld_cuddled_blocks and weld_nested_quotes.
        $self->weld_nested_containers();

        $self->weld_nested_quotes();
    }

    # Note that weld_nested_containers() changes the _LEVEL_ values, so
    # weld_cuddled_blocks must use the _TRUE_LEVEL_ values instead.

    # Here is a good test case to be sure that both cuddling and welding
    # are working and not interfering with each other: <<snippets/ce_wn1.in>>

    #   perltidy -wn -ce

   # if ($BOLD_MATH) { (
   #     $labels, $comment,
   #     join( '', '<B>', &make_math( $mode, '', '', $_ ), '</B>' )
   # ) } else { (
   #     &process_math_in_latex( $mode, $math_style, $slevel, "\\mbox{$text}" ),
   #     $after
   # ) }

    $self->weld_cuddled_blocks();

    # After all welding is complete, we make a note of which seqence numbers
    # have welds for quick checks.
    my @q;
    my $ris_welded_seqno = $self->[_ris_welded_seqno_];
    @q = keys %{ $self->[_rweld_len_left_closing_] };
    @{$ris_welded_seqno}{@q} = (1) x scalar(@q);
    @q = keys %{ $self->[_rweld_len_right_closing_] };
    @{$ris_welded_seqno}{@q} = (1) x scalar(@q);
    @q = keys %{ $self->[_rweld_len_left_opening_] };
    @{$ris_welded_seqno}{@q} = (1) x scalar(@q);
    @q = keys %{ $self->[_rweld_len_right_opening_] };
    @{$ris_welded_seqno}{@q} = (1) x scalar(@q);

    # total number of sequenced items involved in a weld, for
    # quick checks for avoiding calls to weld_len_xxx
    $total_weld_count = 0 + keys %{$ris_welded_seqno};

    return;
}

sub cumulative_length_before_K {
    my ( $self, $KK ) = @_;
    my $rLL = $self->[_rLL_];
    return ( $KK <= 0 ) ? 0 : $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_];
}

sub cumulative_length_after_K {

    # NOTE: This routine not currently called; could be deleted
    my ( $self, $KK ) = @_;
    my $rLL = $self->[_rLL_];
    return $rLL->[$KK]->[_CUMULATIVE_LENGTH_];
}

sub weld_cuddled_blocks {
    my ($self) = @_;

    # Called once per file to handle cuddled formatting

    my $rweld_len_right_closing = $self->[_rweld_len_right_closing_];

    # This routine implements the -cb flag by finding the appropriate
    # closing and opening block braces and welding them together.
    return unless ( %{$rcuddled_block_types} );

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );
    my $rbreak_container = $self->[_rbreak_container_];

    my $K_opening_container = $self->[_K_opening_container_];
    my $K_closing_container = $self->[_K_closing_container_];

    my $length_to_opening_seqno = sub {
        my ($seqno) = @_;
        my $KK      = $K_opening_container->{$seqno};
        my $lentot  = $KK <= 0 ? 0 : $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_];
        return $lentot;
    };
    my $length_to_closing_seqno = sub {
        my ($seqno) = @_;
        my $KK      = $K_closing_container->{$seqno};
        my $lentot  = $KK <= 0 ? 0 : $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_];
        return $lentot;
    };

    my $is_broken_block = sub {

        # a block is broken if the input line numbers of the braces differ
        # we can only cuddle between broken blocks
        my ($seqno) = @_;
        my $K_opening = $K_opening_container->{$seqno};
        return unless ( defined($K_opening) );
        my $K_closing = $K_closing_container->{$seqno};
        return unless ( defined($K_closing) );
        return $rbreak_container->{$seqno}
          || $rLL->[$K_closing]->[_LINE_INDEX_] !=
          $rLL->[$K_opening]->[_LINE_INDEX_];
    };

    # A stack to remember open chains at all levels: This is a hash rather than
    # an array for safety because negative levels can occur in files with
    # errors.  This allows us to keep processing with negative levels.
    # $in_chain{$level} = [$chain_type, $type_sequence];
    my %in_chain;
    my $CBO = $rOpts->{'cuddled-break-option'};

    # loop over structure items to find cuddled pairs
    my $level = 0;
    my $KNEXT = $self->[_K_first_seq_item_];
    while ( defined($KNEXT) ) {
        my $KK = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];
        my $rtoken_vars   = $rLL->[$KK];
        my $type_sequence = $rtoken_vars->[_TYPE_SEQUENCE_];
        if ( !$type_sequence ) {
            next if ( $KK == 0 );    # first token in file may not be container

            # A fault here implies that an error was made in the little loop at
            # the bottom of sub 'respace_tokens' which set the values of
            # _KNEXT_SEQ_ITEM_.  Or an error has been introduced in the
            # loop control lines above.
            Fault("sequence = $type_sequence not defined at K=$KK");
        }

        # We use the original levels because they get changed by sub
        # 'weld_nested_containers'. So if this were to be called before that
        # routine, the levels would be wrong and things would go bad.
        my $last_level = $level;
        $level = $rtoken_vars->[_LEVEL_TRUE_];

        if    ( $level < $last_level ) { $in_chain{$last_level} = undef }
        elsif ( $level > $last_level ) { $in_chain{$level}      = undef }

        # We are only looking at code blocks
        my $token = $rtoken_vars->[_TOKEN_];
        my $type  = $rtoken_vars->[_TYPE_];
        next unless ( $type eq $token );

        if ( $token eq '{' ) {

            my $block_type = $rtoken_vars->[_BLOCK_TYPE_];
            if ( !$block_type ) {

                # patch for unrecognized block types which may not be labeled
                my $Kp = $self->K_previous_nonblank($KK);
                while ( $Kp && $rLL->[$Kp]->[_TYPE_] eq '#' ) {
                    $Kp = $self->K_previous_nonblank($Kp);
                }
                next unless $Kp;
                $block_type = $rLL->[$Kp]->[_TOKEN_];

            }
            if ( $in_chain{$level} ) {

                # we are in a chain and are at an opening block brace.
                # See if we are welding this opening brace with the previous
                # block brace.  Get their identification numbers:
                my $closing_seqno = $in_chain{$level}->[1];
                my $opening_seqno = $type_sequence;

                # The preceding block must be on multiple lines so that its
                # closing brace will start a new line.
                if ( !$is_broken_block->($closing_seqno) ) {
                    next unless ( $CBO == 2 );
                    $rbreak_container->{$closing_seqno} = 1;
                }

                # we will let the trailing block be either broken or intact
                ## && $is_broken_block->($opening_seqno);

                # We can weld the closing brace to its following word ..
                my $Ko = $K_closing_container->{$closing_seqno};
                my $Kon;
                if ( defined($Ko) ) {
                    $Kon = $self->K_next_nonblank($Ko);
                }

                # ..unless it is a comment
                if ( defined($Kon) && $rLL->[$Kon]->[_TYPE_] ne '#' ) {
                    my $dlen =
                      $rLL->[$Kon]->[_CUMULATIVE_LENGTH_] -
                      $rLL->[ $Ko - 1 ]->[_CUMULATIVE_LENGTH_];
                    $rweld_len_right_closing->{$closing_seqno} = $dlen;

                    # Set flag that we want to break the next container
                    # so that the cuddled line is balanced.
                    $rbreak_container->{$opening_seqno} = 1
                      if ($CBO);
                }

            }
            else {

                # We are not in a chain. Start a new chain if we see the
                # starting block type.
                if ( $rcuddled_block_types->{$block_type} ) {
                    $in_chain{$level} = [ $block_type, $type_sequence ];
                }
                else {
                    $block_type = '*';
                    $in_chain{$level} = [ $block_type, $type_sequence ];
                }
            }
        }
        elsif ( $token eq '}' ) {
            if ( $in_chain{$level} ) {

                # We are in a chain at a closing brace.  See if this chain
                # continues..
                my $Knn = $self->K_next_code($KK);
                next unless $Knn;

                my $chain_type          = $in_chain{$level}->[0];
                my $next_nonblank_token = $rLL->[$Knn]->[_TOKEN_];
                if (
                    $rcuddled_block_types->{$chain_type}->{$next_nonblank_token}
                  )
                {

                    # Note that we do not weld yet because we must wait until
                    # we we are sure that an opening brace for this follows.
                    $in_chain{$level}->[1] = $type_sequence;
                }
                else { $in_chain{$level} = undef }
            }
        }
    }
    return;
}

sub find_nested_pairs {
    my $self = shift;

    # This routine is called once per file to do preliminary work needed for
    # the --weld-nested option.  This information is also needed for adding
    # semicolons.

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );
    my $Num = @{$rLL};

    my $K_opening_container = $self->[_K_opening_container_];
    my $K_closing_container = $self->[_K_closing_container_];

    # We define an array of pairs of nested containers
    my @nested_pairs;

    # Names of calling routines can either be marked as 'i' or 'w',
    # and they may invoke a sub call with an '->'. We will consider
    # any consecutive string of such types as a single unit when making
    # weld decisions.  We also allow a leading !
    my $is_name_type = {
        'i'  => 1,
        'w'  => 1,
        'U'  => 1,
        '->' => 1,
        '!'  => 1,
    };

    # Loop over all closing container tokens
    foreach my $inner_seqno ( keys %{$K_closing_container} ) {
        my $K_inner_closing = $K_closing_container->{$inner_seqno};

        # See if it is immediately followed by another, outer closing token
        my $K_outer_closing = $K_inner_closing + 1;
        $K_outer_closing += 1
          if ( $K_outer_closing < $Num
            && $rLL->[$K_outer_closing]->[_TYPE_] eq 'b' );

        next unless ( $K_outer_closing < $Num );
        my $outer_seqno = $rLL->[$K_outer_closing]->[_TYPE_SEQUENCE_];
        next unless ($outer_seqno);
        my $token_outer_closing = $rLL->[$K_outer_closing]->[_TOKEN_];
        next unless ( $is_closing_token{$token_outer_closing} );

        # Now we have to check the opening tokens.
        my $K_outer_opening = $K_opening_container->{$outer_seqno};
        my $K_inner_opening = $K_opening_container->{$inner_seqno};
        next unless defined($K_outer_opening) && defined($K_inner_opening);

        # Verify that the inner opening token is the next container after the
        # outer opening token.
        my $K_io_check = $rLL->[$K_outer_opening]->[_KNEXT_SEQ_ITEM_];
        next unless defined($K_io_check);
        if ( $K_io_check != $K_inner_opening ) {

            # The inner opening container does not immediately follow the outer
            # opening container, but we may still allow a weld if they are
            # separated by a sub signature.  For example, we may have something
            # like this, where $K_io_check may be at the first 'x' instead of
            # 'io'.  So we need to hop over the signature and see if we arrive
            # at 'io'.

            #            oo               io
            #             |     x       x |
            #   $obj->then( sub ( $code ) {
            #       ...
            #       return $c->render(text => '', status => $code);
            #   } );
            #   | |
            #  ic oc

            next if $rLL->[$K_inner_opening]->[_BLOCK_TYPE_] ne 'sub';
            next if $rLL->[$K_io_check]->[_TOKEN_] ne '(';
            my $seqno_signature = $rLL->[$K_io_check]->[_TYPE_SEQUENCE_];
            next unless defined($seqno_signature);
            my $K_signature_closing = $K_closing_container->{$seqno_signature};
            next unless defined($K_signature_closing);
            my $K_test = $rLL->[$K_signature_closing]->[_KNEXT_SEQ_ITEM_];
            next
              unless ( defined($K_test) && $K_test == $K_inner_opening );

            # OK, we have arrived at 'io' in the above diagram.  We should put
            # a limit on the length or complexity of the signature here.  There
            # is no perfect way to do this, one way is to put a limit on token
            # count.  For consistency with older versions, we should allow a
            # signature with a single variable to weld, but not with
            # multiple variables.  A single variable as in 'sub ($code) {' can
            # have a $Kdiff of 2 to 4, depending on spacing.

            # But two variables like 'sub ($v1,$v2) {' can have a diff of 4 to
            # 7, depending on spacing. So to keep formatting consistent with
            # previous versions, we will also avoid welding if there is a comma
            # in the signature.

            my $Kdiff = $K_signature_closing - $K_io_check;
            next if ( $Kdiff > 4 );

            my $saw_comma;
            foreach my $KK ( $K_io_check + 1 .. $K_signature_closing - 1 ) {
                if ( $rLL->[$KK]->[_TYPE_] eq ',' ) { $saw_comma = 1; last }
            }
            next if ($saw_comma);
        }

        # Yes .. this is a possible nesting pair.
        # They can be separated by a small amount.
        my $K_diff = $K_inner_opening - $K_outer_opening;

        # Count nonblank characters separating them.
        if ( $K_diff < 0 ) { next }    # Shouldn't happen
        my $Kn             = $K_outer_opening;
        my $nonblank_count = 0;
        my $type;
        my $is_name;

        # Here is an example of a long identifier chain which counts as a
        # single nonblank here (this spans about 10 K indexes):
        #     if ( !Boucherot::SetOfConnections->new->handler->execute(
        #        ^--K_o_o                                             ^--K_i_o
        #       @array) )
        my $Kn_first = $K_outer_opening;
        for (
            my $Kn = $K_outer_opening + 1 ;
            $Kn <= $K_inner_opening ;
            $Kn += 1
          )
        {
            next if ( $rLL->[$Kn]->[_TYPE_] eq 'b' );
            if ( !$nonblank_count )        { $Kn_first = $Kn }
            if ( $Kn eq $K_inner_opening ) { $nonblank_count++; last; }

            # skip chain of identifier tokens
            my $last_type    = $type;
            my $last_is_name = $is_name;
            $type    = $rLL->[$Kn]->[_TYPE_];
            $is_name = $is_name_type->{$type};
            next if ( $is_name && $last_is_name );

            $nonblank_count++;
            last if ( $nonblank_count > 2 );
        }

        if (

            # adjacent opening containers, like: do {{
            $nonblank_count == 1

            # short item following opening paren, like:  fun( yyy (
            || (   $nonblank_count == 2
                && $rLL->[$K_outer_opening]->[_TOKEN_] eq '(' )

            # anonymous sub + prototype or sig:  )->then( sub ($code) {
            # ... but it seems best not to stack two structural blocks, like
            # this
            #    sub make_anon_with_my_sub { sub {
            # because it probably hides the structure a little too much.
            || (   $rLL->[$K_inner_opening]->[_BLOCK_TYPE_] eq 'sub'
                && $rLL->[$Kn_first]->[_TOKEN_] eq 'sub'
                && !$rLL->[$K_outer_opening]->[_BLOCK_TYPE_] )
          )
        {
            push @nested_pairs,
              [ $inner_seqno, $outer_seqno, $K_inner_closing ];
        }
        next;
    }

    # The weld routine expects the pairs in order in the form
    #   [$seqno_inner, $seqno_outer]
    # And they must be in the same order as the inner closing tokens
    # (otherwise, welds of three or more adjacent tokens will not work).  The K
    # value of this inner closing token has temporarily been stored for
    # sorting.
    @nested_pairs =

      # Drop the K index after sorting (it would cause trouble downstream)
      map { [ $_->[0], $_->[1] ] }

      # Sort on the K values
      sort { $a->[2] <=> $b->[2] } @nested_pairs;

    return \@nested_pairs;
}

sub is_excluded_weld {

    # decide if this weld is excluded by user request
    my ( $self, $KK, $is_leading ) = @_;
    my $rLL         = $self->[_rLL_];
    my $rtoken_vars = $rLL->[$KK];
    my $token       = $rtoken_vars->[_TOKEN_];
    my $rflags      = $weld_nested_exclusion_rules{$token};
    return 0 unless ( defined($rflags) );
    my $flag = $is_leading ? $rflags->[0] : $rflags->[1];
    return 0 unless ( defined($flag) );
    return 1 if $flag eq '*';

    my ( $is_f, $is_k, $is_w );
    my $Kp = $self->K_previous_nonblank($KK);
    if ( defined($Kp) ) {
        my $type_p  = $rLL->[$Kp]->[_TYPE_];
        my $token_p = $rLL->[$Kp]->[_TOKEN_];

        # keyword?
        $is_k = $type_p eq 'k';

        # function call? Use the same definition as used for
        # the parameter 'space-function-paren'
        $is_f =
             $type_p =~ /^[wUG]$/
          || $type_p eq '->'
          || $type_p =~ /^[wi]$/ && $token_p =~ /^(\&|->)/;

        # either keyword or function call?
        $is_w = $is_k || $is_f;
    }

    my $match;
    if    ( $flag eq 'k' ) { $match = $is_k }
    elsif ( $flag eq 'K' ) { $match = !$is_k }
    elsif ( $flag eq 'f' ) { $match = $is_f }
    elsif ( $flag eq 'F' ) { $match = !$is_f }
    elsif ( $flag eq 'w' ) { $match = $is_w }
    elsif ( $flag eq 'W' ) { $match = !$is_w }
    return $match;
}

sub weld_nested_containers {
    my ($self) = @_;

    # Called once per file for option '--weld-nested-containers'

    my $rweld_len_left_closing  = $self->[_rweld_len_left_closing_];
    my $rweld_len_left_opening  = $self->[_rweld_len_left_opening_];
    my $rweld_len_right_closing = $self->[_rweld_len_right_closing_];
    my $rweld_len_right_opening = $self->[_rweld_len_right_opening_];

    # This routine implements the -wn flag by "welding together"
    # the nested closing and opening tokens which were previously
    # identified by sub 'find_nested_pairs'.  "welding" simply
    # involves setting certain hash values which will be checked
    # later during formatting.

    my $rLL                 = $self->[_rLL_];
    my $rlines              = $self->[_rlines_];
    my $K_opening_container = $self->[_K_opening_container_];
    my $K_closing_container = $self->[_K_closing_container_];

    # Find nested pairs of container tokens for any welding.
    my $rnested_pairs = $self->find_nested_pairs();

    # Return unless there are nested pairs to weld
    return unless defined($rnested_pairs) && @{$rnested_pairs};

    # This array will hold the sequence numbers of the tokens to be welded.
    my @welds;

    # Variables needed for estimating line lengths
    my $starting_indent;
    my $starting_lentot;

    # A tolerance to the length for length estimates.  In some rare cases
    # this can avoid problems where a final weld slightly exceeds the
    # line length and gets broken in a bad spot.
    my $length_tol = 1;

    my $excess_length_to_K = sub {
        my ($K) = @_;

        # Estimate the length from the line start to a given token
        my $length = $self->cumulative_length_before_K($K) - $starting_lentot;
        my $excess_length =
          $starting_indent + $length + $length_tol - $rOpts_maximum_line_length;
        return ($excess_length);
    };

    my $length_to_opening_seqno = sub {
        my ($seqno) = @_;
        my $KK      = $K_opening_container->{$seqno};
        my $lentot  = defined($KK)
          && $KK > 0 ? $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_] : 0;
        return $lentot;
    };

    my $length_to_closing_seqno = sub {
        my ($seqno) = @_;
        my $KK      = $K_closing_container->{$seqno};
        my $lentot  = defined($KK)
          && $KK > 0 ? $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_] : 0;
        return $lentot;
    };

    # Abbreviations:
    #  _oo=outer opening, i.e. first of  { {
    #  _io=inner opening, i.e. second of { {
    #  _oc=outer closing, i.e. second of } {
    #  _ic=inner closing, i.e. first of  } }

    my $previous_pair;

    # Main loop over nested pairs...
    # We are working from outermost to innermost pairs so that
    # level changes will be complete when we arrive at the inner pairs.
    while ( my $item = pop( @{$rnested_pairs} ) ) {
        my ( $inner_seqno, $outer_seqno ) = @{$item};

        my $Kouter_opening = $K_opening_container->{$outer_seqno};
        my $Kinner_opening = $K_opening_container->{$inner_seqno};
        my $Kouter_closing = $K_closing_container->{$outer_seqno};
        my $Kinner_closing = $K_closing_container->{$inner_seqno};

        my $outer_opening = $rLL->[$Kouter_opening];
        my $inner_opening = $rLL->[$Kinner_opening];
        my $outer_closing = $rLL->[$Kouter_closing];
        my $inner_closing = $rLL->[$Kinner_closing];

        my $iline_oo = $outer_opening->[_LINE_INDEX_];
        my $iline_io = $inner_opening->[_LINE_INDEX_];
        my $iline_ic = $inner_closing->[_LINE_INDEX_];

        # Set flag saying if this pair starts a new weld
        my $starting_new_weld = !( @welds && $outer_seqno == $welds[-1]->[0] );

        # Set flag saying if this pair is adjacent to the previous nesting pair
        # (even if previous pair was rejected as a weld)
        my $touch_previous_pair =
          defined($previous_pair) && $outer_seqno == $previous_pair->[0];
        $previous_pair = $item;

        # Set a flag if we should not weld. It sometimes looks best not to weld
        # when the opening and closing tokens are very close.  However, there
        # is a danger that we will create a "blinker", which oscillates between
        # two semi-stable states, if we do not weld.  So the rules for
        # not welding have to be carefully defined and tested.
        my $do_not_weld;
        if ( !$touch_previous_pair ) {

            # If this pair is not adjacent to the previous pair (skipped or
            # not), then measure lengths from the start of line of oo

            my $rK_range = $rlines->[$iline_oo]->{_rK_range};
            my ( $Kfirst, $Klast ) = @{$rK_range};
            $starting_lentot =
              $Kfirst <= 0 ? 0 : $rLL->[ $Kfirst - 1 ]->[_CUMULATIVE_LENGTH_];
            $starting_indent = 0;
            if ( !$rOpts_variable_maximum_line_length ) {
                my $level = $rLL->[$Kfirst]->[_LEVEL_];
                $starting_indent = $rOpts_indent_columns * $level;
            }

            # DO-NOT-WELD RULE 1:
            # Do not weld something that looks like the start of a two-line
            # function call, like this: <<snippets/wn6.in>>
            #    $trans->add_transformation(
            #        PDL::Graphics::TriD::Scale->new( $sx, $sy, $sz ) );
            # We will look for a semicolon after the closing paren.

            # We want to weld something complex, like this though
            # my $compass = uc( opposite_direction( line_to_canvas_direction(
            #     @{ $coords[0] }, @{ $coords[1] } ) ) );
            # Otherwise we will get a 'blinker'. For example, the following
            # would become a blinker without this rule:
            #        $Self->_Add( $SortOrderDisplay{ $Field
            #              ->GenerateFieldForSelectSQL() } );
            # But it is okay to weld a two-line statement if it looks like
            # it was already welded, meaning that the two opening containers are
            # on a different line that the two closing containers.  This is
            # necessary to prevent blinking of something like this with
            # perltidy -wn -pbp (starting indentation two levels deep):

            # $top_label->set_text( gettext(
            #    "Unable to create personal directory - check permissions.") );

            my $iline_oc = $outer_closing->[_LINE_INDEX_];
            my $token_oo = $outer_opening->[_TOKEN_];
            if (   $iline_oc == $iline_oo + 1
                && $iline_io == $iline_ic
                && $token_oo eq '(' )
            {

                # Look for following semicolon...
                my $Knext_nonblank = $self->K_next_nonblank($Kouter_closing);
                my $next_nonblank_type =
                  defined($Knext_nonblank)
                  ? $rLL->[$Knext_nonblank]->[_TYPE_]
                  : 'b';
                if ( $next_nonblank_type eq ';' ) {

                    # Then do not weld if no other containers between inner
                    # opening and closing.
                    my $Knext_seq_item = $inner_opening->[_KNEXT_SEQ_ITEM_];
                    if ( $Knext_seq_item == $Kinner_closing ) {
                        $do_not_weld ||= 1;
                    }
                }
            }
        }

        # DO-NOT-WELD RULE 2:
        # Do not weld an opening paren to an inner one line brace block
        # We will just use old line numbers for this test and require
        # iterations if necessary for convergence

        # For example, otherwise we could cause the opening paren
        # in the following example to separate from the caller name
        # as here:

        #    $_[0]->code_handler
        #     	( sub { $more .= $_[1] . ":" . $_[0] . "\n" } );

        # Here is another example where we do not want to weld:
        #  $wrapped->add_around_modifier(
        #    sub { push @tracelog => 'around 1'; $_[0]->(); } );

        # If the one line sub block gets broken due to length or by the
        # user, then we can weld.  The result will then be:
        # $wrapped->add_around_modifier( sub {
        #    push @tracelog => 'around 1';
        #    $_[0]->();
        # } );

        if ( $iline_ic == $iline_io ) {

            my $token_oo = $outer_opening->[_TOKEN_];
            $do_not_weld ||= $token_oo eq '(';
        }

        # DO-NOT-WELD RULE 3:
        # Do not weld if this makes our line too long
        $do_not_weld ||= $excess_length_to_K->($Kinner_opening) > 0;

        # DO-NOT-WELD RULE 4; implemented for git#10:
        # Do not weld an opening -ce brace if the next container is on a single
        # line, different from the opening brace. (This is very rare).  For
        # example, given the following with -ce, we will avoid joining the {
        # and [

        #  } else {
        #      [ $_, length($_) ]
        #  }

        # because this would produce a terminal one-line block:

        #  } else { [ $_, length($_) ]  }

        # which may not be what is desired. But given this input:

        #  } else { [ $_, length($_) ]  }

        # then we will do the weld and retain the one-line block
        if ( $rOpts->{'cuddled-else'} ) {
            my $block_type = $rLL->[$Kouter_opening]->[_BLOCK_TYPE_];
            if ( $block_type && $rcuddled_block_types->{'*'}->{$block_type} ) {
                my $io_line = $inner_opening->[_LINE_INDEX_];
                my $ic_line = $inner_closing->[_LINE_INDEX_];
                my $oo_line = $outer_opening->[_LINE_INDEX_];
                $do_not_weld ||=
                  ( $oo_line < $io_line && $ic_line == $io_line );
            }
        }

        # DO-NOT-WELD RULE 5: do not include welds excluded by user
        if ( !$do_not_weld && %weld_nested_exclusion_rules ) {
            $do_not_weld ||=
              $self->is_excluded_weld( $Kouter_opening, $starting_new_weld );
            $do_not_weld ||= $self->is_excluded_weld( $Kinner_opening, 0 );
        }

        if ($do_not_weld) {

            # After neglecting a pair, we start measuring from start of point io
            $starting_lentot =
              $self->cumulative_length_before_K($Kinner_opening);
            $starting_indent = 0;
            if ( !$rOpts_variable_maximum_line_length ) {
                my $level = $inner_opening->[_LEVEL_];
                $starting_indent = $rOpts_indent_columns * $level;
            }

            # Normally, a broken pair should not decrease indentation of
            # intermediate tokens:
            ##      if ( $last_pair_broken ) { next }
            # However, for long strings of welded tokens, such as '{{{{{{...'
            # we will allow broken pairs to also remove indentation.
            # This will keep very long strings of opening and closing
            # braces from marching off to the right.  We will do this if the
            # number of tokens in a weld before the broken weld is 4 or more.
            # This rule will mainly be needed for test scripts, since typical
            # welds have fewer than about 4 welded tokens.
            if ( !@welds || @{ $welds[-1] } < 4 ) { next }
        }

        # otherwise start new weld ...
        elsif ($starting_new_weld) {
            push @welds, $item;
        }

        # ... or extend current weld
        else {
            unshift @{ $welds[-1] }, $inner_seqno;
        }

        # After welding, reduce the indentation level if all intermediate tokens
        my $dlevel = $outer_opening->[_LEVEL_] - $inner_opening->[_LEVEL_];
        if ( $dlevel != 0 ) {
            my $Kstart = $Kinner_opening;
            my $Kstop  = $Kinner_closing;
            for ( my $KK = $Kstart ; $KK <= $Kstop ; $KK++ ) {
                $rLL->[$KK]->[_LEVEL_] += $dlevel;
            }
        }
    }

    # Define weld lengths needed later to set line breaks
    foreach my $item (@welds) {

        # sweep from inner to outer

        my $inner_seqno;
        my $len_close = 0;
        my $len_open  = 0;
        foreach my $outer_seqno ( @{$item} ) {
            if ($inner_seqno) {

                my $dlen_opening =
                  $length_to_opening_seqno->($inner_seqno) -
                  $length_to_opening_seqno->($outer_seqno);

                my $dlen_closing =
                  $length_to_closing_seqno->($outer_seqno) -
                  $length_to_closing_seqno->($inner_seqno);

                $len_open  += $dlen_opening;
                $len_close += $dlen_closing;

            }

            $rweld_len_left_closing->{$outer_seqno}  = $len_close;
            $rweld_len_right_opening->{$outer_seqno} = $len_open;

            $inner_seqno = $outer_seqno;
        }

        # sweep from outer to inner
        foreach my $seqno ( reverse @{$item} ) {
            $rweld_len_right_closing->{$seqno} =
              $len_close - $rweld_len_left_closing->{$seqno};
            $rweld_len_left_opening->{$seqno} =
              $len_open - $rweld_len_right_opening->{$seqno};
        }
    }

    #####################################
    # DEBUG
    #####################################
    if (0) {
        my $count = 0;
        local $" = ')(';
        foreach my $weld (@welds) {
            print "\nWeld number $count has seq: (@{$weld})\n";
            foreach my $seq ( @{$weld} ) {
                print <<EOM;
	seq=$seq
        left_opening=$rweld_len_left_opening->{$seq};
        right_opening=$rweld_len_right_opening->{$seq};
        left_closing=$rweld_len_left_closing->{$seq};
        right_closing=$rweld_len_right_closing->{$seq};
EOM
            }

            $count++;
        }
    }
    return;
}

sub weld_nested_quotes {

    # Called once per file for option '--weld-nested-containers'. This
    # does welding on qw quotes.

    my $self = shift;

    # See if quotes are excluded from welding
    my $rflags = $weld_nested_exclusion_rules{'q'};
    return if ( defined($rflags) && defined( $rflags->[1] ) );

    my $rweld_len_left_closing  = $self->[_rweld_len_left_closing_];
    my $rweld_len_right_opening = $self->[_rweld_len_right_opening_];

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );
    my $Num = @{$rLL};

    my $K_opening_container = $self->[_K_opening_container_];
    my $K_closing_container = $self->[_K_closing_container_];
    my $rlines              = $self->[_rlines_];

    my $is_single_quote = sub {
        my ( $Kbeg, $Kend, $quote_type ) = @_;
        foreach my $K ( $Kbeg .. $Kend ) {
            my $test_type = $rLL->[$K]->[_TYPE_];
            next   if ( $test_type eq 'b' );
            return if ( $test_type ne $quote_type );
        }
        return 1;
    };

    my $excess_line_length_K = sub {
        my ( $KK, $Ktest ) = @_;

        # what is the excess length if we add token $Ktest to the line with $KK?
        my $iline    = $rLL->[$KK]->[_LINE_INDEX_];
        my $rK_range = $rlines->[$iline]->{_rK_range};
        my ( $Kfirst, $Klast ) = @{$rK_range};
        my $starting_lentot =
          $Kfirst <= 0 ? 0 : $rLL->[ $Kfirst - 1 ]->[_CUMULATIVE_LENGTH_];
        my $starting_indent = 0;
        my $length_tol      = 1;
        if ( !$rOpts_variable_maximum_line_length ) {
            my $level = $rLL->[$Kfirst]->[_LEVEL_];
            $starting_indent = $rOpts_indent_columns * $level;
        }

        my $length = $rLL->[$Ktest]->[_CUMULATIVE_LENGTH_] - $starting_lentot;
        my $excess_length =
          $starting_indent + $length + $length_tol - $rOpts_maximum_line_length;
        return $excess_length;
    };

    # look for single qw quotes nested in containers
    my $KNEXT = $self->[_K_first_seq_item_];
    while ( defined($KNEXT) ) {
        my $KK = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];
        my $rtoken_vars = $rLL->[$KK];
        my $outer_seqno = $rtoken_vars->[_TYPE_SEQUENCE_];
        if ( !$outer_seqno ) {
            next if ( $KK == 0 );    # first token in file may not be container

            # A fault here implies that an error was made in the little loop at
            # the bottom of sub 'respace_tokens' which set the values of
            # _KNEXT_SEQ_ITEM_.  Or an error has been introduced in the
            # loop control lines above.
            Fault("sequence = $outer_seqno not defined at K=$KK");
        }

        my $token = $rtoken_vars->[_TOKEN_];
        if ( $is_opening_token{$token} ) {

            # see if the next token is a quote of some type
            my $Kn = $KK + 1;
            $Kn += 1
              if ( $Kn < $Num && $rLL->[$Kn]->[_TYPE_] eq 'b' );
            next unless ( $Kn < $Num );

            my $next_token = $rLL->[$Kn]->[_TOKEN_];
            my $next_type  = $rLL->[$Kn]->[_TYPE_];
            next
              unless ( ( $next_type eq 'q' || $next_type eq 'Q' )
                && $next_token =~ /^q/ );

            # The token before the closing container must also be a quote
            my $K_closing = $K_closing_container->{$outer_seqno};
            my $Kt_end    = $self->K_previous_nonblank($K_closing);
            next unless $rLL->[$Kt_end]->[_TYPE_] eq $next_type;

            # Do not weld to single-line quotes. Nothing is gained, and it may
            # look bad.
            next if ( $Kt_end == $Kn );

            # Only weld to quotes delimited with container tokens. This is
            # because welding to arbitrary quote delimiters can produce code
            # which is less readable than without welding.
            my $closing_delimiter = substr( $rLL->[$Kt_end]->[_TOKEN_], -1, 1 );
            next
              unless ( $is_closing_token{$closing_delimiter}
                || $closing_delimiter eq '>' );

            # Now make sure that there is just a single quote in the container
            next
              unless ( $is_single_quote->( $Kn + 1, $Kt_end - 1, $next_type ) );

            # If welded, the line must not exceed allowed line length
            # Assume old line breaks for this estimate.
            next if ( $excess_line_length_K->( $KK, $Kn ) > 0 );

            # Check weld exclusion rules for outer container
            my $is_leading = !$self->[_rweld_len_left_opening_]->{$outer_seqno};
            next if ( $self->is_excluded_weld( $KK, $is_leading ) );

            # OK to weld
            # FIXME: Are these always correct?
            $rweld_len_left_closing->{$outer_seqno}  = 1;
            $rweld_len_right_opening->{$outer_seqno} = 2;

            # Undo one indentation level if an extra level was added to this
            # multiline quote
            my $qw_seqno = $self->[_rstarting_multiline_qw_seqno_by_K_]->{$Kn};
            if (   $qw_seqno
                && $self->[_rmultiline_qw_has_extra_level_]->{$qw_seqno} )
            {
                foreach my $K ( $Kn + 1 .. $Kt_end - 1 ) {
                    $rLL->[$K]->[_LEVEL_] -= 1;
                }
                $rLL->[$Kn]->[_CI_LEVEL_]     = 0;
                $rLL->[$Kt_end]->[_CI_LEVEL_] = 0;
            }

            # undo CI for other welded quotes
            else {

                foreach my $K ( $Kn .. $Kt_end ) {
                    $rLL->[$K]->[_CI_LEVEL_] = 0;
                }
            }

            # Change the level of a closing qw token to be that of the outer
            # containing token. This will allow -lp indentation to function
            # correctly in the vertical aligner.
            $rLL->[$Kt_end]->[_LEVEL_] = $rLL->[$K_closing]->[_LEVEL_];
        }
    }
    return;
}

sub weld_len_left {

    my ( $self, $seqno, $type_or_tok ) = @_;

    # Given the sequence number of a token, and the token or its type,
    # return the length of any weld to its left

    # quick check
    return 0
      unless ( $total_weld_count
        && $seqno
        && $self->[_ris_welded_seqno_]->{$seqno} );

    my $weld_len;
    if ( $is_closing_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_left_closing_]->{$seqno};
    }
    elsif ( $is_opening_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_left_opening_]->{$seqno};
    }
    $weld_len = 0 unless ( defined($weld_len) );
    return $weld_len;
}

sub weld_len_right {

    my ( $self, $seqno, $type_or_tok ) = @_;

    # Given the sequence number of a token, and the token or its type,
    # return the length of any weld to its right

    # quick check
    return 0
      unless ( $total_weld_count
        && $seqno
        && $self->[_ris_welded_seqno_]->{$seqno} );

    my $weld_len;
    if ( $is_closing_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_right_closing_]->{$seqno};
    }
    elsif ( $is_opening_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_right_opening_]->{$seqno};
    }
    $weld_len = 0 unless ( defined($weld_len) );
    return $weld_len;
}

sub weld_len_right_to_go {
    my ( $self, $i ) = @_;

    # Given the index of a token in the 'to_go' array return the length of any
    # weld to its right.

    # Back up at a blank.
    return 0 unless ( $total_weld_count && $i >= 0 );
    if ( $i > 0 && $types_to_go[$i] eq 'b' ) { $i-- }

    my $seqno = $type_sequence_to_go[$i];

    return 0 unless ( $seqno && $self->[_ris_welded_seqno_]->{$seqno} );

    my $weld_len;
    my $type_or_tok = $types_to_go[$i];
    if ( $is_closing_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_right_closing_]->{$seqno};
    }
    elsif ( $is_opening_type{$type_or_tok} ) {
        $weld_len = $self->[_rweld_len_right_opening_]->{$seqno};
    }
    $weld_len = 0 unless ( defined($weld_len) );
    return $weld_len;
}

sub mark_short_nested_blocks {

    # This routine looks at the entire file and marks any short nested blocks
    # which should not be broken.  The results are stored in the hash
    #     $rshort_nested->{$type_sequence}
    # which will be true if the container should remain intact.
    #
    # For example, consider the following line:

    #   sub cxt_two { sort { $a <=> $b } test_if_list() }

    # The 'sort' block is short and nested within an outer sub block.
    # Normally, the existance of the 'sort' block will force the sub block to
    # break open, but this is not always desirable. Here we will set a flag for
    # the sort block to prevent this.  To give the user control, we will
    # follow the input file formatting.  If either of the blocks is broken in
    # the input file then we will allow it to remain broken. Otherwise we will
    # set a flag to keep it together in later formatting steps.

    # The flag which is set here will be checked in two places:
    # 'sub process_line_of_CODE' and 'sub starting_one_line_block'

    my $self = shift;
    return if $rOpts->{'indent-only'};

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );

    return unless ( $rOpts->{'one-line-block-nesting'} );

    my $K_opening_container = $self->[_K_opening_container_];
    my $K_closing_container = $self->[_K_closing_container_];
    my $rbreak_container    = $self->[_rbreak_container_];
    my $rshort_nested       = $self->[_rshort_nested_];
    my $rlines              = $self->[_rlines_];

    # Variables needed for estimating line lengths
    my $starting_indent;
    my $starting_lentot;
    my $length_tol = 1;

    my $excess_length_to_K = sub {
        my ($K) = @_;

        # Estimate the length from the line start to a given token
        my $length = $self->cumulative_length_before_K($K) - $starting_lentot;
        my $excess_length =
          $starting_indent + $length + $length_tol - $rOpts_maximum_line_length;
        return ($excess_length);
    };

    my $is_broken_block = sub {

        # a block is broken if the input line numbers of the braces differ
        my ($seqno) = @_;
        my $K_opening = $K_opening_container->{$seqno};
        return unless ( defined($K_opening) );
        my $K_closing = $K_closing_container->{$seqno};
        return unless ( defined($K_closing) );
        return $rbreak_container->{$seqno}
          || $rLL->[$K_closing]->[_LINE_INDEX_] !=
          $rLL->[$K_opening]->[_LINE_INDEX_];
    };

    # loop over all containers
    my @open_block_stack;
    my $iline = -1;
    my $KNEXT = $self->[_K_first_seq_item_];
    while ( defined($KNEXT) ) {
        my $KK = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];
        my $rtoken_vars   = $rLL->[$KK];
        my $type_sequence = $rtoken_vars->[_TYPE_SEQUENCE_];
        if ( !$type_sequence ) {
            next if ( $KK == 0 );    # first token in file may not be container

            # A fault here implies that an error was made in the little loop at
            # the bottom of sub 'respace_tokens' which set the values of
            # _KNEXT_SEQ_ITEM_.  Or an error has been introduced in the
            # loop control lines above.
            Fault("sequence = $type_sequence not defined at K=$KK");
        }

        # We are just looking at code blocks
        my $token = $rtoken_vars->[_TOKEN_];
        my $type  = $rtoken_vars->[_TYPE_];
        next unless ( $type eq $token );
        my $block_type = $rtoken_vars->[_BLOCK_TYPE_];
        next unless ($block_type);

        # Keep a stack of all acceptable block braces seen.
        # Only consider blocks entirely on one line so dump the stack when line
        # changes.
        my $iline_last = $iline;
        $iline = $rLL->[$KK]->[_LINE_INDEX_];
        if ( $iline != $iline_last ) { @open_block_stack = () }

        if ( $token eq '}' ) {
            if (@open_block_stack) { pop @open_block_stack }
        }
        next unless ( $token eq '{' );

        # block must be balanced (bad scripts may be unbalanced)
        my $K_opening = $K_opening_container->{$type_sequence};
        my $K_closing = $K_closing_container->{$type_sequence};
        next unless ( defined($K_opening) && defined($K_closing) );

        # require that this block be entirely on one line
        next if ( $is_broken_block->($type_sequence) );

        # See if this block fits on one line of allowed length (which may
        # be different from the input script)
        $starting_lentot =
          $KK <= 0 ? 0 : $rLL->[ $KK - 1 ]->[_CUMULATIVE_LENGTH_];
        $starting_indent = 0;
        if ( !$rOpts_variable_maximum_line_length ) {
            my $level = $rLL->[$KK]->[_LEVEL_];
            $starting_indent = $rOpts_indent_columns * $level;
        }

        # Dump the stack if block is too long and skip this block
        if ( $excess_length_to_K->($K_closing) > 0 ) {
            @open_block_stack = ();
            next;
        }

        # OK, Block passes tests, remember it
        push @open_block_stack, $type_sequence;

        # We are only marking nested code blocks,
        # so check for a previous block on the stack
        next unless ( @open_block_stack > 1 );

        # Looks OK, mark this as a short nested block
        $rshort_nested->{$type_sequence} = 1;

    }
    return;
}

sub adjust_indentation_levels {

    my ($self) = @_;

    # Called once per file to do special indentation adjustments.
    # These routines adjust levels either by changing _CI_LEVEL_ directly or
    # by setting modified levels in the array $self->[_radjusted_levels_].

    # Initialize the adjusted levels. These will be the levels actually used
    # for computing indentation.

    # NOTE: This routine is called after the weld routines, which may have
    # already adjusted _LEVEL_, so we are making adjustments on top of those
    # levels.  It would be much nicer to have the weld routines also use this
    # adjustment, but that gets complicated when we combine -gnu -wn and have
    # some welded quotes.
    my $radjusted_levels = $self->[_radjusted_levels_];
    my $rLL              = $self->[_rLL_];
    foreach my $KK ( 0 .. @{$rLL} - 1 ) {
        $radjusted_levels->[$KK] = $rLL->[$KK]->[_LEVEL_];
    }

    # First set adjusted levels for any non-indenting braces.
    $self->non_indenting_braces();

    # Adjust indentation for list containers
    $self->adjust_container_indentation();

    # Set adjusted levels for the whitespace cycle option.
    $self->whitespace_cycle_adjustment();

    # Adjust continuation indentation if -bli is set
    $self->bli_adjustment();

    $self->extended_ci()
      if ( $rOpts->{'extended-continuation-indentation'} );

    # Now clip any adjusted levels to be non-negative
    $self->clip_adjusted_levels();

    return;
}

sub clip_adjusted_levels {

    # Replace any negative adjusted levels with zero.
    # Negative levels can occur in files with brace errors.
    my ($self) = @_;
    my $radjusted_levels = $self->[_radjusted_levels_];
    return unless defined($radjusted_levels) && @{$radjusted_levels};
    foreach ( @{$radjusted_levels} ) { $_ = 0 if ( $_ < 0 ) }
    return;
}

sub non_indenting_braces {

    # Called once per file to handle the --non-indenting-braces parameter.
    # Remove indentation within marked braces if requested
    my ($self) = @_;
    return unless ( $rOpts->{'non-indenting-braces'} );

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );

    my $rspecial_side_comment_type = $self->[_rspecial_side_comment_type_];

    my $radjusted_levels = $self->[_radjusted_levels_];
    my $Kmax             = @{$rLL} - 1;
    my @seqno_stack;

    my $is_non_indenting_brace = sub {
        my ($KK) = @_;

        # looking for an opening block brace
        my $token      = $rLL->[$KK]->[_TOKEN_];
        my $block_type = $rLL->[$KK]->[_BLOCK_TYPE_];
        return unless ( $token eq '{' && $block_type );

        # followed by a comment
        my $K_sc = $KK + 1;
        $K_sc += 1
          if ( $K_sc <= $Kmax && $rLL->[$K_sc]->[_TYPE_] eq 'b' );
        return unless ( $K_sc <= $Kmax );
        my $type_sc = $rLL->[$K_sc]->[_TYPE_];
        return unless ( $type_sc eq '#' );

        # on the same line
        my $line_index    = $rLL->[$KK]->[_LINE_INDEX_];
        my $line_index_sc = $rLL->[$K_sc]->[_LINE_INDEX_];
        return unless ( $line_index_sc == $line_index );

        # get the side comment text
        my $token_sc = $rLL->[$K_sc]->[_TOKEN_];

        # The pattern ends in \s but we have removed the newline, so
        # we added it back for the match. That way we require an exact
        # match to the special string and also allow additional text.
        $token_sc .= "\n";
        my $is_nib = ( $token_sc =~ /$non_indenting_brace_pattern/ );
        if ($is_nib) { $rspecial_side_comment_type->{$K_sc} = 'NIB' }
        return $is_nib;
    };

    foreach my $KK ( 0 .. $Kmax ) {
        my $num   = @seqno_stack;
        my $seqno = $rLL->[$KK]->[_TYPE_SEQUENCE_];
        if ($seqno) {
            my $token = $rLL->[$KK]->[_TOKEN_];
            if ( $token eq '{' && $is_non_indenting_brace->($KK) ) {
                push @seqno_stack, $seqno;
            }
            if ( $token eq '}' && @seqno_stack && $seqno_stack[-1] == $seqno ) {
                pop @seqno_stack;
                $num -= 1;
            }
        }
        next unless $num;
        $radjusted_levels->[$KK] -= $num;
    }
    return;
}

sub whitespace_cycle_adjustment {

    my $self = shift;

    # Called once per file to implement the --whitespace-cycle option
    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );
    my $radjusted_levels = $self->[_radjusted_levels_];

    my $rOpts_whitespace_cycle = $rOpts->{'whitespace-cycle'};
    if ( $rOpts_whitespace_cycle && $rOpts_whitespace_cycle > 0 ) {

        my $Kmax = @{$rLL} - 1;

        my $whitespace_last_level  = -1;
        my @whitespace_level_stack = ();
        my $last_nonblank_type     = 'b';
        my $last_nonblank_token    = '';
        foreach my $KK ( 0 .. $Kmax ) {
            my $level_abs = $radjusted_levels->[$KK];
            my $level     = $level_abs;
            if ( $level_abs < $whitespace_last_level ) {
                pop(@whitespace_level_stack);
            }
            if ( !@whitespace_level_stack ) {
                push @whitespace_level_stack, $level_abs;
            }
            elsif ( $level_abs > $whitespace_last_level ) {
                $level = $whitespace_level_stack[-1] +
                  ( $level_abs - $whitespace_last_level );

                if (
                    # 1 Try to break at a block brace
                    (
                           $level > $rOpts_whitespace_cycle
                        && $last_nonblank_type eq '{'
                        && $last_nonblank_token eq '{'
                    )

                    # 2 Then either a brace or bracket
                    || (   $level > $rOpts_whitespace_cycle + 1
                        && $last_nonblank_token =~ /^[\{\[]$/ )

                    # 3 Then a paren too
                    || $level > $rOpts_whitespace_cycle + 2
                  )
                {
                    $level = 1;
                }
                push @whitespace_level_stack, $level;
            }
            $level = $whitespace_level_stack[-1];
            $radjusted_levels->[$KK] = $level;

            $whitespace_last_level = $level_abs;
            my $type  = $rLL->[$KK]->[_TYPE_];
            my $token = $rLL->[$KK]->[_TOKEN_];
            if ( $type ne 'b' ) {
                $last_nonblank_type  = $type;
                $last_nonblank_token = $token;
            }
        }
    }
    return;
}

sub adjust_container_indentation {

    # Called once per file to implement the -bbhb* and related flags:

    # -bbhbi=n
    # -bbsbi=n
    # -bbpi=n

    # where:

    # n=0  default indentation (usually one ci)
    # n=1  outdent one ci
    # n=2  indent one level (minus one ci)
    # n=3  indent one extra ci [This may be dropped]

    my ($self) = @_;

    return unless %container_indentation_options;

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );

    # Option 2 needs the following array:
    my $radjusted_levels = $self->[_radjusted_levels_];

    # Loop over all opening container tokens
    my $K_opening_container  = $self->[_K_opening_container_];
    my $ris_broken_container = $self->[_ris_broken_container_];
    foreach my $seqno ( keys %{$K_opening_container} ) {
        my $KK = $K_opening_container->{$seqno};

        # this routine is not for code block braces
        my $block_type = $rLL->[$KK]->[_BLOCK_TYPE_];
        next if ($block_type);

        # These flags only apply if the corresponding -bb* flags
        # have been set to non-default values
        my $rtoken_vars = $rLL->[$KK];
        my $token       = $rtoken_vars->[_TOKEN_];
        my $flag        = $container_indentation_options{$token};
        next unless ($flag);

        # Require previous nonblank to be certain types (= and =>)
        # Note similar coding in sub insert_breaks_before...
        my $Kprev = $KK - 1;
        next if ( $Kprev < 0 );
        my $prev_type = $rLL->[$Kprev]->[_TYPE_];
        if ( $prev_type eq 'b' ) {
            $Kprev--;
            next if ( $Kprev < 0 );
            $prev_type = $rLL->[$Kprev]->[_TYPE_];
        }
        next unless ( $is_equal_or_fat_comma{$prev_type} );

        # This is only for list containers
        next unless $self->is_list_by_seqno($seqno);

        # and only for broken lists
        next unless $ris_broken_container->{$seqno};

        # NOTE: We are adjusting indentation of the opening container. The
        # closing container will normally follow the indentation of the opening
        # container automatically, so this is not currently done.
        my $ci = $rLL->[$KK]->[_CI_LEVEL_];
        next unless ($ci);

        # option 1: outdent
        if ( $flag == 1 ) {
            $ci -= 1;
        }

        # option 2: indent one level
        elsif ( $flag == 2 ) {
            $ci -= 1;
            $radjusted_levels->[$KK] += 1;
        }

        # option 3: for testing only, probably will be deleted
        elsif ( $flag == 3 ) {
            $ci += 1;
        }
        $rLL->[$KK]->[_CI_LEVEL_] = $ci if ( $ci >= 0 );
    }
    return;
}

sub extended_ci {

    # This routine implements the -xci (--extended-continuation-indentation)
    # flag.  We add CI to interior tokens of a container which itself has CI but
    # only if a token does not already have CI.

    # To do this, we will locate opening tokens which themselves have
    # continuation indentation (CI).  We track them with their sequence
    # numbers.  These sequence numbers are called 'controlling sequence
    # numbers'.  They apply continuation indentation to the tokens that they
    # contain.  These inner tokens remember their controlling sequence numbers.
    # Later, when these inner tokens are output, they have to see if the output
    # lines with their controlling tokens were output with CI or not.  If not,
    # then they must remove their CI too.

    # The controlling CI concept works hierarchically.  But CI itself is not
    # hierarchical; it is either on or off. There are some rare instances where
    # it would be best to have hierarchical CI too, but not enough to be worth
    # the programming effort.

    # The operations to remove unwanted CI are done in sub 'undo_ci'.

    my ($self) = @_;

    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );

    my $ris_seqno_controlling_ci = $self->[_ris_seqno_controlling_ci_];
    my $rseqno_controlling_my_ci = $self->[_rseqno_controlling_my_ci_];

    # Loop over all opening container tokens
    my $K_opening_container  = $self->[_K_opening_container_];
    my $K_closing_container  = $self->[_K_closing_container_];
    my $ris_broken_container = $self->[_ris_broken_container_];
    my @seqno_stack;
    my $seqno_top;
    my $KLAST;
    my $KNEXT = $self->[_K_first_seq_item_];

    while ( defined($KNEXT) ) {

        # Fix all tokens up to the next sequence item if we are changing CI
        if ($seqno_top) {
            my $count = 0;
            for ( my $Kt = $KLAST + 1 ; $Kt < $KNEXT ; $Kt++ ) {
                if ( !$rLL->[$Kt]->[_CI_LEVEL_] ) {
                    $rLL->[$Kt]->[_CI_LEVEL_] = 1;
                    $rseqno_controlling_my_ci->{$Kt} = $seqno_top;
                    $count++;
                }
            }
            $ris_seqno_controlling_ci->{$seqno_top} += $count;
        }

        $KLAST = $KNEXT;
        my $KK = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];

        my $seqno     = $rLL->[$KK]->[_TYPE_SEQUENCE_];
        my $K_opening = $K_opening_container->{$seqno};

        # see if we have reached the end of the current controlling container
        if ( $seqno_top && $seqno == $seqno_top ) {
            $seqno_top = pop @seqno_stack;
        }

        # Patch to fix some block types...
        # Certain block types arrive from the tokenizer without CI but should
        # have it for this option.  These include anonymous subs and
        #     do sort map grep eval
        my $block_type = $rLL->[$KK]->[_BLOCK_TYPE_];
        if ( $block_type && $is_block_with_ci{$block_type} ) {
            $rLL->[$KK]->[_CI_LEVEL_] = 1;
            if ($seqno_top) {
                $rseqno_controlling_my_ci->{$KK} = $seqno_top;
                $ris_seqno_controlling_ci->{$seqno_top}++;
            }
        }

        # If this does not have ci, update ci if necessary and continue looking
        if ( !$rLL->[$KK]->[_CI_LEVEL_] ) {
            if ($seqno_top) {
                $rLL->[$KK]->[_CI_LEVEL_] = 1;
                $rseqno_controlling_my_ci->{$KK} = $seqno_top;
                $ris_seqno_controlling_ci->{$seqno_top}++;
            }
            next;
        }

        # We are looking for opening container tokens with ci
        next unless ( defined($K_opening) && $KK == $K_opening );

        # Make sure there is a corresponding closing container
        # (could be missing if the script has a brace error)
        my $K_closing = $K_closing_container->{$seqno};
        next unless defined($K_closing);

        # Require different input lines. This will filter out a large number
        # of small hash braces and array brackets.  If we accidentally filter
        # out an important container, it will get fixed on the next pass.
        next
          if (
            $rLL->[$K_opening]->[_LINE_INDEX_] ==
            $rLL->[$K_closing]->[_LINE_INDEX_]
            && ( $rLL->[$K_closing]->[_CUMULATIVE_LENGTH_] -
                $rLL->[$K_opening]->[_CUMULATIVE_LENGTH_] >
                $rOpts_maximum_line_length )
          );

        # This becomes the next controlling container
        push @seqno_stack, $seqno_top if ($seqno_top);
        $seqno_top = $seqno;
    }
    return;
}

sub bli_adjustment {

    # Called once per file to implement the --brace-left-and-indent option.
    # If -bli is set, adds one continuation indentation for certain braces
    my $self = shift;
    return unless ( $rOpts->{'brace-left-and-indent'} );
    my $rLL = $self->[_rLL_];
    return unless ( defined($rLL) && @{$rLL} );
    my $ris_bli_container   = $self->[_ris_bli_container_];
    my $K_opening_container = $self->[_K_opening_container_];
    my $KNEXT               = $self->[_K_first_seq_item_];

    while ( defined($KNEXT) ) {
        my $KK = $KNEXT;
        $KNEXT = $rLL->[$KNEXT]->[_KNEXT_SEQ_ITEM_];
        my $block_type = $rLL->[$KK]->[_BLOCK_TYPE_];
        if ( $block_type && $block_type =~ /$bli_pattern/ ) {
            my $seqno     = $rLL->[$KK]->[_TYPE_SEQUENCE_];
            my $K_opening = $K_opening_container->{$seqno};
            if ( defined($K_opening) ) {
                if ( $KK eq $K_opening ) {
                    $rLL->[$KK]->[_CI_LEVEL_]++;
                    $ris_bli_container->{$seqno} = 1;
                }
                else {
                    $rLL->[$KK]->[_CI_LEVEL_] =
                      $rLL->[$K_opening]->[_CI_LEVEL_];
                }
            }
        }
    }
    return;
}

sub find_multiline_qw {

    my $self = shift;

    # Multiline qw quotes are not sequenced items like containers { [ (
    # but behave in some respects in a similar way. So this routine finds them
    # and creates a separate sequence number system for later use.

    # This is straightforward because they always begin at the end of one line
    # and and at the beginning of a later line. This is true no matter how we
    # finally make our line breaks, so we can find them before deciding on new
    # line breaks.

    my $rstarting_multiline_qw_seqno_by_K = {};
    my $rending_multiline_qw_seqno_by_K   = {};
    my $rKrange_multiline_qw_by_seqno     = {};
    my $rcontains_multiline_qw_by_seqno   = {};
    my $rmultiline_qw_has_extra_level     = {};

    my $rlines = $self->[_rlines_];
    my $rLL    = $self->[_rLL_];
    my $qw_seqno;
    my $num_qw_seqno = 0;
    my $K_start_multiline_qw;

    foreach my $line_of_tokens ( @{$rlines} ) {

        my $line_type = $line_of_tokens->{_line_type};
        next unless ( $line_type eq 'CODE' );
        my $rK_range = $line_of_tokens->{_rK_range};
        my ( $Kfirst, $Klast ) = @{$rK_range};
        next unless ( defined($Kfirst) && defined($Klast) );   # skip blank line
        if ( defined($K_start_multiline_qw) ) {
            my $type = $rLL->[$Kfirst]->[_TYPE_];

            # shouldn't happen
            if ( $type ne 'q' ) {
                DEVEL_MODE && print STDERR <<EOM;
STRANGE: started multiline qw at K=$K_start_multiline_qw but didn't see q qw at K=$Kfirst\n";
EOM
                $K_start_multiline_qw = undef;
                next;
            }
            my $Kprev  = $self->K_previous_nonblank($Kfirst);
            my $Knext  = $self->K_next_nonblank($Kfirst);
            my $type_m = defined($Kprev) ? $rLL->[$Kprev]->[_TYPE_] : 'b';
            my $type_p = defined($Knext) ? $rLL->[$Knext]->[_TYPE_] : 'b';
            if ( $type_m eq 'q' && $type_p ne 'q' ) {
                $rending_multiline_qw_seqno_by_K->{$Kfirst} = $qw_seqno;
                $rKrange_multiline_qw_by_seqno->{$qw_seqno} =
                  [ $K_start_multiline_qw, $Kfirst ];
                $K_start_multiline_qw = undef;
                $qw_seqno             = undef;
            }
        }
        if ( !defined($K_start_multiline_qw)
            && $rLL->[$Klast]->[_TYPE_] eq 'q' )
        {
            my $Kprev  = $self->K_previous_nonblank($Klast);
            my $Knext  = $self->K_next_nonblank($Klast);
            my $type_m = defined($Kprev) ? $rLL->[$Kprev]->[_TYPE_] : 'b';
            my $type_p = defined($Knext) ? $rLL->[$Knext]->[_TYPE_] : 'b';
            if ( $type_m ne 'q' && $type_p eq 'q' ) {
                $num_qw_seqno++;
                $qw_seqno             = 'q' . $num_qw_seqno;
                $K_start_multiline_qw = $Klast;
                $rstarting_multiline_qw_seqno_by_K->{$Klast} = $qw_seqno;
            }
        }
    }

    # Give multiline qw lists extra indentation instead of CI.  This option
    # works well but is currently only activated when the -xci flag is set.
    # The reason is to avoid unexpected changes in formatting.
    if ( $rOpts->{'extended-continuation-indentation'} ) {
        while ( my ( $qw_seqno, $rKrange ) =
            each %{$rKrange_multiline_qw_by_seqno} )
        {
            my ( $Kbeg, $Kend ) = @{$rKrange};

            # require isolated closing token
            my $token_end = $rLL->[$Kend]->[_TOKEN_];
            next
              unless ( length($token_end) == 1
                && ( $is_closing_token{$token_end} || $token_end eq '>' ) );

            # require isolated opening token
            my $token_beg = $rLL->[$Kbeg]->[_TOKEN_];

            # allow space(s) after the qw
            if ( length($token_beg) > 3 && substr( $token_beg, 2, 1 ) eq ' ' ) {
                $token_beg =~ s/\s+//;
            }

            next unless ( length($token_beg) == 3 );

            foreach my $KK ( $Kbeg + 1 .. $Kend - 1 ) {
                $rLL->[$KK]->[_LEVEL_]++;
                $rLL->[$KK]->[_CI_LEVEL_] = 0;
            }

            # set flag for -wn option, which will remove the level
            $rmultiline_qw_has_extra_level->{$qw_seqno} = 1;
        }
    }

    # For the -lp option we need to mark all parent containers of
    # multiline quotes
    if ($rOpts_line_up_parentheses) {

        while ( my ( $qw_seqno, $rKrange ) =
            each %{$rKrange_multiline_qw_by_seqno} )
        {
            my ( $Kbeg, $Kend ) = @{$rKrange};
            my $parent_seqno = $self->parent_seqno_by_K($Kend);
            next unless ($parent_seqno);

            # If the parent container exactly surrounds this qw, then -lp
            # formatting seems to work so we will not mark it.
            my $is_tightly_contained;
            my $Kn      = $self->K_next_nonblank($Kend);
            my $seqno_n = defined($Kn) ? $rLL->[$Kn]->[_TYPE_SEQUENCE_] : undef;
            if ( defined($seqno_n) && $seqno_n eq $parent_seqno ) {

                my $Kp = $self->K_previous_nonblank($Kbeg);
                my $seqno_p =
                  defined($Kp) ? $rLL->[$Kp]->[_TYPE_SEQUENCE_] : undef;
                if ( defined($seqno_p) && $seqno_p eq $parent_seqno ) {
                    $is_tightly_contained = 1;
                }
            }
            $rcontains_multiline_qw_by_seqno->{$parent_seqno} = 1
              unless ($is_tightly_contained);

            # continue up the tree marking parent containers
            while (1) {
                $parent_seqno = $self->[_rparent_of_seqno_]->{$parent_seqno};
                last
                  unless ( defined($parent_seqno)
                    && $parent_seqno ne SEQ_ROOT );
                $rcontains_multiline_qw_by_seqno->{$parent_seqno} = 1;
            }
        }
    }

    $self->[_rstarting_multiline_qw_seqno_by_K_] =
      $rstarting_multiline_qw_seqno_by_K;
    $self->[_rending_multiline_qw_seqno_by_K_] =
      $rending_multiline_qw_seqno_by_K;
    $self->[_rKrange_multiline_qw_by_seqno_] = $rKrange_multiline_qw_by_seqno;
    $self->[_rcontains_multiline_qw_by_seqno_] =
      $rcontains_multiline_qw_by_seqno;
    $self->[_rmultiline_qw_has_extra_level_] = $rmultiline_qw_has_extra_level;

    return;
}

######################################
# CODE SECTION 6: Process line-by-line
######################################

sub process_all_lines {

    # Main loop over all lines of a file.
    # Lines are processed according to type.

    my $self                       = shift;
    my $rlines                     = $self->[_rlines_];
    my $sink_object                = $self->[_sink_object_];
    my $fh_tee                     = $self->[_fh_tee_];
    my $rOpts_keep_old_blank_lines = $rOpts->{'keep-old-blank-lines'};
    my $file_writer_object         = $self->[_file_writer_object_];
    my $logger_object              = $self->[_logger_object_];
    my $vertical_aligner_object    = $self->[_vertical_aligner_object_];
    my $save_logfile               = $self->[_save_logfile_];

    # Note for RT#118553, leave only one newline at the end of a file.
    # Example code to do this is in comments below:
    # my $Opt_trim_ending_blank_lines = 0;
    # if ($Opt_trim_ending_blank_lines) {
    #     while ( my $line_of_tokens = pop @{$rlines} ) {
    #         my $line_type = $line_of_tokens->{_line_type};
    #         if ( $line_type eq 'CODE' ) {
    #             my $CODE_type = $line_of_tokens->{_code_type};
    #             next if ( $CODE_type eq 'BL' );
    #         }
    #         push @{$rlines}, $line_of_tokens;
    #         last;
    #     }
    # }

   # But while this would be a trivial update, it would have very undesirable
   # side effects when perltidy is run from within an editor on a small snippet.
   # So this is best done with a separate filter, such
   # as 'delete_ending_blank_lines.pl' in the examples folder.

    # Flag to prevent blank lines when POD occurs in a format skipping sect.
    my $in_format_skipping_section;

    # set locations for blanks around long runs of keywords
    my $rwant_blank_line_after = $self->keyword_group_scan();

    my $line_type = "";
    my $i         = -1;
    foreach my $line_of_tokens ( @{$rlines} ) {
        $i++;

        # insert blank lines requested for keyword sequences
        if (   $i > 0
            && defined( $rwant_blank_line_after->{ $i - 1 } )
            && $rwant_blank_line_after->{ $i - 1 } == 1 )
        {
            $self->want_blank_line();
        }

        my $last_line_type = $line_type;
        $line_type = $line_of_tokens->{_line_type};
        my $input_line = $line_of_tokens->{_line_text};

        # _line_type codes are:
        #   SYSTEM         - system-specific code before hash-bang line
        #   CODE           - line of perl code (including comments)
        #   POD_START      - line starting pod, such as '=head'
        #   POD            - pod documentation text
        #   POD_END        - last line of pod section, '=cut'
        #   HERE           - text of here-document
        #   HERE_END       - last line of here-doc (target word)
        #   FORMAT         - format section
        #   FORMAT_END     - last line of format section, '.'
        #   DATA_START     - __DATA__ line
        #   DATA           - unidentified text following __DATA__
        #   END_START      - __END__ line
        #   END            - unidentified text following __END__
        #   ERROR          - we are in big trouble, probably not a perl script

        # put a blank line after an =cut which comes before __END__ and __DATA__
        # (required by podchecker)
        if ( $last_line_type eq 'POD_END' && !$self->[_saw_END_or_DATA_] ) {
            $file_writer_object->reset_consecutive_blank_lines();
            if ( !$in_format_skipping_section && $input_line !~ /^\s*$/ ) {
                $self->want_blank_line();
            }
        }

        # handle line of code..
        if ( $line_type eq 'CODE' ) {

            my $CODE_type = $line_of_tokens->{_code_type};
            $in_format_skipping_section = $CODE_type eq 'FS';

            # Handle blank lines
            if ( $CODE_type eq 'BL' ) {

                # If keep-old-blank-lines is zero, we delete all
                # old blank lines and let the blank line rules generate any
                # needed blanks.

                # and delete lines requested by the keyword-group logic
                my $kgb_keep = !( defined( $rwant_blank_line_after->{$i} )
                    && $rwant_blank_line_after->{$i} == 2 );

                # But: the keep-old-blank-lines flag has priority over kgb flags
                $kgb_keep = 1 if ( $rOpts_keep_old_blank_lines == 2 );

                if ( $rOpts_keep_old_blank_lines && $kgb_keep ) {
                    $self->flush($CODE_type);
                    $file_writer_object->write_blank_code_line(
                        $rOpts_keep_old_blank_lines == 2 );
                    $self->[_last_line_leading_type_] = 'b';
                }
                next;
            }
            else {

          # Let logger see all non-blank lines of code. This is a slow operation
          # so we avoid it if it is not going to be saved.
                if ( $save_logfile && $logger_object ) {
                    $logger_object->black_box( $line_of_tokens,
                        $vertical_aligner_object->get_output_line_number );
                }
            }

            # Handle Format Skipping (FS) and Verbatim (VB) Lines
            if ( $CODE_type eq 'VB' || $CODE_type eq 'FS' ) {
                $self->write_unindented_line("$input_line");
                $file_writer_object->reset_consecutive_blank_lines();
                next;
            }

            # Handle all other lines of code
            $self->process_line_of_CODE($line_of_tokens);
        }

        # handle line of non-code..
        else {

            # set special flags
            my $skip_line = 0;
            if ( substr( $line_type, 0, 3 ) eq 'POD' ) {

                # Pod docs should have a preceding blank line.  But stay
                # out of __END__ and __DATA__ sections, because
                # the user may be using this section for any purpose whatsoever
                if ( $rOpts->{'delete-pod'} ) { $skip_line = 1; }
                if ( $rOpts->{'trim-pod'} )   { $input_line =~ s/\s+$// }
                if (   !$skip_line
                    && !$in_format_skipping_section
                    && $line_type eq 'POD_START'
                    && !$self->[_saw_END_or_DATA_] )
                {
                    $self->want_blank_line();
                }
            }

            # leave the blank counters in a predictable state
            # after __END__ or __DATA__
            elsif ( $line_type eq 'END_START' || $line_type eq 'DATA_START' ) {
                $file_writer_object->reset_consecutive_blank_lines();
                $self->[_saw_END_or_DATA_] = 1;
            }

            # write unindented non-code line
            if ( !$skip_line ) {
                $self->write_unindented_line($input_line);
            }
        }
    }
    return;

} ## end sub process_all_lines

sub keyword_group_scan {
    my $self = shift;

    # Called once per file to process the --keyword-group-blanks-* parameters.

    # Manipulate blank lines around keyword groups (kgb* flags)
    # Scan all lines looking for runs of consecutive lines beginning with
    # selected keywords.  Example keywords are 'my', 'our', 'local', ... but
    # they may be anything.  We will set flags requesting that blanks be
    # inserted around and within them according to input parameters.  Note
    # that we are scanning the lines as they came in in the input stream, so
    # they are not necessarily well formatted.

    # The output of this sub is a return hash ref whose keys are the indexes of
    # lines after which we desire a blank line.  For line index i:
    #     $rhash_of_desires->{$i} = 1 means we want a blank line AFTER line $i
    #     $rhash_of_desires->{$i} = 2 means we want blank line $i removed
    my $rhash_of_desires = {};

    my $Opt_blanks_before = $rOpts->{'keyword-group-blanks-before'};   # '-kgbb'
    my $Opt_blanks_after  = $rOpts->{'keyword-group-blanks-after'};    # '-kgba'
    my $Opt_blanks_inside = $rOpts->{'keyword-group-blanks-inside'};   # '-kgbi'
    my $Opt_blanks_delete = $rOpts->{'keyword-group-blanks-delete'};   # '-kgbd'
    my $Opt_size          = $rOpts->{'keyword-group-blanks-size'};     # '-kgbs'

    # A range of sizes can be input with decimal notation like 'min.max' with
    # any number of dots between the two numbers. Examples:
    #    string    =>    min    max  matches
    #    1.1             1      1    exactly 1
    #    1.3             1      3    1,2, or 3
    #    1..3            1      3    1,2, or 3
    #    5               5      -    5 or more
    #    6.              6      -    6 or more
    #    .2              -      2    up to 2
    #    1.0             1      0    nothing
    my ( $Opt_size_min, $Opt_size_max ) = split /\.+/, $Opt_size;
    if (   $Opt_size_min && $Opt_size_min !~ /^\d+$/
        || $Opt_size_max && $Opt_size_max !~ /^\d+$/ )
    {
        Warn(<<EOM);
Unexpected value for -kgbs: '$Opt_size'; expecting 'min' or 'min.max'; 
ignoring all -kgb flags
EOM

        # Turn this option off so that this message does not keep repeating
        # during iterations and other files.
        $rOpts->{'keyword-group-blanks-size'} = "";
        return $rhash_of_desires;
    }
    $Opt_size_min = 1 unless ($Opt_size_min);

    if ( $Opt_size_max && $Opt_size_max < $Opt_size_min ) {
        return $rhash_of_desires;
    }

    # codes for $Opt_blanks_before and $Opt_blanks_after:
    # 0 = never (delete if exist)
    # 1 = stable (keep unchanged)
    # 2 = always (insert if missing)

    return $rhash_of_desires
      unless $Opt_size_min > 0
      && ( $Opt_blanks_before != 1
        || $Opt_blanks_after != 1
        || $Opt_blanks_inside
        || $Opt_blanks_delete );

    my $Opt_pattern         = $keyword_group_list_pattern;
    my $Opt_comment_pattern = $keyword_group_list_comment_pattern;
    my $Opt_repeat_count =
      $rOpts->{'keyword-group-blanks-repeat-count'};    # '-kgbr'

    my $rlines              = $self->[_rlines_];
    my $rLL                 = $self->[_rLL_];
    my $K_closing_container = $self->[_K_closing_container_];

    # variables for the current group and subgroups:
    my ( $ibeg, $iend, $count, $level_beg, $K_closing, @iblanks, @group,
        @subgroup );

    # Definitions:
    # ($ibeg, $iend) = starting and ending line indexes of this entire group
    #         $count = total number of keywords seen in this entire group
    #     $level_beg = indententation level of this group
    #         @group = [ $i, $token, $count ] =list of all keywords & blanks
    #      @subgroup =  $j, index of group where token changes
    #       @iblanks = line indexes of blank lines in input stream in this group
    #  where i=starting line index
    #        token (the keyword)
    #        count = number of this token in this subgroup
    #            j = index in group where token changes
    #
    # These vars will contain values for the most recently seen line:
    my ( $line_type, $CODE_type, $K_first, $K_last );

    my $number_of_groups_seen = 0;

    ####################
    # helper subroutines
    ####################

    my $insert_blank_after = sub {
        my ($i) = @_;
        $rhash_of_desires->{$i} = 1;
        my $ip = $i + 1;
        if ( defined( $rhash_of_desires->{$ip} )
            && $rhash_of_desires->{$ip} == 2 )
        {
            $rhash_of_desires->{$ip} = 0;
        }
        return;
    };

    my $split_into_sub_groups = sub {

        # place blanks around long sub-groups of keywords
        # ...if requested
        return unless ($Opt_blanks_inside);

        # loop over sub-groups, index k
        push @subgroup, scalar @group;
        my $kbeg = 1;
        my $kend = @subgroup - 1;
        for ( my $k = $kbeg ; $k <= $kend ; $k++ ) {

            # index j runs through all keywords found
            my $j_b = $subgroup[ $k - 1 ];
            my $j_e = $subgroup[$k] - 1;

            # index i is the actual line number of a keyword
            my ( $i_b, $tok_b, $count_b ) = @{ $group[$j_b] };
            my ( $i_e, $tok_e, $count_e ) = @{ $group[$j_e] };
            my $num = $count_e - $count_b + 1;

            # This subgroup runs from line $ib to line $ie-1, but may contain
            # blank lines
            if ( $num >= $Opt_size_min ) {

                # if there are blank lines, we require that at least $num lines
                # be non-blank up to the boundary with the next subgroup.
                my $nog_b = my $nog_e = 1;
                if ( @iblanks && !$Opt_blanks_delete ) {
                    my $j_bb = $j_b + $num - 1;
                    my ( $i_bb, $tok_bb, $count_bb ) = @{ $group[$j_bb] };
                    $nog_b = $count_bb - $count_b + 1 == $num;

                    my $j_ee = $j_e - ( $num - 1 );
                    my ( $i_ee, $tok_ee, $count_ee ) = @{ $group[$j_ee] };
                    $nog_e = $count_e - $count_ee + 1 == $num;
                }
                if ( $nog_b && $k > $kbeg ) {
                    $insert_blank_after->( $i_b - 1 );
                }
                if ( $nog_e && $k < $kend ) {
                    my ( $i_ep, $tok_ep, $count_ep ) = @{ $group[ $j_e + 1 ] };
                    $insert_blank_after->( $i_ep - 1 );
                }
            }
        }
    };

    my $delete_if_blank = sub {
        my ($i) = @_;

        # delete line $i if it is blank
        return unless ( $i >= 0 && $i < @{$rlines} );
        my $line_type = $rlines->[$i]->{_line_type};
        return if ( $line_type ne 'CODE' );
        my $code_type = $rlines->[$i]->{_code_type};
        if ( $code_type eq 'BL' ) { $rhash_of_desires->{$i} = 2; }
        return;
    };

    my $delete_inner_blank_lines = sub {

        # always remove unwanted trailing blank lines from our list
        return unless (@iblanks);
        while ( my $ibl = pop(@iblanks) ) {
            if ( $ibl < $iend ) { push @iblanks, $ibl; last }
            $iend = $ibl;
        }

        # now mark mark interior blank lines for deletion if requested
        return unless ($Opt_blanks_delete);

        while ( my $ibl = pop(@iblanks) ) { $rhash_of_desires->{$ibl} = 2 }

    };

    my $end_group = sub {

        # end a group of keywords
        my ($bad_ending) = @_;
        if ( defined($ibeg) && $ibeg >= 0 ) {

            # then handle sufficiently large groups
            if ( $count >= $Opt_size_min ) {

                $number_of_groups_seen++;

                # do any blank deletions regardless of the count
                $delete_inner_blank_lines->();

                if ( $ibeg > 0 ) {
                    my $code_type = $rlines->[ $ibeg - 1 ]->{_code_type};

                    # patch for hash bang line which is not currently marked as
                    # a comment; mark it as a comment
                    if ( $ibeg == 1 && !$code_type ) {
                        my $line_text = $rlines->[ $ibeg - 1 ]->{_line_text};
                        $code_type = 'BC'
                          if ( $line_text && $line_text =~ /^#/ );
                    }

                    # Do not insert a blank after a comment
                    # (this could be subject to a flag in the future)
                    if ( $code_type !~ /(BC|SBC|SBCX)/ ) {
                        if ( $Opt_blanks_before == INSERT ) {
                            $insert_blank_after->( $ibeg - 1 );

                        }
                        elsif ( $Opt_blanks_before == DELETE ) {
                            $delete_if_blank->( $ibeg - 1 );
                        }
                    }
                }

                # We will only put blanks before code lines. We could loosen
                # this rule a little, but we have to be very careful because
                # for example we certainly don't want to drop a blank line
                # after a line like this:
                #   my $var = <<EOM;
                if ( $line_type eq 'CODE' && defined($K_first) ) {

                    # - Do not put a blank before a line of different level
                    # - Do not put a blank line if we ended the search badly
                    # - Do not put a blank at the end of the file
                    # - Do not put a blank line before a hanging side comment
                    my $level    = $rLL->[$K_first]->[_LEVEL_];
                    my $ci_level = $rLL->[$K_first]->[_CI_LEVEL_];

                    if (   $level == $level_beg
                        && $ci_level == 0
                        && !$bad_ending
                        && $iend < @{$rlines}
                        && $CODE_type ne 'HSC' )
                    {
                        if ( $Opt_blanks_after == INSERT ) {
                            $insert_blank_after->($iend);
                        }
                        elsif ( $Opt_blanks_after == DELETE ) {
                            $delete_if_blank->( $iend + 1 );
                        }
                    }
                }
            }
            $split_into_sub_groups->();
        }

        # reset for another group
        $ibeg      = -1;
        $iend      = undef;
        $level_beg = -1;
        $K_closing = undef;
        @group     = ();
        @subgroup  = ();
        @iblanks   = ();
    };

    my $find_container_end = sub {

        # If the keyword lines ends with an open token, find the closing token
        # '$K_closing' so that we can easily skip past the contents of the
        # container.
        return if ( $K_last <= $K_first );
        my $KK        = $K_last;
        my $type_last = $rLL->[$KK]->[_TYPE_];
        my $tok_last  = $rLL->[$KK]->[_TOKEN_];
        if ( $type_last eq '#' ) {
            $KK       = $self->K_previous_nonblank($KK);
            $tok_last = $rLL->[$KK]->[_TOKEN_];
        }
        if ( $KK > $K_first && $tok_last =~ /^[\(\{\[]$/ ) {

            my $type_sequence = $rLL->[$KK]->[_TYPE_SEQUENCE_];
            my $lev           = $rLL->[$KK]->[_LEVEL_];
            if ( $lev == $level_beg ) {
                $K_closing = $K_closing_container->{$type_sequence};
            }
        }
    };

    my $add_to_group = sub {
        my ( $i, $token, $level ) = @_;

        # End the previous group if we have reached the maximum
        # group size
        if ( $Opt_size_max && @group >= $Opt_size_max ) {
            $end_group->();
        }

        if ( @group == 0 ) {
            $ibeg      = $i;
            $level_beg = $level;
            $count     = 0;
        }

        $count++;
        $iend = $i;

        # New sub-group?
        if ( !@group || $token ne $group[-1]->[1] ) {
            push @subgroup, scalar(@group);
        }
        push @group, [ $i, $token, $count ];

        # remember if this line ends in an open container
        $find_container_end->();

        return;
    };

    ###################################
    # loop over all lines of the source
    ###################################
    $end_group->();
    my $i = -1;
    foreach my $line_of_tokens ( @{$rlines} ) {

        $i++;
        last
          if ( $Opt_repeat_count > 0
            && $number_of_groups_seen >= $Opt_repeat_count );

        $CODE_type = "";
        $K_first   = undef;
        $K_last    = undef;
        $line_type = $line_of_tokens->{_line_type};

        # always end a group at non-CODE
        if ( $line_type ne 'CODE' ) { $end_group->(); next }

        $CODE_type = $line_of_tokens->{_code_type};

        # end any group at a format skipping line
        if ( $CODE_type && $CODE_type eq 'FS' ) {
            $end_group->();
            next;
        }

        # continue in a verbatim (VB) type; it may be quoted text
        if ( $CODE_type eq 'VB' ) {
            if ( $ibeg >= 0 ) { $iend = $i; }
            next;
        }

        # and continue in blank (BL) types
        if ( $CODE_type eq 'BL' ) {
            if ( $ibeg >= 0 ) {
                $iend = $i;
                push @{iblanks}, $i;

                # propagate current subgroup token
                my $tok = $group[-1]->[1];
                push @group, [ $i, $tok, $count ];
            }
            next;
        }

        # examine the first token of this line
        my $rK_range = $line_of_tokens->{_rK_range};
        ( $K_first, $K_last ) = @{$rK_range};
        if ( !defined($K_first) ) {

            # Somewhat unexpected blank line..
            # $rK_range is normally defined for line type CODE, but this can
            # happen for example if the input line was a single semicolon which
            # is being deleted.  In that case there was code in the input
            # file but it is not being retained. So we can silently return.
            return $rhash_of_desires;
        }

        my $level    = $rLL->[$K_first]->[_LEVEL_];
        my $type     = $rLL->[$K_first]->[_TYPE_];
        my $token    = $rLL->[$K_first]->[_TOKEN_];
        my $ci_level = $rLL->[$K_first]->[_CI_LEVEL_];

        # see if this is a code type we seek (i.e. comment)
        if (   $CODE_type
            && $Opt_comment_pattern
            && $CODE_type =~ /$Opt_comment_pattern/ )
        {

            my $tok = $CODE_type;

            # Continuing a group
            if ( $ibeg >= 0 && $level == $level_beg ) {
                $add_to_group->( $i, $tok, $level );
            }

            # Start new group
            else {

                # first end old group if any; we might be starting new
                # keywords at different level
                if ( $ibeg > 0 ) { $end_group->(); }
                $add_to_group->( $i, $tok, $level );
            }
            next;
        }

        # See if it is a keyword we seek, but never start a group in a
        # continuation line; the code may be badly formatted.
        if (   $ci_level == 0
            && $type eq 'k'
            && $token =~ /$Opt_pattern/ )
        {

            # Continuing a keyword group
            if ( $ibeg >= 0 && $level == $level_beg ) {
                $add_to_group->( $i, $token, $level );
            }

            # Start new keyword group
            else {

                # first end old group if any; we might be starting new
                # keywords at different level
                if ( $ibeg > 0 ) { $end_group->(); }
                $add_to_group->( $i, $token, $level );
            }
            next;
        }

        # This is not one of our keywords, but we are in a keyword group
        # so see if we should continue or quit
        elsif ( $ibeg >= 0 ) {

            # - bail out on a large level change; we may have walked into a
            #   data structure or anoymous sub code.
            if ( $level > $level_beg + 1 || $level < $level_beg ) {
                $end_group->();
                next;
            }

            # - keep going on a continuation line of the same level, since
            #   it is probably a continuation of our previous keyword,
            # - and keep going past hanging side comments because we never
            #   want to interrupt them.
            if ( ( ( $level == $level_beg ) && $ci_level > 0 )
                || $CODE_type eq 'HSC' )
            {
                $iend = $i;
                next;
            }

            # - continue if if we are within in a container which started with
            # the line of the previous keyword.
            if ( defined($K_closing) && $K_first <= $K_closing ) {

                # continue if entire line is within container
                if ( $K_last <= $K_closing ) { $iend = $i; next }

                # continue at ); or }; or ];
                my $KK = $K_closing + 1;
                if ( $rLL->[$KK]->[_TYPE_] eq ';' ) {
                    if ( $KK < $K_last ) {
                        if ( $rLL->[ ++$KK ]->[_TYPE_] eq 'b' ) { ++$KK }
                        if ( $KK > $K_last || $rLL->[$KK]->[_TYPE_] ne '#' ) {
                            $end_group->(1);
                            next;
                        }
                    }
                    $iend = $i;
                    next;
                }

                $end_group->(1);
                next;
            }

            # - end the group if none of the above
            $end_group->();
            next;
        }

        # not in a keyword group; continue
        else { next }
    }

    # end of loop over all lines
    $end_group->();
    return $rhash_of_desires;

} ## end sub keyword_group_scan

#######################################
# CODE SECTION 7: Process lines of code
#######################################

{    ## begin closure process_line_of_CODE

    # The routines in this closure receive lines of code and combine them into
    # 'batches' and send them along. A 'batch' is the unit of code which can be
    # processed further as a unit. It has the property that it is the largest
    # amount of code into which which perltidy is free to place one or more
    # line breaks within it without violating any constraints.

    # When a new batch is formed it is sent to sub 'grind_batch_of_code'.

    # flags needed by the store routine
    my $line_of_tokens;
    my $no_internal_newlines;
    my $side_comment_follows;
    my $CODE_type;

    # range of K of tokens for the current line
    my ( $K_first, $K_last );

    my ( $rLL, $radjusted_levels );

    # past stored nonblank tokens
    my (
        $last_last_nonblank_token,  $last_last_nonblank_type,
        $last_nonblank_token,       $last_nonblank_type,
        $last_nonblank_block_type,  $K_last_nonblank_code,
        $K_last_last_nonblank_code, $looking_for_else,
        $is_static_block_comment,   $batch_CODE_type,
        $last_line_had_side_comment,
    );

    # Called once at the start of a new file
    sub initialize_process_line_of_CODE {
        $last_nonblank_token        = ';';
        $last_nonblank_type         = ';';
        $last_last_nonblank_token   = ';';
        $last_last_nonblank_type    = ';';
        $last_nonblank_block_type   = "";
        $K_last_nonblank_code       = undef;
        $K_last_last_nonblank_code  = undef;
        $looking_for_else           = 0;
        $is_static_block_comment    = 0;
        $batch_CODE_type            = "";
        $last_line_had_side_comment = 0;
        return;
    }

    # Batch variables: these describe the current batch of code being formed
    # and sent down the pipeline.  They are initialized in the next
    # sub.
    my ( $rbrace_follower, $index_start_one_line_block,
        $semicolons_before_block_self_destruct,
        $starting_in_quote, $ending_in_quote, );

    # Called before the start of each new batch
    sub initialize_batch_variables {

        $max_index_to_go      = UNDEFINED_INDEX;
        @summed_lengths_to_go = @nesting_depth_to_go = (0);

        # The initialization code for the remaining batch arrays is as follows
        # and can be activated for testing.  But profiling shows that it is
        # time-consuming to re-initialize the batch arrays and is not necessary
        # because the maximum valid token, $max_index_to_go, is carefully
        # controlled.  This means however that it is not possible to do any
        # type of filter or map operation directly on these arrays.  And it is
        # not possible to use negative indexes. As a precaution against program
        # changes which might do this, sub pad_array_to_go adds some undefs at
        # the end of the current batch of data.

        # So 'long story short': this is a waste of time
        0 && do { #<<<
        @block_type_to_go            = ();
        @type_sequence_to_go         = ();
        @container_environment_to_go = ();
        @bond_strength_to_go         = ();
        @forced_breakpoint_to_go     = ();
        @token_lengths_to_go         = ();
        @levels_to_go                = ();
        @mate_index_to_go            = ();
        @ci_levels_to_go             = ();
        @nobreak_to_go               = ();
        @old_breakpoint_to_go        = ();
        @tokens_to_go                = ();
        @K_to_go                     = ();
        @types_to_go                 = ();
        @leading_spaces_to_go        = ();
        @reduced_spaces_to_go        = ();
        @inext_to_go                 = ();
        @iprev_to_go                 = ();
        };

        $rbrace_follower = undef;
        $ending_in_quote = 0;
        destroy_one_line_block();
        return;
    }

    sub leading_spaces_to_go {

        # return the number of indentation spaces for a token in the output
        # stream; these were previously stored by 'set_leading_whitespace'.

        my ($ii) = @_;
        return 0 if ( $ii < 0 );
        my $indentation = $leading_spaces_to_go[$ii];
        return ref($indentation) ? $indentation->get_spaces() : $indentation;
    }

    sub create_one_line_block {
        ( $index_start_one_line_block, $semicolons_before_block_self_destruct )
          = @_;
        return;
    }

    sub destroy_one_line_block {
        $index_start_one_line_block            = UNDEFINED_INDEX;
        $semicolons_before_block_self_destruct = 0;
        return;
    }

    # Routine to place the current token into the output stream.
    # Called once per output token.

    use constant DEBUG_STORE => 0;

    sub store_token_to_go {

        my ( $self, $Ktoken_vars, $rtoken_vars ) = @_;

        # Add one token to the next batch.
        # $Ktoken_vars = the index K in the global token array
        # $rtoken_vars = $rLL->[$Ktoken_vars] = the corresponding token values
        #                unless they are temporarily being overriden

        # NOTE: This routine needs to be coded efficiently because it is called
        # once per token.  I have gotten it down from the second slowest to the
        # eighth slowest, but that still seems rather slow for what it does.

        # This closure variable has already been defined, for efficiency:
        #     my $radjusted_levels = $self->[_radjusted_levels_];

        my $type = $rtoken_vars->[_TYPE_];

        # Check for emergency flush...
        # The K indexes in the batch must always be a continuous sequence of
        # the global token array.  The batch process programming assumes this.
        # If storing this token would cause this relation to fail we must dump
        # the current batch before storing the new token.  It is extremely rare
        # for this to happen. One known example is the following two-line
        # snippet when run with parameters
        # --noadd-newlines  --space-terminal-semicolon:
        #    if ( $_ =~ /PENCIL/ ) { $pencil_flag= 1 } ; ;
        #    $yy=1;
        if ( $max_index_to_go >= 0 ) {
            my $Klast = $K_to_go[$max_index_to_go];
            if ( $Ktoken_vars != $Klast + 1 ) {
                $self->flush_batch_of_CODE();
            }

            # Do not output consecutive blank tokens ... this should not
            # happen, but it is worth checking.  Later code can then make the
            # simplifying assumption that blank tokens are not consecutive.
            elsif ( $type eq 'b' && $types_to_go[$max_index_to_go] eq 'b' ) {
                return;
            }
        }

        ++$max_index_to_go;
        $batch_CODE_type               = $CODE_type;
        $K_to_go[$max_index_to_go]     = $Ktoken_vars;
        $types_to_go[$max_index_to_go] = $type;

        $old_breakpoint_to_go[$max_index_to_go]    = 0;
        $forced_breakpoint_to_go[$max_index_to_go] = 0;
        $mate_index_to_go[$max_index_to_go]        = -1;

        my $token = $tokens_to_go[$max_index_to_go] = $rtoken_vars->[_TOKEN_];
        my $ci_level = $ci_levels_to_go[$max_index_to_go] =
          $rtoken_vars->[_CI_LEVEL_];

        # Clip levels to zero if there are level errors in the file.
        # We had to wait until now for reasons explained in sub 'write_line'.
        my $level = $rtoken_vars->[_LEVEL_];
        if ( $level < 0 ) { $level = 0 }
        $levels_to_go[$max_index_to_go] = $level;

        $nesting_depth_to_go[$max_index_to_go] = $rtoken_vars->[_SLEVEL_];
        $block_type_to_go[$max_index_to_go]    = $rtoken_vars->[_BLOCK_TYPE_];
        $container_environment_to_go[$max_index_to_go] =
          $rtoken_vars->[_CONTAINER_ENVIRONMENT_];
        $type_sequence_to_go[$max_index_to_go] =
          $rtoken_vars->[_TYPE_SEQUENCE_];

        $nobreak_to_go[$max_index_to_go] =
          $side_comment_follows ? 2 : $no_internal_newlines;

        my $length = $rtoken_vars->[_TOKEN_LENGTH_];

        # Safety check that length is defined. Should not be needed now.
        # Former patch for indent-only, in which the entire set of tokens is
        # turned into type 'q'. Lengths may have not been defined because sub
        # 'respace_tokens' is bypassed. We do not need lengths in this case,
        # but we will use the character count to have a defined value.  In the
        # future, it would be nicer to have 'respace_tokens' convert the lines
        # to quotes and get correct lengths.
        if ( !defined($length) ) { $length = length($token) }

        $token_lengths_to_go[$max_index_to_go] = $length;

        # We keep a running sum of token lengths from the start of this batch:
        #   summed_lengths_to_go[$i]   = total length to just before token $i
        #   summed_lengths_to_go[$i+1] = total length to just after token $i
        $summed_lengths_to_go[ $max_index_to_go + 1 ] =
          $summed_lengths_to_go[$max_index_to_go] + $length;

        my $in_continued_quote =
          ( $Ktoken_vars == $K_first ) && $line_of_tokens->{_starting_in_quote};
        if ( $max_index_to_go == 0 ) {
            $starting_in_quote = $in_continued_quote;
        }

        # Define the indentation that this token will have in two cases:
        # Without CI = reduced_spaces_to_go
        # With CI    = leading_spaces_to_go
        if ($in_continued_quote) {
            $leading_spaces_to_go[$max_index_to_go] = 0;
            $reduced_spaces_to_go[$max_index_to_go] = 0;
        }
        else {
            $reduced_spaces_to_go[$max_index_to_go] = my $reduced_spaces =
              $rOpts_indent_columns * $radjusted_levels->[$Ktoken_vars];
            $leading_spaces_to_go[$max_index_to_go] =
              $reduced_spaces + $rOpts_continuation_indentation * $ci_level;
        }

        # Correct these values if -lp is used
        if ($rOpts_line_up_parentheses) {
            $self->set_leading_whitespace( $Ktoken_vars, $K_last_nonblank_code,
                $K_last_last_nonblank_code, $level, $ci_level,
                $in_continued_quote );
        }

        DEBUG_STORE && do {
            my ( $a, $b, $c ) = caller();
            print STDOUT
"STORE: from $a $c: storing token $token type $type lev=$level at $max_index_to_go\n";
        };
        return;
    }

    sub flush_batch_of_CODE {

        # Finish any batch packaging and call the process routine.
        # This must be the only call to grind_batch_of_CODE()
        my ($self) = @_;

        return unless ( $max_index_to_go >= 0 );

        # Create an array to hold variables for this batch
        my $this_batch = [];
        $this_batch->[_starting_in_quote_] = $starting_in_quote;
        $this_batch->[_ending_in_quote_]   = $ending_in_quote;
        $this_batch->[_max_index_to_go_]   = $max_index_to_go;
        $this_batch->[_rK_to_go_]          = \@K_to_go;
        $this_batch->[_batch_CODE_type_]   = $batch_CODE_type;

        # The flag $is_static_block_comment applies to the line which just
        # arrived. So it only applies if we are outputting that line.
        $this_batch->[_is_static_block_comment_] =
             defined($K_first)
          && $max_index_to_go == 0
          && $K_to_go[0] == $K_first ? $is_static_block_comment : 0;

        $self->[_this_batch_] = $this_batch;

        $last_line_had_side_comment =
          $max_index_to_go > 0 && $types_to_go[$max_index_to_go] eq '#';

        $self->grind_batch_of_CODE();

        # Done .. this batch is history
        $self->[_this_batch_] = [];

        initialize_batch_variables();
        initialize_forced_breakpoint_vars();
        initialize_gnu_batch_vars()
          if $rOpts_line_up_parentheses;

        return;
    }

    sub end_batch {

        # end the current batch, EXCEPT for a few special cases
        my ($self) = @_;

        # Exception 1: Do not end line in a weld
        return
          if ( $total_weld_count
            && $self->weld_len_right_to_go($max_index_to_go) );

        # Exception 2: just set a tentative breakpoint if we might be in a
        # one-line block
        if ( $index_start_one_line_block != UNDEFINED_INDEX ) {
            $self->set_forced_breakpoint($max_index_to_go);
            return;
        }

        $self->flush_batch_of_CODE();
        return;
    }

    sub flush_vertical_aligner {
        my ($self) = @_;
        my $vao = $self->[_vertical_aligner_object_];
        $vao->flush();
        return;
    }

    # flush is called to output any tokens in the pipeline, so that
    # an alternate source of lines can be written in the correct order
    sub flush {
        my ( $self, $CODE_type ) = @_;

        # end the current batch with 1 exception

        destroy_one_line_block();

        # Exception: if we are flushing within the code stream only to insert
        # blank line(s), then we can keep the batch intact at a weld. This
        # improves formatting of -ce.  See test 'ce1.ce'
        if ( $CODE_type && $CODE_type eq 'BL' ) { $self->end_batch() }

        # otherwise, we have to shut things down completely.
        else { $self->flush_batch_of_CODE() }

        $self->flush_vertical_aligner();
        return;
    }

    sub process_line_of_CODE {

        my ( $self, $my_line_of_tokens ) = @_;

        # This routine is called once per INPUT line to process all of the
        # tokens on that line.

        # It outputs full-line comments and blank lines immediately.

        # The tokens are copied one-by-one from the global token array $rLL to
        # a set of '_to_go' arrays which collect batches of tokens for a
        # further processing via calls to 'sub store_token_to_go', until a well
        # defined 'structural' break point* or 'forced' breakpoint* is reached.
        # Then, the batch of collected '_to_go' tokens is passed along to 'sub
        # grind_batch_of_CODE' for further processing.

        # * 'structural' break points are basically line breaks corresponding
        # to code blocks.  An example is a chain of if-elsif-else statements,
        # which should typically be broken at the opening and closing braces.

        # * 'forced' break points are breaks required by side comments or by
        # special user controls.

        # So this routine is just making an initial set of required line
        # breaks, basically regardless of the maximum requested line length.
        # The subsequent stage of formating make additional line breaks
        # appropriate for lists and logical structures, and to keep line
        # lengths below the requested maximum line length.

        $line_of_tokens = $my_line_of_tokens;
        $CODE_type      = $line_of_tokens->{_code_type};
        my $input_line_number = $line_of_tokens->{_line_number};
        my $input_line        = $line_of_tokens->{_line_text};

        # initialize closure variables
        my $rK_range = $line_of_tokens->{_rK_range};
        ( $K_first, $K_last ) = @{$rK_range};

        # remember original starting index in case it changes
        my $K_first_true = $K_first;

        $rLL              = $self->[_rLL_];
        $radjusted_levels = $self->[_radjusted_levels_];

        my $file_writer_object = $self->[_file_writer_object_];
        my $rbreak_container   = $self->[_rbreak_container_];
        my $rshort_nested      = $self->[_rshort_nested_];
        my $sink_object        = $self->[_sink_object_];
        my $fh_tee             = $self->[_fh_tee_];
        my $ris_bli_container  = $self->[_ris_bli_container_];

        if ( !defined($K_first) ) {

            # Empty line: This can happen if tokens are deleted, for example
            # with the -mangle parameter
            return;
        }

        $no_internal_newlines = 0;
        if ( !$rOpts_add_newlines || $CODE_type eq 'NIN' ) {
            $no_internal_newlines = 2;
        }

        $side_comment_follows = 0;
        my $is_comment =
          ( $K_first == $K_last && $rLL->[$K_first]->[_TYPE_] eq '#' );
        my $is_static_block_comment_without_leading_space =
          $CODE_type eq 'SBCX';
        $is_static_block_comment =
          $CODE_type eq 'SBC' || $is_static_block_comment_without_leading_space;
        my $is_hanging_side_comment = $CODE_type eq 'HSC';
        my $is_VERSION_statement    = $CODE_type eq 'VER';

        if ($is_VERSION_statement) {
            $self->[_saw_VERSION_in_this_file_] = 1;
            $no_internal_newlines = 2;
        }

        # Add interline blank if any
        my $last_old_nonblank_type   = "b";
        my $first_new_nonblank_token = "";
        if ( $max_index_to_go >= 0 ) {
            $last_old_nonblank_type   = $types_to_go[$max_index_to_go];
            $first_new_nonblank_token = $rLL->[$K_first]->[_TOKEN_];
            if (  !$is_comment
                && $types_to_go[$max_index_to_go] ne 'b'
                && $K_first > 0
                && $rLL->[ $K_first - 1 ]->[_TYPE_] eq 'b' )
            {
                $K_first -= 1;
            }
        }

        my $rtok_first = $rLL->[$K_first];

        my $in_quote = $line_of_tokens->{_ending_in_quote};
        $ending_in_quote = $in_quote;
        my $guessed_indentation_level =
          $line_of_tokens->{_guessed_indentation_level};

        ######################################
        # Handle a block (full-line) comment..
        ######################################
        if ($is_comment) {

            if ( $rOpts->{'delete-block-comments'} ) {
                $self->flush();
                return;
            }

            destroy_one_line_block();
            $self->end_batch();

            # output a blank line before block comments
            if (
                # unless we follow a blank or comment line
                $self->[_last_line_leading_type_] ne '#'
                && $self->[_last_line_leading_type_] ne 'b'

                # only if allowed
                && $rOpts->{'blanks-before-comments'}

                # if this is NOT an empty comment, unless it follows a side
                # comment and could become a hanging side comment.
                && (
                    $rtok_first->[_TOKEN_] ne '#'
                    || (   $last_line_had_side_comment
                        && $rLL->[$K_first]->[_LEVEL_] > 0 )
                )

                # not after a short line ending in an opening token
                # because we already have space above this comment.
                # Note that the first comment in this if block, after
                # the 'if (', does not get a blank line because of this.
                && !$self->[_last_output_short_opening_token_]

                # never before static block comments
                && !$is_static_block_comment
              )
            {
                $self->flush();    # switching to new output stream
                $file_writer_object->write_blank_code_line();
                $self->[_last_line_leading_type_] = 'b';
            }

            if (
                $rOpts->{'indent-block-comments'}
                && (  !$rOpts->{'indent-spaced-block-comments'}
                    || $input_line =~ /^\s+/ )
                && !$is_static_block_comment_without_leading_space
              )
            {
                my $Ktoken_vars = $K_first;
                my $rtoken_vars = $rLL->[$Ktoken_vars];
                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );
                $self->end_batch();
            }
            else {

                # switching to new output stream
                $self->flush();

                # Note that last arg in call here is 'undef' for comments
                $file_writer_object->write_code_line(
                    $rtok_first->[_TOKEN_] . "\n", undef );
                $self->[_last_line_leading_type_] = '#';
            }
            return;
        }

        # compare input/output indentation except for continuation lines
        # (because they have an unknown amount of initial blank space)
        # and lines which are quotes (because they may have been outdented)
        $self->compare_indentation_levels( $K_first, $guessed_indentation_level,
            $input_line_number )
          unless ( $is_hanging_side_comment
            || $rtok_first->[_CI_LEVEL_] > 0
            || $guessed_indentation_level == 0
            && $rtok_first->[_TYPE_] eq 'Q' );

        ##########################
        # Handle indentation-only
        ##########################

        # NOTE: In previous versions we sent all qw lines out immediately here.
        # No longer doing this: also write a line which is entirely a 'qw' list
        # to allow stacking of opening and closing tokens.  Note that interior
        # qw lines will still go out at the end of this routine.
        if ( $CODE_type eq 'IO' ) {
            $self->flush();
            my $line = $input_line;

            # Fix for rt #125506 Unexpected string formating
            # in which leading space of a terminal quote was removed
            $line =~ s/\s+$//;
            $line =~ s/^\s+// unless ( $line_of_tokens->{_starting_in_quote} );

            my $Ktoken_vars = $K_first;

            # We work with a copy of the token variables and change the
            # first token to be the entire line as a quote variable
            my $rtoken_vars = $rLL->[$Ktoken_vars];
            $rtoken_vars = copy_token_as_type( $rtoken_vars, 'q', $line );

            # Patch: length is not really important here
            $rtoken_vars->[_TOKEN_LENGTH_] = length($line);

            $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );
            $self->end_batch();
            return;
        }

        ############################
        # Handle all other lines ...
        ############################

        # If we just saw the end of an elsif block, write nag message
        # if we do not see another elseif or an else.
        if ($looking_for_else) {

            unless ( $rLL->[$K_first]->[_TOKEN_] =~ /^(elsif|else)$/ ) {
                write_logfile_entry("(No else block)\n");
            }
            $looking_for_else = 0;
        }

        # This is a good place to kill incomplete one-line blocks
        if (
            (
                   ( $semicolons_before_block_self_destruct == 0 )
                && ( $max_index_to_go >= 0 )
                && ( $last_old_nonblank_type eq ';' )
                && ( $first_new_nonblank_token ne '}' )
            )

            # Patch for RT #98902. Honor request to break at old commas.
            || (   $rOpts_break_at_old_comma_breakpoints
                && $max_index_to_go >= 0
                && $last_old_nonblank_type eq ',' )
          )
        {
            $forced_breakpoint_to_go[$max_index_to_go] = 1
              if ($rOpts_break_at_old_comma_breakpoints);
            destroy_one_line_block();
            $self->end_batch();
        }

        # Keep any requested breaks before this line.  Note that we have to
        # use the original K_first because it may have been reduced above
        # to add a blank.
        if ( $self->[_rbreak_before_Kfirst_]->{$K_first_true} ) {
            destroy_one_line_block();
            $self->end_batch();
        }

        # loop to process the tokens one-by-one

        # We do not want a leading blank if the previous batch just got output
        if ( $max_index_to_go < 0 && $rLL->[$K_first]->[_TYPE_] eq 'b' ) {
            $K_first++;
        }

        foreach my $Ktoken_vars ( $K_first .. $K_last ) {

            my $rtoken_vars   = $rLL->[$Ktoken_vars];
            my $token         = $rtoken_vars->[_TOKEN_];
            my $type          = $rtoken_vars->[_TYPE_];
            my $block_type    = $rtoken_vars->[_BLOCK_TYPE_];
            my $type_sequence = $rtoken_vars->[_TYPE_SEQUENCE_];

            # If we are continuing after seeing a right curly brace, flush
            # buffer unless we see what we are looking for, as in
            #   } else ...
            if ( $rbrace_follower && $type ne 'b' ) {

                unless ( $rbrace_follower->{$token} ) {
                    $self->end_batch();
                }
                $rbrace_follower = undef;
            }

            # Get next nonblank on this line
            my $next_nonblank_token      = '';
            my $next_nonblank_token_type = 'b';
            if ( $Ktoken_vars < $K_last ) {
                my $Knnb = $Ktoken_vars + 1;
                if (   $rLL->[$Knnb]->[_TYPE_] eq 'b'
                    && $Knnb < $K_last )
                {
                    $Knnb++;
                }
                $next_nonblank_token      = $rLL->[$Knnb]->[_TOKEN_];
                $next_nonblank_token_type = $rLL->[$Knnb]->[_TYPE_];
            }

            # Do not allow breaks which would promote a side comment to a
            # block comment.  In order to allow a break before an opening
            # or closing BLOCK, followed by a side comment, those sections
            # of code will handle this flag separately.
            $side_comment_follows = ( $next_nonblank_token_type eq '#' );
            my $is_opening_BLOCK =
              (      $type eq '{'
                  && $token eq '{'
                  && $block_type
                  && !$rshort_nested->{$type_sequence}
                  && $block_type ne 't' );
            my $is_closing_BLOCK =
              (      $type eq '}'
                  && $token eq '}'
                  && $block_type
                  && !$rshort_nested->{$type_sequence}
                  && $block_type ne 't' );

            if (   $side_comment_follows
                && !$is_opening_BLOCK
                && !$is_closing_BLOCK )
            {
                $no_internal_newlines = 1;
            }

            # We're only going to handle breaking for code BLOCKS at this
            # (top) level.  Other indentation breaks will be handled by
            # sub scan_list, which is better suited to dealing with them.
            if ($is_opening_BLOCK) {

                # Tentatively output this token.  This is required before
                # calling starting_one_line_block.  We may have to unstore
                # it, though, if we have to break before it.
                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );

                # Look ahead to see if we might form a one-line block..
                my $too_long =
                  $self->starting_one_line_block( $Ktoken_vars,
                    $K_last_nonblank_code, $K_last );
                $self->clear_breakpoint_undo_stack();

                # to simplify the logic below, set a flag to indicate if
                # this opening brace is far from the keyword which introduces it
                my $keyword_on_same_line = 1;
                if (
                       $max_index_to_go >= 0
                    && $last_nonblank_type eq ')'
                    && ( ( $rtoken_vars->[_SLEVEL_] < $nesting_depth_to_go[0] )
                        || $too_long )
                  )
                {
                    $keyword_on_same_line = 0;
                }

                # decide if user requested break before '{'
                my $want_break =

                  # use -bl flag if not a sub block of any type
                  $block_type !~ /$ANYSUB_PATTERN/
                  ? $rOpts->{'opening-brace-on-new-line'}

                  # use -sbl flag for a named sub block
                  : $block_type !~ /$ASUB_PATTERN/
                  ? $rOpts->{'opening-sub-brace-on-new-line'}

                  # use -asbl flag for an anonymous sub block
                  : $rOpts->{'opening-anonymous-sub-brace-on-new-line'};

                # Break if requested with -bli flag
                $want_break ||= $ris_bli_container->{$type_sequence};

                # Do not break if this token is welded to the left
                if ( $self->weld_len_left( $type_sequence, $token ) ) {
                    $want_break = 0;
                }

                # Break before an opening '{' ...
                if (

                    # if requested
                    $want_break

                    # and we were unable to start looking for a block,
                    && $index_start_one_line_block == UNDEFINED_INDEX

                    # or if it will not be on same line as its keyword, so that
                    # it will be outdented (eval.t, overload.t), and the user
                    # has not insisted on keeping it on the right
                    || (   !$keyword_on_same_line
                        && !$rOpts->{'opening-brace-always-on-right'} )
                  )
                {

                    # but only if allowed
                    unless ($no_internal_newlines) {

                        # since we already stored this token, we must unstore it
                        $self->unstore_token_to_go();

                        # then output the line
                        $self->end_batch();

                        # and now store this token at the start of a new line
                        $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );
                    }
                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # now output this line
                unless ($no_internal_newlines) {
                    $self->end_batch();
                }
            }

            elsif ($is_closing_BLOCK) {

                # If there is a pending one-line block ..
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    # we have to terminate it if..
                    if (

                        # it is too long (final length may be different from
                        # initial estimate). note: must allow 1 space for this
                        # token
                        $self->excess_line_length( $index_start_one_line_block,
                            $max_index_to_go ) >= 0

                        # or if it has too many semicolons
                        || (   $semicolons_before_block_self_destruct == 0
                            && $last_nonblank_type ne ';' )
                      )
                    {
                        destroy_one_line_block();
                    }
                }

                # put a break before this closing curly brace if appropriate
                unless ( $no_internal_newlines
                    || $index_start_one_line_block != UNDEFINED_INDEX )
                {

                    # write out everything before this closing curly brace
                    $self->end_batch();
                }

                # Now update for side comment
                if ($side_comment_follows) { $no_internal_newlines = 1 }

                # store the closing curly brace
                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );

                # ok, we just stored a closing curly brace.  Often, but
                # not always, we want to end the line immediately.
                # So now we have to check for special cases.

                # if this '}' successfully ends a one-line block..
                my $is_one_line_block = 0;
                my $keep_going        = 0;
                if ( $index_start_one_line_block != UNDEFINED_INDEX ) {

                    # Remember the type of token just before the
                    # opening brace.  It would be more general to use
                    # a stack, but this will work for one-line blocks.
                    $is_one_line_block =
                      $types_to_go[$index_start_one_line_block];

                    # we have to actually make it by removing tentative
                    # breaks that were set within it
                    $self->undo_forced_breakpoint_stack(0);
                    $self->set_nobreaks( $index_start_one_line_block,
                        $max_index_to_go - 1 );

                    # then re-initialize for the next one-line block
                    destroy_one_line_block();

                    # then decide if we want to break after the '}' ..
                    # We will keep going to allow certain brace followers as in:
                    #   do { $ifclosed = 1; last } unless $losing;
                    #
                    # But make a line break if the curly ends a
                    # significant block:
                    if (
                        (
                            $is_block_without_semicolon{$block_type}

                            # Follow users break point for
                            # one line block types U & G, such as a 'try' block
                            || $is_one_line_block =~ /^[UG]$/
                            && $Ktoken_vars == $K_last
                        )

                        # if needless semicolon follows we handle it later
                        && $next_nonblank_token ne ';'
                      )
                    {
                        $self->end_batch()
                          unless ($no_internal_newlines);
                    }
                }

                # set string indicating what we need to look for brace follower
                # tokens
                if ( $block_type eq 'do' ) {
                    $rbrace_follower = \%is_do_follower;
                    if ( $self->tight_paren_follows( $K_to_go[0], $Ktoken_vars )
                      )
                    {
                        $rbrace_follower = { ')' => 1 };
                    }
                }
                elsif ( $block_type =~ /^(if|elsif|unless)$/ ) {
                    $rbrace_follower = \%is_if_brace_follower;
                }
                elsif ( $block_type eq 'else' ) {
                    $rbrace_follower = \%is_else_brace_follower;
                }

                # added eval for borris.t
                elsif ($is_sort_map_grep_eval{$block_type}
                    || $is_one_line_block eq 'G' )
                {
                    $rbrace_follower = undef;
                    $keep_going      = 1;
                }

                # anonymous sub
                elsif ( $block_type =~ /$ASUB_PATTERN/ ) {

                    if ($is_one_line_block) {
                        $rbrace_follower = \%is_anon_sub_1_brace_follower;
                    }
                    else {
                        $rbrace_follower = \%is_anon_sub_brace_follower;
                    }
                }

                # None of the above: specify what can follow a closing
                # brace of a block which is not an
                # if/elsif/else/do/sort/map/grep/eval
                # Testfiles:
                # 'Toolbar.pm', 'Menubar.pm', bless.t, '3rules.pl', 'break1.t
                else {
                    $rbrace_follower = \%is_other_brace_follower;
                }

                # See if an elsif block is followed by another elsif or else;
                # complain if not.
                if ( $block_type eq 'elsif' ) {

                    if ( $next_nonblank_token_type eq 'b' ) {    # end of line?
                        $looking_for_else = 1;    # ok, check on next line
                    }
                    else {

                        unless ( $next_nonblank_token =~ /^(elsif|else)$/ ) {
                            write_logfile_entry("No else block :(\n");
                        }
                    }
                }

                # keep going after certain block types (map,sort,grep,eval)
                # added eval for borris.t
                if ($keep_going) {

                    # keep going
                }

                # if no more tokens, postpone decision until re-entring
                elsif ( ( $next_nonblank_token_type eq 'b' )
                    && $rOpts_add_newlines )
                {
                    unless ($rbrace_follower) {
                        $self->end_batch()
                          unless ($no_internal_newlines);
                    }
                }

                elsif ($rbrace_follower) {

                    unless ( $rbrace_follower->{$next_nonblank_token} ) {
                        $self->end_batch()
                          unless ($no_internal_newlines);
                    }
                    $rbrace_follower = undef;
                }

                else {
                    $self->end_batch()
                      unless ($no_internal_newlines);
                }

            }    # end treatment of closing block token

            # handle semicolon
            elsif ( $type eq ';' ) {

                my $break_before_semicolon = ( $Ktoken_vars == $K_first )
                  && $rOpts_break_at_old_semicolon_breakpoints;

                # kill one-line blocks with too many semicolons
                $semicolons_before_block_self_destruct--;
                if (
                       $break_before_semicolon
                    || ( $semicolons_before_block_self_destruct < 0 )
                    || (   $semicolons_before_block_self_destruct == 0
                        && $next_nonblank_token_type !~ /^[b\}]$/ )
                  )
                {
                    destroy_one_line_block();
                    $self->end_batch() if ($break_before_semicolon);
                }

                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );

                $self->end_batch()
                  unless (
                    $no_internal_newlines
                    || (   $rOpts_keep_interior_semicolons
                        && $Ktoken_vars < $K_last )
                    || ( $next_nonblank_token eq '}' )
                  );

            }

            # handle here_doc target string
            elsif ( $type eq 'h' ) {

                # no newlines after seeing here-target
                $no_internal_newlines = 2;
                destroy_one_line_block();
                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );
            }

            # handle all other token types
            else {

                $self->store_token_to_go( $Ktoken_vars, $rtoken_vars );
            }

            # remember two previous nonblank OUTPUT tokens
            if ( $type ne '#' && $type ne 'b' ) {
                $last_last_nonblank_token  = $last_nonblank_token;
                $last_last_nonblank_type   = $last_nonblank_type;
                $last_nonblank_token       = $token;
                $last_nonblank_type        = $type;
                $last_nonblank_block_type  = $block_type;
                $K_last_last_nonblank_code = $K_last_nonblank_code;
                $K_last_nonblank_code      = $Ktoken_vars;
            }

        }    # end of loop over all tokens in this 'line_of_tokens'

        my $type = $rLL->[$K_last]->[_TYPE_];

        # we have to flush ..
        if (

            # if there is a side comment...
            $type eq '#'

            # if this line ends in a quote
            # NOTE: This is critically important for insuring that quoted lines
            # do not get processed by things like -sot and -sct
            || $in_quote

            # if this is a VERSION statement
            || $is_VERSION_statement

            # to keep a label at the end of a line
            || $type eq 'J'

            # if we are instructed to keep all old line breaks
            || !$rOpts->{'delete-old-newlines'}

            # we have a request to keep a break after this line
            || $self->[_rbreak_after_Klast_]->{$K_last}

            # if this is a line of the form 'use overload'. A break here
            # in the input file is a good break because it will allow
            # the operators which follow to be formatted well. Without
            # this break the formatting with -ci=4 -xci is poor, for example.

            #   use overload
            #     '+' => sub {
            #       print length $_[2], "\n";
            #       my ( $x, $y ) = _order(@_);
            #       Number::Roman->new( int $x + $y );
            #     },
            #     '-' => sub {
            #       my ( $x, $y ) = _order(@_);
            #       Number::Roman->new( int $x - $y );
            #     };
            || (   $max_index_to_go == 2
                && $types_to_go[0] eq 'k'
                && $tokens_to_go[0] eq 'use'
                && $tokens_to_go[$max_index_to_go] eq 'overload' )
          )
        {
            destroy_one_line_block();
            $self->end_batch();
        }

        # mark old line breakpoints in current output stream
        if ( $max_index_to_go >= 0 && !$rOpts_ignore_old_breakpoints ) {
            my $jobp = $max_index_to_go;
            if ( $types_to_go[$max_index_to_go] eq 'b' && $max_index_to_go > 0 )
            {
                $jobp--;
            }
            $old_breakpoint_to_go[$jobp] = 1;
        }
        return;
    } ## end sub process_line_of_CODE
} ## end closure process_line_of_CODE

sub tight_paren_follows {

    my ( $self, $K_to_go_0, $K_ic ) = @_;

    # Input parameters:
    #   $K_to_go_0 = first token index K of this output batch (=K_to_go[0])
    #   $K_ic = index of the closing do brace (=K_to_go[$max_index_to_go])
    # Return parameter:
    #   false if we want a break after the closing do brace
    #   true if we do not want a break after the closing do brace

    # We are at the closing brace of a 'do' block.  See if this brace is
    # followed by a closing paren, and if so, set a flag which indicates
    # that we do not want a line break between the '}' and ')'.

    # xxxxx ( ...... do {  ... } ) {
    #                          ^-------looking at this brace, K_ic

    # Subscript notation:
    # _i = inner container (braces in this case)
    # _o = outer container (parens in this case)
    # _io = inner opening = '{'
    # _ic = inner closing = '}'
    # _oo = outer opening = '('
    # _oc = outer closing = ')'

    #       |--K_oo                 |--K_oc  = outer container
    # xxxxx ( ...... do {  ...... } ) {
    #                   |--K_io   |--K_ic    = inner container

    # In general, the safe thing to do is return a 'false' value
    # if the statement appears to be complex.  This will have
    # the downstream side-effect of opening up outer containers
    # to help make complex code readable.  But for simpler
    # do blocks it can be preferable to keep the code compact
    # by returning a 'true' value.

    return unless defined($K_ic);
    my $rLL = $self->[_rLL_];

    # we should only be called at a closing block
    my $seqno_i = $rLL->[$K_ic]->[_TYPE_SEQUENCE_];
    return unless ($seqno_i);    # shouldn't happen;

    # This only applies if the next nonblank is a ')'
    my $K_oc = $self->K_next_nonblank($K_ic);
    return unless defined($K_oc);
    my $token_next = $rLL->[$K_oc]->[_TOKEN_];
    return unless ( $token_next eq ')' );

    my $seqno_o = $rLL->[$K_oc]->[_TYPE_SEQUENCE_];
    my $K_io    = $self->[_K_opening_container_]->{$seqno_i};
    my $K_oo    = $self->[_K_opening_container_]->{$seqno_o};
    return unless ( defined($K_io) && defined($K_oo) );

    # RULE 1: Do not break before a closing signature paren
    # (regardless of complexity).  This is a fix for issue git#22.
    # Looking for something like:
    #   sub xxx ( ... do {  ... } ) {
    #                               ^----- next block_type
    my $K_test = $self->K_next_nonblank($K_oc);
    if ( defined($K_test) ) {
        my $block_type = $rLL->[$K_test]->[_BLOCK_TYPE_];
        if (   $block_type
            && $rLL->[$K_test]->[_TYPE_] eq '{'
            && $block_type =~ /$ANYSUB_PATTERN/ )
        {
            return 1;
        }
    }

    # RULE 2: Break if the contents within braces appears to be 'complex'.  We
    # base this decision on the number of tokens between braces.

    # xxxxx ( ... do {  ... } ) {
    #                 ^^^^^^

    # Although very simple, it has the advantages of (1) being insensitive to
    # changes in lengths of identifier names, (2) easy to understand, implement
    # and test.  A test case for this is 't/snippets/long_line.in'.

    # Example: $K_ic - $K_oo = 9       [Pass Rule 2]
    # if ( do { $2 !~ /&/ } ) { ... }

    # Example: $K_ic - $K_oo = 10      [Pass Rule 2]
    # for ( split /\s*={70,}\s*/, do { local $/; <DATA> }) { ... }

    # Example: $K_ic - $K_oo = 20      [Fail Rule 2]
    # test_zero_args( "do-returned list slice", do { ( 10, 11 )[ 2, 3 ]; });

    return if ( $K_ic - $K_io > 16 );

    # RULE 3: break if the code between the opening '(' and the '{' is 'complex'
    # As with the previous rule, we decide based on the token count

    # xxxxx ( ... do {  ... } ) {
    #        ^^^^^^^^

    # Example: $K_ic - $K_oo = 9       [Pass Rule 2]
    #          $K_io - $K_oo = 4       [Pass Rule 3]
    # if ( do { $2 !~ /&/ } ) { ... }

    # Example: $K_ic - $K_oo = 10    [Pass rule 2]
    #          $K_io - $K_oo = 9     [Pass rule 3]
    # for ( split /\s*={70,}\s*/, do { local $/; <DATA> }) { ... }

    return if ( $K_io - $K_oo > 9 );

    # RULE 4: Break if we have already broken this batch of output tokens
    return if ( $K_oo < $K_to_go_0 );

    # RULE 5: Break if input is not on one line
    # For example, we will set the flag for the following expression
    # written in one line:

    # This has: $K_ic - $K_oo = 10    [Pass rule 2]
    #           $K_io - $K_oo = 8     [Pass rule 3]
    #   $self->debug( 'Error: ' . do { local $/; <$err> } );

    # but we break after the brace if it is on multiple lines on input, since
    # the user may prefer it on multiple lines:

    # [Fail rule 5]
    #   $self->debug(
    #       'Error: ' . do { local $/; <$err> }
    #   );

    if ( !$rOpts_ignore_old_breakpoints ) {
        my $iline_oo = $rLL->[$K_oo]->[_LINE_INDEX_];
        my $iline_oc = $rLL->[$K_oc]->[_LINE_INDEX_];
        return if ( $iline_oo != $iline_oc );
    }

    # OK to keep the paren tight
    return 1;
}

sub starting_one_line_block {

    # after seeing an opening curly brace, look for the closing brace and see
    # if the entire block will fit on a line.  This routine is not always right
    # so a check is made later (at the closing brace) to make sure we really
    # have a one-line block.  We have to do this preliminary check, though,
    # because otherwise we would always break at a semicolon within a one-line
    # block if the block contains multiple statements.

    my ( $self, $Kj, $K_last_nonblank, $K_last ) = @_;

    my $rbreak_container    = $self->[_rbreak_container_];
    my $rshort_nested       = $self->[_rshort_nested_];
    my $rLL                 = $self->[_rLL_];
    my $K_opening_container = $self->[_K_opening_container_];

    # kill any current block - we can only go 1 deep
    destroy_one_line_block();

    # return value:
    #  1=distance from start of block to opening brace exceeds line length
    #  0=otherwise

    my $i_start = 0;

    # This routine should not have been called if there are no tokens in the
    # 'to_go' arrays of previously stored tokens.  A previous call to
    # 'store_token_to_go' should have stored an opening brace. An error here
    # indicates that a programming change may have caused a flush operation to
    # clean out the previously stored tokens.
    if ( !defined($max_index_to_go) || $max_index_to_go < 0 ) {
        Fault("program bug: store_token_to_go called incorrectly\n");
    }

    # return if block should be broken
    my $type_sequence = $rLL->[$Kj]->[_TYPE_SEQUENCE_];
    if ( $rbreak_container->{$type_sequence} ) {
        return 0;
    }

    my $ris_bli_container = $self->[_ris_bli_container_];
    my $is_bli            = $ris_bli_container->{$type_sequence};

    my $block_type             = $rLL->[$Kj]->[_BLOCK_TYPE_];
    my $index_max_forced_break = get_index_max_forced_break();

    my $previous_nonblank_token = '';
    my $i_last_nonblank         = -1;
    if ( defined($K_last_nonblank) ) {
        $i_last_nonblank = $K_last_nonblank - $K_to_go[0];
        if ( $i_last_nonblank >= 0 ) {
            $previous_nonblank_token = $rLL->[$K_last_nonblank]->[_TOKEN_];
        }
    }

    # find the starting keyword for this block (such as 'if', 'else', ...)
    if (   $max_index_to_go == 0
        || $block_type =~ /^[\{\}\;\:]$/
        || $block_type =~ /^package/ )
    {
        $i_start = $max_index_to_go;
    }

    # the previous nonblank token should start these block types
    elsif (
        $i_last_nonblank >= 0
        && (   $previous_nonblank_token eq $block_type
            || $block_type =~ /$ANYSUB_PATTERN/
            || $block_type =~ /\(\)/ )
      )
    {
        $i_start = $i_last_nonblank;

        # For signatures and extended syntax ...
        # If this brace follows a parenthesized list, we should look back to
        # find the keyword before the opening paren because otherwise we might
        # form a one line block which stays intack, and cause the parenthesized
        # expression to break open. That looks bad.
        if ( $tokens_to_go[$i_start] eq ')' ) {

            # Find the opening paren
            my $K_start = $K_to_go[$i_start];
            return 0 unless defined($K_start);
            my $seqno = $type_sequence_to_go[$i_start];
            return 0 unless ($seqno);
            my $K_opening = $K_opening_container->{$seqno};
            return 0 unless defined($K_opening);
            my $i_opening = $i_start + ( $K_opening - $K_start );

            # give up if not on this line
            return 0 unless ( $i_opening >= 0 );
            $i_start = $i_opening;    ##$index_max_forced_break + 1;

            # go back one token before the opening paren
            if ( $i_start > 0 )                                  { $i_start-- }
            if ( $types_to_go[$i_start] eq 'b' && $i_start > 0 ) { $i_start--; }
            my $lev = $levels_to_go[$i_start];
            if ( $lev > $rLL->[$Kj]->[_LEVEL_] ) { return 0 }
        }
    }

    elsif ( $previous_nonblank_token eq ')' ) {

        # For something like "if (xxx) {", the keyword "if" will be
        # just after the most recent break. This will be 0 unless
        # we have just killed a one-line block and are starting another.
        # (doif.t)
        # Note: cannot use inext_index_to_go[] here because that array
        # is still being constructed.
        $i_start = $index_max_forced_break + 1;
        if ( $types_to_go[$i_start] eq 'b' ) {
            $i_start++;
        }

        # Patch to avoid breaking short blocks defined with extended_syntax:
        # Strip off any trailing () which was added in the parser to mark
        # the opening keyword.  For example, in the following
        #    create( TypeFoo $e) {$bubba}
        # the blocktype would be marked as create()
        my $stripped_block_type = $block_type;
        $stripped_block_type =~ s/\(\)$//;

        unless ( $tokens_to_go[$i_start] eq $stripped_block_type ) {
            return 0;
        }
    }

    # patch for SWITCH/CASE to retain one-line case/when blocks
    elsif ( $block_type eq 'case' || $block_type eq 'when' ) {

        # Note: cannot use inext_index_to_go[] here because that array
        # is still being constructed.
        $i_start = $index_max_forced_break + 1;
        if ( $types_to_go[$i_start] eq 'b' ) {
            $i_start++;
        }
        unless ( $tokens_to_go[$i_start] eq $block_type ) {
            return 0;
        }
    }

    else {
        return 1;
    }

    my $pos = total_line_length( $i_start, $max_index_to_go ) - 1;

    # see if length is too long to even start
    if ( $pos > $maximum_line_length[ $levels_to_go[$i_start] ] ) {
        return 1;
    }

    foreach my $Ki ( $Kj + 1 .. $K_last ) {

        # old whitespace could be arbitrarily large, so don't use it
        if ( $rLL->[$Ki]->[_TYPE_] eq 'b' ) { $pos += 1 }
        else { $pos += $rLL->[$Ki]->[_TOKEN_LENGTH_] }

        # ignore some small blocks
        my $type_sequence = $rLL->[$Ki]->[_TYPE_SEQUENCE_];
        my $nobreak       = $rshort_nested->{$type_sequence};

        # Return false result if we exceed the maximum line length,
        if ( $pos > $maximum_line_length[ $levels_to_go[$i_start] ] ) {
            return 0;
        }

        # keep going for non-containers
        elsif ( !$type_sequence ) {

        }

        # return if we encounter another opening brace before finding the
        # closing brace.
        elsif ($rLL->[$Ki]->[_TOKEN_] eq '{'
            && $rLL->[$Ki]->[_TYPE_] eq '{'
            && $rLL->[$Ki]->[_BLOCK_TYPE_]
            && !$nobreak )
        {
            return 0;
        }

        # if we find our closing brace..
        elsif ($rLL->[$Ki]->[_TOKEN_] eq '}'
            && $rLL->[$Ki]->[_TYPE_] eq '}'
            && $rLL->[$Ki]->[_BLOCK_TYPE_]
            && !$nobreak )
        {

            # be sure any trailing comment also fits on the line
            my $Ki_nonblank = $Ki;
            if ( $Ki_nonblank < $K_last ) {
                $Ki_nonblank++;
                if (   $rLL->[$Ki_nonblank]->[_TYPE_] eq 'b'
                    && $Ki_nonblank < $K_last )
                {
                    $Ki_nonblank++;
                }
            }

            # Patch for one-line sort/map/grep/eval blocks with side comments:
            # We will ignore the side comment length for sort/map/grep/eval
            # because this can lead to statements which change every time
            # perltidy is run.  Here is an example from Denis Moskowitz which
            # oscillates between these two states without this patch:

## --------
## grep { $_->foo ne 'bar' } # asdfa asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf
##  @baz;
##
## grep {
##     $_->foo ne 'bar'
##   }    # asdfa asdf asdf asdf asdf asdf asdf asdf asdf asdf asdf
##   @baz;
## --------

            # When the first line is input it gets broken apart by the main
            # line break logic in sub process_line_of_CODE.
            # When the second line is input it gets recombined by
            # process_line_of_CODE and passed to the output routines.  The
            # output routines (set_continuation_breaks) do not break it apart
            # because the bond strengths are set to the highest possible value
            # for grep/map/eval/sort blocks, so the first version gets output.
            # It would be possible to fix this by changing bond strengths,
            # but they are high to prevent errors in older versions of perl.

            if (   $Ki < $K_last
                && $rLL->[$Ki_nonblank]->[_TYPE_] eq '#'
                && !$is_sort_map_grep{$block_type} )
            {

                $pos += $rLL->[$Ki_nonblank]->[_TOKEN_LENGTH_];

                if ( $Ki_nonblank > $Ki + 1 ) {

                    # source whitespace could be anything, assume
                    # at least one space before the hash on output
                    if ( $rLL->[ $Ki + 1 ]->[_TYPE_] eq 'b' ) {
                        $pos += 1;
                    }
                    else { $pos += $rLL->[ $Ki + 1 ]->[_TOKEN_LENGTH_] }
                }

                if ( $pos >= $maximum_line_length[ $levels_to_go[$i_start] ] ) {
                    return 0;
                }
            }

            # ok, it's a one-line block
            create_one_line_block( $i_start, 20 );
            return 0;
        }

        # just keep going for other characters
        else {
        }
    }

    # We haven't hit the closing brace, but there is still space. So the
    # question here is, should we keep going to look at more lines in hopes of
    # forming a new one-line block, or should we stop right now. The problem
    # with continuing is that we will not be able to honor breaks before the
    # opening brace if we continue.

    # Typically we will want to keep trying to make one-line blocks for things
    # like sort/map/grep/eval.  But it is not always a good idea to make as
    # many one-line blocks as possible, so other types are not done.  The user
    # can always use -mangle.

    # If we want to keep going, we will create a new one-line block.
    # The blocks which we can keep going are in a hash, but we never want
    # to continue if we are at a '-bli' block.
    if ( $want_one_line_block{$block_type} && !$is_bli ) {
        create_one_line_block( $i_start, 1 );
    }
    return 0;
}

sub unstore_token_to_go {

    # remove most recent token from output stream
    my $self = shift;
    if ( $max_index_to_go > 0 ) {
        $max_index_to_go--;
    }
    else {
        $max_index_to_go = UNDEFINED_INDEX;
    }
    return;
}

sub compare_indentation_levels {

    # Check to see if output line tabbing agrees with input line
    # this can be very useful for debugging a script which has an extra
    # or missing brace.

    my ( $self, $K_first, $guessed_indentation_level, $line_number ) = @_;
    return unless ( defined($K_first) );

    my $rLL = $self->[_rLL_];

    my $structural_indentation_level = $rLL->[$K_first]->[_LEVEL_];
    my $radjusted_levels             = $self->[_radjusted_levels_];
    if ( defined($radjusted_levels) && @{$radjusted_levels} == @{$rLL} ) {
        $structural_indentation_level = $radjusted_levels->[$K_first];
    }

    my $is_closing_block = $rLL->[$K_first]->[_TYPE_] eq '}'
      && $rLL->[$K_first]->[_BLOCK_TYPE_];

    if ( $guessed_indentation_level ne $structural_indentation_level ) {
        $self->[_last_tabbing_disagreement_] = $line_number;

        if ($is_closing_block) {

            if ( !$self->[_in_brace_tabbing_disagreement_] ) {
                $self->[_in_brace_tabbing_disagreement_] = $line_number;
            }
            if ( !$self->[_first_brace_tabbing_disagreement_] ) {
                $self->[_first_brace_tabbing_disagreement_] = $line_number;
            }

        }

        if ( !$self->[_in_tabbing_disagreement_] ) {
            $self->[_tabbing_disagreement_count_]++;

            if ( $self->[_tabbing_disagreement_count_] <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"Start indentation disagreement: input=$guessed_indentation_level; output=$structural_indentation_level\n"
                );
            }
            $self->[_in_tabbing_disagreement_]    = $line_number;
            $self->[_first_tabbing_disagreement_] = $line_number
              unless ( $self->[_first_tabbing_disagreement_] );
        }
    }
    else {

        $self->[_in_brace_tabbing_disagreement_] = 0 if ($is_closing_block);

        my $in_tabbing_disagreement = $self->[_in_tabbing_disagreement_];
        if ($in_tabbing_disagreement) {

            if ( $self->[_tabbing_disagreement_count_] <= MAX_NAG_MESSAGES ) {
                write_logfile_entry(
"End indentation disagreement from input line $in_tabbing_disagreement\n"
                );

                if ( $self->[_tabbing_disagreement_count_] == MAX_NAG_MESSAGES )
                {
                    write_logfile_entry(
                        "No further tabbing disagreements will be noted\n");
                }
            }
            $self->[_in_tabbing_disagreement_] = 0;

        }
    }
    return;
}

###################################################
# CODE SECTION 8: Utilities for setting breakpoints
###################################################

{    ## begin closure set_forced_breakpoint

    my $forced_breakpoint_count;
    my $forced_breakpoint_undo_count;
    my @forced_breakpoint_undo_stack;
    my $index_max_forced_break;

    # Break before or after certain tokens based on user settings
    my %break_before_or_after_token;

    BEGIN {
        my @q = qw( = . : ? and or xor && || );
        push @q, ',';
        @break_before_or_after_token{@q} = (1) x scalar(@q);
    }

    sub initialize_forced_breakpoint_vars {
        $forced_breakpoint_count      = 0;
        $index_max_forced_break       = UNDEFINED_INDEX;
        $forced_breakpoint_undo_count = 0;
        @forced_breakpoint_undo_stack = ();
        return;
    }

    sub get_forced_breakpoint_count {
        return $forced_breakpoint_count;
    }

    sub get_forced_breakpoint_undo_count {
        return $forced_breakpoint_undo_count;
    }

    sub get_index_max_forced_break {
        return $index_max_forced_break;
    }

    sub set_fake_breakpoint {

        # Just bump up the breakpoint count as a signal that there are breaks.
        # This is useful if we have breaks but may want to postpone deciding
        # where to make them.
        $forced_breakpoint_count++;
        return;
    }

    use constant DEBUG_FORCE => 0;

    sub set_forced_breakpoint {
        my ( $self, $i ) = @_;

        return unless defined $i && $i >= 0;

        # no breaks between welded tokens
        return if ( $self->weld_len_right_to_go($i) );

        my $token = $tokens_to_go[$i];

        # For certain tokens, use user settings to decide if we break before or
        # after it
        #    qw( = . : ? and or xor && || )
        if ( $break_before_or_after_token{$token} ) {
            if ( $want_break_before{$token} && $i >= 0 ) { $i-- }
        }

        # breaks are forced before 'if' and 'unless'
        elsif ( $is_if_unless{$token} ) { $i-- }

        if ( $i >= 0 && $i <= $max_index_to_go ) {
            my $i_nonblank = ( $types_to_go[$i] ne 'b' ) ? $i : $i - 1;

            DEBUG_FORCE && do {
                my ( $a, $b, $c ) = caller();
                print STDOUT
"FORCE $forced_breakpoint_count from $a $c with i=$i_nonblank max=$max_index_to_go tok=$tokens_to_go[$i_nonblank] type=$types_to_go[$i_nonblank] nobr=$nobreak_to_go[$i_nonblank]\n";
            };

            ######################################################################
            # NOTE: if we call set_closing_breakpoint below it will then call
            # this routing back. So there is the possibility of an infinite
            # loop if a programming error is made. As a precaution, I have
            # added a check on the forced_breakpoint flag, so that we won't
            # keep trying to set it.  That will give additional protection
            # against a loop.
            ######################################################################

            if (   $i_nonblank >= 0
                && $nobreak_to_go[$i_nonblank] == 0
                && !$forced_breakpoint_to_go[$i_nonblank] )
            {
                $forced_breakpoint_to_go[$i_nonblank] = 1;

                if ( $i_nonblank > $index_max_forced_break ) {
                    $index_max_forced_break = $i_nonblank;
                }
                $forced_breakpoint_count++;
                $forced_breakpoint_undo_stack[ $forced_breakpoint_undo_count++ ]
                  = $i_nonblank;

                # if we break at an opening container..break at the closing
                if ( $is_opening_sequence_token{ $tokens_to_go[$i_nonblank] } )
                {
                    $self->set_closing_breakpoint($i_nonblank);
                }
            }
        }
        return;
    }

    sub clear_breakpoint_undo_stack {
        my ($self) = @_;
        $forced_breakpoint_undo_count = 0;
        return;
    }

    use constant DEBUG_UNDOBP => 0;

    sub undo_forced_breakpoint_stack {

        my ( $self, $i_start ) = @_;

        # Given $i_start, a non-negative index the 'undo stack' of breakpoints,
        # remove all breakpoints from the top of the 'undo stack' down to and
        # including index $i_start.

        # The 'undo stack' is a stack of all breakpoints made for a batch of
        # code.

        if ( $i_start < 0 ) {
            $i_start = 0;
            my ( $a, $b, $c ) = caller();

            # Bad call, can only be due to a recent programming change.
            # Better stop here.
            Fault(
"Program Bug: undo_forced_breakpoint_stack from $a $c has bad i=$i_start "
            );
        }

        while ( $forced_breakpoint_undo_count > $i_start ) {
            my $i =
              $forced_breakpoint_undo_stack[ --$forced_breakpoint_undo_count ];
            if ( $i >= 0 && $i <= $max_index_to_go ) {
                $forced_breakpoint_to_go[$i] = 0;
                $forced_breakpoint_count--;

                DEBUG_UNDOBP && do {
                    my ( $a, $b, $c ) = caller();
                    print STDOUT
"UNDOBP: undo forced_breakpoint i=$i $forced_breakpoint_undo_count from $a $c max=$max_index_to_go\n";
                };
            }

            # shouldn't happen, but not a critical error
            else {
                DEBUG_UNDOBP && do {
                    my ( $a, $b, $c ) = caller();
                    print STDOUT
"Program Bug: undo_forced_breakpoint from $a $c has i=$i but max=$max_index_to_go";
                };
            }
        }
        return;
    }
} ## end closure set_forced_breakpoint

{    ## begin closure set_closing_breakpoint

    my %postponed_breakpoint;

    sub initialize_postponed_breakpoint {
        %postponed_breakpoint = ();
        return;
    }

    sub has_postponed_breakpoint {
        my ($seqno) = @_;
        return $postponed_breakpoint{$seqno};
    }

    sub set_closing_breakpoint {

        # set a breakpoint at a matching closing token
        my ( $self, $i_break ) = @_;

        if ( $mate_index_to_go[$i_break] >= 0 ) {

            # CAUTION: infinite recursion possible here:
            #   set_closing_breakpoint calls set_forced_breakpoint, and
            #   set_forced_breakpoint call set_closing_breakpoint
            #   ( test files attrib.t, BasicLyx.pm.html).
            # Don't reduce the '2' in the statement below
            if ( $mate_index_to_go[$i_break] > $i_break + 2 ) {

             # break before } ] and ), but sub set_forced_breakpoint will decide
             # to break before or after a ? and :
                my $inc = ( $tokens_to_go[$i_break] eq '?' ) ? 0 : 1;
                $self->set_forced_breakpoint(
                    $mate_index_to_go[$i_break] - $inc );
            }
        }
        else {
            my $type_sequence = $type_sequence_to_go[$i_break];
            if ($type_sequence) {
                my $closing_token = $matching_token{ $tokens_to_go[$i_break] };
                $postponed_breakpoint{$type_sequence} = 1;
            }
        }
        return;
    }
} ## end closure set_closing_breakpoint

#########################################
# CODE SECTION 9: Process batches of code
#########################################

{    ## begin closure grind_batch_of_CODE

    # The routines in this closure begin the processing of a 'batch' of code.

    # A variable to keep track of consecutive nonblank lines so that we can
    # insert occasional blanks
    my @nonblank_lines_at_depth;

    # A variable to remember maximum size of previous batches; this is needed
    # by the logical padding routine
    my $peak_batch_size;
    my $batch_count;

    sub initialize_grind_batch_of_CODE {
        @nonblank_lines_at_depth = ();
        $peak_batch_size         = 0;
        $batch_count             = 0;
        return;
    }

    # sub grind_batch_of_CODE receives sections of code which are the longest
    # possible lines without a break.  In other words, it receives what is left
    # after applying all breaks forced by blank lines, block comments, side
    # comments, pod text, and structural braces.  Its job is to break this code
    # down into smaller pieces, if necessary, which fit within the maximum
    # allowed line length.  Then it sends the resulting lines of code on down
    # the pipeline to the VerticalAligner package, breaking the code into
    # continuation lines as necessary.  The batch of tokens are in the "to_go"
    # arrays.  The name 'grind' is slightly suggestive of a machine continually
    # breaking down long lines of code, but mainly it is unique and easy to
    # remember and find with an editor search.

    # The two routines 'process_line_of_CODE' and 'grind_batch_of_CODE' work
    # together in the following way:

    # - 'process_line_of_CODE' receives the original INPUT lines one-by-one and
    # combines them into the largest sequences of tokens which might form a new
    # line.
    # - 'grind_batch_of_CODE' determines which tokens will form the OUTPUT
    # lines.

    # So sub 'process_line_of_CODE' builds up the longest possible continouus
    # sequences of tokens, regardless of line length, and then
    # grind_batch_of_CODE breaks these sequences back down into the new output
    # lines.

    # Sub 'grind_batch_of_CODE' ships its output lines to the vertical aligner.

    use constant DEBUG_GRIND => 0;

    sub grind_batch_of_CODE {

        my ($self) = @_;
        my $file_writer_object = $self->[_file_writer_object_];

        my $this_batch = $self->[_this_batch_];
        $batch_count++;

        my $starting_in_quote        = $this_batch->[_starting_in_quote_];
        my $ending_in_quote          = $this_batch->[_ending_in_quote_];
        my $is_static_block_comment  = $this_batch->[_is_static_block_comment_];
        my $rK_to_go                 = $this_batch->[_rK_to_go_];
        my $ris_seqno_controlling_ci = $self->[_ris_seqno_controlling_ci_];

        my $rLL = $self->[_rLL_];

        # This routine is only called from sub flush_batch_of_code, so that
        # routine is a better spot for debugging.
        DEBUG_GRIND && do {
            my $token = my $type = "";
            if ( $max_index_to_go >= 0 ) {
                $token = $tokens_to_go[$max_index_to_go];
                $type  = $types_to_go[$max_index_to_go];
            }
            my $output_str = join "", @tokens_to_go[ 0 .. $max_index_to_go ];
            print STDERR <<EOM;
grind got batch number $batch_count with $max_index_to_go tokens, last type '$type' tok='$token', text:
$output_str
EOM
        };

        # Safety check - shouldn't happen. The calling routine must not call
        # here unless there are tokens in the batch to be processed.  This
        # fault can only be triggered by a recent programming change.
        if ( $max_index_to_go < 0 ) {
            Fault(
"sub grind incorrectly called with max_index_to_go=$max_index_to_go"
            );
        }

        # Initialize some batch variables
        my $comma_count_in_batch = 0;
        my $ilast_nonblank       = -1;
        my @colon_list;
        my @ix_seqno_controlling_ci;
        for ( my $i = 0 ; $i <= $max_index_to_go ; $i++ ) {
            $bond_strength_to_go[$i] = 0;
            $iprev_to_go[$i]         = $ilast_nonblank;
            $inext_to_go[$i]         = $i + 1;

            my $type = $types_to_go[$i];
            if ( $type ne 'b' ) {
                if ( $ilast_nonblank >= 0 ) {
                    $inext_to_go[$ilast_nonblank] = $i;

                    # just in case there are two blanks in a row (shouldn't
                    # happen)
                    if ( ++$ilast_nonblank < $i ) {
                        $inext_to_go[$ilast_nonblank] = $i;
                    }
                }
                $ilast_nonblank = $i;

                # This is a good spot to efficiently collect information needed
                # for breaking lines...

                if ( $type eq ',' ) { $comma_count_in_batch++; }

                # gather info needed by sub set_continuation_breaks
                my $seqno = $type_sequence_to_go[$i];
                if ($seqno) {

                    # remember indexes of any tokens controlling xci
                    # in this batch. This list is needed by sub undo_ci.
                    if ( $ris_seqno_controlling_ci->{$seqno} ) {
                        push @ix_seqno_controlling_ci, $i;
                    }

                    if ( $type eq '?' ) {
                        push @colon_list, $type;
                    }
                    elsif ( $type eq ':' ) {
                        push @colon_list, $type;
                    }
                }
            }
        }

        my $comma_arrow_count_contained =
          $self->match_opening_and_closing_tokens();

        # tell the -lp option we are outputting a batch so it can close
        # any unfinished items in its stack
        finish_lp_batch();

        # If this line ends in a code block brace, set breaks at any
        # previous closing code block braces to breakup a chain of code
        # blocks on one line.  This is very rare but can happen for
        # user-defined subs.  For example we might be looking at this:
        #  BOOL { $server_data{uptime} > 0; } NUM { $server_data{load}; } STR {
        my $saw_good_break = 0;    # flag to force breaks even if short line
        if (

            # looking for opening or closing block brace
            $block_type_to_go[$max_index_to_go]

            # never any good breaks if just one token
            && $max_index_to_go > 0

            # but not one of these which are never duplicated on a line:
            # until|while|for|if|elsif|else
            && !$is_block_without_semicolon{ $block_type_to_go[$max_index_to_go]
            }
          )
        {
            my $lev = $nesting_depth_to_go[$max_index_to_go];

            # Walk backwards from the end and
            # set break at any closing block braces at the same level.
            # But quit if we are not in a chain of blocks.
            for ( my $i = $max_index_to_go - 1 ; $i >= 0 ; $i-- ) {
                last if ( $levels_to_go[$i] < $lev );   # stop at a lower level
                next if ( $levels_to_go[$i] > $lev );   # skip past higher level

                if ( $block_type_to_go[$i] ) {
                    if ( $tokens_to_go[$i] eq '}' ) {
                        $self->set_forced_breakpoint($i);
                        $saw_good_break = 1;
                    }
                }

                # quit if we see anything besides words, function, blanks
                # at this level
                elsif ( $types_to_go[$i] !~ /^[\(\)Gwib]$/ ) { last }
            }
        }

        my $imin = 0;
        my $imax = $max_index_to_go;

        # trim any blank tokens
        if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
        if ( $types_to_go[$imax] eq 'b' ) { $imax-- }

        # anything left to write?
        if ( $imin <= $imax ) {

            my $last_line_leading_type  = $self->[_last_line_leading_type_];
            my $last_line_leading_level = $self->[_last_line_leading_level_];
            my $last_last_line_leading_level =
              $self->[_last_last_line_leading_level_];

            # add a blank line before certain key types but not after a comment
            if ( $last_line_leading_type ne '#' ) {
                my $want_blank    = 0;
                my $leading_token = $tokens_to_go[$imin];
                my $leading_type  = $types_to_go[$imin];

                # blank lines before subs except declarations and one-liners
                if ( $leading_type eq 'i' ) {
                    if ( $leading_token =~ /$SUB_PATTERN/ ) {
                        $want_blank = $rOpts->{'blank-lines-before-subs'}
                          if ( terminal_type_i( $imin, $imax ) !~ /^[\;\}]$/ );
                    }

                    # break before all package declarations
                    elsif ( substr( $leading_token, 0, 8 ) eq 'package ' ) {
                        $want_blank = $rOpts->{'blank-lines-before-packages'};
                    }
                }

                # break before certain key blocks except one-liners
                if ( $leading_type eq 'k' ) {
                    if ( $leading_token eq 'BEGIN' || $leading_token eq 'END' )
                    {
                        $want_blank = $rOpts->{'blank-lines-before-subs'}
                          if ( terminal_type_i( $imin, $imax ) ne '}' );
                    }

                    # Break before certain block types if we haven't had a
                    # break at this level for a while.  This is the
                    # difficult decision..
                    elsif ($last_line_leading_type ne 'b'
                        && $leading_token =~
                        /^(unless|if|while|until|for|foreach)$/ )
                    {
                        my $lc =
                          $nonblank_lines_at_depth[$last_line_leading_level];
                        if ( !defined($lc) ) { $lc = 0 }

                       # patch for RT #128216: no blank line inserted at a level
                       # change
                        if ( $levels_to_go[$imin] != $last_line_leading_level )
                        {
                            $lc = 0;
                        }

                        $want_blank =
                             $rOpts->{'blanks-before-blocks'}
                          && $lc >= $rOpts->{'long-block-line-count'}
                          && $self->consecutive_nonblank_lines() >=
                          $rOpts->{'long-block-line-count'}
                          && terminal_type_i( $imin, $imax ) ne '}';
                    }
                }

                # Check for blank lines wanted before a closing brace
                if ( $leading_token eq '}' ) {
                    if (   $rOpts->{'blank-lines-before-closing-block'}
                        && $block_type_to_go[$imin]
                        && $block_type_to_go[$imin] =~
                        /$blank_lines_before_closing_block_pattern/ )
                    {
                        my $nblanks =
                          $rOpts->{'blank-lines-before-closing-block'};
                        if ( $nblanks > $want_blank ) {
                            $want_blank = $nblanks;
                        }
                    }
                }

                if ($want_blank) {

                   # future: send blank line down normal path to VerticalAligner
                    $self->flush_vertical_aligner();
                    $file_writer_object->require_blank_code_lines($want_blank);
                }
            }

            # update blank line variables and count number of consecutive
            # non-blank, non-comment lines at this level
            $last_last_line_leading_level = $last_line_leading_level;
            $last_line_leading_level      = $levels_to_go[$imin];
            if ( $last_line_leading_level < 0 ) { $last_line_leading_level = 0 }
            $last_line_leading_type = $types_to_go[$imin];
            if (   $last_line_leading_level == $last_last_line_leading_level
                && $last_line_leading_type ne 'b'
                && $last_line_leading_type ne '#'
                && defined( $nonblank_lines_at_depth[$last_line_leading_level] )
              )
            {
                $nonblank_lines_at_depth[$last_line_leading_level]++;
            }
            else {
                $nonblank_lines_at_depth[$last_line_leading_level] = 1;
            }

            $self->[_last_line_leading_type_]  = $last_line_leading_type;
            $self->[_last_line_leading_level_] = $last_line_leading_level;
            $self->[_last_last_line_leading_level_] =
              $last_last_line_leading_level;

            # Flag to remember if we called sub 'pad_array_to_go'.
            # Some routines (scan_list(), set_continuation_breaks() ) need some
            # extra tokens added at the end of the batch.  Most batches do not
            # use these routines, so we will avoid calling 'pad_array_to_go'
            # unless it is needed.
            my $called_pad_array_to_go;

            # set all forced breakpoints for good list formatting
            my $is_long_line = $max_index_to_go > 0
              && $self->excess_line_length( $imin, $max_index_to_go ) > 0;

            my $old_line_count_in_batch =
              $max_index_to_go == 0
              ? 1
              : $self->get_old_line_count( $K_to_go[0],
                $K_to_go[$max_index_to_go] );

            if (
                   $is_long_line
                || $old_line_count_in_batch > 1

                # must always call scan_list() with unbalanced batches because
                # it is maintaining some stacks
                || is_unbalanced_batch()

                # call scan_list if we might want to break at commas
                || (
                    $comma_count_in_batch
                    && (   $rOpts_maximum_fields_per_table > 0
                        || $rOpts_comma_arrow_breakpoints == 0 )
                )

                # call scan_list if user may want to break open some one-line
                # hash references
                || (   $comma_arrow_count_contained
                    && $rOpts_comma_arrow_breakpoints != 3 )
              )
            {
                # add a couple of extra terminal blank tokens
                $self->pad_array_to_go();
                $called_pad_array_to_go = 1;

                ## This caused problems in one version of perl for unknown reasons:
                ## $saw_good_break ||= scan_list();
                my $sgb = $self->scan_list($is_long_line);
                $saw_good_break ||= $sgb;
            }

            # let $ri_first and $ri_last be references to lists of
            # first and last tokens of line fragments to output..
            my ( $ri_first, $ri_last );

            # write a single line if..
            if (

                # we aren't allowed to add any newlines
                !$rOpts_add_newlines

                # or,
                || (

                    # this line is 'short'
                    !$is_long_line

                    # and we didn't see a good breakpoint
                    && !$saw_good_break

                    # and we don't already have an interior breakpoint
                    && !get_forced_breakpoint_count()
                )
              )
            {
                @{$ri_first} = ($imin);
                @{$ri_last}  = ($imax);
            }

            # otherwise use multiple lines
            else {

                # add a couple of extra terminal blank tokens if we haven't
                # already done so
                $self->pad_array_to_go() unless ($called_pad_array_to_go);

                ( $ri_first, $ri_last ) =
                  $self->set_continuation_breaks( $saw_good_break,
                    \@colon_list );

                $self->break_all_chain_tokens( $ri_first, $ri_last );

                $self->break_equals( $ri_first, $ri_last );

                # now we do a correction step to clean this up a bit
                # (The only time we would not do this is for debugging)
                if ($rOpts_recombine) {
                    ( $ri_first, $ri_last ) =
                      $self->recombine_breakpoints( $ri_first, $ri_last );
                }

                $self->insert_final_ternary_breaks( $ri_first, $ri_last )
                  if (@colon_list);
            }

            $self->insert_breaks_before_list_opening_containers( $ri_first,
                $ri_last )
              if ( %break_before_container_types && $max_index_to_go > 0 );

            # do corrector step if -lp option is used
            my $do_not_pad = 0;
            if ($rOpts_line_up_parentheses) {
                $do_not_pad =
                  $self->correct_lp_indentation( $ri_first, $ri_last );
            }

            # unmask any invisible line-ending semicolon.  They were placed by
            # sub respace_tokens but we only now know if we actually need them.
            if ( !$tokens_to_go[$imax] && $types_to_go[$imax] eq ';' ) {
                my $i       = $imax;
                my $tok     = ';';
                my $tok_len = 1;
                if ( $want_left_space{';'} != WS_NO ) {
                    $tok     = ' ;';
                    $tok_len = 2;
                }
                $tokens_to_go[$i]        = $tok;
                $token_lengths_to_go[$i] = $tok_len;
                my $KK = $K_to_go[$i];
                $rLL->[$KK]->[_TOKEN_]        = $tok;
                $rLL->[$KK]->[_TOKEN_LENGTH_] = $tok_len;
                my $line_number = 1 + $self->get_old_line_index($KK);
                $self->note_added_semicolon($line_number);
            }

            if ( $rOpts_one_line_block_semicolons == 0 ) {
                $self->delete_one_line_semicolons( $ri_first, $ri_last );
            }

            # The line breaks for this batch of code have been finalized. Now we
            # can to package the results for further processing.  We will switch
            # from the local '_to_go' buffer arrays (i-index) back to the global
            # token arrays (K-index) at this point.
            my $rlines_K;
            my $index_error;
            for ( my $n = 0 ; $n < @{$ri_first} ; $n++ ) {
                my $ibeg = $ri_first->[$n];
                my $Kbeg = $K_to_go[$ibeg];
                my $iend = $ri_last->[$n];
                my $Kend = $K_to_go[$iend];
                if ( $iend - $ibeg != $Kend - $Kbeg ) {
                    $index_error = $n unless defined($index_error);
                }
                push @{$rlines_K},
                  [ $Kbeg, $Kend, $forced_breakpoint_to_go[$iend] ];
            }

            # Check correctness of the mapping between the i and K token
            # indexes.  (The K index is the global index, the i index is the
            # batch index).  It is important to do this check because an error
            # would be disasterous.  The reason that we should never see an
            # index error here is that sub 'store_token_to_go' has a check to
            # make sure that the indexes in batches remain continuous.  Since
            # sub 'store_token_to_go' controls feeding tokens into batches, so
            # no index discrepancies should occur unless a recent programming
            # change has introduced a bug.
            if ( defined($index_error) ) {

                # Temporary debug code - should never get here
                for ( my $n = 0 ; $n < @{$ri_first} ; $n++ ) {
                    my $ibeg  = $ri_first->[$n];
                    my $Kbeg  = $K_to_go[$ibeg];
                    my $iend  = $ri_last->[$n];
                    my $Kend  = $K_to_go[$iend];
                    my $idiff = $iend - $ibeg;
                    my $Kdiff = $Kend - $Kbeg;
                    print STDERR <<EOM;
line $n, irange $ibeg-$iend = $idiff, Krange $Kbeg-$Kend = $Kdiff;
EOM
                }
                Fault(
                    "Index error at line $index_error; i and K ranges differ");
            }

            $this_batch->[_rlines_K_]        = $rlines_K;
            $this_batch->[_ibeg0_]           = $ri_first->[0];
            $this_batch->[_peak_batch_size_] = $peak_batch_size;
            $this_batch->[_do_not_pad_]      = $do_not_pad;
            $this_batch->[_batch_count_]     = $batch_count;
            $this_batch->[_rix_seqno_controlling_ci_] =
              \@ix_seqno_controlling_ci;

            $self->send_lines_to_vertical_aligner();

            # Insert any requested blank lines after an opening brace.  We have
            # to skip back before any side comment to find the terminal token
            my $iterm;
            for ( $iterm = $imax ; $iterm >= $imin ; $iterm-- ) {
                next if $types_to_go[$iterm] eq '#';
                next if $types_to_go[$iterm] eq 'b';
                last;
            }

            # write requested number of blank lines after an opening block brace
            if ( $iterm >= $imin && $types_to_go[$iterm] eq '{' ) {
                if (   $rOpts->{'blank-lines-after-opening-block'}
                    && $block_type_to_go[$iterm]
                    && $block_type_to_go[$iterm] =~
                    /$blank_lines_after_opening_block_pattern/ )
                {
                    my $nblanks = $rOpts->{'blank-lines-after-opening-block'};
                    $self->flush_vertical_aligner();
                    $file_writer_object->require_blank_code_lines($nblanks);
                }
            }
        }

        # Remember the largest batch size processed. This is needed by the
        # logical padding routine to avoid padding the first nonblank token
        if ( $max_index_to_go && $max_index_to_go > $peak_batch_size ) {
            $peak_batch_size = $max_index_to_go;
        }

        return;
    }
} ## end closure grind_batch_of_CODE

{    ## begin closure match_opening_and_closing_tokens

    # closure to keep track of unbalanced containers.
    # arrays shared by the routines in this block:
    my %saved_opening_indentation;
    my @unmatched_opening_indexes_in_this_batch;
    my @unmatched_closing_indexes_in_this_batch;
    my %comma_arrow_count;

    sub initialize_saved_opening_indentation {
        %saved_opening_indentation = ();
        return;
    }

    sub is_unbalanced_batch {
        return @unmatched_opening_indexes_in_this_batch +
          @unmatched_closing_indexes_in_this_batch;
    }

    sub match_opening_and_closing_tokens {

        # Match up indexes of opening and closing braces, etc, in this batch.
        # This has to be done after all tokens are stored because unstoring
        # of tokens would otherwise cause trouble.

        my ($self) = @_;

        @unmatched_opening_indexes_in_this_batch = ();
        @unmatched_closing_indexes_in_this_batch = ();
        %comma_arrow_count                       = ();
        my $comma_arrow_count_contained = 0;

        foreach my $i ( 0 .. $max_index_to_go ) {
            if ( $type_sequence_to_go[$i] ) {
                my $token = $tokens_to_go[$i];
                if ( $is_opening_sequence_token{$token} ) {
                    push @unmatched_opening_indexes_in_this_batch, $i;
                }
                elsif ( $is_closing_sequence_token{$token} ) {

                    my $i_mate = pop @unmatched_opening_indexes_in_this_batch;
                    if ( defined($i_mate) && $i_mate >= 0 ) {
                        if ( $type_sequence_to_go[$i_mate] ==
                            $type_sequence_to_go[$i] )
                        {
                            $mate_index_to_go[$i]      = $i_mate;
                            $mate_index_to_go[$i_mate] = $i;
                            my $seqno = $type_sequence_to_go[$i];
                            if ( $comma_arrow_count{$seqno} ) {
                                $comma_arrow_count_contained +=
                                  $comma_arrow_count{$seqno};
                            }
                        }
                        else {
                            push @unmatched_opening_indexes_in_this_batch,
                              $i_mate;
                            push @unmatched_closing_indexes_in_this_batch, $i;
                        }
                    }
                    else {
                        push @unmatched_closing_indexes_in_this_batch, $i;
                    }
                }
            }
            elsif ( $tokens_to_go[$i] eq '=>' ) {
                if (@unmatched_opening_indexes_in_this_batch) {
                    my $j     = $unmatched_opening_indexes_in_this_batch[-1];
                    my $seqno = $type_sequence_to_go[$j];
                    $comma_arrow_count{$seqno}++;
                }
            }
        }
        return $comma_arrow_count_contained;
    }

    sub save_opening_indentation {

        # This should be called after each batch of tokens is output. It
        # saves indentations of lines of all unmatched opening tokens.
        # These will be used by sub get_opening_indentation.

        my ( $self, $ri_first, $ri_last, $rindentation_list ) = @_;

        # QW INDENTATION PATCH 1:
        # Also save indentation for multiline qw quotes
        my @i_qw;
        my $seqno_qw_opening;
        if ( $types_to_go[$max_index_to_go] eq 'q' ) {
            my $KK = $K_to_go[$max_index_to_go];
            $seqno_qw_opening =
              $self->[_rstarting_multiline_qw_seqno_by_K_]->{$KK};
            if ($seqno_qw_opening) {
                push @i_qw, $max_index_to_go;
            }
        }

        # we need to save indentations of any unmatched opening tokens
        # in this batch because we may need them in a subsequent batch.
        foreach ( @unmatched_opening_indexes_in_this_batch, @i_qw ) {

            my $seqno = $type_sequence_to_go[$_];

            if ( !$seqno ) {
                if ( $seqno_qw_opening && $_ == $max_index_to_go ) {
                    $seqno = $seqno_qw_opening;
                }
                else {

                    # shouldn't happen
                    $seqno = 'UNKNOWN';
                }
            }

            $saved_opening_indentation{$seqno} = [
                lookup_opening_indentation(
                    $_, $ri_first, $ri_last, $rindentation_list
                )
            ];
        }
        return;
    }

    sub get_saved_opening_indentation {
        my ($seqno) = @_;
        my ( $indent, $offset, $is_leading, $exists ) = ( 0, 0, 0, 0 );

        if ($seqno) {
            if ( $saved_opening_indentation{$seqno} ) {
                ( $indent, $offset, $is_leading ) =
                  @{ $saved_opening_indentation{$seqno} };
                $exists = 1;
            }
        }

        # some kind of serious error it doesn't exist
        # (example is badfile.t)

        return ( $indent, $offset, $is_leading, $exists );
    }
} ## end closure match_opening_and_closing_tokens

sub lookup_opening_indentation {

    # get the indentation of the line in the current output batch
    # which output a selected opening token
    #
    # given:
    #   $i_opening - index of an opening token in the current output batch
    #                whose line indentation we need
    #   $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    #   $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    #   $rindentation_list - reference to a list containing the indentation
    #            used for each line.  (NOTE: the first slot in
    #            this list is the last returned line number, and this is
    #            followed by the list of indentations).
    #
    # return
    #   -the indentation of the line which contained token $i_opening
    #   -and its offset (number of columns) from the start of the line

    my ( $i_opening, $ri_start, $ri_last, $rindentation_list ) = @_;

    if ( !@{$ri_last} ) {

        # An error here implies a bug introduced by a recent program change.
        # Every batch of code has lines.
        Fault("Error in opening_indentation: no lines");
        return;
    }

    my $nline = $rindentation_list->[0];    # line number of previous lookup

    # reset line location if necessary
    $nline = 0 if ( $i_opening < $ri_start->[$nline] );

    # find the correct line
    unless ( $i_opening > $ri_last->[-1] ) {
        while ( $i_opening > $ri_last->[$nline] ) { $nline++; }
    }

    # Error - token index is out of bounds - shouldn't happen
    # A program bug has been introduced in one of the calling routines.
    # We better stop here.
    else {
        my $i_last_line = $ri_last->[-1];
        Fault(<<EOM);
Program bug in call to lookup_opening_indentation - index out of range
 called with index i_opening=$i_opening  > $i_last_line = max index of last line
This batch has max index = $max_index_to_go,
EOM
        report_definite_bug();    # old coding, will not get here
        $nline = $#{$ri_last};
    }

    $rindentation_list->[0] =
      $nline;                     # save line number to start looking next call
    my $ibeg       = $ri_start->[$nline];
    my $offset     = token_sequence_length( $ibeg, $i_opening ) - 1;
    my $is_leading = ( $ibeg == $i_opening );
    return ( $rindentation_list->[ $nline + 1 ], $offset, $is_leading );
}

{    ## begin closure terminal_type_i

    my %is_sort_map_grep_eval_do;

    BEGIN {
        my @q = qw(sort map grep eval do);
        @is_sort_map_grep_eval_do{@q} = (1) x scalar(@q);
    }

    sub terminal_type_i {

      #    returns type of last token on this line (terminal token), as follows:
      #    returns # for a full-line comment
      #    returns ' ' for a blank line
      #    otherwise returns final token type

        my ( $ibeg, $iend ) = @_;

        # Start at the end and work backwards
        my $i      = $iend;
        my $type_i = $types_to_go[$i];

        # Check for side comment
        if ( $type_i eq '#' ) {
            $i--;
            if ( $i < $ibeg ) {
                return wantarray ? ( $type_i, $ibeg ) : $type_i;
            }
            $type_i = $types_to_go[$i];
        }

        # Skip past a blank
        if ( $type_i eq 'b' ) {
            $i--;
            if ( $i < $ibeg ) {
                return wantarray ? ( $type_i, $ibeg ) : $type_i;
            }
            $type_i = $types_to_go[$i];
        }

        # Found it..make sure it is a BLOCK termination,
        # but hide a terminal } after sort/grep/map because it is not
        # necessarily the end of the line.  (terminal.t)
        my $block_type = $block_type_to_go[$i];
        if (
            $type_i eq '}'
            && ( !$block_type
                || ( $is_sort_map_grep_eval_do{$block_type} ) )
          )
        {
            $type_i = 'b';
        }
        return wantarray ? ( $type_i, $i ) : $type_i;
    }

} ## end closure terminal_type_i

sub pad_array_to_go {

    # To simplify coding in scan_list and set_bond_strengths, it helps to
    # create some extra blank tokens at the end of the arrays.  We also add
    # some undef's to help guard against using invalid data.
    my ($self) = @_;
    $K_to_go[ $max_index_to_go + 1 ]             = undef;
    $tokens_to_go[ $max_index_to_go + 1 ]        = '';
    $tokens_to_go[ $max_index_to_go + 2 ]        = '';
    $tokens_to_go[ $max_index_to_go + 3 ]        = undef;
    $types_to_go[ $max_index_to_go + 1 ]         = 'b';
    $types_to_go[ $max_index_to_go + 2 ]         = 'b';
    $types_to_go[ $max_index_to_go + 3 ]         = undef;
    $nesting_depth_to_go[ $max_index_to_go + 2 ] = undef;
    $nesting_depth_to_go[ $max_index_to_go + 1 ] =
      $nesting_depth_to_go[$max_index_to_go];

    #    /^[R\}\)\]]$/
    if ( $is_closing_type{ $types_to_go[$max_index_to_go] } ) {
        if ( $nesting_depth_to_go[$max_index_to_go] <= 0 ) {

            # Nesting depths are equivalent to the _SLEVEL_ variable which is
            # clipped to be >=0 in sub write_line, so it should not be possible
            # to get here unless the code has a bracing error which leaves a
            # closing brace with zero nesting depth.
            unless ( get_saw_brace_error() ) {
                warning(
"Program bug in pad_array_to_go: hit nesting error which should have been caught\n"
                );
                report_definite_bug();
            }
        }
        else {
            $nesting_depth_to_go[ $max_index_to_go + 1 ] -= 1;
        }
    }

    #       /^[L\{\(\[]$/
    elsif ( $is_opening_type{ $types_to_go[$max_index_to_go] } ) {
        $nesting_depth_to_go[ $max_index_to_go + 1 ] += 1;
    }
    return;
}

sub break_all_chain_tokens {

    # scan the current breakpoints looking for breaks at certain "chain
    # operators" (. : && || + etc) which often occur repeatedly in a long
    # statement.  If we see a break at any one, break at all similar tokens
    # within the same container.
    #
    my ( $self, $ri_left, $ri_right ) = @_;

    my %saw_chain_type;
    my %left_chain_type;
    my %right_chain_type;
    my %interior_chain_type;
    my $nmax = @{$ri_right} - 1;

    # scan the left and right end tokens of all lines
    my $count = 0;
    for my $n ( 0 .. $nmax ) {
        my $il    = $ri_left->[$n];
        my $ir    = $ri_right->[$n];
        my $typel = $types_to_go[$il];
        my $typer = $types_to_go[$ir];
        $typel = '+' if ( $typel eq '-' );    # treat + and - the same
        $typer = '+' if ( $typer eq '-' );
        $typel = '*' if ( $typel eq '/' );    # treat * and / the same
        $typer = '*' if ( $typer eq '/' );
        my $tokenl = $tokens_to_go[$il];
        my $tokenr = $tokens_to_go[$ir];

        if ( $is_chain_operator{$tokenl} && $want_break_before{$typel} ) {
            next if ( $typel eq '?' );
            push @{ $left_chain_type{$typel} }, $il;
            $saw_chain_type{$typel} = 1;
            $count++;
        }
        if ( $is_chain_operator{$tokenr} && !$want_break_before{$typer} ) {
            next if ( $typer eq '?' );
            push @{ $right_chain_type{$typer} }, $ir;
            $saw_chain_type{$typer} = 1;
            $count++;
        }
    }
    return unless $count;

    # now look for any interior tokens of the same types
    $count = 0;
    for my $n ( 0 .. $nmax ) {
        my $il = $ri_left->[$n];
        my $ir = $ri_right->[$n];
        foreach my $i ( $il + 1 .. $ir - 1 ) {
            my $type = $types_to_go[$i];
            $type = '+' if ( $type eq '-' );
            $type = '*' if ( $type eq '/' );
            if ( $saw_chain_type{$type} ) {
                push @{ $interior_chain_type{$type} }, $i;
                $count++;
            }
        }
    }
    return unless $count;

    # now make a list of all new break points
    my @insert_list;

    # loop over all chain types
    foreach my $type ( keys %saw_chain_type ) {

        # quit if just ONE continuation line with leading .  For example--
        # print LATEXFILE '\framebox{\parbox[c][' . $h . '][t]{' . $w . '}{'
        #  . $contents;
        last if ( $nmax == 1 && $type =~ /^[\.\+]$/ );

        # loop over all interior chain tokens
        foreach my $itest ( @{ $interior_chain_type{$type} } ) {

            # loop over all left end tokens of same type
            if ( $left_chain_type{$type} ) {
                next if $nobreak_to_go[ $itest - 1 ];
                foreach my $i ( @{ $left_chain_type{$type} } ) {
                    next unless $self->in_same_container_i( $i, $itest );
                    push @insert_list, $itest - 1;

                    # Break at matching ? if this : is at a different level.
                    # For example, the ? before $THRf_DEAD in the following
                    # should get a break if its : gets a break.
                    #
                    # my $flags =
                    #     ( $_ & 1 ) ? ( $_ & 4 ) ? $THRf_DEAD : $THRf_ZOMBIE
                    #   : ( $_ & 4 ) ? $THRf_R_DETACHED
                    #   :              $THRf_R_JOINABLE;
                    if (   $type eq ':'
                        && $levels_to_go[$i] != $levels_to_go[$itest] )
                    {
                        my $i_question = $mate_index_to_go[$itest];
                        if ( $i_question > 0 ) {
                            push @insert_list, $i_question - 1;
                        }
                    }
                    last;
                }
            }

            # loop over all right end tokens of same type
            if ( $right_chain_type{$type} ) {
                next if $nobreak_to_go[$itest];
                foreach my $i ( @{ $right_chain_type{$type} } ) {
                    next unless $self->in_same_container_i( $i, $itest );
                    push @insert_list, $itest;

                    # break at matching ? if this : is at a different level
                    if (   $type eq ':'
                        && $levels_to_go[$i] != $levels_to_go[$itest] )
                    {
                        my $i_question = $mate_index_to_go[$itest];
                        if ( $i_question >= 0 ) {
                            push @insert_list, $i_question;
                        }
                    }
                    last;
                }
            }
        }
    }

    # insert any new break points
    if (@insert_list) {
        $self->insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
    }
    return;
}

sub insert_additional_breaks {

    # this routine will add line breaks at requested locations after
    # sub set_continuation_breaks has made preliminary breaks.

    my ( $self, $ri_break_list, $ri_first, $ri_last ) = @_;
    my $i_f;
    my $i_l;
    my $line_number = 0;
    foreach my $i_break_left ( sort { $a <=> $b } @{$ri_break_list} ) {

        next if ( $nobreak_to_go[$i_break_left] );

        $i_f = $ri_first->[$line_number];
        $i_l = $ri_last->[$line_number];
        while ( $i_break_left >= $i_l ) {
            $line_number++;

            # shouldn't happen unless caller passes bad indexes
            if ( $line_number >= @{$ri_last} ) {
                warning(
"Non-fatal program bug: couldn't set break at $i_break_left\n"
                );
                report_definite_bug();
                return;
            }
            $i_f = $ri_first->[$line_number];
            $i_l = $ri_last->[$line_number];
        }

        # Do not leave a blank at the end of a line; back up if necessary
        if ( $types_to_go[$i_break_left] eq 'b' ) { $i_break_left-- }

        my $i_break_right = $inext_to_go[$i_break_left];
        if (   $i_break_left >= $i_f
            && $i_break_left < $i_l
            && $i_break_right > $i_f
            && $i_break_right <= $i_l )
        {
            splice( @{$ri_first}, $line_number, 1, ( $i_f, $i_break_right ) );
            splice( @{$ri_last},  $line_number, 1, ( $i_break_left, $i_l ) );
        }
    }
    return;
}

sub in_same_container_i {

    # check to see if tokens at i1 and i2 are in the
    # same container, and not separated by a comma, ? or :
    # This is an interface between the _to_go arrays to the rLL array
    my ( $self, $i1, $i2 ) = @_;
    return $self->in_same_container_K( $K_to_go[$i1], $K_to_go[$i2] );
}

{    ## begin closure in_same_container_K
    my $ris_break_token;
    my $ris_comma_token;

    BEGIN {

        # all cases break on seeing commas at same level
        my @q = qw( => );
        push @q, ',';
        @{$ris_comma_token}{@q} = (1) x scalar(@q);

        # Non-ternary text also breaks on seeing any of qw(? : || or )
        # Example: we would not want to break at any of these .'s
        #  : "<A HREF=\"#item_" . htmlify( 0, $s2 ) . "\">$str</A>"
        push @q, qw( or || ? : );
        @{$ris_break_token}{@q} = (1) x scalar(@q);
    }

    sub in_same_container_K {

        # Check to see if tokens at K1 and K2 are in the same container,
        # and not separated by certain characters: => , ? : || or
        # This version uses the newer $rLL data structure.

        my ( $self, $K1, $K2 ) = @_;
        if ( $K2 < $K1 ) { ( $K1, $K2 ) = ( $K2, $K1 ) }
        my $rLL     = $self->[_rLL_];
        my $depth_1 = $rLL->[$K1]->[_SLEVEL_];
        return if ( $depth_1 < 0 );
        return unless ( $rLL->[$K2]->[_SLEVEL_] == $depth_1 );

        # Select character set to scan for
        my $type_1 = $rLL->[$K1]->[_TYPE_];
        my $rbreak = ( $type_1 ne ':' ) ? $ris_break_token : $ris_comma_token;

        # Fast preliminary loop to verify that tokens are in the same container
        my $KK = $K1;
        while (1) {
            $KK = $rLL->[$KK]->[_KNEXT_SEQ_ITEM_];
            last if !defined($KK);
            last if ( $KK >= $K2 );
            my $depth_K = $rLL->[$KK]->[_SLEVEL_];
            return if ( $depth_K < $depth_1 );
            next   if ( $depth_K > $depth_1 );
            if ( $type_1 ne ':' ) {
                my $tok_K = $rLL->[$KK]->[_TOKEN_];
                return if ( $tok_K eq '?' || $tok_K eq ':' );
            }
        }

        # Slow loop checking for certain characters

        ###########################################################
        # This is potentially a slow routine and not critical.
        # For safety just give up for large differences.
        # See test file 'infinite_loop.txt'
        ###########################################################
        return if ( $K2 - $K1 > 200 );

        foreach my $K ( $K1 + 1 .. $K2 - 1 ) {

            my $depth_K = $rLL->[$K]->[_SLEVEL_];
            next   if ( $depth_K > $depth_1 );
            return if ( $depth_K < $depth_1 );    # redundant, checked above
            my $tok = $rLL->[$K]->[_TOKEN_];
            return if ( $rbreak->{$tok} );
        }
        return 1;
    }
} ## end closure in_same_container_K

sub break_equals {

    # Look for assignment operators that could use a breakpoint.
    # For example, in the following snippet
    #
    #    $HOME = $ENV{HOME}
    #      || $ENV{LOGDIR}
    #      || $pw[7]
    #      || die "no home directory for user $<";
    #
    # we could break at the = to get this, which is a little nicer:
    #    $HOME =
    #         $ENV{HOME}
    #      || $ENV{LOGDIR}
    #      || $pw[7]
    #      || die "no home directory for user $<";
    #
    # The logic here follows the logic in set_logical_padding, which
    # will add the padding in the second line to improve alignment.
    #
    my ( $self, $ri_left, $ri_right ) = @_;
    my $nmax = @{$ri_right} - 1;
    return unless ( $nmax >= 2 );

    # scan the left ends of first two lines
    my $tokbeg = "";
    my $depth_beg;
    for my $n ( 1 .. 2 ) {
        my $il     = $ri_left->[$n];
        my $typel  = $types_to_go[$il];
        my $tokenl = $tokens_to_go[$il];

        my $has_leading_op = ( $tokenl =~ /^\w/ )
          ? $is_chain_operator{$tokenl}    # + - * / : ? && ||
          : $is_chain_operator{$typel};    # and, or
        return unless ($has_leading_op);
        if ( $n > 1 ) {
            return
              unless ( $tokenl eq $tokbeg
                && $nesting_depth_to_go[$il] eq $depth_beg );
        }
        $tokbeg    = $tokenl;
        $depth_beg = $nesting_depth_to_go[$il];
    }

    # now look for any interior tokens of the same types
    my $il = $ri_left->[0];
    my $ir = $ri_right->[0];

    # now make a list of all new break points
    my @insert_list;
    for ( my $i = $ir - 1 ; $i > $il ; $i-- ) {
        my $type = $types_to_go[$i];
        if (   $is_assignment{$type}
            && $nesting_depth_to_go[$i] eq $depth_beg )
        {
            if ( $want_break_before{$type} ) {
                push @insert_list, $i - 1;
            }
            else {
                push @insert_list, $i;
            }
        }
    }

    # Break after a 'return' followed by a chain of operators
    #  return ( $^O !~ /win32|dos/i )
    #    && ( $^O ne 'VMS' )
    #    && ( $^O ne 'OS2' )
    #    && ( $^O ne 'MacOS' );
    # To give:
    #  return
    #       ( $^O !~ /win32|dos/i )
    #    && ( $^O ne 'VMS' )
    #    && ( $^O ne 'OS2' )
    #    && ( $^O ne 'MacOS' );
    my $i = 0;
    if (   $types_to_go[$i] eq 'k'
        && $tokens_to_go[$i] eq 'return'
        && $ir > $il
        && $nesting_depth_to_go[$i] eq $depth_beg )
    {
        push @insert_list, $i;
    }

    return unless (@insert_list);

    # One final check...
    # scan second and third lines and be sure there are no assignments
    # we want to avoid breaking at an = to make something like this:
    #    unless ( $icon =
    #           $html_icons{"$type-$state"}
    #        or $icon = $html_icons{$type}
    #        or $icon = $html_icons{$state} )
    for my $n ( 1 .. 2 ) {
        my $il = $ri_left->[$n];
        my $ir = $ri_right->[$n];
        foreach my $i ( $il + 1 .. $ir ) {
            my $type = $types_to_go[$i];
            return
              if ( $is_assignment{$type}
                && $nesting_depth_to_go[$i] eq $depth_beg );
        }
    }

    # ok, insert any new break point
    if (@insert_list) {
        $self->insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
    }
    return;
}

{    ## begin closure recombine_breakpoints

    # This routine is called once per batch to see if it would be better
    # to combine some of the lines into which the batch has been broken.

    my %is_amp_amp;
    my %is_ternary;
    my %is_math_op;
    my %is_plus_minus;
    my %is_mult_div;

    BEGIN {

        my @q;
        @q = qw( && || );
        @is_amp_amp{@q} = (1) x scalar(@q);

        @q = qw( ? : );
        @is_ternary{@q} = (1) x scalar(@q);

        @q = qw( + - * / );
        @is_math_op{@q} = (1) x scalar(@q);

        @q = qw( + - );
        @is_plus_minus{@q} = (1) x scalar(@q);

        @q = qw( * / );
        @is_mult_div{@q} = (1) x scalar(@q);
    }

    sub Debug_dump_breakpoints {

        # Debug routine to dump current breakpoints...not normally called
        # We are given indexes to the current lines:
        # $ri_beg = ref to array of BEGinning indexes of each line
        # $ri_end = ref to array of ENDing indexes of each line
        my ( $self, $ri_beg, $ri_end, $msg ) = @_;
        print STDERR "----Dumping breakpoints from: $msg----\n";
        for my $n ( 0 .. @{$ri_end} - 1 ) {
            my $ibeg = $ri_beg->[$n];
            my $iend = $ri_end->[$n];
            my $text = "";
            foreach my $i ( $ibeg .. $iend ) {
                $text .= $tokens_to_go[$i];
            }
            print STDERR "$n ($ibeg:$iend) $text\n";
        }
        print STDERR "----\n";
        return;
    }

    sub delete_one_line_semicolons {

        my ( $self, $ri_beg, $ri_end ) = @_;
        my $rLL                 = $self->[_rLL_];
        my $K_opening_container = $self->[_K_opening_container_];

        # Walk down the lines of this batch and delete any semicolons
        # terminating one-line blocks;
        my $nmax = @{$ri_end} - 1;

        foreach my $n ( 0 .. $nmax ) {
            my $i_beg    = $ri_beg->[$n];
            my $i_e      = $ri_end->[$n];
            my $K_beg    = $K_to_go[$i_beg];
            my $K_e      = $K_to_go[$i_e];
            my $K_end    = $K_e;
            my $type_end = $rLL->[$K_end]->[_TYPE_];
            if ( $type_end eq '#' ) {
                $K_end = $self->K_previous_nonblank($K_end);
                if ( defined($K_end) ) { $type_end = $rLL->[$K_end]->[_TYPE_]; }
            }

            # we are looking for a line ending in closing brace
            next
              unless ( $type_end eq '}' && $rLL->[$K_end]->[_TOKEN_] eq '}' );

            # ...and preceded by a semicolon on the same line
            my $K_semicolon = $self->K_previous_nonblank($K_end);
            next unless defined($K_semicolon);
            my $i_semicolon = $i_beg + ( $K_semicolon - $K_beg );
            next if ( $i_semicolon <= $i_beg );
            next unless ( $rLL->[$K_semicolon]->[_TYPE_] eq ';' );

            # Safety check - shouldn't happen - not critical
            # This is not worth throwing a Fault, except in DEVEL_MODE
            if ( $types_to_go[$i_semicolon] ne ';' ) {
                DEVEL_MODE
                  && Fault("unexpected type looking for semicolon");
                next;
            }

            # ... with the corresponding opening brace on the same line
            my $type_sequence = $rLL->[$K_end]->[_TYPE_SEQUENCE_];
            my $K_opening     = $K_opening_container->{$type_sequence};
            next unless ( defined($K_opening) );
            my $i_opening = $i_beg + ( $K_opening - $K_beg );
            next if ( $i_opening < $i_beg );

            # ... and only one semicolon between these braces
            my $semicolon_count = 0;
            foreach my $K ( $K_opening + 1 .. $K_semicolon - 1 ) {
                if ( $rLL->[$K]->[_TYPE_] eq ';' ) {
                    $semicolon_count++;
                    last;
                }
            }
            next if ($semicolon_count);

            # ...ok, then make the semicolon invisible
            $tokens_to_go[$i_semicolon]            = "";
            $token_lengths_to_go[$i_semicolon]     = 0;
            $rLL->[$K_semicolon]->[_TOKEN_]        = "";
            $rLL->[$K_semicolon]->[_TOKEN_LENGTH_] = 0;
        }
        return;
    }

    sub recombine_breakpoints {

        # sub set_continuation_breaks is very liberal in setting line breaks
        # for long lines, always setting breaks at good breakpoints, even
        # when that creates small lines.  Sometimes small line fragments
        # are produced which would look better if they were combined.
        # That's the task of this routine.
        #
        # We are given indexes to the current lines:
        # $ri_beg = ref to array of BEGinning indexes of each line
        # $ri_end = ref to array of ENDing indexes of each line
        my ( $self, $ri_beg, $ri_end ) = @_;

        my $rOpts_short_concatenation_item_length =
          $rOpts->{'short-concatenation-item-length'};

        # Make a list of all good joining tokens between the lines
        # n-1 and n.
        my @joint;
        my $nmax = @{$ri_end} - 1;
        for my $n ( 1 .. $nmax ) {
            my $ibeg_1 = $ri_beg->[ $n - 1 ];
            my $iend_1 = $ri_end->[ $n - 1 ];
            my $iend_2 = $ri_end->[$n];
            my $ibeg_2 = $ri_beg->[$n];

            my ( $itok, $itokp, $itokm );

            foreach my $itest ( $iend_1, $ibeg_2 ) {
                my $type = $types_to_go[$itest];
                if (   $is_math_op{$type}
                    || $is_amp_amp{$type}
                    || $is_assignment{$type}
                    || $type eq ':' )
                {
                    $itok = $itest;
                }
            }
            $joint[$n] = [$itok];
        }

        my $more_to_do = 1;

        # We keep looping over all of the lines of this batch
        # until there are no more possible recombinations
        my $nmax_last = @{$ri_end};
        my $reverse   = 0;
        while ($more_to_do) {
            my $n_best = 0;
            my $bs_best;
            my $nmax = @{$ri_end} - 1;

            # Safety check for infinite loop
            unless ( $nmax < $nmax_last ) {

                # Shouldn't happen because splice below decreases nmax on each
                # iteration.  An error can only be due to a recent programming
                # change.
                Fault("Program bug-infinite loop in recombine breakpoints\n");
            }
            $nmax_last  = $nmax;
            $more_to_do = 0;
            my $skip_Section_3;
            my $leading_amp_count = 0;
            my $this_line_is_semicolon_terminated;

            # loop over all remaining lines in this batch
            for my $iter ( 1 .. $nmax ) {

                # alternating sweep direction gives symmetric results
                # for recombining lines which exceed the line length
                # such as eval {{{{.... }}}}
                my $n;
                if   ($reverse) { $n = 1 + $nmax - $iter; }
                else            { $n = $iter }

                #----------------------------------------------------------
                # If we join the current pair of lines,
                # line $n-1 will become the left part of the joined line
                # line $n will become the right part of the joined line
                #
                # Here are Indexes of the endpoint tokens of the two lines:
                #
                #  -----line $n-1--- | -----line $n-----
                #  $ibeg_1   $iend_1 | $ibeg_2   $iend_2
                #                    ^
                #                    |
                # We want to decide if we should remove the line break
                # between the tokens at $iend_1 and $ibeg_2
                #
                # We will apply a number of ad-hoc tests to see if joining
                # here will look ok.  The code will just issue a 'next'
                # command if the join doesn't look good.  If we get through
                # the gauntlet of tests, the lines will be recombined.
                #----------------------------------------------------------
                #
                # beginning and ending tokens of the lines we are working on
                my $ibeg_1    = $ri_beg->[ $n - 1 ];
                my $iend_1    = $ri_end->[ $n - 1 ];
                my $iend_2    = $ri_end->[$n];
                my $ibeg_2    = $ri_beg->[$n];
                my $ibeg_nmax = $ri_beg->[$nmax];

                # combined line cannot be too long
                my $excess = $self->excess_line_length( $ibeg_1, $iend_2, 1 );
                next if ( $excess > 0 );

                my $type_iend_1 = $types_to_go[$iend_1];
                my $type_iend_2 = $types_to_go[$iend_2];
                my $type_ibeg_1 = $types_to_go[$ibeg_1];
                my $type_ibeg_2 = $types_to_go[$ibeg_2];

                # terminal token of line 2 if any side comment is ignored:
                my $iend_2t      = $iend_2;
                my $type_iend_2t = $type_iend_2;

                # some beginning indexes of other lines, which may not exist
                my $ibeg_0 = $n > 1          ? $ri_beg->[ $n - 2 ] : -1;
                my $ibeg_3 = $n < $nmax      ? $ri_beg->[ $n + 1 ] : -1;
                my $ibeg_4 = $n + 2 <= $nmax ? $ri_beg->[ $n + 2 ] : -1;

                my $bs_tweak = 0;

                #my $depth_increase=( $nesting_depth_to_go[$ibeg_2] -
                #        $nesting_depth_to_go[$ibeg_1] );

                0 && do {
                    print STDERR
"RECOMBINE: n=$n imid=$iend_1 if=$ibeg_1 type=$type_ibeg_1 =$tokens_to_go[$ibeg_1] next_type=$type_ibeg_2 next_tok=$tokens_to_go[$ibeg_2]\n";
                };

                # If line $n is the last line, we set some flags and
                # do any special checks for it
                if ( $n == $nmax ) {

                    # a terminal '{' should stay where it is
                    # unless preceded by a fat comma
                    next if ( $type_ibeg_2 eq '{' && $type_iend_1 ne '=>' );

                    if (   $type_iend_2 eq '#'
                        && $iend_2 - $ibeg_2 >= 2
                        && $types_to_go[ $iend_2 - 1 ] eq 'b' )
                    {
                        $iend_2t      = $iend_2 - 2;
                        $type_iend_2t = $types_to_go[$iend_2t];
                    }

                    $this_line_is_semicolon_terminated = $type_iend_2t eq ';';
                }

                #----------------------------------------------------------
                # Recombine Section 0:
                # Examine the special token joining this line pair, if any.
                # Put as many tests in this section to avoid duplicate code and
                # to make formatting independent of whether breaks are to the
                # left or right of an operator.
                #----------------------------------------------------------

                my ($itok) = @{ $joint[$n] };
                if ($itok) {

                    # FIXME: Patch - may not be necessary
                    my $iend_1 =
                        $type_iend_1 eq 'b'
                      ? $iend_1 - 1
                      : $iend_1;

                    my $iend_2 =
                        $type_iend_2 eq 'b'
                      ? $iend_2 - 1
                      : $iend_2;
                    ## END PATCH

                    my $type = $types_to_go[$itok];

                    if ( $type eq ':' ) {

                   # do not join at a colon unless it disobeys the break request
                        if ( $itok eq $iend_1 ) {
                            next unless $want_break_before{$type};
                        }
                        else {
                            $leading_amp_count++;
                            next if $want_break_before{$type};
                        }
                    } ## end if ':'

                    # handle math operators + - * /
                    elsif ( $is_math_op{$type} ) {

                        # Combine these lines if this line is a single
                        # number, or if it is a short term with same
                        # operator as the previous line.  For example, in
                        # the following code we will combine all of the
                        # short terms $A, $B, $C, $D, $E, $F, together
                        # instead of leaving them one per line:
                        #  my $time =
                        #    $A * $B * $C * $D * $E * $F *
                        #    ( 2. * $eps * $sigma * $area ) *
                        #    ( 1. / $tcold**3 - 1. / $thot**3 );

                        # This can be important in math-intensive code.

                        my $good_combo;

                        my $itokp  = min( $inext_to_go[$itok],  $iend_2 );
                        my $itokpp = min( $inext_to_go[$itokp], $iend_2 );
                        my $itokm  = max( $iprev_to_go[$itok],  $ibeg_1 );
                        my $itokmm = max( $iprev_to_go[$itokm], $ibeg_1 );

                        # check for a number on the right
                        if ( $types_to_go[$itokp] eq 'n' ) {

                            # ok if nothing else on right
                            if ( $itokp == $iend_2 ) {
                                $good_combo = 1;
                            }
                            else {

                                # look one more token to right..
                                # okay if math operator or some termination
                                $good_combo =
                                  ( ( $itokpp == $iend_2 )
                                      && $is_math_op{ $types_to_go[$itokpp] } )
                                  || $types_to_go[$itokpp] =~ /^[#,;]$/;
                            }
                        }

                        # check for a number on the left
                        if ( !$good_combo && $types_to_go[$itokm] eq 'n' ) {

                            # okay if nothing else to left
                            if ( $itokm == $ibeg_1 ) {
                                $good_combo = 1;
                            }

                            # otherwise look one more token to left
                            else {

                                # okay if math operator, comma, or assignment
                                $good_combo = ( $itokmm == $ibeg_1 )
                                  && ( $is_math_op{ $types_to_go[$itokmm] }
                                    || $types_to_go[$itokmm] =~ /^[,]$/
                                    || $is_assignment{ $types_to_go[$itokmm] }
                                  );
                            }
                        }

                        # look for a single short token either side of the
                        # operator
                        if ( !$good_combo ) {

                            # Slight adjustment factor to make results
                            # independent of break before or after operator in
                            # long summed lists.  (An operator and a space make
                            # two spaces).
                            my $two = ( $itok eq $iend_1 ) ? 2 : 0;

                            $good_combo =

                              # numbers or id's on both sides of this joint
                              $types_to_go[$itokp] =~ /^[in]$/
                              && $types_to_go[$itokm] =~ /^[in]$/

                              # one of the two lines must be short:
                              && (
                                (
                                    # no more than 2 nonblank tokens right of
                                    # joint
                                    $itokpp == $iend_2

                                    # short
                                    && token_sequence_length( $itokp, $iend_2 )
                                    < $two +
                                    $rOpts_short_concatenation_item_length
                                )
                                || (
                                    # no more than 2 nonblank tokens left of
                                    # joint
                                    $itokmm == $ibeg_1

                                    # short
                                    && token_sequence_length( $ibeg_1, $itokm )
                                    < 2 - $two +
                                    $rOpts_short_concatenation_item_length
                                )

                              )

                              # keep pure terms; don't mix +- with */
                              && !(
                                $is_plus_minus{$type}
                                && (   $is_mult_div{ $types_to_go[$itokmm] }
                                    || $is_mult_div{ $types_to_go[$itokpp] } )
                              )
                              && !(
                                $is_mult_div{$type}
                                && (   $is_plus_minus{ $types_to_go[$itokmm] }
                                    || $is_plus_minus{ $types_to_go[$itokpp] } )
                              )

                              ;
                        }

                        # it is also good to combine if we can reduce to 2 lines
                        if ( !$good_combo ) {

                            # index on other line where same token would be in a
                            # long chain.
                            my $iother =
                              ( $itok == $iend_1 ) ? $iend_2 : $ibeg_1;

                            $good_combo =
                                 $n == 2
                              && $n == $nmax
                              && $types_to_go[$iother] ne $type;
                        }

                        next unless ($good_combo);

                    } ## end math

                    elsif ( $is_amp_amp{$type} ) {
                        ##TBD
                    } ## end &&, ||

                    elsif ( $is_assignment{$type} ) {
                        ##TBD
                    } ## end assignment
                }

                #----------------------------------------------------------
                # Recombine Section 1:
                # Join welded nested containers immediately
                #----------------------------------------------------------

                if (
                    $type_sequence_to_go[$iend_1]
                    && $self->weld_len_right( $type_sequence_to_go[$iend_1],
                        $type_iend_1 )

                    || $type_sequence_to_go[$ibeg_2] && $self->weld_len_left(
                        $type_sequence_to_go[$ibeg_2], $type_ibeg_2
                    )
                  )
                {
                    $n_best = $n;
                    last;
                }

                $reverse = 0;

                #----------------------------------------------------------
                # Recombine Section 2:
                # Examine token at $iend_1 (right end of first line of pair)
                #----------------------------------------------------------

                # an isolated '}' may join with a ';' terminated segment
                if ( $type_iend_1 eq '}' ) {

                    # Check for cases where combining a semicolon terminated
                    # statement with a previous isolated closing paren will
                    # allow the combined line to be outdented.  This is
                    # generally a good move.  For example, we can join up
                    # the last two lines here:
                    #  (
                    #      $dev,  $ino,   $mode,  $nlink, $uid,     $gid, $rdev,
                    #      $size, $atime, $mtime, $ctime, $blksize, $blocks
                    #    )
                    #    = stat($file);
                    #
                    # to get:
                    #  (
                    #      $dev,  $ino,   $mode,  $nlink, $uid,     $gid, $rdev,
                    #      $size, $atime, $mtime, $ctime, $blksize, $blocks
                    #  ) = stat($file);
                    #
                    # which makes the parens line up.
                    #
                    # Another example, from Joe Matarazzo, probably looks best
                    # with the 'or' clause appended to the trailing paren:
                    #  $self->some_method(
                    #      PARAM1 => 'foo',
                    #      PARAM2 => 'bar'
                    #  ) or die "Some_method didn't work";
                    #
                    # But we do not want to do this for something like the -lp
                    # option where the paren is not outdentable because the
                    # trailing clause will be far to the right.
                    #
                    # The logic here is synchronized with the logic in sub
                    # sub set_adjusted_indentation, which actually does
                    # the outdenting.
                    #
                    $skip_Section_3 ||= $this_line_is_semicolon_terminated

                      # only one token on last line
                      && $ibeg_1 == $iend_1

                      # must be structural paren
                      && $tokens_to_go[$iend_1] eq ')'

                      # style must allow outdenting,
                      && !$closing_token_indentation{')'}

                      # only leading '&&', '||', and ':' if no others seen
                      # (but note: our count made below could be wrong
                      # due to intervening comments)
                      && ( $leading_amp_count == 0
                        || $type_ibeg_2 !~ /^(:|\&\&|\|\|)$/ )

                      # but leading colons probably line up with a
                      # previous colon or question (count could be wrong).
                      && $type_ibeg_2 ne ':'

                      # only one step in depth allowed.  this line must not
                      # begin with a ')' itself.
                      && ( $nesting_depth_to_go[$iend_1] ==
                        $nesting_depth_to_go[$iend_2] + 1 );

                    # YVES patch 2 of 2:
                    # Allow cuddled eval chains, like this:
                    #   eval {
                    #       #STUFF;
                    #       1; # return true
                    #   } or do {
                    #       #handle error
                    #   };
                    # This patch works together with a patch in
                    # setting adjusted indentation (where the closing eval
                    # brace is outdented if possible).
                    # The problem is that an 'eval' block has continuation
                    # indentation and it looks better to undo it in some
                    # cases.  If we do not use this patch we would get:
                    #   eval {
                    #       #STUFF;
                    #       1; # return true
                    #       }
                    #       or do {
                    #       #handle error
                    #     };
                    # The alternative, for uncuddled style, is to create
                    # a patch in set_adjusted_indentation which undoes
                    # the indentation of a leading line like 'or do {'.
                    # This doesn't work well with -icb through
                    if (
                           $block_type_to_go[$iend_1] eq 'eval'
                        && !$rOpts->{'line-up-parentheses'}
                        && !$rOpts->{'indent-closing-brace'}
                        && $tokens_to_go[$iend_2] eq '{'
                        && (
                            ( $type_ibeg_2 =~ /^(|\&\&|\|\|)$/ )
                            || (   $type_ibeg_2 eq 'k'
                                && $is_and_or{ $tokens_to_go[$ibeg_2] } )
                            || $is_if_unless{ $tokens_to_go[$ibeg_2] }
                        )
                      )
                    {
                        $skip_Section_3 ||= 1;
                    }

                    next
                      unless (
                        $skip_Section_3

                        # handle '.' and '?' specially below
                        || ( $type_ibeg_2 =~ /^[\.\?]$/ )
                      );
                }

                elsif ( $type_iend_1 eq '{' ) {

                    # YVES
                    # honor breaks at opening brace
                    # Added to prevent recombining something like this:
                    #  } || eval { package main;
                    next if $forced_breakpoint_to_go[$iend_1];
                }

                # do not recombine lines with ending &&, ||,
                elsif ( $is_amp_amp{$type_iend_1} ) {
                    next unless $want_break_before{$type_iend_1};
                }

                # Identify and recombine a broken ?/: chain
                elsif ( $type_iend_1 eq '?' ) {

                    # Do not recombine different levels
                    next
                      if ( $levels_to_go[$ibeg_1] ne $levels_to_go[$ibeg_2] );

                    # do not recombine unless next line ends in :
                    next unless $type_iend_2 eq ':';
                }

                # for lines ending in a comma...
                elsif ( $type_iend_1 eq ',' ) {

                    # Do not recombine at comma which is following the
                    # input bias.
                    # TODO: might be best to make a special flag
                    next if ( $old_breakpoint_to_go[$iend_1] );

                 # an isolated '},' may join with an identifier + ';'
                 # this is useful for the class of a 'bless' statement (bless.t)
                    if (   $type_ibeg_1 eq '}'
                        && $type_ibeg_2 eq 'i' )
                    {
                        next
                          unless ( ( $ibeg_1 == ( $iend_1 - 1 ) )
                            && ( $iend_2 == ( $ibeg_2 + 1 ) )
                            && $this_line_is_semicolon_terminated );

                        # override breakpoint
                        $forced_breakpoint_to_go[$iend_1] = 0;
                    }

                    # but otherwise ..
                    else {

                        # do not recombine after a comma unless this will leave
                        # just 1 more line
                        next unless ( $n + 1 >= $nmax );

                    # do not recombine if there is a change in indentation depth
                        next
                          if (
                            $levels_to_go[$iend_1] != $levels_to_go[$iend_2] );

                        # do not recombine a "complex expression" after a
                        # comma.  "complex" means no parens.
                        my $saw_paren;
                        foreach my $ii ( $ibeg_2 .. $iend_2 ) {
                            if ( $tokens_to_go[$ii] eq '(' ) {
                                $saw_paren = 1;
                                last;
                            }
                        }
                        next if $saw_paren;
                    }
                }

                # opening paren..
                elsif ( $type_iend_1 eq '(' ) {

                    # No longer doing this
                }

                elsif ( $type_iend_1 eq ')' ) {

                    # No longer doing this
                }

                # keep a terminal for-semicolon
                elsif ( $type_iend_1 eq 'f' ) {
                    next;
                }

                # if '=' at end of line ...
                elsif ( $is_assignment{$type_iend_1} ) {

                    # keep break after = if it was in input stream
                    # this helps prevent 'blinkers'
                    next if $old_breakpoint_to_go[$iend_1]

                      # don't strand an isolated '='
                      && $iend_1 != $ibeg_1;

                    my $is_short_quote =
                      (      $type_ibeg_2 eq 'Q'
                          && $ibeg_2 == $iend_2
                          && token_sequence_length( $ibeg_2, $ibeg_2 ) <
                          $rOpts_short_concatenation_item_length );
                    my $is_ternary =
                      ( $type_ibeg_1 eq '?'
                          && ( $ibeg_3 >= 0 && $types_to_go[$ibeg_3] eq ':' ) );

                    # always join an isolated '=', a short quote, or if this
                    # will put ?/: at start of adjacent lines
                    if (   $ibeg_1 != $iend_1
                        && !$is_short_quote
                        && !$is_ternary )
                    {
                        next
                          unless (
                            (

                                # unless we can reduce this to two lines
                                $nmax < $n + 2

                             # or three lines, the last with a leading semicolon
                                || (   $nmax == $n + 2
                                    && $types_to_go[$ibeg_nmax] eq ';' )

                                # or the next line ends with a here doc
                                || $type_iend_2 eq 'h'

                               # or the next line ends in an open paren or brace
                               # and the break hasn't been forced [dima.t]
                                || (  !$forced_breakpoint_to_go[$iend_1]
                                    && $type_iend_2 eq '{' )
                            )

                            # do not recombine if the two lines might align well
                            # this is a very approximate test for this
                            && (

                              # RT#127633 - the leading tokens are not operators
                                ( $type_ibeg_2 ne $tokens_to_go[$ibeg_2] )

                                # or they are different
                                || (   $ibeg_3 >= 0
                                    && $type_ibeg_2 ne $types_to_go[$ibeg_3] )
                            )
                          );

                        if (

                            # Recombine if we can make two lines
                            $nmax >= $n + 2

                            # -lp users often prefer this:
                            #  my $title = function($env, $env, $sysarea,
                            #                       "bubba Borrower Entry");
                            #  so we will recombine if -lp is used we have
                            #  ending comma
                            && (  !$rOpts_line_up_parentheses
                                || $type_iend_2 ne ',' )
                          )
                        {

                           # otherwise, scan the rhs line up to last token for
                           # complexity.  Note that we are not counting the last
                           # token in case it is an opening paren.
                            my $tv    = 0;
                            my $depth = $nesting_depth_to_go[$ibeg_2];
                            foreach my $i ( $ibeg_2 + 1 .. $iend_2 - 1 ) {
                                if ( $nesting_depth_to_go[$i] != $depth ) {
                                    $tv++;
                                    last if ( $tv > 1 );
                                }
                                $depth = $nesting_depth_to_go[$i];
                            }

                         # ok to recombine if no level changes before last token
                            if ( $tv > 0 ) {

                                # otherwise, do not recombine if more than two
                                # level changes.
                                next if ( $tv > 1 );

                              # check total complexity of the two adjacent lines
                              # that will occur if we do this join
                                my $istop =
                                  ( $n < $nmax )
                                  ? $ri_end->[ $n + 1 ]
                                  : $iend_2;
                                foreach my $i ( $iend_2 .. $istop ) {
                                    if ( $nesting_depth_to_go[$i] != $depth ) {
                                        $tv++;
                                        last if ( $tv > 2 );
                                    }
                                    $depth = $nesting_depth_to_go[$i];
                                }

                        # do not recombine if total is more than 2 level changes
                                next if ( $tv > 2 );
                            }
                        }
                    }

                    unless ( $tokens_to_go[$ibeg_2] =~ /^[\{\(\[]$/ ) {
                        $forced_breakpoint_to_go[$iend_1] = 0;
                    }
                }

                # for keywords..
                elsif ( $type_iend_1 eq 'k' ) {

                    # make major control keywords stand out
                    # (recombine.t)
                    next
                      if (

                        #/^(last|next|redo|return)$/
                        $is_last_next_redo_return{ $tokens_to_go[$iend_1] }

                        # but only if followed by multiple lines
                        && $n < $nmax
                      );

                    if ( $is_and_or{ $tokens_to_go[$iend_1] } ) {
                        next
                          unless $want_break_before{ $tokens_to_go[$iend_1] };
                    }
                }

                #----------------------------------------------------------
                # Recombine Section 3:
                # Examine token at $ibeg_2 (left end of second line of pair)
                #----------------------------------------------------------

                # join lines identified above as capable of
                # causing an outdented line with leading closing paren
                # Note that we are skipping the rest of this section
                # and the rest of the loop to do the join
                if ($skip_Section_3) {
                    $forced_breakpoint_to_go[$iend_1] = 0;
                    $n_best = $n;
                    last;
                }

                # handle lines with leading &&, ||
                elsif ( $is_amp_amp{$type_ibeg_2} ) {

                    $leading_amp_count++;

                    # ok to recombine if it follows a ? or :
                    # and is followed by an open paren..
                    my $ok =
                      (      $is_ternary{$type_ibeg_1}
                          && $tokens_to_go[$iend_2] eq '(' )

                    # or is followed by a ? or : at same depth
                    #
                    # We are looking for something like this. We can
                    # recombine the && line with the line above to make the
                    # structure more clear:
                    #  return
                    #    exists $G->{Attr}->{V}
                    #    && exists $G->{Attr}->{V}->{$u}
                    #    ? %{ $G->{Attr}->{V}->{$u} }
                    #    : ();
                    #
                    # We should probably leave something like this alone:
                    #  return
                    #       exists $G->{Attr}->{E}
                    #    && exists $G->{Attr}->{E}->{$u}
                    #    && exists $G->{Attr}->{E}->{$u}->{$v}
                    #    ? %{ $G->{Attr}->{E}->{$u}->{$v} }
                    #    : ();
                    # so that we either have all of the &&'s (or ||'s)
                    # on one line, as in the first example, or break at
                    # each one as in the second example.  However, it
                    # sometimes makes things worse to check for this because
                    # it prevents multiple recombinations.  So this is not done.
                      || ( $ibeg_3 >= 0
                        && $is_ternary{ $types_to_go[$ibeg_3] }
                        && $nesting_depth_to_go[$ibeg_3] ==
                        $nesting_depth_to_go[$ibeg_2] );

                    next if !$ok && $want_break_before{$type_ibeg_2};
                    $forced_breakpoint_to_go[$iend_1] = 0;

                    # tweak the bond strength to give this joint priority
                    # over ? and :
                    $bs_tweak = 0.25;
                }

                # Identify and recombine a broken ?/: chain
                elsif ( $type_ibeg_2 eq '?' ) {

                    # Do not recombine different levels
                    my $lev = $levels_to_go[$ibeg_2];
                    next if ( $lev ne $levels_to_go[$ibeg_1] );

                    # Do not recombine a '?' if either next line or
                    # previous line does not start with a ':'.  The reasons
                    # are that (1) no alignment of the ? will be possible
                    # and (2) the expression is somewhat complex, so the
                    # '?' is harder to see in the interior of the line.
                    my $follows_colon = $ibeg_1 >= 0 && $type_ibeg_1 eq ':';
                    my $precedes_colon =
                      $ibeg_3 >= 0 && $types_to_go[$ibeg_3] eq ':';
                    next unless ( $follows_colon || $precedes_colon );

                    # we will always combining a ? line following a : line
                    if ( !$follows_colon ) {

                        # ...otherwise recombine only if it looks like a chain.
                        # we will just look at a few nearby lines to see if
                        # this looks like a chain.
                        my $local_count = 0;
                        foreach my $ii ( $ibeg_0, $ibeg_1, $ibeg_3, $ibeg_4 ) {
                            $local_count++
                              if $ii >= 0
                              && $types_to_go[$ii] eq ':'
                              && $levels_to_go[$ii] == $lev;
                        }
                        next unless ( $local_count > 1 );
                    }
                    $forced_breakpoint_to_go[$iend_1] = 0;
                }

                # do not recombine lines with leading '.'
                elsif ( $type_ibeg_2 eq '.' ) {
                    my $i_next_nonblank = min( $inext_to_go[$ibeg_2], $iend_2 );
                    next
                      unless (

                   # ... unless there is just one and we can reduce
                   # this to two lines if we do.  For example, this
                   #
                   #
                   #  $bodyA .=
                   #    '($dummy, $pat) = &get_next_tex_cmd;' . '$args .= $pat;'
                   #
                   #  looks better than this:
                   #  $bodyA .= '($dummy, $pat) = &get_next_tex_cmd;'
                   #    . '$args .= $pat;'

                        (
                               $n == 2
                            && $n == $nmax
                            && $type_ibeg_1 ne $type_ibeg_2
                        )

                        #  ... or this would strand a short quote , like this
                        #                . "some long quote"
                        #                . "\n";

                        || (   $types_to_go[$i_next_nonblank] eq 'Q'
                            && $i_next_nonblank >= $iend_2 - 1
                            && $token_lengths_to_go[$i_next_nonblank] <
                            $rOpts_short_concatenation_item_length )
                      );
                }

                # handle leading keyword..
                elsif ( $type_ibeg_2 eq 'k' ) {

                    # handle leading "or"
                    if ( $tokens_to_go[$ibeg_2] eq 'or' ) {
                        next
                          unless (
                            $this_line_is_semicolon_terminated
                            && (
                                $type_ibeg_1 eq '}'
                                || (

                                    # following 'if' or 'unless' or 'or'
                                    $type_ibeg_1 eq 'k'
                                    && $is_if_unless{ $tokens_to_go[$ibeg_1] }

                                    # important: only combine a very simple or
                                    # statement because the step below may have
                                    # combined a trailing 'and' with this or,
                                    # and we do not want to then combine
                                    # everything together
                                    && ( $iend_2 - $ibeg_2 <= 7 )
                                )
                            )
                          );

                        #X: RT #81854
                        $forced_breakpoint_to_go[$iend_1] = 0
                          unless $old_breakpoint_to_go[$iend_1];
                    }

                    # handle leading 'and' and 'xor'
                    elsif ($tokens_to_go[$ibeg_2] eq 'and'
                        || $tokens_to_go[$ibeg_2] eq 'xor' )
                    {

                        # Decide if we will combine a single terminal 'and'
                        # after an 'if' or 'unless'.

                        #     This looks best with the 'and' on the same
                        #     line as the 'if':
                        #
                        #         $a = 1
                        #           if $seconds and $nu < 2;
                        #
                        #     But this looks better as shown:
                        #
                        #         $a = 1
                        #           if !$this->{Parents}{$_}
                        #           or $this->{Parents}{$_} eq $_;
                        #
                        next
                          unless (
                            $this_line_is_semicolon_terminated
                            && (

                                # following 'if' or 'unless' or 'or'
                                $type_ibeg_1 eq 'k'
                                && (   $is_if_unless{ $tokens_to_go[$ibeg_1] }
                                    || $tokens_to_go[$ibeg_1] eq 'or' )
                            )
                          );
                    }

                    # handle leading "if" and "unless"
                    elsif ( $is_if_unless{ $tokens_to_go[$ibeg_2] } ) {

                      # FIXME: This is still experimental..may not be too useful
                        next
                          unless (
                            $this_line_is_semicolon_terminated

                            #  previous line begins with 'and' or 'or'
                            && $type_ibeg_1 eq 'k'
                            && $is_and_or{ $tokens_to_go[$ibeg_1] }

                          );
                    }

                    # handle all other leading keywords
                    else {

                        # keywords look best at start of lines,
                        # but combine things like "1 while"
                        unless ( $is_assignment{$type_iend_1} ) {
                            next
                              if ( ( $type_iend_1 ne 'k' )
                                && ( $tokens_to_go[$ibeg_2] ne 'while' ) );
                        }
                    }
                }

                # similar treatment of && and || as above for 'and' and 'or':
                # NOTE: This block of code is currently bypassed because
                # of a previous block but is retained for possible future use.
                elsif ( $is_amp_amp{$type_ibeg_2} ) {

                    # maybe looking at something like:
                    # unless $TEXTONLY || $item =~ m%</?(hr>|p>|a|img)%i;

                    next
                      unless (
                        $this_line_is_semicolon_terminated

                        # previous line begins with an 'if' or 'unless' keyword
                        && $type_ibeg_1 eq 'k'
                        && $is_if_unless{ $tokens_to_go[$ibeg_1] }

                      );
                }

                # handle line with leading = or similar
                elsif ( $is_assignment{$type_ibeg_2} ) {
                    next unless ( $n == 1 || $n == $nmax );
                    next if $old_breakpoint_to_go[$iend_1];
                    next
                      unless (

                        # unless we can reduce this to two lines
                        $nmax == 2

                        # or three lines, the last with a leading semicolon
                        || ( $nmax == 3 && $types_to_go[$ibeg_nmax] eq ';' )

                        # or the next line ends with a here doc
                        || $type_iend_2 eq 'h'

                        # or this is a short line ending in ;
                        || ( $n == $nmax && $this_line_is_semicolon_terminated )
                      );
                    $forced_breakpoint_to_go[$iend_1] = 0;
                }

                #----------------------------------------------------------
                # Recombine Section 4:
                # Combine the lines if we arrive here and it is possible
                #----------------------------------------------------------

                # honor hard breakpoints
                next if ( $forced_breakpoint_to_go[$iend_1] > 0 );

                my $bs = $bond_strength_to_go[$iend_1] + $bs_tweak;

                # Require a few extra spaces before recombining lines if we are
                # at an old breakpoint unless this is a simple list or terminal
                # line.  The goal is to avoid oscillating between two
                # quasi-stable end states.  For example this snippet caused
                # problems:
##    my $this =
##    bless {
##        TText => "[" . ( join ',', map { "\"$_\"" } split "\n", $_ ) . "]"
##      },
##      $type;
                next
                  if ( $old_breakpoint_to_go[$iend_1]
                    && !$this_line_is_semicolon_terminated
                    && $n < $nmax
                    && $excess + 4 > 0
                    && $type_iend_2 ne ',' );

                # do not recombine if we would skip in indentation levels
                if ( $n < $nmax ) {
                    my $if_next = $ri_beg->[ $n + 1 ];
                    next
                      if (
                           $levels_to_go[$ibeg_1] < $levels_to_go[$ibeg_2]
                        && $levels_to_go[$ibeg_2] < $levels_to_go[$if_next]

                        # but an isolated 'if (' is undesirable
                        && !(
                               $n == 1
                            && $iend_1 - $ibeg_1 <= 2
                            && $type_ibeg_1 eq 'k'
                            && $tokens_to_go[$ibeg_1] eq 'if'
                            && $tokens_to_go[$iend_1] ne '('
                        )
                      );
                }

                # honor no-break's
                next if ( $bs >= NO_BREAK - 1 );

                # remember the pair with the greatest bond strength
                if ( !$n_best ) {
                    $n_best  = $n;
                    $bs_best = $bs;
                }
                else {

                    if ( $bs > $bs_best ) {
                        $n_best  = $n;
                        $bs_best = $bs;
                    }
                }
            }

            # recombine the pair with the greatest bond strength
            if ($n_best) {
                splice @{$ri_beg}, $n_best, 1;
                splice @{$ri_end}, $n_best - 1, 1;
                splice @joint, $n_best, 1;

                # keep going if we are still making progress
                $more_to_do++;
            }
        }
        return ( $ri_beg, $ri_end );
    }
} ## end closure recombine_breakpoints

sub insert_final_ternary_breaks {

    my ( $self, $ri_left, $ri_right ) = @_;

    # Called once per batch to look for and do any final line breaks for
    # long ternary chains

    my $nmax = @{$ri_right} - 1;

    # scan the left and right end tokens of all lines
    my $count         = 0;
    my $i_first_colon = -1;
    for my $n ( 0 .. $nmax ) {
        my $il    = $ri_left->[$n];
        my $ir    = $ri_right->[$n];
        my $typel = $types_to_go[$il];
        my $typer = $types_to_go[$ir];
        return if ( $typel eq '?' );
        return if ( $typer eq '?' );
        if    ( $typel eq ':' ) { $i_first_colon = $il; last; }
        elsif ( $typer eq ':' ) { $i_first_colon = $ir; last; }
    }

    # For long ternary chains,
    # if the first : we see has its ? is in the interior
    # of a preceding line, then see if there are any good
    # breakpoints before the ?.
    if ( $i_first_colon > 0 ) {
        my $i_question = $mate_index_to_go[$i_first_colon];
        if ( $i_question > 0 ) {
            my @insert_list;
            for ( my $ii = $i_question - 1 ; $ii >= 0 ; $ii -= 1 ) {
                my $token = $tokens_to_go[$ii];
                my $type  = $types_to_go[$ii];

                # For now, a good break is either a comma or,
                # in a long chain, a 'return'.
                # Patch for RT #126633: added the $nmax>1 check to avoid
                # breaking after a return for a simple ternary.  For longer
                # chains the break after return allows vertical alignment, so
                # it is still done.  So perltidy -wba='?' will not break
                # immediately after the return in the following statement:
                # sub x {
                #    return 0 ? 'aaaaaaaaaaaaaaaaaaaaa' :
                #      'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb';
                # }
                if (
                    (
                           $type eq ','
                        || $type eq 'k' && ( $nmax > 1 && $token eq 'return' )
                    )
                    && $self->in_same_container_i( $ii, $i_question )
                  )
                {
                    push @insert_list, $ii;
                    last;
                }
            }

            # insert any new break points
            if (@insert_list) {
                $self->insert_additional_breaks( \@insert_list, $ri_left,
                    $ri_right );
            }
        }
    }
    return;
}

sub insert_breaks_before_list_opening_containers {

    my ( $self, $ri_left, $ri_right ) = @_;

    # This routine is called once per batch to implement the parameters
    # --break-before-hash-brace, etc.

    # Nothing to do if none of these parameters has been set
    return unless %break_before_container_types;

    my $nmax = @{$ri_right} - 1;
    return unless ( $nmax >= 0 );

    my $rLL                   = $self->[_rLL_];
    my $ris_broken_container  = $self->[_ris_broken_container_];
    my $rhas_broken_container = $self->[_rhas_broken_container_];
    my $rparent_of_seqno      = $self->[_rparent_of_seqno_];

    # scan the ends of all lines
    my @insert_list;
    for my $n ( 0 .. $nmax ) {
        my $il = $ri_left->[$n];
        my $ir = $ri_right->[$n];
        next unless ( $ir > $il );
        my $Kl       = $K_to_go[$il];
        my $Kr       = $K_to_go[$ir];
        my $Kend     = $Kr;
        my $iend     = $ir;
        my $type_end = $rLL->[$Kr]->[_TYPE_];

        # Backup before any side comment
        if ( $type_end eq '#' ) {
            $Kend = $self->K_previous_nonblank($Kr);
            next unless defined($Kend);
            $type_end = $rLL->[$Kend]->[_TYPE_];
            $iend     = $ir + ( $Kend - $Kr );
        }

        next unless ( $Kl < $Kend - 1 );

        my $seqno = $rLL->[$Kend]->[_TYPE_SEQUENCE_];
        next unless ( defined($seqno) );

        # Only for types of container tokens with a non-default break option
        my $token_end    = $rLL->[$Kend]->[_TOKEN_];
        my $break_option = $break_before_container_types{$token_end};
        next unless ($break_option);

        # Require previous nonblank to be certain types (= and =>)
        # Note similar coding in sub adjust_container_indentation
        my $Kprev     = $Kend - 1;
        my $prev_type = $rLL->[$Kprev]->[_TYPE_];
        if ( $prev_type eq 'b' ) {
            $Kprev--;
            next if ( $Kprev <= $Kl );
            $prev_type = $rLL->[$Kprev]->[_TYPE_];
        }
        next unless ( $is_equal_or_fat_comma{$prev_type} );

        # This must be a list (this will exclude all code blocks)
        next unless $self->is_list_by_seqno($seqno);

        # Never break a weld
        next if ( $self->weld_len_left( $seqno, $token_end ) );

        # Final decision is based on selected option:

        # Option 1 = stable, try to follow input
        my $ok_to_break;
        if ( $break_option == 1 ) {
            if ( $ir - 2 > $il ) {
                $ok_to_break = $old_breakpoint_to_go[ $ir - 2 ];
            }
        }

        # Option 2 = only if complex list, meaning:
        #  - this list contains a broken container, or
        #  - this list is contained in a broken list
        elsif ( $break_option == 2 ) {
            $ok_to_break = $rhas_broken_container->{$seqno};
            if ( !$ok_to_break ) {
                my $parent = $rparent_of_seqno->{$seqno};
                $ok_to_break = $self->is_list_by_seqno($parent);
            }
        }

        # Option 3 = always break
        elsif ( $break_option == 3 ) {
            $ok_to_break = 1;
        }

        # Shouldn't happen! Bad flag, but make behavior same as 3
        else {
            $ok_to_break = 1;
        }

        next unless ($ok_to_break);

        # This meets the criteria, so install a break before the opening token.
        my $Kbreak = $self->K_previous_nonblank($Kend);
        my $ibreak = $Kbreak - $Kl + $il;
        next if ( $ibreak < $il );
        next if ( $nobreak_to_go[$ibreak] );
        push @insert_list, $ibreak;

    }

    # insert any new break points
    if (@insert_list) {
        $self->insert_additional_breaks( \@insert_list, $ri_left, $ri_right );
    }
    return;
}

sub note_added_semicolon {
    my ( $self, $line_number ) = @_;
    $self->[_last_added_semicolon_at_] = $line_number;
    if ( $self->[_added_semicolon_count_] == 0 ) {
        $self->[_first_added_semicolon_at_] = $line_number;
    }
    $self->[_added_semicolon_count_]++;
    write_logfile_entry("Added ';' here\n");
    return;
}

sub note_deleted_semicolon {
    my ( $self, $line_number ) = @_;
    $self->[_last_deleted_semicolon_at_] = $line_number;
    if ( $self->[_deleted_semicolon_count_] == 0 ) {
        $self->[_first_deleted_semicolon_at_] = $line_number;
    }
    $self->[_deleted_semicolon_count_]++;
    write_logfile_entry("Deleted unnecessary ';' at line $line_number\n");
    return;
}

sub note_embedded_tab {
    my ( $self, $line_number ) = @_;
    $self->[_embedded_tab_count_]++;
    $self->[_last_embedded_tab_at_] = $line_number;
    if ( !$self->[_first_embedded_tab_at_] ) {
        $self->[_first_embedded_tab_at_] = $line_number;
    }

    if ( $self->[_embedded_tab_count_] <= MAX_NAG_MESSAGES ) {
        write_logfile_entry("Embedded tabs in quote or pattern\n");
    }
    return;
}

sub correct_lp_indentation {

    # When the -lp option is used, we need to make a last pass through
    # each line to correct the indentation positions in case they differ
    # from the predictions.  This is necessary because perltidy uses a
    # predictor/corrector method for aligning with opening parens.  The
    # predictor is usually good, but sometimes stumbles.  The corrector
    # tries to patch things up once the actual opening paren locations
    # are known.
    my ( $self, $ri_first, $ri_last ) = @_;
    my $do_not_pad = 0;

    #  Note on flag '$do_not_pad':
    #  We want to avoid a situation like this, where the aligner inserts
    #  whitespace before the '=' to align it with a previous '=', because
    #  otherwise the parens might become mis-aligned in a situation like
    #  this, where the '=' has become aligned with the previous line,
    #  pushing the opening '(' forward beyond where we want it.
    #
    #  $mkFloor::currentRoom = '';
    #  $mkFloor::c_entry     = $c->Entry(
    #                                 -width        => '10',
    #                                 -relief       => 'sunken',
    #                                 ...
    #                                 );
    #
    #  We leave it to the aligner to decide how to do this.

    # first remove continuation indentation if appropriate
    my $max_line = @{$ri_first} - 1;

    # looking at each line of this batch..
    my ( $ibeg, $iend );
    foreach my $line ( 0 .. $max_line ) {
        $ibeg = $ri_first->[$line];
        $iend = $ri_last->[$line];

        # looking at each token in this output line..
        foreach my $i ( $ibeg .. $iend ) {

            # How many space characters to place before this token
            # for special alignment.  Actual padding is done in the
            # continue block.

            # looking for next unvisited indentation item
            my $indentation = $leading_spaces_to_go[$i];
            if ( !$indentation->get_marked() ) {
                $indentation->set_marked(1);

                # looking for indentation item for which we are aligning
                # with parens, braces, and brackets
                next unless ( $indentation->get_align_paren() );

                # skip closed container on this line
                if ( $i > $ibeg ) {
                    my $im = max( $ibeg, $iprev_to_go[$i] );
                    if (   $type_sequence_to_go[$im]
                        && $mate_index_to_go[$im] <= $iend )
                    {
                        next;
                    }
                }

                if ( $line == 1 && $i == $ibeg ) {
                    $do_not_pad = 1;
                }

                # Ok, let's see what the error is and try to fix it
                my $actual_pos;
                my $predicted_pos = $indentation->get_spaces();
                if ( $i > $ibeg ) {

                    # token is mid-line - use length to previous token
                    $actual_pos = total_line_length( $ibeg, $i - 1 );

                    # for mid-line token, we must check to see if all
                    # additional lines have continuation indentation,
                    # and remove it if so.  Otherwise, we do not get
                    # good alignment.
                    my $closing_index = $indentation->get_closed();
                    if ( $closing_index > $iend ) {
                        my $ibeg_next = $ri_first->[ $line + 1 ];
                        if ( $ci_levels_to_go[$ibeg_next] > 0 ) {
                            $self->undo_lp_ci( $line, $i, $closing_index,
                                $ri_first, $ri_last );
                        }
                    }
                }
                elsif ( $line > 0 ) {

                    # handle case where token starts a new line;
                    # use length of previous line
                    my $ibegm = $ri_first->[ $line - 1 ];
                    my $iendm = $ri_last->[ $line - 1 ];
                    $actual_pos = total_line_length( $ibegm, $iendm );

                    # follow -pt style
                    ++$actual_pos
                      if ( $types_to_go[ $iendm + 1 ] eq 'b' );
                }
                else {

                    # token is first character of first line of batch
                    $actual_pos = $predicted_pos;
                }

                my $move_right = $actual_pos - $predicted_pos;

                # done if no error to correct (gnu2.t)
                if ( $move_right == 0 ) {
                    $indentation->set_recoverable_spaces($move_right);
                    next;
                }

                # if we have not seen closure for this indentation in
                # this batch, we can only pass on a request to the
                # vertical aligner
                my $closing_index = $indentation->get_closed();

                if ( $closing_index < 0 ) {
                    $indentation->set_recoverable_spaces($move_right);
                    next;
                }

                # If necessary, look ahead to see if there is really any
                # leading whitespace dependent on this whitespace, and
                # also find the longest line using this whitespace.
                # Since it is always safe to move left if there are no
                # dependents, we only need to do this if we may have
                # dependent nodes or need to move right.

                my $right_margin = 0;
                my $have_child   = $indentation->get_have_child();

                my %saw_indentation;
                my $line_count = 1;
                $saw_indentation{$indentation} = $indentation;

                if ( $have_child || $move_right > 0 ) {
                    $have_child = 0;
                    my $max_length = 0;
                    if ( $i == $ibeg ) {
                        $max_length = total_line_length( $ibeg, $iend );
                    }

                    # look ahead at the rest of the lines of this batch..
                    foreach my $line_t ( $line + 1 .. $max_line ) {
                        my $ibeg_t = $ri_first->[$line_t];
                        my $iend_t = $ri_last->[$line_t];
                        last if ( $closing_index <= $ibeg_t );

                        # remember all different indentation objects
                        my $indentation_t = $leading_spaces_to_go[$ibeg_t];
                        $saw_indentation{$indentation_t} = $indentation_t;
                        $line_count++;

                        # remember longest line in the group
                        my $length_t = total_line_length( $ibeg_t, $iend_t );
                        if ( $length_t > $max_length ) {
                            $max_length = $length_t;
                        }
                    }
                    $right_margin =
                      $maximum_line_length[ $levels_to_go[$ibeg] ] -
                      $max_length;
                    if ( $right_margin < 0 ) { $right_margin = 0 }
                }

                my $first_line_comma_count =
                  grep { $_ eq ',' } @types_to_go[ $ibeg .. $iend ];
                my $comma_count = $indentation->get_comma_count();
                my $arrow_count = $indentation->get_arrow_count();

                # This is a simple approximate test for vertical alignment:
                # if we broke just after an opening paren, brace, bracket,
                # and there are 2 or more commas in the first line,
                # and there are no '=>'s,
                # then we are probably vertically aligned.  We could set
                # an exact flag in sub scan_list, but this is good
                # enough.
                my $indentation_count = keys %saw_indentation;
                my $is_vertically_aligned =
                  (      $i == $ibeg
                      && $first_line_comma_count > 1
                      && $indentation_count == 1
                      && ( $arrow_count == 0 || $arrow_count == $line_count ) );

                # Make the move if possible ..
                if (

                    # we can always move left
                    $move_right < 0

                    # but we should only move right if we are sure it will
                    # not spoil vertical alignment
                    || ( $comma_count == 0 )
                    || ( $comma_count > 0 && !$is_vertically_aligned )
                  )
                {
                    my $move =
                      ( $move_right <= $right_margin )
                      ? $move_right
                      : $right_margin;

                    foreach ( keys %saw_indentation ) {
                        $saw_indentation{$_}
                          ->permanently_decrease_available_spaces( -$move );
                    }
                }

                # Otherwise, record what we want and the vertical aligner
                # will try to recover it.
                else {
                    $indentation->set_recoverable_spaces($move_right);
                }
            }
        }
    }
    return $do_not_pad;
}

sub undo_lp_ci {

    # If there is a single, long parameter within parens, like this:
    #
    #  $self->command( "/msg "
    #        . $infoline->chan
    #        . " You said $1, but did you know that it's square was "
    #        . $1 * $1 . " ?" );
    #
    # we can remove the continuation indentation of the 2nd and higher lines
    # to achieve this effect, which is more pleasing:
    #
    #  $self->command("/msg "
    #                 . $infoline->chan
    #                 . " You said $1, but did you know that it's square was "
    #                 . $1 * $1 . " ?");

    my ( $self, $line_open, $i_start, $closing_index, $ri_first, $ri_last ) =
      @_;
    my $max_line = @{$ri_first} - 1;

    # must be multiple lines
    return unless $max_line > $line_open;

    my $lev_start     = $levels_to_go[$i_start];
    my $ci_start_plus = 1 + $ci_levels_to_go[$i_start];

    # see if all additional lines in this container have continuation
    # indentation
    my $n;
    my $line_1 = 1 + $line_open;
    for ( $n = $line_1 ; $n <= $max_line ; ++$n ) {
        my $ibeg = $ri_first->[$n];
        my $iend = $ri_last->[$n];
        if ( $ibeg eq $closing_index ) { $n--; last }
        return if ( $lev_start != $levels_to_go[$ibeg] );
        return if ( $ci_start_plus != $ci_levels_to_go[$ibeg] );
        last   if ( $closing_index <= $iend );
    }

    # we can reduce the indentation of all continuation lines
    my $continuation_line_count = $n - $line_open;
    @ci_levels_to_go[ @{$ri_first}[ $line_1 .. $n ] ] =
      (0) x ($continuation_line_count);
    @leading_spaces_to_go[ @{$ri_first}[ $line_1 .. $n ] ] =
      @reduced_spaces_to_go[ @{$ri_first}[ $line_1 .. $n ] ];
    return;
}

###############################################
# CODE SECTION 10: Code to break long statments
###############################################

sub set_continuation_breaks {

    # Called once per batch to set breaks in long lines.

    # Define an array of indexes for inserting newline characters to
    # keep the line lengths below the maximum desired length.  There is
    # an implied break after the last token, so it need not be included.

    # Method:
    # This routine is part of series of routines which adjust line
    # lengths.  It is only called if a statement is longer than the
    # maximum line length, or if a preliminary scanning located
    # desirable break points.   Sub scan_list has already looked at
    # these tokens and set breakpoints (in array
    # $forced_breakpoint_to_go[$i]) where it wants breaks (for example
    # after commas, after opening parens, and before closing parens).
    # This routine will honor these breakpoints and also add additional
    # breakpoints as necessary to keep the line length below the maximum
    # requested.  It bases its decision on where the 'bond strength' is
    # lowest.

    # Output: returns references to the arrays:
    #  @i_first
    #  @i_last
    # which contain the indexes $i of the first and last tokens on each
    # line.

    # In addition, the array:
    #   $forced_breakpoint_to_go[$i]
    # may be updated to be =1 for any index $i after which there must be
    # a break.  This signals later routines not to undo the breakpoint.

    my ( $self, $saw_good_break, $rcolon_list ) = @_;

    # @{$rcolon_list} is a list of all the ? and : tokens in the batch, in
    # order.

    use constant DEBUG_BREAKPOINTS => 0;

    my @i_first        = ();    # the first index to output
    my @i_last         = ();    # the last index to output
    my @i_colon_breaks = ();    # needed to decide if we have to break at ?'s
    if ( $types_to_go[0] eq ':' ) { push @i_colon_breaks, 0 }

    my $rOpts_fuzzy_line_length = $rOpts->{'fuzzy-line-length'};

    $self->set_bond_strengths();

    my $imin = 0;
    my $imax = $max_index_to_go;
    if ( $types_to_go[$imin] eq 'b' ) { $imin++ }
    if ( $types_to_go[$imax] eq 'b' ) { $imax-- }
    my $i_begin = $imin;        # index for starting next iteration

    my $leading_spaces          = leading_spaces_to_go($imin);
    my $line_count              = 0;
    my $last_break_strength     = NO_BREAK;
    my $i_last_break            = -1;
    my $max_bias                = 0.001;
    my $tiny_bias               = 0.0001;
    my $leading_alignment_token = "";
    my $leading_alignment_type  = "";

    # see if any ?/:'s are in order
    my $colons_in_order = 1;
    my $last_tok        = "";
    foreach ( @{$rcolon_list} ) {
        if ( $_ eq $last_tok ) { $colons_in_order = 0; last }
        $last_tok = $_;
    }

    # This is a sufficient but not necessary condition for colon chain
    my $is_colon_chain = ( $colons_in_order && @{$rcolon_list} > 2 );

    #-------------------------------------------------------
    # BEGINNING of main loop to set continuation breakpoints
    # Keep iterating until we reach the end
    #-------------------------------------------------------
    while ( $i_begin <= $imax ) {
        my $lowest_strength        = NO_BREAK;
        my $starting_sum           = $summed_lengths_to_go[$i_begin];
        my $i_lowest               = -1;
        my $i_test                 = -1;
        my $lowest_next_token      = '';
        my $lowest_next_type       = 'b';
        my $i_lowest_next_nonblank = -1;
        my $maximum_line_length =
          $maximum_line_length[ $levels_to_go[$i_begin] ];

        #-------------------------------------------------------
        # BEGINNING of inner loop to find the best next breakpoint
        #-------------------------------------------------------
        my $strength = NO_BREAK;
        for ( $i_test = $i_begin ; $i_test <= $imax ; $i_test++ ) {
            my $type                     = $types_to_go[$i_test];
            my $token                    = $tokens_to_go[$i_test];
            my $next_type                = $types_to_go[ $i_test + 1 ];
            my $next_token               = $tokens_to_go[ $i_test + 1 ];
            my $i_next_nonblank          = $inext_to_go[$i_test];
            my $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            my $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            my $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];

            # adjustments to the previous bond strength may have been made, and
            # we must keep the bond strength of a token and its following blank
            # the same;
            my $last_strength = $strength;
            $strength = $bond_strength_to_go[$i_test];
            if ( $type eq 'b' ) { $strength = $last_strength }

            # use old breaks as a tie-breaker.  For example to
            # prevent blinkers with -pbp in this code:

##@keywords{
##    qw/ARG OUTPUT PROTO CONSTRUCTOR RETURNS DESC PARAMS SEEALSO EXAMPLE/}
##    = ();

            # At the same time try to prevent a leading * in this code
            # with the default formatting:
            #
##                return
##                    factorial( $a + $b - 1 ) / factorial( $a - 1 ) / factorial( $b - 1 )
##                  * ( $x**( $a - 1 ) )
##                  * ( ( 1 - $x )**( $b - 1 ) );

            # reduce strength a bit to break ties at an old breakpoint ...
            if (
                $old_breakpoint_to_go[$i_test]

                # which is a 'good' breakpoint, meaning ...
                # we don't want to break before it
                && !$want_break_before{$type}

                # and either we want to break before the next token
                # or the next token is not short (i.e. not a '*', '/' etc.)
                && $i_next_nonblank <= $imax
                && (   $want_break_before{$next_nonblank_type}
                    || $token_lengths_to_go[$i_next_nonblank] > 2
                    || $next_nonblank_type eq ','
                    || $is_opening_type{$next_nonblank_type} )
              )
            {
                $strength -= $tiny_bias;
            }

            # otherwise increase strength a bit if this token would be at the
            # maximum line length.  This is necessary to avoid blinking
            # in the above example when the -iob flag is added.
            else {
                my $len =
                  $leading_spaces +
                  $summed_lengths_to_go[ $i_test + 1 ] -
                  $starting_sum;
                if ( $len >= $maximum_line_length ) {
                    $strength += $tiny_bias;
                }
            }

            my $must_break = 0;

            # Force an immediate break at certain operators
            # with lower level than the start of the line,
            # unless we've already seen a better break.
            #
            ##############################################
            # Note on an issue with a preceding ?
            ##############################################
            # We don't include a ? in the above list, but there may
            # be a break at a previous ? if the line is long.
            # Because of this we do not want to force a break if
            # there is a previous ? on this line.  For now the best way
            # to do this is to not break if we have seen a lower strength
            # point, which is probably a ?.
            #
            # Example of unwanted breaks we are avoiding at a '.' following a ?
            # from pod2html using perltidy -gnu:
            # )
            # ? "\n&lt;A NAME=\""
            # . $value
            # . "\"&gt;\n$text&lt;/A&gt;\n"
            # : "\n$type$pod2.html\#" . $value . "\"&gt;$text&lt;\/A&gt;\n";
            if (
                ( $strength <= $lowest_strength )
                && ( $nesting_depth_to_go[$i_begin] >
                    $nesting_depth_to_go[$i_next_nonblank] )
                && (
                    $next_nonblank_type =~ /^(\.|\&\&|\|\|)$/
                    || (   $next_nonblank_type eq 'k'
                        && $next_nonblank_token =~ /^(and|or)$/ )
                )
              )
            {
                $self->set_forced_breakpoint($i_next_nonblank);
            }

            if (

                # Try to put a break where requested by scan_list
                $forced_breakpoint_to_go[$i_test]

                # break between ) { in a continued line so that the '{' can
                # be outdented
                # See similar logic in scan_list which catches instances
                # where a line is just something like ') {'.  We have to
                # be careful because the corresponding block keyword might
                # not be on the first line, such as 'for' here:
                #
                # eval {
                #     for ("a") {
                #         for $x ( 1, 2 ) { local $_ = "b"; s/(.*)/+$1/ }
                #     }
                # };
                #
                || (
                       $line_count
                    && ( $token eq ')' )
                    && ( $next_nonblank_type eq '{' )
                    && ($next_nonblank_block_type)
                    && ( $next_nonblank_block_type ne $tokens_to_go[$i_begin] )

                    # RT #104427: Dont break before opening sub brace because
                    # sub block breaks handled at higher level, unless
                    # it looks like the preceding list is long and broken
                    && !(
                        $next_nonblank_block_type =~ /$ANYSUB_PATTERN/
                        && ( $nesting_depth_to_go[$i_begin] ==
                            $nesting_depth_to_go[$i_next_nonblank] )
                    )

                    && !$rOpts->{'opening-brace-always-on-right'}
                )

                # There is an implied forced break at a terminal opening brace
                || ( ( $type eq '{' ) && ( $i_test == $imax ) )
              )
            {

                # Forced breakpoints must sometimes be overridden, for example
                # because of a side comment causing a NO_BREAK.  It is easier
                # to catch this here than when they are set.
                if ( $strength < NO_BREAK - 1 ) {
                    $strength   = $lowest_strength - $tiny_bias;
                    $must_break = 1;
                }
            }

            # quit if a break here would put a good terminal token on
            # the next line and we already have a possible break
            if (
                   !$must_break
                && ( $next_nonblank_type eq ';' || $next_nonblank_type eq ',' )
                && (
                    (
                        $leading_spaces +
                        $summed_lengths_to_go[ $i_next_nonblank + 1 ] -
                        $starting_sum
                    ) > $maximum_line_length
                )
              )
            {
                last if ( $i_lowest >= 0 );
            }

            # Avoid a break which would strand a single punctuation
            # token.  For example, we do not want to strand a leading
            # '.' which is followed by a long quoted string.
            # But note that we do want to do this with -extrude (l=1)
            # so please test any changes to this code on -extrude.
            if (
                   !$must_break
                && ( $i_test == $i_begin )
                && ( $i_test < $imax )
                && ( $token eq $type )
                && (
                    (
                        $leading_spaces +
                        $summed_lengths_to_go[ $i_test + 1 ] -
                        $starting_sum
                    ) < $maximum_line_length
                )
              )
            {
                $i_test = min( $imax, $inext_to_go[$i_test] );
                redo;
            }

            if ( ( $strength <= $lowest_strength ) && ( $strength < NO_BREAK ) )
            {

                # break at previous best break if it would have produced
                # a leading alignment of certain common tokens, and it
                # is different from the latest candidate break
                last
                  if ($leading_alignment_type);

                # Force at least one breakpoint if old code had good
                # break It is only called if a breakpoint is required or
                # desired.  This will probably need some adjustments
                # over time.  A goal is to try to be sure that, if a new
                # side comment is introduced into formatted text, then
                # the same breakpoints will occur.  scbreak.t
                last
                  if (
                    $i_test == $imax            # we are at the end
                    && !get_forced_breakpoint_count()
                    && $saw_good_break          # old line had good break
                    && $type =~ /^[#;\{]$/      # and this line ends in
                                                # ';' or side comment
                    && $i_last_break < 0        # and we haven't made a break
                    && $i_lowest >= 0           # and we saw a possible break
                    && $i_lowest < $imax - 1    # (but not just before this ;)
                    && $strength - $lowest_strength < 0.5 * WEAK # and it's good
                  );

                # Do not skip past an important break point in a short final
                # segment.  For example, without this check we would miss the
                # break at the final / in the following code:
                #
                #  $depth_stop =
                #    ( $tau * $mass_pellet * $q_0 *
                #        ( 1. - exp( -$t_stop / $tau ) ) -
                #        4. * $pi * $factor * $k_ice *
                #        ( $t_melt - $t_ice ) *
                #        $r_pellet *
                #        $t_stop ) /
                #    ( $rho_ice * $Qs * $pi * $r_pellet**2 );
                #
                if (
                       $line_count > 2
                    && $i_lowest >= 0    # and we saw a possible break
                    && $i_lowest < $i_test
                    && $i_test > $imax - 2
                    && $nesting_depth_to_go[$i_begin] >
                    $nesting_depth_to_go[$i_lowest]
                    && $lowest_strength < $last_break_strength - .5 * WEAK
                  )
                {
                    # Make this break for math operators for now
                    my $ir = $inext_to_go[$i_lowest];
                    my $il = $iprev_to_go[$ir];
                    last
                      if ( $types_to_go[$il] =~ /^[\/\*\+\-\%]$/
                        || $types_to_go[$ir] =~ /^[\/\*\+\-\%]$/ );
                }

                # Update the minimum bond strength location
                $lowest_strength        = $strength;
                $i_lowest               = $i_test;
                $lowest_next_token      = $next_nonblank_token;
                $lowest_next_type       = $next_nonblank_type;
                $i_lowest_next_nonblank = $i_next_nonblank;
                last if $must_break;

                # set flags to remember if a break here will produce a
                # leading alignment of certain common tokens
                if (   $line_count > 0
                    && $i_test < $imax
                    && ( $lowest_strength - $last_break_strength <= $max_bias )
                  )
                {
                    my $i_last_end = $iprev_to_go[$i_begin];
                    my $tok_beg    = $tokens_to_go[$i_begin];
                    my $type_beg   = $types_to_go[$i_begin];
                    if (

                        # check for leading alignment of certain tokens
                        (
                               $tok_beg eq $next_nonblank_token
                            && $is_chain_operator{$tok_beg}
                            && (   $type_beg eq 'k'
                                || $type_beg eq $tok_beg )
                            && $nesting_depth_to_go[$i_begin] >=
                            $nesting_depth_to_go[$i_next_nonblank]
                        )

                        || (   $tokens_to_go[$i_last_end] eq $token
                            && $is_chain_operator{$token}
                            && ( $type eq 'k' || $type eq $token )
                            && $nesting_depth_to_go[$i_last_end] >=
                            $nesting_depth_to_go[$i_test] )
                      )
                    {
                        $leading_alignment_token = $next_nonblank_token;
                        $leading_alignment_type  = $next_nonblank_type;
                    }
                }
            }

            my $too_long = ( $i_test >= $imax );
            if ( !$too_long ) {
                my $next_length =
                  $leading_spaces +
                  $summed_lengths_to_go[ $i_test + 2 ] -
                  $starting_sum;
                $too_long = $next_length > $maximum_line_length;

                # To prevent blinkers we will avoid leaving a token exactly at
                # the line length limit unless it is the last token or one of
                # several "good" types.
                #
                # The following code was a blinker with -pbp before this
                # modification:
##                    $last_nonblank_token eq '('
##                        && $is_indirect_object_taker{ $paren_type
##                            [$paren_depth] }
                # The issue causing the problem is that if the
                # term [$paren_depth] gets broken across a line then
                # the whitespace routine doesn't see both opening and closing
                # brackets and will format like '[ $paren_depth ]'.  This
                # leads to an oscillation in length depending if we break
                # before the closing bracket or not.
                if (  !$too_long
                    && $i_test + 1 < $imax
                    && $next_nonblank_type ne ','
                    && !$is_closing_type{$next_nonblank_type} )
                {
                    $too_long = $next_length >= $maximum_line_length;
                }
            }

            DEBUG_BREAKPOINTS && do {
                my $ltok     = $token;
                my $rtok     = $next_nonblank_token ? $next_nonblank_token : "";
                my $i_testp2 = $i_test + 2;
                if ( $i_testp2 > $max_index_to_go + 1 ) {
                    $i_testp2 = $max_index_to_go + 1;
                }
                if ( length($ltok) > 6 ) { $ltok = substr( $ltok, 0, 8 ) }
                if ( length($rtok) > 6 ) { $rtok = substr( $rtok, 0, 8 ) }
                print STDOUT
"BREAK: i=$i_test imax=$imax $types_to_go[$i_test] $next_nonblank_type sp=($leading_spaces) lnext= $summed_lengths_to_go[$i_testp2] 2long=$too_long str=$strength    $ltok $rtok\n";
            };

            # allow one extra terminal token after exceeding line length
            # if it would strand this token.
            if (   $rOpts_fuzzy_line_length
                && $too_long
                && $i_lowest == $i_test
                && $token_lengths_to_go[$i_test] > 1
                && ( $next_nonblank_type eq ';' || $next_nonblank_type eq ',' )
              )
            {
                $too_long = 0;
            }

            # we are done if...
            last
              if (

                # ... no more space and we have a break
                $too_long && $i_lowest >= 0

                # ... or no more tokens
                || $i_test == $imax
              );
        }

        #-------------------------------------------------------
        # END of inner loop to find the best next breakpoint
        # Now decide exactly where to put the breakpoint
        #-------------------------------------------------------

        # it's always ok to break at imax if no other break was found
        if ( $i_lowest < 0 ) { $i_lowest = $imax }

        # semi-final index calculation
        my $i_next_nonblank     = $inext_to_go[$i_lowest];
        my $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        my $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        #-------------------------------------------------------
        # ?/: rule 1 : if a break here will separate a '?' on this
        # line from its closing ':', then break at the '?' instead.
        #-------------------------------------------------------
        foreach my $i ( $i_begin + 1 .. $i_lowest - 1 ) {
            next unless ( $tokens_to_go[$i] eq '?' );

            # do not break if probable sequence of ?/: statements
            next if ($is_colon_chain);

            # do not break if statement is broken by side comment
            next
              if ( $tokens_to_go[$max_index_to_go] eq '#'
                && terminal_type_i( 0, $max_index_to_go ) !~ /^[\;\}]$/ );

            # no break needed if matching : is also on the line
            next
              if ( $mate_index_to_go[$i] >= 0
                && $mate_index_to_go[$i] <= $i_next_nonblank );

            $i_lowest = $i;
            if ( $want_break_before{'?'} ) { $i_lowest-- }
            last;
        }

        #-------------------------------------------------------
        # END of inner loop to find the best next breakpoint:
        # Break the line after the token with index i=$i_lowest
        #-------------------------------------------------------

        # final index calculation
        $i_next_nonblank     = $inext_to_go[$i_lowest];
        $next_nonblank_type  = $types_to_go[$i_next_nonblank];
        $next_nonblank_token = $tokens_to_go[$i_next_nonblank];

        DEBUG_BREAKPOINTS
          && print STDOUT
          "BREAK: best is i = $i_lowest strength = $lowest_strength\n";

        #-------------------------------------------------------
        # ?/: rule 2 : if we break at a '?', then break at its ':'
        #
        # Note: this rule is also in sub scan_list to handle a break
        # at the start and end of a line (in case breaks are dictated
        # by side comments).
        #-------------------------------------------------------
        if ( $next_nonblank_type eq '?' ) {
            $self->set_closing_breakpoint($i_next_nonblank);
        }
        elsif ( $types_to_go[$i_lowest] eq '?' ) {
            $self->set_closing_breakpoint($i_lowest);
        }

        #-------------------------------------------------------
        # ?/: rule 3 : if we break at a ':' then we save
        # its location for further work below.  We may need to go
        # back and break at its '?'.
        #-------------------------------------------------------
        if ( $next_nonblank_type eq ':' ) {
            push @i_colon_breaks, $i_next_nonblank;
        }
        elsif ( $types_to_go[$i_lowest] eq ':' ) {
            push @i_colon_breaks, $i_lowest;
        }

        # here we should set breaks for all '?'/':' pairs which are
        # separated by this line

        $line_count++;

        # save this line segment, after trimming blanks at the ends
        push( @i_first,
            ( $types_to_go[$i_begin] eq 'b' ) ? $i_begin + 1 : $i_begin );
        push( @i_last,
            ( $types_to_go[$i_lowest] eq 'b' ) ? $i_lowest - 1 : $i_lowest );

        # set a forced breakpoint at a container opening, if necessary, to
        # signal a break at a closing container.  Excepting '(' for now.
        if (
            (
                   $tokens_to_go[$i_lowest] eq '{'
                || $tokens_to_go[$i_lowest] eq '['
            )
            && !$forced_breakpoint_to_go[$i_lowest]
          )
        {
            $self->set_closing_breakpoint($i_lowest);
        }

        # get ready to go again
        $i_begin                 = $i_lowest + 1;
        $last_break_strength     = $lowest_strength;
        $i_last_break            = $i_lowest;
        $leading_alignment_token = "";
        $leading_alignment_type  = "";
        $lowest_next_token       = '';
        $lowest_next_type        = 'b';

        if ( ( $i_begin <= $imax ) && ( $types_to_go[$i_begin] eq 'b' ) ) {
            $i_begin++;
        }

        # update indentation size
        if ( $i_begin <= $imax ) {
            $leading_spaces = leading_spaces_to_go($i_begin);
        }
    }

    #-------------------------------------------------------
    # END of main loop to set continuation breakpoints
    # Now go back and make any necessary corrections
    #-------------------------------------------------------

    #-------------------------------------------------------
    # ?/: rule 4 -- if we broke at a ':', then break at
    # corresponding '?' unless this is a chain of ?: expressions
    #-------------------------------------------------------
    if (@i_colon_breaks) {

        # using a simple method for deciding if we are in a ?/: chain --
        # this is a chain if it has multiple ?/: pairs all in order;
        # otherwise not.
        # Note that if line starts in a ':' we count that above as a break
        my $is_chain = ( $colons_in_order && @i_colon_breaks > 1 );

        unless ($is_chain) {
            my @insert_list = ();
            foreach (@i_colon_breaks) {
                my $i_question = $mate_index_to_go[$_];
                if ( $i_question >= 0 ) {
                    if ( $want_break_before{'?'} ) {
                        $i_question = $iprev_to_go[$i_question];
                    }

                    if ( $i_question >= 0 ) {
                        push @insert_list, $i_question;
                    }
                }
                $self->insert_additional_breaks( \@insert_list, \@i_first,
                    \@i_last );
            }
        }
    }
    return ( \@i_first, \@i_last );
}

###########################################
# CODE SECTION 11: Code to break long lists
###########################################

{    ## begin closure scan_list

    # These routines and variables are involved in finding good
    # places to break long lists.

    my (
        $block_type,               $current_depth,
        $depth,                    $i,
        $i_last_nonblank_token,    $last_colon_sequence_number,
        $last_nonblank_token,      $last_nonblank_type,
        $last_nonblank_block_type, $last_old_breakpoint_count,
        $minimum_depth,            $next_nonblank_block_type,
        $next_nonblank_token,      $next_nonblank_type,
        $old_breakpoint_count,     $starting_breakpoint_count,
        $starting_depth,           $token,
        $type,                     $type_sequence,
    );

    my (
        @breakpoint_stack,              @breakpoint_undo_stack,
        @comma_index,                   @container_type,
        @identifier_count_stack,        @index_before_arrow,
        @interrupted_list,              @item_count_stack,
        @last_comma_index,              @last_dot_index,
        @last_nonblank_type,            @old_breakpoint_count_stack,
        @opening_structure_index_stack, @rfor_semicolon_list,
        @has_old_logical_breakpoints,   @rand_or_list,
        @i_equals,
    );

    # these arrays must retain values between calls
    my ( @has_broken_sublist, @dont_align, @want_comma_break );

    sub initialize_scan_list {
        @dont_align         = ();
        @has_broken_sublist = ();
        @want_comma_break   = ();
        return;
    }

    # routine to define essential variables when we go 'up' to
    # a new depth
    sub check_for_new_minimum_depth {
        my $depth = shift;
        if ( $depth < $minimum_depth ) {

            $minimum_depth = $depth;

            # these arrays need not retain values between calls
            $breakpoint_stack[$depth]              = $starting_breakpoint_count;
            $container_type[$depth]                = "";
            $identifier_count_stack[$depth]        = 0;
            $index_before_arrow[$depth]            = -1;
            $interrupted_list[$depth]              = 1;
            $item_count_stack[$depth]              = 0;
            $last_nonblank_type[$depth]            = "";
            $opening_structure_index_stack[$depth] = -1;

            $breakpoint_undo_stack[$depth]       = undef;
            $comma_index[$depth]                 = undef;
            $last_comma_index[$depth]            = undef;
            $last_dot_index[$depth]              = undef;
            $old_breakpoint_count_stack[$depth]  = undef;
            $has_old_logical_breakpoints[$depth] = 0;
            $rand_or_list[$depth]                = [];
            $rfor_semicolon_list[$depth]         = [];
            $i_equals[$depth]                    = -1;

            # these arrays must retain values between calls
            if ( !defined( $has_broken_sublist[$depth] ) ) {
                $dont_align[$depth]         = 0;
                $has_broken_sublist[$depth] = 0;
                $want_comma_break[$depth]   = 0;
            }
        }
        return;
    }

    # routine to decide which commas to break at within a container;
    # returns:
    #   $bp_count = number of comma breakpoints set
    #   $do_not_break_apart = a flag indicating if container need not
    #     be broken open
    sub set_comma_breakpoints {

        my ( $self, $dd ) = @_;
        my $bp_count           = 0;
        my $do_not_break_apart = 0;

        # anything to do?
        if ( $item_count_stack[$dd] ) {

            # handle commas not in containers...
            if ( $dont_align[$dd] ) {
                $self->do_uncontained_comma_breaks($dd);
            }

            # handle commas within containers...
            else {
                my $fbc = get_forced_breakpoint_count();

                # always open comma lists not preceded by keywords,
                # barewords, identifiers (that is, anything that doesn't
                # look like a function call)
                my $must_break_open = $last_nonblank_type[$dd] !~ /^[kwiU]$/;

                $self->set_comma_breakpoints_do(
                    {
                        depth            => $dd,
                        i_opening_paren  => $opening_structure_index_stack[$dd],
                        i_closing_paren  => $i,
                        item_count       => $item_count_stack[$dd],
                        identifier_count => $identifier_count_stack[$dd],
                        rcomma_index     => $comma_index[$dd],
                        next_nonblank_type  => $next_nonblank_type,
                        list_type           => $container_type[$dd],
                        interrupted         => $interrupted_list[$dd],
                        rdo_not_break_apart => \$do_not_break_apart,
                        must_break_open     => $must_break_open,
                        has_broken_sublist  => $has_broken_sublist[$dd],
                    }
                );
                $bp_count           = get_forced_breakpoint_count() - $fbc;
                $do_not_break_apart = 0 if $must_break_open;
            }
        }
        return ( $bp_count, $do_not_break_apart );
    }

    sub do_uncontained_comma_breaks {

        # Handle commas not in containers...
        # This is a catch-all routine for commas that we
        # don't know what to do with because the don't fall
        # within containers.  We will bias the bond strength
        # to break at commas which ended lines in the input
        # file.  This usually works better than just trying
        # to put as many items on a line as possible.  A
        # downside is that if the input file is garbage it
        # won't work very well. However, the user can always
        # prevent following the old breakpoints with the
        # -iob flag.
        my ( $self, $dd ) = @_;
        my $bias                  = -.01;
        my $old_comma_break_count = 0;
        foreach my $ii ( @{ $comma_index[$dd] } ) {
            if ( $old_breakpoint_to_go[$ii] ) {
                $old_comma_break_count++;
                $bond_strength_to_go[$ii] = $bias;

                # reduce bias magnitude to force breaks in order
                $bias *= 0.99;
            }
        }

        # Also put a break before the first comma if
        # (1) there was a break there in the input, and
        # (2) there was exactly one old break before the first comma break
        # (3) OLD: there are multiple old comma breaks
        # (3) NEW: there are one or more old comma breaks (see return example)
        #
        # For example, we will follow the user and break after
        # 'print' in this snippet:
        #    print
        #      "conformability (Not the same dimension)\n",
        #      "\t", $have, " is ", text_unit($hu), "\n",
        #      "\t", $want, " is ", text_unit($wu), "\n",
        #      ;
        #
        # Another example, just one comma, where we will break after
        # the return:
        #  return
        #    $x * cos($a) - $y * sin($a),
        #    $x * sin($a) + $y * cos($a);

        # Breaking a print statement:
        # print SAVEOUT
        #   ( $? & 127 ) ? " (SIG#" . ( $? & 127 ) . ")" : "",
        #   ( $? & 128 ) ? " -- core dumped" : "", "\n";
        #
        #  But we will not force a break after the opening paren here
        #  (causes a blinker):
        #        $heap->{stream}->set_output_filter(
        #            poe::filter::reference->new('myotherfreezer') ),
        #          ;
        #
        my $i_first_comma = $comma_index[$dd]->[0];
        if ( $old_breakpoint_to_go[$i_first_comma] ) {
            my $level_comma = $levels_to_go[$i_first_comma];
            my $ibreak      = -1;
            my $obp_count   = 0;
            for ( my $ii = $i_first_comma - 1 ; $ii >= 0 ; $ii -= 1 ) {
                if ( $old_breakpoint_to_go[$ii] ) {
                    $obp_count++;
                    last if ( $obp_count > 1 );
                    $ibreak = $ii
                      if ( $levels_to_go[$ii] == $level_comma );
                }
            }

            # Changed rule from multiple old commas to just one here:
            if ( $ibreak >= 0 && $obp_count == 1 && $old_comma_break_count > 0 )
            {
                # Do not to break before an opening token because
                # it can lead to "blinkers".
                my $ibreakm = $ibreak;
                $ibreakm-- if ( $types_to_go[$ibreakm] eq 'b' );
                if ( $ibreakm >= 0 && $types_to_go[$ibreakm] !~ /^[\(\{\[L]$/ )
                {
                    $self->set_forced_breakpoint($ibreak);
                }
            }
        }
        return;
    }

    my %is_logical_container;
    my %quick_filter;

    BEGIN {
        my @q = qw# if elsif unless while and or err not && | || ? : ! #;
        @is_logical_container{@q} = (1) x scalar(@q);

        # This filter will allow most tokens to skip past a section of code
        %quick_filter = %is_assignment;
        @q            = qw# => . ; < > ~ #;
        push @q, ',';
        @quick_filter{@q} = (1) x scalar(@q);
    }

    sub set_for_semicolon_breakpoints {
        my ( $self, $dd ) = @_;
        foreach ( @{ $rfor_semicolon_list[$dd] } ) {
            $self->set_forced_breakpoint($_);
        }
        return;
    }

    sub set_logical_breakpoints {
        my ( $self, $dd ) = @_;
        if (
               $item_count_stack[$dd] == 0
            && $is_logical_container{ $container_type[$dd] }

            || $has_old_logical_breakpoints[$dd]
          )
        {

            # Look for breaks in this order:
            # 0   1    2   3
            # or  and  ||  &&
            foreach my $i ( 0 .. 3 ) {
                if ( $rand_or_list[$dd][$i] ) {
                    foreach ( @{ $rand_or_list[$dd][$i] } ) {
                        $self->set_forced_breakpoint($_);
                    }

                    # break at any 'if' and 'unless' too
                    foreach ( @{ $rand_or_list[$dd][4] } ) {
                        $self->set_forced_breakpoint($_);
                    }
                    $rand_or_list[$dd] = [];
                    last;
                }
            }
        }
        return;
    }

    sub is_unbreakable_container {

        # never break a container of one of these types
        # because bad things can happen (map1.t)
        my $dd = shift;
        return $is_sort_map_grep{ $container_type[$dd] };
    }

    sub scan_list {

        my ( $self, $is_long_line ) = @_;

        # This routine is responsible for setting line breaks for all lists,
        # so that hierarchical structure can be displayed and so that list
        # items can be vertically aligned.  The output of this routine is
        # stored in the array @forced_breakpoint_to_go, which is used to set
        # final breakpoints.

        # It is called once per batch if the batch is a list.
        my $rOpts_break_at_old_attribute_breakpoints =
          $rOpts->{'break-at-old-attribute-breakpoints'};
        my $rOpts_break_at_old_keyword_breakpoints =
          $rOpts->{'break-at-old-keyword-breakpoints'};
        my $rOpts_break_at_old_logical_breakpoints =
          $rOpts->{'break-at-old-logical-breakpoints'};
        my $rOpts_break_at_old_method_breakpoints =
          $rOpts->{'break-at-old-method-breakpoints'};
        my $rOpts_break_at_old_ternary_breakpoints =
          $rOpts->{'break-at-old-ternary-breakpoints'};

        $starting_depth = $nesting_depth_to_go[0];

        $block_type                 = ' ';
        $current_depth              = $starting_depth;
        $i                          = -1;
        $last_colon_sequence_number = -1;
        $last_nonblank_token        = ';';
        $last_nonblank_type         = ';';
        $last_nonblank_block_type   = ' ';
        $last_old_breakpoint_count  = 0;
        $minimum_depth = $current_depth + 1;    # forces update in check below
        $old_breakpoint_count      = 0;
        $starting_breakpoint_count = get_forced_breakpoint_count();
        $token                     = ';';
        $type                      = ';';
        $type_sequence             = '';

        my $total_depth_variation = 0;
        my $i_old_assignment_break;
        my $depth_last = $starting_depth;

        check_for_new_minimum_depth($current_depth);

        my $want_previous_breakpoint = -1;

        my $saw_good_breakpoint;
        my $i_line_end   = -1;
        my $i_line_start = -1;

        # loop over all tokens in this batch
        while ( ++$i <= $max_index_to_go ) {
            if ( $type ne 'b' ) {
                $i_last_nonblank_token    = $i - 1;
                $last_nonblank_type       = $type;
                $last_nonblank_token      = $token;
                $last_nonblank_block_type = $block_type;
            } ## end if ( $type ne 'b' )
            $type          = $types_to_go[$i];
            $block_type    = $block_type_to_go[$i];
            $token         = $tokens_to_go[$i];
            $type_sequence = $type_sequence_to_go[$i];
            my $next_type       = $types_to_go[ $i + 1 ];
            my $next_token      = $tokens_to_go[ $i + 1 ];
            my $i_next_nonblank = ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );
            $next_nonblank_type       = $types_to_go[$i_next_nonblank];
            $next_nonblank_token      = $tokens_to_go[$i_next_nonblank];
            $next_nonblank_block_type = $block_type_to_go[$i_next_nonblank];

            # set break if flag was set
            if ( $want_previous_breakpoint >= 0 ) {
                $self->set_forced_breakpoint($want_previous_breakpoint);
                $want_previous_breakpoint = -1;
            }

            $last_old_breakpoint_count = $old_breakpoint_count;
            if ( $old_breakpoint_to_go[$i] ) {
                $i_line_end   = $i;
                $i_line_start = $i_next_nonblank;

                $old_breakpoint_count++;

                # Break before certain keywords if user broke there and
                # this is a 'safe' break point. The idea is to retain
                # any preferred breaks for sequential list operations,
                # like a schwartzian transform.
                if ($rOpts_break_at_old_keyword_breakpoints) {
                    if (
                           $next_nonblank_type eq 'k'
                        && $is_keyword_returning_list{$next_nonblank_token}
                        && (   $type =~ /^[=\)\]\}Riw]$/
                            || $type eq 'k'
                            && $is_keyword_returning_list{$token} )
                      )
                    {

                        # we actually have to set this break next time through
                        # the loop because if we are at a closing token (such
                        # as '}') which forms a one-line block, this break might
                        # get undone.
                        $want_previous_breakpoint = $i;
                    } ## end if ( $next_nonblank_type...)
                } ## end if ($rOpts_break_at_old_keyword_breakpoints)

                # Break before attributes if user broke there
                if ($rOpts_break_at_old_attribute_breakpoints) {
                    if ( $next_nonblank_type eq 'A' ) {
                        $want_previous_breakpoint = $i;
                    }
                }

                # remember an = break as possible good break point
                if ( $is_assignment{$type} ) {
                    $i_old_assignment_break = $i;
                }
                elsif ( $is_assignment{$next_nonblank_type} ) {
                    $i_old_assignment_break = $i_next_nonblank;
                }
            } ## end if ( $old_breakpoint_to_go...)

            next if ( $type eq 'b' );
            $depth = $nesting_depth_to_go[ $i + 1 ];

            $total_depth_variation += abs( $depth - $depth_last );
            $depth_last = $depth;

            # safety check - be sure we always break after a comment
            # Shouldn't happen .. an error here probably means that the
            # nobreak flag did not get turned off correctly during
            # formatting.
            if ( $type eq '#' ) {
                if ( $i != $max_index_to_go ) {
                    warning(
"Non-fatal program bug: backup logic required to break after a comment\n"
                    );
                    report_definite_bug();
                    $nobreak_to_go[$i] = 0;
                    $self->set_forced_breakpoint($i);
                } ## end if ( $i != $max_index_to_go)
            } ## end if ( $type eq '#' )

            # Force breakpoints at certain tokens in long lines.
            # Note that such breakpoints will be undone later if these tokens
            # are fully contained within parens on a line.
            if (

                # break before a keyword within a line
                $type eq 'k'
                && $i > 0

                # if one of these keywords:
                #   /^(if|unless|while|until|for)$/
                && $is_if_unless_while_until_for{$token}

                # but do not break at something like '1 while'
                && ( $last_nonblank_type ne 'n' || $i > 2 )

                # and let keywords follow a closing 'do' brace
                && $last_nonblank_block_type ne 'do'

                && (
                    $is_long_line

                    # or container is broken (by side-comment, etc)
                    || (   $next_nonblank_token eq '('
                        && $mate_index_to_go[$i_next_nonblank] < $i )
                )
              )
            {
                $self->set_forced_breakpoint( $i - 1 );
            } ## end if ( $type eq 'k' && $i...)

            # remember locations of -> if this is a pre-broken method chain
            if ( $type eq '->' ) {
                if ($rOpts_break_at_old_method_breakpoints) {

                    # Case 1: look for lines with leading pointers
                    if ( $i == $i_line_start ) {
                        $self->set_forced_breakpoint( $i - 1 );
                    }

                    # Case 2: look for cuddled pointer calls
                    else {

                        # look for old lines with leading ')->' or ') ->'
                        # and, when found, force a break before the
                        # opening paren and after the previous closing paren.
                        if (
                               $i_line_start >= 0
                            && $types_to_go[$i_line_start] eq '}'
                            && (   $i == $i_line_start + 1
                                || $i == $i_line_start + 2
                                && $types_to_go[ $i - 1 ] eq 'b' )
                          )
                        {
                            $self->set_forced_breakpoint( $i_line_start - 1 );
                            $self->set_forced_breakpoint(
                                $mate_index_to_go[$i_line_start] );
                        }
                    }
                }
            } ## end if ( $type eq '->' )

            # remember locations of '||'  and '&&' for possible breaks if we
            # decide this is a long logical expression.
            elsif ( $type eq '||' ) {
                push @{ $rand_or_list[$depth][2] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            } ## end elsif ( $type eq '||' )
            elsif ( $type eq '&&' ) {
                push @{ $rand_or_list[$depth][3] }, $i;
                ++$has_old_logical_breakpoints[$depth]
                  if ( ( $i == $i_line_start || $i == $i_line_end )
                    && $rOpts_break_at_old_logical_breakpoints );
            } ## end elsif ( $type eq '&&' )
            elsif ( $type eq 'f' ) {
                push @{ $rfor_semicolon_list[$depth] }, $i;
            }
            elsif ( $type eq 'k' ) {
                if ( $token eq 'and' ) {
                    push @{ $rand_or_list[$depth][1] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                } ## end if ( $token eq 'and' )

                # break immediately at 'or's which are probably not in a logical
                # block -- but we will break in logical breaks below so that
                # they do not add to the forced_breakpoint_count
                elsif ( $token eq 'or' ) {
                    push @{ $rand_or_list[$depth][0] }, $i;
                    ++$has_old_logical_breakpoints[$depth]
                      if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints );
                    if ( $is_logical_container{ $container_type[$depth] } ) {
                    }
                    else {
                        if ($is_long_line) { $self->set_forced_breakpoint($i) }
                        elsif ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_logical_breakpoints )
                        {
                            $saw_good_breakpoint = 1;
                        }
                    } ## end else [ if ( $is_logical_container...)]
                } ## end elsif ( $token eq 'or' )
                elsif ( $token eq 'if' || $token eq 'unless' ) {
                    push @{ $rand_or_list[$depth][4] }, $i;
                    if ( ( $i == $i_line_start || $i == $i_line_end )
                        && $rOpts_break_at_old_logical_breakpoints )
                    {
                        $self->set_forced_breakpoint($i);
                    }
                } ## end elsif ( $token eq 'if' ||...)
            } ## end elsif ( $type eq 'k' )
            elsif ( $is_assignment{$type} ) {
                $i_equals[$depth] = $i;
            }

            if ($type_sequence) {

                # handle any postponed closing breakpoints
                if ( $is_closing_sequence_token{$token} ) {
                    if ( $type eq ':' ) {
                        $last_colon_sequence_number = $type_sequence;

                        # retain break at a ':' line break
                        if ( ( $i == $i_line_start || $i == $i_line_end )
                            && $rOpts_break_at_old_ternary_breakpoints )
                        {

                            $self->set_forced_breakpoint($i);

                            # break at previous '='
                            if ( $i_equals[$depth] > 0 ) {
                                $self->set_forced_breakpoint(
                                    $i_equals[$depth] );
                                $i_equals[$depth] = -1;
                            }
                        } ## end if ( ( $i == $i_line_start...))
                    } ## end if ( $type eq ':' )
                    if ( has_postponed_breakpoint($type_sequence) ) {
                        my $inc = ( $type eq ':' ) ? 0 : 1;
                        $self->set_forced_breakpoint( $i - $inc );
                    }
                } ## end if ( $is_closing_sequence_token{$token} )

                # set breaks at ?/: if they will get separated (and are
                # not a ?/: chain), or if the '?' is at the end of the
                # line
                elsif ( $token eq '?' ) {
                    my $i_colon = $mate_index_to_go[$i];
                    if (
                        $i_colon <= 0  # the ':' is not in this batch
                        || $i == 0     # this '?' is the first token of the line
                        || $i ==
                        $max_index_to_go    # or this '?' is the last token
                      )
                    {

                        # don't break at a '?' if preceded by ':' on
                        # this line of previous ?/: pair on this line.
                        # This is an attempt to preserve a chain of ?/:
                        # expressions (elsif2.t).  And don't break if
                        # this has a side comment.
                        $self->set_forced_breakpoint($i)
                          unless (
                            $type_sequence == (
                                $last_colon_sequence_number +
                                  TYPE_SEQUENCE_INCREMENT
                            )
                            || $tokens_to_go[$max_index_to_go] eq '#'
                          );
                        $self->set_closing_breakpoint($i);
                    } ## end if ( $i_colon <= 0  ||...)
                } ## end elsif ( $token eq '?' )
            } ## end if ($type_sequence)

#print "LISTX sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth\n";

            #------------------------------------------------------------
            # Handle Increasing Depth..
            #
            # prepare for a new list when depth increases
            # token $i is a '(','{', or '['
            #------------------------------------------------------------
            if ( $depth > $current_depth ) {

                $breakpoint_stack[$depth] = get_forced_breakpoint_count();
                $breakpoint_undo_stack[$depth] =
                  get_forced_breakpoint_undo_count();
                $has_broken_sublist[$depth]            = 0;
                $identifier_count_stack[$depth]        = 0;
                $index_before_arrow[$depth]            = -1;
                $interrupted_list[$depth]              = 0;
                $item_count_stack[$depth]              = 0;
                $last_comma_index[$depth]              = undef;
                $last_dot_index[$depth]                = undef;
                $last_nonblank_type[$depth]            = $last_nonblank_type;
                $old_breakpoint_count_stack[$depth]    = $old_breakpoint_count;
                $opening_structure_index_stack[$depth] = $i;
                $rand_or_list[$depth]                  = [];
                $rfor_semicolon_list[$depth]           = [];
                $i_equals[$depth]                      = -1;
                $want_comma_break[$depth]              = 0;
                $container_type[$depth] =

                  #      k => && || ? : .
                  $is_container_label_type{$last_nonblank_type}
                  ? $last_nonblank_token
                  : "";
                $has_old_logical_breakpoints[$depth] = 0;

                # if line ends here then signal closing token to break
                if ( $next_nonblank_type eq 'b' || $next_nonblank_type eq '#' )
                {
                    $self->set_closing_breakpoint($i);
                }

                # Not all lists of values should be vertically aligned..
                $dont_align[$depth] =

                  # code BLOCKS are handled at a higher level
                  ( $block_type ne "" )

                  # certain paren lists
                  || ( $type eq '(' ) && (

                    # it does not usually look good to align a list of
                    # identifiers in a parameter list, as in:
                    #    my($var1, $var2, ...)
                    # (This test should probably be refined, for now I'm just
                    # testing for any keyword)
                    ( $last_nonblank_type eq 'k' )

                    # a trailing '(' usually indicates a non-list
                    || ( $next_nonblank_type eq '(' )
                  );

                # patch to outdent opening brace of long if/for/..
                # statements (like this one).  See similar coding in
                # set_continuation breaks.  We have also catch it here for
                # short line fragments which otherwise will not go through
                # set_continuation_breaks.
                if (
                    $block_type

                    # if we have the ')' but not its '(' in this batch..
                    && ( $last_nonblank_token eq ')' )
                    && $mate_index_to_go[$i_last_nonblank_token] < 0

                    # and user wants brace to left
                    && !$rOpts->{'opening-brace-always-on-right'}

                    && ( $type eq '{' )     # should be true
                    && ( $token eq '{' )    # should be true
                  )
                {
                    $self->set_forced_breakpoint( $i - 1 );
                } ## end if ( $block_type && ( ...))
            } ## end if ( $depth > $current_depth)

            #------------------------------------------------------------
            # Handle Decreasing Depth..
            #
            # finish off any old list when depth decreases
            # token $i is a ')','}', or ']'
            #------------------------------------------------------------
            elsif ( $depth < $current_depth ) {

                check_for_new_minimum_depth($depth);

                # force all outer logical containers to break after we see on
                # old breakpoint
                $has_old_logical_breakpoints[$depth] ||=
                  $has_old_logical_breakpoints[$current_depth];

                # Patch to break between ') {' if the paren list is broken.
                # There is similar logic in set_continuation_breaks for
                # non-broken lists.
                if (   $token eq ')'
                    && $next_nonblank_block_type
                    && $interrupted_list[$current_depth]
                    && $next_nonblank_type eq '{'
                    && !$rOpts->{'opening-brace-always-on-right'} )
                {
                    $self->set_forced_breakpoint($i);
                } ## end if ( $token eq ')' && ...

#print "LISTY sees: i=$i type=$type  tok=$token  block=$block_type depth=$depth next=$next_nonblank_type next_block=$next_nonblank_block_type inter=$interrupted_list[$current_depth]\n";

                # set breaks at commas if necessary
                my ( $bp_count, $do_not_break_apart ) =
                  $self->set_comma_breakpoints($current_depth);

                my $i_opening = $opening_structure_index_stack[$current_depth];
                my $saw_opening_structure = ( $i_opening >= 0 );

                # this term is long if we had to break at interior commas..
                my $is_long_term = $bp_count > 0;

                # If this is a short container with one or more comma arrows,
                # then we will mark it as a long term to open it if requested.
                # $rOpts_comma_arrow_breakpoints =
                #    0 - open only if comma precedes closing brace
                #    1 - stable: except for one line blocks
                #    2 - try to form 1 line blocks
                #    3 - ignore =>
                #    4 - always open up if vt=0
                #    5 - stable: even for one line blocks if vt=0
                if (  !$is_long_term
                    && $saw_opening_structure
                    && $is_opening_token{ $tokens_to_go[$i_opening] }
                    && $index_before_arrow[ $depth + 1 ] > 0
                    && !$opening_vertical_tightness{ $tokens_to_go[$i_opening] }
                  )
                {
                    $is_long_term = $rOpts_comma_arrow_breakpoints == 4
                      || ( $rOpts_comma_arrow_breakpoints == 0
                        && $last_nonblank_token eq ',' )
                      || ( $rOpts_comma_arrow_breakpoints == 5
                        && $old_breakpoint_to_go[$i_opening] );
                } ## end if ( !$is_long_term &&...)

                # mark term as long if the length between opening and closing
                # parens exceeds allowed line length
                if ( !$is_long_term && $saw_opening_structure ) {
                    my $i_opening_minus =
                      $self->find_token_starting_list($i_opening);

                    # Note: we have to allow for one extra space after a
                    # closing token so that we do not strand a comma or
                    # semicolon, hence the '>=' here (oneline.t)
                    $is_long_term =
                      $self->excess_line_length( $i_opening_minus, $i ) >= 0;
                } ## end if ( !$is_long_term &&...)

                # We've set breaks after all comma-arrows.  Now we have to
                # undo them if this can be a one-line block
                # (the only breakpoints set will be due to comma-arrows)
                if (

                    # user doesn't require breaking after all comma-arrows
                    ( $rOpts_comma_arrow_breakpoints != 0 )
                    && ( $rOpts_comma_arrow_breakpoints != 4 )

                    # and if the opening structure is in this batch
                    && $saw_opening_structure

                    # and either on the same old line
                    && (
                        $old_breakpoint_count_stack[$current_depth] ==
                        $last_old_breakpoint_count

                        # or user wants to form long blocks with arrows
                        || $rOpts_comma_arrow_breakpoints == 2
                    )

                  # and we made some breakpoints between the opening and closing
                    && ( $breakpoint_undo_stack[$current_depth] <
                        get_forced_breakpoint_undo_count() )

                    # and this block is short enough to fit on one line
                    # Note: use < because need 1 more space for possible comma
                    && !$is_long_term

                  )
                {
                    $self->undo_forced_breakpoint_stack(
                        $breakpoint_undo_stack[$current_depth] );
                } ## end if ( ( $rOpts_comma_arrow_breakpoints...))

                # now see if we have any comma breakpoints left
                my $has_comma_breakpoints =
                  ( $breakpoint_stack[$current_depth] !=
                      get_forced_breakpoint_count() );

                # update broken-sublist flag of the outer container
                $has_broken_sublist[$depth] =
                     $has_broken_sublist[$depth]
                  || $has_broken_sublist[$current_depth]
                  || $is_long_term
                  || $has_comma_breakpoints;

# Having come to the closing ')', '}', or ']', now we have to decide if we
# should 'open up' the structure by placing breaks at the opening and
# closing containers.  This is a tricky decision.  Here are some of the
# basic considerations:
#
# -If this is a BLOCK container, then any breakpoints will have already
# been set (and according to user preferences), so we need do nothing here.
#
# -If we have a comma-separated list for which we can align the list items,
# then we need to do so because otherwise the vertical aligner cannot
# currently do the alignment.
#
# -If this container does itself contain a container which has been broken
# open, then it should be broken open to properly show the structure.
#
# -If there is nothing to align, and no other reason to break apart,
# then do not do it.
#
# We will not break open the parens of a long but 'simple' logical expression.
# For example:
#
# This is an example of a simple logical expression and its formatting:
#
#     if ( $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4 )
#
# Most people would prefer this than the 'spacey' version:
#
#     if (
#         $bigwasteofspace1 && $bigwasteofspace2
#         || $bigwasteofspace3 && $bigwasteofspace4
#     )
#
# To illustrate the rules for breaking logical expressions, consider:
#
#             FULLY DENSE:
#             if ( $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc ))
#
# This is on the verge of being difficult to read.  The current default is to
# open it up like this:
#
#             DEFAULT:
#             if (
#                 $opt_excl
#                 and ( exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc )
#               )
#
# This is a compromise which tries to avoid being too dense and to spacey.
# A more spaced version would be:
#
#             SPACEY:
#             if (
#                 $opt_excl
#                 and (
#                     exists $ids_excl_uc{$id_uc}
#                     or grep $id_uc =~ /$_/, @ids_excl_uc
#                 )
#               )
#
# Some people might prefer the spacey version -- an option could be added.  The
# innermost expression contains a long block '( exists $ids_...  ')'.
#
# Here is how the logic goes: We will force a break at the 'or' that the
# innermost expression contains, but we will not break apart its opening and
# closing containers because (1) it contains no multi-line sub-containers itself,
# and (2) there is no alignment to be gained by breaking it open like this
#
#             and (
#                 exists $ids_excl_uc{$id_uc}
#                 or grep $id_uc =~ /$_/, @ids_excl_uc
#             )
#
# (although this looks perfectly ok and might be good for long expressions).  The
# outer 'if' container, though, contains a broken sub-container, so it will be
# broken open to avoid too much density.  Also, since it contains no 'or's, there
# will be a forced break at its 'and'.

                # set some flags telling something about this container..
                my $is_simple_logical_expression = 0;
                if (   $item_count_stack[$current_depth] == 0
                    && $saw_opening_structure
                    && $tokens_to_go[$i_opening] eq '('
                    && $is_logical_container{ $container_type[$current_depth] }
                  )
                {

                    # This seems to be a simple logical expression with
                    # no existing breakpoints.  Set a flag to prevent
                    # opening it up.
                    if ( !$has_comma_breakpoints ) {
                        $is_simple_logical_expression = 1;
                    }

                    # This seems to be a simple logical expression with
                    # breakpoints (broken sublists, for example).  Break
                    # at all 'or's and '||'s.
                    else {
                        $self->set_logical_breakpoints($current_depth);
                    }
                } ## end if ( $item_count_stack...)

                if ( $is_long_term
                    && @{ $rfor_semicolon_list[$current_depth] } )
                {
                    $self->set_for_semicolon_breakpoints($current_depth);

                    # open up a long 'for' or 'foreach' container to allow
                    # leading term alignment unless -lp is used.
                    $has_comma_breakpoints = 1
                      unless $rOpts_line_up_parentheses;
                } ## end if ( $is_long_term && ...)

                if (

                    # breaks for code BLOCKS are handled at a higher level
                    !$block_type

                    # we do not need to break at the top level of an 'if'
                    # type expression
                    && !$is_simple_logical_expression

                    ## modification to keep ': (' containers vertically tight;
                    ## but probably better to let user set -vt=1 to avoid
                    ## inconsistency with other paren types
                    ## && ($container_type[$current_depth] ne ':')

                    # otherwise, we require one of these reasons for breaking:
                    && (

                        # - this term has forced line breaks
                        $has_comma_breakpoints

                       # - the opening container is separated from this batch
                       #   for some reason (comment, blank line, code block)
                       # - this is a non-paren container spanning multiple lines
                        || !$saw_opening_structure

                        # - this is a long block contained in another breakable
                        #   container
                        || (   $is_long_term
                            && $container_environment_to_go[$i_opening] ne
                            'BLOCK' )
                    )
                  )
                {

                    # For -lp option, we must put a breakpoint before
                    # the token which has been identified as starting
                    # this indentation level.  This is necessary for
                    # proper alignment.
                    if ( $rOpts_line_up_parentheses && $saw_opening_structure )
                    {
                        my $item = $leading_spaces_to_go[ $i_opening + 1 ];
                        if (   $i_opening + 1 < $max_index_to_go
                            && $types_to_go[ $i_opening + 1 ] eq 'b' )
                        {
                            $item = $leading_spaces_to_go[ $i_opening + 2 ];
                        }
                        if ( defined($item) ) {
                            my $i_start_2;
                            my $K_start_2 = $item->get_starting_index_K();
                            if ( defined($K_start_2) ) {
                                $i_start_2 = $K_start_2 - $K_to_go[0];
                            }
                            if (
                                defined($i_start_2)

                                # we are breaking after an opening brace, paren,
                                # so don't break before it too
                                && $i_start_2 ne $i_opening
                                && $i_start_2 >= 0
                                && $i_start_2 <= $max_index_to_go
                              )
                            {

                                # Only break for breakpoints at the same
                                # indentation level as the opening paren
                                my $test1 = $nesting_depth_to_go[$i_opening];
                                my $test2 = $nesting_depth_to_go[$i_start_2];
                                if ( $test2 == $test1 ) {
                                    $self->set_forced_breakpoint(
                                        $i_start_2 - 1 );
                                }
                            } ## end if ( defined($i_start_2...))
                        } ## end if ( defined($item) )
                    } ## end if ( $rOpts_line_up_parentheses...)

                    # break after opening structure.
                    # note: break before closing structure will be automatic
                    if ( $minimum_depth <= $current_depth ) {

                        $self->set_forced_breakpoint($i_opening)
                          unless ( $do_not_break_apart
                            || is_unbreakable_container($current_depth) );

                        # break at ',' of lower depth level before opening token
                        if ( $last_comma_index[$depth] ) {
                            $self->set_forced_breakpoint(
                                $last_comma_index[$depth] );
                        }

                        # break at '.' of lower depth level before opening token
                        if ( $last_dot_index[$depth] ) {
                            $self->set_forced_breakpoint(
                                $last_dot_index[$depth] );
                        }

                        # break before opening structure if preceded by another
                        # closing structure and a comma.  This is normally
                        # done by the previous closing brace, but not
                        # if it was a one-line block.
                        if ( $i_opening > 2 ) {
                            my $i_prev =
                              ( $types_to_go[ $i_opening - 1 ] eq 'b' )
                              ? $i_opening - 2
                              : $i_opening - 1;

                            if (
                                $types_to_go[$i_prev] eq ','
                                && (   $types_to_go[ $i_prev - 1 ] eq ')'
                                    || $types_to_go[ $i_prev - 1 ] eq '}' )
                              )
                            {
                                $self->set_forced_breakpoint($i_prev);
                            }

                            # also break before something like ':('  or '?('
                            # if appropriate.
                            elsif (
                                $types_to_go[$i_prev] =~ /^([k\:\?]|&&|\|\|)$/ )
                            {
                                my $token_prev = $tokens_to_go[$i_prev];
                                if ( $want_break_before{$token_prev} ) {
                                    $self->set_forced_breakpoint($i_prev);
                                }
                            } ## end elsif ( $types_to_go[$i_prev...])
                        } ## end if ( $i_opening > 2 )
                    } ## end if ( $minimum_depth <=...)

                    # break after comma following closing structure
                    if ( $next_type eq ',' ) {
                        $self->set_forced_breakpoint( $i + 1 );
                    }

                    # break before an '=' following closing structure
                    if (
                        $is_assignment{$next_nonblank_type}
                        && ( $breakpoint_stack[$current_depth] !=
                            get_forced_breakpoint_count() )
                      )
                    {
                        $self->set_forced_breakpoint($i);
                    } ## end if ( $is_assignment{$next_nonblank_type...})

                    # break at any comma before the opening structure Added
                    # for -lp, but seems to be good in general.  It isn't
                    # obvious how far back to look; the '5' below seems to
                    # work well and will catch the comma in something like
                    #  push @list, myfunc( $param, $param, ..

                    my $icomma = $last_comma_index[$depth];
                    if ( defined($icomma) && ( $i_opening - $icomma ) < 5 ) {
                        unless ( $forced_breakpoint_to_go[$icomma] ) {
                            $self->set_forced_breakpoint($icomma);
                        }
                    }
                }    # end logic to open up a container

                # Break open a logical container open if it was already open
                elsif ($is_simple_logical_expression
                    && $has_old_logical_breakpoints[$current_depth] )
                {
                    $self->set_logical_breakpoints($current_depth);
                }

                # Handle long container which does not get opened up
                elsif ($is_long_term) {

                    # must set fake breakpoint to alert outer containers that
                    # they are complex
                    set_fake_breakpoint();
                } ## end elsif ($is_long_term)

            } ## end elsif ( $depth < $current_depth)

            #------------------------------------------------------------
            # Handle this token
            #------------------------------------------------------------

            $current_depth = $depth;

            # most token types can skip the rest of this loop
            next unless ( $quick_filter{$type} );

            # handle comma-arrow
            if ( $type eq '=>' ) {
                next if ( $last_nonblank_type eq '=>' );
                next if $rOpts_break_at_old_comma_breakpoints;
                next if $rOpts_comma_arrow_breakpoints == 3;
                $want_comma_break[$depth]   = 1;
                $index_before_arrow[$depth] = $i_last_nonblank_token;
                next;
            } ## end if ( $type eq '=>' )

            elsif ( $type eq '.' ) {
                $last_dot_index[$depth] = $i;
            }

            # Turn off alignment if we are sure that this is not a list
            # environment.  To be safe, we will do this if we see certain
            # non-list tokens, such as ';', and also the environment is
            # not a list.  Note that '=' could be in any of the = operators
            # (lextest.t). We can't just use the reported environment
            # because it can be incorrect in some cases.
            elsif ( ( $type =~ /^[\;\<\>\~]$/ || $is_assignment{$type} )
                && $container_environment_to_go[$i] ne 'LIST' )
            {
                $dont_align[$depth]         = 1;
                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;
            } ## end elsif ( ( $type =~ /^[\;\<\>\~]$/...))

            # now just handle any commas
            next unless ( $type eq ',' );

            $last_dot_index[$depth]   = undef;
            $last_comma_index[$depth] = $i;

            # break here if this comma follows a '=>'
            # but not if there is a side comment after the comma
            if ( $want_comma_break[$depth] ) {

                if ( $next_nonblank_type =~ /^[\)\}\]R]$/ ) {
                    if ($rOpts_comma_arrow_breakpoints) {
                        $want_comma_break[$depth] = 0;
                        next;
                    }
                }

                $self->set_forced_breakpoint($i)
                  unless ( $next_nonblank_type eq '#' );

                # break before the previous token if it looks safe
                # Example of something that we will not try to break before:
                #   DBI::SQL_SMALLINT() => $ado_consts->{adSmallInt},
                # Also we don't want to break at a binary operator (like +):
                # $c->createOval(
                #    $x + $R, $y +
                #    $R => $x - $R,
                #    $y - $R, -fill   => 'black',
                # );
                my $ibreak = $index_before_arrow[$depth] - 1;
                if (   $ibreak > 0
                    && $tokens_to_go[ $ibreak + 1 ] !~ /^[\)\}\]]$/ )
                {
                    if ( $tokens_to_go[$ibreak] eq '-' ) { $ibreak-- }
                    if ( $types_to_go[$ibreak] eq 'b' )  { $ibreak-- }
                    if ( $types_to_go[$ibreak] =~ /^[,wiZCUG\(\{\[]$/ ) {

                        # don't break pointer calls, such as the following:
                        #  File::Spec->curdir  => 1,
                        # (This is tokenized as adjacent 'w' tokens)
                        ##if ( $tokens_to_go[ $ibreak + 1 ] !~ /^->/ ) {

                        # And don't break before a comma, as in the following:
                        # ( LONGER_THAN,=> 1,
                        #    EIGHTY_CHARACTERS,=> 2,
                        #    CAUSES_FORMATTING,=> 3,
                        #    LIKE_THIS,=> 4,
                        # );
                        # This example is for -tso but should be general rule
                        if (   $tokens_to_go[ $ibreak + 1 ] ne '->'
                            && $tokens_to_go[ $ibreak + 1 ] ne ',' )
                        {
                            $self->set_forced_breakpoint($ibreak);
                        }
                    } ## end if ( $types_to_go[$ibreak...])
                } ## end if ( $ibreak > 0 && $tokens_to_go...)

                $want_comma_break[$depth]   = 0;
                $index_before_arrow[$depth] = -1;

                # handle list which mixes '=>'s and ','s:
                # treat any list items so far as an interrupted list
                $interrupted_list[$depth] = 1;
                next;
            } ## end if ( $want_comma_break...)

            # break after all commas above starting depth
            if ( $depth < $starting_depth && !$dont_align[$depth] ) {
                $self->set_forced_breakpoint($i)
                  unless ( $next_nonblank_type eq '#' );
                next;
            }

            # add this comma to the list..
            my $item_count = $item_count_stack[$depth];
            if ( $item_count == 0 ) {

                # but do not form a list with no opening structure
                # for example:

                #            open INFILE_COPY, ">$input_file_copy"
                #              or die ("very long message");

                if ( ( $opening_structure_index_stack[$depth] < 0 )
                    && $container_environment_to_go[$i] eq 'BLOCK' )
                {
                    $dont_align[$depth] = 1;
                }
            } ## end if ( $item_count == 0 )

            $comma_index[$depth][$item_count] = $i;
            ++$item_count_stack[$depth];
            if ( $last_nonblank_type =~ /^[iR\]]$/ ) {
                $identifier_count_stack[$depth]++;
            }
        } ## end while ( ++$i <= $max_index_to_go)

        #-------------------------------------------
        # end of loop over all tokens in this batch
        #-------------------------------------------

        # set breaks for any unfinished lists ..
        for ( my $dd = $current_depth ; $dd >= $minimum_depth ; $dd-- ) {

            $interrupted_list[$dd]   = 1;
            $has_broken_sublist[$dd] = 1 if ( $dd < $current_depth );
            $self->set_comma_breakpoints($dd);
            $self->set_logical_breakpoints($dd)
              if ( $has_old_logical_breakpoints[$dd] );
            $self->set_for_semicolon_breakpoints($dd);

            # break open container...
            my $i_opening = $opening_structure_index_stack[$dd];
            $self->set_forced_breakpoint($i_opening)
              unless (
                is_unbreakable_container($dd)

                # Avoid a break which would place an isolated ' or "
                # on a line
                || (   $type eq 'Q'
                    && $i_opening >= $max_index_to_go - 2
                    && ( $token eq "'" || $token eq '"' ) )
              );
        } ## end for ( my $dd = $current_depth...)

        # Return a flag indicating if the input file had some good breakpoints.
        # This flag will be used to force a break in a line shorter than the
        # allowed line length.
        if ( $has_old_logical_breakpoints[$current_depth] ) {
            $saw_good_breakpoint = 1;
        }

        # A complex line with one break at an = has a good breakpoint.
        # This is not complex ($total_depth_variation=0):
        # $res1
        #   = 10;
        #
        # This is complex ($total_depth_variation=6):
        # $res2 =
        #  (is_boundp("a", 'self-insert') && is_boundp("b", 'self-insert'));
        elsif ($i_old_assignment_break
            && $total_depth_variation > 4
            && $old_breakpoint_count == 1 )
        {
            $saw_good_breakpoint = 1;
        } ## end elsif ( $i_old_assignment_break...)

        return $saw_good_breakpoint;
    } ## end sub scan_list
} ## end closure scan_list

sub find_token_starting_list {

    # When testing to see if a block will fit on one line, some
    # previous token(s) may also need to be on the line; particularly
    # if this is a sub call.  So we will look back at least one
    # token. NOTE: This isn't perfect, but not critical, because
    # if we mis-identify a block, it will be wrapped and therefore
    # fixed the next time it is formatted.
    my ( $self, $i_opening_paren ) = @_;
    my $i_opening_minus = $i_opening_paren;
    my $im1             = $i_opening_paren - 1;
    my $im2             = $i_opening_paren - 2;
    my $typem1          = $im1 >= 0 ? $types_to_go[$im1] : 'b';
    my $typem2          = $im2 >= 0 ? $types_to_go[$im2] : 'b';

    if ( $typem1 eq ',' || ( $typem1 eq 'b' && $typem2 eq ',' ) ) {
        $i_opening_minus = $i_opening_paren;
    }
    elsif ( $tokens_to_go[$i_opening_paren] eq '(' ) {
        $i_opening_minus = $im1 if $im1 >= 0;

        # walk back to improve length estimate
        for ( my $j = $im1 ; $j >= 0 ; $j-- ) {
            last if ( $types_to_go[$j] =~ /^[\(\[\{L\}\]\)Rb,]$/ );
            $i_opening_minus = $j;
        }
        if ( $types_to_go[$i_opening_minus] eq 'b' ) { $i_opening_minus++ }
    }
    elsif ( $typem1 eq 'k' ) { $i_opening_minus = $im1 }
    elsif ( $typem1 eq 'b' && $im2 >= 0 && $types_to_go[$im2] eq 'k' ) {
        $i_opening_minus = $im2;
    }
    return $i_opening_minus;
}

{    ## begin closure set_comma_breakpoints_do

    my %is_keyword_with_special_leading_term;

    BEGIN {

        # These keywords have prototypes which allow a special leading item
        # followed by a list
        my @q =
          qw(formline grep kill map printf sprintf push chmod join pack unshift);
        @is_keyword_with_special_leading_term{@q} = (1) x scalar(@q);
    }

    use constant DEBUG_SPARSE => 0;

    sub set_comma_breakpoints_do {

        # Given a list with some commas, set breakpoints at some of the
        # commas, if necessary, to make it easy to read.

        my ( $self, $rinput_hash ) = @_;

        my $depth               = $rinput_hash->{depth};
        my $i_opening_paren     = $rinput_hash->{i_opening_paren};
        my $i_closing_paren     = $rinput_hash->{i_closing_paren};
        my $item_count          = $rinput_hash->{item_count};
        my $identifier_count    = $rinput_hash->{identifier_count};
        my $rcomma_index        = $rinput_hash->{rcomma_index};
        my $next_nonblank_type  = $rinput_hash->{next_nonblank_type};
        my $list_type           = $rinput_hash->{list_type};
        my $interrupted         = $rinput_hash->{interrupted};
        my $rdo_not_break_apart = $rinput_hash->{rdo_not_break_apart};
        my $must_break_open     = $rinput_hash->{must_break_open};
        my $has_broken_sublist  = $rinput_hash->{has_broken_sublist};

        # nothing to do if no commas seen
        return if ( $item_count < 1 );

        my $i_first_comma     = $rcomma_index->[0];
        my $i_true_last_comma = $rcomma_index->[ $item_count - 1 ];
        my $i_last_comma      = $i_true_last_comma;
        if ( $i_last_comma >= $max_index_to_go ) {
            $i_last_comma = $rcomma_index->[ --$item_count - 1 ];
            return if ( $item_count < 1 );
        }

        #---------------------------------------------------------------
        # find lengths of all items in the list to calculate page layout
        #---------------------------------------------------------------
        my $comma_count = $item_count;
        my @item_lengths;
        my @i_term_begin;
        my @i_term_end;
        my @i_term_comma;
        my $i_prev_plus;
        my @max_length = ( 0, 0 );
        my $first_term_length;
        my $i      = $i_opening_paren;
        my $is_odd = 1;

        foreach my $j ( 0 .. $comma_count - 1 ) {
            $is_odd      = 1 - $is_odd;
            $i_prev_plus = $i + 1;
            $i           = $rcomma_index->[$j];

            my $i_term_end =
              ( $i == 0 || $types_to_go[ $i - 1 ] eq 'b' ) ? $i - 2 : $i - 1;
            my $i_term_begin =
              ( $types_to_go[$i_prev_plus] eq 'b' )
              ? $i_prev_plus + 1
              : $i_prev_plus;
            push @i_term_begin, $i_term_begin;
            push @i_term_end,   $i_term_end;
            push @i_term_comma, $i;

            # note: currently adding 2 to all lengths (for comma and space)
            my $length =
              2 + token_sequence_length( $i_term_begin, $i_term_end );
            push @item_lengths, $length;

            if ( $j == 0 ) {
                $first_term_length = $length;
            }
            else {

                if ( $length > $max_length[$is_odd] ) {
                    $max_length[$is_odd] = $length;
                }
            }
        }

        # now we have to make a distinction between the comma count and item
        # count, because the item count will be one greater than the comma
        # count if the last item is not terminated with a comma
        my $i_b =
          ( $types_to_go[ $i_last_comma + 1 ] eq 'b' )
          ? $i_last_comma + 1
          : $i_last_comma;
        my $i_e =
          ( $types_to_go[ $i_closing_paren - 1 ] eq 'b' )
          ? $i_closing_paren - 2
          : $i_closing_paren - 1;
        my $i_effective_last_comma = $i_last_comma;

        my $last_item_length = token_sequence_length( $i_b + 1, $i_e );

        if ( $last_item_length > 0 ) {

            # add 2 to length because other lengths include a comma and a blank
            $last_item_length += 2;
            push @item_lengths, $last_item_length;
            push @i_term_begin, $i_b + 1;
            push @i_term_end,   $i_e;
            push @i_term_comma, undef;

            my $i_odd = $item_count % 2;

            if ( $last_item_length > $max_length[$i_odd] ) {
                $max_length[$i_odd] = $last_item_length;
            }

            $item_count++;
            $i_effective_last_comma = $i_e + 1;

            if ( $types_to_go[ $i_b + 1 ] =~ /^[iR\]]$/ ) {
                $identifier_count++;
            }
        }

        #---------------------------------------------------------------
        # End of length calculations
        #---------------------------------------------------------------

        #---------------------------------------------------------------
        # Compound List Rule 1:
        # Break at (almost) every comma for a list containing a broken
        # sublist.  This has higher priority than the Interrupted List
        # Rule.
        #---------------------------------------------------------------
        if ($has_broken_sublist) {

            # Break at every comma except for a comma between two
            # simple, small terms.  This prevents long vertical
            # columns of, say, just 0's.
            my $small_length = 10;    # 2 + actual maximum length wanted

            # We'll insert a break in long runs of small terms to
            # allow alignment in uniform tables.
            my $skipped_count = 0;
            my $columns       = table_columns_available($i_first_comma);
            my $fields        = int( $columns / $small_length );
            if (   $rOpts_maximum_fields_per_table
                && $fields > $rOpts_maximum_fields_per_table )
            {
                $fields = $rOpts_maximum_fields_per_table;
            }
            my $max_skipped_count = $fields - 1;

            my $is_simple_last_term = 0;
            my $is_simple_next_term = 0;
            foreach my $j ( 0 .. $item_count ) {
                $is_simple_last_term = $is_simple_next_term;
                $is_simple_next_term = 0;
                if (   $j < $item_count
                    && $i_term_end[$j] == $i_term_begin[$j]
                    && $item_lengths[$j] <= $small_length )
                {
                    $is_simple_next_term = 1;
                }
                next if $j == 0;
                if (   $is_simple_last_term
                    && $is_simple_next_term
                    && $skipped_count < $max_skipped_count )
                {
                    $skipped_count++;
                }
                else {
                    $skipped_count = 0;
                    my $i = $i_term_comma[ $j - 1 ];
                    last unless defined $i;
                    $self->set_forced_breakpoint($i);
                }
            }

            # always break at the last comma if this list is
            # interrupted; we wouldn't want to leave a terminal '{', for
            # example.
            if ($interrupted) {
                $self->set_forced_breakpoint($i_true_last_comma);
            }
            return;
        }

#my ( $a, $b, $c ) = caller();
#print "LISTX: in set_list $a $c interrupt=$interrupted count=$item_count
#i_first = $i_first_comma  i_last=$i_last_comma max=$max_index_to_go\n";
#print "depth=$depth has_broken=$has_broken_sublist[$depth] is_multi=$is_multiline opening_paren=($i_opening_paren) \n";

        #---------------------------------------------------------------
        # Interrupted List Rule:
        # A list is forced to use old breakpoints if it was interrupted
        # by side comments or blank lines, or requested by user.
        #---------------------------------------------------------------
        if (   $rOpts_break_at_old_comma_breakpoints
            || $interrupted
            || $i_opening_paren < 0 )
        {
            $self->copy_old_breakpoints( $i_first_comma, $i_true_last_comma );
            return;
        }

        #---------------------------------------------------------------
        # Looks like a list of items.  We have to look at it and size it up.
        #---------------------------------------------------------------

        my $opening_token = $tokens_to_go[$i_opening_paren];
        my $opening_environment =
          $container_environment_to_go[$i_opening_paren];

        #-------------------------------------------------------------------
        # Return if this will fit on one line
        #-------------------------------------------------------------------

        my $i_opening_minus = $self->find_token_starting_list($i_opening_paren);
        return
          unless $self->excess_line_length( $i_opening_minus, $i_closing_paren )
          > 0;

        #-------------------------------------------------------------------
        # Now we know that this block spans multiple lines; we have to set
        # at least one breakpoint -- real or fake -- as a signal to break
        # open any outer containers.
        #-------------------------------------------------------------------
        set_fake_breakpoint();

        # be sure we do not extend beyond the current list length
        if ( $i_effective_last_comma >= $max_index_to_go ) {
            $i_effective_last_comma = $max_index_to_go - 1;
        }

        # Set a flag indicating if we need to break open to keep -lp
        # items aligned.  This is necessary if any of the list terms
        # exceeds the available space after the '('.
        my $need_lp_break_open = $must_break_open;
        if ( $rOpts_line_up_parentheses && !$must_break_open ) {
            my $columns_if_unbroken =
              $maximum_line_length[ $levels_to_go[$i_opening_minus] ] -
              total_line_length( $i_opening_minus, $i_opening_paren );
            $need_lp_break_open =
                 ( $max_length[0] > $columns_if_unbroken )
              || ( $max_length[1] > $columns_if_unbroken )
              || ( $first_term_length > $columns_if_unbroken );
        }

        # Specify if the list must have an even number of fields or not.
        # It is generally safest to assume an even number, because the
        # list items might be a hash list.  But if we can be sure that
        # it is not a hash, then we can allow an odd number for more
        # flexibility.
        my $odd_or_even = 2;    # 1 = odd field count ok, 2 = want even count

        if (   $identifier_count >= $item_count - 1
            || $is_assignment{$next_nonblank_type}
            || ( $list_type && $list_type ne '=>' && $list_type !~ /^[\:\?]$/ )
          )
        {
            $odd_or_even = 1;
        }

        # do we have a long first term which should be
        # left on a line by itself?
        my $use_separate_first_term = (
            $odd_or_even == 1           # only if we can use 1 field/line
              && $item_count > 3        # need several items
              && $first_term_length >
              2 * $max_length[0] - 2    # need long first term
              && $first_term_length >
              2 * $max_length[1] - 2    # need long first term
        );

        # or do we know from the type of list that the first term should
        # be placed alone?
        if ( !$use_separate_first_term ) {
            if ( $is_keyword_with_special_leading_term{$list_type} ) {
                $use_separate_first_term = 1;

                # should the container be broken open?
                if ( $item_count < 3 ) {
                    if ( $i_first_comma - $i_opening_paren < 4 ) {
                        ${$rdo_not_break_apart} = 1;
                    }
                }
                elsif ($first_term_length < 20
                    && $i_first_comma - $i_opening_paren < 4 )
                {
                    my $columns = table_columns_available($i_first_comma);
                    if ( $first_term_length < $columns ) {
                        ${$rdo_not_break_apart} = 1;
                    }
                }
            }
        }

        # if so,
        if ($use_separate_first_term) {

            # ..set a break and update starting values
            $use_separate_first_term = 1;
            $self->set_forced_breakpoint($i_first_comma);
            $i_opening_paren = $i_first_comma;
            $i_first_comma   = $rcomma_index->[1];
            $item_count--;
            return if $comma_count == 1;
            shift @item_lengths;
            shift @i_term_begin;
            shift @i_term_end;
            shift @i_term_comma;
        }

        # if not, update the metrics to include the first term
        else {
            if ( $first_term_length > $max_length[0] ) {
                $max_length[0] = $first_term_length;
            }
        }

        # Field width parameters
        my $pair_width = ( $max_length[0] + $max_length[1] );
        my $max_width =
          ( $max_length[0] > $max_length[1] ) ? $max_length[0] : $max_length[1];

        # Number of free columns across the page width for laying out tables
        my $columns = table_columns_available($i_first_comma);

        # Estimated maximum number of fields which fit this space
        # This will be our first guess
        my $number_of_fields_max =
          maximum_number_of_fields( $columns, $odd_or_even, $max_width,
            $pair_width );
        my $number_of_fields = $number_of_fields_max;

        # Find the best-looking number of fields
        # and make this our second guess if possible
        my ( $number_of_fields_best, $ri_ragged_break_list,
            $new_identifier_count )
          = $self->study_list_complexity( \@i_term_begin, \@i_term_end,
            \@item_lengths, $max_width );

        if (   $number_of_fields_best != 0
            && $number_of_fields_best < $number_of_fields_max )
        {
            $number_of_fields = $number_of_fields_best;
        }

        # ----------------------------------------------------------------------
        # If we are crowded and the -lp option is being used, try to
        # undo some indentation
        # ----------------------------------------------------------------------
        if (
            $rOpts_line_up_parentheses
            && (
                $number_of_fields == 0
                || (   $number_of_fields == 1
                    && $number_of_fields != $number_of_fields_best )
            )
          )
        {
            my $available_spaces =
              $self->get_available_spaces_to_go($i_first_comma);
            if ( $available_spaces > 0 ) {

                my $spaces_wanted = $max_width - $columns;    # for 1 field

                if ( $number_of_fields_best == 0 ) {
                    $number_of_fields_best =
                      get_maximum_fields_wanted( \@item_lengths );
                }

                if ( $number_of_fields_best != 1 ) {
                    my $spaces_wanted_2 =
                      1 + $pair_width - $columns;    # for 2 fields
                    if ( $available_spaces > $spaces_wanted_2 ) {
                        $spaces_wanted = $spaces_wanted_2;
                    }
                }

                if ( $spaces_wanted > 0 ) {
                    my $deleted_spaces =
                      $self->reduce_lp_indentation( $i_first_comma,
                        $spaces_wanted );

                    # redo the math
                    if ( $deleted_spaces > 0 ) {
                        $columns = table_columns_available($i_first_comma);
                        $number_of_fields_max =
                          maximum_number_of_fields( $columns, $odd_or_even,
                            $max_width, $pair_width );
                        $number_of_fields = $number_of_fields_max;

                        if (   $number_of_fields_best == 1
                            && $number_of_fields >= 1 )
                        {
                            $number_of_fields = $number_of_fields_best;
                        }
                    }
                }
            }
        }

        # try for one column if two won't work
        if ( $number_of_fields <= 0 ) {
            $number_of_fields = int( $columns / $max_width );
        }

        # The user can place an upper bound on the number of fields,
        # which can be useful for doing maintenance on tables
        if (   $rOpts_maximum_fields_per_table
            && $number_of_fields > $rOpts_maximum_fields_per_table )
        {
            $number_of_fields = $rOpts_maximum_fields_per_table;
        }

        # How many columns (characters) and lines would this container take
        # if no additional whitespace were added?
        my $packed_columns = token_sequence_length( $i_opening_paren + 1,
            $i_effective_last_comma + 1 );
        if ( $columns <= 0 ) { $columns = 1 }    # avoid divide by zero
        my $packed_lines = 1 + int( $packed_columns / $columns );

        # are we an item contained in an outer list?
        my $in_hierarchical_list = $next_nonblank_type =~ /^[\}\,]$/;

        if ( $number_of_fields <= 0 ) {

#         #---------------------------------------------------------------
#         # We're in trouble.  We can't find a single field width that works.
#         # There is no simple answer here; we may have a single long list
#         # item, or many.
#         #---------------------------------------------------------------
#
#         In many cases, it may be best to not force a break if there is just one
#         comma, because the standard continuation break logic will do a better
#         job without it.
#
#         In the common case that all but one of the terms can fit
#         on a single line, it may look better not to break open the
#         containing parens.  Consider, for example
#
#             $color =
#               join ( '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; }
#                 keys %colors );
#
#         which will look like this with the container broken:
#
#             $color = join (
#                 '/',
#                 sort { $color_value{$::a} <=> $color_value{$::b}; } keys %colors
#             );
#
#         Here is an example of this rule for a long last term:
#
#             log_message( 0, 256, 128,
#                 "Number of routes in adj-RIB-in to be considered: $peercount" );
#
#         And here is an example with a long first term:
#
#         $s = sprintf(
# "%2d wallclock secs (%$f usr %$f sys + %$f cusr %$f csys = %$f CPU)",
#             $r, $pu, $ps, $cu, $cs, $tt
#           )
#           if $style eq 'all';

            my $i_last_comma = $rcomma_index->[ $comma_count - 1 ];
            my $long_last_term =
              $self->excess_line_length( 0, $i_last_comma ) <= 0;
            my $long_first_term =
              $self->excess_line_length( $i_first_comma + 1, $max_index_to_go )
              <= 0;

            # break at every comma ...
            if (

                # if requested by user or is best looking
                $number_of_fields_best == 1

                # or if this is a sublist of a larger list
                || $in_hierarchical_list

                # or if multiple commas and we don't have a long first or last
                # term
                || ( $comma_count > 1
                    && !( $long_last_term || $long_first_term ) )
              )
            {
                foreach ( 0 .. $comma_count - 1 ) {
                    $self->set_forced_breakpoint( $rcomma_index->[$_] );
                }
            }
            elsif ($long_last_term) {

                $self->set_forced_breakpoint($i_last_comma);
                ${$rdo_not_break_apart} = 1 unless $must_break_open;
            }
            elsif ($long_first_term) {

                $self->set_forced_breakpoint($i_first_comma);
            }
            else {

                # let breaks be defined by default bond strength logic
            }
            return;
        }

        # --------------------------------------------------------
        # We have a tentative field count that seems to work.
        # How many lines will this require?
        # --------------------------------------------------------
        my $formatted_lines = $item_count / ($number_of_fields);
        if ( $formatted_lines != int $formatted_lines ) {
            $formatted_lines = 1 + int $formatted_lines;
        }

        # So far we've been trying to fill out to the right margin.  But
        # compact tables are easier to read, so let's see if we can use fewer
        # fields without increasing the number of lines.
        $number_of_fields =
          compactify_table( $item_count, $number_of_fields, $formatted_lines,
            $odd_or_even );

        # How many spaces across the page will we fill?
        my $columns_per_line =
          ( int $number_of_fields / 2 ) * $pair_width +
          ( $number_of_fields % 2 ) * $max_width;

        my $formatted_columns;

        if ( $number_of_fields > 1 ) {
            $formatted_columns =
              ( $pair_width * ( int( $item_count / 2 ) ) +
                  ( $item_count % 2 ) * $max_width );
        }
        else {
            $formatted_columns = $max_width * $item_count;
        }
        if ( $formatted_columns < $packed_columns ) {
            $formatted_columns = $packed_columns;
        }

        my $unused_columns = $formatted_columns - $packed_columns;

        # set some empirical parameters to help decide if we should try to
        # align; high sparsity does not look good, especially with few lines
        my $sparsity = ($unused_columns) / ($formatted_columns);
        my $max_allowed_sparsity =
            ( $item_count < 3 )    ? 0.1
          : ( $packed_lines == 1 ) ? 0.15
          : ( $packed_lines == 2 ) ? 0.4
          :                          0.7;

        # Begin check for shortcut methods, which avoid treating a list
        # as a table for relatively small parenthesized lists.  These
        # are usually easier to read if not formatted as tables.
        if (
            $packed_lines <= 2                    # probably can fit in 2 lines
            && $item_count < 9                    # doesn't have too many items
            && $opening_environment eq 'BLOCK'    # not a sub-container
            && $opening_token eq '('              # is paren list
          )
        {

            # Shortcut method 1: for -lp and just one comma:
            # This is a no-brainer, just break at the comma.
            if (
                $rOpts_line_up_parentheses    # -lp
                && $item_count == 2           # two items, one comma
                && !$must_break_open
              )
            {
                my $i_break = $rcomma_index->[0];
                $self->set_forced_breakpoint($i_break);
                ${$rdo_not_break_apart} = 1;
                return;

            }

            # method 2 is for most small ragged lists which might look
            # best if not displayed as a table.
            if (
                ( $number_of_fields == 2 && $item_count == 3 )
                || (
                    $new_identifier_count > 0    # isn't all quotes
                    && $sparsity > 0.15
                )    # would be fairly spaced gaps if aligned
              )
            {

                my $break_count = $self->set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                # NOTE: we should really use the true break count here,
                # which can be greater if there are large terms and
                # little space, but usually this will work well enough.
                unless ($must_break_open) {

                    if ( $break_count <= 1 ) {
                        ${$rdo_not_break_apart} = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        ${$rdo_not_break_apart} = 1;
                    }
                }
                return;
            }

        }    # end shortcut methods

        # debug stuff
        DEBUG_SPARSE && do {
            print STDOUT
"SPARSE:cols=$columns commas=$comma_count items:$item_count ids=$identifier_count pairwidth=$pair_width fields=$number_of_fields lines packed: $packed_lines packed_cols=$packed_columns fmtd:$formatted_lines cols /line:$columns_per_line  unused:$unused_columns fmtd:$formatted_columns sparsity=$sparsity allow=$max_allowed_sparsity\n";

        };

        #---------------------------------------------------------------
        # Compound List Rule 2:
        # If this list is too long for one line, and it is an item of a
        # larger list, then we must format it, regardless of sparsity
        # (ian.t).  One reason that we have to do this is to trigger
        # Compound List Rule 1, above, which causes breaks at all commas of
        # all outer lists.  In this way, the structure will be properly
        # displayed.
        #---------------------------------------------------------------

        # Decide if this list is too long for one line unless broken
        my $total_columns = table_columns_available($i_opening_paren);
        my $too_long      = $packed_columns > $total_columns;

        # For a paren list, include the length of the token just before the
        # '(' because this is likely a sub call, and we would have to
        # include the sub name on the same line as the list.  This is still
        # imprecise, but not too bad.  (steve.t)
        if ( !$too_long && $i_opening_paren > 0 && $opening_token eq '(' ) {

            $too_long = $self->excess_line_length( $i_opening_minus,
                $i_effective_last_comma + 1 ) > 0;
        }

        # FIXME: For an item after a '=>', try to include the length of the
        # thing before the '=>'.  This is crude and should be improved by
        # actually looking back token by token.
        if ( !$too_long && $i_opening_paren > 0 && $list_type eq '=>' ) {
            my $i_opening_minus = $i_opening_paren - 4;
            if ( $i_opening_minus >= 0 ) {
                $too_long = $self->excess_line_length( $i_opening_minus,
                    $i_effective_last_comma + 1 ) > 0;
            }
        }

        # Always break lists contained in '[' and '{' if too long for 1 line,
        # and always break lists which are too long and part of a more complex
        # structure.
        my $must_break_open_container = $must_break_open
          || ( $too_long
            && ( $in_hierarchical_list || $opening_token ne '(' ) );

#print "LISTX: next=$next_nonblank_type  avail cols=$columns packed=$packed_columns must format = $must_break_open_container too-long=$too_long  opening=$opening_token list_type=$list_type formatted_lines=$formatted_lines  packed=$packed_lines max_sparsity= $max_allowed_sparsity sparsity=$sparsity \n";

        #---------------------------------------------------------------
        # The main decision:
        # Now decide if we will align the data into aligned columns.  Do not
        # attempt to align columns if this is a tiny table or it would be
        # too spaced.  It seems that the more packed lines we have, the
        # sparser the list that can be allowed and still look ok.
        #---------------------------------------------------------------

        if (   ( $formatted_lines < 3 && $packed_lines < $formatted_lines )
            || ( $formatted_lines < 2 )
            || ( $unused_columns > $max_allowed_sparsity * $formatted_columns )
          )
        {

            #---------------------------------------------------------------
            # too sparse: would look ugly if aligned in a table;
            #---------------------------------------------------------------

            # use old breakpoints if this is a 'big' list
            # FIXME: See if this is still necessary. sub sweep_left_to_right
            # now fixes a lot of problems.
            if ( $packed_lines > 2 && $item_count > 10 ) {
                write_logfile_entry("List sparse: using old breakpoints\n");
                $self->copy_old_breakpoints( $i_first_comma, $i_last_comma );
            }

            # let the continuation logic handle it if 2 lines
            else {

                my $break_count = $self->set_ragged_breakpoints( \@i_term_comma,
                    $ri_ragged_break_list );
                ++$break_count if ($use_separate_first_term);

                unless ($must_break_open_container) {
                    if ( $break_count <= 1 ) {
                        ${$rdo_not_break_apart} = 1;
                    }
                    elsif ( $rOpts_line_up_parentheses && !$need_lp_break_open )
                    {
                        ${$rdo_not_break_apart} = 1;
                    }
                }
            }
            return;
        }

        #---------------------------------------------------------------
        # go ahead and format as a table
        #---------------------------------------------------------------
        write_logfile_entry(
            "List: auto formatting with $number_of_fields fields/row\n");

        my $j_first_break =
          $use_separate_first_term ? $number_of_fields : $number_of_fields - 1;

        for (
            my $j = $j_first_break ;
            $j < $comma_count ;
            $j += $number_of_fields
          )
        {
            my $i = $rcomma_index->[$j];
            $self->set_forced_breakpoint($i);
        }
        return;
    }
} ## end closure set_comma_breakpoints_do

sub study_list_complexity {

    # Look for complex tables which should be formatted with one term per line.
    # Returns the following:
    #
    #  \@i_ragged_break_list = list of good breakpoints to avoid lines
    #    which are hard to read
    #  $number_of_fields_best = suggested number of fields based on
    #    complexity; = 0 if any number may be used.
    #
    my ( $self, $ri_term_begin, $ri_term_end, $ritem_lengths, $max_width ) = @_;
    my $item_count            = @{$ri_term_begin};
    my $complex_item_count    = 0;
    my $number_of_fields_best = $rOpts_maximum_fields_per_table;
    my $i_max                 = @{$ritem_lengths} - 1;
    ##my @item_complexity;

    my $i_last_last_break = -3;
    my $i_last_break      = -2;
    my @i_ragged_break_list;

    my $definitely_complex = 30;
    my $definitely_simple  = 12;
    my $quote_count        = 0;

    for my $i ( 0 .. $i_max ) {
        my $ib = $ri_term_begin->[$i];
        my $ie = $ri_term_end->[$i];

        # define complexity: start with the actual term length
        my $weighted_length = ( $ritem_lengths->[$i] - 2 );

        ##TBD: join types here and check for variations
        ##my $str=join "", @tokens_to_go[$ib..$ie];

        my $is_quote = 0;
        if ( $types_to_go[$ib] =~ /^[qQ]$/ ) {
            $is_quote = 1;
            $quote_count++;
        }
        elsif ( $types_to_go[$ib] =~ /^[w\-]$/ ) {
            $quote_count++;
        }

        if ( $ib eq $ie ) {
            if ( $is_quote && $tokens_to_go[$ib] =~ /\s/ ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            else {
            }
        }
        else {
            if ( grep { $_ eq 'b' } @types_to_go[ $ib .. $ie ] ) {
                $complex_item_count++;
                $weighted_length *= 2;
            }
            if ( grep { $_ eq '..' } @types_to_go[ $ib .. $ie ] ) {
                $weighted_length += 4;
            }
        }

        # add weight for extra tokens.
        $weighted_length += 2 * ( $ie - $ib );

##        my $BUB = join '', @tokens_to_go[$ib..$ie];
##        print "# COMPLEXITY:$weighted_length   $BUB\n";

##push @item_complexity, $weighted_length;

        # now mark a ragged break after this item it if it is 'long and
        # complex':
        if ( $weighted_length >= $definitely_complex ) {

            # if we broke after the previous term
            # then break before it too
            if (   $i_last_break == $i - 1
                && $i > 1
                && $i_last_last_break != $i - 2 )
            {

                ## FIXME: don't strand a small term
                pop @i_ragged_break_list;
                push @i_ragged_break_list, $i - 2;
                push @i_ragged_break_list, $i - 1;
            }

            push @i_ragged_break_list, $i;
            $i_last_last_break = $i_last_break;
            $i_last_break      = $i;
        }

        # don't break before a small last term -- it will
        # not look good on a line by itself.
        elsif ($i == $i_max
            && $i_last_break == $i - 1
            && $weighted_length <= $definitely_simple )
        {
            pop @i_ragged_break_list;
        }
    }

    my $identifier_count = $i_max + 1 - $quote_count;

    # Need more tuning here..
    if (   $max_width > 12
        && $complex_item_count > $item_count / 2
        && $number_of_fields_best != 2 )
    {
        $number_of_fields_best = 1;
    }

    return ( $number_of_fields_best, \@i_ragged_break_list, $identifier_count );
}

sub get_maximum_fields_wanted {

    # Not all tables look good with more than one field of items.
    # This routine looks at a table and decides if it should be
    # formatted with just one field or not.
    # This coding is still under development.
    my ($ritem_lengths) = @_;

    my $number_of_fields_best = 0;

    # For just a few items, we tentatively assume just 1 field.
    my $item_count = @{$ritem_lengths};
    if ( $item_count <= 5 ) {
        $number_of_fields_best = 1;
    }

    # For larger tables, look at it both ways and see what looks best
    else {

        my $is_odd            = 1;
        my @max_length        = ( 0,     0 );
        my @last_length_2     = ( undef, undef );
        my @first_length_2    = ( undef, undef );
        my $last_length       = undef;
        my $total_variation_1 = 0;
        my $total_variation_2 = 0;
        my @total_variation_2 = ( 0, 0 );

        foreach my $j ( 0 .. $item_count - 1 ) {

            $is_odd = 1 - $is_odd;
            my $length = $ritem_lengths->[$j];
            if ( $length > $max_length[$is_odd] ) {
                $max_length[$is_odd] = $length;
            }

            if ( defined($last_length) ) {
                my $dl = abs( $length - $last_length );
                $total_variation_1 += $dl;
            }
            $last_length = $length;

            my $ll = $last_length_2[$is_odd];
            if ( defined($ll) ) {
                my $dl = abs( $length - $ll );
                $total_variation_2[$is_odd] += $dl;
            }
            else {
                $first_length_2[$is_odd] = $length;
            }
            $last_length_2[$is_odd] = $length;
        }
        $total_variation_2 = $total_variation_2[0] + $total_variation_2[1];

        my $factor = ( $item_count > 10 ) ? 1 : ( $item_count > 5 ) ? 0.75 : 0;
        unless ( $total_variation_2 < $factor * $total_variation_1 ) {
            $number_of_fields_best = 1;
        }
    }
    return ($number_of_fields_best);
}

sub table_columns_available {
    my $i_first_comma = shift;
    my $columns =
      $maximum_line_length[ $levels_to_go[$i_first_comma] ] -
      leading_spaces_to_go($i_first_comma);

    # Patch: the vertical formatter does not line up lines whose lengths
    # exactly equal the available line length because of allowances
    # that must be made for side comments.  Therefore, the number of
    # available columns is reduced by 1 character.
    $columns -= 1;
    return $columns;
}

sub maximum_number_of_fields {

    # how many fields will fit in the available space?
    my ( $columns, $odd_or_even, $max_width, $pair_width ) = @_;
    my $max_pairs        = int( $columns / $pair_width );
    my $number_of_fields = $max_pairs * 2;
    if (   $odd_or_even == 1
        && $max_pairs * $pair_width + $max_width <= $columns )
    {
        $number_of_fields++;
    }
    return $number_of_fields;
}

sub compactify_table {

    # given a table with a certain number of fields and a certain number
    # of lines, see if reducing the number of fields will make it look
    # better.
    my ( $item_count, $number_of_fields, $formatted_lines, $odd_or_even ) = @_;
    if ( $number_of_fields >= $odd_or_even * 2 && $formatted_lines > 0 ) {
        my $min_fields;

        for (
            $min_fields = $number_of_fields ;
            $min_fields >= $odd_or_even
            && $min_fields * $formatted_lines >= $item_count ;
            $min_fields -= $odd_or_even
          )
        {
            $number_of_fields = $min_fields;
        }
    }
    return $number_of_fields;
}

sub set_ragged_breakpoints {

    # Set breakpoints in a list that cannot be formatted nicely as a
    # table.
    my ( $self, $ri_term_comma, $ri_ragged_break_list ) = @_;

    my $break_count = 0;
    foreach ( @{$ri_ragged_break_list} ) {
        my $j = $ri_term_comma->[$_];
        if ($j) {
            $self->set_forced_breakpoint($j);
            $break_count++;
        }
    }
    return $break_count;
}

sub copy_old_breakpoints {
    my ( $self, $i_first_comma, $i_last_comma ) = @_;
    for my $i ( $i_first_comma .. $i_last_comma ) {
        if ( $old_breakpoint_to_go[$i] ) {
            $self->set_forced_breakpoint($i);
        }
    }
    return;
}

sub set_nobreaks {
    my ( $self, $i, $j ) = @_;
    if ( $i >= 0 && $i <= $j && $j <= $max_index_to_go ) {

        0 && do {
            my ( $a, $b, $c ) = caller();
            my $forced_breakpoint_count = get_forced_breakpoint_count();
            print STDOUT
"NOBREAK: forced_breakpoint $forced_breakpoint_count from $a $c with i=$i max=$max_index_to_go type=$types_to_go[$i]\n";
        };

        @nobreak_to_go[ $i .. $j ] = (1) x ( $j - $i + 1 );
    }

    # shouldn't happen; non-critical error
    else {
        0 && do {
            my ( $a, $b, $c ) = caller();
            print STDOUT
              "NOBREAK ERROR: from $a $c with i=$i j=$j max=$max_index_to_go\n";
        };
    }
    return;
}

###############################################
# CODE SECTION 12: Code for setting indentation
###############################################

sub token_sequence_length {

    # return length of tokens ($ibeg .. $iend) including $ibeg & $iend
    # returns 0 if $ibeg > $iend (shouldn't happen)
    my ( $ibeg, $iend ) = @_;
    return 0 if ( !defined($iend) || $iend < 0 || $ibeg > $iend );
    return $summed_lengths_to_go[ $iend + 1 ] if ( $ibeg < 0 );
    return $summed_lengths_to_go[ $iend + 1 ] - $summed_lengths_to_go[$ibeg];
}

sub total_line_length {

    # return length of a line of tokens ($ibeg .. $iend)
    my ( $ibeg, $iend ) = @_;

    # original coding:
    #return leading_spaces_to_go($ibeg) + token_sequence_length( $ibeg, $iend );

    # this is basically sub 'leading_spaces_to_go':
    my $indentation = $leading_spaces_to_go[$ibeg];
    if ( ref($indentation) ) { $indentation = $indentation->get_spaces() }

    return $indentation + $summed_lengths_to_go[ $iend + 1 ] -
      $summed_lengths_to_go[$ibeg];
}

sub excess_line_length {

    # return number of characters by which a line of tokens ($ibeg..$iend)
    # exceeds the allowable line length.

    # NOTE: Profiling shows that this is a critical routine for efficiency.
    # Therefore I have eliminated additional calls to subs from it.
    my ( $self, $ibeg, $iend, $ignore_right_weld ) = @_;

    # Original expression for line length
    ##$length = leading_spaces_to_go($ibeg) + token_sequence_length( $ibeg, $iend );

    # This is basically sub 'leading_spaces_to_go':
    my $indentation = $leading_spaces_to_go[$ibeg];
    if ( ref($indentation) ) { $indentation = $indentation->get_spaces() }

    my $length =
      $indentation +
      $summed_lengths_to_go[ $iend + 1 ] -
      $summed_lengths_to_go[$ibeg];

    # Include right weld lengths unless requested not to.
    if (  !$ignore_right_weld
        && $type_sequence_to_go[$iend]
        && $total_weld_count )
    {
        my $wr = $self->weld_len_right( $type_sequence_to_go[$iend],
            $types_to_go[$iend] );
        $length += $wr;
    }

    # return the excess
    return $length - $maximum_line_length[ $levels_to_go[$ibeg] ];
}

sub get_spaces {

    # return the number of leading spaces associated with an indentation
    # variable $indentation is either a constant number of spaces or an object
    # with a get_spaces method.
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_spaces() : $indentation;
}

sub get_recoverable_spaces {

    # return the number of spaces (+ means shift right, - means shift left)
    # that we would like to shift a group of lines with the same indentation
    # to get them to line up with their opening parens
    my $indentation = shift;
    return ref($indentation) ? $indentation->get_recoverable_spaces() : 0;
}

sub get_available_spaces_to_go {

    my ( $self, $ii ) = @_;
    my $item = $leading_spaces_to_go[$ii];

    # return the number of available leading spaces associated with an
    # indentation variable.  $indentation is either a constant number of
    # spaces or an object with a get_available_spaces method.
    return ref($item) ? $item->get_available_spaces() : 0;
}

{    ## begin closure set_leading_whitespace (for -lp indentation)

    # These routines are called batch-by-batch to handle the -lp indentation
    # option.  The coding is rather complex, but is only for -lp.

    my $gnu_position_predictor;
    my $gnu_sequence_number;
    my $line_start_index_to_go;
    my $max_gnu_item_index;
    my $max_gnu_stack_index;
    my %gnu_arrow_count;
    my %gnu_comma_count;
    my %last_gnu_equals;
    my @gnu_item_list;
    my @gnu_stack;

    sub initialize_gnu_vars {

        # initialize gnu variables for a new file;
        # must be called once at the start of a new file.

        # initialize the leading whitespace stack to negative levels
        # so that we can never run off the end of the stack
        $gnu_position_predictor =
          0;    # where the current token is predicted to be
        $max_gnu_stack_index = 0;
        $max_gnu_item_index  = -1;
        $gnu_stack[0]        = new_lp_indentation_item( 0, -1, -1, 0, 0 );
        @gnu_item_list       = ();
        return;
    }

    sub initialize_gnu_batch_vars {

        # initialize gnu variables for a new batch;
        # must be called before each new batch
        $gnu_sequence_number++;    # increment output batch counter
        %last_gnu_equals        = ();
        %gnu_comma_count        = ();
        %gnu_arrow_count        = ();
        $line_start_index_to_go = 0;
        $max_gnu_item_index     = UNDEFINED_INDEX;
        return;
    }

    sub new_lp_indentation_item {

        # this is an interface to the IndentationItem class
        my ( $spaces, $level, $ci_level, $available_spaces, $align_paren ) = @_;

        # A negative level implies not to store the item in the item_list
        my $index = 0;
        if ( $level >= 0 ) { $index = ++$max_gnu_item_index; }

        my $starting_index_K = 0;
        if (   defined($line_start_index_to_go)
            && $line_start_index_to_go >= 0
            && $line_start_index_to_go <= $max_index_to_go )
        {
            $starting_index_K = $K_to_go[$line_start_index_to_go];
        }

        my $item = Perl::Tidy::IndentationItem->new(
            spaces              => $spaces,
            level               => $level,
            ci_level            => $ci_level,
            available_spaces    => $available_spaces,
            index               => $index,
            gnu_sequence_number => $gnu_sequence_number,
            align_paren         => $align_paren,
            stack_depth         => $max_gnu_stack_index,
            starting_index_K    => $starting_index_K,
        );

        if ( $level >= 0 ) {
            $gnu_item_list[$max_gnu_item_index] = $item;
        }

        return $item;
    }

    sub set_leading_whitespace {

        # This routine defines leading whitespace for the case of -lp formatting
        # given: the level and continuation_level of a token,
        # define: space count of leading string which would apply if it
        # were the first token of a new line.

        my ( $self, $Kj, $K_last_nonblank, $K_last_last_nonblank,
            $level_abs, $ci_level, $in_continued_quote )
          = @_;

        return unless ($rOpts_line_up_parentheses);
        return unless ( defined($max_index_to_go) && $max_index_to_go >= 0 );

        my $rbreak_container = $self->[_rbreak_container_];
        my $rshort_nested    = $self->[_rshort_nested_];
        my $rLL              = $self->[_rLL_];

        # find needed previous nonblank tokens
        my $last_nonblank_token      = '';
        my $last_nonblank_type       = '';
        my $last_nonblank_block_type = '';

        # and previous nonblank tokens, just in this batch:
        my $last_nonblank_token_in_batch     = '';
        my $last_nonblank_type_in_batch      = '';
        my $last_last_nonblank_type_in_batch = '';

        if ( defined($K_last_nonblank) ) {
            $last_nonblank_token = $rLL->[$K_last_nonblank]->[_TOKEN_];
            $last_nonblank_type  = $rLL->[$K_last_nonblank]->[_TYPE_];
            $last_nonblank_block_type =
              $rLL->[$K_last_nonblank]->[_BLOCK_TYPE_];

            if ( $K_last_nonblank >= $K_to_go[0] ) {
                $last_nonblank_token_in_batch = $last_nonblank_token;
                $last_nonblank_type_in_batch  = $last_nonblank_type;
                if ( defined($K_last_last_nonblank)
                    && $K_last_last_nonblank > $K_to_go[0] )
                {
                    $last_last_nonblank_type_in_batch =
                      $rLL->[$K_last_last_nonblank]->[_TYPE_];
                }
            }
        }

        ################################################################

        # Adjust levels if necessary to recycle whitespace:
        my $level            = $level_abs;
        my $radjusted_levels = $self->[_radjusted_levels_];
        my $nK               = @{$rLL};
        my $nws              = @{$radjusted_levels};
        if ( defined($radjusted_levels) && @{$radjusted_levels} == @{$rLL} ) {
            $level = $radjusted_levels->[$Kj];
            if ( $level < 0 ) { $level = 0 }    # note: this should not happen
        }

        # The continued_quote flag means that this is the first token of a
        # line, and it is the continuation of some kind of multi-line quote
        # or pattern.  It requires special treatment because it must have no
        # added leading whitespace. So we create a special indentation item
        # which is not in the stack.
        if ($in_continued_quote) {
            my $space_count     = 0;
            my $available_space = 0;
            $level = -1;    # flag to prevent storing in item_list
            $leading_spaces_to_go[$max_index_to_go] =
              $reduced_spaces_to_go[$max_index_to_go] =
              new_lp_indentation_item( $space_count, $level, $ci_level,
                $available_space, 0 );
            return;
        }

        # get the top state from the stack
        my $space_count      = $gnu_stack[$max_gnu_stack_index]->get_spaces();
        my $current_level    = $gnu_stack[$max_gnu_stack_index]->get_level();
        my $current_ci_level = $gnu_stack[$max_gnu_stack_index]->get_ci_level();

        my $type        = $types_to_go[$max_index_to_go];
        my $token       = $tokens_to_go[$max_index_to_go];
        my $total_depth = $nesting_depth_to_go[$max_index_to_go];

        if ( $type eq '{' || $type eq '(' ) {

            $gnu_comma_count{ $total_depth + 1 } = 0;
            $gnu_arrow_count{ $total_depth + 1 } = 0;

            # If we come to an opening token after an '=' token of some type,
            # see if it would be helpful to 'break' after the '=' to save space
            my $last_equals = $last_gnu_equals{$total_depth};
            if ( $last_equals && $last_equals > $line_start_index_to_go ) {

                # find the position if we break at the '='
                my $i_test = $last_equals;
                if ( $types_to_go[ $i_test + 1 ] eq 'b' ) { $i_test++ }

                # TESTING
                ##my $too_close = ($i_test==$max_index_to_go-1);

                my $test_position =
                  total_line_length( $i_test, $max_index_to_go );
                my $mll = $maximum_line_length[ $levels_to_go[$i_test] ];

                if (

                    # the equals is not just before an open paren (testing)
                    ##!$too_close &&

                    # if we are beyond the midpoint
                    $gnu_position_predictor >
                    $mll - $rOpts_maximum_line_length / 2

                    # or we are beyond the 1/4 point and there was an old
                    # break at the equals
                    || (
                        $gnu_position_predictor >
                        $mll - $rOpts_maximum_line_length * 3 / 4
                        && (
                            $old_breakpoint_to_go[$last_equals]
                            || (   $last_equals > 0
                                && $old_breakpoint_to_go[ $last_equals - 1 ] )
                            || (   $last_equals > 1
                                && $types_to_go[ $last_equals - 1 ] eq 'b'
                                && $old_breakpoint_to_go[ $last_equals - 2 ] )
                        )
                    )
                  )
                {

                    # then make the switch -- note that we do not set a real
                    # breakpoint here because we may not really need one; sub
                    # scan_list will do that if necessary
                    $line_start_index_to_go = $i_test + 1;
                    $gnu_position_predictor = $test_position;
                }
            }
        }

        my $halfway =
          $maximum_line_length[$level] - $rOpts_maximum_line_length / 2;

        # Check for decreasing depth ..
        # Note that one token may have both decreasing and then increasing
        # depth. For example, (level, ci) can go from (1,1) to (2,0).  So,
        # in this example we would first go back to (1,0) then up to (2,0)
        # in a single call.
        if ( $level < $current_level || $ci_level < $current_ci_level ) {

            # loop to find the first entry at or completely below this level
            my ( $lev, $ci_lev );
            while (1) {
                if ($max_gnu_stack_index) {

                    # save index of token which closes this level
                    $gnu_stack[$max_gnu_stack_index]
                      ->set_closed($max_index_to_go);

                    # Undo any extra indentation if we saw no commas
                    my $available_spaces =
                      $gnu_stack[$max_gnu_stack_index]->get_available_spaces();

                    my $comma_count = 0;
                    my $arrow_count = 0;
                    if ( $type eq '}' || $type eq ')' ) {
                        $comma_count = $gnu_comma_count{$total_depth};
                        $arrow_count = $gnu_arrow_count{$total_depth};
                        $comma_count = 0 unless $comma_count;
                        $arrow_count = 0 unless $arrow_count;
                    }
                    $gnu_stack[$max_gnu_stack_index]
                      ->set_comma_count($comma_count);
                    $gnu_stack[$max_gnu_stack_index]
                      ->set_arrow_count($arrow_count);

                    if ( $available_spaces > 0 ) {

                        if ( $comma_count <= 0 || $arrow_count > 0 ) {

                            my $i =
                              $gnu_stack[$max_gnu_stack_index]->get_index();
                            my $seqno =
                              $gnu_stack[$max_gnu_stack_index]
                              ->get_sequence_number();

                            # Be sure this item was created in this batch.  This
                            # should be true because we delete any available
                            # space from open items at the end of each batch.
                            if (   $gnu_sequence_number != $seqno
                                || $i > $max_gnu_item_index )
                            {
                                warning(
"Program bug with -lp.  seqno=$seqno should be $gnu_sequence_number and i=$i should be less than max=$max_gnu_item_index\n"
                                );
                                report_definite_bug();
                            }

                            else {
                                if ( $arrow_count == 0 ) {
                                    $gnu_item_list[$i]
                                      ->permanently_decrease_available_spaces(
                                        $available_spaces);
                                }
                                else {
                                    $gnu_item_list[$i]
                                      ->tentatively_decrease_available_spaces(
                                        $available_spaces);
                                }
                                foreach my $j ( $i + 1 .. $max_gnu_item_index )
                                {
                                    $gnu_item_list[$j]
                                      ->decrease_SPACES($available_spaces);
                                }
                            }
                        }
                    }

                    # go down one level
                    --$max_gnu_stack_index;
                    $lev    = $gnu_stack[$max_gnu_stack_index]->get_level();
                    $ci_lev = $gnu_stack[$max_gnu_stack_index]->get_ci_level();

                    # stop when we reach a level at or below the current level
                    if ( $lev <= $level && $ci_lev <= $ci_level ) {
                        $space_count =
                          $gnu_stack[$max_gnu_stack_index]->get_spaces();
                        $current_level    = $lev;
                        $current_ci_level = $ci_lev;
                        last;
                    }
                }

                # reached bottom of stack .. should never happen because
                # only negative levels can get here, and $level was forced
                # to be positive above.
                else {
                    warning(
"program bug with -lp: stack_error. level=$level; lev=$lev; ci_level=$ci_level; ci_lev=$ci_lev; rerun with -nlp\n"
                    );
                    report_definite_bug();
                    last;
                }
            }
        }

        # handle increasing depth
        if ( $level > $current_level || $ci_level > $current_ci_level ) {

            # Compute the standard incremental whitespace.  This will be
            # the minimum incremental whitespace that will be used.  This
            # choice results in a smooth transition between the gnu-style
            # and the standard style.
            my $standard_increment =
              ( $level - $current_level ) *
              $rOpts_indent_columns +
              ( $ci_level - $current_ci_level ) *
              $rOpts_continuation_indentation;

            # Now we have to define how much extra incremental space
            # ("$available_space") we want.  This extra space will be
            # reduced as necessary when long lines are encountered or when
            # it becomes clear that we do not have a good list.
            my $available_space = 0;
            my $align_paren     = 0;
            my $excess          = 0;

            my $last_nonblank_seqno;
            if ( defined($K_last_nonblank) ) {
                $last_nonblank_seqno =
                  $rLL->[$K_last_nonblank]->[_TYPE_SEQUENCE_];
            }

            # initialization on empty stack..
            if ( $max_gnu_stack_index == 0 ) {
                $space_count = $level * $rOpts_indent_columns;
            }

            # if this is a BLOCK, add the standard increment
            elsif ($last_nonblank_block_type) {
                $space_count += $standard_increment;
            }

            # if last nonblank token was not structural indentation,
            # just use standard increment
            elsif ( $last_nonblank_type ne '{' ) {
                $space_count += $standard_increment;
            }

            # if this container holds a qw, add the standard increment
            elsif ($last_nonblank_seqno
                && $self->[_rcontains_multiline_qw_by_seqno_]
                ->{$last_nonblank_seqno} )
            {
                $space_count += $standard_increment;
            }

            # otherwise use the space to the first non-blank level change token
            else {

                $space_count = $gnu_position_predictor;

                my $min_gnu_indentation =
                  $gnu_stack[$max_gnu_stack_index]->get_spaces();

                $available_space = $space_count - $min_gnu_indentation;
                if ( $available_space >= $standard_increment ) {
                    $min_gnu_indentation += $standard_increment;
                }
                elsif ( $available_space > 1 ) {
                    $min_gnu_indentation += $available_space + 1;
                }
                elsif ( $last_nonblank_token =~ /^[\{\[\(]$/ ) {
                    if ( ( $tightness{$last_nonblank_token} < 2 ) ) {
                        $min_gnu_indentation += 2;
                    }
                    else {
                        $min_gnu_indentation += 1;
                    }
                }
                else {
                    $min_gnu_indentation += $standard_increment;
                }
                $available_space = $space_count - $min_gnu_indentation;

                if ( $available_space < 0 ) {
                    $space_count     = $min_gnu_indentation;
                    $available_space = 0;
                }
                $align_paren = 1;
            }

            # update state, but not on a blank token
            if ( $types_to_go[$max_index_to_go] ne 'b' ) {

                $gnu_stack[$max_gnu_stack_index]->set_have_child(1);

                ++$max_gnu_stack_index;
                $gnu_stack[$max_gnu_stack_index] =
                  new_lp_indentation_item( $space_count, $level, $ci_level,
                    $available_space, $align_paren );

                # If the opening paren is beyond the half-line length, then
                # we will use the minimum (standard) indentation.  This will
                # help avoid problems associated with running out of space
                # near the end of a line.  As a result, in deeply nested
                # lists, there will be some indentations which are limited
                # to this minimum standard indentation. But the most deeply
                # nested container will still probably be able to shift its
                # parameters to the right for proper alignment, so in most
                # cases this will not be noticeable.
                if ( $available_space > 0 && $space_count > $halfway ) {
                    $gnu_stack[$max_gnu_stack_index]
                      ->tentatively_decrease_available_spaces($available_space);
                }
            }
        }

        # Count commas and look for non-list characters.  Once we see a
        # non-list character, we give up and don't look for any more commas.
        if ( $type eq '=>' ) {
            $gnu_arrow_count{$total_depth}++;

            # tentatively treating '=>' like '=' for estimating breaks
            # TODO: this could use some experimentation
            $last_gnu_equals{$total_depth} = $max_index_to_go;
        }

        elsif ( $type eq ',' ) {
            $gnu_comma_count{$total_depth}++;
        }

        elsif ( $is_assignment{$type} ) {
            $last_gnu_equals{$total_depth} = $max_index_to_go;
        }

        # this token might start a new line
        # if this is a non-blank..
        if ( $type ne 'b' ) {

            # and if ..
            if (

                # this is the first nonblank token of the line
                $max_index_to_go == 1 && $types_to_go[0] eq 'b'

                # or previous character was one of these:
                || $last_nonblank_type_in_batch =~ /^([\:\?\,f])$/

                # or previous character was opening and this does not close it
                || ( $last_nonblank_type_in_batch eq '{' && $type ne '}' )
                || ( $last_nonblank_type_in_batch eq '(' and $type ne ')' )

                # or this token is one of these:
                || $type =~ /^([\.]|\|\||\&\&)$/

                # or this is a closing structure
                || (   $last_nonblank_type_in_batch eq '}'
                    && $last_nonblank_token_in_batch eq
                    $last_nonblank_type_in_batch )

                # or previous token was keyword 'return'
                || (
                    $last_nonblank_type_in_batch eq 'k'
                    && (   $last_nonblank_token_in_batch eq 'return'
                        && $type ne '{' )
                )

                # or starting a new line at certain keywords is fine
                || (   $type eq 'k'
                    && $is_if_unless_and_or_last_next_redo_return{$token} )

                # or this is after an assignment after a closing structure
                || (
                    $is_assignment{$last_nonblank_type_in_batch}
                    && (
                        $last_last_nonblank_type_in_batch =~ /^[\}\)\]]$/

                        # and it is significantly to the right
                        || $gnu_position_predictor > $halfway
                    )
                )
              )
            {
                check_for_long_gnu_style_lines($max_index_to_go);
                $line_start_index_to_go = $max_index_to_go;

                # back up 1 token if we want to break before that type
                # otherwise, we may strand tokens like '?' or ':' on a line
                if ( $line_start_index_to_go > 0 ) {
                    if ( $last_nonblank_type_in_batch eq 'k' ) {

                        if ( $want_break_before{$last_nonblank_token_in_batch} )
                        {
                            $line_start_index_to_go--;
                        }
                    }
                    elsif ( $want_break_before{$last_nonblank_type_in_batch} ) {
                        $line_start_index_to_go--;
                    }
                }
            }
        }

        # remember the predicted position of this token on the output line
        if ( $max_index_to_go > $line_start_index_to_go ) {
            $gnu_position_predictor =
              total_line_length( $line_start_index_to_go, $max_index_to_go );
        }
        else {
            $gnu_position_predictor =
              $space_count + $token_lengths_to_go[$max_index_to_go];
        }

        # store the indentation object for this token
        # this allows us to manipulate the leading whitespace
        # (in case we have to reduce indentation to fit a line) without
        # having to change any token values
        $leading_spaces_to_go[$max_index_to_go] =
          $gnu_stack[$max_gnu_stack_index];
        $reduced_spaces_to_go[$max_index_to_go] =
          ( $max_gnu_stack_index > 0 && $ci_level )
          ? $gnu_stack[ $max_gnu_stack_index - 1 ]
          : $gnu_stack[$max_gnu_stack_index];
        return;
    }

    sub check_for_long_gnu_style_lines {

        # look at the current estimated maximum line length, and
        # remove some whitespace if it exceeds the desired maximum
        my ($mx_index_to_go) = @_;

        # this is only for the '-lp' style
        return unless ($rOpts_line_up_parentheses);

        # nothing can be done if no stack items defined for this line
        return if ( $max_gnu_item_index == UNDEFINED_INDEX );

        # see if we have exceeded the maximum desired line length
        # keep 2 extra free because they are needed in some cases
        # (result of trial-and-error testing)
        my $spaces_needed =
          $gnu_position_predictor -
          $maximum_line_length[ $levels_to_go[$mx_index_to_go] ] + 2;

        return if ( $spaces_needed <= 0 );

        # We are over the limit, so try to remove a requested number of
        # spaces from leading whitespace.  We are only allowed to remove
        # from whitespace items created on this batch, since others have
        # already been used and cannot be undone.
        my @candidates = ();
        my $i;

        # loop over all whitespace items created for the current batch
        for ( $i = 0 ; $i <= $max_gnu_item_index ; $i++ ) {
            my $item = $gnu_item_list[$i];

            # item must still be open to be a candidate (otherwise it
            # cannot influence the current token)
            next if ( $item->get_closed() >= 0 );

            my $available_spaces = $item->get_available_spaces();

            if ( $available_spaces > 0 ) {
                push( @candidates, [ $i, $available_spaces ] );
            }
        }

        return unless (@candidates);

        # sort by available whitespace so that we can remove whitespace
        # from the maximum available first
        @candidates = sort { $b->[1] <=> $a->[1] } @candidates;

        # keep removing whitespace until we are done or have no more
        foreach my $candidate (@candidates) {
            my ( $i, $available_spaces ) = @{$candidate};
            my $deleted_spaces =
              ( $available_spaces > $spaces_needed )
              ? $spaces_needed
              : $available_spaces;

            # remove the incremental space from this item
            $gnu_item_list[$i]->decrease_available_spaces($deleted_spaces);

            my $i_debug = $i;

            # update the leading whitespace of this item and all items
            # that came after it
            for ( ; $i <= $max_gnu_item_index ; $i++ ) {

                my $old_spaces = $gnu_item_list[$i]->get_spaces();
                if ( $old_spaces >= $deleted_spaces ) {
                    $gnu_item_list[$i]->decrease_SPACES($deleted_spaces);
                }

                # shouldn't happen except for code bug:
                else {
                    my $level        = $gnu_item_list[$i_debug]->get_level();
                    my $ci_level     = $gnu_item_list[$i_debug]->get_ci_level();
                    my $old_level    = $gnu_item_list[$i]->get_level();
                    my $old_ci_level = $gnu_item_list[$i]->get_ci_level();
                    warning(
"program bug with -lp: want to delete $deleted_spaces from item $i, but old=$old_spaces deleted: lev=$level ci=$ci_level  deleted: level=$old_level ci=$ci_level\n"
                    );
                    report_definite_bug();
                }
            }
            $gnu_position_predictor -= $deleted_spaces;
            $spaces_needed          -= $deleted_spaces;
            last unless ( $spaces_needed > 0 );
        }
        return;
    }

    sub finish_lp_batch {

        # This routine is called once after each output stream batch is
        # finished to undo indentation for all incomplete -lp
        # indentation levels.  It is too risky to leave a level open,
        # because then we can't backtrack in case of a long line to follow.
        # This means that comments and blank lines will disrupt this
        # indentation style.  But the vertical aligner may be able to
        # get the space back if there are side comments.

        # this is only for the 'lp' style
        return unless ($rOpts_line_up_parentheses);

        # nothing can be done if no stack items defined for this line
        return if ( $max_gnu_item_index == UNDEFINED_INDEX );

        # loop over all whitespace items created for the current batch
        foreach my $i ( 0 .. $max_gnu_item_index ) {
            my $item = $gnu_item_list[$i];

            # only look for open items
            next if ( $item->get_closed() >= 0 );

            # Tentatively remove all of the available space
            # (The vertical aligner will try to get it back later)
            my $available_spaces = $item->get_available_spaces();
            if ( $available_spaces > 0 ) {

                # delete incremental space for this item
                $gnu_item_list[$i]
                  ->tentatively_decrease_available_spaces($available_spaces);

                # Reduce the total indentation space of any nodes that follow
                # Note that any such nodes must necessarily be dependents
                # of this node.
                foreach ( $i + 1 .. $max_gnu_item_index ) {
                    $gnu_item_list[$_]->decrease_SPACES($available_spaces);
                }
            }
        }
        return;
    }
} ## end closure set_leading_whitespace

sub reduce_lp_indentation {

    # reduce the leading whitespace at token $i if possible by $spaces_needed
    # (a large value of $spaces_needed will remove all excess space)
    # NOTE: to be called from scan_list only for a sequence of tokens
    # contained between opening and closing parens/braces/brackets

    my ( $self, $i, $spaces_wanted ) = @_;
    my $deleted_spaces = 0;

    my $item             = $leading_spaces_to_go[$i];
    my $available_spaces = $item->get_available_spaces();

    if (
        $available_spaces > 0
        && ( ( $spaces_wanted <= $available_spaces )
            || !$item->get_have_child() )
      )
    {

        # we'll remove these spaces, but mark them as recoverable
        $deleted_spaces =
          $item->tentatively_decrease_available_spaces($spaces_wanted);
    }

    return $deleted_spaces;
}

###########################################################
# CODE SECTION 13: Preparing batches for vertical alignment
###########################################################

sub send_lines_to_vertical_aligner {

    my ($self) = @_;

    # This routine receives a batch of code for which the final line breaks
    # have been defined. Here we prepare the lines for passing to the vertical
    # aligner.  We do the following tasks:
    # - mark certain vertical alignment tokens, such as '=', in each line
    # - make minor indentation adjustments
    # - do logical padding: insert extra blank spaces to help display certain
    #   logical constructions

    my $this_batch = $self->[_this_batch_];
    my $rlines_K   = $this_batch->[_rlines_K_];
    if ( !@{$rlines_K} ) {

        # This can't happen because sub grind_batch_of_CODE always receives
        # tokens which it turns into one or more lines. If we get here it means
        # that a programming error has caused those lines to be lost.
        Fault("Unexpected call with no lines");
        return;
    }
    my $n_last_line = @{$rlines_K} - 1;

    my $do_not_pad               = $this_batch->[_do_not_pad_];
    my $peak_batch_size          = $this_batch->[_peak_batch_size_];
    my $starting_in_quote        = $this_batch->[_starting_in_quote_];
    my $ending_in_quote          = $this_batch->[_ending_in_quote_];
    my $is_static_block_comment  = $this_batch->[_is_static_block_comment_];
    my $ibeg0                    = $this_batch->[_ibeg0_];
    my $rK_to_go                 = $this_batch->[_rK_to_go_];
    my $batch_count              = $this_batch->[_batch_count_];
    my $rix_seqno_controlling_ci = $this_batch->[_rix_seqno_controlling_ci_];

    my $rLL    = $self->[_rLL_];
    my $Klimit = $self->[_Klimit_];

    my ( $Kbeg_next, $Kend_next ) = @{ $rlines_K->[0] };
    my $type_beg_next  = $rLL->[$Kbeg_next]->[_TYPE_];
    my $token_beg_next = $rLL->[$Kbeg_next]->[_TOKEN_];
    my $type_end_next  = $rLL->[$Kend_next]->[_TYPE_];

    # Construct indexes to the global_to_go arrays so that called routines can
    # still access those arrays. This might eventually be removed
    # when all called routines have been converted to access token values
    # in the rLL array instead.
    my $Kbeg0 = $Kbeg_next;
    my ( $ri_first, $ri_last );
    foreach my $rline ( @{$rlines_K} ) {
        my ( $Kbeg, $Kend ) = @{$rline};
        my $ibeg = $ibeg0 + $Kbeg - $Kbeg0;
        my $iend = $ibeg0 + $Kend - $Kbeg0;
        push @{$ri_first}, $ibeg;
        push @{$ri_last},  $iend;
    }

    my ( $cscw_block_comment, $closing_side_comment );
    if ( $rOpts->{'closing-side-comments'} ) {
        ( $closing_side_comment, $cscw_block_comment ) =
          $self->add_closing_side_comment();
    }

    my $rindentation_list = [0];    # ref to indentations for each line

    # define the array @{$ralignment_type_to_go} for the output tokens
    # which will be non-blank for each special token (such as =>)
    # for which alignment is required.
    my $ralignment_type_to_go =
      $self->set_vertical_alignment_markers( $ri_first, $ri_last );

    # flush before a long if statement to avoid unwanted alignment
    if (   $n_last_line > 0
        && $type_beg_next eq 'k'
        && $token_beg_next =~ /^(if|unless)$/ )
    {
        $self->flush_vertical_aligner();
    }

    $self->undo_ci( $ri_first, $ri_last, $rix_seqno_controlling_ci );

    $self->set_logical_padding( $ri_first, $ri_last, $peak_batch_size,
        $starting_in_quote )
      if ( $rOpts->{'logical-padding'} );

    # Resum lengths. We need accurate lengths for making alignment patterns,
    # and we may have unmasked a semicolon which was not included at the start.
    for ( 0 .. $max_index_to_go ) {
        $summed_lengths_to_go[ $_ + 1 ] =
          $summed_lengths_to_go[$_] + $token_lengths_to_go[$_];
    }

    # loop to prepare each line for shipment
    my ( $Kbeg, $type_beg, $token_beg );
    my ( $Kend, $type_end );
    for my $n ( 0 .. $n_last_line ) {

        my $ibeg              = $ri_first->[$n];
        my $iend              = $ri_last->[$n];
        my $rline             = $rlines_K->[$n];
        my $forced_breakpoint = $rline->[2];

        # we may need to look at variables on three consecutive lines ...

        # Some vars on line [n-1], if any:
        my $Kbeg_last      = $Kbeg;
        my $type_beg_last  = $type_beg;
        my $token_beg_last = $token_beg;
        my $Kend_last      = $Kend;
        my $type_end_last  = $type_end;

        # Some vars on line [n]:
        $Kbeg      = $Kbeg_next;
        $type_beg  = $type_beg_next;
        $token_beg = $token_beg_next;
        $Kend      = $Kend_next;
        $type_end  = $type_end_next;

        # Only forward ending K values of non-comments down the pipeline.
        # This is equivalent to checking that the last CODE_type is blank or
        # equal to 'VER'. See also sub resync_lines_and_tokens for related
        # coding.  Note that '$batch_CODE_type' is the code type of the line
        # to which the ending token belongs.
        my $batch_CODE_type = $this_batch->[_batch_CODE_type_];
        my $Kend_code =
          $batch_CODE_type && $batch_CODE_type ne 'VER' ? undef : $Kend;

        # We use two slightly different definitions of level jump at the end
        # of line:
        #  $ljump is the level jump needed by 'sub set_adjusted_indentation'
        #  $level_jump is the level jump needed by the vertical aligner.
        my $ljump = 0;    # level jump at end of line

        # Get some vars on line [n+1], if any:
        if ( $n < $n_last_line ) {
            ( $Kbeg_next, $Kend_next ) =
              @{ $rlines_K->[ $n + 1 ] };
            $type_beg_next  = $rLL->[$Kbeg_next]->[_TYPE_];
            $token_beg_next = $rLL->[$Kbeg_next]->[_TOKEN_];
            $type_end_next  = $rLL->[$Kend_next]->[_TYPE_];
            $ljump = $rLL->[$Kbeg_next]->[_LEVEL_] - $rLL->[$Kend]->[_LEVEL_];
        }

        # level jump at end of line for the vertical aligner:
        my $level_jump =
          $Kend >= $Klimit
          ? 0
          : $rLL->[ $Kend + 1 ]->[_SLEVEL_] - $rLL->[$Kbeg]->[_SLEVEL_];

        $self->delete_needless_alignments( $ibeg, $iend,
            $ralignment_type_to_go );

        my ( $rtokens, $rfields, $rpatterns, $rfield_lengths ) =
          $self->make_alignment_patterns( $ibeg, $iend,
            $ralignment_type_to_go );

        my ( $indentation, $lev, $level_end, $terminal_type,
            $terminal_block_type, $is_semicolon_terminated, $is_outdented_line )
          = $self->set_adjusted_indentation( $ibeg, $iend, $rfields,
            $rpatterns,         $ri_first, $ri_last,
            $rindentation_list, $ljump,    $starting_in_quote,
            $is_static_block_comment, );

        # we will allow outdenting of long lines..
        my $outdent_long_lines = (

            # which are long quotes, if allowed
            ( $type_beg eq 'Q' && $rOpts->{'outdent-long-quotes'} )

            # which are long block comments, if allowed
              || (
                   $type_beg eq '#'
                && $rOpts->{'outdent-long-comments'}

                # but not if this is a static block comment
                && !$is_static_block_comment
              )
        );

        my $break_alignment_before = $is_outdented_line || $do_not_pad;
        my $break_alignment_after  = $is_outdented_line;

        # flush at an 'if' which follows a line with (1) terminal semicolon
        # or (2) terminal block_type which is not an 'if'.  This prevents
        # unwanted alignment between the lines.
        if ( $type_beg eq 'k' && $token_beg eq 'if' ) {
            my $Km           = $self->K_previous_code($Kbeg);
            my $type_m       = 'b';
            my $block_type_m = 'b';
            if ( defined($Km) ) {
                $type_m       = $rLL->[$Km]->[_TYPE_];
                $block_type_m = $rLL->[$Km]->[_BLOCK_TYPE_];
            }

            # break after anything that is not if-like
            $break_alignment_before ||= $type_m eq ';'
              || ( $type_m eq '}'
                && $block_type_m ne 'if'
                && $block_type_m ne 'unless'
                && $block_type_m ne 'elsif'
                && $block_type_m ne 'else' );
        }

        my $rvertical_tightness_flags =
          $self->set_vertical_tightness_flags( $n, $n_last_line, $ibeg, $iend,
            $ri_first, $ri_last, $ending_in_quote, $closing_side_comment );

        # Set a flag at the final ':' of a ternary chain to request
        # vertical alignment of the final term.  Here is a
        # slightly complex example:
        #
        # $self->{_text} = (
        #    !$section        ? ''
        #   : $type eq 'item' ? "the $section entry"
        #   :                   "the section on $section"
        # )
        # . (
        #   $page
        #   ? ( $section ? ' in ' : '' ) . "the $page$page_ext manpage"
        #   : ' elsewhere in this document'
        # );
        #
        my $is_terminal_ternary = 0;

        if ( $type_beg eq ':' || $n > 0 && $type_end_last eq ':' ) {
            my $last_leading_type = $n > 0 ? $type_beg_last : ':';
            if (   $terminal_type ne ';'
                && $n_last_line > $n
                && $level_end == $lev )
            {
                $level_end     = $rLL->[$Kbeg_next]->[_LEVEL_];
                $terminal_type = $rLL->[$Kbeg_next]->[_TYPE_];
            }
            if (
                $last_leading_type eq ':'
                && (   ( $terminal_type eq ';' && $level_end <= $lev )
                    || ( $terminal_type ne ':' && $level_end < $lev ) )
              )
            {

                # the terminal term must not contain any ternary terms, as in
                # my $ECHO = (
                #       $Is_MSWin32 ? ".\\echo$$"
                #     : $Is_MacOS   ? ":echo$$"
                #     : ( $Is_NetWare ? "echo$$" : "./echo$$" )
                # );
                $is_terminal_ternary = 1;

                my $KP = $rLL->[$Kbeg]->[_KNEXT_SEQ_ITEM_];
                while ( defined($KP) && $KP <= $Kend ) {
                    my $type_KP = $rLL->[$KP]->[_TYPE_];
                    if ( $type_KP eq '?' || $type_KP eq ':' ) {
                        $is_terminal_ternary = 0;
                        last;
                    }
                    $KP = $rLL->[$KP]->[_KNEXT_SEQ_ITEM_];
                }
            }
        }

        my $level_adj        = $lev;
        my $radjusted_levels = $self->[_radjusted_levels_];
        if ( defined($radjusted_levels) && @{$radjusted_levels} == @{$rLL} ) {
            $level_adj = $radjusted_levels->[$Kbeg];
            if ( $level_adj < 0 ) { $level_adj = 0 }
        }

        # add any new closing side comment to the last line
        if ( $closing_side_comment && $n == $n_last_line && @{$rfields} ) {
            $rfields->[-1] .= " $closing_side_comment";

            # NOTE: Patch for csc. We can just use 1 for the length of the csc
            # because its length should not be a limiting factor from here on.
            $rfield_lengths->[-1] += 2;
        }

        # Set flag which tells if this line is contained in a multi-line list
        my $list_seqno = $self->is_list_by_K($Kbeg);

        # send this new line down the pipe
        my $rvalign_hash = {};
        $rvalign_hash->{level}                     = $lev;
        $rvalign_hash->{level_end}                 = $level_end;
        $rvalign_hash->{level_adj}                 = $level_adj;
        $rvalign_hash->{indentation}               = $indentation;
        $rvalign_hash->{list_seqno}                = $list_seqno;
        $rvalign_hash->{outdent_long_lines}        = $outdent_long_lines;
        $rvalign_hash->{is_terminal_ternary}       = $is_terminal_ternary;
        $rvalign_hash->{is_terminal_statement}     = $is_semicolon_terminated;
        $rvalign_hash->{rvertical_tightness_flags} = $rvertical_tightness_flags;
        $rvalign_hash->{level_jump}                = $level_jump;
        $rvalign_hash->{rfields}                   = $rfields;
        $rvalign_hash->{rpatterns}                 = $rpatterns;
        $rvalign_hash->{rtokens}                   = $rtokens;
        $rvalign_hash->{rfield_lengths}            = $rfield_lengths;
        $rvalign_hash->{terminal_block_type}       = $terminal_block_type;
        $rvalign_hash->{batch_count}               = $batch_count;
        $rvalign_hash->{break_alignment_before}    = $break_alignment_before;
        $rvalign_hash->{break_alignment_after}     = $break_alignment_after;
        $rvalign_hash->{Kend}                      = $Kend_code;
        $rvalign_hash->{ci_level}                  = $ci_levels_to_go[$ibeg];

        my $vao = $self->[_vertical_aligner_object_];
        $vao->valign_input($rvalign_hash);

        $do_not_pad = 0;

        # Set flag indicating if this line ends in an opening
        # token and is very short, so that a blank line is not
        # needed if the subsequent line is a comment.
        # Examples of what we are looking for:
        #   {
        #   && (
        #   BEGIN {
        #   default {
        #   sub {
        $self->[_last_output_short_opening_token_]

          # line ends in opening token
          #              /^[\{\(\[L]$/
          = $is_opening_type{$type_end}

          # and either
          && (
            # line has either single opening token
            $Kend == $Kbeg

            # or is a single token followed by opening token.
            # Note that sub identifiers have blanks like 'sub doit'
            #                                 $token_beg !~ /\s+/
            || ( $Kend - $Kbeg <= 2 && index( $token_beg, ' ' ) < 0 )
          )

          # and limit total to 10 character widths
          && token_sequence_length( $ibeg, $iend ) <= 10;

    }    # end of loop to output each line

    # remember indentation of lines containing opening containers for
    # later use by sub set_adjusted_indentation
    $self->save_opening_indentation( $ri_first, $ri_last, $rindentation_list );

    # output any new -cscw block comment
    if ($cscw_block_comment) {
        $self->flush_vertical_aligner();
        my $file_writer_object = $self->[_file_writer_object_];
        $file_writer_object->write_code_line( $cscw_block_comment . "\n" );
    }
    return;
}

{    ## begin closure set_vertical_alignment_markers
    my %is_vertical_alignment_type;
    my %is_not_vertical_alignment_token;
    my %is_vertical_alignment_keyword;
    my %is_terminal_alignment_type;
    my %is_low_level_alignment_token;

    BEGIN {

        my @q;

        # Replaced =~ and // in the list.  // had been removed in RT 119588
        @q = qw#
          = **= += *= &= <<= &&= -= /= |= >>= ||= //= .= %= ^= x=
          { ? : => && || ~~ !~~ =~ !~ // <=> ->
          #;
        @is_vertical_alignment_type{@q} = (1) x scalar(@q);

        # These 'tokens' are not aligned. We need this to remove [
        # from the above list because it has type ='{'
        @q = qw([);
        @is_not_vertical_alignment_token{@q} = (1) x scalar(@q);

        # these are the only types aligned at a line end
        @q = qw(&& || =>);
        @is_terminal_alignment_type{@q} = (1) x scalar(@q);

        # these tokens only align at line level
        @q = ( '{', '(' );
        @is_low_level_alignment_token{@q} = (1) x scalar(@q);

        # eq and ne were removed from this list to improve alignment chances
        @q = qw(if unless and or err for foreach while until);
        @is_vertical_alignment_keyword{@q} = (1) x scalar(@q);
    }

    sub set_vertical_alignment_markers {

        # This routine takes the first step toward vertical alignment of the
        # lines of output text.  It looks for certain tokens which can serve as
        # vertical alignment markers (such as an '=').
        #
        # Method: We look at each token $i in this output batch and set
        # $ralignment_type_to_go->[$i] equal to those tokens at which we would
        # accept vertical alignment.

        my ( $self, $ri_first, $ri_last ) = @_;
        my $rspecial_side_comment_type = $self->[_rspecial_side_comment_type_];

        my $rOpts_add_whitespace = $rOpts->{'add-whitespace'};
        my $ralignment_type_to_go;

        # Initialize the alignment array. Note that closing side comments can
        # insert up to 2 additional tokens beyond the original
        # $max_index_to_go, so we need to check ri_last for the last index.
        my $max_line = @{$ri_first} - 1;
        my $iend     = $ri_last->[$max_line];
        if ( $iend < $max_index_to_go ) { $iend = $max_index_to_go }

        # nothing to do if we aren't allowed to change whitespace
        # or there is only 1 token
        if ( $iend == 0 || !$rOpts_add_whitespace ) {
            for my $i ( 0 .. $iend ) {
                $ralignment_type_to_go->[$i] = '';
            }
            return $ralignment_type_to_go;
        }

        # remember the index of last nonblank token before any sidecomment
        my $i_terminal = $max_index_to_go;
        if ( $types_to_go[$i_terminal] eq '#' ) {
            if ( $i_terminal > 0 && $types_to_go[ --$i_terminal ] eq 'b' ) {
                if ( $i_terminal > 0 ) { --$i_terminal }
            }
        }

        # look at each line of this batch..
        my $last_vertical_alignment_before_index;
        my $vert_last_nonblank_type;
        my $vert_last_nonblank_token;
        my $vert_last_nonblank_block_type;

        foreach my $line ( 0 .. $max_line ) {
            my $ibeg = $ri_first->[$line];
            my $iend = $ri_last->[$line];
            $last_vertical_alignment_before_index = -1;
            $vert_last_nonblank_type              = '';
            $vert_last_nonblank_token             = '';
            $vert_last_nonblank_block_type        = '';

            # look at each token in this output line..
            my $level_beg = $levels_to_go[$ibeg];
            foreach my $i ( $ibeg .. $iend ) {
                my $alignment_type = '';
                my $type           = $types_to_go[$i];
                my $block_type     = $block_type_to_go[$i];
                my $token          = $tokens_to_go[$i];

                # do not align tokens at lower level then start of line
                # except for side comments
                if (   $levels_to_go[$i] < $levels_to_go[$ibeg]
                    && $type ne '#' )
                {
                    $ralignment_type_to_go->[$i] = '';
                    next;
                }

                #--------------------------------------------------------
                # First see if we want to align BEFORE this token
                #--------------------------------------------------------

                # The first possible token that we can align before
                # is index 2 because: 1) it doesn't normally make sense to
                # align before the first token and 2) the second
                # token must be a blank if we are to align before
                # the third
                if ( $i < $ibeg + 2 ) { }

                # must follow a blank token
                elsif ( $types_to_go[ $i - 1 ] ne 'b' ) { }

                # align a side comment --
                elsif ( $type eq '#' ) {

                    my $KK      = $K_to_go[$i];
                    my $sc_type = $rspecial_side_comment_type->{$KK};

                    unless (

                        # it is any specially marked side comment
                        $sc_type

                        # or it is a static side comment
                        || (   $rOpts->{'static-side-comments'}
                            && $token =~ /$static_side_comment_pattern/ )

                        # or a closing side comment
                        || (   $vert_last_nonblank_block_type
                            && $token =~
                            /$closing_side_comment_prefix_pattern/ )
                      )
                    {
                        $alignment_type = $type;
                    }    ## Example of a static side comment
                }

                # otherwise, do not align two in a row to create a
                # blank field
                elsif ( $last_vertical_alignment_before_index == $i - 2 ) { }

                # align before one of these keywords
                # (within a line, since $i>1)
                elsif ( $type eq 'k' ) {

                    #  /^(if|unless|and|or|eq|ne)$/
                    if ( $is_vertical_alignment_keyword{$token} ) {
                        $alignment_type = $token;
                    }
                }

                # align before one of these types..
                # Note: add '.' after new vertical aligner is operational
                elsif ( $is_vertical_alignment_type{$type}
                    && !$is_not_vertical_alignment_token{$token} )
                {
                    $alignment_type = $token;

                    # Do not align a terminal token.  Although it might
                    # occasionally look ok to do this, this has been found to be
                    # a good general rule.  The main problems are:
                    # (1) that the terminal token (such as an = or :) might get
                    # moved far to the right where it is hard to see because
                    # nothing follows it, and
                    # (2) doing so may prevent other good alignments.
                    # Current exceptions are && and || and =>
                    if ( $i == $iend || $i >= $i_terminal ) {
                        $alignment_type = ""
                          unless ( $is_terminal_alignment_type{$type} );
                    }

                    # Do not align leading ': (' or '. ('.  This would prevent
                    # alignment in something like the following:
                    #   $extra_space .=
                    #       ( $input_line_number < 10 )  ? "  "
                    #     : ( $input_line_number < 100 ) ? " "
                    #     :                                "";
                    # or
                    #  $code =
                    #      ( $case_matters ? $accessor : " lc($accessor) " )
                    #    . ( $yesno        ? " eq "       : " ne " )

                    # Also, do not align a ( following a leading ? so we can
                    # align something like this:
                    #   $converter{$_}->{ushortok} =
                    #     $PDL::IO::Pic::biggrays
                    #     ? ( m/GIF/          ? 0 : 1 )
                    #     : ( m/GIF|RAST|IFF/ ? 0 : 1 );
                    if (
                           $i == $ibeg + 2
                        && $types_to_go[ $i - 1 ] eq 'b'
                        && (   $types_to_go[$ibeg] eq '.'
                            || $types_to_go[$ibeg] eq ':'
                            || $types_to_go[$ibeg] eq '?' )
                      )
                    {
                        $alignment_type = "";
                    }

                    # Certain tokens only align at the same level as the
                    # initial line level
                    if (   $is_low_level_alignment_token{$token}
                        && $levels_to_go[$i] != $level_beg )
                    {
                        $alignment_type = "";
                    }

                    # For a paren after keyword, only align something like this:
                    #    if    ( $a ) { &a }
                    #    elsif ( $b ) { &b }
                    if ( $token eq '(' ) {

                        if ( $vert_last_nonblank_type eq 'k' ) {
                            $alignment_type = ""
                              unless $vert_last_nonblank_token =~
                              /^(if|unless|elsif)$/;
                        }
                    }

                    # be sure the alignment tokens are unique
                    # This didn't work well: reason not determined
                    # if ($token ne $type) {$alignment_type .= $type}
                }

                # NOTE: This is deactivated because it causes the previous
                # if/elsif alignment to fail
                #elsif ( $type eq '}' && $token eq '}' && $block_type_to_go[$i])
                #{ $alignment_type = $type; }

                if ($alignment_type) {
                    $last_vertical_alignment_before_index = $i;
                }

                #--------------------------------------------------------
                # Next see if we want to align AFTER the previous nonblank
                #--------------------------------------------------------

                # We want to line up ',' and interior ';' tokens, with the added
                # space AFTER these tokens.  (Note: interior ';' is included
                # because it may occur in short blocks).
                if (

                    # we haven't already set it
                    !$alignment_type

                    # and its not the first token of the line
                    && ( $i > $ibeg )

                    # and it follows a blank
                    && $types_to_go[ $i - 1 ] eq 'b'

                    # and previous token IS one of these:
                    && (   $vert_last_nonblank_type eq ','
                        || $vert_last_nonblank_type eq ';' )

                    # and it's NOT one of these
                    && (   $type ne 'b'
                        && $type ne '#'
                        && !$is_closing_token{$type} )

                    # then go ahead and align
                  )

                {
                    $alignment_type = $vert_last_nonblank_type;
                }

                #--------------------------------------------------------
                # Undo alignment in special cases
                #--------------------------------------------------------
                if ($alignment_type) {

                    # do not align the opening brace of an anonymous sub
                    if ( $token eq '{' && $block_type =~ /$ASUB_PATTERN/ ) {
                        $alignment_type = "";
                    }
                }

                #--------------------------------------------------------
                # then store the value
                #--------------------------------------------------------
                $ralignment_type_to_go->[$i] = $alignment_type;
                if ( $type ne 'b' ) {
                    $vert_last_nonblank_type       = $type;
                    $vert_last_nonblank_token      = $token;
                    $vert_last_nonblank_block_type = $block_type;
                }
            }
        }
        return $ralignment_type_to_go;
    }
} ## end closure set_vertical_alignment_markers

sub get_seqno {

    # get opening and closing sequence numbers of a token for the vertical
    # aligner.  Assign qw quotes a value to allow qw opening and closing tokens
    # to be treated somewhat like opening and closing tokens for stacking
    # tokens by the vertical aligner.
    my ( $self, $ii, $ending_in_quote ) = @_;

    my $rLL        = $self->[_rLL_];
    my $this_batch = $self->[_this_batch_];
    my $rK_to_go   = $this_batch->[_rK_to_go_];

    my $KK    = $rK_to_go->[$ii];
    my $seqno = $rLL->[$KK]->[_TYPE_SEQUENCE_];

    if ( $rLL->[$KK]->[_TYPE_] eq 'q' ) {
        my $SEQ_QW = -1;
        my $token  = $rLL->[$KK]->[_TOKEN_];
        if ( $ii > 0 ) {
            $seqno = $SEQ_QW if ( $token =~ /^qw\s*[\(\{\[]/ );
        }
        else {
            if ( !$ending_in_quote ) {
                $seqno = $SEQ_QW if ( $token =~ /[\)\}\]]$/ );
            }
        }
    }
    return ($seqno);
}

{
    my %undo_extended_ci;

    sub initialize_undo_ci {
        %undo_extended_ci = ();
        return;
    }

    sub undo_ci {

        # Undo continuation indentation in certain sequences
        my ( $self, $ri_first, $ri_last, $rix_seqno_controlling_ci ) = @_;
        my ( $line_1, $line_2, $lev_last );
        my $this_line_is_semicolon_terminated;
        my $max_line = @{$ri_first} - 1;

        my $rseqno_controlling_my_ci = $self->[_rseqno_controlling_my_ci_];

        # Prepare a list of controlling indexes for each line if required.
        # This is used for efficient processing below.  Note: this is
        # critical for speed. In the initial implementation I just looped
        # through the @$rix_seqno_controlling_ci list below. Using NYT_prof, I
        # found that this routine was causing a huge run time in large lists.
        # On a very large list test case, this new coding dropped the run time
        # of this routine from 30 seconds to 169 milliseconds.
        my @i_controlling_ci;
        if ( @{$rix_seqno_controlling_ci} ) {
            my @tmp     = reverse @{$rix_seqno_controlling_ci};
            my $ix_next = pop @tmp;
            foreach my $line ( 0 .. $max_line ) {
                my $iend = $ri_last->[$line];
                while ( defined($ix_next) && $ix_next <= $iend ) {
                    push @{ $i_controlling_ci[$line] }, $ix_next;
                    $ix_next = pop @tmp;
                }
            }
        }

        # Loop over all lines of the batch ...
        foreach my $line ( 0 .. $max_line ) {

            ####################################
            # SECTION 1: Undo needless common CI
            ####################################

            # We are looking at leading tokens and looking for a sequence all
            # at the same level and all at a higher level than enclosing lines.

            # For example, we can undo continuation indentation in sort/map/grep
            # chains

            #    my $dat1 = pack( "n*",
            #        map { $_, $lookup->{$_} }
            #          sort { $a <=> $b }
            #          grep { $lookup->{$_} ne $default } keys %$lookup );

            # to become

            #    my $dat1 = pack( "n*",
            #        map { $_, $lookup->{$_} }
            #        sort { $a <=> $b }
            #        grep { $lookup->{$_} ne $default } keys %$lookup );

            my $ibeg = $ri_first->[$line];
            my $iend = $ri_last->[$line];
            my $lev  = $levels_to_go[$ibeg];
            if ( $line > 0 ) {

                # if we have started a chain..
                if ($line_1) {

                    # see if it continues..
                    if ( $lev == $lev_last ) {
                        if (   $types_to_go[$ibeg] eq 'k'
                            && $is_sort_map_grep{ $tokens_to_go[$ibeg] } )
                        {

                            # chain continues...
                            # check for chain ending at end of a statement
                            if ( $line == $max_line ) {

                                # see of this line ends a statement
                                $this_line_is_semicolon_terminated =
                                  $types_to_go[$iend] eq ';'

                                  # with possible side comment
                                  || ( $types_to_go[$iend] eq '#'
                                    && $iend - $ibeg >= 2
                                    && $types_to_go[ $iend - 2 ] eq ';'
                                    && $types_to_go[ $iend - 1 ] eq 'b' );
                            }
                            $line_2 = $line
                              if ($this_line_is_semicolon_terminated);
                        }
                        else {

                            # kill chain
                            $line_1 = undef;
                        }
                    }
                    elsif ( $lev < $lev_last ) {

                        # chain ends with previous line
                        $line_2 = $line - 1;
                    }
                    elsif ( $lev > $lev_last ) {

                        # kill chain
                        $line_1 = undef;
                    }

                    # undo the continuation indentation if a chain ends
                    if ( defined($line_2) && defined($line_1) ) {
                        my $continuation_line_count = $line_2 - $line_1 + 1;
                        @ci_levels_to_go[ @{$ri_first}[ $line_1 .. $line_2 ] ]
                          = (0) x ($continuation_line_count)
                          if ( $continuation_line_count >= 0 );
                        @leading_spaces_to_go[ @{$ri_first}
                          [ $line_1 .. $line_2 ] ] =
                          @reduced_spaces_to_go[ @{$ri_first}
                          [ $line_1 .. $line_2 ] ];
                        $line_1 = undef;
                    }
                }

                # not in a chain yet..
                else {

                    # look for start of a new sort/map/grep chain
                    if ( $lev > $lev_last ) {
                        if (   $types_to_go[$ibeg] eq 'k'
                            && $is_sort_map_grep{ $tokens_to_go[$ibeg] } )
                        {
                            $line_1 = $line;
                        }
                    }
                }
            }

            ######################################
            # SECTION 2: Undo ci at cuddled blocks
            ######################################

            # Note that sub set_adjusted_indentation will be called later to
            # actually do this, but for now we will tentatively mark cuddled
            # lines with ci=0 so that the the -xci loop which follows will be
            # correct at cuddles.
            if (
                $types_to_go[$ibeg] eq '}'
                && ( $nesting_depth_to_go[$iend] + 1 ==
                    $nesting_depth_to_go[$ibeg] )
              )
            {
                my $terminal_type = $types_to_go[$iend];
                if ( $terminal_type eq '#' && $iend > $ibeg ) {
                    $terminal_type = $types_to_go[ $iend - 1 ];
                    if ( $terminal_type eq '#' && $iend - 1 > $ibeg ) {
                        $terminal_type = $types_to_go[ $iend - 2 ];
                    }
                }
                if ( $terminal_type eq '{' ) {
                    my $Kbeg = $K_to_go[$ibeg];
                    $ci_levels_to_go[$ibeg] = 0;
                }
            }

            #########################################################
            # SECTION 3: Undo ci set by sub extended_ci if not needed
            #########################################################

            # Undo the ci of the leading token if its controlling token
            # went out on a previous line without ci
            if ( $ci_levels_to_go[$ibeg] ) {
                my $Kbeg  = $K_to_go[$ibeg];
                my $seqno = $rseqno_controlling_my_ci->{$Kbeg};
                if ( $seqno && $undo_extended_ci{$seqno} ) {

                    # but do not undo ci set by the -lp flag
                    if ( !ref( $reduced_spaces_to_go[$ibeg] ) ) {
                        $ci_levels_to_go[$ibeg] = 0;
                        $leading_spaces_to_go[$ibeg] =
                          $reduced_spaces_to_go[$ibeg];
                    }
                }
            }

            # Flag any controlling opening tokens in lines without ci.  This
            # will be used later in the above if statement to undo the ci which
            # they added.  The array i_controlling_ci[$line] was prepared at
            # the top of this routine.
            if ( !$ci_levels_to_go[$ibeg]
                && defined( $i_controlling_ci[$line] ) )
            {
                foreach my $i ( @{ $i_controlling_ci[$line] } ) {
                    my $seqno = $type_sequence_to_go[$i];
                    $undo_extended_ci{$seqno} = 1;
                }
            }

            $lev_last = $lev;
        }

        return;
    }
}

{    ## begin closure set_logical_padding
    my %is_math_op;

    BEGIN {

        my @q = qw( + - * / );
        @is_math_op{@q} = (1) x scalar(@q);
    }

    sub set_logical_padding {

        # Look at a batch of lines and see if extra padding can improve the
        # alignment when there are certain leading operators. Here is an
        # example, in which some extra space is introduced before
        # '( $year' to make it line up with the subsequent lines:
        #
        #       if (   ( $Year < 1601 )
        #           || ( $Year > 2899 )
        #           || ( $EndYear < 1601 )
        #           || ( $EndYear > 2899 ) )
        #       {
        #           &Error_OutOfRange;
        #       }
        #
        my ( $self, $ri_first, $ri_last, $peak_batch_size, $starting_in_quote )
          = @_;
        my $max_line = @{$ri_first} - 1;

        my ( $ibeg, $ibeg_next, $ibegm, $iend, $iendm, $ipad, $pad_spaces,
            $tok_next, $type_next, $has_leading_op_next, $has_leading_op );

        # looking at each line of this batch..
        foreach my $line ( 0 .. $max_line - 1 ) {

            # see if the next line begins with a logical operator
            $ibeg      = $ri_first->[$line];
            $iend      = $ri_last->[$line];
            $ibeg_next = $ri_first->[ $line + 1 ];
            $tok_next  = $tokens_to_go[$ibeg_next];
            $type_next = $types_to_go[$ibeg_next];

            $has_leading_op_next = ( $tok_next =~ /^\w/ )
              ? $is_chain_operator{$tok_next}      # + - * / : ? && ||
              : $is_chain_operator{$type_next};    # and, or

            next unless ($has_leading_op_next);

            # next line must not be at lesser depth
            next
              if ( $nesting_depth_to_go[$ibeg] >
                $nesting_depth_to_go[$ibeg_next] );

            # identify the token in this line to be padded on the left
            $ipad = undef;

            # handle lines at same depth...
            if ( $nesting_depth_to_go[$ibeg] ==
                $nesting_depth_to_go[$ibeg_next] )
            {

                # if this is not first line of the batch ...
                if ( $line > 0 ) {

                    # and we have leading operator..
                    next if $has_leading_op;

                    # Introduce padding if..
                    # 1. the previous line is at lesser depth, or
                    # 2. the previous line ends in an assignment
                    # 3. the previous line ends in a 'return'
                    # 4. the previous line ends in a comma
                    # Example 1: previous line at lesser depth
                    #       if (   ( $Year < 1601 )      # <- we are here but
                    #           || ( $Year > 2899 )      #  list has not yet
                    #           || ( $EndYear < 1601 )   # collapsed vertically
                    #           || ( $EndYear > 2899 ) )
                    #       {
                    #
                    # Example 2: previous line ending in assignment:
                    #    $leapyear =
                    #        $year % 4   ? 0     # <- We are here
                    #      : $year % 100 ? 1
                    #      : $year % 400 ? 0
                    #      : 1;
                    #
                    # Example 3: previous line ending in comma:
                    #    push @expr,
                    #        /test/   ? undef
                    #      : eval($_) ? 1
                    #      : eval($_) ? 1
                    #      :            0;

                   # be sure levels agree (do not indent after an indented 'if')
                    next
                      if ( $levels_to_go[$ibeg] ne $levels_to_go[$ibeg_next] );

                    # allow padding on first line after a comma but only if:
                    # (1) this is line 2 and
                    # (2) there are at more than three lines and
                    # (3) lines 3 and 4 have the same leading operator
                    # These rules try to prevent padding within a long
                    # comma-separated list.
                    my $ok_comma;
                    if (   $types_to_go[$iendm] eq ','
                        && $line == 1
                        && $max_line > 2 )
                    {
                        my $ibeg_next_next = $ri_first->[ $line + 2 ];
                        my $tok_next_next  = $tokens_to_go[$ibeg_next_next];
                        $ok_comma = $tok_next_next eq $tok_next;
                    }

                    next
                      unless (
                           $is_assignment{ $types_to_go[$iendm] }
                        || $ok_comma
                        || ( $nesting_depth_to_go[$ibegm] <
                            $nesting_depth_to_go[$ibeg] )
                        || (   $types_to_go[$iendm] eq 'k'
                            && $tokens_to_go[$iendm] eq 'return' )
                      );

                    # we will add padding before the first token
                    $ipad = $ibeg;
                }

                # for first line of the batch..
                else {

                    # WARNING: Never indent if first line is starting in a
                    # continued quote, which would change the quote.
                    next if $starting_in_quote;

                    # if this is text after closing '}'
                    # then look for an interior token to pad
                    if ( $types_to_go[$ibeg] eq '}' ) {

                    }

                    # otherwise, we might pad if it looks really good
                    else {

                        # we might pad token $ibeg, so be sure that it
                        # is at the same depth as the next line.
                        next
                          if ( $nesting_depth_to_go[$ibeg] !=
                            $nesting_depth_to_go[$ibeg_next] );

                        # We can pad on line 1 of a statement if at least 3
                        # lines will be aligned. Otherwise, it
                        # can look very confusing.

                 # We have to be careful not to pad if there are too few
                 # lines.  The current rule is:
                 # (1) in general we require at least 3 consecutive lines
                 # with the same leading chain operator token,
                 # (2) but an exception is that we only require two lines
                 # with leading colons if there are no more lines.  For example,
                 # the first $i in the following snippet would get padding
                 # by the second rule:
                 #
                 #   $i == 1 ? ( "First", "Color" )
                 # : $i == 2 ? ( "Then",  "Rarity" )
                 # :           ( "Then",  "Name" );

                        if ( $max_line > 1 ) {
                            my $leading_token = $tokens_to_go[$ibeg_next];
                            my $tokens_differ;

                            # never indent line 1 of a '.' series because
                            # previous line is most likely at same level.
                            # TODO: we should also look at the leasing_spaces
                            # of the last output line and skip if it is same
                            # as this line.
                            next if ( $leading_token eq '.' );

                            my $count = 1;
                            foreach my $l ( 2 .. 3 ) {
                                last if ( $line + $l > $max_line );
                                my $ibeg_next_next = $ri_first->[ $line + $l ];
                                if ( $tokens_to_go[$ibeg_next_next] ne
                                    $leading_token )
                                {
                                    $tokens_differ = 1;
                                    last;
                                }
                                $count++;
                            }
                            next if ($tokens_differ);
                            next if ( $count < 3 && $leading_token ne ':' );
                            $ipad = $ibeg;
                        }
                        else {
                            next;
                        }
                    }
                }
            }

            # find interior token to pad if necessary
            if ( !defined($ipad) ) {

                for ( my $i = $ibeg ; ( $i < $iend ) && !$ipad ; $i++ ) {

                    # find any unclosed container
                    next
                      unless ( $type_sequence_to_go[$i]
                        && $mate_index_to_go[$i] > $iend );

                    # find next nonblank token to pad
                    $ipad = $inext_to_go[$i];
                    last if ( $ipad > $iend );
                }
                last unless $ipad;
            }

            # We cannot pad the first leading token of a file because
            # it could cause a bug in which the starting indentation
            # level is guessed incorrectly each time the code is run
            # though perltidy, thus causing the code to march off to
            # the right.  For example, the following snippet would have
            # this problem:

##     ov_method mycan( $package, '(""' ),       $package
##  or ov_method mycan( $package, '(0+' ),       $package
##  or ov_method mycan( $package, '(bool' ),     $package
##  or ov_method mycan( $package, '(nomethod' ), $package;

            # If this snippet is within a block this won't happen
            # unless the user just processes the snippet alone within
            # an editor.  In that case either the user will see and
            # fix the problem or it will be corrected next time the
            # entire file is processed with perltidy.
            next if ( $ipad == 0 && $peak_batch_size <= 1 );

## THIS PATCH REMOVES THE FOLLOWING POOR PADDING (math.t) with -pbp, BUT
## IT DID MORE HARM THAN GOOD
##            ceil(
##                      $font->{'loca'}->{'glyphs'}[$x]->read->{'xMin'} * 1000
##                    / $upem
##            ),
##?            # do not put leading padding for just 2 lines of math
##?            if (   $ipad == $ibeg
##?                && $line > 0
##?                && $levels_to_go[$ipad] > $levels_to_go[ $ipad - 1 ]
##?                && $is_math_op{$type_next}
##?                && $line + 2 <= $max_line )
##?            {
##?                my $ibeg_next_next = $ri_first->[ $line + 2 ];
##?                my $type_next_next = $types_to_go[$ibeg_next_next];
##?                next if !$is_math_op{$type_next_next};
##?            }

            # next line must not be at greater depth
            my $iend_next = $ri_last->[ $line + 1 ];
            next
              if ( $nesting_depth_to_go[ $iend_next + 1 ] >
                $nesting_depth_to_go[$ipad] );

            # lines must be somewhat similar to be padded..
            my $inext_next = $inext_to_go[$ibeg_next];
            my $type       = $types_to_go[$ipad];
            my $type_next  = $types_to_go[ $ipad + 1 ];

            # see if there are multiple continuation lines
            my $logical_continuation_lines = 1;
            if ( $line + 2 <= $max_line ) {
                my $leading_token  = $tokens_to_go[$ibeg_next];
                my $ibeg_next_next = $ri_first->[ $line + 2 ];
                if (   $tokens_to_go[$ibeg_next_next] eq $leading_token
                    && $nesting_depth_to_go[$ibeg_next] eq
                    $nesting_depth_to_go[$ibeg_next_next] )
                {
                    $logical_continuation_lines++;
                }
            }

            # see if leading types match
            my $types_match = $types_to_go[$inext_next] eq $type;
            my $matches_without_bang;

            # if first line has leading ! then compare the following token
            if ( !$types_match && $type eq '!' ) {
                $types_match = $matches_without_bang =
                  $types_to_go[$inext_next] eq $types_to_go[ $ipad + 1 ];
            }

            if (

                # either we have multiple continuation lines to follow
                # and we are not padding the first token
                ( $logical_continuation_lines > 1 && $ipad > 0 )

                # or..
                || (

                    # types must match
                    $types_match

                    # and keywords must match if keyword
                    && !(
                           $type eq 'k'
                        && $tokens_to_go[$ipad] ne $tokens_to_go[$inext_next]
                    )
                )
              )
            {

                #----------------------begin special checks--------------
                #
                # SPECIAL CHECK 1:
                # A check is needed before we can make the pad.
                # If we are in a list with some long items, we want each
                # item to stand out.  So in the following example, the
                # first line beginning with '$casefold->' would look good
                # padded to align with the next line, but then it
                # would be indented more than the last line, so we
                # won't do it.
                #
                #  ok(
                #      $casefold->{code}         eq '0041'
                #        && $casefold->{status}  eq 'C'
                #        && $casefold->{mapping} eq '0061',
                #      'casefold 0x41'
                #  );
                #
                # Note:
                # It would be faster, and almost as good, to use a comma
                # count, and not pad if comma_count > 1 and the previous
                # line did not end with a comma.
                #
                my $ok_to_pad = 1;

                my $ibg   = $ri_first->[ $line + 1 ];
                my $depth = $nesting_depth_to_go[ $ibg + 1 ];

                # just use simplified formula for leading spaces to avoid
                # needless sub calls
                my $lsp = $levels_to_go[$ibg] + $ci_levels_to_go[$ibg];

                # look at each line beyond the next ..
                my $l = $line + 1;
                foreach my $ltest ( $line + 2 .. $max_line ) {
                    $l = $ltest;
                    my $ibg = $ri_first->[$l];

                    # quit looking at the end of this container
                    last
                      if ( $nesting_depth_to_go[ $ibg + 1 ] < $depth )
                      || ( $nesting_depth_to_go[$ibg] < $depth );

                    # cannot do the pad if a later line would be
                    # outdented more
                    if ( $levels_to_go[$ibg] + $ci_levels_to_go[$ibg] < $lsp ) {
                        $ok_to_pad = 0;
                        last;
                    }
                }

                # don't pad if we end in a broken list
                if ( $l == $max_line ) {
                    my $i2 = $ri_last->[$l];
                    if ( $types_to_go[$i2] eq '#' ) {
                        my $i1 = $ri_first->[$l];
                        next if terminal_type_i( $i1, $i2 ) eq ',';
                    }
                }

                # SPECIAL CHECK 2:
                # a minus may introduce a quoted variable, and we will
                # add the pad only if this line begins with a bare word,
                # such as for the word 'Button' here:
                #    [
                #         Button      => "Print letter \"~$_\"",
                #        -command     => [ sub { print "$_[0]\n" }, $_ ],
                #        -accelerator => "Meta+$_"
                #    ];
                #
                #  On the other hand, if 'Button' is quoted, it looks best
                #  not to pad:
                #    [
                #        'Button'     => "Print letter \"~$_\"",
                #        -command     => [ sub { print "$_[0]\n" }, $_ ],
                #        -accelerator => "Meta+$_"
                #    ];
                if ( $types_to_go[$ibeg_next] eq 'm' ) {
                    $ok_to_pad = 0 if $types_to_go[$ibeg] eq 'Q';
                }

                next unless $ok_to_pad;

                #----------------------end special check---------------

                my $length_1 = total_line_length( $ibeg,      $ipad - 1 );
                my $length_2 = total_line_length( $ibeg_next, $inext_next - 1 );
                $pad_spaces = $length_2 - $length_1;

                # If the first line has a leading ! and the second does
                # not, then remove one space to try to align the next
                # leading characters, which are often the same.  For example:
                #  if (  !$ts
                #      || $ts == $self->Holder
                #      || $self->Holder->Type eq "Arena" )
                #
                # This usually helps readability, but if there are subsequent
                # ! operators things will still get messed up.  For example:
                #
                #  if (  !exists $Net::DNS::typesbyname{$qtype}
                #      && exists $Net::DNS::classesbyname{$qtype}
                #      && !exists $Net::DNS::classesbyname{$qclass}
                #      && exists $Net::DNS::typesbyname{$qclass} )
                # We can't fix that.
                if ($matches_without_bang) { $pad_spaces-- }

                # make sure this won't change if -lp is used
                my $indentation_1 = $leading_spaces_to_go[$ibeg];
                if ( ref($indentation_1) ) {
                    if ( $indentation_1->get_recoverable_spaces() == 0 ) {
                        my $indentation_2 = $leading_spaces_to_go[$ibeg_next];
                        unless ( $indentation_2->get_recoverable_spaces() == 0 )
                        {
                            $pad_spaces = 0;
                        }
                    }
                }

                # we might be able to handle a pad of -1 by removing a blank
                # token
                if ( $pad_spaces < 0 ) {

                    # Deactivated for -kpit due to conflict. This block deletes
                    # a space in an attempt to improve alignment in some cases,
                    # but it may conflict with user spacing requests.  For now
                    # it is just deactivated if the -kpit option is used.
                    if ( $pad_spaces == -1 ) {
                        if (   $ipad > $ibeg
                            && $types_to_go[ $ipad - 1 ] eq 'b'
                            && !%keyword_paren_inner_tightness )
                        {
                            $self->pad_token( $ipad - 1, $pad_spaces );
                        }
                    }
                    $pad_spaces = 0;
                }

                # now apply any padding for alignment
                if ( $ipad >= 0 && $pad_spaces ) {

                    my $length_t = total_line_length( $ibeg, $iend );
                    if ( $pad_spaces + $length_t <=
                        $maximum_line_length[ $levels_to_go[$ibeg] ] )
                    {
                        $self->pad_token( $ipad, $pad_spaces );
                    }
                }
            }
        }
        continue {
            $iendm          = $iend;
            $ibegm          = $ibeg;
            $has_leading_op = $has_leading_op_next;
        }    # end of loop over lines
        return;
    }
} ## end closure set_logical_padding

sub pad_token {

    # insert $pad_spaces before token number $ipad
    my ( $self, $ipad, $pad_spaces ) = @_;
    my $rLL     = $self->[_rLL_];
    my $KK      = $K_to_go[$ipad];
    my $tok     = $rLL->[$KK]->[_TOKEN_];
    my $tok_len = $rLL->[$KK]->[_TOKEN_LENGTH_];

    if ( $pad_spaces > 0 ) {
        $tok = ' ' x $pad_spaces . $tok;
        $tok_len += $pad_spaces;
    }
    elsif ( $pad_spaces == -1 && $tokens_to_go[$ipad] eq ' ' ) {
        $tok     = "";
        $tok_len = 0;
    }
    else {

        # shouldn't happen
        return;
    }

    $tok     = $rLL->[$KK]->[_TOKEN_]        = $tok;
    $tok_len = $rLL->[$KK]->[_TOKEN_LENGTH_] = $tok_len;

    $token_lengths_to_go[$ipad] += $pad_spaces;
    $tokens_to_go[$ipad] = $tok;

    foreach my $i ( $ipad .. $max_index_to_go ) {
        $summed_lengths_to_go[ $i + 1 ] += $pad_spaces;
    }
    return;
}

{    ## begin closure make_alignment_patterns

    my %block_type_map;
    my %keyword_map;
    my %operator_map;
    my %is_w_n_C;

    BEGIN {

        # map related block names into a common name to
        # allow alignment
        %block_type_map = (
            'unless'  => 'if',
            'else'    => 'if',
            'elsif'   => 'if',
            'when'    => 'if',
            'default' => 'if',
            'case'    => 'if',
            'sort'    => 'map',
            'grep'    => 'map',
        );

        # map certain keywords to the same 'if' class to align
        # long if/elsif sequences. [elsif.pl]
        %keyword_map = (
            'unless'  => 'if',
            'else'    => 'if',
            'elsif'   => 'if',
            'when'    => 'given',
            'default' => 'given',
            'case'    => 'switch',

            # treat an 'undef' similar to numbers and quotes
            'undef' => 'Q',
        );

        # map certain operators to the same class for pattern matching
        %operator_map = (
            '!~' => '=~',
            '+=' => '+=',
            '-=' => '+=',
            '*=' => '+=',
            '/=' => '+=',
        );

        %is_w_n_C = (
            'w' => 1,
            'n' => 1,
            'C' => 1,
        );
    }

    sub delete_needless_alignments {
        my ( $self, $ibeg, $iend, $ralignment_type_to_go ) = @_;

        # Remove unwanted alignments.  This routine is a place to remove
        # alignments which might cause problems at later stages.  There are
        # currently two types of fixes:

        # 1. Remove excess parens
        # 2. Remove alignments within 'elsif' conditions

        # Patch #1: Excess alignment of parens can prevent other good
        # alignments.  For example, note the parens in the first two rows of
        # the following snippet.  They would normally get marked for alignment
        # and aligned as follows:

        #    my $w = $columns * $cell_w + ( $columns + 1 ) * $border;
        #    my $h = $rows * $cell_h +    ( $rows + 1 ) * $border;
        #    my $img = new Gimp::Image( $w, $h, RGB );

        # This causes unnecessary paren alignment and prevents the third equals
        # from aligning. If we remove the unwanted alignments we get:

        #    my $w   = $columns * $cell_w + ( $columns + 1 ) * $border;
        #    my $h   = $rows * $cell_h + ( $rows + 1 ) * $border;
        #    my $img = new Gimp::Image( $w, $h, RGB );

        # A rule for doing this which works well is to remove alignment of
        # parens whose containers do not contain other aligning tokens, with
        # the exception that we always keep alignment of the first opening
        # paren on a line (for things like 'if' and 'elsif' statements).

        # Setup needed constants
        my $i_good_paren  = -1;
        my $imin_match    = $iend + 1;
        my $i_elsif_close = $ibeg - 1;
        my $i_elsif_open  = $iend + 1;
        if ( $iend > $ibeg ) {
            if ( $types_to_go[$ibeg] eq 'k' ) {

                # Paren patch: mark a location of a paren we should keep, such
                # as one following something like a leading 'if', 'elsif',..
                $i_good_paren = $ibeg + 1;
                if ( $types_to_go[$i_good_paren] eq 'b' ) {
                    $i_good_paren++;
                }

                # 'elsif' patch: remember the range of the parens of an elsif,
                # and do not make alignments within them because this can cause
                # loss of padding and overall brace alignment in the vertical
                # aligner.
                if (   $tokens_to_go[$ibeg] eq 'elsif'
                    && $i_good_paren < $iend
                    && $tokens_to_go[$i_good_paren] eq '(' )
                {
                    $i_elsif_open  = $i_good_paren;
                    $i_elsif_close = $mate_index_to_go[$i_good_paren];
                }
            }
        }

        # Loop to make the fixes on this line
        my @imatch_list;
        for my $i ( $ibeg .. $iend ) {

            if ( $ralignment_type_to_go->[$i] ) {

                # Patch #2: undo alignment within elsif parens
                if ( $i > $i_elsif_open && $i < $i_elsif_close ) {
                    $ralignment_type_to_go->[$i] = '';
                    next;
                }
                push @imatch_list, $i;

            }
            if ( $tokens_to_go[$i] eq ')' ) {

                # Patch #1: undo the corresponding opening paren if:
                # - it is at the top of the stack
                # - and not the first overall opening paren
                # - does not follow a leading keyword on this line
                my $imate = $mate_index_to_go[$i];
                if (   @imatch_list
                    && $imatch_list[-1] eq $imate
                    && ( $ibeg > 1 || @imatch_list > 1 )
                    && $imate > $i_good_paren )
                {
                    $ralignment_type_to_go->[$imate] = '';
                    pop @imatch_list;
                }
            }
        }
        return;
    }

    sub make_alignment_patterns {

        # Here we do some important preliminary work for the
        # vertical aligner.  We create three arrays for one
        # output line. These arrays contain strings that can
        # be tested by the vertical aligner to see if
        # consecutive lines can be aligned vertically.
        #
        # The three arrays are indexed on the vertical
        # alignment fields and are:
        # @tokens - a list of any vertical alignment tokens for this line.
        #   These are tokens, such as '=' '&&' '#' etc which
        #   we want to might align vertically.  These are
        #   decorated with various information such as
        #   nesting depth to prevent unwanted vertical
        #   alignment matches.
        # @fields - the actual text of the line between the vertical alignment
        #   tokens.
        # @patterns - a modified list of token types, one for each alignment
        #   field.  These should normally each match before alignment is
        #   allowed, even when the alignment tokens match.
        my ( $self, $ibeg, $iend, $ralignment_type_to_go ) = @_;
        my @tokens        = ();
        my @fields        = ();
        my @patterns      = ();
        my @field_lengths = ();
        my $i_start       = $ibeg;

        my $depth          = 0;
        my %container_name = ( 0 => "" );

        my $j = 0;    # field index

        $patterns[0] = "";
        my %token_count;
        for my $i ( $ibeg .. $iend ) {

            # Keep track of containers balanced on this line only.
            # These are used below to prevent unwanted cross-line alignments.
            # Unbalanced containers already avoid aligning across
            # container boundaries.

            my $type       = $types_to_go[$i];
            my $token      = $tokens_to_go[$i];
            my $depth_last = $depth;
            if ( $type_sequence_to_go[$i] ) {
                if ( $is_opening_type{$token} ) {

                    # if container is balanced on this line...
                    my $i_mate = $mate_index_to_go[$i];
                    if ( $i_mate > $i && $i_mate <= $iend ) {
                        $depth++;

                     # Append the previous token name to make the container name
                     # more unique.  This name will also be given to any commas
                     # within this container, and it helps avoid undesirable
                     # alignments of different types of containers.

                     # Containers beginning with { and [ are given those names
                     # for uniqueness. That way commas in different containers
                     # will not match. Here is an example of what this prevents:
                     #	a => [ 1,       2, 3 ],
                     #   b => { b1 => 4, b2 => 5 },
                     # Here is another example of what we avoid by labeling the
                     # commas properly:

                   # is_d( [ $a,        $a ], [ $b,               $c ] );
                   # is_d( { foo => $a, bar => $a }, { foo => $b, bar => $c } );
                   # is_d( [ \$a,       \$a ], [ \$b,             \$c ] );

                        my $name = $token;
                        if ( $token eq '(' ) {
                            $name = $self->make_paren_name($i);
                        }
                        $container_name{$depth} = "+" . $name;

                       # Make the container name even more unique if necessary.
                       # If we are not vertically aligning this opening paren,
                       # append a character count to avoid bad alignment because
                       # it usually looks bad to align commas within containers
                       # for which the opening parens do not align.  Here
                       # is an example very BAD alignment of commas (because
                       # the atan2 functions are not all aligned):
                       #    $XY =
                       #      $X * $RTYSQP1 * atan2( $X, $RTYSQP1 ) +
                       #      $Y * $RTXSQP1 * atan2( $Y, $RTXSQP1 ) -
                       #      $X * atan2( $X,            1 ) -
                       #      $Y * atan2( $Y,            1 );
                       #
                       # On the other hand, it is usually okay to align commas
                       # if opening parens align, such as:
                       #    glVertex3d( $cx + $s * $xs, $cy,            $z );
                       #    glVertex3d( $cx,            $cy + $s * $ys, $z );
                       #    glVertex3d( $cx - $s * $xs, $cy,            $z );
                       #    glVertex3d( $cx,            $cy - $s * $ys, $z );
                       #
                       # To distinguish between these situations, we will append
                       # the length of the line from the previous matching
                       # token, or beginning of line, to the function name.
                       # This will allow the vertical aligner to reject
                       # undesirable matches.

                        # if we are not aligning on this paren...
                        if ( !$ralignment_type_to_go->[$i] ) {

                            # Sum length from previous alignment
                            my $len = token_sequence_length( $i_start, $i - 1 );

                            # Minor patch: do not include the length of any '!'.
                            # Otherwise, commas in the following line will not
                            # match
                            #  ok( 20, tapprox( ( pdl 2,  3 ), ( pdl 2, 3 ) ) );
                            #  ok( 21, !tapprox( ( pdl 2, 3 ), ( pdl 2, 4 ) ) );
                            if ( grep { $_ eq '!' }
                                @types_to_go[ $i_start .. $i - 1 ] )
                            {
                                $len -= 1;
                            }

                            if ( $i_start == $ibeg ) {

                              # For first token, use distance from start of line
                              # but subtract off the indentation due to level.
                              # Otherwise, results could vary with indentation.
                                $len +=
                                  leading_spaces_to_go($ibeg) -
                                  $levels_to_go[$i_start] *
                                  $rOpts_indent_columns;
                                if ( $len < 0 ) { $len = 0 }
                            }

                            # tack this length onto the container name to try
                            # to make a unique token name
                            $container_name{$depth} .= "-" . $len;
                        }
                    }
                }
                elsif ( $is_closing_type{$token} ) {
                    $depth-- if $depth > 0;
                }
            }

            # if we find a new synchronization token, we are done with
            # a field
            if ( $i > $i_start && $ralignment_type_to_go->[$i] ) {

                my $tok = my $raw_tok = $ralignment_type_to_go->[$i];

                # map similar items
                my $tok_map = $operator_map{$tok};
                $tok = $tok_map if ($tok_map);

                # make separators in different nesting depths unique
                # by appending the nesting depth digit.
                if ( $raw_tok ne '#' ) {
                    $tok .= "$nesting_depth_to_go[$i]";
                }

                # also decorate commas with any container name to avoid
                # unwanted cross-line alignments.
                if ( $raw_tok eq ',' || $raw_tok eq '=>' ) {

                  # If we are at an opening token which increased depth, we have
                  # to use the name from the previous depth.
                    my $depth_p =
                      ( $depth_last < $depth ? $depth_last : $depth );
                    if ( $container_name{$depth_p} ) {
                        $tok .= $container_name{$depth_p};
                    }
                }

                # Patch to avoid aligning leading and trailing if, unless.
                # Mark trailing if, unless statements with container names.
                # This makes them different from leading if, unless which
                # are not so marked at present.  If we ever need to name
                # them too, we could use ci to distinguish them.
                # Example problem to avoid:
                #    return ( 2, "DBERROR" )
                #      if ( $retval == 2 );
                #    if   ( scalar @_ ) {
                #        my ( $a, $b, $c, $d, $e, $f ) = @_;
                #    }
                if ( $raw_tok eq '(' ) {
                    if (   $ci_levels_to_go[$ibeg]
                        && $container_name{$depth} =~ /^\+(if|unless)/ )
                    {
                        $tok .= $container_name{$depth};
                    }
                }

                # Decorate block braces with block types to avoid
                # unwanted alignments such as the following:
                # foreach ( @{$routput_array} ) { $fh->print($_) }
                # eval                          { $fh->close() };
                if ( $raw_tok eq '{' && $block_type_to_go[$i] ) {
                    my $block_type = $block_type_to_go[$i];

                    # map certain related block types to allow
                    # else blocks to align
                    $block_type = $block_type_map{$block_type}
                      if ( defined( $block_type_map{$block_type} ) );

                    # remove sub names to allow one-line sub braces to align
                    # regardless of name
                    if ( $block_type =~ /$SUB_PATTERN/ ) { $block_type = 'sub' }

                    # allow all control-type blocks to align
                    if ( $block_type =~ /^[A-Z]+$/ ) { $block_type = 'BEGIN' }

                    $tok .= $block_type;
                }

                # Mark multiple copies of certain tokens with the copy number
                # This will allow the aligner to decide if they are matched.
                # For now, only do this for equals. For example, the two
                # equals on the next line will be labeled '=0' and '=0.2'.
                # Later, the '=0.2' will be ignored in alignment because it
                # has no match.

                # $|          = $debug = 1 if $opt_d;
                # $full_index = 1          if $opt_i;

                if ( $raw_tok eq '=' || $raw_tok eq '=>' ) {
                    $token_count{$tok}++;
                    if ( $token_count{$tok} > 1 ) {
                        $tok .= '.' . $token_count{$tok};
                    }
                }

                # concatenate the text of the consecutive tokens to form
                # the field
                push( @fields,
                    join( '', @tokens_to_go[ $i_start .. $i - 1 ] ) );

                push @field_lengths,
                  $summed_lengths_to_go[$i] - $summed_lengths_to_go[$i_start];

                # store the alignment token for this field
                push( @tokens, $tok );

                # get ready for the next batch
                $i_start = $i;
                $j++;
                $patterns[$j] = "";
            }

            # continue accumulating tokens

            # for keywords we have to use the actual text
            if ( $type eq 'k' ) {

                my $tok_fix = $tokens_to_go[$i];

                # but map certain keywords to a common string to allow
                # alignment.
                $tok_fix = $keyword_map{$tok_fix}
                  if ( defined( $keyword_map{$tok_fix} ) );
                $patterns[$j] .= $tok_fix;
            }

            elsif ( $type eq 'b' ) {
                $patterns[$j] .= $type;
            }

            # handle non-keywords..
            else {

                my $type_fix = $type;

                # Mark most things before arrows as a quote to
                # get them to line up. Testfile: mixed.pl.
                #                      $type =~ /^[wnC]$/
                if ( $i < $iend - 1 && $is_w_n_C{$type} ) {
                    my $next_type = $types_to_go[ $i + 1 ];
                    my $i_next_nonblank =
                      ( ( $next_type eq 'b' ) ? $i + 2 : $i + 1 );

                    if ( $types_to_go[$i_next_nonblank] eq '=>' ) {
                        $type_fix = 'Q';

                        # Patch to ignore leading minus before words,
                        # by changing pattern 'mQ' into just 'Q',
                        # so that we can align things like this:
                        #  Button   => "Print letter \"~$_\"",
                        #  -command => [ sub { print "$_[0]\n" }, $_ ],
                        if ( $patterns[$j] eq 'm' ) { $patterns[$j] = "" }
                    }
                }

                # Convert a bareword within braces into a quote for matching.
                # This will allow alignment of expressions like this:
                #    local ( $SIG{'INT'} ) = IGNORE;
                #    local ( $SIG{ALRM} )  = 'POSTMAN';
                if (   $type eq 'w'
                    && $i > $ibeg
                    && $i < $iend
                    && $types_to_go[ $i - 1 ] eq 'L'
                    && $types_to_go[ $i + 1 ] eq 'R' )
                {
                    $type_fix = 'Q';
                }

                # patch to make numbers and quotes align
                if ( $type eq 'n' ) { $type_fix = 'Q' }

                # patch to ignore any ! in patterns
                if ( $type eq '!' ) { $type_fix = '' }

                $patterns[$j] .= $type_fix;
            }
        }

        # done with this line .. join text of tokens to make the last field
        push( @fields, join( '', @tokens_to_go[ $i_start .. $iend ] ) );
        push @field_lengths,
          $summed_lengths_to_go[ $iend + 1 ] - $summed_lengths_to_go[$i_start];

        return ( \@tokens, \@fields, \@patterns, \@field_lengths );
    }

} ## end closure make_alignment_patterns

sub make_paren_name {
    my ( $self, $i ) = @_;

    # The token at index $i is a '('.
    # Create an alignment name for it to avoid incorrect alignments.

    # Start with the name of the previous nonblank token...
    my $name = "";
    my $im   = $i - 1;
    return "" if ( $im < 0 );
    if ( $types_to_go[$im] eq 'b' ) { $im--; }
    return "" if ( $im < 0 );
    $name = $tokens_to_go[$im];

    # Prepend any sub name to an isolated -> to avoid unwanted alignments
    # [test case is test8/penco.pl]
    if ( $name eq '->' ) {
        $im--;
        if ( $im >= 0 && $types_to_go[$im] ne 'b' ) {
            $name = $tokens_to_go[$im] . $name;
        }
    }

    # Finally, remove any leading arrows
    if ( substr( $name, 0, 2 ) eq '->' ) {
        $name = substr( $name, 2 );
    }
    return $name;
}

{    ## begin closure set_adjusted_indentation

    my ( $last_indentation_written, $last_unadjusted_indentation,
        $last_leading_token );

    sub initialize_adjusted_indentation {
        $last_indentation_written    = 0;
        $last_unadjusted_indentation = 0;
        $last_leading_token          = "";
        return;
    }

    sub set_adjusted_indentation {

        # This routine has the final say regarding the actual indentation of
        # a line.  It starts with the basic indentation which has been
        # defined for the leading token, and then takes into account any
        # options that the user has set regarding special indenting and
        # outdenting.

        # This routine has to resolve a number of complex interacting issues,
        # including:
        # 1. The various -cti=n type flags, which contain the desired change in
        #    indentation for lines ending in commas and semicolons, should be
        #    followed,
        # 2. qw quotes require special processing and do not fit perfectly
        #    with normal containers,
        # 3. formatting with -wn can complicate things, especially with qw
        #    quotes,
        # 4. formatting with the -lp option is complicated, and does not
        #    work well with qw quotes and with -wn formatting.
        # 5. a number of special situations, such as 'cuddled' formatting.
        # 6. This routine is mainly concerned with outdenting closing tokens
        #    but note that there is some overlap with the functions of sub
        #    undo_ci, which was processed earlier, so care has to be taken to
        #    keep them coordinated.

        my (
            $self,       $ibeg,
            $iend,       $rfields,
            $rpatterns,  $ri_first,
            $ri_last,    $rindentation_list,
            $level_jump, $starting_in_quote,
            $is_static_block_comment,
        ) = @_;

        my $rLL                      = $self->[_rLL_];
        my $ris_bli_container        = $self->[_ris_bli_container_];
        my $rseqno_controlling_my_ci = $self->[_rseqno_controlling_my_ci_];

        # we need to know the last token of this line
        my ( $terminal_type, $i_terminal ) = terminal_type_i( $ibeg, $iend );

        my $terminal_block_type = $block_type_to_go[$i_terminal];
        my $is_outdented_line   = 0;

        my $type_beg      = $types_to_go[$ibeg];
        my $token_beg     = $tokens_to_go[$ibeg];
        my $K_beg         = $K_to_go[$ibeg];
        my $ibeg_weld_fix = $ibeg;
        my $seqno_beg     = $type_sequence_to_go[$ibeg];
        my $is_bli_beg    = $seqno_beg ? $ris_bli_container->{$seqno_beg} : 0;

        # QW INDENTATION PATCH 3:
        my $seqno_qw_closing;
        if ( $type_beg eq 'q' && $ibeg == 0 ) {
            my $KK = $K_to_go[$ibeg];
            $seqno_qw_closing =
              $self->[_rending_multiline_qw_seqno_by_K_]->{$KK};
        }

        my $is_semicolon_terminated = $terminal_type eq ';'
          && ( $nesting_depth_to_go[$iend] < $nesting_depth_to_go[$ibeg]
            || $seqno_qw_closing );

        # NOTE: A future improvement would be to make it semicolon terminated
        # even if it does not have a semicolon but is followed by a closing
        # block brace. This would undo ci even for something like the
        # following, in which the final paren does not have a semicolon because
        # it is a possible weld location:

        # if ($BOLD_MATH) {
        #     (
        #         $labels, $comment,
        #         join( '', '<B>', &make_math( $mode, '', '', $_ ), '</B>' )
        #     )
        # }
        #

        # MOJO: Set a flag if this lines begins with ')->'
        my $leading_paren_arrow = (
                 $types_to_go[$ibeg] eq '}'
              && $tokens_to_go[$ibeg] eq ')'
              && (
                ( $ibeg < $i_terminal && $types_to_go[ $ibeg + 1 ] eq '->' )
                || (   $ibeg < $i_terminal - 1
                    && $types_to_go[ $ibeg + 1 ] eq 'b'
                    && $types_to_go[ $ibeg + 2 ] eq '->' )
              )
        );

        ##########################################################
        # Section 1: set a flag and a default indentation
        #
        # Most lines are indented according to the initial token.
        # But it is common to outdent to the level just after the
        # terminal token in certain cases...
        # adjust_indentation flag:
        #       0 - do not adjust
        #       1 - outdent
        #       2 - vertically align with opening token
        #       3 - indent
        ##########################################################
        my $adjust_indentation         = 0;
        my $default_adjust_indentation = $adjust_indentation;

        my (
            $opening_indentation, $opening_offset,
            $is_leading,          $opening_exists
        );

        # Update the $is_bli flag as we go. It is initially 1.
        # We note seeing a leading opening brace by setting it to 2.
        # If we get to the closing brace without seeing the opening then we
        # turn it off.  This occurs if the opening brace did not get output
        # at the start of a line, so we will then indent the closing brace
        # in the default way.
        if ( $is_bli_beg && $is_bli_beg == 1 ) {
            my $K_opening_container = $self->[_K_opening_container_];
            my $K_opening           = $K_opening_container->{$seqno_beg};
            if ( $K_beg eq $K_opening ) {
                $ris_bli_container->{$seqno_beg} = $is_bli_beg = 2;
            }
            else { $is_bli_beg = 0 }
        }

        # QW PATCH for the combination -lp -wn
        # For -lp formatting use $ibeg_weld_fix to get around the problem
        # that with -lp type formatting the opening and closing tokens to not
        # have sequence numbers.
        if ($seqno_qw_closing) {
            my $K_next_nonblank = $self->K_next_code($K_beg);
            if ( defined($K_next_nonblank) ) {
                my $type_sequence = $rLL->[$K_next_nonblank]->[_TYPE_SEQUENCE_];
                my $token         = $rLL->[$K_next_nonblank]->[_TOKEN_];
                my $welded = $self->weld_len_left( $type_sequence, $token );
                if ($welded) {
                    my $itest = $ibeg + ( $K_next_nonblank - $K_beg );
                    if ( $itest <= $max_index_to_go ) {
                        $ibeg_weld_fix = $itest;
                    }
                }
            }
        }

        # if we are at a closing token of some type..
        if ( $is_closing_type{$type_beg} || $seqno_qw_closing ) {

            # get the indentation of the line containing the corresponding
            # opening token
            (
                $opening_indentation, $opening_offset,
                $is_leading,          $opening_exists
              )
              = $self->get_opening_indentation( $ibeg_weld_fix, $ri_first,
                $ri_last, $rindentation_list, $seqno_qw_closing );

            # First set the default behavior:
            if (

                # default behavior is to outdent closing lines
                # of the form:   ");  };  ];  )->xxx;"
                $is_semicolon_terminated

                # and 'cuddled parens' of the form:   ")->pack("
                # Bug fix for RT #123749]: the types here were
                # incorrectly '(' and ')'.  Corrected to be '{' and '}'
                || (
                       $terminal_type eq '{'
                    && $type_beg eq '}'
                    && ( $nesting_depth_to_go[$iend] + 1 ==
                        $nesting_depth_to_go[$ibeg] )
                )

                # remove continuation indentation for any line like
                # 	} ... {
                # or without ending '{' and unbalanced, such as
                #       such as '}->{$operator}'
                || (
                    $type_beg eq '}'

                    && (   $types_to_go[$iend] eq '{'
                        || $levels_to_go[$iend] < $levels_to_go[$ibeg] )
                )

                # and when the next line is at a lower indentation level...

                # PATCH #1: and only if the style allows undoing continuation
                # for all closing token types. We should really wait until
                # the indentation of the next line is known and then make
                # a decision, but that would require another pass.

                # PATCH #2: and not if this token is under -xci control
                || (   $level_jump < 0
                    && !$some_closing_token_indentation
                    && !$rseqno_controlling_my_ci->{$K_beg} )

                # Patch for -wn=2, multiple welded closing tokens
                || (   $i_terminal > $ibeg
                    && $is_closing_type{ $types_to_go[$iend] } )

              )
            {
                $adjust_indentation = 1;
            }

            # outdent something like '),'
            if (
                $terminal_type eq ','

                # Removed this constraint for -wn
                # OLD: allow just one character before the comma
                # && $i_terminal == $ibeg + 1

                # require LIST environment; otherwise, we may outdent too much -
                # this can happen in calls without parentheses (overload.t);
                && $container_environment_to_go[$i_terminal] eq 'LIST'
              )
            {
                $adjust_indentation = 1;
            }

            # undo continuation indentation of a terminal closing token if
            # it is the last token before a level decrease.  This will allow
            # a closing token to line up with its opening counterpart, and
            # avoids an indentation jump larger than 1 level.
            if (   $types_to_go[$i_terminal] =~ /^[\}\]\)R]$/
                && $i_terminal == $ibeg
                && defined($K_beg) )
            {
                my $K_next_nonblank = $self->K_next_code($K_beg);

                if ( !$is_bli_beg && defined($K_next_nonblank) ) {
                    my $lev        = $rLL->[$K_beg]->[_LEVEL_];
                    my $level_next = $rLL->[$K_next_nonblank]->[_LEVEL_];

                    # and do not undo ci if it was set by the -xci option
                    $adjust_indentation = 1
                      if ( $level_next < $lev
                        && !$rseqno_controlling_my_ci->{$K_beg} );
                }

                # Patch for RT #96101, in which closing brace of anonymous subs
                # was not outdented.  We should look ahead and see if there is
                # a level decrease at the next token (i.e., a closing token),
                # but right now we do not have that information.  For now
                # we see if we are in a list, and this works well.
                # See test files 'sub*.t' for good test cases.
                if (   $block_type_to_go[$ibeg] =~ /$ASUB_PATTERN/
                    && $container_environment_to_go[$i_terminal] eq 'LIST'
                    && !$rOpts->{'indent-closing-brace'} )
                {
                    (
                        $opening_indentation, $opening_offset,
                        $is_leading,          $opening_exists
                      )
                      = $self->get_opening_indentation( $ibeg, $ri_first,
                        $ri_last, $rindentation_list );
                    my $indentation = $leading_spaces_to_go[$ibeg];
                    if ( defined($opening_indentation)
                        && get_spaces($indentation) >
                        get_spaces($opening_indentation) )
                    {
                        $adjust_indentation = 1;
                    }
                }
            }

            # YVES patch 1 of 2:
            # Undo ci of line with leading closing eval brace,
            # but not beyond the indention of the line with
            # the opening brace.
            if (   $block_type_to_go[$ibeg] eq 'eval'
                && !$rOpts->{'line-up-parentheses'}
                && !$rOpts->{'indent-closing-brace'} )
            {
                (
                    $opening_indentation, $opening_offset,
                    $is_leading,          $opening_exists
                  )
                  = $self->get_opening_indentation( $ibeg, $ri_first, $ri_last,
                    $rindentation_list );
                my $indentation = $leading_spaces_to_go[$ibeg];
                if ( defined($opening_indentation)
                    && get_spaces($indentation) >
                    get_spaces($opening_indentation) )
                {
                    $adjust_indentation = 1;
                }
            }

            # patch for issue git #40: -bli setting has priority
            $adjust_indentation = 0 if ($is_bli_beg);

            $default_adjust_indentation = $adjust_indentation;

            # Now modify default behavior according to user request:
            # handle option to indent non-blocks of the form );  };  ];
            # But don't do special indentation to something like ')->pack('
            if ( !$block_type_to_go[$ibeg] ) {

                # Note that logical padding has already been applied, so we may
                # need to remove some spaces to get a valid hash key.
                my $tok = $tokens_to_go[$ibeg];
                my $cti = $closing_token_indentation{$tok};

                # Fix the value of 'cti' for an isloated non-welded closing qw
                # delimiter.
                if ( $seqno_qw_closing && $ibeg_weld_fix == $ibeg ) {

                    # A quote delimiter which is not a container will not have
                    # a cti value defined.  In this case use the style of a
                    # paren. For example
                    #   my @fars = (
                    #      qw<
                    #        far
                    #        farfar
                    #        farfars-far
                    #      >,
                    #   );
                    if ( !defined($cti) && length($tok) == 1 ) {

                        # something other than ')', '}', ']' ; use flag for ')'
                        $cti = $closing_token_indentation{')'};

                        # But for now, do not outdent non-container qw
                        # delimiters because it would would change existing
                        # formatting.
                        if ( $tok ne '>' ) { $cti = 3 }
                    }

                    # A non-welded closing qw cannot currently use -cti=1
                    # because that option requires a sequence number to find
                    # the opening indentation, and qw quote delimiters are not
                    # sequenced items.
                    if ( defined($cti) && $cti == 1 ) { $cti = 0 }
                }

                if ( !defined($cti) ) {

                    # $cti may not be defined for several reasons.
                    # -padding may have been applied so the character
                    #  has a length > 1
                    # - we may have welded to a closing quote token.
                    #   Here is an example (perltidy -wn):
                    #       __PACKAGE__->load_components( qw(
                    #  >         Core
                    #  >
                    #  >     ) );
                    $adjust_indentation = 0;

                }
                elsif ( $cti == 1 ) {
                    if (   $i_terminal <= $ibeg + 1
                        || $is_semicolon_terminated )
                    {
                        $adjust_indentation = 2;
                    }
                    else {
                        $adjust_indentation = 0;
                    }
                }
                elsif ( $cti == 2 ) {
                    if ($is_semicolon_terminated) {
                        $adjust_indentation = 3;
                    }
                    else {
                        $adjust_indentation = 0;
                    }
                }
                elsif ( $cti == 3 ) {
                    $adjust_indentation = 3;
                }
            }

            # handle option to indent blocks
            else {
                if (
                    $rOpts->{'indent-closing-brace'}
                    && (
                        $i_terminal == $ibeg    #  isolated terminal '}'
                        || $is_semicolon_terminated
                    )
                  )                             #  } xxxx ;
                {
                    $adjust_indentation = 3;
                }
            }
        }

        # if at ');', '};', '>;', and '];' of a terminal qw quote
        elsif ($rpatterns->[0] =~ /^qb*;$/
            && $rfields->[0] =~ /^([\)\}\]\>]);$/ )
        {
            if ( $closing_token_indentation{$1} == 0 ) {
                $adjust_indentation = 1;
            }
            else {
                $adjust_indentation = 3;
            }
        }

        # if line begins with a ':', align it with any
        # previous line leading with corresponding ?
        elsif ( $types_to_go[$ibeg] eq ':' ) {
            (
                $opening_indentation, $opening_offset,
                $is_leading,          $opening_exists
              )
              = $self->get_opening_indentation( $ibeg, $ri_first, $ri_last,
                $rindentation_list );
            if ($is_leading) { $adjust_indentation = 2; }
        }

        ##########################################################
        # Section 2: set indentation according to flag set above
        #
        # Select the indentation object to define leading
        # whitespace.  If we are outdenting something like '} } );'
        # then we want to use one level below the last token
        # ($i_terminal) in order to get it to fully outdent through
        # all levels.
        ##########################################################
        my $indentation;
        my $lev;
        my $level_end = $levels_to_go[$iend];

        if ( $adjust_indentation == 0 ) {
            $indentation = $leading_spaces_to_go[$ibeg];
            $lev         = $levels_to_go[$ibeg];
        }
        elsif ( $adjust_indentation == 1 ) {

            # Change the indentation to be that of a different token on the line
            # Previously, the indentation of the terminal token was used:
            # OLD CODING:
            # $indentation = $reduced_spaces_to_go[$i_terminal];
            # $lev         = $levels_to_go[$i_terminal];

            # Generalization for MOJO:
            # Use the lowest level indentation of the tokens on the line.
            # For example, here we can use the indentation of the ending ';':
            #    } until ($selection > 0 and $selection < 10);   # ok to use ';'
            # But this will not outdent if we use the terminal indentation:
            #    )->then( sub {      # use indentation of the ->, not the {
            # Warning: reduced_spaces_to_go[] may be a reference, do not
            # do numerical checks with it

            my $i_ind = $ibeg;
            $indentation = $reduced_spaces_to_go[$i_ind];
            $lev         = $levels_to_go[$i_ind];
            while ( $i_ind < $i_terminal ) {
                $i_ind++;
                if ( $levels_to_go[$i_ind] < $lev ) {
                    $indentation = $reduced_spaces_to_go[$i_ind];
                    $lev         = $levels_to_go[$i_ind];
                }
            }
        }

        # handle indented closing token which aligns with opening token
        elsif ( $adjust_indentation == 2 ) {

            # handle option to align closing token with opening token
            $lev = $levels_to_go[$ibeg];

            # calculate spaces needed to align with opening token
            my $space_count =
              get_spaces($opening_indentation) + $opening_offset;

            # Indent less than the previous line.
            #
            # Problem: For -lp we don't exactly know what it was if there
            # were recoverable spaces sent to the aligner.  A good solution
            # would be to force a flush of the vertical alignment buffer, so
            # that we would know.  For now, this rule is used for -lp:
            #
            # When the last line did not start with a closing token we will
            # be optimistic that the aligner will recover everything wanted.
            #
            # This rule will prevent us from breaking a hierarchy of closing
            # tokens, and in a worst case will leave a closing paren too far
            # indented, but this is better than frequently leaving it not
            # indented enough.
            my $last_spaces = get_spaces($last_indentation_written);
            if ( !$is_closing_token{$last_leading_token} ) {
                $last_spaces +=
                  get_recoverable_spaces($last_indentation_written);
            }

            # reset the indentation to the new space count if it works
            # only options are all or none: nothing in-between looks good
            $lev = $levels_to_go[$ibeg];
            if ( $space_count < $last_spaces ) {
                if ($rOpts_line_up_parentheses) {
                    my $lev = $levels_to_go[$ibeg];
                    $indentation =
                      new_lp_indentation_item( $space_count, $lev, 0, 0, 0 );
                }
                else {
                    $indentation = $space_count;
                }
            }

            # revert to default if it doesn't work
            else {
                $space_count = leading_spaces_to_go($ibeg);
                if ( $default_adjust_indentation == 0 ) {
                    $indentation = $leading_spaces_to_go[$ibeg];
                }
                elsif ( $default_adjust_indentation == 1 ) {
                    $indentation = $reduced_spaces_to_go[$i_terminal];
                    $lev         = $levels_to_go[$i_terminal];
                }
            }
        }

        # Full indentaion of closing tokens (-icb and -icp or -cti=2)
        else {

            # handle -icb (indented closing code block braces)
            # Updated method for indented block braces: indent one full level if
            # there is no continuation indentation.  This will occur for major
            # structures such as sub, if, else, but not for things like map
            # blocks.
            #
            # Note: only code blocks without continuation indentation are
            # handled here (if, else, unless, ..). In the following snippet,
            # the terminal brace of the sort block will have continuation
            # indentation as shown so it will not be handled by the coding
            # here.  We would have to undo the continuation indentation to do
            # this, but it probably looks ok as is.  This is a possible future
            # update for semicolon terminated lines.
            #
            #     if ($sortby eq 'date' or $sortby eq 'size') {
            #         @files = sort {
            #             $file_data{$a}{$sortby} <=> $file_data{$b}{$sortby}
            #                 or $a cmp $b
            #                 } @files;
            #         }
            #
            if (   $block_type_to_go[$ibeg]
                && $ci_levels_to_go[$i_terminal] == 0 )
            {
                my $spaces = get_spaces( $leading_spaces_to_go[$i_terminal] );
                $indentation = $spaces + $rOpts_indent_columns;

                # NOTE: for -lp we could create a new indentation object, but
                # there is probably no need to do it
            }

            # handle -icp and any -icb block braces which fall through above
            # test such as the 'sort' block mentioned above.
            else {

                # There are currently two ways to handle -icp...
                # One way is to use the indentation of the previous line:
                # $indentation = $last_indentation_written;

                # The other way is to use the indentation that the previous line
                # would have had if it hadn't been adjusted:
                $indentation = $last_unadjusted_indentation;

                # Current method: use the minimum of the two. This avoids
                # inconsistent indentation.
                if ( get_spaces($last_indentation_written) <
                    get_spaces($indentation) )
                {
                    $indentation = $last_indentation_written;
                }
            }

            # use previous indentation but use own level
            # to cause list to be flushed properly
            $lev = $levels_to_go[$ibeg];
        }

        # remember indentation except for multi-line quotes, which get
        # no indentation
        unless ( $ibeg == 0 && $starting_in_quote ) {
            $last_indentation_written    = $indentation;
            $last_unadjusted_indentation = $leading_spaces_to_go[$ibeg];
            $last_leading_token          = $tokens_to_go[$ibeg];

            # Patch to make a line which is the end of a qw quote work with the
            # -lp option.  Make $token_beg look like a closing token as some
            # type even if it is not.  This veriable will become
            # $last_leading_token at the end of this loop.  Then, if the -lp
            # style is selected, and the next line is also a
            # closing token, it will not get more indentation than this line.
            # We need to do this because qw quotes (at present) only get
            # continuation indentation, not one level of indentation, so we
            # need to turn off the -lp indentation.

            # ... a picture is worth a thousand words:

            # perltidy -wn -gnu (Without this patch):
            #   ok(defined(
            #       $seqio = $gb->get_Stream_by_batch([qw(J00522 AF303112
            #       2981014)])
            #             ));

            # perltidy -wn -gnu (With this patch):
            #  ok(defined(
            #      $seqio = $gb->get_Stream_by_batch([qw(J00522 AF303112
            #      2981014)])
            #  ));
            ## if ($seqno_qw_closing) { $last_leading_token = ')' }
            if ( $seqno_qw_closing
                && ( length($token_beg) > 1 || $token_beg eq '>' ) )
            {
                $last_leading_token = ')';
            }
        }

        # be sure lines with leading closing tokens are not outdented more
        # than the line which contained the corresponding opening token.

        #############################################################
        # updated per bug report in alex_bug.pl: we must not
        # mess with the indentation of closing logical braces so
        # we must treat something like '} else {' as if it were
        # an isolated brace
        #############################################################
        my $is_isolated_block_brace = $block_type_to_go[$ibeg]
          && ( $i_terminal == $ibeg
            || $is_if_elsif_else_unless_while_until_for_foreach{
                $block_type_to_go[$ibeg] } );

        # only do this for a ':; which is aligned with its leading '?'
        my $is_unaligned_colon = $types_to_go[$ibeg] eq ':' && !$is_leading;

        if (
            defined($opening_indentation)
            && !$leading_paren_arrow    # MOJO
            && !$is_isolated_block_brace
            && !$is_unaligned_colon
          )
        {
            if ( get_spaces($opening_indentation) > get_spaces($indentation) ) {
                $indentation = $opening_indentation;
            }
        }

        # remember the indentation of each line of this batch
        push @{$rindentation_list}, $indentation;

        # outdent lines with certain leading tokens...
        if (

            # must be first word of this batch
            $ibeg == 0

            # and ...
            && (

                # certain leading keywords if requested
                (
                       $rOpts->{'outdent-keywords'}
                    && $types_to_go[$ibeg] eq 'k'
                    && $outdent_keyword{ $tokens_to_go[$ibeg] }
                )

                # or labels if requested
                || ( $rOpts->{'outdent-labels'} && $types_to_go[$ibeg] eq 'J' )

                # or static block comments if requested
                || (   $types_to_go[$ibeg] eq '#'
                    && $rOpts->{'outdent-static-block-comments'}
                    && $is_static_block_comment )
            )
          )

        {
            my $space_count = leading_spaces_to_go($ibeg);
            if ( $space_count > 0 ) {
                $space_count -= $rOpts_continuation_indentation;
                $is_outdented_line = 1;
                if ( $space_count < 0 ) { $space_count = 0 }

                # do not promote a spaced static block comment to non-spaced;
                # this is not normally necessary but could be for some
                # unusual user inputs (such as -ci = -i)
                if ( $types_to_go[$ibeg] eq '#' && $space_count == 0 ) {
                    $space_count = 1;
                }

                if ($rOpts_line_up_parentheses) {
                    $indentation =
                      new_lp_indentation_item( $space_count, $lev, 0, 0, 0 );
                }
                else {
                    $indentation = $space_count;
                }
            }
        }

        return ( $indentation, $lev, $level_end, $terminal_type,
            $terminal_block_type, $is_semicolon_terminated,
            $is_outdented_line );
    }
} ## end closure set_adjusted_indentation

sub get_opening_indentation {

    # get the indentation of the line which output the opening token
    # corresponding to a given closing token in the current output batch.
    #
    # given:
    # $i_closing - index in this line of a closing token ')' '}' or ']'
    #
    # $ri_first - reference to list of the first index $i for each output
    #               line in this batch
    # $ri_last - reference to list of the last index $i for each output line
    #              in this batch
    # $rindentation_list - reference to a list containing the indentation
    #            used for each line.
    # $qw_seqno - optional sequence number to use if normal seqno not defined
    #           (TODO: would be more general to just look this up from index i)
    #
    # return:
    #   -the indentation of the line which contained the opening token
    #    which matches the token at index $i_opening
    #   -and its offset (number of columns) from the start of the line
    #
    my ( $self, $i_closing, $ri_first, $ri_last, $rindentation_list, $qw_seqno )
      = @_;

    # first, see if the opening token is in the current batch
    my $i_opening = $mate_index_to_go[$i_closing];
    my ( $indent, $offset, $is_leading, $exists );
    $exists = 1;
    if ( defined($i_opening) && $i_opening >= 0 ) {

        # it is..look up the indentation
        ( $indent, $offset, $is_leading ) =
          lookup_opening_indentation( $i_opening, $ri_first, $ri_last,
            $rindentation_list );
    }

    # if not, it should have been stored in the hash by a previous batch
    else {
        my $seqno = $type_sequence_to_go[$i_closing];
        $seqno = $qw_seqno unless ($seqno);
        ( $indent, $offset, $is_leading, $exists ) =
          get_saved_opening_indentation($seqno);
    }
    return ( $indent, $offset, $is_leading, $exists );
}

sub set_vertical_tightness_flags {

    my ( $self, $n, $n_last_line, $ibeg, $iend, $ri_first, $ri_last,
        $ending_in_quote, $closing_side_comment )
      = @_;

    # Define vertical tightness controls for the nth line of a batch.
    # We create an array of parameters which tell the vertical aligner
    # if we should combine this line with the next line to achieve the
    # desired vertical tightness.  The array of parameters contains:
    #
    #   [0] type: 1=opening non-block    2=closing non-block
    #             3=opening block brace  4=closing block brace
    #
    #   [1] flag: if opening: 1=no multiple steps, 2=multiple steps ok
    #             if closing: spaces of padding to use
    #   [2] sequence number of container
    #   [3] valid flag: do not append if this flag is false. Will be
    #       true if appropriate -vt flag is set.  Otherwise, Will be
    #       made true only for 2 line container in parens with -lp
    #
    # These flags are used by sub set_leading_whitespace in
    # the vertical aligner

    my $rvertical_tightness_flags = [ 0, 0, 0, 0, 0, 0 ];

    # Uses these parameters:
    #   $rOpts_block_brace_tightness
    #   $rOpts_block_brace_vertical_tightness
    #   $rOpts_stack_closing_block_brace
    #   %opening_vertical_tightness
    #   %closing_vertical_tightness
    #   %opening_token_right
    #   %stack_closing_token
    #   %stack_opening_token

    #--------------------------------------------------------------
    # Vertical Tightness Flags Section 1:
    # Handle Lines 1 .. n-1 but not the last line
    # For non-BLOCK tokens, we will need to examine the next line
    # too, so we won't consider the last line.
    #--------------------------------------------------------------
    if ( $n < $n_last_line ) {

        #--------------------------------------------------------------
        # Vertical Tightness Flags Section 1a:
        # Look for Type 1, last token of this line is a non-block opening token
        #--------------------------------------------------------------
        my $ibeg_next = $ri_first->[ $n + 1 ];
        my $token_end = $tokens_to_go[$iend];
        my $iend_next = $ri_last->[ $n + 1 ];
        if (
               $type_sequence_to_go[$iend]
            && !$block_type_to_go[$iend]
            && $is_opening_token{$token_end}
            && (
                $opening_vertical_tightness{$token_end} > 0

                # allow 2-line method call to be closed up
                || (   $rOpts_line_up_parentheses
                    && $token_end eq '('
                    && $iend > $ibeg
                    && $types_to_go[ $iend - 1 ] ne 'b' )
            )
          )
        {

            # avoid multiple jumps in nesting depth in one line if
            # requested
            my $ovt       = $opening_vertical_tightness{$token_end};
            my $iend_next = $ri_last->[ $n + 1 ];
            unless (
                $ovt < 2
                && ( $nesting_depth_to_go[ $iend_next + 1 ] !=
                    $nesting_depth_to_go[$ibeg_next] )
              )
            {

                # If -vt flag has not been set, mark this as invalid
                # and aligner will validate it if it sees the closing paren
                # within 2 lines.
                my $valid_flag = $ovt;
                @{$rvertical_tightness_flags} =
                  ( 1, $ovt, $type_sequence_to_go[$iend], $valid_flag );
            }
        }

        #--------------------------------------------------------------
        # Vertical Tightness Flags Section 1b:
        # Look for Type 2, first token of next line is a non-block closing
        # token .. and be sure this line does not have a side comment
        #--------------------------------------------------------------
        my $token_next = $tokens_to_go[$ibeg_next];
        if (   $type_sequence_to_go[$ibeg_next]
            && !$block_type_to_go[$ibeg_next]
            && $is_closing_token{$token_next}
            && $types_to_go[$iend] ne '#' )    # for safety, shouldn't happen!
        {
            my $ovt = $opening_vertical_tightness{$token_next};
            my $cvt = $closing_vertical_tightness{$token_next};
            if (

                # Never append a trailing line like   ')->pack(' because it
                # will throw off later alignment.  So this line must start at a
                # deeper level than the next line (fix1 for welding, git #45).
                (
                    $nesting_depth_to_go[$ibeg_next] >=
                    $nesting_depth_to_go[ $iend_next + 1 ] + 1
                )
                && (
                    $cvt == 2
                    || (
                        $container_environment_to_go[$ibeg_next] ne 'LIST'
                        && (
                            $cvt == 1

                            # allow closing up 2-line method calls
                            || (   $rOpts_line_up_parentheses
                                && $token_next eq ')' )
                        )
                    )
                )
              )
            {

                # decide which trailing closing tokens to append..
                my $ok = 0;
                if ( $cvt == 2 || $iend_next == $ibeg_next ) { $ok = 1 }
                else {
                    my $str = join( '',
                        @types_to_go[ $ibeg_next + 1 .. $ibeg_next + 2 ] );

                    # append closing token if followed by comment or ';'
                    # or another closing token (fix2 for welding, git #45)
                    if ( $str =~ /^b?[\)\]\}R#;]/ ) { $ok = 1 }
                }

                if ($ok) {
                    my $valid_flag = $cvt;
                    @{$rvertical_tightness_flags} = (
                        2,
                        $tightness{$token_next} == 2 ? 0 : 1,
                        $type_sequence_to_go[$ibeg_next], $valid_flag,
                    );
                }
            }
        }

        #--------------------------------------------------------------
        # Vertical Tightness Flags Section 1c:
        # Implement the Opening Token Right flag (Type 2)..
        # If requested, move an isolated trailing opening token to the end of
        # the previous line which ended in a comma.  We could do this
        # in sub recombine_breakpoints but that would cause problems
        # with -lp formatting.  The problem is that indentation will
        # quickly move far to the right in nested expressions.  By
        # doing it after indentation has been set, we avoid changes
        # to the indentation.  Actual movement of the token takes place
        # in sub valign_output_step_B.
        #--------------------------------------------------------------
        if (
            $opening_token_right{ $tokens_to_go[$ibeg_next] }

            # previous line is not opening
            # (use -sot to combine with it)
            && !$is_opening_token{$token_end}

            # previous line ended in one of these
            # (add other cases if necessary; '=>' and '.' are not necessary
            && !$block_type_to_go[$ibeg_next]

            # this is a line with just an opening token
            && (   $iend_next == $ibeg_next
                || $iend_next == $ibeg_next + 2
                && $types_to_go[$iend_next] eq '#' )

            # looks bad if we align vertically with the wrong container
            && $tokens_to_go[$ibeg] ne $tokens_to_go[$ibeg_next]
          )
        {
            my $valid_flag = 1;
            my $spaces     = ( $types_to_go[ $ibeg_next - 1 ] eq 'b' ) ? 1 : 0;
            @{$rvertical_tightness_flags} =
              ( 2, $spaces, $type_sequence_to_go[$ibeg_next], $valid_flag, );
        }

        #--------------------------------------------------------------
        # Vertical Tightness Flags Section 1d:
        # Stacking of opening and closing tokens (Type 2)
        #--------------------------------------------------------------
        my $stackable;
        my $token_beg_next = $tokens_to_go[$ibeg_next];

        # patch to make something like 'qw(' behave like an opening paren
        # (aran.t)
        if ( $types_to_go[$ibeg_next] eq 'q' ) {
            if ( $token_beg_next =~ /^qw\s*([\[\(\{])$/ ) {
                $token_beg_next = $1;
            }
        }

        if (   $is_closing_token{$token_end}
            && $is_closing_token{$token_beg_next} )
        {
            $stackable = $stack_closing_token{$token_beg_next}
              unless ( $block_type_to_go[$ibeg_next] )
              ;    # shouldn't happen; just checking
        }
        elsif ($is_opening_token{$token_end}
            && $is_opening_token{$token_beg_next} )
        {
            $stackable = $stack_opening_token{$token_beg_next}
              unless ( $block_type_to_go[$ibeg_next] )
              ;    # shouldn't happen; just checking
        }

        if ($stackable) {

            my $is_semicolon_terminated;
            if ( $n + 1 == $n_last_line ) {
                my ( $terminal_type, $i_terminal ) =
                  terminal_type_i( $ibeg_next, $iend_next );
                $is_semicolon_terminated = $terminal_type eq ';'
                  && $nesting_depth_to_go[$iend_next] <
                  $nesting_depth_to_go[$ibeg_next];
            }

            # this must be a line with just an opening token
            # or end in a semicolon
            if (
                $is_semicolon_terminated
                || (   $iend_next == $ibeg_next
                    || $iend_next == $ibeg_next + 2
                    && $types_to_go[$iend_next] eq '#' )
              )
            {
                my $valid_flag = 1;
                my $spaces = ( $types_to_go[ $ibeg_next - 1 ] eq 'b' ) ? 1 : 0;
                @{$rvertical_tightness_flags} =
                  ( 2, $spaces, $type_sequence_to_go[$ibeg_next], $valid_flag,
                  );
            }
        }
    }

    #--------------------------------------------------------------
    # Vertical Tightness Flags Section 2:
    # Handle type 3, opening block braces on last line of the batch
    # Check for a last line with isolated opening BLOCK curly
    #--------------------------------------------------------------
    elsif ($rOpts_block_brace_vertical_tightness
        && $ibeg eq $iend
        && $types_to_go[$iend] eq '{'
        && $block_type_to_go[$iend] =~
        /$block_brace_vertical_tightness_pattern/ )
    {
        @{$rvertical_tightness_flags} =
          ( 3, $rOpts_block_brace_vertical_tightness, 0, 1 );
    }

    #--------------------------------------------------------------
    # Vertical Tightness Flags Section 3:
    # Handle type 4, a closing block brace on the last line of the batch Check
    # for a last line with isolated closing BLOCK curly
    # Patch: added a check for any new closing side comment which the
    # -csc option may generate. If it exists, there will be a side comment
    # so we cannot combine with a brace on the next line.  This issue
    # occurs for the combination -scbb and -csc is used.
    #--------------------------------------------------------------
    elsif ($rOpts_stack_closing_block_brace
        && $ibeg eq $iend
        && $block_type_to_go[$iend]
        && $types_to_go[$iend] eq '}'
        && ( !$closing_side_comment || $n < $n_last_line ) )
    {
        my $spaces = $rOpts_block_brace_tightness == 2 ? 0 : 1;
        @{$rvertical_tightness_flags} =
          ( 4, $spaces, $type_sequence_to_go[$iend], 1 );
    }

    # pack in the sequence numbers of the ends of this line
    my $seqno_beg = $type_sequence_to_go[$ibeg];
    if ( !$seqno_beg && $types_to_go[$ibeg] eq 'q' ) {
        $seqno_beg = $self->get_seqno( $ibeg, $ending_in_quote );
    }
    my $seqno_end = $type_sequence_to_go[$iend];
    if ( !$seqno_end && $types_to_go[$iend] eq 'q' ) {
        $seqno_end = $self->get_seqno( $iend, $ending_in_quote );
    }
    $rvertical_tightness_flags->[4] = $seqno_beg;
    $rvertical_tightness_flags->[5] = $seqno_end;
    return $rvertical_tightness_flags;
}

##########################################################
# CODE SECTION 14: Code for creating closing side comments
##########################################################

{    ## begin closure accumulate_csc_text

# These routines are called once per batch when the --closing-side-comments flag
# has been set.

    my %block_leading_text;
    my %block_opening_line_number;
    my $csc_new_statement_ok;
    my $csc_last_label;
    my %csc_block_label;
    my $accumulating_text_for_block;
    my $leading_block_text;
    my $rleading_block_if_elsif_text;
    my $leading_block_text_level;
    my $leading_block_text_length_exceeded;
    my $leading_block_text_line_length;
    my $leading_block_text_line_number;

    sub initialize_csc_vars {
        %block_leading_text           = ();
        %block_opening_line_number    = ();
        $csc_new_statement_ok         = 1;
        $csc_last_label               = "";
        %csc_block_label              = ();
        $rleading_block_if_elsif_text = [];
        $accumulating_text_for_block  = "";
        reset_block_text_accumulator();
        return;
    }

    sub reset_block_text_accumulator {

        # save text after 'if' and 'elsif' to append after 'else'
        if ($accumulating_text_for_block) {

            if ( $accumulating_text_for_block =~ /^(if|elsif)$/ ) {
                push @{$rleading_block_if_elsif_text}, $leading_block_text;
            }
        }
        $accumulating_text_for_block        = "";
        $leading_block_text                 = "";
        $leading_block_text_level           = 0;
        $leading_block_text_length_exceeded = 0;
        $leading_block_text_line_number     = 0;
        $leading_block_text_line_length     = 0;
        return;
    }

    sub set_block_text_accumulator {
        my ( $self, $i ) = @_;
        $accumulating_text_for_block = $tokens_to_go[$i];
        if ( $accumulating_text_for_block !~ /^els/ ) {
            $rleading_block_if_elsif_text = [];
        }
        $leading_block_text                 = "";
        $leading_block_text_level           = $levels_to_go[$i];
        $leading_block_text_line_number     = $self->get_output_line_number();
        $leading_block_text_length_exceeded = 0;

        # this will contain the column number of the last character
        # of the closing side comment
        $leading_block_text_line_length =
          length($csc_last_label) +
          length($accumulating_text_for_block) +
          length( $rOpts->{'closing-side-comment-prefix'} ) +
          $leading_block_text_level * $rOpts_indent_columns + 3;
        return;
    }

    sub accumulate_block_text {
        my ( $self, $i ) = @_;

        # accumulate leading text for -csc, ignoring any side comments
        if (   $accumulating_text_for_block
            && !$leading_block_text_length_exceeded
            && $types_to_go[$i] ne '#' )
        {

            my $added_length = $token_lengths_to_go[$i];
            $added_length += 1 if $i == 0;
            my $new_line_length =
              $leading_block_text_line_length + $added_length;

            # we can add this text if we don't exceed some limits..
            if (

                # we must not have already exceeded the text length limit
                length($leading_block_text) <
                $rOpts_closing_side_comment_maximum_text

                # and either:
                # the new total line length must be below the line length limit
                # or the new length must be below the text length limit
                # (ie, we may allow one token to exceed the text length limit)
                && (
                    $new_line_length <
                    $maximum_line_length[$leading_block_text_level]

                    || length($leading_block_text) + $added_length <
                    $rOpts_closing_side_comment_maximum_text
                )

               # UNLESS: we are adding a closing paren before the brace we seek.
               # This is an attempt to avoid situations where the ... to be
               # added are longer than the omitted right paren, as in:

             #   foreach my $item (@a_rather_long_variable_name_here) {
             #      &whatever;
             #   } ## end foreach my $item (@a_rather_long_variable_name_here...

                || (
                    $tokens_to_go[$i] eq ')'
                    && (
                        (
                               $i + 1 <= $max_index_to_go
                            && $block_type_to_go[ $i + 1 ] eq
                            $accumulating_text_for_block
                        )
                        || (   $i + 2 <= $max_index_to_go
                            && $block_type_to_go[ $i + 2 ] eq
                            $accumulating_text_for_block )
                    )
                )
              )
            {

                # add an extra space at each newline
                if ( $i == 0 ) { $leading_block_text .= ' ' }

                # add the token text
                $leading_block_text .= $tokens_to_go[$i];
                $leading_block_text_line_length = $new_line_length;
            }

            # show that text was truncated if necessary
            elsif ( $types_to_go[$i] ne 'b' ) {
                $leading_block_text_length_exceeded = 1;
                $leading_block_text .= '...';
            }
        }
        return;
    }

    sub accumulate_csc_text {

        my ($self) = @_;

        # called once per output buffer when -csc is used. Accumulates
        # the text placed after certain closing block braces.
        # Defines and returns the following for this buffer:

        my $block_leading_text = "";    # the leading text of the last '}'
        my $rblock_leading_if_elsif_text;
        my $i_block_leading_text =
          -1;    # index of token owning block_leading_text
        my $block_line_count    = 100;    # how many lines the block spans
        my $terminal_type       = 'b';    # type of last nonblank token
        my $i_terminal          = 0;      # index of last nonblank token
        my $terminal_block_type = "";

        # update most recent statement label
        $csc_last_label = "" unless ($csc_last_label);
        if ( $types_to_go[0] eq 'J' ) { $csc_last_label = $tokens_to_go[0] }
        my $block_label = $csc_last_label;

        # Loop over all tokens of this batch
        for my $i ( 0 .. $max_index_to_go ) {
            my $type       = $types_to_go[$i];
            my $block_type = $block_type_to_go[$i];
            my $token      = $tokens_to_go[$i];

            # remember last nonblank token type
            if ( $type ne '#' && $type ne 'b' ) {
                $terminal_type       = $type;
                $terminal_block_type = $block_type;
                $i_terminal          = $i;
            }

            my $type_sequence = $type_sequence_to_go[$i];
            if ( $block_type && $type_sequence ) {

                if ( $token eq '}' ) {

                    # restore any leading text saved when we entered this block
                    if ( defined( $block_leading_text{$type_sequence} ) ) {
                        ( $block_leading_text, $rblock_leading_if_elsif_text )
                          = @{ $block_leading_text{$type_sequence} };
                        $i_block_leading_text = $i;
                        delete $block_leading_text{$type_sequence};
                        $rleading_block_if_elsif_text =
                          $rblock_leading_if_elsif_text;
                    }

                    if ( defined( $csc_block_label{$type_sequence} ) ) {
                        $block_label = $csc_block_label{$type_sequence};
                        delete $csc_block_label{$type_sequence};
                    }

                    # if we run into a '}' then we probably started accumulating
                    # at something like a trailing 'if' clause..no harm done.
                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] <= $leading_block_text_level )
                    {
                        my $lev = $levels_to_go[$i];
                        reset_block_text_accumulator();
                    }

                    if ( defined( $block_opening_line_number{$type_sequence} ) )
                    {
                        my $output_line_number =
                          $self->get_output_line_number();
                        $block_line_count =
                          $output_line_number -
                          $block_opening_line_number{$type_sequence} + 1;
                        delete $block_opening_line_number{$type_sequence};
                    }
                    else {

                        # Error: block opening line undefined for this line..
                        # This shouldn't be possible, but it is not a
                        # significant problem.
                    }
                }

                elsif ( $token eq '{' ) {

                    my $line_number = $self->get_output_line_number();
                    $block_opening_line_number{$type_sequence} = $line_number;

                    # set a label for this block, except for
                    # a bare block which already has the label
                    # A label can only be used on the next {
                    if ( $block_type =~ /:$/ ) { $csc_last_label = "" }
                    $csc_block_label{$type_sequence} = $csc_last_label;
                    $csc_last_label = "";

                    if (   $accumulating_text_for_block
                        && $levels_to_go[$i] == $leading_block_text_level )
                    {

                        if ( $accumulating_text_for_block eq $block_type ) {

                            # save any leading text before we enter this block
                            $block_leading_text{$type_sequence} = [
                                $leading_block_text,
                                $rleading_block_if_elsif_text
                            ];
                            $block_opening_line_number{$type_sequence} =
                              $leading_block_text_line_number;
                            reset_block_text_accumulator();
                        }
                        else {

                            # shouldn't happen, but not a serious error.
                            # We were accumulating -csc text for block type
                            # $accumulating_text_for_block and unexpectedly
                            # encountered a '{' for block type $block_type.
                        }
                    }
                }
            }

            if (   $type eq 'k'
                && $csc_new_statement_ok
                && $is_if_elsif_else_unless_while_until_for_foreach{$token}
                && $token =~ /$closing_side_comment_list_pattern/ )
            {
                $self->set_block_text_accumulator($i);
            }
            else {

                # note: ignoring type 'q' because of tricks being played
                # with 'q' for hanging side comments
                if ( $type ne 'b' && $type ne '#' && $type ne 'q' ) {
                    $csc_new_statement_ok =
                      ( $block_type || $type eq 'J' || $type eq ';' );
                }
                if (   $type eq ';'
                    && $accumulating_text_for_block
                    && $levels_to_go[$i] == $leading_block_text_level )
                {
                    reset_block_text_accumulator();
                }
                else {
                    $self->accumulate_block_text($i);
                }
            }
        }

        # Treat an 'else' block specially by adding preceding 'if' and
        # 'elsif' text.  Otherwise, the 'end else' is not helpful,
        # especially for cuddled-else formatting.
        if ( $terminal_block_type =~ /^els/ && $rblock_leading_if_elsif_text ) {
            $block_leading_text =
              $self->make_else_csc_text( $i_terminal, $terminal_block_type,
                $block_leading_text, $rblock_leading_if_elsif_text );
        }

        # if this line ends in a label then remember it for the next pass
        $csc_last_label = "";
        if ( $terminal_type eq 'J' ) {
            $csc_last_label = $tokens_to_go[$i_terminal];
        }

        return ( $terminal_type, $i_terminal, $i_block_leading_text,
            $block_leading_text, $block_line_count, $block_label );
    }

    sub make_else_csc_text {

        # create additional -csc text for an 'else' and optionally 'elsif',
        # depending on the value of switch
        #
        #  = 0 add 'if' text to trailing else
        #  = 1 same as 0 plus:
        #      add 'if' to 'elsif's if can fit in line length
        #      add last 'elsif' to trailing else if can fit in one line
        #  = 2 same as 1 but do not check if exceed line length
        #
        # $rif_elsif_text = a reference to a list of all previous closing
        # side comments created for this if block
        #
        my ( $self, $i_terminal, $block_type, $block_leading_text,
            $rif_elsif_text )
          = @_;
        my $csc_text = $block_leading_text;

        my $rOpts_closing_side_comment_else_flag =
          $rOpts->{'closing-side-comment-else-flag'};

        if (   $block_type eq 'elsif'
            && $rOpts_closing_side_comment_else_flag == 0 )
        {
            return $csc_text;
        }

        my $count = @{$rif_elsif_text};
        return $csc_text unless ($count);

        my $if_text = '[ if' . $rif_elsif_text->[0];

        # always show the leading 'if' text on 'else'
        if ( $block_type eq 'else' ) {
            $csc_text .= $if_text;
        }

        # see if that's all
        if ( $rOpts_closing_side_comment_else_flag == 0 ) {
            return $csc_text;
        }

        my $last_elsif_text = "";
        if ( $count > 1 ) {
            $last_elsif_text = ' [elsif' . $rif_elsif_text->[ $count - 1 ];
            if ( $count > 2 ) { $last_elsif_text = ' [...' . $last_elsif_text; }
        }

        # tentatively append one more item
        my $saved_text = $csc_text;
        if ( $block_type eq 'else' ) {
            $csc_text .= $last_elsif_text;
        }
        else {
            $csc_text .= ' ' . $if_text;
        }

        # all done if no length checks requested
        if ( $rOpts_closing_side_comment_else_flag == 2 ) {
            return $csc_text;
        }

        # undo it if line length exceeded
        my $length =
          length($csc_text) +
          length($block_type) +
          length( $rOpts->{'closing-side-comment-prefix'} ) +
          $levels_to_go[$i_terminal] * $rOpts_indent_columns + 3;
        if ( $length > $maximum_line_length[$leading_block_text_level] ) {
            $csc_text = $saved_text;
        }
        return $csc_text;
    }
} ## end closure accumulate_csc_text

{    ## begin closure balance_csc_text

    # Some additional routines for handling the --closing-side-comments option

    my %matching_char;

    BEGIN {
        %matching_char = (
            '{' => '}',
            '(' => ')',
            '[' => ']',
            '}' => '{',
            ')' => '(',
            ']' => '[',
        );
    }

    sub balance_csc_text {

        # Append characters to balance a closing side comment so that editors
        # such as vim can correctly jump through code.
        # Simple Example:
        #  input  = ## end foreach my $foo ( sort { $b  ...
        #  output = ## end foreach my $foo ( sort { $b  ...})

        # NOTE: This routine does not currently filter out structures within
        # quoted text because the bounce algorithms in text editors do not
        # necessarily do this either (a version of vim was checked and
        # did not do this).

        # Some complex examples which will cause trouble for some editors:
        #  while ( $mask_string =~ /\{[^{]*?\}/g ) {
        #  if ( $mask_str =~ /\}\s*els[^\{\}]+\{$/ ) {
        #  if ( $1 eq '{' ) {
        # test file test1/braces.pl has many such examples.

        my ($csc) = @_;

        # loop to examine characters one-by-one, RIGHT to LEFT and
        # build a balancing ending, LEFT to RIGHT.
        for ( my $pos = length($csc) - 1 ; $pos >= 0 ; $pos-- ) {

            my $char = substr( $csc, $pos, 1 );

            # ignore everything except structural characters
            next unless ( $matching_char{$char} );

            # pop most recently appended character
            my $top = chop($csc);

            # push it back plus the mate to the newest character
            # unless they balance each other.
            $csc = $csc . $top . $matching_char{$char} unless $top eq $char;
        }

        # return the balanced string
        return $csc;
    }
} ## end closure balance_csc_text

sub add_closing_side_comment {

    my $self = shift;
    my $rLL  = $self->[_rLL_];

    # add closing side comments after closing block braces if -csc used
    my ( $closing_side_comment, $cscw_block_comment );

    #---------------------------------------------------------------
    # Step 1: loop through all tokens of this line to accumulate
    # the text needed to create the closing side comments. Also see
    # how the line ends.
    #---------------------------------------------------------------

    my ( $terminal_type, $i_terminal, $i_block_leading_text,
        $block_leading_text, $block_line_count, $block_label )
      = $self->accumulate_csc_text();

    #---------------------------------------------------------------
    # Step 2: make the closing side comment if this ends a block
    #---------------------------------------------------------------
    my $have_side_comment = $types_to_go[$max_index_to_go] eq '#';

    # if this line might end in a block closure..
    if (
        $terminal_type eq '}'

        # ..and either
        && (

            # the block is long enough
            ( $block_line_count >= $rOpts->{'closing-side-comment-interval'} )

            # or there is an existing comment to check
            || (   $have_side_comment
                && $rOpts->{'closing-side-comment-warnings'} )
        )

        # .. and if this is one of the types of interest
        && $block_type_to_go[$i_terminal] =~
        /$closing_side_comment_list_pattern/

        # .. but not an anonymous sub
        # These are not normally of interest, and their closing braces are
        # often followed by commas or semicolons anyway.  This also avoids
        # possible erratic output due to line numbering inconsistencies
        # in the cases where their closing braces terminate a line.
        && $block_type_to_go[$i_terminal] ne 'sub'

        # ..and the corresponding opening brace must is not in this batch
        # (because we do not need to tag one-line blocks, although this
        # should also be caught with a positive -csci value)
        && $mate_index_to_go[$i_terminal] < 0

        # ..and either
        && (

            # this is the last token (line doesn't have a side comment)
            !$have_side_comment

            # or the old side comment is a closing side comment
            || $tokens_to_go[$max_index_to_go] =~
            /$closing_side_comment_prefix_pattern/
        )
      )
    {

        # then make the closing side comment text
        if ($block_label) { $block_label .= " " }
        my $token =
"$rOpts->{'closing-side-comment-prefix'} $block_label$block_type_to_go[$i_terminal]";

        # append any extra descriptive text collected above
        if ( $i_block_leading_text == $i_terminal ) {
            $token .= $block_leading_text;
        }

        $token = balance_csc_text($token)
          if $rOpts->{'closing-side-comments-balanced'};

        $token =~ s/\s*$//;    # trim any trailing whitespace

        # handle case of existing closing side comment
        if ($have_side_comment) {

            # warn if requested and tokens differ significantly
            if ( $rOpts->{'closing-side-comment-warnings'} ) {
                my $old_csc = $tokens_to_go[$max_index_to_go];
                my $new_csc = $token;
                $new_csc =~ s/\s+//g;            # trim all whitespace
                $old_csc =~ s/\s+//g;            # trim all whitespace
                $new_csc =~ s/[\]\)\}\s]*$//;    # trim trailing structures
                $old_csc =~ s/[\]\)\}\s]*$//;    # trim trailing structures
                $new_csc =~ s/(\.\.\.)$//;       # trim trailing '...'
                my $new_trailing_dots = $1;
                $old_csc =~ s/(\.\.\.)\s*$//;    # trim trailing '...'

                # Patch to handle multiple closing side comments at
                # else and elsif's.  These have become too complicated
                # to check, so if we see an indication of
                # '[ if' or '[ # elsif', then assume they were made
                # by perltidy.
                if ( $block_type_to_go[$i_terminal] eq 'else' ) {
                    if ( $old_csc =~ /\[\s*elsif/ ) { $old_csc = $new_csc }
                }
                elsif ( $block_type_to_go[$i_terminal] eq 'elsif' ) {
                    if ( $old_csc =~ /\[\s*if/ ) { $old_csc = $new_csc }
                }

                # if old comment is contained in new comment,
                # only compare the common part.
                if ( length($new_csc) > length($old_csc) ) {
                    $new_csc = substr( $new_csc, 0, length($old_csc) );
                }

                # if the new comment is shorter and has been limited,
                # only compare the common part.
                if ( length($new_csc) < length($old_csc)
                    && $new_trailing_dots )
                {
                    $old_csc = substr( $old_csc, 0, length($new_csc) );
                }

                # any remaining difference?
                if ( $new_csc ne $old_csc ) {

                    # just leave the old comment if we are below the threshold
                    # for creating side comments
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                    }

                    # otherwise we'll make a note of it
                    else {

                        warning(
"perltidy -cscw replaced: $tokens_to_go[$max_index_to_go]\n"
                        );

                        # save the old side comment in a new trailing block
                        # comment
                        my $timestamp = "";
                        if ( $rOpts->{'timestamp'} ) {
                            my ( $day, $month, $year ) = (localtime)[ 3, 4, 5 ];
                            $year  += 1900;
                            $month += 1;
                            $timestamp = "$year-$month-$day";
                        }
                        $cscw_block_comment =
"## perltidy -cscw $timestamp: $tokens_to_go[$max_index_to_go]";
## "## perltidy -cscw $year-$month-$day: $tokens_to_go[$max_index_to_go]";
                    }
                }
                else {

                    # No differences.. we can safely delete old comment if we
                    # are below the threshold
                    if ( $block_line_count <
                        $rOpts->{'closing-side-comment-interval'} )
                    {
                        $token = undef;
                        $self->unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq '#' );
                        $self->unstore_token_to_go()
                          if ( $types_to_go[$max_index_to_go] eq 'b' );
                    }
                }
            }

            # switch to the new csc (unless we deleted it!)
            if ($token) {
                $tokens_to_go[$max_index_to_go] = $token;
                my $K = $K_to_go[$max_index_to_go];
                $rLL->[$K]->[_TOKEN_] = $token;
                $rLL->[$K]->[_TOKEN_LENGTH_] =
                  length($token);    # NOTE: length no longer important
            }
        }

        # handle case of NO existing closing side comment
        else {

            # To avoid inserting a new token in the token arrays, we
            # will just return the new side comment so that it can be
            # inserted just before it is needed in the call to the
            # vertical aligner.
            $closing_side_comment = $token;
        }
    }
    return ( $closing_side_comment, $cscw_block_comment );
}

############################
# CODE SECTION 15: Summarize
############################

sub wrapup {

    # This is the last routine called when a file is formatted.
    # Flush buffer and write any informative messages
    my $self = shift;

    $self->flush();
    my $file_writer_object = $self->[_file_writer_object_];
    $file_writer_object->decrement_output_line_number()
      ;    # fix up line number since it was incremented
    we_are_at_the_last_line();
    my $added_semicolon_count    = $self->[_added_semicolon_count_];
    my $first_added_semicolon_at = $self->[_first_added_semicolon_at_];
    my $last_added_semicolon_at  = $self->[_last_added_semicolon_at_];

    if ( $added_semicolon_count > 0 ) {
        my $first = ( $added_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $added_semicolon_count > 1 ) ? "semicolons were" : "semicolon was";
        write_logfile_entry("$added_semicolon_count $what added:\n");
        write_logfile_entry(
            "  $first at input line $first_added_semicolon_at\n");

        if ( $added_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_added_semicolon_at\n");
        }
        write_logfile_entry("  (Use -nasc to prevent semicolon addition)\n");
        write_logfile_entry("\n");
    }

    my $deleted_semicolon_count    = $self->[_deleted_semicolon_count_];
    my $first_deleted_semicolon_at = $self->[_first_deleted_semicolon_at_];
    my $last_deleted_semicolon_at  = $self->[_last_deleted_semicolon_at_];
    if ( $deleted_semicolon_count > 0 ) {
        my $first = ( $deleted_semicolon_count > 1 ) ? "First" : "";
        my $what =
          ( $deleted_semicolon_count > 1 )
          ? "semicolons were"
          : "semicolon was";
        write_logfile_entry(
            "$deleted_semicolon_count unnecessary $what deleted:\n");
        write_logfile_entry(
            "  $first at input line $first_deleted_semicolon_at\n");

        if ( $deleted_semicolon_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_deleted_semicolon_at\n");
        }
        write_logfile_entry("  (Use -ndsm to prevent semicolon deletion)\n");
        write_logfile_entry("\n");
    }

    my $embedded_tab_count    = $self->[_embedded_tab_count_];
    my $first_embedded_tab_at = $self->[_first_embedded_tab_at_];
    my $last_embedded_tab_at  = $self->[_last_embedded_tab_at_];
    if ( $embedded_tab_count > 0 ) {
        my $first = ( $embedded_tab_count > 1 ) ? "First" : "";
        my $what =
          ( $embedded_tab_count > 1 )
          ? "quotes or patterns"
          : "quote or pattern";
        write_logfile_entry("$embedded_tab_count $what had embedded tabs:\n");
        write_logfile_entry(
"This means the display of this script could vary with device or software\n"
        );
        write_logfile_entry("  $first at input line $first_embedded_tab_at\n");

        if ( $embedded_tab_count > 1 ) {
            write_logfile_entry(
                "   Last at input line $last_embedded_tab_at\n");
        }
        write_logfile_entry("\n");
    }

    my $first_tabbing_disagreement = $self->[_first_tabbing_disagreement_];
    my $last_tabbing_disagreement  = $self->[_last_tabbing_disagreement_];
    my $tabbing_disagreement_count = $self->[_tabbing_disagreement_count_];
    my $in_tabbing_disagreement    = $self->[_in_tabbing_disagreement_];

    if ($first_tabbing_disagreement) {
        write_logfile_entry(
"First indentation disagreement seen at input line $first_tabbing_disagreement\n"
        );
    }

    my $first_btd = $self->[_first_brace_tabbing_disagreement_];
    if ($first_btd) {
        my $msg =
"First closing brace indentation disagreement started at input line $first_btd\n";
        write_logfile_entry($msg);

        # leave a hint in the .ERR file if there was a brace error
        if ( get_saw_brace_error() ) { warning("NOTE: $msg") }
    }

    my $in_btd = $self->[_in_brace_tabbing_disagreement_];
    if ($in_btd) {
        my $msg =
"Ending with brace indentation disagreement which started at input line $in_btd\n";
        write_logfile_entry($msg);

        # leave a hint in the .ERR file if there was a brace error
        if ( get_saw_brace_error() ) { warning("NOTE: $msg") }
    }

    if ($in_tabbing_disagreement) {
        my $msg =
"Ending with indentation disagreement which started at input line $in_tabbing_disagreement\n";
        write_logfile_entry($msg);
    }
    else {

        if ($last_tabbing_disagreement) {

            write_logfile_entry(
"Last indentation disagreement seen at input line $last_tabbing_disagreement\n"
            );
        }
        else {
            write_logfile_entry("No indentation disagreement seen\n");
        }
    }

    if ($first_tabbing_disagreement) {
        write_logfile_entry(
"Note: Indentation disagreement detection is not accurate for outdenting and -lp.\n"
        );
    }
    write_logfile_entry("\n");

    my $vao = $self->[_vertical_aligner_object_];
    $vao->report_anything_unusual();

    $file_writer_object->report_line_length_errors();

    $self->[_converged_] = $file_writer_object->get_convergence_check()
      || $rOpts->{'indent-only'};

    return;
}

} ## end package Perl::Tidy::Formatter
1;
