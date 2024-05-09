#####################################################################
#
# Perl::Tidy::Tokenizer reads a source and breaks it into a stream of tokens
#
# Usage Outline:
#
#   STEP 1: initialize or re-initialze Tokenizer with user options
#     Perl::Tidy::Tokenizer::check_options($rOpts);
#
#   STEP 2: create a tokenizer for a specific input source object
#     my $tokenizer = Perl::Tidy::Tokenizer->new(
#         source_object      => $source,
#         ...
#     );
#
#   STEP 3: get and process each tokenized 'line' (a hash ref of token info)
#    while ( my $line = $tokenizer->get_line() ) {
#        $formatter->write_line($line);
#    }
#
#   STEP 4: report errors
#    my $severe_error = $tokenizer->report_tokenization_errors();
#
# The source object can be a STRING ref, an ARRAY ref, or an object with a
# get_line() method which supplies one line (a character string) perl call.
#
# NOTE: This is not a real class.  Only one tokenizer my be used.
#
########################################################################

package Perl::Tidy::Tokenizer;
use strict;
use warnings;
use English qw( -no_match_vars );

our $VERSION = '20240511';

use Carp;

use constant DEVEL_MODE   => 0;
use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };

{ #<<< A non-indenting brace to contain all lexical variables

# Parent sequence number of tree of containers; must be 1
use constant SEQ_ROOT => 1;

# Defaults for guessing old indentation
use constant INDENT_COLUMNS_DEFAULT => 4;
use constant TAB_SIZE_DEFAULT       => 8;

# Decimal values of some ascii characters for quick checks
use constant ORD_TAB           => 9;
use constant ORD_SPACE         => 32;
use constant ORD_PRINTABLE_MIN => 33;
use constant ORD_PRINTABLE_MAX => 126;

# A limit on message length when problems are detected
use constant LONG_MESSAGE => 256;

# GLOBAL VARIABLES which change during tokenization:
# These could also be stored in $self but it is more convenient and
# efficient to make them global lexical variables.
# INITIALIZER: sub prepare_for_a_new_file
my (

    $brace_depth,
    $context,
    $current_package,
    $last_nonblank_block_type,
    $last_nonblank_token,
    $last_nonblank_type,
    $next_sequence_number,
    $paren_depth,
    $rbrace_context,
    $rbrace_package,
    $rbrace_structural_type,
    $rbrace_type,
    $rcurrent_depth,
    $rcurrent_sequence_number,
    $rdepth_array,
    $ris_block_function,
    $ris_block_list_function,
    $ris_constant,
    $ris_user_function,
    $rnested_statement_type,
    $rnested_ternary_flag,
    $rparen_semicolon_count,
    $rparen_vars,
    $rparen_type,
    $rsaw_function_definition,
    $rsaw_use_module,
    $rsquare_bracket_structural_type,
    $rsquare_bracket_type,
    $rstarting_line_of_current_depth,
    $rtotal_depth,
    $ruser_function_prototype,
    $square_bracket_depth,
    $statement_type,
    $total_depth,
);

my (

    # GLOBAL CONSTANTS for routines in this package,
    # INITIALIZER: BEGIN block.
    %can_start_digraph,
    %expecting_operator_token,
    %expecting_operator_types,
    %expecting_term_token,
    %expecting_term_types,
    %is_block_operator,
    %is_digraph,
    %is_file_test_operator,
    %is_if_elsif_unless,
    %is_if_elsif_unless_case_when,
    %is_indirect_object_taker,
    %is_keyword_rejecting_question_as_pattern_delimiter,
    %is_keyword_rejecting_slash_as_pattern_delimiter,
    %is_keyword_taking_list,
    %is_keyword_taking_optional_arg,
    %is_q_qq_qw_qx_qr_s_y_tr_m,
    %is_q_qq_qx_qr_s_y_tr_m,
    %quote_modifiers,
    %is_semicolon_or_t,
    %is_sort_map_grep,
    %is_sort_map_grep_eval_do,
    %is_tetragraph,
    %is_trigraph,
    %is_valid_token_type,
    %other_line_endings,
    %really_want_term,
    @closing_brace_names,
    @opening_brace_names,

    # GLOBAL CONSTANT hash lookup table of operator expected values
    # INITIALIZER: BEGIN block
    %op_expected_table,

    # GLOBAL VARIABLES which are constant after being configured.
    # INITIALIZER: BEGIN block and modified by sub check_options
    %is_code_block_token,
    %is_keyword,
    %is_my_our_state,
    %is_package,
    %matching_end_token,

    # INITIALIZER: sub check_options
    $code_skipping_pattern_begin,
    $code_skipping_pattern_end,
    $format_skipping_pattern_begin,
    $format_skipping_pattern_end,

    $rOpts_code_skipping,
    $rOpts_code_skipping_begin,
    $rOpts_format_skipping,
    $rOpts_format_skipping_begin,
    $rOpts_format_skipping_end,
    $rOpts_starting_indentation_level,
    $rOpts_indent_columns,
    $rOpts_look_for_hash_bang,
    $rOpts_look_for_autoloader,
    $rOpts_look_for_selfloader,
    $rOpts_trim_qw,
    $rOpts_extended_syntax,
    $rOpts_continuation_indentation,
    $rOpts_outdent_labels,
    $rOpts_maximum_level_errors,
    $rOpts_maximum_unexpected_errors,
    $rOpts_indent_closing_brace,
    $rOpts_non_indenting_braces,
    $rOpts_non_indenting_brace_prefix,
    $rOpts_whitespace_cycle,

    $tabsize,
    %is_END_DATA_format_sub,
    %is_grep_alias,
    %is_sub,
    $guess_if_method,
);

# possible values of operator_expected()
use constant TERM     => -1;
use constant UNKNOWN  => 0;
use constant OPERATOR => 1;

# possible values of context
use constant SCALAR_CONTEXT  => -1;
use constant UNKNOWN_CONTEXT => 0;
use constant LIST_CONTEXT    => 1;

# Maximum number of little messages; probably need not be changed.
use constant MAX_NAG_MESSAGES => 6;

BEGIN {

    # Array index names for $self.
    # Do not combine with other BEGIN blocks (c101).
    my $i = 0;
    use constant {
        _rhere_target_list_                  => $i++,
        _in_here_doc_                        => $i++,
        _here_doc_target_                    => $i++,
        _here_quote_character_               => $i++,
        _in_data_                            => $i++,
        _in_end_                             => $i++,
        _in_format_                          => $i++,
        _in_error_                           => $i++,
        _in_trouble_                         => $i++,
        _warning_count_                      => $i++,
        _html_tag_count_                     => $i++,
        _in_pod_                             => $i++,
        _in_code_skipping_                   => $i++,
        _in_format_skipping_                 => $i++,
        _in_attribute_list_                  => $i++,
        _in_quote_                           => $i++,
        _quote_target_                       => $i++,
        _line_start_quote_                   => $i++,
        _starting_level_                     => $i++,
        _know_starting_level_                => $i++,
        _last_line_number_                   => $i++,
        _saw_perl_dash_P_                    => $i++,
        _saw_perl_dash_w_                    => $i++,
        _saw_use_strict_                     => $i++,
        _saw_brace_error_                    => $i++,
        _hit_bug_                            => $i++,
        _look_for_autoloader_                => $i++,
        _look_for_selfloader_                => $i++,
        _saw_autoloader_                     => $i++,
        _saw_selfloader_                     => $i++,
        _saw_hash_bang_                      => $i++,
        _saw_end_                            => $i++,
        _saw_data_                           => $i++,
        _saw_negative_indentation_           => $i++,
        _started_tokenizing_                 => $i++,
        _debugger_object_                    => $i++,
        _diagnostics_object_                 => $i++,
        _logger_object_                      => $i++,
        _save_logfile_                       => $i++,
        _unexpected_error_count_             => $i++,
        _started_looking_for_here_target_at_ => $i++,
        _nearly_matched_here_target_at_      => $i++,
        _line_of_text_                       => $i++,
        _rlower_case_labels_at_              => $i++,
        _maximum_level_                      => $i++,
        _true_brace_error_count_             => $i++,
        _rOpts_                              => $i++,
        _rinput_lines_                       => $i++,
        _input_line_index_next_              => $i++,
        _rtrimmed_input_lines_               => $i++,
        _rclosing_brace_indentation_hash_    => $i++,
        _show_indentation_table_             => $i++,
        _rnon_indenting_brace_stack_         => $i++,
    };
} ## end BEGIN

{    ## closure for subs to count instances

    # methods to count instances
    my $_count = 0;
    sub get_count        { return $_count; }
    sub _increment_count { return ++$_count }
    sub _decrement_count { return --$_count }
}

sub DESTROY {
    my $self = shift;
    _decrement_count();
    return;
}

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

sub Die {
    my ($msg) = @_;
    Perl::Tidy::Die($msg);
    croak "unexpected return from Perl::Tidy::Die";
}

sub Fault {
    my ( $self, $msg ) = @_;

    # This routine is called for errors that really should not occur
    # except if there has been a bug introduced by a recent program change.
    # Please add comments at calls to Fault to explain why the call
    # should not occur, and where to look to fix it.
    my ( $package0, $filename0, $line0, $subroutine0 ) = caller(0);
    my ( $package1, $filename1, $line1, $subroutine1 ) = caller(1);
    my ( $package2, $filename2, $line2, $subroutine2 ) = caller(2);
    my $pkg = __PACKAGE__;

    # Catch potential error of Fault not called as a method
    my $input_stream_name;
    if ( !ref($self) ) {
        $msg = "Fault not called as a method - please fix\n";
        if ( $self && length($self) < LONG_MESSAGE ) { $msg .= $self }
        $self              = undef;
        $input_stream_name = "(UNKNOWN)";
    }
    else {
        $input_stream_name = $self->get_input_stream_name();
    }

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

sub make_skipping_pattern {
    my ( $rOpts, $opt_name, $default ) = @_;
    my $param = $rOpts->{$opt_name};
    if ( !$param ) { $param = $default }
    $param =~ s/^\s+//;    # allow leading spaces to be like format-skipping
    if ( $param !~ /^#/ ) {
        Die("ERROR: the $opt_name parameter '$param' must begin with '#'\n");
    }

    # Note that the ending \s will match a newline
    my $pattern = '^\s*' . $param . '\s';
    if ( Perl::Tidy::Formatter::bad_pattern($pattern) ) {
        Die(
"ERROR: the $opt_name parameter '$param' causes the invalid regex '$pattern'\n"
        );
    }
    return $pattern;
} ## end sub make_skipping_pattern

sub check_options {

    # Check Tokenizer parameters
    my $rOpts = shift;

    %is_sub = ();
    $is_sub{'sub'} = 1;

    %is_END_DATA_format_sub = (
        '__END__'  => 1,
        '__DATA__' => 1,
        'format'   => 1,
        'sub'      => 1,
    );

    # Install any aliases to 'sub'
    if ( $rOpts->{'sub-alias-list'} ) {

        # Note that any 'sub-alias-list' has been preprocessed to
        # be a trimmed, space-separated list which includes 'sub'
        # for example, it might be 'sub method fun'
        my @sub_alias_list = split /\s+/, $rOpts->{'sub-alias-list'};
        foreach my $word (@sub_alias_list) {
            $is_sub{$word}                 = 1;
            $is_END_DATA_format_sub{$word} = 1;
        }
    }

    # Set global flag to say if we have to guess if bareword 'method' is
    # a sub when 'method' is in %is_sub. This will be true unless:
    #   (1) the user entered 'method' as sub alias, or
    #   (2) the user set --use-feature=class
    # In these two cases we can assume that 'method' is a sub alias.
    $guess_if_method = 1;
    if ( $is_sub{'method'} ) { $guess_if_method = 0 }

    #------------------------------------------------
    # Update hash values for any -use-feature options
    #------------------------------------------------

    my $use_feature_class = 1;
    if ( $rOpts->{'use-feature'} ) {
        if ( $rOpts->{'use-feature'} =~ /\bnoclass\b/ ) {
            $use_feature_class = 0;
        }
        elsif ( $rOpts->{'use-feature'} =~ /\bclass\b/ ) {
            $guess_if_method = 0;
        }
        else {
            ## neither 'class' nor 'noclass' seen so use default
        }
    }

    # These are the main updates for this option. There are additional
    # changes elsewhere, usually indicated with a comment 'rt145706'

    # Update hash values for use_feature=class, added for rt145706
    # see 'perlclass.pod'

    # IMPORTANT: We are changing global hash values initially set in a BEGIN
    # block.  Values must be defined (true or false) for each of these new
    # words whether true or false. Otherwise, programs using the module which
    # change options between runs (such as test code) will have
    # incorrect settings and fail.

    # There are 4 new keywords:

    # 'class' - treated specially as generalization of 'package'
    # Note: we must not set 'class' to be a keyword to avoid problems
    # with older uses.
    $is_package{'class'} = $use_feature_class;

    # 'method' - treated like sub using the sub-alias-list option
    # Note: we must not set 'method' to be a keyword to avoid problems
    # with older uses.
    if ($use_feature_class) {
        $is_sub{'method'}                 = 1;
        $is_END_DATA_format_sub{'method'} = 1;
    }

    # 'field'  - added as a keyword, and works like 'my'
    $is_keyword{'field'}      = $use_feature_class;
    $is_my_our_state{'field'} = $use_feature_class;

    # 'ADJUST' - added as a keyword and works like 'BEGIN'
    # TODO: if ADJUST gets a paren list, this will need to be updated
    $is_keyword{'ADJUST'}          = $use_feature_class;
    $is_code_block_token{'ADJUST'} = $use_feature_class;

    %is_grep_alias = ();
    if ( $rOpts->{'grep-alias-list'} ) {

        # Note that 'grep-alias-list' has been preprocessed to be a trimmed,
        # space-separated list
        my @q = split /\s+/, $rOpts->{'grep-alias-list'};
        @is_grep_alias{@q} = (1) x scalar(@q);
    }

    $rOpts_starting_indentation_level = $rOpts->{'starting-indentation-level'};
    $rOpts_indent_columns             = $rOpts->{'indent-columns'};
    $rOpts_look_for_hash_bang         = $rOpts->{'look-for-hash-bang'};
    $rOpts_look_for_autoloader        = $rOpts->{'look-for-autoloader'};
    $rOpts_look_for_selfloader        = $rOpts->{'look-for-selfloader'};
    $rOpts_trim_qw                    = $rOpts->{'trim-qw'};
    $rOpts_extended_syntax            = $rOpts->{'extended-syntax'};
    $rOpts_continuation_indentation   = $rOpts->{'continuation-indentation'};
    $rOpts_outdent_labels             = $rOpts->{'outdent-labels'};
    $rOpts_maximum_level_errors       = $rOpts->{'maximum-level-errors'};
    $rOpts_maximum_unexpected_errors  = $rOpts->{'maximum-unexpected-errors'};
    $rOpts_code_skipping              = $rOpts->{'code-skipping'};
    $rOpts_code_skipping_begin        = $rOpts->{'code-skipping-begin'};
    $rOpts_format_skipping            = $rOpts->{'format-skipping'};
    $rOpts_format_skipping_begin      = $rOpts->{'format-skipping-begin'};
    $rOpts_format_skipping_end        = $rOpts->{'format-skipping-end'};
    $rOpts_indent_closing_brace       = $rOpts->{'indent-closing-brace'};
    $rOpts_non_indenting_braces       = $rOpts->{'non-indenting-braces'};
    $rOpts_non_indenting_brace_prefix = $rOpts->{'non-indenting-brace-prefix'};
    $rOpts_whitespace_cycle           = $rOpts->{'whitespace-cycle'};

    # In the Tokenizer, --indent-columns is just used for guessing old
    # indentation, and must be positive.  If -i=0 is used for this run (which
    # is possible) we'll just guess that the old run used 4 spaces per level.
    if ( !$rOpts_indent_columns ) {
        $rOpts_indent_columns = INDENT_COLUMNS_DEFAULT;
    }

    # Define $tabsize, the number of spaces per tab for use in
    # guessing the indentation of source lines with leading tabs.
    # Assume same as for this run if tabs are used, otherwise assume
    # a default value, typically 8
    $tabsize =
        $rOpts->{'entab-leading-whitespace'}
      ? $rOpts->{'entab-leading-whitespace'}
      : $rOpts->{'tabs'} ? $rOpts->{'indent-columns'}
      :                    $rOpts->{'default-tabsize'};
    if ( !$tabsize ) { $tabsize = TAB_SIZE_DEFAULT }

    $code_skipping_pattern_begin =
      make_skipping_pattern( $rOpts, 'code-skipping-begin', '#<<V' );
    $code_skipping_pattern_end =
      make_skipping_pattern( $rOpts, 'code-skipping-end', '#>>V' );

    $format_skipping_pattern_begin =
      make_skipping_pattern( $rOpts, 'format-skipping-begin', '#<<<' );
    $format_skipping_pattern_end =
      make_skipping_pattern( $rOpts, 'format-skipping-end', '#>>>' );

    return;
} ## end sub check_options

sub new {

    my ( $class, @arglist ) = @_;
    if ( @arglist % 2 ) { croak "Odd number of items in arg hash list\n" }

    my %defaults = (
        source_object        => undef,
        debugger_object      => undef,
        diagnostics_object   => undef,
        logger_object        => undef,
        starting_level       => undef,
        starting_line_number => 1,
        rOpts                => {},
    );
    my %args = ( %defaults, @arglist );

    # we are given an object with a get_line() method to supply source lines
    my $source_object = $args{source_object};
    my $rOpts         = $args{rOpts};

    # Check call args
    if ( !defined($source_object) ) {
        Die(
"Perl::Tidy::Tokenizer::new called without a 'source_object' parameter\n"
        );
    }
    if ( !ref($source_object) ) {
        Die(<<EOM);
sub Perl::Tidy::Tokenizer::new received a 'source_object' parameter which is not a reference;
'source_object' must be a reference to a STRING, ARRAY, or object with a 'getline' method
EOM
    }

    my $logger_object = $args{logger_object};

    # Tokenizer state data is as follows:
    # _rhere_target_list_    reference to list of here-doc targets
    # _here_doc_target_      the target string for a here document
    # _here_quote_character_ the type of here-doc quoting (" ' ` or none)
    #                        to determine if interpolation is done
    # _quote_target_         character we seek if chasing a quote
    # _line_start_quote_     line where we started looking for a long quote
    # _in_here_doc_          flag indicating if we are in a here-doc
    # _in_pod_               flag set if we are in pod documentation
    # _in_code_skipping_     flag set if we are in a code skipping section
    # _in_format_skipping_   flag set if we are in a format skipping section
    # _in_error_             flag set if we saw severe error (binary in script)
    # _in_trouble_           set if we saw a troublesome lexical like 'my sub s'
    # _warning_count_        number of calls to logger sub warning
    # _html_tag_count_       number of apparent html tags seen (indicates html)
    # _in_data_              flag set if we are in __DATA__ section
    # _in_end_               flag set if we are in __END__ section
    # _in_format_            flag set if we are in a format description
    # _in_attribute_list_    flag telling if we are looking for attributes
    # _in_quote_             flag telling if we are chasing a quote
    # _starting_level_       indentation level of first line
    # _diagnostics_object_   place to write debugging information
    # _unexpected_error_count_ error count used to limit output
    # _lower_case_labels_at_ line numbers where lower case labels seen
    # _hit_bug_              program bug detected

    my $self = [];
    $self->[_rhere_target_list_]        = [];
    $self->[_in_here_doc_]              = 0;
    $self->[_here_doc_target_]          = EMPTY_STRING;
    $self->[_here_quote_character_]     = EMPTY_STRING;
    $self->[_in_data_]                  = 0;
    $self->[_in_end_]                   = 0;
    $self->[_in_format_]                = 0;
    $self->[_in_error_]                 = 0;
    $self->[_in_trouble_]               = 0;
    $self->[_warning_count_]            = 0;
    $self->[_html_tag_count_]           = 0;
    $self->[_in_pod_]                   = 0;
    $self->[_in_code_skipping_]         = 0;
    $self->[_in_format_skipping_]       = 0;
    $self->[_in_attribute_list_]        = 0;
    $self->[_in_quote_]                 = 0;
    $self->[_quote_target_]             = EMPTY_STRING;
    $self->[_line_start_quote_]         = -1;
    $self->[_starting_level_]           = $args{starting_level};
    $self->[_know_starting_level_]      = defined( $args{starting_level} );
    $self->[_last_line_number_]         = $args{starting_line_number} - 1;
    $self->[_saw_perl_dash_P_]          = 0;
    $self->[_saw_perl_dash_w_]          = 0;
    $self->[_saw_use_strict_]           = 0;
    $self->[_saw_brace_error_]          = 0;
    $self->[_hit_bug_]                  = 0;
    $self->[_look_for_autoloader_]      = $rOpts_look_for_autoloader;
    $self->[_look_for_selfloader_]      = $rOpts_look_for_selfloader;
    $self->[_saw_autoloader_]           = 0;
    $self->[_saw_selfloader_]           = 0;
    $self->[_saw_hash_bang_]            = 0;
    $self->[_saw_end_]                  = 0;
    $self->[_saw_data_]                 = 0;
    $self->[_saw_negative_indentation_] = 0;
    $self->[_started_tokenizing_]       = 0;
    $self->[_debugger_object_]          = $args{debugger_object};
    $self->[_diagnostics_object_]       = $args{diagnostics_object};
    $self->[_logger_object_]            = $logger_object;
    $self->[_unexpected_error_count_]   = 0;
    $self->[_started_looking_for_here_target_at_] = 0;
    $self->[_nearly_matched_here_target_at_]      = undef;
    $self->[_line_of_text_]                       = EMPTY_STRING;
    $self->[_rlower_case_labels_at_]              = undef;
    $self->[_maximum_level_]                      = 0;
    $self->[_true_brace_error_count_]             = 0;
    $self->[_rnon_indenting_brace_stack_]         = [];
    $self->[_show_indentation_table_]             = 0;

    $self->[_rclosing_brace_indentation_hash_] = {
        valid                 => undef,
        rhistory_line_number  => [0],
        rhistory_level_diff   => [0],
        rhistory_anchor_point => [1],
    };

    $self->[_rOpts_] = $rOpts;
    $self->[_save_logfile_] =
      defined($logger_object) && $logger_object->get_save_logfile();

    bless $self, $class;

    $self->prepare_for_a_new_file($source_object);
    $self->find_starting_indentation_level();

    # This is not a full class yet, so die if an attempt is made to
    # create more than one object.

    if ( _increment_count() > 1 ) {
        confess
"Attempt to create more than 1 object in $class, which is not a true class yet\n";
    }

    return $self;

} ## end sub new

# Called externally
sub get_unexpected_error_count {
    my ($self) = @_;
    return $self->[_unexpected_error_count_];
}

# Called externally
sub is_keyword {
    my ($str) = @_;
    return $is_keyword{$str};
}

#----------------------------------------------------------------
# Line input routines, previously handled by the LineBuffer class
#----------------------------------------------------------------
sub make_source_array {

    my ( $self, $line_source_object ) = @_;

    # Convert the source into an array of lines
    my $rinput_lines = [];

    my $rsource = ref($line_source_object);
    my $source_string;

    if ( !$rsource ) {

        # shouldn't happen: this should have been checked in sub new
        $self->Fault(<<EOM);
sub Perl::Tidy::Tokenizer::new received a 'source_object' parameter which is not a reference;
'source_object' must be a reference to a STRING, ARRAY, or object with a 'getline' method
EOM
    }

    # handle an ARRAY ref
    elsif ( $rsource eq 'ARRAY' ) {
        $rinput_lines  = $line_source_object;
        $source_string = join( EMPTY_STRING, @{$line_source_object} );
    }

    # handle a SCALAR ref
    elsif ( $rsource eq 'SCALAR' ) {
        $source_string = ${$line_source_object};
        my @lines = split /^/, $source_string;
        $rinput_lines = \@lines;
    }

    # handle an object - must have a get_line method
    else {

        # This will die if user's object does have a 'get_line' method
        while ( my $line = $line_source_object->get_line() ) {
            push( @{$rinput_lines}, $line );
        }
        $source_string = join( EMPTY_STRING, @{$rinput_lines} );
    }

    # Get trimmed lines. It is much faster to strip leading whitespace from
    # the whole input file at once than line-by-line.

    # Add a terminal newline if needed to keep line count unchanged:
    # - avoids problem of losing a last line which is just \r and no \n  (c283)
    # - but check input line count to avoid adding line to an empty file (c286)
    if ( @{$rinput_lines} && $source_string !~ /\n$/ ) {
        $source_string .= "\n";
    }

    # Remove leading whitespace except newlines
    $source_string =~ s/^ [^\S\n]+ //gxm;

    # Then break the string into lines
    my @trimmed_lines = split /^/, $source_string;

    # Safety check - a change in number of lines would be a disaster
    if ( @trimmed_lines != @{$rinput_lines} ) {

        # Shouldn't happen - die in DEVEL_MODE and fix
        my $ntr = @trimmed_lines;
        my $utr = @{$rinput_lines};
        DEVEL_MODE
          && $self->Fault(
            "trimmed / untrimmed line counts differ: $ntr / $utr\n");

        # Otherwise we can safely continue with undefined trimmed lines.  They
        # will be detected and fixed later.
        @trimmed_lines = ();
    }

    $self->[_rinput_lines_]          = $rinput_lines;
    $self->[_rtrimmed_input_lines_]  = \@trimmed_lines;
    $self->[_input_line_index_next_] = 0;
    return;
} ## end sub make_source_array

sub peek_ahead {
    my ( $self, $buffer_index ) = @_;

    # look $buffer_index lines ahead of the current location without disturbing
    # the input
    my $line;
    my $rinput_lines = $self->[_rinput_lines_];
    my $line_index   = $buffer_index + $self->[_input_line_index_next_];
    if ( $line_index < @{$rinput_lines} ) {
        $line = $rinput_lines->[$line_index];
    }
    return $line;
} ## end sub peek_ahead

#-----------------------------------------
# interface to Perl::Tidy::Logger routines
#-----------------------------------------
sub warning {

    my ( $self, $msg ) = @_;

    my $logger_object = $self->[_logger_object_];
    $self->[_warning_count_]++;
    if ($logger_object) {
        my $msg_line_number = $self->[_last_line_number_];
        $logger_object->warning( $msg, $msg_line_number );
    }
    return;
} ## end sub warning

sub get_input_stream_name {

    my $self = shift;

    my $input_stream_name = EMPTY_STRING;
    my $logger_object     = $self->[_logger_object_];
    if ($logger_object) {
        $input_stream_name = $logger_object->get_input_stream_name();
    }
    return $input_stream_name;
} ## end sub get_input_stream_name

sub complain {

    my ( $self, $msg ) = @_;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        my $input_line_number = $self->[_last_line_number_];
        $logger_object->complain( $msg, $input_line_number );
    }
    return;
} ## end sub complain

sub write_logfile_entry {

    my ( $self, $msg ) = @_;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->write_logfile_entry($msg);
    }
    return;
} ## end sub write_logfile_entry

sub interrupt_logfile {

    my $self = shift;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->interrupt_logfile();
    }
    return;
} ## end sub interrupt_logfile

sub resume_logfile {

    my $self = shift;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->resume_logfile();
    }
    return;
} ## end sub resume_logfile

sub brace_warning {
    my ( $self, $msg ) = @_;
    $self->[_saw_brace_error_]++;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        my $msg_line_number = $self->[_last_line_number_];
        $logger_object->brace_warning( $msg, $msg_line_number );
    }
    return;
} ## end sub brace_warning

sub increment_brace_error {

    # This is same as sub brace_warning but without a message
    my $self = shift;
    $self->[_saw_brace_error_]++;

    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->increment_brace_error();
    }
    return;
} ## end sub increment_brace_error

sub get_saw_brace_error {
    my $self = shift;
    return $self->[_saw_brace_error_];
}

sub report_definite_bug {
    my $self = shift;
    $self->[_hit_bug_] = 1;
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
    return;
} ## end sub report_definite_bug

#-------------------------------------
# Interface to Perl::Tidy::Diagnostics
#-------------------------------------
sub write_diagnostics {
    my ( $self, $msg ) = @_;
    my $input_line_number  = $self->[_last_line_number_];
    my $diagnostics_object = $self->[_diagnostics_object_];
    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics( $msg, $input_line_number );
    }
    return;
} ## end sub write_diagnostics

sub report_tokenization_errors {

    my ($self) = @_;

    # Report any tokenization errors and return a flag '$severe_error'.
    # Set $severe_error = 1 if the tokenization errors are so severe that
    # the formatter should not attempt to format the file. Instead, it will
    # just output the file verbatim.

    # set severe error flag if tokenizer has encountered file reading problems
    # (i.e. unexpected binary characters)
    # or code which may not be formatted correctly (such as 'my sub q')
    # The difference between _in_error_ and _in_trouble_ is that
    # _in_error_ stops the tokenizer immediately whereas
    # _in_trouble_ lets the tokenizer finish so that all errors are seen
    # Both block formatting and cause the input stream to be output verbatim.
    my $severe_error = $self->[_in_error_] || $self->[_in_trouble_];

    # And do not format if it looks like an html file (c209)
    $severe_error ||= $self->[_html_tag_count_] && $self->[_warning_count_];

    # Inform the logger object on length of input stream
    my $logger_object = $self->[_logger_object_];
    if ($logger_object) {
        my $last_line_number = $self->[_last_line_number_];
        $logger_object->set_last_input_line_number($last_line_number);
    }

    my $maxle = $rOpts_maximum_level_errors;
    my $maxue = $rOpts_maximum_unexpected_errors;
    $maxle = 1 unless defined($maxle);
    $maxue = 0 unless defined($maxue);

    my $level = get_indentation_level();
    if ( $level != $self->[_starting_level_] ) {
        $self->warning("final indentation level: $level\n");

        $self->[_show_indentation_table_] = 1;

        my $level_diff = $self->[_starting_level_] - $level;
        if ( $level_diff < 0 ) { $level_diff = -$level_diff }

        # Set severe error flag if the level error is greater than 1.
        # The formatter can function for any level error but it is probably
        # best not to attempt formatting for a high level error.
        if ( $maxle >= 0 && $level_diff > $maxle ) {
            $severe_error = 1;
            $self->warning(<<EOM);
Formatting will be skipped because level error '$level_diff' exceeds -maxle=$maxle; use -maxle=-1 to force formatting
EOM
        }
    }

    $self->check_final_nesting_depths();

    if ( $self->[_show_indentation_table_] ) {
        $self->show_indentation_table();
    }

    # Likewise, large numbers of brace errors usually indicate non-perl
    # scripts, so set the severe error flag at a low number.  This is similar
    # to the level check, but different because braces may balance but be
    # incorrectly interlaced.
    if ( $self->[_true_brace_error_count_] > 2 ) {
        $severe_error = 1;
    }

    if ( $rOpts_look_for_hash_bang
        && !$self->[_saw_hash_bang_] )
    {
        $self->warning(
            "hit EOF without seeing hash-bang line; maybe don't need -x?\n");
    }

    if ( $self->[_in_format_] ) {
        $self->warning("hit EOF while in format description\n");
    }

    if ( $self->[_in_code_skipping_] ) {
        $self->write_logfile_entry(
            "hit EOF while in lines skipped with --code-skipping\n");
    }

    if ( $self->[_in_pod_] ) {

        # Just write log entry if this is after __END__ or __DATA__
        # because this happens to often, and it is not likely to be
        # a parsing error.
        if ( $self->[_saw_data_] || $self->[_saw_end_] ) {
            $self->write_logfile_entry(
"hit eof while in pod documentation (no =cut seen)\n\tthis can cause trouble with some pod utilities\n"
            );
        }

        else {
            $self->complain(
"hit eof while in pod documentation (no =cut seen)\n\tthis can cause trouble with some pod utilities\n"
            );
        }

    }

    if ( $self->[_in_here_doc_] ) {
        $severe_error = 1;
        my $here_doc_target = $self->[_here_doc_target_];
        my $started_looking_for_here_target_at =
          $self->[_started_looking_for_here_target_at_];
        if ($here_doc_target) {
            $self->warning(
"hit EOF in here document starting at line $started_looking_for_here_target_at with target: $here_doc_target\n"
            );
        }
        else {
            $self->warning(<<EOM);
Hit EOF in here document starting at line $started_looking_for_here_target_at with empty target string.
  (Perl will match to the end of file but this may not be intended).
EOM
        }
        my $nearly_matched_here_target_at =
          $self->[_nearly_matched_here_target_at_];
        if ($nearly_matched_here_target_at) {
            $self->warning(
"NOTE: almost matched at input line $nearly_matched_here_target_at except for whitespace\n"
            );
        }
    }

    # Something is seriously wrong if we ended inside a quote
    if ( $self->[_in_quote_] ) {
        $severe_error = 1;
        my $line_start_quote = $self->[_line_start_quote_];
        my $quote_target     = $self->[_quote_target_];
        my $what =
          ( $self->[_in_attribute_list_] )
          ? "attribute list"
          : "quote/pattern";
        $self->warning(
"hit EOF seeking end of $what starting at line $line_start_quote ending in $quote_target\n"
        );
    }

    if ( $self->[_hit_bug_] ) {
        $severe_error = 1;
    }

    # Multiple "unexpected" type tokenization errors usually indicate parsing
    # non-perl scripts, or that something is seriously wrong, so we should
    # avoid formatting them.  This can happen for example if we run perltidy on
    # a shell script or an html file.  But unfortunately this check can
    # interfere with some extended syntaxes, such as RPerl, so it has to be off
    # by default.
    my $ue_count = $self->[_unexpected_error_count_];
    if ( $maxue > 0 && $ue_count > $maxue ) {
        $self->warning(<<EOM);
Formatting will be skipped since unexpected token count = $ue_count > -maxue=$maxue; use -maxue=0 to force formatting
EOM
        $severe_error = 1;
    }

    if ( !$self->[_saw_perl_dash_w_] ) {
        $self->write_logfile_entry("Suggest including 'use warnings;'\n");
    }

    if ( $self->[_saw_perl_dash_P_] ) {
        $self->write_logfile_entry(
            "Use of -P parameter for defines is discouraged\n");
    }

    if ( !$self->[_saw_use_strict_] ) {
        $self->write_logfile_entry("Suggest including 'use strict;'\n");
    }

    # it is suggested that labels have at least one upper case character
    # for legibility and to avoid code breakage as new keywords are introduced
    if ( $self->[_rlower_case_labels_at_] ) {
        my @lower_case_labels_at = @{ $self->[_rlower_case_labels_at_] };
        $self->write_logfile_entry(
            "Suggest using upper case characters in label(s)\n");
        local $LIST_SEPARATOR = ')(';
        $self->write_logfile_entry(
            "  defined at line(s): (@lower_case_labels_at)\n");
    }
    return $severe_error;
} ## end sub report_tokenization_errors

sub show_indentation_table {
    my ($self) = @_;

    # Output indentation table made at closing braces.  This can be helpful for
    # the case of a missing brace in a previously formatted file.

    # skip if -wc is used (rare); it is too complex to use
    return if ($rOpts_whitespace_cycle);

    # skip if non-indenting-brace-prefix (very rare, but could be fixed)
    return if ($rOpts_non_indenting_brace_prefix);

    # skip if starting level is not zero (probably in editor)
    return if ($rOpts_starting_indentation_level);

    # skip if indentation analysis is not valid
    my $rhash = $self->[_rclosing_brace_indentation_hash_];
    return if ( !$rhash->{valid} );

    my $rhistory_line_number  = $rhash->{rhistory_line_number};
    my $rhistory_level_diff   = $rhash->{rhistory_level_diff};
    my $rhistory_anchor_point = $rhash->{rhistory_anchor_point};

    # Remove the first artificial point from the table
    shift @{$rhistory_line_number};
    shift @{$rhistory_level_diff};
    shift @{$rhistory_anchor_point};

    # Remove dubious points at an anchor point = 2 and beyond
    # These can occur when non-indenting braces are used
    my $num_his = @{$rhistory_level_diff};
    foreach my $i ( 0 .. $num_his - 1 ) {
        if ( $rhistory_anchor_point->[$i] == 2 ) {
            $num_his = $i;
            last;
        }
    }
    return if ( $num_his <= 1 );

    # Ignore an ending non-anchor point
    if ( !$rhistory_anchor_point->[-1] ) {
        $num_his -= 1;
    }

    # Ignore an ending point which is the same as the previous point
    if ( $num_his > 1 ) {
        if ( $rhistory_level_diff->[ $num_his - 1 ] ==
            $rhistory_level_diff->[ $num_his - 2 ] )
        {
            $num_his -= 1;
        }
    }

    # skip if the table does not have at least 2 points to pinpoint an error
    return if ( $num_his <= 1 );

    # skip if first point shows a level error - the analysis may not be valid
    return if ( $rhistory_level_diff->[0] );

    # Since the table could be arbitrarily large, we will limit the table to N
    # lines.  If there are more lines than that, we will show N-3 lines, then
    # ..., then the last 2 lines. Allow about 3 lines per error, so a table
    # limit of 10 can localize up to about 3 errors in a file.
    my $nlines_max   = 10;
    my @pre_indexes  = ( 0 .. $num_his - 1 );
    my @post_indexes = ();
    if ( @pre_indexes > $nlines_max ) {
        if ( $nlines_max >= 5 ) {
            @pre_indexes  = ( 0 .. $nlines_max - 4 );
            @post_indexes = ( $num_his - 2, $num_his - 1 );
        }
        else {
            @pre_indexes = ( 0 .. $nlines_max - 1 );
        }
    }

    my @output_lines;
    push @output_lines, <<EOM;
Table of nesting level differences at closing braces.
This might help localize brace errors if the file was previously formatted.
line:  (brace level) - (level expected from old indentation)
EOM
    foreach my $i (@pre_indexes) {
        my $lno  = $rhistory_line_number->[$i];
        my $diff = $rhistory_level_diff->[$i];
        push @output_lines, <<EOM;
$lno: $diff
EOM
    }
    if (@post_indexes) {
        push @output_lines, "...\n";
        foreach my $i (@post_indexes) {
            my $lno  = $rhistory_line_number->[$i];
            my $diff = $rhistory_level_diff->[$i];
            push @output_lines, <<EOM;
$lno: $diff
EOM
        }
    }
    push @output_lines, "\n";
    my $output_str = join EMPTY_STRING, @output_lines;

    $self->interrupt_logfile();
    $self->warning($output_str);
    $self->resume_logfile();

    return;
} ## end sub show_indentation_table

sub report_v_string {

    # warn if this version can't handle v-strings
    my ( $self, $tok ) = @_;
    if ( $] < 5.006 ) {
        $self->warning(
"Found v-string '$tok' but v-strings are not implemented in your version of perl; see Camel 3 book ch 2\n"
        );
    }
    return;
} ## end sub report_v_string

sub is_valid_token_type {
    my ($type) = @_;
    return $is_valid_token_type{$type};
}

sub log_numbered_msg {
    my ( $self, $msg ) = @_;

    # write input line number + message to logfile
    my $input_line_number = $self->[_last_line_number_];
    $self->write_logfile_entry("Line $input_line_number: $msg");
    return;
} ## end sub log_numbered_msg

sub get_line {

    my $self = shift;

    # Read the next input line and tokenize it
    # Returns:
    #   $line_of_tokens = ref to hash of info for the tokenized line

    # USES GLOBAL VARIABLES:
    #   $brace_depth, $square_bracket_depth, $paren_depth

    # get the next line from the input array
    my $input_line;
    my $trimmed_input_line;
    my $line_index   = $self->[_input_line_index_next_];
    my $rinput_lines = $self->[_rinput_lines_];
    if ( $line_index < @{$rinput_lines} ) {
        $trimmed_input_line = $self->[_rtrimmed_input_lines_]->[$line_index];
        $input_line         = $rinput_lines->[ $line_index++ ];
        $self->[_input_line_index_next_] = $line_index;
    }

    $self->[_line_of_text_] = $input_line;

    return if ( !defined($input_line) );

    my $input_line_number = ++$self->[_last_line_number_];

    # Find and remove what characters terminate this line, including any
    # control r
    my $input_line_separator = EMPTY_STRING;
    if ( chomp($input_line) ) {
        $input_line_separator = $INPUT_RECORD_SEPARATOR;
    }

    # The first test here very significantly speeds things up, but be sure to
    # keep the regex and hash %other_line_endings the same.
    if ( $other_line_endings{ substr( $input_line, -1 ) } ) {
        if ( $input_line =~ s/([\r\035\032])+$// ) {
            $input_line_separator = $1 . $input_line_separator;

            # This could make the trimmed input line incorrect, so the
            # safe thing to do is to make it undef to force it to be
            # recomputed later.
            $trimmed_input_line = undef;
        }
    }

    # For backwards compatibility we keep the line text terminated with
    # a newline character
    $input_line .= "\n";
    $self->[_line_of_text_] = $input_line;

    # create a data structure describing this line which will be
    # returned to the caller.

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
    #   SKIP           - code skipping section
    #   SKIP_END       - last line of code skipping section, '#>>V'
    #   DATA_START     - __DATA__ line
    #   DATA           - unidentified text following __DATA__
    #   END_START      - __END__ line
    #   END            - unidentified text following __END__
    #   ERROR          - we are in big trouble, probably not a perl script

    # Other variables:
    #   _curly_brace_depth     - depth of curly braces at start of line
    #   _square_bracket_depth  - depth of square brackets at start of line
    #   _paren_depth           - depth of parens at start of line
    #   _starting_in_quote     - this line continues a multi-line quote
    #                            (so don't trim leading blanks!)
    #   _ending_in_quote       - this line ends in a multi-line quote
    #                            (so don't trim trailing blanks!)
    my $line_of_tokens = {
        _line_type                 => 'EOF',
        _line_text                 => $input_line,
        _line_number               => $input_line_number,
        _guessed_indentation_level => 0,
        _curly_brace_depth         => $brace_depth,
        _square_bracket_depth      => $square_bracket_depth,
        _paren_depth               => $paren_depth,
## Skip these needless initializations for efficiency:
##      _rtoken_type               => undef,
##      _rtokens                   => undef,
##      _rlevels                   => undef,
##      _rblock_type               => undef,
##      _rtype_sequence            => undef,
##      _starting_in_quote         => 0,
##      _ending_in_quote           => 0,
    };

    # must print line unchanged if we are in a here document
    if ( $self->[_in_here_doc_] ) {

        $line_of_tokens->{_line_type} = 'HERE';
        my $here_doc_target      = $self->[_here_doc_target_];
        my $here_quote_character = $self->[_here_quote_character_];
        my $candidate_target     = $input_line;
        chomp $candidate_target;

        # Handle <<~ targets, which are indicated here by a leading space on
        # the here quote character
        if ( $here_quote_character =~ /^\s/ ) {
            $candidate_target =~ s/^\s+//;
        }
        if ( $candidate_target eq $here_doc_target ) {
            $self->[_nearly_matched_here_target_at_] = undef;
            $line_of_tokens->{_line_type} = 'HERE_END';
            $self->log_numbered_msg("Exiting HERE document $here_doc_target\n");

            my $rhere_target_list = $self->[_rhere_target_list_];
            if ( @{$rhere_target_list} ) {  # there can be multiple here targets
                ( $here_doc_target, $here_quote_character ) =
                  @{ shift @{$rhere_target_list} };
                $self->[_here_doc_target_]      = $here_doc_target;
                $self->[_here_quote_character_] = $here_quote_character;
                $self->log_numbered_msg(
                    "Entering HERE document $here_doc_target\n");
                $self->[_nearly_matched_here_target_at_] = undef;
                $self->[_started_looking_for_here_target_at_] =
                  $input_line_number;
            }
            else {
                $self->[_in_here_doc_]          = 0;
                $self->[_here_doc_target_]      = EMPTY_STRING;
                $self->[_here_quote_character_] = EMPTY_STRING;
            }
        }

        # check for error of extra whitespace
        # note for PERL6: leading whitespace is allowed
        else {
            $candidate_target =~ s/^ \s+ | \s+ $//gx;    # trim both ends
            if ( $candidate_target eq $here_doc_target ) {
                $self->[_nearly_matched_here_target_at_] = $input_line_number;
            }
        }
        return $line_of_tokens;
    }

    # Print line unchanged if we are in a format section
    elsif ( $self->[_in_format_] ) {

        if ( $input_line =~ /^\.[\s#]*$/ ) {

            # Decrement format depth count at a '.' after a 'format'
            $self->[_in_format_]--;

            # This is the end when count reaches 0
            if ( !$self->[_in_format_] ) {
                $self->log_numbered_msg("Exiting format section\n");
                $line_of_tokens->{_line_type} = 'FORMAT_END';

                # Make the tokenizer mark an opening brace which follows
                # as a code block. Fixes issue c202/t032.
                $last_nonblank_token = ';';
                $last_nonblank_type  = ';';
            }
        }
        else {
            $line_of_tokens->{_line_type} = 'FORMAT';
            if ( $input_line =~ /^\s*format\s+\w+/ ) {

                # Increment format depth count at a 'format' within a 'format'
                # This is a simple way to handle nested formats (issue c019).
                $self->[_in_format_]++;
            }
        }
        return $line_of_tokens;
    }

    # must print line unchanged if we are in pod documentation
    elsif ( $self->[_in_pod_] ) {

        $line_of_tokens->{_line_type} = 'POD';
        if ( $input_line =~ /^=cut/ ) {
            $line_of_tokens->{_line_type} = 'POD_END';
            $self->log_numbered_msg("Exiting POD section\n");
            $self->[_in_pod_] = 0;
        }
        if ( $input_line =~ /^\#\!.*perl\b/ && !$self->[_in_end_] ) {
            $self->warning(
                "Hash-bang in pod can cause older versions of perl to fail! \n"
            );
        }

        return $line_of_tokens;
    }

    # print line unchanged if in skipped section
    elsif ( $self->[_in_code_skipping_] ) {

        $line_of_tokens->{_line_type} = 'SKIP';
        if ( $input_line =~ /$code_skipping_pattern_end/ ) {
            $line_of_tokens->{_line_type} = 'SKIP_END';
            $self->log_numbered_msg("Exiting code-skipping section\n");
            $self->[_in_code_skipping_] = 0;
        }
        elsif ( $input_line =~ /$code_skipping_pattern_begin/ ) {

            # warn of duplicate starting comment lines, git #118
            my $lno = $self->[_in_code_skipping_];
            $self->warning(
                "Already in code-skipping section which started at line $lno\n"
            );
        }
        else {
            # not a code-skipping control line
        }
        return $line_of_tokens;
    }

    # must print line unchanged if we have seen a severe error (i.e., we
    # are seeing illegal tokens and cannot continue.  Syntax errors do
    # not pass this route).  Calling routine can decide what to do, but
    # the default can be to just pass all lines as if they were after __END__
    elsif ( $self->[_in_error_] ) {
        $line_of_tokens->{_line_type} = 'ERROR';
        return $line_of_tokens;
    }

    # print line unchanged if we are __DATA__ section
    elsif ( $self->[_in_data_] ) {

        # ...but look for POD
        # Note that the _in_data and _in_end flags remain set
        # so that we return to that state after seeing the
        # end of a pod section
        if ( $input_line =~ /^=(\w+)\b/ && $1 ne 'cut' ) {
            $line_of_tokens->{_line_type} = 'POD_START';
            $self->log_numbered_msg("Entering POD section\n");
            $self->[_in_pod_] = 1;
            return $line_of_tokens;
        }
        else {
            $line_of_tokens->{_line_type} = 'DATA';
            return $line_of_tokens;
        }
    }

    # print line unchanged if we are in __END__ section
    elsif ( $self->[_in_end_] ) {

        # ...but look for POD
        # Note that the _in_data and _in_end flags remain set
        # so that we return to that state after seeing the
        # end of a pod section
        if ( $input_line =~ /^=(\w+)\b/ && $1 ne 'cut' ) {
            $line_of_tokens->{_line_type} = 'POD_START';
            $self->log_numbered_msg("Entering POD section\n");
            $self->[_in_pod_] = 1;
            return $line_of_tokens;
        }
        else {
            $line_of_tokens->{_line_type} = 'END';
            return $line_of_tokens;
        }
    }
    else {
        # not a special control line
    }

    # check for a hash-bang line if we haven't seen one
    if (   !$self->[_saw_hash_bang_]
        && substr( $input_line, 0, 2 ) eq '#!'
        && $input_line =~ /^\#\!.*perl\b/ )
    {
        $self->[_saw_hash_bang_] = $input_line_number;

        # check for -w and -P flags
        if ( $input_line =~ /^\#\!.*perl\s.*-.*P/ ) {
            $self->[_saw_perl_dash_P_] = 1;
        }

        if ( $input_line =~ /^\#\!.*perl\s.*-.*w/ ) {
            $self->[_saw_perl_dash_w_] = 1;
        }

        if (
            $input_line_number > 1

            # leave any hash bang in a BEGIN block alone
            # i.e. see 'debugger-duck_type.t'
            && !(
                   $last_nonblank_block_type
                && $last_nonblank_block_type eq 'BEGIN'
            )
            && !$rOpts_look_for_hash_bang

            # Try to avoid giving a false alarm at a simple comment.
            # These look like valid hash-bang lines:

            #!/usr/bin/perl -w
            #!   /usr/bin/perl -w
            #!c:\perl\bin\perl.exe

            # These are comments:
            #! I love perl
            #!  sunos does not yet provide a /usr/bin/perl

            # Comments typically have multiple spaces, which suggests
            # the filter
            && $input_line =~ /^\#\!(\s+)?(\S+)?perl/
          )
        {

            # this is helpful for VMS systems; we may have accidentally
            # tokenized some DCL commands
            if ( $self->[_started_tokenizing_] ) {
                $self->warning(
"There seems to be a hash-bang after line 1; do you need to run with -x ?\n"
                );
            }
            else {
                $self->complain("Useless hash-bang after line 1\n");
            }
        }

        # Report the leading hash-bang as a system line
        # This will prevent -dac from deleting it
        else {
            $line_of_tokens->{_line_type} = 'SYSTEM';
            return $line_of_tokens;
        }
    }

    # wait for a hash-bang before parsing if the user invoked us with -x
    if ( $rOpts_look_for_hash_bang
        && !$self->[_saw_hash_bang_] )
    {
        $line_of_tokens->{_line_type} = 'SYSTEM';
        return $line_of_tokens;
    }

    # a first line of the form ': #' will be marked as SYSTEM
    # since lines of this form may be used by tcsh
    if ( $input_line_number == 1 && $input_line =~ /^\s*\:\s*\#/ ) {
        $line_of_tokens->{_line_type} = 'SYSTEM';
        return $line_of_tokens;
    }

    # now we know that it is ok to tokenize the line...
    # the line tokenizer will modify any of these private variables:
    #        _rhere_target_list_
    #        _in_data_
    #        _in_end_
    #        _in_format_
    #        _in_error_
    #        _in_code_skipping_
    #        _in_format_skipping_
    #        _in_pod_
    #        _in_quote_

    $self->tokenize_this_line( $line_of_tokens, $trimmed_input_line );

    # Now finish defining the return structure and return it
    $line_of_tokens->{_ending_in_quote} = $self->[_in_quote_];

    # handle severe error (binary data in script)
    if ( $self->[_in_error_] ) {
        $self->[_in_quote_] = 0;    # to avoid any more messages
        $self->warning("Giving up after error\n");
        $line_of_tokens->{_line_type} = 'ERROR';
        reset_indentation_level(0);    # avoid error messages
        return $line_of_tokens;
    }

    # handle start of pod documentation
    if ( $self->[_in_pod_] ) {

        # This gets tricky..above a __DATA__ or __END__ section, perl
        # accepts '=cut' as the start of pod section. But afterwards,
        # only pod utilities see it and they may ignore an =cut without
        # leading =head.  In any case, this isn't good.
        if ( $input_line =~ /^=cut\b/ ) {
            if ( $self->[_saw_data_] || $self->[_saw_end_] ) {
                $self->complain("=cut while not in pod ignored\n");
                $self->[_in_pod_] = 0;
                $line_of_tokens->{_line_type} = 'POD_END';
            }
            else {
                $line_of_tokens->{_line_type} = 'POD_START';
                if ( !DEVEL_MODE ) {
                    $self->warning(
"=cut starts a pod section .. this can fool pod utilities.\n"
                    );
                }
                $self->log_numbered_msg("Entering POD section\n");
            }
        }

        else {
            $line_of_tokens->{_line_type} = 'POD_START';
            $self->log_numbered_msg("Entering POD section\n");
        }

        return $line_of_tokens;
    }

    # handle start of skipped section
    if ( $self->[_in_code_skipping_] ) {

        $line_of_tokens->{_line_type} = 'SKIP';
        $self->log_numbered_msg("Entering code-skipping section\n");
        return $line_of_tokens;
    }

    # see if this line contains here doc targets
    my $rhere_target_list = $self->[_rhere_target_list_];
    if ( @{$rhere_target_list} ) {

        my ( $here_doc_target, $here_quote_character ) =
          @{ shift @{$rhere_target_list} };
        $self->[_in_here_doc_]          = 1;
        $self->[_here_doc_target_]      = $here_doc_target;
        $self->[_here_quote_character_] = $here_quote_character;
        $self->log_numbered_msg("Entering HERE document $here_doc_target\n");
        $self->[_started_looking_for_here_target_at_] = $input_line_number;
    }

    # NOTE: __END__ and __DATA__ statements are written unformatted
    # because they can theoretically contain additional characters
    # which are not tokenized (and cannot be read with <DATA> either!).
    if ( $self->[_in_data_] ) {
        $line_of_tokens->{_line_type} = 'DATA_START';
        $self->log_numbered_msg("Starting __DATA__ section\n");
        $self->[_saw_data_] = 1;

        # keep parsing after __DATA__ if use SelfLoader was seen
        if ( $self->[_saw_selfloader_] ) {
            $self->[_in_data_] = 0;
            $self->log_numbered_msg(
                "SelfLoader seen, continuing; -nlsl deactivates\n");
        }

        return $line_of_tokens;
    }

    elsif ( $self->[_in_end_] ) {
        $line_of_tokens->{_line_type} = 'END_START';
        $self->log_numbered_msg("Starting __END__ section\n");
        $self->[_saw_end_] = 1;

        # keep parsing after __END__ if use AutoLoader was seen
        if ( $self->[_saw_autoloader_] ) {
            $self->[_in_end_] = 0;
            $self->log_numbered_msg(
                "AutoLoader seen, continuing; -nlal deactivates\n");
        }
        return $line_of_tokens;
    }
    else {
        # not in __END__ or __DATA__
    }

    # now, finally, we know that this line is type 'CODE'
    $line_of_tokens->{_line_type} = 'CODE';

    # remember if we have seen any real code
    if (  !$self->[_started_tokenizing_]
        && $input_line !~ /^\s*$/
        && $input_line !~ /^\s*#/ )
    {
        $self->[_started_tokenizing_] = 1;
    }

    if ( $self->[_debugger_object_] ) {
        $self->[_debugger_object_]->write_debug_entry($line_of_tokens);
    }

    # Note: if keyword 'format' occurs in this line code, it is still CODE
    # (keyword 'format' need not start a line)
    if ( $self->[_in_format_] ) {
        $self->log_numbered_msg("Entering format section\n");
    }

    if ( $self->[_in_quote_]
        and ( $self->[_line_start_quote_] < 0 ) )
    {
        if ( ( my $quote_target = $self->[_quote_target_] ) !~ /^\s*$/ ) {
            $self->[_line_start_quote_] = $input_line_number;
            $self->log_numbered_msg(
                "Start multi-line quote or pattern ending in $quote_target\n");
        }
    }
    elsif ( ( $self->[_line_start_quote_] >= 0 )
        && !$self->[_in_quote_] )
    {
        $self->[_line_start_quote_] = -1;
        $self->log_numbered_msg("End of multi-line quote or pattern\n");
    }
    else {
        # not at the edge of a quote
    }

    # we are returning a line of CODE
    return $line_of_tokens;
} ## end sub get_line

sub find_starting_indentation_level {

    # We need to find the indentation level of the first line of the
    # script being formatted.  Often it will be zero for an entire file,
    # but if we are formatting a local block of code (within an editor for
    # example) it may not be zero.  The user may specify this with the
    # -sil=n parameter but normally doesn't so we have to guess.
    #
    my ($self) = @_;
    my $starting_level = 0;

    # use value if given as parameter
    if ( $self->[_know_starting_level_] ) {
        $starting_level = $self->[_starting_level_];
    }

    # if we know there is a hash_bang line, the level must be zero
    elsif ($rOpts_look_for_hash_bang) {
        $self->[_know_starting_level_] = 1;
    }

    # otherwise figure it out from the input file
    else {
        my $line;
        my $i = 0;

        # keep looking at lines until we find a hash bang or piece of code
        # ( or, for now, an =pod line)
        my $msg = EMPTY_STRING;
        my $in_code_skipping;
        while ( $line = $self->peek_ahead( $i++ ) ) {

            # if first line is #! then assume starting level is zero
            if ( $i == 1 && $line =~ /^\#\!/ ) {
                $starting_level = 0;
                last;
            }

            # ignore lines fenced off with code-skipping comments
            if ( $line =~ /^\s*#/ ) {
                if ( !$in_code_skipping ) {
                    if (   $rOpts_code_skipping
                        && $line =~ /$code_skipping_pattern_begin/ )
                    {
                        $in_code_skipping = 1;
                        next;
                    }
                }
                else {
                    if ( $line =~ /$code_skipping_pattern_end/ ) {
                        $in_code_skipping = 0;
                    }
                    next;
                }

                # Note that we could also ignore format-skipping lines here
                # but it isn't clear if that would be best.
                # See c326 for example code.

                next;
            }
            next if ($in_code_skipping);

            next if ( $line =~ /^\s*$/ );    # skip past blank lines

            $starting_level = $self->guess_old_indentation_level($line);
            last;
        }
        $msg = "Line $i implies starting-indentation-level = $starting_level\n";
        $self->write_logfile_entry("$msg");
    }
    $self->[_starting_level_] = $starting_level;
    reset_indentation_level($starting_level);
    return;
} ## end sub find_starting_indentation_level

sub guess_old_indentation_level {
    my ( $self, $line ) = @_;

    # Guess the indentation level of an input line.
    #
    # For the first line of code this result will define the starting
    # indentation level.  It will mainly be non-zero when perltidy is applied
    # within an editor to a local block of code.
    #
    # This is an impossible task in general because we can't know what tabs
    # meant for the old script and how many spaces were used for one
    # indentation level in the given input script.  For example it may have
    # been previously formatted with -i=7 -et=3.  But we can at least try to
    # make sure that perltidy guesses correctly if it is applied repeatedly to
    # a block of code within an editor, so that the block stays at the same
    # level when perltidy is applied repeatedly.
    #
    # USES GLOBAL VARIABLES: (none)
    my $level = 0;

    # find leading tabs, spaces, and any statement label
    my $spaces = 0;
    if ( $line =~ /^(\t+)?(\s+)?(\w+:[^:])?/ ) {

        # If there are leading tabs, we use the tab scheme for this run, if
        # any, so that the code will remain stable when editing.
        if ($1) { $spaces += length($1) * $tabsize }

        if ($2) { $spaces += length($2) }

        # correct for outdented labels
        if (   $3
            && $rOpts_outdent_labels
            && $rOpts_continuation_indentation > 0 )
        {
            $spaces += $rOpts_continuation_indentation;
        }
    }

    $level = int( $spaces / $rOpts_indent_columns );
    return ($level);
} ## end sub guess_old_indentation_level

# This is a currently unused debug routine
sub dump_functions {

    my $fh = *STDOUT;
    foreach my $pkg ( keys %{$ris_user_function} ) {
        $fh->print("\nnon-constant subs in package $pkg\n");

        foreach my $sub ( keys %{ $ris_user_function->{$pkg} } ) {
            my $msg = EMPTY_STRING;
            if ( $ris_block_list_function->{$pkg}{$sub} ) {
                $msg = 'block_list';
            }

            if ( $ris_block_function->{$pkg}{$sub} ) {
                $msg = 'block';
            }
            $fh->print("$sub $msg\n");
        }
    }

    foreach my $pkg ( keys %{$ris_constant} ) {
        $fh->print("\nconstants and constant subs in package $pkg\n");

        foreach my $sub ( keys %{ $ris_constant->{$pkg} } ) {
            $fh->print("$sub\n");
        }
    }
    return;
} ## end sub dump_functions

sub prepare_for_a_new_file {

    my ( $self, $source_object ) = @_;

    # copy the source object lines to an array of lines
    $self->make_source_array($source_object);

    # previous tokens needed to determine what to expect next
    $last_nonblank_token      = ';';    # the only possible starting state which
    $last_nonblank_type       = ';';    # will make a leading brace a code block
    $last_nonblank_block_type = EMPTY_STRING;

    # scalars for remembering statement types across multiple lines
    $statement_type = EMPTY_STRING;     # '' or 'use' or 'sub..' or 'case..'

    # scalars for remembering where we are in the file
    $current_package = "main";
    $context         = UNKNOWN_CONTEXT;

    # hashes used to remember function information
    $ris_constant             = {};     # user-defined constants
    $ris_user_function        = {};     # user-defined functions
    $ruser_function_prototype = {};     # their prototypes
    $ris_block_function       = {};
    $ris_block_list_function  = {};
    $rsaw_function_definition = {};
    $rsaw_use_module          = {};

    # variables used to track depths of various containers
    # and report nesting errors
    $paren_depth              = 0;
    $brace_depth              = 0;
    $square_bracket_depth     = 0;
    $rcurrent_depth           = [ (0) x scalar @closing_brace_names ];
    $total_depth              = 0;
    $rtotal_depth             = [];
    $rcurrent_sequence_number = [];
    $next_sequence_number     = SEQ_ROOT + 1;

    $rparen_type                     = [];
    $rparen_semicolon_count          = [];
    $rparen_vars                     = [];
    $rbrace_type                     = [];
    $rbrace_structural_type          = [];
    $rbrace_context                  = [];
    $rbrace_package                  = [];
    $rsquare_bracket_type            = [];
    $rsquare_bracket_structural_type = [];
    $rdepth_array                    = [];
    $rnested_ternary_flag            = [];
    $rnested_statement_type          = [];
    $rstarting_line_of_current_depth = [];

    $rparen_type->[$paren_depth]            = EMPTY_STRING;
    $rparen_semicolon_count->[$paren_depth] = 0;
    $rparen_vars->[$paren_depth]            = [];
    $rbrace_type->[$brace_depth] = ';';   # identify opening brace as code block
    $rbrace_structural_type->[$brace_depth]        = EMPTY_STRING;
    $rbrace_context->[$brace_depth]                = UNKNOWN_CONTEXT;
    $rbrace_package->[$paren_depth]                = $current_package;
    $rsquare_bracket_type->[$square_bracket_depth] = EMPTY_STRING;
    $rsquare_bracket_structural_type->[$square_bracket_depth] = EMPTY_STRING;

    initialize_tokenizer_state();
    return;
} ## end sub prepare_for_a_new_file

{    ## closure for sub tokenize_this_line

    use constant BRACE          => 0;
    use constant SQUARE_BRACKET => 1;
    use constant PAREN          => 2;
    use constant QUESTION_COLON => 3;

    # TV1: scalars for processing one LINE.
    # Re-initialized on each entry to sub tokenize_this_line.
    my (
        $block_type,        $container_type,    $expecting,
        $i,                 $i_tok,             $input_line,
        $input_line_number, $last_nonblank_i,   $max_token_index,
        $next_tok,          $next_type,         $peeked_ahead,
        $prototype,         $rhere_target_list, $rtoken_map,
        $rtoken_type,       $rtokens,           $tok,
        $type,              $type_sequence,     $indent_flag,
    );

    # TV2: refs to ARRAYS for processing one LINE
    # Re-initialized on each call.
    my $routput_token_list    = [];    # stack of output token indexes
    my $routput_token_type    = [];    # token types
    my $routput_block_type    = [];    # types of code block
    my $routput_type_sequence = [];    # nesting sequential number
    my $routput_indent_flag   = [];    #

    # TV3: SCALARS for quote variables.  These are initialized with a
    # subroutine call and continually updated as lines are processed.
    my ( $in_quote, $quote_type, $quote_character, $quote_pos, $quote_depth,
        $quoted_string_1, $quoted_string_2, $allowed_quote_modifiers );

    # TV4: SCALARS for multi-line identifiers and
    # statements. These are initialized with a subroutine call
    # and continually updated as lines are processed.
    my ( $id_scan_state, $identifier, $want_paren );

    # TV5: SCALARS for tracking indentation level.
    # Initialized once and continually updated as lines are
    # processed.
    my (
        $nesting_token_string, $nesting_block_string,
        $nesting_block_flag,   $level_in_tokenizer,
    );

    # TV6: SCALARS for remembering several previous
    # tokens. Initialized once and continually updated as
    # lines are processed.
    my (
        $last_nonblank_container_type, $last_nonblank_type_sequence,
        $last_last_nonblank_token,     $last_last_nonblank_type,
        $last_nonblank_prototype,
    );

    # ----------------------------------------------------------------
    # beginning of tokenizer variable access and manipulation routines
    # ----------------------------------------------------------------

    sub initialize_tokenizer_state {

        # GV1: initialized once
        # TV1: initialized on each call
        # TV2: initialized on each call
        # TV3:
        $in_quote                = 0;
        $quote_type              = 'Q';
        $quote_character         = EMPTY_STRING;
        $quote_pos               = 0;
        $quote_depth             = 0;
        $quoted_string_1         = EMPTY_STRING;
        $quoted_string_2         = EMPTY_STRING;
        $allowed_quote_modifiers = EMPTY_STRING;

        # TV4:
        $id_scan_state = EMPTY_STRING;
        $identifier    = EMPTY_STRING;
        $want_paren    = EMPTY_STRING;

        # TV5:
        $nesting_token_string = EMPTY_STRING;
        $nesting_block_string = '1';            # initially in a block
        $nesting_block_flag   = 1;
        $level_in_tokenizer   = 0;

        # TV6:
        $last_nonblank_container_type = EMPTY_STRING;
        $last_nonblank_type_sequence  = EMPTY_STRING;
        $last_last_nonblank_token     = ';';
        $last_last_nonblank_type      = ';';
        $last_nonblank_prototype      = EMPTY_STRING;
        return;
    } ## end sub initialize_tokenizer_state

    sub save_tokenizer_state {

        # Global variables:
        my $rGV1 = [
            $brace_depth,
            $context,
            $current_package,
            $last_nonblank_block_type,
            $last_nonblank_token,
            $last_nonblank_type,
            $next_sequence_number,
            $paren_depth,
            $rbrace_context,
            $rbrace_package,
            $rbrace_structural_type,
            $rbrace_type,
            $rcurrent_depth,
            $rcurrent_sequence_number,
            $rdepth_array,
            $ris_block_function,
            $ris_block_list_function,
            $ris_constant,
            $ris_user_function,
            $rnested_statement_type,
            $rnested_ternary_flag,
            $rparen_semicolon_count,
            $rparen_vars,
            $rparen_type,
            $rsaw_function_definition,
            $rsaw_use_module,
            $rsquare_bracket_structural_type,
            $rsquare_bracket_type,
            $rstarting_line_of_current_depth,
            $rtotal_depth,
            $ruser_function_prototype,
            $square_bracket_depth,
            $statement_type,
            $total_depth,

        ];

        # Tokenizer closure variables:
        my $rTV1 = [
            $block_type,        $container_type,    $expecting,
            $i,                 $i_tok,             $input_line,
            $input_line_number, $last_nonblank_i,   $max_token_index,
            $next_tok,          $next_type,         $peeked_ahead,
            $prototype,         $rhere_target_list, $rtoken_map,
            $rtoken_type,       $rtokens,           $tok,
            $type,              $type_sequence,     $indent_flag,
        ];

        my $rTV2 = [
            $routput_token_list, $routput_token_type,
            $routput_block_type, $routput_type_sequence,
            $routput_indent_flag,
        ];

        my $rTV3 = [
            $in_quote,        $quote_type,
            $quote_character, $quote_pos,
            $quote_depth,     $quoted_string_1,
            $quoted_string_2, $allowed_quote_modifiers,
        ];

        my $rTV4 = [ $id_scan_state, $identifier, $want_paren ];

        my $rTV5 = [
            $nesting_token_string, $nesting_block_string,
            $nesting_block_flag,   $level_in_tokenizer,
        ];

        my $rTV6 = [
            $last_nonblank_container_type, $last_nonblank_type_sequence,
            $last_last_nonblank_token,     $last_last_nonblank_type,
            $last_nonblank_prototype,
        ];
        return [ $rGV1, $rTV1, $rTV2, $rTV3, $rTV4, $rTV5, $rTV6 ];
    } ## end sub save_tokenizer_state

    sub restore_tokenizer_state {
        my ($rstate) = @_;
        my ( $rGV1, $rTV1, $rTV2, $rTV3, $rTV4, $rTV5, $rTV6 ) = @{$rstate};

        (
            $brace_depth,
            $context,
            $current_package,
            $last_nonblank_block_type,
            $last_nonblank_token,
            $last_nonblank_type,
            $next_sequence_number,
            $paren_depth,
            $rbrace_context,
            $rbrace_package,
            $rbrace_structural_type,
            $rbrace_type,
            $rcurrent_depth,
            $rcurrent_sequence_number,
            $rdepth_array,
            $ris_block_function,
            $ris_block_list_function,
            $ris_constant,
            $ris_user_function,
            $rnested_statement_type,
            $rnested_ternary_flag,
            $rparen_semicolon_count,
            $rparen_vars,
            $rparen_type,
            $rsaw_function_definition,
            $rsaw_use_module,
            $rsquare_bracket_structural_type,
            $rsquare_bracket_type,
            $rstarting_line_of_current_depth,
            $rtotal_depth,
            $ruser_function_prototype,
            $square_bracket_depth,
            $statement_type,
            $total_depth,

        ) = @{$rGV1};

        (
            $block_type,        $container_type,    $expecting,
            $i,                 $i_tok,             $input_line,
            $input_line_number, $last_nonblank_i,   $max_token_index,
            $next_tok,          $next_type,         $peeked_ahead,
            $prototype,         $rhere_target_list, $rtoken_map,
            $rtoken_type,       $rtokens,           $tok,
            $type,              $type_sequence,     $indent_flag,
        ) = @{$rTV1};

        (
            $routput_token_list, $routput_token_type,
            $routput_block_type, $routput_type_sequence,
            $routput_indent_flag,
        ) = @{$rTV2};

        (
            $in_quote, $quote_type, $quote_character, $quote_pos, $quote_depth,
            $quoted_string_1, $quoted_string_2, $allowed_quote_modifiers,
        ) = @{$rTV3};

        ( $id_scan_state, $identifier, $want_paren ) = @{$rTV4};

        (
            $nesting_token_string, $nesting_block_string,
            $nesting_block_flag,   $level_in_tokenizer,
        ) = @{$rTV5};

        (
            $last_nonblank_container_type, $last_nonblank_type_sequence,
            $last_last_nonblank_token,     $last_last_nonblank_type,
            $last_nonblank_prototype,
        ) = @{$rTV6};
        return;
    } ## end sub restore_tokenizer_state

    sub split_pretoken {

        my ( $self, $numc ) = @_;

     # Split the leading $numc characters from the current token (at index=$i)
     # which is pre-type 'w' and insert the remainder back into the pretoken
     # stream with appropriate settings.  Since we are splitting a pre-type 'w',
     # there are three cases, depending on if the remainder starts with a digit:
     # Case 1: remainder is type 'd', all digits
     # Case 2: remainder is type 'd' and type 'w': digits and other characters
     # Case 3: remainder is type 'w'

        # Examples, for $numc=1:
        #   $tok    => $tok_0 $tok_1 $tok_2
        #   'x10'   => 'x'    '10'                # case 1
        #   'x10if' => 'x'    '10'   'if'         # case 2
        #   '0ne    => 'O'            'ne'        # case 3

        # where:
        #   $tok_1 is a possible string of digits (pre-type 'd')
        #   $tok_2 is a possible word (pre-type 'w')

        # return 1 if successful
        # return undef if error (shouldn't happen)

        # Calling routine should update '$type' and '$tok' if successful.

        my $pretoken = $rtokens->[$i];
        if (   $pretoken
            && length($pretoken) > $numc
            && substr( $pretoken, $numc ) =~ /^(\d*)(.*)$/ )
        {

            # Split $tok into up to 3 tokens:
            my $tok_0 = substr( $pretoken, 0, $numc );
            my $tok_1 = defined($1) ? $1 : EMPTY_STRING;
            my $tok_2 = defined($2) ? $2 : EMPTY_STRING;

            my $len_0 = length($tok_0);
            my $len_1 = length($tok_1);
            my $len_2 = length($tok_2);

            my $pre_type_0 = 'w';
            my $pre_type_1 = 'd';
            my $pre_type_2 = 'w';

            my $pos_0 = $rtoken_map->[$i];
            my $pos_1 = $pos_0 + $len_0;
            my $pos_2 = $pos_1 + $len_1;

            my $isplice = $i + 1;

            # Splice in any digits
            if ($len_1) {
                splice @{$rtoken_map},  $isplice, 0, $pos_1;
                splice @{$rtokens},     $isplice, 0, $tok_1;
                splice @{$rtoken_type}, $isplice, 0, $pre_type_1;
                $max_token_index++;
                $isplice++;
            }

            # Splice in any trailing word
            if ($len_2) {
                splice @{$rtoken_map},  $isplice, 0, $pos_2;
                splice @{$rtokens},     $isplice, 0, $tok_2;
                splice @{$rtoken_type}, $isplice, 0, $pre_type_2;
                $max_token_index++;
            }

            $rtokens->[$i] = $tok_0;
            return 1;
        }

        # Shouldn't get here - bad call parameters
        if (DEVEL_MODE) {
            $self->Fault(<<EOM);
While working near line number $input_line_number, bad arg '$tok' passed to sub split_pretoken()
EOM
        }
        return;
    } ## end sub split_pretoken

    sub get_indentation_level {
        return $level_in_tokenizer;
    }

    sub reset_indentation_level {
        $level_in_tokenizer = shift;
        return;
    }

    sub peeked_ahead {
        my $flag = shift;

        # get/set the closure flag '$peeked_ahead'
        # - set $peeked_ahead to $flag if given, then
        # - return current value
        $peeked_ahead = defined($flag) ? $flag : $peeked_ahead;
        return $peeked_ahead;
    } ## end sub peeked_ahead

    # ------------------------------------------------------------
    # end of tokenizer variable access and manipulation routines
    # ------------------------------------------------------------

    #------------------------------
    # beginning of tokenizer hashes
    #------------------------------

    my %matching_start_token = ( '}' => '{', ']' => '[', ')' => '(' );

    # These block types terminate statements and do not need a trailing
    # semicolon
    # patched for SWITCH/CASE/
    my %is_zero_continuation_block_type;
    my @q;
    @q = qw( } { BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue ;
      if elsif else unless while until for foreach switch case given when);
    @is_zero_continuation_block_type{@q} = (1) x scalar(@q);

    my %is_logical_container;
    @q = qw(if elsif unless while and or err not && !  || for foreach);
    @is_logical_container{@q} = (1) x scalar(@q);

    my %is_binary_type;
    @q = qw(|| &&);
    @is_binary_type{@q} = (1) x scalar(@q);

    my %is_binary_keyword;
    @q = qw(and or err eq ne cmp);
    @is_binary_keyword{@q} = (1) x scalar(@q);

    # 'L' is token for opening { at hash key
    my %is_opening_type;
    @q = qw< L { ( [ >;
    @is_opening_type{@q} = (1) x scalar(@q);

    my %is_opening_or_ternary_type;
    push @q, '?';
    @is_opening_or_ternary_type{@q} = (1) x scalar(@q);

    # 'R' is token for closing } at hash key
    my %is_closing_type;
    @q = qw< R } ) ] >;
    @is_closing_type{@q} = (1) x scalar(@q);

    my %is_closing_or_ternary_type;
    push @q, ':';
    @is_closing_or_ternary_type{@q} = (1) x scalar(@q);

    my %is_redo_last_next_goto;
    @q = qw(redo last next goto);
    @is_redo_last_next_goto{@q} = (1) x scalar(@q);

    my %is_use_require;
    @q = qw(use require);
    @is_use_require{@q} = (1) x scalar(@q);

    # This hash holds the array index in $self for these keywords:
    # Fix for issue c035: removed 'format' from this hash
    my %is_END_DATA = (
        '__END__'  => _in_end_,
        '__DATA__' => _in_data_,
    );

    my %is_list_end_type;
    @q = qw( ; { } );
    push @q, ',';
    @is_list_end_type{@q} = (1) x scalar(@q);

    # table showing how many quoted things to look for after quote operator..
    # s, y, tr have 2 (pattern and replacement)
    # others have 1 (pattern only)
    my %quote_items = (
        's'  => 2,
        'y'  => 2,
        'tr' => 2,
        'm'  => 1,
        'qr' => 1,
        'q'  => 1,
        'qq' => 1,
        'qw' => 1,
        'qx' => 1,
    );

    my %is_for_foreach;
    @q = qw(for foreach);
    @is_for_foreach{@q} = (1) x scalar(@q);

    # These keywords may introduce blocks after parenthesized expressions,
    # in the form:
    # keyword ( .... ) { BLOCK }
    # patch for SWITCH/CASE: added 'switch' 'case' 'given' 'when'
    # NOTE for --use-feature=class: if ADJUST blocks eventually take a
    # parameter list, then ADJUST might need to be added to this list (see
    # perlclass.pod)
    my %is_blocktype_with_paren;
    @q =
      qw(if elsif unless while until for foreach switch case given when catch);
    @is_blocktype_with_paren{@q} = (1) x scalar(@q);

    my %is_case_default;
    @q = qw(case default);
    @is_case_default{@q} = (1) x scalar(@q);

    #------------------------
    # end of tokenizer hashes
    #------------------------

    # ------------------------------------------------------------
    # beginning of various scanner interface routines
    # ------------------------------------------------------------
    sub scan_replacement_text {

        # check for here-docs in replacement text invoked by
        # a substitution operator with executable modifier 'e'.
        #
        # given:
        #  $replacement_text
        # return:
        #  $rht = reference to any here-doc targets
        my ( $self, $replacement_text ) = @_;

        # quick check
        return if ( $replacement_text !~ /<</ );

        $self->write_logfile_entry(
            "scanning replacement text for here-doc targets\n");

        # save the logger object for error messages
        my $logger_object = $self->[_logger_object_];

        # save all lexical variables
        my $rstate = save_tokenizer_state();
        _decrement_count();    # avoid error check for multiple tokenizers

        # make a new tokenizer
        my $tokenizer = Perl::Tidy::Tokenizer->new(
            source_object        => \$replacement_text,
            logger_object        => $logger_object,
            starting_line_number => $input_line_number,
        );

        # scan the replacement text
        while ( $tokenizer->get_line() ) { }

        # remove any here doc targets
        my $rht = undef;
        if ( $tokenizer->[_in_here_doc_] ) {
            $rht = [];
            push @{$rht},
              [
                $tokenizer->[_here_doc_target_],
                $tokenizer->[_here_quote_character_]
              ];
            if ( $tokenizer->[_rhere_target_list_] ) {
                push @{$rht}, @{ $tokenizer->[_rhere_target_list_] };
                $tokenizer->[_rhere_target_list_] = undef;
            }
            $tokenizer->[_in_here_doc_] = undef;
        }

        # now its safe to report errors
        my $severe_error = $tokenizer->report_tokenization_errors();

        # TODO: Could propagate a severe error up

        # restore all tokenizer lexical variables
        restore_tokenizer_state($rstate);

        # return the here doc targets
        return $rht;
    } ## end sub scan_replacement_text

    sub scan_bare_identifier {
        my $self = shift;
        ( $i, $tok, $type, $prototype ) = $self->scan_bare_identifier_do(

            $input_line,
            $i,
            $tok,
            $type,
            $prototype,
            $rtoken_map,
            $max_token_index
        );
        return;
    } ## end sub scan_bare_identifier

    sub scan_identifier {

        my $self = shift;

        (

            $i,
            $tok,
            $type,
            $id_scan_state,
            $identifier,
            my $split_pretoken_flag

        ) = $self->scan_complex_identifier(

            $i,
            $id_scan_state,
            $identifier,
            $rtokens,
            $max_token_index,
            $expecting,
            $rparen_type->[$paren_depth]
        );

        # Check for signal to fix a special variable adjacent to a keyword,
        # such as '$^One$0'.
        if ($split_pretoken_flag) {

            # Try to fix it by splitting the pretoken
            if (   $i > 0
                && $rtokens->[ $i - 1 ] eq '^'
                && $self->split_pretoken(1) )
            {
                $identifier = substr( $identifier, 0, 3 );
                $tok        = $identifier;
            }
            else {

                # This shouldn't happen ...
                my $var    = substr( $tok, 0, 3 );
                my $excess = substr( $tok, 3 );
                $self->interrupt_logfile();
                $self->warning(<<EOM);
$input_line_number: Trouble parsing at characters '$excess' after special variable '$var'.
A space may be needed after '$var'.
EOM
                $self->resume_logfile();
            }
        }
        return;
    } ## end sub scan_identifier

    use constant VERIFY_FASTSCAN => 0;
    my %fast_scan_context;

    BEGIN {
        %fast_scan_context = (
            '$' => SCALAR_CONTEXT,
            '*' => SCALAR_CONTEXT,
            '@' => LIST_CONTEXT,
            '%' => LIST_CONTEXT,
            '&' => UNKNOWN_CONTEXT,
        );
    } ## end BEGIN

    sub scan_simple_identifier {

        # This is a wrapper for sub scan_identifier. It does a fast preliminary
        # scan for certain common identifiers:
        #   '$var', '@var', %var, *var, &var, '@{...}', '%{...}'
        # If it does not find one of these, or this is a restart, it calls the
        # original scanner directly.

        # This gives the same results as the full scanner in about 1/4 the
        # total runtime for a typical input stream.

        # Notation:
        #     $var * 2
        #     ^^   ^
        #     ||  |
        #     ||  ---- $i_next [= next nonblank pretoken ]
        #     |----$i_plus_1 [= a bareword ]
        #     ---$i_begin [= a sigil]

        my $self = shift;

        my $i_begin   = $i;
        my $tok_begin = $tok;
        my $i_plus_1  = $i + 1;
        my $fast_scan_type;

        #-------------------------------------------------------
        # Do full scan for anything following a pointer, such as
        #      $cref->&*;    # a postderef
        #-------------------------------------------------------
        if ( $last_nonblank_token eq '->' ) {

        }

        #------------------------------
        # quick scan with leading sigil
        #------------------------------
        elsif ( !$id_scan_state
            && $i_plus_1 <= $max_token_index
            && $fast_scan_context{$tok} )
        {
            $context = $fast_scan_context{$tok};

            # look for $var, @var, ...
            if ( $rtoken_type->[$i_plus_1] eq 'w' ) {
                my $pretype_next = EMPTY_STRING;
                if ( $i_plus_1 < $max_token_index ) {
                    my $i_next = $i_plus_1 + 1;
                    if (   $rtoken_type->[$i_next] eq 'b'
                        && $i_next < $max_token_index )
                    {
                        $i_next += 1;
                    }
                    $pretype_next = $rtoken_type->[$i_next];
                }
                if ( $pretype_next ne ':' && $pretype_next ne "'" ) {

                    # Found type 'i' like '$var', '@var', or '%var'
                    $identifier     = $tok . $rtokens->[$i_plus_1];
                    $tok            = $identifier;
                    $type           = 'i';
                    $i              = $i_plus_1;
                    $fast_scan_type = $type;
                }
            }

            # Look for @{ or %{  .
            # But we must let the full scanner handle things ${ because it may
            # keep going to get a complete identifier like '${#}'  .
            elsif (
                $rtoken_type->[$i_plus_1] eq '{'
                && (   $tok_begin eq '@'
                    || $tok_begin eq '%' )
              )
            {

                $identifier     = $tok;
                $type           = 't';
                $fast_scan_type = $type;
            }
            else {
                ## out of tricks
            }
        }

        #---------------------------
        # Quick scan with leading ->
        # Look for ->[ and ->{
        #---------------------------
        elsif (
               $tok eq '->'
            && $i < $max_token_index
            && (   $rtokens->[$i_plus_1] eq '{'
                || $rtokens->[$i_plus_1] eq '[' )
          )
        {
            $type           = $tok;
            $fast_scan_type = $type;
            $identifier     = $tok;
            $context        = UNKNOWN_CONTEXT;
        }
        else {
            ## out of tricks
        }

        #--------------------------------------
        # Verify correctness during development
        #--------------------------------------
        if ( VERIFY_FASTSCAN && $fast_scan_type ) {

            # We will call the full method
            my $identifier_simple = $identifier;
            my $tok_simple        = $tok;
            my $i_simple          = $i;
            my $context_simple    = $context;

            $tok = $tok_begin;
            $i   = $i_begin;
            $self->scan_identifier();

            if (   $tok ne $tok_simple
                || $type ne $fast_scan_type
                || $i != $i_simple
                || $identifier ne $identifier_simple
                || $id_scan_state
                || $context ne $context_simple )
            {
                print {*STDERR} <<EOM;
scan_simple_identifier differs from scan_identifier:
simple:  i=$i_simple, tok=$tok_simple, type=$fast_scan_type, ident=$identifier_simple, context='$context_simple
full:    i=$i, tok=$tok, type=$type, ident=$identifier, context='$context state=$id_scan_state
EOM
            }
        }

        #-------------------------------------------------
        # call full scanner if fast method did not succeed
        #-------------------------------------------------
        if ( !$fast_scan_type ) {
            $self->scan_identifier();
        }
        return;
    } ## end sub scan_simple_identifier

    sub method_ok_here {

        # Return:
        #   false if this is definitely an invalid method declaration
        #   true otherwise (even if not sure)

        # We are trying to avoid problems with old uses of 'method'
        # when --use-feature=class is set (rt145706).
        # For example, this should cause a return of 'false':

        #  method paint => sub {
        #    return;
        #  };

        my $self = shift;

        # from do_scan_sub:
        my $i_beg   = $i + 1;
        my $pos_beg = $rtoken_map->[$i_beg];
        pos($input_line) = $pos_beg;

        # TEST 1: look a valid sub NAME
        if (
            $input_line =~ m{\G\s*
        ((?:\w*(?:'|::))*)  # package - something that ends in :: or '
        (\w+)               # NAME    - required
        }gcx
          )
        {
            # For possible future use..
            my $subname = $2;
            my $package = $1 ? $1 : EMPTY_STRING;
        }
        else {
            return;
        }

        # TEST 2: look for invalid characters after name, such as here:
        #    method paint => sub {
        #     ...
        #    }
        my $next_char = EMPTY_STRING;
        if ( $input_line =~ m/\s*(\S)/gcx ) { $next_char = $1 }
        if ( !$next_char || $next_char eq '#' ) {
            ( $next_char, my $i_next ) =
              $self->find_next_nonblank_token( $max_token_index,
                $rtokens, $max_token_index );
        }

        if ( !$next_char ) {

            # out of characters - give up
            return;
        }

        # Possibly valid next token types:
        # '(' could start prototype or signature
        # ':' could start ATTRIBUTE
        # '{' cold start BLOCK
        # ';' or '}' could end a statement
        if ( $next_char !~ /^[\(\:\{\;\}]/ ) {

            # This does not match use feature 'class' syntax
            return;
        }

        # We will stop here and assume that this is valid syntax for
        # use feature 'class'.
        return 1;
    } ## end sub method_ok_here

    sub class_ok_here {

        # Return:
        #   false if this is definitely an invalid class declaration
        #   true otherwise (even if not sure)

        # We are trying to avoid problems with old uses of 'class'
        # when --use-feature=class is set (rt145706).  We look ahead
        # see if this use of 'class' is obviously inconsistent with
        # the syntax of use feature 'class'.  This allows the default
        # setting --use-feature=class to work for old syntax too.

        # Valid class declarations look like
        #   class NAME ?ATTRS ?VERSION ?BLOCK
        # where ATTRS VERSION and BLOCK are optional

        # For example, this should produce a return of 'false':
        #
        #   class ExtendsBasicAttributes is BasicAttributes{

        my $self = shift;

        # TEST 1: class stmt can only go where a new statment can start
        if ( !new_statement_ok() ) { return }

        my $i_beg   = $i + 1;
        my $pos_beg = $rtoken_map->[$i_beg];
        pos($input_line) = $pos_beg;

        # TEST 2: look for a valid NAME
        if (
            $input_line =~ m{\G\s*
        ((?:\w*(?:'|::))*)  # package - something that ends in :: or '
        (\w+)               # NAME    - required
        }gcx
          )
        {
            # For possible future use..
            my $subname = $2;
            my $package = $1 ? $1 : EMPTY_STRING;
        }
        else {
            return;
        }

        # TEST 3: look for valid characters after NAME
        my $next_char = EMPTY_STRING;
        if ( $input_line =~ m/\s*(\S)/gcx ) { $next_char = $1 }
        if ( !$next_char || $next_char eq '#' ) {
            ( $next_char, my $i_next ) =
              $self->find_next_nonblank_token( $max_token_index,
                $rtokens, $max_token_index );
        }
        if ( !$next_char ) {

            # out of characters - give up
            return;
        }

        # Must see one of: ATTRIBUTE, VERSION, BLOCK, or end stmt

        # Possibly valid next token types:
        # ':' could start ATTRIBUTE
        # '\d' could start VERSION
        # '{' cold start BLOCK
        # ';' could end a statement
        # '}' could end statement but would be strange

        if ( $next_char !~ /^[\:\d\{\;\}]/ ) {

            # This does not match use feature 'class' syntax
            return;
        }

        # We will stop here and assume that this is valid syntax for
        # use feature 'class'.
        return 1;
    } ## end sub class_ok_here

    sub scan_id {
        my $self = shift;
        ( $i, $tok, $type, $id_scan_state ) = $self->scan_id_do(

            $input_line,
            $i, $tok,
            $rtokens,
            $rtoken_map,
            $id_scan_state,
            $max_token_index
        );
        return;
    } ## end sub scan_id

    sub scan_number {
        my $self = shift;
        my $number;
        ( $i, $type, $number ) =
          $self->scan_number_do( $input_line, $i, $rtoken_map, $type,
            $max_token_index );
        return $number;
    } ## end sub scan_number

    use constant VERIFY_FASTNUM => 0;

    sub scan_number_fast {

        # This is a wrapper for sub scan_number. It does a fast preliminary
        # scan for a simple integer.  It calls the original scan_number if it
        # does not find one.

        my $self      = shift;
        my $i_begin   = $i;
        my $tok_begin = $tok;
        my $number;

        #---------------------------------
        # Quick check for (signed) integer
        #---------------------------------

        # This will be the string of digits:
        my $i_d   = $i;
        my $tok_d = $tok;
        my $typ_d = $rtoken_type->[$i_d];

        # check for signed integer
        my $sign = EMPTY_STRING;
        if (   $typ_d ne 'd'
            && ( $typ_d eq '+' || $typ_d eq '-' )
            && $i_d < $max_token_index )
        {
            $sign = $tok_d;
            $i_d++;
            $tok_d = $rtokens->[$i_d];
            $typ_d = $rtoken_type->[$i_d];
        }

        # Handle integers
        if (
            $typ_d eq 'd'
            && (
                $i_d == $max_token_index
                || (   $i_d < $max_token_index
                    && $rtoken_type->[ $i_d + 1 ] ne '.'
                    && $rtoken_type->[ $i_d + 1 ] ne 'w' )
            )
          )
        {
            # Let let full scanner handle multi-digit integers beginning with
            # '0' because there could be error messages.  For example, '009' is
            # not a valid number.

            if ( $tok_d eq '0' || substr( $tok_d, 0, 1 ) ne '0' ) {
                $number = $sign . $tok_d;
                $type   = 'n';
                $i      = $i_d;
            }
        }

        #--------------------------------------
        # Verify correctness during development
        #--------------------------------------
        if ( VERIFY_FASTNUM && defined($number) ) {

            # We will call the full method
            my $type_simple   = $type;
            my $i_simple      = $i;
            my $number_simple = $number;

            $tok    = $tok_begin;
            $i      = $i_begin;
            $number = $self->scan_number();

            if (   $type ne $type_simple
                || ( $i != $i_simple && $i <= $max_token_index )
                || $number ne $number_simple )
            {
                print {*STDERR} <<EOM;
scan_number_fast differs from scan_number:
simple:  i=$i_simple, type=$type_simple, number=$number_simple
full:  i=$i, type=$type, number=$number
EOM
            }
        }

        #----------------------------------------
        # call full scanner if may not be integer
        #----------------------------------------
        if ( !defined($number) ) {
            $number = $self->scan_number();
        }
        return $number;
    } ## end sub scan_number_fast

    # a sub to warn if token found where term expected
    sub error_if_expecting_TERM {
        my $self = shift;
        if ( $expecting == TERM ) {
            if ( $really_want_term{$last_nonblank_type} ) {
                $self->report_unexpected(
                    {
                        found           => $tok,
                        expecting       => "term",
                        i_tok           => $i_tok,
                        last_nonblank_i => $last_nonblank_i,
                        rpretoken_map   => $rtoken_map,
                        rpretoken_type  => $rtoken_type,
                        input_line      => $input_line,
                    }
                );
                return 1;
            }
        }
        return;
    } ## end sub error_if_expecting_TERM

    # a sub to warn if token found where operator expected
    sub error_if_expecting_OPERATOR {
        my ( $self, $thing ) = @_;

        # Issue warning on error if expecting operator
        # Given: $thing = the unexpected token or issue
        #               = undef to use current pre-token

        if ( $expecting == OPERATOR ) {
            if ( !defined($thing) ) { $thing = $tok }
            $self->report_unexpected(
                {
                    found           => $thing,
                    expecting       => "operator",
                    i_tok           => $i_tok,
                    last_nonblank_i => $last_nonblank_i,
                    rpretoken_map   => $rtoken_map,
                    rpretoken_type  => $rtoken_type,
                    input_line      => $input_line,
                }
            );
            if ( $i_tok == 0 ) {
                $self->interrupt_logfile();
                $self->warning("Missing ';' or ',' above?\n");
                $self->resume_logfile();
            }
            return 1;
        }
        return;
    } ## end sub error_if_expecting_OPERATOR

    # ------------------------------------------------------------
    # end scanner interfaces
    # ------------------------------------------------------------

    #------------------
    # Tokenization subs
    #------------------
    sub do_GREATER_THAN_SIGN {

        my $self = shift;

        # '>'
        $self->error_if_expecting_TERM()
          if ( $expecting == TERM );
        return;
    } ## end sub do_GREATER_THAN_SIGN

    sub do_VERTICAL_LINE {

        my $self = shift;

        # '|'
        $self->error_if_expecting_TERM()
          if ( $expecting == TERM );
        return;
    } ## end sub do_VERTICAL_LINE

    # An identifier in possible indirect object location followed by any of
    # these tokens: -> , ; } (plus others) is not an indirect object. Fix c257.
    my %Z_test_hash;

    BEGIN {
        my @qZ = qw#
          -> ; } ) ]
          => =~ = == !~ || >= != *= .. && |= .= -= += <= %=
          ^= &&= ||= //= <=>
          #;
        push @qZ, ',';
        @Z_test_hash{@qZ} = (1) x scalar(@qZ);
    }

    sub do_DOLLAR_SIGN {

        my $self = shift;

        # '$'
        # start looking for a scalar
        $self->error_if_expecting_OPERATOR("Scalar")
          if ( $expecting == OPERATOR );
        $self->scan_simple_identifier();

        if ( $identifier eq '$^W' ) {
            $self->[_saw_perl_dash_w_] = 1;
        }

        # Check for identifier in indirect object slot
        # (vorboard.pl, sort.t).  Something like:
        #   /^(print|printf|sort|exec|system)$/
        if (
               $is_indirect_object_taker{$last_nonblank_token}
            && $last_nonblank_type eq 'k'
            || ( ( $last_nonblank_token eq '(' )
                && $is_indirect_object_taker{ $rparen_type->[$paren_depth] } )
            || (   $last_nonblank_type eq 'w'
                || $last_nonblank_type eq 'U' )    # possible object
          )
        {

            # An identifier followed by '->' is not indirect object;
            # fixes b1175, b1176. Fix c257: Likewise for other tokens like
            # comma, semicolon, closing brace, and single space.
            my ( $next_nonblank_token, $i_next ) =
              $self->find_next_noncomment_token( $i, $rtokens,
                $max_token_index );
            $type = 'Z' if ( !$Z_test_hash{$next_nonblank_token} );
        }
        return;
    } ## end sub do_DOLLAR_SIGN

    sub do_LEFT_PARENTHESIS {

        my $self = shift;

        # '('
        ++$paren_depth;

        # variable to enable check for brace after closing paren (c230)
        my $want_brace = EMPTY_STRING;

        if ($want_paren) {
            $container_type = $want_paren;
            $want_brace     = $want_paren;
            $want_paren     = EMPTY_STRING;
        }
        elsif ( substr( $statement_type, 0, 3 ) eq 'sub'
            && $statement_type =~ /^sub\b/ )
        {
            $container_type = $statement_type;
        }
        else {
            $container_type = $last_nonblank_token;

            # We can check for a syntax error here of unexpected '(',
            # but this is going to get messy...
            if (
                $expecting == OPERATOR

                # Be sure this is not a method call of the form
                # &method(...), $method->(..), &{method}(...),
                # $ref[2](list) is ok & short for $ref[2]->(list)
                # NOTE: at present, braces in something like &{ xxx }
                # are not marked as a block, we might have a method call.
                # Added ')' to fix case c017, something like ()()()
                && $last_nonblank_token !~ /^(?:[\]\}\)\&]|\-\>)/
              )
            {

                # ref: camel 3 p 703.
                if ( $last_last_nonblank_token eq 'do' ) {
                    $self->complain(
"do SUBROUTINE is deprecated; consider & or -> notation\n"
                    );
                }
                else {

                    # if this is an empty list, (), then it is not an
                    # error; for example, we might have a constant pi and
                    # invoke it with pi() or just pi;
                    my ( $next_nonblank_token, $i_next ) =
                      $self->find_next_nonblank_token( $i, $rtokens,
                        $max_token_index );

                    # Patch for c029: give up error check if
                    # a side comment follows
                    if (   $next_nonblank_token ne ')'
                        && $next_nonblank_token ne '#' )
                    {
                        my $hint;

                        $self->error_if_expecting_OPERATOR('(');

                        if ( $last_nonblank_type eq 'C' ) {
                            $hint =
                              "$last_nonblank_token has a void prototype\n";
                        }
                        elsif ( $last_nonblank_type eq 'i' ) {
                            if (   $i_tok > 0
                                && $last_nonblank_token =~ /^\$/ )
                            {
                                $hint =
                                  "Do you mean '$last_nonblank_token->(' ?\n";
                            }
                        }
                        else {
                            ## no hint
                        }
                        if ($hint) {
                            $self->interrupt_logfile();
                            $self->warning($hint);
                            $self->resume_logfile();
                        }
                    } ## end if ( $next_nonblank_token...
                } ## end else [ if ( $last_last_nonblank_token...
            } ## end if ( $expecting == OPERATOR...
        }

        ( $type_sequence, $indent_flag ) =
          $self->increase_nesting_depth( PAREN, $rtoken_map->[$i_tok] );

        # propagate types down through nested parens
        # for example: the second paren in 'if ((' would be structural
        # since the first is.

        if ( $last_nonblank_token eq '(' ) {
            $type = $last_nonblank_type;
        }

        #     We exclude parens as structural after a ',' because it
        #     causes subtle problems with continuation indentation for
        #     something like this, where the first 'or' will not get
        #     indented.
        #
        #         assert(
        #             __LINE__,
        #             ( not defined $check )
        #               or ref $check
        #               or $check eq "new"
        #               or $check eq "old",
        #         );
        #
        #     Likewise, we exclude parens where a statement can start
        #     because of problems with continuation indentation, like
        #     these:
        #
        #         ($firstline =~ /^#\!.*perl/)
        #         and (print $File::Find::name, "\n")
        #           and (return 1);
        #
        #         (ref($usage_fref) =~ /CODE/)
        #         ? &$usage_fref
        #           : (&blast_usage, &blast_params, &blast_general_params);

        else {
            $type = '{';
        }

        if ( $last_nonblank_type eq ')' ) {
            $self->warning(
                "Syntax error? found token '$last_nonblank_type' then '('\n");
        }

        # git #105: Copy container type and want-brace flag at ') (';
        # propagate the container type onward so that any subsequent brace gets
        # correctly marked.  I have implemented this as a general rule, which
        # should be safe, but if necessary it could be restricted to certain
        # container statement types such as 'for'.
        if ( $last_nonblank_token eq ')' ) {
            my $rvars = $rparen_vars->[$paren_depth];
            if ( defined($rvars) ) {
                $container_type = $rparen_type->[$paren_depth];
                ( my $type_lp, $want_brace ) = @{$rvars};
            }
        }

        $rparen_type->[$paren_depth]            = $container_type;
        $rparen_vars->[$paren_depth]            = [ $type, $want_brace ];
        $rparen_semicolon_count->[$paren_depth] = 0;

        return;

    } ## end sub do_LEFT_PARENTHESIS

    sub do_RIGHT_PARENTHESIS {

        my $self = shift;

        # ')'
        ( $type_sequence, $indent_flag ) =
          $self->decrease_nesting_depth( PAREN, $rtoken_map->[$i_tok] );

        my $rvars = $rparen_vars->[$paren_depth];
        if ( defined($rvars) ) {
            my ( $type_lp, $want_brace ) = @{$rvars};
            if ( $type_lp && $type_lp eq '{' ) {
                $type = '}';
            }
        }

        $container_type = $rparen_type->[$paren_depth];

        # restore statement type as 'sub' at closing paren of a signature
        # so that a subsequent ':' is identified as an attribute
        if ( substr( $container_type, 0, 3 ) eq 'sub'
            && $container_type =~ /^sub\b/ )
        {
            $statement_type = $container_type;
        }

        if ( $is_for_foreach{ $rparen_type->[$paren_depth] } ) {
            my $num_sc = $rparen_semicolon_count->[$paren_depth];
            if ( $num_sc > 0 && $num_sc != 2 ) {
                $self->warning("Expected 2 ';' in 'for(;;)' but saw $num_sc\n");
            }
        }

        if ( $paren_depth > 0 ) { $paren_depth-- }
        return;
    } ## end sub do_RIGHT_PARENTHESIS

    sub do_COMMA {

        my $self = shift;

        # ','
        if ( $last_nonblank_type eq ',' ) {
            $self->complain("Repeated ','s \n");
        }

        # Note that we have to check both token and type here because a
        # comma following a qw list can have last token='(' but type = 'q'
        elsif ( $last_nonblank_token eq '(' && $last_nonblank_type eq '{' ) {
            $self->warning("Unexpected leading ',' after a '('\n");
        }
        else {
            # no complaints about the comma
        }

        # patch for operator_expected: note if we are in the list (use.t)
        if ( $statement_type eq 'use' ) { $statement_type = '_use' }
        return;

    } ## end sub do_COMMA

    sub do_SEMICOLON {

        my $self = shift;

        # ';'
        $context        = UNKNOWN_CONTEXT;
        $statement_type = EMPTY_STRING;
        $want_paren     = EMPTY_STRING;

        if ( $is_for_foreach{ $rparen_type->[$paren_depth] } )
        {    # mark ; in for loop

            # Be careful: we do not want a semicolon such as the
            # following to be included:
            #
            #    for (sort {strcoll($a,$b);} keys %investments) {

            if (   $brace_depth == $rdepth_array->[PAREN][BRACE][$paren_depth]
                && $square_bracket_depth ==
                $rdepth_array->[PAREN][SQUARE_BRACKET][$paren_depth] )
            {

                $type = 'f';
                $rparen_semicolon_count->[$paren_depth]++;
            }
        }
        return;
    } ## end sub do_SEMICOLON

    sub do_QUOTATION_MARK {

        my $self = shift;

        # '"'
        $self->error_if_expecting_OPERATOR("String")
          if ( $expecting == OPERATOR );
        $in_quote                = 1;
        $type                    = 'Q';
        $allowed_quote_modifiers = EMPTY_STRING;
        return;
    } ## end sub do_QUOTATION_MARK

    sub do_APOSTROPHE {

        my $self = shift;

        # "'"
        $self->error_if_expecting_OPERATOR("String")
          if ( $expecting == OPERATOR );
        $in_quote                = 1;
        $type                    = 'Q';
        $allowed_quote_modifiers = EMPTY_STRING;
        return;
    } ## end sub do_APOSTROPHE

    sub do_BACKTICK {

        my $self = shift;

        # '`'
        $self->error_if_expecting_OPERATOR("String")
          if ( $expecting == OPERATOR );
        $in_quote                = 1;
        $type                    = 'Q';
        $allowed_quote_modifiers = EMPTY_STRING;
        return;
    } ## end sub do_BACKTICK

    sub do_SLASH {

        my $self = shift;

        # '/'
        my $is_pattern;

        # a pattern cannot follow certain keywords which take optional
        # arguments, like 'shift' and 'pop'. See also '?'.
        if (
            $last_nonblank_type eq 'k'
            && $is_keyword_rejecting_slash_as_pattern_delimiter{
                $last_nonblank_token}
          )
        {
            $is_pattern = 0;
        }
        elsif ( $expecting == UNKNOWN ) {    # indeterminate, must guess..
            my $msg;
            ( $is_pattern, $msg ) =
              $self->guess_if_pattern_or_division( $i, $rtokens, $rtoken_type,
                $rtoken_map, $max_token_index );

            if ($msg) {
                $self->write_diagnostics("DIVIDE:$msg\n");
                $self->write_logfile_entry($msg);
            }
        }
        else { $is_pattern = ( $expecting == TERM ) }

        if ($is_pattern) {
            $in_quote                = 1;
            $type                    = 'Q';
            $allowed_quote_modifiers = $quote_modifiers{'m'};
        }
        else {    # not a pattern; check for a /= token

            if ( $rtokens->[ $i + 1 ] eq '=' ) {    # form token /=
                $i++;
                $tok  = '/=';
                $type = $tok;
            }

            #DEBUG - collecting info on what tokens follow a divide
            # for development of guessing algorithm
            ##  if (
            ##      $self->is_possible_numerator( $i, $rtokens,
            ##          $max_token_index ) < 0
            ##    )
            ##  {
            ##      $self->write_diagnostics("DIVIDE? $input_line\n");
            ##  }
        }
        return;
    } ## end sub do_SLASH

    sub do_LEFT_CURLY_BRACKET {

        my $self = shift;

        # '{'
        # if we just saw a ')', we will label this block with
        # its type.  We need to do this to allow sub
        # code_block_type to determine if this brace starts a
        # code block or anonymous hash.  (The type of a paren
        # pair is the preceding token, such as 'if', 'else',
        # etc).
        $container_type = EMPTY_STRING;

        # ATTRS: for a '{' following an attribute list, reset
        # things to look like we just saw a sub name
        # Added 'package' (can be 'class') for --use-feature=class (rt145706)
        if ( substr( $statement_type, 0, 3 ) eq 'sub' ) {
            $last_nonblank_token = $statement_type;
            $last_nonblank_type  = 'S';               # c250 change
            $statement_type      = EMPTY_STRING;
        }
        elsif ( substr( $statement_type, 0, 7 ) eq 'package' ) {
            $last_nonblank_token = $statement_type;
            $last_nonblank_type  = 'P';               # c250 change
            $statement_type      = EMPTY_STRING;
        }

        # patch for SWITCH/CASE: hide these keywords from an immediately
        # following opening brace
        elsif ( ( $statement_type eq 'case' || $statement_type eq 'when' )
            && $statement_type eq $last_nonblank_token )
        {
            $last_nonblank_token = ";";
        }

        elsif ( $last_nonblank_token eq ')' ) {
            $last_nonblank_token = $rparen_type->[ $paren_depth + 1 ];

            # defensive move in case of a nesting error (pbug.t)
            # in which this ')' had no previous '('
            # this nesting error will have been caught
            if ( !defined($last_nonblank_token) ) {
                $last_nonblank_token = 'if';
            }

            # Syntax check at '){'
            if ( $is_blocktype_with_paren{$last_nonblank_token} ) {

                my $rvars = $rparen_vars->[ $paren_depth + 1 ];
                if ( defined($rvars) ) {
                    my ( $type_lp, $want_brace ) = @{$rvars};

                    # OLD: Now verify that this is not a trailing form
                    # FIX for git #124: we have to skip this check because
                    # the 'gather' keyword of List::Gather can operate on
                    # a full statement, so it isn't possible to be sure
                    # this is a trailing form.
                    if ( 0 && !$want_brace ) {
                        $self->warning(
"syntax error at ') {', unexpected '{' after closing ')' of a trailing '$last_nonblank_token'\n"
                        );
                    }
                }
            }
            else {
                if ($rOpts_extended_syntax) {

                    # we append a trailing () to mark this as an unknown
                    # block type.  This allows perltidy to format some
                    # common extensions of perl syntax.
                    # This is used by sub code_block_type
                    $last_nonblank_token .= '()';
                }
                else {
                    my $list =
                      join( SPACE, sort keys %is_blocktype_with_paren );
                    $self->warning(
"syntax error at ') {', didn't see one of: <<$list>>; If this code is okay try using the -xs flag\n"
                    );
                }
            }
        }

        # patch for paren-less for/foreach glitch, part 2.
        # see note below under 'qw'
        elsif ($last_nonblank_token eq 'qw'
            && $is_for_foreach{$want_paren} )
        {
            $last_nonblank_token = $want_paren;
            if ( $last_last_nonblank_token eq $want_paren ) {
                $self->warning(
"syntax error at '$want_paren .. {' -- missing \$ loop variable\n"
                );

            }
            $want_paren = EMPTY_STRING;
        }
        else {
            # not special
        }

        # now identify which of the three possible types of
        # curly braces we have: hash index container, anonymous
        # hash reference, or code block.

        # non-structural (hash index) curly brace pair
        # get marked 'L' and 'R'
        if ( is_non_structural_brace() ) {
            $type = 'L';

            # patch for SWITCH/CASE:
            # allow paren-less identifier after 'when'
            # if the brace is preceded by a space
            if (   $statement_type eq 'when'
                && $last_nonblank_type eq 'i'
                && $last_last_nonblank_type eq 'k'
                && ( $i_tok == 0 || $rtoken_type->[ $i_tok - 1 ] eq 'b' ) )
            {
                $type       = '{';
                $block_type = $statement_type;
            }
        }

        # code and anonymous hash have the same type, '{', but are
        # distinguished by 'block_type',
        # which will be blank for an anonymous hash
        else {

            $block_type =
              $self->code_block_type( $i_tok, $rtokens, $rtoken_type,
                $max_token_index );

            # patch to promote bareword type to function taking block
            if (   $block_type
                && $last_nonblank_type eq 'w'
                && $last_nonblank_i >= 0 )
            {
                if ( $routput_token_type->[$last_nonblank_i] eq 'w' ) {
                    $routput_token_type->[$last_nonblank_i] =
                      $is_grep_alias{$block_type} ? 'k' : 'G';
                }
            }

            # patch for SWITCH/CASE: if we find a stray opening block brace
            # where we might accept a 'case' or 'when' block, then take it
            if (   $statement_type eq 'case'
                || $statement_type eq 'when' )
            {
                if ( !$block_type || $block_type eq '}' ) {
                    $block_type = $statement_type;
                }
            }
        }

        $rbrace_type->[ ++$brace_depth ] = $block_type;

        # Patch for CLASS BLOCK definitions: do not update the package for the
        # current depth if this is a BLOCK type definition.
        # TODO: should make 'class' separate from 'package' and only do
        # this for 'class'
        $rbrace_package->[$brace_depth] = $current_package
          if ( substr( $block_type, 0, 8 ) ne 'package ' );

        $rbrace_structural_type->[$brace_depth] = $type;
        $rbrace_context->[$brace_depth]         = $context;
        ( $type_sequence, $indent_flag ) =
          $self->increase_nesting_depth( BRACE, $rtoken_map->[$i_tok] );

        return;
    } ## end sub do_LEFT_CURLY_BRACKET

    sub do_RIGHT_CURLY_BRACKET {

        my $self = shift;

        # '}'
        $block_type = $rbrace_type->[$brace_depth];
        if ($block_type) { $statement_type = EMPTY_STRING }
        if ( defined( $rbrace_package->[$brace_depth] ) ) {
            $current_package = $rbrace_package->[$brace_depth];
        }

        # can happen on brace error (caught elsewhere)
        else {
        }
        ( $type_sequence, $indent_flag ) =
          $self->decrease_nesting_depth( BRACE, $rtoken_map->[$i_tok] );

        if ( $rbrace_structural_type->[$brace_depth] eq 'L' ) {
            $type = 'R';
        }

        # propagate type information for 'do' and 'eval' blocks, and also
        # for smartmatch operator.  This is necessary to enable us to know
        # if an operator or term is expected next.
        if ( $is_block_operator{$block_type} ) {
            $tok = $block_type;
        }

        # pop non-indenting brace stack if sequence number matches
        if ( @{ $self->[_rnon_indenting_brace_stack_] }
            && $self->[_rnon_indenting_brace_stack_]->[-1] eq $type_sequence )
        {
            pop @{ $self->[_rnon_indenting_brace_stack_] };
        }

        $context = $rbrace_context->[$brace_depth];
        if ( $brace_depth > 0 ) { $brace_depth--; }
        return;
    } ## end sub do_RIGHT_CURLY_BRACKET

    sub do_AMPERSAND {

        my $self = shift;

        # '&' = maybe sub call? start looking
        # We have to check for sub call unless we are sure we
        # are expecting an operator.  This example from s2p
        # got mistaken as a q operator in an early version:
        #   print BODY &q(<<'EOT');
        if ( $expecting != OPERATOR ) {

            # But only look for a sub call if we are expecting a term or
            # if there is no existing space after the &.
            # For example we probably don't want & as sub call here:
            #    Fcntl::S_IRUSR & $mode;
            if ( $expecting == TERM || $next_type ne 'b' ) {
                $self->scan_simple_identifier();
            }
        }
        else {
        }
        return;
    } ## end sub do_AMPERSAND

    sub do_LESS_THAN_SIGN {

        my $self = shift;

        # '<' - angle operator or less than?
        if ( $expecting != OPERATOR ) {
            ( $i, $type ) =
              $self->find_angle_operator_termination( $input_line, $i,
                $rtoken_map, $expecting, $max_token_index );
        }
        else {
        }
        return;
    } ## end sub do_LESS_THAN_SIGN

    sub do_QUESTION_MARK {

        my $self = shift;

        # '?' = conditional or starting pattern?
        my $is_pattern;

        # Patch for rt #126965
        # a pattern cannot follow certain keywords which take optional
        # arguments, like 'shift' and 'pop'. See also '/'.
        if (
            $last_nonblank_type eq 'k'
            && $is_keyword_rejecting_question_as_pattern_delimiter{
                $last_nonblank_token}
          )
        {
            $is_pattern = 0;
        }

        # patch for RT#131288, user constant function without prototype
        # last type is 'U' followed by ?.
        elsif ( $last_nonblank_type =~ /^[FUY]$/ ) {
            $is_pattern = 0;
        }
        elsif ( $expecting == UNKNOWN ) {

            # In older versions of Perl, a bare ? can be a pattern
            # delimiter.  In perl version 5.22 this was
            # dropped, but we have to support it in order to format
            # older programs. See:
            ## https://perl.developpez.com/documentations/en/5.22.0/perl5211delta.html
            # For example, the following line worked
            # at one time:
            #      ?(.*)? && (print $1,"\n");
            # In current versions it would have to be written with slashes:
            #      /(.*)/ && (print $1,"\n");
            my $msg;
            ( $is_pattern, $msg ) =
              $self->guess_if_pattern_or_conditional( $i, $rtokens,
                $rtoken_type, $rtoken_map, $max_token_index );

            if ($msg) { $self->write_logfile_entry($msg) }
        }
        else { $is_pattern = ( $expecting == TERM ) }

        if ($is_pattern) {
            $in_quote                = 1;
            $type                    = 'Q';
            $allowed_quote_modifiers = $quote_modifiers{'m'};
        }
        else {
            ( $type_sequence, $indent_flag ) =
              $self->increase_nesting_depth( QUESTION_COLON,
                $rtoken_map->[$i_tok] );
        }
        return;
    } ## end sub do_QUESTION_MARK

    sub do_STAR {

        my $self = shift;

        # '*' = typeglob, or multiply?
        if ( $expecting == UNKNOWN && $last_nonblank_type eq 'Z' ) {
            if (   $next_type ne 'b'
                && $next_type ne '('
                && $next_type ne '#' )    # Fix c036
            {
                $expecting = TERM;
            }
        }
        if ( $expecting == TERM ) {
            $self->scan_simple_identifier();
        }
        else {

            if ( $rtokens->[ $i + 1 ] eq '=' ) {
                $tok  = '*=';
                $type = $tok;
                $i++;
            }
            elsif ( $rtokens->[ $i + 1 ] eq '*' ) {
                $tok  = '**';
                $type = $tok;
                $i++;
                if ( $rtokens->[ $i + 1 ] eq '=' ) {
                    $tok  = '**=';
                    $type = $tok;
                    $i++;
                }
            }
            else {
                ## not multiple characters
            }
        }
        return;
    } ## end sub do_STAR

    sub do_DOT {

        my $self = shift;

        # '.' =  what kind of . ?
        if ( $expecting != OPERATOR ) {
            $self->scan_number();
            if ( $type eq '.' ) {
                $self->error_if_expecting_TERM()
                  if ( $expecting == TERM );
            }
        }
        else {
        }
        return;
    } ## end sub do_DOT

    sub do_COLON {

        my $self = shift;

        # ':' = label, ternary, attribute, ?

        # if this is the first nonblank character, call it a label
        # since perl seems to just swallow it
        if ( $input_line_number == 1 && $last_nonblank_i == -1 ) {
            $type = 'J';
        }

        # ATTRS: check for a ':' which introduces an attribute list
        # either after a 'sub' keyword or within a paren list
        # Added 'package' (can be 'class') for --use-feature=class (rt145706)
        elsif ( $statement_type =~ /^(sub|package)\b/ ) {
            $type = 'A';
            $self->[_in_attribute_list_] = 1;
        }

        # Within a signature, unless we are in a ternary.  For example,
        # from 't/filter_example.t':
        #    method foo4 ( $class: $bar ) { $class->bar($bar) }
        elsif ( $rparen_type->[$paren_depth] =~ /^sub\b/
            && !is_balanced_closing_container(QUESTION_COLON) )
        {
            $type = 'A';
            $self->[_in_attribute_list_] = 1;
        }

        # check for scalar attribute, such as
        # my $foo : shared = 1;
        elsif ($is_my_our_state{$statement_type}
            && $rcurrent_depth->[QUESTION_COLON] == 0 )
        {
            $type = 'A';
            $self->[_in_attribute_list_] = 1;
        }

        # Look for Switch::Plain syntax if an error would otherwise occur
        # here. Note that we do not need to check if the extended syntax
        # flag is set because otherwise an error would occur, and we would
        # then have to output a message telling the user to set the
        # extended syntax flag to avoid the error.
        #  case 1: {
        #  default: {
        #  default:
        # Note that the line 'default:' will be parsed as a label elsewhere.
        elsif ( $is_case_default{$statement_type}
            && !is_balanced_closing_container(QUESTION_COLON) )
        {
            # mark it as a perltidy label type
            $type = 'J';
        }

        # otherwise, it should be part of a ?/: operator
        else {
            ( $type_sequence, $indent_flag ) =
              $self->decrease_nesting_depth( QUESTION_COLON,
                $rtoken_map->[$i_tok] );
            if ( $last_nonblank_token eq '?' ) {
                $self->warning("Syntax error near ? :\n");
            }
        }
        return;
    } ## end sub do_COLON

    sub do_PLUS_SIGN {

        my $self = shift;

        # '+' = what kind of plus?
        if ( $expecting == TERM ) {
            my $number = $self->scan_number_fast();

            # unary plus is safest assumption if not a number
            if ( !defined($number) ) { $type = 'p'; }
        }
        elsif ( $expecting == OPERATOR ) {
        }
        else {
            if ( $next_type eq 'w' ) { $type = 'p' }
        }
        return;
    } ## end sub do_PLUS_SIGN

    sub do_AT_SIGN {

        my $self = shift;

        # '@' = sigil for array?
        $self->error_if_expecting_OPERATOR("Array")
          if ( $expecting == OPERATOR );
        $self->scan_simple_identifier();
        return;
    } ## end sub do_AT_SIGN

    sub do_PERCENT_SIGN {

        my $self = shift;

        # '%' = hash or modulo?
        # first guess is hash if no following blank or paren
        if ( $expecting == UNKNOWN ) {
            if ( $next_type ne 'b' && $next_type ne '(' ) {
                $expecting = TERM;
            }
        }
        if ( $expecting == TERM ) {
            $self->scan_simple_identifier();
        }
        return;
    } ## end sub do_PERCENT_SIGN

    sub do_LEFT_SQUARE_BRACKET {

        my $self = shift;

        # '['
        $rsquare_bracket_type->[ ++$square_bracket_depth ] =
          $last_nonblank_token;
        ( $type_sequence, $indent_flag ) =
          $self->increase_nesting_depth( SQUARE_BRACKET,
            $rtoken_map->[$i_tok] );

        # It may seem odd, but structural square brackets have
        # type '{' and '}'.  This simplifies the indentation logic.
        if ( !is_non_structural_brace() ) {
            $type = '{';
        }
        $rsquare_bracket_structural_type->[$square_bracket_depth] = $type;
        return;
    } ## end sub do_LEFT_SQUARE_BRACKET

    sub do_RIGHT_SQUARE_BRACKET {

        my $self = shift;

        # ']'
        ( $type_sequence, $indent_flag ) =
          $self->decrease_nesting_depth( SQUARE_BRACKET,
            $rtoken_map->[$i_tok] );

        if ( $rsquare_bracket_structural_type->[$square_bracket_depth] eq '{' )
        {
            $type = '}';
        }

        # propagate type information for smartmatch operator.  This is
        # necessary to enable us to know if an operator or term is expected
        # next.
        if ( $rsquare_bracket_type->[$square_bracket_depth] eq '~~' ) {
            $tok = $rsquare_bracket_type->[$square_bracket_depth];
        }

        if ( $square_bracket_depth > 0 ) { $square_bracket_depth--; }
        return;
    } ## end sub do_RIGHT_SQUARE_BRACKET

    sub do_MINUS_SIGN {

        my $self = shift;

        # '-' = what kind of minus?
        if ( ( $expecting != OPERATOR )
            && $is_file_test_operator{$next_tok} )
        {
            my ( $next_nonblank_token, $i_next ) =
              $self->find_next_nonblank_token( $i + 1, $rtokens,
                $max_token_index );

            # check for a quoted word like "-w=>xx";
            # it is sufficient to just check for a following '='
            if ( $next_nonblank_token eq '=' ) {
                $type = 'm';
            }
            else {
                $i++;
                $tok .= $next_tok;
                $type = 'F';
            }
        }
        elsif ( $expecting == TERM ) {
            my $number = $self->scan_number_fast();

            # maybe part of bareword token? unary is safest
            if ( !defined($number) ) { $type = 'm'; }

        }
        elsif ( $expecting == OPERATOR ) {
        }
        else {

            if ( $next_type eq 'w' ) {
                $type = 'm';
            }
        }
        return;
    } ## end sub do_MINUS_SIGN

    sub do_CARAT_SIGN {

        my $self = shift;

        # '^'
        # check for special variables like ${^WARNING_BITS}
        if ( $expecting == TERM ) {

            if (   $last_nonblank_token eq '{'
                && ( $next_tok !~ /^\d/ )
                && ( $next_tok =~ /^\w/ ) )
            {

                if ( $next_tok eq 'W' ) {
                    $self->[_saw_perl_dash_w_] = 1;
                }
                $tok  = $tok . $next_tok;
                $i    = $i + 1;
                $type = 'w';

                # Optional coding to try to catch syntax errors. This can
                # be removed if it ever causes incorrect warning messages.
                # The '{^' should be preceded by either by a type or '$#'
                # Examples:
                #   $#{^CAPTURE}       ok
                #   *${^LAST_FH}{NAME} ok
                #   @{^HOWDY}          ok
                #   $hash{^HOWDY}      error

                # Note that a type sigil '$' may be tokenized as 'Z'
                # after something like 'print', so allow type 'Z'
                if (   $last_last_nonblank_type ne 't'
                    && $last_last_nonblank_type ne 'Z'
                    && $last_last_nonblank_token ne '$#' )
                {
                    $self->warning("Possible syntax error near '{^'\n");
                }
            }

            else {
                if ( !$self->error_if_expecting_TERM() ) {

                    # Something like this is valid but strange:
                    # undef ^I;
                    $self->complain("The '^' seems unusual here\n");
                }
            }
        }
        return;
    } ## end sub do_CARAT_SIGN

    sub do_DOUBLE_COLON {

        my $self = shift;

        #  '::' = probably a sub call
        $self->scan_bare_identifier();
        return;
    } ## end sub do_DOUBLE_COLON

    sub do_LEFT_SHIFT {

        my $self = shift;

        # '<<' = maybe a here-doc?
        if ( $expecting != OPERATOR ) {
            my ( $found_target, $here_doc_target, $here_quote_character,
                $saw_error );
            (
                $found_target, $here_doc_target, $here_quote_character, $i,
                $saw_error
              )
              = $self->find_here_doc( $expecting, $i, $rtokens, $rtoken_type,
                $rtoken_map, $max_token_index );

            if ($found_target) {
                push @{$rhere_target_list},
                  [ $here_doc_target, $here_quote_character ];
                $type = 'h';
                if ( length($here_doc_target) > 80 ) {
                    my $truncated = substr( $here_doc_target, 0, 80 );
                    $self->complain("Long here-target: '$truncated' ...\n");
                }
                elsif ( !$here_doc_target ) {
                    $self->warning(
                        'Use of bare << to mean <<"" is deprecated' . "\n" )
                      if ( !$here_quote_character );
                }
                elsif ( $here_doc_target !~ /^[A-Z_]\w+$/ ) {
                    $self->complain(
                        "Unconventional here-target: '$here_doc_target'\n");
                }
                else {
                    # nothing to complain about
                }
            }
            elsif ( $expecting == TERM ) {
                if ( !$saw_error ) {

                    # shouldn't happen..arriving here implies an error in
                    # the logic in sub 'find_here_doc'
                    if (DEVEL_MODE) {
                        $self->Fault(<<EOM);
Program bug; didn't find here doc target
EOM
                    }
                    $self->warning(
                        "Possible program error: didn't find here doc target\n"
                    );
                    $self->report_definite_bug();
                }
            }

            # target not found, expecting == UNKNOWN
            else {
                # assume it is a shift
            }
        }
        else {
        }
        return;
    } ## end sub do_LEFT_SHIFT

    sub do_NEW_HERE_DOC {

        # '<<~' = a here-doc, new type added in v26

        my $self = shift;

        return
          if ( $i >= $max_token_index );  # here-doc not possible if end of line
        if ( $expecting != OPERATOR ) {
            my ( $found_target, $here_doc_target, $here_quote_character,
                $saw_error );
            (
                $found_target, $here_doc_target, $here_quote_character, $i,
                $saw_error
              )
              = $self->find_here_doc( $expecting, $i, $rtokens, $rtoken_type,
                $rtoken_map, $max_token_index );

            if ($found_target) {

                if ( length($here_doc_target) > 80 ) {
                    my $truncated = substr( $here_doc_target, 0, 80 );
                    $self->complain("Long here-target: '$truncated' ...\n");
                }
                elsif ( $here_doc_target !~ /^[A-Z_]\w+$/ ) {
                    $self->complain(
                        "Unconventional here-target: '$here_doc_target'\n");
                }
                else {
                    # nothing to complain about
                }

                # Note that we put a leading space on the here quote
                # character indicate that it may be preceded by spaces
                $here_quote_character = SPACE . $here_quote_character;
                push @{$rhere_target_list},
                  [ $here_doc_target, $here_quote_character ];
                $type = 'h';
            }

            # target not found ..
            elsif ( $expecting == TERM ) {
                if ( !$saw_error ) {

                    # shouldn't happen..arriving here implies an error in
                    # the logic in sub 'find_here_doc'
                    if (DEVEL_MODE) {
                        $self->Fault(<<EOM);
Program bug; didn't find here doc target
EOM
                    }
                    $self->warning(
                        "Possible program error: didn't find here doc target\n"
                    );
                    $self->report_definite_bug();
                }
            }

            # Target not found, expecting==UNKNOWN
            else {
                $self->warning("didn't find here doc target after '<<~'\n");
            }
        }
        else {
            $self->error_if_expecting_OPERATOR();
        }
        return;
    } ## end sub do_NEW_HERE_DOC

    sub do_POINTER {

        #  '->'
        return;
    }

    sub do_PLUS_PLUS {

        my $self = shift;

        # '++'
        # type = 'pp' for pre-increment, '++' for post-increment
        if    ( $expecting == OPERATOR ) { $type = '++' }
        elsif ( $expecting == TERM )     { $type = 'pp' }

        # handle ( $expecting == UNKNOWN )
        else {

            # look ahead ..
            my ( $next_nonblank_token, $i_next ) =
              $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

            # Fix for c042: look past a side comment
            if ( $next_nonblank_token eq '#' ) {
                ( $next_nonblank_token, $i_next ) =
                  $self->find_next_nonblank_token( $max_token_index,
                    $rtokens, $max_token_index );
            }

            if ( $next_nonblank_token eq '$' ) { $type = 'pp' }
        }
        return;
    } ## end sub do_PLUS_PLUS

    sub do_FAT_COMMA {

        my $self = shift;

        # '=>'
        if ( $last_nonblank_type eq $tok ) {
            $self->complain("Repeated '=>'s \n");
        }

        # patch for operator_expected: note if we are in the list (use.t)
        # TODO: make version numbers a new token type
        if ( $statement_type eq 'use' ) { $statement_type = '_use' }
        return;
    } ## end sub do_FAT_COMMA

    sub do_MINUS_MINUS {

        my $self = shift;

        # '--'
        # type = 'mm' for pre-decrement, '--' for post-decrement

        if    ( $expecting == OPERATOR ) { $type = '--' }
        elsif ( $expecting == TERM )     { $type = 'mm' }

        # handle ( $expecting == UNKNOWN )
        else {

            # look ahead ..
            my ( $next_nonblank_token, $i_next ) =
              $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

            # Fix for c042: look past a side comment
            if ( $next_nonblank_token eq '#' ) {
                ( $next_nonblank_token, $i_next ) =
                  $self->find_next_nonblank_token( $max_token_index,
                    $rtokens, $max_token_index );
            }

            if ( $next_nonblank_token eq '$' ) { $type = 'mm' }
        }

        return;
    } ## end sub do_MINUS_MINUS

    sub do_LOGICAL_AND {

        my $self = shift;

        # '&&'
        $self->error_if_expecting_TERM()
          if ( $expecting == TERM && $last_nonblank_token ne ',' );    #c015
        return;
    } ## end sub do_LOGICAL_AND

    sub do_LOGICAL_OR {

        my $self = shift;

        # '||'
        $self->error_if_expecting_TERM()
          if ( $expecting == TERM && $last_nonblank_token ne ',' );    #c015
        return;
    } ## end sub do_LOGICAL_OR

    sub do_SLASH_SLASH {

        my $self = shift;

        # '//'
        $self->error_if_expecting_TERM()
          if ( $expecting == TERM );
        return;
    } ## end sub do_SLASH_SLASH

    sub do_DIGITS {

        my $self = shift;

        # 'd' = string of digits
        $self->error_if_expecting_OPERATOR("Number")
          if ( $expecting == OPERATOR );

        my $number = $self->scan_number_fast();
        if ( !defined($number) ) {

            # shouldn't happen - we should always get a number
            if (DEVEL_MODE) {
                $self->Fault(<<EOM);
non-number beginning with digit--program bug
EOM
            }
            $self->warning(
                "Unexpected error condition: non-number beginning with digit\n"
            );
            $self->report_definite_bug();
        }
        return;
    } ## end sub do_DIGITS

    sub do_ATTRIBUTE_LIST {

        my ( $self, $next_nonblank_token ) = @_;

        # Called at a bareword encountered while in an attribute list
        # returns 'is_attribute':
        #    true if attribute found
        #    false if an attribute (continue parsing bareword)

        # treat bare word followed by open paren like qw(
        if ( $next_nonblank_token eq '(' ) {

            # For something like:
            #     : prototype($$)
            # we should let do_scan_sub see it so that it can see
            # the prototype.  All other attributes get parsed as a
            # quoted string.
            if ( $tok eq 'prototype' ) {
                $id_scan_state = 'prototype';

                # start just after the word 'prototype'
                my $i_beg = $i + 1;
                ( $i, $tok, $type, $id_scan_state ) = $self->do_scan_sub(
                    {
                        input_line      => $input_line,
                        i               => $i,
                        i_beg           => $i_beg,
                        tok             => $tok,
                        type            => $type,
                        rtokens         => $rtokens,
                        rtoken_map      => $rtoken_map,
                        id_scan_state   => $id_scan_state,
                        max_token_index => $max_token_index,
                    }
                );

                # If successful, mark as type 'q' to be consistent
                # with other attributes.  Type 'w' would also work.
                if ( $i > $i_beg ) {
                    $type = 'q';
                    return 1;
                }

                # If not successful, continue and parse as a quote.
            }

            # All other attribute lists must be parsed as quotes
            # (see 'signatures.t' for good examples)
            $in_quote                = $quote_items{'q'};
            $allowed_quote_modifiers = $quote_modifiers{'q'};
            $type                    = 'q';
            $quote_type              = 'q';
            return 1;
        }

        # handle bareword not followed by open paren
        else {
            $type = 'w';
            return 1;
        }

        # attribute not found
        return;
    } ## end sub do_ATTRIBUTE_LIST

    sub do_X_OPERATOR {

        my $self = shift;

        if ( $tok eq 'x' ) {
            if ( $rtokens->[ $i + 1 ] eq '=' ) {    # x=
                $tok  = 'x=';
                $type = $tok;
                $i++;
            }
            else {
                $type = 'x';
            }
        }
        else {

            # Split a pretoken like 'x10' into 'x' and '10'.
            # Note: In previous versions of perltidy it was marked
            # as a number, $type = 'n', and fixed downstream by the
            # Formatter.
            $type = 'n';
            if ( $self->split_pretoken(1) ) {
                $type = 'x';
                $tok  = 'x';
            }
        }
        return;
    } ## end sub do_X_OPERATOR

    sub do_USE_CONSTANT {

        my $self = shift;

        $self->scan_bare_identifier();
        my ( $next_nonblank_tok2, $i_next2 ) =
          $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

        if ($next_nonblank_tok2) {

            if ( $is_keyword{$next_nonblank_tok2} ) {

                # Assume qw is used as a quote and okay, as in:
                #  use constant qw{ DEBUG 0 };
                # Not worth trying to parse for just a warning

                # NOTE: This warning is deactivated because recent
                # versions of perl do not complain here, but
                # the coding is retained for reference.
                if ( 0 && $next_nonblank_tok2 ne 'qw' ) {
                    $self->warning(
"Attempting to define constant '$next_nonblank_tok2' which is a perl keyword\n"
                    );
                }
            }

            else {
                $ris_constant->{$current_package}{$next_nonblank_tok2} = 1;
            }
        }
        return;
    } ## end sub do_USE_CONSTANT

    sub do_KEYWORD {

        my $self = shift;

        # found a keyword - set any associated flags
        $type = 'k';

        # Since for and foreach may not be followed immediately
        # by an opening paren, we have to remember which keyword
        # is associated with the next '('
        # Previously, before update   c230 : if ( $is_for_foreach{$tok} ) {
        ##(if elsif unless while until for foreach switch case given when catch)
        if ( $is_blocktype_with_paren{$tok} ) {
            if ( new_statement_ok() ) {
                $want_paren = $tok;
            }
        }

        # recognize 'use' statements, which are special
        if ( $is_use_require{$tok} ) {
            $statement_type = $tok;
            $self->error_if_expecting_OPERATOR()
              if ( $expecting == OPERATOR );
        }

        # remember my and our to check for trailing ": shared"
        elsif ( $is_my_our_state{$tok} ) {
            $statement_type = $tok;
        }

        # Check for unexpected 'elsif'
        elsif ( $tok eq 'elsif' ) {
            if (

                !$is_if_elsif_unless{$last_nonblank_block_type}

                # Allow isolated blocks of any kind during editing
                # by checking for a last noblank token of ';' and no
                # sequence numbers having been issued (c272). The check
                # on sequence number is not perfect but good enough.
                && !(
                       $last_nonblank_token eq ';'
                    && $next_sequence_number == SEQ_ROOT + 1
                )

              )
            {
                $self->warning(
                    "expecting '$tok' to follow one of 'if|elsif|unless'\n");
            }
        }

        # Check for unexpected 'else'
        elsif ( $tok eq 'else' ) {

            # patched for SWITCH/CASE
            if (

                !$is_if_elsif_unless_case_when{$last_nonblank_block_type}

                # patch to avoid an unwanted error message for
                # the case of a parenless 'case' (RT 105484):
                # switch ( 1 ) { case x { 2 } else { } }
                && !$is_if_elsif_unless_case_when{$statement_type}

                # Allow isolated blocks of any kind during editing (c272)
                && !(
                       $last_nonblank_token eq ';'
                    && $next_sequence_number == SEQ_ROOT + 1
                )

              )
            {
                $self->warning(
"expecting '$tok' to follow one of 'if|elsif|unless|case|when'\n"
                );
            }
        }

        # patch for SWITCH/CASE if 'case' and 'when are
        # treated as keywords.  Also 'default' for Switch::Plain
        elsif ($tok eq 'when'
            || $tok eq 'case'
            || $tok eq 'default' )
        {
            $statement_type = $tok;    # next '{' is block
        }

        # feature 'err' was removed in Perl 5.10.  So mark this as
        # a bareword unless an operator is expected (see c158).
        elsif ( $tok eq 'err' ) {
            if ( $expecting != OPERATOR ) { $type = 'w' }
        }
        else {
            ## no special treatment needed
        }

        return;
    } ## end sub do_KEYWORD

    sub do_QUOTE_OPERATOR {

        my $self = shift;

        if ( $expecting == OPERATOR ) {

            # Be careful not to call an error for a qw quote
            # where a parenthesized list is allowed.  For example,
            # it could also be a for/foreach construct such as
            #
            #    foreach my $key qw\Uno Due Tres Quadro\ {
            #        print "Set $key\n";
            #    }
            #

            # Or it could be a function call.
            # NOTE: Braces in something like &{ xxx } are not
            # marked as a block, we might have a method call.
            # &method(...), $method->(..), &{method}(...),
            # $ref[2](list) is ok & short for $ref[2]->(list)
            #
            # See notes in 'sub code_block_type' and
            # 'sub is_non_structural_brace'

            my $paren_list_possible = $tok eq 'qw'
              && ( $last_nonblank_token =~ /^([\]\}\&]|\-\>)/
                || $is_for_foreach{$want_paren} );

            if ( !$paren_list_possible ) {
                $self->error_if_expecting_OPERATOR();
            }
        }
        $in_quote                = $quote_items{$tok};
        $allowed_quote_modifiers = $quote_modifiers{$tok};

        # All quote types are 'Q' except possibly qw quotes.
        # qw quotes are special in that they may generally be trimmed
        # of leading and trailing whitespace.  So they are given a
        # separate type, 'q', unless requested otherwise.
        $type =
          ( $tok eq 'qw' && $rOpts_trim_qw )
          ? 'q'
          : 'Q';
        $quote_type = $type;
        return;
    } ## end sub do_QUOTE_OPERATOR

    sub do_UNKNOWN_BAREWORD {

        my ( $self, $next_nonblank_token ) = @_;

        $self->scan_bare_identifier();

        if (   $statement_type eq 'use'
            && $last_nonblank_token eq 'use' )
        {
            $rsaw_use_module->{$current_package}->{$tok} = 1;
        }

        if ( $type eq 'w' ) {

            if ( $expecting == OPERATOR ) {

                # Patch to avoid error message for RPerl overloaded
                # operator functions: use overload
                #    '+' => \&sse_add,
                #    '-' => \&sse_sub,
                #    '*' => \&sse_mul,
                #    '/' => \&sse_div;
                # TODO: this could eventually be generalized
                if (   $rsaw_use_module->{$current_package}->{'RPerl'}
                    && $tok =~ /^sse_(mul|div|add|sub)$/ )
                {

                }

                # Fix part 1 for git #63 in which a comment falls
                # between an -> and the following word.  An
                # alternate fix would be to change operator_expected
                # to return an UNKNOWN for this type.
                elsif ( $last_nonblank_type eq '->' ) {

                }

                # don't complain about possible indirect object
                # notation.
                # For example:
                #   package main;
                #   sub new($) { ... }
                #   $b = new A::;  # calls A::new
                #   $c = new A;    # same thing but suspicious
                # This will call A::new but we have a 'new' in
                # main:: which looks like a constant.
                #
                elsif ( $last_nonblank_type eq 'C' ) {
                    if ( $tok !~ /::$/ ) {
                        $self->complain(<<EOM);
Expecting operator after '$last_nonblank_token' but found bare word '$tok'
       Maybe indirectet object notation?
EOM
                    }
                }
                else {
                    $self->error_if_expecting_OPERATOR("bareword");
                }
            }

            # mark bare words immediately followed by a paren as
            # functions
            $next_tok = $rtokens->[ $i + 1 ];
            if ( $next_tok eq '(' ) {

                # Patch for issue c151, where we are processing a snippet and
                # have not seen that SPACE is a constant.  In this case 'x' is
                # probably an operator. The only disadvantage with an incorrect
                # guess is that the space after it may be incorrect. For example
                #   $str .= SPACE x ( 16 - length($str) ); See also b1410.
                if ( $tok eq 'x' && $last_nonblank_type eq 'w' ) { $type = 'x' }

                # Fix part 2 for git #63.  Leave type as 'w' to keep
                # the type the same as if the -> were not separated
                elsif ( $last_nonblank_type ne '->' ) { $type = 'U' }

                # not a special case
                else { }

            }

            # underscore after file test operator is file handle
            if ( $tok eq '_' && $last_nonblank_type eq 'F' ) {
                $type = 'Z';
            }

            # patch for SWITCH/CASE if 'case' and 'when are
            # not treated as keywords:
            if (
                ( $tok eq 'case' && $rbrace_type->[$brace_depth] eq 'switch' )
                || (   $tok eq 'when'
                    && $rbrace_type->[$brace_depth] eq 'given' )
              )
            {
                $statement_type = $tok;    # next '{' is block
                $type           = 'k';     # for keyword syntax coloring
            }
            if ( $next_nonblank_token eq '(' ) {

                # patch for SWITCH/CASE if switch and given not keywords
                # Switch is not a perl 5 keyword, but we will gamble
                # and mark switch followed by paren as a keyword.  This
                # is only necessary to get html syntax coloring nice,
                # and does not commit this as being a switch/case.
                if ( $tok eq 'switch' || $tok eq 'given' ) {
                    $type = 'k';    # for keyword syntax coloring
                }

                # mark 'x' as operator for something like this (see b1410)
                #  my $line = join( LD_X, map { LD_H x ( $_ + 2 ) } @$widths );
                elsif ( $tok eq 'x' && $last_nonblank_type eq 'w' ) {
                    $type = 'x';
                }
                else {
                    ## not a special case
                }
            }
        }
        return;
    } ## end sub do_UNKNOWN_BAREWORD

    sub sub_attribute_ok_here {

        my ( $self, $tok_kw, $next_nonblank_token, $i_next ) = @_;

        # Decide if 'sub :' can be the start of a sub attribute list.
        # We will decide based on if the colon is followed by a
        # bareword which is not a keyword.
        # Changed inext+1 to inext to fixed case b1190.
        my $sub_attribute_ok_here;
        if (   $is_sub{$tok_kw}
            && $expecting != OPERATOR
            && $next_nonblank_token eq ':' )
        {
            my ( $nn_nonblank_token, $i_nn ) =
              $self->find_next_nonblank_token( $i_next, $rtokens,
                $max_token_index );
            $sub_attribute_ok_here =
                 $nn_nonblank_token =~ /^\w/
              && $nn_nonblank_token !~ /^\d/
              && !$is_keyword{$nn_nonblank_token};
        }
        return $sub_attribute_ok_here;
    } ## end sub sub_attribute_ok_here

    sub do_BAREWORD {

        my ($self) = @_;

        # handle a bareword token:
        # returns
        #    true if this token ends the current line
        #    false otherwise

        my $next_nonblank_token;
        my $i_next = $i + 1;
        if ( $i_next <= $max_token_index && $rtoken_type->[$i_next] eq 'b' ) {
            $i_next++;
        }
        if ( $i_next <= $max_token_index ) {
            $next_nonblank_token = $rtokens->[$i_next];
        }
        else {
            ( $next_nonblank_token, $i_next ) =
              $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );
        }

        # a bare word immediately followed by :: is not a keyword;
        # use $tok_kw when testing for keywords to avoid a mistake
        my $tok_kw = $tok;
        if (   $rtokens->[ $i + 1 ] eq ':'
            && $rtokens->[ $i + 2 ] eq ':' )
        {
            $tok_kw .= '::';
        }

        if ( $self->[_in_attribute_list_] ) {
            my $is_attribute = $self->do_ATTRIBUTE_LIST($next_nonblank_token);
            return if ($is_attribute);
        }

        #----------------------------------------
        # Starting final if-elsif- chain of tests
        #----------------------------------------

        # This is the return flag:
        #   true => this is the last token on the line
        #   false => keep tokenizing the line
        my $is_last;

        # The following blocks of code must update these vars:
        # $type - the final token type, must always be set

        # In addition, if additional pretokens are added:
        # $tok  - the final token
        # $i    - the index of the last pretoken

        # They may also need to check and set various flags

        # Scan a bare word following a -> as an identifier; it could
        # have a long package name.  Fixes c037, c041.
        if ( $last_nonblank_token eq '->' ) {
            $self->scan_bare_identifier();

            # a bareward after '->' gets type 'i'
            $type = 'i';
        }

        # Quote a word followed by => operator
        elsif (
            ( $next_nonblank_token eq '=' && $rtokens->[ $i_next + 1 ] eq '>' )

            # unless the word is __END__ or __DATA__ and is the only word on
            # the line.
            && ( !defined( $is_END_DATA{$tok_kw} )
                || $input_line !~ /^\s*__(?:END|DATA)__\s*$/ )
          )
        {
            # Bareword followed by a fat comma - see 'git18.in'
            # This code was previously sub do_QUOTED_BAREWORD: see c316, c317

            #   'v25=>1'   is a v-string key!
            #   '-v25=>1'  is also a v-string key!
            if ( $tok =~ /^v\d+$/ ) {
                $type = 'v';
                $self->complain("v-string used as hash key\n");
                $self->report_v_string($tok);
            }

            # If tok is something like 'x17' then it could
            # actually be operator x followed by number 17.
            # For example, here:
            #     123x17 => [ 792, 1224 ],
            # (a key of 123 repeated 17 times, perhaps not
            # what was intended). We will mark x17 as type
            # 'n' and it will be split. If the previous token
            # was also a bareword then it is not very clear is
            # going on.  In this case we will not be sure that
            # an operator is expected, so we just mark it as a
            # bareword.  Perl is a little murky in what it does
            # with stuff like this, and its behavior can change
            # over time.  Something like
            #    a x18 => [792, 1224], will compile as
            # a key with 18 a's.  But something like
            #    push @array, a x18;
            # is a syntax error.
            elsif (
                   $expecting == OPERATOR
                && substr( $tok, 0, 1 ) eq 'x'
                && ( length($tok) == 1
                    || substr( $tok, 1, 1 ) =~ /^\d/ )
              )
            {
                $type = 'n';
                if ( $self->split_pretoken(1) ) {
                    $type = 'x';
                    $tok  = 'x';
                }
                $self->complain("x operator in hash key\n");
            }
            else {

                # git #18
                $type = 'w';
                $self->error_if_expecting_OPERATOR();
            }
        }

        # quote a bare word within braces..like xxx->{s}; note that we
        # must be sure this is not a structural brace, to avoid
        # mistaking {s} in the following for a quoted bare word:
        #     for(@[){s}bla}BLA}
        # Also treat q in something like var{-q} as a bare word, not
        # a quote operator
        elsif (
            $next_nonblank_token eq '}'
            && (
                $last_nonblank_type eq 'L'
                || (   $last_nonblank_type eq 'm'
                    && $last_last_nonblank_type eq 'L' )
            )
          )
        {
            $type = 'w';
        }

        # handle operator x (now we know it isn't $x=)
        elsif (
               $expecting == OPERATOR
            && substr( $tok, 0, 1 ) eq 'x'
            && ( length($tok) == 1
                || substr( $tok, 1, 1 ) =~ /^\d/ )
          )
        {
            $self->do_X_OPERATOR();
        }
        elsif ( $tok_kw eq 'CORE::' ) {
            $type = $tok = $tok_kw;
            $i += 2;
        }
        elsif ( ( $tok eq 'strict' )
            and ( $last_nonblank_token eq 'use' ) )
        {
            $self->[_saw_use_strict_] = 1;
            $self->scan_bare_identifier();
        }

        elsif ( ( $tok eq 'warnings' )
            and ( $last_nonblank_token eq 'use' ) )
        {
            $self->[_saw_perl_dash_w_] = 1;

            # scan as identifier, so that we pick up something like:
            # use warnings::register
            $self->scan_bare_identifier();
        }

        elsif (
               $tok eq 'AutoLoader'
            && $self->[_look_for_autoloader_]
            && (
                $last_nonblank_token eq 'use'

                # these regexes are from AutoSplit.pm, which we want
                # to mimic
                || $input_line =~ /^\s*(use|require)\s+AutoLoader\b/
                || $input_line =~ /\bISA\s*=.*\bAutoLoader\b/
            )
          )
        {
            $self->write_logfile_entry("AutoLoader seen, -nlal deactivates\n");
            $self->[_saw_autoloader_]      = 1;
            $self->[_look_for_autoloader_] = 0;
            $self->scan_bare_identifier();
        }

        elsif (
               $tok eq 'SelfLoader'
            && $self->[_look_for_selfloader_]
            && (   $last_nonblank_token eq 'use'
                || $input_line =~ /^\s*(use|require)\s+SelfLoader\b/
                || $input_line =~ /\bISA\s*=.*\bSelfLoader\b/ )
          )
        {
            $self->write_logfile_entry("SelfLoader seen, -nlsl deactivates\n");
            $self->[_saw_selfloader_]      = 1;
            $self->[_look_for_selfloader_] = 0;
            $self->scan_bare_identifier();
        }

        elsif ( ( $tok eq 'constant' )
            and ( $last_nonblank_token eq 'use' ) )
        {
            $self->do_USE_CONSTANT();
        }

        # various quote operators
        elsif ( $is_q_qq_qw_qx_qr_s_y_tr_m{$tok} ) {
            $self->do_QUOTE_OPERATOR();
        }

        # check for a statement label
        elsif (
               ( $next_nonblank_token eq ':' )
            && ( $rtokens->[ $i_next + 1 ] ne ':' )
            && ( $i_next <= $max_token_index )    # colon on same line

            # like 'sub : lvalue' ?
            && !$self->sub_attribute_ok_here( $tok_kw, $next_nonblank_token,
                $i_next )
            && new_statement_ok()
          )
        {
            if ( $tok !~ /[A-Z]/ ) {
                push @{ $self->[_rlower_case_labels_at_] }, $input_line_number;
            }
            $type = 'J';
            $tok .= ':';
            $i = $i_next;
        }

        # 'sub' or other sub alias
        elsif ( $is_sub{$tok_kw} ) {

            # Update for --use-feature=class (rt145706):
            # We have to be extra careful to avoid misparsing other uses of
            # 'method' in older scripts.
            if ( $tok_kw eq 'method' && $guess_if_method ) {
                if (   $expecting == OPERATOR
                    || $next_nonblank_token !~ /^[\w\:]/
                    || !$self->method_ok_here() )
                {
                    $self->do_UNKNOWN_BAREWORD($next_nonblank_token);
                }
                else {
                    initialize_subname();
                    $self->scan_id();
                }
            }
            else {
                $self->error_if_expecting_OPERATOR()
                  if ( $expecting == OPERATOR );
                initialize_subname();
                $self->scan_id();
            }
        }

        # 'package'
        elsif ( $is_package{$tok_kw} ) {

            # Update for --use-feature=class (rt145706):
            # We have to be extra careful because 'class' may be used for other
            # purposes on older code; i.e.
            #   class($x)   - valid sub call
            #   package($x) - error
            if ( $tok_kw eq 'class' ) {
                if (   $expecting == OPERATOR
                    || $next_nonblank_token !~ /^[\w\:]/
                    || !$self->class_ok_here() )
                {
                    $self->do_UNKNOWN_BAREWORD($next_nonblank_token);
                }
                else { $self->scan_id() }
            }
            else {
                $self->error_if_expecting_OPERATOR()
                  if ( $expecting == OPERATOR );
                $self->scan_id();
            }
        }

        # Fix for c035: split 'format' from 'is_format_END_DATA' to be
        # more restrictive. Require a new statement to be ok here.
        elsif ( $tok_kw eq 'format' && new_statement_ok() ) {
            $type                = ';';    # make tokenizer look for TERM next
            $self->[_in_format_] = 1;
            $is_last             = 1;      ## is last token on this line
        }

        # Note on token types for format, __DATA__, __END__:
        # It simplifies things to give these type ';', so that when we
        # start rescanning we will be expecting a token of type TERM.
        # We will switch to type 'k' before outputting the tokens.
        elsif ( defined( $is_END_DATA{$tok_kw} ) ) {
            $type = ';';    # make tokenizer look for TERM next

            # Remember that we are in one of these three sections
            $self->[ $is_END_DATA{$tok_kw} ] = 1;
            $is_last = 1;                          ## is last token on this line
        }

        elsif ( $is_keyword{$tok_kw} ) {
            $self->do_KEYWORD();
        }

        # check for inline label following
        #         /^(redo|last|next|goto)$/
        elsif (( $last_nonblank_type eq 'k' )
            && ( $is_redo_last_next_goto{$last_nonblank_token} ) )
        {
            $type = 'j';
        }

        # something else --
        else {
            $self->do_UNKNOWN_BAREWORD($next_nonblank_token);
        }

        return $is_last;

    } ## end sub do_BAREWORD

    sub do_FOLLOW_QUOTE {

        my $self = shift;

        # Continue following a quote on a new line
        $type = $quote_type;

        if ( !@{$routput_token_list} ) {    # initialize if continuation line
            push( @{$routput_token_list}, $i );
            $routput_token_type->[$i] = $type;

        }

        # scan for the end of the quote or pattern
        (
            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string_1,
            $quoted_string_2,

        ) = $self->do_quote(

            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string_1,
            $quoted_string_2,
            $rtokens,
            $rtoken_type,
            $rtoken_map,
            $max_token_index,

        );

        # all done if we didn't find it
        if ($in_quote) { return }

        # save pattern and replacement text for rescanning
        my $qs1 = $quoted_string_1;

        # re-initialize for next search
        $quote_character = EMPTY_STRING;
        $quote_pos       = 0;
        $quote_type      = 'Q';
        $quoted_string_1 = EMPTY_STRING;
        $quoted_string_2 = EMPTY_STRING;
        if ( ++$i > $max_token_index ) { return }

        # look for any modifiers
        if ($allowed_quote_modifiers) {

            # check for exact quote modifiers
            if ( $rtokens->[$i] =~ /^[A-Za-z_]/ ) {
                my $str = $rtokens->[$i];
                my $saw_modifier_e;
                while ( $str =~ /\G$allowed_quote_modifiers/gc ) {
                    my $pos  = pos($str);
                    my $char = substr( $str, $pos - 1, 1 );
                    $saw_modifier_e ||= ( $char eq 'e' );
                }

                # For an 'e' quote modifier we must scan the replacement
                # text for here-doc targets...
                # but if the modifier starts a new line we can skip
                # this because either the here doc will be fully
                # contained in the replacement text (so we can
                # ignore it) or Perl will not find it.
                # See test 'here2.in'.
                if ( $saw_modifier_e && $i_tok >= 0 ) {

                    my $rht = $self->scan_replacement_text($qs1);

                    # Change type from 'Q' to 'h' for quotes with
                    # here-doc targets so that the formatter (see sub
                    # process_line_of_CODE) will not make any line
                    # breaks after this point.
                    if ($rht) {
                        push @{$rhere_target_list}, @{$rht};
                        $type = 'h';
                        if ( $i_tok < 0 ) {
                            my $ilast = $routput_token_list->[-1];
                            $routput_token_type->[$ilast] = $type;
                        }
                    }
                }

                if ( defined( pos($str) ) ) {

                    # matched
                    if ( pos($str) == length($str) ) {
                        if ( ++$i > $max_token_index ) { return }
                    }

                    # Looks like a joined quote modifier
                    # and keyword, maybe something like
                    # s/xxx/yyy/gefor @k=...
                    # Example is "galgen.pl".  Would have to split
                    # the word and insert a new token in the
                    # pre-token list.  This is so rare that I haven't
                    # done it.  Will just issue a warning citation.

                    # This error might also be triggered if my quote
                    # modifier characters are incomplete
                    else {
                        $self->warning(<<EOM);

Partial match to quote modifier $allowed_quote_modifiers at word: '$str'
Please put a space between quote modifiers and trailing keywords.
EOM

                        # print "token $rtokens->[$i]\n";
                        # my $num = length($str) - pos($str);
                        # $rtokens->[$i]=substr($rtokens->[$i],pos($str),$num);
                        # print "continuing with new token $rtokens->[$i]\n";

                        # skipping past this token does least damage
                        if ( ++$i > $max_token_index ) { return }
                    }
                }
                else {

                    # example file: rokicki4.pl
                    # This error might also be triggered if my quote
                    # modifier characters are incomplete
                    $self->write_logfile_entry(
                        "Note: found word $str at quote modifier location\n");
                }
            }

            # re-initialize
            $allowed_quote_modifiers = EMPTY_STRING;
        }
        return;
    } ## end sub do_FOLLOW_QUOTE

    # ------------------------------------------------------------
    # begin hash of code for handling most token types
    # ------------------------------------------------------------
    my $tokenization_code = {

        '>'   => \&do_GREATER_THAN_SIGN,
        '|'   => \&do_VERTICAL_LINE,
        '$'   => \&do_DOLLAR_SIGN,
        '('   => \&do_LEFT_PARENTHESIS,
        ')'   => \&do_RIGHT_PARENTHESIS,
        ','   => \&do_COMMA,
        ';'   => \&do_SEMICOLON,
        '"'   => \&do_QUOTATION_MARK,
        "'"   => \&do_APOSTROPHE,
        '`'   => \&do_BACKTICK,
        '/'   => \&do_SLASH,
        '{'   => \&do_LEFT_CURLY_BRACKET,
        '}'   => \&do_RIGHT_CURLY_BRACKET,
        '&'   => \&do_AMPERSAND,
        '<'   => \&do_LESS_THAN_SIGN,
        '?'   => \&do_QUESTION_MARK,
        '*'   => \&do_STAR,
        '.'   => \&do_DOT,
        ':'   => \&do_COLON,
        '+'   => \&do_PLUS_SIGN,
        '@'   => \&do_AT_SIGN,
        '%'   => \&do_PERCENT_SIGN,
        '['   => \&do_LEFT_SQUARE_BRACKET,
        ']'   => \&do_RIGHT_SQUARE_BRACKET,
        '-'   => \&do_MINUS_SIGN,
        '^'   => \&do_CARAT_SIGN,
        '::'  => \&do_DOUBLE_COLON,
        '<<'  => \&do_LEFT_SHIFT,
        '<<~' => \&do_NEW_HERE_DOC,
        '->'  => \&do_POINTER,
        '++'  => \&do_PLUS_PLUS,
        '=>'  => \&do_FAT_COMMA,
        '--'  => \&do_MINUS_MINUS,
        '&&'  => \&do_LOGICAL_AND,
        '||'  => \&do_LOGICAL_OR,
        '//'  => \&do_SLASH_SLASH,

        # No special code for these types yet, but syntax checks
        # could be added.
        ##  '!'   => undef,
        ##  '!='  => undef,
        ##  '!~'  => undef,
        ##  '%='  => undef,
        ##  '&&=' => undef,
        ##  '&='  => undef,
        ##  '+='  => undef,
        ##  '-='  => undef,
        ##  '..'  => undef,
        ##  '..'  => undef,
        ##  '...' => undef,
        ##  '.='  => undef,
        ##  '<<=' => undef,
        ##  '<='  => undef,
        ##  '<=>' => undef,
        ##  '<>'  => undef,
        ##  '='   => undef,
        ##  '=='  => undef,
        ##  '=~'  => undef,
        ##  '>='  => undef,
        ##  '>>'  => undef,
        ##  '>>=' => undef,
        ##  '\\'  => undef,
        ##  '^='  => undef,
        ##  '|='  => undef,
        ##  '||=' => undef,
        ##  '//=' => undef,
        ##  '~'   => undef,
        ##  '~~'  => undef,
        ##  '!~~' => undef,

    };

    # ------------------------------------------------------------
    # end hash of code for handling individual token types
    # ------------------------------------------------------------

    use constant DEBUG_TOKENIZE => 0;

    my %is_arrow_or_Z;

    BEGIN {
        my @qZ = qw( -> Z );
        @is_arrow_or_Z{@qZ} = (1) x scalar(@qZ);
    }

    sub tokenize_this_line {

        # This routine tokenizes one line. The results are stored in
        # the hash ref '$line_of_tokens'.

        # Given:
        #   $line_of_tokens = ref to hash of values being filled for this line
        #   $trimmed_input_line
        #        = the input line without leading whitespace, OR
        #        = undef if not available
        # Returns:
        #   nothing

        my ( $self, $line_of_tokens, $trimmed_input_line ) = @_;
        my $untrimmed_input_line = $line_of_tokens->{_line_text};

        # Extract line number for use in error messages
        $input_line_number = $line_of_tokens->{_line_number};

        #-------------------------------------
        # Check for start of pod documentation
        #-------------------------------------
        if ( substr( $untrimmed_input_line, 0, 1 ) eq '='
            && $untrimmed_input_line =~ /^=[A-Za-z_]/ )
        {

            # Must not be in multi-line quote
            # and must not be in an equation
            my $blank_after_Z = 1;
            if (
                !$in_quote
                && ( $self->operator_expected( '=', 'b', $blank_after_Z ) ==
                    TERM )
              )
            {
                $self->[_in_pod_] = 1;
                return;
            }
        }

        #--------------------------
        # Trim leading whitespace ?
        #--------------------------
        # Use untrimmed line if we are continuing in a type 'Q' quote
        if ( $in_quote && $quote_type eq 'Q' ) {
            $line_of_tokens->{_starting_in_quote} = 1;
            $input_line = $untrimmed_input_line;
            chomp $input_line;
        }

        # Trim start of this line if we are not continuing a quoted line.
        # Do not trim end because we might end in a quote (test: deken4.pl)
        # Perl::Tidy::Formatter will delete needless trailing blanks
        else {
            $line_of_tokens->{_starting_in_quote} = 0;

            # Use the pre-computed trimmed line if defined (most efficient)
            $input_line = $trimmed_input_line;

            # otherwise trim the raw input line (much less efficient)
            if ( !defined($input_line) ) {
                $input_line = $untrimmed_input_line;
                $input_line =~ s/^\s+//;
            }

            chomp $input_line;

            # define 'guessed_indentation_level' if logfile will be saved
            if ( $self->[_save_logfile_] && length($input_line) ) {
                my $guess =
                  $self->guess_old_indentation_level($untrimmed_input_line);
                $line_of_tokens->{_guessed_indentation_level} = $guess;
            }
        }

        #------------
        # Blank lines
        #------------
        if ( !length($input_line) ) {
            $line_of_tokens->{_line_type}        = 'CODE';
            $line_of_tokens->{_rtokens}          = [];
            $line_of_tokens->{_rtoken_type}      = [];
            $line_of_tokens->{_rlevels}          = [];
            $line_of_tokens->{_rblock_type}      = [];
            $line_of_tokens->{_nesting_tokens_0} = $nesting_token_string;
            $line_of_tokens->{_nesting_blocks_0} = $nesting_block_string;
            return;
        }

        #---------
        # Comments
        #---------
        if ( !$in_quote && substr( $input_line, 0, 1 ) eq '#' ) {

            # and check for skipped section
            if (
                (
                    substr( $input_line, 0, 4 ) eq '#<<V'
                    || $rOpts_code_skipping_begin
                )
                && $rOpts_code_skipping

                # note that the code_skipping_patterns require a newline
                && $input_line . "\n" =~ /$code_skipping_pattern_begin/
              )
            {
                $self->[_in_code_skipping_] = $self->[_last_line_number_];
                return;
            }

            if ( !$self->[_in_format_skipping_] ) {
                if (
                    (
                        substr( $input_line, 0, 4 ) eq '#<<<'
                        || $rOpts_format_skipping_begin
                    )
                    && $rOpts_format_skipping

                    # note that the code_skipping_patterns require a newline
                    && $input_line . "\n" =~ /$format_skipping_pattern_begin/
                  )
                {
                    $self->[_in_format_skipping_] = $self->[_last_line_number_];
                }
            }
            else {
                if (
                    (
                        substr( $input_line, 0, 4 ) eq '#>>>'
                        || $rOpts_format_skipping_end
                    )

                    # note that the code_skipping_patterns require a newline
                    && $input_line . "\n" =~ /$format_skipping_pattern_end/
                  )
                {
                    $self->[_in_format_skipping_] = 0;
                }
            }

            # Optional fast processing of a block comment
            $line_of_tokens->{_line_type}        = 'CODE';
            $line_of_tokens->{_rtokens}          = [$input_line];
            $line_of_tokens->{_rtoken_type}      = ['#'];
            $line_of_tokens->{_rlevels}          = [$level_in_tokenizer];
            $line_of_tokens->{_rblock_type}      = [EMPTY_STRING];
            $line_of_tokens->{_nesting_tokens_0} = $nesting_token_string;
            $line_of_tokens->{_nesting_blocks_0} = $nesting_block_string;
            return;
        }

        #-------------------------------------
        # Loop to find all tokens on this line
        #-------------------------------------

        # Update the copy of the line for use in error messages
        # This must be exactly what we give the pre_tokenizer
        $self->[_line_of_text_] = $input_line;

        # re-initialize for the main loop
        $routput_token_list    = [];    # stack of output token indexes
        $routput_token_type    = [];    # token types
        $routput_block_type    = [];    # types of code block
        $routput_type_sequence = [];    # nesting sequential number

        $rhere_target_list = [];

        $tok             = $last_nonblank_token;
        $type            = $last_nonblank_type;
        $prototype       = $last_nonblank_prototype;
        $last_nonblank_i = -1;
        $block_type      = $last_nonblank_block_type;
        $container_type  = $last_nonblank_container_type;
        $type_sequence   = $last_nonblank_type_sequence;
        $indent_flag     = 0;
        $peeked_ahead    = 0;

        $self->tokenizer_main_loop();

        #-------------------------------------------------
        # Done tokenizing this line ... package the result
        #-------------------------------------------------
        $self->tokenizer_wrapup_line($line_of_tokens);

        return;
    } ## end sub tokenize_this_line

    sub tokenizer_main_loop {

        my ($self) = @_;

        # Break one input line into tokens
        # We are working on closure variables.

        # Start by breaking the line into pre-tokens
        ( $rtokens, $rtoken_map, $rtoken_type ) = pre_tokenize($input_line);

        # Verify that all leading whitespace has been trimmed
        # except for quotes of type 'Q' (c273).
        if (   @{$rtokens}
            && $rtoken_type->[0] eq 'b'
            && !( $in_quote && $quote_type eq 'Q' ) )
        {

            # Shouldn't happen if calling sub did trim operation correctly.
            DEVEL_MODE && $self->Fault(<<EOM);
leading blank at line
$input_line
EOM

            # Fix by removing the leading blank token.  This fix has been
            # tested and works correctly even if no whitespaces was trimmed.
            # But it is an inefficient way to do things because, for example,
            # it forces all comments to be processed by sub pre_tokenize.
            # And it may cause indented code-skipping comments to be missed.
            shift @{$rtokens};
            shift @{$rtoken_map};
            shift @{$rtoken_type};
        }

        $max_token_index = scalar( @{$rtokens} ) - 1;
        push( @{$rtokens}, SPACE, SPACE, SPACE )
          ;    # extra whitespace simplifies logic
        push( @{$rtoken_map},  0,   0,   0 );     # shouldn't be referenced
        push( @{$rtoken_type}, 'b', 'b', 'b' );

        # initialize for main loop
        if (0) { #<<< this is not necessary
        foreach my $ii ( 0 .. $max_token_index + 3 ) {
            $routput_token_type->[$ii]    = EMPTY_STRING;
            $routput_block_type->[$ii]    = EMPTY_STRING;
            $routput_type_sequence->[$ii] = EMPTY_STRING;
            $routput_indent_flag->[$ii]   = 0;
        }
        }

        $i     = -1;
        $i_tok = -1;

        #-----------------------
        # main tokenization loop
        #-----------------------

        # we are looking at each pre-token of one line and combining them
        # into tokens
        while ( ++$i <= $max_token_index ) {

            # continue looking for the end of a quote
            if ($in_quote) {
                $self->do_FOLLOW_QUOTE();
                last if ( $in_quote || $i > $max_token_index );
            }

            if ( $type ne 'b' && $type ne 'CORE::' ) {

                # try to catch some common errors
                if ( ( $type eq 'n' ) && ( $tok ne '0' ) ) {

                    if ( $last_nonblank_token eq 'eq' ) {
                        $self->complain("Should 'eq' be '==' here ?\n");
                    }
                    elsif ( $last_nonblank_token eq 'ne' ) {
                        $self->complain("Should 'ne' be '!=' here ?\n");
                    }
                    else {
                        # that's all
                    }
                }

                # fix c090, only rotate vars if a new token will be stored
                if ( $i_tok >= 0 ) {

                    $last_last_nonblank_token = $last_nonblank_token;
                    $last_last_nonblank_type  = $last_nonblank_type;

                    $last_nonblank_prototype      = $prototype;
                    $last_nonblank_block_type     = $block_type;
                    $last_nonblank_container_type = $container_type;
                    $last_nonblank_type_sequence  = $type_sequence;
                    $last_nonblank_i              = $i_tok;
                    $last_nonblank_token          = $tok;
                    $last_nonblank_type           = $type;
                }

                # Check for patches
                if ( $is_arrow_or_Z{$last_last_nonblank_type} ) {

                    # Patch for c030: Fix things in case a '->' got separated
                    # from the subsequent identifier by a side comment.  We
                    # need the last_nonblank_token to have a leading -> to
                    # avoid triggering an operator expected error message at
                    # the next '('. See also fix for git #63.
                    if ( $last_last_nonblank_type eq '->' ) {
                        if (   $last_nonblank_type eq 'w'
                            || $last_nonblank_type eq 'i' )
                        {
                            $last_nonblank_token = '->' . $last_nonblank_token;
                            $last_nonblank_type  = 'i';
                        }
                    }

                    # Fix part #3 for git82: propagate type 'Z' though L-R pair
                    elsif ( $last_last_nonblank_type eq 'Z' ) {
                        if ( $last_nonblank_type eq 'R' ) {
                            $last_nonblank_type  = $last_last_nonblank_type;
                            $last_nonblank_token = $last_last_nonblank_token;
                        }
                    }
                    else {
                        # No other patches
                    }
                }
            }

            # store previous token type
            if ( $i_tok >= 0 ) {
                $routput_token_type->[$i_tok]    = $type;
                $routput_block_type->[$i_tok]    = $block_type;
                $routput_type_sequence->[$i_tok] = $type_sequence;
                $routput_indent_flag->[$i_tok]   = $indent_flag;
            }

            # get the next pre-token and type
            # $tok and $type will be modified to make the output token
            my $pre_tok  = $tok  = $rtokens->[$i];      # get the next pre-token
            my $pre_type = $type = $rtoken_type->[$i];  # and type

            # re-initialize various flags for the next output token
            (

                # remember the starting index of this token; we will update $i
                $i_tok,
                $block_type,
                $container_type,
                $type_sequence,
                $indent_flag,
                $prototype,
              )
              = (

                $i,
                EMPTY_STRING,
                EMPTY_STRING,
                EMPTY_STRING,
                0,
                EMPTY_STRING,
              );

            # this pre-token will start an output token
            push( @{$routput_token_list}, $i_tok );

            #---------------------------------------------------
            # The token search leads to one of 5 main END NODES:
            #---------------------------------------------------

            #-----------------------
            # END NODE 1: whitespace
            #-----------------------
            next if ( $pre_type eq 'b' );

            #----------------------
            # END NODE 2: a comment
            #----------------------
            if ( $pre_type eq '#' ) {

                # push non-indenting brace stack Look for a possible
                # non-indenting brace.  This is only used to give a hint in
                # case the file is unbalanced.
                # Hardwired to '#<<<' for efficiency.  We will not use the
                # result later if the pattern has been changed (very unusual).
                if (   $last_nonblank_token eq '{'
                    && $last_nonblank_block_type
                    && $last_nonblank_type_sequence
                    && !$self->[_in_format_skipping_]
                    && $rOpts_non_indenting_braces )
                {
                    my $offset = $rtoken_map->[$i_tok];
                    my $text   = substr( $input_line, $offset, 5 );
                    my $len    = length($text);
                    if (   $len == 4 && $text eq '#<<<'
                        || $len > 4 && $text eq '#<<< ' )
                    {
                        push @{ $self->[_rnon_indenting_brace_stack_] },
                          $last_nonblank_type_sequence;
                    }
                }
                last;
            }

            # continue gathering identifier if necessary
            if ($id_scan_state) {

                if ( $is_sub{$id_scan_state} || $is_package{$id_scan_state} ) {
                    $self->scan_id();
                }
                else {
                    $self->scan_identifier();
                }

                if ($id_scan_state) {

                    # Still scanning ...
                    # Check for side comment between sub and prototype (c061)

                    # done if nothing left to scan on this line
                    last if ( $i > $max_token_index );

                    my ( $next_nonblank_token, $i_next ) =
                      find_next_nonblank_token_on_this_line( $i, $rtokens,
                        $max_token_index );

                    # done if it was just some trailing space
                    last if ( $i_next > $max_token_index );

                    # something remains on the line ... must be a side comment
                    next;
                }

                next if ( ( $i > 0 ) || $type );

                # didn't find any token; start over
                $type = $pre_type;
                $tok  = $pre_tok;
            }

            #-----------------------------------------------------------
            # Combine pre-tokens into digraphs and trigraphs if possible
            #-----------------------------------------------------------

            # See if we can make a digraph...
            # The following tokens are excluded and handled specially:
            # '/=' is excluded because the / might start a pattern.
            # 'x=' is excluded since it might be $x=, with $ on previous line
            # '**' and *= might be typeglobs of punctuation variables
            # I have allowed tokens starting with <, such as <=,
            # because I don't think these could be valid angle operators.
            # test file: storrs4.pl
            if (   $can_start_digraph{$tok}
                && $i < $max_token_index
                && $is_digraph{ $tok . $rtokens->[ $i + 1 ] } )
            {

                my $combine_ok = 1;
                my $test_tok   = $tok . $rtokens->[ $i + 1 ];

                # check for special cases which cannot be combined

                # '//' must be defined_or operator if an operator is expected.
                # TODO: Code for other ambiguous digraphs (/=, x=, **, *=)
                # could be migrated here for clarity

                # Patch for RT#102371, misparsing a // in the following snippet:
                #     state $b //= ccc();
                # The solution is to always accept the digraph (or trigraph)
                # after type 'Z' (possible file handle).  The reason is that
                # sub operator_expected gives TERM expected here, which is
                # wrong in this case.
                if ( $test_tok eq '//' && $last_nonblank_type ne 'Z' ) {

                    # note that here $tok = '/' and the next tok and type is '/'
                    my $blank_after_Z;
                    $expecting =
                      $self->operator_expected( $tok, '/', $blank_after_Z );

                    # Patched for RT#101547, was 'unless ($expecting==OPERATOR)'
                    $combine_ok = 0 if ( $expecting == TERM );
                }

                # Patch for RT #114359: mis-parsing of "print $x ** 0.5;
                # Accept the digraphs '**' only after type 'Z'
                # Otherwise postpone the decision.
                if ( $test_tok eq '**' ) {
                    if ( $last_nonblank_type ne 'Z' ) { $combine_ok = 0 }
                }

                if (

                    # still ok to combine?
                    $combine_ok

                    && ( $test_tok ne '/=' )    # might be pattern
                    && ( $test_tok ne 'x=' )    # might be $x
                    && ( $test_tok ne '*=' )    # typeglob?

                    # Moved above as part of fix for
                    # RT #114359: Missparsing of "print $x ** 0.5;
                    # && ( $test_tok ne '**' )    # typeglob?
                  )
                {
                    $tok = $test_tok;
                    $i++;

                    # Now try to assemble trigraphs.  Note that all possible
                    # perl trigraphs can be constructed by appending a character
                    # to a digraph.
                    $test_tok = $tok . $rtokens->[ $i + 1 ];

                    if ( $is_trigraph{$test_tok} ) {
                        $tok = $test_tok;
                        $i++;
                    }

                    # The only current tetragraph is the double diamond operator
                    # and its first three characters are NOT a trigraph, so
                    # we do can do a special test for it
                    else {
                        if ( $test_tok eq '<<>' ) {
                            $test_tok .= $rtokens->[ $i + 2 ];
                            if ( $is_tetragraph{$test_tok} ) {
                                $tok = $test_tok;
                                $i += 2;
                            }
                        }
                    }
                }
            }

            $type      = $tok;
            $next_tok  = $rtokens->[ $i + 1 ];
            $next_type = $rtoken_type->[ $i + 1 ];

            # expecting an operator here? first try table lookup, then function
            $expecting = $op_expected_table{$last_nonblank_type};
            if ( !defined($expecting) ) {
                my $blank_after_Z = $last_nonblank_type eq 'Z'
                  && ( $i == 0 || $rtoken_type->[ $i - 1 ] eq 'b' );
                $expecting =
                  $self->operator_expected( $tok, $next_type, $blank_after_Z );
            }

            DEBUG_TOKENIZE && do {
                local $LIST_SEPARATOR = ')(';
                my @debug_list = (
                    $last_nonblank_token,         $tok,
                    $next_tok,                    $brace_depth,
                    $rbrace_type->[$brace_depth], $paren_depth,
                    $rparen_type->[$paren_depth],
                );
                print {*STDOUT} "TOKENIZE:(@debug_list)\n";
            };

            # The next token is '$tok'.
            # Now we have to define its '$type'

            #------------------------
            # END NODE 3: a bare word
            #------------------------
            if ( $pre_type eq 'w' ) {
                my $is_last = $self->do_BAREWORD();
                last if ($is_last);
                next;
            }

            # Turn off attribute list on first non-blank, non-bareword,
            # and non-comment (added to fix c038)
            $self->[_in_attribute_list_] = 0;

            #-------------------------------
            # END NODE 4: a string of digits
            #-------------------------------
            if ( $pre_type eq 'd' ) {
                $self->do_DIGITS();
                next;
            }

            #------------------------------------------
            # END NODE 5: everything else (punctuation)
            #------------------------------------------
            my $code = $tokenization_code->{$tok};
            if ($code) {
                $code->($self);
                redo if $in_quote;
            }
        }    ## End main tokenizer loop

        # Store the final token
        if ( $i_tok >= 0 ) {
            $routput_token_type->[$i_tok]    = $type;
            $routput_block_type->[$i_tok]    = $block_type;
            $routput_type_sequence->[$i_tok] = $type_sequence;
            $routput_indent_flag->[$i_tok]   = $indent_flag;
        }

        # Remember last nonblank values
        if ( $type ne 'b' && $type ne '#' ) {

            $last_last_nonblank_token = $last_nonblank_token;
            $last_last_nonblank_type  = $last_nonblank_type;

            $last_nonblank_prototype      = $prototype;
            $last_nonblank_block_type     = $block_type;
            $last_nonblank_container_type = $container_type;
            $last_nonblank_type_sequence  = $type_sequence;
            $last_nonblank_token          = $tok;
            $last_nonblank_type           = $type;
        }

        # reset indentation level if necessary at a sub or package
        # in an attempt to recover from a nesting error
        if ( $level_in_tokenizer < 0 ) {
            if ( $input_line =~ /^\s*(sub|package)\s+(\w+)/ ) {
                reset_indentation_level(0);
                $self->brace_warning("resetting level to 0 at $1 $2\n");
            }
        }

        $self->[_in_quote_] = $in_quote;
        $self->[_quote_target_] =
            $in_quote
          ? $matching_end_token{$quote_character}
              ? $matching_end_token{$quote_character}
              : $quote_character
          : EMPTY_STRING;
        $self->[_rhere_target_list_] = $rhere_target_list;

        return;
    } ## end sub tokenizer_main_loop

    sub tokenizer_wrapup_line {
        my ( $self, $line_of_tokens ) = @_;

        #---------------------------------------------------------
        # Package a line of tokens for shipping back to the caller
        #---------------------------------------------------------

        # Arrays to hold token values for this line:
        my (
            @output_levels,     @output_block_type, @output_type_sequence,
            @output_token_type, @output_tokens
        );

        $line_of_tokens->{_nesting_tokens_0} = $nesting_token_string;

        # Remember starting nesting block string
        my $nesting_block_string_0 = $nesting_block_string;

        #-----------------
        # Loop over tokens
        #-----------------
        # $i is the index of the pretoken which starts this full token
        foreach my $ii ( @{$routput_token_list} ) {

            my $type_i = $routput_token_type->[$ii];

            #----------------------------------------
            # Section 1. Handle a non-sequenced token
            #----------------------------------------
            if ( !$routput_type_sequence->[$ii] ) {

                #-------------------------------
                # Section 1.1. types ';' and 't'
                #-------------------------------
                # - output anonymous 'sub' as keyword (type 'k')
                # - output __END__, __DATA__, and format as type 'k' instead
                #   of ';' to make html colors correct, etc.
                if ( $is_semicolon_or_t{$type_i} ) {
                    my $tok_i = $rtokens->[$ii];
                    if ( $is_END_DATA_format_sub{$tok_i} ) {
                        $type_i = 'k';
                    }
                }

                #----------------------------------------------
                # Section 1.2. Check for an invalid token type.
                #----------------------------------------------
                # This can happen by running perltidy on non-scripts although
                # it could also be bug introduced by programming change.  Perl
                # silently accepts a 032 (^Z) and takes it as the end
                elsif ( !$is_valid_token_type{$type_i} ) {
                    my $val = ord($type_i);
                    $self->warning(
"unexpected character decimal $val ($type_i) in script\n"
                    );
                    $self->[_in_error_] = 1;
                }
                else {
                    # valid token type other than ; and t
                }

                #----------------------------------------------------
                # Section 1.3. Store values for a non-sequenced token
                #----------------------------------------------------
                push( @output_levels,        $level_in_tokenizer );
                push( @output_block_type,    EMPTY_STRING );
                push( @output_type_sequence, EMPTY_STRING );
                push( @output_token_type,    $type_i );

            }

            #------------------------------------
            # Section 2. Handle a sequenced token
            #    One of { [ ( ? : ) ] }
            #------------------------------------
            else {

                # $level_i is the level we will store.  Levels of braces are
                # set so that the leading braces have a HIGHER level than their
                # CONTENTS, which is convenient for indentation.
                my $level_i = $level_in_tokenizer;

                # $tok_i is the PRE-token.  It only equals the token for symbols
                my $tok_i = $rtokens->[$ii];

                # $routput_indent_flag->[$ii] indicates that we need a change
                # in level at a nested ternary, as follows
                #     1 => at a nested ternary ?
                #    -1 => at a nested ternary :
                #     0 => otherwise

                #--------------------------------------------
                # Section 2.1 Handle a level-increasing token
                #--------------------------------------------
                if ( $is_opening_or_ternary_type{$type_i} ) {

                    if ( $type_i eq '?' ) {

                        if ( $routput_indent_flag->[$ii] > 0 ) {
                            $level_in_tokenizer++;

                            # break BEFORE '?' in a nested ternary
                            $level_i = $level_in_tokenizer;
                            $nesting_block_string .= "$nesting_block_flag";

                        }
                    }
                    else {

                        $nesting_token_string .= $tok_i;

                        if ( $type_i eq '{' || $type_i eq 'L' ) {

                            $level_in_tokenizer++;

                            if ( $routput_block_type->[$ii] ) {
                                $nesting_block_flag = 1;
                                $nesting_block_string .= '1';
                            }
                            else {
                                $nesting_block_flag = 0;
                                $nesting_block_string .= '0';
                            }
                        }
                    }
                }

                #---------------------------------------------
                # Section 2.2. Handle a level-decreasing token
                #---------------------------------------------
                elsif ( $is_closing_or_ternary_type{$type_i} ) {

                    if ( $type_i ne ':' ) {
                        my $char = chop $nesting_token_string;
                        if ( $char ne $matching_start_token{$tok_i} ) {
                            $nesting_token_string .= $char . $tok_i;
                        }
                    }

                    if (
                           $type_i eq '}'
                        || $type_i eq 'R'

                        # only the second and higher ? : have levels
                        || $type_i eq ':' && $routput_indent_flag->[$ii] < 0
                      )
                    {

                        $level_i = --$level_in_tokenizer;

                        if ( $level_in_tokenizer < 0 ) {
                            if ( !$self->[_saw_negative_indentation_] ) {
                                $self->[_saw_negative_indentation_] = 1;
                                $self->warning(
                                    "Starting negative indentation\n");
                            }
                        }

                        # restore previous level values
                        if ( length($nesting_block_string) > 1 )
                        {    # true for valid script
                            chop $nesting_block_string;
                            $nesting_block_flag =
                              substr( $nesting_block_string, -1 ) eq '1';
                        }

                    }
                }

                #-----------------------------------------------------
                # Section 2.3. Unexpected sequenced token type - error
                #-----------------------------------------------------
                else {

                    # The tokenizer should only be assigning sequence numbers
                    # to types { [ ( ? ) ] } :
                    DEVEL_MODE && $self->Fault(<<EOM);
unexpected sequence number on token type $type_i with pre-tok=$tok_i
EOM
                }

                #------------------------------------------------
                # Section 2.4. Store values for a sequenced token
                #------------------------------------------------

                # The starting nesting block string, which is used in any .LOG
                # output, should include the first token of the line
                if ( !@output_levels ) {
                    $nesting_block_string_0 = $nesting_block_string;
                }

                # Store values for a sequenced token
                push( @output_levels,        $level_i );
                push( @output_block_type,    $routput_block_type->[$ii] );
                push( @output_type_sequence, $routput_type_sequence->[$ii] );
                push( @output_token_type,    $type_i );

            }
        }    ## End loop to over tokens

        #---------------------
        # Post-loop operations
        #---------------------

        $line_of_tokens->{_nesting_blocks_0} = $nesting_block_string_0;

        # Form and store the tokens
        if (@output_levels) {

            my $im     = shift @{$routput_token_list};
            my $offset = $rtoken_map->[$im];
            foreach my $ii ( @{$routput_token_list} ) {
                my $numc = $rtoken_map->[$ii] - $offset;
                push( @output_tokens, substr( $input_line, $offset, $numc ) );
                $offset += $numc;

                # programming note: it seems most efficient to 'next' out of
                # a critical loop like this as early as possible. So instead
                # of 'if ( DEVEL_MODE && $numc < 0 )' we write:
                next unless DEVEL_MODE;
                next if ( $numc > 0 );

                # Should not happen unless @{$rtoken_map} is corrupted
                $self->Fault(
                    "number of characters is '$numc' but should be >0\n");
            }

            # Form and store the final token of this line
            my $numc = length($input_line) - $offset;
            push( @output_tokens, substr( $input_line, $offset, $numc ) );

            if (DEVEL_MODE) {
                if ( $numc <= 0 ) {

                    # check '$rtoken_map' and '$routput_token_list'
                    $self->Fault(
                        "Number of Characters is '$numc' but should be >0\n");
                }

                # Make sure we didn't gain or lose any characters
                my $test_line = join EMPTY_STRING, @output_tokens;
                if ( $test_line ne $input_line ) {
                    my $len_input = length($input_line);
                    my $len_test  = length($test_line);

                    # check '$rtoken_map' and '$routput_token_list'
                    $self->Fault(<<EOM);
Reconstruted line difers from input; input_length=$len_input test_length=$len_test
input:'$input_line'
test :'$test_line'
EOM
                }
            }
        }

        # Wrap up this line of tokens for shipping to the Formatter
        $line_of_tokens->{_rtoken_type}    = \@output_token_type;
        $line_of_tokens->{_rtokens}        = \@output_tokens;
        $line_of_tokens->{_rblock_type}    = \@output_block_type;
        $line_of_tokens->{_rtype_sequence} = \@output_type_sequence;
        $line_of_tokens->{_rlevels}        = \@output_levels;

        #-----------------------------------------------------------------
        # Compare input indentation with computed levels at closing braces
        #-----------------------------------------------------------------
        # This may provide a useful hint for error location if the file
        # is not balanced in braces.  Closing braces are used because they
        # have a well-defined indentation and can be processed efficiently.
        if ( $output_tokens[0] eq '}' ) {

            my $blk = $output_block_type[0];
            if (
                (
                    # builtin block types without continuation indentation
                    $is_zero_continuation_block_type{$blk}

                    # or a named sub, but skip sub aliases for efficiency,
                    # since this is just for diagnostic info
                    || substr( $blk, 0, 4 ) eq 'sub '
                )

                # and we are not in format skipping
                && !$self->[_in_format_skipping_]
              )
            {

                # subtract 1 space for newline in untrimmed line
                my $untrimmed_input_line = $line_of_tokens->{_line_text};
                my $space_count =
                  length($untrimmed_input_line) - length($input_line) - 1;

                # check for tabs
                if ( $space_count
                    && ord( substr( $untrimmed_input_line, 0, 1 ) ) == ORD_TAB )
                {
                    if ( $untrimmed_input_line =~ /^(\t+)?(\s+)?/ ) {
                        if ($1) { $space_count += length($1) * $tabsize }
                        if ($2) { $space_count += length($2) }
                    }
                }

                # '$guess' = the level according to indentation
                my $guess = int( $space_count / $rOpts_indent_columns );

                # subtract 1 level from guess for --indent-closing-brace
                $guess -= 1 if ($rOpts_indent_closing_brace);

                # subtract 1 from $level for each non-indenting brace level
                my $adjust = @{ $self->[_rnon_indenting_brace_stack_] };

                my $level = $output_levels[0];

                # find the difference between expected and indentation guess
                my $level_diff = $level - $adjust - $guess;

                my $rhash = $self->[_rclosing_brace_indentation_hash_];

                # results are only valid if we guess correctly at the
                # first spaced brace
                if ( $space_count && !defined( $rhash->{valid} ) ) {
                    $rhash->{valid} = !$level_diff;
                }

                # save the result
                my $rhistory_line_number  = $rhash->{rhistory_line_number};
                my $rhistory_level_diff   = $rhash->{rhistory_level_diff};
                my $rhistory_anchor_point = $rhash->{rhistory_anchor_point};

                if ( $rhistory_level_diff->[-1] != $level_diff ) {

                    # Patch for non-indenting-braces: if we guess zero and
                    # match before all non-indenting braces have been found,
                    # it means that we would need negative indentation to
                    # match if/when the brace is found. So we have a problem
                    # from here on. We indicate this with a value 2 instead
                    # of 1 as a signal to stop outputting the table here.
                    my $anchor = 1;
                    if ( $guess == 0 && $adjust > 0 ) { $anchor = 2 }

                    # add an anchor point
                    push @{$rhistory_level_diff},   $level_diff;
                    push @{$rhistory_line_number},  $input_line_number;
                    push @{$rhistory_anchor_point}, $anchor;
                }
                else {

                    # add a movable point following an anchor point
                    if ( $rhistory_anchor_point->[-1] ) {
                        push @{$rhistory_level_diff},   $level_diff;
                        push @{$rhistory_line_number},  $input_line_number;
                        push @{$rhistory_anchor_point}, 0;
                    }

                    # extend a movable point
                    else {
                        $rhistory_line_number->[-1] = $input_line_number;
                    }
                }
            }
        }

        return;
    } ## end sub tokenizer_wrapup_line

} ## end tokenize_this_line

#######################################################################
# Tokenizer routines which assist in identifying token types
#######################################################################

# Define Global '%op_expected_table'
#  = hash table of operator expected values based on last nonblank token

# exceptions to perl's weird parsing rules after type 'Z'
my %is_weird_parsing_rule_exception;

my %is_paren_dollar;

my %is_n_v;

BEGIN {

    # Always expecting TERM following these types:
    # note: this is identical to '@value_requestor_type' defined later.
    # Fix for c250: add new type 'P' for package (expecting VERSION or {}
    # after package NAMESPACE, so expecting TERM)
    # Fix for c250: add new type 'S' for sub (not expecting operator)
    my @q = qw(
      ; ! + x & ?  F J - p / Y : % f U ~ A G j L P S * . | ^ < = [ m { \ > t
      || >= != mm *= => .. !~ == && |= .= pp -= =~ += <= %= ^= x= ~~ ** << /=
      &= // >> ~. &. |. ^.
      ... **= <<= >>= &&= ||= //= <=> !~~ &.= |.= ^.= <<~
    );
    push @q, ',';
    push @q, '(';     # for completeness, not currently a token type
    push @q, '->';    # was previously in UNKNOWN
    @op_expected_table{@q} = (TERM) x scalar(@q);

    # Always UNKNOWN following these types;
    # previously had '->' in this list for c030
    @q = qw( w );
    @op_expected_table{@q} = (UNKNOWN) x scalar(@q);

    # Always expecting OPERATOR ...
    # 'n' and 'v' are currently excluded because they might be VERSION numbers
    # 'i' is currently excluded because it might be a package
    # 'q' is currently excluded because it might be a prototype
    # Fix for c030: removed '->' from this list:
    # Fix for c250: added 'i' because new type 'P' was added
    @q = qw( -- C h R ++ ] Q <> i );    ## n v q );
    push @q, ')';
    @op_expected_table{@q} = (OPERATOR) x scalar(@q);

    # Fix for git #62: added '*' and '%'
    @q = qw( < ? * % );
    @is_weird_parsing_rule_exception{@q} = (1) x scalar(@q);

    @q = qw<) $>;
    @is_paren_dollar{@q} = (1) x scalar(@q);

    @q = qw( n v );
    @is_n_v{@q} = (1) x scalar(@q);

} ## end BEGIN

use constant DEBUG_OPERATOR_EXPECTED => 0;

sub operator_expected {

    # Returns a parameter indicating what types of tokens can occur next

    # Call format:
    #    $op_expected =
    #      $self->operator_expected( $tok, $next_type, $blank_after_Z );
    # where
    #    $tok is the current token
    #    $next_type is the type of the next token (blank or not)
    #    $blank_after_Z = flag for guessing after a type 'Z':
    #       true  if $tok follows type 'Z' with intermediate blank
    #       false if $tok follows type 'Z' with no intermediate blank
    #       ignored if $tok does not follow type 'Z'

    # Many perl symbols have two or more meanings.  For example, '<<'
    # can be a shift operator or a here-doc operator.  The
    # interpretation of these symbols depends on the current state of
    # the tokenizer, which may either be expecting a term or an
    # operator.  For this example, a << would be a shift if an OPERATOR
    # is expected, and a here-doc if a TERM is expected.  This routine
    # is called to make this decision for any current token.  It returns
    # one of three possible values:
    #
    #     OPERATOR - operator expected (or at least, not a term)
    #     UNKNOWN  - can't tell
    #     TERM     - a term is expected (or at least, not an operator)
    #
    # The decision is based on what has been seen so far.  This
    # information is stored in the "$last_nonblank_type" and
    # "$last_nonblank_token" variables.  For example, if the
    # $last_nonblank_type is '=~', then we are expecting a TERM, whereas
    # if $last_nonblank_type is 'n' (numeric), we are expecting an
    # OPERATOR.
    #
    # If a UNKNOWN is returned, the calling routine must guess. A major
    # goal of this tokenizer is to minimize the possibility of returning
    # UNKNOWN, because a wrong guess can spoil the formatting of a
    # script.
    #
    # Adding NEW_TOKENS: it is critically important that this routine be
    # updated to allow it to determine if an operator or term is to be
    # expected after the new token.  Doing this simply involves adding
    # the new token character to one of the regexes in this routine or
    # to one of the hash lists
    # that it uses, which are initialized in the BEGIN section.
    # USES GLOBAL VARIABLES: $last_nonblank_type, $last_nonblank_token,
    # $statement_type

    # When possible, token types should be selected such that we can determine
    # the 'operator_expected' value by a simple hash lookup.  If there are
    # exceptions, that is an indication that a new type is needed.

    my ( $self, $tok, $next_type, $blank_after_Z ) = @_;

    #--------------------------------------------
    # Section 1: Table lookup will get most cases
    #--------------------------------------------

    # Many types are can be obtained by a table lookup.  This typically handles
    # more than half of the calls.  For speed, the caller may try table lookup
    # first before calling this sub.
    my $op_expected = $op_expected_table{$last_nonblank_type};
    if ( defined($op_expected) ) {
        DEBUG_OPERATOR_EXPECTED
          && print {*STDOUT}
"OPERATOR_EXPECTED: Table Lookup; returns $op_expected for last type $last_nonblank_type token $last_nonblank_token\n";
        return $op_expected;
    }

    DEBUG_OPERATOR_EXPECTED
      && print {*STDOUT}
"OPERATOR_EXPECTED: in hardwired table for last type $last_nonblank_type token $last_nonblank_token\n";

    #---------------------------------------------
    # Section 2: Handle special cases if necessary
    #---------------------------------------------

    # Types 'k', '}' and 'Z' depend on context
    # Types 'n', 'v', 'q' also depend on context.

    # identifier...
    # Fix for c250: removed coding for type 'i' because 'i' and new type 'P'
    # are now done by hash table lookup

    #--------------------
    # Section 2A: keyword
    #--------------------
    if ( $last_nonblank_type eq 'k' ) {

        # keywords expecting TERM:
        if ( $expecting_term_token{$last_nonblank_token} ) {

            # Exceptions from TERM:

            # // may follow perl functions which may be unary operators
            # see test file dor.t (defined or);
            if (
                   $tok eq '/'
                && $next_type eq '/'
                && $is_keyword_rejecting_slash_as_pattern_delimiter{
                    $last_nonblank_token}
              )
            {
                return OPERATOR;
            }

            # Patch to allow a ? following 'split' to be a deprecated pattern
            # delimiter.  This patch is coordinated with the omission of split
            # from the list
            # %is_keyword_rejecting_question_as_pattern_delimiter. This patch
            # will force perltidy to guess.
            if (   $tok eq '?'
                && $last_nonblank_token eq 'split' )
            {
                return UNKNOWN;
            }

            return TERM;
        }

        # keywords expecting OPERATOR:
        if ( $expecting_operator_token{$last_nonblank_token} ) {
            return OPERATOR;
        }

        return TERM;

    } ## end type 'k'

    #------------------------------------
    # Section 2B: Closing container token
    #------------------------------------

    # Note that the actual token for type '}' may also be a ')'.

    # Also note that $last_nonblank_token is not the token corresponding to
    # $last_nonblank_type when the type is a closing container.  In that
    # case it is the token before the corresponding opening container token.
    # So for example, for this snippet
    #       $a = do { BLOCK } / 2;
    # the $last_nonblank_token is 'do' when $last_nonblank_type eq '}'.

    if ( $last_nonblank_type eq '}' ) {

        # handle something after 'do' and 'eval'
        if ( $is_block_operator{$last_nonblank_token} ) {

            # something like $a = do { BLOCK } / 2;
            return OPERATOR;    # block mode following }
        }

        #       $last_nonblank_token =~ /^(\)|\$|\-\>)/
        if ( $is_paren_dollar{ substr( $last_nonblank_token, 0, 1 ) }
            || substr( $last_nonblank_token, 0, 2 ) eq '->' )
        {
            if ( $last_nonblank_token eq '$' ) { return UNKNOWN }
            return OPERATOR;
        }

        # Check for smartmatch operator before preceding brace or square
        # bracket.  For example, at the ? after the ] in the following
        # expressions we are expecting an operator:
        #
        # qr/3/ ~~ ['1234'] ? 1 : 0;
        # map { $_ ~~ [ '0', '1' ] ? 'x' : 'o' } @a;
        if ( $last_nonblank_token eq '~~' ) {
            return OPERATOR;
        }

        # A right brace here indicates the end of a simple block.  All
        # non-structural right braces have type 'R' all braces associated with
        # block operator keywords have been given those keywords as
        # "last_nonblank_token" and caught above.  (This statement is order
        # dependent, and must come after checking $last_nonblank_token).

        # patch for dor.t (defined or).
        if (   $tok eq '/'
            && $next_type eq '/'
            && $last_nonblank_token eq ']' )
        {
            return OPERATOR;
        }

        # Patch for RT #116344: misparse a ternary operator after an
        # anonymous hash, like this:
        #   return ref {} ? 1 : 0;
        # The right brace should really be marked type 'R' in this case,
        # and it is safest to return an UNKNOWN here. Expecting a TERM will
        # cause the '?' to always be interpreted as a pattern delimiter
        # rather than introducing a ternary operator.
        if ( $tok eq '?' ) {
            return UNKNOWN;
        }
        return TERM;

    } ## end type '}'

    #-------------------------------
    # Section 2C: number or v-string
    #-------------------------------
    # An exception is for VERSION numbers a 'use' statement. It has the format
    #     use Module VERSION LIST
    # We could avoid this exception by writing a special sub to parse 'use'
    # statements and perhaps mark these numbers with a new type V (for VERSION)
    if ( $is_n_v{$last_nonblank_type} ) {
        if ( $statement_type eq 'use' ) {
            return UNKNOWN;
        }
        return OPERATOR;
    }

    #---------------------
    # Section 2D: qw quote
    #---------------------
    # TODO: labeled prototype words would better be given type 'A' or maybe
    # 'J'; not 'q'; or maybe mark as type 'Y'?
    if ( $last_nonblank_type eq 'q' ) {
        if ( $last_nonblank_token eq 'prototype' ) {
            return TERM;
        }

        # update for --use-feature=class (rt145706):
        # Look for class VERSION after possible attribute, as in
        #    class Example::Subclass : isa(Example::Base) 1.345 { ... }
        if ( $statement_type =~ /^package\b/ ) {
            return TERM;
        }

        # everything else
        return OPERATOR;
    }

    #-----------------------------------
    # Section 2E: file handle or similar
    #-----------------------------------
    if ( $last_nonblank_type eq 'Z' ) {

        # angle.t
        if ( $last_nonblank_token =~ /^\w/ ) {
            return UNKNOWN;
        }

        # Exception to weird parsing rules for 'x(' ... see case b1205:
        # In something like 'print $vv x(...' the x is an operator;
        # Likewise in 'print $vv x$ww' the x is an operator (case b1207)
        # otherwise x follows the weird parsing rules.
        if ( $tok eq 'x' && $next_type =~ /^[\(\$\@\%]$/ ) {
            return OPERATOR;
        }

        # The 'weird parsing rules' of next section do not work for '<' and '?'
        # It is best to mark them as unknown.  Test case:
        #  print $fh <DATA>;
        if ( $is_weird_parsing_rule_exception{$tok} ) {
            return UNKNOWN;
        }

        # For possible file handle like "$a", Perl uses weird parsing rules.
        # For example:
        # print $a/2,"/hi";   - division
        # print $a / 2,"/hi"; - division
        # print $a/ 2,"/hi";  - division
        # print $a /2,"/hi";  - pattern (and error)!
        # Some examples where this logic works okay, for '&','*','+':
        #    print $fh &xsi_protos(@mods);
        #    my $x = new $CompressClass *FH;
        #    print $OUT +( $count % 15 ? ", " : "\n\t" );
        if (   $blank_after_Z
            && $next_type ne 'b' )
        {
            return TERM;
        }

        # Note that '?' and '<' have been moved above
        # ( $tok =~ /^([x\/\+\-\*\%\&\.\?\<]|\>\>)$/ ) {
        if ( $tok =~ /^([x\/\+\-\*\%\&\.]|\>\>)$/ ) {

            # Do not complain in 'use' statements, which have special syntax.
            # For example, from RT#130344:
            #   use lib $FindBin::Bin . '/lib';
            if ( $statement_type ne 'use' ) {
                $self->complain(
"operator in possible indirect object location not recommended\n"
                );
            }
            return OPERATOR;
        }

        # all other cases

        return UNKNOWN;
    }

    #--------------------------
    # Section 2F: anything else
    #--------------------------
    return UNKNOWN;

} ## end sub operator_expected

sub new_statement_ok {

    # Returns:
    #   true if a new statement can begin here
    #   false otherwise

    # USES GLOBAL VARIABLES: $last_nonblank_token, $last_nonblank_type,
    # $brace_depth, $rbrace_type

    # Uses:
    # - See if a 'class' statement can occur here
    # - See if a keyword begins at a new statement; i.e. is an 'if' a
    #   block if or a trailing if?  Also see if 'format' starts a statement.
    # - Decide if a ':' is part of a statement label (not a ternary)

    # Curly braces are tricky because some small blocks do not get marked as
    # blocks..

    # if it follows an opening curly brace..
    if ( $last_nonblank_token eq '{' ) {

        # The safe thing is to return true in all cases because:
        # - a ternary ':' cannot occur here
        # - an 'if' here, for example, cannot be a trailing if
        # See test case c231 for an example.
        # This works but could be improved, if necessary, by returning
        #   'false' at obvious non-blocks.
        return 1;
    }

    # if it follows a closing code block curly brace..
    elsif ($last_nonblank_token eq '}'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # a new statement can follow certain closing block braces ...
        # FIXME: The following has worked well but returns true in some cases
        # where it really should not.  We could fix this by either excluding
        # certain blocks, like sort/map/grep/eval/asub or by just including
        # certain blocks.
        return $rbrace_type->[$brace_depth];
    }

    # otherwise, it is a label if and only if it follows a ';' (real or fake)
    # or another label
    else {
        return ( $last_nonblank_type eq ';' || $last_nonblank_type eq 'J' );
    }
} ## end sub new_statement_ok

sub code_block_type {

    # Decide if this is a block of code, and its type.
    # Must be called only when $type = $token = '{'
    # The problem is to distinguish between the start of a block of code
    # and the start of an anonymous hash reference
    # Returns "" if not code block, otherwise returns 'last_nonblank_token'
    # to indicate the type of code block.  (For example, 'last_nonblank_token'
    # might be 'if' for an if block, 'else' for an else block, etc).
    # USES GLOBAL VARIABLES: $last_nonblank_token, $last_nonblank_type,
    # $last_nonblank_block_type, $brace_depth, $rbrace_type

    # handle case of multiple '{'s

# print "BLOCK_TYPE EXAMINING: type=$last_nonblank_type tok=$last_nonblank_token\n";

    my ( $self, $i, $rtokens, $rtoken_type, $max_token_index ) = @_;
    if (   $last_nonblank_token eq '{'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        if ( $rbrace_type->[$brace_depth] ) {
            return $self->decide_if_code_block( $i, $rtokens, $rtoken_type,
                $max_token_index );
        }

        # cannot start a code block within an anonymous hash
        else {
            return EMPTY_STRING;
        }
    }

    elsif ( $last_nonblank_token eq ';' ) {

        # an opening brace where a statement may appear is probably
        # a code block but might be and anonymous hash reference
        return $self->decide_if_code_block( $i, $rtokens, $rtoken_type,
            $max_token_index );
    }

    # handle case of '}{'
    elsif ($last_nonblank_token eq '}'
        && $last_nonblank_type eq $last_nonblank_token )
    {

        # a } { situation ...
        # could be hash reference after code block..(blktype1.t)
        if ($last_nonblank_block_type) {
            return $self->decide_if_code_block( $i, $rtokens, $rtoken_type,
                $max_token_index );
        }

        # must be a block if it follows a closing hash reference
        else {
            return $last_nonblank_token;
        }
    }

    #--------------------------------------------------------------
    # NOTE: braces after type characters start code blocks, but for
    # simplicity these are not identified as such.  See also
    # sub is_non_structural_brace.
    #--------------------------------------------------------------

##    elsif ( $last_nonblank_type eq 't' ) {
##       return $last_nonblank_token;
##    }

    # brace after label:
    elsif ( $last_nonblank_type eq 'J' ) {
        return $last_nonblank_token;
    }

# otherwise, look at previous token.  This must be a code block if
# it follows any of these:
# /^(BEGIN|END|CHECK|INIT|AUTOLOAD|DESTROY|UNITCHECK|continue|if|elsif|else|unless|do|while|until|eval|for|foreach|map|grep|sort)$/
    elsif ($is_code_block_token{$last_nonblank_token}
        || $is_grep_alias{$last_nonblank_token} )
    {

        # Bug Patch: Note that the opening brace after the 'if' in the following
        # snippet is an anonymous hash ref and not a code block!
        #   print 'hi' if { x => 1, }->{x};
        # We can identify this situation because the last nonblank type
        # will be a keyword (instead of a closing paren)
        if (
            $last_nonblank_type eq 'k'
            && (   $last_nonblank_token eq 'if'
                || $last_nonblank_token eq 'unless' )
          )
        {
            return EMPTY_STRING;
        }
        else {
            return $last_nonblank_token;
        }
    }

    # or a sub or package BLOCK
    # Fixed for c250 to include new package type 'P', and change 'i' to 'S'
    elsif (
           $last_nonblank_type eq 'P'
        || $last_nonblank_type eq 'S'
        || ( $last_nonblank_type eq 't'
            && substr( $last_nonblank_token, 0, 3 ) eq 'sub' )
      )
    {
        return $last_nonblank_token;
    }

    elsif ( $statement_type =~ /^(sub|package)\b/ ) {
        return $statement_type;
    }

    # user-defined subs with block parameters (like grep/map/eval)
    elsif ( $last_nonblank_type eq 'G' ) {
        return $last_nonblank_token;
    }

    # check bareword
    elsif ( $last_nonblank_type eq 'w' ) {

        # check for syntax 'use MODULE LIST'
        # This fixes b1022 b1025 b1027 b1028 b1029 b1030 b1031
        return EMPTY_STRING if ( $statement_type eq 'use' );

        return $self->decide_if_code_block( $i, $rtokens, $rtoken_type,
            $max_token_index );
    }

    # Patch for bug # RT #94338 reported by Daniel Trizen
    # for-loop in a parenthesized block-map triggering an error message:
    #    map( { foreach my $item ( '0', '1' ) { print $item} } qw(a b c) );
    # Check for a code block within a parenthesized function call
    elsif ( $last_nonblank_token eq '(' ) {
        my $paren_type = $rparen_type->[$paren_depth];

        #                   /^(map|grep|sort)$/
        if ( $paren_type && $is_sort_map_grep{$paren_type} ) {

            # We will mark this as a code block but use type 't' instead
            # of the name of the containing function.  This will allow for
            # correct parsing but will usually produce better formatting.
            # Braces with block type 't' are not broken open automatically
            # in the formatter as are other code block types, and this usually
            # works best.
            return 't';    # (Not $paren_type)
        }
        else {
            return EMPTY_STRING;
        }
    }

    # handle unknown syntax ') {'
    # we previously appended a '()' to mark this case
    elsif ( $last_nonblank_token =~ /\(\)$/ ) {
        return $last_nonblank_token;
    }

    # anything else must be anonymous hash reference
    else {
        return EMPTY_STRING;
    }
} ## end sub code_block_type

sub decide_if_code_block {

    # USES GLOBAL VARIABLES: $last_nonblank_token
    my ( $self, $i, $rtokens, $rtoken_type, $max_token_index ) = @_;

    my ( $next_nonblank_token, $i_next ) =
      $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

    # we are at a '{' where a statement may appear.
    # We must decide if this brace starts an anonymous hash or a code
    # block.
    # return "" if anonymous hash, and $last_nonblank_token otherwise

    # initialize to be code BLOCK
    my $code_block_type = $last_nonblank_token;

    # Check for the common case of an empty anonymous hash reference:
    # Maybe something like sub { { } }
    if ( $next_nonblank_token eq '}' ) {
        $code_block_type = EMPTY_STRING;
    }

    else {

        # To guess if this '{' is an anonymous hash reference, look ahead
        # and test as follows:
        #
        # it is a hash reference if next come:
        #   - a string or digit followed by a comma or =>
        #   - bareword followed by =>
        # otherwise it is a code block
        #
        # Examples of anonymous hash ref:
        # {'aa',};
        # {1,2}
        #
        # Examples of code blocks:
        # {1; print "hello\n", 1;}
        # {$a,1};

        # We are only going to look ahead one more (nonblank/comment) line.
        # Strange formatting could cause a bad guess, but that's unlikely.
        my @pre_types;
        my @pre_tokens;

        # Ignore the rest of this line if it is a side comment
        if ( $next_nonblank_token ne '#' ) {
            @pre_types  = @{$rtoken_type}[ $i + 1 .. $max_token_index ];
            @pre_tokens = @{$rtokens}[ $i + 1 .. $max_token_index ];
        }

        # Here 20 is arbitrary but generous, and prevents wasting lots of time
        # in mangled files
        my ( $rpre_tokens, $rpre_types ) =
          $self->peek_ahead_for_n_nonblank_pre_tokens(20);
        if ( defined($rpre_types) && @{$rpre_types} ) {
            push @pre_types,  @{$rpre_types};
            push @pre_tokens, @{$rpre_tokens};
        }

        # put a sentinel token to simplify stopping the search
        push @pre_types, '}';
        push @pre_types, '}';

        my $jbeg = 0;
        $jbeg = 1 if $pre_types[0] eq 'b';

        # first look for one of these
        #  - bareword
        #  - bareword with leading -
        #  - digit
        #  - quoted string
        my $j = $jbeg;
        if ( $pre_types[$j] =~ /^[\'\"]/ ) {

            # find the closing quote; don't worry about escapes
            my $quote_mark = $pre_types[$j];
            foreach my $k ( $j + 1 .. @pre_types - 2 ) {
                if ( $pre_types[$k] eq $quote_mark ) {
                    $j = $k + 1;
                    last;
                }
            }
        }
        elsif ( $pre_types[$j] eq 'd' ) {
            $j++;
        }
        elsif ( $pre_types[$j] eq 'w' ) {
            $j++;
        }
        elsif ( $pre_types[$j] eq '-' && $pre_types[ ++$j ] eq 'w' ) {
            $j++;
        }
        else {
            # none of the above
        }
        if ( $j > $jbeg ) {

            $j++ if $pre_types[$j] eq 'b';

            # Patched for RT #95708
            if (

                # it is a comma which is not a pattern delimiter except for qw
                (
                    $pre_types[$j] eq ','
                    && !$is_q_qq_qx_qr_s_y_tr_m{ $pre_tokens[$jbeg] }
                )

                # or a =>
                || ( $pre_types[$j] eq '=' && $pre_types[ ++$j ] eq '>' )
              )
            {
                $code_block_type = EMPTY_STRING;
            }
        }

        if ($code_block_type) {

            # Patch for cases b1085 b1128: It is uncertain if this is a block.
            # If this brace follows a bareword, then append a space as a signal
            # to the formatter that this may not be a block brace.  To find the
            # corresponding code in Formatter.pm search for 'b1085'.
            $code_block_type .= SPACE if ( $code_block_type =~ /^\w/ );
        }
    }

    return $code_block_type;
} ## end sub decide_if_code_block

sub report_unexpected {

    # report unexpected token type and show where it is
    # USES GLOBAL VARIABLES: (none)
    my ( $self, $rcall_hash ) = @_;

    my $found           = $rcall_hash->{found};
    my $expecting       = $rcall_hash->{expecting};
    my $i_tok           = $rcall_hash->{i_tok};
    my $last_nonblank_i = $rcall_hash->{last_nonblank_i};
    my $rpretoken_map   = $rcall_hash->{rpretoken_map};
    my $rpretoken_type  = $rcall_hash->{rpretoken_type};
    my $input_line      = $rcall_hash->{input_line};

    if ( ++$self->[_unexpected_error_count_] <= MAX_NAG_MESSAGES ) {
        my $msg = "found $found where $expecting expected";
        my $pos = $rpretoken_map->[$i_tok];
        $self->interrupt_logfile();
        my $input_line_number = $self->[_last_line_number_];
        my ( $offset, $numbered_line, $underline ) =
          make_numbered_line( $input_line_number, $input_line, $pos );
        $underline = write_on_underline( $underline, $pos - $offset, '^' );

        my $trailer = EMPTY_STRING;
        if ( ( $i_tok > 0 ) && ( $last_nonblank_i >= 0 ) ) {
            my $pos_prev = $rpretoken_map->[$last_nonblank_i];
            my $num;
            if ( $rpretoken_type->[ $i_tok - 1 ] eq 'b' ) {
                $num = $rpretoken_map->[ $i_tok - 1 ] - $pos_prev;
            }
            else {
                $num = $pos - $pos_prev;
            }
            if ( $num > 40 ) { $num = 40; $pos_prev = $pos - 40; }

            $underline =
              write_on_underline( $underline, $pos_prev - $offset, '-' x $num );
            $trailer = " (previous token underlined)";
        }
        $underline =~ s/\s+$//;
        $self->warning( $numbered_line . "\n" );
        $self->warning( $underline . "\n" );
        $self->warning( $msg . $trailer . "\n" );
        $self->resume_logfile();
    }
    return;
} ## end sub report_unexpected

my %is_sigil_or_paren;
my %is_R_closing_sb;

BEGIN {

    my @q = qw< $ & % * @ ) >;
    @is_sigil_or_paren{@q} = (1) x scalar(@q);

    @q = qw(R ]);
    @is_R_closing_sb{@q} = (1) x scalar(@q);
} ## end BEGIN

sub is_non_structural_brace {

    # Decide if a brace or bracket is structural or non-structural
    # by looking at the previous token and type
    # USES GLOBAL VARIABLES: $last_nonblank_type, $last_nonblank_token

    # EXPERIMENTAL: Mark slices as structural; idea was to improve formatting.
    # Tentatively deactivated because it caused the wrong operator expectation
    # for this code:
    #      $user = @vars[1] / 100;
    # Must update sub operator_expected before re-implementing.
    # if ( $last_nonblank_type eq 'i' && $last_nonblank_token =~ /^@/ ) {
    #    return 0;
    # }

    #--------------------------------------------------------------
    # NOTE: braces after type characters start code blocks, but for
    # simplicity these are not identified as such.  See also
    # sub code_block_type
    #--------------------------------------------------------------

    ##if ($last_nonblank_type eq 't') {return 0}

    # otherwise, it is non-structural if it is decorated
    # by type information.
    # For example, the '{' here is non-structural:   ${xxx}
    # Removed '::' to fix c074
    ## $last_nonblank_token =~ /^([\$\@\*\&\%\)]|->|::)/
    return (
        ## $last_nonblank_token =~ /^([\$\@\*\&\%\)]|->)/
        $is_sigil_or_paren{ substr( $last_nonblank_token, 0, 1 ) }
          || substr( $last_nonblank_token, 0, 2 ) eq '->'

          # or if we follow a hash or array closing curly brace or bracket
          # For example, the second '{' in this is non-structural: $a{'x'}{'y'}
          # because the first '}' would have been given type 'R'
          ##|| $last_nonblank_type =~ /^([R\]])$/
          || $is_R_closing_sb{$last_nonblank_type}
    );
} ## end sub is_non_structural_brace

#######################################################################
# Tokenizer routines for tracking container nesting depths
#######################################################################

# The following routines keep track of nesting depths of the nesting
# types, ( [ { and ?.  This is necessary for determining the indentation
# level, and also for debugging programs.  Not only do they keep track of
# nesting depths of the individual brace types, but they check that each
# of the other brace types is balanced within matching pairs.  For
# example, if the program sees this sequence:
#
#         {  ( ( ) }
#
# then it can determine that there is an extra left paren somewhere
# between the { and the }.  And so on with every other possible
# combination of outer and inner brace types.  For another
# example:
#
#         ( [ ..... ]  ] )
#
# which has an extra ] within the parens.
#
# The brace types have indexes 0 .. 3 which are indexes into
# the matrices.
#
# The pair ? : are treated as just another nesting type, with ? acting
# as the opening brace and : acting as the closing brace.
#
# The matrix
#
#  $rdepth_array->[$a][$b][ $rcurrent_depth->[$a] ] = $rcurrent_depth->[$b];
#
# saves the nesting depth of brace type $b (where $b is either of the other
# nesting types) when brace type $a enters a new depth.  When this depth
# decreases, a check is made that the current depth of brace types $b is
# unchanged, or otherwise there must have been an error.  This can
# be very useful for localizing errors, particularly when perl runs to
# the end of a large file (such as this one) and announces that there
# is a problem somewhere.
#
# A numerical sequence number is maintained for every nesting type,
# so that each matching pair can be uniquely identified in a simple
# way.

sub increase_nesting_depth {
    my ( $self, $aa, $pos ) = @_;

    # USES GLOBAL VARIABLES: $rcurrent_depth,
    # $rcurrent_sequence_number, $rdepth_array,
    # $rstarting_line_of_current_depth, $statement_type
    my $cd_aa = ++$rcurrent_depth->[$aa];
    $total_depth++;
    $rtotal_depth->[$aa][$cd_aa] = $total_depth;
    my $input_line_number = $self->[_last_line_number_];
    my $input_line        = $self->[_line_of_text_];

    # Sequence numbers increment by number of items.  This keeps
    # a unique set of numbers but still allows the relative location
    # of any type to be determined.

    # make a new unique sequence number
    my $seqno = $next_sequence_number++;

    $rcurrent_sequence_number->[$aa][$cd_aa] = $seqno;

    $rstarting_line_of_current_depth->[$aa][$cd_aa] =
      [ $input_line_number, $input_line, $pos ];

    for my $bb ( 0 .. @closing_brace_names - 1 ) {
        next if ( $bb == $aa );
        $rdepth_array->[$aa][$bb][$cd_aa] = $rcurrent_depth->[$bb];
    }

    # set a flag for indenting a nested ternary statement
    my $indent = 0;
    if ( $aa == QUESTION_COLON ) {
        $rnested_ternary_flag->[$cd_aa] = 0;
        if ( $cd_aa > 1 ) {
            if ( $rnested_ternary_flag->[ $cd_aa - 1 ] == 0 ) {
                my $pdepth = $rtotal_depth->[$aa][ $cd_aa - 1 ];
                if ( $pdepth == $total_depth - 1 ) {
                    $indent = 1;
                    $rnested_ternary_flag->[ $cd_aa - 1 ] = -1;
                }
            }
        }
    }

    # Fix part #1 for git82: save last token type for propagation of type 'Z'
    $rnested_statement_type->[$aa][$cd_aa] =
      [ $statement_type, $last_nonblank_type, $last_nonblank_token ];
    $statement_type = EMPTY_STRING;
    return ( $seqno, $indent );
} ## end sub increase_nesting_depth

sub is_balanced_closing_container {

    # Return true if a closing container can go here without error
    # Return false if not
    my ($aa) = @_;

    # cannot close if there was no opening
    my $cd_aa = $rcurrent_depth->[$aa];
    return if ( $cd_aa <= 0 );

    # check that any other brace types $bb contained within would be balanced
    for my $bb ( 0 .. @closing_brace_names - 1 ) {
        next if ( $bb == $aa );
        return
          if ( $rdepth_array->[$aa][$bb][$cd_aa] != $rcurrent_depth->[$bb] );
    }

    # OK, everything will be balanced
    return 1;
} ## end sub is_balanced_closing_container

sub decrease_nesting_depth {

    my ( $self, $aa, $pos ) = @_;

    # USES GLOBAL VARIABLES: $rcurrent_depth,
    # $rcurrent_sequence_number, $rdepth_array, $rstarting_line_of_current_depth
    # $statement_type
    my $seqno             = 0;
    my $input_line_number = $self->[_last_line_number_];
    my $input_line        = $self->[_line_of_text_];

    my $outdent = 0;
    $total_depth--;
    my $cd_aa = $rcurrent_depth->[$aa];
    if ( $cd_aa > 0 ) {

        # set a flag for un-indenting after seeing a nested ternary statement
        $seqno = $rcurrent_sequence_number->[$aa][$cd_aa];
        if ( $aa == QUESTION_COLON ) {
            $outdent = $rnested_ternary_flag->[$cd_aa];
        }

        # Fix part #2 for git82: use saved type for propagation of type 'Z'
        # through type L-R braces.  Perl seems to allow ${bareword}
        # as an indirect object, but nothing much more complex than that.
        ( $statement_type, my $saved_type, my $saved_token ) =
          @{ $rnested_statement_type->[$aa][ $rcurrent_depth->[$aa] ] };
        if (   $aa == BRACE
            && $saved_type eq 'Z'
            && $last_nonblank_type eq 'w'
            && $rbrace_structural_type->[$brace_depth] eq 'L' )
        {
            $last_nonblank_type = $saved_type;
        }

        # check that any brace types $bb contained within are balanced
        for my $bb ( 0 .. @closing_brace_names - 1 ) {
            next if ( $bb == $aa );

            if ( $rdepth_array->[$aa][$bb][$cd_aa] != $rcurrent_depth->[$bb] ) {
                my $diff =
                  $rcurrent_depth->[$bb] - $rdepth_array->[$aa][$bb][$cd_aa];

                # don't whine too many times
                my $saw_brace_error = $self->get_saw_brace_error();
                if (
                    $saw_brace_error <= MAX_NAG_MESSAGES

                    # if too many closing types have occurred, we probably
                    # already caught this error
                    && ( ( $diff > 0 ) || ( $saw_brace_error <= 0 ) )
                  )
                {
                    $self->interrupt_logfile();
                    my $rsl = $rstarting_line_of_current_depth->[$aa][$cd_aa];
                    my $sl  = $rsl->[0];
                    my $rel = [ $input_line_number, $input_line, $pos ];
                    my $el  = $rel->[0];
                    my ($ess);

                    if ( $diff == 1 || $diff == -1 ) {
                        $ess = EMPTY_STRING;
                    }
                    else {
                        $ess = 's';
                    }
                    my $bname =
                      ( $diff > 0 )
                      ? $opening_brace_names[$bb]
                      : $closing_brace_names[$bb];
                    $self->write_error_indicator_pair( @{$rsl}, '^' );
                    my $msg = <<"EOM";
Found $diff extra $bname$ess between $opening_brace_names[$aa] on line $sl and $closing_brace_names[$aa] on line $el
EOM

                    if ( $diff > 0 ) {
                        my $rml =
                          $rstarting_line_of_current_depth->[$bb]
                          [ $rcurrent_depth->[$bb] ];
                        my $ml = $rml->[0];
                        $msg .=
"    The most recent un-matched $bname is on line $ml\n";
                        $self->write_error_indicator_pair( @{$rml}, '^' );
                    }
                    $self->write_error_indicator_pair( @{$rel}, '^' );
                    $self->warning($msg);
                    $self->resume_logfile();
                }
                $self->increment_brace_error();
                if ( $bb eq BRACE ) { $self->[_show_indentation_table_] = 1 }
            }
        }
        $rcurrent_depth->[$aa]--;
    }
    else {

        my $saw_brace_error = $self->get_saw_brace_error();
        if ( $saw_brace_error <= MAX_NAG_MESSAGES ) {
            my $msg = <<"EOM";
There is no previous $opening_brace_names[$aa] to match a $closing_brace_names[$aa] on line $input_line_number
EOM
            $self->indicate_error( $msg, $input_line_number, $input_line, $pos,
                '^' );
        }
        $self->increment_brace_error();
        if ( $aa eq BRACE ) { $self->[_show_indentation_table_] = 1 }

        # keep track of errors in braces alone (ignoring ternary nesting errors)
        $self->[_true_brace_error_count_]++
          if ( $closing_brace_names[$aa] ne "':'" );
    }
    return ( $seqno, $outdent );
} ## end sub decrease_nesting_depth

sub check_final_nesting_depths {

    # USES GLOBAL VARIABLES: $rcurrent_depth, $rstarting_line_of_current_depth
    my $self = shift;

    for my $aa ( 0 .. @closing_brace_names - 1 ) {

        my $cd_aa = $rcurrent_depth->[$aa];
        if ($cd_aa) {
            my $rsl = $rstarting_line_of_current_depth->[$aa][$cd_aa];
            my $sl  = $rsl->[0];
            my $msg = <<"EOM";
Final nesting depth of $opening_brace_names[$aa]s is $cd_aa
The most recent un-matched $opening_brace_names[$aa] is on line $sl
EOM
            $self->indicate_error( $msg, @{$rsl}, '^' );
            $self->increment_brace_error();
            if ( $aa eq BRACE ) { $self->[_show_indentation_table_] = 1 }
        }
    }
    return;
} ## end sub check_final_nesting_depths

#######################################################################
# Tokenizer routines for looking ahead in input stream
#######################################################################

sub peek_ahead_for_n_nonblank_pre_tokens {

    # returns next n pretokens if they exist
    # returns undef's if hits eof without seeing any pretokens
    # USES GLOBAL VARIABLES: (none)
    my ( $self, $max_pretokens ) = @_;
    my $line;
    my $i = 0;
    my ( $rpre_tokens, $rmap, $rpre_types );

    while ( $line = $self->peek_ahead( $i++ ) ) {
        $line =~ s/^\s+//;                 # trim leading blanks
        next if ( length($line) <= 0 );    # skip blank
        next if ( $line =~ /^#/ );         # skip comment
        ( $rpre_tokens, $rmap, $rpre_types ) =
          pre_tokenize( $line, $max_pretokens );
        last;
    }
    return ( $rpre_tokens, $rpre_types );
} ## end sub peek_ahead_for_n_nonblank_pre_tokens

# look ahead for next non-blank, non-comment line of code
sub peek_ahead_for_nonblank_token {

    # USES GLOBAL VARIABLES: (none)
    my ( $self, $rtokens, $max_token_index ) = @_;
    my $line;
    my $i = 0;

    while ( $line = $self->peek_ahead( $i++ ) ) {
        $line =~ s/^\s+//;                 # trim leading blanks
        next if ( length($line) <= 0 );    # skip blank
        next if ( $line =~ /^#/ );         # skip comment

        # Updated from 2 to 3 to get trigraphs, added for case b1175
        my ( $rtok, $rmap, $rtype ) = pre_tokenize( $line, 3 );
        my $j = $max_token_index + 1;

        foreach my $tok ( @{$rtok} ) {
            last if ( $tok =~ "\n" );
            $rtokens->[ ++$j ] = $tok;
        }
        last;
    }
    return;
} ## end sub peek_ahead_for_nonblank_token

#######################################################################
# Tokenizer guessing routines for ambiguous situations
#######################################################################

sub guess_if_pattern_or_conditional {

    # this routine is called when we have encountered a ? following an
    # unknown bareword, and we must decide if it starts a pattern or not
    # input parameters:
    #   $i - token index of the ? starting possible pattern
    # output parameters:
    #   $is_pattern = 0 if probably not pattern,  =1 if probably a pattern
    #   msg = a warning or diagnostic message
    # USES GLOBAL VARIABLES: $last_nonblank_token

    my ( $self, $i, $rtokens, $rtoken_type, $rtoken_map, $max_token_index ) =
      @_;
    my $is_pattern = 0;
    my $msg        = "guessing that ? after $last_nonblank_token starts a ";

    if ( $i >= $max_token_index ) {
        $msg .= "conditional (no end to pattern found on the line)\n";
    }
    else {
        my $ibeg = $i;
        $i = $ibeg + 1;
        my $next_token = $rtokens->[$i];    # first token after ?

        # look for a possible ending ? on this line..
        my $in_quote        = 1;
        my $quote_depth     = 0;
        my $quote_character = EMPTY_STRING;
        my $quote_pos       = 0;
        my $quoted_string;
        (

            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string,

        ) = $self->follow_quoted_string(

            $ibeg,
            $in_quote,
            $rtokens,
            $rtoken_type,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $max_token_index,

        );

        if ($in_quote) {

            # we didn't find an ending ? on this line,
            # so we bias towards conditional
            $is_pattern = 0;
            $msg .= "conditional (no ending ? on this line)\n";

            # we found an ending ?, so we bias towards a pattern
        }
        else {

            # Watch out for an ending ? in quotes, like this
            #    my $case_flag = File::Spec->case_tolerant ? '(?i)' : '';
            my $s_quote = 0;
            my $d_quote = 0;
            my $colons  = 0;
            foreach my $ii ( $ibeg + 1 .. $i - 1 ) {
                my $tok = $rtokens->[$ii];
                if ( $tok eq ":" ) { $colons++ }
                if ( $tok eq "'" ) { $s_quote++ }
                if ( $tok eq '"' ) { $d_quote++ }
            }
            if ( $s_quote % 2 || $d_quote % 2 || $colons ) {
                $is_pattern = 0;
                $msg .= "found ending ? but unbalanced quote chars\n";
            }
            elsif (
                $self->pattern_expected( $i, $rtokens, $max_token_index ) >= 0 )
            {
                $is_pattern = 1;
                $msg .= "pattern (found ending ? and pattern expected)\n";
            }
            else {
                $msg .= "pattern (uncertain, but found ending ?)\n";
            }
        }
    }
    return ( $is_pattern, $msg );
} ## end sub guess_if_pattern_or_conditional

my %is_known_constant;
my %is_known_function;

BEGIN {

    # Constants like 'pi' in Trig.pm are common
    my @q = qw(pi pi2 pi4 pip2 pip4);
    @is_known_constant{@q} = (1) x scalar(@q);

    # parenless calls of 'ok' are common
    @q = qw( ok );
    @is_known_function{@q} = (1) x scalar(@q);
} ## end BEGIN

sub guess_if_pattern_or_division {

    # this routine is called when we have encountered a / following an
    # unknown bareword, and we must decide if it starts a pattern or is a
    # division
    # input parameters:
    #   $i - token index of the / starting possible pattern
    # output parameters:
    #   $is_pattern = 0 if probably division,  =1 if probably a pattern
    #   msg = a warning or diagnostic message
    # USES GLOBAL VARIABLES: $last_nonblank_token
    my ( $self, $i, $rtokens, $rtoken_type, $rtoken_map, $max_token_index ) =
      @_;
    my $is_pattern = 0;
    my $msg        = "guessing that / after $last_nonblank_token starts a ";
    my $ibeg       = $i;

    if ( $i >= $max_token_index ) {
        $msg .= "division (no end to pattern found on the line)\n";
    }
    else {
        my $divide_possible =
          $self->is_possible_numerator( $i, $rtokens, $max_token_index );

        if ( $divide_possible < 0 ) {
            $msg        = "pattern (division not possible here)\n";
            $is_pattern = 1;
            return ( $is_pattern, $msg );
        }

        $i = $ibeg + 1;
        my $next_token = $rtokens->[$i];    # first token after slash

        # One of the things we can look at is the spacing around the slash.
        # There # are four possible spacings around the first slash:
        #
        #     return pi/two;#/;     -/-
        #     return pi/ two;#/;    -/+
        #     return pi / two;#/;   +/+
        #     return pi /two;#/;    +/-   <-- possible pattern
        #
        # Spacing rule: a space before the slash but not after the slash
        # usually indicates a pattern.  We can use this to break ties.
        # Note: perl seems to take a newline as a space in this rule (c243)
        my $space_before          = $i < 2 || $rtokens->[ $i - 2 ] =~ m/^\s/;
        my $space_after           = $next_token                    =~ m/^\s/;
        my $is_pattern_by_spacing = $space_before && !$space_after;

        # look for a possible ending / on this line..
        my $in_quote        = 1;
        my $quote_depth     = 0;
        my $quote_character = EMPTY_STRING;
        my $quote_pos       = 0;
        my $quoted_string;
        (

            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string
          )
          = $self->follow_quoted_string(

            $ibeg,
            $in_quote,
            $rtokens,
            $rtoken_type,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $max_token_index
          );

        if ($in_quote) {

            # we didn't find an ending / on this line, so we bias towards
            # division
            if ( $divide_possible >= 0 ) {
                $is_pattern = 0;
                $msg .= "division (no ending / on this line)\n";
            }
            else {

                # assuming a multi-line pattern ... this is risky, but division
                # does not seem possible.  If this fails, it would either be due
                # to a syntax error in the code, or the division_expected logic
                # needs to be fixed.
                $msg        = "multi-line pattern (division not possible)\n";
                $is_pattern = 1;
            }
        }

        # we found an ending /, so we bias slightly towards a pattern
        else {

            my $pattern_expected =
              $self->pattern_expected( $i, $rtokens, $max_token_index );

            if ( $pattern_expected >= 0 ) {

                # pattern looks possible...
                if ( $divide_possible >= 0 ) {

                    # Both pattern and divide can work here...

                    # Increase weight of divide if a pure number follows
                    $divide_possible += $next_token =~ /^\d+$/;

                    # Check for known constants in the numerator, like 'pi'
                    if ( $is_known_constant{$last_nonblank_token} ) {
                        $msg .=
"division (pattern works too but saw known constant '$last_nonblank_token')\n";
                        $is_pattern = 0;
                    }

                    # A very common bare word in pattern expressions is 'ok'
                    elsif ( $is_known_function{$last_nonblank_token} ) {
                        $msg .=
"pattern (division works too but saw '$last_nonblank_token')\n";
                        $is_pattern = 1;
                    }

                    # If one rule is more definite, use it
                    elsif ( $divide_possible > $pattern_expected ) {
                        $msg .=
                          "division (more likely based on following tokens)\n";
                        $is_pattern = 0;
                    }

                    # otherwise, use the spacing rule
                    elsif ($is_pattern_by_spacing) {
                        $msg .=
"pattern (guess on spacing, but division possible too)\n";
                        $is_pattern = 1;
                    }
                    else {
                        $msg .=
"division (guess on spacing, but pattern is possible too)\n";
                        $is_pattern = 0;
                    }
                }

                # divide_possible < 0 means divide can not work here
                else {
                    $is_pattern = 1;
                    $msg .= "pattern (division not possible)\n";
                }
            }

            # pattern does not look possible...
            else {

                if ( $divide_possible >= 0 ) {
                    $is_pattern = 0;
                    $msg .= "division (pattern not possible)\n";
                }

                # Neither pattern nor divide look possible...go by spacing
                else {
                    if ($is_pattern_by_spacing) {
                        $msg .= "pattern (guess on spacing)\n";
                        $is_pattern = 1;
                    }
                    else {
                        $msg .= "division (guess on spacing)\n";
                        $is_pattern = 0;
                    }
                }
            }
        }
    }
    return ( $is_pattern, $msg );
} ## end sub guess_if_pattern_or_division

# try to resolve here-doc vs. shift by looking ahead for
# non-code or the end token (currently only looks for end token)
# returns 1 if it is probably a here doc, 0 if not
sub guess_if_here_doc {

    my ( $self, $next_token ) = @_;

    # This is how many lines we will search for a target as part of the
    # guessing strategy.  It is a constant because there is probably
    # little reason to change it.
    # USES GLOBAL VARIABLES: $current_package $ris_constant,
    my $HERE_DOC_WINDOW = 40;

    my $here_doc_expected = 0;
    my $line;
    my $k   = 0;
    my $msg = "checking <<";

    while ( $line = $self->peek_ahead( $k++ ) ) {
        chomp $line;

        if ( $line =~ /^$next_token$/ ) {
            $msg .= " -- found target $next_token ahead $k lines\n";
            $here_doc_expected = 1;    # got it
            last;
        }
        last if ( $k >= $HERE_DOC_WINDOW );
    }

    if ( !$here_doc_expected ) {

        if ( !defined($line) ) {
            $here_doc_expected = -1;    # hit eof without seeing target
            $msg .= " -- must be shift; target $next_token not in file\n";

        }
        else {                          # still unsure..taking a wild guess

            if ( !$ris_constant->{$current_package}{$next_token} ) {
                $here_doc_expected = 1;
                $msg .=
                  " -- guessing it's a here-doc ($next_token not a constant)\n";
            }
            else {
                $msg .=
                  " -- guessing it's a shift ($next_token is a constant)\n";
            }
        }
    }
    $self->write_logfile_entry($msg);
    return $here_doc_expected;
} ## end sub guess_if_here_doc

#######################################################################
# Tokenizer Routines for scanning identifiers and related items
#######################################################################

sub scan_bare_identifier_do {

    # this routine is called to scan a token starting with an alphanumeric
    # variable or package separator, :: or '.
    # USES GLOBAL VARIABLES: $current_package, $last_nonblank_token,
    # $last_nonblank_type, $rparen_type, $paren_depth

    my (

        $self,

        $input_line,
        $i,
        $tok,
        $type,
        $prototype,
        $rtoken_map,
        $max_token_index

    ) = @_;

    my $i_begin = $i;
    my $package = undef;

    my $i_beg = $i;

    # we have to back up one pretoken at a :: since each : is one pretoken
    if ( $tok eq '::' ) { $i_beg-- }
    if ( $tok eq '->' ) { $i_beg-- }
    my $pos_beg = $rtoken_map->[$i_beg];
    pos($input_line) = $pos_beg;

    #  Examples:
    #   A::B::C
    #   A::
    #   ::A
    #   A'B
    if (
        $input_line =~ m{
         \G\s*                # start at pos
         ( (?:\w*(?:'|::)) )* # $1 = maybe package name like A:: A::B:: or A'
         (?:(?:->)?           #      maybe followed by '->'
         (\w+))?              # $2 = maybe followed by sub name
        }gcx
      )
    {
        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok = substr( $input_line, $pos_beg, $numc );

        # type 'w' includes anything without leading type info
        # ($,%,@,*) including something like abc::def::ghi
        $type = 'w';

        my $sub_name = EMPTY_STRING;
        if ( defined($2) ) { $sub_name = $2; }
        if ( defined($1) ) {
            $package = $1;

            # patch: don't allow isolated package name which just ends
            # in the old style package separator (single quote).  Example:
            #   use CGI':all';
            if ( !($sub_name) && substr( $package, -1, 1 ) eq '\'' ) {
                $pos--;
            }

            $package =~ s/\'/::/g;
            if ( $package =~ /^\:/ ) { $package = 'main' . $package }
            $package =~ s/::$//;
        }
        else {
            $package = $current_package;

            # patched for c043, part 1: keyword does not follow '->'
            if ( $is_keyword{$tok} && $last_nonblank_type ne '->' ) {
                $type = 'k';
            }
        }

        # if it is a bareword..  patched for c043, part 2: not following '->'
        if ( $type eq 'w' && $last_nonblank_type ne '->' ) {

            # check for v-string with leading 'v' type character
            # (This seems to have precedence over filehandle, type 'Y')
            if ( substr( $tok, 0, 1 ) eq 'v' && $tok =~ /^v\d[_\d]*$/ ) {

                # we only have the first part - something like 'v101' -
                # look for more
                if ( $input_line =~ m/\G(\.\d[_\d]*)+/gc ) {
                    $pos  = pos($input_line);
                    $numc = $pos - $pos_beg;
                    $tok  = substr( $input_line, $pos_beg, $numc );
                }
                $type = 'v';
                $self->report_v_string($tok);
            }

            elsif ( $ris_constant->{$package}{$sub_name} ) {
                $type = 'C';
            }

            # bareword after sort has implied empty prototype; for example:
            # @sorted = sort numerically ( 53, 29, 11, 32, 7 );
            # This has priority over whatever the user has specified.
            elsif ($last_nonblank_token eq 'sort'
                && $last_nonblank_type eq 'k' )
            {
                $type = 'Z';
            }

            # Note: strangely, perl does not seem to really let you create
            # functions which act like eval and do, in the sense that eval
            # and do may have operators following the final }, but any operators
            # that you create with prototype (&) apparently do not allow
            # trailing operators, only terms.  This seems strange.
            # If this ever changes, here is the update
            # to make perltidy behave accordingly:

            # elsif ( $ris_block_function->{$package}{$tok} ) {
            #    $tok='eval'; # patch to do braces like eval  - doesn't work
            #    $type = 'k';
            #}
            # TODO: This could become a separate type to allow for different
            # future behavior:
            elsif ( $ris_block_function->{$package}{$sub_name} ) {
                $type = 'G';
            }
            elsif ( $ris_block_list_function->{$package}{$sub_name} ) {
                $type = 'G';
            }
            elsif ( $ris_user_function->{$package}{$sub_name} ) {
                $type      = 'U';
                $prototype = $ruser_function_prototype->{$package}{$sub_name};
            }

            # check for indirect object
            elsif (

                # added 2001-03-27: must not be followed immediately by '('
                # see fhandle.t
                ( $input_line !~ m/\G\(/gc )

                # and
                && (

                    # preceded by keyword like 'print', 'printf' and friends
                    $is_indirect_object_taker{$last_nonblank_token}

                    # or preceded by something like 'print(' or 'printf('
                    || (
                        ( $last_nonblank_token eq '(' )
                        && $is_indirect_object_taker{
                            $rparen_type->[$paren_depth]
                        }

                    )
                )
              )
            {

                # may not be indirect object unless followed by a space;
                # updated 2021-01-16 to consider newline to be a space.
                # updated for case b990 to look for either ';' or space
                if ( pos($input_line) == length($input_line)
                    || $input_line =~ m/\G[;\s]/gc )
                {
                    $type = 'Y';

                    # Abandon Hope ...
                    # Perl's indirect object notation is a very bad
                    # thing and can cause subtle bugs, especially for
                    # beginning programmers.  And I haven't even been
                    # able to figure out a sane warning scheme which
                    # doesn't get in the way of good scripts.

                    # Complain if a filehandle has any lower case
                    # letters.  This is suggested good practice.
                    # Use 'sub_name' because something like
                    # main::MYHANDLE is ok for filehandle
                    if ( $sub_name =~ /[a-z]/ ) {

                        # could be bug caused by older perltidy if
                        # followed by '('
                        if ( $input_line =~ m/\G\s*\(/gc ) {
                            $self->complain(
"Caution: unknown word '$tok' in indirect object slot\n"
                            );
                        }
                    }
                }

                # bareword not followed by a space -- may not be filehandle
                # (may be function call defined in a 'use' statement)
                else {
                    $type = 'Z';
                }
            }

            # none of the above special types
            else {
            }
        }

        # Now we must convert back from character position
        # to pre_token index.
        # I don't think an error flag can occur here ..but who knows
        my $error;
        ( $i, $error ) =
          inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
        if ($error) {
            $self->warning(
                "scan_bare_identifier: Possibly invalid tokenization\n");
        }
    }

    # no match but line not blank - could be syntax error
    # perl will take '::' alone without complaint
    else {
        $type = 'w';

        # change this warning to log message if it becomes annoying
        $self->warning("didn't find identifier after leading ::\n");
    }
    return ( $i, $tok, $type, $prototype );
} ## end sub scan_bare_identifier_do

sub scan_id_do {

    # This is the new scanner and will eventually replace scan_identifier.
    # Only type 'sub' and 'package' are implemented.
    # Token types $ * % @ & -> are not yet implemented.
    #
    # Scan identifier following a type token.
    # The type of call depends on $id_scan_state: $id_scan_state = ''
    # for starting call, in which case $tok must be the token defining
    # the type.
    #
    # If the type token is the last nonblank token on the line, a value
    # of $id_scan_state = $tok is returned, indicating that further
    # calls must be made to get the identifier.  If the type token is
    # not the last nonblank token on the line, the identifier is
    # scanned and handled and a value of '' is returned.

    my (

        $self,

        $input_line,
        $i,
        $tok,
        $rtokens,
        $rtoken_map,
        $id_scan_state,
        $max_token_index

    ) = @_;

    use constant DEBUG_NSCAN => 0;
    my $type = EMPTY_STRING;
    my $i_beg;

    #print "NSCAN:entering i=$i, tok=$tok, type=$type, state=$id_scan_state\n";
    #my ($a,$b,$c) = caller;
    #print "NSCAN: scan_id called with tok=$tok $a $b $c\n";

    # on re-entry, start scanning at first token on the line
    if ($id_scan_state) {
        $i_beg = $i;
        $type  = EMPTY_STRING;
    }

    # on initial entry, start scanning just after type token
    else {
        $i_beg         = $i + 1;
        $id_scan_state = $tok;
        $type          = 't';
    }

    # find $i_beg = index of next nonblank token,
    # and handle empty lines
    my $blank_line          = 0;
    my $next_nonblank_token = $rtokens->[$i_beg];
    if ( $i_beg > $max_token_index ) {
        $blank_line = 1;
    }
    else {

        # only a '#' immediately after a '$' is not a comment
        if ( $next_nonblank_token eq '#' ) {
            if ( $tok ne '$' ) {
                $blank_line = 1;
            }
        }

        if ( $next_nonblank_token =~ /^\s/ ) {
            ( $next_nonblank_token, $i_beg ) =
              find_next_nonblank_token_on_this_line( $i_beg, $rtokens,
                $max_token_index );
            if ( $next_nonblank_token =~ /(^#|^\s*$)/ ) {
                $blank_line = 1;
            }
        }
    }

    # handle non-blank line; identifier, if any, must follow
    if ( !$blank_line ) {

        if ( $is_sub{$id_scan_state} ) {
            ( $i, $tok, $type, $id_scan_state ) = $self->do_scan_sub(
                {
                    input_line      => $input_line,
                    i               => $i,
                    i_beg           => $i_beg,
                    tok             => $tok,
                    type            => $type,
                    rtokens         => $rtokens,
                    rtoken_map      => $rtoken_map,
                    id_scan_state   => $id_scan_state,
                    max_token_index => $max_token_index,
                }
            );
        }

        elsif ( $is_package{$id_scan_state} ) {
            ( $i, $tok, $type ) = $self->do_scan_package(
                {
                    input_line      => $input_line,
                    i               => $i,
                    i_beg           => $i_beg,
                    tok             => $tok,
                    type            => $type,
                    rtokens         => $rtokens,
                    rtoken_map      => $rtoken_map,
                    max_token_index => $max_token_index,
                }
            );
            $id_scan_state = EMPTY_STRING;
        }

        else {
            $self->warning("invalid token in scan_id: $tok\n");
            $id_scan_state = EMPTY_STRING;
        }
    }

    if ( $id_scan_state && ( !defined($type) || !$type ) ) {

        # shouldn't happen:
        if (DEVEL_MODE) {
            $self->Fault(<<EOM);
Program bug in scan_id: undefined type but scan_state=$id_scan_state
EOM
        }
        $self->warning(
"Possible program bug in sub scan_id: undefined type but scan_state=$id_scan_state\n"
        );
        $self->report_definite_bug();
    }

    DEBUG_NSCAN && do {
        print {*STDOUT}
          "NSCAN: returns i=$i, tok=$tok, type=$type, state=$id_scan_state\n";
    };
    return ( $i, $tok, $type, $id_scan_state );
} ## end sub scan_id_do

sub check_prototype {
    my ( $proto, $package, $subname ) = @_;
    return if ( !defined($package) );
    return if ( !defined($subname) );
    if ( defined($proto) ) {
        $proto =~ s/^\s*\(\s*//;
        $proto =~ s/\s*\)$//;
        if ($proto) {
            $ris_user_function->{$package}{$subname}        = 1;
            $ruser_function_prototype->{$package}{$subname} = "($proto)";

            # prototypes containing '&' must be treated specially..
            if ( $proto =~ /\&/ ) {

                # right curly braces of prototypes ending in
                # '&' may be followed by an operator
                if ( $proto =~ /\&$/ ) {
                    $ris_block_function->{$package}{$subname} = 1;
                }

                # right curly braces of prototypes NOT ending in
                # '&' may NOT be followed by an operator
                else {
                    $ris_block_list_function->{$package}{$subname} = 1;
                }
            }
        }
        else {
            $ris_constant->{$package}{$subname} = 1;
        }
    }
    else {
        $ris_user_function->{$package}{$subname} = 1;
    }
    return;
} ## end sub check_prototype

sub do_scan_package {

    # do_scan_package parses a package name
    # it is called with $i_beg equal to the index of the first nonblank
    # token following a 'package' token.
    # USES GLOBAL VARIABLES: $current_package,

    # package NAMESPACE
    # package NAMESPACE VERSION
    # package NAMESPACE BLOCK
    # package NAMESPACE VERSION BLOCK
    #
    # If VERSION is provided, package sets the $VERSION variable in the given
    # namespace to a version object with the VERSION provided. VERSION must be
    # a "strict" style version number as defined by the version module: a
    # positive decimal number (integer or decimal-fraction) without
    # exponentiation or else a dotted-decimal v-string with a leading 'v'
    # character and at least three components.
    # reference http://perldoc.perl.org/functions/package.html

    my ( $self, $rcall_hash ) = @_;

    my $input_line      = $rcall_hash->{input_line};
    my $i               = $rcall_hash->{i};
    my $i_beg           = $rcall_hash->{i_beg};
    my $tok             = $rcall_hash->{tok};
    my $type            = $rcall_hash->{type};
    my $rtokens         = $rcall_hash->{rtokens};
    my $rtoken_map      = $rcall_hash->{rtoken_map};
    my $max_token_index = $rcall_hash->{max_token_index};

    my $package = undef;
    my $pos_beg = $rtoken_map->[$i_beg];
    pos($input_line) = $pos_beg;

    # handle non-blank line; package name, if any, must follow
    if ( $input_line =~ m/\G\s*((?:\w*(?:'|::))*\w*)/gc ) {
        $package = $1;
        $package = ( defined($1) && $1 ) ? $1 : 'main';
        $package =~ s/\'/::/g;
        if ( $package =~ /^\:/ ) { $package = 'main' . $package }
        $package =~ s/::$//;
        my $pos  = pos($input_line);
        my $numc = $pos - $pos_beg;
        $tok  = 'package ' . substr( $input_line, $pos_beg, $numc );
        $type = 'P';    # Fix for c250, previously 'i'

        # Now we must convert back from character position
        # to pre_token index.
        # I don't think an error flag can occur here ..but ?
        my $error;
        ( $i, $error ) =
          inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
        if ($error) { $self->warning("Possibly invalid package\n") }
        $current_package = $package;

        # we should now have package NAMESPACE
        # now expecting VERSION, BLOCK, or ; to follow ...
        # package NAMESPACE VERSION
        # package NAMESPACE BLOCK
        # package NAMESPACE VERSION BLOCK
        my ( $next_nonblank_token, $i_next ) =
          $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

        # check that something recognizable follows, but do not parse.
        # A VERSION number will be parsed later as a number or v-string in the
        # normal way.  What is important is to set the statement type if
        # everything looks okay so that the operator_expected() routine
        # knows that the number is in a package statement.
        # Examples of valid primitive tokens that might follow are:
        #  1235  . ; { } v3  v
        # FIX: added a '#' since a side comment may also follow
        # Added ':' for class attributes (for --use-feature=class, rt145706)
        if ( $next_nonblank_token =~ /^([v\.\d;\{\}\#\:])|v\d|\d+$/ ) {
            $statement_type = $tok;
        }
        else {
            $self->warning(
                "Unexpected '$next_nonblank_token' after package name '$tok'\n"
            );
        }
    }

    # no match but line not blank --
    # could be a label with name package, like package:  , for example.
    else {
        $type = 'k';
    }

    return ( $i, $tok, $type );
} ## end sub do_scan_package

{    ## begin closure for sub scan_complex_identifier

    use constant DEBUG_SCAN_ID => 0;

    # Constant hash:
    my %is_special_variable_char;

    BEGIN {

        # These are the only characters which can (currently) form special
        # variables, like $^W: (issue c066).
        my @q =
          qw{ ? A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ };
        @is_special_variable_char{@q} = (1) x scalar(@q);
    } ## end BEGIN

    # These are the possible states for this scanner:
    my $scan_state_SIGIL     = '$';
    my $scan_state_ALPHA     = 'A';
    my $scan_state_COLON     = ':';
    my $scan_state_LPAREN    = '(';
    my $scan_state_RPAREN    = ')';
    my $scan_state_AMPERSAND = '&';
    my $scan_state_SPLIT     = '^';

    # Only these non-blank states may be returned to caller:
    my %is_returnable_scan_state = (
        $scan_state_SIGIL     => 1,
        $scan_state_AMPERSAND => 1,
    );

    # USES GLOBAL VARIABLES:
    #    $context, $last_nonblank_token, $last_nonblank_type

    #-----------
    # call args:
    #-----------
    my ( $i, $id_scan_state, $identifier, $rtokens, $max_token_index,
        $expecting, $container_type );

    #-------------------------------------------
    # my variables, re-initialized on each call:
    #-------------------------------------------
    my $i_begin;                # starting index $i
    my $type;                   # returned identifier type
    my $tok_begin;              # starting token
    my $tok;                    # returned token
    my $id_scan_state_begin;    # starting scan state
    my $identifier_begin;       # starting identifier
    my $i_save;                 # a last good index, in case of error
    my $message;                # hold error message for log file
    my $tok_is_blank;
    my $last_tok_is_blank;
    my $in_prototype_or_signature;
    my $saw_alpha;
    my $saw_type;
    my $allow_tick;

    sub initialize_my_scan_id_vars {

        # Initialize all 'my' vars on entry
        $i_begin   = $i;
        $type      = EMPTY_STRING;
        $tok_begin = $rtokens->[$i_begin];
        $tok       = $tok_begin;
        if ( $tok_begin eq ':' ) { $tok_begin = '::' }
        $id_scan_state_begin = $id_scan_state;
        $identifier_begin    = $identifier;
        $i_save              = undef;

        $message           = EMPTY_STRING;
        $tok_is_blank      = undef;          # a flag to speed things up
        $last_tok_is_blank = undef;

        $in_prototype_or_signature =
          $container_type && $container_type =~ /^sub\b/;

        # these flags will be used to help figure out the type:
        $saw_alpha = undef;
        $saw_type  = undef;

        # allow old package separator (') except in 'use' statement
        $allow_tick = ( $last_nonblank_token ne 'use' );
        return;
    } ## end sub initialize_my_scan_id_vars

    #----------------------------------
    # Routines for handling scan states
    #----------------------------------
    sub do_id_scan_state_dollar {

        my $self = shift;

        # We saw a sigil, now looking to start a variable name
        if ( $tok eq '$' ) {

            $identifier .= $tok;

            # we've got a punctuation variable if end of line (punct.t)
            if ( $i == $max_token_index ) {
                $type          = 'i';
                $id_scan_state = EMPTY_STRING;
            }
        }
        elsif ( $tok =~ /^\w/ ) {    # alphanumeric ..
            $saw_alpha = 1;
            $identifier .= $tok;

            # now need :: except for special digit vars like '$1' (c208)
            $id_scan_state = $tok =~ /^\d/ ? EMPTY_STRING : $scan_state_COLON;
        }
        elsif ( $tok eq '::' ) {
            $id_scan_state = $scan_state_ALPHA;
            $identifier .= $tok;
        }

        # POSTDEFREF ->@ ->% ->& ->*
        elsif ( ( $tok =~ /^[\@\%\&\*]$/ ) && $identifier =~ /\-\>$/ ) {
            $identifier .= $tok;
        }
        elsif ( $tok eq "'" && $allow_tick ) {    # alphanumeric ..
            $saw_alpha     = 1;
            $id_scan_state = $scan_state_COLON;    # now need ::
            $identifier .= $tok;

            # Perl will accept leading digits in identifiers,
            # although they may not always produce useful results.
            # Something like $main::0 is ok.  But this also works:
            #
            #  sub howdy::123::bubba{ print "bubba $54321!\n" }
            #  howdy::123::bubba();
            #
        }
        elsif ( $tok eq '#' ) {

            my $is_punct_var = $identifier eq '$$';

            # side comment or identifier?
            if (

                # A '#' starts a comment if it follows a space. For example,
                # the following is equivalent to $ans=40.
                #   my $ #
                #     ans = 40;
                !$last_tok_is_blank

                # a # inside a prototype or signature can only start a
                # comment
                && !$in_prototype_or_signature

                # these are valid punctuation vars: *# %# @# $#
                # May also be '$#array' or POSTDEFREF ->$#
                && (   $identifier =~ /^[\%\@\$\*]$/
                    || $identifier =~ /\$$/ )

                # but a '#' after '$$' is a side comment; see c147
                && !$is_punct_var

              )
            {
                $identifier .= $tok;    # keep same state, a $ could follow
            }
            else {

                # otherwise it is a side comment
                if    ( $identifier eq '->' )                 { }
                elsif ($is_punct_var)                         { $type = 'i' }
                elsif ( $id_scan_state eq $scan_state_SIGIL ) { $type = 't' }
                else                                          { $type = 'i' }
                $i             = $i_save;
                $id_scan_state = EMPTY_STRING;
            }
        }

        elsif ( $tok eq '{' ) {

            # check for something like ${#} or ${?}, where ? is a special char
            if (
                (
                       $identifier eq '$'
                    || $identifier eq '@'
                    || $identifier eq '$#'
                )
                && $i + 2 <= $max_token_index
                && $rtokens->[ $i + 2 ] eq '}'
                && $rtokens->[ $i + 1 ] !~ /[\s\w]/
              )
            {
                my $next2 = $rtokens->[ $i + 2 ];
                my $next1 = $rtokens->[ $i + 1 ];
                $identifier .= $tok . $next1 . $next2;
                $i += 2;
                $id_scan_state = EMPTY_STRING;
            }
            else {

                # skip something like ${xxx} or ->{
                $id_scan_state = EMPTY_STRING;

                # if this is the first token of a line, any tokens for this
                # identifier have already been accumulated
                if ( $identifier eq '$' || $i == 0 ) {
                    $identifier = EMPTY_STRING;
                }
                $i = $i_save;
            }
        }

        # space ok after leading $ % * & @
        elsif ( $tok =~ /^\s*$/ ) {

            $tok_is_blank = 1;

            # note: an id with a leading '&' does not actually come this way
            if ( $identifier =~ /^[\$\%\*\&\@]/ ) {

                if ( length($identifier) > 1 ) {
                    $id_scan_state = EMPTY_STRING;
                    $i             = $i_save;
                    $type          = 'i';    # probably punctuation variable
                }
                else {

                    # fix c139: trim line-ending type 't'
                    if ( $i == $max_token_index ) {
                        $i    = $i_save;
                        $type = 't';
                    }

                    # spaces after $'s are common, and space after @
                    # is harmless, so only complain about space
                    # after other type characters. Space after $ and
                    # @ will be removed in formatting.  Report space
                    # after % and * because they might indicate a
                    # parsing error.  In other words '% ' might be a
                    # modulo operator.  Delete this warning if it
                    # gets annoying.
                    elsif ( $identifier !~ /^[\@\$]$/ ) {
                        $message =
                          "Space in identifier, following $identifier\n";
                    }
                    else {
                        # silently accept space after '$' and '@' sigils
                    }
                }
            }

            elsif ( $identifier eq '->' ) {

                # space after '->' is ok except at line end ..
                # so trim line-ending in type '->' (fixes c139)
                if ( $i == $max_token_index ) {
                    $i    = $i_save;
                    $type = '->';
                }
            }

            # stop at space after something other than -> or sigil
            # Example of what can arrive here:
            #   eval { $MyClass->$$ };
            else {
                $id_scan_state = EMPTY_STRING;
                $i             = $i_save;
                $type          = 'i';
            }
        }
        elsif ( $tok eq '^' ) {

            # check for some special variables like $^ $^W
            if ( $identifier =~ /^[\$\*\@\%]$/ ) {
                $identifier .= $tok;
                $type = 'i';

                # There may be one more character, not a space, after the ^
                my $next1 = $rtokens->[ $i + 1 ];
                my $chr   = substr( $next1, 0, 1 );
                if ( $is_special_variable_char{$chr} ) {

                    # It is something like $^W
                    # Test case (c066) : $^Oeq'linux'
                    $i++;
                    $identifier .= $next1;

                    # If pretoken $next1 is more than one character long,
                    # set a flag indicating that it needs to be split.
                    $id_scan_state =
                      ( length($next1) > 1 ) ? $scan_state_SPLIT : EMPTY_STRING;
                }
                else {

                    # it is just $^
                    # Simple test case (c065): '$aa=$^if($bb)';
                    $id_scan_state = EMPTY_STRING;
                }
            }
            else {
                $id_scan_state = EMPTY_STRING;
                $i             = $i_save;
            }
        }
        else {    # something else

            if ( $in_prototype_or_signature && $tok =~ /^[\),=#]/ ) {

                # We might be in an extrusion of
                #     sub foo2 ( $first, $, $third ) {
                # looking at a line starting with a comma, like
                #   $
                #   ,
                # in this case the comma ends the signature variable
                # '$' which will have been previously marked type 't'
                # rather than 'i'.
                if ( $i == $i_begin ) {
                    $identifier = EMPTY_STRING;
                    $type       = EMPTY_STRING;
                }

                # at a # we have to mark as type 't' because more may
                # follow, otherwise, in a signature we can let '$' be an
                # identifier here for better formatting.
                # See 'mangle4.in' for a test case.
                else {
                    $type = 'i';
                    if ( $id_scan_state eq $scan_state_SIGIL && $tok eq '#' ) {
                        $type = 't';
                    }
                    $i = $i_save;
                }
                $id_scan_state = EMPTY_STRING;
            }

            # check for various punctuation variables
            elsif ( $identifier =~ /^[\$\*\@\%]$/ ) {
                $identifier .= $tok;
            }

            # POSTDEFREF: Postfix reference ->$* ->%*  ->@* ->** ->&* ->$#*
            elsif ($tok eq '*'
                && $identifier =~ /\-\>([\@\%\$\*\&]|\$\#)$/ )
            {
                $identifier .= $tok;
            }

            elsif ( $identifier eq '$#' ) {

                if ( $tok eq '{' ) { $type = 'i'; $i = $i_save }

                # perl seems to allow just these: $#: $#- $#+
                elsif ( $tok =~ /^[\:\-\+]$/ ) {
                    $type = 'i';
                    $identifier .= $tok;
                }
                else {
                    $i = $i_save;
                    $self->write_logfile_entry(
                        'Use of $# is deprecated' . "\n" );
                }
            }
            elsif ( $identifier eq '$$' ) {

                # perl does not allow references to punctuation
                # variables without braces.  For example, this
                # won't work:
                #  $:=\4;
                #  $a = $$:;
                # You would have to use
                #  $a = ${$:};

                # '$$' alone is punctuation variable for PID
                $i = $i_save;
                if   ( $tok eq '{' ) { $type = 't' }
                else                 { $type = 'i' }
            }
            elsif ( $identifier eq '->' ) {
                $i = $i_save;
            }
            else {
                $i = $i_save;
                if ( length($identifier) == 1 ) {
                    $identifier = EMPTY_STRING;
                }
            }
            $id_scan_state = EMPTY_STRING;
        }
        return;
    } ## end sub do_id_scan_state_dollar

    sub do_id_scan_state_alpha {

        my $self = shift;

        # looking for alphanumeric after ::
        $tok_is_blank = $tok =~ /^\s*$/;

        if ( $tok =~ /^\w/ ) {    # found it
            $identifier .= $tok;
            $id_scan_state = $scan_state_COLON;    # now need ::
            $saw_alpha     = 1;
        }
        elsif ( $tok eq "'" && $allow_tick ) {
            $identifier .= $tok;
            $id_scan_state = $scan_state_COLON;    # now need ::
            $saw_alpha     = 1;
        }
        elsif ( $tok_is_blank && $identifier =~ /^sub / ) {
            $id_scan_state = $scan_state_LPAREN;
            $identifier .= $tok;
        }
        elsif ( $tok eq '(' && $identifier =~ /^sub / ) {
            $id_scan_state = $scan_state_RPAREN;
            $identifier .= $tok;
        }
        else {
            $id_scan_state = EMPTY_STRING;
            $i             = $i_save;
        }
        return;
    } ## end sub do_id_scan_state_alpha

    sub do_id_scan_state_colon {

        my $self = shift;

        # looking for possible :: after alphanumeric

        $tok_is_blank = $tok =~ /^\s*$/;

        if ( $tok eq '::' ) {    # got it
            $identifier .= $tok;
            $id_scan_state = $scan_state_ALPHA;    # now require alpha
        }
        elsif ( $tok =~ /^\w/ ) {    # more alphanumeric is ok here
            $identifier .= $tok;
            $id_scan_state = $scan_state_COLON;    # now need ::
            $saw_alpha     = 1;
        }
        elsif ( $tok eq "'" && $allow_tick ) {     # tick

            if ( $is_keyword{$identifier} ) {
                $id_scan_state = EMPTY_STRING;     # that's all
                $i             = $i_save;
            }
            else {
                $identifier .= $tok;
            }
        }
        elsif ( $tok_is_blank && $identifier =~ /^sub / ) {
            $id_scan_state = $scan_state_LPAREN;
            $identifier .= $tok;
        }
        elsif ( $tok eq '(' && $identifier =~ /^sub / ) {
            $id_scan_state = $scan_state_RPAREN;
            $identifier .= $tok;
        }
        else {
            $id_scan_state = EMPTY_STRING;    # that's all
            $i             = $i_save;
        }
        return;
    } ## end sub do_id_scan_state_colon

    sub do_id_scan_state_left_paren {

        my $self = shift;

        # looking for possible '(' of a prototype

        if ( $tok eq '(' ) {    # got it
            $identifier .= $tok;
            $id_scan_state = $scan_state_RPAREN;    # now find the end of it
        }
        elsif ( $tok =~ /^\s*$/ ) {                 # blank - keep going
            $identifier .= $tok;
            $tok_is_blank = 1;
        }
        else {
            $id_scan_state = EMPTY_STRING;          # that's all - no prototype
            $i             = $i_save;
        }
        return;
    } ## end sub do_id_scan_state_left_paren

    sub do_id_scan_state_right_paren {

        my $self = shift;

        # looking for a ')' of prototype to close a '('

        $tok_is_blank = $tok =~ /^\s*$/;

        if ( $tok eq ')' ) {    # got it
            $identifier .= $tok;
            $id_scan_state = EMPTY_STRING;    # all done
        }
        elsif ( $tok =~ /^[\s\$\%\\\*\@\&\;]/ ) {
            $identifier .= $tok;
        }
        else {    # probable error in script, but keep going
            $self->warning(
                "Unexpected '$tok' while seeking end of prototype\n");
            $identifier .= $tok;
        }
        return;
    } ## end sub do_id_scan_state_right_paren

    sub do_id_scan_state_ampersand {

        my $self = shift;

        # Starting sub call after seeing an '&'
        if ( $tok =~ /^[\$\w]/ ) {    # alphanumeric ..
            $id_scan_state = $scan_state_COLON;    # now need ::
            $saw_alpha     = 1;
            $identifier .= $tok;
        }
        elsif ( $tok eq "'" && $allow_tick ) {     # alphanumeric ..
            $id_scan_state = $scan_state_COLON;    # now need ::
            $saw_alpha     = 1;
            $identifier .= $tok;
        }
        elsif ( $tok =~ /^\s*$/ ) {                # allow space
            $tok_is_blank = 1;

            # fix c139: trim line-ending type 't'
            if ( length($identifier) == 1 && $i == $max_token_index ) {
                $i    = $i_save;
                $type = 't';
            }
        }
        elsif ( $tok eq '::' ) {                   # leading ::
            $id_scan_state = $scan_state_ALPHA;    # accept alpha next
            $identifier .= $tok;
        }
        elsif ( $tok eq '{' ) {
            if ( $identifier eq '&' || $i == 0 ) {
                $identifier = EMPTY_STRING;
            }
            $i             = $i_save;
            $id_scan_state = EMPTY_STRING;
        }
        elsif ( $tok eq '^' ) {
            if ( $identifier eq '&' ) {

                # Special variable (c066)
                $identifier .= $tok;
                $type = 'i';

                # To be a special $^ variable, there may be one more character,
                # not a space, after the ^
                my $next1 = $rtokens->[ $i + 1 ];
                my $chr   = substr( $next1, 0, 1 );
                if ( $is_special_variable_char{$chr} ) {

                    # It is something like &^O
                    $i++;
                    $identifier .= $next1;

                    # If pretoken $next1 is more than one character long,
                    # set a flag indicating that it needs to be split.
                    $id_scan_state =
                      ( length($next1) > 1 ) ? $scan_state_SPLIT : EMPTY_STRING;
                }
                else {

                    # It is &^.  This is parsed by perl as a call to sub '^',
                    # even though it would be difficult to create a sub '^'.
                    # So we mark it as an identifier (c068).
                    $id_scan_state = EMPTY_STRING;
                }
            }
            else {
                $identifier = EMPTY_STRING;
                $i          = $i_save;
            }
        }
        else {

            # punctuation variable?
            # testfile: cunningham4.pl
            #
            # We have to be careful here.  If we are in an unknown state,
            # we will reject the punctuation variable.  In the following
            # example the '&' is a binary operator but we are in an unknown
            # state because there is no sigil on 'Prima', so we don't
            # know what it is.  But it is a bad guess that
            # '&~' is a function variable.
            # $self->{text}->{colorMap}->[
            #   Prima::PodView::COLOR_CODE_FOREGROUND
            #   & ~tb::COLOR_INDEX ] =
            #   $sec->{ColorCode}

            # Fix for case c033: a '#' here starts a side comment
            if ( $identifier eq '&' && $expecting && $tok ne '#' ) {
                $identifier .= $tok;
            }
            else {
                $identifier = EMPTY_STRING;
                $i          = $i_save;
                $type       = '&';
            }
            $id_scan_state = EMPTY_STRING;
        }
        return;
    } ## end sub do_id_scan_state_ampersand

    #-------------------
    # hash of scanner subs
    #-------------------
    my $scan_identifier_code = {
        $scan_state_SIGIL     => \&do_id_scan_state_dollar,
        $scan_state_ALPHA     => \&do_id_scan_state_alpha,
        $scan_state_COLON     => \&do_id_scan_state_colon,
        $scan_state_LPAREN    => \&do_id_scan_state_left_paren,
        $scan_state_RPAREN    => \&do_id_scan_state_right_paren,
        $scan_state_AMPERSAND => \&do_id_scan_state_ampersand,
    };

    sub scan_complex_identifier {

        # This routine assembles tokens into identifiers.  It maintains a
        # scan state, id_scan_state.  It updates id_scan_state based upon
        # current id_scan_state and token, and returns an updated
        # id_scan_state and the next index after the identifier.

        # This routine now serves a a backup for sub scan_simple_identifier
        # which handles most identifiers.

        # Note that $self must be a 'my' variable and not be a closure
        # variables like the other args. Otherwise it will not get
        # deleted by a DESTROY call at the end of a file. Then an
        # attempt to create multiple tokenizers can occur when multiple
        # files are processed, causing an error.

        (
            my $self,

            $i,
            $id_scan_state,
            $identifier,
            $rtokens,
            $max_token_index,
            $expecting,
            $container_type

        ) = @_;

        # return flag telling caller to split the pretoken
        my $split_pretoken_flag;

        #-------------------
        # Initialize my vars
        #-------------------

        initialize_my_scan_id_vars();

        #--------------------------------------------------------
        # get started by defining a type and a state if necessary
        #--------------------------------------------------------

        if ( !$id_scan_state ) {
            $context = UNKNOWN_CONTEXT;

            # fixup for digraph
            if ( $tok eq '>' ) {
                $tok       = '->';
                $tok_begin = $tok;
            }
            $identifier = $tok;

            if ( $last_nonblank_token eq '->' ) {
                $identifier    = '->' . $identifier;
                $id_scan_state = $scan_state_SIGIL;
            }
            elsif ( $tok eq '$' || $tok eq '*' ) {
                $id_scan_state = $scan_state_SIGIL;
                $context       = SCALAR_CONTEXT;
            }
            elsif ( $tok eq '%' || $tok eq '@' ) {
                $id_scan_state = $scan_state_SIGIL;
                $context       = LIST_CONTEXT;
            }
            elsif ( $tok eq '&' ) {
                $id_scan_state = $scan_state_AMPERSAND;
            }
            elsif ( $tok eq 'sub' or $tok eq 'package' ) {
                $saw_alpha     = 0;    # 'sub' is considered type info here
                $id_scan_state = $scan_state_SIGIL;
                $identifier .=
                  SPACE;    # need a space to separate sub from sub name
            }
            elsif ( $tok eq '::' ) {
                $id_scan_state = $scan_state_ALPHA;
            }
            elsif ( $tok =~ /^\w/ ) {
                $id_scan_state = $scan_state_COLON;
                $saw_alpha     = 1;
            }
            elsif ( $tok eq '->' ) {
                $id_scan_state = $scan_state_SIGIL;
            }
            else {

                # shouldn't happen: bad call parameter
                my $msg =
"Program bug detected: scan_complex_identifier received bad starting token = '$tok'\n";
                if (DEVEL_MODE) { $self->Fault($msg) }
                if ( !$self->[_in_error_] ) {
                    $self->warning($msg);
                    $self->[_in_error_] = 1;
                }
                $id_scan_state = EMPTY_STRING;

                # emergency return
                goto RETURN;
            }
            $saw_type = !$saw_alpha;
        }
        else {
            $i--;
            $saw_alpha = ( $tok =~ /^\w/ );
            $saw_type  = ( $tok =~ /([\$\%\@\*\&])/ );

            # check for a valid starting state
            if ( DEVEL_MODE && !$is_returnable_scan_state{$id_scan_state} ) {
                $self->Fault(<<EOM);
Unexpected starting scan state in sub scan_complex_identifier: '$id_scan_state'
EOM
            }
        }

        #------------------------------
        # loop to gather the identifier
        #------------------------------

        $i_save = $i;

        while ( $i < $max_token_index && $id_scan_state ) {

            # Be sure we have code to handle this state before we proceed
            my $code = $scan_identifier_code->{$id_scan_state};
            if ( !$code ) {

                if ( $id_scan_state eq $scan_state_SPLIT ) {
                    ## OK: this is the signal to exit and split the pretoken
                }

                # unknown state - should not happen
                else {
                    if (DEVEL_MODE) {
                        $self->Fault(<<EOM);
Unknown scan state in sub scan_complex_identifier: '$id_scan_state'
Scan state at sub entry was '$id_scan_state_begin'
EOM
                    }
                    $id_scan_state = EMPTY_STRING;
                    $i             = $i_save;
                }
                last;
            }

            # Remember the starting index for progress check below
            my $i_start_loop = $i;

            $last_tok_is_blank = $tok_is_blank;
            if   ($tok_is_blank) { $tok_is_blank = undef }
            else                 { $i_save       = $i }

            $tok = $rtokens->[ ++$i ];

            # patch to make digraph :: if necessary
            if ( ( $tok eq ':' ) && ( $rtokens->[ $i + 1 ] eq ':' ) ) {
                $tok = '::';
                $i++;
            }

            $code->($self);

            # check for forward progress: a decrease in the index $i
            # implies that scanning has finished
            last if ( $i <= $i_start_loop );

        } ## end of main loop

        #-------------
        # Check result
        #-------------

        # Be sure a valid state is returned
        if ($id_scan_state) {

            if ( !$is_returnable_scan_state{$id_scan_state} ) {

                if ( $id_scan_state eq $scan_state_SPLIT ) {
                    $split_pretoken_flag = 1;
                }

                if ( $id_scan_state eq $scan_state_RPAREN ) {
                    $self->warning(
                        "Hit end of line while seeking ) to end prototype\n");
                }

                $id_scan_state = EMPTY_STRING;
            }

            # Patch: the deprecated variable $# does not combine with anything
            # on the next line.
            if ( $identifier eq '$#' ) { $id_scan_state = EMPTY_STRING }
        }

        # Be sure the token index is valid
        if ( $i < 0 ) { $i = 0 }

        # Be sure a token type is defined
        if ( !$type ) {

            if ($saw_type) {

                if ($saw_alpha) {

                  # The type without the -> should be the same as with the -> so
                  # that if they get separated we get the same bond strengths,
                  # etc.  See b1234
                    if (   $identifier =~ /^->/
                        && $last_nonblank_type eq 'w'
                        && substr( $identifier, 2, 1 ) =~ /^\w/ )
                    {
                        $type = 'w';
                    }
                    else { $type = 'i' }
                }
                elsif ( $identifier eq '->' ) {
                    $type = '->';
                }
                elsif (
                    ( length($identifier) > 1 )

                    # In something like '@$=' we have an identifier '@$'
                    # In something like '$${' we have type '$$' (and only
                    # part of an identifier)
                    && !( $identifier =~ /\$$/ && $tok eq '{' )
                    && $identifier ne 'sub '
                    && $identifier ne 'package '
                  )
                {
                    $type = 'i';
                }
                else { $type = 't' }
            }
            elsif ($saw_alpha) {

                # type 'w' includes anything without leading type info
                # ($,%,@,*) including something like abc::def::ghi
                $type = 'w';

                # Fix for b1337, if restarting scan after line break between
                # '->' or sigil and identifier name, use type 'i'
                if (   $id_scan_state_begin
                    && $identifier =~ /^([\$\%\@\*\&]|->)/ )
                {
                    $type = 'i';
                }
            }
            else {
                $type = EMPTY_STRING;
            }    # this can happen on a restart
        }

        # See if we formed an identifier...
        if ($identifier) {
            $tok = $identifier;
            if ($message) { $self->write_logfile_entry($message) }
        }

        # did not find an identifier, back  up
        else {
            $tok = $tok_begin;
            $i   = $i_begin;
        }

      RETURN:

        DEBUG_SCAN_ID && do {
            my ( $a, $b, $c ) = caller();
            print {*STDOUT}
"SCANID: called from $a $b $c with tok, i, state, identifier =$tok_begin, $i_begin, $id_scan_state_begin, $identifier_begin\n";
            print {*STDOUT}
"SCANID: returned with tok, i, state, identifier =$tok, $i, $id_scan_state, $identifier\n";
        };

        return (

            $i,
            $tok,
            $type,
            $id_scan_state,
            $identifier,
            $split_pretoken_flag
        );
    } ## end sub scan_complex_identifier
} ## end closure for sub scan_complex_identifier

{    ## closure for sub do_scan_sub

    my %warn_if_lexical;

    BEGIN {

        # lexical subs with these names can cause parsing errors in this version
        my @q = qw( m q qq qr qw qx s tr y );
        @warn_if_lexical{@q} = (1) x scalar(@q);
    } ## end BEGIN

    # saved package and subnames in case prototype is on separate line
    my ( $package_saved, $subname_saved );

    # initialize subname each time a new 'sub' keyword is encountered
    sub initialize_subname {
        $package_saved = EMPTY_STRING;
        $subname_saved = EMPTY_STRING;
        return;
    }

    use constant {
        SUB_CALL       => 1,
        PAREN_CALL     => 2,
        PROTOTYPE_CALL => 3,
    };

    sub do_scan_sub {

        # do_scan_sub parses a sub name and prototype.

        # At present there are three basic CALL TYPES which are
        # distinguished by the starting value of '$tok':
        # 1. $tok='sub', id_scan_state='sub'
        #    it is called with $i_beg equal to the index of the first nonblank
        #    token following a 'sub' token.
        # 2. $tok='(', id_scan_state='sub',
        #    it is called with $i_beg equal to the index of a '(' which may
        #    start a prototype.
        # 3. $tok='prototype', id_scan_state='prototype'
        #    it is called with $i_beg equal to the index of a '(' which is
        #    preceded by ': prototype' and has $id_scan_state eq 'prototype'

        # Examples:

        # A single type 1 call will get both the sub and prototype
        #   sub foo1 ( $$ ) { }
        #   ^

        # The subname will be obtained with a 'sub' call
        # The prototype on line 2 will be obtained with a '(' call
        #   sub foo1
        #   ^                    <---call type 1
        #     ( $$ ) { }
        #     ^                  <---call type 2

        # The subname will be obtained with a 'sub' call
        # The prototype will be obtained with a 'prototype' call
        #   sub foo1 ( $x, $y ) : prototype ( $$ ) { }
        #   ^ <---type 1                    ^ <---type 3

        # TODO: add future error checks to be sure we have a valid
        # sub name.  For example, 'sub &doit' is wrong.  Also, be sure
        # a name is given if and only if a non-anonymous sub is
        # appropriate.
        # USES GLOBAL VARS: $current_package, $last_nonblank_token,
        # $rsaw_function_definition,
        # $statement_type

        my ( $self, $rcall_hash ) = @_;

        my $input_line      = $rcall_hash->{input_line};
        my $i               = $rcall_hash->{i};
        my $i_beg           = $rcall_hash->{i_beg};
        my $tok             = $rcall_hash->{tok};
        my $type            = $rcall_hash->{type};
        my $rtokens         = $rcall_hash->{rtokens};
        my $rtoken_map      = $rcall_hash->{rtoken_map};
        my $id_scan_state   = $rcall_hash->{id_scan_state};
        my $max_token_index = $rcall_hash->{max_token_index};

        my $i_entry = $i;

        # Determine the CALL TYPE
        # 1=sub
        # 2=(
        # 3=prototype
        my $call_type =
            $tok eq 'prototype' ? PROTOTYPE_CALL
          : $tok eq '('         ? PAREN_CALL
          :                       SUB_CALL;

        $id_scan_state = EMPTY_STRING;  # normally we get everything in one call
        my $subname = $subname_saved;
        my $package = $package_saved;
        my $proto   = undef;
        my $attrs   = undef;
        my $match;

        my $pos_beg = $rtoken_map->[$i_beg];
        pos($input_line) = $pos_beg;

        # Look for the sub NAME if this is a SUB call
        if (
               $call_type == SUB_CALL
            && $input_line =~ m{\G\s*
        ((?:\w*(?:'|::))*)  # package - something that ends in :: or '
        (\w+)               # NAME    - required
        }gcx
          )
        {
            $match   = 1;
            $subname = $2;

            my $is_lexical_sub =
              $last_nonblank_type eq 'k' && $last_nonblank_token eq 'my';
            if ( $is_lexical_sub && $1 ) {
                $self->warning("'my' sub $subname cannot be in package '$1'\n");
                $is_lexical_sub = 0;
            }

            if ($is_lexical_sub) {

                # lexical subs use the block sequence number as a package name
                my $seqno =
                  $rcurrent_sequence_number->[BRACE]
                  [ $rcurrent_depth->[BRACE] ];
                $seqno   = 1 if ( !defined($seqno) );
                $package = $seqno;
                if ( $warn_if_lexical{$subname} ) {
                    $self->warning(
"'my' sub '$subname' matches a builtin name and may not be handled correctly in this perltidy version.\n"
                    );

                    # This may end badly, it is safest to block formatting
                    # For an example, see perl527/lexsub.t (issue c203)
                    $self->[_in_trouble_] = 1;
                }
            }
            else {
                $package = ( defined($1) && $1 ) ? $1 : $current_package;
                $package =~ s/\'/::/g;
                if ( $package =~ /^\:/ ) { $package = 'main' . $package }
                $package =~ s/::$//;
            }

            my $pos  = pos($input_line);
            my $numc = $pos - $pos_beg;
            $tok  = 'sub ' . substr( $input_line, $pos_beg, $numc );
            $type = 'S';    ## Fix for c250, was 'i';

            # remember the sub name in case another call is needed to
            # get the prototype
            $package_saved = $package;
            $subname_saved = $subname;
        }

        # Now look for PROTO ATTRS for all call types
        # Look for prototype/attributes which are usually on the same
        # line as the sub name but which might be on a separate line.
        # For example, we might have an anonymous sub with attributes,
        # or a prototype on a separate line from its sub name

        # NOTE: We only want to parse PROTOTYPES here. If we see anything that
        # does not look like a prototype, we assume it is a SIGNATURE and we
        # will stop and let the the standard tokenizer handle it.  In
        # particular, we stop if we see any nested parens, braces, or commas.
        # Also note, a valid prototype cannot contain any alphabetic character
        #  -- see https://perldoc.perl.org/perlsub
        # But it appears that an underscore is valid in a prototype, so the
        # regex below uses [A-Za-z] rather than \w
        # This is the old regex which has been replaced:
        # $input_line =~ m/\G(\s*\([^\)\(\}\{\,#]*\))?  # PROTO
        # Added '=' for issue c362
        my $saw_opening_paren = $input_line =~ /\G\s*\(/;
        if (
            $input_line =~ m{\G(\s*\([^\)\(\}\{\,#A-Za-z=]*\))?  # PROTO
            (\s*:)?                              # ATTRS leading ':'
            }gcx
            && ( $1 || $2 )
          )
        {
            $proto = $1;
            $attrs = $2;

            # Append the prototype to the starting token if it is 'sub' or
            # 'prototype'.  This is not necessary but for compatibility with
            # previous versions when the -csc flag is used:
            if ( $proto && ( $match || $call_type == PROTOTYPE_CALL ) ) {
                $tok .= $proto;
            }

            # If we just entered the sub at an opening paren on this call, not
            # a following :prototype, label it with the previous token.  This is
            # necessary to propagate the sub name to its opening block.
            elsif ( $call_type == PAREN_CALL ) {
                $tok = $last_nonblank_token;
            }
            else {
            }

            $match ||= 1;

            # Patch part #1 to fixes cases b994 and b1053:
            # Mark an anonymous sub keyword without prototype as type 'k', i.e.
            #    'sub : lvalue { ...'
            $type = 'S';    ## C250, was 'i';
            if ( $tok eq 'sub' && !$proto ) { $type = 'k' }
        }

        if ($match) {

            # ATTRS: if there are attributes, back up and let the ':' be
            # found later by the scanner.
            my $pos = pos($input_line);
            if ($attrs) {
                $pos -= length($attrs);
            }

            my $next_nonblank_token = $tok;

            # catch case of line with leading ATTR ':' after anonymous sub
            if ( $pos == $pos_beg && $tok eq ':' ) {
                $type = 'A';
                $self->[_in_attribute_list_] = 1;
            }

            # Otherwise, if we found a match we must convert back from
            # string position to the pre_token index for continued parsing.
            else {

                # I don't think an error flag can occur here ..but ?
                my $error;
                ( $i, $error ) = inverse_pretoken_map( $i, $pos, $rtoken_map,
                    $max_token_index );
                if ($error) { $self->warning("Possibly invalid sub\n") }

                # Patch part #2 to fixes cases b994 and b1053:
                # Do not let spaces be part of the token of an anonymous sub
                # keyword which we marked as type 'k' above...i.e. for
                # something like:
                #    'sub : lvalue { ...'
                # Back up and let it be parsed as a blank
                if (   $type eq 'k'
                    && $attrs
                    && $i > $i_entry
                    && substr( $rtokens->[$i], 0, 1 ) =~ m/\s/ )
                {
                    $i--;
                }

                # check for multiple definitions of a sub
                ( $next_nonblank_token, my $i_next ) =
                  find_next_nonblank_token_on_this_line( $i, $rtokens,
                    $max_token_index );
            }

            if ( $next_nonblank_token =~ /^(\s*|#)$/ )
            {    # skip blank or side comment
                my ( $rpre_tokens, $rpre_types ) =
                  $self->peek_ahead_for_n_nonblank_pre_tokens(1);
                if ( defined($rpre_tokens) && @{$rpre_tokens} ) {
                    $next_nonblank_token = $rpre_tokens->[0];
                }
                else {
                    $next_nonblank_token = '}';
                }
            }

            # See what's next...
            if ( $next_nonblank_token eq '{' ) {
                if ($subname) {

                    # Check for multiple definitions of a sub, but
                    # it is ok to have multiple sub BEGIN, etc,
                    # so we do not complain if name is all caps
                    if (   $rsaw_function_definition->{$subname}{$package}
                        && $subname !~ /^[A-Z]+$/ )
                    {
                        my $lno =
                          $rsaw_function_definition->{$subname}{$package};
                        if ( $package =~ /^\d/ ) {
                            $self->warning(
"already saw definition of lexical 'sub $subname' at line $lno\n"
                            );

                        }
                        else {
                            if ( !DEVEL_MODE ) {
                                $self->warning(
"already saw definition of 'sub $subname' in package '$package' at line $lno\n"
                                );
                            }
                        }
                    }
                    $rsaw_function_definition->{$subname}{$package} =
                      $self->[_last_line_number_];
                }
            }
            elsif ( $next_nonblank_token eq ';' ) {
            }
            elsif ( $next_nonblank_token eq '}' ) {
            }

            # ATTRS - if an attribute list follows, remember the name
            # of the sub so the next opening brace can be labeled.
            # Setting 'statement_type' causes any ':'s to introduce
            # attributes.
            elsif ( $next_nonblank_token eq ':' ) {
                if ( $call_type == SUB_CALL ) {
                    $statement_type =
                      substr( $tok, 0, 3 ) eq 'sub' ? $tok : 'sub';
                }
            }

            # if we stopped before an open paren ...
            elsif ( $next_nonblank_token eq '(' ) {

                # If we DID NOT see this paren above then it must be on the
                # next line so we will set a flag to come back here and see if
                # it is a PROTOTYPE

                # Otherwise, we assume it is a SIGNATURE rather than a
                # PROTOTYPE and let the normal tokenizer handle it as a list
                if ( !$saw_opening_paren ) {
                    $id_scan_state = 'sub';    # we must come back to get proto
                }
                if ( $call_type == SUB_CALL ) {
                    $statement_type =
                      substr( $tok, 0, 3 ) eq 'sub' ? $tok : 'sub';
                }
            }

            # something else..
            elsif ($next_nonblank_token) {

                if ( $rcall_hash->{tok} eq 'method' && $call_type == SUB_CALL )
                {
                    # For a method call, silently ignore this error (rt145706)
                    # to avoid needless warnings. Example which can produce it:
                    #     test(method Pack (), "method");

                    # TODO: scan for use feature 'class' and:
                    # - if we saw 'use feature 'class' then issue the warning.
                    # - if we did not see use feature 'class' then issue the
                    #   warning and suggest turning off --use-feature=class
                }
                else {
                    $subname = EMPTY_STRING unless defined($subname);
                    $self->warning(
"expecting ':' or ';' or '{' after definition or declaration of sub '$subname' but saw '$next_nonblank_token'\n"
                    );
                }
            }

            # EOF technically ok
            else {
            }

            check_prototype( $proto, $package, $subname );
        }

        # no match to either sub name or prototype, but line not blank
        else {

        }
        return ( $i, $tok, $type, $id_scan_state );
    } ## end sub do_scan_sub
}

#########################################################################
# Tokenizer utility routines which may use CONSTANTS but no other GLOBALS
#########################################################################

sub find_next_nonblank_token {
    my ( $self, $i, $rtokens, $max_token_index ) = @_;

    # Returns the next nonblank token after the token at index $i
    # To skip past a side comment, and any subsequent block comments
    # and blank lines, call with i=$max_token_index

    # Skip any ending blank (fix c258). It would be cleaner if caller passed
    # $rtoken_map, so we could check for type 'b', and avoid a regex test, but
    # benchmarking shows that this test does not take significant time.  So
    # that would be a nice update but not essential.  Also note that ending
    # blanks will not occur for text previously processed by perltidy.
    if (   $i == $max_token_index - 1
        && $rtokens->[$max_token_index] =~ /^\s+$/ )
    {
        $i++;
    }

    if ( $i >= $max_token_index ) {
        if ( !peeked_ahead() ) {
            peeked_ahead(1);
            $self->peek_ahead_for_nonblank_token( $rtokens, $max_token_index );
        }
    }

    my $next_nonblank_token = $rtokens->[ ++$i ];

    # Any more tokens?
    return ( SPACE, $i )
      if ( !defined($next_nonblank_token) || !length($next_nonblank_token) );

    # Skip over whitespace
    my $ord = ord( substr( $next_nonblank_token, 0, 1 ) );
    if (

        ( $ord <= ORD_PRINTABLE_MIN || $ord >= ORD_PRINTABLE_MAX )

        # Quick test for ascii space or tab
        && (
            ( $ord == ORD_SPACE || $ord == ORD_TAB )

            # Slow test to for something else identified as whitespace
            || $next_nonblank_token =~ /^\s+$/
        )
      )
    {
        $next_nonblank_token = $rtokens->[ ++$i ];
        return ( SPACE, $i ) unless defined($next_nonblank_token);
    }

    # We should be at a nonblank now
    return ( $next_nonblank_token, $i );

} ## end sub find_next_nonblank_token

sub find_next_noncomment_token {
    my ( $self, $i, $rtokens, $max_token_index ) = @_;

    # Given the current character position, look ahead past any comments
    # and blank lines and return the next token, including digraphs and
    # trigraphs.

    my ( $next_nonblank_token, $i_next ) =
      $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

    # skip past any side comment
    if ( $next_nonblank_token eq '#' ) {
        ( $next_nonblank_token, $i_next ) =
          $self->find_next_nonblank_token( $i_next, $rtokens,
            $max_token_index );
    }

    # check for a digraph
    if (   $next_nonblank_token
        && $next_nonblank_token ne SPACE
        && defined( $rtokens->[ $i_next + 1 ] ) )
    {
        my $test2 = $next_nonblank_token . $rtokens->[ $i_next + 1 ];
        if ( $is_digraph{$test2} ) {
            $next_nonblank_token = $test2;
            $i_next              = $i_next + 1;

            # check for a trigraph
            if ( defined( $rtokens->[ $i_next + 1 ] ) ) {
                my $test3 = $next_nonblank_token . $rtokens->[ $i_next + 1 ];
                if ( $is_trigraph{$test3} ) {
                    $next_nonblank_token = $test3;
                    $i_next              = $i_next + 1;
                }
            }
        }
    }

    return ( $next_nonblank_token, $i_next );
} ## end sub find_next_noncomment_token

sub is_possible_numerator {

    # Look at the next non-comment character and decide if it could be a
    # numerator.  Return
    #   1 - yes
    #   0 - can't tell
    #  -1 - no

    my ( $self, $i, $rtokens, $max_token_index ) = @_;
    my $is_possible_numerator = 0;

    my $next_token = $rtokens->[ $i + 1 ];
    if ( $next_token eq '=' ) { $i++; }    # handle /=
    my ( $next_nonblank_token, $i_next ) =
      $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

    if ( $next_nonblank_token eq '#' ) {
        ( $next_nonblank_token, $i_next ) =
          $self->find_next_nonblank_token( $max_token_index, $rtokens,
            $max_token_index );
    }

    if ( $next_nonblank_token =~ / [ \( \$ \w \. \@ ] /x ) {
        $is_possible_numerator = 1;
    }
    elsif ( $next_nonblank_token =~ /^\s*$/ ) {
        $is_possible_numerator = 0;
    }
    else {
        $is_possible_numerator = -1;
    }

    return $is_possible_numerator;
} ## end sub is_possible_numerator

{    ## closure for sub pattern_expected
    my %pattern_test;

    BEGIN {

        # List of tokens which may follow a pattern.  Note that we will not
        # have formed digraphs at this point, so we will see '&' instead of
        # '&&' and '|' instead of '||'

        # /(\)|\}|\;|\&\&|\|\||and|or|while|if|unless)/
        my @q = qw( & && | || ? : + - * and or while if unless);
        push @q, ')', '}', ']', '>', ',', ';';
        @pattern_test{@q} = (1) x scalar(@q);
    } ## end BEGIN

    sub pattern_expected {

        # This a filter for a possible pattern.
        # It looks at the token after a possible pattern and tries to
        # determine if that token could end a pattern.
        # returns -
        #   1 - yes
        #   0 - can't tell
        #  -1 - no
        my ( $self, $i, $rtokens, $max_token_index ) = @_;
        my $is_pattern = 0;

        my $next_token = $rtokens->[ $i + 1 ];

        # skip a possible quote modifier
        my $possible_modifiers = $quote_modifiers{'m'};
        if ( $next_token =~ /^$possible_modifiers/ ) {
            $i++;
        }

        my ( $next_nonblank_token, $i_next ) =
          $self->find_next_nonblank_token( $i, $rtokens, $max_token_index );

        if ( $pattern_test{$next_nonblank_token} ) {
            $is_pattern = 1;
        }
        else {

            # Added '#' to fix issue c044
            if (   $next_nonblank_token =~ /^\s*$/
                || $next_nonblank_token eq '#' )
            {
                $is_pattern = 0;
            }
            else {
                $is_pattern = -1;
            }
        }
        return $is_pattern;
    } ## end sub pattern_expected
}

sub find_next_nonblank_token_on_this_line {
    my ( $i, $rtokens, $max_token_index ) = @_;
    my $next_nonblank_token;

    if ( $i < $max_token_index ) {
        $next_nonblank_token = $rtokens->[ ++$i ];

        if ( $next_nonblank_token =~ /^\s*$/ ) {

            if ( $i < $max_token_index ) {
                $next_nonblank_token = $rtokens->[ ++$i ];
            }
        }
    }
    else {
        $next_nonblank_token = EMPTY_STRING;
    }
    return ( $next_nonblank_token, $i );
} ## end sub find_next_nonblank_token_on_this_line

sub find_angle_operator_termination {

    # We are looking at a '<' and want to know if it is an angle operator.
    # We are to return:
    #   $i = pretoken index of ending '>' if found, current $i otherwise
    #   $type = 'Q' if found, '>' otherwise
    my ( $self, $input_line, $i_beg, $rtoken_map, $expecting, $max_token_index )
      = @_;
    my $i    = $i_beg;
    my $type = '<';
    pos($input_line) = 1 + $rtoken_map->[$i];

    my $filter;

    # we just have to find the next '>' if a term is expected
    if ( $expecting == TERM ) { $filter = '[\>]' }

    # we have to guess if we don't know what is expected
    elsif ( $expecting == UNKNOWN ) { $filter = '[\>\;\=\#\|\<]' }

    # shouldn't happen - we shouldn't be here if operator is expected
    else {
        if (DEVEL_MODE) {
            $self->Fault(<<EOM);
Bad call to find_angle_operator_termination
EOM
        }
        return ( $i, $type );
    }

    # To illustrate what we might be looking at, in case we are
    # guessing, here are some examples of valid angle operators
    # (or file globs):
    #  <tmp_imp/*>
    #  <FH>
    #  <$fh>
    #  <*.c *.h>
    #  <_>
    #  <jskdfjskdfj* op/* jskdjfjkosvk*> ( glob.t)
    #  <${PREFIX}*img*.$IMAGE_TYPE>
    #  <img*.$IMAGE_TYPE>
    #  <Timg*.$IMAGE_TYPE>
    #  <$LATEX2HTMLVERSIONS${dd}html[1-9].[0-9].pl>
    #
    # Here are some examples of lines which do not have angle operators:
    #  return unless $self->[2]++ < $#{$self->[1]};
    #  < 2  || @$t >
    #
    # the following line from dlister.pl caused trouble:
    #  print'~'x79,"\n",$D<1024?"0.$D":$D>>10,"K, $C files\n\n\n";
    #
    # If the '<' starts an angle operator, it must end on this line and
    # it must not have certain characters like ';' and '=' in it.  I use
    # this to limit the testing.  This filter should be improved if
    # possible.

    if ( $input_line =~ /($filter)/g ) {

        if ( $1 eq '>' ) {

            # We MAY have found an angle operator termination if we get
            # here, but we need to do more to be sure we haven't been
            # fooled.
            my $pos = pos($input_line);

            my $pos_beg = $rtoken_map->[$i];
            my $str     = substr( $input_line, $pos_beg, ( $pos - $pos_beg ) );

            # Test for '<' after possible filehandle, issue c103
            # print $fh <>;          # syntax error
            # print $fh <DATA>;      # ok
            # print $fh < DATA>;     # syntax error at '>'
            # print STDERR < DATA>;  # ok, prints word 'DATA'
            # print BLABLA <DATA>;   # ok; does nothing unless BLABLA is defined
            if ( $last_nonblank_type eq 'Z' ) {

                # $str includes brackets; something like '<DATA>'
                if (   substr( $last_nonblank_token, 0, 1 ) !~ /[A-Za-z_]/
                    && substr( $str, 1, 1 ) !~ /[A-Za-z_]/ )
                {
                    return ( $i, $type );
                }
            }

            # Reject if the closing '>' follows a '-' as in:
            # if ( VERSION < 5.009 && $op-> name eq 'assign' ) { }
            if ( $expecting eq UNKNOWN ) {
                my $check = substr( $input_line, $pos - 2, 1 );
                if ( $check eq '-' ) {
                    return ( $i, $type );
                }
            }

            ######################################debug#####
            #$self->write_diagnostics( "ANGLE? :$str\n");
            #print "ANGLE: found $1 at pos=$pos str=$str check=$check\n";
            ######################################debug#####
            $type = 'Q';
            my $error;
            ( $i, $error ) =
              inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );

            # It may be possible that a quote ends midway in a pretoken.
            # If this happens, it may be necessary to split the pretoken.
            if ($error) {
                if (DEVEL_MODE) {
                    $self->Fault(<<EOM);
unexpected error condition returned by inverse_pretoken_map
EOM
                }
                $self->warning(
                    "Possible tokinization error..please check this line\n");
            }

            # Check for accidental formatting of a markup language doc...
            # Formatting will be skipped if we set _html_tag_count_ and
            # also set a warning of any kind.
            my $is_html_tag;
            my $is_first_string =
              $i_beg == 0 && $self->[_last_line_number_] == 1;

            # html comment '<!...' of any type
            if ( $str =~ /^<\s*!/ ) {
                $is_html_tag = 1;
                if ($is_first_string) {
                    $self->warning(
"looks like a markup language, continuing error checks\n"
                    );
                }
            }

            # html end tag, something like </h1>
            elsif ( $str =~ /^<\s*\/\w+\s*>$/ ) {
                $is_html_tag = 1;
            }

            # xml prolog?
            elsif ( $str =~ /^<\?xml\s.*\?>$/i && $is_first_string ) {
                $is_html_tag = 1;
                $self->warning(
                    "looks like a markup language, continuing error checks\n");
            }
            else {
                ## doesn't look like a markup tag
            }

            if ($is_html_tag) {
                $self->[_html_tag_count_]++;
            }

            # count blanks on inside of brackets
            my $blank_count = 0;
            $blank_count++ if ( $str =~ /<\s+/ );
            $blank_count++ if ( $str =~ /\s+>/ );

            # Now let's see where we stand....
            # OK if math op not possible
            if ( $expecting == TERM ) {
            }

            elsif ($is_html_tag) {
            }

            # OK if there are no more than 2 non-blank pre-tokens inside
            # (not possible to write 2 token math between < and >)
            # This catches most common cases
            elsif ( $i <= $i_beg + 3 + $blank_count ) {

                # No longer any need to document this common case
                ## $self->write_diagnostics("ANGLE(1 or 2 tokens): $str\n");
            }

            # OK if there is some kind of identifier inside
            #   print $fh <tvg::INPUT>;
            elsif ( $str =~ /^<\s*\$?(\w|::|\s)+\s*>$/ ) {
                $self->write_diagnostics("ANGLE (contains identifier): $str\n");
            }

            # Not sure..
            else {

                # Let's try a Brace Test: any braces inside must balance
                my $br = 0;
                while ( $str =~ /\{/g ) { $br++ }
                while ( $str =~ /\}/g ) { $br-- }
                my $sb = 0;
                while ( $str =~ /\[/g ) { $sb++ }
                while ( $str =~ /\]/g ) { $sb-- }
                my $pr = 0;
                while ( $str =~ /\(/g ) { $pr++ }
                while ( $str =~ /\)/g ) { $pr-- }

                # if braces do not balance - not angle operator
                if ( $br || $sb || $pr ) {
                    $i    = $i_beg;
                    $type = '<';
                    $self->write_diagnostics(
                        "NOT ANGLE (BRACE={$br ($pr [$sb ):$str\n");
                }

                # we should keep doing more checks here...to be continued
                # Tentatively accepting this as a valid angle operator.
                # There are lots more things that can be checked.
                else {
                    $self->write_diagnostics(
                        "ANGLE-Guessing yes: $str expecting=$expecting\n");
                    $self->write_logfile_entry(
                        "Guessing angle operator here: $str\n");
                }
            }
        }

        # didn't find ending >
        else {
            if ( $expecting == TERM ) {
                $self->warning("No ending > for angle operator\n");
            }
        }
    }
    return ( $i, $type );
} ## end sub find_angle_operator_termination

sub scan_number_do {

    #  scan a number in any of the formats that Perl accepts
    #  Underbars (_) are allowed in decimal numbers.
    #  input parameters -
    #      $input_line  - the string to scan
    #      $i           - pre_token index to start scanning
    #    $rtoken_map    - reference to the pre_token map giving starting
    #                    character position in $input_line of token $i
    #  output parameters -
    #    $i            - last pre_token index of the number just scanned
    #    number        - the number (characters); or undef if not a number

    my ( $self, $input_line, $i, $rtoken_map, $input_type, $max_token_index ) =
      @_;
    my $pos_beg = $rtoken_map->[$i];
    my $pos;
    my $i_begin = $i;
    my $number  = undef;
    my $type    = $input_type;

    my $first_char = substr( $input_line, $pos_beg, 1 );

    # Look for bad starting characters; Shouldn't happen..
    if ( $first_char !~ /[\d\.\+\-Ee]/ ) {
        if (DEVEL_MODE) {
            $self->Fault(<<EOM);
Program bug - scan_number given bad first character = '$first_char'
EOM
        }
        return ( $i, $type, $number );
    }

    # handle v-string without leading 'v' character ('Two Dot' rule)
    # (vstring.t)
    # Here is the format prior to including underscores:
    ## if ( $input_line =~ /\G((\d+)?\.\d+(\.\d+)+)/g ) {
    pos($input_line) = $pos_beg;
    if ( $input_line =~ /\G((\d[_\d]*)?\.[\d_]+(\.[\d_]+)+)/g ) {
        $pos = pos($input_line);
        my $numc = $pos - $pos_beg;
        $number = substr( $input_line, $pos_beg, $numc );
        $type   = 'v';
    }

    # handle octal, hex, binary
    if ( !defined($number) ) {
        pos($input_line) = $pos_beg;

        # Perl 5.22 added floating point literals, like '0x0.b17217f7d1cf78p0'
        # For reference, the format prior to hex floating point is:
        #   /\G[+-]?0(([xX][0-9a-fA-F_]+)|([0-7_]+)|([bB][01_]+))/g )
        #             (hex)               (octal)   (binary)
        if (
            $input_line =~ m{

            \G[+-]?0(                   # leading [signed] 0

           # a hex float, i.e. '0x0.b17217f7d1cf78p0'
           ([xX][0-9a-fA-F_]*            # X and optional leading digits
           (\.([0-9a-fA-F][0-9a-fA-F_]*)?)?   # optional decimal and fraction
           [Pp][+-]?[0-9a-fA-F]          # REQUIRED exponent with digit
           [0-9a-fA-F_]*)                # optional Additional exponent digits

           # or hex integer
           |([xX][0-9a-fA-F_]+)

           # or octal fraction
           |([oO]?[0-7_]+          # string of octal digits
           (\.([0-7][0-7_]*)?)?    # optional decimal and fraction
           [Pp][+-]?[0-7]          # REQUIRED exponent, no underscore
           [0-7_]*)                # Additional exponent digits with underscores

           # or octal integer
           |([oO]?[0-7_]+)         # string of octal digits

           # or a binary float
           |([bB][01_]*            # 'b' with string of binary digits
           (\.([01][01_]*)?)?      # optional decimal and fraction
           [Pp][+-]?[01]           # Required exponent indicator, no underscore
           [01_]*)                 # additional exponent bits

           # or binary integer
           |([bB][01_]+)           # 'b' with string of binary digits

           )}gx
          )
        {
            $pos = pos($input_line);
            my $numc = $pos - $pos_beg;
            $number = substr( $input_line, $pos_beg, $numc );
            $type   = 'n';
        }
    }

    # handle decimal
    if ( !defined($number) ) {
        pos($input_line) = $pos_beg;

        if ( $input_line =~ /\G([+-]?[\d_]*(\.[\d_]*)?([Ee][+-]?(\d+))?)/g ) {
            $pos = pos($input_line);

            # watch out for things like 0..40 which would give 0. by this;
            if (   ( substr( $input_line, $pos - 1, 1 ) eq '.' )
                && ( substr( $input_line, $pos, 1 ) eq '.' ) )
            {
                $pos--;
            }
            my $numc = $pos - $pos_beg;
            $number = substr( $input_line, $pos_beg, $numc );
            $type   = 'n';
        }
    }

    # filter out non-numbers like e + - . e2  .e3 +e6
    # the rule: at least one digit, and any 'e' must be preceded by a digit
    if (
        $number !~ /\d/    # no digits
        || (   $number =~ /^(.*)[eE]/
            && $1 !~ /\d/ )    # or no digits before the 'e'
      )
    {
        $number = undef;
        $type   = $input_type;
        return ( $i, $type, $number );
    }

    # Found a number; now we must convert back from character position
    # to pre_token index. An error here implies user syntax error.
    # An example would be an invalid octal number like '009'.
    my $error;
    ( $i, $error ) =
      inverse_pretoken_map( $i, $pos, $rtoken_map, $max_token_index );
    if ($error) { $self->warning("Possibly invalid number\n") }

    return ( $i, $type, $number );
} ## end sub scan_number_do

sub inverse_pretoken_map {

    # Starting with the current pre_token index $i, scan forward until
    # finding the index of the next pre_token whose position is $pos.
    my ( $i, $pos, $rtoken_map, $max_token_index ) = @_;
    my $error = 0;

    while ( ++$i <= $max_token_index ) {

        if ( $pos <= $rtoken_map->[$i] ) {

            # Let the calling routine handle errors in which we do not
            # land on a pre-token boundary.  It can happen by running
            # perltidy on some non-perl scripts, for example.
            if ( $pos < $rtoken_map->[$i] ) { $error = 1 }
            $i--;
            last;
        }
    }
    return ( $i, $error );
} ## end sub inverse_pretoken_map

sub find_here_doc {

    # find the target of a here document, if any
    # input parameters:
    #   $i - token index of the second < of <<
    #   ($i must be less than the last token index if this is called)
    # output parameters:
    #   $found_target = 0 didn't find target; =1 found target
    #   HERE_TARGET - the target string (may be empty string)
    #   $i - unchanged if not here doc,
    #    or index of the last token of the here target
    #   $saw_error - flag noting unbalanced quote on here target
    my (

        $self,

        $expecting,
        $i,
        $rtokens,
        $rtoken_type,
        $rtoken_map,
        $max_token_index

    ) = @_;

    my $ibeg                 = $i;
    my $found_target         = 0;
    my $here_doc_target      = EMPTY_STRING;
    my $here_quote_character = EMPTY_STRING;
    my $saw_error            = 0;
    my ( $next_nonblank_token, $i_next_nonblank, $next_token );
    $next_token = $rtokens->[ $i + 1 ];

    # perl allows a backslash before the target string (heredoc.t)
    my $backslash = 0;
    if ( $next_token eq '\\' ) {
        $backslash  = 1;
        $next_token = $rtokens->[ $i + 2 ];
    }

    ( $next_nonblank_token, $i_next_nonblank ) =
      find_next_nonblank_token_on_this_line( $i, $rtokens, $max_token_index );

    if ( $next_nonblank_token =~ /[\'\"\`]/ ) {

        my $in_quote    = 1;
        my $quote_depth = 0;
        my $quote_pos   = 0;
        my $quoted_string;

        (

            $i,
            $in_quote,
            $here_quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string
          )
          = $self->follow_quoted_string(

            $i_next_nonblank,
            $in_quote,
            $rtokens,
            $rtoken_type,
            $here_quote_character,
            $quote_pos,
            $quote_depth,
            $max_token_index
          );

        if ($in_quote) {    # didn't find end of quote, so no target found
            $i = $ibeg;
            if ( $expecting == TERM ) {
                $self->warning(
"Did not find here-doc string terminator ($here_quote_character) before end of line \n"
                );
                $saw_error = 1;
            }
        }
        else {              # found ending quote
            $found_target = 1;

            my $tokj;
            foreach my $j ( $i_next_nonblank + 1 .. $i - 1 ) {
                $tokj = $rtokens->[$j];

                # we have to remove any backslash before the quote character
                # so that the here-doc-target exactly matches this string
                next
                  if ( $tokj eq "\\"
                    && $j < $i - 1
                    && $rtokens->[ $j + 1 ] eq $here_quote_character );
                $here_doc_target .= $tokj;
            }
        }
    }

    elsif ( ( $next_token =~ /^\s*$/ ) and ( $expecting == TERM ) ) {
        $found_target = 1;
        $self->write_logfile_entry(
            "found blank here-target after <<; suggest using \"\"\n");
        $i = $ibeg;
    }
    elsif ( $next_token =~ /^\w/ ) {    # simple bareword or integer after <<

        my $here_doc_expected;
        if ( $expecting == UNKNOWN ) {
            $here_doc_expected = $self->guess_if_here_doc($next_token);
        }
        else {
            $here_doc_expected = 1;
        }

        if ($here_doc_expected) {
            $found_target    = 1;
            $here_doc_target = $next_token;
            $i               = $ibeg + 1;
        }

    }
    else {

        if ( $expecting == TERM ) {
            $found_target = 1;
            $self->write_logfile_entry("Note: bare here-doc operator <<\n");
        }
        else {
            $i = $ibeg;
        }
    }

    # patch to neglect any prepended backslash
    if ( $found_target && $backslash ) { $i++ }

    return ( $found_target, $here_doc_target, $here_quote_character, $i,
        $saw_error );
} ## end sub find_here_doc

sub do_quote {

    # follow (or continue following) quoted string(s)
    # $in_quote return code:
    #   0 - ok, found end
    #   1 - still must find end of quote whose target is $quote_character
    #   2 - still looking for end of first of two quotes
    #
    # Returns updated strings:
    #  $quoted_string_1 = quoted string seen while in_quote=1
    #  $quoted_string_2 = quoted string seen while in_quote=2
    my (

        $self,

        $i,
        $in_quote,
        $quote_character,
        $quote_pos,
        $quote_depth,
        $quoted_string_1,
        $quoted_string_2,
        $rtokens,
        $rtoken_type,
        $rtoken_map,
        $max_token_index,

    ) = @_;

    my $quoted_string;
    if ( $in_quote == 2 ) {    # two quotes/quoted_string_1s to follow
        my $ibeg = $i;
        (

            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string
          )
          = $self->follow_quoted_string(

            $ibeg,
            $in_quote,
            $rtokens,
            $rtoken_type,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $max_token_index
          );
        $quoted_string_2 .= $quoted_string;
        if ( $in_quote == 1 ) {
            if ( $quote_character =~ /[\{\[\<\(]/ ) { $i++; }
            $quote_character = EMPTY_STRING;
        }
        else {
            $quoted_string_2 .= "\n";
        }
    }

    if ( $in_quote == 1 ) {    # one (more) quote to follow
        my $ibeg = $i;
        (

            $i,
            $in_quote,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $quoted_string
          )
          = $self->follow_quoted_string(

            $ibeg,
            $in_quote,
            $rtokens,
            $rtoken_type,
            $quote_character,
            $quote_pos,
            $quote_depth,
            $max_token_index
          );
        $quoted_string_1 .= $quoted_string;
        if ( $in_quote == 1 ) {
            $quoted_string_1 .= "\n";
        }
    }
    return (

        $i,
        $in_quote,
        $quote_character,
        $quote_pos,
        $quote_depth,
        $quoted_string_1,
        $quoted_string_2,

    );
} ## end sub do_quote

# Some possible non-word quote delimiters, for preliminary checking
my %is_punct_char;

BEGIN {

    my @q = qw# / " ' { } ( ) [ ] < > ; + - * | % ! x ~ = ? : . ^ & #;
    push @q, '#';
    push @q, ',';
    @is_punct_char{@q} = (1) x scalar(@q);
}

sub follow_quoted_string {

    # scan for a specific token, skipping escaped characters
    # if the quote character is blank, use the first non-blank character
    # input parameters:
    #   $rtokens = reference to the array of tokens
    #   $i = the token index of the first character to search
    #   $in_quote = number of quoted strings being followed
    #   $beginning_tok = the starting quote character
    #   $quote_pos = index to check next for alphanumeric delimiter
    # output parameters:
    #   $i = the token index of the ending quote character
    #   $in_quote = decremented if found end, unchanged if not
    #   $beginning_tok = the starting quote character
    #   $quote_pos = index to check next for alphanumeric delimiter
    #   $quote_depth = nesting depth, since delimiters '{ ( [ <' can be nested.
    #   $quoted_string = the text of the quote (without quotation tokens)
    my (

        $self,

        $i_beg,
        $in_quote,
        $rtokens,
        $rtoken_type,
        $beginning_tok,
        $quote_pos,
        $quote_depth,
        $max_token_index,

    ) = @_;

    my ( $tok, $end_tok );
    my $i             = $i_beg - 1;
    my $quoted_string = EMPTY_STRING;

    0 && do {
        print {*STDOUT}
"QUOTE entering with quote_pos = $quote_pos i=$i beginning_tok =$beginning_tok\n";
    };

    # for a non-blank token, get the corresponding end token
    if (
        $is_punct_char{$beginning_tok}
        || ( length($beginning_tok)
            && $beginning_tok !~ /^\s+$/ )
      )
    {
        $end_tok =
            $matching_end_token{$beginning_tok}
          ? $matching_end_token{$beginning_tok}
          : $beginning_tok;
    }

    # for a blank token, find and use the first non-blank one
    else {
        my $allow_quote_comments = ( $i < 0 ) ? 1 : 0; # i<0 means we saw a <cr>

        while ( $i < $max_token_index ) {
            $tok = $rtokens->[ ++$i ];

            if ( $rtoken_type->[$i] ne 'b' ) {

                if ( ( $tok eq '#' ) && ($allow_quote_comments) ) {
                    $i = $max_token_index;
                }
                else {

                    if ( length($tok) > 1 ) {
                        if ( $quote_pos <= 0 ) { $quote_pos = 1 }
                        $beginning_tok = substr( $tok, $quote_pos - 1, 1 );
                    }
                    else {
                        $beginning_tok = $tok;
                        $quote_pos     = 0;
                    }
                    $end_tok =
                        $matching_end_token{$beginning_tok}
                      ? $matching_end_token{$beginning_tok}
                      : $beginning_tok;
                    $quote_depth = 1;
                    last;
                }
            }
            else {
                $allow_quote_comments = 1;
            }
        }
    }

    # There are two different loops which search for the ending quote
    # character.  In the rare case of an alphanumeric quote delimiter, we
    # have to look through alphanumeric tokens character-by-character, since
    # the pre-tokenization process combines multiple alphanumeric
    # characters, whereas for a non-alphanumeric delimiter, only tokens of
    # length 1 can match.

    #----------------------------------------------------------------
    # Case 1 (rare): loop for case of alphanumeric quote delimiter..
    # "quote_pos" is the position the current word to begin searching
    #----------------------------------------------------------------
    if ( !$is_punct_char{$beginning_tok} && $beginning_tok =~ /\w/ ) {

        # Note this because it is not recommended practice except
        # for obfuscated perl contests
        if ( $in_quote == 1 ) {
            $self->write_logfile_entry(
                "Note: alphanumeric quote delimiter ($beginning_tok) \n");
        }

        # Note: changed < to <= here to fix c109. Relying on extra end blanks.
        while ( $i <= $max_token_index ) {

            if ( $quote_pos == 0 || ( $i < 0 ) ) {
                $tok = $rtokens->[ ++$i ];

                if ( $tok eq '\\' ) {

                    # retain backslash unless it hides the end token
                    $quoted_string .= $tok
                      unless ( $rtokens->[ $i + 1 ] eq $end_tok );
                    $quote_pos++;
                    last if ( $i >= $max_token_index );
                    $tok = $rtokens->[ ++$i ];
                }
            }
            my $old_pos = $quote_pos;

            $quote_pos = 1 + index( $tok, $end_tok, $quote_pos );

            if ( $quote_pos > 0 ) {

                $quoted_string .=
                  substr( $tok, $old_pos, $quote_pos - $old_pos - 1 );

                # NOTE: any quote modifiers will be at the end of '$tok'. If we
                # wanted to check them, this is the place to get them.  But
                # this quote form is rarely used in practice, so it isn't
                # worthwhile.

                $quote_depth--;

                if ( $quote_depth == 0 ) {
                    $in_quote--;
                    last;
                }
            }
            else {
                if ( $old_pos <= length($tok) ) {
                    $quoted_string .= substr( $tok, $old_pos );
                }
            }
        }
    }

    #-----------------------------------------------------------------------
    # Case 2 (normal): loop for case of a non-alphanumeric quote delimiter..
    #-----------------------------------------------------------------------
    else {

        while ( $i < $max_token_index ) {
            $tok = $rtokens->[ ++$i ];

            if ( $tok eq $end_tok ) {
                $quote_depth--;

                if ( $quote_depth == 0 ) {
                    $in_quote--;
                    last;
                }
            }
            elsif ( $tok eq $beginning_tok ) {
                $quote_depth++;
            }
            elsif ( $tok eq '\\' ) {

                # retain backslash unless it hides the beginning or end token
                $tok = $rtokens->[ ++$i ];
                $quoted_string .= '\\'
                  if ( $tok ne $end_tok && $tok ne $beginning_tok );
            }
            else {
                ## nothing special
            }
            $quoted_string .= $tok;
        }
    }
    if ( $i > $max_token_index ) { $i = $max_token_index }
    return (

        $i,
        $in_quote,
        $beginning_tok,
        $quote_pos,
        $quote_depth,
        $quoted_string,

    );
} ## end sub follow_quoted_string

sub indicate_error {
    my ( $self, $msg, $line_number, $input_line, $pos, $carrat ) = @_;
    $self->interrupt_logfile();
    $self->warning($msg);
    $self->write_error_indicator_pair( $line_number, $input_line, $pos,
        $carrat );
    $self->resume_logfile();
    return;
} ## end sub indicate_error

sub write_error_indicator_pair {
    my ( $self, $line_number, $input_line, $pos, $carrat ) = @_;
    my ( $offset, $numbered_line, $underline ) =
      make_numbered_line( $line_number, $input_line, $pos );
    $underline = write_on_underline( $underline, $pos - $offset, $carrat );
    $self->warning( $numbered_line . "\n" );
    $underline =~ s/\s+$//;
    $self->warning( $underline . "\n" );
    return;
} ## end sub write_error_indicator_pair

sub make_numbered_line {

    #  Given an input line, its line number, and a character position of
    #  interest, create a string not longer than 80 characters of the form
    #     $lineno: sub_string
    #  such that the sub_string of $str contains the position of interest
    #
    #  Here is an example of what we want, in this case we add trailing
    #  '...' because the line is long.
    #
    # 2: (One of QAML 2.0's authors is a member of the World Wide Web Con ...
    #
    #  Here is another example, this time in which we used leading '...'
    #  because of excessive length:
    #
    # 2: ... er of the World Wide Web Consortium's
    #
    #  input parameters are:
    #   $lineno = line number
    #   $str = the text of the line
    #   $pos = position of interest (the error) : 0 = first character
    #
    #   We return :
    #     - $offset = an offset which corrects the position in case we only
    #       display part of a line, such that $pos-$offset is the effective
    #       position from the start of the displayed line.
    #     - $numbered_line = the numbered line as above,
    #     - $underline = a blank 'underline' which is all spaces with the same
    #       number of characters as the numbered line.

    my ( $lineno, $str, $pos ) = @_;
    my $offset = ( $pos < 60 ) ? 0 : $pos - 40;
    my $excess = length($str) - $offset - 68;
    my $numc   = ( $excess > 0 ) ? 68 : undef;

    if ( defined($numc) ) {
        if ( $offset == 0 ) {
            $str = substr( $str, $offset, $numc - 4 ) . " ...";
        }
        else {
            $str = "... " . substr( $str, $offset + 4, $numc - 4 ) . " ...";
        }
    }
    else {

        if ( $offset == 0 ) {
        }
        else {
            $str = "... " . substr( $str, $offset + 4 );
        }
    }

    my $numbered_line = sprintf( "%d: ", $lineno );
    $offset -= length($numbered_line);
    $numbered_line .= $str;
    my $underline = SPACE x length($numbered_line);
    return ( $offset, $numbered_line, $underline );
} ## end sub make_numbered_line

sub write_on_underline {

    # The "underline" is a string that shows where an error is; it starts
    # out as a string of blanks with the same length as the numbered line of
    # code above it, and we have to add marking to show where an error is.
    # In the example below, we want to write the string '--^' just below
    # the line of bad code:
    #
    # 2: (One of QAML 2.0's authors is a member of the World Wide Web Con ...
    #                 ---^
    # We are given the current underline string, plus a position and a
    # string to write on it.
    #
    # In the above example, there will be 2 calls to do this:
    # First call:  $pos=19, pos_chr=^
    # Second call: $pos=16, pos_chr=---
    #
    # This is a trivial thing to do with substr, but there is some
    # checking to do.

    my ( $underline, $pos, $pos_chr ) = @_;

    # check for error..shouldn't happen
    if ( $pos < 0 || $pos > length($underline) ) {
        return $underline;
    }
    my $excess = length($pos_chr) + $pos - length($underline);
    if ( $excess > 0 ) {
        $pos_chr = substr( $pos_chr, 0, length($pos_chr) - $excess );
    }
    substr( $underline, $pos, length($pos_chr), $pos_chr );
    return ($underline);
} ## end sub write_on_underline

sub pre_tokenize {

    my ( $str, $max_tokens_wanted ) = @_;

    # Input parameters:
    #  $str = string to be parsed
    #  $max_tokens_wanted > 0  to stop on reaching this many tokens.
    #                     = undef or 0 means get all tokens

    # Break a string, $str, into a sequence of preliminary tokens (pre-tokens).
    # We look for these types of tokens:
    #   words       (type='w'),               example: 'max_tokens_wanted'
    #   digits      (type = 'd'),             example: '0755'
    #   whitespace  (type = 'b'),             example: '   '
    #   single character punct (type = char)  example: '='

    # Later operations will combine one or more of these pre-tokens into final
    # tokens.  We cannot do better than this yet because we might be in a
    # quoted string or pattern.

    # An advantage of doing this pre-tokenization step is that it keeps almost
    # all of the regex parsing very simple and localized right here.  A
    # disadvantage is that in some extremely rare instances we will have to go
    # back and split a pre-token.

    # Return parameters:
    my @tokens    = ();     # array of the tokens themselves
    my @token_map = (0);    # string position of start of each token
    my @type      = ();     # 'b'=whitespace, 'd'=digits, 'w'=alpha, or punct

    if ( !$max_tokens_wanted ) { $max_tokens_wanted = -1 }

    while ( $max_tokens_wanted-- ) {

        if (
            $str =~ m{
             \G(
               (\s+) #     type 'b'  = whitespace - this must come before \W
             | (\W)  #  or type=char = single-character, non-whitespace punct
             | (\d+) #  or type 'd'  = sequence of digits - must come before \w
             | (\w+) #  or type 'w'  = words not starting with a digit
             )
            }gcx
          )
        {
            push @tokens, $1;
            push @type,
              defined($2) ? 'b' : defined($3) ? $1 : defined($4) ? 'd' : 'w';
            push @token_map, pos($str);
        }

        # that's all..
        else {
            return ( \@tokens, \@token_map, \@type );
        }
    }

    return ( \@tokens, \@token_map, \@type );
} ## end sub pre_tokenize

sub show_tokens {

    # this is an old debug routine
    # not called, but saved for reference
    my ( $rtokens, $rtoken_map ) = @_;
    my $num = scalar( @{$rtokens} );

    foreach my $i ( 0 .. $num - 1 ) {
        my $len = length( $rtokens->[$i] );
        print {*STDOUT} "$i:$len:$rtoken_map->[$i]:$rtokens->[$i]:\n";
    }
    return;
} ## end sub show_tokens

sub dump_token_types {
    my ( $class, $fh ) = @_;

    # This should be the latest list of token types in use
    # adding NEW_TOKENS: add a comment here
    $fh->print(<<'END_OF_LIST');

Here is a list of the token types currently used for lines of type 'CODE'.
For the following tokens, the "type" of a token is just the token itself.

.. :: << >> ** && .. || // -> => += -= .= %= &= |= ^= *= <>
( ) <= >= == =~ !~ != ++ -- /= x=
... **= <<= >>= &&= ||= //= <=>
, + - / * | % ! x ~ = \ ? : . < > ^ &

The following additional token types are defined:

 type    meaning
    b    blank (white space)
    {    indent: opening structural curly brace or square bracket or paren
         (code block, anonymous hash reference, or anonymous array reference)
    }    outdent: right structural curly brace or square bracket or paren
    [    left non-structural square bracket (enclosing an array index)
    ]    right non-structural square bracket
    (    left non-structural paren (all but a list right of an =)
    )    right non-structural paren
    L    left non-structural curly brace (enclosing a key)
    R    right non-structural curly brace
    ;    terminal semicolon
    f    indicates a semicolon in a "for" statement
    h    here_doc operator <<
    #    a comment
    Q    indicates a quote or pattern
    q    indicates a qw quote block
    k    a perl keyword
    C    user-defined constant or constant function (with void prototype = ())
    U    user-defined function taking parameters
    G    user-defined function taking block parameter (like grep/map/eval)
    S    sub definition     (reported as type 'i' in older versions)
    P    package definition (reported as type 'i' in older versions)
    t    type indicater such as %,$,@,*,&,sub
    w    bare word (perhaps a subroutine call)
    i    identifier of some type (with leading %, $, @, *, &, sub, -> )
    n    a number
    v    a v-string
    F    a file test operator (like -e)
    Y    File handle
    Z    identifier in indirect object slot: may be file handle, object
    J    LABEL:  code block label
    j    LABEL after next, last, redo, goto
    p    unary +
    m    unary -
    pp   pre-increment operator ++
    mm   pre-decrement operator --
    A    : used as attribute separator

    Here are the '_line_type' codes used internally:
    SYSTEM         - system-specific code before hash-bang line
    CODE           - line of perl code (including comments)
    POD_START      - line starting pod, such as '=head'
    POD            - pod documentation text
    POD_END        - last line of pod section, '=cut'
    HERE           - text of here-document
    HERE_END       - last line of here-doc (target word)
    FORMAT         - format section
    FORMAT_END     - last line of format section, '.'
    SKIP           - code skipping section
    SKIP_END       - last line of code skipping section, '#>>V'
    DATA_START     - __DATA__ line
    DATA           - unidentified text following __DATA__
    END_START      - __END__ line
    END            - unidentified text following __END__
    ERROR          - we are in big trouble, probably not a perl script
END_OF_LIST

    return;
} ## end sub dump_token_types

#------------------
# About Token Types
#------------------

# The array "valid_token_types" in the BEGIN section has an up-to-date list
# of token types.  Sub 'dump_token_types' should be kept up to date with
# token types.

# Ideally, tokens are the smallest pieces of text
# such that a newline may be inserted between any pair of tokens without
# changing or invalidating the program. This version comes close to this,
# although there are necessarily a few exceptions which must be caught by
# the formatter.  Many of these involve the treatment of bare words.
#
# To simplify things, token types are either a single character, or they
# are identical to the tokens themselves.
#
# As a debugging aid, the -D flag creates a file containing a side-by-side
# comparison of the input string and its tokenization for each line of a file.
# This is an invaluable debugging aid.
#
# In addition to tokens, and some associated quantities, the tokenizer
# also returns flags indication any special line types.  These include
# quotes, here_docs, formats.
#
#------------------
# Adding NEW_TOKENS
#------------------
#
# Here are some notes on the minimal steps.  I wrote these notes while
# adding the 'v' token type for v-strings, which are things like version
# numbers 5.6.0, and ip addresses, and will use that as an example.  ( You
# can use your editor to search for the string "NEW_TOKENS" to find the
# appropriate sections to change):

# *. For another example, search for the smartmatch operator '~~'
# with your editor to see where updates were made for it.

# *. For another example, search for the string 'c250', which shows
# locations where changes for new types 'P' and 'S' were made.

# *. Think of a new, unused character for the token type, and add to
# the array @valid_token_types in the BEGIN section of this package.
# For example, I used 'v' for v-strings.
#
# *. Implement coding to recognize the $type of the token in this routine.
# This is the hardest part, and is best done by imitating or modifying
# some of the existing coding.  For example, to recognize v-strings, I
# patched 'sub scan_bare_identifier' to recognize v-strings beginning with
# 'v' and 'sub scan_number' to recognize v-strings without the leading 'v'.
#
# *. Update sub operator_expected.  This update is critically important but
# the coding is trivial.  Look at the comments in that routine for help.
# For v-strings, which should behave like numbers, I just added 'v' to the
# regex used to handle numbers and strings (types 'n' and 'Q').
#
# *. Implement a 'bond strength' rule in sub set_bond_strengths in
# Perl::Tidy::Formatter for breaking lines around this token type.  You can
# skip this step and take the default at first, then adjust later to get
# desired results.  For adding type 'v', I looked at sub bond_strength and
# saw that number type 'n' was using default strengths, so I didn't do
# anything.  I may tune it up someday if I don't like the way line
# breaks with v-strings look.
#
# *. Implement a 'whitespace' rule in sub set_whitespace_flags in
# Perl::Tidy::Formatter.  For adding type 'v', I looked at this routine
# and saw that type 'n' used spaces on both sides, so I just added 'v'
# to the array @spaces_both_sides.
#
# *. Update HtmlWriter package so that users can colorize the token as
# desired.  This is quite easy; see comments identified by 'NEW_TOKENS' in
# that package.  For v-strings, I initially chose to use a default color
# equal to the default for numbers, but it might be nice to change that
# eventually.
#
# *. Update comments in Perl::Tidy::Tokenizer::dump_token_types.
#
# *. Run lots and lots of debug tests.  Start with special files designed
# to test the new token type.  Run with the -D flag to create a .DEBUG
# file which shows the tokenization.  When these work ok, test as many old
# scripts as possible.  Start with all of the '.t' files in the 'test'
# directory of the distribution file.  Compare .tdy output with previous
# version and updated version to see the differences.  Then include as
# many more files as possible. My own technique has been to collect a huge
# number of perl scripts (thousands!) into one directory and run perltidy
# *, then run diff between the output of the previous version and the
# current version.

BEGIN {

    # These names are used in error messages
    @opening_brace_names = qw# '{' '[' '(' '?' #;
    @closing_brace_names = qw# '}' ']' ')' ':' #;

    my @q;

    my @digraphs = qw(
      .. :: << >> ** && || // -> => += -= .= %= &= |= ^= *= <>
      <= >= == =~ !~ != ++ -- /= x= ~~ ~. |. &. ^.
    );
    @is_digraph{@digraphs} = (1) x scalar(@digraphs);

    @q = qw(
      . : < > * & | / - = + -  %  ^ !  x ~
    );
    @can_start_digraph{@q} = (1) x scalar(@q);

    my @trigraphs = qw( ... **= <<= >>= &&= ||= //= <=> !~~ &.= |.= ^.= <<~);
    @is_trigraph{@trigraphs} = (1) x scalar(@trigraphs);

    my @tetragraphs = qw( <<>> );
    @is_tetragraph{@tetragraphs} = (1) x scalar(@tetragraphs);

    # make a hash of all valid token types for self-checking the tokenizer
    # (adding NEW_TOKENS : select a new character and add to this list)
    # fix for c250: added new token type 'P' and 'S'
    my @valid_token_types = qw#
      A b C G L R f h Q k t w i q n p m F pp mm U j J Y Z v P S
      { } ( ) [ ] ; + - / * | % ! x ~ = \ ? : . < > ^ &
      #;
    push( @valid_token_types, @digraphs );
    push( @valid_token_types, @trigraphs );
    push( @valid_token_types, @tetragraphs );
    push( @valid_token_types, ( '#', ',', 'CORE::' ) );
    @is_valid_token_type{@valid_token_types} = (1) x scalar(@valid_token_types);

    # a list of file test letters, as in -e (Table 3-4 of 'camel 3')
    my @file_test_operators =
      qw( A B C M O R S T W X b c d e f g k l o p r s t u w x z);
    @is_file_test_operator{@file_test_operators} =
      (1) x scalar(@file_test_operators);

    # these functions have prototypes of the form (&), so when they are
    # followed by a block, that block MAY BE followed by an operator.
    # Smartmatch operator ~~ may be followed by anonymous hash or array ref
    @q = qw( do eval );
    @is_block_operator{@q} = (1) x scalar(@q);

    # these functions allow an identifier in the indirect object slot
    @q = qw( print printf sort exec system say);
    @is_indirect_object_taker{@q} = (1) x scalar(@q);

    # Note: 'field' will be added by sub check_options if --use-feature=class
    @q = qw(my our state);
    @is_my_our_state{@q} = (1) x scalar(@q);

    # These tokens may precede a code block
    # patched for SWITCH/CASE/CATCH.  Actually these could be removed
    # now and we could let the extended-syntax coding handle them.
    # Added 'default' for Switch::Plain.
    # Note: 'ADJUST' will be added by sub check_options if --use-feature=class
    @q =
      qw( BEGIN END CHECK INIT AUTOLOAD DESTROY UNITCHECK continue if elsif else
      unless do while until eval for foreach map grep sort
      switch case given when default catch try finally);
    @is_code_block_token{@q} = (1) x scalar(@q);

    # Note: this hash was formerly named '%is_not_zero_continuation_block_type'
    # to contrast it with the block types in '%is_zero_continuation_block_type'
    @q = qw( sort map grep eval do );
    @is_sort_map_grep_eval_do{@q} = (1) x scalar(@q);

    @q = qw( sort map grep );
    @is_sort_map_grep{@q} = (1) x scalar(@q);

    %is_grep_alias = ();

    # I'll build the list of keywords incrementally
    my @Keywords = ();

    # keywords and tokens after which a value or pattern is expected,
    # but not an operator.  In other words, these should consume terms
    # to their right, or at least they are not expected to be followed
    # immediately by operators.
    my @value_requestor = qw(
      AUTOLOAD
      BEGIN
      CHECK
      DESTROY
      END
      EQ
      GE
      GT
      INIT
      LE
      LT
      NE
      UNITCHECK
      abs
      accept
      alarm
      and
      atan2
      bind
      binmode
      bless
      break
      caller
      chdir
      chmod
      chomp
      chop
      chown
      chr
      chroot
      close
      closedir
      cmp
      connect
      continue
      cos
      crypt
      dbmclose
      dbmopen
      defined
      delete
      die
      dump
      each
      else
      elsif
      eof
      eq
      evalbytes
      exec
      exists
      exit
      exp
      fc
      fcntl
      fileno
      flock
      for
      foreach
      formline
      ge
      getc
      getgrgid
      getgrnam
      gethostbyaddr
      gethostbyname
      getnetbyaddr
      getnetbyname
      getpeername
      getpgrp
      getpriority
      getprotobyname
      getprotobynumber
      getpwnam
      getpwuid
      getservbyname
      getservbyport
      getsockname
      getsockopt
      glob
      gmtime
      goto
      grep
      gt
      hex
      if
      index
      int
      ioctl
      join
      keys
      kill
      last
      lc
      lcfirst
      le
      length
      link
      listen
      local
      localtime
      lock
      log
      lstat
      lt
      map
      mkdir
      msgctl
      msgget
      msgrcv
      msgsnd
      my
      ne
      next
      no
      not
      oct
      open
      opendir
      or
      ord
      our
      pack
      pipe
      pop
      pos
      print
      printf
      prototype
      push
      quotemeta
      rand
      read
      readdir
      readlink
      readline
      readpipe
      recv
      redo
      ref
      rename
      require
      reset
      return
      reverse
      rewinddir
      rindex
      rmdir
      scalar
      seek
      seekdir
      select
      semctl
      semget
      semop
      send
      sethostent
      setnetent
      setpgrp
      setpriority
      setprotoent
      setservent
      setsockopt
      shift
      shmctl
      shmget
      shmread
      shmwrite
      shutdown
      sin
      sleep
      socket
      socketpair
      sort
      splice
      split
      sprintf
      sqrt
      srand
      stat
      state
      study
      substr
      symlink
      syscall
      sysopen
      sysread
      sysseek
      system
      syswrite
      tell
      telldir
      tie
      tied
      truncate
      uc
      ucfirst
      umask
      undef
      unless
      unlink
      unpack
      unshift
      untie
      until
      use
      utime
      values
      vec
      waitpid
      warn
      while
      write
      xor

      switch
      case
      default
      given
      when
      err
      say
      isa

      catch

    );

    # Note: 'ADJUST', 'field' are added by sub check_options
    # if --use-feature=class

    # patched above for SWITCH/CASE given/when err say
    # 'err' is a fairly safe addition.
    # Added 'default' for Switch::Plain. Note that we could also have
    # a separate set of keywords to include if we see 'use Switch::Plain'
    push( @Keywords, @value_requestor );

    # These are treated the same but are not keywords:
    my @extra_vr = qw(
      constant
      vars
    );
    push( @value_requestor, @extra_vr );

    @expecting_term_token{@value_requestor} = (1) x scalar(@value_requestor);

    # this list contains keywords which do not look for arguments,
    # so that they might be followed by an operator, or at least
    # not a term.
    my @operator_requestor = qw(
      endgrent
      endhostent
      endnetent
      endprotoent
      endpwent
      endservent
      fork
      getgrent
      gethostent
      getlogin
      getnetent
      getppid
      getprotoent
      getpwent
      getservent
      setgrent
      setpwent
      time
      times
      wait
      wantarray
    );

    push( @Keywords, @operator_requestor );

    # These are treated the same but are not considered keywords:
    my @extra_or = qw(
      STDERR
      STDIN
      STDOUT
    );

    push( @operator_requestor, @extra_or );

    @expecting_operator_token{@operator_requestor} =
      (1) x scalar(@operator_requestor);

    # these token TYPES expect trailing operator but not a term
    # note: ++ and -- are post-increment and decrement, 'C' = constant
    my @operator_requestor_types = qw( ++ -- C <> q );

    # NOTE: This hash is available but not currently used
    @expecting_operator_types{@operator_requestor_types} =
      (1) x scalar(@operator_requestor_types);

    # these token TYPES consume values (terms)
    # note: pp and mm are pre-increment and decrement
    # f=semicolon in for,  F=file test operator
    my @value_requestor_type = qw#
      L { ( [ ~ !~ =~ ; . .. ... A : && ! || // = + - x
      **= += -= .= /= *= %= x= &= |= ^= <<= >>= &&= ||= //=
      <= >= == != => \ > < % * / ? & | ** <=> ~~ !~~ <<~
      f F pp mm Y p m U J G j >> << ^ t
      ~. ^. |. &. ^.= |.= &.=
      #;
    push( @value_requestor_type, ',' )
      ;    # (perl doesn't like a ',' in a qw block)

    # NOTE: This hash is available but not currently used
    @expecting_term_types{@value_requestor_type} =
      (1) x scalar(@value_requestor_type);

    # Note: the following valid token types are not assigned here to
    # hashes requesting to be followed by values or terms, but are
    # instead currently hard-coded into sub operator_expected:
    # ) -> :: Q R Z ] b h i k n v w } #

    # For simple syntax checking, it is nice to have a list of operators which
    # will really be unhappy if not followed by a term.  This includes most
    # of the above...
    @really_want_term{@value_requestor_type} =
      (1) x scalar(@value_requestor_type);

    # with these exceptions...
    delete $really_want_term{'U'}; # user sub, depends on prototype
    delete $really_want_term{'F'}; # file test works on $_ if no following term
    delete $really_want_term{'Y'}; # indirect object, too risky to check syntax;
                                   # let perl do it
    @q = qw(q qq qx qr s y tr m);
    @is_q_qq_qx_qr_s_y_tr_m{@q} = (1) x scalar(@q);

    # Note added 'qw' here
    @q = qw(q qq qw qx qr s y tr m);
    @is_q_qq_qw_qx_qr_s_y_tr_m{@q} = (1) x scalar(@q);

    # Quote modifiers:
    # original ref: camel 3 p 147,
    # but perl may accept undocumented flags
    # perl 5.10 adds 'p' (preserve)
    # Perl version 5.22 added 'n'
    # From http://perldoc.perl.org/perlop.html we have
    # /PATTERN/msixpodualngc or m?PATTERN?msixpodualngc
    # s/PATTERN/REPLACEMENT/msixpodualngcer
    # y/SEARCHLIST/REPLACEMENTLIST/cdsr
    # tr/SEARCHLIST/REPLACEMENTLIST/cdsr
    # qr/STRING/msixpodualn
    %quote_modifiers = (
        's'  => '[msixpodualngcer]',
        'y'  => '[cdsr]',
        'tr' => '[cdsr]',
        'm'  => '[msixpodualngc]',
        'qr' => '[msixpodualn]',
        'q'  => EMPTY_STRING,
        'qq' => EMPTY_STRING,
        'qw' => EMPTY_STRING,
        'qx' => EMPTY_STRING,
    );

    # Note: 'class' will be added by sub check_options if -use-feature=class
    @q = qw(package);
    @is_package{@q} = (1) x scalar(@q);

    @q = qw( if elsif unless );
    @is_if_elsif_unless{@q} = (1) x scalar(@q);

    @q = qw( ; t );
    @is_semicolon_or_t{@q} = (1) x scalar(@q);

    @q = qw( if elsif unless case when );
    @is_if_elsif_unless_case_when{@q} = (1) x scalar(@q);

    # Hash of other possible line endings which may occur.
    # Keep these coordinated with the regex where this is used.
    # Note: chr(13) = chr(015)="\r".
    @q = ( chr(13), chr(29), chr(26) );
    @other_line_endings{@q} = (1) x scalar(@q);

    # These keywords are handled specially in the tokenizer code:
    my @special_keywords = qw(
      do
      eval
      format
      m
      package
      q
      qq
      qr
      qw
      qx
      s
      sub
      tr
      y
    );
    push( @Keywords, @special_keywords );

    # Keywords after which list formatting may be used
    # WARNING: do not include |map|grep|eval or perl may die on
    # syntax errors (map1.t).
    my @keyword_taking_list = qw(
      and
      chmod
      chomp
      chop
      chown
      dbmopen
      die
      elsif
      exec
      fcntl
      for
      foreach
      formline
      getsockopt
      if
      index
      ioctl
      join
      kill
      local
      msgctl
      msgrcv
      msgsnd
      my
      open
      or
      our
      pack
      print
      printf
      push
      read
      readpipe
      recv
      return
      reverse
      rindex
      seek
      select
      semctl
      semget
      send
      setpriority
      setsockopt
      shmctl
      shmget
      shmread
      shmwrite
      socket
      socketpair
      sort
      splice
      split
      sprintf
      state
      substr
      syscall
      sysopen
      sysread
      sysseek
      system
      syswrite
      tie
      unless
      unlink
      unpack
      unshift
      until
      vec
      warn
      while
      given
      when
    );

    # NOTE: This hash is available but not currently used
    @is_keyword_taking_list{@keyword_taking_list} =
      (1) x scalar(@keyword_taking_list);

    # perl functions which may be unary operators.

    # This list is used to decide if a pattern delimited by slashes, /pattern/,
    # can follow one of these keywords.
    @q = qw(
      chomp eof eval fc lc pop shift uc undef
    );

    @is_keyword_rejecting_slash_as_pattern_delimiter{@q} =
      (1) x scalar(@q);

    # These are keywords for which an arg may optionally be omitted.  They are
    # currently only used to disambiguate a ? used as a ternary from one used
    # as a (deprecated) pattern delimiter.  In the future, they might be used
    # to give a warning about ambiguous syntax before a /.
    # Note: split has been omitted (see note below).
    my @keywords_taking_optional_arg = qw(
      abs
      alarm
      caller
      chdir
      chomp
      chop
      chr
      chroot
      close
      cos
      defined
      die
      eof
      eval
      evalbytes
      exit
      exp
      fc
      getc
      glob
      gmtime
      hex
      int
      last
      lc
      lcfirst
      length
      localtime
      log
      lstat
      mkdir
      next
      oct
      ord
      pop
      pos
      print
      printf
      prototype
      quotemeta
      rand
      readline
      readlink
      readpipe
      redo
      ref
      require
      reset
      reverse
      rmdir
      say
      select
      shift
      sin
      sleep
      sqrt
      srand
      stat
      study
      tell
      uc
      ucfirst
      umask
      undef
      unlink
      warn
      write
    );
    @is_keyword_taking_optional_arg{@keywords_taking_optional_arg} =
      (1) x scalar(@keywords_taking_optional_arg);

    # This list is used to decide if a pattern delimited by question marks,
    # ?pattern?, can follow one of these keywords.  Note that from perl 5.22
    # on, a ?pattern? is not recognized, so we can be much more strict than
    # with a /pattern/. Note that 'split' is not in this list. In current
    # versions of perl a question following split must be a ternary, but
    # in older versions it could be a pattern.  The guessing algorithm will
    # decide.  We are combining two lists here to simplify the test.
    @q = ( @keywords_taking_optional_arg, @operator_requestor );
    @is_keyword_rejecting_question_as_pattern_delimiter{@q} =
      (1) x scalar(@q);

    # These are not used in any way yet
    #    my @unused_keywords = qw(
    #     __FILE__
    #     __LINE__
    #     __PACKAGE__
    #     );

    #  The list of keywords was originally extracted from function 'keyword' in
    #  perl file toke.c version 5.005.03, using this utility, plus a
    #  little editing: (file getkwd.pl):
    #  while (<>) { while (/\"(.*)\"/g) { print "$1\n"; } }
    #  Add 'get' prefix where necessary, then split into the above lists.
    #  This list should be updated as necessary.
    #  The list should not contain these special variables:
    #  ARGV DATA ENV SIG STDERR STDIN STDOUT
    #  __DATA__ __END__

    @is_keyword{@Keywords} = (1) x scalar(@Keywords);

    %matching_end_token = (
        '{' => '}',
        '(' => ')',
        '[' => ']',
        '<' => '>',
    );
} ## end BEGIN

} ## end package Perl::Tidy::Tokenizer
1;
