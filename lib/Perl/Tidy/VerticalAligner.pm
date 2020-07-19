package Perl::Tidy::VerticalAligner;
use strict;
use warnings;
our $VERSION = '20200619.02';

use Perl::Tidy::VerticalAligner::Alignment;
use Perl::Tidy::VerticalAligner::Line;

# The Perl::Tidy::VerticalAligner package collects output lines and
# attempts to line up certain common tokens, such as => and #, which are
# identified by the calling routine.
#
# There are two main routines: valign_input and flush.  Append acts as a
# storage buffer, collecting lines into a group which can be vertically
# aligned.  When alignment is no longer possible or desirable, it dumps
# the group to flush.
#
#     valign_input -----> flush
#
#     collects          writes
#     vertical          one
#     groups            group

BEGIN {

    # Caution: these debug flags produce a lot of output
    # They should all be 0 except when debugging small scripts

    use constant VALIGN_DEBUG_FLAG_APPEND  => 0;
    use constant VALIGN_DEBUG_FLAG_APPEND0 => 0;
    use constant VALIGN_DEBUG_FLAG_TERNARY => 0;
    use constant VALIGN_DEBUG_FLAG_TABS    => 0;

    my $debug_warning = sub {
        print STDOUT "VALIGN_DEBUGGING with key $_[0]\n";
        return;
    };

    VALIGN_DEBUG_FLAG_APPEND  && $debug_warning->('APPEND');
    VALIGN_DEBUG_FLAG_APPEND0 && $debug_warning->('APPEND0');
    VALIGN_DEBUG_FLAG_TERNARY && $debug_warning->('TERNARY');
    VALIGN_DEBUG_FLAG_TABS    && $debug_warning->('TABS');

}

use vars qw(
  $vertical_aligner_self
  $maximum_alignment_index
  $ralignment_list
  $maximum_jmax_seen
  $minimum_jmax_seen
  $previous_minimum_jmax_seen
  $previous_maximum_jmax_seen
  @group_lines
  $group_level
  $group_type
  $group_maximum_gap
  $marginal_match
  $last_level_written
  $last_leading_space_count
  $extra_indent_ok
  $zero_count
  $last_comment_column
  $last_side_comment_line_number
  $last_side_comment_length
  $last_side_comment_level
  $outdented_line_count
  $first_outdented_line_at
  $last_outdented_line_at
  $diagnostics_object
  $logger_object
  $file_writer_object
  @side_comment_history
  $comment_leading_space_count
  $is_matching_terminal_line
  $consecutive_block_comments

  $cached_line_text
  $cached_line_text_length
  $cached_line_type
  $cached_line_flag
  $cached_seqno
  $cached_line_valid
  $cached_line_leading_space_count
  $cached_seqno_string

  $valign_buffer_filling
  @valign_buffer

  $seqno_string
  $last_nonblank_seqno_string

  $rOpts

  $rOpts_maximum_line_length
  $rOpts_variable_maximum_line_length
  $rOpts_continuation_indentation
  $rOpts_indent_columns
  $rOpts_tabs
  $rOpts_entab_leading_whitespace
  $rOpts_valign

  $rOpts_fixed_position_side_comment
  $rOpts_minimum_space_to_comment

);

sub initialize {

    my ( $class, @args ) = @_;

    my %defaults = (
        rOpts              => undef,
        file_writer_object => undef,
        logger_object      => undef,
        diagnostics_object => undef,
        length_function    => sub { return length( $_[0] ) },
    );
    my %args = ( %defaults, @args );

    $rOpts              = $args{rOpts};
    $file_writer_object = $args{file_writer_object};
    $logger_object      = $args{logger_object};
    $diagnostics_object = $args{diagnostics_object};
    my $length_function = $args{length_function};

    # variables describing the entire space group:
    $ralignment_list            = [];
    $group_level                = 0;
    $last_level_written         = -1;
    $extra_indent_ok            = 0;    # can we move all lines to the right?
    $last_side_comment_length   = 0;
    $maximum_jmax_seen          = 0;
    $minimum_jmax_seen          = 0;
    $previous_minimum_jmax_seen = 0;
    $previous_maximum_jmax_seen = 0;

    # variables describing each line of the group
    @group_lines = ();                  # list of all lines in group

    $outdented_line_count          = 0;
    $first_outdented_line_at       = 0;
    $last_outdented_line_at        = 0;
    $last_side_comment_line_number = 0;
    $last_side_comment_level       = -1;
    $is_matching_terminal_line     = 0;

    # most recent 3 side comments; [ line number, column ]
    $side_comment_history[0] = [ -300, 0 ];
    $side_comment_history[1] = [ -200, 0 ];
    $side_comment_history[2] = [ -100, 0 ];

    # valign_output_step_B cache:
    $cached_line_text                = "";
    $cached_line_text_length         = 0;
    $cached_line_type                = 0;
    $cached_line_flag                = 0;
    $cached_seqno                    = 0;
    $cached_line_valid               = 0;
    $cached_line_leading_space_count = 0;
    $cached_seqno_string             = "";

    # string of sequence numbers joined together
    $seqno_string               = "";
    $last_nonblank_seqno_string = "";

    # frequently used parameters
    $rOpts_indent_columns           = $rOpts->{'indent-columns'};
    $rOpts_tabs                     = $rOpts->{'tabs'};
    $rOpts_entab_leading_whitespace = $rOpts->{'entab-leading-whitespace'};
    $rOpts_fixed_position_side_comment =
      $rOpts->{'fixed-position-side-comment'};
    $rOpts_minimum_space_to_comment = $rOpts->{'minimum-space-to-comment'};
    $rOpts_maximum_line_length      = $rOpts->{'maximum-line-length'};
    $rOpts_variable_maximum_line_length =
      $rOpts->{'variable-maximum-line-length'};
    $rOpts_valign = $rOpts->{'valign'};

    $consecutive_block_comments = 0;
    forget_side_comment();

    initialize_for_new_group();

    $vertical_aligner_self = { length_function => $length_function, };
    bless $vertical_aligner_self, $class;
    return $vertical_aligner_self;
}

sub initialize_for_new_group {
    @group_lines                 = ();
    $maximum_alignment_index     = -1;  # alignments in current group
    $zero_count                  = 0;   # count consecutive lines without tokens
    $group_maximum_gap           = 0;   # largest gap introduced
    $group_type                  = "";
    $marginal_match              = 0;
    $comment_leading_space_count = 0;
    $last_leading_space_count    = 0;
    return;
}

# interface to Perl::Tidy::Diagnostics routines
sub write_diagnostics {
    my $msg = shift;
    if ($diagnostics_object) {
        $diagnostics_object->write_diagnostics($msg);
    }
    return;
}

# interface to Perl::Tidy::Logger routines
sub warning {
    my ($msg) = @_;
    if ($logger_object) {
        $logger_object->warning($msg);
    }
    return;
}

sub write_logfile_entry {
    my ($msg) = @_;
    if ($logger_object) {
        $logger_object->write_logfile_entry($msg);
    }
    return;
}

sub report_definite_bug {
    if ($logger_object) {
        $logger_object->report_definite_bug();
    }
    return;
}

sub get_cached_line_count {
    my $self = shift;
    return @group_lines + ( $cached_line_type ? 1 : 0 );
}

sub get_spaces {

    # return the number of leading spaces associated with an indentation
    # variable $indentation is either a constant number of spaces or an
    # object with a get_spaces method.
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

sub get_stack_depth {

    my $indentation = shift;
    return ref($indentation) ? $indentation->get_stack_depth() : 0;
}

sub make_alignment {
    my ( $col, $token ) = @_;

    # make one new alignment at column $col which aligns token $token
    ++$maximum_alignment_index;

    #my $alignment = new Perl::Tidy::VerticalAligner::Alignment(
    my $nlines    = @group_lines;
    my $alignment = Perl::Tidy::VerticalAligner::Alignment->new(
        column          => $col,
        starting_column => $col,
        matching_token  => $token,
        starting_line   => $nlines - 1,
        ending_line     => $nlines - 1,
        serial_number   => $maximum_alignment_index,
    );
    $ralignment_list->[$maximum_alignment_index] = $alignment;
    return $alignment;
}

sub dump_alignments {
    print STDOUT
"Current Alignments:\ni\ttoken\tstarting_column\tcolumn\tstarting_line\tending_line\n";
    for my $i ( 0 .. $maximum_alignment_index ) {
        my $column          = $ralignment_list->[$i]->get_column();
        my $starting_column = $ralignment_list->[$i]->get_starting_column();
        my $matching_token  = $ralignment_list->[$i]->get_matching_token();
        my $starting_line   = $ralignment_list->[$i]->get_starting_line();
        my $ending_line     = $ralignment_list->[$i]->get_ending_line();
        print STDOUT
"$i\t$matching_token\t$starting_column\t$column\t$starting_line\t$ending_line\n";
    }
    return;
}

sub save_alignment_columns {
    for my $i ( 0 .. $maximum_alignment_index ) {
        $ralignment_list->[$i]->save_column();
    }
    return;
}

sub restore_alignment_columns {
    for my $i ( 0 .. $maximum_alignment_index ) {
        $ralignment_list->[$i]->restore_column();
    }
    return;
}

sub forget_side_comment {
    $last_comment_column = 0;
    return;
}

sub maximum_line_length_for_level {

    # return maximum line length for line starting with a given level
    my $maximum_line_length = $rOpts_maximum_line_length;
    if ($rOpts_variable_maximum_line_length) {
        my $level = shift;
        if ( $level < 0 ) { $level = 0 }
        $maximum_line_length += $level * $rOpts_indent_columns;
    }
    return $maximum_line_length;
}

sub push_group_line {

    my ($new_line) = @_;
    push @group_lines, $new_line;
    return;
}

sub valign_input {

    # Place one line in the current vertical group.
    #
    # The input parameters are:
    #     $level = indentation level of this line
    #     $rfields = reference to array of fields
    #     $rpatterns = reference to array of patterns, one per field
    #     $rtokens   = reference to array of tokens starting fields 1,2,..
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
    # group if possible.  Otherwise it causes the current group to be dumped
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

    my ($rline_hash) = @_;

    my $level                     = $rline_hash->{level};
    my $level_end                 = $rline_hash->{level_end};
    my $indentation               = $rline_hash->{indentation};
    my $is_forced_break           = $rline_hash->{is_forced_break};
    my $outdent_long_lines        = $rline_hash->{outdent_long_lines};
    my $is_terminal_ternary       = $rline_hash->{is_terminal_ternary};
    my $is_terminal_statement     = $rline_hash->{is_terminal_statement};
    my $do_not_pad                = $rline_hash->{do_not_pad};
    my $rvertical_tightness_flags = $rline_hash->{rvertical_tightness_flags};
    my $level_jump                = $rline_hash->{level_jump};
    my $rfields                   = $rline_hash->{rfields};
    my $rtokens                   = $rline_hash->{rtokens};
    my $rpatterns                 = $rline_hash->{rpatterns};
    my $rfield_lengths            = $rline_hash->{rfield_lengths};

    # number of fields is $jmax
    # number of tokens between fields is $jmax-1
    my $jmax = @{$rfields} - 1;

    my $leading_space_count = get_spaces($indentation);

    # set outdented flag to be sure we either align within statements or
    # across statement boundaries, but not both.
    my $is_outdented = $last_leading_space_count > $leading_space_count;
    $last_leading_space_count = $leading_space_count;

    # Patch: undo for hanging side comment
    my $is_hanging_side_comment =
      ( $jmax == 1 && $rtokens->[0] eq '#' && $rfields->[0] =~ /^\s*$/ );
    $is_outdented = 0 if $is_hanging_side_comment;

    # Forget side comment alignment after seeing 2 or more block comments
    my $is_block_comment = ( $jmax == 0 && $rfields->[0] =~ /^#/ );
    if ($is_block_comment) {
        $consecutive_block_comments++;
    }
    else {
        if ( $consecutive_block_comments > 1 ) { forget_side_comment() }
        $consecutive_block_comments = 0;
    }

    VALIGN_DEBUG_FLAG_APPEND0 && do {
        my $nlines = @group_lines;
        print STDOUT
"APPEND0: entering lines=$nlines new #fields= $jmax, leading_count=$leading_space_count last_cmt=$last_comment_column force=$is_forced_break, level_jump=$level_jump, level=$level, group_level=$group_level, level_jump=$level_jump\n";
    };

    # Validate cached line if necessary: If we can produce a container
    # with just 2 lines total by combining an existing cached opening
    # token with the closing token to follow, then we will mark both
    # cached flags as valid.
    if ($rvertical_tightness_flags) {
        if (   @group_lines <= 1
            && $cached_line_type
            && $cached_seqno
            && $rvertical_tightness_flags->[2]
            && $rvertical_tightness_flags->[2] == $cached_seqno )
        {
            $rvertical_tightness_flags->[3] ||= 1;
            $cached_line_valid ||= 1;
        }
    }

    # do not join an opening block brace with an unbalanced line
    # unless requested with a flag value of 2
    if (   $cached_line_type == 3
        && !@group_lines
        && $cached_line_flag < 2
        && $level_jump != 0 )
    {
        $cached_line_valid = 0;
    }

    # patch until new aligner is finished
    if ($do_not_pad) { my_flush() }

    # shouldn't happen:
    if ( $level < 0 ) { $level = 0 }

    # do not align code across indentation level changes
    # or if vertical alignment is turned off for debugging
    if ( $level != $group_level || $is_outdented || !$rOpts_valign ) {

        # we are allowed to shift a group of lines to the right if its
        # level is greater than the previous and next group
        $extra_indent_ok =
          ( $level < $group_level && $last_level_written < $group_level );

        my_flush();

        # If we know that this line will get flushed out by itself because
        # of level changes, we can leave the extra_indent_ok flag set.
        # That way, if we get an external flush call, we will still be
        # able to do some -lp alignment if necessary.
        $extra_indent_ok = ( $is_terminal_statement && $level > $group_level );

        $group_level = $level;

        # wait until after the above flush to get the leading space
        # count because it may have been changed if the -icp flag is in
        # effect
        $leading_space_count = get_spaces($indentation);

    }

    # --------------------------------------------------------------------
    # Collect outdentable block COMMENTS
    # --------------------------------------------------------------------
    my $is_blank_line = "";
    if ( $group_type eq 'COMMENT' ) {
        if (
            (
                   $is_block_comment
                && $outdent_long_lines
                && $leading_space_count == $comment_leading_space_count
            )
            || $is_blank_line
          )
        {
            push_group_line( [ $rfields->[0], $rfield_lengths->[0] ] );
            return;
        }
        else {
            my_flush();
        }
    }

    # --------------------------------------------------------------------
    # add dummy fields for terminal ternary
    # --------------------------------------------------------------------
    my $j_terminal_match;

    if ( $is_terminal_ternary && @group_lines ) {
        $j_terminal_match = fix_terminal_ternary(
            $group_lines[-1], $rfields, $rtokens,
            $rpatterns,       $rfield_lengths
        );
        $jmax = @{$rfields} - 1;
    }

    # --------------------------------------------------------------------
    # add dummy fields for else statement
    # --------------------------------------------------------------------

    if (   $rfields->[0] =~ /^else\s*$/
        && @group_lines
        && $level_jump == 0 )
    {

        $j_terminal_match = fix_terminal_else(
            $group_lines[-1], $rfields, $rtokens,
            $rpatterns,       $rfield_lengths
        );
        $jmax = @{$rfields} - 1;
    }

    # --------------------------------------------------------------------
    # Handle simple line of code with no fields to match.
    # --------------------------------------------------------------------
    if ( $jmax <= 0 ) {
        $zero_count++;

        if ( @group_lines
            && !get_recoverable_spaces( $group_lines[0]->get_indentation() ) )
        {

            # flush the current group if it has some aligned columns..
            if ( $group_lines[0]->get_jmax() > 1 ) { my_flush() }

            # flush current group if we are just collecting side comments..
            elsif (

                # ...and we haven't seen a comment lately
                ( $zero_count > 3 )

                # ..or if this new line doesn't fit to the left of the comments
                || ( ( $leading_space_count + $rfield_lengths->[0] ) >
                    $group_lines[0]->get_column(0) )
              )
            {
                my_flush();
            }
        }

        # start new COMMENT group if this comment may be outdented
        if (   $is_block_comment
            && $outdent_long_lines
            && !@group_lines )
        {
            $group_type                  = 'COMMENT';
            $comment_leading_space_count = $leading_space_count;
            push_group_line( [ $rfields->[0], $rfield_lengths->[0] ] );
            return;
        }

        # just write this line directly if no current group, no side comment,
        # and no space recovery is needed.
        if ( !@group_lines && !get_recoverable_spaces($indentation) ) {

            valign_output_step_B(
                leading_space_count       => $leading_space_count,
                line                      => $rfields->[0],
                line_length               => $rfield_lengths->[0],
                side_comment_length       => 0,
                outdent_long_lines        => $outdent_long_lines,
                rvertical_tightness_flags => $rvertical_tightness_flags,
                level                     => $level
            );

            return;
        }
    }
    else {
        $zero_count = 0;
    }

    # programming check: (shouldn't happen)
    # an error here implies an incorrect call was made
    if ( @{$rfields} && ( @{$rtokens} != ( @{$rfields} - 1 ) ) ) {
        my $nt = @{$rtokens};
        my $nf = @{$rfields};
        warning(
"Program bug in Perl::Tidy::VerticalAligner - number of tokens = $nt should be one less than number of fields: $nf)\n"
        );
        report_definite_bug();
    }
    my $maximum_line_length_for_level = maximum_line_length_for_level($level);

    # --------------------------------------------------------------------
    # create an object to hold this line
    # --------------------------------------------------------------------
    my $new_line = Perl::Tidy::VerticalAligner::Line->new(
        jmax                      => $jmax,
        jmax_original_line        => $jmax,
        rtokens                   => $rtokens,
        rfields                   => $rfields,
        rpatterns                 => $rpatterns,
        rfield_lengths            => $rfield_lengths,
        indentation               => $indentation,
        leading_space_count       => $leading_space_count,
        outdent_long_lines        => $outdent_long_lines,
        list_type                 => "",
        is_hanging_side_comment   => $is_hanging_side_comment,
        maximum_line_length       => $maximum_line_length_for_level,
        rvertical_tightness_flags => $rvertical_tightness_flags,
        is_terminal_ternary       => $is_terminal_ternary,
        j_terminal_match          => $j_terminal_match,
        is_forced_break           => $is_forced_break,
    );

    # --------------------------------------------------------------------
    # It simplifies things to create a zero length side comment
    # if none exists.
    # --------------------------------------------------------------------
    make_side_comment( $new_line, $level_end );

    # --------------------------------------------------------------------
    # Decide if this is a simple list of items.
    # There are 3 list types: none, comma, comma-arrow.
    # We use this below to be less restrictive in deciding what to align.
    # --------------------------------------------------------------------
    if ($is_forced_break) {
        decide_if_list($new_line);
    }

    # --------------------------------------------------------------------
    # Append this line to the current group (or start new group)
    # --------------------------------------------------------------------
    if ( !@group_lines ) {
        add_to_group($new_line);
    }
    else {
        push_group_line($new_line);
    }

    # output this group if it ends in a terminal else or ternary line
    if ( defined($j_terminal_match) ) {
        my_flush();
    }

    # Force break after jump to lower level
    if ( $level_jump < 0 ) {
        my_flush();
    }

    # --------------------------------------------------------------------
    # Some old debugging stuff
    # --------------------------------------------------------------------
    VALIGN_DEBUG_FLAG_APPEND && do {
        print STDOUT "APPEND fields:";
        dump_array( @{$rfields} );
        print STDOUT "APPEND tokens:";
        dump_array( @{$rtokens} );
        print STDOUT "APPEND patterns:";
        dump_array( @{$rpatterns} );
        dump_alignments();
    };

    return;
}

sub join_hanging_comment {

    my $line = shift;
    my $jmax = $line->get_jmax();
    return 0 unless $jmax == 1;                  # must be 2 fields
    my $rtokens = $line->get_rtokens();
    return 0 unless $rtokens->[0] eq '#';    # the second field is a comment..
    my $rfields = $line->get_rfields();
    return 0 unless $rfields->[0] =~ /^\s*$/;    # the first field is empty...
    my $old_line            = shift;
    my $maximum_field_index = $old_line->get_jmax();
    return 0
      unless $maximum_field_index > $jmax;    # the current line has more fields
    my $rpatterns      = $line->get_rpatterns();
    my $rfield_lengths = $line->get_rfield_lengths();

    $line->set_is_hanging_side_comment(1);
    $jmax = $maximum_field_index;
    $line->set_jmax($jmax);
    $rfields->[$jmax]         = $rfields->[1];
    $rfield_lengths->[$jmax]  = $rfield_lengths->[1];
    $rtokens->[ $jmax - 1 ]   = $rtokens->[0];
    $rpatterns->[ $jmax - 1 ] = $rpatterns->[0];
    foreach my $j ( 1 .. $jmax - 1 ) {
        $rfields->[$j]         = " "; # NOTE: caused glitch unless 1 blank, why?
        $rfield_lengths->[$j]  = 1;
        $rtokens->[ $j - 1 ]   = "";
        $rpatterns->[ $j - 1 ] = "";
    }
    return 1;
}

# create an empty side comment if none exists
sub make_side_comment {
    my ( $new_line, $level_end ) = @_;
    my $jmax    = $new_line->get_jmax();
    my $rtokens = $new_line->get_rtokens();

    # if line does not have a side comment...
    if ( ( $jmax == 0 ) || ( $rtokens->[ $jmax - 1 ] ne '#' ) ) {
        my $rfields        = $new_line->get_rfields();
        my $rfield_lengths = $new_line->get_rfield_lengths();
        my $rpatterns      = $new_line->get_rpatterns();
        $jmax += 1;
        $rtokens->[ $jmax - 1 ]  = '#';
        $rfields->[$jmax]        = '';
        $rfield_lengths->[$jmax] = 0;
        $rpatterns->[$jmax]      = '#';
        $new_line->set_jmax($jmax);
        $new_line->set_jmax_original_line($jmax);
    }

    # line has a side comment..
    else {

        # don't remember old side comment location for very long
        my $line_number = $vertical_aligner_self->get_output_line_number();
        my $rfields     = $new_line->get_rfields();
        if (
            $line_number - $last_side_comment_line_number > 12

            # and don't remember comment location across block level changes
            || (   $level_end < $last_side_comment_level
                && $rfields->[0] =~ /^}/ )
          )
        {
            forget_side_comment();
        }
        $last_side_comment_line_number = $line_number;
        $last_side_comment_level       = $level_end;
    }
    return;
}

sub decide_if_list {

    my $line = shift;

    # A list will be taken to be a line with a forced break in which all
    # of the field separators are commas or comma-arrows (except for the
    # trailing #)

    # List separator tokens are things like ',3'   or '=>2',
    # where the trailing digit is the nesting depth.  Allow braces
    # to allow nested list items.
    my $rtokens    = $line->get_rtokens();
    my $test_token = $rtokens->[0];
    if ( $test_token =~ /^(\,|=>)/ ) {
        my $list_type = $test_token;
        my $jmax      = $line->get_jmax();

        foreach ( 1 .. $jmax - 2 ) {
            if ( $rtokens->[$_] !~ /^(\,|=>)/ ) {
                $list_type = "";
                last;
            }
        }
        $line->set_list_type($list_type);
    }
    return;
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
    # returns 1 if the terminal item should be indented

    my ( $old_line, $rfields, $rtokens, $rpatterns, $rfield_lengths ) = @_;
    return unless ($old_line);

## FUTURE CODING
##     my ( $old_line, $end_line ) = @_;
##     return unless ( $old_line && $end_line );
##
##     my $rfields   = $end_line->get_rfields();
##     my $rpatterns = $end_line->get_rpatterns();
##     my $rtokens   = $end_line->get_rtokens();

    my $jmax        = @{$rfields} - 1;
    my $rfields_old = $old_line->get_rfields();

    my $rpatterns_old       = $old_line->get_rpatterns();
    my $rtokens_old         = $old_line->get_rtokens();
    my $maximum_field_index = $old_line->get_jmax();

    # look for the question mark after the :
    my ($jquestion);
    my $depth_question;
    my $pad        = "";
    my $pad_length = 0;
    foreach my $j ( 0 .. $maximum_field_index - 1 ) {
        my $tok = $rtokens_old->[$j];
        if ( $tok =~ /^\?(\d+)$/ ) {
            $depth_question = $1;

            # depth must be correct
            next unless ( $depth_question eq $group_level );

            $jquestion = $j;
            if ( $rfields_old->[ $j + 1 ] =~ /^(\?\s*)/ ) {
                $pad_length = length($1);
                $pad        = " " x $pad_length;
            }
            else {
                return;    # shouldn't happen
            }
            last;
        }
    }
    return unless ( defined($jquestion) );    # shouldn't happen

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

    VALIGN_DEBUG_FLAG_TERNARY && do {
        local $" = '><';
        print STDOUT "CURRENT FIELDS=<@{$rfields_old}>\n";
        print STDOUT "CURRENT TOKENS=<@{$rtokens_old}>\n";
        print STDOUT "CURRENT PATTERNS=<@{$rpatterns_old}>\n";
        print STDOUT "UNMODIFIED FIELDS=<@{$rfields}>\n";
        print STDOUT "UNMODIFIED TOKENS=<@{$rtokens}>\n";
        print STDOUT "UNMODIFIED PATTERNS=<@{$rpatterns}>\n";
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
            return unless ( $patterns[0] =~ s/^\:/?/ );

            # install leading tokens and patterns of existing line
            unshift( @tokens,   @{$rtokens_old}[ 0 .. $jquestion ] );
            unshift( @patterns, @{$rpatterns_old}[ 0 .. $jquestion ] );

            # insert appropriate number of empty fields
            splice( @fields,        1, 0, ('') x $jadd ) if $jadd;
            splice( @field_lengths, 1, 0, (0) x $jadd )  if $jadd;
        }

        # handle sub-case of first field just equal to leading colon.
        # This can happen for example in the example below where
        # the leading '(' would create a new alignment token
        # : ( $name =~ /[]}]$/ ) ? ( $mname = $name )
        # :                        ( $mname = $name . '->' );
        else {

            return unless ( $jmax > 0 && $tokens[0] ne '#' ); # shouldn't happen

            # prepend a leading ? onto the second pattern
            $patterns[1] = "?b" . $patterns[1];

            # pad the second field
            $fields[1]        = $pad . $fields[1];
            $field_lengths[1] = $pad_length + $field_lengths[1];

            # install leading tokens and patterns of existing line, replacing
            # leading token and inserting appropriate number of empty fields
            splice( @tokens,   0, 1, @{$rtokens_old}[ 0 .. $jquestion ] );
            splice( @patterns, 1, 0, @{$rpatterns_old}[ 1 .. $jquestion ] );
            splice( @fields,        1, 0, ('') x $jadd ) if $jadd;
            splice( @field_lengths, 1, 0, (0) x $jadd )  if $jadd;
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
        splice( @fields,        0, 0, ('') x $jadd ) if $jadd;
        splice( @field_lengths, 0, 0, (0) x $jadd )  if $jadd;
    }

    VALIGN_DEBUG_FLAG_TERNARY && do {
        local $" = '><';
        print STDOUT "MODIFIED TOKENS=<@tokens>\n";
        print STDOUT "MODIFIED PATTERNS=<@patterns>\n";
        print STDOUT "MODIFIED FIELDS=<@fields>\n";
    };

    # all ok .. update the arrays
    @{$rfields}        = @fields;
    @{$rtokens}        = @tokens;
    @{$rpatterns}      = @patterns;
    @{$rfield_lengths} = @field_lengths;
## FUTURE CODING
##     $end_line->set_rfields( \@fields );
##     $end_line->set_rtokens( \@tokens );
##     $end_line->set_rpatterns( \@patterns );

    # force a flush after this line
    return $jquestion;
}

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
    my ( $old_line, $rfields, $rtokens, $rpatterns, $rfield_lengths ) = @_;
    return unless ($old_line);
    my $jmax = @{$rfields} - 1;
    return unless ( $jmax > 0 );

    # check for balanced else block following if/elsif/unless
    my $rfields_old = $old_line->get_rfields();

    # TBD: add handling for 'case'
    return unless ( $rfields_old->[0] =~ /^(if|elsif|unless)\s*$/ );

    # look for the opening brace after the else, and extract the depth
    my $tok_brace = $rtokens->[0];
    my $depth_brace;
    if ( $tok_brace =~ /^\{(\d+)/ ) { $depth_brace = $1; }

    # probably:  "else # side_comment"
    else { return }

    my $rpatterns_old       = $old_line->get_rpatterns();
    my $rtokens_old         = $old_line->get_rtokens();
    my $maximum_field_index = $old_line->get_jmax();

    # be sure the previous if/elsif is followed by an opening paren
    my $jparen    = 0;
    my $tok_paren = '(' . $depth_brace;
    my $tok_test  = $rtokens_old->[$jparen];
    return unless ( $tok_test eq $tok_paren );    # shouldn't happen

    # Now find the opening block brace
    my ($jbrace);
    foreach my $j ( 1 .. $maximum_field_index - 1 ) {
        my $tok = $rtokens_old->[$j];
        if ( $tok eq $tok_brace ) {
            $jbrace = $j;
            last;
        }
    }
    return unless ( defined($jbrace) );           # shouldn't happen

    # Now splice the tokens and patterns of the previous line
    # into the else line to insure a match.  Add empty fields
    # as necessary.
    my $jadd = $jbrace - $jparen;
    splice( @{$rtokens},   0, 0, @{$rtokens_old}[ $jparen .. $jbrace - 1 ] );
    splice( @{$rpatterns}, 1, 0, @{$rpatterns_old}[ $jparen + 1 .. $jbrace ] );
    splice( @{$rfields},        1, 0, ('') x $jadd );
    splice( @{$rfield_lengths}, 1, 0, (0) x $jadd );

    # force a flush after this line if it does not follow a case
    if   ( $rfields_old->[0] =~ /^case\s*$/ ) { return }
    else                                      { return $jbrace }
}

{    # sub check_match
    my %is_good_alignment;
    my $EXPLAIN;

    BEGIN {

        # Vertically aligning on certain "good" tokens is usually okay
        # so we can be less restrictive in marginal cases.
        my @q = qw( { ? => = );
        push @q, (',');
        @is_good_alignment{@q} = (1) x scalar(@q);

        $EXPLAIN = 0;
    }

    sub check_match {

        # See if the current line matches the current vertical alignment group.
        # If not, flush the current group.
        my ( $new_line, $old_line ) = @_;

        # uses global variables:
        #  $previous_minimum_jmax_seen
        #  $maximum_jmax_seen
        #  $marginal_match
        my $jmax                = $new_line->get_jmax();
        my $maximum_field_index = $old_line->get_jmax();

        # Variable $imax_align will be set to indicate the maximum token index
        # to be matched in the left-to-right sweep, in the case that this line
        # does not exactly match the current group.
        my $imax_align = -1;

        # variable $GoToLoc explains reason for no match, for debugging
        my $GoToLoc = "";

        my $jmax_original_line      = $new_line->get_jmax_original_line();
        my $is_hanging_side_comment = $new_line->get_is_hanging_side_comment();
        my $rtokens                 = $new_line->get_rtokens();
        my $rfields                 = $new_line->get_rfields();
        my $rfield_lengths          = $new_line->get_rfield_lengths();
        my $rpatterns               = $new_line->get_rpatterns();
        my $list_type               = $new_line->get_list_type();

        my $group_list_type = $old_line->get_list_type();
        my $old_rpatterns   = $old_line->get_rpatterns();
        my $old_rtokens     = $old_line->get_rtokens();

        my $jlimit = $jmax - 2;
        if ( $jmax > $maximum_field_index ) {
            $jlimit = $maximum_field_index - 2;
        }

        # handle comma-separated lists ..
        if ( $group_list_type && ( $list_type eq $group_list_type ) ) {
            for my $j ( 0 .. $jlimit ) {
                my $old_tok = $old_rtokens->[$j];
                my $new_tok = $rtokens->[$j];
                $GoToLoc = "different tokens: $old_tok ne $new_tok";
                goto NO_MATCH if ( $old_tok ne $new_tok );
                $imax_align = $j;
            }
        }

        # do detailed check for everything else except hanging side comments
        elsif ( !$is_hanging_side_comment ) {

            my $leading_space_count = $new_line->get_leading_space_count();

            my $max_pad = 0;
            my $min_pad = 0;
            my $saw_good_alignment;

            for my $j ( 0 .. $jlimit ) {

                my $old_tok = $old_rtokens->[$j];
                my $new_tok = $rtokens->[$j];

                # Note on encoding used for alignment tokens:
                # -------------------------------------------
                # Tokens are "decorated" with information which can help
                # prevent unwanted alignments.  Consider for example the
                # following two lines:
                #   local ( $xn, $xd ) = split( '/', &'rnorm(@_) );
                #   local ( $i, $f ) = &'bdiv( $xn, $xd );
                # There are three alignment tokens in each line, a comma,
                # an =, and a comma.  In the first line these three tokens
                # are encoded as:
                #    ,4+local-18     =3      ,4+split-7
                # and in the second line they are encoded as
                #    ,4+local-18     =3      ,4+&'bdiv-8
                # Tokens always at least have token name and nesting
                # depth.  So in this example the ='s are at depth 3 and
                # the ,'s are at depth 4.  This prevents aligning tokens
                # of different depths.  Commas contain additional
                # information, as follows:
                # ,  {depth} + {container name} - {spaces to opening paren}
                # This allows us to reject matching the rightmost commas
                # in the above two lines, since they are for different
                # function calls.  This encoding is done in
                # 'sub send_lines_to_vertical_aligner'.

                # Pick off actual token.
                # Everything up to the first digit is the actual token.

                my ( $alignment_token, $lev, $tag, $tok_count ) =
                  decode_alignment_token($new_tok);

                # see if the decorated tokens match
                my $tokens_match = $new_tok eq $old_tok

                  # Exception for matching terminal : of ternary statement..
                  # consider containers prefixed by ? and : a match
                  || ( $new_tok =~ /^,\d*\+\:/ && $old_tok =~ /^,\d*\+\?/ );

                # No match if the alignment tokens differ...
                if ( !$tokens_match ) {
                    $GoToLoc = "tokens differ: $new_tok ne $old_tok";
                    goto NO_MATCH;
                }

                # Calculate amount of padding required to fit this in.
                # $pad is the number of spaces by which we must increase
                # the current field to squeeze in this field.
                my $pad =
                  $rfield_lengths->[$j] - $old_line->current_field_width($j);
                if ( $j == 0 ) { $pad += $leading_space_count; }

                # remember max pads to limit marginal cases
                if ( $alignment_token ne '#' ) {
                    if ( $pad > $max_pad ) { $max_pad = $pad }
                    if ( $pad < $min_pad ) { $min_pad = $pad }
                }
                if ( $is_good_alignment{$alignment_token} ) {
                    $saw_good_alignment = 1;
                }

                # If patterns don't match, we have to be careful...
                if ( $old_rpatterns->[$j] ne $rpatterns->[$j] ) {

                    # flag this as a marginal match since patterns differ
                    $marginal_match = 1
                      if ( $marginal_match == 0 && @group_lines == 1 );

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
                        $GoToLoc = "do not align commas in unnamed containers";
                        goto NO_MATCH unless ( $new_tok =~ /[A-Za-z]/ );
                    }

                    # do not align parens unless patterns match;
                    # large ugly spaces can occur in math expressions.
                    elsif ( $alignment_token eq '(' ) {

                        # But we can allow a match if the parens don't
                        # require any padding.
                        $GoToLoc = "do not align '(' unless patterns match";
                        if ( $pad != 0 ) { goto NO_MATCH }
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
                        if (
                            substr( $old_rpatterns->[$j], 0, 1 ) ne
                            substr( $rpatterns->[$j],     0, 1 ) )
                        {
                            $GoToLoc = "first character before equals differ";
                            goto NO_MATCH;
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
                        elsif (
                            ( index( $old_rpatterns->[$j], ',' ) >= 0 ) ne
                            ( index( $rpatterns->[$j],     ',' ) >= 0 ) )
                        {
                            $imax_align = -1;
                            $GoToLoc = "mixed commas/no-commas before equals";
                            goto NO_MATCH;
                        }

                        # If we pass that test, we'll call it a marginal match.
                        # Here is an example of a marginal match:
                        #       $done{$$op} = 1;
                        #       $op         = compile_bblock($op);
                        # The left tokens are both identifiers, but
                        # one accesses a hash and the other doesn't.
                        # We'll let this be a tentative match and undo
                        # it later if we don't find more than 2 lines
                        # in the group.
                        elsif ( @group_lines == 1 ) {
                            $marginal_match =
                              2;    # =2 prevents being undone below
                        }
                    }
                }

                # Everything matches so far, so we can update the maximum index
                # for partial alignment.
                $imax_align = $j;

            } ## end for my $j ( 0 .. $jlimit)

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
            if (   $marginal_match == 1
                && $jmax == $maximum_field_index
                && ( $saw_good_alignment || ( $max_pad < 3 && $min_pad > -3 ) )
              )
            {
                $marginal_match = 0;
            }

            ##print "marginal=$marginal_match saw=$saw_good_alignment jmax=$jmax max=$maximum_field_index maxpad=$max_pad minpad=$min_pad\n";
        }

        # The tokens match, but the lines must have identical number of
        # tokens to join the group.
        if ( $maximum_field_index != $jmax ) {
            $GoToLoc = "token count differs";
            goto NO_MATCH;
        }

        $EXPLAIN && print "match, imax_align=$imax_align, jmax=$jmax\n";

        # The tokens match. Now See if there is space for this line in the
        # current group.
        check_fit( $new_line, $old_line, $jlimit );

        return;

      NO_MATCH:

        # variable $GoToLoc is for debugging
        $EXPLAIN && print "no match because $GoToLoc, flag=$imax_align\n";

        end_rgroup($imax_align);
        return;
    }
}

sub check_fit {

    my ( $new_line, $old_line, $imax_align ) = @_;
    return unless (@group_lines);

    # The new line has alignments identical to the current group. Now we have
    # to see if the new line can fit into the group without causing a field
    # to exceed the line length limit.  If it cannot, we will end the current
    # group and start a new one.

    my $jmax                = $new_line->get_jmax();
    my $leading_space_count = $new_line->get_leading_space_count();
    my $rfield_lengths      = $new_line->get_rfield_lengths();

    my $group_list_type = $group_lines[0]->get_list_type();

    my $padding_so_far    = 0;
    my $padding_available = $old_line->get_available_space_on_right();

    # Save current columns in case this line does not fit.
    save_alignment_columns();

    # Loop over all alignments ...
    my $maximum_field_index = $old_line->get_jmax();
    for my $j ( 0 .. $jmax ) {

        my $pad = $rfield_lengths->[$j] - $old_line->current_field_width($j);

        if ( $j == 0 ) {
            $pad += $leading_space_count;
        }

        # Remember largest gap of the group, excluding gap to side comment.
        if (   $pad < 0
            && $group_maximum_gap < -$pad
            && $j > 0
            && $j < $jmax - 1 )
        {
            $group_maximum_gap = -$pad;
        }

        # Keep going if this field does not need any space.
        next if $pad < 0;

        # See if it needs too much space.
        if ( $pad > $padding_available ) {

            # Not enough room for it; revert to starting state then flush.
            restore_alignment_columns();
            end_rgroup($imax_align);
            last;
        }

        # This line fits, squeeze it in.
        $old_line->increase_field_width( $j, $pad );
        $padding_available -= $pad;

        # remember largest gap of the group, excluding gap to side comment
        if ( $pad > $group_maximum_gap && $j > 0 && $j < $jmax - 1 ) {
            $group_maximum_gap = $pad;
        }
    }
    return;
}

sub add_to_group {

    # The current line either starts a new alignment group or is
    # accepted into the current alignment group.
    my ($new_line) = @_;
    push_group_line($new_line);

    # initialize field lengths if starting new group
    if ( @group_lines == 1 ) {

        my $jmax           = $new_line->get_jmax();
        my $rfields        = $new_line->get_rfields();
        my $rfield_lengths = $new_line->get_rfield_lengths();
        my $rtokens        = $new_line->get_rtokens();
        my $col            = $new_line->get_leading_space_count();

        for my $j ( 0 .. $jmax ) {
            $col += $rfield_lengths->[$j];

            # create initial alignments for the new group
            my $token = "";
            if ( $j < $jmax ) { $token = $rtokens->[$j] }
            my $alignment = make_alignment( $col, $token );
            $new_line->set_alignment( $j, $alignment );
        }

        $maximum_jmax_seen = $jmax;
        $minimum_jmax_seen = $jmax;
    }

    # use previous alignments otherwise
    else {
        my @new_alignments = $group_lines[-2]->get_alignments();
        $new_line->set_alignments(@new_alignments);
    }

    # remember group jmax extremes for next call to valign_input
    $previous_minimum_jmax_seen = $minimum_jmax_seen;
    $previous_maximum_jmax_seen = $maximum_jmax_seen;
    return;
}

sub dump_array {

    # debug routine to dump array contents
    local $" = ')(';
    print STDOUT "(@_)\n";
    return;
}

# flush() sends the current Perl::Tidy::VerticalAligner group down the
# pipeline to Perl::Tidy::FileWriter.

# This is the external flush, which also empties the buffer and cache
sub flush {

    # the buffer must be emptied first, then any cached text
    dump_valign_buffer();

    if (@group_lines) {
        my_flush();
    }
    else {
        if ($cached_line_type) {
            $seqno_string = $cached_seqno_string;
            valign_output_step_C( $cached_line_text,
                $cached_line_leading_space_count,
                $last_level_written );
            $cached_line_type        = 0;
            $cached_line_text        = "";
            $cached_line_text_length = 0;
            $cached_seqno_string     = "";
        }
    }
    return;
}

sub reduce_valign_buffer_indentation {

    my ($diff) = @_;
    if ( $valign_buffer_filling && $diff ) {
        my $max_valign_buffer = @valign_buffer;
        foreach my $i ( 0 .. $max_valign_buffer - 1 ) {
            my ( $line, $leading_space_count, $level ) =
              @{ $valign_buffer[$i] };
            my $ws = substr( $line, 0, $diff );
            if ( ( length($ws) == $diff ) && $ws =~ /^\s+$/ ) {
                $line = substr( $line, $diff );
            }
            if ( $leading_space_count >= $diff ) {
                $leading_space_count -= $diff;
                $level = level_change( $leading_space_count, $diff, $level );
            }
            $valign_buffer[$i] = [ $line, $leading_space_count, $level ];
        }
    }
    return;
}

sub level_change {

    # compute decrease in level when we remove $diff spaces from the
    # leading spaces
    my ( $leading_space_count, $diff, $level ) = @_;
    if ($rOpts_indent_columns) {
        my $olev =
          int( ( $leading_space_count + $diff ) / $rOpts_indent_columns );
        my $nlev = int( $leading_space_count / $rOpts_indent_columns );
        $level -= ( $olev - $nlev );
        if ( $level < 0 ) { $level = 0 }
    }
    return $level;
}

sub dump_valign_buffer {
    if (@valign_buffer) {
        foreach (@valign_buffer) {
            valign_output_step_D( @{$_} );
        }
        @valign_buffer = ();
    }
    $valign_buffer_filling = "";
    return;
}

sub my_flush_comment {

    # Output a group of COMMENT lines

    return unless (@group_lines);
    my $leading_space_count = $comment_leading_space_count;
    my $leading_string      = get_leading_string($leading_space_count);

    # look for excessively long lines
    my $max_excess = 0;
    foreach my $item (@group_lines) {
        my ( $str, $str_len ) = @{$item};
        my $excess =
          $str_len +
          $leading_space_count -
          maximum_line_length_for_level($group_level);
        if ( $excess > $max_excess ) {
            $max_excess = $excess;
        }
    }

    # zero leading space count if any lines are too long
    if ( $max_excess > 0 ) {
        $leading_space_count -= $max_excess;
        if ( $leading_space_count < 0 ) { $leading_space_count = 0 }
        $last_outdented_line_at = $file_writer_object->get_output_line_number();
        unless ($outdented_line_count) {
            $first_outdented_line_at = $last_outdented_line_at;
        }
        my $nlines = @group_lines;
        $outdented_line_count += $nlines;
    }

    # write the lines
    my $outdent_long_lines = 0;

    foreach my $item (@group_lines) {
        my ( $line, $line_len ) = @{$item};
        valign_output_step_B(
            leading_space_count       => $leading_space_count,
            line                      => $line,
            line_length               => $line_len,
            side_comment_length       => 0,
            outdent_long_lines        => $outdent_long_lines,
            rvertical_tightness_flags => "",
            level                     => $group_level,
        );
    }

    initialize_for_new_group();
    return;
}

sub my_flush {

    # This is the vertical aligner internal flush, which leaves the cache
    # intact
    return unless (@group_lines);

    # Debug
    0 && do {
        my ( $a, $b, $c ) = caller();
        my $nlines = @group_lines;
        print STDOUT
"APPEND0: my_flush called from $a $b $c lines=$nlines, type=$group_type \n";
    };

    # handle a group of COMMENT lines
    if ( $group_type eq 'COMMENT' ) { my_flush_comment() }

    # Output a single line of CODE
    elsif ( @group_lines == 1 ) {
        adjust_side_comment_single_group();
        my $extra_leading_spaces = get_extra_leading_spaces();
        my $line                 = $group_lines[0];
        my $group_leader_length  = $line->get_leading_space_count();
        valign_output_step_A(
            line                 => $line,
            min_ci_gap           => 0,
            do_not_align         => 0,
            group_leader_length  => $group_leader_length,
            extra_leading_spaces => $extra_leading_spaces
        );
        initialize_for_new_group();
    }

    # Handle vertical alignment of multiple lines of CODE lines.  Most of
    # the work of vertical aligning happens here.
    else {

        # we will rebuild alignment line group(s);
        my @all_lines = @group_lines;
        initialize_for_new_group();

        # STEP 1: Remove most unmatched tokens. They block good alignments.
        delete_unmatched_tokens( \@all_lines );

        # STEP 2: Construct a tree of matched lines and delete some small deeper
        # levels of tokens.  They also block good alignments.
        my ( $rgroup_id, $rgroup_index ) = prune_alignment_tree( \@all_lines );

        # STEP 3: Sweep top to bottom, forming groups of lines with exactly
        # matching common alignments.
        my $rgroups =
          sweep_top_to_bottom( \@all_lines, $rgroup_id, $rgroup_index );

        # STEP 4: Sweep left to right through these groups, looking for
        # leading alignment tokens shared by groups.
        sweep_left_to_right( \@all_lines, $rgroups );

        # STEP 5: Move side comments to a common column if possible.
        adjust_side_comment_multiple_groups( \@all_lines, $rgroups );

        # STEP 6: For the -lp option, increase the indentation of lists
        # to the desired amount, but do not exceed the line length limit.
        my $extra_leading_spaces =
          get_extra_leading_spaces_multiple_groups( \@all_lines, $rgroups );

        # STEP 7: Output the lines.
        # All lines in this batch have the same basic leading spacing:
        my $group_leader_length = $all_lines[0]->get_leading_space_count();

        foreach my $line (@all_lines) {
            valign_output_step_A(
                line                 => $line,
                min_ci_gap           => 0,
                do_not_align         => 0,
                group_leader_length  => $group_leader_length,
                extra_leading_spaces => $extra_leading_spaces
            );
        }
        initialize_for_new_group();
    } ## end handling of multiple lines
    return;
}

{    # rgroups

    # The variable $rgroups will hold the partition of all lines in this output
    # batch into groups with common alignments.

    my $rgroups;
    BEGIN { $rgroups = [] }

    sub initialize_rgroups {
        $rgroups = [];
        return;
    }

    sub get_rgroups {
        return $rgroups;
    }

    sub add_to_rgroup {
        my ( $rline, $jend ) = @_;

        add_to_group($rline);

        # A line has just been added to @group_lines, so we include it
        # in the current subgroup, or start a new one.
        # There will be 1 line in @group_lines when a new subgroup starts
        my $jbeg   = $jend;
        my $nlines = @group_lines;
        if ( $nlines > 1 ) {
            my $rvals = pop @{$rgroups};
            $jbeg = $rvals->[0];
        }
        push @{$rgroups}, [ $jbeg, $jend, undef ];
        return;
    }

    sub end_rgroup {

        my ($imax_align) = @_;
        return unless @{$rgroups};
        return unless @group_lines;

        # Undo alignment of some poor two-line combinations.
        # We had to wait until now to know the line count.
        decide_if_aligned_pair($imax_align);

        $rgroups->[-1]->[2] = $imax_align;

        initialize_for_new_group();
        return;
    }
}

sub sweep_top_to_bottom {
    my ( $rlines, $rgroup_id, $rgroup_index ) = @_;
    my $jline = -1;

    # Partition the set of lines into final alignment subgroups
    # and store the alignments with the lines.
    initialize_rgroups();
    $is_matching_terminal_line = 0;
    return unless @{$rlines};    # shouldn't happen

    my $keep_group_intact = $rOpts->{'line-up-parentheses'} && $extra_indent_ok;

    # Setting the _end_group flag for the last line causes problems for -lp
    # formatting, so we unset it.
    $rlines->[-1]->{_end_group} = 0;

    # Loop over all lines ...
    foreach my $new_line ( @{$rlines} ) {
        $jline++;

        # Start a new subgroup if necessary
        if ( !@group_lines ) {
            add_to_rgroup( $new_line, $jline );
            if ( $new_line->{_end_group} ) {
                end_rgroup(-1);
            }
            next;
        }

        my $j_terminal_match = $new_line->get_j_terminal_match();
        my $base_line        = $group_lines[0];

        # Initialize a global flag saying if the last line of the group
        # should match end of group and also terminate the group.  There
        # should be no returns between here and where the flag is handled
        # at the bottom.
        my $col_matching_terminal = 0;
        if ( defined($j_terminal_match) ) {

            # remember the column of the terminal ? or { to match with
            $col_matching_terminal = $base_line->get_column($j_terminal_match);

            # set global flag for sub decide_if_aligned_pair
            $is_matching_terminal_line = 1;
        }

        # -------------------------------------------------------------
        # Allow hanging side comment to join current group, if any. This
        # will help keep side comments aligned, because otherwise we
        # will have to start a new group, making alignment less likely.
        # -------------------------------------------------------------
        if ( $new_line->get_is_hanging_side_comment() ) {
            join_hanging_comment( $new_line, $base_line );
        }

        # If this line has no matching tokens, then flush out the lines
        # BEFORE this line unless both it and the previous line have side
        # comments.  This prevents this line from pushing side coments out
        # to the right.
        elsif ( $new_line->get_jmax() == 1 && !$keep_group_intact ) {

            # There are no matching tokens, so now check side comments.
            # Programming note: accessing arrays with index -1 is
            # risky in Perl, but we have verified there is at least one
            # line in the group and that there is at least one field.
            my $prev_comment = $group_lines[-1]->get_rfields()->[-1];
            my $side_comment = $new_line->get_rfields()->[-1];
            end_rgroup(-1) unless ( $side_comment && $prev_comment );
        }

        # See if the new line matches and fits the current group.
        # Flush the current group if not.
        check_match( $new_line, $base_line );

        # Store the new line
        add_to_rgroup( $new_line, $jline );

        if ( defined($j_terminal_match) ) {

            # if there is only one line in the group (maybe due to failure
            # to match perfectly with previous lines), then align the ? or
            # { of this terminal line with the previous one unless that
            # would make the line too long
            if ( @group_lines == 1 ) {
                $base_line = $group_lines[0];
                my $col_now = $base_line->get_column($j_terminal_match);
                my $pad     = $col_matching_terminal - $col_now;
                my $padding_available =
                  $base_line->get_available_space_on_right();
                if ( $pad > 0 && $pad <= $padding_available ) {
                    $base_line->increase_field_width( $j_terminal_match, $pad );
                }
            }
            end_rgroup(-1);
            $is_matching_terminal_line = 0;
        }

        # end the group if we know we cannot match next line.
        elsif ( $new_line->{_end_group} ) {
            end_rgroup(-1);
        }
    } ## end loop over lines
    end_rgroup(-1);
    my $rgroups = get_rgroups();
    return ($rgroups);
}

sub sweep_left_to_right {

    my ( $rlines, $rgroups ) = @_;

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
    return unless ( $ng_max > 0 );

    ############################################################################
    # Step 1: Loop over groups to find all common leading alignment tokens
    ############################################################################

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
        $rtokens = $line->get_rtokens();
        $imax    = $line->get_jmax() - 2;
        $istop   = -1 unless ( defined($istop) );
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
        # unless they form a simple list.  The alignment in this case can look
        # strange in some cases.
        if (   $jend == $jbeg
            && $jend_m == $jbeg_m
            && !$rlines->[$jbeg]->get_list_type()
            && ( $ng == 1 || $istop_mm < 0 )
            && ( $ng == $ng_max || $istop < 0 ) )
        {

            # We will just align a leading equals
            next unless ( $imax_min >= 0 && $rtokens->[0] =~ /^=\d/ );

            # In this case we will limit padding to one indent distance.  This
            # is a compromise to keep some vertical alignment but prevent large
            # gaps, which do not look good for just two lines.
            my $ng_m = $ng - 1;
            $max_move{"$ng_m"} = $short_pad;
            $max_move{"$ng"}   = $short_pad;
        }

        # Loop to find all common leading tokens.
        if ( $imax_min >= 0 ) {
            foreach my $i ( 0 .. $imax_min ) {
                my $tok   = $rtokens->[$i];
                my $tok_m = $rtokens_m->[$i];
                last if ( $tok ne $tok_m );
                push @icommon, [ $i, $ng ];
            }
        }
    }
    return unless @icommon;

    ###########################################################
    # Step 2: Reorder and consolidate the list into a task list
    ###########################################################

    # We have to work first from lowest token index to highest, then by group,
    # sort our list first on token index then group number
    @icommon = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @icommon;

    # Make a task list of the form
    #   [$i, ng_beg, $ng_end], ..
    # where
    #   $i is the index of the token to be aligned
    #   $ng_beg..$ng_end is the group range for this action
    my @todo;
    my ( $i, $ng_end );
    foreach my $item (@icommon) {
        my $ng_last = $ng_end;
        my $i_last  = $i;
        ( $i, $ng_end ) = @{$item};
        my $ng_beg = $ng_end - 1;
        if ( defined($ng_last) && $ng_beg == $ng_last && $i == $i_last ) {
            my $var = pop(@todo);
            $ng_beg = $var->[1];
        }
        push @todo, [ $i, $ng_beg, $ng_end ];
    }

    ###############################
    # Step 3: Execute the task list
    ###############################
    do_left_to_right_sweep( $rlines, $rgroups, \@todo, \%max_move, $short_pad );
    return;
}

sub do_left_to_right_sweep {
    my ( $rlines, $rgroups, $rtodo, $rmax_move, $short_pad ) = @_;

    my $move_to_common_column = sub {

        # Move the alignment column of token $itok to $col_want for a sequence
        # of groups.
        my ( $ngb, $nge, $itok, $col_want ) = @_;
        return unless ( defined($ngb) && $nge > $ngb );
        foreach my $ng ( $ngb .. $nge ) {
            my ( $jbeg, $jend ) = @{ $rgroups->[$ng] };
            my $line  = $rlines->[$jbeg];
            my $col   = $line->get_column($itok);
            my $avail = $line->get_available_space_on_right();
            my $move  = $col_want - $col;
            if ( $move > 0 ) {
                next
                  if ( defined( $rmax_move->{$ng} )
                    && $move > $rmax_move->{$ng} );
                $line->increase_field_width( $itok, $move );
            }

            # Note that we continue on even if the move would have been
            # negative.  We could also throw a switch to stop at this point,
            # but if we keep going we may get some additional alignments.
            # So there may be jumps in aligned/non-aligned tokens when
            # we are running out of space, but it does not seem to look
            # any worse than stopping altogether.
        }
    };

    foreach my $task ( @{$rtodo} ) {
        my ( $itok, $ng_beg, $ng_end ) = @{$task};

        # Nothing to do for a single group
        next unless ( $ng_end > $ng_beg );

        my $ng_first;     # index of the first group of a continuous sequence
        my $col_want;     # the common alignment column of a sequence of groups
        my $col_limit;    # maximum column before bumping into max line length
        my $line_count_ng_m = 0;
        my $jmax_m;
        my $istop_m;

        # Loop over the groups
        foreach my $ng ( $ng_beg .. $ng_end ) {
            my ( $jbeg, $jend, $istop ) = @{ $rgroups->[$ng] };
            my $line_count_ng = $jend - $jbeg + 1;

            # Important: note that since all lines in a group have a common
            # alignments object, we just have to work on one of the lines (the
            # first line).  All of the rest will be changed automatically.
            my $line = $rlines->[$jbeg];
            my $jmax = $line->get_jmax();

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
                $istop_m         = $istop;
                next;
            }

            # RULE: prevent a 'tail-wag-dog' syndrom:
            # Do not let one or two lines with a different number of alignments
            # open up a big gap in a large block.  For example, we will prevent
            # something like this, where the first line prys open the rest:

            #  $worksheet->write( "B7", "http://www.perl.com", undef, $format );
            #  $worksheet->write( "C7", "",                    $format );
            #  $worksheet->write( "D7", "",                    $format );
            #  $worksheet->write( "D8", "",                    $format );
            #  $worksheet->write( "D8", "",                    $format );

            # We should exclude from consideration two groups which are
            # effectively the same but separated because one does not
            # fit in the maximum allowed line length.
            my $is_same_group = $jmax == $jmax_m && $istop_m == $jmax_m - 2;
            my $is_big_gap;
            if ( !$is_same_group ) {
                $is_big_gap ||=
                     $line_count_ng >= 4
                  && $line_count_ng_m <= 2
                  && $col_want > $col + $short_pad;
                $is_big_gap ||=
                     $line_count_ng_m >= 4
                  && $line_count_ng <= 2
                  && $col > $col_want + $short_pad;
            }

            # quit and restart if it cannot join this batch
            if ( $col_want > $col_max || $col > $col_limit || $is_big_gap ) {
                $move_to_common_column->( $ng_first, $ng - 1, $itok,
                    $col_want );
                $ng_first        = $ng;
                $col_want        = $col;
                $col_limit       = $col_max;
                $line_count_ng_m = $line_count_ng;
                $jmax_m          = $jmax;
                $istop_m         = $istop;
                next;
            }

            $line_count_ng_m += $line_count_ng;

            # update the common column and limit
            if ( $col > $col_want )      { $col_want  = $col }
            if ( $col_max < $col_limit ) { $col_limit = $col_max }

        } ## end loop over groups

        if ( $ng_end > $ng_first ) {
            $move_to_common_column->( $ng_first, $ng_end, $itok, $col_want );
        } ## end loop over groups for one task
    } ## end loop over tasks

    return;
}

sub delete_selected_tokens {

    my ( $line_obj, $ridel, $new_list_ok ) = @_;

    # $line_obj    is the line to be modified
    # $ridel       is a ref to list of indexes to be deleted
    # $new_list_ok is flag giving permission to convert non-list to list

    # remove an unused alignment token(s) to improve alignment chances

    return unless ( defined($line_obj) && defined($ridel) && @{$ridel} );

    my $jmax_old           = $line_obj->get_jmax();
    my $rfields_old        = $line_obj->get_rfields();
    my $rfield_lengths_old = $line_obj->get_rfield_lengths();
    my $rpatterns_old      = $line_obj->get_rpatterns();
    my $rtokens_old        = $line_obj->get_rtokens();

    my $EXPLAIN = 0;

    local $" = '> <';
    $EXPLAIN && print <<EOM;
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

    my $kmax      = @{$ridel} - 1;
    my $k         = 0;
    my $jdel_next = $ridel->[$k];

    if ( $jdel_next < 0 ) { return }    # shouldnt happen
    my $pattern      = $rpatterns_old->[0];
    my $field        = $rfields_old->[0];
    my $field_length = $rfield_lengths_old->[0];
    push @{$rfields_new},        $field;
    push @{$rfield_lengths_new}, $field_length;
    push @{$rpatterns_new},      $pattern;

    for ( my $j = 0 ; $j < $jmax_old ; $j++ ) {
        my $token        = $rtokens_old->[$j];
        my $field        = $rfields_old->[ $j + 1 ];
        my $field_length = $rfield_lengths_old->[ $j + 1 ];
        my $pattern      = $rpatterns_old->[ $j + 1 ];
        if ( $k > $kmax || $j < $jdel_next ) {
            push @{$rtokens_new},        $token;
            push @{$rfields_new},        $field;
            push @{$rpatterns_new},      $pattern;
            push @{$rfield_lengths_new}, $field_length;
        }
        elsif ( $j == $jdel_next ) {
            $rfields_new->[-1] .= $field;
            $rfield_lengths_new->[-1] += $field_length;
            $rpatterns_new->[-1] .= $pattern;
            if ( ++$k <= $kmax ) {
                my $jdel_last = $jdel_next;
                $jdel_next = $ridel->[$k];
                if ( $jdel_next < $jdel_last ) {

                    # FIXME:
                    print STDERR "bad jdel_next=$jdel_next\n";
                    return;
                }
            }
        }
    }

    # ----- x ------ x ------ x ------
    #t      0        1        2        <- token indexing
    #f   0      1        2        3    <- field and pattern

    my $jmax_new = @{$rfields_new} - 1;
    $line_obj->set_rtokens($rtokens_new);
    $line_obj->set_rpatterns($rpatterns_new);
    $line_obj->set_rfields($rfields_new);
    $line_obj->set_rfield_lengths($rfield_lengths_new);
    $line_obj->set_jmax($jmax_new);

    # update list type based on new leading token
    my $old_list_type = $line_obj->get_list_type();
    my $new_list_type = "";
    if ( $rtokens_new->[0] =~ /^(=>|,)/ ) {
        $new_list_type = $rtokens_new->[0];
    }

    # An existing list will still be a list but with possibly different leading
    # token
    if ($old_list_type) {
        if ( $old_list_type ne $new_list_type ) {
            $line_obj->set_list_type($new_list_type);
        }
    }

    # A non-list line could become a list if all non-list tokens have been
    # deleted. But only do this if the "new_list_ok" flag is set.  The following
    # two-line snippet shows an example of unwanted => alignement which can
    # occur if we promote lines to be lists without permission:
    #  $w1->bin( $xc, $yc,   { Panel => 3 } );
    #  $w1->env( 0, 1, 0, 1, { Axis  => 'Box' } );
    elsif ( $new_list_type && $new_list_ok ) {
        my ( $raw_tok, $lev, $tag, $tok_count ) =
          decode_alignment_token($new_list_type);

        # But for lines with leading commas, we will require that they be
        # tagged before converting a line from non-list to a list.
        if ($tag) {
            for ( my $i = 1 ; $i < @{$rtokens_new} - 1 ; $i++ ) {
                if ( $rtokens_new->[$i] !~ /^(,|=>)/ ) {
                    $new_list_type = "";
                    last;
                }
            }
            $line_obj->set_list_type($new_list_type) if ($new_list_type);
        }
    }

    $EXPLAIN && print <<EOM;

new jmax: $jmax_new
new tokens: <@{$rtokens_new}>
new patterns: <@{$rpatterns_new}>
new fields: <@{$rfields_new}>
EOM
    return;
}

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
    my ( $raw_tok, $lev, $tag, $tok_count ) = ( $tok, 0, "", 1 );
    if ( $tok =~ /^(\D+)(\d+)([^\.]*)(\.(\d+))?$/ ) {
        $raw_tok   = $1;
        $lev       = $2;
        $tag       = $3 if ($3);
        $tok_count = $5 if ($5);
    }
    return ( $raw_tok, $lev, $tag, $tok_count );
}

{    # sub is_deletable_token

    my %is_deletable_equals;

    BEGIN {
        my @q;

        # These tokens with = may be deleted for vertical aligmnemt
        @q = qw(
          <= >= == =~ != <=>
          =>
        );
        @is_deletable_equals{@q} = (1) x scalar(@q);

    }

    sub is_deletable_token {

        # Normally we should allow an isolated token to be deleted because
        # this will improve the chances of getting vertical alignments.
        # But it can be useful not to delete selected tokens in order to
        # prevent some undesirable alignments.
        my ( $token, $i, $imax, $jline, $i_eq ) = @_;

        my ( $raw_tok, $lev, $tag, $tok_count ) =
          decode_alignment_token($token);

        # Always okay to delete second and higher copies of a token
        if ( $tok_count > 1 ) { return 1 }

        # only remove lower level commas
        if ( $raw_tok eq ',' ) {

            # Do not delete commas before an equals
            return if ( defined($i_eq) && $i < $i_eq );

            # Do not delete line-level commas
            return if ( $lev <= $group_level );
        }

        # most operators with an equals sign should be retained if at
        # same level as this statement
        elsif ( $raw_tok =~ /=/ ) {
            return
              unless ( $lev > $group_level || $is_deletable_equals{$raw_tok} );
        }

        # otherwise, ok to delete the token
        return 1;
    }
}

sub delete_unmatched_tokens {
    my ($rlines) = @_;

    # This is a preliminary step in vertical alignment in which we remove as
    # many obviously un-needed alignment tokens as possible.  This will prevent
    # them from interfering with the final alignment.

    return unless @{$rlines} > 1;

    my $has_terminal_match = $rlines->[-1]->get_j_terminal_match();

    # ignore hanging side comments in these operations
    my @filtered   = grep { !$_->{_is_hanging_side_comment} } @{$rlines};
    my $rnew_lines = \@filtered;
    my @equals_info;
    my @line_info;

    my $jmax = @{$rnew_lines} - 1;
    return unless $jmax >= 0;

    my %is_good_tok;

    # create a hash of tokens for each line
    my $rline_hashes = [];
    my $saw_list_type;
    my $max_lev_diff = 0;
    foreach my $line ( @{$rnew_lines} ) {
        my $rhash     = {};
        my $rtokens   = $line->get_rtokens();
        my $rpatterns = $line->get_rpatterns();
        if ( !$saw_list_type && $line->get_list_type() ) { $saw_list_type = 1 }
        my $i = 0;
        my ( $i_eq, $tok_eq, $pat_eq );
        my ( $lev_min, $lev_max );
        foreach my $tok ( @{$rtokens} ) {
            my ( $raw_tok, $lev, $tag, $tok_count ) =
              decode_alignment_token($tok);

            if ( $tok !~ /^[#]$/ ) {
                if ( !defined($lev_min) ) { $lev_min = $lev; $lev_max = $lev; }
                else {
                    if ( $lev < $lev_min ) { $lev_min = $lev }
                    if ( $lev > $lev_max ) { $lev_max = $lev }
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
        push @equals_info, [ $i_eq, $tok_eq, $pat_eq ];
        push @line_info, [ $lev_min, $lev_max ];
        if ( defined($lev_min) ) {
            my $lev_diff = $lev_max - $lev_min;
            if ( $lev_diff > $max_lev_diff ) { $max_lev_diff = $lev_diff }
        }
    }

    # compare each line pair and record matches
    my $rtok_hash = {};
    my $nr        = 0;
    for ( my $jl = 0 ; $jl < $jmax ; $jl++ ) {
        my $nl = $nr;
        $nr = 0;
        my $jr      = $jl + 1;
        my $rhash_l = $rline_hashes->[$jl];
        my $rhash_r = $rline_hashes->[$jr];
        my $count   = 0;                      # UNUSED NOW?
        my $ntoks   = 0;
        foreach my $tok ( keys %{$rhash_l} ) {
            $ntoks++;
            if ( defined( $rhash_r->{$tok} ) ) {
                if ( $tok ne '#' ) { $count++; }
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
            $rnew_lines->[$jl]->{_end_group} = 1;
        }

        # Also set a line break if both lines have simple equals but with
        # different leading characters in patterns.  This check is similar to
        # one in sub check_match, and will prevent sub prune_alignment_tree
        # from removing alignments which otherwise should be kept. This fix
        # is rarely needed, but it can occasionally improve formatting.
        # For example:
        #     my $name = $this->{Name};
        #     $type = $this->ctype($genlooptype) if defined $genlooptype;
        #     my $declini = ( $asgnonly ? ""          : "\t$type *" );
        #     my $cast    = ( $type     ? "($type *)" : "" );
        # The last two lines start with 'my' and will not match the previous
        # line starting with $type, so we do not want prune_alignment tree
        # to delete their ? : alignments at a deeper level.
        my ( $i_eq_l, $tok_eq_l, $pat_eq_l ) = @{ $equals_info[$jl] };
        my ( $i_eq_r, $tok_eq_r, $pat_eq_r ) = @{ $equals_info[$jr] };
        if ( defined($i_eq_l) && defined($i_eq_r) ) {
            if (   $tok_eq_l eq $tok_eq_r
                && $i_eq_l == 0
                && $i_eq_r == 0
                && substr( $pat_eq_l, 0, 1 ) ne substr( $pat_eq_r, 0, 1 ) )
            {
                $rnew_lines->[$jl]->{_end_group} = 1;
            }
        }
    }

    # find subgroups
    my @subgroups;
    push @subgroups, [ 0, $jmax ];
    for ( my $jl = 0 ; $jl < $jmax ; $jl++ ) {
        if ( $rnew_lines->[$jl]->{_end_group} ) {
            $subgroups[-1]->[1] = $jl;
            push @subgroups, [ $jl + 1, $jmax ];
        }
    }

    # Loop to process each subgroups
    foreach my $item (@subgroups) {
        my ( $jbeg, $jend ) = @{$item};

        # look for complete ternary or if/elsif/else blocks
        my $nlines = $jend - $jbeg + 1;
        my %token_line_count;
        for ( my $jj = $jbeg ; $jj <= $jend ; $jj++ ) {
            my %seen;
            my $line    = $rnew_lines->[$jj];
            my $rtokens = $line->get_rtokens();
            foreach my $tok ( @{$rtokens} ) {
                if ( !$seen{$tok} ) {
                    $seen{$tok}++;
                    $token_line_count{$tok}++;
                }
            }
        }

        # Look for if/else/elsif and ternary blocks
        my $is_full_block;
        foreach my $tok ( keys %token_line_count ) {
            if ( $token_line_count{$tok} == $nlines ) {
                if ( $tok =~ /^\?/ || $tok =~ /^\{\d+if/ ) {
                    $is_full_block = 1;
                }
            }
        }

        # remove unwanted alignment tokens
        for ( my $jj = $jbeg ; $jj <= $jend ; $jj++ ) {
            my $line    = $rnew_lines->[$jj];
            my $rtokens = $line->get_rtokens();
            my $rhash   = $rline_hashes->[$jj];
            my $i_eq    = $equals_info[$jj]->[0];
            my @idel;
            my $imax = @{$rtokens} - 2;
            my $delete_above_level;

            for ( my $i = 0 ; $i <= $imax ; $i++ ) {
                my $tok = $rtokens->[$i];
                next if ( $tok eq '#' );    # shouldn't happen
                my ( $iii, $il, $ir, $raw_tok, $lev, $tag, $tok_count ) =
                  @{ $rhash->{$tok} };

                # always remove unmatched tokens
                my $delete_me = !defined($il) && !defined($ir);

                # also, if this is a complete ternary or if/elsif/else block,
                # remove all alignments which are not also in every line
                $delete_me ||=
                  ( $is_full_block && $token_line_count{$tok} < $nlines );

                # Remove all tokens above a certain level following a previous
                # deletion.  For example, we have to remove tagged higher level
                # alignment tokens following a => deletion because the tags of
                # higher level tokens will now be incorrect. For example, this
                # will prevent aligning commas as follows after deleting the
                # second =>
                #    $w->insert(
                #	ListBox => origin => [ 270, 160 ],
                #	size    => [ 200,           55 ],
                #    );
                if ( defined($delete_above_level) ) {
                    if ( $lev > $delete_above_level ) {
                        $delete_me ||= 1;    #$tag;
                    }
                    else { $delete_above_level = undef }
                }

                if (
                    $delete_me
                    && is_deletable_token( $tok, $i, $imax, $jj, $i_eq )

                    # Patch: do not touch the first line of a terminal match,
                    # such as below, because j_terminal has already been set.
                    #    if ($tag) { $tago = "<$tag>"; $tagc = "</$tag>"; }
                    #    else      { $tago = $tagc = ''; }
                    # But see snippets 'else1.t' and 'else2.t'
                    && !( $jj == $jbeg && $has_terminal_match && $nlines == 2 )

                  )
                {
                    push @idel, $i;
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
            }

            if (@idel) {
                delete_selected_tokens( $line, \@idel, $saw_list_type );
            }
        }

    }    # End loop over subgroups

    return $saw_list_type;
}

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
    for ( my $jj = 0 ; $jj < @{$rlines} ; $jj++ ) {
        my ($line) = $rlines->[$jj];
        my $rtokens = $line->get_rtokens();
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
    for ( my $jj = 0 ; $jj < @{$rlines} ; $jj++ ) {
        my ($line) = $rlines->[$jj];

        my $rtokens = $line->get_rtokens();
        my $i       = -1;
        my ( $lev_min, $lev_max );
        my $token_pattern_max = "";
        my %saw_level;
        my @token_info;
        my $is_monotonic = 1;

        # find the index of the last token before the side comment
        my $imax      = @{$rtokens} - 2;
        my $imax_true = $imax;

        # If the entire group is monotonic, and the line ends in a comma list,
        # walk it back to the first such comma. this will have the effect of
        # making all trailing ragged comma lists match in the prune tree
        # routine.  these trailing comma lists can better be handled by later
        # alignment rules.
        my $tok_end = $rtokens->[$imax];
        if ( $all_monotonic && $tok_end =~ /^,/ ) {
            my $i = $imax - 1;
            while ( $i >= 0 && $rtokens->[$i] eq $tok_end ) {
                $imax = $i;
                $i--;
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
        my @levs            = sort keys %saw_level;
        if ( !defined($lev_min) ) {
            $lev_min                     = -1;
            $lev_max                     = -1;
            $levs[0]                     = -1;
            $rtoken_patterns->{$lev_min} = "";
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

            my $debug   = 0;
            my $lev_top = pop @levs;    # alread did max level
            my $itok    = -1;
            foreach my $tok ( @{$rtokens} ) {
                $itok++;
                last if ( $itok > $imax );
                ##my ( $raw_tok, $lev, $tag, $tok_count ) = @{ $token_info[$itok] };
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
            local $" = ')(';
            print "lev_min=$lev_min, lev_max=$lev_max, levels=(@levs)\n";
            foreach my $key ( sort keys %{$rtoken_patterns} ) {
                print "$key => $rtoken_patterns->{$key}\n";
                print "$key => @{$rtoken_indexes->{$key}}\n";
            }
        };
    } ## end loop over lines
    return $rline_values;
}

sub prune_alignment_tree {
    my ($rlines) = @_;
    my $jmax = @{$rlines} - 1;
    return unless $jmax > 0;

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
    # (zero), so if there were no other alignements, these lines would all
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

    my $EXPLAIN = 0;

    ####################################################################
    # Prune Tree Step 1. Start by scanning the lines and collecting info
    ####################################################################

    # Note that the caller had this info but we have to redo this now because
    # alignment tokens may have been deleted.
    my $rline_values = get_line_token_info($rlines);

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

    my $rgroup_id = [];

    # Array to store info about the location of each line in the tree:
    #   $rgroup_id->[$jj] = $id
    # where
    #   $jj = line index
    #   $id = "n1.n2.n3" = decimal tree identifier of the group, i.e.
    #    "1.0.3" = group 1 -> child 0 -> child 3

    my $rgroup_index = {};

    # Hash giving information for each group
    #   $rgroup_id{$id} = [$jbeg, $jend, ]
    # where
    #   $jbeg = index of first line of group
    #   $jend = index of last line of group

    # the patterns and levels of the current group being formed at each depth
    my ( @token_patterns_current, @levels_current, @token_indexes_current );

    # the patterns and levels of the next line being tested at each depth
    my ( @token_patterns_next, @levels_next, @token_indexes_next );

    #########################################################
    # define a recursive worker subroutine for tree construction
    #########################################################

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

    ######################################################
    # Prune Tree Step 2. Loop to form the tree of matches.
    ######################################################
    for ( my $jp = 0 ; $jp <= $jmax ; $jp++ ) {

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
        foreach (@levels_next) {
            $token_patterns_next[$depth] =
              defined($_) ? $rtoken_patterns->{$_} : undef;
            $token_indexes_next[$depth] =
              defined($_) ? $rtoken_indexes->{$_} : undef;
            $depth++;
        }

        # Look for a change in match groups...

        # Initialize on the first line
        if ( $jp == 0 ) {
            my $n_parent;
            $end_node->( 0, $jm, $n_parent );
        }

        # End groups if a hard flag has been set
        elsif ( $rlines->[$jm]->{_end_group} ) {
            my $n_parent;
            $end_node->( 0, $jm, $n_parent );
        }

        # Continue at hanging side comment
        elsif ( $rlines->[$jp]->{_is_hanging_side_comment} ) {
            next;
        }

        # Otherwise see if anything changed and update the tree if so
        else {
            foreach my $depth ( 0 .. $MAX_DEPTH ) {

                my $def_current = defined( $token_patterns_current[$depth] );
                my $def_next    = defined( $token_patterns_next[$depth] );
                last unless ( $def_current || $def_next );
                if (   !$def_current
                    || !$def_next
                    || $token_patterns_current[$depth] ne
                    $token_patterns_next[$depth] )
                {
                    my $n_parent;
                    if ( $depth > 0 && defined( $match_tree[ $depth - 1 ] ) ) {
                        $n_parent = @{ $match_tree[ $depth - 1 ] } - 1;
                    }
                    $end_node->( $depth, $jm, $n_parent );
                    last;
                }
            }
        }
    } ## end loop to form tree of matches

    ##########################################################
    # Prune Tree Step 3. Make links from parent to child nodes
    ##########################################################

    # It seemed cleaner to do this as a separate step rather than during tree
    # construction.  The children nodes have links up to the parent node which
    # created them.  Now make links in the opposite direction, so the parents
    # can find the children.  We store the range of children nodes ($nc_beg,
    # $nc_end) of each parent with two additional indexes in the orignal array.
    # These will be undef if no children.
    for ( my $depth = $MAX_DEPTH ; $depth > 0 ; $depth-- ) {
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

    if ( 0 || $EXPLAIN > 0 ) {
        print "Tree complete. Found these groups:\n";
        foreach my $depth ( 0 .. $MAX_DEPTH ) {
            Dump_tree_groups( \@{ $match_tree[$depth] }, "depth=$depth" );
        }
    }

    #######################################################
    # Prune Tree Step 4. Make a list of nodes to be deleted
    #######################################################

    #  list of lines with tokens to be deleted:
    #  [$jbeg, $jend, $level_keep]
    #  $jbeg..$jend is the range of line indexes,
    #  $level_keep is the minimum level to keep
    my @delete_list;

    #  Groups with ending comma lists and their range of sizes:
    #  $ragged_comma_group{$id} = [ imax_group_min, imax_group_max ]
    my %ragged_comma_group;

    # Define a threshold line count for forcing a break
    my $nlines_break = 3;

    # We work with a list of nodes to visit at the next deeper depth.
    my @todo_list;
    if ( defined( $match_tree[0] ) ) {
        @todo_list = ( 0 .. @{ $match_tree[0] } - 1 );
    }

    for ( my $depth = 0 ; $depth <= $MAX_DEPTH ; $depth++ ) {
        last unless (@todo_list);
        my @todo_next;
        foreach my $np (@todo_list) {
            my ( $jbeg_p, $jend_p, $np_p, $lev_p, $pat_p, $nc_beg_p, $nc_end_p,
                $rindexes_p )
              = @{ $match_tree[$depth]->[$np] };
            my $nlines_p = $jend_p - $jbeg_p + 1;

            # Make a unique identifier for this group of matched lines
            my $id;
            if   ( $depth == 0 ) { $id = "$np" }
            else                 { $id = $rgroup_id->[$jbeg_p] . ".$np" }

            # Make a modified group name if this is a simple comma list.
            # This can simplify later operations.
            if ( !defined($nc_beg_p) ) {
                my ( $lev_min, $lev_max, $rtoken_patterns, $rlevs,
                    $rtoken_indexes, $is_monotonic, $imax_line, $imax_used )
                  = @{ $rline_values->[$jbeg_p] };
                if (   $lev_min == $group_level
                    && $imax_used == 0
                    && $imax_line != $imax_used )
                {
                    $id = "C" . $id;
                }
            }

            $rgroup_index->{$id} = [ $jbeg_p, $jend_p ];
            foreach my $jj ( $jbeg_p .. $jend_p ) {
                $rgroup_id->[$jj] = $id;
            }

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
            my $nmin_mono     = $depth + 3;    #TODO: test with 2
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

    #############################################################
    # Prune Tree Step 5. Loop to delete selected alignment tokens
    #############################################################
    foreach my $item (@delete_list) {
        my ( $jbeg, $jend, $level_keep ) = @{$item};
        foreach my $jj ( $jbeg .. $jend ) {
            my $line = $rlines->[$jj];
            my @idel;
            my $rtokens = $line->get_rtokens();
            my $imax    = @{$rtokens} - 2;
            for ( my $i = 0 ; $i <= $imax ; $i++ ) {
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
    return ( $rgroup_id, $rgroup_index );
} ## end sub prune_alignment_tree

sub Dump_tree_groups {
    my ( $rgroup, $msg ) = @_;
    print "$msg\n";
    local $" = ')(';
    foreach my $item ( @{$rgroup} ) {
        my @fix = @{$item};
        foreach (@fix) { $_ = "undef" unless defined $_; }
        $fix[4] = "...";
        print "(@fix)\n";
    }
    return;
}

{    # decide_if_aligned_pair

    my %is_if_or;
    my %is_assignment;

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
    }

## uses Global symbols {
##  '$group_level'
##  '$last_comment_column'
##  '$last_level_written'
##  '$last_side_comment_length'

##  '$is_matching_terminal_line'
##  '$marginal_match'
##  '$previous_maximum_jmax_seen'
##  '$previous_minimum_jmax_seen'

##  '$rOpts_minimum_space_to_comment'
##  '@group_lines'
## }

    sub decide_if_aligned_pair {

        my ($imax_align) = @_;

        # Do not try to align two lines which are not really similar
        return unless ( @group_lines == 2 );
        return if ($is_matching_terminal_line);

        # always align lists
        my $group_list_type = $group_lines[0]->get_list_type();
        return 0 if ($group_list_type);

        my $jmax0          = $group_lines[0]->get_jmax();
        my $jmax1          = $group_lines[1]->get_jmax();
        my $rtokens        = $group_lines[0]->get_rtokens();
        my $leading_equals = ( $rtokens->[0] =~ /=/ );

        # scan the tokens on the second line
        my $rtokens1 = $group_lines[1]->get_rtokens();
        my $saw_if_or;        # if we saw an 'if' or 'or' at group level
        my $raw_tokb = "";    # first token seen at group level
        for ( my $j = 0 ; $j < $jmax1 - 1 ; $j++ ) {
            my ( $raw_tok, $lev, $tag, $tok_count ) =
              decode_alignment_token( $rtokens1->[$j] );
            if ( $raw_tok && $lev == $group_level ) {
                if ( !$raw_tokb ) { $raw_tokb = $raw_tok }
                $saw_if_or ||= $is_if_or{$raw_tok};
            }
        }

        # A marginal match is a match which has different patterns. Normally,
        # we should not allow exactly two lines to match if marginal. But
        # we can allow matching in some specific cases.
        my $is_marginal = $marginal_match;

        # lines with differing number of alignment tokens are marginal
        $is_marginal ||=
          $previous_maximum_jmax_seen != $previous_minimum_jmax_seen
          && !$is_assignment{$raw_tokb};

        # We will use the line endings to help decide on alignments...
        # See if the lines end with semicolons...
        my $rpatterns0 = $group_lines[0]->get_rpatterns();
        my $rpatterns1 = $group_lines[1]->get_rpatterns();
        my $sc_term0;
        my $sc_term1;
        if ( $jmax0 < 1 || $jmax1 < 1 ) {

            # shouldn't happen
        }
        else {
            my $pat0 = $rpatterns0->[ $jmax0 - 1 ];
            my $pat1 = $rpatterns1->[ $jmax1 - 1 ];
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
          && $jmax1 == 2
          && $sc_term0 ne $sc_term1;

        # Undo the marginal match flag in certain cases,
        if ($is_marginal) {

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
            my $pat0 = $rpatterns0->[0];
            my $pat1 = $rpatterns1->[0];

            ##########################################################
            # Turn off the marginal flag for some types of assignments
            ##########################################################
            if ( $is_assignment{$raw_tokb} ) {

                # undo marginal flag if first line is semicolon terminated
                # and leading patters match
                if ($sc_term0) {    # && $sc_term1) {
                    $is_marginal = $pat0 ne $pat1;
                }
            }
            elsif ( $raw_tokb eq '=>' ) {

                # undo marginal flag if patterns match
                $is_marginal = $pat0 ne $pat1;
            }
            elsif ( $raw_tokb eq '=~' ) {

                # undo marginal flag if both lines are semicolon terminated
                # and leading patters match
                if ( $sc_term1 && $sc_term0 ) {
                    $is_marginal = $pat0 ne $pat1;
                }
            }

            ######################################################
            # Turn off the marginal flag if we saw an 'if' or 'or'
            ######################################################

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
        }

        # Remove the alignments if still marginal
        if ($is_marginal) { combine_fields($imax_align) }
        return;
    }
}

sub get_extra_leading_spaces_multiple_groups {

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

    return 0 unless ($extra_indent_ok);
    return 0 unless ( @{$rlines} && @{$rgroups} );

    my $object = $rlines->[0]->get_indentation();
    return 0 unless ( ref($object) );
    my $extra_leading_spaces            = 0;
    my $extra_indentation_spaces_wanted = get_recoverable_spaces($object);

    # loop over all groups
    my $ng = -1;
    foreach my $item ( @{$rgroups} ) {
        $ng++;
        my ( $jbeg, $jend ) = @{$item};
        foreach my $j ( $jbeg .. $jend ) {
            next if ( $j == 0 );

            # all indentation objects must be the same
            if ( $object != $rlines->[$j]->get_indentation() ) {
                return 0;
            }
        }

       # find the maximum space without exceeding the line length for this group
        my $avail = $rlines->[$jbeg]->get_available_space_on_right();
        my $spaces =
          ( $avail > $extra_indentation_spaces_wanted )
          ? $extra_indentation_spaces_wanted
          : $avail;
        if ( $spaces < 0 ) { $spaces = 0 }

        # update the minimum spacing
        if ( $ng == 0 || $spaces < $extra_leading_spaces ) {
            $extra_leading_spaces = $spaces;
        }
    }

    # update the indentation object because with -icp the terminal
    # ');' will use the same adjustment.
    $object->permanently_decrease_available_spaces( -$extra_leading_spaces );
    return $extra_leading_spaces;
}

sub adjust_side_comment_multiple_groups {

    my ( $rlines, $rgroups ) = @_;

    # let's see if we can move the side comment field out a little
    # to improve readability (the last field is always a side comment field)

## uses Global symbols {
##  '$group_level'                    -- the common level of all these lines
##  '$last_level_written'             -- level of previous set of lines
##  '$last_comment_column'            -- comment col of previous lines
##  '$last_side_comment_length'       -- its length
##  '$rOpts_minimum_space_to_comment'
## }

    # Look for any nonblank side comments
    my ( $ng_sc_beg, $ng_sc_end );
    my ( $j_sc_beg,  $j_sc_end );
    my $ng = -1;
    my @is_group_with_side_comment;
    foreach my $item ( @{$rgroups} ) {
        $ng++;
        my ( $jbeg, $jend ) = @{$item};
        foreach my $j ( $jbeg .. $jend ) {
            my $line = $rlines->[$j];
            my $jmax = $line->get_jmax();
            if ( $line->get_rfield_lengths()->[$jmax] ) {
                $is_group_with_side_comment[$ng]++;
                if ( !defined($ng_sc_beg) ) {
                    $ng_sc_beg = $ng;
                    $ng_sc_end = $ng;
                    $j_sc_beg  = $j;
                    $j_sc_end  = $j;
                }
                else {
                    $ng_sc_end = $ng;
                    $j_sc_end  = $j;
                }
            }
        }
    }

    # done if nothing to do
    return unless defined($ng_sc_beg);

    # If there are multiple groups we will do two passes
    # so that we can find a common alignment for all groups.
    my $MAX_PASS = ( $ng_sc_end > $ng_sc_beg ) ? 2 : 1;

    # Loop over passes
    my $max_comment_column = $last_comment_column;
    for ( my $PASS = 1 ; $PASS <= $MAX_PASS ; $PASS++ ) {

        # If there are two passes, then on the last pass make the old column
        # equal to the largest of the group.  This will result in the comments
        # being aligned if possible.
        if ( $PASS == $MAX_PASS ) { $last_comment_column = $max_comment_column }

        # Loop over the groups
        my $ng = -1;
        my $column_limit;
        foreach my $item ( @{$rgroups} ) {
            $ng++;
            next if ( $ng < $ng_sc_beg );
            last if ( $ng > $ng_sc_end );
            next unless ( $is_group_with_side_comment[$ng] );
            my ( $jbeg, $jend ) = @{$item};

            # Note that since all lines in a group have common alignments, we
            # just have to work on one of the lines (the first line).
            my $line = $rlines->[$jbeg];
            my $jmax = $line->get_jmax();
            last if ( $PASS < $MAX_PASS && $line->{_is_hanging_side_comment} );

            # the maximum space without exceeding the line length:
            my $avail = $line->get_available_space_on_right();

            # try to use the previous comment column
            my $side_comment_column = $line->get_column( $jmax - 1 );
            my $move = $last_comment_column - $side_comment_column;

            # Remember the maximum possible column of the first line with side
            # comment
            if ( !defined($column_limit) ) {
                $column_limit = $side_comment_column + $avail;
            }

            if ( $jmax > 0 ) {

                # but if this doesn't work, give up and use the minimum space
                if ( $move > $avail ) {
                    $move = $rOpts_minimum_space_to_comment - 1;
                }

                # but we want some minimum space to the comment
                my $min_move = $rOpts_minimum_space_to_comment - 1;
                if (   $move >= 0
                    && $last_side_comment_length > 0
                    && ( $j_sc_beg == 0 )
                    && $group_level == $last_level_written )
                {
                    $min_move = 0;
                }

                if ( $move < $min_move ) {
                    $move = $min_move;
                }

                # previously, an upper bound was placed on $move here,
                # (maximum_space_to_comment), but it was not helpful

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
                    $last_comment_column = $line->get_column( $jmax - 1 );
                }
            }
        } ## end loop over groups
    } ## end loop over passes
    return;
}

sub adjust_side_comment_single_group {

    my $do_not_align = shift;

    # let's see if we can move the side comment field out a little
    # to improve readability (the last field is always a side comment field)
    my $have_side_comment       = 0;
    my $first_side_comment_line = -1;
    my $maximum_field_index     = $group_lines[0]->get_jmax();
    my $i                       = 0;
    foreach my $line (@group_lines) {
        if ( $line->get_rfield_lengths()->[$maximum_field_index] ) {
            $have_side_comment       = 1;
            $first_side_comment_line = $i;
            last;
        }
        $i++;
    }

    my $kmax = $maximum_field_index + 1;

    if ($have_side_comment) {

        my $line = $group_lines[0];

        # the maximum space without exceeding the line length:
        my $avail = $line->get_available_space_on_right();

        # try to use the previous comment column
        my $side_comment_column = $line->get_column( $kmax - 2 );
        my $move                = $last_comment_column - $side_comment_column;

        if ( $kmax > 0 && !$do_not_align ) {

            # but if this doesn't work, give up and use the minimum space
            if ( $move > $avail ) {
                $move = $rOpts_minimum_space_to_comment - 1;
            }

            # but we want some minimum space to the comment
            my $min_move = $rOpts_minimum_space_to_comment - 1;
            if (   $move >= 0
                && $last_side_comment_length > 0
                && ( $first_side_comment_line == 0 )
                && $group_level == $last_level_written )
            {
                $min_move = 0;
            }

            if ( $move < $min_move ) {
                $move = $min_move;
            }

            # previously, an upper bound was placed on $move here,
            # (maximum_space_to_comment), but it was not helpful

            # don't exceed the available space
            if ( $move > $avail ) { $move = $avail }

            # we can only increase space, never decrease
            if ( $move > 0 ) {
                $line->increase_field_width( $maximum_field_index - 1, $move );
            }

            # remember this column for the next group
            $last_comment_column = $line->get_column( $kmax - 2 );
        }
        else {

            # try to at least line up the existing side comment location
            if ( $kmax > 0 && $move > 0 && $move < $avail ) {
                $line->increase_field_width( $maximum_field_index - 1, $move );
                $do_not_align = 0;
            }

            # reset side comment column if we can't align
            else {
                forget_side_comment();
            }
        }
    }
    return $do_not_align;
}

sub valign_output_step_A {

    ###############################################################
    # This is Step A in writing vertically aligned lines.
    # The line is prepared according to the alignments which have
    # been found. Then it is shipped to the next step.
    ###############################################################

    my %input_hash = @_;

    my $line                 = $input_hash{line};
    my $min_ci_gap           = $input_hash{min_ci_gap};
    my $do_not_align         = $input_hash{do_not_align};
    my $group_leader_length  = $input_hash{group_leader_length};
    my $extra_leading_spaces = $input_hash{extra_leading_spaces};

    my $rfields                   = $line->get_rfields();
    my $rfield_lengths            = $line->get_rfield_lengths();
    my $leading_space_count       = $line->get_leading_space_count();
    my $outdent_long_lines        = $line->get_outdent_long_lines();
    my $maximum_field_index       = $line->get_jmax();
    my $rvertical_tightness_flags = $line->get_rvertical_tightness_flags();

    # add any extra spaces
    if ( $leading_space_count > $group_leader_length ) {
        $leading_space_count += $min_ci_gap;
    }

    my $str     = $rfields->[0];
    my $str_len = $rfield_lengths->[0];

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
        my $col = $line->get_column( $j - 1 );
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
        if ( $rOpts_fixed_position_side_comment && $j == $maximum_field_index )
        {
            my $newpad = $pad + $rOpts_fixed_position_side_comment - $col - 1;
            if ( $newpad >= 0 ) { $pad = $newpad; }
        }

        # accumulate the padding
        if ( $pad > 0 ) { $total_pad_count += $pad; }

        # add this field
        if ( !defined $rfields->[$j] ) {
            write_diagnostics("UNDEFined field at j=$j\n");
        }

        # only add padding when we have a finite field;
        # this avoids extra terminal spaces if we have empty fields
        if ( $rfield_lengths->[$j] > 0 ) {
            $str .= ' ' x $total_pad_count;
            $str_len += $total_pad_count;
            $total_pad_count = 0;
            $str .= $rfields->[$j];
            $str_len += $rfield_lengths->[$j];
        }
        else {
            $total_pad_count = 0;
        }

        # update side comment history buffer
        if ( $j == $maximum_field_index ) {
            my $lineno = $file_writer_object->get_output_line_number();
            shift @side_comment_history;
            push @side_comment_history, [ $lineno, $col ];
        }
    }

    my $side_comment_length = $rfield_lengths->[$maximum_field_index];

    # ship this line off
    valign_output_step_B(
        leading_space_count => $leading_space_count + $extra_leading_spaces,
        line                => $str,
        line_length         => $str_len,
        side_comment_length => $side_comment_length,
        outdent_long_lines  => $outdent_long_lines,
        rvertical_tightness_flags => $rvertical_tightness_flags,
        level                     => $group_level,
    );
    return;
}

sub get_extra_leading_spaces {

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

    my $extra_leading_spaces = 0;
    if ($extra_indent_ok) {
        my $object = $group_lines[0]->get_indentation();
        if ( ref($object) ) {
            my $extra_indentation_spaces_wanted =
              get_recoverable_spaces($object);

            # all indentation objects must be the same
            for my $i ( 1 .. @group_lines - 1 ) {
                if ( $object != $group_lines[$i]->get_indentation() ) {
                    $extra_indentation_spaces_wanted = 0;
                    last;
                }
            }

            if ($extra_indentation_spaces_wanted) {

                # the maximum space without exceeding the line length:
                my $avail = $group_lines[0]->get_available_space_on_right();
                $extra_leading_spaces =
                  ( $avail > $extra_indentation_spaces_wanted )
                  ? $extra_indentation_spaces_wanted
                  : $avail;

                # update the indentation object because with -icp the terminal
                # ');' will use the same adjustment.
                $object->permanently_decrease_available_spaces(
                    -$extra_leading_spaces );
            }
        }
    }
    return $extra_leading_spaces;
}

sub combine_fields {

    # We have a group of two lines for which we do not want to align tokens
    # between index $imax_align and the side comment.  So we will delete fields
    # between $imax_align and the side comment.  Alignments have already
    # been set so we have to adjust them.

    my ($imax_align) = @_;
    if ( !defined($imax_align) ) { $imax_align = -1 }

    # Correction: although this routine has the ability to retain some leading
    # alignments, overall the results are much better if we always remove all
    # of the alignments.  Here is an example of the problem if we do not
    # do this. The first two lines are marginal but match their =~ matches
    # the third line. But if we keep it we get a big gap:
    #  return $path unless $path =~ /^~/;
    #  $path                     =~ s:^~([^/]+):(getpwnam($1))[$[+7]:e;
    #  $path =~ s:^~:$ENV{'HOME'} || (getpwuid($<))[$[+7]:e;
    $imax_align = -1;

    # Uses global variables:
    #  @group_lines

    # First delete the unwanted tokens
    my $jmax_old       = $group_lines[0]->get_jmax();
    my @old_alignments = $group_lines[0]->get_alignments();
    my @idel           = ( $imax_align + 1 .. $jmax_old - 2 );

    return unless (@idel);

    foreach my $line (@group_lines) {
        delete_selected_tokens( $line, \@idel );
    }

    # Now adjust the alignments.  Note that the side comment alignment
    # is always at jmax-1, and there is an ending alignment at jmax.
    my @new_alignments;
    if ( $imax_align >= 0 ) {
        @new_alignments[ 0 .. $imax_align ] =
          @old_alignments[ 0 .. $imax_align ];
    }

    my $jmax_new = $group_lines[0]->get_jmax();

    $new_alignments[ $jmax_new - 1 ] = $old_alignments[ $jmax_old - 1 ];
    $new_alignments[$jmax_new] = $old_alignments[$jmax_old];
    $group_lines[0]->set_alignments(@new_alignments);
    $group_lines[1]->set_alignments(@new_alignments);
    return;
}

sub get_output_line_number {

    # the output line number reported to a caller is the number of items
    # written plus the number of items in the buffer
    my $self   = shift;
    my $nlines = @group_lines;
    return $nlines + $file_writer_object->get_output_line_number();
}

sub valign_output_step_B {

    ###############################################################
    # This is Step B in writing vertically aligned lines.
    # Vertical tightness is applied according to preset flags.
    # In particular this routine handles stacking of opening
    # and closing tokens.
    ###############################################################

    my %input_hash = @_;

    my $leading_space_count       = $input_hash{leading_space_count};
    my $str                       = $input_hash{line};
    my $str_length                = $input_hash{line_length};
    my $side_comment_length       = $input_hash{side_comment_length};
    my $outdent_long_lines        = $input_hash{outdent_long_lines};
    my $rvertical_tightness_flags = $input_hash{rvertical_tightness_flags};
    my $level                     = $input_hash{level};

    # Useful -gcs test cases for wide characters are
    # perl527/(method.t.2, reg_mesg.t, mime-header.t)

    # handle outdenting of long lines:
    if ($outdent_long_lines) {
        my $excess =
          $str_length -
          $side_comment_length +
          $leading_space_count -
          maximum_line_length_for_level($level);
        if ( $excess > 0 ) {
            $leading_space_count = 0;
            $last_outdented_line_at =
              $file_writer_object->get_output_line_number();

            unless ($outdented_line_count) {
                $first_outdented_line_at = $last_outdented_line_at;
            }
            $outdented_line_count++;
        }
    }

    # Make preliminary leading whitespace.  It could get changed
    # later by entabbing, so we have to keep track of any changes
    # to the leading_space_count from here on.
    my $leading_string =
      $leading_space_count > 0 ? ( ' ' x $leading_space_count ) : "";
    my $leading_string_length = length($leading_string);

    # Unpack any recombination data; it was packed by
    # sub send_lines_to_vertical_aligner. Contents:
    #
    #   [0] type: 1=opening non-block    2=closing non-block
    #             3=opening block brace  4=closing block brace
    #   [1] flag: if opening: 1=no multiple steps, 2=multiple steps ok
    #             if closing: spaces of padding to use
    #   [2] sequence number of container
    #   [3] valid flag: do not append if this flag is false
    #
    my ( $open_or_close, $tightness_flag, $seqno, $valid, $seqno_beg,
        $seqno_end );
    if ($rvertical_tightness_flags) {
        (
            $open_or_close, $tightness_flag, $seqno, $valid, $seqno_beg,
            $seqno_end
        ) = @{$rvertical_tightness_flags};
    }

    $seqno_string = $seqno_end;

    # handle any cached line ..
    # either append this line to it or write it out
    # Note: the function length() is used in this next test out of caution.
    # All testing has shown that the variable $cached_line_text_length is
    # correct, but its calculation is complex and a loss of cached text would
    # be a disaster.
    if ( length($cached_line_text) ) {

        # Dump an invalid cached line
        if ( !$cached_line_valid ) {
            valign_output_step_C( $cached_line_text,
                $cached_line_leading_space_count,
                $last_level_written );
        }

        # Handle cached line ending in OPENING tokens
        elsif ( $cached_line_type == 1 || $cached_line_type == 3 ) {

            my $gap = $leading_space_count - $cached_line_text_length;

            # handle option of just one tight opening per line:
            if ( $cached_line_flag == 1 ) {
                if ( defined($open_or_close) && $open_or_close == 1 ) {
                    $gap = -1;
                }
            }

            if ( $gap >= 0 && defined($seqno_beg) ) {
                $leading_string        = $cached_line_text . ' ' x $gap;
                $leading_string_length = $cached_line_text_length + $gap;
                $leading_space_count   = $cached_line_leading_space_count;
                $seqno_string = $cached_seqno_string . ':' . $seqno_beg;
                $level        = $last_level_written;
            }
            else {
                valign_output_step_C( $cached_line_text,
                    $cached_line_leading_space_count,
                    $last_level_written );
            }
        }

        # Handle cached line ending in CLOSING tokens
        else {
            my $test_line = $cached_line_text . ' ' x $cached_line_flag . $str;
            my $test_line_length =
              $cached_line_text_length + $cached_line_flag + $str_length;
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
                && ( $test_line_length <=
                    maximum_line_length_for_level($last_level_written) )
              )
            {

                $seqno_string = $cached_seqno_string . ':' . $seqno_beg;

                # Patch to outdent closing tokens ending # in ');'
                # If we are joining a line like ');' to a previous stacked
                # set of closing tokens, then decide if we may outdent the
                # combined stack to the indentation of the ');'.  Since we
                # should not normally outdent any of the other tokens more than
                # the indentation of the lines that contained them, we will
                # only do this if all of the corresponding opening
                # tokens were on the same line.  This can happen with
                # -sot and -sct.  For example, it is ok here:
                #   __PACKAGE__->load_components( qw(
                #         PK::Auto
                #         Core
                #   ));
                #
                #   But, for example, we do not outdent in this example because
                #   that would put the closing sub brace out farther than the
                #   opening sub brace:
                #
                #   perltidy -sot -sct
                #   $c->Tk::bind(
                #       '<Control-f>' => sub {
                #           my ($c) = @_;
                #           my $e = $c->XEvent;
                #           itemsUnderArea $c;
                #       } );
                #
                if ( $str =~ /^\);/ && $cached_line_text =~ /^[\)\}\]\s]*$/ ) {

                    # The way to tell this is if the stacked sequence numbers
                    # of this output line are the reverse of the stacked
                    # sequence numbers of the previous non-blank line of
                    # sequence numbers.  So we can join if the previous
                    # nonblank string of tokens is the mirror image.  For
                    # example if stack )}] is 13:8:6 then we are looking for a
                    # leading stack like [{( which is 6:8:13 We only need to
                    # check the two ends, because the intermediate tokens must
                    # fall in order.  Note on speed: having to split on colons
                    # and eliminate multiple colons might appear to be slow,
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
                            if ( ( length($ws) == $diff ) && $ws =~ /^\s+$/ ) {

                                $test_line = substr( $test_line, $diff );
                                $cached_line_leading_space_count -= $diff;
                                $last_level_written =
                                  level_change(
                                    $cached_line_leading_space_count,
                                    $diff, $last_level_written );
                                reduce_valign_buffer_indentation($diff);
                            }

                            # shouldn't happen, but not critical:
                            ##else {
                            ## ERROR transferring indentation here
                            ##}
                        }
                    }
                }

                $str                   = $test_line;
                $str_length            = $test_line_length;
                $leading_string        = "";
                $leading_string_length = 0;
                $leading_space_count   = $cached_line_leading_space_count;
                $level                 = $last_level_written;
            }
            else {
                valign_output_step_C( $cached_line_text,
                    $cached_line_leading_space_count,
                    $last_level_written );
            }
        }
    }
    $cached_line_type        = 0;
    $cached_line_text        = "";
    $cached_line_text_length = 0;

    # make the line to be written
    my $line        = $leading_string . $str;
    my $line_length = $leading_string_length + $str_length;

    # write or cache this line
    if ( !$open_or_close || $side_comment_length > 0 ) {
        valign_output_step_C( $line, $leading_space_count, $level );
    }
    else {
        $cached_line_text                = $line;
        $cached_line_text_length         = $line_length;
        $cached_line_type                = $open_or_close;
        $cached_line_flag                = $tightness_flag;
        $cached_seqno                    = $seqno;
        $cached_line_valid               = $valid;
        $cached_line_leading_space_count = $leading_space_count;
        $cached_seqno_string             = $seqno_string;
    }

    $last_level_written       = $level;
    $last_side_comment_length = $side_comment_length;
    $extra_indent_ok          = 0;
    return;
}

sub valign_output_step_C {

    ###############################################################
    # This is Step C in writing vertically aligned lines.
    # Lines are either stored in a buffer or passed along to the next step.
    # The reason for storing lines is that we may later want to reduce their
    # indentation when -sot and -sct are both used.
    ###############################################################
    my @args = @_;

    # Dump any saved lines if we see a line with an unbalanced opening or
    # closing token.
    dump_valign_buffer() if ( $seqno_string && $valign_buffer_filling );

    # Either store or write this line
    if ($valign_buffer_filling) {
        push @valign_buffer, [@args];
    }
    else {
        valign_output_step_D(@args);
    }

    # For lines starting or ending with opening or closing tokens..
    if ($seqno_string) {
        $last_nonblank_seqno_string = $seqno_string;

        # Start storing lines when we see a line with multiple stacked opening
        # tokens.
        # patch for RT #94354, requested by Colin Williams
        if ( $seqno_string =~ /^\d+(\:+\d+)+$/ && $args[0] !~ /^[\}\)\]\:\?]/ )
        {

            # This test is efficient but a little subtle: The first test says
            # that we have multiple sequence numbers and hence multiple opening
            # or closing tokens in this line.  The second part of the test
            # rejects stacked closing and ternary tokens.  So if we get here
            # then we should have stacked unbalanced opening tokens.

            # Here is a complex example:

            # Foo($Bar[0], {  # (side comment)
            # 	baz => 1,
            # });

            # The first line has sequence 6::4.  It does not begin with
            # a closing token or ternary, so it passes the test and must be
            # stacked opening tokens.

            # The last line has sequence 4:6 but is a stack of closing tokens,
            # so it gets rejected.

            # Note that the sequence number of an opening token for a qw quote
            # is a negative number and will be rejected.
            # For example, for the following line:
            #    skip_symbols([qw(
            # $seqno_string='10:5:-1'.  It would be okay to accept it but
            # I decided not to do this after testing.

            $valign_buffer_filling = $seqno_string;

        }
    }
    return;
}

sub valign_output_step_D {

    ###############################################################
    # This is Step D in writing vertically aligned lines.
    # Write one vertically aligned line of code to the output object.
    ###############################################################

    my ( $line, $leading_space_count, $level ) = @_;

    # The line is currently correct if there is no tabbing (recommended!)
    # We may have to lop off some leading spaces and replace with tabs.
    if ( $leading_space_count > 0 ) {

        # Nothing to do if no tabs
        if ( !( $rOpts_tabs || $rOpts_entab_leading_whitespace )
            || $rOpts_indent_columns <= 0 )
        {

            # nothing to do
        }

        # Handle entab option
        elsif ($rOpts_entab_leading_whitespace) {

         # Patch 12-nov-2018 based on report from Glenn. Extra padding was
         # not correctly entabbed, nor were side comments:
         # Increase leading space count for a padded line to get correct tabbing
            if ( $line =~ /^(\s+)(.*)$/ ) {
                my $spaces = length($1);
                if ( $spaces > $leading_space_count ) {
                    $leading_space_count = $spaces;
                }
            }

            my $space_count =
              $leading_space_count % $rOpts_entab_leading_whitespace;
            my $tab_count =
              int( $leading_space_count / $rOpts_entab_leading_whitespace );
            my $leading_string = "\t" x $tab_count . ' ' x $space_count;
            if ( $line =~ /^\s{$leading_space_count,$leading_space_count}/ ) {
                substr( $line, 0, $leading_space_count ) = $leading_string;
            }
            else {

                # shouldn't happen - program error counting whitespace
                # - skip entabbing
                VALIGN_DEBUG_FLAG_TABS
                  && warning(
"Error entabbing in valign_output_step_D: expected count=$leading_space_count\n"
                  );
            }
        }

        # Handle option of one tab per level
        else {
            my $leading_string = ( "\t" x $level );
            my $space_count =
              $leading_space_count - $level * $rOpts_indent_columns;

            # shouldn't happen:
            if ( $space_count < 0 ) {

                # But it could be an outdented comment
                if ( $line !~ /^\s*#/ ) {
                    VALIGN_DEBUG_FLAG_TABS
                      && warning(
"Error entabbing in valign_output_step_D: for level=$group_level count=$leading_space_count\n"
                      );
                }
                $leading_string = ( ' ' x $leading_space_count );
            }
            else {
                $leading_string .= ( ' ' x $space_count );
            }
            if ( $line =~ /^\s{$leading_space_count,$leading_space_count}/ ) {
                substr( $line, 0, $leading_space_count ) = $leading_string;
            }
            else {

                # shouldn't happen - program error counting whitespace
                # we'll skip entabbing
                VALIGN_DEBUG_FLAG_TABS
                  && warning(
"Error entabbing in valign_output_step_D: expected count=$leading_space_count\n"
                  );
            }
        }
    }
    $file_writer_object->write_code_line( $line . "\n" );
    return;
}

{    # begin get_leading_string

    my @leading_string_cache;

    sub get_leading_string {

        # define the leading whitespace string for this line..
        my $leading_whitespace_count = shift;

        # Handle case of zero whitespace, which includes multi-line quotes
        # (which may have a finite level; this prevents tab problems)
        if ( $leading_whitespace_count <= 0 ) {
            return "";
        }

        # look for previous result
        elsif ( $leading_string_cache[$leading_whitespace_count] ) {
            return $leading_string_cache[$leading_whitespace_count];
        }

        # must compute a string for this number of spaces
        my $leading_string;

        # Handle simple case of no tabs
        if ( !( $rOpts_tabs || $rOpts_entab_leading_whitespace )
            || $rOpts_indent_columns <= 0 )
        {
            $leading_string = ( ' ' x $leading_whitespace_count );
        }

        # Handle entab option
        elsif ($rOpts_entab_leading_whitespace) {
            my $space_count =
              $leading_whitespace_count % $rOpts_entab_leading_whitespace;
            my $tab_count = int(
                $leading_whitespace_count / $rOpts_entab_leading_whitespace );
            $leading_string = "\t" x $tab_count . ' ' x $space_count;
        }

        # Handle option of one tab per level
        else {
            $leading_string = ( "\t" x $group_level );
            my $space_count =
              $leading_whitespace_count - $group_level * $rOpts_indent_columns;

            # shouldn't happen:
            if ( $space_count < 0 ) {
                VALIGN_DEBUG_FLAG_TABS
                  && warning(
"Error in get_leading_string: for level=$group_level count=$leading_whitespace_count\n"
                  );

                # -- skip entabbing
                $leading_string = ( ' ' x $leading_whitespace_count );
            }
            else {
                $leading_string .= ( ' ' x $space_count );
            }
        }
        $leading_string_cache[$leading_whitespace_count] = $leading_string;
        return $leading_string;
    }
}    # end get_leading_string

sub report_anything_unusual {
    my $self = shift;
    if ( $outdented_line_count > 0 ) {
        write_logfile_entry(
            "$outdented_line_count long lines were outdented:\n");
        write_logfile_entry(
            "  First at output line $first_outdented_line_at\n");

        if ( $outdented_line_count > 1 ) {
            write_logfile_entry(
                "   Last at output line $last_outdented_line_at\n");
        }
        write_logfile_entry(
            "  use -noll to prevent outdenting, -l=n to increase line length\n"
        );
        write_logfile_entry("\n");
    }
    return;
}
1;

