#####################################################################
#
# the Perl::Tidy::IndentationItem class supplies items which contain
# how much whitespace should be used at the start of a line
#
#####################################################################

package Perl::Tidy::IndentationItem;
use strict;
use warnings;
our $VERSION = '20201001';

BEGIN {

    # Array index names
    my $i = 0;
    use constant {
        _spaces_             => $i++,
        _level_              => $i++,
        _ci_level_           => $i++,
        _available_spaces_   => $i++,
        _closed_             => $i++,
        _comma_count_        => $i++,
        _sequence_number_    => $i++,
        _index_              => $i++,
        _have_child_         => $i++,
        _recoverable_spaces_ => $i++,
        _align_paren_        => $i++,
        _marked_             => $i++,
        _stack_depth_        => $i++,
        _starting_index_K_   => $i++,
        _arrow_count_        => $i++,
    };
}

sub AUTOLOAD {

    # Catch any undefined sub calls so that we are sure to get
    # some diagnostic information.  This sub should never be called
    # except for a programming error.
    our $AUTOLOAD;
    return if ( $AUTOLOAD eq 'DESTROY' );
    my ( $pkg, $fname, $lno ) = caller();
    print STDERR <<EOM;
======================================================================
Unexpected call to Autoload looking for sub $AUTOLOAD
Called from package: '$pkg'  
Called from File '$fname'  at line '$lno'
This error is probably due to a recent programming change
======================================================================
EOM
    exit 1;
}

sub DESTROY {

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub new {

    # Create an 'indentation_item' which describes one level of leading
    # whitespace when the '-lp' indentation is used.
    my ( $class, %input_hash ) = @_;

    my $spaces              = $input_hash{spaces};
    my $level               = $input_hash{level};
    my $ci_level            = $input_hash{ci_level};
    my $available_spaces    = $input_hash{available_spaces};
    my $index               = $input_hash{index};
    my $gnu_sequence_number = $input_hash{gnu_sequence_number};
    my $align_paren         = $input_hash{align_paren};
    my $stack_depth         = $input_hash{stack_depth};
    my $starting_index_K    = $input_hash{starting_index_K};

    my $closed            = -1;
    my $arrow_count       = 0;
    my $comma_count       = 0;
    my $have_child        = 0;
    my $want_right_spaces = 0;
    my $marked            = 0;

    # DEFINITIONS:
    # spaces             =>  # total leading white spaces
    # level              =>  # the indentation 'level'
    # ci_level           =>  # the 'continuation level'
    # available_spaces   =>  # how many left spaces available
    #                        # for this level
    # closed             =>  # index where we saw closing '}'
    # comma_count        =>  # how many commas at this level?
    # sequence_number    =>  # output batch number
    # index              =>  # index in output batch list
    # have_child         =>  # any dependents?
    # recoverable_spaces =>  # how many spaces to the right
    #                        # we would like to move to get
    #                        # alignment (negative if left)
    # align_paren        =>  # do we want to try to align
    #                        # with an opening structure?
    # marked             =>  # if visited by corrector logic
    # stack_depth        =>  # indentation nesting depth
    # starting_index_K   =>  # first token index K of this level
    # arrow_count        =>  # how many =>'s

    my $self = [];
    $self->[_spaces_]             = $spaces;
    $self->[_level_]              = $level;
    $self->[_ci_level_]           = $ci_level;
    $self->[_available_spaces_]   = $available_spaces;
    $self->[_closed_]             = $closed;
    $self->[_comma_count_]        = $comma_count;
    $self->[_sequence_number_]    = $gnu_sequence_number;
    $self->[_index_]              = $index;
    $self->[_have_child_]         = $have_child;
    $self->[_recoverable_spaces_] = $want_right_spaces;
    $self->[_align_paren_]        = $align_paren;
    $self->[_marked_]             = $marked;
    $self->[_stack_depth_]        = $stack_depth;
    $self->[_starting_index_K_]   = $starting_index_K;
    $self->[_arrow_count_]        = $arrow_count;

    bless $self, $class;
    return $self;
}

sub permanently_decrease_available_spaces {

    # make a permanent reduction in the available indentation spaces
    # at one indentation item.  NOTE: if there are child nodes, their
    # total SPACES must be reduced by the caller.

    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_available_spaces();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_available_spaces($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->set_recoverable_spaces(0);

    return $deleted_spaces;
}

sub tentatively_decrease_available_spaces {

    # We are asked to tentatively delete $spaces_needed of indentation
    # for an indentation item.  We may want to undo this later.  NOTE: if
    # there are child nodes, their total SPACES must be reduced by the
    # caller.
    my ( $item, $spaces_needed ) = @_;
    my $available_spaces = $item->get_available_spaces();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $item->decrease_available_spaces($deleted_spaces);
    $item->decrease_SPACES($deleted_spaces);
    $item->increase_recoverable_spaces($deleted_spaces);
    return $deleted_spaces;
}

sub get_stack_depth {
    my $self = shift;
    return $self->[_stack_depth_];
}

sub get_spaces {
    my $self = shift;
    return $self->[_spaces_];
}

sub get_marked {
    my $self = shift;
    return $self->[_marked_];
}

sub set_marked {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_marked_] = $value;
    }
    return $self->[_marked_];
}

sub get_available_spaces {
    my $self = shift;
    return $self->[_available_spaces_];
}

sub decrease_SPACES {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_spaces_] -= $value;
    }
    return $self->[_spaces_];
}

sub decrease_available_spaces {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_available_spaces_] -= $value;
    }
    return $self->[_available_spaces_];
}

sub get_align_paren {
    my $self = shift;
    return $self->[_align_paren_];
}

sub get_recoverable_spaces {
    my $self = shift;
    return $self->[_recoverable_spaces_];
}

sub set_recoverable_spaces {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_recoverable_spaces_] = $value;
    }
    return $self->[_recoverable_spaces_];
}

sub increase_recoverable_spaces {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_recoverable_spaces_] += $value;
    }
    return $self->[_recoverable_spaces_];
}

sub get_ci_level {
    my $self = shift;
    return $self->[_ci_level_];
}

sub get_level {
    my $self = shift;
    return $self->[_level_];
}

sub get_sequence_number {
    my $self = shift;
    return $self->[_sequence_number_];
}

sub get_index {
    my $self = shift;
    return $self->[_index_];
}

sub get_starting_index_K {
    my $self = shift;
    return $self->[_starting_index_K_];
}

sub set_have_child {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_have_child_] = $value;
    }
    return $self->[_have_child_];
}

sub get_have_child {
    my $self = shift;
    return $self->[_have_child_];
}

sub set_arrow_count {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_arrow_count_] = $value;
    }
    return $self->[_arrow_count_];
}

sub get_arrow_count {
    my $self = shift;
    return $self->[_arrow_count_];
}

sub set_comma_count {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_comma_count_] = $value;
    }
    return $self->[_comma_count_];
}

sub get_comma_count {
    my $self = shift;
    return $self->[_comma_count_];
}

sub set_closed {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_closed_] = $value;
    }
    return $self->[_closed_];
}

sub get_closed {
    my $self = shift;
    return $self->[_closed_];
}
1;
