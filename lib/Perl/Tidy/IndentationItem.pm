#####################################################################
#
# the Perl::Tidy::IndentationItem class supplies items which contain
# how much whitespace should be used at the start of a line
#
#####################################################################

package Perl::Tidy::IndentationItem;
use strict;
use warnings;
our $VERSION = '20210111';

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

    # required to avoid call to AUTOLOAD in some versions of perl
}

sub new {

    # Create an 'indentation_item' which describes one level of leading
    # whitespace when the '-lp' indentation is used.
    my ( $class, %input_hash ) = @_;

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
    $self->[_spaces_]             = $input_hash{spaces};
    $self->[_level_]              = $input_hash{level};
    $self->[_ci_level_]           = $input_hash{ci_level};
    $self->[_available_spaces_]   = $input_hash{available_spaces};
    $self->[_closed_]             = -1;
    $self->[_comma_count_]        = 0;
    $self->[_sequence_number_]    = $input_hash{gnu_sequence_number};
    $self->[_index_]              = $input_hash{index};
    $self->[_have_child_]         = 0;
    $self->[_recoverable_spaces_] = 0;
    $self->[_align_paren_]        = $input_hash{align_paren};
    $self->[_marked_]             = 0;
    $self->[_stack_depth_]        = $input_hash{stack_depth};
    $self->[_starting_index_K_]   = $input_hash{starting_index_K};
    $self->[_arrow_count_]        = 0;

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
    return $_[0]->[_stack_depth_];
}

sub get_spaces {
    return $_[0]->[_spaces_];
}

sub get_marked {
    return $_[0]->[_marked_];
}

sub set_marked {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_marked_] = $value;
    }
    return $self->[_marked_];
}

sub get_available_spaces {
    return $_[0]->[_available_spaces_];
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
    return $_[0]->[_align_paren_];
}

sub get_recoverable_spaces {
    return $_[0]->[_recoverable_spaces_];
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
    return $_[0]->[_ci_level_];
}

sub get_level {
    return $_[0]->[_level_];
}

sub get_sequence_number {
    return $_[0]->[_sequence_number_];
}

sub get_index {
    return $_[0]->[_index_];
}

sub get_starting_index_K {
    return $_[0]->[_starting_index_K_];
}

sub set_have_child {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_have_child_] = $value;
    }
    return $self->[_have_child_];
}

sub get_have_child {
    return $_[0]->[_have_child_];
}

sub set_arrow_count {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_arrow_count_] = $value;
    }
    return $self->[_arrow_count_];
}

sub get_arrow_count {
    return $_[0]->[_arrow_count_];
}

sub set_comma_count {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_comma_count_] = $value;
    }
    return $self->[_comma_count_];
}

sub get_comma_count {
    return $_[0]->[_comma_count_];
}

sub set_closed {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_closed_] = $value;
    }
    return $self->[_closed_];
}

sub get_closed {
    return $_[0]->[_closed_];
}
1;
