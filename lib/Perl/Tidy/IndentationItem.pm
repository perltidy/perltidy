#####################################################################
#
# The Perl::Tidy::IndentationItem class supplies items which contain
# how much whitespace should be used at the start of a line
#
#####################################################################

package Perl::Tidy::IndentationItem;
use strict;
use warnings;

our $VERSION = '20250214.02';

BEGIN {

    # Array index names
    # Do not combine with other BEGIN blocks (c101).
    my $i = 0;
    use constant {
        _spaces_             => $i++,
        _level_              => $i++,
        _ci_level_           => $i++,
        _available_spaces_   => $i++,
        _closed_             => $i++,
        _comma_count_        => $i++,
        _lp_item_index_      => $i++,
        _have_child_         => $i++,
        _recoverable_spaces_ => $i++,
        _align_seqno_        => $i++,
        _marked_             => $i++,
        _K_begin_line_       => $i++,
        _arrow_count_        => $i++,
        _standard_spaces_    => $i++,
        _K_extra_space_      => $i++,
    };
} ## end BEGIN

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
    # lp_item_index     =>  # index in output batch list
    # have_child         =>  # any dependents?
    # recoverable_spaces =>  # how many spaces to the right
    #                        # we would like to move to get
    #                        # alignment (negative if left)
    # align_seqno        =>  # if we are aligning with an opening structure,
    #                        # this is its seqno
    # marked             =>  # if visited by corrector logic
    # K_begin_line   =>  # first token index K of this level
    # arrow_count        =>  # how many =>'s

    my $self = [];
    bless $self, $class;

    $self->[_spaces_]             = $input_hash{spaces};
    $self->[_level_]              = $input_hash{level};
    $self->[_ci_level_]           = $input_hash{ci_level};
    $self->[_available_spaces_]   = $input_hash{available_spaces};
    $self->[_closed_]             = -1;
    $self->[_comma_count_]        = 0;
    $self->[_lp_item_index_]      = $input_hash{lp_item_index};
    $self->[_have_child_]         = 0;
    $self->[_recoverable_spaces_] = 0;
    $self->[_align_seqno_]        = $input_hash{align_seqno};
    $self->[_marked_]             = 0;
    $self->[_K_begin_line_]       = $input_hash{K_begin_line};
    $self->[_arrow_count_]        = 0;
    $self->[_standard_spaces_]    = $input_hash{standard_spaces};
    $self->[_K_extra_space_]      = $input_hash{K_extra_space};

    return $self;
} ## end sub new

sub permanently_decrease_available_spaces {

    # make a permanent reduction in the available indentation spaces
    # at one indentation item.  NOTE: if there are child nodes, their
    # total SPACES must be reduced by the caller.

    my ( $self, $spaces_needed ) = @_;
    my $available_spaces = $self->get_available_spaces();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;

    # Fixed for c085; a zero value must remain unchanged unless the closed
    # flag has been set.
    my $closed = $self->get_closed();
    $self->decrease_available_spaces($deleted_spaces)
      if ( $available_spaces != 0 || $closed >= 0 );
    $self->decrease_SPACES($deleted_spaces);
    $self->set_recoverable_spaces(0);

    return $deleted_spaces;
} ## end sub permanently_decrease_available_spaces

sub tentatively_decrease_available_spaces {

    # We are asked to tentatively delete $spaces_needed of indentation
    # for an indentation item.  We may want to undo this later.  NOTE: if
    # there are child nodes, their total SPACES must be reduced by the
    # caller.
    my ( $self, $spaces_needed ) = @_;
    my $available_spaces = $self->get_available_spaces();
    my $deleted_spaces =
      ( $available_spaces > $spaces_needed )
      ? $spaces_needed
      : $available_spaces;
    $self->decrease_available_spaces($deleted_spaces);
    $self->decrease_SPACES($deleted_spaces);
    $self->increase_recoverable_spaces($deleted_spaces);
    return $deleted_spaces;
} ## end sub tentatively_decrease_available_spaces

# time-critical sub
sub get_spaces {
    return $_[0]->[_spaces_];
}

sub get_standard_spaces {
    my $self = shift;
    return $self->[_standard_spaces_];
}

# time-critical sub
sub get_marked {
    return $_[0]->[_marked_];
}

sub set_marked {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_marked_] = $value;
    }
    return $self->[_marked_];
} ## end sub set_marked

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
} ## end sub decrease_SPACES

sub decrease_available_spaces {
    my ( $self, $value ) = @_;

    if ( defined($value) ) {
        $self->[_available_spaces_] -= $value;
    }
    return $self->[_available_spaces_];
} ## end sub decrease_available_spaces

sub get_align_seqno {
    my $self = shift;
    return $self->[_align_seqno_];
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
} ## end sub set_recoverable_spaces

sub increase_recoverable_spaces {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_recoverable_spaces_] += $value;
    }
    return $self->[_recoverable_spaces_];
} ## end sub increase_recoverable_spaces

sub get_ci_level {
    my $self = shift;
    return $self->[_ci_level_];
}

sub get_level {
    my $self = shift;
    return $self->[_level_];
}

sub get_spaces_level_ci {
    my $self = shift;
    return [ $self->[_spaces_], $self->[_level_], $self->[_ci_level_] ];
}

sub get_lp_item_index {
    my $self = shift;
    return $self->[_lp_item_index_];
}

sub get_K_begin_line {
    my $self = shift;
    return $self->[_K_begin_line_];
}

sub get_K_extra_space {
    my $self = shift;
    return $self->[_K_extra_space_];
}

sub set_have_child {
    my ( $self, $value ) = @_;
    if ( defined($value) ) {
        $self->[_have_child_] = $value;
    }
    return $self->[_have_child_];
} ## end sub set_have_child

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
} ## end sub set_arrow_count

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
} ## end sub set_comma_count

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
} ## end sub set_closed

sub get_closed {
    my $self = shift;
    return $self->[_closed_];
}
1;
