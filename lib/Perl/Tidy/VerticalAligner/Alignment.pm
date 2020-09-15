#####################################################################
#
# the Perl::Tidy::VerticalAligner::Alignment class holds information
# on a single column being aligned
#
#####################################################################
package Perl::Tidy::VerticalAligner::Alignment;
use strict;
use warnings;
our $VERSION = '20200907.01';

{

    #    _column          # the current column number
    #    _starting_column # column number when created
    #    _saved_column    # a place for temporary storage

    my %default_data = (
        column          => undef,
        starting_column => undef,
        saved_column    => undef,
    );

    # class population count
    {
        my $_count = 0;
        sub get_count        { return $_count }
        sub _increment_count { return ++$_count }
        sub _decrement_count { return --$_count }
    }

    # constructor
    sub new {
        my ( $caller, %arg ) = @_;
        my $caller_is_obj = ref($caller);
        my $class         = $caller_is_obj || $caller;
        my $self          = bless {}, $class;

        foreach my $key ( keys %default_data ) {
            my $_key = '_' . $key;
            if    ( exists $arg{$key} ) { $self->{$_key} = $arg{$key} }
            elsif ($caller_is_obj)      { $self->{$_key} = $caller->{$_key} }
            else { $self->{$_key} = $default_data{$_key} }
        }
        if ( !defined( $self->{_starting_column} ) ) {
            $self->{_starting_column} = $self->{_column};
        }
        $self->_increment_count();
        return $self;
    }

    sub DESTROY {
        my $self = shift;
        $self->_decrement_count();
        return;
    }

    sub get_column { my $self = shift; return $self->{_column} }

    sub get_starting_column {
        my $self = shift;
        return $self->{_starting_column};
    }

    sub set_column { my ( $self, $val ) = @_; $self->{_column} = $val; return }

    sub set_starting_column {
        my ( $self, $val ) = @_;
        $self->{_starting_column} = $val;
        return;
    }

    sub increment_column {
        my ( $self, $val ) = @_;
        $self->{_column} += $val;
        return;
    }

    sub save_column {
        my $self = shift;
        $self->{_saved_column} = $self->{_column};
        return;
    }

    sub restore_column {
        my $self = shift;
        $self->{_column} = $self->{_saved_column};
        return;
    }
}

1;
