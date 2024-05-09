#####################################################################
#
# The Perl::Tidy::Debugger class shows line tokenization
#
#####################################################################

package Perl::Tidy::Debugger;
use strict;
use warnings;
use English qw( -no_match_vars );
our $VERSION = '20240511';

use constant EMPTY_STRING => q{};
use constant SPACE        => q{ };

sub new {

    my ( $class, $filename, $is_encoded_data ) = @_;

    return bless {
        _debug_file        => $filename,
        _debug_file_opened => 0,
        _fh                => undef,
        _is_encoded_data   => $is_encoded_data,
    }, $class;
} ## end sub new

sub really_open_debug_file {

    my $self            = shift;
    my $debug_file      = $self->{_debug_file};
    my $is_encoded_data = $self->{_is_encoded_data};
    my $fh = Perl::Tidy::streamhandle( $debug_file, 'w', $is_encoded_data );
    if ( !$fh ) {
        Perl::Tidy::Warn("can't open debug file '$debug_file'\n");
    }
    $self->{_debug_file_opened} = 1;
    $self->{_fh}                = $fh;
    $fh->print(
        "Use -dump-token-types (-dtt) to get a list of token type codes\n");
    return;
} ## end sub really_open_debug_file

sub close_debug_file {

    my $self = shift;
    if ( $self->{_debug_file_opened} ) {
        my $fh         = $self->{_fh};
        my $debug_file = $self->{_debug_file};
        if (   $fh
            && $fh->can('close')
            && $debug_file ne '-'
            && !ref($debug_file) )
        {
            $fh->close()
              or Perl::Tidy::Warn(
                "can't close DEBUG file '$debug_file': $OS_ERROR\n");
        }
    }
    return;
} ## end sub close_debug_file

sub write_debug_entry {

    # This is a debug dump routine which may be modified as necessary
    # to dump tokens on a line-by-line basis.  The output will be written
    # to the .DEBUG file when the -D flag is entered.
    my ( $self, $line_of_tokens ) = @_;

    my $input_line = $line_of_tokens->{_line_text};

    my $rtoken_type = $line_of_tokens->{_rtoken_type};
    my $rtokens     = $line_of_tokens->{_rtokens};
    my $rlevels     = $line_of_tokens->{_rlevels};

    my $input_line_number = $line_of_tokens->{_line_number};
    my $line_type         = $line_of_tokens->{_line_type};

    my $token_str              = "$input_line_number: ";
    my $reconstructed_original = "$input_line_number: ";

    my $pattern   = EMPTY_STRING;
    my @next_char = ( '"', '"' );
    my $i_next    = 0;
    if ( !$self->{_debug_file_opened} ) {
        $self->really_open_debug_file();
    }
    my $fh = $self->{_fh};

    foreach my $j ( 0 .. @{$rtoken_type} - 1 ) {

        # testing patterns
        if ( $rtoken_type->[$j] eq 'k' ) {
            $pattern .= $rtokens->[$j];
        }
        else {
            $pattern .= $rtoken_type->[$j];
        }
        $reconstructed_original .= $rtokens->[$j];
        my $num      = length( $rtokens->[$j] );
        my $type_str = $rtoken_type->[$j];

        # be sure there are no blank tokens (shouldn't happen)
        # This can only happen if a programming error has been made
        # because all valid tokens are non-blank
        if ( $type_str eq SPACE ) {
            $fh->print("BLANK TOKEN on the next line\n");
            $type_str = $next_char[$i_next];
            $i_next   = 1 - $i_next;
        }

        if ( length($type_str) == 1 ) {
            $type_str = $type_str x $num;
        }
        $token_str .= $type_str;
    }

    # Write what you want here ...
    # $fh->print "$input_line\n";
    # $fh->print "$pattern\n";
    $fh->print("$reconstructed_original\n");
    $fh->print("$token_str\n");

    return;
} ## end sub write_debug_entry
1;
