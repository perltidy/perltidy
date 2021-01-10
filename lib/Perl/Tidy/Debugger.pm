#####################################################################
#
# The Perl::Tidy::Debugger class shows line tokenization
#
#####################################################################

package Perl::Tidy::Debugger;
use strict;
use warnings;
our $VERSION = '20210111';

sub new {

    my ( $class, $filename, $is_encoded_data ) = @_;

    return bless {
        _debug_file        => $filename,
        _debug_file_opened => 0,
        _fh                => undef,
        _is_encoded_data   => $is_encoded_data,
    }, $class;
}

sub really_open_debug_file {

    my $self            = shift;
    my $debug_file      = $self->{_debug_file};
    my $is_encoded_data = $self->{_is_encoded_data};
    my ( $fh, $filename ) =
      Perl::Tidy::streamhandle( $debug_file, 'w', $is_encoded_data );
    if ( !$fh ) {
        Perl::Tidy::Warn("can't open $debug_file: $!\n");
    }
    $self->{_debug_file_opened} = 1;
    $self->{_fh}                = $fh;
    $fh->print(
        "Use -dump-token-types (-dtt) to get a list of token type codes\n");
    return;
}

sub close_debug_file {

    my $self = shift;
    my $fh   = $self->{_fh};
    if ( $self->{_debug_file_opened} ) {
        if ( !eval { $self->{_fh}->close(); 1 } ) {

            # ok, maybe no close function
        }
    }
    return;
}

sub write_debug_entry {

    # This is a debug dump routine which may be modified as necessary
    # to dump tokens on a line-by-line basis.  The output will be written
    # to the .DEBUG file when the -D flag is entered.
    my ( $self, $line_of_tokens ) = @_;

    my $input_line = $line_of_tokens->{_line_text};

    my $rtoken_type = $line_of_tokens->{_rtoken_type};
    my $rtokens     = $line_of_tokens->{_rtokens};
    my $rlevels     = $line_of_tokens->{_rlevels};
    my $rslevels    = $line_of_tokens->{_rslevels};
    my $rblock_type = $line_of_tokens->{_rblock_type};

    my $input_line_number = $line_of_tokens->{_line_number};
    my $line_type         = $line_of_tokens->{_line_type};

    my ( $j, $num );

    my $token_str              = "$input_line_number: ";
    my $reconstructed_original = "$input_line_number: ";
    my $block_str              = "$input_line_number: ";

    my $pattern   = "";
    my @next_char = ( '"', '"' );
    my $i_next    = 0;
    unless ( $self->{_debug_file_opened} ) { $self->really_open_debug_file() }
    my $fh = $self->{_fh};

    # FIXME: could convert to use of token_array instead
    foreach my $j ( 0 .. @{$rtoken_type} - 1 ) {

        # testing patterns
        if ( $rtoken_type->[$j] eq 'k' ) {
            $pattern .= $rtokens->[$j];
        }
        else {
            $pattern .= $rtoken_type->[$j];
        }
        $reconstructed_original .= $rtokens->[$j];
        $block_str              .= "($rblock_type->[$j])";
        $num = length( $rtokens->[$j] );
        my $type_str = $rtoken_type->[$j];

        # be sure there are no blank tokens (shouldn't happen)
        # This can only happen if a programming error has been made
        # because all valid tokens are non-blank
        if ( $type_str eq ' ' ) {
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
}
1;
