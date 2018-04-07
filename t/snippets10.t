# **This script was automatically generated**
# Created with: ./make_t.pl
# Thu Apr  5 07:31:24 2018

# To locate test #13 for example, search for the string '#13'

use strict;
use Test;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    #####################################
    # SECTION 1: Parameter combinations #
    #####################################
    $rparams = {
        'def' => "",
        'wn'  => "-wn",
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

        'wn5' => <<'----------',
# qw weld with -wn
use_all_ok(
 qw{
   PPI
   PPI::Tokenizer
   PPI::Lexer
   PPI::Dumper
   PPI::Find
   PPI::Normal
   PPI::Util
   PPI::Cache
   }
);
----------

        'wn6' => <<'----------',
	    # illustration of some do-not-weld rules
	
    	    # do not weld a two-line function call
            $trans->add_transformation( PDL::Graphics::TriD::Scale->new( $sx, $sy, $sz ) );
        
            # but weld this more complex statement
            my $compass = uc( opposite_direction( line_to_canvas_direction(
                @{ $coords[0] }, @{ $coords[1] } ) ) );
        
            # do not weld to a one-line block because the function could get separated
	    # from its opening paren 
            $_[0]->code_handler
                 ( sub { $morexxxxxxxxxxxxxxxxxx .= $_[1] . ":" . $_[0] . "\n" } );

	    # another example; do not weld because the sub is not broken
            $wrapped->add_around_modifier( 
		sub { push @tracelog => 'around 1'; $_[0]->(); } );

	    # but okay to weld here because the sub is broken
            $wrapped->add_around_modifier( sub { 
			push @tracelog => 'around 1'; $_[0]->(); } );
----------
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'wn5.def' => {
            source => "wn5",
            params => "def",
            expect => <<'#1...........',
# qw weld with -wn
use_all_ok(
    qw{
      PPI
      PPI::Tokenizer
      PPI::Lexer
      PPI::Dumper
      PPI::Find
      PPI::Normal
      PPI::Util
      PPI::Cache
      }
);
#1...........
        },

        'wn5.wn' => {
            source => "wn5",
            params => "wn",
            expect => <<'#2...........',
# qw weld with -wn
use_all_ok( qw{
      PPI
      PPI::Tokenizer
      PPI::Lexer
      PPI::Dumper
      PPI::Find
      PPI::Normal
      PPI::Util
      PPI::Cache
      } );
#2...........
        },

        'wn6.def' => {
            source => "wn6",
            params => "def",
            expect => <<'#3...........',
            # illustration of some do-not-weld rules

            # do not weld a two-line function call
            $trans->add_transformation(
                PDL::Graphics::TriD::Scale->new( $sx, $sy, $sz ) );

            # but weld this more complex statement
            my $compass = uc(
                opposite_direction(
                    line_to_canvas_direction(
                        @{ $coords[0] }, @{ $coords[1] }
                    )
                )
            );

      # do not weld to a one-line block because the function could get separated
      # from its opening paren
            $_[0]->code_handler(
                sub { $morexxxxxxxxxxxxxxxxxx .= $_[1] . ":" . $_[0] . "\n" } );

            # another example; do not weld because the sub is not broken
            $wrapped->add_around_modifier(
                sub { push @tracelog => 'around 1'; $_[0]->(); } );

            # but okay to weld here because the sub is broken
            $wrapped->add_around_modifier(
                sub {
                    push @tracelog => 'around 1';
                    $_[0]->();
                }
            );
#3...........
        },

        'wn6.wn' => {
            source => "wn6",
            params => "wn",
            expect => <<'#4...........',
            # illustration of some do-not-weld rules

            # do not weld a two-line function call
            $trans->add_transformation(
                PDL::Graphics::TriD::Scale->new( $sx, $sy, $sz ) );

            # but weld this more complex statement
            my $compass = uc( opposite_direction( line_to_canvas_direction(
                @{ $coords[0] }, @{ $coords[1] }
            ) ) );

      # do not weld to a one-line block because the function could get separated
      # from its opening paren
            $_[0]->code_handler(
                sub { $morexxxxxxxxxxxxxxxxxx .= $_[1] . ":" . $_[0] . "\n" } );

            # another example; do not weld because the sub is not broken
            $wrapped->add_around_modifier(
                sub { push @tracelog => 'around 1'; $_[0]->(); } );

            # but okay to weld here because the sub is broken
            $wrapped->add_around_modifier( sub {
                push @tracelog => 'around 1';
                $_[0]->();
            } );
#4...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

foreach my $key ( sort keys %{$rtests} ) {
    my $output;
    my $sname  = $rtests->{$key}->{source};
    my $expect = $rtests->{$key}->{expect};
    my $pname  = $rtests->{$key}->{params};
    my $source = $rsources->{$sname};
    my $params = defined($pname) ? $rparams->{$pname} : "";
    my $stderr_string;
    my $errorfile_string;
    my $err = Perl::Tidy::perltidy(
        source      => \$source,
        destination => \$output,
        perltidyrc  => \$params,
        argv        => '',             # for safety; hide any ARGV from perltidy
        stderr      => \$stderr_string,
        errorfile => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        if ($err) {
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            print STDERR
"This error received calling Perl::Tidy with '$sname' + '$pname'\n";
            ok( !$errorfile_string );
        }
    }
    else {
        ok( $output, $expect );
    }
}
