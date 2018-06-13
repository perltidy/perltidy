# **This script was automatically generated**
# Created with: ./make_t.pl
# Tue Jun 12 19:09:24 2018

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
        'vtc' => <<'----------',
-sbvtc=2
-bvtc=2
-pvtc=2
----------
        'wn' => "-wn",
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

        'vtc2' => <<'----------',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1
        )->result eq 'Alabama'
    );
----------

        'vtc3' => <<'----------',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
    )[$wday];
----------

        'vtc4' => <<'----------',
my$bg_color=$im->colorAllocate(unpack('C3',pack('H2H2H2',unpack('a2a2a2',(length($options_r->{'bg_color'})?$options_r->{'bg_color'}:$MIDI::Opus::BG_color)))));
----------

        'wn1' => <<'----------',
    my $bg_color = $im->colorAllocate(
        unpack(
            'C3',
            pack(
                'H2H2H2',
                unpack(
                    'a2a2a2',
                    (
                        length( $options_r->{'bg_color'} )
                        ? $options_r->{'bg_color'}
                        : $MIDI::Opus::BG_color
                    )
                )
            )
        )
    );
----------

        'wn2' => <<'----------',
if ($PLATFORM eq 'aix') {
    skip_symbols([qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
    )]);
}
----------

        'wn3' => <<'----------',
deferred->resolve->then(
    sub {
        push @out, 'Resolve';
        return $then;
    }
)->then(
    sub {
        push @out, 'Reject';
        push @out, @_;
    }
);
----------

        'wn4' => <<'----------',
{{{
            # Orignal formatting looks nice but would be hard to duplicate
            return exists $G->{ Attr }->{ E } &&
                   exists $G->{ Attr }->{ E }->{ $u } &&
                   exists $G->{ Attr }->{ E }->{ $u }->{ $v } ?
                              %{ $G->{ Attr }->{ E }->{ $u }->{ $v } } :
                              ( );
}}}
----------

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

        'vtc2.vtc' => {
            source => "vtc2",
            params => "vtc",
            expect => <<'#1...........',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1 )->result eq 'Alabama' );
#1...........
        },

        'vtc3.def' => {
            source => "vtc3",
            params => "def",
            expect => <<'#2...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
    )[$wday];
#2...........
        },

        'vtc3.vtc' => {
            source => "vtc3",
            params => "vtc",
            expect => <<'#3...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday" )[$wday];
#3...........
        },

        'vtc4.def' => {
            source => "vtc4",
            params => "def",
            expect => <<'#4...........',
my $bg_color = $im->colorAllocate(
    unpack(
        'C3',
        pack(
            'H2H2H2',
            unpack(
                'a2a2a2',
                (
                    length( $options_r->{'bg_color'} )
                    ? $options_r->{'bg_color'}
                    : $MIDI::Opus::BG_color
                )
            )
        )
    )
);
#4...........
        },

        'vtc4.vtc' => {
            source => "vtc4",
            params => "vtc",
            expect => <<'#5...........',
my $bg_color = $im->colorAllocate(
    unpack(
        'C3',
        pack(
            'H2H2H2',
            unpack(
                'a2a2a2',
                (
                    length( $options_r->{'bg_color'} )
                    ? $options_r->{'bg_color'}
                    : $MIDI::Opus::BG_color ) ) ) ) );
#5...........
        },

        'wn1.def' => {
            source => "wn1",
            params => "def",
            expect => <<'#6...........',
    my $bg_color = $im->colorAllocate(
        unpack(
            'C3',
            pack(
                'H2H2H2',
                unpack(
                    'a2a2a2',
                    (
                        length( $options_r->{'bg_color'} )
                        ? $options_r->{'bg_color'}
                        : $MIDI::Opus::BG_color
                    )
                )
            )
        )
    );
#6...........
        },

        'wn1.wn' => {
            source => "wn1",
            params => "wn",
            expect => <<'#7...........',
    my $bg_color = $im->colorAllocate( unpack(
        'C3',
        pack(
            'H2H2H2',
            unpack(
                'a2a2a2',
                (
                    length( $options_r->{'bg_color'} )
                    ? $options_r->{'bg_color'}
                    : $MIDI::Opus::BG_color
                )
            )
        )
    ) );
#7...........
        },

        'wn2.def' => {
            source => "wn2",
            params => "def",
            expect => <<'#8...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols(
        [
            qw(
              Perl_dump_fds
              Perl_ErrorNo
              Perl_GetVars
              PL_sys_intern
              )
        ]
    );
}
#8...........
        },

        'wn2.wn' => {
            source => "wn2",
            params => "wn",
            expect => <<'#9...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols( [ qw(
          Perl_dump_fds
          Perl_ErrorNo
          Perl_GetVars
          PL_sys_intern
          ) ] );
}
#9...........
        },

        'wn3.def' => {
            source => "wn3",
            params => "def",
            expect => <<'#10...........',
deferred->resolve->then(
    sub {
        push @out, 'Resolve';
        return $then;
    }
)->then(
    sub {
        push @out, 'Reject';
        push @out, @_;
    }
);
#10...........
        },

        'wn3.wn' => {
            source => "wn3",
            params => "wn",
            expect => <<'#11...........',
deferred->resolve->then( sub {
    push @out, 'Resolve';
    return $then;
} )->then( sub {
    push @out, 'Reject';
    push @out, @_;
} );
#11...........
        },

        'wn4.def' => {
            source => "wn4",
            params => "def",
            expect => <<'#12...........',
{
    {
        {
            # Orignal formatting looks nice but would be hard to duplicate
            return
                 exists $G->{Attr}->{E}
              && exists $G->{Attr}->{E}->{$u}
              && exists $G->{Attr}->{E}->{$u}->{$v}
              ? %{ $G->{Attr}->{E}->{$u}->{$v} }
              : ();
        }
    }
}
#12...........
        },

        'wn4.wn' => {
            source => "wn4",
            params => "wn",
            expect => <<'#13...........',
{ { {

    # Orignal formatting looks nice but would be hard to duplicate
    return
         exists $G->{Attr}->{E}
      && exists $G->{Attr}->{E}->{$u} && exists $G->{Attr}->{E}->{$u}->{$v}
      ? %{ $G->{Attr}->{E}->{$u}->{$v} }
      : ();
} } }
#13...........
        },

        'wn5.def' => {
            source => "wn5",
            params => "def",
            expect => <<'#14...........',
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
#14...........
        },

        'wn5.wn' => {
            source => "wn5",
            params => "wn",
            expect => <<'#15...........',
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
#15...........
        },

        'wn6.def' => {
            source => "wn6",
            params => "def",
            expect => <<'#16...........',
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
#16...........
        },

        'wn6.wn' => {
            source => "wn6",
            params => "wn",
            expect => <<'#17...........',
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
#17...........
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
