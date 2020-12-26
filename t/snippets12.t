# Created with: ./make_t.pl

# Contents:
#1 vtc1.def
#2 vtc1.vtc
#3 vtc2.def
#4 vtc2.vtc
#5 vtc3.def
#6 vtc3.vtc
#7 vtc4.def
#8 vtc4.vtc
#9 wn1.def
#10 wn1.wn
#11 wn2.def
#12 wn2.wn
#13 wn3.def
#14 wn3.wn
#15 wn4.def
#16 wn4.wn
#17 wn5.def
#18 wn5.wn
#19 wn6.def
#20 wn6.wn

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test::More;
use Carp;
use Perl::Tidy;
my $rparams;
my $rsources;
my $rtests;

BEGIN {

    ###########################################
    # BEGIN SECTION 1: Parameter combinations #
    ###########################################
    $rparams = {
        'def' => "",
        'vtc' => <<'----------',
-sbvtc=2
-bvtc=2
-pvtc=2
----------
        'wn' => "-wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'vtc1' => <<'----------',
@lol = (
        [   'Dr. Watson', undef,    '221b', 'Baker St.',
            undef,        'London', 'NW1',  undef,
            'England',    undef
        ],
        [   'Sam Gamgee', undef,      undef, 'Bagshot Row',
            undef,        'Hobbiton', undef, undef,
            'The Shire',  undef],
        );
----------

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

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'vtc1.def' => {
            source => "vtc1",
            params => "def",
            expect => <<'#1...........',
@lol = (
    [
        'Dr. Watson', undef,    '221b', 'Baker St.',
        undef,        'London', 'NW1',  undef,
        'England',    undef
    ],
    [
        'Sam Gamgee', undef,      undef, 'Bagshot Row',
        undef,        'Hobbiton', undef, undef,
        'The Shire',  undef
    ],
);
#1...........
        },

        'vtc1.vtc' => {
            source => "vtc1",
            params => "vtc",
            expect => <<'#2...........',
@lol = (
    [
        'Dr. Watson', undef,    '221b', 'Baker St.',
        undef,        'London', 'NW1',  undef,
        'England',    undef ],
    [
        'Sam Gamgee', undef,      undef, 'Bagshot Row',
        undef,        'Hobbiton', undef, undef,
        'The Shire',  undef ], );
#2...........
        },

        'vtc2.def' => {
            source => "vtc2",
            params => "def",
            expect => <<'#3...........',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1
        )->result eq 'Alabama'
    );
#3...........
        },

        'vtc2.vtc' => {
            source => "vtc2",
            params => "vtc",
            expect => <<'#4...........',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1 )->result eq 'Alabama' );
#4...........
        },

        'vtc3.def' => {
            source => "vtc3",
            params => "def",
            expect => <<'#5...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
    )[$wday];
#5...........
        },

        'vtc3.vtc' => {
            source => "vtc3",
            params => "vtc",
            expect => <<'#6...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday" )[$wday];
#6...........
        },

        'vtc4.def' => {
            source => "vtc4",
            params => "def",
            expect => <<'#7...........',
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
#7...........
        },

        'vtc4.vtc' => {
            source => "vtc4",
            params => "vtc",
            expect => <<'#8...........',
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
#8...........
        },

        'wn1.def' => {
            source => "wn1",
            params => "def",
            expect => <<'#9...........',
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
#9...........
        },

        'wn1.wn' => {
            source => "wn1",
            params => "wn",
            expect => <<'#10...........',
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
#10...........
        },

        'wn2.def' => {
            source => "wn2",
            params => "def",
            expect => <<'#11...........',
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
#11...........
        },

        'wn2.wn' => {
            source => "wn2",
            params => "wn",
            expect => <<'#12...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols( [ qw(
        Perl_dump_fds
        Perl_ErrorNo
        Perl_GetVars
        PL_sys_intern
    ) ] );
}
#12...........
        },

        'wn3.def' => {
            source => "wn3",
            params => "def",
            expect => <<'#13...........',
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
#13...........
        },

        'wn3.wn' => {
            source => "wn3",
            params => "wn",
            expect => <<'#14...........',
deferred->resolve->then( sub {
    push @out, 'Resolve';
    return $then;
} )->then( sub {
    push @out, 'Reject';
    push @out, @_;
} );
#14...........
        },

        'wn4.def' => {
            source => "wn4",
            params => "def",
            expect => <<'#15...........',
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
#15...........
        },

        'wn4.wn' => {
            source => "wn4",
            params => "wn",
            expect => <<'#16...........',
{ { {

    # Orignal formatting looks nice but would be hard to duplicate
    return
         exists $G->{Attr}->{E}
      && exists $G->{Attr}->{E}->{$u} && exists $G->{Attr}->{E}->{$u}->{$v}
      ? %{ $G->{Attr}->{E}->{$u}->{$v} }
      : ();
} } }
#16...........
        },

        'wn5.def' => {
            source => "wn5",
            params => "def",
            expect => <<'#17...........',
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
#17...........
        },

        'wn5.wn' => {
            source => "wn5",
            params => "wn",
            expect => <<'#18...........',
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
#18...........
        },

        'wn6.def' => {
            source => "wn6",
            params => "def",
            expect => <<'#19...........',
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
#19...........
        },

        'wn6.wn' => {
            source => "wn6",
            params => "wn",
            expect => <<'#20...........',
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
#20...........
        },
    };

    my $ntests = 0 + keys %{$rtests};
    plan tests => $ntests;
}

###############
# EXECUTE TESTS
###############

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
        errorfile   => \$errorfile_string,    # not used when -se flag is set
    );
    if ( $err || $stderr_string || $errorfile_string ) {
        print STDERR "Error output received for test '$key'\n";
        if ($err) {
            print STDERR "An error flag '$err' was returned\n";
            ok( !$err );
        }
        if ($stderr_string) {
            print STDERR "---------------------\n";
            print STDERR "<<STDERR>>\n$stderr_string\n";
            print STDERR "---------------------\n";
            ok( !$stderr_string );
        }
        if ($errorfile_string) {
            print STDERR "---------------------\n";
            print STDERR "<<.ERR file>>\n$errorfile_string\n";
            print STDERR "---------------------\n";
            ok( !$errorfile_string );
        }
    }
    else {
        if ( !is( $output, $expect, $key ) ) {
            my $leno = length($output);
            my $lene = length($expect);
            if ( $leno == $lene ) {
                print STDERR
"#> Test '$key' gave unexpected output.  Strings differ but both have length $leno\n";
            }
            else {
                print STDERR
"#> Test '$key' gave unexpected output.  String lengths differ: output=$leno, expected=$lene\n";
            }
        }
    }
}
