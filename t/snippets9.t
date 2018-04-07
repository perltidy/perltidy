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
        'def'  => "",
        'vmll' => <<'----------',
-vmll
-bbt=2
-bt=2
-pt=2
-sbt=2
----------
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

        'version2' => <<'----------',
# On one line so MakeMaker will see it.
require Exporter; our $VERSION = $Exporter::VERSION;
----------

        'vert' => <<'----------',
# if $w->vert is tokenized as type 'U' then the ? will start a quote
# and an error will occur.
sub vert {
}
sub Restore {
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}
----------

        'vmll' => <<'----------',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {is_deeply(\@init_metas_called, [1]) || diag(Dumper(\@init_metas_called))}

    This has the comma on the next line
    exception {Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)},
----------

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
    };

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'version2.def' => {
            source => "version2",
            params => "def",
            expect => <<'#1...........',
# On one line so MakeMaker will see it.
require Exporter; our $VERSION = $Exporter::VERSION;
#1...........
        },

        'vert.def' => {
            source => "vert",
            params => "def",
            expect => <<'#2...........',
# if $w->vert is tokenized as type 'U' then the ? will start a quote
# and an error will occur.
sub vert {
}

sub Restore {
    $w->vert ? $w->delta_width(0) : $w->delta_height(0);
}
#2...........
        },

        'vmll.def' => {
            source => "vmll",
            params => "def",
            expect => <<'#3...........',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {
        is_deeply( \@init_metas_called, [1] )
          || diag( Dumper( \@init_metas_called ) );
    }

    This has the comma on the next line exception {
        Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)
    },
#3...........
        },

        'vmll.vmll' => {
            source => "vmll",
            params => "vmll",
            expect => <<'#4...........',
    # perltidy -act=2 -vmll will leave these intact and greater than 80 columns
    # in length, which is what vmll does
    BEGIN {is_deeply(\@init_metas_called, [1]) || diag(Dumper(\@init_metas_called))}

    This has the comma on the next line exception {
        Class::MOP::Class->initialize("NonExistent")->rebless_instance($foo)
    },
#4...........
        },

        'vtc1.def' => {
            source => "vtc1",
            params => "def",
            expect => <<'#5...........',
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
#5...........
        },

        'vtc1.vtc' => {
            source => "vtc1",
            params => "vtc",
            expect => <<'#6...........',
@lol = (
    [
        'Dr. Watson', undef,    '221b', 'Baker St.',
        undef,        'London', 'NW1',  undef,
        'England',    undef ],
    [
        'Sam Gamgee', undef,      undef, 'Bagshot Row',
        undef,        'Hobbiton', undef, undef,
        'The Shire',  undef ], );
#6...........
        },

        'vtc2.def' => {
            source => "vtc2",
            params => "def",
            expect => <<'#7...........',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1
        )->result eq 'Alabama'
    );
#7...........
        },

        'vtc2.vtc' => {
            source => "vtc2",
            params => "vtc",
            expect => <<'#8...........',
    ok(
        $s->call(
            SOAP::Data->name('getStateName')
              ->attr( { xmlns => 'urn:/My/Examples' } ),
            1 )->result eq 'Alabama' );
#8...........
        },

        'vtc3.def' => {
            source => "vtc3",
            params => "def",
            expect => <<'#9...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
    )[$wday];
#9...........
        },

        'vtc3.vtc' => {
            source => "vtc3",
            params => "vtc",
            expect => <<'#10...........',
    $day_long = (
        "Sunday",   "Monday", "Tuesday",  "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday" )[$wday];
#10...........
        },

        'vtc4.def' => {
            source => "vtc4",
            params => "def",
            expect => <<'#11...........',
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
#11...........
        },

        'vtc4.vtc' => {
            source => "vtc4",
            params => "vtc",
            expect => <<'#12...........',
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
#12...........
        },

        'wn1.def' => {
            source => "wn1",
            params => "def",
            expect => <<'#13...........',
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
#13...........
        },

        'wn1.wn' => {
            source => "wn1",
            params => "wn",
            expect => <<'#14...........',
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
#14...........
        },

        'wn2.def' => {
            source => "wn2",
            params => "def",
            expect => <<'#15...........',
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
#15...........
        },

        'wn2.wn' => {
            source => "wn2",
            params => "wn",
            expect => <<'#16...........',
if ( $PLATFORM eq 'aix' ) {
    skip_symbols( [ qw(
          Perl_dump_fds
          Perl_ErrorNo
          Perl_GetVars
          PL_sys_intern
          ) ] );
}
#16...........
        },

        'wn3.def' => {
            source => "wn3",
            params => "def",
            expect => <<'#17...........',
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
#17...........
        },

        'wn3.wn' => {
            source => "wn3",
            params => "wn",
            expect => <<'#18...........',
deferred->resolve->then( sub {
    push @out, 'Resolve';
    return $then;
} )->then( sub {
    push @out, 'Reject';
    push @out, @_;
} );
#18...........
        },

        'wn4.def' => {
            source => "wn4",
            params => "def",
            expect => <<'#19...........',
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
#19...........
        },

        'wn4.wn' => {
            source => "wn4",
            params => "wn",
            expect => <<'#20...........',
{ { {

    # Orignal formatting looks nice but would be hard to duplicate
    return
         exists $G->{Attr}->{E}
      && exists $G->{Attr}->{E}->{$u} && exists $G->{Attr}->{E}->{$u}->{$v}
      ? %{ $G->{Attr}->{E}->{$u}->{$v} }
      : ();
} } }
#20...........
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
