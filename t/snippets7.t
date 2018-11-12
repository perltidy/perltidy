# Created with: ./make_t.pl

# Contents:
#1 rt102451.def
#2 rt104427.def
#3 rt106492.def
#4 rt107832.def
#5 rt107832.rt107832
#6 rt111519.def
#7 rt111519.rt111519
#8 rt112534.def
#9 rt113689.def
#10 rt113689.rt113689
#11 rt113792.def
#12 rt114359.def
#13 rt114909.def
#14 rt116344.def
#15 rt119140.def
#16 rt119588.def
#17 rt119970.def
#18 rt119970.rt119970
#19 rt123492.def
#20 rt123749.def

# To locate test #13 you can search for its name or the string '#13'

use strict;
use Test;
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
        'def'      => "",
        'rt107832' => <<'----------',
-lp
-boc
----------
        'rt111519' => <<'----------',
-io
-dac
----------
        'rt113689' => <<'----------',
-blao=2
-blbc=1
-blaol='*'
-blbcl='*'
----------
        'rt119970' => "-wn",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'rt102451' => <<'----------',
# RT#102451 bug test; unwanted spaces added before =head1 on each pass
#<<<

=head1 NAME

=cut

my %KA_CACHE; # indexed by uhost currently, points to [$handle...] array


=head1 NAME

=cut

#>>>
----------

        'rt104427' => <<'----------',
#!/usr/bin/env perl 
use v5.020;    #includes strict
use warnings;
use experimental 'signatures';
setidentifier();
exit;
sub setidentifier ( $href = {} ) { say 'hi'; }
----------

        'rt106492' => <<'----------',
my $ct = Courriel::Header::ContentType->new( mime_type => 'multipart/alternative', attributes => { boundary => unique_boundary }, );
----------

        'rt107832' => <<'----------',
my %temp = 
( 
supsup => 123, 
nested => { 
asdf => 456, 
yarg => 'yarp', 
}, );
----------

        'rt111519' => <<'----------',
use strict;
use warnings;
my $x = 1; # comment not removed
# comment will be removed
my $y = 2; # comment also not removed
----------

        'rt112534' => <<'----------',
get( on_ready => sub ($worker) { $on_ready->end; return; }, on_exit => sub ( $worker, $status ) { return; }, on_data => sub ($data) { $self->_on_data(@_) if $self; return; } );
----------

        'rt113689' => <<'----------',
$a = sub {
    if ( !defined( $_[0] ) ) {
        print("Hello, World\n");
    }
    else { print( $_[0], "\n" ); }
};
----------

        'rt113792' => <<'----------',
print "hello world\n";
__DATA__ 
=> 1/2 : 0.5 
----------

        'rt114359' => <<'----------',
my $x = 2; print $x ** 0.5;
----------

        'rt114909' => <<'----------',
#!perl
use strict;
use warnings;

use experimental 'signatures';

sub reader ( $line_sep, $chomp ) {
    return sub ( $fh, $out ) : prototype(*$) {
        local $/ = $line_sep;
        my $content = <$fh>;
        return undef unless defined $content;
        chomp $content if $chomp;
        $$out .= $content;
        return 1;
    };
}

BEGIN {
    *get_line = reader( "\n", 1 );
}

while ( get_line( STDIN, \my $buf ) ) {
    print "Got: $buf\n";
}
----------

        'rt116344' => <<'----------',
# Rt116344
# Attempting to tidy the following code failed:
sub broken {
    return ref {} ? 1 : 0;
    something();
}
----------

        'rt119140' => <<'----------',
while (<<>>) { }
----------

        'rt119588' => <<'----------',
sub demo {
    my $self     = shift;
    my $longname = shift // "xyz";
}
----------

        'rt119970' => <<'----------',
my $x = [
    {
        fooxx => 1,
        bar => 1,
    }
];
----------

        'rt123492' => <<'----------',
if (1) {
    print <<~EOF;
    Hello there
    EOF
}
----------

        'rt123749' => <<'----------',
get('http://mojolicious.org')->then(
    sub {
        my $mojo = shift;
        say $mojo->res->code;
        return get('http://metacpan.org');
    }
)->then(
    sub {
        my $cpan = shift;
        say $cpan->res->code;
    }
)->catch(
    sub {
        my $err = shift;
        warn "Something went wrong: $err";
    }
)->wait;
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'rt102451.def' => {
            source => "rt102451",
            params => "def",
            expect => <<'#1...........',
# RT#102451 bug test; unwanted spaces added before =head1 on each pass
#<<<

=head1 NAME

=cut

my %KA_CACHE; # indexed by uhost currently, points to [$handle...] array


=head1 NAME

=cut

#>>>
#1...........
        },

        'rt104427.def' => {
            source => "rt104427",
            params => "def",
            expect => <<'#2...........',
#!/usr/bin/env perl 
use v5.020;    #includes strict
use warnings;
use experimental 'signatures';
setidentifier();
exit;
sub setidentifier ( $href = {} ) { say 'hi'; }
#2...........
        },

        'rt106492.def' => {
            source => "rt106492",
            params => "def",
            expect => <<'#3...........',
my $ct = Courriel::Header::ContentType->new(
    mime_type  => 'multipart/alternative',
    attributes => { boundary => unique_boundary },
);
#3...........
        },

        'rt107832.def' => {
            source => "rt107832",
            params => "def",
            expect => <<'#4...........',
my %temp = (
    supsup => 123,
    nested => {
        asdf => 456,
        yarg => 'yarp',
    },
);
#4...........
        },

        'rt107832.rt107832' => {
            source => "rt107832",
            params => "rt107832",
            expect => <<'#5...........',
my %temp = (
    supsup => 123,
    nested => {
                asdf => 456,
                yarg => 'yarp',
    },
);
#5...........
        },

        'rt111519.def' => {
            source => "rt111519",
            params => "def",
            expect => <<'#6...........',
use strict;
use warnings;
my $x = 1;    # comment not removed

# comment will be removed
my $y = 2;    # comment also not removed
#6...........
        },

        'rt111519.rt111519' => {
            source => "rt111519",
            params => "rt111519",
            expect => <<'#7...........',
use strict;
use warnings;
my $x = 1;
my $y = 2;
#7...........
        },

        'rt112534.def' => {
            source => "rt112534",
            params => "def",
            expect => <<'#8...........',
get(
    on_ready => sub ($worker) { $on_ready->end; return; },
    on_exit  => sub ( $worker, $status ) {
        return;
    },
    on_data => sub ($data) { $self->_on_data(@_) if $self; return; }
);
#8...........
        },

        'rt113689.def' => {
            source => "rt113689",
            params => "def",
            expect => <<'#9...........',
$a = sub {
    if ( !defined( $_[0] ) ) {
        print("Hello, World\n");
    }
    else { print( $_[0], "\n" ); }
};
#9...........
        },

        'rt113689.rt113689' => {
            source => "rt113689",
            params => "rt113689",
            expect => <<'#10...........',
$a = sub {


    if ( !defined( $_[0] ) ) {


        print("Hello, World\n");

    }
    else { print( $_[0], "\n" ); }

};
#10...........
        },

        'rt113792.def' => {
            source => "rt113792",
            params => "def",
            expect => <<'#11...........',
print "hello world\n";
__DATA__ 
=> 1/2 : 0.5 
#11...........
        },

        'rt114359.def' => {
            source => "rt114359",
            params => "def",
            expect => <<'#12...........',
my $x = 2;
print $x **0.5;
#12...........
        },

        'rt114909.def' => {
            source => "rt114909",
            params => "def",
            expect => <<'#13...........',
#!perl
use strict;
use warnings;

use experimental 'signatures';

sub reader ( $line_sep, $chomp ) {
    return sub ( $fh, $out ) : prototype(*$) {
        local $/ = $line_sep;
        my $content = <$fh>;
        return undef unless defined $content;
        chomp $content if $chomp;
        $$out .= $content;
        return 1;
    };
}

BEGIN {
    *get_line = reader( "\n", 1 );
}

while ( get_line( STDIN, \my $buf ) ) {
    print "Got: $buf\n";
}
#13...........
        },

        'rt116344.def' => {
            source => "rt116344",
            params => "def",
            expect => <<'#14...........',
# Rt116344
# Attempting to tidy the following code failed:
sub broken {
    return ref {} ? 1 : 0;
    something();
}
#14...........
        },

        'rt119140.def' => {
            source => "rt119140",
            params => "def",
            expect => <<'#15...........',
while ( <<>> ) { }
#15...........
        },

        'rt119588.def' => {
            source => "rt119588",
            params => "def",
            expect => <<'#16...........',
sub demo {
    my $self     = shift;
    my $longname = shift // "xyz";
}
#16...........
        },

        'rt119970.def' => {
            source => "rt119970",
            params => "def",
            expect => <<'#17...........',
my $x = [
    {
        fooxx => 1,
        bar   => 1,
    }
];
#17...........
        },

        'rt119970.rt119970' => {
            source => "rt119970",
            params => "rt119970",
            expect => <<'#18...........',
my $x = [ {
    fooxx => 1,
    bar   => 1,
} ];
#18...........
        },

        'rt123492.def' => {
            source => "rt123492",
            params => "def",
            expect => <<'#19...........',
if (1) {
    print <<~EOF;
    Hello there
    EOF
}
#19...........
        },

        'rt123749.def' => {
            source => "rt123749",
            params => "def",
            expect => <<'#20...........',
get('http://mojolicious.org')->then(
    sub {
        my $mojo = shift;
        say $mojo->res->code;
        return get('http://metacpan.org');
    }
)->then(
    sub {
        my $cpan = shift;
        say $cpan->res->code;
    }
)->catch(
    sub {
        my $err = shift;
        warn "Something went wrong: $err";
    }
)->wait;
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
