# Created with: ./make_t.pl

# Contents:
#1 angle.def
#2 arrows1.def
#3 arrows2.def
#4 attrib1.def
#5 attrib2.def
#6 attrib3.def
#7 bar1.bar
#8 bar1.def
#9 block1.def
#10 boc1.boc
#11 boc1.def
#12 boc2.boc
#13 boc2.def
#14 break1.def
#15 break2.def
#16 break3.def
#17 break4.def
#18 carat.def
#19 ce1.ce
#20 ce1.def

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
        'bar' => "-bar",
        'boc' => "-boc",
        'ce'  => "-cuddled-blocks",
        'def' => "",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'angle' => <<'----------',
# This is an angle operator:
@message_list =sort sort_algorithm < INDEX_FILE >;# angle operator

# Not an angle operator:
# Patched added in guess routine for this case:
if ( VERSION < 5.009 && $op->name eq 'aassign' ) {
}

----------

        'arrows1' => <<'----------',
# remove spaces around arrows
my $obj = Bio::Variation::AAChange -> new;
my $termcap = Term::Cap -> Tgetent( { TERM => undef } );
----------

        'arrows2' => <<'----------',
$_[ 0]-> Blue -> backColor(( $_[ 0]-> Blue -> backColor == cl::Blue ) ? cl::LightBlue  : cl::Blue );
----------

        'attrib1' => <<'----------',
sub be_careful () : locked method {
    my $self = shift;

    # ...
}
----------

        'attrib2' => <<'----------',
sub 
witch 
()   # prototype may be on new line, but cannot put line break within prototype
: 
locked 
{
	print "and your little dog ";
}
----------

        'attrib3' => <<'----------',
package Canine;
package Dog;
my Canine $spot : Watchful ;  
package Felis;
my $cat : Nervous;
package X;
sub foo : locked ;  
package X;
sub Y::x : locked { 1 }  
package X;
sub foo { 1 }
package Y;
BEGIN { *bar = \&X::foo; }
package Z;
sub Y::bar : locked ;  
----------

        'bar1' => <<'----------',
if ($bigwasteofspace1 && $bigwasteofspace2 || $bigwasteofspace3 && $bigwasteofspace4) { }
----------

        'block1' => <<'----------',
# Some block tests
print "start main running\n";
die "main now dying\n";
END {$a=6; print "1st end, a=$a\n"} 
CHECK {$a=8; print "1st check, a=$a\n"} 
INIT {$a=10; print "1st init, a=$a\n"} 
END {$a=12; print "2nd end, a=$a\n"} 
BEGIN {$a=14; print "1st begin, a=$a\n"} 
INIT {$a=16; print "2nd init, a=$a\n"} 
BEGIN {$a=18; print "2nd begin, a=$a\n"} 
CHECK {$a=20; print "2nd check, a=$a\n"} 
END {$a=23; print "3rd end, a=$a\n"} 

----------

        'boc1' => <<'----------',
# RT#98902
# Running with -boc (break-at-old-comma-breakpoints) should not
# allow forming a single line
my @bar = map { {
     number => $_,
     character => chr $_,
     padding => (' ' x $_),
} } ( 0 .. 32 );
----------

        'boc2' => <<'----------',
my @list = (
    1,
    1, 1,
    1, 2, 1,
    1, 3, 3, 1,
    1, 4, 6, 4, 1,);

----------

        'break1' => <<'----------',
    # break at ;
    $self->__print("*** Type 'p' now to show start up log\n") ;    # XXX add to banner?
----------

        'break2' => <<'----------',
        # break before the '->'
        ( $current_feature_item->children )[0]->set( $current_feature->primary_tag );
        $sth->{'Database'}->{'xbase_tables'}->{ $parsed_sql->{'table'}[0] }->field_type($_);
----------

        'break3' => <<'----------',
    # keep the anonymous hash block together:
    my $red_color = $widget->window->get_colormap->color_alloc( { red => 65000, green => 0, blue => 0 } );
----------

        'break4' => <<'----------',
        spawn( "$LINTIAN_ROOT/unpack/list-binpkg", "$LINTIAN_LAB/info/binary-packages", $v ) == 0 or fail("cannot create binary package list"); 
----------

        'carat' => <<'----------',
my $a=${^WARNING_BITS};
@{^HOWDY_PARDNER}=(101,102);
${^W} = 1;
$bb[$^]] = "bubba";
----------

        'ce1' => <<'----------',
# test -ce with blank lines and comments between blocks
if($value[0] =~ /^(\#)/){    # skip any comment line
  last SWITCH;
}


elsif($value[0] =~ /^(o)$/ or $value[0] =~ /^(os)$/){
  $os=$value[1];
  last SWITCH;
}

elsif($value[0] =~ /^(b)$/ or $value[0] =~ /^(dbfile)$/)

# comment
{
  $dbfile=$value[1];
  last SWITCH;
# Add the additional site
}else{
	$rebase_hash{$name} .= " $site";
}
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'angle.def' => {
            source => "angle",
            params => "def",
            expect => <<'#1...........',
# This is an angle operator:
@message_list = sort sort_algorithm < INDEX_FILE >;    # angle operator

# Not an angle operator:
# Patched added in guess routine for this case:
if ( VERSION < 5.009 && $op->name eq 'aassign' ) {
}

#1...........
        },

        'arrows1.def' => {
            source => "arrows1",
            params => "def",
            expect => <<'#2...........',
# remove spaces around arrows
my $obj     = Bio::Variation::AAChange->new;
my $termcap = Term::Cap->Tgetent( { TERM => undef } );
#2...........
        },

        'arrows2.def' => {
            source => "arrows2",
            params => "def",
            expect => <<'#3...........',
$_[0]->Blue->backColor(
    ( $_[0]->Blue->backColor == cl::Blue ) ? cl::LightBlue : cl::Blue );
#3...........
        },

        'attrib1.def' => {
            source => "attrib1",
            params => "def",
            expect => <<'#4...........',
sub be_careful () : locked method {
    my $self = shift;

    # ...
}
#4...........
        },

        'attrib2.def' => {
            source => "attrib2",
            params => "def",
            expect => <<'#5...........',
sub witch
  ()  # prototype may be on new line, but cannot put line break within prototype
  : locked {
    print "and your little dog ";
}
#5...........
        },

        'attrib3.def' => {
            source => "attrib3",
            params => "def",
            expect => <<'#6...........',
package Canine;

package Dog;
my Canine $spot : Watchful;

package Felis;
my $cat : Nervous;

package X;
sub foo : locked;

package X;
sub Y::x : locked { 1 }

package X;
sub foo { 1 }

package Y;
BEGIN { *bar = \&X::foo; }

package Z;
sub Y::bar : locked;
#6...........
        },

        'bar1.bar' => {
            source => "bar1",
            params => "bar",
            expect => <<'#7...........',
if (   $bigwasteofspace1 && $bigwasteofspace2
    || $bigwasteofspace3 && $bigwasteofspace4 ) {
}
#7...........
        },

        'bar1.def' => {
            source => "bar1",
            params => "def",
            expect => <<'#8...........',
if (   $bigwasteofspace1 && $bigwasteofspace2
    || $bigwasteofspace3 && $bigwasteofspace4 )
{
}
#8...........
        },

        'block1.def' => {
            source => "block1",
            params => "def",
            expect => <<'#9...........',
# Some block tests
print "start main running\n";
die "main now dying\n";
END   { $a = 6;  print "1st end, a=$a\n" }
CHECK { $a = 8;  print "1st check, a=$a\n" }
INIT  { $a = 10; print "1st init, a=$a\n" }
END   { $a = 12; print "2nd end, a=$a\n" }
BEGIN { $a = 14; print "1st begin, a=$a\n" }
INIT  { $a = 16; print "2nd init, a=$a\n" }
BEGIN { $a = 18; print "2nd begin, a=$a\n" }
CHECK { $a = 20; print "2nd check, a=$a\n" }
END   { $a = 23; print "3rd end, a=$a\n" }

#9...........
        },

        'boc1.boc' => {
            source => "boc1",
            params => "boc",
            expect => <<'#10...........',
# RT#98902
# Running with -boc (break-at-old-comma-breakpoints) should not
# allow forming a single line
my @bar = map {
    {
        number    => $_,
        character => chr $_,
        padding   => ( ' ' x $_ ),
    }
} ( 0 .. 32 );
#10...........
        },

        'boc1.def' => {
            source => "boc1",
            params => "def",
            expect => <<'#11...........',
# RT#98902
# Running with -boc (break-at-old-comma-breakpoints) should not
# allow forming a single line
my @bar =
  map { { number => $_, character => chr $_, padding => ( ' ' x $_ ), } }
  ( 0 .. 32 );
#11...........
        },

        'boc2.boc' => {
            source => "boc2",
            params => "boc",
            expect => <<'#12...........',
my @list = (
    1,
    1, 1,
    1, 2, 1,
    1, 3, 3, 1,
    1, 4, 6, 4, 1,
);

#12...........
        },

        'boc2.def' => {
            source => "boc2",
            params => "def",
            expect => <<'#13...........',
my @list = ( 1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1, );

#13...........
        },

        'break1.def' => {
            source => "break1",
            params => "def",
            expect => <<'#14...........',
    # break at ;
    $self->__print("*** Type 'p' now to show start up log\n")
      ;    # XXX add to banner?
#14...........
        },

        'break2.def' => {
            source => "break2",
            params => "def",
            expect => <<'#15...........',
        # break before the '->'
        ( $current_feature_item->children )[0]
          ->set( $current_feature->primary_tag );
        $sth->{'Database'}->{'xbase_tables'}->{ $parsed_sql->{'table'}[0] }
          ->field_type($_);
#15...........
        },

        'break3.def' => {
            source => "break3",
            params => "def",
            expect => <<'#16...........',
    # keep the anonymous hash block together:
    my $red_color = $widget->window->get_colormap->color_alloc(
        { red => 65000, green => 0, blue => 0 } );
#16...........
        },

        'break4.def' => {
            source => "break4",
            params => "def",
            expect => <<'#17...........',
        spawn( "$LINTIAN_ROOT/unpack/list-binpkg",
            "$LINTIAN_LAB/info/binary-packages", $v ) == 0
          or fail("cannot create binary package list");
#17...........
        },

        'carat.def' => {
            source => "carat",
            params => "def",
            expect => <<'#18...........',
my $a = ${^WARNING_BITS};
@{^HOWDY_PARDNER} = ( 101, 102 );
${^W}             = 1;
$bb[$^]] = "bubba";
#18...........
        },

        'ce1.ce' => {
            source => "ce1",
            params => "ce",
            expect => <<'#19...........',
# test -ce with blank lines and comments between blocks
if ( $value[0] =~ /^(\#)/ ) {    # skip any comment line
    last SWITCH;

} elsif ( $value[0] =~ /^(o)$/ or $value[0] =~ /^(os)$/ ) {
    $os = $value[1];
    last SWITCH;

} elsif ( $value[0] =~ /^(b)$/ or $value[0] =~ /^(dbfile)$/ )

  # comment
{
    $dbfile = $value[1];
    last SWITCH;

    # Add the additional site
} else {
    $rebase_hash{$name} .= " $site";
}
#19...........
        },

        'ce1.def' => {
            source => "ce1",
            params => "def",
            expect => <<'#20...........',
# test -ce with blank lines and comments between blocks
if ( $value[0] =~ /^(\#)/ ) {    # skip any comment line
    last SWITCH;
}

elsif ( $value[0] =~ /^(o)$/ or $value[0] =~ /^(os)$/ ) {
    $os = $value[1];
    last SWITCH;
}

elsif ( $value[0] =~ /^(b)$/ or $value[0] =~ /^(dbfile)$/ )

  # comment
{
    $dbfile = $value[1];
    last SWITCH;

    # Add the additional site
}
else {
    $rebase_hash{$name} .= " $site";
}
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
