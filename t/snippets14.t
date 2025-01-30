# Created with: ./make_t.pl

# Contents:
#1 else1.def
#2 else2.def
#3 ternary3.def
#4 align17.def
#5 align18.def
#6 kgb1.def
#7 kgb1.kgb
#8 kgb2.def
#9 kgb2.kgb
#10 kgb3.def
#11 kgb3.kgb
#12 kgb4.def
#13 kgb4.kgb
#14 kgb5.def
#15 kgb5.kgb
#16 kgbd.def
#17 kgbd.kgbd
#18 kgb_tight.def
#19 gnu5.def

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
        'def'  => "",
        'kgb'  => "-kgb",
        'kgbd' => "-kgbd -kgb",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'align17' => <<'----------',
# align => even at broken sub block
my%opt=(
'cc'=>sub{$param::cachecom=1;},
'cd'=>sub{$param::cachedisable=1;},
'p'=>sub{
$param::pflag=1;
$param::build=0;
}
);
----------

        'align18' => <<'----------',
#align '&&'
for($ENV{HTTP_USER_AGENT}){
$page=
/Mac/&&'m/Macintrash.html'
||/Win(dows)?NT/&&'e/evilandrude.html'
||/Win|MSIE|WebTV/&&'m/MicroslothWindows.html'
||/Linux/&&'l/Linux.html'
||/HP-UX/&&'h/HP-SUX.html'
||/SunOS/&&'s/ScumOS.html'
||'a/AppendixB.html';
}
----------

        'else1' => <<'----------',
# pad after 'if' when followed by 'elsif'
if    ( not defined $dir or not length $dir ) { $rslt = ''; }
elsif ( $dir =~ /^\$\([^\)]+\)\Z(?!\n)/s )    { $rslt = $dir; }
else                                          { $rslt = vmspath($dir); }
----------

        'else2' => <<'----------',
	# no pad after 'if' when followed by 'else'
        if ( $m = $g[$x][$y] ) { print $$m{v}; $$m{i}->() }
        else                   { print " " }
----------

        'gnu5' => <<'----------',
        # side comments limit gnu type formatting with l=80; note extra comma
        push @tests, [
            "Lowest code point requiring 13 bytes to represent",    # 2**36
            "\xff\x80\x80\x80\x80\x80\x81\x80\x80\x80\x80\x80\x80",
            ($::is64bit) ? 0x1000000000 : -1,    # overflows on 32bit
          ],
          ;
----------

        'kgb1' => <<'----------',
# a variety of line types for testing -kgb
use strict;
use Test;
use Encode qw(from_to encode decode
  encode_utf8 decode_utf8
  find_encoding is_utf8);
use charnames qw(greek);
our $targetdir = "/usr/local/doc/HTML/Perl";
local (
    $tocfile,   $loffile,   $lotfile,         $footfile,
    $citefile,  $idxfile,   $figure_captions, $table_captions,
    $footnotes, $citations, %font_size,       %index,
    %done,      $t_title,   $t_author,        $t_date,
    $t_address, $t_affil,   $changed
);
my @UNITCHECKs =
    B::unitcheck_av->isa("B::AV")
  ? B::unitcheck_av->ARRAY
  : ();
my @CHECKs = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
my $dna  = Bio::LiveSeq::DNA->new( -seq => $dnasequence );
my $min  = 1;
my $max  = length($dnasequence);
my $T = $G->_strongly_connected;
my %R = $T->vertex_roots;
my @C;    # We're not calling the strongly_connected_components()
	  # Do not separate this hanging side comment from previous
my $G = shift;
my $exon = Bio::LiveSeq::Exon->new(
    -seq    => $dna,
    -start  => $min,
    -end    => $max,
    -strand => 1
);
my $octal_mode;
my @inputs = (
    0777, 0700, 0470, 0407, 0433, 0400, 0430, 0403, 0111, 0100,
    0110, 0101, 0731, 0713, 0317, 0371, 0173, 0137
);
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) +
  ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
my $r = q{
pm_to_blib: $(TO_INST_PM)
};
my $regcomp_re =
  "(?<routine>ckWARN(?:\\d+)?reg\\w*|vWARN\\d+|$regcomp_fail_re)";
my $position = List::MoreUtils::firstidx {
    refaddr $_ == $key
}
my @exons = ($exon);
my $fastafile2 = "/tmp/tmpfastafile2";
my $grepcut = 'egrep -v "[[:digit:]]|^ *$|sequences" | cut -c8-'; # grep/cut
my $alignprogram =
"/usr/local/etc/bioinfo/fasta2/align -s /usr/local/etc/bioinfo/fasta2/idnaa.mat $fastafile1 $fastafile2 2>/dev/null | $grepcut"
  ;                                                               # ALIGN
my $xml      = new Mioga::XML::Simple( forcearray => 1 );
my $xml_tree = $xml->XMLin($skel_file);
my $skel_name =
  ( exists( $xml_tree->{'name'} ) ) ? $xml_tree->{'name'} : "";
my $grp = GroupGetValues( $conf->{dbh}, $group_id );
my $adm_profile =
  ProfileGetUser( $conf->{dbh}, $grp->{id_admin}, $group_id );
my $harness = TAP::Harness->new(
    { verbosity => 1, formatter_class => "TAP::Formatter::Console" } );
require File::Temp;
require Time::HiRes;
my ( $fh, $filename ) = File::Temp::tempfile("Time-HiRes-utime-XXXXXXXXX");
use File::Basename qw[dirname];
my $dirname = dirname($filename);
my $CUT         = qr/\n=cut.*$EOP/;
my $pod_or_DATA = qr/
              ^=(?:head[1-4]|item) .*? $CUT
            | ^=pod .*? $CUT
            | ^=for .*? $CUT
            | ^=begin .*? $CUT
            | ^__(DATA|END)__\r?\n.*
            /smx;
require Cwd;
( my $boot = $self->{NAME} ) =~ s/:/_/g;
doit(
sub { @E::ISA = qw/F/ },
sub { @E::ISA = qw/D/; @C::ISA = qw/F/ },
sub { @C::ISA = qw//; @A::ISA = qw/K/ },
sub { @A::ISA = qw//; @J::ISA = qw/F K/ },
sub { @J::ISA = qw/F/; @H::ISA = qw/K G/ },
sub { @H::ISA = qw/G/; @B::ISA = qw/B/ },
sub { @B::ISA = qw//; @K::ISA = qw/K J I/ },
sub { @K::ISA = qw/J I/; @D::ISA = qw/A H B C/ },
);
my %extractor_for = (
    quotelike => [ $ws, $variable,    $id, { MATCH => \&extract_quotelike } ],
    regex     => [ $ws, $pod_or_DATA, $id, $exql ],
    string    => [ $ws, $pod_or_DATA, $id, $exql ],
    code => [
        $ws,            { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    code_no_comments => [
        { DONT_MATCH => $comment },
        $ncws,          { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    executable => [ $ws, { DONT_MATCH => $pod_or_DATA } ],
    executable_no_comments =>
      [ { DONT_MATCH => $comment }, $ncws, { DONT_MATCH => $pod_or_DATA } ],
    all => [ { MATCH => qr/(?s:.*)/ } ],
);
exit 1;
----------

        'kgb2' => <<'----------',
# with -kgb, do no break after last my 
sub next_sibling {
	my $self     = shift;
	my $parent   = $_PARENT{refaddr $self} or return '';
	my $key      = refaddr $self;
	my $elements = $parent->{children};
	my $position = List::MoreUtils::firstidx {
		refaddr $_ == $key
		} @$elements;
	$elements->[$position + 1] || '';
}

----------

        'kgb3' => <<'----------',
#!/usr/bin/perl -w
use strict;  # with -kgb, no break after hash bang
our ( @Changed, $TAP );  # break after isolated 'our'
use File::Compare;
use Symbol;
use Text::Wrap();
use Text::Warp();
use Blast::IPS::MathUtils qw(
  set_interpolation_points
  table_row_interpolation
  two_point_interpolation
);  # with -kgb, break around isolated 'local' below
use Text::Warp();
local($delta2print) =
	(defined $size) ? int($size/50) : $defaultdelta2print;
print "break before this line\n";
----------

        'kgb4' => <<'----------',
print "hello"; # with -kgb, break after this line
use strict;
use warnings;
use Test::More tests => 1;
use Pod::Simple::XHTML;
my $c = <<EOF;
=head1 Documentation
The keyword group dies here
Do not put a blank line in this here-doc
EOF
my $d = $c ."=cut\n";
exit 1; 
_END_
----------

        'kgb5' => <<'----------',
# with -kgb, do not put blank in ternary
print "Starting\n"; # with -kgb, break after this line
my $A = "1";
my $B = "0";
my $C = "1";
my $D = "1";
my $result =
    $A
  ? $B
      ? $C
          ? "+A +B +C"
          : "+A +B -C"
      : "+A -B"
  : "-A";
my $F = "0";
print "with -kgb, put blank above this line; result=$result\n";
----------

        'kgb_tight' => <<'----------',
# a variety of line types for testing -kgb
use strict;
use Test;
use Encode qw(from_to encode decode
  encode_utf8 decode_utf8
  find_encoding is_utf8);

use charnames qw(greek);
our $targetdir = "/usr/local/doc/HTML/Perl";

local (
    $tocfile,   $loffile,   $lotfile,         $footfile,
    $citefile,  $idxfile,   $figure_captions, $table_captions,
    $footnotes, $citations, %font_size,       %index,
    %done,      $t_title,   $t_author,        $t_date,
    $t_address, $t_affil,   $changed
);
my @UNITCHECKs =
    B::unitcheck_av->isa("B::AV")
  ? B::unitcheck_av->ARRAY
  : ();

my @CHECKs = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
my $dna  = Bio::LiveSeq::DNA->new( -seq => $dnasequence );
my $min  = 1;
my $max  = length($dnasequence);
my $T = $G->_strongly_connected;

my %R = $T->vertex_roots;
my @C;    # We're not calling the strongly_connected_components()
	  # Do not separate this hanging side comment from previous

my $G = shift;

my $exon = Bio::LiveSeq::Exon->new(
    -seq    => $dna,
    -start  => $min,
    -end    => $max,
    -strand => 1
);
my @inputs = (
    0777, 0700, 0470, 0407, 0433, 0400, 0430, 0403, 0111, 0100,
    0110, 0101, 0731, 0713, 0317, 0371, 0173, 0137
);
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) +
  ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
my $r = q{
pm_to_blib: $(TO_INST_PM)
};
my $regcomp_re =
  "(?<routine>ckWARN(?:\\d+)?reg\\w*|vWARN\\d+|$regcomp_fail_re)";
my $position = List::MoreUtils::firstidx {
    refaddr $_ == $key
}

my $alignprogram =
"/usr/local/etc/bioinfo/fasta2/align -s /usr/local/etc/bioinfo/fasta2/idnaa.mat $fastafile1 $fastafile2 2>/dev/null | $grepcut"
  ;                                                               # ALIGN
my $skel_name =
  ( exists( $xml_tree->{'name'} ) ) ? $xml_tree->{'name'} : "";
my $grp = GroupGetValues( $conf->{dbh}, $group_id );

my $adm_profile =
  ProfileGetUser( $conf->{dbh}, $grp->{id_admin}, $group_id );
my $harness = TAP::Harness->new(
    { verbosity => 1, formatter_class => "TAP::Formatter::Console" } );
require File::Temp;

require Time::HiRes;

my ( $fh, $filename ) = File::Temp::tempfile("Time-HiRes-utime-XXXXXXXXX");
use File::Basename qw[dirname];
my $dirname = dirname($filename);
my $CUT         = qr/\n=cut.*$EOP/;

my $pod_or_DATA = qr/
              ^=(?:head[1-4]|item) .*? $CUT
            | ^=pod .*? $CUT
            | ^=for .*? $CUT
            | ^=begin .*? $CUT
            | ^__(DATA|END)__\r?\n.*
            /smx;

require Cwd;
print "continuing\n";
exit 1;
----------

        'kgbd' => <<'----------',
package A1::B2;

use strict;

require Exporter;
use A1::Context;

use A1::Database;
use A1::Bibliotek;
use A1::Author;
use A1::Title;

use vars qw($VERSION @ISA @EXPORT);
$VERSION = 0.01;
----------

        'ternary3' => <<'----------',
# this previously caused trouble because of the = and =~
push( @aligns,
      ( ( $a = shift @a ) =~ /[^n]/ ) ? $a
    : (@isnum) ? 'n'
    :            'l' )
  unless $opt_a;
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'else1.def' => {
            source => "else1",
            params => "def",
            expect => <<'#1...........',
# pad after 'if' when followed by 'elsif'
if    ( not defined $dir or not length $dir ) { $rslt = ''; }
elsif ( $dir =~ /^\$\([^\)]+\)\Z(?!\n)/s )    { $rslt = $dir; }
else                                          { $rslt = vmspath($dir); }
#1...........
        },

        'else2.def' => {
            source => "else2",
            params => "def",
            expect => <<'#2...........',
        # no pad after 'if' when followed by 'else'
        if ( $m = $g[$x][$y] ) { print $$m{v}; $$m{i}->() }
        else                   { print " " }
#2...........
        },

        'ternary3.def' => {
            source => "ternary3",
            params => "def",
            expect => <<'#3...........',
# this previously caused trouble because of the = and =~
push(
    @aligns,
    ( ( $a = shift @a ) =~ /[^n]/ ) ? $a
    : (@isnum)                      ? 'n'
    :                                 'l'
) unless $opt_a;
#3...........
        },

        'align17.def' => {
            source => "align17",
            params => "def",
            expect => <<'#4...........',
# align => even at broken sub block
my %opt = (
    'cc' => sub { $param::cachecom     = 1; },
    'cd' => sub { $param::cachedisable = 1; },
    'p'  => sub {
        $param::pflag = 1;
        $param::build = 0;
    }
);
#4...........
        },

        'align18.def' => {
            source => "align18",
            params => "def",
            expect => <<'#5...........',
#align '&&'
for ( $ENV{HTTP_USER_AGENT} ) {
    $page =
         /Mac/            && 'm/Macintrash.html'
      || /Win(dows)?NT/   && 'e/evilandrude.html'
      || /Win|MSIE|WebTV/ && 'm/MicroslothWindows.html'
      || /Linux/          && 'l/Linux.html'
      || /HP-UX/          && 'h/HP-SUX.html'
      || /SunOS/          && 's/ScumOS.html'
      || 'a/AppendixB.html';
}
#5...........
        },

        'kgb1.def' => {
            source => "kgb1",
            params => "def",
            expect => <<'#6...........',
# a variety of line types for testing -kgb
use strict;
use Test;
use Encode qw(from_to encode decode
  encode_utf8 decode_utf8
  find_encoding is_utf8);
use charnames qw(greek);
our $targetdir = "/usr/local/doc/HTML/Perl";
local (
    $tocfile,   $loffile,   $lotfile,         $footfile,
    $citefile,  $idxfile,   $figure_captions, $table_captions,
    $footnotes, $citations, %font_size,       %index,
    %done,      $t_title,   $t_author,        $t_date,
    $t_address, $t_affil,   $changed
);
my @UNITCHECKs =
    B::unitcheck_av->isa("B::AV")
  ? B::unitcheck_av->ARRAY
  : ();
my @CHECKs = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
my $dna    = Bio::LiveSeq::DNA->new( -seq => $dnasequence );
my $min    = 1;
my $max    = length($dnasequence);
my $T      = $G->_strongly_connected;
my %R      = $T->vertex_roots;
my @C;    # We're not calling the strongly_connected_components()
          # Do not separate this hanging side comment from previous
my $G    = shift;
my $exon = Bio::LiveSeq::Exon->new(
    -seq    => $dna,
    -start  => $min,
    -end    => $max,
    -strand => 1
);
my $octal_mode;
my @inputs = (
    0777, 0700, 0470, 0407, 0433, 0400, 0430, 0403, 0111, 0100,
    0110, 0101, 0731, 0713, 0317, 0371, 0173, 0137
);
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) + ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
my $r = q{
pm_to_blib: $(TO_INST_PM)
};
my $regcomp_re =
  "(?<routine>ckWARN(?:\\d+)?reg\\w*|vWARN\\d+|$regcomp_fail_re)";
my $position = List::MoreUtils::firstidx {
    refaddr $_ == $key
}
my @exons      = ($exon);
my $fastafile2 = "/tmp/tmpfastafile2";
my $grepcut    = 'egrep -v "[[:digit:]]|^ *$|sequences" | cut -c8-';  # grep/cut
my $alignprogram =
"/usr/local/etc/bioinfo/fasta2/align -s /usr/local/etc/bioinfo/fasta2/idnaa.mat $fastafile1 $fastafile2 2>/dev/null | $grepcut"
  ;                                                                   # ALIGN
my $xml      = new Mioga::XML::Simple( forcearray => 1 );
my $xml_tree = $xml->XMLin($skel_file);
my $skel_name =
  ( exists( $xml_tree->{'name'} ) ) ? $xml_tree->{'name'} : "";
my $grp = GroupGetValues( $conf->{dbh}, $group_id );
my $adm_profile =
  ProfileGetUser( $conf->{dbh}, $grp->{id_admin}, $group_id );
my $harness = TAP::Harness->new(
    { verbosity => 1, formatter_class => "TAP::Formatter::Console" } );
require File::Temp;
require Time::HiRes;
my ( $fh, $filename ) = File::Temp::tempfile("Time-HiRes-utime-XXXXXXXXX");
use File::Basename qw[dirname];
my $dirname     = dirname($filename);
my $CUT         = qr/\n=cut.*$EOP/;
my $pod_or_DATA = qr/
              ^=(?:head[1-4]|item) .*? $CUT
            | ^=pod .*? $CUT
            | ^=for .*? $CUT
            | ^=begin .*? $CUT
            | ^__(DATA|END)__\r?\n.*
            /smx;
require Cwd;
( my $boot = $self->{NAME} ) =~ s/:/_/g;
doit(
    sub { @E::ISA = qw/F/ },
    sub { @E::ISA = qw/D/;   @C::ISA = qw/F/ },
    sub { @C::ISA = qw//;    @A::ISA = qw/K/ },
    sub { @A::ISA = qw//;    @J::ISA = qw/F K/ },
    sub { @J::ISA = qw/F/;   @H::ISA = qw/K G/ },
    sub { @H::ISA = qw/G/;   @B::ISA = qw/B/ },
    sub { @B::ISA = qw//;    @K::ISA = qw/K J I/ },
    sub { @K::ISA = qw/J I/; @D::ISA = qw/A H B C/ },
);
my %extractor_for = (
    quotelike => [ $ws, $variable,    $id, { MATCH => \&extract_quotelike } ],
    regex     => [ $ws, $pod_or_DATA, $id, $exql ],
    string    => [ $ws, $pod_or_DATA, $id, $exql ],
    code      => [
        $ws, { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    code_no_comments => [
        { DONT_MATCH => $comment },
        $ncws, { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    executable             => [ $ws, { DONT_MATCH => $pod_or_DATA } ],
    executable_no_comments =>
      [ { DONT_MATCH => $comment }, $ncws, { DONT_MATCH => $pod_or_DATA } ],
    all => [ { MATCH => qr/(?s:.*)/ } ],
);
exit 1;
#6...........
        },

        'kgb1.kgb' => {
            source => "kgb1",
            params => "kgb",
            expect => <<'#7...........',
# a variety of line types for testing -kgb
use strict;
use Test;
use Encode qw(from_to encode decode
  encode_utf8 decode_utf8
  find_encoding is_utf8);
use charnames qw(greek);
our $targetdir = "/usr/local/doc/HTML/Perl";
local (
    $tocfile,   $loffile,   $lotfile,         $footfile,
    $citefile,  $idxfile,   $figure_captions, $table_captions,
    $footnotes, $citations, %font_size,       %index,
    %done,      $t_title,   $t_author,        $t_date,
    $t_address, $t_affil,   $changed
);

my @UNITCHECKs =
    B::unitcheck_av->isa("B::AV")
  ? B::unitcheck_av->ARRAY
  : ();
my @CHECKs = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
my $dna    = Bio::LiveSeq::DNA->new( -seq => $dnasequence );
my $min    = 1;
my $max    = length($dnasequence);
my $T      = $G->_strongly_connected;
my %R      = $T->vertex_roots;
my @C;    # We're not calling the strongly_connected_components()
          # Do not separate this hanging side comment from previous
my $G    = shift;
my $exon = Bio::LiveSeq::Exon->new(
    -seq    => $dna,
    -start  => $min,
    -end    => $max,
    -strand => 1
);
my $octal_mode;
my @inputs = (
    0777, 0700, 0470, 0407, 0433, 0400, 0430, 0403, 0111, 0100,
    0110, 0101, 0731, 0713, 0317, 0371, 0173, 0137
);
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) + ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
my $r = q{
pm_to_blib: $(TO_INST_PM)
};
my $regcomp_re =
  "(?<routine>ckWARN(?:\\d+)?reg\\w*|vWARN\\d+|$regcomp_fail_re)";
my $position = List::MoreUtils::firstidx {
    refaddr $_ == $key
}
my @exons      = ($exon);
my $fastafile2 = "/tmp/tmpfastafile2";
my $grepcut    = 'egrep -v "[[:digit:]]|^ *$|sequences" | cut -c8-';  # grep/cut
my $alignprogram =
"/usr/local/etc/bioinfo/fasta2/align -s /usr/local/etc/bioinfo/fasta2/idnaa.mat $fastafile1 $fastafile2 2>/dev/null | $grepcut"
  ;                                                                   # ALIGN
my $xml      = new Mioga::XML::Simple( forcearray => 1 );
my $xml_tree = $xml->XMLin($skel_file);
my $skel_name =
  ( exists( $xml_tree->{'name'} ) ) ? $xml_tree->{'name'} : "";
my $grp = GroupGetValues( $conf->{dbh}, $group_id );
my $adm_profile =
  ProfileGetUser( $conf->{dbh}, $grp->{id_admin}, $group_id );
my $harness = TAP::Harness->new(
    { verbosity => 1, formatter_class => "TAP::Formatter::Console" } );

require File::Temp;
require Time::HiRes;
my ( $fh, $filename ) = File::Temp::tempfile("Time-HiRes-utime-XXXXXXXXX");
use File::Basename qw[dirname];
my $dirname     = dirname($filename);
my $CUT         = qr/\n=cut.*$EOP/;
my $pod_or_DATA = qr/
              ^=(?:head[1-4]|item) .*? $CUT
            | ^=pod .*? $CUT
            | ^=for .*? $CUT
            | ^=begin .*? $CUT
            | ^__(DATA|END)__\r?\n.*
            /smx;
require Cwd;

( my $boot = $self->{NAME} ) =~ s/:/_/g;
doit(
    sub { @E::ISA = qw/F/ },
    sub { @E::ISA = qw/D/;   @C::ISA = qw/F/ },
    sub { @C::ISA = qw//;    @A::ISA = qw/K/ },
    sub { @A::ISA = qw//;    @J::ISA = qw/F K/ },
    sub { @J::ISA = qw/F/;   @H::ISA = qw/K G/ },
    sub { @H::ISA = qw/G/;   @B::ISA = qw/B/ },
    sub { @B::ISA = qw//;    @K::ISA = qw/K J I/ },
    sub { @K::ISA = qw/J I/; @D::ISA = qw/A H B C/ },
);
my %extractor_for = (
    quotelike => [ $ws, $variable,    $id, { MATCH => \&extract_quotelike } ],
    regex     => [ $ws, $pod_or_DATA, $id, $exql ],
    string    => [ $ws, $pod_or_DATA, $id, $exql ],
    code      => [
        $ws, { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    code_no_comments => [
        { DONT_MATCH => $comment },
        $ncws, { DONT_MATCH => $pod_or_DATA },
        $variable, $id, { DONT_MATCH => \&extract_quotelike }
    ],
    executable             => [ $ws, { DONT_MATCH => $pod_or_DATA } ],
    executable_no_comments =>
      [ { DONT_MATCH => $comment }, $ncws, { DONT_MATCH => $pod_or_DATA } ],
    all => [ { MATCH => qr/(?s:.*)/ } ],
);
exit 1;
#7...........
        },

        'kgb2.def' => {
            source => "kgb2",
            params => "def",
            expect => <<'#8...........',
# with -kgb, do no break after last my
sub next_sibling {
    my $self     = shift;
    my $parent   = $_PARENT{ refaddr $self } or return '';
    my $key      = refaddr $self;
    my $elements = $parent->{children};
    my $position = List::MoreUtils::firstidx {
        refaddr $_ == $key
    }
    @$elements;
    $elements->[ $position + 1 ] || '';
}

#8...........
        },

        'kgb2.kgb' => {
            source => "kgb2",
            params => "kgb",
            expect => <<'#9...........',
# with -kgb, do no break after last my
sub next_sibling {

    my $self     = shift;
    my $parent   = $_PARENT{ refaddr $self } or return '';
    my $key      = refaddr $self;
    my $elements = $parent->{children};
    my $position = List::MoreUtils::firstidx {
        refaddr $_ == $key
    }
    @$elements;
    $elements->[ $position + 1 ] || '';
}

#9...........
        },

        'kgb3.def' => {
            source => "kgb3",
            params => "def",
            expect => <<'#10...........',
#!/usr/bin/perl -w
use strict;                # with -kgb, no break after hash bang
our ( @Changed, $TAP );    # break after isolated 'our'
use File::Compare;
use Symbol;
use Text::Wrap();
use Text::Warp();
use Blast::IPS::MathUtils qw(
  set_interpolation_points
  table_row_interpolation
  two_point_interpolation
);                         # with -kgb, break around isolated 'local' below
use Text::Warp();
local ($delta2print) =
  ( defined $size ) ? int( $size / 50 ) : $defaultdelta2print;
print "break before this line\n";
#10...........
        },

        'kgb3.kgb' => {
            source => "kgb3",
            params => "kgb",
            expect => <<'#11...........',
#!/usr/bin/perl -w
use strict;                # with -kgb, no break after hash bang
our ( @Changed, $TAP );    # break after isolated 'our'

use File::Compare;
use Symbol;
use Text::Wrap();
use Text::Warp();
use Blast::IPS::MathUtils qw(
  set_interpolation_points
  table_row_interpolation
  two_point_interpolation
);                         # with -kgb, break around isolated 'local' below
use Text::Warp();

local ($delta2print) =
  ( defined $size ) ? int( $size / 50 ) : $defaultdelta2print;

print "break before this line\n";
#11...........
        },

        'kgb4.def' => {
            source => "kgb4",
            params => "def",
            expect => <<'#12...........',
print "hello";    # with -kgb, break after this line
use strict;
use warnings;
use Test::More tests => 1;
use Pod::Simple::XHTML;
my $c = <<EOF;
=head1 Documentation
The keyword group dies here
Do not put a blank line in this here-doc
EOF
my $d = $c . "=cut\n";
exit 1;
_END_
#12...........
        },

        'kgb4.kgb' => {
            source => "kgb4",
            params => "kgb",
            expect => <<'#13...........',
print "hello";    # with -kgb, break after this line

use strict;
use warnings;
use Test::More tests => 1;
use Pod::Simple::XHTML;
my $c = <<EOF;
=head1 Documentation
The keyword group dies here
Do not put a blank line in this here-doc
EOF
my $d = $c . "=cut\n";
exit 1;
_END_
#13...........
        },

        'kgb5.def' => {
            source => "kgb5",
            params => "def",
            expect => <<'#14...........',
# with -kgb, do not put blank in ternary
print "Starting\n";    # with -kgb, break after this line
my $A = "1";
my $B = "0";
my $C = "1";
my $D = "1";
my $result =
    $A
  ? $B
      ? $C
          ? "+A +B +C"
          : "+A +B -C"
      : "+A -B"
  : "-A";
my $F = "0";
print "with -kgb, put blank above this line; result=$result\n";
#14...........
        },

        'kgb5.kgb' => {
            source => "kgb5",
            params => "kgb",
            expect => <<'#15...........',
# with -kgb, do not put blank in ternary
print "Starting\n";    # with -kgb, break after this line

my $A = "1";
my $B = "0";
my $C = "1";
my $D = "1";
my $result =
    $A
  ? $B
      ? $C
          ? "+A +B +C"
          : "+A +B -C"
      : "+A -B"
  : "-A";
my $F = "0";
print "with -kgb, put blank above this line; result=$result\n";
#15...........
        },

        'kgbd.def' => {
            source => "kgbd",
            params => "def",
            expect => <<'#16...........',
package A1::B2;

use strict;

require Exporter;
use A1::Context;

use A1::Database;
use A1::Bibliotek;
use A1::Author;
use A1::Title;

use vars qw($VERSION @ISA @EXPORT);
$VERSION = 0.01;
#16...........
        },

        'kgbd.kgbd' => {
            source => "kgbd",
            params => "kgbd",
            expect => <<'#17...........',
package A1::B2;

use strict;
require Exporter;

use A1::Context;
use A1::Database;
use A1::Bibliotek;
use A1::Author;
use A1::Title;
use vars qw($VERSION @ISA @EXPORT);

$VERSION = 0.01;
#17...........
        },

        'kgb_tight.def' => {
            source => "kgb_tight",
            params => "def",
            expect => <<'#18...........',
# a variety of line types for testing -kgb
use strict;
use Test;
use Encode qw(from_to encode decode
  encode_utf8 decode_utf8
  find_encoding is_utf8);

use charnames qw(greek);
our $targetdir = "/usr/local/doc/HTML/Perl";

local (
    $tocfile,   $loffile,   $lotfile,         $footfile,
    $citefile,  $idxfile,   $figure_captions, $table_captions,
    $footnotes, $citations, %font_size,       %index,
    %done,      $t_title,   $t_author,        $t_date,
    $t_address, $t_affil,   $changed
);
my @UNITCHECKs =
    B::unitcheck_av->isa("B::AV")
  ? B::unitcheck_av->ARRAY
  : ();

my @CHECKs = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
my $dna    = Bio::LiveSeq::DNA->new( -seq => $dnasequence );
my $min    = 1;
my $max    = length($dnasequence);
my $T      = $G->_strongly_connected;

my %R = $T->vertex_roots;
my @C;    # We're not calling the strongly_connected_components()
          # Do not separate this hanging side comment from previous

my $G = shift;

my $exon = Bio::LiveSeq::Exon->new(
    -seq    => $dna,
    -start  => $min,
    -end    => $max,
    -strand => 1
);
my @inputs = (
    0777, 0700, 0470, 0407, 0433, 0400, 0430, 0403, 0111, 0100,
    0110, 0101, 0731, 0713, 0317, 0371, 0173, 0137
);
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) + ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
my $r = q{
pm_to_blib: $(TO_INST_PM)
};
my $regcomp_re =
  "(?<routine>ckWARN(?:\\d+)?reg\\w*|vWARN\\d+|$regcomp_fail_re)";
my $position = List::MoreUtils::firstidx {
    refaddr $_ == $key
}

my $alignprogram =
"/usr/local/etc/bioinfo/fasta2/align -s /usr/local/etc/bioinfo/fasta2/idnaa.mat $fastafile1 $fastafile2 2>/dev/null | $grepcut"
  ;    # ALIGN
my $skel_name =
  ( exists( $xml_tree->{'name'} ) ) ? $xml_tree->{'name'} : "";
my $grp = GroupGetValues( $conf->{dbh}, $group_id );

my $adm_profile =
  ProfileGetUser( $conf->{dbh}, $grp->{id_admin}, $group_id );
my $harness = TAP::Harness->new(
    { verbosity => 1, formatter_class => "TAP::Formatter::Console" } );
require File::Temp;

require Time::HiRes;

my ( $fh, $filename ) = File::Temp::tempfile("Time-HiRes-utime-XXXXXXXXX");
use File::Basename qw[dirname];
my $dirname = dirname($filename);
my $CUT     = qr/\n=cut.*$EOP/;

my $pod_or_DATA = qr/
              ^=(?:head[1-4]|item) .*? $CUT
            | ^=pod .*? $CUT
            | ^=for .*? $CUT
            | ^=begin .*? $CUT
            | ^__(DATA|END)__\r?\n.*
            /smx;

require Cwd;
print "continuing\n";
exit 1;
#18...........
        },

        'gnu5.def' => {
            source => "gnu5",
            params => "def",
            expect => <<'#19...........',
        # side comments limit gnu type formatting with l=80; note extra comma
        push @tests, [
            "Lowest code point requiring 13 bytes to represent",    # 2**36
            "\xff\x80\x80\x80\x80\x80\x81\x80\x80\x80\x80\x80\x80",
            ($::is64bit) ? 0x1000000000 : -1,    # overflows on 32bit
          ],
          ;
#19...........
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
