# Created with: ./make_t.pl

# Contents:
#1 gnu1.gnu
#2 gnu2.def
#3 gnu2.gnu
#4 gnu3.def
#5 gnu3.gnu
#6 gnu4.def
#7 gnu4.gnu
#8 hanging_side_comments1.def
#9 hanging_side_comments2.def
#10 hash1.def
#11 hashbang.def
#12 here1.def
#13 html1.def
#14 html1.html
#15 ident1.def
#16 if1.def
#17 iscl1.def
#18 iscl1.iscl
#19 label1.def
#20 lextest1.def

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
        'gnu'  => "-gnu",
        'html' => <<'----------',
-fmt="html"
-nts
----------
        'iscl' => "-iscl",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'gnu1' => <<'----------',
@common_sometimes = (
    "aclocal.m4", "acconfig.h", "config.h.top", "config.h.bot",
    "stamp-h.in", 'stamp-vti'
);
----------

        'gnu2' => <<'----------',
$search_mb = $menu_bar->Menubutton(
    '-text'        => 'Search',
    '-relief'      => 'raised',
    '-borderwidth' => 2,
)->pack(
    '-side' => 'left',
    '-padx' => 2
);
----------

        'gnu3' => <<'----------',
$output_rules .= &file_contents_with_transform( 's/\@TEXI\@/' . $info_cursor . '/g; ' . 's/\@VTI\@/' . $vti . '/g; ' . 's/\@VTEXI\@/' . $vtexi . '/g;' . 's,\@MDDIR\@,' . $conf_pat . ',g;', 'texi-vers');
----------

        'gnu4' => <<'----------',
my $mzef = Bio::Tools::MZEF->new( '-file' => Bio::Root::IO->catfile("t", "genomic-seq.mzef"));
----------

        'hanging_side_comments1' => <<'----------',
$valuestr .= $value . " " ;    # with a trailing space in case there are multiple values
   # for this tag (allowed in GFF2 and .ace format)
----------

        'hanging_side_comments2' => <<'----------',
# keep '=' lined up even with hanging side comments
$ax=1;# side comment
 # hanging side comment
$boondoggle=5;# side comment
$beetle=5;# side comment
 # hanging side comment
$d=3;
----------

        'hash1' => <<'----------',
%TV=(flintstones=>{series=>"flintstones",nights=>[qw(monday thursday friday)],
members=>[{name=>"fred",role=>"lead",age=>36,},{name=>"wilma",role=>"wife",
age=>31,},{name=>"pebbles",role=>"kid",age=>4,},],},jetsons=>{series=>"jetsons",
nights=>[qw(wednesday saturday)],members=>[{name=>"george",role=>"lead",age=>41,
},{name=>"jane",role=>"wife",age=>39,},{name=>"elroy",role=>"kid",age=>9,},],},
simpsons=>{series=>"simpsons",nights=>[qw(monday)],members=>[{name=>"homer",
role=>"lead",age=>34,},{name=>"marge",role=>"wife",age=>37,},{name=>"bart",
role=>"kid",age=>11,},],},);
----------

        'hashbang' => <<'----------',
#!/usr/bin/perl
----------

        'here1' => <<'----------',
is( <<~`END`, "ok\n", '<<~`HEREDOC`' );
  $Perl -le "print 'ok'"
  END
----------

        'html1' => <<'----------',
if   ( $editlblk eq 1 ) { $editlblk = "on";  $editlblkchecked = "checked" }
else                    { $editlblk = "off"; $editlblkchecked = "unchecked" }
----------

        'ident1' => <<'----------',
package A;
sub new  {
   print "A::new! $_[0] $_[1]\n";
   return 1;
}
package main;
my $scanner = new A::() ;
$scanner = new A::;
$scanner = new A 'a';
----------

        'if1' => <<'----------',
# one-line blocks
if ( $editlblk eq 1 ) { $editlblk = "on"; $editlblkchecked = "checked" }
else { $editlblk = "off"; $editlblkchecked = "unchecked" }
----------

        'iscl1' => <<'----------',
	# -iscl will not allow alignment of hanging side comments (currently)
        $gsmatch = ( $sub >= 50 ) ? "equal" : "lequal"; # Force an equal match for
               # dev, but be more forgiving
               # for releases
----------

        'label1' => <<'----------',
INIT : { 
$a++;
print "looping with label INIT:, a=$a\n";
  if ($a<10) {goto INIT}
}
package: {
    print "hello!\n";
}
sub: {
    print "hello!\n";
}
----------

        'lextest1' => <<'----------',
$_= <<'EOL';
   $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL
LOOP:{print(" digits"),redo LOOP if/\G\d+\b[,.;]?\s*/gc;print(" lowercase"),
redo LOOP if/\G[a-z]+\b[,.;]?\s*/gc;print(" UPPERCASE"), redo 
LOOP if/\G[A-Z]+\b[,.;]?\s*/gc;print(" Capitalized"),
redo LOOP if/\G[A-Z][a-z]+\b[,.;]?\s*/gc;
print(" MiXeD"),redo LOOP if/\G[A-Za-z]+\b[,.;]?\s*/gc;print(
" alphanumeric"),redo LOOP if/\G[A-Za-z0-9]+\b[,.;]?\s*/gc;print(" line-noise"
),redo LOOP if/\G[^A-Za-z0-9]+/gc;print". That's all!\n";}
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'gnu1.gnu' => {
            source => "gnu1",
            params => "gnu",
            expect => <<'#1...........',
@common_sometimes = (
                     "aclocal.m4",   "acconfig.h",
                     "config.h.top", "config.h.bot",
                     "stamp-h.in",   'stamp-vti'
                    );
#1...........
        },

        'gnu2.def' => {
            source => "gnu2",
            params => "def",
            expect => <<'#2...........',
$search_mb = $menu_bar->Menubutton(
    '-text'        => 'Search',
    '-relief'      => 'raised',
    '-borderwidth' => 2,
)->pack(
    '-side' => 'left',
    '-padx' => 2
);
#2...........
        },

        'gnu2.gnu' => {
            source => "gnu2",
            params => "gnu",
            expect => <<'#3...........',
$search_mb = $menu_bar->Menubutton(
                                   '-text'        => 'Search',
                                   '-relief'      => 'raised',
                                   '-borderwidth' => 2,
  )->pack('-side' => 'left',
          '-padx' => 2);
#3...........
        },

        'gnu3.def' => {
            source => "gnu3",
            params => "def",
            expect => <<'#4...........',
$output_rules .= &file_contents_with_transform(
    's/\@TEXI\@/'
      . $info_cursor . '/g; '
      . 's/\@VTI\@/'
      . $vti . '/g; '
      . 's/\@VTEXI\@/'
      . $vtexi . '/g;'
      . 's,\@MDDIR\@,'
      . $conf_pat . ',g;',
    'texi-vers'
);
#4...........
        },

        'gnu3.gnu' => {
            source => "gnu3",
            params => "gnu",
            expect => <<'#5...........',
$output_rules .=
  &file_contents_with_transform(
                                's/\@TEXI\@/'
                                  . $info_cursor . '/g; '
                                  . 's/\@VTI\@/'
                                  . $vti . '/g; '
                                  . 's/\@VTEXI\@/'
                                  . $vtexi . '/g;'
                                  . 's,\@MDDIR\@,'
                                  . $conf_pat . ',g;',
                                'texi-vers'
                               );
#5...........
        },

        'gnu4.def' => {
            source => "gnu4",
            params => "def",
            expect => <<'#6...........',
my $mzef = Bio::Tools::MZEF->new(
    '-file' => Bio::Root::IO->catfile( "t", "genomic-seq.mzef" ) );
#6...........
        },

        'gnu4.gnu' => {
            source => "gnu4",
            params => "gnu",
            expect => <<'#7...........',
my $mzef = Bio::Tools::MZEF->new(
                    '-file' => Bio::Root::IO->catfile("t", "genomic-seq.mzef"));
#7...........
        },

        'hanging_side_comments1.def' => {
            source => "hanging_side_comments1",
            params => "def",
            expect => <<'#8...........',
$valuestr .=
  $value . " ";    # with a trailing space in case there are multiple values
                   # for this tag (allowed in GFF2 and .ace format)
#8...........
        },

        'hanging_side_comments2.def' => {
            source => "hanging_side_comments2",
            params => "def",
            expect => <<'#9...........',
# keep '=' lined up even with hanging side comments
$ax         = 1;    # side comment
                    # hanging side comment
$boondoggle = 5;    # side comment
$beetle     = 5;    # side comment
                    # hanging side comment
$d          = 3;
#9...........
        },

        'hash1.def' => {
            source => "hash1",
            params => "def",
            expect => <<'#10...........',
%TV = (
    flintstones => {
        series  => "flintstones",
        nights  => [qw(monday thursday friday)],
        members => [
            { name => "fred", role => "lead", age => 36, },
            {
                name => "wilma",
                role => "wife",
                age  => 31,
            },
            { name => "pebbles", role => "kid", age => 4, },
        ],
    },
    jetsons => {
        series  => "jetsons",
        nights  => [qw(wednesday saturday)],
        members => [
            {
                name => "george",
                role => "lead",
                age  => 41,
            },
            { name => "jane",  role => "wife", age => 39, },
            { name => "elroy", role => "kid",  age => 9, },
        ],
    },
    simpsons => {
        series  => "simpsons",
        nights  => [qw(monday)],
        members => [
            {
                name => "homer",
                role => "lead",
                age  => 34,
            },
            { name => "marge", role => "wife", age => 37, },
            {
                name => "bart",
                role => "kid",
                age  => 11,
            },
        ],
    },
);
#10...........
        },

        'hashbang.def' => {
            source => "hashbang",
            params => "def",
            expect => <<'#11...........',
#!/usr/bin/perl
#11...........
        },

        'here1.def' => {
            source => "here1",
            params => "def",
            expect => <<'#12...........',
is( <<~`END`, "ok\n", '<<~`HEREDOC`' );
  $Perl -le "print 'ok'"
  END
#12...........
        },

        'html1.def' => {
            source => "html1",
            params => "def",
            expect => <<'#13...........',
if   ( $editlblk eq 1 ) { $editlblk = "on";  $editlblkchecked = "checked" }
else                    { $editlblk = "off"; $editlblkchecked = "unchecked" }
#13...........
        },

        'html1.html' => {
            source => "html1",
            params => "html",
            expect => <<'#14...........',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<!-- Generated by perltidy  -->
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>perltidy</title>
<style type="text/css">
<!--
/* default style sheet generated by perltidy */
body {background: #FFFFFF; color: #000000}
pre { color: #000000;
      background: #FFFFFF;
      font-family: courier;
    }

.c  { color: #228B22;} /* comment */
.cm { color: #000000;} /* comma */
.co { color: #000000;} /* colon */
.h  { color: #CD5555; font-weight:bold;} /* here-doc-target */
.hh { color: #CD5555; font-style:italic;} /* here-doc-text */
.i  { color: #00688B;} /* identifier */
.j  { color: #CD5555; font-weight:bold;} /* label */
.k  { color: #8B008B; font-weight:bold;} /* keyword */
.m  { color: #FF0000; font-weight:bold;} /* subroutine */
.n  { color: #B452CD;} /* numeric */
.p  { color: #000000;} /* paren */
.pd { color: #228B22; font-style:italic;} /* pod-text */
.pu { color: #000000;} /* punctuation */
.q  { color: #CD5555;} /* quote */
.s  { color: #000000;} /* structure */
.sc { color: #000000;} /* semicolon */
.v  { color: #B452CD;} /* v-string */
.w  { color: #000000;} /* bareword */
-->
</style>
</head>
<body>
<a name="-top-"></a>
<h1>perltidy</h1>
<hr />
<!-- contents of filename: perltidy -->
<pre>
<span class="k">if</span>   <span class="s">(</span> <span class="i">$editlblk</span> <span class="k">eq</span> <span class="n">1</span> <span class="s">)</span> <span class="s">{</span> <span class="i">$editlblk</span> = <span class="q">&quot;on&quot;</span><span class="sc">;</span>  <span class="i">$editlblkchecked</span> = <span class="q">&quot;checked&quot;</span> <span class="s">}</span>
<span class="k">else</span>                    <span class="s">{</span> <span class="i">$editlblk</span> = <span class="q">&quot;off&quot;</span><span class="sc">;</span> <span class="i">$editlblkchecked</span> = <span class="q">&quot;unchecked&quot;</span> <span class="s">}</span>
</pre>
</body>
</html>
#14...........
        },

        'ident1.def' => {
            source => "ident1",
            params => "def",
            expect => <<'#15...........',
package A;

sub new {
    print "A::new! $_[0] $_[1]\n";
    return 1;
}

package main;
my $scanner = new A::();
$scanner = new A::;
$scanner = new A 'a';
#15...........
        },

        'if1.def' => {
            source => "if1",
            params => "def",
            expect => <<'#16...........',
# one-line blocks
if   ( $editlblk eq 1 ) { $editlblk = "on";  $editlblkchecked = "checked" }
else                    { $editlblk = "off"; $editlblkchecked = "unchecked" }
#16...........
        },

        'iscl1.def' => {
            source => "iscl1",
            params => "def",
            expect => <<'#17...........',
        # -iscl will not allow alignment of hanging side comments (currently)
        $gsmatch =
          ( $sub >= 50 ) ? "equal" : "lequal";    # Force an equal match for
                                                  # dev, but be more forgiving
                                                  # for releases
#17...........
        },

        'iscl1.iscl' => {
            source => "iscl1",
            params => "iscl",
            expect => <<'#18...........',
        # -iscl will not allow alignment of hanging side comments (currently)
        $gsmatch = ( $sub >= 50 ) ? "equal" : "lequal";    # Force an equal match for
                                                           # dev, but be more forgiving
                                                           # for releases
#18...........
        },

        'label1.def' => {
            source => "label1",
            params => "def",
            expect => <<'#19...........',
INIT: {
    $a++;
    print "looping with label INIT:, a=$a\n";
    if ( $a < 10 ) { goto INIT }
}
package: {
    print "hello!\n";
}
sub: {
    print "hello!\n";
}
#19...........
        },

        'lextest1.def' => {
            source => "lextest1",
            params => "def",
            expect => <<'#20...........',
$_ = <<'EOL';
   $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL
LOOP: {
    print(" digits"),       redo LOOP if /\G\d+\b[,.;]?\s*/gc;
    print(" lowercase"),    redo LOOP if /\G[a-z]+\b[,.;]?\s*/gc;
    print(" UPPERCASE"),    redo LOOP if /\G[A-Z]+\b[,.;]?\s*/gc;
    print(" Capitalized"),  redo LOOP if /\G[A-Z][a-z]+\b[,.;]?\s*/gc;
    print(" MiXeD"),        redo LOOP if /\G[A-Za-z]+\b[,.;]?\s*/gc;
    print(" alphanumeric"), redo LOOP if /\G[A-Za-z0-9]+\b[,.;]?\s*/gc;
    print(" line-noise"),   redo LOOP if /\G[^A-Za-z0-9]+/gc;
    print ". That's all!\n";
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
