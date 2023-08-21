# Created with: ./make_t.pl

# Contents:
#1 ce_wn1.ce_wn
#2 ce_wn1.def
#3 colin.colin
#4 colin.def
#5 essential.def
#6 essential.essential1
#7 essential.essential2
#8 extrude1.def
#9 extrude1.extrude
#10 extrude2.def
#11 extrude2.extrude
#12 extrude3.def
#13 extrude3.extrude
#14 extrude4.def
#15 extrude4.extrude
#16 fabrice_bug.def
#17 fabrice_bug.fabrice_bug
#18 format1.def
#19 given1.def
#20 gnu1.def

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
        'ce_wn' => <<'----------',
-cuddled-blocks
-wn
----------
        'colin' => <<'----------',
-l=0
-pt=2
-nsfs
-sbt=2
-ohbr
-opr
-osbr
-pvt=2
-schb
-scp
-scsb
-sohb
-sop
-sosb
----------
        'def'        => "",
        'essential1' => <<'----------',
-syn
-i=0
-l=100000
-nasc
-naws
-dws
-nanl
-blbp=0
-blbs=0
-nbbb
-kbl=0
-mbl=0
----------
        'essential2'  => "-extrude",
        'extrude'     => "--extrude",
        'fabrice_bug' => "-bt=0",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'ce_wn1' => <<'----------',
if ($BOLD_MATH) {
    (
        $labels, $comment,
        join( '', ' < B > ', &make_math( $mode, '', '', $_ ), ' < /B>' )
      )
}
else {
    (
        &process_math_in_latex( $mode, $math_style, $slevel, "\\mbox{$text}" ),
        $after
      )
}
----------

        'colin' => <<'----------',
env(0, 15, 0, 10, {
    Xtitle => 'X-data',
    Ytitle => 'Y-data',
    Title  => 'An example of errb and points',
    Font   => 'Italic'
});
----------

        'essential' => <<'----------',
# Run with mangle to squeeze out the white space
# also run with extrude

# never combine two bare words or numbers
status and ::ok(1);

return ::spw(...);

for bla::bla:: abc;

# do not combine 'overload::' and 'and'
if $self->{bareStringify} and ref $_
and defined %overload:: and defined &{'overload::StrVal'};

# do not combine 'SINK' and 'if'
my $size=-s::SINK if $file;

# do not combine to make $inputeq"quit"
if ($input eq"quit"); 

# do not combine a number with a concatenation dot to get a float '78.'
$vt100_compatible ? "\e[0;0H" : ('-' x 78 . "\n");

# do not join a minus with a bare word, because you might form
# a file test operator.  Here  "z-i" would be taken as a file test.
if (CORE::abs($z - i) < $eps); 

# '= -' should not become =- or you will get a warning

# and something like these could become ambiguous without space
# after the '-':
use constant III=>1;
$a = $b - III;
$a = - III;

# keep a space between a token ending in '$' and any word;
die @$ if $@;

# avoid combining tokens to create new meanings. Example:
# this must not become $a++$b
$a+ +$b;

# another example: do not combine these two &'s:
allow_options & &OPT_EXECCGI;

# Perl is sensitive to whitespace after the + here:
$b = xvals $a + 0.1 * yvals $a;

# keep paren separate here:
use Foo::Bar ();

# need space after foreach my; for example, this will fail in
# older versions of Perl:
foreach my$ft(@filetypes)...

# must retain space between grep and left paren; "grep(" may fail
my $match = grep (m/^-extrude$/, @list) ? 1 : 0;

# don't stick numbers next to left parens, as in:
use Mail::Internet 1.28 ();

# do not remove space between an '&' and a bare word because
# it may turn into a function evaluation, like here
# between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
$opts{rdonly} = (($opts{mode} & O_ACCMODE) == O_RDONLY);
----------

        'extrude1' => <<'----------',
# do not break before the ++
print $x++ . "\n";
----------

        'extrude2' => <<'----------',
    if (-l pid_filename()) {
	return readlink(pid_filename());
    }
----------

        'extrude3' => <<'----------',
# Breaking before a ++ can cause perl to guess wrong
print( ( $i++ & 1 ) ? $_ : ( $change{$_} || $_ ) );

# Space between '&' and 'O_ACCMODE' is essential here
$opts{rdonly} = (($opts{mode} & O_ACCMODE) == O_RDONLY);
----------

        'extrude4' => <<'----------',
# From Safe.pm caused trouble with extrude
use Opcode 1.01, qw(
  opset opset_to_ops opmask_add
  empty_opset full_opset invert_opset verify_opset
  opdesc opcodes opmask define_optag opset_to_hex
);
----------

        'fabrice_bug' => <<'----------',
# no space around ^variable with -bt=0
my $before = ${^PREMATCH};
my $after  = ${PREMATCH};
----------

        'format1' => <<'----------',
    if (/^--list$/o) {
        format =
@<<<<<<<<<<<<<<<<<<<<<<<< 	@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$_, $val
.
          print "Available strips:\n";
        for ( split ( /\|/, $known_strips ) ) {
            $val = $defs{$_}{'name'};
            write;
        }
    }
----------

        'given1' => <<'----------',
	given ([9,"a",11]) {
		when (qr/\d/)  {
				given ($count) {
					when (1)      { ok($count==1) }
						else { ok($count!=1) }
					when ([5,6])  { ok(0) } else { ok(1) }
				}
			    }
		ok(1) when 11;
	}
----------

        'gnu1' => <<'----------',
@common_sometimes = (
    "aclocal.m4", "acconfig.h", "config.h.top", "config.h.bot",
    "stamp-h.in", 'stamp-vti'
);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'ce_wn1.ce_wn' => {
            source => "ce_wn1",
            params => "ce_wn",
            expect => <<'#1...........',
if ($BOLD_MATH) { (
    $labels, $comment,
    join( '', ' < B > ', &make_math( $mode, '', '', $_ ), ' < /B>' )
) } else { (
    &process_math_in_latex( $mode, $math_style, $slevel, "\\mbox{$text}" ),
    $after
) }
#1...........
        },

        'ce_wn1.def' => {
            source => "ce_wn1",
            params => "def",
            expect => <<'#2...........',
if ($BOLD_MATH) {
    (
        $labels, $comment,
        join( '', ' < B > ', &make_math( $mode, '', '', $_ ), ' < /B>' )
    )
}
else {
    (
        &process_math_in_latex( $mode, $math_style, $slevel, "\\mbox{$text}" ),
        $after
    )
}
#2...........
        },

        'colin.colin' => {
            source => "colin",
            params => "colin",
            expect => <<'#3...........',
env(0, 15, 0, 10, {
    Xtitle => 'X-data',
    Ytitle => 'Y-data',
    Title  => 'An example of errb and points',
    Font   => 'Italic'
});
#3...........
        },

        'colin.def' => {
            source => "colin",
            params => "def",
            expect => <<'#4...........',
env(
    0, 15, 0, 10,
    {
        Xtitle => 'X-data',
        Ytitle => 'Y-data',
        Title  => 'An example of errb and points',
        Font   => 'Italic'
    }
);
#4...........
        },

        'essential.def' => {
            source => "essential",
            params => "def",
            expect => <<'#5...........',
# Run with mangle to squeeze out the white space
# also run with extrude

# never combine two bare words or numbers
status and ::ok(1);

return ::spw(...);

for bla::bla:: abc;

# do not combine 'overload::' and 'and'
if $self->{bareStringify}
  and ref $_
  and defined %overload::
  and defined &{'overload::StrVal'};

# do not combine 'SINK' and 'if'
my $size = -s ::SINK if $file;

# do not combine to make $inputeq"quit"
if ( $input eq "quit" );

# do not combine a number with a concatenation dot to get a float '78.'
$vt100_compatible ? "\e[0;0H" : ( '-' x 78 . "\n" );

# do not join a minus with a bare word, because you might form
# a file test operator.  Here  "z-i" would be taken as a file test.
if ( CORE::abs( $z - i ) < $eps );

# '= -' should not become =- or you will get a warning

# and something like these could become ambiguous without space
# after the '-':
use constant III => 1;
$a = $b - III;
$a = - III;

# keep a space between a token ending in '$' and any word;
die @$ if $@;

# avoid combining tokens to create new meanings. Example:
# this must not become $a++$b
$a + +$b;

# another example: do not combine these two &'s:
allow_options & &OPT_EXECCGI;

# Perl is sensitive to whitespace after the + here:
$b = xvals $a + 0.1 * yvals $a;

# keep paren separate here:
use Foo::Bar ();

# need space after foreach my; for example, this will fail in
# older versions of Perl:
foreach my $ft (@filetypes) ...

  # must retain space between grep and left paren; "grep(" may fail
  my $match = grep ( m/^-extrude$/, @list ) ? 1 : 0;

# don't stick numbers next to left parens, as in:
use Mail::Internet 1.28 ();

# do not remove space between an '&' and a bare word because
# it may turn into a function evaluation, like here
# between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
$opts{rdonly} = ( ( $opts{mode} & O_ACCMODE ) == O_RDONLY );
#5...........
        },

        'essential.essential1' => {
            source => "essential",
            params => "essential1",
            expect => <<'#6...........',
# Run with mangle to squeeze out the white space
# also run with extrude
# never combine two bare words or numbers
status and ::ok(1);
return ::spw(...);
for bla::bla:: abc;
# do not combine 'overload::' and 'and'
if$self->{bareStringify}and ref$_ and defined%overload:: and defined&{'overload::StrVal'};
# do not combine 'SINK' and 'if'
my$size=-s::SINK if$file;
# do not combine to make $inputeq"quit"
if($input eq"quit");
# do not combine a number with a concatenation dot to get a float '78.'
$vt100_compatible?"\e[0;0H":('-' x 78 ."\n");
# do not join a minus with a bare word, because you might form
# a file test operator.  Here  "z-i" would be taken as a file test.
if(CORE::abs($z- i)<$eps);
# '= -' should not become =- or you will get a warning
# and something like these could become ambiguous without space
# after the '-':
use constant III=>1;
$a=$b- III;
$a=- III;
# keep a space between a token ending in '$' and any word;
die@$ if$@;
# avoid combining tokens to create new meanings. Example:
# this must not become $a++$b
$a+ +$b;
# another example: do not combine these two &'s:
allow_options& &OPT_EXECCGI;
# Perl is sensitive to whitespace after the + here:
$b=xvals$a + 0.1*yvals$a;
# keep paren separate here:
use Foo::Bar ();
# need space after foreach my; for example, this will fail in
# older versions of Perl:
foreach my$ft(@filetypes)...
  # must retain space between grep and left paren; "grep(" may fail
  my$match=grep (m/^-extrude$/,@list)?1:0;
# don't stick numbers next to left parens, as in:
use Mail::Internet 1.28 ();
# do not remove space between an '&' and a bare word because
# it may turn into a function evaluation, like here
# between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
$opts{rdonly}=(($opts{mode}& O_ACCMODE)==O_RDONLY);
#6...........
        },

        'essential.essential2' => {
            source => "essential",
            params => "essential2",
            expect => <<'#7...........',
# Run with mangle to squeeze out the white space
# also run with extrude
# never combine two bare words or numbers
status
and
::ok(
1
)
;
return
::spw(
...
)
;
for
bla::bla::
abc
;
# do not combine 'overload::' and 'and'
if
$self
->
{bareStringify}
and
ref
$_
and
defined
%overload::
and
defined
&{
'overload::StrVal'
}
;
# do not combine 'SINK' and 'if'
my$size
=
-s::SINK
if
$file
;
# do not combine to make $inputeq"quit"
if
(
$input
eq
"quit"
)
;
# do not combine a number with a concatenation dot to get a float '78.'
$vt100_compatible?
"\e[0;0H"
:
(
'-'
x
78
.
"\n"
)
;
# do not join a minus with a bare word, because you might form
# a file test operator.  Here  "z-i" would be taken as a file test.
if
(
CORE::abs
(
$z
-
i
)
<
$eps
)
;
# '= -' should not become =- or you will get a warning
# and something like these could become ambiguous without space
# after the '-':
use
constant
III=>
1
;
$a
=
$b
-
III
;
$a
=
-
III
;
# keep a space between a token ending in '$' and any word;
die
@$
if
$@
;
# avoid combining tokens to create new meanings. Example:
# this must not become $a++$b
$a
+
+
$b
;
# another example: do not combine these two &'s:
allow_options
&
&OPT_EXECCGI
;
# Perl is sensitive to whitespace after the + here:
$b
=
xvals$a
+
0.1
*
yvals
$a
;
# keep paren separate here:
use
Foo::Bar (
)
;
# need space after foreach my; for example, this will fail in
# older versions of Perl:
foreach
my$ft
(
@filetypes
)
...
# must retain space between grep and left paren; "grep(" may fail
my$match
=
grep
(
m/^-extrude$/
,
@list
)
?
1
:
0
;
# don't stick numbers next to left parens, as in:
use
Mail::Internet
1.28
(
)
;
# do not remove space between an '&' and a bare word because
# it may turn into a function evaluation, like here
# between '&' and 'O_ACCMODE', producing a syntax error [File.pm]
$opts{rdonly}
=
(
(
$opts{mode}
&
O_ACCMODE
)
==
O_RDONLY
)
;
#7...........
        },

        'extrude1.def' => {
            source => "extrude1",
            params => "def",
            expect => <<'#8...........',
# do not break before the ++
print $x++ . "\n";
#8...........
        },

        'extrude1.extrude' => {
            source => "extrude1",
            params => "extrude",
            expect => <<'#9...........',
# do not break before the ++
print$x++
.
"\n"
;
#9...........
        },

        'extrude2.def' => {
            source => "extrude2",
            params => "def",
            expect => <<'#10...........',
    if ( -l pid_filename() ) {
        return readlink( pid_filename() );
    }
#10...........
        },

        'extrude2.extrude' => {
            source => "extrude2",
            params => "extrude",
            expect => <<'#11...........',
if
(
-l pid_filename(
)
)
{
return
readlink
(
pid_filename(
)
)
;
}
#11...........
        },

        'extrude3.def' => {
            source => "extrude3",
            params => "def",
            expect => <<'#12...........',
# Breaking before a ++ can cause perl to guess wrong
print( ( $i++ & 1 ) ? $_ : ( $change{$_} || $_ ) );

# Space between '&' and 'O_ACCMODE' is essential here
$opts{rdonly} = ( ( $opts{mode} & O_ACCMODE ) == O_RDONLY );
#12...........
        },

        'extrude3.extrude' => {
            source => "extrude3",
            params => "extrude",
            expect => <<'#13...........',
# Breaking before a ++ can cause perl to guess wrong
print
(
(
$i++
&
1
)
?
$_
:
(
$change{
$_
}
||
$_
)
)
;
# Space between '&' and 'O_ACCMODE' is essential here
$opts{rdonly}
=
(
(
$opts{mode}
&
O_ACCMODE
)
==
O_RDONLY
)
;
#13...........
        },

        'extrude4.def' => {
            source => "extrude4",
            params => "def",
            expect => <<'#14...........',
# From Safe.pm caused trouble with extrude
use Opcode 1.01, qw(
  opset opset_to_ops opmask_add
  empty_opset full_opset invert_opset verify_opset
  opdesc opcodes opmask define_optag opset_to_hex
);
#14...........
        },

        'extrude4.extrude' => {
            source => "extrude4",
            params => "extrude",
            expect => <<'#15...........',
# From Safe.pm caused trouble with extrude
use
Opcode
1.01
,
qw(
opset opset_to_ops opmask_add
empty_opset full_opset invert_opset verify_opset
opdesc opcodes opmask define_optag opset_to_hex
)
;
#15...........
        },

        'fabrice_bug.def' => {
            source => "fabrice_bug",
            params => "def",
            expect => <<'#16...........',
# no space around ^variable with -bt=0
my $before = ${^PREMATCH};
my $after  = ${PREMATCH};
#16...........
        },

        'fabrice_bug.fabrice_bug' => {
            source => "fabrice_bug",
            params => "fabrice_bug",
            expect => <<'#17...........',
# no space around ^variable with -bt=0
my $before = ${^PREMATCH};
my $after  = ${ PREMATCH };
#17...........
        },

        'format1.def' => {
            source => "format1",
            params => "def",
            expect => <<'#18...........',
    if (/^--list$/o) {
        format =
@<<<<<<<<<<<<<<<<<<<<<<<< 	@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$_, $val
.
        print "Available strips:\n";
        for ( split( /\|/, $known_strips ) ) {
            $val = $defs{$_}{'name'};
            write;
        }
    }
#18...........
        },

        'given1.def' => {
            source => "given1",
            params => "def",
            expect => <<'#19...........',
        given ( [ 9, "a", 11 ] ) {
            when (qr/\d/) {
                given ($count) {
                    when (1)          { ok( $count == 1 ) }
                    else              { ok( $count != 1 ) }
                    when ( [ 5, 6 ] ) { ok(0) }
                    else              { ok(1) }
                }
            }
            ok(1) when 11;
        }
#19...........
        },

        'gnu1.def' => {
            source => "gnu1",
            params => "def",
            expect => <<'#20...........',
@common_sometimes = (
    "aclocal.m4", "acconfig.h", "config.h.top", "config.h.bot",
    "stamp-h.in", 'stamp-vti'
);
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
