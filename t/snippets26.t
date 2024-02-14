# Created with: ./make_t.pl

# Contents:
#1 bal.bal2
#2 bal.def
#3 lpxl.lpxl6
#4 c133.c133
#5 c133.def
#6 git93.def
#7 git93.git93
#8 c139.def
#9 drc.def
#10 drc.drc
#11 git105.def
#12 git106.def
#13 git106.git106
#14 c154.def
#15 code_skipping.code_skipping
#16 c158.def
#17 git108.def
#18 git108.git108
#19 wtc.def

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
        'bal2'          => "-bal=2",
        'c133'          => "-boc",
        'code_skipping' => <<'----------',
# same as the default but tests -cs -csb and -cse
--code-skipping
--code-skipping-begin='#<<V'
--code-skipping-end='#>>V'
----------
        'def'    => "",
        'drc'    => "-ndrc",
        'git106' => "-xlp -gnu -xci",
        'git108' => "-wn -wfc",
        'git93'  => <<'----------',
-vxl='q'
----------
        'lpxl6' => <<'----------',
# equivalent to -lpxl='{ [ F(2'
-lp -lpil='f(2'
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'bal' => <<'----------',
{
  L1:
  L2:
  L3: return;
};
----------

        'c133' => <<'----------',
# this will make 1 line unless -boc is used
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)
);

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
----------

        'c139' => <<'----------',
# The '&' has trailing spaces
@l = &    
_  
( -49, -71 );

# This '$' has trailing spaces
my $    
b = 40;

# this arrow has trailing spaces
$r = $c->         
sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );

# spaces and blank line
@l = &    

_  
( -49, -71 );

# spaces and blank line
$r = $c->         

sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );
----------

        'c154' => <<'----------',
{{{{
for (
    $order =
    $start_order * $nbSubOrderByOrder + $start_suborder ;
    !exists $level_hash{$level}->{$order}
    and $order <=
    $stop_order * $nbSubOrderByOrder + $stop_suborder ;
    $order++
  )
{
}

# has comma
for (
    $q = 201 ;
    print '-' x 79,
    "\n" ;
    $g = (
	$f ^ ( $w = ( $z = $m . $e ) ^ substr $e, $q )
	  ^ ( $n = $b ^ $d | $a ^ $l )
    ) & ( $w | $z ^ $f ^ $n ) & ( $l | $g )
  )
{
    ...;
}

for (
    $j = 0, $match_j = -1 ;
    $j < $sub_len
      &&

      # changed from naive_string_matcher
      $sub->[$j] eq $big->[ $i + $j ] ; $j++
  )
{
    ...;
}
}}}}
----------

        'c158' => <<'----------',
my $meta = try { $package->meta }
or die "$package does not have a ->meta method\n";

my ($curr) = current();
err(@_);
----------

        'code_skipping' => <<'----------',
%Hdr=%U2E=%E2U=%Fallback=();
$in_charmap=$nerror=$nwarning=0;
$.=0;
#<<V  code skipping: perltidy will pass this verbatim without error checking

    }}} {{{

#>>V
my $self=shift;
my $cloning=shift;
----------

        'drc' => <<'----------',
ignoreSpec( $file, "file",, \%spec,,, \%Rspec );
----------

        'git105' => <<'----------',
use v5.36;

use experimental 'for_list';

for my ( $k, $v ) ( 1, 2, 3, 4 ) {
    say "$k:$v";
}
say 'end';

----------

        'git106' => <<'----------',
is( $module->VERSION, $expected,
    "$main_module->VERSION matches $module->VERSION ($expected)" );

ok( ( $@ eq "" && "@b" eq "1 4 5 9" ),
    'redefinition should not take effect during the sort' );

&$f(
    ( map { $points->slice($_) } @sls1 ),
    ( map { $n->slice($_) } @sls1 ),
    ( map { $this->{Colors}->slice($_) } @sls1 )
);

AA(
    "0123456789012345678901234567890123456789",
    "0123456789012345678901234567890123456789"
);

AAAAAA(
    "0123456789012345678901234567890123456789",
    "0123456789012345678901234567890123456789"
);

# padded
return !( $elem->isa('PPI::Statement::End')
    || $elem->isa('PPI::Statement::Data') );

for (
    $s = $dbobj->seq( $k, $v, R_LAST ) ;
    $s == 0 ;
    $s = $dbobj->seq( $k, $v, R_PREV )
  )
{
    print "$k: $v\n";
}

# excess without -xci
fresh_perl_is( '-C-',
    <<'abcdefghijklmnopq', {}, "ambiguous unary operator check doesn't crash" );
Warning: Use of "-C-" without parentheses is ambiguous at - line 1.
abcdefghijklmnopq

# excess with -xci
{
    {
        {
            $self->privmsg( $to,
                "One moment please, I shall display the groups with agendas:" );
        }
    }
}
----------

        'git108' => <<'----------',
elf->call_method(
    method_name_foo => {
        some_arg1       => $foo,
        some_other_arg3 => $bar->{'baz'},
    }
);

# leading dash
my $species = new Bio::Species(
    -classification => [
        qw(
          sapiens Homo Hominidae
          Catarrhini Primates Eutheria
          Mammalia Vertebrata
          Chordata Metazoa Eukaryota
        )
    ]
);
----------

        'git93' => <<'----------',
use Cwd qw[cwd];
use Carp qw(carp);
use IPC::Cmd qw{can_run run QUOTE};
use File::Path qw/mkpath/;
use File::Temp qw[tempdir];
use Params::Check qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue ) => 'blue' ],
    [ qw( brown blue blue blue ) => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
----------

        'lpxl' => <<'----------',
# simple function call
my $loanlength = getLoanLength(
                                $borrower->{'categorycode'},    # sc1
                                $iteminformation->{'itemtype'},
                                $borrower->{'branchcode'}       # sc3
);

# function call, more than one level deep
my $o = very::long::class::name->new(
    {
        propA => "a",
        propB => "b",
        propC => "c",
    }
);

# function call with sublist
debug(
      "Connecting to DB.",
      "Extra-Parameters: " . join("<->", $extra_parms),
      "Config: " . join("<->", %config)
     );

# simple function call with code block
$m->command(-label   => 'Save',
            -command => sub { print "DOS\n"; save_dialog($win); });

# function call, ternary in list
return
  OptArgs2::Result->usage(
    $style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage',
    'usage: ' . $usage . "\n" );

# not a function call
%blastparam = (
    -run            => \%runparam,
    -file           => '',
    -parse          => 1,
    -signif         => 1e-5,
);

# 'local' is a keyword, not a user function
    local (
        $len,    $pts,      @colspec, $char, $cols,
        $repeat, $celldata, $at_text, $after_text
    );

# square bracket with sublists
$data = [
         ListElem->new(id => 0, val => 100),
         ListElem->new(id => 2, val => 50),
         ListElem->new(id => 1, val => 10),
        ];

# curly brace with sublists
$behaviour = {
              cat   => {nap    => "lap",   eat  => "meat"},
              dog   => {prowl  => "growl", pool => "drool"},
              mouse => {nibble => "kibble"},
             };
----------

        'wtc' => <<'----------',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney", ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart", ],
);

# single line
( $name, $body ) = ( $2, $3, );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
      %$item,
      text => $leaf,
      color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle'
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create  => 1, );
    },
)->pack( -side => 'left', );

my $no_index_1_1 =
  { 'map' =>
      { ':key' => { name => \&string, list => { value => \&string }, }, }, };


----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'bal.bal2' => {
            source => "bal",
            params => "bal2",
            expect => <<'#1...........',
{
  L1: L2: L3: return;
};
#1...........
        },

        'bal.def' => {
            source => "bal",
            params => "def",
            expect => <<'#2...........',
{
  L1:
  L2:
  L3: return;
};
#2...........
        },

        'lpxl.lpxl6' => {
            source => "lpxl",
            params => "lpxl6",
            expect => <<'#3...........',
# simple function call
my $loanlength = getLoanLength(
                                $borrower->{'categorycode'},    # sc1
                                $iteminformation->{'itemtype'},
                                $borrower->{'branchcode'}       # sc3
);

# function call, more than one level deep
my $o = very::long::class::name->new(
    {
        propA => "a",
        propB => "b",
        propC => "c",
    }
);

# function call with sublist
debug(
    "Connecting to DB.",
    "Extra-Parameters: " . join( "<->", $extra_parms ),
    "Config: " . join( "<->", %config )
);

# simple function call with code block
$m->command(
    -label   => 'Save',
    -command => sub { print "DOS\n"; save_dialog($win); }
);

# function call, ternary in list
return OptArgs2::Result->usage(
    $style == OptArgs2::STYLE_FULL ? 'FullUsage' : 'NormalUsage',
    'usage: ' . $usage . "\n" );

# not a function call
%blastparam = (
    -run    => \%runparam,
    -file   => '',
    -parse  => 1,
    -signif => 1e-5,
);

# 'local' is a keyword, not a user function
local (
    $len,    $pts,      @colspec, $char, $cols,
    $repeat, $celldata, $at_text, $after_text
);

# square bracket with sublists
$data = [
    ListElem->new( id => 0, val => 100 ),
    ListElem->new( id => 2, val => 50 ),
    ListElem->new( id => 1, val => 10 ),
];

# curly brace with sublists
$behaviour = {
    cat   => { nap    => "lap",   eat  => "meat" },
    dog   => { prowl  => "growl", pool => "drool" },
    mouse => { nibble => "kibble" },
};
#3...........
        },

        'c133.c133' => {
            source => "c133",
            params => "c133",
            expect => <<'#4...........',
# this will make 1 line unless -boc is used
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)
);

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
#4...........
        },

        'c133.def' => {
            source => "c133",
            params => "def",
            expect => <<'#5...........',
# this will make 1 line unless -boc is used
return ( $x * cos($a) - $y * sin($a), $x * sin($a) + $y * cos($a) );

# broken list - issue c133
return (
    $x * cos($a) - $y * sin($a),
    $x * sin($a) + $y * cos($a)

);

# no parens
return
  $x * cos($a) - $y * sin($a),
  $x * sin($a) + $y * cos($a);
#5...........
        },

        'git93.def' => {
            source => "git93",
            params => "def",
            expect => <<'#6...........',
use Cwd                       qw[cwd];
use Carp                      qw(carp);
use IPC::Cmd                  qw{can_run run QUOTE};
use File::Path                qw/mkpath/;
use File::Temp                qw[tempdir];
use Params::Check             qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue )     => 'blue' ],
    [ qw( brown blue blue blue )    => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword       { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
#6...........
        },

        'git93.git93' => {
            source => "git93",
            params => "git93",
            expect => <<'#7...........',
use Cwd qw[cwd];
use Carp qw(carp);
use IPC::Cmd qw{can_run run QUOTE};
use File::Path qw/mkpath/;
use File::Temp qw[tempdir];
use Params::Check qw<check>;
use Module::Load::Conditional qw#can_load#;
use Locale::Maketext::Simple Style => 'gettext';    # does not align

# do not align on these 'q' token types - not use statements...
my $gene_color_sets = [
    [ qw( blue blue blue blue )     => 'blue' ],
    [ qw( brown blue blue blue )    => 'brown' ],
    [ qw( brown brown green green ) => 'brown' ],
];

sub quux : PluginKeyword       { 'quux' }
sub qaax : PluginKeyword(qiix) { die "unimplemented" }

use vars qw($curdir);
no strict qw(vars);
#7...........
        },

        'c139.def' => {
            source => "c139",
            params => "def",
            expect => <<'#8...........',
# The '&' has trailing spaces
@l = &_( -49, -71 );

# This '$' has trailing spaces
my $b = 40;

# this arrow has trailing spaces
$r = $c->sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );

# spaces and blank line
@l = &

  _( -49, -71 );

# spaces and blank line
$r = $c->

  sql_set_env_attr( $evh, $SQL_ATTR_ODBC_VERSION, $SQL_OV_ODBC2, 0 );
#8...........
        },

        'drc.def' => {
            source => "drc",
            params => "def",
            expect => <<'#9...........',
ignoreSpec( $file, "file", \%spec, \%Rspec );
#9...........
        },

        'drc.drc' => {
            source => "drc",
            params => "drc",
            expect => <<'#10...........',
ignoreSpec( $file, "file",, \%spec,,, \%Rspec );
#10...........
        },

        'git105.def' => {
            source => "git105",
            params => "def",
            expect => <<'#11...........',
use v5.36;

use experimental 'for_list';

for my ( $k, $v ) ( 1, 2, 3, 4 ) {
    say "$k:$v";
}
say 'end';

#11...........
        },

        'git106.def' => {
            source => "git106",
            params => "def",
            expect => <<'#12...........',
is( $module->VERSION, $expected,
    "$main_module->VERSION matches $module->VERSION ($expected)" );

ok( ( $@ eq "" && "@b" eq "1 4 5 9" ),
    'redefinition should not take effect during the sort' );

&$f(
    ( map { $points->slice($_) } @sls1 ),
    ( map { $n->slice($_) } @sls1 ),
    ( map { $this->{Colors}->slice($_) } @sls1 )
);

AA(
    "0123456789012345678901234567890123456789",
    "0123456789012345678901234567890123456789"
);

AAAAAA(
    "0123456789012345678901234567890123456789",
    "0123456789012345678901234567890123456789"
);

# padded
return !( $elem->isa('PPI::Statement::End')
    || $elem->isa('PPI::Statement::Data') );

for (
    $s = $dbobj->seq( $k, $v, R_LAST ) ;
    $s == 0 ;
    $s = $dbobj->seq( $k, $v, R_PREV )
  )
{
    print "$k: $v\n";
}

# excess without -xci
fresh_perl_is( '-C-',
    <<'abcdefghijklmnopq', {}, "ambiguous unary operator check doesn't crash" );
Warning: Use of "-C-" without parentheses is ambiguous at - line 1.
abcdefghijklmnopq

# excess with -xci
{
    {
        {
            $self->privmsg( $to,
                "One moment please, I shall display the groups with agendas:" );
        }
    }
}
#12...........
        },

        'git106.git106' => {
            source => "git106",
            params => "git106",
            expect => <<'#13...........',
is($module->VERSION, $expected,
   "$main_module->VERSION matches $module->VERSION ($expected)");

ok(($@ eq "" && "@b" eq "1 4 5 9"),
   'redefinition should not take effect during the sort');

&$f((map { $points->slice($_) } @sls1),
    (map { $n->slice($_) } @sls1),
    (map { $this->{Colors}->slice($_) } @sls1));

AA("0123456789012345678901234567890123456789",
   "0123456789012345678901234567890123456789");

AAAAAA("0123456789012345678901234567890123456789",
       "0123456789012345678901234567890123456789");

# padded
return !(   $elem->isa('PPI::Statement::End')
         || $elem->isa('PPI::Statement::Data'));

for ($s = $dbobj->seq($k, $v, R_LAST) ;
     $s == 0 ;
     $s = $dbobj->seq($k, $v, R_PREV))
{
    print "$k: $v\n";
}

# excess without -xci
fresh_perl_is('-C-',
     <<'abcdefghijklmnopq', {}, "ambiguous unary operator check doesn't crash");
Warning: Use of "-C-" without parentheses is ambiguous at - line 1.
abcdefghijklmnopq

# excess with -xci
{
    {
        {
            $self->privmsg($to,
                   "One moment please, I shall display the groups with agendas:"
            );
        }
    }
}
#13...........
        },

        'c154.def' => {
            source => "c154",
            params => "def",
            expect => <<'#14...........',
{
    {
        {
            {
                for (
                    $order =
                      $start_order * $nbSubOrderByOrder + $start_suborder ;
                    !exists $level_hash{$level}->{$order}
                      and $order <=
                      $stop_order * $nbSubOrderByOrder + $stop_suborder ;
                    $order++
                  )
                {
                }

                # has comma
                for (
                    $q = 201 ;
                    print '-' x 79, "\n" ;
                    $g = (
                        $f ^ ( $w = ( $z = $m . $e ) ^ substr $e, $q )
                          ^ ( $n = $b ^ $d | $a ^ $l )
                    ) & ( $w | $z ^ $f ^ $n ) & ( $l | $g )
                  )
                {
                    ...;
                }

                for (
                    $j = 0, $match_j = -1 ;
                    $j < $sub_len
                      &&

                      # changed from naive_string_matcher
                      $sub->[$j] eq $big->[ $i + $j ] ;
                    $j++
                  )
                {
                    ...;
                }
            }
        }
    }
}
#14...........
        },

        'code_skipping.code_skipping' => {
            source => "code_skipping",
            params => "code_skipping",
            expect => <<'#15...........',
%Hdr        = %U2E    = %E2U      = %Fallback = ();
$in_charmap = $nerror = $nwarning = 0;
$.          = 0;
#<<V  code skipping: perltidy will pass this verbatim without error checking

    }}} {{{

#>>V
my $self    = shift;
my $cloning = shift;
#15...........
        },

        'c158.def' => {
            source => "c158",
            params => "def",
            expect => <<'#16...........',
my $meta = try { $package->meta }
  or die "$package does not have a ->meta method\n";

my ($curr) = current();
err(@_);
#16...........
        },

        'git108.def' => {
            source => "git108",
            params => "def",
            expect => <<'#17...........',
elf->call_method(
    method_name_foo => {
        some_arg1       => $foo,
        some_other_arg3 => $bar->{'baz'},
    }
);

# leading dash
my $species = new Bio::Species(
    -classification => [
        qw(
          sapiens Homo Hominidae
          Catarrhini Primates Eutheria
          Mammalia Vertebrata
          Chordata Metazoa Eukaryota
        )
    ]
);
#17...........
        },

        'git108.git108' => {
            source => "git108",
            params => "git108",
            expect => <<'#18...........',
elf->call_method( method_name_foo => {
    some_arg1       => $foo,
    some_other_arg3 => $bar->{'baz'},
} );

# leading dash
my $species = new Bio::Species( -classification => [ qw(
    sapiens Homo Hominidae
    Catarrhini Primates Eutheria
    Mammalia Vertebrata
    Chordata Metazoa Eukaryota
) ] );
#18...........
        },

        'wtc.def' => {
            source => "wtc",
            params => "def",
            expect => <<'#19...........',
# both single and multiple line lists:
@LoL = (
    [ "fred",   "barney", ],
    [ "george", "jane",  "elroy" ],
    [ "homer",  "marge", "bart", ],
);

# single line
( $name, $body ) = ( $2, $3, );

# multiline, but not bare
$text = $main->Scrolled( TextUndo, $yyy, $zzz, $wwwww,
    selectbackgroundxxxxx => 'yellow', );

# this will pass for 'h'
my $new = {
    %$item,
    text  => $leaf,
    color => 'green',
};

# matches 'i'
my @list = (

    $xx,
    $yy
);

# does not match 'h'
$c1->create(
    'rectangle', 40, 60, 80, 80,
    -fill => 'red',
    -tags => 'rectangle'
);

$dasm_frame->Button(
    -text    => 'Locate',
    -command => sub {
        $target_binary = $fs->Show( -popover => 'cursor', -create => 1, );
    },
)->pack( -side => 'left', );

my $no_index_1_1 =
  { 'map' =>
      { ':key' => { name => \&string, list => { value => \&string }, }, }, };

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
