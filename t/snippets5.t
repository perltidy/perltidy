# Created with: ./make_t.pl

# Contents:
#1 list1.def
#2 listop1.def
#3 listop2.def
#4 lp1.def
#5 lp1.lp
#6 mangle1.def
#7 mangle1.mangle
#8 mangle2.def
#9 mangle2.mangle
#10 mangle3.def
#11 mangle3.mangle
#12 math1.def
#13 math2.def
#14 math3.def
#15 math4.def
#16 nasc.def
#17 nasc.nasc
#18 nothing.def
#19 nothing.nothing
#20 otr1.def

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
        'def'     => "",
        'lp'      => "-lp",
        'mangle'  => "--mangle",
        'nasc'    => "-nasc",
        'nothing' => "",
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'list1' => <<'----------',
%height=("letter",27.9, "legal",35.6, "arche",121.9, "archd",91.4, "archc",61,
 "archb",45.7, "archa",30.5, "flsa",33, "flse",33, "halfletter",21.6,
 "11x17",43.2, "ledger",27.9);
%width=("letter",21.6, "legal",21.6, "arche",91.4, "archd",61, "archc",45.7,
 "archb",30.5, "archa",22.9, "flsa",21.6, "flse",21.6, "halfletter",14,
 "11x17",27.9, "ledger",43.2);
----------

        'listop1' => <<'----------',
my @sorted = map { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map { [ $_, rand ] } @list;
----------

        'listop2' => <<'----------',
my @sorted =
  map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;
----------

        'lp1' => <<'----------',
# a good test problem for -lp; thanks to Ian Stuart
push @contents,
  $c->table(
 { -border => '1' },
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   " Author ",
   $c->textfield(
    -tabindex => "1",
    -name => "author",
    -default => "$author",
    -size => '20'
   )
  ),
  $c->td(
   $c->strong(" Publication Date "),
   $c->textfield(
    -tabindex => "2",
    -name => "pub_date",
    -default => "$pub_date",
    -size => '20'
   ),
  )
 ),
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   { -colspan => '2' },
   $c->strong("Title"),
   $c->textfield(
    -tabindex => "3",
    -name => "title",
    -default => "$title",
    -override => '1',
    -size => '40'
   ),
  )
 ),
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   $c->table(
    $c->Tr(
     $c->td( { -valign => 'top' }, $c->strong(" Document Type ") ),
     $c->td(
      { -valign => 'top' },
      $c->scrolling_list(
       -tabindex => "4",
       -name => "doc_type",
       -values => [@docCodeValues],
       -labels => \%docCodeLabels,
       -default => "$doc_type"
      )
     )
    )
   )
  ),
  $c->td(
   $c->table(
    $c->Tr(
     $c->td(
      { -valign => 'top' },
      $c->strong( " Relevant Discipline ", $c->br(), "Area " )
     ),
     $c->td(
      { -valign => 'top' },
      $c->scrolling_list(
       -tabindex => "5",
       -name => "discipline",
       -values => [@discipValues],
       -labels => \%discipLabels,
       -default => "$discipline"
      ),
     )
    )
   )
  )
 ),
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   { -colspan => '2' },
   $c->table(
    $c->Tr(
     $c->td(
      { -valign => 'top' }, $c->strong(" Relevant Subject Area "),
      $c->br(), "You may select multiple areas",
     ),
     $c->td(
      { -valign => 'top' },
      $c->checkbox_group(
       -tabindex => "6",
       -name => "subject",
       -values => [@subjValues],
       -labels => \%subjLabels,
       -defaults => [@subject],
       -rows => "2"
      )
     )
    )
   )
  )
 ),
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   { -colspan => '2' },
   $c->strong("Location<BR>"),
   $c->small("(ie, where to find it)"),
   $c->textfield(
    -tabindex => "7",
    -name => "location",
    -default => "$location",
    -size => '40'
   )
  )
 ),
 $c->Tr(
  { -valign => 'top' },
  $c->td(
   { -colspan => '2' },
   $c->table(
    $c->Tr(
     $c->td(
      { -valign => 'top' }, "Description",
      $c->br(), $c->small("Maximum 750 letters.")
     ),
     $c->td(
      { -valign => 'top' },
      $c->textarea(
       -tabindex => "8",
       -name => "description",
       -default => "$description",
       -wrap => "soft",
       -rows => '10',
       -columns => '60'
      )
     )
    )
   )
  )
 ),
  );
----------

        'mangle1' => <<'----------',
# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  ".my_wrap("","          ",$v) : $v;
----------

        'mangle2' => <<'----------',
# hanging side comments - do not remove leading space with -mangle
if ( $size1 == 0 || $size2 == 0 ) {    # special handling for zero-length
    if ( $size2 + $size1 == 0 ) {      # files.
        exit 0;
    }
    else {                             # Can't we say 'differ at byte zero'
                                       # and so on here?  That might make
                                       # more sense than this behavior.
                                       # Also, this should be made consistent
                                       # with the behavior when skip >=
                                       # filesize.
        if ($volume) {
            warn "$0: EOF on $file1\n" unless $size1;
            warn "$0: EOF on $file2\n" unless $size2;
        }
        exit 1;
    }
}

----------

        'mangle3' => <<'----------',
# run with --mangle
# Troublesome punctuation variables: $$ and $#

# don't delete ws between '$$' and 'if'
kill 'ABRT', $$ if $panic++;

# Do not remove the space between '$#' and 'eq'
$, = "Hello, World!\n";
$#=$,; 
print "$# ";
$# eq $,? print "yes\n" : print "no\n";

# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  ".my_wrap("","          ",$v) : $v;

# must not remove space before 'CAKE'
use constant CAKE => atan2(1,1)/2;
if ($arc >= - CAKE && $arc <= CAKE) {
}

# do not remove the space after 'JUNK':
print JUNK ("<","&",">")[rand(3)];# make these a bit more likely
----------

        'math1' => <<'----------',
my $xyz_shield = [ [ -0.060,  -0.060,  0. ],
                   [ 0.060,   -0.060,  0. ],
                   [ 0.060,   0.060,   0. ],
                   [ -0.060,  0.060,   0. ],
                   [ -0.0925, -0.0925, 0.092 ],
                   [ 0.0925,  -0.0925, 0.092 ],
                   [ 0.0925,  0.0925,  0.092 ],
                   [ -0.0925, 0.0925,  0.092 ], ];
----------

        'math2' => <<'----------',
$ans = pdl(
           [0, 0, 0, 0, 0],
           [0, 0, 2, 0, 0],
           [0, 1, 5, 2, 0],
           [0, 0, 4, 0, 0],
           [0, 0, 0, 0, 0]
           );
----------

        'math3' => <<'----------',
    my ( $x, $y ) = ( $x0 + $index_x * $xgridwidth * $xm + ( $map_x * $xm * $xgridwidth ) / $detailwidth, $y0 - $index_y * $ygridwidth * $ym - ( $map_y * $ym * $ygridwidth ) / $detailheight,);
----------

        'math4' => <<'----------',
my$u=($range*$pratio**(1./3.))/$wratio;
my$factor=exp(-(18/$u)**4);
my$ovp=(1-$factor)*(70-0.655515*$u)+(1000/($u**1.3)+10000/($u**3.3))*$factor;
my$impulse=(1-$factor)*(170-$u)+(350/$u**0.65+500/$u**5)*$factor;
$ovp=$ovp*$pratio;
$impulse=$impulse*$wratio*$pratio**(2/3);
----------

        'nasc' => <<'----------',
	# will break and add semicolon unless -nasc is given
        eval { $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed } };
----------

        'nothing' => <<'----------',
----------

        'otr1' => <<'----------',
return $pdl->slice(
    join ',',
    (
        map {
                $_ eq "X" ? ":"
              : ref $_ eq "ARRAY" ? join ':', @$_
              : !ref $_ ? $_
              : die "INVALID SLICE DEF $_"
        } @_
    )
);
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'list1.def' => {
            source => "list1",
            params => "def",
            expect => <<'#1...........',
%height = (
    "letter",     27.9, "legal", 35.6, "arche",  121.9,
    "archd",      91.4, "archc", 61,   "archb",  45.7,
    "archa",      30.5, "flsa",  33,   "flse",   33,
    "halfletter", 21.6, "11x17", 43.2, "ledger", 27.9
);
%width = (
    "letter",     21.6, "legal", 21.6, "arche",  91.4,
    "archd",      61,   "archc", 45.7, "archb",  30.5,
    "archa",      22.9, "flsa",  21.6, "flse",   21.6,
    "halfletter", 14,   "11x17", 27.9, "ledger", 43.2
);
#1...........
        },

        'listop1.def' => {
            source => "listop1",
            params => "def",
            expect => <<'#2...........',
my @sorted = map { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map { [ $_, rand ] } @list;
#2...........
        },

        'listop2.def' => {
            source => "listop2",
            params => "def",
            expect => <<'#3...........',
my @sorted =
  map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;
#3...........
        },

        'lp1.def' => {
            source => "lp1",
            params => "def",
            expect => <<'#4...........',
# a good test problem for -lp; thanks to Ian Stuart
push @contents,
  $c->table(
    { -border => '1' },
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            " Author ",
            $c->textfield(
                -tabindex => "1",
                -name     => "author",
                -default  => "$author",
                -size     => '20'
            )
        ),
        $c->td(
            $c->strong(" Publication Date "),
            $c->textfield(
                -tabindex => "2",
                -name     => "pub_date",
                -default  => "$pub_date",
                -size     => '20'
            ),
        )
    ),
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            { -colspan => '2' },
            $c->strong("Title"),
            $c->textfield(
                -tabindex => "3",
                -name     => "title",
                -default  => "$title",
                -override => '1',
                -size     => '40'
            ),
        )
    ),
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            $c->table(
                $c->Tr(
                    $c->td(
                        { -valign => 'top' },
                        $c->strong(" Document Type ")
                    ),
                    $c->td(
                        { -valign => 'top' },
                        $c->scrolling_list(
                            -tabindex => "4",
                            -name     => "doc_type",
                            -values   => [@docCodeValues],
                            -labels   => \%docCodeLabels,
                            -default  => "$doc_type"
                        )
                    )
                )
            )
        ),
        $c->td(
            $c->table(
                $c->Tr(
                    $c->td(
                        { -valign => 'top' },
                        $c->strong(
                            " Relevant Discipline ", $c->br(), "Area "
                        )
                    ),
                    $c->td(
                        { -valign => 'top' },
                        $c->scrolling_list(
                            -tabindex => "5",
                            -name     => "discipline",
                            -values   => [@discipValues],
                            -labels   => \%discipLabels,
                            -default  => "$discipline"
                        ),
                    )
                )
            )
        )
    ),
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            { -colspan => '2' },
            $c->table(
                $c->Tr(
                    $c->td(
                        { -valign => 'top' },
                        $c->strong(" Relevant Subject Area "),
                        $c->br(),
                        "You may select multiple areas",
                    ),
                    $c->td(
                        { -valign => 'top' },
                        $c->checkbox_group(
                            -tabindex => "6",
                            -name     => "subject",
                            -values   => [@subjValues],
                            -labels   => \%subjLabels,
                            -defaults => [@subject],
                            -rows     => "2"
                        )
                    )
                )
            )
        )
    ),
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            { -colspan => '2' },
            $c->strong("Location<BR>"),
            $c->small("(ie, where to find it)"),
            $c->textfield(
                -tabindex => "7",
                -name     => "location",
                -default  => "$location",
                -size     => '40'
            )
        )
    ),
    $c->Tr(
        { -valign => 'top' },
        $c->td(
            { -colspan => '2' },
            $c->table(
                $c->Tr(
                    $c->td(
                        { -valign => 'top' },
                        "Description", $c->br(),
                        $c->small("Maximum 750 letters.")
                    ),
                    $c->td(
                        { -valign => 'top' },
                        $c->textarea(
                            -tabindex => "8",
                            -name     => "description",
                            -default  => "$description",
                            -wrap     => "soft",
                            -rows     => '10',
                            -columns  => '60'
                        )
                    )
                )
            )
        )
    ),
  );
#4...........
        },

        'lp1.lp' => {
            source => "lp1",
            params => "lp",
            expect => <<'#5...........',
# a good test problem for -lp; thanks to Ian Stuart
push @contents,
  $c->table(
             { -border => '1' },
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             " Author ",
                             $c->textfield(
                                            -tabindex => "1",
                                            -name     => "author",
                                            -default  => "$author",
                                            -size     => '20'
                             )
                     ),
                     $c->td(
                             $c->strong(" Publication Date "),
                             $c->textfield(
                                            -tabindex => "2",
                                            -name     => "pub_date",
                                            -default  => "$pub_date",
                                            -size     => '20'
                             ),
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->strong("Title"),
                             $c->textfield(
                                            -tabindex => "3",
                                            -name     => "title",
                                            -default  => "$title",
                                            -override => '1',
                                            -size     => '40'
                             ),
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             $c->table(
                                        $c->Tr(
                                            $c->td(
                                                   { -valign => 'top' },
                                                   $c->strong(" Document Type ")
                                            ),
                                            $c->td(
                                                { -valign => 'top' },
                                                $c->scrolling_list(
                                                    -tabindex => "4",
                                                    -name     => "doc_type",
                                                    -values => [@docCodeValues],
                                                    -labels => \%docCodeLabels,
                                                    -default => "$doc_type"
                                                )
                                            )
                                        )
                             )
                     ),
                     $c->td(
                             $c->table(
                                        $c->Tr(
                                             $c->td(
                                                    { -valign => 'top' },
                                                    $c->strong(
                                                        " Relevant Discipline ",
                                                        $c->br(),
                                                        "Area "
                                                    )
                                             ),
                                             $c->td(
                                                 { -valign => 'top' },
                                                 $c->scrolling_list(
                                                     -tabindex => "5",
                                                     -name     => "discipline",
                                                     -values => [@discipValues],
                                                     -labels => \%discipLabels,
                                                     -default => "$discipline"
                                                 ),
                                             )
                                        )
                             )
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->table(
                                  $c->Tr(
                                      $c->td(
                                          { -valign => 'top' },
                                          $c->strong(" Relevant Subject Area "),
                                          $c->br(),
                                          "You may select multiple areas",
                                      ),
                                      $c->td(
                                              { -valign => 'top' },
                                              $c->checkbox_group(
                                                       -tabindex => "6",
                                                       -name     => "subject",
                                                       -values => [@subjValues],
                                                       -labels => \%subjLabels,
                                                       -defaults => [@subject],
                                                       -rows     => "2"
                                              )
                                      )
                                  )
                             )
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->strong("Location<BR>"),
                             $c->small("(ie, where to find it)"),
                             $c->textfield(
                                            -tabindex => "7",
                                            -name     => "location",
                                            -default  => "$location",
                                            -size     => '40'
                             )
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->table(
                                       $c->Tr(
                                           $c->td(
                                               { -valign => 'top' },
                                               "Description",
                                               $c->br(),
                                               $c->small("Maximum 750 letters.")
                                           ),
                                           $c->td(
                                                 { -valign => 'top' },
                                                 $c->textarea(
                                                     -tabindex => "8",
                                                     -name     => "description",
                                                     -default => "$description",
                                                     -wrap    => "soft",
                                                     -rows    => '10',
                                                     -columns => '60'
                                                 )
                                           )
                                       )
                             )
                     )
             ),
  );
#5...........
        },

        'mangle1.def' => {
            source => "mangle1",
            params => "def",
            expect => <<'#6...........',
# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  " . my_wrap( "", "          ", $v ) : $v;
#6...........
        },

        'mangle1.mangle' => {
            source => "mangle1",
            params => "mangle",
            expect => <<'#7...........',
# The space after the '?' is essential and must not be deleted
print$::opt_m ? "  Files:  ".my_wrap("","          ",$v):$v;
#7...........
        },

        'mangle2.def' => {
            source => "mangle2",
            params => "def",
            expect => <<'#8...........',
# hanging side comments - do not remove leading space with -mangle
if ( $size1 == 0 || $size2 == 0 ) {    # special handling for zero-length
    if ( $size2 + $size1 == 0 ) {      # files.
        exit 0;
    }
    else {                             # Can't we say 'differ at byte zero'
                                       # and so on here?  That might make
                                       # more sense than this behavior.
                                       # Also, this should be made consistent
                                       # with the behavior when skip >=
                                       # filesize.
        if ($volume) {
            warn "$0: EOF on $file1\n" unless $size1;
            warn "$0: EOF on $file2\n" unless $size2;
        }
        exit 1;
    }
}

#8...........
        },

        'mangle2.mangle' => {
            source => "mangle2",
            params => "mangle",
            expect => <<'#9...........',
# hanging side comments - do not remove leading space with -mangle
if($size1==0||$size2==0){# special handling for zero-length
if($size2+$size1==0){# files.
exit 0;}else{# Can't we say 'differ at byte zero'
 # and so on here?  That might make
 # more sense than this behavior.
 # Also, this should be made consistent
 # with the behavior when skip >=
 # filesize.
if($volume){warn"$0: EOF on $file1\n" unless$size1;
warn"$0: EOF on $file2\n" unless$size2;}exit 1;}}
#9...........
        },

        'mangle3.def' => {
            source => "mangle3",
            params => "def",
            expect => <<'#10...........',
# run with --mangle
# Troublesome punctuation variables: $$ and $#

# don't delete ws between '$$' and 'if'
kill 'ABRT', $$ if $panic++;

# Do not remove the space between '$#' and 'eq'
$, = "Hello, World!\n";
$# = $,;
print "$# ";
$# eq $, ? print "yes\n" : print "no\n";

# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  " . my_wrap( "", "          ", $v ) : $v;

# must not remove space before 'CAKE'
use constant CAKE => atan2( 1, 1 ) / 2;
if ( $arc >= - CAKE && $arc <= CAKE ) {
}

# do not remove the space after 'JUNK':
print JUNK ( "<", "&", ">" )[ rand(3) ];    # make these a bit more likely
#10...........
        },

        'mangle3.mangle' => {
            source => "mangle3",
            params => "mangle",
            expect => <<'#11...........',
# run with --mangle
# Troublesome punctuation variables: $$ and $#
# don't delete ws between '$$' and 'if'
kill 'ABRT',$$ if$panic++;
# Do not remove the space between '$#' and 'eq'
$,="Hello, World!\n";
$#=$,;
print"$# ";
$# eq$,?print"yes\n":print"no\n";
# The space after the '?' is essential and must not be deleted
print$::opt_m ? "  Files:  ".my_wrap("","          ",$v):$v;
# must not remove space before 'CAKE'
use constant CAKE=>atan2(1,1)/2;
if($arc>=- CAKE&&$arc<=CAKE){}
# do not remove the space after 'JUNK':
print JUNK ("<","&",">")[rand(3)];# make these a bit more likely
#11...........
        },

        'math1.def' => {
            source => "math1",
            params => "def",
            expect => <<'#12...........',
my $xyz_shield = [
    [ -0.060,  -0.060,  0. ],
    [  0.060,  -0.060,  0. ],
    [  0.060,   0.060,  0. ],
    [ -0.060,   0.060,  0. ],
    [ -0.0925, -0.0925, 0.092 ],
    [  0.0925, -0.0925, 0.092 ],
    [  0.0925,  0.0925, 0.092 ],
    [ -0.0925,  0.0925, 0.092 ],
];
#12...........
        },

        'math2.def' => {
            source => "math2",
            params => "def",
            expect => <<'#13...........',
$ans = pdl(
    [ 0, 0, 0, 0, 0 ],
    [ 0, 0, 2, 0, 0 ],
    [ 0, 1, 5, 2, 0 ],
    [ 0, 0, 4, 0, 0 ],
    [ 0, 0, 0, 0, 0 ]
);
#13...........
        },

        'math3.def' => {
            source => "math3",
            params => "def",
            expect => <<'#14...........',
    my ( $x, $y ) = (
        $x0 +
          $index_x * $xgridwidth * $xm +
          ( $map_x * $xm * $xgridwidth ) / $detailwidth,
        $y0 -
          $index_y * $ygridwidth * $ym -
          ( $map_y * $ym * $ygridwidth ) / $detailheight,
    );
#14...........
        },

        'math4.def' => {
            source => "math4",
            params => "def",
            expect => <<'#15...........',
my $u      = ( $range * $pratio**( 1. / 3. ) ) / $wratio;
my $factor = exp( -( 18 / $u )**4 );
my $ovp    = ( 1 - $factor ) * ( 70 - 0.655515 * $u ) +
  ( 1000 / ( $u**1.3 ) + 10000 / ( $u**3.3 ) ) * $factor;
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) + ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
$ovp     = $ovp * $pratio;
$impulse = $impulse * $wratio * $pratio**( 2 / 3 );
#15...........
        },

        'nasc.def' => {
            source => "nasc",
            params => "def",
            expect => <<'#16...........',
        # will break and add semicolon unless -nasc is given
        eval {
            $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed };
        };
#16...........
        },

        'nasc.nasc' => {
            source => "nasc",
            params => "nasc",
            expect => <<'#17...........',
        # will break and add semicolon unless -nasc is given
        eval {
            $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed }
        };
#17...........
        },

        'nothing.def' => {
            source => "nothing",
            params => "def",
            expect => <<'#18...........',
#18...........
        },

        'nothing.nothing' => {
            source => "nothing",
            params => "nothing",
            expect => <<'#19...........',
#19...........
        },

        'otr1.def' => {
            source => "otr1",
            params => "def",
            expect => <<'#20...........',
return $pdl->slice(
    join ',',
    (
        map {
                $_ eq "X"         ? ":"
              : ref $_ eq "ARRAY" ? join ':', @$_
              : !ref $_           ? $_
              : die "INVALID SLICE DEF $_"
        } @_
    )
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
