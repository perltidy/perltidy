# **This script was automatically generated**
# Created with: ./make_t.pl
# Thu Apr  5 07:31:23 2018

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
        'def'     => "",
        'lp'      => "-lp",
        'mangle'  => "--mangle",
        'nasc'    => "-nasc",
        'nothing' => "",
        'otr'     => <<'----------',
-ohbr
-opr
-osbr
----------
    };

    ######################
    # SECTION 2: Sources #
    ######################
    $rsources = {

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

    ##############################
    # SECTION 3: Expected output #
    ##############################
    $rtests = {

        'listop1.def' => {
            source => "listop1",
            params => "def",
            expect => <<'#1...........',
my @sorted = map { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map { [ $_, rand ] } @list;
#1...........
        },

        'listop2.def' => {
            source => "listop2",
            params => "def",
            expect => <<'#2...........',
my @sorted =
  map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, rand ] } @list;
#2...........
        },

        'lp1.def' => {
            source => "lp1",
            params => "def",
            expect => <<'#3...........',
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
#3...........
        },

        'lp1.lp' => {
            source => "lp1",
            params => "lp",
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
                                      " Relevant Discipline ", $c->br(), "Area "
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
                                               "Description", $c->br(),
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
#4...........
        },

        'mangle1.def' => {
            source => "mangle1",
            params => "def",
            expect => <<'#5...........',
# The space after the '?' is essential and must not be deleted
print $::opt_m ? "  Files:  " . my_wrap( "", "          ", $v ) : $v;
#5...........
        },

        'mangle1.mangle' => {
            source => "mangle1",
            params => "mangle",
            expect => <<'#6...........',
# The space after the '?' is essential and must not be deleted
print$::opt_m ? "  Files:  ".my_wrap("","          ",$v):$v;
#6...........
        },

        'mangle2.def' => {
            source => "mangle2",
            params => "def",
            expect => <<'#7...........',
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

#7...........
        },

        'mangle2.mangle' => {
            source => "mangle2",
            params => "mangle",
            expect => <<'#8...........',
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
#8...........
        },

        'mangle3.def' => {
            source => "mangle3",
            params => "def",
            expect => <<'#9...........',
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
#9...........
        },

        'mangle3.mangle' => {
            source => "mangle3",
            params => "mangle",
            expect => <<'#10...........',
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
#10...........
        },

        'math1.def' => {
            source => "math1",
            params => "def",
            expect => <<'#11...........',
my $xyz_shield = [
    [ -0.060,  -0.060,  0. ],
    [ 0.060,   -0.060,  0. ],
    [ 0.060,   0.060,   0. ],
    [ -0.060,  0.060,   0. ],
    [ -0.0925, -0.0925, 0.092 ],
    [ 0.0925,  -0.0925, 0.092 ],
    [ 0.0925,  0.0925,  0.092 ],
    [ -0.0925, 0.0925,  0.092 ],
];
#11...........
        },

        'math2.def' => {
            source => "math2",
            params => "def",
            expect => <<'#12...........',
$ans = pdl(
    [ 0, 0, 0, 0, 0 ],
    [ 0, 0, 2, 0, 0 ],
    [ 0, 1, 5, 2, 0 ],
    [ 0, 0, 4, 0, 0 ],
    [ 0, 0, 0, 0, 0 ]
);
#12...........
        },

        'math3.def' => {
            source => "math3",
            params => "def",
            expect => <<'#13...........',
    my ( $x, $y ) = (
        $x0 +
          $index_x * $xgridwidth * $xm +
          ( $map_x * $xm * $xgridwidth ) / $detailwidth,
        $y0 -
          $index_y * $ygridwidth * $ym -
          ( $map_y * $ym * $ygridwidth ) / $detailheight,
    );
#13...........
        },

        'math4.def' => {
            source => "math4",
            params => "def",
            expect => <<'#14...........',
my $u      = ( $range * $pratio**( 1. / 3. ) ) / $wratio;
my $factor = exp( -( 18 / $u )**4 );
my $ovp =
  ( 1 - $factor ) * ( 70 - 0.655515 * $u ) +
  ( 1000 / ( $u**1.3 ) + 10000 / ( $u**3.3 ) ) * $factor;
my $impulse =
  ( 1 - $factor ) * ( 170 - $u ) + ( 350 / $u**0.65 + 500 / $u**5 ) * $factor;
$ovp     = $ovp * $pratio;
$impulse = $impulse * $wratio * $pratio**( 2 / 3 );
#14...........
        },

        'nasc.def' => {
            source => "nasc",
            params => "def",
            expect => <<'#15...........',
        # will break and add semicolon unless -nasc is given
        eval {
            $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed };
        };
#15...........
        },

        'nasc.nasc' => {
            source => "nasc",
            params => "nasc",
            expect => <<'#16...........',
        # will break and add semicolon unless -nasc is given
        eval {
            $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed }
        };
#16...........
        },

        'nothing.def' => {
            source => "nothing",
            params => "def",
            expect => <<'#17...........',
#17...........
        },

        'nothing.nothing' => {
            source => "nothing",
            params => "nothing",
            expect => <<'#18...........',
#18...........
        },

        'otr1.def' => {
            source => "otr1",
            params => "def",
            expect => <<'#19...........',
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
#19...........
        },

        'otr1.otr' => {
            source => "otr1",
            params => "otr",
            expect => <<'#20...........',
return $pdl->slice(
    join ',', (
        map {
                $_ eq "X" ? ":"
              : ref $_ eq "ARRAY" ? join ':', @$_
              : !ref $_ ? $_
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
