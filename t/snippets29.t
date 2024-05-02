# Created with: ./make_t.pl

# Contents:
#1 git125.git125
#2 vsn.def
#3 vsn.vsn1
#4 vsn.vsn2
#5 dia.def
#6 dia.dia1
#7 dia.dia2
#8 dia.dia3
#9 git134.def
#10 git135.def
#11 git135.git135
#12 c352.def
#13 c353.c353
#14 c353.def
#15 git137.def
#16 git137.git137
#17 git138.def
#18 git138.git138
#19 vsn.vsn3

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
        'c353' => <<'----------',
--valign-wide-equals
--valign-if-unless
----------
        'def'  => "",
        'dia1' => "-dia",
        'dia2' => "-aia",
        'dia3' => <<'----------',
-dia -aia -iac=2
-ias='][ }->{ ]->{ }->['
----------
        'git125' => "-ssp=0",
        'git135' => "--valign-wide-equals",
        'git137' => "-mci -nolq -ci=4",
        'git138' => <<'----------',
-xlp
-vt=2
----------
        'vsn1' => <<'----------',
-vsn
-gnu
----------
        'vsn2' => <<'----------',
# turn off vsn with -vsnl
-vsn
-vsnl=0
----------
        'vsn3' => <<'----------',
# turn off vsn
-nvsn
----------
    };

    ############################
    # BEGIN SECTION 2: Sources #
    ############################
    $rsources = {

        'c352' => <<'----------',
                # issue c352, recombine a small terminal quote
                $text .= filter_blocks( $code, line( substr( $source, 0, $pos[0] ), $line ) ) . ")";
                print( ( $Pipe->ResizeBuffer($NewSize) == $NewSize ) ? "Successful" : "Unsucessful" ) . "!\n\n";
                my $func = 'encode_utf8(' . ( !defined $str ? 'undef' : is_utf8($str) ? '$utf8_str' : '$ascii_str' ) . ')';
----------

        'c353' => <<'----------',
@ns        = split( /, ?/, join( ',', @ns ) );
@cnames    = split( /, ?/, join( ',', @cnames ) );
$recurse   = 1       unless ( defined $recurse );
$port      = 53      unless ( defined $port );
$srcport   = 0       unless ( defined $srcport );
$ttl       = 30 * 60 unless ( defined $ttl );
$hash      = 32      if ( defined $hash && $hash <= 0 );
$hash      = 63      if ( defined $hash && $hash > 63 );
$unique    = 1       unless ( defined $hash || defined $unique );
$unique  ||= $hash   if (1);

$basepath  = $CWD unless length($basepath);
$basepath .= '/'  if -d $basepath && $basepath !~ m#/$#;

$$hr  = $1 || $5 || $9 || 0;    # 9 is undef, but 5 is defined..
$$mr  = $2 || $6 || 0;
$$sr  = $3 || $7 || 0;
$ampm = $4 || $8 || $10;
$$tzr = $11;
$$hr += 12 if $ampm and "\U$ampm" eq "PM" && $$hr != 12;
$$hr = 0 if $$hr == 12 && "\U$ampm" eq "AM";
$$hr = 0 if $$hr == 24;

$map = $DEFAULT_MAP               unless defined $map;
$map = $DEFAULT_PATH . "/" . $map unless $map =~ m|/|;
$map .= $DEFAULT_EXT unless $map =~ m|/[^/]+\.[^/]+$|;
----------

        'dia' => <<'----------',
return $this->{'content'}[$row][$col];
return $this->{'content'}->[$row]->[$col];
return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};
return $self->{'commandline'}{'arg_list'}[0][0]{'hostgroups'};
$names->{'strings'}[ $featureEntry->{'settings'}{$setting} ][1][0]{0};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ]->[1]->[0]->{0};
$this->{'hline_color'}[ $last_drawn_row + 1 ][$col];
$this->{'hline_color'}->[ $last_drawn_row + 1 ]->[$col];
@{ $table{$file}{$subname}{$pack}{ $type . $name }->{$event} };
$tagslib->{ $fields[$x_i]->tag() }{ $subf[$i][0] }{tab};
$m2_results{ $modlog->{uid} }->{m2_count}{ $_->{uid} }++;
$self->_get_meta_data_hash_ref()->{ $p_object->get_key() }->[$p_offset];
my $v2  = [ 1, 2, [ 3, 4 ] ]->[2]->[0];
my $val1 = ${$x}[1];
my $val2 = $${$x}->[1];
----------

        'git125' => <<'----------',
sub Add ( $x, $y );
sub Sub( $x, $y );
----------

        'git134' => <<'----------',
# test padding at '[ $clientKey,' by sub pad_broken_list, see git #134
sub foo {
    my ( $self, $clientKey, $systemKey ) = @_;

    $q->do(
        q{
          Something
        },
        [   $clientKey,
            $systemKey,
        ],
    );

    return;
}
----------

        'git135' => <<'----------',
# simple alignments
$j /= 2;
$pow2 = $pow2 * $pow2;

@tmp = reverse split( /\//, $date );
$tmp[1] -= 1;
$tmp[2] -= 1900;
$epoch = timelocal( 0, 0, 0, @tmp );
$$dow_ref{$date} = $day = ( localtime $epoch )[6];

$state{rate} *= $rate_multiplier;
$state{catalyst} = $catalyst;
$state{_trail} .= ", catalysed ($file:$line)";

# trailing alignments
$self->{'_expect'} ||= $e || 'UNKNOWN';
$self->{'_s'}         = $s  || 'UNKNOWN';
$self->{'_word_size'} = $w  || 'UNKNOWN';
$self->{'_t1'}        = $t1 || 'UNKNOWN';

$v    = "'" . $v unless $v =~ /^'/;
$v   .= "'"      unless $v =~ /'$/;
$hex .= " $v";

$mask |= $catmask;
$mask |= $DeadBits{$word}               if $fatal;
$mask  = ~( ~$mask | $DeadBits{$word} ) if $no_fatal;

{{{
            # line limit exceeded if we align final '=' and 'if'
            my $row  = $list_count;
            $row    /= 2        if $main::config_parms{html_category_cols} == 2;
            $height = $row * 25 if $row * 25 < $height;
}}}

----------

        'git137' => <<'----------',
generate_error( msg =>
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);

subtype 'USState' => as Str => where {
    (        exists $STATES->{code2state}{ uc($_) }
          || exists $STATES->{state2code}{ uc($_) } );
};

$self->blurt( "Error: No INPUT definition for type '$type', typekind '"
      . $type->xstype
      . "' found" );
----------

        'git138' => <<'----------',
my $sth = $dbh->prepare( "select * from accountlines
  where (borrowernumber = ?) and (amountoutstanding<>0)
  order by date"
);
$VAR1 = [ 'method',
          1,
          'prepare',
          'SELECT table_name, table_owner, num_rows FROM iitables
                  where table_owner != \'$ingres\' and table_owner != \'DBA\''
];
----------

        'vsn' => <<'----------',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
----------
    };

    ####################################
    # BEGIN SECTION 3: Expected output #
    ####################################
    $rtests = {

        'git125.git125' => {
            source => "git125",
            params => "git125",
            expect => <<'#1...........',
sub Add( $x, $y );
sub Sub( $x, $y );
#1...........
        },

        'vsn.def' => {
            source => "vsn",
            params => "def",
            expect => <<'#2...........',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [  1,     2,    5,     6,      3,     1.5,  -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,     0 ],
    [  9,     8,    9,     8.4,    7.1,   7.5,   8,     3,    -3 ],
    [  0.1,   0.2,  0.5,   0.4,    0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine(  35,  0 );
$s->drawLine(  0,   10 );
$s->drawLine( -35,  0 );
$s->drawLine(  0,  -10 );
#2...........
        },

        'vsn.vsn1' => {
            source => "vsn",
            params => "vsn1",
            expect => <<'#3...........',
@data = (
         ["1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"],
         [ 1,     2,    5,     6,      3,     1.5,  -1,    -3,    -4],
         [-4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,     0],
         [ 9,     8,    9,     8.4,    7.1,   7.5,   8,     3,    -3],
         [ 0.1,   0.2,  0.5,   0.4,    0.3,   0.5,   0.1,   0,     0.4],
        );

$s->drawLine( 35,  0);
$s->drawLine( 0,   10);
$s->drawLine(-35,  0);
$s->drawLine( 0,  -10);
#3...........
        },

        'vsn.vsn2' => {
            source => "vsn",
            params => "vsn2",
            expect => <<'#4...........',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
#4...........
        },

        'dia.def' => {
            source => "dia",
            params => "def",
            expect => <<'#5...........',
return $this->{'content'}[$row][$col];
return $this->{'content'}->[$row]->[$col];
return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};
return $self->{'commandline'}{'arg_list'}[0][0]{'hostgroups'};
$names->{'strings'}[ $featureEntry->{'settings'}{$setting} ][1][0]{0};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ]->[1]->[0]->{0};
$this->{'hline_color'}[ $last_drawn_row + 1 ][$col];
$this->{'hline_color'}->[ $last_drawn_row + 1 ]->[$col];
@{ $table{$file}{$subname}{$pack}{ $type . $name }->{$event} };
$tagslib->{ $fields[$x_i]->tag() }{ $subf[$i][0] }{tab};
$m2_results{ $modlog->{uid} }->{m2_count}{ $_->{uid} }++;
$self->_get_meta_data_hash_ref()->{ $p_object->get_key() }->[$p_offset];
my $v2   = [ 1, 2, [ 3, 4 ] ]->[2]->[0];
my $val1 = ${$x}[1];
my $val2 = $${$x}->[1];
#5...........
        },

        'dia.dia1' => {
            source => "dia",
            params => "dia1",
            expect => <<'#6...........',
return $this->{'content'}[$row][$col];
return $this->{'content'}[$row][$col];
return $self->{'commandline'}{'arg_list'}[0][0]{'hostgroups'};
return $self->{'commandline'}{'arg_list'}[0][0]{'hostgroups'};
$names->{'strings'}[ $featureEntry->{'settings'}{$setting} ][1][0]{0};
$names->{'strings'}[ $featureEntry->{'settings'}{$setting} ]->[1][0]{0};
$this->{'hline_color'}[ $last_drawn_row + 1 ][$col];
$this->{'hline_color'}[ $last_drawn_row + 1 ][$col];
@{ $table{$file}{$subname}{$pack}{ $type . $name }{$event} };
$tagslib->{ $fields[$x_i]->tag() }{ $subf[$i][0] }{tab};
$m2_results{ $modlog->{uid} }->{m2_count}{ $_->{uid} }++;
$self->_get_meta_data_hash_ref()->{ $p_object->get_key() }->[$p_offset];
my $v2   = [ 1, 2, [ 3, 4 ] ]->[2][0];
my $val1 = ${$x}[1];
my $val2 = $${$x}->[1];
#6...........
        },

        'dia.dia2' => {
            source => "dia",
            params => "dia2",
            expect => <<'#7...........',
return $this->{'content'}->[$row]->[$col];
return $this->{'content'}->[$row]->[$col];
return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};
return $self->{'commandline'}->{'arg_list'}->[0]->[0]->{'hostgroups'};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ][1]->[0]->{0};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ]->[1]->[0]->{0};
$this->{'hline_color'}->[ $last_drawn_row + 1 ]->[$col];
$this->{'hline_color'}->[ $last_drawn_row + 1 ]->[$col];
@{ $table{$file}->{$subname}->{$pack}->{ $type . $name }->{$event} };
$tagslib->{ $fields[$x_i]->tag() }{ $subf[$i]->[0] }{tab};
$m2_results{ $modlog->{uid} }->{m2_count}->{ $_->{uid} }++;
$self->_get_meta_data_hash_ref()->{ $p_object->get_key() }->[$p_offset];
my $v2   = [ 1, 2, [ 3, 4 ] ]->[2]->[0];
my $val1 = ${$x}[1];
my $val2 = $${$x}->[1];
#7...........
        },

        'dia.dia3' => {
            source => "dia",
            params => "dia3",
            expect => <<'#8...........',
return $this->{'content'}->[$row][$col];
return $this->{'content'}->[$row][$col];
return $self->{'commandline'}->{'arg_list'}->[0][0]->{'hostgroups'};
return $self->{'commandline'}->{'arg_list'}->[0][0]->{'hostgroups'};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ][1][0]->{0};
$names->{'strings'}->[ $featureEntry->{'settings'}->{$setting} ][1][0]->{0};
$this->{'hline_color'}->[ $last_drawn_row + 1 ][$col];
$this->{'hline_color'}->[ $last_drawn_row + 1 ][$col];
@{ $table{$file}->{$subname}->{$pack}->{ $type . $name }->{$event} };
$tagslib->{ $fields[$x_i]->tag() }->{ $subf[$i][0] }->{tab};
$m2_results{ $modlog->{uid} }->{m2_count}->{ $_->{uid} }++;
$self->_get_meta_data_hash_ref()->{ $p_object->get_key() }->[$p_offset];
my $v2   = [ 1, 2, [ 3, 4 ] ]->[2][0];
my $val1 = ${$x}[1];
my $val2 = $${$x}->[1];
#8...........
        },

        'git134.def' => {
            source => "git134",
            params => "def",
            expect => <<'#9...........',
# test padding at '[ $clientKey,' by sub pad_broken_list, see git #134
sub foo {
    my ( $self, $clientKey, $systemKey ) = @_;

    $q->do(
        q{
          Something
        },
        [   $clientKey,
            $systemKey,
        ],
    );

    return;
}
#9...........
        },

        'git135.def' => {
            source => "git135",
            params => "def",
            expect => <<'#10...........',
# simple alignments
$j /= 2;
$pow2 = $pow2 * $pow2;

@tmp = reverse split( /\//, $date );
$tmp[1] -= 1;
$tmp[2] -= 1900;
$epoch = timelocal( 0, 0, 0, @tmp );
$$dow_ref{$date} = $day = ( localtime $epoch )[6];

$state{rate} *= $rate_multiplier;
$state{catalyst} = $catalyst;
$state{_trail} .= ", catalysed ($file:$line)";

# trailing alignments
$self->{'_expect'} ||= $e || 'UNKNOWN';
$self->{'_s'}         = $s  || 'UNKNOWN';
$self->{'_word_size'} = $w  || 'UNKNOWN';
$self->{'_t1'}        = $t1 || 'UNKNOWN';

$v = "'" . $v unless $v =~ /^'/;
$v .= "'" unless $v =~ /'$/;
$hex .= " $v";

$mask |= $catmask;
$mask |= $DeadBits{$word} if $fatal;
$mask = ~( ~$mask | $DeadBits{$word} ) if $no_fatal;

{
    {
        {
            # line limit exceeded if we align final '=' and 'if'
            my $row = $list_count;
            $row /= 2           if $main::config_parms{html_category_cols} == 2;
            $height = $row * 25 if $row * 25 < $height;
        }
    }
}

#10...........
        },

        'git135.git135' => {
            source => "git135",
            params => "git135",
            expect => <<'#11...........',
# simple alignments
$j    /= 2;
$pow2  = $pow2 * $pow2;

@tmp              = reverse split( /\//, $date );
$tmp[1]          -= 1;
$tmp[2]          -= 1900;
$epoch            = timelocal( 0, 0, 0, @tmp );
$$dow_ref{$date}  = $day = ( localtime $epoch )[6];

$state{rate}     *= $rate_multiplier;
$state{catalyst}  = $catalyst;
$state{_trail}   .= ", catalysed ($file:$line)";

# trailing alignments
$self->{'_expect'}    ||= $e  || 'UNKNOWN';
$self->{'_s'}           = $s  || 'UNKNOWN';
$self->{'_word_size'}   = $w  || 'UNKNOWN';
$self->{'_t1'}          = $t1 || 'UNKNOWN';

$v    = "'" . $v unless $v =~ /^'/;
$v   .= "'"      unless $v =~ /'$/;
$hex .= " $v";

$mask |= $catmask;
$mask |= $DeadBits{$word}               if $fatal;
$mask  = ~( ~$mask | $DeadBits{$word} ) if $no_fatal;

{
    {
        {
            # line limit exceeded if we align final '=' and 'if'
            my $row  = $list_count;
            $row    /= 2        if $main::config_parms{html_category_cols} == 2;
            $height = $row * 25 if $row * 25 < $height;
        }
    }
}

#11...........
        },

        'c352.def' => {
            source => "c352",
            params => "def",
            expect => <<'#12...........',
                # issue c352, recombine a small terminal quote
                $text .= filter_blocks( $code,
                    line( substr( $source, 0, $pos[0] ), $line ) ) . ")";
                print( ( $Pipe->ResizeBuffer($NewSize) == $NewSize )
                    ? "Successful"
                    : "Unsucessful" ) . "!\n\n";
                my $func =
                  'encode_utf8('
                  . ( !defined $str ? 'undef'
                    : is_utf8($str) ? '$utf8_str'
                    :                 '$ascii_str' ) . ')';
#12...........
        },

        'c353.c353' => {
            source => "c353",
            params => "c353",
            expect => <<'#13...........',
@ns        = split( /, ?/, join( ',', @ns ) );
@cnames    = split( /, ?/, join( ',', @cnames ) );
$recurse   = 1       unless ( defined $recurse );
$port      = 53      unless ( defined $port );
$srcport   = 0       unless ( defined $srcport );
$ttl       = 30 * 60 unless ( defined $ttl );
$hash      = 32      if ( defined $hash && $hash <= 0 );
$hash      = 63      if ( defined $hash && $hash > 63 );
$unique    = 1       unless ( defined $hash || defined $unique );
$unique  ||= $hash   if (1);

$basepath  = $CWD unless length($basepath);
$basepath .= '/'  if -d $basepath && $basepath !~ m#/$#;

$$hr   = $1 || $5 || $9 || 0;    # 9 is undef, but 5 is defined..
$$mr   = $2 || $6 || 0;
$$sr   = $3 || $7 || 0;
$ampm  = $4 || $8 || $10;
$$tzr  = $11;
$$hr  += 12 if $ampm and "\U$ampm" eq "PM" && $$hr != 12;
$$hr   = 0  if $$hr == 12                  && "\U$ampm" eq "AM";
$$hr   = 0  if $$hr == 24;

$map  = $DEFAULT_MAP               unless defined $map;
$map  = $DEFAULT_PATH . "/" . $map unless $map =~ m|/|;
$map .= $DEFAULT_EXT               unless $map =~ m|/[^/]+\.[^/]+$|;
#13...........
        },

        'c353.def' => {
            source => "c353",
            params => "def",
            expect => <<'#14...........',
@ns      = split( /, ?/, join( ',', @ns ) );
@cnames  = split( /, ?/, join( ',', @cnames ) );
$recurse = 1       unless ( defined $recurse );
$port    = 53      unless ( defined $port );
$srcport = 0       unless ( defined $srcport );
$ttl     = 30 * 60 unless ( defined $ttl );
$hash    = 32 if ( defined $hash && $hash <= 0 );
$hash    = 63 if ( defined $hash && $hash > 63 );
$unique  = 1 unless ( defined $hash || defined $unique );
$unique ||= $hash if (1);

$basepath = $CWD unless length($basepath);
$basepath .= '/' if -d $basepath && $basepath !~ m#/$#;

$$hr  = $1 || $5 || $9 || 0;    # 9 is undef, but 5 is defined..
$$mr  = $2 || $6 || 0;
$$sr  = $3 || $7 || 0;
$ampm = $4 || $8 || $10;
$$tzr = $11;
$$hr += 12 if $ampm and "\U$ampm" eq "PM" && $$hr != 12;
$$hr = 0 if $$hr == 12 && "\U$ampm" eq "AM";
$$hr = 0 if $$hr == 24;

$map = $DEFAULT_MAP               unless defined $map;
$map = $DEFAULT_PATH . "/" . $map unless $map =~ m|/|;
$map .= $DEFAULT_EXT unless $map =~ m|/[^/]+\.[^/]+$|;
#14...........
        },

        'git137.def' => {
            source => "git137",
            params => "def",
            expect => <<'#15...........',
generate_error( msg =>
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);

subtype 'USState' => as Str => where {
    (        exists $STATES->{code2state}{ uc($_) }
          || exists $STATES->{state2code}{ uc($_) } );
};

$self->blurt( "Error: No INPUT definition for type '$type', typekind '"
      . $type->xstype
      . "' found" );
#15...........
        },

        'git137.git137' => {
            source => "git137",
            params => "git137",
            expect => <<'#16...........',
generate_error( msg =>
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
);

subtype 'USState' => as Str => where {
    (      exists $STATES->{code2state}{ uc($_) }
        || exists $STATES->{state2code}{ uc($_) } );
};

$self->blurt( "Error: No INPUT definition for type '$type', typekind '"
    . $type->xstype
    . "' found" );
#16...........
        },

        'git138.def' => {
            source => "git138",
            params => "def",
            expect => <<'#17...........',
my $sth = $dbh->prepare(
    "select * from accountlines
  where (borrowernumber = ?) and (amountoutstanding<>0)
  order by date"
);
$VAR1 = [
    'method',
    1,
    'prepare',
    'SELECT table_name, table_owner, num_rows FROM iitables
                  where table_owner != \'$ingres\' and table_owner != \'DBA\''
];
#17...........
        },

        'git138.git138' => {
            source => "git138",
            params => "git138",
            expect => <<'#18...........',
my $sth = $dbh->prepare( "select * from accountlines
  where (borrowernumber = ?) and (amountoutstanding<>0)
  order by date"
);
$VAR1 = [ 'method',
          1,
          'prepare',
          'SELECT table_name, table_owner, num_rows FROM iitables
                  where table_owner != \'$ingres\' and table_owner != \'DBA\''
];
#18...........
        },

        'vsn.vsn3' => {
            source => "vsn",
            params => "vsn3",
            expect => <<'#19...........',
@data = (
    [ "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th" ],
    [ 1,     2,     5,     6,     3,     1.5,   -1,    -3,    -4 ],
    [ -4,    -3,    1,     1,     -3,    -1.5,  -2,    -1,    0 ],
    [ 9,     8,     9,     8.4,   7.1,   7.5,   8,     3,     -3 ],
    [ 0.1,   0.2,   0.5,   0.4,   0.3,   0.5,   0.1,   0,     0.4 ],
);

$s->drawLine( 35,  0 );
$s->drawLine( 0,   10 );
$s->drawLine( -35, 0 );
$s->drawLine( 0,   -10 );
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
