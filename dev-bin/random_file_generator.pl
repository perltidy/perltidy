#!/usr/bin/perl -w
use strict;
use warnings;

# This is one of a set of programs for doing random testing of perltidy.  The
# goal is to try to crash perltidy.  The programs are:

#   random_file_generator.pl  [this file]
#   perltidy_random_setup.pl  [next step]
#   perltidy_random_run.pl    [final step]

# This program creates some random files for testing perltidy. You must supply
# as input args the names of a number of actual source files for it to read and
# manipulate into random files.

# Do not use too many source files
my $MAX_SOURCES = 10;

my $usage = <<EOM;
Create a set of random files for testing perltidy

    random_file_generator file1 [file2 [ ... Num

    file1 file2 ... are perl scripts (or other text files)
    Num is the number of random files to generate [default 100]

Example: generate 100 random files from the scripts in the upper directory:

    random_file_generator.pl ../*.pl 100

EOM

my @source_files  = @ARGV;
my $rsource_files = [];
my $max_cases     = 100;

if (@source_files) {
    $max_cases = pop @source_files;
    if ( $max_cases !~ /^\d+$/ ) {
        push @source_files, $max_cases;
        $max_cases = 100;
    }
}

if (@source_files) {

    # Only work on regular source_files with non-zero length
    @source_files = grep { -f $_ && !-z $_ } @source_files;

    # Sort from largest to smallest; that way we work with the largest files
    @source_files =
      map  { $_->[0] }
      sort { $b->[1] <=> $a->[1] }
      map  { [ $_, -e $_ ? -s $_ : 0 ] } @source_files;

    #if ( !@source_files ) { die "$usage" }

    my $i = -1;
    foreach my $file (@source_files) {
        $i++;
        last if ( $i >= $MAX_SOURCES );
        open( IN, '<', $file ) || die "cannot open $file: $!\n";
        $rsource_files->[$i] = [];
        foreach my $line (<IN>) {
            push @{ $rsource_files->[$i] }, $line;
        }
        close(IN);
    }
}

my $basename = "ranfile";
my $nsources = @{$rsource_files};

for ( my $nf = 1 ; $nf <= $max_cases ; $nf++ ) {
    my $fname = "$basename.$nf";
    my $frac  = rand(1);
    my $ix    = int( rand($nsources) );
    $ix = random_index( $nsources - 1 );
    my $NMETH  = 4;
    my $method = random_index(3);
    my $rfile;
    if ( $method == 3 || !$nsources ) {

        #my $nchars=1+random_index(1000);
        #$rfile = random_characters($nchars);
        my $nlines = 1 + random_index(100);
        $rfile = random_printable_lines( $nlines, 100 );

        #print STDERR "Method $method, nchars=$nchars\n";
        print STDERR "Method $method, nlines=$nlines\n";
    }
    elsif ( $method == 2 ) {
        $rfile = skip_random_lines( $rsource_files->[$ix], $frac );
        print STDERR "Method $method, frac=$frac, file=$ix\n";
    }
    elsif ( $method == 1 ) {
        $rfile = select_random_lines( $rsource_files->[$ix], $frac );
        print STDERR "Method $method, frac=$frac, file=$ix\n";
    }
    elsif ( $method == 0 ) {
        $rfile = reverse_random_lines( $rsource_files->[$ix], $frac );
        print STDERR "Method $method, frac=$frac, file=$ix\n";
    }

    # Shouldn't happen
    else {
        my $nchars = 1 + random_index(1000);
        $rfile = random_characters($nchars);
        print STDERR
"FIXME: method=$method but NMETH=$NMETH; Method $method, nchars=$nchars\n";
    }
    open( OUT, ">", $fname ) || die "cannot open $fname: $!\n";
    foreach my $line ( @{$rfile} ) {
        print OUT $line;
    }
    close OUT;
}

sub OLD_random_index {
    my ($ix_max) = @_;
    $ix_max = 0 if ( $ix_max < 0 );
    my $ix_min = 0;
    my $ix     = int( rand($ix_max) + 0.5 );
    $ix = $ix_max if ( $ix > $ix_max );
    $ix = $ix_min if ( $ix < $ix_min );
    return $ix;
}

sub random_printable_lines {
    my ( $nlines, $max_chars ) = @_;
    $max_chars = 100 unless ($max_chars);
    my @lines;
    for ( 1 .. $nlines ) {
        my $len = int( rand($max_chars) + 0.5 );
        my $str = random_printable_string($len);
        push @lines, $str . "\n";
    }
    return \@lines;
}

sub random_printable_string {
    my ($len) = @_;
    $len = 1 unless ($len);

    # This is the decimal range of printable characters in ASCII.  It is used to
    # make quick preliminary checks before resorting to using a regex.
    my $ord_printable_min = 33;
    my $ord_printable_max = 126;
    my $min               = 200;
    my $max               = 0;
    my $str               = "";
    foreach ( 1 .. $len ) {
        my $ord = random_index( $ord_printable_min, $ord_printable_max );
        my $ch  = chr($ord);

        #print "$ord $ch\n";
        $str .= $ch;
        if ( $ord > $max ) { $max = $ord }
        if ( $ord < $min ) { $min = $ord }
    }

    #print "min=$min max=$max [range is 33 - 126 ]\n";
    #print "$str\n";
    return ($str);
}

sub random_index {

    # 1 arg:  generate random index between 0 and arg1
    # 2 args: generate random index between arg1 and arg2
    my ( $ix_min, $ix_max ) = @_;

    if ( !defined($ix_max) ) {
        $ix_max = $ix_min;
        $ix_min = 0;
    }
    $ix_max = 0 if ( $ix_max < 0 );
    my $idiff = $ix_max - $ix_min;
    my $ix    = $ix_min + int( rand($idiff) + 0.5 );
    $ix = $ix_max if ( $ix > $ix_max );
    $ix = $ix_min if ( $ix < $ix_min );
    return $ix;
}

sub random_characters {

    my ($nchars) = @_;
    my @qset1 = qw# { [ ( } ] ) ; #;
    push @qset1, ',';
    my @qset2 = (
        qw{a b c f g m q r s t w x y z V W X 0 1 8 9},
        ';',  '[', ']', '{', '}',  '(',  ')', '=', '?', '|', '+', '<',
        '>',  '.', '!', '~', '^',  '*',  '$', '@', '&', ':', '%',
        '\\', '/', '_', ' ', "\n", "\t", '-',
        "'",  '"', '`', '#',
    );
    push @qset2, ',';
    my @qset3 = (
        '!%:',               '!%:',
        '!%:',               '!%:',
        '!*:',               '!@:',
        '%:',                '%:,',
        '%:;',               '*:',
        '*:,',               '*::',
        '*:;',               '+%:',
        '+*:',               '+@:',
        '-%:',               '-*:',
        '-@:',               ';%:',
        ';*:',               ';@:',
        '@:',                '@:,',
        '@::',               '@:;',
        '\%:',               '\&:',
        '\*:',               '\@:',
        '~%:',               '~*:',
        '~@:',               '(<',
        '(<',                '=<',
        'm(',                'm(',
        'm<',                'm[',
        'm{',                'q(',
        'q<',                'q[',
        'q{',                's(',
        's<',                's[',
        's{',                'y(',
        'y<',                'y[',
        'y{',                '$\'0',
        '009',               '0bB',
        '0xX',               '009;',
        '0bB;',              '0xX;',
        "<<'",               '<<"',
        '<<`',               '&::',
        '<<a',               '<<V',
        '<<s',               '<<y',
        '<<_',               'm;;_',
        'm[]_',              'm]]_',
        'm{}_',              'm}}_',
        'm--_',              's[]a',
        's[]b',              's[]0',
        's[];',              's[]]',
        's[]=',              's[].',
        's[]_',              's{}]',
        's{}?',              's<>s',
        's<>-',              '*::0',
        '*::1',              '*:::',
        '*::\'',             '$::0',
        '$:::',              '$::\'',
        '@::0',              '@::1',
        '@:::',              '&::0',
        '&::\'',             '%:::',
        '%::\'',             '$:::z',
        '*:::z',             "\\\@::'9:!",
        "} mz}~<<ts",        "<\@<<q-r8\n/",
        "W<<s`[\n(",         "X<<f+X;g(<~\" \n1\n*",
        "c<<t* 9\ns\n~^{s ", "<<V=-<<Wt",
        "[<<g/.<<r>\nV",     "( {8",
    );
    my @lines;
    my $ncpl = 0;
    my $line = "";

    for ( my $ich = 0 ; $ich < $nchars ; $ich++ ) {
        my $nset = random_index(2);
        my $ch;
        if ( $nset == 0 ) {
            my $ix = random_index( @qset1 - 1 );
            $ch = $qset1[$ix];
        }
        elsif ( $nset == 1 ) {
            my $ix = random_index( @qset2 - 1 );
            $ch = $qset2[$ix];
        }
        elsif ( $nset == 2 ) {
            my $ix = random_index( @qset3 - 1 );
            $ch = $qset3[$ix];
        }
        $line .= " $ch ";
        $ncpl++;
        if ( $ncpl > 20 ) {
            $line .= "\n";
            push @lines, $line;
            $ncpl = 0;
        }
    }
    $line .= "\n";
    push @lines, $line;
    return \@lines;
}

sub select_random_lines {

    # select some fraction of the lines in a source file
    # in any order
    my ( $rsource, $fkeep ) = @_;

    my $nlines   = @{$rsource};
    my $num_keep = $nlines * $fkeep;
    my $count    = 0;
    my @selected;
    while ( $count < $num_keep ) {
        my $ii = random_index( $nlines - 1 );
        push @selected, $rsource->[$ii];
        $count++;
    }
    return \@selected;
}

sub reverse_random_lines {

    # skip some fraction of the lines in a source file
    # and reverse the characters on a line
    my ( $rsource, $frand ) = @_;

    my %select;
    my $nlines     = @{$rsource};
    my $num_delete = $nlines * $frand;
    my $count      = 0;
    while ( $count < $num_delete ) {
        my $ii = rand($nlines);
        $ii = int($ii);
        $select{$ii} = 1;
        $count++;
    }

    my @lines;
    my $jj = -1;
    foreach my $line ( @{$rsource} ) {
        $jj++;
        if ( $select{$jj} ) {
            chomp $line;
            $line = reverse($line);
            $line .= "\n";
        }
        push @lines, $line;
    }
    return \@lines;
}

sub skip_random_lines {

    # skip some fraction of the lines in a source file
    # but keep lines in the original order
    my ( $rsource, $fskip ) = @_;

    my %skip;
    my $nlines     = @{$rsource};
    my $num_delete = $nlines * $fskip;
    my $count      = 0;
    while ( $count < $num_delete ) {
        my $ii = rand($nlines);
        $ii = int($ii);
        $skip{$ii} = 1;
        $count++;
    }

    my @selected;
    my $jj = -1;
    foreach my $line ( @{$rsource} ) {
        $jj++;
        next if $skip{$jj};
        push @selected, $line;
    }
    return \@selected;
}

__END__

my $ii = rand(@lines);
$ii = int($ii);
my %skip;
$skip{$ii} = 1;
print STDERR "Skipping line $ii\n";

my @select;
my $jj = -1;
foreach my $line (@lines) {
    $jj++;
    next if $skip{$jj};
    push @selected, $line;
}
foreach my $line (@selected) {
    print STDOUT $line;
}
