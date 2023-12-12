#!/usr/bin/perl -w
use strict;
use warnings;
use Getopt::Long;

#---------------------------------------------------------
# Try to find the git home of perltidy on this computer.
# Assuming that this program is git/dev_bin/get_perltidy.pl,
# or a symlink to that, then we can get its path:
use Cwd qw(abs_path);
my $absolute_path = abs_path( readlink $0 );
print "path is $absolute_path\n";
use File::Basename;
my ( $name, $path, $suffix ) = fileparse($absolute_path);
my $PERLTIDY_GIT = $path . "../";

# If this doesn't work, then replace the above with a specific
# definition here:
# my $PERLTIDY_GIT = '/home/.../git

#---------------------------------------------------------

my $pm2pl = $PERLTIDY_GIT . '/pm2pl';
if ( !-e $pm2pl ) {
    die <<EOM;
Error: did not find 'pm2pl' with the perltidy git directory set as:
'$PERLTIDY_GIT'
Please update this directory in file $0
EOM
}

my $usage = <<EOM;

This script will use pm2pl to get a copy of the latest perltidy.pl
after first making a backup of any existing perltidy.pl
usage:

  perl get_perltidy.pl [-h -b -D

  -h prints this help
  -b=backup option:
    =0 never backup
    =1 always backup [DEFAULT]
    =2 backup if there is a change
  -D=0 for DEVEL_MODE => 0, 
  -D=1 for DEVEL_MODE => 1 [DEFAULT]
EOM

my @option_string = qw(
  h
  b:i
  D:i
);

my %Opts = ();
if ( !GetOptions( \%Opts, @option_string ) ) {
    print STDERR "Exiting due to error in options\n";
    exit 1;
}

if ( $Opts{h} ) {
    print $usage;
    exit 1;
}

my $Dflag = '-D';
if ( defined( $Opts{D} ) && $Opts{D} eq '0' ) {
    $Dflag = "";
    print "turning off -D\n";
}

my $backup_option = 1;
if ( defined( $Opts{b} ) ) {
    $backup_option = $Opts{b};
    if ( $backup_option < 0 || $backup_option > 2 ) {
        print STDERR "unexpected -b: expecting 0,1,2 but got $backup_option\n";
        exit 1;
    }
}

if ($backup_option) {
    my $basename = 'perltidy.pl';

    # make a backup
    my $bname;
    if ( -e $basename ) {
        my $ext;
        for ( my $j = 1 ; $j <= 999 ; $j++ ) {
            $ext   = 'ba' . $j;
            $bname = "$basename.$ext";
            next if ( -e $bname || -e $bname . ".gz" );
            system "mv $basename $bname";
            last;
        }
        if ( !$bname ) {
            die "too many backup versions of $basename - move some\n";
        }
    }

    # get the latest version of perltidy with DEVEL_MODE => 1
    my $msg = get_latest_perltidy($Dflag);

    if ( $backup_option == 2 ) {
        use File::Compare;
        my $diff = compare( $bname, $basename );
        if ( !$diff ) {
            print "perltidy.pl unchanged, no backup saved\n";
            system "mv $bname $basename";
            $bname = "";
        }
    }

    if ($bname) {
        print "Moved $basename -> $bname\n";
        print $msg;
    }
}

sub get_latest_perltidy {
    my ($opt_D) = @_;
    use Cwd qw(getcwd);
    my $starting_dir = getcwd();
    my $dir          = $PERLTIDY_GIT;
    use File::Temp qw/ tempfile tempdir /;
    my ( $fh_tmp, $tmpfile ) = tempfile();
    chdir $dir;
    my $ofile = $starting_dir . '/perltidy.pl';
    system "./pm2pl $opt_D -o $ofile >$tmpfile";
    chdir $starting_dir;
    open my $fh, '<', $tmpfile or die "Can't open $tmpfile: $!";
    unlink $tmpfile;
    my $msg = do { local $/; <$fh> };
    return $msg;
}
