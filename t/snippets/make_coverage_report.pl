#!/usr/bin/perl -w
use strict;
use warnings;
use Perl::Tidy;
use Data::Dumper;

#--------------------------------------------------------------------------
# NOTE: While this gives useful information, I have concluded that due to
# the large number of parameters and their possible interactions, automated
# random testing is a better way to be sure perltidy parameters are tested.
# So this program is no longer used.
#--------------------------------------------------------------------------

# This will eventually read all of the '.par' files and write a report
# showing the parameter coverage.

# The starting point for this program is 'examples/perltidyrc_dump.pl'

# The plan is:
# read each '.par' file
# use perltidy's options-dump feature to convert to long names and return in a hash
# combine all of these results and write back to standard output in sorted order
#
# It will also be useful to output a list of unused parameters

my $usage = <<EOM;
# writes a summary of parameters covered in snippet testing
# no_coverage.txt has list of parameters not covered
# coverage.txt has list of parameters with some coverage
#
# usage:
#
# make_coverage_report.pl filename [filename [...
#  filename is the name of a .perltidyrc config file 

#  if no filenames are given, glob all *.par files
#  -h help
EOM
use Getopt::Std;
my %my_opts;
my $cmdline_uu = $0 . " " . join " ", @ARGV;
getopts( 'hdsq', \%my_opts ) or die "$usage";
if ( $my_opts{h} ) { die "$usage" }

my @files = @ARGV;
if ( !@files ) { @files = glob('*.par') }

# Get a list of all options, their sections and abbreviations
# Also get the list of defaults
my (
    $error_message_g,   $rGetopt_flags, $rsections_uu,
    $rabbreviations_uu, $rOpts_default
) = get_perltidy_options();

if ($error_message_g) {
    die "$error_message_g\n";
}

# Keep a list of the values of parameters that we see
# $rsaw_values->{name}->[list of values seen]
my $rsaw_values = {};

## Start by storing the default values
#foreach my $long_name ( keys %{$rOpts_default} ) {
#    my $val = $rOpts_default->{$long_name};
#    $rsaw_values->{$long_name} = [$val];
#}

# Initialize to defaults
foreach my $long_name ( keys %{$rGetopt_flags} ) {
    if ( defined( $rOpts_default->{$long_name} ) ) {
        my $val = $rOpts_default->{$long_name};
        $rsaw_values->{$long_name} = [$val];
    }
    else {

        # Store a 0 default for all switches with no default value
        my $flag = $rGetopt_flags->{$long_name};
        if ( $flag eq '!' ) {
            my $val = 0;
            $rsaw_values->{$long_name} = [$val];
        }
    }
}

# Loop over config files
foreach my $config_file (@files) {

    # get its options
    my ( $error_message, $rOpts ) = read_perltidyrc($config_file);

    if ($error_message) {
        die "$error_message\n";
    }

    # save these values, we will sort them out below
    foreach my $long_name ( keys %{$rOpts} ) {
        my $val = $rOpts->{$long_name};
        push @{ $rsaw_values->{$long_name} }, $val;
    }
}

# sort the values seen and remove duplicates
my @not_seen;
my @seen;
foreach my $long_name ( keys %{$rGetopt_flags} ) {
    if ( $rsaw_values->{$long_name} ) {
        my @vals   = @{ $rsaw_values->{$long_name} };
        my @uniq   = uniq(@vals);
        my @sorted = sort { $a cmp $b } @uniq;
        $rsaw_values->{$long_name} = \@sorted;
        my $options_flag = $rGetopt_flags->{$long_name};

        # Old: Consider switches with just one value as not seen
        # New: No longer doing this - the default provides the other value
        if ( 0 && $options_flag eq '!' && @sorted < 2 ) {
            push @not_seen, $long_name;
        }
        else {
            push @seen, $long_name;
        }
    }
    else {
        push @not_seen, $long_name;
    }
}

# Remove the unseen from the big hash
foreach my $long_name (@not_seen) {
    delete $rsaw_values->{$long_name};
}

# write list of parameters not seen
my $fh;
my $fnot_seen = "coverage_missing.txt";
@not_seen = sort { $a cmp $b } @not_seen;
open( $fh, ">", $fnot_seen ) || die "cannot open $fnot_seen: $!\n";
$fh->print("# No coverage in test snippets for these parameters\n");
foreach my $long_name (@not_seen) {
    $fh->print("$long_name\n");
}
$fh->close();
print {*STDOUT} "wrote $fnot_seen\n";

# Dump complete summary
#print Data::Dumper->Dump($rsaw_values);
my $fseen = "coverage_values.txt";
open( $fh, ">", $fseen ) || die "can open $fseen: $!\n";
$fh->print( Dumper($rsaw_values) );
$fh->close();
print {*STDOUT} "wrote $fseen\n";

sub uniq { my %saw; grep !$saw{$_}++, @_ }

sub get_perltidy_options {

    my $error_message = "";

    my $stderr = "";    # try to capture error messages
    my $argv   = "";    # do not let perltidy see our @ARGV

    # call with an empty .perltidyrc to get the default parameters
    my $empty_file = "";    # this will be our .perltidyrc file
    my %Opts_default;       # this will receive the default options hash
    my %sections;
    my %abbreviations;
    my %Getopt_flags;
    my $err = Perl::Tidy::perltidy(
        perltidyrc            => \$empty_file,
        stderr                => \$stderr,
        argv                  => \$argv,
        dump_options          => \%Opts_default,
        dump_options_type     => 'full',            # 'full' gives everything
        dump_getopt_flags     => \%Getopt_flags,
        dump_options_category => \%sections,
        dump_abbreviations    => \%abbreviations,
    );
    if ($err) {
        die "Error calling perltidy\n";
    }

    # try to capture any errors generated by perltidy call
    # but for severe errors it will typically croak
    $error_message .= $stderr;

    # debug: show how everything is stored by printing it out
    my $DEBUG = 0;
    if ($DEBUG) {
        print {*STDOUT} "---Getopt Parameters---\n";
        foreach my $key ( sort keys %Getopt_flags ) {
            print {*STDOUT} "$key$Getopt_flags{$key}\n";
        }
        print {*STDOUT} "---Manual Sections---\n";
        foreach my $key ( sort keys %sections ) {
            print {*STDOUT} "$key -> $sections{$key}\n";
        }
        print {*STDOUT} "---Abbreviations---\n";
        foreach my $key ( sort keys %abbreviations ) {
            my @names = @{ $abbreviations{$key} };
            print {*STDOUT} "$key -> {@names}\n";
        }
    }

    return (
        $error_message,  \%Getopt_flags, \%sections,
        \%abbreviations, \%Opts_default,
    );
} ## end sub get_perltidy_options

sub read_perltidyrc {

    # input parameter -
    #   $config_file is the name of a .perltidyrc file we want to read
    #   or a reference to a string or array containing the .perltidyrc file
    #   if not defined, Perl::Tidy will try to find the user's .perltidyrc
    # output parameters -
    #   $error_message will be blank unless an error occurs
    #   $rOpts - reference to the hash of options in the .perlticyrc
    # NOTE:
    #   Perl::Tidy will croak or die on certain severe errors

    my ($config_file) = @_;
    my $error_message = "";
    my %Opts;    # any options found will be put here

    my $stderr = "";    # try to capture error messages
    my $argv   = "";    # do not let perltidy see our @ARGV

    my %abbreviations;
    Perl::Tidy::perltidy(
        perltidyrc         => $config_file,
        dump_options       => \%Opts,
        dump_options_type  => 'perltidyrc',      # default is 'perltidyrc'
        dump_abbreviations => \%abbreviations,
        stderr             => \$stderr,
        argv               => \$argv,
    );

    # try to capture any errors generated by perltidy call
    # but for severe errors it will typically croak
    $error_message .= $stderr;

    # debug: show how everything is stored by printing it out
    my $DEBUG = 0;
    if ($DEBUG) {
        print {*STDOUT} "---Opts---\n";
        foreach my $key ( sort keys %Opts ) {
            print {*STDOUT} "$key -> $Opts{$key}\n";
        }
    }
    return ( $error_message, \%Opts );
} ## end sub read_perltidyrc

__END__

# Notes for future: print missing coverage by section

# Optional: minimize the perltidyrc file length by deleting long_names
# in $rOpts which are also in $rOpts_default and have the same value.
# This would be useful if a perltidyrc file has been constructed from a
# full parameter dump, for example.
if ( $my_opts{d} ) {
    foreach my $long_name ( keys %{$rOpts} ) {
        delete $rOpts->{$long_name} if $equals_default{$long_name};
    }
}

# find user-defined abbreviations
my %abbreviations_user;
foreach my $key ( keys %$rabbreviations ) {
    unless ( $rabbreviations_default->{$key} ) {
        $abbreviations_user{$key} = $rabbreviations->{$key};
    }
}

# dump the options, if any
if ( %$rOpts || %abbreviations_user ) {
    dump_options( $cmdline, \%my_opts, $rOpts, $rGetopt_flags, $rsections,
        $rabbreviations, \%equals_default, \%abbreviations_user );
}
else {
    if ($config_file) {
        print {*STDERR} <<EOM;
No configuration parameters seen in file: $config_file
EOM
    }
    else {
        print {*STDERR} <<EOM;
No .perltidyrc file found, use perltidy -dpro to see locations checked.
EOM
    }
}

sub dump_options {

    # write the options back out as a valid .perltidyrc file
    # This version writes long names by sections
    my ( $cmdline, $rmy_opts, $rOpts, $rGetopt_flags, $rsections,
        $rabbreviations, $requals_default, $rabbreviations_user )
      = @_;

    # $rOpts is a reference to the hash returned by Getopt::Long
    # $rGetopt_flags are the flags passed to Getopt::Long
    # $rsections is a hash giving manual section {long_name}

    # build a hash giving section->long_name->parameter_value
    # so that we can write parameters by section
    my %section_and_name;
    my $rsection_name_value = \%section_and_name;
    my %saw_section;
    foreach my $long_name ( keys %{$rOpts} ) {
        my $section = $rsections->{$long_name};
        $section = "UNKNOWN" unless ($section);    # shouldn't happen

        # build a hash giving section->long_name->parameter_value
        $rsection_name_value->{$section}->{$long_name} = $rOpts->{$long_name};

        # remember what sections are in this hash
        $saw_section{$section}++;
    }

    # build a table for long_name->short_name abbreviations
    my %short_name;
    foreach my $abbrev ( keys %{$rabbreviations} ) {
        ##foreach my $abbrev ( sort keys %$rabbreviations ) {
            my @list = @{ $$rabbreviations{$abbrev} };

            # an abbreviation may expand into one or more other words,
            # but only those that expand to a single word (which must be
            # one of the long names) are the short names that we want
            # here.
            next unless @list == 1;
            my $long_name = $list[0];
            $short_name{$long_name} = $abbrev;
        ##}
    }

    unless ( $rmy_opts->{q} ) {
        my $date = localtime();
        print {*STDOUT} "# perltidy configuration file created $date\n";
        print {*STDOUT} "# using: $cmdline\n";
    }

    # loop to write section-by-section
    foreach my $section ( sort keys %saw_section ) {
        unless ( $rmy_opts->{q} ) {
            print {*STDOUT} "\n";

            # remove leading section number, which is there
            # for sorting, i.e.,
            # 1. Basic formatting options -> Basic formatting options
            my $trimmed_section = $section;
            $trimmed_section =~ s/^\d+\. //;
            print {*STDOUT} "# $trimmed_section\n";
        }

        # loop over all long names for this section
        my $rname_value = $rsection_name_value->{$section};
        foreach my $long_name ( sort keys %{$rname_value} ) {

            # pull out getopt flag and actual parameter value
            my $flag  = $rGetopt_flags->{$long_name};
            my $value = $rname_value->{$long_name};

            # turn this it back into a parameter
            my $prefix       = '--';
            my $short_prefix = '-';
            my $suffix       = "";
            if ($flag) {
                if ( $flag =~ /^=/ ) {
                    if ( $value !~ /^\d+$/ ) { $value = '"' . $value . '"' }
                    $suffix = "=" . $value;
                }
                elsif ( $flag =~ /^!/ ) {
                    $prefix       .= "no" unless ($value);
                    $short_prefix .= "n"  unless ($value);
                }
                elsif ( $flag =~ /^:/ ) {
                    if ( $value !~ /^\d+$/ ) { $value = '"' . $value . '"' }
                    $suffix = "=" . $value;
                }
                else {

                    # shouldn't happen
                    print {*STDOUT}
"# ERROR in dump_options: unrecognized flag $flag for $long_name\n";
                }
            }

##    # These long option names have no abbreviations or are treated specially
##    @option_string = qw(
##      html!
##      noprofile
##      no-profile
##      npro
##      recombine!
##      valign!
##      notidy
##    );

            # print the long version of the parameter
            # with the short version as a side comment
            my $short_name  = $short_name{$long_name};
            my $long_option = $prefix . $long_name . $suffix;

            # A few options do not have a short abbreviation
            # so we will make it the same as the long option
            # These include 'recombine' and 'valign', which are mainly
            # for debugging.
            my $short_option = $long_option;
            if ($short_name) {
                $short_option = $short_prefix . $short_name . $suffix;
            }

            my $note = $requals_default->{$long_name} ? "  [=default]" : "";
            if ( $rmy_opts->{s} ) {
                print {*STDOUT} $short_option. "\n";
            }
            else {
                my $side_comment = "";
                unless ( $rmy_opts->{q} ) {
                    my $spaces = 40 - length($long_option);
                    $spaces = 2 if ( $spaces < 2 );
                    $side_comment =
                      ' ' x $spaces . '# ' . $short_option . $note;
                }
                print {*STDOUT} $long_option . $side_comment . "\n";
            }
        }
    }

    if ( %{$rabbreviations_user} ) {
        unless ( $rmy_opts->{q} ) {
            print {*STDOUT} "\n";
            print {*STDOUT} "# Abbreviations\n";
        }
        foreach my $key ( keys %$rabbreviations_user ) {
            my @vals = @{ $rabbreviations_user->{$key} };
            print {*STDOUT} $key. ' {' . join( ' ', @vals ) . '}' . "\n";
        }
    }
}

sub xx_read_perltidyrc {

    # Example routine to have Perl::Tidy read and validate perltidyrc
    # file, and return related flags and abbreviations.
    #
    # input parameter -
    #   $config_file is the name of a .perltidyrc file we want to read
    #   or a reference to a string or array containing the .perltidyrc file
    #   if not defined, Perl::Tidy will try to find the user's .perltidyrc
    # output parameters -
    #   $error_message will be blank unless an error occurs
    #   $rOpts - reference to the hash of options in the .perlticyrc
    # NOTE:
    #   Perl::Tidy will croak or die on certain severe errors

    my ($config_file) = @_;
    my $error_message = "";
    my %Opts;    # any options found will be put here

    # the module must be installed for this to work
    eval "use Perl::Tidy";
    if ($@) {
        $error_message = "Perl::Tidy not installed\n";
        return ( $error_message, \%Opts );
    }

    # be sure this version supports this
    my $version = $Perl::Tidy::VERSION;
    if ( $version < 20060528 ) {
        $error_message = "perltidy version $version cannot read options\n";
        return ( $error_message, \%Opts );
    }

    my $stderr = "";    # try to capture error messages
    my $argv   = "";    # do not let perltidy see our @ARGV

    # we are going to make two calls to perltidy...
    # first with an empty .perltidyrc to get the default parameters
    my $empty_file = "";    # this will be our .perltidyrc file
    my %Opts_default;       # this will receive the default options hash
    my %abbreviations_default;
    my $err = Perl::Tidy::perltidy(
        perltidyrc         => \$empty_file,
        dump_options       => \%Opts_default,
        dump_options_type  => 'full',                  # 'full' gives everything
        dump_abbreviations => \%abbreviations_default,
        stderr             => \$stderr,
        argv               => \$argv,
    );
    if ($err) {
        die "Error calling perltidy\n";
    }

    # now we call with a .perltidyrc file to get its parameters
    my %Getopt_flags;
    my %sections;
    my %abbreviations;
    Perl::Tidy::perltidy(
        perltidyrc            => $config_file,
        dump_options          => \%Opts,
        dump_options_type     => 'perltidyrc',      # default is 'perltidyrc'
        dump_getopt_flags     => \%Getopt_flags,
        dump_options_category => \%sections,
        dump_abbreviations    => \%abbreviations,
        stderr                => \$stderr,
        argv                  => \$argv,
    );

    # try to capture any errors generated by perltidy call
    # but for severe errors it will typically croak
    $error_message .= $stderr;

    # debug: show how everything is stored by printing it out
    my $DEBUG = 1;
    if ($DEBUG) {
        print {*STDOUT} "---Getopt Parameters---\n";
        foreach my $key ( sort keys %Getopt_flags ) {
            print {*STDOUT} "$key$Getopt_flags{$key}\n";
        }
        print {*STDOUT} "---Manual Sections---\n";
        foreach my $key ( sort keys %sections ) {
            print {*STDOUT} "$key -> $sections{$key}\n";
        }
        print {*STDOUT} "---Abbreviations---\n";
        foreach my $key ( sort keys %abbreviations ) {
            my @names = @{ $abbreviations{$key} };
            print {*STDOUT} "$key -> {@names}\n";
            unless ( $abbreviations_default{$key} ) {
                print {*STDOUT} "NOTE: $key is user defined\n";
            }
        }
    }

    return ( $error_message, \%Opts, \%Getopt_flags, \%sections,
        \%abbreviations, \%Opts_default, \%abbreviations_default, );
}
