use strict;
use File::Temp;
use Test;
use Carp;
BEGIN {plan tests => 4}
use Perl::Tidy;


#----------------------------------------------------------------------
## test string->string
#----------------------------------------------------------------------
my $source_template = <<'EOM';
%height=("letter",27.9, "legal",35.6, "arche",121.9, "archd",91.4, "archc",61,
 "archb",45.7, "archa",30.5, "flsa",33, "flse",33, "halfletter",21.6,
 "11x17",43.2, "ledger",27.9);
%width=("letter",21.6, "legal",21.6, "arche",91.4, "archd",61, "archc",45.7,
 "archb",30.5, "archa",22.9, "flsa",21.6, "flse",21.6, "halfletter",14,
 "11x17",27.9, "ledger",43.2);
EOM

my $perltidyrc;

my $expected_output_template=<<'EOM';
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
EOM

my $source;
my $output;
my $expected_output;

my $CR = chr(015);
my $LF = chr(012);

$perltidyrc = <<'EOM';
-gnu
--output-line-ending="unix"     # use *nix LF EOLs
EOM

$source = $source_template;
$source =~ s/\n/$CR$LF/gmsx;
$expected_output = $expected_output_template;
$expected_output =~ s/\n/$LF/gmsx;

# my ($source_fh, $source_filename) = File::Temp::tempfile(); close $source_filename;
my ($output_fh, $output_filename) = File::Temp::tempfile(); close $output_filename;

# print STDERR "# source_filename = ", $source_filename, "\n";
# print STDERR "# output_filename = ", $output_filename, "\n";

# open $source_fh, ">", $source_filename;
# binmode $source_fh, ":raw";
# print $source_fh $source;
# close $source_fh;

# in-memory output (non-UTF8)

Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '',
);

ok($output, $expected_output, "in-memory EOLs (non-UTF8)");

# file output (non-UTF8)
Perl::Tidy::perltidy(
    source      => \$source,
    destination => $output_filename,
    perltidyrc  => \$perltidyrc,
    argv        => '',
);

{# slurp entire file
    local $/ = undef;
    open $output_fh, "<", $output_filename;
    binmode $output_fh, ":raw";
    $output = <$output_fh>;
}

ok($output, $expected_output, "output file EOLs (non-UTF8)");

$perltidyrc = <<'EOM';
-gnu
--character-encoding="utf8"     # treat files as UTF-8 (decode and encode)
--output-line-ending="unix"     # use *nix LF EOLs
EOM

# in-memory (UTF8)
$source = $source_template;
$source =~ s/\n/$CR$LF/gmsx;
$expected_output = $expected_output_template;
$expected_output =~ s/\n/$LF/gmsx;

Perl::Tidy::perltidy(
    source      => \$source,
    destination => \$output,
    perltidyrc  => \$perltidyrc,
    argv        => '',
);

ok($output, $expected_output, "in-memory EOLs (UTF8)");

# file output (UTF8)

Perl::Tidy::perltidy(
    source      => \$source,
    destination => $output_filename,
    perltidyrc  => \$perltidyrc,
    argv        => '',
);

{# slurp entire file
    local $/ = undef;
    open $output_fh, "<", $output_filename;
    binmode $output_fh, ":raw";
    $output = <$output_fh>;
}

ok($output, $expected_output, "output file EOLs (UTF8)");
