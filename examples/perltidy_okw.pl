#!/usr/bin/perl -w

# Example use a perltidy postfilter to outdent certain leading keywords

# Usage:
# perltidy_okw.pl -sil=1 file.pl

# This version outdents hardwired keywords 'step', 'command', and 'expected'
# The following is an example of the desired effect. The flag -sil=1 is
# needed to get a starting indentation level so that the outdenting 
# is visible.

=pod
step 4;
command 'Share project: project1';
expected 'A project megjelenik a serveren';
      shareProject ('project1', 'login', '123', Login => 1, PortalServer =>
$openJoinAddress);
      valueCheck ('project1_share', listBIMCloudData ('projects'));


step 5;
command 'quitAC';
      quitAC ();
=cut

# Run it exactly like perltidy, and the postfilter removes the
# leading whitespace of lines which begin with your keywords.  The
# postfilter works on the file as a single string, so the 'm' quote
# modifier is needed to make the ^ and $ string positioners work

# See http://perltidy.sourceforge.net/Tidy.html for further details
# on how to call Perl::Tidy
use Perl::Tidy;
my $arg_string = undef;
my $err=Perl::Tidy::perltidy(
    argv => $arg_string,
    postfilter =>
      sub { $_ = $_[0]; s/^\s*(step|command|expected)(.*)$/$1$2/gm; return $_ }
);
if ($err) {
    die "Error calling perltidy\n";
}
