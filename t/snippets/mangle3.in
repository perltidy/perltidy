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
