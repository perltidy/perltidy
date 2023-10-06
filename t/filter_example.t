# Test use of prefilter and postfilter parameters
use strict;
use Carp;
use Perl::Tidy;
use Test::More;
my $name = 'filter_example';

BEGIN {
    plan tests => 1;
}

my $source = <<'ENDS';
use Method::Signatures::Simple;

 method foo1 { $self->bar }

       # with signature
    method foo2($bar, %opts) { $self->bar(reverse $bar) if $opts{rev};
    }

    # attributes
    method foo3 : lvalue { $self->{foo} 
}

 # change invocant name
    method 
foo4 ( $class : $bar ) { $class->bar($bar) }
ENDS

my $expect = <<'ENDE';
use Method::Signatures::Simple;
method foo1 { $self->bar }

# with signature
method foo2 ( $bar, %opts ) {
    $self->bar( reverse $bar ) if $opts{rev};
}

# attributes
method foo3 : lvalue {
    $self->{foo};
}

# change invocant name
method foo4 ( $class : $bar ) { $class->bar($bar) }
ENDE

my $output;
my $stderr_string;
my $errorfile_string;
# -ssp=2 is needed to keep formatting unchanged with new -ssp parameter
my $params = "-ssp=2";
my $err    = Perl::Tidy::perltidy(

    #argv => '-npro',  # fix for RT#127679, avoid reading unwanted .perltidyrc
    argv       => '',
    perltidyrc => \$params,    # avoid reading unwanted .perltidyrc
    prefilter =>
      sub { $_ = $_[0]; s/^\s*method\s+(\w.*)/sub METHOD_$1/gm; return $_ },
    postfilter  => sub { $_ = $_[0]; s/sub\s+METHOD_/method /gm; return $_ },
    source      => \$source,
    destination => \$output,
    stderr      => \$stderr_string,
    errorfile => \$errorfile_string,    # not used when -se flag is set
);
if ( $err || $stderr_string || $errorfile_string ) {
    ok(0);
}
else {
    is( $output, $expect, $name );
}
