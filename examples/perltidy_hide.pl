#!/usr/bin/perl
package main;
use Perl::Tidy;

# Note: This program is no longer necessary because this capability is now
# built into perltidy, but the program is a good example of the use of
# prefilters and postfilters.

=pod

Hide sections of as script from perltidy which are between special comments,
like this:

#<<V

anything between '#<<V' and '#>>V' is hidden from perltidy but seen by perl

#>>V

This works by converting #<<V into =pod and #>>V into =cut before
processing, and then converting back after processing.

This was created for issue git #65. 

=cut

my $arg_string = undef;

exit Perl::Tidy::perltidy(
    argv      => $arg_string,
    prefilter => sub {
        $_ = $_[0];
        s/^(#<<V\b.*)$/=pod $1/gm;
        s/^(#>>V\b.*)$/=cut $1/gm;
        return $_;
    },
    postfilter => sub {
        $_ = $_[0];
        s/^=pod (#<<V\b)/$1/gm;
        s/^=cut (#>>V\b)/$1/gm;
        return $_;
    },
);
