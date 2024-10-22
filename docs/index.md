# Welcome to Perltidy

Perltidy is a Perl script which indents and reformats Perl scripts to make them
easier to read.  If you write Perl scripts, or spend much time
reading them, you will probably find it useful.

Perltidy is free software released under the GNU General Public
License -- please see the included file [COPYING](../COPYING) for details.

The formatting can be controlled with command line parameters.  The default
parameter settings approximately follow the suggestions in the
[Perl Style Guide](https://perldoc.perl.org/perlstyle.html).

Besides reformatting scripts, Perltidy can help in tracking
down errors with missing or extra braces, parentheses, and square brackets
because it is very good at localizing errors.

## Documentation

- [A Brief Perltidy Tutorial](./tutorial.html)

- [Perltidy Style Key](./stylekey.html) will help
 in methodically selecting a set of style parameters.

- [The Perltidy man page](./perltidy.html) explains how
to precisely control the formatting details.

- [The Perl::Tidy man page](./Tidy.html) discusses how to use the Perl::Tidy module

- [Change Log](./ChangeLog.html)


## Prerequisites

Perltidy should run on any system with perl 5.008 or later.
The total disk space needed after removing the installation directory will be
about 4 Mb.


## Download


- The most recent release is always at [CPAN](https://metacpan.org/release/Perl-Tidy)

- The most recent release is also at [sourceforge](https://sourceforge.net/projects/perltidy/)


## Installation

Perl::Tidy can be installed directly from CPAN one of the standard methods.

One way is to download a distribution file, unpack it and then 
test and install using the Makefile.PL:

    perl Makefile.PL
    make
    make test
    make install

The [INSTALL file](./INSTALL.html) has additional installation notes. They
are mainly for older systems but also tell how to use perltidy without doing an installation.

## Links

 - [Perl::Tidy source code repository at GitHub](https://github.com/perltidy/perltidy)
 - [tidyall](https://metacpan.org/pod/distribution/Code-TidyAll/bin/tidyall) is a great tool for automatically running perltidy and other tools including perlcritic on a set of project files.
 - [Tidyview](http://sourceforge.net/projects/tidyview) is a graphical program for tweaking your .perltidyrc configuration parameters.
 - [A perltidy plugin for Sublime Text 2/3](https://github.com/vifo/SublimePerlTidy)


## FEEDBACK / BUG REPORTS

The best place to report bugs and issues is [GitHub](https://github.com/perltidy/perltidy/issues)

Bugs and issues can also be reported at the CPAN site [https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)
