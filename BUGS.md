# Perltidy open BUGS and LIMITATIONS

This file only lists open bugs.  For bugs which have been fixed, see the
ChangeLog.

## The Pod:Html module has some bugs

Perltidy uses the module Pod::Html, and for the most part it works very well
and is very convenient because it part of the standard Perl distribution.  But
for example the following line

    =item B<< <Deck> = Session->new_cflt_deck; >>

which uses double brackets to contain single brackets does not render correctly.

## Issues and Feature Requests

The most recent Issues and Feature requests can be seen [at GitHub](https://github.com/perltidy/perltidy)
