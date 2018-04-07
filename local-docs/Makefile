.phony : docs

CHANGES="../CHANGES.md"
INSTALL="../INSTALL.md"
README="../README.md"
BUGS="../BUGS.md"

DOCS=INSTALL \
README \
BUGS \
CHANGES \
perltidy.md \
tutorial.md

docs: $(DOCS)

README: README.pod
	pod2markdown README.pod >../README.md

INSTALL: INSTALL.pod
	pod2markdown INSTALL.pod >../INSTALL.md

perltidy.md: ../bin/perltidy
	pod2markdown ../bin/perltidy >perltidy.md

tutorial.md: tutorial.pod
	pod2markdown tutorial.pod >tutorial.md

BUGS: BUGS.pod
	pod2markdown BUGS.pod ../BUGS.md

CHANGES: ChangeLog.pod
	pod2markdown ChangeLog.pod >../CHANGES.md
	
