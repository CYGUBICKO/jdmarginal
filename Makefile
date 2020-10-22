## This is the survey repo for Chyun's awareness project

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt README.md Meetings.md"

######################################################################

Sources += $(wildcard *.md *.R *.rmd)

## MREB.review.md

automatic_makeR = defined

######################################################################

varpred.Rout: varpred.R

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

-include makestuff/os.mk
-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
-include makestuff/makeR.mk
-include makestuff/pandoc.mk

