srs: Pendulum_SRS.pdf

Pendulum_SRS.pdf: Pendulum_SRS.tex
	lualatex $(TEXFLAGS) Pendulum_SRS
	-bibtex $(BIBTEXFLAGS) Pendulum_SRS
	lualatex $(TEXFLAGS) Pendulum_SRS
	lualatex $(TEXFLAGS) Pendulum_SRS

.PHONY: srs
