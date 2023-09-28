srs: ChemCode_SRS.pdf

ChemCode_SRS.pdf: ChemCode_SRS.tex
	lualatex $(TEXFLAGS) ChemCode_SRS
	-bibtex $(BIBTEXFLAGS) ChemCode_SRS
	lualatex $(TEXFLAGS) ChemCode_SRS
	lualatex $(TEXFLAGS) ChemCode_SRS

.PHONY: srs
