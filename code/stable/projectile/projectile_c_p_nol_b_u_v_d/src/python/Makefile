build:

run: build
	python Control.py $(RUNARGS)

test: build
	pytest -v *Test.py
	rm output.txt

doc: doxConfig Control.py InputParameters.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
