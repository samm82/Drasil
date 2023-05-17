build:

run: build
	python3 Control.py $(RUNARGS)

test: build
	pytest -v test
	rm output.txt

doc: doxConfig Control.py InputParameters.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
