# set names as variables
TEXFILE= main
RDIR= .
FIGDIR= ./figures

# list all R files
RFILES := $(RDIR)/*.R
# pdf figures created by R
PDFFIGS := $(FIGDIR)/*.pdf
# indicator files to show R file has run
OUT_FILES := $(RFILES:.R=.Rout)

all: $(TEXFILE).pdf $(OUT_FILES)

# let make know about dependencies 
$(RDIR)/plot.Rout: $(RDIR)/plot.R $(RDIR)/data.R
    R CMD BATCH $<

# RUN EVERY R FILE
$(RDIR)/%.Rout: $(RDIR)/%.R $(RDIR)/functions.R
    R CMD BATCH $<

# compile main tex file and show errors
$(TEXFILE).pdf: $(TEXFILE).tex $(OUT_FILES) 
	latexmk -pdf -quiet $(TEXFILE)

view: $(TEXFILE).pdf
	skim $(TEXFILE).pdf

# Clean up stray files
clean:
    rm -fv $(OUT_FILES)
    rm -fv $(CROP_FILES)
    rm -fv *.aux *.log *.toc *.blg *.bbl *.synctex.gz
    rm -fv *.out *.bcf *blx.bib *.run.xml
    rm -fv *.fdb_latexmk *.fls
    rm -fv $(TEXFILE).pdf
