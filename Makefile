WORKSHOP_RMD := $(wildcard workshop/*.Rmd)
READING_RMD := $(wildcard readings/*.Rmd)

all: gca articles

articles : $(READING_RMD)
	$(foreach rmd, $(READING_RMD), Rscript -e "rmarkdown::render('$(rmd)', output_format = 'md_document')";)

gca : $(WORKSHOP_RMD)
	$(foreach rmd, $(WORKSHOP_RMD), Rscript -e "rmarkdown::render('$(rmd)', output_format = 'md_document')";)
