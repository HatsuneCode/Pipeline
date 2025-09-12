## Step 1: Preparation

Download the files `RNAseq.FastqPath.R`, `RNAseq.main.R`, and `RNAseq.parameter.yml` to your computer, whether it's Linux or Windows-wsl.  
Install the following required software: fastp, bowtie2, STAR, RSEM, Rscript.  
Update your software paths by following the examples in `RNAseq.parameter.yml` (please use absolute paths).  

## Step 2: Automatically identify Fastq data.

Run `Rscript RNAseq.FastqPath.R` with the following arguments:  
>`<path to raw data directory>`: The path to the folder containing your fastq.gz files.  
    `<regex for raw data suffix, e.g., _R[1-2].fq.gz>`: The regular expression for the fastq.gz file suffix.  
    `<path to the example RNAseq.parameter.yml>`: The path to the example `RNAseq.parameter.yml` file.  
