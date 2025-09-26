## Step 1: Preparation

1. Download the files `RNAseq.FastqPath.R`, `RNAseq.main.R`, and `RNAseq.parameter.yml` to your computer, whether it's Linux or Windows-wsl.  
2. Install the following required software: fastp, bowtie2, STAR, RSEM, Rscript. Update your software paths by following the examples in `RNAseq.parameter.yml` (please use absolute paths).  
3. Refer to the `RNAseq.makeRef.sh` file to build the references required for each software.  
4. When `splicing: true`, the `rmats.py` and `rmats.main` parameters can be supplied with the specified Python executable and `rmats-turbo-master/rmats.py`. Alternatively, you can provide the `rmats-turbo-master/run_rmats` for one parameter and leave the other blank.  
5. Should you encounter slow RSEM performance while using **Windows-wsl**, attempt to set the outdir to a directory within the native WSL file system, such as shifting the path from `/mnt/e/outdir` to `~/outdir` or make `windows-wsl: true`.

## Step 2: Automatically identify fastq data

Run `Rscript RNAseq.FastqPath.R` with the following arguments:  
1. `<path to raw data directory>`: The path to the folder containing your fastq.gz files.  
2. `<regex for raw data suffix, e.g., _R[1-2].fq.gz>`: The regular expression for the fastq.gz file suffix.  
3. `<path to the example RNAseq.parameter.yml>`: The path to the example `RNAseq.parameter.yml` file. Remember to remove the samples section from the example parameter.yml.  
  
Such as: `Rscript RNAseq.FastqPath.R /mnt/d/fastq _R[1-2].fq.gz RNAseq.parameter.yml`  
  
The `RNAseq.FastqPath.R` script generates an `RNAseq.parameter.yaml` file, populating it with parameters from the example file and the automatically identified fastq.gz paths.  

## Step 3: Start the main program

Review and modify the `RNAseq.parameter.yaml` file, then run: `Rscript RNAseq.main.R RNAseq.parameter.yaml`.  

## Step 4: Start the statistics program

Run `Rscript RNAseq.stat.R RNAseq.parameter.yaml`

## Example

`Rscript RNAseq.FastqPath.R . _R[1_2].fq.gz RNAseq.parameter.yml`  
`Rscript RNAseq.main.R RNAseq.parameter.yaml`  
`Rscript RNAseq.stat.R RNAseq.parameter.yaml`
