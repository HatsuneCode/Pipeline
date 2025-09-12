# Step 1: Preparation

Download the files `RNAseq.FastqPath.R`, `RNAseq.main.R`, and `RNAseq.parameter.yml` to your computer, whether it's Linux or Windows.
Install the following required software: fastp, bowtie2, STAR, RSEM, Rscript.
Update your software paths by following the examples in `parameter.yml`. Please use absolute paths.



第二步：运行 Rscript RNAseq.FastqPath.R <原始数据文件夹路径> <原始数据的后缀正则，如 _R[1-2].fq.gz> <示例的 RNAseq.parameter.yml 路径>
