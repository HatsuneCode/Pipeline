## value: $1 is id (custom name); $2 is fastq prefix
cellranger=/home/songlianhao/software/cellranger-8.0.1/cellranger
reference=/home/songlianhao/reference/10X/refdata-gex-GRCm39-2024-A
fastqs=/home/songlianhao/Fastq/10xV3/
wdir=/home/songlianhao/Project/1.cellranger

id=$1
sample=$2

## cellranger code
mkdir -p $wdir; cd $wdir
$cellranger count --id $id --transcriptome $reference --fastqs $fastqs --sample $sample --create-bam true --localcores 8 --localmem 90
echo done
