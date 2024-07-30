# spaceranger 3.0.1
sp=/home/songlianhao/software/spaceranger-3.0.1/spaceranger
wd=/home/songlianhao/Project/GYJ/1.spaceranger
ref=/home/songlianhao/reference/10X/refdata-gex-GRCm39-2024-A
fastq=/home/songlianhao/Fastq/10xVisium/GYJ/fastq
tif=/home/songlianhao/Fastq/10xVisium/GYJ/tiff
mkdir -p $wd; cd $wd; pwd

# Samples
for i in 1 2; do for j in A B C D; do name=$i$j; echo $name
if [ $i -eq 1 ]; then slide=V13M13-387; else slide=V13Y08-069; fi
$sp count --id $name --transcriptome $ref --image $tif/S$i/$name.tif --fastqs $fastq --sample $name --localcores 6 --localmem 80 --disable-ui --create-bam true --slidefile $tif/S$i/$slide.gpr --slide $slide --area ${j}1
done; done
