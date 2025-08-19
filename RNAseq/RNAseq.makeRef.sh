## Download Ensembl 111
wget https://ftp.ensembl.org/pub/release-111/fasta/mus_musculus/dna/Mus_musculus.GRCm39.dna_sm.primary_assembly.fa.gz
wget https://ftp.ensembl.org/pub/release-111/gtf/mus_musculus/Mus_musculus.GRCm39.111.gtf.gz
## Download rRNA
wget https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/001/635/GCF_000001635.27_GRCm39/GCF_000001635.27_GRCm39_rna_from_genomic.fna.gz

## Path
rRNAref=rRNA/rRNA
STARref=starRef
RSEMref=rsemRef/rsemRef

## rRNA
less -mSN GCF_000001635.27_GRCm39_rna_from_genomic.fna.gz | grep '^>' | grep 'gbkey=rRNA' | awk '{print $1}'| sed 's/>//g' > rna.id.list
seqkit grep -f rna.id.list GCF_000001635.27_GRCm39_rna_from_genomic.fna.gz > rRNA.fa
mkdir rRNA
bowtie2-build rRNA.fa $rRNAref
## STAR
gunzip Mus_musculus.GRCm39.dna_sm.primary_assembly.fa.gz Mus_musculus.GRCm39.111.gtf.gz
STAR --runThreadN 6 --runMode genomeGenerate --genomeDir $STARref --genomeFastaFiles Mus_musculus.GRCm39.dna_sm.primary_assembly.fa --sjdbGTFfile Mus_musculus.GRCm39.111.gtf --sjdbOverhang 149
## RSEM
mkdir rsemRef
rsem-prepare-reference Mus_musculus.GRCm39.dna_sm.primary_assembly.fa --gtf Mus_musculus.GRCm39.111.gtf $RSEMref
## Index
samtools faidx Mus_musculus.GRCm39.dna_sm.primary_assembly.fa
