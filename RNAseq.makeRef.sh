##
rRNAref=/home/songlianhao/reference/Mus/rRNA/rRNA
STARref=/home/songlianhao/reference/Mus/starRef
RSEMref=/home/songlianhao/reference/Mus/rsemRef/rsemRef
##
bowtie2-build rRNA.fa $rRNAref
STAR --runThreadN 6 --runMode genomeGenerate --genomeDir $STARref --genomeFastaFiles Mus_musculus.GRCm39.dna_sm.primary_assembly.fa --sjdbGTFfile Mus_musculus.GRCm39.111.gtf --sjdbOverhang 149
rsem-prepare-reference Mus_musculus.GRCm39.dna_sm.primary_assembly.fa --gtf Mus_musculus.GRCm39.111.gtf $RSEMref
