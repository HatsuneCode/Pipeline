## download from NCBI nt library ##
## https://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/nt.gz
## https://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/nt.gz.md5
# pigz -d -k -p 16 nt.gz -c > /mnt/e/NCBI.NT.20240208/nt.fasta
# mkdir nt
# makeblastdb -in nt.fasta -dbtype nucl -out nt/nt.db

## also can remote online ##
blastn -db nt -remote -query unmap.fa -num_threads 16
