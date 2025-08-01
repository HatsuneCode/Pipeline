## download from NCBI nt library ##
## https://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/nt.gz
## https://ftp.ncbi.nlm.nih.gov/blast/db/FASTA/nt.gz.md5
# pigz -d -k -p 16 nt.gz -c > /mnt/e/NCBI.NT.20240208/nt.fasta
# mkdir nt
# makeblastdb -in nt.fasta -dbtype nucl -out nt/nt.db
blastn -db nt -query unmap.fa -num_threads 16 -max_target_seqs 1 -out blast.txt
## also can remote online ##
blastn -db nt -remote -query unmap.fa -max_target_seqs 1 -out blast.txt
