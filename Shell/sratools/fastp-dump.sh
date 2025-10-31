fasterq-dump --split-3 ${sra_file} -O ${output_dir} -v

gzip -1 ${file}
