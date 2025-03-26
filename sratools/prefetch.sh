#！/bin/bash
sra_list_file="F:\xie_task\bioinfo\SRR_Acc_List.txt"
download_to_folder="F:\xie_task\bioinfo\datasets"

prefetch -p -f no --option-file ${sra_list_file} -O ${download_to_folder}
