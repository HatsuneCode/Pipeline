import sys
import re
from Bio import Entrez
import time
import xml.etree.ElementTree as ET
import pandas as pd
from datetime import datetime

def now():
    return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

## 1. 检查命令行参数 ##
if len(sys.argv) < 2:
	print(f"{now()} 错误: 请提供搜索日期。")
	print("用法示例: python 1.pubmed.py '2026/01/02'")
	sys.exit()

Entrez.email = "??@qq.com"
search_date = sys.argv[1]

## 2. 搜索 IDs ##
print(f"{now()} 正在搜索 {search_date} 的文献...")
handle = Entrez.esearch(db = "pubmed", term = "", mindate = search_date, maxdate = search_date, datetype = "pdat", retmax = 100000)
ids = Entrez.read(handle)['IdList']
handle.close()
print(f"{now()} 找到共计 {len(ids)} 篇文献。")
if not ids:
	print(f"{now()} 未找到文献，程序结束。")
	sys.exit()

## 3. 获取 Summary (元数据) ##
## Title, PubDate, abstract-no, AuthorList, LastAuthor, institution-no, FullJournalName, PubTypeList, dio-no
print(f"{now()} 正在下载详细信息...")
handle = Entrez.esummary(db = "pubmed", id = ",".join(ids))
papers = Entrez.read(handle)
handle.close()

## 4. 获取 Abstract (摘要) ##
print(f"{now()} 正在提取 XML 信息...")
handle = Entrez.efetch(db = "pubmed", id = ",".join(ids), retmode = "xml")
xml_data = handle.read()
handle.close()
root = ET.fromstring(xml_data)
papers_data = []
for article in root.findall(".//PubmedArticle"):
	## 摘要
	abstract_elements = article.findall(".//AbstractText")
	abstract_parts = []
	for elem in abstract_elements:
		text_content = "".join(elem.itertext()).strip()
		text_content = re.sub(r'<[^>]*>', '', text_content)
		label = elem.get("Label")
		if label:
			abstract_parts.append(f"{label}: {text_content}")
		else:
			abstract_parts.append(text_content)
	abstract = "\n".join(abstract_parts) if abstract_parts else "No Abstract"
	## 作者
	last_author_inst = "No Institution"
	author_list = article.findall(".//AuthorList/Author")
	if author_list:
		last_author = author_list[-1]
		aff_elem = last_author.find(".//Affiliation")
		if aff_elem is not None and aff_elem.text:
			last_author_inst = aff_elem.text
	papers_data.append({
		'Abstract': abstract.replace('\xa0', ' '),
		'LastAuthorInstitution': last_author_inst })

## 5. 整合数据并生成 DataFrame
print(f"{now()} 正在整理 pubmed 文献表单...")
final_list = []
for summary, detail in zip(papers, papers_data):
	# 如果摘要是 "No Abstract"，则跳过该条文献
	if detail['Abstract'] == "No Abstract":
		continue

	row = {
		'DOI': summary.get('DOI', ''),
		'Title': summary.get('Title', ''),
		'PubDate': summary.get('PubDate', ''),
		'AuthorList': ", ".join(summary.get('AuthorList', [])),
		'LastAuthor': summary.get('LastAuthor', ''),
		'LastAuthorInstitution': detail['LastAuthorInstitution'],
		'FullJournalName': summary.get('FullJournalName', ''),
		'PubTypeList': ", ".join(summary.get('PubTypeList', [])),
		'Abstract': detail['Abstract']
	}
	final_list.append(row)

df = pd.DataFrame(final_list)

## 6. 输出为 TXT (以制表符 \t 分隔)
print(f"{now()} 正在输出表单文件...")
if not df.empty:
	output_file = f"pubmed_{search_date.replace('/', '-')}.txt"
	df.to_csv(output_file, sep='\t', index=False, encoding='utf-8')
	print(f"{now()} 处理完成！过滤后剩余 {len(df)} 篇文献。")
	print(f"{now()} 结果已保存至: {output_file}")
else:
	print(f"{now()} 过滤后没有符合条件的文献（全部无摘要）。")
