import sys
import requests
import pandas as pd
from datetime import datetime

def now():
	return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

## 1. 检查命令行参数
if len(sys.argv) < 2:
	print(f"{now()} 错误: 请提供搜索日期 (YYYY/MM/DD)。")
	print("用法示例: python 3.rxiv_api.py '2026/01/02'")
	sys.exit()

search_date = sys.argv[1].replace('/', '-')

def fetch_rxiv_data(server, date):
	"""
	从 biorxiv 或 medrxiv 获取指定日期的所有数据
	server: 'biorxiv' or 'medrxiv'
	"""
	all_papers = []
	cursor = 0
	print(f"{now()} 正在从 {server} 调取 {date} 的文献...")
	while True:
		# API 格式: https://api.biorxiv.org/details/[server]/[start_date]/[end_date]/[cursor]
		url = f"https://api.biorxiv.org/details/{server}/{date}/{date}/{cursor}"
		try:
			response = requests.get(url)
			data = response.json()
		except Exception as e:
			print(f"{now()} 请求出错: {e}")
			break
		# 检查是否有数据
		messages = data.get('messages', [])
		if not messages or messages[0].get('status') != 'ok':
			break
		collection = data.get('collection', [])
		if not collection:
			break
		all_papers.extend(collection)
		# 判断是否还有下一页
		total = int(messages[0].get('total', 0))
		count = int(messages[0].get('count', 0))
		cursor += count
		print(f"{now()} 目前已从 {server} 处理文献数目: {cursor}")
		if cursor >= total:
			break
	return all_papers

## 2. 执行搜索 (同时抓取两个平台)
servers = ['biorxiv', 'medrxiv']
combined_data = []

for s in servers:
	papers = fetch_rxiv_data(s, search_date)
	combined_data.extend(papers)

## 3. 提取信息并整合
final_list = []

for work in combined_data:
	# 过滤掉无摘要的
	abstract_text = work.get('abstract', '').strip()
	if not abstract_text or abstract_text.lower() == "no abstract":
		continue
	# bioRxiv 的作者是以分号分隔的字符串
	author_str = work.get('authors', '')
	author_names = [a.strip() for a in author_str.split(';') if a.strip()]
	# 提取最后一位作者
	last_author = author_names[-1] if author_names else ""
	# bioRxiv API 提供对应机构信息 (通常是通讯作者机构)
	last_inst = work.get('author_corresponding_institution', 'No Institution')
	row = {
		'DOI': work.get('doi', ''),
		'Title': work.get('title', ''),
		'PubDate': work.get('date', ''),
		'AuthorList': ", ".join(author_names),
		'LastAuthor': last_author,
		'LastAuthorInstitution': last_inst,
		'FullJournalName': work.get('server', '').upper(),
		'PubTypeList': 'Preprint',
		'Abstract': abstract_text
	}
	final_list.append(row)

## 4. 输出
df = pd.DataFrame(final_list)
if not df.empty:
	output_file = f"rxiv_{search_date}.txt"
	# 保持与你之前代码一致的制表符分隔格式
	df.to_csv(output_file, sep='\t', index=False, encoding='utf-8')
	print(f"{now()} 处理完成！从 bioRxiv/medRxiv 过滤后剩余 {len(df)} 篇文献。")
	print(f"{now()} 结果已保存至: {output_file}")
else:
	print(f"{now()} 在 bioRxiv/medRxiv 未找到符合条件的文献。")
