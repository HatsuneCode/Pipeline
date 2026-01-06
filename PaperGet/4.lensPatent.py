import sys
import requests
import pandas as pd
from datetime import datetime
import time

LENS_API_KEY = '配置你的 Lens API Key'
TARGET_DOMAINS = ['A']

def now():
	return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

## 1. 检查命令行参数
if len(sys.argv) < 2:
	print(f"{now()} 错误: 请提供搜索日期 (YYYY-MM-DD)。")
	sys.exit()

search_date = sys.argv[1].replace('/', '-')

def fetch_lens_patents(date):
	"""
	使用 Lens Patent API 获取指定日期的专利
	"""
	url = 'https://api.lens.org/patent/search'
	headers = {
		'Authorization': LENS_API_KEY,
		'Content-Type': 'application/json'
	}

	# 基础查询：日期
	must_conditions = [
		{"range": {"publication_date": {"gte": date, "lte": date}}}
	]

	# 加入领域筛选 (IPC 前缀)
	if domains:
		domain_query = {
			"bool": {
			"should": [{"prefix": {"class_ipc.symbol": d}} for d in domains]
			}
        	}
        	must_conditions.append(domain_query)

	# 构建查询：按发布日期筛选
	# 这里的 query 逻辑是：publication_date 在指定的这一天内
	query_body = {
		"query": {
			"bool": {
				"must": must_conditions
			}
		},
        	"size": 100,  # 每页记录数 (最大100)
        	"from": 0,
        	"include": ["lens_id", "pub_date", "title", "abstract", "inventors", "applicants", "jurisdiction", "type"]
	}

	all_patents = []
	print(f"{now()} 正在从 Lens API 调取 {date} 的专利信息...")

	while True:
		response = requests.post(url, json=query_body, headers=headers)
		if response.status_code != 200:
			print(f"{now()} 错误: API 请求失败，状态码: {response.status_code}")
			print(response.text)
			break

		data = response.json()
		results = data.get('data', [])
		if not results:
			break

		all_patents.extend(results)

		# 翻页处理
		total = data.get('total', 0)
		query_body['from'] += len(results)

		if query_body['from'] >= total or query_body['from'] >= 10000:
			break

		# 稍微延迟，避免触发频率限制
		time.sleep(0.6)

	return all_patents

## 2. 执行抓取
patents_data = fetch_lens_patents(search_date, domains = TARGET_DOMAINS)

## 3. 提取信息并整合
final_list = []

for p in patents_data:
	# 提取摘要
	abstracts = p.get('abstract', [])
	eng_abs = [a.get('value') for a in abstracts if a.get('lang') == 'en']
	abstract_text = eng_abs[0] if eng_abs else (abstracts[0].get('value', '') if abstracts else "")
	if not abstract_text or abstract_text.strip().lower() == "no abstract":
		continue

	# 提取发明人
	inventors = p.get('inventors', [])
	inventor_names = [f"{i.get('first_name', '')} {i.get('last_name', '')}".strip() for i in inventors]
	inventor_names = [n for n in inventor_names if n]
    
	# 提取第一位发明人
	first_author = inventor_names[0] if inventor_names else "N/A"

	# 提取申请机构 (对应 Institution)
	applicants = p.get('applicants', [])
	inst_names = [a.get('name', '') for a in applicants if a.get('name')]
	institution = "; ".join(inst_names) if inst_names else "No Institution"

	row = {
		'DOI': p.get('lens_id', ''), 
		'Title': p.get('title', [{}])[0].get('value', '') if p.get('title') else 'N/A',
		'PubDate': p.get('pub_date', ''),
		'AuthorList': ", ".join(inventor_names),
		'LastAuthor': first_author,
		'Institution': institution,
		'Source': f"Patent ({p.get('jurisdiction', '')})",
		'Type': p.get('type', 'Patent'),
		'Abstract': abstract_text.replace('\t', ' ').replace('\n', ' ')
	}
	final_list.append(row)

# 3. 输出
if final_list:
	df = pd.DataFrame(final_list)
	output_file = f"lensPatent_{search_date}.txt"
	df.to_csv(output_file, sep='\t', index=False, encoding='utf-8')
	print(f"{now()} 完成！提取到 {len(df)} 条专利数据。")
else:
	print(f"{now()} 未找到符合条件的专利。")
