import sys
import pandas as pd
import requests
from datetime import datetime

SERPAPI_KEY = "可以朝我要 SerpAPI 的 KEY"

def now():
	return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

## 1. 检查命令行参数
if len(sys.argv) < 2:
	print(f"{now()} 错误: 请提供搜索日期 (YYYY-MM-DD)。")
	print("用法示例: python google_patents.py '2026-01-02'")
	sys.exit()

search_date = sys.argv[1].replace('/', '-')

formatted_date = search_date.replace('-', '')
cpc_codes = "(cpc:A61K OR cpc:A61P OR cpc:C12N OR cpc:C07K)"
query_str = f"{cpc_codes}; after:publication:{formatted_date}"
final_list = []
start_index = 0

print(f"{now()} 正在搜索: {query_str}")

while True:
	params = {
		"engine": "google_patents",
		"q": query_str,
		"api_key": SERPAPI_KEY,
		"num": 100,      # 每页最大100条
		"start": start_index
		}

	try:
		response = requests.get("https://serpapi.com/search", params=params)
		data = response.json()
            
		if "organic_results" not in data:
			break
            
		results = data["organic_results"]
		for patent in results:
			# 提取信息
			row = {
				'PatentID': patent.get('publication_number'),
				'Title': patent.get('title'),
				'PubDate': search_date,
				'Assignee': patent.get('assignee', 'N/A'),
				'Inventor': patent.get('inventor', 'N/A'),
				'PDF_Link': patent.get('pdf'),
				# snippet 在这里通常对应摘要的简述
				'Abstract': patent.get('snippet', '').replace('\n', ' ') 
			}
                
		# 过滤掉无摘要或无标题的（与你 OpenAlex 逻辑一致）
		if not row['Abstract'] or row['Abstract'] == "No Abstract":
			continue
                    
		final_list.append(row)

		# 检查是否有下一页
		if "serpapi_pagination" in data and "next" in data["serpapi_pagination"]:
			start_index += 100
			print(f"{now()} 已处理 {len(final_list)} 条结果，准备翻页...")
		else:
			break

	except Exception as e:
		print(f"{now()} 请求出错: {e}")
		break

print(final_list)
