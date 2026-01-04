import sys
import pandas as pd
from pyalex import Works, config
from datetime import datetime

config.email = "??????@qq.com"

def now():
	return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

def reconstruct_abstract(inverted_index):
	"""将 OpenAlex 的倒排索引摘要还原为文本"""
	if not inverted_index:
		return "No Abstract"
	# 计算摘要长度并填充单词
	word_index = []
	for word, pos_list in inverted_index.items():
		for pos in pos_list:
			word_index.append((pos, word))
	# 按照位置排序
	word_index.sort(key=lambda x: x[0])
	return " ".join([word for pos, word in word_index])

## 1. 检查命令行参数
if len(sys.argv) < 2:
	print(f"{now()} 错误: 请提供搜索日期 (YYYY-MM-DD)。")
	sys.exit()

search_date = sys.argv[1].replace('/', '-')

## 2. 执行搜索
print(f"{now()} 正在从 OpenAlex 调取 {search_date} 的文献...")
# 筛选 Domain 领域
query = Works().filter(primary_topic={"domain.id": "1|4"})
# 使用 filter 筛选出版日期
query = query().filter(from_publication_date=search_date, to_publication_date=search_date)
results = query.get()

## 3. 提取信息
final_list = []
count = 0

for page in query.paginate(per_page=200):
	for work in page:
		count += 1
		# 还原摘要
		abstract_text = reconstruct_abstract(work.get('abstract_inverted_index'))
    
		# 过滤掉无摘要的（与你 PubMed 代码逻辑一致）
		if abstract_text == "No Abstract":
			continue

		# 提取作者信息
		authorships = work.get('authorships', [])
		author_names = [a.get('author', {}).get('display_name', '') for a in authorships]
    
		# 提取最后一位作者及其机构
		last_author = ""
		last_inst = "No Institution"
		if authorships:
			last_author_info = authorships[-1]
			last_author = last_author_info.get('author', {}).get('display_name', '')
			inst_list = last_author_info.get('institutions', [])
			if inst_list:
				names = [inst.get('display_name') for inst in inst_list if inst.get('display_name')]
				if names:
					last_inst = "; ".join(names)
		primary_location = work.get('primary_location') or {}
		source = primary_location.get('source') or {}
		journal_name = source.get('display_name', 'N/A')
		row = {
			'DOI': work.get('doi', '').replace('https://doi.org/', '') if work.get('doi') else '',
			'Title': work.get('title', ''),
			'PubDate': work.get('publication_date', ''),
			'AuthorList': ", ".join(author_names),
		 	'LastAuthor': last_author,
			'LastAuthorInstitution': last_inst,
		 	'FullJournalName': journal_name,
			'PubTypeList': work.get('type', ''),
			'Abstract': abstract_text
		}
		final_list.append(row)
	print(f"{now()} 已处理 {count} 篇...")

## 4. 输出
df = pd.DataFrame(final_list)
if not df.empty:
	output_file = f"openalex_{search_date}.txt"
	df.to_csv(output_file, sep='\t', index=False, encoding='utf-8')
	print(f"{now()} 处理完成！过滤后剩余 {len(df)} 篇文献。")
	print(f"{now()} 结果已保存至: {output_file}")
else:
	print(f"{now()} 未找到符合条件的文献。")
