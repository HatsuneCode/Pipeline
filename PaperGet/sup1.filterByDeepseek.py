import pandas as pd
import openai
from tqdm import tqdm
import time
from datetime import datetime

def now():
    return datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")

client = openai.OpenAI(
    api_key="DEEPSEEK_API_KEY", 
    base_url="https://api.deepseek.com"
)

def quick_check(title, abstract):
	"""
	极简判断：只返回 '是' 或 '否'
	"""
	prompt = f"判断该文章是否属于医学科学领域。只需回答'是'或'否'。\n标题：{title}\n摘要：{abstract}"
    
	try:
		response = client.chat.completions.create(
		model="deepseek-chat",
		messages=[
			{"role": "system", "content": "你是一个严谨的学术助理，只回答'是'或'否'，不要解释。"},
			{"role": "user", "content": prompt},
		],
		temperature=0, # 确保结果唯一
		max_tokens=2   # 强制短输出
		)
		return response.choices[0].message.content.strip()
	except Exception:
		return "网络错误"

file_path = "pubmed_2026-01-02.top10.txt"

try:
	df = pd.read_csv(file_path, sep='\t', encoding = 'gbk')
except:
	# 如果读取失败，尝试使用通用的自动识别
	df = pd.read_table(file_path)
print(f"{now()} 成功加载数据，共 {len(df)} 行。")

results = []
for index, row in tqdm(df.iterrows(), total=len(df)):
	# 提取标题和摘要列（请确保列名与你表中的实际名称一致）
	t = str(row.get('Title', ''))
	a = str(row.get('Abstract', ''))
	# 得到判断结果
	res = quick_check(t, a)
	results.append(res)
	# 每 20 行自动保存一次，防止程序中途崩溃
	if (index + 1) % 20 == 0:
		df_temp = df.iloc[:len(results)].copy()
		df_temp['是否医学'] = results
		df_temp.to_excel("temp_results.xlsx", index=False)
# 4. 最终保存
df['是否医学'] = results
print(df)
print(f"{now()} 任务完成！结果已保存至 final_classification.xlsx")
