#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
双向文件夹同步工具（单次同步模式）
G:\\Website\\htdocs  <-->  E:\\Project\\Website\\htdocs

每次运行做一次完整双向同步后退出。
利用快照文件区分"对端新增"与"本端已删"，正确传播删除操作。
"""

import os
import sys
import shutil
import fnmatch
import hashlib
import logging
from dataclasses import dataclass, field
from pathlib import Path

# ── 配置 ──────────────────────────────────────────────
DIR_USB  = Path(r"G:\Website\htdocs")          # U盘同步目录
DIR_PC   = Path(r"E:\Project\Website\htdocs")  # PC 同步目录
LOG_FILE      = DIR_USB.parent / "sync_website.log"
SNAPSHOT_FILE = Path(__file__).parent / "sync_website.snapshot"

# 排除名单：填写相对于同步根目录的路径，支持通配符 *
EXCLUDE_PATTERNS: list[str] = [
    r"NDD\img\RNAseq",
    r"NDD\img\STseq",
]
# 预编译排除规则（程序启动时只算一次）
_EXCLUDE_COMPILED: list[tuple[tuple[str, ...], str]] = [
    (tuple(p.lower() for p in Path(raw).parts), Path(raw).as_posix().lower())
    for raw in EXCLUDE_PATTERNS
]
# ──────────────────────────────────────────────────────

LOG_FILE.parent.mkdir(parents=True, exist_ok=True)  # 确保日志目录存在（首次运行时）
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(message)s",
    datefmt="%H:%M:%S",
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(str(LOG_FILE), encoding="utf-8"),
    ],
)
log = logging.getLogger(__name__)


# ── 统计 ───────────────────────────────────────────────

@dataclass
class SyncStats:
    usb_to_pc: list[str] = field(default_factory=list)  # 复制 G盘 → E盘
    pc_to_usb: list[str] = field(default_factory=list)  # 复制 E盘 → G盘
    del_pc:    list[str] = field(default_factory=list)  # 删除 E盘文件
    del_usb:   list[str] = field(default_factory=list)  # 删除 G盘文件
    skipped:   int = 0                                   # 无变化跳过
    errors:    int = 0                                   # 错误数

stats = SyncStats()


# ── 排除判断 ───────────────────────────────────────────

def is_excluded(path: Path, root: Path) -> bool:
    """检查路径（或其父目录）是否命中排除名单。大小写不敏感。"""
    try:
        rel = path.relative_to(root)
    except ValueError:
        return False

    rel_parts = tuple(p.lower() for p in rel.parts)
    rel_str   = rel.as_posix().lower()

    for pat_parts, pat_str in _EXCLUDE_COMPILED:
        if rel_parts[:len(pat_parts)] == pat_parts:
            return True
        if fnmatch.fnmatch(rel_str, pat_str):
            return True
        if "/" not in pat_str and fnmatch.fnmatch(rel_parts[-1], pat_str):
            return True

    return False


# ── 文件操作 ───────────────────────────────────────────

_md5_cache: dict[str, str] = {}  # 本次运行内的 MD5 缓存，避免同一文件重复读取

def file_md5(path: Path, chunk_size: int = 1 << 20) -> str:
    """计算文件的 MD5 十六进制摘要（分块读取，支持大文件）。结果在本次运行内缓存。"""
    key = str(path)
    if key in _md5_cache:
        return _md5_cache[key]
    h = hashlib.md5()
    with path.open("rb") as f:
        while chunk := f.read(chunk_size):
            h.update(chunk)
    _md5_cache[key] = h.hexdigest()
    return _md5_cache[key]


def safe_copy(src: Path, dst: Path, known_src_md5: str = ""):
    # 先写到同盘临时文件，完成后原子重命名，避免中断留下残缺文件
    tmp = dst.with_name("." + dst.name + ".synctmp")
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, tmp)   # 数据写完后 copy2 会设置正确的 mtime
        # MD5 校验：确认复制内容与源文件完全一致
        src_md5 = known_src_md5 or file_md5(src)
        tmp_md5 = file_md5(tmp)
        if src_md5 != tmp_md5:
            raise ValueError(f"MD5 校验失败 src={src_md5} dst={tmp_md5}")
        tmp.replace(dst)         # 同一文件系统内原子替换
        _md5_cache[str(dst)] = src_md5  # dst 内容与 src 相同，直接复用已知 MD5
        log.info(f"[复制] {src}  →  {dst}  (MD5: {src_md5[:8]}…)")
        if src.is_relative_to(DIR_USB):
            stats.usb_to_pc.append(str(src))
        else:
            stats.pc_to_usb.append(str(src))
    except Exception as e:
        log.error(f"[复制失败] {src} → {dst}  ({e})")
        if tmp.exists():
            try:
                tmp.unlink()
            except Exception:
                pass
        stats.errors += 1


def safe_delete(dst: Path):
    try:
        # 在删除前判断路径归属
        is_pc_side = dst.is_relative_to(DIR_PC)
        if not dst.exists():
            return  # 文件已不存在，不计入统计
        dst.unlink()
        log.info(f"[删文件] {dst}")
        if is_pc_side:
            stats.del_pc.append(str(dst))
        else:
            stats.del_usb.append(str(dst))
    except Exception as e:
        log.error(f"[删除失败] {dst}  ({e})")
        stats.errors += 1


# ── 快照 ───────────────────────────────────────────────

def load_snapshot() -> dict[str, str] | None:
    """加载上次保存的快照；返回 {相对路径: md5} 字典；快照不存在时返回 None。
    兼容旧格式（仅路径，无 MD5）：旧格式条目 MD5 记为空字符串。
    """
    if not SNAPSHOT_FILE.exists():
        return None
    try:
        lines = SNAPSHOT_FILE.read_text(encoding="utf-8").splitlines()
        result: dict[str, str] = {}
        for line in lines:
            line = line.strip()
            if not line:
                continue
            # 新格式：md5hash  rel/path（两空格分隔）
            if "  " in line:
                md5, _, rel = line.partition("  ")
                result[rel.strip()] = md5.strip()
            else:
                # 旧格式：仅路径，MD5 未知
                result[line] = ""
        log.info(f"[快照] 已加载 {len(result)} 条记录")
        return result
    except Exception as e:
        log.warning(f"[快照] 读取失败，将按首次运行处理: {e}")
        return None


def save_snapshot():
    """将当前两侧文件列表及 MD5 保存为快照。
    两侧相同文件只记录一次（MD5 应相同；若不同说明同步未完成，取 USB 侧）。
    """
    try:
        # rel_path → md5
        records: dict[str, str] = {}
        for root in (DIR_USB, DIR_PC):
            if not root.exists():
                continue
            for dirpath, dirs, files in os.walk(root):
                dirpath = Path(dirpath)
                dirs[:] = [d for d in dirs if not is_excluded(dirpath / d, root)]
                for fname in files:
                    if fname.endswith(".synctmp"):
                        continue
                    fpath = dirpath / fname
                    if not is_excluded(fpath, root):
                        rel = fpath.relative_to(root).as_posix()
                        if rel not in records:
                            try:
                                records[rel] = file_md5(fpath)
                            except Exception:
                                records[rel] = ""
        lines = [f"{md5}  {rel}" for rel, md5 in sorted(records.items())]
        SNAPSHOT_FILE.write_text("\n".join(lines), encoding="utf-8")
        log.info(f"[快照] 已保存 {len(records)} 条记录（含 MD5）")
    except Exception as e:
        log.error(f"[快照] 保存失败: {e}")


# ── 同步逻辑 ───────────────────────────────────────────

def collect_files(root: Path) -> dict[str, Path]:
    """收集 root 下所有非排除文件，返回 {相对路径(posix): 绝对Path} 字典。"""
    result: dict[str, Path] = {}
    if not root.exists():
        return result
    for dirpath, dirs, files in os.walk(root):
        dirpath = Path(dirpath)
        dirs[:] = [d for d in dirs if not is_excluded(dirpath / d, root)]
        for fname in files:
            if fname.endswith(".synctmp"):
                continue          # 跳过上次中断留下的临时文件
            fpath = dirpath / fname
            if not is_excluded(fpath, root):
                result[fpath.relative_to(root).as_posix()] = fpath
    return result


def copy_newer(rel: str, usb_f: Path, pc_f: Path, snap_md5: str = ""):
    """将较新的一侧覆盖到较旧的一侧。

    有快照 MD5 时（snap_md5 非空）：
      - 先比较两侧当前 MD5 与快照 MD5，精确判断哪侧发生了变化。
      - 两侧都变化时（冲突）：以 mtime 较新的一侧为准。
      - 两侧都未变化：内容一致，跳过。

    无快照 MD5 时（首次运行或旧格式快照）：
      - 先用 mtime（3 秒阈值）判断，mtime 相近再用 MD5 比对。
    """
    try:
        if snap_md5:
            usb_md5 = file_md5(usb_f)
            pc_md5  = file_md5(pc_f)
            usb_changed = usb_md5 != snap_md5
            pc_changed  = pc_md5  != snap_md5
            if usb_changed and not pc_changed:
                safe_copy(usb_f, pc_f, known_src_md5=usb_md5)
            elif pc_changed and not usb_changed:
                safe_copy(pc_f, usb_f, known_src_md5=pc_md5)
            elif usb_changed and pc_changed:
                # 两侧都修改：以 mtime 较新的为准
                usb_mtime = usb_f.stat().st_mtime
                pc_mtime  = pc_f.stat().st_mtime
                if usb_mtime >= pc_mtime:
                    log.info(f"[冲突] 两侧均已修改，以G盘(mtime较新)为准: {rel}")
                    safe_copy(usb_f, pc_f, known_src_md5=usb_md5)
                else:
                    log.info(f"[冲突] 两侧均已修改，以E盘(mtime较新)为准: {rel}")
                    safe_copy(pc_f, usb_f, known_src_md5=pc_md5)
            else:
                # 两侧均未变化
                stats.skipped += 1
        else:
            # 无快照 MD5：回退到 mtime 比较
            usb_mtime = usb_f.stat().st_mtime
            pc_mtime  = pc_f.stat().st_mtime
            if usb_mtime > pc_mtime + 3:
                safe_copy(usb_f, pc_f)
            elif pc_mtime > usb_mtime + 3:
                safe_copy(pc_f, usb_f)
            else:
                # mtime 相近：用 MD5 判断内容是否真的一致
                if file_md5(usb_f) != file_md5(pc_f):
                    log.info(f"[MD5不同] mtime 相近但内容有差异，以E盘为准: {rel}")
                    safe_copy(pc_f, usb_f)
                else:
                    stats.skipped += 1
    except Exception as e:
        log.error(f"[同步错误] {rel}: {e}")
        stats.errors += 1


def sync(snapshot: dict[str, str] | None):
    """
    执行一次完整的双向同步。

    有快照时：精确区分"删除"与"新增"，正确传播删除操作。
    无快照时（首次运行）：以较新文件为准，不删除任何文件（安全模式）。

    快照中有 & G盘无 & E盘有  →  G盘删了  →  删除E盘对应文件
    快照中有 & G盘有 & E盘无  →  E盘删了  →  删除G盘对应文件
    快照中有 & 两侧都有       →  用快照MD5判断哪侧变化（无MD5则回退到mtime）
    快照中有 & 两侧都无       →  无操作
    快照中无 & 仅G盘有        →  新增     →  复制到E盘
    快照中无 & 仅E盘有        →  新增     →  复制到G盘
    快照中无 & 两侧都有       →  复制较新的一侧（存量文件）
    """
    usb_files = collect_files(DIR_USB)
    pc_files  = collect_files(DIR_PC)

    if snapshot is None:
        # 首次运行：只补齐缺失文件，不删除任何东西
        log.info("[同步] 首次运行，以较新文件为准（不删除多余文件）")
        all_rels = set(usb_files) | set(pc_files)
        for rel in sorted(all_rels):
            in_usb = rel in usb_files
            in_pc  = rel in pc_files
            if in_usb and not in_pc:
                safe_copy(usb_files[rel], DIR_PC / rel)
            elif in_pc and not in_usb:
                safe_copy(pc_files[rel], DIR_USB / rel)
            else:
                copy_newer(rel, usb_files[rel], pc_files[rel])
        return

    # 有快照：精确同步
    all_rels = set(usb_files) | set(pc_files) | set(snapshot)
    for rel in sorted(all_rels):
        in_usb      = rel in usb_files
        in_pc       = rel in pc_files
        in_snapshot = rel in snapshot

        if in_snapshot:
            if not in_usb and not in_pc:
                pass  # 两侧均已删除
            elif not in_usb and in_pc:
                log.info(f"[同步] G盘已删除 → 同步删除E盘: {rel}")
                safe_delete(pc_files[rel])
            elif in_usb and not in_pc:
                log.info(f"[同步] E盘已删除 → 同步删除G盘: {rel}")
                safe_delete(usb_files[rel])
            else:
                copy_newer(rel, usb_files[rel], pc_files[rel], snap_md5=snapshot[rel])
        else:
            if in_usb and not in_pc:
                safe_copy(usb_files[rel], DIR_PC / rel)
            elif in_pc and not in_usb:
                safe_copy(pc_files[rel], DIR_USB / rel)
            else:
                copy_newer(rel, usb_files[rel], pc_files[rel])


# ── 主程序 ─────────────────────────────────────────────

def print_summary():
    """在终端输出分栏汇总表（不写入日志文件）。"""
    action_rows: list[tuple[str, list[str]]] = [
        ("复制  G盘 → E盘", stats.usb_to_pc),
        ("复制  E盘 → G盘", stats.pc_to_usb),
        ("删除  E盘文件",   stats.del_pc),
        ("删除  G盘文件",   stats.del_usb),
    ]
    summary_rows: list[tuple[str, int]] = [
        ("无变化（跳过）", stats.skipped),
        ("错误",           stats.errors),
    ]
    total_ops = sum(len(files) for _, files in action_rows)

    all_labels = [r[0] for r in action_rows] + [r[0] for r in summary_rows]
    w_label = max(len(s) for s in all_labels) + 2
    w_count = 6

    sep  = "─" * (w_label + 1) + "┼" + "─" * (w_count + 2) + "┼" + "─" * 2
    line = "═" * (w_label + w_count + 7)

    print("\n" + line)
    print(f"{'  同步结果汇总':^{w_label + w_count + 6}}")
    print(line)
    print(f" {'操作类型':<{w_label}}│ {'数量':>{w_count}} │ 文件路径（绝对路径）")
    print(sep)
    for label, files in action_rows:
        count = len(files)
        first = files[0] if files else ""
        print(f" {label:<{w_label}}│ {count:>{w_count}} │ {first}")
        for f in files[1:]:
            print(f" {'':<{w_label}}│ {'':{w_count}} │ {f}")
    print(sep)
    for label, count in summary_rows:
        flag = "  !" if label.startswith("错误") and count else ""
        print(f" {label:<{w_label}}│ {count:>{w_count}} │{flag}")
    print(sep)
    print(f" {'实际操作合计':<{w_label}}│ {total_ops:>{w_count}} │")
    print(line + "\n")

def main():
    DIR_USB.mkdir(parents=True, exist_ok=True)
    DIR_PC.mkdir(parents=True, exist_ok=True)

    log.info("=" * 50)
    log.info("  网站文件夹双向同步工具")
    log.info(f"  USB : {DIR_USB}")
    log.info(f"  PC  : {DIR_PC}")
    log.info("=" * 50)

    snapshot = load_snapshot()
    sync(snapshot)
    save_snapshot()

    log.info("=" * 50)
    log.info("  同步完成")
    log.info("=" * 50)

    print_summary()


if __name__ == "__main__":
    main()
