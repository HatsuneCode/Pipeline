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
import logging
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
# ──────────────────────────────────────────────────────

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


# ── 排除判断 ───────────────────────────────────────────

def is_excluded(path: Path, root: Path) -> bool:
    """检查路径（或其父目录）是否命中排除名单。大小写不敏感。"""
    try:
        rel = path.relative_to(root)
    except ValueError:
        return False

    rel_parts = tuple(p.lower() for p in rel.parts)
    rel_str   = rel.as_posix().lower()

    for raw in EXCLUDE_PATTERNS:
        pat       = Path(raw)
        pat_parts = tuple(p.lower() for p in pat.parts)
        pat_str   = pat.as_posix().lower()

        if rel_parts[:len(pat_parts)] == pat_parts:
            return True
        if fnmatch.fnmatch(rel_str, pat_str):
            return True
        if "/" not in pat_str and fnmatch.fnmatch(rel_parts[-1], pat_str):
            return True

    return False


# ── 文件操作 ───────────────────────────────────────────

def safe_copy(src: Path, dst: Path):
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dst)
        log.info(f"[复制] {src}  →  {dst}")
    except Exception as e:
        log.error(f"[复制失败] {src} → {dst}  ({e})")


def safe_delete(dst: Path):
    try:
        if dst.is_dir():
            shutil.rmtree(dst)
            log.info(f"[删目录] {dst}")
        elif dst.exists():
            dst.unlink()
            log.info(f"[删文件] {dst}")
    except Exception as e:
        log.error(f"[删除失败] {dst}  ({e})")


# ── 快照 ───────────────────────────────────────────────

def load_snapshot() -> set[str] | None:
    """加载上次保存的文件列表快照；快照不存在（首次运行）时返回 None。"""
    if not SNAPSHOT_FILE.exists():
        return None
    try:
        lines = SNAPSHOT_FILE.read_text(encoding="utf-8").splitlines()
        result = {l.strip() for l in lines if l.strip()}
        log.info(f"[快照] 已加载 {len(result)} 条记录")
        return result
    except Exception as e:
        log.warning(f"[快照] 读取失败，将按首次运行处理: {e}")
        return None


def save_snapshot():
    """将当前两侧文件列表保存为快照。"""
    try:
        all_files: set[str] = set()
        for root in (DIR_USB, DIR_PC):
            if root.exists():
                for dirpath, dirs, files in os.walk(root):
                    dirpath = Path(dirpath)
                    dirs[:] = [d for d in dirs if not is_excluded(dirpath / d, root)]
                    for fname in files:
                        fpath = dirpath / fname
                        if not is_excluded(fpath, root):
                            all_files.add(fpath.relative_to(root).as_posix())
        SNAPSHOT_FILE.write_text("\n".join(sorted(all_files)), encoding="utf-8")
        log.info(f"[快照] 已保存 {len(all_files)} 条记录")
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
        # 在进入子目录前剪掉排除目录，避免遍历其内部文件
        dirs[:] = [d for d in dirs if not is_excluded(dirpath / d, root)]
        for fname in files:
            fpath = dirpath / fname
            if not is_excluded(fpath, root):
                result[fpath.relative_to(root).as_posix()] = fpath
    return result


def copy_newer(rel: str, usb_f: Path, pc_f: Path):
    """将较新的一侧覆盖到较旧的一侧（时间差 ≤1s 视为相同，跳过）。"""
    try:
        if usb_f.stat().st_mtime > pc_f.stat().st_mtime + 1:
            safe_copy(usb_f, pc_f)
        elif pc_f.stat().st_mtime > usb_f.stat().st_mtime + 1:
            safe_copy(pc_f, usb_f)
    except Exception as e:
        log.error(f"[同步错误] {rel}: {e}")


def sync(snapshot: set[str] | None):
    """
    执行一次完整的双向同步。

    有快照时：精确区分"删除"与"新增"，正确传播删除操作。
    无快照时（首次运行）：以较新文件为准，不删除任何文件（安全模式）。

    快照中有 & G盘无 & E盘有  →  G盘删了  →  删除E盘对应文件
    快照中有 & G盘有 & E盘无  →  E盘删了  →  删除G盘对应文件
    快照中有 & 两侧都有       →  复制较新的一侧
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
    all_rels = set(usb_files) | set(pc_files) | snapshot
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
                copy_newer(rel, usb_files[rel], pc_files[rel])
        else:
            if in_usb and not in_pc:
                safe_copy(usb_files[rel], DIR_PC / rel)
            elif in_pc and not in_usb:
                safe_copy(pc_files[rel], DIR_USB / rel)
            else:
                copy_newer(rel, usb_files[rel], pc_files[rel])


# ── 主程序 ─────────────────────────────────────────────

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


if __name__ == "__main__":
    main()
