"""Сборка корпуса для stylo из data/raw/*.jsonl.

    python -m scraper.build_corpus --mode solo --chunk-words 1500

Результат (имена файлов в формате stylo: <Класс>_<название>.txt):

    corpus/songs/     — по файлу на песню: SlavaKPSS_..., Oxxxymiron_...
    corpus/chunks/    — песни автора, склеенные в куски ~N слов
                        (одна песня слишком коротка для надёжной стилометрии)
    corpus/personas/  — только Слава, класс = альтер-эго (Gnoyny_..., SonyaMarmeladova_...)
    corpus/metadata.csv
"""

from __future__ import annotations

import argparse
import csv
import json
import logging
import re
import shutil
from collections import defaultdict
from pathlib import Path

from .collect import RAW_DIR
from .config import ROOT, Artist, load_config
from .lyrics import extract_parts

log = logging.getLogger("build_corpus")
CORPUS_DIR = ROOT / "corpus"
WORD_RE = re.compile(r"[^\W\d_]+(?:-[^\W\d_]+)*")

# Не песни: интервью, трек-листы, переводы, сниппеты и т.п.
SKIP_TITLE_RE = re.compile(
    r"tracklist|трек-?лист|интервью|interview|перевод|translation|snippet|сниппет"
    r"|\bremix\b|ремикс|\blive\b|концерт|annotated|liner notes",
    re.IGNORECASE,
)
# Батлы (Versus, SlovoSPB, Рвать на битах): там лексика сильно отличается — помечаем
BATTLE_RE = re.compile(r"versus|слово ?spb|slovo|баттл|батл|battle|рвать на битах|раунд", re.IGNORECASE)


_TRANSLIT = dict(zip(
    "абвгдеёжзийклмнопрстуфхцчшщъыьэюя",
    ["a", "b", "v", "g", "d", "e", "e", "zh", "z", "i", "y", "k", "l", "m", "n", "o", "p", "r",
     "s", "t", "u", "f", "h", "ts", "ch", "sh", "sch", "", "y", "", "e", "yu", "ya"],
))


def slug(s: str, maxlen: int = 60) -> str:
    """Латинское имя файла (кириллица в путях ломает stylo под Windows)."""
    s = "".join(_TRANSLIT.get(c, c) for c in s.lower())
    s = re.sub(r"[^a-z0-9]+", "-", s).strip("-")
    return s[:maxlen].strip("-") or "untitled"


def word_count(text: str) -> int:
    return len(WORD_RE.findall(text))


def load_raw() -> list[dict]:
    songs: dict[int, dict] = {}
    for path in sorted(RAW_DIR.glob("*.jsonl")):
        with path.open(encoding="utf-8") as f:
            for line in f:
                if line.strip():
                    rec = json.loads(line)
                    songs.setdefault(rec["id"], rec)  # одна песня может прийти от разных имён
    return list(songs.values())


def assign(rec: dict, artists: list[Artist], **kw) -> list[tuple[Artist, object]]:
    """Возвращает [(исполнитель, Extracted)] для всех исполнителей из конфига, чьи партии есть в песне."""
    out = []
    for a in artists:
        ex = extract_parts(rec["lyrics"], a, rec["primary_artist"], rec["featured_artists"], **kw)
        if ex.text:
            out.append((a, ex))
    return out


def chunk(texts: list[str], size: int) -> list[str]:
    """Склеивает песни подряд в куски не меньше size слов (хвост приклеивается к последнему)."""
    chunks, cur, n = [], [], 0
    for t in texts:
        cur.append(t)
        n += word_count(t)
        if n >= size:
            chunks.append("\n\n".join(cur))
            cur, n = [], 0
    if cur:
        if chunks and n < size / 2:
            chunks[-1] += "\n\n" + "\n\n".join(cur)
        else:
            chunks.append("\n\n".join(cur))
    return chunks


def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--mode", choices=["solo", "any"], default="solo",
                    help="solo — только сольные секции; any — и совместные")
    ap.add_argument("--keep-duplicates", action="store_true", help="не убирать повторы припевов")
    ap.add_argument("--keep-parentheses", action="store_true", help="не удалять текст в скобках (эдлибы)")
    ap.add_argument("--include-battles", action="store_true", help="включать баттл-тексты")
    ap.add_argument("--min-words", type=int, default=60, help="минимум слов в партии, чтобы взять песню")
    ap.add_argument("--chunk-words", type=int, default=1500, help="размер куска для corpus/chunks")
    args = ap.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(message)s")

    target, refs = load_config()
    artists = [target, *refs]
    kw = dict(mode=args.mode, dedupe=not args.keep_duplicates,
              drop_parentheses=not args.keep_parentheses)

    records = load_raw()
    if not records:
        raise SystemExit(f"Нет данных в {RAW_DIR}. Сначала: python -m scraper.collect")

    if CORPUS_DIR.exists():
        shutil.rmtree(CORPUS_DIR)
    for sub in ("songs", "chunks", "personas"):
        (CORPUS_DIR / sub).mkdir(parents=True)

    meta_rows = []
    by_author: dict[str, list[tuple[str, str]]] = defaultdict(list)  # label -> [(дата, текст)]
    used_names: set[str] = set()
    skipped = defaultdict(int)

    for rec in sorted(records, key=lambda r: (r.get("release_date") or "9999", r["title"])):
        title = rec["title"]
        if SKIP_TITLE_RE.search(title) or SKIP_TITLE_RE.search(rec.get("album") or ""):
            skipped["не песня"] += 1
            continue
        battle = bool(BATTLE_RE.search(title) or BATTLE_RE.search(rec.get("album") or ""))
        if battle and not args.include_battles:
            skipped["баттл"] += 1
            continue
        for artist, ex in assign(rec, artists, **kw):
            n = word_count(ex.text)
            if n < args.min_words:
                skipped["мало слов"] += 1
                continue
            base = f"{artist.label}_{slug(title)}"
            name, i = base, 2
            while name in used_names:
                name, i = f"{base}-{i}", i + 1
            used_names.add(name)
            (CORPUS_DIR / "songs" / f"{name}.txt").write_text(ex.text, encoding="utf-8")
            by_author[artist.label].append((rec.get("release_date") or "", ex.text))

            persona = ex.personas[0] if ex.personas else artist.label
            if artist is target:
                pname = f"{persona}_{slug(title)}-{rec['id']}"
                (CORPUS_DIR / "personas" / f"{pname}.txt").write_text(ex.text, encoding="utf-8")

            meta_rows.append({
                "file": f"{name}.txt", "author": artist.label, "persona": persona,
                "title": title, "primary_artist": rec["primary_artist"],
                "featured": "; ".join(rec["featured_artists"]), "album": rec.get("album") or "",
                "release_date": rec.get("release_date") or "", "words": n,
                "sections_kept": ex.n_sections_kept, "sections_total": ex.n_sections_total,
                "url": rec.get("url") or "",
            })

    for label, items in by_author.items():
        for i, text in enumerate(chunk([t for _, t in items], args.chunk_words), 1):
            (CORPUS_DIR / "chunks" / f"{label}_chunk{i:02d}.txt").write_text(text, encoding="utf-8")

    with (CORPUS_DIR / "metadata.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(meta_rows[0]) if meta_rows else ["file"])
        w.writeheader()
        w.writerows(meta_rows)

    log.info("Песен в корпусе: %d; пропущено: %s", len(meta_rows), dict(skipped))
    for label in sorted(by_author):
        words = sum(word_count(t) for _, t in by_author[label])
        log.info("  %-18s песен: %4d  слов: %7d", label, len(by_author[label]), words)
    log.info("Готово: %s", CORPUS_DIR)


if __name__ == "__main__":
    main()
