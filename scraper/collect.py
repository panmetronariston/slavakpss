"""Скачивание текстов с Genius.

    export GENIUS_ACCESS_TOKEN=...   # https://genius.com/api-clients
    python -m scraper.collect --max-songs 150

Для каждого имени из config/artists.json сохраняется data/raw/<имя>.jsonl
(одна песня — одна строка). Уже скачанные песни пропускаются, так что
скрипт можно перезапускать после обрыва.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import re
import sys
import time
from pathlib import Path

from .config import ROOT, load_config, name_variants

log = logging.getLogger("collect")
RAW_DIR = ROOT / "data" / "raw"


def raw_path(genius_name: str) -> Path:
    safe = re.sub(r"[^\w-]+", "_", genius_name).strip("_")
    return RAW_DIR / f"{safe}.jsonl"


def song_record(song, queried_as: str) -> dict:
    body = song._body
    album = body.get("album") or {}
    return {
        "id": body.get("id"),
        "title": song.title,
        "url": song.url,
        "primary_artist": song.artist,
        "featured_artists": [a["name"] for a in song.featured_artists],
        "album": album.get("name"),
        "release_date": body.get("release_date")
        or body.get("release_date_for_display"),
        "queried_as": queried_as,
        "lyrics": song.lyrics,
    }


def collect_artist(genius, name: str, max_songs: int | None, force: bool) -> None:
    path = raw_path(name)
    done: set[int] = set()
    if path.exists() and not force:
        with path.open(encoding="utf-8") as f:
            done = {json.loads(line)["id"] for line in f if line.strip()}
        log.info("%s: уже есть %d песен", name, len(done))

    found = genius.search_artist(name, max_songs=0, include_features=True)
    if found is None:
        log.error("%s: артист не найден на Genius", name)
        return
    if not (name_variants(found.name) & name_variants(name)):
        log.warning("%s: Genius вернул другого артиста — %r, проверьте имя", name, found.name)

    path.parent.mkdir(parents=True, exist_ok=True)
    mode = "w" if force else "a"
    with path.open(mode, encoding="utf-8") as out:
        page, n = 1, len(done)
        while page and (max_songs is None or n < max_songs):
            res = genius.artist_songs(found.id, per_page=50, page=page, sort="popularity")
            for s in res["songs"]:
                if max_songs is not None and n >= max_songs:
                    break
                if s["id"] in done or s.get("lyrics_state") != "complete" or not genius._result_is_lyrics(s):
                    continue
                for attempt in range(3):
                    try:
                        song = genius.search_song(song_id=s["id"])
                        break
                    except Exception as e:  # сеть/429 — подождём
                        log.warning("%s: %s, повтор через %ds", s["title"], e, 5 * (attempt + 1))
                        time.sleep(5 * (attempt + 1))
                else:
                    continue
                if song is None or not song.lyrics:
                    continue
                out.write(json.dumps(song_record(song, name), ensure_ascii=False) + "\n")
                out.flush()
                done.add(s["id"])
                n += 1
                log.info("%s: [%d] %s", name, n, song.title)
            page = res.get("next_page")


def main(argv: list[str] | None = None) -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--max-songs", type=int, default=150, help="макс. песен на одно имя (0 — без ограничения)")
    ap.add_argument("--only", nargs="*", help="скачать только эти имена из конфига")
    ap.add_argument("--skip-reference", action="store_true", help="только Слава КПСС и альтер-эго")
    ap.add_argument("--force", action="store_true", help="перекачать заново")
    args = ap.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(message)s", datefmt="%H:%M:%S")

    token = os.environ.get("GENIUS_ACCESS_TOKEN")
    if not token:
        sys.exit("Укажите токен: export GENIUS_ACCESS_TOKEN=... (https://genius.com/api-clients)")

    from lyricsgenius import Genius

    genius = Genius(token, timeout=20, retries=3, sleep_time=0.5,
                    remove_section_headers=False, skip_non_songs=True)

    target, refs = load_config()
    names = list(target.genius_names)
    if not args.skip_reference:
        names += [n for r in refs for n in r.genius_names]
    if args.only:
        names = [n for n in names if n in args.only]
    for name in names:
        collect_artist(genius, name, args.max_songs or None, args.force)


if __name__ == "__main__":
    main()
