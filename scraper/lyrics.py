"""Очистка текстов Genius и выделение партий конкретного исполнителя.

На Genius текст размечен заголовками секций:

    [Куплет 1: Слава КПСС]
    [Припев: Слава КПСС & Замай]
    [Интро]

Секция приписывается исполнителю, если он указан в заголовке. Секции без
имени в заголовке приписываются «исполнителю по умолчанию» — основному
артисту песни, но только если в песне нет гостей (иначе непонятно, кто поёт).
"""

from __future__ import annotations

import re
from dataclasses import dataclass

from .config import Artist, normalize_name

HEADER_RE = re.compile(r"^\s*\[([^\]]*)\]\s*$")
# Разделители между исполнителями в заголовке секции
PERFORMER_SPLIT_RE = re.compile(
    r"\s*(?:&|,|\+|/|;|\bfeat\.?|\bft\.?|\bwith\b|\band\b|(?<!\w)(?:и|x|х|vs\.?)(?!\w))\s*",
    re.IGNORECASE,
)


@dataclass
class Section:
    header: str | None  # текст заголовка без скобок, None — текст до первого заголовка
    performers: list[str]  # исполнители из заголовка (пусто — не указаны)
    text: str


def strip_genius_junk(lyrics: str) -> str:
    """Убирает служебные строки, которые lyricsgenius иногда оставляет в тексте."""
    lines = lyrics.splitlines()
    # Первая строка вида '123 ContributorsНазвание Lyrics' / 'Название Lyrics'
    if lines and (re.match(r"^\s*\d*\s*Contributors?", lines[0]) or lines[0].rstrip().endswith("Lyrics")):
        lines[0] = re.sub(r"^.*?Lyrics", "", lines[0], count=1)
    text = "\n".join(lines)
    text = re.sub(r"\d*\s*Embed\s*$", "", text.rstrip())
    text = re.sub(r"You might also like", "", text)
    text = re.sub(r"See .{1,80} LiveGet tickets as low as \$\d+", "", text)
    return text.strip()


def parse_performers(header: str) -> list[str]:
    """'Куплет 1: Слава КПСС & Замай' -> ['Слава КПСС', 'Замай']."""
    if ":" in header:
        names = header.split(":", 1)[1]
    elif "—" in header or " - " in header:
        names = re.split(r"—| - ", header, maxsplit=1)[1]
    else:
        # Заголовок без двоеточия: '[Слава КПСС]' или '[Припев]'.
        names = header
    # Имена в скобках — обычно бэк-вокал ('Слава КПСС (Замай)') или латинское
    # написание ('Слава КПСС (Slava KPSS)'); основной исполнитель — вне скобок.
    without_parens = re.sub(r"\([^()]*\)", " ", names)
    if without_parens.strip():
        names = without_parens
    parts = [p.strip(" *_-—") for p in PERFORMER_SPLIT_RE.split(names)]
    return [p for p in parts if p]


def split_sections(lyrics: str) -> list[Section]:
    sections: list[Section] = []
    header: str | None = None
    buf: list[str] = []

    def flush() -> None:
        text = "\n".join(buf).strip()
        if text:
            performers = parse_performers(header) if header else []
            sections.append(Section(header, performers, text))

    for line in lyrics.splitlines():
        m = HEADER_RE.match(line)
        if m:
            flush()
            header, buf = m.group(1).strip(), []
        else:
            buf.append(line)
    flush()
    return sections


def clean_text(text: str, drop_parentheses: bool = True) -> str:
    """Нормализация текста для стилометрии (та же логика, что clean_lyrics() в R)."""
    text = re.sub(r"\[[^\]]*\]", " ", text)  # заголовки секций
    if drop_parentheses:
        # (бэк-вокал, эдлибы) — часто это другие голоса
        text = re.sub(r"\([^()]*\)", " ", text)
    text = text.replace("ё", "е").replace("Ё", "Е")
    lines = [re.sub(r"[ \t]+", " ", ln).strip() for ln in text.splitlines()]
    return "\n".join(ln for ln in lines if ln)


@dataclass
class Extracted:
    text: str
    personas: list[str]  # какие альтер-эго нашлись в песне
    n_sections_total: int
    n_sections_kept: int


def extract_parts(
    lyrics: str,
    artist: Artist,
    primary_artist: str,
    featured_artists: list[str],
    *,
    mode: str = "solo",
    dedupe: bool = True,
    drop_parentheses: bool = True,
) -> Extracted:
    """Оставляет только партии `artist`.

    mode='solo' — только секции, где исполнитель поёт один (чище для атрибуции);
    mode='any'  — также совместные секции ('Слава КПСС & Замай').
    dedupe — повторяющиеся секции (припевы) берутся один раз, чтобы не
    раздувать частоты.
    """
    primary_persona = artist.persona_of(primary_artist)
    others_on_song = [f for f in featured_artists if not artist.matches(f)]
    default_is_artist = primary_persona is not None and not others_on_song

    kept: list[str] = []
    seen: set[str] = set()
    personas: list[str] = []
    sections = split_sections(strip_genius_junk(lyrics))
    for sec in sections:
        if sec.performers:
            hits = [artist.persona_of(p) for p in sec.performers]
            matched = [h for h in hits if h]
            if not matched:
                # Заголовок без двоеточия может быть просто типом секции ('[Припев]')
                if ":" in (sec.header or "") or not default_is_artist:
                    continue
                if any(_looks_like_name(p) for p in sec.performers):
                    continue
                matched = [primary_persona]
            elif mode == "solo" and len(matched) != len(sec.performers):
                continue
            personas.extend(matched)
        elif not default_is_artist:
            continue
        else:
            personas.append(primary_persona)

        key = normalize_name(sec.text)
        if dedupe and key in seen:
            continue
        seen.add(key)
        kept.append(sec.text)

    text = clean_text("\n\n".join(kept), drop_parentheses=drop_parentheses)
    uniq = list(dict.fromkeys(p for p in personas if p))
    return Extracted(text, uniq, len(sections), len(kept))


SECTION_WORDS = {
    "куплет", "припев", "интро", "аутро", "бридж", "предприпев", "пре-припев",
    "переход", "вступление", "концовка", "хук", "проигрыш", "скит", "текст",
    "verse", "chorus", "hook", "intro", "outro", "bridge", "pre-chorus",
    "refrain", "interlude", "skit", "part", "часть", "инструментал", "instrumental",
}


def _looks_like_name(s: str) -> bool:
    """True, если строка из заголовка не похожа на тип секции ('Куплет 2', 'Hook')."""
    words = re.findall(r"[\w-]+", normalize_name(s))
    if not words:
        return False
    return not any(w in SECTION_WORDS or w.isdigit() for w in words)
