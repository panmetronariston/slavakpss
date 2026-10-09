"""Загрузка config/artists.json и сопоставление имён исполнителей."""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "artists.json"


def normalize_name(name: str) -> str:
    """Приводит имя к форме для сравнения: нижний регистр, ё→е, без лишних знаков."""
    name = name.lower().replace("ё", "е")
    name = re.sub(r"[​‎‏\xa0]", " ", name)
    name = re.sub(r"[*_\"'«»“”.]", "", name)
    return re.sub(r"\s+", " ", name).strip()


def name_variants(name: str) -> set[str]:
    """'Слава КПСС (Slava KPSS)' -> {'слава кпсс (slava kpss)', 'слава кпсс', 'slava kpss'}."""
    variants = {normalize_name(name)}
    m = re.match(r"^(.*?)\s*\((.*?)\)\s*$", name)
    if m:
        variants.add(normalize_name(m.group(1)))
        variants.add(normalize_name(m.group(2)))
    return {v for v in variants if v}


@dataclass
class Artist:
    label: str
    genius_names: list[str]
    # persona label -> aliases; у референсных авторов одна персона = label
    personas: dict[str, list[str]] = field(default_factory=dict)

    def __post_init__(self) -> None:
        self._lookup: dict[str, str] = {}
        for persona, aliases in self.personas.items():
            for alias in aliases:
                for v in name_variants(alias):
                    self._lookup[v] = persona

    def persona_of(self, name: str) -> str | None:
        """Возвращает персону, если имя относится к этому исполнителю, иначе None."""
        for v in name_variants(name):
            if v in self._lookup:
                return self._lookup[v]
        return None

    def matches(self, name: str) -> bool:
        return self.persona_of(name) is not None


def load_config(path: Path = CONFIG_PATH) -> tuple[Artist, list[Artist]]:
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    t = data["target"]
    target = Artist(t["label"], t["genius_names"], t["personas"])
    refs = [
        Artist(r["label"], r["genius_names"], {r["label"]: r.get("aliases", []) + r["genius_names"]})
        for r in data["reference"]
    ]
    for a in [target, *refs]:
        if "_" in a.label or " " in a.label:
            raise ValueError(f"label {a.label!r}: для stylo нельзя '_' и пробелы")
    return target, refs
