import json

from scraper import build_corpus


def test_build_corpus(tmp_path, monkeypatch):
    raw = tmp_path / "raw"
    raw.mkdir()
    verse = " ".join(f"слово{i}" for i in range(80))
    songs = [
        {"id": 1, "title": "Песня один", "primary_artist": "Слава КПСС (Slava KPSS)",
         "featured_artists": [], "album": "Альбом", "release_date": "2017-01-01",
         "url": "u1", "lyrics": f"[Куплет 1]\n{verse}\n[Припев]\nприпев"},
        {"id": 2, "title": "Совместная", "primary_artist": "Замай",
         "featured_artists": ["Гнойный"], "album": None, "release_date": "2018-01-01",
         "url": "u2", "lyrics": f"[Куплет 1: Замай]\n{verse} замай\n[Куплет 2: Гнойный]\n{verse} гнойный"},
        {"id": 3, "title": "Интервью Славы", "primary_artist": "Слава КПСС",
         "featured_artists": [], "album": None, "release_date": None, "url": "u3", "lyrics": verse},
    ]
    (raw / "x.jsonl").write_text("\n".join(json.dumps(s, ensure_ascii=False) for s in songs), encoding="utf-8")
    monkeypatch.setattr(build_corpus, "RAW_DIR", raw)
    monkeypatch.setattr(build_corpus, "CORPUS_DIR", tmp_path / "corpus")

    build_corpus.main(["--chunk-words", "80"])

    files = sorted(p.name for p in (tmp_path / "corpus" / "songs").iterdir())
    assert files == ["SlavaKPSS_pesnya-odin.txt", "SlavaKPSS_sovmestnaya.txt", "Zamay_sovmestnaya.txt"]
    personas = sorted(p.name.split("_")[0] for p in (tmp_path / "corpus" / "personas").iterdir())
    assert personas == ["Gnoyny", "SlavaKPSS"]
    chunks = sorted(p.name for p in (tmp_path / "corpus" / "chunks").iterdir())
    assert chunks == ["SlavaKPSS_chunk01.txt", "SlavaKPSS_chunk02.txt", "Zamay_chunk01.txt"]
    slava = (tmp_path / "corpus" / "songs" / "SlavaKPSS_sovmestnaya.txt").read_text(encoding="utf-8")
    assert "гнойный" in slava and "замай" not in slava
