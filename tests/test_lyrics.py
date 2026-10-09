from scraper.config import load_config
from scraper.lyrics import clean_text, extract_parts, parse_performers, strip_genius_junk

TARGET, REFS = load_config()

DUET = """12 ContributorsТестовая песня Lyrics[Интро]
общий текст интро

[Куплет 1: Слава КПСС]
первая строка славы
вторая строка славы (эдлиб)

[Припев: Слава КПСС & Замай]
совместный припев

[Куплет 2: Замай]
строка замая

[Куплет 3: Гнойный (Замай)]
строка гнойного

[Припев: Слава КПСС & Замай]
совместный припев
You might also like
[Аутро]
неизвестно кто
5Embed"""

SOLO = """[Куплет 1]
сольный куплет ёлка

[Припев]
припев повторяется

[Куплет 2]
второй куплет

[Припев]
припев повторяется"""


def test_parse_performers():
    assert parse_performers("Куплет 1: Слава КПСС & Замай") == ["Слава КПСС", "Замай"]
    assert parse_performers("Припев: Слава КПСС (Slava KPSS), Замай и Хаски") == ["Слава КПСС", "Замай", "Хаски"]
    assert parse_performers("Куплет 2: Соня Мармеладова feat. ATL") == ["Соня Мармеладова", "ATL"]
    assert parse_performers("Припев") == ["Припев"]


def test_duet_solo_mode():
    ex = extract_parts(DUET, TARGET, "Замай", ["Слава КПСС"], mode="solo")
    assert "первая строка славы" in ex.text
    assert "строка гнойного" in ex.text
    assert "эдлиб" not in ex.text
    assert "совместный припев" not in ex.text
    assert "замая" not in ex.text
    assert "интро" not in ex.text and "неизвестно" not in ex.text
    assert "Embed" not in ex.text and "might" not in ex.text
    assert set(ex.personas) == {"SlavaKPSS", "Gnoyny"}


def test_duet_any_mode_dedupes_chorus():
    ex = extract_parts(DUET, TARGET, "Замай", ["Слава КПСС"], mode="any")
    assert ex.text.count("совместный припев") == 1


def test_reference_artist_gets_own_parts():
    zamay = next(r for r in REFS if r.label == "Zamay")
    ex = extract_parts(DUET, zamay, "Замай", ["Слава КПСС"])
    assert ex.text == "строка замая"


def test_solo_song_unnamed_sections():
    ex = extract_parts(SOLO, TARGET, "Соня Мармеладова (Sonya Marmeladova)", [])
    assert ex.text.count("припев повторяется") == 1
    assert "елка" in ex.text  # ё -> е
    assert ex.personas == ["SonyaMarmeladova"]
    assert ex.n_sections_kept == 3


def test_unnamed_sections_dropped_when_guests():
    ex = extract_parts(SOLO, TARGET, "Слава КПСС", ["Хаски"])
    assert ex.text == ""


def test_group_page_without_names_dropped():
    ex = extract_parts(SOLO, TARGET, "Антихайп", [])
    assert ex.text == ""


def test_clean_and_junk():
    assert strip_genius_junk("3 ContributorsПесня Lyrics[Куплет]\nтекст\n2Embed") == "[Куплет]\nтекст"
    assert clean_text("[x]\nА  (б) в\n\n") == "А в"
