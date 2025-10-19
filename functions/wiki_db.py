import pandas as pd
import numpy as np
import wikipediaapi
import sqlite3
import os
import requests
from IPython.display import display
import time
import random
from requests.exceptions import RequestException
from json import JSONDecodeError


def get_pageviews(title, lang="en", year=2025, month=9):
    S = requests.Session()
    S.headers.update({"User-Agent": "StudentProject/1.0"})

    title_api = title.replace(" ", "_")
    start = f"{year}{month:02d}01"
    end = f"{year}{month:02d}30"

    URL = f"https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/{lang}.wikipedia/all-access/all-agents/{title_api}/daily/{start}/{end}"

    try:
        resp = S.get(URL, timeout=10)
        if resp.status_code != 200:
            return 0
        data = resp.json()
        total_views = sum(item.get("views", 0) for item in data.get("items", []))
        return total_views
    except:
        return 0


def get_article_metadata(title, lang="en", delay=0.1, max_retries=3):
    S = requests.Session()
    S.headers.update({"User-Agent": "StudentProject/1.0 (estera.maria03@gmail.com)"})
    URL = f"https://{lang}.wikipedia.org/w/api.php"

    PARAMS = {
        "action": "query",
        "titles": title,
        "prop": "revisions|links|categories|images|extracts",
        "rvprop": "user|timestamp",  # dodajemy timestamp rewizji
        "rvlimit": "max",
        "rvdir": "newer",  # pobierzemy najstarszą rewizję jako pierwszą
        "explaintext": 1,
        "format": "json",
        "pllimit": "max",
        "cllimit": "max",
        "ilimit": "max"
    }

    backoff = 0.5
    for attempt in range(1, max_retries + 1):
        try:
            resp = S.get(URL, params=PARAMS, timeout=10)
            if resp.status_code != 200:
                time.sleep(delay + backoff)
                backoff *= 2
                continue
            try:
                data = resp.json()
            except (JSONDecodeError, ValueError):
                time.sleep(delay + backoff)
                backoff *= 2
                continue

            pages = data.get("query", {}).get("pages", {})
            for _, page_data in pages.items():
                extract = page_data.get("extract", "")
                word_count = len(extract.split())

                links = page_data.get("links", [])
                num_links_internal = len(links)

                categories = [c["title"] for c in page_data.get("categories", [])]
                num_categories = len(categories)

                images = [i["title"] for i in page_data.get("images", [])]
                num_images = len(images)

                revisions = page_data.get("revisions", [])
                num_edits = len(revisions)
                editors = set(rev.get("user") for rev in revisions)
                num_editors = len(editors)

                creation_date = revisions[0]["timestamp"] if revisions else None
                page_views = get_pageviews(page_data.get("title"))

                return {
                    "title": page_data.get("title"),
                    "word_count": word_count,
                    "num_links_internal": num_links_internal,
                    "num_categories": num_categories,
                    "categories": categories,
                    "num_images": num_images,
                    "image_titles": images,
                    "num_edits": num_edits,
                    "num_editors": num_editors,
                    "summary": extract,
                    "creation_date": creation_date,
                    "mo_page_views": page_views,
                }
            return None
        except RequestException:
            time.sleep(delay + backoff)
            backoff *= 2
            continue
    return None


def get_articles_from_category(category_name, depth=0):
    wiki = wikipediaapi.Wikipedia(language="en", user_agent="StudentProject/1.0")
    if not category_name.startswith("Category:"):
        category_name = "Category:" + category_name
    category = wiki.page(category_name)
    if not category.exists():
        print(f"Category '{category_name}' not found.")
        return pd.DataFrame()

    articles_data = []

    def add_articles(cat, current_depth=0):
        for page in cat.categorymembers.values():
            if page.ns == 0:
                try:
                    meta = get_article_metadata(page.title)
                    if meta:
                        articles_data.append(meta)
                    time.sleep(random.uniform(0, 0.1))
                except Exception as e:
                    print(f"Błąd przy pobieraniu {page.title}: {e}")
            elif page.ns == 14 and current_depth < depth:
                add_articles(page, current_depth + 1)

    add_articles(category)
    df = pd.DataFrame(articles_data)
    return df