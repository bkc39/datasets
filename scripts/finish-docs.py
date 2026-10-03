#!/usr/bin/env python3
"""Make Scribble's standalone dependency links direct and audit the local site.

Scribble's --redirect serializes a doc name and relative URL for Racket's
local-redirect service. For a standalone preview, link to that published manual
instead: no redirect JavaScript or installed documentation paths are needed.
The HTML structure, stylesheets, scripts, and fonts remain Scribble's own.
"""
import html
from html.parser import HTMLParser
from pathlib import Path
import re
import sys
from urllib.parse import parse_qs, quote, unquote, urlsplit

root = Path(sys.argv[1]).resolve()
redirect = "https://docs.racket-lang.org/local-redirect/index.html"


def direct_link(match):
    url = html.unescape(match[1])
    if not url.startswith(redirect + "?"):
        return match[0]
    query = parse_qs(urlsplit(url).query)
    if "doc" not in query or "rel" not in query:
        raise ValueError(f"Unrecognized Scribble redirect: {url}")
    target = "https://docs.racket-lang.org/" + quote(query["doc"][0], safe="") + "/" + query["rel"][0]
    return 'href="' + html.escape(target, quote=True) + '"'


class Links(HTMLParser):
    def __init__(self, source):
        super().__init__()
        self.anchors = set()
        self.links = []
        self.feed(source)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        for key in ("id", "name"):
            if key in attrs:
                self.anchors.add(attrs[key])
        for key in ("href", "src"):
            if key in attrs:
                self.links.append(attrs[key])


pages = {}
for path in root.rglob("*.html"):
    source = re.sub(r'href="([^"]*)"', direct_link, path.read_text())
    path.write_text(source)
    pages[path] = Links(source)

checked = 0
for path, page in pages.items():
    for link in page.links:
        url = urlsplit(link)
        if url.scheme or url.netloc:
            continue
        target = (path.parent / unquote(url.path)).resolve() if url.path else path
        if not target.is_relative_to(root) or not target.exists():
            raise ValueError(f"Broken local link in {path.name}: {link}")
        if url.fragment and target in pages and unquote(url.fragment) not in pages[target].anchors:
            raise ValueError(f"Missing anchor in {path.name}: {link}")
        checked += 1
print(f"Checked {checked} local links and assets across {len(pages)} pages.")
