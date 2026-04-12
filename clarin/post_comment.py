#!/usr/bin/env python3
"""Post a comment on a Clarin article using Playwright with Chrome's cookies."""
import json
import re
import sys
import time

import browser_cookie3
import requests
from playwright.sync_api import sync_playwright


SECTION_UUID = "00000000-0000-4000-8000-82628f44cd3d"


def _chrome_cookies(domain):
    orig_get_password = browser_cookie3._LinuxPasswordManager.get_password

    def patched_get_password(self, os_crypt_name):
        try:
            return orig_get_password(self, os_crypt_name)
        except Exception:
            return browser_cookie3.CHROMIUM_DEFAULT_PASSWORD

    browser_cookie3._LinuxPasswordManager.get_password = patched_get_password
    try:
        return browser_cookie3.chrome(domain_name=domain)
    finally:
        browser_cookie3._LinuxPasswordManager.get_password = orig_get_password


def _build_requests_session():
    session = requests.Session()
    for domain in ["clarin.com", "viafoura.co"]:
        for cookie in _chrome_cookies(domain):
            session.cookies.set_cookie(cookie)
    return session


def _extract_article_id(article_url):
    match = re.search(r"_\d+_([A-Za-z0-9]+)\.html$", article_url)
    if match:
        return match.group(1)
    return article_url.rstrip("/").rsplit("/", 1)[-1].replace(".html", "")


def _get_container_uuid(session, article_url):
    article_id = _extract_article_id(article_url)
    response = session.get(
        f"https://www.clarin.com/api/viafoura/comments/{article_id}", timeout=20
    )
    response.raise_for_status()
    payload = response.json()
    return payload["data"]["content_container_uuid"]


def _iter_visible_comments(session, container_uuid, limit=100):
    response = session.get(
        f"https://livecomments.viafoura.co/v4/livecomments/{SECTION_UUID}/{container_uuid}/comments",
        params={"limit": limit, "reply_limit": 5, "sorted_by": "newest"},
        timeout=20,
    )
    response.raise_for_status()
    payload = response.json()
    for comment in payload.get("contents", []):
        yield comment
        for reply in (comment.get("replies") or {}).get("contents", []):
            yield reply


def _comment_is_visible(session, article_url, body):
    container_uuid = _get_container_uuid(session, article_url)
    target = " ".join(body.split())
    for comment in _iter_visible_comments(session, container_uuid):
        content = " ".join((comment.get("content") or "").split())
        if content == target:
            return {
                "visible": True,
                "content_uuid": comment.get("content_uuid"),
                "container_uuid": container_uuid,
            }
    return {"visible": False, "container_uuid": container_uuid}


def post_comment(article_url, body, parent_id=None):
    http_session = _build_requests_session()
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True, channel="chrome")

        # Inject Chrome cookies for both clarin.com and viafoura.co
        cookies = []
        for domain in ["clarin.com", "viafoura.co"]:
            cj = _chrome_cookies(domain)
            for c in cj:
                cookie = {
                    "name": c.name,
                    "value": c.value,
                    "domain": c.domain,
                    "path": c.path or "/",
                }
                if c.secure:
                    cookie["secure"] = True
                if c.expires:
                    cookie["expires"] = int(c.expires)
                cookies.append(cookie)

        ctx = browser.new_context(viewport={"width": 1280, "height": 4000})
        ctx.add_cookies(cookies)
        page = ctx.new_page()
        page.goto(article_url, wait_until="domcontentloaded", timeout=30000)

        # Accept consent overlay
        time.sleep(3)
        try:
            page.locator("button.fc-cta-consent").first.click(timeout=5000)
        except Exception:
            pass
        time.sleep(2)

        # Remove any remaining consent overlays
        page.evaluate(
            """() => {
            document.querySelectorAll('.fc-consent-root, .fc-dialog-overlay')
                .forEach(el => el.remove());
        }"""
        )

        # Scroll to load comments widget
        page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        time.sleep(8)

        # Force comment form visible (it starts collapsed with display:none)
        page.evaluate(
            """document.querySelectorAll(".vf-new-content")
               .forEach(el => el.style.setProperty("display", "block", "important"))"""
        )
        time.sleep(1)

        # Fill textarea and submit (force bypasses any remaining overlay issues)
        textarea = page.locator(
            'textarea[data-testid="vf-conversations-new-comment-textarea"]'
        ).first
        textarea.click(force=True, timeout=5000)
        time.sleep(0.5)
        textarea.fill(body)
        time.sleep(1)

        pub_btn = page.locator('button:has-text("Publicar")').first
        pub_btn.click(force=True, timeout=5000)
        time.sleep(3)

        verification = _comment_is_visible(http_session, article_url, body)
        if verification["visible"]:
            print(
                json.dumps(
                    {
                        "success": True,
                        "visible": True,
                        "message": "Comment posted and visible",
                        "content_uuid": verification.get("content_uuid"),
                    }
                )
            )
        else:
            print(
                json.dumps(
                    {
                        "success": True,
                        "visible": False,
                        "message": "Comment submission completed but is not publicly visible yet",
                        "container_uuid": verification.get("container_uuid"),
                    }
                )
            )
        browser.close()


if __name__ == "__main__":
    try:
        data = json.loads(sys.argv[1])
        post_comment(data["url"], data["body"], data.get("parent_id"))
    except Exception as e:
        print(json.dumps({"success": False, "error": str(e)}))
        sys.exit(1)
