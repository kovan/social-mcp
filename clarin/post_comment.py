#!/usr/bin/env python3
"""Post a comment on a Clarin article using Playwright with Chrome's cookies."""
import sys
import json
import time
from playwright.sync_api import sync_playwright
import browser_cookie3


def post_comment(article_url, body, parent_id=None):
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True, channel="chrome")

        # Inject Chrome cookies
        cookies = []
        for domain in ["clarin.com", "viafoura.co"]:
            cj = browser_cookie3.chrome(domain_name=domain)
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

        # Remove consent overlays
        time.sleep(2)
        page.evaluate(
            """() => {
            document.querySelectorAll('.fc-consent-root, .fc-dialog-overlay, [class*="consent"], [class*="cookie-banner"]')
                .forEach(el => el.remove());
        }"""
        )

        # Scroll to load comments
        page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        time.sleep(8)

        # Force comment form visible
        page.evaluate(
            """document.querySelectorAll(".vf-new-content")
               .forEach(el => el.style.setProperty("display", "block", "important"))"""
        )
        time.sleep(1)

        # Fill and submit
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

        print(json.dumps({"success": True, "message": "Comment posted"}))
        browser.close()


if __name__ == "__main__":
    try:
        data = json.loads(sys.argv[1])
        post_comment(data["url"], data["body"], data.get("parent_id"))
    except Exception as e:
        print(json.dumps({"success": False, "error": str(e)}))
        sys.exit(1)
