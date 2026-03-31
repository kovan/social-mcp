#!/usr/bin/env python3
"""Fetch paginated answers from Pikabu via headless Chromium.
Usage: answers_paginate.py <exclude_ids> <base_id> <cookie_file>
Outputs JSON to stdout.
"""
import sys, json, os

os.environ.setdefault("TMPDIR", os.path.expanduser("~/.playwright-tmp"))
os.makedirs(os.environ["TMPDIR"], exist_ok=True)

def main():
    exclude_ids = sys.argv[1] if len(sys.argv) > 1 else ""
    base_id = sys.argv[2] if len(sys.argv) > 2 else "0"
    cookie_file = sys.argv[3] if len(sys.argv) > 3 else ""

    # Load Netscape cookies and convert to Playwright format
    cookies = []
    if cookie_file:
        with open(cookie_file) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                parts = line.split("\t")
                if len(parts) >= 7:
                    cookies.append({
                        "name": parts[5],
                        "value": parts[6],
                        "domain": parts[0],
                        "path": parts[2],
                        "secure": parts[3] == "TRUE",
                        "httpOnly": False
                    })

    from playwright.sync_api import sync_playwright

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context(
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36"
        )

        # Add cookies
        if cookies:
            # Playwright needs url for cookie domain
            pw_cookies = []
            for c in cookies:
                domain = c["domain"].lstrip(".")
                pw_cookies.append({
                    "name": c["name"],
                    "value": c["value"],
                    "domain": c["domain"],
                    "path": c["path"],
                    "secure": c["secure"],
                    "httpOnly": c["httpOnly"],
                    "url": f"https://{domain}/"
                })
            try:
                context.add_cookies(pw_cookies)
            except Exception:
                pass

        page = context.new_page()

        # Visit answers page to establish DDoS-Guard session
        page.goto("https://pikabu.ru/answers", wait_until="networkidle", timeout=20000)

        # Extract CSRF + make pagination POST from browser context
        result = page.evaluate("""
            async ([excludeIds, baseId]) => {
                const el = document.querySelector('script[data-entry="initParams"]');
                const csrf = el ? JSON.parse(el.textContent).csrfToken : '';

                const resp = await fetch('/answers', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
                        'X-Requested-With': 'XMLHttpRequest',
                        'X-Csrf-Token': csrf
                    },
                    body: 'base_id=' + baseId + '&exclude_ids=' + excludeIds
                });
                return await resp.json();
            }
        """, [exclude_ids, base_id])

        browser.close()

    print(json.dumps(result))

if __name__ == "__main__":
    main()
