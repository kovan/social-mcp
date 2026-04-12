#!/usr/bin/env python3
"""Read Clarín/Viafoura notifications using Chrome cookies."""
import json
import sys

import browser_cookie3
import requests


SECTION_UUID = "00000000-0000-4000-8000-82628f44cd3d"
NOTIFICATIONS_URL = f"https://notifications.viafoura.co/v5/notifications/{SECTION_UUID}/all"
PROFILE_URL = (
    "https://livecomments.viafoura.co/v4/livecomments/"
    f"{SECTION_UUID}" "/{user_uuid}/profile"
)


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


def _decode_jwt_payload(token):
    try:
        payload = token.split(".")[1]
        padding = "=" * ((4 - len(payload) % 4) % 4)
        return json.loads(
            __import__("base64").urlsafe_b64decode((payload + padding).encode("utf-8"))
        )
    except Exception:
        return {}


def _build_session():
    session = requests.Session()
    for domain in ["clarin.com", "viafoura.co"]:
        for cookie in _chrome_cookies(domain):
            session.cookies.set_cookie(cookie)
    return session


def _get_user_uuid(session):
    token = session.cookies.get(f"VfAccess_{SECTION_UUID}")
    if not token:
        return None
    payload = _decode_jwt_payload(token)
    return payload.get("vf:uu") or payload.get("sub")


def _get_profile(session, user_uuid):
    if not user_uuid:
        return {}
    response = session.get(PROFILE_URL.format(user_uuid=user_uuid), timeout=20)
    response.raise_for_status()
    return response.json()


def read_notifications(limit=20):
    session = _build_session()
    response = session.get(NOTIFICATIONS_URL, timeout=20)
    response.raise_for_status()
    data = response.json()
    user_uuid = _get_user_uuid(session)
    profile = _get_profile(session, user_uuid)
    notifications = data.get("notifications", [])[: max(1, min(int(limit), 100))]
    return {
        "user_uuid": user_uuid,
        "profile": profile,
        "broadcasts": data.get("broadcasts", []),
        "notifications": notifications,
    }


if __name__ == "__main__":
    try:
        limit = 20
        if len(sys.argv) > 1:
            payload = json.loads(sys.argv[1])
            limit = payload.get("limit", 20)
        print(json.dumps(read_notifications(limit), ensure_ascii=False))
    except Exception as exc:
        print(json.dumps({"error": str(exc)}))
        sys.exit(1)
