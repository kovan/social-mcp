#!/usr/bin/env python3
"""Instagram API bridge using instagrapi. Called by the Clojure MCP server."""
import sys
import json
import os

SESSION_FILE = os.path.expanduser("~/.instagram_session.json")
STATE_FILE = os.path.expanduser("~/.instagram_state.json")


def load_state():
    if os.path.exists(STATE_FILE):
        with open(STATE_FILE) as f:
            return json.load(f)
    return {"commented_posts": []}


def save_state(state):
    with open(STATE_FILE, "w") as f:
        json.dump(state, f)


def load_session_settings():
    if os.path.exists(SESSION_FILE):
        with open(SESSION_FILE) as f:
            return json.load(f)
    return {}


def sessionid_from_settings(settings):
    auth = settings.get("authorization_data") or {}
    if auth.get("sessionid"):
        return auth["sessionid"]
    cookies = settings.get("cookies") or {}
    if isinstance(cookies, dict) and cookies.get("sessionid"):
        return cookies["sessionid"]
    return None


def get_client():
    from instagrapi import Client
    cl = Client()
    cl.delay_range = [1, 3]
    settings = load_session_settings()

    if settings:
        cl.load_settings(SESSION_FILE)
        sessionid = sessionid_from_settings(settings)
        if sessionid:
            try:
                cl.login_by_sessionid(sessionid)
                cl.dump_settings(SESSION_FILE)
                return cl
            except Exception:
                pass

    username = os.environ.get("INSTAGRAM_USERNAME")
    password = os.environ.get("INSTAGRAM_PASSWORD")
    if username and password:
        cl.login(username, password)
        cl.dump_settings(SESSION_FILE)
        return cl

    raise Exception(
        "Instagram auth unavailable: provide INSTAGRAM_USERNAME and "
        "INSTAGRAM_PASSWORD, or ensure ~/.instagram_session.json contains "
        "a valid sessionid"
    )


def format_media(media):
    return {
        "id": str(media.pk),
        "shortcode": media.code,
        "url": f"https://www.instagram.com/p/{media.code}/",
        "type": media.media_type,
        "caption": (media.caption_text or "")[:500],
        "likes": media.like_count,
        "comments": media.comment_count,
        "timestamp": media.taken_at.isoformat() if media.taken_at else None,
        "user": media.user.username if media.user else None,
    }


def format_comment(c):
    return {
        "id": str(c.pk),
        "text": c.text,
        "user": c.user.username if c.user else None,
        "timestamp": c.created_at_utc.isoformat() if c.created_at_utc else None,
        "likes": c.like_count,
    }


def extract_code(url_or_code):
    """Extract shortcode from URL or return as-is."""
    if "/p/" in url_or_code:
        return url_or_code.split("/p/")[1].split("/")[0].split("?")[0]
    if "/reel/" in url_or_code:
        return url_or_code.split("/reel/")[1].split("/")[0].split("?")[0]
    return url_or_code.strip()


def main():
    if len(sys.argv) < 2:
        raise Exception("Usage: instagram_api.py <command> [json_args]")
    cmd = sys.argv[1]
    args = json.loads(sys.argv[2]) if len(sys.argv) > 2 else {}

    cl = get_client()

    if cmd == "hashtag_top":
        tag = args["hashtag"].lstrip("#")
        n = int(args.get("n", 9))
        medias = cl.hashtag_medias_top(tag, amount=n)
        print(json.dumps([format_media(m) for m in medias]))

    elif cmd == "hashtag_recent":
        tag = args["hashtag"].lstrip("#")
        n = int(args.get("n", 20))
        medias = cl.hashtag_medias_recent_v1(tag, max_amount=n)
        print(json.dumps([format_media(m) for m in medias]))

    elif cmd == "post_info":
        code = extract_code(args["url"])
        media_id = cl.media_pk_from_code(code)
        media = cl.media_info(media_id)
        comments = cl.media_comments(media_id, amount=20)
        result = format_media(media)
        result["top_comments"] = [format_comment(c) for c in comments]
        print(json.dumps(result))

    elif cmd == "post_comment":
        code = extract_code(args["url"])
        text = args["text"]
        media_id = cl.media_pk_from_code(code)
        comment = cl.media_comment(media_id, text)
        print(json.dumps({"id": str(comment.pk), "text": comment.text}))

    elif cmd == "user_posts":
        username = args["username"].lstrip("@")
        n = int(args.get("n", 12))
        user_id = cl.user_id_from_username(username)
        medias = cl.user_medias(user_id, amount=n)
        print(json.dumps([format_media(m) for m in medias]))

    elif cmd == "user_info":
        username = args["username"].lstrip("@")
        user = cl.user_info_by_username_v1(username)
        print(json.dumps({
            "id": str(user.pk),
            "username": user.username,
            "full_name": user.full_name,
            "bio": user.biography or "",
            "followers": user.follower_count,
            "following": user.following_count,
            "posts": user.media_count,
            "is_private": user.is_private,
            "is_verified": user.is_verified,
        }))

    elif cmd == "search_hashtags":
        query = args["query"].lstrip("#")
        results = cl.hashtag_search(query)
        print(json.dumps([
            {"name": h.name, "media_count": h.media_count}
            for h in results[:20]
        ]))

    elif cmd == "account_new_posts":
        username = args["username"].lstrip("@")
        n = int(args.get("n", 20))
        state = load_state()
        seen = set(state.get("commented_posts", []))
        user_id = cl.user_id_from_username(username)
        medias = cl.user_medias(user_id, amount=n)
        new_posts = [format_media(m) for m in medias if m.code not in seen]
        print(json.dumps(new_posts))

    elif cmd == "mark_commented":
        shortcode = args["shortcode"]
        state = load_state()
        if shortcode not in state["commented_posts"]:
            state["commented_posts"].append(shortcode)
            save_state(state)
        print(json.dumps({"ok": True, "shortcode": shortcode}))

    elif cmd == "notifications":
        n = int(args.get("n", 20))
        items = cl.news_inbox_v1()
        type_map = {
            1: "like", 2: "like", 3: "like",
            12: "follow", 13: "follow",
            14: "follow_request",
            66: "comment_like",
            101: "like", 102: "comment_like",
            109: "mention",
            159: "memory",
            467: "suggestion",
            834: "comment",
        }

        def media_pk_to_code(pk):
            """Convert media PK to Instagram shortcode (no API call needed)."""
            alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
            code = ''
            while pk > 0:
                code = alphabet[pk % 64] + code
                pk //= 64
            return code

        results = []
        all_stories = items.get("new_stories", []) + items.get("old_stories", [])
        for story in all_stories[:n]:
            st = story.get("story_type", 0)
            story_args = story.get("args", {})
            # Build readable text from the notification
            text = story_args.get("text", "")
            if not text:
                rich_text = story_args.get("rich_text", "")
                if rich_text:
                    text = rich_text
            comment_text = story_args.get("comment_text", "")
            entry = {
                "type": type_map.get(st, f"unknown_{st}"),
                "type_id": st,
                "timestamp": story_args.get("timestamp"),
                "text": text,
                "comment_text": comment_text,
                "profile_name": story_args.get("profile_name", ""),
                "profile_id": str(story_args.get("profile_id", "")),
                "post_url": None,
            }
            # Extract post URL from media info
            media_list = story_args.get("media", [])
            if media_list and isinstance(media_list, list) and len(media_list) > 0:
                m = media_list[0]
                mid = m.get("id", "")
                if mid:
                    try:
                        # media id can be "pk_userid" or just "pk"
                        pk = int(str(mid).split("_")[0])
                        code = media_pk_to_code(pk)
                        entry["post_url"] = f"https://www.instagram.com/p/{code}/"
                    except (ValueError, TypeError):
                        pass
            results.append(entry)
        print(json.dumps(results))

    elif cmd == "follow_user":
        username = args["username"].lstrip("@")
        user_id = cl.user_id_from_username(username)
        result = cl.user_follow(user_id)
        print(json.dumps({"followed": bool(result), "username": username}))

    elif cmd == "generate_image":
        import subprocess
        gen_script = os.path.join(os.path.dirname(os.path.abspath(__file__)), "generate_image.py")
        result = subprocess.run(
            ["python3", gen_script, json.dumps(args)],
            capture_output=True, text=True, check=True
        )
        print(result.stdout.strip())

    elif cmd == "upload_photo":
        path = args["path"]
        caption = args.get("caption", "")
        media = cl.photo_upload(path, caption)
        print(json.dumps(format_media(media)))

    else:
        raise Exception(f"Unknown command: {cmd}")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(json.dumps({"error": str(e)}), file=sys.stderr)
        sys.exit(1)
