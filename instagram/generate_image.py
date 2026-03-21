#!/usr/bin/env python3
"""Generate Instagram images for economic/political content."""
import sys
import json
import os
import textwrap
from PIL import Image, ImageDraw, ImageFont

FONT_BOLD = "/usr/share/fonts/noto/NotoSans-Bold.ttf"
FONT_REG  = "/usr/share/fonts/noto/NotoSans-Regular.ttf"

# Palette: dark slate background, accent colors
PALETTES = {
    "dark":   {"bg": (15, 20, 30),    "accent": (64, 180, 255),  "text": (240, 240, 240), "sub": (160, 170, 185)},
    "red":    {"bg": (28, 10, 10),    "accent": (220, 60, 60),   "text": (240, 235, 230), "sub": (180, 155, 150)},
    "green":  {"bg": (10, 25, 18),    "accent": (50, 200, 120),  "text": (230, 245, 235), "sub": (140, 175, 155)},
    "gold":   {"bg": (22, 18, 8),     "accent": (220, 175, 50),  "text": (245, 240, 220), "sub": (175, 165, 130)},
}


def wrap_text(text, font, draw, max_width):
    """Wrap text to fit max_width pixels."""
    words = text.split()
    lines = []
    current = []
    for word in words:
        test = " ".join(current + [word])
        bbox = draw.textbbox((0, 0), test, font=font)
        if bbox[2] > max_width and current:
            lines.append(" ".join(current))
            current = [word]
        else:
            current.append(word)
    if current:
        lines.append(" ".join(current))
    return lines


def make_stat_card(stat, label, context, palette="dark", out_path=None):
    """Big number + label + one-line context. Good for data points."""
    W, H = 1080, 1080
    p = PALETTES[palette]
    img = Image.new("RGB", (W, H), p["bg"])
    draw = ImageDraw.Draw(img)

    pad = 80
    content_w = W - 2 * pad

    # Accent bar at top
    draw.rectangle([(pad, 60), (pad + 120, 68)], fill=p["accent"])

    # Stat (huge)
    fnt_stat = ImageFont.truetype(FONT_BOLD, 180)
    stat_lines = wrap_text(stat, fnt_stat, draw, content_w)
    y = 120
    for line in stat_lines:
        draw.text((pad, y), line, font=fnt_stat, fill=p["accent"])
        bbox = draw.textbbox((0, 0), line, font=fnt_stat)
        y += bbox[3] - bbox[1] + 10

    # Label
    fnt_label = ImageFont.truetype(FONT_BOLD, 52)
    label_lines = wrap_text(label.upper(), fnt_label, draw, content_w)
    y += 20
    for line in label_lines:
        draw.text((pad, y), line, font=fnt_label, fill=p["text"])
        bbox = draw.textbbox((0, 0), line, font=fnt_label)
        y += bbox[3] - bbox[1] + 8

    # Divider
    y += 30
    draw.rectangle([(pad, y), (W - pad, y + 2)], fill=p["sub"])
    y += 30

    # Context
    fnt_ctx = ImageFont.truetype(FONT_REG, 36)
    ctx_lines = wrap_text(context, fnt_ctx, draw, content_w)
    for line in ctx_lines:
        draw.text((pad, y), line, font=fnt_ctx, fill=p["sub"])
        bbox = draw.textbbox((0, 0), line, font=fnt_ctx)
        y += bbox[3] - bbox[1] + 6

    # Handle
    fnt_handle = ImageFont.truetype(FONT_REG, 30)
    handle = "@void_is_king"
    bbox = draw.textbbox((0, 0), handle, font=fnt_handle)
    draw.text((W - pad - (bbox[2] - bbox[0]), H - 60), handle, font=fnt_handle, fill=p["sub"])

    if out_path is None:
        out_path = f"/tmp/claude-1000/ig_stat_{stat[:10].replace(' ','_')}.jpg"
    img.save(out_path, "JPEG", quality=95)
    return out_path


def make_opinion_card(headline, body, palette="dark", out_path=None):
    """Bold headline + body paragraph. Good for takes/analysis."""
    W, H = 1080, 1080
    p = PALETTES[palette]
    img = Image.new("RGB", (W, H), p["bg"])
    draw = ImageDraw.Draw(img)

    pad = 80
    content_w = W - 2 * pad

    # Accent bar
    draw.rectangle([(pad, 60), (pad + 120, 68)], fill=p["accent"])

    # Opening quote mark
    fnt_quote = ImageFont.truetype(FONT_BOLD, 120)
    draw.text((pad - 10, 80), "\u201c", font=fnt_quote, fill=p["accent"])

    # Headline
    fnt_hl = ImageFont.truetype(FONT_BOLD, 64)
    hl_lines = wrap_text(headline, fnt_hl, draw, content_w)
    y = 220
    for line in hl_lines:
        draw.text((pad, y), line, font=fnt_hl, fill=p["text"])
        bbox = draw.textbbox((0, 0), line, font=fnt_hl)
        y += bbox[3] - bbox[1] + 10

    # Divider
    y += 30
    draw.rectangle([(pad, y), (pad + 60, y + 4)], fill=p["accent"])
    y += 40

    # Body
    fnt_body = ImageFont.truetype(FONT_REG, 38)
    body_lines = wrap_text(body, fnt_body, draw, content_w)
    for line in body_lines:
        draw.text((pad, y), line, font=fnt_body, fill=p["sub"])
        bbox = draw.textbbox((0, 0), line, font=fnt_body)
        y += bbox[3] - bbox[1] + 8

    # Handle
    fnt_handle = ImageFont.truetype(FONT_REG, 30)
    handle = "@void_is_king"
    bbox = draw.textbbox((0, 0), handle, font=fnt_handle)
    draw.text((W - pad - (bbox[2] - bbox[0]), H - 60), handle, font=fnt_handle, fill=p["sub"])

    if out_path is None:
        slug = headline[:20].replace(" ", "_").replace(".", "")
        out_path = f"/tmp/claude-1000/ig_opinion_{slug}.jpg"
    img.save(out_path, "JPEG", quality=95)
    return out_path


def main():
    args = json.loads(sys.argv[1]) if len(sys.argv) > 1 else {}
    kind = args.get("kind", "opinion")
    palette = args.get("palette", "dark")
    out = args.get("out", None)

    if kind == "stat":
        path = make_stat_card(
            stat=args["stat"],
            label=args["label"],
            context=args["context"],
            palette=palette,
            out_path=out,
        )
    else:
        path = make_opinion_card(
            headline=args["headline"],
            body=args["body"],
            palette=palette,
            out_path=out,
        )
    print(json.dumps({"path": path}))


if __name__ == "__main__":
    main()
