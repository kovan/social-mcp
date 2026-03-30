#!/usr/bin/env python3
"""MCP server for Telegram using Telethon (personal account)."""

import asyncio
import json
import sys
import traceback
from datetime import datetime, timezone

from telethon import TelegramClient
from telethon.tl.types import User, Chat, Channel, Message
from telethon.tl.functions.channels import JoinChannelRequest
from telethon.tl.functions.contacts import SearchRequest

API_ID = 34148443
API_HASH = "0ceeca5d1e375bac832682cf86d6f5a4"
SESSION = "/home/k/social-mcp/telegram/session"

TOOLS = [
    {
        "name": "list_chats",
        "description": "List recent Telegram chats/dialogs with unread counts.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "n": {"type": "number", "description": "Number of chats (default 20, max 100)"}
            }
        }
    },
    {
        "name": "read_chat",
        "description": "Read recent messages from a Telegram chat.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "chat": {"type": "string", "description": "Chat name, username (@user), phone number, or numeric ID"},
                "n": {"type": "number", "description": "Number of messages (default 20, max 100)"}
            },
            "required": ["chat"]
        }
    },
    {
        "name": "send_message",
        "description": "Send a message to a Telegram chat or user.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "chat": {"type": "string", "description": "Chat name, username (@user), phone number, or numeric ID"},
                "text": {"type": "string", "description": "Message text (Markdown supported)"}
            },
            "required": ["chat", "text"]
        }
    },
    {
        "name": "reply",
        "description": "Reply to a specific message in a Telegram chat.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "chat": {"type": "string", "description": "Chat name, username (@user), phone number, or numeric ID"},
                "message_id": {"type": "number", "description": "ID of the message to reply to"},
                "text": {"type": "string", "description": "Reply text (Markdown supported)"}
            },
            "required": ["chat", "message_id", "text"]
        }
    },
    {
        "name": "search",
        "description": "Search messages in a chat or globally.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "chat": {"type": "string", "description": "Chat to search in (omit for global search)"},
                "n": {"type": "number", "description": "Number of results (default 20, max 50)"}
            },
            "required": ["query"]
        }
    },
    {
        "name": "get_unread",
        "description": "Get all unread messages across chats.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "n": {"type": "number", "description": "Max chats to check (default 10, max 30)"}
            }
        }
    },
    {
        "name": "search_channels",
        "description": "Search for public Telegram channels and groups globally.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "n": {"type": "number", "description": "Number of results (default 10, max 20)"}
            },
            "required": ["query"]
        }
    },
    {
        "name": "join_channel",
        "description": "Join a Telegram channel or group by username.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "channel": {"type": "string", "description": "Channel/group username (without @)"}
            },
            "required": ["channel"]
        }
    },
    {
        "name": "mark_read",
        "description": "Mark all messages in a chat as read.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "chat": {"type": "string", "description": "Chat name, username (@user), phone number, or numeric ID"}
            },
            "required": ["chat"]
        }
    }
]


def clamp(val, default, lo, hi):
    if val is None:
        return default
    n = int(val) if isinstance(val, (int, float)) else default
    return max(lo, min(n, hi))


def format_entity(entity):
    if isinstance(entity, User):
        name = entity.first_name or ""
        if entity.last_name:
            name += " " + entity.last_name
        tag = f"@{entity.username}" if entity.username else f"id:{entity.id}"
        return f"{name} ({tag})"
    elif isinstance(entity, (Chat, Channel)):
        tag = f"@{entity.username}" if hasattr(entity, 'username') and entity.username else f"id:{entity.id}"
        return f"{entity.title} ({tag})"
    return str(entity)


def format_message(msg):
    if not isinstance(msg, Message):
        return ""
    sender = ""
    if msg.sender:
        if isinstance(msg.sender, User):
            sender = msg.sender.first_name or "Unknown"
            if msg.sender.last_name:
                sender += " " + msg.sender.last_name
        elif hasattr(msg.sender, 'title'):
            sender = msg.sender.title
    date = msg.date.strftime("%Y-%m-%d %H:%M") if msg.date else ""
    text = msg.text or ""
    if msg.media and not text:
        text = f"[media: {type(msg.media).__name__}]"
    elif msg.media:
        text += f" [+media: {type(msg.media).__name__}]"
    reply = f" (reply to {msg.reply_to.reply_to_msg_id})" if msg.reply_to else ""
    return f"[{msg.id}] {sender} | {date}{reply}\n{text}"


async def resolve_chat(client, chat_str):
    """Resolve a chat string to a Telethon entity."""
    # Try as numeric ID
    try:
        chat_id = int(chat_str)
        return await client.get_entity(chat_id)
    except (ValueError, Exception):
        pass
    # Try as username or phone
    try:
        return await client.get_entity(chat_str)
    except Exception:
        pass
    # Try searching dialogs by name
    async for dialog in client.iter_dialogs(limit=100):
        if chat_str.lower() in dialog.name.lower():
            return dialog.entity
    raise ValueError(f"Chat not found: {chat_str}")


class TelegramMCP:
    def __init__(self):
        self.client = None

    async def ensure_client(self):
        if self.client is None:
            self.client = TelegramClient(SESSION, API_ID, API_HASH)
            await self.client.connect()

    async def handle_tool(self, name, args):
        await self.ensure_client()

        if name == "list_chats":
            n = clamp(args.get("n"), 20, 1, 100)
            lines = ["# Recent chats\n"]
            async for dialog in self.client.iter_dialogs(limit=n):
                unread = f" [{dialog.unread_count} unread]" if dialog.unread_count else ""
                entity_type = "user" if dialog.is_user else ("group" if dialog.is_group else "channel")
                tag = ""
                if hasattr(dialog.entity, 'username') and dialog.entity.username:
                    tag = f" @{dialog.entity.username}"
                lines.append(f"- {dialog.name}{tag} ({entity_type}, id:{dialog.id}){unread}")
            return "\n".join(lines)

        elif name == "read_chat":
            entity = await resolve_chat(self.client, args["chat"])
            n = clamp(args.get("n"), 20, 1, 100)
            messages = []
            async for msg in self.client.iter_messages(entity, limit=n):
                formatted = format_message(msg)
                if formatted:
                    messages.append(formatted)
            title = format_entity(entity)
            if messages:
                messages.reverse()
                return f"# {title} ({len(messages)} messages)\n\n" + "\n\n".join(messages)
            return f"# {title}\n\nNo messages."

        elif name == "send_message":
            entity = await resolve_chat(self.client, args["chat"])
            msg = await self.client.send_message(entity, args["text"], parse_mode="md")
            return f"Message sent to {format_entity(entity)}. Message ID: {msg.id}"

        elif name == "reply":
            entity = await resolve_chat(self.client, args["chat"])
            msg_id = int(args["message_id"])
            msg = await self.client.send_message(entity, args["text"], reply_to=msg_id, parse_mode="md")
            return f"Reply sent to {format_entity(entity)}. Message ID: {msg.id}"

        elif name == "search":
            query = args["query"]
            n = clamp(args.get("n"), 20, 1, 50)
            entity = None
            if args.get("chat"):
                entity = await resolve_chat(self.client, args["chat"])
            messages = []
            async for msg in self.client.iter_messages(entity, search=query, limit=n):
                formatted = format_message(msg)
                if formatted:
                    chat_name = ""
                    if entity is None and msg.chat:
                        chat_name = getattr(msg.chat, 'title', None) or getattr(msg.chat, 'first_name', '') or ""
                        chat_name = f" in {chat_name}"
                    messages.append(f"{formatted}{chat_name}")
            if messages:
                messages.reverse()
                scope = format_entity(entity) if entity else "all chats"
                return f"# Search: '{query}' in {scope} ({len(messages)} results)\n\n" + "\n\n".join(messages)
            return f"# Search: '{query}'\n\nNo results."

        elif name == "get_unread":
            n = clamp(args.get("n"), 10, 1, 30)
            sections = []
            async for dialog in self.client.iter_dialogs(limit=100):
                if dialog.unread_count > 0:
                    messages = []
                    async for msg in self.client.iter_messages(dialog.entity, limit=min(dialog.unread_count, 10)):
                        formatted = format_message(msg)
                        if formatted:
                            messages.append(formatted)
                    if messages:
                        messages.reverse()
                        sections.append(f"## {dialog.name} ({dialog.unread_count} unread)\n\n" + "\n\n".join(messages))
                    if len(sections) >= n:
                        break
            if sections:
                return "# Unread messages\n\n" + "\n\n---\n\n".join(sections)
            return "# Unread messages\n\nNo unread messages."

        elif name == "search_channels":
            query = args["query"]
            n = clamp(args.get("n"), 10, 1, 20)
            result = await self.client(SearchRequest(q=query, limit=n))
            lines = [f"# Search channels: '{query}'\n"]
            for chat in result.chats:
                username = f"@{chat.username}" if hasattr(chat, 'username') and chat.username else ""
                members = getattr(chat, 'participants_count', '?')
                megagroup = getattr(chat, 'megagroup', False)
                ctype = "group" if megagroup else "channel"
                lines.append(f"- {chat.title} {username} ({ctype}, {members} members)")
            return "\n".join(lines) if len(lines) > 1 else f"# Search channels: '{query}'\n\nNo results."

        elif name == "join_channel":
            channel = args["channel"].lstrip("@")
            entity = await self.client.get_entity(channel)
            await self.client(JoinChannelRequest(entity))
            return f"Joined {format_entity(entity)}"

        elif name == "mark_read":
            entity = await resolve_chat(self.client, args["chat"])
            await self.client.send_read_acknowledge(entity)
            return f"Marked all messages as read in {format_entity(entity)}."

        else:
            raise ValueError(f"Unknown tool: {name}")

    def respond(self, id, result):
        return {"jsonrpc": "2.0", "id": id, "result": result}

    def error_response(self, id, code, message):
        return {"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message}}

    def tool_result(self, text, error=False):
        return {"content": [{"type": "text", "text": text}], "isError": error}

    async def handle_message(self, msg):
        id = msg.get("id")
        method = msg.get("method")
        params = msg.get("params", {})

        if method == "initialize":
            return self.respond(id, {
                "protocolVersion": "2024-11-05",
                "capabilities": {"tools": {}},
                "serverInfo": {"name": "telegram-mcp", "version": "0.1.0"},
                "instructions": "MCP server for Telegram personal account via Telethon. Requires authenticated session."
            })
        elif method == "notifications/initialized":
            return None
        elif method == "tools/list":
            return self.respond(id, {"tools": TOOLS})
        elif method == "tools/call":
            name = params.get("name")
            arguments = params.get("arguments", {})
            try:
                result = await self.handle_tool(name, arguments)
                return self.respond(id, self.tool_result(result))
            except Exception as e:
                tb = traceback.format_exc()
                return self.respond(id, self.tool_result(f"Error: {e}\n\n{tb}", error=True))
        elif method == "ping":
            return self.respond(id, {})
        else:
            if id:
                return self.error_response(id, -32601, f"Method not found: {method}")
            return None

    async def run(self):
        loop = asyncio.get_event_loop()
        reader = asyncio.StreamReader()
        protocol = asyncio.StreamReaderProtocol(reader)
        await loop.connect_read_pipe(lambda: protocol, sys.stdin)

        while True:
            line = await reader.readline()
            if not line:
                break
            line = line.decode("utf-8").strip()
            if not line:
                continue
            try:
                msg = json.loads(line)
                resp = await self.handle_message(msg)
                if resp:
                    out = json.dumps(resp) + "\n"
                    sys.stdout.buffer.write(out.encode("utf-8"))
                    sys.stdout.buffer.flush()
            except Exception as e:
                print(f"Error processing message: {e}", file=sys.stderr)


def main():
    mcp = TelegramMCP()
    asyncio.run(mcp.run())


if __name__ == "__main__":
    main()
