import asyncio
from telethon import TelegramClient

async def main():
    client = TelegramClient('/home/k/social-mcp/telegram/session', 34148443, '0ceeca5d1e375bac832682cf86d6f5a4')
    await client.start()
    me = await client.get_me()
    print(f'Logged in as {me.first_name} ({me.username})')
    await client.disconnect()

asyncio.run(main())
