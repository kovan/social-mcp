# steam-mcp

A [Model Context Protocol](https://modelcontextprotocol.io) server for the Steam Web API, written in Clojure. Lets Claude (or any MCP client) browse your Steam library, check what friends are playing, look up game details, and more.

## Tools

| Tool | Description |
|------|-------------|
| `player_summary` | Profile info and online status |
| `owned_games` | Full library sorted by playtime |
| `recently_played` | Games played in the last 2 weeks |
| `friends` | Friend list with online status and current game |
| `achievements` | Achievement progress for a specific game |
| `game_news` | Recent news and updates for a game |
| `game_details` | Store page info: description, price, genres, Metacritic score |

## Setup

### 1. Get a Steam API key

Register at https://steamcommunity.com/dev (requires a non-limited Steam account).

### 2. Find your Steam ID

Your 17-digit Steam ID. Find it at https://steamid.io or from your profile URL.

### 3. Configure Claude Code

Add to `~/.claude.json` under `mcpServers`:

```json
"steam": {
  "command": "bash",
  "args": [
    "-c",
    "cd /path/to/steam-mcp && exec clojure -M -m steam.mcp"
  ],
  "env": {
    "STEAM_API_KEY": "your-api-key",
    "STEAM_ID": "your-steam-id"
  }
}
```

### 4. Restart Claude Code

The `steam` tools should now appear. Try asking "what games have I played recently?" or "what are my friends playing?".

## Requirements

- Clojure 1.12+
- curl

## License

MIT
