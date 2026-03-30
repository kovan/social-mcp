# Cantillon Trading System — Project Brief

## Overview
Build a system that uses Claude API as an analysis layer and Alpaca API as the execution layer to make leveraged bets based on a Cantillon Effect thesis (sectors closest to new money creation benefit first).

## Thesis
- Fed reserve management purchases (~$40B/mo in short-term Treasuries)
- $1.9T fiscal deficit (FY2026)
- AI hyperscaler capex (~$527B+ in 2026)
- First-order recipients: semiconductors, cloud hyperscalers, energy/utilities, defense
- Second-order recipients (6-18mo lag): construction/engineering, power equipment, materials

## Target Instruments

### Leveraged ETFs (simpler, short-term directional bets)
- SOXL — 3x semiconductors
- TECL — 3x technology
- UPRO — 3x S&P 500
- TPOR — 3x transportation/infrastructure
- WARNING: volatility decay on daily-reset leveraged ETFs — not for long holds

### Options Candidates (defined risk, more control)
- NVDA, AMD, AVGO (semiconductors)
- VRT, ETN (power/cooling infrastructure)
- CEG, NEE (utilities/energy)
- PWR, EME (construction/engineering)
- GEV (power generation equipment)
- Strategy: buy calls only (no selling), 30-60 DTE, slightly OTM
- Key catalyst: Fed meeting March 17-18, 2026

### Long-term Holds (from watchlist)
See cantillon_watchlist.md for the full list organized by Cantillon proximity.

## Architecture (To Build)

```
Data Sources                  Analysis Layer           Execution Layer
─────────────                 ──────────────           ───────────────
Fed minutes/statements   ──►                      
Earnings transcripts     ──►  Claude API            ──►  Alpaca API
Gov spending docs        ──►  (sentiment extraction,     (order placement,
SEC filings              ──►   capex signal detection,    position mgmt,
Energy permit filings    ──►   thesis scoring)            risk limits)
Market data (Alpaca)     ──►
```

### Components to Build
1. **Data ingestion** — Pull earnings call transcripts, Fed minutes, news feeds
2. **Claude analysis pipeline** — Prompt templates for extracting Cantillon signals:
   - Capex guidance changes (up/down)
   - Money flow indicators (new contracts, government spending announcements)
   - Sentiment shifts in Fed language
3. **Signal scoring** — Aggregate Claude's analysis into buy/hold/reduce signals per name
4. **Alpaca execution** — Place orders via API, enforce position sizing and risk limits
5. **Risk management** — Max position size, stop losses, total portfolio exposure limits

### Risk Parameters (Learning Money)
- Total capital: TBD (small experimental amount)
- Max single position: ~10-20% of total
- Stop loss: define per trade
- No selling options (buying only)
- Paper trade first via Alpaca's paper trading environment

## Tech Stack
- Clojure MCP server (ibkr-mcp) for IBKR integration via TWS API Java interop
- Alpaca official MCP server (alpaca-mcp-server) for Alpaca execution
- Claude subscription as the analysis layer (no API key needed — MCP tools)
- Cantillon thesis + watchlist baked in as MCP resources

## Broker Choice
- Alpaca — API-first, commission-free, instant paper trading, official MCP server
- IBKR — comprehensive fundamentals (EBITDA, financials, analyst estimates), global market access
- Both configured as MCP servers, Claude has access to both simultaneously

## Status
- [x] IBKR MCP server built (Clojure, 14 tools)
- [x] Alpaca MCP server installed (official, alpaca-mcp-server)
- [x] Cantillon thesis + watchlist as MCP resources
- [x] Analysis prompt templates (cantillon_analysis, fed_signal_check)
- [ ] Alpaca API keys (awaiting account)
- [ ] IBKR account funded + Gateway connected
- [ ] Data ingestion (Fed minutes, earnings transcripts)
- [ ] Live paper trading tests
