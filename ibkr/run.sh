#!/bin/bash
exec 2>/tmp/ibkr-mcp-debug.log
echo "run.sh started at $(date)" >&2
echo "PATH=$PATH" >&2
echo "which clojure: $(which clojure 2>&1)" >&2
echo "which java: $(which java 2>&1)" >&2
cd /home/k/ibkr-mcp
echo "cwd: $(pwd)" >&2
exec clojure -M:run
