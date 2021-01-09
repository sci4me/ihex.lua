#!/bin/bash
luacheck lua-ihex-dev-1.rockspec
STATUS=$?
[ $STATUS -ne 0 ] && exit 1

ldoc .
STATUS=$?
[ $STATUS -ne 0 ] && exit 1

git add docs