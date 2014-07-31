#!/bin/sh

if ! lua -v >/dev/null 2>&1; then
    echo "lua not installed.  try 'brew install lua' on mac os x or 'sudo apt-get install lua5.1' on ubuntu linux"
    exit
fi

LUALIB="/opt/opscode/embedded/lualib"

if [ ! -f lpeg.so ]; then
  ln -s $LUALIB/lpeg.so
fi

if [ ! -f cjson.so ]; then
  ln -s $LUALIB/cjson.so
fi

lua -e 'if pcall(require,"lpeg") then os.exit(0) else os.exit(1) end'
if [ $? -ne 0 ]; then
  echo "module lpeg not installed.  try 'brew install luarocks' or 'sudo apt-get install luarocks', then 'luarocks install lpeg'"
  exit
fi

lua -e 'if pcall(require,"cjson") then os.exit(0) else os.exit(1) end'
if [ $? -ne 0 ]; then
  echo "module cjson not installed.  try 'brew install luarocks' or 'sudo apt-get install luarocks', then 'luarocks install lua-cjson'"
  exit
fi

SCRIPT_DIR="/var/opt/opscode/nginx/etc/scripts"

if [ ! -f resolver.lua ]; then
  ln -s $SCRIPT_DIR/resolver.lua
fi

if [ ! -f routes.lua ]; then
  ln -s $SCRIPT_DIR/routes.lua
fi

if [ ! -f route_checks.lua ]; then
  ln -s $SCRIPT_DIR/route_checks.lua
fi

echo "Running tests"
echo "-------------"

lua run_tests.lua
