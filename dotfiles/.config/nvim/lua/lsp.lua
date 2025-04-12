-- Setup language servers.
local lspconfig = require("lspconfig")
-- C
lspconfig.clangd.setup{}
-- zig
lspconfig.zls.setup{}
-- rust
lspconfig.rust_analyzer.setup{}
-- nim
lspconfig.nimls.setup{}
-- lua
lspconfig.lua_ls.setup{}
-- perl
lspconfig.perlpls.setup{}
-- html
lspconfig.html.setup{}
-- Go
lspconfig.gopls.setup{}
-- Bash
lspconfig.bashls.setup{}
-- Python
lspconfig.pyright.setup{}
-- powershell
lspconfig.powershell_es.setup{}
-- php
lspconfig.intelephense.setup{}
-- crystal
lspconfig.crystalline.setup{}
-- ruby
lspconfig.solargraph.setup{}
-- gleam
lspconfig.gleam.setup{}
-- elixir
lspconfig.elixirls.setup{}
-- ocaml
lspconfig.ocamllsp.setup{}
-- vlang
lspconfig.vls.setup{}
