-- -- Setup language servers.
-- local lspconfig = require("lspconfig")
-- -- Guile
-- lspconfig.guile_ls.setup {
--     filetypes = {"scheme", "guile"}
-- }
-- -- C
-- lspconfig.clangd.setup{}
-- -- zig
-- lspconfig.zls.setup{}
-- -- rust
-- lspconfig.rust_analyzer.setup{}
-- -- nim
-- lspconfig.nimls.setup{}
-- -- lua
-- lspconfig.lua_ls.setup{}
-- -- perl
-- lspconfig.perlpls.setup{}
-- -- html
-- lspconfig.html.setup{}
-- -- Go
-- lspconfig.gopls.setup{}
-- -- Bash
-- lspconfig.bashls.setup{}
-- -- Python
-- lspconfig.pyright.setup{}
-- -- powershell
-- lspconfig.powershell_es.setup{}
-- -- php
-- lspconfig.intelephense.setup{}
-- -- crystal
-- lspconfig.crystalline.setup{}
-- -- ruby
-- lspconfig.solargraph.setup{}
-- -- gleam
-- lspconfig.gleam.setup{}
-- -- elixir
-- lspconfig.elixirls.setup{}
-- -- ocaml
-- lspconfig.ocamllsp.setup{}
-- -- vlang
-- lspconfig.vls.setup{}


-- A table defining all language servers and their configurations.
-- Format: { "server_name", { config_table } }
local lsps = {
    -- Guile
    { "guile_ls", { filetypes = {"scheme", "guile"} } },
    -- C/C++
    { "clangd" }, -- Keep empty for default setup
    -- zig
    { "zls" },
    -- rust
    { "rust_analyzer" },
    -- nim
    { "nimls" },
    -- lua
    { "lua_ls" },
    -- perl
    { "perlpls" },
    -- html
    { "html" },
    -- Go
    { "gopls" },
    -- Bash
    { "bashls" },
    -- Python
    { "pyright" },
    -- powershell
    { "powershell_es" },
    -- php
    { "intelephense" },
    -- crystal
    { "crystalline" },
    -- ruby
    { "solargraph" },
    -- gleam
    { "gleam" },
    -- elixir
    { "elixirls" },
    -- ocaml
    { "ocamllsp" },
    -- vlang
    { "vls" },
}

-- Loop through the defined servers and set them up using lspconfig.
for _, lsp in pairs(lsps) do
    local name, config = lsp[1], lsp[2]
    vim.lsp.enable(name)
    if config then
        vim.lsp.config(name, config)
    end
end
