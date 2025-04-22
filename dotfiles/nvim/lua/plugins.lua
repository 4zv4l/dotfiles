require("packer").startup (function(use)
    -- packer itself
    use 'wbthomason/packer.nvim'

    -- lsp
    use 'neovim/nvim-lspconfig'
    use 'nvim-treesitter/nvim-treesitter'
    use 'bakpakin/janet.vim'

    -- completion engine
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/nvim-cmp'
    -- icons for completion
    use 'onsails/lspkind.nvim'
    -- snippet engine (piece of code)
    use({
        'L3MON4D3/LuaSnip',
        -- follow latest release.
        tag = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    })
    -- rounded corner
    use 'gelguy/wilder.nvim'
    use 'romgrk/fzy-lua-native'

    -- status line
    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true }
    }
    -- git sign on the left
    use 'lewis6991/gitsigns.nvim'

    -- autoclose {} () ""
    use 'm4xshen/autoclose.nvim'

    -- telescope
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.2',
        requires = { 'nvim-lua/plenary.nvim' }
    }

    -- file managment
    use {
        "nvim-telescope/telescope-file-browser.nvim",
        requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
    }

    -- colorscheme
    use 'nyoom-engineering/oxocarbon.nvim'
    use 'rebelot/kanagawa.nvim'
    use 'folke/tokyonight.nvim'
    use 'catppuccin/nvim'
    use 'levouh/tint.nvim'

    -- float pop-up terminal
    use 'numToStr/FTerm.nvim'

end)

-- plugins setup

--treesitter
require('nvim-treesitter.configs').setup({
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  }
})
-- autoclose
require("autoclose").setup()
-- telescope
vim.cmd("nnoremap <leader>ff <cmd>Telescope find_files<cr>")
-- colorscheme
require("catppuccin").setup({
    falvour = "mocha",
    transparent_background = true,
})
vim.cmd.colorscheme "catppuccin-mocha"
--vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
--vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
--vim.cmd("highlight Pmenu ctermbg=black guibg=black")
--vim.cmd("highlight clear LineNr")
--vim.cmd("highlight clear SignColumn")
-- rounded corner
local wilder = require("wilder")
local gradient = {
  '#f4468f', '#fd4a85', '#ff507a', '#ff566f', '#ff5e63',
  '#ff6658', '#ff704e', '#ff7a45', '#ff843d', '#ff9036',
  '#f89b31', '#efa72f', '#e6b32e', '#dcbe30', '#d2c934',
  '#c8d43a', '#bfde43', '#b6e84e', '#aff05b'
}
for i, fg in ipairs(gradient) do
  gradient[i] = wilder.make_hl('WilderGradient' .. i, 'Pmenu', {{a = 1}, {a = 1}, {foreground = fg}})
end
wilder.setup({modes = {':', '/', '?'}})
wilder.set_option('renderer', wilder.popupmenu_renderer(
wilder.popupmenu_border_theme({
    highlights = {
        gradient = gradient, -- must be set
        -- selected_gradient key can be set to apply gradient highlighting for the selected candidate.
    },
    highlighter = wilder.highlighter_with_gradient({
        wilder.lua_fzy_highlighter(),
        -- wilder.basic_highlighter(), -- or wilder.lua_fzy_highlighter(),
    }),
    -- 'single', 'double', 'rounded' or 'solid'
    -- can also be a list of 8 characters, see :h wilder#popupmenu_border_theme() for more details
    border = 'rounded',
})
))
-- completion engine
local cmp = require("cmp")
local luasnip = require("luasnip")
local lspkind = require("lspkind")
cmp.setup {
    window = {
        documentation = {
            border = "rounded",
            winhighlight = 'Normal:CmpPmenu,FloatBorder:CmpPmenuBorder,CursorLine:PmenuSel,Search:None',
        },
        completion = {
            border = "rounded",
            winhighlight = 'Normal:CmpPmenu,FloatBorder:CmpPmenuBorder,CursorLine:PmenuSel,Search:None',
        }
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
    mapping = {
        -- move up and down the list
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<Up>'] = cmp.mapping.select_prev_item(),
        ['<Down>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-k>'] = cmp.mapping.select_prev_item(),
        ['<C-j>'] = cmp.mapping.select_next_item(),
        -- Scroll text in the documentation window
        --['<Up>'] = cmp.mapping.scroll_docs(-4),
        --['<Down>'] = cmp.mapping.scroll_docs(4),
        -- Cancel completion
        ['<C-e>'] = cmp.mapping.abort(),
        -- Confirm selection
        ['<C-y>'] = cmp.mapping.confirm({select = true}),
        ['<CR>'] = cmp.mapping.confirm({select = false}),
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
    },
    --formatting = {
    --    format = lspkind.cmp_format {
    --        with_text = true,
    --        menu = {
    --            buffer   = "[buf]",
    --            nvim_lsp = "[LSP]",
    --            luasnip  = "[SNP]",
    --            path     = "[path]",
    --        },
    --    },
    --},
}
-- unfocus pane/window
-- require("tint").setup()

-- git sign on the left
require('gitsigns').setup()

-- pop-up float terminal
require'FTerm'.setup({
    border = 'rounded',
    auto_close = true,
})
vim.keymap.set('n', '<C-i>', '<CMD>lua require("FTerm").toggle()<CR>')
vim.keymap.set('t', '<C-i>', '<C-\\><C-n><CMD>lua require("FTerm").toggle()<CR>')

-- file browser
vim.keymap.set("n", "<space>fb", function()
	require("telescope").extensions.file_browser.file_browser()
end)
