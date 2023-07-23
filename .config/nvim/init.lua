local builtin = require('telescope.builtin')

-- sets
vim.g.mapleader = " "
vim.o.number = true
vim.o.relativenumber = true
vim.o.tabstop = 4 -- TODO
vim.o.softtabstop = 4 -- TODO
vim.o.shiftwidth = 4 -- TODO
vim.o.hlsearch = false
vim.o.signcolumn = "yes"
vim.o.updatetime = 200
vim.o.timeout = true
vim.o.timeoutlen = 200
vim.o.hidden = true
vim.o.undofile = true

vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "<c-d>", "<c-d>zz")
vim.keymap.set("n", "<c-u>", "<c-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

vim.keymap.set("i", "<c-c>", "<esc>")
vim.keymap.set("i", "<c-x>", "<esc>")
vim.keymap.set("t", "<c-x>", "<c-\\><c-n>")
vim.keymap.set("i", "jk", "<esc>")
vim.keymap.set("t", "jk", "<c-\\><c-n>")

vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("v", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>d", "\"_d")
vim.keymap.set("v", "<leader>d", "\"_d")
vim.keymap.set("v", "<leader>p", "\"0p")

vim.keymap.set("n", "<leader><s-f>", "ggVG=<c-o>")
vim.keymap.set("n", "<leader>fp", ":e $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>hrc", ":so $MYVIMRC<CR>")
vim.keymap.set("n", "<leader>bl", "<c-6>")
vim.keymap.set("n", "<leader>hrr", ":so $MYVIMRC<CR>:PackerSync<CR>")

-- colorscheme
require('ayu').setup({
	mirage = true
})
vim.cmd.colorscheme('ayu')

-- packer
require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'
	use 'Shatur/neovim-ayu'
	use {
		'nvim-telescope/telescope.nvim', tag = '0.1.2',
		requires = { {'nvim-lua/plenary.nvim'} }
	}
	use('nvim-treesitter/nvim-treesitter', {run = ':TSUpdate'})
	use {
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		requires = {
			{'neovim/nvim-lspconfig'},
			{
				'williamboman/mason.nvim',
				run = function()
					pcall(vim.cmd, 'MasonUpdate')
				end,
			},
			{'williamboman/mason-lspconfig.nvim'},
			{'hrsh7th/nvim-cmp'},
			{'hrsh7th/cmp-nvim-lsp'},
			{'L3MON4D3/LuaSnip'},
		}

	}
	use "folke/which-key.nvim"
	use {"akinsho/toggleterm.nvim", tag = '*' }
	use 'tpope/vim-fugitive'
	use 'mbbill/undotree'
	use {
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		}
	}
end)

-- lsp
local lsp = require('lsp-zero')
lsp.preset('recommended')
lsp.setup()
local cmp = require('cmp')
require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

cmp.setup({
	mapping = {
		['<C-j>'] = cmp.mapping.select_next_item(cmp_select),
		['<C-k>'] = cmp.mapping.select_prev_item(cmp_select),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
		['<C-Space>'] = cmp.mapping.complete(),
	}
})

-- which-key
require("which-key")

-- treesitter
require('nvim-treesitter.configs').setup {
	sync_install = false,
	auto_install = true,

	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
}

-- telescope
vim.keymap.set("n", "<c-p>", builtin.find_files)
vim.keymap.set("n", "<leader><leader>", builtin.find_files)
vim.keymap.set("n", "<leader>hk", builtin.keymaps)

-- fugitive
vim.keymap.set("n", "<leader>gg", vim.cmd.Git)

-- toggleterm
require('toggleterm').setup({ persist_size = false })
vim.keymap.set("n", "<M-t>", ":ToggleTerm size=200 9<CR>")
vim.keymap.set("t", "<M-t>", "<c-\\><c-n>:ToggleTerm 9<CR>")
vim.keymap.set("n", "<M-1>", ":ToggleTerm 1<CR>")
vim.keymap.set("t", "<M-1>", "<c-\\><c-n>:ToggleTerm 1<CR>")
vim.keymap.set("n", "<M-2>", ":ToggleTerm 2<CR>")
vim.keymap.set("t", "<M-2>", "<c-\\><c-n>:ToggleTerm 2<CR>")
vim.keymap.set("n", "<M-3>", ":ToggleTerm 3<CR>")
vim.keymap.set("t", "<M-3>", "<c-\\><c-n>:ToggleTerm 3<CR>")

-- undotree
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)

-- neotree
vim.keymap.set("n", "<leader>op", ":Neotree toggle<CR>")
