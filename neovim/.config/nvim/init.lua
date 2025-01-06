require("options")
require("open_terminal")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    error("Error cloning lazy.nvim:\n" .. out)
  end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    "tpope/vim-sleuth",
    "machakann/vim-sandwich",
    "mg979/vim-visual-multi",
    "HiPhish/rainbow-delimiters.nvim",
    "NeogitOrg/neogit",
    {
      "folke/todo-comments.nvim",
      event = "VimEnter",
      dependencies = { "nvim-lua/plenary.nvim" },
      opts = { signs = false },
    },
    require("plugins.colorscheme"),
    require("plugins.gitsigns"),
    require("plugins.autocomplete"),
    require("plugins.autoformat"),
    require("plugins.gitsigns"),
    require("plugins.indent_line"),
    require("plugins.lsp"),
    require("plugins.lualine"),
    require("plugins.oil"),
    require("plugins.telescope"),
    require("plugins.treesitter"),
    require("plugins.vimtex"),
    require("plugins.which"),
    require("plugins.debug"),
  },
  {
    install = {
      colorscheme = {
        'carbonfox'
      }
    }
  })
