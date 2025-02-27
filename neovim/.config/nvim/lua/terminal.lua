vim.api.nvim_create_autocmd({"TermOpen"},
  {
    pattern = "term://*",
    callback = function(opts)
      vim.cmd("startinsert")
      vim.cmd("setlocal nonumber")
      vim.cmd("setlocal norelativenumber")
      vim.cmd("setlocal signcolumn=no")
    end
  })

vim.keymap.set('t', '<esc><esc>', "<C-\\><C-n>") -- esc to exit insert mode

local term_clear = function()
  vim.api.nvim_command("startinsert")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<c-a><c-k>reset<cr>', true, false, true), 't', true)
  local sb = vim.bo.scrollback
  vim.bo.scrollback = 1
  vim.bo.scrollback = sb
end

vim.api.nvim_create_user_command('ClearTerminal', term_clear, {})
