return {
    {
        "stevearc/oil.nvim",
        lazy = true,
        init = function()
            function _G.get_oil_winbar()
                local dir = require("oil").get_current_dir()
                if dir then
                    return vim.fn.fnamemodify(dir, ":~")
                else
                    -- If there is no current directory (e.g. over ssh), just show the buffer name
                    return vim.api.nvim_buf_get_name(0)
                end
            end

            local detail = true
            require("oil").setup({
                watch_for_changes = true,
                keymaps = {
                    ["gd"] = {
                        desc = "Toggle file detail view",
                        callback = function()
                            detail = not detail
                            if detail then
                                require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
                            else
                                require("oil").set_columns({ "icon" })
                            end
                        end,
                    },
                },
                win_options = {
                    winbar = "%!v:lua.get_oil_winbar()",
                    signcolumn = "yes:2",
                },
            })
            vim.keymap.set("n", "<leader>-", "<CMD>Oil --float<CR>", { desc = "Open parent directory" })
            vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
        end,
        dependencies = { "nvim-tree/nvim-web-devicons" },
    },
    {
        "refractalize/oil-git-status.nvim",
        dependencies = {
            "stevearc/oil.nvim",
        },
        config = true,
    }
}
