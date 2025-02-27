return {
    {
        "stevearc/oil.nvim",
        lazy = true,
        init = function()
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
                    signcolumn = "yes:2",
                },
                buf_options = {
                    buflisted = true
                }
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
