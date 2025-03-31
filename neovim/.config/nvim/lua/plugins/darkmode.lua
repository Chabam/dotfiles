return {
    "f-person/auto-dark-mode.nvim",
    opts = {
        set_dark_mode = function()
            vim.cmd.colorscheme("carbonfox")
        end,
        set_light_mode = function()
            vim.cmd.colorscheme("dawnfox")
        end,
        update_interval = -1,
        fallback = "dark"
    }
}
