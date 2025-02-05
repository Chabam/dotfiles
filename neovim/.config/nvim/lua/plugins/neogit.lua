return {
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "sindrets/diffview.nvim",
            "ibhagwan/fzf-lua",
        },
        config = true,
        init = function()
            local neogit = require("neogit")

            vim.keymap.set("n", "<leader>g", neogit.open, { desc = "Open Neo[G]it" })
            neogit.setup({
                kind = "auto",
                integrations = {
                    diffview = true,
                },
            })
        end
    }
}
