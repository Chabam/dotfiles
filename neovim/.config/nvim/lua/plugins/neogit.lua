return {
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "sindrets/diffview.nvim",
            "ibhagwan/fzf-lua",
        },
        config = true,
        opts = {
            kind = "auto",
            integrations = {
                diffview = true,
            },
        }
    }
}
