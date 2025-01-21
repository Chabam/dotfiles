return {
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "sindrets/diffview.nvim",

            "nvim-telescope/telescope.nvim",
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
