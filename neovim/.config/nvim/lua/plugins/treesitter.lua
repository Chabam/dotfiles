return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    main = "nvim-treesitter.configs",
    opts = {
        highlight = {
            enable = true,
            disable = { "latex" },
        },
        indent = { enable = true },
    },
}
