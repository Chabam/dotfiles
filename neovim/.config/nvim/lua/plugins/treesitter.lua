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
    {
        "nvim-treesitter/nvim-treesitter-context",
        opts = {
            max_lines = 1,
            mode = "topline",
        },
    },
}
