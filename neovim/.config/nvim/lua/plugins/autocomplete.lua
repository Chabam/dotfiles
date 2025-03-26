return {
    {
        "saghen/blink.cmp",
        version = "1.*",
        opts = {
            keymap = { preset = "enter" },
            appearance = {
                nerd_font_variant = "mono"
            },
            snippets = { preset = "luasnip" },
            sources = {
                default = { "lsp", "path", "snippets", "buffer" },
            },
            fuzzy = { implementation = "prefer_rust_with_warning" },
            signature = { enabled = true }
        },
        opts_extend = { "sources.default" }
    }
}
