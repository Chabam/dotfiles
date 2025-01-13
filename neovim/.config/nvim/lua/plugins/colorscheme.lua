return {
    {
        "EdenEast/nightfox.nvim",
        priority = 1000,
        lazy = false,
        opts = {
            options = {
                terminal_colors = false,
                inverse = {
                    match_paren = true,
                    visual = true,
                    search = true,
                }
            },
            palettes = {
                dawnfox = {
                    bg1 = "#ffffff",
                },
                carbonfox = {
                    bg1 = "#1e1e1e",
                },
            },
            groups = {
                all = {

                    Visual                   = { bg = "palette.sel1", style = "reverse" },

                    TelescopeBorder          = { fg = "palette.bg0", bg = "palette.bg0" },
                    TelescopePromptBorder    = { fg = "palette.bg2", bg = "palette.bg2" },
                    TelescopePromptNormal    = { fg = "palette.fg1", bg = "palette.bg2" },
                    TelescopePromptPrefix    = { fg = "palette.green.bright", bg = "palette.bg2" },
                    TelescopeNormal          = { fg = "palette.fg1", bg = "palette.bg0" },
                    TelescopePreviewTitle    = { fg = "palette.bg0", bg = "palette.pink.bright" },
                    TelescopePromptTitle     = { fg = "palette.bg0", bg = "palette.green.bright" },
                    TelescopeResultsTitle    = { fg = "NONE", bg = "palette.blue.bright" },
                    TelescopeSelection       = { fg = "palette.orange.bright", bg = "NONE" },
                    TelescopePreviewLine     = { fg = "palette.orange.bright", bg = "NONE" },
                    TelescopeMatching        = { fg = "palette.red.bright", bg = "NONE" },

                    Pmenu                    = { fg = "palette.fg1", bg = "palette.bg2" },
                    PmenuSel                 = { fg = "palette.orange.bright", bg = "palette.bg3" },
                    CmpDocumentation         = { fg = "palette.fg1", bg = "palette.bg0" },
                    CmpDocumentationBorder   = { fg = "palette.bg0", bg = "palette.bg0" },

                    CmpItemAbbr              = { fg = "palette.fg1" },
                    CmpItemAbbrDeprecated    = { fg = "syntax.dep", style = "strikethrough" },
                    CmpItemAbbrMatch         = { fg = "palette.red.bright", },
                    CmpItemAbbrMatchFuzzy    = { fg = "palette.red.bright", },

                    CmpItemKindDefault       = { fg = "palette.fg2" },

                    CmpItemKindKeyword       = { fg = "palette.bg0", bg = "palette.fg1" },

                    CmpItemKindVariable      = { fg = "palette.bg0", bg = "syntax.variable" },
                    CmpItemKindConstant      = { fg = "palette.bg0", bg = "syntax.const" },
                    CmpItemKindReference     = { fg = "palette.bg0", bg = "syntax.keyword" },
                    CmpItemKindValue         = { fg = "palette.bg0", bg = "syntax.keyword" },

                    CmpItemKindFunction      = { fg = "palette.bg0", bg = "syntax.func" },
                    CmpItemKindMethod        = { fg = "palette.bg0", bg = "syntax.func" },
                    CmpItemKindConstructor   = { fg = "palette.bg0", bg = "syntax.func" },

                    CmpItemKindInterface     = { fg = "palette.bg0", bg = "syntax.const" },
                    CmpItemKindEvent         = { fg = "palette.bg0", bg = "syntax.const" },
                    CmpItemKindEnum          = { fg = "palette.bg0", bg = "syntax.const" },
                    CmpItemKindUnit          = { fg = "palette.bg0", bg = "syntax.const" },

                    CmpItemKindClass         = { fg = "palette.bg0", bg = "syntax.type" },
                    CmpItemKindStruct        = { fg = "palette.bg0", bg = "syntax.type" },

                    CmpItemKindModule        = { fg = "palette.bg0", bg = "syntax.ident" },

                    CmpItemKindProperty      = { fg = "palette.bg0", bg = "syntax.ident" },
                    CmpItemKindField         = { fg = "palette.bg0", bg = "syntax.ident" },
                    CmpItemKindTypeParameter = { fg = "palette.bg0", bg = "syntax.ident" },
                    CmpItemKindEnumMember    = { fg = "palette.bg0", bg = "syntax.ident" },
                    CmpItemKindOperator      = { fg = "palette.bg0", bg = "syntax.operator" },
                    CmpItemKindSnippet       = { fg = "palette.fg2" },
                },
            },
            inverse = {
                match_paren = true,
            },
        },
        init = function()
            vim.g.terminal_color_0  = '#241f31'
            vim.g.terminal_color_1  = '#c01c28'
            vim.g.terminal_color_2  = '#2ec27e'
            vim.g.terminal_color_3  = '#f5c211'
            vim.g.terminal_color_4  = '#1e78e4'
            vim.g.terminal_color_5  = '#9841bb'
            vim.g.terminal_color_6  = '#0ab9dc'
            vim.g.terminal_color_7  = '#c0bfbc'
            vim.g.terminal_color_8  = '#5e5c64'
            vim.g.terminal_color_9  = '#ed333b'
            vim.g.terminal_color_10 = '#57e389'
            vim.g.terminal_color_11 = '#f8e45c'
            vim.g.terminal_color_12 = '#51a1ff'
            vim.g.terminal_color_13 = '#c061cb'
            vim.g.terminal_color_14 = '#4fd2fd'
            vim.g.terminal_color_15 = '#f6f5f4'

            vim.cmd.colorscheme("carbonfox")
        end,
    }
}
