return {
    {
        "EdenEast/nightfox.nvim",
        priority = 1000,
        lazy = false,
        opts = {
            palettes = {
                dawnfox = {
                    bg = "#ffffff",
                },
            },
            groups = {
                all = {
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

                    -- CmpItemMenu              = { fg = "palette.fg0", bg = "palette.bg0" },
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
            vim.cmd.colorscheme("carbonfox")
        end,
    }
}
