return {
    {
        "EdenEast/nightfox.nvim",
        priority = 1000,
        lazy = false,
        config = function()
            require("nightfox").setup({
                options = {
                    terminal_colors = false,
                    inverse = {
                        match_paren = true,
                    },
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
                    carbonfox = {
                        Visual = { bg = "#2C394E" },
                        CurSearch = { bg = "#5F84C4" },
                    },
                    dawnfox = {
                        Visual = { bg = "#DDEAFF" },
                        CurSearch = { bg = "#9ABFFF" },
                    },
                    all = {
                        Search = { link = "Visual" },
                        IncSearch = { link = "CurSearch" },

                        Substitute = { link = "Visual" },
                        FloatBorder = { fg = "palette.bg0", bg = "palette.bg0" },

                        Pmenu = { fg = "palette.comment", bg = "palette.bg2" },
                        PmenuSel = { bg = "palette.bg3", style = "bold" },
                        PmenuSbar = { bg = "palette.sel1" },
                        PmenuThumb = { bg = "palette.fg3" },

                        FzfLuaNormal = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaPreviewNormal = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaDirPart = { fg = "palette.magenta" },
                        FzfLuaFilePart = { fg = "palette.fg1" },
                        FzfLuaLivePrompt = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaBorder = { fg = "palette.bg0", bg = "palette.bg0" },
                        FzfLuaTitle = { fg = "palette.bg0", bg = "palette.green.bright" },
                        FzfLuaPreviewTitle = { fg = "palette.bg0", bg = "palette.pink" },
                        FzfLuaCursor = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaCursorLineNr = { fg = "palette.red", bg = "palette.bg0" },
                        FzfLuaCursorLine = { bg = "palette.bg1" },
                        FzfLuaScrollBorderEmpty = { bg = "palette.bg1" },
                        FzfLuaScrollBorderFull = { bg = "palette.bg2" },
                        FzfLuaBufNr = { fg = "palette.green" },
                        FzfLuaBufLineNr = { fg = "palette.blue" },
                        FzfLuaBufFlagCur = { fg = "palette.pink" },

                        FzfLuaFzfMatch = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaFzfHeader = { fg = "palette.fg1", bg = "palette.bg0" },
                        FzfLuaFzfGutter = { bg = "palette.bg0" },
                        FzfLuaFzfNormal = { fg = "palette.fg1" },
                        FzfLuaFzfQuery = { fg = "palette.fg1" },
                        FzfLuaFzfMarker = { fg = "palette.pink" },
                        FzfLuaFzfPrompt = { fg = "palette.pink" },
                        FzfLuaHeaderBind = { fg = "palette.blue" },

                        BlinkCmpDoc = { fg = "palette.fg1", bg = "palette.bg0" },
                        BlinkDocBorder = { fg = "palette.bg2", bg = "palette.bg0" },
                        BlinkCmpAbbr = { fg = "palette.comment" },
                        BlinkCmpAbbrDeprecated = { fg = "syntax.dep", style = "strikethrough" },
                        BlinkCmpAbbrMatch = { fg = "palette.fg1" },
                        BlinkCmpAbbrMatchFuzzy = { fg = "palette.fg1" },

                        BlinkCmpKind = { link = "CmpItemKindFunction" },
                        BlinkCmpKindDefault = { fg = "palette.fg2" },

                        BlinkCmpKindKeyword = { fg = "palette.fg1" },

                        BlinkCmpKindVariable = { fg = "syntax.variable" },
                        BlinkCmpKindConstant = { fg = "syntax.const" },
                        BlinkCmpKindReference = { fg = "syntax.keyword" },
                        BlinkCmpKindValue = { fg = "syntax.keyword" },

                        BlinkCmpKindFunction = { fg = "syntax.func" },
                        BlinkCmpKindMethod = { fg = "syntax.func" },
                        BlinkCmpKindConstructor = { fg = "syntax.func" },

                        BlinkCmpKindInterface = { fg = "syntax.const" },
                        BlinkCmpKindEvent = { fg = "syntax.const" },
                        BlinkCmpKindEnum = { fg = "syntax.const" },
                        BlinkCmpKindUnit = { fg = "syntax.const" },

                        BlinkCmpKindClass = { fg = "syntax.type" },
                        BlinkCmpKindStruct = { fg = "syntax.type" },

                        BlinkCmpKindModule = { fg = "syntax.ident" },

                        BlinkCmpKindProperty = { fg = "syntax.ident" },
                        BlinkCmpKindField = { fg = "syntax.ident" },
                        BlinkCmpKindTypeParameter = { fg = "syntax.ident" },
                        BlinkCmpKindEnumMember = { fg = "syntax.ident" },
                        BlinkCmpKindOperator = { fg = "syntax.operator" },
                        BlinkCmpKindSnippet = { fg = "palette.fg2" },

                        LspReferenceText = { link = "Visual" },
                        LspReferenceRead = { link = "LspReferenceText" },
                        LspReferenceWrite = { link = "LspReferenceText" },

                        TreesitterContext = { bg = "palette.bg3" },
                        TreesitterContextLineNumberBottom = { bg = "palette.bg1" },
                    },
                },
                inverse = {
                    match_paren = true,
                },
            })
        end,
        init = function()
            vim.g.terminal_color_0 = "#241f31"
            vim.g.terminal_color_1 = "#c01c28"
            vim.g.terminal_color_2 = "#2ec27e"
            vim.g.terminal_color_3 = "#f5c211"
            vim.g.terminal_color_4 = "#1e78e4"
            vim.g.terminal_color_5 = "#9841bb"
            vim.g.terminal_color_6 = "#0ab9dc"
            vim.g.terminal_color_7 = "#c0bfbc"
            vim.g.terminal_color_8 = "#5e5c64"
            vim.g.terminal_color_9 = "#ed333b"
            vim.g.terminal_color_10 = "#57e389"
            vim.g.terminal_color_11 = "#f8e45c"
            vim.g.terminal_color_12 = "#51a1ff"
            vim.g.terminal_color_13 = "#c061cb"
            vim.g.terminal_color_14 = "#4fd2fd"
            vim.g.terminal_color_15 = "#f6f5f4"
        end,
    },
}
