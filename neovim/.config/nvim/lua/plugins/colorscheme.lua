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

                        CmpDocumentation = { fg = "palette.fg1", bg = "palette.bg0" },
                        CmpDocumentationBorder = { fg = "palette.bg2", bg = "palette.bg0" },
                        CmpItemAbbr = { fg = "palette.comment" },
                        CmpItemAbbrDeprecated = { fg = "syntax.dep", style = "strikethrough" },
                        CmpItemAbbrMatch = { fg = "palette.fg1" },
                        CmpItemAbbrMatchFuzzy = { fg = "palette.fg1" },

                        CmpItemKind = { link = "CmpItemKindFunction" },
                        CmpItemKindDefault = { fg = "palette.fg2" },

                        CmpItemKindKeyword = { fg = "palette.bg0", bg = "palette.fg1" },

                        CmpItemKindVariable = { fg = "palette.bg0", bg = "syntax.variable" },
                        CmpItemKindConstant = { fg = "palette.bg0", bg = "syntax.const" },
                        CmpItemKindReference = { fg = "palette.bg0", bg = "syntax.keyword" },
                        CmpItemKindValue = { fg = "palette.bg0", bg = "syntax.keyword" },

                        CmpItemKindFunction = { fg = "palette.bg0", bg = "syntax.func" },
                        CmpItemKindMethod = { fg = "palette.bg0", bg = "syntax.func" },
                        CmpItemKindConstructor = { fg = "palette.bg0", bg = "syntax.func" },

                        CmpItemKindInterface = { fg = "palette.bg0", bg = "syntax.const" },
                        CmpItemKindEvent = { fg = "palette.bg0", bg = "syntax.const" },
                        CmpItemKindEnum = { fg = "palette.bg0", bg = "syntax.const" },
                        CmpItemKindUnit = { fg = "palette.bg0", bg = "syntax.const" },

                        CmpItemKindClass = { fg = "palette.bg0", bg = "syntax.type" },
                        CmpItemKindStruct = { fg = "palette.bg0", bg = "syntax.type" },

                        CmpItemKindModule = { fg = "palette.bg0", bg = "syntax.ident" },

                        CmpItemKindProperty = { fg = "palette.bg0", bg = "syntax.ident" },
                        CmpItemKindField = { fg = "palette.bg0", bg = "syntax.ident" },
                        CmpItemKindTypeParameter = { fg = "palette.bg0", bg = "syntax.ident" },
                        CmpItemKindEnumMember = { fg = "palette.bg0", bg = "syntax.ident" },
                        CmpItemKindOperator = { fg = "palette.bg0", bg = "syntax.operator" },
                        CmpItemKindSnippet = { fg = "palette.fg2" },

                        LspReferenceText = { link = "Visual" },
                        LspReferenceRead = { link = "LspReferenceText" },
                        LspReferenceWrite = { link = "LspReferenceText" },

                        TreesitterContext = { bg = "palette.bg3" },
                        TreesitterContextLineNumberBottom = { bg = "palette.bg1" },

                        LspSignatureActiveParameter = { bg = "palette.sel1", fg = "NONE" }
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

			if
				vim.fn.has("macunix")
				and os.getenv("XDG_CURRENT_DESKTOP") == "GNOME"
				and os.execute("is-dark-mode.sh")
			then
				vim.cmd.colorscheme("dawnfox")
			else
				vim.cmd.colorscheme("carbonfox")
			end
		end,
	},
}
