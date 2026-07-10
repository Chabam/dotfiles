return {
	"ibhagwan/fzf-lua",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		local fzf = require("fzf-lua")
		fzf.setup({
			fzf_opts = {
				["--pointer"] = " ",
				["--separator"] = " ",
			},
			fzf_colors = {
				["fg"] = { "fg", "Comment" },
				["bg"] = { "bg", "palette.bg0" },
				["hl"] = { "fg", "Normal" },
				["fg+"] = { "fg", "Comment", "bold" },
				["bg+"] = { "bg", { "palette.sel0", "Normal" } },
				["hl+"] = { "fg", "Normal" },
				["info"] = { "fg", "NonText" },
				["prompt"] = { "fg", "FzfLuaBufLineNr" },
				["spinner"] = { "fg", "FzfLuaBufNr" },
				["header"] = { "fg", "Normal" },
				["query"] = { "fg", "palette.green" },
				["gutter"] = "-1",
			},
			winopts = {
				backdrop = false,
				height = 0.7,
				width = 0.65,
				preview = {
					layout = "vertical",
				},
			},
		})
		fzf.register_ui_select()

		vim.keymap.set("n", "<leader>sh", fzf.helptags, { desc = "[S]earch [H]elp" })
		vim.keymap.set("n", "<leader>sk", fzf.keymaps, { desc = "[S]earch [K]eymaps" })
		vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })
		vim.keymap.set("n", "<leader>ss", fzf.builtin, { desc = "[S]earch [S]elect fzf" })
		vim.keymap.set("n", "<leader>sw", fzf.grep_cword, { desc = "[S]earch current [W]ord" })
		vim.keymap.set("n", "<leader>sg", fzf.live_grep_native, { desc = "[S]earch by [G]rep" })
		vim.keymap.set("n", "<leader>sd", fzf.diagnostics_document, { desc = "[S]earch [D]iagnostics" })
		vim.keymap.set("n", "<leader>sr", fzf.resume, { desc = "[S]earch [R]esume" })
		vim.keymap.set("n", "<leader>s.", fzf.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
		vim.keymap.set("n", "<leader>sb", fzf.buffers, { desc = "[S]earch existing [B]uffers" })
		--
		vim.keymap.set("n", "<leader>/", fzf.lgrep_curbuf, { desc = "[/] Fuzzily search in current buffer" })
		vim.keymap.set("n", "<leader>s/", function()
			fzf.live_grep({
				grep_open_files = true,
				prompt_title = "Live Grep in Open Files",
			})
		end, { desc = "[S]earch [/] in Open Files" })

		vim.keymap.set("n", "<leader>sn", function()
			fzf.files({ cwd = vim.fn.stdpath("config") })
		end, { desc = "[S]earch [N]eovim files" })
	end,
}
