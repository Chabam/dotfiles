return {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
        local fzf = require("fzf-lua")
        fzf.setup({ { "borderless-full" } })

        -- vim.keymap.set("n", "<leader>sh", fzf.help_tags, { desc = "[S]earch [H]elp" })
        -- vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[S]earch [K]eymaps" })
        vim.keymap.set("n", "<leader>sf", fzf.files, { desc = "[S]earch [F]iles" })
        -- vim.keymap.set("n", "<leader>ss", builtin.builtin, { desc = "[S]earch [S]elect Telescope" })
        -- vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
        vim.keymap.set("n", "<leader>sg", fzf.live_grep, { desc = "[S]earch by [G]rep" })
        -- vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
        -- vim.keymap.set("n", "<leader>sr", builtin.resume, { desc = "[S]earch [R]esume" })
        -- vim.keymap.set("n", "<leader>s.", builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
        vim.keymap.set("n", "<leader>sb", fzf.buffers, { desc = "[S]earch existing [B]uffers" })
        --
        -- vim.keymap.set("n", "<leader>/", function()
        --     builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
        --     }))
        -- end, { desc = "[/] Fuzzily search in current buffer" })
        vim.keymap.set("n", "<leader>s/", function()
            fzf.live_grep({
                grep_open_files = true,
                prompt_title = "Live Grep in Open Files",
            })
        end, { desc = "[S]earch [/] in Open Files" })

        vim.keymap.set("n", "<leader>sn", function()
            fzf.files({ cwd = vim.fn.stdpath("config") })
        end, { desc = "[S]earch [N]eovim files" })
    end
}

-- return {
--     "nvim-telescope/telescope.nvim",
--     event = "VimEnter",
--     branch = "0.1.x",
--     dependencies = {
--         "nvim-lua/plenary.nvim",
--         {
--             "nvim-telescope/telescope-fzf-native.nvim",
--             build = "make",
--             cond = function()
--                 return vim.fn.executable("make") == 1
--             end,
--         },
--         { "nvim-telescope/telescope-ui-select.nvim" },
--         { "nvim-tree/nvim-web-devicons",            enabled = vim.g.have_nerd_font },
--     },
--     config = function()
--         require("telescope").setup({
--             extensions = {
--                 ["ui-select"] = {
--                     require("telescope.themes").get_dropdown(),
--                 },
--                 fzf = {
--                     fuzzy = true,
--                     override_generic_sorter = true,
--                     override_file_sorter = true,
--                     case_mode = "smart_case",
--                 }
--             },
--             defaults = {
--                 file_ignore_patterns = { 'node_modules', '.git', '.venv' },
--             },
--             pickers = {
--                 find_files = {
--                     hidden = true,
--                     no_ignore = true,
--                 },
--                 live_grep = {
--                     additional_args = {"--no-ignore"}
--                 }
--             }
--         })
--
--         pcall(require("telescope").load_extension, "fzf")
--         pcall(require("telescope").load_extension, "ui-select")
--
--         local builtin = require("telescope.builtin")
--         local actions = require("telescope.actions")
--
--         local buffer_searcher = function()
--             builtin.buffers({
--                 attach_mappings = function(_, map)
--                     map("n", "<C-d>", actions.delete_buffer)
--                     map("i", "<C-d>", actions.delete_buffer)
--
--                     return true
--                 end,
--             })
--         end
--
--         vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[S]earch [H]elp" })
--         vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[S]earch [K]eymaps" })
--         vim.keymap.set("n", "<leader>sf", builtin.find_files, { desc = "[S]earch [F]iles" })
--         vim.keymap.set("n", "<leader>ss", builtin.builtin, { desc = "[S]earch [S]elect Telescope" })
--         vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
--         vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[S]earch by [G]rep" })
--         vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
--         vim.keymap.set("n", "<leader>sr", builtin.resume, { desc = "[S]earch [R]esume" })
--         vim.keymap.set("n", "<leader>s.", builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
--         vim.keymap.set("n", "<leader>sb", buffer_searcher, { desc = "[S]earch existing [B]uffers" })
--
--         vim.keymap.set("n", "<leader>/", function()
--             builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
--             }))
--         end, { desc = "[/] Fuzzily search in current buffer" })
--         vim.keymap.set("n", "<leader>s/", function()
--             builtin.live_grep({
--                 grep_open_files = true,
--                 prompt_title = "Live Grep in Open Files",
--             })
--         end, { desc = "[S]earch [/] in Open Files" })
--
--         vim.keymap.set("n", "<leader>sn", function()
--             builtin.find_files({ cwd = vim.fn.stdpath("config") })
--         end, { desc = "[S]earch [N]eovim files" })
--     end,
-- }
