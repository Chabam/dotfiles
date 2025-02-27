return {
    "nvim-lualine/lualine.nvim",
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
        options = {
            section_separators = '',
            component_separators = '',
        },
        sections = {
            lualine_a = { 'mode' },
            lualine_b = { 'branch', 'diff', 'diagnostics' },
            lualine_c = { 'filesize' },
            lualine_x = { 'encoding', 'fileformat', 'filetype' },
            lualine_y = { 'progress' },
            lualine_z = { 'location' }
        },
        inactive_sections = {
            lualine_c = { { 'filename', path = 1 } },
        },
        tabline = {
            lualine_a = { { 'filename', path = 1, shorting_target = 0 } },
            lualine_y = { 'searchcount', 'selectioncount' },
            lualine_z = { { 'tabs', mode = 2 }, }
        },
    },
}
