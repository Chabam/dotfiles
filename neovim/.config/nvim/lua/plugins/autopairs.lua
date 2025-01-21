return {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true,
    init = function ()
        local autopairs = require("nvim-autopairs")
        autopairs.setup({})

        autopairs.get_rules("'")[1].not_filetypes = { "scheme", "lisp", "racket" }
    end
}
