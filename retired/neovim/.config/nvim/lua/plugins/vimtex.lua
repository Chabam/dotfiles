return {
    "lervag/vimtex",
    lazy = false, -- we don't want to lazy load VimTeX
    init = function()
        vim.g.vimtex_compiler_method = "latexmk"
        vim.g.vimtex_compiler_latexmk = {
            out_dir = "ignore",
            aux_dir = "ignore",
        }
        vim.g.vimtex_log_ignore = {
            "Underfull",
            "Overfull",
        }
    end,
}
