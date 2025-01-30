return {
    { -- LSP Plugins
        "folke/lazydev.nvim",
        ft = "lua",
        opts = {},
    },
    {
        "ray-x/lsp_signature.nvim",
        event = "VeryLazy",
        opts = {
            hint_enable = false,
        },
        config = function(_, opts) require 'lsp_signature'.setup(opts) end
    },
    { -- LSP
        "neovim/nvim-lspconfig",
        dependencies = {
            { "williamboman/mason.nvim", config = true },
            "williamboman/mason-lspconfig.nvim",
            "WhoIsSethDaniel/mason-tool-installer.nvim",
            {
                "j-hui/fidget.nvim",
                opts = {
                    notification = {
                        window = { winblend = 0 }
                    }
                }
            },
            "hrsh7th/cmp-nvim-lsp",
        },
        config = function()
            vim.api.nvim_create_autocmd("LspAttach", {
                group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
                callback = function(event)
                    local map = function(keys, func, desc, mode)
                        mode = mode or "n"
                        vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
                    end
                    local fzf = require("fzf-lua")

                    map("gd", fzf.lsp_definitions, "[G]oto [D]efinition")
                    map("gr", fzf.lsp_references, "[G]oto [R]eferences")
                    map("gI", fzf.lsp_implementations, "[G]oto [I]mplementation")
                    map("<leader>D", fzf.lsp_typedefs, "Type [D]efinition")
                    map("<leader>ds", fzf.lsp_document_symbols, "[D]ocument [S]ymbols")
                    map("<leader>ws", fzf.lsp_workspace_symbols, "[W]orkspace [S]ymbols")

                    map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")

                    map("<leader>ca", require("fzf-lua").lsp_code_actions, "[C]ode [A]ction", { "n", "x" })

                    map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
                    map("gH", function()
                        vim.cmd("ClangdSwitchSourceHeader")
                    end, "[G]oto [H]eader", { "n", "x" })

                    local client = vim.lsp.get_client_by_id(event.data.client_id)
                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight) then
                        local highlight_augroup =
                            vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
                        vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                            buffer = event.buf,
                            group = highlight_augroup,
                            callback = vim.lsp.buf.document_highlight,
                        })

                        vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
                            buffer = event.buf,
                            group = highlight_augroup,
                            callback = vim.lsp.buf.clear_references,
                        })

                        vim.api.nvim_create_autocmd("LspDetach", {
                            group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
                            callback = function(event2)
                                vim.lsp.buf.clear_references()
                                vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event2.buf })
                            end,
                        })
                    end

                    if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
                        map("<leader>th", function()
                            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
                        end, "[T]oggle Inlay [H]ints")
                    end
                end,
            })

            local capabilities = vim.lsp.protocol.make_client_capabilities()
            capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())
            capabilities.textDocument.completion.completionItem.snippetSupport = false

            local servers = {
                clangd = {},
                ltex = {
                    settings = {
                        ltex = {
                            language = "fr",
                            completionEnabled = true,
                        },
                    },
                    filetypes = { "tex", "markdown" },
                    enabled = { "tex", "markdown" },
                },
                pyright = {
                    filetypes = { "python" },
                },
                neocmake = {
                    filetpes = { "cmake", "CMakeLists.txt" },
                },
                lua_ls = {
                    settings = {
                        Lua = {
                            completion = {
                                callSnippet = "Replace",
                            },
                        },
                    },
                    filetypes = { "lua" },
                },
            }

            require("mason").setup()
            require("mason-lspconfig").setup({
                automatic_installation = false,
                ensure_installed = {
                    "clangd",
                    "ltex",
                    "pyright",
                    "neocmake",
                    "lua_ls",
                },
                handlers = {
                    function(server_name)
                        local server = servers[server_name] or {}
                        server.capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {})
                        require("lspconfig")[server_name].setup(server)
                    end,
                },
            })
        end,
    }
}
