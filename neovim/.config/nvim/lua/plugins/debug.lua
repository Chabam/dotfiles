return {
  {
    "theHamsta/nvim-dap-virtual-text",
    opts = {}
  },
  {
    'mfussenegger/nvim-dap',
    dependencies = {
      -- Creates a beautiful debugger UI
      'rcarriga/nvim-dap-ui',

      -- Required dependency for nvim-dap-ui
      'nvim-neotest/nvim-nio',

      -- Installs the debug adapters for you
      'williamboman/mason.nvim',
      'jay-babu/mason-nvim-dap.nvim',

      -- Add your own debuggers here
      'leoluz/nvim-dap-go',
    },
    keys = {
      -- Basic debugging keymaps, feel free to change to your liking!
      {
        '<F5>',
        function()
          require('dap').continue()
        end,
        desc = 'Debug: Start/Continue',
      },
      {
        '<F11>',
        function()
          require('dap').step_into()
        end,
        desc = 'Debug: Step Into',
      },
      {
        '<F10>',
        function()
          require('dap').step_over()
        end,
        desc = 'Debug: Step Over',
      },
      {
        'Shift+<F11>',
        function()
          require('dap').step_out()
        end,
        desc = 'Debug: Step Out',
      },
      {
        '<leader>b',
        function()
          require('dap').toggle_breakpoint()
        end,
        desc = 'Debug: Toggle Breakpoint',
      },
      {
        '<leader>B',
        function()
          require('dap').set_breakpoint(vim.fn.input 'Breakpoint condition: ')
        end,
        desc = 'Debug: Set Breakpoint',
      },
      -- Toggle to see last session result. Without this, you can't see session output in case of unhandled exception.
      {
        '<F7>',
        function()
          require('dapui').toggle()
        end,
        desc = 'Debug: See last session result.',
      },
    },
    config = function()
      local dap = require 'dap'
      local dapui = require 'dapui'

      require('mason-nvim-dap').setup {
        -- Makes a best effort to setup the various debuggers with
        -- reasonable debug configurations
        automatic_installation = true,

        -- You can provide additional configuration to the handlers,
        -- see mason-nvim-dap README for more information
        handlers = {
          cppdbg = function(config)
            config.configurations = {
              {
                name = 'Launch file',
                type = 'cppdbg',
                request = 'launch',
                program = function()
                  return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
                end,
                args = function ()
                  local args_string = vim.fn.input("Arguments: ")
                  return vim.split(args_string, " ")
                end,
                cwd = '${workspaceFolder}',
              },
              {
                name = 'Attach to process',
                program = '/usr/bin/gdb',
                type = 'cppdbg',
                request = 'attach',
                cwd = '${workspaceFolder}',
                processId = '${command:pickProcess}'
              }
            }
            require("mason-nvim-dap").default_setup(config)
          end
        },

        -- You'll need to check that you have the required things installed
        -- online, please don't ask me how to install them :)
        ensure_installed = {
          -- Update this to ensure that you have the debuggers for the langs you want
          "cpptools",
        },
      }

      -- Dap UI setup
      -- For more information, see |:help nvim-dap-ui|
      -- dapui.setup {
      --   -- Set icons to characters that are more likely to work in every terminal.
      --   --    Feel free to remove or use ones that you like more! :)
      --   --    Don't feel like these are good choices.
      --   icons = { expanded = '▾', collapsed = '▸', current_frame = '*' },
      --   controls = {
      --     icons = {
      --       pause = '⏸',
      --       play = '▶',
      --       step_into = '⏎',
      --       step_over = '⏭',
      --       step_out = '⏮',
      --       step_back = 'b',
      --       run_last = '▶▶',
      --       terminate = '⏹',
      --       disconnect = '⏏',
      --     },
      --   },
      -- }
      dapui.setup({})

      -- Change breakpoint icons
      vim.api.nvim_set_hl(0, 'dapbreak', { fg = '#e51400' })
      vim.api.nvim_set_hl(0, 'dapstop', { fg = '#ffcc00' })
      local breakpoint_icons = vim.g.have_nerd_font
          and { Breakpoint = '', BreakpointCondition = '', BreakpointRejected = '', LogPoint = '', Stopped = '' }
          or { Breakpoint = '●', BreakpointCondition = '⊜', BreakpointRejected = '⊘', LogPoint = '◆', Stopped = '⭔' }
      for type, icon in pairs(breakpoint_icons) do
        local tp = 'Dap' .. type
        local hl = (type == 'Stopped') and 'DapStop' or 'DapBreak'
        vim.fn.sign_define(tp, { text = icon, texthl = hl, numhl = hl })
      end
      --

      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close
    end,
  }
}
