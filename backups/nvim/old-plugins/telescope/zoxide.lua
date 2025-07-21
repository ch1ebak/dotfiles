return {
  'jvgrootveld/telescope-zoxide',
  dependencies = { 
    'nvim-lua/popup.nvim',
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
  local t = require("telescope")
  local z_utils = require("telescope._extensions.zoxide.utils")
  t.setup({
    extensions = {
      zoxide = {
        prompt_title = "[ Zoxide ]",
        mappings = {
          default = {
            after_action = function(selection)
              print("Update to (" .. selection.z_score .. ") " .. selection.path)
            end
          },
          ["<C-s>"] = {
            before_action = function(selection) print("before C-s") end,
            action = function(selection)
              vim.cmd.edit(selection.path)
            end
          },
          ["<C-q>"] = { action = z_utils.create_basic_command("split") },
        },
      },
    },
  })
  t.load_extension('zoxide')
  vim.keymap.set("n", "<leader>.", t.extensions.zoxide.list)
end,
}
