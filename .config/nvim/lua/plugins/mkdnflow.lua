-- Fluent navigation and management of markdown notebooks 
return {
  'jakewvincent/mkdnflow.nvim',
  config = function()
      require('mkdnflow').setup({
        mappings = {
          MkdnUpdateNumbering = {'n', '<leader>mn'},
          MkdnCreateLinkFromClipboard = {'n', '<leader>ml'},
        }
      })
  end
}
