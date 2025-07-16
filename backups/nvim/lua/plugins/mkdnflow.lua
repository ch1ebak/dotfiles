return {
  'jakewvincent/mkdnflow.nvim',
  config = function()
      require('mkdnflow').setup({
        mappings = {
          MkdnUpdateNumbering = {'n', '<leader>mn'},
        }
      })
  end
}
