return {
  'jakewvincent/mkdnflow.nvim',
  config = function()
    require('mkdnflow').setup({
      modules = {
          bib = true,
          buffers = true,
          conceal = false,
          cursor = true,
          folds = false,
          foldtext = true,
          links = true,
          lists = true,
          maps = true,
          paths = true,
          tables = true,
          yaml = false,
          cmp = false
      },
      mappings = {
        MkdnEnter = {{'n', 'v'}, '<CR>'},
        MkdnFollowLink = {'n', 'mo'},
        MkdnToggleToDo = {{'n', 'v'}, 'mc'},
        MkdnUpdateNumbering = false,
        MkdnIncreaseHeading = {'n', 'mH'},
        MkdnDecreaseHeading = {'n', 'mL'},
        MkdnCreateLinkFromClipboard = {'n', 'ml'},
        MkdnTableNewRowBelow = {'n', 'tj'},
        MkdnTableNewRowAbove = {'n', 'tk'},
        MkdnTableNewColAfter = {'n', 'tl'},
        MkdnTableNewColBefore = {'n', 'th'},
      }
    })
  end
}
