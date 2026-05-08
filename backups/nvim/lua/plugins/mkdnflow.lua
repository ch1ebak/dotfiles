return {
  'jakewvincent/mkdnflow.nvim',
  config = function()
    require('mkdnflow').setup({})
      mappings = {
        MkdnTableNextCell = { 'i', '<Tab>' },
        MkdnTablePrevCell = { 'i', '<S-Tab>' },
        MkdnTableNextRow = false,
        MkdnTablePrevRow = { 'i', '<M-CR>' },
        MkdnCreateLinkFromClipboard = {
            'n',
            'ml',
        },
        MkdnDecreaseHeading = {
            'n',
            'mL',
        },
        MkdnFollowLink = {
            'n',
            'mo',
        },
        MkdnIncreaseHeading = {
            'n',
            'mH',
        },
        MkdnTableNewColAfter = {
            'n',
            'tl',
        },
        MkdnTableNewColBefore = {
            'n',
            'th',
        },
        MkdnTableNewRowAbove = {
            'n',
            'tk',
        },
        MkdnTableNewRowBelow = {
            'n',
            'tj',
        },
        MkdnToggleToDo = {
            {
                'n',
                'v',
            },
            'mc',
        },
        MkdnUpdateNumbering = false,
      }
  end
}
