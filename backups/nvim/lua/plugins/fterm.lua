return {
  'numToStr/FTerm.nvim',
  config = function ()
    require'FTerm'.setup({
        border = 'single',
        dimensions  = {
            height = 0.9,
            width = 0.9,
        },
    })
    vim.keymap.set('n', '<leader><Return>', '<CMD>lua require("FTerm").toggle()<CR>')
    vim.keymap.set('t', '<leader><Return>', '<C-\\><C-n><CMD>lua require("FTerm").toggle()<CR>')
  end
}
