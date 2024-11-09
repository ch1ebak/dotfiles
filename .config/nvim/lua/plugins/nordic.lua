return {
    'AlexvZyl/nordic.nvim',
    lazy = false,
    priority = 1000,
    config = function()
        require('nordic').setup({
            bold_keywords = false,
            italic_comments = true,
            transparent = {
                bg = true,
                float = true,
            },
            bright_border = false,
            reduced_blue = true,
            swap_backgrounds = false,
            cursorline = {
                bold = false,
                bold_number = true,
                theme = 'dark',
                blend = 0.85,
            },
            noice = {
                style = 'classic',
            },
            telescope = {
                style = 'classic',
            },
            leap = {
                dim_backdrop = false,
            },
            ts_context = {
                dark_background = true,
            }
        })
    end
}
