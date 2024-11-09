return {
    'serenevoid/kiwi.nvim',
    dependencies = {
        "nvim-lua/plenary.nvim"
    },
    opts = {
        {
            name = "default",
            path = "/home/kd/Dokumenty/notatki"
        }
    },
    keys = {
        { "<leader>nn", ":lua require(\"kiwi\").open_wiki_index()<cr>", desc = "Open Wiki index" }
    },
    lazy = true
}
