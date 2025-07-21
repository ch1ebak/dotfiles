return {
  'magnusriga/markdown-tools.nvim',
  dependencies = { 'folke/snacks.nvim' },
  opts = {
    template_dir = vim.fn.expand("~/.config/nvim/templates"),
    picker = 'snacks',
    insert_frontmatter = true,
    -- Functions to generate frontmatter fields.
    -- These functions determine the values used when automatically inserting frontmatter
    -- AND when replacing placeholders in templates.
    -- Each function receives a table `opts` with:
    --   opts.timestamp (string): YYYYMMDDHHMM
    --   opts.filename (string): Full filename including .md extension.
    --   opts.sanitized_name (string): Filename sanitized for use in IDs.
    --   opts.filepath (string): Absolute path to the new file.
    -- Return nil from a function to omit that field from automatically generated frontmatter
    -- and replace its corresponding placeholder with an empty string.

    -- Corresponds to {{id}} placeholder
    frontmatter_id = function(opts)
      -- Default: YYYYMMDDHHMM_sanitized-filename
      return opts.timestamp .. "_" .. opts.sanitized_name
    end,

    -- Corresponds to {{title}} placeholder
    frontmatter_title = function(opts)
      -- Default: Filename without extension
      return vim.fn.fnamemodify(opts.filename, ":t:r")
    end,

    -- Corresponds to {{alias}} placeholder (expects a list/table of strings)
    frontmatter_alias = function(_opts)
      -- Default: Empty list
      return {}
    end,

    -- Corresponds to {{tags}} placeholder (expects a list/table of strings)
    frontmatter_tags = function(_opts)
      -- Default: Empty list
      return {}
    end,

    -- Corresponds to {{date}} placeholder
    frontmatter_date = function(_opts)
      -- Default: Current date YYYY-MM-DD
      return os.date("%Y-%m-%d")
    end,

    -- Define custom frontmatter fields and their generator functions.
    -- The key is the field name (used in frontmatter and as the placeholder {{key}}).
    -- The value is a function receiving the `opts` table.
    -- It can return a string, a table (list) of strings, or nil.
    frontmatter_custom = {},
    -- Example of how to define custom fields:
    -- frontmatter_custom = {
    --   status = function(_opts) return "draft" end,
    --   related = function(_opts) return {} end,
    -- },

    -- Keymappings for shortcuts. Set to `false` or `""` to disable.
    keymaps = {
      create_from_template = "<leader>mnt", -- New Template
      insert_header = "<leader>mH",        -- Header
      insert_code_block = "<leader>mc",    -- Code block
      insert_bold = "<leader>mb",        -- Bold
      insert_highlight = "<leader>mh",    -- Highlight
      insert_italic = "<leader>mi",      -- Italic
      insert_link = "<leader>ml",        -- Link
      insert_table = "<leader>mt",        -- Table
      insert_checkbox = "<leader>mk",    -- Checkbox
      toggle_checkbox = "<leader>mx",    -- Toggle Checkbox
      preview = "<leader>mp",          -- Preview
    },

    -- Enable/disable specific commands
    commands = {
      create_from_template = true,
      insert_header = true,
      insert_code_block = true,
      insert_bold = true,
      insert_italic = true,
      insert_link = true,
      insert_table = true,
      insert_checkbox = true,
      toggle_checkbox = true,
      preview = false, -- Requires `preview_command` to be set
    },

    -- Command or Lua function to execute for Markdown preview.
    preview_command = nil,

    -- Apply local buffer settings for Markdown files
    enable_local_options = true,
    wrap = true,
    conceallevel = 2,
    concealcursor = "nc",
    spell = true,
    spelllang = "en_us",

    -- File types where keymaps should be active
    file_types = { "markdown" },

    -- Automatically continue lists (bullet, numbered, checkbox) on Enter
    continue_lists_on_enter = true,
  },
}
