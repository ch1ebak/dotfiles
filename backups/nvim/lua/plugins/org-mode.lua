return {
  'nvim-orgmode/orgmode',
  event = 'VeryLazy',
  ft = { 'org' },
  config = function()
  require('nvim-treesitter.configs').setup {
    highlight = {
      enable = true,
      disable = {'org'},
      additional_vim_regex_highlighting = {'org'},
    },
    ensure_installed = {'org'},
  }
  require('orgmode').setup({
    org_agenda_files = '/ssd/Obsidian/notatki/01-agenda/*',
    org_default_notes_file = '/ssd/Obsidian/notatki/01-agenda/agenda-taski.org',
    org_archive_location = '/ssd/Obsidian/notatki/01-agenda/archiwum.org',
    win_split_mode = 'vsplit',
    org_startup_folded = 'content',
    org_hide_leading_stars = true,
    org_hide_emphasis_markers = true,
    org_ellipsis = ' ▾',
    org_log_into_drawer = 'LOGBOOK',
    org_adapt_indentation = true,
    org_agenda_start_on_weekday = false,
    org_agenda_start_day = '-1d',
    org_agenda_skip_scheduled_if_done = true,
    org_agenda_skip_deadline_if_done = true,
    org_capture_templates = { t = { description = 'Task', template = '\n* TODO %?\n' } },
    org_todo_keywords = {'TODO(t)', 'WAIT(w)', 'FIXME(f)', '|', 'CANCELED(c)', 'DONE(d)'},
    org_todo_keyword_faces = {
      TODO = ':foreground #b04b57 :weight bold',
      WAIT = ':foreground #e5c179 :weight bold',
      FIXME = ':foreground #a47996 :weight bold',
      CANCELED= ':foreground #85a7a5 :slant italic',
      DONE = ':foreground #87b379 :slant italic',
    },
    mappings = {
      global = {
        org_agenda = {'<Leader>pa'},
        org_capture = {'<Leader>px'}
      },
      org = {
        org_meta_return = {'<C-CR>'},
        org_priority = {'<Leader>mp'},
        org_todo= {'<Leader>mt'},
        org_toggle_checkbox= {'<Leader>mc'},
        org_insert_link = {'<Leader>ma'},
        org_open_at_point = {'<Leader>mo'},
        org_do_promote = {'<Leader>mH'},
        org_do_demote = {'<Leader>mL'},
        org_move_subtree_up = {'<Leader>mJ'},
        org_move_subtree_down = {'<Leader>mK'},
        org_deadline = {'<Leader>md'},
        org_schedule = {'<Leader>ms'},
        org_refile = {'<Leader>mr'},
      },
    },
    ui = {
      folds = {
        colored = false
      }
    },
  })
  vim.api.nvim_create_autocmd('FileType', {
    pattern = 'org',
    callback = function()
      vim.keymap.set('i', '<C-CR>', '<cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>', {
        silent = true,
        buffer = true,
      })
    end,
  })
end,
}
