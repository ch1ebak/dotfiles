return {
  'nvim-orgmode/orgmode',
  event = 'VeryLazy',
  ft = { 'org' },
  config = function()
    require('orgmode').setup({
      org_agenda_files = '~/Dokumenty/notatki/02-agenda/*.org',
      org_default_notes_file = '~/Dokumenty/notatki/02-agenda/Taski.org',
      org_archive_location = '~/Dokumenty/notatki/02-agenda/Archiwum.org',
      win_split_mode = 'vsplit',
      org_startup_folded = 'showeverything',
      org_hide_leading_stars = true,
      org_hide_emphasis_markers = true,
      org_ellipsis = ' ▾',
      org_log_into_drawer = 'LOGBOOK',
      org_adapt_indentation = false,
      org_agenda_start_on_weekday = false,
      org_agenda_start_day = '-1d',
      org_agenda_skip_scheduled_if_done = true,
      org_agenda_skip_deadline_if_done = true,
      org_agenda_span = 'week',
      org_capture_templates = { t = { description = 'Task', template = '\n ** TODO %?\n' } },
      org_todo_keywords = {'TODO(t)', 'WAIT(w)', 'FIXME(f)', '|', 'CANCELED(c)', 'DONE(d)'},
      org_todo_keyword_faces = {
        TODO = ':background #b04b57 :foreground #20242d :weight bold',
        WAIT = ':background #e5c179 :foreground #20242d :weight bold',
        FIXME = ':background #a47996 :foreground #20242d :weight bold',
        CANCELED= ':background #85a7a5 :foreground #20242d :slant italic :weight bold',
        DONE = ':background #87b379 :foreground #20242d :slant italic :weight bold',
      },
      mappings = {
        global = {
          org_agenda = {'<Leader>pa'},
          org_capture = {'<Leader>px'}
        },
        org = {
          org_meta_return = {'<C-CR>'},
          org_priority = {'mp'},
          org_todo= {'mt'},
          org_toggle_checkbox= {'mc'},
          org_insert_link = {'ma'},
          org_open_at_point = {'mo'},
          org_do_promote = {'mH'},
          org_do_demote = {'mL'},
          org_move_subtree_up = {'mJ'},
          org_move_subtree_down = {'mK'},
          org_deadline = {'md'},
          org_schedule = {'ms'},
          org_refile = {'mr'},
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
        vim.keymap.set('i', '<S-CR>', '<cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>', {
          silent = true,
          buffer = true,
        })
      end,
    })
  end,
}
