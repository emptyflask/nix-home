require'nvim-tree'.setup {
  disable_netrw       = true,
  hijack_netrw        = true,
  hijack_cursor       = false,
  update_cwd          = false,
  hijack_directories  = {
    enable = true,
    auto_open = true,
  },
  diagnostics = {
    enable = false,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    }
  },
  update_focused_file = {
    enable      = false,
    update_cwd  = false,
    ignore_list = {}
  },
  system_open = {
    cmd  = nil,
    args = {}
  },
  filters = {
    dotfiles = false,
    custom = {}
  },
  git = {
    enable = true,
    ignore = true,
    timeout = 500,
  },
  renderer = {
    root_folder_label = false,
  },
  view = {
    width = 30,
    side = 'left',
    number = false,
    relativenumber = false,
    signcolumn = "yes"
  },
  trash = {
    cmd = "trash",
    require_confirm = true
  },
  actions = {
    open_file = {
      resize_window = false
    }
  }
}

vim.cmd.hi({"NvimTreeRootFolder",       "guifg=#7c6f64", "gui=bold"})
vim.cmd.hi({"NvimTreeFolderName",       "guifg=#83a598", "gui=none"})
vim.cmd.hi({"NvimTreeEmptyFolderName",  "guifg=#458588", "gui=none"})
vim.cmd.hi({"NvimTreeOpenedFolderName", "guifg=#83a598", "gui=bold"})
vim.cmd.hi({"NvimTreeFolderIcon",       "guifg=#458588", "gui=none"})
