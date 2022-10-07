require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true
  },

  rainbow = {
    enable = true,
    -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
    extended_mode = false, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
    colors = {
      '#458588',
      '#b16286',
      '#cc241d',
      '#d65d0e',
      '#458588',
      '#b16286',
      '#cc241d',
      '#d65d0e',
      '#458588',
      '#b16286',
      '#cc241d',
      '#d65d0e',
      '#458588',
      '#b16286',
      '#cc241d',
      '#d65d0e',
    },
    -- termcolors = {} -- table of colour name strings
  }
}
