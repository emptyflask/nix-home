local dashboard = function()
  local theme = require("alpha.themes.dashboard")
  local logo = [[
                                             
      ████ ██████           █████      ██
     ███████████             █████ 
     █████████ ███████████████████ ███   ███████████
    █████████  ███    █████████████ █████ ██████████████
   █████████ ██████████ █████████ █████ █████ ████ █████
 ███████████ ███    ███ █████████ █████ █████ ████ █████
██████  █████████████████████ ████ █████ █████ ████ ██████
]]
  theme.section.header.val = vim.split(logo, "\n")
  theme.section.header.opts.hl = "AlphaHeader"
  theme.config.layout[1].val = 6
  theme.section.buttons.val = {
    theme.button("e", " " .. " New file",     ":ene<CR>"),
    theme.button("f", " " .. " Find file",    ":Files<CR>"),
    theme.button("r", " " .. " Recent files", ":History<CR>"),
    theme.button("g", " " .. " Find text",    ":Rg<CR>"),
    theme.button("q", " " .. " Quit",         ":qa<CR>"),
  }
  return theme
end

require'alpha'.setup(require'alpha.themes.startify'.config)

require("alpha").setup(dashboard().config)
