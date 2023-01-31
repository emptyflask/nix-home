local null_ls = require("null-ls")

local ruby_code_actions = require("ruby-code-actions")

local sources = {
    -- require any built-ins you want
    null_ls.builtins.formatting.rubocop,
    null_ls.builtins.diagnostics.rubocop,
    -- ...
    -- now require any ruby-code-actions you want
    ruby_code_actions.insert_frozen_string_literal,
    ruby_code_actions.autocorrect_with_rubocop
}

null_ls.setup({sources = sources})
