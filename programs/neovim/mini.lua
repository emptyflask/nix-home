require('mini.ai').setup()

require('mini.align').setup({
  modifiers = {
    [':'] = function(steps, opts)
      opts.split_pattern = ': '
      table.insert(steps.pre_justify, MiniAlign.gen_step.trim())
      table.insert(steps.pre_justify, MiniAlign.gen_step.pair())
      opts.merge_delimiter = ' '
    end
  }
})

require('mini.bracketed').setup()

require('mini.comment').setup({
  mappings = {
    comment = "gc",
    comment_line = "\\\\\\",
    comment_visual = "\\\\",
    textobject = "gc"
  }
})

require('mini.move').setup()

require('mini.pairs').setup()
