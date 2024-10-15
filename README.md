# Nalec - NAtural Language Emacs Commands

## What is nalec?

Nalec provides some simple commands to carry out tasks based on natural language instructions using a large language model of your choice, via the [llm package for emacs](https://github.com/ahyatt/llm). Nalec aims to have the following features.

- A small number of highly flexible commands, leaving details up to the natural language instructions.
- Not specific to program code, but works equally well with any kind of text that might be edited on emacs, including structured documents such as LaTeX and HTML.
- Basic and simple to use interface.
- Automatically generates some small amount of context to supplement the instructions, without sending large amounts of data to the llm.
- Commands motivated by simple real life tasks.

Here are the commands.

- `nalec-insert` insert text at the current point based on a natural language description
- `nalec-replace` kills the current region, sends it to the llm and generates replacement text according to instructions
- `nalec-yank` takes text from the kill ring, adjusts according to instructions, and then inserts the adjusted text
- `nalec-yank-image` takes an image from the clipboard and uses it to generate text at the current point according to instructions (not supported on MS windows)
- `nalec-regexp` use regexp search and replace to carry out a given task
- `nalec-redo` do the previous command again with further instructions

Each command can also be given a blank instruction, in which case it will attempt to do something sensible given the context.

## Demonstration

### `nalec-insert`

## Installation and set up

1.  Install the [llm package for emacs](https://github.com/ahyatt/llm). It is included on ELPA and so can be installed from emacs with `M-x package-refresh-contents` followed by `M-x package-install RET llm RET`.

2.  Download nalec. You can load it on startup by adding `(load-file "path/to/nalec.el")` to your `.emacs`.

3.  Obtain access to a large language model supported by the llm package. This could be one of a number of commercial providers or a model that runs locally.

4.  Add your provider settings to nalec using `M-x customize-group RET nalec RET`. The variable `Nalec Provider Type` is the choice of provider. The variable `Nalec Provider Options` contains additional parameters defining the provider. See the [llm README file](https://github.com/ahyatt/llm/tree/main?tab=readme-ov-file#setting-up-providers) for possible options for each provider. For commercial providers, this will include setting the option `:key` to the API key. For local providers setting `:chat-model` may be required.

## Related packages

There are already existing, much more developed emacs packages using large language models, notably [gptel](https://github.com/karthink/gptel) and [ellama](https://github.com/s-kostyaev/ellama). I think of nalec as intermediate between the two approaches: it has a toolbox of commands like ellama, but aims to make them as simple and flexible as possible, in a similar spirit to gptel.
