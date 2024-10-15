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
Inserts text matching a natural language description. All of the commands are given as a minimum the current major mode as context. Here are two examples using `nalec-insert` with the same instructions but in two different major modes, `python-mode` and `haskell-mode`.

https://github.com/user-attachments/assets/e0e9427b-7062-4a95-9835-7f7ba1051ab1

https://github.com/user-attachments/assets/5c0d975c-f006-4325-a2c7-b40ec757c4aa

### `nalec-replace`
The current region is killed and sent to the llm, to be replaced with new text. An example in `latex-mode`.

https://github.com/user-attachments/assets/90230371-7c55-46a4-b0a6-f333ab85d8aa

### `nalec-yank`
Get text from the kill ring, adapt it and insert the results. In this particular example no instructions were necessary and the llm has correctly guessed the action to take from the insertion text and major mode, `bibtex-mode`.

https://github.com/user-attachments/assets/cfda982a-71c3-427e-8c52-24295cfcbea5

### `nalec-yank-image`
Pulls an image from the clipboard and sends it to the llm with instructions, or in this case just the image and context.

https://github.com/user-attachments/assets/b35db8f7-a53d-48f9-a6eb-94248a8cbd0a

### `nalec-regexp`
Generates a regexp search and replace to carry out instructions. The command is sent to `query-replace-regexp` so you can double check any changes first. You can then use the standard query commands, e.g. `y` to replace one by one, `!` to replace all, `C-g` to abort.

https://github.com/user-attachments/assets/38720c41-7ac9-43ad-a6d9-b5b0f5c02063

### `nalec-redo`
Any text that was inserted is deleted, and the llm is told to try the same thing again with further instructions. Can be used after any of the other nalec commands.

https://github.com/user-attachments/assets/fdfb7931-0682-45ee-9121-fa29085ae365


## Installation and set up

1.  Install the [llm package for emacs](https://github.com/ahyatt/llm). It is included on ELPA and so can be installed from emacs with `M-x package-refresh-contents` followed by `M-x package-install RET llm RET`.

2.  Download nalec using `git clone git@github.com:awswan/nalec.git`. You can load it on startup by adding `(load-file "path/to/nalec.el")` to your `.emacs`.

3.  Obtain access to a large language model supported by the llm package. This could be one of a number of commercial providers or a model that runs locally.

4.  Add your provider settings to nalec using `M-x customize-group RET nalec RET`. The variable `Nalec Provider Type` is the choice of provider. The variable `Nalec Provider Options` contains additional parameters defining the provider. See the [llm README file](https://github.com/ahyatt/llm#setting-up-providers) for possible options for each provider. For commercial providers, this will include setting the option `:key` to the API key. For local providers setting `:chat-model` may be required.
   
5.  By default the `llm` package gives a warning about the use of the current generation of llm's in free software. You can read more about this at [the llm README](https://github.com/ahyatt/llm#llm-and-the-use-of-non-free-llms) and if you feel you understand the issues, you can disable the warning with `M-x customize-save-variable llm-warn-on-nonfree n`.

## Related packages

There are already existing, much more developed emacs packages using large language models, notably [gptel](https://github.com/karthink/gptel) and [ellama](https://github.com/s-kostyaev/ellama). I think of nalec as intermediate between the two approaches: it has a toolbox of commands like ellama, but aims to make them as simple and flexible as possible, in a similar spirit to gptel.
