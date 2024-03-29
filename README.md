# RbTagger

[![Build Status](https://travis-ci.com/thiagoa/rbtagger.svg?branch=master)](https://travis-ci.org/thiagoa/rbtagger)

> A text-based Ruby tags system for Emacs

RbTagger is an Emacs library based on ctags,
[`ripper-tags`](https://github.com/lzap/gem-ripper-tags), and
`xref.el`. It indexes your entire Ruby project along with gems and
provides smarter than average tag lookup.  It aims to provide
context-aware, accurate tag lookup by parsing the current Ruby file
and an easy-to-use tags solution that works out of the box.

RbTagger is currently beta software extracted from my Emacs
configuration.

## Features

- It indexes full projects along with gems, including the Ruby standard library;
- It indexes full Ruby modules, thanks to `ripper-tags` extra options.
- It has contextual tags lookup. RbTagger is aware of Ruby code and
will try to jump to the most specific occurrence for the symbol at
point.
- It takes into account the full Ruby module when looking for
definitions. If point is on `ModOne::ModTwo`, more specifically at
`ModTwo`, the searched tag will be the full module name:
`ModOne::ModTwo`.

## Prerequisites

1. Emacs 25 or greater.
2. The `ruby`, `gem`, and `bundler` commands must be readily accessible
   from within Emacs. If you're on macOS, I recommend installing the
   [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
   package.

## Quickstart

### Installation

RbTagger is available on the two major `package.el` community
maintained repos - [MELPA Stable](http://stable.melpa.org) and
[MELPA](http://melpa.org). If you want to use MELPA stable,
add the following repository to `package-archives`:

```elisp
;; This code snippet should be saved to init.el
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
```

If you want to stay on the bleeding edge:

```elisp
(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
```

After that, you can install RbTagger with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `rbtagger` <kbd>[RET]</kbd>

### Basic usage

Use the following code in init.el to enable `rbtagger-mode` in all
Ruby buffers:

```elisp
;; For ruby-mode
(add-hook 'ruby-mode (rbtagger-mode))

;; For enh-ruby-mode
(add-hook 'enh-ruby-mode (rbtagger-mode))
```

You can generate tags for the current Ruby project with <kbd>M-x</kbd>
`rbtagger-generate-tags`. I strongly recommend setting up automation
either through an [`after-save`](#after-save-hook) hook or [git hooks](#git-hooks) for a
better experience.

After enabling the minor mode, you can find definitions for the symbol
at point with <kbd>M-.</kbd>, which is a shortcut for <kbd>M-x</kbd>
`rbtagger-find-definitions`. The above keybinding replaces Emacs'
keybinding for `xref-find-definitions`.

You can also force displaying a prompt of tags to choose from with the
universal argument: `C-u M-.`.

To pop back to where you were before, the command is still
<kbd>M-,</kbd> or <kbd>M-x</kbd> `xref-pop-marker-stack`, which is a
default `xref` command.

Here is a list of commands:

| Keybinding             | Description                                                |
|------------------------|------------------------------------------------------------|
| <kbd>M-.</kbd>         | `rbtagger-find-definitions`                                |
| <kbd>M-,</kbd>         | `xref-pop-marker-stack`                                    |
| <kbd>C-u M-.</kbd>     | `rbtagger-find-definitions` (displays prompt)              |
| <kbd>C-c C-.</kbd>     | `rbtagger-find-definitions-other-window`                   |
| <kbd>C-u C-c C-.</kbd> | `rbtagger-find-definitions-other-window` (displays prompt) |
| <kbd>C-c M-.</kbd>     | `rbtagger-find-definitions-other-frame`                    |
| <kbd>C-u C-c M-.</kbd> | `rbtagger-find-definitions-other-frame` (displays prompt)  |

I strongly recommend reading up this guide for more details on how to
best use this package.

**TIP**: In the tag prompt, both `TAB` and `?` are set to trigger
autocomplete or display the available tag completions. To insert a
literal question mark in the completion prompt (which is a valid
character for Ruby methods), type `C-q ?`.

## Generating tags

To generate `TAGS`, make sure the current buffer belongs to a Ruby
project with a `Gemfile` and `git` as VCS, then call <kbd>M-x</kbd>
`rbtagger-generate-tags`.

The above command will:

- Install the `ripper-tags` gem if not already installed,
- Index the main project,
- Index the Ruby standard library,
- Index all dependencies declared in `Gemfile`,
- Generate a single `TAGS` file and save it to the root of the
project.

The first call to the command might take a few seconds to complete
depending on the size of your project, but subsequent calls will be
faster because the script will skip directories whose tags have
already been generated. If the gem is a local git project, it will
only be reindexed if the commit hash has changed from the previous
indexing operation.

Make sure to add the following patterns to your global `.gitignore`:

```bash
$ echo "TAGS*" >> ~/.gitignore
$ echo ".TAGS" >> ~/.gitignore
$ echo .ruby_tags_commit_hash >> ~/.gitignore
```

### Troubleshooting

<kbd>M-x</kbd> `rbtagger-generate-tags` will create two hidden buffers
that can be accessed with the following commands:

- <kbd>M-x</kbd> `rbtagger-stdout-log`: The message log of what's being indexed;
- <kbd>M-x</kbd> `rbtagger-stderr-log`: The error log.

You can watch the output of these buffers live for troubleshooting,
or after indexing. Note that they will only hold the output of the
last `rbtagger-generate-tags`.

A message will also be displayed in the minibuffer (or the
`*Messages*` buffer) when the command finishes, or you can configure
[Custom Notifications](#custom-notifications).

## Looking up tags

### Visiting tags tables

I recommend installing the
[`projectile`](https://github.com/bbatsov/projectile) package (also
available on MELPA) and enabling
[`(projectile-mode)`](https://projectile.readthedocs.io/en/latest/installation/)
globally in your init.el to visit a project's `TAGS` file
automatically when switching buffers (on Emacs' `find-file-hook`,)
otherwise you'll have to manually manage the active tags table with
<kbd>M-x</kbd> `visit-tags-table`.

### Finding definitions

<kbd>M-x</kbd> `rbtagger-find-definitions` or <kbd>M-.</kbd> (provided
that [`rbtagger-mode` is enabled](#basic-usage)) tries to find the
best match for the symbol at point by computing a list of candidates
ordered by specificity. It tries to follow Ruby's Constant lookup
rules as closely as possible. Given the following Ruby module:

```ruby
module Tags
  module Lookup
    module Nested
      def self.call(*args)
        # Some code here...
        Rule.call(*args)
      end
    end
  end
end
```

Assuming that point is on `Rule`, RbTagger will try four candidates in
order:

- `Tags::Lookup::Nested::Rule`
- `Tags::Lookup::Rule`
- `Tags::Rule`
- `Rule`

If one of the candidates resolve to one or more matches, it will
either:

- Jump to the first occurrence when dealing with a single match;
- Display a list of tags to choose from when dealing with more than one match.

Subsequent candidates will be skipped.

### Recommended tag settings

I recommend the following settings for a smoother tags experience with
no prompts. Save them in `init.el`:

```elisp
;; Make tag search case sensitive. Highly recommended for
;; maximum precision.
(setq tags-case-fold-search nil)

;; Reread TAGS without querying if it has changed
(setq tags-revert-without-query 1)

;; Always start a new tags list (do not accumulate a list of
;; tags) to keep up with the convention of one TAGS per project.
(setq tags-add-tables nil)
```

## After save hook

You can automate tags generation with an after save hook. If you want
to update your TAGS every time you save a Ruby file, you can setup a
hook like this in your Emacs config:

```elisp
(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'enh-ruby-mode)
             (call-interactively 'rbtagger-generate-tags))))
```

If you use `ruby-mode` instead of `enh-ruby-mode`, just replace
`enh-ruby-mode` in the above snippet.

## Git hooks

It is possible to automate tags generation with the help of
`emacsclient` when committing or running other git operations. Use the
following shell script as the body of the `post-commit`, `post-merge`,
and `post-rewrite` git hooks:

```bash
#!/usr/bin/env bash

emacsclient -e "(rbtagger-generate-tags \"$(pwd)/\")"
```

> TIP: You can setup these hooks as [git
> templates](https://git-template.readthedocs.io/en/latest/) that are
> automatically copied over whenever you `git init` a project.

I recommend adding the following snippet to `init.el`
to start the Emacs server when you launch Emacs:

```elisp
(require 'server)
(unless (server-running-p)
  (server-start))
```

## Custom notifications

RbTagger supports custom notifications via hooks. The hook's callable
takes two arguments: `success` (boolean `t` or `nil`) and
`project-name` (string). Here's an example of how it can be used on
macOS to integrate with notification center:

```elisp
(add-hook
 'rbtagger-after-generate-tag-hook
 (lambda (success project-name)
   (if success
       (notify-os (concat project-name " tags 👍") "Hero")
     (notify-os "Is this a Ruby project? Tags FAILED! 👎" "Basso"))))
```

This particular example assumes you have the following function:

```elisp
(defun notify-os (message sound)
  "Send a notification to macOS's notification center.
Requires terminal-notifier to be installed via homebrew."
  (shell-command
   (combine-and-quote-strings
    (list "terminal-notifier" "-message" message "-sound" sound))))
```

## Why a single TAGS file?

> Can't we use `tags-table-list` to setup more than one tag file for
> lookup, i.e., smaller tag tables for each gem?

Certainly, but that could result in hundreds of junk buffers due to
the way tags work in Emacs. In a project with 300 gems, Emacs would
open 300 buffers while searching for a tag, which would greatly
enlarge the buffer list. For that reason, my preference is a single
`TAGS` file.

## Where RbTagger can improve

RbTagger's source code is simple and concise on purpose and it works
very well for my needs. However, it can improve in the following
areas:

### Contextual tag lookup

As you can see, contextual tag lookup isn't as efficient as it can be
and there is room for improvement.  In my experience, it will find the
tag instantaneously (with 200+ gems) most of the time, but sometimes
it will freeze for about 1 second. The eventual performance hit is
negligible for me, but any improvements on performance, better usage
of `xref`, or the algorithm itself would be hugely appreciated.

### Tag Candidates list

Building up the candidates list is currently an indentation-based and
regex-based algorithm that happens inside Emacs buffers. Ideally, it
should work with static analysis but that would probably make the code
more complex or add more dependencies. Being regex-based means it
would not work properly without properly indented module
declarations. I never found this to be a problem because all my Ruby
files are indented, but again, any contributions on that front will be
appreciated.

### What else?

You are welcome to contribute with anything. Please send PRs!

## Running tests

Through the command line in batch mode:

```bash
$ make test
```

Through Emacs:

1. Open `test/rbtagger-test.el`
2. Run <kbd>M-x</kbd> `eval-buffer`. Side-effect warning: this will add MELPA to `package-archives` and install dependencies.
3. Press `C-c C-r` to run all tests.
4. To run a single test, press <kbd>M-x</kbd> `eval-expression` and type `(ert
   "name-of-the-test")`. See `ert` docs for more options.

The `make` command with no arguments will compile `rbtagger.el` and
run `checkdoc` over it. Any warnings will make the command fail.

## License

Copyright © 2021 Thiago Araújo Silva.

Distributed under the GNU General Public License, version 3
