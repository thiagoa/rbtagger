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

- It indexes full projects along with gems;
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

## Installation

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

## Generating tags

To generate `TAGS`, make sure the current buffer belongs to a Ruby
project with a `Gemfile` and `git` as VCS, then call <kbd>M-x</kbd>
`rbtagger-generate-tags`.

The above command will:

- Install the `ripper-tags` gem if not already installed,
- Index the main project,
- Index all dependencies declared on `Gemfile`,
- Generate a single `TAGS` file and save it to the root of the
project.

The first call to the command might take a few seconds to complete
depending on the size of your project, but subsequent calls will be
much faster because the script will skip gems whose tags have already
been generated. If the gem is a local git project, it will only be
reindexed if the commit hash has changed from the previous indexing
operation.

Make sure to add the following files to your global `.gitignore`:

```bash
$ echo TAGS >> ~/.gitignore
$ echo .ruby_tags_commit_hash >> ~/.gitignore
```

### Troubleshooting

<kbd>M-x</kbd> `rbtagger-generate-tags` will create two hidden buffers:

- The message log, `*rbtagger-log: PROJECT NAME*`, where you can see
  what is being indexed,
- The error log, `*rbtagger-error-log: PROJECT NAME*`.

You can switch to these buffers for troubleshooting even after the
command finishes. These buffers will only hold the output of the last
command.

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

<kbd>M-x</kbd> `rbtagger-find-definitions` tries to find the best
match for the symbol at point by computing a list of candidates
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
;; Always start a new tags list (do not accumulate a list of
;; tags) to keep up with the convention of one TAGS per project.
(setq tags-add-tables nil)

;; Reread TAGS without querying if it has changed
(setq tags-revert-without-query 1)
```

### Setting up a keyboard shortcut

The classic Emacs shortcut for looking up tags is <kbd>M-.</kbd>,
which calls `xref-find-definitions`.  I recommend overriding this
shortcut with `rbtagger-find-definitions` in your favorite Ruby mode:

```elisp
;; For enh-ruby-mode
(with-eval-after-load 'enh-ruby-mode
  (define-key enh-ruby-mode-map (kbd "M-.") 'rbtagger-find-definitions))

;; For ruby-mode
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "M-.") 'rbtagger-find-definitions))
```

For popping back to where you were before, the command is still <kbd>M-.</kbd>
or <kbd>M-x</kbd> `xref-pop-marker-stack`.

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

## Running the tests

Through the command line in batch mode:

```bash
$ cd test
$ emacs -Q -l rbtagger-test.el --batch -f ert-run-tests-batch-and-exit
```

Through Emacs:

1. Open `test/rbtagger-test.el`
2. Run <kbd>M-x</kbd> `eval-buffer`. Side-effect warning: this will add MELPA to
   `package-archives`.
3. Press `C-c C-r` to run all tests.
4. To run a single test, press <kbd>M-x</kbd> `eval-expression` and type `(ert
   "name-of-the-test")`. See `ert` docs for more options.

## License

Copyright © 2019 Thiago Araújo Silva.

Distributed under the GNU General Public License, version 3
