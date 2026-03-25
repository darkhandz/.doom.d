;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Keep this file as a small entrypoint. Doom-specific behavior lives in
;; focused modules under `modules/`, grouped by responsibility.

(load! "modules/core")
(load! "modules/editing")
(load! "modules/completion")
(load! "modules/navigation")
(load! "modules/lsp")
(load! "modules/project")
(load! "modules/workflow")
