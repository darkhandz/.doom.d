## Warning
This is my personal doom-emacs configration, it's only for myself to sync with Linux dists (WSL).
If you want to use it, please edit the `config.el` and change the font first, otherwise, you emacs may encounter black screen or undefined behavior.
- `~/.emacs.d/bin/doom sync` or `~/.config/emacs/bin/doom sync`, it's up to your doom emacs dir.
- windows shortcut to use VcXsrv: `"C:\Windows\System32\wsl.exe -d your-wsl-dist-name export DISPLAY=localhost:0.0; export LIBGL_ALWAYS_INDIRECT=1; setsid emacs;"`

### key binding in brief
- mouse side key to jump-forward/backward
- `SPACE l/k` to switch *next/previous* buffer
- `SPACE 1/2/3/4...` to quickly switch between windows
- `SPACE [/]` to navigate *next/previous* hunk
- `ALT 1/2/3/4...` to quickly switch between workspaces
- add symbol-overlay functions to `SPACE d`
- change evil visual/normal `s` and `S` to avy function 
- `F6/F7/F8` to run custom project commands in `vterm`, defined in `.dirs-locals.el`

### other changes
- prefer `ccls` over `clangd`
- ivy search(rg) at least 2 chars to trigger
- disable corfu preview selection on input cursor
- projectile don't auto add projects
- avy in all windows
- disable whitespace-mode (sometimes cause visual indent issue)
- make `_` as part of a *word* in c-mode/python-mode/js2-mode
- hide `^M` on c-mode/text-mode
- change evil `p/c/s` actions to no yanking
