# brushup.el

Dynamic theme-aware color palette for Emacs.

Brushup generates gradient colors from your current theme's foreground and background, so face customizations work consistently across any theme. When you switch themes, all registered styles automatically update.

## Features

- Parametric gradient generation from theme colors
- Automatic dark/light theme detection
- Style registration system for face customizations
- `use-package` `:brushup` keyword integration
- Re-evaluates all styles on theme change

## Installation

### With use-package and elpaca

```elisp
(use-package brushup
  :ensure (:host github :repo "chiply/brushup"))
```

### With use-package and straight.el

```elisp
(use-package brushup
  :straight (:host github :repo "chiply/brushup"))
```

## Usage

### Register face customizations

```elisp
(add-to-list 'brushup-styles
  '(set-face-attribute 'some-face nil :background brushup-bg-2))
```

### With use-package `:brushup` keyword

```elisp
(use-package my-package
  :ensure t
  :brushup
  (add-to-list 'brushup-styles
    '(set-face-attribute 'my-face nil
       :foreground brushup-fg-3
       :background brushup-bg-1)))
```

### Manual refresh

```
M-x brushup
```

## Palette Variables

| Variable | Description |
|----------|-------------|
| `brushup-fg` | Theme foreground color |
| `brushup-bg` | Theme background color |
| `brushup-fg-1` to `brushup-fg-6` | Foreground gradient (toward bg, 1=subtle, 6=strong) |
| `brushup-bg-1` to `brushup-bg-6` | Background gradient (toward fg, 1=subtle, 6=strong) |
| `brushup-bg-1_0` | Subtle background shift (for solaire-like effects) |
| `brushup-dark-p` | `t` if current theme is dark |

## Customization

```elisp
;; Change gradient step size (default: 7%)
(setq brushup-gradient-step 10)
```

## License

GPL-3.0
