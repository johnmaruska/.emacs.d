Misc

    C-x C-u   upcase-region
    C-x C-l   downcase-region  (disabled command)
    M-%       query-replace
    
Improvements TODO:

- better use of Mark ring
- better use of Yank ring

# 3 Keys

   By default, the prefix keys in Emacs are ‘C-c’, ‘C-h’, ‘C-x’, ‘C-x
<RET>’, ‘C-x @’, ‘C-x a’, ‘C-x n’, ‘C-x r’, ‘C-x v’, ‘C-x 4’, ‘C-x 5’,
‘C-x 6’, <ESC>, ‘M-g’, and ‘M-o’.  (<F1> and <F2> are aliases for ‘C-h’
and ‘C-x 6’.)

   Typing the help character (‘C-h’ or <F1>) after a prefix key displays
a list of the commands starting with that prefix.  The sole exception to
this rule is <ESC>: ‘<ESC> C-h’ is equivalent to ‘C-M-h’, which does
something else entirely.  You can, however, use <F1> to display a list
of commands starting with <ESC>.

# 7 Inserting Text
## 7.2 Changing the Location of a Point

    M-r         move-to-window-line-top-bottom
    M-g g       goto-line
    M-g c       goto-char
    M-g <TAB>   move-to-column
    C-x C-n     set-goal-column  C-n and C-p trend towards them
    C-u C-x C-n unset-goal-column

## 7.7 Blank Lines

    C-o       open-line
    C-x C-o   delete-blank-lines

## 7.9 Cursor Position Information

    M-=   count-words-region
    M-x count-words

# Minibuffer
## 8.2 Minibuffers for File Names

    M-n  cycle default-directory during find-file

## 8.3 Editing in the Minibuffer

    C-q C-j to insert a newline in minibuffer
    C-o     same thing but after cursor instead of at cursor

## 8.5 Minibuffer History

    M-p     previous-history-element
    M-n     next-history-element

    <UP>    previous-line-or-history-element
    <DOWN>  next-line-or-history-element

    M-r     previous-matching-history-element
    M-s     next-matching-history-element

## 8.6 Repeating Minibuffer Commands

    C-x <ESC> <ESC>   repeat-complex-command
    M-x list-command-history

# 10 Help

    C-h a TOPICS <RET>   apropos on TOPICS

    C-h i d m emacs <RET> i TOPIC <RET>
    
search for TOPIC in indices of emacs info manual
Help > Info > Dir > Menu > emacs > Index > TOPIC

    C-h i d m emacs <RET> s TOPIC <RET>
    
same as bove but searches text not indices

    C-h p    packages
    C-h C-f  FAQ
    
## 10.1 Help Summary

honestly too many. just let a helper package show key completions from
`C-h`

## 10.5 Help Mode Commands

    C-c C-c   help-follow-symbol

    C-c C-f   help-go-forward
    r

    C-c C-b   help-go-backward
    l
    
## 10.8 Other Help Commands

    C-h l   view-lossage
    C-h e   view-echo-area-messages, i.e. *Messages*
    C-h m   describe mode
    
# 11 Mark
## 11.1 Setting the Mark

    C-x C-x  exchange-point-and-mark
    mouse-3 (right-click)  mouse-save-then-kill
    
## 11.2 Commands to Mark Textual Objects

    M-@       mark-word
    C-M-@     mark-sexp
    M-h       mark-paragraph
    C-M-h     mark-defun
    C-x C-p   mark-page
    C-x h     mark-whole-buffer

## 11.4 The Mark Ring

    C-u C-<SPC> cycles through old marks

## 11.5 The Global Mark Ring

16 entries of marks stored in global ring
only stores marks if youve switched buffers

    C-x C-<SPC>  pop-global-mark

# 12 Killing

AM HERE
