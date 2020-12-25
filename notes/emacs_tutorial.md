# Move Screenfuls

	C-v	Move forward one screenful
	M-v	Move backward one screenful
	C-l	Clear screen and redisplay all the text,
		 moving the text around the cursor
		 to the center of the screen.
		 (That's CONTROL-L, not CONTROL-1.)

# Cursor Control

Could use arrow keys but standard positions are better

                          Previous line, C-p
                                  :
                                  :
   Backward, C-b .... Current cursor position .... Forward, C-f
                                  :
                                  :
                            Next line, C-n

CTRL moves by character, META moves by words.

	C-f	Move forward a character
	C-b	Move backward a character

	M-f	Move forward a word
	M-b	Move backward a word

	C-n	Move to next line
	C-p	Move to previous line

	C-a	Move to beginning of line
	C-e	Move to end of line

	M-a	Move back to beginning of sentence
	M-e	Move forward to end of sentence

    M-< move to the beginning of the whole text
    M-> move to the end of the whole text
    
Prefix arguments (C-u) for movement are repeaters.
Prefix arguments for screenfuls make it scroll by lines


# Windows

    C-x 1   One window (i.e., kill all other windows).

C-x typically indicates a windows command


# Inserting and Deleting

	<DEL>        Delete the character just before the cursor
	C-d          Delete the next character after the cursor

	M-<DEL>      Kill the word immediately before the cursor
	M-d          Kill the next word after the cursor

	C-k          Kill from the cursor position to end of line
	M-k          Kill to the end of the current sentence
    
Can also kill a region by marking the start with C-<SPC> and C-w to
kill-region

    C-y   Yank the most recent kill
    M-y   after a C-y, cycle through previous yanks

# Extending the command set

	C-x	Character eXtend.  Followed by one character.
	M-x	Named command eXtend.  Followed by a long name.


	C-x C-f		Find file
	C-x C-s		Save file
	C-x s		Save some buffers
	C-x C-b		List buffers
	C-x b		Switch buffer
	C-x C-c		Quit Emacs
	C-x 1		Delete all but one window
	C-x u		Undo
    C-x f       Set fill column

# Mode Line

To view documentation on your current major mode, type C-h m.


# Searching

    C-s forward incremental search
    C-r backward incremental search

# Multiple windows

    C-M-v       scroll-other-window
    C-x 4 C-f   find-file-other-window

# Multiple Frames

    C-x 5 2 make-frame-command
    C-x 5 0 delete-frame
    C-x 5 o other-frame

# Recursive editing levels

<ESC> <ESC> <ESC>  keyboard-escape-quit
