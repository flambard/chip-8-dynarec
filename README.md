Dynamic CHIP-8-to-Lisp Recompiler
=================================

With chip-8-dynarec a [CHIP-8](http://en.wikipedia.org/wiki/Chip-8) ROM may be
read and compiled into Lisp code.

Usage:

    > (chip-8-dynarec:compile-program #P"/path/to/game")
    => CHIP-8-PROGRAM:GAME
    > (chip-8-program:game display keyboard buzzer)
    ...


Compiled CHIP-8 programs are added to and exported from the package
`CHIP-8-PROGRAM`.


I/O interfaces
--------------

### Keyboard

Input is read from a hex keyboard with keys 0 to F (16 keys).

[Generic Function]  
__chip-8-keyboard:key-pressed?__ keyboard key => boolean

> Return _true_ if _key_ is currently pressed. Non-blocking.

[Generic Function]  
__chip-8-keyboard:wait-for-keypress__ keyboard => key

> Wait until a key has been pressed and return it.


### Display

The display is monochrome and has a resolution of 64x32 pixels.

[Generic Function]  
__chip-8-display:clear__ display => _no value_

> Clear _display_.

[Generic Function]  
__chip-8-display:draw__ display x y sprite => boolean

> Draw _sprite_ on _display_ at position _x_, _y_.
> 
> _Sprite_ is a vector of bytes where each byte is a horizontal line of 8 pixels.
> Pixels are xor:ed onto the display. Return _true_ if any pixel has been
> flipped from set to unset.


### Buzzer

The buzzer makes a beeping sound.

[Generic Function]  
__chip-8-buzzer:beep__ buzzer seconds => _no value_

> Make some noise for _seconds_.

[Generic Function]  
__chip-8-buzzer:silence__ buzzer => _no value_

> Stop making noise.
