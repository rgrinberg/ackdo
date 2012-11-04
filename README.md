# ackdo

ackdo is a companion to ack that allows you to preview changes made with
sed (or a similar tool) to ack's (or grep's) direct output

### Installing

To install simply run the following: (OCaml >= 3.12 is the only prereq)
```
$ make
$ sudo make install
```

### Usage

Say you want to rename a function foo_bar to fooBar.
To preview changes:
```
$ ack -w 'foo_bar' | sed 's/foo_bar/fooBar/' | ackdo 
```
If you are happy with these changes you can write them with:
```
$ ack -w 'foo_bar' | sed 's/foo_bar/fooBar/' | ackdo -d
```

TODO : document everything else

### Disclaimer

Use at your own peril. Ackdo comes with absolutely no warranty.

