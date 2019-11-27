# x16-life
 Conway's Game of Life for Commander X16

This is an implementation that pre-sets the cells with their neighbor counts so there's no need to check neighbors when making generations.  It's pretty fast but I didn't see an obvious way to implement wrapping which bummed me out.

In 6502 using the ca65 assembler (Part of cc65 toolchain).

![](life.gif)

Stefan Wessels
27 November 2019 - Initial Revision
