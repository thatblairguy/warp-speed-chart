      integer*2 pen (4), array1(9)
      real array2(7)
      character*40 string

      common /pens/ pen

c
c  Prevent the user from accidentally terminating the program while
c  still in video mode.  This line is NOT standard Fortran 77.
c
      option nbreak

c
C  Set up for video mode.
c
	call plots (0, 0, 0)   ! Video output, direct video writes, autodetect.

c
c  Find what video mode was selected, if not at least 640 by 200 pixels
c  with 16 colors, then abort with a message.
c
      call grinfo (array1, array2, string)
      if ((array1 (5) .ne. 13) .and. (array1 (5) .ne. 14) .and.
     +         (array1 (5) .ne. 16) .and. (array1 (5) .ne. 18) .and.
     +         (array1 (5) .ne. 19)) then
           call plot (0.0, 0.0, 999)
           stop "Sorry!  This program requires color EGA or VGA."
      endif

c
c  Set up the most used plotting colors.
c
      pen (1) = 0  ! Black
      pen (2) = 9  ! Blue
      pen (3) = 4  ! Red
      pen (4) = 15 ! White

      call warp_function ()

c
c  Terminate video mode.
c
      call plot (0.0, 0.0, 999)

      end



      subroutine warp_function ()
      integer*2 pen (4)
      real warp, x, x_pt, y_pt
      integer*1 pos /1/

      common /pens/ pen

c
c  Velocity = (Warp Factor) ^ 3  x  (light speed)
c  (From Star Fleet Technical Manual, Technical Order # 02:06:20)
c
      warp (x) = x**3

      call clear (4)   ! Clear the screen to White (Pen #4).


c     Draw the axis system in red
      call newpen(pen(3))
      call axisb (1.25, 0.5, "Warp Factor", -11, 9.0, 0.0, 0.0, 1.0)
      call axisb (1.25, 0.5, "Multiple of Light Speed  ",  25, 7.0,
     +         90.0, 0.0, 100.0)


c     Scale down factor of 105.0 actually fits a Y axis of 735 units.
c     Since the AXIS routine only deals with axes of whole number
c     lengths, we have to be content to let the light speed for warp 9
c     (729c) go "Off the scale"
      y_pt = (warp (1.0) / 105.0) + 0.5
      call plot (2.25, y_pt, 3)


      call newpen (2)          ! Initially, plot in green.

      do x_pt = 2.25, 10.25, 0.1
           if (x_pt .GT. 7.25) then
               if (pos .EQ. 1) then      ! Greater than Warp 6, Scotty 
                   call newpen (14)      ! gets fidgety.  Yellow Alert!
                   pos = 2
               else if (x_pt .GT. 9.25) then
                   if (pos .EQ. 2) then     ! Greater than Warp 8, the poor
                       call newpen (4)      ! bairns can't take much more
                       pos = 3              ! of this abuse.  Condition red.
                   endif
               endif
           endif

           y_pt = (warp (x_pt - 1.25) / 105.0) + 0.5
           call plot (x_pt, y_pt, 2)
      enddo

      call newpen(pen(2))
      call symbol (0.5, 8.0, 0.4, "Warp Factor versus light speed",
     +                 0.0, 30)
      call symbol (0.5, 7.6, 0.4, "for Constitution class Starships.",
     +                 0.0, 33)
      call wait_key ()

      end



c  Clear the screen to the specified pen color.
      subroutine clear (color)
      integer*2 color
      real x(4), y(4)
      integer*2 pen (4)
      data x / 0.0, 0.0, 11.0, 11.0 /
      data y / 0.0, 8.5, 8.5, 0.0 /
      common /pens/ pen
      call newpen (pen(color))
      call fill (4, x, y)
      end


      subroutine wait_key ()
      logical*1 iskey

10    if (.not. iskey()) then
           goto 10
       else
           call kbclr ()
      endif
      end
