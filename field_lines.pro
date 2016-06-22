pro field_lines

tt=dialog_pickfile(Filter='result*.sav')

restore,tt
dim=size(bx3d)

;**** This programs draws the magnetic field lines above
;**** the photospheric boundary.
;**** It takes as an input the three 3-D arrays containing
;**** the components of the magnetic field, which were
;**** produced by the "extrapolation.pro" program.


device, decomposed=0
loadct,8
!P.Color=255


;****VARIOUS PARAMETERS****
nn=100   ;**** Number of field lines to be drawn
xs=700  ;**** x-dimension of the window
ys=700  ;**** y-dimension of the window



;****X,Y,Z DIMENSIONS OF THE ARRAYS****
ttx=dim(1)
tty=dim(2)
ttz=dim(3)



;****ARRAYS****
Bx3d=FLTARR(ttx,tty,ttz)  ;*** The 3-D array containing the
                          ; x component of the magnetic field
By3d=FLTARR(ttx,tty,ttz)  ;*** The 3-D array containing the
                          ; y component of the magnetic field
Bz3d=FLTARR(ttx,tty,ttz)  ;*** The 3-D array containing the
                          ; z component of the magnetic field
XX=INTARR(nn)             ;*** The X coordinates of the starting
                          ; points for the field lines
YY=INTARR(nn)             ;*** The Y coordinates of the starting
                          ; points for the field lines
SZ1=INTARR(nn)            ;*** The Z coordinates of the starting
                          ; points are set to zero
BZ0=FLTARR(ttx,tty)       ;*** The boundary data array



;****RESTORING THE INPUT DATA ARRAYS****
restore,tt
BZ0=bz3d(*,*,0)             ;*** The array containing the z component
                            ; of the field at the boundary level and
                            ; will be used for the contour plot at the
                            ; boundary
tt1=tt
;tt1=dialog_pickfile(Filter='*.sav')
;restore,tt1
;data=rebin(data,192,192)
;BZ1=data<200

;****CALL THE SUBROUTINE WHICH WILL DETERMINE THE LINES TO BE DRAWN****

;*** This subroutine allows the user to choose by himself
;*** the starting points of the field lines, by simply clicking
;*** with the mouse on the boundary surface.
;rat,XX,YY,nn,ttx,tty,FLOAT(BZ0)



;*** This subroutines selects the nn starting points of
;*** the field lines. A part of the nn points corresponds
;*** to those with the most intense magnetic field at the
;*** boundary level.The rest of the points correspond
;*** to those whose value of the normal component at
;*** the boundary is above a certain (adjustable)threshold.
autoselect,XX,YY,nn,ttx,tty,Float(BZ0)



;****Make the contour plot of the boundary and draw the axis****
!X.Margin=[0,0] & !Y.Margin=[0,0] & !Z.Margin=[0,0]
SCALE3,zrange=[0,200],Ax=20,Az=-20

WINDOW,XSIZE=xs,YSIZE=ys


maxbz0=max(abs(BZ0))
BZ0(0,0)=maxbz0
BZ0(1,0)=-maxbz0


contour,Float(BZ0),/fill,NLEVELS=370,Charsize=3.5,ZVALUE=0.0,$
       /T3D,XRANGE=[0,ttx],YRANGE=[0,tty],XSTYLE=1,YSTYLE=1


Axis,0,ttx,100,XAXIS=1,/save,/T3D,Charsize=999,XRANGE=[0,ttx],XSTYLE=1
Axis,0,0,100,YAXIS=0,/save,/T3D,Charsize=999,YRANGE=[0,tty],YSTYLE=1
Axis,0,0,0,ZAXIS=1,/save,/T3D,Charsize=2.5,ZRANGE=[0,ttz],ZSTYLE=1
Axis,0,ttx,0,ZAXIS=1,/save,/T3D,Charsize=999,ZRANGE=[0,ttz],ZSTYLE=1
Axis,ttx,ttx,0,ZAXIS=0,/save,/T3D,Charsize=999,ZRANGE=[0,ttz],ZSTYLE=1



;****DRAW THE FIELD LINES****
;xx(3)=155
;xx(4)=176
;xx(5)=178
;xx(6)=142
;xx(18)=143
;xx(13)=162
;yy(3)=110
;yy(4)=111
;yy(5)=112
;yy(6)=102
;yy(18)=100
;yy(13)=98
FLOW3,Bx3d,By3d,Bz3d,/BLOB,LEN=5,NSTEPS=32000,SX=XX,SY=YY,SZ=SZ1
;print,xx,yy


tt=strmid(tt,0,strpos(tt,'.'))
write_bmp,tt+'.bmp',TVRD()

END



;*************************************************************************************


;*** This subroutine allows the user to choose by himself
;*** the starting points of the field lines, by simply clicking
;*** with the mouse on the boundary surface.


Pro rat,XX,YY,nn,ttx,tty,BZ0



WINDOW,XSize=512, YSize=512
contour,BZ0,/fill,nlevels=100,xrange=[0,ttx],xstyle=1,$
        yrange=[0,tty],ystyle=1
XYOUTS,50,220,'Choose your no 1 point',CHARSIZE=1.3,/dev

i=0
!MOUSE.button=1


WHILE (!MOUSE.button NE 4 AND i LE nn-1) DO BEGIN
  cursor,a,b,/data,/UP
  EMPTY
  contour,BZ0,/fill,nlevels=100,xrange=[0,ttx],$
          xstyle=1,yrange=[0,tty],ystyle=1
    IF !Mouse.button EQ 1 THEN BEGIN
      c=a
      d=b
      posinf='x='+STRCOMPRESS(FIX(c)) + 'y=' + $
             STRCOMPRESS(FIX(d)) + ' ' + $
             STRCOMPRESS('Middle click to accept the point you have choosen,right click to exit')
      XYOUTS,50,220,posinf,CHARSIZE=1.3,/dev
   ENDIF

   IF !Mouse.button EQ 2 THEN BEGIN
     XX(i)=FIX(c)
     YY(i)=FIX(d)
     print,XX(i),YY(i)
     pointif='Choose your no' + STRCOMPRESS(i+2) + 'point'
       IF i LT nn-1 THEN BEGIN
         XYOUTS,50,220,posinf,CHARSIZE=1.3,/dev
       ENDIF ELSE BEGIN
         XYOUTS,50,220,'you can not choose more points',$
                CHARSIZE=1.3,/dev
       ENDELSE
     i=i+1
   ENDIF

ENDWHILE


END



;*************************************************************************************


;*** This subroutine selects the nn starting points of the
;*** field lines.A part of the nn points corresponds to those
;*** with the most intense magnetic field at the boundary
;*** level.The rest of the points correspond to those whose
;*** value of the normal component at the boundary is above a
;*** certain (adjustable) thereshold.


Pro autoselect,XX,YY,nn,ttx,tty,BZ0



BZ0dummy=FLTARR(ttx,tty)

BZ0dummy=BZ0
BZ0dummy2=BZ0

For i=0,nn-1-nn/3 DO BEGIN
  resultdummy=max(BZ0dummy,K)
  XX(i)=K MOD ttx
  YY(i)=K/tty
  BZ0dummy(XX(i),YY(i))=0.0
ENDFOR


bzmax=max(bz0dummy2)

FOR i=0,ttx-1 DO BEGIN
    FOR j=0,tty-1 DO BEGIN
      IF bz0dummy2(i,j) gt bzmax/2.0 THEN bz0dummy2(i,j)=0.0
    ENDFOR
ENDFOR


FOR i=nn-nn/3,nn-1 DO BEGIN
  resultdummy=max(BZ0dummy2,K)
  XX(i)=K MOD ttx
  YY(i)=K/tty
  BZ0dummy2(XX(i),YY(i))=0.0
ENDFOR

END
