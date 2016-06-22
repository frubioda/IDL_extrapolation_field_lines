pro extrapolation

;**** This program is the basic extrapolation program.
;**** It takes as an input the value of the normal component
;**** of the magnetic field at the boundary. The file containing
;**** the input data is either a fits file or an IDL binary
;**** data file. Using an extrapolation method introdused
;**** by Alissandrakis,it reconstucts the 3-D magnetic
;**** field above the photosphere (boundary) giving as an
;**** output an IDL binary data file, which contains three
;**** 3-D arrays with the values of each one of the magnetic
;**** field components in the 3_d space.


tt=dialog_pickfile(FILTER='MDI_*.sav')
restore,tt
;bz2=mdi_struct.b_long
bz2=bz

dim=size(bz2)

;****TABLE DIMENSIONS****

fitsx=dim(1) ;**** The x dimension of the fits file
fitsy=dim(2) ;**** The y dimension of the fits file
ss=dim(1)*2     ;**** The dimension of the "anti-aliasing" array used
xd=dim(1)     ;**** The x dimension of the input and the output arrays
yd=dim(2)     ;**** The y dimension of the input and the output arrays
zd=dim(2)    ;**** The z dimension of the output arrays



;****TABLE USED****

;**** The 1-D arrays containing the coordinates in the F.T.domain
Uxspace=FLTARR(ss)
Vyspace=FLTARR(ss)

;**** The 2-D dummy arrays used for the calculation of the F.T.
;**** and the inverse F.T. of the solutions of the field during
;**** each height-step.
Bx=COMPLEXARR(ss,ss)
By=COMPLEXARR(ss,ss)
Bzz=COMPLEXARR(ss,ss)

;**** The array contained in the fits file
BZ2=FLTARR(fitsx,fitsy)

;**** The part of the fits file we use as an input array
BZ22=FLTARR(xd,yd)

;**** The "anti-aliasing" array for the boundary data
BZ2big=FLTARR(ss,ss)

;**** The F.T. of the boundary array
BZ2F=COMPLEXARR(ss,ss)

;**** The dummy arrays containing the real part of the results
;**** without the surrounding zeroes of the "anti-aliasing arrays",
;**** at each height-step
Bxf=FLTARR(xd,yd)
Byf=FLTARR(xd,yd)
Bzf=FLTARR(xd,yd)

;**** The output arrays
Bx3d=FLTARR(xd,yd,zd)
By3d=FLTARR(xd,yd,zd)
Bz3d=FLTARR(xd,yd,zd)



;****CONSTANTS****
pi=ACOS(-1.0)
N=ss
D=1
a=0.017



;****COUNTERS****
ii1=FIX(FLOAT(ss)/2.0)
ii2=ss-1



;****FILES OF DATA****

;restore, 'case_1_low.dat'

;**** Restoring the magnetogram from the fits file
;**** and selecting the area of interest.
;**** Subroutine MRDFITS is used to read fits file and is available at
;**** http://idlastro.gsfc.nasa.gov/mrdfits.html

;BZ2=MRDFITS('20030713_1130.fits')
restore,tt
;bz2=mdi_struct.b_long
bz2=bz

For i=0,dim(1)-1 Do Begin
        For j=0,dim(2)-1 Do Begin
         BZ22(i,j)=BZ2(i,j)
        EndFor
EndFor



;****ADD ZEROS BECAUSE OF THE ALIASING PROBLEM****
For i=0,ss/2-1 Do Begin
        For j=0,ss/2-1 Do Begin
           BZ2big(i+dim(1)/2,j+dim(2)/2)=BZ22(i,j)
        EndFor
EndFor



;****MAKE THE FFT OF THE BOUNDARY DATA****

BZ2F=FFT(BZ2big,-1)



;****MAKE THE TABLES WITH THE VARIABLES U,V IN THE F.T. DOMAIN

FOR i=0,ii1       DO  Uxspace(i)=float(N-(N-2.*i))/(float(2.*N)*D)
FOR i=ii1+1,ii2   DO  Uxspace(i)=float(-(N+(N-2.*i)))/(float(2.*N)*D)
FOR i=0,ii1       DO  Vyspace(i)=float(N-(N-2.*i))/(float(2.*N)*D)
FOR i=ii1+1,ii2   DO  Vyspace(i)=float(-(N+(N-2.*i)))/(float(2.*N)*D)



;****DEFINING THE (F.T.) COMPONENTS OF G AND B****
w=0
FOR z=0,zd-1 DO BEGIN
   print, z
   FOR i=0,ii2 DO BEGIN
      FOR j=0,ii2 DO BEGIN

         qq=SQRT(Uxspace(i)^2.0+Vyspace(j)^2.0)

          IF qq LT a/(2.0*pi) THEN BEGIN  ;*** setting the large scale
           Bx(i,j)=0.0                    ;*** spatial components to
           By(i,j)=0.0                    ;*** zero
           Bzz(i,j)=0.0
           w=w+1
          ENDIF ELSE BEGIN

          k=(4.0*pi^2.0*qq^2.0-a^2.0)^0.5
          Gx=COMPLEX(0.,-1.)*((Uxspace(i)*k-a*Vyspace(j))$
          *Exp(FLOAT(-z)*D*k) /(2.0*pi*qq^2.0))
          Bx(i,j)=Gx*BZ2F(i,j)
            Gy=COMPLEX(0,-1)*((Vyspace(j)*k+a*Uxspace(i))$
            *Exp(FLOAT(-z)*D*k) /(2.0*pi*qq^2.0))
            By(i,j)=Gy*BZ2F(i,j)
              Gz=EXP(FLOAT(-z)*D*k)
              Bzz(i,j)=Gz*BZ2F(i,j)
          ENDELSE

      ENDFOR
   ENDFOR



;****MAKE THE INVERSE F.T. OF THE COMPONENTS OF THE MAGNETIC FIELD****

   Bx=FFT(Bx, 1, /OVERWRITE)
   By=FFT(By, 1, /OVERWRITE)
   Bzz=FFT(Bzz, 1, /OVERWRITE)
     Bxf=Float(Bx(ss/2-xd/2:ss/2+xd/2-1,ss/2-yd/2:ss/2+yd/2-1))
     Byf=Float(By(ss/2-xd/2:ss/2+xd/2-1,ss/2-yd/2:ss/2+yd/2-1))
     Bzf=Float(Bzz(ss/2-xd/2:ss/2+xd/2-1,ss/2-yd/2:ss/2+yd/2-1))
       Bx3d(*,*,z)=Bxf
       By3d(*,*,z)=Byf
       Bz3d(*,*,z)=Bzf
ENDFOR
print, w

tt=strmid(tt,28,32)

save, filename='result_'+tt,Bx3d,By3d,Bz3d

END
