pro extrapo,bres

; PURPOSE: Convert the output format of MANOLIS programm in the input for field_lines.pro

dim=size(bres)
bx3d=fltarr(dim(2),dim(3),dim(4))
by3d=fltarr(dim(2),dim(3),dim(4))
bz3d=fltarr(dim(2),dim(3),dim(4))

bx3d(*,*,*)=bres(0,*,*,*)
by3d(*,*,*)=bres(1,*,*,*)
bz3d(*,*,*)=bres(2,*,*,*)

save, filename='result_10484_24102003_0447.sav',Bx3d,By3d,Bz3d

End