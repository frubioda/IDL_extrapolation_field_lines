pro extrapo_rev

filename=DIALOG_PICKFILE(FILTER='*.sav')
restore,filename

dim=size(bx3d)
bres=fltarr(3,dim(1),dim(2),dim(2))

bres(0,*,*,*)=bx3d(*,*,*)
bres(1,*,*,*)=by3d(*,*,*)
bres(2,*,*,*)=bz3d(*,*,*)

save, filename='m_'+strmid(filename,strpos(filename,'result')),Bres

End