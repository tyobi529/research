module sub_pm_pavement

use globals
use subprogs

implicit none
contains

!     ====================================
!           透水性舗装のプログラム
!     ====================================

SUBROUTINE PM_PAVEMENT
real(8) pm_smesh, qf, k0, kf, a, H, b
pm_smesh = 0
H = 1.5
a = 0.014
b = 1.287

! 比浸透量算出
kf = a*H + b


! 道路メッシュの合計面積
do me = 1, mesh
  if(MARK(me)==1)then
  pm_smesh = pm_smesh + smesh(me)
  endif
enddo

! 道路全体の浸透能力
qf = kf*pm_smesh











END SUBROUTINE PM_PAVEMENT

end module sub_pm_pavement
