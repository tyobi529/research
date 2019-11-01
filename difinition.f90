変数

tday:計算日数
thour:時間
tmin:分
tsec:秒


sub_connect_1.f90
DV:グレーチング１つあたりの地表面から雨水ますに流入する流量
   雨水ますから下水道に流入する流量
CDW:堰の公式の係数
CDO:オリフィスの公式の係数
L1:グレーチングの縦の長さ
L2:グレーチングの横の長さ
DL:グレーチングの周囲の長さ
GG:重力加速度
HD:雨水ますのピエゾ水頭
HM:地表面の水位
AG:グレーチングの断面積
CDME:1ユニットあたりの雨水ますの個数
CNCD:高さ？
B0:最小グレーチング幅
SDB_X:雨水ますの長さ
SDB_W:雨水ますの幅
ASDB:雨水ますの断面積

uu,vv:リンク中点の流速
um,vn:リンク中点のフラックス？um =（um*hl) "sub_flux.f90"のfluxで求める。
uum,vvm:重心の流速
umm,vnm:重心のフラックス
u11:連続式の移流項（3辺のリンクのフラックス×図心の速度）
u13:連続式の重力項


ファイル
"inputdata/manhole.dat" 11番
mnhl:マンホールの総数
id_mh(n):マンホールの番号
grp_mh(n):多分使ってない
no_mh(n):使ってない
bs_mh(n):マンホール底の標高
kubun_mh(n):
shp_mh(n):マンホールの形状
x_mh(n), y_mh(n):マンホールの座標
area_mh(n):断面積
num(n):マンホールにつながっている管の数(２なら直線,３ならT字の真ん中,１なら管の端)
(isw_mh(n,j), j=1,num(n)):同じ間でつながっているマンホールの番号
--------------------------------
MARK_SW_CHECK(i):(i=1~6,31,49,124,127は0、それ以外は1)
MARK_MH_CHECK(i):(i=3,4,32,37,39,42,44,45,47,49は0、それ以外は1)


"inputdata/conduit.dat" 12番
iswr:管の総数
id_sw(i):管の番号
idshp_sw(i):管の形状（1:長方形,2:円形）
idd1:管の幅(mm) dd1_sw(i):mに換算
idd2:管の高さ※円形の場合は半径
slp_sw(i):傾き（0.2なら1000mに対して0.2m下がる）
dst_sw(i):管の長さ（座標から算出しており、一つの管の合計が代入される）
bsup_sw(i), bsdw_sw(i):上流端/下流端の標高
mhup_sw(i), mhdw_sw(i):上流端/下流端に接続されているマンホール番号
ipt_sw(i):その管につながるマンホールの数（１つの直線なら２）
(x_sw(i,j), y_sw(i,j), j=1,ipt_sw(i)):管につながるマンホールの座標（"ipt_sw"の値によってデータの数が異なる）
jswr(i):20mの管何個分か（切り捨てなので50mの管なら2）


"inputdata/ogurisu_fujita_2.dat" 13番
node=73862
1~73862まで
i
dnox(no), dnoy(no) ノードのxy座標
MESH=146600
73862~MESHまで
N, I, J, K, MARK(ME)
MARK(ME)はメッシュの属性？建物・道路・河川・その他

"inputdata/h-ar.dat" 14番 下水管内の水深と断面積の変換
h_spc(k):2L/D？ L:潤辺
a_spc(k):A/A0(A:水がある部分の断面積)
r_spc(k):R/R0
A:水が溜まっている部分の断面積 A0:管の断面積 R:径深 R0:水がいっぱいの時の径深
h:水がある部分の水深 D:管の直径

"inputdata/bs_120.dat" 15番
baseo(me):メッシュの標高

"inputdata/x-rain-ogurisu_1110_b.dat" 16番 使ってない

"inputdata/mesh_ogurisu_fugita.dat" 17番
mesh=146600
ko(me) メッシュの辺(頂点)の数
(menode(me, k), k = 1, ko(me)) メッシュを構成するノードの番号（３つ）
(melink(me, k), k = 1, ko(me)) メッシュを構成するリンクの番号（３つ）
smesh(me) メッシュの面積(m**2)
xmesh(me), ymesh(me) メッシュの重心のxy座標
(rtuv(me, k), k = 1, ko(me)) 重心の速度(umm,vnm)を出すための重み（３つ）

"inputdata/link_ogurisu_fugita.dat" 18番
link=220461 リンクの数
limesh(li, 1), limesh(li, 2) そのリンクが接するメッシュの番号
linode(li, 1), linode(li, 2) そのリンクの両端のノード番号

scv(li) 連続した水面の流速(移流項)計算に必要な値
rthl(li, 1),rthl(li, 2) リンクの中点における水深を求めるための重み
ux(li), uy(li) 速度をxy方向に分解するための値
hl(li):メッシュの図心の水深

"inputdata/pump.dat"
"out/H-SDM-2.dat"
"out/U-SDM-2.dat"
"out/V-SDM-2.dat"
"out/sewer_ogurisu.dat"

SUBROUTINE STORM_BOX
NM:道路属性を持つメッシュの総数(計算用)
SDBR:道路属性を持つメッシュの総数

subroutine fluxsw
ac_cnq 流量に関するパラメータ？
q_con_total(con_mh(j))
q_con(con_mh(j))

meshデータ入力流れ

⑴"ORI-A-10-5-CNA-1.0.f90"での作業
data fn/ 'READFILE/ogurisu.dat', 'READFILE/NAKAHAMA_BENTEN.DAT'/
open(10, file = fn(kkk), action = 'read')
によって'ogurisu.dat'を開き10番を割りあてる

read(10, 1009) fmsh
によって'ogurisu.dat'内の13行目'inputdata/mesh_ogurisu_fugita.dat'を読み込みファイルをfmshに格納

open(17, file = fmsh, action = 'read')
によってfmesh('mesh_ogurisu_fugita.dat')を開き17番を割り当てる

call rdat
によって関数rdatを呼び出す。

⑵"sub_rdt.f90"での作業
read(17, 1701) mesh
によってmeshに'mesh_ogurisu_fugita.dat'の146600が入る
do me = 1, mesh
  read(17, 1702) ko(me), (menode(me, k), k = 1, ko(me))
  read(17, 1703) (melink(me, k), k = 1, ko(me))
  read(17, 1704) smesh(me), xmesh(me), ymesh(me)
  read(17, 1705) (rtuv(me, k), k = 1, ko(me))
!        xmesh(me) = xmesh(me) -  43995.00
!        ymesh(me) = ymesh(me) - 154999.00
  read(500,4000) ii(me), jj(me)
enddo
によって'mesh_ogurisu_fugita.dat'のデータを読み込む
