REM >!RunImage
REM BarCode wimp front end proggy.
:
REM -- Greetings tired Hacker,
REM    Go rest you curious head,
REM    For if you keep looking at my code,
REM    The shock will strike you dead!
:
PROCinit
:
WHILE NOT quit%
PROCpoll
ENDWHILE
SYS "Wimp_CloseDown"
END
:
DEF PROCinit
maxws%=&300
DIM block% 255,name% 11,mainind% maxws%
DIM mainind2% maxws%
DIM wimpicon% 20
DIM imenu% 99
DIM P% 2048,Q% 2048
DIM blk% 984
DIM icontext% 40
DIM trans% 16,rect% 16,plotat% 8
path$="":type$="":obj$=""
desc$="Object is a"
DIM sbspr% 8,sbtext% 255,sbval% 3
$sbspr%="file_0b4":$sbtext%="BarCode"
$sbval%="A~ "
ver$="1.05 (25/4/94)"
quit%=FALSE:app$="BarCode"
SYS "Wimp_Initialise",200,&4B534154,app$
ON ERROR PROCerror(REPORT$+" at line "+STR$ERL)
file%=OPENIN"<BarCode$Dir>.!Sprites"
size%=EXT#file%+4:CLOSE#file%
DIM sparea% size%
!sparea%=size%:sparea%!4=0
sparea%!8=16
sparea%!12=16
SYS "OS_SpriteOp",&10A,sparea%,"<BarCode$Dir>.!Sprites"
SYS "Wimp_OpenTemplate",,"<BarCode$Dir>.Templates"
SYS "Wimp_LoadTemplate",,P%,mainind%,mainind%+maxws%,-1,"info",0
REM SYS "Wimp_LoadTemplate",,Q%,mainind2%,mainind2%+maxws%,-1,"savewin",0
SYS "Wimp_CreateWindow",,P% TO info%
REM SYS "Wimp_CreateWindow",,Q% TO save%
SYS "Wimp_CloseTemplate"
save%=FNcreate_window(0,0,264,164,0,0,&84000012,"Save as:")
a%=FNcreate_icon(save%,100,-92,68,68,&6102,"",sbspr%,1,9)
a%=FNcreate_icon(save%,8,-156,192,48,&700F13D,"",sbtext%,sbval%,256)
a%=FNcreate_icon(save%,208,-156,48,48,&C701903D,"OK",0,0,0)
PROCwrite_t(info%,4,ver$)
PROCsetupmenu(imenu%)
$wimpicon%="!BarCode"
block%!0=-1:block%!4=0:block%!8=0:block%!12=34*2:block%!16=17*4
block%!20=&0700311A:block%!24=wimpicon%:block%!28=-1:REM sparea%
block%!32=LEN("!barcode")
SYS "Wimp_CreateIcon",,block% TO ibhandle%
REM ibhandle%=FNcreate_icon(-1,0,0,68,68,&3002,"!drawcad",0,0,0)
ANumber$="012345678900"
Ln=12
Fmt=0
AandB=0
DIM Sets$(10)
FOR X=0 TO 9
READ Sets$(X)
NEXT
DIM Numbers$(10,3)
FOR X=0 TO 9
FOR Y=1 TO 3
READ Numbers$(X,Y)
NEXT
NEXT
Sopt=0
PROCload
ENDPROC
:
DEF PROCpoll
SYS "Wimp_Poll",0,block% TO reason%
CASE reason% OF
WHEN 2:SYS "Wimp_OpenWindow",,block%
WHEN 3:SYS "Wimp_CloseWindow",,block%
WHEN 6:PROCclick(block%!12)
WHEN 7:PROCstartsave
WHEN 8:PROCkeypress(block%!24)
WHEN 9:PROCmenuselect
WHEN 17,18:PROCreceive
REM WHEN 19:PROCreport("Transfer failed - receiver died",1)
ENDCASE
ENDPROC
:
DEF PROCreport(err$,flag%)
name$=app$
IF flag% AND 16 THEN name$="Message from "+name$
!block%=255
$(block%+4)=err$+CHR$0
SYS "Wimp_ReportError",block%,flag%,name$ TO ,errclick%
ENDPROC
:
DEF PROCerror(a$)
PROCreport(a$,1)
SYS "Wimp_CloseDown"
END
ENDPROC
:
DEF FNcreate_window(x%,y%,w%,h%,extx%,exty%,flags%,title$)
:
REM visible work area
!block%=x%
block%!4=y%
block%!8=x%+w%
block%!12=y%+h%
:
REM scroll offsets
block%!16=0
block%!20=0
:
REM handle behind and window flags
block%!24=-1
block%!28=flags%
:
REM window colours
block%?32=7
block%?33=2
block%?34=7
block%?35=1
block%?36=3
block%?37=1
block%?38=2
:
REM work area extent
block%!40=0
block%!44=-h%-exty%
block%!48=w%+extx%
block%!52=0
:
REM title bar and work area flags
block%!56=&19
block%!60=3<<12
:
REM sprite area pointer and minimum size
block%!64=1
block%!68=0
:
REM window title
$(block%+72)=title$
:
REM number of icons
block%!84=0
:
SYS "Wimp_CreateWindow",,block% TO handle%
=handle%
:
DEF PROCclick(win%)
CASE win% OF
WHEN info%:PROCinfo_sel(block%!16)
WHEN -2:PROCibar(block%!8)
WHEN save%:
CASE block%!16 OF
WHEN 0:IF block%!8 AND 64 PROCdragbox
WHEN 2:IF block%!8 AND 5 PROCquicksave
ENDCASE
ENDCASE
ENDPROC
:
DEF PROCkeypress(key%)
CASE TRUE OF
WHEN key%=13 AND !block%=save%
PROCquicksave
OTHERWISE:SYS "Wimp_ProcessKey",key%
ENDCASE
ENDPROC
:
DEF FNcreate_icon(whan%,ix%,iy%,iw%,ih%,flag%,text$,ptr1%,ptr2%,ptr3%)
!block%=whan%
block%!4=ix%
block%!8=iy%
block%!12=ix%+iw%
block%!16=iy%+ih%
block%!20=flag%
IF ptr1%=0 THEN
$(block%+24)=text$
ELSE
block%!24=ptr1%
block%!28=ptr2%
block%!32=ptr3%
ENDIF
SYS "Wimp_CreateIcon",,block% TO ihandle%
=ihandle%
:
DEF PROCibar(button%)
CASE button% OF
WHEN 1,4:PROCibar_click
WHEN 2:PROCload:PROCshowmenu(imenu%,!block%-64,300)
ENDCASE
ENDPROC
:
DEF PROCibar_click
*Filer_Run <BarCode$Dir>.BarCdMnPg
ENDPROC
:
DEF PROCsetupmenu(menu%)
READ title$,num%:$menu%=title$
width%=(LEN(title$)-2)*16
menu%!12=&70207:menu%!20=44:menu%!24=0
ptr%=menu%+28:FOR i%=1 TO num%
READ mflags%,subptr%,item$
!ptr%=mflags%:ptr%!4=subptr%
ptr%!8=&7000021:$(ptr%+12)=item$
a%=(LEN(item$)+1)*16
IF a%>width% width%=a%
ptr%+=24:NEXT
menu%!16=width%
ENDPROC
:
DEF PROCshowmenu(menu%,mx%,my%)
SYS "Wimp_CreateMenu",,menu%,mx%,my%
ENDPROC
:
DEF PROCmenuselect
sel1%=!block%:sel2%=block%!4
SYS "Wimp_GetPointerInfo",,block%
button%=block%!8
CASE sel1% OF
WHEN 1:PROCquicksave
WHEN 2:PROCprint
WHEN 3:quit%=TRUE
ENDCASE
IF button%=1 THEN PROCshowmenu(imenu%,0,0)
ENDPROC
:
DEF PROCreceive
CASE block%!16 OF
WHEN 0:quit%=TRUE
WHEN 2:PROCdatasave
WHEN 3:PROCdataload(block%!40)
ENDCASE
ENDPROC
:
DEF PROCdataload(type%)
IF type%<>&0B4 THEN ENDPROC
path$=FNstring(block%+44)
PROCload_file(path$)
PROCsave
$sbtext%=path$
ENDPROC
:
DEF FNstring(ptr%)
LOCAL a$
WHILE ?ptr%<>0
a$+=CHR$(?ptr%):ptr%+=1
ENDWHILE
=a$
:
DEF PROCstartsave
SYS "Wimp_GetPointerInfo",,block%
block%!20=block%!12:block%!24=block%!16
block%!28=!block%:block%!32=block%!4
block%!36=LENobj$+LENpath$+LENtype$+3
!block%=64:block%!12=0
block%!16=1:block%!40=&0B4
$(block%+44)=FNgetleaf($sbtext%)
SYS "Wimp_SendMessage",18,block%,block%!20,block%!24
ENDPROC
:
DEF PROCquicksave
IF INSTR($sbtext%,".") THEN
PROCsaveit
ELSE
PROCreport("To save, drag the icon to a directory viewer",1)
ENDIF
ENDPROC
:
DEF PROCsaveit
PROCload
CASE Sopt OF
WHEN 0:
file%=OPENOUT($sbtext%)
BPUT#file%,ANumber$
BPUT#file%,AandB
BPUT#file%,Fmt
CLOSE#file%
OSCLI "SetType "+$sbtext%+" 0B4"
WHEN 1:PROCdraw
WHEN 2:PROCsprite
ENDCASE
ENDPROC
:
DEF FNgetleaf(a$)
WHILE INSTR(a$,".")
a$=MID$(a$,INSTR(a$,".")+1)
ENDWHILE
=a$+CHR$0
:
DEF PROCdatasave
$sbtext%=FNstring(block%+44)
PROCsaveit
block%!12=block%!8
block%!16=3:!block%=256
SYS "Wimp_SendMessage",18,block%,block%!20,block%!24
ENDPROC
:
DEF PROCdragbox
!block%=save%
SYS "Wimp_GetWindowState",,block%
ox%=block%!4-block%!20
oy%=block%!16-block%!24
block%!4=0
SYS "Wimp_GetIconState",,block%
block%!4=5:block%!8=ox%+block%!8
block%!12=oy%+block%!12
block%!16=ox%+block%!16
block%!20=oy%+block%!20
block%!24=0:block%!28=0
block%!32=&7FFFFFFF
block%!36=&7FFFFFFF
SYS "Wimp_DragBox",,block%
ENDPROC
:
DEF PROCwrite_t(win%,ic%,text$)
!block%=win%:block%!4=ic%
SYS "Wimp_GetIconState",,block%
$(block%!28)=text$
!block%=win%:block%!4=ic%
block%!8=0:block%!12=0
SYS "Wimp_SetIconState",,block%
ENDPROC
:
DEF PROCinfo_sel(no)
IF no=10 THEN *Filer_Run <BarCode$Dir>.Manual
ENDPROC
:
DEF PROCprint
PROCload
OSCLI ("RMEnsure PDriver 0 ERROR 255 No printer driver installed")
SYS "Hourglass_On"
pf%=OPENOUT("printer:")
SYS "PDriver_SelectJob",pf%,"Test job"
LOCAL ERROR
ON ERROR LOCAL:RESTORE ERROR:SYS "PDriver_AbortJob",pf%:CLOSE#pf%:SYS "Hourglass_Off":PROCreport(REPORT$+" : "+STR$(ERL),1):ENDPROC
SYS "PDriver_PageSize" TO ,w%,h%,l%,b%,r%,t%
!rect%=0:rect%!4=0
rect%!8=856:rect%!12=1030
!trans%=1<<16:trans%!4=0
trans%!8=0:trans%!12=1<<16
!plotat%=l%+108000:plotat%!4=b%+216000
SYS "PDriver_GiveRectangle",0,rect%,trans%,plotat%,&DDDDDD00
SYS "PDriver_DrawPage",1,block%,0,0 TO more%
PROCgenview
SYS "Font_CacheAddr" TO version%,cacheused%,cachesize%
SYS "Font_FindFont",,"Trinity.Bold",24*16,24*16,0,0 TO Text%
SYS "Font_FindFont",,"System.Fixed",40*16,40*16,0,0 TO Text3%
SYS "Font_SetPalette",,1,2,3,&88888800,&FFFFFF00
SYS "Font_SetFontColours",0,1,2,3
WHILE more%
SYS "ColourTrans_SetGCOL",0
PROCview
SYS "PDriver_GetRectangle",,block% TO more%
ENDWHILE
SYS "Font_LoseFont",Text%
SYS "Font_LoseFont",Text3%
SYS "PDriver_EndJob",pf%
SYS "Hourglass_Off"
RESTORE ERROR
CLOSE#pf%
ENDPROC
:
DEF PROCset_fcol(r,g,b,r2,g2,b2)
LOCAL set_fcol%
r=r*(1/255)
g=g*(1/255)
b=b*(1/255)
r2=r2*(1/255)
g2=g2*(1/255)
b2=b2*(1/255)
SYS "OS_SWINumberFromString",,"ColourTrans_SetFontColours" TO set_fcol%
SYS set_fcol%,0,((r2*&FF)<<8)+((g2*&FF)<<16)+((b2*&FF)<<24),((r*&FF)<<8)+((g*&FF)<<16)+((b*&FF)<<24),14
ENDPROC
:
DEF PROCcentre(y%,t$,hand%)
LOCAL x1%,y1%
 SYS "Font_SetFont",hand%
 SYS "Font_StringBBox",,t$ TO ,,,x1%,y1%
 x1%=x1%*180/72000
 y1%=y1%*180/72000
 SYS "Font_Paint",,t$,16,428-x1%/2,y%-y1%/2
ENDPROC
:
DEF PROCwhere(x%,y%,t$,hand%)
 SYS "Font_SetFont",hand%
 SYS "Font_Paint",,t$,16,x%,y%
ENDPROC
:
DEF PROCgenview
LOCAL X,Y,Z
BCode$="DLD"
FOR X=1 TO Ln/2
Add=VAL(MID$(ANumber$,X,1))
CASE MID$(Sets$(AandB),X,1) OF
WHEN "A":BCode$+=Numbers$(Add,1)
WHEN "B":BCode$+=Numbers$(Add,2)
ENDCASE
NEXT
BCode$+="LDLDL"
FOR X=Ln/2+1 TO Ln
Add=VAL(MID$(ANumber$,X,1))
BCode$+=Numbers$(Add,3)
NEXT
BCode$+="DLD"
ENDPROC
:
DEF PROCview
SYS "ColourTrans_SetGCOL",&FFFFFF00
REM RECTANGLE FILL 202,162,840,588
RECT FILL 0,0,1080,1030
FOR X=1 TO LEN(BCode$)
IF MID$(BCode$,X,1)="L" THEN SYS "ColourTrans_SetGCOL",&FFFFFF00 ELSE SYS "ColourTrans_SetGCOL",&00000000
RECT FILL 60+((X-1)*8),212,8,528
NEXT
SYS "ColourTrans_SetGCOL",&FFFFFF00
RECT FILL 60+24,172,42*8,100
RECT FILL 60+24+42*8+35,172,42*8,100
PROCset_fcol(0,0,0,255,255,255)
PROCwhere(0,172,STR$(AandB),Text3%)
PROCwhere(60+44,172,LEFT$(ANumber$,6),Text3%)
PROCwhere(60+32+42*8+35,172,RIGHT$(ANumber$,6),Text3%)
F$="Unknown"
CASE Fmt OF
WHEN 0:F$="EAN"
WHEN 1:F$="ITF"
ENDCASE
PROCset_fcol(0,0,0,255,255,255)
PROCcentre(950,"Article Number : "+ANumber$,Text%)
PROCcentre(880,"Style : "+Sets$(AandB)+" CCCCCC ("+STR$(AandB)+")",Text%)
PROCcentre(810,"Format : "+F$,Text%)
PROCcentre(50,"LEN Copyright R.J.Wareham 1994",Text%)
ENDPROC
:
DEF PROCload
file%=OPENIN("<BarCode$Dir>.Sopts")
Sopt=BGET#file%
CLOSE#file%
CASE Sopt OF
WHEN 0:$sbspr%="file_0B4"
WHEN 1:$sbspr%="file_AFF"
WHEN 2:$sbspr%="file_FF9"
ENDCASE
ANumber$=""
file%=OPENIN("<BarCode$Dir>.Temp")
FOR Scn=1 TO 12
TEMP=BGET#file%
ANumber$+=CHR$(TEMP)
NEXT
DUMMY=BGET#file%
AandB=BGET#file%
Fmt=BGET#file%
CLOSE #file%
ENDPROC
:
DEF PROCload_file(p$)
ANumber$=""
file%=OPENIN(p$)
FOR Scn=1 TO 12
TEMP=BGET#file%
ANumber$+=CHR$(TEMP)
NEXT
DUMMY=BGET#file%
AandB=BGET#file%
Fmt=BGET#file%
CLOSE #file%
PROCsave
ENDPROC
:
DEF PROCsave
file%=OPENOUT("<BarCode$Dir>.Temp")
BPUT#file%,ANumber$
BPUT#file%,AandB
BPUT#file%,Fmt
CLOSE#file%
ENDPROC
:
DEF PROCsprite
file%=OPENOUT("<BarCode$Dir>.SpName")
BPUT#file%,$sbtext%
CLOSE#file%
*Filer_Run <BarCode$Dir>.Sprite
ENDPROC
:
DEF PROCdraw
file%=OPENOUT("<BarCode$Dir>.SpName")
BPUT#file%,$sbtext%
CLOSE#file%
file%=OPENOUT($sbtext%)
BPUT#file%,$sbtext%
CLOSE#file%
*Filer_Run <BarCode$Dir>.Draw
OSCLI("SetType "+$sbtext%+" AFF")
ENDPROC
:
DATA BarCode,4,0,info%,Info,0,save%,Save,%10,-1,Print,&80,-1,Quit
DATA AAAAAA,AABABB,AABBAB,AABBBA,ABAABB
DATA ABBAAB,ABBBAA,ABABAB,ABABBA,ABBABA
DATA LLLDDLD,LDLLDDD,DDDLLDL
DATA LLDDLLD,LDDLLDD,DDLLDDL
DATA LLDLLDD,LLDDLDD,DDLDDLL
DATA LDDDDLD,LDLLLLD,DLLLLDL
DATA LDLLLDD,LLDDDLD,DLDDDLL
DATA LDDLLLD,LDDDLLD,DLLDDDL
DATA LDLDDDD,LLLLDLD,DLDLLLL
DATA LDDDLDD,LLDLLLD,DLLLDLL
DATA LDDLDDD,LLLDLLD,DLLDLLL
DATA LLLDLDD,LLDLDDD,DDDLDLL
