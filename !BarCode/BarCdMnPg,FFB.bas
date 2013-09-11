REM >BrCdMnPrg
REM Bar code main proggy.
:
MODE 15
PROCinit
PROCmain_menu
PROCfinish
*DESKTOP
END
:
DEF PROCinit
CLS
OFF
SYS "Hourglass_On"
ON ERROR SYS "Hourglass_Smash":REPORT:PRINT " : ";ERL:END
DIM Menus$(10,10)
DIM MenItems(10)
READ NoMen
FOR Y=1 TO NoMen
READ Items
MenItems(Y)=Items
FOR X=1 TO Items
READ Item$
Menus$(Y,X)=Item$
NEXT
NEXT
MOUSE COLOUR 1,0,0,0
MOUSE COLOUR 2,255,255,255
ANumber$="012345678900"
Ln=12
Fmt=0
AandB=0
PROCfonts
PROCset_fcol(255,255,255,0,0,0)
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
PROCget_env
IF env$<>"" THEN PROCload_file(env$)
SYS "Hourglass_Smash"
ENDPROC
:
DEF PROCget_env
LOCAL I%
SYS "OS_GetEnv" TO env$
IF INSTR(env$,"-quit") THEN
I%=INSTR(env$,"""")
I%=INSTR(env$,"""",I%+1)
REPEAT
I%+=1
UNTIL MID$(env$,I%,1)<>" "
env$=MID$(env$,I%)
ENDPROC
:
DEF PROCload
file%=OPENIN("<BarCode$Dir>.Sopts")
Sopt=BGET#file%
CLOSE#file%
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
DEF PROCfonts
SYS "Font_CacheAddr" TO version%,cacheused%,cachesize%
SYS "Font_FindFont",,"Trinity.Medium",24*16,24*16,0,0 TO Text%
SYS "Font_FindFont",,"Trinity.Bold",70*16,70*16,0,0 TO Text2%
SYS "Font_FindFont",,"System.Fixed",40*16,40*16,0,0 TO Text3%
SYS "Font_SetPalette",,1,2,3,&88888800,&FFFFFF00
SYS "Font_SetFontColours",0,1,2,3
ENDPROC
:
DEF PROCcentre(y%,t$,hand%)
LOCAL x1%,y1%
 SYS "Font_SetFont",hand%
 SYS "Font_StringBBox",,t$ TO ,,,x1%,y1%
 x1%=x1%*180/72000
 y1%=y1%*180/72000
 SYS "Font_Paint",,t$,16,640-x1%/2,y%-y1%/2
ENDPROC
:
DEF PROCwhere(x%,y%,t$,hand%)
 SYS "Font_SetFont",hand%
 SYS "Font_Paint",,t$,16,x%,y%
ENDPROC
:
DEF PROCdel(W)
LOCAL D
FOR D=1 TO W:WAIT:NEXT
ENDPROC
:
DEF PROCset_gcol(r,g,b,D)
LOCAL FLAGS%,set_gcol%
r=r*(1/255)
g=g*(1/255)
b=b*(1/255)
FLAGS%=&100
IF D THEN FLAGS%=FLAGS%-&100
SYS "OS_SWINumberFromString",,"ColourTrans_SetGCOL" TO set_gcol%
SYS set_gcol%,((r*&FF)<<8)+((g*&FF)<<16)+((b*&FF)<<24),,,FLAGS%
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
DEF PROCmain_menu
REPEAT
Ch=FNmenu("Main Menu",1)
IF Ch=1 THEN
REPEAT
Ch2=FNmenu("New Barcode",2)
CASE Ch2 OF
REM WHEN 0:PROCFormat
WHEN 0:PROCarticle
WHEN 1:PROCAandB
WHEN 2:PROCview
ENDCASE
UNTIL Ch2=3
ENDIF
IF Ch=0 THEN PROCinfo
IF Ch=2 THEN PROCview
IF Ch=3 THEN PROCsav_opt
UNTIL Ch=4
ENDPROC
:
DEF FNmenu(title$,n)
PROCclear_mouse
PROCflush
LOCAL StartY,Y,X,B,HI,OHI,x1%,y1%
CLS
SYS "Font_SetFont",Text2%
SYS "Font_StringBBox",,title$ TO ,,,x1%,y1%
x1%=x1%*180/72000
y1%=y1%*180/72000
PROCset_gcol(64,128,255,1)
RECT FILL 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
GCOL 63 TINT 0
RECT 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
PROCset_fcol(255,128,64,64,128,255)
PROCcentre(900,title$,Text2%)
PROCset_fcol(255,255,255,0,0,0)
StartY=400+(100*(MenItems(n)/2))
FOR Y=0 TO MenItems(n)-1
REM PROCwhere(200,StartY-(Y*100),STR$(Y+1)+")..",Text%)
PROCcentre(StartY-(Y*100),Menus$(n,Y+1),Text%)
NEXT
MOUSE RECT 200,850-StartY+10,880,MenItems(n)*100-20
HI=0
OHI=0
GCOL 3,63 TINT 255
RECT FILL 200,StartY-50-(HI*100),880,100
REPEAT
RECT FILL 200,StartY-50-(HI*100),880,100
MOUSE X,Y,Z
HI=((MenItems(n)*100)-(Y-(850-StartY))) DIV 100
IF HI<>OHI THEN OHI=HI
REM SOUND 1,-15,153+(HI*4),1
RECT FILL 200,StartY-50-(HI*100),880,100
WAIT
UNTIL Z=4
PROCclear_mouse
MOUSE RECT 0,0,1280,1030
=HI
:
DEF PROCclear_mouse
LOCAL X,Y,Z
REPEAT
MOUSE X,Y,Z
UNTIL Z=0
ENDPROC
:
DEF PROCfinish
SYS "Font_LoseFont",Text%
SYS "Font_LoseFont",Text2%
SYS "Font_LoseFont",Text3%
PROCflush
PROCsave
ENDPROC
:
DEF PROCsave
file%=OPENOUT("<BarCode$Dir>.Sopts")
BPUT#file%,Sopt
CLOSE#file%
file%=OPENOUT("<BarCode$Dir>.Temp")
BPUT#file%,ANumber$
BPUT#file%,AandB
BPUT#file%,Fmt
CLOSE#file%
ENDPROC
:
DEF PROCarticle
LOCAL x1%,y1%,X,Y,Z,In$
PROCflush
CLS
SYS "Font_SetFont",Text2%
SYS "Font_StringBBox",,"Set Article NUSR" TO ,,,x1%,y1%
x1%=x1%*180/72000
y1%=y1%*180/72000
PROCset_gcol(64,255,64,1)
RECT FILL 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
GCOL 63 TINT 0
RECT 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
PROCset_fcol(64,64,64,64,255,64)
PROCcentre(900,"Set Article NUSR",Text2%)
PROCset_fcol(255,255,255,0,0,0)
PROCcentre(700,"Type in the "+STR$(Ln)+" digit article number below :-",Text%)
REPEAT
In$=""
REPEAT
G=GET
IF (G>=48 AND G<=57) OR (G=8) THEN
IF G=8 AND LEN(In$)>0 THEN In$=LEFT$(In$,LEN(In$)-1)
IF G<>8 In$+=CHR$(G)
WAIT
GCOL 0 TINT 0
RECT FILL 0,200,1280,300
PROCcentre(350,In$,Text2%)
ENDIF
UNTIL LEN(In$)=Ln
PROCcentre(20,"Is this correct? (Y/N)",Text%)
REPEAT
YN$=GET$
UNTIL YN$="Y" OR YN$="N" OR YN$="y" OR YN$="n"
RECT FILL 0,0,1280,650
UNTIL YN$="Y" OR YN$="y"
ANumber$=In$
ENDPROC
:
DEF PROCflush
REPEAT
A$=INKEY$(0)
UNTIL A$=""
ENDPROC
:
DEF PROCAandB
LOCAL OCh3,Ch3,quit,mnu
mnu=0
quit=FALSE
OCh3=0
Ch3=0
REPEAT
OCh3=Ch3
Ch3=FNmenu("A and B set "+STR$(AandB),mnu+4)
REM IF mnu=0 THEN Ch3=FNmenu("A and B set "+STR$(OCh3),mnu+4)
REM IF mnu=1 THEN Ch3=FNmenu("A and B set "+STR$(OCh3+5),mnu+4)
IF mnu=0 AND Ch3<>5 AND Ch3<>6 THEN AandB=Ch3 ELSE IF mnu=1 AND Ch3<>5 AND Ch3<>6 THEN AandB=Ch3+5
IF Ch3=6 THEN quit=TRUE
IF Ch3=5 AND mnu=0 THEN mnu+=1:Ch3=OCh3
IF Ch3=5 AND mnu=1 THEN mnu-=1:Ch3=OCh3
UNTIL quit=TRUE
ENDPROC
:
DEF PROCsav_opt
LOCAL OCh3,Ch3,F$
OCh3=0
Ch3=Sopt
REPEAT
OCh3=Ch3
CASE OCh3 OF
WHEN 0:F$="BARCODE"
WHEN 1:F$="DRAW"
WHEN 2:F$="SPRITE"
ENDCASE
Ch3=FNmenu("Save "+F$,6)
UNTIL Ch3=3
Sopt=OCh3
ENDPROC
:
DEF PROCFormat
ENDPROC
LOCAL OCh3,Ch3
OCh3=0
Ch3=Fmt
REPEAT
OCh3=Ch3
CASE OCh3 OF
WHEN 0:F$="EAN"
WHEN 1:F$="ITF"
ENDCASE
Ch3=FNmenu("Format "+F$,3)
UNTIL Ch3=2
Fmt=OCh3
ENDPROC
:
DEF PROCview
LOCAL X,BCode$,Y,Z,Add,F$
CLS
PROCclear_mouse
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
GCOL 63 TINT 255
REM RECTANGLE FILL 202,162,840,588
RECT FILL 0,0,1280,1030
FOR X=1 TO LEN(BCode$)
IF MID$(BCode$,X,1)="L" THEN GCOL 63 TINT 255 ELSE GCOL 0 TINT 0
RECT FILL 272+((X-1)*8),212,8,528
NEXT
GCOL 63 TINT 255
RECT FILL 272+24,172,42*8,100
RECT FILL 272+24+42*8+35,172,42*8,100
PROCset_fcol(0,0,0,255,255,255)
PROCwhere(212,172,STR$(AandB),Text3%)
PROCwhere(272+44,172,LEFT$(ANumber$,6),Text3%)
PROCwhere(272+32+42*8+35,172,RIGHT$(ANumber$,6),Text3%)
CASE Fmt OF
WHEN 0:F$="EAN"
WHEN 1:F$="ITF"
ENDCASE
PROCset_fcol(128,192,128,255,255,255)
PROCcentre(950,"Article Number : "+ANumber$,Text%)
PROCcentre(880,"Style : "+Sets$(AandB)+" CCCCCC ("+STR$(AandB)+")",Text%)
PROCcentre(810,"Format : "+F$,Text%)
PROCset_fcol(0,0,0,255,255,255)
PROCcentre(100,"Press a mouse button to continue",Text%)
REPEAT
MOUSE X,Y,Z
UNTIL Z<>0
PROCclear_mouse
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
DEF PROCinfo
LOCAL X,Y,Z,x1%,y1%
PROCclear_mouse
CLS
SYS "Font_SetFont",Text2%
SYS "Font_StringBBox",,"Information" TO ,,,x1%,y1%
x1%=x1%*180/72000
y1%=y1%*180/72000
PROCset_gcol(64,255,64,1)
RECT FILL 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
GCOL 63 TINT 0
RECT 640-x1%/2-20,900-y1%/2-20,x1%+40,y1%+40
PROCset_fcol(64,64,64,64,255,64)
PROCcentre(900,"Information",Text2%)
PROCset_fcol(255,255,255,0,0,0)
PROCcentre(700,"BarCode",Text2%)
PROCcentre(500,"Barcode Creator",Text%)
PROCcentre(400,"LEN Copyright 1994 R.J.Wareham",Text%)
PROCcentre(300,"1.05 (25/4/94)",Text%)
PROCset_fcol(255,64,255,0,0,0)
PROCcentre(100,"Press a mouse button to continue",Text%)
REPEAT
MOUSE X,Y,Z
UNTIL Z<>0
PROCclear_mouse
ENDPROC
:
DATA 6,5
DATA Information
DATA Start a New / Edit Current Barcode,Display Current Barcode
DATA Save Options,Quit
DATA 4
DATA Set Article Number,Set A and B Sets,View Current Barcode,Back to Main Menu
DATA 3
DATA EAN Format,ITF Format,Back to New Barcode Menu
DATA 7
DATA 0) .. AAAAAA,1) .. AABABB,2) .. AABBAB,3) .. AABBBA,4) .. ABAABB,More ->,Back to New Barcode Menu
DATA 7
DATA 5) .. ABBAAB,6) .. ABBBAA,7) .. ABABAB,8) .. ABABBA,9) .. ABBABA,<- Back up,Back to New Barcode Menu
DATA 4
DATA Save as BarCode format,Save as Drawfile,Save as Screenshot,Back to Main Menu
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
