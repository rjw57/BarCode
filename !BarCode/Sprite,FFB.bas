MODE 15
CLS
OFF
SYS "Hourglass_On"
ON ERROR SYS "Hourglass_Smash":REPORT:PRINT " : ";ERL:END
DIM Menus$(10,10)
DIM MenItems(10)
RESTORE
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
file%=OPENIN("<BarCode$Dir>.SpName")
save$=""
REPEAT
TEMP=BGET#file%
save$+=CHR$(TEMP)
UNTIL EOF#file%
CLOSE#file%
SYS "Hourglass_Smash"
PROCdraw
OSCLI ("ScreenSave "+save$)
*DESKTOP
END
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
DEF PROCdraw
LOCAL X,BCode$,Y,Z,Add,F$
SYS "Font_CacheAddr" TO version%,cacheused%,cachesize%
SYS "Font_FindFont",,"Trinity.Bold",24*16,24*16,0,0 TO Text%
SYS "Font_FindFont",,"System.Fixed",40*16,40*16,0,0 TO Text3%
SYS "Font_SetPalette",,1,2,3,&88888800,&FFFFFF00
SYS "Font_SetFontColours",0,1,2,3
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
SYS "Font_LoseFont",Text%
SYS "Font_LoseFont",Text3%
ENDPROC
:
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

