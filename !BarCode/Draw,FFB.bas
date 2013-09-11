SYS "Hourglass_On"
ON ERROR SYS "Hourglass_Smash":REPORT:PRINT " : ";ERL:END
DIM Menus$(10,10)
DIM MenItems(10)
file%=OPENIN("<BarCode$Dir>.SpName")
save$=""
REPEAT
TEMP=BGET#file%
save$+=CHR$(TEMP)
UNTIL EOF#file%
CLOSE#file%
RESTORE
PROCinit
PROCdrawfile_start(save$)
PROCfonttable
ANumber$="012345678900"
Ln=12
Fmt=0
AandB=0
DIM Se$(10)
FOR X=0 TO 9
READ Se$(X)
NEXT
DIM Numbers$(10,3)
FOR X=0 TO 9
FOR Y=1 TO 3
READ Numbers$(X,Y)
NEXT
NEXT
Sopt=0
PROCload
SYS "Hourglass_Smash"
drawfile$=save$
PROCgroup_start
CASE Fmt OF
WHEN 0:F$="EAN"
WHEN 1:F$="ITF"
ENDCASE
PROCtext(2*inch,10.5*inch,"Article Number : "+ANumber$,24*point,24*point,2,black%,white%)
PROCtext(2.25*inch,10*inch,"Style  : "+Se$(AandB)+" CCCCCC",24*point,24*point,2,black%,white%)
PROCtext(3.2*inch,9.5*inch,"Format : "+F$,24*point,24*point,2,black%,white%)
PROCtext(2.2*inch,1*inch,"LEN Copyright R.J.Wareham",24*point,24*point,2,black%,white%)
PROCdraw_barcode
PROCgroup_end:PROCdrawfile_end
OSCLI("CLOSE")
END
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
DEF PROCinit
maxlev%=200
DIM start%(maxlev%),box%(maxlev%)
DIM l%(maxlev%),b%(maxlev%),r%(maxlev%),t%(maxlev%)
DIM buf% 256
big%=&7FFFFFFF
EF=0.552256944
i%=0:c%=0
inch=&B400
point=640
cm=18140
black%=0
white%=&FFFFFF00
none%=-1
ENDPROC
:
REM error trap
ON ERROR OFF
REPORT
IF ERR=1234 THEN PRINT ELSE PRINT;" at line ";ERL
IF c%<>0 THEN CLOSE#c%
IF i%<>0 THEN CLOSE#i%
ON
END
:
DEF PROCdrawfile_start(D$)
c%=OPENOUT(D$)
IF c%=0 THEN ERROR 1234,"Can't open output file"
drawfile$=D$
PROCputw(&77617244):PROCputw(201):PROCputw(0)
PROCputs12("MakeDraw")
lev%=-1
PROChead_here(TRUE)
ENDPROC
:
DEF PROCdrawfile_end
IF lev%>0 THEN PRINT"Warning: Draw file closed with object(s) unfinished     "
PROChead_now
CLOSE#c%:c%=0
REM OSCLI("SetType "+drawfile$+" AFF")
ENDPROC
:
DEF PROCpath_start(x%,y%,width%,lcol%,fcol%)
PROCputw(2)
PROChead_here(TRUE)
PROCputw(fcol%):REM fill
PROCputw(lcol%):REM colour
PROCputw(width%):REM width
PROCputw(0):REM path style
PROCpath_move(x%,y%)
ENDPROC
:
DEF PROCrectangle(x%,y%,xl%,yl%,lcol%,fcol%)
PROCpath_start(x%,(y%-yl%)/2,xl%,lcol%,fcol%)
PROCpath_move(x%,y%)
PROCpath_draw(x%+xl%,y%)
PROCpath_draw(x%+xl%,y%+yl%)
PROCpath_draw(x%,y%+yl%)
REM PROCpath_draw(x%,y%)
PROCpath_close
PROCpath_end
ENDPROC
:
DEF PROCpath_move(x%,y%)
PROCputw(2)
PROCputxy(x%,y%)
ENDPROC
:
DEF PROCpath_draw(x%,y%)
PROCputw(8)
PROCputxy(x%,y%)
ENDPROC
:
DEF PROCpath_bezier(xc1%,yc1%,xc2%,yc2%,x%,y%)
PROCputw(6)
PROCputxy(xc1%,yc1%)
PROCputxy(xc2%,yc2%)
PROCputxy(x%,y%)
ENDPROC
:
DEF PROCpath_close
PROCputw(5)
ENDPROC
:
DEF PROCpath_end
PROCputw(0)
PROChead_now
ENDPROC
:
DEF PROCellipse(x%,y%,w%,h%,width%,lcol%,fcol%)
PROCpath_start(x%,y%-h%,width%,lcol%,fcol%)
PROCpath_bezier(x%+w%*EF,y%-h%,x%+w%,y%-h%*EF,x%+w%,y%)
PROCpath_bezier(x%+w%,y%+h%*EF,x%+w%*EF,y%+h%,x%,y%+h%)
PROCpath_bezier(x%-w%*EF,y%+h%,x%-w%,y%+h%*EF,x%-w%,y%)
PROCpath_bezier(x%-w%,y%-h%*EF,x%-w%*EF,y%-h%,x%,y%-h%)
PROCpath_close
PROCpath_end
ENDPROC
:
DEF PROCgroup_start
REM groups cannot be nested in this version
PROCputw(6)
PROChead_here(TRUE)
PROCputs12("group")
ENDPROC
:
DEF PROCgroup_end
PROChead_now
ENDPROC
:
DEF PROCfonttable
LOCAL font%,font$
RESTORE <line>DhF
READ font%,font$
IF font$="END" THEN ENDPROC
PROCputw(0)
PROChead_here(FALSE)
REPEAT
 PROCputs(CHR$(font%)+font$)
 READ font%,font$
UNTIL font$="END"
PROCalign
PROChead_now
ENDPROC
:
REM font table data
DATA 1,"System.Fixed"
DATA 2,"Trinity.Bold"
DATA 0,"END"
:
DEF PROCsprite(x%,y%,w%,h%,F$,S$)
IF LEN(S$)>12 THEN PRINT"Bad sprite name ";S$:ENDPROC
LOCAL i%,N%,len%
i%=OPENIN(F$)
IF i%=0 THEN PRINT"Sprite file ";F$;" not found":ENDPROC
$buf%=S$+STRING$(12,CHR$0)
FOR N%=0 TO 11
 IF buf%?N%>64 AND buf%?N%<91 THEN buf%?N%+=32
NEXT
N%=FNword:IF N%=0 THEN PRINT"No sprites in file ";F$:ENDPROC
PTR#i%=FNword-4
REPEAT
 len%=FNword
 IF FNword=!buf% AND FNword=buf%!4 AND FNword=buf%!8 THEN N%=-1 ELSE N%-     =1
 IF N%>0 THEN PTR#i%=PTR#i%+len%-16
UNTIL N%<1
IF N%=0 THEN PRINT"Sprite ";S$;" not found":ENDPROC
PROCputw(5)
PROChead_here(FALSE)
PROCputxy(x%,y%)
PROCputxy(x%+w%+1,y%+h%+1)
PROCputw(len%)
PROCputw(!buf%):PROCputw(buf%!4):PROCputw(buf%!8)
FOR N%=17 TO len%:BPUT#c%,BGET#i%:NEXT
CLOSE#i%:i%=0
PROCalign
PROChead_now
ENDPROC
:
DEF PROCtext(x%,y%,text$,xsize%,ysize%,font%,col%,bcol%)
PROCputw(1)
PROChead_here(FALSE)
PROCputxy(x%,y%-ysize%*.5)
PROCputxy(x%+LEN(text$)*xsize%,y%+ysize%)
PROCputw(col%)
PROCputw(bcol%)
PROCputw(font%)
PROCputw(xsize%):PROCputw(ysize%)
PROCputxy(x%,y%)
PROCputs(text$):PROCalign
PROChead_now
ENDPROC
:
DEF FNcol(R%,G%,B%)
=(R%<<8)+(G%<<16)+(B%<<24)
:
DEF PROCputw(A%)
BPUT#c%,A% AND &FF
BPUT#c%,(A%>>>8) AND &FF
BPUT#c%,(A%>>>16) AND &FF
BPUT#c%,(A%>>>24) AND &FF
ENDPROC
:
DEF PROCputs12(A$)
LOCAL A%
A$=A$+STRING$(12," ")
FOR A%=1 TO 12:BPUT#c%,ASC(MID$(A$,A%,1)):NEXT
ENDPROC
:
DEF PROCputs(A$)
LOCAL A%
FOR A%=1 TO LEN(A$):BPUT#c%,ASC(MID$(A$,A%,1)):NEXT
BPUT#c%,0
ENDPROC
:
DEF PROChead_here(box%)
IF lev%=maxlev% THEN ERROR 1234,"Too many nested groups.  Edit program t     o increase limit."
lev%+=1
box%(lev%)=box%
start%(lev%)=PTR#c%
IF lev%>0 THEN PROCputw(0)
IF box% THEN
 PROCputw(0):PROCputw(0)
 PROCputw(0):PROCputw(0)
ENDIF
l%(lev%)=big%:b%(lev%)=big%
r%(lev%)=-big%:t%(lev%)=-big%
ENDPROC
:
DEF PROChead_now
LOCAL end%
IF lev%<0 THEN PRINT"Warning: attempt to end more objects than were star     ted":ENDPROC
end%=PTR#c%
PTR#c%=start%(lev%)
IF lev%>0 THEN PROCputw(end%-start%(lev%)+4)
IF box%(lev%) THEN
 PROCputw(l%(lev%)):PROCputw(b%(lev%))
 PROCputw(r%(lev%)+1):PROCputw(t%(lev%)+1)
ENDIF
IF lev%>0 THEN
 lev%-=1
 IF l%(lev%+1)<l%(lev%) THEN l%(lev%)=l%(lev%+1)
 IF b%(lev%+1)<b%(lev%) THEN b%(lev%)=b%(lev%+1)
 IF r%(lev%+1)>r%(lev%) THEN r%(lev%)=r%(lev%+1)
 IF t%(lev%+1)>t%(lev%) THEN t%(lev%)=t%(lev%+1)
ENDIF
PTR#c%=end%
ENDPROC
:
DEF PROCputxy(x%,y%)
PROCputw(x%):PROCputw(y%)
IF x%<l%(lev%) THEN l%(lev%)=x%
IF y%<b%(lev%) THEN b%(lev%)=y%
IF x%>r%(lev%) THEN r%(lev%)=x%
IF y%>t%(lev%) THEN t%(lev%)=y%
ENDPROC
:
DEF PROCalign
WHILE PTR#c% AND 3:BPUT#c%,0:ENDWHILE
ENDPROC
:
DEF FNword
=BGET#i% OR (&100*BGET#i%) OR (&10000*BGET#i%) OR ((BGET#i%)<<24)
:
DEF PROCdraw_barcode
LOCAL X,BCode$,Y,Z,Add,F$
BCode$="DLD"
FOR X=1 TO Ln/2
Add=VAL(MID$(ANumber$,X,1))
CASE MID$(Se$(AandB),X,1) OF
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
REM RECTANGLE FILL 202,162,840,588
REM RECTANGLE FILL 0,0,1280,1030
PROCgroup_start
FOR X=1 TO LEN(BCode$)
IF MID$(BCode$,X,1)="D" THEN PROCrectangle(10000+35000+((X-1)*3200),136000,1600,230000,black%,black%)
NEXT
PROCrectangle(10000+8000+2400+42*1600+2500,122000,42*1600,1000,white%,white%)
PROCrectangle(10000+8000+2400+42*1600+2500+147000,122000,42*1600,1000,white%,white%)
PROCtext(1.25*inch,3.3*inch-40000,LEFT$(ANumber$,6),65*point,65*point,1,black%,white%)
PROCtext(4.45*inch,3.3*inch-40000,RIGHT$(ANumber$,6),65*point,65*point,1,black%,white%)
PROCtext(0.4*inch,3.3*inch-40000,STR$(AandB),65*point,65*point,1,black%,white%)
PROCgroup_end
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
