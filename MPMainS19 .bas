#include "fbgfx.bi"
using fb
'MacPuf Main Program
'DIM AS INTEGER Drucker = FREEFILE
'OPEN LPT "LPT:HP Deskjet D1400 series,,EMU=TTY" AS #Drucker
'PRINT #Drucker,"123456789012345678901234567890123456789012345678901234567890123456789012"

'PRINT "Beliebige Taste zum Beenden druecken..."
'Adaptation of the FORTRAN program published by C.J. Dickinson in 1976.
'Translation from FORTRAN into FreeBasic and modernization of the graphic output. 
'Erxercise and trial program in MPAppIIIc.bas.
'During this process, the code will be tested frequently using MPMainTest,
' this program.
Screen 20
Common shared as Integer NE(), NDUMP(),ITEM(),N(),K2,K4,NTAB(),KZAHL
Common shared as Single TDUMP(),TDLAY(), T(),V2,WALUE,SIMLT, C(),ZEIT1,ZEIT2
Common Shared as Single O2CON,C2CON,SAT,PC2S,PO2S'Mainly for GASES/GSINV
Common shared as Integer KL,KT,INI,NW1,NW2,JKL,NEOF,NW,NARTI, MT, INDEX,ITRIG1()
Dim as Single T(120),TJJ(8),C(70), TDUMP(160),TDLAY(40),E,X,Y,U,W,V,FTCO,FTCOC,Z,FY,FD
Dim as Single XVENT, BO2ADJ,PC2,PO2
Dim as Integer NO(23), NTAB (23),NE(8),NDUMP(20),I,MORAN,J,ITEM(20),KOUNTER,MAXKOUNT
Dim as Integer N(99),ITRIG1(73),LL3,LL1,LL2,LL4,LL5,NREPT
Common shared as Integer NFLAG, J3, ISPAR, NA, NB, NC, ND,JOKE
Common shared as String ITRIG(),DATUM,ZEIT
Dim as String ITRIG(73),ANSW
DIM AS INTEGER Drucker = FREEFILE
ScreenControl SET_WINDOW_POS,1,1
NA=18'=508 (>80 min) crashes without any information. Where? Why?
'NA=508
'INI,KT,KL = Input/Output nos.; NW1, NW2,JKL,ITRIG and NEOF used in
'interactive dialogue (S/R NXTWD). NW=1 (new), NW=0 (changed subject)
'NFLAG is spare (INIT.=1)for coding users's special instructions.
'NARTI (1=Nat., 0=Artific.) ventilation. K2 and K4 concern symptoms (S/R SYMPTOMS)
'INDEX is pointer in delay line, S/R DELAY

'Common shared TDLAY is used for greater accuracy, providing a delay in venous
'return.If S/R DELAY is not used, the respective common block can be removed.
'Common shared NDUMP and TDUMP are only used for store/baacktrack (S/R DUMP)

'Now comes a huge list of common variables
Common shared as Single FIO2,FIC2,CO,PD,FADM,BULLA,VLUNG,ELAST,VADM,AZ
Common shared as Single BZ,CZ,BARPR,TEMP,TRQ,TC2MT,TVOL,HB,PCV,VBLVL
Common shared as Single ADDC3,BC3AJ,DPG,PR,FITNS,SPACE1,COMAX,SHUNT,VC,PEEP
'variable SPACE changed to SPACE1, obviously a reserved word in FreeBasic
Common shared as Single VO2CT,PA, RPH, VPH, FVENT,BPH,BAGO, BAGC,AO2MT,AC2MT
Common shared as Single AO2PR,AC2PR,DPH,XLACT,BO2PR,BC2PR,TIDVL,RRATE,RO2CT,VC2MT
Common shared as Single DVENT, SVENT,PC2CT,PO2CT,TO2CT,TC2CT,BO2CT,BC2CT,TPH,RC3CT
Common shared as Single VC2CT,RO2MT,RC2MT,XRESP,AN2MT,BO2MT,BC2MT,CBF,PC,DSPAC
Common shared as Single REFLV,RO2PR,CONSO,RC2PR,PG,PJ,TND,RC2CT,QB,PW
Common shared as Single FT,CONOM,BUBBL,TC2RF,TC3MT,VC3MT,TC3CT,VC3CT,TLAMT,RLACT
Common shared as Single BC3CT,BO2AD,COADJ,EO2CT,TO2MT,TO2PR,TC2PR,VO2MT,AVENT,PL
Common shared as Single EC2CT,TN2MT,TN2PR,FEV,SN2PR,EN2CT,UN2MT,RN2MT,X109,X110
Common shared as Single TC3AJ,SN2MT,QA,RVADM,XDSPA,BAG,XMALE,HT,WT,AGE
'The EQUIVALENCE list C1=C(1) to C70=C(70) is omitted.

'Eqivql ITRIG(73)/NTAB(1) and FIO2/T(1)?

Data 0,1,1,1,0,1,18,0,1,0,69,33,51,35,60,41,72,74,1,1,0,0,1
For I=1 to 23
    Read NO(I)
Next
'Print NO(18)
'sleep
'GetKey
'LL1 = 1 is normal; 2 is death; 3 is arithm. error
Data 10,1,17
Read LL1,LL2,LL3
'"E" specifies a very small quantity to avoid zero divisions
Data 0.00000001
Read E
OPEN LPT "LPT:HP Deskjet D1400 series,,EMU=TTY" AS #Drucker
'Here was the data input to the COMMON BLOCK variables, now immediately following
'L170 (call to MINIT) 
'set up dampig and pH functions
Declare Function DAMP (X as single,Y as single,Z as single) as single
Declare Function PHFNC (X as single,Y as single) as single

Function DAMP (X as single,Y as single,Z as single) as single 
    Return (X*Z+Y)/(1.+Z)
End Function
Function PHFNC (X as single,Y as single) as single 
    Return 6.1+Log(X/(0.03*Y))*0.434294482
End Function
Declare Function IFUNC (Z as single) as single
Function IFUNC (Z as single) as single 
    Return Int(Z*0.5+1.1)
    End Function
 Declare Function FUNC1(HT as Single,AGE as Single, X as Single, Y as Single, Z as Single) as Single
    Declare Function FUNC2(HT as Single, X as Single, Y as Single) as Single
Function FUNC1 (HT as Single,AGE as Single, X as Single, Y as Single, Z as Single) as Single
    Return X*HT-Y*AGE-Z
End Function
Function FUNC2 (HT as Single, X as Single, Y as Single) as Single
    Return X+(HT*0.01)^3*Y
End Function
'******************************************************************************
Declare Sub SYMPT 
Declare Sub CLIN1
Declare Sub CLIN2
Declare Sub VART
Declare Sub TVAR
Declare Sub VALUE
Declare Sub V1(I as Integer)
Declare Sub MINIT(ByRef LL1 as Integer, ByRef LL3 as Integer, ByRef LL4 as Integer, ByRef NREPT as Integer)
'LL1 as Integer, LL3 as Integer, LL4 as Integer,
Declare Sub QUERY (ByREf LL3 as Integer)
Declare Sub NXTWD (ByRef LL3 as Integer, ByRef XXX as Single, ByRef NNN as Integer, IMIN as Integer, IMAX as Integer)
Declare Sub DUMP(ByRef LL1 as Integer)
Declare Sub KONST( ByRef NREPT as Integer)
Declare Sub BAGER (ByREF N as Integer, ByRef CA as Single, ByREf CB as Single, ByRef CC as Single)
Declare Sub GSINV(PH as Single)
Declare Sub GASES (PH as Single) 
Declare Sub DELAY
Declare Sub DEATH(ByRef LL1 as Integer,ByRef LL2 as Integer,ByRef LL5 as Integer)
Declare Sub BRETH 
Declare Sub UNITS(N as Integer)
Declare Sub DEADY (ByRef LL1 as Integer,ByRef LL5 as Integer, ByRef NREPT as Integer)
Declare Sub EXPLAINGRAPH
'-----------------------------------------------------------------------------
Sub EXPLAINGRAPH
    Cls
    Print
    Print
    Print "                       Explaining the graphic features"
    
    Print "  This calls the graphic program Acknowledge by Biopac to display a MacPuf session."
    Print "  If Acknowledge is not available, quit MacPuf and use your own data acquisition"
    Print "  program to retrieve the stored *.txt files of either the S or T type, see below."
    Print
    Print "  Each session is automatically stored as *.txt file in the folder MacPufSess"
    Print "  with a date and time stamp. Once the graphic program is visible on screen,"
    Print "  you may start the key logger script MACPUFSTART to choose a session file, see below."
    Print "  It is extremely important that the position of windows on the screen are not changed "
    Print "  because the key logger expects its targets on a defined position on the screen!!!"
    Print
    Print "  It therefore will work properly right now only at one (1920 x 1080) screen resolution!"
    Print "  If your screen resolution is different, labeling  the channels and scaling should be done"
    Print "  manually."
    Print
    Print "  Once you have chosen the session TEXT FILE AND IMPORTED it into Acknowledge, enlarge both" 
    Print
    Print "  1.) the Acknowledge window and 2.) the session window to full screen size."
    Print
    Print "                  ONLY THEN start the key logger MACPUFCONT:"
    Print
    Print "  The cursor movements produced by the key logger will now find the correct"
    Print "  positions and activate the desired actions. If this is not done properly,"
    Print "  the results are unpredictable! MacPufCont will run for about 1 minute, it inserts"
    Print "  the proper channel names, the time scale, scales the y axes etc. and saves you a lot of work."
    Print "  (The hidden channel 9 contains information on the time step MacPuf uses at any moment)"
    Print "  The data stored (S type files), starting with the first channel, are: arterial pO2, arterial SO2, arterial pCO2,"
    Print "  arterial pH, arterial HCO3-, ventilation rate, tidal volume, cardiac output, arterial lactate "
    Print "  and finally a time marker. By changing the run options, 8 different channels can be stored " 
    Print "  at your convenience (T type files). Then, of course, labelling and scaling must be done manually."
    Print 
    Print "  Once the logger has finished its job, you may use all Acknowledge features to explore"
    Print "  the graph. You also may save it as an Acknowledge file: give it a name of your choice and"
    Print "  save it. This allows you to retrieve the file later immediately as an Acknowledge file."
    Print 
    Print "      ****  Hit any key now to call the program Acknowledge from Biopac  ****"
    Print
    Print "          MacPuf continues to run when you exit the graphic program."
    Print
    sleep
    getkey
    Return
    end sub

'------------------------------------------------------------------------------
Sub VALUE
    Dim as Integer I
    'Dim as Single WALUE
   ' Print
    'Print "VALUE: "
    
    WALUE=0.0
    I=1
    'Print "Value 1: ";ITEM(I),I
    V1(I)
    WALUE=V2
    'Print "**VAL: ";WALUE, V2
    'Print V2
    'sleep
    'GetKey
    'If ITEM(I)=1 Or I>=20 then goto L1
    'I=I+1
    'V1(I)
    'WALUE=WALUE*(10.0^V2)
    'Print "**VAL: ";WALUE, V2
   'L1:
    'Print "from VALUE: "; WALUE,V2,I,ITEM(I)
    Return
End sub
'Declare Sub V1 (ITEM as Integer,I as Integer)
'-------------------------------------------------------------------------------
Sub V1 (I as Integer)
    Dim as Single DIGIT(13),S,P
    Dim as Integer NDOT, IZ
    'Print
    'Print "V1: "
   DIGIT(1)=0.0: DIGIT(2)=1.0:DIGIT(3)=2.0:DIGIT(4)=3.0:DIGIT(5)=4.0:DIGIT(6)=5.0
   DIGIT(7)=6.0:DIGIT(8)=7.0:DIGIT(9)=8.0:DIGIT(10)=9.0:DIGIT(11)=0.0:DIGIT(12)=0.0
   DIGIT(13)=-1.0
   S=1.0
   V2=0.0
   P=0.0
   NDOT=0
   'Print "V1: "
   'Print "I","ITEM","IZ","DIGIT(IZ)","V2"
   'Print ITEM(I),
   'sleep
   L100:
   If ITEM(I)=1 Or I>=20 then Goto L150
   If ITEM(I)=12 then goto L120
   If ITEM(I)=13 then goto L130
   If NDOT=1 then Goto L110
   IZ=ITEM(I)
   V2=(V2*10.0+DIGIT(IZ))
   'Print I,ITEM(I),IZ, DIGIT(IZ),V2
   'sleep
   'GetKey
   Goto L140
   L110:
   P=P+1.0
   IZ=ITEM(I)
   V2=(V2+(DIGIT(IZ)/(10.0^P)))
   'Print "I","ITEM","IZ","DIGIT(IZ)","V2","NDOT"
   'Print I,ITEM(I),IZ, DIGIT(IZ),V2,NDOT
   Goto L140
   L120:
   NDOT=NDOT+1
   Goto L140
   L130:
   IZ=ITEM(I)
   S=DIGIT(IZ)
   L140:
   I=I+1
   Goto L100
   L150:
   V2=V2*S
   Return
End Sub

'-------------------------------------------------------------------------------
Sub VART
'Print "VART: "
FIO2=T(1):FIC2=T(2):CO=T(3):PD=T(4):FADM=T(5):BULLA=T(6):VLUNG=T(7):ELAST=T(8):VADM=T(9):AZ=T(10)
BZ=T(11):CZ=T(12):BARPR=T(13):TEMP=T(14):TRQ=T(15):TC2MT=T(16):TVOL=T(17):HB=T(18):PCV=T(19):VBLVL=T(20)
ADDC3=T(21):BC3AJ=T(22):DPG=T(23):PR=T(24):FITNS=T(25):SPACE1=T(26):COMAX=T(27):SHUNT=T(28):VC=T(29):PEEP=T(30)
'variable SPACE changed to SPACE1, obviously a reserved word in FreeBasic
VO2CT=T(31):PA=T(32):RPH=T(33):VPH=T(34):FVENT=T(35):BPH=T(36):BAGO=T(37):BAGC=T(38):AO2MT=T(39):AC2MT=T(40)
AO2PR=T(41):AC2PR=T(42):DPH=T(43):XLACT=T(44):BO2PR=T(45):BC2PR=T(46):TIDVL=T(47):RRATE=T(48):RO2CT=T(49):VC2MT=T(50)
DVENT=T(51): SVENT=T(52):PC2CT=T(53):PO2CT=T(54):TO2CT=T(55):TC2CT=T(56):BO2CT=T(57):BC2CT=T(58):TPH=T(59):RC3CT=T(60)
VC2CT=T(61):RO2MT=T(62):RC2MT=T(63):XRESP=T(64):AN2MT=T(65):BO2MT=T(66):BC2MT=T(67):CBF=T(68):PC=T(69):DSPAC=T(70)
REFLV=T(71):RO2PR=T(72):CONSO=T(73):RC2PR=T(74):PG=T(75):PJ=T(76):TND=T(77):RC2CT=T(78):QB=T(79):PW=T(80)
FT=T(81):CONOM=T(82):BUBBL=T(83):TC2RF=T(84):TC3MT=T(85):VC3MT=T(86):TC3CT=T(87):VC3CT=T(88):TLAMT=T(89):RLACT=T(90)
BC3CT=T(91):BO2AD=T(92):COADJ=T(93):EO2CT=T(94):TO2MT=T(95):TO2PR=T(96):TC2PR=T(97):VO2MT=T(98):AVENT=T(99):PL=T(100)
EC2CT=T(101):TN2MT=T(102):TN2PR=T(103):FEV=T(104):SN2PR=T(105):EN2CT=T(106):UN2MT=T(107):RN2MT=T(108):X109=T(109):X110=T(110)
TC3AJ=T(111):SN2MT=T(112):QA=T(113):RVADM=T(114):XDSPA=T(115):BAG=T(116):XMALE=T(117):HT=T(118):WT=T(119):AGE=T(120)
Return
end sub
'--------------------------------------------------------------------------------
Sub TVAR
'Print "TVAR: "
T(1)=FIO2:T(2)=FIC2:T(3)=CO:T(4)=PD:T(5)=FADM:T(6)=BULLA:T(7)=VLUNG:T(8)=ELAST:T(9)=VADM:T(10)=AZ
T(11)=BZ:T(12)=CZ:T(13)=BARPR:T(14)=TEMP:T(15)=TRQ:T(16)=TC2MT:T(17)=TVOL:T(18)=HB:T(19)=PCV:T(20)=VBLVL
T(21)=ADDC3:T(22)=BC3AJ:T(23)=DPG:T(24)=PR:T(25)=FITNS:T(26)=SPACE1:T(27)=COMAX:T(28)=SHUNT:T(29)=VC:T(30)=PEEP
'variable SPACE changed to SPACE1, obviously a reserved word in FreeBasic
T(31)=VO2CT:T(32)=PA:T(33)=RPH:T(34)=VPH:T(35)=FVENT:T(36)=BPH:T(37)=BAGO:T(38)=BAGC:T(39)=AO2MT:T(40)=AC2MT
T(41)=AO2PR:T(42)=AC2PR:T(43)=DPH:T(44)=XLACT:T(45)=BO2PR:T(46)=BC2PR:T(47)=TIDVL:T(48)=RRATE:T(49)=RO2CT:T(50)=VC2MT
T(51)=DVENT:T(52)=SVENT:T(53)=PC2CT:T(54)=PO2CT:T(55)=TO2CT:T(56)=TC2CT:T(57)=BO2CT:T(58)=BC2CT:T(59)=TPH:T(60)=RC3CT
T(61)=VC2CT:T(62)=RO2MT:T(63)=RC2MT:T(64)=XRESP:T(65)=AN2MT:T(66)=BO2MT:T(67)=BC2MT:T(68)=CBF:T(69)=PC:T(70)=DSPAC
T(71)=REFLV:T(72)=RO2PR:T(73)=CONSO:T(74)=RC2PR:T(75)=PG:T(76)=PJ:T(77)=TND:T(78)=RC2CT:T(79)=QB:T(80)=PW
T(81)=FT:T(82)=CONOM:T(83)=BUBBL:T(84)=TC2RF:T(85)=TC3MT:T(86)=VC3MT:T(87)=TC3CT:T(88)=VC3CT:T(89)=TLAMT:T(90)=RLACT
T(91)=BC3CT:T(92)=BO2AD:T(93)=COADJ:T(94)=EO2CT:T(95)=TO2MT:T(96)=TO2PR:T(97)=TC2PR:T(98)=VO2MT:T(99)=AVENT:T(100)=PL
T(101)=EC2CT:T(102)=TN2MT:T(103)=TN2PR:T(104)=FEV:T(105)=SN2PR:T(106)=EN2CT:T(107)=UN2MT:T(108)=RN2MT:T(109)=X109:T(110)=X110
T(111)=TC3AJ:T(112)=SN2MT:T(113)=QA:T(114)=RVADM:T(115)=XDSPA:T(116)=BAG:T(117)=XMALE:T(118)=HT:T(119)=WT:T(120)=AGE
Return
end sub
'-------------------------------------------------------------------------------
Sub DEADY (ByRef LL1 as Integer,ByRef LL5 as Integer, ByRef NREPT as Integer)

    'Print "DEADY: "
    'Subroutine DEADY (LL1,LL2,LL5,NREPT,SIMLT)
'This S/R prints out values at the end of a run, and asks for, 
'interprts and acts on various change options, using S/R NXTWD
'to input keyboard instructions. Then returns to main program.
'DIMENSION T(120),IANS (10),ICHAR(5)
      'COMMON KT,KL,INI,NW1,NW2,JKL,ITRIG(73),NEOF,NW
      'COMMON NFLAG,J3,ISPAR,NA,NB,NC,ND,NE(8)
      'COMMON NARTI,MT,K2,K4,INDEX
'C        COMMON T(120)
'C       COMMON NARTI,MT,INDEX
 '       COMMON FIO2,FIC2,CO,PD,FADM,BULLA,VLUNG,ELAST,VADM,AZ,
 '    x BZ,CZ,BARPR,TEMP,TRQ,TC2MT,TVOL,HB,PCV,VBLVL,
 '    x ADDC3,BC3AJ,DPG,PR,FITNS,SPACE1,COMAX,SHUNT,VC,PEEP,
'    x AO2PR,AC2PR,DPH,XLACT,BO2PR,BC2PR,TIDVL,RRATE,RO2CT,VC2MT,
 '    x DVENT, SVENT,PC2CT,PO2CT,TO2CT,TC2CT,BO2CT,BC2CT,TPH,RC3CT,
'     x VC2CT,RO2MT,RC2MT,XRESP,AN2MT,BO2MT,BC2MT,CBF,PC,DSPAC,
 '    x REFLV,RO2PR,CONSO,RC2PR,PG,PJ,TND,RC2CT,QB,PW,
 '    x FT,CONOM,BUBBL,TC2RF,TC3MT,VC3MT,TC3CT,VC3CT,TLAMT,RLACT,
 '    x BC3CT,BO2AD,COADJ,EO2CT,TO2MT,TO2PR,TC2PR,VO2MT,AVENT,PL,
 '    x EC2CT,TN2MT,TN2PR,FEV,SN2PR,EN2CT,UN2MT,RN2MT,X109,X110,
 '    x TC3AJ,SN2MT,QA,RVADM,XDSPA,BAG,XMALE,HT,WT,AGE
  '    EQUIVALENCE (FIO2,T(1))
DIM AS INTEGER Drucker = FREEFILE
Dim as String ICHAR(5)
Dim as Integer KYY,I,J,K,IJ,NNN,NL,IANS(10)
Dim as Single X,Y,RR,XX,XXX,PPH,XT
ICHAR(1)="1":ICHAR(2)="/":ICHAR(3)="5":ICHAR(4)="/":ICHAR(5)=" "
If LL1-2=0 then goto L380
If LL1-2<0 then goto L360
L300:
Print " .1.Backtrack, 2.Continue, 3. Restart, 4. Inspect, 5. Stop"
NW2=0
'LL1=7
NXTWD(7,XX,NNN,1,6)
Do
    If NNN=1 then goto L330
    If NNN=2 then goto L600
    If NNN=3 then goto L600
    If NNN=4 then goto L500
    If NNN=5 then goto L1120
    If NNN=6 then goto L610
Loop
L330:
NW=0
Goto L920
L360:
'PRINT ITRIG(73)
'Sleep
'GetKey
ITRIG1(73)=ITRIG1(73)+1
If NC-2>=0 then goto L1110
If NB=1 then goto L470
If MT<1 then goto L1110
L380: X=10.*2.^(Abs(8.-RPH)*3.33)
'Dim as Single RO2PR,RO2CT,PJ,RPH,X,RC3CT,RC2PR,RC2CT
'Dim as Single RRATE,TIDVL,DVENT, COADJ,DSPAC,PW
'Dim as Single T(6)
'RO2PR=101.3:RO2CT=19.4:PJ=98.6:RPH=7.43:X=41.2:RC3CT=23.2:RC2PR=39.6:RC2CT=20.1
'RRATE=16.3:TIDVL=563.:DVENT=8.6: COADJ=5.3:DSPAC=373:PW=2.3
'T(1)=20.9:T(2)=0.03:T(3)=5.3:T(4)=99.5:T(5)=1.3:T(6)=230.4
'VART
Print
Print
Print " Final values for this run were ..."
'Print using " Arterial pO2  =####.#;  O2 Cont =####.#; O2 Sat =###.#%";RO2PR;RO2CT;PJ
Print using " Arterial pO2  =####.#;  O2 Cont =####.#; O2 Sat =###.#%";T(72);RO2CT;PJ
'Print using " Arterial pCO2 =####.#; CO2 Cont =####.#";RC2PR;RC2CT
Print using " Arterial pCO2 =####.#; CO2 Cont =####.#";T(74);RC2CT
Print using " Arterial pH =###.## (###.# nM); Arterial bicarbonate###.# mMole";RPH;X;RC3CT
Print
Print using " Respiratory rate =###.# 1/min; Tidal vol.=###### ml";RRATE;TIDVL
Print using " Total ventilation=####.# L/min; actual cardiac output=####.# L/min";DVENT; COADJ
Print using " Total dead space##### cc; Actual venous admixture####.# percent";DSPAC;PW
Print
'TVAR
'UNITS(2)
If LL1=2 then goto L300
'ITRIG(73) controls print out of the first 6 factors at the end of a run
If ITRIG1(73)>2 then goto L1110
Print using " 1.Insp. O2=####.#, 2.CO2=####.# percent, 3.Nom.card.outp.=####.# percent";T(1);T(2);T(3)
Print using " 4.Tiss.metab.=####.# %, 5.Venous admixt=####.# percent, 6.D.space+=#####.# ml";T(4);T(5);T(6)
If ITRIG1(73)=2 then Print "The factor list will now disappear - cf handbook"
'sleep
'getkey
Goto L1110
L430:
NW=0
CLIN1
Print "DEADY after CLIN1: NB,FT,NW2,NA,NEOF,ISPAR: ";NB,FT,NW2,NA,NEOF,ISPAR
sleep
GetKey
If NB=2 then goto L1200
Goto L470:
'If not new subject (NW=0) insert '1/5' in input buffer to renew
'the subject from initial values, then return to CLIN1
L440: If NW>0 then goto L430
NB=1
KYY=69-NW1
For I=1 to KYY
    ITRIG(73-I)=ITRIG(69-I)
Next I
For I=1 to 4
    ITRIG(NW1+I-1)=ICHAR(I)
Next I
If NW2<=19 then ITRIG(NW1+3)=ICHAR(5)
NW2=20
Goto L600
L470:
If NW2>19 then goto L490
Print
Print " Do you want to  1.Change, 2.Continue, 3. Restart, 4. Inspect, 5.Stop 6. See all"
NW2=0
L490:
'LL3=15
NXTWD(15,XXX,NNN,1,6)
JKL=0
Do
    If NNN=1 then goto L640
    If NNN=2 then goto L630
    If NNN=3 then goto L600
    If NNN=4 then goto L500
    If NNN=5 then goto L1120
    If NNN=6 then goto L610
Loop
L500:
X=T(22)+T(91)
Y=T(16)*1000.
Print "              Part.Pressures   Contents cc%    Amounts in cc       pH     HCO3-"
Print "               O2     CO2       O2      CO2      O2     CO2"
Print
Print using " Arterial  ######.# ######.# #####.# ######.#   ####.# ####.#   ####.## #####.#";T(72);T(74);T(49);T(78);T(62);T(63);T(33);T(60)
Print using " Alv./Lung ######.# ######.# (Sat=###.#%)       ####.# ####.#";T(41);T(42);T(76);T(39);T(40)
Print using " (Pulm.Cap) #####.# ######.# #####.# ######.#";T(41);T(42);T(54);T(53)
Print using " Brain/CSF ######.# ######.# #####.# ######.#   ####.# ####.#   ####.## #####.#";T(45);T(46);T(57);T(58);T(66);T(67);T(36);X
Print using " Tissue/ECF #####.#  (####.#) ####.# ######.#   ####.# ######   ####.##";T(96);T(97);T(55);T(56);T(95);Y;T(59)
Print using " Mixed ven. (####.#)######.#  ####.# ######.#   ####.# ####.#   ####.## #####.#";T(96);T(97);T(31);T(61);T(98);T(50);T(34);T(88)
If PL>=1. then BAGER (7,X,Y,PPH)
Print using " Plasma lactate conc.=####.1 mMole/L";RLACT
If LL1=2 Or LL5<0 then goto L300
RR=DSPAC/(TIDVL+0.000001)
X=QA/FT
Y=QB/FT
Print
Print using " O2 uptake = #####.#  CO2 output = #####.# cc/min (STPD) Expired RQ = ####.#";X;Y;PC
Print using " Tot.vent.=####.#   Alv.vent(BTPS)=####.# R.rate=###.#  Ven.admx=###.#";DVENT ;FVENT;RRATE;PW
If DVENT<0.2 then goto L560
Print using " Dead space(BTPS)=####.#  Tidal vol.=####.# D.Sp/TIDVL ratio=####.#";DSPAC;TIDVL;RR
L560: If COADJ <=0.1 then goto L575
Print using " Cardiac output=####.#   Cerebral blood flow=####.# ml/100g/min";COADJ;CBF
L575:
If BUBBL<=0 then goto L585
Print using " Nitrogen supersaturation index=#####.#";BUBBL
L585:
If PL<0. then BAGER (6,X,Y,PPH)
Print
If LL1-2>=0 then goto L300
goto L470
L600:
MT=0
LL1=3
'Print "LL1: ";LL1
Goto L1200
'This outputs the whole T array?
L610:
K=1
Print
For J=1 to 24
For I=1 to 5 '120?
    Print using "T(###)->####.##   ";K;T(K);
    K=K+1
Next I
Print
Next J
If LL1-2>=0 then goto L300
Goto L470
L630:
If NB=1 then NB=0
NREPT=NA
'Print "L630: LL1: ";LL1
Goto L1200
L640:
If NW2>19 goto L660
Print " 1. Change values, 2.Nat/Art. vent., 3.Store/Bktrk, 4.Run change 5.Presets"
NW2=0
L660:
'LL3=3
NXTWD(3,XXX,NNN,1,5)
If NNN<>5 then NW=0
Do
    If NNN=1 then goto L930
    If NNN=2 then goto L820
    If NNN=3 then goto L920
    If NNN=4 then goto L670
    If NNN=5 then goto L440
Loop
L670: If NW2>19 then goto L690
Print " Type no. of seconds for run (1800 max.)"
L690:
'LL3=7
NXTWD(7,XT,NNN,2,1800)
If NW2 >19 then goto L710
Print " Type interval between computations in secs (10 max.)"
L710:NXTWD(12,XXX,NNN,1,10)
FT=XXX/60.+0.000001
NA=Int(XT/XXX)
If NW2>19 then goto L730
Print " Do you want 1. All, 2.every 6th, 3.every 30th value printed"
L730:NXTWD(11,XXX,ISPAR,1,3)
If NW2>19 then goto L750
'This use of NXTWD allows a string of 1 to 8 numbers to be fed into
'array NE
Print " Do you want 1.Graphs + text, 2.Graphs only, 3.Selected values"
L750:
'LL3=5
NXTWD(5,XXX,NC,1,4)
MT=1
DO
    If NC=1 then goto L470
    If NC=2 then goto L810
    If NC=3 then goto L760
    If NC=4 then goto L470
Loop
L760:
If NW2>19 then goto L780
Print " Type up to 8 nos. - 69 is standard"
NW=2
L780:
NEOF=9
NL=0
JKL=1
L790:
'LL3=6
NXTWD(6,XXX,NNN,1,120)
NW2=NW2+1
NL=NL+1
If JKL=0 then goto L800
If NEOF=0 then goto L470
NE(NL)=NNN
'Print "NL,NE(NL): ";NL,NE(NL)
'sleep
'getkey
Goto L790
L800:
NE(NL)=NNN
'Print "NL,NE(NL): ";NL,NE(NL)
'sleep
'getkey
L810:
MT=0
Goto L470
L820:
If NW2>19 then goto L840
Print " 1. Give artificial, 2.Return to natural ventilation"
L840:
'LL3=7
NXTWD(7,XXX,NARTI,1,2)
NARTI=NARTI-1
If NARTI <=0 then goto L850
PEEP=0.
'For printer
Print "**** Natural ventilation ****"
'Print #Drucker, "**** Natural ventilation"
Goto L470
L850:
If NW2>19 then goto L870
Print "  Now ventilation in cycles/min"
L870:
'LL3=7
NXTWD(7,RRATE,NNN,0,200)
If RRATE <= 0.001 then RRATE=0.001
If NW2>19 then goto L890
Print "  Give tidal volume in ml"
L890:
'LL3=8
NXTWD(8,TIDVL,NNN,0,5000)
'Are RRATE and TIDVL input above mixed up?
If TIDVL<=0.001 then TIDVL=0.001
NW=0
If NW2>19 then goto L910
Print " Positive end exspiratory pressure - type either '0' or"
Print " number of cm of water (up to 15)"
L910:
'LL3=16
NXTWD(16,PEEP,NNN,0,15)
Print "*** ";
Print using "Artif. ventil. at###.#/min";RRATE;
Print ",";
Print using "##### ml tidal vol. ";TIDVL;
Print using " and#### cm PEEP";PEEP;
Print " ***"
Goto L470
L920:
'Print " DEADY: before DUMP: LL1: ";LL1
'sleep
'getkey
DUMP (LL1)
NB=0
Goto L470
L930:
If NW2>19 then goto L950
Print " Type number of factors (1-30) to change, or 100 for bag expts., etc."
NW2=0
L950:
NEOF=10
NL=0
KYY=-1
JKL=1
L960:
'LL3=9
NXTWD(9,XXX,NNN,1,120)
If NNN=100 then goto L1100
KYY=KYY+1
NW2=NW2+1
NL=NL+1
If JKL=0 then goto L980
If NEOF=0 then goto L990
IANS(NL)=NNN
If NNN<=30 then goto L975
Print using " Pointless -- factor ### will be changed back again during the next run";NNN 
L975:
Goto L960
L980:
IANS(NL)=NNN
KYY=KYY+1
L990:
If NW2>19 then goto L1000
NW2=0
L1000:
For IJ=1 to KYY
I=IANS(IJ)
If NW2>19 then goto L1020
Print using " Factor ### (current value=#####.#), specify new value";I;T(I)
L1020:
'LL3=10
NXTWD(10,XXX,NNN,-500,16000)
Print using " Factor ### = #####.#, (previously = #####.#)";I;XXX;T(I)
T(I)=XXX
If T(I)=0. then T(I)=0.000000001
Next IJ
VART
'Print "FIO2: ";FIO2
'sleep
'GetKey
L1050:
NW=0
REFLV=VLUNG
Goto L470
L1100:
BAGER (1,X,Y,PPH)
Goto L1050
'Check for various symptoms
L1110:
If NB=2 then DUMP (LL1)
SYMPT
Goto L470
L1120:
Print " Thank you for your interest. Please let your tutor have"
Print "any comments, suggestions or criticism. Good Bye."
end
L1200:
Return
end sub
'------------------------------------------------------------------------------
Sub CLIN1
    'Subroutine CLIN1(SIMLT)
'This subroutine produces preset patients
Dim as Integer NNN
Dim as Single XXX
'Usual COMMON block
If NW2>19 then goto L110
Print " Do you want 1. Preset patients or subjects, or"
Print "             2. Specify your own patients or subjects?"
NW2=0
L110:
NA=18
FT=0.16667
NARTI=1
ISPAR=1
NEOF=1
NXTWD(7,XXX,NNN,1,2)
'NEOF is re-calculated in NXTWD, return is -1
If NNN=1 then goto L120
If NNN=2 then goto L280
L120: If NW2>19 then goto L140
Print " The following preset patients are available "
Print " 1. Normal fit subject exercising at 300 kpm/min"
Print " 2. Same, at 900 kpm/min"
Print " 3. Unfit normal person erxercising at 900 kpm/min"
Print " 4. Normal subject, compressed to 10 atmospheres for 10 min"
Print " 5. Chronic airways obstruction with ventilatory failure"
Print " 6. Same, but with acute exacerbation, e.g. added bronchopneumonia"
Print " 7. Cheyne-Stoke breathing due to brain stemm damage and heart disease"
Print " ...Type number"
FT=0.16667
NW2=0
L140: NXTWD(7,XXX,NNN,1,7)
Do
If NNN=1 then goto L150
If NNN=2 then goto L160
If NNN=3 then goto L170
If NNN=4 then goto L190
If NNN=5 then goto L200
If NNN=6 then goto L240
If NNN=7 then goto L250
Loop
'Preset 1 - normal subject excising at 300 kpm/min
L150: PD=400.
TRQ=0.888
Goto L180
'Preset 2 - normal subject excising at 900 kpm/min
L160:
PD=800.
TRQ=0.98
Goto L180
'Preset 3 - unfit normal subject exercising at 900 kpm/min
L170: FITNS=35.5
Goto L160
L180:
NA=36
Goto L255
'Preset 4 - Normal subject compressed for 25 min to 10 atmospheres
L190:
BARPR=7600.*SIMLT
NA=30
SN2MT=2750.
J3=20
Goto L230
'Preset 5 - Chronic airway obstruction with ventilatory failure
L200:
VADM=28.
VC=1.2
VLUNG=5000.
L210:
PR=30.
TC3MT=480.
BC3AJ=4.
TC2MT=15.7
VC2MT=2600.
BC2MT=1000.
ELAST=34.
L220:NA=72
L230: ISPAR=2
Goto L260
'Preset 6 - Chronic airways obstruction with exacerbation
L240:
VC=1.
BULLA=30.
VADM=60.
XDSPA=30.
AZ=50.
VLUNG=7000.
Goto L210
'Preset 7 - Cheyne Stoke breathing due to brain stem damage and heart disease
L250:
CZ=0.
AZ=160.
BZ=50.
CO=58.
VADM=30.
XRESP=0.
AC2MT=90.
L255: FT=0.033334
L260: NB=2
If NW2>19 then TVAR:Return
Print " Time scale is altered for this simulation"
Print " Wait until I work this out!"
TVAR
Return
'CLIN2 is the routine for creating individual patients and inserting 
'pulmonary function tests
L280:
CLIN2
Goto L220
End Sub
'------------------------------------------------------------------------------
'Subroutine CLIN2
Sub CLIN2
    'This subroutine is used only for creating subjets to order,
    'and for allowing as far as possible for function test results.
    'Usual COMMON tables 
    'ARRAY stores normal function tests results for functions used later
    '(FUNC1 and FUNC2)
    'EQUIVALENCE (FIO2,T(1))
    Dim as Single PFEV,XXX,GUESS,TEST,PVC,PDCO,RAT,RATIO,X,TAR,ARRAY(3,10)
    Dim as Integer NNN, NN, K,J
     Dim ARRAY1 (1 to 10, 1 to 3) as Single => {{0.026,0.009,2.18},{0.047,0.0075,4.583},_
   {0.0452,0.024,2.852},{0.0582,0.025,4.241},{0.035,0.025,1.932},{0.0363,0.032,1.26},_
   {0.382,0.732,100.0},{-0.125,0.951,100.0},{0.221,0.683,100.0},{-0.011,0.782,100.0}}
   For K=1 to 10
       For J=1 to 3
           ARRAY(J,K)=ARRAY1(K,J)
   'Print ARRAY(J,K),
Next J
 'Print
Next K
  ' Sleep
   'Getkey
   
    'Data 0.026,0.009,2.18,0.047,0.0075,4.583,_
    '0.0452,0.024,2.852,0.0582,0.025,4.241,0.035,0.025,1.932,_
    '0.0363,0.032,1.26,0.382,0.732,100.0,-0.125,0.951,100.0,_
    '0.221,0.683,100.0,-0.011,0.782,100.0
'For K=1 to 10
 '   For J=1 to 3
  '      Read ARRAY(J,K)
  '  Next J
'Next K

    'Declare Function FUNC1(HT,AGE, X as Single, Y as Single, Z as Single) as Single
    'Declare Function FUNC2(HT,X as Single, Y as Single) as Single
'Function FUNC1 (HT,AGE, X as Single, Y as Single, Z as Single) as Single
   ' Return X*HT-Y*AGE-Z
'End Function
'Function FUNC2 (HT, X as Single, Y as Single) as Single
    'Return X+(HT*0.01)^3*Y
'End Function
'Initialize predicted FEV, VC and DCO in case next section skipped
PFEV=FEV
PVC=VC
PDCO=20.0
if NW2>19 then goto L110
Print " 70 kg average man. Do yo want to specify someone else?"
Print " 1. Yes, 2. No"
NW2=0
L110:
NXTWD(7, XXX, NNN, 1, 2)
Do
    If NNN=1 then goto L120
    If NNN=2 then goto L280
Loop
'Build up the specified subject
L120:
IF NW2>19 then goto L140
Print "Please specify: 1.Male, 2.Female"
NW2=0
L140: NXTWD(2, XXX, NNN, 1,2)
If NNN=2 then XMALE=0.
IF NW2>19 then goto L160:
Print "Give me the height in cm (183 cm = 6 ft)"
NW2=0
L160:
NXTWD(7,HT,NNN,95,200)
If NW2>19 then goto L180
Print "Now weight in kg"
NW2=0
L180:NXTWD(7,WT,NN,20,200)
RAT=WT*5880.0/HT^1.6
If XMALE<0.5 then RAT=RAT*1.064
IF NW2>19 then goto L200
Print " and age in years."
NW2=0
L200: NXTWD (7,AGE,NNN,8,100)
K=Int(XMALE)+1
VLUNG=FUNC1(HT,AGE,ARRAY(1,K),ARRAY(2,K),ARRAY(3,K))
K=K+2
X=Abs(20.0-AGE)
PDCO=(7.6*VLUNG+5.0)*(100.0-X)*0.01
VLUNG=VLUNG*1000.0
If AGE<17. then goto L210
PVC=FUNC1(HT,AGE,ARRAY(1,K),ARRAY(2,K),ARRAY(3,K))
K=K+2
PFEV=FUNC1(HT,AGE,ARRAY(1,K),ARRAY(2,K),ARRAY(3,K))
Goto L220
L210:
K=K+4
PVC=FUNC2(HT,ARRAY(1,K),ARRAY(2,K))
K=K+2
PFEV=FUNC2(HT,ARRAY(1,K),ARRAY(2,K))
L220:
CONSO=WT^0.75*10.33
X=PVC-PFEV-0.1
If X<0.0 then PVC=PFEV+0.1
CONOM=CONSO*0.0195
If XMALE>0.5 then goto L230
CONOM=CONOM*0.9
HB=13.5
PCV=0.41
L230:
VBLVL=CONOM*300.+1500.
TVOL=(WT^0.6*0.465)+6.0
X=TVOL*0.084
TC3MT=TC3MT*X^1.35
TC2MT=TC2MT*X
SN2MT=SN2MT*X
Print       "              Litres       ml/mmHg/min    ml/min     litres/min      %Ideal"
Print       "Predicted   FEV1    VC     Diff. capac.  O2 consm.    Card.outp.      weight"
Print using "           ###.#   ###.#    ###.#          ###.#       ###.#          ###.#";PFEV;_
PVC;PDCO;CONSO;CONOM;RAT
COMAX=(210.0-0.65*AGE)*0.0008*HT
If NW2>19 then goto L260
NW2=0
Print " Do you want to enter spirometer results, 1.Yes, 2.No"
L260:
NXTWD(7,XXX,NNN,1,2)
Do
    if NNN=1 then goto L280
    if NNN=2 then goto L270
Loop
L270:
VC=PVC
FEV=PFEV
Goto L400
'Incorporate spirometer results
L280:
If NW2>19 then goto L310
L290: Print " Give me the value for FEV1 in litres"
NW2=0
L310:
NXTWD(7,FEV,NNN,0,6)
If FEV>0.13 then goto L330
Print " Very unlikely - recheck and":goto L290
L330:
If NW2>19 then goto L360
L340: Print " Now the value for VC"
NW2=0
L360:
NXTWD(7,VC,NNN,0,7)
If VC>=0.2 then goto L380
Print " Probably a technical error - please repeat"
Goto L340
L380:
If VC>FEV then goto L400
Print " It can never be the same, or less than the FEV1"
Print " Let's have both again to be sure"
Goto L290
L400:
RATIO=FEV/VC
X=RATIO*100.0
Print using " The FEV1/VC ratio is ##.# , i.e. #### percent";RATIO;X
RAT=PFEV/FEV
TAR=1.0/RAT
XDSPA=20.0*RAT^0.7-20.0
If RATIO >=0.65 then goto L460
Print " These values indicate airways obstruction."
Print
IF NW2>19 then goto L440
Print " Is your patient"
Print " 1.acutely breathless, 2.not much changed in last week or so "
NW2=0
L440:
NXTWD(7,XXX,NNN,1,2)
'Set values for obstructed subject
VLUNG=VLUNG*(0.5+0.43/RATIO)
If NNN-1<>0 then goto L450
'Acute obstruction only
L445:ELAST=FEV+60.0/(FEV+1.0)-11.0
VADM=2.0*RAT^1.7
If VADM>85.0 then VADM=85.0
CZ=30.*RAT+70.0
If CZ>140.0 then CZ=140.0
If FEV<1.0 then CZ=140.0*FEV
Goto L580
'Add in changes appropriate for chronic obstructed subjects
L450:
BC3AJ=0.35*RAT-0.35
ELAST=FEV+96.0/(FEV+4.0)-11.0
X=1.2*RAT-1.2
If X<=0.0 then x=0.0
X=X^0.52
If (PFEV-FEV-2.5)<0. then goto L454
PR=(FEV+3.0)*21.0
L454:
TC3MT=TC3MT+TVOL*3.0*X
CZ=10.0*RAT+90.0
XDSPA=XDSPA*1.2
VADM=2.0*RAT^1.1
TC2MT=TC2MT+TVOL*0.045*X
Goto L470
'Unobstructed subject
L460:
VADM=VADM*RAT^1.6
If FEV<1.0 then VADM=7.0*RAT
ELAST=FEV+72.0/(FEV+2.0)-11.0
CZ=20.0*RAT+80.0
L470:
If NW2>19 then goto L490
Print " Have you measured diffusing capacity, 1. Yes, 2.No"
NW2=0
L490: NXTWD(7,XXX,NNN,1,2)
' *** Incorporate diffusing capacity measurement
'Remaining statements adjust effective venous admixture according to
'diffusion capacity estimates or measurements
GUESS=PDCO*(1.0+TAR)*0.5
If NNN=1 then goto L510
Print using " In that case I shall assume that it is ###.# ml/mmHg/min.";GUESS
Print " This would be an average sort of figure for your subject."
Goto L580
L510:
If NW2>19 then goto L530
Print " Good. Please give me your value in ml/mmHg/min"
NW2=0
L530: NXTWD(7,XXX,NNN,3,100)
X=PDCO+7.0
If XXX<X then goto L550
Print " This seems like nonsense, unless your value was obtained during exercise."
Print using " I am going to assume it was ###.#  ml/mmHg/min";GUESS
XXX=GUESS
L550:
TEST=XXX/GUESS
If TEST<1.4 and TEST>0.6 then goto L570
Print " Unusual .... but you are the boss!"
'Make change with with different diff. capacity really different
L570:
RVADM=GUESS/XXX-1.0
XDSPA=XDSPA*2.0/(1.0+TEST)
L580:
BO2MT=30.0
TO2MT=700.0
If ELAST>75.0 then ELAST=75.0
TVAR
Return
End Sub

'------------------------------------------------------------------------------
Sub SYMPT
'Subroutine SYMPT (SIMLT)
'This subroutine specifies various symptoms under appropriate conditions.
'COMMON block as usual
'K1 - K4 concerned with improvemet of symptoms (logic is complex)
Dim as Single X
Dim as Integer K1,K3,K5
'X=1./SIMLT
X=1
L100:K1=0
K3=0
If TN2PR*X-6000. >0. then goto L130
If 12.5-BO2PR*X<=0. then goto L150
L130:
Print " Your patient is unrousable"
K1=1
Goto L428
L150:
If BO2PR*X-13.9 >0. then goto L180
Print " My eyes are going dim"
K1=1
L180:
If BUBBL-160.<=0. then goto L210
Print:Print "    I have terrible pains in my leg"
K1=1
Goto L240
L210:
If BUBBL-101.<=0. then goto L240
Print "      My skin is itching"
K3=1
L240:
If TN2PR*X-4300.>0. then goto L260
If BC2PR*X-80.<=0. then goto L280
L260:Print "    I am feeling drowsy"'; TN2PR,X,SIMLT
K3=1
L280:
If NARTI<=0 then goto L390
L290:
If RRATE-46.<=0. then goto L330
Print "This ... is .... impossible .."
L320:K1=1
goto L390
L330:
If RRATE-35.<=0. then goto L360
Print "   I am very short of breath"
goto L320:
L360:
If RRATE-27.<=0. then goto L390
Print "    I am rather short of breath"
K3=1
L390:
If TPH-7.59 <=0. then goto L420
Print "    I am getting tinglings and cramps in my hands"
K3=1
L420: If TPH-7.13 >0. then goto L428
Print "    I don't feel well at all"
K3=1
L428:If TPH-7.08>=0. then goto L450
Print:Print "Your patient is twitching"
K1=1
L450:
If NARTI>0 then goto L490
X=SVENT*0.9-DVENT
If X<=0. then goto L490
Print:Print "  Your patient is fighting the ventilator"
K3=1
L490:
X=1.3*HB-RO2CT
If X-7.<0 then goto L520
Print:Print " Your patient is very blue"
K1=1
Goto L550
L520:If X-5.0<0. then goto L550
Print:Print "Your patient is blue"
K3=1
L550:
If K1-K3<=0 then goto L570
K3=1
L570:K5=K1+K3
If K2-K5<=0 then goto L600
Print:Print "God bless you, doctor, I feel really well again."
Print "It's like a miracle."
Goto L660
L600:
If K2-K1 <=0 then goto L630
Print:Print "I feel better but not right now."
Print "Can't you do something else for me?"
Goto L660
L630:
If K4-K3<=0 then goto L660
Print:Print " That is better, doctor, ....."
Print " but are you going to do any other nasty things to me?"
Print
L660:
K2=K1
K4=K3
If NB <> 2 then goto L670
'Alter no. of iterations after S/R CLIN1 and CLIN2 subjects in steady state.
NA=30
If FT<0.1 then NA=18
L670:
Return
End Sub
'----------------------------------------------------------------------------
Sub UNITS(N as Integer)
'Print "UNITS: "
'Subroutine UNITS (N, SIMLT)
'Transform pressure variables to SI units if necessary
'SIMLT has value of 1 for mmHg, 0.1332 for SI (KPa)
'COMMON NTAB(101),T(105)
Dim as Integer NSI(10),I,J
Dim as Single X
'NSI contains factor numbers of all pressure variables for conversion
'before output display
NSI(1)=41:NSI(2)=42:NSI(3)=45:NSI(4)=46:NSI(5)=72:NSI(6)=74:NSI(7)=96:NSI(8)=97:NSI(9)=103:NSI(10)=105
X=SIMLT
'print "SIMLT: ";SIMLT:sleep:getkey
If N-2=0 then X=1./X
For I=1 to 10
    J=NSI(I)
    'Print "A: ";J;T(J),"B: ";I;NSI(I),
    'Print:Print
    T(J)=T(J)*X
    'Print "C: ";J;T(J)
Next I
'Print
'Print "N: ";N, SIMLT
'Sleep
'GetKey
Return
End Sub
'-----------------------------------------------------------------------------
Sub BRETH
'Print "BRETH: "
    'Subroutine BRETH
'This does graphic output of total ventilation (DVENT in main
'program), resp. rate (RRATE), art. pCO2 (RC2PR), and art. pO2
'(RO2PR). T2, T2 etc. used as in S/R BAGER
'COMMON KT,KL,INI,NW1,NW2,JKL,ITRIG(73),NEOF,NW
'COMMON NFLAG,J3,ISPAR,NA,NB,NC,ND,NE(8)
'COMMON NARTI,MT,K2,K4,INDEX
'COMMON T1(47),RRATE,T2(2),DVENT,T3(20),RO2PR,T4,RC2PR,T5(8),BUBBL
'T1, T2 are dummy variables which put VO2CT, FT etc in the right position
'in the COMMON block, eg VO2CT is the 31st variable of the large variable list.
'Declare Function IFUNC (Z as single) as single
'Function IFUNC (Z as single) as single 
'    Return Int(Z*0.5+1.1)
 '   End Function
'Dim as Single RO2PR,RC2PR,RRATE,DVENT,BUBBL
'DIM AS INTEGER Drucker = FREEFILE
DIM AS INTEGER Drucker = FREEFILE
Dim as Integer I,KK,KD,KR,KH,KN,IOK(100),K,FF
Dim as String NOMEN,EXT,FNAME,PFAD
'When the sub is called these variables are set to zero!
'Print "Anfg. Breth ";KZAHL;ZEIT1;ZEIT2
'sleep
'getkey
'DATUM=MID(Date,1,2)+Chr(95)+MID(Date,4,2)+Chr(95)+MID(Date,7,4)
'ZEIT=MID(Time,1,2)+Chr(95)+MID(Time,4,2)+Chr(95)+MID(Time,7,2)
PFAD="C:\"
EXT=".txt"
FNAME=PFAD+"MacPufSess\"+"S "+DATUM+CHR(95)+ZEIT+EXT
'Print DATUM,ZEIT,FNAME
'sleep
'getkey
'FNAME="Session"+EXT
FF=FreeFile
'Print FNAME,FF
'Sleep Getkey
'ZEIT1=J3*60+ND 'Time in sec
Close #FF
Open FNAME for APPEND as #FF
'Print Err:Sleep:GetKey
If Err >0 then Print "Opening file failed 1, Err: ";Err:Sleep:GetKey:Goto L90
'Print "KZahl: ";KZAHL
If KZAHL=1 then ZEIT1=J3*60+ND:ZEIT2=0:goto L85
ZEIT2=J3*60+ND
'Print "Zeitdiff: ";ZEIT2-ZEIT1;" ";KZAHL;" ";ZEIT1;" ";ZEIT2
'sleep
'getkey
'If ZEIT2-ZEIT1<9 then goto L86
L85:
Print #FF, RO2PR;Chr(09);PJ;Chr(09);RC2PR;Chr(09);RPH;Chr(09);RC3CT;Chr(09);RRATE;_
Chr(09);DVENT;Chr(09);COADJ;Chr(09);RLACT;Chr(09);ND
Close #FF
ZEIT1=ZEIT2
L86:
KZAHL=KZAHL+1
'Print KZAHL;ZEIT1;ZEIT2
'sleep
'getkey

'Print KZAHL;ZEIT1;ZEIT2
'sleep
'getkey
NOMEN=""
FNAME=""


L90:
Close #FF
NOMEN=""
FNAME=""

OPEN LPT "LPT:HP Deskjet D1400 series,,EMU=TTY" AS #Drucker
Dim as String DOT(1),BLANK(1),C(1),OX(1),F(1),V(1),X(2),XLINE(65)
DOT(1)=".":BLANK(1)=" ":C(1)="C":OX(1)="O":F(1)="F":V(1)="V":X(1)="*":X(2)="<"
'IOK(I)=(I-1)*(65-I)
'RO2PR=100.:RC2PR=42.2:RRATE=15.:DVENT=11.:BUBBL=7.
'J3=45:ND=30     
IOK(I)=(I-1)*(65-I)    
KD=IFUNC(RO2PR)
KK=IFUNC(RC2PR)
KR=IFUNC(RRATE)
KH=IFUNC(DVENT)
KN=IFUNC(BUBBL)-1
For K= 1 to 65
    XLINE(K)=BLANK(1)
Next K
'Print "K's: ";KD,KK,KR,KH,KN
'Print OX(1);"!"
'sleep
'getkey
XLINE(1)=DOT(1)
If IOK(KD)<0 then goto L110
if KD>65 then KD=65:XLINE(KD)=X(2):Goto L110
XLINE(KD)=OX(1)
L110:
If IOK(KK)<0 then goto L120
if KK>65 then KK=65:XLINE(KK)=X(2):Goto L120
XLINE(KK)=C(1)
L120:
If IOK(KR)<0 then goto L130
if KR>65 then KR=65:XLINE(KR)=X(2):Goto L130
XLINE(KR)=F(1)
L130:
If IOK(KH)<0 then goto L140
if KH>65 then KH=65:XLINE(KH)=X(2):Goto L140
XLINE(KH)=V(1)
L140:
If IOK(KN)<0 then goto L150
if KN>65 then KN=65:XLINE(KN)=X(2):Goto L150
XLINE (KN)=X(1)
L150:
'CONTINUE
'Print " (KPa)(0)            ";"(4)";"            ";"(8)";"           ";"(12)";
'Print "           ";"(16)"
'Print "  MINS 0";"        ";"20";"        ";"40";"        ";"60";"        ";
'Print "80";"        ";"100";"       ";"120"
'Print " +SECS .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";
'Print "    .";"    .";"    .";"    ."
Print using "####.";J3;
Print using "## ";ND;
'Print J3*60+ND;
For I=1 to 65
Print XLINE(I);
Next I
Print
'Print XLINE(65)
Goto L200
Print #Drucker, "###.";J3;
Print #Drucker,"## ";ND;
For I=1 to 65
Print #Drucker, XLINE(I);
Next I
Print #Drucker,
L200:
'
'GetKey
Return
End Sub
'-----------------------------------------------------------------------------

Sub DEATH (ByRef LL1 as Integer,ByRef LL2 as Integer,ByRef LL5 as Integer)
   'Print "DEATH: "
    'Subroutine DEATH(LL1,LL5)
'This S/R checks for 1. Errors, 2. Death, 3. Intolerable acidosis
'Check: is LL3 needed outside von DEATH? Not in COMMON, not in Call List?
Dim as Integer MM(6),I,LL3
'Equivalence (PD,T(4)),(TPH,T(59)),(PG,T(75)),(BUBBL,T(83))
MM(1)=60:MM(2)=87:MM(3)=88:MM(4)=95:MM(5)=41:MM(6)=42
IF LL5>=0 then goto L140
For I=1 to 6
LL5=MM(I)
If T(LL5)-0.001<0. then goto L110
Next I
LL5=0
L110:
Print using " * Factor ## has gone negative - time interval probably too long.";LL5
LL3=3
goto L360
'Check for lethal conditions
L140:
LL1=1
If PG >=0. then goto L150
L145:PG=0.
L150:
If PG-7.>0. then goto L190
If TPH-6.63<=0. then goto L210
If TPH-7.8>0. then goto L230
If BUBBL-280.<=0. then goto L300
Goto L240
L190:
Print:Print "Anoxemia has been severe and irrecoverable"
goto L270
L210:
Print:Print " Tissue pH has fallen to a lethally low level"
Goto L270
L230:
Print:Print " Tissue pH has risen to a lethally high level"
Goto L270
L240:
Print:Print " The brain is irrecoverably full of gas bubbles"
L270:
Print:Print " Your patient has died. RIP"
COADJ=0.0
L290:
LL1=2
Goto L360
'Check for intolerable acidosis during exercise
L300:
If PD-290.<0. then goto L360
If TPH-7.1>0. then goto L360
Print " ...... I can't go on .... "
L360:
TVAR
Return
end sub
'------------------------------------------------------------------------------
Sub DELAY
    'Print "DELAY: "'
    'SUBROUTINE DELAY
'COMMON KT,KL,INI,NW1,NW2,JKL,ITRIG(73),NEOF,NW
'COMMON NFLAG,J3,ISPAR,NA,NB,NC,ND,NE(8)
'COMMON NARTI,MT,K2,K4,INDEX
'COMMON T1(30),VO2CT,T2(29),VC2CT,T3(19),FT,T4(4),VC3MT,T5(6),
'xCOADJ,T6(3),TC2PR,T7(23),TDLAY(40)
'T1, T2 are dummy variables which put VO2CT, FT etc in the right position
'in the COMMON block, eg VO2CT is the 31st variable of the large variable list.
'Delay line for circulation of gases etc
Dim as Integer NFT,M,I,N
NFT=Int(13.2*Sqr(COADJ*FT))
'Print "COADJ,FT: ";COADJ,FT
'Print "NFT: ";NFT, INDEX
'Sleep
'GetKey
If NFT-10>0 then NFT=10
M=INDEX+NFT-1
For I = INDEX to M
N=I
If N-10>0 then N=N-10
TDLAY(N)=VO2CT
TDLAY(N+10)=VC2CT
TDLAY(N+20)=VC3MT
TDLAY(N+30)=TC2PR
Next I
'Print VO2CT,VC2CT,VC3MT,TC2PR
N=INDEX+NFT
If N-10>0 then N=N-10
VO2CT=TDLAY(N)
VC2CT=TDLAY(N+10)
VC3MT=TDLAY(N+20)
TC2PR=TDLAY(N+30)
INDEX=N
'Print VO2CT,VC2CT,VC3MT,TC2PR
'Sleep
'GetKey
Return
end sub
'------------------------------------------------------------------------------
Sub GASES (PH as Single)
'Print "Gases";
'Subroutine GASES (PO2,PC2,O2CON,C2CON,PH,SAT)
'This S/R takes any values of pO2, pCO2 and pH and works out
'O2 content and CO2 content (and saturation of O2) and
'returns results to calling routine. Maths is that of Kelman who
'has published these computer routines.
'Temperatures, hemoglobin and PCV are also allowed for.
'Refer to Kelman's paper for details
'COMMON NTAB(101)
'COMMON Y1(13),TEMP,Y2(3),HB,PCV,Y3(23),DPH
'Above contains all necessary variable which, to save space,
'are referred by array numbers rather than identifying letters.
'See Main Program for names of variables in array Y().
Dim as single A1,A2,A3,A4,A5,A6,A7
Dim as Single X,P,T,SOL,DOX,DR,CP,CC,H,PK
'Data -8.532229e3,2.121401e3,-6.707399e1,9.359609e5,-3.134626e4,2.396167e3,-6.710441e1
'Read A1,A2,A3,A4,A5,A6,A7
A1=-8.532229e3:A2=2.121401e3:A3=-6.707399e1:A4=9.359609e5:A5=-3.134626e4
A6=2.396167e3:A7=-6.710441e1
X=PO2S*10.^(0.4*(PH-DPH)+0.024*(37.-TEMP)+0.026057669*(Log(40./PC2S)))
If X-0.1<0. then X=0.1
If X-10.>=0.0 then goto L130
SAT=(0.003683+0.000584*X)*X
Goto L140
L130:
SAT=(X*(X*(X*(X+A3)+A2)+A1))/(X*(X*(X*(X+A7)+A6)+A5)+A4)
L140:
O2CON=HB*SAT*1.34+0.003*PO2S
P=7.4-PH
PK=6.086+P*0.042+(38.-TEMP)*(0.00472+0.00139*P)
T=37.-TEMP
SOL=0.0307+(0.00057+0.00002*T)*T
DOX=0.59+(0.2913-0.0844*P)*P
DR=0.664+(0.2275-0.0938*P)*P
T=DOX+(DR-DOX)*(1.0-SAT)
CP=SOL*PC2S*(1.0+10.^(PH-PK))
CC=T*CP
H=PCV*0.01
C2CON=(CC*H+(1.0-H)*CP)*2.22
'Print " End GASES "
Return
End Sub
'******************************************************************************
Sub GSINV(PH as Single)
'Print:Print "GSINV: ";
    'Print "O2CON, C2CON:  ";O2CON,C2CON
    'Print "PO2S, PC2S  :";PO2S,PC2S
    'Print
    
    'Subroutine GSINV (PO2,PC2,O2CON,C2CON,PH,SAT)
'This S/R reverses gases and by an optimised process of
'successive approximation uses O2 and CO2 contents to compute
'the respective partial pressures
'COMMON NTAB(101)
'COMMON Y1(13),TEMP,Y2(3),HB,PCV,Y3(23),DPH,Y4(40),PK
Dim as Single ERR1,FACT,D1Z,D2Z,XX2,YY2,XP2,YP2,D1,D2,DS,X,Y,XX1,YY1,XP1,YP1
Dim as Single O2CON1,C2CON1
Dim as Integer ICH1,ICH2,ICH3,ICH4
O2CON1=O2CON:C2CON1=C2CON
ERR1=0.01:FACT=1.0'ERR1 in program 0.01
'ERR1 reduced from 0.01 to 0.001 which ends the otherwise infinite 
'loop in GSINV in spite of stable partial pressure values. Problems? (*HS).
'Flags ICH1/3 signify states in approximation of pO2, ICH2/4
'similarly for pCO2. ICH1=0 means pO2 is well enough approximated
'with absolute accuracy 'ERR1' in O2CON (oxygen content)
'ICH1=1 means a bracketing procedure to establish bounds on the 
'pO2 value is in progress.
'ICH3=1 a bracket has been established and a linear interpolation made
'for next extimate of pO2.
'ICH3=0 No bracket at this time.
'Set magnitude initial step lengths for bracketing procedure to find
'bounds within pO2 and pCO2 must lie.
'Set initial flag values as if current values for pO2 and pCO2 were
'as interpolated at a bracket.
D1Z=2.
D2Z=2.
ICH1=1
ICH2=1
ICH3=1
ICH4=1
'Start search from current estimates of pO2 and pCO2
'Call GASES(PO2,PC2,X,Y,PH,SAT)

L100: GASES(PH)
X=O2CON:Y=C2CON
'Print "GS1: ICH1:",ICH1;" ICH2:",ICH2;" ICH3:",ICH3;" ICH4:",ICH4
'sleep
'Getkey

'XX2/YY2 store discrepancy between known content and values
'computed using current guesses of pressures
XX2=X-O2CON1
YY2=Y-C2CON1
'Print
'Print "O2DIFF, C2DIFF: ";XX2,YY2
'Print "O2CON, C2CON:  ";O2CON,C2CON
'Print "PO2S, PC2S  :";PO2S,PC2S
'sleep
'GetKey
'avoid error in sign function below
if XX2=0. then XX2=0.001
If YY2=0. then YY2=0.001
'XP2, YP2 store pressure estimates corresponding to discrepancies 
'XX2, YY2.Thes are all used in in interpolation procedure below .
'Form new search vectors D1/D2, after bracketing and interpolation,
'according to sign of discrepancies XX2/YY2.
'Reset ICH3/4 to zero.

L140:
XP2=PO2S
YP2=PC2S
If ICH3-1<>0 then goto L160
ICH3=0
'D1=SIGN(D1Z,-XX2)
D1=Sgn(-XX2)*Abs(D1Z)
'Print "D1(1): ";D1
'Sleep
'GetKey
L160:
if ICH4-1<>0 then goto L210
ICH4=0
'D2=SIGN(D2Z,-YY2)
D2=Sgn(-YY2)*Abs(D2Z)
'Print "D2 (1): ";D2
'Test for convergence of calculated and given contents. Set flags
'ICH1/ICH2 to zero if contents approximated well enough
If Abs(XX2-ERR1)>0 then goto L190
ICH1=0
L190:
If Abs(YY2-ERR1)>0 then goto L210
ICH2=0
'Finish if both contents approximated well enough
If ICH1+ICH2<=0 then goto L450'Return
L210:
DS=D1*ICH1
X=PO2S+DS-PO2S*0.25
If X>=0 then goto L230
DS=-PO2S*0.75
L230:
PO2S=PO2S+DS
'Compute trial increment of pCO2 as for pO2 above
DS=D2*ICH2
X=PC2S+DS-PC2S*0.25
If X>=0. then goto L250
DS=-PC2S*0.75
L250:
PC2S=PC2S+DS
'Enforce lower bound on pO2/pCO2
If PO2S-0.1>=0. then goto L270
PO2S=0.1
L270:
If PC2S-0.1>=0. then goto L290
PC2S=0.1
'Compute contents with trial increments made to pO2/pCO2
L290:
'Call GASES(PO2,PC2,X,Y,PH,SAT)
GASES(PH)
X=O2CON:Y=C2CON
'Print "GS2: ICH1:",ICH1;" ICH2:",ICH2;" ICH3:",ICH3;" ICH4:",ICH4
'sleep
'Getkey
'Save last one pairs of values PO2/O2CON discrepancy in
'XX1,XP1, similarly for PC2/C2CON discrepancy in YY1/YP1.
'Then place latest pairs of values in XX2/XP2, YY2/YP2 as before.
XX1=XX2
XX2=X-O2CON1
XP1=XP2
XP2=PO2S
YY1=YY2
YY2=Y-C2CON1
YP1=YP2
YP2=PC2S
'First look at discrepancy in O2CON (ICH1=0). If within limit,
''ERR1" accept values of PO2 and look at C2CON.
'Print "XFehler: "; Abs(XX2-ERR1)
If Abs(XX2)-ERR1>0. then goto L310

ICH1=0
Goto L360
'If O2CON discrepancy still too high test whether trial pO2 has overshot
'the correct solution

L310:
ICH1=1
If XX2*D1=0. then goto L360
If XX2*D1>0. then goto L350
'If not overshot, solution lies further along direction of trial increment.
'Compute a new trial increment based on last two pairs of conents/pressure
'values. Factor 'FACT' was used in optimising the algorithm in practice.
L320: If XP2-XP1<>0. then goto L340
'D1=SIGN(D1Z,-XX2)
D1=Sgn(-XX2)*Abs(D1Z)
'Print "D1 (2) ";D1'Sleep:GetKey
Goto L360
L340:
D1=(XP2-XP1)*Abs(XX2)/(FACT*Abs(XX2-XX1))
Goto L360
'If overshot, ie bracket found on pO2, linearly interpolate using last 2 pairs
'of pressure/content values to get new trial value for pO2. Set ICH3=1 to 
'indicate bracketed interpolated value for new search to be initiated 
'at statement L100. Reduce initial search vector to aid convergence.
L350:
ICH3=1
PO2S=XP1+(XP2-XP1)*Abs(XX1)/(Abs(XX2)+Abs(XX1))
D1Z=D1Z/2.
'Now repeat above procedure for disrepancy in C2CON
L360:
'Print "YFehler: "; Abs(YY2-ERR1)
If Abs(YY2)-ERR1>0. then goto L380
ICH2=0
Goto L430
L380:
ICH2=1
If YY2*D2=0. then goto L430
If YY2*D2>0. then goto L420
'Keep going
L390:If YP2-YP1<>0. then goto L410
'D2=SIGN(D2Z,-YY2)
D2=Sgn(-YY2)*Abs(D2Z)
'Print "D2 (2): ";D2
Goto L430
L410:
D2=(YP2-YP1)*Abs(YY2)/(FACT*Abs(YY2-YY1))
Goto L430
'Bracket found
L420:
ICH4=1
PC2S=YP1+(YP2-YP1)*Abs(YY1)/(Abs(YY1)+Abs(YY2))
D2Z=D2Z/2.
'Finish if both O2CON and P2CON well enough approximated. If either
'pO2 or pCO2 search is at a bracket, establish new search at statemen L100.
'Else, if both are continuing along previous directions of search, proceed 
'from statement L210
L430:
'Print "L430: ICH1, ICH2", ICH1,ICH2
'Sleep
'GetKey
If ICH1+ICH2<=0 then goto L450
L440:
'Print "L440: ICH3,ICH4 ", ICH3, ICH4
'Sleep
'GetKey
If ICH3+ICH4-1>=0 then goto L100
Goto L210
L450:
'Print "End GSINV "
Return
end sub
'*******************************************************************************

Sub BAGER(ByRef N as Integer, ByREf CA as Single, ByRef CB as Single, ByREf CC as Single)
    'Print "BAGER: "'Program
   'Subroutine BAGER (N,CA,CB,CC,SIMLT)
'This deals with bag rebreathing etc.
'Common KT,KL,INI,NW1,NW2,JKL,ITRIG(73),NEOF,NW
'Common NFLAG,J3,ISPAR,NA,NB,NC,ND,NE(8)
'COmmon NARTI,MT,K2,K4,INDEX
'Common array uses T1,T2 etc in many subroutines to save unused lists
'of named variables
'Common   FIO2,FIC2,T1(4),T2(5)
'Common   BARPR,TEMP,T3(22)
'Common   BAGO, BAGC,AO2MT,AC2MT
'Common   T4(6),TIDVL,RRATE,T5(21)
'Common   DSPAC
'Common   REFLV,T6(7),QB,PW
'Common   FT,T7(17),
'Common   AVENT,PL
'Common   T8(12),
'Common   QA,T9(2),BAG
Dim as Integer NNN,NN
Dim as Single CORR,X,XNMT,BAGP,BAGPO,XXX
Do
If N=1 then goto L100
If N=2 then goto L430
If N=3 then goto L430
If N=4 then goto L440
If N=5 then goto L500
If N=6 then goto L510
If N=7 then goto L230
Loop
L100: If NW2>19. then goto L120
Print " Do you want to make your subject "
Print " 1. Close the glottis, 2. Collect exspired air in a bag,"
Print " 3. Rebreathe from a bag, 4. Same, with CO2 absorber attached,"
Print " 5. Restore status quo - breathing air, glottis open, no bag."
NW2=0.
L120:
NXTWD(7,XXX,NNN,1,5)
'Index PL codes BAGER calls from main program
PL=0.
If NNN=1 then goto L410
If NNN=2 then goto L150
If NNN=3 then goto L140
If NNN=4 then goto L130
If NNN=5 then goto L170
L130:
PL=PL+1.
L140:
PL=PL+1.
L150:
PL=PL+1.
If NNN-2>0. then goto L200
' Collection of exspired air into empty bag
L160:
BAG=0.00001
BAGO=000000.1
Goto L300
'Restore all to normal. Bag stays filled as was left.
L170:
FIO2=20.93
FIC2=0.03
Print " *** Glottis open - Bag disconnected - Breathing air ***"
L190: TVAR:Return
' Bag rebreathing option
L200:
If NW2>19 then goto L220
NW2=0
Print " Do you want 1. 100% O2, 2. a gas mixture, 3. to go on with previous bag"
L220:NXTWD(7,XXX,NNN,1,3)
'Set BTPS/STPD correction factors
L230:
X=BARPR/SIMLT-1.2703*TEMP
CORR=(273.+TEMP)/(X*0.3592)
IF N-7=0 then goto L530
L240:if NNN-3=0 then goto L390
L250: if NW2>19 goto L280
Print " Give initial bag volume in bag, in ml (BTPS)"
L280: NXTWD(7,BAG,NN,300,30000)
If NNN-2>0 then goto L390
If NNN-2=0 then goto L310
L290:BAGO=BAG/CORR:If BAGO<0. then BAGO=0.
L300:BAGC=0.0000001
Goto L390
L310:
IF NW2>19 then goto L330
NW2=0
Print " Specify percent CO2"
L330:NXTWD(7,BAGC,NNN,0,100)
If NW2>19 then goto L350
NW2=0
Print " Now percent Oxygen"
L350:NXTWD(7,BAGO,NNN,0,100)
X=BAGO+BAGC
If X-100.<=0. then goto L380
Print " Ridiculous - try again!"
Goto L310
L380:
X=BAG*0.01/CORR
BAGO=BAGO*X:If BAGO<0. then BAGO=0.
BAGC=BAGC*X:If BAGC<0. then BAGC=0.
L390:
X=CORR*100./BAG
XXX=BAGO*X:If XXX<0. then XXX=0.
X=BAGC*X
Print " *** ml";BAG;:Print " bag connected, containing ";XXX;
Print "% O2 and ";X;:Print "% CO2"
TVAR:Return
'Glottis closed; store lung volume for reference
L410:
Print " *** Glottis closed until factor 100 changed again."
REFLV=VLUNG
PL=-1.
TVAR:Return
'Set inspired air equal to mixture in the bag
L430:
X=1./(BAG*CC+0.00001)
FIO2=BAGO*X
FIC2=BAGC*X
If PL >2.5 then FIC2=0.
TVAR:Return
'DSPVT is gas in upper airway breathed out into the bag
L440:If PL>1. then goto L450
'Next section for bag collection only
X=DSPAC*RRATE*FT
BAG=BAG+CA+X
X=X*CC
BAGO=BAGO+CB*AO2MT+FIO2*X
BAGC=BAGC+CB*AC2MT+FIC2*X
TVAR :Return
'Test if tidal volume is greater than bag volume, if so code exit from main.
L450:
If BAG>TIDVL then goto L470
Print " *** Tidal volume to big for bag ***"
CB=-10000.
TVAR:Return
L470:If PL<2.5 then goto L480
BAG=BAG-AC2MT*0.01/CC:If BAG<0. then BAG=0.
BAGC=0.
Goto L490
'This skipped if CO2 absorber in circuit
L480:BAGC=BAGC+QB
L490:BAGO=BAGO-QA
BAG=BAG-AVENT+CA:If BAG<0. then BAG=0.
TVAR:Return
L500:
VLUNG=CA*0.01/CC
TVAR:Return
L510:Print using " Lung volume ###### ml(BTPS)";VLUNG
TVAR:Return
'Display section for 'Inspect' table in SR DEADY
L530:
If BAG <0.001 then BAG=0.001
X=X*(273+TEMP)/(273*BAG)
BAGP=BAGC*X*SIMLT:If BAGP<0. then BAGP=0.
BAGPO=BAGO*X*SIMLT:If BAGPO<0. then BAGPO=0.
XNMT=BAG/CORR-BAGO-BAGC
Print using " Bag        #####.# ######.# ####### ########     N2=####### *Bag Vol. ####### (BTPS)";BAGPO;BAGP;BAGO;BAGC;XNMT;BAG
TVAR
Return
End Sub
'-------------------------------------------------------------------------------
Sub KONST( ByRef NREPT as Integer)
    'Subroutine KONST
'KONS(ByRef as Integer NREPT)
'This S/R computes all parameters constant during a run.
'Print "KONST: "
Dim as Single X
DIM AS INTEGER Drucker = FREEFILE
OPEN LPT "LPT:HP Deskjet D1400 series,,EMU=TTY" AS #Drucker


If NC>1 Or NB=1 then goto L111
Print " (KPa) (0)            ";"(4)";"            ";"(8)";"           ";"(12)";
Print "           ";"(16)"
Print "  MINS  0";"        ";"20";"        ";"40";"        ";"60";"        ";
Print "80";"        ";"100";"       ";"120"
Print " +SECS  .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";
Print "    .";"    .";"    .";"    ."
Goto L111:

'OPEN LPT "LPT:HP Deskjet D1400 series,,EMU=TTY" AS #Drucker
Print #Drucker, "  (KPa) 0)            ";"(4)";"            ";"(8)";"           ";"(12)";
Print #Drucker, "           ";"(16)"
Print #Drucker, "  MINS  0";"        ";"20";"        ";"40";"        ";"60";"        ";
Print #Drucker, "80";"        ";"100";"       ";"120"
Print #Drucker, " +SECS .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";"    .";
Print #Drucker, "    .";"    .";"    .";"    ."
'CLOSE #Drucker  ' CLOSE erst am Ende des ganzen Run!!!!
L111:
X=TIDVL*RRATE
If (CO>340. Or PD>950. Or SHUNT>0.5) And FT>0.04 then goto L112
If (CO>210. Or PD>350. Or FIO2<7. Or CO<5. Or PL<0. Or X<1. Or PR<5.) And FT>0.09 then goto L114
Goto L120
L112:
FT=0.0333334
Goto L116
L114: FT=0.0833334
L116:
Print:Print " Iteration interval too long for this situation. I have shortened it."
Print " To change back, use *4. Run change* option."
L120:
If (N(95)=0 And X<1.) Or PL<-0.5 Or PR<1. then AVENT=0.001
If TEMP >=30. then goto L140
Print " *Sorry, no can do, let us say 30 degrees centigrade."
TEMP=30.
L140:
C(1)=1000./VBLVL
C(2)=100./VBLVL
C(3)=0.0203*HB
C(4)=(ELAST+105.)*0.01
C(5)=2.7/(VC+0.4)
C(6)=FEV*25.+29.
C(7)=CONSO*PD*0.00081*(TEMP-26.)^1.05
C(8)=(30.-PEEP*5./ELAST)*0.0016*CONOM*(TEMP-12.2)
C(9)=(C(7)-CONSO)*0.01
C(10)=FT*0.005
C(11)=BARPR/SIMLT-1.2703*TEMP
C(12)=C(11)*0.003592/(273.+TEMP)
C(13)=0.9/TVOL
C(14)=SHUNT+1.
C(15)=2./WT
C(16)=CO*0.01
C(17)=FT*10.
C(18)=VADM*80.
C(19)=(PD-90.)*RVADM*0.05
If C(19)<-1. then C(19)=-1.
C(20)=650.*VC
If BULLA>0. then C(20)=C(20)+Sqr(BULLA)*15.
C(21)=(40.-PEEP)*0.025
C(22)=4.5/FT
C(23)=20.*SIMLT/BARPR
C(24)=CONOM*0.3
C(25)=100.*C(12)
C(26)=7./TVOL
C(27)=FT*0.1
C(28)=30000./(VBLVL+1000.)
C(29)=FT*0.0039*WT^0.425*HT^0.725
C(30)=520./TVOL
C(31)=2.7/TVOL
C(32)=C(29)+0.0000001
C(33)=C(3)*308.-TVOL*0.65*C(30)
C(34)=0.004/(C(12)*FT)
C(35)=0.01/C(12)
C(36)=7.7*C(13)
C(37)=SPACE1/FT
C(38)=20./VLUNG
C(39)=FT*0.127
C(40)=C(29)*(PD-25.)*1.3
C(41)=FT*(TEMP-24.5)*1.82
C(42)=0.003*C(29)
C(43)=1./(1.+7.7*C(3))
X=(PD*0.01)^0.8*VC*0.2
C(44)=AZ*X*0.0132
C(45)=BZ*X*0.008
C(46)=CZ*0.78*((C(7)*0.00051)^0.97+0.01)
X=0.5+356./C(11)
If X>1.then X=1.
C(47)=PR*0.000214*(TEMP-29.)^1.5*X
C(48)=0.04*(TEMP-26.)*VC
C(49)=9.+Sqr(ELAST*1.25)
C(50)=(150.+PR)*0.0275*(TEMP-17.)
C(51)=RRATE*TIDVL*0.001
C(52)=VLUNG*0.03-20.+BULLA
C(53)=0.8/FT
C(54)=XDSPA*0.001
C(55)=0.1/FT
C(56)=0.001/FT
C(57)=FT*60.
C(58)=FT*1.27
C(59)=FT*0.3
C(60)=FT*0.008
C(61)=0.22/FT
C(62)=FT*240000./(CZ+300.)
C(63)=FT*C(7)*0.12
C(64)=0.01488*HB*(TVOL+VBLVL*0.001)
C(65)=7.324-CZ*0.00005
C(66)=C(65)-0.002
C(67)=CZ-30.
C(68)=FT*3000./(PD+200.)
C(69)=(FITNS-20.)*0.00035
C(70)=C(29)*1.3
ADDC3=ADDC3/NREPT
DPH=7.4+(DPG-3.8)*0.025
If DPH>7.58 then DPH=7.58
If DPG>13. then DPG=13.

'sleep
'GetKey
'return
end sub
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Sub DUMP (ByRef LL1 as Integer)
Dim as Single SPEICHER (180)
Dim as Integer FF,I,NNN, ITEMS
Dim as Single XXX
Dim as String NOMEN,EXT,FNAME,TEXT(100),VORNAME
EXT=".MCP"
VORNAME="C:\Program Files (x86)\FreeBasic\"
L90:
If LL1>9 then goto L140:
If NTAB(5)=2 then goto L120
If LL1 >1 then goto L170
If NW2 >19 then goto L110
Print " Do you want to  1. Store present state, 2. Recall stored experiment, 3. Do nothing, "
Print "                 4. Get list of available files."
L110:
NXTWD (14, XXX,NNN,1,4)
If NNN=1 then goto L120
If NNN=2 then goto L170
If NNN=3 then goto L300
If NNN=4 then goto L180
L120:
Input "Give name for experimental situation to be stored:";NOMEN
'NOMEN="TEST"
FNAME=VORNAME+NOMEN+EXT
FF=FreeFile
'Print FNAME,FF
'Sleep Getkey
Open FNAME for APPEND as #FF
'Print Err:Sleep:GetKey
If Err >0 then Print "Opening file failed 2":Sleep:GetKey:Goto L200

L140:
For I=2 to 20
    SPEICHER(I)=NTAB(I)
Next I
For I=21 to 180
    SPEICHER(I)=T(I-20)
Next I
For I=1 to 180
Print #FF, SPEICHER (I)
Next I
Print " *** Stored at this point as ";FNAME
'Sleep
'Getkey
Close #FF
NOMEN=""
FNAME=""
Return
L170:
Input "Give name for stored experimental situation:";NOMEN
FNAME=VORNAME+NOMEN+EXT
FF=FreeFile
Open FNAME for Input as #FF
If Err >0 then Print "Opening file failed 3":Print Err:Sleep:GetKey:Goto L200
For I=1 to 180
    Input #FF, SPEICHER(I)
Next I
For I=2 to 20
    NTAB(I)=SPEICHER(I)
Next I
For I=21 to 180
    T(I-20)=SPEICHER(I)
Next I
Print "File ";FNAME;" successfully retrieved."
Close #FF
VART
NOMEN=""
FNAME=""
L200:LL1=1
Goto L300
L180:
FF=FreeFile
FNAME=VORNAME+"NameList"+".MCP"
Open FNAME for Input as #FF
If Err =0 then Print "File opened sucessfully":Goto L185:
Print " Open failure 4, Error: ";Err;:Sleep:GetKey: Goto L300:
L185:
Input #FF, ITEMS
For I=1 to ITEMS
    Input #FF, TEXT(I)
    Print I;".";" ";TEXT(I)
Next I
Print 
Sleep
GetKey
Close #FF
FNAME=""
Goto L90:
L300:
Return
End Sub
'----------------------------------------------------------------------------
Sub NXTWD (ByRef LL3 as Integer, ByRef XXX as Single, ByRef NNN as Integer, IMIN as Integer, IMAX as Integer)
    'Subroutine NXTWD (LL3, XXX,NNN, IMIN, IMAX)
' This S/R and functions VALUE and V1 allow numbers to be entered
'in free format and retrieved in standard real or integer form, or
'fed into arrays if needed or, if *Q* is typed, reference is made to
' S/R QUERY to explain the meaning of the question. Otherwise, this 
'S/R and functions are selfexplanatory.
Dim as Integer J,NREF,IL,L,K,I
Dim as String ICHAR (18),EINGABE
Dim as Single XMIN, XMAX
'COMMON KT,KL,INI,NW1,NW2,JKL,ITRIG(73),NEOF
'IMIN=-100
'IMAX=100000

'Data " ","1","2","3","4","5","6","7","8","9","0",".","-","/","B","F","U","Q"
'For J=1 to 18
 '   Read ICHAR(J)
 '   Next J
ICHAR(1)=" ":ICHAR(2)="1":ICHAR(3)="2":ICHAR(4)="3":ICHAR(5)="4":ICHAR(6)="5":ICHAR(7)="6":ICHAR(8)="7":
ICHAR(9)="8":ICHAR(10)="9":ICHAR(11)="0":ICHAR(12)=".":ICHAR(13)="-":ICHAR(14)="/":ICHAR(15)="B"
ICHAR(16)="F":ICHAR(17)="U":ICHAR(18)="Q"
XMIN=IMIN
XMAX=IMAX

EINGABE=""
'Print
'Print "NXTDW: "
Print
L100:
For J=1 to 20
    ITEM(J)=1
Next J
If NW2 >= 1 then goto L200
NREF=NEOF
For J=1 to 72
    ITRIG(J)=ICHAR(1)
Next J
IL=1
'GetKey
Input;EINGABE
Print
'Sleep
'GetKey
'Print "Input string: "; EINGABE
'Print Len (EINGABE)
For J=1 to Len(EINGABE)
    ITRIG(J)=Mid (EINGABE,J,1)
    ITRIG1(J)=ValInt(ITRIG(J))
Next J
'For J=1 to Len (EINGABE)
 '   Print ITRIG(J),
'Next J
'sleep
'GetKey
If ITRIG(1)<>ICHAR(1) And ITRIG(1) <> ICHAR(14) then goto L190
L160: Print "A number must be entered!"
Goto L240:
L180: QUERY (LL3):'Print "Q: ",LL3
Goto L240
L190:
If ITRIG(IL) = ICHAR(18) then goto L180
NW1=IL
L200:
L=0
For IL=NW1 to 72
    For J=1 to 14
        If ITRIG(IL)=ICHAR(J) then goto L260:
    Next J
If ITRIG(IL)=ICHAR(15) Or ITRIG(IL)=ICHAR(16) Or ITRIG(IL)=ICHAR(17) then goto L340
L220:
Print " Only numbers are acceptable."
L240: Print "Please try again."
NW2=0
NEOF=NREF
goto L100
L260:
if J<>1 And J<>14 then goto L270
If L > 0 then goto L290
Goto L280
L270:
L=L+1
If L=20 then goto L360
ITEM(L)=J
If J=1 goto L290
L280: Next IL
IL=72
if NW2>19 then goto L160
L290:
NW1=IL+1
'detect slash/delimiters
If J=14 goto L330
If J=1 And NEOF<=1 then NW2=0
If J=1 And ITRIG(NW1)=ICHAR(1) And NW2>19 then goto L380
L300:
If NW1>=72 then goto L320
'PRINT "ITEM"
'For K=1 to Len (EINGABE)
'Print "(";K;") ";ITEM(K);" ";I
'Next K
'Print
'Sleep
'GetKey
VALUE
XXX=WALUE
If XXX<XMIN Or XXX>XMAX then goto L360
NNN=Int(XXX)
'Print "Werte: ";XXX, NNN
'Sleep
'GetKey
L310:
NEOF=NEOF-1: Goto L400
L320:
NEOF=1
NW2=0
Goto L310
L330:
NW2=20
JKL=0
NEOF=1
Goto L300
L340:
Print "Don't use rude words!"
Goto L220
L360:
Print "I'm afraid your number is no good."
Goto L240
L380:
JKL=0
NW2=0
Goto L300
L400:
'TVAR
'sleep
'GetKey
'goto L100:
Return
end sub


'***********************************************************************
Sub QUERY (ByRef LL3 as Integer)
    'Subroutine QUERY
'This does explanations of questions, and is also used when calling up the model.
'LL3 is also 1st argument of subroutine NXTWD, and determines output text for 
'start, and QUERY calls

'Dim as Integer LL3
'L1:
'Input "LL3:",LL3
DIM AS INTEGER Drucker = FREEFILE
'Dim as Integer LL3
'Print
'Print "QUERY: "
Do
    if LL3=1 then goto L170
    if LL3=2 then goto L140
    if LL3=3 then goto L260
    if LL3=4 then goto L320
    if LL3=5 then goto L280
    if LL3=6 then goto L300
    if LL3=7 then goto L320
    if LL3=8 then goto L320
    if LL3=9 then goto L360
    if LL3=10 then goto L380
    if LL3=11 then goto L200
    if LL3=12 then goto L340
    if LL3=13 then goto L320
    if LL3=14 then goto L220
    if LL3=15 then goto L240
    if LL3=16 then goto L400
    if LL3=17 then goto L100
    if LL3=18 then goto L120
    'goto L100
    goto L100
    
Loop
L100:
'*** Local arrangements
Print " To proceed type 1 then press ENTER key,"
Print " to get brief description type 2 then ENTER"
Goto L420
L120:
Print "*Unless changed later, columns refer as follows ...Factor numbers below"
Print "          Exp.RQ  Art.pH   Tot-Vent-Alv    HCO3  Alv.pO2   pO2-Art-pCO2"
Goto L420
L140:
Print
Print " MacPuf is a model of the human respiratory system designed at McMaster"
Print " University Medical School, Canada, and St. Bartholomews Hospital"
Print " Medical College, London, by Dr.C.J. Dickinson, Dr.E.J.Campbell,"
Print " Dr.A.S. Rebuck, Dr.N.L. Jones, Dr.D. Ingram and Dr.K. Ahmed in 1976."
Print " The program was translated from FORTRAN to compiled BASIC by"
Print " Dr.H.Schroeder, University of Hamburg Medical School, in 2015."
Print " Dr. Schroeder also added extended storage capabilities and a graphic"
Print " output using the graphic engine of AcKnowledge by BIOPAC. This is "
Print " eplained at program Start or Restart."
Print " MacPuf was created to study gas transport and exchange."
Print " He contains simulated lungs, circulating blood and tissues."
Print " Initially, he breathes at a rate and depth determined"
Print " by known influences upon ventilation."
Print
Print " Once into a standard run of 3 minutes (changeable)"
Print " the display unit plots a vertical graph of ventilation, rate of"
Print " breathing, art. pCO2 and pO2 (symbols V, F, C and O respectively)."
Print 
Print " These data are stored also in a txt.file in C:\MacPufSess with a file"
Print " name starting with a S followed by the date and time of storage. Files"
Print " starting with T & Time stamp contain 8 data which are defined in a run"
Print " using the option 4. Change run. This is useful for bag experiments, "
Print " for example, when bag volume and its O2/CO2 (116, 37,38) amounts are to"
Print " be monitored. Both text files can be used by Acknowledge to generate "
Print " graphs."
Print " After each run, you can change anything (eg. inspired oxygen %)"
Print " then run another 3 minutes and watch the results."
L170:
'**** Local arrangements
Print
Print " If anything goes wrong, note what you did and inform your tutor."
Print " A prize of 1 Euro will be given to anyone convincingly showing"
Print " inpossible behaviour of the model in a possible clinical situation."
Print " Any time you can't understand an instruction type Q (query) and"
Print " press ENTER."
Print
Print "OK, let's go ... "
Print
Print " Symbols -- VV= tot. vent., FF= frequ. breathing, CC= art. pCO2,"
Print " OO= art. pO2.   **= N2 supersat. index if present"
Goto L420
L200:
Print " If every 6th value printed computation will take place "
Print " as specified in previous requests. Similarly for 30th."
Goto L420
L220:
Print " This allows you to store the present state of your subject and to"
Print " recreate him again through use of this same option, by backtracking."
Goto L420
L240:
Print " 1 is obvious, 2 starts another run of standard 3 minutes,"
Print " 3 starts again with a new object, 4 prints a table of most"
Print " useful values, 5 stops the program."
Goto L420:
L260:
Print " 1 is obvious 2 will return to natural ventilation if artificial"
Print " has been used and allows you to stop ventilation or to give"
Print " graded artificial ventilation , 3 allows you to store the"
Print " present state by a dump instruction, and also bring this state back"
Print " again by a backtrack instruction, 4 allows you to"
Print " alter length of run and also get selected values printed to order"
Print " 5 gives preset patients and insertion of function tests."
Goto L420:
L280:
Print " 1 gives the graph format you have seen,"
Print " 2 suppresses text and values and leaves just graphs,"
Print " 3 will allow columns of up to 8 selected variables to be printed"
Print " instead."
Goto L420
L300:
Print " This is the special type of output. Refer to the"
Print " handbook for interpretative code. *69* will give the following"
Print " in columns: 1. Time, 2. exp.RQ, 3.art. pH, 4. tot. ventilation,"
Print " 5. alv. ventln., 6. art. bicarb., 7. alv. pCO2, 8. art. pO2,"
Print " 9. art. pCO2."
Print " Other variables can be obtained by typing in a different number"
Print " or string of numbers. Refer to handbook."
Goto L420:
L320:
Print " If you cannot understand THIS, give up!":Sleep:Close #Drucker:End
Goto L420
L340:
Print " Normal is 10 sec, but for big metabolic rates, cardiac outputs or"
Print " ventilations, a smaller iteration interval, eg. 5 secs,"
Print " would be better."
Goto L420
L360:
'*** Local arrangements
Print " Type each factor no. you want to alter as a string of numbers"
Print " separated by blanks, ENTER at end."
Print " Typing 100 allows you to do special experiments, eg. close"
Print " glottis, exspire into or rebreathe from a bag."
Goto L420
'*** Local arrangements
L380:
Print " Type in the values in the usual units, eg. to alter factor 1 from"
Print " its present value to 10 (percent oxygen) type *10* and press ENTER."
Goto L420
L400:
Print " Positive end expired pressure tends to reduce cardiac output"
Print " but can improve oxygen uptake under some conditions."

L420:
Return
end sub
'*************************************************************************
Sub MINIT (ByRef LL1 as Integer, ByRef LL3 as Integer, ByRef LL4 as Integer, ByRef NREPT as Integer)
    'Call MINIt(LL1,LL3,LL4,NREPT,SIMLT)
 'Initialize all values and indices for a new subject
 'Dim as Single REFER(120)
 'Common Shared KT,KL,INI,NW1,NW2,JKL,ITRIG(),NEOF,NW as Integer
 'Common Shared NFLAG,J3,ISPAR,NA,NB,NC,ND,NE() as Integer
 'Common Shared NARTI,MT,K2,K4,INDEX as Integer
 'Common Shared T(), TDLAY ()
 Dim as Integer NE(8), I, NNN,J,K
 Dim as Single X, XXX
 'Dim as Single T(120), TDLAY(40)
 'Constants for initializing a new subject
' Data 20.93, 0.03,100.0,100.0,0.0,0.0,3000.0,5.0,3.0,100.0
' Data 100.0,100.0,760.0,37.0,0.8,13.38,12.0,14.8,45.0,3000.0
' Data 0.0,0.0,3.8,100.0,33.0,0.4,35.0,0.0,5.0,0.0
' Data 14.56,1.0,7.4,37,4.26,7.33,0.0,0.0,348.1,143.3
' Data 101.9,39.75,7.40,0.0,28.89,52.8,461.9,12.82,19.54,1540.0
' Data 5.92,5.92,47.25,19.66,14.47,51.4,10.05,56.62,7.37,23.82
' Data 51.33,195.44,473.5,10.99,1987.0,18.06,677.0,51.94,0.80,129.2
' Data 3000.0,94.18,240.0,39.9,0.0,97.13,-10.0,47.35,33.29,2.36
' Data 0.166667,4.6,0.0,45.43,318.0,71.55,25.48,25.48,34.63,0.99
' Data 22.7,1.0,5.01,19.54,178.4,40.13,45.43,437.0,710.8,0.0
' Data 47.35,76.07,571.1,4.0,564.0,0.73,0.0,7.26,1.0,1.0
 'Data -0.03,967.0,41.61,0.0,0.0,0.0,1.0,178.0,70.0,40.0
 'For J=1 to 120
  '   Read REFER(J)
     'Print REFER(J),
' next
Dim as Single Refer(120) 
Dim S(1 to 12, 1 to 10) as Single => {{20.93, 0.03,100.0,100.0,0.0,0.0,3000.0,5.0,3.0,100.0},_
{100.0,100.0,760.0,37.0,0.8,13.38,12.0,14.8,45.0,3000.0},_
{0.0,0.0,3.8,100.0,33.0,0.4,35.0,0.0,5.0,0.0},_
{14.56,1.0,7.4,7.37,4.26,7.33,0.0,0.0,348.1,143.3},_
{101.9,39.75,7.40,0.0,28.89,52.8,461.9,12.82,19.54,1540.0},_
{5.92,5.92,47.25,19.66,14.47,51.4,10.05,56.62,7.37,23.82},_
{51.33,195.44,473.5,10.99,1987.0,18.06,677.0,51.94,0.80,129.2},_
{3000.0,94.18,240.0,39.9,0.0,97.13,-10.0,47.35,33.29,2.36},_
{0.166667,4.6,0.0,45.43,318.0,71.55,25.48,25.48,34.63,0.99},_
{22.7,1.0,5.01,19.54,178.4,40.13,45.43,437.0,710.8,0.0},_
{47.35,76.07,571.1,4.0,564.0,0.73,0.0,7.26,1.0,1.0},_
{-0.03,967.0,41.61,0.0,0.0,0.0,1.0,178.0,70.0,40.0}}
K=1
For J=1 to 12
For I = 1 to 10
    Refer (K)=S(J,I)
    K=K+1
Next I
Next J
'For I= 1 to 120
   ' Print "(";I;") "; REFER(I),
'Next I
'Print "MINIT:"
 Print:Print "                  *** ---- New Subject ---- ***":Print
 'sleep
 'GetKey
 if LL1<10 then goto L200
 T(81)=0.166667
 'Print "MINIT: ";LL1, LL3
 'sleep
 'GetKey
 QUERY (LL3)
 NW2=0
 'LL3=7
 NXTWD(7,XXX,LL3,0,2)
 'Print "MINIT: XXX, NNN  ";XXX,NNN
 'sleep
' GetKey
 'ITRIG(73) controls print out of the first six factors at the end of run
 'Other indices mostly concerned with interactive dialogue (Subrout. NXTWD)
 if LL3=0 then goto L150
 L120: if LL3-2=0 then ITRIG1(73)=-5
 L140: QUERY (LL3) :goto L160
 L150: NC=3
 L160:if NW2>19 then goto L170
 Print
 Print " To use SI units (kPa) type 1, for mmHg type 2 "
 'sleep
 'GetKey
  NW2=0
  'LL3=7
 L170: NXTWD(7,XXX,NNN,1,2)
 'Print "MINIT: ";XXX,NNN
' sleep
 'GetKey
 SIMLT=1.0
 if NNN=1 then SIMLT=0.1332
 L200:
 NW=1
 NARTI=1
 J3=0
 MT=1
 K2=0
 K4=0
 if NC-3<>0 then goto L250
 L220: if LL1-10<>0 then goto L260
 L230:'LL3=18:
 QUERY (18)
 L260:
Print Using " MINS     ";
For I=1 to 8
    Print using "(###)   ";NE(I);
Next I
Print
 
 L250:
 INDEX=1
 NREPT=1
 X=T(81)
 For I=1 to 120
     T(I)=REFER(I)
 Next I
 For I=1 to 10
     TDLAY(I)=T(31)
     TDLAY(I+10)=T(61)
     TDLAY(I+20)=T(86)
     TDLAY(I+30)=T(97)
 Next I
 T(13)=T(13)*SIMLT
 if LL1 <10 then goto L240
 'Print "MINIT: before DUMP: LL1: ";LL1:sleep:getkey
 'DUMP (LL1)
 LL1=1
 L240:
 T(81)=X
 T(77)=-X*60.0
 LL4=-1
 'Print "End MINIT"
 VART
 KZAHL=1
 Return
End Sub

'******************************************************************************
' ******* MacPuf Main Program *********
'INI=input, KT= output device (terminal), KL= output plotter/graph
 Dim as Integer FF 'I,KK,KD,KR,KH,KN,IOK(100),K,FF
    Dim as String NOMEN,EXT,FNAME,PFAD
INI=2
KT=1
KL=1
'******************************************************************************
'Examine previous sessions using Biopac AcqKnowledge
'Print "               *********************************************"
'Print "               * Do you want to examine previous sessions? *"
'Print "               *********************************************"
'Input ANSW
'If Not(ANSW="y" or ANSW="j" or ANSW = "1" or ANSW="J" or ANSW="Y") then goto L5
'Run "C:\Program Files (x86)\BIOPAC Systems Inc\AcqKnowledge 3.7.3\Acq37.exe",
'Sleep
'GetKey
'Initialize all indices for standard output, etc and print opening
'L5:
For I=1 to 23
    NTAB(I)=NO(I)
Next
Print " -- MacPuf -- version 76.4, revitalized June 2015 -- "
'sleep
'GetKey
' Initialize all working values for a new subject with S/R MINIT
'NREPT=18
L170:MINIT (LL1, LL3, LL4, NREPT)
'Print " ";Chr(0C);
Print "               *********************************************"
Print "               * Do you want to examine previous sessions? *"
Print "               *********************************************"
Print
Print "           To accept this and the next option hit y, Y, 1, J"
Print
Input ANSW
If Not(ANSW="y" or ANSW="j" or ANSW = "1" or ANSW="J" or ANSW="Y") then goto L5
Print "        Do you want to use the graphic capabilities (program Acknowledge)?"
ANSW=""
Input ANSW
If Not(ANSW="y" or ANSW="j" or ANSW = "1" or ANSW="J" or ANSW="Y") then goto L5
EXPLAINGRAPH
Chain "C:\Program Files (x86)\BIOPAC Systems Inc\AcqKnowledge 3.7.3\Acq37.exe"
Print
Print "                    **** Back from AcqKnowledge ****"
Print
'Sleep
'GetKey
L5:
DATUM=MID(Date,1,2)+Chr(95)+MID(Date,4,2)+Chr(95)+MID(Date,7,4)
ZEIT=MID(Time,1,2)+Chr(95)+MID(Time,4,2)+Chr(95)+MID(Time,7,2)
'Print DATUM,ZEIT
'sleep
'getkey

'Print "Main nach MINIT"',FIO2,AGE

'Print "Main nach MINIT: "; SIMLT
'Sleep
'GetKey
KOUNTER=0
'Compute working parameters unchanged during the run
L190: KONST(NREPT)
KOUNTER=KOUNTER+1
'Main program loop operates every FT (fractional time) minute(s)
'For I=1 to 70
 '   Print I;")";C(I),
'Next I
'Sleep
'GetKey
'CLOSE #Drucker  ' Ein CHR(12) zum Bestätigen des Druckauftrages ist nicht
                    ' notwendig. Wenn allerdings eine neue Seite begonnen werden
NREPT=NA+1
'Print "Main befor loop: "; NREPT
'sleep
'GetKey

For MORAN = 1 to NREPT
    KOUNTER=KOUNTER+1
    'if FT>=0.16666 then MAXKOUNT=80:goto L20 ' 10sec
    'if FT=0.150001 then MAXKOUNT=509'9 sec
    'if FT=0.133343 then MAXKOUNT=509'8 sec
    'if FT=0.1166677 then MAXKOUNT=509'7sec
    'if FT=0.100001 then MAXKOUNT=509'6 sec
    'if FT=0.08333433 then MAXKOUNT=509'5 sec
    'if FT=0.06667 then MAXKOUNT=
    'if FT=0.05 then MAXKOUNT=
    'if FT=0.0333 then MAXKOUNT=
    'if FT>=0.0166 then MAXKOUNT=200'1 sec
    'L20:
    'if J3<MAXKOUNT then goto L10:
    'Print:Print:Print " **** Fatal internal error. Need to end the program! ****"
    'Print "           Too many iterations of main loop "
    'Print
    'This unknown error depends on the time interval chosen, and on the chosen duration for a run.
     'With 10 sec (FT=0.16667)and 180 sec run
     
    'the KOUNTER can go up to 509 or about 83 min, with 1 sec (FT=0.01667) up 
    'to 15000 or about 230 min. This error trap allows at least 80 min of data storage.
    'Heap size problem? Use Allocate?
    'sleep
    'Input "Do you want to save the results?",SPEICHER
    'getkey
   'goto L2000
   'L10:
    'Print "Main in Loop, MORAN ";MORAN
    'if bag rebreathing in action make inspired gases as in bag
    if PL-1.5 >0 then BAGER (2,C(12),C(12),C(12))
    'increment loop counter
    L210:LL4=LL4+1
    'next automatically increases cardiac output if O2 supplied is low
    Y=RO2CT*0.056
    L220: if Y-0.35 <0 then Y=0.35
    'COADJ is adjusted cardiac output, used mainly as index FTCO
    'which takes account of cardiac output per unit time.
    'Output goes up as total O2 consumption increases.
    L240: COADJ=DAMP((C(8)/Y+C(9)/(Y^2))*C(16),COADJ,C(61))
    if CO-3.0 >=0 then goto L260
    'if heart stopped make output at once zero to stop arterial
    'composition changing - "E" is a very small number.
    L250: COADJ=E: goto L280
    'limit maximum cardiac output
    L260: if COADJ-COMAX >0 then COADJ=COMAX
    'FTCO is no. of 100 ml portions of blood circulating per fractional
    'time interval
    L280: FTCO=C(17)*COADJ
    FTCOC=FTCO*(1.0-C(69)*COADJ)
    'O2 cont.or.press. influences venous admixture and (very slowly) 2,3-DPG
    DPG=DPG+(23.3-RO2CT-DPG)*C(10)
    X=AO2PR
    if X-200.0 > 0. then X=200.0
    L300: Y=AO2PR
    if Y-600.0 > 0. then Y=600.0
    L320: if AO2PR-400.0 >0 then X=X-(Y-400.0)*0.3
    L340: if X-55.0 <= 0.0 then X=55.0
    'PW=effective venous admixture, affected by PEEP, alv. pO2, etc
    'and also incorporationg a fixed shunt component, FADM
    L360: PW=(C(18)/X+C(19))*C(21)+FADM
    ' limiting ridiculous admixtures exceeding 100
    if PW-100.0 > 0.0 then PW=100.0
    L380: X=PW*0.01
    
    PC=1.0-X
    'art. CO2 and O2 amounts incremented by mixture of pure venous
    'and pure idealized pulm. capillary blood, determined by ratios X and PC
    'Nitrogen content is determined in terms of partial pressures
    RN2MT=RN2MT+FTCO*((X*TN2PR+PC*(C(11)-AO2PR-AC2PR))*0.00127-EN2CT)
    U=X*VC2CT+PC*PC2CT
    V=X*VO2CT+PC*PO2CT
    RC2MT=RC2MT+FTCO*(U-EC2CT)
    RO2MT=RO2MT+FTCO*(V-EO2CT)
    'Print "Main: RO2MT, RC2MT, EO2CT, EC2CT: ";RO2MT,RC2MT,EO2CT,EC2CT
    'Print "VC2MT,AC2MT,AC2PR: ";VC2MT,AC2MT,AC2PR
    
    'sleep
    'getkey
    'contents passing to tissues affected by rates of blood flow
    W=C(22)/COADJ
    'if heart stopped prevent changes of arterial blood composition
    if W-100.0 >0. then W=0.0
    L400: EO2CT=DAMP(RO2MT*0.1, EO2CT,W)
    EC2CT=DAMP(RC2MT*0.1,EC2CT,W)
    EN2CT=DAMP(RN2MT*0.1,EN2CT,W)
    Z=COADJ*C(17)
    'R-2CT is content of blood reaching chemoreceptors
    RO2CT=DAMP(V,RO2CT,Z)
    'Print "Main, RO2CT nach Damp: ";RO2CT
    'sleep
    'GetKey
    RC2CT=DAMP(U,RC2CT,Z)
    'O2CON and C2CON are used every time for entering S/R GASES. Beforre
    'entering S/R GASES (dissoc. curves) set contents of O2 and CO2 for art.
    'blood, same for bicarb. (HCO3), which has to take account of in vitro 
    'influence of art. PO2 on bicarb. conc., so that pH can be calculated.
    RC3CT=C(3)*(RC2PR-40.0)+VC3MT*C(1)
    if RC3CT <=0. then goto L940
    'enter art. bicarb., calc. pH and enter value into RPH (art. pH)
    L410: RPH=PHFNC(RC3CT,RC2PR)
   'Print "Main before GSINV"
    'Print "RO2PR: ";RO2PR, "RC2PR: ";RC2PR, " pH: ";RPH
    'Print "RO2CT: ";RO2CT, "RC2CT: ";RC2CT
    'Sleep 
    'GetKey
    O2CON=RO2CT:C2CON=RC2CT:PO2S=RO2PR:PC2S=RC2PR
    'Print "RO2CT:",O2CON,"RC2CT:",C2CON
    
    'O2CON=21.5:C2CON=5.:RPH=7.2
    'Call GSINV(RO2PR,RC2PR,RO2CT,RC2CT,RPH,SAT)
    GSINV(RPH)
    RO2PR=PO2S:RC2PR=PC2S:RO2CT=O2CON:RC2CT=C2CON
   'Print "Main after GSINV"
    'Print "RO2CT:",RO2CT,"RC2CT:",RC2CT
    'Print "RO2PR:",RO2PR,"RC2PR:",RC2PR
    'Print "PCV,HB,SAT,TEMP: ",PCV,HB,SAT,TEMP
   'Sleep
   '
   'GetKey
    'store arterial saturation as percentage
    PJ=SAT*100.0
    'U is energy expenditure from "metabolic rate" specified by operator.
    '1st=O2 consumption of resp. muscles, 2nd=O2 consumpt. of heart, 3rd=
    'rest of body. In the event of anaerobic metabolism, same energy require-
    'ments involve 11x no of molecules of lactate produced with XLACT O2 spared.
    L490:U=FT*(Abs(SVENT)^C(4)*C(5)+COADJ+C(7))
    'compute new tissue gas amounts (T-2MT)
    L500: TO2MT=TO2MT+FTCOC*(EO2CT-TO2CT)-U+XLACT
    if TO2MT-E <=0.0 then goto L940
    'compute tissue pO2, damping appropriately
    L510:TO2PR=DAMP(TO2MT*C(31),TO2PR,C(55))
    X=TO2MT-250.0
    'next section concerns lactic acid metabolism
    if X > 0 then TO2PR=45.0+0.09*X
   'local variable Y will be used later for cataboism related
   'to cardiac output and metablism
   L530: Y=RLACT*C(29)
   'Z=catabolic rate for lactate
   'X is threshold - when TPH less than 7.0 catabolism impaired
   'cerebrl. blood flow (CBF)is used as described below (empirically)
   W=CBF*0.019
   if W-1.0 >0. then W=1.0
   L536:X=TPH*10.0-69.0
   if X-W>0.0 then X=W
   '1st term is hepatic removal, 2nd is renal removal with pH influence
   '3rd term is blood flow related metabolism by muscles, made
   'dependent on cardiac output (COADJ). Whole expression is 
   'multiplied by Y, a function of blood lactate concentration
   L550: Z=Y*(X*0.8612+0.0232*2.0^((8.0-TPH)*3.33)+COADJ*0.01)
   'local variable W above makes slight allowance for reduced liver blood flow,
   'which causes decreased metabolism and increased production
   'when there is a low pCO2 or alkalosis.
   'Cerebral blood flow (CBF) is used for computation since it is 
   'computed eleswehere and changes in the appropriate way.
   W=C(70)/(W+0.3)
   'FITNS is threshold for switch to anaerobic metabolism
   'related to FITNESS
   V=FITNS-TO2PR
   if V>0.0 then W=W+C(42)*(V+1.0)^4:Z=Z*TO2PR*0.04
   'catabolism (Z) falls falls is tissue pO2 too low 
   'Next statement (Z=.. above) provides virtual trigger effect and rapidly
   'increases lactic acid production if tissue pO2 is low
   'XLACT is O2 sparing effect of lactic acid production 
   '(not allowed to exceed actual O2 consumption)
   L570: X=2.04*(W-C(32))
   if X-U>0 then X=U
   L590: XLACT=DAMP(X,XLACT,C(53))
   'limit of rate of lactate formation determined by metaboic drive
   'to tissues (i.e. level of exercise), and to body size
   if W-C(40)>0.0 then W=C(40)
   'reduce lactate catabolism if cardiac output less than 1/3 normal
   'to take account of probably diminished liver and kidney blood flows
   L610: X=C(24)-COADJ
   if X>0.0 then Z=Z*COADJ/C(24)
   'increase total lactic acid by difference between production and
   'catabolism
   L630: V=W-Z
   TLAMT=TLAMT+V
   RLACT=DAMP(TLAMT*C(15),RLACT,COADJ*C(55))
   'next is for nitrogen stores in tissues
   'move N2 between fast (T) and slow (S) tissue compartments
   'according to partial pressre differences
   L640: X=(TN2PR-SN2PR)*C(60)
   TN2MT=TN2MT+FTCOC*(EN2CT-TN2PR*0.00127)-X
   SN2MT=SN2MT+X
   'test if slow space supersaturated
   Y=(SN2MT*C(26)-C(11))*C(27)
   'if so, augment U and decremet S, or vice versa if
   'if ambinet pressure relativley higher
   if Y<=0.0 then Y=Y*0.03:if UN2MT<=0.0 then goto L670
   L660: SN2MT=SN2MT-Y
   'BUBBL is arbitrary index of bubble symptoms, taking into account
   'BTPS volume and loading by number of molecules of gas
   '*** n.b. 'BUBBL' is only a rough index - program under development
   BUBBL=UN2MT^1.2*C(23)
   UN2MT=UN2MT+Y
   'compute partial pressures
   L670: SN2PR=SN2MT*C(26)
   TN2PR=TN2MT*C(28)
   'Tissue CO2 exchanges;U=metabolism;0.001 converts cc to litres
   TC2MT=TC2MT+(FTCOC*(EC2CT-TC2CT)+TRQ*U)*0.001
   'Print "TC2MT, TRQ, U: ";TC2MT,TRQ,U
   'compute partial pressure from total CO2 and standard bicarbonate
   TC2PR=(TC2MT*C(30)-TC3MT*C(36)+C(33))*C(43)'Hurra! Fehler gefunden (13.Okt.2015)
   'FY stores change in tissue CO2 for adjusting tissue/HCO3 buffers
   FY=TC2PR-TC2RF
   TC2RF=TC2PR
   '0.4 in line below represents buffers of lactic acid partly inside
   'cells, so that displacement of bicarbonate is less then
   'strict molar equivalence
   TC3MT=TC3MT+FTCOC*0.1*(VC3MT*C(1)-TC3MT*C(13))-0.4*V
   Y=(TC2PR-40.0)*C(3)
   L690: TC3CT=TC3MT*C(13)+Y
   if TC3CT<=0.0 then goto L940
   L700: TPH=PHFNC(TC3CT,TC2PR)
   PO2S=TO2PR:PC2S=TC2PR
   'Print "Tissue: ";
   GASES (TPH)
   TO2CT=O2CON:TC2CT=C2CON
   'amounts of gases in venous pool increased by arriving blood from
   'tissues and decremented by blood going to lungs. Same for bicarbonate.
   'Contents V-2CT then determined
   X=C(69)*COADJ*FTCO
   VC2MT=VC2MT+FTCOC*TC2CT-FTCO*(VC2CT*C(14)-RC2CT*SHUNT)+X*EC2CT
   VO2MT=VO2MT+FTCOC*TO2CT-FTCO*(VO2CT*C(14)-RO2CT*SHUNT)+X*EO2CT
   VC3MT=VC3MT+FTCOC*0.1*(TC3MT*C(13)-VC3MT*C(1))+ADDC3
   'Print "VC3MT,TC3MT,C(13),C(1),ADDC3: ";VC3MT,TC3MT,C(13),C(1),ADDC3
   X=TC3AJ*C(10)
   TC3AJ=TC3AJ-ADDC3+V-X
   TC3MT=TC3MT-FY*C(64)+X*0.67
   'Print "TC3MT,FY,TC2PR,C(64),X: ";TC3MT,FY,TC2PR,C(64),X
   VO2CT=VO2MT*C(2)
   VC2CT=VC2MT*C(2)
   '** Subroutine DELAY can be omitted with loss of accuracy only when
   '** using short iteration intervals
   DELAY
   VC3CT=VC3MT*C(1)+Y
   if VC3CT<=0.0 then goto L940
   L710: VPH=PHFNC(VC3CT,TC2PR):Goto L740
   'This section-rarely needed-is for diffusion respiration
   L720:
   'Z=100.0-FIO2-FIC2'This line is missing in MAIN! See p 63 in Book, and below.
   'But this is OK because L720 is jumped to at L800, after Z has been definde.
   FD=XVENT*C(12)
   AO2MT=AO2MT-FD*FIO2
   AC2MT=AC2MT-FD*FIC2
   'Print "L 720: AC2MT: ";AC2MT
   AN2MT=AN2MT-FD*Z
   goto L850
   'prevent later division errors if gas exchange zero
   L730:
   QA=0.001
   QB=E
   goto L840
   'Next long section concerns gas exchange in lungs. PC is fraction
   'of cardiac output perfectly mixed with alveolar gases. U and V = 
   'amounts of gas taken in per unit fractual time (FT).
   L740:
   'Print "PC: ";PC
   PC=FTCO*C(14)*PC
   X=AVENT*C(12)
   U=X*FIO2
   V=X*FIC2
   'Next three statements compute new amounts of of each gas in lungs.
   'W is volume at end of nominal 'inspiration'.
   AO2MT=AO2MT+U
   AC2MT=AC2MT+V
   'Print 
   'Print "L740: AC2MT: ";AC2MT
   'Print "PC, FTCO, C(14),AVENT, C(12),V: ";PC,FTCO,C(14),AVENT, C(12),V
   Z=100.0-FIO2-FIC2
   AN2MT=AN2MT+AVENT*C(12)*Z
   W=AO2MT+AC2MT+AN2MT
   'now calculate alveolar partial pressures
   X=C(11)/W
   PO2=AO2MT*X
   PC2=AC2MT*X
   'change alveolar gas amounts in accordance with blood gas contents
   'entering (V-2CT)and leaving (P-2CT) the lungs.
   'PC = final new amount of total gas at end of all.
   AO2MT=AO2MT+PC*(VO2CT-PO2CT)
   AC2MT=AC2MT+PC*(VC2CT-PC2CT)
   'Print "L740: AC2MT: ";AC2MT
   'Print "VC2CT, PC2CT: ";VC2CT, PC2CT
   AN2MT=AN2MT+PC*(TN2PR*0.00127-EN2CT)
   PC=AO2MT+AC2MT+AN2MT
   'Print "PC gas Volume: ";PC
   'FY becomes positive only if more gas goes out than in, in which case
   'FY is later brought into the calculation of effective dead space
   If (PL-2.0)=0.0 then goto L770
   if (AVENT-20.0)<0.0 then goto L770
   L760: FY=(PC-W)*C(34)/RRATE
   goto L780
   L770:FY=0.0
   L780:if PL<0.0 then BAGER(5,PC,X,C(12))
   'XVENT is volume exhaled in nom. time (FT) down to resting lung volume.
   'If this is negative there is some diffusion respiration in which case go 
   'back.
   L800:
   XVENT=PC*C(35)-VLUNG
   if XVENT <0.0 then goto L720
   L810: DVENT=XVENT*C(25)/PC
   'U=O2, V=CO2 uptake in FT time; Y,Z,PC = gas outputs
   Y=DVENT*AO2MT
   Z=DVENT*AC2MT
   PC=DVENT*AN2MT
   'algebraic summing of intake and output of O2 and CO2
   QA=U-Y
   QB=Z-V
   if PL-0.5>0.0 then BAGER(4,XVENT,DVENT,C(12))
   if (DVENT+9000.0)<0.0 then goto L940
   L830:if (AVENT-E)<=0.0 then goto L730
   'set new amounts of each gas,then calculate partial pressures
   L840: 
   AO2MT=AO2MT-Y
   AC2MT=AC2MT-Z
   'Print "L840: AC2MT: ";AC2MT
   AN2MT=AN2MT-PC
   L850: U=C(11)/(AO2MT+AC2MT+AN2MT)
   'take account of insp./exsp. ratio
   V=C(37)/RRATE
   if V-4.0 >0.0 then V=4.0
   L870:if AVENT-20.0<0.0 then V=0.0
   'speed of change of alveolar gas tensions (X) = function of tidal volume
   L890:X=(TIDVL+100.0)*C(38)
   'compute 'end-exspiratory' partial pressures
   Y=AO2MT*U
   Z=AC2MT*U
   'damp function used to prevent oscillatory swings of alveolary gas pressures
   AO2PR=DAMP((Y+(PO2-Y)*V),AO2PR,X)
   'Print "DAMP AO2PR, PO2: ";AO2PR,PO2
   'sleep
   'getkey
   AC2PR=DAMP((Z+(PC2-Z)*V),AC2PR,X)
   if AO2PR-E<=0.0 then goto L940
   L900: if AC2PR-E<=0.0 then goto L940
   'determine exspired RQ (PC) then alv. gas tensions, and eventually
   'contents of CO2 and O2 in pulmon. capill. blood (P-CT)
   L910:if QA=0.0 then goto L930
   L920:PC=QB/QA
   L930:X=VC3MT*C(1)+C(3)*(AC2PR-40.0)
   'Print "VC3MT,C(1),C(3),AC2PR,E: ";VC3MT,C(1),C(3),AC2PR,E
   if X-E>=0.0 then goto L950
   L940:LL5=-1:goto L1455
   L950:Y=PHFNC(X,AC2PR)
   PO2S=AO2PR:PC2S=AC2PR
   'Print "Alveoli: ";
   GASES(Y)
   PO2CT=O2CON:PC2CT=C2CON
   'next determines cerebral blood fow adjustments in relation
   'to cardiac output and brain pH (PCO2 sensitive)
   Z=SQR(COADJ)*0.5
   if Z-1.0 >0.0 then Z=1.0
   L970:Y=(7.4-BPH)*(BC2PR*0.0184-BC3AJ*0.1)
   if Y>0.0 then Y=300.0*Y^2.0
   L990: if Y-4.4>0 then Y=4.4
   L1010: CBF=DAMP((Y-0.12)*42.5*Z,CBF*Z,C(55))
   'compute brain gas amounts by metabolism assuming RQ of .98 and
   'allowing for different amounts supplied in art. blood and leaving in venous
   'blood. Check for arithmetric errors
   'then calculate brain gas tensions from guestimated dissoc. curves
   Y=CBF*C(39)
   X=C(41)*(BO2AD+0.25)
   Z=X
   if BO2PR-18.0 >0.0 then goto L1040
   L1020: Z=X*(BO2PR*0.11-1.0)
   X=X*(19.0-BO2PR)
   if Z<0.0 then Z=0.0
   L1040:
   BO2MT=BO2MT+Y*(RO2CT-BO2CT)-2.0*Z*(BO2AD+0.1)
   if BO2MT<=0.0 then BO2MT=0.1
   L1060: 
   BC2MT=BC2MT+Y*(RC2CT-BC2CT)+2.15*X
   BO2PR=BO2MT*1.6
   BC2PR=BC2MT*0.078
   W=BC2PR-40.0
   Y=BC3CT+BC3AJ+0.2*W
   'A small proportion of added bicarbonate is added also to CSF,
   'thus affecting breathing appropriately
   BC3AJ=BC3AJ+((RC3CT-24.0)*0.3-BC3AJ)*C(42)
   'adjust  bicarb. to pCO2 then calculate 'brain pH', i.e. pH at
   'receptor. Then proceed to determine contents of O2  and CO2 in blood
   'leaving the brain
   X=PHFNC(2.0*Y+RC3CT,2.0*BC2PR+RC2PR)
   Z=((ABS(X-BPH)+E)*100.0)^2+0.04
   if Z-C(17)>0.0 then Z=C(17)
   L1080:
   BPH=DAMP(X,BPH,Z)
   'restrict rate of change of brain receptor pH
   Z=PHFNC(VC3MT*C(1)+(BC2PR-40.0)*C(3),BC2PR)
   PO2S=BO2PR:PC2S=BC2PR
   'Print "Brain: ";
   GASES(Z)
   BO2CT=O2CON:BC2CT=C2CON
   'Now follow ventilation calculations, starting with NARTI=0 which
   'is artificial ventilation. Compute total dead space, anatomical and
   'physiological, then alveolar ventilation (AVENT)
   if NARTI <=0.0 then DVENT=C(51)
   'natural ventilation controls. Total ventilation ((U and SVENT)=sum of
   'central CO2 (pH) chemoreceptor drive (slope X, intercept Z) O2 lack
   'receptor  (slope Y)central neurogenic drive  (proportnl. to 
   'O2 consumption C(7), + constant etc.). AZ etc. are for manual adjustments
   'of ventilatory controls
   L1100:
   Y=(118.0-PJ)*0.05
   Z=Y*0.002
   X=(C(65)+Z-BPH)*1000.0*Y
   if X<0.0 then x=0.0
   L1120:
   W=(C(66)+Z-BPH)*150.0*Y
   'high brain pH or low pCO2 only inhibits ventilation if learnt intrinsic
   'drive (CZ) is reduced or absent
   if W>=0.0 then goto L1150
   L1130: if C(67)>0.0 then W=0.0
   L1150:
   Z=(BC2PR-120.0)*0.25
   if Z<0.0 then Z=0.0
   L1170:
   Y=(98.0-PJ)*(RC2PR-25.0)*0.12
   if Y<0.0 then Y=0.0
   'BO2AD is is index of brain oxygenation adequacy and lowers 
   'and ventually stops breathing if too low
   L1190:
   U=BO2PR-11.0
   if U <0.0 then goto L1210
   L1200: U=1.0
   goto L1220
   L1210: U=0.0
   L1220:
   BO2AD=DAMP(U,BO2AD,C(63))
   'prevent immediate changes in specified ventilation capacity
   XRESP=DAMP(C(46),XRESP,C(68))
   'compute total additive effect of ventilation stimuli
   U=(C(44)*(X+W)+C(45)*Y+XRESP-Z)*C(47)
   'restrict to maximal value, predicted or assumed
   if U-C(6)>0.0 then U=C(6)
   'damp speed of response according to cardiac output and depth
   'of breathing, including brain oxygenation index
   L1240:
   X=(COADJ+5.0)*C(62)/(TIDVL+400.0)
   SVENT=BO2AD*DAMP(U,SVENT,X)
   if PL<0.0 then goto L1280
   'if vent. stimuli inadequate, then apnoea
   L1250:if SVENT-C(48)>0.0 then goto L1290
   if NARTI <=0.0 then goto L1290
   L1280:
   DVENT=E
   RRATE=0.0001
   goto L1350
   'respiratory rate calculated from constant+elastance+total ventn.+
   'art. pO2 allowing for manual adjustment of breathing CAP (PR)
   L1290:
   if NARTI<=0.0 then goto L1320
   L1300: DVENT=SVENT
   RRATE=(C(49)+DVENT^0.7*0.37)*C(50)/(PJ+40.0)
   if RRATE-1.0 <=0.0 then goto L1350
   if COADJ-0.5<=0.0 then goto L1350
   'calculate dynamic dead space as sum of anatomical factors and
   'those dependent on ventilation anbd perfusion, then calculate alveolar
   'ventilation
   L1320:
   U=AO2PR*0.15
   if U-70.0 >0.0 then U=70.0
   L1340:
   DSPAC=DAMP((C(52)+DVENT*100.0/RRATE^1.12+20.0*DVENT/(COADJ+5.0)+U+FY+C(54)*(TIDVL+500.0)),DSPAC,C(55))
   L1350:
   TIDVL=DVENT*1000.0/RRATE
   L1360: AVENT=(TIDVL-DSPAC)*RRATE*FT
   'restrict tidal volume to maximum (C(20)), then if necessary go back to
   'recompute respiratory rate, providing artific. vent. not in use
   if NARTI <=0.0 then goto L1380
   X=TIDVL-C(20)
   if X<=0.0 then goto L1380
   L1370: TIDVL=C(20)
   RRATE=DVENT*1000.0/TIDVL
   goto L1360
   L1380:
   if AVENT<=0.0 then AVENT=E
   FVENT=AVENT*C(56)
   ' TND, J3, ND concerned with timing marks. PG is index of time brain has
   'been deprived of oxygen. If too great, death results
   TND=TND+C(57)
   if BO2AD-0.3 >0.0 then goto L1420
   PG=PG-(BO2AD-1.0)*C(58)
   goto L1430
   L1420: PG=PG-BO2AD*C(59)
   L1430:
   if TND-60.0 <0.0 then goto L1450
   L1440: 
   TND=TND-60.0
   J3=J3+1
   L1450:ND=int(TND)
   'test for death, or give error message if LL5=-1
   L1455:DEATH (LL1, LL2, LL5)
   'Print "Main after DEATH: LL1 ";LL1
   if LL1-2>=0 then goto L1600'Jumps out of MORAN loop
   'output suppression instructions
   if NB-1<0.0 then goto L1470
   if NB-1=0.0 then goto L1590
   L1460: if MORAN-NREPT<>0.0 then goto L1590
   goto L1500
   L1470:
   if ISPAR-2<0 then goto L1500
   if ISPAR-2>0 then goto L1490
   L1480:if LL4-6<>0 then goto L1590
   goto L1500
   L1490:if LL4-30<>0 then goto L1590
   'reset loop counter
   L1500:LL4=0
   'choose type of display
   if NC-3 >=0 then goto L1515
   L1510: BRETH
   'FEHL=Err
   'On Error goto L2000
   '
   'Print "Kounter*FT: ";KOUNTER,FT,J3
   goto L1590
   'convert all pressures in mmHg to SI units if previously specified
   'before output
   L1515:UNITS(1)
   'print "SIMLT: ";SIMLT:sleep:getkey
   L1520:if NC-3>0 then print T(1):goto L1580
   L1540:
   'DIM AS INTEGER Drucker = FREEFILE
    'Dim as Integer FF 'I,KK,KD,KR,KH,KN,IOK(100),K,FF
    'Dim as String NOMEN,EXT,FNAME,PFAD
    PFAD="C:\"
    EXT=".txt"
    FNAME=PFAD+"MacPufSess\"+"T "+DATUM+CHR(95)+ZEIT+EXT
    FF=FreeFile
    Close #FF
    Open FNAME for APPEND as #FF
    If Err >0 then Print "Opening file failed 1, Err: ";Err:Sleep:GetKey
   'Print #FF, RO2PR;Chr(09);PJ;Chr(09);RC2PR;Chr(09);RPH;Chr(09);RC3CT;Chr(09);RRATE;_
    'Chr(09);DVENT;Chr(09);COADJ;Chr(09);RLACT;Chr(09);ND

  Print Using "###:##";J3;ND;
  For J=1 to 8'NL?
       JOKE=NE(J)
       TJJ(J)=T(JOKE)
       Print Using " #####.##";TJJ(J);
       'Print #FF, TJJ(J);Chr(09);
   Next J
   Print
   For J=1 to 8
       Print #FF, TJJ(J);Chr(09);
    Next J
    Print #FF, ND
   Close #FF
   'Print "Hello"
   'Print J3, ND
   'convert all pressure units back to mmHg before going on
   L1580:
   UNITS(2)
   L1590:'TVAR
    'Print "Min, sec: ";J3, ND
    'sleep
'GetKey
Next MORAN
TVAR
'reset bicarbonate addition index to zero
ADDC3=0.0
'enter subroutine DEADY to allow manual changes of variables
L1600:UNITS (1)
'UNITS (2)
DEADY (LL1, LL5,NREPT)
'reset units to mmHg
UNITS(2)
NREPT=NA
'Print "Main; End: ";NA

L1610:
'Print "Main L1610:NREPT, LL1: ",NREPT,LL1
if LL1-1>0 then goto L170
goto L190
L2000:
end
'-----------------------------Subroutines----------------------------------

'   
   
   
   
   
   
    
    
    
    
    

