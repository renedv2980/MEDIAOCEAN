*PHASE SCHOJPTR                                                                 
         TITLE 'DEMO CONVERSION'                                                
SCHOJPTR TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                    00256*77
         EJECT                                                          00278   
SCHOJPTR CSECT                                                          00279*77
         PRINT NOGEN                                                    00280   
         NMOD1 0,SCHOJPTR,RA,RR=RE                                      00281*77
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE  00282**2
         LH    R4,=Y(WORKD-SCHOJPTR)                                    00283*77
         LA    R4,SCHOJPTR(R4)                                          00284*77
         USING WORKD,R4                                                 00285**2
*                                                                       00286**2
         SPACE 1                                                        00287   
         ST    RE,RELO                                                  00288   
         L     RC,ARREC                                                 00289   
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD   00290   
         SPACE 1                                                        00291   
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT   00292   
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES         00293   
         SPACE 1                                                        00294   
         B     *+4(R1)             ROUTINE HOOK                         00295   
         SPACE 1                                                        00296   
         B     READ                PROCESS INPUT TAPE                   00297   
         B     CNVWR               SORT RECORD HOOK                     00298   
         B     ENDJOB              E-O-F ON INPUT                       00299   
         B     EXIT                                                     00300   
         EJECT                                                          00301   
READ     DS    0H                                                       00302   
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE          00303   
         BE    READ20                                                   00304   
         MVI   NOTAVAL,0                                                00305   
         L     RE,=A(PRGAVG)                                            00306   
         ST    RE,ACOMWRK          PASS ON TO OUTPUT PHASE              00307   
         MVI   BYPREAD,X'FF'       SET TO 1ST-TIME-THRU                 00309   
         CLI   RELOFRST,1          TEST FOR RELOCATED DTF ADDRESS       00311   
         BNE   READ20                                                   00312   
         MVI   RELOFRST,0                                               00316   
         SPACE 1                                                        00317   
READ20   DS    0H                                                       00318   
         L     R3,ARREC                                                 00319   
         XC    0(4,R3),0(R3)       INSERT VARIABLE LENGTH RECORD        00320   
         MVC   0(2,R3),=H'400'     HEADER                               00321*67
         LA    R3,4(R3)                                                 00322   
         BAL   RE,JPTRS                                                 00332*67
*                                                                               
READ100  MVI   BYPREAD,0                                                00391*82
         MVI   INTAPESW,X'40'      DROP RECORD                          00392   
         B     ENDJOB                                                   00393   
         EJECT                                                          00394   
*********************************************************************   00395   
*READ HARDCODED KEYS FROM TABLE AND OUTPUT TO FILE.                     00396   
*********************************************************************   00397   
JPTRS    NTR1                                                           00398   
*                                                                               
         LA    R5,THISKEY                                                       
         USING PJKEY,R5                                                         
         LA    R3,KEYTAB           DDS PROGRAM NUMBERS TO SET                   
LPTOP    DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    JPX                                                              
         XC    THISKEY,THISKEY                                                  
         MVC   THISKEY(18),0(R3)   HARD CODED KEYTABLE                          
*                                                                               
         GOTO1 ABLDREC,DMCB,(C'P',THISKEY)                                      
         GOTO1 APUTTAPE,0                                                       
*                                                                               
         LA    R3,L'KEYTAB(R3)                                                  
         B     LPTOP                                                            
*                                                                       00403*67
JPX      XIT1                                                                   
***************************************************                             
*NUMTAB - TABLE OF PROGRAM NUMBERS TO SET IN MAP                        00403*67
***************************************************                             
KEYTAB   DS    0CL18                                                            
*        DC    X'D1D5D5D7D7D7D7E2000000000007005909C5'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000008177609C6'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000008225809C7'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000008230909C8'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000009311509C9'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000009868009CA'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010255409CB'                          
         DC    X'D1D5D5D7D7D7D7E2000000000010255509C5'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010308609CD'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010308809CE'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010309009CF'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010309309D0'                          
*        DC    X'D1D5D5D7D7D7D7E2000000000010309509D1'                          
         DC    X'D1D5D5D7D7D7D7E2000000000010319909C6'                          
         DC    X'D1D5D5D7D7D7D7E2000000000010320309C7'                          
         DC    X'D1D5D5D7D7D7D7E2000000000010320509C8'                          
         DC    X'D1D5D5D7D7D7D7E2000000000010320609C9'                          
         DC    X'FF'                                                            
**********************************************************************  01394**6
*CNVWR - N/A                                                            00403*67
**********************************************************************  01394**6
CNVWR    B     EXIT                                                     01207**3
         EJECT                                                          01208**3
**********************************************************************  01394**6
* EOF ON INPUT FILE                                                     01395   
**********************************************************************  01396**6
ENDJOB   DS    0H                                                       01399   
         MVI   INTAPESW,X'02'                                           01400   
                                                                        01402**6
EXIT     XMOD1 1                                                        01405**6
                                                                                
         GETEL R6,DATADISP,ELCODE                                       01406   
         DROP  R2,R4,R8,RA,RB                                           01407*71
                                                                                
*********************************************************************** 02065*38
* LITERAL POOL - SURB2 NMOD                                             02066*38
*********************************************************************** 02067*38
         LTORG                                                          02068**6
         EJECT                                                          02069**6
                                                                        02070**3
*********************************************************************** 02071**3
* WORKING STORAGE                                                       02072**3
*********************************************************************** 02073**3
                                                                        02074**3
WORKD    DS    0F                                                       02077**3
RELO     DS    A                                                        02078**3
INTABADR DS    A                   INTAB TABLE ADDRESS                  02079**3
DATADISP DS    H                                                        02080**3
ELCODE   DS    X                                                        02081**3
PACK16   DS    PL16                                                     02082**3
ASLOT    DS    A                                                        02083**3
GOSUBN   DS    X                                                        02084*38
SVHHUNIV DS    F                   HOUSEHOLD UNIVERSE FROM H1 RECORD    02085**3
HPTPARA  DS    6F                                                       02086**3
TAPEMKT  DS    H                                                        02087*75
DDSNUM   DS    XL2                                                              
THISKEY  DS    XL20                                                             
*                                                                       02088**3
RELOFRST DC    X'01'                                                    02089**3
BYPREAD  DC    X'00'                                                    02090**3
BYPASSH4 DC    X'00'               BYPASS ALL BUT 1ST H4 RECORD         02091**3
BYPASS01 DC    X'00'               BYPASS SINGLE DAY AVERAGES           02092**3
CORRSW   DC    X'00'               CORRRECTION RECORD SWITCH            02093**3
HUTSW    DC    X'00'               HUT RECORD GENERATION SWITCH         02094**3
MTWTFSW  DC    X'00'               HUT RECORD (M-T-W-T-F USED)          02095**3
PREVSTIM DC    X'FF'               HUT RECORD PREVIOUS START QTR HR     02096**3
*                                                                       02097**3
GAASW    DC    C'N'                GROSS AVG AUD SWITCH                 02098**3
HAVPP    DC    X'00'               HAVE PROGRAM PUTS                    02099**3
QHSFRST  DS    C                                                        02100**3
QHRSW    DS    C                                                        02101**3
BYTE     DS    C                                                        02102**3
SWITCH   DS    C                                                        02103**3
CNVWRSW  DS    C                                                        02104**3
VARIOUS  DS    X                   1=VARIOUS                            02105**3
VARS     DC    CL7'0000000'        USED FOR VARIOUS (AVERAGES)          02106**3
DAYS     DC    CL7'0000000'        USED FOR VARIOUS (INDIVIDUAL DAYS)   02107**3
*                                                                       02108**3
SVDA     DS    F                                                        02109**3
SVKEY    DC    XL24'00'                                                 02110**3
PAVKEY   DC    XL24'00'                                                 02111**3
*                                                                       02112**3
BLANKS   DC    80C' '                                                   02113**3
ZEROS    DC    256C'0'             ZEROS - USE FOR A VARIETY OF THINGS  02114**3
*                                                                       02115**3
SAVETIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR     02116**3
VARSTIME DS    7XL8                SAVE D4EVSHR, D4EVSMIN AND D4DUR     02117**3
*                                                                       02118**3
SAVVAR   DS    7XL7                SAVE VAR INFO FROM INDIVIDUAL DAYS   02119**3
*                                                                       02120**3
                                                                        02121*34
*-------------------------------------------------------------------    02122*34
*DBUFF - CLEARED WHEN D-RECD W/TOT DUR PRESENT.                         02123*34
*        HOLDS START TIME OF PRGM ON DAY, TOTAL DUR, UP TO              02124*34
*        12- HLF HOURS (QHR CODE) WITH DURATION RAN IN THAT HLF HR      02125*34
*-------------------------------------------------------------------    02126*34
DBUFF    DS    0C                  7-DAYS, 12-HLFHRS W/1 BYTE DUR BKTS  02127*34
DBUFMON  DS    CL4                 START HOUR/MIN                       02128*34
         DS    CL1                 TOTAL DUR FOR DAY                    02129*34
         DS    12CL2               12 BKTS: HLFHR CODE/DUR IN HLFHR     02130*34
DBUFDAYQ EQU   *-DBUFMON           DISP TO NEXT DAY IN BUFFER           02131*34
         DS    6CL(DBUFDAYQ)       6 MORE DAYS                          02132*34
DBUFFQ   EQU   *-DBUFF             L'DBUFFER                            02133*34
*                                                                       02134*34
DBDAYS   DS    CL7                 DAYS ACTIVE IN DBUFF                 02135*34
*-------------------             PROCESS H-RECS USING DBUFF INFO        02136*34
HLFDAYS  DS    CL7                 DAYS THIS 1/2HR WAS ACTIVE           02137*34
HLFADUR  DS    X                   AVERAGE 1/2HR DUR                    02138*34
HTOTDUR  DS    F                   TOTAL DUR FOR 1/2HR ACROSS DAYS      02139*34
STQHR    DS    X                                                        02140*34
*-------------------------------------------------------------------    02141*34
*                                                                       02142*34
THREE    DS    CL3                                                      02143**3
         DS    0F                                                       02144**3
CALVPH   DS    CL(VPHINT-VPHDEMS+4) CALCULATE VPH'S                     02145**3
SAVEINT  DS    CL(INTACCS-INTVALS) SAVE INTERIM RECORD                  02146**3
SAVEPNUM DS    CL10                SAVE PROGRAM NUMBER                  02147**3
*                                                                       02148**3
TIMTAB   DS    7CL3                                                     02149**3
*                                                                       02150**3
         EJECT                                                          02151**3
* REPORT DESCRIPTOR SAVE AREA (RESET FOR CORRECTION RECORDS)            02152**3
*                                                                       02153**3
BOOK1    DS    XL2                 KEY BOOK                             02154**3
IBOOK1   DS    XL2                 INTERNAL BOOK                        02155**3
*                                                                       02156**3
BOOK1S   DS    XL2                 KEY BOOK (SAVE AREA)                 02157**3
IBOOK1S  DS    XL2                 INTERNAL BOOK (SAVE AREA)            02158**3
*                                                                       02159**3
* HOUSEHOLD USAGE SAVE AREA                                             02160**3
*                                                                       02161**3
HUTSQH   DS    X                   NEXT QUARTER HOUR                    02162**3
HUTDUR   DS    X                                                        02163**3
HUTBOOK  DS    XL2                 KEY BOOK VALUE                       02164**3
HUTIBOOK DS    XL2                 INTERNAL BOOK VALUE                  02165**3
HUTSTA   DS    CL5                                                      02166**3
HUTDAYWK DS    X                                                        02167**3
*                                                                       02168**3
* PROGRAM DESCRIPTOR SAVE AREA                                          02169**3
*                                                                       02170**3
PVALS    DS    0CL87                                                    02171**3
PNTINUM  DS    CL5                 NTI PRG NUMBER PWOS                  02172**3
PNUM     DS    XL2                 PROGRAM NUMBER                       02173**3
PNET     DS    CL4                 NETWORK                              02174**3
PDPT     DS    C                   NSI DAYPART (ALWAYS ZERO)            02175**3
PDPT2    DS    CL2                 NTI DAYPART                          02176**3
PTYP     DS    CL2                 PROGRAM TYPE                         02177**3
PPREM    DS    C                   PREMIERE CODE                        02178**3
PNAME    DS    CL25                PROGRAM NAME                         02179**3
PTITLE   DS    CL32                EPISODE TITLE                        02180**3
*                                                                       02181**3
PDAY1    DS    X                   DAY CODE                             02182**3
PDAY1BIT DS    X                   DAY BITS                             02183**3
*                                                                       02184**3
PWK1STIM DS    XL2                 START TIME                           02185**3
PWK1DURM DS    X                   DURATION IN MINUTES                  02186**3
PWK1AUD  DS    XL2                                                      02187**3
PWK1STAC DS    XL2                                                      02188**3
PWK1COV  DS    XL2                                                      02189**3
PWK1DTYP DS    X                                                        02190**3
PWK1RSF  DS    X                                                        02191**3
*                                                                       02192**3
         EJECT                                                          02193**3
SVD4     DS    0CL83               SAVE D4 RCD'S POSITIONS 305-387      02194**3
*        THESE FIELDS ARE NOT REPORTED ON ALL D4 RECORDS                02195**3
S4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT          02196**3
         DS    CL5        310-314  TOT PROGRAM HEADEND COUNT            02197**3
S4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT       02198**3
         DS    CL2        317-318  CA PROGRAM COVERAGE                  02199**3
         DS    CL9        319-327  (FILLER)                             02200**3
S4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES      02201**3
S4TOTDUR DS    CL6        331-336  TOTAL DURATION                       02202**3
S4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)  02203**3
S4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)        02204**3
S4REPORT DS    CL1        349-349  REPORTABILITY IND. 0=REPORTABLE      02205**3
         DS    CL3        350-352  CA HH RTG                            02206**3
         DS    CL1        353      CA REPORTABILITY                     02207**3
S4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)      02208**3
S4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                   02209**3
         DS    CL9        365-373  CA AUD PROJ                          02210**3
         DS    CL2        374-375  CA HH SHR                            02211**3
S4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)   02212**3
S4RTG   DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)          02213*17
         DS    CL13       388-400  (FILLER)                             02214**3
*                                                                       02215**3
*        THESE FIELDS ARE NOT REPORTED ON ALL H4 RECORDS                02216**3
SVPROJ   DS    CL9         92-100  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)   02217**3
SVRTG    DS    CL3        101-103  HALF HOUR AUDIENCE RATING (XX.X)     02218**3
SVSHR    DS    CL2        114-115  HALF HOUR PROGRAM SHARE (XX)         02219**3
*                                                                       02220**3
SVRECLN  DS    F                   SAVE INTERIM RECORD'S LENGTH         02221**3
SVNOACCS DS    F                   SAVE NUMBER OF INTACCS FULLWORDS     02222**3
*                                                                       02223**3
* FILE DCB AND BUFFER AREA                                              02224**3
*                                                                       02225**3
IN1      DCB   DDNAME=IN1,                                             X02226**3
               DSORG=PS,                                               X02227**3
               RECFM=FB,                                               X02228**3
               LRECL=400,                                              X02229*67
               BLKSIZE=16800,                                          X02230*67
               MACRF=GM,                                               X02231**3
               EODAD=ENDJOB                                             02232**3
*                                                                       02233**3
*                                                                       02236**3
SUMWGHT  DS    CL2                                                      02237**3
SUMDAY   DS    C                                                        02238**3
SUMSQH   DS    C                                                        02239**3
SUMEQH   DS    C                                                        02240**3
SUMSTIM  DS    CL2                                                      02241**3
SUMETIM  DS    CL2                                                      02242**3
         DS    0F                                                       02243**3
SUMHPT   DS    CL188                                                    02244**3
SVAVGHI  DS    F                   SAVE PROGRAM HH                      02245**3
HWCAREA  DS    0CL36               SAVE HH WITH CHILD DATA              02246**3
HWCV18   DS    F                                                        02247**3
HWCV12   DS    F                                                        02248**3
HWCV06   DS    F                                                        02249**3
HWCR18   DS    F                                                        02250**3
HWCR12   DS    F                                                        02251**3
HWCR06   DS    F                                                        02252**3
HWCI18   DS    F                                                        02253**3
HWCI12   DS    F                                                        02254**3
HWCI06   DS    F                                                        02255**3
*                                                                       02256**3
*                                                                       02257**3
IPGAASW  DS    C                                                        02258**3
NOTAVAL  DS    C                   DATA NOT AVAILABLE                   02259**3
UNIVSAVE DS    50F                 SAVE AREA FOR PER. UNIVERSES         02262**3
**SVHREC   DS    2000C               SAVE HUT RECORD                    02263**3
*                                                                       02264**3
*        THE NEXT 1500 BYTES USED FOR EITHER THE SORT OR INTERIM RECORD 02265**3
*                                                                       02266**3
SVSREC   EQU   *                   SAVE SORT RECORD                     02267**3
SVIREC   DS    2000C               SAVE INTERIM RECORD                  02268**3
SVGAIMP  DS    444C                                                     02269**3
         SPACE 1                                                        02270**3
*                            NET(4),PRG#(2),DAY(1) E.G. 7C,7F,90        02271**3
PRGAVG   DS    0XL7                PROG#'S HAVING ASSOCD AVG RECD       02272**3
         DC    1000X'00'           PROG#'S HAVING ASSOCD AVG RECD       02273**3
         DC    X'FFFF'                                                  02274**3
         EJECT                                                          02275**3
******************************************                              02276*20
* TABLE OF DAY BIT SETTINGS AND DAY CODES                               02277*20
******************************************                              02278*20
                                                                        02279*20
NETDAYTB DS    0CL2                                                     02280*20
         DC    X'4010'             MON                                  02281*20
         DC    X'2020'             TUE                                  02282*20
         DC    X'1030'             WED                                  02283*20
         DC    X'0840'             THU                                  02284*20
         DC    X'0450'             FRI                                  02285*20
         DC    X'0260'             SAT                                  02286*20
         DC    X'0170'             SUN                                  02287*20
         DC    X'7C00'             M-F                                  02288*20
         DC    X'7F80'             M-SUN                                02289*20
NETDAYS  EQU   (*-NETDAYTB)/L'NETDAYTB                                  02290*20
*                                                                       02291*20
*******************************************************                 02292*22
* TABLE OF DAY CODES AND SORT VALUES FOR 'Q' RECORDS                    02293*22
*******************************************************                 02294*22
QSORTAB  DS    0XL2                                                     02295*22
         DC    X'8000'             M-S                                  02296*22
         DC    X'0001'             M-F                                  02297*22
         DC    X'9002'             VAR                                  02298*22
         DC    X'1003'             MON                                  02299*22
         DC    X'2004'             TUE                                  02300*22
         DC    X'3005'             WED                                  02301*22
         DC    X'4006'             THU                                  02302*22
         DC    X'5007'             FRI                                  02303*22
         DC    X'6008'             SAT                                  02304*22
         DC    X'7009'             SUN                                  02305*22
QSORTABS EQU   (*-QSORTAB)/L'QSORTAB                                    02306*22
*                                                                       02307*22
         SPACE 1                                                        02308*22
* TABLE OF TELECAST TYPES AND THEIR BIT SETTINGS USED FOR INTDTYP       02309*22
*                                                                       02310*22
TYPTAB   DS    0CL3                                                     02311*22
         DC    C'  ',X'09'         REGULAR - ORIGINAL (PROTECT)         02312*22
         DC    C'0 ',X'09'         REGULAR - ORIGINAL                   02313*22
         DC    C' Y',X'11'         REGULAR - REPEAT   (PROTECT)         02314*22
         DC    C'0Y',X'11'         REGULAR - REPEAT                     02315*22
         DC    C'1 ',X'0C'         SPECIAL - ORIGINAL                   02316*22
         DC    C'1Y',X'14'         SPECIAL - REPEAT                     02317*22
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                      02318*22
*                                                                       02319*22
         EJECT                                                          02320*22
*********************************************************************   02321*60
* TABLE OF NETWORK CODES AND THEIR CALL LETTERS                         02322*22
*********************************************************************   02323*60
*                                                                       02324*22
NETTAB   DS    0CL9                                                     02325*22
*      ******************************************                       02326*60
*              NETWORKS                                                 02327*60
*      ******************************************                       02328*60
         DC    C'ABC ',CL5'ABC T'                                       02329*22
         DC    C'CBS ',CL5'CBS T'                                       02330*22
         DC    C'FOX ',CL5'FOX T'                                       02331*22
         DC    C'NBC ',CL5'NBC T'                                       02332*22
         DC    C'PAR ',CL5'PAR T'      PARAMOUNT NTI                    02333*22
         DC    C'PAX ',CL5'PAX T'                                       02334*52
         DC    C'PX  ',CL5'PX  T'                                       02335*52
         DC    C'WB  ',CL5'WB  T'      WARNER BROTHERS NTI              02336*22
*      ******************************************                       02337*60
*      AS OF 7/99 NEW DEFINITIONS FOR AGGREGATES:                       02338*60
*      ******************************************                       02339*60
         DC    C'AGG',AL1(03),CL5'PBS T'  PBS                           02340*61
         DC    C'AGG',AL1(04),CL5'PPY T'  <= NEW DEFN HAS EXCLUSIONS    02341*61
         DC    C'AGG',AL1(07),CL5'XIN T'  IND W/O FOX,SUPS,TBS          02342*61
         DC    C'AGG',AL1(08),CL5'XSU T'  SPST:WGN,XWGN,WPIX,KTLA,WSBK  02343*61
         DC    C'AGG',AL1(10),CL5'AGA T'  ABC AFFILIATES                02344*61
         DC    C'AGG',AL1(11),CL5'AGB T'  CBS AFFILIATES                02345*61
         DC    C'AGG',AL1(12),CL5'AGC T'  NBC AFFILIATES                02346*61
         DC    C'AGG',AL1(21),CL5'FAF T'  FOX AFFIL INCL BCAST & CABLE  02347*62
         DC    C'AGG',AL1(24),CL5'ONA T'  OTHER NETWORK AFFILIATES      02348*61
         DC    C'AGG',AL1(26),CL5'TNA T'  TOTAL NETWORK AFFILIATES      02349*61
         DC    C'AGG',AL1(27),CL5'IBR T'  INDEP BROADCAST               02350*61
         DC    C'AGG',AL1(28),CL5'ADC T'   AD SUPPORTED CABLE ORIG      02351*61
         DC    C'AGG',AL1(29),CL5'NAC T'   NON-AD SUPORTED CABLE NETS   02352*61
*                                                                       02353*59
*OLD FILES:                                                             02354*65
*&&DO                                                                   02355*66
         DC    C'AGG',X'01',CL5'IND T'                                  02356*65
         DC    C'AGG',X'02',CL5'SUP T'                                  02357*65
         DC    C'AGG',X'03',CL5'PBS T'                                  02358*65
         DC    C'AGG',X'04',CL5'PAY T'                                  02359*65
         DC    C'AGG',X'05',CL5'CAB T'                                  02360*65
         DC    C'AGG',X'06',CL5'FAF T'  EFF. 01/91                      02361*65
         DC    C'AGG',X'07',CL5'XIN T'                                  02362*65
         DC    C'AGG',X'08',CL5'XSU T'                                  02363*65
         DC    C'AGG',X'09',CL5'CAT T'                                  02364*65
         DC    C'AGG',X'0A',CL5'AGA T'                                  02365*65
         DC    C'AGG',X'0B',CL5'AGB T'                                  02366*65
         DC    C'AGG',X'0C',CL5'AGC T'                                  02367*65
         DC    C'AGG',X'0D',CL5'AGD T'                                  02368*65
         DC    C'AGG',X'0E',CL5'AGE T'                                  02369*65
         DC    C'AGG',X'0F',CL5'AGF T'                                  02370*65
*&&                                                                     02371*66
*                                                                       02372*65
*      ******************************************                       02373*60
*                SYNDICATORS                                            02374*60
*      ******************************************                       02375*60
         DC    C'B/O ',CL5'BLORS'       BLAIR ENT./ORBIS                02376*22
         DC    C'ESPN',CL5'ESPNS'       ESPN SPORTS NETWORK             02377*22
         DC    C'F/L ',CL5'F/L S'       FRIES DIST./LBS                 02378*22
         DC    C'G-T ',CL5'G-T S'       GOODSEN-TODMAN                  02379*22
         DC    C'G/Y ',CL5'G/Y S'       GAYLORD/LBS COMMUNICATIONS      02380*22
         DC    C'G/L ',CL5'G/L S'       GAYLORD SYNDICOM                02381*22
         DC    C'J/M ',CL5'J/M S'       JOHNSON PUB./MK THOMAS          02382*22
         DC    C'L-T ',CL5'L-T S'       LORIMAR-TELEPICTURES            02383*22
         DC    C'L/C ',CL5'L/C S'       LBS COMMUNICATIONS/COKE         02384*22
         DC    C'L/O ',CL5'L/O S'       LBS/JIM OWENS                   02385*22
         DC    C'M&&M ',CL5'M&&M S'       M & M SYNDICATION             02386*22
         DC    C'M-T ',CL5'M-T S'       MCA-TV                          02387*22
         DC    C'M/&& ',CL5'M/&& S'       MGM/UA COMM. INC./LBS         02388*22
         DC    C'M/C ',CL5'M/C S'       MGM/UA CAMELOT                  02389*22
         DC    C'M/L ',CL5'M/L S'       MGM ENT./LBS COMM.              02390*22
         DC    C'M/U ',CL5'M/U S'       MGM/UA TEVISION DIST.           02391*22
         DC    C'O&&M ',CL5'O&&M S'       OLGILVY AND MATHER            02392*22
         DC    C'P/A ',CL5'P/A S'       PARKSIDE/ACCESS                 02393*22
         DC    C'S/O ',CL5'S/O S'       SYNDICOM ORBIS                  02394*22
         DC    C'T/G ',CL5'T/G S'       TELETRIB/GROUP W                02395*22
         DC    C'T/H ',CL5'T/H S'       TELETRIB/HAL ROACH              02396*22
         DC    C'T/L ',CL5'T/L S'       TELETRIB/LORIMAR                02397*22
         DC    C'T/M ',CL5'T/M S'       TELETRIB/MCA                    02398*22
         DC    C'T/Q ',CL5'T/Q S'       TELETRIB/QINTEX ENT.            02399*22
         DC    C'T/S ',CL5'T/S S'       TITAN SPORTS                    02400*22
         DC    C'T/T ',CL5'T/T S'       TPE/TELEREP                     02401*22
         DC    C'T/V ',CL5'T/V S'       TELETRIB/VIACOM                 02402*22
         DC    C'TBS ',CL5'TBS S'       TURNER BROADCASTING SALES       02403*22
         DC    C'TEC ',CL5'TEC S'       TELETRIB/EMBASSY                02404*22
         DC    C'TEL ',CL5'TEL S'       TELEREP                         02405*22
         DC    C'TEN ',CL5'TEN S'       TELEVISION ENTERPRISE NETW.     02406*22
         DC    C'TLT ',CL5'TLT S'       TELETRIB                        02407*22
         DC    C'TM  ',CL5'TM  S'       TEN MEDIA                       02408*22
         DC    C'TOT ',CL5'TOT S'       TOTAL VIDEO                     02409*22
         DC    C'TPE ',CL5'TPE S'       TELEVISON PROGRAM ENTERP.       02410*22
         DC    C'TRC ',CL5'TRC S'       TELEVISION RADIO CABLE GROUP    02411*22
         DC    C'TRE ',CL5'TRE S'       TRIBUNE ENTERTAINMENT           02412*22
         DC    C'TRI ',CL5'TRI S'       TRI-STAR PICTURES INC.          02413*22
         DC    C'TTW ',CL5'TTW S'       WTTW                            02414*22
         DC    C'TW  ',CL5'TW  S'       TRANS WORLD INTERNATIONAL       02415*22
         DC    C'VIA ',CL5'VIA S'       VIACOM ENTERPRISES, INC.        02416*22
         DC    C'VIC ',CL5'VIC S'       VICTORY TELEVISION              02417*22
         DC    C'WB  ',CL5'WB  S'       WARNER BROTHERS TELEVISION      02418*22
         DC    C'WC  ',CL5'WC  S'       WINNER COMM.                    02419*22
         DC    C'WV  ',CL5'WV  S'       WORLDVISION ENTERP.             02420*22
         DC    C'WW  ',CL5'WW  S'       WESTERN WORLD TELEVISION        02421*22
         DC    C'Y&&R ',CL5'Y&&R S'       YOUNG & RUBICAM               02422*22
         DC    C'2/L ',CL5'2/L S'       20TH CENTURY FOX/LBS            02423*22
         DC    C'2/M ',CL5'2/M S'       20TH CENTURY FOX/MPC, INC.      02424*22
         DC    C'20  ',CL5'20F S'       20TH CENTURY FOX TV             02425*22
NETWORKS EQU   (*-NETTAB)/L'NETTAB                                      02426*22
                                                                        02427**3
WORKX    EQU   *                                                        02428**3
                                                                        02429**5
         EJECT                                                          02430**3
*********************************************************************** 02431**3
* DSECTS  ******  DSECTS  ****** DSECTS ****** DSECTS ****** DSECTS     02432**3
*********************************************************************** 02433**3
*          DATA SET DENNHPTD   AT LEVEL 002 AS OF 09/22/88                      
         EJECT                                                          02435   
* DSECT TO COVER NIELSEN'S MEDIA INFORMATION TAPE (1ST 60 POSITIONS)    02436   
*                                                                       02437   
MIREC    DSECT                                                          02438   
MISEQ    DS    CL2          1-2    RECORD SEQUENCE CODE                 02439   
MISAMPLE DS    CL1          3      SAMPLE INDICATOR                     02440   
*                                    BLANK = NATIONAL PEOPLE METER      02441   
*                                    H     = NATIONAL HISPANIC          02442   
MICORDTE DS    CL7          4-10   START OF CORRECTION INTERVAL         02443   
*                                    CYYMMDD                            02444   
MIORIG   DS    CL1         11      0=ORIGINAL 1=CORRECTION 2=DELETION   02445   
MICORWHY DS    CL3         12-14   CORRECTION REASON                    02446   
*                                   BLANK = N/A                         02447   
*                                   100=NEW 101=VIEWING CHANGE          02448   
*                                   102=DESCRIPTIVE DATA CHANGE         02449   
*                                   103=INFORMATIONAL ONLY CHANGE       02450   
*                                   200=PROPRIETARY DATA CHANGE         02451   
MINET    DS    CL6         15-20   NETWORK (ABC, CBS, NBC, ETC.)        02452   
*                                  OR - DATA TYPE CODE (TVU, INT, ETC.) 02453   
MINUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE        02454   
MITRACK  DS    CL3         31-33   TRACKAGE ID (CABLE ONLY)             02455   
MIFEED   DS    CL1         34      FEED PATTERN                         02456   
*                                   " "=NTI D=DUAL L=LIVE H=HISPANIC    02457   
MIBREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT        02458   
MISPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL         02459   
*                                  2=RECURRING SPECIAL                  02460   
MIESTYPE DS    CL1         37      AUDIENCE ESTIMATE TYPE               02461   
*                                  1=AVG AUD  2=GAA 3=GSA(SYND W/SPOT)  02462   
MIDELVRY DS    CL2         38-39   DELIVERY OPTIONS                     02463   
*                                   "  "  = NOT USED                    02464   
*                                   01-20 = AIRS ACROSS MULTIPLE DPTS   02465   
*                                   21-40 = DELIVERY VEHICLE            02466   
*                                   41-40 = IN-HOME/OUT-OF-HOME         02467   
MISYNORG DS    CL1         40      SYNDICATORS ONLY - ORIG/REPEAT/COMB  02468   
MISYNPRO DS    CL1         41      SYNDICATORS ONLY - PROGRAM SPOTS     02469   
MIDYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                 02470   
MISTART  DS    CL7         44-50   CYYMMDD                              02471   
MIEND    DS    CL7         51-57   CYYMMDD                              02472   
MITELNUM DS    CL10        58-67   TELECAST NUMBER                      02473   
MICOMNUM DS    CL3         68-70   COMPONENT NUMBER                     02474   
MICOVSAM DS    CL6         71-76   COVERAGE SAMPLE ID                   02475   
MICOVCAL DS    CL1         77      COVERAGE CALCULATION IND             02476   
*                                   BLANK = TOTAL US                    02477   
*                                   1     = HOMES ABLE TO VIEW          02478   
*                                   2     = TOTAL CABLE UNIVERSE        02479   
*                                   3     = OTHER                       02480   
MIMKTBRK DS    CL3         78-80   MARKET BREAK IDENTIFIER              02481   
MITPHH   DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER   02482   
MIQHID   DS    CL2         83-84   QUARTER HOUR ID                      02483   
MITYPE   DS    CL1         85      RECORD TYPE - B,C,D,E,F,G,H,M,P      02484   
*                                                OR BLANK               02485   
MIEVSHR  DS    CL2         86-87   EVENT START HOUR                     02486   
MIEVSMIN DS    CL2         88-89   EVENT START MINUTE                   02487   
MIEVSSEC DS    CL2         90-91   EVENT START SECOND                   02488   
MIDEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID           02489   
MIDEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE              02490   
MIDEMAVI DS    CL1         96      DEMO AVERAGE IND                     02491   
MIVCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR   02492   
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK   02493   
MINA     DS    CL1         98      NOT AVAILABLE FLAG - BLANK=OK        02494   
*                                    1=NOT AVAILABLE                    02495   
MIRCTR   DS    CL1         99      RECORD CTR - 0=UNIQUE RECORD IN 1-58 02496   
*                                    1-9=MULTIPLE OCCURRENCES           02497   
MIBUCTYP DS    CL1        100      BUCKET TYPE                          02498   
MIPPPV   DS    CL1        101      POCKETPIECE VS PRELIMINARY IND       02499   
MIVARRSN DS    CL4        102-105  VARIANCE REASON                      02500   
*                                                                       02501   
         DS    CL9        106-114  (FILLER - ALWAYS BLANK)              02502   
*                                                                       02503   
         EJECT                                                          02504   
* DSECT TO COVER REPORT DESCRIPTOR RECORD                               02505   
*                                                                       02506   
B0REC    DSECT                                                          02507   
B0SEQ    DS    CL2          1-2    C'00' - REPORT DESCRIPTOR RECORD     02508   
         DS    CL39         3-41                                        02509   
B0DYSWKS DS    CL2         42-43   NUM DAYS/WEEKS=01                    02510   
B0START  DS    CL7         44-50   CYYMMDD                              02511   
B0END    DS    CL7         51-57   CYYMMDD                              02512   
         DS    CL57        58-114                                       02513   
B0REPORT DS    CL25        115-139 REPORT/FILE ID - "POCKETPIECE"       02514   
*                                                                       02515   
         SPACE 2                                                        02516   
* DSECT TO COVER HOUSEHOLD UNIVERSE ESTIMATE RECORD                     02517   
*                                                                       02518   
H1REC    DSECT                                                          02519   
H1SEQ    DS    CL2          1-2    C'01' - UNIVERSE ESTIMATE RECORD     02520   
H1SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02521   
         DS    CL7          4-10                                        02522   
H1CORR   DS    CL1         11      CORRECTION FLAG                      02523   
         DS    CL3         12-14                                        02524   
H1DTC    DS    CL6         15-20   DATA TYPE CODE = 'UES   '            02525   
         DS    CL23        21-43                                        02526   
H1START  DS    CL7         44-50   CYYMMDD (START OF SEASON)            02527   
H1END    DS    CL7         51-57   CYYMMDD (END OF SEASON)              02528   
         DS    CL20        58-77                                        02529   
H1MKTBRK DS    CL3         78-80                                        02530   
         DS    CL4         81-84                                        02531   
H1TYPE   DS    CL1         85      C'H' - HOUSEHOLD DATA                02532   
         DS    CL94        86-179                                       02533   
H1HHUNIV DS    CL9        180-188  HH UNIVERSE ESTIMATE (000,XXX,XX0)   02534   
*                                    EXPRESSED AS THOUSANDS             02535   
         SPACE 2                                                        02536   
* DSECT TO COVER PERSONS UNIVERSE ESTIMATE RECORD                       02537   
*                                                                       02538   
P1REC    DSECT                                                          02539   
P1SEQ    DS    CL2          1-1    C'01' - UNIVERSE ESTIMATE RECORD     02540   
P1SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02541   
         DS    CL7          4-10                                        02542   
P1CORR   DS    CL1         11      CORRECTION FLAG                      02543   
         DS    CL3         12-14                                        02544   
P1DTC    DS    CL6         15-20   DATA TYPE CODE = 'UES   '            02545   
         DS    CL23        21-43                                        02546   
P1START  DS    CL7         44-50   CYYMMDD (START OF SEASON)            02547   
P1END    DS    CL7         51-57   CYYMMDD (END OF SEASON)              02548   
         DS    CL20        58-77                                        02549   
P1MKTBRK DS    CL3         78-80                                        02550   
         DS    CL4         81-84                                        02551   
P1TYPE   DS    CL1         85      C'P' - PERSONS DATA                  02552   
         DS    CL30        86-115                                       02553   
P1DATA   DS    0CL9       116      DEMO GROUP CATAGORIES                02554   
*                                                                       02555   
         SPACE 2                                                        02556   
* DSECT TO COVER HOUSEHOLD SAMPLE COUNTS RECORD                         02557   
*                                                                       02558   
H2REC    DSECT                                                          02559   
H2SEQ    DS    CL2          1-2    C'02' - SAMPLE COUNTS RECORD         02560   
H2SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02561   
         DS    CL7          4-10                                        02562   
H2CORR   DS    CL1         11      CORRECTION FLAG                      02563   
         DS    CL3         12-14                                        02564   
H2DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '            02565   
         DS    CL23        21-43                                        02566   
H2START  DS    CL7         44-50   CYYMMDD                              02567   
H2END    DS    CL7         51-57   CYYMMDD                              02568   
         DS    CL20        58-77                                        02569   
H2MKTBRK DS    CL3         78-80                                        02570   
         DS    CL4         81-84                                        02571   
H2TYPE   DS    CL1         85      C'H' - PERSONS DATA                  02572   
         DS    CL33        86-118                                       02573   
H2DAYS   DS    0CL7       (119-125)  DAYS OF WEEK INDICATOR             02574   
H2MON    DS    CL1        119        MONDAY      (1 OR 0)               02575   
H2TUE    DS    CL1        120        TUESDAY     (1 OR 0)               02576   
H2WED    DS    CL1        121        WEDNESDAY   (1 OR 0)               02577   
H2THU    DS    CL1        122        THURSDAY    (1 OR 0)               02578   
H2FRI    DS    CL1        123        FRIDAY      (1 OR 0)               02579   
H2SAT    DS    CL1        124        SATURDAY    (1 OR 0)               02580   
H2SUN    DS    CL1        125        SUNDAY      (1 OR 0)               02581   
*                                                                       02582   
         SPACE 2                                                        02583   
* DSECT TO COVER PERSONS SAMPLE COUNTS RECORD                           02584   
*                                                                       02585   
P2REC    DSECT                                                          02586   
P2SEQ    DS    CL2          1-2    C'02' - SAMPLE COUNTS RECORD         02587   
P2SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02588   
         DS    CL7          4-10                                        02589   
P2CORR   DS    CL1         11      CORRECTION FLAG                      02590   
         DS    CL3         12-14                                        02591   
P2DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '            02592   
         DS    CL23        21-43                                        02593   
P2START  DS    CL7         44-50   CYYMMDD                              02594   
P2END    DS    CL7         51-57   CYYMMDD                              02595   
         DS    CL20        58-77                                        02596   
P2MKTBRK DS    CL3         78-80                                        02597   
         DS    CL4         81-84                                        02598   
P2TYPE   DS    CL1         85      C'P' - PERSONS DATA                  02599   
         DS    CL6         86-91                                        02600   
P2DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID           02601   
         DS    CL21        95-115                                       02602   
P2DATA   DS    0CL9       116      DEMO GROUP CATAGORIES                02603   
*                                                                       02604   
         SPACE 2                                                        02605   
* DSECT TO COVER HOUSEHOLD USAGE RECORD                                 02606   
*                                                                       02607   
H3REC    DSECT                                                          02608   
H3SEQ    DS    CL2          1-1    C'03' - HOUSEHOLD USAGE RECORD       02609   
H3SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02610   
         DS    CL7          4-10                                        02611   
H3CORR   DS    CL1         11      CORRECTION FLAG                      02612   
         DS    CL3         12-14                                        02613   
H3DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '            02614   
         DS    CL16        21-36                                        02615   
H3AET    DS    C           37      AUDIENCE TYPE 1=AVG AUD              02616   
         DS    CL6         38-43                                        02617   
H3START  DS    CL7         44-50   CYYMMDD                              02618   
H3END    DS    CL7         51-57   CYYMMDD                              02619   
         DS    CL23        58-80                                        02620   
H3PHH    DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER   02621   
         DS    CL2         83-84                                        02622   
H3TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA      02623   
H3EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)             02624   
H3EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)        02625   
         DS    CL7         90-96                                        02626   
H3VCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR   02627   
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK   02628   
         DS    CL17        98-114                                       02629   
H3DUR    DS    CL4        115-118  EVENT DURATION   0000-1440 MINUTES   02630   
         DS    CL61       119-179                                       02631   
H3AVEHUT DS    CL9        180-188  AUDIENCE AVE HUT PROJ. (XXX,XXX,XXX) 02632   
H3AVERTG DS    CL3        189-191  AUDIENCE AVE HUT RATING (XX.X)       02633   
         DS    CL68       192-259                                       02634   
H3QH1HUT DS    CL9        260-268  1ST QTR HR HUT PROJ (XXX,XXX,XXX)    02635   
H3QH1RTG DS    CL3        269-271  1ST QTR HR HUT RATING (XX.X)         02636   
         DS    CL37       272-308                                       02637   
H3QH2HUT DS    CL9        309-317  2ND QTR HR HUT PROJ (XXX,XXX,XXX)    02638   
H3QH2RTG DS    CL3        318-320  2ND QTR HR HUT RATING (XX.X)         02639   
         DS    CL80       321-400                                       02640   
H3OPT    DS    CL1        401      0=REGULAR DATA   1=OPTIONAL DATA     02641   
*                                                                       02642   
         SPACE 2                                                        02643   
* DSECT TO COVER DEMO USAGE RECORD                                      02644   
*                                                                       02645   
P3REC    DSECT                                                          02646   
P3SEQ    DS    CL2          1-2    C'03' - DEMO USAGE RECORD            02647   
P3SAMPLE DS    CL1          3      SAMPLE INDICATOR = BLANK             02648   
         DS    CL7          4-10                                        02649   
P3CORR   DS    CL1         11      CORRECTION FLAG                      02650   
         DS    CL3         12-14                                        02651   
P3DTC    DS    CL6         15-20   DATA TYPE CODE = 'INT   '            02652   
         DS    CL16        21-36                                        02653   
P3AET    DS    C           37      AUDIENCE TYPE 1=AVG AUD              02654   
         DS    CL6         38-43                                        02655   
P3START  DS    CL7         44-50   CYYMMDD                              02656   
P3END    DS    CL7         51-57   CYYMMDD                              02657   
         DS    CL27        58-84                                        02658   
P3TYPE   DS    CL1         85      C'P' - PERSONS                       02659   
         DS    CL6         86-91                                        02660   
P3DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID           02661   
         DS    CL21        95-115                                       02662   
P3DCATS  DS    CL180      116-295  DEMOGRAPHIC CATEGORIES (20CL9)       02663   
         DS    CL105      296-400                                       02664   
P3OPT    DS    CL1        401-401  0=REGULAR DATA   1=OPTIONAL DATA     02665   
*                                                                       02666   
         EJECT                                                          02667   
* DSECT TO COVER PROGRAM DESCRIPTOR RECORD                              02668   
*                                                                       02669   
D4REC    DSECT                                                          02670   
D4SEQ    DS    CL2          1-2    C'04' - PROGRAM DESCRIPTOR RECORD    02671   
         DS    CL8          3-10                                        02672   
D4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION      02673   
         DS    CL3         12-14                                        02674   
D4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)            02675   
D4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE        02676   
         DS    CL4         31-34                                        02677   
D4BREAK  DS    CL1         35      0=NOT A BREAKOUT...1=BREAKOUT        02678   
D4SPEC   DS    CL1         36      0=NOT A SPECIAL....1=SPECIAL         02679   
         DS    CL1         37                                           02680   
         DS    CL2         38-39                                        02681   
D4TTYPE  DS    CL1         40      TELECAST TYPE (SYND)                 02682   
*                                  O=ORIGINAL   R=REPEAT                02683   
*                                  C=COMBINED   M=MULTIPLE              02684   
         DS    CL1         41                                           02685   
D4DYSWKS DS    CL2         42-43   NUMBER OF DAYS/WEEKS                 02686   
D4START  DS    CL7         44-50   CYYMMDD                              02687   
D4END    DS    CL7         51-57   CYYMMDD                              02688   
         DS    CL20        58-77                                        02689   
D4MKTBRK DS    CL3         78-80   MARKET BREAK                         02690   
         DS    CL4         81-84                                        02691   
D4TYPE   DS    CL1         85      C'D' - PROGRAM DESCRIPTOR DATA       02692   
D4EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)             02693   
D4EVSMIN DS    CL2         88-89   EVENT START MINUTE (00-59)           02694   
         DS    CL7         90-96                                        02695   
D4VCR    DS    CL1         97      VCR INDICATOR - BLANK=INCLUDES VCR   02696   
*                                    1=EXCLUDES   2=INCLUDES PLAYBACK   02697   
D4NAVAL  DS    CL1         98      1 = DATA NOT AVAILABLE               02698   
         DS    CL16        99-114                                       02699   
D4NAME   DS    CL25       115-139  PROGRAM NAME                         02700   
         DS    CL40       140-179                                       02701   
D4TITLE  DS    CL32       180-211  EPISODE TITLE                        02702   
         DS    CL4        212-215                                       02703   
D4ALPHA  DS    CL4        216-219  PROGRAM TYPE (ALPHA)                 02704   
         DS    CL9        220-228                                       02705   
D4MULTI  DS    CL1        229      MULTI-DAY     Y=MULTIPLE TELECASTS   02706   
D4REPEAT DS    CL1        230      REPEAT IND.   Y=COMPLEX PROGRAM      02707   
         DS    CL7        231-237                                       02708   
D4PREM   DS    CL1        238      PREMIER IND.  Y=PREMIER TELECAST     02709   
         DS    CL11       239-249                                       02710   
D4SYNID  DS    CL4        250-253  SYNDICATOR ID (SEE NETWORK TABLE)    02711   
         DS    CL22       254-275                                       02712   
D4DPT    DS    CL2        276-277  REPORTED DAYPART                     02713   
*                                    PR=PRIME TIME   WM=WEEKDAY MORNING 02714   
*                                    EF=EARLY FRINGE WD=WEEKDAY DAYTIME 02715   
*                                    LF=LATE FRINGE  ED=WEEKEND DAYTIME 02716   
D4DUR    DS    CL4        278-281  EVENT DURATION   0000-1440 MINUTES   02717   
D4DAYS   DS    0CL7      (282-288) DAYS OF WEEK INDICATOR               02718   
D4MON    DS    CL1        282        MONDAY      (1 OR 0)               02719   
D4TUE    DS    CL1        283        TUESDAY     (1 OR 0)               02720   
D4WED    DS    CL1        284        WEDNESDAY   (1 OR 0)               02721   
D4THU    DS    CL1        285        THURSDAY    (1 OR 0)               02722   
D4FRI    DS    CL1        286        FRIDAY      (1 OR 0)               02723   
D4SAT    DS    CL1        287        SATURDAY    (1 OR 0)               02724   
D4SUN    DS    CL1        288        SUNDAY      (1 OR 0)               02725   
         DS    CL16       289-304                                       02726   
*                                                                       02727   
         EJECT                                                          02728   
*        THESE FIELDS (305-   ) ARE NOT REPORTED ON ALL D4 RECORDS      02729   
D4STA    DS    CL5        305-309  TOTAL PROGRAM STATION COUNT          02730   
         DS    CL5        310-314                                       02731   
D4COV    DS    CL2        315-316  TOTAL PROGRAM COVERAGE PERCENT       02732   
         DS    CL11       317-327                                       02733   
D4TELE   DS    CL3        328-330  TELECASTS 01=DAY 02-07=AVERAGES      02734   
D4TOTDUR DS    CL6        331-336  TOTAL DURATION                       02735   
D4AVEHUT DS    CL9        337-345  PROGRAM AVERAGE PROJ. (XXX,XXX,XXX)  02736   
D4AVERTG DS    CL3        346-348  PROGRAM AVERAGE RATING (XX.X)        02737   
D4REPORT DS    CL1        349      REPORTABILITY IND. 0=REPORTABLE      02738   
         DS    CL4        350-353                                       02739   
D4HUT    DS    CL9        354-362  PROGRAM HUT PROJ. (XXX,XXX,XXX)      02740   
D4SHR    DS    CL2        363-364  PROGRAM SHARE (XX)                   02741   
         DS    CL11       365-375                                       02742   
D4PROJ   DS    CL9        376-384  TOTAL AUDIENCE PROJ. (XXX,XXX,XXX)   02743   
D4RTG    DS    CL3        385-387  TOTAL AUDIENCE RATING (XX.X)         02744   
         DS    CL12       388-400                                       02745   
*                                                                       02746   
D4OPT    DS    CL1        401-401  0=REGULAR DATA   1=OPTIONAL DATA     02747   
*                                                                       02748   
         EJECT                                                          02749   
* DSECT TO COVER HOUSEHOLD PROGRAM RECORD                               02750   
*                                                                       02751   
H4REC    DSECT                                                          02752   
H4SEQ    DS    CL2          1-2    C'04' - HALF HOUR DETAIL RECORD      02753   
         DS    CL8          3-10                                        02754   
H4ORIG   DS    CL1         11      0=ORIGINAL.........1=CORRECTION      02755   
         DS    CL3         12-14                                        02756   
H4NET    DS    CL6         15-20   NETWORK (ABC, CBS OR NBC)            02757   
H4NUM    DS    CL10        21-30   PROGRAM/STATION/CATEGORY CODE        02758   
         DS    CL13        31-43                                        02759   
H4START  DS    CL7         44-50   CYYMMDD                              02760   
H4END    DS    CL7         51-57   CYYMMDD                              02761   
         DS    CL23        58-80                                        02762   
H4HALF   DS    CL2         81-82   HALF HOUR ID   01-48                 02763   
         DS    CL2         83-84                                        02764   
H4TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA      02765   
H4EVSHR  DS    CL2         86-87   EVENT START HOUR                     02766   
H4EVSMIN DS    CL2         88-89   EVENT START MINUTE                   02767   
         DS    CL25        90-114                                       02768   
H4EVDUR  DS    CL4        115-118  DURATION PRG RAN W/IN 1/2HR          02769   
H4DAYS   DS    0CL7      (119-125) DAYS OF WEEK INDICATOR               02770   
H4MON    DS    CL1        119        MONDAY      (1 OR 0)               02771   
H4TUE    DS    CL1        120        TUESDAY     (1 OR 0)               02772   
H4WED    DS    CL1        121        WEDNESDAY   (1 OR 0)               02773   
H4THU    DS    CL1        122        THURSDAY    (1 OR 0)               02774   
H4FRI    DS    CL1        123        FRIDAY      (1 OR 0)               02775   
H4SAT    DS    CL1        124        SATURDAY    (1 OR 0)               02776   
H4SUN    DS    CL1        125        SUNDAY      (1 OR 0)               02777   
         DS    CL54       126-179                                       02778   
H4PROJ   DS    CL9        180-188  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)   02779   
H4RTG    DS    CL3        189-191  HALF HOUR AUDIENCE RATING (XX.X)     02780   
H4REPT   DS    CL1        192      1/2HR REPORTABILITY INDICATOR        02781   
         DS    CL13       192-205                                       02782   
H4SHR    DS    CL2        206-207  HALF HOUR PROGRAM SHARE (XX)         02783   
         DS    CL46       208-253                                       02784   
H4DUR1   DS    CL6        254-259  1ST QTR HOUR   1-15 MINS.-INDIV. DAY 02785   
*                                    DURATION     2-75 MINS.-AVERAGES   02786   
         DS    CL43       260-302                                       02787   
H4DUR2   DS    CL6        303-308  2ND QTR HOUR   0-15 MINS.-INDIV. DAY 02788   
*                                    DURATION     2-75 MINS.-AVERAGES   02789   
         SPACE 2                                                        02790   
* DSECT TO COVER DEMO PROGRAM RECORD                                    02791   
*                                                                       02792   
P4REC    DSECT                                                          02793   
P4SEQ    DS    CL2          1-2    C'04' - DEMO PROGRAM RECORD          02794   
         DS    CL12         3-14                                        02795   
P4NET    DS    CL6         15-20   NETWORK                              02796   
         DS    CL64        21-84                                        02797   
P4TYPE   DS    CL1         85      C'P' - PERSONS                       02798   
         DS    CL6         86-91                                        02799   
P4DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID           02800   
P4DEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE              02801   
*                                    0=NOT PROGRAM PUTS  1=PROGRAM PUTS 02802   
         DS    CL20        96-115                                       02803   
P4DCATS  DS    CL180      116-295  DEMOGRAPHIC CATEGORIES (20CL9)       02804   
         EJECT                                                          02805   
* DSECT TO COVER NON-NETWORK HOUSEHOLD USAGE RECORD                     02806   
*                                                                       02807   
H5REC    DSECT                                                          02808   
H5SEQ    DS    CL2          1-2    C'05' - NON-NETWORK HOUSEHOLD RECORD 02809   
         DS    CL12         3-14                                        02810   
H5NET    DS    CL6         15-20   DATA TYPE CODE (AGG)                 02811   
H5NUM    DS    CL10        21-30   STATION CODE   1=IND   2=SUP         02812   
*                                         3=PBS   4=PAY   5=CAB         02813   
         DS    CL13        31-43                                        02814   
H5START  DS    CL7         44-50   CYYMMDD                              02815   
H5END    DS    CL7         51-57   CYYMMDD                              02816   
         DS    CL23        58-80                                        02817   
H5PHH    DS    CL2         81-82   TOTAL PROGRAM/HALF HOUR IDENTIFIER   02818   
         DS    CL2         83-84                                        02819   
H5TYPE   DS    CL1         85      C'H' - HALF HOUR HOUSEHOLD DATA      02820   
H5EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)             02821   
H5EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)        02822   
         DS    CL90        90-179                                       02823   
H5HOME   DS    CL9        180-188  HALF HOUR AUD. PROJ. (XXX,XXX,XXX)   02824   
H5RTG    DS    CL3        189-191  HALF HOUR AUD. RATING (XX.X)         02825   
*                                                                       02826   
         SPACE 2                                                        02827   
* DSECT TO COVER NON-NETWORK DEMO USAGE RECORD                          02828   
*                                                                       02829   
P5REC    DSECT                                                          02830   
P5SEQ    DS    CL2          1-2    C'05' - NON-NETWORK DEMO RECORD      02831   
         DS    CL82         3-84                                        02832   
P5TYPE   DS    CL1         85      C'P' - PERSONS                       02833   
P5EVSHR  DS    CL2         86-87   EVENT START HOUR (06-29)             02834   
P5EVSMIN DS    CL2         88-89   EVENT START MINUTE (00 OR 30)        02835   
         DS    CL2         90-91                                        02836   
P5DEMGRP DS    CL3         92-94   DEMOGRAPHIC GROUP START ID           02837   
P5DEMTYP DS    CL1         95      DEMOGRAPHIC RECORD TYPE              02838   
*                                    0=NOT PROGRAM PUTS  1=PROGRAM PUTS 02839   
         EJECT                                                          02840   
*        DEINTD                                                         02841   
*          DATA SET DEINTD     AT LEVEL 005 AS OF 11/17/87                      
* DSECT TO COVER INTERIM CONVERSION RECORDS                             00002   
*                                                                       00003   
INTERD   DSECT                                                          00004   
INTRECLN DS    F                   RECORD LENGTH                        00005   
INTKEY   DS    CL30                SORT KEY                             00006   
INTVALS  DS    0CL20               FILTERABLE VALUES                    00007   
INTRTYP  DS    CL1                 RECORD TYPE                          00008   
INTMRKT  DS    XL2                 MARKET NUMBER                        00009   
INTSTA   DS    CL5                 STATION CALL LETTERS                 00010   
INTBOOK  DS    XL2                 BOOK (BINARY YYMM)                   00011   
INTSTYP  DS    XL1                 STATION TYPE                         00012   
INTDAYWK DS    XL1                 DAY & WEEK                           00013   
INTSQH   DS    XL1                 START QTR HR                         00014   
INTEQH   DS    XL1                 END QTR HR                           00015   
INTSPILL DS    C                   Y=SPILL MARKET                       00016   
INTADUR  DS    CL1                 ACTUAL DURATION FOR PRGM RECS.       00017**2
INTPNO   DS    CL2                 PROG NUMBER(SYND AND NETW PROGS)     00018**4
INTBTYP  DS    CL1                 INTERNALLY GENERATED BOOK TYPE       00019**5
         DS    CL1                                                      00020**5
INTDATA  DS    0C                  ALPHA DATA & BINARY VALUES           00021   
         SPACE 1                                                        02843   
*        DEINTNTID                                                      02844   
*          DATA SET DEINTNT3D  AT LEVEL 008 AS OF 10/31/95                      
*                                  NTI FILE CONVERSION FIELDS (8733)    00002**4
INTDYBIT DS    B                   DAY CODE BIT SETTING                 00003   
INTDUR   DS    X                   DURATION IN QUARTER HOURS            00004   
INTDURM  DS    X                   DURATION IN MINUTES                  00005   
INTSTIM  DS    XL2                 PROGRAM START TIME - MILITARY        00006   
INTETIM  DS    XL2                 PROGRAM END TIME - MILITARY          00007   
*                                                                       00008   
INTPNUM  DS    XL2                 PROGRAM NUMBER                       00009   
INTPNTI  DS    CL5                 5 CHAR NTI # PWOS                    00010**6
INTPTYP  DS    CL2                 PROGRAM TYPE                         00011   
INTDPT   DS    C                   NSI DAYPART (ALWAYS ZERO)            00012**4
INTPREM  DS    C                   PREMIERE INDICATOR                   00013   
INTPNAME DS    CL25                PROGRAM NAME                         00014   
INTTITLE DS    CL32                EPISODE TITLE                        00015**5
INTAUD   DS    XL2                 AUDIENCE PROJECTION                  00016   
INTSTAC  DS    XL2                 STATION COUNT                        00017   
INTCOV   DS    XL2                 COVERAGE                             00018   
INTRSF   DS    X                   REDUCED STAT FACILITIES INDICATOR    00019**2
*                                                                       00020**2
INTMTYP  DS    X                   MARKET TYPE                          00021   
INTDTYP  DS    X                   DATA TYPE - SET BY TELECAST TYPE     00022   
INTIBOOK DS    XL2                 INTERNAL BOOK VALUE (FOR '5E' ELEM)  00023**3
INTDPT2  DS    CL2                 NTI DAYPART                          00024**4
INTVAR   DS    7XL7    XL1,XL1     VAR - 7 DAYS    START QH/END QH      00025**4
*                      XL1                         DURATION IN MINUTES  00026**4
*                      XL2,XL2                     START TIME/END TIME  00027**4
*                                                                       00028   
INTACCS  DS    0F                  WORK AREA VALUES                     00029   
         EJECT                                                          02846   
*        DECALVPHD                                                      02847   
*          DATA SET DECALVPHD  AT LEVEL 004 AS OF 09/04/87                      
         TITLE 'DECALVPH INTERFACE BLOCK'                               00002   
CALVPHD  DSECT                                                          00003   
VPHDEMS  DS    A                   START OF DEMOS (4 BYTE VALUES)       00004   
*                                   IN DEMDISP ORDER                    00005   
VPHDATE  DS    XL3                 START DATE                           00006   
VPHDPT   DS    CL2                 NTI DAYPART                          00007   
VPHPCOD  DS    CL2                 NTI PROGRAM CODE                     00008**2
VPHSPCL  DS    CL1                 NTI SPECIAL INDICATOR                00009   
VPHTELE  DS    XL1                 NUMBER OF DAYS/TELECASTS             00010   
VPHDUR   DS    XL2                 DURATION IN MINUTES                  00011   
VPHACTD  DS    X                   ACTUAL DAYS                          00012**3
*                                  X'40'=MON...X'01'=SUN                00013**3
VPHINT   DS    A                   A(INTAB TABLE)                       00014**3
         SPACE 2                                                        00015**3
*                 IN TAB ENTRY DSECT                                    00016**3
*                 TABLE CONSISTS OF MULTIPLE ENTRIES TERMINATED         00017**3
*                 BY A DATE OF X'000000'                                00018**3
INTABD   DSECT                                                          00019**3
INTABTAB DS    0XL156                                                   00020**4
INTABDAT DS    XL3                 DATE OF INTAB COUNTS                 00021**4
INTABDAY DS    X                   DAY OF WEEK                          00022**3
*                                  X'40'=MON...X'01'=SUN                00023**3
INTABCNT DS    38F                 IN TAB COUNTS                        00024**3
         EJECT                                                          02849   
*          DATA SET DDDPRINT   AT LEVEL 001 AS OF 02/19/88                      
         SPACE 1                                                        00002   
DPRINT   DSECT                                                          00003   
P        DS    CL132               USERS PRINT LINE - WILL BE CLEARED   00004   
*                                  WITH SPACES AFTER PRINTING           00005   
HEAD1    DS    0CL132                                                   00006   
HEADDATE DS    CL14                DATE DD MMM YY                       00007   
         DS    CL5                                                      00008   
HEADTIME DS    CL10                TIME HH.MM                           00009   
         DS    CL5                                                      00010   
TITLE    DS    CL60                ENTRIES IN THIS FIELD ARE UNDERLINED 00011   
         DS    CL5                                                      00012   
HEADPAGE DS    CL09                PAGE NNNN                            00013   
         DS    CL5                                                      00014   
HEADUSER DS    CL19                AVAILABLE FOR USER                   00015   
         SPACE 1                                                        00016   
MID1     DS    CL132               NON SPACE MID-HEADING LINES          00017   
MID2     DS    CL132               WILL BE PRINTED AFTER TITLE          00018   
MID3     DS    CL132                                                    00019   
MID4     DS    CL132                                                    00020   
         SPACE 1                                                        00021   
SUB1     DS    CL132               NON SPACE SUB-HEADING LINES          00022   
SUB2     DS    CL132               WILL BE PRINTED AFTER MIDS           00023   
SUB3     DS    CL132                                                    00024   
SPACES   DS    CL132               PRESET TO SPACES                     00025   
         SPACE 1                                                        00026   
SPACING  DS    CL4                 PRESET TO PRINT AND SINGLE SPACE     00027   
*                                  (BL01) BYTES 3/4 CAN BE CHANGED      00028   
         SPACE 1                                                        00029   
LINE     DS    PL2                 LINE COUNT - PRESET TO PL2'75'       00030   
*                                  HEADLINE PRINTING CAN BE FORCED BY   00031   
*                                  SETTING THIS GREATER THAN MAXLINE    00032   
         SPACE 1                                                        00033   
MAXLINE  DS    PL2                 PRESET TO 60 LINES PER PAGE          00034   
         SPACE 1                                                        00035   
PAGE     DS    PL4                 PAGE NUMBER - PACKED DECIMAL         00036   
         SPACE 1                   PRESET TO 1 - CAN BE RESET BY USER   00037   
MONTHS   DS    CL36                12 X 3 BYTES  (JAN - DEC)            00038   
SPECDATE DS    CL12                FILL WITH DATE=YYMMDD FOR OVERRIDE   00039   
         SPACE 2                                                        02851   
         PRINT OFF                                                      02852   
         TITLE '- DEMO CONVERSION - NTI POCKETPIECE'                    02854   
         LTORG                                                                  
         EJECT                                                                  
AKPROC   CSECT                                                                  
         NMOD1 0,AKPROC                                                         
         USING KDSCT,R6                                                         
         USING ADSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING AKPROC+4096,R2                                                   
* YW25                                                                          
         TM    A0116+(L'A0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0116,=16C'0'                                                    
         PACK  DUB,A0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0193,FULL+(4-L'K0193)                                           
* YW68                                                                          
         TM    A0125+(L'A0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0125,=16C'0'                                                    
         PACK  DUB,A0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0197,FULL+(4-L'K0197)                                           
* YW911                                                                         
         TM    A0134+(L'A0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0134,=16C'0'                                                    
         PACK  DUB,A0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0201,FULL+(4-L'K0201)                                           
* YW1214                                                                        
         TM    A0143+(L'A0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0143,=16C'0'                                                    
         PACK  DUB,A0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0205,FULL+(4-L'K0205)                                           
* YW1517                                                                        
         TM    A0152+(L'A0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0152,=16C'0'                                                    
         PACK  DUB,A0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0209,FULL+(4-L'K0209)                                           
* YW1820                                                                        
         TM    A0161+(L'A0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0161,=16C'0'                                                    
         PACK  DUB,A0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0213,FULL+(4-L'K0213)                                           
* YW2124                                                                        
         TM    A0170+(L'A0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0170,=16C'0'                                                    
         PACK  DUB,A0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0217,FULL+(4-L'K0217)                                           
* YW2529                                                                        
         TM    A0179+(L'A0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0179,=16C'0'                                                    
         PACK  DUB,A0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0221,FULL+(4-L'K0221)                                           
* YW3034                                                                        
         TM    A0188+(L'A0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0188,=16C'0'                                                    
         PACK  DUB,A0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0225,FULL+(4-L'K0225)                                           
* YW3539                                                                        
         TM    A0197+(L'A0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0197,=16C'0'                                                    
         PACK  DUB,A0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0229,FULL+(4-L'K0229)                                           
* YW4044                                                                        
         TM    A0206+(L'A0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0206,=16C'0'                                                    
         PACK  DUB,A0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0233,FULL+(4-L'K0233)                                           
* YW4549                                                                        
         TM    A0215+(L'A0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0215,=16C'0'                                                    
         PACK  DUB,A0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0237,FULL+(4-L'K0237)                                           
* YW5054                                                                        
         TM    A0224+(L'A0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0224,=16C'0'                                                    
         PACK  DUB,A0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0241,FULL+(4-L'K0241)                                           
* YW5564                                                                        
         TM    A0233+(L'A0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0233,=16C'0'                                                    
         PACK  DUB,A0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0245,FULL+(4-L'K0245)                                           
* YW65+                                                                         
         TM    A0242+(L'A0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0242,=16C'0'                                                    
         PACK  DUB,A0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0249,FULL+(4-L'K0249)                                           
* YM25                                                                          
         TM    A0251+(L'A0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0251,=16C'0'                                                    
         PACK  DUB,A0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0253,FULL+(4-L'K0253)                                           
* YM68                                                                          
         TM    A0260+(L'A0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0260,=16C'0'                                                    
         PACK  DUB,A0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0257,FULL+(4-L'K0257)                                           
* YM911                                                                         
         TM    A0269+(L'A0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0269,=16C'0'                                                    
         PACK  DUB,A0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0261,FULL+(4-L'K0261)                                           
* YM1214                                                                        
         TM    A0278+(L'A0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0278,=16C'0'                                                    
         PACK  DUB,A0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0265,FULL+(4-L'K0265)                                           
* YM1517                                                                        
         TM    A0287+(L'A0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   A0287,=16C'0'                                                    
         PACK  DUB,A0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0269,FULL+(4-L'K0269)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISAK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
AYKPROC  CSECT                                                                  
         NMOD1 0,AYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING AYKPROC+4096,R2                                                  
         LA    RE,AKYFEND                                                       
         CLI    AKYFRST,1                                                       
         BNE   AKYPROCX                                                         
AKYPROC1   CLI   1(RE),255                                                      
         BE    AKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   AKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,AKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   AKYFRST,0                                                        
AKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   AKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    AKYPROCX                                                         
         B     AKYPROC1                                                         
         SPACE 2                                                                
AKYFRST    DC    X'01'                                                          
AKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
AKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BKPROC   CSECT                                                                  
         NMOD1 0,BKPROC                                                         
         USING KDSCT,R6                                                         
         USING BDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING BKPROC+4096,R2                                                   
* YM1820                                                                        
         TM    B0116+(L'B0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0116,=16C'0'                                                    
         PACK  DUB,B0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0273,FULL+(4-L'K0273)                                           
* YM2124                                                                        
         TM    B0125+(L'B0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0125,=16C'0'                                                    
         PACK  DUB,B0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0277,FULL+(4-L'K0277)                                           
* YM2529                                                                        
         TM    B0134+(L'B0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0134,=16C'0'                                                    
         PACK  DUB,B0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0281,FULL+(4-L'K0281)                                           
* YM3034                                                                        
         TM    B0143+(L'B0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0143,=16C'0'                                                    
         PACK  DUB,B0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0285,FULL+(4-L'K0285)                                           
* YM3539                                                                        
         TM    B0152+(L'B0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0152,=16C'0'                                                    
         PACK  DUB,B0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0289,FULL+(4-L'K0289)                                           
* YM4044                                                                        
         TM    B0161+(L'B0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0161,=16C'0'                                                    
         PACK  DUB,B0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0293,FULL+(4-L'K0293)                                           
* YM4549                                                                        
         TM    B0170+(L'B0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0170,=16C'0'                                                    
         PACK  DUB,B0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0297,FULL+(4-L'K0297)                                           
* YM5054                                                                        
         TM    B0179+(L'B0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0179,=16C'0'                                                    
         PACK  DUB,B0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0301,FULL+(4-L'K0301)                                           
* YM5564                                                                        
         TM    B0188+(L'B0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0188,=16C'0'                                                    
         PACK  DUB,B0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0305,FULL+(4-L'K0305)                                           
* YM65+                                                                         
         TM    B0197+(L'B0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0197,=16C'0'                                                    
         PACK  DUB,B0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0309,FULL+(4-L'K0309)                                           
* YWMOMS                                                                        
         TM    B0206+(L'B0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0206,=16C'0'                                                    
         PACK  DUB,B0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0313,FULL+(4-L'K0313)                                           
* YWW1820                                                                       
         TM    B0224+(L'B0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0224,=16C'0'                                                    
         PACK  DUB,B0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0317,FULL+(4-L'K0317)                                           
* YWW2124                                                                       
         TM    B0233+(L'B0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0233,=16C'0'                                                    
         PACK  DUB,B0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0321,FULL+(4-L'K0321)                                           
* YWW2534                                                                       
         TM    B0242+(L'B0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0242,=16C'0'                                                    
         PACK  DUB,B0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0325,FULL+(4-L'K0325)                                           
* YWW3544                                                                       
         TM    B0251+(L'B0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0251,=16C'0'                                                    
         PACK  DUB,B0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0329,FULL+(4-L'K0329)                                           
* YWW4549                                                                       
         TM    B0260+(L'B0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0260,=16C'0'                                                    
         PACK  DUB,B0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0333,FULL+(4-L'K0333)                                           
* YWW5054                                                                       
         TM    B0269+(L'B0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0269,=16C'0'                                                    
         PACK  DUB,B0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0337,FULL+(4-L'K0337)                                           
* YWW55+                                                                        
         TM    B0278+(L'B0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   B0278,=16C'0'                                                    
         PACK  DUB,B0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0341,FULL+(4-L'K0341)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISBK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
BYKPROC  CSECT                                                                  
         NMOD1 0,BYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING BYKPROC+4096,R2                                                  
         LA    RE,BKYFEND                                                       
         CLI    BKYFRST,1                                                       
         BNE   BKYPROCX                                                         
BKYPROC1   CLI   1(RE),255                                                      
         BE    BKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   BKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,BKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   BKYFRST,0                                                        
BKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   BKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    BKYPROCX                                                         
         B     BKYPROC1                                                         
         SPACE 2                                                                
BKYFRST    DC    X'01'                                                          
BKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
BKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CKPROC   CSECT                                                                  
         NMOD1 0,CKPROC                                                         
         USING KDSCT,R6                                                         
         USING CDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING CKPROC+4096,R2                                                   
* ZW25                                                                          
         TM    C0116+(L'C0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0116,=16C'0'                                                    
         PACK  DUB,C0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1049,FULL+(4-L'K1049)                                           
* ZW68                                                                          
         TM    C0125+(L'C0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0125,=16C'0'                                                    
         PACK  DUB,C0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1053,FULL+(4-L'K1053)                                           
* ZW911                                                                         
         TM    C0134+(L'C0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0134,=16C'0'                                                    
         PACK  DUB,C0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1057,FULL+(4-L'K1057)                                           
* ZW1214                                                                        
         TM    C0143+(L'C0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0143,=16C'0'                                                    
         PACK  DUB,C0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1061,FULL+(4-L'K1061)                                           
* ZW1517                                                                        
         TM    C0152+(L'C0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0152,=16C'0'                                                    
         PACK  DUB,C0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1065,FULL+(4-L'K1065)                                           
* ZW1820                                                                        
         TM    C0161+(L'C0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0161,=16C'0'                                                    
         PACK  DUB,C0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1069,FULL+(4-L'K1069)                                           
* ZW2124                                                                        
         TM    C0170+(L'C0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0170,=16C'0'                                                    
         PACK  DUB,C0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1073,FULL+(4-L'K1073)                                           
* ZW2529                                                                        
         TM    C0179+(L'C0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0179,=16C'0'                                                    
         PACK  DUB,C0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1077,FULL+(4-L'K1077)                                           
* ZW3034                                                                        
         TM    C0188+(L'C0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0188,=16C'0'                                                    
         PACK  DUB,C0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1081,FULL+(4-L'K1081)                                           
* ZW3539                                                                        
         TM    C0197+(L'C0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0197,=16C'0'                                                    
         PACK  DUB,C0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1085,FULL+(4-L'K1085)                                           
* ZW4044                                                                        
         TM    C0206+(L'C0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0206,=16C'0'                                                    
         PACK  DUB,C0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1089,FULL+(4-L'K1089)                                           
* ZW4549                                                                        
         TM    C0215+(L'C0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0215,=16C'0'                                                    
         PACK  DUB,C0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1093,FULL+(4-L'K1093)                                           
* ZW5054                                                                        
         TM    C0224+(L'C0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0224,=16C'0'                                                    
         PACK  DUB,C0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1097,FULL+(4-L'K1097)                                           
* ZW5564                                                                        
         TM    C0233+(L'C0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0233,=16C'0'                                                    
         PACK  DUB,C0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1101,FULL+(4-L'K1101)                                           
* ZW65+                                                                         
         TM    C0242+(L'C0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0242,=16C'0'                                                    
         PACK  DUB,C0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1105,FULL+(4-L'K1105)                                           
* ZM25                                                                          
         TM    C0251+(L'C0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0251,=16C'0'                                                    
         PACK  DUB,C0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1109,FULL+(4-L'K1109)                                           
* ZM68                                                                          
         TM    C0260+(L'C0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0260,=16C'0'                                                    
         PACK  DUB,C0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1113,FULL+(4-L'K1113)                                           
* ZM911                                                                         
         TM    C0269+(L'C0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0269,=16C'0'                                                    
         PACK  DUB,C0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1117,FULL+(4-L'K1117)                                           
* ZM1214                                                                        
         TM    C0278+(L'C0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0278,=16C'0'                                                    
         PACK  DUB,C0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1121,FULL+(4-L'K1121)                                           
* ZM1517                                                                        
         TM    C0287+(L'C0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   C0287,=16C'0'                                                    
         PACK  DUB,C0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1125,FULL+(4-L'K1125)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISCK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
CYKPROC  CSECT                                                                  
         NMOD1 0,CYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING CYKPROC+4096,R2                                                  
         LA    RE,CKYFEND                                                       
         CLI    CKYFRST,1                                                       
         BNE   CKYPROCX                                                         
CKYPROC1   CLI   1(RE),255                                                      
         BE    CKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   CKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,CKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   CKYFRST,0                                                        
CKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   CKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    CKYPROCX                                                         
         B     CKYPROC1                                                         
         SPACE 2                                                                
CKYFRST    DC    X'01'                                                          
CKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
CKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DKPROC   CSECT                                                                  
         NMOD1 0,DKPROC                                                         
         USING KDSCT,R6                                                         
         USING DDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING DKPROC+4096,R2                                                   
* ZM1820                                                                        
         TM    D0116+(L'D0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0116,=16C'0'                                                    
         PACK  DUB,D0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1129,FULL+(4-L'K1129)                                           
* ZM2124                                                                        
         TM    D0125+(L'D0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0125,=16C'0'                                                    
         PACK  DUB,D0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1133,FULL+(4-L'K1133)                                           
* ZM2529                                                                        
         TM    D0134+(L'D0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0134,=16C'0'                                                    
         PACK  DUB,D0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1137,FULL+(4-L'K1137)                                           
* ZM3034                                                                        
         TM    D0143+(L'D0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0143,=16C'0'                                                    
         PACK  DUB,D0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1141,FULL+(4-L'K1141)                                           
* ZM3539                                                                        
         TM    D0152+(L'D0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0152,=16C'0'                                                    
         PACK  DUB,D0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1145,FULL+(4-L'K1145)                                           
* ZM4044                                                                        
         TM    D0161+(L'D0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0161,=16C'0'                                                    
         PACK  DUB,D0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1149,FULL+(4-L'K1149)                                           
* ZM4549                                                                        
         TM    D0170+(L'D0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0170,=16C'0'                                                    
         PACK  DUB,D0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1153,FULL+(4-L'K1153)                                           
* ZM5054                                                                        
         TM    D0179+(L'D0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0179,=16C'0'                                                    
         PACK  DUB,D0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1157,FULL+(4-L'K1157)                                           
* ZM5564                                                                        
         TM    D0188+(L'D0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0188,=16C'0'                                                    
         PACK  DUB,D0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1161,FULL+(4-L'K1161)                                           
* ZM65+                                                                         
         TM    D0197+(L'D0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0197,=16C'0'                                                    
         PACK  DUB,D0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1165,FULL+(4-L'K1165)                                           
* ZWMOMS                                                                        
         TM    D0206+(L'D0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0206,=16C'0'                                                    
         PACK  DUB,D0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1169,FULL+(4-L'K1169)                                           
* ZWW1820                                                                       
         TM    D0224+(L'D0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0224,=16C'0'                                                    
         PACK  DUB,D0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1173,FULL+(4-L'K1173)                                           
* ZWW2124                                                                       
         TM    D0233+(L'D0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0233,=16C'0'                                                    
         PACK  DUB,D0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1177,FULL+(4-L'K1177)                                           
* ZWW2534                                                                       
         TM    D0242+(L'D0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0242,=16C'0'                                                    
         PACK  DUB,D0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1181,FULL+(4-L'K1181)                                           
* ZWW3544                                                                       
         TM    D0251+(L'D0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0251,=16C'0'                                                    
         PACK  DUB,D0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1185,FULL+(4-L'K1185)                                           
* ZWW4549                                                                       
         TM    D0260+(L'D0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0260,=16C'0'                                                    
         PACK  DUB,D0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1189,FULL+(4-L'K1189)                                           
* ZWW5054                                                                       
         TM    D0269+(L'D0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0269,=16C'0'                                                    
         PACK  DUB,D0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1193,FULL+(4-L'K1193)                                           
* ZWW55+                                                                        
         TM    D0278+(L'D0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   D0278,=16C'0'                                                    
         PACK  DUB,D0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K1197,FULL+(4-L'K1197)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISDK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
DYKPROC  CSECT                                                                  
         NMOD1 0,DYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING DYKPROC+4096,R2                                                  
         LA    RE,DKYFEND                                                       
         CLI    DKYFRST,1                                                       
         BNE   DKYPROCX                                                         
DKYPROC1   CLI   1(RE),255                                                      
         BE    DKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   DKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,DKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   DKYFRST,0                                                        
DKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   DKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    DKYPROCX                                                         
         B     DKYPROC1                                                         
         SPACE 2                                                                
DKYFRST    DC    X'01'                                                          
DKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
DKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EKPROC   CSECT                                                                  
         NMOD1 0,EKPROC                                                         
         USING KDSCT,R6                                                         
         USING EDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING EKPROC+4096,R2                                                   
* RHOMES                                                                        
         TM    E0150+(L'E0150-1),X'F0'                                          
         BO    *+10                                                             
         MVC   E0150,=16C'0'                                                    
         PACK  DUB,E0150                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    E0141+(L'E0141-1),X'F0'                                          
         BO    *+10                                                             
         MVC   E0141,=16C'0'                                                    
         PACK  DUB,E0141                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISEK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
EYKPROC  CSECT                                                                  
         NMOD1 0,EYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING EYKPROC+4096,R2                                                  
         LA    RE,EKYFEND                                                       
         CLI    EKYFRST,1                                                       
         BNE   EKYPROCX                                                         
EKYPROC1   CLI   1(RE),255                                                      
         BE    EKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   EKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,EKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   EKYFRST,0                                                        
EKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   EKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    EKYPROCX                                                         
         B     EKYPROC1                                                         
         SPACE 2                                                                
EKYFRST    DC    X'01'                                                          
EKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
EKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FKPROC   CSECT                                                                  
         NMOD1 0,FKPROC                                                         
         USING KDSCT,R6                                                         
         USING FDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING FKPROC+4096,R2                                                   
* RHOMES                                                                        
         TM    F0346+(L'F0346-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0346,=16C'0'                                                    
         PACK  DUB,F0346                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    F0337+(L'F0337-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0337,=16C'0'                                                    
         PACK  DUB,F0337                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    F0363+(L'F0363-1),X'F0'                                          
         BO    *+10                                                             
         MVC   F0363,=16C'0'                                                    
         PACK  DUB,F0363                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BAL   R9,PRECISFK                                                      
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISFK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
FYKPROC  CSECT                                                                  
         NMOD1 0,FYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING FYKPROC+4096,R2                                                  
         LA    RE,FKYFEND                                                       
         CLI    FKYFRST,1                                                       
         BNE   FKYPROCX                                                         
FKYPROC1   CLI   1(RE),255                                                      
         BE    FKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   FKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,FKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   FKYFRST,0                                                        
FKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   FKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    FKYPROCX                                                         
         B     FKYPROC1                                                         
         SPACE 2                                                                
FKYFRST    DC    X'01'                                                          
FKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
FKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
GKPROC   CSECT                                                                  
         NMOD1 0,GKPROC                                                         
         USING KDSCT,R6                                                         
         USING GDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING GKPROC+4096,R2                                                   
* RHOMES                                                                        
         TM    G0189+(L'G0189-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0189,=16C'0'                                                    
         PACK  DUB,G0189                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    G0180+(L'G0180-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0180,=16C'0'                                                    
         PACK  DUB,G0180                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
* SHOMES                                                                        
         TM    G0206+(L'G0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   G0206,=16C'0'                                                    
         PACK  DUB,G0206                                                        
         CVB   RF,DUB                                                           
         MVC   HALF,=H'01'                                                      
         BAL   R9,PRECISGK                                                      
         ST    RF,FULL                                                          
         MVC   K0541,FULL+(4-L'K0541)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISGK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
GYKPROC  CSECT                                                                  
         NMOD1 0,GYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING GYKPROC+4096,R2                                                  
         LA    RE,GKYFEND                                                       
         CLI    GKYFRST,1                                                       
         BNE   GKYPROCX                                                         
GKYPROC1   CLI   1(RE),255                                                      
         BE    GKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   GKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,GKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   GKYFRST,0                                                        
GKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   GKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    GKYPROCX                                                         
         B     GKYPROC1                                                         
         SPACE 2                                                                
GKYFRST    DC    X'01'                                                          
GKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
GKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
HKPROC   CSECT                                                                  
         NMOD1 0,HKPROC                                                         
         USING KDSCT,R6                                                         
         USING HDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING HKPROC+4096,R2                                                   
* YW25                                                                          
         TM    H0116+(L'H0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0116,=16C'0'                                                    
         PACK  DUB,H0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0193,FULL+(4-L'K0193)                                           
* YW68                                                                          
         TM    H0125+(L'H0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0125,=16C'0'                                                    
         PACK  DUB,H0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0197,FULL+(4-L'K0197)                                           
* YW911                                                                         
         TM    H0134+(L'H0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0134,=16C'0'                                                    
         PACK  DUB,H0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0201,FULL+(4-L'K0201)                                           
* YW1214                                                                        
         TM    H0143+(L'H0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0143,=16C'0'                                                    
         PACK  DUB,H0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0205,FULL+(4-L'K0205)                                           
* YW1517                                                                        
         TM    H0152+(L'H0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0152,=16C'0'                                                    
         PACK  DUB,H0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0209,FULL+(4-L'K0209)                                           
* YW1820                                                                        
         TM    H0161+(L'H0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0161,=16C'0'                                                    
         PACK  DUB,H0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0213,FULL+(4-L'K0213)                                           
* YW2124                                                                        
         TM    H0170+(L'H0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0170,=16C'0'                                                    
         PACK  DUB,H0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0217,FULL+(4-L'K0217)                                           
* YW2529                                                                        
         TM    H0179+(L'H0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0179,=16C'0'                                                    
         PACK  DUB,H0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0221,FULL+(4-L'K0221)                                           
* YW3034                                                                        
         TM    H0188+(L'H0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0188,=16C'0'                                                    
         PACK  DUB,H0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0225,FULL+(4-L'K0225)                                           
* YW3539                                                                        
         TM    H0197+(L'H0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0197,=16C'0'                                                    
         PACK  DUB,H0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0229,FULL+(4-L'K0229)                                           
* YW4044                                                                        
         TM    H0206+(L'H0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0206,=16C'0'                                                    
         PACK  DUB,H0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0233,FULL+(4-L'K0233)                                           
* YW4549                                                                        
         TM    H0215+(L'H0215-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0215,=16C'0'                                                    
         PACK  DUB,H0215                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0237,FULL+(4-L'K0237)                                           
* YW5054                                                                        
         TM    H0224+(L'H0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0224,=16C'0'                                                    
         PACK  DUB,H0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0241,FULL+(4-L'K0241)                                           
* YW5564                                                                        
         TM    H0233+(L'H0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0233,=16C'0'                                                    
         PACK  DUB,H0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0245,FULL+(4-L'K0245)                                           
* YW65+                                                                         
         TM    H0242+(L'H0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0242,=16C'0'                                                    
         PACK  DUB,H0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0249,FULL+(4-L'K0249)                                           
* YM25                                                                          
         TM    H0251+(L'H0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0251,=16C'0'                                                    
         PACK  DUB,H0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0253,FULL+(4-L'K0253)                                           
* YM68                                                                          
         TM    H0260+(L'H0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0260,=16C'0'                                                    
         PACK  DUB,H0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0257,FULL+(4-L'K0257)                                           
* YM911                                                                         
         TM    H0269+(L'H0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0269,=16C'0'                                                    
         PACK  DUB,H0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0261,FULL+(4-L'K0261)                                           
* YM1214                                                                        
         TM    H0278+(L'H0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0278,=16C'0'                                                    
         PACK  DUB,H0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0265,FULL+(4-L'K0265)                                           
* YM1517                                                                        
         TM    H0287+(L'H0287-1),X'F0'                                          
         BO    *+10                                                             
         MVC   H0287,=16C'0'                                                    
         PACK  DUB,H0287                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0269,FULL+(4-L'K0269)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISHK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
HYKPROC  CSECT                                                                  
         NMOD1 0,HYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING HYKPROC+4096,R2                                                  
         LA    RE,HKYFEND                                                       
         CLI    HKYFRST,1                                                       
         BNE   HKYPROCX                                                         
HKYPROC1   CLI   1(RE),255                                                      
         BE    HKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   HKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,HKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   HKYFRST,0                                                        
HKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   HKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    HKYPROCX                                                         
         B     HKYPROC1                                                         
         SPACE 2                                                                
HKYFRST    DC    X'01'                                                          
HKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
HKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IKPROC   CSECT                                                                  
         NMOD1 0,IKPROC                                                         
         USING KDSCT,R6                                                         
         USING IDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING IKPROC+4096,R2                                                   
* YM1820                                                                        
         TM    I0116+(L'I0116-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0116,=16C'0'                                                    
         PACK  DUB,I0116                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0273,FULL+(4-L'K0273)                                           
* YM2124                                                                        
         TM    I0125+(L'I0125-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0125,=16C'0'                                                    
         PACK  DUB,I0125                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0277,FULL+(4-L'K0277)                                           
* YM2529                                                                        
         TM    I0134+(L'I0134-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0134,=16C'0'                                                    
         PACK  DUB,I0134                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0281,FULL+(4-L'K0281)                                           
* YM3034                                                                        
         TM    I0143+(L'I0143-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0143,=16C'0'                                                    
         PACK  DUB,I0143                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0285,FULL+(4-L'K0285)                                           
* YM3539                                                                        
         TM    I0152+(L'I0152-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0152,=16C'0'                                                    
         PACK  DUB,I0152                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0289,FULL+(4-L'K0289)                                           
* YM4044                                                                        
         TM    I0161+(L'I0161-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0161,=16C'0'                                                    
         PACK  DUB,I0161                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0293,FULL+(4-L'K0293)                                           
* YM4549                                                                        
         TM    I0170+(L'I0170-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0170,=16C'0'                                                    
         PACK  DUB,I0170                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0297,FULL+(4-L'K0297)                                           
* YM5054                                                                        
         TM    I0179+(L'I0179-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0179,=16C'0'                                                    
         PACK  DUB,I0179                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0301,FULL+(4-L'K0301)                                           
* YM5564                                                                        
         TM    I0188+(L'I0188-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0188,=16C'0'                                                    
         PACK  DUB,I0188                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0305,FULL+(4-L'K0305)                                           
* YM65+                                                                         
         TM    I0197+(L'I0197-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0197,=16C'0'                                                    
         PACK  DUB,I0197                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0309,FULL+(4-L'K0309)                                           
* YWMOMS                                                                        
         TM    I0206+(L'I0206-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0206,=16C'0'                                                    
         PACK  DUB,I0206                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0313,FULL+(4-L'K0313)                                           
* YWW1820                                                                       
         TM    I0224+(L'I0224-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0224,=16C'0'                                                    
         PACK  DUB,I0224                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0317,FULL+(4-L'K0317)                                           
* YWW2124                                                                       
         TM    I0233+(L'I0233-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0233,=16C'0'                                                    
         PACK  DUB,I0233                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0321,FULL+(4-L'K0321)                                           
* YWW2534                                                                       
         TM    I0242+(L'I0242-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0242,=16C'0'                                                    
         PACK  DUB,I0242                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0325,FULL+(4-L'K0325)                                           
* YWW3544                                                                       
         TM    I0251+(L'I0251-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0251,=16C'0'                                                    
         PACK  DUB,I0251                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0329,FULL+(4-L'K0329)                                           
* YWW4549                                                                       
         TM    I0260+(L'I0260-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0260,=16C'0'                                                    
         PACK  DUB,I0260                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0333,FULL+(4-L'K0333)                                           
* YWW5054                                                                       
         TM    I0269+(L'I0269-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0269,=16C'0'                                                    
         PACK  DUB,I0269                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0337,FULL+(4-L'K0337)                                           
* YWW55+                                                                        
         TM    I0278+(L'I0278-1),X'F0'                                          
         BO    *+10                                                             
         MVC   I0278,=16C'0'                                                    
         PACK  DUB,I0278                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0341,FULL+(4-L'K0341)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISIK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
IYKPROC  CSECT                                                                  
         NMOD1 0,IYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING IYKPROC+4096,R2                                                  
         LA    RE,IKYFEND                                                       
         CLI    IKYFRST,1                                                       
         BNE   IKYPROCX                                                         
IKYPROC1   CLI   1(RE),255                                                      
         BE    IKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   IKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,IKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   IKYFRST,0                                                        
IKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   IKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    IKYPROCX                                                         
         B     IKYPROC1                                                         
         SPACE 2                                                                
IKYFRST    DC    X'01'                                                          
IKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
IKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
JKPROC   CSECT                                                                  
         NMOD1 0,JKPROC                                                         
         USING KDSCT,R6                                                         
         USING JDSCT,R7                                                         
         USING ZDSCT,R5                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING JKPROC+4096,R2                                                   
* RHOMES                                                                        
         TM    J0189+(L'J0189-1),X'F0'                                          
         BO    *+10                                                             
         MVC   J0189,=16C'0'                                                    
         PACK  DUB,J0189                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0533,FULL+(4-L'K0533)                                           
* IHOMES                                                                        
         TM    J0180+(L'J0180-1),X'F0'                                          
         BO    *+10                                                             
         MVC   J0180,=16C'0'                                                    
         PACK  DUB,J0180                                                        
         CVB   RF,DUB                                                           
         ST    RF,FULL                                                          
         MVC   K0537,FULL+(4-L'K0537)                                           
         XMOD1 1                                                                
         SPACE 2                                                                
PRECISJK LH    RE,HALF                                                          
         LPR   RE,RE                                                            
         LA    R0,1                                                             
         MH    R0,=H'10'                                                        
         BCT   RE,*-4                                                           
         SR    RE,RE                                                            
         TM    HALF,X'80'                                                       
         BO    *+8                                                              
         MR    RE,R0                                                            
         BR    R9                                                               
         LPR   RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,FULL                                                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         DS    H                                                                
         LTORG                                                                  
         EJECT                                                                  
JYKPROC  CSECT                                                                  
         NMOD1 0,JYKPROC                                                        
         USING KDSCT,R6                                                         
         USING ZDSCT,R7                                                         
         LR    R2,RB                                                            
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING JYKPROC+4096,R2                                                  
         LA    RE,JKYFEND                                                       
         CLI    JKYFRST,1                                                       
         BNE   JKYPROCX                                                         
JKYPROC1   CLI   1(RE),255                                                      
         BE    JKYPROCX                                                         
         LH    RF,2(RE)                                                         
         CLI   0(RE),3                                                          
         BE    *+8                                                              
         CLI   0(RE),1                                                          
         BNE   JKYPROC2                                                         
         LR    R3,R6                                                            
         CLI   1(RE),1                                                          
         BNE   *+6                                                              
         LR    R3,R7                                                            
         CLI   1(RE),4                                                          
         BNE   *+8                                                              
         LA    R3,JKYSTRT                                                       
         AR    RF,R3                                                            
         ST    RF,FULL                                                          
         MVC   1(3,RE),FULL+1                                                   
         MVI   JKYFRST,0                                                        
JKYPROC2   LA    RE,4(RE)                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   JKYPROC1                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    JKYPROCX                                                         
         B     JKYPROC1                                                         
         SPACE 2                                                                
JKYFRST    DC    X'01'                                                          
JKYSTRT    DS    0F                                                             
         DC    F'10'                                                            
         DC    F'100'                                                           
         DC    F'1000'                                                          
JKYPROCX  DS    0H                                                              
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        DEDEMFILE                                                      02866   
         PRINT OFF                                                      02867   
*          DATA SET DEDEMFILE  AT LEVEL 054 AS OF 09/03/99                      
********************************************************************    00002*11
*  BECAUSE RADIO STATIONS CAN BE DEFINED AS HOME TO MORE THAN ONE  *    00003*11
*  MARKET YOU MUST USE THE MLHOME/BSHOME/DRHOME FIELD TO DETERMINE *    00004*11
*  WHETHER A RADIO STATION IS HOME TO A MARKET OR WHETHER IT SPILLS*    00005*11
*  INTO THAT MARKET. ON ALL TV FILES THIS MUST BE DETERMINED BY    *    00006*11
*  THE EXISTANCE OF A SPILL MARKET NUMBER. SPILL STATIONS HAVE ONE *    00007*11
*  HOME STATIONS DO NOT.                                           *    00008*11
********************************************************************    00009*11
*<X>DEMO SYSTEM KEYS                                                    00010*13
*        <   > DSECT TO COVER MARKET NAME RECORDS                       00011*13
*                                                                       00012   
DMKEY    DSECT                                                          00013   
DMKMAJOR DS    0CL18                                                    00014   
DMCODE   DS    CL1                 RECORD TYPE                          00015   
DMCODEQU EQU   C'A'                                                     00016   
DMMEDIA  DS    CL1                 MEDIA                                00017   
DMSRC    DS    CL1                 SOURCE                               00018   
         DS    XL15                                                     00019   
DMKSTAT  DS    XL1                 KEY STATUS                           00020   
DMNDXDA  DS    XL4                 INDEX D/A                            00021   
         ORG   DMKSTAT                                                  00022   
DMMINOR  DS    0XL2                                                     00023   
DMRMKT   DS    XL2                 RATING SERVICE MARKET                00024   
DMRLEN   DS    XL2                 RECORD LENGTH                        00025   
DMRSTAT  DS    XL1                 RECORD STATUS                        00026   
DMFRSTEL DS    0C                                                       00027   
*                                  X'01' MARKET NAME ELEMENT            00028   
         EJECT                                                          00029   
*        <   > DSECT TO COVER NETWORK CORRECTIONS (PASSIVE)             00030*24
*                                                                       00031   
CHKEY    DSECT                                                          00032*22
CHKMAJOR DS    0XL18                                                    00033*22
CHCODE   DS    CL1                 RECORD TYPE                          00034*22
CHCODEQU EQU   C'C'                                                     00035*22
CHMEDIA  DS    CL1                 MEDIA                                00036*22
CHSRC    DS    CL1                 SOURCE                               00037*22
CHOBOOK  DS    XL2                 ORIGINATING BOOK (NEW INFO)          00038*24
CHSTAT   DS    CL5                 STATION                              00039*22
CHBTYP   DS    XL1                 BOOK TYPE                            00040*22
CHDW     DS    XL1                 DAY/WEEK                             00041*22
CHSTIM   DS    XL1                 START QUARTER HOUR                   00042*22
CHPNUM   DS    XL2                 PROGRAM NUMBER                       00043*22
CHRTYP   DS    CL1                 RECORD TYPE (P OR Q)                 00044*22
CHRBOOK  DS    XL2                 REVISION BOOK (OLD INFO)             00045*24
CHKSTAT  DS    XL1                 KEY STATUS                           00046*22
CHNDXDA  DS    XL4                 INDEX D/A                            00047*22
         SPACE 2                                                        00048*22
*        <   > DSECT TO COVER STATION EQUIVALENCE RECORDS (PASSIVE)     00049*32
DSEKEY   DSECT                                                          00050*32
DSEKMAJ  DS   0XL18               L' MAJOR KEYXL18                      00051*32
DSECODE  DS    CL1                 RECORD TYPE                          00052*33
DSECDEQU EQU   C'E'                                                     00053*33
DSEMEDIA DS    CL1                 MEDIA                                00054*33
DSESRC   DS    CL1                 SOURCE                               00055*32
DSEIND   DS    XL1                 INDICATOR                            00056*44
DSEIBKLW EQU   X'00'                BOOK LOW (IN KEY)                   00057*44
DSEIBKHI EQU   X'01'                BOOK HIGH (IN KEY)                  00058*44
DSEISTID EQU   X'03'                STATION ID RECORD                   00059*54
*                                  BOOK LOW                             00060*44
DSEOLDCL DS    CL5                 OLD CALL LETTERS                     00061*32
DSENEWCL DS    CL5                 NEW CALL LETTERS                     00062*32
DSEEFFBK DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00063*32
         ORG   DSEOLDCL            BOOK HIGH                            00064*44
DSEBKEFF DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00065*44
DSECLOLD DS    CL5                 OLD CALL LETTERS                     00066*44
DSECLNEW DS    CL5                 NEW CALL LETTERS                     00067*44
         DS    CL2                                                      00068*44
*                                                                       00069*54
         ORG   DSEOLDCL            DSEIND = DSEISTID                    00070*54
DSESTNTY DS     XL1                 STATION TYPE (BINARY)               00071*54
DSESTNID DS     XL3                 STATION ID   (BINARY)               00072*54
DSESBK   DS     XL2                 BOOK WHEN CALL LETTERS IN EFFECT    00073*54
DSESCALL DS     CL5                 STATION CALL LETTERS                00074*54
DSESMKT  DS     XL2                 MARKET                              00075*54
DSESHOME DS     CL1                 RADIO HOME/SPILL INDICATOR          00076*54
*                                                                       00077*54
DSEKSTAT DS    XL1                 KEY STATUS                           00078*32
DSENDXDA DS    XL4                 INDEX D/A                            00079*32
DSERECLQ EQU   *-DSEKEY                                                 00080*34
*        <   > DSECT TO COVER STATION LIST RECORDS (PASSIVE)            00081*22
*                                                                       00082*22
BSKEY    DSECT                                                          00083   
BSKMAJOR DS    0XL18                                                    00084   
BSCODE   DS    CL1                 RECORD TYPE                          00085   
BSCODEQU EQU   C'M'                                                     00086   
BSMEDIA  DS    CL1                 MEDIA                                00087   
BSSRC    DS    CL1                 SOURCE                               00088   
BSBOOK   DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00089   
BSIND    DS    XL1                 STATION LIST RECORD INDICATOR        00090   
BSINDEQU EQU   X'02'                                                    00091   
BSSTAT   DS    CL5                 STATION                              00092   
BSKMKT   DS    XL2                 SPILL MARKET                         00093   
BSSTYP   DS    XL1                 STATION TYPE                         00094   
BSBTYP   DS    XL1                 BOOK TYPE                            00095   
BSRMKT   DS    XL2                 RATING SERVICE MARKET                00096   
BSHOME   DS    XL1                 RADIO HOME STATION INDICATOR H/S     00097*11
BSKSTAT  DS    XL1                 KEY STATUS                           00098   
BSNDXDA  DS    XL4                 INDEX D/A                            00099   
         EJECT                                                          00100*24
*        <   > DSECT TO COVER MARKET LIST RECORDS (PASSIVE)             00101*13
*                                                                       00102   
MLKEY    DSECT                                                          00103   
MLKMAJOR DS    0XL18                                                    00104   
MLCODE   DS    XL1                 RECORD TYPE                          00105   
MLCODEQU EQU   C'M'                                                     00106   
MLMEDIA  DS    CL1                 MEDIA                                00107   
MLSRC    DS    CL1                 SOURCE                               00108   
MLBOOK   DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00109   
MLIND    DS    XL1                 MARKET LIST RECORD INDICATOR         00110   
MLINDEQU EQU   X'00'                                                    00111   
MLRMKT   DS    XL2                 RATING SERVICE MARKET                00112   
MLSTAT   DS    CL5                 STATION                              00113   
MLKMKT   DS    XL2                 SPILL MARKET                         00114   
MLSTYP   DS    XL1                 STATION TYPE                         00115   
MLBTYP   DS    XL1                 BOOK TYPE                            00116   
MLHOME   DS    XL1                 RADIO HOME STATION INDICATOR H/S     00117*11
MLKSTAT  DS    XL1                 KEY STATUS                           00118   
MLNDXDA  DS    XL4                 INDEX D/A                            00119   
         EJECT                                                          00120*24
*        <   > DSECT TO COVER NETWORK DAY/TIME RECORD (PASSIVE)         00121*13
*                                                                       00122   
PNKEY    DSECT                                                          00123   
PNKMAJOR DS    0CL18                                                    00124   
PNCODE   DS    CL1                 RECORD TYPE                          00125   
PNCODEQU EQU   C'N'                                                     00126   
PNMEDIA  DS    CL1                 MEDIA                                00127   
PNSRC    DS    CL1                 SOURCE                               00128   
PNIND    DS    XL1                 NETWORK DAY/TIME POINTER INDICATOR   00129   
PNINDEQU EQU   X'00'                                                    00130   
PNBTYP   EQU   PNIND               BOOK TYPE (REPLACES PNINDEQU)        00131**8
PNBOOK   DS    XL2                 BOOK (BINARY YYMM)                   00132   
PNSTAT   DS    CL5                 STATION (NETWORK)                    00133   
PNDW     DS    XL1                 DAY/WEEK                             00134   
PNSTIM   DS    XL1                 START QUARTER HOUR                   00135   
PNPNUM   DS    XL2                 PROGRAM NUMBER                       00136   
PNACTDAY DS    XL1                 ACTUAL DAY                           00137   
PNACTDUR DS    XL1                 ACTUAL DURATION (MINUTES)            00138   
PNBSIND  DS    CL1                 BRKOUT/SPCIAL/REG=0 INDICATOR        00139*49
PNKSTAT  DS    XL1                 KEY STATUS                           00140   
PNNDXDA  DS    XL4                 INDEX D/A                            00141   
         EJECT                                                          00142   
*        <   > DSECT TO COVER PROGRAM AVERAGES DEMOGRAPHIC RECORDS      00143*13
*                                                                       00144   
PRKEY    DSECT                                                          00145   
PRKMAJOR DS    0XL18                                                    00146   
PRCODE   DS    CL1                 RECORD TYPE                          00147   
PRCODEQU EQU   C'P'                                                     00148   
PRMEDIA  DS    CL1                 MEDIA                                00149   
PRMED    EQU   PRMEDIA                                                  00150   
PRSRC    DS    CL1                 SOURCE                               00151   
PRSTAT   DS    CL5                 STATION                              00152   
PRKMKT   DS    XL2                 SPILL MARKET                         00153   
PRBOOK   DS    XL2                 BOOK (BINARY YYMM)                   00154   
PRSTYP   DS    XL1                 STATION TYPE                         00155   
PRBTYP   DS    XL1                 BOOK TYPE                            00156   
         DS    XL4                                                      00157   
PRKSTAT  DS    XL1                 KEY STATUS                           00158   
PRNDXDA  DS    XL4                 INDEX D/A                            00159   
         ORG   PRKSTAT                                                  00160   
PRKMINOR DS    0XL2                                                     00161   
PRSTIM   DS    XL1                 STRT QTR HOUR                        00162   
PRDW     DS    XL1                 DAY OF WEEK                          00163   
PRRLEN   DS    XL2                 RECORD LENGTH                        00164   
PRRSTAT  DS    XL1                 RECORD STATUS                        00165   
PRFRSTEL DS    0C                                                       00166   
*                                  X'01' MARKET TYPE ELEMENT            00167   
*                                  X'10' PROGRAM TYPE ELEMENT           00168   
*                                  X'20' DAY/QTR HOUR ELEMENT           00169   
*                                  X'21' PROGRAM NAME ELEMENT           00170   
*                                  X'24' EPISODE TITLE ELEMENT          00171*24
*                                  X'25' CORRECTION ELEMENT             00172*24
*                                  X'VV' DEMOGRAPHIC ELEMENTS           00173   
         EJECT                                                          00174   
*        <   > DSECT TO COVER NETWORK PROGRAM RECORDS                   00175*13
*                                                                       00176   
PMKEY    DSECT                                                          00177   
PMKMAJOR DS    0CL18                                                    00178   
PMCODE   DS    CL1                 RECORD TYPE                          00179   
PMCODEQU EQU   C'Q'                                                     00180   
PMMEDIA  DS    CL1                 MEDIA                                00181   
PMSRC    DS    CL1                 SOURCE                               00182   
PMBOOK   DS    XL2                 BOOK                                 00183   
PMSTAT   DS    CL5                 STATION                              00184   
PMSTYP   DS    XL1                 STATION TYPE                         00185**7
PMBTYP   DS    XL1                 BOOK TYPE                            00186**7
         DS    XL6                                                      00187**7
PMKSTAT  DS    XL1                 KEY STATUS                           00188   
PMNDXDA  DS    XL4                 INDEX D/A                            00189   
         ORG   PMKSTAT                                                  00190   
PMKMINOR DS    0XL2                                                     00191   
PMPNUM   DS    XL2                 PROGRAM NUMBER                       00192   
PMRLEN   DS    XL2                 RECORD LENGTH                        00193   
PMRSTAT  DS    XL1                 RECORD STATUS                        00194   
PMDATA   DS    0C                                                       00195   
*                                  X'01' MARKET TYPE ELEMENT            00196   
*                                  X'10' NETWORK PROGRAM TYPE ELEMENT   00197   
*                                  X'20' DAY/QTR HOUR ELEMENT           00198   
*                                  X'21' PROGRAM NAME ELEMENT           00199   
*                                  X'22' NETW PROG RUN TIME ELEMENT     00200   
*                                  X'VV' DEMOGRAPHIC ELEMENTS           00201   
         EJECT                                                          00202   
*              DSECT TO COVER DEMOGRAPHIC RECORDS                       00203   
*        <   >                                                          00204*13
DRKEY    DSECT                                                          00205   
DRKMAJOR DS    0XL18                                                    00206   
DRCODE   DS    CL1                 RECORD TYPE                          00207   
DRCODEQU EQU   C'R'                                                     00208   
DRMEDIA  DS    CL1                 MEDIA                                00209   
DRSRC    DS    CL1                 SOURCE                               00210   
DRSTAT   DS    CL5                 STATION                              00211   
DRBOOK   DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00212   
DRKMKT   DS    XL2                 SPILL MARKET                         00213*15
DRSTYP   DS    XL1                 STATION TYPE                         00214   
DRBTYP   DS    XL1                 BOOK TYPE                            00215   
DRHOME   DS    XL1                 RADIO HOME INDICATOR H/S             00216*11
         DS    XL3                                                      00217*11
DRKSTAT  DS    XL1                 KEY STATUS                           00218   
DRNDXDA  DS    XL4                 INDEX D/A                            00219   
         ORG   DRKSTAT                                                  00220   
DRKMINOR DS    0XL2                                                     00221   
DRHIGHD  DS    XL1                 HIGH DAY IN RECORD                   00222   
DRHIQHR  DS    XL1                 HIGH QTR HOUR IN RECORD              00223   
DRRLEN   DS    XL2                 RECORD LENGTH                        00224   
DRRSTAT  DS    XL1                 RECORD STATUS                        00225   
DRFRSTEL DS    0C                                                       00226   
*                                  X'01' MARKET TYPE ELEMENT            00227   
*                                  X'02' SATELLITE STATION ELEMENT      00228   
*                                  X'20' QUARTER HOUR ELEMENT           00229   
*                                  X'21' DAYPART MARKET INFO ELEMENT    00230**2
*                                  X'VV' DEMOGRAPHIC ELEMENTS           00231   
         SPACE 2                                                        00232   
*        <   > DSECT TO COVER STATION/BOOK LIST RECORDS (PASSIVE)       00233*13
*                                                                       00234   
SBKEY    DSECT                                                          00235   
SBKMAJOR DS    0CL18                                                    00236   
SBCODE   DS    CL1                 RECORD TYPE                          00237   
SBCODEQU EQU   C'S'                                                     00238   
SBMEDIA  DS    CL1                 MEDIA                                00239   
SBSRC    DS    CL1                 SOURCE                               00240   
SBSTAT   DS    CL5                 STATION                              00241   
SBKMKT   DS    XL2                 SPILL MARKET                         00242   
SBSTYP   DS    XL1                 STATION TYPE                         00243   
SBBTYP   DS    XL1                 BOOK TYPE                            00244   
SBRMKT   DS    XL2                 RATING SERVICE MARKET NUMBER         00245   
SBBOOK   DS    XL2                 BOOK                                 00246   
SBHOME   DS    XL1                 RADIO HOME STATION INDICATOR H/S     00247*12
         DS    XL1                                                      00248*12
SBKSTAT  DS    XL1                 RECORD STATUS                        00249   
SBNDXDA  DS    XL4                 INDEX D/A                            00250   
         EJECT                                                          00251*15
*        <   > DSECT TO COVER PROGRAM INDEX POINTERS (PASSIVE)          00252*15
*                                                                       00253*15
*        NOTE - WE DO NOT CARRY BOOK TYPE INDICATORS SINCE THE          00254*17
*               PROGRAM MUST REMAIN THE SAME REGARDLESS OF WHAT         00255*17
*               SORT OF AREA WE ARE DEALING WITH. SPILL HOWEVER MUST    00256*17
*               BE CARRIED SINCE THIS COULD ACTUALLY BE A DIFFERENT     00257*17
*               TRANSMITTER. UNLIKELY THAT THERE WOULD BE DIFFERENT     00258*17
*               PROGRAMMING BUT BETTER SAFE THAN SORRY.                 00259*17
PIKEY    DSECT                                                          00260*15
PIKMAJOR DS    0CL18                                                    00261*15
PIKCODE  DS    CL1                 RECORD CODE                          00262*15
PICODEQU EQU   C'I'                                                     00263*15
PIMEDIA  DS    C                   MEDIA                                00264*15
PISRC    DS    C                   SOURCE                               00265*17
PIBOOK   DS    XL2                 BOOK                                 00266*17
PIPNUM   DS    XL3                 RATING SERVICE PROGRAM NUMBER        00267*15
PISTA    DS    XL5                 STATION CALL LETTERS                 00268*48
PIKMKT   DS    XL2                 SPILL MARKET                         00269*17
PIDAY    DS    XL1                 ACTIVE DAYS X'40=MON...01=SUN'       00270*15
*                                              X'80=MULTI-DAY ROTATOR'  00271*17
PISQH    DS    XL1                 EARLIEST POSSIBLE START QH           00272*47
PIEQH    DS    XL1                 LATEST      "      END  QH           00273*48
PISTAT   DS    XL1                 STATUS                               00274*15
PINDXDA  DS    XL4                 INDEX DA                             00275*15
         SPACE 2                                                        00276*38
*        <   > DSECT TO COVER NTI PROGRAM NUMBER EQUATES                00277*37
*                 CONVERTS NTI NUMBERS TO DDS PROGRAM NUMBERS OR        00278*37
*                 DDS PROGRAM NUMBERS TO DDS FILE PROGRAM NUMBERS       00279*37
PJKEY    DSECT                                                          00280*37
PJKMAJOR DS    0CL18                                                    00281*37
PJCODE   DS    CL1                 RECORD TYPE                          00282*37
PJCODEQU EQU   C'J'                                                     00283*37
PJMEDIA  DS    CL1                 MEDIA                                00284*37
PJSRC    DS    CL1                 SOURCE                               00285*37
PJSTAT   DS    CL5                 STATION                              00286*37
PJBOOK   DS    XL2                 BOOK                                 00287*37
PJBTYPE  DS    CL1                 BOOK TYPE IF BOOK <> 0               00288*37
PJEXTNUM DS    XL5      PWOS       EXTERNAL NUMBER                      00289*37
*                                   BOOK = 0  NTI PROGRAM NUMBER        00290*37
*                                   BOOK <> 0 DDS PROGRAM NUMBER        00291*37
PJINTNUM DS    XL2      BIN        INTERNAL NUMBER                      00292*37
*                                   BOOK =  0 DDS PROGRAM NUMBER        00293*37
*                                   BOOK <> 0 DDS FILE NUMBER           00294*37
PJKSTAT  DS    XL1                 RECORD STATUS                        00295*37
PJNDXDA  DS    XL4                 INDEX D/A                            00296*37
         ORG   PJNDXDA                                                  00297*37
         DS    XL1                                                      00298*37
PJRLEN   DS    XL2                 RECORD LENGTH                        00299*37
PJRSTAT  DS    XL1                 RECORD STATUS                        00300*37
*                                                                       00301*37
         EJECT                                                          00302*38
*        <   > DSECT TO COVER PROGRAM NAME RECORD KEY                   00303*18
*                                                                       00304*18
PKKEY    DSECT                                                          00305*18
PKKMAJOR DS    0CL18                                                    00306*18
PKCODE   DS    CL1                 RECORD CODE                          00307*18
PKCODEQU EQU   C'K'                                                     00308*18
PKMEDIA  DS    CL1                 MEDIA                                00309*18
PKSRC    DS    CL1                 SOURCE                               00310*18
PKBTYP   DS    CL1                                                      00311*31
         DS    CL4                                                      00312*31
PKPNUM   DS    CL3                 PROGRAM NUMBER                       00313*18
PKBOOK   DS    CL2                                                      00314*30
PKSTA    DS    CL5                                                      00315*30
PKSTAT   DS    XL1                 STATUS                               00316*18
PKFRSTEL DS    0C                                                       00317*19
*                                  X'01' PROGRAM NAME ELEMENTS          00318*18
         EJECT                                                          00319*18
*        <   > DSECT TO COVER UNIVERSE RECORDS (PASSIVE)                00320*13
*                                                                       00321   
UKEY     DSECT                                                          00322   
UKMAJOR  DS    0XL18                                                    00323   
UCODE    DS    CL1                 RECORD TYPE                          00324   
UCODEQU  EQU   C'U'                                                     00325   
UMEDIA   DS    CL1                 MEDIA                                00326   
USRC     DS    CL1                 SOURCE                               00327   
UMKT     DS    XL2                 RATING SERVICE MARKET                00328   
UBOOK    DS    XL2                 BOOK (XC WITH FF'S FOR BINARY YYMM)  00329   
UBTYP    DS    XL1                 BOOK TYPE                            00330   
UDEMO    DS    XL1                 DEMO NUMBER                          00331   
UUNIV    DS    XL4                 UNIVERSE                             00332   
         DS    CL5                                                      00333   
UKSTAT   DS    XL1                 KEY STATUS                           00334   
UNDXDA   DS    XL4                 INDEX D/A                            00335   
         EJECT                                                          00336   
*<X>DEMO SYSTEM ELEMENTS                                                00337*13
*        <   > DSECT TO COVER MARKET NAME ELEMENT                       00338*13
*                                                                       00339   
DMELEM   DSECT                                                          00340   
DMECODE  DS    XL1                 ELEMENT CODE                         00341   
DMECODEQ EQU   X'01'                                                    00342   
DMLEN    DS    XL1                 ELEMENT LENGTH                       00343   
DMMNO    DS    XL2                 RATING SERVICE MARKET                00344   
DMMNAME  DS    0C                  MARKET NAME (MAX 30)                 00345   
         SPACE 2                                                        00346   
*        <   > DSECT TO COVER PROGRAM NAME ELEMENT                      00347*13
*               (PAV FILE)                                              00348*18
PNELEM   DSECT                                                          00349   
PNECODE  DS    XL1                 ELEMENT CODE                         00350   
PNECODEQ EQU   X'01'                                                    00351   
PNELEN   DS    XL1                 ELEMENT LENGTH                       00352   
PNNAME   DS    0C                  PROGRAM NAME                         00353   
         SPACE 2                                                        00354   
*        <   > DSECT TO COVER PROGRAM NAME ELEMENT (PROGRAM CODE REC)   00355*18
PKELEM   DSECT                                                          00356*18
PKECODE  DS    CL1                 ELEMENT CODE                         00357*18
PKECODEQ EQU   X'01'                                                    00358*18
PKELEN   DS    XL1                 ELEMENT LENGTH                       00359*18
PKFRSTBK DS    XL2                 FIRST ACTIVE BOOK                    00360*18
PKLASTBK DS    XL2                 LAST ACTIVE BOOK                     00361*18
PKSUBPNO DS    CL1                 SUB PROGRAM NUMBER                   00362*18
PKPNAME  DS    0C                  PROGRAM NAME                         00363*18
PKPNMAX  EQU   14                                                       00364*18
         SPACE 2                                                        00365*18
*        <   > DSECT TO COVER MARKET TYPE ELEMENT                       00366*13
*                                                                       00367   
MARELEM  DSECT                                                          00368   
MARCODE  DS    XL1                 ELEMENT CODE                         00369   
MARCODEQ EQU   X'01'                                                    00370   
MARELN   DS    XL1                 ELEMENT LENGTH                       00371   
MARNO    DS    XL2                 RATING SERVICE MARKET                00372   
MARTYPE  DS    XL1                 MARKET TYPE                          00373   
MARSTYP  DS    XL1                 STATION TYPE                         00374   
MARDATE  DS    XL2                 CREATION DATE                        00375   
MARLNEQ  EQU   *-MARELEM                                                00376   
         SPACE 2                                                        00377   
*        <   > DSECT TO COVER S2 SATELLITE STATION ELEMENT              00378*13
*                                                                       00379   
SATELEM  DSECT                                                          00380   
SATCODE  DS    XL1                 ELEMENT CODE                         00381   
SATCODEQ EQU   X'02'                                                    00382   
SATLEN   DS    XL1                 ELEMENT LENGTH                       00383   
SATCALL  DS    0CL5                VARIABLE NUMBER OF STATIONS          00384   
         EJECT                                                          00385*43
*        <   > DSECT TO COVER DPT MARKET INFO RECORD                    00386*13
*                                                                       00387**5
DIELEM   DSECT                                                          00388**5
DIECODE  DS    XL1                 ELEMENT CODE                         00389**5
DIECODEQ EQU   X'03'                                                    00390**5
DILEN    DS    XL1                 ELEMENT LENGTH                       00391**5
DIMCLASS DS    C                   MARKET CLASS                         00392**5
DIMSUBCL DS    C                   MARKET SUBCLASS                      00393**5
DIDMAIND DS    C                   DMA INDICATOR                        00394**5
DITZ     DS    C                   TIME ZONE                            00395**5
DIROUND  DS    C                   ROUNDING CONTROL                     00396**5
DIRSTAT  DS    C                   REPORTABILITY STATUS                 00397**5
DIAFFL   DS    CL3                 NETWORK AFFILIATION                  00398**5
DICHNL   DS    XL1                 CHANNEL                              00399**5
DIADJ1   DS    XL2                 ADJACENT MARKET #1 NUMBER            00400**5
DIADJ2   DS    XL2                 ADJACENT MARKET #2 NUMBER            00401**5
DIADJ3   DS    XL2                 ADJACENT MARKET #3 NUMBER            00402**5
DIELNEQ  EQU   *-DIELEM                                                 00403**5
         SPACE 2                                                        00404*43
*        <   > NTI PROGRAM NUMBERS ALREADY USED FOR NTI/SYN/NHT FILES   00405*53
*                                                                       00406*43
NTIELEM  DSECT                                                          00407*53
NTICODE  DS    XL1                 ELEMENT CODE                         00408*53
NTICODEQ EQU   X'04'                                                    00409*53
NTIELN   DS    XL1                 ELEMENT LENGTH                       00410*53
NTISEQ   DS    XL1                 SEQ# X 250 = START NTI# IN THIS ELEM 00411*53
NTIBITS  DS    XL250               TYPICALLY 7 ELEMS W/250 BYTES        00412*53
NTIELNQ  EQU   *-NTIELEM           FULL ELEM=250 BYTES                  00413*53
         ORG   NTIBITS                                                  00414*53
NTIBITL  DS    XL192               LAST ELEM OF LAST RECD: 192 BYTES    00415*53
NTIELNQL EQU   *-NTIELEM           LENGTH FOR SHORTER ELEM              00416*53
         SPACE 2                                                        00417*43
*        <   > DSECT TO COVER PROGRAM DAYS AND TIMES BITS OF TELECASTS  00418*53
*                                                                       00419*53
TIMELEM  DSECT                                                          00420*53
TIMCODE  DS    XL1                 ELEMENT CODE                         00421*53
TIMCODEQ EQU   X'04'                                                    00422*53
TIMELN   DS    XL1                 ELEMENT LENGTH                       00423*53
TIMBITS  DS    XL96                DAY & TIME BITS FOR PRG'S TELECASTS  00424*53
TIMELNQ  EQU   *-TIMELEM           ELEMENT LENGTH                       00425*53
         SPACE 2                                                        00426*53
*        <   > DSECT FOR CABLE PROGRAMS CNTL RECD                       00427*45
*                                                                       00428*45
CNTELEM  DSECT                                                          00429*45
CNTCODE  DS    XL1                 ELEMENT CODE                         00430*45
CNTCODEQ EQU   X'05'                                                    00431*45
CNTELN   DS    XL1                 ELEMENT LENGTH                       00432*45
CNTTRKD  DS    XL1                 0=UNTRKD EPIS ONLY  1=TRKS EXIST     00433*45
         DS    CL19                SPARE                                00434*45
CNTELNQ  EQU   *-CNTELEM           ELEMENT LENGTH                       00435*45
         SPACE 2                                                        00436*45
*                                                                       00437*51
*        <   > DSECT TO COVER NTI DAILY MESSAGE FILE ELEMENT            00438*51
MSGELEM  DSECT                                                          00439*51
MSGCODE  DS    XL1                 ELEMENT CODE                         00440*51
MSGCODEQ EQU   X'06'                                                    00441*52
MSGELN   DS    XL1                                                      00442*52
MSGLINE  DS    CL75                KEEP 75 CHARS/LINE                   00443*52
MSGELNQ  EQU   *-MSGELEM                                                00444*51
         SPACE 2                                                        00445*51
*                                                                       00446*43
*        <   > DSECT TO COVER PROGRAM IDENTIFIER ELEMENT                00447*37
DIDELEM  DSECT                                                          00448*40
DIDCODE  DS    XL1                 ELEMENT CODE                         00449*40
DIDCODEQ EQU   X'0F'                                                    00450*40
DIDELLN  DS    XL1                 LENGTH                               00451*40
DIDTYPE  DS    CL1                 TYPE OF DEMO INFO:                   00452*40
*                                  0=PROGRAM SUMARY DATA                00453*37
*                                  1=TRACKAGE SUMARY DATA               00454*37
*                                  2=EPISODE DATA                       00455*37
DIDELNEQ EQU   *-DIDELEM           LENGTH OF ID ELEMENT                 00456*40
         SPACE 2                                                        00457*43
*        <   > DSECT TO COVER PROGRAM DAY/TIME/WEEK ELEMENT             00458*13
*                                                                       00459   
PNAELEM  DSECT                                                          00460   
PNACODE  DS    XL1                 ELEMENT CODE                         00461   
PNACODEQ EQU   X'10'                                                    00462   
PNALEN   DS    XL1                 ELEMENT LENGTH                       00463   
PNATYPE  DS    XL1                 TYPE OF DATA                         00464   
*                                   0=NORMAL TIME PERIOD ONLY           00465   
*                                   1=FULL CYCLE                        00466   
PNAPTYP  DS    XL2                 NETWORK PROGRAM TYPE                 00467   
         DS    XL5                 SPARE                                00468   
PNALNEQ  EQU   *-PNAELEM                                                00469   
         EJECT                                                          00470*43
*        <   > DSECT TO COVER NETWORK PROGRAM TYPE ELEMENT              00471*13
*                                                                       00472   
PHTELEM  DSECT                                                          00473   
PHTCODE  DS    XL1                 ELEMENT CODE                         00474   
PHTCODEQ EQU   X'10'                                                    00475   
PHTLEN   DS    XL1                 ELEMENT LENGTH                       00476   
PHTDTYP  DS    CL1                 DATA TYPE (TELECAST TYPE - INDICATES 00477   
*                                  REGULAR/SPECIAL AND ORIGINAL/REPEAT) 00478   
*                                  X'01' = REGULAR                      00479   
*                                  X'04' = SPECIAL                      00480   
*                                  X'08' = ORIGINAL                     00481   
*                                  X'10' = REPEAT                       00482   
*                                  X'20' = MULTIPLE PROGRAMS            00483*27
*                                  X'80' = OPTIONAL INDIVIDUAL DAY      00484*23
PHTPTYP  DS    CL2                 NETWORK PROGRAM TYPE                 00485   
PHTDPT   DS    CL1                 NSI DAYPART (IF ZERO, USE PHTDPT2)   00486*20
PHTPREM  DS    CL1                 PREMEIRE INDICATOR                   00487   
PHTRCH   DS    CL2                 REACH (0,000)                        00488   
PHTSCNT  DS    CL2                 STATION COUNT                        00489   
PHTCOVR  DS    CL2                 COVERAGE                             00490   
PHTRSF   DS    XL1                 REDUCED STATION FACILITIES INDICATOR 00491   
*                                  X'01' = REDUCED FACILITIES           00492   
PHTDPT2  DS    CL2                 NSI DAYPART (USED IF PHTDPT=ZERO)    00493*20
PHTNTI   DS    CL5                 NTI PRG/TRK/EPIS NUMBER(# ON TAPE)   00494*37
PHTDDS   DS    CL2                 DDS INTERNAL PROGRAM NUMBER          00495*37
PHTPTYP4 DS    CL4                 NEW 4-CHAR PROGRAM TYPE ALPHA        00496*37
PHTSPTYP DS    CL4                 SUB PROGRAM TYPE                     00497*37
         DS    CL2                 SPARE                                00498*39
PHTLNEQ  EQU   *-PHTELEM                                                00499   
         SPACE 2                                                        00500   
*        <   > DSECT TO COVER QUARTER HOUR ELEMENT                      00501*13
*                                                                       00502   
QHELEM   DSECT                                                          00503   
QHCODE   DS    XL1                 ELEMENT CODE                         00504   
QHCODEQ  EQU   X'20'                                                    00505   
QHELN    DS    XL1                 ELEMENT LENGTH                       00506   
QHDAY    DS    XL1                 DAY                                  00507   
*                                   BIT 0   = DIFFERENT START-END DAYS  00508   
*                                   BIT 1-3 = START DAY                 00509   
*                                   BIT 4-7 = END DAY                   00510   
QHSQH    DS    XL1                 START QUARTER HOUR                   00511   
QHEQH    DS    XL1                 END QUARTER HOUR                     00512   
QHWKS    DS    XL1                 ACTUAL WEEKS FOR PROGRAM             00513   
*                                   BIT 0-2 = NOT USED                  00514   
*                                   BIT 3   = 2W FOLLOWED BY 4W AVERAGE 00515   
*                                   BIT 4-7 = WEEK 1-4 ACTIVITY         00516**4
*                                             0=INACTIVE, 1=ACTIVE      00517   
*                                             WEEK 1 = X'08'            00518**4
*                                             WEEK 2 = X'04'            00519**4
*                                             WEEK 3 = X'02'            00520**4
*                                             WEEK 4 = X'01'            00521**4
QHPNAME  DS    0C                  PROGRAM NAME (MAX 14)                00522   
         DS    CL14                                                     00523*14
QHPNMAX  EQU   *-QHPNAME           MAXIMUM PROGRAM NAME LENGTH          00524*14
         SPACE 2                                                        00525*29
*        <   > DSECT TO COVER QH PROGRAM INFO ELEMENT                   00526*28
*                EFFECTIVE JAN89 ON NSI TIME PERIOD                     00527*28
QIELEM   DSECT                                                          00528*28
QICODE   DS    XL1                 ELEMENT CODE                         00529*28
QICODEQ  EQU   X'21'                                                    00530*28
QIELN    DS    XL1                 ELEMENT LENGTH                       00531*28
QIPNUM   DS    XL3                 PROGRAM NUMBER FOR NETWORK           00532*28
*                                    AND SYNDICATED PROGRAMS            00533*14
QIPTYPE  DS    CL2                 PROGRAM TYPE                         00534*28
QIPNAM6  DS    CL6                 6 CHARACTER PROGRAM NAME             00535*28
QIAFFIL  DS    CL5                 AFFILIATION CODES                    00536*28
QIELNEQ  EQU   *-QIELEM            ORIGINAL ELEMENT LENGTH              00537*35
* *********************REVISION 1********************************       00538*35
*  EFFECTIVE OCT/93 - CHECK ELEM LENGTH TO SEE IF REV FIELD EXISTS      00539*35
QIREV    DS    XL1                 REVISION NUMBER                      00540*35
QIPRSRC  DS    CL2                 PROGRAMMING SOURCE                   00541*35
QIELNEQ1 EQU   *-QIELEM            REVISION 1 ELEMENT LENGTH            00542*35
* ***************************************************************       00543*35
         EJECT                                                          00544   
*        <   > DSECT TO COVER PAV DAY/QUARTER HOUR ELEMENT              00545*13
*                                                                       00546   
PHELEM   DSECT                                                          00547   
PHCODE   DS    XL1                 ELEMENT CODE                         00548   
PHCODEQ  EQU   X'20'                                                    00549   
PHELN    DS    XL1                 ELEMENT LENGTH                       00550   
PHPNUM   DS    CL2                 PROGRAM NUMBER                       00551   
         ORG   PHPNUM                                                   00552*36
PHREVID  DS    XL1                 REVISION ID FLAG = X'FF'             00553*36
PHREV    DS    XL1                 REVISION NUMBER                      00554*36
PHDUR    DS    CL1                 DURATION IN QUARTER HOURS            00555   
PHPRVST  DS    CL1                 START QTR HR OF PREVIOUS RECORD      00556   
PHPRVDW  DS    CL1                 DAY/WEEK OF PREVIOUS RECORD          00557   
PHDTYPE  DS    CL1                 DATA TYPE                            00558   
*                                   0=NORMAL                            00559   
*                                   1=FULL CYCLE                        00560   
PHDWKS   DS    XL1                 ACTIVE WEEKS                         00561   
*                                   BIT 4   = WEEK 1 (X'08')            00562**4
*                                   BIT 5   = WEEK 2 (X'04')            00563**4
*                                   BIT 6   = WEEK 3 (X'02')            00564**4
*                                   BIT 7   = WEEK 4 (X'01')            00565**4
PHDUPUT  DS    XL1                 0=PUTS, 1=UNIVERSES                  00566   
PHDBOOK  DS    XL2                 ORIGINAL BOOK                        00567   
PHDURTOT DS    CL1                 TOTAL DURATION OF PROGRAM            00568**9
PHELNEQ  EQU   *-PHELEM            ORIGINAL ELEMENT LENGTH              00569*36
*                                                                       00570*36
* --------REVISION ID = X'FF' ONLY------                                00571*36
* REVISION 1 (STARTS WITH OCT/93 BOOK)                                  00572*36
*                                                                       00573*36
PHPNUM3  DS    XL3                 PROGRAM NUMBER FOR NETWORK           00574*36
*                                    AND SYNDICATED PROGRAMS            00575*36
PHPTYPE  DS    CL2                 PROGRAM TYPE                         00576*36
PHPNAM6  DS    CL6                 6 CHARACTER PROGRAM NAME             00577*36
PHAFFIL  DS    CL5                 AFFILIATION CODES                    00578*36
PHPRSRC  DS    CL2                 PROGRAMMING SOURCE                   00579*36
PHELNEQ1 EQU   *-PHELEM            REVISION 1 ELEMENT LENGTH            00580*36
         SPACE 2                                                        00581**2
*        <   > DSECT TO COVER PAV PROGRAM NAME ELEMENT                  00582*13
*                                                                       00583   
PPNELEM  DSECT                                                          00584   
PPNCODE  DS    XL1                 ELEMENT CODE                         00585   
PPNCODEQ EQU   X'21'                                                    00586   
PPNELN   DS    XL1                 ELEMENT LENGTH                       00587   
PPNNME   DS    0C                  PROGRAM NAME                         00588   
         SPACE 2                                                        00589   
*        <   > DSECT TO COVER NETWORK PROGRAM RUN TIME ELEMENT          00590*13
*                                                                       00591   
NTELEM   DSECT                                                          00592   
NTCODE   DS    XL1                 ELEMENT CODE                         00593   
NTCODEQU EQU   X'22'                                                    00594   
NTLEN    DS    XL1                 ELEMENT LENGTH                       00595   
NTSQH    DS    XL1                 START QUARTER HOUR                   00596   
NTEQH    DS    XL1                 END QUARTER HOUR                     00597   
NTDUR    DS    XL1                 DURATION (MINUTES)                   00598   
NTSTIM   DS    XL2                 ACTUAL START TIME (MILITARY)         00599   
NTETIM   DS    XL2                 END TIME (MILITARY)                  00600   
NTDAY    DS    X                   DAY (BINARY) - X'80' = INVALID DATE  00601   
NTTYPE   DS    X                   TELECAST INDICATORS  X'01'=BREAKOUT  00602*41
*                                                       X'02'=SPECIAL   00603*41
NTSLNQ   EQU   *-NTELEM            TELC TIMES  ELEM LENGTH              00604*42
         ORG   NTTYPE                                                   00605*41
NTFEED   DS    C                   FEED PATTERN                         00606*37
NTAUDES  DS    X                   AUDIENCE ESTIMATE TYPE               00607*37
NTTNUM   DS    XL4                 TELECAST NUMBER                      00608*37
NTCOVCL  DS    X                   COVG SAMPLE CALULATION IND           00609*37
NTLIB    DS    XL4                 LIBRARY ID                           00610*37
NTCLTEP  DS    XL4                 CLT SPEC EPISODE NUMBER              00611*37
NTCMCL   DS    CL1                 COMMERCIAL STATUS                    00612*37
NTLIVE   DS    CL1                 LIVE EVENT INDIC                     00613*37
NTPOA    DS    CL1                 PROGRAM ORIGINAL/ACQUIRED CODE       00614*37
NTEOA    DS    CL1                 EPISODE ORIG/ACQUIRED CODE           00615*37
NTGAP    DS    CL1                 GAPPED INDIC                         00616*37
         DS    XL1                 SPARE                                00617*39
NTLENEQ  EQU   *-NTELEM                                                 00618   
         EJECT                                                          00619*24
*        <   > DSECT TO COVER SECTION LEAD ELEMENT                      00620*13
*                                                                       00621**6
SLELEM   DSECT                                                          00622**6
SLCODE   DS    XL1                 ELEMENT CODE                         00623**6
SLCODEQ  EQU   X'23'                                                    00624**6
SLLEN    DS    XL1                 ELEMENT LENGTH (3)                   00625**6
SLSECT   DS    XL1                 DEMO CATEGORY                        00626**6
         SPACE 2                                                        00627   
*        <   > DSECT TO COVER PAV EPISODE TITLE ELEMENT                 00628*21
*                                                                       00629*21
PPTELEM  DSECT                                                          00630*21
PPTCODE  DS    XL1                 ELEMENT CODE                         00631*21
PPTCODEQ EQU   X'24'                                                    00632*21
PPTELN   DS    XL1                 ELEMENT LENGTH                       00633*21
PPTTITLE DS    0C                  EPISODE TITLE                        00634*21
         SPACE 2                                                        00635*21
*        <   > DSECT TO COVER DEMOGRAPHIC RATINGS ELEMENT               00636*13
*                                                                       00637   
DREELEM  DSECT                                                          00638   
DRECODE  DS    XL1                 ELEMENT CODE (VARIABLE)              00639   
DRELEN   DS    XL1                 ELEMENT LENGTH                       00640   
DREDUPA  DS    XL2                 DISP OF DUPLICATE DATA ELEMENT       00641   
*                                  PRESENT IF HIGH ORDER BIT ON         00642   
         ORG   DREDUPA                                                  00643   
DRDATA   DS    0C                  VARIABLE NUMBER OF ONE BYTE FIELDS   00644   
*                                  PRESENT IF LOW ORDER BIT OF CODE OFF 00645   
         ORG   DREDUPA                                                  00646   
DREFCTRL DS    XL1                 FIELD CONTROL                        00647   
*                                   BIT 0   = DUPLICATE ELEMENT FLAG    00648*46
*                                              (SEE DREDUPA)            00649*46
*                                   BIT 1   = IMPLIED DECIMAL           00650*46
*                                   BIT 2   = OFF=MULTIPLY BY 10        00651*46
*                                   BIT 3   = NEGATIVE NUMBERS PRESENT  00652*46
*                                             (IF HOB OF FIELD ON)      00653*46
*                                   BIT 4   = UNUSED                    00654*46
*                                   BIT 5-7 = FIELD LENGTH              00655   
*                                        PRESENT IF LOW ORDER BIT OF    00656*46
*                                        ELEMENT CODE ON FOR MOST FILES 00657*46
*                                        (EXCEPTIONS = NHN FILE)        00658*46
DRDATA1  DS    0C                  VARIABLE NUMBER OF VARIABLE LENGTH   00659   
*                                  FIELDS                               00660   
         SPACE 2                                                        00661*24
*        <   > DSECT TO COVER NETWORK CORRECTION ELEMENT                00662*24
*                                                                       00663*24
CRRELEM  DSECT                                                          00664*24
CRRCODE  DS    XL1                 ELEMENT CODE                         00665*24
CRRCODEQ EQU   X'25'                                                    00666*25
CRRLEN   DS    XL1                 ELEMENT LENGTH                       00667*24
CRRTYPE  DS    XL1                 BITS 0-6 - (UNUSED)                  00668*24
*                                  BIT 7 -  0=ADD 1=CHANGE              00669*24
CRROBOOK DS    XL2                 ORIGINATING BOOK YYWW (NEW INFO)     00670*26
CRRLENEQ EQU   *-CRRELEM                                                00671*24
*                                                                       00672*24
         EJECT                                                          00673   
*        <   > DSECT TO COVER PAV DEMOGRAPHIC RATINGS ELEMENT           00674*13
*                                                                       00675   
PPEELEM  DSECT                                                          00676   
PPECODE  DS    XL1                 ELEMENT CODE (VARIABLE)              00677   
PPELEN   DS    XL1                 ELEMENT LENGTH                       00678   
PPEDUPA  DS    XL2                 DISP OF DUPLICATE DATA ELEMENT       00679   
*                                  PRESENT IF HIGH ORDER BIT ON         00680   
         ORG   PPEDUPA                                                  00681   
PPDATA   DS    0C                  VARIABLE NUMBER OF ONE BYTE FIELDS   00682   
*                                  PRESENT IF LOW ORDER BIT OF CODE OFF 00683   
         ORG   PPEDUPA                                                  00684   
PPEFCTRL DS    XL1                 FIELD CONTROL                        00685   
*                                   BIT 1   = IMPLIED DECIMAL           00686   
*                                   BIT 5-7 = FIELD LENGTH              00687   
*                                  PRESENT IF LOW ORDER BIT OF CODE ON  00688   
PPDATA1  DS    0C                  VARIABLE NUMBER OF VARIABLE LENGTH   00689   
*                                  FIELDS                               00690   
         PRINT ON                                                       02869   
         SPACE 2                                                        02870   
*        DEDEMCNVD                                                      02871   
         PRINT OFF                                                      02872   
*          DATA SET DEDEMCNVD  AT LEVEL 023 AS OF 06/14/99                      
         TITLE 'DEMCON - GLOBAL WORKING STORAGE DSECT'                  00002   
DEMCOND  DSECT                                                          00003   
*                                  TEMPORARY STORAGE                    00004   
DUB      DS    D                                                        00005   
DUB1     DS    D                                                        00006   
FULL     DS    F                                                        00007   
FULL1    DS    F                                                        00008   
HALF     DS    H                                                        00009   
HALF1    DS    H                                                        00010   
DMCB     DS    6F                                                       00011   
DMCB1    DS    6F                                                       00012   
WORK     DS    CL80                                                     00013   
WORK1    DS    CL80                                                     00014   
TEMP     DS    CL256                                                    00015   
*                                  CONTROLLER SAVE STORAGE              00016   
ABASE    DS    A                                                        00017   
BBASE    DS    A                                                        00018   
TODAY    DS    CL8                 TODAY'S DATE (MM/DD/YY)              00019   
TODAYB   DS    XL2                 TODAY'S DATE (COMPRESSED)            00020   
*                                  RUN VALUES                           00021   
MODE     DS    XL1                 RUN MODE                             00022   
RUN      EQU   X'01'               CONVERT TAPE                         00023   
REPORT   EQU   X'02'               REPORT ONLY                          00024   
SOURCE   DS    0CL3                                                     00025   
MEDIA    DS    C                   MEDIA                                00026   
OUTSRC   DS    C                   OUTPUT SOURCE                        00027   
SRCSW    DS    C                   SOURCE CODE                          00028   
NETBKSW  DS    X                   1=NETWORK BOOK                       00029   
SORTWORK DS    C                   NUMBER OF SORT WORK FILES (1-9)      00030   
FILNUM   DS    X                   INPUT TAPE FILE NUMBER (1-9)         00031   
GLOBAL   DS    C                   Y=CANADIAN GLOBAL NETWORK            00032   
FORCE    DS    C                   Y=FORCE BOOK TO FILTBOOK VALUE       00033   
BOOKTYPE DS    C                   SPECIAL BOOK TYPE                    00034   
NRECS    DS    XL4                 NUMBER OF RECORDS TO PROCESS         00035   
RUNTIME  DS    XL4                 MAXIMUM RUN TIME VALUE               00036   
*                                  RECORD PRINTING SPECS                00037   
RPRINT   DS    X,5XL4              RATING SERVICE TAPE RECORD           00038   
IPRINT   DS    X,5XL4              INTERIM RECORD (PRE SORT)            00039   
SPRINT   DS    X,5XL4              INTERIM RECORD (POST SORT)           00040   
WPRINT   DS    X,5XL4              WORK RECORD                          00041   
OPRINT   DS    X,5XL4              OUTPUT TAPE RECORD                   00042   
*                                  PHASE NAMES                          00043   
IPHASE   DS    CL8                 INPUT RECORD PHASE                   00044   
OPHASE   DS    CL8                 OUTPUT RECORD PHASE                  00045   
         EJECT                                                          00046   
*                                  INTERIM RECORD FILTERS               00047   
*  FIRST BYTE OF FILTER FIELDS IS A COUNT OF THE ACTIVE ENTRIES         00048**5
FILTBOOK DS    X,5XL4              BOOK (YYMMYYMM OR YYW1YYW2)          00049   
FILTSTAT DS    X,5XL10             STATION CALL LETTERS                 00050   
FILTWEEK DS    X,2XL2              NETWORK WEEKS                        00051   
         DS    CL45                SPARE                                00052   
FILTMRKT DS    X,10XL2             MARKET NUMBERS                       00053   
FILTSTYP DS    X,5XL1              STATION TYPES                        00054   
FILTTIME DS    X,5XL2              TIMES (START QHR/END QHR)            00055   
FILTRECS DS    X,5CL1              RECORD TYPES                         00056   
*                                  I/O SWITCHES                         00057   
INTAPESW DS    X                   0=CLOSED,1=OPEN,2=EOF(CLOSED)        00058   
OUTAPESW DS    X                   0=CLOSED,1=OPEN,2=FORCE CLOSE        00059   
SORTSW   DS    X                   0=NOT INIT,1=INIT                    00060   
*                                  RECORD/DATA LENGTHS                  00061   
INTALPHL DS    X                   L'ALPHA DATA ON INTERIM RECORD       00062   
INTNUMAC DS    X                   N'ACCUMS ON INTERIM RECORD           00063   
INTRECL  DS    XL2                 L'INTERIM RECORD                     00064   
WORKRECL DS    XL2                 L'WORK RECORD                        00065   
SAVEMRKT DS    XL2                 SAVED MARKET NUMBER                  00066   
*                                                                       00067   
IPATCH   DS    X,5CL23             PATCHES FOR INPUT PHASE              00068   
OPATCH   DS    X,5CL23             PATCHES FOR OUTPUT PHASE             00069   
CPATCH   DS    X,5CL23             PATCHES FOR CONTROLLER               00070   
DBLOCKA  DS    CL256               DEMO LOOK-UP CONTROL BLOCK           00071   
*                                                                       00072**3
HUTPRINT DS    C                   D=DETAIL,S=SUMMARY                   00073**3
PRINTSW  DS    B                   USER PRINT CONTROL SWITCH            00074**3
*                                  X'80'= DO NOT PRINT RECORD           00075**3
INTNMAC2 DS    H                   HALFWORD FOR INTNUMAC                00076**4
XSPILL   DS    C                   XTRA SPILL ACTIVE                    00077**7
BYPSORT  DS    C                   X'80' = BYPASS SORT RECORD           00078**9
ACOMWRK  DS    A                   A(WRK AREA)SHARED BY INP/OUT PHASES  00079*10
*                                  SOME MORE FIELDS FOR JCL'S PARMS     00080*11
KEYONLY  DS    C                   Y=PRINT KEY AND RELEVANT DATA ONLY   00081*11
OPELEM   DS    X,10XL1             ELEMENT CODES OF ELEMS TO PRINT      00082*12
ORIGINAL DS    C                   Y=LOAD AS ORIG DATA--CLEANS OUT WK   00083*14
EOFMSG   DS    C                   N=BYPASS CONSOLE MSG(ASSUME IT'S Y)  00084*16
IPCS     DS    C                   N/Y IF YES, SYSMDUMP=DATASET         00085*21
*                                  FOR NTI DAILIES CONVERSION           00086*14
         ORG   DEMCOND+1500                                             00087   
*                                  INTERNAL ADDRESS DIRECTORY           00088   
AOTAPE   DS    A                   INPUT TAPE DTF                       00089   
APARMTAB DS    A                   PARAMETER TABLE                      00090   
ASRCTAB  DS    A                   SOURCE TABLE                         00091   
ARREC    DS    A                   RATING SERVICE RECORD                00092   
AIREC    DS    A                   INTERIM RECORD (PRE SORT)            00093   
ASREC    DS    A                   INTERIM RECORD (POST SORT)           00094   
AWREC    DS    A                   WORK RECORD                          00095   
AOREC    DS    A                   OUTPUT RECORD                        00096   
APUTTAPE DS    A                   PUT A RECORD TO TAPE                 00097   
APRNTREC DS    A                   PRINT A RECORD                       00098   
ABLDREC  DS    A                   BUILD BASIC OUTPUT RECORD            00099   
APUTEL   DS    A                   ADD AN ELEMENT TO OUTPUT RECORD      00100   
ACOMFACS DS    A                   COMMON FACILITIES LIST               00101   
ASTXITIT DS    A                   STXIT IT ROUTINE                     00102   
ASTXITRG DS    A                   STXIT REGISTERS                      00103   
AIPHASE  DS    A                   INPUT PHASE                          00104   
AOPHASE  DS    A                   OUTPUT PHASE                         00105   
VMKTRANK DS    V                   MARKET RANK TABLE                    00106   
VPUTBUFF DS    V                   RDTPT/MKTAC PUT BUFFER               00107   
         EJECT                                                          00108   
*                                  EXTERNAL ADDRESS DIRECTORY           00109   
VADDAY   DS    V                                                        00110   
VCARDS   DS    V                                                        00111   
VCPRINT  DS    V                                                        00112   
VDATAMGR DS    V                                                        00113   
VDATCON  DS    V                                                        00114   
VDATVAL  DS    V                                                        00115   
VDELCOND DS    V                                                        00116   
VDELEXP  DS    V                                                        00117   
VDEMCALC DS    V                                                        00118   
VDEDPTAC DS    V                                                        00119**2
VDERDTPT DS    V                                                        00120   
VDEMKTAC DS    V                                                        00121   
VDEMUNIV DS    V                                                        00122   
VDUMPOUT DS    V                                                        00123   
VDWDISP  DS    V                                                        00124   
VPWDISP  DS    V                                                        00125   
VGETDAY  DS    V                                                        00126   
VHEXOUT  DS    V                                                        00127   
VLOADER  DS    V                                                        00128   
VLOGIO   DS    V                                                        00129   
VNETWEEK DS    V                                                        00130   
VHRTOQH  DS    V                                                        00131   
VQHTOHR  DS    V                                                        00132   
VPHASES  DS    V                                                        00133   
VPRINT   DS    V                                                        00134   
VPRINTER DS    V                                                        00135   
VSCANNER DS    V                                                        00136   
VSORTC   DS    V                                                        00137   
VSORTER  DS    V                                                        00138   
VTICTOC  DS    V                                                        00139   
VTIMVAL  DS    V                                                        00140   
VUNTIME  DS    V                                                        00141   
VXSORT   DS    V                                                        00142   
VHEXIN   DS    V                                                        00143   
VNSIWEEK DS    V                                                        00144**6
VLDCREC  DS    V                                                        00145**8
VBINSRCH DS    V                                                        00146*12
VDEMDISP DS    V                                                        00147*12
VCALUNV  DS    V                                                        00148*13
VCALVPH  DS    V                                                        00149*13
VCALPUT  DS    V                                                        00150*13
VNTIPRG  DS    V                                                        00151*18
VBITMAP1 DS    V                   HOLDS NTI PRG # IN USE               00152*19
VBITMAP2 DS    V                   2ND BUFF NEEDED FOR NHTI             00153*19
VNTIDDS  DS    V                   TABLE OF NEWLY ASGND NTI-DDS PRG#S   00154*19
VT00AE6  DS    V                   A(DEGET01)                           00155*20
         DS    A                   V(PERVAL)                            00156*22
VHLFBUF  DS    V                   V(HLFBUF)                            00157*23
VQHRBUF  DS    V                   V(QHRBUF)                            00158*23
         PRINT ON                                                       02874   
         SPACE 2                                                        02875   
         LTORG                                                                  
         EJECT                                                                  
ADSCT    DSECT                                                                  
A0000    DS    0C                                                               
         ORG   *+0115                                                           
A0116    DS    CL0009              YW25                                         
A0125    DS    CL0009              YW68                                         
A0134    DS    CL0009              YW911                                        
A0143    DS    CL0009              YW1214                                       
A0152    DS    CL0009              YW1517                                       
A0161    DS    CL0009              YW1820                                       
A0170    DS    CL0009              YW2124                                       
A0179    DS    CL0009              YW2529                                       
A0188    DS    CL0009              YW3034                                       
A0197    DS    CL0009              YW3539                                       
A0206    DS    CL0009              YW4044                                       
A0215    DS    CL0009              YW4549                                       
A0224    DS    CL0009              YW5054                                       
A0233    DS    CL0009              YW5564                                       
A0242    DS    CL0009              YW65+                                        
A0251    DS    CL0009              YM25                                         
A0260    DS    CL0009              YM68                                         
A0269    DS    CL0009              YM911                                        
A0278    DS    CL0009              YM1214                                       
A0287    DS    CL0009              YM1517                                       
         LTORG                                                                  
         EJECT                                                                  
BDSCT    DSECT                                                                  
B0000    DS    0C                                                               
         ORG   *+0115                                                           
B0116    DS    CL0009              YM1820                                       
B0125    DS    CL0009              YM2124                                       
B0134    DS    CL0009              YM2529                                       
B0143    DS    CL0009              YM3034                                       
B0152    DS    CL0009              YM3539                                       
B0161    DS    CL0009              YM4044                                       
B0170    DS    CL0009              YM4549                                       
B0179    DS    CL0009              YM5054                                       
B0188    DS    CL0009              YM5564                                       
B0197    DS    CL0009              YM65+                                        
B0206    DS    CL0009              YWMOMS                                       
B0215    DS    CL0009              *                                            
B0224    DS    CL0009              YWW1820                                      
B0233    DS    CL0009              YWW2124                                      
B0242    DS    CL0009              YWW2534                                      
B0251    DS    CL0009              YWW3544                                      
B0260    DS    CL0009              YWW4549                                      
B0269    DS    CL0009              YWW5054                                      
B0278    DS    CL0009              YWW55+                                       
B0287    DS    CL0009              *                                            
         LTORG                                                                  
         EJECT                                                                  
CDSCT    DSECT                                                                  
C0000    DS    0C                                                               
         ORG   *+0115                                                           
C0116    DS    CL0009              ZW25                                         
C0125    DS    CL0009              ZW68                                         
C0134    DS    CL0009              ZW911                                        
C0143    DS    CL0009              ZW1214                                       
C0152    DS    CL0009              ZW1517                                       
C0161    DS    CL0009              ZW1820                                       
C0170    DS    CL0009              ZW2124                                       
C0179    DS    CL0009              ZW2529                                       
C0188    DS    CL0009              ZW3034                                       
C0197    DS    CL0009              ZW3539                                       
C0206    DS    CL0009              ZW4044                                       
C0215    DS    CL0009              ZW4549                                       
C0224    DS    CL0009              ZW5054                                       
C0233    DS    CL0009              ZW5564                                       
C0242    DS    CL0009              ZW65+                                        
C0251    DS    CL0009              ZM25                                         
C0260    DS    CL0009              ZM68                                         
C0269    DS    CL0009              ZM911                                        
C0278    DS    CL0009              ZM1214                                       
C0287    DS    CL0009              ZM1517                                       
         LTORG                                                                  
         EJECT                                                                  
DDSCT    DSECT                                                                  
D0000    DS    0C                                                               
         ORG   *+0115                                                           
D0116    DS    CL0009              ZM1820                                       
D0125    DS    CL0009              ZM2124                                       
D0134    DS    CL0009              ZM2529                                       
D0143    DS    CL0009              ZM3034                                       
D0152    DS    CL0009              ZM3539                                       
D0161    DS    CL0009              ZM4044                                       
D0170    DS    CL0009              ZM4549                                       
D0179    DS    CL0009              ZM5054                                       
D0188    DS    CL0009              ZM5564                                       
D0197    DS    CL0009              ZM65+                                        
D0206    DS    CL0009              ZWMOMS                                       
D0215    DS    CL0009              *                                            
D0224    DS    CL0009              ZWW1820                                      
D0233    DS    CL0009              ZWW2124                                      
D0242    DS    CL0009              ZWW2534                                      
D0251    DS    CL0009              ZWW3544                                      
D0260    DS    CL0009              ZWW4549                                      
D0269    DS    CL0009              ZWW5054                                      
D0278    DS    CL0009              ZWW55+                                       
D0287    DS    CL0009              *                                            
         LTORG                                                                  
         EJECT                                                                  
EDSCT    DSECT                                                                  
E0000    DS    0C                                                               
         ORG   *+0140                                                           
E0141    DS    CL0009              IHOMES                                       
E0150    DS    CL0003              RHOMES                                       
         LTORG                                                                  
         EJECT                                                                  
FDSCT    DSECT                                                                  
F0000    DS    0C                                                               
         ORG   *+0336                                                           
F0337    DS    CL0009              IHOMES                                       
F0346    DS    CL0003              RHOMES                                       
F0349    DS    CL0014              *                                            
F0363    DS    CL0002              SHOMES                                       
         LTORG                                                                  
         EJECT                                                                  
GDSCT    DSECT                                                                  
G0000    DS    0C                                                               
         ORG   *+0179                                                           
G0180    DS    CL0009              IHOMES                                       
G0189    DS    CL0003              RHOMES                                       
G0192    DS    CL0014              *                                            
G0206    DS    CL0002              SHOMES                                       
         LTORG                                                                  
         EJECT                                                                  
HDSCT    DSECT                                                                  
H0000    DS    0C                                                               
         ORG   *+0115                                                           
H0116    DS    CL0009              YW25                                         
H0125    DS    CL0009              YW68                                         
H0134    DS    CL0009              YW911                                        
H0143    DS    CL0009              YW1214                                       
H0152    DS    CL0009              YW1517                                       
H0161    DS    CL0009              YW1820                                       
H0170    DS    CL0009              YW2124                                       
H0179    DS    CL0009              YW2529                                       
H0188    DS    CL0009              YW3034                                       
H0197    DS    CL0009              YW3539                                       
H0206    DS    CL0009              YW4044                                       
H0215    DS    CL0009              YW4549                                       
H0224    DS    CL0009              YW5054                                       
H0233    DS    CL0009              YW5564                                       
H0242    DS    CL0009              YW65+                                        
H0251    DS    CL0009              YM25                                         
H0260    DS    CL0009              YM68                                         
H0269    DS    CL0009              YM911                                        
H0278    DS    CL0009              YM1214                                       
H0287    DS    CL0009              YM1517                                       
         LTORG                                                                  
         EJECT                                                                  
IDSCT    DSECT                                                                  
I0000    DS    0C                                                               
         ORG   *+0115                                                           
I0116    DS    CL0009              YM1820                                       
I0125    DS    CL0009              YM2124                                       
I0134    DS    CL0009              YM2529                                       
I0143    DS    CL0009              YM3034                                       
I0152    DS    CL0009              YM3539                                       
I0161    DS    CL0009              YM4044                                       
I0170    DS    CL0009              YM4549                                       
I0179    DS    CL0009              YM5054                                       
I0188    DS    CL0009              YM5564                                       
I0197    DS    CL0009              YM65+                                        
I0206    DS    CL0009              YWMOMS                                       
I0215    DS    CL0009              *                                            
I0224    DS    CL0009              YWW1820                                      
I0233    DS    CL0009              YWW2124                                      
I0242    DS    CL0009              YWW2534                                      
I0251    DS    CL0009              YWW3544                                      
I0260    DS    CL0009              YWW4549                                      
I0269    DS    CL0009              YWW5054                                      
I0278    DS    CL0009              YWW55+                                       
I0287    DS    CL0009              *                                            
         LTORG                                                                  
         EJECT                                                                  
JDSCT    DSECT                                                                  
J0000    DS    0C                                                               
         ORG   *+0179                                                           
J0180    DS    CL0009              IHOMES                                       
J0189    DS    CL0003              RHOMES                                       
         LTORG                                                                  
         EJECT                                                                  
KDSCT    DSECT                                                                  
K0000    DS    0C                                                               
         ORG   *+0192                                                           
K0193    DS    CL0004              YW25                                         
K0197    DS    CL0004              YW68                                         
K0201    DS    CL0004              YW911                                        
K0205    DS    CL0004              YW1214                                       
K0209    DS    CL0004              YW1517                                       
K0213    DS    CL0004              YW1820                                       
K0217    DS    CL0004              YW2124                                       
K0221    DS    CL0004              YW2529                                       
K0225    DS    CL0004              YW3034                                       
K0229    DS    CL0004              YW3539                                       
K0233    DS    CL0004              YW4044                                       
K0237    DS    CL0004              YW4549                                       
K0241    DS    CL0004              YW5054                                       
K0245    DS    CL0004              YW5564                                       
K0249    DS    CL0004              YW65+                                        
K0253    DS    CL0004              YM25                                         
K0257    DS    CL0004              YM68                                         
K0261    DS    CL0004              YM911                                        
K0265    DS    CL0004              YM1214                                       
K0269    DS    CL0004              YM1517                                       
K0273    DS    CL0004              YM1820                                       
K0277    DS    CL0004              YM2124                                       
K0281    DS    CL0004              YM2529                                       
K0285    DS    CL0004              YM3034                                       
K0289    DS    CL0004              YM3539                                       
K0293    DS    CL0004              YM4044                                       
K0297    DS    CL0004              YM4549                                       
K0301    DS    CL0004              YM5054                                       
K0305    DS    CL0004              YM5564                                       
K0309    DS    CL0004              YM65+                                        
K0313    DS    CL0004              YWMOMS                                       
K0317    DS    CL0004              YWW1820                                      
K0321    DS    CL0004              YWW2124                                      
K0325    DS    CL0004              YWW2534                                      
K0329    DS    CL0004              YWW3544                                      
K0333    DS    CL0004              YWW4549                                      
K0337    DS    CL0004              YWW5054                                      
K0341    DS    CL0004              YWW55+                                       
K0345    DS    CL0004              YHOMES                                       
K0349    DS    CL0004              VW18+                                        
K0353    DS    CL0004              VW1849                                       
K0357    DS    CL0004              VM18+                                        
K0361    DS    CL0004              VW2554                                       
K0365    DS    CL0004              VWWRK                                        
K0369    DS    CL0004              VW1217                                       
K0373    DS    CL0004              VM1849                                       
K0377    DS    CL0004              VM2554                                       
K0381    DS    CL0004              VM55+                                        
K0385    DS    CL0004              VW2549                                       
K0389    DS    CL0004              VW55+                                        
K0393    DS    CL0004              VWW1849                                      
K0397    DS    CL0004              VM1217                                       
K0401    DS    CL0004              VM2549                                       
K0405    DS    CL0004              VV2+                                         
K0409    DS    CL0004              VW1834                                       
K0413    DS    CL0004              VM1834                                       
K0417    DS    CL0004              VWMOMS                                       
K0421    DS    CL0004              VM2149                                       
K0425    DS    CL0004              VM3564                                       
K0429    DS    CL0004              VW21+                                        
K0433    DS    CL0004              VW3564                                       
K0437    DS    CL0004              VM2154                                       
K0441    DS    CL0004              VM21+                                        
K0445    DS    CL0004              VW2149                                       
K0449    DS    CL0004              VW2154                                       
K0453    DS    CL0004              VWW2554                                      
K0457    DS    CL0004              VV611                                        
K0461    DS    CL0004              VV1217                                       
K0465    DS    CL0004              VV211                                        
K0469    DS    CL0004              VW211                                        
K0473    DS    CL0004              VM211                                        
K0477    DS    CL0004              VW611                                        
K0481    DS    CL0004              VM611                                        
K0485    DS    CL0004              VV1214                                       
K0489    DS    CL0004              VV25                                         
K0493    DS    CL0004              VV1517                                       
K0497    DS    CL0004              VW1824                                       
K0501    DS    CL0004              VW1524                                       
K0505    DS    CL0004              VM1824                                       
K0509    DS    CL0004              VM1524                                       
K0513    DS    CL0004              VV68                                         
K0517    DS    CL0004              VV911                                        
K0521    DS    CL0004              VHWC18                                       
K0525    DS    CL0004              VHWC12                                       
K0529    DS    CL0004              VHWC6                                        
K0533    DS    CL0004              RHOMES                                       
K0537    DS    CL0004              IHOMES                                       
K0541    DS    CL0004              SHOMES                                       
K0545    DS    CL0004              FN03                                         
K0549    DS    CL0004              FN04                                         
K0553    DS    CL0004              FN05                                         
K0557    DS    CL0004              FN06                                         
K0561    DS    CL0004              FN07                                         
K0565    DS    CL0004              FN08                                         
K0569    DS    CL0004              VV18+                                        
K0573    DS    CL0004              VV1834                                       
K0577    DS    CL0004              VV1849                                       
K0581    DS    CL0004              VV2549                                       
K0585    DS    CL0004              VV2554                                       
K0589    DS    CL0004              VV3564                                       
K0593    DS    CL0004              VV5564                                       
K0597    DS    CL0004              VV55+                                        
K0601    DS    CL0004              VV1824                                       
K0605    DS    CL0004              VV2149                                       
K0609    DS    CL0004              VV2154                                       
K0613    DS    CL0004              VV21+                                        
K0617    DS    CL0004              VV1524                                       
K0621    DS    CL0004              RHWC18                                       
K0625    DS    CL0004              RHWC12                                       
K0629    DS    CL0004              RHWC6                                        
K0633    DS    CL0004              IHWC18                                       
K0637    DS    CL0004              IHWC12                                       
K0641    DS    CL0004              IHWC6                                        
K0645    DS    CL0004              FN00                                         
K0649    DS    CL0004              FN01                                         
K0653    DS    CL0004              FN02                                         
K0657    DS    CL0004              FN03                                         
K0661    DS    CL0004              FN04                                         
K0665    DS    CL0004              FN05                                         
K0669    DS    CL0004              FN06                                         
K0673    DS    CL0004              FN07                                         
K0677    DS    CL0004              FN08                                         
K0681    DS    CL0004              FN09                                         
K0685    DS    CL0004              FN0A                                         
K0689    DS    CL0004              FN0B                                         
K0693    DS    CL0004              FN0C                                         
K0697    DS    CL0004              FN0D                                         
K0701    DS    CL0004              FN0E                                         
K0705    DS    CL0004              FN0F                                         
K0709    DS    CL0004              FN10                                         
K0713    DS    CL0004              FN11                                         
K0717    DS    CL0004              FN12                                         
K0721    DS    CL0004              FN13                                         
K0725    DS    CL0004              FN14                                         
K0729    DS    CL0004              FN15                                         
K0733    DS    CL0004              FN16                                         
K0737    DS    CL0004              FN17                                         
K0741    DS    CL0004              FN18                                         
K0745    DS    CL0004              FN19                                         
K0749    DS    CL0004              FN1A                                         
K0753    DS    CL0004              FN1B                                         
K0757    DS    CL0004              FN1C                                         
K0761    DS    CL0004              FN1D                                         
K0765    DS    CL0004              FN1E                                         
K0769    DS    CL0004              FN1F                                         
K0773    DS    CL0004              FN20                                         
K0777    DS    CL0004              FN21                                         
K0781    DS    CL0004              FN22                                         
K0785    DS    CL0004              FN23                                         
K0789    DS    CL0004              FN24                                         
K0793    DS    CL0004              FN25                                         
K0797    DS    CL0004              FN26                                         
K0801    DS    CL0004              FN27                                         
K0805    DS    CL0004              FN28                                         
K0809    DS    CL0004              FN29                                         
K0813    DS    CL0004              FN2A                                         
K0817    DS    CL0004              FN2B                                         
K0821    DS    CL0004              OW18+                                        
K0825    DS    CL0004              OW1849                                       
K0829    DS    CL0004              OM18+                                        
K0833    DS    CL0004              OW2554                                       
K0837    DS    CL0004              OWWRK                                        
K0841    DS    CL0004              OW1217                                       
K0845    DS    CL0004              OM1849                                       
K0849    DS    CL0004              OM2554                                       
K0853    DS    CL0004              OM55+                                        
K0857    DS    CL0004              OW2549                                       
K0861    DS    CL0004              OW55+                                        
K0865    DS    CL0004              OWW1849                                      
K0869    DS    CL0004              OM1217                                       
K0873    DS    CL0004              OM2549                                       
K0877    DS    CL0004              OV2+                                         
K0881    DS    CL0004              OW1834                                       
K0885    DS    CL0004              OM1834                                       
K0889    DS    CL0004              OWMOMS                                       
K0893    DS    CL0004              OM2149                                       
K0897    DS    CL0004              OM3564                                       
K0901    DS    CL0004              OW21+                                        
K0905    DS    CL0004              OW3564                                       
K0909    DS    CL0004              OM2154                                       
K0913    DS    CL0004              OM21+                                        
K0917    DS    CL0004              OW2149                                       
K0921    DS    CL0004              OW2154                                       
K0925    DS    CL0004              OWW2554                                      
K0929    DS    CL0004              OV611                                        
K0933    DS    CL0004              OV1217                                       
K0937    DS    CL0004              OV211                                        
K0941    DS    CL0004              OW211                                        
K0945    DS    CL0004              OM211                                        
K0949    DS    CL0004              OW611                                        
K0953    DS    CL0004              OM611                                        
K0957    DS    CL0004              OV1214                                       
K0961    DS    CL0004              OV25                                         
K0965    DS    CL0004              OV1517                                       
K0969    DS    CL0004              OW1824                                       
K0973    DS    CL0004              OW1524                                       
K0977    DS    CL0004              OM1824                                       
K0981    DS    CL0004              OM1524                                       
K0985    DS    CL0004              OV611                                        
K0989    DS    CL0004              OV911                                        
K0993    DS    CL0004              OV18+                                        
K0997    DS    CL0004              OV1834                                       
K1001    DS    CL0004              OV1849                                       
K1005    DS    CL0004              OV2549                                       
K1009    DS    CL0004              OV2554                                       
K1013    DS    CL0004              OV3564                                       
K1017    DS    CL0004              OV5564                                       
K1021    DS    CL0004              OV55+                                        
K1025    DS    CL0004              OV1824                                       
K1029    DS    CL0004              OV2149                                       
K1033    DS    CL0004              OV2154                                       
K1037    DS    CL0004              OV21+                                        
K1041    DS    CL0004              OV1524                                       
K1045    DS    CL0004              OHOMES                                       
K1049    DS    CL0004              ZW25                                         
K1053    DS    CL0004              ZW68                                         
K1057    DS    CL0004              ZW911                                        
K1061    DS    CL0004              ZW1214                                       
K1065    DS    CL0004              ZW1517                                       
K1069    DS    CL0004              ZW1820                                       
K1073    DS    CL0004              ZW2124                                       
K1077    DS    CL0004              ZW2529                                       
K1081    DS    CL0004              ZW3034                                       
K1085    DS    CL0004              ZW3539                                       
K1089    DS    CL0004              ZW4044                                       
K1093    DS    CL0004              ZW4549                                       
K1097    DS    CL0004              ZW5054                                       
K1101    DS    CL0004              ZW5564                                       
K1105    DS    CL0004              ZW65+                                        
K1109    DS    CL0004              ZM25                                         
K1113    DS    CL0004              ZM68                                         
K1117    DS    CL0004              ZM911                                        
K1121    DS    CL0004              ZM1214                                       
K1125    DS    CL0004              ZM1517                                       
K1129    DS    CL0004              ZM1820                                       
K1133    DS    CL0004              ZM2124                                       
K1137    DS    CL0004              ZM2529                                       
K1141    DS    CL0004              ZM3034                                       
K1145    DS    CL0004              ZM3539                                       
K1149    DS    CL0004              ZM4044                                       
K1153    DS    CL0004              ZM4549                                       
K1157    DS    CL0004              ZM5054                                       
K1161    DS    CL0004              ZM5564                                       
K1165    DS    CL0004              ZM65+                                        
K1169    DS    CL0004              ZWMOMS                                       
K1173    DS    CL0004              ZWW1820                                      
K1177    DS    CL0004              ZWW2124                                      
K1181    DS    CL0004              ZWW2534                                      
K1185    DS    CL0004              ZWW3544                                      
K1189    DS    CL0004              ZWW4549                                      
K1193    DS    CL0004              ZWW5054                                      
K1197    DS    CL0004              ZWW55+                                       
K1201    DS    CL0004              WW25                                         
K1205    DS    CL0004              WW68                                         
K1209    DS    CL0004              WW911                                        
K1213    DS    CL0004              WW1214                                       
K1217    DS    CL0004              WW1517                                       
K1221    DS    CL0004              WW1820                                       
K1225    DS    CL0004              WW2124                                       
K1229    DS    CL0004              WW2529                                       
K1233    DS    CL0004              WW3034                                       
K1237    DS    CL0004              WW3539                                       
K1241    DS    CL0004              WW4044                                       
K1245    DS    CL0004              WW4549                                       
K1249    DS    CL0004              WW5054                                       
K1253    DS    CL0004              WW5564                                       
K1257    DS    CL0004              WW65+                                        
K1261    DS    CL0004              WM25                                         
K1265    DS    CL0004              WM68                                         
K1269    DS    CL0004              WM911                                        
K1273    DS    CL0004              WM1214                                       
K1277    DS    CL0004              WM1517                                       
K1281    DS    CL0004              WM1820                                       
K1285    DS    CL0004              WM2124                                       
K1289    DS    CL0004              WM2529                                       
K1293    DS    CL0004              WM3034                                       
K1297    DS    CL0004              WM3539                                       
K1301    DS    CL0004              WM4044                                       
K1305    DS    CL0004              WM4549                                       
K1309    DS    CL0004              WM5054                                       
K1313    DS    CL0004              WM5564                                       
K1317    DS    CL0004              WM65+                                        
K1321    DS    CL0004              WWMOMS                                       
K1325    DS    CL0004              WWW1820                                      
K1329    DS    CL0004              WWW2124                                      
K1333    DS    CL0004              WWW2534                                      
K1337    DS    CL0004              WWW3544                                      
K1341    DS    CL0004              WWW4549                                      
K1345    DS    CL0004              WWW5054                                      
K1349    DS    CL0004              WWW55+                                       
K1353    DS    CL0004              BW25                                         
K1357    DS    CL0004              BW68                                         
K1361    DS    CL0004              BW911                                        
K1365    DS    CL0004              BW1214                                       
K1369    DS    CL0004              BW1517                                       
K1373    DS    CL0004              BW1820                                       
K1377    DS    CL0004              BW2124                                       
K1381    DS    CL0004              BW2529                                       
K1385    DS    CL0004              BW3034                                       
K1389    DS    CL0004              BW3539                                       
K1393    DS    CL0004              BW4044                                       
K1397    DS    CL0004              BW4549                                       
K1401    DS    CL0004              BW5054                                       
K1405    DS    CL0004              BW5564                                       
K1409    DS    CL0004              BW65+                                        
K1413    DS    CL0004              BM25                                         
K1417    DS    CL0004              BM68                                         
K1421    DS    CL0004              BM911                                        
K1425    DS    CL0004              BM1214                                       
K1429    DS    CL0004              BM1517                                       
K1433    DS    CL0004              BM1820                                       
K1437    DS    CL0004              BM2124                                       
K1441    DS    CL0004              BM2529                                       
K1445    DS    CL0004              BM3034                                       
K1449    DS    CL0004              BM3539                                       
K1453    DS    CL0004              BM4044                                       
K1457    DS    CL0004              BM4549                                       
K1461    DS    CL0004              BM5054                                       
K1465    DS    CL0004              BM5564                                       
K1469    DS    CL0004              BM65+                                        
K1473    DS    CL0004              BWMOMS                                       
K1477    DS    CL0004              BWW1820                                      
K1481    DS    CL0004              BWW2124                                      
K1485    DS    CL0004              BWW2534                                      
K1489    DS    CL0004              BWW3544                                      
K1493    DS    CL0004              BWW4549                                      
K1497    DS    CL0004              BWW5054                                      
K1501    DS    CL0004              BWW55+                                       
K1505    DS    CL0004              MW18+                                        
K1509    DS    CL0004              MW1849                                       
K1513    DS    CL0004              MM18+                                        
K1517    DS    CL0004              MW2554                                       
K1521    DS    CL0004              MWWRK                                        
K1525    DS    CL0004              MW1217                                       
K1529    DS    CL0004              MM1849                                       
K1533    DS    CL0004              MM2554                                       
K1537    DS    CL0004              MM55+                                        
K1541    DS    CL0004              MW2549                                       
K1545    DS    CL0004              MW55+                                        
K1549    DS    CL0004              MWW1849                                      
K1553    DS    CL0004              MM1217                                       
K1557    DS    CL0004              MM2549                                       
K1561    DS    CL0004              MV2+                                         
K1565    DS    CL0004              MW1834                                       
K1569    DS    CL0004              MM1834                                       
K1573    DS    CL0004              MWMOMS                                       
K1577    DS    CL0004              MM2149                                       
K1581    DS    CL0004              MM3564                                       
K1585    DS    CL0004              MW21+                                        
K1589    DS    CL0004              MW3564                                       
K1593    DS    CL0004              MM2154                                       
K1597    DS    CL0004              MM21+                                        
K1601    DS    CL0004              MW2149                                       
K1605    DS    CL0004              MW2154                                       
K1609    DS    CL0004              MWW2554                                      
K1613    DS    CL0004              MV611                                        
K1617    DS    CL0004              MV1217                                       
K1621    DS    CL0004              MV211                                        
K1625    DS    CL0004              MW211                                        
K1629    DS    CL0004              MM211                                        
K1633    DS    CL0004              MW611                                        
K1637    DS    CL0004              MM611                                        
K1641    DS    CL0004              MV1214                                       
K1645    DS    CL0004              MV25                                         
K1649    DS    CL0004              MV1517                                       
K1653    DS    CL0004              MW1824                                       
K1657    DS    CL0004              MW1524                                       
K1661    DS    CL0004              MM1824                                       
K1665    DS    CL0004              MM1524                                       
K1669    DS    CL0004              MV68                                         
K1673    DS    CL0004              MV911                                        
K1677    DS    CL0004              MHWC18                                       
K1681    DS    CL0004              MHWC12                                       
K1685    DS    CL0004              MHWC6                                        
K1689    DS    CL0004              LHOMES                                       
K1693    DS    CL0004              NHOMES                                       
K1697    DS    CL0004              FN02                                         
K1701    DS    CL0004              FN03                                         
K1705    DS    CL0004              FN04                                         
K1709    DS    CL0004              FN05                                         
K1713    DS    CL0004              FN06                                         
K1717    DS    CL0004              FN07                                         
K1721    DS    CL0004              FN08                                         
K1725    DS    CL0004              MV18+                                        
K1729    DS    CL0004              MV1834                                       
K1733    DS    CL0004              MV1849                                       
K1737    DS    CL0004              MV2549                                       
K1741    DS    CL0004              MV2554                                       
K1745    DS    CL0004              MV3564                                       
K1749    DS    CL0004              MV5564                                       
K1753    DS    CL0004              MV55+                                        
K1757    DS    CL0004              MV1824                                       
K1761    DS    CL0004              MV2149                                       
K1765    DS    CL0004              MV2154                                       
K1769    DS    CL0004              MV21+                                        
K1773    DS    CL0004              MV1524                                       
K1777    DS    CL0004              LHWC18                                       
K1781    DS    CL0004              LHWC12                                       
K1785    DS    CL0004              LHWC6                                        
K1789    DS    CL0004              NHWC18                                       
K1793    DS    CL0004              NHWC12                                       
K1797    DS    CL0004              NHWC6                                        
         LTORG                                                                  
         EJECT                                                                  
ZDSCT    DSECT                                                                  
         ORG   *+0000                                                           
         DC    X'00'                                                            
         SPACE 2                                                                
         END                                                                    
