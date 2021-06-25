*          DATA SET SPWRI11    AT LEVEL 096 AS OF 12/23/04                      
*PHASE T20411A                                                                  
*INCLUDE SPFMTINO                                                               
         TITLE 'T20411 - GENERAL FOODS TAPE'                                    
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI11 (T20411) - GENERAL FOODS TAPE                    *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 13AUG04 95 EFJ -- USE SPFMTINO TO OUTPUT INVOICE NUMBER           *           
* 06AUG01 92 EFJ -- GET DYNALLOC FROM TWADCONS                      *           
* 17JUL01 91 EFJ -- AGY CODE 5 FOR CLT KRM FOR YN                   *           
* 11JUL01 90 EFJ -- CHANGE TOTNET AND TOTCOM TO DC PL8'0'           *           
* 20MAR00 89 EFJ -- ADD H9 MEDIAVEST TO TABLE                       *           
* 13FEB01 88 EFJ -- ADD CLT MG EXCEPTION FOR MINDSHARE              *           
* 17JAN01 87 EFJ -- MINDSHARE CHANGES                               *           
* 19JAN00    BPLA   MORE Y2K BUG FIXES (GFMON AND BILLMON)                      
* 26AUG98 85 EFJ -- FIX L84 FIX                                     *           
* 16JUN98 84 NRK -- Y2K COMPLIANCE                                  *           
* 05MAY98 83 EFJ -- X RECORD TYPES NO LONGER NEEDED (SKIP BUYS)     *           
* 12MAR98 82 EFJ -- TWADCONS UNDEFINED ONLINE                       *           
* 11FEB97 74 EFJ -- TREAT CLT KRX LIKE KRB                          *           
* 09JAN97 73 EFJ -- ADDED WW TO AGYTAB                              *           
*                -- HISTORY LOST.  TOMBSTONE ADDED                  *           
*                                                                   *           
*********************************************************************           
T20411   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20411,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   NOMAS                                                            
         L     R1,TWADCONS                                                      
***         L     R1,TSPFUSER-TWADCOND(R1)                                      
***         ST    R1,ASAVE                                                      
         USING TWADCOND,R1                                                      
         MVC   ASAVE,TSPFUSER                                                   
         MVC   VDYNALLO,TDYNALLO                                                
         DROP  R1                                                               
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BZ    NOMAS                                                            
         USING MASTD,RE                                                         
         ST    R1,MCUSRDMP                                                      
         LA    R1,1024(R1)                                                      
         ST    R1,MCUSRDMP+4                                                    
         OI    MCUSRDMP+4,X'80'                                                 
         DROP  RE                                                               
*                                  REPORT CALLING MODE                          
NOMAS    CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     RUNLAST                                      
         BE    LST                                                              
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   INIT1                                                            
         ZAP   TAPECNT,=P'0'       YES-INITIALIZE TAPE COUNT                    
         MVI   TAPEOPEN,C'N'       TAPE NOT OPENED YET                          
         MVI   TEMPOPEN,C'N'       TEMP FILE NOT OPENED YET                     
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
         LA    RE,SVDCB1                                                        
         A     RE,ASAVE                                                         
         ST    RE,AGFFILE                                                       
         MVC   0(96,RE),GFFILE     MOVE DCB FOR GFFILE                          
*                                                                               
         LA    RE,SVDCB2                                                        
         A     RE,ASAVE                                                         
         ST    RE,AGFTEMP                                                       
         MVC   0(96,RE),GFTEMP     MOVE DCB FOR GFTEMP                          
*                                                                               
         B     INIT2                                                            
*                                                                               
INIT1    L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INIT2    OI    SBQSKIP,SBQSKGL+SBQSKBUY     READ STATION BILL RECORDS           
*NIT2    OI    SBQSKIP,SBQSKGL     READ BUYS AND STATION BILL RECORDS           
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         OI    SBQPER,SBQPMN       MONTHS                                       
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    SBQRDOPT,SBQROMAB   READ MANUAL BILLING                          
         OI    DATAIND2,DIEFFCST   GET EFFECTIVE BUY COST                       
         OI    DATAIND2,DIEST      ESTIMATE                                     
         XC    LEVELS,LEVELS                                                    
         MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
         XC    MONTAB,MONTAB       INITIALIZE MONTH TABLE                       
         MVI   SVPRD,0                                                          
         MVI   SVEST,0                                                          
         XC    SVBILKEY,SVBILKEY                                                
         LA    RE,PRDTAB           CLEAR PRODUCT AND ESTIMATE TABLES            
         LA    RF,255*L'PRDTAB                                                  
         XCEF  ,                                                                
         LA    RE,ESTTAB                                                        
         LA    RF,255*L'ESTTAB                                                  
         XCEF  ,                                                                
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,GFTTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    CLI   SBQBPRD,X'FF'       TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
         LA    R2,GFTMONH          VALIDATE BILLING MONTH                       
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(2,WORK),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         MVC   BILLMON,DUB         SAVE BILLING MONTH YYMM                      
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(2,SBQBILST)    MONTH OF BILLING                 
         MVC   DUB+4(2),=C'31'                                                  
         GOTO1 (RF),(R1),DUB,(2,SBQBILEN)      START/END DATES                  
         GOTO1 (RF),(R1),DUB,(6,BILLMON6)                                       
**Y2K                                                                           
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 (RF),(R1),DUB,(X'20',WORK)                                       
         MVC   BILLMON(4),WORK                                                  
**Y2K                                                                           
*                                                                               
         MVI   SUPTAP,C'N'                                                      
         LA    R2,GFTTAPH          OPTION TO SUPPRESS TAPE                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   SUPTAP,WORK                                                      
         CLI   SUPTAP,C'Y'                                                      
         BE    XIT                                                              
         CLI   SUPTAP,C'N'                                                      
         BNE   EINV                                                             
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCAG     AGENCY FIRST --                              
         BNE   INP4                                                             
         MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         MVI   AGENCYCD,C' '                                                    
         LA    R1,AGYTAB           FIND GF AGENCY CODE                          
*                                                                               
INP2     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    ERRCD,ERRAGY                                                     
         B     INP4                                                             
         CLC   SBAGY,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     INP2                                                             
         MVC   AGENCYCD,2(R1)                                                   
*                                                                               
         CLI   AGENCYCD,C'A'       FOR YNR                                      
         BNE   INP3                                                             
         CLC   GFTCLT(3),=C'KRX'   FOR CLIENT KRX                               
         BE    INP2A                                                            
         CLC   GFTCLT(3),=C'KRB'   AND CLIENT KRB                               
         BE    INP2A                                                            
         CLC   GFTCLT(3),=C'KRM'   AND CLIENT KRM                               
         BNE   INP3                                                             
INP2A    MVI   AGENCYCD,C'5'       AGY CODE IS 5                                
*                                                                               
INP3     CLI   AGENCYCD,C'/'       FOR H7                                       
         BNE   INP4                                                             
         CLC   GFTCLT(2),=C'MG'    FOR CLIENT MG                                
         BE    *+14                                                             
         CLC   GFTCLT(2),=C'KR'   FOR CLIENT KR                                 
         BNE   INP3A                                                            
         CLI   GFTCLT+2,C' '                                                    
         BH    INP3A                                                            
         MVI   AGENCYCD,C'W'       AGY CODE IS W (SAME AS JW)                   
         B     INP4                                                             
*                                                                               
INP3A    CLC   GFTCLT(3),=C'KRG'   FOR CLIENT KRG                               
         BE    *+6                                                              
         DCHO                                                                   
         MVI   AGENCYCD,C'M'       AGY CODE IS W (SAME AS OM)                   
         B     INP4                                                             
*                                                                               
AGYTAB   DC    CL2'OM',CL1'M'                                                   
         DC    CL2'YN',CL1'A'                                                   
         DC    CL2'H7',CL1'/'                                                   
         DC    CL2'JW',CL1'W'                                                   
         DC    CL2'FC',CL1'C'                                                   
         DC    CL2'WW',CL1'H'                                                   
         DC    CL2'H9',CL1'Q'                                                   
         DC    CL2'SJ',CL1'S'   SJR TEST                                        
         DC    X'00'                                                            
*                                                                               
INP4     CLI   SBMODE,SBPROCBL     STATION BILL RECORDS                         
         BE    *+12                                                             
         CLI   SBMODE,SBPROCSP     AND BUY RECORDS ---                          
         BNE   INPX                                                             
         OC    MONTAB,MONTAB       TEST MONTH TABLE SET YET                     
         BNZ   INP8                                                             
         L     R2,AMONTHS          NO-SET MONTH TABLE                           
         LA    R3,MONTAB                                                        
         LA    R0,24                                                            
*                                                                               
INP5     OC    0(4,R2),0(R2)                                                    
         BZ    INP6                                                             
         MVC   0(4,R3),0(R2)                                                    
         GOTO1 DATCON,DMCB,(2,2(R2)),DUB                                        
         MVC   4(4,R3),DUB                                                      
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,INP5                                                          
*                                                                               
INP6     MVI   0(R3),X'FF'                                                      
*                                                                               
INP8     CLI   SBMODE,SBPROCBL     TEST BILL RECORD                             
         BNE   INP9                                                             
         XC    BILLST,BILLST       YES-ACCEPT ALL MONTHS OF SERVICE             
         MVC   BILLEND,=X'FFFF'                                                 
         CLC   SBBPRD,SVPRD        TEST CHANGE OF PRODUCT                       
         BE    INP9                                                             
         BAS   RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
         MVC   SVPRD,SBBPRD                                                     
*                                                                               
INP9     CLC   SBBEST,SVEST        TEST CHANGE OF ESTIMATE                      
         BE    INP10                                                            
         BAS   RE,GETEST           YES-GET GF CODES FOR ESTIMATE                
         MVC   SVEST,SBBEST                                                     
*                                                                               
INP10    B     INPX                                                             
*                                                                               
INPX     B     XIT                                                              
         EJECT                                                                  
* GET GF PRODUCT CODES                                                          
*                                                                               
GETPRD   NTR1  ,                                                                
         NI    ERRCD,255-ERRPRD                                                 
         MVC   COMPYDIV,SPACES                                                  
         MVC   PRODCODE,SPACES                                                  
         ZIC   R6,SBBPRD           INDEX INTO PRODUCT TABLE                     
         BCTR  R6,0                                                             
         MH    R6,=Y(L'PRDTAB)                                                  
         LA    R6,PRDTAB(R6)                                                    
         OC    0(L'PRDTAB,R6),0(R6)   TEST CODES FOUND YET                      
         BZ    GETP1                                                            
         MVC   COMPYDIV,0(R6)      YES                                          
         MVC   PRODCODE,L'COMPYDIV(R6)                                          
         B     GETP8                                                            
*                                                                               
GETP1    XC    KEY,KEY             NO-READ GF ESTIMATE RECORD                   
         LA    R5,KEY                 WITH ESTIMATE=0                           
         USING PGESTD,R5                                                        
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         MVC   PGKPRD,SBPRD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(PGKEST-PGKEY),KEYSAVE  TEST RECORD FOUND                     
         BNE   GETP7                                                            
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGKEDQ(R5)       SCAN ELEMENTS FOR                            
         SR    R0,R0               DIVISION/BRAND AND PRODUCT CODE              
*                                                                               
GETP2    CLI   0(R5),0                                                          
         BE    GETP7                                                            
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   GETP6                                                            
         USING PGSTELMD,R5                                                      
         LA    R1,COMPYDIV                                                      
         LA    RE,L'COMPYDIV-1                                                  
         CLC   PGSTNAME,QCOMPDIV                                                
         BE    GETP4                                                            
         LA    R1,PRODCODE                                                      
         LA    RE,L'PRODCODE-1                                                  
         CLC   PGSTNAME,QPRDCODE                                                
         BNE   GETP6                                                            
*                                                                               
GETP4    EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
GETP6    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETP2                                                            
*                                                                               
GETP7    MVC   0(L'COMPYDIV,R6),COMPYDIV                                        
         MVC   L'COMPYDIV(L'PRODCODE,R6),PRODCODE                               
*                                                                               
GETP8    CLC   COMPYDIV,SPACES     TEST FOR PRODUCT ERRORS                      
         BNH   GETP10                                                           
         CLC   PRODCODE,SPACES                                                  
         BH    GETPX                                                            
*                                                                               
GETP10   OI    ERRCD,ERRPRD                                                     
*                                                                               
GETPX    B     XIT                                                              
         EJECT                                                                  
* GET GF ESTIMATE CODES                                                         
*                                                                               
GETEST   NTR1  ,                                                                
         NI    ERRCD,255-ERREST                                                 
         MVC   NATURAL,SPACES                                                   
         MVC   SUBNATRL,SPACES                                                  
         ZIC   R6,SBBEST           INDEX INTO ESTIMATE TABLE                    
         BCTR  R6,0                                                             
         MH    R6,=Y(L'ESTTAB)                                                  
         LA    R6,ESTTAB(R6)                                                    
         OC    0(L'ESTTAB,R6),0(R6)   TEST CODES FOUND YET                      
         BZ    GETE1                                                            
         MVC   NATURAL,0(R6)       YES                                          
         MVC   SUBNATRL,L'NATURAL(R6)                                           
         B     GETE8                                                            
*                                                                               
GETE1    XC    KEY,KEY             NO-READ GF ESTIMATE RECORD                   
         LA    R5,KEY                                                           
         USING PGESTD,R5                                                        
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         MVC   PGKPRD,=C'POL'      USE PRODUCT=POL                              
         MVC   PGKEST,SBBEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(PGKLENQ),KEYSAVE    TEST RECORD FOUND                        
         BNE   GETE7                                                            
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGKEDQ(R5)       SCAN ELEMENTS FOR                            
         SR    R0,R0               NATURAL AND SUB-NATURAL CODES                
*                                                                               
GETE2    CLI   0(R5),0                                                          
         BE    GETE7                                                            
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   GETE6                                                            
         USING PGSTELMD,R5                                                      
         LA    R1,NATURAL                                                       
         LA    RE,L'NATURAL-1                                                   
         CLC   PGSTNAME,QNATURAL                                                
         BE    GETE4                                                            
         LA    R1,SUBNATRL                                                      
         LA    RE,L'SUBNATRL-1                                                  
         CLC   PGSTNAME,QSUBNATL                                                
         BNE   GETE6                                                            
*                                                                               
GETE4    EX    RE,*+4                                                           
         MVC   0(0,R1),PGSTDATA                                                 
*                                                                               
GETE6    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETE2                                                            
*                                                                               
GETE7    MVC   0(L'NATURAL,R6),NATURAL                                          
         MVC   L'NATURAL(L'SUBNATRL,R6),SUBNATRL                                
*                                                                               
GETE8    CLC   NATURAL,SPACES      TEST FOR ESTIMATE ERRORS                     
         BNH   GETE10                                                           
         CLC   SUBNATRL,SPACES                                                  
         BH    GETEX                                                            
*                                                                               
GETE10   OI    ERRCD,ERREST                                                     
*                                                                               
GETEX    B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHKNTR  NTR1  ,                                                                
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT TO SORT                                  
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEADHK                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IKEY    ',A(IKEY)     GF REPORT                              
         DC    CL8'OKEY    ',A(OKEY)                                            
         DC    CL8'INET    ',A(INET)                                            
         DC    CL8'ONET    ',A(ONET)                                            
         DC    CL8'ICOM    ',A(ICOM)                                            
         DC    CL8'OCOM    ',A(OCOM)                                            
         DC    CL8'IDATE   ',A(IDATE)                                           
         DC    CL8'ODATE   ',A(ODATE)                                           
         DC    CL8'OFILLER ',A(OFILLER)                                         
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
         DC    CL8'LASTCOL ',A(LASTCOL)                                         
*                                                                               
         DC    CL8'IAGENCY ',A(IAGENCY)  AGENCY REPORT                          
         DC    CL8'OAGENCY ',A(OAGENCY)                                         
         DC    CL8'OMEDIA  ',A(OMEDIA)                                          
         DC    CL8'ICOMPANY',A(ICOMPANY)                                        
         DC    CL8'IDIV    ',A(IDIV)                                            
         DC    CL8'IPRDCODE',A(IPRDCODE)                                        
         DC    CL8'IESTIM  ',A(IESTIM)                                          
         DC    CL8'IMONTH  ',A(IMONTH)                                          
         DC    CL8'OMONTH  ',A(OMONTH)                                          
         DC    CL8'IINV    ',A(IINV)                                            
         DC    CL8'ITYPE   ',A(ITYPE)                                           
         DC    CL8'INAT    ',A(INAT)                                            
         DC    CL8'ISUB    ',A(ISUB)                                            
         DC    CL8'IMARKET ',A(IMARKET)                                         
         DC    CL8'I2NET   ',A(I2NET)                                           
         DC    CL8'O2NET   ',A(O2NET)                                           
         DC    CL8'I2COM   ',A(I2COM)                                           
         DC    CL8'O2COM   ',A(O2COM)                                           
         DC    CL8'TOTAL   ',A(TOTAL)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         MVI   GLOPTS+2,1          REPORT 1 = TAPE RECORD MAP                   
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R5,SBACURCH         R4=A(BUY RECORD CHUNK)                       
         USING SCHUNKD,R5                                                       
         LR    R6,R5               R5=A(BILL RECORD INVOIVE ELEM)               
         USING STABELEM,R6                                                      
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC2                                                            
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     R1,GLADTENT                                                      
         CLI   DRINLEV-DRIND(R1),1 TEST LEVEL 1                                 
         BH    EXEC2                                                            
         MVI   REJECT,C'N'         YES-RESET THE REJECT SWITCH                  
         MVI   MONEY,C'N'          RESET MONEY SWITCH                           
*                                                                               
EXEC2    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IKEY     MVI   SBEUNALL,C'N'       MAKE SURE NO UNALLOCATED IN FUTURE           
         CLC   SBPRD,=X'FEFEFE'    TEST PRODUCT UNALLOCATED                     
         BNE   *+12                                                             
         MVI   REJECT,C'Y'         YES-REJECT THIS RECORD                       
         B     XIT                                                              
*&&DO                                                                           
         CLI   SBMODE,SBPROCSP     TEST BUY RECORD                              
         BNE   IKEY1                                                            
         CLC   SBBPRD,SVPRD        YES-TEST CHANGE OF PRODUCT                   
         BE    IKEY1                                                            
         BAS   RE,GETPRD           YES-GET GF CODES FOR PRODUCT                 
         MVC   SVPRD,SBBPRD                                                     
*&&                                                                             
*                                                                               
IKEY1    MVC   GFCOMDIV,COMPYDIV                                                
         MVC   GFPRODCD,PRODCODE                                                
         MVC   GFAGY,AGENCYCD                                                   
         MVC   GFEST,SPACES                                                     
         MVC   GFEST(3),SBCLT                                                   
         MVC   GFEST+3(3),SBPRD                                                 
         MVC   GFEST+6(3),SBEST                                                 
         CLI   SBMODE,SBPROCBL     BILL RECORD --                               
         BNE   IKEY2                                                            
         MVC   FULL(2),STABPER     MONTH OF SERVICE (YYMM)                      
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB)                                 
         MVC   GFMON,DUB                                                        
         MVC   GFINV(1),SBMED      INVOIVE NUMBER                               
         MVI   GFINV+1,C'/'                                                     
         GOTO1 (RF),(R1),(2,STABBDT),DUB                                        
         MVC   GFINV+2(2),DUB+2                                                 
         MVI   GFINV+4,C'/'                                                     
**NOP    MVC   HALF,STABINV                                                     
**NOP    NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
**NOP    LH    RE,HALF                                                          
**NOP    CVD   RE,DUB                                                           
**NOP    OI    DUB+7,X'0F'                                                      
**NOP    UNPK  GFINV+5(4),DUB                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(SPFMTINO),DMCB,,(C'U',STABINV)                                
         L     RF,DMCB+4                                                        
         MVC   GFINV+5(4),0(RF)                                                 
*                                                                               
         MVI   GFBLTYPE,C'P'                                                    
         B     IKEY4                                                            
*                                                                               
IKEY2    DC    H'0'                                                             
*                                                                               
*&&DO                                                                           
IKEY2    CLI   SBMODE,SBPROCSP     BUY RECORD -                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,MONTAB                                                        
IKEY3    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,0(R1)                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,2(R1)                                                     
         BNH   *+12                                                             
         LA    R1,8(R1)                                                         
         B     IKEY3                                                            
         MVC   GFMON,4(R1)                                                      
**Y2K                                                                           
         MVC   WORK(4),GFMON                                                    
         MVC   WORK+4(2),=C'01'      DAY TO 01                                  
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',DUB)                                 
         MVC   GFMON(4),DUB                                                     
**Y2K                                                                           
         MVC   GFINV,XFF                                                        
         MVI   GFBLTYPE,C'X'                                                    
*&&                                                                             
*                                                                               
IKEY4    MVC   GFNAT,NATURAL                                                    
         MVC   GFSUBNAT,SUBNATRL                                                
         MVC   GFMKT,SBMKT                                                      
         MVC   0(L'GFKEY,R2),GFKEY                                              
         B     XIT                                                              
*                                                                               
OKEY     MVC   GFREC,SPACES        INIT GF RECORD TO SPACES                     
         MVC   GFKEY,0(R2)                                                      
         CLC   GFINV,XFF           TEST BUY RECORD DATA                         
         BNE   *+10                                                             
         MVC   GFINV,SPACES        YES-INVOICE=SPACES                           
         MVC   0(L'GFKEY,R3),GFKEY  GF KEY OUTPUT                               
         MVI   REJECT,C'N'                                                      
         MVI   MONEY,C'N'                                                       
         CLI   GFBLTYPE,C'P'       TEST TYPE=BILL                               
         BNE   *+14                                                             
         MVC   SVKEY,GFKEY         YES-SAVE LATEST BILL KEY                     
         B     XIT                                                              
*&&DO                                                                           
         CLI   GFBLTYPE,C'X'       TEST TYPE=BUY                                
         BNE   XIT                                                              
******** CLC   GFMON,BILLMON                                                    
******** BL    *+14                                                             
         CLC   GFMON,BILLMON       YES-REJECT ANY MONTH PRIOR TO                
         BL    *+14                    BILLING MONTH                            
         CLC   SVKEY,GFKEY         TEST KEY SAME AS LAST BILL KEY               
         BNE   XIT                                                              
         MVI   REJECT,C'Y'         YES-REJECT                                   
*&&                                                                             
         B     XIT                                                              
*                                                                               
INET     CLI   SBMODE,SBPROCBL     BILL RECORD -                                
         BNE   *+14                                                             
         MVC   0(4,R2),SBBILNET    NET INVOICE AMOUNT                           
         B     INETX                                                            
*&&DO                                                                           
         CLI   SBMODE,SBPROCSP     BUY RECORD -                                 
         BE    *+6                                                              
*&&                                                                             
         DC    H'0'                                                             
         MVC   0(4,R2),SCNET       NET ORDERED                                  
INETX    B     INETCOMX                                                         
*                                                                               
*                                  OUTPUT NET DOLLARS                           
ONET     ICM   R1,15,0(R2)                                                      
         BZ    *+8                                                              
         MVI   MONEY,C'Y'                                                       
         ST    R1,GFBNET                                                        
         CVD   R1,DUB                                                           
         UNPK  GFNET,DUB                                                        
         MVC   0(L'GFNET,R3),GFNET                                              
         B     XIT                                                              
*                                                                               
ICOM     CLI   SBMODE,SBPROCBL     COMMISSION                                   
         BNE   ICOM2                                                            
         BAS   RE,BILLFORM         APPLY BILL FORMULA TO GROSS                  
         L     RE,FULL                                                          
         L     RF,SBBILNET                                                      
         SR    RE,RF                                                            
         ST    RE,0(R2)                                                         
         B     ICOMX                                                            
*&&DO                                                                           
ICOM2    CLI   SBMODE,SBPROCSP                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
ICOM2    DC    H'0'                                                             
*                                                                               
         L     RE,SCEFFCST         BUY RECORD -                                 
         L     RF,SCNET                                                         
         SR    RE,RF                                                            
         ST    RE,0(R2)                                                         
ICOMX    B     INETCOMX                                                         
*                                                                               
INETCOMX OC    0(4,R2),0(R2)       TEST ANY MONEY                               
         BZ    *+8                                                              
         MVI   MONEY,C'Y'          YES                                          
         B     XIT                                                              
*                                                                               
*                                  OUTPUT COMMISSION DOLLARS                    
OCOM     ICM   R1,15,0(R2)                                                      
         BZ    *+8                                                              
         MVI   MONEY,C'Y'                                                       
         ST    R1,GFBCOM                                                        
         CVD   R1,DUB                                                           
         UNPK  GFCOM,DUB                                                        
         MVC   0(L'GFCOM,R3),GFCOM                                              
         B     XIT                                                              
*                                                                               
IDATE    MVC   0(4,R2),BILLMON     BILLING MONTH                                
         B     XIT                                                              
*                                                                               
ODATE    MVC   GFDATE,0(R2)                                                     
         MVC   0(L'GFDATE,R3),GFDATE                                            
         B     XIT                                                              
*                                                                               
*Y2K*                                                                           
******   NOTE - BILLMON WILL ALREADY BE IN CORRECT FORMAT NOW                   
*                                                                               
******   MVC   DUB(4),GFDATE       SET UP FOR DATCON CALL                       
******   MVC   DUB+4(2),=C'01'     SET DD FOR DATCON CALL                       
******   GOTO1 DATCON,DMCB,(0,DUB),(X'20',WORK) NO FUNNY DATES                  
******   MVC   0(4,R3),WORK        MOVE OUT THE DATE                            
******   B     XIT                                                              
*                                                                               
OFILLER  L     R1,GLADTENT                                                      
         ZIC   RE,DROLEN-DROD(R1)                                               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),SPACES                                                   
         B     XIT                                                              
*                                                                               
IERR     MVC   0(1,R2),ERRCD       ERRORS                                       
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         CLI   ERRCD,0                                                          
         BE    XIT                                                              
         LR    R1,R3                                                            
         TM    ERRCD,ERRAGY                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERRPRD                                                     
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   XIT                                                              
         MVI   0(R1),C' '                                                       
         B     XIT                                                              
*                                                                               
*                                  LAST COLUMN - OUTPUT TAPE RECORD             
LASTCOL  CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    XIT                 YES                                          
         CLI   MONEY,C'N'          TEST $0                                      
         BE    XIT                 YES                                          
         L     R3,AGFTEMP                                                       
         L     R5,AGFFILE                                                       
         LA    R6,GFREC                                                         
         CLI   TEMPOPEN,C'Y'       TEST TEMP FILE OPENED YET                    
         BE    LASTCOL2                                                         
         MVI   TEMPOPEN,C'Y'                                                    
         GOTO1 VDYNALLO,DMCB,(X'80',DDTEMP),(X'40',TMPALLOC)                    
         OPEN  ((R3),OUTPUT)       OPEN TEMP DISK FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LASTCOL2 CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    LASTCOL4                                                         
         CLI   TAPEOPEN,C'Y'       TEST TAPE OPENED YET                         
         BE    LASTCOL4                                                         
         MVI   TAPEOPEN,C'Y'                                                    
         MVC   DSNGF+13(2),SBAGY                                                
         GOTO1 VDYNALLO,DMCB,DDGF,DSNGF                                         
         OPEN  ((R5),OUTPUT)       OPEN TAPE FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LASTCOL4 MVC   GFMED,SBMED                                                      
         PUT   (R3),(R6)           PUT TO TEMP DISK FILE                        
         MVC   GFFILLER,SPACES                                                  
         AP    TAPECNT,=P'1'                                                    
         CLI   SUPTAP,C'Y'         OPTION TO SUPPRESS TAPE                      
         BE    XIT                                                              
         PUT   (R5),(R6)           PUT TO TAPE                                  
         B     XIT                                                              
         EJECT                                                                  
* INPUT/OUPUT ROUTINES FOR AGENCY REPORT                                        
*                                                                               
         SPACE 1                                                                
IAGENCY  MVC   0(1,R2),GFAGY                                                    
         B     XIT                                                              
*                                                                               
OAGENCY  MVC   0(6,R3),=C'AGENCY'                                               
         MVC   7(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
OMEDIA   MVC   0(5,R3),=C'MEDIA'                                                
         MVC   7(1,R3),0(R2)                                                    
         B     XIT                                                              
*                                                                               
ICOMPANY MVC   0(1,R2),GFCOMDIV                                                 
         B     XIT                                                              
*                                                                               
IDIV     MVC   0(1,R2),GFCOMDIV+1                                               
         B     XIT                                                              
*                                                                               
IPRDCODE MVC   0(4,R2),GFPRODCD                                                 
         B     XIT                                                              
*                                                                               
IESTIM   MVC   0(12,R2),GFEST                                                   
         B     XIT                                                              
*                                                                               
IMONTH   MVC   0(4,R2),GFMON                                                    
         B     XIT                                                              
*                                                                               
OMONTH   MVC   DUB(4),0(R2)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,DUB,(6,(R3))                                         
         B     XIT                                                              
*                                                                               
IINV     MVC   0(9,R2),GFINV                                                    
         B     XIT                                                              
*                                                                               
ITYPE    MVC   0(9,R2),GFBLTYPE                                                 
         B     XIT                                                              
*                                                                               
INAT     MVC   0(3,R2),GFNAT                                                    
         B     XIT                                                              
*                                                                               
ISUB     MVC   0(3,R2),GFSUBNAT                                                 
         B     XIT                                                              
*                                                                               
IMARKET  MVC   0(4,R2),GFMKT                                                    
         B     XIT                                                              
*                                                                               
I2NET    ZAP   0(8,R2),=P'0'                                                    
         ICM   RE,15,GFBNET                                                     
         BZ    XIT                                                              
         MVI   MONEY,C'Y'                                                       
         CVD   RE,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
*                                                                               
O2NET    TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BZ    *+10                                                             
         ZAP   TOTNET,0(R2)        YES-SAVE TOTAL NET                           
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
I2COM    ZAP   0(8,R2),=P'0'                                                    
         ICM   RE,15,GFBCOM                                                     
         BZ    XIT                                                              
         MVI   MONEY,C'Y'                                                       
         CVD   RE,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         B     XIT                                                              
*                                                                               
O2COM    TM    GLINDS,GLTOTLIN     TEST TOTAL                                   
         BZ    *+10                                                             
         ZAP   TOTCOM,0(R2)        YES-SAVE TOTAL COMMISSION                    
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
TOTAL    AP    TAPECNT,=P'1'       ADD ONE TO TAPE COUNT FOR TRAILER            
         MVC   0(7,R3),=C'**ALL**'                                              
         MVC   198(6,R3),=C'COUNT='                                             
         LA    R3,198+198(R3)                                                   
         UNPK  0(7,R3),TAPECNT     PRINT TAPE COUNT                             
         OI    6(R3),X'F0'                                                      
         B     XIT                                                              
         EJECT                                                                  
* APPLY BILL FORMULA TO A DOLLAR AMOUNT                                         
* INPUT  : R6=A(BILL ELEMENT)                                                   
* OUTPUT : FULL=BILL FORMULA APPLIED TO GROSS/NET                               
*          SBBILNET=EFFECTIVE NET BILLED                                        
*                                                                               
BILLFORM NTR1  ,                                                                
         USING STABELEM,R6                                                      
         L     R5,SBAIO1                                                        
         USING STABUCKD,R5                                                      
         CLC   SVBILKEY(STABKPRD-STABUCKD),0(R5)  TEST NEW AGY/MED/CLT          
         BE    *+8                                                              
         BAS   RE,GETPRDDF                        YES-GET DEFAULT FORM          
         CLC   SVBILKEY(STABKEST-STABUCKD),0(R5)  TEST PRODUCT CHANGE           
         BE    *+8                                                              
         BAS   RE,GETPRDBF                        YES-GET PRD BILL FORM         
         CLC   SVBILKEY(STABKMKT-STABUCKD),0(R5)  TEST PRD/EST CHANGE           
         BE    BILLFO1                                                          
         BAS   RE,GETESTBF                        YES-GET EST BILL FORM         
         CLC   STABKEST,SVBILKEY+(STABKEST-STABUCKD)  TEST EST CHANGE           
         BE    *+8                                                              
         BAS   RE,GETESTDF         YES-GET DEFAULT ESTIMATE BILL FORM           
         MVC   SVBILKEY,0(R5)                                                   
*                                                                               
BILLFO1  MVC   BLFORM,ESBILFOR     ESTIMATE FORMULA                             
         OC    ESBILFOR,ESBILFOR                                                
         BNZ   BILLFO2                                                          
         MVC   BLFORM,PRBILFOR     PRODUCT FORMULA                              
         CLC   PREFFDT,STABPER     TEST EFFECTIVE DATE                          
         BH    *+14                                                             
         OC    PRBILFOR,PRBILFOR                                                
         BNZ   BILLFO2                                                          
         MVC   BLFORM,EDBILFOR     DEFAULT ESTIMATE FORMULA                     
         OC    EDBILFOR,EDBILFOR                                                
         BNZ   BILLFO2                                                          
         MVC   BLFORM,PDBILBAS     DEFAULT PRODUCT FORMULA                      
         CLC   PDEFFDT,STABPER     TEST EFFECTIVE DATE                          
         BH    *+14                                                             
         OC    PDBILFOR,PDBILFOR                                                
         BNZ   BILLFO2                                                          
         XC    BLFORM,BLFORM                                                    
*                                  CALCULATE EFFECTIVE BILLING                  
BILLFO2  XC    SPBVALD(SPBVALDL),SPBVALD                                        
         GOTO1 SPBVAL,DMCB,(C'E',(R6)),SPBVALD,BLFORM                           
         MVC   FULL,SPBVACT                                                     
         MVC   SBBILNET,SPBVENET                                                
         B     BILLFOX                                                          
*                                                                               
*BILLFO2 SR    R4,R4               USING THE BILL FORMULA                       
         CLI   1(R6),19            R4=TAX                                       
         BNH   BILLFO3                                                          
         LA    R1,STABTAX                                                       
         CLI   1(R6),21                                                         
         BNH   *+8                                                              
         LA    R1,1(R1)                                                         
         ICM   R4,7,0(R1)                                                       
*                                                                               
BILLFO3  L     R2,STABGRS                                                       
         SR    R2,R4               R2=GROSS-TAX                                 
         L     R3,STABNET                                                       
         SR    R3,R4               R3=NET-TAX                                   
         SR    RF,RF                                                            
         OC    BLCOM,BLCOM                                                      
         BZ    BILLFO4                                                          
         SR    RE,RE                                                            
         LR    RF,R2                                                            
         TM    BLBASE,X'01'                                                     
         BZ    *+6                                                              
         LR    RF,R3                                                            
         M     RE,BLCOM                                                         
         SLDA  RE,1                                                             
         D     RE,=F'1000000'                                                   
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*                                                                               
BILLFO4  LR    RE,R2                                                            
         TM    BLBASE,X'10'                                                     
         BZ    *+6                                                              
         LR    RE,R3                                                            
         AR    RE,RF                                                            
         AR    RE,R4               ADD TAX BACK IN                              
         ST    RE,FULL                                                          
*                                                                               
BILLFOX  B     XIT                                                              
         EJECT                                                                  
* GET DEFAULT PRODUCT BILL FORMULA                                              
*                                                                               
GETPRDDF NTR1  ,                                                                
         XC    PDBILFOR,PDBILFOR                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PRDHDRD,R2          GET DEFAULT FORMULA                          
         MVI   PKEYTYPE,0          FOR PRODUCT AAA                              
         MVC   PKEYAM,STABKAM                                                   
         MVC   PKEYCLT,STABKCLT                                                 
         MVC   PKEYPRD,=C'AAA'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   PRDDF2                                                           
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   PDBILBAS,PBILLBAS                                                
         MVC   PDBILCOM,PBILLCOM                                                
         MVC   PDEFFDT,PBILLDT                                                  
*                                                                               
PRDDF2   L     RE,=A(BILFOTAB)     CLEAR TABLE FOR ESTIMATE DEFAULT             
         LA    RF,L'BILFOTAB       BILL FORMULAE                                
         MH    RF,=H'255'                                                       
         XCEFL ,                                                                
         XC    KEY,KEY             GET ESTIMATE DEFAULT FORMULAE                
         LA    R2,KEY              FOR PRODUCT AAA                              
         USING ESTHDRD,R2                                                       
         MVC   EKEYTYPE,0                                                       
         MVC   EKEYAM,STABKAM                                                   
         MVC   EKEYCLT,STABKCLT                                                 
         MVC   EKEYPRD,=C'AAA'                                                  
         MVI   EKEYEST,1                                                        
*                                                                               
PRDDF4   DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
PRDDF6   CLC   KEY(7),KEYSAVE                                                   
         BNE   PRDDFX                                                           
         OC    KEY+8(5),KEY+8      SKIP PAST THE BILL HEADERS                   
         BZ    *+14                                                             
         MVC   KEY+8(5),XFF                                                     
         B     PRDDF4                                                           
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         ZIC   R1,EKEYEST                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'BILFOTAB)                                                
         L     RE,=A(BILFOTAB)                                                  
         LA    R1,0(R1,RE)                                                      
         MVC   0(1,R1),EKEYEST                                                  
         MVC   1(1,R1),EBILLBAS                                                 
         MVC   2(4,R1),EBILLCOM                                                 
         GOTO1 SEQ                                                              
         LA    R2,KEY                                                           
         B     PRDDF6                                                           
*                                                                               
PRDDFX   B     XIT                                                              
         EJECT                                                                  
* GET PRODUCT BILL FORMULA                                                      
*                                                                               
GETPRDBF NTR1  ,                                                                
         XC    PRBILFOR,PRBILFOR                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PRDHDRD,R2          GET PRODUCT RECORD                           
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,STABKAM                                                   
         MVC   PKEYCLT,STABKCLT                                                 
         MVC   PKEYPRD,SBPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   PRDBFX                                                           
         OC    KEY+7(6),KEY+7                                                   
         BNZ   PRDBFX                                                           
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   PRBILBAS,PBILLBAS                                                
         MVC   PRBILCOM,PBILLCOM                                                
         MVC   PREFFDT,PBILLDT                                                  
*                                                                               
PRDBFX   B     XIT                                                              
         EJECT                                                                  
* GET ESTIMATE BILL FORMULA                                                     
*                                                                               
GETESTBF NTR1  ,                                                                
         XC    ESBILFOR,ESBILFOR                                                
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,STABKAM                                                   
         MVC   EKEYCLT,STABKCLT                                                 
         MVC   EKEYPRD,SBPRD                                                    
         MVC   EKEYEST,STABKEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   ESTBFX                                                           
         OC    KEY+8(5),KEY+8                                                   
         BNZ   ESTBFX                                                           
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   ESBILBAS,EBILLBAS                                                
         MVC   ESBILCOM,EBILLCOM                                                
*                                                                               
ESTBFX   B     XIT                                                              
         EJECT                                                                  
* GET ESTIMATE DEFAULT BILL FORMULA                                             
*                                                                               
GETESTDF NTR1  ,                                                                
         XC    EDBILFOR,EDBILFOR                                                
         ZIC   R1,STABKEST                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'BILFOTAB)                                                
         L     RE,=A(BILFOTAB)                                                  
         LA    R1,0(R1,RE)                                                      
         CLI   0(R1),0                                                          
         BE    ESTDFX                                                           
         MVC   EDBILBAS,1(R1)                                                   
         MVC   EDBILCOM,2(R1)                                                   
*                                                                               
ESTDFX   B     XIT                                                              
         EJECT                                                                  
* DRIVER ABOUT TO PUT TO SORT                                                   
*                                                                               
PUTSRT   CLI   REJECT,C'Y'         TEST RECORD REJECTED                         
         BE    *+12                                                             
         CLI   MONEY,C'N'          OR $0                                        
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       YES-TELL DRIVER TO REJECT                    
         B     XIT                                                              
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEADHK   L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEADLINE 5)                             
         CLI   GLOPTS+2,1                                                       
         BNE   *+14                                                             
         MVC   51(24,R2),=C'** GF TAPE RECORD MAP **'                           
         B     HEADHK2                                                          
         GOTO1 GENHEAD                                                          
         CLI   GLRECNO,3                                                        
         BNE   *+14                                                             
         MVC   54(18,R2),=C'** AGENCY RECAP **'                                 
         B     HEADHK2                                                          
         MVC   53(19,R2),=C'** AGENCY REPORT **'                                
         CLI   GLRECNO,2                                                        
         BNE   HEADHK2                                                          
         A     R2,PWIDTH                                                        
         MVC   53(19,R2),=C'*** TYPE TOTALS ***'                                
*                                                                               
HEADHK2  A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         MVC   51(16,R2),=C'BILLING MONTH OF'                                   
         MVC   68(6,R2),BILLMON6                                                
*                                                                               
HEADHKX  B     XIT                                                              
         EJECT                                                                  
* ABOUT TO PRINT A LINE                                                         
*                                                                               
PRINT    CLI   GLOPTS+2,2          TEST AGENCY REPORT                           
         BE    XIT                 YES-PRINT ALL LINES                          
         CLI   REJECT,C'Y'         IF LINE REJECTED                             
         BE    *+12                                                             
         CLI   MONEY,C'N'          OR $0,                                       
         BNE   *+8                                                              
         MVI   GLHOOK,GLDONT       TELL DRIVER                                  
         MVI   REJECT,C'N'                                                      
         MVI   MONEY,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
* FINAL HOOK                                                                    
*                                                                               
FINAL    L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVC   SVAGYCD,AGENCYCD                                                 
         MVC   SVTIT,TITLE                                                      
         MVC   SVSUBTIT,SUBTITLE                                                
         MVC   SVSPLID,SPOOLID                                                  
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
* RUNLAST                                                                       
*                                                                               
LST      L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CLI   TEMPOPEN,C'Y'       VERIFY ANY RECORDS WERE OUTPUT               
         BNE   LSTX                                                             
         GOTO1 INITDRIV            YES-INITIALIZE DRIVER                        
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHKNTR                                                       
         ST    R1,GLAHOOK                                                       
         MVI   GLOPTS+2,2          2ND REPORT = AGENCY REPORT                   
         OI    GLINDS,GLPALTOT                                                  
         MVI   MYFIRSTH,8                                                       
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         L     R3,AGFTEMP                                                       
         LA    R6,GFREC                                                         
         CLOSE ((R3))              CLOSE TEMP DISK FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  ((R3),INPUT)        OPEN TEMP DISK FILE FOR INPUT                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R1,LST90                                                         
         STCM  R1,7,33(R3)         RESET EODAD ADDRESS                          
*                                                                               
LST2     DS    0H                                                               
         GET   (R3),(R6)           GET A RECORD                                 
         MVC   SBMED,GFMED                                                      
         MVC   SBQMED,GFMED                                                     
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     LST2                                                             
*                                                                               
LST90    MVC   TITLE,SVTIT         RESTORE TITLES                               
         MVC   SUBTITLE,SVSUBTIT                                                
         MVC   SPOOLID,SVSPLID                                                  
         GOTO1 OPENPQ              INITIALIZE SPOOL                             
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GFREC,C'9'          TAPE TOTAL RECORD                            
         MVC   GFREC+1(L'GFREC-1),GFREC  ALL NINES                              
         MVC   GFAGY,SVAGYCD       AGENCY CODE                                  
         MVI   GFBLTYPE,C'T'       TYPE OF BILLING = T                          
         ZAP   DUB,TOTNET          TOTAL NET                                    
         UNPK  GFNET,DUB                                                        
         ZAP   DUB,TOTCOM          TOTAL COMMISSION                             
         UNPK  GFCOM,DUB                                                        
*                                                                               
         L     R3,AGFTEMP                                                       
         CLOSE ((R3))              CLOSE TEMP DISK FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAPEOPEN,C'Y'       TEST TAPE FILE WAS OPENED                    
         BNE   LSTX                                                             
*                                                                               
         L     R5,AGFFILE          YES-PUT TRAILER RECORD TO TAPE               
         LA    R6,GFREC                                                         
         PUT   (R5),(R6)                                                        
         CLOSE ((R5))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LSTX     B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
ASAVE    DS    A                                                                
VDYNALLO DS    A                                                                
TOTNET   DC    PL8'0'                                                           
TOTCOM   DC    PL8'0'                                                           
*                                                                               
QCOMPDIV DC    CL8'DIVBRNCD'                                                    
QPRDCODE DC    CL8'PROD CD '                                                    
QNATURAL DC    CL8'GF NTRL '                                                    
QSUBNATL DC    CL8'GFSBNTRL'                                                    
*                                                                               
COMPYDIV DS    CL2                 GF FIELDS                                    
PRODCODE DS    CL4                                                              
NATURAL  DS    CL3                                                              
SUBNATRL DS    CL3                                                              
AGENCYCD DS    CL1                                                              
*                                                                               
SVPRD    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVBILKEY DS    XL13                                                             
REJECT   DS    CL1                                                              
MONEY    DS    CL1                                                              
SUPTAP   DS    CL1                                                              
BILLMON  DS    CL4                                                              
BILLMON6 DS    CL6                                                              
*                                                                               
         DS    0F                                                               
         DS    CL3                                                              
BLFORM   DS    0CL5                BILL FORMULA                                 
BLBASE   DS    XL1                                                              
BLCOM    DS    F                                                                
*                                                                               
ESBILFOR DS    0CL5                ESTIMATE BILL FORMULA                        
ESBILBAS DS    XL1                                                              
ESBILCOM DS    XL4                                                              
PRBILFOR DS    0CL5                PRODUCT BILL FORMULA                         
PRBILBAS DS    XL1                                                              
PRBILCOM DS    XL4                                                              
PREFFDT  DS    XL2                 PRODUCT EFFECTIVE MONTH                      
EDBILFOR DS    0CL5                DEFAULT ESTIMATE BILL FORMULA                
EDBILBAS DS    XL1                                                              
EDBILCOM DS    XL4                                                              
PDBILFOR DS    0CL5                DEFAULT PRODUCT BILL FORMULA                 
PDBILBAS DS    XL1                                                              
PDBILCOM DS    XL4                                                              
PDEFFDT  DS    XL2                 DEFAULT PRODUCT EFFECTIVE MONTH              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRAGY   EQU   X'80'               AGENCY                                       
ERRPRD   EQU   X'40'               PRODUCT                                      
ERREST   EQU   X'20'               ESTIMATE                                     
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
*                                                                               
MONTAB   DS    25XL8               MONTH TABLE                                  
*                                                                               
DDGF     DC    CL8'GFFILE'                                                      
DSNGF    DC    CL20'SPTTAPE.SP0GFXX1'                                           
DDTEMP   DC    CL8'GFTEMP'                                                      
TMPALLOC DC    XL6'000001000001'                                                
         EJECT                                                                  
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0D                                                               
*                                                                               
AGFFILE  DS    A                                                                
AGFTEMP  DS    A                                                                
TAPECNT  DS    PL4                                                              
TAPEOPEN DS    CL1                                                              
TEMPOPEN DS    CL1                                                              
SVAGYCD  DS    CL1                                                              
SVTIT    DS    CL63                                                             
SVSUBTIT DS    CL32                                                             
SVSPLID  DS    CL3                                                              
SAVVALX  DS    0D                                                               
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
* EQUATES BELOW ARE USED TO COMPUTE ADDRESS OF SAVED DCBS                       
* DCBS ARE NOT SAVED OR RESTORED                                                
*                                                                               
         DS    0D                                                               
SVDCB1   EQU   SAVVALX-SAVVALS+512   DSPL TO SAVED DCB1                         
SVDCB2   EQU   SAVVALX-SAVVALS+640   DSPL TO SAVED DCB2                         
         EJECT                                                                  
* THESE DCBS ARE MOVED AT RUNFRST TO SPFUSER                                    
* THE ADDRESSES ARE IN THE SAVE AREA ABOVE                                      
         DS    0D                                                               
         DC    CL8'*GFFILE*'                                                    
GFFILE   DCB   DDNAME=GFFILE,DSORG=PS,LRECL=80,BLKSIZE=800,            X        
               MACRF=(GM,PM),RECFM=FB                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*GFTEMP*'                                                    
GFTEMP   DCB   DDNAME=GFTEMP,DSORG=PS,LRECL=80,BLKSIZE=800,            X        
               MACRF=(GM,PM),RECFM=FB,EODAD=LST90                               
         DS    0D                                                               
         DC    CL8'*GFREC*'                                                     
GFREC    DS    0CL80           *** GF RECORD ***                                
*                                                                               
GFKEY    DS    0CL43                                                            
GFCOMDIV DS    CL2                                                              
GFPRODCD DS    CL4                                                              
GFAGY    DS    CL1                                                              
GFEST    DS    CL12                                                             
GFMON    DS    CL4                                                              
GFINV    DS    CL9                                                              
GFBLTYPE DS    CL1                                                              
GFNAT    DS    CL3                                                              
GFSUBNAT DS    CL3                                                              
GFMKT    DS    CL4                                                              
*                                                                               
GFNET    DS    CL10                                                             
GFCOM    DS    CL10                                                             
*                                                                               
GFDATE   DS    CL4                                                              
*                                                                               
GFFILLER DS    CL13                                                             
         ORG   GFFILLER                                                         
GFMED    DS    CL1                                                              
GFBNET   DS    XL4                                                              
GFBCOM   DS    XL4                                                              
         ORG                                                                    
         SPACE 2                                                                
SVKEY    DS    CL(GFMON-GFKEY+L'GFMON)    SAVED KEY                             
         EJECT                                                                  
* PRODUCT AND ESTIMATE TABLES FOR GF CODES                                      
*                                                                               
PRDTAB   DS    255XL(L'COMPYDIV+L'PRODCODE)                                     
*                                                                               
ESTTAB   DS    255XL(L'NATURAL+L'SUBNATRL)                                      
         SPACE 2                                                                
* ESTIMATE DEFAULT BILL FORMULA TABLE                                           
*                                                                               
BILFOTAB DS    255XL6                                                           
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENPGEST                                                                     
*SPGENSTAB                                                                      
*SPGENPRD                                                                       
*SPGENEST                                                                       
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENSTAB                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
       ++INCLUDE DDMASTD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPWRI11   12/23/04'                                      
         END                                                                    
