*          DATA SET DMWRKZC    AT LEVEL 001 AS OF 03/22/11                      
*PHASE WRKZCA                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE SORTER                                                                 
         TITLE '$WRKZC - CONVERT WRKF/WRKZ FILES FROM TAPE TO TAPE'             
         PRINT NOGEN                                                            
WRKZC    CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**WKZZ**,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(WKBUFF-WORKD) ADDRESS OUT OF RANGE WORK                    
         AR    R1,RC                                                            
         ST    R1,AWKBUFF                                                       
         L     R1,=A(CTIO-WORKD)                                                
         AR    R1,RC                                                            
         ST    R1,ACTIO                                                         
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**CTIO**'                                             
*                                                                               
         MVC   ERRINF,SPACES                                                    
         MVC   WRKFILE,WRKFIL                                                   
         MVC   WRKZILE,WRKZIL                                                   
*                                                                               
         LA    R7,IOAREA           R7=IOAREA                                    
         USING FWLHDRD,R7                                                       
         LA    R8,IOAREA           R8=IOAREA                                    
         USING ZWLHDRD,R8                                                       
*                                                                               
         USING FUKRECD,FWKKEY                                                   
         USING ZUKRECD,ZWKKEY                                                   
*                                                                               
         USING PLINED,PLINE                                                     
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,OPENALL          OPEN FILES                                   
         BAS   RE,MAIN             MAIN LOOP                                    
         BAS   RE,CLOSEALL         CLOSE FILES                                  
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*        INITIALISE                                                             
***********************************************************************         
INIT     NTR1                                                                   
         LA    R3,CARD                                                          
INIT002  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'DDSIO=',0(R3)                                                 
         BNE   INIT003                                                          
         L     RF,=V(DDSIO)        OVERRIDE DDSIO NAME                          
         MVC   0(8,RF),6(R3)                                                    
         B     INIT002                                                          
*                                                                               
INIT003  CLC   =C'DSPACE=',0(R3)                                                
         BNE   INIT004                                                          
         L     RF,=A(SSB)          SET DSPACE ID IN SSB                         
         MVC   SSODSPAC-SSOOFF(1,RF),7(R3)                                      
         B     INIT002                                                          
*                                                                               
INIT004  GOTO1 =V(DATAMGR),DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                  
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(1,TODAY)                                  
         GOTO1 (RF),(R1),(5,0),(2,TODAYC)                                       
         BAS   RE,GETTIME                                                       
*                                                                               
         MVC   TITLE,TITLE1                                                     
         LA    R1,TITLE            PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,CARD                                                          
         B     INIT012                                                          
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
INIT012  CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
INIT015  LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT010                                                          
         DC    H'0'                Make sure someone knows about it             
*                                                                               
INIT020  MVI   RCWRKF,C' '         Clear progress message flags                 
         XC    RCWRKFC,RCWRKFC                                                  
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
***********************************************************************         
*        OPEN FILES                                                             
***********************************************************************         
OPENALL  NTR1                                                                   
*                                                                               
         OPEN  (TAPEIN,INPUT)      OPEN INPUT                                   
         OPEN  (TAPEOUT,OUTPUT)    OPEN OUTPUT                                  
*                                                                               
         CLI   REPFLG,C'N'         TEST FOR REPORT=NO                           
         BE    OPENALLX                                                         
         CLI   INPUT+3,C'Z'                                                     
         BE    OPENALL1                                                         
         GOTO1 =V(SORTER),DMCB,SRTCARD,RECCARD                                  
         B     OPENALLX                                                         
OPENALL1 GOTO1 =V(SORTER),DMCB,SRTCARZ,RECCARZ                                  
*                                                                               
OPENALLX B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        READ DATA LINES AND WRITE TO WRKZ                                      
***********************************************************************         
MAIN     NTR1                                                                   
         ST    RD,MAINRD           SAVE THIS RD FOR TAPEEND EXIT                
*                                                                               
MAIN010  MVI   FERI,0              INIT FILE ERROR INDICATOR                    
*                                                                               
         BAS   RE,GETFILE          GET A WRKZ FILE                              
         BNE   TAPEEND             EXIT WHEN NO MORE                            
*                                                                               
         BAS   RE,PUTSORT          PUT REPORT DATA TO SORT                      
*                                                                               
         BAS   RE,PUTREC           OPEN THE FILE                                
         BE    MAIN020                                                          
         BAS   RE,PUTERRS          OUTPUT ERROR MESSAGE                         
         B     MAIN110                                                          
*                                                                               
MAIN020  BAS   RE,READREC          READ A RECORD                                
         BNE   MAIN050                                                          
*                                                                               
         BAS   RE,PUTREC           WRITE A RECORD                               
         BE    MAIN020                                                          
         BAS   RE,PUTERRS          OUTPUT ERROR MESSAGE                         
         B     MAIN110                                                          
*                                                                               
MAIN050  BAS   RE,PUTREC           WRITE EOF RECORD                             
         BE    MAIN010                                                          
         BAS   RE,PUTERRS          OUTPUT ERROR MESSAGE                         
         B     MAIN010                                                          
*                                                                               
MAIN110  B     MAIN020                                                          
*                                                                               
TAPEEND  L     RD,MAINRD           RESTORE RD                                   
*                                                                               
         MVI   WRKFILE+4,C' '      No more WRKFs                                
         MVI   WRKZILE+4,C' '      No more WRKZs                                
*                                                                               
         BAS   RE,REPORT           WRITE A REPORT                               
*                                                                               
MAINX    B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET NEXT FILE FROM INPUT SOURCE                                        
***********************************************************************         
GETFILE  NTR1                                                                   
         CLI   INPUT+3,C'Z'                                                     
         BE    GETF020                                                          
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
GETF010  GET   TAPEIN,IOAREAF      GET UNTIL SOF                                
         CLC   FWLSOFLAB,=C'*SOFSOF*'                                           
         BNE   GETF010                                                          
         MVC   FCSOFNDX,FWLINDEX                                                
*                                                                               
         MVI   FWLFLAG,0           ENSURE FLAGS ARE ZERO                        
         MVC   FWKKEY,FWLUSRID     SET USERID FROM KEY                          
         CLI   FWLREPFNO,0                                                      
         BE    *+10                                                             
         MVC   WRKFILE+4(1),FWLREPFNO                                           
*                                                                               
*        OC    FWLRECS,FWLRECS     IGNORE FILES WITH NO RECORDS                 
*        BZ    GETF010                                                          
*        TM    FWLSTAT,FW_STRUN    IGNORE FILE WITH A TEMP STATUS               
*        BNZ   GETF010                                                          
*                                                                               
         CLI   REPLACE,C'Y'        PUT BACK WITH SAME REF#                      
         BNE   *+8                                                              
         OI    FWLFLAG,FWLFLREFN                                                
*                                                                               
         CLI   RESET,C'Y'          RESET RETAIN TIME                            
         BNE   *+12                                                             
         OI    FWLFLAG,FWLFLRSET   SET RESET FLAG                               
         B     *+8                                                              
         OI    FWLFLAG,FWLFLRCOP   OR COPY FLAG                                 
         B     GETFILX                                                          
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
GETF020  GET   TAPEIN,IOAREAF      GET UNTIL SOF                                
         CLC   ZWLSOFLAB,=C'*SOFSOF*'                                           
         BNE   GETF020                                                          
         MVC   ZCSOFNDX,ZWLINDEX                                                
*                                                                               
         MVI   ZWLFLAG,0           ENSURE FLAGS ARE ZERO                        
         MVC   ZWKKEY,ZWLUSRID     SET USERID FROM KEY                          
         CLI   ZWLREPFNO,0                                                      
         BE    *+10                                                             
         MVC   WRKZILE+4(1),ZWLREPFNO                                           
*                                                                               
*        OC    ZWLRECS,ZWLRECS     IGNORE FILES WITH NO RECORDS                 
*        BZ    GETF020                                                          
*        TM    ZWLSTAT,ZW_STRUN    IGNORE FILE WITH A TEMP STATUS               
*        BNZ   GETF020                                                          
*                                                                               
         CLI   REPLACE,C'Y'        PUT BACK WITH SAME REF#                      
         BNE   *+8                                                              
         OI    ZWLFLAG,ZWLFLREFN                                                
*                                                                               
         CLI   RESET,C'Y'          RESET RETAIN TIME                            
         BNE   *+12                                                             
         OI    ZWLFLAG,ZWLFLRSET   SET RESET FLAG                               
         B     *+8                                                              
         OI    ZWLFLAG,ZWLFLRCOP   OR COPY FLAG                                 
*                                                                               
GETFILX  B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET NEXT WRK RECORD                                                    
***********************************************************************         
READREC  NTR1                                                                   
*                                                                               
         GET   TAPEIN,IOAREAF                                                   
*                                                                               
         CLI   INPUT+3,C'Z'                                                     
         BE    READ010                                                          
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
         CLC   FWLSOFLAB,=C'*EOFEOF*'     END OF FILE                           
         BE    EXITNE                                                           
         CLC   FWLSOFLAB,=C'*EOFERR*'     END OF FILE ERROR                     
         BE    EXITNE                                                           
         CLC   FWLSOFLAB+2(6),=C'OFEERR'  BAD DMWRKZ END OF FILE ERROR          
         BNE   READRX                                                           
         MVC   FWLSOFLAB,=C'*EOFERR*'                                           
         MVC   IOAREAF,=X'000C0000'                                             
         B     EXITNE                                                           
*-----------------------------------------                                      
* WRKZ                                                                          
*-----------------------------------------                                      
READ010  CLC   ZWLSOFLAB,=C'*EOFEOF*'     END OF FILE                           
         BE    EXITNE                                                           
         CLC   ZWLSOFLAB,=C'*EOFERR*'     END OF FILE ERROR                     
         BE    EXITNE                                                           
         CLC   ZWLSOFLAB+2(6),=C'OFEERR'  BAD DMWRKZ END OF FILE ERROR          
         BNE   READRX                                                           
         MVC   ZWLSOFLAB,=C'*EOFERR*'                                           
         MVC   IOAREAF,=X'000C0000'                                             
         B     EXITNE                                                           
*                                                                               
READRX   B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PUT NEXT WRK RECORD                                                    
***********************************************************************         
PUTREC   NTR1                                                                   
         MVI   DMCB+8,0            CLEAR ERROR FLAG                             
*                                                                               
         CLC   IOAREAF(2),=H'0004'   Deal with bad record length                
         BL    PUTR060                                                          
         CLC   IOAREAF(2),=AL2(WRKFRECL)                                        
         BH    PUTR060                                                          
*                                                                               
         CLI   INPUT+3,C'Z'                                                     
         BE    PUTR040                                                          
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
         CLC   FWLSOFLAB,=C'*EOFEOF*' END-OF-FILE                               
         BNE   PUTR010                                                          
         CLI   FERI,FERL             Previous rec in file had bad len?          
         BNE   PUTR010               . NO                                       
         MVC   FWLSOFLAB,=C'*EOFERR*' . YES, mark file as bad                   
         MVI   FERI,0                Clear file error indicator                 
*                                                                               
PUTR010  CLI   OUTPUT+3,C'Z'                                                    
         BNE   PUTR020                                                          
         CLC   FWLSOFLAB,=C'*SOFSOF*'                                           
         BNE   PUTR020                                                          
         BAS   RE,FTOZ                                                          
*                                                                               
PUTR020  PUT   TAPEOUT,IOAREAF                                                  
*                                                                               
         CLC   FWLSOFLAB,=C'*EOFERR*' END-OF-FILE error?                        
         BNE   EXITEQ                                                           
         MVI   DMCB+8,X'80'          Simulate END-OF-FILE error                 
         MVC   ERRINF,FCSOFNDX                                                  
         B     EXITNE                                                           
*-----------------------------------------                                      
* WRKZ                                                                          
*-----------------------------------------                                      
PUTR040  CLC   ZWLSOFLAB,=C'*EOFEOF*' END-OF-FILE                               
         BNE   PUTR050                                                          
         CLI   FERI,FERL             Previous rec in file had bad len?          
         BNE   PUTR050               . NO                                       
         MVC   ZWLSOFLAB,=C'*EOFERR*' . YES, mark file as bad                   
         MVI   FERI,0                Clear file error indicator                 
*                                                                               
PUTR050  CLI   OUTPUT+3,C'F'                                                    
         BNE   PUTR055                                                          
         CLC   ZWLSOFLAB,=C'*SOFSOF*'                                           
         BNE   PUTR020                                                          
         BAS   RE,ZTOF                                                          
*                                                                               
PUTR055  PUT   TAPEOUT,IOAREAF                                                  
*                                                                               
         CLC   ZWLSOFLAB,=C'*EOFERR*' END-OF-FILE error?                        
         BNE   EXITEQ                                                           
         MVI   DMCB+8,X'80'          Simulate END-OF-FILE error                 
         MVC   ERRINF,ZCSOFNDX                                                  
         B     EXITNE                                                           
*                                                                               
PUTR060  MVI   DMCB+8,X'41'          Simulate format error for bad len          
         MVC   ERRINF(6),=C'RECLN='                                             
         MVI   FERI,FERL                                                        
         GOTO1 =V(HEXOUT),PLIST,IOAREAF,ERRINF+6,2                              
         B     EXITNE                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CLOSE ALL AND EXIT                                                     
***********************************************************************         
CLOSEALL NTR1                                                                   
*                                                                               
         CLOSE TAPEIN              CLOSE TAPEIN                                 
         CLOSE TAPEOUT             CLOSE TAPEOUT                                
         B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PUT INDEX RECORD TO SORTER                                             
***********************************************************************         
PUTSORT  ST    RE,SAVERE                                                        
*                                                                               
         CLI   REPFLG,C'N'               TEST FOR REPORT=NO                     
         BE    PUTSORTX                                                         
*                                                                               
         CLI   INPUT+3,C'Z'                                                     
         BE    PUTSORT1                                                         
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
         MVC   FWLINDEX-1(1),WRKFILE+4                                          
         MVC   BYTE,FWLSOFEND                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',FWLINDEX-1                               
         MVC   FWLSOFEND(1),BYTE                                                
         B     PUTSORTX                                                         
*-----------------------------------------                                      
* WRKZ                                                                          
*-----------------------------------------                                      
PUTSORT1 MVC   ZWLINDEX-1(1),WRKZILE+4                                          
         MVC   BYTE,ZWLSOFEND                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',ZWLINDEX-1                               
         MVC   ZWLSOFEND(1),BYTE                                                
*                                                                               
PUTSORTX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT AN ERROR LINE                                                    
***********************************************************************         
PUTERRS  NTR1                                                                   
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+1(10),=C'PUT ERROR '                                       
*                                                                               
         TM    DMCB+8,X'41'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'FORMAT ERR'                                       
         B     PUTERRS1                                                         
*                                                                               
         TM    DMCB+8,X'81'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'TOO BIG   '                                       
         B     PUTERRS1                                                         
*                                                                               
         TM    DMCB+8,X'40'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'DISK ERROR'                                       
         B     PUTERRS1                                                         
*                                                                               
         TM    DMCB+8,X'80'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'EOF ERROR '                                       
         B     PUTERRS1                                                         
*                                                                               
PUTERRS1 MVC   PLINE+12(10),ERRINF                                              
         MVC   ERRINF,SPACES                                                    
         LA    RF,ZCSOFNDX                                                      
         CLI   INPUT+3,C'Z'                                                     
         BE    *+8                                                              
         LA    RF,FCSOFNDX                                                      
         GOTO1 =V(HEXOUT),PLIST,(RF),PLINE+23,20                                
         BAS   RE,PRINTL                                                        
         LA    R1,DMCB                                                          
         B     EXITEQU                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT A WRKZ REPORT                                                    
***********************************************************************         
REPORT   NTR1                                                                   
*                                                                               
         CLI   INPUT+3,C'Z'                                                     
         BE    REP100                                                           
*-----------------------------------------                                      
* WRKF                                                                          
*-----------------------------------------                                      
         ZAP   LINE,=P'99'         START WITH A TITLE LINE                      
*                                                                               
REP010   MVC   PLINE,T2                                                         
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BZ    REPORTX                                                          
         MVC   FWLINDEX-1(126),0(RF)                                            
*                                                                               
         OC    GUSER,GUSER         NO TOTALS FIRST TIME                         
         BZ    REP015                                                           
         CLC   FWLUSRID,GUSER      SKIP IF SAME AS PREV                         
         BE    REP020                                                           
         BAS   RE,TOTALS           PRINT USER TOTALS                            
*                                                                               
         CLC   CUSERF,FWLINDEX-1                                                
         BE    REP014                                                           
*                                                                               
         BAS   RE,FTOTALS          PRINT FILE TOTALS                            
         ZAP   LINE,=P'99'                                                      
         MVC   PLINE,T2                                                         
         B     REP015                                                           
*                                                                               
REP014   BAS   RE,PRINTL                                                        
         MVC   PLINE,T2                                                         
*                                                                               
REP015   MVC   GUSER,FWLUSRID      GET USERID NAME                              
         BAS   RE,GETUSR                                                        
*                                                                               
REP020   BAS   RE,COUNT            KEEP A COUNT OF TOTALS                       
         MVC   PLUSER,GUSERN                                                    
         MVC   PLWRKF(1),FWLINDEX-1 WRKZ FILE NUMBER                            
         MVC   DUB,FWLKEY                                                       
         BAS   RE,GETKEY           FILE KEY AND REF#                            
         MVC   PLKEY,WORK                                                       
         MVC   HALF,FWLFILENO                                                   
         BAS   RE,EDITH                                                         
         MVC   PLREFNO,DUB+3                                                    
*                                                                               
         MVC   PLDESC,FWLDESC                                                   
         MVC   PLTYPE,SPACES                                                    
         MVC   PLTYPE+1(1),FWLTYPE                                              
*                                                                               
REP040   MVC   PLATTR,=C'O.EP.X.S'                                              
         LA    RF,PLATTR                                                        
         MVC   BYTE,FWLATTB         SET /CLR ATTR BITS                          
         LA    R1,X'80'                                                         
REP041   EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BO    *+8                                                              
         MVI   0(RF),C' '          REPLACE WITH ' ' IF ZERO                     
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   REP041                                                           
*                                                                               
REP050   MVC   BYTE,FWLSTAT                                                     
         BAS   RE,STATOUT                                                       
         MVC   PLSTAT,DUB                                                       
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),FWLAGES                                                
         BAS   RE,EDITH                                                         
         MVC   PLSIZE,DUB+5                                                     
*                                                                               
         MVC   PLLIVED,DOTS                                                     
         GOTO1 =V(DATCON),DMCB,(2,FWLDATEL),(17,PLLIVED)                        
         MVC   PLDEADD,DOTS                                                     
         OC    FWLDATED,FWLDATED                                                
         BZ    REP060                                                           
         GOTO1 (RF),(R1),(2,FWLDATED),(17,PLDEADD)                              
REP060   MVC   PLRETAD,DOTS                                                     
         GOTO1 (RF),(R1),(2,FWLAGERD),(17,PLRETAD)                              
*                                                                               
         MVC   DUB(2),FWLTIMEL                                                  
         BAS   RE,TIMEOUT                                                       
         MVC   PLLIVET,DUB+2                                                    
*                                                                               
REP070   MVC   PLDEADT,DOTS                                                     
         MVC   PLSENTO,DOTS                                                     
         MVC   DUB(2),FWLTIMED                                                  
         OC    DUB(2),DUB                                                       
         BZ    REP080                                                           
         BAS   RE,TIMEOUT                                                       
         MVC   PLDEADT,DUB+2                                                    
         MVC   PLSENTO,FWLPRSYM                                                 
*                                                                               
REP080   SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,FWLAGERT                                                      
         MH    R1,=H'10'           CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   PLRETAT,DUB+2       RETAIN TIME                                  
*                                                                               
         MVC   FULL,FWLRECS                                                     
         BAS   RE,EDITF                                                         
         MVC   PLRECS,DUB+3                                                     
         MVC   FULL,FWLFSIZE                                                    
         BAS   RE,EDITF                                                         
         MVC   PLBYTES,DUB+3                                                    
         MVC   HALF,FWLAVGRL                                                    
         BAS   RE,EDITH                                                         
         MVC   PLAVG,DUB+3                                                      
         MVC   HALF,FWLMAXRL                                                    
         BAS   RE,EDITH                                                         
         MVC   PLMAX,DUB+3                                                      
         MVC   BYTE,FWLNCI                                                      
         BAS   RE,EDITB                                                         
         MVC   PLCIS,DUB+5                                                      
         MVC   BYTE,FWLNCIX                                                     
         BAS   RE,EDITB                                                         
         MVC   PLCISX,DUB+5                                                     
*                                                                               
         BAS   RE,PRINTL                                                        
         B     REP010                                                           
*-----------------------------------------                                      
* WRKZ                                                                          
*-----------------------------------------                                      
REP100   ZAP   LINE,=P'99'         START WITH A TITLE LINE                      
*                                                                               
REP110   MVC   PLINE,T2                                                         
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BZ    REPORTX                                                          
         MVC   ZWLINDEX-1(168),0(RF)                                            
*                                                                               
         OC    GUSER,GUSER         NO TOTALS FIRST TIME                         
         BZ    REP115                                                           
         CLC   ZWLUSRID,GUSER       SKIP IF SAME AS PREV                        
         BE    REP120                                                           
         BAS   RE,TOTALS           PRINT USER TOTALS                            
*                                                                               
         CLC   CUSERF,ZWLINDEX-1                                                
         BE    REP114                                                           
*                                                                               
         BAS   RE,FTOTALS          PRINT FILE TOTALS                            
         ZAP   LINE,=P'99'                                                      
         MVC   PLINE,T2                                                         
         B     REP115                                                           
*                                                                               
REP114   BAS   RE,PRINTL                                                        
         MVC   PLINE,T2                                                         
*                                                                               
REP115   MVC   GUSER,ZWLUSRID       GET USERID NAME                             
         BAS   RE,GETUSR                                                        
*                                                                               
REP120   BAS   RE,COUNT            KEEP A COUNT OF TOTALS                       
         MVC   PLUSER,GUSERN                                                    
         MVC   PLWRKF(1),ZWLINDEX-1 WRKZ FILE NUMBER                            
         MVC   DUB,ZWLKEY                                                       
         BAS   RE,GETKEY           FILE KEY AND REF#                            
         MVC   PLKEY,WORK                                                       
         MVC   FULL,ZWLFILENO                                                   
         BAS   RE,EDITF                                                         
         MVC   PLREFNO,DUB+3                                                    
*                                                                               
         MVC   PLDESC,ZWLDESC                                                   
         MVC   PLTYPE,SPACES                                                    
         MVC   PLTYPE+1(1),ZWLTYPE                                              
*                                                                               
REP140   MVC   PLATTR,=C'O.EP.X.S'                                              
         LA    RF,PLATTR                                                        
         MVC   BYTE,ZWLATTB        SET /CLR ATTR BITS                           
         LA    R1,X'80'                                                         
REP141   EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0                                                           
         BO    *+8                                                              
         MVI   0(RF),C' '          REPLACE WITH ' ' IF ZERO                     
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   REP141                                                           
*                                                                               
REP150   MVC   BYTE,ZWLSTAT                                                     
         BAS   RE,STATOUT                                                       
         MVC   PLSTAT,DUB                                                       
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),ZWLAGES                                                
         BAS   RE,EDITH                                                         
         MVC   PLSIZE,DUB+5                                                     
*                                                                               
         MVC   PLLIVED,DOTS                                                     
         GOTO1 =V(DATCON),DMCB,(2,ZWLDATEL),(17,PLLIVED)                        
         MVC   PLDEADD,DOTS                                                     
         OC    ZWLDATED,ZWLDATED                                                
         BZ    REP160                                                           
         GOTO1 (RF),(R1),(2,ZWLDATED),(17,PLDEADD)                              
REP160   MVC   PLRETAD,DOTS                                                     
         GOTO1 (RF),(R1),(2,ZWLAGERD),(17,PLRETAD)                              
*                                                                               
         MVC   DUB(2),ZWLTIMEL                                                  
         BAS   RE,TIMEOUT                                                       
         MVC   PLLIVET,DUB+2                                                    
*                                                                               
REP170   MVC   PLDEADT,DOTS                                                     
         MVC   PLSENTO,DOTS                                                     
         MVC   DUB(2),ZWLTIMED                                                  
         OC    DUB(2),DUB                                                       
         BZ    REP180                                                           
         BAS   RE,TIMEOUT                                                       
         MVC   PLDEADT,DUB+2                                                    
         MVC   PLSENTO,ZWLPRSYM                                                 
*                                                                               
REP180   SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,ZWLAGERT                                                      
         MH    R1,=H'10'           CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   PLRETAT,DUB+2       RETAIN TIME                                  
*                                                                               
         MVC   FULL,ZWLRECS                                                     
         BAS   RE,EDITF                                                         
         MVC   PLRECS,DUB+3                                                     
         MVC   FULL,ZWLFSIZE                                                    
         BAS   RE,EDITF                                                         
         MVC   PLBYTES,DUB+3                                                    
         MVC   HALF,ZWLAVGRL                                                    
         BAS   RE,EDITH                                                         
         MVC   PLAVG,DUB+3                                                      
         MVC   HALF,ZWLMAXRL                                                    
         BAS   RE,EDITH                                                         
         MVC   PLMAX,DUB+3                                                      
         MVC   BYTE,ZWLNCI                                                      
         BAS   RE,EDITB                                                         
         MVC   PLCIS,DUB+5                                                      
         MVC   BYTE,ZWLNCIX                                                     
         BAS   RE,EDITB                                                         
         MVC   PLCISX,DUB+5                                                     
*                                                                               
         BAS   RE,PRINTL                                                        
         B     REP110                                                           
*                                                                               
REPORTX  BAS   RE,TOTALS           PRINT USER TOTALS                            
         BAS   RE,FTOTALS          PRINT FILE TOTALS                            
         B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT TOTALS LINE                                                      
***********************************************************************         
TOTALS   NTR1                                                                   
*                                                                               
         MVC   PLINE,T4            CLOSE OFF BOX                                
         MVI   PBOX1,ML                                                         
         MVI   PBOXX,MR                                                         
         BAS   RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         MVI   PBOX1,VB                                                         
         MVI   PBOXX,VB                                                         
         MVC   PLINE+2(8),CUSERN                                                
         MVC   PLINE+10(3),=C'WK='                                              
         MVC   PLINE+13(1),CUSERF                                               
         LA    R2,UCTFILE                                                       
         BAS   RE,PCOUNTS                                                       
         MVC   PLINE+15(48),MYWORK                                              
         MVC   PLINE+64(7),=C'ACTIVE '                                          
         LA    R2,UCAFILE                                                       
         BAS   RE,PCOUNTS                                                       
         MVC   PLINE+72(48),MYWORK                                              
         BAS   RE,PRINTL                                                        
         XC    UCOUNTS,UCOUNTS                                                  
         MVC   PLINE,T1            SET UP FOR NEXT BOX                          
         MVI   PLINE,C' '                                                       
         MVI   PBOX1,ML                                                         
         MVI   PBOXX,MR                                                         
TOTALSX  B     EXITEQ                                                           
*                                                                               
FTOTALS  NTR1                                                                   
*                                                                               
         MVI   PLINE,HB                                                         
         MVC   PLINE+1(155),PLINE                                               
         MVI   PBOX1,ML                                                         
         MVI   PBOXX,MR                                                         
         BAS   RE,PRINTL                                                        
         MVC   PLINE,SPACES                                                     
         MVI   PBOX1,VB                                                         
         MVI   PBOXX,VB                                                         
         MVC   PLINE+2(8),=C'TOTALS  '                                          
         MVC   PLINE+10(3),=C'WK='                                              
         MVC   PLINE+13(1),CUSERF                                               
         LA    R2,TCTFILE                                                       
         BAS   RE,PCOUNTS                                                       
         MVC   PLINE+15(48),MYWORK                                              
         MVC   PLINE+64(7),=C'ACTIVE '                                          
         LA    R2,TCAFILE                                                       
         BAS   RE,PCOUNTS                                                       
         MVC   PLINE+72(48),MYWORK                                              
*                                                                               
         BAS   RE,PRINTL                                                        
         XC    TCOUNTS,TCOUNTS                                                  
         MVI   PLINE,HB                                                         
         MVC   PLINE+1(155),PLINE                                               
         MVI   PBOX1,BL                                                         
         MVI   PBOXX,BR                                                         
         BAS   RE,PRINTL                                                        
FTOTALSX B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        COUNT TOTALS FOR USER / ALL                                            
***********************************************************************         
COUNT    NTR1                                                                   
*                                                                               
         CLI   INPUT+3,C'Z'                                                     
         BE    CNT100                                                           
*                                                                               
         SR    R1,R1               SET R0 TO NUMBER OF PART 2S                  
         SR    R0,R0                                                            
         IC    R1,FWLNCI                                                        
         IC    R0,FWLNCIX                                                       
         BCTR  R1,0                                                             
         AR    R0,R1                                                            
*                                                                               
         MVC   CUSERN,GUSERN       SAVE NAME                                    
         MVC   CUSERF,FWLINDEX-1    SAVE NAFILE NUMBER                          
*                                                                               
         LA    RF,TCTFILE          TOTALS TOR FILE                              
         BAS   RE,COUNTR                                                        
         LA    RF,UCTFILE          TOTALS FOR USER                              
         BAS   RE,COUNTR                                                        
*                                                                               
         TM    FWLSTAT,X'80'        TEST FOR ACVTIVE                            
         BZ    COUNTX                                                           
         CLC   FWLAGERD,TODAYC      TEST FOR WELL EXPIRED                       
         BL    COUNTX                                                           
         BH    COUNT050                                                         
         CLC   FWLAGERT,TIMEI       TEST FOR JUST EXPIRED                       
         BL    COUNTX                                                           
*                                                                               
COUNT050 LA    RF,TCAFILE          TOTAL ACTIVES FOR FILE                       
         BAS   RE,COUNTR                                                        
         LA    RF,UCAFILE          TOTAL ACTIVES FOR USER                       
         BAS   RE,COUNTR                                                        
*                                                                               
COUNTX   B     EXITEQ                                                           
*                                                                               
COUNTR   L     R1,FILEQ(RF)        FILE COUNT                                   
         LA    R1,1(R1)                                                         
         ST    R1,FILEQ(RF)                                                     
*                                                                               
         L     R1,BYTEQ(RF)        BYTE COUNT                                   
         A     R1,FWLFSIZE                                                      
         ST    R1,BYTEQ(RF)                                                     
*                                                                               
         L     R1,CI1Q(RF)         1 CI COUNT                                   
         LA    R1,1(R1)                                                         
         ST    R1,CI1Q(RF)                                                      
*                                                                               
         LR    R1,R0               2 CI COUNT                                   
         A     R1,CI2Q(RF)                                                      
         ST    R1,CI2Q(RF)                                                      
         BR    RE                                                               
*                                                                               
CNT100   SR    R1,R1               SET R0 TO NUMBER OF PART 2S                  
         SR    R0,R0                                                            
         IC    R1,ZWLNCI                                                        
         IC    R0,ZWLNCIX                                                       
         BCTR  R1,0                                                             
         AR    R0,R1                                                            
*                                                                               
         MVC   CUSERN,GUSERN       SAVE NAME                                    
         MVC   CUSERF,ZWLINDEX-1   SAVE NAFILE NUMBER                           
*                                                                               
         LA    RF,TCTFILE          TOTALS TOR FILE                              
         BAS   RE,CNTR                                                          
         LA    RF,UCTFILE          TOTALS FOR USER                              
         BAS   RE,CNTR                                                          
*                                                                               
         TM    ZWLSTAT,X'80'       TEST FOR ACVTIVE                             
         BZ    CNTX                                                             
         CLC   ZWLAGERD,TODAYC     TEST FOR WELL EXPIRED                        
         BL    CNTX                                                             
         BH    CNT150                                                           
         CLC   ZWLAGERT,TIMEI      TEST FOR JUST EXPIRED                        
         BL    CNTX                                                             
*                                                                               
CNT150   LA    RF,TCAFILE          TOTAL ACTIVES FOR FILE                       
         BAS   RE,CNTR                                                          
         LA    RF,UCAFILE          TOTAL ACTIVES FOR USER                       
         BAS   RE,CNTR                                                          
*                                                                               
CNTX     B     EXITEQ                                                           
*                                                                               
CNTR     L     R1,FILEQ(RF)        FILE COUNT                                   
         LA    R1,1(R1)                                                         
         ST    R1,FILEQ(RF)                                                     
*                                                                               
         L     R1,BYTEQ(RF)        BYTE COUNT                                   
         A     R1,ZWLFSIZE                                                      
         ST    R1,BYTEQ(RF)                                                     
*                                                                               
         L     R1,CI1Q(RF)         1 CI COUNT                                   
         LA    R1,1(R1)                                                         
         ST    R1,CI1Q(RF)                                                      
*                                                                               
         LR    R1,R0               2 CI COUNT                                   
         A     R1,CI2Q(RF)                                                      
         ST    R1,CI2Q(RF)                                                      
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        BUILD LINE OF COUNTER SUMMARY                                          
***********************************************************************         
PCOUNTS  NTR1                                                                   
*                                                                               
         MVC   MYWORK,SPACES                                                    
         MVC   MYWORK+00(6),=C'FILES='                                          
         MVC   FULL,FILEQ(R2)                                                   
         BAS   RE,EDITF                                                         
         MVC   MYWORK+06(5),DUB1                                                
         MVC   MYWORK+12(6),=C'BYTES='                                          
         MVC   FULL,BYTEQ(R2)                                                   
         BAS   RE,EDITF                                                         
         MVC   MYWORK+18(5),DUB1                                                
         MVC   MYWORK+24(6),=C'PART2='                                          
         MVC   FULL,CI2Q(R2)                                                    
         BAS   RE,EDITF                                                         
         MVC   MYWORK+30(5),DUB1                                                
*                                                                               
         MVC   MYWORK+36(6),=C'SPACE='                                          
         SR    R0,R0                                                            
         L     R1,CI1Q(R2)                                                      
         M     R0,CI1SIZE                                                       
         ST    R1,FULL                                                          
         L     R1,CI2Q(R2)                                                      
         M     R0,CI2SIZE                                                       
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         BAS   RE,EDITF                                                         
         MVC   MYWORK+42(5),DUB1                                                
*                                                                               
PCOUNTX  B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET USERID FROM 2 CHR NUMBER                                           
***********************************************************************         
GETUSR   NTR1                                                                   
         XC    KEY,KEY             FIND ID REC                                  
         MVI   KEY,CTIKTYPQ                                                     
         MVC   KEY+CTIKNUM-CTIREC(2),GUSER                                      
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,KEY,ACTIO                         
         CLI   8(R1),0                                                          
         BNE   GETUS050                                                         
         L     R1,ACTIO                                                         
         LA    R1,CTIDATA-CTIREC(R1)                                            
GETUS010 CLI   0(R1),X'02'         LOOK FOR ID NAME ELEMENT                     
         BE    GETUS020                                                         
         SR    RF,RF                                                            
         ICM   RF,1,1(R1)          NEXT                                         
         BZ    GETUS050                                                         
         AR    R1,RF                                                            
         B     GETUS010                                                         
GETUS020 MVC   GUSERN,SPACES       COPY NAME TO GUSERN                          
         IC    RF,1(R1)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GUSERN(0),2(R1)                                                  
         B     EXITEQU                                                          
*                                                                               
GETUS050 MVC   GUSERN,SPACES       NO RECORD JUST EDIT OUT                      
         EDIT  (B2,GUSER),(6,GUSERN),ALIGN=LEFT                                 
         B     EXITEQU                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
*        BUILD SPPSDDC IN WORK FROM W_KEY IN DUB                                
***********************************************************************         
GETKEY   ST    RE,SAVERE                                                        
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(4),DUB+2       DISPLAY SYSPRG & SUBPRG                      
         LA    RF,WORK+4                                                        
*                                                                               
         CLI   DUB+6,X'5C'         TEST FOR "*"                                 
         BE    GETK020                                                          
         CLI   DUB+6,X'C1'         TEST FOR A-9                                 
         BNL   GETK020                                                          
         IC    R1,DUB+6            MUST BE PACKED DAY                           
         MVC   1(1,RF),DUB+6                                                    
         OC    1(1,RF),=X'F0'      UNPACK AND DISPLAY                           
         SRL   R1,4                                                             
         STC   R1,0(RF)                                                         
         OC    0(1,RF),=X'F0'                                                   
         LA    RF,2(RF)            BUMP 2 CHRS                                  
         B     GETK026                                                          
*                                                                               
GETK020  MVC   0(1,RF),DUB+6       SINGLE CHR DAY                               
GETK025  LA    RF,1(RF)                                                         
*                                                                               
GETK026  MVC   0(1,RF),DUB+7       DISPLAY CLASS                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        STATUS ROUTINE BYTE=STAT DUB=RESULT                                    
***********************************************************************         
STATOUT  NTR1                                                                   
         MVC   DUB,SPACES          CLEAR OUTPUT AREA                            
         LA    RF,STATTBL                                                       
         SR    R0,R0                                                            
*                                                                               
         LA    R1,X'80'            START FROM X'80'                             
STAT010  EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              TEST STATUS SET                              
         BZ    STAT020                                                          
*                                                                               
         CH    R0,=H'0'                                                         
         BNE   STAT011                                                          
         MVC   DUB(4),0(RF)        FIRST GOES INTO DUB                          
         B     STAT019                                                          
*                                                                               
STAT011  CH    R0,=H'1'                                                         
         BNE   STAT012                                                          
         MVI   DUB+4,C','          SECOND IS STAT,STA                           
         MVC   DUB+5(3),0(RF)                                                   
         B     STAT019                                                          
*                                                                               
STAT012  CH    R0,=H'2'                                                         
         BNE   STAT020                                                          
         MVC   DUB+2(3),DUB+4                                                   
         MVI   DUB+5,C','          THIRD IS ST,ST,ST                            
         MVC   DUB+6(2),0(RF)                                                   
         B     STAT019                                                          
*                                                                               
STAT019  AH    R0,=H'1'                                                         
*                                                                               
STAT020  SRL   R1,1                NEXT STATUS ENTRY                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   STAT010             LOOP BACK                                    
*                                                                               
STATOUTX B     EXITEQ                                                           
*                                                                               
STATTBL  DC    C'ACTV'                                                          
         DC    C'HOLD'                                                          
         DC    C'PROC'                                                          
         DC    C'SENT'                                                          
         DC    C'KEEP'                                                          
         DC    C'DELD'                                                          
         DC    C'SNDG'                                                          
         DC    C'CRTG'                                                          
         DC    C'XXXX'                                                          
         DC    C'RUNG'                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
*        EDIT ROUTINE                                                           
***********************************************************************         
EDITF    ST    RE,SAVERE           EDIT FROM FULL                               
         L     RE,FULL                                                          
         B     EDITRE                                                           
EDITH    ST    RE,SAVERE           EDIT FROM HALF                               
         LH    RE,HALF                                                          
         B     EDITRE                                                           
EDITB    ST    RE,SAVERE           EDIT FROM BYTE                               
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
*                                                                               
EDITRE   MVI   EDITCHR,C' '        SET CHR TO BLANK                             
         C     RE,=F'9216'                                                      
         BL    EDITRF              IF < 9K JUST EDIT                            
         C     RE,=F'1048576'                                                   
         BNL   EDITRM              IF > 1M EDIT MEG                             
*                                                                               
         MVI   EDITCHR,C'k'        EDIT k                                       
         SR    RF,RF                                                            
         SRDL  RE,10               DIVIDE BY 1K                                 
         SRL   RF,22                                                            
         B     EDITRF                                                           
*                                                                               
EDITRM   MVI   EDITCHR,C'M'        EDIT M                                       
         SR    RF,RF                                                            
         SRDL  RE,20               DIVIDE BY 1M                                 
         SRL   RF,12                                                            
*                                                                               
EDITRF   EDIT  (RE),(8,DUB),DUB=EDUB,ZERO=NOBLANK                               
*                                                                               
         CLI   EDITCHR,C' '        NORMAL EDIT EXITS HERE                       
         BE    EDITX                                                            
*                                                                               
         MVC   DUB(5),DUB+3        SHIFT NUMBER ALONG 3                         
*                                                                               
         MH    RF,=H'10'                                                        
         SRL   RF,10                                                            
         CLI   EDITCHR,C'M'                                                     
         BNE   *+8                                                              
         SRL   RF,10                                                            
         EDIT  (RF),(1,DUB+6),DUB=EDUB,ZERO=NOBLANK                             
         MVI   DUB+5,C'.'                                                       
         MVC   DUB+7,EDITCHR       INSERT DEC AND EDIT CHR                      
         CLI   DUB+2,C' '                                                       
         BE    EDITX               EXIT NOW IF <= 5 CHRS                        
         MVC   DUB1(5),DUB                                                      
         MVC   DUB(7),SPACES                                                    
         MVC   DUB+2(5),DUB1       ELSE DROP THE DEC PLACE                      
*                                                                               
EDITX    LM    RE,RF,DUB           PUT ALIGN=LEFT INTO DUB1                     
         STM   RE,RF,DUB1                                                       
EDIT1    LM    RE,RF,DUB1                                                       
         CLI   DUB1,C' '                                                        
         BNE   EDITXX                                                           
         SLDL  RE,8                                                             
         STM   RE,RF,DUB1                                                       
         MVI   DUB1+7,C' '                                                      
         B     EDIT1                                                            
*                                                                               
EDITXX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        TIME OUTPUT ROUTINE                                                    
***********************************************************************         
TIMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
         MVC   DUB+2(6),DOTS                                                    
         CLI   DUB,23                                                           
         BH    TIMEOUTX                                                         
         CLI   DUB+1,59                                                         
         BH    TIMEOUTX                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+2(2),DUB1+6(2)                                               
*                                                                               
         MVI   DUB+4,C':'                                                       
         SR    R0,R0                                                            
         IC    R0,DUB+1                                                         
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+5(2),DUB1+6(2)                                               
*                                                                               
TIMEOUTX BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET TIME NOW                                                           
***********************************************************************         
GETTIME  ST    RE,SAVERE                                                        
         TIME  TU                  R0=TIME IN 1/38400 SECS                      
         SRDL  R0,32                                                            
         D     R0,=F'38400'        R1=TIME IN SECONDS                           
         LR    RF,R1                                                            
         MH    R1,=H'3'                                                         
         SRL   R1,2                R1=(SECS*3)/4                                
         STH   R1,TIMEC            TIMEC=TIME IN SPECIAL UNITS                  
         LR    R1,RF                                                            
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY MINUTES                            
         SR    RE,RE                                                            
         LR    RF,R1                                                            
         D     RE,=F'10'                                                        
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,TIMEI            TIMEI=SINGLE BYTE 10MIN INCREMENT            
         SR    R0,R0                                                            
         D     R0,=F'60'           R0=MINS,R1=HOURS                             
         STC   R1,TIMEB                                                         
         STC   R0,TIMEB+1          TIMEB=B'HHHHHHHHMMMMMMMM'                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT TITLE                                                            
***********************************************************************         
TITLE1   DC    CL166' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'--------------------- PARAMETER CARDS ---------------'         
         DC    C'-----------------------------------------------------'         
         DC    C'-----------------------------------------------------'         
         DC    C'-------'                                                       
         ORG                                                                    
                                                                                
***********************************************************************         
*        REPORT LINES                                                           
***********************************************************************         
T1       DC    C'1'                                                             
         DC    AL1(TL),10AL1(HB)                                                
         DC    AL1(TM),02AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),16AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),08AL1(HB)                                                
         DC    AL1(TM),08AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),08AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TR)                                                          
         DC    9AL1(BB)                                                         
*                                                                               
T2       DC    C' '                                                             
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB),02AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),16AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),08AL1(BB)                                                
         DC    AL1(VB),08AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),08AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB)                                                          
         DC    9AL1(BB)                                                         
*                                                                               
TITLE2   DC    C' '                                                             
         DC    AL1(VB),CL10'Userid'                                             
         DC    AL1(VB),CL02'Wk'                                                 
         DC    AL1(VB),CL07'File'                                               
         DC    AL1(VB),CL05'Ref#'                                               
         DC    AL1(VB),CL16'Description'                                        
         DC    AL1(VB),CL03'Typ'                                                
         DC    AL1(VB),CL08'Attrib'                                             
         DC    AL1(VB),CL08'Status'                                             
         DC    AL1(VB),CL03'Siz'                                                
         DC    AL1(VB),CL13'Cretated on'                                        
         DC    AL1(VB),CL13'Sent on'                                            
         DC    AL1(VB),CL08'Sent to'                                            
         DC    AL1(VB),CL13'Retain until'                                       
         DC    AL1(VB),CL05'Recs'                                               
         DC    AL1(VB),CL05'Bytes'                                              
         DC    AL1(VB),CL05'Avg'                                                
         DC    AL1(VB),CL05'Max'                                                
         DC    AL1(VB),CL03'Cis'                                                
         DC    AL1(VB),CL03'Xci'                                                
         DC    AL1(VB)                                                          
         DC    9AL1(BB)                                                         
*                                                                               
T3       DC    C' '                                                             
         DC    AL1(ML),10AL1(HB)                                                
         DC    AL1(MM),02AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),16AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),08AL1(HB)                                                
         DC    AL1(MM),08AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),08AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MR)                                                          
         DC    9AL1(BB)                                                         
*                                                                               
T4       DC    C' '                                                             
         DC    AL1(BL),10AL1(HB)                                                
         DC    AL1(BM),02AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),16AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),08AL1(HB)                                                
         DC    AL1(BM),08AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),08AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BR)                                                          
         DC    9AL1(BB)                                                         
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        BOX EQUATES                                                            
***********************************************************************         
TL       EQU   X'AC'               TOP LEFT                                     
TM       EQU   X'CC'               TOP MIDDLE                                   
TR       EQU   X'BC'               TOP RIGHT                                    
HB       EQU   X'BF'               HORIZONTAL BAR                               
VB       EQU   X'FA'               VERTICAL  BAR                                
ML       EQU   X'EB'               MIDDLE LEFT                                  
MM       EQU   X'8F'               MIDDLE MIDDLE                                
MR       EQU   X'EC'               MIDDLE RIGHT                                 
BL       EQU   X'AB'               BOTTOM LEFT                                  
BM       EQU   X'CB'               BOTTOM MIDDLE                                
BR       EQU   X'BB'               BOTTOM RIGHT                                 
BB       EQU   X'40'               BLANK LINE                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PRINT ROUTINES                                                         
***********************************************************************         
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,TITLE2                                                  
         PUT   SYSPRINT,T3                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*                                                                               
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4,4),X'0000',AL3(INPUT)                           
         DC    C'OUTPUT ',AL1(5,4),X'0000',AL3(OUTPUT)                          
         DC    C'REPORT ',AL1(5,1),X'0000',AL3(REPFLG)                          
         DC    C'REPLACE',AL1(6,1),X'0000',AL3(REPLACE)                         
         DC    C'RESET  ',AL1(4,1),X'0000',AL3(RESET)                           
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
INPUT    DC    C'WRKZ'             INPUT=WRKF/WRKZ TAPE                         
OUTPUT   DC    C'WRKZ'             OUTPUT=WRKF/WRKZ TAPE                        
REPFLG   DC    C'Y'                REPORT=YES                                   
REPLACE  DC    C'N'                REPLACE=NO                                   
RESET    DC    C'N'                RESET=NO                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE INPUT CARDS                                                          
***********************************************************************         
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRUSR  LA    R1,=C'INVALID USERD   '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                      
***********************************************************************         
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT 16BIT WRKF TO 20BIT WRKZ                                              
***********************************************************************         
FTOZ     NTR1                                                                   
         LA    R7,BIGWORK                                                       
         MVC   BIGWORK(FWLSOFEND-FWLHDRD),IOAREA                                
         XC    ZWLHDRD(ZWLSOFEND-ZWLHDRD),ZWLHDRD                               
*                                                                               
*        WLHEAD - 24 BYTE HEADER (WRKF/WRKZ)                                    
*                                                                               
         MVC   ZWLSOFLAB,FWLSOFLAB                                              
         MVC   ZWLFLAG,FWLFLAG                                                  
         MVC   ZWLREPRNO+2(2),FWLREPRNO                                         
         MVC   ZWLREPFNO,FWLREPFNO                                              
*                                                                               
*        WLINDEX- 24 BYTES (WRKF) - 40 BYTES (WRKFZ)                            
*                                                                               
         MVC   ZWLKEY,FWLKEY                                                    
         MVC   ZWLFILENO+2(2),FWLFILENO                                         
         MVC   ZWLTYPE,FWLTYPE                                                  
         MVC   ZWLATTB,FWLATTB                                                  
         MVC   ZWLSTAT,FWLSTAT                                                  
         MVC   ZWLSEQ,FWLSEQ                                                    
         MVC   ZWLAGES,FWLAGES                                                  
         MVC   ZWLAGELD,FWLAGELD                                                
         MVC   ZWLUDATA,FWLUDATA                                                
         MVC   ZWLAGERD,FWLAGERD                                                
         MVC   ZWLAGERT,FWLAGERT                                                
         MVC   ZWLAGELT,FWLAGELT                                                
*                                                                               
*        WLBATTR - BLOCK ATTRIBUTES - 12 BYTES (WRKF) 16 BYTES (WRKZ)           
*                                                                               
         MVC   ZWLRECS,FWLRECS                                                  
*                                                                               
*        WLDATA - FILE ATTRIBUTES/DATA- 88 BYTES (WRKF/WRKZ)                    
*                                                                               
         MVC   ZWLDATA,FWLDATA                                                  
*                                                                               
*        RECORD LENGTH                                                          
*                                                                               
         MVC   IOAREAF(2),=AL2(ZWLSOFEND-ZWLHDRD)                               
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONVERT 20BIT WRKZ TO 16BIT WRKF                                              
***********************************************************************         
ZTOF     NTR1                                                                   
         LA    R8,BIGWORK                                                       
*                                                                               
         MVC   BIGWORK(ZWLSOFEND-ZWLHDRD),IOAREA                                
         XC    FWLHDRD(FWLSOFEND-FWLHDRD),FWLHDRD                               
*                                                                               
*        WLHEAD - 24 BYTE HEADER (WRKZ/WRKF)                                    
*                                                                               
         MVC   FWLSOFLAB,ZWLSOFLAB                                              
         MVC   FWLFLAG,ZWLFLAG                                                  
         MVC   FWLREPRNO,ZWLREPRNO+2                                            
         MVC   FWLREPFNO,ZWLREPFNO                                              
*                                                                               
*        WLINDEX- 24 BYTES (WRKF) - 40 BYTES (WRKZ)                             
*                                                                               
         MVC   FWLKEY,ZWLKEY                                                    
         MVC   FWLFILENO,ZWLFILENO+2                                            
         MVC   FWLTYPE,ZWLTYPE                                                  
         MVC   FWLATTB,ZWLATTB                                                  
         MVC   FWLSTAT,ZWLSTAT                                                  
         MVC   FWLSEQ,ZWLSEQ                                                    
         MVC   FWLAGES,ZWLAGES                                                  
         MVC   FWLAGELD,ZWLAGELD                                                
         MVC   FWLUDATA,ZWLUDATA                                                
         MVC   FWLAGERD,ZWLAGERD                                                
         MVC   FWLAGERT,ZWLAGERT                                                
         MVC   FWLAGELT,ZWLAGELT                                                
*                                                                               
*        WLBATTR - BLOCK ATTRIBUTES - 12 BYTES (WRKF) 16 BYTES (WRKZ)           
*                                                                               
         MVC   FWLRECS,ZWLRECS                                                  
*                                                                               
*        WLDATA - FILE ATTRIBUTES/DATA- 88 BYTES (WRKZ/WRKF)                    
*                                                                               
         MVC   FWLDATA,ZWLDATA                                                  
*                                                                               
*        RECORD LENGTH                                                          
*                                                                               
         MVC   IOAREAF(2),=AL2(FWLSOFEND-FWLHDRD)                               
*                                                                               
         J     EXIT                                                             
         DROP  R7,R8                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS & LTORG                                                      
***********************************************************************         
DMREAD   DC    CL8'DMREAD '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
BUFFER   DC    CL8'BUFFER'                                                      
WRKZIL   DC    CL8'WRKZIL '                                                     
WRKFIL   DC    CL8'WRKFIL '                                                     
SEQ      DC    CL8'SEQ '                                                        
SPACES   DC    CL166' '                                                         
DOTS     DC    CL16'................'                                           
MAXLINE  DC    P'60'                                                            
*                                                                               
SRTCARD  DC    C'SORT FIELDS=(1,10,BI,A) '                                      
RECCARD  DC    C'RECORD TYPE=F,LENGTH=126 '                                     
SRTCARZ  DC    C'SORT FIELDS=(1,16,BI,A) '                                      
RECCARZ  DC    C'RECORD TYPE=F,LENGTH=168 '                                     
*                                                                               
RCMSGH   DC    AL2(RCMSGL)                                                      
RCMSGM   DC    C' -PROGRESS- '                                                  
         DC    C'WRKZ'                                                          
RCMSGF   DC    C' '                                                             
         DC    C' - file read count = '                                         
RCMSGC   DC    C'XXXXXXXXXX'                                                    
RCMSGL   EQU   *-RCMSGM                                                         
*                                                                               
WRKFRECL EQU   13680                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        DCBS & ADCONS                                                          
***********************************************************************         
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=TAPEEND,        X        
               RECFM=VB,BLKSIZE=0,LRECL=0,BUFNO=2                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,BLKSIZE=0,LRECL=WRKFRECL,BUFNO=2                        
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
UTL      DC    F'0',X'01',XL3'00',XL252'00'                                     
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',1024X'00'                          
*                                                                               
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'WKWKWKWK'                                                    
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
AWKBUFF  DS    A                                                                
ACTIO    DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EDITCHR  DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
CARDEND  DS    A                                                                
WRKFILE  DS    CL8                                                              
WRKZILE  DS    CL8                                                              
ERRINF   DS    CL10                                                             
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
BIGWORK  DS    XL255                                                            
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
TODAY    DS    CL3                 YYMMDD PWOS                                  
TIMEI    DS    CL1                 BINARY 10 MINUTES                            
TODAYC   DS    H                   TODAY COMP                                   
TIMEB    DS    H                   BINARY HHMM                                  
TIMEC    DS    H                   BINARY (SECS*3)/4                            
*                                                                               
RCWRKF   DS    C                   PROGRESS MESSAGE WRKZ Number                 
RCWRKFC  DS    F                   PROGRESS MESSAGE WRKZ COUNT                  
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL40                                                             
FWKKEY   DS    CL40                                                             
ZWKKEY   DS    CL40                                                             
*                                                                               
FCSOFNDX DS    CL(L'FWLINDEX)     SOF INDEX FOR CURRENT FILE                    
ZCSOFNDX DS    CL(L'ZWLINDEX)     SOF INDEX FOR CURRENT FILE                    
*                                                                               
FERI     DS    C                   FILE ERROR INDICTOR                          
FERL     EQU   C'L'                . RECORD LENGTH ERROR                        
*                                                                               
         DS    0F                                                               
*PREFIX=Z                                                                       
       ++INCLUDE DMWRKZW                                                        
*PREFIX=                                                                        
         DS    0F                                                               
*PREFIX=F                                                                       
       ++INCLUDE DMWRKFW                                                        
*PREFIX=                                                                        
*                                                                               
CI1SIZE  DS    F                                                                
CI2SIZE  DS    F                                                                
CITSIZE  DS    F                                                                
*                                                                               
GUSER    DS    XL2                                                              
GUSERN   DS    CL10                                                             
*                                                                               
CUSERN   DS    CL10                NAME OF USER FOR COUNT                       
CUSERF   DS    CL1                 FILE NUMBER OF USER                          
*                                                                               
FILEQ    EQU   0                                                                
BYTEQ    EQU   4                                                                
CI1Q     EQU   8                                                                
CI2Q     EQU   12                                                               
         DS    0F                                                               
UCOUNTS  DS    0XL32                                                            
UCAFILE  DS    F                   USER COUNT ACTIVE FILES                      
UCABYTE  DS    F                   USER COUNT ACTIVE BYTES                      
UCA1CI   DS    F                   USER COUNT ACTIVE PT1S                       
UCA2CI   DS    F                   USER COUNT ACTIVE PT2S                       
*                                                                               
UCTFILE  DS    F                   USER COUNT TOTAL FILES                       
UCTBYTE  DS    F                   USER COUNT TOTAL BYTES                       
UCT1CI   DS    F                   USER COUNT TOTAL PT1S                        
UCT2CI   DS    F                   USER COUNT TOTAL PT2S                        
*                                                                               
TCOUNTS  DS    0XL32                                                            
TCAFILE  DS    F                   TOTAL COUNT ACTIVE FILES                     
TCABYTE  DS    F                   TOTAL COUNT ACTIVE BYTES                     
TCA1CI   DS    F                   TOTAL COUNT ACTIVE PT1S                      
TCA2CI   DS    F                   TOTAL COUNT ACTIVE PT2S                      
*                                                                               
TCTFILE  DS    F                   TOTAL COUNT TOTAL FILES                      
TCTBYTE  DS    F                   TOTAL COUNT TOTAL BYTES                      
TCT1CI   DS    F                   TOTAL COUNT TOTAL PT1S                       
TCT2CI   DS    F                   TOTAL COUNT TOTAL PT2S                       
*                                                                               
IOAREAF  DS    XL4                                                              
IOAREA   DS    CL(WRKFRECL)                                                     
*                                                                               
         DS    CL8                                                              
CTIO     DS    2048C                                                            
*                                                                               
WKBUFF   DS    14336C                                                           
*                                                                               
SPARE    DS    1024C                                                            
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
PLINED   DSECT                                                                  
         DS    CL1                                                              
PBOX1    DS    CL1                                                              
PLUSER   DS    CL10                                                             
         DS    CL1                                                              
PLWRKF   DS    CL2                                                              
         DS    CL1                                                              
PLKEY    DS    CL7                                                              
         DS    CL1                                                              
PLREFNO  DS    CL5                                                              
         DS    CL1                                                              
PLDESC   DS    CL16                                                             
         DS    CL1                                                              
PLTYPE   DS    CL3                                                              
         DS    CL1                                                              
PLATTR   DS    CL8                                                              
         DS    CL1                                                              
PLSTAT   DS    CL8                                                              
         DS    CL1                                                              
PLSIZE   DS    CL3                                                              
         DS    CL1                                                              
PLLIVED  DS    CL7                                                              
         DS    CL1                                                              
PLLIVET  DS    CL5                                                              
         DS    CL1                                                              
PLDEADD  DS    CL7                                                              
         DS    CL1                                                              
PLDEADT  DS    CL5                                                              
         DS    CL1                                                              
PLSENTO  DS    CL8                                                              
         DS    CL1                                                              
PLRETAD  DS    CL7                                                              
         DS    CL1                                                              
PLRETAT  DS    CL5                                                              
         DS    CL1                                                              
PLRECS   DS    CL5                                                              
         DS    CL1                                                              
PLBYTES  DS    CL5                                                              
         DS    CL1                                                              
PLAVG    DS    CL5                                                              
         DS    CL1                                                              
PLMAX    DS    CL5                                                              
         DS    CL1                                                              
PLCIS    DS    CL3                                                              
         DS    CL1                                                              
PLCISX   DS    CL3                                                              
PBOXX    DS    CL1                                                              
         EJECT                                                                  
         DCBD    DSORG=QS,DEVD=DA                                               
*                                                                               
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* FASSBOFF                                                                      
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
*                                                                               
*PREFIX=F                                                                       
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE DMWRKFK                                                        
*PREFIX=                                                                        
*                                                                               
*PREFIX=Z                                                                       
       ++INCLUDE DMWRKZL                                                        
       ++INCLUDE DMWRKZD                                                        
       ++INCLUDE DMWRKZK                                                        
*PREFIX=                                                                        
*                                                                               
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DMWRKZC   03/22/11'                                      
         END                                                                    
