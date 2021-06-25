*          DATA SET DMWRKZZ    AT LEVEL 005 AS OF 03/11/16                      
*PHASE WRKZZA                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE SORTER                                                                 
         TITLE '$WRKZZ - TRANSFER WRKZ FILES TO DISK/TAPE'                      
         PRINT NOGEN                                                            
WRKZZ    CSECT                                                                  
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
         MVC   WRKFILE,WRKZIL                                                   
*                                                                               
         LA    R8,IOAREA           R8=IOAREA                                    
         USING WLHDRD,R8                                                        
         USING UKRECD,WKKEY                                                     
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
                                                                                
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
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
INIT020  MVI   RCWRKZ,C' '         Clear progress message flags                 
         XC    RCWRKZC,RCWRKZC                                                  
         CLI   MODE,C'M'           MODE=MOVE                                    
         BNE   *+8                                                              
         MVI   RCWRKZ,C'Z'         JUST TOTAL COUNT FOR MOVE                    
*                                                                               
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* OPEN FILES                                                                    
***********************************************************************         
OPENALL  NTR1                                                                   
*                                                                               
OPEN010  CLI   INPUT,C'T'          TEST TAPE INPUT                              
         BNE   OPEN020                                                          
         OPEN  (TAPEIN,INPUT)      OPEN INPUT                                   
*                                                                               
OPEN020  CLI   OUTPUT,C'T'         TEST TAPE OUTPUT                             
         BNE   OPEN030                                                          
         OPEN  (TAPEOUT,OUTPUT)    OPEN OUTPUT                                  
*                                                                               
OPEN030  CLI   REPFLG,C'N'         TEST FOR REPORT=NO                           
         BE    OPEN040                                                          
         GOTO1 =V(SORTER),DMCB,SRTCARD,RECCARD                                  
*                                                                               
OPEN040  B     EXITEQ                                                           
                                                                                
***********************************************************************         
* READ DATA LINES AND WRITE TO WRKZ                                             
***********************************************************************         
MAIN     NTR1                                                                   
         ST    RD,MAINRD           SAVE THIS RD FOR TAPEEND EXIT                
*                                                                               
         CLI   OUTPUT,C'T'         allow dumps                                  
         BE    MAIN02                                                           
*                                                                               
MAIN02   SAC   0                   WKBUFF HAS BEEN STOPPED                      
         LAM   AR0,ARF,=16F'0'                                                  
*                                                                               
         BAS   RE,GETLIST          GET LIST OF WRKZ FLES                        
*                                                                               
         BAS   RE,INITWK           GET FILE CI DETAIL                           
*                                                                               
MAIN010  MVI   FERI,0              INIT FILE ERROR INDICATOR                    
*                                                                               
         BAS   RE,GETFILE          GET A WRKZ FILE                              
         BE    MAIN015                                                          
*                                                                               
         BAS   RE,GETMORE          IF EOF LOOK FOR MORE FILES                   
         BE    MAIN010                                                          
         BNE   TAPEEND             EXIT WHEN NO MORE                            
*                                                                               
MAIN015  BAS   RE,CHECKPR          Track and report on progress                 
*                                                                               
         BAS   RE,FILTER           FILTER OUT UNWANTED FILES                    
         BNE   MAIN010                                                          
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
MAIN110  CLI   OUTPUT,C'T'         ON ERROR PURGE FILE ON WRKZ DISK             
         BE    MAIN020                                                          
         GOTO1 =V(DATAMGR),DMCB,=C'CLO/PUR',WRKFILE,WKKEY,(RF),AWKBUFF          
         B     MAIN010                                                          
*                                                                               
TAPEEND  EQU   *                                                                
         L     RD,MAINRD           RESTORE RD                                   
*                                                                               
         MVI   WRKFILE+4,C' '      No more WRKZs                                
         BAS   RE,CHECKPR          Track and report on progress                 
*                                                                               
         BAS   RE,REPORT           WRITE A REPORT                               
*                                                                               
MAINX    B     EXITEQ                                                           
                                                                                
***********************************************************************         
* GET NEXT FILE FROM INPUT SOURCE                                               
***********************************************************************         
GETFILE  NTR1                                                                   
         CLI   INPUT,C'T'                                                       
         BNE   GETF500                                                          
*                                                                               
* TAPE INPUT                                                                    
*                                                                               
GETF010  GET   TAPEIN,IOAREAF      GET UNTIL SOF                                
         CLC   WLSOFLAB,=C'*SOFSOF*'                                            
         BNE   GETF010                                                          
         MVC   CSOFNDX,WLINDEX                                                  
*                                                                               
         MVI   WLFLAG,0            ENSURE FLAGS ARE ZERO                        
         MVC   WKKEY,WLUSRID       SET USERID FROM KEY                          
         CLI   WLREPFNO,0                                                       
         BE    *+10                                                             
         MVC   WRKFILE+4(1),WLREPFNO                                            
*                                                                               
         OC    WLRECS,WLRECS       IGNORE FILES WITH NO RECORDS                 
         BZ    GETF010                                                          
         TM    WLSTAT,W_STRUN      IGNORE FILE WITH A TEMP STATUS               
         BNZ   GETF010                                                          
*                                                                               
         CLI   MODE,C'M'           MODE=MOVE                                    
         BNE   WRWK019                                                          
         MVC   ORGWRKZ,WLREPFNO                                                 
         GOTO1 =V(DATAMGR),DMCB,GFILE,WRKZIL,WKKEY,IOAREA,AWKBUFF               
         CLC   ORGWRKZ,UKUSRINF+4                                               
         BE    WRWK019                                                          
         MVC   WLREPFNO,UKUSRINF+4                                              
         MVC   WRKFILE+4(1),WLREPFNO                                            
*                                                                               
WRWK019  LA    R1,WRKZ             TEST FOR FILE IN SELECTION LIST              
GETF020  CLI   WLREPFNO,0                                                       
         BE    GETF900                                                          
         CLC   0(1,R1),WLREPFNO                                                 
         BE    GETF900                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '          NOT IN LIST SO GET NEXT FILE                 
         BE    GETF010                                                          
         BNE   GETF020                                                          
*                                                                               
* FILE INPUT                                                                    
*                                                                               
GETF500  BAS   RE,READWK                                                        
         BNE   EXITNE              EXIT NEQ IF EOF                              
         CLC   WLSOFLAB,=C'*SOFSOF*'                                            
         BNE   GETF500                                                          
         MVC   CSOFNDX,WLINDEX                                                  
*                                                                               
         MVI   WLFLAG,0            ENSURE FLAGS ARE ZERO                        
         MVC   IOAREAF,=X'00980000'                                             
         B     GETF900                                                          
*                                                                               
GETF900  CLI   REPLACE,C'Y'        PUT BACK WITH SAME REF#                      
         BNE   *+8                                                              
         OI    WLFLAG,WLFLREFN                                                  
*                                                                               
         OC    ADJ#+1(4),ADJ#+1    IS THERE AN ADJUST NUMBER?                   
         BZ    GETF910                                                          
         ICM   R1,15,ADJ#+1        IF SO, ADD IT TO THE FILE REFERENCE          
         ICM   RF,15,WLFILENO                                                   
         AR    R1,RF                                                            
         STCM  R1,15,WLFILENO                                                   
*                                                                               
GETF910  CLI   RESET,C'Y'          RESET RETAIN TIME                            
         BNE   *+12                                                             
         OI    WLFLAG,WLFLRSET     SET RESET FLAG                               
         B     *+8                                                              
         OI    WLFLAG,WLFLRCOP     OR COPY FLAG                                 
*                                                                               
GETFILX  B     EXITEQ                                                           
                                                                                
***********************************************************************         
* GET LIST OF WORKER FILES                                                      
***********************************************************************         
GETLIST  NTR1                                                                   
         CLI   INPUT,C'T'          TAPE HAS ONLY ONE FILE                       
         BNE   *+12                                                             
         CLI   OUTPUT,C'T'                                                      
         BE    GETLISX                                                          
*                                                                               
         L     R5,AWKBUFF          GET LIST OF DISK FILES                       
         LA    RF,IOAREA                                                        
         GOTO1 =V(DATAMGR),DMCB,(0,GLIST),WRKFILE,WKKEY,(RF),(R5)               
         ICM   RE,15,UKUSRINF                                                   
         BZ    GETLISD                                                          
*                                                                               
         SR    R1,R1               R1=NUM OF FILES IN LIST                      
         ICM   R1,1,0(RE)                                                       
         BZ    GETLISD             CHECK HI LO BOUNDS                           
         CH    R1,=H'16'                                                        
         BH    GETLISD                                                          
*                                                                               
         LA    R1,2(R1)            ADD TWO FOR HDR AND TRL                      
         SLL   R1,3                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRKZLST(0),0(RE)    COPY WRKZ LIST TO MY OWN AREA                
         XC    WRKZLSTX,WRKZLSTX   SET END OF MAXIMUM LIST                      
*                                                                               
         MVC   WRKFILE,=C'WRKZ    '                                             
         MVC   WRKFILE+4(1),WRKZ   START WITH FIRST SPECIFIED ENTRY             
*                                                                               
GETLISX  B     EXITEQ                                                           
*                                                                               
GETLISD  DC    H'0'                                                             
                                                                                
***********************************************************************         
* ANY MORE INPUT FILES                                                          
***********************************************************************         
GETMORE  NTR1                                                                   
         CLI   INPUT,C'T'          NOT IF TAPE LOAD                             
         BE    EXITNE                                                           
*                                                                               
         LA    RE,WRKZNTRY         START WITH FIRST FILE                        
GETM010  CLC   WRKFILE+4(1),1(RE)                                               
         BE    GETM050                                                          
         LA    RE,8(RE)                                                         
         CLI   0(RE),0             LAST FILE SO EOF                             
         BE    EXITNE                                                           
         B     GETM010                                                          
*                                                                               
GETM050  LA    RE,8(RE)            SELECT NEXT FILE AND EXIT OK                 
         CLI   0(RE),0             LAST FILE SO EOF                             
         BE    EXITNE                                                           
         MVC   WRKFILE+4(1),1(RE)                                               
*                                                                               
         LA    R1,WRKZ             TEST FOR FILE IN SELECTION LIST              
GETM060  CLC   0(1,R1),WRKFILE+4                                                
         BE    EXITEQ                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '          NOT IN LIST SO GET NEXT FILE                 
         BE    GETM010                                                          
         B     GETM060                                                          
                                                                                
***********************************************************************         
* FILTER OUT UNWANTED FILES                                                     
***********************************************************************         
FILTER   NTR1                                                                   
*                                                                               
FILT010  OC    WLUSRID,WLUSRID     TEST FOR ZERO USER                           
         BZ    FILTERN                                                          
         OC    USERID,USERID       TEST FOR USER=ALL                            
         BZ    FILT020                                                          
         CLC   USERID,WLUSRID      OR SPECIFIC USER                             
         BE    FILT020                                                          
         B     FILTERN                                                          
*                                                                               
FILT020  SR    RF,RF               CDATE FILTER <=>                             
         IC    RF,CDATE                                                         
         CLC   WLAGELD,CDATE+1                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FILT030                                                        
         B     FILTERN                                                          
*                                                                               
FILT030  SR    RF,RF               DDATE FILTER <=>                             
         B     FILT040             AGEDD NO LONGER DEFINED                      
         IC    RF,DDATE                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FILT040                                                        
         B     FILTERN                                                          
*                                                                               
FILT040  SR    RF,RF               RDATE FILTER <=>                             
         IC    RF,RDATE                                                         
         CLC   WLAGERD,RDATE+1                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FILT050                                                        
         B     FILTERN                                                          
*                                                                               
FILT050  LA    R0,6                6 CHR FILE ID                                
         LA    R1,FILE                                                          
         LA    RF,WLFILEID                                                      
FILT051  CLI   0(R1),C' '          BLANK IS LAST CHR                            
         BE    FILT060                                                          
         CLI   0(R1),C'*'          * MATCHES ALL                                
         BE    *+14                                                             
         CLC   0(1,R1),0(RF)                                                    
         BNE   FILTERN                                                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FILT051                                                       
*                                  RID# (report ID #)                           
FILT060  LLC   RF,RID#             Branch condition                             
         CLC   WLREPRNO,RID#+1     Actual value                                 
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FILT070                                                        
         B     FILTERN                                                          
FILT070  DS    0H                                                               
*                                                                               
FILTERY  B     EXITEQ                                                           
FILTERN  B     EXITNE                                                           
                                                                                
***********************************************************************         
* GET NEXT WRKZ RECORD                                                          
***********************************************************************         
READREC  NTR1                                                                   
*                                                                               
         CLI   INPUT,C'T'                                                       
         BE    *+12                                                             
         BAS   RE,READWK                                                        
         B     READR010                                                         
         GET   TAPEIN,IOAREAF                                                   
*                                                                               
READR010 CLC   WLSOFLAB,=C'*EOFEOF*'     END OF FILE                            
         BE    EXITNE                                                           
         CLC   WLSOFLAB,=C'*EOFERR*'     END OF FILE ERROR                      
         BE    EXITNE                                                           
         CLC   WLSOFLAB+2(6),=C'OFEERR'  BAD DMWRKZ END OF FILE ERROR           
         BNE   READR020                                                         
         MVC   WLSOFLAB,=C'*EOFERR*'                                            
         MVC   IOAREAF,=X'000C0000'                                             
         B     EXITNE                                                           
*                                                                               
READR020 CLI   INPUT,C'T'                                                       
         BE    EXITEQ                                                           
         ICM   R1,15,IOAREA              SET UP TAPE LEN                        
         A     R1,=X'00040000'                                                  
         STCM  R1,15,IOAREAF                                                    
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* PUT NEXT WRKZ RECORD                                                          
***********************************************************************         
PUTREC   NTR1                                                                   
         MVI   DMCB+8,0            CLEAR ERROR FLAG                             
*                                                                               
         CLI   OUTPUT,C'T'                                                      
         BNE   PUTR500                                                          
*                                                                               
* TAPE OUTPUT (DUMP)                                                            
*                                                                               
         CLC   IOAREAF(2),=H'0004'   Deal with bad record length                
         BL    PUTR020                                                          
         CLC   IOAREAF(2),=AL2(WRKZRECL)                                        
         BH    PUTR020                                                          
*                                                                               
         CLC   WLSOFLAB,=C'*EOFEOF*' END-OF-FILE                                
         BNE   PUTR010                                                          
         CLI   FERI,FERL             Previous rec in file had bad len?          
         BNE   PUTR010               . NO                                       
         MVC   WLSOFLAB,=C'*EOFERR*' . YES, mark file as bad                    
         MVI   FERI,0                Clear file error indicator                 
*                                                                               
PUTR010  PUT   TAPEOUT,IOAREAF                                                  
*                                                                               
         CLC   WLSOFLAB,=C'*EOFERR*' END-OF-FILE error?                         
         BNE   EXITEQ                                                           
         MVI   DMCB+8,X'80'          Simulate END-OF-FILE error                 
         MVC   ERRINF,CSOFNDX                                                   
         B     EXITNE                                                           
*                                                                               
PUTR020  MVI   DMCB+8,X'41'          Simulate format error for bad len          
         MVC   ERRINF(6),=C'RECLN='                                             
         MVI   FERI,FERL                                                        
         GOTO1 =V(HEXOUT),PLIST,IOAREAF,ERRINF+6,2                              
         B     EXITNE                                                           
*                                                                               
* DISK OUTPUT (RESTORE)                                                         
*                                                                               
PUTR500  BAS   RE,WRITEWK          WRITE WRKZ RECORD                            
         BE    EXITEQ                                                           
         B     EXITNE                                                           
                                                                                
***********************************************************************         
* CLOSE ALL AND EXIT                                                            
***********************************************************************         
CLOSEALL NTR1                                                                   
*                                                                               
CLOSE010 CLI   INPUT,C'T'          TEST INPUT=TAPE                              
         BNE   CLOSE020                                                         
         CLOSE TAPEIN              CLOSE TAPEIN                                 
*                                                                               
CLOSE020 CLI   OUTPUT,C'T'         TEST OUTPUT=TAPE                             
         BNE   CLOSE030                                                         
         CLOSE TAPEOUT             CLOSE TAPEOUT                                
*                                                                               
CLOSE030 EQU   *                                                                
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* CALL WRKZ WITH BUFFER COMMAND                                                 
***********************************************************************         
INITWK   ST    RE,SAVERE                                                        
         LA    RF,CIADDR                                                        
         MVC   CIADDR,=X'00001001'                                              
         GOTO1 =V(DATAMGR),DMCB,BUFFER,WRKFILE,WKKEY,(RF),AWKBUFF               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AWKBUFF                                                       
         MVC   CIDATA,12(R1)                                                    
*                                                                               
         LH    R1,CIBLKLN          CALCULATE SIZE OF 1S AND 2S                  
         MH    R1,CIHIREC                                                       
         LR    R0,R1                                                            
         MH    R1,CITRKS                                                        
         ST    R1,CI1SIZE                                                       
         LR    R1,R0                                                            
         MH    R1,CJTRKS                                                        
         ST    R1,CI2SIZE                                                       
*                                                                               
         SR    R0,R0               CALCULATE TOTAL SIZE OF FILE                 
         ICM   R1,15,CICITOT                                                    
*                                                                               
         M     R0,CI1SIZE                                                       
         ST    R1,CITSIZE                                                       
         LH    R1,CJCITOT                                                       
         M     R0,CI2SIZE                                                       
         A     R1,CITSIZE                                                       
         ST    R1,CITSIZE                                                       
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* CALL WRKZ WITH DMPRINT COMMAND                                                
***********************************************************************         
WRITEWK  ST    RE,SAVERE           FOR VARIABLE OR HEADER                       
*                                                                               
         LA    RF,IOAREA                                                        
         GOTO1 =V(DATAMGR),DMCB,DMPRINT,WRKFILE,WKKEY,(RF),AWKBUFF              
         CLI   DMCB+8,0                                                         
*                                                                               
WRITEWKX L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* CALL WRKZ WITH SEQ COMMAND                                                    
***********************************************************************         
READWK   LA    RF,IOAREA           FOR VARIABLE OR HEADER                       
*                                                                               
         ST    RE,SAVERE                                                        
         OI    UKFLAG,X'C0'                                                     
         GOTO1 =V(DATAMGR),DMCB,SEQ,WRKFILE,WKKEY,(RF),AWKBUFF                  
         CLI   8(R1),X'90'                                                      
         BE    RDWKEOF                                                          
         CLI   8(R1),0                                                          
         BNE   RERROR                                                           
         L     RE,SAVERE           EXIT EQU ALL OK                              
         BR    RE                                                               
*                                                                               
RDWKEOF  ICM   RE,15,SAVERE        EXIT NEQ END OF FILE                         
         BR    RE                                                               
*                                                                               
RERROR   DC    H'0'                DUMP IF BAD RETURN                           
                                                                                
***********************************************************************         
* PUT INDEX RECORD TO SORTER                                                    
***********************************************************************         
PUTSORT  ST    RE,SAVERE                                                        
*                                                                               
         CLI   REPFLG,C'N'               TEST FOR REPORT=NO                     
         BE    PUTSORTX                                                         
         MVC   WLINDEX-1(1),WRKFILE+4                                           
         MVC   BYTE,WLSOFEND                                                    
*                                                                               
         CLI   MODE,C'M'                 MODE=MOVE?                             
         BNE   *+10                                                             
         MVC   WLSOFEND(1),ORGWRKZ                                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',WLINDEX-1                                
*                                                                               
         MVC   WLSOFEND(1),BYTE                                                 
PUTSORTX L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* PRINT AN ERROR LINE                                                           
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
         TM    DMCB+8,X'40'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'DISK ERROR'                                       
         B     PUTERRS1                                                         
         TM    DMCB+8,X'80'                                                     
         BNO   *+14                                                             
         MVC   PLINE+1(10),=C'EOF ERROR '                                       
         B     PUTERRS2                                                         
*                                                                               
PUTERRS1 MVC   PLINE+12(10),ERRINF                                              
         MVC   ERRINF,SPACES                                                    
         L     RF,AWKBUFF                                                       
         GOTO1 =V(HEXOUT),PLIST,(RF),PLINE+23,20                                
         BAS   RE,PRINTL                                                        
         LA    R1,DMCB                                                          
         B     EXITEQU                                                          
*                                                                               
PUTERRS2 MVC   PLINE+12(10),ERRINF                                              
         MVC   ERRINF,SPACES                                                    
         LA    RF,CSOFNDX                                                       
         GOTO1 =V(HEXOUT),PLIST,(RF),PLINE+23,20                                
         BAS   RE,PRINTL                                                        
         LA    R1,DMCB                                                          
         B     EXITEQU                                                          
                                                                                
***********************************************************************         
* CHECK PROGRESS                                                                
***********************************************************************         
CHECKPR  NTR1                                                                   
         L     R1,RCWRKZC          Bump file count                              
         AHI   R1,1                                                             
         ST    R1,RCWRKZC                                                       
*                                                                               
         CLI   RCWRKZ,C'Z'         MODE=MOVE, MAY NOT BE IN ORDER               
         BNE   CHKPR002                                                         
         CLI   WRKFILE+4,C' '                                                   
         BNE   CHKPR004                                                         
CHKPR002 CLC   RCWRKZ,WRKFILE+4    Same WRKZ                                    
         BNE   CHKPR010            . No, do something                           
CHKPR004 SR    R0,R0               Every 10000 files show progress              
         D     R0,=F'10000'                                                     
         CHI   R0,0                                                             
         BNE   CHKPRX                                                           
         B     CHKPR050                                                         
*                                                                               
CHKPR010 CLI   RCWRKZ,C' '         First time through                           
         BE    CHKPR020            . yes, just print                            
         SHI   R1,1                                                             
         ST    R1,RCWRKZC                                                       
         MVC   RCMSGF,RCWRKZ       Last for this WRKZ                           
         EDIT  (B4,RCWRKZC),RCMSGC,ZERO=NOBLANK,ALIGN=LEFT                      
         LA    R4,RCMSGH           Message header                               
         WTO   TEXT=(R4)                                                        
*                                                                               
         CLI   WRKFILE+4,C' '                                                   
         BE    CHKPRX                                                           
*                                                                               
         LHI   R1,1                Start over                                   
         ST    R1,RCWRKZC                                                       
CHKPR020 MVC   RCWRKZ,WRKFILE+4    keep track of WRKFile                        
*                                                                               
CHKPR050 MVC   RCMSGF,RCWRKZ       Output progress message                      
         EDIT  (B4,RCWRKZC),RCMSGC,ZERO=NOBLANK,ALIGN=LEFT                      
         LA    R4,RCMSGH                                                        
         WTO   TEXT=(R4)                                                        
*                                                                               
CHKPRX   B     EXIT                                                             
                                                                                
***********************************************************************         
* PRINT A WRKZ REPORT                                                           
***********************************************************************         
REPORT   NTR1                                                                   
*                                                                               
         ZAP   LINE,=P'99'         START WITH A TITLE LINE                      
         MVI   ORGWRKZ,0                                                        
*                                                                               
REP010   MVC   PLINE,T2                                                         
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BZ    REPORTX                                                          
         MVC   WLINDEX-1(126),0(RF)                                             
*                                                                               
         OC    GUSER,GUSER         NO TOTALS FIRST TIME                         
         BZ    REP015                                                           
         CLC   WLUSRID,GUSER       SKIP IF SAME AS PREV                         
         BE    REP020                                                           
         BAS   RE,TOTALS           PRINT USER TOTALS                            
*                                                                               
         CLC   CUSERF,WLINDEX-1                                                 
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
REP015   MVC   GUSER,WLUSRID       GET USERID NAME                              
         BAS   RE,GETUSR                                                        
*                                                                               
REP020   BAS   RE,COUNT            KEEP A COUNT OF TOTALS                       
         MVC   PLUSER,GUSERN                                                    
         MVC   PLWRKZ(1),WLINDEX-1 WRKZ FILE NUMBER                             
         MVC   DUB,WLKEY                                                        
         BAS   RE,GETKEY           FILE KEY AND REF#                            
         MVC   PLKEY,WORK                                                       
         MVC   FULL,WLFILENO                                                    
         MVI   EDITCHR,C'T'        EDIT AS TEXT, NOT COUNT                      
         BAS   RE,EDITF                                                         
         MVC   PLREFNO,DUB+2                                                    
*                                                                               
         MVC   PLDESC,WLDESC                                                    
*                                                                               
         CLI   MODE,C'M'           WAS THIS MOVED?                              
         BNE   REP030                                                           
         CLC   WLSOFEND(1),WLINDEX-1                                            
         BE    REP030              . NO                                         
         MVC   ORGWRKZ,WLSOFEND                                                 
*                                                                               
REP030   MVC   PLTYPE,SPACES                                                    
         MVC   PLTYPE+1(1),WLTYPE                                               
*                                                                               
REP040   MVC   PLATTR,=C'O.EP.X.S'                                              
         LA    RF,PLATTR                                                        
         MVC   BYTE,WLATTB         SET /CLR ATTR BITS                           
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
REP050   MVC   BYTE,WLSTAT                                                      
         BAS   RE,STATOUT                                                       
         MVC   PLSTAT,DUB                                                       
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),WLAGES                                                 
         BAS   RE,EDITH                                                         
         MVC   PLSIZE,DUB+5                                                     
*                                                                               
EP060    MVC   PLLIVED,DOTS                                                     
         GOTO1 =V(DATCON),DMCB,(2,WLDATEL),(17,PLLIVED)                         
         MVC   PLDEADD,DOTS                                                     
         OC    WLDATED,WLDATED                                                  
         BZ    REP061                                                           
         GOTO1 (RF),(R1),(2,WLDATED),(17,PLDEADD)                               
REP061   MVC   PLRETAD,DOTS                                                     
         GOTO1 (RF),(R1),(2,WLAGERD),(17,PLRETAD)                               
*                                                                               
         MVC   DUB(2),WLTIMEL                                                   
         BAS   RE,TIMEOUT                                                       
         MVC   PLLIVET,DUB+2                                                    
*                                                                               
REP070   MVC   PLDEADT,DOTS                                                     
         MVC   DUB(2),WLTIMED                                                   
         OC    DUB(2),DUB                                                       
         BZ    REP080                                                           
         BAS   RE,TIMEOUT                                                       
         MVC   PLDEADT,DUB+2                                                    
*                                                                               
REP080   SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,WLAGERT                                                       
         MH    R1,=H'10'           CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BAS   RE,TIMEOUT                                                       
         MVC   PLRETAT,DUB+2       RETAIN TIME                                  
*                                                                               
         MVC   FULL,WLRECS                                                      
         BAS   RE,EDITF                                                         
         MVC   PLRECS,DUB+3                                                     
         MVC   FULL,WLFSIZE                                                     
         BAS   RE,EDITF                                                         
         MVC   PLBYTES,DUB+3                                                    
         MVC   HALF,WLAVGRL                                                     
         BAS   RE,EDITH                                                         
         MVC   PLAVG,DUB+3                                                      
         MVC   HALF,WLMAXRL                                                     
         BAS   RE,EDITH                                                         
         MVC   PLMAX,DUB+3                                                      
         MVC   BYTE,WLNCI                                                       
         BAS   RE,EDITB                                                         
         MVC   PLCIS,DUB+5                                                      
         MVC   BYTE,WLNCIX                                                      
         BAS   RE,EDITB                                                         
         MVC   PLCISX,DUB+5                                                     
*                                                                               
         BAS   RE,PRINTL                                                        
         B     REP010                                                           
*                                                                               
REPORTX  BAS   RE,TOTALS           PRINT USER TOTALS                            
         BAS   RE,FTOTALS          PRINT FILE TOTALS                            
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* PRINT TOTALS LINE                                                             
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
*                                                                               
         CLI   MODE,C'M'                                                        
         BNE   TOTALS10                                                         
         CLI   ORGWRKZ,0                                                        
         BE    TOTALS10                                                         
         MVC   PLINE+122(17),=C'**MOVED FROM WRKZ'                              
         MVC   PLINE+139(1),ORGWRKZ                                             
         MVC   PLINE+140(2),=C'**'                                              
         MVI   ORGWRKZ,0                                                        
*                                                                               
TOTALS10 BAS   RE,PRINTL                                                        
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
                                                                                
***********************************************************************         
* COUNT TOTALS FOR USER / ALL                                                   
***********************************************************************         
COUNT    NTR1                                                                   
         SR    R1,R1               SET R0 TO NUMBER OF PART 2S                  
         SR    R0,R0                                                            
         IC    R1,WLNCI                                                         
         IC    R0,WLNCIX                                                        
         BCTR  R1,0                                                             
         AR    R0,R1                                                            
*                                                                               
         MVC   CUSERN,GUSERN       SAVE NAME                                    
         MVC   CUSERF,WLINDEX-1    SAVE NAFILE NUMBER                           
*                                                                               
         LA    RF,TCTFILE          TOTALS TOR FILE                              
         BAS   RE,COUNTR                                                        
         LA    RF,UCTFILE          TOTALS FOR USER                              
         BAS   RE,COUNTR                                                        
*                                                                               
         TM    WLSTAT,X'80'        TEST FOR ACVTIVE                             
         BZ    COUNTX                                                           
         CLC   WLAGERD,TODAYC      TEST FOR WELL EXPIRED                        
         BL    COUNTX                                                           
         BH    COUNT050                                                         
         CLC   WLAGERT,TIMEI       TEST FOR JUST EXPIRED                        
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
         A     R1,WLFSIZE                                                       
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
                                                                                
***********************************************************************         
* BUILD LINE OF COUNTER SUMMARY                                                 
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
                                                                                
***********************************************************************         
* GET USERID FROM 2 CHR NUMBER                                                  
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
                                                                                
***********************************************************************         
* BUILD SPPSDDC IN WORK FROM W_KEY IN DUB                                       
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
                                                                                
***********************************************************************         
* STATUS ROUTINE BYTE=STAT DUB=RESULT                                           
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
                                                                                
***********************************************************************         
* EDIT ROUTINE                                                                  
***********************************************************************         
EDITF    ST    RE,SAVERE           EDIT FROM FULL                               
         L     RE,FULL                                                          
         B     EDITRT                                                           
EDITH    ST    RE,SAVERE           EDIT FROM HALF                               
         LH    RE,HALF                                                          
         B     EDITRT                                                           
EDITB    ST    RE,SAVERE           EDIT FROM BYTE                               
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
*                                                                               
EDITRT   CLI   EDITCHR,C'T'        TREAT AS TEXT, NOT AS A COUNT                
         BNE   EDITRE                                                           
         MVI   EDITCHR,C' '                                                     
         B     EDITRF                                                           
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
         MVC   DUB+7(1),EDITCHR    INSERT DEC AND EDIT CHR                      
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
                                                                                
***********************************************************************         
* TIME OUTPUT ROUTINE                                                           
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
                                                                                
***********************************************************************         
* GET TIME NOW                                                                  
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
                                                                                
***********************************************************************         
* PRINT TITLE                                                                   
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
* REPORT LINES                                                                  
***********************************************************************         
T1       DC    C'1'                                                             
         DC    AL1(TL),10AL1(HB)                                                
         DC    AL1(TM),02AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(TM),06AL1(HB)                                                
         DC    AL1(TM),16AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),08AL1(HB)                                                
         DC    AL1(TM),08AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TM),03AL1(HB)                                                
         DC    AL1(TR)                                                          
         DC    16AL1(BB)                                                        
*                                                                               
T2       DC    C' '                                                             
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB),02AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(VB),06AL1(BB)                                                
         DC    AL1(VB),16AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),08AL1(BB)                                                
         DC    AL1(VB),08AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),07AL1(BB)                                                
         DC    AL1(BB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB),03AL1(BB)                                                
         DC    AL1(VB)                                                          
         DC    16AL1(BB)                                                        
*                                                                               
TITLE2   DC    C' '                                                             
         DC    AL1(VB),CL10'Userid'                                             
         DC    AL1(VB),CL02'Wk'                                                 
         DC    AL1(VB),CL07'File'                                               
         DC    AL1(VB),CL06'Ref#'                                               
         DC    AL1(VB),CL16'Description'                                        
         DC    AL1(VB),CL03'Typ'                                                
         DC    AL1(VB),CL08'Attrib'                                             
         DC    AL1(VB),CL08'Status'                                             
         DC    AL1(VB),CL03'Siz'                                                
         DC    AL1(VB),CL13'Cretated on'                                        
         DC    AL1(VB),CL13'Sent on'                                            
         DC    AL1(VB),CL13'Retain until'                                       
         DC    AL1(VB),CL05'Recs'                                               
         DC    AL1(VB),CL05'Bytes'                                              
         DC    AL1(VB),CL05'Avg'                                                
         DC    AL1(VB),CL05'Max'                                                
         DC    AL1(VB),CL03'Cis'                                                
         DC    AL1(VB),CL03'Xci'                                                
         DC    AL1(VB)                                                          
         DC    16AL1(BB)                                                        
*                                                                               
T3       DC    C' '                                                             
         DC    AL1(ML),10AL1(HB)                                                
         DC    AL1(MM),02AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(MM),06AL1(HB)                                                
         DC    AL1(MM),16AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),08AL1(HB)                                                
         DC    AL1(MM),08AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MM),03AL1(HB)                                                
         DC    AL1(MR)                                                          
         DC    16AL1(BB)                                                        
*                                                                               
T4       DC    C' '                                                             
         DC    AL1(BL),10AL1(HB)                                                
         DC    AL1(BM),02AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(BM),06AL1(HB)                                                
         DC    AL1(BM),16AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),08AL1(HB)                                                
         DC    AL1(BM),08AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),07AL1(HB)                                                
         DC    AL1(HB),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BM),03AL1(HB)                                                
         DC    AL1(BR)                                                          
         DC    16AL1(BB)                                                        
                                                                                
***********************************************************************         
* BOX EQUATES                                                                   
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
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
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
                                                                                
***********************************************************************         
* PARAMETER CARDS AND HANDLING ROUTINE                                          
***********************************************************************         
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
*        X'0080'                   NUMBER IS A FULLWORD                         
***********************************************************************         
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4,4),X'0000',AL3(INPUT)                           
         DC    C'OUTPUT ',AL1(5,4),X'0000',AL3(OUTPUT)                          
         DC    C'USER   ',AL1(3,2),X'8000',AL3(VALUSR)                          
         DC    C'WRKZ   ',AL1(3,16),X'0000',AL3(WRKZ)                           
         DC    C'CDATE  ',AL1(4,2),X'6100',AL3(CDATE)                           
         DC    C'DDATE  ',AL1(4,2),X'6100',AL3(DDATE)                           
         DC    C'RDATE  ',AL1(4,2),X'6100',AL3(RDATE)                           
         DC    C'REPORT ',AL1(5,1),X'0000',AL3(REPFLG)                          
         DC    C'REPLACE',AL1(6,1),X'0000',AL3(REPLACE)                         
         DC    C'RESET  ',AL1(4,1),X'0000',AL3(RESET)                           
         DC    C'FILE   ',AL1(3,6),X'8000',AL3(VALFILE)                         
         DC    C'MODE   ',AL1(3,6),X'8000',AL3(VALMODE)                         
         DC    C'WRSRV  ',AL1(4,1),X'8000',AL3(VALSRV)  SUPPORT WRSRV=          
         DC    C'WRITE  ',AL1(4,1),X'8000',AL3(VALSRV)  AND WRITE=              
         DC    C'RID#   ',AL1(3,6),X'6880',AL3(RID#)                            
         DC    C'ADJ#   ',AL1(3,6),X'4880',AL3(ADJ#)                            
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
INPUT    DC    C'DISK'             INPUT=DISK/TAPE                              
OUTPUT   DC    C'TAPE'             OUTPUT=DISK/TAPE                             
REPFLG   DC    C'Y'                REPORT=YES                                   
REPLACE  DC    C'N'                REPLACE=NO                                   
RESET    DC    C'N'                RESET=NO                                     
WRKZ     DC    C'123456789ABCDEFG '  WRKZ=123456789                             
USERID   DC    X'0000'             USER=ALL                                     
FILE     DC    C'*     '           FILE=ALL                                     
CDATE    DC    XL3'F00000'         COMP LIVE DATE                               
DDATE    DC    XL3'F00000'         COMP DEAD DATE                               
RDATE    DC    XL3'F00000'         COMP RETAIN DATE                             
MODE     DC    C'      '           MODE=LOAD/MOVE                               
RID#     DC    X'F0',X'00000000'   Report ID# to filter                         
ADJ#     DC    X'F0',X'00000000'   Report Adjust number                         
                                                                                
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
         ST    R4,FULL                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         L     R4,FULL                                                          
         TM    10(R4),X'80'        FULLWORD?                                    
         BZ    VALC500                                                          
         ST    R1,0(RF)                                                         
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
CERRFIL  LA    R1,=C'INVALID FILE    '                                          
         B     CERRX                                                            
CERRMOD  LA    R1,=C'INVALID MODE    '                                          
         B     CERRX                                                            
CERRYON  LA    R1,=C'MUST BE YES/NO  '                                          
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
                                                                                
***********************************************************************         
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                             
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
                                                                                
***********************************************************************         
* VALIDATE USRID=                                                               
***********************************************************************         
VALUSR   NTR1                                                                   
*                                                                               
         XC    KEY,KEY             FIND ID REC                                  
         MVI   KEY,CTIKTYPQ                                                     
         MVC   KEY+CTIKID-CTIREC,SPACES                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+CTIKID-CTIREC(0),0(R2)                                       
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,KEY,ACTIO                         
         CLI   8(R1),0                                                          
         BNE   CERRUSR                                                          
         L     R1,ACTIO                                                         
         LA    R1,CTIDATA-CTIREC(R1)                                            
VALUS010 CLI   0(R1),X'02'         LOOK FOR ID NUMBER ELEMENT                   
         BE    VALUS020                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R1)          NEXT                                         
         BZ    CERRUSR                                                          
         AR    R1,R0                                                            
         B     VALUS010                                                         
VALUS020 MVC   USERID(2),2(R1)                                                  
         B     EXITEQU                                                          
                                                                                
***********************************************************************         
* VALIDATE FILE= R2=FILE R1=EX LEN                                              
***********************************************************************         
VALFILE  NTR1                                                                   
         MVC   FILE,SPACES        CLEAR TO SPACES                               
         CH    R1,=H'6'                                                         
         BE    VALF500                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILE(0),0(R2)      JUST COPY INTO FILE                           
         B     EXITEQU                                                          
*                                                                               
VALF500  MVC   FILE(4),0(R2)                                                    
         TM    4(R2),X'F0'        TEST NUMERIC DAY                              
         BNO   CERRFIL                                                          
         TM    5(R2),X'F0'                                                      
         BNO   CERRFIL                                                          
         MVO   5(1,R2),4(1,R2)                                                  
         MVC   FILE+4(1),5(R2)    MOVE PACKED DAY                               
*                                                                               
         MVC   FILE+5(1),6(R2)    AND FINALY CLASS                              
*                                                                               
         B     EXITEQU                                                          
                                                                                
***********************************************************************         
* VALIDATE MODE= LOAD/MOVE                                                      
***********************************************************************         
VALMODE  NTR1                                                                   
         CLC   =C'LOAD',0(R2)            LOAD                                   
         BNE   VALMODE2                                                         
         MVC   MODE(4),=C'LOAD'                                                 
         B     VALMODE8                                                         
*                                                                               
VALMODE2 CLC   =C'MOVE',0(R2)            LOAD AND MOVE A USER ID                
         BNE   CERRMOD                                                          
         MVC   MODE(4),=C'MOVE'                                                 
*                                                                               
VALMODE8 CLI   RDATE,X'F0'         Any RDATE card?   (B)                        
         BNE   VALMODEX            . No, then modify RDATE for load             
         MVI   RDATE,X'B0'         <=                (BH)                       
         MVC   RDATE+1(2),TODAYC   Don't bother w/ expired reports              
*                                                                               
VALMODEX B     EXITEQU                                                          
                                                                                
***********************************************************************         
* SERVICE FILES READ-ONLY                                                       
***********************************************************************         
VALSRV   NTR1                                                                   
         CLI   0(R2),C'Y'         WRITE=YES                                     
         BE    EXITEQU                                                          
         CLI   0(R2),C'N'         WRITE=NO                                      
         BNE   CERRYON                                                          
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND-SSOOFF(RF),SSOWSRN                                      
         B     EXITEQU                                                          
                                                                                
***********************************************************************         
* CONSTANTS & LTORG                                                             
***********************************************************************         
DMREAD   DC    CL8'DMREAD '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
BUFFER   DC    CL8'BUFFER'                                                      
GLIST    DC    CL8'GLIST  '                                                     
GFILE    DC    CL8'GFILE  '                                                     
WRKZIL   DC    CL8'WRKZIL '                                                     
SEQ      DC    CL8'SEQ '                                                        
SPACES   DC    CL166' '                                                         
DOTS     DC    CL16'................'                                           
MAXLINE  DC    P'60'                                                            
*                                                                               
SRTCARD  DC    C'SORT FIELDS=(1,9,BI,A) '                                       
RECCARD  DC    C'RECORD TYPE=F,LENGTH=126 '                                     
*                                                                               
RCMSGH   DC    AL2(RCMSGL)                                                      
RCMSGM   DC    C' -PROGRESS- '                                                  
         DC    C'WRKZ'                                                          
RCMSGF   DC    C' '                                                             
         DC    C' - file read count = '                                         
RCMSGC   DC    C'XXXXXXXXXX'                                                    
RCMSGL   EQU   *-RCMSGM                                                         
*                                                                               
WRKZRECL EQU   13680                                                            
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=TAPEEND,        X        
               RECFM=VB,BLKSIZE=0,LRECL=0,BUFNO=2                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,BLKSIZE=0,LRECL=WRKZRECL,BUFNO=2                        
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
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
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
ERRINF   DS    CL10                                                             
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
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
RCWRKZ   DS    C                   PROGRESS MESSAGE WRKZ Number                 
RCWRKZC  DS    F                   PROGRESS MESSAGE WRKZ COUNT                  
ORGWRKZ  DS    C                   ORIGINAL WORKER FILE ON MOVE                 
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
         DS    CL40                                                             
WKKEY    DS    CL40                                                             
*                                                                               
CSOFNDX  DS    CL(L'WLINDEX)       SOF INDEX FOR CURRENT FILE                   
*                                                                               
FERI     DS    C                   FILE ERROR INDICTOR                          
FERL     EQU   C'L'                . RECORD LENGTH ERROR                        
*                                                                               
WRKZLST  DS    0X                                                               
WRKZMAX  DS    X                   NUMBER OF WRKZ FILES                         
         DS    X                                                                
WRKZFLG  DS    X                                                                
         DS    XL5                                                              
WRKZNTRY DS    16XL8               MAXIMUM OF 16 WRKZ FILES                     
WRKZLSTX DS    XL8                                                              
*                                                                               
         DS    0F                                                               
       ++INCLUDE DMWRKZW                                                        
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
IOAREA   DS    CL(WRKZRECL)                                                     
*                                                                               
         DS    CL8                                                              
CTIO     DS    2048C                                                            
*                                                                               
WKBUFF   DS    14336C                                                           
*                                                                               
SPARE    DS    1024C                                                            
WORKX    EQU   *                                                                
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
PLINED   DSECT                                                                  
         DS    CL1                                                              
PBOX1    DS    CL1                                                              
PLUSER   DS    CL10                                                             
         DS    CL1                                                              
PLWRKZ   DS    CL2                                                              
         DS    CL1                                                              
PLKEY    DS    CL7                                                              
         DS    CL1                                                              
PLREFNO  DS    CL6                                                              
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
* DMWRKZL                                                                       
* DMWRKZK                                                                       
* CTGENFILE                                                                     
* FASSBOFF                                                                      
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMWRKZL                                                        
       ++INCLUDE DMWRKZD                                                        
       ++INCLUDE DMWRKZK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DMWRKZZ   03/11/16'                                      
         END                                                                    
