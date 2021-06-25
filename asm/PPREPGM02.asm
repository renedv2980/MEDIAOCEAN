*          DATA SET PPREPGM02  AT LEVEL 045 AS OF 07/06/18                      
*PHASE PPGM02A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*                                                                               
         TITLE 'PPGM02 - CHANGE LOG'                                            
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-23466   07/06/18 UCOMM BUG & INCLUDE BILL IF UCOMM MISSING*         
***********************************************************************         
*                                                                               
*  BPLA  11/14  NEW FORMAT - OLD SOURCE IS PPREPGM02O                           
*               COMMA DELIMITED,COLLAPSED BY PO#                                
*                                                                               
*  BPLA  1/08   ADD YEAR OF SERVICE FILTER                                      
*                                                                               
*                                                                               
         TITLE 'PPGM02 - PRINTPAK GM INTERFACE'                                 
*                                                                               
*                                                                               
* QPUB+1(2)     = YEAR OF SERVICE FILTER (LAST 2 DIGITS)                        
*                                                                               
PPGM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPGM02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         LA    R8,SPACEND                                                       
         USING PPGMWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    REQL                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,FBUYCLI        FRIST FOR CLIENT                             
         BE    FBC                                                              
         CLI   MODE,LBUYCLI       LAST FOR CLIENT                               
         BE    LBCLI                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
RUNF     DS    0H                  RUN FIRST                                    
         MVI   ERRORSW,0                                                        
         MVI   ALLOWSW,0           DYNAMIC ALLOCATION                           
*                                                                               
         SR    R0,R0            SET INVOICE LIST BINSRCH PARS                   
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,INVTLEN                                                       
         LHI   R4,INVKLEN                                                       
         L     R5,=A(INVMAX)                                                    
         STM   R0,R5,INVPARS                                                    
*                                                                               
         SR    R0,R0            SET PPGTAB BINSRCH PARS                         
         L     R1,=A(PPGTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,PPGTLEN                                                       
         LHI   R4,PPGKLEN                                                       
         L     R5,=A(BINMAX)                                                    
         STM   R0,R5,BINVALS                                                    
*                                                                               
         LA    R3,RUNTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
         XC    TRUNINVS,TRUNINVS    TAPE INVOICE TOTALS                         
*                                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         LA    RF,PESTREC                                                       
         ST    RF,ADEST                                                         
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRD                                                         
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLT                                                         
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   REQF10                                                           
         MVI   FIRSTSW,1                                                        
*                                                                               
         MVI   ENDRPTSW,0         CLEAR SWITCH USED IN END PROCESSES            
*                                                                               
         XC    START(12),START                                                  
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
         XC    UCOMDATA,UCOMDATA                                                
*                                                                               
REQF10   DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
         CLI   QOPT1,C'N'          SKIP TAPE                                    
         BE    REQF30                                                           
         TM    ALLOWSW,X'01'       DYNALOC ONLY 1ST TIME                        
         BO    REQF30                                                           
*                                                                               
         LA    R3,PPDYNDSN                                                      
         MVC   13(2,R3),QAGENCY                                                 
*                                                                               
* PREVENT DYNALLOC FAILURES BY SERIALIZING ON THE DSN                           
*                                                                               
         ENQ   (MAJORNAM,(3),E,DSNLENQ,SYSTEM)                                  
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'PPGTAPE '),(0,0(R3))                         
         OPEN  (PPGTAPE,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    ALLOWSW,X'01'                                                    
*                                                                               
REQF30   DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         LA    R3,REQTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    CLTINVS,CLTINVS   CLEAR CLIENT AND REQ INVOICE TOTALS            
         XC    REQINVS,REQINVS                                                  
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   TOTSW,0                                                          
         OC    START(12),START                                                  
         BNZ   *+10                                                             
         MVC   START(12),QSTART    SET START-END FROM FRST REQ                  
         CLC   START(12),SPACES                                                 
         BNE   REQF40                                                           
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD       ***   CONVERT START             
         GOTO1 DATCON,DMCB,(0,START),(0,START)  *** TO NEW DATE FORMAT          
         MVC   END,START                                                        
REQF40   DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
         MVI   SVACTYR,0                                                        
         CLC   QPUB+1(2),SPACES                                                 
         BNH   REQFX                                                            
*                                                                               
         MVI   SVACTYR,X'00'          YEAR OF SERVICE FILTER                    
         MVC   WORK,=C'010101'                                                  
         MVC   WORK(2),QPUB+1                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   SVACTYR,WORK+6                                                   
*                                                                               
REQFX    B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
PPDYNDSN DC    CL(DSNLENQ)'PRTTAPE.PP0GMAG2'                                    
*        NOTE- SUFFIX 2 FOR NEW FORMAT                                          
DSNLENQ  EQU   20                                                               
MAJORNAM DC    C'PPGTAPE '                                                      
*                                                                               
PPGTAPE  DCB   DDNAME=PPGTAPE,DSORG=PS,RECFM=FB,LRECL=00300,           X        
               BLKSIZE=00300,MACRF=PM                                           
*                                                                               
FBC      DS    0H                  FIRST FOR CLIENT                             
*                                                                               
         XC    LASTCPE,LASTCPE                                                  
         XC    BINTABN,BINTABN       MUST CLEAR FOR BINSRCH                     
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
*                                                                               
         CLI   QCLIENT,C'*'        ONE OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLI   QCLIENT,C'$'        ALL OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLC   QCLIENT,=C'ALL'     ALL CLIENTS REQUEST ?                        
         BNE   FBC1D               NO - ONE CLIENT ONLY REQUEST                 
*                                                                               
FBC1B    MVI   PCLTREC,0           TO CLEAR MEDIA NAME OVERRIDE                 
*                                                                               
FBC1D    MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID2,SPACES                                                    
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),PCLTKCLT                                              
         MVC   SVMID+12(L'PCLTNAME),PCLTNAME                                    
         MVC   WORK(8),SPACES                                                   
         CLI   PCLTNUM,X'FF'                                                    
         BNE   FBC2                                                             
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),PCLTNUM+1(3)                                           
         MVI   WORK+5,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC2     MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'80'                                                       
         BNE   FBC3                                                             
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'7F'                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),DUB                                                    
         MVI   WORK+6,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC3     DS    0H                                                               
         MVC   WORK+1(3),PCLTNUM                                                
         OC    WORK+1(3),SPACES                                                 
         CLC   WORK+1(3),SPACES                                                 
         BE    FBC4X                                                            
         MVI   WORK,C'('                                                        
         LA    R1,WORK+3                                                        
FBC3C    CLI   0(R1),C' '                                                       
         BH    FBC3D                                                            
         BCT   R1,FBC3C                                                         
*                                                                               
FBC3D    MVI   1(R1),C')'                                                       
*                                                                               
FBC4     DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME                                           
FBC4C    CLI   0(R1),C' '                                                       
         BH    FBC4E                                                            
         BCT   R1,FBC4C                                                         
FBC4E    MVC   2(8,R1),WORK                                                     
*                                                                               
FBC4X    DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME+10                                        
FBC5     CLI   0(R1),C' '                                                       
         BH    FBC10                                                            
         BCT   R1,FBC5                                                          
*                                                                               
FBC10    LA    R2,SVMID2                                                        
         LA    R1,132(R1)           SAME DISPLACEMENT INTO SVMID2               
         SR    R1,R2                                                            
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'               SOMETING VERY WRONG                           
*                                                                               
         EX    R1,*+8                                                           
         B     FBCX                                                             
         MVC   SVMID2(0),=40C'-'                                                
*                                                                               
FBCX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
LBCLI    DS    0H                  LAST FOR CLIENT                              
*                                  SEND ENTRIES TO DOWNLOAD                     
         L     R2,BINTABN          FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
         CLI   QOPT1,C'N'          CREATE TAPE ?                                
         BE    ZEROCNT                                                          
*                                                                               
         L     R3,=A(PPGTAB)       I HAVE SOMETHING TO SEND                     
         XC    SVPPGKEY,SVPPGKEY                                                
LBCLI5   BRAS  RE,TAPEOUT                                                       
         LA    R3,PPGTLEN(R3)      TO NEXT RECORD IN TABLE                      
         BCT   R2,LBCLI5                                                        
*                                                                               
ZEROCNT  B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
REQL     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLIENT,C'$'                                                     
         BE    REQL20                                                           
*                                                                               
         MVC   CLTINVS,CLTINVCN    LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BE    REQL20              NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM LBLR OR LBOFF          
         BRAS  RE,PRBILRTN           GO FINISH LAST PRD/CLT                     
         MVI   ENDRPTSW,0          CLEAR                                        
*                                                                               
REQL20   DS    0H                                                               
*        CLI   RACTSW,0                                                         
*        BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BRAS  RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
REQL40   BRAS  RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
*                                                                               
REQLX    B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                  RUN LAST                                     
         BRAS  RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,RUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
         MVC   BLINE(15),=C'**FILE TOTALS**'                                    
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,TRUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT               
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
*********CLI   ERRORSW,0        ANY ERRORS?                                     
*********BE    RUNL10              NO                                           
*******  TM    ERRORSW,X'01'     ESTIMATE UCOMS ARE MISSING                     
*******  BO    RUNL3                                                            
***      MVC   P+3(32),=C'*** ESTIMATE USER1 INCORRECT ***'                     
***      BRAS  RE,PRNT                                                          
*** 3    TM    ERRORSW,X'02'     REGION "INVALID" ?                             
***      BNO   RUNL4                                                            
***      MVC   P+3(33),=C'*** REGION NOT A VALID NUMBER ***'                    
***      BRAS  RE,PRNT                                                          
*** 4    DS    0H                                                               
*                                                                               
*        DS    0H                                                               
*        MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
*        BRAS  RE,PRNT                                                          
*                                                                               
*                       CLOSE DOWNLOAD FILE HERE                                
RUNL10   DS    0H                                                               
***      CLI   DOWNACT,C'Y'       DOWNLOADING ACTIVITY?                         
***      BNE   EXIT                                                             
***      GOTO1 VDOWNLD,DMCB,(RA)                                                
         CLOSE (PPGTAPE)                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*                            ***** PROCESS BILL *****                           
         SPACE 2                                                                
PRBIL    DS    0H                                                               
         MVI   ENDRPTSW,0          CLEAR                                        
         BRAS  RE,PRBILRTN         BILL ROUTINE                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
***OFF                                                                          
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,CLTINVCN    LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   OACTSW,0            ANY BILL FOR OFFICE PROCESSED                
         BE    LBOFF5              NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM REQL OR LBOFF          
         BRAS  RE,PRBILRTN           MUST DO PRD/CLT TOTALS FIRST               
         MVI   ENDRPTSW,0          CLEAR                                        
*                                                                               
LBOFF5   MVI   BLPRD,C'*'                                                       
*****    MVC   BLPRD+1(1),RCSVOFC                                               
*****    MVI   BLPRD+2,C' '                                                     
         GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,BLPRD+1                              
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
*                                                                               
         MVI   OACTSW,0                                                         
*                                                                               
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   DS    0H                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,OFFINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         L     R0,REQINVS        ROLL OFFICE INVOICE TOTAL                      
         A     R0,OFFINVS        TO REQUEST                                     
         ST    R0,REQINVS                                                       
         XC    OFFINVS,OFFINVS                                                  
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
FBOFF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         MVI   OACTSW,0                                                         
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
***OFF                                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      TAPEOUT                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TAPEOUT  NTR1                                                                   
*                                                                               
         LA    RE,OUTREC           INIT TO SPACES                               
         LHI   R1,L'OUTREC-1                                                    
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         CLI   COLSW,C'Y'          HAVE I DONE COLHDS?                          
         BE    TAPEO2                                                           
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(COLHLEN-250),250(RF)                                  
         CLI   QOPT2,C'Y'          PRINT TRACE ?                                
         BNE   TAPEO1                                                           
         LH    R0,=H'300'                                                       
         GOTO1 =V(PRNTBL),DMCB,0,OUTREC,C'DUMP',(R0),=C'1D'                     
*                                                                               
TAPEO1   LA    R1,PPGTAPE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         MVI   COLSW,C'Y'                                                       
*                                                                               
*                                                                               
         USING PPGRECD,R3          R3 POINTS TO PPGREC                          
TAPEO2   LA    RE,OUTREC           INIT TO SPACES                               
         LHI   R1,L'OUTREC-1                                                    
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         CP    PPGBAMT,=P'0'      SKIP IF INVOICE TOTAL IS ZERO                 
         BE    TAPEOUTX                                                         
*                                                                               
         LA    R2,OUTREC                                                        
         MVI   0(R2),C'1'                                                       
         CP    PPGBAMT,=P'0'                                                    
         BNL   *+8                                                              
         MVI   0(R2),C'2'         CREDIT MEMO                                   
         MVI   1(R2),C','         DELIMITER                                     
         LA    R2,2(R2)                                                         
         MVC   0(09,R2),=C'98601255,'                                           
         LA    R2,09(R2)                                                        
         MVC   0(4,R2),=C'USD,'                                                 
         LA    R2,4(R2)                                                         
         MVC   0(L'PPGEUC1,R2),PPGEUC1                                          
         LA    R2,L'PPGEUC1(R2)                                                 
TAPEO3   CLI   0(R2),C' '                                                       
         BH    TAPEO5                                                           
         SH    R2,=H'1'                                                         
         B     TAPEO3                                                           
*                                                                               
TAPEO5   DS    0H                                                               
         MVI   1(R2),C','    DELIMITER                                          
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(L'PPGEUC2,R2),PPGEUC2                                          
         LA    R2,L'PPGEUC2(R2)                                                 
TAPEO7   CLI   0(R2),C' '                                                       
         BH    TAPEO10                                                          
         SH    R2,=H'1'                                                         
         B     TAPEO7                                                           
*                                                                               
TAPEO10  DS    0H                                                               
         MVI   1(R2),C','    DELIMITER                                          
         LA    R2,2(R2)                                                         
         MVI   0(R2),C','    FOR EMPTY MATERIAL NUMBER                          
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(L'PPGEUC3,R2),PPGEUC3                                          
         LA    R2,L'PPGEUC3(R2)                                                 
TAPE12   CLI   0(R2),C' '                                                       
         BH    TAPEO15                                                          
         SH    R2,=H'1'                                                         
         B     TAPE12                                                           
*                                                                               
TAPEO15  DS    0H                                                               
         MVI   1(R2),C','    DELIMITER                                          
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(L'PPGEUC4,R2),PPGEUC4                                          
         LA    R2,L'PPGEUC4(R2)                                                 
TAPE17   CLI   0(R2),C' '                                                       
         BH    TAPEO20                                                          
         SH    R2,=H'1'                                                         
         B     TAPE17                                                           
*                                                                               
TAPEO20  DS    0H                                                               
         MVI   1(R2),C','    DELIMITER                                          
         LA    R2,2(R2)                                                         
         MVC   0(4,R2),=C'ZLV,'   WITH DELIMITER                                
         LA    R2,4(R2)                                                         
         MVC   0(10,R2),PPGDINV   INV # - NO DASHES                             
         LA    R2,10(R2)                                                        
TAPEO21  CLI   0(R2),C' '                                                       
         BH    TAPEO22                                                          
         SH    R2,=H'1'                                                         
         B     TAPEO21                                                          
*                                                                               
TAPEO22  MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         MVC   0(8,R2),PPGINVD     INVOICE DATE ON BILL                         
         MVI   8(R2),C','          DELIMITER                                    
         LA    R2,9(R2)                                                         
         ZAP   MYDUB,PPGBAMT                                                    
         AP    MYDUB,PPGTAX                                                     
         EDIT  MYDUB,(13,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R2,R0   ADD LENGTH OF OUTPUT                                     
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(4,R2),=C'0.00'     MUST USE IF ZERO                            
         LH    R0,=H'4'                                                         
         CP    PPGTAX,=P'0'                                                     
         BE    TAPEO25                                                          
         EDIT  PPGTAX,(13,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                      
TAPEO25  AR    R2,R0   ADD LENGTH OF OUTPUT                                     
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         MVC   0(4,R2),=C'1,1,'                                                 
         LA    R2,4(R2)                                                         
         EDIT  MYDUB,(13,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                       
**NOFCOM AR    R2,R0   ADD LENGTH OF OUTPUT                                     
**NOFCOM MVI   0(R2),C','                                                       
         CLI   QOPT2,C'Y'          PRINT TRACE ?                                
         BNE   TAPEO40                                                          
         LH    R0,=H'300'                                                       
         GOTO1 =V(PRNTBL),DMCB,0,OUTREC,C'DUMP',(R0),=C'1D'                     
*                                                                               
TAPEO40  LA    R1,PPGTAPE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
TAPEOUTX DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        PROCESS BILL                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRBILRTN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM REQL OR LBOFF ?                  
         BE    PRB2B               YES                                          
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    PRBRTNX                                                          
         CLI   QEND,C' '                                                        
         BE    PRB01                                                            
         CLC   PBILLDAT,QEND                                                    
         BH    PRBRTNX                                                          
*                                                                               
PRB01    DS    0H                                                               
         CLI   SVACTYR,0           SEE IF FILTERING ON YEAR OF SERVICE          
         BE    PRB01C                                                           
         CLC   PBILKMOS(1),SVACTYR MUST MATCH YEAR                              
         BNE   PRBRTNX                                                          
*                                                                               
PRB01C   CLI   PBRETAIL,X'81'      CORP RETAIL?                                 
         BE    PRBRTNX                                                          
         CLC   LASTCPE,PBILLREC+4  NEW ESTIMATE                                 
         BNE   PRB01D                                                           
*        BE    PRB05                                                            
         CLI   QOPT1,C'N'          IF JUST REPORT, SKIP UCOM CHECK              
         BE    PRB07               DID IT FIRST TIME FOR THIS C/P/E             
*                                                                               
PRB01D   MVC   LASTCPE,PBILLREC+4                                               
*                                  NOW CHECK FOR UCOMM RECORDS                  
         MVC   USAVKEY,KEY         SAVE MY KEY                                  
         LA    R3,UCOMBLK          SET-UP DDUCOM CONTROL BLOCK                  
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         XC    UCTTLS,UCTTLS       CLEAR SAVED UCOMM TITLES                     
         XC    UCOMDATA,UCOMDATA   CLEAR SAVED UCOMM DATA                       
         USING DDUCOMD,R3                                                       
*                                                                               
PRB02    MVC   UCACOMF,VCOMFACS    COMFACS                                      
         MVI   UCSYS,C'P'          SYSTEM TO PRINT (PRINT)                      
         MVC   UCAGY,PBILKAGY      AGENCY                                       
         MVC   UCMED,PBILKMED                                                   
         MVC   UCCLT,PBILKCLT      PACKED CLIENT                                
         MVC   UCPRD,PBILKPRD      PRODUCT CODE                                 
         MVC   UCEST,PBILKEST      ESTIMATE CODE                                
         OI    UCOPT,UCOEST                                                     
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   PRB05               ERROR RETURN - JUST EXIT DON'T DIE           
         TM    UCDATA,UCDNOEST     NO EST DATA                                  
         BO    PRB04                                                            
*                                                                               
PRB03    L     RE,UCETTLS          EST TITLES                                   
         MVC   UCTTLS,0(RE)                                                     
         L     RF,UCEDATA          EST DATA                                     
         MVC   UCOMDATA,0(RF)                                                   
PRB04    MVC   KEY,USAVKEY         SINCE SEQ READING MAY BE AFFECTED            
         GOTO1 HIGH                                                             
*                                                                               
PRB05    DS    0H                                                               
*                                                                               
*        FIRST 2 EST UCOMMS MUST BE PRESENT                                     
*                                                                               
         OC    UCOMDATA(32),UCOMDATA   ANY UCOM DATA? 1ST UCOM?                 
         BZ    PRB06                                                            
         OC    UCOMDATA+32(32),UCOMDATA+32    2ND UCOM?                         
         BZ    PRB06                                                            
***      OC    UCOMDATA+64(32),UCOMDATA+64    3RD UCOM?                         
***      BZ    PRB06                                                            
***      OC    UCOMDATA+96(32),UCOMDATA+96    4TH UCOM?                         
***      BNZ   PRB07                                                            
         B     PRB07                                                            
         DROP  R3                                                               
*                                                                               
PRB06    DS    0H                                                               
         MVC   P+1(24),=C'MISSING EST UCOMM DATA -'                             
         MVC   P+26(3),PBILKPRD                                                 
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+31(3),DUB+6(2)                                                 
         BRAS  RE,PRNT                                                          
*                                                                               
***      CLI   QOPT1,C'N'          IF JUST REPORT, DON'T SKIP IT                
***      BE    *+8                                                              
***      B     PRBRTNX             SKIP THIS BILL HEADER                        
*                                                                               
PRB07    DS    0H                                                               
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT6,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    PRBRTNX                                                          
PRB09    DS    0H                                                               
         CLI   QOPT6,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT6,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT6,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT6,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    PRBRTNX                                                          
*                                                                               
PRB09D   DS    0H                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*****                                                                           
*****    SET EFFECTIVE VALUES INTO PBILLREC                                     
*****                                                                           
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                 SET MYGST AND MYPST AND MYHST                 
*****    ZAP   MYGST,=P'0'                                                      
*****    ZAP   MYPST,=P'0'                                                      
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
************                                                                    
******   NOTE USE PPBVEBC FOR CASH DISC                                         
******   DO NOT CALCULATE FROM PBILLGRS AND PBILLBIL (G-CD)                     
************                                                                    
*                                                                               
         CLC   PBILLKEY(14),LSTBLKY  IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,PBILLKEY                                                 
         XC    EATOTS,EATOTS                                                    
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
         LA    R2,PBILLGRS                                                      
         LA    R3,EATOTS                                                        
         LA    R4,4                4 FIELDS STARTING AT GROSS                   
*                                                                               
PRB1F    DS    0H                                                               
         ZAP   DUB,0(BPLEQ,R2)                                                  
         CVB   R0,DUB                                                           
         A     R0,0(R3)                                                         
         ST    R0,0(R3)                                                         
*                                                                               
         LA    R2,BPLEQ(R2)                                                     
         LA    R3,4(R3)                                                         
         BCT   R4,PRB1F                                                         
*                                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   PBRETAIL,0                                                       
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   PBRETAIL,X'81'      IGNORE CORP BILLS                            
         BE    PRBRTNX                                                          
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
PRB2C    CLI   PBILLMOD,C'P'       BYPASS PRD MODE BILLS                        
         BE    PRBRTNX                                                          
         CLI   PBILLMOD,C'S'       AND SERIES MODE                              
         BE    PRBRTNX                                                          
*                                                                               
         CP     PBILLRCV,=P'0'     SKIP ZERO INVOICES FROM FILE                 
         BE     PRBRTNX            AND REPORT                                   
*                                                                               
*                                                                               
*****    CLC   PBILKCLT,LASTKCLT    SEE IF SAME CLIENT                          
*****    BE    PRB2C2                                                           
*                                                                               
*****    MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
*****    SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
*****    L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
*****    A     R1,RELO                                                          
*****    SR    R2,R2                                                            
*****    LA    R3,4                                                             
*****    LA    R4,4                                                             
*****    L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
*****    STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
*****    MVC   WORK(2),PBILKBMN                                                 
*****    MVC   WORK+2(2),PBILKBNO                                               
*****    GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
*****    OC    1(3,R1),1(R1)                                                    
*****    BNZ   *+6                                                              
*****    DC    H'0'                TABLE FULL                                   
*                                                                               
         LA    R3,WK                                                            
         XC    WK,WK                                                            
         USING INVTABD,R3                                                       
         MVC   INVKMED,QMEDIA                                                   
         MVC   INVKCLI,PBILKCLT                                                 
         MVC   INVINVMO,PBILKBMN                                                
         MVC   INVINVN,PBILKBNO                                                 
         MVC   INVIDAT,PBILLDAT       INVOICE DATE                              
         ZAP   INVAMT,PBILLRCV                                                  
         MVC   INVYOS,PBILKMOS        YEAR OF SERVICE                           
         MVC   INVMOS(1),PBILKMOS+1   MONTH OF SERVICE                          
         GOTO1 BINSRCH,INVPARS,(1,INVTABD)                                      
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   0(R1),1             REC INSERTED?                                
         BE    PRB2C2                                                           
         L     R3,0(R1)            R3 -> TO EXISTING REC                        
*                                  WK HAS REC PASSED TO BINSRCH                 
INVD     USING INVTABD,WK          SAME KEY                                     
         AP    INVAMT,INVD.INVAMT  ADD ACTUAL                                   
         LA    R1,INVMOS           MOS LIST                                     
         LHI   R0,12                                                            
PRB2C2C  CLI   0(R1),0                                                          
         BH    *+14                                                             
         MVC   0(1,R1),INVD.INVMOS                                              
         B     PRB2C7                                                           
*                                                                               
         CLC   INVD.INVMOS(1),0(R1)  SAME MOS?                                  
         BE    PRB2C7                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,PRB2C2C                                                       
*                                                                               
         B     PRB2C7                                                           
         DROP  R3                                                               
         DROP  INVD                                                             
*                                                                               
PRB2C2   CLC   PBILKCLT,LASTKCLT   SEE IF SAME CLIENT                           
         BNE   PRB2C4                                                           
         L     R0,CLTINVCN         ADD ANOTHER INVOICE FOR THE CLIENT           
         AHI   R0,1                                                             
         ST    R0,CLTINVCN                                                      
         B     PRB2C7                                                           
PRB2C4   MVC   CLTINVS,CLTINVCN    TOTAL # OF INVOICES FOR THIS CLT             
         XC    CLTINVCN,CLTINVCN                                                
         LHI   R0,1                                                             
         ST    R0,CLTINVCN         1ST INVOICE FOR NEW CLIENT                   
*                                                                               
PRB2C7   LA     R3,WK              POST TO EST TABLE                            
         USING  PPGRECD,R3                                                      
         MVC    PPGKMED,QMEDIA                                                  
         MVC    PPGKCLI,PBILKCLT                                                
         MVC    PPGINVMO,PBILKBMN                                               
         MVC    PPGINVN,PBILKBNO                                                
         MVC    PPGKPRO,PBILKPRD                                                
         MVC    PPGKEST,PBILKEST                                                
         L      RE,ADPRD                                                        
         MVC    PPGKEUC1,UCOMDATA  1ST EST UCOMM PO#                            
         MVC    PPGEUC1,UCOMDATA                                                
         OC     PPGKEUC1,PPGKEUC1  ANYTHING ?                                   
         BNZ    *+8                                                             
         OI     ERRORSW,X'01'                                                   
         MVC    PPGEUC2,UCOMDATA+32                                             
         OC     PPGEUC2,PPGEUC2    2ND EST UCOMM                                
         BNZ    *+8                                                             
         OI     ERRORSW,X'01'                                                   
         MVC    PPGEUC3,UCOMDATA+64                                             
         OC     PPGEUC3,PPGEUC3    3ND EST UCOMM                                
         BNZ    *+8                                                             
         OI     ERRORSW,X'01'                                                   
         MVC    PPGEUC4,UCOMDATA+96                                             
         OC     PPGEUC4,PPGEUC4    4ND EST UCOMM                                
         BNZ    *+8                                                             
         OI     ERRORSW,X'01'                                                   
*                                                                               
         ZAP    PPGBAMT,PBILLRCV                                                
         L      R0,PPBVETAX                                                     
         CVD    R0,MYDUB                                                        
         ZAP    PPGTAX,MYDUB                                                    
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
         L     RF,DMCB             10 CHAR INVOICE                              
         LA    RE,PPGDINV                                                       
         LHI   R0,10                                                            
PRB2C8   CLI   0(RF),C'-'          REMOVE DASHES                                
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PRB2C8                                                        
*                                                                               
         GOTO1 DATCON,DMCB,PBILLDAT,(20,PPGINVD)                                
*                                                                               
         GOTO1  BINSRCH,BINVALS,(1,PPGRECD)                                     
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     PRB2C9                                                          
         L      R3,0(R1)           R3 -> TO EXISTING REC                        
*                                  WK HAS REC PASSED TO BINSRCH                 
PPGD     USING PPGRECD,WK          SAME KEY                                     
         AP    PPGBAMT,PPGD.PPGBAMT                                             
         AP    PPGTAX,PPGD.PPGTAX                                               
         DROP  R3,PPGD                                                          
PRB2C9   CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
*                                                                               
*                                                                               
PRB2C5   CLC   PBILKCLT(6),LASTKCLT CHK FOR SAME      CLT + PRD                 
         BE    PRB3                YES                                          
         CLC   PBILKCLT,LASTKCLT   CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   PBILKPRD,LASTKPRD                                                
         BE    PRB2D                                                            
PRB2A    CLI   LASTKPRD,0                                                       
         BE    PRB2D                                                            
PRB2B    DS    0H                                                               
         MVC   BLPRD,LASTKPRD                                                   
         MVC   BLINE+5(18),=C'**PRODUCT TOTALS**'                               
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
         CLC   PBILKCLT,LASTKCLT   SEE IF NEW CLIENT ALSO                       
         BNE   PRB2D               IF YES THEN DON'T CHK PROFILE                
*                                                                               
         CLI   PROGPROF+1,C'Y'     CHK PAGE FOR PRODUCT                         
         BNE   PRB2D                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWCLT,C'Y'         SO CLIENT WILL REPRINT                       
*                                  FOR EACH PRODUCT                             
*                                                                               
*                                                                               
PRB2D    CLC   PBILKCLT,LASTKCLT                                                
         BE    PRB3                                                             
*                                                                               
         CLI   LASTKCLT,0          SEE IF FIRST TIME                            
         BNE   PRB2F                                                            
         MVC   LASTPROF,PROGPROF   MUST SAVE PROFILE                            
         B     PRB3                                                             
*                                                                               
PRB2F    MVC   BLPRD,LASTKCLT                                                   
         XC    LASTKPRD(25),LASTKPRD                                            
         MVC   BLINE+5(17),=C'**CLIENT TOTALS**'                                
*                                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,CLTINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
*                                                                               
         LA    R3,CLTTOTS                                                       
         MVI   TOTSW,1                                                          
         BRAS  RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
*                                  FIRST CHECK PROFILE OF LAST CLIENT           
         CLI   LASTPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   LASTPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   LASTPROF,PROGPROF                                                
*                                                                               
***OFF                                                                          
         CLI   QCLIENT,C'$'          SEE IF DOING OFFILCE LIST                  
         BNE   PRB2M                 ROLL CLTTOTS TO OFFTOTS                    
         LA    R3,CLTTOTS                                                       
         LA    R4,OFFTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,OFFINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,OFFINVS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,REQINVS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
         BE    PRBRTNX             YES - EXIT - WILL RETURN TO                  
*                                    REQTOTALS OR OFFICE TOTALS                 
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   PSECOND,SVMID2                                                   
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,PBILLKEY                                                 
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,PBILKPRD           PRD                                     
*                                                                               
         LA    R3,PBILKEST                                                      
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         MVC   BLEST(3),WORK+2          EST                                     
*                                                                               
PRB8     DS    0H                                                               
         CLI   PBILLPER,C' '                                                    
         BNH   PRB9                                                             
         CLI   PBILLPER,C'M'      MONTHLY?                                      
         BNE   PRB10                                                            
PRB9     GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB10    DS    0H                  SPECIAL BILL PERIOD                          
         MVC   FULL(2),PBILKMOS                                                 
         MVC   FULL+2(1),PBILLSTA                                               
         GOTO1 DATCON,DMCB,(3,FULL),(7,BLPER)                                   
*                                                                               
         MVI   PSECOND+BLPERD-1,C'-'                                            
         MVC   FULL+2(1),PBILLEND                                               
*        GOTO1 (RF),(R1),,(3,WORK)                                              
         GOTO1 DATCON,(R1),,(5,WORK)                                            
         MVC   PSECOND+BLPERD(5),WORK+3                                         
*                                                                               
PRB12    DS    0H                                                               
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)   SAVE FULL INVOICE NUMBERR                       
*                                                                               
         L     RF,DMCB+4        ADDRESS OF "SHORT" FORMAT                       
         MVC   DSINVNO(7),0(RF)     MAY INCLUDE DASH                            
******   MVC   BLINO(2),0(RF)                                                   
         MVC   BLINO+2(4),3(RF)     DON'T MOVE '-'                              
         L     RF,DMCB+8        ADDRESS OF Y/M                                  
         MVC   BLINO(2),0(RF)                                                   
         MVC   DINVNO(6),BLINO                                                  
*                                                                               
PRB13X   DS    0H                                                               
*            SEE IF RETAIL CORP OR DISTRIBUTOR BILL                             
         CLI   PBRETAIL,X'02'                                                   
         BE    PRB13X2                                                          
         CLI   PBRETAIL,X'01'                                                   
         BNE   PRB13X5                                                          
*                                                                               
PRB13X2  MVC   PSECOND+BLRDATD(12),=C'RETAIL ACCT='                             
         MVC   PSECOND+BLRDATD+12(12),PBRACCT                                   
*                                                                               
PRB13X5  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,BLRDAT)         RUN DATE             
*                                                                               
*        GOTO1 (RF),(R1),(1,PBILINVD),(4,BLBDAT)       BILL DATE                
         GOTO1 DATCON,(R1),(3,PBILINVD),(7,BLBDAT)      BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),PBILLTYP                                                
         LA    R3,BLTYP+2                                                       
         CLI   PBILLTYP,C'0'       SEE IF NEW BILL                              
         BNH   PRB14               YES                                          
*                                                                               
         MVC   BLTYP(3),=C'ORI'                                                 
         LA    R3,BLTYP+3                                                       
         CLI   PBILLTYP,C'3'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'DET'                                                 
         CLI   PBILLTYP,C'4'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'MAN'                                                 
PRB14    DS    0H                                                               
         MVI   0(R3),C'-'                                                       
*                                                                               
         TM    PBILCMSW,X'02'          FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        SEE IF ALSO AOR                            
         BO    PRB14D                                                           
         MVC   1(3,R3),=C'COM'                                                  
*                                                                               
PRB14D   CLI   QOPT6,C'C'          TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   PBILLNET,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
         MVC   1(3,R3),=C'ADJ'                                                  
         CLI   PBILSEP,C'A'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'CD '                                                  
         CLI   PBILSEP,C'C'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        AOR BILL                                   
         BO    PRB16                                                            
         MVC   1(3,R3),=C'UFC'                                                  
         TM    PBILCMSW,X'01'         UP FRONT COMMISSION                       
         BO    PRB16                                                            
         MVC   1(3,R3),=C'NET'                                                  
         TM    PBILCMSW,X'08'         UP FRONT COMMISSION - NET                 
         BO    PRB16                                                            
*                                                                               
         MVC   1(3,R3),=C'REG'                                                  
*                                                                               
PRB16    DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
*                                                                               
**NEW 3/17/89                                                                   
         CLI   QOPT6,C'A'          SEE IF AOR ONLY                              
         BE    PRB16A              YES - THEN INCLUDE IN TOTALS                 
         TM    PBILCMSW,X'20'      SEE IF AOR BILL                              
         BZ    PRB16B                                                           
PRB16A   ZAP   BILTOTS(6),=P'0'                                                 
         ZAP   BILTOTS+6(6),=P'0'                                               
         ZAP   BILTOTS+12(6),=P'0'                                              
         ZAP   BILTOTS+18(6),PBILLRCV    SET AC TO RCVBL                        
         ZAP   BILTOTS+24(6),PBILLRCV    ACTUAL                                 
         AP    BILTOTS+24(6),MYGST                                              
         AP    BILTOTS+24(6),MYPST                                              
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
         B     PRB16E                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),PBILLNET     NET                          
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'                                     
         SP    BILTOTS+2*BPLEQ(BPLEQ),PBILLRCV     CD IS -(RCVBL)               
*                                                                               
         CLI   PBILSEP,C'C'        FOR SEP CD BILL                              
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'        CD IS 0                      
*                                                                               
         CLI   PBILCDSW,C'S'       IF SEP (ON OTHER BILL) BILL                  
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),PPBVEBC      EFFECTIVE CD                 
*                                                                               
PRB16C   ZAP   BILTOTS+4*BPLEQ(BPLEQ),PBILLRCV     RCVBL                        
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYGST        ADD GST TO BILL AMT          
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYPST        ADD PST TO BILL AMT          
         ZAP   BILTOTS+5*BPLEQ(BPLEQ),MYGST        AND GST TOTALS               
         ZAP   BILTOTS+6*BPLEQ(BPLEQ),MYPST        AND PST TOTALS               
*                                                                               
         ZAP   DUB,PBILLRCV                                                     
         TM    PBILCMSW,X'02'      UNLESS COMMISSION ONLY BILL                  
         BNZ   PRB16D                                                           
         TM    PBILBASA,X'04'                      IS CD ALREADY OUT?           
         BNZ   PRB16D                              YES                          
         SP    DUB,BILTOTS+2*BPLEQ(BPLEQ)          NO, TAKE IT OUT NOW          
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),DUB          FOR AC CALC                  
         SP    BILTOTS+3*BPLEQ(BPLEQ),PBILLNET     -NET = TRUE AC               
*                                                                               
PRB16E   LA    R3,BILTOTS                                                       
         BRAS  RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BRAS  RE,PRNT                                                          
*&&DO                                                                           
         CLI   QOPT1,C'F'               FORMULA OPTION                          
         BNE   PRB22                    NO                                      
         OC    PBILBASA(5),PBILBASA                                             
         BZ    PRB22                    NO FORMULA                              
         CLI   PBILBASA,5          DONT PRINT IF G-CD +0 PCT                    
         BNE   *+14                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB22                                                            
         LA    R3,BLINO                                                         
         MVC   0(8,R3),=C'FORMULA='                                             
         SR    RF,RF                                                            
         IC    RF,PBILBASA                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         LA    R3,11(R3)                                                        
         CLI   1(RF),C' '                                                       
         BE    *+8                                                              
         LA    R3,3(R3)                                                         
         MVI   1(R3),C'0'                                                       
         LA    R0,1                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB18                                                            
         EDIT  (B3,PBILADJ),(8,1(R3)),5,ALIGN=LEFT                              
         LR    R4,R3                                                            
         AR    R4,R0               POINT TO END                                 
PRB17    DS    0H                                                               
         LR    R5,R4                                                            
         CLI   0(R4),C'0'                                                       
         BE    PRB17D                                                           
         CLI   0(R4),C'.'                                                       
         BE    PRB17B                                                           
         B     PRB18                                                            
PRB17B   LA    R4,1                END SCAN ON .                                
PRB17D   BCTR  R0,R0               SHORTEN LENGTH                               
         MVI   0(R5),C' '                                                       
PRB17F   BCT   R4,PRB17                                                         
*                                                                               
PRB18    DS    0H                                                               
         MVI   0(R3),C'+'                                                       
         TM    PBILADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         AR    R3,R0                                                            
         MVC   2(6,R3),=C'PCT OF'                                               
         SR    RF,RF                                                            
         IC    RF,PBILBASB                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
PRB22    DS    0H                                                               
         CLI   QOPT2,C'R'               REVERSALS OPTION                        
         BNE   PRB26                    NO                                      
         CLI   PBILLCDT,C'0'                                                    
         BE    PRB26                    NOT REVERSED                            
         LA    R3,BLINO                                                         
         MVC   0(11,R3),=C'REVERSED BY'                                         
         MVC   12(4,R3),PBILLCAN+2                                              
         MVC   17(2,R3),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(5,20(R3))                              
*                                                                               
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*&&                                                                             
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         MVI   RACTSW,1            SET REQUEST ACTIVITY                         
         MVI   OACTSW,1            SET REQUEST ACTIVITY                         
*                                                                               
PRB28    DS    0H                                                               
PRBRTNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 3                                                                
ADJWRDS  DS    0C                                                               
         DC    C'G   '                                                          
         DC    C'N   '                                                          
         DC    C'    '                                                          
         DC    C'    '                                                          
         DC    C'G-CD'                                                          
         DC    C'N-CD'                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        FORMAT $ AMTS                                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
FMTAMTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   STAGSW,0                                                         
         CP    0(6,R3),=P'9999999999'   COMPARE GROSS TO 100 MILLION            
         BL    *+8                                                              
         MVI   STAGSW,1            SET TO STAGGER TOTALS                        
*                                                                               
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         LH    R1,=H'-1'          USED WHEN STAGGERING                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         LA    R1,131              NEXT LINE -1                                 
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         LH    R1,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         LA    R1,131              NEXT LINE-1                                  
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         LH    R1,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   FMTAX                                                            
*                                                                               
         LA    R5,30(R3)           GST (PAST IS AT 36(R3))                      
         LA    R4,BLGST                                                         
         BAS   RE,FMTEDT8       PRINT PST UNDER GST                             
*                                                                               
FMTAX    MVI   TOTSW,0                                                          
*SMY*    B     EXIT                                                             
         XIT1                                                                   
         SPACE 2                                                                
FMTEDT   DS    0H                                                               
         CLI   STAGSW,1       SEE IF STAGGERING                                 
         BNE   FMTEDT5                                                          
         AR    R4,R1          ADJUST R4 FOR STAGGERING                          
         EDIT  (P6,0(R5)),(16,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   14(R4),C'C'                                                      
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   15(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
FMTEDT5  EDIT  (P6,0(R5)),(15,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   13(R4),C'C'                                                      
         BER   RE                                                               
         MVI   13(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
FMTEDT8  DS    0H               SPECIAL EDT FOR GST AND PST                     
         EDIT  (P6,0(R5)),(14,0(R4)),2,COMMAS=YES,CR=YES                        
         EDIT  (P6,6(R5)),(14,132(R4)),2,COMMAS=YES,CR=YES                      
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   12(R4),C'C'                                                      
         BE    FMTEDT8C                                                         
         MVI   12(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BE    FMTEDT8C                                                         
         MVI   13(R4),C'*'                                                      
*                                                                               
FMTEDT8C DS    0H                                                               
         CLI   12+132(R4),C'C'                                                  
         BER   RE                                                               
         MVI   12+132(R4),C'*'                                                  
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   13+132(R4),C'*'                                                  
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      PRINT ROUTINE                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRNT     NTR1  BASE=*,LABEL=*                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,0                                                       
         CLI   PAGYNAT,C'C'           SEE IF CANADIAN                           
         BNE   *+8                                                              
         MVI   RCSUBPRG,50                                                      
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNT2                                                            
         ZIC   R1,LINE                                                          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         STC   R1,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    PRNT90                                                           
PRNT2    DS    0H                                                               
         CLI   MODE,LBUYREQ          REQUEST TOTALS                             
         BNL   PRNT90                                                           
         CLI   QCLIENT,C'$'          SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT3                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),RCSVOFC                                               
         GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,HEAD5+7                              
         B     PRNT4                                                            
PRNT3    CLI   QCLIENT,C'*'                                                     
         BNE   PRNT4                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),QCLIENT+1                                             
         GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD5+7                            
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT6,C'A'         TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT6,C'B'         AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT6,C'X'         NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT6,C'C'         COMMISSION ONLY BILLS                         
         BNE   PRNT7                                                            
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
PRNT7    DS    0H                                                               
PRNT90   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
PRNTXIT  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      ROLTOTS                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
ROLTOTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,7           FOR BCT                                           
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
ROLTOTSX DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      CLRTOTS                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
CLRTOTS  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,7                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
CLRTOTSX DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         TITLE 'DSECTS AND WORK AREAS'                                          
PPGMWRKD DSECT                                                                  
PPGMWRK  DS    0C                                                               
VDOWNLD  DS    A                                                                
VPPBVAL  DS    A                                                                
VPFMTINO DS    A                                                                
VOFFOUT  DS    A                                                                
VDDUCOM  DS    A                                                                
RELO     DS    A                                                                
ADCLT    DS    A                                                                
ADEST    DS    A                                                                
AAAEST   DS    A           AAA ESTIMATE                                         
ADPRD    DS    A                                                                
ERRORSW  DS    XL1                                                              
COLSW    DS    CL1                                                              
DOWNACT  DS    CL1                                                              
SVLINE   DS    XL1                                                              
SVFORCEH DS    CL1                                                              
SVPPGKEY DS    CL31                                                             
LASTCPE  DS    CL8         LAST CLT/PRD/EST                                     
SVMOS    DS    CL4         SAVED MOS - THEIR FORMAT (MMYY)                      
SVEST    DS    CL3         SAVED ESTIMATE   (NNN)                               
START    DS    CL6                                                              
END      DS    CL6                                                              
SVACTYR  DS    XL1          YEAR OF SERVICE FILTER (FROM QPUB+1(2))             
TAXSW    DS    XL1                                                              
SEQNUM   DS    CL1                                                              
SAVVEND  DS    CL09         AGY VENDOR CODE                                     
SAVFILE  DS    CL13         BDE FILE NAME                                       
SAVTERM  DS    CL4          PAYMENT TERMS                                       
MYPACK   DS    PL6          USED WHEN REPORTING GST/HST/PST                     
MYPPOS   DS    PL6          POSITIVE OF MYPACK                                  
         DS    0D                                                               
SVBILL   DS    PL6          SAVED BILTOTS+24                                    
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
*                                                                               
FIRSTSW  DS    X                                                                
REVSW    DS    CL1                 REVISION STATUS                              
RETAIL   DS    CL1                 'Y' IF RETAIL BILL                           
OACTSW   DS    CL1         SET TO 1 IF BILL FOR OFFICE PROCESSED                
RACTSW   DS    CL1         SET TO 1 IF BILL FOR REQUEST PROCESSED               
LSTBLKY  DS    XL25                                                             
         DS    0F                                                               
EATOTS   DS    XL16                                                             
TAPTYP   DS    C                                                                
LASTKEY  DS    0CL32                                                            
         DS    CL4                                                              
LASTKCLT DS    CL3                                                              
LASTKPRD DS    CL3                                                              
         DS    CL22                                                             
*                                                                               
LASTPROF DS    XL16                                                             
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                 FULL INVOICE NUMBER                         
*                                   ME-MN-NNNN                                  
*                               OR  MN-ME-NNNN                                  
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
BILTOTS  DS    7PL6                                                             
PRDTOTS  DS    7PL6                                                             
CLTTOTS  DS    7PL6                                                             
OFFTOTS  DS    7PL6                                                             
REQTOTS  DS    7PL6                                                             
RUNTOTS  DS    7PL6                                                             
TRUNTOTS DS    7PL6                FOR TAPE REQS                                
*                                                                               
*                                                                               
INVPARS  DS    F                FOR INVTAB BINSRCH                              
AINVTAB  DS    F                                                                
INVTABN  DS    F                                                                
INVTABL  DS    F                                                                
         DS    2F                                                               
*                                                                               
INVMAX   EQU   50000               MAX INVOICES PER CLT                         
*                                                                               
*                                                                               
BINVALS  DS    F                   PPGTAB PARS                                  
ABINTAB  DS    F                                                                
BINTABN  DS    F                                                                
BINTABL  DS    F                                                                
         DS    2F                                                               
*                                                                               
BINMAX   EQU   4000                MAX # OF RECS                                
*                                                                               
CLTINVS  DS    F                   CLIENT INVOICES TOTAL                        
CLTINVCN DS    F                   CLIENT INVOICES COUNTER                      
OFFINVS  DS    F                                                                
REQINVS  DS    F                                                                
RUNINVS  DS    F                                                                
TRUNINVS DS    F                                                                
*                                                                               
STAGSW   DS    X                                                                
TOTSW    DS    X                                                                
SAVMAX   DS    X                                                                
SVCD     DS    PL8                                                              
*                                                                               
NEWCLT   DS    CL1                                                              
ELCODE   DS    CL1                                                              
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
*                                                                               
WK       DS    CL64                                                             
WK2      DS    CL255                                                            
ENDRPTSW DS    XL1                 E=IN PRBILRTN FROM LBLR OR LBOFF             
ALLOWSW  DS    CL1                                                              
*                                                                               
       ++INCLUDE PPBVALD           NEW PPBVAL DSECT                             
*                                                                               
PBIREC   DS    CL256                                                            
         DS    CL34                TO ALLOW FOR 300 BYTE RECORDS                
         DS    CL114                                                            
         DS    F                                                                
OUTREC   DS    CL500                                                            
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
UCOMQ    EQU   *-UCOMBLK                                                        
USAVKEY  DS    XL32                                                             
*                                                                               
BILLINED DSECT                                                                  
BLINE    DS    0C                                                               
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINO    DS    CL6                                                              
         DS    CL1                                                              
BLRDAT   DS    CL8                                                              
         DS    CL1                                                              
BLBDAT   DS    CL5                                                              
         DS    CL1                                                              
BLTYP    DS    CL6                                                              
BLGRS    DS    CL15                                                             
BLNET    DS    CL15                                                             
BLCD     DS    CL15                                                             
BLAC     DS    CL15                                                             
BLBIL    DS    CL15                                                             
BLGST    DS    CL13                                                             
         SPACE 3                                                                
BLESTD   EQU   BLEST-BLINE                                                      
BLPERD   EQU   BLPER-BLINE                                                      
BLRDATD  EQU   BLRDAT-BLINE                                                     
*                                                                               
*                                                                               
HRECD    DSECT                     HEADER RECORD                                
HRTYP    DS    CL1                 H                                            
*RSAP    DS    CL10                1000000008    OLD PLANWORKS CODE             
HRSAP    DS    CL10                1000000039                                   
HRINVNUM DS    CL9                 INVOICE NUMBER                               
HRINVDAT DS    CL8                 CCYYMMDD                                     
HRDOCTYP DS    CL1                 I OR C                                       
HRINAME  DS    CL40                YEAR/CLIENT/PRODUCT                          
HRCOMM   DS    CL255               EST/MOS/PROD(S)                              
HRECDLQ  EQU   *-HRTYP                                                          
*                                                                               
IRECD    DSECT                     ITEM RECORD                                  
IRTYP    DS    CL1                 I                                            
IRSEQ    DS    CL4                 SEQUENCE NUMBER                              
IRUCOM1  DS    CL10                UCOM1                                        
IRUCOM2  DS    CL5                 UCOM2                                        
IRAMT    DS    CL13                ACTUAL $                                     
IRPMT    DS    CL4                 PAYMENT TERMS                                
IRCOMM   DS    0CL160              DDS COMMENTS                                 
IRMED    DS    CL10                MEDIA                                        
         DS    CL1                 /                                            
IRCLT    DS    CL3                 CLIENT                                       
         DS    CL1                 /                                            
IRPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                 /                                            
IREST    DS    CL3                 ESTIMATE                                     
         DS    CL138               SPARE                                        
IRECDLQ  EQU   *-IRTYP                                                          
*                                                                               
*                                                                               
PPGRECD  DSECT                                                                  
PPGKEY   DS    0XL33                                                            
PPGKMED  DS    CL1                                                              
PPGKCLI  DS    CL3                                                              
PPGKPRO  DS    CL3                                                              
PPGKEST  DS    XL2                                                              
PPGKEUC1 DS    CL20          PO# EST 1ST UCOMM                                  
PPGKNUMB DS    0XL4                                                             
PPGINVMO DS    XL2           INVOICE MONTH -- YM  (NOT MOS)                     
PPGINVN  DS    XL2           INVOICE NUMBER                                     
*            ______                                                             
*              33                                                               
PPGKLEN  EQU   *-PPGKEY                                                         
PPGDINV  DS    CL10          INVOICE# NO DASHES                                 
PPGINVD  DS    CL8           INVOICE DATE ON BILL YYYYMMDD                      
PPGEUC1  DS    CL20          EST UCOM 1  AGREEMENT #                            
PPGEUC2  DS    CL10          EST UCOM 2  PLD LINE ITEM #                        
PPGEUC3  DS    CL10          EST UCOM 3  PO SERVICE LINE                        
PPGEUC4  DS    CL32          EST UCOM 4  PO ITEM SHORT TEXT                     
PPGBAMT  DS    PL6           ACTUAL                                             
PPGTAX   DS    PL6           TAX                                                
PPGTLEN  EQU   *-PPGKEY                                                         
*                                                                               
INVTABD  DSECT                                                                  
INVKMED  DS    CL1                                                              
INVKCLI  DS    CL3                                                              
INVKNUMB DS    0XL4                                                             
INVINVMO DS    XL2           INVOICE MONTH                                      
INVINVN  DS    XL2           INVOICE NUMBER                                     
INVKLEN  EQU   *-INVKMED                                                        
INVIDAT  DS    CL6           BQDATE                                             
INVAMT   DS    PL6           TOTAL AMOUNT FOR INVOICE                           
INVYOS   DS    XL1           YEAR OF SERVICE                                    
INVMOS   DS    CL12          INVOICE MONTHS OF SERVICE                          
INVTLEN  EQU   *-INVKMED                                                        
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*INVTLEN)                                               
         DC    X'00'                                                            
*                                                                               
PPGTAB   CSECT                                                                  
         ORG   *+(4000*PPGTLEN)                                                 
*                                                                               
COLHDS   CSECT                                                                  
         DC     C'Record Type,'                                                 
         DC     C'GM Vendor Number,'                                            
         DC     C'PO/Invoice Currency,'                                         
         DC     C'Agreement Number,'                                            
         DC     C'PLD Line Item Number,'                                        
         DC     C'Material Number,'                                             
         DC     C'PO Service Line,'                                             
         DC     C'PO Item Short Text,'                                          
         DC     C'PO Item UOM,'                                                 
         DC     C'Vendor''s Invoice Number,'                                    
         DC     C'Invoice Date,'                                                
         DC     C'Invoice Amount,'                                              
         DC     C'Total Tax Amount,'                                            
         DC     C'Invoice Item,'                                                
         DC     C'Invoice Item Quantity,'                                       
         DC     C'Invoice Item Amount'                                          
COLHDX   EQU    *                                                               
COLHLEN  EQU    *-COLHDS                                                        
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
BPLEQ    EQU   6                   LENGTH OF PBILLREC PACKS (PL6)               
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PAORREC                                                        
*                                                                               
       ++INCLUDE DDUCOMD                                                        
*                                                                               
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(4000*PPGTLEN)                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045PPREPGM02 07/06/18'                                      
         END                                                                    
