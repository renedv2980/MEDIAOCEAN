*          DATA SET SPREPGM02  AT LEVEL 098 AS OF 02/27/15                      
*PHASE SPGM02A                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE GETUSER                                                                
*                                                                               
         TITLE 'SPGM02 - CHANGE LOG'                                            
*                                                                               
*  BPLA  11/14  NEW FORMAT - OLD SOURCE IS PPREPGM02O                           
*               COMMA DELIMITED,COLLAPSED BY PO#                                
*                                                                               
         TITLE 'SPGM02 - SPOTPAK/NETPAK  GM  INTERFACE'                         
*                                                                               
SPGM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPGM02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    R8,SPACEND                                                       
         USING SPGMWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,PROCBILL                                                    
         BE    PRBIL                                                            
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,CLTFRST        FRIST FOR CLIENT                             
         BE    CLTF                                                             
         CLI   MODE,CLTLAST       LAST FOR CLIENT                               
         BE    CLTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
RUNF     DS    0H                  RUN FIRST                                    
         MVI   ERRORSW,0                                                        
         MVI   ALLOWSW,0           DYNAMIC ALLOCATION                           
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
*                                                                               
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
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
*                                                                               
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   REQF10                                                           
         MVI   FIRSTSW,1                                                        
*                                                                               
         MVI   ENDRPTSW,0         CLEAR SWITCH USED IN END PROCESSES            
*                                                                               
         XC    START(12),START                                                  
         XC    LSTBLKY,LSTBLKY                                                  
         MVC   SAVMAX,MAXLINES                                                  
         XC    LASTKCLT,LASTKCLT                                                
         XC    LASTKPRD,LASTKPRD                                                
         XC    LASTCLI,LASTCLI                                                  
         XC    UCTTLS,UCTTLS                                                    
         XC    UCOMDATA,UCOMDATA                                                
*                                                                               
REQF10   DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
         CLI   QOPT1,C'N'          SKIP TAPE                                    
         BE    REQF30                                                           
*                                                                               
         TM    ALLOWSW,X'01'       DON'T ALLOCATE MORE THEN ONCE                
         BO    REQF30                                                           
         LA    R3,NEDYNDSN                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+8                                                              
         LA    R3,SPDYNDSN                                                      
*                                                                               
         MVC   13(2,R3),QAGY                                                    
*                                                                               
* PREVENT DYNALLOC FAILURES BY SERIALIZING ON THE DSN                           
*                                                                               
         ENQ   (MAJORNAM,(3),E,DSNLENQ,SYSTEM)                                  
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'SBITAPE '),(0,0(R3))                         
         OPEN  (SBITAPE,OUTPUT)                                                 
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
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         XC    LASTCLI,LASTCLI                                                  
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
         MVC   SVQST(12),QSTART                                                 
*Y2K*                                                                           
         MVC   QSTART,=C'700101'   SET LONG RANGE START                         
         MVI   QEND,X'FF'            AND END FOR SPONSOR                        
         MVC   QEND+1(5),=C'91231'                                              
*                                                                               
         CLC   Q2USER(2),SPACES                                                 
         BNH   EXIT                                                             
         MVI   SVACTYR,X'00'                                                    
         MVC   WORK,=C'010101'                                                  
         MVC   WORK(2),Q2USER                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   SVACTYR,WORK+6                                                   
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
SPDYNDSN DC    CL(DSNLENQ)'SPTTAPE.SP0GMAG2'                                    
NEDYNDSN DC    CL(DSNLENQ)'NETTAPE.NE0GMAG2'                                    
*                                                                               
*        NOTE SUFFIX 2 FOR NEW FORMAT                                           
*                                                                               
DSNLENQ  EQU   20                                                               
MAJORNAM DC    C'SBTTAPE '                                                      
*                                                                               
SBITAPE  DCB   DDNAME=SBITAPE,DSORG=PS,RECFM=FB,LRECL=00300,           X        
               BLKSIZE=00300,MACRF=PM                                           
         EJECT                                                                  
*                                                                               
CLTF     DS    0H                  FIRST FOR CLIENT                             
         XC    LASTCPE,LASTCPE                                                  
         XC    BINTABN,BINTABN      MUST CLEAR FOR BINSRCH                      
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         CLI   COFFICE,C' '                                                     
         BNH   CLTF10                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
CLTF10   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         CLI   QCLT,C'*'          ONE OFFICE REQUEST ?                          
         BE    CLTF20              YES                                          
         CLI   QCLT,C'$'          ALL OFFICE REQUEST ?                          
         BE    CLTF20              YES                                          
         CLC   QCLT,=C'ALL'       ALL CLIENTS REQUEST ?                         
         BNE   CLTF30              NO - ONE CLIENT ONLY REQUEST                 
*                                                                               
CLTF20   DS    0H                                                               
*                                                                               
CLTF30   MVI   NEWCLT,C'Y'                                                      
         CLI   PROGPROF+0,C'Y'     NEW PAGE PER CLIENT?                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  SET CLIENT DATA IN SVMID                     
         MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),CLT                                                   
         MVC   SVMID+12(24),CLTNM                                               
*                                                                               
         LA    RF,SVMID+36                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CCLTIFC-CLTHDR(RE),C' '                                          
         BNH   CLTFX                                                            
         MVI   2(RF),C'('                                                       
         MVC   3(8,RF),CCLTIFC-CLTHDR(RE)                                       
         LA    RF,11(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
CLTFX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CLTL     DS    0H                  LAST FOR CLIENT                              
*                                  SEND ENTRIES TO DOWNLOAD                     
         L     R2,BINTABN          FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
         CLI   QOPT1,C'N'          DON'T CREATE TAPE?                           
         BE    ZEROCNT                                                          
*                                                                               
         L     R3,=A(PPGTAB)                                                    
         XC    SVPPGKEY,SVPPGKEY                                                
CLTL15   BRAS  RE,TAPEOUT                                                       
         LA    R3,PPGTLEN(R3) TO NEXT RECORD IN TABLE                           
         BCT   R2,CLTL15                                                        
*                                                                               
ZEROCNT  B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
REQL     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLT,C'$'                                                        
         BE    REQL20                                                           
*                                                                               
         MVC   CLTINVS,CLTINVCN    LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BE    REQL20              NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM REQL OR LBOFF          
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
*?*      CLI   QOPT4,C'N'                                                       
*?*      BE    REQL40                                                           
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
*****    CLI   ERRORSW,0        ANY ERRORS?                                     
*****    BE    RUNL10              NO                                           
*?*      TM    ERRORSW,X'01'     ESTIMATE USER1 INVALID ?                       
*?*      BNO   RUNL3                                                            
*?*      MVC   P+3(32),=C'*** ESTIMATE USER1 INCORRECT ***'                     
*?*      BRAS  RE,PRNT                                                          
*?*L3    TM    ERRORSW,X'02'     REGION "INVALID" ?                             
*?*      BNO   RUNL4                                                            
*?*      MVC   P+3(33),=C'*** REGION NOT A VALID NUMBER ***'                    
*?*      BRAS  RE,PRNT                                                          
*                                                                               
*UNL8    DS    0H                                                               
*        MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
*        BRAS  RE,PRNT                                                          
*                                                                               
*                       CLOSE DOWNLOAD FILE HERE                                
RUNL10   DS    0H                                                               
*                                                                               
         CLOSE (SBITAPE)                                                        
*?*      CLI   DOWNACT,C'Y'       DOWNLOADING ACTIVITY?                         
*?*      BNE   EXIT                                                             
*?*      GOTO1 VDOWNLD,DMCB,(RA)                                                
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
         MVC   BLPRD+1(1),SAVCOFF                                               
*******  GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,BLPRD+1                              
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
         XC    LASTCLI,LASTCLI                                                  
         MVI   OACTSW,0                                                         
         LA    R3,OFFTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         L     RF,ADCLT                                                         
         MVC   SAVCOFF,COFFICE-CLTHDR(RF)                                       
         B     EXIT                                                             
***OFF                                                                          
         SPACE 2                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      TAPEOUT                                        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
TAPEOUT  NTR1                                                                   
*                                                                               
         LA    RE,OUTREC           INIT TO SPACES                               
         LHI   R1,L'OUTREC-1                                                    
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
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
         GOTO1 PRNTBL,DMCB,0,OUTREC,C'DUMP',(R0),=C'1D'                         
*                                                                               
TAPEO1   LA    R1,SBITAPE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         MVI   COLSW,C'Y'                                                       
*                                                                               
TAPEO2   LA    RE,OUTREC           INIT TO SPACES                               
         LHI   R1,L'OUTREC-1                                                    
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         USING PPGRECD,R3                                                       
*                                                                               
         CP    PPGBAMT,=P'0'    INVOICE TOTAL ZERO - THEN SKIP                  
         BE    TAPEOUTX                                                         
*                                                                               
         LA    R2,OUTREC                                                        
         MVI   0(R2),C'1'                                                       
         CP    PPGBAMT,=P'0'                                                    
         BNL   *+8                                                              
*                                                                               
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
TAPEO10  DS    0H                                                               
         MVI   1(R2),C','    DELIMITER                                          
         LA    R2,2(R2)                                                         
         MVI   0(R2),C','    EMPTY FIELD (MATERIAL NUMBER)                      
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
         MVC   0(4,R2),=C'0.00'                                                 
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
**NOCOM  AR    R2,R0   ADD LENGTH OF OUTPUT                                     
**NOCOM  MVI   0(R2),C','                                                       
*                                                                               
         CLI   QOPT2,C'Y'          PRINT TRACE ?                                
         BNE   TAPEO45                                                          
         LH    R0,=H'300'                                                       
         GOTO1 PRNTBL,DMCB,0,OUTREC,C'DUMP',(R0),=C'1D'                         
*                                                                               
TAPEO45  LA    R1,SBITAPE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
TAPEOUTX DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
PTNETTB  DS    0CL4                                                             
         DC    C'ABC '                                                          
         DC    C'NBC '                                                          
         DC    C'CBS '                                                          
         DC    C'FOX '                                                          
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        PROCESS BILL                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRBILRTN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
         CLI   ENDRPTSW,C'E'       COMING FROM REQL OR LBOFF ?                  
         BE    PRB2B               YES                                          
*                                                                               
         TM    BILSTAT3,BSTLMGQ    DO NOT PROCESS LMG BILLS                     
         BO    PRBRTNX                                                          
*                                                                               
         CLC   BDATE,SVQST         DATES FILTER                                 
         BL    PRBRTNX                                                          
         CLI   QEND,C' '                                                        
         BE    PRB01                                                            
         CLC   BDATE,SVQEND                                                     
         BH    PRBRTNX                                                          
*                                                                               
         CLI   SVACTYR,X'00'                                                    
         BNH   PRB01                                                            
         CLC   SVACTYR,BKEYYSRV                                                 
         BNE   PRBRTNX                                                          
*                                                                               
PRB01    DS    0H                                                               
         CLI   BRETAIL,X'81'       CORP RETAIL?                                 
         BE    PRBRTNX                                                          
*                                                                               
****     CLC   LASTCPE(5),BILLREC+2  NEW PRODUCT                                
****     BE    PRB02                                                            
PRB02    CLC   LASTCPE,BILLREC+2  NEW ESTIMATE                                  
*        BE    PRB05                                                            
         BNE   PRB02B                                                           
         CLI   QOPT1,C'N'          IF JUST REPORT, SKIP UCOM CHECK              
         BE    PRB07               DID IT FIRST TIME FOR THIS C/P/E             
*                                                                               
PRB02B   MVC   LASTCPE,BILLREC+2    SAVE CLT/PRD/EST                            
*                                                                               
*        CLI   QOPT1,C'N'          DON'T CHECK FOR UCOM IF NO TAPE              
*        BE    PRB07                                                            
*                                  NOW CHECK FOR UCOMM RECORDS                  
         MVC   USAVKEY,KEY         SAVE MY KEY                                  
         LA    R3,UCOMBLK          SET-UP DDUCOM CONTROL BLOCK                  
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
*                                                                               
         MVC   UCPRD,BKEYPRD       PRODUCT CODE                                 
*                                                                               
PRB02C   MVC   UCACOMF,ACOMFACS    COMFACS                                      
         MVI   UCSYS,C'N'          SYSTEM TO PRINT (NET)                        
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+8                                                              
         MVI   UCSYS,C'S'          SYSTEM TO PRINT (SPOT)                       
         MVC   UCSAM,BKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,BKEYCLT      PACKED CLIENT                                
         MVC   UCSEST,BKEYEST      BINARY ESTIMATE NUMBER                       
         OI    UCOPT,UCOEST        RETURN ESTIMATE UCOMMS                       
         XC    UCTTLS,UCTTLS                                                    
         XC    UCOMDATA,UCOMDATA                                                
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   PRB04               ERROR RETURN - JUST EXIT DON'T DIE           
         TM    UCDATA,UCDNOEST     NO ESTIMATE DATA ?                           
*        BO    PRB04                                                            
         BNO   PRB03                                                            
         CLC   =C'POL',UCPRD       DID WE READ FOR POL YET?                     
         BNE   *+14                                                             
         XC    UCOMDATA,UCOMDATA                                                
         B     PRB04               YES, DONE                                    
*                                                                               
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         MVC   UCPRD,=C'POL'                                                    
         B     PRB02C                                                           
*                                                                               
PRB03    L     RE,UCETTLS          ESTIMATE TITLES                              
         MVC   UCTTLS,0(RE)                                                     
         L     RF,UCEDATA          ESTIMATE DATA                                
         MVC   UCOMDATA,0(RF)                                                   
PRB04    MVC   KEY,USAVKEY         SINCE SEQ READING MAY BE AFFECTED            
         GOTO1 HIGH                                                             
         DROP  R3                                                               
*                                                                               
PRB05    DS    0H                                                               
         OC    UCOMDATA(32),UCOMDATA   ANY UCOM DATA? 1ST UCOM?                 
         BZ    *+14                                                             
         OC    UCOMDATA+32(32),UCOMDATA+32    2ND UCOM?                         
         BNZ   PRB07                                                            
*                                                                               
*                                                                               
PRB07    TM    BILSTAT,X'02'       IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT5+1,C'C'        NO, ARE WE SKIPPING OTHERS                   
         BE    PRBRTNX                                                          
PRB09    DS    0H                                                               
         CLI   QOPT5+1,C' '          CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT5+1,C'A'          ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    BILSTAT,X'20'                                                    
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT5+1,C'B'          AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    BILSTAT,X'30'                                                    
         BNO   PRBRTNX                                                          
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT5+1,C'X'          SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    BILSTAT,X'20'                                                    
         BO    PRBRTNX                                                          
*                                                                               
PRB09D   DS    0H                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD                               
*                                 SET MYGST AND MYPST AND MYHST                 
         L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
         CLC   BKEY(10),LSTBLKY      IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,BKEY                                                     
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
*                                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   BRETAIL,0                                                        
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   BRETAIL,X'81'       IGNORE CORP BILLS                            
         BE    PRBRTNX                                                          
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
******   MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
******   SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
******   L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
******   SR    R2,R2                                                            
******   LA    R3,4                                                             
******   LA    R4,4                                                             
******   L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
******   STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
PRB2C    DS    0H                                                               
*                                                                               
         CP     BACTP,=P'0'     SKIP ZERO INVOICES FROM FILE                    
         BE     PRBRTNX          AND REPORT                                     
*                                                                               
         XC    WK,WK                                                            
         LA    R3,WK                                                            
         USING INVTABD,R3                                                       
         MVC   INVKMED,QMED                                                     
         MVC   INVKCLI,BKEYCLT                                                  
         MVC   INVINVMO,BKEYMBIL                                                
         NI    INVINVMO,X'0F'         GET RID OF YEAR, LEAVE MONTH              
         MVC   INVINVN,BKEYINV                                                  
         MVC   INVIDAT,BDATE          RUN DATE                                  
         ZAP   INVAMT,SPBVACTP                                                  
         MVC   INVYOS,BKEYYSRV        YOS                                       
         MVC   INVMOS(1),BKEYMSRV     MOS                                       
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
PRB2C2   CLC   BKEYCLT,LASTKCLT    SEE IF SAME CLIENT                           
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
PRB2C7   LA     R3,WK2             POST TO EST TABLE                            
         MVI    WK2,C' '           INIT TO SPACES                               
         MVC    WK2+1(L'WK2-1),WK2                                              
         USING  PPGRECD,R3                                                      
         MVC    PPGKMED,QMED                                                    
         MVC    PPGKCLI,BKEYCLT                                                 
         MVC    PPGKPRO(3),BKEYPRD                                              
         MVC    PPGINVMO,BKEYMBIL                                               
         NI     PPGINVMO,X'0F'                                                  
         MVC    PPGINVN,BKEYINV                                                 
         MVC    PPGKEST,BKEYEST                                                 
         MVC    PPGEUC1,UCOMDATA                                                
         MVC    PPGEUC2,UCOMDATA+32                                             
         MVC    PPGEUC3,UCOMDATA+64                                             
         MVC    PPGEUC4,UCOMDATA+96                                             
         ZAP    PPGBAMT,BACTP                                                   
         ZAP    PPGTAX,=P'0'          NO TAX FOR GM                             
         MVC    PPGKSMED,BLMED                                                  
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,BDATE,(2,BKEYINV),(QMED,B1PROF),      X        
               B1XPROF                                                          
         L     RF,DMCB             10 CHAR INVOICE                              
         LA    RE,PPGDINV                                                       
         LHI   R0,10                                                            
TAPEO30  CLI   0(RF),C'-'          REMOVE DASHES                                
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,TAPEO30                                                       
*                                                                               
         GOTO1 DATCON,DMCB,BDATE,(20,PPGINVD)                                   
*                                                                               
         GOTO1  BINSRCH,BINVALS,(1,PPGRECD)                                     
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     PRB2C9                                                          
         L      R3,0(R1)           R3 -> TO EXISTING REC                        
*                                  WK2 HAS REC PASSED TO BINSRCH                
PPGD     USING PPGRECD,WK2         SAME KEY                                     
         AP    PPGBAMT,PPGD.PPGBAMT                                             
PRB2C8C  MVI   NETPAKSW,C'Y'                                                    
         BNE   PRB2C9                                                           
         CLC   PPGKSMED,PPGD.PPGKSMED  FOR NETPACK                              
         BE    PRB2C9                                                           
         MVI   PPGKSMED,C' '        MIXED SUB-MEDIA                             
         DROP  R3,PPGD                                                          
*                                                                               
PRB2C9   DS    0H                                                               
PRB2C20  CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
*                                                                               
         CLC   BKEYCLT(5),LASTKCLT CHK FOR SAME CLT + PRD                       
         BE    PRB3                YES                                          
         CLC   BKEYCLT,LASTKCLT    CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   BKEYPRD,LASTKPRD                                                 
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
         CLI   ENDRPTSW,C'E'       COMING FROM REQL OR LBOFF ?                  
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
         CLC   BKEYCLT,LASTKCLT    SEE IF NEW CLIENT ALSO                       
         BNE   PRB2D               IF YES THEN DON'T CHK PROFILE                
*                                                                               
         CLI   PROGPROF+1,C'Y'     CHK PAGE FOR PRODUCT                         
         BNE   PRB2D                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWCLT,C'Y'         SO CLIENT WILL REPRINT                       
*                                  FOR EACH PRODUCT                             
*                                                                               
*                                                                               
PRB2D    CLC   BKEYCLT,LASTKCLT                                                 
         BE    PRB3                                                             
*                                                                               
         CLI   LASTKCLT,0          SEE IF FIRST TIME                            
         BNE   PRB2F                                                            
         MVC   LASTPROF,PROGPROF   MUST SAVE PROFILE                            
         B     PRB3                                                             
*                                                                               
PRB2F    MVC   BLPRD,LASTCLI                                                    
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
         CLI   QCLT,C'$'             SEE IF DOING OFFILCE LIST                  
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
         CLI   ENDRPTSW,C'E'       COMING FROM REQL OR LBOFF ?                  
         BE    PRBRTNX             YES - EXIT - WILL RETURN TO                  
*                                    REQTOTALS OR OFFICE TOTALS                 
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   P2,SVMID2                                                        
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,BKEY                                                     
         MVC   LASTCLI,CLT                                                      
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,BKEYPRD            PRD                                     
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         MVC   BLEST(3),WORK+2          EST                                     
*                                                                               
PRB8     DS    0H                                                               
PRB9     GOTO1 DATCON,DMCB,(3,BKEYYSRV),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB12    DS    0H                                                               
*                                                                               
         GOTO1 =V(SPFMTINO),DMCB,BDATE,(2,BKEYINV),                    X        
               (QMED,B1PROF),B1XPROF                                            
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
         CLI   BRETAIL,X'02'                                                    
         BE    PRB13X2                                                          
         CLI   BRETAIL,X'01'                                                    
         BNE   PRB13X5                                                          
*                                                                               
PRB13X2  MVC   P2+BLRDATD(12),=C'RETAIL ACCT='                                  
         MVC   P2+BLRDATD+12(12),BRETACCT                                       
*                                                                               
PRB13X5  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(5,BLRDAT)            RUN DATE             
*                                                                               
         GOTO1 DATCON,(R1),(0,BQDATE),(7,BLBDAT)        BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),BTYPE                                                   
         LA    R3,BLTYP+2                                                       
PRB14    DS    0H                                                               
         MVI   0(R3),C'-'                                                       
*                                                                               
         TM    BILSTAT,X'02'           FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    BILSTAT,X'20'         SEE IF ALSO AOR                            
         BO    PRB14D                                                           
         MVC   1(3,R3),=C'COM'                                                  
*                                                                               
PRB14D   CLI   QOPT5+1,C'C'        TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   SPBVNETP,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
         MVC   1(3,R3),=C'MAN'                                                  
         TM    BILSTAT,X'40'         MANUAL BILL                                
         BO    PRB16                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    BILSTAT,X'20'         AOR BILL                                   
         BO    PRB16                                                            
         MVC   1(3,R3),=C'UFC'                                                  
         TM    BILSTAT,X'01'          UP FRONT COMMISSION                       
         BO    PRB16                                                            
         MVC   1(3,R3),=C'NET'                                                  
         TM    BILSTAT,X'08'          UP FRONT COMMISSION - NET                 
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
         CLI   QOPT5+1,C'A'        SEE IF AOR ONLY                              
         BE    PRB16A              YES - THEN INCLUDE IN TOTALS                 
         TM    BILSTAT,X'20'       SEE IF AOR BILL                              
         BZ    PRB16B                                                           
PRB16A   ZAP   BILTOTS(6),=P'0'                                                 
         ZAP   BILTOTS+6(6),=P'0'                                               
         ZAP   BILTOTS+12(6),=P'0'                                              
         ZAP   BILTOTS+18(6),BACTP       SET AC TO RCVBL                        
         ZAP   BILTOTS+24(6),BACTP       ACTUAL                                 
         AP    BILTOTS+24(6),MYGST                                              
         AP    BILTOTS+24(6),MYPST                                              
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
         B     PRB16E                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   ZAP   BILTOTS+0*BPLEQ(BPLEQ),SPBVGRSP     GROSS                        
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),SPBVNETP     NET                          
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'                                     
         SP    BILTOTS+2*BPLEQ(BPLEQ),BACTP        CD IS -(RCVBL)               
*                                                                               
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'        CD IS 0                      
*                                                                               
PRB16C   ZAP   BILTOTS+4*BPLEQ(BPLEQ),BACTP        RCVBL                        
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYGST        ADD GST TO BILL AMT          
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYPST        ADD PST TO BILL AMT          
         ZAP   BILTOTS+5*BPLEQ(BPLEQ),MYGST        AND GST TOTALS               
         ZAP   BILTOTS+6*BPLEQ(BPLEQ),MYPST        AND PST TOTALS               
*                                                                               
         ZAP   DUB,BACTP                                                        
         TM    BILSTAT,X'02'       UNLESS COMMISSION ONLY BILL                  
         BNZ   PRB16D                                                           
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),DUB          FOR AC CALC                  
         SP    BILTOTS+3*BPLEQ(BPLEQ),SPBVNETP     -NET = TRUE AC               
*                                                                               
PRB16E   LA    R3,BILTOTS                                                       
         BRAS  RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
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
PRBRTNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
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
*        LA    R5,12(R3)           CD                                           
*        LA    R4,BLCD                                                          
*        LH    R1,=H'-1'                                                        
*        BAS   RE,FMTEDT                                                        
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
         LA    R5,30(R3)           GST (PAST IS AT 36(R3))                      
         LA    R4,BLGST                                                         
         BAS   RE,FMTEDT8       PRINT PST UNDER GST                             
*                                                                               
FMTAX    MVI   TOTSW,0                                                          
*SMY*    B     EXIT                                                             
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
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
******   CLI   PAGYNAT,C'C'       SEE IF CANADIAN                               
******   BNE   *+8                ALWAYS IS FOR LABATTS                         
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
         CLI   MODE,REQLAST          REQUEST TOTALS                             
         BNL   PRNT90                                                           
         LA    R4,HEAD3+49                                                      
         MVC   0(23,R4),=C'PERIOD FROM          TO'                             
         GOTO1 DATCON,DMCB,SVQST,(8,12(R4))                                     
         GOTO1 DATCON,DMCB,SVQEND,(8,24(R4))                                    
         CLI   QCLT,C'$'             SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT3                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),RCSVOFC                                               
*****    GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,HEAD5+7                              
         L     RF,ADCLT                                                         
         MVC   HEAD5+7(1),COFFICE-CLTHDR(RF)                                    
         B     PRNT4                                                            
*                                                                               
PRNT3    CLI   QCLT,C'*'                                                        
         BNE   PRNT4                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),QCLT+1                                                
*****    GOTO1 VOFFOUT,DMCB,QCLT+1,HEXOUT,HEAD5+7                               
         MVC   HEAD5+7(1),QCLT+1                                                
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT5+1,C'A'       TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT5+1,C'B'       AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT5+1,C'X'       NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT5+1,C'C'       COMMISSION ONLY BILLS                         
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
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*                                                                               
         TITLE 'DSECTS AND WORK AREAS'                                          
SPGMWRKD DSECT                                                                  
*                                                                               
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VDDUCOM  DS    A                                                                
VGETUSER DS    A                                                                
VOFFOUT  DS    A                                                                
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
RELO     DS    A                                                                
LASTPROF DS    CL16                                                             
SVMKEY   DS    CL32                                                             
SVBAMT   DS    PL6                                                              
SVPPGKEY DS    CL10                                                             
*                                                                               
TAXSW    DS    XL1                                                              
ERRORSW  DS    XL1                                                              
COLSW    DS    XL1                                                              
SAVVEND  DS    CL09                                                             
SAVFILE  DS    CL13                                                             
SVMOS    DS    CL4                                                              
MYSVEST  DS    CL3                                                              
SEQNUM   DS    CL1                                                              
SAVCOFF  DS    CL1                                                              
SVLINE   DS    X                                                                
SVFORCEH DS    CL1                                                              
MYPPOS   DS    PL6                                                              
MYPACK   DS    PL6                                                              
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
WK       DS    CL64         KEY AND KEYSAVE                                     
WK2      DS    CL256                                                            
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
DOWNACT  DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL1                                                              
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
NEWCLT   DS    C                                                                
NETPAKSW DS    C                                                                
SKIPBILL DS    X                                                                
JWOASW   DS    C                                                                
YEARDIG  DS    XL1                 YEAR DIGIT                                   
DECADE   DS    XL1                                                              
WIAPLSW  DS    C                   WESTERN/APL SWITCH                           
WIQASW   DS    C                   WESTERN/APL QA SWITCH                        
WAPLBTSW DS    C                   YET ANOTHER MODE                             
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
*                                                                               
EEUSER1  DS    CL53        OUTPUT OF GETUSER - ESTIMATE USER 1                  
EEUSER2  DS    CL37                          - ESTIMATE USER 2                  
PPUSER1  DS    CL53        OUTPUT OF GETUSER - PRODUCT USER 1                   
PPUSER2  DS    CL37                          - PRODUCT USER 2                   
*                                                                               
START    DS    CL6                                                              
END      DS    CL6                                                              
SAVMAX   DS    X                                                                
STAGSW   DS    X                                                                
TOTSW    DS    X                                                                
RACTSW   DS    X                                                                
OACTSW   DS    X                                                                
FIRSTSW  DS    X                                                                
ENDRPTSW DS    X                                                                
LASTKEY  DS    0CL13                                                            
         DS    CL1                                                              
LASTKAM  DS    CL1                                                              
LASTKCLT DS    CL2                                                              
LASTKPRD DS    CL3                                                              
LASTKEST DS    CL1                                                              
LASTKMOS DS    CL2                                                              
LASTKBMO DS    CL1                                                              
LASTKINO DS    CL2                                                              
*                                                                               
LASTCPE  DS    CL6                 LAST CLT/PRD/EST                             
LASTCLI  DS    CL3                                                              
*                                                                               
INVPARS  DS    F                   INVOICE BINSRCH PARS                         
AINVTAB  DS    F                                                                
INVTABN  DS    F                                                                
INVTABL  DS    F                                                                
         DS    2F                                                               
*                                                                               
INVMAX   EQU   10000               MAX INVOICES PER CLT                         
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
CLTINVS  DS    F                   CLIENT INVOICES                              
CLTINVCN DS    F                   CLIENT INVOICES COUNTER                      
OFFINVS  DS    F                   OFFICE INVOICES                              
REQINVS  DS    F                   REQUEST INVOICES                             
RUNINVS  DS    F                   RUN INVOICES                                 
TRUNINVS DS    F                                                                
*                                                                               
ALLOWSW  DS    XL1                                                              
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
BPLEQ    EQU   6                   LENGTH OF PACKED ACCUMULATORS                
*                                                                               
BDEHSW   DS    XL1                                                              
BDEH1    DS    CL132                                                            
BDEH2    DS    CL132                                                            
BDEH3    DS    CL132                                                            
BDEH4    DS    CL132                                                            
*                                                                               
SVACTYR  DS    X                   ACTIVITY YEAR FILTER                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256            CONTAINS LENGTH FOR VAR LEN RECORDS             
         DS    F                                                                
OUTREC   DS    CL500                                                            
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL13             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
         SPACE 3                                                                
AMOUNTSD DSECT                                                                  
AMTGRS   DS    PL6                 GROSS                                        
AMTAC    DS    PL6                 ACTUAL COMMISSION                            
AMTACT   DS    PL6                 ACTUAL                                       
AMTNET   DS    PL6                 NET                                          
AMTCNT   DS    PL6                 COUNT                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTHST   DS    PL6                 HST                                          
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
*                                                                               
         EJECT                                                                  
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
         DS    CL1                                                              
BLGRS    DS    CL16                                                             
BLNET    DS    CL16                                                             
*LCD     DS    CL15                                                             
BLAC     DS    CL16                                                             
BLBIL    DS    CL16                                                             
BLGST    DS    CL16                                                             
         SPACE 3                                                                
BLESTD   EQU   BLEST-BLINE                                                      
BLPERD   EQU   BLPER-BLINE                                                      
BLRDATD  EQU   BLRDAT-BLINE                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
         ORG                                                                    
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
         SPACE 3                                                                
         PRINT ON                                                               
*                                                                               
HRECD    DSECT                     HEADER RECORD                                
HRTYP    DS    CL1                 H                                            
*RSAP    DS    CL10                1000000008 - WAS FOR PLANWORKS               
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
IRMED    DS    CL10                MEDIA NAME                                   
         DS    CL1                 /                                            
IRCLT    DS    CL3                 CLIENT                                       
         DS    CL1                 /                                            
IREST    DS    CL3                 ESTIMATE                                     
         DS    CL1                 /                                            
IRPRDLST DS    CL140               PRD LIST: 3 CHAR PRD + '/', MAX 35           
         DS    CL1                                                              
IRECDLQ  EQU   *-IRTYP                                                          
*                                                                               
*                                                                               
PPGRECD  DSECT                                                                  
PPGKEY   DS    0XL32                                                            
PPGKMED  DS    CL1                                                              
PPGKSMED DS    CL1                                                              
PPGKCLI  DS    XL2                                                              
PPGKPRO  DS    CL3                                                              
PPGKEST  DS    XL1                                                              
PPGKEUC1 DS    CL20          PO# EST 1ST UCOMM                                  
PPGKNUMB DS    0XL4                                                             
PPGINVMO DS    XL2           INVOICE MONTH -- YM  (NOT MOS)                     
PPGINVN  DS    XL2           INVOICE NUMBER                                     
*            ______                                                             
*              32                                                               
PPGKLEN  EQU   *-PPGKEY                                                         
PPGDINV  DS    CL10          INVOICE# NO DASHES                                 
PPGINVD  DS    CL8           INVOICE DATE ON BILL YYYYMMDD                      
PPGEUC1  DS    CL20          EST UCOM 1  AGREEMENT #                            
PPGEUC2  DS    CL10          EST UCOM 2  PLD LINE ITEM #                        
PPGEUC3  DS    CL10          EST UCOM 3  PO SERVICE LINE                        
PPGEUC4  DS    CL32          EST UCOM 4  PO ITEM SHORT TEST                     
PPGBAMT  DS    PL6           ACTUAL                                             
PPGTAX   DS    PL6           TAX                                                
PPGTLEN  EQU   *-PPGKEY                                                         
*                                                                               
INVTABD  DSECT                                                                  
INVKMED  DS    CL1                                                              
INVKCLI  DS    XL2                                                              
INVKNUMB DS    0XL3                                                             
INVINVMO DS    XL1           INVOICE MONTH                                      
INVINVN  DS    XL2           INVOICE NUMBER                                     
INVKLEN  EQU   *-INVKMED                                                        
INVIDAT  DS    CL6           RUN DATE                                           
INVAMT   DS    PL6           TOTAL AMOUNT FOR INVOICE                           
INVYOS   DS    XL1           YEAR OF SERVICE                                    
INVMOS   DS    CL12          INVOICE MONTHS OF SERVICE                          
INVTLEN  EQU   *-INVKMED                                                        
*                                                                               
*                                                                               
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*INVTLEN)                                               
         DC    X'00'                                                            
*                                                                               
PPGTAB   CSECT                                                                  
         ORG   *+(BINMAX*PPGTLEN)                                               
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098SPREPGM02 02/27/15'                                      
         END                                                                    
