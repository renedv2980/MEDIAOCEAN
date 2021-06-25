*          DATA SET PPREPLO02  AT LEVEL 015 AS OF 01/20/15                      
*PHASE PPLO02A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE GETUSER                                                                
*                                                                               
         TITLE 'PPLO02 - CHANGE LOG'                                            
*                                                                               
*                                                                               
         TITLE 'PPLO02 - PRINTPAK OREAL INTERFACE'                              
*                                                                               
*                                                                               
* QOPT4        Y= PRODUCE DOWNLOAD FILE                                         
*                                                                               
* QINVNO1      COL 53(4) START INVOICE NUMBER (IN QSECOND)                      
* QINVNO2      COL 57(4) END INVOICE NUMBER (IN QSECOND)                        
*                                                                               
PPLO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPLO02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         LA    R8,SPACEND                                                       
         USING PPLOWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,FBUYREQ                                                     
         BE    FBLR                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBLR                                                             
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
         MVI   MAXLINES,57                                                      
         MVI   ERRORSW,0                                                        
         MVI   DOWNACT,0           CLEAR DOWNLOAD ACTIVITY                      
*                                                                               
         B     EXIT                                                             
*                                                                               
FBLR     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         XC    LOWINV,LOWINV     CLEAR LOW INVOICE NUMBER FILTER                
         MVC   HIINV,=X'FFFF'    SET HIGH INVOICE NUMBER FILTER TO MAX          
         CLC   QINVNO1(4),SPACES                                                
         BE    REQF5                                                            
         PACK  DUB,QINVNO1(4)                                                   
         CVB   R0,DUB                                                           
         STH   R0,LOWINV                                                        
*                                                                               
REQF5    DS    0H                                                               
         CLC   QINVNO2(4),SPACES                                                
         BE    REQF10                                                           
         PACK  DUB,QINVNO2(4)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HIINV                                                         
*                                                                               
REQF10   DS    0H                                                               
         MVC   QINVNO1(4),SPACES      CLEAR FOR PPG                             
         MVC   QINVNO2(4),SPACES      CLEAR FOR PPG                             
*                                                                               
         LA    RF,PESTREC                                                       
         ST    RF,ADEST                                                         
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRD                                                         
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLT                                                         
*                                                                               
         L     RF,=V(GETUSER)                                                   
         A     RF,RELO                                                          
         ST    RF,VGETUSER                                                      
*                                                                               
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
*                                                                               
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
*                                                                               
         L     RF,=V(PPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VPPBVAL                                                       
*                                                                               
         L     RF,=V(PPFMTINO)                                                  
         A     RF,RELO                                                          
         ST    RF,VPFMTINO                                                      
*                                                                               
         L     RF,=V(OFFOUT)                                                    
         A     RF,RELO                                                          
         ST    RF,VOFFOUT                                                       
*                                                                               
*                                                                               
         SR    R0,R0            SET INVOICE LIST BINSRCH PARS                   
         L     R1,=A(INVTAB)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)    INVMAX IS 10,000 INVS PER CLT                   
         STM   R0,R5,INVPARS                                                    
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   FBLR1                                                            
         MVI   FIRSTSW,1                                                        
*                                                                               
         MVI   ENDRPTSW,0         CLEAR SWITCH USED IN END PROCESSES            
*                                                                               
         LA    R3,RUNTOTS                                                       
         BRAS  RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
         XC    TRUNINVS,TRUNINVS    TAPE INVOICE TOTALS                         
*                                                                               
         XC    START(12),START                                                  
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
FBLR2    DS    0H                                                               
         CLI   QOPT4,C'Y'          PRODUCE TAPE (DOWNLOAD FILE)                 
         BNE   FBLR3               NO                                           
*                                                                               
         GOTO1 VDOWNLD,DMCB,(RA)   INITIALIZE DOWNLOAD                          
*                                                                               
FBLR3    DS    0H                                                               
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
         BNE   FBLR4                                                            
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD       ***   CONVERT START             
         GOTO1 DATCON,DMCB,(0,START),(0,START)  *** TO NEW DATE FORMAT          
         MVC   END,START                                                        
FBLR4    DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
FBC      DS    0H                  FIRST FOR CLIENT                             
*                                                                               
         XC    LASTCPE,LASTCPE                                                  
         XC    PPGRECNT,PPGRECNT     MUST CLEAR FOR BINSRCH                     
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
         L     R2,PPGRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
*                                                                               
LBCLI3   L     R3,AOFPPGT                                                       
         LA    R3,L'PPGKEY(R3)     BUMP PAST PSUEDO KEY                         
LBCLI5   GOTO1 VDOWNLD,DMCB,(RA),(R3)                                           
         LA    R3,PPGTLEN(R3) TO NEXT RECORD IN TABLE                           
         BCT   R2,LBCLI5                                                        
*                                                                               
ZEROCNT  B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLIENT,C'$'                                                     
         BE    LBLR2                                                            
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BE    LBLR2               NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM LBLR OR LBOFF          
         BRAS  RE,PRBILRTN           GO FINISH LAST PRD/CLT                     
         MVI   ENDRPTSW,0          CLEAR                                        
*                                                                               
LBLR2    DS    0H                                                               
         CLI   RACTSW,0                                                         
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BRAS  RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         CLI   QOPT4,C'Y'                                                       
         BNE   LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BRAS  RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
LBLR4    BRAS  RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BRAS  RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
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
         CLI   ERRORSW,0        ANY ERRORS?                                     
         BE    RUNL10              NO                                           
         MVC   P+1(36),=C'*** ERRORS HAVE BEEN ENCOUNTERED ***'                 
         BRAS  RE,PRNT                                                          
         TM    ERRORSW,X'01'     ESTIMATE USER1 INVALID ?                       
         BNO   RUNL3                                                            
         MVC   P+3(43),=C'*** ESTIMATE USER1 INCORRECT OR MISSING ***'          
         BRAS  RE,PRNT                                                          
RUNL3    DS    0H                                                               
         TM    ERRORSW,X'02'     ESTIMATE USER2 INVALID ?                       
         BNO   RUNL8                                                            
         MVC   P+3(43),=C'*** ESTIMATE USER2 INCORRECT OR MISSING ***'          
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8    DS    0H                                                               
         MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
         BRAS  RE,PRNT                                                          
*                                                                               
*                       CLOSE DOWNLOAD FILE HERE                                
RUNL10   DS    0H                                                               
         CLI   DOWNACT,C'Y'       DOWNLOADING ACTIVITY?                         
         BNE   EXIT                                                             
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
         SPACE 2                                                                
*                            ***** PROCESS BILL *****                           
         SPACE 2                                                                
PRBIL    DS    0H                                                               
*                                                                               
*        MANUAL BILL CHECK SKIPPED AS PER THEIR INSTRUCTIONS                    
*        TICKET # 0294538N  OCT12/10                                            
*                                                                               
**NO-OP  CLI   PBILLTYP,C'M'       ONLY PROCESS MANUAL BILLS                    
**NO-OP  BNE   EXIT                                                             
*                                                                               
*                                                                               
         CLC   PBILKBNO,LOWINV                                                  
         BL    EXIT                                                             
         CLC   PBILKBNO,HIINV                                                   
         BH    EXIT                                                             
*                                                                               
         MVI   ENDRPTSW,0          CLEAR                                        
         BRAS  RE,PRBILRTN         BILL ROUTINE                                 
*                                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
***OFF                                                                          
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   OACTSW,0            ANY BILL FOR OFFICE PROCESSED                
         BE    LBOFF5              NO                                           
         MVI   ENDRPTSW,C'E'       INDICATES COMING FROM LBLR OR LBOFF          
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
*                       POST BILL TO TABLE                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS     0D                                                              
POSTB    NTR1   BASE=*,LABEL=*                                                  
         LA     R3,PPGREC                                                       
         USING  PPGRECD,R3                                                      
         MVC    PPGIDATE,PBILINVD                                               
         MVC    PPGINVF,DINVFULL      FULL INVOICE NUMBER                       
         MVC    PPGINVNO,DINVNO       LAST 6 DIGITS OF INVOICE NUMBER           
         MVC    PPGMOB,PBILKBMN       BILLING MONTH - YM                        
         MVC    PPGEU1,EUSER1+21      JUST THE VALUE                            
         MVC    PPGEU2,EUSER2+21      JUST THE VALUE                            
         OC     PPGEU1,SPACES                                                   
         OC     PPGEU2,SPACES                                                   
*                                                                               
*        CHECKS FOR MISSING ESTIMATE USER DATA SKIPPED                          
*        ACCORDING TO THEIR INSTRUCTIONS                                        
*        TICKET # 0294538N                                                      
*                                                                               
         B      POST4                                                           
*                                                                               
         CLC    PPGEU1,SPACES                                                   
         BH     POST2                                                           
         MVC    P+3(43),=C'*** ESTIMATE USER1 INCORRECT OR MISSING ***'         
         BRAS   RE,PRNT                                                         
         OI     ERRORSW,X'01'                                                   
*                                                                               
POST2    CLC    PPGEU2,SPACES                                                   
         BH     POST4                                                           
         MVC    P+3(43),=C'*** ESTIMATE USER2 INCORRECT OR MISSING ***'         
         MVI    SPACING,2                                                       
         BRAS   RE,PRNT                                                         
         OI     ERRORSW,X'02'                                                   
*                                                                               
POST4    DS     0H                                                              
         CLI    QOPT4,C'Y'               SEE IF CREATING FILE                   
         BNE    POSTBX                                                          
*                                                                               
         ZAP    PPGBAMT,BILTOTS+24(6) AMOUNT DUE (INCLUDES TAXES)               
         ZAP    PPGGST,MYGST          BILLS GST                                 
         AP     PPGGST,MYHST          ADD HST                                   
         ZAP    PPGPST,MYPST          PST                                       
         SP     PPGPST,MYHST          EXCLUDE HST                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    PPGMED,PBILKMED                                                 
         MVC    PPGKMED,PBILKMED                                                
         MVC    PPGCLI,PBILKCLT                                                 
**NOPRD* MVC    PPGPRO,PBILKPRD  NO-OP SINCE PRDS MIGHT BE TOGETHER             
         MVC    PPGKEST,PBILKEST                                                
         MVC    PPGEST,PBILKEST                                                 
         MVC    PPGINVMO,PBILKBMN+1    BILLING MONTH                            
         MVC    PPGINVN,PBILKBNO                                                
*                                                                               
         L      R2,AOFPPGT        ADDRESS OF PPGTAB                             
         PRINT  GEN                                                             
         GOTO1  =V(BINSRCH),BINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L      RF,BINVALS               ADDRESS OF FOUND RECORD                
         LA     RF,L'PPGKEY(RF)          PAST KEY                               
*                                                                               
POSTB5   DS     0H                                                              
         AP     AMTDIS(6,RF),PPGBAMT                                            
         AP     GSTDIS(6,RF),PPGGST                                             
         AP     PSTDIS(6,RF),PPGPST                                             
GOTOXIT  DS     0H                                                              
         MVC    BINVALS,=A(PPGKMED)                                             
         MVI    BINVALS,1                                                       
POSTBX   DS     0H                                                              
         XIT1                                                                   
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(PPGKMED)      RECORD TO BE ADDED                             
         DC    A(PPGTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
PPGRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(PPGTLEN)       LEN OF RECORD                                 
         DC    AL4(L'PPGKEY)      KEY SIZE                                      
         DC    F'4000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFPPGT  DC    A(PPGTABLE)                                                      
PPGKEY   DS    0XL12                                                            
PPGKMED  DS    CL1                                                              
PPGCLI   DS    CL3                                                              
PPGPRO   DS    CL3    **NOPRD*  NOT SET AS THEY MAY DO PRDS TOGETHER            
PPGINUMB DS    0XL3                                                             
PPGINVMO DS    XL1           INVOICE MONTH                                      
PPGINVN  DS    XL2           INVOICE NUMBER                                     
PPGKEST  DS    XL2                                                              
*            ______                                                             
*              12                                                               
PPGREC   DS    0C                                                               
         ORG   *+PPGRECL                                                        
*                                                                               
ENDPPGR  DS    0C                                                               
*                                                                               
PPGTLEN  EQU   ENDPPGR-PPGKEY                                                   
*                                                                               
***********************                                                         
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        PROCESS BILL                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         DS    0D                                                               
PRBILRTN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ENDRPTSW,C'E'       COMING FROM LBLR OR LBOFF ?                  
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
         CLI   PBRETAIL,X'81'      CORP RETAIL?                                 
         BE    PRBRTNX                                                          
*                                  GET USER FIELDS                              
         CLC   LASTCPE(6),PBILLREC+4  NEW PRODUCT                               
         BE    PRB02                                                            
*                                                                               
         XC    PPUSER1,PPUSER1     CLEAR                                        
         XC    PPUSER2,PPUSER2                                                  
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'P',ADPRD),(0,PPUSER1),    X        
               (0,PPUSER2)                                                      
*                                                                               
PRB02    CLC   LASTCPE,PBILLREC+4  NEW ESTIMATE                                 
         BE    PRB05                                                            
*                                                                               
         MVC   LASTCPE,PBILLREC+4                                               
*                                                                               
         XC    EUSER1,EUSER1       CLEAR                                        
         XC    EUSER2,EUSER2                                                    
         XC    UCOMDATA,UCOMDATA                                                
*                                                                               
*        GET PO# (EUSER1) AND LINE ITEM# (EUSER2)                               
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLT),(C'E',ADEST),(0,EUSER1),(0,EUX        
               SER2)                                                            
*                                                                               
*                                                                               
*        CHECKS FOR MISSING ESTIMATE USER DATA SKIPPED                          
*        ACCORDING TO THEIR INSTRUCTIONS                                        
*        TICKET # 0294538N                                                      
*                                                                               
         B     PRB05          SKIP EST USER CHECKS                              
*                                                                               
         CLC   EUSER1(4),=C'PO #'                                               
         BE    *+8                                                              
         OI    ERRORSW,X'01'                                                    
         CLC   EUSER1+21(4),SPACES                                              
         BH    *+8                                                              
         OI    ERRORSW,X'01'                                                    
*                                                                               
         CLC   EUSER2(11),=C'LINE ITEM #'                                       
         BE    *+8                                                              
         OI    ERRORSW,X'02'                                                    
         CLC   EUSER2+21(4),SPACES                                              
         BH    *+8                                                              
         OI    ERRORSW,X'02'                                                    
*                                                                               
PRB05    DS    0H                                                               
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
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
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
         CLC   PBILKCLT,LASTKCLT    SEE IF SAME CLIENT                          
         BE    PRB2C2                                                           
*                                                                               
         MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
         SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
         L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
         STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
PRB2C2   DS    0H                                                               
         MVC   WORK(2),PBILKBMN                                                 
         MVC   WORK+2(2),PBILKBNO                                               
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
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
         GOTO1 VPFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),                    X        
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
*                                                                               
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
PRB28    DS    0H                                                               
*******  CLI   QOPT4,C'Y'           SEE IF NOT PUTTING ON FILE                  
*******  BNE   PRBRTNX                                                          
         BRAS  RE,POSTB             POST BILL TO TABLE                          
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
*                                                                               
*        DO THIS HEADLINE HERE SINCE PSPECS IN 01 PHASE                         
*        DON'T SUPPORT IMBEDED QUOTE MARKS                                      
*                                                                               
         MVC   HEAD1+53(26),=C'PRINTPAK L''OREAL INTERFACE'                     
         MVC   HEAD2+53(26),=C'--------------------------'                      
*                                                                               
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
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPLOWRKD,R8                                                      
         L     R4,4(R1)        ADDRESS OF PPGREC                                
         USING PPGRECD,R4                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNRPTEND                                                         
*                                                                               
         CLI   MODE,FBUYREQ       SEE IF I NEED TO INTIALIZE                    
         BNE   DNMAIN                                                           
*                                                                               
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
*                                                                               
*****    MAY DO "HEADER TITLES" HERE                                            
*                                                                               
         B     DNXIT                                                            
*                                                                               
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
*****                                                                           
*                                                                               
DNMAIN   DS    0H                                                               
         MVI   DOWNACT,C'Y'        SET DOWN ACTIVITY                            
*                             FULL INVOICE NUMBER                               
*                             WITHOUT DASHES                                    
         MVC   DLCBFLD(2),PPGINVF   MEDIA PART                                  
         LA    R2,DLCBFLD+1                                                     
         CLI   PPGINVF+1,C'-'      WAS IT ONE CHARACTER?                        
         BE    *+8                                                              
         LA    R2,DLCBFLD+2                                                     
         MVC   0(6,R2),PPGINVNO    LAST 6 DIGITS                                
*                                                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  PO# - EST USER 1                             
         MVC   DLCBFLD(32),PPGEU1                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  ITEM # EST USER 2                            
         MVC   DLCBFLD(16),PPGEU2                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  ITEM AMOUNT - SAME AS AMOUNT DUE             
*                                  SINCE THESE ARE MANUAL BILLS                 
         CP    PPGBAMT,=P'0'                                                    
         BNE   DNM10                                                            
         MVC   DLCBFLD(3),=C'.00'                                               
         B     DNM14               OUTPUT IS ZERO                               
*                                                                               
DNM10    ST    R1,SAVER1           SAVE R1                                      
         EDIT  (P6,PPGBAMT),(11,WORK+20),2,FLOAT=-,ALIGN=LEFT                   
         L     R1,SAVER1           RESTORE R1                                   
         LR    R3,R0               LENGTH OF DATA OUTPUT                        
         BCTR  R3,0                FOR EXECUTED MOVE                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20                                               
DNM14    MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  ITEM QUANTITY                                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20  REPEAT AMOUNT DUE                            
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  ITEM QUANTITY                                
         MVC   DLCBFLD(3),=C'VAL'  VAL - VALUE ON THEIR END                     
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  POSTING DATE - SAME AS INVOICE DATE?         
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(3,PPGIDATE),(20,WORK)                               
         L     R1,SAVER1      RESTORE R1                                        
         MVC   DLCBFLD(8),WORK     YYYYMMDD                                     
*                                                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE DATE                                 
         MVC   DLCBFLD(8),WORK     YYYYMMDD                                     
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  CURRENCY                                     
         MVC   DLCBFLD(3),=C'USD'                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CP    PPGBAMT,=P'0'                                                    
         BNE   DNM20                                                            
         MVC   DLCBFLD(3),=C'.00'                                               
         B     DNM24               OUTPUT IS ZERO                               
*                                                                               
DNM20    ST    R1,SAVER1           SAVE R1                                      
         EDIT  (P6,PPGBAMT),(11,WORK+20),2,FLOAT=-,ALIGN=LEFT                   
         L     R1,SAVER1           RESTORE R1                                   
         LR    RF,R0               LENGTH OF DATA OUTPUT                        
         BCTR  RF,0                FOR EXECUTED MOVE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),WORK+20                                               
DNM24    MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  TAX CATEGORY - ALWASY I0                     
         MVC   DLCBFLD(3),=C',I0'  BEGIN WITH ',' - NO TAX ON MANUAL            
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL     SEND END OF LINE                             
         GOTO1 VDLFLD                                                           
         B     DNXIT                                                            
*                                                                               
DNRPTEND DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,DLCBEOR     SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
*                                                                               
DNXIT    DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
DNPRINT  NTR1                                                                   
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    X'6B'       FIELD DELIMITER - COMMA                              
EOTCHR   DC    X'00'       END OF TEXT FIELD DELIMITER                          
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'00'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    X'00'       END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':',X'4F' (VERTICAL BAR)                        
*        COMMA IS X'6B'                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*                                                                               
         TITLE 'DSECTS AND WORK AREAS'                                          
PPLOWRKD DSECT                                                                  
PPLOWRK  DS    0C                                                               
VDOWNLD  DS    A                                                                
VGETUSER DS    A                                                                
VDLFLD   DS    A                                                                
VPPBVAL  DS    A                                                                
VPFMTINO DS    A                                                                
VOFFOUT  DS    A                                                                
VDDUCOM  DS    A                                                                
RELO     DS    A                                                                
ADCLT    DS    A                                                                
ADEST    DS    A                                                                
AAAEST   DS    A           AAA ESTIMATE                                         
ADPRD    DS    A                                                                
*                                                                               
LOWINV   DS    H           START INVOICE NUMBER                                 
HIINV    DS    H           END INVOICE NUMBER                                   
*                                                                               
ERRORSW  DS    XL1                                                              
DOWNACT  DS    CL1                                                              
SVLINE   DS    XL1                                                              
SVFORCEH DS    CL1                                                              
MYPPGK   DS    CL64        FOR SAVING PPG'S KEY AND KEYSAVE                     
LASTCPE  DS    CL8         LAST CLT/PRD/EST                                     
SVMOS    DS    CL4         SAVED MOS - THEIR FORMAT (MMYY)                      
SVEST    DS    CL3         SAVED ESTIMATE   (NNN)                               
EUSER1   DS    CL53        OUTPUT OF GETUSER - ESTIMATE USER 1                  
EUSER2   DS    CL37                          - ESTIMATE USER 2                  
PPUSER1  DS    CL53        OUTPUT OF GETUSER - PRODUCT USER 1                   
PPUSER2  DS    CL37                          - PRODUCT USER 2                   
START    DS    CL6                                                              
END      DS    CL6                                                              
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
INVPARS  DS    6F               FOR INVTAB BINSRCH                              
*                                                                               
INVMAX   EQU   10000            ALLOW 10000 INVOICES PER CLIENT                 
*                                                                               
CLTINVS  DS    F                INVOICE TOTALS                                  
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
ENDRPTSW DS    XL1                 E=IN PRBILRTN FROM LBLR OR LBOFF             
*                                                                               
       ++INCLUDE PPBVALD           NEW PPBVAL DSECT                             
*                                                                               
PBIREC   DS    CL256                                                            
         DS    CL34                TO ALLOW FOR 300 BYTE RECORDS                
         DS    CL114                                                            
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
UCOMQ    EQU   *-UCOMBLK                                                        
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
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
* START AND END INVOIVE NUMBERS COL 53(4), AND COL 56 (4)                       
*                                                                               
QINVNO1  EQU   QRECORD+52                                                       
QINVNO2  EQU   QRECORD+56                                                       
*                                                                               
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
PPGRECD  DSECT                                                                  
PPGMED   DS    CL1           MEDIA (ALSO IN KEY)                                
PPGEST   DS    XL2           ESTIMATE (ALSO IN KEY)                             
PPGIDATE DS    XL3           INVOICE DATE                                       
PPGINVF  DS    CL10          FULL INVOICE NUMBER                                
PPGMOB   DS    XL2           FROM PBILKBMN                                      
PPGCDTYP DS    CL1           CD ITEMS INDICATOR                                 
PPGEU1   DS    CL32          EST USER 1                                         
PPGEU2   DS    CL16          EST USER 2                                         
PPGENAME DS    CL20          ESTIMATE NAME                                      
PPGPU1   DS    CL32          PRD USER 1                                         
PPGMGR1  DS    CL3           REGION FROM PNBMGR1 IN BILLREC                     
PPGINVNO DS    CL6           LAST 6 DIGITS OF INVOICE NUMBER                    
*        THE FIELDS WILL BE THE VALUES FOR THE FIRST BILL                       
*        FOR THE BINSRCH KEY                                                    
*                                                                               
*        THE FIELDS BELOW ARE THE TOTALS FOR THE INVOICE                        
*        (BINSRCH KEY)                                                          
PPGBAMT  DS    PL6                                                              
PPGGST   DS    PL6                                                              
PPGPST   DS    PL6                                                              
*                                                                               
PPGRECL  EQU   *-PPGMED                                                         
*                                                                               
LEDIS    EQU   PPGPU1-PPGMED                                                    
AMTDIS   EQU   PPGBAMT-PPGMED                                                   
GSTDIS   EQU   PPGGST-PPGMED                                                    
PSTDIS   EQU   PPGPST-PPGMED                                                    
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(4000*PPGTLEN)                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPREPLO02 01/20/15'                                      
         END                                                                    
