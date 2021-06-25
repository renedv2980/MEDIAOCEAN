*          DATA SET PPREPJW02  AT LEVEL 069 AS OF 07/18/18                      
*PHASE PPJW02A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE BINSRCH2        LATEST VERSION                                         
*INCLUDE PPFMTINO                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE PRINT                                                                  
*                                                                               
*********************************************************************           
* USER    JIRA       DATE                  CHANGE LOG                           
* ---- ----------  -------- -----------------------------------------           
* SMUR SPEC-11729  05/07/18 NEW (D)IGITAL AUDIO (18.3)                          
*                                                                               
*********************************************************************           
         TITLE 'PPJW02 - JWT INTERFACE'                                         
*                                                                               
*                                                                               
* QOPT1        'F' = PRINT FORMULAS                                             
* QOPT2        'R' = PRINT REVERSAL INFO                                        
* QOPT3        'N' = NO PRINTING                                                
* QOPT4        ' ' = NO TAPE, Y = NORMAL TAPE, ELSE SPECIAL TAPE CODE           
* QOPT5         S= SOON BILLS ONLY                                              
* QOPT6         AOR OPTION                                                      
*               A=AOR ONLY                                                      
*               B=AOR ONLY AND AOR/CLIENT                                       
*               X=EXCLUDE AOR                                                   
*               N=EXCLUDE COMMISSION ONLY                                       
*               BLANK=AOR AND NON-AOR                                           
*              'C' = COMMISION ONLY BILLS                                       
* QOPT7    Y =  PRINT 50 INPUT AND OUTPUT RECORDS                               
*               BUT DON'T ACTUALLY PRODUCE A TAPE                               
*                                                                               
* QINVNO1      COL 53(4) START INVOICE NUMBER                                   
* QINVNO2      COL 57(4) END INVOICE NUMBER                                     
*                                                                               
*        NOTE = QOPT6 IS SET FROM AGYLST                                        
*               SOME ENTRIES HAVE X - NO AOR BILLS                              
*                                                                               
PPJW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPJW02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,PPJW02+4095                                                   
         LA    R9,1(R9)                                                         
         USING PPJW02+4096,R9           ** NOTE USE OF R9 AS                    
*                                          SECOND BASE REGISTER                 
         LA    R8,SPACEND                                                       
         USING PPJWWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
JACTCDN  EQU   16216          SET $ TYPE IN HEADER TO CAD                       
JGEOCDN  EQU   17004          SET $ TYPE IN HEADER TO CAD                       
*                             FOR THIS ID (INSTEAD OF USD)                      
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
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
         CLI   MODE,FBUYCLI                                                     
         BE    FBC                 FIRST BILL FOR CLIENT                        
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
RUNF     DS    0H                  RUN FIRST                                    
         RELOC RELO                                                             
*                                                                               
*                                                                               
         ZAP   JWAORDUE,=P'0'     CLEAR TOTAL OF AOR BILLS                      
*                                                                               
         MVI   BKOPENSW,C'N'      SET BK FILE NOT OPEN                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
*                                                                               
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R6,DUB                                                           
         EDIT  (R6),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         MVI   DYNSW,C'N'          SET TAPE NOT ALLOCATED                       
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
         LA    R3,RUNTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BAS   RE,CLRTOTS                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
REQF     DS    0H                                                               
         SPACE 3                                                                
FBLR     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         MVI   CAD$SW,C'N'                                                      
         CLC   RCORIGID,=Y(JACTCDN)     USE CAD FOR THIS ID                     
         BNE   *+8                      INSTEAD FO USD IN HEADER                
         MVI   CAD$SW,C'Y'                                                      
         CLC   RCORIGID,=Y(JGEOCDN)     USE CAD FOR THIS ID                     
         BNE   *+8                      INSTEAD FO USD IN HEADER                
         MVI   CAD$SW,C'Y'                                                      
*                                                                               
         LA    R3,REQTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         MVC   SVQOPT7,QOPT7     SAVE                                           
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
         MVI   TESTMQ,C'P'         SET TO PROD MQ NOTIFICATION                  
         CLI   QOPT7,C'N'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'Y'          TRACING OUTPUT RECORDS                       
         BNE   *+8                 ALSO SUPPRESS MQ                             
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT7,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'         TEST MQ                                      
*                                                                               
REQF13B  DS    0H                                                               
REQF13M  DS    0H                                                               
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
         ZAP   JWCOUNT,=P'0'       CLEAR JWT RECORD COUNT                       
         ZAP   JWTOTAL,=P'0'       CLEAR JWT $ TOTAL                            
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
         XC    TRUNINVS,TRUNINVS    TAPE INVOICE TOTALS                         
*                                                                               
         XC    START(12),START                                                  
         XC    AOUTP,AOUTP                                                      
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
         MVC   DYNDDN,=CL8'PJWTAPE'                                             
         MVC   DYNDSN,=CL20'PRTTAPE.PP0JWXX1'                                   
         MVC   DYNDSN+13(2),QAGENCY                                             
         CLC   QAGENCY,=C'O$'                                                   
         BNE   *+10                                                             
         MVC   DYNDSN+13(2),=C'OS'      O$ NOT ALLOWED AS DSN                   
*                                                                               
*              NO-OP MIX FOR TAPE SPECS CHK                                     
**                                                                              
         CLI   QOPT4,C'N'                                                       
         BE    FBLR2                                                            
*                                                                               
         MVC   BYTE,QOPT4          TAPE SPEC                                    
         CLI   BYTE,C'Y'           Y = BLANK                                    
         BNE   *+8                                                              
         MVI   BYTE,C' '                                                        
         CLI   TAPTYP,0            TEST ALREADY SET                             
         BNE   *+14                                                             
         MVC   TAPTYP,BYTE                                                      
         B     FBLR2                                                            
         CLC   TAPTYP,BYTE         MAY NOT MIX TAPE SPECS                       
         BE    FBLR2                                                            
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   PSECOND(55),=C'**MIXED TAPE SPECS IN ONE RUN - THIS REQUX        
               EST BYPASSED**'                                                  
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
FBLR2    DS    0H                                                               
         CLI   QOPT4,C'N'          NO TAPE                                      
         BE    FBLR3                                                            
         LM    R3,R5,AGYLST                                                     
         A     R3,RELO                                                          
         A     R5,RELO                                                          
         MVC   WORK(4),SPACES                                                   
         MVC   WORK(2),RCAGYFLT                                                 
         MVC   WORK+2(1),TAPTYP    SPECIAL FORMAT CODE                          
*                                                                               
         CLC   WORK(3),0(R3)                                                    
         BE    *+8                                                              
         BXLE  R3,R4,*-10                                                       
*                                                                               
         MVC   QOPT6,3(R3)         SET AOR OPTION FOR AGYLST                    
*                                                                               
         L     RF,4(R3)                                                         
         LTR   RF,RF               TEST HAVE TAPE PROC                          
         BNZ   FBLR2D                                                           
         CLI   QOPT4,C' '         OK IF NO TAPE SPEC                            
         BNH   FBLR3                                                            
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   PSECOND(35),=C'**INVALID TAPE SPEC- REQ BYPASSED**'              
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
FBLR2D   DS    0H                                                               
         A     RF,RELO                                                          
         ST    RF,AOUTP                                                         
*                                                                               
FBLR3    DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,REQTOTS                                                       
         BAS   RE,CLRTOTS                                                       
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
AGYLST   DS    0F                AGENCY TABLE                                   
         DC    A(AGYLST+12)                                                     
         DC    A(8)                                                             
         DC    A(AGYLSTX-1)                                                     
*                                                                               
         DC    CL4'FR  ',A(JWT)                                                 
         DC    CL4'H7  ',A(JWT)                                                 
         DC    CL4'JS  ',A(JWT)                                                 
         DC    CL4'O$  ',A(JWT)                                                 
         DC    CL4'SJ  ',A(JWT)      SJR FOR TESTING ONLY                       
         DC    CL4'H0 X',A(JWT)      MSHTOA  (CANADIAN) NO-AOR                  
         DC    CL4'HY X',A(JWT)      OUTTO   (CANADIAN) NO-AOR                  
*                                                                               
AGYLSTX  EQU   *                                                                
         DC    CL4'XX',A(0)                                                     
         EJECT                                                                  
FBC      DS    0H                  FIRST FOR CLIENT                             
*                                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   TBCLTF0                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
TBCLTF0  DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
*                                                                               
         CLI   QOPT4,C'N'           SEE IF A TAPE REQUEST                       
         BE    FBC0X                NO - PROCESS ALL CLTS                       
*                                                                               
*                                                                               
FBC0D    DS    0H                                                               
         B     FBC0X                                                            
FBC0N    MVI   MODE,LBUYCLI         SKIP TO NEXT CLIENT                         
         B     EXIT                                                             
*                                                                               
FBC0F    DS    0H                                                               
         B     FBC0X                                                            
FBC0X    DS    0H                                                               
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
*                                                                               
         MVC   JWCLTNAM,PCLTNAME    ALTER NAME IN INTERFACE                     
         LA    R1,JWCLTNAM          - IT CAN'T HAVE A COMMA                     
         LA    R2,20                                                            
FBC1D3   CLI   0(R1),C','      CHANGE , TO DASH                                 
         BNE   FBC1D5                                                           
         MVI   0(R1),C'-'                                                       
FBC1D5   LA    R1,1(R1)                                                         
         BCT   R2,FBC1D3                                                        
*                                                                               
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
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLIENT,C'$'                                                     
         BE    LBLR2                                                            
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BNE   PRB2B               IF YES - GO FINISH LAST PRD/CLT              
LBLR2    DS    0H                                                               
         CLI   RACTSW,0                                                         
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         CLI   QOPT4,C'N'                                                       
         BE    LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BAS   RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
LBLR4    MVI   BLPRD,C' '       MIGHT STILL HAVE AN * - CLEAR IT                
         BAS   RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                  RUN LAST                                     
         MVI   BLPRD,C' '       MIGHT STILL HAVE AN * - CLEAR IT                
         BAS   RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,RUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
         MVC   BLINE(15),=C'**TAPE TOTALS**'                                    
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,TRUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT               
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BAS   RE,PRNT                                                          
         L     RF,AOUTP                                                         
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                  PROCESS BILL                                 
         SPACE 2                                                                
PRBIL    DS    0H                                                               
*                                                                               
         CLC   PBILKBNO,LOWINV       BE SURE BILL PASSES INV # RANGE            
         BL    EXIT                                                             
         CLC   PBILKBNO,HIINV                                                   
         BH    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'S'            SOON BILLS ONLY?                           
         BNE   PRBIL5                                                           
         TM    PBILSTAT,PSTSOONQ    UPDATIVE SOON BILL                          
         BZ    EXIT                                                             
*                                                                               
PRBIL5   DS    0H                                                               
*                                                                               
PRB05    DS    0H                                                               
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT6,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
         B     PRB09A                                                           
*                                                                               
PRB09    DS    0H                                                               
         CLI   QOPT6,C'N'          EXCLUDING COMMISSION ONLY?                   
         BE    EXIT                                                             
PRB09A   DS    0H                                                               
*                                                                               
         CLI   QOPT6,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT6,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT6,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT6,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    EXIT                                                             
*                                                                               
PRB09D   DS    0H                                                               
*                                                                               
*                                                                               
         MVC   JWPRDNAM,PPRDNAME    ALTER NAME IN INTERFACE                     
         LA    R1,JWPRDNAM          - IT CAN'T HAVE A COMMA                     
         LA    R2,20                                                            
PRB09D3  CLI   0(R1),C','      CHANGE , TO DASH                                 
         BNE   PRB09D5                                                          
         MVI   0(R1),C'-'                                                       
PRB09D5  LA    R1,1(R1)                                                         
         BCT   R2,PRB09D3                                                       
*                                                                               
*                                                                               
PRB09DX  GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
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
         L     R0,PPBVPST       INCLUDES HST                                    
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
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
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB2                                                             
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB2     DS    0H                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   PBRETAIL,0                                                       
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   PBRETAIL,X'81'      IGNORE CORP BILLS                            
         BE    EXIT                                                             
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
PRB2C    CLI   PBILLMOD,C'P'       BYPASS PRD MODE BILLS                        
         BE    EXIT                                                             
         CLI   PBILLMOD,C'S'       AND SERIES MODE                              
         BE    EXIT                                                             
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
*                                                                               
         MVC   BLINE+4(19),=C' **PRODUCT TOTALS**'                              
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         CLI   MODE,LBUYREQ        REQUEST LAST                                 
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
         CLI   MODE,OFCLAST        OR LAST FOR OFFICE                           
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
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
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
         BAS   RE,ROLTOTS                                                       
         L     R0,OFFINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,OFFINVS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,REQINVS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         CLI   MODE,LBUYREQ            RETURN TO REQTOTALS                      
         BE    LBLR2                                                            
         CLI   MODE,OFCLAST            RETURN TO OFFICE TOTALS                  
         BE    LBOFF5                                                           
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   PSECOND,SVMID2                                                   
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,PBILLKEY                                                 
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,PBILKPRD           PRD                                     
         OC    PBILKEST,PBILKEST                                                
         BZ    PRB7                                                             
         LA    R3,PBILKEST                                                      
         BAS   RE,CVD                                                           
         MVC   BLEST(3),WORK+2        ONE EST                                   
         B     PRB8                                                             
*                                                                               
PRB7     DS    0H                                                               
         MVC   BLEST+1(2),=C'NO'                                                
*                                                                               
         B     PRB8                                                             
*                                                                               
PRB8     DS    0H                                                               
         CLI   PBILLPER,C' '       NOT USED FOR NEW BILLING                     
         BNH   PRB9                                                             
         CLI   PBILLPER,C'M'       TEST MONTH BILL                              
         BNE   PRB10                                                            
*PRB9     GOTO1 DTCNV,DMCB,(1,PBILKMOS),(5,BLPER)                               
PRB9     GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB10    DS    0H                  SPECIAL BILL PERIOD                          
         MVC   FULL(2),PBILKMOS                                                 
         MVC   FULL+2(1),PBILLSTA                                               
*        GOTO1 DTCNV,DMCB,(1,FULL),(4,BLPER)                                    
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
         L     RF,DMCB+12                                                       
         MVC   DINVMED,0(RF)       MEDIA PART                                   
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
*        GOTO1 DTCNV,DMCB,PBILLDAT,(3,BLRDAT)          RUN DATE                 
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,BLRDAT)         RUN DATE             
*                                                                               
*        GOTO1 (RF),(R1),(1,PBILINVD),(4,BLBDAT)       BILL DATE                
         GOTO1 DATCON,(R1),(3,PBILINVD),(7,BLBDAT)      BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),PBILLTYP                                                
         LA    R3,BLTYP+2                                                       
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
PRB14D   DS    0H                                                               
*                                                                               
PRB14E   CLI   QOPT6,C'C'          TEST TO LEAVE NET                            
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
         B     PRB16F                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
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
         B     PRB16F                                                           
*                                                                               
PRB16E   DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
         SP    BILTOTS+0*BPLEQ(BPLEQ),PBILLBIL                                  
         AP    BILTOTS+0*BPLEQ(BPLEQ),PBILLRCV                                  
*                                                                               
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),PBILLGRS                                  
         SP    BILTOTS+2*BPLEQ(BPLEQ),PBILLBIL     CD IS -(RCVBL)               
*                                                                               
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),PBILLNET     NET                          
         AP    BILTOTS+1*BPLEQ(BPLEQ),BILTOTS+2*BPLEQ(BPLEQ)                    
*                                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+0*BPLEQ(BPLEQ)                    
         SP    BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+1*BPLEQ(BPLEQ)                    
         SP    BILTOTS+3*BPLEQ(BPLEQ),BILTOTS+2*BPLEQ(BPLEQ)                    
*                                                                               
         ZAP   BILTOTS+4*BPLEQ(BPLEQ),PBILLRCV     RCVBL                        
*                                                                               
PRB16F   DS    0H                                                               
         LA    R3,BILTOTS                                                       
         BAS   RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         TM    PBILCMSW,X'20'           SEE IF AOR BILL                         
         BZ    *+10                                                             
         AP    JWAORDUE,BILTOTS+24(6)                                           
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
         BAS   RE,PRNT                                                          
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
*        GOTO1 DTCNV,DMCB,PBILLCDT,(3,20(R3))                                   
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(5,20(R3))                              
*                                                                               
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         MVI   RACTSW,1            SET REQUEST ACTIVITY                         
         MVI   OACTSW,1            SET REQUEST ACTIVITY                         
*                                                                               
PRB28    DS    0H                                                               
         L     RF,AOUTP            SPECIAL INTERFACE MODULE                     
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         CLI   QOPT4,C'N'           SEE IF NOT PUTTING ON TAPE                  
         BE    EXIT                                                             
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***OFF                                                                          
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         CLI   OACTSW,0                                                         
         BNE   PRB2B      MUST DO PRD/CLT TOTALS FIRST                          
*                         WILL RETURN TO LBOFF5                                 
LBOFF5   MVI   BLPRD,C'*'                                                       
*****    MVC   BLPRD+1(1),RCSVOFC                                               
*****    MVI   BLPRD+2,C' '                                                     
*****    GOTO1 =V(OFFOUT),DMCB,RCSVOFC,HEXOUT,BLPRD+1                           
*                                                                               
         CLI   OACTSW,0    ANY OFFICE ACTIVITY?                                 
         BE    EXIT        NO - JUST EXIT                                       
*                                                                               
         MVC   BLPRD+1(2),SAVCOFF  (FROM OFFICER CALL)                          
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
*                                                                               
*                                                                               
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         OC    OFFINVS,OFFINVS     ANY INVOICES?                                
         BNZ   LBOFF8                                                           
*                                                                               
         MVI   OACTSW,0                                                         
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   DS    0H                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,OFFINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
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
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
***OFF                                                                          
*                                  FORMAT $ AMTS                                
         SPACE 2                                                                
*                                                                               
NEXTEL1  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL1X                                                         
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL1                                                          
*                                                                               
NEXTEL1X LTR   RE,RE       NOT FOUND -                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
FMTAMTS  NTR1                                                                   
         SPACE 2                                                                
         MVI   STAGSW,0                                                         
         CP    0(6,R3),=P'9999999999'   COMPARE GROSS TO 100 MILLION            
         BL    *+8                                                              
         MVI   STAGSW,1            SET TO STAGGER TOTALS                        
*                                                                               
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         LH    R6,=H'-1'          USED WHEN STAGGERING                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         LA    R6,131              NEXT LINE -1                                 
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         LH    R6,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         LA    R6,131              NEXT LINE-1                                  
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         LH    R6,=H'-1'                                                        
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
         B     EXIT                                                             
         SPACE 2                                                                
FMTEDT   DS    0H                                                               
         CLI   STAGSW,1       SEE IF STAGGERING                                 
         BNE   FMTEDT5                                                          
         AR    R4,R6          ADJUST R4 FOR STAGGERING                          
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
         SPACE 3                                                                
         EJECT                                                                  
         SPACE 2                                                                
PRNT     NTR1                                                                   
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
         MVC   HEAD4(8),=C'OFFICE X'                                            
*****    MVC   HEAD4+7(1),RCSVOFC                                               
*****    GOTO1 =V(OFFOUT),DMCB,RCSVOFC,HEXOUT,HEAD4+7                           
         MVC   HEAD4+7(2),SAVCOFF                                               
         B     PRNT4                                                            
PRNT3    CLI   QCLIENT,C'*'                                                     
         BNE   PRNT4                                                            
         MVC   HEAD4(8),=C'OFFICE X'                                            
*****    MVC   HEAD4+7(1),QCLIENT+1                                             
*****    GOTO1 =V(OFFOUT),DMCB,QCLIENT+1,HEXOUT,HEAD4+7                         
         MVC   HEAD4+7(2),SAVCOFF                                               
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT6,C'A'         TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD5(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT6,C'B'         AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD5(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT6,C'X'         NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD5(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT6,C'C'         COMMISSION ONLY BILLS                         
         BNE   PRNT7                                                            
         MVC   HEAD5(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
PRNT7    DS    0H                                                               
         CLI   QOPT6,C'N'     NON-COMMISSION ONLY BILLS                         
         BNE   PRNT8                                                            
         MVC   HEAD5(34),=C'**COMMISSION ONLY BILLS EXCLUDED**'                 
PRNT8    DS    0H                                                               
         CLI   QOPT5,C'S'     SOON BILLS ONLY                                   
         BNE   PRNT90                                                           
         LA    R1,HEAD5                                                         
         CLC   HEAD5(5),SPACES  SEE IF HEAD5 USED ALREADY                       
         BE    *+8                                                              
         LA    R1,HEAD6         IF SO, USE HEAD6                                
         MVC   0(19,R1),=C'**SOON BILLS ONLY**'                                 
*                                                                               
PRNT90   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
         B     EXIT                                                             
         SPACE 3                                                                
ROLTOTS  DS    0H                                                               
         LA    R0,7           FOR BCT                                           
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
         BR    RE                                                               
         SPACE 3                                                                
CLRTOTS  DS    0H                                                               
         LA    R0,7                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
CVD      DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
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
         TITLE 'J WALTER THOMPSON'                                              
*                                  JWT                                          
         SPACE 2                                                                
JWT      NMOD1 0,JWPROC                                                         
         L     RC,PPFILEC                                                       
*                                                                               
JWPR1    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    JWPRL                                                            
*                                                                               
         MVI   PBIREC,C' '                                                      
         MVC   PBIREC+1(L'PBIREC-1),PBIREC                                      
*                                                                               
*     - BILL INV DATE (MM/DD/YYYY)                                              
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)      INV DATE                 
         MVC   PBIREC+0(2),WORK+4                                               
         MVI   PBIREC+2,C'/'                                                    
         MVC   PBIREC+3(2),WORK+6                                               
         MVI   PBIREC+5,C'/'                                                    
         MVC   PBIREC+6(4),WORK                                                 
         MVI   PBIREC+10,C','            DELIMITER                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILDUED),(20,WORK) DATE DUE                      
         MVC   PBIREC+11(2),WORK+4                                              
         MVI   PBIREC+13,C'/'                                                   
         MVC   PBIREC+14(2),WORK+6                                              
         MVI   PBIREC+16,C'/'                                                   
         MVC   PBIREC+17(4),WORK                                                
         MVI   PBIREC+21,C','            DELIMITER                              
*                           2 POSITIONS LINE NUMBER WILL GO HERE                
         MVC   PBIREC+22(10),DINVFULL   THEY SEEM TO USE SHORTER INV            
         CLI   DINVFULL+9,C' '             SEE IF BIG                           
         BH    JWPR2                                                            
         MVI   PBIREC+31,C','                                                   
         B     *+8                                                              
JWPR2    MVI   PBIREC+32,C','            DELIMITER                              
*                                        LINE NUMBER WILL BE HERE               
         MVI   PBIREC+36,C','                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,WORK)  RUN DATE                      
         MVC   BRUNMTH,WORK              SAVE RUN MONTH FOR LATER               
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,PBIREC+37)                           
         MVI   PBIREC+43,C','            DELIMITER                              
         MVC   PBIREC+44(6),=C'TRADE,'                                          
*                                                                               
*        AFTER THIS FIELD USE R6                                                
*                                                                               
         LA    R6,PBIREC+50                                                     
         CLI   QMEDIA,C'T'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(7),=C'MOBILE,'                                         
         LA    R6,PBIREC+51                                                     
         CLI   QMEDIA,C'B'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(11),=C'DIG. AUDIO,'                                    
         LA    R6,PBIREC+55                                                     
         CLI   QMEDIA,C'D'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(7),=C'SOCIAL,'                                         
         LA    R6,PBIREC+51                                                     
         CLI   QMEDIA,C'L'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(7),=C'SEARCH,'                                         
         LA    R6,PBIREC+51                                                     
         CLI   QMEDIA,C'S'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(8),=C'OUTDOOR,'                                        
         LA    R6,PBIREC+52                                                     
         CLI   QMEDIA,C'O'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(9),=C'MAGAZINE,'                                       
         LA    R6,PBIREC+53                                                     
         CLI   QMEDIA,C'M'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(10),=C'NEWSPAPER,'                                     
         LA    R6,PBIREC+54                                                     
         CLI   QMEDIA,C'N'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(11),=C'NAT. VIDEO,'                                    
         LA    R6,PBIREC+55                                                     
         CLI   QMEDIA,C'V'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(11),=C'LOC. VIDEO,'                                    
         LA    R6,PBIREC+55                                                     
         CLI   QMEDIA,C'W'                                                      
         BE    JWTR3                                                            
         MVC   PBIREC+44(12),=C'INTERACTIVE,'                                   
         LA    R6,PBIREC+56                                                     
         CLI   QMEDIA,C'I'                                                      
         BE    JWTR3                                                            
         DC    H'0'      UNKNOWN MEDIA                                          
*                                                                               
JWTR3    DS    0H                                                               
*                                                                               
         MVC   0(3,R6),PBILKCLT                                                 
         CLI   PBILKCLT+2,C' '    SEE IF 2 CHARACTER CLIENT CODE                
         BH    JWTR3C                                                           
         MVI   2(R6),C','                                                       
         LA    R6,3(R6)                                                         
         B     JWTR3D                                                           
*                                                                               
JWTR3C   MVI   3(R6),C','         DELIMITER                                     
         LA    R6,4(R6)                                                         
JWTR3D   MVC   0(20,R6),JWCLTNAM                                                
         LA    R6,20(R6)                                                        
JWTR4    CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR6                                                            
         BCT   R6,JWTR4                                                         
JWTR6    MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVC   0(3,R6),PBILKPRD                                                 
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '      CHECK FOR 2 CHARACTER PRD CODE                   
         BE    *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         MVC   0(20,R6),JWPRDNAM                                                
         LA    R6,20(R6)                                                        
JWTR8    CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR10                                                           
         BCT   R6,JWTR8                                                         
JWTR10   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
         MVI   3(R6),C','       DELIMITER                                       
         LA    R6,4(R6)                                                         
*                                                                               
         MVC   0(3,R6),BRUNMTH    RUN MONTH                                     
         MVI   3(R6),C','         DELIMITER                                     
         LA    R6,4(R6)                                                         
*                                                                               
         MVI   0(R6),C'0'                                                       
         TM    PBILCMSW,X'20'  SEE IF AN AOR BILL                               
         BNZ   JWTR14          DON'T REPORT AMT DUE HERE                        
*                                                                               
         ZAP   MYDUB,PBILLRCV                                                   
         AP    MYDUB,MYGST     INCLUDE TAXES                                    
         AP    MYDUB,MYPST                                                      
*                                                                               
         CP    MYDUB,=P'0'                                                      
         BE    JWTR14                                                           
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR12   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR14                                                           
         BCT   R6,JWTR12                                                        
JWTR14   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVI   0(R6),C'0'                                                       
         TM    PBILCMSW,X'20'  SEE IF AN AOR BILL                               
         BNZ   JWTR18          DON'T REPORT NET HERE                            
         CP    PBILLNET,=P'0'                                                   
         BE    JWTR18                                                           
         EDIT  PBILLNET,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                         
         LA    R6,12(R6)                                                        
JWTR16   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR18                                                           
         BCT   R6,JWTR16                                                        
JWTR18   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVI   0(R6),C'0'                                                       
         ZAP   MYDUB,PBILLRCV                                                   
         AP    MYDUB,MYGST     INCLUDE TAXES                                    
         AP    MYDUB,MYPST                                                      
*                                                                               
         ZAP   DOUBLE,PPBVEBC     EFFECTIVE CASH DISC                           
*                                                                               
         TM    PBILCMSW,X'20'  SEE IF AN AOR BILL                               
         BNZ   JWTR19          SHOW AMOUNT DUE AS COMISSION                     
         ZAP   MYDUB,PBILLRCV        RECIEVABLE                                 
******   AP    MYDUB,MYGST     INCLUDE TAXES                                    
******   AP    MYDUB,MYPST                                                      
         SP    MYDUB,PBILLNET        LESS (G-AC-CD)                             
         TM    PBILBASA,X'04'      TEST CD REFLECTED IN RCVBL                   
         BNZ   *+10                YES - OK                                     
         SP    MYDUB,DOUBLE          ELSE SUBTRACT CD                           
JWTR19   CP    MYDUB,=P'0'                                                      
         BE    JWTR22                                                           
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR20   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR22                                                           
         BCT   R6,JWTR20                                                        
JWTR22   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                               ZERO AOR FIELDS FOR NOW                         
                                                                                
JWTR23   MVI   0(R6),C'0'                                                       
         MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         TM    PBILCMSW,X'20'  SEE IF AN AOR BILL                               
         BZ    JWTR23X     IF SO SHOW NET IN AOR NET FIELD                      
         MVI   0(R6),C'0'                                                       
         CP    PBILLRCV,=P'0'                                                   
         BE    JWTR23A         REVERSE SIGN OF AMT DUE                          
         ZAP   DUB,PBILLRCV    FOR THIS FIELD                                   
         AP    DUB,MYGST       INCLUDE TAXES                                    
         AP    DUB,MYPST                                                        
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         CVD   R0,MYDUB                                                         
         EDIT  MYDUB,(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,12(R6)                                                        
JWTR23A  CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR23B                                                          
         BCT   R6,JWTR23A                                                       
JWTR23B  MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         B     JWTR23XX                                                         
                                                                                
*                                                                               
JWTR23X  MVI   0(R6),C'0'                                                       
         MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                               DOUBLE HAS CASH DISC                            
JWTR23XX MVI   0(R6),C'0'                                                       
         CP    DOUBLE,=P'0'                                                     
         BE    JWTR26                                                           
*                                                                               
         EDIT  (P8,DOUBLE),(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                      
         LA    R6,12(R6)                                                        
JWTR24   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR26                                                           
         BCT   R6,JWTR24                                                        
JWTR26   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
*                                                                               
         MVC   0(5,R6),=C'0,0,0'         3 EMTPY FIELDS AT END                  
*                                                                               
         LA    R6,5(R6)                                                         
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   JWPR50                                                           
*                                                                               
         MVI   0(R6),C','      DELIMITER                                        
         LA    R6,1(R6)                                                         
*                                                                               
         SP    MYPST,MYHST      REMOVE MYHST FROM MYPST                         
*                                                                               
         CP    MYPST,=P'0'     CHECK FOR QST (WORKS FOR NOW)                    
         BE    JWTR30                                                           
         ZAP   DOUBLE,MYGST       COMBINE GST AND PST (QST)                     
         AP    DOUBLE,MYPST                                                     
         EDIT  (P8,DOUBLE),(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                      
         LA    R6,12(R6)                                                        
JWTR28   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR29                                                           
         BCT   R6,JWTR28                                                        
JWTR29   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         MVC   0(3,R6),=C'QST'                                                  
         B     JWPR50                                                           
*                                                                               
JWTR30   ZAP   DOUBLE,MYGST     ONLY MYGST OR MYHST                             
         AP    DOUBLE,MYHST     SHOULD BE NON-ZERO (NOT BOTH)                   
         EDIT  (P8,DOUBLE),(12,0(R6)),2,ALIGN=LEFT,FLOAT=-                      
         LA    R6,12(R6)                                                        
JWTR32   CLI   0(R6),C' '       SCAN BACKWQRDS FOR NON-SPACE                    
         BH    JWTR34                                                           
         BCT   R6,JWTR32                                                        
JWTR34   MVI   1(R6),C','       DELIMITER                                       
         LA    R6,2(R6)                                                         
         MVC   0(3,R6),=C'GST'                                                  
         CP    MYHST,=P'0'     ANY HST PRESENT                                  
         BE    JWPR50                                                           
         MVC   0(3,R6),=C'HST' REPORT AS HST                                    
         B     JWPR50                                                           
*                                                                               
*                             NOTE - NO COMMA AFTER LAST FIELD                  
*                                                                               
JWPR50   DS    0H                                                               
         B     JWLAR1           PATCH OUT FOR TESTING                           
*                                                                               
*        FILE RECORDS CREATED AT RUNLAST                                        
*                                                                               
*        RECORDS HERE ARE JUST GOING TO BINSRCH                                 
*                                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   JWLAR1                                                           
         CP    COUNT,=P'50'                                                     
         BH    JWLAR1                                                           
         AP    COUNT,=P'1'                                                      
         MVC   P(18),=C'***BILLING RECORD'                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBILLKEY,P,60,0                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBILLKEY+60,P,60,0                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(15),=C'***TAPE RECORD'                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(100),PBIREC                                                    
         MVC   PSECOND(80),PBIREC+100                                           
         GOTO1 REPORT                                                           
         MVC   P(21),=C'***TAPE RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC,P,60,0                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+60,P,60,0                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIREC+120,P,60,0                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     *+6                                                              
COUNT    DC    PL2'0'                                                           
*                                                                               
*                                                                               
*                                                                               
JWLAR1   DS    0H                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    JWTMED,PBILKMED                                                 
         MVC    JWTCLI,PBILKCLT                                                 
         MVC    JWTINVMO,PBILKBMN+1    BILLING MONTH                            
         MVC    JWTINVN,PBILKBNO                                                
         MVC    JWTMOS,PBILKMOS                                                 
         MVC    JWTPRO,PBILKPRD                                                 
         MVC    JWTEST,PBILKEST                                                 
*                                                                               
         MVC    JWTREC,PBIREC                                                   
*                                                                               
         L      R2,AOFJWTT        ADDRESS OF JWTTAB                             
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),BINVALS                                              
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS   SHOULD NOT HAPPEN                          
*                                                                               
         DC      H'0'                                                           
**       L       RF,BINVALS               ADDRESS OF FOUND RECORD               
**       LA      RF,L'JWTKEY(RF)          PAST KEY                              
**       AP      45(9,RF),PBIREC+45(9)     CD                                   
**       AP      34(11,RF),PBIREC+34(11)     GROSS                              
**       AP      54(11,RF),PBIREC+54(11)                                        
**       AP      65(11,RF),PBIREC+65(11)                                        
**       AP      87(9,RF),PBIREC+87(9)      AOR FEE                             
*                                                                               
GOTOXIT  DS    0H                                                               
         MVC   BINVALS,=A(JWTMED)                                               
         MVI   BINVALS,1                                                        
         XIT1                                                                   
*                                                                               
JWPRL    DS    0H                                                               
*                                                                               
         L     R2,JWTRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROX               NO OPEN NOR CLOSE NEEDED                     
*                                                                               
         CLI   SVQOPT7,C'Y'       TEST RUN - NO TAPE                            
         BE    JWPRL5                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OPEN  (JWPBIT1,(OUTPUT))                                               
JWPRL5   DS    0H                                                               
         L     R2,JWTRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        FIRST CREATE AND SEND HEADER RECORD                                    
*                                                                               
         MVI   PBIREC,C' '                                                      
         MVC   PBIREC+1(L'PBIREC-1),PBIREC                                      
*                                                                               
         MVC   PBIREC(8),=C'HDR,USD,'                                           
         CLI   CAD$SW,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PBIREC+4(3),=C'CAD'                                              
*                                                                               
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   *+10                                                             
         MVC   PBIREC+4(3),=C'CAD'                                              
*                                                                               
         LA    R6,PBIREC+8                                                      
         EDIT  (B4,JWTRECNT),(5,0(R6)),0,ALIGN=LEFT                             
         LA    R6,5(R6)                                                         
JWPRL2   CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL4                                                           
         BCT   R6,JWPRL2                                                        
JWPRL4   MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         EDIT  (B4,TRUNINVS),(5,0(R6)),0,ALIGN=LEFT                             
         LA    R6,5(R6)                                                         
JWPRL6   CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL8                                                           
         BCT   R6,JWPRL6                                                        
JWPRL8   MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
*                                                                               
         ZAP   MYDUB,TRUNTOTS+24(6) TOTAL AMOUNT DUE                            
*                                                                               
         SP    MYDUB,JWAORDUE       REMOVE AOR AMTS                             
*                                                                               
         MVI   0(R6),C'0'                                                       
         CP    MYDUB,=P'0'                                                      
         BE    JWPRL12                                                          
         EDIT  MYDUB,(13,0(R6)),2,ALIGN=LEFT,FLOAT=-                            
         LA    R6,13(R6)                                                        
JWPRL10  CLI   0(R6),C' '          SCAN BACKWARD FOR NON-SPACE                  
         BH    JWPRL12                                                          
         BCT   R6,JWPRL10                                                       
JWPRL12  DS    0H                   NOTE - NO COMMA AFTER LAST FIELD            
*                                                                               
*        SET RECORD LENGTH IN PBIRECL HERE                                      
*                                                                               
         CLI   SVQOPT7,C'Y'        DUMPING OUTPUT                               
         BNE   JWPRL15                                                          
         MVC   P(19),=C'** HEADER RECORD **'                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(100),PBIREC                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(16),=C'** HEADER HEX **'                                       
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,PBIRECL,P,60,0                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         B     JWPRL20                                                          
*                                                                               
JWPRL15  LA    R1,JWPBIT1                                                       
         LA    R0,PBIREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
*                                  DETAILS                                      
JWPRL20  DS    0H                                                               
         XC    JLASTINV,JLASTINV                                                
         ZAP   ITMCNT,=P'0'                                                     
         L     R3,AOFJWTT                                                       
         LA    R3,L'JWTKEY(R3)     BUMP PAST PSUEDO KEY                         
JWTAGN   DS    0H                                                               
**NEW 1/12/89                                                                   
JWTAGN5  DS    0H                                                               
         SH    R3,=H'14'         BACK UP R3 TO KEY                              
         CLC   0(7,R3),JLASTINV  SAME INVOICE #?  MED/CLT/INV                   
         BE    JWTAGN5B                                                         
         ZAP   ITMCNT,=P'1'        RESET TO 1                                   
         MVC   JLASTINV,0(R3)      SAVE MED/CLT/INV                             
         AH    R3,=H'14'           RE-BUMP PAST KEY                             
         B     JWTAGN5C                                                         
*                                                                               
JWTAGN5B DS    0H                                                               
         AH    R3,=H'14'           RE-BUMP PAST KEY                             
         AP    ITMCNT,=P'1'                                                     
JWTAGN5C UNPK  33(3,R3),ITMCNT                                                  
***      CLI   33(R3),C'0'     CODE TO CLEAR LEADING ZEROS                      
***      BNE   *+8             (NO-OPED)                                        
***      MVI   33(R3),C' '     NO LEADING ZERO                                  
***      CLI   34(R3),C'0'                                                      
***      BNE   *+8                                                              
***      MVI   34(R3),C' '     NO LEADING ZERO                                  
         OI    35(R3),X'F0'                                                     
***                                                                             
***      HERE IS WHERE I MAY NEED TO SHIFT RECORD DATA                          
***      IF THEY CAN'T ACCEPT LEADING ZEROS NOR BLANKS                          
***                                                                             
         MVC   SAVREC(180),0(R3)  SAVE ORIGINAL RECORD                          
         CLI   33(R3),C'0'    ITEM NUMBER OVER 99?                              
         BH    JWTAGN6                                                          
         MVC   33(180-33,R3),SAVREC+34                                          
         MVI   179(R3),C' '                                                     
         CLI   33(R3),C'0'    ITEM NUMBER OVER 9?                               
         BH    JWTAGN6                                                          
         MVC   33(180-34,R3),SAVREC+35                                          
         MVI   178(R3),C' '                                                     
*                                                                               
JWTAGN6  DS    0H                                                               
         CLI   SVQOPT7,C'Y'        TEST RUN - NO TAPE                           
         BE    JWTAGN7                                                          
*                                                                               
         LA    R1,JWPBIT1                                                       
         PUT   (1),(3)                                                          
*                                                                               
JWTAGN7  DS    0H                                                               
         CLI   SVQOPT7,C'Y'                                                     
         BNE   BINGO                                                            
         CP    COUNT,=P'100'                                                    
         BH    BINGO                                                            
         AP    COUNT,=P'1'                                                      
         MVC   P(9),=C'***OUTPUT'                                               
         GOTO1 REPORT                                                           
         MVC   P(75),0(R3)                                                      
         MVC   PSECOND(75),75(R3)                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(14),=C'***OUTPUT- HEX'                                         
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,(R3),P,65,0                                          
         GOTO1 REPORT                                                           
         LA    RF,65(R3)                                                        
         GOTO1 HEXOUT,DMCB,(RF),P,65,0                                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    RF,130(R3)                                                       
         GOTO1 HEXOUT,DMCB,(RF),P,50,0                                          
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
BINGO    LA    R3,194(R3)   TO NEXT RECORD IN TABLE                             
         BCT   R2,JWTAGN                                                        
*                                                                               
ZEROCNT  DS    0H                                                               
*                                                                               
         CLI   SVQOPT7,C'Y'     TEST RUN - NO TAPE                              
         BE    ZEROX                                                            
*                                                                               
         LA    R2,JWPBIT1                                                       
         CLOSE ((2),)                                                           
ZEROX    B     GOTOXIT                                                          
*                                                                               
JWPBIT1  DCB   DDNAME=PJWTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00180,                                            X        
               BLKSIZE=00180,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         LTORG                                                                  
ITMCNT   DS    PL2                                                              
JLASTINV DS    CL7                                                              
         EJECT                                                                  
*                                                                               
NEXTELM  NTR1  BASE=*,LABEL=*                                                   
NEXTEL   ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELMX                                                         
         CLC   0(1,R2),ELCODE                                                   
         BE    *+10                                                             
         B     NEXTEL                                                           
NEXTELMX LTR   RE,RE       NOT FOUND -                                          
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(JWTMED)        RECORD TO BE ADDED                            
         DC    A(JWTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
JWTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'194'             LEN OF RECORD                                 
         DC    F'14'              KEY SIZE                                      
         DC    F'3000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFJWTT  DC    A(JWTTABLE)                                                      
JWTKEY   DS    0XL14                                                            
JWTMED   DS    CL1                                                              
JWTCLI   DS    CL3                                                              
JWTINUMB DS    0XL3                                                             
JWTINVMO DS    XL1           INVOICE MONTH                                      
JWTINVN  DS    XL2           INVOICE NUMBER                                     
JWTPRO   DS    CL3                                                              
JWTEST   DS    XL2                                                              
JWTMOS   DS    XL2           MONTH OF SERVICE                                   
*            ______                                                             
*              14                                                               
JWTREC   DS    CL180                                                            
ENDJWTR  DS    0C                                                               
***********************                                                         
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
NEXTELD  NTR1  BASE=*,LABEL=*                                                   
NEXTELD1 ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELDX                                                         
         CLC   0(1,R2),ELCODE                                                   
         BE    *+10                                                             
         B     NEXTELD1                                                         
NEXTELDX LTR   RE,RE       NOT FOUND -                                          
*                                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'DSECTS AND WORK AREAS'                                          
PPJWWRKD DSECT                                                                  
PPJWWRK  DS    0C                                                               
AOUTP    DS    A                                                                
VOFFICER DS    A                                                                
RELO     DS    A                                                                
*                                                                               
LOWINV   DS    H           START INVOICE NUMBER                                 
HIINV    DS    H           END INVOICE NUMBER                                   
*                                                                               
CAD$SW   DS    CL1         SET TO Y FOR IDS- JACTCDN OR JGEOCDN                 
*                          (16216+17004) GET CAD INSTEAD OF USD                 
*                                                                               
START    DS    CL6                                                              
END      DS    CL6                                                              
ZEROS    DS    CL30                                                             
DYNSW    DS    CL1                                                              
         DS    0D                                                               
JWCOUNT  DS    PL8                                                              
JWTOTAL  DS    PL8                                                              
JWAORDUE DS    PL8                                                              
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
*                                                                               
FIRSTSW  DS    X                                                                
BRUNMTH  DS    CL3                                                              
SAVCOFF  DS    CL2         OFFICE CODE (FROM OFFICER CALL)                      
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
         DS    CL22                NOT SPARE - REST OF LASTKEY                  
*                                                                               
TESTMQ   DS    C                                                                
BKSFTP   DS    C                   Y=DOING BURGER KING SFTP                     
*                                  FOR MINDSHARE BURGER KING MQ MSG             
BKOPENSW DS    C                   Y=BURGER KING FILE OPEN                      
BKFISCAL DS    CL4                 FISCAL YEAR                                  
BKBYR    DS    CL4                 BILLING MOS - YEAR                           
BKBMTH   DS    CL2                 BILLING MOS - MONTH                          
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
*                                                                               
*                                                                               
JWCLTNAM DS    CL20                CLT NAME WITHOUT COMMAS                      
JWPRDNAM DS    CL20                PRD NAME WITHOUT COMMAS                      
*                                                                               
SAVREC   DS    CL180               SAVE OF ORIGINAL RECORD                      
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                    FOR MINDSHARE BURGER KING AGID = H7PK                      
*                                                                               
         DS    CL22                                                             
*                                                                               
LASTPROF DS    XL16                                                             
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
DINVMED  DS    CL2                  INVOICE MEDIA PART                          
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                 FULL INVOICE NUMBER                         
*                                   ME-MN-NNNN                                  
*                               OR  MN-ME-NNNN                                  
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
MCMRECS  DS    PL2'0'                                                           
OORECS   DS    PL2'0'                                                           
*                                                                               
FISCAL   DS    CL4                 BK FISCAL YEAR                               
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
MYFULL   DS    F                                                                
SAVER1   DS    F                                                                
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
SVQOPT7  DS    CL1                                                              
NEWCLT   DS    CL1                                                              
ELCODE   DS    CL1                                                              
SH9PRD   DS    CL3                                                              
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
*                                                                               
SVEDWARD DS    CL60     SAVED $ AMTS FOR EDWARD JONES TRAILER RECORD            
*                       (OOPROCE)                                               
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
*                                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
         DS    0H               FOR ALIGNMENT                                   
PBIRECL  DS    CL2              RECORD LENGTH                                   
PBIREC   DS    CL200                                                            
*                                                                               
WID      DSECT                                                                  
WIOFFICE DS    CL1           1                                                  
WISYSTEM DS    CL1           2                                                  
WIMEDIA  DS    CL1           3                                                  
WICLICOD DS    CL3           4                                                  
WICLINAM DS    CL20          7                                                  
         DS    CL10                                                             
WIPRDCOD DS    CL3           37                                                 
WIESTNUM DS    CL3           40                                                 
         DS    CL3                                                              
WIESTNAM DS    CL20          46                                                 
         DS    CL15                                                             
WIINVNUM DS    CL6           81                                                 
         DS    CL4                                                              
WIINVDAT DS    CL6           91                                                 
WIDUEDAT DS    CL6           97                                                 
WIMONSER DS    CL4                                                              
WIACTBAM DS    CL11                                                             
WICASH   DS    CL11                                                             
WITAX    DS    CL11                                                             
WIGST    DS    CL11                                                             
WIPST    DS    CL11                                                             
WIAGENCY DS    CL2                                                              
WINET    DS    CL11                                                             
WISUBMED DS    CL1                                                              
WIPW#ACT DS    CL11                                                             
WIPW#NET DS    CL11                                                             
WIPW#TAX DS    CL11                                                             
WIPWBILL DS    CL1                                                              
WIAORNAM DS    CL30                                                             
WIPRDGCD DS    CL4                                                              
WIPRDGNM DS    CL24                                                             
WIUDEFP1 DS    CL32                                                             
WIUDEFP2 DS    CL16                                                             
WIUDEFE1 DS    CL32                                                             
WIUDEFE2 DS    CL16                                                             
WIBILLTP DS    CL3                                                              
         DS    CL1                                                              
WICLTNUM DS    CL6                                                              
         DS    CL2                                                              
WIARDAT  DS    CL4                                                              
WIGROSS  DS    CL11                                                             
WICLIPRD DS    CL5                                                              
WIPRDDES DS    CL20                                                             
WIAGYCOM DS    CL11                                                             
WIRETAIL DS    CL12                                                             
*                                                                               
PM50     DSECT                                                                  
PM50VEN  DS    CL6      VENDOR                                                  
PM50MED  DS    CL2      MEDIA                                                   
PM50TYP  DS    CL1      TYPE                                                    
PM50INV  DS    CL13     INVOICE NUMBER                                          
PM50EST  DS    CL12     ESTIMATE                                                
PM50FLL  DS    CL9      SPACES                                                  
PM50RTP  DS    CL2      RECORD TYPE                                             
PM50CGA  DS    PL6                                                              
PM50CDS  DS    PL6                                                              
PM50NCA  DS    PL6                                                              
PM50NCD  DS    PL6                                                              
PM50PSF  DS    PL6                                                              
*                                                                               
PM55     DSECT                                                                  
PM55VEN  DS    CL6      VENDOR                                                  
PM55MED  DS    CL2      MEDIA                                                   
PM55TYP  DS    CL1      TYPE                                                    
PM55INV  DS    CL13     INVOICE NUMBER                                          
PM55EST  DS    CL12     ESTIMATE                                                
PM55FLL  DS    CL6      SPACES                                                  
PM55PRD  DS    CL3                                                              
PM55RTP  DS    CL2      RECORD TYPE                                             
PM55CGA  DS    PL6                                                              
PM55CDS  DS    PL6                                                              
PM55NCA  DS    PL6                                                              
PM55NCD  DS    PL6                                                              
PM55PSF  DS    PL6                                                              
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
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
*                                                                               
* START AND END INVOIVE NUMBERS COL 53(4), AND COL 57 (4)                       
*                                                                               
QINVNO1  EQU   QRECORD+52                                                       
QINVNO2  EQU   QRECORD+56                                                       
*                                                                               
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DSECT                      
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
       ++INCLUDE DDOFFICED                                                      
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
JWTTABLE CSECT                                                                  
         DS    3000CL194                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069PPREPJW02 07/18/18'                                      
         END                                                                    
