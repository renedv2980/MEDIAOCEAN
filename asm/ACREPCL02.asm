*          DATA SET ACREPCL02  AT LEVEL 041 AS OF 05/01/02                      
*PHASE ACCL02A,*                                                                
*INCLUDE PRINT                                                                  
         TITLE 'ACCL02 - CLIENT INVESTMENTS REPORT'                             
ACCL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCL**,R9,R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         LA    RC,SPACEND                                                       
         USING ACCLD,RC                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              RUN INITIALIZATION                                               
*--------------------------------------------------------------------*          
         CLI   MODE,RUNFRST                                                     
         BNE   REQF100                                                          
         L     RF,=A(BUFFALOC)            SET UP ALL ADDRESSABILITY             
         ST    RF,ABUFF                                                         
         L     RF,=A(ACBUFF)                                                    
         ST    RF,ACCBUFF                                                       
         LA    RF,IOAREA1                                                       
         ST    RF,AIO1                                                          
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         L     RF,GETOPT                  NOOP GETOPT (BCR OPCODE)              
         MVC   0(2,RF),=X'07FE'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              REQUEST INITIALIZATION                                           
*--------------------------------------------------------------------*          
REQF100  CLI   MODE,REQFRST                                                     
         BNE   LEDG100                                                          
         MVC   SVCOMP,QCOMPANY                                                  
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF       SET BUFFALO                     
         MVI   RCSUBPRG,0                       01 SPROG                        
         MVC   PAGE,=H'1'                       PAGE COUNTER                    
         MVI   FORCEHED,C'Y'                    NEW PAGE                        
*                                                                               
         MVC   QENCH(4),QMOSEND                                                 
         MVC   QENCH+4(2),=C'01'                                                
         GOTO1 DATCON,DMCB,(0,QENCH),(1,QENPK)                                  
         GOTO1 DATCON,DMCB,(0,QENCH),(6,HEADDT)                                 
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        FIRST FOR LEDGER - GET NAME AND ACCOUNT LEVEL LENGTHS                  
*--------------------------------------------------------------------*          
LEDG100  CLI   MODE,LEDGFRST                                                    
         BNE   LEVA100                                                          
         USING LDGRECD,R2                                                       
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SR'                                                   
         BNE   XIT                                                              
LED130   L     R4,ADLDGHIR           GET ACCOUNT LEVELS                         
         USING ACLELD,R4                                                        
         LA    R3,ACLVALS                                                       
         LA    R2,SLEVA                                                         
         LA    R5,4                                                             
LED140   MVC   0(L'SLEVA,R2),0(R3)                                              
         LA    R3,16(R3)                                                        
         LA    R2,L'SLEVA(R2)                                                   
         BCT   R5,LED140                                                        
         B     XIT                                                              
         SPACE 2                                                                
*--------------------------------------------------------------------*          
*        IF SR AND LEVEL B SAVE NAME TO PUT ON BUFFRECS                         
*--------------------------------------------------------------------*          
LEVA100  CLI   MODE,LEVAFRST                                                    
         BNE   LEVB100                                                          
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRA                                                       
         MVC   SVANAME,SPACES                                                   
         USING NAMELD,R4                                                        
         L     R4,ADLVANAM                                                      
         ZIC   R3,NAMLN                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVANAME(0),NAMEREC                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        IF SR AND LEVEL B SAVE NAME TO PUT ON BUFFRECS                         
*--------------------------------------------------------------------*          
LEVB100  CLI   MODE,LEVBFRST                                                    
         BNE   LEVC100                                                          
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRB                                                       
         MVC   SVBNAME,SPACES                                                   
         USING NAMELD,R4                                                        
         L     R4,ADLVBNAM                                                      
         ZIC   R3,NAMLN                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVBNAME(0),NAMEREC                                               
         MVC   SVDEPT,4(R2)                                                     
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        IF SR AND LEVEL C SAVE NAME TO PUT ON BUFFRECS                         
*--------------------------------------------------------------------*          
LEVC100  CLI   MODE,LEVCFRST                                                    
         BNE   PHIS100                                                          
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRC                                                       
         MVC   SAVKEY,0(R2)                                                     
         MVC   SVCNAME,SPACES                                                   
         USING NAMELD,R4                                                        
         L     R4,ADLVCNAM                                                      
         ZIC   R3,NAMLN                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SVCNAME(0),NAMEREC                                               
*                                                                               
         L     R2,ADHEIRC                TO INSURE ALL CLIENTS SHOW UP          
         ZAP   WKDEB,=P'0'               SEND OUT ZERO REC AT CLIENT            
         ZAP   WKCRED,=P'0'              LEVEL                                  
         BAS   RE,PUTSR                                                         
*                                                                               
*              READ SJ BALANCES FOR THIS SR CLIENT                              
*                                                                               
         L     R3,ADHEIRC                SR CLIENT LEVEL RECORD                 
         XC    MYKEY,MYKEY               CLEAR AND BUILD KEY                    
         MVC   MYKEY(33),SPACES                                                 
         MVC   SVCLI,6(R3)                                                      
         MVC   MYKEY(1),0(R3)                                                   
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(3),SVCLI                                                 
         MVC   COMMAND,DMRDHI                                                   
LEVC130  GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,AIO1                      
         MVC   COMMAND,DMRSEQ                                                   
         L     R2,AIO1                                                          
         L     R3,ADHEIRC                SR CLIENT LEVEL RECORD                 
         CLC   0(1,R2),0(R3)                                                    
         BNE   LEVCXIT                                                          
         CLC   1(2,R2),=C'SJ'                                                   
         BNE   LEVCXIT                                                          
         CLC   3(3,R2),SVCLI              IF NOT THIS CLIENT - EXIT             
         BH    LEVCXIT                                                          
*                                                                               
         LR    R3,R2                      NEED TO STORE DEPT THIS CLI           
         MVI   ELCODE,PPRELQ              IN LINKED TO - TO AVOID DUPS          
         BAS   RE,GETEL                   ON WIP BALANCE - GET FROM             
         BNE   LEVC135                    RECEIVABLE ACCT ON PRODUCTION         
         USING PPRELD,R3                  PROFILE                               
         CLC   PPRRECVA,SPACES                                                  
         BNH   LEVC130                                                          
         MVC   SJDEPT,PPRRECVA+1                                                
         B     LEVC130                                                          
*                                                                               
LEVC135  DS    0H                                                               
         LR    R3,R2                                                            
         USING TRSEL,R3                                                         
         MVI   ELCODE,TRSELQ              GET BALANCE ELEMENTS                  
         BAS   RE,GETEL                                                         
         BNE   LEVC137                                                          
         CLC   TRSPMOS,QENPK              COMPARE TO REQUEST END                
         BH    LEVC130                    IF HIGH - SENT TO BUFFALO             
*                                                                               
LEVC137  LR    R3,R2                                                            
         MVI   ELCODE,TRNELQ              GET BALANCE ELEMENTS                  
         BAS   RE,GETEL                                                         
         BNE   LEVC130                                                          
         ZAP   WKDEB,=P'0'                INITIALIZE AMOUNTS                    
         ZAP   WKCRED,=P'0'                                                     
         USING TRNELD,R3                                                        
LEVC140  DS    0H                                                               
         CLC   SVDEPT,SJDEPT                                                    
         BNE   LEVC130                                                          
LEVC142  LA    R4,WKDEB                                                         
         TM    TRNSTAT,X'80'                                                    
         BO    *+8                                                              
         LA    R4,WKCRED                                                        
         AP    0(L'WKDEB,R4),TRNAMNT      ELSE ACCUMULATE                       
*                                                                               
LEVC150  DS    0H                                                               
         BAS   RE,PUTSJ                   PUT SJ RECORD TO BUFFALO              
         B     LEVC130                                                          
*                                                                               
LEVCXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PROCESS HISTORY RECORDS                                                
*--------------------------------------------------------------------*          
PHIS100  CLI   MODE,PROCSBAC                                                    
         BNE   REQL010                                                          
*                                                                               
         ZAP   WKDEB,=P'0'                INITIALIZE TEMP AMOUNTS               
         ZAP   WKCRED,=P'0'                                                     
         L     R2,ADSUBAC                 ONLY NEED TO PROCESS SR               
         USING ACTRECD,R2                 RECORDS                               
         CLC   1(2,R2),=C'SR'                                                   
         BNE   PHISXIT                                                          
*                                                                               
         USING BUKELD,R3                                                        
PHIS110  LR    R3,R2                      GET '45' BUCKET ELEMNETS              
         MVI   ELCODE,BUKELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   PHISXIT                    IF NONE EXIT                          
PHIS115  CLC   BUKMOS,QENPK               WITHIN REQUEST PARAMETERS?            
         BH    PHIS120                                                          
         AP    WKDEB,BUKDR                ACCUMULATE AMOUNTS                    
         AP    WKCRED,BUKCR                                                     
         BAS   RE,NEXTEL                  GET NEXT BUCKET ELEMENT               
         BE    PHIS115                                                          
*                                                                               
PHIS120  DS    0H                         PUT OUT SORT RECORD                   
         BAS   RE,PUTSR                                                         
*                                                                               
PHISXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ALL RECORD PROCESSING DONE AT LAST FOR REQUEST                         
*--------------------------------------------------------------------*          
REQL010  CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
*                                                                               
         XC    BUFAREA,BUFAREA         INITIALIZE ALL SAVE FIELDS               
         XC    SVLOC,SVLOC                                                      
         XC    SVDEPT,SVDEPT                                                    
         XC    SVCLI,SVCLI                                                      
         XC    SVANAME,SVANAME                                                  
         XC    SVBNAME,SVBNAME                                                  
         XC    SVCNAME,SVCNAME                                                  
         MVI   TOTSW,0                                                          
*                                                                               
*              INITIALIZE ALL ACCUMULATORS                                      
         LA    R3,CURACUM              CURRENT LOW LEVEL ACCOUNT                
         BAS   RE,CLRACUM                                                       
         LA    R3,CLNACUM              CLIENT TOTALS                            
         BAS   RE,CLRACUM                                                       
         LA    R3,DEPACUM              DEPARTMENT TOTALS                        
         BAS   RE,CLRACUM                                                       
         LA    R3,LOCACUM              LOCATION TOTALS                          
         BAS   RE,CLRACUM                                                       
         LA    R3,REPACUM              REPORT TOTALS                            
         BAS   RE,CLRACUM                                                       
*                                                                               
         MVC   BFCOMP,SVCOMP                                                    
         MVC   BFUL,=C'SR'                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFAREA,1                            
         B     REQL075                                                          
*                                                                               
REQL050  GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFAREA,1                            
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFAREA,1                             
REQL075  MVC   BUFSAVE,BUFAREA                                                  
         TM    DMCB+8,X'80'            WAS READ SUCCESSFUL                      
         BO    REQL900                                                          
*                                                                               
REQL100  OC    SVCLI,SVCLI             IS THIS FIRST PASS OF REQUEST            
         BZ    REQL400                                                          
         CLC   SVCLI,BFSRCLT           HAS CLIENT CHANGED                       
         BE    *+8                                                              
         OI    TOTSW,CLICH             INDICATE CLIENT CHANGE                   
         CLC   SVDEPT,BFSRDPT          HAS DEPARTMENT BREAK                     
         BE    *+8                                                              
         OI    TOTSW,DEPTCH            INDICATE DEPARTMENT BREAK                
         CLC   SVLOC,BFSRLOC           HAS LOCATION CHANGED                     
         BE    *+8                                                              
         OI    TOTSW,LOCCH             INDICATE LOCATION BREAK                  
*                                                                               
         BAS   RE,TOTCHEK              PRINT APPROPRITE TOTAL BREAKS            
*                                                                               
REQL400  DS    0H                                                               
         MVC   SVCLI,BFSRCLT           SET TO CURRENT CLIENT                    
         MVC   SVDEPT,BFSRDPT                         DEPARTMENT                
         MVC   SVLOC,BFSRLOC                          LOCATION                  
         MVC   SVANAME,BFANAME                                                  
         MVC   SVBNAME,BFBNAME                                                  
         MVC   SVCNAME,BFCNAME                                                  
*                                                                               
         ZAP   CURSR,BFAMT1            SR ACCOUNT BALANCE                       
         SP    CURSR,BFAMT2                                                     
*                                                                               
         ZAP   CURSJ,=P'0'             INITIALIZE                               
         MVC   BFACCT,SPACES           GET CORRESPONDING SJ ACCOUNT             
         MVC   BFUL,=C'SJ'                                                      
         MVC   BFSJCLT,SVCLI                                                    
         MVC   BFSJDEPT,SVDEPT                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFAREA,1                            
         CLC   BFUL,=C'SJ'                                                      
         BNE   REQL450                                                          
         CLC   BFSJCLT,SVCLI                                                    
         BNE   REQL450                                                          
         CLC   BFSJDEPT,SVDEPT                                                  
         BNE   REQL450                                                          
         ZAP   CURSJ,BFAMT1            SJ ACCOUNT BALANCE FOR THIS              
         SP    CURSJ,BFAMT2            SR CLIENT                                
*                                                                               
REQL450  DS    0H                                                               
         ZAP   CURSRSJ,CURSR                                                    
         AP    CURSRSJ,CURSJ                                                    
         MVC   BUFAREA,BUFSAVE         RESTORE SR BUFF REC                      
*              LOOP THROUGH AND READ ALL BUDGETS CORRESPONDING TO THIS          
*              THIRD LEVEL SR ACCOUNT - SEE BUDGET TABLE                        
         LA    R3,BUD1                 BUDGET TABLE                             
         LA    R4,BUDNMS               NUMBER OF BUDGETS IN TABLE               
         LA    R6,CURCRLM              POINT TO ACCUMULATORS                    
REQL460  LA    R2,MYKEY                BUILD BUDGET KEY                         
         XC    MYKEY,MYKEY                                                      
         MVC   0(33,R2),SPACES                                                  
         USING BUDRECD,R2                                                       
         MVI   BUDKTYP,BUDKTYPQ        RECORD TYPE                              
         MVC   BUDKCPY,BFCOMP          COMPANY                                  
         MVC   BUDKUNT(2),BFUL         UNIT/LEDGER                              
         MVC   BUDKACT,BFACCT          ACCOUNT                                  
         MVC   BUDKBUDN,0(R3)          BUDGET NUMBER                            
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MYKEY,AIO1                       
         CLI   DMCB+8,0                RECORD FOUND                             
         BNE   *+8                                                              
         BAS   RE,TOTBUD               TOTAL BUDGETS FOR THIS PERIOD            
         LA    R3,L'BUD1(R3)           BUMP BUDGET TABLE                        
         LA    R6,L'CURBUD(R6)         BUMP ACCUMULATORS                        
         BCT   R4,REQL460                                                       
*                                                                               
REQL500  DS    0H                                                               
         LA    R3,CURACUM              POINT TO CURRENT ACCUMULATORS            
         BAS   RE,PRNTAMT              AND PUT TO PRINT LINE                    
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         MVC   PCLI,BFCNAME            CLIENT NAME                              
         MVC   PCLICD,BFSRCLT          CLIENT CODE                              
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,ROLACUM              ROLL TO HIGHER LEVEL ACCUMS              
         LA    R3,CURACUM              POINT TO AND                             
         BAS   RE,CLRACUM              CLEAR CURRENT ACCUMULATORS               
         B     REQL050                                                          
*                                                                               
REQL900  DS    0H                                                               
         BAS   RE,REPTOT               PRINT REPORT TOTALS                      
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CHECK IF INTERMEDIATE TOTALS NEED BE PRINTED                           
*--------------------------------------------------------------------*          
TOTCHEK  NTR1                                                                   
         TM    TOTSW,LOCCH             IF LOCATION CHANGE - NEED TO             
         BZ    *+12                    PRINT ALL SUB TOTALS                     
         BAS   RE,LOCTOT                                                        
         B     TOTCXIT                                                          
*                                                                               
         TM    TOTSW,DEPTCH            IF DEPT CHANGE - NEED TO PRINT           
         BZ    *+12                    DEPTARMENT SUBTOTALS                     
         BAS   RE,DEPTTOT                                                       
         B     TOTCXIT                                                          
*                                                                               
         TM    TOTSW,CLICH             IF CLI CHANGE - NEED TO PRINT            
         BZ    TOTCXIT                 CLI SUBTOTALS                            
         BAS   RE,CLNTOT                                                        
*                                                                               
TOTCXIT  MVI   TOTSW,0                                                          
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        TOTAL BUDGET AMOUNTS                                                   
*--------------------------------------------------------------------*          
TOTBUD   NTR1                                                                   
         L     R2,AIO1                                                          
         USING BUDRECD,R2                                                       
         LA    R3,49(R2)                                                        
         USING BAMELD,R3                                                        
TOTB30   CLI   0(R3),0                 END OF BUDGET RECORD - EXIT              
         BE    TOTBXIT                                                          
         CLC   BAMMNTH(1),QENPK        ONLY TOTAL BUDGETS FOR REQUEST           
         BNE   TOTB50                  YEAR                                     
         AP    0(8,R6),BAMBUDG         TOTAL BUDGET                             
TOTB50   ZIC   R5,BAMLN                BUMP BUDGET ELEMENT                      
         AR    R3,R5                                                            
         B     TOTB30                                                           
*                                                                               
TOTBXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PRINT REPORT TOTALS                                                    
*--------------------------------------------------------------------*          
REPTOT   NTR1                                                                   
         OI    TOTSW,LOCCH             WILL KEEP FROM PAGING                    
         BAS   RE,LOCTOT               PRINT LAST LOCATION TOTALS               
*                                                                               
         LA    R3,REPACUM              POINT TO REPORT TOTALS AND               
         BAS   RE,PRNTAMT              PUT TO PRINT LINE                        
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         MVC   PCLI(17),=C'**REPORT TOTALS**'                                   
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,REPACUM              CLEAR FOR NEXT RUN                       
         BAS   RE,CLRACUM                                                       
*                                                                               
REPXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PRINT LOCATION TOTALS                                                  
*--------------------------------------------------------------------*          
LOCTOT   NTR1                                                                   
         MVI   FORCEHED,C'N'                                                    
         BAS   RE,CLNTOT               PRINT CLIENT AND                         
         BAS   RE,DEPTTOT              DEPARTMENT TOTALS                        
*                                                                               
         LA    R3,LOCACUM              POINT TO LOCATION TOTALS                 
         BAS   RE,PRNTAMT              AND PUT TO PRINT LINE                    
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         MVC   PCLI(22),=C'**TOTALS FOR LOCATION '                              
         MVC   PCLI+22(L'SVLOC),SVLOC                                           
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,LOCACUM              CLEAR LOCATION TOTALS                    
         BAS   RE,CLRACUM                                                       
*                                                                               
LOCXIT   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PRINT DEPARTMENT TOTALS                                                
*--------------------------------------------------------------------*          
DEPTTOT  NTR1                                                                   
         BAS   RE,CLNTOT               PRINT CLIENT TOTALS                      
*                                                                               
         GOTO1 ACREPORT                SKIP LINE                                
         LA    R3,DEPACUM              POINT TO DEPARTMENT TOTALS               
         BAS   RE,PRNTAMT              AND PUT TO PRINT LINES                   
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         MVC   PCLI(25),=C'**TOTALS FOR DEPTARTMENT '                           
         MVC   PCLI+25(L'SVDEPT),SVDEPT                                         
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,DEPACUM              POINT TO AND CLEAR DEPT                  
         BAS   RE,CLRACUM              TOTALS                                   
*                                                                               
DEPTXIT  DS    0H                                                               
         TM    TOTSW,LOCCH             IF LOCATION TOTALS ARE GOING TO          
         BO    *+8                     BE PRINTED DO NOT SKIP TO A              
         MVI   FORCEHED,C'Y'           NEW PAGE                                 
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PRINT CLIENT TOTALS                                                    
*--------------------------------------------------------------------*          
CLNTOT   NTR1                                                                   
         B     XIT                                                              
**T                                                                             
         MVI   SPACING,2                                                        
         LA    R3,CLNACUM              POINT TO CLIENT TOTALS                   
         BAS   RE,PRNTAMT              AND PUT TO PRINT LINE                    
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         MVC   PCLI(20),=C'**TOTALS FOR CLIENT '                                
         MVC   PCLI+20(L'SVCLI),SVCLI                                           
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,CLNACUM              CLEAR CLIENT TOTALS                      
         BAS   RE,CLRACUM                                                       
*                                                                               
CLNXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT AMOUNTS TO PRINT LINE                                              
*--------------------------------------------------------------------*          
PRNTAMT  NTR1                                                                   
         MVC   XHEAD3+12(L'SVLOC),SVLOC                                         
         MVC   XHEAD3+14+L'SVLOC(L'SVANAME),SVANAME                             
         MVC   XHEAD4+14(L'SVDEPT),SVDEPT                                       
         MVC   XHEAD4+16+L'SVDEPT(L'SVBNAME),SVBNAME                            
         MVC   XHEAD3+145(L'HEADDT),HEADDT                                      
         LA    R2,XP                                                            
         USING PRNTD,R2                                                         
         LA    R4,PAMTS1               EDIT ACCUMULATORS TO PRINT LINE          
         LA    R5,PENT1                                                         
PRN100   EDIT  (P8,(R3)),(15,(R4)),2,COMMAS=YES,MINUS=YES                       
         LA    R3,CLNLEN(R3)                                                    
         LA    R4,PAMLEN1(R4)                                                   
         BCT   R5,PRN100                                                        
*                                                                               
         LA    R4,PAMTS2               EDIT INIDICATOR BUDGETS                  
         LA    R5,PENT2                                                         
PRN200   DS    0H                                                               
         ZAP   DUB,0(8,R3)                                                      
         SRP   DUB(8),62,5                                                      
         EDIT  (P8,DUB),(8,(R4)),0,COMMAS=YES,ZERO=NOBLANK                      
         LA    R3,CLNLEN(R3)                                                    
         LA    R4,PAMLEN2(R4)                                                   
         BCT   R5,PRN200                                                        
*                                                                               
PRNXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        INTITIALIZE ACCUMULATORS                                               
*        UPON ENTERING - R3 POINTS TO TABLE TO BE CLEARED                       
*--------------------------------------------------------------------*          
CLRACUM  NTR1                                                                   
         LA    R5,CLNENT                  LENGTH OF FIELD                       
CLR100   ZAP   0(CLNLEN,R3),=P'0'         CLEAR                                 
         LA    R3,CLNLEN(R3)              BUMP ACCUMULATOR TABLE                
         BCT   R5,CLR100                                                        
*                                                                               
CLRXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROLL TO ACCUMULATORS                                                   
*--------------------------------------------------------------------*          
ROLACUM  NTR1                                                                   
         LA    R2,TABSTART                BEGINNING OF TABLES                   
         LA    R3,TABNUM                  NUMBER OF TABLES                      
*                                                                               
ROL050   LA    R4,CURENT                  NUMBER OF ENTRIES                     
         LA    R5,CURACUM                 CURRENT AMOUNTS TO ROLL               
ROL100   AP    0(CURLEN,R2),0(CURLEN,R5)                                        
         LA    R2,CURLEN(R2)              BUMP RECEIVING TABLE                  
         LA    R5,CURLEN(R5)              BUMP GIVING TABLE                     
         BCT   R4,ROL100                  NUMBER OF ENTRIES PER TABLE           
         BCT   R3,ROL050                  NUMBER OF TABLES                      
*                                                                               
ROLXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO                                                
*--------------------------------------------------------------------*          
PUTSR    NTR1                                                                   
         USING ACTRECD,R2                                                       
         XC    BUFAREA,BUFAREA            CLEAR BUFFALO AREA                    
         MVC   BFKEY,SPACES               INITIALIZE KEY TO SPACES              
         MVC   BFCOMP,ACTKCPY             COMPANY CODE                          
         MVC   BFUL,ACTKUNT               UNIT/LEDGER                           
         ZIC   R3,SLEVC                   FOR LENGTH OF LEVEL C                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BFACCT(0),ACTKACT          MOVE IN ACCOUNT CODE                  
         MVC   BFANAME,SVANAME            LEVEL A NAME                          
         MVC   BFBNAME,SVBNAME                  B NAME                          
         MVC   BFCNAME,SVCNAME                  C NAME                          
         ZAP   BFAMT1,WKDEB                                                     
         ZAP   BFAMT2,WKCRED                                                    
         ZAP   BFAMT3,=P'1'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA                               
*                                                                               
         CLI   QOPT2,C'Y'                 DUMP OUT BUFFALO RECORDS FOR          
         BNE   PTSRXIT                    TESTING                               
         MVC   XP(L'BFKEY+L'BFCMMNT),BUFAREA                                    
         ZAP   DUB,BFAMT1                                                       
         OI    DUB+7,X'03'                                                      
         UNPK  XPSECOND+5(16),DUB                                               
         ZAP   DUB,BFAMT2                                                       
         OI    DUB+7,X'03'                                                      
         UNPK  XPSECOND+25(16),DUB                                              
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
PTSRXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        PUT RECS OUT TO BUFFALO                                                
*--------------------------------------------------------------------*          
PUTSJ    NTR1                                                                   
         USING ACTRECD,R2                                                       
         XC    BUFAREA,BUFAREA                                                  
         MVC   BFKEY,SPACES                                                     
         MVC   BFCOMP,ACTKCPY                                                   
         MVC   BFUL,ACTKUNT                                                     
         MVC   BFSJCLT,ACTKACT                                                  
         MVC   BFSJDEPT,SJDEPT                                                  
         ZAP   BFAMT1,WKDEB                                                     
         ZAP   BFAMT2,WKCRED                                                    
         ZAP   BFAMT3,=P'1'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFAREA                               
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   PTSJXIT                                                          
         MVC   XP(L'BFKEY+L'BFCMMNT),BUFAREA                                    
         ZAP   DUB,BFAMT1                                                       
         OI    DUB+7,X'03'                                                      
         UNPK  XPSECOND+5(16),DUB                                               
         ZAP   DUB,BFAMT2                                                       
         OI    DUB+7,X'03'                                                      
         UNPK  XPSECOND+25(16),DUB                                              
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
PTSJXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        SET UP BOXES                                                *          
*--------------------------------------------------------------------*          
BXHOOK   NMOD1 0,*BXHOOK*                                                       
         L     RC,BOXRC                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS(198),XSPACES                                             
*                                                                               
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+39,C'C'                                                  
         MVI   BOXCOLS+44,C'C'                                                  
         MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+96,C'C'                                                  
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+130,C'C'                                                 
         MVI   BOXCOLS+140,C'C'                                                 
         MVI   BOXCOLS+150,C'C'                                                 
         MVI   BOXCOLS+160,C'R'                                                 
*                                                                               
BXH500   DS    0H                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        LITERALS                                                               
*--------------------------------------------------------------------*          
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        CONSTANTS                                                              
*--------------------------------------------------------------------*          
*                                                                               
ACBUFF   DS    CL2000                                                           
*                                                                               
BUDS     DS    0C                       TABLE OF BUDGETS REPORTED ON            
BUD1     DC    XL2'0006'                                                        
BUDLEN   EQU   *-BUDS                                                           
BUD2     DC    XL2'0007'                                                        
BUD3     DC    XL2'0008'                                                        
BUD4     DC    XL2'0009'                                                        
BUD5     DC    XL2'000A'                                                        
BUDNMS   EQU   (*-BUDS)/BUDLEN                                                  
*                                                                               
         BUFF  LINES=2000,ROWS=1,COLUMNS=3,COMMENT=108,FLAVOR=PACKED,  X        
               KEYLIST=(15,A)                                                   
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              LOCAL WORKING STORAGE                                            
*--------------------------------------------------------------------*          
ACCLD    DSECT                                                                  
*              AREA FOR BUILDING BUFFALO RECS                                   
BUFAREA  DS    0CL147           BUFFALO RECORD:                                 
BFKEY    DS    0CL15            BUFFALO KEY:                                    
BFCOMP   DS    CL1                 COMPANY CODE                                 
BFUL     DS    CL2                 UNIT/LEDGER                                  
BFACCT   DS    0CL12               ACCOUNT CODE                                 
BFSRLOC  DS    CL1                 LOCATION                                     
BFSRDPT  DS    CL2                 DEPARTMENT                                   
BFSRCLT  DS    CL3                 CLIENT CODE                                  
BFKSRSP  DS    CL6                 SPACES                                       
         ORG   BFACCT           SJ RECORDS                                      
BFSJCLT  DS    CL3                 CLIENT                                       
BFSJDEPT DS    CL2                 DEPARTMENT                                   
BFSJSP   DS    CL7                                                              
BFCMMNT  DS    0CL108           BUFFALO TEXT:                                   
BFANAME  DS    CL36                SR ACCOUNT NAME                              
BFBNAME  DS    CL36                SR ACCOUNT NAME                              
BFCNAME  DS    CL36                SR ACCOUNT NAME                              
BFAMTS   DS    0CL24            BUFFALO AMOUNTS:                                
BFAMT1   DS    PL8                 DEBITS                                       
BFAMT2   DS    PL8                 CREDITS                                      
BFAMT3   DS    PL8                 DUMMY COLUMN TO FORCE TOTALING               
BFLNGTH  EQU   *-BFKEY                                                          
*                                                                               
*                                                                               
*              AREA FOR BUILDING SUMMARY RECORDS FROM INITIAL BUFF REC          
BUFAREA2 DS    CL(BFLNGTH)                                                      
BUFSAVE  DS    CL(BFLNGTH)         SAVE BUF TO RESTORE SEQUENTIAL               
*                                                                               
*                                                                               
WKDEB    DS    PL6                                                              
WKCRED   DS    PL6                                                              
HEADDT   DS    CL6                 QSTART IN CHAR FORMAT                        
QENCH    DS    CL6                 QEND IN CHAR FORMAT                          
QENPK    DS    CL3                 QEND IN PACKED UNSIGNED FORMAT               
TODAY2   DS    CL2                                                              
ELCODE   DS    XL1                 GETEL ELEMENT CODE                           
MYKEY    DS    CL49                                                             
SAVKEY   DS    CL42                                                             
SLEVA    DS    CL1                 LEVEL A LENGTH FOR UL=SR                     
SLEVB    DS    CL1                 LEVEL B LENGTH FOR UL=SR                     
SLEVC    DS    CL1                 LEVEL C LENGTH FOR UL=SR                     
SLEVD    DS    CL1                 LEVEL D LENGTH FOR UL=SR                     
SVANAME  DS    CL36                LEVEL A NAME                                 
SVBNAME  DS    CL36                LEVEL B NAME                                 
SVCNAME  DS    CL36                LEVEL C NAME                                 
SVCOMP   DS    XL1                 SAVE COMPANY CODE                            
SVLOC    DS    CL1                 SAVE LAST LOCATION                           
SVDEPT   DS    CL2                 SAVE LAST DEPARTMENT                         
SVCLI    DS    CL3                 SAVE LAST CLIENT                             
COMMAND  DS    CL8                                                              
TOTSW    DS    CL1                 TOTAL SWITCH                                 
LOCCH    EQU   X'80'               LOCATION BREAK                               
DEPTCH   EQU   X'40'               DEPARTMENT BREAK                             
CLICH    EQU   X'20'               CLIENT BREAK                                 
INDSW    DS    CL1                 TOTAL SWITCH                                 
SJREAD   EQU   X'80'               READ SJ RECORD                               
SJDEPT   DS    CL2                 SAVE LAST DEPARTMENT                         
         EJECT                                                                  
*                                                                               
*        SUMMARY TOTALS                                                         
CURACUM  DS    0C                  CURRENT PRINT LINE ACCUMULATOR               
CURSR    DS    PL8                                                              
CURLEN   EQU   *-CURACUM                                                        
CURSJ    DS    PL8                                                              
CURSRSJ  DS    PL8                                                              
CURCRLM  DS    PL8                                                              
CURBUD   DS    PL8                                                              
CURCON   DS    PL8                                                              
CURLET   DS    PL8                                                              
CURUPD   DS    PL8                                                              
CURENT   EQU   (*-CURACUM)/CURLEN                                               
CURTBLN  EQU   *-CURACUM                                                        
*                                                                               
TABSTART DS    0C                                                               
CLNACUM  DS    0C                  CLIENT LEVEL ACCUMULATORS                    
         DS    PL8                                                              
CLNLEN   EQU   *-CLNACUM                                                        
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
CLNENT   EQU   (*-CLNACUM)/CLNLEN                                               
*                                                                               
DEPACUM  DS    0C                  DEPARTMENT LEVEL ACCUMULATORS                
         DS    PL8                                                              
DEPLEN   EQU   *-DEPACUM                                                        
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
DEPENT   EQU   (*-DEPACUM)/DEPLEN                                               
*                                                                               
LOCACUM  DS    0C                  LOCATION LEVEL ACCUMULATORS                  
         DS    PL8                                                              
LOCLEN   EQU   *-LOCACUM                                                        
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
LOCENT   EQU   (*-LOCACUM)/LOCLEN                                               
*                                                                               
REPACUM  DS    0C                  REPORT LEVEL ACCUMULATORS                    
         DS    PL8                                                              
REPLEN   EQU   *-REPACUM                                                        
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
REPENT   EQU   (*-REPACUM)/REPLEN                                               
*                                                                               
TABNUM   EQU   (*-TABSTART)/CURTBLN     NUMBER OF TABLE LEVELS                  
*                                                                               
ABUFF    DS    A                                                                
ACCBUFF  DS    A                                                                
AIO1     DS    A                                                                
RELO     DS    A                                                                
ADBOX    DS    A                                                                
*                                                                               
IOAREA1  DS    CL1000                                                           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DSECTS                                                                 
*--------------------------------------------------------------------*          
*              COVER PRINT LINE                                                 
PRNTD    DSECT                                                                  
         DS    CL2                                                              
PCLI     DS    CL36                                                             
         DS    CL3                                                              
PCLICD   DS    CL3                                                              
         DS    CL3                                                              
PAMTS1   DS    0C                                                               
PARBAL   DS    CL15                                                             
         DS    CL2                                                              
PAMLEN1  EQU   *-PAMTS1                                                         
PWIPBAL  DS    CL15                                                             
         DS    CL2                                                              
PTOUT    DS    CL15                                                             
         DS    CL2                                                              
PCRLIM   DS    CL15                                                             
         DS    CL2                                                              
PBUDG    DS    CL15                                                             
         DS    CL2                                                              
PENT1    EQU   (*-PAMTS1)/PAMLEN1                                               
*                                                                               
PAMTS2   DS    0C                                                               
PCONTR   DS    CL8                                                              
         DS    CL2                                                              
PAMLEN2  EQU   *-PAMTS2                                                         
PLETTER  DS    CL8                                                              
         DS    CL2                                                              
PUPDAT   DS    CL8                                                              
         DS    CL2                                                              
PENT2    EQU   (*-PAMTS2)/PAMLEN2                                               
*                                                                               
*                                                                               
*        ACGENFILE                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDBOXEQUS                                                              
*        DDMASTD                                                                
*        DDREPXTRAD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041ACREPCL02 05/01/02'                                      
         END                                                                    
