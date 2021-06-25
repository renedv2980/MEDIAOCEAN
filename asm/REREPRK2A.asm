*          DATA SET REREPRK2A  AT LEVEL 039 AS OF 05/01/02                      
*PHASE RERK02A,*                                                                
*&&      SET   T0=N,T1=N,T2=N,T3=N                                              
         TITLE 'REREPRK02 - KRG BUDGET RANKING REPORT'                          
*********************************************************************           
*                                                                   *           
*        REREPRK02 --- KRG BUDGET RANKING REPORT                    *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* 14APR1997 (JRD) -- INITIAL ENTRY                                  *           
*                                                                   *           
* 21MAY1997 (BU ) -- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* 18JUN1997 (JRD) -- NEGATIVE DOLLAR TOTAL FIX                      *           
*                    EOF CHECK ON PRIOR READ                        *           
*                    INIT WORKING STORAGE EACH TIME                 *           
*                                                                   *           
* 19JUN1997 (JRD) -- CHECK MASTER REP & READ REP RECORD             *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*                                                                   *           
*********************************************************************           
* DISPLAY OPTIONS:                                                  *           
*   REQUESTOR = $$XTRACT                                            *           
*          WILL SHOW CONTRACT INFORMATION                           *           
*                                                                   *           
*   &&T0 = Y                                                        *           
*          PRINT OUT LINE SHOWING PRINTLINE USAGE                   *           
*   &&T1 = Y                                                        *           
*          PRINT OUT TSAR RECORDS AS THE SECONDARY KEYS ARE BUILT   *           
*   &&T2 = Y                                                        *           
*          PRINT OUT TSAR RECORDS LEFT AFTER BUILDING FINAL KEYS    *           
*   &&T3 = Y                                                        *           
*          PRINT OUT ROUNDED AND UNROUNDED DOLLARS.  ONLY USEFUL    *           
*           WHEN ACTUALLY USING PENNIES OPTION                      *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      RA  =  WORKD                                                 *           
*      RC  =  FILEC                                                 *           
*                                                                   *           
*********************************************************************           
RERK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RERK02,R7,RR=RE                                              
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
MODEEXEQ CR    RB,RB                                                            
         B     *+6                                                              
MODEEXNE LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
***********************************************************************         
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(RUNFRST),AL3(RUN1ST)     RUN FIRST                           
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
***********************************************************************         
*   RUN1ST: FIRST TIME FOR RUN                                                  
***********************************************************************         
RUN1ST   NTR1                                                                   
         MVC   SAVREP,RREPKREP     NEED TO SAVE THIS IN CASE OF MASTER          
         MVC   SAVRNAME,RREPSHRT   THIS TOO                                     
         B     MODEEXEQ                                                         
         EJECT                                                                  
***********************************************************************         
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
***********************************************************************         
INITIAL  NTR1                                                                   
         LA    RE,OVERWORK         CLEAR WORKING STORAGE                        
         LA    RF,OVWORKLN                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         ZAP   PRANK,=P'0'                                                      
         ZAP   PCKOF16B,=P'0'                                                   
         ZAP   PCK16B,=P'0'                                                     
         ZAP   TOTDOL,=P'0'                                                     
         ZAP   CUMDOL,=P'0'                                                     
         ZAP   PTOTDOL,=P'0'                                                    
         ZAP   PCUMDOL,=P'0'                                                    
         ZAP   RANK,=P'0'                                                       
*                                                                               
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,HEDHOOK                                                       
         ST    R3,HEADHOOK                                                      
*                                                                               
         L     R2,ATSARDWA         A(TSAR CONTROL BOLOCK AREA)                  
         USING TSARD,R2                                                         
         XC    0(TSARDL,R2),0(R2)                                               
         MVC   TSABUF,AAGGREG      USE AGGREG BUFFER                            
         MVC   TSAREC,=A(LENAGG)                                                
         LA    R0,TSKLENQ          SET KEY LENGTH                               
         STC   R0,TSKEYL                                                        
         LA    R0,L'BUFFREC        SET RECORD LENGTH                            
         STH   R0,TSRECL                                                        
         MVI   TSOFFACT,TSAINI     INITIALIZE BUFFER                            
         GOTO1 ATSAROFF,(R2)                                                    
         MVC   SVTSRBLK,0(R2)      SAVE THE TSAR BLOCK                          
         B     MODEEXEQ                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              SHOW CONTRACT DETAILS                                            
***********************************************************************         
POST     NTR1                                                                   
*                                                                               
         L     R4,ANEWMON          A(MONTH TABLE)                               
         XC    CONTOTS,CONTOTS                                                  
         NI    MISCFLG1,FF-MF1CBUCK                                             
*                                                                               
*   FIND REQUEST START DATE IN TABLE.                                           
*                                                                               
POST0020 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0030            FOUND - BEGIN TO PULL FIGURES                
         BH    MODEEXEQ            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0020            GO BACK FOR NEXT                             
*                                                                               
POST0030 EQU   *                                                                
         CLI   0(R4),0             ANY TABLE ENTRY?                             
         BE    POST0100            NO  - EXIT                                   
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    POST0100            TABLE > END DATE - EXIT                      
*                                                                               
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
*                                                                               
         MVC   CONTOTS(4),TOTORD(R6)                                            
         TM    FLAG6(R4),X'01'     ANY INVOICED $ IN BUCKET?                    
         BZ    *+10                NO  - TAKE ORDERED $                         
         MVC   CONTOTS(4),CUASATIN(R6)                                          
*                                                                               
         MVC   CONTOTS+4(4),PRASATOR(R6)                                        
         TM    FLAG6(R4),X'04'     ANY INVOICED $ IN BUCKET?                    
         BZ    *+10                NO  - TAKE ORDERED $                         
         MVC   CONTOTS+4(4),PRASATIN(R6)                                        
*                                                                               
         OC    CONTOTS(4),CONTOTS  ANY VALUE IN BUCKET?                         
         BNZ   *+14                                                             
         OC    CONTOTS+4(4),CONTOTS+4                                           
         BZ    POST0070            NO - DON'T SET BUCKET FLAG                   
*                                                                               
         OI    MISCFLG1,MF1CBUCK                                                
*                                                                               
         CLC   =C'$$XTRACT',QUESTOR                                             
         B     POST0060            JUST TOTALS UNLESS PATCHED                   
         OC    CONTOTS+12(4),CONTOTS+12                                         
         BNZ   POST0060                                                         
         OC    CONTOTS+8(4),CONTOTS+8                                           
         BNZ   POST0060                                                         
*                                                                               
         MVC   P+1(2),RCONKREP                                                  
         MVC   P+4(11),=C'CONTRACT = '                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         MVC   P+25(L'RSTAKSTA),RSTAKSTA                                        
         MVC   P+35(L'RSTAOWN),RSTAOWN                                          
         GOTO1 REPORT                                                           
*                                                                               
POST0060 EQU   *                                                                
         ICM   R2,15,CONTOTS+8     LOAD TOTAL CURR INV $                        
         ICM   R1,15,CONTOTS       ADD CURRENT INV $                            
         AR    R2,R1                                                            
         ST    R2,CONTOTS+8        STORE IT BACK                                
*                                                                               
         ICM   R2,15,CONTOTS+12    LOAD TOTAL PRIOR INV $                       
         ICM   R1,15,CONTOTS+4     ADD PRIOR INV $                              
         AR    R2,R1                                                            
         ST    R2,CONTOTS+12       STORE IT BACK                                
*                                                                               
         CLC   =C'$$XTRACT',QUESTOR                                             
         B     POST0070            JUST TOTALS UNLESS PATCHED                   
         BAS   RE,POSTDISP                                                      
*                                                                               
POST0070 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST0030            SWING BACK FOR NEXT BUCKET                   
*                                                                               
POST0100 EQU   *                                                                
         TM    MISCFLG1,MF1CBUCK   ANY BUCKETS?                                 
         BNO   POSTX               NO                                           
*                                                                               
*&&T3                                                                           
         MVC   P(11),=C'CONTRACT = '                                            
         MVC   P+12(2),RCONKREP                                                 
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         EDIT  (B4,CONTOTS+8),(10,P+25),2,COMMAS=YES,MINUS=YES                  
*&&                                                                             
         SR    R0,R0                                                            
         ICM   R1,15,CONTOTS+8                                                  
         LPR   R1,R1                                                            
         CLI   QACCTOPT,C'P'       PENNIES?                                     
         BNE   *+14                                                             
         AH    R1,=H'50'                                                        
         LA    RE,100                                                           
         DR    R0,RE                                                            
         TM    CONTOTS+8,X'80'     LOSS?                                        
         BNO   *+6                 NO                                           
         LNR   R1,R1                                                            
         ST    R1,CONTOTS+8                                                     
*&&T3                                                                           
         EDIT  (B4,CONTOTS+8),(10,P+40),COMMAS=YES,MINUS=YES                    
         EDIT  (B4,CONTOTS+12),(10,P+55),2,COMMAS=YES,MINUS=YES                 
*&&                                                                             
         SR    R0,R0                                                            
         ICM   R1,15,CONTOTS+12                                                 
         LPR   R1,R1                                                            
         CLI   QACCTOPT,C'P'       PENNIES?                                     
         BNE   *+14                                                             
         AH    R1,=H'50'                                                        
         LA    RE,100                                                           
         DR    R0,RE                                                            
         TM    CONTOTS+12,X'80'    LOSS?                                        
         BNO   *+6                 NO                                           
         LNR   R1,R1                                                            
         ST    R1,CONTOTS+12                                                    
*                                                                               
*&&T3                                                                           
         EDIT  (B4,CONTOTS+12),(10,P+70),COMMAS=YES,MINUS=YES                   
         GOTO1 REPORT                                                           
*&&                                                                             
         CLC   =C'$$XTRACT',QUESTOR                                             
         BNE   POST0106                                                         
*                                                                               
         MVC   P+1(L'RSTAOWN),RSTAOWN                                           
         MVC   P+10(L'RSTAKSTA),RSTAKSTA                                        
         MVC   P+20(7),=C'TOTAL: '                                              
         OC    CONTOTS+8(4),CONTOTS+8                                           
         BZ    POST0102                                                         
*                                                                               
         MVC   P+30(7),=C'CURRENT'                                              
         EDIT  (B4,CONTOTS+8),(10,P+40),COMMAS=YES,MINUS=YES                    
POST0102 EQU   *                                                                
         OC    CONTOTS+12(4),CONTOTS+12                                         
         BZ    POST0104                                                         
*                                                                               
         MVC   P+60(5),=C'PRIOR'                                                
         EDIT  (B4,CONTOTS+12),(10,P+70),COMMAS=YES,MINUS=YES                   
POST0104 EQU   *                                                                
         MVC   P+099(11),=C'CONTRACT = '                                        
         MVC   P+112(2),RCONKREP                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,P+115,4,=C'TOG'                             
                                                                                
         GOTO1 REPORT                                                           
*                                                                               
POST0106 EQU   *                                                                
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
         MVC   0(TSARDL,R2),SVTSRBLK                                            
*                                                                               
         OC    CONTOTS+8(4),CONTOTS+8                                           
         BZ    POST0150            NO CURRENT DOLLARS                           
**************************                                                      
** CURRENT OWNER RECORD **                                                      
**************************                                                      
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
         MVI   TSKTYP,X'01'        CURRENT                                      
         MVC   TSKOWN,RSTAOWN      OWNER                                        
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+10                                                             
         MVC   TSKSTA,RSTAKSTA     STATION IF NO OWNER                          
*                                                                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         MVC   OLDBFREC,BUFFREC                                                 
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    *+14                YES - ADD THE RECORD                         
         CLC   TSKEY(TSKLENQ),OLDBFREC                                          
         BE    POST0110            RECORD FOUND - UPDATE IT                     
*                                                                               
         MVC   BUFFREC,OLDBFREC                                                 
         ICM   R0,15,CONTOTS+8                                                  
         CVD   R0,DUB                                                           
         ZAP   TSRDOL,DUB                                                       
*                                                                               
         MVC   TSRNAME,SPACES                                                   
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSKCOMP,RCONKREP    NOW WE ADD THE COMPANY RECORD                
         MVC   TSRNAME,RREPSHRT                                                 
*                                                                               
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     POST0150                                                         
*                                                                               
POST0110 DS    0H                  UPDATE EXISTING OWNER RECORD                 
         ICM   R0,15,CONTOTS+8                                                  
         CVD   R0,DUB                                                           
         AP    TSRDOL,DUB          ADD DOLLARS TO OWNER                         
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            WHAT HAPPEND HERE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
****************************                                                    
** CURRENT COMPANY RECORD **                                                    
****************************                                                    
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
         MVI   TSKTYP,X'01'        CURRENT                                      
         MVC   TSKOWN,RSTAOWN      OWNER                                        
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+10                                                             
         MVC   TSKSTA,RSTAKSTA     STATION IF NO OWNER                          
         MVC   TSKCOMP,RCONKREP    COMPANY                                      
*                                                                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         MVC   OLDBFREC,BUFFREC                                                 
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    *+14                YES - ADD THE RECORD                         
         CLC   TSKEY(TSKLENQ),OLDBFREC                                          
         BE    POST0120            RECORD FOUND - UPDATE IT                     
*                                                                               
         MVC   BUFFREC,OLDBFREC                                                 
         ICM   R0,15,CONTOTS+8                                                  
         CVD   R0,DUB                                                           
         ZAP   TSRDOL,DUB                                                       
         MVC   TSRNAME,RREPSHRT                                                 
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     POST0150                                                         
*                                                                               
POST0120 DS    0H                  UPDATE COMPANY RECORD                        
         ICM   R0,15,CONTOTS+8                                                  
         CVD   R0,DUB                                                           
         AP    TSRDOL,DUB          ADD COMPANY DOLLARS                          
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            WHAT HAPPENED HERE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
POST0150 DS    0H                                                               
         OC    CONTOTS+12(4),CONTOTS+12                                         
         BZ    POST0200            NO PRIOR DOLLARS                             
************************                                                        
** PRIOR OWNER RECORD **                                                        
************************                                                        
         XC    BUFFREC,BUFFREC                                                  
         MVI   TSKTYP,X'02'        PRIOR                                        
         MVC   TSKOWN,RSTAOWN      OWNER                                        
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+10                                                             
         MVC   TSKSTA,RSTAKSTA     STATION IF NO OWNER                          
*                                                                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         MVC   OLDBFREC,BUFFREC                                                 
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    *+14                YES - ADD THE RECORD                         
         CLC   TSKEY(TSKLENQ),OLDBFREC                                          
         BE    POST0160            RECORD FOUND - UPDATE IT                     
*                                                                               
         MVC   BUFFREC,OLDBFREC                                                 
         ICM   R0,15,CONTOTS+12                                                 
         CVD   R0,DUB                                                           
         ZAP   TSRDOL,DUB                                                       
*                                                                               
         MVC   TSRNAME,SPACES                                                   
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSKCOMP,RCONKREP    NOW WE ADD THE COMPANY RECORD                
         MVC   TSRNAME,RREPSHRT                                                 
*                                                                               
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     POST0200                                                         
*                                                                               
POST0160 DS    0H                  UPDATE PRIOR OWNER RECORD                    
         ICM   R0,15,CONTOTS+12                                                 
         CVD   R0,DUB                                                           
         AP    TSRDOL,DUB          ADD PRIOR OWNER DOLLARS                      
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            WHAT HAPPEND HERE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
**************************                                                      
** PRIOR COMPANY RECORD **                                                      
**************************                                                      
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
         MVI   TSKTYP,X'02'        ONLY CURRENT FOR NOW                         
         MVC   TSKOWN,RSTAOWN      OWNER                                        
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+10                                                             
         MVC   TSKSTA,RSTAKSTA     STATION IF NO OWNER                          
         MVC   TSKCOMP,RCONKREP    COMPANY                                      
*                                                                               
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         MVC   OLDBFREC,BUFFREC                                                 
         GOTO1 ATSAROFF,(R2)                                                    
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    *+14                YES - ADD THE RECORD                         
         CLC   TSKEY(TSKLENQ),OLDBFREC                                          
         BE    POST0170            RECORD FOUND - UPDATE IT                     
*                                                                               
         MVC   BUFFREC,OLDBFREC                                                 
         ICM   R0,15,CONTOTS+12                                                 
         CVD   R0,DUB                                                           
         ZAP   TSRDOL,DUB                                                       
         MVC   TSRNAME,RREPSHRT                                                 
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     POST0200                                                         
*                                                                               
POST0170 DS    0H                  UPDATE PRIOR COMPANY RECORD                  
         ICM   R0,15,CONTOTS+12                                                 
         CVD   R0,DUB                                                           
         AP    TSRDOL,DUB          ADD PRIOR COMPANY DOLLARS                    
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAPUT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            WHAT HAPPENED HERE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
POST0200 DS    0H                                                               
         DROP  R2                                                               
*                                                                               
POSTX    EQU   *                                                                
         B     MODEEXEQ                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT A BUCKET                                                                
***********************************************************************         
POSTDISP NTR1                                                                   
         MVC   P+2(07),=C'DATE = '                                              
         MVC   P+09(6),0(R4)                                                    
         OC    CONTOTS(4),CONTOTS                                               
         BZ    POSTD02                                                          
         MVC   P+20(7),=C'CURRENT'                                              
         EDIT  (4,CONTOTS),(10,P+30),COMMAS=YES,MINUS=YES                       
POSTD02  DS    0H                                                               
         OC    CONTOTS+4(4),CONTOTS+4                                           
         BZ    POSTD04                                                          
         MVC   P+50(5),=C'PRIOR'                                                
         EDIT  (4,CONTOTS+4),(10,P+60),COMMAS=YES,MINUS=YES                     
POSTD04  DS    0H                                                               
         GOTO1 REPORT                                                           
         B     MODEEXEQ                                                         
         EJECT                                                                  
*&&T0                                                                           
DOTS     DC    C'.....................................................'         
*&&                                                                             
***********************************************************************         
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
***********************************************************************         
RPTDONE  NTR1                                                                   
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
*&&T0                                                                           
         MVC   P+CURRCOL(CURRLEN),DOTS                                          
         MVC   P+OWNRCOL(OWNRLEN),DOTS                                          
         MVC   P+CURBCOL(CURBLEN),DOTS                                          
         MVC   P+CURVCOL(CURVLEN),DOTS                                          
         MVC   P+CURCCOL(CURCLEN),DOTS                                          
         MVC   P+CURPCOL(CURPLEN),DOTS                                          
         MVC   P+PRIRCOL(PRIRLEN),DOTS                                          
         MVC   P+PRIBCOL(PRIBLEN),DOTS                                          
         MVC   P+PRIVCOL(PRIVLEN),DOTS                                          
         MVC   P+PRICCOL(PRICLEN),DOTS                                          
         MVC   P+PRIPCOL(PRIPLEN),DOTS                                          
         GOTO1 REPORT                                                           
*&&                                                                             
         L     R2,ATSARDWA         A(TSAR CONTROL BLOCK)                        
         USING TSARD,R2            RELOAD TSAR BLOCK                            
         MVC   0(TSARDL,R2),SVTSRBLK                                            
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
*&&T1                                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 REPORT                                                           
*&&                                                                             
RPTD0010 DS    0H                                                               
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    RPTD0100            YES                                          
*                                                                               
*&&T1                                                                           
         EDIT  (B1,TSKTYP),(2,P)                                                
         MVC   P+3(L'TSKOWN),TSKOWN                                             
         MVC   P+8(L'TSKSTA),TSKSTA                                             
         MVC   P+15(L'TSKCOMP),TSKCOMP                                          
         EDIT  TSRDOL,(10,P+20),ZERO=NOBLANK,MINUS=YES                          
         EDIT  (B2,TSRORNK),(3,P+40),ZERO=NOBLANK                               
         GOTO1 REPORT                                                           
*&&                                                                             
         CLI   TSKTYP,X'01'                                                     
         BE    RPTD0012                                                         
         CLI   TSKTYP,X'02'                                                     
         BE    RPTD0012                                                         
         CLI   TSKTYP,X'12'                                                     
         BE    RPTD0012                                                         
         B     RPTD0090            SKIP THIS TYPE                               
*                                                                               
RPTD0012 DS    0H                                                               
         MVC   OLDBFREC,BUFFREC                                                 
OB       USING TSKEY,OLDBFREC                                                   
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSADEL     DELETE IT                                    
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
         CLI   OB.TSKTYP,X'12'     THIRD PASS ON PRIOR?                         
         BE    RPTD0020            YES - GO DO IT THEN                          
*                                                                               
*************************                                                       
** SECOND PASS RECORDS **                                                       
*************************                                                       
         OC    OB.TSKCOMP,OB.TSKCOMP   OWNER ENTRY?                             
         BNZ   RPTD0014                NO                                       
*                                                                               
         MVC   PCKOF16B,OB.TSRDOL                                               
         CLI   OB.TSKTYP,X'01'         CURRENT RECORD?                          
         BNE   *+10                    NO                                       
         AP    TOTDOL,OB.TSRDOL                                                 
         CLI   OB.TSKTYP,X'02'         PRIOR RECORD?                            
         BNE   *+10                    NO                                       
         AP    PTOTDOL,OB.TSRDOL                                                
*                                                                               
RPTD0014 DS    0H                  STUFF FOR BOTH OWNER AND COMPANY             
         XC    BUFFREC,BUFFREC                                                  
         ZIC   RE,OB.TSKTYP                                                     
         LA    RE,X'10'(RE)                                                     
         STC   RE,TSKTYP                                                        
*                                                                               
         ZAP   TSKODOL,=P'9999999999999999999999999999999'                      
         MVI   TSKODSN,0           NOT MINUS                                    
*                                                                               
         CP    PCKOF16B,=P'0'      NEGATIVE?                                    
         BL    *+14                YES                                          
         SP    TSKODOL,PCKOF16B                                                 
         B     RPTD0016                                                         
*                                                                               
         MVI   TSKODSN,C'-'        MINUS                                        
         ZAP   TSKODOL,PCKOF16B                                                 
*                                                                               
RPTD0016 DS    0H                                                               
         OC    OB.TSKCOMP,OB.TSKCOMP   COMPANY ENTRY?                           
         BZ    RPTD0018                NO                                       
*                                                                               
         ZAP   TSKCDOL,=P'9999999999999999999999999999999'                      
         MVI   TSKCDSN,0               NOT MINUS                                
*                                                                               
         CP    OB.TSRDOL,=P'0'         NEGATIVE?                                
         BL    *+14                    YES                                      
         SP    TSKCDOL,OB.TSRDOL                                                
         B     RPTD0018                                                         
*                                                                               
         MVI   TSKCDSN,C'-'            MINUS                                    
         ZAP   TSKCDOL,OB.TSRDOL                                                
*                                                                               
RPTD0018 DS    0H                                                               
         B     RPTD0050                                                         
*                                                                               
*****************************                                                   
** THIRD PASS PRIOR RECORD **                                                   
*****************************                                                   
RPTD0020 DS    0H                                                               
         XC    BUFFREC,BUFFREC                                                  
         ZIC   RE,OB.TSKTYP                                                     
         LA    RE,X'10'(RE)                                                     
         STC   RE,TSKTYP                                                        
*                                                                               
         OC    OB.TSKCOMP,OB.TSKCOMP    OWNER RECORD?                           
         BNZ   RPTD0022                 NO                                      
         AP    PRANK,=P'1'                                                      
         CVB   RE,PRANK                                                         
         STCM  RE,3,TSRORNK                                                     
*                                                                               
RPTD0022 DS    0H                                                               
***************************                                                     
** COMMON ON ALL RECORDS **                                                     
***************************                                                     
RPTD0050 DS    0H                                                               
         MVC   TSKOWN,OB.TSKOWN                                                 
         MVC   TSKSTA,OB.TSKSTA                                                 
         MVC   TSKCOMP,OB.TSKCOMP                                               
         MVC   TSRDOL,OB.TSRDOL                                                 
         MVC   TSRNAME,OB.TSRNAME                                               
         DROP  OB                                                               
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0            NEED TO EXPAND BUFFER                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BUFFREC,OLDBFREC                                                 
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
         CLC   BUFFREC,OLDBFREC                                                 
         BNE   RPTD0010                                                         
*                                                                               
RPTD0090 DS    0H                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     RPTD0010                                                         
*                                                                               
RPTD0100 DS    0H                                                               
*&&T1                                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 REPORT                                                           
*&&                                                                             
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
         NI    MISCFLG1,X'FF'-MF1START                                          
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
*&&T2                                                                           
         MVI   P,C'*'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 REPORT                                                           
RPTDX110 DS    0H                  EXTRA CODE TO PRINTOUT TSARBUFF              
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    RPTDX120            YES                                          
*                                                                               
         EDIT  (B1,TSKTYP),(2,P)                                                
         MVC   P+3(L'TSKOWN),TSKOWN                                             
         MVC   P+8(L'TSKSTA),TSKSTA                                             
         MVC   P+15(L'TSKCOMP),TSKCOMP                                          
         EDIT  TSRDOL,(10,P+20),ZERO=NOBLANK,MINUS=YES                          
         EDIT  (B2,TSRORNK),(3,P+40),ZERO=NOBLANK                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     RPTDX110                                                         
*                                                                               
RPTDX120 DS    0H                  RESTORE AFTER PRINTING TSARBUFF              
         MVI   P,C'*'                                                           
         MVC   P+1(L'P-1),P                                                     
         GOTO1 REPORT                                                           
*                                                                               
         XC    BUFFREC,BUFFREC     CLEAR TSAR RECORD                            
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
*&&                                                                             
RPTD0110 DS    0H                  PRINT THE REPORT                             
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BO    RPTDX               YES                                          
         CLI   TSKTYP,X'11'        CURRENT RECORD?                              
         BE    RPTD0112            YES                                          
         B     RPTDX               NO - WE'RE DONE                              
*                                                                               
RPTD0112 DS    0H                                                               
         OC    TSKCOMP,TSKCOMP     OWNER RECORD?                                
         BNZ   RPTD0130            NO                                           
*                                                                               
         TM    MISCFLG1,MF1START   STARTED?                                     
         BZ    RPTD0114            NO                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
RPTD0114 DS    0H                                                               
         OI    MISCFLG1,MF1START                                                
*                                                                               
         AP    RANK,=P'1'                                                       
         EDIT  RANK,(CURRLEN,P+CURRCOL)                                         
*                                  PRINT OWNER NAME                             
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+14                                                             
         MVC   P+OWNRCOL(L'TSKSTA),TSKSTA                                       
         B     RPTD0120                                                         
*                                                                               
         MVC   P+OWNRCOL(L'TSKOWN),TSKOWN                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ROWNKEY,R3                                                       
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,SAVREP                                                  
         MVC   ROWNKOWN,TSKOWN                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   ROWNKEY,KEYSAVE                                                  
         BNE   RPTD0120                                                         
         LA    RF,ROFFREC                                                       
         ST    RF,AIOAREA          NEED SOMETHING                               
         GOTO1 GREC                                                             
         L     R3,AIOAREA                                                       
         MVC   P+OWNRCOL(20),ROWNNAME                                           
         DROP  R3                                                               
*                                                                               
RPTD0120 DS    0H                                                               
         EDIT  TSRDOL,(CURBLEN,P+CURBCOL),ZERO=NOBLANK,MINUS=YES                
*                                                                               
         ZAP   WORK+20(16),TSRDOL                                               
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),TOTDOL                                                   
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(CURVLEN,P+CURVCOL),1                               
*                                                                               
         AP    CUMDOL,TSRDOL                                                    
         EDIT  CUMDOL,(CURCLEN,P+CURCCOL),MINUS=YES                             
*                                                                               
         ZAP   WORK+20(16),CUMDOL                                               
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),TOTDOL                                                   
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(CURPLEN,P+CURPCOL),1                               
*                                                                               
         MVC   PCKOF16B,TSRDOL                                                  
         B     RPTD0200                                                         
*                                                                               
RPTD0130 DS    0H                                                               
         MVC   P+OWNRCOL+2(L'TSKCOMP),TSKCOMP                                   
         CLC   TSRNAME,SPACES                                                   
         BNH   *+10                                                             
         MVC   P+6(20),TSRNAME                                                  
*                                                                               
         EDIT  TSRDOL,(CURBLEN,P+CURBCOL),ZERO=NOBLANK,MINUS=YES                
*                                                                               
         ZAP   WORK+20(16),TSRDOL                                               
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),PCKOF16B                                                 
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(CURVLEN,P+CURVCOL),1                               
*                                                                               
RPTD0200 DS    0H                  READ PRIOR RECORD                            
         MVC   OLDBFREC,BUFFREC    FOR RESTORE                                  
         XC    BUFFREC,BUFFREC                                                  
OB       USING TSKEY,OLDBFREC                                                   
         MVI   TSKTYP,X'22'        THIRD PASS PRIOR                             
         MVC   TSKOWN,OB.TSKOWN                                                 
         OC    TSKOWN,TSKOWN                                                    
         BNZ   *+10                                                             
         MVC   TSKSTA,OB.TSKSTA    STATION IF NO OWNER                          
         MVC   TSKCOMP,OB.TSKCOMP                                               
*                                                                               
         MVC   WORK(TSKLENQ),TSKEY                                              
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
         TM    TSERRS,TSEEOF       END OF FILE?                                 
         BNZ   *+14                                                             
         CLC   TSKEY(TSKLENQ),WORK                                              
         BE    RPTD0202                                                         
*                                                                               
         XC    TSRORNK,TSRORNK                                                  
         ZAP   TSRDOL,=P'0'                                                     
*                                                                               
RPTD0202 DS    0H                  READ PRIOR RECORD                            
         OC    OB.TSKCOMP,OB.TSKCOMP    OWNER?                                  
         BNZ   RPTD0230                 NO                                      
         DROP  OB                                                               
*                                                                               
         EDIT  (B2,TSRORNK),(PRIRLEN,P+PRIRCOL)                                 
         OC    TSRORNK,TSRORNK                                                  
         BNZ   *+10                                                             
         MVC   P+80(3),=C'N/A'                                                  
*                                                                               
         EDIT  TSRDOL,(PRIBLEN,P+PRIBCOL),ZERO=NOBLANK,MINUS=YES                
*                                                                               
         ZAP   WORK+20(16),TSRDOL                                               
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),PTOTDOL                                                  
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(PRIVLEN,P+PRIVCOL),1                               
*                                                                               
         AP    PCUMDOL,TSRDOL                                                   
         EDIT  PCUMDOL,(PRICLEN,P+PRICCOL),MINUS=YES                            
*                                                                               
         ZAP   WORK+20(16),PCUMDOL                                              
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),PTOTDOL                                                  
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(PRIPLEN,P+PRIPCOL),1                               
*                                                                               
         MVC   PCK16B,TSRDOL                                                    
         B     RPTD0300                                                         
*                                                                               
RPTD0230 DS    0H                                                               
         EDIT  TSRDOL,(PRIBLEN,P+PRIBCOL),ZERO=NOBLANK,MINUS=YES                
*                                                                               
         ZAP   WORK+20(16),TSRDOL                                               
         MP    WORK+20(16),=P'1000'                                             
         ZAP   WORK(8),PCK16B                                                   
         CP    WORK(8),=P'0'                                                    
         BE    *+14                                                             
         DP    WORK+20(16),WORK(8)                                              
         B     *+10                                                             
         ZAP   WORK+20(8),=P'0'                                                 
         EDIT  (P8,WORK+20),(PRIVLEN,P+PRIVCOL),1                               
*                                                                               
RPTD0300 DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   BUFFREC,OLDBFREC                                                 
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSARDH     FIRST READ IS HIGH                           
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
         CLC   BUFFREC,OLDBFREC                                                 
         BE    *+6                 WHERE DID IT GO?                             
         DC    H'0'                                                             
*                                                                               
         LA    R0,BUFFREC          SET A(BUFFER RECORD)                         
         ST    R0,TSAREC           INSERT INTO TSAR BLOCK                       
         MVI   TSOFFACT,TSANXT                                                  
         GOTO1 ATSAROFF,(R2)                                                    
         B     RPTD0110                                                         
*                                                                               
RPTDX    DS    0H                                                               
         B     MODEEXEQ                                                         
         EJECT                                                                  
***********************************************************************         
* HEAD HOOK ROUTINE                                                             
***********************************************************************         
HEDHOOK  NTR1                                                                   
         USING HEDHOOK,RF          ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-HEDHOOK                                              
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         MVC   HEAD1+6(L'SAVRNAME),SAVRNAME                                     
*                                                                               
         MVC   HEAD3+10(L'QOFFICE),QOFFICE                                      
         CLC   QOFFICE,SPACES                                                   
         BH    *+10                                                             
         MVC   HEAD3+10(11),=C'ALL OFFICES'                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,QASAT),(10,HEAD4+42)                              
         GOTO1 DATCON,DMCB,(0,QASAT),(10,HEAD4+98)                              
*                                                                               
         CLC   QASAT,SPACES                                                     
         BH    MODEEXEQ                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(10,HEAD4+42)                                  
         GOTO1 DATCON,DMCB,(5,0),(10,HEAD4+98)                                  
         B     MODEEXEQ                                                         
         EJECT                                                                  
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
***********************************************************************         
* NON RE-ENTRANT WORKING STORAGE                                                
***********************************************************************         
*********************                                                           
** WORKING STORAGE **                                                           
*********************                                                           
RELO     DS    F                                                                
SAVEREGS DS    11F                                                              
*                                                                               
SAVREP   DS    CL(L'RREPKREP)                                                   
SAVRNAME DS    CL(L'RREPSHRT)                                                   
*                                                                               
OVERWORK DS    0F                  THIS SPACES GETS CLEARED IN INIT             
CONTOTS  DS    XL(4*4)                                                          
*                                                                               
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
*                                                                               
MISCFLG1 DS    X                   FLAGS                                        
MF1CBUCK EQU   X'80'                - CONTRACT HAS BUCKETS                      
MF1PRIOR EQU   X'40'                - PRIOR RECORDS BEING PROCESSED             
MF1START EQU   X'20'                - REPORT HAS BEEN STARTED                   
*                                                                               
         DS    0D                                                               
PRANK    DS    PL8                 PRIOR YEARS RANK - LENGTH FOR CVB            
PCKOF16B DS    PL16                                                             
PCK16B   DS    PL16                                                             
TOTDOL   DS    PL16                MASTER'S TOTAL DOLLARS                       
CUMDOL   DS    PL16                CUMULATIVE DOLLARS                           
PTOTDOL  DS    PL16                PRIOR MASTER'S TOTAL DOLLARS                 
PCUMDOL  DS    PL16                PRIOR CUMULATIVE DOLLARS                     
RANK     DS    PL3                 THIS YEARS RANK                              
SVTSRBLK DS    CL(TSARDL)                                                       
*                                                                               
****************************                                                    
** TSAR RECORD DEFINITION **                                                    
****************************                                                    
TSKEY    EQU   *                                                                
TSKTYP   DS    C                   RECORD TYPE                                  
TSKODSN  DS    C                   SIGN (0 OR C'-')                             
TSKODOL  DS    PL16                9'S COMPL OWN $$'S                           
TSKOWN   DS    CL(L'RSTAOWN)       STATION OWNER CODE                           
TSKSTA   DS    CL(L'RSTAKSTA)      STATION CALL LETTERS(WHEN NO OWNER)          
*                                                                               
TSKCDSN  DS    C                   SIGN (0 OR C'-')                             
TSKCDOL  DS    PL16                9'S COMPL COMPANY $$'S                       
TSKCOMP  DS    CL2                 COMPANY(REP) CODE                            
*                                    NULL FOR OWNER RECORD                      
TSKLENQ  EQU   *-TSKEY             KEY LENGTH                                   
*                                                                               
TSREC    EQU   *                                                                
TSRDOL   DS    PL16                $$'S                                         
TSRNAME  DS    CL20                REP SHORT NAME                               
TSRORNK  DS    XL2                 OWNER ORDER(FOR PRIOR 3RD PASS)              
TSRLENQ  EQU   *-TSKEY             RECORD LENGTH                                
*                                                                               
         ORG   TSKEY                                                            
BUFFREC  DS    CL(TSRLENQ)                                                      
OLDBFREC DS    CL(TSRLENQ)                                                      
*                                                                               
         ORG                                                                    
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
OVWORKLN EQU   *-OVERWORK                                                       
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
FF       EQU   X'FF'                                                            
       ++INCLUDE REREPRKEQU                                                     
         LTORG                                                                  
         EJECT                                                                  
* REREPRGEQA                                                                    
* REREPTSAR                                                                     
* REGENALL1                                                                     
* REREPWORKD                                                                    
* REREPMODES                                                                    
* REXADDRD                                                                      
* RESUBREPS                                                                     
* DDTSARD                                                                       
* REGENREQ2                                                                     
* DDMASTC                                                                       
         PRINT OFF                                                              
       ++INCLUDE REREPRGEQA                                                     
       ++INCLUDE REREPTSAR                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
       ++INCLUDE DDTSARD                                                        
QREC2D   DSECT                                                                  
       ++INCLUDE REGENREQ2                                                      
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039REREPRK2A 05/01/02'                                      
         END                                                                    
