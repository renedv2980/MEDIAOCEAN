*          DATA SET REREP1502S AT LEVEL 145 AS OF 05/01/02                      
*PHASE RE1502A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'REREP1502 (RE1502) REP TAKEOVER EXTRACT/UPDATE'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREP1502 -- EXTRACTS OR UPDATES DARE TAKEOVER CONTRACTS  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* FEB18/97 (SKU) --- ORIGINAL ENTRY                                *            
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
* JUL30/98 (SKU) --- ADD STATION CALL LETTERS IN X'AD' KEY         *            
*                                                                   *           
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*                                                                  *            
*     OPTION1   S =  EXTRACT TO CREATE TAPE (NEED ID= AND STATION) *            
*               R =  UPDATE WITH INPUT TAPE (NEED ID= )            *            
*     QUESTOR+1 T =  TEST/NO UPDATE                                *            
*                                                                  *            
*  UPDATE PROCESS:                                                 *            
*    FOR THE UPDATE PROCESS, THIS PROGRAM WILL USE TWO DISK FILES. *            
*    THE FIRST CONTAINS RECORDS DUMPED FROM TAPES SENT BY JDS.     *            
*    THE SECOND IS A TEMPORARY BUFFER TO HOLD UNPROCESSED RECORDS  *            
*    FROM THE FIRST DISK FILE. THE PROGRAM WILL READ THE RECORDS   *            
*    FROM THE FIRST DISK FILE, UPDATE CONTRACT RECORDS OR COPY     *            
*    SKIPPED RECORDS TO DISK FILE TWO. AT THE END, THE CONTENTS    *            
*    OF DISK FILE TWO WILL BE DUMPED OVER DISK FILE ONE. THIS      *            
*    ENSURES THAT SKIPPED RECORDS WILL HAVE A CHANCE TO BE         *            
*    PROCESSED IN SUBSEQUENT RUNS.                                 *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RE1502   CSECT                                                                  
         NMOD1 0,**RE15**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 INITIAL                                                          
*                                                                               
         CLI   QOPTION1,C'R'                                                    
         BE    SW20                                                             
         CLI   QOPTION1,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 EXTRACT             RETRIEVE DARE INFO FOR TAPE                  
         GOTO1 DISPEXTS            DISPLAY TOTALS FOR RUN                       
         B     SW30                                                             
*                                                                               
SW20     DS    0H                                                               
         GOTO1 UPDATE              UPDATE FILE                                  
         GOTO1 DUMPFILE            DUMP UNPROCESSED RECORDS BACK                
         GOTO1 DISPTOTS            DISPLAY TOTALS FOR RUN                       
*                                                                               
SW30     DS    0H                                                               
*                                                                               
MAINX    DS    0H                                                               
         B     EXIT                EXIT                                         
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
******************************************************************              
INITIAL  NTR1                                                                   
         CLI   QOPTION1,C'R'       TAPE CREATION?                               
         BE    INIT0010                                                         
         CLI   QOPTION1,C'S'       UPDATE FILE WITH INPUT TAPE?                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FILOUTA,(OUTPUT))                                               
         B     INIT0015                                                         
*                                                                               
INIT0010 DS    0H                                                               
         MVC   P(37),=C'         OLD  OLD                 NEW'                  
         GOTO1 REPORT                                                           
         MVC   P(52),=C'STATION  REP  CONTRACT  DARE      CONTRACT    SX        
               TATUS'                                                           
         GOTO1 REPORT                                                           
         MVC   P(52),=C'-------  ---  --------  ----      --------    -X        
               -----'                                                           
         GOTO1 REPORT                                                           
*                                                                               
         OPEN  (INDISK1,(INPUT))                                                
         OPEN  (OUTDISK2,(OUTPUT))                                              
*                                                                               
INIT0015 DS    0H                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RE,VUTL                                                          
         ST    RE,UTLADDR                                                       
         MVC   UTLSTUFF,0(RE)      SAVE UTLSTUFF FOR DISPLAY                    
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE DARE STATION RECORDS             
*                                                                               
*        GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AREC,0                                             
*        L     RF,UTLADDR          LOAD A(UTL)                                  
*        MVI   4(RF),X'0A'         RESET UTL TO CONTROL FILE                    
*  OPEN GEN FILE                                                                
*        GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',FLIST,AREC                   
*                                                                               
         B     EXIT                                                             
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NGENFILE'                                                    
         DC    CL8' GENDIR '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
******************************************************************              
*  EXTRACT:   RETRIEVE ALL DARE X'51' RECORDS FOR A STATION                     
******************************************************************              
EXTRACT  NTR1                                                                   
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDARKEY,R6          SET RECORD DEFINITION                        
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,QREP                                                    
         MVC   RDARKSTA,QSTATION                                                
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     EXT20                                                            
                                                                                
EXT10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
EXT20    DS    0H                                                               
         CLC   KEY(15),KEYSAVE                                                  
         BNE   EXTX                                                             
                                                                                
         GOTO1 GETRECRD                                                         
*                                                                               
* COMPARE AGAINST ASAT DATE                                                     
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,RDARESEN),(0,FEND)                            
*                                                                               
         CLC   FEND(4),QASAT                                                    
         BL    EXT10                                                            
*                                                                               
         CLC   =C'$EDI$',RDARAGAD                                               
         BE    EXT10               SKIP EDI/NON-DARE AGENCY ORDERS              
*                                                                               
EXT30    DS    0H                                                               
         MVC   P+1(8),=C'DAR REC:'                                              
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),RDARKORD                                               
         EDIT  (P5,MYWORK),(8,P+10),ALIGN=LEFT                                  
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),RDARREP#                                               
         EDIT  (P5,MYWORK),(8,P+28),ALIGN=LEFT                                  
         GOTO1 REPORT                                                           
                                                                                
         MVI   REC-3,TKOLENQ       INSERT LENGTH FOR PUT                        
         MVC   TKOSTA,RDARKSTA                                                  
         GOTO1 =V(HEXOUT),DMCB,RDARREP#,TKOCON,L'RDARREP#,=C'TOG'               
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,TKODAR,L'RDARKORD,=C'TOG'               
         MVC   TKOSID,RDARSNDR                                                  
         MVC   TKORTN,RDARRTS                                                   
         MVC   TKORTC,RDARKAGY                                                  
                                                                                
         BAS   RE,PUTRECS                                                       
                                                                                
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
                                                                                
*        CLI   QUESTOR+1,C'T'                                                   
*        BNE   EXT30                                                            
*        CLC   CONCTR,=F'400'                                                   
*        BL    EXT30                                                            
         B     EXT10                                                            
                                                                                
EXTX     DS    0H                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* UPDATE: UPDATE CONTRACT RECORDS FROM INPUT TAPE                               
******************************************************************              
UPDATE   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6          SET RECORD DEFINITION                        
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,QREP                                                    
         MVC   RSTAKSTA,QSTATION                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   UPD03                                                            
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R6,RECORD                                                        
         MVI   ELCODE,X'0C'                                                     
         BAS   RE,GETEL                                                         
         BNE   UPD03                                                            
         USING RSTAFNEL,R6                                                      
         MVC   OLDREP,RSTAFNFO                                                  
         DROP  R6                                                               
*                                                                               
UPD03    DS    0H                                                               
         L     R4,ARECAREA                                                      
*                                                                               
         CLI   QUESTOR+1,C'T'                                                   
         BNE   UPD05                                                            
         MVC   P(28),=C'*** TEST RUN, NO UPDATES ***'                           
         GOTO1 REPORT                                                           
*                                                                               
UPD05    DS    0H                                                               
         GET   INDISK1,(R4)                                                     
*                                                                               
         MVC   REC,4(R4)                                                        
*                                                                               
         CLC   TKOSTA(4),QSTATION                                               
         BE    UPD08                                                            
         PUT   OUTDISK2,(R4)       NOT PROCESSED, SAVE IN OUTPUT FILE           
         B     UPD05                                                            
*                                                                               
UPD08    DS    0H                                                               
         MVC   P(L'TKOSTA),TKOSTA                                               
         MVC   P+9(L'OLDREP),OLDREP                                             
         MVC   P+14(L'TKOCON),TKOCON                                            
         MVC   P+24(L'TKODAR),TKODAR                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONKEY,R6          SET RECORD DEFINITION                        
         MVI   RCONTTYP,X'AD'                                                   
         MVC   RCONTREP,QREP                                                    
         MVC   RCONTSTA,QSTATION                                                
         GOTO1 =V(HEXIN),DMCB,TKOCON,RCONTOLD,L'TKOCON                          
         DROP  R6                                                               
                                                                                
         GOTO1 HIGHDIR                                                          
         B     UPD20                                                            
                                                                                
UPD10    DS    0H                                                               
         GOTO1 SEQDIR                                                           
                                                                                
UPD20    DS    0H                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BE    UPD30                                                            
         MVC   P+46(32),=C'** NOT ASSIGNED TO A CONTRACT **'                    
         GOTO1 REPORT                                                           
         PUT   OUTDISK2,(R4)       NOT PROCESSED, SAVE IN OUTPUT FILE           
         L     RF,NUPDCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NUPDCTR                                                       
         B     UPD05                                                            
                                                                                
UPD30    DS    0H                                                               
         GOTO1 GETRECRD                                                         
*                                                                               
         LA    R6,RECORD                                                        
         MVI   ELCODE,X'1C'                                                     
         BAS   RE,GETEL                                                         
         BE    UPD40                                                            
         MVC   P+46(31),=C'** NO TAKEOVER ELEMENT FOUND **'                     
         GOTO1 REPORT                                                           
         PUT   OUTDISK2,(R4)       NOT PROCESSED, SAVE IN OUTPUT FILE           
         L     RF,NUPDCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NUPDCTR                                                       
         B     UPD05                                                            
*                                                                               
UPD40    DS    0H                                                               
         USING RCONTKEL,R6                                                      
         GOTO1 =V(HEXIN),DMCB,TKOCON,WORK,L'TKOCON                              
         CLC   RCONTKCN,WORK                                                    
         BE    UPD50                                                            
         MVC   P+46(32),=C'** TAKEOVER CONTRACT MISMATCH **'                    
         GOTO1 REPORT                                                           
         PUT   OUTDISK2,(R4)       NOT PROCESSED, SAVE IN OUTPUT FILE           
         L     RF,NUPDCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,NUPDCTR                                                       
         B     UPD05                                                            
*                                                                               
UPD50    DS    0H                                                               
         OC    RCONTKRC(26),RCONTKRC                                            
         BZ    UPD55                                                            
*                                                                               
         LA    R6,RECORD                                                        
         USING RCONREC,R6                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+34,L'RCONKCON,=C'TOG'                 
         DROP  R6                                                               
*                                                                               
         MVC   P+46(30),=C'** CONTRACT ALREADY UPDATED **'                      
         GOTO1 REPORT                                                           
         L     RF,SKIPCTR          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,SKIPCTR                                                       
         B     UPD05               ALREADY PROCESSED, SKIP AND DISCARD          
*                                                                               
UPD55    DS    0H                                                               
         USING RCONTKEL,R6                                                      
         MVC   RCONTKRC,TKOSID                                                  
         MVC   RCONTKRT,TKORTN                                                  
         DROP  R6                                                               
*                                                                               
         LA    R6,RECORD                                                        
         USING RCONREC,R6                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+34,L'RCONKCON,=C'TOG'                 
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'1C'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'1D'                                                      
         BNE   UPD60                                                            
         USING RCONDREL,R6                                                      
         GOTO1 =V(HEXIN),DMCB,TKODAR,RCONDRLK,L'TKODAR                          
         OI    RCONDRFG,X'01'          FLAG AS TAKEOVER                         
         B     UPD70                                                            
         DROP  R6                                                               
*                                                                               
UPD60    DS    0H                                                               
         XC    ELEM,ELEM                                                        
ELM      USING RCONDREL,ELEM                                                    
         MVI   ELM.RCONDRCD,X'1D'                                               
         MVI   ELM.RCONDRLN,RCONDL2Q                                            
         GOTO1 =V(HEXIN),DMCB,TKODAR,ELM.RCONDRLK,L'TKODAR                      
         MVI   ELM.RCONDRFG,X'01'      FLAG AS TAKEOVER                         
         DROP  ELM                                                              
*                                                                               
         GOTO1 =V(RECUP),DMCB,(2,AREC),ELEM,(R6)                                
*                                                                               
UPD70    DS    0H                                                               
         MVC   P+46(32),=C'** CONTRACT FOUND AND UPDATED **'                    
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QUESTOR+1,C'T'      NO UPDATE?                                   
         BNE   UPD75                                                            
*        GOTO1 DISPPUT                                                          
         B     UPD80                                                            
*                                                                               
UPD75    DS    0H                                                               
         GOTO1 PUTRECRD                                                         
*                                                                               
UPD80    DS    0H                                                               
         L     RF,CONCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
                                                                                
         B     UPD05                                                            
                                                                                
UPDX     DS    0H                                                               
         CLOSE INDISK1                                                          
         CLOSE OUTDISK2                                                         
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* DUMPFILE: DUMP UNPROCESSED RECORDS FROM INDISK2 TO OUTDISK1                   
******************************************************************              
DUMPFILE NTR1                                                                   
         CLI   QUESTOR+1,C'T'      IF TEST RUN, DON'T DUMP THE FILE             
         BE    DUMPX                                                            
         OPEN  (INDISK2,(INPUT))                                                
         OPEN  (OUTDISK1,(OUTPUT))                                              
         L     R4,ARECAREA                                                      
*                                                                               
DUMP10   DS    0H                                                               
         GET   INDISK2,(R4)                                                     
         PUT   OUTDISK1,(R4)                                                    
         B     DUMP10                                                           
*                                                                               
DUMP20   DS    0H                                                               
         CLOSE INDISK2                                                          
         CLOSE OUTDISK1                                                         
*                                                                               
DUMPX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'CONTRACTS UPDATED:'                                   
         EDIT  CONCTR,(12,P+28),COMMAS=YES,ZERO=NOBLANK                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'CONTRACTS NOT UPDATED:'                               
         EDIT  NUPDCTR,(12,P+28),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(26),=C'CONTRACTS ALREADY UPDATED:'                           
         EDIT  SKIPCTR,(12,P+28),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPEXTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(18),=C'RECORDS EXTRACTED:'                                   
         EDIT  CONCTR,(12,P+28),COMMAS=YES,ZERO=NOBLANK                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         LA    R4,RECORD                                                        
         ZICM  RF,RECORD+27,2                                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PUTRECS: GENERATE OUTFILE ENTRIES TO TAPE                     *              
******************************************************************              
PUTRECS  NTR1                                                                   
*        LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,64               SET LENGTH OF 64                             
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         B     EXIT                                                             
*                                                                               
******************************************************************              
*  PUTRECRD: UPDATE RECORD IN REPFILE                            *              
******************************************************************              
*                                                                               
PUTRECRD NTR1                                                                   
         LA    R6,PUTREC                                                        
         B     LINKFILE               TO RECORD CONTROL                         
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
*                                                                               
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD NTR1                                                                   
         LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,RECORD,      X        
               (0,DMWORK)                                                       
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ELCODE   DS    CL1                                                              
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
UTLADDR  DS    A                                                                
UTLSTUFF DS    CL5                                                              
ELEM     DS    CL256                                                            
MYWORK   DS    CL256                                                            
FEND     DS    CL6                                                              
OLDREP   DS    CL3                                                              
*                                                                               
CONBYTES DS    F                   PUT ALL COUNTERS HERE FOR MASS CLEAR         
CONCTR   DS    F                                                                
NUPDCTR  DS    F                                                                
SKIPCTR  DS    F                                                                
OTHERCTR DS    F                                                                
CTRLENQ  EQU   *-CONBYTES                                                       
*                                                                               
AREC     DC    A(RECORD)                                                        
*                                                                               
* 8K BLOCK SIZE, 9 TRACK, 1600 BPI TAPE ??????                                  
*                                                                               
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=64,BLKSIZE=8192,BUFNO=2                                    
INDISK1  DCB   DDNAME=INDISK1,DSORG=PS,RECFM=VB,LRECL=64,              X        
               BLKSIZE=8192,MACRF=GM,EODAD=UPDX                                 
OUTDISK2 DCB   DDNAME=OUTDISK2,DSORG=PS,RECFM=VB,MACRF=PM,             X        
               LRECL=64,BLKSIZE=8192,BUFNO=2                                    
INDISK2  DCB   DDNAME=INDISK2,DSORG=PS,RECFM=VB,LRECL=64,              X        
               BLKSIZE=8192,MACRF=GM,EODAD=DUMP20                               
OUTDISK1 DCB   DDNAME=OUTDISK1,DSORG=PS,RECFM=VB,MACRF=PM,             X        
               LRECL=64,BLKSIZE=8192,BUFNO=2                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL(TKOLENQ)                                                      
         ORG   REC                                                              
TKOSTA   DS    CL6                                                              
TKOCON   DS    CL8                                                              
TKODAR   DS    CL8                                                              
TKOSID   DS    CL10                                                             
TKORTN   DS    CL16                                                             
TKORTC   DS    CL5                                                              
TKOLENQ  EQU   *-TKOSTA                                                         
*                                                                               
RECORD   DS    CL6008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE RECORD                                  
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145REREP1502S05/01/02'                                      
         END                                                                    
