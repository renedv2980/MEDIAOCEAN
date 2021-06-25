*          DATA SET REREP3S02C AT LEVEL 003 AS OF 05/01/02                      
*PHASE RE7C02A,*                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'REREP7C02 -- GENERAL CONTRACT PROCESS'                          
*                                                                               
*- REREP7C02 -- GENERAL CONTRACT PROCESS                                        
*                                                                               
*        DOES ALL PROCESSING ON THE 'RUNFRST' MODE                              
*                                                                               
*        READS THE ALL THE CONTRACT RECORDS LOOKING FOR SPECIFIC                
*              DATA                                                             
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
RE7C02   CSECT                                                                  
         NMOD1 0,**RE7C02,R9,RR=R5                                              
*                                                                               
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         LA    RC,RECAREA                                                       
         USING RECS,RC                                                          
*                                                                               
*- CHECK FOR VALID PROCESSING MODE.                                             
         CLI   MODE,REQFRST                                                     
         BE    PROC                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
*- PROC                                                                         
PROC     BAS   RE,INITIAL          STARTUP STUFF                                
         BAS   RE,FINDEM           READ DATA/BUILD SORTFILE                     
         GOTO1 REPORT                                                           
         OC    NUMRECS(4),NUMRECS                                               
         BNZ   MAIN10                                                           
         MVC   P(L'NODATA),NODATA                                               
         GOTO1 REPORT                                                           
         B     MAIN40                                                           
MAIN10   EQU   *                                                                
         MVC   P(L'#RECS),#RECS                                                 
         LA    R3,P+L#RECS                                                      
         EDIT  (B4,NUMRECS),(9,0(R3))                                           
         GOTO1 REPORT                                                           
         MVC   P(L'#ELS),#ELS                                                   
         LA    R3,P+L#ELS                                                       
         EDIT  (B4,NUMELS),(9,0(R3))                                            
         GOTO1 REPORT                                                           
         L     R4,NUMUP                                                         
         A     R4,NUMFOR                                                        
         LTR   R4,R4                                                            
         BNZ   MAIN20                                                           
         MVC   P(L'NOERRORS),NOERRORS                                           
         GOTO1 REPORT                                                           
         B     MAIN40                                                           
MAIN20   EQU   *                                                                
         MVC   P(L'#UP),#UP                                                     
         LA    R3,P+L#UP                                                        
         EDIT  (B4,NUMUP),(9,0(R3))                                             
         GOTO1 REPORT                                                           
         MVC   P(L'#FOR),#FOR                                                   
         LA    R3,P+L#FOR                                                       
         EDIT  (B4,NUMFOR),(9,0(R3))                                            
         GOTO1 REPORT                                                           
         MVC   P(L'#ERRS),#ERRS                                                 
         LA    R3,P+L#ERRS                                                      
         EDIT  (R4),(9,0(R3))                                                   
         GOTO1 REPORT                                                           
         MVC   P(L'#ERRREC),#ERRREC                                             
         LA    R3,P+L#ERRREC                                                    
         EDIT  (B4,NUMEREC),(9,0(R3))                                           
         GOTO1 REPORT                                                           
MAIN40   EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P(13),=C'END OF REPORT'                                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- REPORT INITIAL                                                               
INITIAL  NTR1                                                                   
         MVI   SORTSW,C'N'                                                      
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         LA    R2,IOAREA                                                        
         ST    R2,AIOAREA                                                       
         MVC   P(132),SPACES                                                    
         XC    NUMRECS,NUMRECS                                                  
         XC    NUMELS,NUMELS                                                    
         XC    NUMUP,NUMUP                                                      
         XC    NUMFOR,NUMFOR                                                    
         XC    NUMEREC,NUMEREC                                                  
         XC    ERRORDA(4),ERRORDA                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*- FINDEM -- READ KEYS FOR EACH RECORD FOR EACH REP IN REPLIST.                 
*            BUILD SORT FILE.                                                   
*                                                                               
FINDEM   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           READ VIA PRIMARY KEY                         
         CLC   QREP(2),=C'SJ'                                                   
         BE    FIND05                                                           
         MVC   KEY+2(2),QREP                                                    
FIND05   EQU   *                                                                
         BAS   RE,HIGH             START READING                                
         B     FIND20                                                           
FIND10   EQU   *                                                                
         BAS   RE,SEQ                                                           
FIND20   EQU   *                                                                
         CLI   KEY,X'0C'           STILL CONTRACT?                              
         BNE   FIND900             NOPE, GET OUT                                
         CLC   QREP(2),=C'SJ'                                                   
         BE    FIND30                                                           
         CLC   KEY+2(2),QREP                                                    
         BNE   FIND900             NOPE, GET OUT                                
FIND30   EQU   *                                                                
         BAS   RE,GREC                                                          
         L     R1,NUMRECS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NUMRECS                                                       
FIND100  EQU   *                                                                
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         B     FIND120                                                          
FIND110  EQU   *                                                                
         BAS   RE,NEXTEL                                                        
FIND120  EQU   *                                                                
         BNE   FIND10              DONE WITH THIS RECORD, GET NEXT              
         CLC   4(6,R6),=X'B6B400000000'        MONDAY MAY20/91                  
         BE    FIND200                                                          
         CLC   4(6,R6),=X'B6BB00000000'        MONDAY MAY27/91                  
         BE    FIND200                                                          
         CLC   4(6,R6),=X'B6C300000000'        MONDAY JUN3/91                   
         BE    FIND200                                                          
         CLC   4(6,R6),=X'B6CA00000000'        MONDAY JUN10/91                  
         BE    FIND200                                                          
         CLC   4(6,R6),=X'B6D100000000'        MONDAY JUN17/91                  
         BE    FIND200                                                          
         CLC   4(6,R6),=X'B6D800000000'        MONDAY JUN24/91                  
         BNE   FIND110             TRY NEXT ELEMENT                             
*                                                                               
FIND200  EQU   *                                                                
         L     R1,NUMELS                                                        
         LA    R1,1(R1)                                                         
         ST    R1,NUMELS                                                        
         LR    R7,R6                                                            
         SH    R7,=H'10'                                                        
         CLC   0(4,R6),0(R7)       ZERO'ING AN EXISTING MONTH?                  
         BNE   FIND210                                                          
         MVC   P+078(12),=C'<<< UPDATING'                                       
         L     R1,NUMUP                                                         
         LA    R1,1(R1)                                                         
         ST    R1,NUMUP                                                         
         B     FIND230                                                          
FIND210  EQU   *                                                                
         LA    R7,20(R7)                                                        
         CLC   0(4,R6),0(R7)                                                    
         BNE   FIND220                                                          
         MVC   P+078(11),=C'*** FORWARD'                                        
         L     R1,NUMFOR                                                        
         LA    R1,1(R1)                                                         
         ST    R1,NUMFOR                                                        
         B     FIND230                                                          
FIND220  EQU   *                                                                
         CLI   QOPTION1,C' '                                                    
         BE    FIND110                                                          
FIND230  EQU   *                                                                
         CLI   P+078,C' '                                                       
         BE    FIND240                                                          
         CLC   ERRORDA(4),KEY+28                                                
         BE    FIND240                                                          
         MVC   ERRORDA(4),KEY+28                                                
         L     R1,NUMEREC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,NUMEREC                                                       
FIND240  EQU   *                                                                
         MVC   P+000(5),=C'REP ='                                               
         MVC   P+007(2),IOAREA+2                                                
         MVC   P+011(17),=C'CONTRACT NUMBER ='                                  
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),IOAREA+23(4)                                             
         EDIT  (P5,WORK),(8,P+030),ALIGN=LEFT                                   
         MVC   P+040(5),=C'D/A ='                                               
         GOTO1 =V(HEXOUT),DMCB,KEY+28,P+046,4                                   
         GOTO1 =V(HEXOUT),DMCB,(R6),P+056,10                                    
         GOTO1 REPORT                                                           
         MVC   P(132),SPACES                                                    
         BNE   FIND110             TRY NEXT ELEMENT                             
*                                                                               
*                                                                               
*- FINISHED WITH KEY READ PHASE                                                 
FIND900  XIT1                      ALL DONE.  BACK TO MAIN                      
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,34,ELCODE                                                     
         PRINT NOGEN                                                            
         SPACE 2                                                                
*                                                                               
*- MESSAGE LITERALS                                                             
*                                                                               
NODATA   DC    C'** NO RECORDS FOUND **'                                        
NOERRORS DC    C'***** NO ERRORS FOUND *****'                                   
*                                                                               
#RECS    DC    C'NUMBER OF CONTRACTS READ     '                                 
#ELS     DC    C'NUMBER OF ELEMENTS SELECTED  '                                 
#UP      DC    C'NUMBER OF UPDATE ELEMENTS    '                                 
#FOR     DC    C'NUMBER OF FORWARD ELEMENTS   '                                 
#ERRS    DC    C'TOTAL NUMBER OF ERRORS       '                                 
#ERRREC  DC    C'NUMBER OF RECORDS IN ERROR   '                                 
*                                                                               
L#RECS   EQU   2+L'#RECS                                                        
L#ELS    EQU   2+L'#ELS                                                         
L#UP     EQU   2+L'#UP                                                          
L#FOR    EQU   2+L'#FOR                                                         
L#ERRS   EQU   2+L'#ERRS                                                        
L#ERRREC EQU   2+L'#ERRREC                                                      
         EJECT                                                                  
       ++INCLUDE RGENIO            DATA MANAGER INTERFACE                       
         EJECT                                                                  
RELO     DS    A                                                                
SORTSW   DS    CL1                 'Y' IF SORT DATA;'N' IF END                  
FOOTSW   DS    CL1                 'Y' TO PRINT FOOTERS                         
ERRSW    DS    CL1                 'Y' IF ERRORS FOUND                          
PRINTKEY DS    CL1                 C'Y' TO PRINT KEY INFO                       
MISSOUT  DS    CL1                                                              
DIFFDATA DS    CL1                                                              
ELCODE   DS    CL1                                                              
*                                                                               
         DS    0F                  FULL WORD VARIABLES                          
NUMRECS  DS    F                   NUMBER OF CONTRACTS READ                     
NUMELS   DS    F                   NUMBER OF ELEMETS SELECTED                   
NUMUP    DS    F                   NUMBER OF UPDATE ELEMENTS                    
NUMFOR   DS    F                   NUMBER OF FORWARD ELEMENTS                   
NUMEREC  DS    F                   NUMBER OF FORWARD ELEMENTS                   
*                                                                               
AOUT     DS    A                   A(OUTPUT FOR ERROR PRINTING)                 
*                                                                               
COMMAND  DS    CL8                 FOR DGENIO                                   
AIOAREA  DS    A                   A(IO BUFFER)                                 
*                                                                               
REP2ERR  DS    A                                                                
REP3ERR  DS    A                                                                
ADDENTRY DS    A                   A(DD TABLE ENTRY)                            
*                                                                               
*                                                                               
ERRORDA  DS    CL4                 DISK ADDR OF LAST REC IN ERROR               
SAVEKEY  DS    CL10                SORT REC KEY ID & DATA (NOT REP)             
*                                                                               
IOAREA   DS    1008X               IO BUFFER                                    
IO       EQU   IOAREA              A SHORTHAND WAY TO SAY IOAREA                
*                                                                               
         LTORG                                                                  
*                                                                               
*- SPACE TO READ IN 3 RECORDS AT A TIME                                         
         DS    0F                                                               
RECAREA  DS    (3*1008)X                                                        
         SPACE                                                                  
*                                                                               
*- ORG RECORDS TO IOAREA TO GET LABELS.                                         
         SPACE 2                                                                
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
******   PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
RECS     DSECT                     RECORD HOLD AREAS FOR COMPARES               
*                                                                               
REC1     DS    1008X               NEED 1 BUFFER PER REP FOR COMPARES           
REC2     DS    1008X                                                            
REC3     DS    1008X                                                            
*                                                                               
VDATAMGR EQU   DATAMGR                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REREP3S02C05/01/02'                                      
         END                                                                    
