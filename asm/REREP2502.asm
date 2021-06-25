*          DATA SET REREP2502  AT LEVEL 122 AS OF 01/23/98                      
*PHASE RE2502C,*                                                                
         TITLE 'MODULE TO CHANGE STRATEGY KEY'                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2502 (RE2502) --- CHANGE STRATEGY KEY               *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 10MAR94 (SKU) THIS IS A ONE TIME PROGRAM AND WILL BE DELETED    *             
*                                                                 *             
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1              *             
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*******************************************************************             
*                                                                               
RE2502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2502,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
*        MVI   RUNMODE,C'T'        TEST MODE, DO NOT UPDATE FILE                
         MVI   RUNMODE,C'R'        RUN MODE, UPDATES FILE                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
PC10     DS    0H                                                               
         ZAP   OLDCOUNT,=P'0'                                                   
         ZAP   NEWCOUNT,=P'0'                                                   
*                                                                               
* READ FOR OLD STRATEGY KEYS                                                    
*                                                                               
         XC    KEY,KEY             READ ALL STRATEGY RECORDS                    
         MVI   KEY,X'39'                                                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLI   KEY,X'39'                                                        
         BNE   PCEXIT                                                           
         CLC   KEY+2(2),=C'BL'                                                  
         BNE   PCSEQ                                                            
*                                                                               
         MVC   P(3),=C'IN:'                                                     
         AP    OLDCOUNT,=P'1'                                                   
         MVC   P+5(32),KEY                                                      
         GOTO1 REPORT                                                           
*                                                                               
         XC    STAKEY,STAKEY                                                    
         MVI   STAKEY,X'02'                                                     
         MVC   STAKEY+20(7),KEY+2  REP CODE+STATION CALL                        
         MVC   KEYSAVE,STAKEY                                                   
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,STAKEY,STAKEY,0               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   STAKEY(27),KEYSAVE                                               
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!!!                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,STAKEY+28,STAREC,   X        
               DMWORK                                                           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,STAREC                                                        
         USING RSTAREC,R6                                                       
*                                                                               
         XC    NEWKEY,NEWKEY                                                    
         MVI   NEWKEY,X'39'                                                     
         MVC   NEWKEY+12(3),KEY+1                                               
         MVC   NEWKEY+15(2),RSTAGRUP                                            
         MVC   NEWKEY+17(10),KEY+4                                              
         MVC   NEWKEY+28(4),KEY+28                                              
         DROP  R6                                                               
*                                                                               
         CLI   RUNMODE,C'T'                                                     
         BE    TEST10                                                           
         GOTO1 DATAMGR,DMCB,DMADD,REPDIR,NEWKEY,NEWKEY,0                        
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TEST10   DS    0H                                                               
         MVC   P(4),=C'ADD:'                                                    
         AP    NEWCOUNT,=P'1'                                                   
         MVC   P+5(32),NEWKEY                                                   
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),REPFILE,KEY+28,STRREC,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   STRREC(27),NEWKEY                                                
         CLI   RUNMODE,C'T'                                                     
         BE    TEST20                                                           
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,STRREC,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TEST20   DS    0H                                                               
         MVC   P(4),=C'REC:'                                                    
         MVC   P+5(50),STRREC                                                   
         GOTO1 REPORT                                                           
*                                                                               
         OI    KEY+27,X'80'                                                     
         CLI   RUNMODE,C'T'                                                     
         BE    TEST30                                                           
         GOTO1 DATAMGR,DMCB,DMWRT,REPDIR,KEY,KEY,0                              
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TEST30   DS    0H                                                               
         MVC   P(4),=C'DEL:'                                                    
         MVC   P+5(32),KEY                                                      
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCEXIT   DS    0H                                                               
         MVC   P+3(15),=C'TOTAL OLD KEYS'                                       
         EDIT  (P5,OLDCOUNT),(7,P+17)                                           
         GOTO1 REPORT                                                           
         MVC   P+3(15),=C'TOTAL NEW KEYS'                                       
         EDIT  (P5,NEWCOUNT),(7,P+17)                                           
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
COUNT    DC    PL5'0'                                                           
OLDCOUNT DC    PL5'0'                                                           
NEWCOUNT DC    PL5'0'                                                           
RELO     DS    A                                                                
STAKEY   DS    CL(L'KEY)                                                        
NEWKEY   DS    CL(L'KEY)                                                        
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
RUNMODE  DS    C                                                                
*                                                                               
STRREC   DS    CL1000                                                           
STAREC   DS    CL1000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1A                                                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122REREP2502 01/23/98'                                      
         END                                                                    
