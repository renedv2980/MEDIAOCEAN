*          DATA SET REREP2402A AT LEVEL 243 AS OF 09/09/99                      
*PHASE RE2402A,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO UPDATE CONFIRMED EDI DARE RECORDS'                    
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 08SEP99 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         XC    KEY,KEY             READ ALL EDI DARE RECORDS                    
         MVI   RDARKTYP,X'41'                                                   
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
*        MVC   P(8),=C'DMRSEQ:'                                                 
*        MVC   P+10(27),KEY                                                     
*        GOTO1 REPORT                                                           
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLI   KEY,X'41'                                                        
         BNE   PCX                                                              
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         CLC   =C'ED',RDARKAGY     EDI AGENCY                                   
         BNE   PCSEQ                                                            
         CLI   RDARKRT,X'10'       HEADER ONLY                                  
         BNE   PCSEQ                                                            
         MVC   P(13),=C'EDI DARE REC:'                                          
         MVC   P+15(32),KEY                                                     
         GOTO1 =V(HEXOUT),DMCB,KEY+20,P+40,4                                    
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        MVC   P(6),=C'GETREC'                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         CLC   =C'$EDI$',RDARAGAD                                               
         BNE   PCSEQ                                                            
*                                                                               
         MVC   DARKEY,IOAREA                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),RDARKREP                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),RDARREP#                                                  
         MVI   DUB+4,X'0F'                                                      
         SRP   DUB(5),64-1,0                                                    
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB(5)                                                
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMP                
                                                                                
         MVC   KEY+23(4),WORK                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PC100                                                            
         MVC   P(8),=C'MISSING:'                                                
         MVC   P+10(27),KEY                                                     
         GOTO1 REPORT                                                           
         B     PC200                                                            
*                                                                               
PC100    DS    0H                                                               
*        MVC   P(14),=C'FOUND CONTRACT'                                         
*        GOTO1 REPORT                                                           
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,CONAREA,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CONAREA                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   PC200                                                            
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'04'+X'02'                                             
         BZ    PC200                                                            
         DROP  R6                                                               
*                                                                               
         LA    R6,CONAREA                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   PC200                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    PC200                                                            
         DROP  R6                                                               
*                                                                               
         MVC   KEY,DARKEY                                                       
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PC160                                                            
         DC    H'0'                                                             
*                                                                               
PC150    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   PC200                                                            
*                                                                               
PC160    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(8),=C'DELETING'                                                
         MVC   P+10(32),IOAREA                                                  
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,IOAREA                                                        
         USING RDARREC,R6                                                       
         OI    RDARCNTL,X'80'      DELETE 41 RECORD                             
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         MVI   KEY+27,X'80'                                                     
         GOTO1 DATAMGR,DMCB,DMWRT,REPDIR,KEY,KEY,0                              
         CLI   RDARKRT,X'10'       HEADER ONLY                                  
         BNE   PC150                                                            
         AP    COUNT,=P'1'                                                      
         B     PC150                                                            
*                                                                               
PC200    DS    0H                                                               
         MVC   KEY(27),DARKEY                                                   
*                                                                               
         MVC   P(10),=C'RESTORING:'                                             
         MVC   P+12(27),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PCSEQ                                                            
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
PCX      DS    0H                                                               
         MVC   P(6),=C'PURGED'                                                  
         EDIT  (P5,COUNT),(7,P+11)                                              
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
COUNT    DC    PL5'0'                                                           
KCOUNT   DC    PL5'0'                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*EPLIST  DC    C'CMD4GPIFI1I2I8I9MGRMS1TO'                                      
*                                                                               
         LTORG                                                                  
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
DARKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
SVCONNUM DS    CL4                                                              
*                                                                               
IOAREA   DS    CL4000                                                           
CONAREA  DS    CL4000                                                           
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
*      ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'243REREP2402A09/09/99'                                      
         END                                                                    
