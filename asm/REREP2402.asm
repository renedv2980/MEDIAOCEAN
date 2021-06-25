*          DATA SET REREP2402  AT LEVEL 183 AS OF 01/05/96                      
*PHASE RE2402A,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIX REP TO SPOT TRANSER BUG'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 03JAN96 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
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
         LA    R3,REPLIST                                                       
*                                                                               
PC05     DS    0H                                                               
         LA    R4,KEY                                                           
         USING RBUYKEY,R4                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,0(R3)                                                   
         DROP  R4                                                               
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLC   KEYSAVE(RBUYKCON-RBUYREC),KEY                                    
         BE    PC25                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BE    PCX                                                              
         B     PC05                                                             
*                                                                               
PC25     DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING RBUYSPEL,R6                                                      
         OC    RBUYSPST,RBUYSPST                                                
         BNZ   PCSEQ                                                            
         DROP  R6                                                               
*                                                                               
PC28     DS    0H                                                               
         MVC   BUYKEY,KEY          SAVE OFF BUY KEY                             
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         MVC   P+1(2),RBUYKREP                                                  
         EDIT  RBUYKLIN,(3,P+4)                                                 
         MVC   P+9(4),=C'BUY:'                                                  
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),RBUYKCON CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+10(5),WORK+5(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+10(5)   CHANGE TO PWOS                              
         PACK  CONNUM(1),WORK+3(1)                                              
         PACK  CONNUM+1(1),WORK+2(1)                                            
         PACK  CONNUM+2(1),WORK+1(1)                                            
         PACK  CONNUM+3(1),WORK(1)                                              
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,CONNUM,P+14,4                                    
*                                                                               
         MVC   P+24(4),=C'D/A:'                                                 
         GOTO1 =V(HEXOUT),DMCB,KEY+28,P+29,4                                    
*                                                                               
         AP    COUNT,=P'1'                                                      
*                                                                               
         LA    R6,IOAREA                                                        
         USING RBUYREC,R6                                                       
         PACK  CONNUM(1),RBUYKCON+3(1)                                          
         PACK  CONNUM+1(1),RBUYKCON+2(1)                                        
         PACK  CONNUM+2(1),RBUYKCON+1(1)                                        
         PACK  CONNUM+3(1),RBUYKCON(1)                                          
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCONREC,R4                                                       
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,RBUYKREP                                                
         MVC   RCONPCON,CONNUM                                                  
         DROP  R4,R6                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,CONAREA,     X        
               DMWORK                                                           
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY,BUYKEY                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEYSAVE(RBUYKCON-RBUYREC),KEY                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R5,CONAREA                                                       
         USING RCONREC,R5                                                       
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
         USING RBUYSPEL,R6                                                      
         OC    RBUYSPST,RBUYSPST                                                
         BNZ   PCSEQ                                                            
         MVC   RBUYSPST,RCONKSTA                                                
         MVC   RBUYSADV,RCONKADV                                                
         MVC   RBUYSPRD,RCONPRD                                                 
         MVC   P+60(5),=C'*****'                                                
*                                                                               
PC40     DS    0H                                                               
         MVI   RBUYSPTM+L'RBUYSPTM,X'20'  FLIP FLOP                             
*                                                                               
         MVC   P+40(5),RBUYSPST                                                 
         MVC   P+47(4),RBUYSADV                                                 
         MVC   P+53(3),RBUYSPRD                                                 
         DROP  R5,R6                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC50     DS    0H                                                               
         GOTO1 REPORT                                                           
         B     PCSEQ                                                            
                                                                                
PCX      DS    0H                                                               
         MVC   P(6),=C'TOTAL:'                                                  
         EDIT  (P5,COUNT),(7,P+8)                                               
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
COUNT    DC    PL5'0'                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'CMD4GPIFI1I2I8I9MGRMS1TO'                                      
         DC    X'0000'                                                          
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
*                                                                               
IOAREA   DS    CL1000                                                           
CONAREA  DS    CL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
*      ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'183REREP2402 01/05/96'                                      
         END                                                                    
