*          DATA SET REREP2402H AT LEVEL 104 AS OF 11/05/99                      
*PHASE RE2402H,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'SWEEP CONS FOR MISSING AGY/ADV'                                 
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE                          *             
*                                                                 *             
* --------------------------------------------------------------- *             
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
         ST    RA,DUMPLIST+8                                                    
         LHI   R0,4096                                                          
         ST    R0,DUMPLIST+12                                                   
         MVI   DUMPLIST+12,X'80'                                                
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
         XC    COUNTER,COUNTER                                                  
         XC    COUNTER1,COUNTER1                                                
*                                                                               
         LA    R6,KEY                                                           
         MVI   KEY,X'0C'                                                        
         MVC   KEY+2(2),QREP                                                    
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
*&&DO                                                                           
         L     RF,COUNTER                                                       
         LA    RF,1(RF)                                                         
         ST    RF,COUNTER                                                       
         L     RF,COUNTER1                                                      
         LA    RF,1(RF)                                                         
         ST    RF,COUNTER1                                                      
         CLC   COUNTER1,=F'10000'                                               
         BL    PC30                                                             
         XC    COUNTER1,COUNTER1                                                
         MVC   P+1(16),=C'PROCESSING RECS:'                                     
         EDIT  COUNTER,(7,P+20)                                                 
         MVC   P+30(32),KEY                                                     
         GOTO1 REPORT                                                           
*&&                                                                             
PC30     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PCX                                                              
*                                                                               
         OC    P,SPACES                                                         
*                                                                               
         MVC   WORK(6),KEY+13                                                   
         BAS   RE,CKAGY                                                         
         BNE   PC40                                                             
         GOTO1 HEXOUT,DMCB,KEY+23,P,4                                           
         MVC   P+8(6),WORK                                                      
*                                                                               
PC40     MVC   WORK(4),KEY+19                                                   
         BAS   RE,CKADV                                                         
         BNE   PC50                                                             
         GOTO1 HEXOUT,DMCB,KEY+23,P,4                                           
         MVC   P+14(4),WORK                                                     
*                                                                               
PC50     DS    0H                                                               
         CLC   P,SPACES                                                         
         BE    PCSEQ                                                            
         GOTO1 REPORT                                                           
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
NZ       EQU   *                                                                
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
Z        EQU   *                                                                
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
CKAGY    NTR1                                                                   
         LA    R4,AGYTAB                                                        
CKAGY5   CLI   0(R4),X'FF'                                                      
         BE    EXITL               NOT IN TABLE                                 
         CLC   WORK(L'AGYTAB),0(R4)                                             
         BE    EXITOK              MATCH                                        
         LA    R4,L'AGYTAB(R4)                                                  
         B     CKAGY5                                                           
*                                                                               
CKADV    NTR1                                                                   
         LA    R4,ADVTAB                                                        
CKADV5   CLI   0(R4),X'FF'                                                      
         BE    EXITL               NOT IN TABLE                                 
         CLC   WORK(L'ADVTAB),0(R4)                                             
         BE    EXITOK              MATCH                                        
         LA    R4,L'ADVTAB(R4)                                                  
         B     CKADV5                                                           
*                                                                               
AGYTAB   DS    0CL6                                                             
         DC    CL6'0083  '                                                      
         DC    CL6'RJWACL'                                                      
         DC    CL6'DEUTNY'                                                      
         DC    CL6'KINGPH'                                                      
         DC    X'FF'                                                            
ADVTAB   DS    0CL4                                                             
         DC    CL4'REGS'                                                        
         DC    CL4'PVAN'                                                        
         DC    CL4'D180'                                                        
         DC    CL4'N437'                                                        
         DC    X'FF'                                                            
*                                                                               
COUNT    DS    F                                                                
COUNTER  DS    F                                                                
COUNTER1 DS    F                                                                
SVKEY    DS    CL27                                                             
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         DC    A(0,0)                                                           
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*                                                                               
RELO     DS    A                                                                
AAGYTAB  DS    A                                                                
AADVTAB  DS    V                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
MYKEY    DS    CL32                                                             
MYKEYSV  DS    CL32                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104REREP2402H11/05/99'                                      
         END                                                                    
