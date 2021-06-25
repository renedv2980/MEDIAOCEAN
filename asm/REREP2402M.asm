*          DATA SET REREP2402M AT LEVEL 008 AS OF 04/05/99                      
*PHASE RE2402M,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND DUPLICATE CONTRACTS'                             
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 11NOV97 (SKU) DATE OF CONCEPTION                                *             
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
         XC    TOTAL,TOTAL                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RCON8TYP,X'8D'                                                   
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
         CLI   KEY,X'8D'                                                        
         BNE   PCX                                                              
         CLI   RCON8RID,1                                                       
         BE    PC40                                                             
PC30     GOTO1 =V(HEXOUT),DMCB,RCON8RID,P,1                                     
         GOTO1 =V(HEXOUT),DMCB,RCON8CON,P+4,4                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+20,27                                      
         GOTO1 REPORT                                                           
PC40     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   RCON8RID,2                                                       
         BNE   PC30                                                             
*                                                                               
PC50     DS    0H                                                               
         CLI   RCON8RID,2                                                       
         BE    PC70                                                             
PC60     GOTO1 =V(HEXOUT),DMCB,RCON8RID,P,1                                     
         GOTO1 =V(HEXOUT),DMCB,RCON8CON,P+4,4                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+20,27                                      
         GOTO1 REPORT                                                           
PC70     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   RCON8RID,3                                                       
         BNE   PC60                                                             
*                                                                               
PC80     DS    0H                                                               
         CLI   RCON8RID,3                                                       
         BE    PC100                                                            
PC90     GOTO1 =V(HEXOUT),DMCB,RCON8RID,P,1                                     
         GOTO1 =V(HEXOUT),DMCB,RCON8CON,P+4,4                                   
         GOTO1 =V(HEXOUT),DMCB,KEY,P+20,27                                      
         GOTO1 REPORT                                                           
PC100    MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'8D'                                                        
         BNE   PCX                                                              
         CLI   RCON8RID,1                                                       
         BNE   PC90                                                             
         B     PC20                                                             
*                                                                               
PCX      DS    0H                                                               
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
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
SVCONNUM DS    CL4                                                              
TOTAL    DS    XL4                                                              
MYWORK   DS    CL256                                                            
*                                                                               
IOAREA   DS    CL2000                                                           
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
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREP2402M04/05/99'                                      
         END                                                                    
