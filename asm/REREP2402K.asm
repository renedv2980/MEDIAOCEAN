*          DATA SET REREP2402K AT LEVEL 255 AS OF 11/19/97                      
*PHASE RE2402K,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND BUCKET BUG'                                      
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
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,=C'MG'                                                  
         DROP  R6                                                               
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
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PCX                                                              
         MVC   CONKEY,KEY                                                       
         XC    TOTAL,TOTAL                                                      
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         TM    RCONMODR+1,X'20'                                                 
         BO    PCSEQ                                                            
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         USING RCONBKEL,R6                                                      
PC30     ZICM  R3,RCONBKAM,4                                                    
         ZICM  R4,TOTAL,4                                                       
         AR    R3,R4                                                            
         STCM  R3,15,TOTAL                                                      
         DROP  R6                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PC30                                                             
*                                                                               
         OC    TOTAL,TOTAL                                                      
         BZ    PCSEQ                                                            
*                                                                               
         XC    BUYKEY,BUYKEY                                                    
         LA    R5,BUYKEY                                                        
         USING RBUYREC,R5                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,=C'MG'                                                  
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         ZAP   WORK(5),=P'0'       CHANGE FROM PWOS TO PWS                      
         MVO   WORK(5),RCONKCON                                                 
         ZAP   WORK+10(5),=P'99999999' GET 9'S COMPLEMENT                       
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)  CHANGE TO PWOS                               
         DROP  R6                                                               
*                                                                               
         PACK  BUYKEY+18(1),WORK+3(1) REVERSE THE COMPLIMENT                    
         PACK  BUYKEY+19(1),WORK+2(1)                                           
         PACK  BUYKEY+20(1),WORK+1(1)                                           
         PACK  BUYKEY+21(1),WORK(1)                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),BUYKEY                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BE    PC40                                                             
*                                                                               
         LA    R6,CONKEY                                                        
         USING RCONKEY,R6                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P,4                                     
         MVC   P+10(2),RCONKREP                                                 
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
PC40     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),CONKEY                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     PCSEQ                                                            
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
**PAN#1  DC    CL21'255REREP2402K11/19/97'                                      
         END                                                                    
