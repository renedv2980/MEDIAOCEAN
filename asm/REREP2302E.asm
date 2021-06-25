*          DATA SET REREP2302E AT LEVEL 005 AS OF 05/01/02                      
*PHASE RE2302E,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO CHECK OLD SYTLE INV RECORDS'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
*                                                                 *             
*        THIS MODULE CHECKS THE FOLLOWING:                        *             
*              1. OLD STYLE INVENTORY (SPARE IN 4TH INV #)        *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2302,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
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
         XC    SAVEKEY,SAVEKEY                                                  
         XC    COUNT,COUNT                                                      
         XC    TOTAL,TOTAL                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,DHIGH                                                         
         B     PC20                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,DSEQ                                                          
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
         CLI   RINVKSRC,C'M'                                                    
         BE    PCSEQ                                                            
         CLI   RINVKSRC,C'S'                                                    
         BE    PCSEQ                                                            
         CLI   RINVKINV+3,0        OLD STYLE?                                   
         BNE   PCSEQ                                                            
*                                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(3),RINVKINV                                                 
         GOTO1 HEXOUT,DMCB,RINVKINV,P+16,3,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+30)                                
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PC200    L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
DHIGH    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DSEQ     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DGETREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DADDREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,ADDREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DPUTREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DDMWRT   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY,0                          
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
         DC    CL4'DATA'                                                        
COUNT    DS    F                                                                
TOTAL    DS    F                                                                
SAVEKEY  DS    CL27                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2302,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
         DC    CL8'XXXXXXXX'                                                    
IO       DS    XL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP2302E05/01/02'                                      
         END                                                                    
