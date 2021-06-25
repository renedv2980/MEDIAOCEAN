*          DATA SET REREP2402G AT LEVEL 012 AS OF 05/12/99                      
*PHASE RE2402G,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE GETKSRC                                                                
         TITLE 'MODULE TO FIND DUPLICATE INVENTORY RECORDS'                     
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 07MAY99 (GL ) DATE OF CONCEPTION                                *             
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
         XC    TOTAL,TOTAL                                                      
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
*                                                                               
PCGET    DS    0H                                                               
         LA    R4,ININVREC-4                                                    
         GET   FILEIN,(R4)                                                      
         AP    READIN,=P'1'                                                     
*                                                                               
         XC    KEY,KEY             READ ALL INVENTORY RECORDS                   
         MVC   KEY(27),ININVREC                                                 
*&&DO                                                                           
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PC25                                                             
         MVC   P(6),=C'FOUND:'                                                  
         GOTO1 =V(HEXOUT),DMCB,KEYSAVE,P+08,27,=C'TOG'                          
         GOTO1 REPORT                                                           
         AP    CNTFOUND,=P'1'                                                   
         B     PCGET                                                            
*&&                                                                             
*                                                                               
PC25     DS    0H                                                               
         CLC   KEYSAVE(24),KEY                                                  
         BE    PC25B                                                            
         GOTO1 REPORT                                                           
PC25B    EQU   *                                                                
*                                                                               
         MVC   P(6),=C'WROTE:'                                                  
*&&DO                                                                           
         LA    R5,KEYSAVE                                                       
*&&                                                                             
         LA    R5,KEY                                                           
         USING RINVREC,R5                                                       
         MVC   P+08(2),RINVKREP                                                 
         MVC   P+11(5),RINVKSTA                                                 
         MVC   P+17(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+22),0                              
                                                                                
         XC    MYWORK,MYWORK                                                    
         MVC   MYWORK+2(1),RINVKSRC                                             
         GOTO1 =V(GETKSRC),DMCB,(C'K',MYWORK),MYWORK                            
         MVC   P+31(1),MYWORK+0                                                 
                                                                                
         GOTO1 DATCON,DMCB,(3,RINVKBK),(6,P+33),0                               
         CLI   MYWORK+4,C' '                                                    
         BNH   *+10                                                             
         MVI   P+39,C'('                                                        
         MVC   P+40(1),MYWORK+4                                                 
         MVI   P+41,C')'                                                        
         LA    R0,P                                                             
         PUT   FILEOUT,(R0)                                                     
         GOTO1 REPORT                                                           
         DROP  R5                                                               
                                                                                
*&&DO                                                                           
         PUT   FILEOUT,(R4)                                                     
*&&                                                                             
         MVC   KEYSAVE,KEY                                                      
         AP    CNTWROTE,=P'1'                                                   
         B     PCGET                                                            
*                                                                               
PCX      DS    0H                                                               
         CLOSE (FILEIN,)                                                        
         CLOSE (FILEOUT,)                                                       
*                                                                               
EXIT     DS    0H                                                               
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
RPT002   MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 REPORT                                                           
         BXLE  R3,R4,RPT002                                                     
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               MACRF=GM,                                               X        
               EODAD=PCX                                                        
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,         DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=8000,                                           X        
               MACRF=PM                                                         
*                                                                               
*                                                                               
CNTRS    DS    0D                                                               
READIN   DC    PL4'0',CL14'FILEIN READS'                                        
CNTFOUND DC    PL4'0',CL14'TRACKS FOUND'                                        
CNTWROTE DC    PL4'0',CL14'TRACKS WRITTEN'                                      
FILOUT   DC    PL4'0',CL14'FILE OUT'                                            
CNTRSX   EQU   *-1                                                              
*                                                                               
REPLACED DC    PL5'0'                                                           
NOGOOD   DC    PL5'0'                                                           
EXACTC   DC    PL5'0'                                                           
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
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
TOTAL    DS    XL4                                                              
MYWORK   DS    CL256                                                            
         DS    D                                                                
ININVREC DS    CL4000                                                           
*                                                                               
INVAREA  DS    CL4000                                                           
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
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REREP2402G05/12/99'                                      
         END                                                                    
