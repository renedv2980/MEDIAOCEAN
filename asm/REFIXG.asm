*          DATA SET REFIXG     AT LEVEL 010 AS OF 08/31/00                      
*          DATA SET REFIXG     AT LEVEL 009 AS OF 05/06/99                      
*PHASE REFIXGA,*,NOAUTO                                                         
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'REFIX - REPFILE RECOVERY EXTRACT PROGRAM'                       
REFIX    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,REFIX,=V(REGSAVE)                                              
*                                                                               
         L     RC,=A(RECONWKC)                                                  
         USING RECONWK,RC                                                       
         LR    R2,RC                                                            
         AHI   R2,4096                                                          
         USING RECONWK+4096,R2                                                  
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
*&&DO                                                                           
*                                                                               
         LA    RE,REFIX             SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*&&                                                                             
*                                                                               
         DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     OPEN                                                             
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,27,A),FORMAT=BI,WORK=1)'                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4204'                                  
                                                                                
*                                                                               
OPEN     DS    0H                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         EJECT                                                                  
IN2      DS    0H                                                               
         BAS   RE,READREC                                                       
*                                                                               
         CLI   RFILTY,X'82'        TEST REPFILE                                 
         BNE   IN2                                                              
         CLI   RRECTY,X'03'        IGNORE ADDED RECORDS                         
         BE    IN2                                                              
         CLI   RKEY,X'12'          INVENTORY ONLY                               
         BE    ININV                                                            
         BNE   IN2                                                              
                                                                                
*                                                                               
ININV    DS    0H                                                               
         CLI   RRECTY,1            THIS HAD BETTER BE A COPY!!                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R0,FREC                                                          
         LHI   R1,FRECX-FREC                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR RECORD AREA                            
*                                                                               
         ZICM  RF,RECVHDR-4,2                                                   
         AHI   RF,-(L'RECVHDR)                                                  
         SLL   RF,16                                                            
         STCM  RF,15,FSPARE                                                     
         SRL   RF,16                                                            
                                                                                
         AHI   RF,-4                                                            
         LA    RE,RKEY                                                          
         LR    R1,RF                                                            
         LA    R0,FREC                                                          
         MVCL  R0,RE               MOVE "COPY" RECORD INTO RECORD AREA          
                                                                                
*                                                                               
         DS    0H                  LOOK FOR "CHANGE" RECORD                     
         BAS   RE,READREC                                                       
         CLI   RRECTY,2            THIS HAD BETTER BE A CHANGE!!                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   RKEY(L'RINVKEY),FKEY   IS THIS COPY/CHG CORRUPTED?               
         BE    IN2                     NOPE, SKIP THIS COPY/CHG PAIR            
*                                                                               
         DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',FSPARE                                   
         AP    SORTIN,=P'1'                                                     
         B     IN2                                                              
                                                                                
*                                                                               
ENDIN1   DS    0H                                                               
         CLOSE (RECVIN,)                                                        
         B     OUTPUT                                                           
                                                                                
*                                                                               
** OUTPUT "COPY" RECORD TO DISKFILE                                             
*                                                                               
OUTPUT   DS    0H                                                               
*                                                                               
OP2      DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)                                                      
         BZ    OPDONE                                                           
         AP    SORTOUT,=P'1'                                                    
*                                                                               
         DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,(R3),P,4,=C'TOG'                                 
         GOTO1 =V(HEXOUT),DMCB,4(R3),P+10,27,=C'TOG'                            
         GOTO1 =V(PRINTER)                                                      
                                                                                
         PUT   FILEOUT,(R3)                                                     
         AP    FILOUT,=P'1'                                                     
         B     OP2                                                              
                                                                                
*                                                                               
OPDONE   DS    0H                                                               
         CLOSE (FILEOUT,)                                                       
         B     REPORT                                                           
         EJECT                                                                  
                                                                                
                                                                                
REPORT   DS    0H                                                               
         MVC   LINE,=PL2'75'                                                    
         MVC   TITLE(19),=C'REPFILE RECONSTRUCT'                                
*                                                                               
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
RPT002   MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 =V(PRINTER)                                                      
         BXLE  R3,R4,RPT002                                                     
                                                                                
*                                                                               
         DC    H'0'                                                             
         XBASE                                                                  
                                                                                
                                                                                
                                                                                
READREC  NTR1                                                                   
         LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(R0)                                                      
                                                                                
         AP    RCVREAD,=P'1'                                                    
         XIT1                                                                   
                                                                                
*                                                                               
CNTRS    DS    0D                                                               
RCVREAD  DC    PL4'0',CL14'RECOVERY READS'                                      
SORTIN   DC    PL4'0',CL14'SORT IN'                                             
SORTOUT  DC    PL4'0',CL14'SORT OUT'                                            
FILOUT   DC    PL4'0',CL14'FILE OUT'                                            
CNTRSX   EQU   *-1                                                              
*                                                                               
         EJECT                                                                  
DUB      DS    D                                                                
SAVR1    DS    F                                                                
MYSEQ    DS    F                                                                
SAVSIN   DS    F                                                                
NEWKEYSW DC    X'00'                                                            
ANSWER   DS    D                                                                
DMCB     DS    6F                                                               
CONNUM   DS    XL4                                                              
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
SORTREC  DS    XL(27+4+4+27)                                                    
SVSRTREC DS    XL(L'SORTREC)                                                    
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,          DOS SYS014                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04200,                                            X        
               MACRF=GM,                                               X        
               EODAD=ENDIN1                                                     
*&&DO                                                                           
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=137,                                              X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
*&&                                                                             
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,         DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         DROP  R2,R7,RB,RC                                                      
         EJECT                                                                  
RECONWK  DSECT                                                                  
*                                                                               
AKEYTAB  DS    A                   A(KEYTABLE)                                  
*                                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL27     ***** HARD CODE KEY LENGTH HERE *****                   
RSEQ     DS    CL3                                                              
                                                                                
*                                                                               
         ORG   RECONWK+X'100'                                                   
RSPARE   DS    CL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0C                                                               
         DS    4200C                                                            
         SPACE 3                                                                
         DS    0D                                                               
         DS     F                                                               
FSPARE   DS     F                  ROOM FOR LENGTH                              
FKEY     DS    0C                                                               
FREC     DS    4200C                                                            
FRECX    EQU   *                                                                
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    0C                                                               
         SPACE 3                                                                
RINVRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
RECONWKC CSECT                                                                  
         DS    4000D                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REFIXG    08/31/00'                                      
         END                                                                    
