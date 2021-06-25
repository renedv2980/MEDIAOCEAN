*          DATA SET SPREPFX2OU AT LEVEL 010 AS OF 05/09/00                      
*PHASE SPFX02B                                                                  
*===============================================================*               
* THIS PROGRAM READS AN EXTRACTED RECOVERY FILE AND RESTORES    *               
* COPY RECORDS TO SPTFILE                                       *               
*===============================================================*               
         TITLE 'SPFX02 - RESTORE CLOBBERED BUYS'                                
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX10     OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX12     LA    R0,RECVHDR-4        READ INPUT TAPE                              
         GET   FILEIN,(0)                                                       
*                                                                               
         CLC   =X'2101',RFILTY                                                  
         BNE   FX12                                                             
*&&DO                                                                           
         CLC   RSIN,=X'0001DA66'                                                
         BNE   FX12                                                             
*&&                                                                             
         LA    R4,RECVHDR+24       POINT TO BUYREC KEY                          
         CLI   0(R4),X'B2'         COKE RADIO CLOBBERED                         
         BE    *+6                                                              
         DC    H'0'                                                             
* RESTORE THIS RECORD                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(R4)       A-M/CLT/PRD/MKT/STA/EST                      
         MVC   KEY+11(1),10(R4)    LINE                                         
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    FX20                                                             
* NOT THERE - ADD IT                                                            
         LA    RE,RECVHDR+24       POINT TO RECOVERY BUY                        
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R6,ADBUY                                                         
         LA    R7,2(RF)            INSURANCE                                    
         MVCL  R6,RE                                                            
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX14                                                             
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,ADBUY,DMWORK          
*                                                                               
FX14     L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'AFTER',(R6),C'DUMP',(R0),=C'1D00'                 
         B     FX12                                                             
         EJECT                                                                  
FX20     GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BEFORE',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
         LA    RE,RECVHDR+24       POINT TO RECOVERY BUY                        
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R6,ADBUY                                                         
         LA    R7,2(RF)            INSURANCE                                    
         MVCL  R6,RE                                                            
*                                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'AFTER',(R6),C'DUMP',(R0),=C'1D00'                 
         B     FX12                                                             
*                                                                               
ENDIN    GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTIT    NTR1                                                                   
         MVC   P(1),QMED                                                        
         MVC   P+2(3),QCLT                                                      
         MVC   P+6(3),KEY+4                                                     
         SR    R0,R0                                                            
         ICM   R0,1,KEY+7                                                       
         BZ    PRTIT2                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(3),DUB                                                      
PRTIT2   EQU   *                                                                
         GOTO1 HEXOUT,DMCB,KEY,P+15,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
FLAG     DC    C'N'                                                             
         LTORG                                                                  
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=2008,             X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
         DS    F                   ROOM FOR RECLEN                              
       ++INCLUDE DMRCVRHDR                                                      
         DS    XL4000                                                           
         EJECT                                                                  
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
*                                                                               
*        PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPFX2OU05/09/00'                                      
         END                                                                    
