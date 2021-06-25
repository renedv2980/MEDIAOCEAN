*          DATA SET SPREPFX02B AT LEVEL 012 AS OF 04/05/00                      
*          DATA SET SPREPFXRP  AT LEVEL 004 AS OF 11/24/98                      
*PHASE SPFX02B                                                                  
         TITLE 'SPFX02 - MERGE STABUCK RECORDS FOR WRLA'                        
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
FX12     L     R7,ADCLT            READ INPUT TAPE                              
         AHI   R7,-4                                                            
         GET   FILEIN,(7)                                                       
*                                                                               
         LH    R0,0(R7)                                                         
         AR    R7,R0                                                            
         XC    0(256,R7),0(R7)                                                  
*                                                                               
FX14     L     R7,ADCLT                                                         
         MVC   KEY(13),0(R7)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),0(R7)       MATCH 0E01 REC                               
         BE    FX30                GO MERGE                                     
*                                                                               
FX20     L     R0,ADCLT            ADD RECORD TO FILE                           
         ST    R0,AREC                                                          
         GOTO1 ADD                                                              
         L     R6,ADCLT                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'ADDED 0E01',(R6),C'DUMP',(R0),=C'1D00'            
         B     FX12                AND GET NEXT TAPE RECORD                     
*                                                                               
* RECORDS EQUAL - ADD ANY TAPE ELEMENTS NOT IN FILE RECORD                      
*                                                                               
FX30     GOTO1 GETBUY              READ FILE REC INTO ADBUY                     
*                                                                               
         MVI   FLAG,C'N'           SET NOT CHANGED FLAG                         
         L     R7,ADCLT            POINT TO TAPE RECORD                         
         LA    R7,24(R7)                                                        
*                                                                               
FX32     BAS   RE,CHKFILE                                                       
         BNE   FX34                ELEM NOT THERE - NEED TO ADD IT              
* FOUND IT - CHECK NEXT TAPE ELEMENT                                            
         SR    R0,R0                                                            
         ICM   R0,1,1(R7)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   FX32                                                             
         B     FX40                                                             
*                                                                               
FX34     MVI   FLAG,C'Y'           SET FLAG TO WRITE RECORD                     
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
*                                                                               
FX36     CLC   0(6,R7),0(R6)                                                    
         BL    FX38                                                             
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   FX36                                                             
FX38     GOTO1 RECUP,DMCB,ADBUY,(R7),(R6)                                       
         B     FX32                                                             
*                                                                               
FX40     CLI   FLAG,C'Y'                                                        
         BE    FX41                                                             
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'NO CHANGE',(R6),C'DUMP',(R0),=C'1D00'             
         B     FX12                                                             
*                                                                               
FX41     CLI   RCWRITE,C'Y'                                                     
         BNE   FX42                                                             
         GOTO1 PUTBUY                                                           
*                                                                               
FX42     L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'CHANGED 0E01',(R6),C'DUMP',(R0),=C'1D00'          
         B     FX12                                                             
*                                                                               
CHKFILE  NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
CHKFIL2  CLC   0(6,R6),0(R7)       MATCH ELCODE/LEN/MOS/BILLDATE                
         BE    CHKFILEQ                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CHKFIL2                                                          
CHKFILNQ LTR   RE,RE                                                            
         B     *+6                                                              
CHKFILEQ CR    RE,RE                                                            
         XIT1                                                                   
*        EJECT                                                                  
ENDIN    CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AENDREQ                                                          
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
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPFX02B04/05/00'                                      
         END                                                                    
