*          DATA SET ACEXTHST   AT LEVEL 004 AS OF 02/24/93                      
*PHASE ACEXTHST,+0                                                              
         TITLE 'REMOVE PEELED ITEMS AND CREATE A HISTORY FILE'                  
AEXTHST  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*AEXTHST*                                                      
         LR    R9,R1                                                            
         USING ACCWORKD,R9                                                      
         L     R2,AIOAREA                                                       
         LA    R3,4(R2)                                                         
         USING ACKEYD,R3                                                        
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         LA    R8,HSTTAPE          R8=A(TAPE DTF)                               
         CLI   OVSWITCH,0          FIRST TINE                                   
         BE    ACCF                                                             
         CLI   OVSWITCH,1          PROCESS RECORD                               
         BE    ACCP                                                             
         CLI   OVSWITCH,X'FF'      LAST TIME                                    
         BE    ACCL                                                             
         B     EXIT                                                             
         EJECT                                                                  
*              FIRST TIME OPEN TAPE                                             
*                                                                               
ACCF     OPEN  (HSTTAPE,(OUTPUT))                                               
         MVI   OVSWITCH,1          SET TO PROCESS NEXT TIME                     
         B     EXIT                                                             
         SPACE 2                                                                
*              PROCESS TRANSACTIONS                                             
*                                                                               
ACCP     CLI   PROCREC,TRNSACTN    TRANSACTION                                  
         BNE   EXIT                                                             
         OC    ACDTPEEL,ACDTPEEL   IS IT PEELED                                 
         BZ    EXIT                                                             
         CLC   ACDTPEEL,PEELDATE                                                
         BH    EXIT                                                             
         SR    R1,R1                                                            
         ICM   R1,3,ACLENGTH       SAVE RECORD LENGTH                           
         STH   R1,SAVELEN                                                       
         LR    RF,R1                                                            
         LA    R0,IO                                                            
         LA    RE,ACKEYD                                                        
         MVCL  R0,RE               RECORD TO IO                                 
         LH    R1,SAVELEN                                                       
         LA    R1,28(R1)                                                        
         STCM  R1,3,IOLEN                                                       
         LA    R2,IOLEN                                                         
         PUT   (R8),(R2)                                                        
         MVI   WRITE,X'FF'                                                      
         AP    HSTRECS,=P'1'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*             LAST TIME CLOSE THE TAPE                                          
*                                                                               
ACCL     CLOSE (HSTTAPE)                                                        
         GOTO1 VPRINTER                                                         
         EDIT  HSTRECS,(7,P)                                                    
         MVC   P+8(24),=CL24'RECORDS MOVED TO HISTORY'                          
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XMOD1                                                                  
         EJECT                                                                  
HSTRECS  DC    PL4'0'                                                           
SAVELEN  DS    H                                                                
*              DCB HISTORY TAPE                                                 
*                                                                               
HSTTAPE  DCB   DDNAME=HSTTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=8200,BLKSIZE=8204                                 
*                                                                               
IOLEN    DC    F'0'                LENGTH                                       
IORCV    DC    X'6103',22X'00'     RCV HEADER (ACCOUNT - ADD)                   
IO       DS    2000C                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACACCWORKD                                                     
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACEXTHST  02/24/93'                                      
         END                                                                    
