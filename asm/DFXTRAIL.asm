*          DATA SET DFXTRAIL   AT LEVEL 001 AS OF 05/15/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DFXTRALA                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS DESIGNED TO MODIFY        *         
* EXTRACT OUTPUT DATASETS WHICH HAVE BEEN POST-PROCESSED BY DFSORT.   *         
*                                                                     *         
* THIS EXIT UPDATES A DXTRACT TRAILER RECORD WITH AN ACCURATE RECORD  *         
* COUNT AND BYTE COUNT. NO OTHER RECORDS ARE MODIFIED.                *         
*                                                                     *         
***********************************************************************         
DFXTRAIL CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
*                                                                               
         L     R3,0(R1)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         AP    RECORD_COUNT,=P'1'  INCREMENT RECORD COUNT                       
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)          L'RECORD FROM RDW                            
         CVD   RF,DUB                                                           
         AP    BYTE_COUNT,DUB      KEEP RUNNING TOTAL BYTE COUNT                
*                                                                               
         USING DXTRLD,R3                                                        
         CLC   DXTRLTYP,DXTRLDQ    DXTRACT TRAILER RECORD?                      
         BNE   KEEP                NO: JUST RETURN THE RECORD AS IS             
*                                                                               
* UPDATE THE TRAILER RECORD COUNTS                                              
*                                                                               
         EDIT  RECORD_COUNT,DXTRLNR,ZERO=NOBLANK,FILL=0                         
         EDIT  BYTE_COUNT,DXTRLNB,ZERO=NOBLANK,FILL=0                           
*                                                                               
KEEP     DS    0H                                                               
         SGR   GR1,GR1                                                          
         LR    R1,R3               A(OUTPUT RECORD)                             
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         B     GOBACK                                                           
         DROP  R3                                                               
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
         ORG   DFXTRAIL+(((*-DFXTRAIL)/256)+1)*256 FOR I-CACHE PIPELINE         
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
*                                                                               
DUB          DS D                                                               
WORK         DS CL17                                                            
RECORD_COUNT DC PL8'0'             EXTRACT FILE NUMBER OF RECORDS               
BYTE_COUNT   DC PL8'0'             EXTRACT FILE NUMBER OF BYTES                 
         EJECT                                                                  
       ++INCLUDE DXRECID                                                        
         SPACE 3                                                                
       ++INCLUDE DXTRLD                                                         
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DFXTRAIL  05/15/14'                                      
         END                                                                    
