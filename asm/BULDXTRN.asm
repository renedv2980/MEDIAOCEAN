*          DATA SET BULDXTRN   AT LEVEL 010 AS OF 12/29/86                      
*PHASE BULDXTRN,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'LDXTRN - BUDGET SYSTEM - GENERAL EXTERNAL'                      
*        PARAMS VIA R1                                                          
*        AL3   A(RECORD) ETC.                                                   
*                                                                               
LDXTRN   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,BULDXTRN,RR=R5                                       
         USING WORKD,RC                                                         
         ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO             SAVE RELOCATION FACTOR                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    LDX                 INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    LDR                 PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    LDF                 END-OF-FILE                                  
         B     LDX                                                              
         SPACE 2                                                                
* PROCESS RECORD                                                                
*                                                                               
LDR      L     R2,AREC             R2=A(RECORD)                                 
         USING BURECD,R2                                                        
*                                                                               
         CLI   BUKSYS,C'B'         TEST BUDGET SYSTEM                           
         BNE   LDRX                                                             
         CLI   BUKRTYP,BUKRTYPQ    TEST BUDGET RECORD                           
         BNE   LDRX                                                             
         TM    BURCTYP,BUKCOUT     TEST OUTLINE RECORD                          
         BZ    LDRX                                                             
         LA    R6,BUFRSTEL                                                      
         USING BUPTRD,R6                                                        
LDR2     CLI   BUPTREL,0                                                        
         BE    LDRX                                                             
         CLI   BUPTREL,BUPTRELQ                                                 
         BNE   LDR4                                                             
         LA    R3,BUPOINT                                                       
         USING BUCKEY,R3                                                        
         CLI   BUCPLAN+2,C' '      TEST THIRD BYTE OF PLAN IS SPACE             
         BNE   LDR4                NO                                           
         SR    R0,R0                                                            
         ICM   R0,3,BURLEN                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,=C'BEFORE'),(R2),C'DUMP',(R0),=C'1D',X        
               RR=RELO                                                          
         MVI   BUCPLAN+2,0         SET THIRD BYTE TO ZERO                       
         AP    ELFIXES,=P'1'                                                    
         GOTO1 =V(PRNTBL),DMCB,(5,=C'AFTER'),(R2),C'DUMP',(R0),=C'1D'  X        
               RR=RELO                                                          
*                                                                               
LDR4     ZIC   R0,BUPTRLEN                                                      
         AR    R6,R0                                                            
         B     LDR2                                                             
*                                                                               
LDRX     L     R1,APARM                                                         
         MVI   0(R1),0             RETURN OK TO LOAD                            
         B     LDX                                                              
*                                                                               
* END-OF-FILE CODE                                                              
*                                                                               
LDF      GOTO1 VPRINTER                                                         
         MVC   P+10(13),=C'ELEMENT FIXES'                                       
         MVI   P+23,C'='                                                        
         ZAP   DUB,ELFIXES                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  P+24(7),DUB                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
LDX      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* CONSTANTS                                                                     
*                                                                               
ELFIXES  DC    PL4'0'                                                           
         SPACE 1                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         SPACE 2                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* BUGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE BUGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010BULDXTRN  12/29/86'                                      
         END                                                                    
