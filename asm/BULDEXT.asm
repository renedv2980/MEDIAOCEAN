*          DATA SET BULDEXT    AT LEVEL 012 AS OF 07/14/88                      
*PHASE BULDEXT,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE MSUNPK                                                                 
*INCLUDE MSUNPKA                                                                
*INCLUDE MSPACKA                                                                
         TITLE 'LDXTRN - BUDGET SYSTEM - GENERAL EXTERNAL'                      
*        PARAMS VIA R1                                                          
*        AL3   A(RECORD) ETC.                                                   
*                                                                               
LDXTRN   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,BULDEXT*,RR=R5                                       
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
*                                                                               
         USING BURULD,R6                                                        
LDR2     CLI   BURULEL,0                                                        
         BE    LDRX                                                             
         CLI   BURULEL,BURULELQ                                                 
         BNE   LDR4                                                             
         CLI   BURULTYP,RUSTA                                                   
         BNE   LDR4                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BURLEN                                                      
         GOTO1 =V(PRNTBL),DMCB,(6,=C'BEFORE'),(R2),C'DUMP',(R0),=C'1D',X        
               RR=RELO                                                          
*                                                                               
         MVC   MSTA(2),=X'0001'                                                 
         MVC   MSTA+2(3),BURULE                                                 
         GOTO1 MSUNPK,DMCB,MSTA,MKT,STA                                         
         ZIC   R3,MSTA+4                                                        
         SLL   R3,28                                                            
         SRL   R3,28                                                            
         CH    R3,=H'5'                                                         
         BNE   *+10                                                             
         AP    ERRCNT,=P'1'                                                     
         CLI   STA+4,C' '                                                       
         BNE   *+8                                                              
         MVI   STA+4,C'T'                                                       
         GOTO1 MSPACKA,(R1),MKT,STA,MSTA2                                       
         CLI   8(R1),X'FF'                                                      
         BNE   *+10                                                             
         AP    ERRCNT,=P'1'                                                     
         ZIC   RE,MSTA2+4                                                       
         SRL   RE,4                                                             
         SLL   RE,4                                                             
         OR    RE,R3                                                            
         STC   RE,MSTA2+4                                                       
         MVC   BURULE(3),MSTA2+2                                                
         AP    CONVCNT,=P'1'                                                    
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(5,=C'AFTER'),(R2),C'DUMP',(R0),=C'1D', X        
               RR=RELO                                                          
*                                                                               
         GOTO1 MSUNPKA,DMCB,MSTA2,MKT,STA2                                      
         MVC   P(4),=C'OLD='                                                    
         MVC   P+4(5),STA                                                       
         MVC   P+10(4),=C'NEW='                                                 
         MVC   P+14(5),STA2                                                     
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
LDR4     ZIC   R0,BURULEN                                                       
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
         MVC   P+10(13),=C'STATION FIXES'                                       
         MVI   P+23,C'='                                                        
         ZAP   DUB,CONVCNT                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  P+24(7),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P+10(11),=C'ERROR COUNT'                                         
         MVC   P+23,C'='                                                        
         ZAP   DUB,ERRCNT                                                       
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
MSUNPK   DC    V(MSUNPK)                                                        
MSUNPKA  DC    V(MSUNPKA)                                                       
MSPACKA  DC    V(MSPACKA)                                                       
*                                                                               
ERRCNT   DC    PL8'0'                                                           
CONVCNT  DC    PL8'0'                                                           
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
*                                                                               
MSTA     DS    XL5                                                              
MSTA2    DS    XL5                                                              
MKT      DS    CL4                                                              
STA      DS    CL5                                                              
STA2     DS    CL5                                                              
*                                                                               
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
         SPACE 2                                                                
* BUGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE BUGENEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012BULDEXT   07/14/88'                                      
         END                                                                    
