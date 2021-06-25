*          DATA SET SPREPFXSM  AT LEVEL 026 AS OF 05/01/02                      
*          DATA SET SPREPFXCBY AT LEVEL 028 AS OF 07/15/97                      
*PHASE SPFX02S                                                                  
         TITLE 'SPFX02 - FIND BAD TRAFFIC COMMERCIALS/BJDE'                     
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
FX02     CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* CLTFRST                                                                       
FX10     DS    0H                                                               
         MVC   P(15),=C'STARTING CLIENT'                                        
         MVC   P+17(1),MED                                                      
         MVC   P+19(3),CLT                                                      
         GOTO1 REPORT                                                           
* BUILD A LIST OF ALL THE EXISTING CMML SEQ NUMBERS                             
         L     R0,ACMLTAB                                                       
         L     R1,=A(CMLTABX-CMLTAB)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AA1'                                                  
         L     R6,ADCLT                                                         
         MVC   KEY+2(3),1(R6)       MOVE A-M/CLT                                
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   FX100                                                            
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   FX20                                                             
*                                                                               
         CLI   KEY+5,0             MAKE SURE ONLY 2 DIGITS                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,KEY+6                                                       
         A     RE,ACMLTAB                                                       
         MVI   0(RE),C'Y'        MOVE LAST DIGIT                                
         B     FX12                                                             
         EJECT                                                                  
* NOW READ ALL THE BUYS                                                         
*                                                                               
FX20     MVI   DMINBTS,X'08'       PASS DELETES                                 
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)       MOVE A-M/CLT                                  
         MVI   KEY+3,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX30                                                             
*                                                                               
FX25     GOTO1 SEQ                                                              
*                                                                               
FX30     CLC   KEY(3),KEYSAVE      TEST SAME A/M/CLT                            
         BNE   FX100                                                            
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    FX25                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         TM    15(R6),X'80'        TEST DELETED                                 
         BO    FX25                                                             
         CLC   KEY+4(2),4(R6)      TEST SPILL                                   
         BNE   FX25                YES- IGNORE                                  
*                                                                               
         MVI   ELCDLO,X'18'                                                     
         MVI   ELCDHI,X'18'                                                     
         MVI   FIXED,C'N'                                                       
         LA    R6,24(R6)                                                        
*                                                                               
FX32     BAS   RE,NEXTEL                                                        
         BNE   FX40                                                             
*                                                                               
FX34     CLI   1(R6),8                                                          
         BNE   FX32                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,2(R6)          GET CML SEQNUM                               
         BZ    FX32                                                             
         A     RE,ACMLTAB                                                       
         CLI   0(RE),0                                                          
         BNE   FX32                                                             
         ST    R6,ELADDR           SAVE ELEMENT ADDRESS                         
*                                                                               
FX36     CLI   FIXED,C'Y'          TEST BEEN HERE BEFORE                        
         BE    FX38                                                             
*                                                                               
         BAS   RE,PRTBUY           PRINT EACH BUY ONCE ONLY                     
         MVI   FIXED,C'Y'          SET FLAG                                     
*                                                                               
FX38     GOTO1 RECUP,DMCB,ADBUY,(R6)   DELETE BAD ELEMENT                       
         BAS   RE,NEXTEL2                                                       
         BE    FX34                                                             
*                                                                               
FX40     CLI   FIXED,C'Y'          TEST RECORD CHANGED                          
         BNE   FX25                                                             
*                                                                               
         BC    0,FX42              THIS CODE ONE TIME ONLY                      
         OI    *-3,X'F0'                                                        
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
FX42     GOTO1 PUTBUY                                                           
         B     FX25                                                             
*                                                                               
FX100    MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,KEY+11           GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         L     R6,ELADDR                                                        
         GOTO1 HEXOUT,DMCB,2(R6),PCML,2,=C'TOG'                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
ACMLTAB  DC    A(CMLTAB)                                                        
ELADDR   DS    A                                                                
COUNT    DS    F                                                                
SPOTCNT  DS    F                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SPTLEN   DS    X                                                                
PROD     DS    X                                                                
FIXED    DS    C                                                                
ELEM     DS    XL256                                                            
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
BUYFIX   DC    F'0',CL20'BUYS FIXED'                                            
EBUYFIX  DC    F'0',CL20'EXPLODED BUYS FIXED'                                   
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
CMLTAB   DS    65536X                                                           
CMLTABX  EQU   *                                                                
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
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
PCML     DS    CL4                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPFXSM 05/01/02'                                      
         END                                                                    
