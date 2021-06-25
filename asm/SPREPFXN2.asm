*          DATA SET SPREPFXN2  AT LEVEL 002 AS OF 08/29/97                      
*          DATA SET SPREPFXNE  AT LEVEL 027 AS OF 08/29/97                      
*PHASE SPFX02T                                                                  
         TITLE 'SPFX02 - FIX MISSED 0B ELEMS FROM POL CONV PROGRAM'             
*---------------------------------------------------------------*               
* PRINTN BUY RECORDS CHANGED AFTER CONVERSION DATE FOR NE       *               
*---------------------------------------------------------------*               
         SPACE 1                                                                
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
         MVI   DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETBUY                                                           
         MVI   DMINBTS,0                                                        
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         TM    15(R6),X'80'        TEST DELETED                                 
         BO    FX25                                                             
         CLC   KEY+4(2),4(R6)      TEST SPILL                                   
         BNE   FX25                YES- IGNORE                                  
*                                                                               
         USING BUYRECD,R6                                                       
         CLC   BDCHG,CNVDATE                                                    
         BL    FX25                                                             
         BAS   RE,PRTBUY                                                        
         B     FX25                                                             
*                                                                               
FX100    GOTO1 AENDREQ                                                          
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
         CLC   LASTKEY(13),KEY     TEST SAME AS BEFORE                          
         BE    EXIT                                                             
         MVC   LASTKEY(13),KEY                                                  
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
         CLC   BDCHG(3),CNVDATE                                                 
         BL    PRTB4                                                            
         GOTO1 DATCON,DMCB,(3,BDCHG),(11,PCML)                                  
*                                                                               
PRTB4    GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
LASTKEY  DS    XL13                                                             
CNVDATE  DC    AL1(97,08,22)                                                    
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
ELEM     DS    XL256                                                            
ELADDR   DS    A                                                                
COUNT    DS    F                                                                
SPOTCNT  DS    F                                                                
MAXCHARS DC    H'50'               MAX INPUT CHARS PER LINE                     
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SPTLEN   DS    X                                                                
PROD     DS    X                                                                
FIXED    DS    C                                                                
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
**PAN#1  DC    CL21'002SPREPFXN2 08/29/97'                                      
         END                                                                    
