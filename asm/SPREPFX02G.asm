*          DATA SET SPREPFX02G AT LEVEL 010 AS OF 09/10/97                      
*PHASE SPFX02G                                                                  
         TITLE 'SPFX02 - FIX BAD JWT GOAL RECORDS'                              
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
                                                                                
* REQFRST                                                                       
*                                                                               
FX10     DS    0H                                                               
         XC    BWEEKS,BWEEKS                                                    
         MVC   WORK(12),=C'971015980110'                                        
* GET BROADCAST WEEK DATES                                                      
         GOTO1 MOBILE,DMCB,(50,WORK),(4,BWEEKS)                                 
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         L     R6,ADCLT                                                         
         MVC   KEY+1(3),1(R6)      A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(4),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   FX100                                                            
*                                                                               
         CLC   KEY(5),KEYSAVE      TEST CHANGE OF PRODUCT                       
         BE    FX16                                                             
         MVC   KEYSAVE,KEY         SAVE CURRENT KEY                             
*                                                                               
FX16     BAS   RE,GETPRD           LOOK UP PRODUCT IN CLIENT HEADER             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         TM    15(R6),X'80'        TEST DELETED                                 
         BO    FX12                                                             
*                                                                               
         USING GOALRECD,R6                                                      
         CLI   GDCPPES,0           TEST CPP EST PRESENT                         
         BNE   FX12                YES - IGNORE                                 
         DROP  R6                                                               
* SEE IF CPP GUIDE NEEDED (GOALS OR DOLLARS MISSING FROM 21 ELEM)               
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
         MVI   FIXED,C'N'                                                       
         LA    R6,24(R6)                                                        
         USING GLEMENT,R6                                                       
*                                                                               
FX32     BAS   RE,NEXTEL                                                        
         BNE   FX12                                                             
*                                                                               
         OC    GLGRP,GLGRP         TEST ZERO POINTS                             
         BZ    FX40                GO CHECK FOR CPP GUIDE                       
         OC    GLBUDGET,GLBUDGET                                                
         BNZ   FX12                HAVE BOTH - SKIP                             
         DROP  R6                                                               
         SPACE 1                                                                
*==============================================================*                
* READ THE ESTIMATE HEADER TO GET CPP DATA                     *                
*==============================================================*                
         SPACE 1                                                                
FX40     MVC   SVGLKEY,KEY         SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVGLKEY+1  A-M/CLT                                      
         MVC   KEY+4(3),SVPRD      PRODUCT                                      
         MVC   KEY+7(1),SVGLKEY+7  EST                                          
*                                                                               
         L     R6,ADEST                                                         
         CLC   KEY(8),0(R6)        TEST HAVE IT ALREADY                         
         BNE   FX42                                                             
         MVC   KEY(13),SVGLKEY                                                  
         B     FX44                                                             
*                                                                               
FX42     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVGLKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MUST FIND PREVIOUS RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY              REREAD GOAL RECORD                           
*                                                                               
FX44     L     RF,ADEST                                                         
         USING ESTHDRD,RF                                                       
         L     R6,ADBUY                                                         
         USING GOALRECD,R6                                                      
         MVC   GDCPPCL(3),ECPPCLT                                               
         DROP  R6,RF                                                            
*                                                                               
         L     RE,BUYFIX                                                        
         LA    RE,1(RE)                                                         
         ST    RE,BUYFIX                                                        
         SRDL  RE,32                                                            
         D     RE,=F'50'           PRINT EVERY 50TH RECORD                      
         C     RE,=F'1'            TEST REMAINDER 1                             
         BNE   FX50                                                             
*                                                                               
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'GOLREC',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
FX50     GOTO1 PUTBUY                                                           
         BAS   RE,PRTBUY                                                        
         B     FX12                                                             
*                                                                               
FX100    MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
* GET EBCDIC PRODUCT CODE FROM GOAL RECORD                                      
         SPACE 1                                                                
GETPRD   L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         LA    R6,CLIST                                                         
         DROP  R6                                                               
GETPRD2  CLC   KEY+4(1),3(R6)      MATCH PRD                                    
         BE    GETPRD4                                                          
         LA    R6,4(R6)                                                         
         CLI   0(R6),C'A'                                                       
         BNL   GETPRD2                                                          
         DC    H'0'                                                             
GETPRD4  MVC   SVPRD(4),0(R6)                                                   
         BR    RE                                                               
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
         USING GOALRECD,R6                                                      
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),GAGYALPH                                                 
         GOTO1 CLUNPK,DMCB,GKEYCLT,PCLT                                         
*                                                                               
         MVC   PPRD,SVPRD                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,GKEYMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
*                                                                               
         ZIC   R0,GKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         MVC   PLIN,GKEYDPT        GET LINE NUMBER                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN+1(2),DUB                                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
SPOTCNT  DS    F                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SPTLEN   DS    X                                                                
PROD     DS    X                                                                
FIXED    DS    C                                                                
ELEM     DS    XL256                                                            
BWEEKS   DS    XL200                                                            
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
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENEST                                                       
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
**PAN#1  DC    CL21'010SPREPFX02G09/10/97'                                      
         END                                                                    
