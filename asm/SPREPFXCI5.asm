*          DATA SET SPREPFXCI5 AT LEVEL 008 AS OF 06/05/96                      
*PHASE SPFX02V                                                                  
         TITLE 'SPFX02 - COKE BUYS'                                             
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
         CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* PROCBUY                                                                       
FX10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING BUYRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   BUYKAM,X'B0'                                                     
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 HIGH                                                             
                                                                                
FX20     LA    R6,KEY                                                           
         CLC   SVKEY,BUYKAM        AM,CLT,PRD,MKSTA,EST                         
         BE    FX60                                                             
                                                                                
FX25     TM    FLAG,AUTOI5+NONI5                                                
         BNO   FX50                                                             
         MVC   SAVEKEY,KEY                                                      
         LA    R3,LNTABLE                                                       
FX30     CLI   0(R3),0                                                          
         BE    FX40                                                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   KEY(10),SVKEY                                                    
         MVI   BUYKPRD,X'FF'       POL POINTER                                  
         MVC   KEY+11(1),0(R3)                                                  
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    KEY+13,X'80'                                                     
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         MVI   DMOUTBTS,X'FD'                 READ FOR DELETES                  
         GOTO1 GET                                                              
         OI    15(R6),X'80'                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX35                                                             
         MVC   SAVEKEY2,KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,AREC                      
FX35     GOTO1 HEXOUT,DMCB,KEY,P,14                                             
         GOTO1 HEXOUT,DMCB,(R6),P+35,16                                         
         GOTO1 REPORT                                                           
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         LA    R3,1(R3)                                                         
         B     FX30                                                             
                                                                                
FX40     XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 HIGH                                                             
         CLC   SAVEKEY,KEY                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
FX50     MVI   FLAG,0              RESET FLAG                                   
         XC    LNTABLE,LNTABLE                                                  
         LA    R5,LNTABLE                                                       
                                                                                
         LA    R6,KEY                                                           
         CLI   BUYKAM,X'B4'                                                     
         BH    FX100                                                            
         CLC   BUYKCLT,=X'885F'    CLIENT CC                                    
         BE    FX90                                                             
         CLI   BUYKPRD,X'FF'                                                    
         BE    FX85                                                             
         MVC   SVKEY,BUYKAM                                                     
                                                                                
FX60     L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
                                                                                
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         MVI   DMOUTBTS,X'FD'                 READ FOR DELETES                  
         GOTO1 GET                                                              
                                                                                
         CLC   =C'AUTO - I5',BDPROGRM                                           
         BNE   *+12                                                             
         OI    FLAG,AUTOI5                                                      
         B     FX70                                                             
                                                                                
         CLC   =C'ADJ',BDPROGRM    IGNORE  ADJUSTMENTS                          
         BE    FX70                                                             
                                                                                
         TM    BDCIND2,X'04'       IGNORE NEGATIVE COST                         
         BNO   FX65                                                             
         TM    BDCIND2,X'08'                                                    
         BO    FX70                                                             
         B     *+12                                                             
FX65     TM    BDCIND,X'01'                                                     
         BNZ   FX70                                                             
                                                                                
         CLC   BDSTART,=X'60031F'  COMPARE W/ MAR31/96                          
         BNH   FX70                DON'T CARE ABOUT BEFORE MARCH 31             
         CLC   BDSTART,=X'60041C'  COMPARE W/ APR28/96                          
         BNL   FX70                                                             
         OI    FLAG,NONI5                                                       
         MVC   0(1,R5),10(R6)      LINE NUMBER                                  
         LA    R5,1(R5)                                                         
                                                                                
FX70     TM    FLAG,AUTOI5+NONI5                                                
         BNO   FX80                                                             
                                                                                
***      GOTO1 HEXOUT,DMCB,KEY,P,13                                             
***      GOTO1 REPORT                                                           
FX80     OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 SEQ                                                              
         B     FX20                                                             
                                                                                
FX85     LA    R6,KEY              GET NEXT PRODUCT                             
         MVC   BUYMSTA(9),=X'FFFFFFFFFFFFFFFFFF'                                
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 HIGH                                                             
         B     FX20                                                             
                                                                                
FX90     LA    R6,KEY              GET NEXT CLIENT                              
         MVC   BUYKPRD(10),=X'FFFFFFFFFFFFFFFFFFFF'                             
         OI    DMINBTS,X'08'                  READ FOR DELETES                  
         GOTO1 HIGH                                                             
         B     FX20                                                             
                                                                                
FX100    MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
LNSTRT   DS    F                                                                
ELCODE   DS    X                                                                
FLAG     DS    X                                                                
NONI5    EQU   X'80'                                                            
AUTOI5   EQU   X'40'                                                            
SAVEKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
LNTABLE  DS    XL256                                                            
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
BUYFIX   DC    F'0',CL20'BUYS FIXED'                                            
EBUYFIX  DC    F'0',CL20'EXPLODED BUYS FIXED'                                   
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
SPILLELS DS    1024C                                                            
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
SDEFRECD DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENSDEF                                                      
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
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPFXCI506/05/96'                                      
         END                                                                    
