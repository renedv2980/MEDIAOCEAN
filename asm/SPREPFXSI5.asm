*          DATA SET SPREPFXSI5 AT LEVEL 044 AS OF 06/25/96                      
*PHASE SPFX02I                                                                  
         TITLE 'SPFX02 - FIND SI5 BUYS FOR COKEAT'                              
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
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
REQF     DS    0H                                                               
         LA    R0,TABLEMAX                                                      
         GOTO1 ,BINPARMS,,AFFTAB,0,L'AFFENTRY,(0,L'AFFENTRY),(R0)               
         B     EXIT                                                             
*                                                                               
* PROCBUY                                                                       
FX10     DS    0H                                                               
         LA    R5,AFFENTRY                                                      
         USING AFFD,R5                                                          
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   =C'SI5',BDPROGRM                                                 
         BNE   EXIT                                                             
         CLI   BUYKBUY,0                                                        
         BE    EXIT                                                             
         CLC   KEY(10),SAVEKEY    A-M/CLT/PRD/MKT/STA/EST                       
         BE    FX60                                                             
*                                                                               
         L     R3,BINPARMS+8                                                    
         LTR   R3,R3                                                            
         BZ    FX60                                                             
         LA    R4,AFFTAB                                                        
         XC    SVENTRY,SVENTRY                                                  
         NI    FLAG,X'FF'-DUPFOUND                                              
*                                                                               
FX20     CLC   SVENTRY(4),0(R4)                                                 
         BNE   FX40                                                             
*                                                                               
         TM    FLAG,DUPFOUND                                                    
         BO    FX30                                                             
         BAS   RE,PRTBUY                                                        
*                                                                               
FX30     DS    0H                                                               
         GOTO1 HEXOUT,DMCB,SVENTRY,P,5,=C'TOG'                                  
         EDIT  (B1,SVENTRY+4),(3,P+15)                                          
         GOTO1 HEXOUT,DMCB,(R4),P+20,5,=C'TOG'                                  
         EDIT  (B1,4(R4)),(3,P+35)                                              
         GOTO1 REPORT                                                           
         OI    FLAG,DUPFOUND                                                    
*                                                                               
FX40     MVC   SVENTRY,0(R4)                                                    
         LA    R4,AFFTABLQ(R4)                                                  
         BCT   R3,FX20                                                          
*                                                                               
         XC    BINPARMS+8(4),BINPARMS+8                                         
FX60     MVC   SAVEKEY,KEY                                                      
         LA    R2,BDELEM                                                        
         USING BDELEM,R2                                                        
         DROP  R2                                                               
*                                                                               
         USING AFFELEM,R2                                                       
         NI    FLAG,X'FF'-AFFOUND                                               
*                                                                               
FX80     CLI   0(R2),0                                                          
         BE    FX100                                                            
FX90     ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),X'10'                                                      
         BNE   FX80                                                             
*                                                                               
         MVC   AFDATE,ADATE                                                     
         MVC   AFTIME,ATIME                                                     
         MVC   AFLINE,BUYKBUY                                                   
         OI    FLAG,AFFOUND                                                     
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(1,AFFENTRY)                                    
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     FX90                                                             
*                                                                               
FX100    DS    0H                                                               
         TM    FLAG,AFFOUND                                                     
         BO    EXIT                                                             
*                                                                               
         MVC   P(5),=C'NO AF'                                                   
         EDIT  (B1,BUYKBUY),(3,P+40)                                            
         BAS   RE,PRTBUY                                                        
         B     EXIT                                                             
         DROP  R2,R5                                                            
*                                                                               
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         LA    R6,SAVEKEY                                                       
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
PRTB2    CLC   3(1,RE),BUYKPRD                                                  
         BE    PRTB4                                                            
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BH    PRTB2                                                            
         DC    H'0'                                                             
PRTB4    DS    0H                                                               
         MVC   PPRD,0(RE)                                                       
*                                                                               
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
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
         BC    0,EXIT                                                           
         OI    *-3,X'F0'                                                        
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
NUMENTRY DS    F                                                                
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
ELCODE   DS    X                                                                
SPTLEN   DS    X                                                                
PROD     DS    X                                                                
ELEM     DS    XL256                                                            
AFFENTRY DS    XL5                                                              
FLAG     DS    X                                                                
AFFOUND  EQU   X'80'                                                            
DUPFOUND EQU   X'40'                                                            
SAVEKEY  DS    XL10                                                             
SVENTRY  DS    XL5                                                              
*                                                                               
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
AFFTAB   DS    1000XL5                                                          
TABLEMAX EQU   (*-AFFTAB)/5        MAX ENTRIES                                  
AFFD     DSECT                                                                  
AFDATE   DS    XL2                                                              
AFTIME   DS    XL2                                                              
AFLINE   DS    X                                                                
AFFTABLQ EQU   *-AFFD                                                           
*                                                                               
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
PPRD     DS    CL3                                                              
         DS    CL2                                                              
*                                                                               
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
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
**PAN#1  DC    CL21'044SPREPFXSI506/25/96'                                      
         END                                                                    
