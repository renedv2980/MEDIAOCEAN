*          DATA SET SPREPFXANZ AT LEVEL 009 AS OF 01/09/01                      
*PHASE SPFX02Z                                                                  
         TITLE 'SPFX02 - SET MASTER PRODUCT WHEN MISSING'                       
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
         CLI   MODE,PROCBUY                                                     
         BE    PBUY                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
         B     EXIT                                                             
                                                                                
* PROCBUY                                                                       
PBUY     DS    0H                                                               
         USING BUYRECD,R3                                                       
         L     R3,ADBUY                                                         
*                                                                               
         OC    BDMASPRD,BDMASPRD    ALREADY HAS MASTER PRD                      
         BNZ   EXIT                                                             
*                                                                               
         LA    R6,BDELEM                                                        
PBUY10   CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'0B'                                                      
         BE    PBUY20                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PBUY10                                                           
PBUY20   CLI   1(R6),X'0E'         ALLOCATED                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDMASPRD(1),10(R6)  MOVE PRD TO MASPRD                           
         GOTO1 PUTBUY                                                           
*                                                                               
         L     RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
*                                                                               
*        C     RF,=F'10'                                                        
*        BH    *+8                                                              
         BAS   RE,PRTBUY                                                        
*                                                                               
PBX      B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
*        SR    R0,R0                                                            
*        ICM   R0,3,13(R5)                                                      
*        GOTO1 PRNTBL,DMCB,=C'BUYREC',(R5),C'DUMP',(R0),=C'1D00'                
*                                                                               
         L     R5,ADBUY                                                         
         USING BUYRECD,R5                                                       
PRTB2    LA    R4,P                                                             
         USING PLINED,R4                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*        MVC   PPRD,=C'POL'                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
*        CLI   QOPT5,C'Y'          PRINT OUT KEY FOR DEBUGGING                  
*        BNE   PRTB3                                                            
         GOTO1 HEXOUT,DMCB,BDMASPRD,PIND2,1                                     
         GOTO1 HEXOUT,DMCB,KEY,PKEY,18                                          
PRTB3    GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SAVEKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PIND2    DS    CL1                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPFXANZ01/09/01'                                      
         END                                                                    
