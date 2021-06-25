*          DATA SET SPREPFXANB AT LEVEL 043 AS OF 11/06/00                      
*PHASE SPFX02B                                                                  
         TITLE 'SPFX02 - COUNT MATCHED AND TOTAL INVOICES'                      
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
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
DMXIT    XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         ZAP   COUNT,=P'0'                                                      
         ZAP   MATCHED,=P'0'                                                    
         ZAP   PAIDINV,=P'0'                                                    
         MVC   BYTE,BAGYMD                                                      
         NI    BYTE,X'F0'          JUST AGY                                     
*                                                                               
SNV      XC    INVKEY,INVKEY                                                    
         MVC   INVKEY(2),=X'0E03'                                               
         MVC   INVKEY+2(1),BYTE    JUST AGY                                     
         MVC   KEYSAVE,INVKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',INVKEY,INVKEY,0               
         B     SNV05                                                            
SNVSEQ   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',INVKEY,INVKEY,0               
SNV05    CLC   INVKEY(2),KEYSAVE                                                
         BH    SNVX                                                             
         MVC   BYTE2,INVKEY+2                                                   
         NI    BYTE2,X'F0'         JUST AGY                                     
         CLC   BYTE,BYTE2                                                       
         BNE   SNVX                                                             
*                                                                               
****     CLC   =X'36DE',INVKEY+8     SEP/00 INVOICE                             
****     CLC   =X'36FE',INVKEY+8     AUG/00 INVOICE                             
         CLC   =X'371E',INVKEY+8     JUL/00 INVOICE                             
         BNE   SNVSEQ                                                           
*                                                                               
         AP    COUNT,=P'1'                                                      
         TM    INVKEY+33,X'80'     MATCHED                                      
         BZ    *+10                                                             
         AP    MATCHED,=P'1'                                                    
*                                                                               
         LA    R3,INVKEY                                                        
         USING SNVKEY,R3                                                        
         MVC   INVDA,SNVDDA                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',INVDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SNVSEQ                                                           
         USING SNVHDELD,R6                                                      
         TM    SNVHDCTL,SNVHDPDQ   PAID                                         
         BNO   *+10                                                             
         AP    PAIDINV,=P'1'                                                    
         B     SNVSEQ                                                           
*                                                                               
SNVX     MVC   P(18),=C'NUMBER OF INVOICES'                                     
         EDIT  COUNT,(12,P+20),COMMAS=YES                                       
         GOTO1 REPORT                                                           
         MVC   P(14),=C'NUMBER MATCHED'                                         
         EDIT  MATCHED,(12,P+20),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P(11),=C'NUMBER PAID'                                            
         EDIT  PAIDINV,(12,P+20),COMMAS=YES                                     
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         DROP  R6                                                               
*                                                                               
*==================================================================             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REQL     MVC   P(18),=C'NUMBER OF INVOICES'                                     
         EDIT  COUNT,(12,P+20),COMMAS=YES,ALIGN=LEFT                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,42,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
NETNUM   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
COUNT    DS    PL16                                                             
MATCHED  DS    PL8                                                              
PAIDINV  DS    PL8                                                              
TEMP     DS    CL80                                                             
BYTE2    DS    X                                                                
INVDA    DS    XL4                                                              
INVKEY   DS    CL50                                                             
INVKEYSV DS    CL50                                                             
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
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
**PAN#1  DC    CL21'043SPREPFXANB11/06/00'                                      
         END                                                                    
