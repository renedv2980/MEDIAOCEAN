*          DATA SET SPREPFXMEL AT LEVEL 046 AS OF 04/17/00                      
*PHASE SPFX02N                                                                  
         TITLE 'SPFX02 - DELETE COKEAT STATION LOCKIN RECORDS'                  
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       ZAP   CLTCOUNT,=P'0'                                                   
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)                                                     
         GOTO1 HIGH                                                             
         B     FX4                                                              
FX2      GOTO1 SEQ                                                              
*                                                                               
FX4      CLC   KEY(3),KEYSAVE      SAME A-M/FCLT                                
         BNE   ENDREQ                                                           
         CLI   KEY+3,X'FF'                                                      
         BE    ENDREQ                                                           
         CLI   KEY+10,X'FF'                                                     
         BE    FX2                                                              
         CLI   KEY+10,X'80'        TEST SPILL POINTER                           
         BE    FX2                                                              
         AP    CLTCOUNT,=P'1'                                                   
         BAS   RE,PRTBUY           NON-POL BUY IS BAD NEWS                      
         B     FX2                                                              
*                                                                               
ENDREQ   MVC   P(1),QMED                                                        
         MVC   P+2(3),QCLT                                                      
*                                                                               
         OI    CLTCOUNT+3,X'0F'                                                 
         UNPK  P+7(8),CLTCOUNT                                                  
         MVC   P+17(12),=C'NON-POL BUYS'                                        
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
*                                                                               
CLTCOUNT DC    PL4'0'                                                           
*&&DO                                                                           
         LHI   RF,ESTTABX-ESTTAB                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
* READ ALL POL ESTHDRS FOR CLIENT CC                                            
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(4),0(R6)                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         B     FX4                                                              
*                                                                               
FX2      GOTO1 SEQ                                                              
*                                                                               
FX4      CLC   KEY(7),KEYSAVE                                                   
         BNE   FX10                                                             
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   FX2                                                              
*                                                                               
         GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,ESTART,(2,DUB)                                       
         GOTO1 (RF),(R1),EEND,(2,DUB+2)                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,KEY+7            GET EST NUMBER                               
         BCTR  RE,0                                                             
         SLL   RE,2                X 4                                          
         LA    RE,ESTTAB(RE)                                                    
         OC    0(4,RE),0(RE)                                                    
         BZ    *+6                                                              
         DCHO                                                                   
         MVC   0(4,RE),DUB                                                      
         B     FX2                                                              
*                                                                               
FX10     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D73'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(3),KEYSAVE                                                   
         BNE   FX100                                                            
*                                                                               
         SR    R4,R4                                                            
         IC    R4,KEY+8            GET EST NUMBER                               
         BCTR  R4,0                                                             
         SLL   R4,2                                                             
         LA    R4,ESTTAB(R4)                                                    
         OC    0(4,R4),0(R4)       TEST EST ON FILE                             
         BNZ   FX16                                                             
         AP    NFECOUNT,=P'1'                                                   
         B     FX40                NO - DELETE RECORD                           
*                                                                               
FX16     GOTO1 GETBUY              GET THE RECORD                               
*                                                                               
         L     R6,ADBUY                                                         
         USING SLKRECD,R6                                                       
*                                                                               
         MVI   MYFLAG,0                                                         
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,SLKFSTEL                                                      
*                                                                               
FX20     BAS   RE,NEXTEL                                                        
         BNE   FX30                                                             
*                                                                               
FX22     CLC   2(2,R6),0(R4)       TEST IN EST PERIOD                           
         BL    FX24                NO                                           
         CLC   2(2,R6),2(R4)                                                    
         BNH   FX20                                                             
*                                                                               
FX24     MVI   MYFLAG,C'Y'         SET RECORD UPDATE                            
         GOTO1 RECUP,DMCB,ADBUY,(R6)   DELETE THE FIRST ONE                     
         BAS   RE,NEXTEL2                                                       
         BE    FX22                                                             
         EJECT                                                                  
*SEE IF ANY 03 ELEMENTS LEFT IN RECORD                                          
*                                                                               
FX30     CLI   MYFLAG,C'Y'         DID WE CHANGE RECORD                         
         BE    FX32                NO                                           
         AP    NOCHANGE,=P'1'                                                   
         B     FX12                                                             
*                                                                               
FX32     L     R6,ADBUY                                                         
         USING SLKRECD,R6                                                       
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,SLKFSTEL                                                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FX40                GO DELETE RECORD                             
* ELEMENTS LEFT - WRITE RECORD                                                  
         GOTO1 PUTBUY                                                           
*                                                                               
         AP    PUTCOUNT,=P'1'                                                   
         CP    PUTCOUNT,=P'10'                                                  
         BH    FX35                                                             
         SR    R0,R0                                                            
         L     R6,ADBUY                                                         
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'PUTTED',(R6),C'DUMP',(R0),=C'1D00'                
         GOTO1 REPORT                                                           
*                                                                               
FX35     B     FX12                                                             
*                                                                               
PUTCOUNT DC    PL4'0'                                                           
NOCHANGE DC    PL4'0'                                                           
NFECOUNT DC    PL4'0'                                                           
*                                                                               
* DELETE THE RECORD                                                             
*                                                                               
FX40     AP    DELCOUNT,=P'1'                                                   
         OI    KEY+13,X'80'        SET DIRECTORY DELETED                        
         GOTO1 WRITE                                                            
*                                                                               
         L     R6,ADBUY                                                         
         OI    15(R6),X'80'                                                     
         GOTO1 PUTBUY                                                           
*                                                                               
         MVC   P(7),=C'DELETED'                                                 
         GOTO1 HEXOUT,DMCB,(R6),P+10,13,=C'TOG'                                 
         GOTO1 REPORT                                                           
         B     FX12                                                             
DELCOUNT DC    PL4'0'                                                           
*&&                                                                             
PRINTIT  NTR1                                                                   
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
FX100    DS    0H                                                               
         MVC   P(8),=C'NOCHANGE'                                                
         OI    NOCHANGE+3,X'0F'                                                 
         UNPK  P+10(7),NOCHANGE                                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(6),=C'PUTTED'                                                  
         OI    PUTCOUNT+3,X'0F'                                                 
         UNPK  P+8(10),PUTCOUNT                                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'DELETED'                                                 
         OI    DELCOUNT+3,X'0F'                                                 
         UNPK  P+10(10),DELCOUNT                                                
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'NO EST '                                                 
         OI    NFECOUNT+3,X'0F'                                                 
         UNPK  P+10(10),NFECOUNT                                                
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
PRTBUY   NTR1    <<<NOTE THIS IS A NONSTANDARD PRTBUY>>>                        
         CLC   LASTKEY(9),KEY      SAME A-M/CLT/PRD/MKT/STA/EST                 
         BE    EXIT                                                             
         MVC   LASTKEY,KEY                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING BUYRECD,R6                                                       
*                                                                               
         GOTO1 HEXOUT,DMCB,(R6),P,13,=C'TOG'                                    
*                                                                               
PRTB2    LA    R5,P+30                                                          
         USING PLINED,R5                                                        
*                                                                               
         MVC   PMED,QMED                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
PRTB4    CLC   3(1,RE),BUYKPRD                                                  
         BE    PRTB6                                                            
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   PRTB4                                                            
         LA    RE,=C'***'                                                       
PRTB6    MVC   PPRD,0(RE)                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
                                                                                
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,KEY+12           GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
LASTKEY  DC    XL13'00'                                                         
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
MYFLAG   DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    0D                                                               
         DS    256XL4                                                           
ESTTABX  EQU   *                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
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
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSLK                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SPREPFXMEL04/17/00'                                      
         END                                                                    
