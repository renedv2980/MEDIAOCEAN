*          DATA SET SPREPFXACK AT LEVEL 001 AS OF 08/10/00                      
*PHASE SPFX02L                                                                  
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
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
*                                                                               
FX       LHI   RF,ESTTABX-ESTTAB                                                
         LA    RE,ESTTAB                                                        
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
**************************************************************                  
*                                                                               
FX10     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D72'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         GOTO1 HIGH                                                             
FX11     CLC   KEY(3),KEYSAVE                                                   
         BNE   FX100                                                            
         MVC   SVSLHKEY,KEY        SAVE HEADER KEY                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D73'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(3),SVSLHKEY+10     SEQ NUMBER                              
         GOTO1 HIGH                                                             
         B     FX14                                                             
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(6),KEYSAVE                                                   
         BE    FX15                                                             
         MVC   KEY,SVSLHKEY                                                     
         GOTO1 HIGH                RESTORE                                      
         GOTO1 SEQ                 AND NEXT                                     
         B     FX11                                                             
*                                                                               
FX15     SR    R4,R4                                                            
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
*        GOTO1 HEXOUT,DMCB,KEY,P,13,0                                           
*        GOTO1 REPORT                                                           
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
*                                                                               
* ELEMENTS LEFT - WRITE RECORD                                                  
*                                                                               
         GOTO1 PUTBUY                                                           
         AP    PUTCOUNT,=P'1'                                                   
*                                                                               
         CLC   SVPKEY(6),KEY                                                    
         BNE   FX33                                                             
         CLC   SVPKEY(9),KEY                                                    
         BE    FX34                                                             
         L     R2,SVPADR                                                        
         BAS   RE,GETPRO                                                        
         MVI   2(R2),C'*'          TO MEAN CHANGED                              
         EDIT  (B1,KEY+8),(3,4(R2)),FILL=0                                      
         LA    R2,8(R2)                                                         
         ST    R2,SVPADR                                                        
         MVC   SVPKEY,KEY                                                       
         B     FX34                                                             
*                                                                               
FX33     DS    0H                                                               
         GOTO1 REPORT              PRINT OLD LINE                               
         MVC   SVPKEY,KEY                                                       
         MVC   P(7),=C'DELETED'                                                 
         MVC   P+10(1),QMED                                                     
         GOTO1 CLUNPK,DMCB,SVSLHKEY+3,P+12                                      
         GOTO1 MSUNPK,DMCB,SVSLHKEY+5,P+16,P+21                                 
         LA    R2,P+29                                                          
         BAS   RE,GETPRO                                                        
         MVI   2(R2),C'*'          TO MEAN CHANGED                              
         EDIT  (B1,KEY+8),(3,4(R2)),FILL=0                                      
         LA    R2,8(R2)                                                         
         ST    R2,SVPADR                                                        
*                                                                               
FX34     DS    0H                                                               
         B     FX12                SKIP PRINT                                   
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
* DELETE THE RECORD                                                             
*                                                                               
FX40     AP    DELCOUNT,=P'1'                                                   
         OI    KEY+13,X'C0'        SET DIRECTORY DELETED                        
         GOTO1 WRITE                                                            
*                                                                               
         CLC   SVPKEY(6),KEY                                                    
         BNE   FX42                                                             
         CLC   SVPKEY(9),KEY                                                    
         BE    FX45                                                             
         L     R2,SVPADR                                                        
         BAS   RE,GETPRO                                                        
         EDIT  (B1,KEY+8),(3,4(R2)),FILL=0                                      
         LA    R2,8(R2)                                                         
         ST    R2,SVPADR                                                        
         MVC   SVPKEY,KEY                                                       
         B     FX45                                                             
*                                                                               
FX42     DS    0H                                                               
         GOTO1 REPORT              PRINT OLD LINE                               
         MVC   SVPKEY,KEY                                                       
         MVC   P(7),=C'DELETED'                                                 
         MVC   P+10(1),QMED                                                     
         GOTO1 CLUNPK,DMCB,SVSLHKEY+3,P+12                                      
         GOTO1 MSUNPK,DMCB,SVSLHKEY+5,P+16,P+21                                 
*                                                                               
         LA    R2,P+29                                                          
         BAS   RE,GETPRO                                                        
         EDIT  (B1,KEY+8),(3,4(R2)),FILL=0                                      
         LA    R2,8(R2)                                                         
         ST    R2,SVPADR                                                        
*                                                                               
FX45     CLI   MYFLAG,C'Y'         DID WE CHANGE RECORD ITSELF                  
         BNE   FX12                                                             
         L     R6,ADBUY                                                         
         OI    15(R6),X'C0'                                                     
         GOTO1 PUTBUY                                                           
         B     FX12                                                             
*                                                                               
GETPRO   NTR1                                                                   
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
GP10     CLC   3(1,RE),KEY+6                                                    
         BE    GP20                                                             
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BL    GPX                                                              
         B     GP10                                                             
GP20     MVC   0(3,R2),0(RE)                                                    
         MVI   3(R2),C'/'                                                       
GPX      XIT1                                                                   
*                                                                               
FX100    DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(8),=C'NOCHANGE'                                                
         EDIT  (P4,NOCHANGE),(8,P+10),FILL=0                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'CHANGED'                                                 
         EDIT  (P4,PUTCOUNT),(8,P+10),FILL=0                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'DELETED'                                                 
         EDIT  (P4,DELCOUNT),(8,P+10),FILL=0                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'NO EST '                                                 
         EDIT  (P4,NFECOUNT),(8,P+10),FILL=0                                    
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         XIT1                                                                   
         EJECT                                                                  
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
SVSLHKEY DS    XL13                                                             
SVPKEY   DS    XL13                                                             
SVPADR   DS    F                                                                
*                                                                               
PUTCOUNT DC    PL4'0'                                                           
NOCHANGE DC    PL4'0'                                                           
NFECOUNT DC    PL4'0'                                                           
DELCOUNT DC    PL4'0'                                                           
*                                                                               
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
**PAN#1  DC    CL21'001SPREPFXACK08/10/00'                                      
         END                                                                    
