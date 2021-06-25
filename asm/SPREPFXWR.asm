*          DATA SET SPREPFXWR  AT LEVEL 029 AS OF 07/21/94                      
*PHASE SPFX02W                                                                  
         TITLE 'SPFX02 - FIX WRLA STATION FILE'                                 
SPFX02   CSECT                                                                  
         DS    4096C                                                            
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
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
* REQFRST - READ FOR STATIONS IN LIST                                           
*                                                                               
FX10     DS    0H                                                               
         L     R4,=A(STALIST)                                                   
*                                                                               
FX12     XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'                                                       
         MVC   KEY+2(4),0(R4)                                                   
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),=C'WR'                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
         L     R6,ADSTAT                                                        
         CLC   KEY(9),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX14     CLC   9(3,R6),=C'000'     TEST CLIENT SPECIFIC STATION                 
         BE    FX20                                                             
         BAS   RE,CHKCLT                                                        
         BNE   FX18                                                             
         ZIC   RE,4(R4)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,4(R4)            BUMP COUNTER                                 
         OI    17(R6),X'80'        DELETE THE RECORD                            
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX16                                                             
         GOTO1 (RF),(R1),=C'DMWRT'                                              
* PRINT THE KEY OUT                                                             
FX16     MVC   P(15),0(R6)                                                      
         GOTO1 REPORT                                                           
*                                                                               
FX18     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ'                                          
         L     R6,ADSTAT                                                        
         CLC   KEY(9),0(R6)                                                     
         BE    FX14                                                             
*                                                                               
FX20     LA    R4,5(R4)            NEXT STATION                                 
         CLI   0(R4),X'FF'                                                      
         BNE   FX12                                                             
         EJECT                                                                  
* PRINT OUT STATION COUNTERS                                                    
         L     R4,=A(CLTLIST)                                                   
*                                                                               
FX102    MVC   P(3),0(R4)                                                       
         ZIC   R0,3(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(3),DUB                                                       
         GOTO1 REPORT                                                           
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FX102                                                            
*                                                                               
         L     R4,=A(STALIST)                                                   
*                                                                               
FX112    MVC   P(4),0(R4)                                                       
         ZIC   R0,4(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+5(3),DUB                                                       
         GOTO1 REPORT                                                           
         LA    R4,5(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   FX112                                                            
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
CHKCLT   NTR1                                                                   
         L     R5,=A(CLTLIST)                                                   
*                                                                               
CHKCLT2  CLC   0(3,R5),9(R6)                                                    
         BE    CHKCLT4             EXIT WITH CC EQ                              
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   CHKCLT2                                                          
         LTR   RB,RB               SET CC NEQ                                   
         B     CHKCLTX                                                          
*                                                                               
CHKCLT4  ZIC   RE,3(R5)            BUMP COUNTER                                 
         LA    RE,1(RE)                                                         
         STC   RE,3(R5)                                                         
         CR    RB,RB               SET CC EQU                                   
*                                                                               
CHKCLTX  XIT1                                                                   
*                                                                               
CLTLIST  DS    0D                                                               
         DC    C'AGV',X'00'                                                     
         DC    C'AMI',X'00'                                                     
         DC    C'APV',X'00'                                                     
         DC    C'AVG',X'00'                                                     
         DC    C'BLS',X'00'                                                     
         DC    C'BMM',X'00'                                                     
         DC    C'CIA',X'00'                                                     
         DC    C'DIV',X'00'                                                     
         DC    C'DVS',X'00'                                                     
         DC    C'EXR',X'00'                                                     
         DC    C'FUM',X'00'                                                     
         DC    C'GNM',X'00'                                                     
         DC    C'GON',X'00'                                                     
         DC    C'GSN',X'00'                                                     
         DC    C'GS2',X'00'                                                     
         DC    C'HSK',X'00'                                                     
         DC    C'INK',X'00'                                                     
         DC    C'ISG',X'00'                                                     
         DC    C'ITD',X'00'                                                     
         DC    C'LNI',X'00'                                                     
         DC    C'MCL',X'00'                                                     
         DC    C'MEY',X'00'                                                     
         DC    C'MMM',X'00'                                                     
         DC    C'MOY',X'00'                                                     
         DC    C'MSU',X'00'                                                     
         DC    C'MS1',X'00'                                                     
         DC    C'MTL',X'00'                                                     
         DC    C'NOU',X'00'                                                     
         DC    C'NSR',X'00'                                                     
         DC    C'PFP',X'00'                                                     
         DC    C'PGY',X'00'                                                     
         DC    C'PN2',X'00'                                                     
         DC    C'PTK',X'00'                                                     
         DC    C'QSP',X'00'                                                     
         DC    C'RGL',X'00'                                                     
         DC    C'SDE',X'00'                                                     
         DC    C'SII',X'00'                                                     
         DC    C'SMG',X'00'                                                     
         DC    C'SM1',X'00'                                                     
         DC    C'SPM',X'00'                                                     
         DC    C'TIG',X'00'                                                     
         DC    C'UDR',X'00'                                                     
         DC    C'USP',X'00'                                                     
         DC    C'WDL',X'00'                                                     
         DC    C'WSG',X'00'                                                     
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
STALIST  DC    C'ACCT',X'00'                                                    
         DC    C'ADC ',X'00'                                                    
         DC    C'AEN ',X'00'                                                    
         DC    C'AMIC',X'00'                                                    
         DC    C'BET ',X'00'                                                    
         DC    C'BOX ',X'00'                                                    
         DC    C'CAR ',X'00'                                                    
         DC    C'CBH ',X'00'                                                    
         DC    C'CMT ',X'00'                                                    
         DC    C'CNBC',X'00'                                                    
         DC    C'CNN ',X'00'                                                    
         DC    C'COMD',X'00'                                                    
         DC    C'CRT ',X'00'                                                    
         DC    C'DISC',X'00'                                                    
         DC    C'EAST',X'00'                                                    
         DC    C'ENT ',X'00'                                                    
         DC    C'ESN ',X'00'                                                    
         DC    C'ESP ',X'00'                                                    
         DC    C'ESPN',X'00'                                                    
         DC    C'FAM ',X'00'                                                    
         DC    C'FDN ',X'00'                                                    
         DC    C'HLN ',X'00'                                                    
         DC    C'HSE ',X'00'                                                    
         DC    C'HTS ',X'00'                                                    
         DC    C'INSP',X'00'                                                    
         DC    C'INTC',X'00'                                                    
         DC    C'LIFE',X'00'                                                    
         DC    C'MEN ',X'00'                                                    
         DC    C'MID ',X'00'                                                    
         DC    C'MSG ',X'00'                                                    
         DC    C'MTV ',X'00'                                                    
         DC    C'NAN ',X'00'                                                    
         DC    C'NECH',X'00'                                                    
         DC    C'NEWS',X'00'                                                    
         DC    C'NICK',X'00'                                                    
         DC    C'NOST',X'00'                                                    
         DC    C'PASN',X'00'                                                    
         DC    C'PREV',X'00'                                                    
         DC    C'PRIM',X'00'                                                    
         DC    C'PSN ',X'00'                                                    
         DC    C'PTC ',X'00'                                                    
         DC    C'SAT ',X'00'                                                    
         DC    C'SCC ',X'00'                                                    
         DC    C'SCP ',X'00'                                                    
         DC    C'SPCC',X'00'                                                    
         DC    C'SSN ',X'00'                                                    
         DC    C'SUN ',X'00'                                                    
         DC    C'TBS ',X'00'                                                    
         DC    C'TLC ',X'00'                                                    
         DC    C'TNN ',X'00'                                                    
         DC    C'TNT ',X'00'                                                    
         DC    C'TRAV',X'00'                                                    
         DC    C'TWC ',X'00'                                                    
         DC    C'USA ',X'00'                                                    
         DC    C'VHI ',X'00'                                                    
         DC    C'VISN',X'00'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
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
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLSTIME  DS    CL4                                                              
         DS    CL1                                                              
PLETIME  DS    CL4                                                              
         DS    CL1                                                              
PLDATE   DS    CL8                                                              
         DS    CL1                                                              
PLSTIMEN DS    CL4                                                              
         DS    CL1                                                              
PLETIMEN DS    CL4                                                              
         DS    CL1                                                              
PLDATEN  DS    CL8                                                              
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPREPFXWR 07/21/94'                                      
         END                                                                    
