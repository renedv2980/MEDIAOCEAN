*          DATA SET SPREP4502  AT LEVEL 042 AS OF 05/01/02                      
*PHASE SP4502A,+0                                                               
         TITLE 'SP045-02 SPOT STATION ACTIVITY'                                 
SP045    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP045                                                          
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE                                                                  
         CLI   MODE,PROCREC                                                     
         BNE   EXIT                                                             
         BAS   RE,PRCREC                                                        
         GOTO1 AENDREQ                                                          
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 3                                                                
PRCREC   NTR1                                                                   
         SPACE                                                                  
         CLI   QAREA+10,C'R'       REP REC                                      
         BE    PROCREP                                                          
         CLI   QAREA+10,C'S'       STATION REC                                  
         BE    PROCSTA                                                          
         CLI   QAREA+10,C'A'       ADDRESS REC                                  
         BE    PROCADRS                                                         
         CLI   QAREA+10,C'M'       MARKET REC                                   
         BE    PROCMKT                                                          
         SPACE                                                                  
PREC10   MVC   P+17(1),QAREA+10    REC TYPE                                     
         MVC   P+18(1),QMED        MEDIA                                        
         MVC   P+19(7),QSTA        STA CALL/REP CODE + AGY                      
         MVC   P+28(22),=C'** RECORD NOT FOUND **'                              
         B     PRINTX                                                           
         EJECT                                                                  
PROCREP  DS    0H                  * STATION REP REC *                          
         SPACE                                                                  
         CLC   QAREA+10(1),QSAVE+10      SAME TYPE OF REC                       
         BE    PR10                                                             
         SPACE                                                                  
         MVC   H7+6(19),=C'REP ADDRESS RECORDS'                                 
         MVC   H9+25(3),=C'REP'                                                 
         MVC   H10+25(3),=C'---'                                                
         MVI   RCSUBPRG,X'1'             REP HEADER                             
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
PR10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QAREA+10     REC TYPE                                     
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(3),QSTA       STATION CALL/REP CODE                        
         MVC   KEY+5(2),QAGY       AGENCY                                       
         SPACE                                                                  
         L     RC,ADREP                                                         
         USING REPREC,RC                                                        
         BAS   RE,HISTA                                                         
         CLC   KEY(7),REPKEY                                                    
         BNE   PREC10                                                           
         MVC   QSAVE+10(1),QAREA+10                                             
         SPACE                                                                  
         MVC   P+17(1),REPKMED                                                  
         MVC   P+25(3),REPKREP       REP CODE                                   
         MVC   P+39(22),RNAME                                                   
         MVC   P+62(24),R1LINE                                                  
         MVC   P+87(16),R2LINE                                                  
         MVC   P+106(2),R3LINE                                                  
         MVC   P+112(5),RZIP                                                    
         MVC   P+122(10),RZIP+13                                                
         SPACE                                                                  
         MVC   P+10(3),=C'ADD'                                                  
         CLI   QUESTOR-1,C'A'                                                   
         BE    PR20                                                             
         MVC   P+10(3),=C'CHA'                                                  
PR20     B     PRINTX                                                           
         EJECT                                                                  
PROCSTA  DS    0H                  * STATION MASTER REC *                       
         SPACE                                                                  
         CLC   QAREA+10(1),QSAVE+10      SAME TYPE OF REC                       
         BE    PS10                                                             
         SPACE                                                                  
         MVC   H7+6(22),=C'STATION MASTER RECORDS'                              
         MVI   RCSUBPRG,X'2'             STA HEADER                             
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
PS10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QAREA+10     REC TYPE                                     
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(5),QSTA       STATION CALL(REP CODE)                       
         MVC   KEY+7(2),QAGY       AGENCY                                       
         SPACE                                                                  
         L     RC,ADSTAT                                                        
         USING STAREC,RC                                                        
         CLC   QCLT,=C'ALL'                                                     
         BE    PS12                                                             
         MVC   KEY+9(3),QCLT                                                    
         BAS   RE,HISTA                                                         
         B     PS14                                                             
         SPACE                                                                  
PS12     BAS   RE,HISTA                                                         
         CLC   KEY(9),STAKEY                                                    
         B     PS16                                                             
PS14     CLC   KEY(12),STAKEY                                                   
         SPACE                                                                  
PS16     BNE   PREC10                                                           
         MVC   QSAVE+10(1),QAREA+10                                             
         SPACE                                                                  
         MVC   P+24(4),STAKEY+2    STATN CALL                                   
         MVC   P+17(1),STAKMED     STATN MEDIA                                  
         CLI   STAKEY+1,C'R'                                                    
         BNE   PMST1                                                            
         MVI   P+28,C'-'                                                        
         MVC   P+29(1),STAKEY+6    A OR F                                       
         MVI   P+30,C'M'                                                        
PMST1    MVC   P+33(1),STYPE                                                    
         MVC   P+43(3),SNETWRK                                                  
         CLC   SCHNL(4),=4C'0'                                                  
         BE    PMST4                                                            
         CLC   SCHNL(4),=4C' '                                                  
         BE    PMST4                                                            
         MVC   P+56(4),SCHNL                                                    
PMST4    CLI   STAKEY+9,X'F0'                                                   
         BE    *+10                                                             
         MVC   P+70(3),STAKEY+9                                                 
         MVC   P+82(4),SMKT                                                     
         LA    1,3                                                              
         LA    2,P+97                                                           
         LA    3,SREP                                                           
CPREP    CLC   0(3,3),=3X'F0'      CK MST FOR UP TO 3 REPS (CODES)              
         BE    PS15                                                             
         MVC   0(3,2),0(3)                                                      
         LA    2,4(2)                                                           
         LA    3,3(3)                                                           
         BCT   1,CPREP                                                          
         B     PS15                                                             
         SPACE                                                                  
PS15     MVC   P+10(3),=C'ADD'                                                  
         CLI   QUESTOR-1,C'A'                                                   
         BE    PS20                                                             
         MVC   P+10(3),=C'CHA'                                                  
*                                                                               
PS20     LA    R2,P+112           PRINT GST CODE                                
         MVC   0(1,R2),SGSTCODE                                                 
*                                                                               
PS30     B     PRINTX                                                           
         EJECT                                                                  
PROCADRS DS    0H                  * STATION ADDRESS REC *                      
         SPACE                                                                  
         CLC   QAREA+10(1),QSAVE+10      SAME TYPE OF REC                       
         BE    PA10                                                             
         SPACE                                                                  
         MVC   H7+6(23),=C'STATION ADDRESS RECORDS'                             
         MVC   H9+23(7),=C'STATION'                                             
         MVC   H10+23(7),=C'-------'                                            
         MVI   RCSUBPRG,X'1'                                                    
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
PA10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QAREA+10     REC TYPE                                     
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(5),QSTA       STATION CALL(REP CODE)                       
         MVC   KEY+7(2),QAGY       AGENCY                                       
         SPACE                                                                  
         L     RC,ADSTATAD                                                      
         USING ADDRREC,RC                                                       
         BAS   RE,HISTA                                                         
         CLC   KEY(9),ADDRKEY                                                   
         BNE   PREC10                                                           
         MVC   QSAVE+10(1),QAREA+10                                             
         SPACE                                                                  
         MVC   P+17(1),ADDKMED                                                  
         MVC   P+24(4),ADDRKEY+2   STATN CALL                                   
         MVC   P+39(20),ANAME                                                   
         MVC   P+62(24),A1LINE     ADDR                                         
         MVC   P+87(16),A2LINE     CITY                                         
         MVC   P+106(2),A3LINE     STATE                                        
         MVC   P+112(5),AZIP                                                    
         MVC   P+122(10),AZIP+5          NOW ABIGZIP                            
*                                                                               
PA15     MVC   P+10(3),=C'ADD'                                                  
         CLI   QUESTOR-1,C'A'                                                   
         BE    PA20                                                             
         MVC   P+10(3),=C'CHA'                                                  
PA20     B     PRINTX                                                           
         EJECT                                                                  
*                                                                               
* MARKET RECORDS *                                                              
         SPACE                                                                  
PROCMKT  DS    0H                                                               
         L     R8,ADAGY                                                         
         USING AGYHDR,R8                                                        
         MVI   RCSUBPRG,3                                                       
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(1),QAREA+10     REC TYPE                                     
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(4),QMKT       MKT                                          
         MVC   KEY+6(2),QAGY       AGENCY                                       
         SPACE                                                                  
         L     RC,ADMARKET                                                      
         USING MKTREC,RC                                                        
         BAS   RE,HISTA                                                         
         CLC   KEY(8),MKTKEY                                                    
         BNE   PREC10                                                           
         SPACE                                                                  
         MVC   P(11),=C'MARKET CODE'                                            
         MVC   P+18(4),MKTKMKT                                                  
         SPACE                                                                  
         MVC   P+98(3),=C'ADD'                                                  
         CLI   QUESTOR-1,C'A'                                                   
         BE    *+10                                                             
         MVC   P+98(6),=C'CHANGE'                                               
         SPACE                                                                  
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(11),=C'MARKET NAME'                                            
         MVC   P+18(24),MKTNAME                                                 
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(9),=C'TIME ZONE'                                               
         OC    MKTZONE,MKTZONE                                                  
         BZ    *+10                                                             
         MVC   P+18(1),MKTZONE                                                  
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(11),=C'SWEEP CLASS'                                            
         OC    MKTCLASS,MKTCLASS                                                
         BZ    *+10                                                             
         MVC   P+18(1),MKTCLASS                                                 
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(11),=C'MARKET RANK'                                            
         OC    MKTRANK,MKTRANK                                                  
         BZ    *+10                                                             
         MVC   P+18(3),MKTRANK                                                  
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(12),=C'MARKET HOMES'                                           
         OC    MKTHOMES,MKTHOMES                                                
         BZ    *+10                                                             
         MVC   P+18(8),MKTHOMES                                                 
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(3),=C'NTA'                                                     
         OC    MKTNTA,MKTNTA                                                    
         BZ    *+10                                                             
         MVC   P+18(2),MKTNTA                                                   
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(13),=C'MARKET WEIGHT'                                          
         OC    MKTWT,MKTWT                                                      
         BZ    PM10                                                             
         EDIT  (C4,MKTWT),(5,P+18),2,ALIGN=LEFT                                 
PM10     BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(12),=C'MARKET SHARE'                                           
         OC    MKTSHR,MKTSHR                                                    
         BZ    PM20                                                             
         EDIT  (C4,MKTSHR),(5,P+18),2,ALIGN=LEFT                                
PM20     BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(15),=C'RATING SRVC-MKT'                                        
         OC    MKTRS1,MKTRS1                                                    
         BZ    PM30                                                             
         MVC   P+18(3),=C'NSI'                                                  
         CLI   MKTRS1,C'0'                                                      
         BE    PM25                                                             
         MVC   P+18(3),=C'BBM'                                                  
         CLI   AGYPROF+7,C'C'                                                   
         BE    PM25                                                             
         MVC   P+18(3),=C'ARB'                                                  
PM25     EDIT  (2,MKTRSM1),(4,P+22)                                             
         BAS   R7,PRNT                                                          
         SPACE                                                                  
PM30     MVC   P(15),=C'RATING SRVC-MKT'                                        
         OC    MKTRS2,MKTRS2                                                    
         BZ    PM40                                                             
         MVC   P+18(3),=C'NSI'                                                  
         CLI   MKTRS2,C'0'                                                      
         BE    PM35                                                             
         MVC   P+18(3),=C'BBM'                                                  
         CLI   AGYPROF+7,C'C'        ONLY IF CANADIAN                           
         BE    PM35                                                             
         MVC   P+18(3),=C'ARB'                                                  
*                                                                               
PM35     EDIT  (2,MKTRSM2),(4,P+22)                                             
PM40     BAS   R7,PRNT                                                          
         SPACE                                                                  
         MVC   P(16),=C'LMT ACCESS CODES'                                       
         OC    MKTLTACC,MKTLTACC                                                
         BZ    *+10                                                             
         MVC   P+18(3),MKTLTACC                                                 
         MVI   SPACING,2                                                        
         BAS   R7,PRNT                                                          
         SPACE                                                                  
         B     EXIT                                                             
*                                                                               
PRNT     DS    0H                                                               
         GOTO1 REPORT                                                           
         BR    R7                                                               
         EJECT                                                                  
*                                                                               
HISTA    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(RC)                     
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
PRINTX   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
        PRINT ON                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042SPREP4502 05/01/02'                                      
         END                                                                    
