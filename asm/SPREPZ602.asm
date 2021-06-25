*          DATA SET SPREPZ602  AT LEVEL 007 AS OF 05/01/02                      
*PHASE SPZ602A                                                                  
         TITLE 'SPREPZ602 - EASI - INVOICE REPORT'                              
         PRINT NOGEN                                                            
*                                                                               
*        QOPT1      TRACE A=ALL,E=ERRORS,M=E+MODES,M+RECORDS                    
*                                                                               
SPZ602   CSECT                                                                  
         NMOD1 0,SPZ602                                                         
*                                                                               
         L     R9,0(R1)                                                         
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,R9,RA                                                    
         LA    RC,SPACEND                                                       
         USING SPZ6WRKD,RC                                                      
         L     R7,=A(EZBLOCKC)                                                  
         USING EZBLOCKD,R7                                                      
*                                                                               
         L     RF,UTL                                                           
         MVC   SAVSYS,4(RF)                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    PROC                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
* REQUEST FIRST                                                                 
*                                                                               
PROC     DS    0H                                                               
         LA    RE,EZBLOCKD                                                      
         LA    RF,EZBLOCKL                                                      
         XCEF                                                                   
*                                                                               
         MVC   EZWKRFIL,=CL8'WKFILE'                                            
         L     RF,=A(WRKRBUFF)                                                  
         ST    RF,EZWKRBUF                                                      
         L     RF,=A(WRKRREC)                                                   
         ST    RF,EZWKRREC                                                      
         LA    RF,EZMPROC                                                       
         ST    RF,EZHOOK                                                        
         L     RF,ADBUY                                                         
         ST    RF,EZAREC                                                        
*                                                                               
         MVC   EZCOMFCS,ACOMFACS                                                
         MVC   EZPRINT,PRINT                                                    
         MVC   EZUTL,UTL                                                        
*                                                                               
         MVC   EZWRITE,RCWRITE                                                  
         MVC   EZAGY,AGY                                                        
         MVC   EZBAGYMD,BAGYMD                                                  
*                                                                               
         MVC   EZTEST,QOPT1        TEST OPTION                                  
         MVI   EZTRACE,0           TRACE OPTION                                 
         CLI   QOPT1,C'N'                                                       
         BE    PR100                                                            
         CLI   QOPT1,C' '                                                       
         BE    PR100                                                            
         CLI   QOPT1,C'A'                                                       
         BNE   *+8                                                              
         MVI   EZTRACE,X'FF'       TRACE EVERYTHING                             
         CLI   QOPT1,C'E'                                                       
         BNE   *+8                                                              
         MVI   EZTRACE,X'20'       TRACE ERRORS                                 
         CLI   QOPT1,C'M'                                                       
         BNE   *+8                                                              
         MVI   EZTRACE,X'60'       TRACE MODES AND ERRORS                       
         CLI   QOPT1,C'F'                                                       
         BNE   *+8                                                              
         MVI   EZTRACE,X'E0'       TRACE FIELDS AND ERRORS AND MODES            
         CLI   QOPT1,C'R'                                                       
         BNE   *+8                                                              
         MVI   EZTRACE,X'70'       TRACE MODES, ERRORS, AND ADDED RECS          
*                                                                               
PR100    DS    0H                                                               
         XC    EZWKRIND,EZWKRIND                                                
*                                                                               
PR200    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EZWKRFIL,EZWKRIND,EZWKRREC,      X        
               EZWKRBUF                                                         
         TM    DMCB+8,X'80'        TEST EOF                                     
         BNZ   PR300                                                            
*                                                                               
         LA    R4,EZWKRIND                                                      
         USING UKRECD,R4                                                        
         CLI   UKDAY,X'99'         TEST AN EASI FILE                            
         BNE   PR200                                                            
         USING EZWKRIXD,R4                                                      
*                                                                               
         CLC   EZWIUID,RCORIGID    TEST RIGHT ID                                
         BNE   PR200                                                            
*                                                                               
         MVC   WORK(4),EZWISTN         PACK STATION                             
         MVC   WORK+4(1),EZWIMED                                                
         GOTO1 MSPACK,DMCB,=C'0000',WORK,BMKTSTA                                
*                                  READ FIRST RECORD                            
         GOTO1 DATAMGR,DMCB,=C'READ',EZWKRFIL,EZWKRIND,EZWKRREC,       X        
               EZWKRBUF                                                         
         TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
         BNZ   PR200               SKIP- GET NEXT INDEX                         
*                                                                               
         L     R3,EZWKRBUF         SAVE 64 BYTE DESCRIPTOR REC                  
         MVC   SVWFREC0,0(R3)                                                   
*                                                                               
         USING WKRECD,R3                                                        
         GOTO1 =V(EZMOD),DMCB,EZBLOCKD                                          
         B     PR200               NEXT INDEX                                   
         DROP  R3,R4                                                            
         SPACE 2                                                                
*                                  DRIVER OUTPUT PHASE                          
*                                  -------------------                          
PR300    B     EXIT                                                             
         EJECT                                                                  
*        EZMOD PROCESSING ROUTINE                                               
*                                                                               
EZMPROC  NTR1                                                                   
         ST    R0,SAVER0                                                        
         LA    R3,MODESET                                                       
         LA    R0,MODESETC                                                      
EZ100    CLC   EZMODE,0(R3)                                                     
         BE    EZ120                                                            
         LA    R3,MODESETL(R3)                                                  
         BCT   R0,EZ100                                                         
         B     EZXIT                                                            
*                                                                               
EZ120    SR    RF,RF                                                            
         ICM   RF,7,1(R3)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
EZXIT    B     EXIT                                                             
         EJECT                                                                  
*              INVP SET UP HEADING INFO                                         
INVPRTN  ST    RE,SAVERE                                                        
         MVC   P1+0(7),=C'AGENCY-'                                              
         MVC   P1+8(30),EZAGNAM                                                 
         MVC   P1+41(8),=C'STATION-'                                            
         MVC   P1+50(7),EZSTATN                                                 
         MVC   P1+82(6),=C'PAYEE-'                                              
         MVC   P1+89(30),EZPYNAM                                                
*                                                                               
         MVC   P2+8(30),EZAGLIN1                                                
         MVC   P2+50(7),EZSTLIN1                                                
         MVC   P2+89(30),EZPYPYLN1                                              
*                                                                               
         MVC   P3+8(30),EZAGLIN2                                                
         MVC   P3+50(7),EZSTLIN2                                                
         MVC   P3+89(30),EZPYPYLN2                                              
*                                                                               
         MVC   P4+8(30),EZAGLIN3                                                
         MVC   P4+50(7),EZSTLIN3                                                
         MVC   P4+89(30),EZPYPYLN3                                              
*                                                                               
         MVC   P5+8(30),EZAGLIN4                                                
         MVC   P5+50(7),EZSTLIN4                                                
         MVC   P5+89(30),EZPYPYLN4                                              
*                                                                               
         MVC   P7+0(14),=C'INVOICE NUMBER-'                                     
         MVC   P7+16(30),EZIHINV                                                
         MVC   P7+28(5),=C'DATE-'                                               
         MVC   P7+50(7),EZIHDIDT                                                
         CLI   NEWINV,C'Y'                                                      
         BNE   IP100                                                            
         MVI   NEWINV,C'N'                                                      
         MVC   P7+73(15),=C'REPRESENTATIVE-'                                    
         MVC   P7+88(7),EZSTREP                                                 
*                                                                               
         MVC   P8+73(12),=C'SALESPERSON-'                                       
         MVC   P8+88(25),EZIHSLSP                                               
*                                                                               
         MVC   P9+0(11),=C'ADVERTISER-'                                         
         MVC   P9+12(25),EZIHADVN                                               
         MVC   P9+38(8),=C'BROADCAST MONTH-'                                    
         MVC   P9+55(6),EZIHDMOS                                                
         MVC   P9+73(15),=C'AGENCY CONTACT-'                                    
         MVC   P9+88(25),EZIHACON                                               
*                                                                               
         MVC   P10+0(8),=C'PRODUCT-'                                            
         MVC   P10+12(25),EZIHPRDN                                              
         MVC   P10+38(16),=C'SCHEDULE DATES-'                                   
         MVC   P10+55(17),EZIHSDT                                               
         MVC   P10+73(15),=C'STATION CONTACT'                                   
         MVC   P10+88(25),EZIHSCON                                              
*                                                                               
         MVC   P11+0(9),=C'ESTIMATE-'                                           
         MVC   P11+12(10),EZIHEST                                               
         MVC   P11+38(14),=C'ORDER NUMBER-'                                     
         MVC   P11+55(17),EZIHCNTR                                              
         LA    R2,P13+100                                                       
         B     *+8                                                              
IP100    LA    R2,P8+100                                                        
         MVC   0(4,R2),=C'PAGE'                                                 
         SR    R0,R0                                                            
         ICM   R0,3,PAGE                                                        
         EDIT  (R0),(4,5(R2),ALIGN=LEFT                                         
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1+14(8),=C'SCHEDULE'                                            
         MVC   P1+55(16),=C'ACTUAL BROADCAST'                                   
         MVC   P1+93(13),=C'RECONCILATION'                                      
*                                                                               
         MVC   P2+22(4),=C'RATE'                                                
         MVC   P2+36(3),=C'NO.'                                                 
         MVC   P2+69(3),=C'M/G'                                                 
*                                                                               
         MVC   P3+3(L'LITLN3LE),LITLN3                                          
         MVC   P3+123(5),=C'DR/CR'                                              
         GOTO1 REPORT                                                           
IPXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              LINP SET UP PRINT LINE                                           
LINPRTN  ST    RE,SAVERE                                                        
         MVC   PDAY,EZIODDYS                                                    
         MVC   PTIME(5),EZIODSTM                                                
         MVI   PTIME+5,C'-'                                                     
         MVC   PTIME+6(5),EZIODETM                                              
         L     R0,EZIOBRAT                                                      
         EDIT  (R0),(7,PRATE),2                                                 
         MVC   PMTH,EZIONTMS                                                    
LPXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              ANNP SET ANNOC. UP PRINT LINE                                    
ANNPRTN  ST    RE,SAVERE                                                        
         MVC   PADATE,EZIADDTS                                                  
         MVC   PADAY,EZIADDAY                                                   
         MVC   PATIME,EZIADTIM                                                  
         MVC   PATYPE,EZIATYP                                                   
         MVC   PACLASS,EZIACLS                                                  
         MVC   PAPB,EZIAPGB                                                     
         MVC   PAMG,EZIADMGD                                                    
         L     R0,EZIABRAT                                                      
         EDIT  (R0),(7,PARATE),2                                                
APXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*              RECP SET RECON PRINT LINE                                        
RECPRTN  ST    RE,SAVERE                                                        
RPXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*             ANNL LAST ANNOC.                                                  
ANNLRTN  ST    RE,SAVERE                                                        
ALXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*             LINL LAST LINE                                                    
LINLRTN  ST    RE,SAVERE                                                        
LLXIT    L     RE,SAVERE                                                        
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
LITLN3   DC    C'DAYS      TIME     DTAIL   RATE  MTH   DATE  DAY  TIMEX        
               CLASS  P/B   FOR  PRODUCT/FILM     RATE    REMARKS'              
LITLN3L  EQU   *-LITLN3                                                         
MODESET  DC    AL1(EZINVP),AL3(INVPRTN-SPZ602)                                  
MODESETL EQU   *-MODESET                                                        
         DC    AL1(EZLINP),AL3(LINPRTN-SPZ602)                                  
         DC    AL1(EZANNP),AL3(ANNPRTN-SPZ602)                                  
         DC    AL1(EZRECP),AL3(RECPRTN-SPZ602)                                  
         DC    AL1(EZLINL),AL3(LINLRTN-SPZ602)                                  
         DC    AL1(EZANNL),AL3(ANNLRTN-SPZ602)                                  
         DC    AL1(EZRECL),AL3(RECLRTN-SPZ602)                                  
         DC    AL1(EZINVL),AL3(INVLRTN-SPZ602)                                  
MODESETC EQU   *-MODESET/MODESETL                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*WKRBUF*'                                                    
WRKRBUFF DS    4096X                                                            
         DS    0D                                                               
         DC    CL8'*WKRREC*'                                                    
WRKRREC  DS    2000X                                                            
         DS    0D                                                               
         DC    CL8'*EZBLKF*'                                                    
EZBLOCKC DS    0X                                                               
         ORG   *+EZBLOCKL                                                       
         EJECT                                                                  
         DS    0D                                                               
SPZ6WRKD DSECT                                                                  
*                                                                               
SVKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
SAVSYS   DS    XL1                                                              
FIRSTSW  DS    CL1                                                              
TRCOPT   DS    CL1                                                              
NEWINV   DS    CL1                                                              
LPPTRSW  DS    CL1                                                              
APPTRSW  DS    CL1                                                              
         DS    0F                                                               
SAVER0   DS    F                                                                
SAVERE   DS    F                                                                
         DS    0D                                                               
SVWFREC0 DS    XL64                                                             
         EJECT                                                                  
PLINED   DSECT                                                                  
PLINE    DS    0CL132                                                           
         DS    CL1                                                              
PDAYS    DS    CL8                                                              
         DS    CL1                                                              
PDTIME   DS    CL11                                                             
         DS    CL1                                                              
PRATED   DS    CL3                                                              
         DS    CL1                                                              
PRATE    DS    CL7                                                              
         DS    CL1                                                              
PMTH     DS    CL7                                                              
         DS    CL3                                                              
PADATE   DS    CL5                                                              
         DS    CL1                                                              
PADAY    DS    CL3                                                              
         DS    CL1                                                              
PATIME   DS    CL5                                                              
         DS    CL1                                                              
PATYPE   DS    CL3                                                              
         DS    CL1                                                              
PACLASS  DS    CL3                                                              
         DS    CL1                                                              
PAPB     DS    CL7                                                              
         DS    CL1                                                              
PAMG     DS    CL5                                                              
         DS    CL1                                                              
PAPF     DS    CL14                                                             
         DS    CL1                                                              
PARATE   DS    CL7                                                              
         DS    CL3                                                              
PARE     DS    CL18                                                             
         DS    CL1                                                              
PADRCR   DS    CL7                                                              
         DS    CL1                                                              
*                                                                               
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPGENEZ                                                        
         PRINT ON                                                               
*                                                                               
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPZ602 05/01/02'                                      
         END                                                                    
*                                                                               
