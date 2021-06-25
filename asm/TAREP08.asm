*          DATA SET TAREP08    AT LEVEL 032 AS OF 03/08/10                      
*PHASE T70308C,*                                                                
         TITLE 'T70308 - INVOICE REGISTER - VALIDATE SCREEN'                    
T70308   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70308                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TREGD,R7            INVOICE REGISTER DSECT                       
         L     RA,ATWA                                                          
         USING T703FFD,RA           SCREEN                                      
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VREC                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              VALIDATE RECORD                                                  
*                                                                               
VREC     DS    0H                                                               
         MVC   TRTODAY,TGTODAY1    TODAY'S PACKED DATE                          
         XC    TROPTS,TROPTS       CLEAR OPTIONS BYTE                           
         XC    TIFAGY,TIFAGY             AGENCY FILTER                          
         XC    TIFEMP,TIFEMP             EMPLOYER                               
         XC    TIQPSTR,TIQPSTR           START AND END DATES                    
         XC    TIQPEND,TIQPEND                                                  
         XC    DUEEDTE,DUEEDTE     EBCDIC DUE DATE FILTER                       
         XC    DUEPDTE,DUEPDTE     PWOS DUE DATE FILTER                         
         MVC   SINAGYN,SPACES            AGENCY NAME                            
         OI    SINAGYNH+6,X'80'    TRANSMIT                                     
         MVC   SINCURN,SPACES            CURRENCY                               
         OI    SINCURNH+6,X'80'    TRANSMIT                                     
         MVC   SINEMPN,SPACES            EMPLOYER NAME                          
         OI    SINEMPNH+6,X'80'    TRANSMIT                                     
         MVC   SINOFFN,SPACES            OFFICE NAME                            
         OI    SINOFFNH+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    R2,SINPERH          RUN FOR WHAT PERIOD                          
         GOTO1 ANY                                                              
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB   CLEAR OUTPUT BLOCK                           
         MVC   BYTE,5(R2)          SET L'INPUT FIELD                            
         OI    BYTE,X'40'          SET TO VALIDATE MM/DD                        
         MVC   PVALCSTA,TGTODAY2   SET TODAY'S/CONTROLLER'S DATE                
*                                                                               
         GOTO1 PERVAL,DMCB,(BYTE,WORK),('PVIN1DYL+PVINTOD',(R3))                
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BO    STRINV                                                           
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BO    ENDINV                                                           
         TM    4(R1),PVRCONE       IF BOTH DATES INPUT                          
         BO    VREC20                                                           
         CLI   4(R1),PVRCOK        THEN TEST GOOD RETURN CODE                   
         BNE   INVERR                                                           
         B     VREC30                                                           
*                                                                               
VREC20   MVC   PVALPEND,PVALPSTA   SET END DATE TO = START DATE                 
*                                                                               
VREC30   TM    PVALASSM,X'77'                                                   
         BZ    VREC90                                                           
         MVC   8(17,R2),PVALCPER   RE-DISPLAY GENERATED DATES                   
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
VREC90   MVC   PERIOD,8(R2)                                                     
         MVC   TREPDATE,PVALPEND                                                
         MVC   TRSPDATE,PVALPSTA                                                
*                                                                               
         MVC   TIQPEND,PVALPEND    SYSIO FILTERS                                
         MVC   TIQPSTR,PVALPSTA                                                 
         MVI   TIQDTYPE,TIQDBILL   FILTER ON BILL DATE                          
         GOTO1 DATCON,DMCB,(1,TREPDATE),(0,TRECHDTE)                            
         GOTO1 (RF),(R1),(1,TREPDATE),(8,TREDDATE)                              
         GOTO1 (RF),(R1),(1,TRSPDATE),(0,TRSCHDTE)                              
*                                                                               
         LA    R2,SINCURRH         BY CURRENCY                                  
         CLI   5(R2),0                                                          
         BE    VREC120                                                          
         CLI   8(R2),C'U'          DEFAULT TO US DOLLARS                        
         BNE   VREC100                                                          
         MVC   SINCURN(4),=C'U.S.'                                              
         B     VREC110                                                          
*                                                                               
VREC100  CLI   8(R2),C'C'          CANADIAN                                     
         BNE   VREC105                                                          
         MVC   SINCURN(6),=C'CANADA'                                            
         B     VREC110                                                          
*                                                                               
VREC105  CLI   8(R2),C'E'          EUROS                                        
         BNE   INVERR                                                           
         MVC   SINCURN(5),=C'EUROS'                                             
*                                                                               
VREC110  OI    SINCURNH+6,X'80'                                                 
         MVC   TIFCUR,SINCURR      SET SYSIO FILTER                             
*                                                                               
VREC120  LA    R2,SINEMPH          FILTER BY EMPLOYER                           
         CLI   5(R2),0                                                          
         BE    VREC130                                                          
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SINEMPNH                        
         MVC   TIFEMP,SINEMP                                                    
         OC    TIFEMP,SPACES                                                    
*                                                                               
VREC130  LA    R2,SINOFFH          FILTER BY OFFICE                             
         CLI   5(R2),0                                                          
         BE    VREC140                                                          
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'08',(R2)),SINOFFNH                        
*                                                                               
VREC140  LA    R2,SINAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    VREC150                                                          
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SINAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
         MVC   TNAME2,SINAGYN                                                   
         OC    TNAME2,SPACES                                                    
*                                                                               
VREC150  LA    R2,SINBNPH          FILTER ON B-N-P ONLY                         
         CLI   5(R2),0                                                          
         BE    VREC160                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC160                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    TROPTS,TRBNP        SET OPTION FLAG                              
*                                                                               
VREC160  LA    R2,SINCODH          FILTER ON COD ONLY                           
         CLI   5(R2),0                                                          
         BE    VREC170                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC170                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    TROPTS,TRCOD        SET OPTION FLAG                              
*                                                                               
VREC170  LA    R2,SINURGH          FILTER ON URG ONLY                           
         CLI   5(R2),0                                                          
         BE    VREC180                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC180                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    TROPTS,TRURG        SET OPTION FLAG                              
*                                                                               
VREC180  LA    R2,SINNCWH          FILTER ON NOT YET PAID                       
         CLI   5(R2),0                                                          
         BE    VREC190                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC190                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR                                                           
         OI    TROPTS,TRNCW        SET OPTION FLAG                              
*                                                                               
VREC190  LA    R2,SINDUEH          FILTER ON DUE DATE                           
         CLI   5(R2),0                                                          
         BE    VREC220                                                          
         GOTO1 ANY                 THIS WILL PUT DUE DATE IN WORK               
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB   CLEAR OUTPUT BLOCK                           
         MVC   BYTE,5(R2)          SET L'INPUT FIELD                            
         OI    BYTE,X'40'          SET TO VALIDATE MM/DD                        
*                                                                               
         GOTO1 PERVAL,DMCB,(BYTE,WORK),(R3)                                     
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BO    STRINV                                                           
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BO    ENDINV                                                           
         TM    4(R1),PVRCONE       IF BOTH DATES INPUT                          
         BO    VREC200                                                          
         CLI   4(R1),PVRCOK        THEN TEST GOOD RETURN CODE                   
         BNE   INVERR                                                           
         B     VREC210                                                          
*                                                                               
VREC200  MVC   PVALPEND,PVALPSTA   SET END DATE TO = START DATE                 
*                                                                               
VREC210  MVC   DUEPDTE,PVALPSTA                                                 
         MVC   DUEEDTE,PVALEEND                                                 
         MVC   TIQPSTR,DUEPDTE     RESET FILTER DATE                            
         MVC   TIQPEND,DUEPDTE                                                  
         MVI   TIQDTYPE,TIQDDUE    FILTER ON DUE DATE                           
*                                                                               
VREC220  LA    R2,SINOPTH          FOR FUTURE OPTIONS                           
         CLI   5(R2),0                                                          
         BE    VREC250                                                          
*                                                                               
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VREC230  CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VREC240                                                          
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TROPTS,TRTRACE      SET TRACE ON                                 
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
*                                                                               
VREC240  DS    0H                                                               
         LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VREC230          AND CONTINUE                                 
*                                                                               
VREC250  DS    0H                                                               
*                                                                               
VRECX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         SPACE 1                                                                
         USING SCAND,R3                                                         
ADDISP   NTR1                                                                   
         ZIC   RF,SCLEN1           L'LHS                                        
         ZIC   RE,SCLEN2           + L'RHS                                      
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RE,=H'1'            + '=' SIGN IF THERE IS A RIGHT HALF          
         LA    RF,1(RF,RE)         + DELIMITER                                  
         AH    RF,HALF             + L'SO FAR                                   
         STH   RF,HALF             = CURRENT DISPLACEMENT INTO FIELD            
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
NTFND    MVI   ERROR,NOTFOUND                                                   
         B     ERXIT                                                            
*                                                                               
ACCERR   MVI   ERROR,ERSWACC       CAN'T SWITCH TO ACC SYSTEM                   
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     ERXIT                                                            
*                                                                               
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     ERXIT                                                            
*                                                                               
ERXIT    DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF8D                                                       
         EJECT                                                                  
       ++INCLUDE TAREGD                                                         
         EJECT                                                                  
*DDPERVAL                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*ACGENBOTH                                                                      
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032TAREP08   03/08/10'                                      
         END                                                                    
