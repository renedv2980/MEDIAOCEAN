*          DATA SET ACREPWL02  AT LEVEL 008 AS OF 03/06/97                      
*PHASE ACWL02A                                                                  
ACWL02   TITLE '- WORKER FILE LISTING'                                          
ACWL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**WL02**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
         L     R8,ADMASTC                                                       
         USING MASTD,R8                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         CLI   QOPT1,C'A'          ALL IDS                                      
         BE    *+10                                                             
         MVC   WRKUID,ORIGINUM                                                  
*                                                                               
         MVC   WRKSPG,QUESTOR      SYSTEM PROGRAM                               
*                                                                               
         CLI   QUESTOR+3,C'*'      SUB-PROGRAM                                  
         BE    *+10                                                             
         MVC   WRKSUB,QUESTOR+3                                                 
*                                                                               
         CLC   QUESTOR+4(2),=C'**' DAY                                          
         BE    *+16                                                             
         PACK  DUB(2),QUESTOR+4(3)                                              
         MVC   WRKDAY,DUB                                                       
*                                                                               
         CLI   QUESTOR+6,C'*'      TYPE                                         
         BE    *+10                                                             
         MVC   WRKTYP,QUESTOR+6                                                 
*                                                                               
         CLI   QUESTOR+7,C'*'                                                   
         BE    WL00                                                             
         PACK  DUB,QUESTOR+7(4)                                                 
         CVB   R0,DUB                                                           
         STCM  R0,3,WRKSEQ                                                      
*                                                                               
WL00     MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'A'                                                       
         BNE   WL01                                                             
         OC    SAVKEY,SAVKEY                                                    
         BNZ   *+16                                                             
         MVC   SAVKEY,WRKKEY                                                    
         XC    WRKKEY,WRKKEY                                                    
         GOTO1 WORKER,DMCB,WRKINDX,AWRKBUFF,WRKKEY                              
         CLI   8(R1),0                                                          
         BNE   EXIT                                                             
         CLC   WRKSPG,SAVKEY+(WRKSPG-WRKKEY)                                    
         BNE   WL00                                                             
         CLI   SAVKEY+(WRKSUB-WRKKEY),0                                         
         BE    *+14                                                             
         CLC   WRKSUB,SAVKEY+(WRKSUB-WRKKEY)                                    
         BNE   WL00                                                             
         CLI   SAVKEY+(WRKDAY-WRKKEY),0                                         
         BE    *+14                                                             
         CLC   WRKDAY,SAVKEY+(WRKDAY-WRKKEY)                                    
         BNE   WL00                                                             
         CLI   SAVKEY+(WRKTYP-WRKKEY),0                                         
         BE    *+14                                                             
         CLC   WRKTYP,SAVKEY+(WRKTYP-WRKKEY)                                    
         BNE   WL00                                                             
         OC    SAVKEY+(WRKSEQ-WRKKEY)(L'WRKSEQ),SAVKEY+(WRKSEQ-WRKKEY)          
         BZ    *+14                                                             
         CLC   WRKSEQ,SAVKEY+(WRKSEQ-WRKKEY)                                    
         BNE   WL00                                                             
         CLI   QOPT2,C'U'          unkept only                                  
         BNE   *+12                                                             
         TM    WRKSTA,X'08'        TEST KEEP                                    
         BO    WL00                                                             
         B     WL02                                                             
*                                                                               
WL01     GOTO1 WORKER,DMCB,WRKINDX,AWRKBUFF,WRKKEY                              
         CLI   8(R1),0                                                          
         BE    WL02                                                             
         MVC   P(16),=C'NO SUCH ID ,KEY='                                       
         GOTO1 HEXOUT,DMCB,WRKKEY,P+16,8                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DELETE FILE FEATURE                                                 *         
***********************************************************************         
         SPACE 2                                                                
WL02     TM    RUNINDS1,RUNIDELS                                                
         BZ    WL04                                                             
         GOTO1 WORKER,DMCB,WRKDELT,AWRKBUFF,WRKKEY                              
         MVC   P(22),=C'ENTRY HAS BEEN DELETED'                                 
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* PURGE FILE FEATURE                                                  *         
***********************************************************************         
         SPACE 2                                                                
WL04     TM    RUNINDS1,RUNIPURG                                                
         BZ    WL06                                                             
         GOTO1 WORKER,DMCB,WRKPRGE,AWRKBUFF,WRKKEY                              
         MVC   P(21),=C'ENTRY HAS BEEN PURGED'                                  
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* KEEP FILE FEATURE                                                   *         
***********************************************************************         
         SPACE 2                                                                
WL06     TM    RUNINDS1,RUNIKEEP                                                
         BZ    WL08                                                             
         TM    WRKSTA,X'08'                                                     
         BO    EXIT                                                             
         GOTO1 WORKER,DMCB,WRKKEEP,AWRKBUFF,WRKKEY                              
         MVC   P(28),=C'ENTRY STATUS CHANGED TO KEEP'                           
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* RESTORE FILE FEATURE                                                *         
***********************************************************************         
         SPACE 2                                                                
WL08     TM    RUNINDS1,RUNIREST                                                
         BZ    WL10                                                             
         GOTO1 WORKER,DMCB,WRKUNKP,AWRKBUFF,WRKKEY                              
         GOTO1 (RF),(R1),WRKREST,AWRKBUFF,WRKKEY                                
         MVC   P(14),=C'ENTRY RESTORED'                                         
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ FILE AND PRINT                                                 *         
***********************************************************************         
         SPACE 1                                                                
WL10     L     R1,ADBXAREA                                                      
         USING BOXD,R1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+08,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXCOLS+(PBL-P),C'L'                                             
         MVI   BOXCOLS+(PB1-P),C'C'                                             
         MVI   BOXCOLS+(PB2-P),C'C'                                             
         MVI   BOXCOLS+(PB3-P),C'C'                                             
         MVI   BOXCOLS+(PB4-P),C'C'                                             
         MVI   BOXCOLS+(PB5-P),C'C'                                             
         MVI   BOXCOLS+(PB6-P),C'C'                                             
         MVI   BOXCOLS+(PB7-P),C'C'                                             
         MVI   BOXCOLS+(PB8-P),C'C'                                             
         MVI   BOXCOLS+(PBR-P),C'R'                                             
         DROP  R1                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   FILEIT,=P'0'                                                     
         ZAP   FILEDR,=P'0'                                                     
         ZAP   FILECR,=P'0'                                                     
*                                                                               
WL12     GOTO1 WORKER,DMCB,WRKREAD,AWRKBUFF,WRKKEY,WRKIOLN                      
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   WLTOT                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R1,WRKIOLN                                                       
         LA    R1,WRKIOLN(R1)                                                   
         MVI   0(R1),0                                                          
         LA    R2,WRKIO                                                         
         USING PSTKEYD,R2                                                       
         CLI   PSTKEL,PSTKELQ                                                   
         BNE   WL13                                                             
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,PSTKEY,IOAREA                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,IOAREA                                                        
         USING TRNRECD,R3                                                       
         MVC   PACT,TRNKUNT                                                     
         MVC   PCAC(L'TRNKULC),TRNKCUNT                                         
         CLI   TRNKCCPY,C'*'                                                    
         BE    *+12                                                             
         CLI   TRNKCCPY,C' '                                                    
         BNE   *+10                                                             
         MVC   PCAC,TRNKCCPY                                                    
         LA    R2,TRNRECD+ACCORFST                                              
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PWRK2,TRNOFFC                                                    
         AP    FILEIT,=P'1'                                                     
         B     WL14                                                             
*                                                                               
         USING PSHEADD,R2                                                       
WL13     CLI   PSHDEL,PSSBELQ                                                   
         BE    WLTOT                                                            
         CLI   PSHDEL,CRDELQ       CHECK REGISTER ELEMENT                       
         BE    WL12                SKIP IT                                      
         CLI   PSHDEL,PSHDELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PACT,PSHDACC+(ACTKUNT-ACTKEY)                                    
         MVC   PCAC(L'PCAC-1),PSHDSBAC+1                                        
         CLI   PSHDSBAC,C'*'                                                    
         BE    *+12                                                             
         CLI   PSHDSBAC,C' '                                                    
         BNE   *+10                                                             
         MVC   PCAC,PSHDSBAC                                                    
         MVC   PWRK1,PSHDANAL                                                   
         AP    FILEIT,=P'1'                                                     
         SR    R0,R0                                                            
         IC    R0,PSHDLEN                                                       
         AR    R2,R0                                                            
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   WL18                                                             
         MVC   PWRK2,TRNOFFC                                                    
         CLC   PWRK1,SPACES                                                     
         BE    WL14                                                             
         CLC   PWRK2,SPACES                                                     
         BE    WL14                                                             
         MVI   PDLM,C'/'                                                        
*                                                                               
WL14     GOTO1 DATCON,DMCB,(1,TRNDATE),(8,PDAT)                                 
         MVC   PREF,TRNREF                                                      
         MVC   PBAT,TRNBTCH                                                     
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNNARR+1-TRNELD)                                          
         BM    WL16                                                             
         CLC   TRNOFFC,=C'99'                                                   
         BNE   *+8                                                              
         LA    R1,14                                                            
         MVI   BIGWORK,C' '                                                     
         MVC   BIGWORK+1(L'BIGWORK-1),BIGWORK                                   
         EX    R1,*+4                                                           
         MVC   BIGWORK(0),TRNNARR                                               
         GOTO1 CHOPPER,DMCB,(L'BIGWORK,BIGWORK),(L'PNAR,PNAR),1                 
*                                                                               
WL16     LA    RE,FILEDR                                                        
         LA    R3,PDEB                                                          
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+12                                                             
         LA    RE,FILECR                                                        
         LA    R3,PCRD                                                          
         AP    0(L'FILEDR,RE),TRNAMNT                                           
         EDIT  (P6,TRNAMNT),(13,0(R3)),2,MINUS=YES                              
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'                                                       
         TM    RUNINDS1,RUNIREVS                                                
         BZ    *+10                                                             
         ZAP   TRNAMNT,DUB                                                      
*                                                                               
WL18     GOTO1 PRNTIT              PRINT DETAIL LINE                            
*                                                                               
WL20     SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   WL22                                                             
         TM    RUNINDS1,RUNIOTHS                                                
         BZ    WL36                                                             
         GOTO1 PRNTIT                                                           
         B     WL36                                                             
*                                                                               
         USING NAMELD,R2                                                        
WL22     CLI   NAMEL,NAMELQ                                                     
         BNE   WL24                                                             
         TM    RUNINDS1,RUNIOTHS                                                
         BZ    WL20                                                             
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   PNAR(5),=C'Name='                                                
         EX    RE,*+4                                                           
         MVC   PNAR+5(0),NAMEREC                                                
         GOTO1 PRNTIT                                                           
         B     WL20                                                             
*                                                                               
         USING ADRELD,R2                                                        
WL24     CLI   ADREL,ADRELQ                                                     
         BNE   WL28                                                             
         TM    RUNINDS1,RUNIOTHS                                                
         BZ    WL20                                                             
         MVC   PNAR(8),=C'Address='                                             
         SR    R0,R0                                                            
         IC    R0,ADRNUM                                                        
         LA    R1,ADRADD1                                                       
WL26     MVC   PNAR+8(L'ADRADD1),0(R1)                                          
         GOTO1 PRNTIT                                                           
         LA    R1,L'ADRADD1(R1)                                                 
         BCT   R0,WL26                                                          
         B     WL20                                                             
*                                                                               
         USING RSTELD,R2                                                        
WL28     CLI   RSTEL,RSTELQ                                                     
         BNE   WL30                                                             
         TM    RUNINDS1,RUNIOTHS                                                
         BZ    WL20                                                             
         MVC   PNAR(7),=C'Status='                                              
         MVC   PNAR+7(5),RSTANAL                                                
         GOTO1 PRNTIT                                                           
         B     WL20                                                             
*                                                                               
         USING SCIELD,R2                                                        
WL30     CLI   SCIEL,SCIELQ                                                     
         BNE   WL20                                                             
         TM    RUNINDS1,RUNIOTHS                                                
         BZ    WL34                                                             
         MVC   PNAR(30),=C'Subsidiary cash,Type=X,Amount='                      
         MVC   PNAR+21(L'SCITYPE),SCITYPE                                       
         EDIT  (P6,SCIAMNT),(10,PNAR+30),2,MINUS=YES,ALIGN=LEFT                 
         CLI   SCILN,SCILN2Q                                                    
         BL    WL32                                                             
         LR    R3,R0                                                            
         LA    R3,PNAR+30(R3)                                                   
         MVI   1(R3),C'/'                                                       
         EDIT  (P6,SCINET),(10,3(R3)),2,MINUS=YES,ALIGN=LEFT                    
WL32     GOTO1 PRNTIT                                                           
WL34     TM    RUNINDS1,RUNIREVS                                                
         BZ    WL20                                                             
         ZAP   DUB,SCIAMNT                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SCIAMNT,DUB                                                      
         CLI   SCILN,SCILN2Q                                                    
         BL    WL20                                                             
         ZAP   DUB,SCINET                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   SCINET,DUB                                                       
         B     WL20                                                             
*                                                                               
WL36     TM    RUNINDS1,RUNIREVS                                                
         BZ    WL12                                                             
         GOTO1 WORKER,DMCB,WRKWRIT,AWRKBUFF,WRKKEY                              
         BE    WL12                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* END OF FILE ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
WLTOT    MVC   P,SPACES                                                         
         GOTO1 PRNTIT                                                           
         MVC   PNAR(13),=C'Detail totals'                                       
         EDIT  (P6,FILEIT),(10,PNAR+20)                                         
         MVC   PNAR+31(5),=C'Items'                                             
         EDIT  (P6,FILEDR),(13,PDEB),2,MINUS=YES                                
         EDIT  (P6,FILECR),(13,PCRD),2,MINUS=YES                                
         MVI   PSECOND,0                                                        
         GOTO1 PRNTIT                                                           
         USING PSSUBFD,R2                                                       
         CLI   PSSBEL,PSSBELQ                                                   
         BNE   EXIT                                                             
         MVC   PCAC,PSSBDESC                                                    
         MVC   PNAR(14),=C'Summary totals'                                      
         EDIT  (P6,PSSBRECS),(10,PNAR+20)                                       
         MVC   PNAR+31(5),=C'Items'                                             
         EDIT  (P6,PSSBCASH),(13,PDEB),2,MINUS=YES                              
         MVC   PCRD,PDEB                                                        
         GOTO1 PRNTIT                                                           
         TM    RUNINDS1,RUNIREVS                                                
         BZ    WLTOT3                                                           
         ZAP   DUB,PSSBCASH                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   PSSBCASH,DUB                                                     
         GOTO1 WORKER,DMCB,WRKWRIT,AWRKBUFF,WRKKEY                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WLTOT3   CLI   QOPT3,C'K'          KEEP ZERO RUN                                
         BNE   WLNXT                                                            
         CP    PSSBCASH,=P'0'                                                   
         BNE   WLNXT                                                            
         GOTO1 WORKER,DMCB,WRKKEEP,AWRKBUFF,WRKKEY                              
         MVC   P(28),=C'ENTRY STATUS CHANGED TO KEEP'                           
         GOTO1 ACREPORT                                                         
*                                                                               
WLNXT    CLI   QOPT1,C'A'                                                       
         BE    WL00                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT WORKER KEY AND PRINT A LINE                                  *         
***********************************************************************         
         SPACE 1                                                                
PRNTIT   NTR1  ,                                                                
         BAS   RE,GETID            GET APLHA ID INTO WORK                       
         MVI   HEAD5,0                                                          
         MVC   HEAD4+6(L'USERID),USERID                                         
         LA    RE,HEAD4+6+L'USERID-1                                            
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C','                                                       
         MVC   2(4,RE),WRKSPG                                                   
         CLI   5(RE),0                                                          
         BNE   *+8                                                              
         MVI   5(RE),C'*'                                                       
         UNPK  6(3,RE),WRKDAY(2)                                                
         MVC   8(1,RE),WRKTYP                                                   
         MVI   9(RE),C','                                                       
         EDIT  (B2,WRKSEQ),(4,10(RE)),ALIGN=LEFT                                
         GOTO1 ACREPORT                                                         
PRNTITX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETID- READ CONTROL FILE FOR ID                                               
***********************************************************************         
                                                                                
GETID    NTR1  ,                                                                
         LA    R5,IO1                                                           
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),WRKUID                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,CTIDATA          FIND ID ELEMENT                              
         SR    R0,R0                                                            
*                                                                               
GET8     CLI   0(R5),0             NO ELEMENT                                   
         BE    EXIT                                                             
         CLI   0(R5),X'02'                                                      
         BE    GET10                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GET8                                                             
*                                                                               
GET10    MVC   USERID,2(R5)                                                     
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ACCOUNT  DC    C'ACCOUNT'                                                       
AWRKBUFF DC    A(WRKBUFF)                                                       
WRKREAD  DC    C'READ'                                                          
WRKWRIT  DC    C'WRITE'                                                         
WRKUNKP  DC    C'UNKEEP'                                                        
WRKKEEP  DC    C'KEEP'                                                          
WRKPRGE  DC    C'PURGE'                                                         
WRKDELT  DC    C'DELETE'                                                        
WRKREST  DC    C'RESTORE'                                                       
WRKINDX  DC    C'INDEX'                                                         
WRKBUFF  DC    4500X'00'                                                        
         EJECT                                                                  
WORKD    DSECT                     ** WORKING STORAGE **                        
BIGWORK  DS    CL200                                                            
*                                                                               
WRKKEY   DS    0XL16               ** WORKER KEY **                             
WRKUID   DS    XL2                 USER-ID NUMBER                               
WRKSPG   DS    CL3                 SYSTEM/PROGRAM                               
WRKSUB   DS    CL1                 SUB-PROGRAM/LEDGER                           
WRKDAY   DS    CL1                 DAY                                          
WRKTYP   DS    CL1                 FILE TYPE                                    
WRKTYPQ  EQU   C'P'                DEFAULT BIGWORK FILE TYPE                    
         DS    XL2                 N/D                                          
WRKSEQ   DS    XL2                 SEQUENCE NUMBER                              
WRKSTA   DS    XL1                 STATUS                                       
         ORG   WRKKEY+L'WRKKEY                                                  
*                                                                               
SAVKEY   DS    XL(L'WRKKEY)                                                     
*                                                                               
RUNINDS1 DS    XL1                 ** RUN INDICATORS BYTE 1 **                  
RUNIDELS EQU   X'40'               DELETE=YES                                   
RUNIOTHS EQU   X'20'               OTHERS                                       
RUNIREVS EQU   X'10'               REVERSE                                      
RUNIKEEP EQU   X'08'               KEEP                                         
RUNIREST EQU   X'04'               RESTORE                                      
RUNIPURG EQU   X'02'               PURGE                                        
*                                                                               
USERID   DS    CL8                 USER-ID CODE                                 
FILEDR   DS    PL6                 FILE DEBITS                                  
FILECR   DS    PL6                 FILE CREDITS                                 
FILEIT   DS    PL6                 FILE NUMBER OF ITEMS                         
CPYCODE  DS    XL1                 COMPANY CODE                                 
LOZLOGO  DS    CL7                 COMPANY LOGO                                 
LOZNAME  DS    CL36                COMPANY NAME                                 
LOZADDR  DS    CL44                COMPANY ADDRESS                              
*                                                                               
IO1      DS    XL1000                                                           
*                                                                               
WRKIOLN  DS    F                   WORKER RECORD LENGTH                         
WRKIO    DS    XL2000              WORKER RECORD & GENERAL I/O AREA             
IOAREA   DS    XL2000              FOR FILE READING                             
WKLSTDX  EQU   *                                                                
         EJECT                                                                  
         ORG                                                                    
         SPACE 1                                                                
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                                                                
PBL      DS    CL1                                                              
PACT     DS    CL14                ACCOUNT CODE (U/L/ACCOUNT)                   
PB1      DS    CL1                                                              
PCAC     DS    CL15                CONTRA ACCOUNT                               
PB2      DS    CL1                                                              
PWRK1    DS    CL2                 BIGWORK CODE1                                
PDLM     DS    CL1                                                              
PWRK2    DS    CL2                 BIGWORK CODE 2                               
PB3      DS    CL1                                                              
PDAT     DS    CL8                 TRANSACTION DATE                             
PB4      DS    CL1                                                              
PREF     DS    CL6                 REFERENCE NUMBER                             
PB5      DS    CL1                                                              
PBAT     DS    CL6                 BATCH                                        
PB6      DS    CL1                                                              
PNAR     DS    CL42                TRANSACTION NARRATIVE                        
PB7      DS    CL1                                                              
PDEB     DS    CL13                DEBIT AMOUNT                                 
PB8      DS    CL1                                                              
PCRD     DS    CL13                CREDIT AMOUNT                                
PBR      DS    CL1                                                              
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPWL02 03/06/97'                                      
         END                                                                    
