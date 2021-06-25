*          DATA SET DDREPWKLST AT LEVEL 014 AS OF 05/25/18                      
*PHASE DDWKLSTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE CHOPPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE DMDMGRL                                                                
         TITLE 'WORKER LISTING'                                                 
         ENTRY UTL                                                              
         ENTRY SSB                                                              
DDWKLIST CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**WKLST,=V(REGSAVE),RA                                         
         L     RC,=A(WKC)                                                       
         USING WKC,RC                                                           
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
*                                                                               
         LR    RE,RB                SET FOR STXITER                             
         L     RF,STXITER                                                       
         STM   RE,RF,ADSTX                                                      
         OI    ADSTX+4,X'80'                                                    
*        GOTO1 STXITER,DMCB,ADSTX                                               
*                                                                               
         LA    R7,P                                                             
         USING PLD,R7                                                           
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         PACK  DUB(2),WORK+4(3)                                                 
         MVC   DAY,DUB                                                          
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS                                                *         
***********************************************************************         
                                                                                
WKL2     GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD                                                      
         BE    WKL11                                                            
         MVC   P(L'CARD),CARD                                                   
         GOTO1 PRINTER                                                          
*                                                                               
         SR    R1,R1               GET LENGTH OF DATA                           
         LA    R3,CARD                                                          
         LA    R0,L'CARD                                                        
WKL3     CLI   0(R3),C'='                                                       
         BE    WKL4                                                             
         CLI   0(R3),C' '                                                       
         BNH   WKL4                                                             
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,WKL3                                                          
         B     ERR1                BAD CARD                                     
*                                                                               
WKL4     BCTR  R1,0                ADJUST LENGTH                                
         LA    RE,CRDTAB                                                        
WKL5     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CARD(0),0(RE)       CARD VS. TABLE                               
         BE    WKL6                                                             
         LA    RE,L'CRDTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   WKL5                                                             
         B     ERR1                BAD CARD                                     
*                                                                               
WKL6     ICM   RF,15,10(RE)        RF=A(ROUTINE OR DATA)                        
         SR    R1,R1                                                            
         ICM   R1,1,10(RE)         R0=LENGTH OF DATA                            
         BNZ   WKL7                                                             
         BASR  R9,RF               GO TO VALIDATION ROUTINE                     
         B     WKL2                GET NEXT CARD                                
*                                                                               
WKL7     CLI   0(R3),C' '          TEST Y OR N                                  
         BNE   *+8                                                              
         LA    R3,=C'=Y'           DEFAULT IS Y                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(R3)                                                    
         B     WKL2                GET NEXT CARD                                
*                                                                               
WKL11    OC    ORIGINUM,ORIGINUM   TEST ORIGIN CARD                             
         BZ    ERR2                                                             
*                                                                               
WKL13    OC    PROGRAM,PROGRAM     TEST PROGRAM CARD                            
         BZ    ERR3                                                             
         B     WKINDX                                                           
         EJECT                                                                  
***********************************************************************         
* MAKE IT SO YOU TURN ON SWITCH TO MAKE SERVICE FILES READ-ONLY.      *         
***********************************************************************         
VWRSRV   CLI   CARD+6,C'N'          IS IT "NO", ALLOW UPDATE                    
         BNER  R9                                                               
         L     RE,=V(SSB)                                                       
         USING SSBD,RE                                                          
         OI    SSOMTIND,SSOWSRN                                                 
         DROP  RE                                                               
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATION ROUTINES                                                 *         
***********************************************************************         
VORIGIN  CLI   1(R3),X'F0'         TEST FOR ALPHA OR NUMBERIC                   
         BNL   *+12                                                             
         BAS   RE,GETID            GET ALPHA ID                                 
         B     WKL2                                                             
         LA    RF,1(R3)            GET NUMERIC ID                               
         LA    RE,50(RF)                                                        
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD+7(0)                                                    
         CVB   RF,DUB                                                           
         STCM  RF,3,ORIGINUM                                                    
         BR    R9                                                               
                                                                                
*                                  DAY=NN                                       
VDAY     PACK  DUB(2),CARD+4(3)                                                 
         MVC   DAY,DUB                                                          
         BR    R9                                                               
                                                                                
*                                  SUB=                                         
VSUB     CLC   =C'NUL',CARD+4                                                   
         BER   R9                                                               
         MVC   SUBPRG,CARD+4       MOVE IN SUB VALUE                            
         BR    R9                                                               
*                                  EXTRA= (USE FOR TWO CHAR SYSTEMS)            
VEXTRA   CLC   =C'NUL',CARD+6                                                   
         BER   R9                                                               
         MVC   EXTRA,CARD+6                                                     
         BR    R9                                                               
*                                  SEQUENCE=NNN                                 
VSEQNCE  LA    RF,CARD+9                                                        
         LA    RE,CARD+50                                                       
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD+9(0)                                                    
         CVB   RF,DUB                                                           
         STCM  RF,3,SEQUENCE                                                    
         BR    R9                                                               
                                                                                
*                                  DATE=MM/DD/YY                                
VDATE    GOTO1 DATVAL,DMCB,(0,CARD+5),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERR1                                                             
         MVC   SPECDATE(5),CARD                                                 
         MVC   SPECDATE+5(6),DATE                                               
         BR    R9                                                               
                                                                                
*                                  DSPACE=A/R/T/C/Q                             
         USING SSBD,RF                                                          
VDSPACE  L     RF,=V(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
         BR    R9                                                               
         DROP  RF                                                               
*                                  DDSIO=XXXXXXXX                               
VDDSIO   L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* WORKER FILE ROUTINES                                                *         
***********************************************************************         
                                                                                
WKINDX   LA    RF,ID                                                            
         USING UKRECD,RF                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,ORIGINUM    USER ID NUMBER                               
         MVC   UKSYSPRG,PROGRAM    SYSTEM/PROGRAM                               
         MVC   UKSUBPRG,SUBPRG     SUB PROGRAM                                  
         MVC   UKDAY,DAY           DAY NUMBER                                   
         MVC   UKCLASS,TYPE        CLASS                                        
         MVC   UKEXTRA,EXTRA       EXTRA                                        
         MVC   UKFILNO,SEQUENCE    FILE SEQ NUMBER WITHIN USER ID               
         DROP  RF                                                               
*                                                                               
         MVC   COMMAND,DMINDX                                                   
         BAS   RE,WRKR                                                          
         TM    8(R1),X'10'         TEST RNF                                     
         BO    ERR4                                                             
         CLI   8(R1),0                                                          
         BE    WKDELT                                                           
         DC    H'0'                DISK ERROR                                   
*                                                                               
WKDELT   CLI   DELETE,C'Y'         TEST ACTION DELETE                           
         BNE   WKPURG                                                           
         MVC   COMMAND,DMDEL                                                    
         BAS   RE,WRKR                                                          
         B     OKDELT                                                           
*                                                                               
WKPURG   CLI   PURGE,C'Y'          TEST ACTION PURGE                            
         BNE   WKKEEP                                                           
         MVC   COMMAND,DMPURG                                                   
         BAS   RE,WRKR                                                          
         B     OKPURG                                                           
*                                                                               
WKKEEP   CLI   KEEP,C'N'           TEST KEEP=N                                  
         BE    WKUNKP                                                           
         TM    ID+12,X'08'         TEST FILE ON KEEP                            
         BO    OKKEEP1                                                          
         MVC   COMMAND,DMKEEP      CHANGE TO KEEP                               
         BAS   RE,WRKR                                                          
         B     OKKEEP2                                                          
*                                                                               
WKUNKP   CLI   RESTORE,C'N'        TEST RESTORE=Y                               
         BE    PRNT                                                             
         MVC   COMMAND,DMUNKP      UNKEEP                                       
         BAS   RE,WRKR                                                          
         MVC   COMMAND,DMRSTR      AND RESTORE                                  
         BAS   RE,WRKR                                                          
         B     OKUNKP                                                           
         EJECT                                                                  
***********************************************************************         
* READ FILE AND PRINT                                                 *         
***********************************************************************         
                                                                                
PRNT     MVC   TITLE+17(14),=C'WORKER LISTING'                                  
         LA    R7,SUB1             SET HEADINGS                                 
         MVC   PLACC(L'ACC),ACC    ACCOUNT                                      
         MVC   PLCON(L'CON),CON    CONTRA                                       
         MVC   PLWRK(L'WRK),WRK    WORK CODES                                   
         MVC   PLDTE(L'DTE),DTE    DATE                                         
         MVC   PLREF(L'REF),REF    REFERENCE                                    
         MVC   PLBAT(L'BAT),BAT    BATCH                                        
         MVC   PLNAR(L'NAR),NAR    NARRATIVE                                    
         MVC   PLDR(L'DR),DR       DEBIT                                        
         MVC   PLCR(L'CR),CR       CREDIT                                       
*                                                                               
         LA    R7,SUB2             UNDELINE ALL                                 
         MVC   PLACC(L'ACC),DASH                                                
         MVC   PLCON(L'CON),DASH                                                
         MVC   PLWRK(L'WRK),DASH                                                
         MVC   PLDTE(L'DTE),DASH                                                
         MVC   PLREF(L'REF),DASH                                                
         MVC   PLBAT(L'BAT),DASH                                                
         MVC   PLNAR(L'NAR),DASH                                                
         MVC   PLDR(L'DR),DASH                                                  
         MVC   PLCR(L'CR),DASH                                                  
         LA    R7,P                                                             
                                                                                
         ZAP   PAGE,=P'1'                                                       
         MVC   LINE,=PL2'80'                                                    
*                                                                               
         MVC   MID1+0(4),=C'KEY'   PRINT THE KEY                                
         EDIT  (B2,ID),(4,MID1+5),FILL=0                                        
         MVC   MID1+9(4),ID+2      SYSTEM PROGRAM                               
         UNPK  MID1+13(3),ID+6(2)  DAY                                          
         MVC   MID1+15(1),ID+7     CLASS                                        
         MVC   MID1+16(1),ID+8     EXTRA                                        
         EDIT  (B2,ID+10),(4,MID1+17),FILL=0                                    
*                                                                               
PRNT3    MVC   COMMAND,DMREAD                                                   
         BAS   RE,WRKR                                                          
         TM    8(R1),X'80'         IS IT EOF?                                   
         BO    EOJ                                                              
*                                                                               
PRNT5    LA    RE,T-4                                                           
         LH    RF,0(RE)            SET X'00' BYTE AT END OF RECORD.             
         AR    RF,RE                                                            
         MVI   0(RF),0                                                          
         LA    R2,T                                                             
         CLI   T,PSSBELQ           TEST TRAILER ELEMENT X'52'                   
         BE    EOF                                                              
         AP    RECCOUNT,=P'1'      COUNT RECORDS                                
*                                                                               
         USING FFTELD,R2                                                        
         CLI   FFTEL,FFTELQ       TEST FREE FORM TEXT ELEMENT                   
         BNE   PRNT6                                                            
         CLI   FFTTYPE,FFTTWFSA   TEST WORKER FILE START ACCOUNT                
         BE    PRNT3              YES, SKIP RECORD                              
         DROP  R2                                                               
*                                                                               
PRNT6    CLI   0(R2),MPDKTYPQ      MEDIA TRANSFER RECORDS X'2F'                 
         BE    PRNT3                                                            
         CLI   0(R2),PSTKELQ       TRANSACTION KEY X'51'                        
         BE    TRANSKY                                                          
         CLI   0(R2),CRDELQ        CHECK REGISTER ELEMENT X'CD'                 
         BE    CKREG                                                            
         CLI   0(R2),PSHDELQ       POSTING HEADER X'50'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PSHEADD,R2                                                       
         MVC   PLACC,PSHDACC       ACCOUNT                                      
         MVC   PLCON,PSHDSBAC      CONTRA                                       
         MVC   PLWRK(2),PSHDANAL   CODES                                        
         AP    ITEMS,=P'1'                                                      
         SR    R3,R3                                                            
         IC    R3,PSHDLEN                                                       
         AR    R2,R3                                                            
*                                                                               
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        PROCESS TRANSACTION                          
         BNE   PRNT7                                                            
         BAS   RE,PTRN                                                          
*                                                                               
PRNT7    CLI   COUNT,C'Y'                                                       
         BNE   PRNT9                                                            
         EDIT  RECCOUNT,(6,PLBAL)                                               
*                                                                               
PRNT9    GOTO1 PRINTER                                                          
         CLI   OTHERS,C'Y'         TEST OTHER ELEMENT OPTION                    
         BNE   *+8                                                              
         BAS   RE,POTH                                                          
         CLI   TRACE,C'Y'          TEST TRACE OPTION                            
         BNE   *+8                                                              
         BAS   RE,TRAC                                                          
         CLI   REVERSE,C'Y'        TEST REVERSE OPTION                          
         BNE   *+8                                                              
         BAS   RE,REV                                                           
         B     PRNT3                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TRANSACTION ELEMENT DETAIL                                    *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
PTRN     NTR1  ,                                                                
         MVC   PLWRK+3(2),TRNANAL  WORKCODES                                    
         CLC   PLWRK(2),SPACES                                                  
         BE    PTRN3                                                            
         CLC   PLWRK+3(2),SPACES                                                
         BE    PTRN3                                                            
         MVI   PLWRK+2,C'/'                                                     
*                                                                               
PTRN3    GOTO1 DATCON,DMCB,(1,TRNDATE),(8,PLDTE)                                
         MVC   PLREF,TRNREF                                                     
         MVC   PLBAT,TRNBTCH                                                    
         SR    R3,R3                                                            
         IC    R3,TRNLN                                                         
         SH    R3,=H'29'                                                        
         BM    PTRN7                                                            
         CLC   TRNANAL,=C'99'                                                   
         BNE   *+8                                                              
         LA    R3,14                                                            
*                                                                               
PTRN5    MVC   WORK,SPACES                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRNNARR                                                  
         LA    R3,1(R3)                                                         
         LA    R0,L'PLNAR                                                       
         GOTO1 CHOPPER,DMCB,((R3),WORK),((R0),PLNAR),1                          
*                                                                               
PTRN7    TM    TRNSTAT,X'80'                                                    
         BO    PTRN9                                                            
         AP    CREDITS,TRNAMNT                                                  
         EDIT  (P6,TRNAMNT),(15,PLCR),2,MINUS=YES                               
         CLI   BALSW,C'Y'                                                       
         BNE   PTRNX                                                            
         CP    DEBITS,CREDITS                                                   
         BE    PTRNX                                                            
         ZAP   DUB,DEBITS                                                       
         SP    DUB,CREDITS                                                      
         EDIT  (P8,DUB),(15,PLBAL),2,MINUS=YES                                  
         B     PTRNX                                                            
*                                                                               
PTRN9    AP    DEBITS,TRNAMNT                                                   
         EDIT  (P6,TRNAMNT),(15,PLDR),2,MINUS=YES                               
         CLI   BALSW,C'Y'                                                       
         BNE   PTRNX                                                            
         CP    DEBITS,CREDITS                                                   
         BE    PTRNX                                                            
         ZAP   DUB,DEBITS                                                       
         SP    DUB,CREDITS                                                      
         EDIT  (P8,DUB),(15,PLBAL),2,MINUS=YES                                  
PTRNX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OTHER ELEMENTS                                                *         
***********************************************************************         
                                                                                
POTH     NTR1  ,                                                                
POTH1    SR    R3,R3               FIND OTHER ELEMENTS                          
         IC    R3,1(R2)                                                         
         LTR   R3,R3               TEST BAD LENGTH                              
         BZ    XIT                                                              
         AR    R2,R3                                                            
         CLI   0(R2),0             TEST EOF                                     
         BNE   POTH3                                                            
         GOTO1 PRINTER                                                          
         B     XIT                                                              
*                                                                               
POTH3    LA    RE,ELTAB                                                         
         CLC   0(1,RE),0(R2)       TABLE VS. ELEMENT                            
         BE    POTH5                                                            
         LA    RE,L'ELTAB(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   POTH3+L'POTH3                                                    
         B     POTH1                                                            
*                                                                               
POTH5    SR    RF,RF                                                            
         ICM   RF,7,1(RE)          SET ADDRESS OF ROUTINE                       
         BASR  R9,RF                                                            
         B     POTH1                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT OUT NAMELD ** NAME ELEMENT **                                           
**********************************************************************          
         USING NAMELD,R2                                                        
PNAM     SR    R3,R3               NAME ELEMENT                                 
         IC    R3,NAMLN                                                         
         SHI   R3,3                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+57(0),NAMEREC                                                  
         MVC   P+49(4),=C'NAME'                                                 
         GOTO1 PRINTER                                                          
         BR    R9                                                               
                                                                                
**********************************************************************          
* PRINT OUT ADRELD ** ADDRESS ELEMENT **                                        
**********************************************************************          
         USING ADRELD,R2                                                        
PADR     MVC   P+49(7),=C'ADDRESS' ADDRESS ELEMENT                              
         SR    R3,R3                                                            
         IC    R3,ADRNUM                                                        
         LA    R4,ADRADD1                                                       
*                                                                               
PADR3    MVC   P+57(26),0(R4)                                                   
         GOTO1 PRINTER                                                          
         LA    R4,26(R4)                                                        
         BCT   R3,PADR3                                                         
         BR    R9                                                               
                                                                                
**********************************************************************          
* PRINT OUT RSTELD ** RECORD STATUS ELEMENT **                                  
**********************************************************************          
         USING RSTELD,R2                                                        
PRST     MVC   P+49(6),=C'STATUS'  RECORD STATUS                                
         MVC   P+57(5),RSTANAL                                                  
         GOTO1 PRINTER                                                          
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT OUT XPYELD - ** EXTRA PAYMENT ELEMENT **                                
**********************************************************************          
         USING XPYELD,R2                                                        
PXPY     MVC   P+49(4),=C'CASH'                                                 
         CP    XPYCD,=P'0'                                                      
         BE    PXPY3                                                            
         MVC   P+57(9),=C'DISCOUNT='                                            
         EDIT  (P6,XPYCD),(12,P+66),2,ALIGN=LEFT                                
         GOTO1 PRINTER                                                          
*                                                                               
PXPY3    MVC   P+57(7),=C'CLIENT='                                              
         MVC   P+64(20),XPYCLI                                                  
         GOTO1 PRINTER                                                          
         MVC   P+57(8),=C'PRODUCT='                                             
         MVC   P+65(20),XPYPRO                                                  
         GOTO1 PRINTER                                                          
         MVC   P+57(8),=C'INVOICE='                                             
         MVC   P+65(14),XPYINV                                                  
         GOTO1 PRINTER                                                          
         MVC   P+57(7),=C'PERIOD='                                              
         MVC   P+64(17),XPYPER                                                  
         GOTO1 PRINTER                                                          
         CLI   XPYLN-XPYELD(R2),XPYLN2Q                                         
         BL    PXPYX                                                            
         MVC   P+57(13),=C'INVOICE DATE='                                       
         LA    RF,XPYDATE-XPYELD(R2)                                            
         OC    0(L'XPYDATE,RF),0(RF)                                            
         BZ    PXPYX                                                            
         GOTO1 DATCON,DMCB,(2,0(RF)),(8,P+70)                                   
         GOTO1 PRINTER                                                          
PXPYX    BR    R9                                                               
                                                                                
**********************************************************************          
* PRINT OUT SCIELD - ** SUBSIDIARY CASH INFO ELEMENT **                         
**********************************************************************          
         USING SCIELD,R2                                                        
PSCI     MVC   P+49(30),=C'SUBSIDIARY CASH,TYPE=!,AMOUNT='                      
         MVC   P+70(1),SCITYPE                                                  
         EDIT  (P6,SCIAMNT),(10,P+79),2,MINUS=YES,ALIGN=LEFT                    
         CLI   SCILN,X'0F'                                                      
         BL    PSCI3                                                            
         LR    R3,R0                                                            
         LA    R3,P+79(R3)                                                      
         MVI   1(R3),C'/'                                                       
         EDIT  (P6,SCIADMN),(10,3(R3)),2,MINUS=YES,ALIGN=LEFT                   
*                                                                               
PSCI3    GOTO1 PRINTER                                                          
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT MDTELD DATA - ** MEDIA TRANSFER ELEMENT **                              
**********************************************************************          
         USING MDTELD,R2                                                        
PMDT     LA    R3,P+49                                                          
         MVC   0(4,R3),=C'CLI='                                                 
         MVC   4(3,R3),MDTCLI                                                   
         LA    R3,8(R3)                                                         
         MVC   0(4,R3),=C'PRO='                                                 
         MVC   4(3,R3),MDTPRD                                                   
         LA    R3,8(R3)                                                         
         MVC   0(4,R3),=C'JOB='                                                 
         MVC   4(6,R3),MDTJOB                                                   
         GOTO1 PRINTER                                                          
*                                                                               
         LA    R4,MDTTAB           MEDIA INTERFACE                              
         LA    R3,P+49                                                          
PMDT3    CLI   0(R4),X'FF'                                                      
         BE    PMDT9               END-OF-TABLE                                 
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         LA    R1,4(R5)                                                         
         CLM   R1,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    PMDT7                                                            
         AR    R5,R2               R5=A(FIELD)                                  
         OC    0(4,R5),0(R5)                                                    
         BZ    PMDT7               DON'T DISPLAY ZERO                           
         LA    R1,P+118                                                         
         CR    R3,R1                                                            
         BL    PMDT5                                                            
         GOTO1 PRINTER                                                          
         LA    R3,P+49                                                          
*                                                                               
PMDT5    MVC   0(3,R3),1(R4)       DISPLAY TAG                                  
         EDIT  (4,0(R5)),(11,3(R3)),2,MINUS=YES,FLOAT==,ALIGN=LEFT              
         LR    R1,R0                                                            
         LA    R3,4(R1,R3)         BUMP TO NEXT PRINT LOCATION                  
*                                                                               
PMDT7    LA    R4,L'MDTTAB(R4)     BUMP TO NEXT FIELD IN TABLE                  
         B     PMDT3                                                            
PMDT9    GOTO1 PRINTER                                                          
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT MDPELD DATA - ** MEDIA TRANSFER (PACKED DATA)                           
**********************************************************************          
         USING MDPELD,R2                                                        
PMDP     LA    R4,MDPTAB           MEDIA INTERFACE                              
         LA    R3,P+49                                                          
PMDP3    CLI   0(R4),X'FF'                                                      
         BE    PMDP9               END-OF-TABLE                                 
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         LA    R1,4(R5)                                                         
         CLM   R1,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    PMDP7                                                            
         AR    R5,R2               R5=A(FIELD)                                  
         CP    0(6,R5),=P'0'                                                    
         BE    PMDP7               DON'T DISPLAY ZERO                           
         LA    R1,P+118                                                         
         CR    R3,R1                                                            
         BL    PMDP5                                                            
         GOTO1 PRINTER                                                          
         LA    R3,P+49                                                          
*                                                                               
PMDP5    MVC   0(3,R3),1(R4)       DISPLAY TAG                                  
         EDIT  (P6,0(R5)),(11,3(R3)),2,MINUS=YES,FLOAT==,ALIGN=LEFT             
         LR    R1,R0                                                            
         LA    R3,4(R1,R3)         BUMP TO NEXT PRINT LOCATION                  
*                                                                               
PMDP7    LA    R4,L'MDPTAB(R4)     BUMP TO NEXT FIELD IN TABLE                  
         B     PMDT3                                                            
PMDP9    GOTO1 PRINTER                                                          
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REVERSE TRANSACTION AND SUBSIDIARY ELEMENTS                         *         
***********************************************************************         
REV      NTR1  ,                                                                
         LA    R2,T                                                             
         CLI   0(R2),PSHDELQ       POSTING HEADER ELEMENT                       
         BNE   REV5                                                             
         CLI   1(R2),PSHEADL       MAKE SURE BY CHECKING THE LENGTH             
         BNE   REV5                                                             
REV3     SR    R3,R3                                                            
         IC    R3,1(R2)            SKIP HEADER                                  
         AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    REVX                                                             
*                                                                               
REV5     LA    RE,REVTAB                                                        
         CLC   0(1,RE),0(R2)       TABLE VS. ELEMENT                            
         BE    REV7                                                             
         LA    RE,L'ELTAB(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   REV5+L'REV5                                                      
         B     REV3                                                             
*                                                                               
REV7     SR    RF,RF                                                            
         ICM   RF,7,1(RE)          SET ADDRESS OF ROUTINE                       
         BASR  R9,RF                                                            
         B     REV3                                                             
*                                                                               
REVX     MVC   COMMAND,DMWRITE                                                  
         BAS   RE,WRKR                                                          
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT TRNELD DATA - ** TRANSACTION ELEMENT **                                 
**********************************************************************          
         USING TRNELD,R2                                                        
RTRN     ZAP   DUB,TRNAMNT         REVERSE TRANSACTION AMOUNTS                  
         MP    DUB,=P'-1'                                                       
         ZAP   TRNAMNT,DUB                                                      
         CLC   TRNANAL,=C'99'                                                   
         BNE   RTRNX                                                            
         LA    R6,3                                                             
         LA    R5,TRNNARR+15                                                    
RTRN3    ZAP   DUB,0(6,R5)         REVERSE COMM. AMOUNT                         
         MP    DUB,=P'-1'                  BILLED AMOUNT                        
         ZAP   0(6,R5),DUB                 PAYABLE AMOUNT                       
         LA    R5,6(R5)                                                         
         BCT   R6,RTRN3                                                         
RTRNX    BR    R9                                                               
                                                                                
**********************************************************************          
* PRINT XPYELD DATA - ** EXTRA PAYMENT ELEMENT **                               
**********************************************************************          
         USING XPYELD,R2                                                        
RXPY     ZAP   DUB,XPYCD           EXTRA PAYMENT CASH DISCOUNT                  
         MP    DUB,=P'-1'                                                       
         ZAP   XPYCD,DUB                                                        
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
         USING SCIELD,R2                                                        
RSCI     ZAP   DUB,SCIAMNT         SUBSIDIARY CASH                              
         MP    DUB,=P'-1'                                                       
         ZAP   SCIAMNT,DUB                                                      
         CLI   SCILN,SCILN2Q                                                    
         BL    RSCIX                                                            
         ZAP   DUB,SCIADMN                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SCIADMN,DUB                                                      
RSCIX    BR    R9                                                               
                                                                                
**********************************************************************          
* PRINT PBIELD DATA - ** PST BILLED ELEMENT **                                  
**********************************************************************          
         USING PBIELD,R2                                                        
RPBI     ZAP   DUB,PBIPST          PST BILLED ELEMENT                           
         MP    DUB,=P'-1'                                                       
         ZAP   PBIPST,DUB                                                       
         ZAP   DUB,PBIGROSS                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   PBIGROSS,DUB                                                     
         ZAP   DUB,PBICOMM                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   PBICOMM,DUB                                                      
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT PSSUBFD DATA -                                                          
**********************************************************************          
         USING PSSUBFD,R2                                                       
RPSSB    ZAP   DUB,PSSBCASH        SUB FILE TRAILER                             
         MP    DUB,=P'-1'                                                       
         ZAP   PSSBCASH,DUB                                                     
         BR    R9                                                               
**********************************************************************          
* PICTAB VS MDTTAB                                                              
**********************************************************************          
RPIC     LA    RF,PICTAB                                                        
         B     RMDT3                                                            
*                                                                               
RMDT     LA    RF,MDTTAB                                                        
RMDT3    CLI   0(RF),X'FF'                                                      
         BER   R9                  END-OF-TABLE                                 
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RE,4(R1)                                                         
         CLM   RE,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    RMDT5                                                            
         AR    R1,R2               R1=A(FIELD)                                  
         L     RE,0(R1)                                                         
         LCR   RE,RE               SET COMPLEMENT                               
         ST    RE,0(R1)                                                         
RMDT5    LA    RF,L'PICTAB(RF)     BUMP TO NEXT FIELD IN TABLE                  
         B     RMDT3                                                            
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* MDTTAB                                                                        
**********************************************************************          
RMDP     LA    RF,MDPTAB                                                        
RMDP3    CLI   0(RF),X'FF'                                                      
         BER   R9                  END-OF-TABLE                                 
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RE,4(R1)                                                         
         CLM   RE,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    RMDP5                                                            
         AR    R1,R2               R1=A(FIELD)                                  
         ZAP   DUB,0(6,R1)                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   0(6,R1),DUB                                                      
RMDP5    LA    RF,L'MDPTAB(RF)     BUMP TO NEXT FIELD IN TABLE                  
         B     RMDP3                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT OUT VBIELD - ** VAT BILLED ELEMENT **                                   
**********************************************************************          
         USING VBIELD,R2                                                        
RVBI     ZAP   DUB,VBIVAT          VAT AMOUNT BILLED                            
         MP    DUB,=P'-1'                                                       
         ZAP   VBIVAT,DUB                                                       
         ZAP   DUB,VBIGROSS        GROSS AMOUNT BILLED                          
         MP    DUB,=P'-1'                                                       
         ZAP   VBIGROSS,DUB                                                     
         ZAP   DUB,VBICOMM         COMMISSION AMOUNT BILLED                     
         MP    DUB,=P'-1'                                                       
         ZAP   VBICOMM,DUB                                                      
         BR    R9                                                               
         DROP  R2                                                               
                                                                                
**********************************************************************          
* PRINT OUT PTAELD - ** PROD TRANSACTION ACTIVITY **                            
**********************************************************************          
         USING PTAELD,R2                                                        
RPTA     ZAP   DUB,PTANET          NET AMOUNT IN AGENCY CURRENCY                
         MP    DUB,=P'-1'                                                       
         ZAP   PTANET,DUB                                                       
*                                                                               
         ZAP   DUB,PTAWUAMT        WRITE-UP AMOUNT                              
         MP    DUB,=P'-1'                                                       
         ZAP   PTAWUAMT,DUB                                                     
*                                                                               
         ZAP   DUB,PTACDSC         CASH DISCOUNT                                
         MP    DUB,=P'-1'                                                       
         ZAP   PTACDSC,DUB                                                      
*                                                                               
         LH    RE,PTAHOURS                                                      
         LCR   RE,RE               SET COMPLEMENT                               
         STH   RE,PTAHOURS                                                      
*                                                                               
         ZAP   DUB,PTARCOM         COMMISSION AMOUNT                            
         MP    DUB,=P'-1'                                                       
         ZAP   PTARCOM,DUB                                                      
*                                                                               
         OI    PTASTAT1,PTASREVS   MARK AS REVERSAL                             
         BR    R9                                                               
         DROP  R2                                                               
                                                                                
**********************************************************************          
* PRINT OUT INCELD - ** INTERNAL INCOME ELEMENT **                              
**********************************************************************          
         USING INCELD,R2                                                        
RINC     ZAP   DUB,INCAMNT         CURRENT BILLING LESS PREVIOUS                
         MP    DUB,=P'-1'                                                       
         ZAP   INCAMNT,DUB                                                      
         ZAP   DUB,INCINAM         INCOME AMOUNT                                
         MP    DUB,=P'-1'                                                       
         ZAP   INCINAM,DUB                                                      
         BR    R9                                                               
         DROP  R2                                                               
                                                                                
**********************************************************************          
* PRINT OUT FFTELD - ** FREEFORM TEXT ELEMENT **                                
**********************************************************************          
         USING FFTELD,R2                                                        
RFFT     CLI   FFTTYPE,FFTTWRKC                                                 
         BNER  R9                                                               
         ZAP   DUB,FFTWAMT         WORK AMOUNT                                  
         MP    DUB,=P'-1'                                                       
         ZAP   FFTWAMT,DUB                                                      
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSACTION KEYS                                                    *         
***********************************************************************         
         USING TRNRECD,R2                                                       
TRANSKY  LA    R2,2(R2)            POINT PAST TYPE/LEN                          
         MVC   P(8),=C'KEY=X''  '''                                             
         GOTO1 HEXOUT,DMCB,TRNKCPY,P+6,1                                        
         MVC   P+10(14),TRNKULA                                                 
         MVC   P+26(2),TRNKOFF                                                  
         MVC   P+31(5),=C'X''  '''                                              
         GOTO1 HEXOUT,DMCB,TRNKCCPY,P+33,1                                      
         MVC   P+38(14),TRNKULC                                                 
         MVC   P+53(9),=C'X''      '''                                          
         GOTO1 HEXOUT,DMCB,TRNKDATE,P+55,3                                      
         MVC   P+64(6),TRNKREF                                                  
         MVC   P+71(5),=C'X''  '''                                              
         GOTO1 HEXOUT,DMCB,TRNKSBR,P+73,1                                       
         GOTO1 PRINTER                                                          
         B     PRNT3                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK REGISTER RECORDS                                              *         
***********************************************************************         
         USING CRDELD,R2                                                        
CKREG    MVC   P(L'CRDDATA),CRDDATA                                             
         GOTO1 PRINTER                                                          
         B     PRNT3                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE FOR ID NUMBER                                     *         
***********************************************************************         
         USING UTLD,R3                                                          
GETID    NTR1  ,                                                                
         L     R3,=V(UTL)                                                       
         MVI   TSYS,10              CONTROL SYSTEM                              
         GOTO1 DATAMGR,DMCB,DMOPEN,CONTROL,NCTFILE                              
         LA    R5,T                                                             
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,CARD+7       ALPHA ID FROM CARD                           
         MVC   CTSAVE,CTIKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R5),(R5),(0,0)                       
         CLC   CTSAVE,CTIKEY                                                    
         BNE   ERR5                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
GETID2   CLI   0(R5),0                                                          
         BE    ERR5                                                             
         CLI   0(R5),X'02'                                                      
         BE    GETID4                                                           
*                                                                               
GETID3   IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     GETID2                                                           
*                                                                               
GETID4   MVC   ORIGINUM,2(R5)                                                   
         MVI   TSYS,1              BACK TO SERVICE                              
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CHARACTER AND HEX FORMAT                           *         
***********************************************************************         
TRAC     NTR1  ,                                                                
         LA    R4,T                R4=A(FIRST ELEMENT ON RECORD)                
         SR    R0,R0                                                            
TRAC3    CLI   0(R4),0             END OF RECORD?                               
         BE    XIT                                                              
*                                                                               
         LR    R5,R4                                                            
         SR    R3,R3                                                            
         IC    R3,1(R5)            R3=WIDTH OF DATA                             
*                                                                               
TRAC5    LR    R2,R3                                                            
         CH    R2,=Y(MAXWIDTH)                                                  
         BNH   *+8                                                              
         LH    R2,=Y(MAXWIDTH)     R2=WIDTH OF PRINTING THIS LINE               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+3(0),0(R5)       MOVE DATA TO PRINT LINE                       
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    P+3(0),OUTTAB      TRANSLATE TO PRINTABLE                        
         ZAP   DUB,LINE                                                         
         AP    DUB,=P'3'                                                        
         CP    DUB,MAXLINE                                                      
         BNH   *+10                                                             
         ZAP   LINE,MAXLINE                                                     
         GOTO1 PRINTER                                                          
         LA    R2,1(R2)                                                         
         GOTO1 HEXOUT,DMCB,(R5),HEXWORK,(R2),SEP                                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+3(0),HEXWORK     MOVE ZONES HALF TO PLINE                      
         GOTO1 PRINTER                                                          
         LA    RF,HEXWORK+1(R2)                                                 
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+3(0),0(RF)       MOVE NUMERIC HALF TO PLINE                    
         GOTO1 PRINTER                                                          
         LA    R2,1(R2)                                                         
         AR    R5,R2               POINT TO NEXT INPUT CHUNK                    
         SR    R3,R2               DECREMENT DATA WIDTH                         
         BP    TRAC5                                                            
         GOTO1 PRINTER                                                          
*                                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     TRAC3                                                            
         EJECT                                                                  
***********************************************************************         
* END OF FILE ROUTINE                                                 *         
***********************************************************************         
EOF      MVC   P,SPACES                                                         
         GOTO1 PRINTER                                                          
         MVC   P+64(5),=C'ITEMS'                                                
         MVC   P+39(13),=C'DETAIL TOTALS'                                       
         EDIT  (P6,ITEMS),(10,P+53)                                             
         EDIT  (P8,DEBITS),(15,PLDR),2,MINUS=YES                                
         GOTO1 PRINTER                                                          
         EDIT  (P8,CREDITS),(15,PLCR),2,MINUS=YES                               
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         USING PSSUBFD,R2                                                       
         MVC   P+17(15),PSSBDESC                                                
         MVC   P+39(14),=C'SUMMARY TOTALS'                                      
         EDIT  (P6,PSSBRECS),(10,P+53)                                          
         MVC   P+64(5),=C'ITEMS'                                                
         EDIT  (P6,PSSBCASH),(15,PLDR),2,MINUS=YES                              
         MVC   PLCR(L'PLCR),PLDR                                                
         GOTO1 PRINTER                                                          
         CLI   REVERSE,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,REV                                                           
*                                                                               
         OC    SEQUENCE,SEQUENCE                                                
         BNZ   EOJ                                                              
*                                  FOR A GENERAL READ - SEE IF                  
*                                  WE CAN FIND ANOTHER TO PRINT                 
*                                                                               
EOF4     MVC   COMMAND,DMINDX      LOCATE ANOTHER INDEX RECORD                  
         BAS   RE,WRKR                                                          
         TM    8(R1),X'80'                                                      
         BNZ   EOJ                                                              
         CLC   ID(8),ORIGINUM      MUST BE SAME FILE ID                         
         BNE   EOF4                                                             
         ZAP   DEBITS,=P'0'        SET FOR NEXT FILE                            
         ZAP   CREDITS,=P'0'                                                    
         ZAP   RECCOUNT,=P'0'                                                   
         ZAP   ITEMS,=P'0'                                                      
         B     PRNT                PROCESS THE NEXT                             
         EJECT                                                                  
***********************************************************************         
* WORKER FILE INTERFACE                                               *         
***********************************************************************         
                                                                                
WRKR     LR    R0,RE                                                            
         LA    RF,T-4                                                           
         GOTO1 WORKER,DMCB,COMMAND,ADBUFF,ID,(RF)                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MESSAGES                                                     *         
***********************************************************************         
                                                                                
OKDELT   MVC   P(22),=C'ENTRY HAS BEEN DELETED'                                 
         B     ALLX                                                             
*                                                                               
OKPURG   MVC   P(21),=C'ENTRY HAS BEEN PURGED'                                  
         B     ALLX                                                             
*                                                                               
OKKEEP1  MVC   P(24),=C'KEEP STATUS IS UNCHANGED'                               
         B     ALLX                                                             
*                                                                               
OKKEEP2  MVC   P(28),=C'ENTRY STATUS CHANGED TO KEEP'                           
         B     ALLX                                                             
*                                                                               
OKUNKP   MVC   P(14),=C'ENTRY RESTORED'                                         
         B     ALLX                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
                                                                                
ERR1     MVC   P(8),=C'BAD CARD'                                                
         B     ALLX                                                             
*                                                                               
ERR2     MVC   P(19),=C'MISSING ORIGIN CARD'                                    
         B     ALLX                                                             
*                                                                               
ERR3     MVC   P(20),=C'MISSING PROGRAM CARD'                                   
         B     ALLX                                                             
*                                                                               
ERR4     MVC   P(16),=C'NO SUCH ID ,KEY='                                       
         GOTO1 HEXOUT,DMCB,ID,P+16,16                                           
         B     ALLX                                                             
*                                                                               
ERR5     MVC   P(8),=C'BAD CARD'                                                
         B     ALLX                                                             
*                                                                               
ALLX     GOTO1 PRINTER                                                          
*                                                                               
EOJ      GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XBASE                                                                  
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
WKC      CSECT                                                                  
ADBUFF   DC    A(POSTBUFF)                                                      
*                                                                               
CARDS    DC    V(CARDS)                                                         
CHOPPER  DC    V(CHOPPER)                                                       
CPRINT   DC    V(CPRINT)                                                        
DATAMGR  DC    V(DATAMGR)                                                       
DATCON   DC    V(DATCON)                                                        
DATVAL   DC    V(DATVAL)                                                        
HEXOUT   DC    V(HEXOUT)                                                        
PDUMPER  DC    V(PDUMPER)                                                       
PRINT    DC    V(PRINT)                                                         
PRINTER  DC    V(PRINTER)                                                       
WORKER   DC    V(WORKER)                                                        
STXITER  DC    V(STXITER)                                                       
*                                                                               
BALSW    DC    C'N'                                                             
DELETE   DC    C'N'                                                             
PURGE    DC    C'N'                                                             
REVERSE  DC    C'N'                                                             
KEEP     DC    C'N'                                                             
RESTORE  DC    C'N'                                                             
OTHERS   DC    C'N'                                                             
COUNT    DC    C'N'                                                             
TRACE    DC    C'N'                                                             
*                                                                               
ID       DC    XL16'00'                                                         
ORIGINUM DC    XL2'00'                                                          
PROGRAM  DC    XL3'00'                                                          
SUBPRG   DC    X'00'                                                            
DAY      DC    X'00'                                                            
TYPE     DC    C'P'                                                             
EXTRA    DC    X'00'                                                            
SEQUENCE DC    XL2'00'                                                          
*                                                                               
*                                                                               
CONTROL  DC    C'CONTROL '                                                      
NCTFILE  DC    C'NCTFILE X'                                                     
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
COMMAND  DS    CL8                                                              
DMINDX   DC    CL8'INDEX'                                                       
DMDEL    DC    CL8'DELETE'                                                      
DMPURG   DC    CL8'PURGE'                                                       
DMKEEP   DC    CL8'KEEP'                                                        
DMUNKP   DC    CL8'UNKEEP'                                                      
DMRSTR   DC    CL8'RESTORE'                                                     
DMREAD   DC    CL8'READ'                                                        
DMWRITE  DC    CL8'WRITE'                                                       
DMRDHI   DC    CL8'DMRDHI'                                                      
DMOPEN   DC    CL8'OPEN'                                                        
*                                                                               
DEBITS   DC    PL8'0'                                                           
CREDITS  DC    PL8'0'                                                           
ITEMS    DC    PL6'0'                                                           
RECCOUNT DC    PL4'0'                                                           
*                                                                               
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    CL255                                                            
CARD     DS    CL80                                                             
*                                                                               
CTSAVE   DS    CL25                                                             
DATE     DS    CL6                                                              
MAXWIDTH EQU   120                 PRINT WIDTH                                  
HEXWORK  DS    CL(MAXWIDTH*2)      DUMMY PRINT LINE                             
*                                                                               
ACC      DC    C'ACCOUNT'                                                       
CON      DC    C'CONTRA-ACCOUNT'                                                
WRK      DC    C'CODES'                                                         
DTE      DC    C'DATE'                                                          
REF      DC    C'REF.'                                                          
BAT      DC    C'BATCH'                                                         
NAR      DC    C'NARRATIVE'                                                     
DR       DC    C'       DEBITS'                                                 
CR       DC    C'       CREDITS'                                                
DASH     DC    20C'-'                                                           
*                                                                               
SEP      DC    C'SEP'              HEXOUT PARAMETER                             
OUTTAB   DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-DF                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
*                                                                               
CRDTAB   DS    0XL14                                                            
         DC    CL10'DSPACE=   ',AL4(VDSPACE)                                    
         DC    CL10'DDSIO=    ',AL4(VDDSIO)                                     
         DC    CL10'WRSRV=    ',AL4(VWRSRV)                                     
         DC    CL10'WRITE=    ',AL4(VWRSRV)                                     
         DC    CL10'ORIGIN=   ',AL4(VORIGIN)                                    
         DC    CL10'SEQUENCE= ',AL4(VSEQNCE)                                    
         DC    CL10'DAY=      ',AL4(VDAY)                                       
         DC    CL10'DATE=     ',AL4(VDATE)                                      
         DC    CL10'PROG=     ',AL1(L'PROGRAM),AL3(PROGRAM)                     
         DC    CL10'SUB=      ',AL4(VSUB)                                       
         DC    CL10'TYPE=     ',AL1(L'TYPE),AL3(TYPE)                           
         DC    CL10'EXTRA=    ',AL4(VEXTRA)                                     
         DC    CL10'BALANCE=  ',AL1(L'BALSW),AL3(BALSW)                         
         DC    CL10'DELETE=   ',AL1(L'DELETE),AL3(DELETE)                       
         DC    CL10'PURGE=    ',AL1(L'PURGE),AL3(PURGE)                         
         DC    CL10'REVERSE=  ',AL1(L'REVERSE),AL3(REVERSE)                     
         DC    CL10'KEEP=     ',AL1(L'KEEP),AL3(KEEP)                           
         DC    CL10'RESTORE=  ',AL1(L'RESTORE),AL3(RESTORE)                     
         DC    CL10'OTHERS=   ',AL1(L'OTHERS),AL3(OTHERS)                       
         DC    CL10'COUNT=    ',AL1(L'COUNT),AL3(COUNT)                         
         DC    CL10'TRACE=    ',AL1(L'TRACE),AL3(TRACE)                         
         DC    X'FF'                                                            
*                                                                               
ELTAB    DS    0XL4                OTHER ELEMENTS                               
         DC    AL1(NAMELQ),AL3(PNAM)                                            
         DC    AL1(ADRELQ),AL3(PADR)                                            
         DC    AL1(RSTELQ),AL3(PRST)                                            
         DC    AL1(XPYELQ),AL3(PXPY)                                            
         DC    AL1(SCIELQ),AL3(PSCI)                                            
         DC    AL1(MDTELQ),AL3(PMDT)                                            
         DC    AL1(MDPELQ),AL3(PMDP)                                            
         DC    X'FF'                                                            
*                                                                               
REVTAB   DS    0XL4                REVERSE ELEMENTS                             
         DC    AL1(TRNELQ),AL3(RTRN)                                            
         DC    AL1(XPYELQ),AL3(RXPY)                                            
         DC    AL1(SCIELQ),AL3(RSCI)                                            
         DC    AL1(PBIELQ),AL3(RPBI)                                            
         DC    AL1(PSSBELQ),AL3(RPSSB)                                          
         DC    AL1(MDTELQ),AL3(RMDT)                                            
         DC    AL1(MDPELQ),AL3(RMDP)                                            
         DC    AL1(VBIELQ),AL3(RVBI)                                            
         DC    AL1(PTAELQ),AL3(RPTA)                                            
         DC    AL1(INCELQ),AL3(RINC)                                            
         DC    AL1(FFTELQ),AL3(RFFT)                                            
         DC    X'9E',AL3(RPIC)                                                  
         DC    X'FF'                                                            
*                                                                               
*                                  TABLES FOR BINARY AMOUNT REVERSALS           
PICTAB   DS    0XL4                                                             
         DC    AL1(PICAFEES-PICATD),C'FEE'                                      
         DC    AL1(PICAPNW-PICATD),C'P&&W'                                      
         DC    AL1(PICAHNW-PICATD),C'H&&W'                                      
         DC    AL1(PICATNH-PICATD),C'T&&H'                                      
         DC    AL1(PICACR-PICATD),C'CRD'                                        
         DC    X'FF'                                                            
*                                                                               
MDTTAB   DS    0XL4                                                             
         DC    AL1(MDTGRS-MDTELD),C'GRS'                                        
         DC    AL1(MDTNET-MDTELD),C'NET'                                        
         DC    AL1(MDTCOM-MDTELD),C'COM'                                        
         DC    AL1(MDTCD-MDTELD),C'CD '                                         
         DC    AL1(MDTINTL-MDTELD),C'INT'                                       
         DC    AL1(MDTRECV-MDTELD),C'RCV'                                       
         DC    AL1(MDTVAT-MDTELD),C'VAT'                                        
         DC    X'FF'                                                            
*                                                                               
MDPTAB   DS    0XL4                                                             
         DC    AL1(MDPGRS-MDPELD),C'GRS'                                        
         DC    AL1(MDPNET-MDPELD),C'NET'                                        
         DC    AL1(MDPCOM-MDPELD),C'COM'                                        
         DC    AL1(MDPCD-MDPELD),C'CD '                                         
         DC    AL1(MDPINTL-MDPELD),C'INT'                                       
         DC    AL1(MDPRECV-MDPELD),C'RCV'                                       
         DC    AL1(MDPVAT-MDPELD),C'VAT'                                        
         DC    X'FF'                                                            
*                                                                               
EDMSK    DC    X'40202020202020202020202020214B202060'                          
*                                                                               
ADSTX    DS    2F                  FOR STXITER                                  
*                                                                               
         DS    F                                                                
T        DS    CL1200                                                           
*                                                                               
POSTBUFF DC    4640X'00'                                                        
*                                                                               
         DS    0F                                                               
         DC    CL16'*UTL*UTL*UTL**UTL'                                          
UTL      DC    F'0',AL1(01),XL250'00'                                           
*                                                                               
         DS    0F                                                               
         DC    CL16'*SSB*SSB*SSB**SSB'                                          
SSB      DC    H'0',X'FF',X'00',1020X'00'                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR THE PRINT LINE                                            *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
PLACC    DS    CL15                                                             
         DS    CL1                                                              
PLCON    DS    CL15                                                             
         DS    CL1                                                              
PLWRK    DS    CL5                                                              
         DS    CL1                                                              
PLDTE    DS    CL8                                                              
         DS    CL1                                                              
PLREF    DS    CL6                                                              
         DS    CL1                                                              
PLBAT    DS    CL6                                                              
         DS    CL1                                                              
PLNAR    DS    CL23                                                             
         DS    CL1                                                              
PLDR     DS    CL15                                                             
         DS    CL1                                                              
PLCR     DS    CL15                                                             
         DS    CL1                                                              
PLBAL    DS    CL15                                                             
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PROD INTRFACE AMOUNTS BY CATEGORY ELEMENT            *         
***********************************************************************         
                                                                                
PICATD   DSECT                                                                  
PICAEL   DS    CL1       B         ELEMENT CODE X'9E'                           
PICALEN  DS    CL1       B         ELEMENT LENGTH X'19'                         
PICACAT  DS    CL1       B         CATEGORY EQUATE                              
PICANUM  DS    CL1       B         NUMBER OF PEOPLE IN CATEGORY                 
         DS    CL1                 SPARE                                        
PICAFEES DS    CL4       B         FEES                                         
PICAPNW  DS    CL4       B         P&W AMOUNT                                   
PICAHNW  DS    CL4       B         H&W AMOUNT                                   
PICATNH  DS    CL4       B         T&H AMOUNT                                   
PICACR   DS    CL4       B         CREDITS                                      
PICALNQ  EQU   *-PICATD                                                         
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCNTRL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG SSBD                                                               
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DDREPWKLST05/25/18'                                      
         END                                                                    
