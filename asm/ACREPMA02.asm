*          DATA SET ACREPMA02  AT LEVEL 043 AS OF 03/09/17                      
*PHASE ACMA02A                                                                  
*INCLUDE PQPROF                                                                 
*INCLUDE GETLOGO                                                                
ACMA02   TITLE '- MARKER TURNAROUND REPORT(S)'                                  
ACMA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACMA**,R9                                                    
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         USING UKRECD,WRKID        WORKER FILE INDEX                            
*                                                                               
         CLI   MODE,PROCRCVR       PROCESS RECOVERY FILE RECORD                 
         BE    PROC                                                             
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    FRST                                                             
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    LAST                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST ROUTINE                                                   *         
*                                                                     *         
* BUILD A TABLE OF COMPANY CODES FOR THIS ACCOUNT FILE (SEE CPYTABD)  *         
***********************************************************************         
         SPACE 1                                                                
FRST     LH    R2,=Y(IO1-WORKD)                                                 
         LA    R2,WORKD(R2)                                                     
         ST    R2,AIO1                                                          
         LH    R2,=Y(IO2-WORKD)                                                 
         LA    R2,WORKD(R2)                                                     
         ST    R2,AIO2                                                          
         LH    R2,=Y(IO3-WORKD)                                                 
         LA    R2,WORKD(R2)                                                     
         ST    R2,AIO3                                                          
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         MVC   CTRY,MCCTRY                                                      
         MVC   AUTL,MCUTL          GET ADDRESS OF UTL                           
         DROP  R2                                                               
*                                                                               
         L     R2,AIO1             BUILD KEY OF FIRST COMPANY                   
         USING CPYRECD,R2                                                       
         L     R3,ACPYTAB                                                       
         USING CPYTABD,R3          R3=A(COMPANY TABLE)                          
         LA    R4,X'40'                                                         
*                                                                               
FRST2    MVC   CPYKEY,SPACES                                                    
         LA    R4,1(R4)                                                         
         STC   R4,CPYKCPY                                                       
         CLI   CPYKCPY,X'FF'                                                    
         BE    FRST20                                                           
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,CPYRECD,CPYRECD                       
         BNE   FRST2                                                            
*                                                                               
         XC    SAVPROD,SAVPROD                                                  
         LA    R1,CPYRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING CPYELD,R1                                                        
FRST4    CLI   CPYEL,0             TEST E-O-R                                   
         BE    FRST14                                                           
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BNE   FRST6                                                            
         MVC   CPYTCPY,CPYKCPY                                                  
         MVC   CPYTUID,CPYUID                                                   
         MVC   CPYTSTA1,CPYSTAT1                                                
         MVC   CPYTSTA2,CPYSTAT2                                                
         MVC   CPYTSTA3,CPYSTAT3                                                
         MVC   CPYTSTA4,CPYSTAT4                                                
         MVC   CPYTALPH,CPYALPHA                                                
         MVC   CPYTLOGO,CPYLOGO                                                 
         MVC   SAVPROD,CPYPROD     SAVE PROD U/L FOR LEDGER READ                
         CLI   CPYLN,CPYLN2Q                                                    
         BL    FRST12                                                           
         MVC   CPYTSTA5,CPYSTAT5                                                
         MVC   CPYTSTA6,CPYSTAT6                                                
         MVC   CPYTSTA7,CPYSTAT7                                                
         MVC   CPYTSTA8,CPYSTAT8                                                
         TM    CPYTSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    *+8                                                              
         OI    RCFLAG1,RCFBCURT    TELL MONACC TO BUILD CURRENCY TABLE          
         B     FRST12                                                           
*                                                                               
         USING NAMELD,R1                                                        
FRST6    CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   FRST8                                                            
         SR    RE,RE                                                            
         ICM   RE,1,NAMLN                                                       
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   CPYTNAME,SPACES                                                  
         MVC   CPYTNAME(0),NAMEREC                                              
         EX    RE,*-6                                                           
         B     FRST12                                                           
*                                                                               
         USING ADRELD,R1                                                        
FRST8    CLI   ADREL,ADRELQ        TEST ADDRESS ELEMENT                         
         BNE   FRST10                                                           
         MVC   CPYTADDR,ADRADD1                                                 
         B     FRST12                                                           
FRST10   DS    0H                                                               
*                                                                               
FRST12   IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     FRST4                                                            
*                                  READ PRODUCTION LEDGER FOR LENGTHS           
         USING LDGRECD,R2                                                       
FRST14   MVC   LDGKUNT(L'SAVPROD),SAVPROD                                       
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,LDGRECD,LDGRECD                       
         BNE   FRST18              NOT FOUND                                    
         LA    R1,LDGRECD+ACCORFST                                              
         USING ACLELD,R1                                                        
FRST16   CLI   ACLEL,0                                                          
         BE    FRST18              NO ELEMENT                                   
         CLI   ACLEL,ACLELQ        FIND LENGTHS ELEMENT                         
         BE    *+14                                                             
         IC    R0,ACLLN                                                         
         AR    R1,R0                                                            
         B     FRST16                                                           
         SR    RF,RF                                                            
         MVC   CPYPRDAL,ACLVALS                                                 
         IC    R0,ACLVALS+L'ACLVALS                                             
         IC    RF,ACLVALS+(L'ACLVALS*2)                                         
         SR    RF,R0               (CLI+PRD+JOB)-(CLI+PRD)                      
         STC   RF,CPYPRDCL                                                      
         IC    RF,ACLVALS                                                       
         SR    R0,RF               (CLI+PRD)-(CLI)                              
         STC   R0,CPYPRDBL                                                      
*                                                                               
FRST18   DS    0H                                                               
         MVC   CPYTCTRY,CTRY       DEFAULT IS THE DDS COUNTRY CODE              
         MVC   CPYSALPH,CPYTALPH   DEFAULT SECURITY-ID IS ALPHA-ID              
                                                                                
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY       READ ACCESS RECORD TO ESTABLISH              
         MVI   CT5KTYP,CT5KTYPQ    AGENCY COUNTRY CODE                          
         MVC   CT5KALPH,CPYTALPH                                                
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CT5REC,CT5REC                         
         BNE   FRST19                                                           
         LA    R1,CT5DATA                                                       
         USING CTAGDD,R1                                                        
         SR    R0,R0                                                            
                                                                                
FRST18C  CLI   CTAGDEL,0                                                        
         BE    FRST19                                                           
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    FRST18E                                                          
         CLI   CTAGDEL,CTSEAELQ                                                 
         BE    FRST18F                                                          
                                                                                
FRST18D  IC    R0,CTAGDLEN                                                      
         AR    R1,R0                                                            
         B     FRST18C                                                          
                                                                                
FRST18E  CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    FRST18D                                                          
         MVC   CPYTCTRY,CTAGDCTY   SET AGENCY COUNTRY CODE                      
         B     FRST18D                                                          
         DROP  R1                                                               
*                                                                               
         USING CTSEAD,R1                                                        
FRST18F  MVC   CPYSALPH,CTSEAAID   SAVE SECURITY ID IF FOUND                    
         B     FRST18D                                                          
         DROP  R1                                                               
*                                                                               
FRST19   LA    R3,CPYTABL(R3)      BUMP TABLE                                   
         B     FRST2                                                            
*                                                                               
FRST20   GOTO1 ADDICTAT,DMCB,C'LL  ',DICI,DICO                                  
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         LA    R0,HEADS                                                         
         ST    R0,HEADHOOK                                                      
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECOVERY FILE RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
PROC     MVI   CHQTAB,CHQTEOTQ     CLEAR CHEQUE TABLE                           
         MVI   CHQTABN,0           SET NUMBER OF CHEQUES PROCESSED              
         L     R2,ADTRANS                                                       
         USING RCVRECD,R2          R2=A(RECOVERY HEADER)                        
         CLI   RCVPRGNO,RCVPMRKQ   TEST MARKER PROGRAM                          
         BNE   EXIT                                                             
         CLI   RCVRECTY,RCVRCPYQ   TEST COPY OR CHANGE                          
         BE    *+12                                                             
         CLI   RCVRECTY,RCVRCHAQ                                                
         BNE   EXIT                                                             
         CLI   RCVFILTY,RCVFAMST   TEST ACCMST RECORD                           
         BE    PROC2                                                            
         CLI   RCVFILTY,RCVFAACC   TEST ACCFIL RECORD                           
         BNE   EXIT                                                             
         GOTO1 VACCEMU,DMCB,EMUOLDN,,,RCVRECRD                                  
         ORG   *-2                                                              
         LR    R2,R1               (EMU USES R2 FOR PLIST ADDRESS)              
         O     R2,=X'FF000000'                                                  
         BASR  RE,RF                                                            
         L     R2,ADTRANS                                                       
*                                                                               
PROC2    LA    R3,RCVRECRD                                                      
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4           R4=A(TRANSACTION ELEMENT)                    
         CLI   TRNEL,TRNELQ        TEST TRANSACTION ELEMENT                     
         BNE   EXIT                                                             
         CLI   RCVRECTY,RCVRCPYQ   SAVE COPY RECORD FOR NEXT MODE               
         BNE   PROC4                                                            
         L     R0,AIO2                                                          
         SR    R1,R1                                                            
         ICM   R1,3,TRNRLEN                                                     
         LA    RE,TRNKEY                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
*                                                                               
PROC4    L     R1,AIO2                                                          
         CLC   TRNKEY,TRNKEY-TRNRECD(R1)                                        
         BNE   EXIT                                                             
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         SR    R0,R0                                                            
         USING TRSEL,R1                                                         
PROC6    IC    R0,TRSLN                                                         
         AR    R1,R0                                                            
         CLI   TRSEL,0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSEL,TRSELQ                                                     
         BNE   PROC6                                                            
         MVI   WORK,0                                                           
         CLI   TRSLN,TRSLNQ                                                     
         BL    *+10                                                             
         MVC   WORK(L'TRSMARK),TRSMARK                                          
*                                                                               
         LA    R1,TRNELD                                                        
PROC8    IC    R0,TRSLN                                                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSEL,TRSELQ                                                     
         BNE   PROC8                                                            
         CLI   TRSLN,TRSLNQ                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSMARK,WORK        TEST MARKER ACTION CHANGED                   
         BE    EXIT                                                             
*                                                                               
         LR    R6,R1               SAVE ADDRESS IN R1 TO R6                     
         LA    R0,SREC                                                          
         SR    R1,R1                                                            
         LA    R1,SRECL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SREC                                   
         LR    R1,R6               RESET R1                                     
                                                                                
         MVC   SKSEQ,RCVSEQNO+1    SET RECORD SEQUENCE NUMBER                   
         MVC   SKCPY,TRNKCPY       SET COMPANY CODE                             
         MVC   SKUID,RCVUSRID      SET USER-ID                                  
         MVC   SKMIN,TRSMARK       SET MARKER ACTION NUMBER                     
         MVC   SDSDT,TRSBSTDT      SET BANK STATEMENT DATE                      
         MVI   SDMRK,C' '                                                       
         MVC   SDPID,RCVTERM                                                    
         TM    SKMIN,TRSMUMQ                                                    
         BZ    PROC9                                                            
         NI    SKMIN,255-TRSMUMQ                                                
         MVI   SDMRK,C'*'          INDICATE NEGATIVE ACTION                     
         L     R3,AIO2             POINT TO COPY RECORD FOR UN(ACTION)          
         LA    R4,TRNRFST                                                       
*                                                                               
PROC9    L     R1,ACPYTAB                                                       
         USING CPYTABD,R1          LOCATE COMPANY TABLE ENTRY                   
PROC10   CLI   CPYTABD,CPYTEOTQ    TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                DIDN'T FIND A COMPANY RECORD                 
         CLC   TRNKCPY,CPYTCPY                                                  
         BE    *+12                                                             
         LA    R1,CPYTABL(R1)                                                   
         B     PROC10                                                           
         ST    R1,ACPYNTRY                                                      
         CLC   CPYTUID,SKUID       TEST SORT REC IS FOR PRINCIPAL ID            
         BE    *+8                                                              
         MVI   CPYTINDS,CPYTIMUQ   SET MIXED USER-IDS ON THIS COMPANY           
         MVC   SDALPH,CPYTALPH                                                  
         MVC   SDSALPH,CPYSALPH                                                 
*                                                                               
         L     R1,AACTTAB                                                       
         USING ACTTABD,R1          LOCATE MARKER ACTION                         
PROC12   CLI   ACTTABD,ACTTEOTQ                                                 
         BE    EXIT                                                             
         CLC   ACTTACT,SKMIN                                                    
         BE    *+12                                                             
         LA    R1,ACTTABL(R1)                                                   
         B     PROC12                                                           
         MVC   SKMAJ,ACTTREP       SET REPORT NUMBER                            
         MVC   SKACT,TRNKUNT       SET ACCOUNT CODE                             
*                                                                               
         MVC   SDREF,TRNREF        SET TRANSACTION VALUES                       
         MVC   SDDDT,TRNRSDUE                                                   
         MVC   SDDAT,TRNDATE                                                    
         MVC   SDACT,TRNKUNT                                                    
         MVC   SDSTA,TRNSTAT                                                    
         MVC   SDCAC,TRNKCUNT                                                   
         MVC   SDWRK,TRNANAL                                                    
         ZAP   SDAMT,TRNAMNT                                                    
         MVC   SDBRF,TRNMOS        MOS+BREF                                     
         MVC   SAVBTYP,TRNTYPE                                                  
         MVI   SDNAR,C' '                                                       
         MVC   SDNAR+1(L'SDNAR-1),SDNAR                                         
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SHI   RE,TRNLN1Q+1                                                     
         CH    RE,=H'0'                                                         
         BL    PROC13                                                           
         SR    RF,RF                                                            
         LA    RF,L'SDNAR                                                       
         CR    RE,RF                                                            
         BNH   PROC12A                                                          
         LR    RE,RF                                                            
                                                                                
PROC12A  MVC   SDNAR(0),TRNNARR                                                 
         EX    RE,*-6                                                           
                                                                                
PROC13   LA    R1,TRNELD                                                        
         SR    R0,R0                                                            
*                                                                               
         USING OTHELD,R1                                                        
PROC14   IC    R0,OTHLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   OTHEL,0             TEST E-O-R                                   
         BE    PROC80                                                           
*                                                                               
         USING GDAELD,R1                                                        
         CLI   GDAEL,GDAELQ                                                     
         BNE   PROC16                                                           
         CLI   GDATYPE,GDATRECN                                                 
         BNE   PROC14                                                           
         MVC   SRECDT,GDADATE                                                   
         MVC   SCLRDT,GDADATE2                                                  
         B     PROC14                                                           
*                                                                               
*&&UK                                                                           
         CLI   OTHEL,OTHELQ        TEST OTHERS ELEMENT                          
         BNE   PROC16                                                           
         MVC   SDSUB,OTHNUM                                                     
         B     PROC14                                                           
*&&                                                                             
         USING MPYELD,R1                                                        
PROC16   CLI   MPYEL,MPYELQ        TEST MANUAL PAYMENT ELEMENT                  
         BNE   PROC20                                                           
         MVC   SDCHQ,MPYNO                                                      
         MVC   SDCDT,MPYDTE                                                     
         MVC   SDCBA,MPYBNK                                                     
         CLC   MPYBNK,TRNKCUNT     TEST MAN. CHEQUE: CONTRA U/L=BANK            
         BNE   PROC18                                                           
         CP    TRNAMNT,PZERO                         AMOUNT -CR                 
         BNL   PROC18                                                           
         MVC   SDREF,SPACES        YES - CLEAR REFERENCE NUMBER                 
PROC18   CLC   MPYNO,SPACES        IGNORE IF CHEQUE NUMBER SPACES               
         BE    PROC14                                                           
         LA    RF,CHQTAB                                                        
         USING CHQTABD,RF          RF=A(MANUAL CHEQUE TABLE)                    
         LA    RE,CHQMAXN                                                       
         CLI   CHQTABD,CHQTEOTQ    LOCATE A FREE ENTRY                          
         BE    *+14                                                             
         LA    RF,CHQTABL(RF)                                                   
         BCT   RE,*-12                                                          
         DC    H'0'                DIE IF MORE THAN MAX TABLE ENTRIES           
         IC    RE,CHQTABN          BUMP NUMBER OF CHEQUES PROCESSED             
         LA    RE,1(RE)                                                         
         STC   RE,CHQTABN                                                       
         MVI   CHQTINDS,0          SET INDICATOR BYTE                           
         MVC   CHQTCHQ,MPYNO       BUILD A TABLE ENTRY                          
         MVC   CHQTDAT,MPYDTE                                                   
         ZAP   DUB,MPYAMNT                                                      
         CLC   SDREF,SPACES        TEST MANUAL CHEQUE                           
         BNE   *+10                                                             
         MP    DUB,=P'-1'          REVERSE SIGN OF CHEQUE AMOUNT                
         ZAP   CHQTAMT,DUB                                                      
         MVI   CHQTABD+CHQTABL,CHQTEOTQ                                         
         CLI   MPYLN,MPYLN3Q                                                    
         BL    PROC14                                                           
         OI    CHQTINDS,CHQTIPRT   SET PART PAYMENT FOUND                       
         ZAP   DUB,MPYPART         REVERSE SIGN OF CHEQUE AMOUNT                
         CLC   SDREF,SPACES        TEST MANUAL CHEQUE                           
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         ZAP   CHQTAMT,DUB                                                      
         B     PROC14                                                           
         DROP  RF                                                               
*                                                                               
*&&UK                                                                           
         USING CPJELD,R1                                                        
PROC20   CLI   CPJEL,CPJELQ        TEST CLIENT/PRODUCT/JOB                      
         BNE   PROC24                                                           
         MVC   SDCPJ,SPACES                                                     
         CLI   CPJTYPE,CPJTOTH                                                  
         BNE   *+14                                                             
         MVC   SDCPJ(L'CPJOULA),CPJOULA                                         
         B     PROC14                                                           
         MVI   SDCPJ+0,C'S'                                                     
         MVC   SDCPJ+1(1),CPJTYPE                                               
         CLI   CPJTYPE,CPJTJOB     TEST PRODUCTION JOB                          
         BE    *+14                                                             
         MVC   SDCPJ+2(L'SDCPJ-2),CPJEXP                                        
         B     PROC14                                                           
         USING CPYTABD,RE                                                       
         L     RE,ACPYNTRY                                                      
         LA    R2,SDCPJ+2                                                       
         SR    RF,RF                                                            
         ICM   RF,1,CPYPRDAL       L'CLIENT                                     
         BZ    PROC14                                                           
         CH    RF,=Y(L'CPJCLI)                                                  
         BNH   *+8                                                              
         LA    RF,L'CPJCLI                                                      
         BCTR  RF,0                                                             
         MVC   0(0,R2),CPJCLI                                                   
         EX    RF,*-6                                                           
         LA    R2,2(R2,RF)                                                      
         IC    RF,CPYPRDBL         L'PRODUCT                                    
         CH    RF,=Y(L'CPJPRO)                                                  
         BNH   *+8                                                              
         LA    RF,L'CPJPRO                                                      
         BCTR  RF,0                                                             
         MVC   0(0,R2),CPJPRO                                                   
         EX    RF,*-6                                                           
         LA    R2,2(R2,RF)                                                      
         IC    RF,CPYPRDCL         L'JOB                                        
         CH    RF,=Y(L'CPJJOB)                                                  
         BNH   *+8                                                              
         LA    RF,L'CPJJOB                                                      
         BCTR  RF,0                                                             
         MVC   0(0,R2),CPJJOB                                                   
         EX    RF,*-6                                                           
         B     PROC14                                                           
         DROP  RE                                                               
*                                                                               
         USING MXPELD,R1                                                        
PROC24   CLI   MXPEL,MXPELQ                                                     
         BNE   PROC25                                                           
         MVC   SDCPJ,SPACES                                                     
         MVC   SDCPJ(L'MXPCLICD),MXPCLICD                                       
         B     PROC14                                                           
*                                                                               
         USING AFCELD,R1                                                        
PROC25   CLI   AFCEL,AFCELQ                                                     
         BNE   PROC26                                                           
         TM    AFCXSTAT,AFCXSMEM                                                
         BNZ   PROC14                                                           
         MVC   SDCUR,AFCCURR                                                    
         ZAP   SDLOC,AFCAMNT                                                    
         B     PROC14                                                           
*&&                                                                             
*&&US                                                                           
PROC20   L     RE,ACPYNTRY         COBBLE CLI/PRO/JOB TOGETHER                  
         USING CPYTABD,RE                                                       
         MVC   SDCPJ,SPACES                                                     
         MVC   SDCPJ(L'SDCAC),SDCAC  USE C/A IF ALL ELSE FAILS                  
         SR    RF,RF                                                            
         CLI   SKACT+1,C'V'        TEST PRODUCTION LEDGER                       
         BE    *+12                                                             
         CLI   SKACT+1,C'Y'        DITTO                                        
         BNE   PROC22                                                           
         USING OTHELD,R1                                                        
         CLI   OTHEL,OTHELQ                                                     
         BNE   PROC26                                                           
         ICM   RF,1,CPYPRDAL                                                    
         BZ    PROC14              NO LENGTHS AVAILABLE                         
         LA    RF,1(RF)            L'CLIENT+L'UL -1                             
         MVC   SDCPJ(0),SDCAC      CLIENT FROM CONTRA                           
         EX    RF,*-6                                                           
         LA    R2,SDCPJ+1(RF)                                                   
         MVI   0(R2),C' '                                                       
         IC    RF,CPYPRDBL                                                      
         LR    R0,RF               SAVE L'PRODUCT FOR JOB POSITION              
         BCTR  RF,0                                                             
         MVC   1(0,R2),OTHNUM      PRODUCT FROM OTHNUM                          
         EX    RF,*-6                                                           
         LA    R2,2(RF,R2)                                                      
         IC    RF,CPYPRDCL                                                      
         DROP  RE                                                               
         LA    RE,SDCPJ+L'SDCPJ                                                 
         SR    RE,R2               RE=L'SDCPJ REMAINING                         
         CR    RF,RE               ENSURE L'SDCPJ NOT EXCEEDED                  
         BL    *+6                                                              
         LR    RF,RE               IF NOT LOW, SET EXECUTE TO FIT               
         BCTR  RE,0                PRESET REMAINDER TO SPACES                   
         MVC   0(0,R2),SPACES                                                   
         EX    RE,*-6                                                           
         LR    RE,R0               R0=L'PRODUCT                                 
         LA    RE,OTHNUM(RE)       RE=A(JOB) IN OTHNUM                          
         CLI   SAVBTYP,9           UNLESS BT NOT MEDIA TRANSFERS                
         BE    *+8                                                              
         LA    RE,OTHNUM+6         IN WHICH CASE JOB IS HERE                    
         BCTR  RF,0                                                             
         MVC   0(0,R2),0(RE)                                                    
         EX    RF,*-6                                                           
         B     PROC14                                                           
*                                  PRODUCTION LEDGERS                           
         USING CPYTABD,RE                                                       
         USING XPYELD,R1                                                        
PROC22   CLI   XPYEL,XPYELQ                                                     
         BNE   PROC26                                                           
         ICM   RF,1,CPYPRDAL                                                    
         BZ    PROC14              NO LENGTHS AVAILABLE                         
         BCTR  RF,0                                                             
         MVC   SDCPJ(0),SDCAC+2    CLIENT FROM CONTRA                           
         EX    RF,*-6                                                           
         LA    R2,SDCPJ+1(RF)                                                   
         MVI   0(R2),C' '                                                       
         IC    RF,CPYPRDBL         L'PRODUCT                                    
         BCTR  RF,0                                                             
         DROP  RE                                                               
         LA    RE,TRNKREF          PRODUCT IS REFERENCE ...                     
         CLI   SKACT+1,C'P'        ... IF PRINT VENDOR                          
         BE    PROC23                                                           
         CLI   SKACT+1,C'Q'        DITTO                                        
         BE    PROC23                                                           
         LA    RE,XPYPRO           PRODUCT MIGHT BE HERE                        
         CLC   XPYPRO(3),=C'POL'   OR IF 'POL'                                  
         BNE   PROC23                                                           
         LA    RE,XPYPRO+4         HERE                                         
PROC23   MVC   1(0,R2),0(RE)                                                    
         EX    RF,*-6                                                           
         SR    R0,R0                                                            
         ICM   R0,3,XPYEST         TEST ESTIMATE                                
         BZ    PROC14                                                           
         LA    R2,1(RF,R2)         APPEND ESTIMATE TO SOURCE CLI/PRO            
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         UNPK  0(6,R2),DUB         UNPACK INTO REMAINING LENGTH                 
         OI    5(R2),X'F0'                                                      
         LA    R0,5                SHUFFLE OFF LEADING ZEROS                    
PROC24   CLI   0(R2),C'0'                                                       
         BNE   PROC14                                                           
         MVC   0(5,R2),1(R2)                                                    
         MVI   5(R2),C' '          CLEAR TRAILING BYTE                          
         BCT   R0,PROC24                                                        
         B     PROC14                                                           
*&&                                                                             
         USING GDAELD,R1                                                        
PROC26   CLI   GDAEL,GDAELQ                                                     
         BNE   PROC14                                                           
         CLI   GDATYPE,GDATERPD                                                 
         BNE   PROC14                                                           
         MVC   SDEPD,GDADATE                                                    
         B     PROC14                                                           
                                                                                
PROC80   CLI   CHQTABN,1           TEST ONE CHEQUE WAS PROCESSED                
         BNE   PROC82                                                           
         TM    CHQTAB+(CHQTINDS-CHQTABD),CHQTIPRT                               
         BNZ   PROC82                                                           
         ZAP   CHQTAB+(CHQTAMT-CHQTABD)(L'CHQTAMT),TRNAMNT                      
*                                                                               
PROC82   LA    R4,CHQTAB                                                        
         USING CHQTABD,R4          R4=A(MULTI-CHEQUE TABLE)                     
*                                                                               
PROC84   NI    REPINDS,255-REPICHQ                                              
         L     R1,AREPTAB                                                       
         USING REPTABD,R1          R1=A(REPORT DEFINITION TABLE)                
PROC86   CLI   REPTABD,REPTEOTQ    TEST END OF TABLE                            
         BE    EXIT                                                             
         CLC   REPTREP,SKMAJ       MATCH REPORT NUMBER                          
         BNE   PROC88                                                           
         CLC   REPTACT,SKMIN       MATCH MARKER ACTION NUMBER                   
         BNE   PROC88                                                           
         TM    REPTIPRT,REPTIFCQ   TEST FOREIGN CURRENCY COMPANY                
         BZ    PROC90                                                           
         L     RE,ACPYNTRY         COBBLE CLI/PRO/JOB TOGETHER                  
         TM    CPYTSTA6-CPYTABD(RE),CPYSFMCR+CPYSFOCR                           
         BNZ   PROC90                                                           
*                                                                               
PROC88   LA    R1,REPTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         B     PROC86                                                           
*                                                                               
PROC90   LA    R2,REPTSORT         R2=A(SORT ELEMENTS)                          
         LA    R0,L'REPTSORT       R0=MAX NUMBER OF SORT ELEMENTS               
         LA    R5,SKOTH            R5=A(SORT KEY)                               
PROC92   CLI   0(R2),0             TEST END OF PARAMETERS                       
         BE    PROC98                                                           
*                                                                               
         L     R1,ADATTAB                                                       
         USING DATTABD,R1          R1=A(DATA TABLE)                             
PROC94   CLI   DATTABD,DATTEOTQ    TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DATTDAT,27                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DATTDAT,0(R2)       MATCH DATA NUMBER                            
         BE    *+12                                                             
         LA    R1,DATTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         B     PROC94                                                           
         SR    RE,RE                                                            
         ICM   RE,3,DATTDISP                                                    
         LA    RE,SDATA(RE)                                                     
         TM    DATTINDS,DATTICHQ   TEST CHEQUE DATA REQUIRED                    
         BZ    PROC96                                                           
         CLI   CHQTAB,CHQTEOTQ     TEST CHEQUE TABLE IS EMPTY (CHEQUE)          
         BE    PROC96                                                           
         OI    REPINDS,REPICHQ                                                  
         MVC   SDCHQ,CHQTCHQ       SET CHEQUE NUMBER                            
         MVC   SDCDT,CHQTDAT       SET CHEQUE DATE                              
         ZAP   SDAMT,CHQTAMT       SET CHEQUE (MATCHED) AMOUNT                  
*                                                                               
PROC96   SR    RF,RF                                                            
         IC    RF,DATTDLEN                                                      
         BCTR  RF,0                RF=L'DATA-1                                  
         MVC   0(0,R5),0(RE)       MOVE DATA TO SORT KEY                        
         EX    RF,*-6                                                           
         LA    R5,1(RF,R5)         BUMP TO NEXT OUTPUT AREA                     
         LA    R2,1(R2)            BUMP TO NEXT SORT ELEMENT                    
         BCT   R0,PROC92           DO FOR NUMBER OF ELEMENTS                    
*                                                                               
PROC98   CLI   SORTSW,0            TEST SORT INITIALISED                        
         BNE   PROC100                                                          
         LA    R0,SKEYL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTCARD+15(2),DUB                                               
         LA    R0,SRECL                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTRECD+21(3),DUB                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,SORTRECD,0                                
         MVI   SORTSW,1            SET SORT INITIALISED                         
*                                                                               
PROC100  GOTO1 ADSORTER,DMCB,SORTPUT,SREC                                       
         L     RE,ACPYNTRY                                                      
         ICM   R0,3,SKUID          SAVE ORIGINAL USER-ID                        
         MVC   SKUID,CPYTUID-CPYTABD(RE)  GENERATE AN EXTRA COPY                
         MVI   SKCNS,1                    FOR CONSOLIDATED REPORT               
         BASR  RE,RF                                                            
         STCM  R0,3,SKUID          RESET ORIGINAL USER-ID                       
         MVI   SKCNS,0             RESET SORT REPORT TYPE                       
         TM    REPINDS,REPICHQ     TEST CHEQUE DATA REQUIRED                    
         BZ    EXIT                                                             
         LA    R4,CHQTABL(R4)      BUMP TO NEXT CHEQUE                          
         CLI   CHQTABD,CHQTEOTQ                                                 
         BNE   PROC84                                                           
         B     EXIT                                                             
         DROP  R1,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* GET RECORDS AND PRINT REPORT                                        *         
***********************************************************************         
         SPACE 1                                                                
LAST     CLI   SORTSW,0            TEST ANYTHING SORTED                         
         BE    EXIT                                                             
         XC    LKEY,LKEY           SET NO PREVIOUS KEY SAVED                    
         LA    R0,NACCUMS          CLEAR ACCUMULATORS                           
         LA    RF,ACTCR                                                         
         ZAP   0(L'ACTCR,RF),PZERO                                              
         LA    RF,L'ACTCR(RF)                                                   
         BCT   R0,*-10                                                          
*                                                                               
LAST10   GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RE,15,4(R1)                                                      
         BZ    LAST20                                                           
         SR    R7,R7                                                            
         LA    R7,SRECL                                                         
         LR    RF,R7                                                            
         LA    R6,SREC                                                          
         MVCL  R6,RE                                                            
         B     LAST30                                                           
*                                                                               
LAST20   LA    R1,SRECL            BUILD EOF RECORD                             
         LA    R0,SREC                                                          
         XR    RE,RE                                                            
         LA    RF,X'FF'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
                                                                                
         CLI   RCPOSTNG,C'N'       TEST POSTING FILE REQUIRED                   
         BE    LAST30                                                           
         GOTO1 DATAMGR,DMCB,WRKCLO,WRKFIL,WRKID,AIO2,AWRKBUFF                   
*                                                                               
LAST30   OC    LKEY,LKEY           TEST FIRST TIME                              
         BZ    LAST80                                                           
         CLC   SKEY(SKEYC),LKEY    TEST ALL CONTROL BREAKS                      
         BE    LAST290                                                          
         L     R2,AREPNTRY                                                      
         USING REPTABD,R2                                                       
         GOTO1 TOTPRT,1            PRINT ACCOUNT LEVEL TOTALS                   
         GOTO1 TOTADD,1            ADD INTO LEDGER LEVEL AND CLEAR              
         L     RF,ADBXAREA         PRINT BOX LINE AFTER TOTALS                  
         MVI   BOXREQ-BOXD(RF),C'B'                                             
         CLC   SKEY((SKACT-SKEY)+(ACTKACT-ACTKUNT)),LKEY  TEST C/U/L            
         BE    LAST50                                                           
         TM    REPTIPRT,REPTILGQ   TEST TOTALS REQUIRED FOR LEDGER              
         BNO   LAST40                                                           
         GOTO1 TOTPRT,2            PRINT LEDGER TOTALS                          
LAST40   GOTO1 TOTADD,2            ADD LEDGER INTO CPY LEVEL AND CLEAR          
         TM    REPTIPRT,REPTIPLQ   TEST NEW PAGE ON CHANGE OF LEDGER            
         BZ    LAST50                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LAST50   CLC   SKEY(SKACT-SKEY),LKEY  TEST CPY CHANGE                           
         BE    LAST290                                                          
         MVI   FORCEHED,C'N'       DON'T SKIP TO NEW PAGE                       
         GOTO1 TOTPRT,3            PRINT COMPANY TOTALS                         
         GOTO1 TOTADD,3            CLEAR COMPANY LEVEL TOTALS                   
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C'C'                                             
         GOTO1 PRINT,DMCB,SPACES,BL01                                           
         XC    LKEY,LKEY           CLEAR SAVED KEY                              
*                                                                               
LAST60   CLC   SKEY(SKMIN-SKEY),LKEY                                            
         BE    LAST80                                                           
         L     RE,ACPYNTRY         TEST REMOTE LOGO COMPANY                     
         TM    CPYTSTA8-CPYTABD(RE),CPYSRLOG                                    
         BNZ   *+12                                                             
         TM    REPINDS,REPIREM     TEST REMOTE REPORT                           
         BNZ   LAST70                                                           
         L     R2,LOGOC                                                         
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
                                                                                
         USING BOXD,RF                                                          
         L     RF,ADBXAREA                                                      
         MVC   BYTE,BOXYORN                                                     
         MVI   BOXYORN,C'N'                                                     
         GOTO1 LOGO,DMCB,LOGOD                                                  
         L     RF,ADBXAREA                                                      
         MVC   BOXYORN,BYTE                                                     
         B     LAST80                                                           
         DROP  R2                                                               
*                                                                               
LAST70   GOTO1 PRINT,DMCB,SPACES,BC01                                           
         GOTO1 (RF),(R1),(L'PRTCLOSE,PRTCLOSE)                                  
*                                                                               
LAST80   CLI   SKEY,X'FF'          TEST E-O-F                                   
         BE    EXIT                                                             
         NI    REPINDS,255-REPICNM                                              
         CLI   SKCNS,1             TEST CONSOLIDATED COPY                       
         BNE   LAST90                                                           
         XC    LKEY,LKEY           CLEAR SAVED KEY                              
         TM    REPINDS,REPICNS     TEST CONSOLIDATED REPORT REQUIRED            
         BZ    LAST10              NO - IGNORE ALL                              
         OI    REPINDS,REPICNM     YES - SET CONSOLIDATED REPORT MODE           
*                                                                               
LAST90   L     R1,ACPYTAB                                                       
         USING CPYTABD,R1          LOCATE COMPANY TABLE ENTRY                   
LAST100  CLI   CPYTABD,CPYTEOTQ    TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                DIDN'T FIND A COMPANY RECORD                 
         CLC   SKCPY,CPYTCPY                                                    
         BE    *+12                                                             
         LA    R1,CPYTABL(R1)                                                   
         B     LAST100                                                          
         NI    REPINDS,255-REPICNS CLEAR CONSOLIDATED REPORT BIT                
         CLI   CPYTINDS,CPYTIMUQ   TEST MIXED USER-IDS ON THIS COMPANY          
         BNE   *+8                 NO                                           
         OI    REPINDS,REPICNS     YES - SET CONSOLIDATED REPORT REQ'D          
         ST    R1,ACPYNTRY                                                      
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   MCCTRY,CPYTCTRY                                                  
         DROP  R1,RF                                                            
*                                                                               
         L     R2,AREPTAB          INITIALISE FOR NEW REPORT                    
         USING REPTABD,R2                                                       
LAST110  CLI   REPTABD,REPTEOTQ    TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   REPTREP,SKMAJ       MATCH ON REPORT NUMBER                       
         BNE   LAST120                                                          
         CLC   REPTACT,SKMIN       AND MARKER ACTION NUMBER                     
         BNE   LAST120                                                          
         TM    REPTIPRT,REPTIFCQ   TEST FOREIGN CURRENCY REPORT                 
         BZ    LAST130                                                          
         L     RE,ACPYNTRY         TEST FOREIGN CURRENCY COMPANY                
         TM    CPYTSTA6-CPYTABD(RE),CPYSFMCR+CPYSFOCR                           
         BNZ   LAST130                                                          
*                                                                               
LAST120  LA    R2,REPTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         B     LAST110                                                          
*                                                                               
LAST130  ST    R2,AREPNTRY         SAVE A(REPORT TABLE ENTRY)                   
         MVC   RCPROG,REPTCODE     SET REPORT CODE                              
         SR    RF,RF                                                            
         ICM   RF,3,REPTNAME                                                    
         LA    RF,DICO(RF)                                                      
         MVC   LTIT,0(RF)          SET REPORT TITLE                             
         LA    RF,LTIT                                                          
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         LA    RE,LTIT+L'LTIT-1                                                 
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    RE,L'LTIT(RE)       POINT RE & RF INTO LUND FIELD                
         LA    RF,L'LTIT(RF)                                                    
         MVC   LUND,SPACES                                                      
         MVI   0(RF),X'BF'         HORIZONTAL BOX CHARACTER                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   1(0,RF),0(RF)       UNDERSCORE THE TITLE                         
         EX    RE,*-6                                                           
         MVC   LACT,SPACES                                                      
         SR    RF,RF                                                            
         ICM   RF,3,REPTMACT                                                    
         BZ    LAST140                                                          
         LA    RF,DICO(RF)                                                      
         SR    RE,RE                                                            
         IC    RE,REPTMLEN                                                      
         BCTR  RE,0                                                             
         MVC   LACT(0),0(RF)       SET NEGATIVE MARKER ACTION WORD              
         EX    RE,*-6                                                           
LAST140  MVC   LHED,SPACES                                                      
         MVC   HEAD5,SPACES                                                     
         MVI   LMRK,0                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,REPTPRNT                                                      
         USING REPTPRNT,R2         R2=A(PRINT COLUMN SPECS)                     
         LA    R0,L'REPTPRNT       R0=MAX NUMBER OF PRINT SPECS                 
         SR    R3,R3               R3=CURRENT LINE DISPLACEMENT                 
         L     R4,ADBXAREA                                                      
         USING BOXD,R4             R5=A(BOX)                                    
         MVI   BOXCOLS,C' '                                                     
         MVC   BOXCOLS+1(L'BOXCOLS-1),BOXCOLS                                   
         MVI   BOXROWS,C' '                                                     
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXROWS+05,C'T'                                                  
         MVI   BOXROWS+07,C'M'                                                  
*&&UK*&& MVI   BOXROWS+99,C'B'                                                  
*&&US*&& MVI   BOXROWS+60,C'B'                                                  
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
*                                                                               
LAST150  CLI   REPTPRNT,0          TEST END OF PRINT SPEC                       
         BE    LAST180                                                          
         L     R5,ADATTAB                                                       
         USING DATTABD,R5          R5=A(DATA TABLE)                             
LAST160  CLI   DATTABD,DATTEOTQ    TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DATTDAT,REPTPRNT    MATCH ON DATA NUMBER                         
         BE    *+12                                                             
         LA    R5,DATTABL(R5)                                                   
         B     LAST160                                                          
         CLI   DATTDAT,DMRK        TEST NEGATIVE MARKING FIELD                  
         BNE   *+8                                                              
         STC   R3,LMRK             SAVE DISPLACEMENT TO DATA                    
         SR    RE,RE                                                            
         ICM   RE,3,DATTHEAD                                                    
         LA    RE,DICO(RE)         RE=A(DICTIONARY WORD)                        
         SR    RF,RF                                                            
         ICM   RF,1,DATTHLEN       RF=WIDTH OF COLUMN HEADING                   
         BZ    LAST170                                                          
         BCTR  RF,0                                                             
         LA    R1,LHED(R3)                                                      
         MVC   1(0,R1),0(RE)       MOVE COLUMN HEADING TO LHED                  
         EX    RF,*-6                                                           
LAST170  LA    R1,BOXCOLS(R3)                                                   
         MVI   0(R1),C'C'          SET BOX COLUMN                               
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         MVI   0(R1),C'L'          OR BOX LEFT (IF FIRST TIME)                  
         IC    RF,DATTOLEN                                                      
         LA    R3,1(RF,R3)         R3=A(NEXT OUTPUT AREA)                       
         LA    R2,1(R2)            BUMP TO NEXT DATA ELEMENT                    
         BCT   R0,LAST150                                                       
LAST180  LA    R1,BOXCOLS(R3)      SET BOX RIGHT                                
         MVI   0(R1),C'R'                                                       
         DROP  R2,R4,R5                                                         
*                                                                               
         CLC   SKEY(SKMIN-SKEY),LKEY                                            
         BE    LAST290                                                          
         L     R1,ACPYTAB                                                       
         USING CPYTABD,R1          R1=A(COMPANY TABLE)                          
LAST190  CLI   CPYTABD,CPYTEOTQ    TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SKCPY,CPYTCPY       MATCH ON COMPANY CODE                        
         BE    *+12                                                             
         LA    R1,CPYTABL(R1)                                                   
         B     LAST190                                                          
         ST    R1,ACPYTAB                                                       
         DROP  R1                                                               
*                                                                               
         L     R2,AIO1                                                          
         USING CTIREC,R2           R2=A(USER-ID RECORD)                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,SKUID                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTIREC,CTIREC                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UIDLOG1,SPACES                                                   
         MVC   UIDLOG2,SPACES                                                   
         MVC   UIDNAME,SPACES                                                   
         MVC   UIDADDR,SPACES                                                   
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CTIDATA          LOCATE ORIGIN DETAILS ELEMENT                
         USING CTDSTD,R1                                                        
LAST200  IC    R0,CTDSTLEN                                                      
         AR    R1,R0                                                            
         CLI   CTDSTEL,0           TEST E-O-R                                   
         BE    LAST220                                                          
         CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION DETAILS ELEMENT             
         BNE   LAST200                                                          
         MVC   UIDLOG1,CTDSTLG1                                                 
         MVC   UIDLOG2,CTDSTLG2                                                 
         MVC   UIDNAME,CTDSTNAM                                                 
         MVC   UIDADDR,CTDSTADD                                                 
         CLI   CTDSTLEN,166        TEST LONG ELEMENT                            
         BL    LAST210                                                          
         MVC   UIDADD2,CTDSTAD2    YES - GET REST OF ADDRESS/ATTN DETS          
         MVC   UIDADD3,CTDSTAD3                                                 
LAST210  MVC   UIDJOBC,CTDSTPOW                                                 
         CLI   UIDJOBC+L'UIDJOBC-1,C' '                                         
         BNE   *+8                                                              
         MVI   UIDJOBC+L'UIDJOBC-1,C'X'                                         
         B     LAST200                                                          
         DROP  R1,R2                                                            
*                                                                               
LAST220  CLI   RCPOSTNG,C'N'       TEST POSTING FILE REQUIRED                   
         BE    LAST230                                                          
*                                                                               
         XC    WRKID,WRKID         BUILD KEY OF WORKER FILE                     
         MVC   WRKIUID,SKUID                                                    
         MVI   WRKISYS,WRKISYSQ                                                 
         MVC   WRKIPRG,RCPROG                                                   
         MVC   WRKIDAY,TODAYP+2                                                 
         MVI   WRKITYP,WRKITYPQ                                                 
*                                                                               
         OI    UKFLAG,UKFSDUPS     ALLOW DUPLICATE KEYS                         
         MVC   UKFILNO,=X'FFFF'    FILE SEQUENCE NUMBER FORCE OPEN              
*                                                                               
         L     R2,AIO2                                                          
         USING WRKRECD,R2          BUILD WORKER RECORD                          
         XC    WRKRECD(WRKRLENQ),WRKRECD                                        
         MVC   WRKRLEN,=Y(WRKRLENQ)                                             
         MVC   WRKRCPY,SKCPY                                                    
         MVC   WRKRUID,SKUID                                                    
         MVC   WRKRLOGO,UIDLOG1                                                 
         L     R1,ACPYTAB                                                       
         CLC   UIDLOG1,SPACES                                                   
         BNE   *+10                                                             
         MVC   WRKRLOGO,CPYTLOGO-CPYTABD(R1)                                    
         GOTO1 DATAMGR,DMCB,WRKADD,WRKFIL,WRKID,WRKRECD,AWRKBUFF                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
LAST230  NI    REPINDS,255-REPIREM SET REPORT NOT REMOTE                        
         L     R3,REMOTEC                                                       
         USING REMOTED,R3          R3=A(REMOTE CSECT)                           
         XC    REMOTKEY,REMOTKEY                                                
*                                                                               
         XC    SVREMOTE,SVREMOTE                                                
         LA    R4,SVREMOTE                                                      
SAVE     USING REMOTED,R4                                                       
         XC    WORK,WORK                                                        
         MVI   WORK,C'A'             SYSTEM CODE IS 'A' ACCOUNTING              
         L     RF,AREPNTRY                                                      
         MVC   WORK+1(2),REPTCODE-REPTABD(RF) PROGRAM CODE MA/MR/MU ETC         
         MVC   WORK+3(2),SKUID                                                  
         L     RF,AUTL                                                          
         MVC   SVSEN,4(RF)           SAVE SYSTEM UTL                            
         MVI   4(RF),X'0A'           HARD CODE TO READ CONTROL FILE             
         GOTO1 PQPROF,DMCB,(0,WORK),(0,SVREMOTE),ADCOMFAC                       
         L     RF,AUTL                                                          
         MVC   4(1,RF),SVSEN         RESTORE SYSTEM UTL                         
         MVC   REMOTTYP,SAVE.REMOTTYP                                           
         MVC   REMOTARC,SAVE.REMOTARC   SET ARCHIVE BITS                        
         MVC   REMOTTY1,SAVE.REMOTTY1                                           
         DROP  SAVE                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING CTPREC,R2           R2=A(PROFILE RECORD)                         
         XC    CTPKEY,CTPKEY                                                    
         XC    OUTTYPE,OUTTYPE                                                  
LAST240  MVI   CTPKTYP,CTPKTYPQ                                                 
         L     RF,AREPNTRY                                                      
         MVI   CTPKSYS,C'A'                                                     
         MVC   CTPKPROG,REPTCODE-REPTABD(RF)                                    
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTPREC,CTPREC                         
         BNE   LAST270                                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CTPDATA          R1=A(FIRST ELEMENT)                          
         USING CTOCOD,R1                                                        
LAST250  IC    R0,CTOCOLEN         LOCATE OUTPUT TYPE CODE ELEMENT              
         AR    R1,R0                                                            
         CLI   CTOCOEL,0           TEST E-O-R                                   
         BE    LAST260                                                          
         CLI   CTOCOEL,CTOCOELQ    TEST OUTPUT CODE ELEMENT                     
         BNE   LAST250                                                          
         MVC   OUTTYPE,CTOCODE                                                  
         B     LAST250                                                          
*                                                                               
LAST260  OC    CTPKORIG,CTPKORIG                                                
         BNZ   LAST270                                                          
         XC    CTPKEY,CTPKEY                                                    
         MVC   CTPKORIG,SKUID      READ AGENCY LEVEL PROFILE NOW.               
         B     LAST240                                                          
*                                                                               
LAST270  CLC   OUTTYPE(L'REMLIT),REMLIT                                         
         BNE   LAST275                                                          
         MVC   REMOTSYS(3),CTPKSYS                                              
         MVC   REMOTJID,CTPKSYS                                                 
         MVI   REMOTFLG,X'FA'                                                   
         MVC   REMOTDSC(L'REPLIT),REPLIT                                        
         MVC   REMOTDST,SKUID                                                   
         MVI   REMOTCLS,C'Q'                                                    
         OI    REPINDS,REPIREM     SET REPORT IS REMOTE                         
         DROP  R2,R3                                                            
*                                                                               
LAST275  MVC   WORK,SKUID                                                       
         GOTO1 =A(OUTTRNTO),DMCB,AIO3,WORK                                      
*                                                                               
LAST280  L     RE,ACPYNTRY         TEST REMOTE LOGO COMPANY                     
         TM    CPYTSTA8-CPYTABD(RE),CPYSRLOG                                    
         BNZ   *+12                                                             
         TM    REPINDS,REPIREM     TEST REPORT IS REMOTE                        
         BNZ   LAST290                                                          
         L     R2,LOGOC            INITIALISE LOGOD & PRINT START LOGOS         
         USING LOGOD,R2                                                         
         GOTO1 GETLOGO,DMCB,SKUID,LOGOD,DATAMGR                                 
         MVC   LOGO1,UIDLOG1                                                    
         MVC   LOGO2,UIDLOG2                                                    
         MVC   LOGONAME,UIDNAME                                                 
         MVC   LOGOADD,UIDADDR                                                  
         MVC   LOGOADD2,UIDADD2                                                 
         MVC   LOGOADD3,UIDADD3                                                 
         MVC   LOGOIDNO,SKUID                                                   
         L     R1,ACPYNTRY                                                      
         USING CPYTABD,R1          SET DEFAULTS FROM COMPANY VALUES             
         CLC   LOGO1,SPACES                                                     
         BNE   *+10                                                             
         MVC   LOGO1,CPYTLOGO                                                   
         CLC   LOGONAME,SPACES                                                  
         BNE   *+10                                                             
         MVC   LOGONAME,CPYTNAME                                                
         CLC   LOGOADD,SPACES                                                   
         BNE   *+10                                                             
         MVC   LOGOADD,CPYTADDR                                                 
         MVC   LOGOJOB(L'UIDJOBC),UIDJOBC                                       
         MVI   LOGOJOB+L'UIDJOBC,C'A'                                           
         L     RF,AREPNTRY                                                      
         MVC   LOGOJOB+L'UIDJOBC+1(L'REPTCODE),REPTCODE-REPTABD(RF)             
         DROP  R1                                                               
         MVI   LOGOTYPE,C'S'                                                    
         MVI   LOGOEND,C'X'                                                     
                                                                                
         USING BOXD,RF                                                          
         L     RF,ADBXAREA                                                      
         MVC   BYTE,BOXYORN                                                     
         MVI   BOXYORN,C'N'                                                     
         GOTO1 LOGO,DMCB,LOGOD     PRINT START LOGOS                            
         L     RF,ADBXAREA                                                      
         MVC   BOXYORN,BYTE                                                     
         DROP  R2,RF                                                            
*                                                                               
LAST290  L     R2,AREPNTRY                                                      
         USING REPTABD,R2                                                       
         LA    R2,REPTPRNT                                                      
         USING REPTPRNT,R2         R2=A(REPORT COLUMN SPECS)                    
         LA    R0,L'REPTPRNT       R0=MAX NUMBER OF COLUMN SPECS                
         LA    R3,P+1              R3=A(FIRST OUTPUT COLUMN)                    
*                                                                               
LAST300  CLI   REPTPRNT,0          TEST END OF COLUMN SPEC                      
         BE    LAST350                                                          
         L     R4,ADATTAB                                                       
         USING DATTABD,R4          R4=A(DATA TABLE)                             
LAST310  CLI   DATTABD,DATTEOTQ    LOCATE DATA TABLE ENTRY                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DATTDAT,REPTPRNT    MATCH ON DATA NUMBER                         
         BE    *+12                                                             
         LA    R4,DATTABL(R4)                                                   
         B     LAST310                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,DATTDISP                                                    
         LA    R1,SDATA(R1)        R1=A(INPUT DATA)                             
         SR    RE,RE                                                            
         IC    RE,DATTDLEO                                                      
         BCTR  RE,0                RE=L'INPUT DATA-1                            
         SR    RF,RF                                                            
         ICM   RF,3,DATTOEDT       RF=DISPLACEMENT TO EDIT ROUTINE              
         BZ    *+14                                                             
         LA    RF,ACMA02(RF)                                                    
         BASR  RE,RF                                                            
         B     LAST330                                                          
         LR    R6,R3                                                            
         LR    R7,R1                                                            
         SR    RF,RF                                                            
         IC    RF,DATTNLIN         RF=NUMBER OF LINES TO PRINT                  
LAST320  MVC   0(0,R6),0(R7)       MOVE DATA TO PRINT LINE                      
         EX    RE,*-6                                                           
         LA    R6,L'P(R6)                                                       
         AR    R7,RE                                                            
         AHI   R7,1                                                             
         BCT   RF,LAST320                                                       
*                                                                               
LAST330  XC    ACODNAM,ACODNAM     SUPPRESS ACCOUNT PRINTING                    
         CLI   REPTPRNT,DAM1       TEST THIS IS AN AMOUNT                       
         BE    *+12                                                             
         CLI   REPTPRNT,DAM2                                                    
         BNE   LAST340                                                          
         LA    RE,P+1                                                           
         LR    R0,R3                                                            
         SR    R0,RE                                                            
         STCM  R0,3,DTOT           SAVE DISP INTO P TO PRINT TOTAL              
         ZAP   DUB,0(L'SDAMT,R1)   EXTRACT TRANSACTION AMOUNT                   
         LA    RE,ACMKCR                                                        
         CLI   SDMRK,C'*'          TEST THIS IS A REVERSAL                      
         BNE   *+14                                                             
         LA    RE,ACUMCR                                                        
         MP    DUB,=P'-1'          YES - REVERSE SIGN OF POSTING                
         LA    RF,ACTCR                                                         
         TM    SDSTA,TRNSDR        TEST DEBIT POSTING                           
         BZ    *+12                                                             
         LA    RE,2*L'ACMKCR(RE)                                                
         LA    RF,ACTDR                                                         
         AP    0(L'ACMKCR,RE),DUB  ADD INTO ACCOUNT DRS/CRS AND UN/MARK         
         AP    0(L'ACTCR,RF),DUB                                                
*                                                                               
LAST340  SR    R1,R1                                                            
         IC    R1,DATTOLEN                                                      
         LA    R3,1(R3,R1)         BUMP TO NEXT OUTPUT ADDRESS                  
         LA    R2,1(R2)            BUMP TO NEXT SPEC                            
         BCT   R0,LAST300          DO FOR NUMBER OF SPECS                       
         DROP  R2,R4                                                            
*                                                                               
LAST350  GOTO1 ACREPORT            PRINT A LINE                                 
         MVC   LKEY,SKEY           SET THIS TIME KEY                            
         B     LAST10                                                           
*                                                                               
SVREMOTE DS    CL(REMOTEDL)        SAVED REMOTE BLOCK                           
SVSEN    DS    XL1                                                              
OUTTYPE  DS    CL(L'CTOCODE)                                                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT AND PRINT TOTAL LINES AT ANY LEVEL                *         
* NTRY R1=TOTALS LEVEL (1=ACCOUNT, 2=LEDGER, 3=COMPANY)               *         
*      R2=AREPNTRY                                                    *         
*      R3=A(LOCATION TO PRINT TOTALS)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R2                                                       
TOTPRT   NTR1                                                                   
         BCTR  R1,0                                                             
         LR    R0,R1                                                            
         SLL   R0,2                0=ACCOUNT, 4=LEDGER, 8=COMPANY               
         TM    REPTIPRT,REPTIMKQ   TEST MARKED/UNMARKED TOTALS WANTED           
         BO    *+12                                                             
         LA    R4,PSECOND+1        DOUBLE SPACE FIRST TOTAL LINE                
         B     TOTP6                                                            
         LA    R4,ACMKCR                                                        
         SLL   R0,3                DISP TO UN/MARKED TOTALS FOR LEVEL           
         AR    R4,R0                                                            
         ZAP   DUB,0(L'ACMKCR,R4)  CREDITS                                      
         ZAP   DUB2,L'ACMKCR(L'ACUMCR,R4)                                       
         TM    REPTIPRT,REPTICRQ                                                
         BO    TOTP2                                                            
         LA    RF,2*L'ACMKCR(R4)                                                
         ZAP   DUB,0(L'ACMKDR,RF)  DEBITS                                       
         ZAP   DUB2,L'ACMKDR(L'ACUMDR,RF)                                       
         TM    REPTIPRT,REPTIDRQ                                                
         BO    TOTP2                                                            
         SP    DUB,0(L'ACMKCR,R4)  BALANCE                                      
         SP    DUB2,L'ACMKCR(L'ACUMCR,R4)                                       
*                                                                               
TOTP2    LA    R4,PSECOND+1        DOUBLE SPACE FIRST TOTAL LINE                
         SRL   R0,3                DISP TO LITERALS FOR TOTAL LEVEL             
         CP    DUB,PZERO                                                        
         BZ    TOTP4                                                            
         LA    RE,REPTTMAC                                                      
         AR    RE,R0                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          'TOTAL MARKED FOR ACC/LGR/CPY'               
         BZ    TOTP4               NO TOTAL NAME DEFINED                        
         LA    RF,DICO(RF)                                                      
         MVC   0(L'AC@THEAC,R4),0(RF)                                           
         LH    R3,DTOT                                                          
         AR    R3,R4                                                            
         GOTO1 EDAMT1,DUB                                                       
         GOTO1 ACREPORT            PRINT MARKED ACCOUNT TOTALS                  
         LA    R4,P+1                                                           
*                                                                               
TOTP4    CP    DUB2,PZERO                                                       
         BZ    TOTP6                                                            
         LA    RE,REPTTUAC                                                      
         AR    RE,R0                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          'TOTAL UNMARKED FOR ACC/LGR/CPY'             
         BZ    TOTP6                                                            
         LA    RF,DICO(RF)                                                      
         MVC   0(L'AC@THEAC,R4),0(RF)                                           
         LH    R3,DTOT                                                          
         AR    R3,R4                                                            
         GOTO1 EDAMT1,DUB2                                                      
         GOTO1 ACREPORT            PRINT UNMARKED ACCOUNT TOTALS                
         LA    R4,P+1                                                           
*                                                                               
TOTP6    CLM   R0,1,=X'04'                                                      
         BE    TOTPX               NO LEDGER LEVEL TOTAL                        
         LA    RF,ACTCR                                                         
         MVC   0(L'AC@TCHTA,R4),AC@TCHTA  'TOTAL CHANGE TO ACC'                 
         LTR   R0,R0               TEST ACCOUNT LEVEL                           
         BZ    *+14                                                             
         MVC   0(L'AC@TCHTC,R4),AC@TCHTC  'TOTAL CHANGE TO CPY'                 
         LA    RF,CPYCR                                                         
         ZAP   DUB,0(L'ACTCR,RF)        CREDITS                                 
         TM    REPTIPRT,REPTICRQ                                                
         BO    TOTP8                                                            
         ZAP   DUB,L'ACTCR(L'ACTDR,RF)  DEBITS                                  
         TM    REPTIPRT,REPTIDRQ                                                
         BO    TOTP8                                                            
         SP    DUB,0(L'ACTCR,RF)        BALANCE                                 
*                                                                               
TOTP8    LH    R3,DTOT                                                          
         AR    R3,R4                                                            
         GOTO1 EDAMT1,DUB                                                       
         GOTO1 ACREPORT            PRINT THE TOTALS                             
*                                                                               
TOTPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD TOTALS INTO NEXT HIGHER LEVEL AND CLEAR LOWER LEVEL  *         
* NTRY R1=TOTALS LEVEL (1=ACCOUNT, 2=LEDGER, 3=COMPANY)               *         
***********************************************************************         
         SPACE 1                                                                
TOTADD   NTR1                                                                   
         BCTR  R1,0                                                             
         LR    R0,R1                                                            
         SLL   R0,2                0=ACCOUNT, 4=LEDGER, 8=COMPANY               
         LA    RE,ACTCR            ADD TOTALS TO NEXT LEVEL AND CLEAR           
         LA    RF,ACMKCR                                                        
         LTR   R0,R0                                                            
         BZ    TOTA2                                                            
         SLL   R0,2                                                             
         AR    RE,R0               +2*PL8                                       
         SLL   R0,1                                                             
         AR    RF,R0               +4*PL8                                       
         SRA   R0,6                                                             
         BNZ   TOTA4               AT COMPANY LEVEL CLEAR ACCUMS ONLY           
*                                                                               
TOTA2    AP    16(L'ACTCR,RE),00(L'ACTCR,RE)                                    
         AP    24(L'ACTDR,RE),08(L'ACTDR,RE)                                    
         AP    32(L'ACMKCR,RF),00(L'ACMKCR,RF)                                  
         AP    40(L'ACUMCR,RF),08(L'ACUMCR,RF)                                  
         AP    48(L'ACMKDR,RF),16(L'ACMKDR,RF)                                  
         AP    56(L'ACUMDR,RF),24(L'ACUMDR,RF)                                  
*                                                                               
TOTA4    ZAP   00(L'ACTCR,RE),PZERO         CLEAR ACCUMULATORS                  
         ZAP   08(L'ACTDR,RE),PZERO                                             
         ZAP   00(L'ACMKCR,RF),PZERO                                            
         ZAP   08(L'ACMKDR,RF),PZERO                                            
         ZAP   16(L'ACUMCR,RF),PZERO                                            
         ZAP   24(L'ACUMDR,RF),PZERO                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT AN ACCOUNT CODE AND NAME (IF RECORD FOUND)          *         
*                                                                     *         
* NTRY - R1=A(ACCOUNT CODE (ULACCOUNT))                               *         
*        R3=A(OUTPUT AREA)                                            *         
***********************************************************************         
         SPACE 1                                                                
EDACT    NTR1  ,                                                                
         CLC   0(L'SKACT,R1),LKEY+(SKACT-SKEY)                                  
         BE    EDACTX                                                           
         MVC   LCODNAM,SPACES                                                   
         MVC   LCODNAM(ACTKEND-L'ACTKCPY),0(R1)                                 
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,SKCPY                                                    
         MVC   ACTKUNT(ACTKEND-L'ACTKCPY),0(R1)                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,ACTRECD,ACTRECD                       
         BNE   EDACT4                                                           
         LA    R1,ACTRECD+(ACRECORD-ACKEYD)                                     
         SR    R0,R0                                                            
         USING NAMELD,R1                                                        
EDACT2   CLI   NAMEL,0             TEST E-O-R                                   
         BE    EDACT4                                                           
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         B     EDACT2                                                           
         LA    RF,LCODNAM+(ACTKEND-L'ACTKCPY-1)                                 
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   2(0,RF),NAMEREC     ATTACH ACCOUNT NAME                          
         EX    RE,*-6                                                           
EDACT4   MVC   0(L'LCODNAM,R3),LCODNAM                                          
         STCM  R3,15,ACODNAM       SAVE A(ACCOUNT CODE & NAME)                  
EDACTX   B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT AN ACCOUNT CODE AND NAME IN LINE BELOW              *         
*                                                                     *         
* NTRY - R1=A(ACCOUNT CODE (ULACCOUNT))                               *         
*        R3=A(OUTPUT AREA)                                            *         
***********************************************************************         
         SPACE 1                                                                
EDACTNR  NTR1  ,                                                                
         CLC   0(L'SKACT,R1),LKEY+(SKACT-SKEY)                                  
         BE    EDACTNX                                                          
         MVC   LCODNAM,SPACES                                                   
         MVC   LCODNAM(ACTKEND-L'ACTKCPY),0(R1)                                 
         MVC   0(L'ACTKUNT+L'ACTKLDG+L'ACTKACT,R3),0(R1)                        
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,SKCPY                                                    
         MVC   ACTKUNT(ACTKEND-L'ACTKCPY),0(R1)                                 
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,ACTRECD,ACTRECD                       
         BNE   EDACTN4                                                          
         LA    R1,ACTRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING NAMELD,R1                                                        
EDACTN2  CLI   NAMEL,0             TEST E-O-R                                   
         BE    EDACTN4                                                          
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         B     EDACTN2                                                          
         LA    RF,LCODNAM+(ACTKEND-L'ACTKCPY-1)                                 
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   2(0,RF),NAMEREC     ATTACH ACCOUNT NAME                          
         EX    RE,*-6                                                           
         MVC   L'P(0,R3),NAMEREC                                                
         EX    RE,*-6                                                           
EDACTN4  LR    R3,R3                                                            
EDACTNX  B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EDIT A DATE IN PACKED FORMAT                             *         
***********************************************************************         
         SPACE 1                                                                
EDDATP   NTR1  ,                                                                
         LR    RF,R1                                                            
         OC    0(3,RF),0(RF)                                                    
         BZ    EDDATPX                                                          
         GOTO1 DATCON,DMCB,(1,(RF)),(17,(R3))                                   
EDDATPX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EDIT A DATE IN COMPRESSED FORMAT                         *         
***********************************************************************         
         SPACE 1                                                                
EDDATC   NTR1  ,                                                                
         LR    RF,R1                                                            
         OC    0(2,RF),0(RF)                                                    
         BZ    EDDATCX                                                          
         GOTO1 DATCON,DMCB,(2,(RF)),(17,(R3))                                   
EDDATCX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EDIT AN AMOUNT (VANILLA)                                 *         
***********************************************************************         
         SPACE 1                                                                
EDAMT1   NTR1  ,                                                                
         LR    RF,R1                                                            
         CURED (P8,0(RF)),(15,0(R3)),2,MINUS=YES,COMMAS=YES                     
EDAMT1X  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EDIT AN AMOUNT WITH TRAILING DR/CR                       *         
***********************************************************************         
         SPACE 1                                                                
EDAMT2   NTR1  ,                                                                
         LR    RF,R1                                                            
         CURED (P8,0(RF)),(15,0(R3)),2,MINUS=YES,COMMAS=YES                     
         MVC   15(2,R3),AC@CR                                                   
         TM    SDSTA,TRNSDR                                                     
         BZ    *+10                                                             
         MVC   15(2,R3),AC@DR                                                   
EDAMT2X  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO EDIT A LOCAL CURRENCY AMOUNT                             *         
* NTRY - R1 POINTS TO A 3 BYTE CURRENCY CODE FOLLOWED BY A PL8 AMOUNT *         
***********************************************************************         
         SPACE 1                                                                
EDLOC    NTR1  ,                                                                
         LR    RF,R1               POINT TO CURRENCY/AMOUNT                     
         OC    0(3,RF),0(RF)       TEST CURRENCY SET                            
         BZ    EDLOCX                                                           
         L     R2,AMONACC          LOOK UP CURRENCY CODE IN TABLE               
         L     R2,ACMACURT-ACMD(R2)                                             
         USING CURTABD,R2                                                       
EDLOC2   CLI   CURTCUR,0           TEST END OF TABLE                            
         BE    EDLOCX                                                           
         CLC   CURTCUR,0(RF)       MATCH CURRENCY CODE TO TABLE                 
         BE    *+12                                                             
         LA    R2,CURTABLQ(R2)                                                  
         B     EDLOC2                                                           
         CURED (P8,3(RF)),(15,0(R3)),(R2),MINUS=YES,CURSYMB=YES                 
*                                                                               
EDLOCX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT THE PERSON ID CODE                                 *         
***********************************************************************         
         SPACE 1                                                                
EDPID    NTR1  ,                                                                
         LR    RF,R1                                                            
         L     R2,AIO1                                                          
         USING SA0REC,R2           GET PERSON FROM PID                          
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SDSALPH                                                  
         MVC   SA0KNUM,0(RF)                                                    
         MVC   0(L'SAPALPID,R3),SPACES                                          
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,SA0REC,SA0REC                         
         BNE   EDPIDX                                                           
                                                                                
         SR    R0,R0                                                            
         LA    R1,SA0DATA          GET THE NAME                                 
                                                                                
         USING SAPALD,R1                                                        
EDPID2   CLI   SAPALEL,0                                                        
         BE    EDPIDX                                                           
         CLI   SAPALEL,SAPALELQ                                                 
         BE    EDPID4                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     EDPID2                                                           
                                                                                
EDPID4   MVC   0(L'SAPALPID,R3),SAPALPID                                        
                                                                                
EDPIDX   B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO BUILD HEADLINES                                          *         
***********************************************************************         
         SPACE 1                                                                
HEADS    MVC   HEAD1+29(L'LTIT),LTIT                                            
         MVC   HEAD2+29(L'LUND),LUND                                            
         TM    REPINDS,REPICNM     TEST CONSOLIDATED REPORT MODE                
         BNO   *+10                                                             
         MVC   HEAD3+29(L'AC@CONRP),AC@CONRP                                    
         MVC   HEAD7,LHED                                                       
         MVC   HEAD4+1(L'AC@CPY),AC@CPY                                         
         LA    RF,HEAD4+L'AC@CPY                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         L     R1,ACPYNTRY                                                      
         MVC   2(L'CPYTNAME,RF),CPYTNAME-CPYTABD(R1)                            
         SR    RF,RF                                                            
         ICM   RF,1,LMRK                                                        
         BZ    HEADS2                                                           
         LA    RF,HEAD5+1(RF)                                                   
         MVC   0(2,RF),=C'*='                                                   
         MVC   2(L'LACT,RF),LACT                                                
HEADS2   ICM   R1,15,ACODNAM                                                    
         BZ    HEADSX                                                           
         MVC   0(L'LCODNAM,R1),LCODNAM                                          
HEADSX   BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
         SPACE 1                                                                
ACPYTAB  DC    A(CPYTAB)           A(COMPANY TABLE)                             
ACPYNTRY DC    A(0)                A(COMPANY TABLE ENTRY)                       
AREPTAB  DC    A(REPTAB)           A(REPORT TABLE)                              
AREPNTRY DC    A(0)                A(REPORT TABLE ENTRY)                        
AACTTAB  DC    A(ACTTAB)           A(MARKER ACTION TABLE)                       
ADATTAB  DC    A(DATTAB)           A(DATA TABLE)                                
AWRKBUFF DC    A(WRKBUFF)          A(WORKER BUFFER)                             
AUTL     DC    A(0)                A(UTL)                                       
PQPROF   DC    V(PQPROF)                                                        
GETLOGO  DC    V(GETLOGO)                                                       
         EJECT                                                                  
DICI     DS    0X                  ** DATA DICTIONARY LIST **                   
         DCDDL AC#ACCCN,30                                                      
         DCDDL AC#AMT,15,R                                                      
         DCDDL AC#AMLOC,15,R                                                    
         DCDDL AC#ATHIR,60,C                                                    
         DCDDL AC#BACNM,30                                                      
         DCDDL AC#BATRF,6                                                       
         DCDDL AC#BNKRL,60,C                                                    
         DCDDL AC#BNKVD,60,C                                                    
         DCDDL AC#CHKC,6                                                        
         DCDDL AC#CONRP,60,C                                                    
         DCDDL AC#CPY,12                                                        
         DCDDL AC#CR,3                                                          
         DCDDL AC#CTRA,14                                                       
         DCDDL AC#DATE,8                                                        
         DCDDL AC#DUEDT,8                                                       
         DCDDL AC#ERPYD,4                                                       
         DCDDL AC#INVND,70,C                                                    
         DCDDL AC#NRTV,9                                                        
         DCDDL AC#DR,3                                                          
         DCDDL AC#BNKA,L'SDCBA                                                  
         DCDDL AC#RVRPL,60,C                                                    
         DCDDL AC#HLDFB,60,C                                                    
         DCDDL AC#INVHP,60,C                                                    
         DCDDL AC#MANCM,60,C                                                    
         DCDDL AC#MANCL,60,C                                                    
         DCDDL AC#PJCNM,30                                                      
         DCDDL AC#REF,6                                                         
         DCDDL AC#SRC,14                                                        
         DCDDL AC#STMDT,8                                                       
         DCDDL AC#SUBR,9                                                        
         DCDDL AC#SUPCN,30                                                      
         DCDDL AC#TCHTA,25                                                      
         DCDDL AC#TCHTC,25                                                      
         DCDDL AC#TAUAC,30                                                      
         DCDDL AC#TUAAC,30                                                      
         DCDDL AC#TAUCO,30                                                      
         DCDDL AC#TUACO,30                                                      
         DCDDL AC#THEAC,30                                                      
         DCDDL AC#TUHAC,30                                                      
         DCDDL AC#THELE,30                                                      
         DCDDL AC#TUHLE,30                                                      
         DCDDL AC#THECO,30                                                      
         DCDDL AC#TUHCO,30                                                      
         DCDDL AC#TREAC,30                                                      
         DCDDL AC#TURAC,30                                                      
         DCDDL AC#TRECO,30                                                      
         DCDDL AC#TURCO,30                                                      
         DCDDL AC#TVOAC,30                                                      
         DCDDL AC#TUVAC,30                                                      
         DCDDL AC#TVOCO,30                                                      
         DCDDL AC#TUVCO,30                                                      
         DCDDL AC#UATH,20                                                       
         DCDDL AC#UHELD,20                                                      
         DCDDL AC#UMTCD,20                                                      
         DCDDL AC#URECN,20                                                      
         DCDDL AC#UREVD,20                                                      
         DCDDL AC#UVODD,20                                                      
         DCDDL AC#WC,2                                                          
         DCDDL AC#RECDT,8                                                       
         DCDDL AC#CLRDT,8                                                       
         DCDDL AC#MODBY,10                                                      
DICIX    DC    X'00'                                                            
         SPACE 1                                                                
BC01     DC    C'BC01'                                                          
BL01     DC    C'BL01'                                                          
EMUOLDN  DC    C'OLDN'                                                          
PRTCLOSE DC    C'CLOSE'                                                         
WRKCLO   DC    C'CLO'                                                           
WRKADD   DC    C'ADD'                                                           
WRKFIL   DC    C'WKFILE '                                                       
ACCFIL   DC    C'ACCFIL '                                                       
CONFIL   DC    C'CTFILE '                                                       
GETREC   DC    C'GETREC '                                                       
REMLIT   DC    C'REMOTE'                                                        
REPLIT   DC    C'**AUTO**'                                                      
         SPACE 1                                                                
SORTCARD DC    C'SORT FIELDS=(1,??,A),FORMAT=BI,WORK=1 '                        
SORTRECD DC    C'RECORD TYPE=F,LENGTH=??? '                                     
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
SORTSW   DC    AL1(0)              SORT ACTIVITY SWITCH                         
CTRY     DC    XL1'00'             DEFAULT COUNTRY CODE                         
         SPACE 1                                                                
WRKBUFF  DS    4500C               BUFFER FOR WORKER                            
         EJECT                                                                  
ACTTAB   DS    0H                  ** MARKER ACTION TABLE **                    
         DC    AL1(TRSMWHQ),AL1(REPTRWPQ)                                       
         DC    AL1(TRSMBRQ),AL1(REPTRBAQ)                                       
         DC    AL1(TRSMCAQ),AL1(REPTRCRQ)                                       
         DC    AL1(TRSMCCQ),AL1(REPTRCRQ)                                       
         DC    AL1(TRSMCHQ),AL1(REPTRCRQ)                                       
         DC    AL1(TRSZOOMQ),AL1(REPTRCRQ)                                      
         DC    AL1(TRSMGRQ),AL1(REPTRGRQ)                                       
         DC    AL1(TRSMBVQ),AL1(REPTRBAQ)                                       
         DC    AL1(TRSMCMQ),AL1(REPTRCRQ)                                       
ACTTABX  DC    AL1(ACTTEOTQ)                                                    
         SPACE 1                                                                
REPTAB   DS    0H                  ** REPORT TABLE **                           
*                                  * HELD FROM BILLING LIST *                   
*&&UK                                                                           
         DC    AL1(REPTRWPQ),AL1(TRSMWHQ)                                       
         DC    AL1(REPTIDRQ+REPTIMKQ)                                           
         DC    C'MW',AL2(AC@HLDFB-DICO)                                         
         DC    AL2(AC@UHELD-DICO),AL1(L'AC@UHELD)                               
         DC    AL2(AC@THEAC-DICO,AC@TUHAC-DICO,0,0)                             
         DC    AL2(AC@THECO-DICO,AC@TUHCO-DICO)                                 
         DC    AL1(DJOB,DREF,DSUB,DDAT,DCAC,0,0,0)                              
         DC    AL1(DJOB,DREF,DSUB,DDAT,DCAC,DMRK,DAM1,0,0,0,0,0)                
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRWPQ),AL1(TRSMWHQ)                                       
         DC    AL1(REPTIDRQ+REPTIMKQ)                                           
         DC    C'MW',AL2(AC@HLDFB-DICO)                                         
         DC    AL2(AC@UHELD-DICO),AL1(L'AC@UHELD)                               
         DC    AL2(AC@THEAC-DICO,AC@TUHAC-DICO,0,0)                             
         DC    AL2(AC@THECO-DICO,AC@TUHCO-DICO)                                 
         DC    AL1(DJOB,DWRK,DCAC,DREF,DDAT,0,0,0)                              
         DC    AL1(DJOB,DWRK,DCAC,DREF,DDAT,DBRF,DMRK,DAM1,DPID,0,0,0)          
*&&                                                                             
*                                  * BANK RECONCILIATION LIST *                 
*&&UK                                                                           
         DC    AL1(REPTRBAQ),AL1(TRSMBRQ)                                       
         DC    AL1(REPTIBLQ+REPTIMKQ)                                           
         DC    C'MR',AL2(AC@BNKRL-DICO)                                         
         DC    AL2(AC@URECN-DICO),AL1(L'AC@URECN)                               
         DC    AL2(AC@TREAC-DICO,AC@TURAC-DICO,0,0)                             
         DC    AL2(AC@TRECO-DICO,AC@TURCO-DICO)                                 
         DC    AL1(DBNK,DREF,DDAT,DCAC,0,0,0,0)                                 
         DC    AL1(DBNK,DSDT,DREF,DDAT,DCAC,DMRK,DAM2,0,0,0,0,0)                
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRBAQ),AL1(TRSMBRQ)                                       
         DC    AL1(REPTIBLQ+REPTIMKQ)                                           
         DC    C'MR',AL2(AC@BNKRL-DICO)                                         
         DC    AL2(AC@URECN-DICO),AL1(L'AC@URECN)                               
         DC    AL2(AC@TREAC-DICO,AC@TURAC-DICO,0,0)                             
         DC    AL2(AC@TRECO-DICO,AC@TURCO-DICO)                                 
         DC    AL1(DBNK,DCHK,DDAT,DCAC,0,0,0,0)                                 
         DC    AL1(DBNK,DSDT,DCHK,DDAT,DCAC,DBRF,DMRK,DAM2,DCLR)                
         DC    AL1(DPID,0,0)                                                    
*&&                                                                             
*                                  * VOIDED CHEQUES (& PAYMENTS) LIST *         
*&&UK                                                                           
         DC    AL1(REPTRBAQ),AL1(TRSMBVQ)                                       
         DC    AL1(REPTIBLQ+REPTIMKQ)                                           
         DC    C'MR',AL2(AC@BNKVD-DICO)                                         
         DC    AL2(AC@UVODD-DICO),AL1(L'AC@UVODD)                               
         DC    AL2(AC@TVOAC-DICO,AC@TUVAC-DICO,0,0)                             
         DC    AL2(AC@TVOCO-DICO,AC@TUVCO-DICO)                                 
         DC    AL1(DBNK,DREF,DDAT,DCAC,0,0,0,0)                                 
         DC    AL1(DBNK,DREF,DDAT,DCAC,DMRK,DAM2,0,0,0,0,0,0)                   
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRBAQ),AL1(TRSMBVQ)                                       
         DC    AL1(REPTIBLQ+REPTIMKQ)                                           
         DC    C'MR',AL2(AC@BNKVD-DICO)                                         
         DC    AL2(AC@UVODD-DICO),AL1(L'AC@UVODD)                               
         DC    AL2(AC@TVOAC-DICO,AC@TUVAC-DICO,0,0)                             
         DC    AL2(AC@TVOCO-DICO,AC@TUVCO-DICO)                                 
         DC    AL1(DBNK,DCHK,DDAT,DCAC,0,0,0,0)                                 
         DC    AL1(DBNK,DCHK,DDAT,DCAC,DBRF,DMRK,DAM2,DPID,0,0,0,0)             
*&&                                                                             
*                                  * AUTHORISATION LIST - INVOICE REG*          
*&&UK                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSMCAQ)                                       
         DC    AL1(REPTICRQ+REPTIMKQ+REPTIFCQ)                                  
         DC    C'MA',AL2(AC@ATHIR-DICO)                                         
         DC    AL2(AC@UATH-DICO),AL1(L'AC@UATH)                                 
         DC    AL2(AC@TAUAC-DICO,AC@TUAAC-DICO,0,0)                             
         DC    AL2(AC@TAUCO-DICO,AC@TUACO-DICO)                                 
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,0,0,0)                              
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,DMRK,DAM1,DLOC,0,0,0,0)             
*                                                                               
         DC    AL1(REPTRCRQ),AL1(TRSMCAQ)                                       
         DC    AL1(REPTICRQ+REPTIMKQ)                                           
         DC    C'MA',AL2(AC@ATHIR-DICO)                                         
         DC    AL2(AC@UATH-DICO),AL1(L'AC@UATH)                                 
         DC    AL2(AC@TAUAC-DICO,AC@TUAAC-DICO,0,0)                             
         DC    AL2(AC@TAUCO-DICO,AC@TUACO-DICO)                                 
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,0,0,0)                              
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,DMRK,DAM1,0,0,0,0,0)                
*&&                                                                             
*&&US                              * MANUAL CHEQUE MATCHING LIST *              
         DC    AL1(REPTRCRQ),AL1(TRSMCAQ)                                       
         DC    AL1(REPTICRQ+REPTIMKQ)                                           
         DC    C'MA',AL2(AC@ATHIR-DICO)                                         
         DC    AL2(AC@UATH-DICO),AL1(L'AC@UATH)                                 
         DC    AL2(AC@TAUAC-DICO,AC@TUAAC-DICO,0,0)                             
         DC    AL2(AC@TAUCO-DICO,AC@TUACO-DICO)                                 
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,0,0,0)                              
         DC    AL1(DSUP,DCPJ,DREF,DSUB,DDAT,DMRK,DAM1,DPID,0,0,0,0)             
*&&                                * MANUAL CHEQUE MATCHING LIST *              
         DC    AL1(REPTRCRQ),AL1(TRSMCCQ)                                       
         DC    AL1(REPTICRQ)                                                    
         DC    C'MA',AL2(AC@MANCM-DICO)                                         
         DC    AL2(AC@UMTCD-DICO),AL1(L'AC@UMTCD)                               
         DC    6AL2(0)                                                          
         DC    AL1(DSUP,DCHQ,DREF,DSUB,DDAT,0,0,0)                              
         DC    AL1(DSUP,DCHQ,DREF,DSUB,DDAT,DMRK,DAM1,DPID,0,0,0,0)             
*                                  * MANUAL CHEQUE LIST *                       
*&&UK                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSMCMQ)                                       
         DC    AL1(REPTICRQ)                                                    
         DC    C'MA',AL2(AC@MANCL-DICO)                                         
         DC    AL2(0),AL1(0)                                                    
         DC    6AL2(0)                                                          
         DC    AL1(DSUP,DCBA,DCHQ,DDAT,0,0,0,0)                                 
         DC    AL1(DSUP,DREF,DDAT,DSUB,DCBA,DCHQ,DCDT,DAM1,0,0,0,0)             
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSMCMQ)                                       
         DC    AL1(REPTICRQ)                                                    
         DC    C'MA',AL2(AC@MANCL-DICO)                                         
         DC    AL2(0),AL1(0)                                                    
         DC    6AL2(0)                                                          
         DC    AL1(DSUP,DCBA,DCHQ,DDAT,0,0,0,0)                                 
         DC    AL1(DSUP,DREF,DDAT,DCBA,DCHQ,DCDT,DAM1,DPID,0,0,0,0)             
*&&                                                                             
*                                  * INVOICES HELD FROM PAYMENT LIST *          
*&&UK                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSMCHQ)                                       
         DC    AL1(REPTICRQ+REPTIMKQ+REPTIFCQ)                                  
         DC    C'MA',AL2(AC@INVHP-DICO)                                         
         DC    AL2(AC@UHELD-DICO),AL1(L'AC@UHELD)                               
         DC    AL2(AC@THEAC-DICO,AC@TUHAC-DICO,0,0)                             
         DC    AL2(AC@THECO-DICO,AC@TUHCO-DICO)                                 
         DC    AL1(DSUP,DREF,DSUB,DDAT,DCPJ,0,0,0)                              
         DC    AL1(DSUP,DREF,DSUB,DDAT,DCPJ,DMRK,DAM1,DLOC,0,0,0,0)             
*                                                                               
         DC    AL1(REPTRCRQ),AL1(TRSMCHQ)                                       
         DC    AL1(REPTICRQ+REPTIMKQ)                                           
         DC    C'MA',AL2(AC@INVHP-DICO)                                         
         DC    AL2(AC@UHELD-DICO),AL1(L'AC@UHELD)                               
         DC    AL2(AC@THEAC-DICO,AC@TUHAC-DICO,0,0)                             
         DC    AL2(AC@THECO-DICO,AC@TUHCO-DICO)                                 
         DC    AL1(DSUP,DREF,DSUB,DDAT,DCPJ,0,0,0)                              
         DC    AL1(DSUP,DREF,DSUB,DDAT,DCPJ,DMRK,DAM1,0,0,0,0,0)                
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSMCHQ)                                       
         DC    AL1(REPTICRQ+REPTIPLQ+REPTILGQ+REPTIMKQ)                         
         DC    C'MA',AL2(AC@INVHP-DICO)                                         
         DC    AL2(AC@UHELD-DICO),AL1(L'AC@UHELD)                               
         DC    AL2(AC@THEAC-DICO,AC@TUHAC-DICO)                                 
         DC    AL2(AC@THELE-DICO,AC@TUHLE-DICO)                                 
         DC    AL2(AC@THECO-DICO,AC@TUHCO-DICO)                                 
         DC    AL1(DSUP,DREF,DBRF,DDAT,DCPJ,0,0,0)                              
         DC    AL1(DSUP,DREF,DBRF,DDAT,DCPJ,DMRK,DAM1,DPID,0,0,0,0)             
*&&                                                                             
*               * INVOICES DUE DATE, NARRATIVE AND EARLIEST PAY DATE *          
*&&UK                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSZOOMQ)                                      
         DC    AL1(REPTICRQ)                                                    
         DC    C'MA',AL2(AC@INVND-DICO)                                         
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0,0,0,0)                                                     
         DC    AL2(0,0)                                                         
         DC    AL1(DSUP,DREF,DSUB,DDAT,DCPJ,0,0,0)                              
         DC    AL1(DSUPN,DREF,DDAT,DNAR,DDDT,DEPD,DAAL,0,0,0,0,0)               
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRCRQ),AL1(TRSZOOMQ)                                      
         DC    AL1(REPTICRQ)                                                    
         DC    C'MA',AL2(AC@INVND-DICO)                                         
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0,0)                                                         
         DC    AL2(0,0)                                                         
         DC    AL2(0,0)                                                         
         DC    AL1(DSUP,DREF,DBRF,DDAT,DCPJ,0,0,0)                              
         DC    AL1(DSUPN,DREF,DDAT,DNAR,DDDT,DAM1,DPID,0,0,0,0,0)               
*&&                                                                             
*                                  * REVERSED POSTINGS LIST *                   
*&&UK                                                                           
         DC    AL1(REPTRGRQ),AL1(TRSMGRQ)                                       
         DC    AL1(REPTIBLQ+REPTIPLQ)                                           
         DC    C'MG',AL2(AC@RVRPL-DICO)                                         
         DC    AL2(AC@UREVD-DICO),AL1(L'AC@UREVD)                               
         DC    6AL2(0)                                                          
         DC    AL1(DACT,DCAC,DWRK,DAM2,DREF,DSUB,DDAT,0)                        
         DC    AL1(DACT,DCAC,DREF,DSUB,DDAT,DMRK,DAM2,0,0,0,0,0)                
*&&                                                                             
*&&US                                                                           
         DC    AL1(REPTRGRQ),AL1(TRSMGRQ)                                       
         DC    AL1(REPTIBLQ+REPTIPLQ)                                           
         DC    C'MU',AL2(AC@RVRPL-DICO)                                         
         DC    AL2(AC@UREVD-DICO),AL1(L'AC@UREVD)                               
         DC    6AL2(0)                                                          
         DC    AL1(DACT,DCAC,DWRK,DAM2,DREF,DSUB,DBRF,0)                        
         DC    AL1(DACT,DCAC,DREF,DDAT,DBRF,DMRK,DAM2,DPID,0,0,0,0)             
*&&                                                                             
REPTABX  DC    AL1(REPTEOTQ)                                                    
         SPACE 1                                                                
DATTAB   DS    0H                  ** DATA TABLE **                             
*                                                                               
         DC    AL1(DACT,0)                                                      
         DC    AL2(SDACT-SDATA),AL1(L'SDACT,L'SDACT)                            
         DC    AL2(AC@ACCCN-DICO),AL1(L'AC@ACCCN)                               
         DC    AL2(EDACT-ACMA02),AL1(51,1)                                      
*                                                                               
         DC    AL1(DSUP,0)                                                      
         DC    AL2(SDACT-SDATA),AL1(L'SDACT,L'SDACT)                            
         DC    AL2(AC@SUPCN-DICO),AL1(L'AC@SUPCN)                               
         DC    AL2(EDACT-ACMA02),AL1(51,1)                                      
*                                                                               
         DC    AL1(DSUPN,0)                                                     
         DC    AL2(SDACT-SDATA),AL1(L'SDACT,L'SDACT)                            
         DC    AL2(AC@SUPCN-DICO),AL1(L'AC@SUPCN)                               
         DC    AL2(EDACTNR-ACMA02),AL1(36,2)                                    
*                                                                               
         DC    AL1(DBNK,0)                                                      
         DC    AL2(SDACT-SDATA),AL1(L'SDACT,L'SDACT)                            
         DC    AL2(AC@BACNM-DICO),AL1(L'AC@BACNM)                               
         DC    AL2(EDACT-ACMA02),AL1(43,1)                                      
*                                                                               
         DC    AL1(DJOB,0)                                                      
         DC    AL2(SDACT-SDATA),AL1(L'SDACT,L'SDACT)                            
         DC    AL2(AC@PJCNM-DICO),AL1(L'AC@PJCNM)                               
         DC    AL2(EDACT-ACMA02),AL1(51,1)                                      
*                                                                               
         DC    AL1(DREF,0)                                                      
         DC    AL2(SDREF-SDATA),AL1(L'SDREF,L'SDREF)                            
         DC    AL2(AC@REF-DICO),AL1(L'AC@REF)                                   
         DC    AL2(0),AL1(L'SDREF,1)                                            
*                                                                               
         DC    AL1(DDAT,0)                                                      
         DC    AL2(SDDAT-SDATA),AL1(L'SDDAT,L'SDDAT)                            
         DC    AL2(AC@DATE-DICO),AL1(L'AC@DATE)                                 
         DC    AL2(EDDATP-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DSUB,0)                                                      
         DC    AL2(SDSUB-SDATA),AL1(L'SDSUB,L'SDSUB)                            
         DC    AL2(AC@SUBR-DICO),AL1(L'AC@SUBR)                                 
         DC    AL2(0),AL1(L'SDSUB,1)                                            
*                                                                               
         DC    AL1(DCAC,0)                                                      
         DC    AL2(SDCAC-SDATA),AL1(L'SDCAC,L'SDCAC)                            
         DC    AL2(AC@CTRA-DICO),AL1(L'AC@CTRA)                                 
         DC    AL2(0),AL1(L'SDCAC,1)                                            
*                                                                               
         DC    AL1(DCPJ,0)                                                      
         DC    AL2(SDCPJ-SDATA),AL1(L'SDCPJ,L'SDCPJ)                            
         DC    AL2(AC@SRC-DICO),AL1(L'AC@SRC)                                   
         DC    AL2(0),AL1(L'SDCPJ,1)                                            
*                                                                               
         DC    AL1(DMRK,0)                                                      
         DC    AL2(SDMRK-SDATA),AL1(L'SDMRK,L'SDMRK)                            
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0),AL1(L'SDMRK,1)                                            
*                                                                               
         DC    AL1(DAM1,0)                                                      
         DC    AL2(SDAMT-SDATA),AL1(L'SDAMT,L'SDAMT)                            
         DC    AL2(AC@AMT-DICO),AL1(L'AC@AMT)                                   
         DC    AL2(EDAMT1-ACMA02),AL1(15,1)                                     
*                                                                               
         DC    AL1(DAM2,0)                                                      
         DC    AL2(SDAMT-SDATA),AL1(L'SDAMT,L'SDAMT)                            
         DC    AL2(AC@AMT-DICO),AL1(L'AC@AMT)                                   
         DC    AL2(EDAMT2-ACMA02),AL1(17,1)                                     
*                                                                               
         DC    AL1(DCHQ,DATTICHQ)                                               
         DC    AL2(SDCHQ-SDATA),AL1(L'SDCHQ,L'SDCHQ)                            
         DC    AL2(AC@CHKC-DICO),AL1(L'AC@CHKC)                                 
         DC    AL2(0),AL1(L'SDCHQ,1)                                            
*                                                                               
         DC    AL1(DCDT,DATTICHQ)                                               
         DC    AL2(SDCDT-SDATA),AL1(L'SDCDT,L'SDCDT)                            
         DC    AL2(AC@DATE-DICO),AL1(L'AC@DATE)                                 
         DC    AL2(EDDATC-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DWRK,0)                                                      
         DC    AL2(SDWRK-SDATA),AL1(L'SDWRK,L'SDWRK)                            
         DC    AL2(AC@WC-DICO),AL1(L'AC@WC)                                     
         DC    AL2(0),AL1(L'SDWRK,1)                                            
*                                                                               
         DC    AL1(DSDT,0)                                                      
         DC    AL2(SDSDT-SDATA),AL1(L'SDSDT,L'SDSDT)                            
         DC    AL2(AC@STMDT-DICO),AL1(L'AC@STMDT)                               
         DC    AL2(EDDATC-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DBRF,0)                                                      
         DC    AL2(SDBRF-SDATA),AL1(L'SDBRF,L'SDBRF)                            
         DC    AL2(AC@BATRF-DICO),AL1(L'AC@BATRF)                               
         DC    AL2(0),AL1(L'SDBRF,1)                                            
*                                                                               
         DC    AL1(DCHK,0)                                                      
         DC    AL2(SDREF-SDATA),AL1(L'SDREF,L'SDREF)                            
         DC    AL2(AC@CHKC-DICO),AL1(L'AC@CHKC)                                 
         DC    AL2(0),AL1(L'SDREF,1)                                            
*                                                                               
         DC    AL1(DCBA,0)                                                      
         DC    AL2(SDCBA-SDATA),AL1(L'SDCBA,L'SDCBA)                            
         DC    AL2(AC@BNKA-DICO),AL1(L'AC@BNKA)                                 
         DC    AL2(0),AL1(L'SDCBA,1)                                            
*                                                                               
         DC    AL1(DLOC,0)                                                      
         DC    AL2(SDCUR-SDATA),AL1(L'SDCUR+L'SDLOC,L'SDCUR+L'SDLOC)            
         DC    AL2(AC@AMLOC-DICO),AL1(L'AC@AMLOC)                               
         DC    AL2(EDLOC-ACMA02),AL1(15,1)                                      
*                                                                               
         DC    AL1(DNAR,0)                                                      
         DC    AL2(SDNAR-SDATA),AL1(L'SDNAR,L'SDNAR/4)                          
         DC    AL2(AC@NRTV-DICO),AL1(L'AC@NRTV)                                 
         DC    AL2(0),AL1(L'SDNAR/4,4)                                          
*                                                                               
         DC    AL1(DDDT,0)                                                      
         DC    AL2(SDDDT-SDATA),AL1(L'SDDDT,L'SDDDT)                            
         DC    AL2(AC@DUEDT-DICO),AL1(L'AC@DUEDT)                               
         DC    AL2(EDDATC-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DEPD,0)                                                      
         DC    AL2(SDEPD-SDATA),AL1(L'SDEPD,L'SDEPD)                            
         DC    AL2(AC@ERPYD-DICO),AL1(L'AC@ERPYD)                               
         DC    AL2(EDDATP-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DREC,0)                                                      
         DC    AL2(SRECDT-SDATA),AL1(L'SRECDT,L'SRECDT)                         
         DC    AL2(AC@RECDT-DICO),AL1(L'AC@RECDT)                               
         DC    AL2(EDDATP-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DCLR,0)                                                      
         DC    AL2(SCLRDT-SDATA),AL1(L'SCLRDT,L'SCLRDT)                         
         DC    AL2(AC@CLRDT-DICO),AL1(L'AC@CLRDT)                               
         DC    AL2(EDDATP-ACMA02),AL1(8,1)                                      
*                                                                               
         DC    AL1(DPID,0)                                                      
         DC    AL2(SDPID-SDATA),AL1(L'SDPID,L'SDPID)                            
         DC    AL2(AC@MODBY-DICO),AL1(L'AC@MODBY)                               
         DC    AL2(EDPID-ACMA02),AL1(10,1)                                      
*                                                                               
DATTABX  DC    AL1(DATTEOTQ)                                                    
         SPACE 1                                                                
CPYTAB   DC    ((X'FE'-X'40')*CPYTABL)X'00'                                     
         EJECT                                                                  
       ++INCLUDE ACOTRNTO                                                       
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
REPINDS  DS    XL1                 REPORT INDICTORS                             
REPIREM  EQU   X'80'               OUTPUT REMOTE FOR THIS REPORT                
REPICNS  EQU   X'40'               OUTPUT CONSOLIDATED REPORT                   
REPICNM  EQU   X'20'               CONSOLIDATED REPORT MODE                     
REPICHQ  EQU   X'10'               MULTICHEQUE DETAIL REQUIRED FOR SORT         
TODAYP   DS    PL3                 TODAY'S DATE (PWOS)                          
*                                                                               
DTOT     DS    H                   DISPLACEMENT INTO P TO PRINT TOTALS          
DUB2     DS    PL8                                                              
ACTCR    DS    PL8                 TOTAL ACCOUNT CREDITS                        
ACTDR    DS    PL8                 TOTAL ACCOUNT DEBITS                         
LDGCR    DS    PL8                 TOTAL LEDGER CREDITS                         
LDGDR    DS    PL8                 TOTAL LEDGER DEBITS                          
CPYCR    DS    PL8                 TOTAL COMPANY CREDITS                        
CPYDR    DS    PL8                 TOTAL COMPANY DEBITS                         
ACMKCR   DS    PL8                 TOTAL ACCOUNT CREDITS MARKED                 
ACUMCR   DS    PL8                 TOTAL ACCOUNT CREDITS UNMARKED               
ACMKDR   DS    PL8                 TOTAL ACCOUNT DEBITS MARKED                  
ACUMDR   DS    PL8                 TOTAL ACCOUNT DEBITS UNMARKED                
LEMKCR   DS    PL8                 TOTAL LEDGER CREDITS MARKED                  
LEUMCR   DS    PL8                 TOTAL LEDGER CREDITS UNMARKED                
LEMKDR   DS    PL8                 TOTAL LEDGER DEBITS MARKED                   
LEUMDR   DS    PL8                 TOTAL LEDGER DEBITS UNMARKED                 
COMKCR   DS    PL8                 TOTAL COMPANY CREDITS MARKED                 
COUMCR   DS    PL8                 TOTAL COMPANY CREDITS UNMARKED               
COMKDR   DS    PL8                 TOTAL COMPANY DEBITS MARKED                  
COUMDR   DS    PL8                 TOTAL COMPANY DEBITS UNMARKED                
NACCUMS  EQU   (*-ACTCR)/L'ACTCR                                                
*                                                                               
UIDNAME  DS    CL(L'CTORGNAM)      USER-ID NAME                                 
UIDADDR  DS    CL(L'CTORGADD)      USER-ID ADDRESS                              
UIDADD2  DS    CL(L'CTORGADD)      USER-ID ADDRESS LINE 2                       
UIDADD3  DS    CL(L'CTORGADD)      USER-ID ADDRESS LINE 3                       
UIDLOG1  DS    CL(L'CTDSTLG1)      USER-ID LOGO 1                               
UIDLOG2  DS    CL(L'CTDSTLG2)      USER-ID LOGO 2                               
UIDJOBC  DS    CL(L'CTDSTPOW)      JES JOB CODE                                 
*                                                                               
SREC     DS    0X                  ** SORT RECORD **                            
SKEY     DS    0X                  SORT KEY                                     
SKCPY    DS    XL(L'CPYKCPY)       COMPANY CODE                                 
SKCNS    DS    XL1                 1=RECORD IS FOR CONSOLIDATED REPORT          
SKUID    DS    XL(L'CPYUID)        USER-ID NUMBER                               
SKMAJ    DS    XL1                 MAJOR REPORT NUMBER                          
SKMIN    DS    XL(L'TRSMARK)       MARKER ACTION NUMBER                         
SKACT    DS    XL(L'ACTKCULA-1)    ACCOUNT CODE (ALWAYS FIRST KEY)              
SKEYC    EQU   *-SKEY              LENGTH OF SAVED KEY                          
         ORG   SKACT                                                            
SKOTH    DS    XL64                OTHER SORT KEY FIELDS                        
SKSEQ    DS    XL(L'RCVSEQNO-1)    TRANSACTION SEQUENCE NUMBER                  
SKEYL    EQU   *-SKEY              LENGTH OF SORT KEY                           
*                                                                               
SDATA    DS    0X                  SORT DATA                                    
SDALPH   DS    CL2                 ALPHA COMPANY                                
SDACT    DS    XL(L'ACTKCULA-1)    ACCOUNT CODE                                 
SDSTA    DS    XL(L'TRNSTAT)       TRANSACTION STATUS                           
SDREF    DS    CL(L'TRNREF)        TRANSACTION REFERENCE NUMBER                 
SDDAT    DS    PL(L'TRNDATE)       TRANSACTION DATE                             
SDSUB    DS    CL(L'OTHNUM)        SUB-REFERENCE NUMBER                         
SDCAC    DS    XL(L'TRNKCULC-1)    CONTRA-ACCOUNT                               
SDWRK    DS    CL(L'TRNANAL)       WORK CODE                                    
SDCPJ    DS    CL16                CLIENT/PRODUCT/JOB CODE                      
SDMRK    DS    CL1                 BLANK=ACTION, *=UNACTION                     
SDAMT    DS    PL8                 TRANSACTION AMOUNT                           
SDCUR    DS    CL3                 CURRENCY CODE (OR BINARY ZEROES)             
SDLOC    DS    PL8                 LOCAL CURRENCY AMOUNT                        
SDCHQ    DS    CL(L'MPYNO)         CHEQUE NUMBER                                
SDCDT    DS    XL(L'MPYDTE)        CHEQUE DATE                                  
SDCBA    DS    XL(L'MPYBNK)        BANK ACCOUNT                                 
SDSDT    DS    XL(L'TRSBSTDT)      BANK STATEMENT DATE                          
SDBRF    DS    XL6                 BATCH REFERENCE                              
SRECDT   DS    XL(L'GDADATE)       BANK RECONCILED DATE                         
SCLRDT   DS    XL(L'GDADATE2)      BANK CLEARED DATE                            
SDNAR    DS    CL140               NARRATIVE                                    
SDEPD    DS    PL(L'TRNDATE)       EARLIEST PAYMENT DATE                        
SDDDT    DS    XL(L'TRNRSDUE)      DUE DATE                                     
SDPID    DS    XL(L'RCVTERM)       PERSON ID RECORD                             
SDSALPH  DS    CL2                 SECURITY ALPHA COMPANY                       
SRECL    EQU   *-SREC              LENGTH OF SORT RECORD                        
*                                                                               
CHQTABN  DS    XL1                 ACTUAL NUMBER OF CHQTAB ENTRIES              
CHQTAB   DS    (CHQMAXN)XL(CHQTABL),X MANUAL CHEQUE TABLE                       
*                                                                               
LKEY     DS    XL(SKEYC)           LAST SORTED RECORD KEY                       
LHED     DS    XL(L'P)             LAST REPORT HEADLINE VALUE                   
LMRK     DS    XL1                 LAST DISPLACEMENT TO MARK QUALIFIER          
LACT     DS    CL18                MARKER NEGATIVE ACTION WORD                  
LTIT     DS    CL60                REPORT NAME                                  
LUND     DS    CL60                REPORT NAME UNDERLINE                        
LCODNAM  DS    CL51                LAST ACCOUNT CODE & NAME                     
ACODNAM  DS    AL4                 A(CODE & NAME IN PRINTLINE)                  
SAVPROD  DS    CL2                 SAVED PRODUCTION U/L CODE                    
SAVBTYP  DS    XL1                 SAVED TRANSACTION BATCH TYPE                 
*                                                                               
WRKID    DS    0XL16               ** WORKER KEY **                             
WRKIUID  DS    XL2                 USER-ID NUMBER                               
WRKISYS  DS    CL1                 SYSTEM                                       
WRKISYSQ EQU   C'A'                                                             
WRKIPRG  DS    CL2                 PROGRAM CODE                                 
         DS    XL1                 N/D                                          
WRKIDAY  DS    PL1                 DAY (PWOS)                                   
WRKITYP  DS    CL1                 WORKER FILE TYPE                             
WRKITYPQ EQU   C'O'                                                             
         DS    XL1                 EXTRA INFO                                   
         DS    XL1                 N/D                                          
WRKIFSN  DS    XL2                 FILE SEQ NUMBER WITHIN USER ID               
         DS    XL1                 FILE STATUS                                  
WRKIFLG  DS    XL1                 FLAG VALUES                                  
         DS    XL2                 TTTT OF FIRST CI                             
*                                                                               
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
*                                                                               
DICO     DS    0C                  ** DICTIONARY WORDS **                       
         DSDDL PRINT=YES                                                        
IO1      DS    2100X               I/O AREA                                     
IO2      DS    2100X               SAVED COPY RECORD                            
IO3      DS    2100X               SAVED COPY RECORD                            
         EJECT                                                                  
CPYTABD  DSECT                     ** COMPANY TABLE **                          
CPYTEOTQ EQU   0                   END OF TABLE INDICATOR                       
CPYTCPY  DS    XL(L'CPYKCPY)       COMPANY CODE                                 
CPYTUID  DS    XL(L'CPYUID)        PRINCIPAL USER-ID NUMBER                     
CPYTSTA1 DS    XL1                 STATUS BYTE 1                                
CPYTSTA2 DS    XL1                 STATUS BYTE 2                                
CPYTSTA3 DS    XL1                 STATUS BYTE 3                                
CPYTSTA4 DS    XL1                 STATUS BYTE 4                                
CPYTSTA5 DS    XL1                 STATUS BYTE 5                                
CPYTSTA6 DS    XL1                 STATUS BYTE 6                                
CPYTSTA7 DS    XL1                 STATUS BYTE 7                                
CPYTSTA8 DS    XL1                 STATUS BYTE 8                                
CPYTALPH DS    XL(L'CPYALPHA)      ALPHA-ID                                     
CPYSALPH DS    XL(L'CPYALPHA)      SECURITY ALPHA-ID                            
CPYTINDS DS    XL1                 COMPANY INDICATORS                           
CPYTIMUQ EQU   X'01'               MIXED USER-IDS IN RECOVERY FILE              
CPYTLOGO DS    XL(L'CPYLOGO)       COMPANY LOGO                                 
CPYTNAME DS    XL(L'NAMEREC)       COMPANY NAME                                 
CPYTADDR DS    XL(L'ADRADD1)       COMPANY ADDRESS LINE 1                       
CPYPRDAL DS    XL1                 PRODUCTION LEDGER L'CLIENT                   
CPYPRDBL DS    XL1                                   L'PRODUCT                  
CPYPRDCL DS    XL1                                   L'JOB                      
CPYTCTRY DS    XL1                 COMPANY COUNTRY CODE                         
CPYTABL  EQU   *-CPYTABD                                                        
         SPACE 1                                                                
ACTTABD  DSECT                     ** MARKER ACTION TABLE **                    
ACTTEOTQ EQU   0                   END OF TABLE INDICATOR                       
ACTTACT  DS    XL1                 MARKER ACTION (SEE TRSMARK)                  
ACTTREP  DS    XL1                 MAJOR REPORT NUMBER                          
ACTTABL  EQU   *-ACTTABD                                                        
         SPACE 1                                                                
DATTABD  DSECT                     ** DATA TABLE **                             
DATTEOTQ EQU   0                   END OF TABLE INDICATOR                       
DATTDAT  DS    XL1                 DATA NUMBER                                  
DSUP     EQU   1                   SUPPLIER ACCOUNT                             
DBNK     EQU   2                   BANK ACCOUNT                                 
DJOB     EQU   3                   PRODUCTION JOB ACCOUNT                       
DREF     EQU   4                   REFERENCE NUMBER                             
DDAT     EQU   5                   TRANSACTION DATE (PWOS)                      
DSUB     EQU   6                   SUB-REFERENCE NUMBER                         
DCAC     EQU   7                   CONTRA-ACCOUNT (ULACCOUNT)                   
DCPJ     EQU   8                   CLI/PRO/JOB OR EXPENSE ACCOUNT               
DMRK     EQU   9                   ACTION QUALIFIER                             
DAM1     EQU   10                  TRANSACTION AMOUNT                           
DAM2     EQU   11                  TRANSACTION AMOUNT                           
DCHQ     EQU   12                  CHEQUE NUMBER                                
DCDT     EQU   13                  CHEQUE DATE (COMPRESSED)                     
DWRK     EQU   14                  WORK CODE                                    
DACT     EQU   15                  ACCOUNT                                      
DSDT     EQU   16                  BANK STATEMENT DATE (COMPRESSED)             
DBRF     EQU   17                  BATCH REFERENCE                              
DCHK     EQU   18                  CHECK# AKA REFERENCE                         
DCBA     EQU   19                  BANK ACCOUNT CODE                            
DLOC     EQU   20                  LOCAL CURRENCY AMOUNT                        
DNAR     EQU   21                  NARRATIVE                                    
DEPD     EQU   22                  EARLIEST PAYMENT DATE                        
DDDT     EQU   23                  DUE DATE                                     
DSUPN    EQU   24                  SUPPLIER ACCOUNT NARROW FORMAT               
DREC     EQU   25                  RECONCILED DATE                              
DCLR     EQU   26                  CLEARED DATE                                 
DPID     EQU   27                  PERSON ID CODE                               
DATTINDS DS    XL1                 DATA TYPE INDICATORS                         
DATTICHQ EQU   X'80'               DATA FROM CHQTAB                             
DATTDISP DS    AL2                 DISPLACEMENT TO DATA IN SREC                 
DATTDLEN DS    AL1                 LENGTH OF DATA IN SDREC                      
DATTDLEO DS    AL1                 LENGTH OF DATA FOR OUTPUT                    
DATTHEAD DS    AL2                 DISPLACEMENT OF HEADING IN DICO              
DATTHLEN DS    AL1                 LENGTH OF HEADING                            
DATTOEDT DS    AL2                 DISPLACEMENT OF EDIT ROUTINE                 
DATTOLEN DS    AL1                 WIDTH OF OUTPUT COLUMN                       
DATTNLIN DS    AL1                 NUMBER OF LINES                              
DATTABL  EQU   *-DATTABD                                                        
         SPACE 1                                                                
REPTABD  DSECT                     ** REPORT TABLE **                           
REPTEOTQ EQU   0                   END OF TABLE INDICATOR                       
REPTREP  DS    XL1                 MAJOR REPORT NUMBER                          
REPTRCRQ EQU   1                   CREDITOR REPORT                              
REPTRBAQ EQU   2                   BANK REPORT                                  
REPTRWPQ EQU   3                   WIP (PRODUCTION)                             
REPTRGRQ EQU   4                   GENERAL REVERSE                              
REPTACT  DS    XL1                 MARKER ACTION                                
REPTIPRT DS    XL1                 REPORT INDICATORS                            
REPTICRQ EQU   X'80'               PRINT TOTAL CREDITS                          
REPTIDRQ EQU   X'40'               PRINT TOTAL DEBITS                           
REPTIBLQ EQU   X'20'               PRINT BALANCE (DEBITS-CREDITS)               
REPTIPLQ EQU   X'10'               NEW PAGE ON CHANGE OF LEDGER                 
REPTIMKQ EQU   X'08'               PRINT TOTAL MARKED/UNMARKED                  
REPTILGQ EQU   X'04'               PRINT TOTALS FOR LEDGER                      
REPTIFCQ EQU   X'02'               FOREIGN CURRENCY REPORT                      
REPTCODE DS    CL2                 REPORT CODE                                  
REPTNAME DS    AL2                 DISPLACEMENT TO REPORT NAME                  
REPTMACT DS    AL2                 DISPLACEMENT TO NEGATIVE ACTION WORD         
REPTMLEN DS    AL1                 LENGTH OF NEGATIVE ACTION WORD               
REPTTMAC DS    AL2                 DISP TO TOTAL MARKED FOR ACC DD EXPR         
REPTTUAC DS    AL2                 DISP TO TOTAL UNMRKD FOR ACC DD EXPR         
REPTTMLE DS    AL2                 DISP TO TOTAL MARKED FOR LGR DD EXPR         
REPTTULE DS    AL2                 DISP TO TOTAL UNMRKD FOR LGR DD EXPR         
REPTTMCO DS    AL2                 DISP TO TOTAL MARKED FOR CPY DD EXPR         
REPTTUCO DS    AL2                 DISP TO TOTAL UNMRKD FOR CPY DD EXPR         
REPTSORT DS    XL8                 SORT KEY VALUES                              
REPTPRNT DS    XL12                PRINT VALUES                                 
REPTABL  EQU   *-REPTABD                                                        
*                                                                               
WRKRECD  DSECT                     ** WORKER RECORD **                          
WRKRLEN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 N/D                                          
WRKRCPY  DS    XL1                 COMPANY CODE                                 
WRKRUID  DS    XL2                 USER-ID NUMBER                               
WRKRLOGO DS    CL(L'UIDLOG1)       LOGO VALUE                                   
WRKRLENQ EQU   *-WRKRECD                                                        
*                                                                               
CHQTABD  DSECT                     ** MANUAL CHEQUE TABLE **                    
CHQTEOTQ EQU   X'00'               END OF TABLE INDICATOR                       
CHQTCHQ  DS    CL(L'MPYNO)         CHEQUE NUMBER                                
CHQTDAT  DS    XL(L'MPYDTE)        CHEQUE DATE                                  
CHQTAMT  DS    PL(L'MPYPART)       AMOUNT (FULL OR PARTIAL)                     
CHQTINDS DS    XL1                 INDICATOR BYTE                               
CHQTIPRT EQU   X'80'               CHQTAMT IS A MPYPART NOT MPYAMNT             
CHQMAXN  EQU   6                   MAXIMUM NUMBER OF TABLE ENTRIES              
CHQTABL  EQU   *-CHQTABD                                                        
         EJECT                                                                  
* DMWRKRK                          WORKER FILE INDEX DSECT                      
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDCURTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCURTABD                                                      
         ORG   CURTLONG+L'CURTLONG                                              
CURTABLQ EQU   *-CURTABD                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043ACREPMA02 03/09/17'                                      
         END                                                                    
