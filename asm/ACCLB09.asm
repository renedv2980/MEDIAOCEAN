*          DATA SET ACCLB09    AT LEVEL 053 AS OF 08/16/00                      
*PHASE T62109A                                                                  
CLB09    TITLE '- BILL PROGRAM - INSERT EXTRA BILLING ITEM'                     
CLB09    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLB9**,R8,R7,R6,CLEAR=YES,RR=RE                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING IWORKD,RC                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         LA    R2,IWORKD                                                        
         LA    R3,IWORKL                                                        
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         CLC   TWASCRN,CSSCRN                                                   
         BE    VAL10                                                            
         GOTO1 AOVRSCR,BOPARM,(CSSCRN,BASOLAYH)                                 
         BE    INI10                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY CLIENT PRODUCT AND JOB                         *         
***********************************************************************         
         SPACE 1                                                                
INI10    MVC   INSCLIC,BCCLICOD    CLIENT CODE                                  
         MVC   INSCLIN,BCCLINAM    CLIENT NAME                                  
         XC    SINSAVE,SINSAVE     CLEAR SAVED DATA                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    R1,BCPROCOD(RE)                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   INSPROC(0),0(R1)    PRODUCT CODE                                 
         MVC   INSPRON,BCPRONAM    PRODUCT NAME                                 
*                                                                               
         IC    RE,BCPROLEN                                                      
         LA    R1,BCJOBCOD(RE)                                                  
         IC    RF,BCJOBLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   INSJOBC(0),0(R1)    JOB CODE                                     
         MVC   INSJOBN,BCJOBNAM    JOB NAME                                     
*                                                                               
         LA    R2,INSSUPCH                                                      
         ST    R2,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE ITEM DETAILS                                  *         
***********************************************************************         
         SPACE 1                                                                
VAL10    LA    R1,INSSUPCH         TEST ANY SCREEN INPUT                        
         XR    RF,RF                                                            
VAL12    ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    VAL14                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+12                                                             
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BZ    VAL15                                                            
         BXH   R1,RF,VAL12                                                      
VAL14    L     RF,AINP             KEEP CURSOR & MESSAGE AS THEY WERE           
         USING TIOBD,RF                                                         
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         DROP  RF                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     EXIT                                                             
*                                                                               
VAL15    MVI   IKEYINP,C'N'        SET NO KEY CHANGE                            
         GOTO1 AFVAL,INSSUPCH                                                   
         BE    VAL16                                                            
         MVC   SINSUPP(L'CPYPROD),BCCPYEL+(CPYPROD-CPYELD)                      
         MVC   SINSUPP+L'CPYPROD(L'BCJOBCOD),BCJOBCOD                           
         MVI   INSSUPC,C'*'                                                     
         MVC   INSSUPC+1(L'SINSUPP),SINSUPP                                     
         OI    INSSUPCH+FVOIND-FVIHDR,FVOXMT                                    
         MVC   SINSUPN,BCJOBNAM                                                 
         MVC   INSSUPN,SINSUPN                                                  
         OI    INSSUPNH+FVOIND-FVIHDR,FVOXMT                                    
         B     VAL20                                                            
VAL16    TM    FVIIND,FVIVAL                                                    
         BO    VAL20                                                            
         OI    INSSUPCH+FVOIND-FVIHDR,FVOXMT                                    
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         LA    RF,IOKEY                                                         
         USING ACTRECD,RF                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'S'        DEFAULT UNIT/LEDGER IS SV                    
         MVI   ACTKLDG,C'V'                                                     
         MVC   ACTKACT,FVIFLD                                                   
         CLI   FVIFLD,C'*'         INDICATES UNIT/LEDGER OVERRIDDEN             
         BNE   VAL18                                                            
         MVC   ACTKUNT,FVIFLD+1                                                 
         MVC   ACTKLDG,FVIFLD+2                                                 
         MVC   ACTKACT,FVIFLD+3                                                 
         DROP  RF                                                               
VAL18    GOTO1 AGETACT,0                                                        
         BNE   EXITN                                                            
         TM    ACBSTAT,ACBSABAL                                                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)                                           
         B     EXITN                                                            
         TM    ACBSTAT,ACBSCLSE+ACBSLOCK                                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         B     EXITN                                                            
         MVC   INSSUPN,ACNAME                                                   
         MVC   SINSUPP,ACCODE+1                                                 
         MVC   SINSUPN,ACNAME                                                   
         OI    INSSUPNH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL20    MVI   FVMINL,1            WORKCODE - REQUIRED INPUT                    
         GOTO1 AFVAL,INSWCH                                                     
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL30                                                            
         CLC   FVIFLD(2),=C'99'                                                 
         BE    *+14                                                             
         CLC   FVIFLD(2),=C'**'                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITN                                                            
         OI    INSWCH+FVOIND-FVIHDR,FVOXMT                                      
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         GOTO1 AGETWRK,FVIFLD                                                   
         BNE   EXITN                                                            
         MVC   SINWC,FVIFLD                                                     
         MVC   INSWCN,BOWORK1                                                   
         OI    INSWCNH+FVOIND-FVIHDR,FVOXMT                                     
*                                                                               
VAL30    MVI   FVMINL,1            REFERENCE - REQUIRED INPUT                   
         GOTO1 AFVAL,INSREFH                                                    
         BNE   EXIT                                                             
         TM    FVIIND,FVIVAL                                                    
         BO    VAL40                                                            
         OI    INSREFH+FVOIND-FVIHDR,FVOXMT                                     
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         MVC   SINREF,FVIFLD                                                    
*                                                                               
VAL40    MVI   FVMINL,1            DATE - REQUIRED INPUT                        
         GOTO1 AFVAL,INSDATEH                                                   
         BE    VAL45                                                            
*        CLI   CSACT,ACTFEE                                                     
*        BNE   EXITN                                                            
*        MVC   SINDATE,BCTODAYP    IF NO DATE FOR FEE USE TODAY                 
*        GOTO1 VDATCON,BCPARM,(1,SINDATE),(17,INSDATE)                          
*        OI    INSDATEH+FVOIND-FVIHDR,FVOXMT                                    
*        B     VAL50                                                            
*                                                                               
VAL45    TM    FVIIND,FVIVAL                                                    
         BO    VAL50                                                            
         MVI   IKEYINP,C'Y'        THERE IS A KEY CHANGE                        
         GOTO1 AVALDAT                                                          
         BNE   EXITN                                                            
         MVC   SINDATE,BCWORK+2                                                 
*                                                                               
VAL50    ZAP   SINNET,BCPZERO                                                   
         ZAP   SINCOM,BCPZERO                                                   
         MVI   FVMINL,0            TEST IF ANY COMM INPUT                       
         GOTO1 AFVAL,INSALCMH                                                   
         BNL   *+8                 YES                                          
         MVI   FVMINL,1            ELSE NET IS REQUIRED INPUT                   
         GOTO1 AFVAL,INSALNTH                                                   
         BH    EXITN                                                            
         BL    VAL55                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN               INPUT MUST BE STRAIGHT CASH                  
         ZAP   SINNET,BODMCB+4(8)                                               
         BNZ   *+8                                                              
VAL55    MVI   FVMINL,1            IF NET ZERO THEN COMM REQUIRED               
         CURED SINNET,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
         GOTO1 AFVAL,INSALCMH                                                   
         BNE   VAL60                                                            
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         MVC   BOBYTE1,CSCURBIL+(CURTDECP-CURTABD)                              
         OI    BOBYTE1,X'80'                                                    
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(BOBYTE1,FVIFLD),(X'40',(RF))                    
         CLI   0(R1),X'FF'                                                      
         BE    EXITN                                                            
         ZAP   SINCOM,BODMCB+4(8)                                               
         BNZ   *+8                                                              
         MVI   FVMINL,1                                                         
         CURED SINCOM,(14,INSALCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALCMH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
VAL60    MVC   SINNAR,BCSPACES                                                  
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR1H                                                   
         LA    R4,SINNAR                                                        
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VAL65                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR(0),FVIFLD                                                 
         LA    RF,1(RF)                                                         
         STC   RF,SINNARL          SET NARRATIVE LENGTH                         
VAL65    MVI   FVMINL,0                                                         
         GOTO1 AFVAL,INSNAR2H                                                   
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BE    ADD10                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SINNAR+L'INSNAR1(0),FVIFLD                                       
         LA    RF,L'INSNAR1+1(RF)                                               
         STC   RF,SINNARL                                                       
         B     ADD10                                                            
         EJECT                                                                  
***********************************************************************         
*              ADD DRAFT TRANSACTION RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
ADD10    CLI   IKEYINP,C'Y'        TEST IF RECORD ALREADY THERE                 
         BNE   CHA10               YES - READ AND UPDATE IT                     
         L     R4,AIO3             BUILD RECORD IN IO3                          
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),BCCPYEL+(CPYPROD-CPYELD)            
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,SINWC                                                   
         MVC   TRNKCCPY,CUABIN                                                  
         MVC   TRNKULC,SINSUPP                                                  
         MVC   TRNKDATE,SINDATE                                                 
         MVC   TRNKREF,SINREF                                                   
*                                                                               
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,SINDATE                                                  
         MVC   TRNREF,SINREF                                                    
         MVI   TRNTYPE,99          ***** TEMP *****                             
         MVC   TRNMOS,BCTMON       SET TODAYS MOA                               
         MVC   TRNBREF,BCSPACES    SET NULL BATCH REFERENCE                     
         MVI   TRNSTAT,TRNSDR+TRNSAUTH                                          
         MVC   TRNANAL,SINWC                                                    
         ZAP   TRNAMNT,BCPZERO                                                  
         SR    RE,RE                                                            
         ICM   RE,1,SINNARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SINNAR                                                
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LA    RF,TRNEL(RE)                                                     
         MVI   0(RF),0             SET E-O-R                                    
         SR    RF,R4                                                            
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
ADD30    L     R4,AADTBLK                                                       
         USING ADDTRND,R4                                                       
         GOTO1 AINIADT                                                          
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         MVC   TRNCACNM,SINSUPN    CONTRA-ACCOUNT NAME                          
         GOTO1 VADDTRN,ADDTRND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNINDS,TRNILAST                                                 
         GOTO1 VADDTRN,ADDTRND                                                  
         L     RE,AIO3                                                          
         AH    RE,=Y(2000-4)                                                    
         MVC   SINDA,0(RE)         SAVE DISK ADDRESS OF TRANSACTION             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              INSERT NEW ITEM IN THE LIST                            *         
***********************************************************************         
         SPACE 1                                                                
INS10    SR    RE,RE               TEST WHETHER WE CAME FROM LIST               
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   IRECACT,0(RE)                                                    
         CLI   IACT,ACTSET                                                      
         BE    INS40                                                            
*                                                                               
         XC    IIOKEY,IIOKEY                                                    
         L     RE,AIO3                                                          
I        USING TRNRECD,IIOKEY                                                   
         MVC   I.TRNKEY,0(RE)                                                   
         MVC   I.TRNKSTA,TRNRSTA-TRNKEY(RE)                                     
         MVC   I.TRNKDA,SINDA                                                   
         DROP  I                                                                
         GOTO1 ASETTRN,BOPARM,(C'S',IIOKEY),AIO3,LSPRATA                        
         IC    RE,TWASESNL                                                      
         BCTR  RE,0                                                             
         STC   RE,TLKSES                                                        
I        USING TLSTD,ILST                                                       
         MVC   I.TLKEY,TLKEY                                                    
         MVC   I.TLKSEQ,BCEFFS     GET RECORD AFTER INSERTION                   
         GOTO1 ATSARIO,BOPARM,('TSARDH',I.TLSTD)                                
         BL    *+10                END-OF-FILE                                  
         BH    INS12                                                            
         DC    H'0'                THIS KEY SHOULD NOT EXIST                    
         MVC   I.TLNUM,CSHIRECN                                                 
         B     INS14                                                            
INS12    SR    RE,RE                                                            
         ICM   RE,3,I.TLNUM                                                     
         BCT   RE,*+4                                                           
         B     INS16                                                            
         STCM  RE,3,I.TLNUM                                                     
*                                                                               
INS14    GOTO1 ATSARIO,BOPARM,('TSAGET',I.TLSTD)  GET 'BEFORE' RECORD           
         CLC   I.TLKEY(TLKSEQ-TLKEY),TLKEY                                      
         BNE   *+10                                                             
         MVC   TLKSEQ,I.TLKSEQ                                                  
         ICM   RE,3,TLKSEQ         INCREMENT SEQUENCE NUMBER                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TLKSEQ                                                      
INS16    MVC   TLRLEN,LSMIXLST+(MIXLRECL-MIXLST)                                
         MVC   TLDA,SINDA                                                       
         L     RF,AIO3                                                          
         MVC   TLSTA,TRNRSTA-TRNRECD(RF)                                        
         MVC   TLRECACT,IRECACT                                                 
         GOTO1 ATSARIO,TSAADD                                                   
*                                                                               
INS40    MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,0,LSPRATA                        
         L     RF,0(R1)                                                         
         USING JCBELD,RF                                                        
         ICM   RE,3,JCBADV         INCREMENT COUNT OF ADVANCE BILLS             
         LA    RE,1(RE)                                                         
         STCM  RE,3,JCBADV                                                      
         DROP  RF                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AI$RECAD)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,INSSUPCH         POSITION CURSOR TO SUPPLIER                  
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHANGE THE ITEM CURRENTLY DISPLAYED (JUST ADDED)       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
CHA10    MVC   IODAOVER,SINDA      SET RECORD DISK ADDRESS                      
         GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3                                                          
         USING TRNELD,R3                                                        
         XC    BOELEM,BOELEM                                                    
         LA    R3,BOELEM                                                        
         SR    RE,RE                                                            
         IC    RE,TRNRFST+(TRNLN-TRNELD)                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNELD(0),TRNRFST                                                
         ICM   RE,1,SINNARL                                                     
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TRNNARR(0),SINNAR                                                
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('TRNELQ',TRNRECD),0                 
         MVI   TRNEL,1             ENSURE THIS IS FIRST ELEMENT                 
         GOTO1 (RF),(R1),(C'P',ACCMST),TRNRECD,TRNELD                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNRFST,TRNELQ                                                   
         BAS   RE,UPDTRN                                                        
         BNE   EXITN                                                            
*                                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDJOB,BOPARM,(C'U',AIO3),AIO1,LSPRATAS,LSPRATA                 
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AI$RECCH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,INSSUPCH         POSITION CURSOR TO SUPPLIER                  
         ST    R1,FVADDR                                                        
         SR    RF,RF               SET ALL FIELDS VALID                         
         ICM   RF,1,FVTLEN-FVIHDR(R1)                                           
         BZ    *+12                                                             
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              UPDATE TRANSACTION PTAEL AND DISPLAY AMOUNTS           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R4                                                       
         USING TRNELD,R3                                                        
UPDTRN   NTR1                                                                   
         GOTO1 AGETOPT,BODMCB,AIO3                                              
         SR    R0,R0                                                            
         CLC   CSCPYCUR,CSBILCUR                                                
         BE    *+8                                                              
         LA    R0,CSEXCVAL                                                      
         GOTO1 VPRORATA,BODMCB,TRNRECD,AGOPBLK,ACOM,(R0),LSPRATA                
         LA    RE,LSPRATAS                                                      
         LA    RF,PR$LNQ                                                        
         LA    R0,LSPRATA                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
*        CLI   CSACT,ACTFEE        TEST IF FEE FEE (ALL COMM & NO NET)          
*        BNE   UPD10                                                            
*        ZAP   SINCOM,SINNET       SET COMMISSION FROM NET INPUT FLD            
*        ZAP   SINNET,BCPZERO      ENSURE NET IS SET TO ZERO                    
*        B     UPD20                                                            
*                                                                               
UPD10    MVI   FVMINL,0            TEST IF ANY COMM INPUT                       
         GOTO1 AFVAL,INSALCMH                                                   
         BNL   UPD20               YES - SINCOM CONTAINS COMMISSION             
*                                                                               
         L     RF,AGOPBLK          NO - CALC. IT ON ALLOCATED NET AMT           
         ZAP   BOPL81(16),SINNET                                                
         SRP   BOPL81(16),2,0                                                   
         MP    BOPL81(16),GOAGYCOM-GOBLOCKD(L'GOAGYCOM,RF)                      
         SRP   BOPL81(16),64-8,5                                                
         ZAP   SINCOM,BOPL81(16)                                                
*                                                                               
UPD20    GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               (1,SINNET),0                                                     
         BE    UPD22                                                            
         LA    RE,INSALNTH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
UPD22    GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),   X        
               0,(1,SINCOM)                                                     
         BE    *+16                                                             
         LA    RE,INSALCMH                                                      
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
*PD20    GOTO1 AALLTRN,BODMCB,('PTATRAL',AIO3),('PTASCASH',LSPRATA),            
*              (1,SINNET),(1,SINCOM)                                            
*        BNE   EXITN                                                            
*        CLI   CSACT,ACTFEE                                                     
*        BNE   UPD30                                                            
*        CURED SINCOM,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,               
*              ZERO=NOBLANK,DMCB=BODMCB                                         
*        OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
*        B     UPD40                                                            
*                                                                               
UPD30    CURED SINNET,(14,INSALNT),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALNTH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
         CURED SINCOM,(14,INSALCM),CSCURBIL,ALIGN=LEFT,MINUS=YES,      X        
               ZERO=NOBLANK,DMCB=BODMCB                                         
         OI    INSALCMH+FVOIND-FVIHDR,FVOXMT                                    
*                                                                               
UPD40    B     EXITY                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*              EXIT POINTS ETC                                        *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              LITERAL POOL/CONSTANTS                                 *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
ACCMST   DC    C'ACCMST  '                                                      
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              DSECTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
IWORKD   DSECT                                                                  
ILST     DS    XL(L'TLST)                                                       
IKEYINP  DS    CL1                                                              
IRECACT  DS    0XL2                                                             
IREC     DS    XL1                                                              
IACT     DS    XL1                                                              
IIOKEY   DS    XL(L'IOKEY)                                                      
IWORKL   EQU   *-IWORKD                                                         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF9D                                                       
         ORG   OSVALS                                                           
SINSAVE  DS    0XL(SINSAVL)                                                     
SINSUPP  DS    CL14                SUPPLIER CODE                                
SINSUPN  DS    CL36                SUPPLIER NAME                                
SINWC    DS    CL2                 WORKCODE                                     
SINREF   DS    CL6                 REFERENCE                                    
SINDATE  DS    PL3                 DATE                                         
SINNET   DS    PL8                 NET ALLOCATION                               
SINCOM   DS    PL8                 COMM ALLOCATION                              
SINNARL  DS    CL1                                                              
SINNAR   DS    CL(L'INSNAR1+L'INSNAR2)                                          
SINDA    DS    CL4                 LAST INSERTED XACTN D/A                      
SINSAVL  EQU   *-SINSUPP                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053ACCLB09   08/16/00'                                      
         END                                                                    
