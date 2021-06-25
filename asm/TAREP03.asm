*          DATA SET TAREP03    AT LEVEL 060 AS OF 03/18/15                      
*PHASE T70303B,*                                                                
         TITLE 'T70303 - TALENT BILLING - VALIDATE SCREEN'                      
T70303   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70303                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING BILLD,R7            BILL DSECT                                   
         L     RA,ATWA                                                          
         USING T703FFD,RA          SCREEN                                       
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         CLI   MODE,VALKEY                                                      
         BE    VREC                                                             
         B     XIT                                                              
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              VALIDATE RECORD                                                  
*                                                                               
VREC     DS    0H                                                               
         LH    RF,=AL2(TBLNQ)      CLEAR STORAGE                                
         XCEFL TBILLD,(RF)                                                      
         MVC   TBTODAY,TGTODAY1    TODAY'S DATE PWOS                            
*                                                                               
         CLI   RECNUM,BI           IF BILLING REPORT                            
         BE    *+12                                                             
         CLI   RECNUM,PB           OR PRINT BILLING                             
         BNE   VREC30                                                           
         CLI   TGCTSTAF,0          AND REQUESTED ONLINE (PAN REQUESTS           
         BE    VREC30                            DON'T HAVE STAFF IDS)          
         TM    WHEN,X'20'          AND NOT RUNNING SOON                         
         BNZ   VREC30                                                           
         BAS   RE,RERUN            SET THIS TO BE A RERUN                       
*                                                                               
VREC30   CLI   RECNUM,UN           UNION REPORT                                 
         BE    VREC100                                                          
         LA    R2,SBIAGYH          AGENCY                                       
         LA    R3,SBIAGYNH         AGENCY NAME                                  
         BAS   RE,VALAGY           VALIDATE AGENCY                              
*                                                                               
         BAS   RE,VALOFF           VALIDATE OFFICE NUMBERS                      
*                                                                               
         BAS   RE,VALBPER          VALIDATE BILL DATE                           
*                                                                               
VREC40   LA    R2,SBIFINVH                                                      
         LA    R3,SBILINVH                                                      
         LA    R4,SBIAGYH                                                       
         BAS   RE,VALINV           VALIDATE INVOICE NUMBER                      
*                                                                               
         LA    R2,SBICIDH                                                       
         LA    R3,SBIAGYH                                                       
         LA    R4,SBIPERH          BILLING PERIOD REQUIRED                      
         BAS   RE,VALCID           VALIDATE COMMERCIAL                          
*                                                                               
******** TM    WHEN,X'20'          IF RUNNING SOON,                             
******** BZ    VREC45                                                           
******** CLI   RECNUM,BI           AND BILLING REPORT                           
******** BNE   VREC45                                                           
******** MVC   QCRDCODE,=C'B2'     DEFAULT JCL                                  
VREC45   BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
         CLI   RECNUM,AC           IF THIS IS ACOPY REPORT                      
         BE    VREC50                                                           
         CLI   RECNUM,EB           OR THIS IS EBILL                             
         BE    VREC50                                                           
         CLI   RECNUM,CC           OR THIS IS CCOPY                             
         BE    VREC50                                                           
         CLI   RECNUM,SC           OR THIS IS SCOPY                             
         BE    VREC50                                                           
         CLI   RECNUM,CR           OR THIS IS CRCOPY                            
         BE    VREC50                                                           
         CLI   RECNUM,DA           OR THIS IS DMACOPY                           
         BNE   VREC60                                                           
*                                                                               
VREC50   BAS   RE,RERUN            TREAT IT AS A RERUN                          
*                                                                               
VREC60   LA    R1,SBIFINVH                                                      
         LA    R3,SBICIDH                                                       
         B     VREC110                                                          
*                                                                               
VREC100  BAS   RE,VALUNI           HANDLE UNION COPIES                          
         LA    R1,SUNFINVH                                                      
         LA    R3,SUNUCIDH                                                      
*                                                                               
VREC110  DS    0H                                                               
*===============================*                                               
*  TEST TO MAKE EBILL INVISIBLE *                                               
*===============================*                                               
         CLI   RECNUM,EB           OR THIS IS EBILL                             
         BNE   VREC200                                                          
         L     R1,TWAMASTC         MASTC                                        
         USING MASTD,R1                                                         
         L     R1,MCVREMOT                                                      
         USING REMOTED,R1                                                       
         OC    REMOTDST,REMOTDST   DEST TO PRINT QUEUE?                         
         BZ    *+8                 NO, DON'T MAKE INVISIBLE                     
         OI    REMOTSTA,X'02'      EBILL INVISIBLE UNTIL REPORT DONE            
         DROP  R1                                                               
*===============================*                                               
*  TEST TO MAKE EBILL INVISIBLE *                                               
*===============================*                                               
VREC200  CLI   WHEN,X'20'          ONLY ALLOW SOON OPTION                       
         BNE   VREC210                                                          
         LA    R2,CONWHENH                                                      
         CLI   5(R1),0             FOR INVOICE NUMBERS                          
         BNE   VREC210                                                          
         CLI   5(R3),0             OR COMMERCIAL REQUESTS                       
         BE    INVERR                                                           
*                                                                               
VREC210  TM    TBOPTS2,TBREGRSN+TBREGRS2    IF RUNNING REG OR REG2              
         BZ    VRECX                                                            
         CLI   TWAWRITE,C'N'       IF WRITE = YES,                              
         BE    VRECX                                                            
         BAS   RE,CKSYS            MAKE SURE SYSTEM IS FQA OR CSC               
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        HANDLE UNION COPIES                                                    
*                                                                               
VALUNI   NTR1                                                                   
         MVI   UNICOPY,C'Y'        SET UNION COPY FLAG                          
         BAS   RE,RERUN            & RERUN FLAGS                                
         LA    R2,SUNAGYH          AGENCY                                       
         LA    R3,SUNAGYNH         AGENCY NAME                                  
         BAS   RE,VALAGY           VALIDATE AGENCY                              
*                                                                               
         BAS   RE,VALPER           VALIDATE PERIOD                              
*                                                                               
         LA    R2,SUNFINVH         SPECIFIC INVOICE NUMBER                      
         LA    R3,SUNLINVH                                                      
         LA    R4,SUNAGYH          AGENCY                                       
         BAS   RE,VALINV           VALIDATE INVOICE NUMBER                      
*                                                                               
         LA    R2,SUNUCIDH         SPECIFIC COMMERCIAL                          
         LA    R3,SUNAGYH          AGENCY                                       
         LA    R4,SUNPERH          PERIOD FIELD                                 
         BAS   RE,VALCID           VALIDATE COMMERCIAL                          
*                                                                               
         BAS   RE,VALUNFLT         VALIDATE UNION FILTER                        
         BAS   RE,VALTURN          VALIDATE TURNAROUNDS                         
         BAS   RE,VALUOPT          VALIDATE UNION OPTIONS                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE AGENCY                                                        
*                                                                               
VALAGY   NTR1                                                                   
         MVC   8(L'TGNAME,R3),SPACES  CLEAR AGENCY NAME                         
         OI    6(R3),X'80'            TRANSMIT                                  
         CLI   5(R2),0                                                          
         BE    MISSERR             FORCE SOME INPUT                             
         CLC   =C'ALL',8(R2)       ALL AGENCIES                                 
         BE    VAGYX                                                            
         CLI   RECNUM,AC           IF AGENCY COPY,                              
         BE    VAGY05                                                           
         CLI   RECNUM,EB           IF EBILL,                                    
         BE    VAGY05                                                           
         CLI   RECNUM,BI           OR IF BILLING OVERNIGHT                      
         BNE   VAGY10              FLISTS ARE VALID                             
         TM    WHEN,X'20'                                                       
         BO    VAGY10                                                           
*                                                                               
*        FOR NOW, DO NOT ALLOW NEGATIVE FLISTS                                  
**GY05   BAS   RE,SPECFILT         POSITIVE OR NEGATIVE FLIST?                  
**       BNE   VAGY10                                                           
*                                                                               
VAGY05   CLI   8(R2),C'@'                                                       
         BNE   VAGY10                                                           
         GOTO1 RECVAL,DMCB,(X'40',TLGLCDQ),(X'08',(R2)),(R3)                    
         CLC   TWAAGY,=C'D2'    ONLY FOR TEST SYSTEM                            
         BE    VAGY08           DO NOT REQUIRE PERIOD                           
         CLC   TWAAGY,=C'D3'    OR TALFQA                                       
         BE    VAGY08           DO NOT REQUIRE PERIOD                           
         CLI   SBIPERH+5,0         IF AGENCY IS AN FLIST,                       
         BNE   *+12                PERIOD MUST BE ENTERED                       
         LA    R2,SBIPERH                                                       
         B     MISSERR                                                          
*                                                                               
VAGY08   CLI   SBIFINVH+5,0        IF AGENCY IS AN FLIST                        
         BE    *+12                NO INVOICES OR COMMERCIALS                   
         LA    R2,SBIFINVH         ALLOWED                                      
         B     INVERR                                                           
         CLI   SBILINVH+5,0                                                     
         BE    *+12                                                             
         LA    R2,SBILINVH                                                      
         B     INVERR                                                           
         CLI   SBICIDH+5,0                                                      
         BE    VAGYX                                                            
         LA    R2,SBICIDH                                                       
         B     INVERR                                                           
*                                                                               
VAGY10   GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),(R3)                            
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
VAGYX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE OFFICE NUMBERS                                                
*                                                                               
VALOFF   NTR1                                                                   
         LA    R2,SBIOFFH          OFFICE LIST                                  
         CLI   5(R2),0                                                          
         BE    VOFFX                                                            
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(L'OFFLIST,BLOCK)                              
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         LA    R4,OFFLIST          LIST OF OFFICES                              
*                                                                               
VOFF10   CLI   SCLEN1,L'TGOFF      CHECK INPUT IS CORRECT LENGTH                
         BNE   INVERR                                                           
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'80',SCDATA1)  VALIDATE OFFICE             
         BNE   INVERR                                                           
         MVC   0(1,R4),TGOFF                                                    
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
*                                                                               
         LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VOFF10           AND CONTINUE                                 
*                                                                               
VOFFX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE BILLING PERIOD                                                
*                                                                               
VALBPER  NTR1                                                                   
         MVC   TBEPDATE,TGTODAY1   SET DEFAULT DATES                            
         MVC   TBSPDATE,TGTODAY1                                                
         MVC   TBECHDTE,TGTODAY0                                                
         MVC   TBEDDATE,TGTODAY8                                                
*                                                                               
         LA    R2,SBIPERH          IF A BILLING PERIOD ENTERED                  
         CLI   5(R2),0                                                          
         BE    VBDTEX                                                           
         OC    TIFAGY,TIFAGY       THEN A SPECIFIC AGENCY                       
         BNZ   VALB10              MUST BE INPUT                                
         CLI   SBIAGY,C'@'         FLISTS ARE OKAY TOO                          
         BE    VALB10                                                           
         LA    R2,SBIAGYH                                                       
         B     INVERR                                                           
*                                                                               
VALB10   BAS   RE,VALPD            ACTUALLY VALIDATE THE PERIOD                 
         CLC   TBTODAY,TBSPDATE    THEN ENSURE DATE < TODAY                     
         BNH   INVERR                                                           
         BAS   RE,RERUN            & SET RERUN FLAGS                            
*                                                                               
VBDTEX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE INVOICE NUMBER                                                
*              R2 - A(FIRST INVOICE NUMBER)                                     
*              R3 - A(LAST INVOICE NUMBER)                                      
*              R4 - A(AGENCY)                                                   
*                                                                               
VALINV   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VINVX                                                            
         OC    TIFAGY,TIFAGY       IF REQUESTING INVOICES                       
         BNZ   VINV10                                                           
         LR    R2,R4               THEN MUST INPUT SPECIFIC AGENCY              
         B     INVERR                                                           
*                                                                               
VINV10   LA    RF,TBILLD                                                        
         AH    RF,=Y(TBOPTS3-TBILLD)  RF -> TBOPTS3                             
         TM    0(RF),TBUPDDTE          SKIP IF UPDATE IS ON                     
         BNZ   *+8                                                              
         BAS   RE,WRIPOST          SET WRITE=NO & POST=NO                       
         LR    R4,R2               SET A(INVOICE TO VALIDATE)                   
         BAS   RE,CKINV                                                         
         MVC   FRSTINV,TGINV                                                    
         MVC   LASTINV,FRSTINV     SET LAST INV = FIRST IN CASE NO LAST         
         CLI   5(R3),0             IF LAST INVOICE INPUT                        
         BE    VINVX                                                            
         LR    R4,R3               SET A(INVOICE TO VALIDATE)                   
         BAS   RE,CKINV                                                         
         MVC   LASTINV,TGINV                                                    
*                                                                               
         CLC   8(6,R2),8(R3)       TEST LAST INVOICE NOT BEFORE FIRST           
         BH    BADRANGE                                                         
         CLC   8(3,R2),8(R3)       TEST SAME MONTH                              
         BNE   BADRANGE                                                         
         PACK  DUB(4),10(4,R2)                                                  
         PACK  DUB+4(4),10(4,R3)                                                
         AP    DUB(4),=P'100'      ENSURE MAXIMUM OF 100 INVS REQUESTED         
         CP    DUB(4),DUB+4(4)                                                  
         BL    BADRANGE                                                         
*                                                                               
VINVX    B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        CHECK VALIDITY OF INVOICE                                              
*                                                                               
CKINV    NTR1                                                                   
         LR    R2,R4               SET FOR ERROR EXIT                           
         CLI   5(R2),6             CHECK LENGTH BEFORE TINVCON                  
         BL    INVERR                                                           
         GOTO1 TINVCON,DMCB,8(R2),TGINV,DATCON  CVT TO INTERNAL FORMAT          
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         XC    TGINV,ALLFF         COMPLEMENT AND SAVE IT                       
*                                                                               
         GOTO1 RECVAL,DMCB,TLINCDQ,0 VALIDATE INVOICE                           
         BNE   ERXIT                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE COMMERCIAL                                                    
*        R2 - CID                                                               
*        R3 - AGY                                                               
*        R4 - PERIOD                                                            
*                                                                               
VALCID   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VCIDX                                                            
         BAS   RE,WRIPOST          SET WRITE=NO & POST=NO                       
         OC    TIFAGY,TIFAGY       IF REQUESTING SPECIFIC COMMERCIAL            
         BNZ   VCID10                                                           
         LR    R2,R3               THEN MUST INPUT SPECIFIC AGENCY              
         B     INVERR                                                           
*                                                                               
VCID10   GOTO1 RECVAL,DMCB,TLCOICDQ,(R2)                                        
         LA    R1,KEY                                                           
         USING TLCOPD,R1                                                        
         MVC   TBVCOM,TLCOICOM     SAVE INTERNAL COMMERCIAL NUMBER              
*                                                                               
         LR    R2,R4                                                            
         CLI   5(R2),0             MUST HAVE PERIOD INPUT                       
         BE    MISSERR                                                          
*                                                                               
VCIDX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE OPTIONS                                                       
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,SBIOPTH          OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
*                                                                               
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VOPT10   CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBTRACE      SET TRACE ON                                 
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT20   CLC   =C'TAX',SCDATA1     TRACE TAX RECORDS                            
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBTXTRAC     SET TRACE ON                                 
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT30   CLC   =C'PROD',SCDATA1    CREATE ONLY PRODUCTION WORKER FILE           
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBPRWK       SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT40   CLC   =C'RCVBL',SCDATA1   CREATE ONLY RECEIVABLE WORKER FILE           
         BNE   VOPT50                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBRCVBL      SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT50   CLC   =C'RETRO',SCDATA1   ONLY DO RETRO INVOICES                       
         BNE   VOPT60                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBRETRO      SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT60   CLC   =C'INPUT',SCDATA1   SPECIAL INPUT = FILE                         
         BNE   VOPT70                                                           
         CLC   =C'FILE',SCDATA2                                                 
         BNE   INVERR                                                           
         OI    TBOPTS2,TBFILE      SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT70   CLC   =C'FIX',SCDATA1     FIX FILE                                     
         BNE   VOPT80                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS2,TBFIXINV    SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT80   CLC   =C'WRITEFIX',SCDATA1    DO WRITES                                
         BNE   VOPT85                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS2,TBWRYES     SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT85   CLC   =C'COPIES',SCDATA1  OVERRIDE AGENCY N'BILLING COPIES             
         BNE   VOPT88                                                           
         CLI   RECNUM,AC           GOOD ON ACOPY & DACOPY                       
         BE    *+12                                                             
         CLI   RECNUM,DA                                                        
         BNE   INVERR                                                           
         TM    SCVAL2,X'80'        INPUT MUST BE NUMERIC                        
         BZ    INVERR                                                           
         L     R1,SCBIN2           AND IN THE 1-255 RANGE                       
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         C     R1,=F'255'                                                       
         BH    INVERR                                                           
         STC   R1,TBAYCOPY         SAVE OVERRIDE OF N'BILLING COPIES            
         OI    TBOPTS2,TBOVNCPY    SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT88   CLC   =C'SRTEST',SCDATA1  SORT BY EST # INSTEAD OF ATTN NAME           
         BNE   VOPT90                                                           
         CLI   RECNUM,AC           ONLY FOR AC COPY                             
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS2,TBSRTEST    SET BIT ON                                   
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
         B     VOPT200                                                          
*                                                                               
VOPT90   CLC   =C'WRNPRG',SCDATA1  POSTINGS WARN PROGRAMMERS ONLY               
         BNE   VOPT100                                                          
         OI    TBOPTS2,TBWRNPRG                                                 
         B     VOPT200                                                          
*                                                                               
VOPT100  CLC   =C'REG2',SCDATA1 REGRESSION 2 TEST RUN                           
         BNE   VOPT110                                                          
         CLC   TWAAGY,=C'D2'    ONLY FOR TEST SYSTEM                            
         BE    VOPT105                                                          
         CLC   TWAAGY,=C'D3'    OR TALFQA                                       
         BNE   INVERR                                                           
         BAS   RE,CKSYS3        CHECK NOT TST IF UPDATE OPTION                  
         BNE   INVERR                                                           
VOPT105  OI    TBOPTS2,TBREGRS2                                                 
         B     VOPT200                                                          
*                                                                               
VOPT110  CLC   =C'REG',SCDATA1  REGRESSION TEST RUN                             
         BNE   VOPT120                                                          
         CLC   TWAAGY,=C'D2'    ONLY FOR TEST SYSTEM                            
         BE    VOPT115                                                          
         CLC   TWAAGY,=C'D3'    OR TALFQA                                       
         BNE   INVERR                                                           
         BAS   RE,CKSYS3        CHECK NOT TST IF UPDATE OPTION                  
         BNE   INVERR                                                           
VOPT115  OI    TBOPTS2,TBREGRSN                                                 
         B     VOPT200                                                          
*                                                                               
VOPT120  CLC   =C'OLDSUI',SCDATA1  READ TATU ELEMENTS FOR SUI                   
         BNE   VOPT130                                                          
         LA    RF,TBILLD                                                        
         AH    RF,=Y(TBOPTS3-TBILLD)  RF -> TBOPTS3                             
         OI    0(RF),TBOLDSUI                                                   
         B     VOPT200                                                          
*                                                                               
VOPT130  CLC   =C'NOBOVER',SCDATA1  SKIP BOVER RECORD                           
         BNE   VOPT140                                                          
         CLC   TWAAGY,=C'D2'    ONLY FOR TEST SYSTEM                            
         BE    VOPT135                                                          
         CLC   TWAAGY,=C'D3'    OR TALFQA                                       
         BNE   INVERR                                                           
VOPT135  LA    RF,TBILLD                                                        
         AH    RF,=Y(TBOPTS3-TBILLD)  RF -> TBOPTS3                             
         OI    0(RF),TBNOBOVR                                                   
         B     VOPT200                                                          
*                                                                               
VOPT140  CLC   =C'UPDATE',SCDATA1  UPDATE WITH DATE (FQA,TST/CSC)               
         BNE   VOPT190                                                          
         MVC   QCRDCODE,=C'BI'                                                  
         CLI   RECNUM,BI           ONLY VALID FOR BILLING                       
         BE    VOPT141                                                          
         MVC   QCRDCODE,=C'PB'                                                  
         CLI   RECNUM,PB           PRINT BILLING                                
         BE    VOPT141                                                          
         CLI   RECNUM,VB           AND P+BILLING                                
         BNE   INVERR                                                           
         MVC   QCRDCODE,=C'VB'                                                  
VOPT141  TM    WHEN,X'20'          ONLY ALLOW IF RUNNING SOON                   
         BZ    INVERR                                                           
         OC    TIFAGY,TIFAGY       A SPECIFIC AGENCY IS REQUIRED                
         BZ    INVERR                                                           
         BAS   RE,CHKINV           CHECK RANGE OF INVOICES <= 10                
         CLI   OFFLINE,C'Y'        IF ONLINE,                                   
         BE    VOPT144                                                          
         GOTO1 TSTLCKT,DMCB,=C'TAL_CHECKS'     TEST LOCK SET                    
         BE    CHKLOCK             THEN LOCKOUT ACCESS                          
VOPT144  BAS   RE,CKSYS2           DSPACE MUST BE Q, T, OR C                    
         BNE   INVERR                                                           
         OI    TBOPTS2,TBWRYES     SET BIT ON TO DO WRITES                      
         NI    TBOPTS,X'FF'-TBNOPOST     SET JOB POST = YES                     
         MVI   TWAWRITE,C'Y'           SET WRITE = YES                          
         LA    RF,TBILLD                                                        
         AH    RF,=Y(TBOPTS3-TBILLD)  RF -> TBOPTS3                             
         OI    0(RF),TBUPDDTE                                                   
         MVI   TWAWHEN,TWW$UPSO    FORCE UPDATIVE SOON                          
         BAS   RE,SETDATE          SET DATE AND VALIDATE                        
         B     VOPT200                                                          
*                                                                               
VOPT190  B     INVERR                                                           
*                                                                               
VOPT200  LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VOPT10           AND CONTINUE                                 
*                                                                               
VOPTX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE PERIOD                                                        
*                                                                               
VALPER   NTR1                                                                   
         LA    R2,SUNPERH                                                       
         CLI   5(R2),0             IF NO PERIOD FOR UNION COPY                  
         BNE   VPER10                                                           
         MVC   8(8,R2),=C'NEXTBDAY'   DEFAULT TO NEXT BUSINESS DAY              
         MVI   5(R2),8                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
VPER10   BAS   RE,VALPD                                                         
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         LA    R2,SUNRUNH                                                       
         GOTO1 PDVAL,DMCB,(X'80',(R3))                                          
         MVC   TBRUNEND,PVALPEND    SET RUN DATES                               
         MVC   TBRUNSTR,PVALPSTA                                                
         DROP  R3                                                               
*                                                                               
VPERX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE PERIOD                                                        
*        R2 - A(PERIOD FIELD)                                                   
VALPD    NTR1                                                                   
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   TBEPDATE,PVALPEND                                                
         MVC   TBSPDATE,PVALPSTA                                                
         MVC   TBECHDTE,PVALEEND                                                
         MVC   TBSCHDTE,PVALESTA                                                
         MVC   TBEDDATE,PVALCPER                                                
         DROP  R3                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE UNION FILTER                                                  
*                                                                               
VALUNFLT NTR1                                                                   
         LA    R2,SUNUNIH          UNION FILTER?                                
         CLI   5(R2),0                                                          
         BE    VUNFX               CANNOT HAVE LOCAL WITHOUT UNION              
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLLOD,R3                                                         
         MVI   KEY,TLLOCDQ         BUILD UNION KEY                              
         MVC   TLLOUN,8(R2)                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLLOLCL-TLLOD),KEYSAVE    MAKE SURE 1 UNION EXISTS           
         BNE   INVERR                                                           
         MVC   TGUNI,TLLOUN        SAVE UNION                                   
*                                                                               
         LA    R2,SUNLCLH          LOCAL FILTER?                                
         CLI   5(R2),0                                                          
         BE    VUNFX                                                            
         GOTO1 RECVAL,DMCB,TLLOCDQ,SUNLCLH                                      
         MVC   TGLCL,TLLOLCL       SAVE LOCAL                                   
*                                                                               
VUNFX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE TURNAROUNF FIELD                                              
*                                                                               
VALTURN  NTR1                                                                   
         LA    R2,SUNTURNH         ONLY TURNAROUNDS?                            
         MVI   UNIFILT,C' '        SET TO ALL UNIONS                            
         CLI   5(R2),0                                                          
         BE    VTURNX                                                           
         CLI   8(R2),C'Y'                                                       
         BE    VTURN10                                                          
         CLI   8(R2),C'N'                                                       
         BNE   INVERR                                                           
*                                                                               
VTURN10  MVC   UNIFILT,8(R2)       SET WHICH UNIONS REQUESTED                   
*                                                                               
VTURNX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE UNION SCREEN OPTIONS                                          
*                                                                               
VALUOPT  NTR1                                                                   
         LA    R2,SUNUOPTH         OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    VUOX                                                             
*                                                                               
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
*                                                                               
VUO10    CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VUO20                                                            
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    TBOPTS,TBTRACE      SET TRACE ON                                 
         BAS   RE,ADDISP           GET DISP. INTO NEXT FIELD                    
*                                                                               
VUO20    LA    R3,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VUO10            AND CONTINUE                                 
*                                                                               
VUOX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE A FILTER EXPRESSION                                     
*              R2=A(HEADER)                                                     
*                                                                               
         SPACE 1                                                                
SPECFILT NTR1                                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    NO                                                               
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BNE   NO                                                               
         SPACE 1                                                                
         LA    R5,WORK+1           POSITIVE LIST                                
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  LA    R5,WORK+2           NEGATIVE LIST                                
         SPACE 1                                                                
VALLIST  XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   INVERR                                                           
         B     YES                                                              
         DROP  R3                                                               
         SPACE 1                                                                
*                                                                               
*        SET ALL FLAGS FOR RERUN MODE                                           
*                                                                               
RERUN    NTR1                                                                   
         OI    TBOPTS,TBRERUN+TBNOPOST SET JOB AS RERUN & POST = NO             
         MVI   TWAWRITE,C'N'           SET WRITE = NO                           
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        SET ALL WRITE=NO & POST = NO                                           
*                                                                               
WRIPOST  NTR1                                                                   
         OI    TBOPTS,TBNOPOST         SET JOB POST = NO                        
         MVI   TWAWRITE,C'N'           SET WRITE = NO                           
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
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
*        CHECK THAT SYSTEM IS FQA OR CSC IF REG/REG2 AND WRITE=YES              
*                                                                               
CKSYS    NTR1                                                                   
**NO-OP  02/2015                                                                
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   RF,15,4(R1)         =A(SSB)                                      
         CLC   0(2,RF),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         USING SSOOFF,RF                                                        
         CLI   SSODSPAC-SSOOFF(RF),C'Q'       DSPACE MUST BE Q OR A C           
         BE    XIT                 WRITE ALLOWED ON FQA                         
         CLI   SSODSPAC-SSOOFF(RF),C'C'                                         
         BE    XIT                 OR CSC ONLY                                  
         DROP  RF                                                               
*&&                                                                             
         TM    PRGSTAT,FQASYS+CSCSYS                                            
         BNZ   XIT                                                              
         BAS   RE,WRIPOST          SET WRITE=NO & POST=NO                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK THAT SYSTEM IS FQA TST OR CSC IF UPDATE DATE OPTION              
*                                                                               
CKSYS2   NTR1                                                                   
         TM    PRGSTAT,FQASYS+CSCSYS  DSPACE MUST BE Q OR C                     
         BNZ   YES                                                              
         TM    PRGSTAT,TESTSYS        OR T                                      
         BZ    NO                                                               
                                                                                
         TM    TBOPTS2,TBREGRSN+TBREGRS2  IF TEST, NO REG OR REG2               
         BNZ   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        CHECK THAT SYSTEM IS NOT TST IF REG AND UPDATE OPTIONS SET             
*                                                                               
CKSYS3   NTR1                                                                   
         TM    PRGSTAT,TESTSYS  IF DSPACE IS T,                                 
         BZ    YES                                                              
                                                                                
         LA    R1,TBILLD                                                        
         AH    R1,=Y(TBOPTS3-TBILLD)  R1 -> TBOPTS3                             
         TM    0(R1),TBUPDDTE      INVALID IF UPDATE STATUS IS SET              
         BNZ   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        UPDATE=DATE OPTION                                                     
*        CHECK THAT INVOICE RANGE IS NO MORE THAN 10 INVOICES                   
*                                                                               
CHKINV   NTR1                                                                   
         OC    FRSTINV,FRSTINV                                                  
         BZ    INVERR                                                           
         OC    LASTINV,LASTINV                                                  
         BZ    INVERR                                                           
         LA    R2,SBIFINVH                                                      
         LA    R3,SBILINVH                                                      
         CLI   5(R3),0           IF NO LAST INVOICE,                            
         BNE   *+6                                                              
         LR    R3,R2             USE FIRST INVOICE AS LAST                      
         PACK  DUB(4),10(4,R2)                                                  
         PACK  DUB+4(4),10(4,R3)                                                
         AP    DUB(4),=P'10'                                                    
         CP    DUB(4),DUB+4(4)                                                  
         BL    BADRANGE                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        UPDATE=DATE OPTION                                                     
*        CHECK THAT UPDATE DATE IS NOT EARLIER THAN TODAY                       
*        SET TODAY'S DATE TO UPDATE DATE                                        
*                                                                               
SETDATE  NTR1                                                                   
         GOTO1 DATVAL,DMCB,SCDATA2,TGTODAY0                                     
         GOTO1 DATCON,DMCB,(0,TGTODAY0),(1,TGTODAY1)                            
         GOTO1 (RF),(R1),(0,TGTODAY0),(2,TGTODAY2)                              
         GOTO1 (RF),(R1),(0,TGTODAY0),(8,TGTODAY8)                              
         GOTO1 (RF),(R1),(0,TGTODAY0),(20,TGTODY20)                             
         CLC   TGTODAY1,TBTODAY    DATE CANNOT BE EARLIER THAN TODAY            
         BL    INVERR                                                           
         MVC   TBTODAY,TGTODAY1    SET DEFAULT DATES                            
         MVC   TBEPDATE,TGTODAY1                                                
         MVC   TBSPDATE,TGTODAY1                                                
         MVC   TBECHDTE,TGTODAY0                                                
         MVC   TBEDDATE,TGTODAY8                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
BADRANGE MVI   ERROR,ERBDRANG      BAD RANGE                                    
         B     ERXIT                                                            
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERXIT                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERXIT                                                            
*                                                                               
PRINTERR MVI   ERROR,INVPRINT      INVALID PRINT OPTION                         
         B     ERXIT                                                            
*                                                                               
CHKLOCK  MVI   ERROR,ERCKLOCK      URGENT CHECK RUN LOCKOUT IN EFFECT           
         B     ERXIT                                                            
*                                                                               
ERXIT    DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
*        CONSTANTS, ETC.                                                        
         SPACE 1                                                                
ALLFF    DC    6X'FF'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TABIDSECT                                                      
         EJECT                                                                  
       ++INCLUDE TABILLD                                                        
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF2D                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
*DDPERVAL                                                                       
*DDMASTD                                                                        
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDREMOTED                                                                      
*TAREPWORKD                                                                     
*FASSBOFF                                                                       
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060TAREP03   03/18/15'                                      
         END                                                                    
