*          DATA SET PPREP4102  AT LEVEL 032 AS OF 09/30/20                      
*PHASE PP4102A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE PRNTOFC                                                                
*INCLUDE DLFLD                                                                  
         PRINT NOGEN                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP4102  CLIENT PRODUCT ESTIMATE LIST CHANGE LOG'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* MZEI 06/20    SPEC-44630 MAKE REPORT DOWNLOADABLE                             
*                                                                               
* BPLA 02/15    LIMIT REPORT TO CLIENTS WITH MEDIA NAME OVERRIDES               
*               QOPT 5 = Y                                                      
*                                                                               
* BPLA 02/14    FLAG MISSING DIVISIONS                                          
*                                                                               
* BPLA 08/13    DON'T DUMP IF BILL FORMULA EFF. DATE PRESENT WITHOUT            
*               A FORMULA                                                       
*                                                                               
* BPLA 11/10    CHANGE FOR $ TYPE COST 2                                        
*                                                                               
* BPLA 02/10    FIX DISPLAY OF E (ESTIMATE ONLY) BILLING FORMULAS               
*               THESE ARE NOT USED IN BILLING.                                  
*                                                                               
* BPLA 09/08    FIX DISPLAY OF OFFICE IN HEADLINES                              
*                                                                               
* BPLA 07/08    FIX DISPLAY OF CLIENT GROUP IDS                                 
*                                                                               
* BPLA 10/07    CLIENTS - UCOMM BILL CONTROL                                    
*                                                                               
* SMYE 03/10/06 STEWARDSHIP (X'40' IN PESTTEST) CODING                          
*                                                                               
* KWAN 11/05    PRINT 2 CH MEDIA OFFICE CODES (PRNTOFC)                         
*                                                                               
* KWAN 07/20/01 PRINT CLIENT TRAFFIC OFFICE CODE (CHKCTRAO)                     
*                                                                               
* KWAN 07/18/01 PRINT LEGAL WARNING ROTATION CODES (CHKNOTRA)                   
*                                                                               
* KWAN 06/04/01 PRINT TRAFFIC = Y/N (PPRDSTAT IS X'20')                         
*                                                                               
* KWAN 01/01    PRINT PRD OFFICE AND TRAFFIC CODE                               
*                                                                               
* SMYE 12/08/00 STORE GETUSER ADDRESS IN VGETUSER AND                           
*               ADD MODE RUNFRST TO STORE ADDRESSES                             
*                                                                               
* KWAN 10/00    PRINT UCOMM INFO (CLT/PRD/EST LEVEL)                            
*                                                                               
* SMYE 04/19/00 CHANGES FOR PPNEWFILE (LARGER PCONREC, ETC.)                    
*                                                                               
* KWAN 11/17/99 PRINT EXCLUSION CLASS ON REPORT                                 
*                                                                               
* KWAN 11/08/99 QOPT3 INACTIVE FILTER (QOPT3=I)                                 
*                                                                               
* SMYE 09/29/99 DISPLAY RFP GROUP CODE FOR CLIENTS                              
*                                                                               
* KWAN 01/19/99 DISPLAY COST2 FACTOR IN CLT & EST RECORDS                       
*               MOVE BILFORM INTO A CSECT                                       
*                                                                               
* SMYE 04/22/98 DISPLAY "FROZEN" FOR CLIENTS                                    
*                                                                               
* SMYE 02/09/98 DISPLAY SFH FOR CLIENT AND ESTIMATE                             
*                                                                               
* SMYE 12/17/97 ADD CODE FOR SECOND ESTIMATE STANDARD COMMENT                   
*                                                                               
* SMYE 09/96    ADD DISPLAY OF GROUP ASSIGNMENTS                                
*                                                                               
* SMYE 09/96    DISPLAY HEX VALUE OF CLIENT OFFICE (CONDITIONALLY)              
*                                                                               
* SMYE 05/96    DISPLAY ZENIETH CLIENT CODE AND MEDIA NAME OVERRIDE             
*                                                                               
* SMYE 12/12/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                        
*                                                                               
* BPLA 5/95     EXPAND COLUMNS TO PRINT ON BILL TABLE                           
*                                                                               
* BPLA 12/94    ADD DISPLAY OF ACC OFFICE AGENCY                                
*                                                                               
* SMUR 06/94    DISPLAY DRD OVERRIDE CLIENT CODES                               
*                                                                               
* BPLA 11/93    DISPLAY PST CODES                                               
*                                                                               
* BPLA 4/6/93   FIX DISPLAY OF USER FIELDS AND USE GETUSER                      
*                                                                               
* LWEI 2/1/93   ADD USER DEFINITION FIELDS                                      
*                                                                               
* BPLA 9/18/91  CHANGE DISPLAY OF BILL FORMULA OVERRIDES                        
*                                                                               
* BPLA 8/27/91  ADD CODE FOR SECOND I/O STANDARD COMMENT                        
*                                                                               
* BPLA 5/31/91  REMOVE CODE FOR OLD PBILPROF FIELDS                             
*               - BILCDSW, BILRDSW AND BILOAF                                   
*               ADD CODE OF NEW FIELDS - BILPADJ AND BILPBASB                   
*                                                                               
* BPLA 3/1/91   SKIP TO NEW PAGE FOR OFFICE                                     
*                                                                               
* BPLA 2/13/91  5 DIGIT LOGIC CHANGED TO ALLOW 99999                            
*               FORMAT IS NOW 3 BYTE BINARY WITH HIGH BYTE                      
*               HIGH BIT ON (X'80')                                             
*                                                                               
* BPLA 2/4/91   CHECK FOR PCLTNUMS BEGINNING WITH X'FE'                         
*               MEANS 2 BYTE BINARY VALUE FOLLOWS                               
*                                                                               
* ROSA 11/20/90 ADD GST CODE FOR CLIENT AND PRODUCTS                            
*                                                                               
* BPLA 8/29/90  ADD DISPLAY OF CONTRACT AND I/O STANDARD COMMENTS               
*                                                                               
* BPLA 11/29/89 ADD ACCOUNT OFFICE                                              
*                                                                               
* ROSA 7/11/88  ADD NEW FIELD FOR CLIENT HEADER // BILLING GROUP                
*                                                                               
* ROSA 4/27/88 CHECK FIRST POSITION OF PCLTNUM FOR X'FF' - IF SO                
*              NEXT 2 BYTES ARE PACKED UNSIGNED.                                
*                                                                               
         TITLE 'PP4102  CLIENT PRODUCT ESTIMATE LISTING'                        
*                                                                               
PP4102   CSECT                                                                  
         NMOD1 0,PP4102,RR=R9                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  QOPT1 -> SUPRESS PROFILES                                          *         
*  QOPT2 -> SUPRESS BILLING FORMULA                                   *         
*  QOPT3 -> Y=SUPRESS PRODUCTS AND CLIENTS NOT LINKED TO ESTIMATES    *         
*           I=INACTIVE                                                *         
*  QOPT4 -> SUPRESS ADDRESSES FOR CLIENTS AND PRODUCTS                *         
*                                                                               
*  QOPT5 -> Y= LIMIT REPORT TO CLIENTS WITH MEDIA NAME OVERRIDES                
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    RF,4095(RC)                                                      
         LA    RF,1(RF)                                                         
         LA    RF,4095(RF)                                                      
         LA    RF,1(RF)                                                         
         USING PPFILED+8192,RF                                                  
*                                                                               
         LA    RE,PCONREC          GET PCONREC ADDRESS                          
         ST    RE,ACONIO1          STORE ADDRESS                                
         DROP  RF                                                               
*                                                                               
         LA    R5,PP4102+4095                                                   
         LA    R5,1(R5)                                                         
         USING PP4102+4096,R5      SECOND BASE REGISTER                         
*                                                                               
         CLI   MODE,REQLAST                                                     
         JNE   CKM0A                                                            
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOAD REPORT OPEN                    
         JNE   EXT                                                              
         BRAS  RE,ENDDOWN          END DOWNLOAD                                 
         J     EXT                                                              
*                                                                               
CKM0A    CLI   MODE,OFCFRST                                                     
         BNE   CKM0                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXT                                                              
*                                                                               
CKM0     CLI   MODE,RUNFRST                                                     
         BNE   CKM1                                                             
*                                                                               
         MVC   DUB,SPACES          GET PSTVAL                                   
         MVC   DUB(6),=C'T00A6B'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   APSTVAL,4(R1)       SAVE ITS ADDR                                
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         L     RE,=V(GETUSER)                                                   
         ST    RE,VGETUSER         STORE GETUSER ADDRESS                        
*                                                                               
         L     RE,=V(PRNTOFC)                                                   
         ST    RE,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         B     EXT                                                              
*                                                                               
ACONIO1  DS    F                   FOR PCONREC ADDRESS                          
*                                                                               
CKM1     CLI   MODE,FESTREQ                                                     
         BNE   CKM2                                                             
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDACTV,C'Y'       TO GET ZZZ ESTS                              
         XC    PCLI,PCLI                                                        
         XC    PPROD,PPROD                                                      
         MVI   SCLTSTAT,X'00'                                                   
*                                                                               
         BRAS  RE,INITDOWN         INITIALIZE DOWNLOAD                          
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   CKM1A                                                            
         LA    R1,MYHOOK                                                        
         ST    R1,HEADHOOK                                                      
         BRAS  RE,DOWNHDRS         DOWNLOAD HEADERS                             
*                                                                               
CKM1A    CLC   QOPT1(2),=C'YY'                                                  
         BNE   CKM1C                                                            
         MVI   RCSUBPRG,30                                                      
         B     EXT                                                              
CKM1C    CLI   QOPT1,C'Y'                                                       
         BNE   CKM1E                                                            
         MVI   RCSUBPRG,10                                                      
         B     EXT                                                              
CKM1E    CLI   QOPT2,C'Y'                                                       
         BNE   EXT                                                              
         MVI   RCSUBPRG,20                                                      
         B     EXT                                                              
*                                                                               
CKM2     CLI   MODE,FESTCLI                                                     
         BNE   CKM3                                                             
*                                                                               
         MVI   CLIPRTSW,C'N'       CLIENT HAS NOT BEEN PRINTED YET              
         CLI   QOPT3,C'Y'                                                       
         BE    EXT                                                              
*                                                                               
         CLI   QOPT5,C'Y'          ONLY ONES WITH MEDIA NAME OVERRIDE           
         BNE   CKM2A                                                            
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'41'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   EXT                 SKIP IF NOT FOUND                            
*                                                                               
CKM2A    CLI   QOPT3,C'I'          FILTERING ON INACTIVE?                       
         BNE   CKM2H                                                            
         MVI   BYTE,C'C'           CLIENT LEVEL INDICATOR                       
         BRAS  RE,CHKEST           CHECK FOR ESTIMATE (WILL SET CC)             
         BE    EXT                                                              
*                                                                               
CKM2H    BAS   RE,CLIPRINT                                                      
         B     EXT                                                              
*                                                                               
CKM3     CLI   MODE,FESTPRO                                                     
         BNE   CKM4                                                             
         CLI   QOPT3,C'Y'                                                       
         BE    EXT                                                              
*                                                                               
         CLI   QOPT3,C'I'          FILTERING ON INACTIVE?                       
         BNE   CKM3H                                                            
         MVI   BYTE,C'P'           PRODUCT LEVEL INDICATOR                      
         BRAS  RE,CHKEST           CHECK FOR ESTIMATE (WILL SET CC)             
         BE    EXT                                                              
         BAS   RE,CLIPRINT         WILL PRINT CLI, IF IT'S NOT PRINTED          
*                                                                               
CKM3H    BAS   RE,PRDPRINT                                                      
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCEST        CHECK FOR ESTIMATE MODE                      
         BNE   EXT                                                              
*                                                                               
         CLI   QOPT3,C'I'          FILTERING ON INACTIVE?                       
         BE    EXT                                                              
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   ESTPRNT                                                          
         CLC   PCLI,PESTKCLT                                                    
         BE    NOTCLI                                                           
         BAS   RE,CLIPRINT                                                      
NOTCLI   CLC   PPROD,PESTKPRD                                                   
         BNE   PRDBEST                                                          
         CLC   PCLI,PESTKCLT                                                    
         BE    ESTPRNT                                                          
PRDBEST  BAS   RE,PRDPRINT                                                      
*                                                                               
* MOVE ESTIMATE DATA TO PRINT LINE                                              
*                                                                               
ESTPRNT  CLC   QEST,SPACES                                                      
         BE    EXT          NOT PRINTING ESTS                                   
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    EST1                                                             
         CLC   QPRODUCT,SPACES                                                  
         BE    EST1                                                             
         CLC   PESTKPRD,QPRODUCT        MUST BE RIGHT PRODUCT                   
         BNE   EXT                                                              
*                                                                               
EST1     SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,7                                                             
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BL    MOVETOP                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(7),=C'PRODUCT'                                               
         MVC   P+11(3),CODESAVE                                                 
         MVC   P+16(11),=C'(CONTINUED)'                                         
         BAS   R8,PRINTIT                                                       
         MVC   P+2(12),=12C'-'                                                  
         CLI   CODESAVE+2,C' '                                                  
         BNE   *+8                                                              
         MVI   P+13,C' '                                                        
         BAS   R8,PRINTIT                                                       
MOVETOP  MVC   PROFILE,PESTPROF                                                 
         MVI   CESW,X'02'                                                       
         BAS   RE,DISPROF                                                       
         LA    R7,PESTBILP         BILLING PROFILE                              
*                                                                               
         GOTO1 =A(BILFORM),DMCB,(RA),RR=RELO                                    
*                                                                               
         MVC   P+4(8),=C'ESTIMATE'                                              
         EDIT  (2,PESTKEST),(3,P+14),ALIGN=LEFT                                 
         MVC   P+19(L'PESTNAME),PESTNAME                                        
*                                                                               
         CLC   QOPT1(2),=C'YY'                                                  
         BNE   EST1B                                                            
         GOTO1 DATCON,DMCB,(0,PESTST),(5,P+52)                                  
         MVC   P+61(2),=C'TO'                                                   
         GOTO1 DATCON,DMCB,(0,PESTEND),(5,P+64)                                 
*                                                                               
EST1B    L     R6,=A(BILLINES)                                                  
         MVI   BILSW,0                                                          
         L     R7,=A(PROLINES)                                                  
         MVI   ESPSW,0                                                          
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         CLI   PESTNAM2,C' '                                                    
         BNH   EST2                                                             
         MVC   P+19(L'PESTNAM2),PESTNAM2                                        
         TM    PESTTEST,X'80'         SEE IF TEST ESTIMATE                      
         BZ    EST1E                                                            
         MVC   P+12(6),=C'*TEST*'                                               
         TM    PESTTEST,X'40'         SEE IF STEWARDSHIP ESTIMATE               
         BZ    *+10                   NO                                        
         MVC   P+12(6),=C'*STEW*'     REPLACE *TEST*                            
         NI    PESTTEST,X'FF'-X'80'    SO I WON'T REDO IT                       
*                                                                               
EST1E    BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
EST2     EQU   *                                                                
         TM    PESTTEST,X'80'          SEE IF TEST ESTIMATE                     
*SMY*    BZ    *+10                                                             
         BZ    EST2B                   NO                                       
         MVC   P+12(6),=C'*TEST*'                                               
         TM    PESTTEST,X'40'         SEE IF STEWARDSHIP ESTIMATE               
         BZ    *+10                   NO                                        
         MVC   P+12(6),=C'*STEW*'     REPLACE *TEST*                            
EST2B DS 0H                                                                     
         CLC   QOPT1(2),=C'YY'                                                  
         BE    EST2D                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTST),(5,P+19)                                  
         MVC   P+28(2),=C'TO'                                                   
         GOTO1 DATCON,DMCB,(0,PESTEND),(5,P+31)                                 
EST2D    BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
*        CLI   QOPT1,C'Y'                                                       
*        BNE   EST2F                                                            
         CLC   P,SPACES                                                         
         BE    *+8                                                              
EST2F    BAS   R8,PRINTIT                                                       
         OC    PESTGRPS,SPACES                                                  
         CLC   PESTGRPS,SPACES                                                  
         BE    EST5                                                             
         MVC   P+19(8),=C'FILTERS='                                             
         MVC   P+28(3),PESTGRPS                                                 
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
EST5     DS    0H                                                               
         CLI   PESTSTAT,C'1'                                                    
         BL    EST11                                                            
         CLI   PESTSTAT,C'2'                                                    
         BH    EST11                                                            
         MVC   P+19(20),=C'REGULARLY LOCKED OUT'                                
         CLI   PESTSTAT,C'1'                                                    
         BE    EST10                                                            
         MVC   P+19(22),=C'PERMANENTLY LOCKED OUT'                              
EST10    BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
EST11    CLI   PESTJOB,C' '                                                     
         BNH   EST12                                                            
         MVC   P+19(10),=C'AD NUMBER='                                          
         MVC   P+30(6),PESTJOB                                                  
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
****  BELOW COMMENTED OUT CODE IS REPLACED IMMEDIATELY FOLLOWING ****           
*****EST12    OC    PESTCOM,SPACES                                              
*****    CLC   PESTCOM,SPACES                                                   
*****    BE    EST13                                                            
*****    MVC   P+19(17),=C'STANDARD COMMENT='                                   
*****    MVC   P+37(6),PESTCOM                                                  
*****    LA    R2,P+37             RIGHT ALIGN COMMENT NUMBER                   
*****    LA    R3,5                                                             
*****    LA    R4,P+42                                                          
*****EST12B   CLI   0(R2),C' '                                                  
*****    BNE   EST12D                                                           
*****    EX    R3,*+8                                                           
*****    B     *+10                                                             
*****    MVC   0(0,R2),1(R2)                                                    
*****    BCT   R3,EST12B                                                        
*                                                                               
EST12    OC    PESTCOM,SPACES                                                   
         CLC   PESTCOM,SPACES      HAVE COMMENT ?                               
         BNH   EST13               NO                                           
         MVC   P+19(17),=C'STANDARD COMMENT='                                   
         CLI   PESTCOM2,0          SECOND COMMENT ?                             
         BH    EST12C              YES                                          
         MVC   P+37(6),PESTCOM     NO - FIRST COMMENT ONLY                      
         B     EST12P                                                           
EST12C   MVC   P+35(2),=C'S='                                                   
         MVC   P+38(6),PESTCOM                                                  
         MVI   P+44,C','                                                        
         MVC   P+45(6),PESTCOM2                                                 
         B     EST12X              CAN'T USE P+47                               
*                                                                               
EST12P   EQU   *                                                                
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
EST12X   BAS   R8,PRINTIT                                                       
*                                                                               
EST13    CLI   PESTREVN,C'0'                                                    
         BL    EST14                                                            
         MVC   P+19(16),=C'REVISION NUMBER='                                    
         MVC   P+36(3),PESTREVN                                                 
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
EST14    CLI   PESTZZZ,C' '                                                     
         BNH   EST15                                                            
         OC    PESTZZZ,SPACES                                                   
         MVC   P+19(47),PESTZZZ                                                 
         CLC   P+47(19),SPACES                                                  
         BNE   EST14X              CAN'T USE P+47                               
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
EST14X   BAS   R8,PRINTIT                                                       
*                                                                               
EST15    DS    0H                  SPECIAL FINANCIAL HANDLING                   
         MVC   P+19(7),=C'SFH = Y'                                              
         TM    PESTTEST,X'01'      SFH = Y ?                                    
         BO    EST15K              YES - PRINT IT                               
         TM    SCLTSTAT,X'01'      SFH CLIENT ?                                 
         BO    EST15D              YES - EST SFH IS (N)O                        
         MVC   P+19(7),SPACES      NOT SFH                                      
         B     EST20                                                            
EST15D   MVI   P+25,C'N'                                                        
EST15K   BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
EST15X   BAS   R8,PRINTIT                                                       
*                                                                               
*                                                                               
*                                                                               
EST20    DS    0H                  COST2 FACTOR                                 
*                                                                               
         OC    PESTCF,PESTCF       ANYTHING IN COST2 FACTOR?                    
         BZ    EST25                                                            
         MVC   P+19(15),=C'COST2 FACTOR = '                                     
         CP    PESTCF,=P'0'        COS2=0 BIT IS ON?                            
         BNE   *+14                                                             
         MVC   P+34(3),=C'0.0'                                                  
         B     EST20X                                                           
         EDIT  (P5,PESTCF),(8,P+34),6,ALIGN=LEFT,FILL=0,DROP=5                  
EST20X   BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
*                                                                               
*                                                                               
EST25    BAS   RE,ESTUSER          DO USER FIELDS                               
*                                                                               
         MVI   BYTE,C'E'           ESTIMATE UCOMM                               
         BRAS  RE,CHKUCOMM                                                      
*                                                                               
NEXTELN  BAS   R8,PRINTIT          FINISH REMAINING LINES                       
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
         CLI   ESPSW,X'FF'         LAST LINE PRINTED                            
         BNE   NEXTELN                                                          
         BAS   R8,PRINTIT                                                       
*                                                                               
         B     EXT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
CLIPRINT NTR1                                                                   
*                                                                               
         CLI   CLIPRTSW,C'N'       CLIENT PRINTED?                              
         BNE   CLIPXX                                                           
*                                                                               
         BRAS  RE,CLRDOWN          CLEAR DOWNLOAD FIELDS                        
*                                                                               
         CLC   QEST,SPACES         SKIP TO NEW PAGE ONLY IF PRINTING            
         BNE   CLIP0               ESTS OR LESS THAN 8 LINES LEFT               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,8                                                             
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BH    CLIP0                                                            
         B     CLIP0A         NO NEW PAGE                                       
*                                                                               
CLIP0    MVI   FORCEHED,C'Y'       PRINT HEADLINES                              
CLIP0A   MVC   PROFILE,PCLTPROF                                                 
         MVI   CESW,X'01'                                                       
         BAS   RE,DISPROF                                                       
         MVC   DLCDIV,PCLTPROF     POSITION 1 = CLIENT DIVISION                 
*                                                                               
*        MOVE CLIENT DATA TO PRINT LINE                                         
*                                                                               
         MVC   PCLI,PCLTKCLT                 SAVE FOR LATER COMPARISON          
         MVC   SCLTSTAT,PCLTSTAT         SAVE FOR TESTING IN ESTPRNT            
         XC    PPROD,PPROD                                                      
         MVC   P+0(6),=C'CLIENT'                                                
         MVC   P+8(3),PCLTKCLT                                                  
         MVC   P+13(L'PCLTNAME),PCLTNAME                                        
*                                                                               
         MVC   DLMED,QMEDIA                                                     
         MVC   DLCLT,PCLTKCLT                                                   
         MVC   DLCLTN,PCLTNAME                                                  
*                                                                               
         L     R7,=A(PROLINES)                                                  
         MVI   ESPSW,0                                                          
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P(11),=12C'-'                                                    
         CLI   PCLTKCLT+2,C' '                                                  
         BNE   *+8                                                              
         MVI   P+10,C' '                                                        
*                                                                               
         XC    DLFRZ,DLFRZ                                                      
         TM    PCLTSTAT,X'02'      FROZEN ?                                     
         BNO   CLIP0B              NO                                           
         MVC   P+13(16),=C'***  FROZEN  ***'                                    
         MVC   DLFRZ,=C'FRZ'                                                    
*                                                                               
CLIP0B   BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         CLI   QOPT4,C'Y'                   SKIP PRINTING CLIENT ADDR           
         BE    CLIP1A                                                           
         BRAS  RE,DLCLADD                   SET CLIENT DOWNLOAD ADDRESS         
         MVC   P+13(L'PCLTLIN1),PCLTLIN1                                        
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P+13(L'PCLTLIN2),PCLTLIN2                                        
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P+13(5),=C'ATTN-'                                                
         MVC   P+18(L'PCLTATTN),PCLTATTN                                        
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         OC    PCLTBNAM,PCLTBNAM                                                
         BZ    CLIP1                                                            
         MVC   P+13(12),=C'BILRCT NAME-'                                        
         MVC   P+25(L'PCLTBNAM),PCLTBNAM                                        
CLIP1    BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
CLIP1A   OC    PCLTOFF,PCLTOFF                                                  
         BZ    SKIPOFF                                                          
         MVC   P+13(7),=C'OFFICE-'                                              
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         MVC   DLOFF,PCLTOFF                                                    
         GOTOR VPRNTOFC,DMCB,PCLTOFF,P+20,VOFFICER,QAGENCY,VCOMFACS             
SKIPOFF  OC    PCLTNUM,PCLTNUM                                                  
         BZ    SKIPNUM                                                          
         MVC   P+30(7),=C'NUMBER-'                                              
         MVC   P+37(L'PCLTNUM),PCLTNUM                                          
*                                                                               
         CLI   PCLTNUM,255         EXCEPTION CLIENT                             
         BNE   CHKFE                                                            
         UNPK  P+37(5),PCLTNUM+1(3)                                             
         MVI   P+41,C' '                                                        
         B     SKIPNUM                                                          
*                                                                               
*                                                                               
CHKFE    DS    0H                   CHECK FOR SPECIAL 3 BYTE BINARY             
*                                   FORMAT                                      
*                                   SEE IF HIGH NIBBLE IS X'80'                 
*                                   IF SO SET OFF - THEN VALUE                  
*                                   IS 3 BYTE BINARY                            
         MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'80'                                                       
         BNE   SKIPNUM                                                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'7F'                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+37(5),DUB                                                      
*                                                                               
SKIPNUM  MVC   DLINTF,P+37          DOWNLOAD INT#                               
         CLC   P+13(25),SPACES                                                  
         BE    ACCOFF                                                           
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
ACCOFF   OC    PCLTAOFC,PCLTAOFC   ANYTHING PRESENT IN ACC OFF?                 
         BZ    ACCOFFX                                                          
         MVC   DLAOFF,PCLTAOFC     ACC OFFICE                                   
         MVC   P+13(16),=C'ACC OFFICE CODE-'                                    
         MVC   P+29(2),PCLTAOFC                                                 
         OC    PCLTACCA,PCLTACCA   CHECK FOR ACC OFFICE AGENCY                  
         BZ    ACCOFF5                                                          
         MVI   P+31,C'/'                                                        
         MVC   P+32(2),PCLTACCA                                                 
*                                                                               
ACCOFF5  BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
ACCOFFX  DS    0H                                                               
*                                                                               
         BRAS  RE,CHKCTRAO         CHECK FOR CLIENT TRAFFIC OFFICE CODE         
*                                                                               
         LA    R2,PCLTELEM         CONTRACT STANDARD COMMENT                    
         MVI   ELCODE2,X'10'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   ISCOM                                                            
         MVC   P+13(26),=C'CONTRACT STANDARD COMMENT-'                          
         MVC   P+39(6),2(R2)                                                    
         MVC   DLCCOMM,2(R2)                                                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
ISCOM    DS    0H                   CONTRACT STANDARD COMMENT                   
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'11'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   DRDCLT                                                           
         MVC   P+13(21),=C'I/O STANDARD COMMENT-'                               
         MVC   P+34(6),2(R2)                                                    
         MVC   DLICOM1,2(R2)                                                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
         BAS   RE,NEXTEL              LOOK FOR ANOTHER                          
         BNE   DRDCLT                                                           
         MVC   P+13(21),=C'I/O STANDARD COMMENT-'                               
         MVC   P+34(6),2(R2)                                                    
         MVC   DLICOM2,2(R2)                                                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
DRDCLT   DS    0H                   DRD OVERRIDE CLIENT                         
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'30'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   ZENCLT                                                           
         MVC   P+13(20),=C'DRD OVERRIDE CLIENT-'                                
         MVC   P+34(3),2(R2)                                                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
ZENCLT   DS    0H                   ZENIETH CLIENT CODE                         
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'32'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   MEDOVR                                                           
         MVC   P+13(20),=C'ZENIETH CLIENT CODE-'                                
         MVC   P+34(5),2(R2)                                                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
MEDOVR   DS    0H                   MEDIA NAME OVERRIDE                         
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'41'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   FINCL                                                            
         MVC   P+13(20),=C'MEDIA NAME OVERRIDE-'                                
         MVC   P+34(10),2(R2)                                                   
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
FINCL    CLI   PCLTFIN,C'Y'                                                     
         BNE   BILGRP                         WAS CLIP3                         
         MVC   P+13(9),=C'FINANCIAL'                                            
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
BILGRP   CLI   PCLTBLGP,X'40'        ANYTHING PRESENT IN BILLGROUP              
         BNH   SFHCLT                                                           
         MVC   P+13(14),=C'BILLING GROUP-'                                      
         MVC   P+27(1),PCLTBLGP                                                 
         MVC   DLBGRP,PCLTBLGP       BILLING GROUP                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
SFHCLT   DS    0H                 TEST FOR SFH (SPEC'L FIN'L HANDLING)          
         LA    R8,SFHAGYT          TBL OF SFH AGENCIES                          
SFHCL2   CLI   0(R8),X'FF'         END ?                                        
         BE    COSTF00             YES - NOT SFH AGENCY                         
         CLC   PCLTKAGY(2),0(R8)   SFH AGENCY ?                                 
         BE    SFHCL6              YES                                          
         LA    R8,2(R8)            BUMP TO NEXT AGENCY                          
         B     SFHCL2              TEST NEXT                                    
SFHCL6   MVC   P+13(7),=C'SFH = N'                                              
         TM    PCLTSTAT,X'01'      SFH = Y ?                                    
         BNO   SFHCLX              NO                                           
         MVI   P+19,C'Y'                                                        
SFHCLX   BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
*                                                                               
*                                                                               
COSTF00  DS    0X                  COST2 FACTOR ELEMENT (X'45')                 
*                                                                               
         TM    PCLTSTAT,X'04'      COS2=Y?                                      
         BNO   *+18                                                             
         MVC   P+13(15),=C'$ TYPE COST2 = '                                     
         MVI   P+28,C'Y'                                                        
         B     COSTF50                                                          
*                                                                               
         TM    PCLTSTAT,X'08'                                                   
         BNO   COSTFXX                                                          
*                                                                               
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'45'                                                    
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
*                                                                               
         DC    H'0'                ELEM MUST EXIST (PCLTSTAT BIT IS ON)         
*                                                                               
         MVC   P+13(15),=C'COST2 FACTOR = '                                     
         USING PCLTCFEL,R2                                                      
*                                                                               
COSTF05  CP    PCLTCF,=P'0'        COST2 FACTOR IS ZERO?                        
         BNE   *+14                                                             
         MVC   P+28(3),=C'0.0'                                                  
         B     COSTF50                                                          
*                                                                               
         EDIT  (P5,PCLTCF),(8,P+28),6,ALIGN=LEFT,FILL=0,DROP=5                  
COSTF50  MVC   DLCOS2,P+28         COST2 FACTOR FOR DOWNLOAD                    
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
COSTFXX  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         LA    R2,PCLTELEM                                                      
         MVI   ELCODE2,X'46'       T/A RFP GROUP ELEMENT                        
         BAS   RE,NEXTEL                                                        
         BNE   RFPGRPX             NO RFP GROUP CODE                            
*                                                                               
         USING PCLTTAEL,R2                                                      
         MVC   P+13(21),=C'T/A RFP GROUP CODE = '                               
         MVC   P+34(L'PCLTTAGRP),PCLTTAGRP                                      
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
RFPGRPX  DS    0H                                                               
*                                                                               
UBCCL    DS    0H                                                               
         TM    PCLTSTAT,X'80'      UCOMM BILL CONTROL                           
         BNO   UBCCLX              NO                                           
         MVC   P+13(7),=C'UBC = Y'                                              
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
UBCCLX   DS    0H                                                               
*                                                                               
*                                                                               
CLIP3A   CLI   PCLTGST,C' '                                                     
         BNH   CLIP3C                                                           
         BAS   RE,GSTHERE                                                       
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
CLIP3C   DS    0H                                                               
         LA    R2,PCLTREC+33                                                    
         LA    R3,P+13                                                          
         MVI   ELCODE2,X'25'       PST ELEMENT                                  
         BAS   RE,DISPPST                                                       
         CLI   PSTOUT,C' '                                                      
         BNH   CLIP3D                                                           
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
CLIP3D   MVI   ELCODE2,X'26'                                                    
         BAS   RE,DISPPST          DISPLAY NEW MAIN PST                         
*                                                                               
         CLI   PSTOUT,C' '                                                      
         BNH   CLIP2X                                                           
         BAS   R8,GETPLN                                                        
         BAS   R8,PRINTIT                                                       
         B     CLIP2X                                                           
*                                                                               
CLIP2    BAS   R8,PRINTIT                                                       
CLIP2X   BAS   R8,GETPLN                                                        
         CLI   ESPSW,X'FF'                                                      
         BNE   CLIP2                                                            
*                                                                               
         MVI   BYTE,C'C'           CLIENT UCOMM (TITLES ONLY)                   
         BRAS  RE,CHKUCOMM                                                      
*                                                                               
         BAS   RE,CGAPRT           GROUP ASSIGNMENTS (CLIENT)                   
*                                                                               
         BAS   R8,PRINTIT                                                       
         MVI   CLIPRTSW,C'Y'       CLIENT HAS BEEN PRINTED                      
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   EXT1L                                                            
         CLC   QPRODUCT,SPACES     IF CLIENT ONLY REQUEST,                      
         BNE   EXT1L                                                            
         BRAS  RE,DOWNROW          DOWNLOAD ROW                                 
*                                                                               
CLIPXX   B     EXT1L                                                            
*                                                                               
***********************************************************************         
*                                                                               
*                                                                               
SFHAGYT  DC    C'MXWIWJWTSJ',X'FF'   "SFH" AGENCIES                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
GSTHERE  NTR1                                                                   
         CLI   PCLTGST,C' '                                                     
         BNH   EXT1L                                                            
         MVC   P+13(9),=C'GST CODE-'                                            
         MVC   P+22(1),PCLTGST                                                  
         B     EXT1L                                                            
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0           END OF REC                                     
         BE    NEXTEL2                                                          
         CLC   0(1,R2),ELCODE2                                                  
         BER   RE                                                               
         LTR   R0,R0                                                            
         BNZ   NEXTEL                                                           
         DC    H'0'             BAD RECORD                                      
*                                                                               
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
PRDPRINT NTR1                                                                   
         CLC   QPRODUCT,SPACES                                                  
         BNE   PRDP1                                                            
         CLC   QEST,SPACES                                                      
         BE    EXT1L                                                            
PRDP1    DS    0C                                                               
         BRAS  RE,CLRDWNP          CLEAR DOWNLOAD FIELDS FOR PRODUCT            
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    PRDP4                                                            
         CLC   PPRDKPRD,QPRODUCT                                                
         BNE   EXT1L                                                            
*                                                                               
PRDP4    LA    R7,PPRDBILP          BILLING PROFILE                             
*                                                                               
         GOTO1 =A(BILFORM),DMCB,(RA),RR=RELO                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,10                                                            
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   MOVEPRD                                                          
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
*        MOVE PRODUCT DATA TO PRINT LINE                                        
*                                                                               
MOVEPRD  MVC   PPROD,PPRDKPRD                                                   
         MVC   DLPRD,PPRDKPRD                                                   
         MVC   P+2(7),=C'PRODUCT'                                               
         MVC   P+11(3),PPRDKPRD                                                 
         MVC   CODESAVE,PPRDKPRD   SAVE PRODUCT CODE                            
         MVC   P+16(L'PPRDNAME),PPRDNAME                                        
         MVC   DLPRDNM,PPRDNAME                                                 
         L     R6,=A(BILLINES)                                                  
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P+2(12),=12C'-'                                                  
         CLI   PPRDKPRD+2,C' '                                                  
         BNE   *+8                                                              
         MVI   P+13,C' '                                                        
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         CLI   QOPT4,C'Y'                    SKIP PRINTING PROD ADDR            
         BE    PRD5                                                             
         MVC   P+16(L'PPRDLIN1),PPRDLIN1                                        
         MVC   DLADD1P,PPRDLIN1                                                 
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P+16(L'PPRDLIN2),PPRDLIN2                                        
         MVC   DLADD2P,PPRDLIN2                                                 
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         MVC   P+16(L'PPRDATTN),PPRDATTN                                        
         MVC   DLADD3P,PPRDATTN                                                 
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         OC    PPRDBILL,PPRDBILL                                                
         BZ    PRD1                                                             
         MVC   P+16(L'PPRDBILL),PPRDBILL                                        
         MVC   DLADD4P,PPRDBILL                                                 
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
**NEW 3/7/89                                                                    
*        CHK FOR 2ND BILL RECEIPT NAME LINE                                     
*                                                                               
         OC    PPRDBIL2,PPRDBIL2                                                
         BZ    PRD1                                                             
         MVC   P+16(L'PPRDBIL2),PPRDBIL2                                        
         MVC   DLADD5P,PPRDBIL2                                                 
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
         B     PRD2                                                             
**NEW 3/7/89                                                                    
PRD1     BAS   R8,GETBLN                                                        
PRD2     BAS   R8,PRINTIT                                                       
PRD5     CLI   PPRDGST,C' '                                                     
         BNH   PRD5B                                                            
         MVC   P+16(9),=C'GST CODE-'                                            
         MVC   P+25(1),PPRDGST                                                  
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
PRD5B    DS    0H                                                               
         LA    R2,PPRDREC+33                                                    
         LA    R3,P+16                                                          
*                                                                               
         MVI   ELCODE2,X'25'                                                    
         BAS   RE,DISPPST                                                       
         CLI   PSTOUT,C' '                                                      
         BNH   PRD5C                                                            
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
*                                                                               
PRD5C    MVI   ELCODE2,X'26'                                                    
         BAS   RE,DISPPST         DISPLAY NEW MAIN PST                          
*                                                                               
         CLI   PSTOUT,C' '                                                      
         BNH   PRD5D                                                            
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
PRD5D    OC    PPRDEXCL,PPRDEXCL                                                
         BZ    PRD5K                                                            
         MVC   P+16(16),=C'ADJACENCY CODE ='                                    
         MVC   P+35(3),PPRDEXCL                                                 
         OI    PPRDEXCL+1,0                                                     
         BZ    *+10                                                             
         MVC   P+30(3),=C'S ='                                                  
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
PRD5K    DS    0H                  FOR FUTURE USES                              
*                                                                               
         BRAS  RE,CHKPRDOT         PRINT PRD OFFICE/TRAFFIC CODES               
*                                                                               
         BRAS  RE,CHKNOTRA         SEE CHKNOTRA FOR ITEMS PRINTED               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         OC    PPRDDIV,PPRDDIV                                                  
         BZ    SKIPDIV                                                          
         MVC   P+16(8),=C'DIVISION'                                             
         MVC   P+25(3),PPRDDIV                                                  
         MVC   DLPDIV,PPRDDIV                                                   
*                                                                               
         MVC   SAVEKEY,KEY     SAVE KEY                                         
         MVI   KEY+3,X'03'                                                      
         MVC   KEY+7(3),PPRDDIV                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+10                                                             
         MVC   P+30(15),=C'**NOT ON FILE**'                                     
*                                                                               
         MVC   KEY,SAVEKEY     RESTORE SEQ READING                              
         GOTO1 HIGH                                                             
*                                                                               
SKIPDIV  OC    PPRDACCT,PPRDACCT                                                
         BZ    SKIPACT                                                          
         MVC   P+29(09),=C'ACCT NO.='                                           
         MVC   P+38(L'PPRDACCT),PPRDACCT                                        
         CLI   PPRDACCT,X'FF'                                                   
         BNE   SKIPACTA                                                         
         MVC   FULL,PPRDACCT                                                    
         MVI   FULL,0                                                           
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+38(5),DUB                                                      
SKIPACTA MVC   DLACCNO,P+38                                                     
SKIPACT  CLC   P+16(20),SPACES                                                  
         BE    SKIPBOTH                                                         
         BAS   R8,GETBLN                                                        
         BAS   R8,PRINTIT                                                       
*                                                                               
SKIPBOTH BAS   RE,PRDUSER                                                       
*                                                                               
         MVI   BYTE,C'P'           PRODUCT UCOMM                                
         BRAS  RE,CHKUCOMM                                                      
*                                                                               
PRD20    BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'         NO MORE LINES TO PRINT                       
         BE    PRDX                                                             
         BAS   R8,PRINTIT                                                       
         B     PRD20                                                            
*                                                                               
PRDX     DS    0H                                                               
         BAS   RE,PGAPRT           GROUP ASSIGNMENTS (PRODUCT)                  
         BAS   R8,PRINTIT                                                       
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   *+8                                                              
         BRAS  RE,DOWNROW                 DOWNLOAD ROW                          
         B     EXT1L                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRINTIT  DS    0H                                                               
*                                                                               
         BRAS  RE,PRINTITB                                                      
PRNTX    BR    R8                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CODESAVE DS    CL3                                                              
SAVELINE DS    CL1                                                              
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
EXT1L    XIT1  1                                                                
EXT      XMOD1 1                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
GETPLN   CLI   ESPSW,X'FF'                                                      
         BE    GETPX                                                            
         CLI   0(R7),0             NO MORE LINES TO PRINT                       
         BE    GETPLN1                                                          
         MVC   P+48(75),0(R7)                                                   
         LA    R7,75(R7)           NEXT LINE                                    
         B     GETPX                                                            
*                                                                               
GETPLN1  MVI   ESPSW,X'FF'         SET NO MORE LINES TO PRINT                   
GETPX    BR    R8                  RETURN                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETBLN   CLI   BILSW,X'FF'                                                      
         BE    GETBX                                                            
         CLI   0(R6),0             NO MORE LINES TO PRINT                       
         BE    GETBLN1                                                          
         MVC   P+48(75),0(R6)                                                   
         LA    R6,75(R6)                                                        
         B     GETBX                                                            
*                                                                               
GETBLN1  MVI   BILSW,X'FF'         SET NO MORE LINES TO PRINT                   
GETBX    BR    R8                  RETURN                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
DISPROF  NTR                                                                    
         LA    R4,32                                                            
         L     R3,=A(PROLINES)                                                  
DISP1    XC    0(75,R3),0(R3)                                                   
         LA    R3,75(R3)                                                        
         BCT   R4,DISP1                                                         
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BE    DISPX                                                            
*                                                                               
         L     R8,=A(PROLINES)                                                  
         LR    R3,R8                                                            
         L     R6,=V(PROFTAB)                                                   
         A     R6,RELO                                                          
         BRAS  RE,DISPROF2                                                      
DISPX    XIT                                                                    
*                                                                               
* OUTPUT ESTIMATE USER DEFINITION DESCRIPTION                                   
*                                                                               
ESTUSER  NTR1                                                                   
*NOP*    GOTO1 =V(GETUSER),DMCB,(C'P',PCLTREC),(C'E',PESTREC),(C':',P+1         
*NOP*          9),0                                                             
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),(C':',P+19),X        
               0                                                                
         CLC   P+19(20),SPACES                                                  
         BE    SKIPEU1                                                          
         CLC   P+47(10),SPACES     SEE IF I CAN USE P+47                        
         BNE   ESTU5                                                            
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
ESTU5    BAS   R8,PRINTIT                                                       
*                                                                               
SKIPEU1  DS    0H                                                               
*NOP*    GOTO1 =V(GETUSER),DMCB,(C'P',PCLTREC),(C'E',PESTREC),0,(C':',P         
*NOP*          +19)                                                             
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),0,(C':',P+19X        
               )                                                                
         CLC   P+19(20),SPACES                                                  
         BE    ESTUX                                                            
         CLC   P+47(10),SPACES     SEE IF I CAN USE P+47                        
         BNE   ESTU10                                                           
         BAS   R8,GETBLN                                                        
         CLI   BILSW,X'FF'                                                      
         BNE   *+8                                                              
         BAS   R8,GETPLN                                                        
ESTU10   BAS   R8,PRINTIT                                                       
*                                                                               
ESTUX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* OUTPUT PRODUCT USER DEFINITION DESCRIPTION                                    
*                                                                               
PRDUSER  NTR1                                                                   
         BRAS  RE,DOWNUSER                                                      
         BE    PRDUX                                                            
*                                                                               
*NOP*    GOTO1 =V(GETUSER),DMCB,(C'P',PCLTREC),(C'P',PPRDREC),(C':',P+1         
*NOP*          6),0                                                             
PRDU3    GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),(C':',P+16),X        
               0                                                                
         CLC   P+16(20),SPACES                                                  
         BE    PRDU10                                                           
         CLC   P+47(10),SPACES   SEE IF I CAN USE P+47                          
         BNE   PRDU5                                                            
         BAS   R8,GETBLN                                                        
PRDU5    BAS   R8,PRINTIT                                                       
*                                                                               
PRDU10   DS    0H                                                               
*NOP*    GOTO1 =V(GETUSER),DMCB,(C'P',PCLTREC),(C'P',PPRDREC),0,(C':',P         
*NOP*          +16)                                                             
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),0,(C':',P+16X        
               )                                                                
         CLC   P+16(20),SPACES                                                  
         BE    PRDUX                                                            
         CLC   P+47(10),SPACES   SEE IF I CAN USE P+47                          
         BNE   PRDU20                                                           
         BAS   R8,GETBLN                                                        
PRDU20   BAS   R8,PRINTIT                                                       
*                                                                               
PRDUX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY PST CODES (R1=A(PST CODES))                                           
*                                                                               
DISPPST  NTR1                      R2 POINTS TO PCLTREC OR PPRDREC              
         XC    PSTOUT,PSTOUT                                                    
**                                                                              
**       NOTE:  ELCODE2 SET ALREADY                                             
**                                                                              
         BAS   RE,NEXTEL                                                        
         BNE   DPX                                                              
*                                                                               
         LA    R1,2(R2)            POINT R1 TO PSTCODES                         
         LA    R2,PSTBLK                                                        
         USING PSTBLKD,R2                                                       
         XC    0(PSTLNQ,R2),0(R2)  CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 APSTVAL,DMCB,(R2)                                                
         CLI   ELCODE2,X'26'       AM I DOING MAIN PST?                         
         BE    DP5N                                                             
         CLI   PSTOUT,C' '         SEE IF I HAVE PST CODES                      
         BNH   DPX                                                              
         MVC   0(3,R3),=C'PST'                                                  
         MVC   4(49,R3),PSTOUT                                                  
         B     DPX                                                              
*                                                                               
DP5N     DS    0H                                                               
         MVC   0(8,R3),=C'MAIN PST'                                             
         MVC   10(12,R3),=C'-NOT ENTERED'                                       
         CLI   PSTOUT,C' '         SEE IF I HAVE PST CODES                      
         BNH   DPX                                                              
         MVC   10(12,R3),SPACES                                                 
         MVC   10(49,R3),PSTOUT                                                 
         B     DPX                                                              
                                                                                
* NOTE: THERE WILL BE A PROBLEM WHEN THERE ARE MORE THAN ABOUT 7 PST            
*       CODES FOR CLTHDRS (THEY MIGHT GET CLOBBERD BY A PROFILE LINE)           
*                                                                               
         DROP  R2                                                               
*                                                                               
DPX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
MYHOOK   NTR1                                                                   
         BRAS  RE,MYHOOK1                                                       
         J     XIT                                                              
*                                  CLIENT GROUP ASSIGNMENTS                     
CGAPRT   NTR1                                                                   
         MVC   KEYSAVE,KEY         STORE CURRENT KEY                            
         MVI   PGRPSW,2            SET PRD GRP SW TO FIRST TIME                 
         CLI   CGRPSW,2            FIRST TIME THRU FOR CLIENT ?                 
         BNE   CGATSTG             NO                                           
         MVI   CGRPSW,0            YES                                          
         XC    KEY,KEY                                                          
         LA    R2,PCLTREC                                                       
         LA    R4,KEY                                                           
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,0(R2)       AGENCY FROM CLT REC                          
         MVC   GRPPMED,2(R2)       MEDIA  FROM CLT REC                          
         MVI   GRPPTYP,GRPPCGQ     CLIENT GROUP                                 
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
* GET FIRST PASSIVE POINTER                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEPKEY(4),GRPPKEY ANY CLT GRPS FOR THIS AGY/MED?               
         BE    CGATSTG             YES                                          
         MVI   CGRPSW,1            NO - DO NOT PRINT "NONE FOUND"               
*                                                                               
CGATSTG  CLI   CGRPSW,1            ANY CLT GRPS FOR THIS AGY/MED?               
         BE    CGAXIT              NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,PCLTREC                                                       
         LA    R4,KEY                                                           
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,0(R2)       AGENCY FROM CLT REC                          
         MVC   GRPPMED,2(R2)       MEDIA  FROM CLT REC                          
         MVI   GRPPTYP,GRPPCGQ     CLIENT GROUP                                 
         MVC   GRPPVAL(6),SPACES   CLT IS BLANK PADDED                          
         MVC   GRPPVAL(3),4(R2)    CLT    FROM CLT REC                          
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
* GET PASSIVE POINTER                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,3                                                             
         STC   R0,SAVELINE         SKIP TO NEW PAGE IF FEWER                    
         CLC   SAVELINE,MAXLINES   THAN 4 LINES LEFT ON PAGE                    
         BL    CGAGO               NO NEW PAGE                                  
         MVI   FORCEHED,C'Y'       PRINT HEADLINES                              
*                                                                               
CGAGO    BAS   R8,PRINTIT          PRINT BLANK LINE                             
*                                                                               
         MVC   P+13(24),=C'CLIENT GROUP ASSIGNMENTS'                            
         MVC   PSECOND+13(24),=25C'-'                                           
         CLC   SAVEPKEY(13),GRPPKEY                                             
         BE    CGAHDR              SAME CLIENT, SET UP TITLES                   
*                                                                               
         MVC   P+48(18),=C'*** NONE FOUND ***'                                  
         BAS   R8,PRINTIT                                                       
         B     CGAXIT              DONE - NO ASSIGNMENTS                        
*                                                                               
CGAHDR   MVC   P+48(28),=C'ID CODE  BREAK 1      NAME 1'                        
         MVC   P+95(19),=C'BREAK 2      NAME 2'                                 
         MVC   PSECOND+48(28),=C'-- ----  -------      ------'                  
         MVC   PSECOND+95(19),=C'-------      ------'                           
         BAS   R8,PRINTIT                                                       
*                                                                               
CGALOOP  CLC   SAVEPKEY(13),GRPPKEY                                             
         BNE   CGAXIT              CLIENT DIFFERENT, DONE                       
         MVC   SAVEPKEY,GRPPKEY                                                 
         LA    R3,P+48             LINE AREA                                    
         USING CGALIND,R3                                                       
         MVC   CGALIN,SPACES                                                    
*                                                                               
         MVC   SAVEID,GRPPID       GROUP ID/CODE FROM PASSIVE PTR               
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         ICM   R6,B'1100',GRPPCODE FROM PASSIVE PTR                             
         SRL   R6,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R6,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CODECHAR(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVC   GRPKAGY,0(R2)       AGENCY FROM CLT REC                          
         MVC   GRPKMED,2(R2)       MEDIA  FROM CLT REC                          
         MVC   GRPKID,SAVEID       ID FROM PASSIVE PTR                          
******** MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKRCOD,GRPKCTYQ   CLT GROUP                                    
         MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
* GET GROUP DEFINITION RECORD                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
*                                                                               
*NOP*    GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PCONREC,DMWORK                
*                                                                               
         L     R4,ACONIO1          I/O FOR GRP DEF REC (PCONREC)                
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,(R4),DMWORK                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    LA    R4,PCONREC          I/O FOR GROUP DEFINITION RECORD              
         LA    R4,33(R4)                                                        
         USING GRPBRKD,R4                                                       
         CLI   GRPBRKCD,GRPBRKCQ   BREAK DESCRIPTION ELEMENT                    
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R6,GRPBK1LN         L'BREAK CODES                                
         ZIC   R0,GRPBK2LN                                                      
         AR    R6,R0               L'WHOLE GROUP CODE                           
         BCTR  R6,0                                                             
         EX    R6,CGAEX                                                         
         B     CGA10                                                            
CGAEX    MVC   CGACODE(0),CODECHAR+1         CODE TO LINE BLANK PADDED          
*                                                                               
CGA10    MVC   CGABRK1,GRPBK1      BREAK TITLES TO LINE                         
         OC    GRPBK2,GRPBK2       MAY BE ONLY ONE                              
         BZ    CGA20                                                            
         MVC   CGABRK2,GRPBK2                                                   
         DROP  R4                                                               
*                                                                               
CGA20    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         MVC   GRPKAGY,0(R2)       AGENCY FROM CLT REC                          
         MVC   GRPKMED,2(R2)       MEDIA  FROM CLT REC                          
         MVC   GRPKID,SAVEID       ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKCTYQ   CLT GROUP                                    
         MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
* GET GROUP RECORD                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
*NOP*    GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PCONREC,DMWORK                
*                                                                               
         L     R4,ACONIO1          I/O FOR GRP DEF REC (PCONREC)                
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,(R4),DMWORK                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    LA    R4,PCONREC          I/O FOR GROUP CODE RECORD                    
         LA    R4,33(R4)                                                        
         USING GRPGRPD,R4                                                       
         CLI   GRPGRPCD,GRPGRPCQ   BREAK NAMES ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   CGANAME1,GRPGNAM1   GROUP NAMES TO LINE                          
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    CGA30                                                            
         MVC   CGANAME2,GRPGNAM2                                                
*                                                                               
CGA30    DS    0H                  PRINT THE LINE                               
*                                  USE TABLE TO TRANSLATE                       
*                                  MAY TRANSLATE TO 2 CHARACTERS                
         LA    RE,SPCGRTAB                                                      
         LHI   RF,SPCGRTBX-SPCGRTAB                                             
CGA32    CLC   SAVEID,2(RE)                                                     
         BE    CGA33                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,CGA32                                                         
*                                  NOT FOUND IN TABLE                           
*                                  JUST MOVE TO LINE                            
         MVC   CGAID,SAVEID        GROUP ID TO LINE                             
         B     CGA34                                                            
*                                                                               
CGA33    MVC   CGAID(2),0(RE)      GROUP ID TO LINE                             
*                                                                               
CGA34    BAS   R8,PRINTIT          PRINT THE LINE                               
         MVC   P,SPACES                                                         
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEPKEY(25),KEY                                                 
         BE    CGA40                                                            
         DC    H'0'                                                             
*                                                                               
CGA40    DS    0H                  NEXT PASSIVE POINTER                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         B     CGALOOP                                                          
*                                                                               
CGAXIT   DS    0H                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     XIT                 ??????? - DO READ HIGH - ???????             
         EJECT                                                                  
*                                  PRODUCT GROUP ASSIGNMENTS                    
         SPACE 2                                                                
PGAPRT   NTR1                                                                   
         BRAS  RE,PGAPRT1                                                       
         JNE   PGAXIT                                                           
*                                                                               
         LA    R2,PPRDREC                                                       
*                                                                               
* GET PASSIVE POINTER                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,3                                                             
         STC   R0,SAVELINE         SKIP TO NEW PAGE IF FEWER                    
         CLC   SAVELINE,MAXLINES   THAN 4 LINES LEFT ON PAGE                    
         JL    PGAGO               NO NEW PAGE                                  
         MVI   FORCEHED,C'Y'       PRINT HEADLINES                              
*                                                                               
PGAGO    BAS   R8,PRINTIT          PRINT BLANK LINE                             
         MVC   P+17(24),=C'CLIENT GROUP ASSIGNMENTS'                            
         MVC   P+16(7),=C'PRODUCT'                                              
         MVC   PSECOND+16(25),=25C'-'                                           
         CLC   SAVEPKEY(13),GRPPKEY                                             
         BE    PGAHDR              SAME PRD, SET UP TITLES                      
*                                                                               
         MVC   P+48(18),=C'*** NONE FOUND ***'                                  
         BAS   R8,PRINTIT                                                       
         B     PGAXIT              DONE - NO ASSIGNMENTS                        
*                                                                               
PGAHDR   MVC   P+48(28),=C'ID CODE  BREAK 1      NAME 1'                        
         MVC   P+95(19),=C'BREAK 2      NAME 2'                                 
         MVC   PSECOND+48(28),=C'-- ----  -------      ------'                  
         MVC   PSECOND+95(19),=C'-------      ------'                           
         BAS   R8,PRINTIT                                                       
*                                                                               
PGALOOP  CLC   SAVEPKEY(13),GRPPKEY                                             
         BNE   PGAXIT              DIFFERENT PRD, DONE                          
         MVC   SAVEPKEY,GRPPKEY                                                 
         LA    R3,P+48             LINE AREA                                    
         USING CGALIND,R3                                                       
         MVC   CGALIN,SPACES                                                    
*                                                                               
         MVC   SAVEID,GRPPID       GROUP ID/CODE FROM PASSIVE PTR               
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         ICM   R6,B'1100',GRPPCODE FROM PASSIVE PTR                             
         SRL   R6,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R6,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CODECHAR(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVC   GRPKAGY,0(R2)       AGENCY FROM PRD REC                          
         MVC   GRPKMED,2(R2)       MEDIA  FROM PRD REC                          
         MVC   GRPKCLT,4(R2)       CLIENT FROM PRD REC                          
         MVC   GRPKID,SAVEID       ID FROM PASSIVE PTR                          
******** MVC   GRPKCODE,=X'0000'   GROUP DEF RECORD                             
         MVI   GRPKRCOD,GRPKPTYQ   PRD GROUP                                    
         MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
* GET GROUP DEFINITION RECORD                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
*                                                                               
*NOP*    GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PCONREC,DMWORK                
*                                                                               
         L     R4,ACONIO1          I/O FOR GRP DEF REC (PCONREC)                
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,(R4),DMWORK                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    LA    R4,PCONREC          I/O FOR GROUP DEFINITION RECORD              
         LA    R4,33(R4)                                                        
         USING GRPBRKD,R4                                                       
         CLI   GRPBRKCD,GRPBRKCQ   BREAK DESCRIPTION ELEMENT                    
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R6,GRPBK1LN         L'BREAK CODES                                
         ZIC   R0,GRPBK2LN                                                      
         AR    R6,R0               L'WHOLE GROUP CODE                           
         BCTR  R6,0                                                             
         EX    R6,PGAEX                                                         
         B     PGA10                                                            
PGAEX    MVC   CGACODE(0),CODECHAR+1         CODE TO LINE BLANK PADDED          
*                                                                               
PGA10    MVC   CGABRK1,GRPBK1      BREAK TITLES TO LINE                         
         OC    GRPBK2,GRPBK2       MAY BE ONLY ONE                              
         BZ    PGA20                                                            
         MVC   CGABRK2,GRPBK2                                                   
         DROP  R4                                                               
*                                                                               
PGA20    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         MVC   GRPKAGY,0(R2)       AGENCY FROM PRD REC                          
         MVC   GRPKMED,2(R2)       MEDIA  FROM PRD REC                          
         MVC   GRPKCLT,4(R2)       CLIENT FROM PRD REC                          
         MVC   GRPKID,SAVEID       ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKPTYQ   PRD GROUP                                    
         MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
* GET GROUP RECORD                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
*NOP*    GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PCONREC,DMWORK                
*                                                                               
         L     R4,ACONIO1          I/O FOR GRP DEF REC (PCONREC)                
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,(R4),DMWORK                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*NOP*    LA    R4,PCONREC          I/O FOR GROUP CODE RECORD                    
         LA    R4,33(R4)                                                        
         USING GRPGRPD,R4                                                       
         CLI   GRPGRPCD,GRPGRPCQ   BREAK NAMES ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   CGANAME1,GRPGNAM1   GROUP NAMES TO LINE                          
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    PGA30                                                            
         MVC   CGANAME2,GRPGNAM2                                                
*                                                                               
PGA30    DS    0H                  PRINT THE LINE                               
         MVC   CGAID,SAVEID        GROUP ID TO LINE                             
         BAS   R8,PRINTIT          PRINT THE LINE                               
         MVC   P,SPACES                                                         
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEPKEY(25),KEY                                                 
         BE    PGA40                                                            
         DC    H'0'                                                             
*                                                                               
PGA40    DS    0H                  NEXT PASSIVE POINTER                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         B     PGALOOP                                                          
*                                                                               
PGAXIT   DS    0H                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         B     XIT                 ??????? - DO READ HIGH - ???????             
         EJECT                                                                  
*                                                                               
PCLI     DS    CL3                                                              
SCLTSTAT DS    CL1                 SAVED CLIENT SFH STATUS BYTE                 
PPROD    DS    CL3                                                              
*                                                                               
BILSW    DS    CL1                                                              
CESW     DS    CL1                                                              
ESPSW    DS    CL1                                                              
CGRPSW   DC    X'2'                                                             
PGRPSW   DC    X'2'                                                             
CLIPRTSW DC    X'0'                SWITCH TO CHECK IF CLIENT IS PRINTED         
SAVEKEY  DS    CL32                                                             
*                                                                               
ELCODE2  DS    CL1                                                              
PROFILE  DS    CL32                                                             
*                                                                               
SAVEID   DS    C                   GROUP ID                                     
SAVECODE DS    XL2                 GROUP CODE (PWOS)                            
CODECHAR DS    XL5                 UNPK AREA                                    
SAVEPKEY DS    XL32                PASSIVE POINTER - PUB GRP ASSIGNS            
SAVEKKEY DS    XL32                PUB GROUP ID DEFINITION                      
SAVENKEY DS    XL32                PUB GROUP CODE                               
*                                                                               
APSTVAL  DS    A                                                                
VGETUSER DS    A                                                                
PSTBLK   DS    CL(PSTLNQ)                                                       
PSTOUT   DS    CL64                                                             
*                                                                               
VPRNTOFC DS    V                   V(PRNTOFC)                                   
VOFFICER DS    V                   A(OFFICER)                                   
*                                                                               
DOWNLOAD DS    CL1                 DOWNLOAD? Y/N                                
*                                                                               
DLDATA   DS    0D                  DOWNLOAD DATA                                
DLMED    DS    CL1                 MEDIA                                        
DLCLT    DS    CL3                 CLIENT                                       
DLCLTN   DS    CL(L'PCLTNAME)      CLIENT NAME                                  
DLADD1   DS    CL(L'PCLTLIN1)      ADDR LINE 1                                  
DLADD2   DS    CL(L'PCLTLIN2)      ADDR LINE 2                                  
DLADD3   DS    CL(L'PCLTATTN)      ADDR LINE 3                                  
DLADD4   DS    CL(L'PCLTBNAM)      ADDR LINE 4                                  
DLOFF    DS    CL(L'PCLTOFF)       OFFICE                                       
DLAOFF   DS    CL(L'PCLTAOFC)      ACC OFFICE                                   
DLFRZ    DS    CL3                 FROZEN FLAG                                  
DLCOS2   DS    CL8                 COS2 FACTOR                                  
DLINTF   DS    CL5                 INTERFACE #                                  
DLTOFC   DS    CL1                 TRAFFIC OFFICE CODE                          
DLCCOMM  DS    CL6                 CONTRACT STANDARD COMMENT                    
DLICOM1  DS    CL6                 I/O STANDARD COMMENT 1                       
DLICOM2  DS    CL6                 I/O STANDARD COMMENT 2                       
DLBGRP   DS    CL1                 BILLING GROUP                                
DLCDIV   DS    CL1                 CLIENT DIVISIONS                             
DLPRD    DS    CL3                 PRODUCT CODE                                 
DLPRDNM  DS    CL(L'PPRDNAME)      PRODUCT NAME                                 
DLADD1P  DS    CL(L'PPRDLIN1)      ADDR LINE 1                                  
DLADD2P  DS    CL(L'PPRDLIN2)      ADDR LINE 2                                  
DLADD3P  DS    CL(L'PPRDATTN)      ADDR LINE 3                                  
DLADD4P  DS    CL(L'PPRDBILL)      ADDR LINE 4                                  
DLADD5P  DS    CL(L'PPRDBIL2)      ADDR LINE 5                                  
DLACCNO  DS    CL5                 ACCOUNT NUMBER                               
DLPDIV   DS    CL(L'PPRDDIV)       DIVISION NUMBER                              
DLBILLFP DS    CL26                BILL FORMULA ON PRODUCT                      
DLEFFDT  DS    CL6                 BILL FORMULA EFFECTIVE DATE                  
DLBCOM1  DS    CL10                STANDARD COMMENT ON BILL 1                   
DLBCOM2  DS    CL10                STANDARD COMMENT ON BILL 2                   
DLBCOM3  DS    CL10                STANDARD COMMENT ON BILL 3                   
DLUSRT1P DS    CL21                UDEF PRODUCT DESCRIPTION 1                   
DLUSRD1P DS    CL32                PRODUCT UDEF 1 ON PRODUCT RECORD             
DLUSRT2P DS    CL21                UDEF PRODUCT DESCRIPTION 2                   
DLUSRD2P DS    CL32                PRODUCT UDEF 2 ON PRODUCT RECORD             
DLUCOM1  DS    CL32                PRODUCT UCOMM DATA #1                        
DLUCOM2  DS    CL32                PRODUCT UCOMM DATA #2                        
DLUCOM3  DS    CL32                PRODUCT UCOMM DATA #3                        
DLUCOM4  DS    CL32                PRODUCT UCOMM DATA #4                        
DLPRDLNQ EQU   *-DLPRD             PRODUCT DOWNLOAD DATA LENGTH                 
DLDATLNQ EQU   *-DLDATA                                                         
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DOWNUSER NTR1  BASE=*,LABEL=*                                                   
         CLI   DOWNLOAD,C'Y'     IF DOWNLOADING DON'T SQUASH TOGETHER           
         JNE   NO                                                               
         GOTOR VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),(0,DLUSRT1P)X        
               ,0                                                               
         GOTOR VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),0,(0,DLUSRT2X        
               P)                                                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLRDOWN  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,DLDATA           START                                        
         LHI   R1,DLDATLNQ         LENGTH TO CLEAR                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLRDWNP  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,DLPRD            START                                        
         LHI   R1,DLPRDLNQ         LENGTH TO CLEAR                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DLCLADD  NTR1  BASE=*,LABEL=*                                                   
         MVC   DLADD1,PCLTLIN1                                                  
         MVC   DLADD2,PCLTLIN2                                                  
         MVC   DLADD3,PCLTATTN                                                  
         MVC   DLADD4,PCLTBNAM                                                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PRINTITB NTR1  BASE=*,LABEL=*                                                   
         CLI   DOWNLOAD,C'Y'       IF DOWNLOADING, SKIP PRINTING                
         BNE   PRNT3                                                            
         BRAS  RE,CLRPLNS          CLEAR PRINT LINES                            
         B     PRNTXX                                                           
                                                                                
PRNT3    CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST                     
         BNE   PRNT5                                                            
         CLI   QCLIENT+1,C'*'      ALL OFFICES IN OFFICE ORDER?                 
         BE    PRNT15                                                           
         MVC   HEAD2(11),=C'OFFICE LIST'                                        
         MVC   HEAD2+12(1),QCLIENT+1                                            
         B     PRNT15                                                           
*                                                                               
PRNT5    CLI   QCLIENT,C'&&'       SEE IF DOING GROUPS                          
         BNE   PRNT10                                                           
         MVC   HEAD3(5),=C'GROUP'                                               
         MVC   HEAD3+6(1),QCLIENT+1                                             
         B     PRNT20                                                           
*                                                                               
PRNT10   CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   PRNT20                                                           
         CLI   QCLIENT+1,C'-'      UNLESS ALL EXCEPT ONE OFFICE                 
         BE    PRNT20                                                           
*                                                                               
PRNT15   MVC   HEAD3(6),=C'OFFICE'                                              
         OC    PCLTOFF,PCLTOFF                                                  
         BZ    PRNT20                                                           
         GOTOR VPRNTOFC,DMCB,PCLTOFF,(C'L',HEAD3+7),VOFFICER,QAGENCY,  X        
               VCOMFACS                                                         
*                                                                               
PRNT20   GOTO1 REPORT                                                           
PRNTXX   J     XIT                                                              
*                                                                               
PGAPRT1  NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE,KEY         STORE CURRENT KEY                            
         CLI   PGRPSW,2            FIRST TIME THRU ?                            
         BNE   PGATSTG             NO                                           
         MVI   PGRPSW,0            YES                                          
         XC    KEY,KEY                                                          
         LA    R2,PPRDREC                                                       
         LA    R4,KEY                                                           
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,0(R2)       AGENCY FROM PRD REC                          
         MVC   GRPPMED,2(R2)       MEDIA  FROM PRD REC                          
         MVC   GRPPCLT,4(R2)       CLIENT FROM PRD REC                          
         MVI   GRPPTYP,GRPPPGQ     PRODUCT GROUP                                
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
* GET FIRST PASSIVE POINTER                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SAVEPKEY(7),GRPPKEY ANY PRD GRPS FOR THIS AGY/MED/CLT ?          
         BE    PGATSTG             YES                                          
         MVI   PGRPSW,1            NO - DO NOT PRINT "NONE FOUND"               
*                                                                               
PGATSTG  CLI   PGRPSW,1            ANY PRD GRPS FOR THIS AGY/MED/CLT ?          
         JE    NO                  NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,PPRDREC                                                       
         LA    R4,KEY                                                           
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,0(R2)       AGENCY FROM PRD REC                          
         MVC   GRPPMED,2(R2)       MEDIA  FROM PRD REC                          
         MVC   GRPPCLT,4(R2)       CLIENT FROM PRD REC                          
         MVI   GRPPTYP,GRPPPGQ     PRODUCT GROUP                                
         MVC   GRPPVAL(6),SPACES   PRD IS BLANK PADDED                          
         MVC   GRPPVAL(3),7(R2)    PRD    FROM PRD REC                          
         MVC   SAVEPKEY,GRPPKEY                                                 
         J     YES                                                              
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*                                                                               
DISPROF2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
DISP2    MVC   *+7(1),CESW         SET MASK IN NEXT INST                        
         TM    4(R6),0                                                          
         BNO   NEXTB1              GET NEXT POSITION                            
         LA    R7,PROFILE-1                                                     
         LR    R3,R8                                                            
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         AR    R7,R1                                                            
         B     DISP3                                                            
*                                                                               
*                                                                               
NEXTB    CLI   5(R6),0                                                          
         BE    NEXTB1              NO FILTER CHAR                               
         CLC   0(1,R7),5(R6)       CHECK FILTER CHARACTER                       
         BE    NEXTB1                                                           
         SR    R1,R1                                                            
         IC    R1,6(R6)                                                         
         SLL   R1,8                                                             
         IC    R1,7(R6)                                                         
         AR    R6,R1               SKIP TO THIS POSITION                        
         CLI   0(R6),X'FF'         END OF TABLE                                 
         BNE   DISP2                                                            
         B     DISPXX                                                           
*                                                                               
NEXTB1   SR    R1,R1               SKIP TO NEXT POSITION                        
         IC    R1,1(R6)                                                         
         SLL   R1,8                                                             
         IC    R1,2(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),X'FF'         END OF TABLE                                 
         BNE   DISP2                                                            
         B     DISPXX                                                           
*                                                                               
DISP3    LA    R2,8(R6)            SET R2 TO FIRST VALUE                        
DISP3B   CLI   0(R2),X'00'         END OF VALUES                                
         BE    DISP3X                                                           
         CLI   0(R2),X'FF'         ANY CHARACTER VALID                          
         BE    DISP3C                                                           
         CLC   0(1,R2),0(R7)       COMPARE VALUE TO PROFILE                     
         BNE   DISP3D                                                           
DISP3C   MVC   0(8,R3),=C'POSITION'                                             
         EDIT  (B1,0(R6)),(2,9(R3)),0                                           
         IC    R1,3(R6)            LENGTH OF FIELD                              
         BCTR  R1,0                                                             
         EX    R1,MOVEV            MOVE VALUE                                   
         B     *+10                                                             
*                                                                               
MOVEV    MVC   13(0,R3),0(R7)      EXECUTED                                     
*                                                                               
         LA    R3,13(R3)                                                        
         AR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'='                                                       
         IC    R1,1(R2)            LENGTH OF DISCRIPTION+2                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,MOVED            MOVE DISCRIPTION                             
         B     *+10                                                             
*                                                                               
MOVED    MVC   2(0,R3),2(R2)       EXECUTED                                     
*                                                                               
NEXTL    LA    R8,75(R8)           NEXT LINE                                    
         B     NEXTB                                                            
*                                                                               
DISP3D   SR    R1,R1               GET NEXT VALUE                               
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     DISP3B                                                           
*                                                                               
DISP3X   CLI   0(R7),C'0'          VALUE NOT FOUND IN TABLE                     
         BE    NEXTB               DEFAULT                                      
         MVC   0(8,R3),=C'POSITION'                                             
         EDIT  (B1,0(R6)),(2,9(R3)),0                                           
         IC    R1,3(R6)            LENGHT OF FIELD                              
         BCTR  R1,0                                                             
         EX    R1,MOVEV                                                         
         B     NEXTL                                                            
*                                                                               
DISPXX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        HOOK FOR DOWNLOAD TO SUPPRESS HEADERS                                  
*                                                                               
MYHOOK1  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         JCT   R0,*-10                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        CLEAR PRINT LINES FOR DOWNLOAD                                         
*                                                                               
CLRPLNS  NTR1  BASE=*,LABEL=*                                                   
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   PSECOND,SPACES                                                   
         J     XIT                                                              
*                                                                               
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
CHKEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PESTKEY,RE                                                       
*                                                                               
         LA    RF,PCLTREC                                                       
         CLI   BYTE,C'P'           PRODUCT LEVEL?                               
         BNE   *+8                                                              
         LA    RF,PPRDREC                                                       
*                                                                               
         MVC   PESTKAGY,0(RF)                                                   
         MVC   PESTKMED,2(RF)                                                   
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,4(RF)                                                   
         CLI   BYTE,C'P'           PRODUCT LEVEL?                               
         BNE   *+10                                                             
         MVC   PESTKPRD,7(RF)                                                   
         DROP  RE                                                               
*                                                                               
         MVC   WORK+32(L'KEY),KEY  FOR COMPARISON LATER                         
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   BYTE,C'C'           CLIENT LEVEL?                                
         BE    CHKE40                                                           
         CLI   BYTE,C'P'           PRODUCT LEVEL?                               
         BE    CHKE45                                                           
         DC    H'0'                NO OTHER POSSIBLE INDICATOR                  
*                                                                               
CHKE40   CLC   KEY(07),WORK+32     EST RECORD FOUND AT CLIENT LEVEL?            
         BE    CHKE50                                                           
         B     CHKE60                                                           
CHKE45   CLC   KEY(10),WORK+32     EST RECORD FOUND AT PRODUCT LEVEL?           
         BNE   CHKE60                                                           
CHKE50   MVI   BYTE,C'Y'           FLAG FOR FOUND                               
*                                                                               
CHKE60   MVC   KEY,WORK            RESTORE FOR PPG                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,999              FOR CONDITION CODE PURPOSE                   
         CLI   BYTE,C'Y'                                                        
         BNE   CHKEFND                                                          
*                                                                               
CHKEXX   DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
CHKEFND  LTR   RE,RE                                                            
*                                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRINT TRAFFIC = Y/N (PPRDSTAT IS X'20')                                       
*                                                                               
* PRINT LEGAL WARNING ROTATION CODES (IN PRD REC, X'40' ELEM)                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKNOTRA NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PPRDSTAT,X'20'      NO TRAFFIC STATUS IS ON FOR PRD?             
         BZ    CHKNTR40                                                         
         MVC   P+16(10),=C'TRAFFIC = '                                          
         MVC   P+16+10(03),=C'NO '                                              
         BAS   R8,NTRGETP                                                       
         BAS   R8,NTRPRINT                                                      
*                                                                               
CHKNTR40 LA    R2,PPRDREC+33                                                    
         MVI   ELCODE2,X'40'        LEGAL WARING ELEM CODE                      
         BAS   RE,NTNXTEL                                                       
         BNE   CHKNTR50                                                         
         USING PPRDLWEL,R2                                                      
         MVC   P+16(26),=C'LEGAL WARNINGS ROTATION = '                          
         MVC   P+16+26+00(1),PPRDROTA+0                                         
         MVI   P+16+26+01,C'1'                                                  
         MVI   P+16+26+02,C','                                                  
         MVC   P+16+26+03(1),PPRDROTA+1                                         
         MVI   P+16+26+04,C'2'                                                  
         MVI   P+16+26+05,C','                                                  
         MVC   P+16+26+06(1),PPRDROTA+2                                         
         MVI   P+16+26+07,C'3'                                                  
         MVI   P+16+26+08,C','                                                  
         MVC   P+16+26+09(1),PPRDROTA+3                                         
         MVI   P+16+26+10,C'4'                                                  
         BAS   R8,NTRGETP                                                       
         BAS   R8,NTRPRINT                                                      
         DROP  R2                                                               
*                                                                               
CHKNTR50 DS    0H                                                               
*                                                                               
         CLI   PPRDEXC,0           ANY EXCLUSION CLASS TO BE PRINTED?           
         BE    XCLASSXX                                                         
*                                                                               
         MVC   P+16(18),=C'EXCLUSION CLASS = '                                  
         MVC   P+45(27),=C'(B=BEER, W=WINE, L=LIQUOR, '                         
         MVC   P+72(23),=C'T=TOBACCO, C=CIGARETTE)'                             
         LA    RE,P+34                                                          
         LA    RF,EXCLTAB1                                                      
         MVC   FULL(1),PPRDEXC                                                  
*                                                                               
CKBITS   MVC   BYTE,FULL                                                        
         NC    FULL(1),0(RF)                                                    
         CLC   FULL(1),BYTE                                                     
         BE    NEXTBT                                                           
         MVC   0(1,RE),1(RF)                                                    
         OC    FULL(1),FULL                                                     
         BZ    XCLASS90                                                         
         MVI   1(RE),C','                                                       
         LA    RE,2(RE)                                                         
*                                                                               
NEXTBT   CLC   2(2,RF),=X'0000'                                                 
         BE    XCLASS90                                                         
         LA    RF,2(RF)                                                         
         OC    FULL(1),FULL                                                     
         BZ    XCLASS90                                                         
         B     CKBITS                                                           
*                                                                               
*                  B   W   L   T   C                                            
EXCLTAB1 DC    X'7FC2BFE6DFD3EFE3F7C30000'                                      
*                                                                               
*                                                                               
XCLASS90 BAS   R8,NTRGETP                                                       
         BAS   R8,NTRPRINT                                                      
*                                                                               
XCLASSXX DS    0H                                                               
*                                                                               
*                                                                               
CHKNTRX  XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
NTNXTEL  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NTNXTEL2                                                         
         CLC   0(1,R2),ELCODE2                                                  
         BER   RE                                                               
         LTR   R0,R0                                                            
         BNZ   NTNXTEL                                                          
         DC    H'0'                BAD RECORD                                   
NTNXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
NTRGETP  CLI   ESPSW,X'FF'         SAME AS GETPLN, USED IN CHKNOTRA             
         BE    NTRGETPX                                                         
         CLI   0(R7),0             NO MORE LINES TO PRINT                       
         BE    NTRGETP1                                                         
         MVC   P+48(75),0(R7)                                                   
         LA    R7,75(R7)           NEXT LINE                                    
         B     NTRGETPX                                                         
*                                                                               
NTRGETP1 MVI   ESPSW,X'FF'         SET NO MORE LINES TO PRINT                   
NTRGETPX BR    R8                  RETURN                                       
*                                                                               
*                                                                               
*                                                                               
NTRPRINT DS    0H                  SAME AS PRINTIT, USED IN CHKNOTRA            
*                                                                               
         CLI   DOWNLOAD,C'Y'       IF DOWNLOADING, SKIP PRINTING                
         BNE   NTRPRT03                                                         
         BRAS  RE,CLRPLNS          CLEAR PRINT LINES                            
         B     NTRPRTX                                                          
*                                                                               
NTRPRT03 CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST                     
         BNE   NTRPRT05                                                         
         CLI   QCLIENT+1,C'*'      ALL OFFICES IN OFFICE ORDER?                 
         BE    NTRPRT15                                                         
         MVC   HEAD2(11),=C'OFFICE LIST'                                        
         MVC   HEAD2+12(1),QCLIENT+1                                            
         B     NTRPRT15                                                         
*                                                                               
NTRPRT05 CLI   QCLIENT,C'&&'       SEE IF DOING GROUPS                          
         BNE   NTRPRT10                                                         
         MVC   HEAD3(5),=C'GROUP'                                               
         MVC   HEAD3+6(1),QCLIENT+1                                             
         B     NTRPRT20                                                         
*                                                                               
NTRPRT10 CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   NTRPRT20                                                         
         CLI   QCLIENT+1,C'-'      UNLESS ALL EXCEPT ONE OFFICE                 
         BE    NTRPRT20                                                         
*                                                                               
NTRPRT15 MVC   HEAD3(6),=C'OFFICE'                                              
         OC    PCLTOFF,PCLTOFF                                                  
         BZ    NTRPRT20                                                         
         GOTOR VPRNTOFC,DMCB,PCLTOFF,(C'L',HEAD3+7),VOFFICER,QAGENCY,  X        
               VCOMFACS                                                         
*                                                                               
NTRPRT20 GOTO1 REPORT                                                           
NTRPRTX  BR    R8                                                               
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRINT PRODUCT OFFICE AND TRAFFIC CODES                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKPRDOT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PPRDOFFC,0          ANY PRD OFFICE CODE TO BE PRINTED?           
         BE    CHKPOT50            NO, CHECK FOR PRD TRAFFIC CODE               
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+16(21),=C'PRODUCT OFFICE CODE ='                               
*                                                                               
*              PRINT PRODUCT OFFICE CODE                                        
*                                                                               
         GOTOR VPRNTOFC,DMCB,PPRDOFFC,P+16+22,VOFFICER,QAGENCY,VCOMFACS         
*                                                                               
         BAS   R8,POTGETP                                                       
         BAS   R8,POTPRINT                                                      
*                                                                               
CHKPOT50 CLI   PPRDTRAF,0          ANY PRD TRAFFIC CODE TO BE PRINTED?          
         BE    CHKPOTX             NO, DONE WITH OFFIEC/TRAFFIC CODES           
         MVC   P,SPACES                                                         
         MVC   P+16(22),=C'PRODUCT TRAFFIC CODE ='                              
*                                                                               
*              PRINT PRODUCT TRAFFIC OFFICE CODE                                
*                                                                               
         GOTOR VPRNTOFC,DMCB,PPRDTRAF,P+16+23,VOFFICER,QAGENCY,VCOMFACS         
*                                                                               
         BAS   R8,POTGETP                                                       
         BAS   R8,POTPRINT                                                      
*                                                                               
         B     CHKPOTX             DONE                                         
*                                                                               
CHKPOTX  XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POTGETP  CLI   ESPSW,X'FF'         SAME AS GETPLN, USED IN CHKPRDOT             
         BE    POTGETPX                                                         
         CLI   0(R7),0             NO MORE LINES TO PRINT                       
         BE    POTGETP1                                                         
         MVC   P+48(75),0(R7)                                                   
         LA    R7,75(R7)           NEXT LINE                                    
         B     POTGETPX                                                         
*                                                                               
POTGETP1 MVI   ESPSW,X'FF'         SET NO MORE LINES TO PRINT                   
POTGETPX BR    R8                  RETURN                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POTPRINT DS    0H                  SAME AS PRINTIT, USED IN CHKPRDOT            
*                                                                               
         CLI   DOWNLOAD,C'Y'       IF DOWNLOADING, SKIP PRINTING                
         BNE   POTPRT03                                                         
         BRAS  RE,CLRPLNS          CLEAR PRINT LINES                            
         B     POTPRTX                                                          
*                                                                               
POTPRT03 CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST                     
         BNE   POTPRT05                                                         
         CLI   QCLIENT+1,C'*'      ALL OFFICES IN OFFICE ORDER?                 
         BE    POTPRT15                                                         
         MVC   HEAD2(11),=C'OFFICE LIST'                                        
         MVC   HEAD2+12(1),QCLIENT+1                                            
         B     POTPRT15                                                         
*                                                                               
POTPRT05 CLI   QCLIENT,C'&&'       SEE IF DOING GROUPS                          
         BNE   POTPRT10                                                         
         MVC   HEAD3(5),=C'GROUP'                                               
         MVC   HEAD3+6(1),QCLIENT+1                                             
         B     POTPRT20                                                         
*                                                                               
POTPRT10 CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   POTPRT20                                                         
         CLI   QCLIENT+1,C'-'      UNLESS ALL EXCEPT ONE OFFICE                 
         BE    POTPRT20                                                         
*                                                                               
POTPRT15 MVC   HEAD3(6),=C'OFFICE'                                              
         OC    PCLTOFF,PCLTOFF                                                  
         BZ    POTPRT20                                                         
*                                                                               
*              PRINT OFFICE CODE                                                
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,(C'L',HEAD3+7),VOFFICER,QAGENCY,  X        
               VCOMFACS                                                         
*                                                                               
POTPRT20 GOTO1 REPORT                                                           
POTPRTX  BR    R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRINT UCOMM INFORMATION                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKUCOMM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,CHKUC50          SET UP PARAMETERS FOR UCOMM CALL             
         GOTO1 =V(DDUCOM),WKAREA                                                
         LA    R7,WKAREA                                                        
         USING DDUCOMD,R7                                                       
*                                                                               
         CLI   BYTE,C'C'           CLIENT?                                      
         BNE   *+16                                                             
         TM    UCDATA,UCDNOCLT                                                  
         BO    CHKUCX              NO CLT TITLE FOUND, DONE                     
         B     CHKUC10                                                          
*                                                                               
         CLI   BYTE,C'P'           PRODUCT?                                     
         BNE   *+16                                                             
         TM    UCDATA,UCDNOPRD                                                  
         BO    CHKUCX              NO PRD LEVEL FOUND, DONE                     
         B     CHKUC10D                                                         
*                                                                               
         CLI   BYTE,C'E'           ESTIMATE?                                    
         BNE   *+16                                                             
         TM    UCDATA,UCDNOEST                                                  
         BO    CHKUCX              NO EST LEVEL FOUND, DONE                     
         B     CHKUC30                                                          
*                                                                               
         DC    H'0'                INVALID INDICATOR IN BYTE                    
*                                                                               
CHKUC10  CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    CHKUC10H                                                         
CHKUC10D OC    UCPLENS,UCPLENS     CHECKING PRD LEVEL                           
         BZ    CHKUCX                                                           
         CLI   UCPLENS+0,X'00'     DO FIRST PAIR (PRD)                          
         BE    CHKUC25D                                                         
CHKUC10H MVC   DOUBLE+0(4),UCPTTLS                                              
         MVC   DOUBLE+4(4),UCPDATA                                              
         MVC   THREE(1),UCPLENS+0                                               
         BRAS  RE,DOWNUCOM         DOWNLOAD UCOMM                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC25D CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCPLENS+1,X'00'     DO SECOND PAIR (PRD)                         
         BE    CHKUC25E                                                         
         L     RE,UCPTTLS                                                       
         LA    RE,20(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCPDATA                                                       
         LA    RE,32(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCPLENS+1                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC25E CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCPLENS+2,X'00'     DO THRID PAIR (PRD)                          
         BE    CHKUC25F                                                         
         L     RE,UCPTTLS                                                       
         LA    RE,40(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCPDATA                                                       
         LA    RE,64(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCPLENS+2                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC25F CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCPLENS+3,X'00'     DO FOURTH PAIR (PRD)                         
         BE    CHKUC30                                                          
         L     RE,UCPTTLS                                                       
         LA    RE,60(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCPDATA                                                       
         LA    RE,96(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCPLENS+3                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC30  CLI   BYTE,C'P'           PRODUCT LEVEL?                               
         BE    CHKUCX              ALL PRD LEVEL IS DONE NOW                    
         CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    CHKUC30H                                                         
         OC    UCELENS,UCELENS     CHECKING EST LEVEL                           
         BZ    CHKUCX                                                           
         CLI   UCELENS+0,X'00'     DO FIRST PAIR (EST)                          
         BE    CHKUC35D                                                         
CHKUC30H MVC   DOUBLE+0(4),UCETTLS                                              
         MVC   DOUBLE+4(4),UCEDATA                                              
         MVC   THREE(1),UCELENS+0                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC35D CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCELENS+1,X'00'     DO SECOND PAIR (EST)                         
         BE    CHKUC35E                                                         
         L     RE,UCETTLS                                                       
         LA    RE,20(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCEDATA                                                       
         LA    RE,32(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCELENS+1                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC35E CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCELENS+2,X'00'     DO THIRD PAIR (EST)                          
         BE    CHKUC35F                                                         
         L     RE,UCETTLS                                                       
         LA    RE,40(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCEDATA                                                       
         LA    RE,64(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCELENS+2                                               
         BAS   RE,CHKUC90          PROCESS UCOMM                                
*                                                                               
CHKUC35F CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    *+12                                                             
         CLI   UCELENS+3,X'00'     DO FOURTH PAIR (EST)                         
         BE    CHKUCX                                                           
         L     RE,UCETTLS                                                       
         LA    RE,40(RE)                                                        
         ST    RE,DOUBLE                                                        
         L     RE,UCEDATA                                                       
         LA    RE,64(RE)                                                        
         ST    RE,DOUBLE+4                                                      
         MVC   THREE(1),UCELENS+3                                               
         BAS   R2,CHKUC40          PROCESS UCOMM W/O RESTORE UCOMM BLK          
         B     CHKUCX                                                           
         DROP  R7                  DONE WITH USING UCOMM BLK                    
*                                                                               
* MUST PASS DESIRED PAIRS AND ADDRS AS PARAMETERS (DOUBLE AND THREE)            
*                                                                               
CHKUC40  DS    0H                  PUT TITLE AND DATA FIELDS ON P               
         L     R3,DOUBLE           GET ADDRESS OF DESIRED TITLE                 
         OC    0(20,R3),0(R3)      ANYTHING IN TITLE?                           
         BZ    CHKUC40X            NOTHING, DONE                                
*                                                                               
         XC    P,P                 NEED TO HAVE BINARY ZEROS                    
*                                                                               
         CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BNE   *+20                                                             
         MVC   P(13),SPACES                                                     
         MVC   P+13(20),0(R3)      PUT UCOM PRD TITLE INTO P                    
         B     CHKUC40D                                                         
         CLI   BYTE,C'P'           PRODUCT LEVEL?                               
         BNE   *+20                                                             
         MVC   P(16),SPACES                                                     
         MVC   P+16(20),0(R3)      PUT UCOM PRD TITLE INTO P                    
         B     CHKUC40D                                                         
         CLI   BYTE,C'E'           ESTIMATE LEVEL?                              
         BNE   *+20                                                             
         MVC   P(19),SPACES                                                     
         MVC   P+19(20),0(R3)      PUT UCOM PRD TITLE INTO P                    
         B     CHKUC40D                                                         
*                                                                               
         DC    H'0'                INVALID INDICATOR IN BYTE                    
*                                                                               
CHKUC40D LA    RF,P                                                             
         BAS   RE,CHKUC60          NEED TO POINT TO END OF P                    
         CLI   BYTE,C'C'           CLIENT TITLE ONLY?                           
         BE    CHKUC40F                                                         
         MVI   0(RF),C':'          PUT COLON INTO P                             
         MVI   1(RF),C' '          PUT A SPACE NEXT TO COLON                    
         AHI   R0,2                TWO MORE CHARACTERS IN P                     
         LA    RF,2(RF)            POINT TO END OF P AGAIN                      
         L     R3,DOUBLE+4         GET ADDRESS OF DESIRED DATA FIELD            
         ZIC   RE,THREE+0          GET LENGTH OF FIELD DATA                     
         AR    R0,RE               TOTAL OF CHARACTERS IN P                     
         BCTR  RE,0                FOR "EX" INSTRUCTION                         
         EX    RE,MOVECK1                                                       
         B     *+10                                                             
MOVECK1  MVC   0(0,RF),0(R3)       GET DATA FIELD                               
*                                                                               
CHKUC40F LA    RE,132              LENGTH OF P                                  
         SR    RE,R0               NUMBER OF X'00' REMAINED IN P                
         LA    RF,P                POINT TO BEGINNING OF P                      
         AR    RF,R0               POINT TO END OF P                            
         CHI   RE,0                                                             
         BE    CHKUC40H            P IS FINE GO PRINT IT                        
         BCTR  RE,0                                                             
         EX    RE,MOVECK2                                                       
         B     *+10                                                             
MOVECK2  MVC   0(0,RF),SPACES      FILL X'00' WITH SPACES                       
*                                                                               
CHKUC40H BAS   R8,UCPRINT          PRINT UCOMM STUFFS                           
CHKUC40X BR    R2                                                               
*                                                                               
* SET UP PARAMETERS FOR UCOMM CALL                                              
*                                                                               
CHKUC50  XC    WKAREA,WKAREA                                                    
         LA    RF,WKAREA           BUILD UCOMM CALL PARAMETERS                  
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,VCOMFACS                                                 
         MVI   UCSYS,C'P'          SYSTEM=PRINT                                 
         MVC   UCAGY,PCLTREC+00    AGENCY                                       
         MVC   UCMED,PCLTREC+02    MEDIA                                        
         MVC   UCCLT,PCLTREC+04                                                 
         CLI   BYTE,C'C'                                                        
         BE    CHKUC50H                                                         
         MVC   UCPRD,PPRDREC+07                                                 
         CLI   BYTE,C'P'                                                        
         BE    CHKUC50K                                                         
         MVC   UCEST,PESTREC+10                                                 
         CLI   BYTE,C'E'                                                        
         BE    CHKUC50M                                                         
         DC    H'0'                INVALID INDICATOR                            
*                                                                               
CHKUC50H OI    UCOPT,UCOTTL        RETURN CLT TITLE ONLY                        
         BR    RE                                                               
CHKUC50K OI    UCOPT,UCOPRD        RETURN PRD LEVEL UCOMM                       
         BR    RE                                                               
CHKUC50M OI    UCOPT,UCOEST        RETURN EST LEVEL UCOMM                       
         BR    RE                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
* DETERMINES NUMBER OF SIGNIFICANT CHARS IN P AND POINT TO END OF IT            
*                                                                               
CHKUC60  DS    0H                  RF MUST BE POINTING TO P                     
         SR    R0,R0                                                            
CHKUC60G CLI   0(RF),0             AT END OF P?                                 
         BER   RE                                                               
         LA    RF,1(RF)                                                         
         AHI   R0,1                NUMBER OF NONE X'00' CHARS IN P              
         CHI   R0,132                                                           
         BNH   *+6                                                              
         DC    H'0'                LINE IS FULL, NOT POSSIBLE                   
         B     CHKUC60G                                                         
*                                                                               
* PRINT UCOMM AND RESTORE UCOMM BLOCK                                           
*                                                                               
CHKUC90  DS    0H                                                               
         ST    RE,FULL                                                          
         BAS   R2,CHKUC40          P IS PRINTED (UCOMM BLK IS CREAMED)          
         BAS   RE,CHKUC50          NEED TO RESTORE UCOMM BLOCK                  
         GOTO1 =V(DDUCOM),WKAREA                                                
         LA    R7,WKAREA           UCOMM BLOCK RESOTRED                         
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
WKAREA   DS    CL65                GENERAL WORKING STORAGE                      
*                                                                               
CHKUCX   XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
UCPRINT  DS    0H                  SAME AS PRINTIT, USED IN CHKUCOMM            
*                                                                               
         CLI   DOWNLOAD,C'Y'       IF DOWNLOADING, SKIP PRINTING                
         BNE   UCPRT03                                                          
         BRAS  RE,CLRPLNS          CLEAR PRINT LINES                            
         B     UCPRTX                                                           
*                                                                               
UCPRT03  CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST                     
         BNE   UCPRT05                                                          
         CLI   QCLIENT+1,C'*'      ALL OFFICES IN OFFICE ORDER?                 
         BE    UCPRT15                                                          
         MVC   HEAD2(11),=C'OFFICE LIST'                                        
         MVC   HEAD2+12(1),QCLIENT+1                                            
         B     UCPRT15                                                          
*                                                                               
UCPRT05  CLI   QCLIENT,C'&&'       SEE IF DOING GROUPS                          
         BNE   UCPRT10                                                          
         MVC   HEAD3(5),=C'GROUP'                                               
         MVC   HEAD3+6(1),QCLIENT+1                                             
         B     UCPRT20                                                          
*                                                                               
UCPRT10  CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   UCPRT20                                                          
         CLI   QCLIENT+1,C'-'      UNLESS EXCEPT ONE OFFICE                     
         BE    UCPRT20                                                          
*                                                                               
UCPRT15  MVC   HEAD3(6),=C'OFFICE'                                              
         OC    PCLTOFF,PCLTOFF                                                  
         BZ    UCPRT20                                                          
*                                                                               
*              PRINT OFFICE CODE                                                
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,(C'L',HEAD3+7),VOFFICER,QAGENCY,  X        
               VCOMFACS                                                         
*                                                                               
UCPRT20  GOTO1 REPORT                                                           
UCPRTX   BR    R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DOWNLOAD UCOMM DATA FOR PRODUCT                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DOWNUCOM NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,C'P'           PRODUCT ONLY                                 
         JNE   XIT                                                              
         L     R3,DOUBLE+4         R3-> UCOMM DATA                              
         ZIC   RE,THREE            LENGTH OF DATA                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLUCOM1(0),0(R3)                                                 
*                                                                               
         AHI   R3,32               BUMP TO UCOMM #2                             
         ZIC   RE,THREE            LENGTH OF DATA                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLUCOM2(0),0(R3)                                                 
*                                                                               
         AHI   R3,32               BUMP TO UCOMM #3                             
         ZIC   RE,THREE            LENGTH OF DATA                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLUCOM3(0),0(R3)                                                 
*                                                                               
         AHI   R3,32               BUMP TO UCOMM #4                             
         ZIC   RE,THREE            LENGTH OF DATA                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLUCOM4(0),0(R3)                                                 
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PRINT CLIENT TRAFFIC OFFICE CODE                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKCTRAO NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PCLTREC+33                                                    
         MVI   ELCODE2,X'50'       TRAFFICE OFFICE ELEM CODE                    
         BAS   RE,CTNXTEL                                                       
         BNE   CHKCTR50                                                         
*                                                                               
         USING PCLTTOEL,R2                                                      
         MVC   P,SPACES                                                         
         MVC   P+13(22),=C'TRAFFIC OFFICE CODE = '                              
*                                                                               
*              PRINT OFFICE CODE                                                
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTTOFC,P+35,VOFFICER,QAGENCY,VCOMFACS            
         MVC   DLTOFC,PCLTTOFC     TRAFFIC OFFICE CODE                          
*                                                                               
         BAS   R8,CTRGETP                                                       
         BAS   R8,CTRPRINT                                                      
         DROP  R2                                                               
*                                                                               
CHKCTR50 DS    0H                  FOR FUTURE USES                              
*                                                                               
CHKCTRAX XIT1                                                                   
*                                                                               
CTNXTEL  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    CTNXTEL2                                                         
         CLC   0(1,R2),ELCODE2                                                  
         BER   RE                                                               
         LTR   R0,R0                                                            
         BNZ   CTNXTEL                                                          
         DC    H'0'                BAD RECORD                                   
CTNXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
CTRGETP  CLI   ESPSW,X'FF'         SAME AS GETPLN, USED IN CHKCTRAO             
         BE    CTRGETPX                                                         
         CLI   0(R7),0             NO MORE LINES TO PRINT                       
         BE    CTRGETP1                                                         
         MVC   P+48(75),0(R7)                                                   
         LA    R7,75(R7)           NEXT LINE                                    
         B     CTRGETPX                                                         
*                                                                               
CTRGETP1 MVI   ESPSW,X'FF'         SET NO MORE LINES TO PRINT                   
CTRGETPX BR    R8                  RETURN                                       
*                                                                               
*                                                                               
*                                                                               
CTRPRINT DS    0H                  SAME AS PRINTIT, USED IN CHKCTRAO            
*                                                                               
         CLI   DOWNLOAD,C'Y'       IF DOWNLOADING, SKIP PRINTING                
         BNE   CTRPRT03                                                         
         BRAS  RE,CLRPLNS          CLEAR PRINT LINES                            
         B     CTRPRTX                                                          
*                                                                               
CTRPRT03 CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST                     
         BNE   CTRPRT05                                                         
         CLI   QCLIENT+1,C'*'      ALL OFFICES IN OFFICE ORDER?                 
         BE    CTRPRT15                                                         
         MVC   HEAD2(11),=C'OFFICE LIST'                                        
         MVC   HEAD2+12(1),QCLIENT+1                                            
         B     CTRPRT15                                                         
*                                                                               
CTRPRT05 CLI   QCLIENT,C'&&'       SEE IF DOING GROUPS                          
         BNE   CTRPRT10                                                         
         MVC   HEAD3(5),=C'GROUP'                                               
         MVC   HEAD3+6(1),QCLIENT+1                                             
         B     CTRPRT20                                                         
*                                                                               
CTRPRT10 CLI   QCLIENT,C'*'        SEE IF DOING AN OFFICE                       
         BNE   CTRPRT20                                                         
         CLI   QCLIENT+1,C'-'      UNLESS EXCEPT ONE CLIENT                     
         BE    CTRPRT20                                                         
*                                                                               
CTRPRT15 MVC   HEAD3(6),=C'OFFICE'                                              
         OC    PCLTOFF,PCLTOFF                                                  
         BZ    CTRPRT20                                                         
*                                                                               
*              PRINT OFFICE CODE                                                
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,(C'L',HEAD3+7),VOFFICER,QAGENCY,  X        
               VCOMFACS                                                         
*                                                                               
CTRPRT20 GOTO1 REPORT                                                           
CTRPRTX  BR    R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
***********************************************************************         
                                                                                
INITDOWN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DOWNLOAD,C'N'                                                    
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         L     RF,MCVREMOT         GET REMOTED ADDRESS                          
         USING REMOTED,RF                                                       
         TM    REMOTTYP,X'18'      TEST OUTPUT TYPE = DOWN OR SQL               
         JZ    XIT                                                              
         MVI   DOWNLOAD,C'Y'                                                    
         DROP  R9,RE,RF                                                         
*                                                                               
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
*                                                                               
         MVC   DUB(8),=CL8'T00A'   LOAD EDITOR                                  
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DLCBAED,4(R1)       DLFLD REQUIRES A(EDITOR)                     
*                                                                               
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,0              PREVENT PAGE BREAK                           
         CLI   MODE,REQFRST                                                     
         JE    PRTDWN10                                                         
         MVI   FORCEHED,C'N'       FORCE NO PAGE BREAK                          
         MVI   FORCEMID,C'N'                                                    
PRTDWN10 GOTO1 REPORT                                                           
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
         DROP R2                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DOWNLOAD HEADERS FOR SPREADSHEET                             *         
***********************************************************************         
                                                                                
DOWNHDRS NTR1  BASE=*,LABEL=*                                                   
         GOTOR OUTPDOWN,DMCB,(C'T',HDMED),L'HDMED                               
         GOTOR OUTPDOWN,DMCB,(C'T',HDCLT),L'HDCLT                               
         GOTOR OUTPDOWN,DMCB,(C'T',HDCLTN),L'HDCLTN                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD1C),L'HDADD1C                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD2C),L'HDADD2C                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD3C),L'HDADD3C                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD4C),L'HDADD4C                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDOFFC),L'HDOFFC                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDAOFF),L'HDAOFF                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDFRZ),L'HDFRZ                               
         GOTOR OUTPDOWN,DMCB,(C'T',HDCOS2),L'HDCOS2                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDINTF),L'HDINTF                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDTOFC),L'HDTOFC                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDCCOMM),L'HDCCOMM                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDICOM1),L'HDICOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDICOM2),L'HDICOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDBGRP),L'HDBGRP                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDCDIV),L'HDCDIV                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDPRD),L'HDPRD                               
         GOTOR OUTPDOWN,DMCB,(C'T',HDPRDNM),L'HDPRDNM                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD1P),L'HDADD1P                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD2P),L'HDADD2P                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD3P),L'HDADD3P                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD4P),L'HDADD4P                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDADD5P),L'HDADD5P                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDACCNO),L'HDACCNO                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDPDIV),L'HDPDIV                             
         GOTOR OUTPDOWN,DMCB,(C'T',HDBILLFP),L'HDBILLFP                         
         GOTOR OUTPDOWN,DMCB,(C'T',HDEFFDT),L'HDEFFDT                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDBCOM1),L'HDBCOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDBCOM2),L'HDBCOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDBCOM3),L'HDBCOM3                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDUSRT1P),L'HDUSRT1P                         
         GOTOR OUTPDOWN,DMCB,(C'T',HDUSRD1P),L'HDUSRD1P                         
         GOTOR OUTPDOWN,DMCB,(C'T',HDUSRT2P),L'HDUSRT2P                         
         GOTOR OUTPDOWN,DMCB,(C'T',HDUSRD2P),L'HDUSRD2P                         
         GOTOR OUTPDOWN,DMCB,(C'T',HDUCOM1),L'HDUCOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDUCOM2),L'HDUCOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDUCOM3),L'HDUCOM3                           
         GOTOR OUTPDOWN,DMCB,(C'T',HDUCOM4),L'HDUCOM4                           
         BRAS  RE,EOLDOWN                                                       
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
HDMED    DC    C'MED'                                                           
HDCLT    DC    C'CLT'                                                           
HDCLTN   DC    C'CLT NAME'                                                      
HDADD1C  DC    C'ADDR LINE 1'                                                   
HDADD2C  DC    C'ADDR LINE 2'                                                   
HDADD3C  DC    C'ADDR LINE 3'                                                   
HDADD4C  DC    C'ADDR LINE 4'                                                   
HDOFFC   DC    C'OFFICE'                                                        
HDAOFF   DC    C'ACC OFFICE'                                                    
HDFRZ    DC    C'FRZ FLAG'                                                      
HDCOS2   DC    C'COS2'                                                          
HDINTF   DC    C'INTERFACE#'                                                    
HDTOFC   DC    C'TRAFF OFF CODE'                                                
HDCCOMM  DC    C'CONTRACT STANDARD COMM'                                        
HDICOM1  DC    C'I/O STANDARD COMM 1'                                           
HDICOM2  DC    C'I/O STANDARD COMM 2'                                           
HDBGRP   DC    C'BILLING GRP'                                                   
HDCDIV   DC    C'CLT DIVISIONS'                                                 
HDPRD    DC    C'PRD'                                                           
HDPRDNM  DC    C'PRD NAME'                                                      
HDADD1P  DC    C'ADDR LINE 1'                                                   
HDADD2P  DC    C'ADDR LINE 2'                                                   
HDADD3P  DC    C'ADDR LINE 3'                                                   
HDADD4P  DC    C'ADDR LINE 4'                                                   
HDADD5P  DC    C'ADDR LINE 5'                                                   
HDACCNO  DC    C'ACCT NO'                                                       
HDPDIV   DC    C'DIVISION'                                                      
HDBILLFP DC    C'BILL FORM-PRD'                                                 
HDEFFDT  DC    C'B.F. EFF DATE'                                                 
HDBCOM1  DC    C'STANDARD COMM ON BILL 1'                                       
HDBCOM2  DC    C'STANDARD COMM ON BILL 2'                                       
HDBCOM3  DC    C'STANDARD COMM ON BILL 3'                                       
HDUSRT1P DC    C'USER TITLE 1'                                                  
HDUSRD1P DC    C'USER DATA 1'                                                   
HDUSRT2P DC    C'USER TITLE 2'                                                  
HDUSRD2P DC    C'USER DATA 2'                                                   
HDUCOM1  DC    C'UCOMM DATA 1'                                                  
HDUCOM2  DC    C'UCOMM DATA 2'                                                  
HDUCOM3  DC    C'UCOMM DATA 3'                                                  
HDUCOM4  DC    C'UCOMM DATA 4'                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DOWNLOAD ROW FOR SPREADSHEET                                 *         
***********************************************************************         
DOWNROW  NTR1  BASE=*,LABEL=*                                                   
         GOTOR OUTPDOWN,DMCB,(C'T',DLMED),L'DLMED                               
         GOTOR OUTPDOWN,DMCB,(C'T',DLCLT),L'DLCLT                               
         GOTOR OUTPDOWN,DMCB,(C'T',DLCLTN),L'DLCLTN                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD1),L'DLADD1                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD2),L'DLADD2                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD3),L'DLADD3                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD4),L'DLADD4                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLOFF),L'DLOFF                               
         GOTOR OUTPDOWN,DMCB,(C'T',DLAOFF),L'DLAOFF                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLFRZ),L'DLFRZ                               
         GOTOR OUTPDOWN,DMCB,(C'T',DLCOS2),L'DLCOS2                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLINTF),L'DLINTF                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLTOFC),L'DLTOFC                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLCCOMM),L'DLCCOMM                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLICOM1),L'DLICOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLICOM2),L'DLICOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLBGRP),L'DLBGRP                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLCDIV),L'DLCDIV                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLPRD),L'DLPRD                               
         GOTOR OUTPDOWN,DMCB,(C'T',DLPRDNM),L'DLPRDNM                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD1P),L'DLADD1P                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD2P),L'DLADD2P                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD3P),L'DLADD3P                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD4P),L'DLADD4P                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLADD5P),L'DLADD5P                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLACCNO),L'DLACCNO                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLPDIV),L'DLPDIV                             
         GOTOR OUTPDOWN,DMCB,(C'T',DLBILLFP),L'DLBILLFP                         
         GOTOR OUTPDOWN,DMCB,(C'T',DLEFFDT),L'DLEFFDT                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLBCOM1),L'DLBCOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLBCOM2),L'DLBCOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLBCOM3),L'DLBCOM3                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLUSRT1P),L'DLUSRT1P                         
         GOTOR OUTPDOWN,DMCB,(C'T',DLUSRD1P),L'DLUSRD1P                         
         GOTOR OUTPDOWN,DMCB,(C'T',DLUSRT2P),L'DLUSRT2P                         
         GOTOR OUTPDOWN,DMCB,(C'T',DLUSRD2P),L'DLUSRD2P                         
         GOTOR OUTPDOWN,DMCB,(C'T',DLUCOM1),L'DLUCOM1                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLUCOM2),L'DLUCOM2                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLUCOM3),L'DLUCOM3                           
         GOTOR OUTPDOWN,DMCB,(C'T',DLUCOM4),L'DLUCOM4                           
         BRAS  RE,EOLDOWN                                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE DDUCOMD           DSECT FOR DDUCOMM CALLS                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BILFORM  CSECT                                                                  
         NMOD1 0,BILFOR,RR=R9                                                   
*                                                                               
         ST    R9,BRELO                                                         
         B     *+8                                                              
BRELO    DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         USING BILPROFD,R7         R7 POINTS TO BILLING PROFILE                 
         LA    R1,6                                                             
         L     R2,=A(BILLINES)                                                  
BILFM0   XC    0(75,R2),0(R2)                                                   
         LA    R2,75(R2)                                                        
         BCT   R1,BILFM0                                                        
         MVI   BILSW,0                                                          
         OC    BILPROF,BILPROF                                                  
         BZ    BILFX               NO PROFLILE                                  
         CLI   QOPT2,C'Y'                                                       
         BE    BILFX                                                            
         L     R4,=A(BILLINES)                                                  
         OC    BILBASA(7),BILBASA  SEE IF USING FORMULA                         
         BZ    BILF1                                                            
         LR    R9,R4               SET R9 TO PRINT LINE                         
         MVC   0(16,R9),=C'BILLING FORMULA='                                    
         LA    R9,17(R9)                                                        
         OC    BILBASA(5),BILBASA  SEE IF FORMULA PRESENT                       
         BNZ   BILFM2                                                           
         MVC   0(4,R9),=C'NONE'                                                 
         LA    R9,6(R9)                                                         
         B     BILFM5              IF NOT STILL SHOW EFF.DATE                   
*                                                                               
BILFM2   LA    R2,BILBASA                                                       
         CLI   BILCMSW,C'C'        SEE IF COMMISSION ONLY                       
         BNE   *+8                                                              
         OI    0(R2),X'10'                                                      
         CLI   BILNBSW,C'Y'        SEE IF NOT USED BY BILLING                   
         BNE   *+8                                                              
         OI    0(R2),X'20'                                                      
         BAS   R8,FMTBAS           TRANSLATE BASE CODE                          
         AR    R9,R3               R3 HAS LENGHT OF BASE                        
         LA    R9,1(R9)                                                         
*                                                                               
         LR    R0,R9                                                            
         SR    R0,R4                                                            
         ST    R0,FULL             SAVE WHERE TO SHOW "SHOW AS" DATA            
*                                                                               
         EDIT  (B3,BILADJ),(8,1(R9)),4,ALIGN=LEFT                               
         OI    7(R9),C'0'                                                       
         OI    8(R9),C'0'                                                       
         LA    R1,8(R9)                                                         
         CLI   0(R1),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(R1),C' '                                                       
         BCT   R1,*-16                                                          
         MVI   0(R1),C' '                                                       
         MVI   0(R9),C'+'                                                       
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   0(R9),C'-'                                                       
         LR    R9,R1               SET R9 TO END OF ADJ                         
         LA    R9,2(R9)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         AHI   R9,-1                                                            
*                                                                               
         MVC   0(6,R9),=C'PCT OF'                                               
         LA    R9,7(R9)                                                         
         LA    R2,BILBASB                                                       
         BAS   R8,FMTBAS                                                        
         AR    R9,R3              ADD LENGHT OF BASA B                          
         LA    R9,2(R9)                                                         
BILFM5   MVC   DLBILLFP,17(R4)                                                  
         OC    BILADAT,BILADAT                                                  
         BZ    BILF0A                                                           
         MVC   0(14,R9),=C'EFFECTIVE DATE'                                      
         LA    R9,15(R9)                                                        
         GOTO1 DATCON,DMCB,(3,BILADAT),(9,0(R9))                                
         MVC   DLEFFDT,0(R9)                                                    
BILF0A   LA    R4,75(R4)           BUMP R4 TO NEXT LINE                         
         LR    R9,R4                                                            
         OC    BILPADJ,BILPADJ     CHECK FOR "SHOW AS" ADJ                      
         BZ    BILF1                                                            
         MVI   0(R9),C' '          SO THIS LINE WILL PRINT                      
         A     R9,FULL                                                          
         AHI   R9,-14                                                           
         MVC   0(13,R9),=C'DISPLAY AC AS'                                       
         AHI   R9,14                                                            
*                                                                               
         EDIT  (B3,BILPADJ),(8,1(R9)),4,ALIGN=LEFT                              
         OI    7(R9),C'0'                                                       
         OI    8(R9),C'0'                                                       
         LA    R1,8(R9)                                                         
         CLI   0(R1),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(R1),C' '                                                       
         BCT   R1,*-16                                                          
         MVI   0(R1),C' '                                                       
         MVI   0(R9),C'+'                                                       
         TM    BILPADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R9),C'-'                                                       
         LR    R9,R1               SET R9 TO END OF ADJ                         
         LA    R9,2(R9)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         AHI   R9,-1                                                            
         MVC   0(6,R9),=C'PCT OF'                                               
         LA    R9,7(R9)                                                         
         LA    R2,BILPBASB                                                      
         BAS   R8,FMTBAS                                                        
         LA    R4,75(R4)                                                        
*                                                                               
BILF1    LR    R9,R4                                                            
         CLI   BILDETS,0                                                        
         BE    BILF2                                                            
         CLI   BILDETS,C'4'        DEFAULT                                      
         BE    BILF2                                                            
         MVC   0(26,R9),=C'COLUMNS TO PRINT ON BILLS-'                          
         LA    R9,27(R9)                                                        
         LA    R1,COLTAB                                                        
         SR    R3,R3                                                            
BILF1A   CLC   BILDETS,1(R1)                                                    
         BNE   NXTCOL                                                           
         IC    R3,0(R1)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         EX    R3,MOVCOL                                                        
         B     BILF1X                                                           
*                                                                               
MOVCOL   MVC   0(0,R9),2(R1)       EXECUTED                                     
*                                                                               
NXTCOL   SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   BILF1A                                                           
         MVC   0(12,R9),=C'UNKNOWN CODE'                                        
BILF1X   LA    R4,75(R4)                                                        
         B     BILF2                                                            
*                                                                               
BILF2    LR    R9,R4                                                            
*                                                                               
         B     BILF3                                                            
**       CLI   BILCDSW,C'S'                                                     
**       BE    BILF2B                                                           
**       CLI   BILRDSW,C'S'                                                     
**       BE    BILF2B                                                           
**       OC    BILOAF,BILOAF         OTHER AGY FEE                              
**       BZ    BILF3                                                            
**                                                                              
**LF2B   CLI   BILCDSW,C'S'                                                     
**       BNE   BILF2C                                                           
**       MVC   0(20,R9),=C'SEPERATE CD INVOICE,'                                
**       LA    R9,21(R9)                                                        
**                                                                              
**LF2C   CLI   BILRDSW,C'S'                                                     
**       BNE   BILF2D                                                           
**       MVC   0(29,R9),=C'SEPERATE INVOICE FOR REG/DST,'                       
**       LA    R9,30(R9)                                                        
**                                                                              
**LF2D   OC    BILOAF,BILOAF                                                    
**       BZ    BILF2X                                                           
**       MVC   0(14,R9),=C'OTHER AGY FEE='                                      
**       LA    R9,14(R9)                                                        
**       EDIT  (B3,BILOAF),(8,0(R9)),5,ALIGN=LEFT,FLOAT=-                       
**       OI    8(R9),C'0'                                                       
**       LA    R1,8(R9)                                                         
**       CLI   0(R1),C'0'                                                       
**       BH    *+20                                                             
**       BL    *+12                                                             
**       MVI   0(R1),C' '                                                       
**       BCT   R1,*-16                                                          
**       MVI   0(R1),C' '                                                       
**                                                                              
**       MVC   1(3,R1),=C'PCT'                                                  
**LF2X   BCTR  R9,0                                                             
**       BCTR  R9,0                                                             
**       CLI   0(R9),C','          BLANK LAST COMMA                             
**       BNE   *+8                                                              
**       MVI   0(R9),C' '                                                       
**       LA    R4,75(R4)           NEXT LINE                                    
**       B     BILF3                                                            
*                                                                               
BILF3    LA    R2,3                FOR  BCT                                     
         LA    R6,BILCMNTS         COMMENTS                                     
         BAS   RE,DWNCMNTS         DOWNLOAD COMMENTS                            
                                                                                
BILF3A   OC    0(7,R6),0(R6)                                                    
         BNZ   BILF3B                                                           
*                                                                               
NXTCMNT  LA    R6,7(R6)            NEXT COMMENT                                 
         BCT   R2,BILF3A                                                        
         B     BILFX                                                            
*                                                                               
BILF3B   LR    R9,R4                                                            
         MVC   0(11,R9),=C'COMMENT NO.'                                         
         MVC   12(6,R9),1(R6)      MOVE COMMENT NO                              
         MVC   19(15,R9),=C'APPLIES TO REG,'                                    
         LA    R1,30(R9)                                                        
         TM    0(R6),X'80'                                                      
         BZ    BILF3C                                                           
         LA    R1,34(R9)                                                        
BILF3C   MVC   0(3,R1),=C'CD,'                                                  
         TM    0(R6),X'40'                                                      
         BZ    BILF3D                                                           
         LA    R1,3(R1)                                                         
*                                                                               
BILF3D   MVC   0(3,R1),=C'ADJ'                                                  
         TM    0(R6),X'20'                                                      
         BO    BILF3E                                                           
         BCTR  R1,0                                                             
         MVI   0(R1),C' '          BLANK LAST ,                                 
         MVC   1(5,R1),=C'BILLS'                                                
         B     BILF3Z                                                           
*                                                                               
BILF3E   MVC   4(5,R1),=C'BILLS'                                                
         MVI   3(R1),C' '          BLANK COMMA THAT MAY BE HERE                 
         B     BILF3Z                                                           
*                                                                               
BILF3Z   LA    R4,75(R4)           NEXT LINE                                    
         B     NXTCMNT                                                          
*                                                                               
FMTBAS   LA    R1,BASTAB                                                        
         SR    R3,R3                                                            
FMTB1    CLC   1(1,R1),0(R2)       R2 POINTS TO CODE                            
         BNE   NXTBAS                                                           
         IC    R3,0(R1)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         EX    R3,MOVBAS                                                        
         LA    R3,1(R3)            RESET R3 TO LENGHT OF BASE                   
         BR    R8                  RETURN                                       
*                                                                               
MOVBAS   MVC   0(0,R9),2(R1)       EXECUTED                                     
*                                                                               
NXTBAS   SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF TABLE                                 
         BNE   FMTB1                                                            
         DC    H'0'                INVALID BASE IN BILLING FORMULA              
*                                                                               
BASTAB   DS    0C                  LENGHT OF ENTRY,CODE,PRINT CHARS             
         DC    X'03',X'01',C'G'                                                 
         DC    X'03',X'02',C'N'                                                 
         DC    X'06',X'05',C'G-CD'                                              
         DC    X'06',X'06',C'N-CD'                                              
         DC    X'04',X'08',C'AC'                                                
         DC    X'04',X'11',C'CG'                                                
         DC    X'04',X'12',C'CN'                                                
         DC    X'07',X'15',C'CG-CD'                                             
         DC    X'07',X'16',C'CN-CD'                                             
         DC    X'05',X'18',C'CAC'                                               
         DC    X'04',X'21',C'EG'      THESE WITH E ARE NOT FOR BILLING          
         DC    X'04',X'22',C'EN'      - ESTIMATES ONLY                          
         DC    X'07',X'25',C'EG-CD'                                             
         DC    X'07',X'26',C'EN-CD'                                             
         DC    X'05',X'28',C'EAC'                                               
         DC    X'00'               END OF TABLE                                 
*                                                                               
COLTAB   DS    0C                  LENGHT OF ENTRY,CODE,DISPLAY CHARS           
         DC    AL1(05),C'1',C'G/N'                                              
         DC    AL1(05),C'2',C'N/G'                                              
         DC    AL1(05),C'3',C'G,N'                                              
         DC    AL1(13),C'5',C'N,CD,N-CD/G'                                      
         DC    AL1(08),C'6',C'G,N,CD'                                           
         DC    AL1(12),C'7',C'G,N,G-CD/N'                                       
         DC    AL1(12),C'8',C'G,N,N-CD/G'                                       
         DC    AL1(13),C'9',C'G,CD,N-CD/N'                                      
         DC    AL1(15),C'A',C'G,G-CD,N-CD/N'                                    
         DC    AL1(13),C'B',C'G,CD,G-CD/G'                                      
         DC    AL1(13),C'C',C'N,CD,N-CD/N'                                      
         DC    AL1(08),C'D',C'G,CD,N'                                           
         DC    AL1(08),C'E',C'N,CD,G'                                           
         DC    X'0000'                                                          
*                                                                               
BILFX    XMOD1 1                                                                
*                                                                               
*                                                                               
***********************************************************************         
*        DOWNLOAD BILL COMMENTS                                                 
*        ON ENTRY R6 -> BILCMNTS                                                
***********************************************************************         
*                                                                               
DWNCMNTS NTR1                                                                   
         LHI   R2,3                COUNTER                                      
         LA    R3,DLBCOM1                                                       
*                                                                               
DWNCM10  LR    R1,R3                                                            
         MVC   0(6,R3),1(R6)       COMMENT                                      
         AHI   R1,7                                                             
         TM    0(R6),X'80'         REGULAR?                                     
         BZ    DWNCM20                                                          
         MVI   0(R1),C'R'                                                       
         AHI   R1,1                                                             
DWNCM20  TM    0(R6),X'40'         CASH DISCOUNT?                               
         BZ    DWNCM30                                                          
         MVI   0(R1),C'C'                                                       
         AHI   R1,1                                                             
DWNCM30  TM    0(R6),X'20'         ADJUSTMENT?                                  
         BZ    DWNCM40                                                          
         MVI   0(R1),C'A'                                                       
DWNCM40  LA    R3,L'DLBCOM1(R3)    NEXT COMMENT                                 
         LA    R6,L'BILCMNTS(R6)                                                
         BCT   R2,DWNCM10                                                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BILLINES CSECT                                                                  
         DS    6CL75                                                            
         DC    X'00'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROLINES CSECT                                                                  
         DS    32CL75                                                           
         DC    X'00'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PROFTAB  CSECT                                                                  
*                                                                               
*        PROFILE POSITION,LENGHT OF ENTRY,LENGHT OF FIELD,X'01'=CLT             
*        X'02'=EST,FILTER CHAR,INCREMENT TO NEXT ENTRY IF FILTER NOT            
*        PASSED                                                                 
*                                                                               
CP1      DC    AL1(1),AL2(CP1X-*+2),AL1(1),X'01',X'00',AL2(0)                   
         DC    C'1',AL1(L'CP1A+2)                                               
CP1A     DC    C'DIVISIONS REQUIRED'                                            
         DC    C'2',AL1(L'CP1B+2)                                               
CP1B     DC    C'PUB REGIONS/DISTRICTS REQUIRED'                                
CP1X     DC    X'00'                                                            
         SPACE 2                                                                
CP2      DC    AL1(2),AL2(CP2X-*+2),AL1(1),X'03',C'1',AL2(CP6-CP2)              
         DC    C'1',AL1(L'CP2A+2)                                               
CP2A     DC    C'BILLING MONTH OVERRIDE'                                        
CP2X     DC    X'00'                                                            
         SPACE 2                                                                
CP3      DC    AL1(3),AL2(CP3X-*+2),AL1(1),X'03',X'00',AL2(0)                   
         DC    C'0',AL1(L'CP3A+2)                                               
CP3A     DC    C'BILLING MTH CALCULATED RELATIVE TO INSERTION MTH'              
         DC    C'1',AL1(L'CP3B+2)                                               
CP3B     DC    C'BILLING MTH CALCULATED RELATIVE TO PAYABLE MTH'                
         DC    C'2',AL1(L'CP3C+2)                                               
CP3C     DC    C'BILLING MTH CALCULATED RELATIVE TO ON-SALE MTH'                
         DC    C'3',AL1(L'CP3D+2)                                               
CP3D     DC    C'BILLING MTH CALCULATED RELATIVE TO CLOSING DATE'               
CP3X     DC    X'00'                                                            
         SPACE 2                                                                
CP4      DC    AL1(4),AL2(CP4X-*+2),AL1(1),X'03',X'00',AL2(0)                   
         DC    C'0',AL1(L'CP4A+2)                                               
CP4A     DC    C'BILLING MTH IS 0 MTHS PRIOR TO BASE MTH'                       
         DC    C'1',AL1(L'CP4B+2)                                               
CP4B     DC    C'BILLING MTH IS 1 MTH PRIOR TO BASE MTH'                        
         DC    C'2',AL1(L'CP4C+2)                                               
CP4C     DC    C'BILLING MTH IS 2 MTHS PRIOR TO BASE MTH'                       
         DC    C'3',AL1(L'CP4D+2)                                               
CP4D     DC    C'BILLING MTH IS 3 MTHS PRIOR TO BASE MTH'                       
         DC    C'A',AL1(L'CP4E+2)                                               
CP4E     DC    C'BILLING MTH IS 1 MTH AFTER BASE MTH'                           
         DC    C'B',AL1(L'CP4F+2)                                               
CP4F     DC    C'BILLING MTH IS 2 MTHS AFTER BASE MTH'                          
         DC    C'C',AL1(L'CP4G+2)                                               
CP4G     DC    C'BILLING MTH IS 3 MTHS AFTER BASE MTH'                          
CP4X     DC    X'00'                                                            
         SPACE 2                                                                
CP5      DC    AL1(5),AL2(CP5X-*+2),AL1(1),X'03',X'00',AL2(0)                   
         DC    C'0',AL1(L'CP5A+2)                                               
CP5A     DC    C'ADJUST BILLING MTH FOR FREQ CODES W,BW,SW,SM'                  
         DC    C'1',AL1(L'CP5B+2)                                               
CP5B     DC    C'NO BILLING MTH ADJUSTMENT FOR FREQ CODES W,BW,SW,SM'           
CP5X     DC    X'00'                                                            
         SPACE 2                                                                
CP6      DC    AL1(6),AL2(CP6X-*+2),AL1(1),X'01',C'2',AL2(CP10-CP6)             
         DC    C'1',AL1(L'CP6A+2)                                               
CP6A     DC    C'MASTER CLIENT'                                                 
         DC    C'2',AL1(L'CP6B+2)                                               
CP6B     DC    C'SLAVE CLIENT'                                                  
CP6X     DC    X'00'                                                            
         SPACE 2                                                                
CP7      DC    AL1(7),AL2(CP7X-*+2),AL1(3),X'01',X'00',AL2(0)                   
         DC    X'FF',AL1(L'CP7A+2)                                              
CP7A     DC    C'MASTER'                                                        
CP7X     DC    X'00'                                                            
         SPACE 2                                                                
CP10     DC    AL1(10),AL2(CP10X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'0',AL1(L'CP10A+2)                                              
CP10A    DC    C'BILLING DONE BY ESTIMATE'                                      
         DC    C'1',AL1(L'CP10B+2)                                              
CP10B    DC    C'BILLING DONE BY PRODUCT'                                       
CP10X    DC    X'00'                                                            
         SPACE 2                                                                
CP11     DC    AL1(11),AL2(CP11X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP11A+2)                                              
CP11A    DC    C'BILLING FORMULAS-ADJUST ON SAME INVOICE'                       
         DC    C'2',AL1(L'CP11B+2)                                              
CP11B    DC    C'BILLING FORMULAS-ADJUST ON SEPERATE INVOICES'                  
CP11X    DC    X'00'                                                            
         SPACE 2                                                                
CP12     DC    AL1(12),AL2(CP12X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP12A+2)                                              
CP12A    DC    C'ON ORI - PRIOR MTHS ON SEPERATE INVOICES'                      
         DC    C'2',AL1(L'CP12B+2)                                              
CP12B    DC    C'ON ORI - PRIOR MTHS ON SEP. INVS. + PREV. INV. NOS.'           
CP12X    DC    X'00'                                                            
         SPACE 2                                                                
CP13     DC    AL1(13),AL2(CP13X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'N',AL1(L'CP13A+2)                                              
CP13A    DC    C'CONTRACTS NOT REQUIRED'                                        
CP13X    DC    X'00'                                                            
         SPACE 2                                                                
CP14     DC    AL1(14),AL2(CP14X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'0',AL1(L'CP14A+2)                                              
CP14A    DC    C'DETAIL BILLING ON PAID ITEMS ONLY'                             
         DC    C'1',AL1(L'CP14B+2)                                              
CP14B    DC    C'DETAIL BILLING ON ORDERED ITEMS'                               
CP14X    DC    X'00'                                                            
         SPACE 2                                                                
CP15     DC    AL1(15),AL2(CP15X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'G',AL1(L'CP15A+2)                                              
CP15A    DC    C'GROSS DOLLARS ENTERED THRU AUTHORIZATION PROGRAM'              
         DC    C'N',AL1(L'CP15B+2)                                              
CP15B    DC    C'NET DOLLARS ENTERED THRU AUTHORIZATION PROGRAM'                
         DC    C'C',AL1(L'CP15C+2)                                              
CP15C    DC    C'''COST'' DOLLARS ENTERED THRU AUTHORIZATION PROGRAM'           
CP15X    DC    X'00'                                                            
         SPACE 2                                                                
CP16     DC    AL1(16),AL2(CP16X-*+2),AL1(1),X'01',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP16A+2)                                              
CP16A    DC    C'COPY NUMBER ON DETAIL BILLS'                                   
         DC    C'2',AL1(L'CP16B+2)                                              
CP16B    DC    C'COPY NUMBER AND CAPTION ON DETAIL BILLS'                       
         DC    C'A',AL1(L'CP16C+2)                                              
CP16C    DC    C'AD NUMBER ON DETAIL BILLS'                                     
         DC    C'B',AL1(L'CP16D+2)                                              
CP16D    DC    C'AD NUMBER AND CAPTION ON DETAIL BILLS'                         
CP16X    DC    X'00'                                                            
         SPACE 2                                                                
CP17     DC    AL1(17),AL2(CP17X-*+2),AL1(1),X'03',X'00',AL2(0)                 
CP17X    DC    X'00'                                                            
         SPACE 2                                                                
CP18     DC    AL1(18),AL2(CP18X-*+2),AL1(1),X'03',X'00',AL2(0)                 
CP18X    DC    X'00'                                                            
         SPACE 2                                                                
CP19     DC    AL1(19),AL2(CP19X-*+2),AL1(1),X'02',C'1',AL2(CPXXX-CP19)         
         DC    C'1',AL1(L'CP19A+2)                                              
CP19A    DC    C'OVERRIDE CLIENT PROFILE FOR POSITIONS 20-32'                   
CP19X    DC    X'00'                                                            
         SPACE 2                                                                
CP20     DC    AL1(20),AL2(CP20X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP20A+2)                                              
CP20A    DC    C'AT PUB LEVEL ON EST PRINT GROSS ONLY'                          
         DC    C'2',AL1(L'CP20B+2)                                              
CP20B    DC    C'AT PUB LEVEL ON EST PRINT GROSS + CD'                          
         DC    C'3',AL1(L'CP20C+2)                                              
CP20C    DC    C'AT PUB LEVEL ON EST PRINT NET ONLY'                            
         DC    C'4',AL1(L'CP20D+2)                                              
CP20D    DC    C'AT PUB LEVEL ON EST PRINT NET + CD'                            
         DC    C'5',AL1(L'CP20E+2)                                              
CP20E    DC    C'AT PUB LEVEL ON EST PRINT GROSS + NET'                         
         DC    C'6',AL1(L'CP20F+2)                                              
CP20F    DC    C'AT PUB LEVEL ON EST PRINT GROSS + NET + CD'                    
         DC    C'7',AL1(L'CP20G+2)                                              
CP20G    DC    C'AT PUB LEVEL ON EST PRINT GROSS LESS CD'                       
         DC    C'8',AL1(L'CP20H+2)                                              
CP20H    DC    C'AT PUB LEVEL ON EST PRINT NET LESS CD'                         
         DC    C'9',AL1(L'CP20I+2)                                              
CP20I    DC    C'AT PUB LEVEL ON EST PRINT G,N,N-CD'                            
         DC    C'B',AL1(L'CP20J+2)                                              
CP20J    DC    C'AT PUB LEVEL ON EST PRINT G,CD,N-CD'                           
         DC    C'C',AL1(L'CP20K+2)                                              
CP20K    DC    C'AT PUB LEVEL ON EST PRINT ''COST'''                            
         DC    C'D',AL1(L'CP20L+2)                                              
CP20L    DC    C'AT PUB LEVEL ON EST PRINT G,''COST'''                          
         DC    C'E',AL1(L'CP20M+2)                                              
CP20M    DC    C'AT PUB LEVEL ON EST PRINT G,CD,''COST'''                       
         DC    C'F',AL1(L'CP20N+2)                                              
CP20N    DC    C'AT PUB LEVEL ON EST PRINT N,''COST'''                          
         DC    C'G',AL1(L'CP20O+2)                                              
CP20O    DC    C'AT PUB LEVEL ON EST PRINT N,CD,''COST'''                       
CP20X    DC    X'00'                                                            
         SPACE 2                                                                
CP21     DC    AL1(21),AL2(CP21X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP21A+2)                                              
CP21A    DC    C'DATES TO PRINT ON ESTS - PAYABLE + CLOSING'                    
         DC    C'2',AL1(L'CP21B+2)                                              
CP21B    DC    C'DATES TO PRINT ON ESTS - CLOSING + ON-SALE'                    
         DC    C'3',AL1(L'CP21C+2)                                              
CP21C    DC    C'DATES TO PRINT ON ESTS - ON-SALE + PAYABLE'                    
         DC    C'4',AL1(L'CP21D+2)                                              
CP21D    DC    C'DATES TO PRINT ON ESTS - PAYABLE'                              
         DC    C'5',AL1(L'CP21E+2)                                              
CP21E    DC    C'DATES TO PRINT ON ESTS - CLOSING'                              
         DC    C'6',AL1(L'CP21F+2)                                              
CP21F    DC    C'DATES TO PRINT ON ESTS - ON-SALE'                              
         DC    C'7',AL1(L'CP21G+2)                                              
CP21G    DC    C'DATES TO PRINT ON ESTS - NONE'                                 
CP21X    DC    X'00'                                                            
         SPACE 2                                                                
CP22     DC    AL1(22),AL2(CP22X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'0',AL1(L'CP22A+2)                                              
CP22A    DC    C'NO PUB ADDRESS TO PRINT OUT ON ESTS'                           
         DC    C'1',AL1(L'CP22B+2)                                              
CP22B    DC    C'PRINT PUB ADDRESS BELOW NAME ON ESTS'                          
CP22X    DC    X'00'                                                            
         SPACE 2                                                                
CP23     DC    AL1(23),AL2(CP23X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'N',AL1(L'CP23A+2)                                              
CP23A    DC    C'SUPPRESS PRINTING OF CONTRACT INFO ON ESTS'                    
         DC    C'S',AL1(L'CP23B+2)                                              
CP23B    DC    C'SUPPRESS CONTRACT RATES + DESCRIPTION ON ESTS'                 
         DC    C'0',AL1(L'CP23C+2)                                              
CP23C    DC    C'PRINT CONTRACT INFO ON ESTS'                                   
CP23X    DC    X'00'                                                            
         SPACE 2                                                                
CP24     DC    AL1(24),AL2(CP24X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP24A+2)                                              
CP24A    DC    C'AT SUMMARY LEVEL ON ESTS PRINT GROSS'                          
         DC    C'2',AL1(L'CP24B+2)                                              
CP24B    DC    C'AT SUMMARY LEVEL ON ESTS PRINT GROSS+CD+GROSS LESS CD'         
         DC    C'3',AL1(L'CP24C+2)                                              
CP24C    DC    C'AT SUMMARY LEVEL ON ESTS PRINT NET'                            
         DC    C'4',AL1(L'CP24D+2)                                              
CP24D    DC    C'AT SUMMARY LEVEL ON ESTS PRINT NET+CD+NET LESS CD'             
         DC    C'5',AL1(L'CP24E+2)                                              
CP24E    DC    C'AT SUMMARY LEVEL ON ESTS PRINT GROSS+NET'                      
         DC    C'6',AL1(L'CP24F+2)                                              
CP24F    DC    C'AT SUMMARY LEVEL ON ESTS PRINT G+N+CD + G-CD + N-CD'           
         DC    C'7',AL1(L'CP24G+2)                                              
CP24G    DC    C'AT SUMMARY LEVEL ON ESTS PRINT G,N,CD,G-CD'                    
         DC    C'C',AL1(L'CP24H+2)                                              
CP24H    DC    C'AT SUMMARY LEVEL ON ESTS PRINT ''COST'''                       
         DC    C'D',AL1(L'CP24I+2)                                              
CP24I    DC    C'AT SUMMARY LEVEL ON ESTS PRINT G,''COST'''                     
         DC    C'E',AL1(L'CP24J+2)                                              
CP24J    DC    C'AT SUMMARY LEVEL ON ESTS PRINT G,CD,''COST'''                  
         DC    C'F',AL1(L'CP24K+2)                                              
CP24K    DC    C'AT SUMMARY LEVEL ON ESTS PRINT N,''COST'''                     
         DC    C'G',AL1(L'CP24L+2)                                              
CP24L    DC    C'AT SUMMARY LEVEL ON ESTS PRINT N,CD,''COST'''                  
CP24X    DC    X'00'                                                            
         SPACE 2                                                                
CP25     DC    AL1(25),AL2(CP25X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP25A+2)                                              
CP25A    DC    C'LNS OR INSRT TOTALS ON ESTS-PUB LEVEL+RECAP'                   
         DC    C'2',AL1(L'CP25B+2)                                              
CP25B    DC    C'LNS OR INSRT TOTALS ON ESTS-PUB LEVEL+RECAP+SUMMARY'           
         DC    C'A',AL1(L'CP25C+2)                                              
CP25C    DC    C'LNS OR INSRT TOTALS ON ESTS-PUB INCH TOTALS'                   
         DC    C'B',AL1(L'CP25D+2)                                              
CP25D    DC    C'LNS OR INSRT TOTALS ON ESTS-SUMMARY INCH TOTALS'               
         DC    C'0',AL1(L'CP25E+2)                                              
CP25E    DC    C'NO LINE OR INSERTION TOTALS ON ESTS'                           
CP25X    DC    X'00'                                                            
         SPACE 2                                                                
CP26     DC    AL1(26),AL2(CP26X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'0',AL1(L'CP26A+2)                                              
CP26A    DC    C'ON ESTS PRINT INSERTION AND BILL MONTH SUMMARIES'              
         DC    C'1',AL1(L'CP26B+2)                                              
CP26B    DC    C'ON ESTS PRINT ON-SALE AND BILL MONTH SUMMARIES'                
CP26X    DC    X'00'                                                            
         SPACE 2                                                                
CP27     DC    AL1(27),AL2(CP27X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'Q',AL1(L'CP27A+2)                                              
CP27A    DC    C'PRINT QUARTERLY TOTALS ON EST SUMMARIES'                       
         DC    C'1',AL1(L'CP27B+2)                                              
CP27B    DC    C'START QUARTERLY TOTALS ON EST SUMMARIES WITH JAN'              
         DC    C'2',AL1(L'CP27C+2)                                              
CP27C    DC    C'START QUARTERLY TOTALS ON EST SUMMARIES WITH FEB'              
         DC    C'3',AL1(L'CP27D+2)                                              
CP27D    DC    C'START QUARTERLY TOTALS ON EST SUMMARIES WITH MAR'              
CP27X    DC    X'00'                                                            
         SPACE 2                                                                
CP28     DC    AL1(28),AL2(CP28X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'T',AL1(L'CP28A+2)                                              
CP28A    DC    C'MTHLY TOTALS AT PUB LEVEL AND 1 PUB PER PAGE ON ESTS'          
         DC    C'U',AL1(L'CP28B+2)                                              
CP28B    DC    C'EDT/VENDOR TOTALS AND ONE PUB PER PAGE ON ESTS'                
         DC    C'V',AL1(L'CP28C+2)                                              
CP28C    DC    C'E/V TOTS,MTHLY TOTS, ONE PUB PER PAGE ON ESTS'                 
CP28X    DC    X'00'                                                            
         SPACE 2                                                                
CP29     DC    AL1(29),AL2(CP29X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP29A+2)                                              
CP29A    DC    C'PRINT PUB RECAPS UNDER SUMMARY PAGES ON ESTS'                  
         DC    C'2',AL1(L'CP29B+2)                                              
CP29B    DC    C'PUB RECAPS ON A NEW PAGE ON ESTS'                              
CP29X    DC    X'00'                                                            
         SPACE 2                                                                
CP30     DC    AL1(30),AL2(CP30X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP30A+2)                                              
CP30A    DC    C'AUTHZD $ IN SEPARATE COLUMN ON EST SUMMARIES'                  
         DC    C'2',AL1(L'CP30B+2)                                              
CP30B    DC    C'AUTHZD $ + GR (NET) LESS AUTHZD $ IN SEP.COLS ON ESTS'         
         DC    C'3',AL1(L'CP30C+2)                                              
CP30C    DC    C'AUTHZD $ ON A SEP. LINE UNDER GR (NET) AND DIFFERENCE'         
CP30X    DC    X'00'                                                            
         SPACE 2                                                                
CP31     DC    AL1(31),AL2(CP31X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'S',AL1(L'CP31A+2)                                              
CP31A    DC    C'SUPPRESS CIRCULATION ON ESTIMATES'                             
CP31X    DC    X'00'                                                            
         SPACE 2                                                                
CP32     DC    AL1(32),AL2(CP32X-*+2),AL1(1),X'03',X'00',AL2(0)                 
         DC    C'1',AL1(L'CP32A+2)                                              
CP32A    DC    C'PRINT COPY NUMBER ON ESTIMATES'                                
         DC    C'2',AL1(L'CP32B+2)                                              
CP32B    DC    C'PRINT COPY NUMBER AND CAPTION ON ESTIMATES'                    
CP32X    DC    X'00'                                                            
         SPACE 2                                                                
CPXXX    DC    X'FFFFFF'                END OF TABLE                            
         EJECT                                                                  
CGALIND  DSECT                     GROUP ASSIGNMENTS LINE                       
CGALIN   DS    0CL84                                                            
CGAID    DS    CL1                                                              
         DS    CL2                                                              
CGACODE  DS    CL4                                                              
         DS    CL2                                                              
CGABRK1  DS    CL12                                                             
         DS    CL1                                                              
CGANAME1 DS    CL24                                                             
         DS    CL1                                                              
CGABRK2  DS    CL12                                                             
         DS    CL1                                                              
CGANAME2 DS    CL24                                                             
         SPACE 2                                                                
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
*                                                                               
       ++INCLUDE DDPSTBLK                                                       
*                                                                               
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PPREP4102 09/30/20'                                      
         END                                                                    
