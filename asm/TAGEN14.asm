*          DATA SET TAGEN14    AT LEVEL 115 AS OF 04/08/14                      
*PHASE T70214C,*                                                                
         TITLE 'T70214 - CLIENT MAINTENANCE'                                    
T70214   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TBLLNQ+SVPTRLNQ+UPPTRLNQ,T70214,R7                               
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ACTBL            SAVE ADDR OF INITIAL AGY/CLI TABLE           
         AHI   RE,ACTBLNQ/2                                                     
         AHI   RE,ACTBLNQ/2                                                     
         ST    RE,ACTBLX                                                        
         AHI   RE,1                                                             
         ST    RE,AADDTBL          SAVE ADDR OF AGY/CLI ADD TABLE               
         AHI   RE,ACLNQ*2                                                       
         ST    RE,AADDTBLX                                                      
         AHI   RE,1                                                             
         ST    RE,ASVPTRS                                                       
         AHI   RE,SVPTRLNQ                                                      
         ST    RE,AUPPTRS                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
CLT10    GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
         CLI   MODE,DISPKEY        IF MODE IS DISPLAY KEY                       
         BE    DK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    CLT5                   DO NOT DISPLAY REC FIRST                  
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    CLT30                                                            
*                                                                               
CLT5     CLI   MODE,RECDEL         DELETE RECORD                                
         BE    CLT40                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    CLT40                       HANDLE PTRS & DISPLAY                
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    CLT20                                                            
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    CLT20                                                            
         CLI   MODE,XRECADD        NEW RECORD ADDED                             
         BE    CLT25                                                            
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         BE    CLT25                                                            
         BNE   CLT50                                                            
*                                                                               
CLT20    GOTO1 ADDPTRS,DMCB,ASVPTRS HANDLE PASSIVE POINTERS                     
CLT25    BRAS  RE,UPDCOM           POSSIBLY UPDATE COMMERCIALS                  
         BRAS  RE,NFYVIT           AND NOTIFY VITA OF ACTION                    
*                                                                               
CLT30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     CLTX                                                             
*                                                                               
CLT40    GOTO1 SAVPTRS,DMCB,ASVPTRS    HANDLE PASSIVE POINTERS                  
         CLI   MODE,RECDEL             DELETE RECORD                            
         BNE   CLTX                                                             
         BAS   RE,CHKDEL                                                        
         B     CLTX                                                             
*                                                                               
CLT50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   CLTX                                                             
         BAS   RE,BLDREC                                                        
*                                                                               
CLTX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
*                                                                               
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       MVC   SVAGY,TGAGY         FOR RESET OF AGENCY                          
         LA    R2,SCLAGYH                                                       
         CLI   5(R2),0             IF NO AGENCY INPUT                           
         BNE   VK20                                                             
         MVC   SCLAGY,SPACES       CLEAR PREVIOUS AGY NAME                      
         MVC   SCLAGYN,SPACES                                                   
         OI    SCLAGYH+6,X'80'     TRANSMIT                                     
         OI    SCLAGYNH+6,X'80'                                                 
*                                                                               
         TM    SCRSTAT,SCRCHG      IF 1ST TIME INTO SCREEN                      
         BNO   VK30                   THEN CHECK IF GLOBAL CLIENT               
*                                                                               
VK10     GOTO1 RECVAL,DMCB,TLCLCDQ,(X'04',SCLCLIH)                              
         BNE   VK30                CLIENT REC NOT FOUND                         
         LA    R4,KEY                     IS THIS ONE WITH GLOBAL               
         USING TLCLD,R4                   AGY                                   
         OC    TLCLAGY,TLCLAGY                                                  
         BZ    VK50                                                             
*                                                                               
VK20     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCLAGYH),SCLAGYNH                     
         MVC   SVAGY,TGAGY         FOR RESET OF AGENCY                          
*                                                                               
         L     R4,AIO              GET AGENCY ELEMENT                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   SVOFF,TAAYTPOF      SAVE OFFICE                                  
         MVC   SVAGSTAT,TAAYSTAT   SAVE AGENCY STATUS                           
         MVC   SVAGG,TAAYAGG       SAVE AGENCY GROUP                            
         MVC   SVHLDS,TAAYHLDS     SAVE HOLDING FEE NOTICE STATUS               
         MVC   SVAGSTA6,TAAYSTA6   SAVE AGENCY STATUS 6                         
         MVI   SVAGSTA7,0                                                       
         CLI   TAAYLEN,TAAYLNQ                                                  
         BL    VK40                                                             
         MVC   SVAGSTA7,TAAYSTA7   SAVE AGENCY STATUS 7                         
         B     VK40                                                             
*                                                                               
VK30     XC    TGAGY,TGAGY         CLEAR GLOBAL AGY                             
         LA    R2,SCLCLIH                                                       
*                                                                               
VK40     MVI   TGBYTE,X'20'                                                     
         CLI   MODE,DISPKEY                                                     
         BE    *+8                                                              
         MVI   TGBYTE,X'40'                                                     
         LA    R2,SCLCLIH                                                       
         CLI   8(R2),C' '                                                       
         BE    INVERR                                                           
         GOTO1 RECVAL,DMCB,TLCLCDQ,(TGBYTE,SCLCLIH)                             
         MVC   TGAGY,SVAGY         RESTORE GLOBAL AGY                           
*                                                                               
VK50     XC    INITCLG,INITCLG                                                  
*                                                                               
         CLI   SVOFF,C'O'          ADDITIONAL BILLING?                          
         BE    VK60                                                             
         CLI   SVOFF,C'Q'                                                       
         BE    VK60                                                             
         OI    SCLADRTH+1,X'2C'    LOW INTENSITY AND PROTECTED                  
         OI    SCLRCURH+1,X'2C'                                                 
         OI    SCLRSUTH+1,X'2C'                                                 
         OI    SCLRFICH+1,X'2C'                                                 
         OI    SCLROFIH+1,X'2C'                                                 
         OI    SCLRWCRH+1,X'2C'                                                 
         OI    SCLRCRPH+1,X'2C'                                                 
         OI    SCLRHNDH+1,X'2C'                                                 
         OI    SCLRCANH+1,X'2C'                                                 
         OI    SCLBTY2H+1,X'2C'                                                 
         B     VK70                                                             
*                                                                               
VK60     NI    SCLADRTH+1,X'F3'    REG INTENSITY                                
         NI    SCLRCURH+1,X'D3'    REG INTENSITY AND UN-PROTECTED               
         NI    SCLRSUTH+1,X'D3'                                                 
         NI    SCLRFICH+1,X'D3'                                                 
         NI    SCLROFIH+1,X'D3'                                                 
         NI    SCLRWCRH+1,X'D3'                                                 
         NI    SCLRCRPH+1,X'D3'                                                 
         NI    SCLRHNDH+1,X'D3'                                                 
         NI    SCLRCANH+1,X'D3'                                                 
         NI    SCLBTY2H+1,X'D3'                                                 
*                                                                               
VK70     OI    SCLADRTH+6,X'80'                                                 
         OI    SCLRCURH+6,X'80'                                                 
         OI    SCLRSUTH+6,X'80'                                                 
         OI    SCLRFICH+6,X'80'                                                 
         OI    SCLROFIH+6,X'80'                                                 
         OI    SCLRWCRH+6,X'80'                                                 
         OI    SCLRCRPH+6,X'80'                                                 
         OI    SCLRHNDH+6,X'80'                                                 
         OI    SCLRCANH+6,X'80'                                                 
         OI    SCLBTY2H+6,X'80'                                                 
*                                                                               
VKX      B     XIT                                                              
*        DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       MVC   SCLAGY,SPACES       CLEAR PREVIOUS AGY                           
         OI    SCLAGYH+6,X'80'                                                  
                                                                                
         MVC   SCLAGYN,SPACES      CLEAR PREVIOUS AGY NAME                      
         OI    SCLAGYNH+6,X'80'                                                 
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO                                                           
         MVC   SCLCLI,TLCLCLI      DISPLAY CLIENT CODE                          
         OI    SCLCLIH+6,X'80'                                                  
                                                                                
         MVI   SCLAGYH+5,0         INITIALIZE AGENCY INPUT LENGTH               
         XC    TGAGY,TGAGY         AND GLOBAL AGENCY                            
                                                                                
         OC    TLCLAGY,TLCLAGY     IF AGENCY IS PRESENT                         
         BZ    DKX                                                              
         MVC   SCLAGY,TLCLAGY      DISPLAY AGENCY CODE                          
         MVI   SCLAGYH+5,6                                                      
         OI    SCLAGYH+6,X'80'                                                  
DKX      B     VK                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         XC    STATBYTS,STATBYTS   CLEAR STATUS BYTES                           
         XC    OEORBR,OEORBR       CLEAR OVERRIDE EMPLOYER OF RECORD            
*                                                                               
         XC    SCLTPCN,SCLTPCN     INIT STAFF ID NAME                           
         OI    SCLTPCNH+6,X'80'                                                 
*                                                                               
         TWAXC SCLCLINH                                                         
         MVC   SCLGCSN,SPACES      CLEAR PROTECTED FIELD                        
         OI    SCLGCSNH+6,X'80'                                                 
         MVC   SCLPCLN,SPACES                                                   
         OI    SCLPCLNH+6,X'80'                                                 
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,SCLCLINH       CLIENT NAME                  
         GOTO1 CHAROUT,DMCB,TASNELQ,SCLSNMEH       SHORT NAME                   
         GOTO1 CHAROUT,DMCB,TAADELQ,(4,SCLADDRH)   ADDRESS                      
         GOTO1 CHAROUT,DMCB,(X'80',TAFNELQ),SCLATTNH,TAFNTATT                   
         GOTO1 CHAROUT,DMCB,(X'80',TACMELQ),SCLEMAIH,TACMTYPI  EMAIL            
*                                                                               
*                                  DISPLAY SIGNATORY                            
         GOTO1 CHAROUT,DMCB,TANUELQ,SCLSIGNH,TANUTSIG                           
*                                                                               
         CLI   SCLSIGNH+5,0        DO WE HAVE ONE?                              
         BE    DR2                 NO                                           
*                                  YES, READ AND DISPLAY NAME                   
         MVC   AIO,AIO2                                                         
         MVC   SVAGY,TGAGY         SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SCLSIGNH),SCLSINMH                    
         MVC   AIO,AIO1                                                         
         MVC   TGAGY,SVAGY         RESTORE GLOBAL AGENCY                        
*                                                                               
         USING TACID,R4            R4=A(CLIENT INFORMATION ELEMENT)             
DR2      L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         MVC   STATCI,TACISTAT     SAVE CLIENT STATUS                           
         MVC   STATCI2,TACISTA2                                                 
         MVC   SVCISTA2,TACISTA2                                                
*                                                                               
         TM    TACISTAT,TACIHSPO+TACIHSEL+TACIHSPE+TAAYHSNC                     
         BZ    DR5                                   IF DEFINED                 
         MVC   TGBYTE,TACISTAT                       STRIP OFF NON-             
         NI    TGBYTE,TACIHSPO+TACIHSEL+TACIHSPE+TACIHSNC                       
         LA    RF,HOLDTAB                            HF STATUSES                
DR3      CLI   0(RF),X'FF'                                                      
         BE    DR5                                   AND DISPLAY                
         MVC   BYTE,0(RF)                            HOLDING FEE STATUS         
         NC    BYTE,TGBYTE                                                      
         BNZ   DR4                                                              
         LA    RF,L'HOLDTAB(RF)                                                 
         B     DR3                                                              
DR4      MVC   SCLHLD,1(RF)                                                     
*                                                                               
DR5      DS    0H                                                               
*        DISPLAY STAFF ID                                                       
*                                                                               
         OC    TACITID,TACITID     IF TPC USER ID DEFINED                       
         BZ    DRTPC5                                                           
*                                                                               
         XC    WORK(10),WORK          CALL USERVAL TO GET ALPHA USER ID         
         MVC   WORK+8(2),TACITID                                                
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SCLTID,TGUSERID                                                  
*                                                                               
DRTPC5   MVC   SCLTPC,TACITPC      DISPLAY DEFAULT TPC (AND NAME)               
*                                                                               
         OC    TGUSER,TGUSER       IF USER ID NOT DEFINED FROM ABOVE            
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
*                                                                               
         MVC   AIO,AIO2            USE AIO2 FOR RECVAL CALL TO DISPLAY          
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'88',SCLTPC),SCLTPCNH  STAFF NAME          
         MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
*                                                                               
         OC    TACICLG,TACICLG     IF CLIENT GROUP                              
         BZ    DR10                                                             
         MVC   SCLGCL,TACICLG      DISPLAY CLIENT GROUP & NAME                  
         MVI   SCLGCLH+5,L'TACICLG-2                                            
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCGCDQ,(X'0C',SCLGCLH),SCLGCSNH                     
         MVC   AIO,AIO1            RESTORE IO AREA                              
         DROP  R4                                                               
*                                                                               
DR10     MVC   INITCLG,SCLGCL      SAVE INITIAL CLIENT GROUP                    
*                                                                               
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
DR11     BNE   DR13                                                             
         TM    TABRSTAT,TABRSACP   CAN'T BE ADDIT'L BILLING RULES               
         BZ    DR15                                                             
         BAS   RE,NEXTEL                                                        
         B     DR11                                                             
*                                                                               
DR13     XR    R4,R4               INDICATE NO TABRD ELEMENT                    
         B     DR30                                                             
*                                                                               
DR15     MVC   STATBR,TABRSTAT                                                  
         MVC   OEORBR,TABROEOR                                                  
*                                                                               
DR30     BAS   RE,DRSTAT           DISPLAY STATUS FIELD                         
*                                                                               
         LTR   R4,R4               IF TABRD ELEMENT                             
         BZ    DR50                                                             
         CLI   TABRTYPE,X'C0'      ALPHABETIC OR NUMERIC?                       
         BNH   *+14                                                             
         MVC   SCLBTYP,TABRTYPE                                                 
         B     DR31                                                             
         EDIT  TABRTYPE,(2,SCLBTYP),ALIGN=LEFT                                  
DR31     CLI   TABRHRLS,X'FF'                                                   
         BNE   DR32                                                             
         MVC   SCLAGRT,=C'N  '                                                  
         B     DR40                                                             
DR32     EDIT  TABRHRLS,(2,SCLAGRT),ALIGN=LEFT                                  
*                                                                               
DR40     BAS   RE,DRRATES          DISPLAY RATES                                
*                                                                               
DR50     CLI   SVOFF,C'O'          OFFICE O, ADDITIONAL BILLING RATES           
         BE    *+12                                                             
         CLI   SVOFF,C'Q'          OFFICE Q, ADDITIONAL BILLING RATES           
         BNE   DR60                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         B     DR57                                                             
DR55     BAS   RE,NEXTEL                                                        
DR57     BNE   DR60                                                             
         TM    TABRSTAT,TABRSACP   ADDITIONAL BILLING RATES?                    
         BZ    DR55                                                             
*                                                                               
         CLI   TABRTYPE,X'C0'      ALPHABETIC OR NUMERIC?                       
         BNH   *+14                                                             
         MVC   SCLBTY2,TABRTYPE                                                 
         B     DR58                                                             
         EDIT  TABRTYPE,(2,SCLBTY2),ALIGN=LEFT                                  
DR58     OI    SCLBTY2H+6,X'80'                                                 
         BAS   RE,DRRATES2         DISPLAY ADDITIONAL RATES                     
*                                                                               
         USING TAISD,R4                                                         
DR60     MVI   ELCODE,TAISELQ      GET PROD INTERFACE ELEMENT                   
         GOTO1 GETL,DMCB,=C'C'                                                  
         BNE   DR70                                                             
         L     R4,TGELEM                                                        
         LA    R2,SCLPCLTH                                                      
         CLC   TAISCDE,SPACES      DO NOT DISPLAY IF BLANKS                     
         BNH   DR70                                                             
         MVC   8(3,R2),TAISCDE                                                  
         BAS   RE,GETPCLI                                                       
*                                                                               
DR70     GOTO1 ACTVOUT,DMCB,SCLLCHGH                                            
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY STATUS FIELD                                  
*                                                                               
DRSTAT   NTR1                                                                   
         LA    R2,SCLSTAT          R2=A(STATUS FIELD)                           
*                                                                               
         OC    OEORBR,OEORBR       IF OVERRIDE EMPLOYER OF RECORD               
         BZ    DRSTAT15                                                         
         MVC   0(4,R2),=C'EMP='    DISPLAY IT                                   
         MVC   4(3,R2),OEORBR                                                   
         LA    R2,6(R2)            AND POINT R2 TO LAST NON-SPACE               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRSTAT15 LA    RF,STATTAB          RF=A(STATUS CODE TABLE)                      
*                                                                               
DRSTAT20 CLI   0(RF),X'FF'         WHILE NOT END OF TABLE                       
         BE    DRSTAT40                                                         
         MVC   FULL(3),0(RF)       IF BIT IS ON IN STATUS BYTE                  
         NC    FULL(3),STATBYTS                                                 
         BZ    DRSTAT30                                                         
*                                                                               
         CLI   0(R2),C' '          THEN IF NOT FIRST CODE                       
         BNH   *+12                                                             
         MVI   1(R2),C','          THEN DISPLAY COMMA AND BUMP PAST             
         LA    R2,2(R2)                                                         
         MVC   0(10,R2),3(RF)      DISPLAY LITERAL                              
*                                                                               
         LA    R2,9(R2)            POINT R2 TO LAST NON-SPACE                   
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
DRSTAT30 LA    RF,L'STATTAB(RF)    BUMP TO NEXT STATUS CODE TABLE ENTRY         
         B     DRSTAT20            LOOP BACK                                    
*                                                                               
         USING TANUD,R4                                                         
DRSTAT40 MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPAC))                                     
         BNE   XIT                                                              
         CLI   0(R2),C' '                                                       
         BNH   *+12                                                             
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),=C'AC='                                                  
         L     R4,TGELEM                                                        
         EDIT  TANUOVAM,(6,3(R2)),2,ALIGN=LEFT                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY BILLING RULES ELEMENT RATES                                           
*                                                                               
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
DRRATES  NTR1                                                                   
         CLI   TABRTYPE,TABRTY99   IF NO CHARGE                                 
         BE    DRRATEX             EXIT                                         
*                                                                               
         TM    TABRSTAT,TABRSSRC   IF USING STANDARD RATE CARD                  
         BZ    DRRATE10                                                         
         GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         MVC   TABRRATE(L'TABRRATE),TGBSRC                                      
*                                                                               
DRRATE10 OC    TABRRATE(L'TABRRATE),TABRRATE   IF AROUND                        
         BZ    DRRATEX                                                          
         LA    R2,SCLCFUTH         DISPLAY BILLING RATES                        
         LA    R3,TABRRATE                                                      
         LA    RF,4                DISPLAY THE 1ST FOUR                         
         BAS   RE,OUTRATE                                                       
*                                                                               
         LA    R2,SCLCWCRH         DISPLAY WC FOR CORPS                         
         LA    R3,TABRWCRP                                                      
         LA    RF,1                RESET RF                                     
         BAS   RE,OUTRATE                                                       
*                                                                               
         LA    R2,SCLCCRPH         DISPLAY CORP                                 
         LA    R3,TABRCORP                                                      
         LA    RF,4                RESET RF                                     
         BAS   RE,OUTRATE                                                       
*                                                                               
DRRATEX  B     XIT                                                              
         EJECT                                                                  
* DISPLAY BILLING RULES ELEMENT RATES                                           
*                                                                               
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
DRRATES2 NTR1                                                                   
         OC    TABRRATE(L'TABRRATE),TABRRATE   IF AROUND                        
         BZ    DRRATE2X                                                         
         LA    R2,SCLRCURH         DISPLAY BILLING RATES                        
         LA    R3,TABRRATE                                                      
         LA    RF,4                DISPLAY THE 1ST FOUR                         
         BAS   RE,OUTRATE                                                       
*                                                                               
         LA    R2,SCLRWCRH         DISPLAY WC FOR CORPS                         
         LA    R3,TABRWCRP                                                      
         LA    RF,1                RESET RF                                     
         BAS   RE,OUTRATE                                                       
*                                                                               
         LA    R2,SCLRCRPH         DISPLAY CORP                                 
         LA    R3,TABRCORP                                                      
         LA    RF,3                RESET RF                                     
         BAS   RE,OUTRATE                                                       
*                                                                               
DRRATE2X B     XIT                                                              
         EJECT                                                                  
*        OUTPUT BILLING RATES                                                   
*                                                                               
OUTRATE  NTR1                                                                   
*                                                                               
OUT10    EDIT  (2,0(R3)),(5,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                    
         OI    6(R2),X'80'         TRANSMIT                                     
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         BO    *-10                                                             
         LA    R3,2(R3)            AND TO NEXT FIELD IN ELEMENT                 
         BCT   RF,OUT10                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BLD1                SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
                                                                                
BLD1     GOTO1 SAVPTRS,DMCB,ASVPTRS SAVE PASSIVE POINTERS FOR CHANGE            
*                                                                               
         MVI   TARAFLAG,0                                                       
         MVI   SVGRUL,0                                                         
         MVI   SVBTYPE,0                                                        
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    BLD1A                                                            
*                                                                               
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BAS   RE,GETEL                                                         
         J     *+8                                                              
BLD1AA   BAS   RE,NEXTEL                                                        
         JNE   BLD1AB                                                           
         TM    TABRSTAT,TABRSACP   SKIP IF ADDITIONAL RATE                      
         JO    BLD1AA                                                           
         MVC   SVBTYPE,TABRTYPE    SAVE ORIGINAL BILLING TYPE                   
*                                                                               
         USING TARAD,R4                                                         
BLD1AB   L     R4,AIO                                                           
         MVI   ELCODE,TARAELQ      TEST IF BILLING RATES ELEMENT                
         BAS   RE,GETEL            EXISTS                                       
         JNE   BLD1A                                                            
         OI    TARAFLAG,TARAFYES                                                
         MVC   SVGRUL,TARAGRUL                                                  
*                                                                               
BLD1A    MVI   ELCODE,TACIELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TABRELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAISELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TAISTYPC))                                     
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 DELL,DMCB,(1,=AL1(TANUTPAC))                                     
*                                                                               
         GOTO1 NAMIN,DMCB,TANAELQ,SCLCLINH         CLIENT NAME                  
         GOTO1 NAMIN,DMCB,TASNELQ,(X'80',SCLSNMEH) SHORT NAME                   
         GOTO1 ADDRIN,DMCB,(X'80',SCLADDRH)        ADDRESS                      
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SCLATTNH),TAFNTATT ATTENTION           
*                                                                               
         BAS   RE,VREMAIL          VALIDATE OPTIONAL EMAIL ADDRESS              
*                                                                               
         CLI   SCLSIGNH+5,0        SIGNATORY ENTERED?                           
         BNE   BLD2                YES                                          
*                                                                               
         MVC   SCLSINM,SPACES      NO, CLEAR PREVIOUS NAME                      
         OI    SCLSINMH+6,X'80'    TRANSMIT                                     
         B     BLD5                DELETE ELEMENT                               
*                                                                               
*                                  VALIDATE SIGNATORY                           
BLD2     MVC   AIO,AIO2            SWAP IO                                      
         MVC   SVAGY,TGAGY         SAVE GLOBAL AGENCY                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,SCLSIGNH                                     
         MVC   AIO,AIO1            RESTORE IO                                   
         MVC   TGAGY,SVAGY         RESTORE GLOBAL AGENCY                        
*                                                                               
*                                  ADD/REMOVE SIGNATORY ELEMENT                 
BLD5     GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SCLSIGNH),TANUTSIG                     
*                                                                               
         BAS   RE,BLDSTAT          VALIDATE STATUS FIELD                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TACID,R4                                                         
         MVC   TACISTAT,STATCI     CLIENT STATUS                                
         MVC   TACISTA2,STATCI2    CLIENT 2 STATUS                              
         TM    SVCISTA2,TACISREG                                                
         BZ    *+8                                                              
         OI    TACISTA2,TACISREG                                                
*                                                                               
*        VALIDATE STAFF ID                                                      
*                                                                               
         CLI   SCLTIDH+5,0         VALIDATE TPC USER-ID (OPTIONAL)              
         BE    BLDTPC1                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 USERVAL,DMCB,SCLTIDH                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   TACITID,TGUSER                                                   
*                                                                               
         B     BLDTPC2                                                          
*                                                                               
BLDTPC1  CLI   SCLTPCH+5,0         VALIDATE DEFAULT TPC (OPTIONAL)              
         BE    BLDTPC5                                                          
*                                                                               
BLDTPC2  MVC   TGUSER,TACITID      SET USER-ID FROM EL.                         
*                                                                               
         OC    TGUSER,TGUSER       IF NOT DEFINED                               
         BNZ   *+10                                                             
         MVC   TGUSER,TWAORIG      USE CONNECT ID FOR USER-ID                   
*                                                                               
         NI    SCLTPCH+4,X'DF'     INSURE WE ALWAYS RE-VALIDATE                 
         GOTO1 RECVAL,DMCB,TLSTCDQ,SCLTPCH                                      
         MVC   TACITPC,TGSTAF                                                   
*                                                                               
BLDTPC5  DS    0H                                                               
*                                                                               
         CLI   SCLHLDH+5,0         VALIDATE HOLDING FEE FIELD                   
         BE    BLD9                                                             
         LA    R2,SCLHLDH                                                       
         TM    SVHLDS,TAAYHSNO     INPUT NOT VALID IF AGENCY                    
         BO    HFPENA              SET FOR NO NOTICES                           
         LA    RF,HOLDTAB                                                       
BLD7     CLI   0(RF),X'FF'                                                      
         BE    INVERR                                                           
         CLC   SCLHLD,1(RF)                                                     
         BE    BLD8                                                             
         LA    RF,L'HOLDTAB(RF)                                                 
         B     BLD7                                                             
BLD8     OC    TACISTAT,0(RF)                                                   
*                                                                               
*        TM    TACISTAT,TAAYHSEL+TAAYHSPE                                       
*        BNZ   INVERR              TEMPORARILY DISALLOW PE AND EL               
*                                                                               
BLD9     LA    R2,SCLGCLH          CLIENT GROUP                                 
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD10                                                            
         MVC   AIO,AIO2            CHANGE IO AREA FOR RECVAL                    
         GOTO1 RECVAL,DMCB,TLCGCDQ,SCLGCLH                                      
         MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   TACICLG,SCLGCL      CLIENT GRP CODE                              
         OC    TACICLG,SPACES                                                   
*                                                                               
BLD10    OC    ELEMENT,ELEMENT     IF TACID ELEMENT TO ADD                      
         BZ    BLD15                                                            
         MVI   TACIEL,TACIELQ      SET ELEMENT CODE                             
         MVI   TACILEN,TACILNQ     SET ELEMENT LENGTH                           
         GOTO1 ADDELEM             AND ADD IT                                   
         DROP  R4                                                               
*                                                                               
BLD15    XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TABRD,R4                                                         
         MVC   TABRSTAT,STATBR     BILLING RULES STATUS                         
         MVC   TABROEOR,OEORBR     OVERRIDE EMPLOYER OF RECORD                  
*                                                                               
         LA    R2,SCLBTYPH         BILLING TYPE                                 
         CLI   5(R2),0                                                          
         BNE   BLD70                                                            
         TM    TABRSTAT,TABRSSRC   IF THERE IS A STANDARD RATE CARD             
         BNO   BLD80                                                            
         B     MISSERR             MUST HAVE A BILLING TYPE                     
*                                                                               
BLD70    MVC   TABRTYPE,SCLBTYP                                                 
         CLC   TGAGY,=CL6'8800'    AGENCY MUST BE 8800 FOR BILL TYPE E          
         BNE   *+16                                                             
         CLI   SCLBTYP,TABRTYE                                                  
         BNE   INVERR                                                           
         B     BLD75                                                            
         GOTO1 VALINUM             MUST BE NUMERIC IF NOT C'E'                  
         MVC   TABRTYPE,ACTUAL                                                  
BLD75    GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         BNE   INVERR                                                           
         CLC   SVBTYPE,TABRTYPE    IF BILLING TYPE IS CHANGING                  
         JE    BLD80                                                            
         TM    TGBTSTAT,BTYPSDFT   CAN'T CHANGE TO DEFUNCT BILLING TYPE         
         JO    INVERR                                                           
         TM    TARAFLAG,TARAFYES   CAN'T CHANGE IF AGY HAS BRATE                
         JO    ERRCHGBR                                                         
*                                                                               
BLD80    LA    R2,SCLAGRTH         APPLIED GRT HANDLING RULE                    
         CLI   5(R2),0                                                          
         BE    BLD85                                                            
         CLI   SCLAGRT,C'N'                                                     
         BNE   BLD83                                                            
         MVI   TABRHRLS,X'FF'                                                   
         B     BLD84                                                            
*                                                                               
BLD83    ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SCLAGRT(0)                                                   
         CVB   R5,DUB                                                           
         STC   R5,TABRHRLS                                                      
*                                                                               
BLD84    TM    TARAFLAG,TARAFYES   HAS BRATE?                                   
         JZ    BLD85                                                            
         CLC   SVGRUL,TABRHRLS     CAN'T CHANGE APPL GRT                        
         JNE   INVERR                                                           
*                                                                               
BLD85    BAS   RE,BLDRATES         VALIDATE BILLING RATES FIELD                 
*                                                                               
         OC    ELEMENT,ELEMENT     IF TABRD ELEMENT TO ADD                      
         BZ    BLDR100                                                          
         MVI   TABREL,TABRELQ      SET ELEMENT CODE                             
         MVI   TABRLEN,TABRLNQ     SET ELEMENT LENGTH                           
         GOTO1 ADDELEM             AND ADD IT                                   
*                                                                               
BLDR100  CLI   SVOFF,C'O'          SPECIAL ADD'L BILLING RATES?                 
         BE    *+12                                                             
         CLI   SVOFF,C'Q'                                                       
         BNE   BLDR200                                                          
         XC    TABRRATE,TABRRATE                                                
         LA    R2,SCLBTY2H                                                      
         MVC   TABRTYPE,SCLBTY2                                                 
         OI    TABRSTAT,TABRSACP   ADDITIONAL BILLING RATES                     
         CLC   TGAGY,=CL6'8800'    AGENCY MUST BE 8800 FOR BILL TYPE E          
         BNE   *+16                                                             
         CLI   SCLBTY2,TABRTYE                                                  
         BNE   INVERR                                                           
         B     BLDR105                                                          
         GOTO1 VALINUM             MUST BE NUMERIC IF NOT C'E'                  
         MVC   TABRTYPE,ACTUAL                                                  
BLDR105  GOTO1 BTYPVAL,DMCB,TABRTYPE                                            
         BNE   INVERR                                                           
         BAS   RE,BLDRATE2         VALIDATE BILLING RATES FIELD                 
         OC    TABRRATE,TABRRATE   ADDITIONAL RATE REQUIRED FOR OFF 9           
         BNZ   BLDR108                                                          
         LA    R2,SCLRCURH                                                      
         B     INVERR                                                           
BLDR108  MVI   TABREL,TABRELQ      SET ELEMENT CODE                             
         MVI   TABRLEN,TABRLNQ     SET ELEMENT LENGTH                           
BLDR110  GOTO1 ADDELEM                                                          
*                                                                               
BLDR200  LA    R2,SCLPCLTH         PRODUCTION CLIENT                            
         CLI   5(R2),0                                                          
         BE    BLD500                                                           
         CLI   SCLAGYH+5,0         INPUT ALLOWED ONLY IF AGENCY INPUT           
         BE    INVERR                                                           
         BAS   RE,GETPCLI                                                       
*                                                                               
         USING TAISD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         MVI   TAISEL,TAISELQ                                                   
         MVI   TAISLEN,TAISLNQ                                                  
         MVI   TAISTYPE,TAISTYPC                                                
         MVC   TAISCDE,SCLPCLT                                                  
         OC    TAISCDE,SPACES                                                   
         GOTO1 ADDELEM             ADD NEW ELEMENT                              
*                                                                               
BLD500   GOTO1 ACTVIN,DMCB,SCLLCHGH                LAST CHANGED                 
*                                                                               
BLD510   MVC   KEY,SVKEY           RESTORE KEY OF ORIGINAL RECORD               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   BLD520                                                           
         BAS   RE,MYADDREC                                                      
         MVI   IOOPT,C'Y'                                                       
*                                                                               
BLD520   GOTO1 ADDPTRS,DMCB,ASVPTRS HANDLE PASSIVE POINTERS                     
*                                                                               
         BAS   RE,UPDSTAFF                                                      
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING NO NEED TO RE-READ                 
         BE    BLD530                                                           
         MVC   AIO,AIO2            ELSE RE-GET RECORD TO PREVENT                
         GOTO1 GETREC                   PUTREC/GETREC SYNDROME                  
BLD530   MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
BLDX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE OPTIONAL EMAIL ADDRESS                                               
*                                                                               
VREMAIL  NTR1                                                                   
         LA    R2,SCLEMAIH                                                      
         CLI   5(R2),0                                                          
         BE    VREM40                                                           
         LA    RE,35                                                            
         LA    RF,SCLEMAI                                                       
VREM20   CLI   0(RF),C'@'          THERE MUST BE A '@'                          
         BE    VREM30                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM20                                                        
         B     INVERR                                                           
*                                                                               
         LA    RE,35                                                            
         LA    RF,SCLEMAI                                                       
VREM30   CLI   0(RF),C'.'          THERE MUST BE A '.'                          
         BE    VREM40                                                           
         AHI   RF,1                                                             
         BCT   RE,VREM30                                                        
         B     INVERR                                                           
*                                                                               
VREM40   GOTO1 NAMIN,DMCB,TACMELQ,(X'C0',SCLEMAIH),TACMTYPI                     
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATUS FIELD                                                         
*                                                                               
BLDSTAT  NTR1                                                                   
         XC    STATBYTS,STATBYTS   CLEAR STATUS BYTES                           
         XC    OEORBR,OEORBR       CLEAR OVERRIDE EMPLOYER OF RECORD            
*                                                                               
         LA    R2,SCLSTATH         R2=A(STATUS FIELD)                           
         CLI   5(R2),0             IF INPUT                                     
         BE    XIT                                                              
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   4(R1),0                                                          
         BE    FLDERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         LA    R5,BLOCK            R5=A(SCAN BLOCK)                             
         USING SCAND,R5                                                         
         SR    R3,R3               R3=A(DISPLACEMENT INTO FIELD)                
*                                                                               
BLDST20  CLC   =C'EMP',SCDATA1     IF EMPLOYER OVERRIDE                         
         BNE   BLDST30                                                          
         CLC   =C'PG ',SCDATA2     EMPLOYER PG IS NO LONGER VALID               
         BE    ERRPGEMP                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'80',SCDATA2)                              
         BNE   FLDERR                                                           
         MVC   OEORBR,SCDATA2                                                   
         ZIC   RF,SCLEN2           BUMP R3 PAST RHS ON SCREEN                   
         LA    R3,1(R3,RF)                                                      
         B     BLDST50             BUMP PAST FIELD                              
*                                                                               
BLDST30  LA    RF,STATTAB          LOOP THROUGH STATUS TABLE                    
BLDST40  CLI   0(RF),X'FF'         ERROR IF END OF TABLE FOUND                  
         BE    BLDST90                                                          
*                                                                               
         ZIC   R1,SCLEN1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),3(RF)    EXACT MATCH TO TABLE VALUE                   
         BE    *+12                                                             
         LA    RF,L'STATTAB(RF)                                                 
         B     BLDST40                                                          
*                                                                               
         CLI   SCLEN2,0            INSURE NO INPUT ON RHS                       
         BNE   FLDERR                                                           
         OC    STATBYTS,0(RF)      SET APPROPRIATE BIT IN APPR BYTE             
*                                                                               
BLDST50  ZIC   RF,SCLEN1           BUMP R3 TO NEXT STATUS CODE                  
         LA    R3,1(R3,RF)                                                      
         LA    R5,SCANNEXT         BUMP R5 TO NEXT SCANNER ENTRY                
         BCT   R0,BLDST20          AND CONTINUE                                 
*                                                                               
         TM    STATBYTS+1,TACISCOD COD CLIENT NOT VALID IF COD AGENCY           
         BZ    BLDST60                                                          
         TM    SVAGSTAT,TAAYSCOD                                                
         BO    INVERR                                                           
*                                                                               
BLDST60  TM    STATBYTS+1,TACIJPCY IF CSF YES,                                  
         BZ    BLDST70                                                          
         TM    STATBYTS+2,TACIJPCN NOCSF IS NOT VALID                           
         BO    INVERR                                                           
         TM    SVAGSTA6,TAAYJPCY   IF AGENCY HAS CSF,                           
         BO    INVERR              CLIENT CANNOT HAVE CSF                       
*                                                                               
BLDST70  TM    STATBYTS+2,TACIJPCN IF NOCSF,                                    
         BZ    BLDST80                                                          
         TM    SVAGSTA6,TAAYJPCY   IF AGENCY DOES NOT HAVE CSF,                 
         BNO   INVERR              CLIENT CANNOT HAVE NOCSF                     
*                                                                               
BLDST80  TM    STATBYTS+2,TACISREG REG STATUS ONLY VALID FOR TAL2 -TST          
         BZ    XIT                                                              
         CLC   =C'D2',TWAAGY                                                    
         JNE   INVERR                                                           
*                                                                               
BLDST90  B     FLDERR                                                           
*&&DO                                                                           
         USING TANUD,R4                                                         
BLDST90  CLC   =C'AC',SCDATA1                                                   
         BNE   FLDERR                                                           
         TM    SVAGSTA7,TAAYSPPL   ONLY VALID FOR CLIENTS UNDER P+              
         BZ    INVERR              AGENCIES                                     
         MVI   ELCODE,TANUELQ      SEE IF AGENCY COMMISSION ENTERED             
         GOTO1 GETL,DMCB,(1,=AL1(TANUTPAC))                 ALREADY             
         BE    INVERR                                                           
         ZIC   RF,SCLEN2                                                        
         GOTO1 CASHVAL,DMCB,SCDATA2,(RF)                                        
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         CLC   4(4,R1),=F'9999'                                                 
         JH    INVERR                                                           
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD OVERSCALE AMOUNT ELEMENT                 
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ2                                                 
         MVI   TANUTYPE,TANUTPAC                                                
         MVC   TANUOVAM,4(R1)                                                   
         GOTO1 ADDELEM                                                          
         B     BLDST50                                                          
         DROP  R4,R5                                                            
*&&                                                                             
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE BILLING RATES AND UPDATES BILLING RULES ELEMENT                      
*                                                                               
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
BLDRATES NTR1                                                                   
         CLI   TABRTYPE,TABRTY99   IF THERE'S A CHARGE                          
         BE    BLDRATEX                                                         
         TM    TABRSTAT,TABRSSRC   IF NOT STANDARD RATE CARD                    
         BO    BLDRATEX                                                         
*                                                                               
         LA    R0,4                                                             
         LA    R5,TABRRATE         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SCLCFUTH         VALIDATE BILLING RATES                       
         CLI   5(R2),0                                                          
         BNE   BLDRAT20                                                         
         BAS   RE,TSTFLD           IF INPUT IN ANY FIELD                        
         BNE   MISSERR                MUST HAVE INPUT IN FIRST FIELD            
         CLI   SCLBTYPH+5,0                                                     
         BNE   MISSERR             MUST HAVE RATES IF BILLING TYPE              
         B     BLDRATEX            NO BILLING RATES                             
*                                                                               
BLDRAT10 CLI   5(R2),0             ANY INPUT                                    
         BE    BLDRAT40                                                         
*                                                                               
BLDRAT20 CLI   SCLBTYPH+5,0        MUST HAVE BILLING TYPE IF RATES              
         BNE   *+12                                                             
         LA    R2,SCLBTYPH         INPUT                                        
         B     MISSERR                                                          
         BAS   RE,GETRATE                                                       
*                                                                               
BLDRAT40 BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,BLDRAT10         AND CONTINUE                                 
*                                                                               
         LA    R0,3                                                             
         LA    R5,TABRCORP         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SCLCCRPH         VALIDATE BILLING RATES                       
*                                                                               
BLDRAT60 CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,GETRATE                                                       
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,BLDRAT60         AND CONTINUE                                 
*                                                                               
         LA    R2,SCLCCANH         CANADIAN HANDLING                            
         CLI   TABRTYPE,TABRTY24   BILLING TYPE 24                              
         BNE   *+14                                                             
         OC    TABRCAN,TABRCAN                                                  
         BNZ   ERR24CAN                                                         
*                                                                               
         LA    R5,TABRWCRP         VALIDATE WC FOR CORPS                        
         LA    R2,SCLCWCRH                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,GETRATE                                                       
*                                                                               
BLDRATEX B     XIT                                                              
         EJECT                                                                  
* VALIDATE BILLING RATES AND UPDATES BILLING RULES ELEMENT                      
*                                                                               
         USING TABRD,R4            R4=A(BILLING RULES ELEMENT)                  
BLDRATE2 NTR1                                                                   
         LA    R0,4                                                             
         LA    R5,TABRRATE         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SCLRCURH         VALIDATE BILLING RATES                       
*                                                                               
BLDRT210 CLI   5(R2),0                                                          
         BE    BLDRT240                                                         
         BAS   RE,GETRATE                                                       
*                                                                               
BLDRT240 BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,BLDRT210         AND CONTINUE                                 
*                                                                               
         LA    R0,3                                                             
         LA    R5,TABRCORP         R5=A(FIELD IN ELEMENT)                       
         LA    R2,SCLRCRPH         VALIDATE BILLING RATES                       
*                                                                               
BLDRT260 CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,GETRATE                                                       
         BAS   RE,BUMP             BUMP TO NEXT FIELD                           
         L     R5,MYFULL                                                        
         L     R2,FULL                                                          
         BCT   R0,BLDRT260         AND CONTINUE                                 
*                                                                               
         LA    R2,SCLRCANH         CANADIAN HANDLING                            
         CLI   TABRTYPE,TABRTY24   BILLING TYPE 24                              
         BNE   *+14                                                             
         OC    TABRCAN,TABRCAN                                                  
         BNZ   ERR24CAN                                                         
*                                                                               
         LA    R5,TABRWCRP         VALIDATE WC FOR CORPS                        
         LA    R2,SCLRWCRH                                                      
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,GETRATE                                                       
*                                                                               
BLDRT2X  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE GETS AMOUNTS FROM FIELDS                                       
*        R2 - FIELD WITH AMOUNT IN IT                                           
*        R5 - A(FIELD IN TABR TO INSERT INTO)                                   
*                                                                               
GETRATE  NTR1                                                                   
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE RATE XX.XX%                     
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         CLC   4(4,R1),=F'5000'    DISALLOW GT 50% (ALSO NEG. RATES)            
         BH    INVERR                                                           
         MVC   0(2,R5),6(R1)       SAVE 2 BYTES                                 
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE BUMPS TO NEXT  FIELDS                                          
*        R5 - A(FIELD IN TABR TO INSERT INTO)                                   
*                                                                               
BUMP     NTR1                                                                   
         LA    R5,2(R5)            BUMP TO NEXT FIELD IN ELEMENT                
         XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         BO    *-10                                                             
         LA    R1,SCLCHANH         IF HAVE INPUT - MUST HAVE HANDLING           
         CR    R2,R1                                                            
         BNE   BMPX                   FEE TOO                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
BMPX     ST    R2,FULL                                                          
         ST    R5,MYFULL                                                        
         B     XIT                                                              
         EJECT                                                                  
*        ROUTINE CHECKS THAT THERE ARE NO PRODUCTS/COMMERCIALS                  
*                FOR THIS CLIENT BEFORE DELETING                                
*                                                                               
         USING TLCLD,R3                                                         
         USING TLPRPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
         OC    TLCLAGY,TLCLAGY     IF GLOBAL CLIENT - CHECK COMMERCIALS         
         BZ    CHKD10                                                           
*                                                                               
         MVI   TLPRPCD,TLPRNCDQ    BUILD PASSIVE KEY FOR PRODUCT                
         MVC   TLPRNCLI,TLCLCLI    USING SAME CLIENT AS RECORD                  
         MVC   TLPRNAGY,TLCLAGY          AND AGENCY                             
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLPRNCLI+L'TLPRNCLI-TLPRPKEY),KEYSAVE                        
         BE    CHKD20              IF CLIENT HAS PRDS CANNOT DELETE             
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY             CHECK FOR COMMERCIALS                        
         LA    R4,KEY                                                           
         USING TLCOD,R4                                                         
*                                                                               
         MVI   TLCOCD,TLCOCDQ      BUILD PASSIVE KEY FOR PRODUCT                
         MVC   TLCOCLI,TLCLCLI     USING SAME CLIENT AS RECORD                  
         MVC   TLCOAGY,TLCLAGY           AND AGENCY                             
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCOCLI+L'TLCOCLI-TLCOKEY),KEYSAVE                           
         BE    CHKD20              IF CLIENT HAS COMML'S CANNOT DELETE          
         B     CHKDX                                                            
         DROP  R4                                                               
*                                                                               
         USING TLCOPD,R4           CHECK FOR COMM'L - GLOBAL CLIENTS            
CHKD10   MVI   TLCOPCD,TLCONCDQ    BUILD PASSIVE KEY FOR COMMERCIAL             
         MVC   TLCONCLI,TLCLCLI    USING SAME CLIENT AS RECORD                  
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCONCLI+L'TLCONCLI-TLCOPKEY),KEYSAVE                        
         BE    CHKD20              IF CLIENT HAS COMML'S CANNOT DELETE          
         B     CHKDX               (EVEN IF FOR SPECIFIC AGY)                   
         DROP  R4                                                               
*                                                                               
CHKD20   LA    R2,SCLCLIH          IF CLIENT CANNOT BE DELETED                  
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         B     NODELETE            ERROR - CAN'T DELETE                         
*                                                                               
CHKDX    MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*        TEST FIELDS TO SEE IF ANY INPUT                                        
*                       R0 - NUMBER OF FIELDS TO TEST                           
*                       R2 - A(FIRST FIELD HEADER)                              
         SPACE 1                                                                
TSTFLD   NTR1                                                                   
         LA    R0,8                                                             
*                                                                               
TST10    CLI   5(R2),0             ANY INPUT                                    
         BNE   NO                  RETURN CC                                    
         ZIC   R3,0(R2)                                                         
         AR    R2,R3               BUMP NEXT FIELD                              
         BCT   R0,TST10                                                         
         B     YES                                                              
         SPACE 2                                                                
* GET PRODUCTION CLIENT FROM ACC SIDE                                           
* R2 - FIELD HEADER                                                             
         SPACE 1                                                                
GETPCLI  NTR1                                                                   
         MVC   KEY,SPACES          ACC SIDE USES SPACES                         
         MVC   KEY+1(2),=C'SJ'     U/L                                          
         MVC   KEY+3(3),8(R2)      PRODUCTION CLIENT                            
         OC    KEY+3(3),SPACES                                                  
         MVC   AIO,AIO2            SWITCH IO AREA                               
         GOTO1 READACC,DMCB,(X'80',SCLPCLNH) GET RECORD AND NAME                
         MVC   AIO,AIO1            RESTORE AIO                                  
         BNE   ERRXIT              ERROR ALREADY SET IN READACC                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD THIS CLIENT TO STAFF RECORDS WITH                 
*              THE NEW CLIENT GROUP                                             
         SPACE 1                                                                
UPDSTAFF NTR1                                                                   
         CLI   SCLGCLH+5,0         IF CLIENT GROUP WAS CHANGED                  
         BE    NO                                                               
         OC    SCLGCL,SPACES                                                    
         CLC   SCLGCL,INITCLG      GO UPDATE ALL STAFF RECORDS                  
         BE    NO                  WITH THE NEW CLIENT GROUP                    
         SPACE 1                                                                
         OC    SCLAGY,SPACES                                                    
         OC    SCLCLI,SPACES                                                    
         SPACE 1                                                                
         USING ACTBLD,R5                                                        
         L     R5,AADDTBL        PUT THIS CLIENT INTO ADD TABLE                 
         XC    0(ACLNQ,R5),0(R5)                                                
         MVI   ACSTAT,TAVANDEF                                                  
         MVC   ACAGY,SCLAGY                                                     
         MVC   ACCLI,SCLCLI                                                     
         MVI   ACLNQ(R5),X'FF'                                                  
         DROP  R5                                                               
         SPACE 1                                                                
         MVC   AIO,AIO2          WILL USE AIO2 FOR STAFF RECORDS                
         SPACE 1                                                                
         USING TLSTPD,R4                                                        
         LA    R4,KEY            READ ALL STAFF RECORDS BELONGING               
         XC    KEY,KEY           TO NEW CLIENT GROUP                            
         MVI   TLSTPCD,TLSTCCDQ                                                 
         MVC   TLSTCCGP,SCLGCL                                                  
         OC    TLSTCCGP,SPACES                                                  
         GOTO1 HIGH                                                             
         B     USTAFF20                                                         
USTAFF10 GOTO1 SEQ                                                              
USTAFF20 CLC   KEY(TLSTCUSR-TLSTPD),KEYSAVE                                     
         BNE   USTAFF70                                                         
         CLI   TLSTCSEQ,0                                                       
         BNE   USTAFF30                                                         
         MVC   SVSTKEY,KEY                                                      
USTAFF30 GOTO1 GETREC                                                           
         SPACE 1                                                                
         BAS   RE,BLDTBL         BUILD INITIAL AGENCY/CLIENT TABLE              
         SPACE 1                                                                
         USING ACTBLD,R3                                                        
         L     R3,ACTBL                                                         
USTAFF40 CLI   0(R3),X'FF'       CLIENT'S AGENCY MUST ALREADY BE                
         BE    USTAFF10          ON STAFF RECORD                                
         CLC   ACAGY,SCLAGY                                                     
         BE    USTAFF50                                                         
         LA    R3,ACLNQ(R3)                                                     
         B     USTAFF40                                                         
         SPACE 1                                                                
USTAFF50 MVC   0(1,R5),ACSTAT    SET DEFAULT STATUS                             
         DROP  R3                                                               
         SPACE 1                                                                
         MVC   KEY,SVSTKEY       GO BACK TO READ FIRST STAFF RECORD             
         GOTO1 HIGH              FOR THIS STAFF MEMBER                          
         CLC   KEY,KEYSAVE                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         OI    WHENOK,X'01'                                                     
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,SVSTPTR                                             
         NI    WHENOK,X'FF'-X'01'                                               
         SPACE 1                                                                
         BAS   RE,UPDRECS                                                       
         SPACE 1                                                                
USTAFF60 MVC   KEY,SVSTKEY       GO BACK TO READ FIRST STAFF RECORD             
         GOTO1 HIGH              FOR THIS STAFF MEMBER                          
         B     USTAFF10                                                         
         SPACE 1                                                                
USTAFF70 MVC   KEY,SVKEY         RESTORE KEY OF ORIGINAL RECORD                 
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         EJECT                                                                  
       ++INCLUDE TASTBLD                                                        
         SPACE 2                                                                
ERRALOA  DC    H'00'                                                            
         SPACE 1                                                                
ERREXC   B     USTAFF60                                                         
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE ADDS/RESTORES DELETED RECORD                             
                                                                                
MYADDREC NTR1                                                                   
         USING TLRCD,R4                                                         
         L     R4,AIO              R4=A(RECORD)                                 
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY              R3=A(KEY)                                    
         XC    KEY,KEY                                                          
         MVC   TLDRKEY,TLRCKEY     SET ACTIVE KEY                               
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
                                                                                
         CLC   TLDRKEY,KEYSAVE     IF KEY ALREADY EXISTS                        
         BNE   MAR10                                                            
         TM    TLDRSTAT,X'80'      ENSURE IT IS DELETED                         
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   TLDRKEY,KEYSAVE     RESTORE ORIGINAL KEY                         
         XC    TLDRSTAT,TLDRSTAT   CLEAR DELETED STATUS                         
         GOTO1 WRITE               AND WRITE IT BACK                            
         DROP  R3                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         L     R4,AIO              R4=A(RECORD)                                 
         MVI   RDUPDATE,C'Y'       THEN READ RECORD FOR UPDATE                  
         GOTO1 GETREC                                                           
         XC    TLRCSTAT,TLRCSTAT   MARK IT UNDELETED                            
         GOTO1 PUTREC              AND PUT IT                                   
         ST    R4,AIO                                                           
         B     MAR20                                                            
         DROP  R4                                                               
                                                                                
MAR10    GOTO1 ADDREC              IF KEY DOES NOT ALREADY EXIST                
MAR20    NI    DMINBTS,X'F7'       JUST ADD RECORD                              
         B     XIT                                                              
         EJECT                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
NODELETE MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         B     ERRXIT                                                           
*                                                                               
FLDERR   MVI   ERROR,INVALID       INVALID WITH DISP INTO FIELD                 
         STC   R3,ERRDISP                                                       
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRPGEMP MVC   MYMSGNO,=Y(ERPGINV) EMPLOYER PG NO LONGER EXISTS                 
         B     TWOBYTE                                                          
*                                                                               
ERR24CAN MVC   MYMSGNO,=Y(ERMUS501)  CANADIAN HANDLING IS 0 FOR BILL 24         
         B     TWOBYTE                                                          
*                                                                               
ERRCHGBR MVC   MYMSGNO,=Y(ERRBRATE)  CAN'T CHANGE BRATE EXISTS                  
         B     TWOBYTE                                                          
*                                                                               
HFPENA   MVC   MYMSGNO,=Y(ERHFPENA) PAPER/ELECTRONIC STATUS NOT ALLOWED         
         B     TWOBYTE                                                          
*                                                                               
TWOBYTE  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
*                                                                               
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
STATTAB  DS    0CL13               TABLE OF STATUS CODES & THEIR BITS           
         DC    AL1(TABRSQTR,0,0),CL10'ESTPDQTR'                                 
**NO-OP* DC    AL1(TABRS75K,0,0),CL10'MAX75'                                    
         DC    AL1(TABRSNWK,0,0),CL10'NWK'                                      
         DC    AL1(TABRSSRC,0,0),CL10'SRC'                                      
         DC    AL1(TABRSNIN,0,0),CL10'NOINT'                                    
         DC    AL1(TABRSINT,0,0),CL10'INT'                                      
         DC    AL1(0,TACISLCK,0),CL10'LOCKED'                                   
         DC    AL1(0,TACISCOD,0),CL10'PUR'                                      
         DC    AL1(0,TACIDCAE,0),CL10'NCLE'                                     
         DC    AL1(0,TACIJPCY,0),CL10'CSF'                                      
         DC    AL1(0,0,TACIJPCN),CL10'NOCSF'                                    
         DC    AL1(0,0,TACISPLH),CL10'SPLH'                                     
         DC    AL1(0,0,TACICIHR),CL10'CIHR'                                     
         DC    AL1(0,0,TACISTCA),CL10'CAN'                                      
         DC    AL1(0,0,TACISREG),CL10'REG'                                      
         DC    X'FF'                                                            
*                                                                               
HOLDTAB  DS    0CL11               TABLE OF STATUS CODES FOR HLD FEE            
         DC    AL1(TACIHSPO),CL10'PO'                                           
         DC    AL1(TACIHSEL),CL10'EL'                                           
         DC    AL1(TACIHSPE),CL10'PE'                                           
         DC    AL1(TACIHSNC),CL10'NC'                                           
         DC    X'FF'                                                            
*                                                                               
PFTABLE  DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'CLIENT  ',CL8'LIST    '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'CGROUP  ',CL8'DISPLAY '                               
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PRODUCT ',CL8'LIST    '                               
PF15     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYTWA,L'TGAGY-1),AL2(SCLAGY-T702FFD)                      
         DC    AL1(KEYTYTWA,L'TGCLI-1),AL2(SCLCLI-T702FFD)                      
PF15X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
SVPTRLNQ EQU   (520*L'TLDRREC)+1                                                
UPPTRLNQ EQU   (520*L'TLDRREC)+1                                                
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(CLIENT RECORD)                    *         
***********************************************************************         
                                                                                
BLDMQMSG NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE = WSSVR SAVE STATUS                   
                                                                                
         LA    R2,BLOCK            R2=A(MQ MESSAGE BLOCK)                       
         BAS   RE,BLDIMMSG         BUILD INSERT/MODIFY MESSAGE                  
         JE    BMQ10                                                            
         BAS   RE,BLDDMSG          OR BUILD DELETE MESSAGE                      
         JNE   BMQNO               LENGTH RETURNED IN R3                        
                                                                                
         USING FAWSSVRD,R1                                                      
BMQ10    TM    TGBYTE,X'80'        IF SAVING TO WSSVR                           
         JZ    BMQYES              DO SO NOW                                    
         LA    R1,WORK                                                          
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         ST    R2,FAWSADR                                                       
         STH   R3,FAWSLEN                                                       
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    BMQYES                                                           
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
BMQYES   XR    RC,RC                                                            
BMQNO    LTR   RC,RC                                                            
BMQX     XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING INSERT/MODIFY MESSAGE          *         
*        ON ENTRY ... AIO1 = A(CLIENT RECORD)                         *         
*                     R2   = A(RESERVED MQ MESSAGE BLOCK)             *         
***********************************************************************         
                                                                                
BLDIMMSG NTR1                                                                   
         CLI   MODE,XRECDEL        IF NOT DELETING RECORD                       
         JE    BMQNO                                                            
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO1                                                          
         OC    TLCLAGY,TLCLAGY     AND NOT A GLOBAL CLIENT                      
         JZ    BMQNO                                                            
                                                                                
         USING IMMSGD,R2                                                        
         LHI   R3,IMMLNQ                                                        
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,IMMSG                                                         
         LR    R1,R3               COPY INSERT MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM10                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM20                                                           
BIMM10   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM30                                                           
                                                                                
BIMM20   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
BIMM30   MVC   IMAGY,TLCLAGY       COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMAGY,IMAGY)                                    
         MVC   IMCOD,TLCLCLI       COPY CLIENT CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
         DROP  R4                                                               
                                                                                
         MVI   IMLCK,C'N'                                                       
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              COPY CLIENT NAME INTO XML                    
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BIMM40                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   IMNAM(0),TANANAME                                                
         GOTO1 ELIMCHAR,DMCB,(L'IMNAM,IMNAM)                                    
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
BIMM40   L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BMQYES                                                           
         TM    TACISTAT,TACISLCK   COPY CLIENT LOCKED STATUS INTO XML           
         JZ    BMQYES                                                           
         MVI   IMLCK,C'Y'                                                       
         J     BMQYES                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(CLIENT RECORD)                         *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDDMSG  NTR1                                                                   
         CLI   MODE,XRECDEL        IF DELETING RECORD                           
         JNE   BMQNO                                                            
         CLI   SCLAGYH+5,0         AND NOT A GLOBAL CLIENT                      
         JE    BMQNO                                                            
                                                                                
         USING DMSGD,R2                                                         
         LHI   R3,DMLNQ                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,DMSG                                                          
         LR    R1,R3               COPY DELETE MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   DAGY,SCLAGY         COPY AGENCY CODE INTO XML                    
         GOTO1 ELIMCHAR,DMCB,(L'DAGY,DAGY)                                      
         MVC   DCOD,SCLCLI         COPY ATTENTION CODE INTO XML                 
         GOTO1 ELIMCHAR,DMCB,(L'DCOD,DCOD)                                      
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
BMQTRUE  DC    CL5'true'                                                        
BMQFALSE DC    CL5'false'                                                       
                                                                                
IMMSG    DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    CL11' '                                                          
         DC    C'<agency isinsert="false" ismodify="false" code="'              
         DC    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<client isinsert="'                                            
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL6' '                                                           
         DC    C'" name="'                                                      
         DC    CL36' '                                                          
         DC    C'" locked="'                                                    
         DC    CL1' '                                                           
         DC    C'">'                                                            
         DC    C'</client>'                                                     
         DC    C'</agency>'                                                     
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DS    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<agency code="'                                                
         DS    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'<client isdelete="true" code="'                                
         DS    CL6' '                                                           
         DC    C'">'                                                            
         DC    C'</client>'                                                     
         DC    C'</agency>'                                                     
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(CLIENT RECORD)                         *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BLDMQMSG,DMCB,0     BUILD UPDATED MQ MESSAGE                     
         JNE   XIT                                                              
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV10                                                             
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSARST   RECALL INITIAL-STATE BASED                   
         MVC   FAWSADR,AIO3        MESSAGE INTO AIO3                            
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         LA    RE,BLOCK                                                         
         LR    RF,R3               IF CHANGE HAS BEEN MADE THAT                 
         L     R0,AIO3             WILL AFFECT THE MESSAGE                      
         LR    R1,R3               SEND THE UPDATED MESSAGE                     
         CLCL  RE,R0                                                            
         JE    XIT                                                              
                                                                                
NV10     GOTO1 NTFYVITA,DMCB,BLOCK,(R3),0                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES ATTACHED COMMERCIALS WITH NEW CLIENT GROUP   *         
*        ON ENTRY ... AIO1 = A(CLIENT RECORD)                         *         
***********************************************************************         
                                                                                
UPDCOM   NTR1  BASE=*,LABEL=*                                                   
         CLI   MODE,XRECPUT        IF JUST PUT RECORD ...                       
         JNE   XIT                                                              
                                                                                
         USING TLCLD,R4                                                         
         L     R4,AIO1                                                          
         MVC   TGAGY,TLCLAGY                                                    
         MVC   TGCLI,TLCLCLI                                                    
         DROP  R4                                                               
                                                                                
         USING TACID,R4                                                         
         XC    TGCLG,TGCLG                                                      
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   UCOM10                                                           
         MVC   TGCLG,TACICLG       SET CURRENT CLIENT GROUP                     
         DROP  R4                                                               
                                                                                
UCOM10   CLC   TGCLG,INITCLG       IF CLIENT GROUP WAS CHANGED ...              
         JE    XIT                                                              
                                                                                
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLWTD,R4                                                         
         L     R4,AIO              ADD TRANSACTION RECORD                       
         XC    0(255,R4),0(R4)                                                  
         MVI   TLWTCD,TLWTCDQ                                                   
         MVI   TLWTWBAP,TLWTMFCG                                                
         MVC   TLWTDATE,TGTODAY1                                                
         TIME  DEC                                                              
         STCM  R0,12,TLWTTIME                                                   
         XC    TLWTTIME,=2X'FF'                                                 
         MVC   TLWTCGAY,TGAGY                                                   
         MVC   TLWTCGCL,TGCLI                                                   
         MVC   TLWTCGCG,TGCLG                                                   
         MVC   TLWTLEN,DATADISP                                                 
         GOTO1 ADDREC                                                           
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR14D                                                       
         SPACE 3                                                                
         ORG   SCLWORK                                                          
*                                                                               
*          DATA SET TAGENFC    AT LEVEL 005 AS OF 04/14/05                      
ACTBL    DS    A                                                                
ACTBLX   DS    A                                                                
*                                                                               
AADDTBL  DS    A                                                                
AADDTBLX DS    A                                                                
*                                                                               
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPTRS  DS    A                   A(UPDATED POINTER BLOCK)                     
*                                                                               
INITCLG  DS    CL(L'SCLGCL)        INITIAL CLIENT GROUP                         
SVSTKEY  DS    XL(L'KEY)           SAVED STAFF KEY                              
LASTKEY  DS    XL(L'TLSTKEY)       LAST PROCESSED STAFF RECORD KEY              
*                                                                               
RECSTAT  DS    XL1                                                              
ADDINGR  EQU   C'A'                                                             
PUTTINGR EQU   C'P'                                                             
*                                                                               
BS2LSSEQ DS    XL1                 WSSVR BLOCK SEQUENCE NUMBER                  
BS2LWBLK DS    XL50                WSSVR BLOCK AREA                             
*                                                                               
MYFULL   DS    F                                                                
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY + DISK ADDR                        
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVOFF    DS    CL1                 SAVED OFFICE                                 
STATBYTS DS    0CL3                STATUS BYTES FOR BOTH BR AND CI ELS          
STATBR   DS    C                   STATUS BYTE FOR BILLING RULES ELEM           
STATCI   DS    C                   STATUS BYTE FOR CLIENT INFO ELEM             
STATCI2  DS    C                   STATUS BYTE 2 FOR CLIENT INFO ELEM           
OEORBR   DS    CL3                 OVERRIDE EMPLOYER OF RECORD                  
SVAGSTAT DS    XL1                 SAVED AGENCY STATUS                          
SVAGG    DS    CL6                 SAVED AGENCY GROUP                           
SVHLDS   DS    XL1                 SAVED AGENCY HOLDING FEE STATUS              
SVAGSTA6 DS    XL1                 SAVED AGENCY STATUS 6                        
SVAGSTA7 DS    XL1                 SAVED AGENCY STATUS 7                        
SVBTYPE  DS    XL1                 SAVED BILLING TYPE                           
TARAFLAG DS    XL1                                                              
TARAFYES EQU   X'80'               CLIENT HAS BRATE                             
SVGRUL   DS    XL1                 BRATE APPL GRT                               
SVCISTA2 DS    X                   SAVE CLIENT STATUS BYTE 2                    
*                                                                               
SVSTPTR  DS    XL(6*38+1)          STAFF POINTERS                               
*                                                                               
MYELEM   DS    XL(L'ELEMENT)                                                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 3                                                                
*                                                                               
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* FAWSSVRD                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*              DSECT FOR AGENCY/CLIENT TABLE                                    
ACTBLD   DSECT                                                                  
ACSTAT   DS    X                                                                
ACAGY    DS    CL6                                                              
ACCLI    DS    CL6                                                              
ACLNQ    EQU   *-ACTBLD                                                         
ACTBLNQ  EQU   (ACLNQ*1500)+1                                                   
TBLLNQ   EQU   ACTBLNQ+ACLNQ+1                                                  
*                                                                               
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL48                                                             
IMAGY    DS    CL6                                                              
         DS    CL2                                                              
         DS    CL18                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMCOD    DS    CL6                                                              
         DS    CL8                                                              
IMNAM    DS    CL36                                                             
         DS    CL10                                                             
IMLCK    DS    CL1                                                              
         DS    CL2                                                              
         DS    CL9                                                              
         DS    CL9                                                              
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL14                                                             
DAGY     DS    CL6                                                              
         DS    CL2                                                              
         DS    CL30                                                             
DCOD     DS    CL6                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115TAGEN14   04/08/14'                                      
         END                                                                    
