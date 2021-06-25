*          DATA SET GEKWX00T   AT LEVEL 043 AS OF 05/01/02                      
*PHASE TF2000A,*                                                                
         TITLE '$KWX MK3 (TF2000) - GEKWX00 - ROOT PHASE'                       
*********************************************************************           
* HISTORY OF CHANGES                                                *           
* 14JUL95 (BU/SKUI) - 'HARD-CODE' REP3 TEMPORARILY                  *           
*                                                                   *           
* 17JAN96 (SKU) --- 'HARD-CODE' FOR TTVNY (NOT AGAIN)               *           
*                                                                   *           
* 26FEB96 (SKU) --- 'HARD-CODE' FOR SELNY (NOT AGAIN)               *           
*                                                                   *           
* 21MAR96 (SKU) --- 'HARD-CODE' FOR PETNY (NOT FUNNY ANYMORE)       *           
*                                                                   *           
* 27MAR96 (SKU) --- 'HARD-CODE' FOR KRGNY (#$%@^&*&^%$#@!##$)       *           
*                                                                   *           
* 05APR96 (SKU) --- 'HARD-CODE' FOR IRNY  (REDRUM REDRUM REDRUM)    *           
*                                                                   *           
* 24MAY96 (SKU) --- INCREASE DESTIDS FROM 1000 TO 3000 BYTES        *           
*                     ***  END TOMBSTONE  ***                       *           
*********************************************************************           
         PRINT NOGEN                                                            
KWX00    CSECT                                                                  
         NMOD1 GWSEND-GWS,**KWX00*,R9,R8,RR=R5,CLEAR=YES                        
         USING KWX00,RB,R9,R8                                                   
         USING GWS,RC                                                           
         ST    R5,RELO                                                          
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVI   PASAVE,TRMINATE                                                  
         MVI   PBSAVE,TRMINATE                                                  
*                                                                               
         ST    RB,ABASE                                                         
         ST    R9,A2NDBASE                                                      
         ST    R8,A3RDBASE                                                      
         ST    RC,AGWS                                                          
         ST    RD,AREGSAVE                                                      
         ST    R1,APARM                                                         
*                                                                               
         MVC   ATWALIST(20),0(R1)                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         MVC   KWXHEAD,SPACES                                                   
         OI    KWXHEADH+6,X'80'                                                 
         MVC   KWXHELP,SPACES                                                   
         OI    KWXHELPH+6,X'80'                                                 
         LA    R1,TWAEND                                                        
         ST    R1,AENDTWA                                                       
         LA    R1,BUFFSAVE-3-9                                                  
         ST    R1,AENDSCR                                                       
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATVAL,CDATVAL                                                  
         MVC   ADATCON,CDATCON                                                  
         MVC   ACASHVAL,CCASHVAL                                                
         MVC   AGETFACT,CGETFACT                                                
*                                                                               
         L     R1,=AL4(DESTIDS-GWS)                                             
         AR    R1,RC                                                            
         ST    R1,ADESTIDS                                                      
         L     R1,=AL4(DESTIDS+3000-GWS)                                        
         AR    R1,RC                                                            
         ST    R1,ADIDMAX                                                       
         L     R1,=AL4(IOB-GWS)                                                 
         AR    R1,RC                                                            
         ST    R1,AIOB                                                          
         L     R1,=AL4(BUFFER-GWS)                                              
         AR    R1,RC                                                            
         ST    R1,ABUFFER                                                       
*                                                                               
         LA    R1,COMMON                                                        
         LA    R2,ACOMMON                                                       
         LA    R3,ACOMMONX-ACOMMON                                              
         SRL   R3,2                                                             
T002     L     R4,0(R1)                                                         
         A     R4,RELO                                                          
         ST    R4,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,T002                                                          
         LA    R2,KWXINITH                                                      
         ST    R2,ACURSOR                                                       
         DROP  R1                                                               
*                                                                               
         GOTO1 AGETFACT,PARAS,0                                                 
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         LA    RE,SYSTAB                                                        
T003     CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SYSTEM NOT RECOGNIZED                        
         CLC   FAOVSYS,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'SYSTAB(RE)                                                  
         B     T003                                                             
         MVC   WKFILE,1(RE)        SAVE WORK FILE NAME FOR SYSTEM               
         B     T021                                                             
*                                                                               
SYSTAB   DS    0CL9                                                             
*&&UK*&& DC    X'06',CL8'ACCWRK'                                                
*&&US*&& DC    X'08',CL8'REPWRK'                                                
*&&US*&& DC    X'0A',CL8'CTWORK'                                                
         DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*              FIRST TIME IN, READ ID RECORD                                    
*                                                                               
T021     OC    SAVTKWID,SAVTKWID                                                
         BNZ   T030                                                             
         MVC   SAVTKWID,TWAUSRID                                                
T022     LA    R7,KEY                                                           
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAUSRID                                             
         MVI   FERN,INVALID                                                     
         BAS   RE,READ                                                          
         BZ    ERROR                                                            
         LA    R6,CTIDATA                                                       
         SR    R5,R5                                                            
T023     CLI   0(R6),0                                                          
         BE    ERROR                                                            
         CLI   0(R6),X'03'                                                      
         BE    T025                                                             
         CLI   0(R6),X'06'                                                      
         BE    T025A                                                            
         CLI   0(R6),X'30'                                                      
         BE    T026                                                             
T024     IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     T023                                                             
         USING CTPASD,R6                                                        
T025     CLI   CTPASLEN,4                                                       
         BNE   T024                                                             
         MVC   SAVTKWID,CTPASDTA   SPECIAL KWX SOURCE ID FOR LISTS              
         B     T024                                                             
         USING CTAGYD,R6                                                        
T025A    MVC   SAVTAGID,CTAGYID                                                 
         B     T024                                                             
         USING CTDSTD,R6                                                        
T026     MVC   SAVTPOW,CTDSTPOW+2  SENDER'S OFFICE                              
         DROP  R6,R7                                                            
*                                                                               
T028     GOTO1 AGETFACT,PARAS,0    SAVE TERMINAL TYPE                           
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   TRMTYPE,FATTYPE                                                  
         CLI   TRMTYPE,0                                                        
         BNE   T030                                                             
         MVI   TRMTYPE,T3270                                                    
         TM    FATSTAT,X'10'                                                    
         BO    T030                                                             
         MVI   TRMTYPE,T2260                                                    
         B     T030                                                             
         DROP  RF                                                               
         EJECT                                                                  
*              VALIDATE INITIALS                                                
*                                                                               
T030     LA    R2,KWXINITH                                                      
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BZ    ERROR                                                            
         CLI   5(R2),3             3 INITIALS REQUIRED                          
         MVI   FERN,INVINIT                                                     
         BL    ERROR                                                            
         OC    8(5,R2),SPACES                                                   
         CLC   LASTINIT,8(R2)                                                   
         BE    T032                                                             
         XC    SAVEREST,SAVEREST                                                
         XC    KWXBOOK,KWXBOOK                                                  
         OI    KWXBOOKH+6,X'80'                                                 
         LA    R0,3                CHECK FOR AT LEAST 3 ALPHA                   
         LA    R1,8(R2)                                                         
T031     CLI   0(R1),C'A'                                                       
         BL    ERROR                                                            
         CLI   0(R1),C'Z'                                                       
         BH    ERROR                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,T031                                                          
         MVC   LASTINIT,8(R2)                                                   
T032     CLI   SAVMODE,0           SET DEFAULT MODE                             
         BNE   T035                                                             
         MVI   SAVMODE,MESSAGE                                                  
         B     T035                                                             
         EJECT                                                                  
*              VALIDATE ACTION                                                  
*                                                                               
T035     MVI   FNDX,0              SCAN ACTION FIELD                            
         MVI   FERN,MISSING                                                     
         LA    R2,KWXACTH                                                       
         ST    R2,ACURSOR                                                       
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R0,L'OLD            LONGEST PERMITTED PARAMETER VALUE            
         GOTO1 ASCANNER,DMCB,((R0),(R2)),(16,SCANBLCK)                          
         CLI   DMCB+4,0                                                         
         MVI   FERN,INVALID                                                     
         BE    ERROR                                                            
*                                                                               
T0366    LA    RF,ACTAB            VALIDATE ACTION VERB AGAINST ACTAB           
         USING ACTIOND,RF                                                       
         SR    R0,R0                                                            
         ZIC   R1,SCANBLCK                                                      
         BCTR  R1,0                                                             
         MVI   FNDX,1                                                           
         CLI   SCANBLCK,2                                                       
         BL    ERROR                                                            
*                                                                               
T037     CLI   0(RF),0             LOOP FOR TABLE ENTRY                         
         BE    ERROR               END OF TABLE                                 
         CLI   SCANBLCK,2          FIND A MATCHING 2-CHAR VERB                  
         BH    T039                                                             
         CLC   ACTNSHRT,SCANBLCK+12                                             
         BE    T040                                                             
T038     IC    R0,ACTNLEN                                                       
         AR    RF,R0                                                            
         B     T037                                                             
T039     EX    R1,*+8              OR IF MORE THAN 2-CHARS COMPARE FULL         
         B     *+10                                                             
         CLC   ACTNFULL(0),SCANBLCK+12                                          
         BNE   T038                                                             
*                                                                               
T040     MVC   BYTE,SAVMODE        FOUND A MATCH - IS MODE COMPATIBLE           
         NC    BYTE,ACTNSTAT                                                    
         BNZ   T041                                                             
         MVI   FERN,INVMODE                                                     
         B     T038                BUMP IF NOT                                  
*                                                                               
T041     TM    ACTNSTAT,DDSONLY    DDS OR PRIVILEGED USER                       
         BZ    T042                                                             
         TM    TWAAUTH,X'80'                                                    
         BNO   T038                                                             
*                                                                               
T042     TM    ACTNSTAT,BOOK       IF ACTION REQUIRES A BOOK TO HAVE            
         BZ    T044                BEEN NOMINATED, CHECK FOR ONE                
         MVI   FERN,NOBOOK                                                      
         LA    RE,FRMBOOK                                                       
         CLI   SAVMODE,FORMAT                                                   
         BE    *+8                                                              
         LA    RE,MSGBOOK                                                       
         OC    0(L'FRMBOOK,RE),0(RE)                                            
         BZ    ERROR                                                            
*                                                                               
T044     TM    ACTNSTAT,WRITACC    IF ACTION REQUIRES WRITE ACCESS              
         BZ    T046                CHECK THAT USER HAS IT                       
         MVI   FERN,NOTAUTH                                                     
         TM    SAVACCS,WRITACC                                                  
         BNO   ERROR                                                            
*                                                                               
T046     MVC   ACTION,ACTNNUM      MOVE ACTION DETAILS TO W/S                   
         MVC   ACTSTAT,ACTNSTAT                                                 
         MVC   OVERLAY,ACTNOV                                                   
         ST    RF,AACTNTRY                                                      
         DROP  RF                                                               
*                                                                               
T048     XC    ALLPARM,ALLPARM     NOW CHECK PARAMETERS                         
         CLI   SCANBLCK+1,0        IF ACTION VERB HAS RHS START WITH IT         
         BE    T050                                                             
         LA    R6,SCANBLCK                                                      
         MVI   FNDX,1                                                           
         B     T052                                                             
*                                                                               
T050     DS    0H                  OTHERWISE START AFTER IT                     
         LA    R6,SCANBLCK+22+L'OLD                                             
         MVI   FNDX,2                                                           
*                                                                               
T052     OC    0(2,R6),0(R6)       LOOP FOR A PARAMETER                         
         BZ    T070                                                             
         MVI   FERN,INVALID                                                     
         LA    R5,PARMTAB                                                       
         USING PARMD,R5                                                         
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    SYNTAXER                                                         
         SR    R0,R0                                                            
*                                                                               
T054     CLI   PARMLEN,0           FIND A MATCHING PARAMETER TABLE NTRY         
         BNE   T054A                                                            
         TM    12(R6),X'F0'        IF NOT FOUND AND PARAM STARTS NUMERC         
         BNO   SYNTAXER            SET IT AS R(EF)=VALUE AND TRY AGAIN          
         MVC   1(1,R6),0(R6)                                                    
         MVC   22(10,R6),12(R6)                                                 
         MVI   0(R6),3                                                          
         MVC   12(3,R6),=C'REF'                                                 
         B     T052                                                             
T054A    EX    R1,T050CLC                                                       
         BE    T056                                                             
T055     IC    R0,PARMLEN                                                       
         AR    R5,R0                                                            
         B     T054                                                             
T050CLC  CLC   PARMWORD(0),12(R6)                                               
*                                                                               
T056     LA    RF,PARMACTS         WITH COMPATIBLE ACTION                       
T057     CLI   0(RF),0                                                          
         BE    T055                                                             
         CLC   ACTION,0(RF)                                                     
         BE    T058                                                             
         LA    RF,1(RF)                                                         
         B     T057                                                             
*                                                                               
T058     SR    R4,R4               TEST FOR DUPLICATE                           
         ICM   R4,3,PARMDEST                                                    
         AR    R4,RC               R4 = A(FNDX+OUTPUT VALUE OF PARAM)           
         BCTR  R4,0                                                             
         CLI   0(R4),0                                                          
         MVI   FERN,DUPLICAT                                                    
         BNE   ERROR                                                            
         MVC   0(1,R4),FNDX                                                     
         LA    R4,1(R4)                                                         
*                                                                               
T060     CLI   PARMSR,0            CALL VALIDATE/CONVERT SR IF ANY              
         BNE   T062                                                             
         MVI   FERN,MISSING                                                     
         CLI   1(R6),0                                                          
         BE    SYNTAXER                                                         
         MVI   FERN,INVALID                                                     
         L     RF,PARMSR                                                        
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R6),(R4)                                              
         BZ    SYNTAXER                                                         
         B     T066                                                             
*                                                                               
T062     CLI   PARMSR,C'Y'         OR MOVE IN C'Y' TO OUTPUT VALUE              
         BNE   T064                                                             
         MVI   0(R4),C'Y'                                                       
         B     T066                                                             
*                                                                               
T064     ZIC   R1,PARMSR           OR MOVE INPUT TO OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     T066                                                             
         MVC   0(0,R4),22(R6)                                                   
*                                                                               
T066     ZIC   R1,FNDX             BUMP TO NEXT SCANNER TABLE ENTRY             
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,22+L'OLD(R6)                                                  
         B     T052                                                             
*                                                                               
SYNTAXER L     RF,AACTNTRY         IF SYNTAX ERROR DISPLAY ACTNFULL ON          
         USING ACTIOND,RF          LINE 3                                       
         ZIC   R1,ACTNLEN                                                       
         LA    R0,ACTNFULL-ACTIOND                                              
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KWXHELP(0),ACTNFULL                                              
         OI    KWXHELPH+6,X'80'                                                 
         B     ERROR                                                            
         DROP  RF                                                               
         EJECT                                                                  
*              GET HEADER RECORD, CALL OVERLAY FOR ACTION AND HANDLE            
*              RETURN                                                           
*                                                                               
T070     MVI   FNDX,0              RESTORE SAVED BUFFER VALS & READ HDR         
         TM    ACTSTAT,BOOK                                                     
         BZ    T075                                                             
         GOTO1 RESTORE,PARAS,(SAVMODE,0)                                        
         BZ    ERROR                                                            
*                                                                               
T075     DS    0H                  CALL OVERLAY                                 
         GOTO1 ACALLOV,PARAS,(OVERLAY,0),0                                      
         MVI   FERN,UNAVAIL                                                     
         CLI   PARAS+4,X'FF'                                                    
         BE    ERROR                                                            
         MVI   FERN,0                                                           
         L     RF,PARAS                                                         
         GOTO1 (RF),PARAS,(RC)     OVERLAY RETURNS                              
         BNZ   T076                CC=EQU IF ERROR (FERN,FNDX,SUBFNDX,          
         CLI   SYNTAX,C'Y'            SYNTAX AND ACURSOR SET)                   
         BE    SYNTAXER            CC=NEQ OTHERWISE (KWXHEAD,ACURSOR            
         B     ERROR                  SET)                                      
T076     BM    END1                CC=NEG IF MORE TO COME                       
         CLI   ACTION,ASE          ADDSEND BECOMES SEND                         
         BNE   END1                                                             
         MVI   ACTION,SEN                                                       
         MVI   OVERLAY,10                                                       
         B     T075                                                             
*                                                                               
END1     GOTO1 CLOSE,PARAS,(SAVMODE,0)  CLOSE/SAVE LIBRARY                      
         BZ    ERROR                                                            
*                                                                               
END2     OC    ATWADIFF,ATWADIFF   CHECK FOR SCREEN FORMAT CHANGE               
         BZ    END3                                                             
         L     R3,ATWADIFF                                                      
         LA    R1,KWXHEADH                                                      
         SR    R2,R2                                                            
         TM    TRMTYPE,T3270       IF NOT 3270 RETRANSMIT                       
         BZ    END2A               IF IT IS 3270, OPTIMISE TRANSMISSION         
         CLI   0(R3),0             IF CHANGE IS JUST THAT NEW SCREEN            
         BNE   END2A                IS SHORTER                                  
         MVC   1(2,R3),=X'0001'    CLEAR AFTER                                  
         BCTR  R3,0                                                             
         IC    R2,0(R1)                                                         
         BXLE  R1,R2,*-4                                                        
         SR    R1,R2                                                            
         OI    6(R1),X'80'         & RETRANSMIT LAST FLD                        
         B     END3                                                             
END2A    CLI   0(R1),0             OTHERWISE RETRANSMIT & CLEAR BEFORE          
         BE    END2B                                                            
         OI    6(R1),X'80'                                                      
         MVI   7(R1),1             (2260 O/P TRANSLATOR QUIRK)                  
         IC    R2,0(R1)                                                         
         AR    R1,R2                                                            
         B     END2A                                                            
END2B    MVC   1(2,R1),=X'0100'                                                 
*                                                                               
END3     EQU   *                                                                
         L     R2,ACURSOR          SET CURSOR TO NEXT UNP                       
         CR    R2,RA                                                            
         BNL   END3A                                                            
         DC    H'0'                                                             
END3A    EQU   *                                                                
         SR    R1,R1                                                            
END4     MVC   BYTE,1(R2)                                                       
         OC    BYTE,6(R2)                                                       
         TM    BYTE,X'20'                                                       
         BZ    *+14                                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     END4                                                             
         OI    6(R2),X'C0'                                                      
         B     EXIT                                                             
*                                                                               
ERRXIT   SR    RB,RB                                                            
OKXIT    LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*              ERROR HANDLING (VIA OVERLAY BECAUSE OF NO.OF SPECIAL             
*              MESSAGES)                                                        
*                                                                               
ERROR    CLI   FERN,X'FE'                                                       
         BE    END1                MESSAGE ALREADY SET                          
         GOTO1 ACALLOV,PARAS,(09,0),0                                           
         CLI   PARAS+4,X'FF'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,PARAS                                                         
         GOTO1 (RF),PARAS,(RC)                                                  
         B     END1                                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE & CONVERT A BOOKNAME IN AN ID= PARAM         
*                                                                               
CHKID    NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(SCANNER ENTRY), R3=A(OUTPUT)            
         ZIC   R4,1(R2)            R4=LENGTH OF INPUT                           
         LA    R7,22(R2)           R7=INPUT POINTER                             
         MVI   FERN,INVALID                                                     
         TM    3(R2),X'80'         ID=0 MEANS DEFAULT                           
         BZ    *+14                                                             
         OC    8(4,R2),8(R2)                                                    
         BZ    OKXIT                                                            
         MVC   0(2,R3),TWAUSRID    DEFAULTS TO THIS USER IF NOT GIVEN           
         MVC   2(4,R3),LASTINIT    AND TO 1ST 4 CHS OF INITIALS                 
         MVI   6(R3),X'01'                                                      
         CLI   12(R2),C'B'                                                      
         BNE   *+12                                                             
         CLI   SAVMODE,MESSAGE                                                  
         BE    CHKI1                                                            
         MVC   0(2,R3),SAVTKWID    PRINCIPAL USER OR ..                         
*&&US                                                                           
*                                                                               
* REPX USER POINTS TO TCHX USER                                                 
*                                                                               
         MVC   0(2,R3),=H'43'      TCH1 ID (UGH)                                
*                                                                               
         CLC   =H'237',TWAUSRID    TCH3 USER?                                   
         BE    CHKI0020            YES                                          
         CLC   =H'4174',TWAUSRID   BIL USER?   REP3                             
         BE    CHKI0020            YES                                          
*                                                                               
* POINT KRGNY REPS TO TCH3                                                      
         CLC   =H'4379',TWAUSRID   KRGNY USER?                                  
         BE    CHKI0020            YES                                          
         CLC   =H'4280',TWAUSRID   BANNY USER?                                  
         BE    CHKI0020            YES                                          
         CLC   =H'4416',TWAUSRID   SYNNY USER?                                  
         BE    CHKI0020            YES                                          
         CLC   =H'4296',TWAUSRID   KRNY USER?                                   
         BE    CHKI0020            YES                                          
         CLC   =H'4307',TWAUSRID   KHNY USER?                                   
         BE    CHKI0020            YES                                          
         CLC   =H'4327',TWAUSRID   EASTNY USER?                                 
         BE    CHKI0020            YES                                          
         CLC   =H'4342',TWAUSRID   CHRNY USER?                                  
         BE    CHKI0020            YES                                          
         CLC   =H'4388',TWAUSRID   NOTNY USER?                                  
         BE    CHKI0020            YES                                          
         CLC   =H'4750',TWAUSRID   SENNY USER?                                  
         BE    CHKI0020            YES                                          
*                                                                               
         CLC   =H'262',TWAUSRID    TCH4 USER?                                   
         BE    CHKI0023            YES                                          
         CLC   =H'2004',TWAUSRID   IRNY USER?                                   
         BE    CHKI0023            YES                                          
         CLC   =H'1128',TWAUSRID   TORNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'1866',TWAUSRID   MMRNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'2002',TWAUSRID   GPNY USER?                                   
         BE    CHKI0023            YES                                          
         CLC   =H'2003',TWAUSRID   MGNY USER?                                   
         BE    CHKI0023            YES                                          
         CLC   =H'2487',TWAUSRID   CABNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'2537',TWAUSRID   SRSNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'2298',TWAUSRID   NONNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'3233',TWAUSRID   DARNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'3618',TWAUSRID   INFNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'3894',TWAUSRID   SHANY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'4130',TWAUSRID   CMBNY USER?                                  
         BE    CHKI0023            YES                                          
         CLC   =H'4947',TWAUSRID   CCRNY USER?                                  
         BE    CHKI0023            YES                                          
*                                                                               
         CLC   =H'316',TWAUSRID    TCH5 USER?                                   
         BE    CHKI0025            YES                                          
         CLC   =H'4478',TWAUSRID   TTVNY USER? REP7                             
         BE    CHKI0030            YES                                          
         CLC   =C'TV',TWAAGY       OR STATIONS WITH TV ALPHA ID                 
         BE    CHKI0030            YES                                          
*                                                                               
         CLC   =H'574',TWAUSRID    TCH7 USER?                                   
         BE    CHKI0030            YES                                          
         CLC   =H'4147',TWAUSRID   UTSNY USER? REP5                             
         BE    CHKI0030            YES                                          
         CLC   =C'UT',TWAAGY       OR STATIONS WITH UT ALPHA ID                 
         BE    CHKI0030            YES                                          
         CLC   =H'3498',TWAUSRID   SELNY USER? REP7                             
         BE    CHKI0030            YES                                          
         CLC   =H'4580',TWAUSRID   INTNY USER? REP7                             
         BE    CHKI0030            YES                                          
         CLC   =C'SZ',TWAAGY       OR STATIONS WITH SZ ALPHA ID                 
         BE    CHKI0030            YES                                          
         CLC   =C'S2',TWAAGY       OR STATIONS WITH S2 ALPHA ID                 
         BE    CHKI0030            YES                                          
*                                                                               
         CLC   =H'1288',TWAUSRID   TCH8 USER?                                   
         BE    CHKI0035            YES                                          
         CLC   =H'3500',TWAUSRID   PETNY USER? REP8                             
         BE    CHKI0035            YES                                          
         CLC   =C'PV',TWAAGY       OR STATIONS WITH PV ALPHA ID                 
         BE    CHKI0035            YES                                          
         B     CHKI0040            YES                                          
*                                                                               
CHKI0020 EQU   *                                                                
         MVC   0(2,R3),=H'237'     TCH3 ID (UGH?  )                             
         B     CHKI0040                                                         
*                                                                               
CHKI0023 EQU   *                                                                
         MVC   0(2,R3),=H'262'     TCH4 ID (UGH?  )                             
         B     CHKI0040                                                         
*                                                                               
CHKI0025 EQU   *                                                                
         MVC   0(2,R3),=H'316'     TCH5 ID (UGH?  )                             
         B     CHKI0040                                                         
*                                                                               
CHKI0030 EQU   *                                                                
         MVC   0(2,R3),=H'574'     TCH7 ID (UGH?  )                             
         B     CHKI0040                                                         
*                                                                               
CHKI0035 EQU   *                                                                
         MVC   0(2,R3),=H'1288'    TCH8 ID (UGH?  )                             
*                                                                               
CHKI0040 EQU   *                                                                
*&&                                                                             
         MVC   2(4,R3),=C'KZZZ'    PLUS K & INITIALS ZZZ                        
*                                                                               
CHKI1    CLI   1(R2),7                                                          
         BNH   CHKI4                                                            
*                                                                               
CHKI2    LR    RF,R4               FORMAT IS USER.IIIINNN                       
         SH    RF,=H'2'                                                         
         LA    RE,1(R7,RF)                                                      
         CLI   0(RE),C'.'                                                       
         BE    *+12                                                             
         BCT   RF,*-12                                                          
         B     ERRXIT                                                           
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),0(R7)                                                  
         BAS   RE,READ                                                          
         BZ    ERRXIT                                                           
         SR    R5,R5                                                            
         LA    R6,CTIDATA                                                       
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     *-14                                                             
         USING CTDSCD,R6                                                        
         MVC   0(2,R3),CTDSC                                                    
         LA    RF,2(RF)            ADJUST LENGTH & POINTER                      
         AR    R7,RF                                                            
         SR    R4,RF                                                            
         BZ    ERRXIT                                                           
         DROP  R6                                                               
*                                                                               
CHKI4    TM    0(R7),X'F0'         HANDLE INITIALS                              
         BO    CHKI6               (MUST START ALPHA)                           
         CLI   12(R2),C'B'                                                      
         BNE   CHKI5                                                            
         CLI   SAVMODE,MESSAGE                                                  
         BNE   CHKI5                                                            
         MVC   2(4,R3),0(R7)                                                    
         LA    RE,4                                                             
         B     *+14                                                             
CHKI5    MVC   3(3,R3),0(R7)                                                    
         LA    RE,3                                                             
         AR    R7,RE                                                            
         SR    R4,RE                                                            
         BZ    OKXIT                                                            
*                                                                               
CHKI6    CH    R4,=H'3'            HANDLE BOOK NUMBER                           
         BH    ERROR                                                            
         MVC   WORK(3),=4C'0'                                                   
         SH    R4,=H'1'                                                         
         BM    OKXIT                                                            
         EX    R4,MVZIT                                                         
         CLC   WORK(3),=4C'0'                                                   
         BNE   ERRXIT                                                           
         LA    R4,1(R4)                                                         
         LA    RF,0(R4,R7)                                                      
         MVI   0(RF),C'0'                                                       
         EX    R4,PACKIT                                                        
         MVC   6(1,R3),DUB+6       PACKED WITHOUT SIGN                          
         MVI   0(RF),C' '                                                       
         CLI   DUB+5,0             IF NUMBER > 99, LEADING DIGIT GOES           
         BE    OKXIT               IN HIGH-ORDER NIBBLE OF CLASS BYTE           
         PACK  DUB+5(1),DUB+5(1)                                                
         MVC   7(1,R3),DUB+5                                                    
         B     OKXIT                                                            
MVZIT    MVZ   WORK(0),0(R7)                                                    
PACKIT   PACK  DUB,0(0,R7)                                                      
         EJECT                                                                  
*              ROUTINE TO VALIDATE AND CONVERT 'REF=N(-M)'                      
*                                                                               
CHKREF   NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(SCANNER ENTRY), R3=A(OUTPUT)            
*                                                                               
CHKR1    LA    R7,22(R2)           SEARCH FOR '-'                               
         LA    R6,1                                                             
         LNR   R6,R6                                                            
         ZIC   RF,1(R2)                                                         
         LA    R5,0(RF,R7)                                                      
         CLI   0(R5),C'-'                                                       
         BE    *+12                                                             
         BXH   R5,R6,*-8                                                        
         B     CHKR2                                                            
         LR    R4,R5                                                            
         SR    R4,R7               R4=L'N'                                      
         SR    RF,R4                                                            
         BCTR  RF,0                RF=L'M                                       
         LA    R7,1(R5)            R7=A(M)                                      
         LA    R3,2(R3)            R3=A(OUTPUT M)                               
         BAS   RE,TOBINARY         CONVERT/STORE M                              
         LR    RF,R4               SAME FOR N                                   
         LA    R7,22(R2)                                                        
         SH    R3,=H'2'                                                         
CHKR2    LA    RE,CHKR4                                                         
TOBINARY CH    RF,=H'3'            R7=A(SOURCE),R3=A(OUTPUT),RF=LENGTH          
         MVI   FERN,NOVERMAX       1-999 NUMERIC                                
         BH    ERRXIT                                                           
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         MVI   FERN,INVALID                                                     
         BM    ERRXIT                                                           
         MVC   WORK(3),=4C'0'                                                   
         EX    RF,MVZIT                                                         
         CLC   WORK(3),=4C'0'                                                   
         MVI   FERN,NONUMERC                                                    
         BNE   ERRXIT                                                           
         EX    RF,PACKIT                                                        
         CVB   R0,DUB                                                           
         STH   R0,0(R3)                                                         
         BR    RE                                                               
*                                                                               
CHKR4    OC    2(2,R3),2(R3)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R3),0(R3)                                                    
         CLC   0(2,R3),2(R3)                                                    
         MVI   FERN,STGTREND                                                    
         BH    ERRXIT                                                           
         LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE AND CONVERT 'NUM=N'                          
*                                                                               
CHKNUM   NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(SCANNER ENTRY), R3=A(OUTPUT)            
         MVI   FERN,INVNUM         NUMERIC IN RANGE 1-9999                      
         TM    3(R2),X'80'                                                      
         BZ    ERRXIT                                                           
         CLC   8(4,R2),=F'3000'                                                 
         MVI   FERN,NOVERMAX                                                    
         BH    ERRXIT                                                           
         OC    8(4,R2),8(R2)                                                    
         MVI   FERN,INVALID                                                     
         BZ    ERRXIT                                                           
         MVC   0(2,R3),10(R2)                                                   
         B     EXIT                                                             
         SPACE 3                                                                
*              ROUTINE TO PRE-PROCESS A REPLACE CHAR STRING (OLD=XXX)           
*                                                                               
CHKOLD   LM    R2,R3,0(R1)         R2=A(SCANNER ENTRY), R3=A(OUTPUT)            
         MVC   0(1,R3),1(R2)       MOVE LEN(1) + VALUE(40) TO OUT               
         MVC   1(40,R3),22(R2)                                                  
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO SAVE USER STRING PARAMETERS PLUS LENGTH               
*              ON ENTRY P1 = A(SCANNER TABLE ENTRY)                             
*                       P2 = A(FIELD TO CONTAIN L+CONTENT OF STRING)            
*              ON EXIT  R6 = A(LAST SCANNER ENTRY FOR STRING)                   
*                                                                               
CHKADDS  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(SCANNER ENTRY), R3=A(FIELD)             
         MVI   FERN,INVALID                                                     
*                                                                               
CHKA1    SR    R4,R4               CONVERT SCANNER ENTRIES BACK TO A            
         MVI   BYTE,0              SIMPLE STRING EG                             
         ZIC   R7,1(R2)            A(3,NAME,NAME)+B(2,NAME)+C-D                 
         LA    R5,22(R2)           BY IGNORING COMMAS WITHIN BRACKETS           
         LA    R6,1                WHEN SEARCHING FOR END OF STRING             
         LA    RF,1(R3)                                                         
*                                                                               
CHKA3    AR    R4,R7               R4=L'STRING, R5/6/7=BXLE REGS                
         AR    R7,R5               RF=OUT POINTER, BYTE= BRACKET FLAG           
         BCTR  R7,0                                                             
*                                                                               
CHKA4    MVC   0(1,RF),0(R5)                                                    
         CLI   0(R5),C'<'                                                       
         BNE   *+8                                                              
         OI    BYTE,1                                                           
         CLI   0(R5),C'>'                                                       
         BNE   *+8                                                              
         XI    BYTE,1                                                           
         LA    RF,1(RF)                                                         
         BXLE  R5,R6,CHKA4                                                      
*                                                                               
         CLI   BYTE,0              AT END OF SCANNER ENTRY GO ON TO             
         BE    CHKA6               NEXT IF IN BRACKETS                          
         LA    R2,L'OLD+22(R2)                                                  
         ZIC   R7,0(R2)                                                         
         LTR   R7,R7                                                            
         MVI   FERN,NOCBRACK                                                    
         BZ    ERRXIT                                                           
         MVI   0(RF),C','          RE-INSERTING A COMMA                         
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,12(R2)                                                        
         B     CHKA3                                                            
*                                                                               
CHKA6    STC   R4,0(R3)                                                         
         LR    R6,R2               RETURN A(LAST SCAN ENTRY FOR STRING)         
         LTR   RB,RB                                                            
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
         SPACE 3                                                                
*              ROUTINE TO VALIDATE AND CONVERT 'ACCESS=WRITE/READ/NONE'         
*                                                                               
CHKACC   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,1(R2)                                                         
         BCTR  R4,0                                                             
         LA    RF,ACCTYPES                                                      
CHKAC2   CLI   0(RF),0                                                          
         BE    ERRXIT                                                           
         EX    R4,ACCLC                                                         
         BE    CHKAC3                                                           
         LA    RF,L'ACCTYPES(RF)                                                
         B     CHKAC2                                                           
ACCLC    CLC   22(0,R2),0(RF)                                                   
CHKAC3   MVC   0(1,R3),22(R2)                                                   
         LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
ACCTYPES DS    0CL5                                                             
         DC    C'WRITEREAD NONE '                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE TO SET UP PARAMETERS FOR FORMAT OR MESSAGE BOOK          
*              LIBRARY FUNCTIONS IN DMCB(5F) AND,IF NECESSARY, PREPARE          
*              INDEX AND BUFFER                                                 
*              ON ENTRY P1 B0 = FORMAT OR MESSAGE (EQUATED)                     
*              ON EXIT BUFSTAT HAS FORMAT OR MESSAGE BIT SET                    
*                                                                               
RESTORE  NTR1                                                                   
         LA    R0,SAVF                                                          
         LA    R2,LIBRARY                                                       
         LA    R3,WKFILE                                                        
         LA    R4,FRMNDEX                                                       
         L     R5,AIOB                                                          
         L     R6,ATIA                                                          
         LA    R7,FRMRECHI                                                      
         CLI   0(R1),FORMAT                                                     
         BE    RST10                                                            
         LA    R0,SAVM                                                          
         LA    R4,MSGNDEX                                                       
         LA    R5,IO                                                            
         L     R6,ABUFFER                                                       
         LA    R7,MSGRECHI                                                      
*                                                                               
RST10    STM   R2,R6,DMCB          SET UP PARAMS IN DMCB                        
         MVC   DMCB(1),0(R1)                                                    
         NC    DMCB(1),BUFSTAT     DO WE NEED TO INITIALISE                     
         BNZ   OKXIT               NO - OUT                                     
         OC    BUFSTAT,0(R1)       YES                                          
         ST    R0,DMCB+12                                                       
         LR    R1,R0                                                            
         MVC   0(L'UKINDEX,R4),0(R1)                                            
         GOTO1 ADATAMGR,DMCB,=C'BUR'                                            
         USING UKRECD,R4                                                        
         MVC   UKLACTN,=C'GET'                                                  
         MVI   UKLREC1+3,1                                                      
         LA    RE,LIBRARY                                                       
         ST    RE,DMCB                                                          
         ST    R5,DMCB+12                                                       
         BASR  RE,RF               GET 1ST REC (HDR)                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         L     RF,UKLRECS                                                       
         SH    RF,NUMHDRS                                                       
         STH   RF,0(R7)            SET LAST CHUNK NUMBER IN BOOK                
         B     OKXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CLOSE A FORMAT OR MESSAGE BOOK AND SAVE               
*              DETAILS FOR LATER ACCESS                                         
*              ON ENTRY P1 B0 = FORMAT OR MESSAGE (EQUATED)                     
*              ON EXIT BUFSTAT HAS FORMAT OR MESSAGE BIT UNSET                  
*                                                                               
CLOSE    NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         MVC   DMCB(1),0(R1)       IS BUFFER IN USE                             
         NC    DMCB(1),BUFSTAT                                                  
         BZ    OKXIT               NO  - EXIT                                   
         XC    BUFSTAT,DMCB        YES - UNSET IN-USE BIT                       
         LA    R2,LIBRARY                                                       
         LA    R3,WKFILE                                                        
         LA    R4,FRMNDEX                                                       
         L     R5,AIOB                                                          
         L     R6,ATIA                                                          
         LA    R7,SAVF                                                          
         CLI   0(R1),FORMAT                                                     
         BE    CLS10                                                            
         LA    R4,MSGNDEX                                                       
         LA    R5,IO                                                            
         L     R6,ABUFFER                                                       
         LA    R7,SAVM                                                          
*                                                                               
CLS10    DS    0H                  DO CLOSE-LIBRARY CALL                        
         USING UKRECD,R4                                                        
         MVC   UKLACTN,=C'CLO'                                                  
         STM   R2,R6,DMCB                                                       
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         LA    RE,=C'BUS'          DO BUFFER SAVE CALL                          
         ST    RE,0(R1)                                                         
         ST    R7,12(R1)                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    OKXIT                                                            
         B     DMERROR                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO ADD (OR INSERT) ONE OR MORE FORMAT OR MESSAGE         
*              CHUNKS                                                           
*              ON ENTRY P1 B0   = FORMAT OR MESSAGE (EQUATED)                   
*                          B2-3 = 1ST CHUNK NUMBER                              
*                       P2 B0   = C'I' IF INSERT NOT APPEND                     
*                          B1-3 = A(RECORD BUFFER) - TERMINATOR=X'FF'           
*              ON EXIT  IOSTAT,DISPLOF/M,DISPHIF/M,FRMRECHI,FRMRPEAT,           
*                       MSGRECHI,FRECMHI UPDATED AS APPROPRIATE                 
*                       CC      = EQU IF ERROR                                  
*                                                                               
ADDCHNK  NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         BAS   RE,RESTORE          SET UP DMCB ETC                              
         L     R3,DMCB+8           R3 = A(INDEX)                                
         USING UKRECD,R3                                                        
         MVC   UKLACTN,=C'ADD'                                                  
         SR    R5,R5               R5 = REC NUM (ZERO IF APPEND)                
         CLI   4(R2),C'I'                                                       
         BNE   *+12                                                             
         LH    R5,2(R2)                                                         
         AH    R5,NUMHDRS                                                       
         ST    R5,UKLREC1                                                       
         L     R6,4(R2)                                                         
         LA    R6,0(R6)            R6 = RECORD BUFFER POINTER                   
         L     R4,DMCB+12          R4 = A(IO AREA FOR F/M)                      
         LA    R7,FRMRECHI         R7 = A(HIGH CHUNK NUM FOR F/M)               
         CLI   0(R2),FORMAT                                                     
         BE    ADC10                                                            
         LA    R7,MSGRECHI                                                      
*                                                                               
ADC10    CLI   0(R6),X'FF'         LOOP FOR A RECORD                            
         BE    OKXIT                                                            
         CR    R4,R6                                                            
         BE    ADC15                                                            
         LR    R0,R4               MOVE REC TO IO AREA                          
         LR    RE,R6                                                            
         LH    R1,0(R6)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
ADC15    GOTO1 ADATAMGR,DMCB       ADD TO BOOK                                  
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         OC    IOSTAT,0(R2)        INDICATE REC IS IN I/O AREA                  
*                                                                               
ADC20    L     RF,UKLRECS          UPDATE HIGH CHUNK NUM                        
         SH    RF,NUMHDRS                                                       
         STH   RF,0(R7)                                                         
*                                                                               
ADC30    CLI   0(R2),FORMAT        FORMAT CHUNK                                 
         BNE   ADC40                                                            
         MVC   DISPHIF,2(R2)       RESET DISPLAY CHUNK NOS (ONLY ONE)           
         MVC   DISPLOF,2(R2)                                                    
         CLI   LAST,C'Y'                                                        
         BE    OKXIT                                                            
         CLI   4(R2),C'I'          INSERT                                       
         BNE   *+14                                                             
         SR    RF,RF                                                            
         ICM   RF,3,FRMRPEAT                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,FRMRPEAT       RESET REPEAT CHUNK NUMBER UNLESS             
         B     OKXIT               LAST=Y                                       
*                                                                               
ADC40    CLI   4(R2),C'I'          MESSAGE CHUNK                                
         BE    ADC50                                                            
         STH   RF,DISPHIM          RESET HIGH DISPLAY CHUNK IF ADD              
         SH    RF,DISPLOM          RESET FRECMHI IE FORMAT CHUNK NUMBER         
         AH    RF,DISPLOF          CORRESPONDING TO LAST MESSAGE CHUNK          
         CH    RF,DISPHIF                                                       
         BNH   *+8                                                              
         LH    RF,DISPHIF                                                       
         STH   RF,FRECMHI                                                       
*                                                                               
ADC50    LH    RF,0(R6)            BUMP TO NEXT IN BUFFER                       
         AR    R6,RF                                                            
         LTR   R5,R5                                                            
         BZ    ADC10                                                            
         AH    R5,=H'1'                                                         
         ST    R5,UKLREC1                                                       
         B     ADC10                                                            
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GET A FORMAT OR MESSAGE CHUNK                         
*              ON ENTRY P1 B0   = FORMAT OR MESSAGE (EQUATED)                   
*                          B2-3 = CHUNK NUMBER (ZERO=HDR REC)                   
*              ON EXIT  IO/IOB  = RECORD AREA (MESSAGE/FORMAT)                  
*                       IOSTAT  = FORMAT AND/OR MESSAGE                         
*                                                                               
GETCHNK  NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         BAS   RE,RESTORE          SET UP DMCB ETC.                             
         BZ    ERRXIT                                                           
         L     R4,DMCB+8           PUT LIBRARY SUBACTION & REC NUM              
         USING UKRECD,R4           IN INDEX                                     
         MVC   UKLACTN,=C'GET'                                                  
         LH    RF,2(R2)                                                         
         AH    RF,NUMHDRS          REC NUM = CHUNK NUM + NUMHDRS                
         ST    RF,UKLREC1                                                       
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         OC    2(2,R2),2(R2)       SET IOSTAT IF NOT HDR                        
         BZ    OKXIT                                                            
         OC    IOSTAT,0(R2)                                                     
         B     OKXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PUT A FORMAT OR MESSAGE CHUNK                         
*              ON ENTRY P1 B0   = FORMAT OR MESSAGE (EQUATED)                   
*                          B2-3 = CHUNK NUMBER                                  
*                       P2      = A(RECORD)                                     
*              ON EXIT  CC      = EQU IF ERROR                                  
*                                                                               
PUTCHNK  NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         BAS   RE,RESTORE          SET UP DMCB                                  
         CLC   4(4,R2),DMCB+12     IS RECORD IN RIGHT I/O AREA                  
         BE    PTC10                                                            
         L     RE,4(R2)                                                         
         LH    RF,0(RE)                                                         
         L     R0,DMCB+12                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
PTC10    L     R4,DMCB+8           SET LIBRARY ACTION & REC NUM                 
         USING UKRECD,R4                                                        
         MVC   UKLACTN,=C'PUT'                                                  
         LH    RF,2(R2)                                                         
         AH    RF,NUMHDRS                                                       
         ST    RF,UKLREC1                                                       
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         OC    IOSTAT,0(R2)                                                     
         B     OKXIT                                                            
         EJECT                                                                  
*              ROUTINE TO DELETE ONE OR MORE FORMAT OR MESSAGE CHUNKS           
*              ON ENTRY P1 B0   = FORMAT OR MESSAGE (EQUATED)                   
*                          B2-3 = 1ST CHUNK NUMBER                              
*                       P2 B2-3 = LAST CHUNK NUMBER                             
*              ON EXIT  CC      = EQU IF ERROR                                  
*                                                                               
DELCHNK  NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         BAS   RE,RESTORE                                                       
         L     R4,DMCB+8                                                        
         USING UKRECD,R4                                                        
         MVC   UKLACTN,=C'DEL'                                                  
         LH    RE,2(R2)                                                         
         LH    RF,6(R2)                                                         
         AH    RE,NUMHDRS                                                       
         AH    RF,NUMHDRS                                                       
         STM   RE,RF,UKLREC1                                                    
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         LA    RF,FRMRECHI         SET NEW HIGH CHUNK NUM FOR BOOK              
         CLI   0(R2),FORMAT                                                     
         BE    DEL10                                                            
         LA    RF,MSGRECHI                                                      
         CLC   MSGRECHI,6(R2)      RESET FORMAT CHNK FOR LAST MES CHNK          
         BNE   *+10                IF LAST DELETED                              
         MVC   FRECMHI,FRMRPEAT                                                 
DEL10    L     RE,UKLRECS                                                       
         SH    RE,NUMHDRS                                                       
         STH   RE,0(RF)                                                         
         CLI   0(R2),MESSAGE                                                    
         BE    OKXIT                                                            
         CLC   FRMRPEAT,2(R2)      IF FORMAT,RESET REPEAT CHUNK NUMBER          
         BL    OKXIT               UNLESS DELETES STARTED BEYOND IT             
         LH    RF,6(R2)                                                         
         SR    RE,RE                                                            
         ICM   RE,3,FRMRPEAT                                                    
         SR    RF,RE                                                            
         BNM   *+6                                                              
         SR    RF,RF                                                            
         SH    RE,6(R2)            HIGH DELETE CHUNK                            
         AH    RE,2(R2)            LOW                                          
         BCTR  RE,0                                                             
         AR    RE,RF                                                            
         STCM  RE,3,FRMRPEAT                                                    
         B     OKXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FIND A FORMAT OR MESSAGE BOOK OR TO FIND NEXT         
*              AVAILABLE ID FOR THIS USER/INITIALS                              
*              ON ENTRY P1 B0   = FORMAT OR MESSAGE                             
*                          B1-3 = A(8 CHAR. BOOKNAME) - CONTAINS NULLS          
*                                 IF CALL IS FOR NEXT AVAILABLE                 
*              ON EXIT  P1 B1-3 = A(BOOKNAME) - NULLS REPLACED                  
*                       CC      = EQU IF ERROR                                  
*                                                                               
FINDBK   NTR1  BASE=ABASE          BUILD PARAMETER LIST                         
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         LA    R3,=C'BUF'                                                       
         LA    R4,WKFILE                                                        
         LA    R5,FRMNDEX                                                       
         L     R6,AIOB                                                          
         L     R7,ATIA                                                          
         CLI   0(R2),FORMAT                                                     
         BE    FDB10                                                            
         LA    R5,MSGNDEX                                                       
         LA    R6,IO                                                            
         L     R7,ABUFFER                                                       
*                                                                               
FDB10    STM   R3,R7,DMCB          INITIALISE                                   
         GOTO1 ADATAMGR,DMCB                                                    
         XC    0(L'FRMNDEX,R5),0(R5)                                            
         USING UKRECD,R5                                                        
         L     R3,0(R2)            PREPARE FOR INDEX CALL                       
         MVC   UKKEY,0(R3)                                                      
         LA    RE,=C'IND'                                                       
         ST    RE,DMCB                                                          
         OC    UKKEY,UKKEY                                                      
         BZ    FDB30                                                            
*                                                                               
FDB20    DS    0H                                                               
         BASR  RE,RF               SIMPLE READ FOR AN ID                        
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         OC    BUFSTAT,0(R2)                                                    
         B     OKXIT                                                            
*                                                                               
FDB30    MVC   0(2,R3),TWAUSRID    SEARCH FOR NEXT AVAILABLE ID                 
         MVI   2(R3),C'K'                                                       
         MVC   3(3,R3),LASTINIT    PUT IN ID UUKIII FOR COMPARISON              
         CLI   SAVMODE,FORMAT                                                   
         BE    FDB35                                                            
         MVC   2(4,R3),LASTINIT    OR UUIIII IF MESSAGE MODE                    
FDB35    DS    0H                                                               
         BASR  RE,RF               THEN SEARCH INDEX FOR HIGHEST NUMBER         
         TM    DMCB+8,X'80'        FOR THIS VALUE                               
         BO    FDB40                                                            
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         CLC   UKKEY(6),0(R3)                                                   
         BNE   FDB35                                                            
         CLC   UKCLASS,7(R3)       UKCLASS CONTAINS 100'S IN 1ST NIBBLE         
         BL    FDB35               (2ND NIBBLE ASSUMED NULL)                    
         BH    *+14                                                             
         CLC   UKDAY,6(R3)         UKDAY CONTAINS 10'S & 1'S                    
         BNH   FDB35                                                            
         MVC   6(2,R3),UKDAY                                                    
         B     FDB35                                                            
*                                                                               
FDB40    OI    7(R3),X'0C'         AT EOF BUMP NUMBER BY 1 UNLESS 99            
         CLI   6(R3),X'99'         IN WHICH CASE BUMP 100'S BY 1 UNLESS         
         BNE   FDB45               999                                          
         CLI   7(R3),X'9C'                                                      
         BNE   *+12                                                             
         MVI   FERN,NOBKNUMS                                                    
         B     ERRXIT                                                           
         AP    7(1,R3),=P'1'                                                    
         MVI   6(R3),0                                                          
         B     *+10                                                             
FDB45    AP    6(2,R3),=P'10'                                                   
         NI    7(R3),X'F0'                                                      
         B     OKXIT                                                            
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO GET A FORMAT CHUNK INTO IOB                           
*              ON ENTRY P1 = NEXT FORMAT CHUNK NUMBER                           
*                            NULL IF 78 BYTE UNPROT LINE REQUIRED               
*                       P3 = F (BYTE 0) IF REPEAT FLD VALS TO BE KEPT           
*              ON EXIT  P1 = FORMAT CHUNK NUMBER USED (OR ZERO)                 
*                       CC = EQU IF FORMAT NOT FOUND & FERN IS SET              
*                                                                               
GETFORM  NTR1  BASE=ABASE,WORK=(R7,4)                                           
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1               R2 = A(P1)                                   
         XC    0(8*4,R7),0(R7)     R7 = W/S FOR ONWARD PARAMETERS               
         L     R3,AIOB             R3 = A(IOB)                                  
*                                                                               
         OC    0(4,R2),0(R2)       P1 = NULLS - PASS BACK 78B UNPROT            
         BNZ   *+14                                                             
         MVC   0(L'FRMFORM,R3),FRMFORM                                          
         B     OKXIT                                                            
*                                                                               
         OC    FRMBOOK,FRMBOOK     NO FRMBOOK ID - PASS BACK DEFAULT            
         BNZ   *+14                IE 72B UNPROT + 3B PROT REF NUM              
         MVC   0(L'FRMDFLT,R3),FRMDFLT                                          
         B     OKXIT                                                            
*                                                                               
         CLC   FRMRPEAT,2(R2)      CHUNK NO GTR THAN REPEAT CHUNK               
         BNL   GTF10               MEANS THIS IS REPEAT UNLESS ITS LAST         
         MVC   2(2,R2),FRMRPEAT                                                 
         CLI   LAST,C'Y'                                                        
         BNE   *+14                                                             
         MVC   2(2,R2),FRMRECHI                                                 
         B     GTF10                                                            
         MVI   28(R7),C'R'         SET REPEAT INDICATOR                         
         TM    IOSTAT,FORMAT                                                    
         BO    GTF06               IOB CONTAINS (LAST) FORMAT CHUNK             
         TM    FRMSTAT,NOUNPS      IF NOT, BUT LAST FORMAT CHUNK HAS NO         
         BNO   GTF10               PRESET UNPROT VALUES, WE CAN USE             
         TM    IOSTAT,MESSAGE      LAST MESSAGE CHUNK                           
         BO    GTF05               DOES IO CONTAIN (LAST) MESSAGE CHNK          
         BAS   R4,GTFGETM          IF NOT READ IT INTO IO                       
GTF05    SR    R1,R1               MOVE IT TO IOB                               
         ICM   R1,3,IO                                                          
         LR    RF,R1                                                            
         LR    R0,R3                                                            
         LA    RE,IO                                                            
         MVCL  R0,RE                                                            
         OI    IOSTAT,FORMAT                                                    
*                                                                               
GTF06    LA    RE,2(R3)            CLEAR ITS UNPROTS UNLESS REPEAT FLDS         
         SR    RF,RF                                                            
GTF07    ICM   RF,1,0(RE)                                                       
         BZ    OKXIT                                                            
         TM    1(RE),X'20'                                                      
         BO    GTF08                                                            
         CLI   8(R2),C'F'          SAVE REPEATS IF P3 BO IS F=FIRST LNE         
         BNE   *+12                                                             
         TM    4(RE),REPEAT                                                     
         BO    GTF08                                                            
         SH    RF,=H'9'                                                         
         EX    RF,GTF09XC                                                       
         LA    RF,9(RF)                                                         
GTF08    AR    RE,RF                                                            
         B     GTF07                                                            
GTF09XC  XC    8(0,RE),8(RE)                                                    
*                                                                               
GTF10    MVC   0(4,R7),0(R2)       COME HERE IF WE NEED TO READ THE             
         MVI   0(R7),FORMAT        FRMBOOK                                      
         GOTO1 AGETCHNK,(R7)                                                    
         BZ    GTFXERR                                                          
         CLI   28(R7),C'R'         REPEAT                                       
         BNE   OKXIT                                                            
*                                                                               
GTF15    DS    0H                  IF ITS A REPEAT CHUNK CHECK TO SEE           
         MVI   BYTE,0              IF IT HAS PRESET UNPROTS AND HANDLE          
         LA    R5,2(R3)            REPEAT FIELDS                                
         SR    R6,R6                                                            
GTF16    ICM   R6,1,0(R5)                                                       
         BZ    GTF20                                                            
         SH    R6,=H'9'                                                         
         TM    1(R5),X'20'                                                      
         BO    GTF19                                                            
         EX    R6,GTF19OC                                                       
         EX    R6,GTF19CLC                                                      
         BE    *+12                                                             
         MVI   BYTE,1              BYTE=1 IF CHUNK CONTAINS AN UNPROT           
         B     GTF19                      WITH PRE-SET VALUE                    
         TM    4(R5),REPEAT                                                     
         BZ    GTF19                                                            
         CLI   8(R2),C'F'                                                       
         BNE   GTF19                                                            
         TM    IOSTAT,MESSAGE      IF FIELD IS A REPEAT IT TAKES ITS            
         BO    GTF18               VALUE FROM THE CORRESPONDING FIELD           
         LA    R4,GTF18            OF LAST MESSAGE CHUNK                        
GTFGETM  MVI   0(R7),MESSAGE                                                    
         MVC   2(2,R7),MSGRECHI                                                 
         GOTO1 AGETCHNK,(R7)                                                    
         BZ    GTFXERR                                                          
         BR    R4                                                               
GTF18    LA    RF,IO                                                            
         AR    RF,R5                                                            
         SR    RF,R3                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),8(RF)                                                    
GTF19    LA    R5,9(R5,R6)                                                      
         B     GTF16                                                            
GTF19OC  OC    8(0,R5),SPACES                                                   
GTF19CLC CLC   8(0,R5),SPACES                                                   
*                                                                               
GTF20    CLI   BYTE,0              SET INDICATOR THAT REPEAT CHUNK HAS          
         BNE   OKXIT               NO UNPROTS WITH PRESET VALUES & SO           
         OI    FRMSTAT,NOUNPS      CAN BE DERIVED FROM MESSAGE CHUNK,           
         B     OKXIT               USUALLY SAVING I/O (BOOK SWITCHING)          
*                                                                               
GTFXERR  MVI   FERN,NOFORMAT                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE A MESSAGE CHUNK IN THE TWA AND BUILD         
*              A CHUNK RECORD                                                   
*              ON ENTRY P1 = MESSAGE CHUNK NUMBER                               
*                       P2 = A(OUTPUT CHUNK RECORD BUFFER SLOT)                 
*                       P3 = A(DEFINING CHUNK RECORD)                           
*                       P4 = A(START OF CHUNK IN TWA)                           
*                       P5 = FIRST UPDATE CHUNK NUMBER                          
*              ON EXIT  P1 = UNCHANGED                                          
*                       P2 = A(NEXT OUTPUT CHUNK BUFFER SLOT)                   
*                       P3 = UNCHANGED                                          
*                       P4 = A(START OF NEXT CHUNK IN TWA)                      
*                       P5 = UNCHANGED                                          
*                       CC = EQU IF VALIDATION ERROR - ACURSOR SET              
*                       CC = NEG IF NO CHANGE AND TWA IS TERMINATED IF          
*                            ACTION IS ADD                                      
*                                                                               
VALMESS  NTR1  BASE=ABASE,WORK=(R7,2)                                           
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         LM    R3,R5,4(R2)                                                      
         MVI   0(R7),0             INITIALISE UNPROT INDICATOR                  
         MVI   0(R3),X'FF'         TERMINATE OUTPUT BUFFER                      
*                                                                               
VLM01    CLC   2(2,R2),18(R2)      IF WE HAVEN'T REACHED 1ST UPDATE REC         
         BL    VLM25               JUST UPDATE REC NUM & TWA POINTER            
         SR    R1,R1               MOVE RECORD TO OUTPUT BUFFER                 
         MVC   0(2,R3),0(R4)       TAKING HDRS FROM DEFINING CHUNK &            
         LA    R3,2(R3)            DATA FROM TWA                                
         LA    R4,2(R4)                                                         
*                                                                               
VLM10    MVC   0(8,R3),0(R4)       LOOP FOR A FLD                               
         CLI   0(R3),0                                                          
         BE    VLM20                                                            
         CLC   0(1,R3),0(R5)                                                    
         BE    VLM12                                                            
         MVI   FERN,FRMINCMP       FORMAT INCOMPATIBLE TWA-CHUNK SO             
         ST    R5,ACURSOR          REQUEST SCREEN ACTION (DUAL UPDATE)          
         B     ERRXIT                                                           
VLM12    MVC   8(80,R3),8(R5)      MOVE DATA FROM TWA                           
         IC    R1,0(R4)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,VLMORSP3         OR IN SPACES                                 
         EX    R1,VLMORSP4                                                      
         TM    1(R5),X'20'                                                      
         BO    *+8                                                              
         MVI   0(R7),C'U'          CHUNK CONTAINS AN UNPROT                     
         TM    4(R3),REFNUM        REVERT REFNUM TO KEYWORD                     
         BNO   *+10                                                             
         MVC   8(3,R3),=C'&&NO'                                                 
         LA    R1,9(R1)                                                         
         AR    R3,R1               BUMP TO NEXT FIELD                           
         AR    R4,R1                                                            
         AR    R5,R1                                                            
         B     VLM10                                                            
VLMORSP3 OC    8(0,R3),SPACES                                                   
VLMORSP4 OC    8(0,R4),SPACES                                                   
*                                                                               
VLM20    MVI   1(R3),X'FF'         CHECK FOR INPUT THIS TIME                    
         LM    R3,R5,4(R2)                                                      
         CLI   0(R7),C'U'          UNLESS ITS ALL PROTECTED                     
         BNE   VLM30                                                            
         LH    R1,0(R3)                                                         
         LR    RF,R1                                                            
         LR    R0,R3                                                            
         LR    RE,R4                                                            
         CLCL  R0,RE                                                            
         LM    R3,R5,4(R2)                                                      
         BNE   VLM30                                                            
         MVI   0(R3),X'FF'         NO CHANGE                                    
         CLI   ACTION,ADM                                                       
         BE    VLM27               IF ADD DONT BUMP TWA PNTER                   
*                                                                               
VLM25    LH    R1,0(R4)            NO CHANGE EXIT                               
         SH    R1,=H'3'            RETURN UPDATED TWA POINTER                   
         AR    R5,R1                                                            
VLM27    ST    R5,12(R2)                                                        
         LNR   R2,R2                                                            
         B     EXIT                                                             
*                                                                               
VLM30    LA    R3,2(R3)            NOW APPLY RULES IF ANY                       
         SR    R0,R0                                                            
*                                                                               
VLM32    MVC   0(4,R7),4(R3)       LOOP FOR A FIELD                             
         NI    0(R7),ALL-REFNUM                                                 
         OC    0(4,R7),0(R7)                                                    
         BZ    VLM40               NO RULES OTHER THAN REFNUM                   
         LA    R6,RULETAB+L'RULETAB                                             
VLM34    CLI   0(R6),X'FF'         LOOP FOR A RULE                              
         BE    VLM40                                                            
         MVC   0(4,R7),0(R6)                                                    
         NC    0(4,R7),4(R3)                                                    
         BZ    VLM36               DOESN'T APPLY                                
         SR    RF,RF                                                            
         ICM   RF,7,5(R6)          DOES,SO CALL VALIDATION ROUTINE              
         A     RF,RELO             PASSING A(FLDHDR)- IN TWA & BUFFER           
         GOTO1 (RF),(R7),(R5),(R3)                                              
         BNZ   VLM36                                                            
         ST    R5,ACURSOR          VALIDATION ERROR                             
         B     EXIT                                                             
VLM36    LA    R6,L'RULETAB(R6)                                                 
         B     VLM34                                                            
*                                                                               
VLM40    IC    R0,0(R3)            BUMP TO NEXT FIELD                           
         AR    R3,R0                                                            
         AR    R5,R0                                                            
         CLI   0(R3),0                                                          
         BNE   VLM32                                                            
*                                                                               
VLM45    LA    R3,1(R3)            END OF CHUNK - ALL VALID                     
         ST    R3,4(R2)            RETURN UPDATED OUTPUT POINTER                
         ST    R5,12(R2)           AND TWA POINTER                              
         B     OKXIT                                                            
         SPACE 3                                                                
*              ROUTINES CALLED BY VALMESS TO APPLY RULES TO FIELDS              
*              ON ENTRY P1 = A(FLD HDR IN TWA)                                  
*                       P2 = A(FLD HDR IN CHUNK REC)                            
*              ON EXIT  CC = EQU IF ERROR - FERN SET                            
*                                                                               
VALNONE  LTR   RB,RB               OK WITHOUT FURTHER CHECK                     
         BR    RE                                                               
*                                                                               
VALREPT  B     VALNONE             REPEAT                                       
*                                                                               
VALMAND  L     RF,0(R1)            MANDATORY                                    
         CLI   5(RF),0                                                          
         BNER  RE                                                               
         MVI   FERN,MISSING                                                     
         BR    RE                                                               
*                                                                               
VALCASH  NTR1  WORK=(R7,1)         CASH                                         
         L     R1,0(R1)                                                         
         CLI   5(R1),0                                                          
         BE    OKXIT                                                            
         ZIC   R2,5(R1)                                                         
         LA    R1,8(R1)                                                         
         STM   R1,R2,0(R7)                                                      
         GOTO1 ACASHVAL,(R7)                                                    
         CLI   0(R1),X'FF'                                                      
         BNE   EXIT                                                             
VLC10    MVI   FERN,INVCASH                                                     
         B     EXIT                                                             
*                                                                               
VALNUMB  NTR1                      NUMERIC (CANT USE FLDIIND)                   
         L     RF,0(R1)                                                         
         SR    R1,R1                                                            
         ICM   R1,1,5(RF)                                                       
         BZ    OKXIT                                                            
         LA    RE,7(R1,RF)                                                      
         TM    0(RE),X'F0'                                                      
         BNO   *+12                                                             
         BCT   R1,*-12                                                          
         B     OKXIT                                                            
         MVI   FERN,INVNUM                                                      
         B     ERRXIT                                                           
*                                                                               
VALCHCK  OI    MSGSTAT,CHECKSUM    CHECKSUM                                     
         BR    RE                                                               
*                                                                               
VALDATE  NTR1  WORK=(R7,1)         DATE                                         
         L     RF,0(R1)                                                         
         CLI   5(RF),0                                                          
         BE    OKXIT                                                            
         LA    RF,8(RF)                                                         
         ST    RF,0(R1)                                                         
         GOTO1 ADATVAL,(R1),,(R7)                                               
         CLI   3(R1),0                                                          
         BNE   EXIT                                                             
         MVI   FERN,INVDATE                                                     
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 3                                                                
         DS    0F                                                               
RULETAB  DS    0CL8                                                             
         DC    X'80000000',X'FF',AL3(0)      REFNUM                             
         DC    X'40000000',C'P',AL3(VALNONE) PROTECT                            
         DC    X'20000000',C'R',AL3(VALREPT) REPEAT                             
         DC    X'10000000',C'M',AL3(VALMAND) MANDATORY                          
         DC    X'08000000',C'S',AL3(VALCHCK) SUM                                
         DC    X'04000000',C'C',AL3(VALCASH) CASH                               
         DC    X'02000000',C'N',AL3(VALNUMB) NUMERIC                            
         DC    X'01000000',C'T',AL3(VALCHCK) CHECKSUM                           
         DC    X'00800000',C'D',AL3(VALDATE) DATE                               
         DC    X'00400000',C'Q',AL3(VALNUMB) SUM MULTIPLIER EG QUANTITY         
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO DISPLAY A CHUNK IN MESSAGE FORMAT                     
*              ON ENTRY P1 B2-3 = FORMAT CHUNK NUMBER OR ZERO                   
*                       P2 B2-3 = MESSAGE CHUNK NUMBER OR ZERO                  
*                       P3 B0   = F IF FIRST CALL FOR SCREEN                    
*                          B1-3 = A(CHUNK RECORD)                               
*                       P4 B0   = START LINE NUMBER ON SCREEN (4-24)            
*                          B1-3 = A(START LOCATION IN TWA)                      
*              ON EXIT  P1/P2   = INCREMENTED BY 1 IF NON-ZERO                  
*                       P3 B0   = CLEARED                                       
*                       P4      = INCREMENTED LINE NUMBER & TWA POINTER         
*                       DISPLOF/LOM/HIF/HIM ARE UPDATED                         
*                       ATWADIFF IS SET TO A(FIRST NEW-FORMAT FLD HDR)          
*                       CC      = EQU IF CHUNK WONT FIT IN TWA OR SCRN          
*                                                                               
DISMESS  NTR1  BASE=ABASE,WORK=(R7,11)                                          
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1               R2 = A(PLIST)                                
         LM    R3,R4,8(R2)                                                      
         LA    R3,0(R3)            R3 = A(CHUNK REC)                            
         LA    R4,0(R4)            R4 = A(TWA)                                  
         USING FLDHDRD,R7          R7 = A(CHUNK FIELD IN PROCESS)               
*                                                                               
DSM01    LH    R0,0(R3)            CHECK FOR ROOM IN TWA                        
         AR    R0,R4                                                            
         SH    R0,=H'3'            RECLEN+TERMINATOR                            
         C     R0,AENDSCR          IS THERE ROOM FOR THIS + TAB(9) +            
         BNL   ERRXIT              TERMINATOR(3)                                
*                                                                               
DSM05    LA    RE,2(R3)            CHECK FOR ROOM ON SCREEN                     
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   *-10                                                             
         SR    RE,RF               RE = A(LAST FIELD IN CHUNK)                  
         LH    R0,2(RE)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'80'           R1 = NUMBER OF EXTRA LINES                   
         ZIC   R5,12(R2)           R5 = START LINE                              
         AR    R1,R5               R1 = LAST LINE USED WITH THIS CHUNK          
         CH    R1,SCRLMAX                                                       
         BL    DSM10                                                            
         BH    ERRXIT                                                           
         TM    4(RE),REFNUM        IF WERE ON THE LAST POSSIBLE LINE            
         BO    DSM10               WE NEED ROOM FOR A TAB FLD - A REF           
         CLI   ACTION,HEL          NUM WILL DO                                  
         BE    DSM10               NOT REQ FOR HELP (ALL PROT)                  
         AR    R0,RF                           -                                
         CH    R0,=H'85'           - OTHERWISE END OF LAST FLD MUST BE          
         BNL   ERRXIT              BEFORE COL 77 TO ALLOW TAB IN 79             
*                                                                               
DSM10    LA    R1,1(R1)            THERE'S ROOM SO PREPARE TO MOVE IT           
         STC   R1,12(R2)           FLD BY FLD - RETURN NEXT LINE NUMBER         
         BCTR  R5,0                                                             
         MH    R5,=H'80'           R5 = BASE FLDADR FOR CHUNK                   
         LA    R3,2(R3)                                                         
*                                                                               
DSM15    ZIC   R6,0(R3)            LOOP FOR A FIELD                             
         SH    R6,=H'9'                                                         
         MVC   FLDHDRD(90),0(R3)   MOVE IT INTO W/S                             
         LH    RF,FLDADR                                                        
         AR    RF,R5                                                            
         STH   RF,FLDADR           SET ADDR TO SCREEN-RELATIVE                  
         XC    FLDIIND(4),FLDIIND                                               
         TM    FLDATB,X'20'                                                     
         BO    DSM18                                                            
         CLC   TWAUSRID,MSGBOOK    PROTECT ANY FIELDS THAT NEED IT FROM         
         BE    DSM18               NON-OWNERS                                   
         TM    SAVACCS,WRITACC                                                  
         BNO   DSM17                                                            
         TM    4(R3),PROTECT       PROTECT RULE SAYS 'PROTECT FROM ALL          
         BNO   DSM18               WRITERS OTHER THAN OWNER'                    
DSM17    OI    FLDOIND,X'A0'                                                    
         OI    FLDATB,X'20'                                                     
DSM18    CLC   8(3,R3),=C'&&NO'    DISPLAY REF NUM                              
         BNE   DSM20                                                            
         EDIT  (2,6(R2)),(3,8(R7)),FILL=0                                       
*                                                                               
DSM20    CLC   FLDLEN(4),0(R4)     CHECK FOR FORMAT CHANGE                      
         BE    DSM22                                                            
         OC    ATWADIFF,ATWADIFF                                                
         BNZ   DSM24                                                            
         ST    R4,ATWADIFF         FIRST TIME SAVE A(TWA HDR) - USED TO         
         B     DSM24               DETERMINE TRANSMIT PROCESS AT END OF         
DSM22    TM    TRMTYPE,T3270       TRANSACTION                                  
         BZ    DSM24               IF NOT 3270 TRANSMIT REGARDLESS              
         EX    R6,DSMCLC           OTHERWISE                                    
         BE    DSM25               CHECK IF FLD IS CHANGED                      
         EX    R6,DSMORSP1                                                      
         EX    R6,DSMORSP2                                                      
         EX    R6,DSMCLC                                                        
         BE    DSM25                                                            
DSM24    OI    FLDOIND,X'80'                                                    
*                                                                               
DSM25    LA    R6,8(R6)            MOVE FLD TO TWA AND BUMP TO NEXT             
         EX    R6,DSMMVC                                                        
         LA    R6,1(R6)                                                         
         AR    R3,R6                                                            
         AR    R4,R6                                                            
         CLI   0(R3),0                                                          
         BNE   DSM15                                                            
*                                                                               
DSM28    MVC   DISPHIF,2(R2)       AT END UPDATE SAVED DISPLAY REF NUM          
         MVC   DISPHIM,6(R2)       RANGES                                       
         CLI   8(R2),C'F'          FIRST                                        
         BNE   DSM30                                                            
         MVC   DISPLOF,2(R2)                                                    
         MVC   DISPLOM,6(R2)                                                    
*                                                                               
DSM30    OC    2(2,R2),2(R2)       AND UPDATE P LIST                            
         BZ    DSM32                                                            
         LH    RF,2(R2)            FORMAT REF NUM                               
         LA    RF,1(RF)                                                         
         STH   RF,2(R2)                                                         
DSM32    OC    6(2,R2),6(R2)                                                    
         BZ    DSM33                                                            
         LH    RF,6(R2)            MESSAGE REF NUM                              
         LA    RF,1(RF)                                                         
         STH   RF,6(R2)                                                         
DSM33    MVI   8(R2),0             FIRST-TIME INDICATOR                         
DSM34    STCM  R4,7,13(R2)         TWA POINTER                                  
         B     OKXIT                                                            
*                                                                               
DSMORSP1 OC    8(0,R4),SPACES                                                   
DSMORSP2 OC    FLDDATA(0),SPACES                                                
DSMCLC   CLC   FLDDATA(0),8(R4)                                                 
DSMMVC   MVC   0(0,R4),FLDLEN                                                   
         DROP  R7                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE FORMAT DEFINITION IN IO                   
*              ON ENTRY P1 = FORMAT CHUNK NUMBER                                
*                                                                               
DISFORM  NTR1  BASE=ABASE,WORK=(R7,2)                                           
         LM    R8,R9,A3RDBASE                                                   
         XC    0(12,R7),0(R7)                                                   
         LR    R2,R1                                                            
         LA    R6,IO                                                            
         LA    R6,2(R6)                                                         
         USING FLDHDRD,R6                                                       
*                                                                               
DSF10    CLI   0(R6),0             LOOP FOR A FLD                               
         BE    DSFEND                                                           
         LH    R4,FLDADR           FIND FLD START                               
         SRDL  R4,32                                                            
         D     R4,=F'80'                                                        
         ZIC   RF,KWXDATAH                                                      
         MR    RE,R5                                                            
         LA    R3,KWXDATAH(RF)     R3 = LINE HDR                                
         OI    6(R3),X'80'                                                      
         LA    R4,7(R3,R4)         R4 = FLD START                               
         ZIC   RF,FLDLEN                                                        
         SH    RF,=H'9'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),FLDDATA     MOVE DATA TO TWA                             
         LA    R1,1(R4,RF)         R1 = CHAR POS AFTER FLD                      
*                                                                               
DSF20    TM    FLDATB,X'20'        IF UNPROT, WRAP IN QUOTES                    
         BO    DSF40                                                            
         BCTR  R4,0                                                             
         MVI   0(R4),QUOT                                                       
         MVI   0(R1),QUOT                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DSF25    MVC   0(4,R7),FLDIIND     AND CHECK FOR RULES                          
         NI    3(R7),ALL-EOL                                                    
         OC    0(4,R7),0(R7)                                                    
         BZ    DSF40                                                            
         MVI   1(R4),C'&&'         IF ANY DISPLAY '&STRING'                     
         LA    R4,2(R4)                                                         
         L     RE,ARULETAB                                                      
*                                                                               
DSF30    CLI   0(RE),X'FF'         LOOP FOR A RULE                              
         BE    DSF40                                                            
         MVC   0(4,R7),0(RE)                                                    
         NC    0(4,R7),FLDIIND                                                  
         BZ    *+14                                                             
         MVC   0(1,R4),4(RE)                                                    
         LA    R4,1(R4)                                                         
         LA    RE,8(RE)                                                         
         B     DSF30                                                            
*                                                                               
DSF40    TM    FLDOLEN,EOL         APPEND END OF LINE INDICATOR IF              
         BZ    DSF45               REQUIRED                                     
         MVC   0(4,R1),=C'&&EOL'                                                
*                                                                               
DSF45    ZIC   R0,FLDLEN           BUMP TO NEXT FLD                             
         AR    R6,R0                                                            
         B     DSF10                                                            
*                                                                               
DSFEND   ZIC   R1,0(R3)            AT END INSERT TERMINATOR /& AFTER            
         AR    R3,R1               LAST FIELD                                   
         SH    R3,=H'2'                                                         
         OC    0(2,R3),0(R3)       CHECK THERE'S ROOM IN THIS LINE              
         BE    DSFEND1                                                          
         LA    R3,2(R3)            IF NOT USE 1ST 2 CHARS OF NEXT ONE           
         OI    6(R3),X'80'                                                      
         LA    R3,8(R3)                                                         
         B     DSFEND2                                                          
         CLI   0(R3),0                                                          
         BH    *+8                                                              
DSFEND1  BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
DSFEND2  MVC   0(2,R3),=C'/&&'                                                  
         MVC   DISPLOF,2(R2)                                                    
         MVC   DISPHIF,2(R2)                                                    
         B     OKXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CREATE A FORMAT OR MESSAGE SCREEN                     
*              ON ENTRY P1 = START FORMAT CHUNK NUMBER                          
*                            ZERO MEANS A SIMPLE 78XN FORMAT SCREEN             
*                       P2 = START MESSAGE CHUNK NUMBER OR ZERO                 
*                       P3 = MAX NUMBER OF CHUNKS TO BE SHOWN OR ZERO           
*                                                                               
SETSCRN  NTR1  BASE=ABASE,WORK=(R7,2)                                           
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         LR    R1,R7               USE LOCAL W/S FOR ONWARD PLIST               
         MVC   0(8,R1),0(R2)       P1&2 = F&M CHUNK NOS                         
         MVC   8(4,R1),AIOB        P3   = F=FIRST,A(IOB)                        
         MVI   8(R1),C'F'                                                       
         LA    RF,KWXDATAH         P4   = SCREEN LINE/START TWA ADDR            
         ST    RF,12(R1)                                                        
         MVI   12(R1),4                                                         
         ICM   R0,15,8(R2)         COUNT                                        
         BNZ   STS01                                                            
         LA    R0,20                                                            
         XC    DISPLOM(8),DISPLOM  INITIALISE SAVED REF RANGES                  
*                                                                               
STS01    BAS   RE,GETFORM          GET FORMAT CHUNK                             
         BZ    STS10               FORMAT NOT FOUND                             
         BAS   RE,DISMESS                                                       
         BZ    STS10               TWA OR SCREEN FULL                           
         LA    R4,IO                                                            
         L     R6,AIOB                                                          
         LH    R5,0(R6)                                                         
         LR    R7,R5                                                            
         MVCL  R4,R6               SAVE LAST DISPLAYED CHUNK REC IN IO          
         NI    FRMSTAT,ALL-NOUNPS                                               
         NI    IOSTAT,ALL-MESSAGE                                               
         BCT   R0,STS01                                                         
*                                                                               
STS10    MVC   4(4,R1),12(R1)      TERMINATE SCREEN                             
         LA    R4,IO                                                            
         ST    R4,0(R1)            PASSING LAST DISPLAYED CHUNK                 
         BAS   RE,TWATERM                                                       
         B     OKXIT                                                            
         EJECT                                                                  
*              ROUTINE TO DISPLAY A SCREENFUL OF MESSAGE CHUNKS                 
*              ON ENTRY P1 = START MESSAGE CHUNK NUMBER                         
*                       P2 = MAX NUMBER OF CHUNKS TO BE SHOWN OR ZERO           
*                                                                               
DISSCRN  NTR1  BASE=ABASE,WORK=(R7,2)                                           
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         LR    R1,R7               USE LOCAL W/S FOR ONWARD PLIST               
         MVC   0(4,R1),0(R2)       P1 = CHUNK NUMBER                            
         MVI   0(R1),MESSAGE            LABELLED MESSAGE FOR GETCHNK            
         MVC   4(4,R1),0(R1)       P2 = P1                                      
         LA    RF,IO                                                            
         ST    RF,8(R1)            P3 = F=FIRST,A(CHUNK REC)                    
         MVI   8(R1),C'F'                                                       
         LA    RF,KWXDATAH         P4 = LINE,A(TWA)                             
         ST    RF,12(R1)                                                        
         MVI   12(R1),4                                                         
         ICM   R0,15,4(R2)         COUNT                                        
         BNZ   DSS01                                                            
         LA    R0,20                                                            
         XC    DISPLOM(8),DISPLOM  INITIALISE SAVED REF RANGES                  
*                                                                               
DSS01    BAS   RE,GETCHNK                                                       
         BZ    DSS10               NOT FOUND                                    
         BAS   RE,DISMESS                                                       
         BZ    DSS10               TWA OR SCREEN FULL                           
         L     R4,AIOB                                                          
         LA    R6,IO                                                            
         LH    R5,0(R6)                                                         
         LR    R7,R5                                                            
         MVCL  R4,R6               SAVE LAST DISPLAYED CHUNK REC IN IOB         
         NI    IOSTAT,ALL-FORMAT                                                
         BCT   R0,DSS01                                                         
*                                                                               
DSS10    MVC   4(4,R1),12(R1)      TERMINATE SCREEN                             
         MVC   0(4,R1),AIOB        PASSING LAST DISPLAYED CHUNK                 
         BAS   RE,TWATERM                                                       
         B     OKXIT                                                            
         EJECT                                                                  
*              ROUTINE TO TERMINATE A SCREEN IN THE TWA                         
*              ON ENTRY P1 = A(LAST CHUNK RECORD DISPLAYED)                     
*                       P2 = A(NEXT AVAILABLE LOCATION IN TWA)                  
*                                                                               
TWATERM  NTR1  BASE=ABASE,WORK=(R7,2)                                           
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         LM    R3,R4,0(R2)                                                      
         LA    R3,0(R3)            R3 = A(CHUNK REC)                            
         LA    R4,0(R4)            R4 = A(TWA)                                  
         USING FLDHDRD,R7          R7 = A(TAB FIELD)                            
         OC    DISPHIF,DISPHIF     UNFORMATTED SCREENS DONT NEED TAB            
         BZ    TWT21               FIELDS                                       
         OC    DISPLOM(8),DISPLOM  NOR DO EMPTY ONES                            
         BZ    TWT21                                                            
*                                                                               
TWT01    LA    RE,2(R3)            FIND LAST FIELD IN LAST CHUNK                
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   *-10                                                             
         SR    RE,RF               RE = A(LAST FLD IN CHUNK)                    
         SR    R4,RF               R4 = A(            TWA)                      
*                                                                               
TWT05    TM    4(RE),REFNUM        IF ITS A REF NUM, MAKE THIS THE              
         BNO   TWT10               TAB FIELD                                    
         NI    1(R4),X'DF'                                                      
         OI    6(R4),X'80'                                                      
         B     TWT20                                                            
*                                                                               
TWT10    DS    0H                  OTHERWISE APPEND A TAB FLD                   
         XC    FLDLEN(9),FLDLEN    BUILD IT FIRST IN LOCAL W/S                  
         MVI   FLDLEN,9                                                         
         LH    R1,2(R4)                                                         
         AR    R1,RF                                                            
         SH    R1,=H'6'                                                         
         LR    RE,R1                                                            
         LA    R1,1(R1)            CHECK FOR COL 80                             
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    RE,2(RE)            IF IT IS GO ON TO COL 2                      
         STH   RE,FLDADR                                                        
         OI    FLDOIND,X'80'                                                    
         AR    R4,RF                                                            
         CLC   FLDLEN(4),0(R4)     IS IT A FORMAT CHANGE                        
         BE    TWT12                                                            
         OC    ATWADIFF,ATWADIFF                                                
         BNZ   TWT14                                                            
         ST    R4,ATWADIFF                                                      
         B     TWT14                                                            
TWT12    CLI   8(R4),C' '          IF NOT IS IT A CHANGE AT ALL                 
         BNH   *+10                                                             
TWT14    MVC   0(9,R4),FLDLEN      MOVE IT INTO TWA                             
         LA    RF,9                                                             
*                                                                               
TWT20    AR    R4,RF               NOW TERMINATE TWA                            
TWT21    OC    FRMSTAT,SAVMODE                                                  
         OC    ATWADIFF,ATWADIFF                                                
         BNZ   TWT25                                                            
         CLI   0(R4),0             WILL THIS BE THE FIRST FORMAT CHANGE         
         BE    OKXIT                                                            
         ST    R4,ATWADIFF         YES                                          
TWT25    XC    0(3,R4),0(R4)                                                    
         B     OKXIT                                                            
         DROP  R7                                                               
         EJECT                                                                  
*              ROUTINE TO SET UP REF NUM DISPLAY AS 'REF N(-N) OF N'            
*              ON ENTRY P1 B0-1 = LOW REF NUMBER                                
*                          B2-3 = HIGH REF NUMBER (OR ZERO)                     
*                       P2      = A(DISPLAY AREA)                               
*                       P3 B2-3 = HIGH REF NUMBER IN BOOK                       
*              ON EXIT  P2      = A(NEXT USEABLE POS IN DISPLAY AREA)           
*                                                                               
EDITREF  NTR1  BASE=ABASE                                                       
         LM    R8,R9,A3RDBASE                                                   
         LR    R2,R1                                                            
         L     R3,4(R2)                                                         
         MVC   0(3,R3),=C'REF'                                                  
         LA    R3,4(R3)                                                         
         LH    R0,0(R2)            LOW NUMBER                                   
         BAS   RE,EDR10                                                         
         CLC   2(2,R2),0(R2)       HIGH NUMBER ONLY IF PRESENT & NOT            
         BE    EDR20               SAME AS LOW                                  
         LH    R0,2(R2)                                                         
         LTR   R0,R0                                                            
         BZ    EDR20                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    RE,EDR20                                                         
*                                                                               
EDR10    EDIT  (R0),(3,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
EDR20    MVC   1(2,R3),=C'OF'      OF N                                         
         LA    R3,4(R3)                                                         
         LH    R0,10(R2)                                                        
         BAS   RE,EDR10                                                         
         LA    R3,1(R3)            RETURN NEXT ADDR AFTER 1 SPACE               
         ST    R3,4(R2)                                                         
         B     OKXIT                                                            
         EJECT                                                                  
*              CTFILE READ                                                      
*                                                                               
READ     NTR1                                                                   
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,KEY,WORK                 
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
         B     OKXIT                                                            
         SPACE 3                                                                
*              WKFILE I/O ERRORS                                                
*                                                                               
DMERROR  MVI   FERN,INVALID                                                     
         TM    DMCB+8,X'D0'                                                     
         BZ    ERRXIT                                                           
         MVI   FERN,RECNFND                                                     
         TM    DMCB+8,X'10'                                                     
         BO    ERRXIT                                                           
         MVI   FERN,ENDFILE                                                     
         TM    DMCB+8,X'80'                                                     
         BO    ERRXIT                                                           
         MVI   FERN,DISKERR                                                     
         B     ERRXIT                                                           
         EJECT                                                                  
*              ACTION TABLE COVERED BY DSECT ACTIOND                            
*                                                                               
ACTAB    DS    0C                                                               
ACTBOF   DC    AL1(ACTBOFX-*,01,BOO,FORMAT)                                     
         DC    CL2'BO'                                                          
         DC    C'BOOK(=NAME)(,ADD OR DEL)(,COMMENT=16CHS)'                      
         DC    C'(,RET=DAYS)(,CLASS=A+B+C)'                                     
ACTBOFX  DS    0H                                                               
ACTBOM   DC    AL1(ACTBOMX-*,01,BOO,MESSAGE)                                    
         DC    CL2'BO'                                                          
         DC    C'BOOK(=NAME)(,ADD OR DEL)(,FORM=NAME)(,COMMENT=16 CHS)'         
         DC    C'(,RET=DAYS)'                                                   
ACTBOMX  DS    0H                                                               
ACTFOR   DC    AL1(ACTFORX-*,01,FOR,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'FO'                                                          
         DC    C'FORMAT=NAME(,N-N)(,MAX=N)'                                     
ACTFORX  DS    0H                                                               
ACTADF   DC    AL1(ACTADFX-*,03,ADF,FORMAT+BOOK+WRITACC)                        
         DC    CL2'AD'                                                          
         DC    C'ADD,N OR ,LAST'                                                
ACTADFX  DS    0H                                                               
ACTADM   DC    AL1(ACTADMX-*,04,ADM,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'AD'                                                          
         DC    C'ADD(,LAST)(,END)(,NEXT=FORMNAME)(,MAX=N)'                      
ACTADMX  DS    0H                                                               
ACTDIF   DC    AL1(ACTDIFX-*,03,DIF,FORMAT+BOOK)                                
         DC    CL2'DI'                                                          
         DC    C'DISPLAY,N OR ,NEXT OR ,LAST'                                   
ACTDIFX  DS    0H                                                               
ACTDIM   DC    AL1(ACTDIMX-*,05,DIM,MESSAGE+BOOK)                               
         DC    CL2'DI'                                                          
         DC    C'DISPLAY(,N-N)OR(,NEXT)OR(,LAST)'                               
ACTDIMX  DS    0H                                                               
ACTCHF   DC    AL1(ACTCHFX-*,03,CHF,FORMAT+BOOK+WRITACC)                        
         DC    CL2'CH'                                                          
         DC    C'CHANGE,N'                                                      
ACTCHFX  DS    0H                                                               
ACTCHM   DC    AL1(ACTCHMX-*,05,CHM,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'CH'                                                          
         DC    C'CHANGE(,N-N)OR(,LAST)'                                         
ACTCHMX  DS    0H                                                               
ACTINF   DC    AL1(ACTINFX-*,03,INF,FORMAT+BOOK+WRITACC)                        
         DC    CL2'IN'                                                          
         DC    C'INSERT,N'                                                      
ACTINFX  DS    0H                                                               
ACTINM   DC    AL1(ACTINMX-*,06,INM,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'IN'                                                          
         DC    C'INSERT,N-N(,FORM=0)'                                           
ACTINMX  DS    0H                                                               
ACTDEF   DC    AL1(ACTDEFX-*,03,DEF,FORMAT+BOOK+WRITACC)                        
         DC    CL2'DE'                                                          
         DC    C'DELETE,N'                                                      
ACTDEFX  DS    0H                                                               
ACTDEM   DC    AL1(ACTDEMX-*,05,DEM,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'DE'                                                          
         DC    C'DELETE,N-N'                                                    
ACTDEMX  DS    0H                                                               
ACTREP   DC    AL1(ACTREPX-*,07,REP,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'RE'                                                          
         DC    C'REPLACE(,N-N),OLD=ABCD,NEW=EFGH'                               
ACTREPX  DS    0H                                                               
ACTEND   DC    AL1(ACTENDX-*,04,END,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'EN'                                                          
         DC    C'END(,CHECK=NO)'                                                
ACTENDX  DS    0H                                                               
ACTUND   DC    AL1(ACTUNDX-*,04,UND,MESSAGE+BOOK+WRITACC)                       
         DC    CL2'UN'                                                          
         DC    C'UNEND'                                                         
ACTUNDX  DS    0H                                                               
ACTCOP   DC    AL1(ACTCOPX-*,08,COP,FORMAT+MESSAGE+BOOK)                        
         DC    CL2'CO'                                                          
         DC    C'COPY(,BOOK=NAME)'                                              
ACTCOPX  DS    0H                                                               
ACTOVC   DC    AL1(ACTOVCX-*,08,OVC,FORMAT+MESSAGE+BOOK)                        
         DC    CL2'OV'                                                          
         DC    C'OVERCOPY,BOOK=NAME'                                            
ACTOVCX  DS    0H                                                               
ACTPRI   DC    AL1(ACTPRIX-*,10,PRI,MESSAGE+BOOK+PRINTYPE)                      
         DC    CL2'PR'                                                          
         DC    C'PRINT(,SPACE=1/2/3)(,REF=NO)'                                  
ACTPRIX  DS    0H                                                               
ACTSEN   DC    AL1(ACTSENX-*,10,SEN,MESSAGE+BOOK+PRINTYPE+WRITACC)              
         DC    CL2'SE'                                                          
         DC    C'SEND,TO=A<N,CCNAME,CCNAME>+B-C(,ACCESS=WRITE)'                 
         DC    C'(,SPACE=1/2/3)(,REF=NO)'                                       
ACTSENX  DS    0H                                                               
ACTASE   DC    AL1(ACTASEX-*,04,ASE,MESSAGE+BOOK+WRITACC+PRINTYPE)              
         DC    CL2'AS'                                                          
         DC    C'ADDSEND,TO=A<N,CCNAME,CCNAME>+B-C(,ACCESS=WRITE)'              
         DC    C'(,SPACE=1/2/3)(,REF=NO)'                                       
ACTASEX  DS    0H                                                               
ACTAUT   DC    AL1(ACTAUTX-*,10,AUT,FORMAT+MESSAGE+BOOK+WRITACC)                
         DC    CL2'AU'                                                          
         DC    C'AUTHORIZE,ACCESS=WRITE/READ/NONE,USER=A+B-C'                   
ACTAUTX  DS    0H                                                               
ACTMOD   DC    AL1(ACTMODX-*,02,MOD,FORMAT+MESSAGE+DDSONLY)                     
         DC    CL2'MO'                                                          
         DC    C'MODE=FORMAT/MESSAGE'                                           
ACTMODX  DS    0H                                                               
ACTHEL   DC    AL1(ACTHELX-*,02,HEL,FORMAT+MESSAGE)                             
         DC    CL2'HE'                                                          
         DC    C'HELP(,REFERENCE OR BOOK OR FORMAT)'                            
ACTHELX  DS    0H                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              ACTION PARAMETER TABLE COVERED BY DSECT PARMD                    
*                                                                               
PARMTAB  DS    0C                                                               
PARB00   DC    AL1(PARBOX-*),CL9'BOOK     ',AL2(ID-GWS),AL4(CHKID)              
         DC    AL1(BOO,COP,OVC,0)                                               
PARBOX   DS    0H                                                               
PARFOR   DC    AL1(PARFOX-*),CL9'FORMAT   ',AL2(FORMID-GWS),AL4(CHKID)          
         DC    AL1(BOO,FOR,INM,0)                                               
PARFOX   DS    0H                                                               
PARMOD   DC    AL1(PARMOX-*),CL9'MODE     ',AL2(NEWL-GWS),AL4(CHKOLD)           
         DC    AL1(MOD,0)                                                       
PARMOX   DS    0H                                                               
PARADD   DC    AL1(PARADX-*),CL9'ADD      ',AL2(ADD-GWS),CL4'Y'                 
         DC    AL1(BOO,0)                                                       
PARADX   DS    0H                                                               
PARDEL   DC    AL1(PARDEX-*),CL9'DELETE   ',AL2(DEL-GWS),CL4'Y'                 
         DC    AL1(BOO,0)                                                       
PARDEX   DS    0H                                                               
PARCOM   DC    AL1(PARCOX-*),CL9'COMMENT  ',AL2(COMMENT-GWS),AL1(16)            
         DC    AL3(0)                                                           
         DC    AL1(BOO,0)                                                       
PARCOX   DS    0H                                                               
PARREF   DC    AL1(PARREX-*),CL9'REFERENCE',AL2(REFLO-GWS),AL4(CHKREF)          
         DC    AL1(FOR,ADF,DIF,DIM,CHF,CHM,DEF,DEM,INF,INM,REP,0)               
PARREX   DS    0H                                                               
PARRF2   DC    AL1(PARR2X-*),CL9'REFERENCE',AL2(REFLO-GWS),AL1(1)               
         DC    AL3(0)                                                           
         DC    AL1(PRI,SEN,ASE,HEL,0,0)                                         
PARR2X   DS    0H                                                               
PARNUM   DC    AL1(PARNUX-*),CL9'MAXIMUM  ',AL2(NUM-GWS),AL4(CHKNUM)            
         DC    AL1(FOR,ADM),AL2(0)                                              
PARNUX   DS    0H                                                               
PARNEXT  DC    AL1(PARNEX-*),CL9'NEXT     ',AL2(NEXT-GWS),CL4'Y'                
         DC    AL1(DIF,DIM,HEL,0)                                               
PARNEX   DS    0H                                                               
PARNID   DC    AL1(PARNIX-*),CL9'NEXT     ',AL2(ID-GWS),AL4(CHKID)              
         DC    AL1(ADM,0)                                                       
PARNIX   DS    0H                                                               
PARLAST  DC    AL1(PARLAX-*),CL9'LAST     ',AL2(LAST-GWS),CL4'Y'                
         DC    AL1(ADF,ADM,DIF,DIM,CHF,DEF,CHM,0)                               
PARLAX   DS    0H                                                               
PAREND   DC    AL1(PARENX-*),CL9'END      ',AL2(ENDMK-GWS),CL4'Y'               
         DC    AL1(ADM,0)                                                       
PARENX   DS    0H                                                               
PAROLD   DC    AL1(PAROLX-*),CL9'OLD      ',AL2(OLDL-GWS),AL4(CHKOLD)           
         DC    AL1(REP,0)                                                       
PAROLX   DS    0H                                                               
PARNEW   DC    AL1(PARNWX-*),CL9'NEW      ',AL2(NEWL-GWS),AL4(CHKOLD)           
         DC    AL1(REP,0)                                                       
PARNWX   DS    0H                                                               
PARTO    DC    AL1(PARTOX-*),CL9'TO       ',AL2(ADDS-GWS),AL4(CHKADDS)          
         DC    AL1(SEN,ASE),AL2(0)                                              
PARTOX   DS    0H                                                               
PARACC   DC    AL1(PARCCX-*),CL9'ACCESS   ',AL2(ACCESS-GWS),AL4(CHKACC)         
         DC    AL1(AUT,SEN,ASE,0)                                               
PARCCX   DS    0H                                                               
PARUSER  DC    AL1(PARUSX-*),CL9'USER     ',AL2(ADDS-GWS),AL4(CHKADDS)          
         DC    AL1(AUT,0)                                                       
PARUSX   DS    0H                                                               
PARRET   DC    AL1(PARRTX-*),CL9'RETENTION',AL2(NUM-GWS),AL4(CHKNUM)            
         DC    AL1(BOO,0)                                                       
PARRTX   DS    0H                                                               
PARSPACE DC    AL1(PARSPX-*),CL9'SPACE    ',AL2(SPACE-GWS),AL1(1)               
         DC    AL3(0)                                                           
         DC    AL1(PRI,SEN,ASE,0)                                               
PARSPX   DS    0H                                                               
PARCHECK DC    AL1(PARCHX-*),CL9'CHECK    ',AL2(CHCKMK-GWS),AL1(1)              
         DC    AL3(0)                                                           
         DC    AL1(END,0)                                                       
PARCHX   DS    0H                                                               
PARBO2   DC    AL1(PARB2X-*),CL9'BOOK     ',AL2(ID-GWS),CL4'Y'                  
         DC    AL1(HEL,0)                                                       
PARB2X   DS    0H                                                               
PARFO2   DC    AL1(PARF2X-*),CL9'FORMAT   ',AL2(FORMID-GWS),CL4'Y'              
         DC    AL1(HEL,0)                                                       
PARF2X   DS    0H                                                               
PARCLASS DC    AL1(PARCLX-*),CL9'CLASS    ',AL2(CLASS-GWS),AL1(L'CLASS)         
         DC    AL3(0)                                                           
         DC    AL1(BOO,0)                                                       
PARCLX   DS    0H                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
COMMON   DC    A(GETFORM)                                                       
         DC    A(GETCHNK)                                                       
         DC    A(VALMESS)                                                       
         DC    A(DISMESS)                                                       
         DC    A(ADDCHNK)                                                       
         DC    A(PUTCHNK)                                                       
         DC    A(DELCHNK)                                                       
         DC    A(DISFORM)                                                       
         DC    A(SETSCRN)                                                       
         DC    A(DISSCRN)                                                       
         DC    A(EDITREF)                                                       
         DC    A(TWATERM)                                                       
         DC    A(RULETAB)                                                       
         DC    A(FINDBK)                                                        
         DC    A(CLOSE)                                                         
         DC    A(ACTAB)                                                         
*                                                                               
NUMHDRS  DC    H'1'                NUMBER OF HEADER RECS PER BOOK               
SCRLMAX  DC    H'22'               MAXIMUM NUMBER OF LINES IN SCREEN            
FRMDFLT  DS    0CL94               DEFAULT FORMAT - WITH REF NOS                
         DC    AL2(94),X'50800001',76X'00'                                      
         DC    X'0B20004C80',3X'00',C'&&NO',X'00'                               
FRMFORM  DS    0CL89               FORMAT FORMAT - NO REF NOS                   
         DC    AL2(89),X'56800001',83X'00'                                      
LIBRARY  DC    CL8'LIBRARY'                                                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GEKWXDSECT                                                     
               EJECT                                                            
*              DSECT TO COVER AN ENTRY IN ACTION TABLE ACTAB                    
*                                                                               
ACTIOND  DSECT                                                                  
ACTNLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
ACTNOV   DS    CL1       B         OVERLAY NUMBER                               
ACTNNUM  DS    CL1       B         ACTION NUMBER (EQUATED)                      
ACTNSTAT DS    CL1       X         ACTION STATUS                                
*                                  X'80' = FORMAT   (COMPATIBLE MODE)           
*                                  X'40' = MESSAGE  (DITTO)                     
*                                  X'20' = BOOK     (REQUIRES BOOK)             
*                                  X'10' = WRITE    (UPDATES)                   
*                                  X'08' = PRINT    (WRITES TO PQ)              
*                                                                               
*                                                                               
ACTNSHRT DS    CL2       C         2-CHARACTER ACTION CODE                      
ACTNFULL DS    0C        C         FULL ACTION SYNTAX (FOR HELP)                
*                                                                               
         SPACE 2                                                                
*              DSECT TO COVER AN ENTRY IN ACTION PARAMETER TABLE PARTAB         
*                                                                               
PARMD    DSECT                                                                  
PARMLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
PARMWORD DS    CL9       C         PARAMETER KEYWORD                            
PARMDEST DS    CL2       B         DISPLACEMENT FROM START OF W/S OF            
*                                  ADDRESS OF PROCESSED PARAMETER VALUE         
PARMSR   DS    CL4       B         B0 = 0    - A(VALIDATE/CONVERT SR)           
*                                  B0 = C'Y' - MOVE C'Y' TO OUTPUT              
*                                  ELSE      - MOVE IN TO OUT FOR L'B0          
PARMACTS DS    0C        B         STRING OF COMPATIBLE ACTNNUMS ENDED          
*                                  BY ZERO                                      
*              NESTED INCLUDES                                                  
         SPACE 1                                                                
* CTGENFILE                                                                     
* DDCOMFACS                                                                     
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043GEKWX00T  05/01/02'                                      
         END                                                                    
