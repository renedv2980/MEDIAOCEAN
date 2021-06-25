*          DATA SET TAREPGEN   AT LEVEL 032 AS OF 01/21/16                      
*PHASE T00A89C,*                                                                
         TITLE 'T00A89 - TALENT WRITER GENERAL ROUTINES'                        
         PRINT NOGEN                                                            
T00A89   CSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
GEN      NTR1  WORK=(R5,10)                                                     
         DROP  RF                                                               
         USING TREPGEND,R5        NEED TO GET RID OF OTHER REFERENCE            
         LR    RB,RF                                                            
         USING T00A89,RB,R7,R6                                                  
         B     *+12                                                             
         DC    CL8'**GEN***'                                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         RELOC RELO                                                             
         SPACE 1                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VWRIUSER                                                         
         B     VVALOPTS                                                         
         B     VVALFILT                                                         
         B     VVALTITS                                                         
         B     VVALPERD                                                         
         B     VWILDINV                                                         
         DC    20X'00'                                                          
         SPACE 1                                                                
         B     VVALLEFT                                                         
         B     VVALRGHT                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         DC    24X'00'                                                          
         SPACE 1                                                                
         B     VINTDRIV                                                         
         B     VINTDRON                                                         
         B     VWRPDRON                                                         
         B     VINTHEAD                                                         
         DC    8X'00'                                                           
         SPACE 1                                                                
         B     VGENHEAD                                                         
         B     VINITIAL                                                         
         DS    20X'00'                                                          
         SPACE 1                                                                
         B     VNUMERIC                                                         
         B     VPACK                                                            
         DC    24X'00'                                                          
         SPACE 1                                                                
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         DC    24X'00'                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         GETEL (R3),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              USER ID RECORD                                                   
         SPACE 3                                                                
VWRIUSER DS    0H                                                               
         L     RE,TASYSTAB         PICK UP A(GLOBAL TABLES)                     
         LA    RF,TGTABLES                                                      
         LA    R0,NTABLS                                                        
         XC    DMCB,DMCB                                                        
VUSER0   L     R1,TASYSTAB         RELOCATE TABLE ENTRIES                       
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,VUSER0                                                        
         SPACE 1                                                                
*                                                                               
         BRAS  RE,SETSYS           SET SYSTEM (FQA/CSC/TST)                     
*                                                                               
         L     R1,SYSPARMS                                                      
*        MVC   AGENCY,0(R1)                                                     
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
         SPACE 1                                                                
VUSER1   CLI   0(R4),0                                                          
         BE    VUSER3                                                           
         CLI   0(R4),X'30'                                                      
         BE    VUSER2A                                                          
         CLI   0(R4),X'02'                                                      
         BE    VUSER2B                                                          
         CLI   0(R4),X'06'                                                      
         BE    VUSER2C                                                          
         CLI   0(R4),X'21'                                                      
         BE    VUSER2D                                                          
         SPACE 1                                                                
VUSER1A  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER1                                                           
         SPACE 1                                                                
         USING CTDSTD,R4                                                        
VUSER2A  MVC   USERNAME,CTDSTNAM                                                
         MVC   USERADDR,CTDSTADD                                                
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTDSCD,R4                                                        
VUSER2B  MVC   AGYSIGN,CTDSC                                                    
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTAGYD,R4                                                        
VUSER2C  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTSYSD,R4                                                        
VUSER2D  CLI   CTSYSNUM,7          INSURE THIS ONE'S FOR TALENT                 
         BNE   VUSER1A                                                          
         OC    TWAACCS(2),TWAACCS  ACCESS                                       
         BNZ   *+10                                                             
         MVC   TWAACCS,CTSYSLMT                                                 
         B     VUSER1A                                                          
         SPACE 1                                                                
VUSER3   GOTO1 GETFACT,DMCB,0                                                   
         L     R4,0(R1)                                                         
         USING FACTSD,R4                                                        
         MVC   TGLINEID,FALINE                                                  
         MVC   SYSID,FASYSID                                                    
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    VUSER4C                                                          
         CLI   TWAFIRST,0          AND THIS IS THE FIRST TIME IN ...            
         BNE   VUSER4C                                                          
         SPACE 1                                                                
         XC    KEY,KEY             GET DDS PID FROM PASSWORD RECORD             
         LA    R2,KEY                                                           
         USING SA0REC,R2                                                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,FATAGYSC    SAVE AGENCY CODE FOR SECURITY                
         MVC   SA0KNUM,FAPASSWD    AND PASSWORD ID NUMBER                       
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         GOTO1 HIGH                                                             
         MVI   USEIO,C'N'                                                       
         CLC   KEY(L'SA0KEY),KEYSAVE                                            
         BNE   VUSER4C                                                          
         SPACE 1                                                                
         L     R2,AIO                                                           
         LA    R2,SA0DATA                                                       
         MVI   ELCODE,SAPALELQ                                                  
         USING SAPALD,R2                                                        
VUSER4A  CLI   0(R2),0                                                          
         BE    VUSER4C                                                          
         CLI   0(R2),SAPALELQ                                                   
         BE    VUSER4B                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     VUSER4A                                                          
VUSER4B  MVC   TGCTSTAF,SAPALPID   SAVE PID AS CONNECT STAFF ID                 
         SPACE 1                                                                
VUSER4C  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         SPACE 1                                                                
         LR    RE,R8                                                            
         AHI   RE,STF2AREA-WKST                                                 
         ST    RE,TGAS2ACC        SAVE A(STAFF LIMIT BLOCK)                     
         SPACE 1                                                                
         MVC   TGSYSTAB,TASYSTAB                                                
         GOTO1 SYSVINIT,DMCB,CONSTAFH,CONPASSH  SYSTEM INITIALIZATION           
         BE    VUSER6                                                           
         CLI   SYSID,1             DON'T CARE ABOUT PASSWORDS IN TST            
         BE    VUSER6                                                           
         B     VUSER6              *** NO-OP UNTIL TP IS READY ***              
         B     PASSEXP             PASSWORD HAS EXPIRED                         
         SPACE 1                                                                
VUSER6   CLI   OFFLINE,C'Y'        IF WE'RE OFFLINE GENCON WILL DO IT           
         BE    *+8                                                              
         MVI   TWAFIRST,1          ELSE SET NO LONGER FIRST TIME IN             
*                                                                               
         BRAS  RE,LKUPRACT         LOOKUP REC/ACT FOR TALX <> TAL1              
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE OVERLAY                                               
         SPACE 2                                                                
VINITIAL DS    0H                                                               
         CLI   OFFLINE,C'Y'        IF WE'RE OFFLINE                             
         BNE   INIT10                                                           
         CLI   MODE,VALKEY         AND WE'RE ABOUT TO VALIDATE KEY              
         BE    *+12                                                             
         CLI   MODE,VALREC         (TEST THIS MODE IN CASE PROG REC.)           
         BNE   INIT05                                                           
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         L     R1,MCVREMOT                                                      
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   AND GOING TO PQ                              
         BZ    INIT05                                                           
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTKEY(8),CONREC  SET REPORT NAME                              
         OC    REMOTKEY(8),SPACES                                               
INIT05   B     INITX                                                            
         DROP  R1                                                               
         SPACE 1                                                                
INIT10   CLI   ACTNUM,ACTREP       ELSE IF ACTION IS REPORT                     
         BE    INIT20                                                           
         CLI   ACTNUM,ACTSEL       OR ACTION IS SELECT                          
         BNE   INITX                                                            
         CLI   THISLSEL,C'Q'       AND REPORT IS SELECTED                       
         BNE   INITX                                                            
         SPACE 1                                                                
INIT20   CLI   MODE,VALKEY         AND WE'RE ABOUT TO VALIDATE KEY              
         BE    *+12                                                             
         CLI   MODE,VALREC         (TEST THIS MODE IN CASE PROG REC.)           
         BNE   INITX                                                            
         TM    WHEN,X'38'          AND THIS IS A REQ FOR OFFLINE REPORT         
         BZ    INITX                                                            
         OC    TGCTSTAF,TGCTSTAF   ****** TEMP CODE TO TRAP BUG *****           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CONKEY(8),TGCTSTAF  MOVE STAFF CODE TO KEY FIELD SO THAT         
         MVI   CONKEYH+5,8         IT GETS PASSED THROUGH TO REQUEST            
         SPACE 1                                                                
INITX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FILTERS                                                 
         SPACE 3                                                                
VVALFILT CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK)                                 
         MVI   FIELDERR,1                                                       
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
FILT2    ZIC   R1,0(R4)            L'FILTER EXPRESSION                          
         BCTR  R1,0                                                             
         L     R3,=A(FILTTAB)                                                   
         A     R3,RELO                                                          
         SPACE 1                                                                
FILT4    EX    R1,*+8              TEST AGAINST LIST                            
         B     *+10                                                             
         CLC   0(0,R3),12(R4)                                                   
         BE    FILTSET                                                          
         LA    R3,L'FILTENT(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BNE   FILT4                                                            
         SPACE 1                                                                
         CLC   12(4,R4),=C'CONV'   CONVERTED FILTER                             
         BNE   FILT6                                                            
         MVI   TIFINCVS,1                                                       
         CLI   22(R4),C'N'                                                      
         BE    FILTENDX                                                         
         MVI   TIFINCVS,2                                                       
         CLI   22(R4),C'Y'                                                      
         BE    FILTENDX                                                         
         B     BADFILT                                                          
*                                                                               
FILT6    CLC   12(5,R4),=C'PURGE'  FILTER ON PURGED RECORDS ONLY                
         BNE   *+12                                                             
         OI    TIFSTAT,TIFSPRG                                                  
         B     FILTENDX                                                         
*                                                                               
         CLC   12(6,R4),=C'PRGERR' FILTER ON PURGE ERROR RECORDS ONLY           
         BNE   *+12                                                             
         OI    TIFSTAT,TIFSPRGE                                                 
         B     FILTENDX                                                         
*                                                                               
FILT8    CLC   12(6,R4),=C'CLANET' FILTER ON CLA NETWORK                        
         BNE   FILT10                                                           
         MVC   TIFLTNET,22(R4)                                                  
         B     FILTENDX                                                         
*                                                                               
FILT10   CLC   12(6,R4),=C'CAUNIT' FILTER ON CA CHECKS                          
         BNE   FILT15                                                           
         OI    TIFLTSPC,TIFLTSCA                                                
         B     FILTENDX                                                         
*                                                                               
FILT15   CLC   12(6,R4),=C'OVERAGE' FILTER ON OVERAGE GUARANTEES                
         BNE   FILT20                                                           
         CLI   22(R4),C'N'                                                      
         BNE   *+8                                                              
         OI    TIFLTSPC,TIFLTSON      JUST OVERAGE=N                            
*                                                                               
         CLI   22(R4),C'Y'                                                      
         BNE   *+8                    DEFAULT ALL                               
         OI    TIFLTSPC,TIFLTSOY      JUST OVERAGE=Y                            
         B     FILTENDX                                                         
*                                                                               
FILT20   CLC   12(4,R4),=C'NHA '   FILTER ON NEW HIRE ACT                       
         BNE   FILT25                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   *+8                                                              
         OI    TIFLTSP2,TIFLTS2A      JUST ELIGIBLE                             
*                                                                               
         CLI   22(R4),C'P'                                                      
         BNE   *+8                    DEFAULT ALL                               
         OI    TIFLTSP2,TIFLTS2P      JUST PENDING                              
         B     FILTENDX                                                         
*                                                                               
FILT25   CLC   12(4,R4),=C'MSC '   FILTER ON MULTI SERVICE CONTRACTS            
         BNE   FILT28                                                           
         OI    TIFLTSP2,TIFLTS2M                                                
         B     FILTENDX                                                         
*                                                                               
FILT28   CLC   12(5,R4),=C'VITAB'  FILTER ON VITA BETA AGENCIES                 
         BNE   FILT30                                                           
         OI    TIFLTSP2,TIFLTS2V                                                
         B     FILTENDX                                                         
*                                                                               
FILT30   CLC   12(4,R4),=C'CIHR'   FILTER ON CIHR AGENCIES / CLIENTS            
         BNE   BADFILT                                                          
         OI    TIQFLAG4,TIQFCIHR                                                
         B     FILTENDX                                                         
*                                                                               
BADFILT  MVC   CONHEAD(L'FLTERR),FLTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LSTERR),LSTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLONG  MVC   CONHEAD(L'LNGERR),LNGERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
FILTSET  MVC   TIFLTLVL,12(R3)     SET FILTER AT RECORD FOR EXACT MATCH         
         LA    RE,TIFINSTY         SPECIAL TESTS FOR INVOICE STATUS             
         CLI   11(R3),101                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFINS2Y                                                      
         CLI   11(R3),102                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPDPY                                                       
         CLI   11(R3),103                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPDSY                                                       
         CLI   11(R3),104                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPO1Y                                                       
         CLI   11(R3),105                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPO2Y                                                       
         CLI   11(R3),106                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPO3Y                                                       
         CLI   11(R3),107                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFPAIY                                                       
         CLI   11(R3),108                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFCDSTY                                                      
         CLI   11(R3),109                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFDUSTY                                                      
         CLI   11(R3),131                                                       
         BE    FILTSTAT                                                         
         LA    RE,TIFCOSTY                                                      
         CLI   11(R3),132                                                       
         BE    FILTSTAT                                                         
         CLI   11(R3),110                                                       
         BE    FILTRUN                                                          
         CLI   11(R3),120                                                       
         BE    FILTCUST                                                         
         SPACE 1                                                                
         CLI   11(R3),133          IF INVOICE FILTER                            
         BNE   FILTSET1                                                         
         CLI   22(R4),C'-'         GIVE ERROR IF NEGATIVE LISTS/FILTERS         
         BE    BADFILT                                                          
         CLC   22(2,R4),=C'@-'                                                  
         BE    BADFILT                                                          
         CLI   22(R4),C'@'         IF FLIST                                     
         BNE   FILTSET0                                                         
         LA    RE,TIFINVD          SET RE=A(TIFINVD)                            
         LA    RF,L'TIFINVD        SET RF=LENGTH-1                              
         BCTR  RF,0                                                             
         B     FILTSET4            THEN HANDLE ELSEWHERE                        
FILTSET0 MVC   TIFINVD,22(R4)      ELSE IT'S A REGULAR FILTER                   
         GOTO1 WILDINV             HANDLE WILDCARDS & CONV TO INTERNAL          
         BNE   BADFILT             GIVE ERROR IF INVALID INVOICE INPUT          
         B     FILTENDX                                                         
         SPACE 1                                                                
FILTSET1 CLC   =CL8'PERF',0(R3)    IF PERF TABLE ENTRY                          
         BNE   *+14                                                             
         CLC   =CL8'PERF',12(R4)   ENSURE USER INPUT EXACTLY SAME               
         BNE   BADFILT                                                          
         SPACE 1                                                                
         CLC   12(4,R4),=C'UNIT'   UNIT = BOTH STATE AND CITY                   
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTTAX+TIFTCITY+TIFTSTTE                               
         CLC   12(6,R4),=C'TSTATE' ADJUSTMENTS FOR STATE TYPE                   
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTTAX+TIFTSTTE                                        
         CLC   12(5,R4),=C'STATE'                                               
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTTAX+TIFTSTTE                                        
         CLC   12(6,R4),=C'RSTATE'                                              
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTRES+TIFTSTTE                                        
         CLC   12(6,R4),=C'WSTATE'                                              
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTWRK+TIFTSTTE                                        
         SPACE 1                                                                
         CLC   12(5,R4),=C'TCITY'  ADJUSTMENTS FOR CITY TYPE                    
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTTAX+TIFTCITY                                        
         CLC   12(4,R4),=C'CITY'                                                
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTTAX+TIFTCITY                                        
         CLC   12(5,R4),=C'RCITY'                                               
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTRES+TIFTCITY                                        
         CLC   12(5,R4),=C'WCITY'                                               
         BNE   *+8                                                              
         OI    TIFTUNIT,TIFTWRK+TIFTCITY                                        
         SPACE 1                                                                
         LH    RE,8(R3)            RELATIVE DISPLACEMENT IN FILTERS             
         LA    RE,TIFILTS(RE)      (NOW HAVE THE ADDRESS)                       
         LH    RF,10(R3)           RF=LENGTH                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       IF PRIOR DATA IN THIS FIELD                  
         BNZ   BADFILT             GIVE ERROR                                   
         SPACE 1                                                                
         CLC   22(2,R4),=C'-@'     CHECK FOR NEGATIVE LISTS                     
         BE    FILTSET6                                                         
         CLC   22(2,R4),=C'@-'                                                  
         BE    FILTSET6                                                         
         CLI   22(R4),C'-'         THEN FOR NEGATIVE FILTER                     
         BE    FILTSET2                                                         
         CLI   22(R4),C'@'         OR LIST FILTERS                              
         BE    FILTSET4                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(R4)      THEN IT'S A REGULAR FILTER                   
                                                                                
         CLC   12(4,R4),=C'UNIT'   UNIT AND NOT FLIST                           
         BNE   FILTEND                                                          
         ZIC   RF,1(R4)            LENGTH OF 2ND HALF INPUT                     
         CHI   RF,3                UNIT IS A LOCAL                              
         BE    *+12                YES                                          
         NI    TIFTUNIT,X'FF'-TIFTCITY    NO, DON'T WANT CITIES                 
         B     FILTEND                                                          
         NI    TIFTUNIT,X'FF'-TIFTSTTE    YES, DON'T WANT STATES                
         B     FILTEND                                                          
         SPACE 1                                                                
FILTSET2 EX    RF,*+8              NEGATIVE FILTERS                             
         B     *+10                                                             
         MVC   0(0,RE),23(R4)                                                   
         NI    0(RE),X'FF'-X'40'   40 BIT OFF IN FIRST BYTE                     
         B     FILTEND                                                          
         SPACE 1                                                                
FILTSET4 EX    RF,*+8              LIST FILTERS                                 
         B     *+10                                                             
         MVC   0(0,RE),23(R4)                                                   
         MVC   WORK(8),23(R4)                                                   
         NI    0(RE),X'FF'-X'80'   80 BIT OFF IN FIRST BYTE                     
         B     FILTSET8                                                         
         SPACE 1                                                                
FILTSET6 EX    RF,*+8              NEGATIVE LISTS                               
         B     *+10                                                             
         MVC   0(0,RE),24(R4)                                                   
         MVC   WORK(8),24(R4)                                                   
         NI    0(RE),X'FF'-X'80'-X'40'    BOTH TOP BITS OFF                     
         SPACE 1                                                                
FILTSET8 LA    R3,WORK+1(RF)       CHECK CODE IS NOT TOO LONG                   
         CLI   0(R3),C' '                                                       
         BH    BADLONG                                                          
         XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
**NO-OP* MVC   TLGLLST,SPACES      COMMENTED OUT 10/27/94                       
**NO-OP* EX    RF,*+8                                                           
**NO-OP* B     *+10                                                             
**NO-OP* MVC   TLGLLST(0),WORK                                                  
         MVC   TLGLLST,WORK                                                     
         OC    TLGLLST,SPACES                                                   
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADLIST                                                          
         B     FILTEND                                                          
         SPACE 1                                                                
FILTSTAT CLI   22(R4),C'N'         STATUS - NEGATIVE OPTION                     
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         OC    0(1,RE),9(R3)                                                    
         B     FILTENDX                                                         
         SPACE 1                                                                
FILTRUN  LA    R3,WORK             RUN=DATE EXPRESSION                          
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB                                                
         LA    RE,22(R4)                                                        
         ST    RE,DMCB+4                                                        
         MVC   DMCB+4(1),1(R4)     L'DATE EXPRESSION                            
         GOTO1 PDVAL,DMCB,(X'40',(R3))                                          
         MVC   TIQRSTR,PVALPSTA                                                 
         MVC   TIQREND,PVALPEND                                                 
         B     FILTENDX                                                         
         SPACE 1                                                                
FILTCUST MVC   TIFCUST,9(R3)       CUSTOM FILTERS                               
         CLI   TIFCUST,20          IF FILTER CRPNOIND                           
         BNE   *+8                                                              
         MVI   TIFW4TY,TAW4TYCO    SET WANT PAYMENTS TO CORP ONLY               
         CLI   TIFCUST,34          IF FILTER TRSNOIND                           
         BNE   *+8                                                              
         MVI   TIFW4TY,TAW4TYTR    SET WANT PAYMENTS TO TRUSTEES ONLY           
         CLI   TIFCUST,0           ROUTINE# COMES FROM LIST                     
         BNE   FILTENDX                                                         
         MVC   TIFCUST,11(R4)      OR IS INPUT W/CUSTOM=NNN(USE BINARY)         
         CLI   TIFCUST,0                                                        
         BNE   FILTENDX                                                         
         B     BADFILT                                                          
         SPACE 1                                                                
FILTEND  CLC   12(2,R4),=C'ID'     ADJUST ID FILTER                             
         BNE   FILTENDX                                                         
         CLI   TIFID+3,C' '                                                     
         BNE   FILTENDX                                                         
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(3),TIFID                                                  
         MVZ   WORK(1),WORK+1                                                   
         OI    WORK+1,X'40'                                                     
         SPACE 1                                                                
FILTENDX LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,FILT2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PERIOD FIELDS                                           
         SPACE 3                                                                
VVALPERD XC    TIQPSTR,TIQPSTR                                                  
         XC    TIQPEND,TIQPEND                                                  
         XC    APERFLD,APERFLD                                                  
         XC    WORK,WORK                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         ST    R2,APERFLD                                                       
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         XC    PVALOUTB,PVALOUTB                                                
         GOTO1 PDVAL,DMCB,PVALOUTB                                              
         MVC   TIQPSTR,PVALPSTA                                                 
         MVC   TIQPEND,PVALPEND                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES WILDCARDS IN TIFINVD AND SETS TIQSTART           
*              WITH INTERNAL INV FORMAT OF TIFINVD                              
         SPACE 3                                                                
VWILDINV DS    0H                                                               
         CLI   TIFINVD+1,C'*'      IF HAVE WILDCARD IN YEAR                     
         BNE   *+14                                                             
         MVC   TIFINVD,WILDCRDS    REPLACE ALL OF TIFINVD WITH *                
         B     WLDI2                                                            
         CLI   TIFINVD,C'*'          IF HAVE WILDCARD IN MONTH                  
         BNE   *+14                                                             
         MVC   TIFINVD+2(4),WILDCRDS REPLACE END OF TIFINVD WITH *              
         B     WLDI2                                                            
         CLI   TIFINVD+2,C'*'        IF HAVE WILDCARD IN 1ST 2 INV NUMS         
         BE    *+12                                                             
         CLI   TIFINVD+3,C'*'                                                   
         BNE   *+14                                                             
         MVC   TIFINVD+2(4),WILDCRDS REPLACE END OF TIFINVD WITH *              
         B     WLDI2                                                            
         CLI   TIFINVD+4,C'*'        IF HAVE WILDCARD IN LST 2 INV NUMS         
         BE    *+12                                                             
         CLI   TIFINVD+5,C'*'                                                   
         BNE   *+10                                                             
         MVC   TIFINVD+4(2),WILDCRDS REPLACE END OF TIFINVD WITH *              
         SPACE                                                                  
WLDI2    LA    R0,L'TIFINVD        R0=L'TIFINVD                                 
         LA    R2,TIFINVD          R2=A(POS IN TIFINVD BEING CHECKED)           
         LA    R3,DUB              R3=A(POS IN DUB BEING SET)                   
WLDI5    CLI   0(R2),C'*'          IF WILDCARD                                  
         BNE   *+12                                                             
         BAS   RE,REPLACE          REPLACE WITH APPROPRIATE CHAR                
         B     *+10                                                             
         MVC   0(1,R3),0(R2)       ELSE MOVE INTO POS IN DUB                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,WLDI5            LOOP FOR L'TIFINVD                           
         SPACE                                                                  
         GOTO1 TINVCON,DMCB,DUB,TIQSTART,DATCON  SET TIQSTART                   
         CLI   0(R1),X'FF'                                                      
         BE    NO                  IF ERROR FROM TINVCON, SET CC NO             
         CLI   TIREAD,TLINCDQ      IF DOING AN INV READ (INV# COMPL)            
         BNE   *+10                                                             
         XC    TIQSTART,=6X'FF'    COMPLEMENT TIQSTART                          
         B     YES                 SET CC YES                                   
         SPACE 3                                                                
*              ROUTINE REPLACES WILDCARDS WITH THE APPROPRIATE CHAR             
REPLACE  DS    0H                                                               
         CHI   R0,6                IF MONTH HAS WILDCARD                        
         BNE   REPL5                                                            
         MVI   0(R3),C'A'          REPLACE WITH "A" IN DUB                      
         CLI   TIREAD,TLCKCDQ      IF DOING A CHECK READ (INV# UNCOMPL)         
         BER   RE                                                               
         MVI   0(R3),C'L'          ELSE REPLACE WITH "L" IN DUB                 
         BR    RE                                                               
REPL5    CLI   TIREAD,TLINCDQ      IF DOING AN INV READ                         
         BNE   *+10                                                             
         MVI   0(R3),C'9'          REPLACE WITH "9" IN DUB                      
         BR    RE                                                               
         MVI   0(R3),C'0'          ELSE REPLACE WITH "0" IN DUB                 
         BR    RE                                                               
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VVALOPTS DS    0H                                                               
         MVI   BOXOPT,C'Y'         PRESET VALUES                                
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   DOWNOPT,0                                                        
         XC    MYRNKMAX,MYRNKMAX                                                
         MVI   DRINDS,0                                                         
         MVI   DRINDS2,0                                                        
         MVI   FISCOPT,1                                                        
         MVI   THOUOPT,C'N'                                                     
         MVI   WIDEOPT,C'N'                                                     
         MVI   TRACEOPT,C'N'                                                    
         MVI   OUTOPTS,0                                                        
         MVI   GRANDOPT,C'N'                                                    
         MVI   NARROPT,C'N'                                                     
         MVI   TESTOPT,0                                                        
         MVI   PEROPT,0                                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   FIELDERR,1                                                       
         GOTO1 SCANNER,DMCB,(40,(R2)),(7,BLOCK),0                               
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT6                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'DOWN '  DOWNLOADING OPTION                           
         BNE   OPT8B                                                            
         OI    DOWNOPT,GLDLACTV                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8B    CLC   12(6,R4),=C'NOHEAD' NO HEADINGS IN DL REPORT                     
         BNE   OPT9                                                             
         OI    DOWNOPT,GLDLNOHD                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(3,R4),=C'MAX'    MAX OPTION FOR RANKING                       
         BNE   OPT9B                                                            
         MVC   MYRNKMAX,8(R4)                                                   
         OC    MYRNKMAX,MYRNKMAX                                                
         BZ    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9B    CLC   12(3,R4),=C'TOP'    TOP OPTION FOR RANKING                       
         BNE   OPT10                                                            
         MVC   MYRNKMAX,8(R4)                                                   
         OC    MYRNKMAX,MYRNKMAX                                                
         BZ    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(6,R4),=C'FISCAL' FISCAL=1-12 OPTION                           
         BNE   OPT12                                                            
         L     R1,8(R4)                                                         
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CH    R1,=H'12'                                                        
         BH    BADOPT                                                           
         LA    R1,FISCLIST-1(R1)                                                
         MVC   FISCOPT,0(R1)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(4,R4),=C'THOU'   THOUSAND OPTION                              
         BNE   OPT13                                                            
         MVI   THOUOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT13    CLC   12(6,R4),=C'DOLLAR' DOLLAR OPTION                                
         BNE   OPT14                                                            
         MVI   THOUOPT,C'$'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(4,R4),=C'WIDE'   WIDE PRINTING (165)                          
         BNE   OPT16                                                            
         MVI   WIDEOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT18                                                            
         MVI   TRACEOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(7,R4),=C'BRACKET' OPTION                                      
         BNE   OPT18B                                                           
         OI    OUTOPTS,DRBKMINO                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18B   CLC   12(5,R4),=C'COMMAS ' OPTION                                      
         BNE   OPT18D                                                           
         OI    OUTOPTS,DRCOMMAO                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18D   CLC   12(5,R4),=C'ALIGN'   ALIGNMENT OPTIONS                           
         BNE   OPT18H                                                           
         CLI   22(R4),C'R'                                                      
         BE    OPT18F                                                           
         CLI   22(R4),C'L'                                                      
         BNE   BADOPT                                                           
         OI    OUTOPTS,DRALGNLO    LEFT                                         
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18F   OI    OUTOPTS,DRALGNRO    RIGHT                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18H   CLC   12(7,R4),=C'NOBLANK' ZERO=NOBLANK OPTION                         
         BNE   OPT20                                                            
         OI    OUTOPTS,DRZEROO                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(8,R4),=C'NOPERIOD' SUPPRESS PERIOD IN HEADLINES               
         BNE   OPT21                                                            
         MVI   PEROPT,C'N'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT21    CLC   12(5,R4),=C'GRAND'   GRAND TOTAL OPTION                          
         BNE   OPT22                                                            
         MVI   GRANDOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT22    CLC   12(6,R4),=C'NARROW'  OPTION                                      
         BNE   OPT24                                                            
         MVI   NARROPT,C'Y'                                                     
         MVI   LEFTOPT,C'Y'         FORCE LEFT OPTION                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT24    CLC   12(6,R4),=C'ALLDET'  OPTION TO GET ALL DETAILS                   
         BNE   OPT28                                                            
         OI    DRINDS,X'02'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT28    CLC   12(6,R4),=C'ALLTOT'  OPTION TO GET ALL TOTALS                    
         BNE   OPT30                                                            
         OI    DRINDS,X'04'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT30    CLC   12(4,R4),=C'TEST'    TEST OPTION                                 
         BNE   OPT32                                                            
         MVC   TESTOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT32    CLC   12(3,R4),=C'PER'     PERIOD OPTION                               
         BNE   OPT34                                                            
         MVI   TIQDTYPE,TIQDBILL                                                
         CLC   22(3,R4),=C'BIL'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDPAY                                                 
         CLC   22(3,R4),=C'PAY'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCHK                                                 
         CLC   22(3,R4),=C'CHE'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDLAST                                                
         CLC   22(3,R4),=C'LAS'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDDUE                                                 
         CLC   22(3,R4),=C'DUE'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCYCS                                                
         CLC   22(4,R4),=C'CYCS'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCYCE                                                
         CLC   22(4,R4),=C'CYCE'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDVADV                                                
         CLC   22(4,R4),=C'VADV'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDRADV                                                
         CLC   22(4,R4),=C'RADV'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDSADV                                                
         CLC   22(4,R4),=C'SADV'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDPADV                                                
         CLC   22(4,R4),=C'PADV'                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDEXP                                                 
         CLC   22(4,R4),=C'EXP '                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDASS                                                 
         CLC   22(4,R4),=C'ASS '                                                
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDAIR                                                 
         CLC   22(5,R4),=C'COAIR'                                               
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDEXP                                                 
         CLC   22(5,R4),=C'COEXP'                                               
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCFIL                                                
         CLC   22(6,R4),=C'CSFDATE'                                             
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCMUS                                                
         CLC   22(6,R4),=C'CSMDATE'                                             
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCREC                                                
         CLC   22(6,R4),=C'CSRDATE'                                             
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCOD                                                 
         CLC   22(3,R4),=C'PUR'                                                 
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDPDTE                                                
         CLC   22(6,R4),=C'COPDTE'                                              
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDRNR                                                 
         CLC   22(6,R4),=C'RNRDTE'                                              
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDRND                                                 
         CLC   22(6,R4),=C'RNDDTE'                                              
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDRGDV                                                
         CLC   22(5,R4),=C'RGADV'                                               
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDAHLD                                                
         CLC   22(6,R4),=C'HLDDTE'                                              
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDFCYC                                                
         CLC   22(6,R4),=C'COFCYC'                                              
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDVDTE                                                
         CLC   22(5,R4),=C'VDATE'                                               
         BE    OPTEND                                                           
         MVI   TIQDTYPE,TIQDCAST                                                
         CLC   22(4,R4),=C'CAST'                                                
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT34    CLC   12(4,R4),=C'FILT'    FILTER SET                                  
         BNE   OPT36                                                            
         MVC   TIFFTYPE,22(R4)                                                  
         CLI   TIFFTYPE,TIFFTYPA    S/B AGENCY                                  
         BE    OPTEND                                                           
         CLI   TIFFTYPE,TIFFTYPC    OR CLIENT                                   
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT36    CLC   12(4,R4),=C'PBNP'    OPTION TO PASS BNP INVOICES                 
         BNE   OPT37                                                            
         OI    TIQFLAGS,TIQFPBNP                                                
         B     OPTEND                                                           
         SPACE 1                                                                
OPT37    CLC   12(4,R4),=C'GREY'    OPTION TO PASS GREY RECORDS                 
         BNE   OPT38                                                            
         OI    TIQFLAG2,TIQFPGRY                                                
         B     OPTEND                                                           
         SPACE 1                                                                
OPT38    CLC   12(5,R4),=C'STACK'   STACK DEFINITION OPTION                     
         BNE   OPT39                                                            
         BRAS  RE,VALSTACK                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT39    CLC   12(4,R4),=C'SUBS'    OPTION TO PASS SUBSIDIARY RECORDS           
         BNE   OPT40                                                            
         OI    TIQFLAG2,TIQFSUB                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT40    CLC   12(3,R4),=C'PRI'     OPTION TO PASS PRIMARY INV RECORDS          
         BNE   OPT40A                                                           
         OI    TIQFLAG2,TIQFPRI                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT40A   CLC   12(5,R4),=C'EXACT'  OPTION TO FILTER EXACTLY(SKIP BLNKS)         
         BNE   OPT41                                                            
         OI    TIQFLAG2,TIQFEXCT                                                
         B     OPTEND                                                           
         SPACE 1                                                                
OPT41    CLC   12(7,R4),=C'NOTRUNC' NO TRUNCATION ON DL                         
         BNE   OPT42                                                            
         OI    DOWNOPT,GLDLNOTR                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT42    CLC   12(7,R4),=C'ALLALPH' ALL ALPHA ON DL                             
         BNE   OPT44                                                            
         OI    DOWNOPT,GLDLALPH                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT44    CLC   12(5,R4),=C'STRIP'   STRIP OUT NUMERICS FOR DL                   
         BNE   OPT46                                                            
         OI    DOWNOPT,GLDLSTRP                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT46    CLC   12(5,R4),=C'WHOLE'   PRINT WHOLE PRINT LINES                     
         BNE   OPT48                                                            
         OI    DRINDS2,GLPWHOLE                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT48    CLC   12(4,R4),=C'XBOX'    EXTRA BOX AFTER ROWS                        
         BNE   OPT50                                                            
         OI    DRINDS2,GLEXTBOX                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT50    CLC   12(6,R4),=C'PBREAK'  PAGE BREAK FOR DETAIL TOTALS                
         BNE   OPT52                                                            
         OI    DRINDS2,GLPBREAK                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT52    CLC   12(4,R4),=C'CUT '    ONLY ONE LINE OF OUTPUT                     
         BNE   OPT54                                                            
         OI    DOWNOPT,GLDLCUT                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT54    CLC   12(8,R4),=C'DOWNTAPE'    DOWN IN 'TAPE' FORMAT                   
         BNE   OPT56                                                            
         OI    DOWNOPT,GLDLACTV+GLDLNOHD+GLDLSTRP+GLDLNOTR+GLDLALPH             
         B     OPTEND                                                           
         SPACE 1                                                                
OPT56    CLC   12(4,R4),=C'SKIP'    SKIP OPTION                                 
         BNE   OPT57                                                            
         OI    TIQFLAGS,TIQFSKIP                                                
         OI    TIFLTSP2,TIFLTWRS                                                
         B     OPTEND                                                           
         SPACE 1                                                                
OPT57    CLC   12(8,R4),=C'LASTHIST' SHOW ONLY LATEST INVOICE                   
         BNE   OPT58                                                            
         OI    TIQFLAG3,TIQFLAST                                                
         B     OPTEND                                                           
         SPACE 1                                                                
OPT58    CLC   12(7,R4),=C'EXCLUDE' EXCLUDE OPTION FOR MAX/MIN                  
         BNE   OPT60                                                            
         OI    DRINDS2,GLEXCLUD                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT60    CLC   12(6,R4),=C'ORTEST'  TESTS ARE 'OR'                              
         BNE   OPT62                                                            
         OI    DRINDS2,GLORTEST                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT62    CLC   12(6,R4),=C'XCTEST'  TESTS ARE 'XC'                              
         BNE   OPT64                                                            
         OI    DRINDS2,GLXCTEST                                                 
         B     OPTEND                                                           
*                                                                               
OPT64    CLC   12(4,R4),=C'GOVT'    GOVERNMENT REPORT                           
         BNE   OPT66                                                            
         NI    TGSYSTAT,X'FF'-TASYSPID    TURN OFF PID                          
         B     OPTEND                                                           
*                                                                               
OPT66    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
OPTEND   LA    R4,62(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
FISCLIST DC    C'123456789ABC'                                                  
         EJECT                                                                  
*              VALIDATE STACK DEFINITION                                        
         SPACE 3                                                                
*              VALIDATE TITLE AND SUBTITLE                                      
         SPACE 3                                                                
VVALTITS MVC   TITLE,SPACES                                                     
         MVC   TITLE(20),=C'TALENT REPORT WRITER'                               
         CLI   5(R2),0                                                          
         BE    VVALTIT2                                                         
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         SPACE 1                                                                
VVALTIT2 CLI   NARROPT,C'Y'                                                     
         BE    VVALTIT3                                                         
         GOTO1 CENTER,DMCB,TITLE,32                                             
         SPACE 1                                                                
VVALTIT3 MVC   SUBTITLE,SPACES                                                  
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VVALTIT4                                                         
         GOTO1 ANY                                                              
         MVC   SUBTITLE,WORK                                                    
         CLI   NARROPT,C'Y'                                                     
         BE    VVALTIT4                                                         
         GOTO1 CENTER,DMCB,SUBTITLE,32                                          
         SPACE 1                                                                
VVALTIT4 B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LEFT HAND HEADERS                                       
         SPACE 3                                                                
VVALLEFT MVI   ROW1WIDE,0          NOT CHECKING REPORT WIDTH YET                
         MVI   TOTWIDTH,0                                                       
         MVI   ANYROWSW,C'N'                                                    
         MVI   MAX,4                                                            
         BAS   RE,DELINS                                                        
         LA    R3,4                MAX 4 FIELDS                                 
         LA    R4,4                (START ON HEAD 4)                            
         SPACE 1                                                                
VVL2     CLI   5(R2),0                                                          
         BE    VVL4                                                             
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
VVL4     BAS   RE,BUMP                                                          
         BCT   R3,VVL2                                                          
         SPACE 1                                                                
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RIGHT SIDE HEADERS                                      
         SPACE 3                                                                
VVALRGHT LA    R3,3                MAX 3 FIELDS                                 
         LA    R4,6                (START ON HEAD 6)                            
         MVI   MAX,3                                                            
         BAS   RE,DELINS                                                        
         SPACE 1                                                                
VVRT2    CLI   5(R2),0                                                          
         BE    VVRT4                                                            
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,97         (COLUMN 97)                                  
         CLI   NARROPT,C'Y'        (NOT ALLOWED FOR NARROW)                     
         BE    BADROW                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   MYPOSO+2,132        (COLUMN 132 FOR WIDE)                        
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
VVRT4    BAS   RE,BUMP                                                          
         BCT   R3,VVRT2                                                         
         SPACE 1                                                                
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         IC    R3,MYFIRSTH                                                      
         CR    R4,R3                                                            
         BL    XIT                                                              
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE MID                                                     
         SPACE 1                                                                
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         BAS   RE,VALROW                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE ROWS                                                    
         SPACE 1                                                                
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL         (REALLY A(FIRST ROW!))                       
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,8                                                             
         STC   R3,MAX                                                           
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         SPACE 1                                                                
VVR2     XC    MYPOSO,MYPOSO                                                    
         BAS   RE,VALROW                                                        
         ZIC   R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   *+8                                                              
         STC   R0,ROW1WIDE                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VVR2                                                          
         STC   R0,ROWWIDTH                                                      
         L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROWS - FIRST VALIDATE FOR KEYWORD                                
         SPACE 3                                                                
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   SAVTOTYP,0          PRE-CLEAR                                    
         BAS   RE,AUTHCHEK                                                      
         MVI   ANYROWSW,C'Y'                                                    
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
         GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
                                                                                
         BRAS  RE,CHKSSN           CHECK IF SSN IN HEADING                      
                                                                                
         CLC   12(5,R4),=C'TEXT '  IF TEXT                                      
         BNE   *+8                                                              
         BAS   RE,GENTEXT          MOVE TEXT TO OUTPUT LITERAL                  
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW2                                                          
         CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW2                                                            
         CH    R0,=H'1'            IF IT'S ON ITS OWN                           
         BE    VROW21                 MAKE NON PRINT                            
         LA    R4,32(R4)           RANK NEEDS A COMPUTE EXPRESSION              
         BCT   R0,*+8                                                           
         B     BADROW                                                           
         AI    FIELDERR,1                                                       
         MVI   DRCMPMAX,C'P'                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   VROW1                                                            
         GOTO1 GROWDRON                                                         
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW1    GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE IN THE ROWS                        
         BZ    VROW2                                                            
         CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
         SPACE 1                                                                
VROW2    CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW4                                                            
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW4    CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROWNXT                                                          
*******  OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
*******  MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROWNXT                                                          
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY ROW EXPRESSIONS                             
         SPACE 3                                                                
VROW12   CLI   12(R4),C'*'         TOTAL EXPRESSION                             
         BNE   VROW14                                                           
         OI    DRTOTAL,X'80'                                                    
         MVC   SAVTOTYP,13(R4)                                                  
*******  MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         LR    RE,R2                                                            
         SPACE 1                                                                
VROW12B  ZIC   RF,0(RE)                                                         
         AR    RE,RF               HAVE A LOOK AT THE NEXT LEVEL                
         CLI   0(RE),0                                                          
         BE    VROWNXT                                                          
         TM    1(RE),X'20'                                                      
         BO    VROW12B             FIND AN UNPROTECTED FIELD                    
         ZIC   RF,5(RE)                                                         
         LTR   RF,RF               WITH SOME DATA                               
         BZ    VROW12B                                                          
         LA    RE,8(RE)                                                         
         SPACE 1                                                                
VROW12D  CLI   0(RE),C'*'          IF A TOTAL IS NOT SPECIFIED                  
         BE    VROWNXT                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VROW12D                                                       
******   OI    DRLAST,X'80'        GENERATE A SPACE BEFORE TOTALS               
******   MVI   DRLSPACE,1                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW14   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW16                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW16   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW18                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROWNXT                                                          
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW18   CLC   12(2,R4),=C'U '                                                  
         BNE   VROW20                                                           
         BAS   RE,VUSRDRON                                                      
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW20   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW22                                                           
         SPACE 1                                                                
VROW21   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW22   CLC   12(4,R4),=C'DET '   TOTAL DETAIL EXPRESSION                      
         BE    VROW23                                                           
         CLC   12(2,R4),=C'D '     DET=N OR D=N FORMAT                          
         BNE   VROW24                                                           
         SPACE 1                                                                
VROW23   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    PICK UP NUMBER OF DETAILS                    
         CLI   DRTDET,0            MUST BE SOMETHING NUMERIC                    
         BE    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW24   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW26                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW26   CLC   12(5,R4),=C'HORIZ'  PRINT HORIZONTAL LINE                        
         BNE   VROW28                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         OI    DRLOPTS,DRLHORIZ    SET HORIZONTAL LINE AS LAST OPTION           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW28   B     BADROW                                                           
         SPACE 1                                                                
VROWNXT  LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW12                                                        
         EJECT                                                                  
*              FINAL ADJUSTMENTS                                                
         SPACE 3                                                                
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROWGEN                                                          
         TM    DRFLAGO,X'80'                                                    
         BNO   VROWGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE 1                                                                
VROWGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRPOSO,MYPOSO                                                    
         SPACE 1                                                                
         TM    DRTOTAL,X'80'       WAS TOTAL REQUESTED?                         
         BNO   VROWADJ2                                                         
         CLI   DRRTNO,X'41'        IF OUT ROUTINE SPECIFIED                     
         BL    VROWADJ2                                                         
         CLI   SAVTOTYP,C'-'                                                    
         BE    VROWADJ2            (*- MEANS SUPPRESS THIS STUFF)               
*******  CLI   DRLENO,14              AND LENGTH IS AT LEAST 14                 
*******  BL    VROWADJ2                                                         
         MVC   DRTRTN,DRRTNO       USE THIS FOR TOTAL AS WELL                   
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
         SPACE 1                                                                
VROWADJ2 GOTO1 GROWDRON                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADROW   MVC   CONHEAD(L'ROWERR),ROWERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADROW2  MVC   CONHEAD(L'COLONLY),COLONLY                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLRANK MVC   CONHEAD(L'LRANKERR),LRANKERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              ROUTINE OT GENERATE A TEXT ROW OR COLUMN                         
         SPACE 1                                                                
GENTEXT  SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BNZ   *+8                                                              
         LA    R1,1                                                             
         CH    R1,=Y(L'DRLITO)                                                  
         BL    *+8                                                              
         LH    R1,=Y(L'DRLITO)                                                  
         STC   R1,DRLENO                                                        
         STC   R1,DRLITLNO                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRLITO(0),22(R4)                                                 
         BR    RE                                                               
         SPACE 2                                                                
*              CHECK AUTHORIZATION                                              
         SPACE 1                                                                
AUTHCHEK NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    XIT                                                              
         TM    4(R2),X'20'         AND ANY USER INPUT?                          
         BO    XIT                                                              
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPC   CLIENTS NOT ALLOWED                          
         BE    BADAUTH                                                          
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    BADAUTH                                                          
         CLI   TGCTSTTY,TASTTYPF                                                
         BE    BADAUTH                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADAUTH  MVC   CONHEAD(L'AUTHERR),AUTHERR                                       
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE COLUMNS                                                 
         SPACE 3                                                                
VVALCOLS ZIC   R0,MAX                                                           
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         CLI   5(R2),0                                                          
         BE    BADNEED1            NEED AT LEAST 1 COLUMN                       
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         XC    EDITLIST,EDITLIST                                                
         LA    R3,EDITLIST                                                      
         MVI   MYCOLNUM,1                                                       
         MVI   FILTSLOT,1                                                       
         SPACE 1                                                                
VVALCOL2 XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SAVE LABEL IN EDIT LIST                      
         BAS   RE,VALCOL                                                        
         BAS   RE,BUMP                                                          
         CLI   CLEXTEND,2          UNLESS THIS IS A CONTINUATION                
         BE    *+10                                                             
         MVC   MYLABEL,8(R2)       SAVE THE LABEL                               
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         AI    MYCOLNUM,1                                                       
         BCT   R0,VVALCOL2                                                      
         SPACE 1                                                                
         TM    DOWNOPT,GLDLACTV    IF DOWNLOADING                               
         BO    XIT                 SKIP WIDTH CHECKS (USER BEWARE)              
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   NARROPT,C'Y'        ONLY 80 ALLOWED WITH NARROW OPT              
         BE    VVALCBIG                                                         
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   WIDEOPT,C'Y'                                                     
         BNE   VVALCBIG                                                         
         CLC   TOTWIDTH,=H'165'                                                 
         BNH   XIT                                                              
         SPACE 1                                                                
VVALCBIG MVC   CONHEAD(36),=C'REPORT HAS NNN CHARACTERS - TOO WIDE'             
         LA    R3,CONHEAD+11                                                    
         EDIT  (2,TOTWIDTH),(3,(R3))                                            
         L     R2,ALASTCOL                                                      
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE FIRST COLUMN EXPRESSION                                 
         SPACE 3                                                                
VALCOL   NTR1                                                                   
         MVI   FIELDERR,1                                                       
         CLI   CLEXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOLB                                                            
         CLI   5(R2),0                                                          
         BE    VCOLNXT2                                                         
         BAS   RE,AUTHCHEK                                                      
         LA    R4,BLOCK+42+42      (DON'T DISTURB FIRST 2 ENTRIES               
         B     VCOL1               FOR EXTEND - COMPUTES MAY BE THERE)          
         SPACE 1                                                                
VCOLB    CLI   5(R2),0                                                          
         BE    XIT                                                              
         ST    R2,ALASTCOL                                                      
         XC    BLOCK(252),BLOCK                                                 
         LA    R4,BLOCK+42                                                      
         MVI   CLEXTEND,0                                                       
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL1                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   CLEXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
         SPACE 1                                                                
VCOL1    GOTO1 SCANNER,DMCB,(20,(R2)),(5,(R4)),0                                
         CLI   CLEXTEND,1          IF WE HAVE AN EXTENSION HERE                 
         BNE   VCOL2                                                            
         ZIC   R1,5(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)               RESTORE ORIGINAL LENGTH                   
         SPACE 1                                                                
VCOL2    ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         SPACE 1                                                                
         CLI   CLEXTEND,2                                                       
         BE    VCOL12                                                           
         XC    DRARGSI(16),DRARGSI                                              
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
         CLI   DRERROR,0                                                        
         BNE   VCOL4                                                            
                                                                                
         BRAS  RE,CHKSSN           CHECK IF SSN IN HEADING                      
                                                                                
         CLC   12(5,R4),=C'TEXT '  IF TEXT                                      
         BNE   *+12                                                             
         BAS   RE,GENTEXT          MOVE TEXT TO OUTPUT LITERAL                  
         B     VCOLNXT                                                          
         CLI   DRATTRIB,C'R'       ROW ONLY ENTRIES NOT ALLOWED                 
         BE    BADCOL2                                                          
         CLI   1(R4),0             ENTRY=XXXX IS BAD                            
         BE    VCOLNXT                                                          
         B     BADCOL                                                           
         SPACE 1                                                                
VCOL4    XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL6                                                            
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOL6    BAS   RE,COMPEDIT         AUTO EDIT FOR COMPUTES                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
         SPACE 1                                                                
SETMAX2  CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETMAX2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                          
         SPACE 3                                                                
VCOL12   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL14                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         CH    R1,=H'1900'                                                      
         BH    VCOL13              (MAYBE A YEAR)                               
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL13   CLI   0(R4),4             (YEARS ARE LENGTH 4)                         
         BNE   BADCOL                                                           
         MVC   DUB(4),12(R4)       MOVE IN YEAR                                 
         MVC   DUB+4(4),=C'0101'                                                
         GOTO1 DATCON,DMCB,(9,DUB),(1,WORK)                                     
         MVC   DRARGSI+10(1),WORK  PLUG IN YEAR                                 
         OC    DRARGSI+13(3),DRARGSI+13                                         
         BZ    VCOLNXT                                                          
         MVC   DRARGSI+13(1),WORK                                               
         B     VCOLNXT                                                          
*                                  CHECK FOR PERIOD EXPRESSION                  
VCOL14   MVC   MYARGSI,DRARGSI     SAVE ARGS - PERVAL MODIFIES                  
         BAS   RE,PERVAL1                                                       
         BNE   VCOL18                                                           
         BAS   RE,PERPOP           (PERVAL FOUND SOMETHING GOOD)                
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL18   MVC   DRARGSI,MYARGSI     RESTORE ARGS.                                
         LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL20                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL24                                                           
         SPACE 1                                                                
VCOL20   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),0                                                          
         BE    VCOLNXT             HN= CAUSES REMOVAL                           
         OI    0(R1),X'80'         OTHERWISE TURN IT BACK ON                    
         MVC   27(1,R1),1(R4)      PASS LITERAL LENGTH TO DRONE                 
         CLC   1(1,R4),DRLENO                                                   
         BNH   VCOL22              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   CONHEAD(L'HOVERR),HOVERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
VCOL22   MVC   28(24,R1),22(R4)    PASS DRONE THE LITERAL                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL28                                                           
         MVI   MYPOSO,C'N'                                                      
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL28   CLC   12(5,R4),=C'THOU '  THOUSAND OPTION                              
         BNE   VCOL29                                                           
         MVI   DRDIVO,5                                                         
         MVI   DRDECO,0                                                         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL29   CLC   12(6,R4),=C'DOLLAR' DOLLAR OPTION                                
         BNE   VCOL30                                                           
         MVI   DRDIVO,2                                                         
         MVI   DRDECO,0                                                         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30   CLC   12(8,R4),=C'BRACKET '   (MINUS NUMBERS)                          
         BNE   VCOL30B                                                          
         OI    DROPTSO,DRBKMINO                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30B  CLC   12(5,R4),=C'COMMAS'     (COMMAS BETWEEN THOUS)                   
         BNE   VCOL30D                                                          
         OI    DROPTSO,DRCOMMAO                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30D  CLC   12(5,R4),=C'ALIGN'      (ALIGNMENT OPTIONS)                      
         BNE   VCOL30H                                                          
         CLI   22(R4),C'R'                                                      
         BE    VCOL30F                                                          
         CLI   22(R4),C'L'                                                      
         BNE   BADCOL                                                           
         OI    DROPTSO,DRALGNLO    LEFT                                         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30F  OI    DROPTSO,DRALGNRO    RIGHT                                        
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30H  CLC   12(7,R4),=C'NOBLANK'    (ZERO=NOBLANK OPTION)                    
         BNE   VCOL32                                                           
         OI    DROPTSO,DRZEROO                                                  
         B     VCOLNXT                                                          
*                                                                               
VCOL32   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL34                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOLNXT                                                          
         EJECT                                                                  
*              SUPPORT COLUMN FILTERS                                           
         SPACE 3                                                                
VCOL34   ZIC   R1,0(R4)            L'FILTER EXPRESSION                          
         BCTR  R1,0                                                             
         L     RE,=A(FILTTAB)                                                   
         A     RE,RELO                                                          
         SPACE 1                                                                
VCOL36   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R4)                                                   
         BE    VCOL38                                                           
         LA    RE,L'FILTENT(RE)                                                 
         CLI   0(RE),X'FF'                                                      
         BNE   VCOL36                                                           
         B     VCOL44                                                           
         SPACE 1                                                                
VCOL38   CLC   0(3,RE),=C'AGR'     SPECIAL I/O FOR SOME FILTERS                 
         BNE   *+8                                                              
         OI    TIQCFILT,TIQCFAGY                                                
         CLC   0(3,RE),=C'OFF'                                                  
         BNE   *+8                                                              
         OI    TIQCFILT,TIQCFAGY                                                
         CLC   0(3,RE),=C'CGR'                                                  
         BNE   *+8                                                              
         OI    TIQCFILT,TIQCFCLI                                                
         CLC   0(3,RE),=C'SEX'                                                  
         BNE   *+8                                                              
         OI    TIQCFILT,TIQCFW4                                                 
         CLC   0(3,RE),=C'RACE'                                                 
         BNE   *+8                                                              
         OI    TIQCFILT,TIQCFW4                                                 
         LA    R1,DRARGSI+6        UP TO 3 SLOTS SUPPORTED                      
         LA    RF,3                                                             
         SPACE 1                                                                
VCOL40   CLI   0(R1),0             FIND AN EMPTY SLOT                           
         BE    VCOL42                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,VCOL40                                                        
         B     BADCOL                                                           
         SPACE 1                                                                
VCOL42   MVC   0(1,R1),FILTSLOT    SAVE FILTER SLOT NUMBER                      
         ZIC   R1,FILTSLOT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,FILTSLOT         USE SLOT NUMBER                              
         SH    R1,=H'2'                                                         
         SLL   R1,4                                                             
         A     R1,ACOLFILT         TO DISPLACE INTO COLUMN FILTERS              
         MVC   0(1,R1),9(RE)       SAVE DISPLACEMENT                            
         MVC   1(1,R1),11(RE)           AND LENGTH                              
         MVC   4(12,R1),22(R4)     AND SAVE FILTERED DATA                       
         SPACE 1                                                                
         CLC   22(2,R4),=C'-@'     CHECK FOR NEG LISTS                          
         BE    VCOL426                                                          
         CLC   22(2,R4),=C'@-'                                                  
         BE    VCOL426                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOL422                                                          
         CLI   22(R4),C'@'                                                      
         BE    VCOL424                                                          
         MVI   3(R1),X'FF'                                                      
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL422  MVC   4(12,R1),23(R4)     (NEGATIVE DATA)                              
         MVI   3(R1),X'FF'-X'40'                                                
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL424  MVC   4(12,R1),23(R4)     (LIST DATA)                                  
         MVI   3(R1),X'FF'-X'80'                                                
         B     VCOL428                                                          
         SPACE 1                                                                
VCOL426  MVC   4(12,R1),24(R4)     (NEGATIVE LIST DATA)                         
         MVI   3(R1),X'FF'-X'80'-X'40'                                          
         SPACE 1                                                                
VCOL428  XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,4(R1)                                                    
         DROP  R3                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADCOL                                                           
         B     VCOLNXT                                                          
         EJECT                                                                  
*              OTHER PARAMETERS                                                 
         SPACE 3                                                                
VCOL44   CLC   12(4,R4),=C'CUME'   CUME OUTPUT OPTION                           
         BNE   VCOL46                                                           
         OI    DROPTSO+1,DRCUME                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL46   CLC   12(4,R4),=C'RANK'   (COLUMN) RANK                                
         BNE   VCOL48                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         MVI   COLRANK,2                                                        
         B     VCOLNXT                                                          
         SPACE 1                                                                
COLRANK  DC    X'00'                                                            
         SPACE 1                                                                
VCOL48   BAS   RE,VCOLMXMN         MAY BE MAX OR MIN EXPRESSION                 
         BE    VCOLNXT                                                          
         SPACE 1                                                                
VCOL99   B     BADCOL                                                           
         EJECT                                                                  
*              ANY MORE COLUMNS TO GENERATE                                     
         SPACE 3                                                                
ANYMOCOL NTR1                                                                   
         CLI   COLRANK,0           COLUMN RANKING                               
         BE    XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         MVC   DRARGSI(1),COLRANK       PASS ARGUMENT                           
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
*****    OI    DROPTSO+1,DRNOLBOX  NO BOX TO LEFT OF RANK                       
         GOTO1 GCOLDRON                                                         
         B     XIT                                                              
         EJECT                                                                  
*              END OF COLUMN VALIDATION                                         
         SPACE 3                                                                
VCOLNXT  DS    0H                                                               
         LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL12                                                        
         SPACE 1                                                                
         CLI   CLEXTEND,1          IF THERE IS AN EXTENSION PENDING             
         BNE   VCOLNXT2                                                         
         MVI   CLEXTEND,2             NOT TIME TO WRAP UP YET                   
         B     XIT                                                              
         SPACE 1                                                                
VCOLNXT2 MVI   CLEXTEND,0                                                       
         SPACE 1                                                                
         BAS   RE,OPTADJ           ADJUSTMENTS FROM OPTIONS                     
         BAS   RE,MOREVAL          CHECK ATTRIBUTES ETC                         
         SPACE 1                                                                
         MVI   DRNARGSI,16         ALWAYS PASS ALL ARGUMENTS                    
         MVI   DRNARGSO,16                                                      
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOLGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE 1                                                                
VCOLGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         GOTO1 GCOLDRON                                                         
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOLGEN2                                                         
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOLGEN2 BAS   RE,ANYMOCOL                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADCOL   MVC   CONHEAD(L'COLERR),COLERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUSER  MVC   CONHEAD(L'USRERR),USRERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADCOL2  MVC   CONHEAD(L'ROWONLY),ROWONLY                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADNEED1 MVC   CONHEAD(L'NEED1ERR),NEED1ERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              ROUTINE TO EDIT COLUMNAR PERIOD EXPRESSION                       
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER LINE)                           
*              OUTPUT              DRARGSI                                      
*                                  10=TYPE  PAY=0 LAST=1 BILL=2                 
*                                           CHECK=3 DUE=4                       
*                                  11-13    START YMD (PWOS)                    
*                                  14-16    END YMD (PWOS)                      
         SPACE 1                                                                
PERVAL1  NTR1                                                                   
         LA    R2,22(R4)                                                        
         ZIC   R3,1(R4)                                                         
         MVI   DRARGSI+9,TIQDPAY                                                
         CLC   12(3,R4),=C'PAY'                                                 
         BE    PERVAL2                                                          
**NO-OP**MVI   DRARGSI+9,TIQDLAST  **MUST BE 5 SEE SETDATE IN SYSDRIVE          
         MVI   DRARGSI+9,5                                                      
         CLC   12(4,R4),=C'LAST'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDBILL                                               
         CLC   12(4,R4),=C'BILL'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDCHK                                                
         CLC   12(5,R4),=C'CHECK'                                               
         BE    PERVAL2                                                          
**NO-OP**MVI   DRARGSI+9,TIQDDUE   **MUST BE 4 SEE SETDATE IN SYSDRIVE          
         MVI   DRARGSI+9,4                                                      
         CLC   12(3,R4),=C'DUE'                                                 
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDCYCS                                               
         CLC   12(4,R4),=C'CYCS'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDCYCE                                               
         CLC   12(4,R4),=C'CYCE'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDVADV                                               
         CLC   12(4,R4),=C'VADV'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDRADV                                               
         CLC   12(4,R4),=C'RADV'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDSADV                                               
         CLC   12(4,R4),=C'SADV'                                                
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDEXP                                                
         CLC   12(3,R4),=C'EXP'                                                 
         BE    PERVAL2                                                          
         MVI   DRARGSI+9,TIQDASS                                                
         CLC   12(3,R4),=C'ASS'                                                 
         BE    PERVAL2                                                          
         LA    R2,12(R4)                                                        
         MVC   DRARGSI+9(1),TIQDTYPE                                            
         ZIC   R3,0(R4)                                                         
         CLI   1(R4),0                                                          
         BNE   NOGOOD                                                           
         CLI   DRARGSI+9,0                                                      
         BNE   PERVAL2                                                          
         MVI   DRARGSI+9,TIQDCHK                                                
         CLI   TIREAD,TLCKCDQ                                                   
         MVI   DRARGSI+9,TIQDBILL                                               
         SPACE 1                                                                
PERVAL2  LTR   R3,R3               R2=A(PERIOD EXPRESS) R3=LENGTH               
         BZ    NOGOOD                                                           
*                                  START DATE                                   
         LA    R1,DRARGSI+11       MUST START WITH A MONTH                      
         BAS   RE,PERMON                                                        
         BNE   NOGOOD                                                           
         BAS   RE,PERYEAR                                                       
         BNE   NOGOOD                                                           
         SH    R3,=H'3'                                                         
         BZ    ITSFINE                                                          
         LA    R2,3(R2)                                                         
         BAS   RE,PERDAY           OPTIONAL DAY                                 
         BNE   NOGOOD                                                           
         ZIC   R1,WORK             (LENGTH RETURNED IN WORK)                    
         AR    R2,R1                                                            
         SR    R3,R1                                                            
         BZ    ITSFINE                                                          
         CLI   0(R2),C'-'                                                       
         BNE   NOGOOD              DATA LENGTH - MUST BE A DASH                 
         LA    R2,1(R2)                                                         
         BCT   R3,*+8                                                           
         B     NOGOOD                                                           
*                                  END DATE NOW                                 
         LA    R1,DRARGSI+14       MUST START WITH A MONTH                      
         BAS   RE,PERMON                                                        
         BNE   NOGOOD                                                           
         BAS   RE,PERYEAR                                                       
         BNE   NOGOOD                                                           
         SH    R3,=H'3'                                                         
         BZ    ITSFINE                                                          
         LA    R2,3(R2)                                                         
         BAS   RE,PERDAY           OPTIONAL DAY                                 
         BNE   NOGOOD                                                           
         ZIC   R1,WORK             (LENGTH RETURNED IN WORK)                    
         SR    R3,R1                                                            
         BNZ   NOGOOD                                                           
         CLC   DRARGSI+10(3),DRARGSI+13                                         
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         EJECT                                                                  
*              SUBROUTINES FOR PERVAL                                           
         SPACE 1                                                                
PERYEAR  NTR1                                                                   
         BCTR  R1,0                R1=A(YMD) PWOS IN ARGS                       
         OC    TIQPEND,TIQPEND     IF NO END DATE SPECIFIED                     
         BNZ   PERYEAR2                                                         
         CLC   1(1,R1),TIQPSTR+1   MONTH MUST MATCH                             
         BNE   NOGOOD                                                           
         MVC   0(1,R1),TIQPSTR     PASS BACK START YEAR                         
         B     ITSFINE                                                          
         SPACE 1                                                                
PERYEAR2 CLC   TIQPSTR(1),TIQPEND  IF REQUEST S/E SAME YEAR                     
         BNE   PERYEAR4                                                         
         CLC   1(1,R1),TIQPSTR+1      MONTH MUST BE WITHIN PERIOD               
         BL    NOGOOD                                                           
         CLC   1(1,R1),TIQPEND+1                                                
         BH    NOGOOD                                                           
         MVC   0(1,R1),TIQPSTR     PASS BACK START YEAR                         
         B     ITSFINE                                                          
         SPACE 1                                                                
*                                  REQUEST END IN FOLLOWING YEAR                
PERYEAR4 MVC   0(1,R1),TIQPSTR     PASS BACK START YEAR                         
         CLC   1(1,R1),TIQPSTR+1        IF MONTH AFTER REQUEST START            
         BNL   ITSFINE                                                          
         MVC   0(1,R1),TIQPEND     PASS BACK END YEAR                           
         CLC   1(1,R1),TIQPEND+1        IF MONTH BEFORE REQUEST END             
         BNH   ITSFINE                                                          
         B     NOGOOD              OTHERWISE ITS NOT OK                         
         SPACE 1                                                                
PERMON   NTR1                                                                   
         LA    R3,MONLIST                                                       
         LA    R4,1                                                             
         LA    R0,12                                                            
         SPACE 1                                                                
PERMON2  STC   R4,0(R1)            PASS BACK MONTH NUMBER                       
         CLC   0(3,R2),0(R3)                                                    
         BE    ITSFINE                                                          
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'10'                                                        
         BNE   *+8                                                              
         LA    R4,X'10'            (RETURNING MONTH IN PWOS)                    
         BCT   R0,PERMON2                                                       
         B     NOGOOD                                                           
         EJECT                                                                  
PERDAY   NTR1                                                                   
*                                   R2=A(POSSIBLE DAY)                          
*                                   R1=A(MONTH/DAY IN ARGS)                     
*                                   RETURN DAY AND LENGTH IN WORK               
         MVI   WORK,0                                                           
         CLI   0(R2),C'-'                                                       
         BE    ITSFINE                                                          
         CLI   0(R2),X'F0'                                                      
         BL    NOGOOD                                                           
         MVI   WORK,1                                                           
         CLI   1(R2),C' '                                                       
         BE    PERDAY2                                                          
         CLI   1(R2),C'-'                                                       
         BE    PERDAY2                                                          
         CLI   1(R2),X'F0'                                                      
         BL    NOGOOD                                                           
         MVI   WORK,2                                                           
*                                                                               
PERDAY2  ZIC   RF,WORK             CONVERT TO PWOS                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         MP    DUB,=P'10'                                                       
         MVC   1(1,R1),DUB+6                                                    
         ZIC   RF,0(R1)            PICK UP MONTH (IN PWOS)                      
         LA    RF,DAYMONS-1(RF)    DISPLACE TO DAYS IN MONTH                    
         CLI   1(R1),0             DAYS CAN'T BE ZERO                           
         BE    NOGOOD                                                           
         CLC   1(1,R1),0(RF)       OR MORE THAN DAYS IN MONTH                   
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         SPACE 1                                                                
DAYMONS  DC    X'312931303130'     MONTHS X'01' TO X'06'                        
         DC    X'313130'                  X'07' TO X'09'                        
         DC    6X'00'                                                           
         DC    X'313031'                  X'10' TO X'12'                        
         EJECT                                                                  
       ++INCLUDE DDVALMNMX                                                      
*              A BIT MORE VALIDATION FOR COLUMNS                                
         SPACE 3                                                                
MOREVAL  NTR1                                                                   
         CLI   DRATTRIB,C'S'             CHECK STACK SPECIFIED                  
         BNE   XIT                                                              
         OC    STACKDEF,STACKDEF                                                
         BZ    NEEDSTAK                                                         
         LA    R2,STACKDEF         COUNT THE NUMBER OF ENTRIES                  
         SR    R1,R1                                                            
         SPACE 1                                                                
STCNT2   STC   R1,DRTYPEI+2        RETURN REPETITION FACTOR                     
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         CLI   1(R2),0             (DON'T COUNT CONTROL ENTRIES)                
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         CH    R1,=H'9'                                                         
         BL    STCNT2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
NEEDSTAK DS    0H                                                               
         MVC   CONHEAD(L'NOSTKERR),NOSTKERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              ROUTINE TO ADJUST ARGUMENTS                                      
         SPACE 3                                                                
*              ROUTINE TO FIGURE OUT EDITS FOR COMPUTES                         
         SPACE 3                                                                
COMPEDIT NTR1                                                                   
         TM    OUTOPTS,DRBKMINO    IF USER ASKED FOR BRACKETS                   
         BNO   *+12                                                             
         MVI   DRFLOATO,0          DO NOT FLOAT IN MINUS AS WELL!               
         OI    DROPTSO,DRBKMINO    AND ENSURE BRACKET OPTION                    
*                                  R4=A(SCANNER TABLE ENTRY)                    
         ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BE    COMPPCT                                                          
         MVI   DRDECO,2            ASSUME 2 DEC PLACES                          
         LA    RE,8(R2)            RE=A(START OF INPUT STRING)                  
         ZIC   R0,5(R2)            R0=LENGTH OF INPUT STRING                    
         SPACE 1                                                                
COMPED1  CLI   0(RE),C'A'          LOOK FOR THE FIRST LETTER                    
         BL    COMPED1B                                                         
         CLI   0(RE),C'Z'                                                       
         BH    COMPED1B                                                         
         B     COMPED2                                                          
         SPACE 1                                                                
COMPED1B LA    RE,1(RE)                                                         
         BCT   R0,COMPED1                                                       
         B     XIT                                                              
         SPACE 1                                                                
COMPED2  LA    R1,EDITLIST         NOW LOOK UP THIS LETTER IN EDITLIST          
         SPACE 1                                                                
COMPED2B CLI   0(R1),0                                                          
         BE    XIT                                                              
         CLC   0(1,R1),0(RE)                                                    
         BE    COMPED3                                                          
         LA    R1,4(R1)                                                         
         B     COMPED2B                                                         
         SPACE 1                                                                
COMPED3  MVC   DRDECO,1(R1)        PICK UP ITS EDIT CHARACTERISTIC              
         MVC   DRDIVO,2(R1)                                                     
         B     XIT                                                              
         SPACE 1                                                                
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     XIT                                                              
         SPACE 1                                                                
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADJUST PERIOD                                         
         SPACE 3                                                                
PERADJ   NTR1                                                                   
         CLC   WORK(6),=C'000000' IF START NOT SPECIFIED                        
         BNE   PERADJ2                                                          
         MVC   WORK(2),WORK+6      COPY YEAR FROM END                           
         MVC   WORK+2(2),=C'01'    USE JANUARY                                  
         MVC   WORK+4(2),=C'01'    MAKE DAY 01                                  
         B     XIT                                                              
         SPACE 1                                                                
PERADJ2  CLC   WORK+6(6),=C'000000' IF END NOT SPECIFIED                        
         BNE   XIT                                                              
         MVC   WORK+6(2),WORK      COPY YEAR FROM START                         
         MVC   WORK+6+2(2),=C'12'  USE DECEMBER                                 
         MVC   WORK+6+4(2),=C'31'  MAKE DAY 31                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT IN PERIOD HEADING SUPPORT                         
         SPACE 3                                                                
PERPOP   NTR1                                                                   
         CLI   MYPOSO,C'N'         NOT NEEDED IF NOT PRINTING                   
         BE    XIT                                                              
         CLI   DRARGSI+11,0        WAS A PERIOD SPECIFIED?                      
         BE    XIT                     (MINIMUM IS 1 MONTH)                     
         LA    R1,DRHEAD1          YES SO FIND AN EMPTY HEADING                 
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD2                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD3                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD4                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         B     XIT                 NO ROOM!                                     
         SPACE 1                                                                
PERPOP2  OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'PERPOP'                                             
         MVC   9(7,R1),DRARGSI+9   PASS THE PERIOD TYPE, S/E                    
         MVI   25(R1),16           ALL ARGUMENTS                                
         B     XIT                                                              
         SPACE 3                                                                
*              ADJUST DRONE BLOCK FOR OPTIONS                                   
         SPACE 1                                                                
OPTADJ   NTR1                                                                   
         CLI   DRTYPEI+1,C'+'      DON'T ADJUST NON ADDITIVE                    
         BNE   OPTADJ2                                                          
         CLI   DRDECO,2            AND IF OUTPUT NOT 2 DECIMAL PLACES           
         BNE   OPTADJ2                                                          
         CLI   THOUOPT,C'Y'        OPTION TO SHOW ALL IN THOUSANDS              
         BNE   OPTADJ1                                                          
         MVI   DRDIVO,5                                                         
         MVI   DRDECO,0                                                         
         SPACE 1                                                                
OPTADJ1  CLI   THOUOPT,C'$'        OPTION TO SHOW ALL IN DOLLARS                
         BNE   OPTADJ2                                                          
         MVI   DRDIVO,2                                                         
         MVI   DRDECO,0                                                         
         SPACE 1                                                                
OPTADJ2  OC    DROPTSO(1),OUTOPTS  PASS THROUGH OUTPUT OPTIONS                  
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL HEADLINE HOOK ROUTINES                                   
         SPACE 3                                                                
VGENHEAD L     RE,ACODEFLD         PICK UP CODE FROM SCREEN                     
         LTR   RE,RE               IF CODE INPUT                                
         BZ    VGENHA                                                           
         CLI   NARROPT,C'Y'        DON'T BOTHER ON NARROW FORMAT                
         BE    VGENHA                                                           
         L     R2,AH1                                                           
         A     R2,PWIDTH           SHOW TO THE RIGHT OF REQUESTOR (H2)          
         LA    R2,32(R2)                                                        
         CLI   0(R2),C' '          SHUFFLE BACK TO LAST NON-SPACE               
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         L     R1,AH1                                                           
         A     R1,PWIDTH                                                        
         CR    R2,R1               MAKE SURE STILL ON H2                        
         BL    VGENHA                                                           
         MVC   3(4,R2),=C'CODE'                                                 
         MVC   8(8,R2),8(RE)                                                    
         OC    8(8,R2),SPACES                                                   
         SPACE 1                                                                
VGENHA   L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,48(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   VGENHB                                                           
         SH    R2,=H'47'                                                        
         L     R1,AH4                                                           
         MVC   BLOCK(24),59(R1)    SAVE H4 RIGHT FOR NARROW                     
         MVC   59(24,R1),SPACES                                                 
         SPACE 1                                                                
VGENHB   MVC   0(32,R2),TITLE      (TITLES ARE ALREADY CENTERED)                
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(32,TITLE),(X'BF',(R2))                            
         A     R2,PWIDTH                                                        
         MVC   0(32,R2),SUBTITLE   AND THE SUBTITLE                             
         SPACE 1                                                                
         L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'6'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,15                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         SPACE 1                                                                
         L     R2,AH4              PERIOD TO HEAD5 RIGHT                        
         LA    R2,96(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,32(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   *+14                                                             
         SH    R2,=H'37'                                                        
         MVC   0(24,R2),BLOCK      (REPLACE SAVED H4 RIGHT)                     
         A     R2,PWIDTH                                                        
         L     RE,APERFLD          PICK UP PERIOD FROM SCREEN                   
         LTR   RE,RE                                                            
         BZ    VGENH2                                                           
         CLI   PEROPT,C'N'         IF REQUESTED TO SUPPRESS PERIOD              
         BE    VGENH2              SKIP IT                                      
         MVC   0(6,R2),=C'PERIOD'                                               
         MVC   7(17,R2),8(RE)                                                   
         OC    7(17,R2),SPACES                                                  
         SPACE 1                                                                
VGENH2   DS    0H                                                               
         A     R2,PWIDTH                                                        
         B     XIT                                                              
         SPACE 1                                                                
SPECDTSW DC    C'N'                                                             
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GENHEAD                                  
         SPACE 3                                                                
GETLONG  NTR1                                                                   
*              INPUTS              R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=NUMBER OF LINES                           
*              OUTPUT              FULL=WIDEST FOUND                            
         SPACE 1                                                                
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
         SPACE 1                                                                
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XIT                                                              
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 3                                                                
SHUFFLE  NTR1                                                                   
*              INPUTS              R2=A(START DATA ON FIRST LINE)               
*                                  R3=A(FROM DATA)                              
*                                  R4=NUMBER OF LINES                           
         SPACE 1                                                                
SHUFFLE2 MVC   WORK,0(R3)                                                       
         MVC   0(60,R3),SPACES                                                  
         MVC   0(60,R2),WORK                                                    
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DRONE UTILITIES                                                  
         SPACE 3                                                                
VINTDRON DS    0H                  INITIALIZATION                               
         MVI   DRWHO,DRTALWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'TALENT'                                              
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   GRANDOPT,C'Y'       IF GRAND TOTALS REQUESTED                    
         BNE   XIT                                                              
         MVC   DROLDBUF,DRCURBUF                                                
         L     R1,DRCURBUF                                                      
         MVC   0(2,R1),=X'4802'    DEAL WITH THAT NOW                           
         LA    R1,2(R1)                                                         
         MVC   0(3,R1),=X'871000'                                               
         MVC   3(13,R1),=C'REPORT TOTALS'                                       
         LA    R1,16(R1)                                                        
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         SPACE 1                                                                
VROWDRON NTR1                      VALIDATE A ROW                               
         MVI   DRACTION,DRROW                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GROWDRON NTR1                      GENERATE A ROW                               
         MVI   DRACTION,DRGENROW                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCOLDRON NTR1                      VALIDATE A COLUMN                            
         MVI   DRACTION,DRCOL                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCOLDRON NTR1                      GENERATE A COLUMN                            
         MVI   DRACTION,DRGENCOL                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCMPDRON NTR1                      VALIDATE A COMP                              
         MVI   DRACTION,DRCMP                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCMPDRON NTR1                      GENERATE A COMP                              
         MVI   DRACTION,DRGENCMP                                                
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
         SPACE 1                                                                
VWRPDRON DS    0H                  WRAP UP                                      
         MVI   DRACTION,DRWRAPUP                                                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                                                              
         SPACE 1                                                                
VUSRDRON NTR1                      VALIDATE USER RECORD                         
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGYALPHA         KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BNE   BADUSER                                                          
         TM    DROPTSO,DRBKMINO    IF BRACKET=M SPECIFIED                       
         BNO   XIT                                                              
         NI    DROPTSO,X'DF'       DON'T NEED MINUS=YES                         
         CLI   DRFLOATO,C'-'                                                    
         BNE   XIT                                                              
         MVI   DRFLOATO,0                OR FLOAT=-                             
         B     XIT                                                              
         EJECT                                                                  
*              MORE DRONE UTILITIES                                             
         SPACE 3                                                                
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                                                              
         SPACE 1                                                                
ALLDRONE GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                 USER NEEDS TO TEST DRERROR                   
         SPACE 1                                                                
BADDRONE MVC   CONHEAD,DRERRMSG    ERROR - SO SHOW WHAT DRONE PASSED            
         B     MYCURSOR                                                         
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE 1                                                                
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE 1                                                                
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 CALLOV,DMCB,X'9A000000',0,0  LOAD T6??9A(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A8A'   LOAD T00A8A (TALENT DRIVER)          
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
         MVI   GLNORBOX,X'40'       TURN OFF ROW BOXES FOR TOTALS               
         MVC   GLDOWNLD,DOWNOPT     PASS THROUGH DOWN LOAD OPTIONS              
         TM    GLDOWNLD,GLDLACTV    IF DOWNLOADING                              
         BZ    DRI1                                                             
         OI    GLDWNLD2,GLDCHGD5    CHANGE D5 DATE FORMAT                       
         CLI   WIDEOPT,C'Y'                                                     
         BNE   DRI1                                                             
         OI    GLDWNLD2,GLD2128     DOWNLOAD FIELDS > 128 CHARS                 
                                                                                
DRI1     MVC   GLINDS,DRINDS        PASS THROUGH DRIVER INDICATORS              
         MVC   GLINDS2,DRINDS2                                                  
         MVC   GLRNKMAX,MYRNKMAX    AND RANK MAXES                              
         SPACE 1                                                                
DRI2     CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   DRI4                                                             
         MVI   GLTRACE,C'Y'                                                     
         SPACE 1                                                                
DRI4     DS    0H                                                               
         EJECT                                                                  
*              INITIALIZATION OF PRINT RELATED FIELDS                           
         SPACE 3                                                                
VINTHEAD L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   XIT                                                              
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         SPACE 1                                                                
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 3                                                                
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,3                                                          
         B     VEXIT                                                            
         SPACE 1                                                                
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         SPACE 1                                                                
XITR1    XIT1  REGS=(R1)                                                        
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         ZIC   R1,PFAID            CHECK PF NUMBER                              
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CH    R1,=H'12'           MAKE 15, 16 ETC                              
         BL    *+8                                                              
         SH    R1,=H'12'           EQUIV TO 3, 4 ETC                            
         STC   R1,PFAID                                                         
         SPACE 1                                                                
DI2      L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R4,RA               INTO TWA                                     
         ZIC   R0,MAX                                                           
         SPACE 1                                                                
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE 1                                                                
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,5                                                          
         BE    COPY2                                                            
         CLI   PFAID,6                                                          
         BE    COPY2                                                            
         XC    BLOCK(80),BLOCK                                                  
         SPACE 1                                                                
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
         SPACE 1                                                                
INSFOUND MVC   CONHEAD(L'INSMESS),INSMESS                                       
         L     R4,ATIOB                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R2,RA               INTO TWA                                     
         B     VERRX2                                                           
         SPACE 1                                                                
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE 1                                                                
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         MVC   CONHEAD(L'DELMESS),DELMESS                                       
         B     VERRX2                                                           
         SPACE 1                                                                
COPY2    LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   PFAID,6                                                          
         BNE   *+8                                                              
         BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         ZIC   R1,0(R2)            GET L'FIELD TO R1                            
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LAST INTO THIS                          
         MVC   5(1,R2),5(R3)                                                    
         OI    6(R2),X'80'                                                      
         OI    6(R3),X'80'                                                      
         MVC   CONHEAD(L'CPYMESS),CPYMESS                                       
         B     VERRX2                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE 1                                                                
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         ZIC   R0,5(R2)            R0 HAS FIELD LENGTH                          
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
         SPACE 1                                                                
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
         SPACE 1                                                                
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
         SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
         SPACE 1                                                                
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VERRXIT                                                          
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
FINV     MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
         SPACE 1                                                                
PASSEXP  MVI   ERROR,ERPASEXP      PASSWORD EXPIRED                             
         LA    R2,CONPASSH                                                      
         B     VERRXIT                                                          
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'                                                      
         B     VERRXIT                                                          
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         SPACE 1                                                                
VEXIT    DS    0H                                                               
VERRXIT  OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
         CLI   ERROR,X'FE'                                                      
         BE    VERRX2                                                           
         GOTO1 ERREX               SYSTEM MESSAGE                               
         SPACE 1                                                                
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
MONLIST  DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
WILDCRDS DC    6C'*'                                                            
         SPACE 1                                                                
*                                  ERROR MESSAGES                               
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
FLTERR   DC    C'** ERROR ** INVALID FILTER'                                    
LSTERR   DC    C'** ERROR ** MISSING FILTER LIST'                               
LNGERR   DC    C'** ERROR ** CODE TOO LONG'                                     
ROWERR   DC    C'** ERROR ** INVALID ROW EXPRESSION'                            
ROWONLY  DC    C'** ERROR ** NOT ALLOWED AS A COLUMN'                           
COLERR   DC    C'** ERROR ** INVALID COLUMN EXPRESSION'                         
USRERR   DC    C'** ERROR ** CANT FIND USER RECORD'                             
COLONLY  DC    C'** ERROR ** NOT ALLOWED AS A ROW'                              
NOSTKERR DC    C'** ERROR ** STACK NOT DEFINED'                                 
HOVERR   DC    C'HEADING OVERRIDE IS WIDER THAN COLUMN'                         
*ERERR   DC    C'** ERROR ** INVALID PERIOD EXPRESSION'                         
*EXERR   DC    C'PERIOD EXPRESSION INCONSISTENT WITH REQUEST'                   
NEED1ERR DC    C'MUST BE AT LEAST 1 ROW AND 1 COLUMN'                           
LRANKERR DC    C'NOT VALID TO RANK ON LAST ROW'                                 
AUTHERR  DC    C'ONLY TP AUTHORIZED TO CHANGE THIS FIELD'                       
INSMESS  DC    C'NEW FIELD INSERTED ON SCREEN'                                  
DELMESS  DC    C'FIELD DELETED ON SCREEN'                                       
CPYMESS  DC    C'FIELD COPIED ON SCREEN'                                        
                                                                                
*                                  NEW SS/PID HEADINGS                          
PIDHEAD  DC    C'PID#'                                                          
PPIDHEAD DC    C'PAYEE PID#'                                                    
         SPACE 1                                                                
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
         SPACE 1                                                                
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF FILTER EXPRESSIONS                                      
         SPACE 3                                                                
FILTTAB  DS    0H                                                               
FILTENT  DS    0CL13                                                            
*                                                            FILTER             
*                EXPRESSION    DISPLACEMENT         LENGTH   LEVEL              
*                ----------    ------------         ------   -----              
*                                                                               
         DC    C'AGROUP  ',AL2(TIFAGG-TIFILTS),AL2(L'TIFAGG),AL1(0)             
         DC    C'AGENCY  ',AL2(TIFAGY-TIFILTS),AL2(L'TIFAGY),AL1(0)             
         DC    C'CGROUP  ',AL2(TIFCLG-TIFILTS),AL2(L'TIFCLG),AL1(0)             
         DC    C'CLIENT  ',AL2(TIFCLI-TIFILTS),AL2(L'TIFCLI),AL1(0)             
         DC    C'PGROUP  ',AL2(TIFPRG-TIFILTS),AL2(L'TIFPRG),AL1(0)             
         DC    C'PRODUCT ',AL2(TIFPRD-TIFILTS),AL2(L'TIFPRD),AL1(0)             
         DC    C'ISCII   ',AL2(TIFISCI-TIFILTS),AL2(L'TIFISCI)                  
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'CID     ',AL2(TIFCID-TIFILTS),AL2(L'TIFCID)                    
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'PERF    ',AL2(TIFSSN-TIFILTS),AL2(L'TIFSSN),AL1(0)             
         DC    C'SSN     ',AL2(TIFSSN-TIFILTS),AL2(L'TIFSSN),AL1(0)             
         DC    C'SS#     ',AL2(TIFSSN-TIFILTS),AL2(L'TIFSSN),AL1(0)             
         DC    C'MUSIC   ',AL2(TIFMUSIC-TIFILTS),AL2(L'TIFMUSIC),AL1(0)         
         DC    C'W4TYPE  ',AL2(TIFW4TY-TIFILTS),AL2(L'TIFW4TY),AL1(0)           
         DC    C'W4W4    ',AL2(TIFW4W4-TIFILTS),AL2(L'TIFW4W4),AL1(0)           
         DC    C'AGENT   ',AL2(TIFAGT-TIFILTS),AL2(L'TIFAGT)                    
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'OFFICE  ',AL2(TIFOFF-TIFILTS),AL2(L'TIFOFF),AL1(0)             
         DC    C'TPOFFICE',AL2(TIFOFF-TIFILTS),AL2(L'TIFOFF),AL1(0)             
         DC    C'EMPLOYER',AL2(TIFEMP-TIFILTS),AL2(L'TIFEMP),AL1(0)             
         DC    C'ESTIMATE',AL2(TIFEST-TIFILTS),AL2(L'TIFEST),AL1(0)             
         DC    C'CAM     ',AL2(TIFONOF-TIFILTS),AL2(L'TIFONOF)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'F1      ',AL2(TIFFILT1-TIFILTS),AL2(L'TIFFILT1),AL1(0)         
         DC    C'F2      ',AL2(TIFFILT2-TIFILTS),AL2(L'TIFFILT2),AL1(0)         
         DC    C'F3      ',AL2(TIFFILT3-TIFILTS),AL2(L'TIFFILT3),AL1(0)         
         DC    C'F4      ',AL2(TIFFILT4-TIFILTS),AL2(L'TIFFILT4),AL1(0)         
         DC    C'GUAR    ',AL2(TIFGUA-TIFILTS),AL2(L'TIFGUA)                    
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'EPISODE ',AL2(TIFEPI-TIFILTS),AL2(L'TIFEPI),AL1(0)             
         DC    C'ID      ',AL2(TIFID-TIFILTS),AL2(L'TIFID),AL1(0)               
         DC    C'CATEGORY',AL2(TIFCAT-TIFILTS),AL2(L'TIFCAT),AL1(0)             
         DC    C'TPC     ',AL2(TIFTPC-TIFILTS),AL2(L'TIFTPC),AL1(0)             
         DC    C'MANAGER ',AL2(TIFMGR-TIFILTS),AL2(L'TIFMGR),AL1(0)             
         DC    C'MEDIA   ',AL2(TIFMED-TIFILTS),AL2(L'TIFMED)                    
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'LASTACT ',AL2(TIFLACT-TIFILTS),AL2(L'TIFLACT),AL1(0)           
         DC    C'UNION   ',AL2(TIFUN-TIFILTS),AL2(L'TIFUN)                      
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'LOCAL   ',AL2(TIFLOCL-TIFILTS),AL2(L'TIFLOCL)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'LENGTH  ',AL2(TIFSEC-TIFILTS),AL2(L'TIFSEC)                    
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'SECONDS ',AL2(TIFSEC-TIFILTS),AL2(L'TIFSEC)                    
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'UNIT    ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'STATE   ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'TSTATE  ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'RSTATE  ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'WSTATE  ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'CITY    ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'TCITY   ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'RCITY   ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'WCITY   ',AL2(TIFUNIT-TIFILTS),AL2(L'TIFUNIT)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'SEX     ',AL2(TIFSEX-TIFILTS),AL2(L'TIFSEX),AL1(0)             
         DC    C'RACE    ',AL2(TIFRACE-TIFILTS),AL2(L'TIFRACE),AL1(0)           
         DC    C'YEAR    ',AL2(TIFYEAR-TIFILTS),AL2(L'TIFYEAR)                  
         DC    AL1(TIFLVLCA+TIFLVLCK)                                           
         DC    C'USE     ',AL2(TIFUSE-TIFILTS),AL2(L'TIFUSE),AL1(0)             
         DC    C'UTYPE   ',AL2(TIFUTYPE-TIFILTS),AL2(L'TIFUTYPE),AL1(0)         
         DC    C'CTYPE   ',AL2(TIFCTYPE-TIFILTS),AL2(L'TIFCTYPE)                
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'PTYPE   ',AL2(TIFPTYPE-TIFILTS),AL2(L'TIFPTYPE),AL1(0)         
         DC    C'ZERO    ',AL2(TIFZERO-TIFILTS),AL2(L'TIFZERO),AL1(0)           
         DC    C'CURRENCY',AL2(TIFCUR-TIFILTS),AL2(L'TIFCUR),AL1(0)             
         DC    C'AREA    ',AL2(TIFAREA-TIFILTS),AL2(L'TIFAREA),AL1(0)           
         DC    C'PUSE    ',AL2(TIFPUSE-TIFILTS),AL2(L'TIFPUSE),AL1(0)           
         DC    C'LOCK    ',AL2(TIFLOCK-TIFILTS),AL2(L'TIFLOCK)                  
         DC    AL1(TIFLVLIN+TIFLVLCO)                                           
         DC    C'ESPD    ',AL2(TIFESPD-TIFILTS),AL2(L'TIFESPD),AL1(0)           
         DC    C'AUTH/PO ',AL2(TIFAUPO-TIFILTS),AL2(L'TIFAUPO),AL1(0)           
         DC    C'SIG     ',AL2(TIFSGNTR-TIFILTS),AL2(L'TIFSGNTR),AL1(0)         
         SPACE 1                                                                
*                                  STATUS 1 TESTS (DSECT VERSION)               
         DC    C'PAID    ',AL2(TAINSPAY),AL2(101),AL1(0)                        
         DC    C'INSPAY  ',AL2(TAINSPAY),AL2(101),AL1(0)                        
         DC    C'APPROVED',AL2(TAINSAPR),AL2(101),AL1(0)                        
         DC    C'INSAPR  ',AL2(TAINSAPR),AL2(101),AL1(0)                        
         DC    C'BI      ',AL2(TAINSBIL),AL2(101),AL1(0)                        
         DC    C'BILLED  ',AL2(TAINSBIL),AL2(101),AL1(0)                        
         DC    C'INSBIL  ',AL2(TAINSBIL),AL2(101),AL1(0)                        
         DC    C'CK      ',AL2(TAINSCHK),AL2(101),AL1(0)                        
         DC    C'CHECKS  ',AL2(TAINSCHK),AL2(101),AL1(0)                        
         DC    C'INSCHK  ',AL2(TAINSCHK),AL2(101),AL1(0)                        
         DC    C'CANCELER',AL2(TAINSCIN),AL2(101),AL1(0)                        
         DC    C'INSCIN  ',AL2(TAINSCIN),AL2(101),AL1(0)                        
         DC    C'HOLD    ',AL2(TAINSHLD),AL2(101),AL1(0)                        
         DC    C'INSHLD  ',AL2(TAINSHLD),AL2(101),AL1(0)                        
         DC    C'CANCELEE',AL2(TAINSCAN),AL2(101),AL1(0)                        
         DC    C'INSCAN  ',AL2(TAINSCAN),AL2(101),AL1(0)                        
         SPACE 1                                                                
*                                  STATUS 2 TESTS                               
         DC    C'URGENT  ',AL2(TAINSURG),AL2(102),AL1(0)                        
         DC    C'INSURG  ',AL2(TAINSURG),AL2(102),AL1(0)                        
         DC    C'INSHLR  ',AL2(TAINSHLR),AL2(102),AL1(0)                        
         DC    C'ADJUST  ',AL2(TAINSADJ),AL2(102),AL1(0)                        
         DC    C'INSADJ  ',AL2(TAINSADJ),AL2(102),AL1(0)                        
         DC    C'INSSCR  ',AL2(TAINSSCR),AL2(102),AL1(0)                        
         DC    C'INSHLP  ',AL2(TAINSHLP),AL2(102),AL1(0)                        
         DC    C'INSRTH  ',AL2(TAINSRTH),AL2(102),AL1(0)                        
         SPACE 1                                                                
*                                  PAYMENT STATUS                               
         DC    C'CASTSEL ',AL2(TAPDPSEL),AL2(103),AL1(0)                        
         DC    C'PDPSEL  ',AL2(TAPDPSEL),AL2(103),AL1(0)                        
         DC    C'CREDIT  ',AL2(TAPDPCRD),AL2(103),AL1(0)                        
         DC    C'PDPCRD  ',AL2(TAPDPCRD),AL2(103),AL1(0)                        
         DC    C'BNP     ',AL2(TAPDPBNP),AL2(103),AL1(0)                        
         DC    C'PDPBNP  ',AL2(TAPDPBNP),AL2(103),AL1(0)                        
         DC    C'DISPDET ',AL2(TAPDPDTL),AL2(103),AL1(0)                        
         DC    C'PDPDTL  ',AL2(TAPDPDTL),AL2(103),AL1(0)                        
         SPACE 1                                                                
         DC    C'PDSMAN  ',AL2(TAPDSMAN),AL2(104),AL1(0)                        
         DC    C'PDSCAN  ',AL2(TAPDSCAN),AL2(104),AL1(0)                        
         DC    C'PDSLFT  ',AL2(TAPDSLFT),AL2(104),AL1(0)                        
         DC    C'PDSDDS  ',AL2(TAPDSDDS),AL2(104),AL1(0)                        
         DC    C'PDSGUP  ',AL2(TAPDSGUP),AL2(104),AL1(0)                        
         DC    C'PDSGOF  ',AL2(TAPDSGOF),AL2(104),AL1(0)                        
         DC    C'PDSCNL  ',AL2(TAPDSCNL),AL2(104),AL1(0)                        
         SPACE 1                                                                
*                                  PAYMENT OPTIONS                              
         DC    C'PDOAPH  ',AL2(TAPDOAPH),AL2(105),AL1(0)                        
         DC    C'PDONAC  ',AL2(TAPDONAC),AL2(105),AL1(0)                        
         DC    C'PDONGC  ',AL2(TAPDONGC),AL2(105),AL1(0)                        
         DC    C'PDOCAN  ',AL2(TAPDOCAN),AL2(105),AL1(0)                        
         DC    C'PDOPHR  ',AL2(TAPDOPHR),AL2(105),AL1(0)                        
         DC    C'PDOTAX  ',AL2(TAPDOTAX),AL2(105),AL1(0)                        
         DC    C'PDOHND  ',AL2(TAPDOHND),AL2(105),AL1(0)                        
         SPACE 1                                                                
         DC    C'PDOURG  ',AL2(TAPDOURG),AL2(106),AL1(0)                        
         DC    C'PDODCL  ',AL2(TAPDODCL),AL2(106),AL1(0)                        
         DC    C'PDODAY  ',AL2(TAPDODAY),AL2(106),AL1(0)                        
         DC    C'PDODAL  ',AL2(TAPDODAL),AL2(106),AL1(0)                        
         DC    C'PDOFCR  ',AL2(TAPDOFCR),AL2(106),AL1(0)                        
         DC    C'PDOAPS  ',AL2(TAPDOAPS),AL2(106),AL1(0)                        
         DC    C'PDOPTU  ',AL2(TAPDOPTU),AL2(106),AL1(0)                        
         DC    C'PDOPLU  ',AL2(TAPDOPLU),AL2(106),AL1(0)                        
         SPACE 1                                                                
         DC    C'PDONUS  ',AL2(TAPDONUS),AL2(107),AL1(0)                        
         DC    C'PDOHNW  ',AL2(TAPDOHNW),AL2(107),AL1(0)                        
         DC    C'PDOCCR  ',AL2(TAPDOCCR),AL2(107),AL1(0)                        
         DC    C'PDODUM  ',AL2(TAPDODUM),AL2(107),AL1(0)                        
         DC    C'PDOGRY  ',AL2(TAPDOGRY),AL2(107),AL1(0)                        
         DC    C'PDORET  ',AL2(TAPDORET),AL2(107),AL1(0)                        
         SPACE 1                                                                
*                                  ADJUSTMENT INDICS                            
         DC    C'PDADVD  ',AL2(TAPDADVD),AL2(108),AL1(0)                        
         DC    C'PDADRI  ',AL2(TAPDADRI),AL2(108),AL1(0)                        
         DC    C'PDADIS  ',AL2(TAPDADIS),AL2(108),AL1(0)                        
         DC    C'PDADTR  ',AL2(TAPDADTR),AL2(108),AL1(0)                        
         DC    C'PDADTT  ',AL2(TAPDADTT),AL2(108),AL1(0)                        
         DC    C'PDADST  ',AL2(TAPDADST),AL2(108),AL1(0)                        
         SPACE 1                                                                
*                                  ADJUSTMENT INDICS                            
         DC    C'CDSLIN  ',AL2(TACDSLIN),AL2(109),AL1(0)                        
         DC    C'CDSVOI  ',AL2(TACDSVOI),AL2(109),AL1(0)                        
         DC    C'CDSSTA  ',AL2(TACDSSTA),AL2(109),AL1(0)                        
         DC    C'CDSTRUST',AL2(TACDSTRS),AL2(109),AL1(0)                        
         SPACE 1                                                                
*                                  DUE COMPANY STATUS                           
         DC    C'DUSAGY  ',AL2(TADUSAGY),AL2(131),AL1(0)                        
         DC    C'DUSCLI  ',AL2(TADUSCLI),AL2(131),AL1(0)                        
         DC    C'DUSAUT  ',AL2(TADUSAUT),AL2(131),AL1(0)                        
         DC    C'DUSCAN  ',AL2(TADUSCAN),AL2(131),AL1(0)                        
         DC    C'DUSHLD  ',AL2(TADUSHLD),AL2(131),AL1(0)                        
         SPACE 1                                                                
*                                  COMMERCIAL STATUS                            
         DC    C'COSTLO  ',AL2(TACOSTLO),AL2(132),AL1(0)                        
         DC    C'COSTNO  ',AL2(TACOSTNO),AL2(132),AL1(0)                        
         DC    C'COSTRL  ',AL2(TACOSTRL),AL2(132),AL1(0)                        
         DC    C'COSCAN  ',AL2(TACOSCAN),AL2(132),AL1(0)                        
         DC    C'COSCRT  ',AL2(TACOSCRT),AL2(132),AL1(0)                        
         DC    C'COSWDT  ',AL2(TACOSWDT),AL2(132),AL1(0)                        
         DC    C'COSRES  ',AL2(TACOSRES),AL2(132),AL1(0)                        
         SPACE 1                                                                
         DC    C'RUN     ',AL2(0),AL2(110),AL1(0) RUN DATE FILT                 
         DC    C'INVOICE ',AL2(0),AL2(133),AL1(0) INVOICE FILT                  
         DC    C'CUSTOM  ',AL2(0),AL2(120),AL1(0) CUSTOM FILT                   
         DC    C'CKGT5000',AL2(1),AL2(120),AL1(0) KEYWORD CUSTOM FILTS          
         DC    C'BSHLD   ',AL2(2),AL2(120),AL1(0) BS* + HLD                     
         DC    C'CLACAB  ',AL2(3),AL2(120),AL1(0) CLA + CAB                     
         DC    C'AIRDATE ',AL2(4),AL2(120),AL1(0) CHECK AIRDT V PERIOD          
         DC    C'NOLINGEN',AL2(5),AL2(120),AL1(0) NO CKS GEN BY LIENS           
         DC    C'JANCYC  ',AL2(6),AL2(120),AL1(0) FFCYC CHECKING                
         DC    C'FEBCYC  ',AL2(7),AL2(120),AL1(0)                               
         DC    C'MARCYC  ',AL2(8),AL2(120),AL1(0)                               
         DC    C'APRCYC  ',AL2(6),AL2(120),AL1(0)                               
         DC    C'MAYCYC  ',AL2(7),AL2(120),AL1(0)                               
         DC    C'JUNCYC  ',AL2(8),AL2(120),AL1(0)                               
         DC    C'JULCYC  ',AL2(6),AL2(120),AL1(0)                               
         DC    C'AUGCYC  ',AL2(7),AL2(120),AL1(0)                               
         DC    C'SEPCYC  ',AL2(8),AL2(120),AL1(0)                               
         DC    C'OCTCYC  ',AL2(6),AL2(120),AL1(0)                               
         DC    C'NOVCYC  ',AL2(7),AL2(120),AL1(0)                               
         DC    C'DECCYC  ',AL2(8),AL2(120),AL1(0)                               
         DC    C'HLDUNUSE',AL2(9),AL2(120),AL1(0) HLDING FEES UNUSED            
         DC    C'PURPRINT',AL2(10),AL2(120),AL1(0) COD INVS PRINTED             
         DC    C'SAGADD  ',AL2(11),AL2(120),AL1(0) SAG ADDRESSES                
         DC    C'AFTADD  ',AL2(12),AL2(120),AL1(0) AFTRA ADDRESSES              
         DC    C'FTRETRO ',AL2(13),AL2(120),AL1(0) RETRO FTRACK RECS            
         DC    C'DUSHLD  ',AL2(14),AL2(120),AL1(0) DUE COMP HELD RECS           
         DC    C'CRSGUA  ',AL2(15),AL2(120),AL1(0) TYPE 6 FIXED GUAR            
         DC    C'ACCRUED ',AL2(16),AL2(120),AL1(0) ACCRUED (NO CHKS)            
         DC    C'ESTADD  ',AL2(17),AL2(120),AL1(0) ESTATE ADDRESSES             
         DC    C'W4RESNEQ',AL2(18),AL2(120),AL1(0) W4 RES NEQ W/HOLDING         
         DC    C'MULTCORP',AL2(19),AL2(120),AL1(0) MULTIPLE CORPS               
         DC    C'CRPNOIND',AL2(20),AL2(120),AL1(0) CKS TO CRPS W/NO IND         
         DC    C'DEFAULT ',AL2(21),AL2(120),AL1(0) CHKS TO DEFLT ST             
         DC    C'AFMADD  ',AL2(22),AL2(120),AL1(0) AFM ADDRESSES                
         DC    C'RETURNED',AL2(23),AL2(120),AL1(0) RETURNED CHECKS              
         DC    C'MAILED  ',AL2(24),AL2(120),AL1(0)  " & MAILED CKS              
         DC    C'FILED   ',AL2(25),AL2(120),AL1(0)  " & FILED CKS               
         DC    C'DIRECT  ',AL2(26),AL2(120),AL1(0) DIRCT DEPOSIT & WIRE         
         DC    C'RGEN    ',AL2(27),AL2(120),AL1(0) GEN. ADVICES(NETTAL)         
         DC    C'RGENUNPD',AL2(28),AL2(120),AL1(0)  "    "  NOT PAID            
         DC    C'HLDUNMTH',AL2(29),AL2(120),AL1(0) UNMATCHED HOLD RECS          
         DC    C'ACTIVEW4',AL2(30),AL2(120),AL1(0) PRFS W/CKS IN PERIOD         
         DC    C'OVSCALE ',AL2(31),AL2(120),AL1(0) CHECKS W/OVERSCALE           
         DC    C'NOSRC   ',AL2(32),AL2(120),AL1(0) NO SRC                       
         DC    C'NOTRSGEN',AL2(33),AL2(120),AL1(0) NO CKS GEN BY W4TRST         
         DC    C'TRSNOIND',AL2(34),AL2(120),AL1(0) CKS TO TRSS W/NO IND         
         DC    C'CKGT10K ',AL2(35),AL2(120),AL1(0) KEYWORD CUSTOM FILTS         
         DC    C'STOPPED ',AL2(36),AL2(120),AL1(0) STOPPED CHECKS               
         DC    C'EFT     ',AL2(37),AL2(120),AL1(0) EFT CHECKS ONLY              
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
SETSYS   NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        IF OFFLINE,                                  
         BNE   SETSYS10                                                         
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   RF,15,4(R1)         =A(SSB)                                      
         CLC   0(2,RF),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         USING SSOOFF,RF                                                        
         CLI   SSODSPAC-SSOOFF(RF),C'Q'       DSPACE MUST BE Q T OR C           
         BNE   *+8                                                              
         OI    PRGSTAT,FQASYS                                                   
         CLI   SSODSPAC-SSOOFF(RF),C'C'                                         
         BNE   *+8                                                              
         OI    PRGSTAT,CSCSYS                                                   
         CLI   SSODSPAC-SSOOFF(RF),C'T'                                         
         BNE   *+8                                                              
         OI    PRGSTAT,TESTSYS                                                  
         B     SETSYSX                                                          
         DROP  RF                                                               
*                                                                               
SETSYS10 GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         L     RE,DMCB                                                          
         USING FACTSD,RE                                                        
         CLI   FASYSID,11          CHECK FOR CSC SYSTEM                         
         BNE   *+8                                                              
         OI    PRGSTAT,CSCSYS                                                   
         CLI   FASYSID,15          CHECK FOR FQA SYSTEM                         
         BNE   *+8                                                              
         OI    PRGSTAT,FQASYS                                                   
         CLI   FASYSID,1           CHECK FOR TEST SYSTEM                        
         BNE   *+8                                                              
         OI    PRGSTAT,TESTSYS                                                  
         DROP  RE                                                               
SETSYSX  J     XIT                                                              
         LTORG                                                                  
*                                                                               
       ++INCLUDE TALURACT                                                       
*              INSERT DELETE UNPROTECTED FIELDS                                 
         SPACE 3                                                                
*              INPUT               R2=A(FIRST UNPROTECTED FIELD)                
*                                  MAX=NUMBER OF INPUT FIELDS                   
         SPACE 1                                                                
CHKSSN   NTR1  BASE=*,LABEL=*                                                   
         TM    TGSYSTAT,TASYSPID                                                
         JZ    XIT                                                              
         LA    R1,DRHEAD1                                                       
         CLC   12(4,R4),=C'SSN '   LOOK FOR THE 6 SSN FIELDS                    
         JE    CHKSSN2                                                          
         CLC   12(6,R4),=C'ANSSN '                                              
         JE    CHKSSN2                                                          
         CLC   12(6,R4),=C'CASSN '                                              
         JE    CHKSSN2                                                          
         CLC   12(7,R4),=C'LNPAYE '                                             
         JE    CHKSSN2                                                          
         LA    R1,DRHEAD2                                                       
         CLC   12(7,R4),=C'WXTSSN '                                             
         JE    CHKSSN2                                                          
         CLC   12(8,R4),=C'NUTTRST '                                            
         JNE   XIT                                                              
                                                                                
CHKSSN2  XC    0(64,R1),0(R1)                                                   
         OI    0(R1),X'80'                                                      
         MVI   27(R1),L'PIDHEAD                                                 
         MVC   28(L'DRH1LIT,R1),SPACES                                          
         MVC   28(L'PIDHEAD,R1),PIDHEAD                                         
         CLC   12(7,R4),=C'LNPAYE '                                             
         JNE   XIT                                                              
         MVI   27(R1),L'PPIDHEAD                                                
         MVC   28(L'PPIDHEAD,R1),PPIDHEAD                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
VALSTACK NTR1  BASE=*,LABEL=*                                                   
         ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         JZ    BADOPT                                                           
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R3,22(R4)           (BUMP TO EXPRESSION)                         
         LA    R4,STACKDEF                                                      
         USING STKENTD,R4                                                       
         LA    R0,8                (MAX 8 TERMS)                                
         SPACE 1                                                                
VSTK2    LA    RE,1(R3)            GET LENGTH OF NEXT TERM                      
         LA    RF,1                                                             
         SPACE 1                                                                
VSTK4    CLI   0(RE),C'/'                                                       
         JE    VSTK6                                                            
         CH    RF,=H'8'                                                         
         JE    BADOPT                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         J     VSTK4                                                            
         SPACE 1                                                                
VSTK6    L     RE,=A(STACKTAB)                                                  
         A     RE,RELO                                                          
         BCTR  RF,0                                                             
         SPACE 1                                                                
VSTK8    EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R3),0(RE)                                                    
         JE    VSTK10                                                           
         CLI   0(RE),X'FF'                                                      
         JE    VSTK12                                                           
         LA    RE,12(RE)                                                        
         J     VSTK8                                                            
         SPACE 1                                                                
VSTK10   MVC   STKEL,9(RE)         SAVE STACK PARAMETERS                        
         MVC   STKROUT,10(RE)                                                   
         LA    R4,L'STKENT(R4)                                                  
         LA    R3,2(R3,RF)         BUMP PAST NEXT / TO NEXT TERM                
         MVI   0(R4),X'FF'         DELIMIT STACK DEFINITION                     
         CLI   0(R3),C' '          MAY HAVE FINISHED STACK NOW                  
         JE    XIT                                                              
         MVI   0(R4),0                                                          
         BCT   R0,VSTK2                                                         
         J     BADOPT                                                           
         SPACE 1                                                                
VSTK12   MVI   STKCON,STKCDET      MAY BE A DXXX EXPRESSION                     
         CLI   0(R3),C'D'                                                       
         JE    VSTK14                                                           
         MVI   STKCON,STKCTOT          OR A TXXX EXPRESSION                     
         CLI   0(R3),C'T'                                                       
         JE    VSTK14                                                           
         J     BADOPT                                                           
         SPACE 1                                                                
VSTK14   LA    R3,1(R3)            SO, BUMP PAST THE T OR D                     
*                                      (RF, LENGTH ALREADY REDUCED)             
         J     VSTK6                   AND HAVE ANOTHER SHOT                    
         LTORG                                                                  
         EJECT                                                                  
*              STACK DEFINITION TABLE                                           
         SPACE 3                                                                
STACKTAB DS    0F                                                               
         DC    C'SPACE   ',AL1(0),AL1(0),AL1(1),AL1(0)                          
         DC    C'SUB*    ',AL1(0),AL1(0),AL1(2),AL1(0)                          
         DC    C'*       ',AL1(0),AL1(0),AL1(3),AL1(0)                          
         DC    C'DIFF    ',AL1(0),AL1(0),AL1(4),AL1(0)                          
         DC    C'BDTOT   ',AL1(0),AL1(TABDELQ),AL1(3),AL1(0)                    
         DC    C'TOT     ',AL1(0),AL1(TABDELQ),AL1(3),AL1(0)                    
         DC    C'BDTAX   ',AL1(0),AL1(TABDELQ),AL1(4),AL1(0)                    
         DC    C'TAX     ',AL1(0),AL1(TABDELQ),AL1(4),AL1(0)                    
         DC    C'BDHND   ',AL1(0),AL1(TABDELQ),AL1(5),AL1(0)                    
         DC    C'HND     ',AL1(0),AL1(TABDELQ),AL1(5),AL1(0)                    
         DC    C'BDHNDC  ',AL1(0),AL1(TABDELQ),AL1(6),AL1(0)                    
         DC    C'HNDC    ',AL1(0),AL1(TABDELQ),AL1(6),AL1(0)                    
         DC    C'BDCSF   ',AL1(0),AL1(TABDELQ),AL1(7),AL1(0)                    
         DC    C'CSF     ',AL1(0),AL1(TABDELQ),AL1(7),AL1(0)                    
         DC    C'BDFICR  ',AL1(0),AL1(TABDELQ),AL1(8),AL1(0)                    
         DC    C'FICR    ',AL1(0),AL1(TABDELQ),AL1(8),AL1(0)                    
         DC    C'BDHAND  ',AL1(0),AL1(TABDELQ),AL1(9),AL1(0)                    
         DC    C'HAND    ',AL1(0),AL1(TABDELQ),AL1(9),AL1(0)                    
         DC    C'BDHAND+ ',AL1(0),AL1(TABDELQ),AL1(10),AL1(0)                   
         DC    C'HAND+   ',AL1(0),AL1(TABDELQ),AL1(10),AL1(0)                   
         DC    C'BDGST   ',AL1(0),AL1(TABDELQ),AL1(12),AL1(0)                   
         DC    C'GST     ',AL1(0),AL1(TABDELQ),AL1(12),AL1(0)                   
         DC    C'BDPST   ',AL1(0),AL1(TABDELQ),AL1(15),AL1(0)                   
         DC    C'PST     ',AL1(0),AL1(TABDELQ),AL1(15),AL1(0)                   
         SPACE 1                                                                
         DC    C'PDGRS   ',AL1(0),AL1(TAPDELQ),AL1(11),AL1(0)                   
         DC    C'GRS     ',AL1(0),AL1(TAPDELQ),AL1(11),AL1(0)                   
         DC    C'PDAPPL  ',AL1(0),AL1(TAPDELQ),AL1(12),AL1(0)                   
         DC    C'APPL    ',AL1(0),AL1(TAPDELQ),AL1(12),AL1(0)                   
         DC    C'PDGUAR  ',AL1(0),AL1(TAPDELQ),AL1(13),AL1(0)                   
         DC    C'GUAR    ',AL1(0),AL1(TAPDELQ),AL1(13),AL1(0)                   
         DC    C'PDPAY   ',AL1(0),AL1(TAPDELQ),AL1(41),AL1(0)                   
         DC    C'PAY     ',AL1(0),AL1(TAPDELQ),AL1(41),AL1(0)                   
         DC    C'PDPAYI  ',AL1(0),AL1(TAPDELQ),AL1(14),AL1(0)                   
         DC    C'PAYI    ',AL1(0),AL1(TAPDELQ),AL1(14),AL1(0)                   
         DC    C'PDPAYC  ',AL1(0),AL1(TAPDELQ),AL1(15),AL1(0)                   
         DC    C'PAYC    ',AL1(0),AL1(TAPDELQ),AL1(15),AL1(0)                   
         DC    C'PDREXP  ',AL1(0),AL1(TAPDELQ),AL1(16),AL1(0)                   
         DC    C'REXP    ',AL1(0),AL1(TAPDELQ),AL1(16),AL1(0)                   
         DC    C'PDSPNH  ',AL1(0),AL1(TAPDELQ),AL1(17),AL1(0)                   
         DC    C'SPNH    ',AL1(0),AL1(TAPDELQ),AL1(17),AL1(0)                   
         DC    C'PDMDED  ',AL1(0),AL1(TAPDELQ),AL1(18),AL1(0)                   
         DC    C'MDED    ',AL1(0),AL1(TAPDELQ),AL1(18),AL1(0)                   
         DC    C'PDPNH   ',AL1(0),AL1(TAPDELQ),AL1(19),AL1(0)                   
         DC    C'PNH     ',AL1(0),AL1(TAPDELQ),AL1(19),AL1(0)                   
         DC    C'PDHNW   ',AL1(0),AL1(TAPDELQ),AL1(20),AL1(0)                   
         DC    C'HNW     ',AL1(0),AL1(TAPDELQ),AL1(20),AL1(0)                   
         DC    C'PDINR   ',AL1(0),AL1(TAPDELQ),AL1(21),AL1(0)                   
         DC    C'INR     ',AL1(0),AL1(TAPDELQ),AL1(21),AL1(0)                   
         DC    C'PDAPPNH ',AL1(0),AL1(TAPDELQ),AL1(22),AL1(0)                   
         DC    C'APPNH   ',AL1(0),AL1(TAPDELQ),AL1(22),AL1(0)                   
         DC    C'PDPAY+  ',AL1(0),AL1(TAPDELQ),AL1(42),AL1(0)                   
         DC    C'PAY+    ',AL1(0),AL1(TAPDELQ),AL1(42),AL1(0)                   
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TAREPWKD                                                       
         EJECT                                                                  
*              DSECT TO COVER STACK DEFINITION ENTRY                            
         SPACE 1                                                                
STKENTD  DSECT                                                                  
STKENT   DS    0CL3                                                             
STKCON   DS    XL1                                                              
STKCDET  EQU   X'80'               ONLY APPLIES TO DETAILS                      
STKCTOT  EQU   X'40'               ONLY APPLIES TO TOTALS                       
STKEL    DS    XL1                                                              
STKROUT  DS    XL1                                                              
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
TREPGEND DSECT                                                                  
RELO     DS    F                                                                
EDITLIST DS    XL64                                                             
         SPACE 2                                                                
       ++INCLUDE TAREPWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAREPFFD                                                                       
*DDGENTWA                                                                       
*TAGENFILE                                                                      
*TASYSEQUS                                                                      
*CTGENFILE                                                                      
*FASSBOFF                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*DDCOMFACS                                                                      
*DRGLOBAL                                                                       
*DRDICFILE                                                                      
*DDBIGBOX                                                                       
*DDPERVALD                                                                      
*DDWIDED                                                                        
*DDMASTD                                                                        
*DDREMOTED                                                                      
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAREPFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
*********INCLUDE DRDICFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032TAREPGEN  01/21/16'                                      
         END                                                                    
