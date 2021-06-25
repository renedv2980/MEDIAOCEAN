*          DATA SET TAGEN1C    AT LEVEL 127 AS OF 09/09/15                      
*PHASE T7021CE                                                                  
         TITLE 'T7021C - W4 MAINTENANCE'                                        
T7021C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021C,R7,RR=R2                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         TM    STATUS,STATPFOK                                                  
         BNO   W402                                                             
         OI    TRNSTAT,OKINTPFK    SET FOR INTERNAL PFKEYS                      
*                                                                               
W402     L     R2,=A(PFTABLE)                                                   
         A     R2,RELO                                                          
         GOTO1 INITIAL,DMCB,(R2)                                                
         MVC   SW4PHED(13),=C'Performer Pid'                                    
         OI    SW4PHEDH+6,X'80'                                                 
         MVC   SW4THED(11),=C'Trustee Pid'                                      
         OI    SW4THEDH+6,X'80'                                                 
         MVC   SW4IHED(9),=C'Indiv Pid'                                         
         OI    SW4IHEDH+6,X'80'                                                 
*                                                                               
W402A    CLI   PFAID,20            HIT PF20 TO REPRINT W2 FORM                  
         BE    W403                                                             
         NI    STATUS,X'FF'-STATW2 IF NO, TURN OFF CONFIRM PENDING STAT         
         B     W405                                                             
W403     TM    STATUS,STATW2       IF YES, TEST CONFIRM PENDING                 
         BO    *+8                                                              
         B     W2CONF              IF NOT PENDING, GIVE CONFIRM MESSAGE         
         NI    STATUS,X'FF'-STATW2 ELSE TURN OFF CONFIRM PENDING STAT           
         MVI   W2FLAG,C'N'         FLAG - GIVE W2 REPRINT MESSAGE               
         BAS   RE,W2PRINT                                                       
         CLI   W2FLAG,C'Y'         IF ACTUALLY UPDATED A W2 RECORD              
         BE    W2MSG               GIVE APPROPRIATE MESSAGE                     
         B     NOW2MSG                                                          
*                                                                               
W405     CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   W410                                                             
         LA    R2,SW4SSNH                                                       
         CLI   SW4SSNH+5,0         ARE WE COMING FROM ANOTHER SCREEN?           
         BNE   W405A                                                            
         B     W406C                                                            
*                                                                               
W405A    CLI   SW4SSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BNE   W406                RECVAL CALL DOES NOT CHECK LENGTH            
         MVC   WORK(9),=9X'F0'     CHECK FOR VALID NUMERIC                      
         MVZ   WORK(9),8(R2)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BNE   INVERR                                                           
         B     W407                                                             
*                                                                               
W406     CLI   ACTNUM,ACTADD       IF ADDING NEW W4 RECORD,                     
         BE    INVERR                                                           
*                                                                               
         CLI   SW4SSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SW4SSN                                                     
         LA    RE,6                ENSURE ALL PID CHARACTERS                    
         LA    RF,SW4SSN                                                        
W406A    CLI   0(RF),C'A'          ARE VALID ALPHANUMERIC                       
         BL    INVPID                                                           
         CLI   0(RF),C'9'                                                       
         BH    INVPID                                                           
         CLI   0(RF),C'Z'                                                       
         BNH   W406B                                                            
         CLI   0(RF),C'0'                                                       
         BL    INVPID                                                           
W406B    LA    RF,1(RF)                                                         
         BCT   RE,W406A                                                         
W406C    GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   W407                                                             
         MVC   SW4SSN,TGSSN                                                     
         MVI   SW4SSNH+5,9                                                      
*                                                                               
W407     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'40',SW4SSNH)                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         CLI   ACTNUM,ACTADD       IF ADDING, DON'T CONVERT SSN                 
         BE    W408                TO PID ON SCREEN                             
         BRAS  RE,DISPID                                                        
*                                                                               
W408     XC    SVW4STAT,SVW4STAT                                                
         XC    SVW4TYPE,SVW4TYPE                                                
*                                                                               
*        CLI   ACTNUM,ACTADD       IF ADDING                                    
*        BNE   W4X                                                              
         TM    SW4SSNH+4,X'80'     AND SS# INPUTTED THIS TIME                   
         BZ    W4X                                                              
         TM    SW4DOBH+4,X'80'     & DOB NOT INPUTTED THIS TIME                 
         BO    W409                                                             
         GOTO1 FLDVAL,DMCB,(X'01',SW4DOBH),SW4DOBH CLEAR DOB FIELD              
W409     TM    SW4TSSNH+4,X'80'    & TRUST SS# NOT INPUTTED THIS TIME           
         BO    W4X                                                              
         GOTO1 FLDVAL,DMCB,(X'01',SW4TSSNH),SW4TSSNH CLEAR TSS# FIELD           
         B     W4X                                                              
*                                                                               
         SPACE 3                                                                
W410     CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    W415                                                             
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    W430                                                             
*                                                                               
W415     CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    W450                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    W450                                                             
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    W420                                                             
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    W420                                                             
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    W420                                                             
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   W440                                                             
*                                                                               
W420     GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK) HANDLE PASSIVE - CHANGE ALL          
         CLI   MODE,XRECADD        ON ADD OR CHANGE                             
         BNE   W425                                                             
         BRAS  RE,DISPID                                                        
         B     *+8                                                              
W425     CLI   MODE,XRECPUT        HANDLE ACTIVE POINTER MYSELF                 
         BNE   W430                                                             
         L     R4,AIO                                                           
         MVC   KEY,0(R4)                                                        
         CLC   SVW4TYPE,NEW4TYPE                                                
         BE    W429                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+TLDRSTAT+1-TLDRD(1),NEW4TYPE                                 
         GOTO1 WRITE                                                            
W429     BRAS  RE,UPDETNAM         UPDATE NAME ON EMPLOYEE/EVTIME RECS          
*                                                                               
W430     BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     W4X                                                              
*                                                                               
W440     CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   W4X                                                              
         BAS   RE,BLDREC                                                        
         B     W4X                                                              
*                                                                               
W450     XC    PTRBLK,PTRBLK       CLEAR POINTER BLOCK                          
         GOTO1 SAVPTRS,DMCB,PTRBLK     HANDLE PASSIVE POINTERS                  
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   W4X                                                              
         BAS   RE,CHKDEL                                                        
*                                                                               
W4X      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
DK       L     R4,AIO              ADDRESS OF THE RECORD                        
         USING TLW4D,R4                                                         
         MVC   SW4SSN,TLW4SSN      SOCIAL SECURITY NUMBER                       
         BRAS  RE,DISPID                                                        
         B     XIT                                                              
         EJECT                                                                  
         DROP R4                                                                
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SW4TYPEH                                                         
         OI    SW4DUEH+1,X'0C'     CLEAR PROTECTED FIELDS                       
         OI    SW4DUEH+6,X'80'     TURN TO LOW INTENSITY                        
         OI    SW4LIENH+1,X'0C'                                                 
         OI    SW4LIENH+6,X'80'                                                 
*                                                                               
         OI    SW4PAYEH+1,X'0C'                                                 
         OI    SW4PAYEH+6,X'80'                                                 
*                                                                               
         MVC   SW4ISS,SPACES                                                    
         OI    SW4ISSH+6,X'80'                                                  
         MVC   SW4ISSN,SPACES                                                   
         OI    SW4ISSNH+6,X'80'                                                 
*                                                                               
         MVC   SW4CIDN,SPACES                                                   
         OI    SW4CIDNH+6,X'80'                                                 
*                                                                               
         L     R4,AIO              GET W4 ELEMENT                               
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   SVW4CRPN,TAW4NAM2   SAVE ORIGINAL NAME                           
                                                                                
         CLI   TAW4TYPE,TAW4TYCA   CANADIAN                                     
         BE    DR2                                                              
         CLI   TAW4TYPE,TAW4TYIN   INDIVIDUAL                                   
         BE    DR2                                                              
         CLI   TAW4TYPE,TAW4TYFO   FOREIGNER                                    
         BNE   DR3                                                              
*                                                                               
DR2      MVC   SW4NAM1,TAW4NAM1    DISPLAY FIRST NAME                           
         MVC   SW4NAM2,TAW4NAM2            LAST NAME                            
         CLI   TAW4LEN,TAW4LN2Q    NEW W4 NAME ELEMENT?                         
         BNE   *+16                NO - DON'T DISPLAY MID NAME/SUFFIX           
         MVC   SW4NAM3,TAW4MIDN    DISPLAY MIDDLE NAME                          
         MVC   SW4NAM4,TAW4SUFF            SUFFIX                               
         B     DR4                                                              
*                                                                               
DR3      MVC   SW4CRPN,TAW4CRPN    CORPORATION NAME                             
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    DR4                 IF CORPORTATION OR                           
         CLI   TAW4TYPE,TAW4TYES   IF ESTATE CONTINUE WITH REST                 
         BNE   DR5                    OF INFO                                   
*                                                                               
DR4      MVC   SW4SEX,TAW4SEX              SEX                                  
         MVC   SW4ETH,TAW4RACE             RACE                                 
*                                                                               
DR5      GOTO1 CHAROUT,DMCB,TANUELQ,SW4PHONH,TANUTPHN PHONE NUMBER              
         MVC   SW4TYPE,TAW4TYPE    W4 TYPE C,I,E,F                              
         MVC   SVW4TYPE,TAW4TYPE   SAVE IT                                      
         OI    SW4TYPEH+4,X'20'    SET VALIDATED                                
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,TAW4INDT),(8,SW4IND)                              
         MVC   SW4FREQ,TAW4FREQ    FREQUENCY                                    
         MVC   SW4LOCL,TAW4LOCL    AFM LOCAL                                    
         MVC   SW4RECP,TAW4RECP    RECIPROCAL STATE                             
*                                                                               
         MVI   SW4FTX,C' '                                                      
         CLI   TAW4TYPE,TAW4TYFO   IF TYPE IS FOREIGNER                         
         BE    DR7                                                              
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BNE   DR10                                                             
DR7      MVI   SW4FTX,C'Y'                                                      
         TM    TAW4STA3,TAW4SNTX   DISPLAY "TAKE FOREIGN TAX?"                  
         BZ    DR10                INDICATOR                                    
         MVI   SW4FTX,C'N'                                                      
*                                                                               
DR10     LA    R2,SW4STAT          DISPLAY STATUS                               
         OC    TAW4CP,TAW4CP                                                    
         BZ    DR15                                                             
         MVC   0(3,R2),=C'CP='                                                  
         MVC   3(2,R2),TAW4CP                                                   
         OC    0(10,R2),SPACES                                                  
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
*                                                                               
DR15     L     RF,=A(STATTAB)      1ST STATUS BYTE                              
         A     RF,RELO                                                          
*                                                                               
DR20     CLI   0(RF),X'FF'                                                      
         BE    DR40                                                             
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,TAW4STAT       IS BIT ON IN 1ST STATUS BYTE                 
         BNZ   DR35                   IN ELEMENT                                
*                                                                               
DR30     LA    RF,L'STATTAB(RF)                                                 
         B     DR20                                                             
*                                                                               
DR35     MVC   0(10,R2),1(RF)      YES - DISPLAY LITERAL                        
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
         B     DR30                                                             
*                                                                               
DR40     L     RF,=A(STATTAB2)     2ND STATUS BYTE                              
         A     RF,RELO                                                          
*                                                                               
DR42     CLI   0(RF),X'FF'         FINISHED 2ND BYTE                            
         BE    DR48                                                             
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,TAW4STA2       IS BIT ON IN 2ND STATUS BYTE                 
         BNZ   DR46                   IN ELEMENT                                
*                                                                               
DR44     LA    RF,L'STATTAB(RF)                                                 
         B     DR42                                                             
*                                                                               
DR46     MVC   0(10,R2),1(RF)      YES - DISPLAY LITERAL                        
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
         B     DR44                                                             
*                                                                               
DR48     L     RF,=A(STATTAB3)     3RD STATUS BYTE                              
         A     RF,RELO                                                          
*                                                                               
DR48A    CLI   0(RF),X'FF'         FINISHED 2ND BYTE                            
         BE    DR48Z                                                            
         MVC   BYTE,0(RF)                                                       
         NC    BYTE,TAW4STA3       IS BIT ON IN 2ND STATUS BYTE                 
         BNZ   DR48C                  IN ELEMENT                                
*                                                                               
DR48B    LA    RF,L'STATTAB(RF)                                                 
         B     DR48A                                                            
*                                                                               
DR48C    MVC   0(10,R2),1(RF)      YES - DISPLAY LITERAL                        
         LA    R2,10-1(R2)                                                      
         BAS   RE,SHUFFLE          SHUFFLE BACK TO END AND INSERT COMMA         
         B     DR48B                                                            
*                                                                               
DR48Z    BAS   RE,TRAIL            CLEAR TRAILING COMMA IF NECESSARY            
*                                                                               
         MVI   SW4SLET,C'N'        SPECIAL LETTER ON FILE                       
         MVI   SW4SLETH+5,1                                                     
         OI    SW4SLETH+6,X'80'                                                 
         TM    TAW4STA3,TAW4SSPL                                                
         BZ    *+8                                                              
         MVI   SW4SLET,C'Y'                                                     
*                                                                               
         MVC   SAVSLET,SW4SLET     SAVE SPECIAL LETTER INPUT                    
*                                                                               
         TM    TAW4STAT,TAW4STDU   DUE COMPANY PRESENT                          
         BNO   DR49                                                             
         NI    SW4DUEH+1,X'FB'     HIGH INTENSITY                               
         OI    SW4DUEH+6,X'80'                                                  
*                                                                               
DR49     TM    TAW4STAT,TAW4STLN   LIEN PRESENT                                 
         BNO   DR50                                                             
         NI    SW4LIENH+1,X'FB'    HIGH INTENSITY                               
         OI    SW4LIENH+6,X'80'                                                 
*                                                                               
DR50     TM    TAW4STA3,TAW4SNHA                                                
         BZ    DR50A                                                            
         OC    TAW4NHAD,TAW4NHAD   NO DATE, SHOW Y                              
         BNZ   *+12                                                             
         MVI   SW4NHA,C'Y'                                                      
         B     DR50A                                                            
         GOTO1 DATCON,DMCB,(1,TAW4NHAD),(8,SW4NHA)                              
*                                                                               
DR50A    TM    TAW4STA3,TAW4SNHP                                                
         BZ    *+8                                                              
         MVI   SW4NHA,C'P'                                                      
         OI    SW4NHAH+6,X'80'                                                  
*                                                                               
         GOTO1 CHAROUT,DMCB,TAA2ELQ,(6,SW4ADDRH)   ADDRESS                      
*                                                                               
         USING TAA2D,R4                                                         
         L     R4,AIO              GET ADDRESS ELEMENT                          
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR50B                                                            
         CLI   TAA2LEN,TAA2LNQ                                                  
         BL    DR50B                                                            
         MVC   SW4CTRY,TAA2CTRY    COUNTRY                                      
         DROP  R4                                                               
                                                                                
DR50B    L     R4,AIO              GET AKA ELEMENT                              
         USING TAAKD,R4                                                         
         MVI   ELCODE,TAAKELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR51                                                             
         MVC   SW4AKA1,TAAKNAM1            AKA FIRST NAME                       
         MVC   SW4AKA2,TAAKNAM2            AKA LAST NAME                        
         DROP  R4                                                               
*                                                                               
DR51     L     R4,AIO              GET PAYEE ELEMENT                            
         USING TAPED,R4                                                         
         MVI   ELCODE,TAPEELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR52                                                             
         OC    TAPEADD1(120),TAPEADD1                                           
         BZ    DR52                                                             
         NI    SW4PAYEH+1,X'FB'    HIGH INTENSITY                               
         OI    SW4PAYEH+6,X'80'                                                 
*                                                                               
DR52     L     R4,AIO              GET FILTERS ELEMENT                          
         USING TAFLD,R4                                                         
         MVI   ELCODE,TAFLELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR54                                                             
         MVC   SW4FILT(4),TAFLFLT1                                              
         DROP  R4                                                               
*                                                                               
DR54     SR    R1,R1               COUNT NUM OF CORPS FOR INDIVIDUAL            
         L     R4,AIO              GET TAX ID ELEMENT                           
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL            IS A CORP NUM PRESENT                        
         BNE   DR70                                                             
*                                                                               
DR60     CLI   TATITYPE,TATITYCO   CORP                                         
         BNE   DR65                                                             
         LA    R1,1(R1)            INCREMENT COUNTER                            
         CH    R1,=H'1'            ONLY PUT WORDS 'ON FILE' OUT ONCE            
         BH    *+10                                                             
         MVC   SW4CID,=C'ON FILE  ' AT LEAST ONE CORP NUM EXISTS                
         CLI   TATICRPN,C'1'        BUT ONLY DISPLAY 1ST                        
         BNE   DR65                                                             
         MVC   SW4CID,TATIID       CORP ID                                      
*                                                                               
         ST    R1,TGFULL                                                        
         GOTO1 SSNPACK,DMCB,SW4CID,TGDUB                                        
         MVC   SW4CID,SPACES                                                    
         MVC   SW4CID(L'TGPID),TGDUB                                            
         MVI   SW4CIDH+5,6                                                      
         OI    SW4CIDH+6,X'80'                                                  
         L     R1,TGFULL                                                        
*                                                                               
DR65     BAS   RE,NEXTEL                                                        
         BNE   DR70                                                             
         B     DR60                                                             
         DROP  R4                                                               
*                                                                               
DR70     CH    R1,=H'1'            IF MORE THAN 1 CORP                          
         BNH   DR80                                                             
         MVI   SW4CIDN,C'*'        MARK IT                                      
         OI    SW4ISSNH+6,X'80'                                                 
*                                                                               
DR80     GOTO1 CHAROUT,DMCB,TANUELQ,SW4MEMH,TANUTMEM  ACTRA MEMBER NUM          
         GOTO1 CHAROUT,DMCB,TANUELQ,SW4GSTH,TANUTGST  GST TAX NUMBER            
         GOTO1 CHAROUT,DMCB,TACMELQ,SW4COMMH,TACMTYPG COMMENT                   
*                                                                               
         GOTO1 ACTVOUT,DMCB,SW4LCHGH                                            
*                                                                               
         L     R4,AIO              GET EMPLOYEE WITHHOLDING DETAILS             
         USING TAWHD,R4                                                         
         MVI   ELCODE,TAWHELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR140               MUST BE AT LEAST 2                           
*                                                                               
DR100    CLC   TAWHUNIT(2),=C'FD' FEDERAL TAX UNIT                              
         BNE   DR110                                                            
         LA    R2,SW4FEDH                                                       
         BRAS  RE,OUTWITH                                                       
         B     DR130                                                            
*                                                                               
DR110    CLI   TAWHUNIT+2,C' '     STATE TAX UNIT - 2 CHARS                     
         BH    DR120                                                            
         LA    R2,SW4SSTAH                                                      
         BRAS  RE,OUTWITH                                                       
         B     DR130                                                            
*                                                                               
DR120    LA    R2,SW4CITYH         MUST BE CITY                                 
         BRAS  RE,OUTWITH                                                       
*                                                                               
DR130    BAS   RE,NEXTEL                                                        
         BE    DR100                                                            
*                                                                               
DR140    DS    0H                                                               
         NI    SW4DOBH+1,X'F3'                                                  
         TM    TGCTSTST,TGCTSCLI   IF STAFF LEVEL C, D OR F                     
         BZ    *+8                                                              
         OI    SW4DOBH+1,X'0C'     MAKE DOB LOW INTENSITY                       
         L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DR142                                                            
         USING TAWXD,R4            R4=A(EXTRA DETAILS ELEMENT FOR W4)           
         GOTO1 DATCON,DMCB,(1,TAWXDOB),(8,SW4DOB)                               
         LA    R2,SW4PDEDH                                                      
         ICM   R3,15,TAWXPCT                                                    
         BRAS  RE,EDITFLAT         EDIT THE DEDUCT PERCENTAGE                   
         MVC   SW4TSSN,TAWXTSSN    TRUSTEE SSN                                  
         OC    TAWXTSSN,TAWXTSSN   IS THERE A TRUSTEE SSN?                      
         BZ    DR141                                                            
         GOTO1 SSNPACK,DMCB,TAWXTSSN,TGPID                                      
         MVC   SW4TSSN,SPACES                                                   
         MVC   SW4TSSN(L'TGPID),TGPID                                           
         MVI   SW4TSSNH+5,6                                                     
         OI    SW4TSSNH+6,X'80'                                                 
*                                                                               
DR141    EDIT  TAWXMERN,(11,SW4MERN),2                                          
*                                                                               
DR142    L     R4,AIO              GET EMPLOYEE OTHER WITHHOLDINGS              
         USING TAOWD,R4                                                         
         MVI   ELCODE,TAOWELQ                                                   
         MVI   WORK,TAOWTCHA       PERMANENT CHARITY                            
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   DR145                                                            
         L     R4,TGELEM                                                        
         LA    R2,SW4PCHRH                                                      
         ICM   R3,15,TAOWFLAT                                                   
         BRAS  RE,EDITFLAT         EDIT FLAT RATE                               
*                                                                               
DR145    DS    0H                                                               
         L     R4,AIO                                                           
         MVI   WORK,TAOWTMPF       MPR FUND                                     
         GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   DR147                                                            
         L     R4,TGELEM                                                        
         LA    R2,SW4MPRFH                                                      
         ICM   R3,15,TAOWFLAT                                                   
         BRAS  RE,EDITFLAT                                                      
*                                                                               
DR147    DS    0H                                                               
         CLI   SW4TYPE,TAW4TYCO    CORPORATION                                  
         BE    *+12                                                             
         CLI   SW4TYPE,TAW4TYTR    OR TRUSTEE                                   
         BNE   DRX                                                              
         MVC   SVKEY,KEY           FIND IND WHO IS ATTATCHED TO THIS            
         MVC   AIO,AIO2            CORP                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLW4PD,R4                                                        
         MVI   TLW4PCD,TLW4CCDQ    FIND FIRST CORP POINTER                      
         MVC   TLW4CCRP,TGSSN                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLW4CSSN-TLW4PD),KEYSAVE                                     
         BNE   DR150                                                            
         MVC   SW4ISS,TLW4CSSN                                                  
         GOTO1 SSNPACK,DMCB,TLW4CSSN,TGPID                                      
         MVC   SW4ISS,SPACES                                                    
         MVC   SW4ISS(L'TGPID),TGPID                                            
         MVI   SW4ISSH+5,6                                                      
DR149    OI    SW4ISSH+6,X'80'                                                  
         GOTO1 SEQ                 IF THERE IS MORE THAN 1 INDIVIDUAL           
         CLC   KEY(TLW4CSSN-TLW4PD),KEYSAVE                                     
         BNE   DR150               ATTATCHED TO THIS CORP                       
         MVI   SW4ISSN,C'*'        SET A STAR                                   
         OI    SW4ISSNH+6,X'80'                                                 
         DROP  R4                                                               
*                                                                               
DR150    MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
*                                                                               
DRX      MVC   SVEXMPTF,SW4FEXM    SAVE EXEMPTION FIELD VALUES                  
         MVC   SVEXMPTS,SW4SEXM                                                 
         MVC   SVEXMPTC,SW4CEXM                                                 
*                                                                               
         USING TAACD,R4                                                         
         L     R4,AIO              SAVE LAST CHANGED DATE/TIME                  
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   SVACCDTE,TAACCDTE                                                
         MVC   SVACCTIM,TAACCTIM                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         XC    TEMPTAWX,TEMPTAWX                                                
         MVI   SVW4TYPE,0                                                       
         MVI   SVW4STA2,0                                                       
         XC    SVW4ZIP,SVW4ZIP                                                  
         XC    ORIGCITY,ORIGCITY                                                
         MVI   ORIGMINR,C'N'                                                    
         NI    STATUS,X'FF'-STATNMCH                                            
*                                                                               
         XC    PTRBLK,PTRBLK       CLEAR POINTER BLOCK                          
         GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE POINTERS                        
*                                                                               
         L     R4,AIO                                                           
         CLI   ACTNUM,ACTADD       IF CHANGING,                                 
         BE    BLD10                                                            
                                                                                
         USING TAA2D,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL            SAVE ADDRESS ZIP CODE                        
         BNE   *+10                                                             
         MVC   SVW4ZIP,TAA2ZIP                                                  
                                                                                
         USING TAWHD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAWHELQ                                                   
         BAS   RE,GETEL            SAVE CITY                                    
         B     *+8                                                              
BLD02    BAS   RE,NEXTEL                                                        
         BNE   BLD03                                                            
         CLI   TAWHUNIT+2,C' '                                                  
         BE    BLD02                                                            
         MVC   ORIGCITY,TAWHUNIT                                                
                                                                                
BLD03    L     R4,AIO                                                           
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   BLD04                                                            
                                                                                
         USING TAACD,R4                                                         
         MVI   ELCODE,TAACELQ      IF RECORD HAS BEEN UPDATED FROM              
         BAS   RE,GETEL            WEB APPLICATION SINCE INITIAL                
         BE    *+6                 DISPLAY, PROMPT FOR REFRESH                  
         DC    H'00'                                                            
         CLC   TAACCDTE,SVACCDTE                                                
         BNE   ERRREF                                                           
         CLC   TAACCTIM,SVACCTIM                                                
         BNE   ERRREF                                                           
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO                                                           
BLD04    MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            SAVE STATUS BYTE                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVW4TYPE,TAW4TYPE                                                
         MVC   SVW4STAT,TAW4STAT   CLEAR ONLY CERTAIN STATUS BYTES              
         NI    SVW4STAT,X'FF'-TAW4STNY-TAW4SCKF                                 
         MVC   SVW4STA2,TAW4STA2   SAVE 2ND STATUS BYTE                         
         MVC   SVW4STA3,TAW4STA3   SAVE 3RD STATUS BYTE                         
         MVC   SVW4NHAD,TAW4NHAD                                                
         MVC   SVW4FREQ,TAW4FREQ                                                
         CLI   SVW4FREQ,C' '                                                    
         BNE   *+8                                                              
         MVI   SVW4FREQ,0                                                       
         DROP  R4                                                               
*                                                                               
         USING TAWXD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   BLD04Z                                                           
*                                                                               
         OC    TAWXDOB,TAWXDOB                                                  
         BZ    BLD04A                                                           
         GOTO1 DATCON,DMCB,(1,TAWXDOB),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+8,18                                 
         CLC   TGTODAY0,WORK+8                                                  
         BNL   BLD04Z                                                           
         MVI   ORIGMINR,C'Y'                                                    
         B     BLD04Z                                                           
                                                                                
BLD04A   OC    TAWXTSSN,TAWXTSSN                                                
         BZ    BLD04Z                                                           
         MVI   ORIGMINR,C'Y'                                                    
         DROP  R4                                                               
*                                                                               
BLD04Z   MVI   ELCODE,TAAKELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAA2ELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAOWELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAWHELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFLELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TATIELQ       DELETE ONLY WHEN TATICRPN = 1               
         GOTO1 DELL,DMCB,=C'C1'                                                 
*                                                                               
         USING TANUD,R4                                                         
         L     R4,AIO               DELETE PID NUMBER ELEMENT                   
         MVI   ELCODE,TANUELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BLD05    BAS   RE,NEXTEL                                                        
         BNE   BLD06                                                            
         CLI   TANUTYPE,TANUPIDN                                                
         BNE   BLD05                                                            
         MVI   TANUEL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R4                                                               
*                                                                               
BLD06    L     R4,AIO                                                           
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TAWXD,R4            IF TAWX ELEMENT EXISTS, SAVE AMT             
         MVC   TEMPTAWX,TAWXHTP    COLLECTED & W/HELD FOR ALL EMPLOYERS         
         GOTO1 REMELEM             THEN REMOVE ELEMENT                          
         DROP  R4                                                               
*                                                                               
         USING TAW4D,R3                                                         
BLD10    XC    BLOCK1,BLOCK1       BUILD ELEM IN BLOCK USING R3                 
         LA    R3,BLOCK1                 AND AFTERWARD MOVE TO ELEMENT          
         MVI   TAW4EL,TAW4ELQ      ELEMENT CODE                                 
         MVI   TAW4LEN,TAW4LN2Q    ELEMENT LENGTH                               
*                                                                               
         LA    R2,SW4TYPEH         TYPE                                         
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    BLD15                                                            
         CLI   ACTNUM,ACTADD       AND ACTION IS NOT ADD                        
         BE    BLD15A                                                           
*        TM    TGCTSTLV,B'11100010' IF NOT PROG, SYSMGR, ADMIN, ACCTING         
*        BNZ   BLD15                                                            
         CLI   TGCTSTTY,C'P'       ONLY PROGRAMMER (10/3/08)                    
         BE    BLD15                                                            
         CLI   TGCTSTTY,C'2'       SYSMGR                                       
         BE    BLD15                                                            
         CLI   TGCTSTTY,C'B'       ACCT MANAGER ARE ALLOWED TO CHANGE           
         BE    BLD15                                                            
*                                                                               
         MVC   8(1,R2),SVW4TYPE    IGNORE CHANGE                                
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         B     NOINPERR            TELL USER INPUT NOT ALLOWED                  
*                                                                               
BLD15    BRAS  RE,CHKAGNT                                                       
*                                                                               
BLD15A   CLI   5(R2),0             IF BLANK                                     
         BNE   BLD16                                                            
         MVI   5(R2),1             DEFAULT TO INDIVIDUAL                        
         OI    6(R2),X'80'                                                      
         MVI   8(R2),C'I'                                                       
*                                                                               
BLD16    MVI   TAW4TYPE,TAW4TYIN   DEFAULT TO INDIVIDUAL                        
         CLI   8(R2),C'I'          I = INDIVIDUAL                               
         BNE   BLD18                                                            
         LA    R2,SW4SSNH          SSN                                          
         CLI   ACTNUM,ACTADD       IF ADDING AN INDIVIDUAL                      
         BNE   BLD20                                                            
         CLI   8(R2),C'9'          CANNOT START SSN WITH 9                      
         BE    INVSSN                                                           
         B     BLD20                                                            
*                                                                               
BLD18    MVI   TAW4TYPE,TAW4TYCA   A = CANADIAN                                 
         CLI   8(R2),C'A'                                                       
         BE    BLD20                                                            
         MVI   TAW4TYPE,TAW4TYCO   C = CORPORATION                              
         CLI   8(R2),C'C'                                                       
         BE    BLD30                                                            
         MVI   TAW4TYPE,TAW4TYTR   T = TRUSTEE                                  
         CLI   8(R2),C'T'                                                       
         BE    BLD30                                                            
         MVI   TAW4TYPE,TAW4TYES   E = ESTATE                                   
         CLI   8(R2),C'E'                                                       
         BE    BLD30                                                            
         MVI   TAW4TYPE,TAW4TYFO   F = FOREIGNER                                
         CLI   8(R2),C'F'                                                       
         BNE   INVERR                                                           
*                                                                               
BLD20    LA    R2,SW4NAM1H                                                      
         CLI   5(R2),0             FIRST NAME                                   
         BE    MISSERR                                                          
         OC    SW4NAM1,SPACES                                                   
         BRAS  RE,VALFSTN                                                       
         MVC   TAW4NAM1,SW4NAM1                                                 
*                                                                               
         LA    R2,SW4NAM2H         FOR ERROR IF THERE IS ONE                    
         CLI   5(R2),0             LAST NAME                                    
         BE    MISSERR                                                          
         OC    SW4NAM2,SPACES                                                   
         BRAS  RE,VALLSTN                                                       
         MVC   TAW4NAM2,SW4NAM2                                                 
*                                                                               
         LA    R2,SW4NAM3H                                                      
         OC    SW4NAM3,SPACES      MIDDLE NAME                                  
         CLC   SW4NAM3,SPACES                                                   
         BE    *+8                                                              
         BRAS  RE,VALMIDN                                                       
         MVC   TAW4MIDN,SW4NAM3                                                 
*                                                                               
         LA    R2,SW4NAM4H                                                      
         OC    SW4NAM4,SPACES      SUFFIX                                       
         CLC   SW4NAM4,SPACES                                                   
         BE    *+8                                                              
         BRAS  RE,VALSUFF                                                       
         MVC   TAW4SUFF,SW4NAM4                                                 
*                                                                               
         BRAS  RE,VALPHONE                                                      
         GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SW4PHONH),TANUTPHN                     
         B     BLD40                                                            
*                                                                               
BLD30    BRAS  RE,VALPHONE                                                      
         GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SW4PHONH),TANUTPHN                     
         LA    R2,SW4CRPNH         CORPORATION NAME                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         OC    SW4CRPN,SPACES                                                   
         MVC   TAW4CRPN,SW4CRPN                                                 
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   IF CORP OR TRUSTEE SKIP                      
         BE    BLD70                                                            
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    BLD70                  BUT AN ESTATE - CONTINUE                  
*                                                                               
BLD40    LA    R2,SW4AKA1H                                                      
         CLI   5(R2),0             AKA NAME                                     
         BE    BLD70                                                            
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAAKD,R4                                                         
         MVI   TAAKEL,TAAKELQ                                                   
         MVI   TAAKLEN,TAAKLNQ                                                  
*                                                                               
         OC    TAAKNAM2(L'TAAKNAM2+L'TAAKNAM1),SPACES                           
         MVC   TAAKNAM1,SW4AKA1                                                 
         LA    R2,SW4AKA2H                                                      
         CLI   5(R2),0             AKA LAST NAME                                
         BE    BLD60                                                            
         MVC   TAAKNAM2,SW4AKA2                                                 
*                                                                               
BLD60    OC    TAAKNAM2(L'TAAKNAM2+L'TAAKNAM1),SPACES                           
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
BLD70    BAS   RE,VALSTAT          VALIDATE STATUS                              
*                                  IF TYPE OR LOCK CHANGING,                    
         BRAS  RE,CHKPEND          CHECK FOR PENDING PAYMENTS                   
         MVC   SVW4STA2,TAW4STA2   SAVE NEW STATUS BYTES TO CHECK IRS           
                                                                                
         BRAS  RE,CHKTCAST         IF TYPE CHANGED TO T CANT HAVE CAST          
*                                                                               
         BRAS  RE,VALSLET          VALIDATE SPECIAL LETTER                      
         BRAS  RE,VALADDR          ADDRESS - REQUIRED                           
         BRAS  RE,SVPID            SAVE PID                                     
*                                                                               
BLD80    CLI   SW4TYPE,TAW4TYTR    IF W4 TYPE IS TRUSTEE                        
         BE    BLD130              IGNORE ALL OTHER INFO UP TO FILTERS          
*                                                                               
         LA    R2,SW4SEXH          GET SEX                                      
         CLI   5(R2),0                                                          
         BNE   BLD85                                                            
         CLI   SW4TYPE,TAW4TYCO    OPTIONAL FOR CORPORATIONS                    
         BE    BLD100                                                           
         B     MISSERR                                                          
BLD85    CLI   SW4SEX,C'F'                                                      
         BE    BLD90                                                            
         CLI   SW4SEX,C'M'                                                      
         BNE   INVERR                                                           
BLD90    MVC   TAW4SEX,SW4SEX                                                   
*                                                                               
BLD100   LA    R2,SW4ETHH          RACE                                         
         CLI   5(R2),0                                                          
         BE    BLD130                                                           
         OC    SW4ETH,SPACES                                                    
         LA    RF,VETHCTY                                                       
BLD110   CLC   0(2,RF),SW4ETH      ETHNICITY                                    
         BE    BLD120                                                           
         CLI   L'VETHCTY(RF),X'FF'                                              
         BE    INVERR                                                           
         LA    RF,L'VETHCTY(RF)                                                 
         B     BLD110                                                           
BLD120   MVC   TAW4RACE,SW4ETH                                                  
*                                                                               
BLD130   BRAS  RE,VALFILT          VALIDATE FILTERS                             
         BRAS  RE,VALFREQ          VALIDATE FREQUENCY                           
         BRAS  RE,VALCID           CORP ID VALIDATION                           
*                                                                               
         LA    R2,SW4INDH          POINT TO INDEMNIFICATION DATE                
         CLI   5(R2),0                   ONLY ALLOWED FOR CORPS                 
         BE    BLD150                    AND TRUSTEES                           
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    *+12                                                             
         CLI   TAW4TYPE,TAW4TYTR                                                
         BNE   NOINPERR                                                         
         GOTO1 DTVAL,DMCB,TAW4INDT                                              
*                                                                               
BLD150   LA    R2,SW4LOCLH         AFM LOCAL                                    
         CLI   5(R2),0                                                          
         BE    BLD160                                                           
         MVC   TGUNI,=C'AFM'       SET GLOBAL UNION                             
         GOTO1 RECVAL,DMCB,TLLOCDQ,SW4LOCLH                                     
         MVC   TAW4LOCL,TGLCL      SET LOCAL IN ELEMENT                         
*                                                                               
BLD160   LA    R2,SW4MEMH          ACTRA MEMBERSHIP NUM                         
         CLI   5(R2),0             IF THERE'S INPUT                             
         BE    BLD165                                                           
         CLI   8(R2),C'A'          1ST POSITION MUST BE ALPHA                   
         BL    INVERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    INVERR                                                           
BLD165   GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SW4MEMH),TANUTMEM NUM OPTIONAL         
*                                                                               
         GOTO1 ACTVIN,DMCB,SW4LCHGH                LAST CHANGED                 
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    BLD190              NO TAX DETAILS FOR CORPS OR TRUSTEES         
         CLI   TAW4TYPE,TAW4TYTR   XCEPT RECIP                                  
         BE    BLD190                                                           
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIANS                                 
         BE    BLD210                                                           
         CLI   TAW4TYPE,TAW4TYFO   OR FOREIGNERS                                
         BE    BLD210                                                           
         CLI   TAW4TYPE,TAW4TYIN                                                
         BNE   BLD170              OR INDIVIDUALS WHO DIDN'T HAVE               
         TM    TAW4STAT,TAW4STNW      TAX INFO AT CONVERSION TIME               
         BNO   BLD170                 AND DO NOT HAVE IT NOW                    
         CLI   SW4FMSH+5,0                                                      
         BE    BLD180                                                           
*                                                                               
BLD170   LA    R2,SW4FEDH                                                       
         MVC   SW4FED,=C'FD '      FED TAX                                      
         MVI   SW4FEDH+5,2                                                      
         OI    4(R2),X'20'         VALID                                        
         OI    6(R2),X'80'                                                      
         BAS   RE,GETTAX                                                        
*                                                                               
BLD180   LA    R2,SW4SSTAH         STATE TAX WITHHOLDINGS                       
         CLI   5(R2),2             DO NOT ALLOW CITIES OR                       
         BNE   INVERR                                                           
         CLC   =C'FD',8(R2)        DON'T ALLOW 'FD' INPUT FOR STATE             
         BE    INVERR                                                           
         CLI   TAW4TYPE,TAW4TYIN   IF INDIVIDUAL                                
         BNE   *+14                                                             
         CLC   =C'OT',8(R2)        DONT ALLOW 'OT' AS INPUT FOR STATE           
         BE    INVERR                                                           
         BAS   RE,GETTAX                                                        
*                                                                               
BLD190   LA    R2,SW4RECPH         RECIPROCAL STATE                             
         CLI   5(R2),0                                                          
         BE    BLD200                                                           
         CLC   =C'FD',8(R2)        DON'T ALLOW 'FD' INPUT FOR STATE             
         BE    INVERR                                                           
         MVC   WORK(2),8(R2)                                                    
         MVI   WORK+2,X'40'                                                     
         GOTO1 TAXVAL,DMCB,(3,WORK)                                             
         BNE   INVERR                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    INVERR              FORWARD                                      
         CLC   TGTACODE(2),SW4SSTA ENSURE NOT SAME AS SOR                       
         BE    INVERR                                                           
         MVC   TAW4RECP,TGTACODE   SAVE IT                                      
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    BLD210              SKIP IF CORP OR TRUSTEE                      
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    BLD210                                                           
*                                                                               
BLD200   LA    R2,SW4CITYH         CITY TAX WITHHOLDINGS                        
         CLI   5(R2),0                                                          
         BE    BLD210                                                           
         CLI   5(R2),3                                                          
         BNE   INVERR                                                           
         BAS   RE,GETTAX                                                        
         CLC   TGTASTCY(2),SW4SSTA IF CITY NO IN STATE                          
         BNE   INVERR              THEN INVALID                                 
*                                                                               
BLD210   CLI   ACTNUM,ACTCHA       IF ACTION CHANGE                             
         BNE   BLD210A                                                          
         CLI   SW4SLET,C'Y'        AND SPECIAL LETTER ON FILE                   
         BNE   BLD210A                                                          
         CLI   TGCTSTTY,TASTTYPP   ONLY PROGRAMMERS                             
         BE    BLD210A                                                          
         CLI   TGCTSTTY,TASTTYP2   PROGRAMMING MANAGERS                         
         BE    BLD210A                                                          
         CLI   TGCTSTTY,TASTTYPA   ACCOUNTING STAFF                             
         BE    BLD210A                                                          
         CLI   TGCTSTTY,TASTTYPB   OR ACCOUNTING MANAGERS                       
         BE    BLD210A                                                          
         OC    SVEXMPTN,SPACES     CANNOT CHANGE EXEMPTION FIELDS               
         LA    R2,SW4FEXMH                                                      
         OI    8(R2),C' '                                                       
         CLC   SVEXMPTF,8(R2)                                                   
         BNE   INVERR                                                           
         LA    R2,SW4SEXMH                                                      
         OI    8(R2),C' '                                                       
         CLC   SVEXMPTS,8(R2)                                                   
         BNE   INVERR                                                           
         LA    R2,SW4CEXMH                                                      
         OI    8(R2),C' '                                                       
         CLC   SVEXMPTC,8(R2)                                                   
         BNE   INVERR                                                           
*                                                                               
BLD210A  BRAS  RE,VALFTX                  VALIDATE TAX FOREIGN TAX              
         BRAS  RE,VALNHA                  VALIDATE NEW HIRE ACT                 
                                                                                
         MVC   NEW4TYPE,TAW4TYPE          SAVE NEW W4 TYPE                      
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(TAW4LN2Q),BLOCK1   MOVE INTO ELEM TO ADD                 
         GOTO1 ADDELEM                                                          
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    BLD210A3                                                         
         CLC   SVW4CRPN,TAW4CRPN   HAS NAME CHANGED?                            
         BE    BLD210A3                                                         
         OI    STATUS,STATNMCH     YES, PROCESS THOSE LATER                     
         MVC   NEW4CRPN,TAW4CRPN   SAVE CHANGED NAME                            
         DROP  R3                                                               
*                                                                               
BLD210A3 XC    ELEMENT,ELEMENT     PRE-CLEAR W4 EXTRA DETAILS ELEMENT           
         LA    R4,ELEMENT                                                       
         USING TAWXD,R4                                                         
         LA    R2,SW4DOBH          R2=A(DATE OF BIRTH)                          
**       CLI   SW4TYPE,C'I'                                                     
**       BE    BLD210B                                                          
**       CLI   SW4TYPE,C'C'        ALLOW CORPS BECAUSE OF CANADA                
**       BE    BLD210B                                                          
**       CLI   5(R2),0                                                          
**       BNE   INVERR                                                           
BLD210B  GOTO1 DTVAL,DMCB,(X'80',TAWXDOB)                                       
*                                                                               
         OC    TAWXDOB,TAWXDOB                                                  
         BZ    BLD210C                                                          
         CLI   ORIGMINR,C'Y'                                                    
         BE    BLD210C                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    BLD210C                                                          
         GOTO1 DATCON,DMCB,(1,TAWXDOB),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+8,18                                 
         CLC   TGTODAY0,WORK+8                                                  
         BNL   BLD210C                                                          
         BRAS  RE,CHKPPHST                                                      
*                                                                               
BLD210C  LA    R2,SW4PDEDH         R2=A(DEDUCT PERCENTAGE)                      
         CLI   5(R2),0                                                          
         BE    BLD211                                                           
**       CLI   SW4TYPE,C'I'                                                     
**       BE    *+8                                                              
**       CLI   SW4TYPE,C'C'        ALLOW CORPS BECAUSE OF CANADA                
**       BNE   INVERR                                                           
         LA    R3,TAWXPCT                                                       
         BRAS  RE,VALFLAT                                                       
*                                                                               
BLD211   LA    R2,SW4MERNH         MINOR'S LIFETIME EARNINGS                    
         CLI   5(R2),0                                                          
         BE    BLD213                                                           
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE EARNINGS                        
         MVC   TAWXMERN,DMCB+4                                                  
*                                                                               
BLD213   LA    R2,SW4TSSNH         R2=A(TRUSTEE S/S NUMBER)                     
         CLI   5(R2),0                                                          
         BE    BLDR214                                                          
**       CLI   SW4TYPE,C'I'                                                     
**       BE    *+8                                                              
**       CLI   SW4TYPE,C'C'        ALLOW CORPS BECAUSE OF CANADA                
**       BNE   INVERR                                                           
*                                                                               
         OC    TAWXDOB,TAWXDOB                                                  
         BNZ   BLD213AA                                                         
         CLI   ORIGMINR,C'Y'                                                    
         BE    BLD213AA                                                         
         CLI   ACTNUM,ACTADD                                                    
         BE    BLD213AA                                                         
         BRAS  RE,CHKPPHST                                                      
*                                                                               
BLD213AA MVC   AIO,AIO2            CHANGE IO AREAS                              
         MVC   SVDSKADD,DMDSKADD   SAVE DISK ADDRESS TO RE-GET REC              
         CLI   SW4TSSNH+5,6        IF NOT HIGHER THAN 6 THAN MUST BE 6          
         BH    BLD213A                                                          
         BNE   INVERR                                                           
         MVC   TGPID,SW4TSSN                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLD213A                                                          
         MVC   SW4TSSN,TGSSN                                                    
         MVI   SW4TSSNH+5,9                                                     
BLD213A  GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',(R2))                                 
         BNE   INVERR                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            IF TRUSTEE W4 DOES NOT HAVE                  
         BNE   INVERR              EMPLOYEE DETAILS ELEMENT - ERROR             
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYTR   MUST BE TYPE TRUSTEE                         
         BNE   INVERR                                                           
         DROP  R4                                                               
         MVC   AIO,AIO1            RESTORE IO AREAS                             
         LA    R4,ELEMENT          AND ELEMENT IN PROGRESS                      
         USING TAWXD,R4                                                         
         MVC   TAWXTSSN,TGSSN                                                   
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING A RECORD - DO NOT                  
         BE    BLDR214                TRY TO RE-GET RECORD                      
         L     R4,AIO              ELSE                                         
         XC    KEY,KEY                RE-GET REC - PREVENT PUTREC PROB          
         MVC   KEY(L'TLDRREC),0(R4)         RESTORE KEY                         
         MVC   KEY+L'TLDRREC-4(4),SVDSKADD  INCLUDING DISK ADDRESS              
         MVC   AIO,AIO2                     DO NOT OVERWRITE CHANGES            
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE AIO1                                 
*                                                                               
         USING TLW4D,R4                                                         
BLDR214  L     R4,AIO                                                           
         MVC   TGSSN,TLW4SSN                                                    
         DROP  R4                                                               
*                                                                               
         USING TAWXD,R4                                                         
         LA    R4,ELEMENT                                                       
         MVC   TAWXHTP(L'TEMPTAWX),TEMPTAWX RESTORE AMT COLLECTED &             
*                                                            W/HELD             
         OC    ELEMENT,ELEMENT     IF ANY FIELDS INPUT                          
         BZ    BLD215                                                           
         MVI   TAWXEL,TAWXELQ                                                   
         MVI   TAWXLEN,TAWXLNQ                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
BLD215   LA    R2,SW4MPRFH         MPR FUND                                     
         CLI   5(R2),0                                                          
         BE    BLD220                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAOWD,R4                                                         
         MVI   TAOWEL,TAOWELQ                                                   
         MVI   TAOWLEN,TAOWLNQ                                                  
         MVI   TAOWTYPE,TAOWTMPF                                                
         LA    R3,TAOWFLAT                                                      
         BRAS  RE,VALFLAT                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
BLD220   BAS   RE,VALCHAR          VALIDATE PERMANENT CHARITY                   
*                                                                               
**NO-OP**LA    R2,SW4GSTH          GST TAX NUMBER - TAKE ANY INPUT              
*        CLI   5(R2),0                              AS OF 3/6                   
*        BE    *+8                                                              
**NO-OP**BAL   RE,VALGST           VALIDATE IT                                  
*                                                                               
         LA    R2,SW4GSTH                                                       
         CLI   5(R2),0                                                          
         BE    BLD250                                                           
         CLI   SW4TYPE,C'A'        HAS TO BE CANADIAN                           
         BE    BLD250                                                           
         CLI   SW4TYPE,C'C'        OR CORP TO HAVE GST #                        
         BNE   INVERR                                                           
BLD250   GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SW4GSTH),TANUTGST SAVE IN TANU         
*                                                                               
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SW4COMMH),TACMTYPG  COMMENT            
*                                                                               
         CLI   PFAID,23            HIT PF23 TO CHANGE FED TAX LINE              
         BE    BLD380                                                           
         CLI   ACTNUM,ACTCHA       ONLY REQUIRE PFKEY FOR CHANGE                
         BNE   BLD380                                                           
         TM    SVW4STA2,TAW4SIRS   IS IRS RESTRICTION ON                        
         BNO   BLDX                                                             
         GOTO1 FLDVAL,DMCB,(X'40',SW4FMSH),SW4FFIXH                             
         BE    BLDX                                                             
         OI    STATUS,STATPFOK                                                  
         L     R2,8(R1)            SET R2 TO FIELD THAT CHANGED                 
         B     IRSERR                  AND SET PFKEY MSG                        
*                                                                               
BLD380   OI    SW4FMSH+4,X'20'                                                  
         OI    SW4FEXMH+4,X'20'                                                 
         OI    SW4FFIXH+4,X'20'                                                 
*                                                                               
         USING TLW4D,R4                                                         
BLDX     L     R4,AIO                  RESTORE KEY                              
         MVC   KEY,0(R4)                                                        
*                                                                               
         CLI   W4ZIPFLG,C'N'       DID ZIP CODE CHANGE?                         
         BE    BLDX10              NO - EXIT                                    
         CLI   PFAID,24            YES - IF PF24 HIT, ALLOW CHANGE              
         BNE   ZIPWARN             NO - GIVE WARNING                            
*                                                                               
BLDX10   CLC   SVW4TYPE,NEW4TYPE       IF W4TYPE CHANGED                        
         BE    XIT                                                              
         MVC   TLW4STA2,NEW4TYPE       SAVE IT IN RECORD                        
**NO-OP**CLI   ACTNUM,ACTADD                                                    
*        BE    XIT                     DONE IF ADDING                           
*        MVI   RDUPDATE,C'Y'                                                    
*        GOTO1 HIGH                    READ ACTV PTR FOR UPDATE                 
*        CLC   KEY(L'TLW4KEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   KEY+TLDRSTAT+1-TLDRD(1),NEW4TYPE  SET NEW W4 TYPE IN PTR         
**NO-OP**GOTO1 WRITE                             WRITE IT BACK                  
         B     XIT                                                              
                                                                                
       ++INCLUDE TAETHCTY          VALID ETHNICITIES                            
         EJECT                                                                  
*        VALIDATE STATUS FIELD                                                  
*                                                                               
         USING TAW4D,R3                                                         
VALSTAT  NTR1                                                                   
         XC    TAW4CP,TAW4CP                                                    
*                                                                               
         LA    R2,SW4STATH         STATUS                                       
         CLI   5(R2),0                                                          
         BE    VSTAT60                                                          
         LA    R2,SW4STATH                                                      
         XC    HALF,HALF           CLEAR FIELD DISP. COUNTER                    
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         XR    R1,R1               R1=DISPLACEMENT INTO FIELD                   
         LA    R5,BLOCK            R5=A(SCAN BLOCK)                             
         USING SCAND,R5                                                         
         LTR   R0,R0                                                            
         BZ    INVSTAT                                                          
*                                                                               
VSTAT10  CLI   SCLEN2,0            INSURE NO INPUT ON RHS                       
         BNE   VSTAT43                                                          
         L     RF,=A(STATTAB)      LOOP THROUGH 1ST STATUS TABLE                
         A     RF,RELO                                                          
*                                                                               
VSTAT20  CLI   0(RF),X'FF'                                                      
         BE    VSTAT30             CHECK NEXT TABLE                             
         CLC   SCDATA1,1(RF)       IF EXACT MATCH TO TABLE VALUE                
         BE    *+12                                                             
         LA    RF,L'STATTAB(RF)                                                 
         B     VSTAT20                                                          
         OC    TAW4STAT,0(RF)      THEN TURN ON BIT IN ELEMENT                  
         B     VSTAT50             AND CHECK NEXT STATUS INPUT                  
*                                                                               
VSTAT30  L     RF,=A(STATTAB2)     LOOP THROUGH 2ND STATUS TABLE                
         A     RF,RELO                                                          
*                                                                               
VSTAT40  CLI   0(RF),X'FF'                                                      
         BE    VSTAT41                                                          
         CLC   SCDATA1,1(RF)       IF EXACT MATCH TO TABLE VALUE                
         BE    *+12                                                             
         LA    RF,L'STATTAB2(RF)                                                
         B     VSTAT40                                                          
         TM    0(RF),TAW4SDD+TAW4SWIR                                           
         BZ    *+12                                                             
         TM    TAW4STA2,TAW4SDD+TAW4SWIR                                        
         BNZ   COMPERR                                                          
         OC    TAW4STA2,0(RF)      THEN TURN ON BIT IN ELEMENT                  
         B     VSTAT50                                                          
*                                                                               
VSTAT41  L     RF,=A(STATTAB3)     LOOP THROUGH 3RD STATUS TABLE                
         A     RF,RELO                                                          
*                                                                               
VSTAT42  CLI   0(RF),X'FF'                                                      
         BE    INVSTAT                                                          
         CLC   SCDATA1,1(RF)       IF EXACT MATCH TO TABLE VALUE                
         BE    *+12                                                             
         LA    RF,L'STATTAB3(RF)                                                
         B     VSTAT42                                                          
         OC    TAW4STA3,0(RF)      THEN TURN ON BIT IN ELEMENT                  
         B     VSTAT50                                                          
*                                                                               
VSTAT43  CLC   =C'CP',SCDATA1                                                   
         BNE   INVSTAT                                                          
         CLI   SCLEN1,2                                                         
         BNE   VSTAT44                                                          
         CLI   SCLEN2,2                                                         
         BNE   VSTAT44                                                          
         CLI   TAW4TYPE,TAW4TYCA   CANADIAN                                     
         BNE   VSTAT44                                                          
         CLC   SW4CTRY,=C'CA'      NOT LIVING IN CANADA                         
         BE    VSTAT44                                                          
*                                                                               
         ST    R1,FULL                                                          
         MVC   HALF,SW4CTRY        SAVE COUNTRY                                 
         MVC   TGCTRY,=C'CA'       MUST BE VALID PROVINCE                       
         MVC   WORK(2),SCDATA2                                                  
         OC    WORK(2),SPACES                                                   
         MVI   WORK+2,C' '                                                      
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         BE    VSTAT45                                                          
         MVC   TGCTRY,HALF         RESET COUNTRY                                
VSTAT44  L     R1,FULL                                                          
         B     INVSTAT                                                          
*                                                                               
VSTAT45  MVC   TGCTRY,HALF         RESET COUNTRY                                
         MVC   TAW4CP,SCDATA2      SET CANADIAN PROVINCE                        
*                                                                               
VSTAT50  ZIC   RF,SCLEN1                                                        
         LA    R1,1(R1,RF)                                                      
         LA    R5,SCANNEXT         BUMP TO IT                                   
         BCT   R0,VSTAT10          AND CONTINUE                                 
         DROP  R5                                                               
*                                                                               
VSTAT60  OC    TAW4STAT,SVW4STAT   TURN ON OLD BITS IN ELEMENT                  
         TM    SVW4STA3,TAW4SREG                                                
         BZ    XIT                                                              
         OI    TAW4STA3,TAW4SREG                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE CORPORATION ID                                                
*                                                                               
*                                                                               
*        VALIDATE PERMANENT CHARITY                                             
*                                                                               
VALCHAR  NTR1                                                                   
         LA    R2,SW4PCHRH         PERMANENT CHARITY                            
         CLI   5(R2),0                                                          
         BE    VCHARX                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAOWD,R4                                                         
         MVI   TAOWEL,TAOWELQ                                                   
         MVI   TAOWLEN,TAOWLNQ                                                  
         MVI   TAOWTYPE,TAOWTCHA                                                
         LA    R3,TAOWFLAT                                                      
         BRAS  RE,VALFLAT                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
VCHARX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE FIRST                                                         
*              R2 - A(FIELD HEADER)                                             
*                                                                               
VALFSTN  NTR1                                                                   
         LA    R1,16               LENGTH OF THE NAME FIELD                     
         LA    RF,8(R2)            A(NAME FIELD)                                
         BRAS  RE,CHKCHR           CHECK FOR INVALID CHAR                       
         BNE   INVERR                                                           
         BRAS  RE,CHKBLK                                                        
         BNE   INVERR                                                           
         B     XIT                                                              
*                                                                               
*        VALIDATE MIDDLE NAME                                                   
*              R2 - A(FIELD HEADER)                                             
*                                                                               
VALMIDN  NTR1                                                                   
         LA    R1,16               LENGTH OF THE NAME FIELD                     
         LA    RF,8(R2)            A(NAME FIELD)                                
         BRAS  RE,CHKCHR           CHECK FOR INVALID CHAR                       
         BNE   INVERR                                                           
         B     XIT                                                              
*                                                                               
*        VALIDATE LAST NAME                                                     
*              R2 - A(FIELD HEADER)                                             
*                                                                               
VALLSTN  NTR1                                                                   
         LA    R1,16               LENGTH OF THE NAME FIELD                     
         LA    RF,8(R2)            A(NAME FIELD)                                
         BRAS  RE,CHKCHR           CHECK FOR INVALID CHAR                       
         BNE   INVERR                                                           
         BRAS  RE,CHKBLK                                                        
         BE    XIT                 ONLY ONE WORD, NO PREFIX, EXIT               
*                                                                               
         LA    RF,VPREFIX                                                       
VLN10    CLI   0(RF),X'FF'                                                      
         BE    INVERR              END OF TABLE, INVALID PREFIX                 
         ZIC   R1,0(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RF),8(R2)                                                    
         BE    VLN20                                                            
         AHI   RF,L'VPREFIX                                                     
         B     VLN10                                                            
*                                                                               
VLN20    LA    RF,9(R1,R2)         POINT TO CHAR AFTER PREFIX+" "               
         LA    RE,15                                                            
         SR    RE,R1               15 - LEN(PREFIX-1)                           
         LR    R1,RE               LEN OF REST OF LAST NAME                     
         BRAS  RE,CHKBLK                                                        
         BE    XIT                 ONLY ONE WORD AFTER PREFIX, EXIT             
         B     INVERR                                                           
*                                                                               
       ++INCLUDE TAPREFIX          VALID LAST NAME PREFIXES                     
         EJECT                                                                  
*                                                                               
*        CHECK BLANKS B/W WORDS - CC=EQ WHEN THERE IS ONLY 1 WORD               
*                               - CC=NE WHEN THERE ARE MORE THAN 1 WORD         
*              R2 - A(FIELD HEADER)                                             
*              R1 - LENGTH OF STRING                                            
*              RF - A(STRING)                                                   
*                                                                               
CHKBLK   NTR1                                                                   
         CLI   0(RF),C' '                                                       
         BE    NO                  1ST CHAR MUST NOT BE BLANK                   
*                                                                               
CB10     CLI   0(RF),C' '                                                       
         BE    CB20                FOUND 1ST BLANK                              
         AHI   RF,1                                                             
         BCT   R1,CB10                                                          
         B     YES                 NO BLANK, OKAY, EXIT                         
*                                                                               
CB20     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BNE   NO                  THERE ARE MORE THAN 1 WORD                   
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        CHECK INVALID CHARS - CC=EQ WHEN ALL CHARS ARE VALID                   
*                            - CC=NE WHEN 1 OR MORE CHARS ARE INVALID           
*              R2 - A(FIELD HEADER)                                             
*              R1 - LENGTH OF STRING                                            
*              RF - A(STRING)                                                   
*                                                                               
CHKCHR   NTR1                                                                   
*                                                                               
CC10     LA    RE,VNAMCHR                                                       
CC20     CLI   0(RE),X'FF'                                                      
         BE    NO                  THIS IS AN INVALID CHAR                      
         CLC   0(1,RF),0(RE)                                                    
         BE    CC30                                                             
         AHI   RE,1                                                             
         B     CC20                                                             
*                                                                               
CC30     AHI   RF,1                                                             
         BCT   R1,CC10                                                          
         B     YES                 ALL CHARS ARE VALID                          
*                                                                               
       ++INCLUDE TANAMCHR          VALID FIRST/LAST NAME CHARACTERS             
         EJECT                                                                  
*                                                                               
*        VALIDATE SUFFIX                                                        
*              R2 - A(FIELD HEADER)                                             
*                                                                               
VALSUFF  NTR1                                                                   
*                                                                               
         LA    R1,VSUFFIX                                                       
VSUFF10  CLI   0(R1),X'FF'                                                      
         BE    INVERR              END OF TABLE, INVALID SUFFIX                 
         CLC   0(L'VSUFFIX,R1),8(R2)                                            
         BE    XIT                 VALID SUFFIX, EXIT                           
         AHI   R1,L'VSUFFIX                                                     
         B     VSUFF10                                                          
*                                                                               
       ++INCLUDE TASUFFIX          VALID SUFFIXES                               
         EJECT                                                                  
*                                                                               
*        ROUTINE GETS W2 KEY & TURNS ON REPRINT BIT                             
*                                                                               
W2PRINT  NTR1                                                                   
         BAS   RE,SECURITY         CHECK SECURITY ACCESS OF USER                
         BNE   SECERR                                                           
         MVC   SVKEY,KEY           SAVE W4 KEY                                  
         BAS   RE,BLDEMP           BLD TABLE OF EMPLOYERS                       
         XC    KEY,KEY                                                          
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,F'-1'                            
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   TGYEAR,WORK+6       SET CCYY OF PREVIOUS YEAR                    
*                                                                               
         MVI   TGTYCUR,C'U'        SET US CURRENCY                              
*                                                                               
W230     LA    R3,EMPTAB                                                        
*                                                                               
W240     OC    0(3,R3),0(R3)       ANY EMPLOYERS LEFT                           
         BZ    W250                                                             
         MVC   TGEMP,0(R3)         SET EMPLOYER                                 
         BAS   RE,UPDW2            UPDATE W2 RECORD                             
         LA    R3,3(R3)                                                         
         B     W240                GET NEXT EMPLOYER                            
*                                                                               
W250     CLI   TGTYCUR,C'C'        IF ALREADY PROCESSED CANADIAN $              
         BE    W260                EXIT                                         
         MVI   TGTYCUR,C'C'        ELSE SET CANADIAN CURRENCY                   
         B     W230                AND DO ALL EMPLOYERS OVER                    
*                                                                               
W260     BAS   RE,SETTAL           SET TALENT FILE                              
         XC    KEY,KEY             REGET RECORD INTO AIO2 TO PREVENT            
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                GETREC/PUTREC SYNDROME                       
         GOTO1 GETREC                                                           
*                                                                               
W2X      MVC   AIO,AIO1            RE-SET I/O AREA                              
         BAS   RE,SETTAL           SET TALENT FILE                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CHECK IF USER ALLOWED TO REQUEST W2 REPRINT                 
*                                                                               
SECURITY NTR1                                                                   
         MVI   BYTE,0                                                           
         CLI   TGSTDSP,0          1ST SECURITY BYTE                             
         BNE   SEC10                                                            
         MVI   BYTE,BP234+BM      SET VALID USERS - PROGRAMMER                  
         B     SEC40              SYSTEMS MANAGER/EXECUTIVE/ADMIN               
*                                                                               
SEC10    CLI   TGSTDSP,1          2ND SECURITY BYTE                             
         BNE   SEC20                                                            
         MVI   BYTE,BAB           SET VALID USERS - ACCOUNTING/                 
         B     SEC40              ACCOUNTING MANAGER                            
*                                                                               
SEC20    CLI   TGSTDSP,2          3RD SECURITY BYTE                             
         BNE   SEC30                                                            
         MVI   BYTE,BO            SET VALID USERS - OPERATOR                    
         B     SEC40                                                            
*                                                                               
SEC30    CLI   TGSTDSP,3          4TH SECURITY BYTE                             
         BNE   NO                                                               
*                                                                               
SEC40    MVC   MYBYTE,BYTE                                                      
         OC    BYTE,TGSTBIT       IS THIS A VALID USER                          
         CLC   BYTE,MYBYTE                                                      
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO BUILD AN EMPLOYER TABLE                                     
*                                                                               
BLDEMP   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLEMD,R1                                                         
         XC    EMPTAB(30),EMPTAB                                                
         LA    R2,EMPTAB                                                        
         MVI   KEY,TLEMCDQ         SET EMPLOYER CODE                            
         GOTO1 HIGH                                                             
         B     BE20                                                             
*                                                                               
BE10     GOTO1 SEQ                                                              
*                                                                               
BE20     CLC   KEY(1),KEYSAVE      STILL READING EMPLOYER RECORDS               
         BNE   BEX                                                              
         MVC   0(3,R2),TLEMEMP                                                  
         LA    R2,3(R2)            BUMP TO NEXT PLACE IN TABLE                  
         B     BE10                                                             
*                                                                               
BEX      B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO GET & UPDATE A W2 RECORD                                    
*                                                                               
UPDW2    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW2CDQ,(X'B4',0)                                    
         BNE   UW2X                                                             
         MVI   W2FLAG,C'Y'                                                      
         L     R1,AIO                                                           
         USING TLW2D,R1                                                         
         OI    TLW2STAT,X'40'      SET REPRINT BIT                              
         GOTO1 PUTREC                                                           
*                                                                               
         LA    R1,KEY                                                           
         USING TLDRD,R1                                                         
         OI    TLDRSTAT,X'40'      SET REPRINT BIT                              
         GOTO1 WRITE                                                            
*                                                                               
UW2X     B     XIT                                                              
         EJECT                                                                  
         USING TAW4D,R3            RESET USING WITH R3                          
*              ROUTINE VALIDATES GST TAX NUMBER AT R2                           
*              FIRST CHAR IS ALPHABETIC FOLLOWED BY 9 NUMERIC                   
         SPACE                                                                  
VALGST   DS    0H                                                               
         CLI   5(R2),10            INPUT LENGTH MUST BE 10                      
         BNE   INVERR                                                           
         CLI   8(R2),C'A'          TEST 1ST INPUT CHAR IS BETWEEN A-Z           
         BL    INVERR                                                           
         CLI   8(R2),C'Z'                                                       
         BH    INVERR                                                           
         SPACE                                                                  
         MVC   DUB(9),=9C'0'       INIT DUB TO 000000000                        
         MVZ   DUB(9),9(R2)        CHECK INPUT CHARS 2-10                       
         CLC   DUB(9),=9C'0'       FOR VALID NUMERIC                            
         BNE   INVERR                                                           
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO TALENT FILE                               
*                                                                               
SETTAL   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         MVC   SYSDIR,=C'TALDIR'                                                
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO CHECK FILE                                
*                                                                               
SETCHK   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         B     XIT                                                              
         EJECT                                                                  
*        ROUTINE CHECKS THAT THERE ARE NO COMMERCIALS                           
*                BEFORE DELETING                                                
*                                                                               
         USING TLW4D,R3                                                         
         USING TLCAPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
*                                                                               
         MVI   TLCAPCD,TLCACCDQ    BUILD PASSIVE KEY FOR CAST                   
         MVC   TLCACSSN,TLW4SSN    USING SAME SSN AS RECORD                     
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCACSSN+L'TLCACSSN-TLCAPKEY),KEYSAVE                        
         BE    CHKD15              IF W4 HAS COMM'LS CANNOT DELETE              
         DROP  R4                                                               
*                                                                               
         USING TLECPD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   TLECPCD,TLECCCDQ    BUILD PASSIVE KEY FOR ECAST                  
         MVC   TLECCSSN,TLW4SSN    USING SAME SSN AS RECORD                     
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLECCEPI-TLECPD),KEYSAVE                                     
         BE    CHKD15              IF W4 HAS EPISODES CANNOT DELETE             
         DROP  R4                                                               
                                                                                
         USING TLW4PD,R4                                                        
         XC    KEY,KEY             NOW CHECK FOR CORP PASSIVE POINTER           
         LA    R4,KEY                                                           
*                                                                               
         MVI   TLW4PCD,TLW4CCDQ    BUILD PASSIVE KEY FOR CORP                   
         MVC   TLW4CCRP,TLW4SSN    USING SSN NUMBER                             
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLW4CSSN-TLW4PKEY),KEYSAVE                                   
         BE    CHKD15              IF CORP HAS W4S CANNOT DELETE                
         DROP  R4                                                               
*                                                                               
         USING TLCKPD,R4                                                        
         XC    KEY,KEY             NOW CHECK FOR CHECKS                         
         LA    R4,KEY                                                           
*                                                                               
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVI   TLCKPCD,TLCKECDQ    BUILD PASSIVE KEY FOR EMPLOYEE'S             
         MVC   TLCKESSN,TLW4SSN    CHECKS USING SSN NUMBER                      
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCKECUR-TLCKPKEY),KEYSAVE                                   
         BE    CHKD15              IF PERF HAS CHECKS CANNOT DELETE             
         BAS   RE,SETTAL           SET TALENT FILE                              
         DROP  R4                                                               
*                                                                               
CHKD10   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
*                                                                               
CHKD15   LA    R2,SW4SSNH          IF W4 CANNOT BE DELETED                      
         BAS   RE,SETTAL           SET TALENT FILE                              
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
*                                                                               
* VALIDATE TAX UNIT AND ADD THE ELEMENT TO THE RECORD                           
*                                                                               
GETTAX   NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT          EMPLOYEE WITHHOLDING ELEM                    
         USING TAWHD,R4                                                         
         MVI   TAWHEL,TAWHELQ                                                   
         MVI   TAWHLEN,TAWHLNQ                                                  
         MVC   TAWHEMP,TGTPEMP     EMPLOYER                                     
         ZIC   R3,5(R2)                                                         
         GOTO1 TAXVAL,DMCB,((R3),8(R2))                                         
         BNE   INVERR                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         BO    INVERR              FORWARD                                      
         BCTR  R3,0                LENGTH - 1 FOR COMPARE                       
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TAWHUNIT(0),8(R2)         ** EXECUTED                            
         OC    TAWHUNIT,SPACES                                                  
         SR    R0,R0               BUMP TO M/S FIELD                            
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             MARITAL STATUS                               
         BE    MISSERR                                                          
         MVI   TAWHSTAT,C'M'                                                    
         CLI   8(R2),C'M'                                                       
         BE    GET30                                                            
         MVI   TAWHSTAT,C'S'                                                    
         CLI   8(R2),C'S'                                                       
         BE    GET30                                                            
         MVI   TAWHSTAT,C'H'                                                    
         CLI   8(R2),C'H'                                                       
         BNE   INVERR                                                           
*                                                                               
GET30    SR    R0,R0               BUMP TO EXEMPTIONS FIELD                     
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             EXEMPTIONS                                   
         BE    MISSERR             BLANK IS ZERO                                
         BAS   RE,VALNUM                                                        
         STC   R1,TAWHEXS                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    GET40                                                            
         LA    R3,TAWHFLAT                                                      
         BRAS  RE,VALFLAT                                                       
*                                                                               
*        ZIC   RF,5(R2)                                                         
*        GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE RATE XX.XX%                     
*        CLI   0(R1),0                                                          
*        BNE   INVERR                                                           
*        CLC   4(4,R1),=F'10000'   DISALLOW GT 100% (ALSO NEG. RATES)           
*        BNL   INVERR                                                           
*        MVC   TAWHFLAT+2,6(R1)    SAVE 2 BYTES                                 
*                                                                               
GET40    GOTO1 ADDELEM                                                          
*                                                                               
GETX     B     XIT                                                              
         DROP  R4,R3                                                            
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE A NUMERIC FIELD                              
*                                                                               
VALNUM   DS    0H                                                               
         MVC   WORK(9),=9X'F0'                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(9),=9X'F0'     INSURES NUMERIC                              
         BNE   INVNUM                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         RETURN PACKED AMOUNT IN DUB                  
         CVB   R1,DUB              AND BINARY AMOUNT IN R1                      
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
* ROUTINE TO FIND END OF STRING AND INSERT A COMMA                              
*                                                                               
SHUFFLE  DS    0H                  R2=A(FARTHEST POSS. END OF STRING)           
         CLI   0(R2),C' '          SHUFFLE BACK TO END                          
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','          INSERT COMMA                                 
         LA    R2,2(R2)            RETURN R2=A(NEXT AVAILABLE SLOT)             
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
*              ROUTINE TO CLEAR TRAILING COMMA                                  
*                                                                               
TRAIL    DS    0H                  R2=A(1 PAST END OF STRING)                   
         BCTR  R2,0                                                             
         CLI   0(R2),C','          IF IT'S AROUND                               
         BNER  RE                                                               
         MVI   0(R2),C' '          CLEAR TRAILING COMMA                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
IRSERR   MVI   ERROR,ERIRSRES      IRS RESTRICTED                               
         B     ERRXIT                                                           
*                                                                               
W2MSG    MVI   MYMSGNO1,71         W2 WILL BE REPRINTED                         
         B     INFXIT                                                           
*                                                                               
W2CONF   MVI   MYMSGNO1,245        HIT PF20 TO CONFIRM W2 REPRINT               
         OI    STATUS,STATW2       SET CONFIRM PENDING STATUS                   
         B     INFXIT                                                           
*                                                                               
NOW2MSG  MVI   MYMSGNO1,72         NO W2 TO PRINT                               
         B     INFXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
NODELETE MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         B     ERRXIT                                                           
*                                                                               
NOINPERR MVI   ERROR,ERNOINP       NO INPUT ALLOWED                             
         B     ERRXIT                                                           
*                                                                               
COMPERR  MVI   ERROR,ERCOMPT       INCOMPATIBLE WITH PREV INPUT                 
         STC   R1,ERRDISP                                                       
         B     ERRXIT                                                           
*                                                                               
INVNUM   MVI   ERROR,NOTNUM        INVALID NUMBER                               
         B     ERRXIT                                                           
*                                                                               
INVPID   LA    R2,SW4SSNH          INVALID PID                                  
         B     INVERR                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     ERRXIT                                                           
*                                                                               
ERLLCAA  MVC   MYMSGNO,=Y(ERRLLCAA) LLC ALREADY HAS AN ASSOCIATED W4            
         B     NTHEEND                                                          
*                                                                               
ERLLC1W  MVC   MYMSGNO,=Y(ERRLLC1W) LLC CAN ONLY HAVE ONE ASSOCIATED W4         
         B     NTHEEND                                                          
*                                                                               
ZIPERR   MVI   ERROR,ERBADZIP      ZIP CODE NOT VALID FOR STATE                 
         B     ERRXIT                                                           
*                                                                               
SECERR   MVI   ERROR,SECLOCK       SECURITY LOCK OUT                            
         B     ERRXIT                                                           
*                                                                               
INVSTAT  MVI   ERROR,INVALID       STATUS INPUT - INVALID                       
         STC   R1,ERRDISP                                                       
         B     ERRXIT                                                           
*                                                                               
INVSSN   MVC   MYMSGNO,=Y(ERINVSSN)  INVALID SSN                                
         B     NTHEEND                                                          
*                                                                               
ERRREF   MVC   MYMSGNO,=Y(ERRW4REF)  RECORD MUST BE REFRESHED                   
         B     NTHEEND                                                          
*                                                                               
ZIPWARN  LA    R2,SW4ADDZH                                                      
         MVC   MYMSGNO,=Y(ERRW4ZIP)  ZIP CODE CHANGED WARNING                   
         B     NTHEEND                                                          
*                                                                               
ERPPPND  MVC   MYMSGNO,=Y(ERRPPPND)  CAN'T MAKE MINOR - PENDING P+ CHKS         
         B     NTHEEND                                                          
*                                                                               
ERW4TCN  MVC   MYMSGNO,=Y(ERRW4TCN)  W4 TYPE CHANGE NOT ALLOWED                 
         B     NTHEEND                                                          
*                                                                               
ERW4TCT  MVC   MYMSGNO,=Y(ERRW4TCT)  ON CAST - CANT CHG TYPE TO TRUSTEE         
         B     NTHEEND                                                          
*                                                                               
W4AGNTC  MVC   MYMSGNO,=Y(EW4AGNTC)  ON AGENT, MUST BE C                        
         B     NTHEEND                                                          
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
NTHEEND  OI    GENSTAT2,USGETTXT   NEW THEEND FOR TWO BYTE ERROR MSGS           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR       TYPE=ERROR                                   
         GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CHKAGNT  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD       ADDING NEW W4 RECORD?                        
         JE    XIT                                                              
*                                                                               
         LA    R2,SW4TYPEH                                                      
         MVC   SVW4KEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING TLANPD,RF                                                        
         MVI   TLANPCD,TLANSCDQ                                                 
         MVC   TLANSSSN,TGSSN                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLANSAGT-TLANPCD),KEYSAVE                                    
         JNE   *+12                                                             
         CLI   8(R2),C'C'          IF W4 HAS AGENT, THEN MUST BE C              
         JNE   W4AGNTC                                                          
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLW4KEY),SVW4KEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
*                                                                               
SVW4KEY  DS    XL(L'TLW4KEY)                                                    
         LTORG                                                                  
*                                                                               
         USING TAW4D,R3            RESET USING WITH R3                          
         SPACE 1                                                                
*                                                                               
*        VALIDATE FLAT DEC XX.XX                                                
*              R2 - A(FIELD HEADER)                                             
*              R3 - A(OUTPUT)                                                   
*                                                                               
VALFLAT  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(RF) VALIDATE RATE XX.XX%                     
         CLI   0(R1),0                                                          
         JNE   INVERR                                                           
         CLC   4(4,R1),=F'10000'   DISALLOW GT 100% (ALSO NEG. RATES)           
         JNL   INVERR                                                           
         MVC   2(2,R3),6(R1)    SAVE 2 BYTES                                    
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*        VALIDATE SPECIAL LETTER FIELD                                          
*                                                                               
         USING TAW4D,R3                                                         
VALSLET  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGCTSTTY,TASTTYPP   IF NOT A PROGRAMMER                          
         BE    VSLET10                                                          
         CLI   TGCTSTTY,TASTTYP2   PROGRAMMING MANAGER                          
         BE    VSLET10                                                          
         CLI   TGCTSTTY,TASTTYPA   ACCOUNTING STAFF                             
         BE    VSLET10                                                          
         CLI   TGCTSTTY,TASTTYPB   OR ACCOUNTING MANAGER                        
         BE    VSLET10                                                          
         CLI   ACTNUM,ACTADD       AND ACTION IS ADD                            
         BNE   *+8                                                              
         MVI   SAVSLET,C'N'        DEFAULT TO N                                 
         MVC   SW4SLET,SAVSLET     RESTORE ORIGINAL VALUE                       
         MVI   SW4SLETH+5,1        OF SPECIAL LETTER FIELD                      
         OI    SW4SLETH+6,X'80'                                                 
         SPACE                                                                  
VSLET10  LA    R2,SW4SLETH         R2=A(SPECIAL LETTER FIELD)                   
         GOTO1 ANY                                                              
         CLI   8(R2),C'N'          MUST BE A Y OR N                             
         BE    VSLETX                                                           
         CLI   8(R2),C'Y'                                                       
         JNE   INVERR                                                           
         OI    TAW4STA3,TAW4SSPL                                                
VSLETX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD AN ADDRESS ELEMENT TO W4 RECORD AT AIO            
                                                                                
VALADDR  NTR1  BASE=*,LABEL=*                                                   
         XC    TGCTRY,TGCTRY                                                    
         XC    TGTACODE,TGTACODE                                                
                                                                                
         USING TAA2D,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INITIALIZE ADDRESS ELEMENT                   
         MVI   TAA2EL,TAA2ELQ                                                   
         MVI   TAA2LEN,TAA2LNQ                                                  
                                                                                
         LA    R2,SW4CTRYH         VALIDATE COUNTRY                             
         CLI   5(R2),0                                                          
         JNE   VADDR10                                                          
         CLI   SW4TYPE,TAW4TYTR    (OPTIONAL IF TYPE IS TRUSTEE)                
         JE    VADDR20                                                          
VADDR10  GOTO1 VALCTRY,DMCB,(R2)                                                
         JNE   ERRXIT                                                           
         MVC   TAA2CTRY,TGCTRY                                                  
                                                                                
         NI    TAW4STA2,X'FF'-TAW4SFGN                                          
         CLC   TGCTRY,=C'US'       IF COUNTRY IS PROVIDED AND IS                
         JE    VADDR30             NOT US                                       
         OI    TAW4STA2,TAW4SFGN   TURN ON FOREIGN ADDRESS STATUS               
                                                                                
VADDR20  CLI   SW4ADD3H+5,0        IF COUNTRY IS NOT US                         
         JE    VADDR30             DISALLOW INPUT IN ADDRESS                    
         LA    R2,SW4ADD3H         LINE 3                                       
         J     INVERR                                                           
                                                                                
VADDR30  LA    R0,3                SET TO PROCESS 3 ADDRESS LINES               
         LA    R5,TAA2ADD1                                                      
         LA    R2,SW4ADDRH                                                      
         SR    RF,RF                                                            
                                                                                
VADDR40  CLI   5(R2),0             IF THERE'S INPUT IN THIS FLD                 
         JE    VADDR50                                                          
         MVC   0(L'TAA2ADD1,R5),SPACES                                          
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R5),8(R2)       MOVE DATA TO ELEMENT                         
         LA    R5,L'TAA2ADD1(R5)   BUMP TO NEXT FIELD IN ELEMENT                
                                                                                
VADDR50  IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         TM    1(R2),X'20'         BUMP TO NEXT UNPROTECTED FIELD               
         JO    VADDR50                                                          
         BCT   R0,VADDR40                                                       
                                                                                
         LA    R2,SW4ADDRH                                                      
         OC    TAA2ADD1,TAA2ADD1   AT LEAST 1 ADDRESS LINE REQUIRED             
         JZ    MISSERR                                                          
                                                                                
         LA    R2,SW4ADDCH         CITY                                         
         CLI   5(R2),0                                                          
         JNE   VADDR60                                                          
         OC    TGCTRY,TGCTRY       OPTIONAL IF COUNTRY IS NOT                   
         JZ    VADDR70             PROVIDED, OTHERWISE REQUIRED                 
         J     MISSERR                                                          
VADDR60  MVC   TAA2CITY,SW4ADDC                                                 
         OC    TAA2CITY,SPACES                                                  
                                                                                
VADDR70  LA    R2,SW4ADDSH         STATE/PROVINCE                               
                                                                                
         OC    TGCTRY,TGCTRY       IF COUNTRY IS NOT PROVIDED                   
         JNZ   VADDR80                                                          
         CLI   5(R2),0             INPUT IS NOT ALLOWED                         
         JE    VADDR100                                                         
         B     INVERR                                                           
                                                                                
VADDR80  MVC   WORK(2),SW4ADDS                                                  
         OC    WORK(2),SPACES                                                   
         MVI   WORK+2,C' '                                                      
         GOTO1 TAXVAL,DMCB,(X'FF',WORK)                                         
         JNE   ERRXIT                                                           
         TM    TGTASTAT,TASUINGF   ENSURE TAX UNIT IS VALID GOING               
         JO    INVERR              FORWARD                                      
                                                                                
         CLC   TGCTRY,=C'US'       IF COUNTRY IS US                             
         JNE   VADDR90                                                          
         CLC   TGTAZIPF,SPACES                                                  
         JE    INVERR              INVALID STATE IF DON'T HAVE 1ST              
         CLC   TGTAZIPT,SPACES     2 ZIP DIGITS FROM AND TO                     
         JE    INVERR                                                           
VADDR90  MVC   TAA2ST,SW4ADDS                                                   
                                                                                
VADDR100 LA    R2,SW4ADDZH                                                      
         CLI   5(R2),0             ZIP CODE IS REQUIRED                         
         JNE   VADDR110                                                         
         OC    TGCTRY,TGCTRY       IF COUNTRY HAS BEEN PROVIDED                 
         JNZ   MISSERR                                                          
         J     VADDR120                                                         
                                                                                
VADDR110 OC    TGCTRY,TGCTRY       IF COUNTRY HAS NOT BEEN PROVIDED             
         JZ    INVERR              ZIP CODE IS NOT ALLOWED                      
         BRAS  RE,USZIPVAL         VALIDATE IF COUNTRY IS US                    
         BRAS  RE,CAZIPVAL         VALIDATE IF COUNTRY IS CA                    
         MVC   TAA2ZIP,SW4ADDZ                                                  
         OC    TAA2ZIP,SPACES                                                   
                                                                                
         MVI   W4ZIPFLG,C'N'                                                    
         CLI   SW4TYPE,TAW4TYIN    INDIVIDUAL?                                  
         JE    *+12                                                             
         CLI   SW4TYPE,TAW4TYES    ESTATE?                                      
         JNE   VADDR120                                                         
                                                                                
         BRAS  RE,GETCITY          GET CITY FROM ZIP CODE                       
                                                                                
         CLC   TGCTRY,=C'US'       IF COUNTRY IS PROVIDED AND IS                
         JNE   VADDR112                                                         
                                                                                
         MVC   SW4CITY,SVW4CITY                                                 
         MVI   SW4CITYH+5,L'SVW4CITY                                            
         OI    SW4CITYH+6,X'80'                                                 
         OC    SVW4CITY,SVW4CITY                                                
         JNZ   VADDR112                                                         
         MVI   SW4CITYH+5,0        IF NO CITY, CLEAR OUT CITY                   
         XC    SW4CMS,SW4CMS       TAX DETAIL FIELDS                            
         OI    SW4CMSH+6,X'80'                                                  
         XC    SW4CEXM,SW4CEXM                                                  
         OI    SW4CEXMH+6,X'80'                                                 
         XC    SW4CFIX,SW4CFIX                                                  
         OI    SW4CFIXH+6,X'80'                                                 
                                                                                
VADDR112 CLI   ACTNUM,ACTADD                                                    
         JE    *+14                                                             
         CLC   SVW4ZIP,TAA2ZIP     CHANGED ZIP CODE?                            
         JE    VADDR115                                                         
*        JE    VADDR120                                                         
         CLC   ORIGCITY,SVW4CITY   CHANGED CITY?                                
         JE    VADDR115                                                         
         MVI   W4ZIPFLG,C'Y'                                                    
                                                                                
VADDR115 CLC   TGTACODE,SW4SSTA                                                 
         JE    *+8                                                              
         MVI   W4ZIPFLG,C'Y'                                                    
                                                                                
         CLC   TGCTRY,=C'US'       IF COUNTRY IS PROVIDED AND IS                
         JNE   VADDR120                                                         
                                                                                
         MVC   SW4SSTA,TGTACODE                                                 
         MVI   SW4SSTAH+5,2                                                     
         OI    SW4SSTAH+6,X'80'                                                 
                                                                                
VADDR120 GOTO1 ADDELEM             ADD THE ELEMENT                              
         J     XIT                                                              
         LTORG                                                                  
         SPACE 3                                                                
*              ROUTINE TO DISPLAY PID INSTEAD OF SS#                            
         SPACE 1                                                                
DISPID   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SW4SSN,SPACES                                                    
         MVC   SW4SSN(L'TGPID),TGPID                                            
         MVI   SW4SSNH+5,6                                                      
         OI    SW4SSNH+6,X'80'                                                  
DPIDX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE PID NUMBER                                       
         SPACE 1                                                                
SVPID    NTR1  BASE=*,LABEL=*                                                   
         USING TLW4D,R3                                                         
         L     R3,AIO                                                           
                                                                                
         USING TANUD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,3+L'TGPID                                                
         MVI   TANUTYPE,TANUPIDN                                                
         GOTO1 SSNPACK,DMCB,TLW4SSN,TANUMBER                                    
         GOTO1 ADDELEM                                                          
         DROP  R3,R4                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE CORPORATION ID                                                       
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
         USING TAW4D,R3                                                         
VALCID   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SW4CIDH                                                       
         CLI   5(R2),0              IF CORP NO INPUT ALLOWED                    
         BNE   VCID10                                                           
         MVI   ELCODE,TATIELQ       ENSURE DELETE FOR CHANGE                    
         GOTO1 DELL,DMCB,=C'C1'                                                 
         B     VCIDX                                                            
*                                                                               
VCID10   CLI   TAW4TYPE,TAW4TYCO   DON'T ALLOW FOR CORPS OR TRUSTEES            
         BE    NOINPERR                                                         
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    NOINPERR                                                         
*                                                                               
         MVC   SVDSKADD,DMDSKADD   SAVE DISK ADDRESS TO RE-GET REC              
         MVC   SVSSNUM,TGSSN       SAVE GLOBAL SS NUMBER                        
*                                                                               
         DROP  R3                  DROP PREVIOUS USING TAW4D                    
         MVC   AIO,AIO2            CHANGE IO AREAS                              
         LA    R2,SW4CIDH          ENSURE NUM  IS REALLY A CORP                 
         MVC   CORPSSN,SW4CID                                                   
*                                                                               
         CLI   SW4CIDH+5,6         CONVERT TO SS# ON SCREEN                     
         BH    VCID15                                                           
         BL    INVERR                                                           
         MVC   TGDUB(L'TGPID),SW4CID                                            
         GOTO1 SSNUNPK,DMCB,TGDUB,CORPSSN                                       
         BNE   VCID15                                                           
         MVC   SW4CID,CORPSSN                                                   
         MVI   SW4CIDH+5,9                                                      
*                                                                               
VCID15   GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',(R2))                                 
*                                                                               
         USING TLW4PD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             IF CORPORATION HAS AN L IN                   
         MVI   TLW4PCD,TLW4LCDQ    FILTER FIELD, ENSURE THAT THIS               
         MVC   TLW4LFID,CORPSSN    IS ITS ONLY ASSOCIATION                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4PKEY),KEYSAVE                                          
         JNE   VCID18                                                           
         XC    KEY,KEY                                                          
         MVI   TLW4PCD,TLW4CCDQ                                                 
         MVC   TLW4CCRP,CORPSSN                                                 
         GOTO1 HIGH                                                             
         J     VCID17                                                           
VCID16   GOTO1 SEQ                                                              
VCID17   CLC   KEY(TLW4CSSN-TLW4PCD),KEYSAVE                                    
         JNE   VCID18                                                           
         CLC   TLW4CSSN,SVSSNUM                                                 
         JNE   ERLLCAA                                                          
         J     VCID16                                                           
         DROP  R3                                                               
*                                                                               
VCID18   GOTO1 SSNPACK,DMCB,CORPSSN,TGDUB                                       
         MVC   SW4CID,SPACES                                                    
         MVC   SW4CID(L'TGPID),TGDUB                                            
         MVI   SW4CIDH+5,6                                                      
         OI    SW4CIDH+6,X'80'                                                  
*                                                                               
         L     R4,AIO                                                           
         USING TAW4D,R4                                                         
         MVC   AIO,AIO1            RESTORE IO AREAS                             
         MVC   TGSSN,SVSSNUM       RESTORE GLOBAL SS NUMBER                     
         MVI   ELCODE,TAW4ELQ      GET DETAILS ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAW4TYPE,TAW4TYCO   INPUT MUST BE CORPORATION                    
         BNE   INVERR                                                           
*                                                                               
         CLC   =C'ON FILE',8(R2)   IF CORP #1 DOES NOT EXIST                    
         BE    VCIDX               BUT OTHERS DO - CONTINUE                     
         MVI   ELCODE,TATIELQ      ENSURE SAME CORP DOESN'T EXIST               
         USING TATID,R4                                                         
         L     R4,AIO              ALREADY IN RECORD                            
         BAS   RE,GETEL                                                         
         BNE   VCID40                                                           
*                                                                               
VCID20   CLI   TATITYPE,TATITYCO   THIS ELEMENT MUST BE A CORP                  
         BNE   VCID30                                                           
         CLC   CORPSSN,TATIID      IF IT EXISTS - INVALID                       
         BE    INVERR                                                           
*                                                                               
VCID30   BAS   RE,NEXTEL                                                        
         BE    VCID20                                                           
         DROP  R4                                                               
*                                                                               
VCID40   XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TATID,R4                                                         
         MVI   TATIEL,TATIELQ                                                   
         MVI   TATILEN,TATILNQ                                                  
         MVI   TATITYPE,TATITYCO   CORP                                         
         MVI   TATICRPN,C'1'       CORP NUMBER                                  
                                                                                
         MVC   TATIID(L'CORPSSN),CORPSSN                                        
         OC    TATIID,SPACES                                                    
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING A RECORD - DO NOT                  
         BE    VCIDX                  TRY TO RE-GET RECORD                      
         L     R4,AIO              ELSE                                         
         XC    KEY,KEY                RE-GET REC - PREVENT PUTREC PROB          
         MVC   KEY(L'TLDRREC),0(R4)         RESTORE KEY                         
         MVC   KEY+L'TLDRREC-4(4),SVDSKADD  INCLUDING DISK ADDRESS              
         MVC   AIO,AIO2                     DO NOT OVERWRITE CHANGES            
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1            RESTORE AIO1                                 
*                                                                               
VCIDX    B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* PRINT OUT TAX INFO FROM WITHHOLDING ELEMENT                                   
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
         USING TAWHD,R4                                                         
OUTWITH  NTR1  BASE=*,LABEL=*                                                   
         MVC   8(3,R2),TAWHUNIT      FEDERAL                                    
         OC    8(3,R2),SPACES                                                   
         SR    R0,R0                 BUMP TO NEXT FIELD                         
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVC   8(1,R2),TAWHSTAT      MARITAL STATUS                             
         OI    4(R2),X'20'           SET FIELD VALID                            
         SR    R0,R0                 BUMP TO NEXT FIELD                         
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         EDIT  (B1,TAWHEXS),(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         OI    4(R2),X'20'            SET FIELD VALID                           
         SR    R0,R0                  EXEMPTIONS   BUMP TO NEXT FIELD           
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         ICM   R3,15,TAWHFLAT                                                   
         BRAS  RE,EDITFLAT                                                      
         OI    4(R2),X'20'            SET FIELD VALID                           
*                                                                               
OUTX     B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                       *         
***********************************************************************         
                                                                                
STATTAB  DS    0CL11               TABLE TO COVER STATUS BYTE                   
         DC    AL1(TAW4STNY),CL10'NOYTD'   NO YEAR TO DATE ON CHECKS            
         DC    AL1(TAW4SCKF),CL10'CKFIRST' SORT CHECKS FIRST                    
         DC    X'FF'                                                            
*                                                                               
         SPACE 1                                                                
STATTAB2 DS    0CL11                    TABLE TO COVER 2ND STATUS BYTE          
         DC    AL1(TAW4SIRS),CL10'IRS'     IRS LOCK ON FED WITHHOLDING          
         DC    AL1(TAW4SLCK),CL10'LOCKED'  RECORD LOCKED                        
**NO-OP**DC    AL1(TAW4SNFI),CL10'NOFICA'  NO FICA WITHHOLDING                  
         DC    AL1(TAW4SNDU),CL10'NODUC'   NO DUE COMPANY WITHHOLDING           
         DC    AL1(TAW4SNPE),CL10'NOPEN'   NO PENSION                           
         DC    AL1(TAW4SFGN),CL10'FGNADD'  FOREIGN ADDRESS                      
         DC    AL1(TAW4SDD),CL10'DIRECT'   DIRECT DEPOSIT CHECKS                
         DC    AL1(TAW4SWIR),CL10'WIRE'    WIRE TRANSFER CHECKS                 
         DC    X'FF'                                                            
         SPACE 1                                                                
STATTAB3 DS    0CL11                    TABLE TO COVER 3RD STATUS BYTE          
         DC    AL1(TAW4SEFT),CL10'EFT'     EFT CHECKS                           
         DC    AL1(TAW4SREG),CL10'REG'     REGRESSION TESTING W4                
         DC    X'FF'                                                            
         SPACE 3                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PAYEE   ',CL8'DISPLAY '                               
PF13     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CCOM    ',CL8'LIST    '                               
PF14     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3' ',CL8'YTD     ',CL8'DISPLAY '                               
PF15     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'LIEN    ',CL8'LIST    '                               
PF16     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP ',CL8'LIST    '                               
PF17     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF18X-*,18,0,(PF18X-PF18)/KEYLNQ,0)                          
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF18     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF18X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,(PF19X-PF19)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
PF19     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF19X    EQU   *                                                                
*                                                                               
         DC    AL1(PF20X-*,20,0,(PF20X-PF20)/KEYLNQ,0)                          
         DC    CL3' ',CL8'TD1     ',CL8'DISPLAY '                               
PF20     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF20X    EQU   *                                                                
*                                                                               
         DC    AL1(PF21X-*,21,0,(PF21X-PF21)/KEYLNQ,0)                          
         DC    CL3' ',CL8'TRUST   ',CL8'DISPLAY '                               
PF21     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF21X    EQU   *                                                                
*                                                                               
         DC    AL1(PF22X-*,22,0,(PF22X-PF22)/KEYLNQ,0)                          
         DC    CL3' ',CL8'GCON    ',CL8'LIST    '                               
PF22     DC    AL1(KEYTYTWA,L'SW4SSN-1),AL2(SW4SSN-T702FFD)                     
PF22X    EQU   *                                                                
*                                                                               
         DC    AL1(PF23X-*,23,PFTINT,0,PFTRETRN)                                
         DC    CL3' ',CL8'        ',CL8'        '                               
PF23X    EQU   *                                                                
*                                                                               
         DC    AL1(PF24X-*,24,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF24X    EQU   *                                                                
*                                                                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES ZIP CODE FOR US W4S                        *         
*        ON ENTRY ... R2 = A(ZIP CODE FIELD)                          *         
*                     R3 = A(W4 DETAILS ELEMENT)                      *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
USZIPVAL NTR1  BASE=*,LABEL=*                                                   
         TM    TAW4STA2,TAW4SFGN   EXIT IF COUNTRY IS NOT US                    
         JO    XIT                                                              
         DROP  R3                                                               
                                                                                
         MVC   DUB(9),=9C'0'       INIT DUB TO 000000000                        
                                                                                
         CLI   5(R2),5             INPUT LENGTH MUST BE 5 OR 10                 
         JE    USZV10                                                           
         CLI   5(R2),10                                                         
         JNE   INVERR                                                           
         CLI   13(R2),C'-'         IF LNGTH IS 10, 6TH CHAR MUST BE "-"         
         JNE   INVERR                                                           
         MVZ   DUB+5(4),14(R2)                                                  
                                                                                
USZV10   MVZ   DUB(5),8(R2)                                                     
         CLC   DUB(9),=9C'0'       CHECK VALID NUMERIC                          
         JNE   INVERR                                                           
                                                                                
         CLC   8(3,R2),TGTAZIPF    CHECK FIRST 3 DIGITS FIT INTO RANGE          
         JL    ZIPERR                                                           
         CLC   8(3,R2),TGTAZIPT                                                 
         JNH   USZV20                                                           
                                                                                
         CLC   TGTACODE,=C'GA '    IF GEORGIA,                                  
         JNE   ZIPERR                                                           
         CLC   8(3,R2),=C'398'     398 - 399 ALSO ALLOWED                       
         JL    ZIPERR                                                           
         CLC   8(3,R2),=C'399'                                                  
         JH    ZIPERR                                                           
                                                                                
USZV20   CLC   TGTACODE,=C'PR '    IF PUERTO RICO                               
         JNE   USZV30                                                           
         CLC   8(3,R2),=C'008'     CAN'T START WITH 008                         
         JE    ZIPERR                                                           
         J     XIT                                                              
                                                                                
USZV30   CLC   TGTACODE,=C'VI '    IF VIRGIN ISLANDS                            
         JNE   XIT                                                              
         CLC   8(5,R2),=C'00851'   ONLY 00801-00851 IS VALID                    
         JH    ZIPERR                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR USZIPVAL ROUTINE                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES ZIP CODE FOR CANADIAN W4S                  *         
*        ON ENTRY ... R2 = A(ZIP CODE FIELD)                          *         
***********************************************************************         
                                                                                
CAZIPVAL NTR1  BASE=*,LABEL=*                                                   
         CLC   TGCTRY,=C'CA'       IF COUNTRY IS CANADA                         
         JNE   XIT                                                              
         CLI   5(R2),7             INPUT LENGTH MUST BE 7                       
         JNE   INVERR                                                           
                                                                                
         GOTO1 VALCANUM,DMCB,SW4ADDZ+1                                          
         GOTO1 VALCAALP,DMCB,SW4ADDZ+2                                          
         JNE   INVERR                                                           
         CLI   SW4ADDZ+3,C' '                                                   
         JNE   INVERR                                                           
         GOTO1 VALCANUM,DMCB,SW4ADDZ+4                                          
         GOTO1 VALCAALP,DMCB,SW4ADDZ+5                                          
         JNE   INVERR                                                           
         GOTO1 VALCANUM,DMCB,SW4ADDZ+6                                          
                                                                                
         USING CTRYSUBD,R1                                                      
         L     R1,TGACTRY                                                       
         LA    R1,CTRYSZIP                                                      
         LHI   R0,L'CTRYSZIP                                                    
CZV10    CLC   SW4ADDZ(1),0(R1)                                                 
         JE    XIT                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,CZV10                                                         
         J     INVERR                                                           
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES CHARACTER IS VALID NUMERIC                   *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
VALCANUM NTR1                                                                   
         L     RE,0(R1)                                                         
         CLI   0(RE),C'0'                                                       
         JL    INVERR                                                           
         CLI   0(RE),C'9'                                                       
         JH    INVERR                                                           
         J     XIT                                                              
                                                                                
       ++INCLUDE TAVCAALP                                                       
***********************************************************************         
*        CONSTANTS AND LITERALS FOR CAZIPVAL ROUTINES                 *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE "TAKE FOREIGN TAX" FIELD                 *         
*        ON ENTRY ... R3 = A(W4 DETAILS ELEMENT)                      *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
VALFTX   NTR1  BASE=*,LABEL=*                                                   
         CLI   TAW4TYPE,TAW4TYFO   IF TYPE IS FOREIGNER                         
         JE    VALFTX00                                                         
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         JNE   XIT                                                              
VALFTX00 CLI   SW4FTXH+5,0         DEFAULT "TAKE FOREIGN TAX?" TO Y             
         JE    VALFTX10                                                         
         LA    R2,SW4FTXH                                                       
         CLI   SW4FTX,C'Y'         VALID VALUES: Y OR N                         
         JE    VALFTX10                                                         
         CLI   SW4FTX,C'N'                                                      
         JNE   INVERR                                                           
         OI    TAW4STA3,TAW4SNTX   IF N, TURN ON STATUS                         
                                                                                
VALFTX10 CLC   TAW4TYPE,SVW4TYPE   IF W4 TYPE IS NOT BEING CHANGED              
         JNE   XIT                                                              
         CLI   TGCTSTTY,TASTTYPP   AND STAFF IS NOT PROGRAMMER                  
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYPA   ACCOUNTING                                   
         JE    XIT                                                              
         CLI   TGCTSTTY,TASTTYPB   OR ACCOUNTING MANAGER                        
         JE    XIT                                                              
         MVC   TGBYTE,SVW4STA3     IGNORE ANY INPUT IN THE FIELD                
         NI    TGBYTE,TAW4SNTX                                                  
         NI    TAW4STA3,X'FF'-TAW4SNTX                                          
         OC    TAW4STA3,TGBYTE                                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VALFTX ROUTINE                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE "NEW HIRE ACT" FIELD                     *         
*        ON ENTRY ... R3 = A(W4 DETAILS ELEMENT)                      *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
VALNHA   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGCTSTTY,TASTTYPP   IF STAFF CATEGORY IS NOT PROGRAMMER          
         JE    VNHA10                                                           
         CLI   TGCTSTTY,TASTTYPA   ACCOUNTING STAFF                             
         JE    VNHA10                                                           
         CLI   TGCTSTTY,TASTTYPB   OR ACCOUNTING MANAGERS                       
         JE    VNHA10                                                           
         CLI   TAW4TYPE,TAW4TYIN   AND TYPE IS AN INDIVIDUAL                    
         JNE   VNHA10                                                           
         MVC   TGBYTE,SVW4STA3                                                  
         NI    TGBYTE,TAW4SNHA+TAW4SNHP                                         
         OC    TAW4STA3,TGBYTE     RETAIN ORIGINAL NHA VALUES                   
         MVC   TAW4NHAD,SVW4NHAD                                                
         J     XIT                                                              
                                                                                
VNHA10   CLI   SW4NHAH+5,0         IF INPUT IS PROVIDED                         
         JE    XIT                                                              
         CLI   TAW4TYPE,TAW4TYIN   TYPE MUST BE INDIVIDUAL                      
         JNE   VNHAINV                                                          
                                                                                
         CLI   SW4NHAH+5,1         IF ONLY 1 CHAR, HAS TO BE P                  
         BE    VNHA20                                                           
                                                                                
         LA    R2,SW4NHAH          R2=A(DATE OF BIRTH)                          
         GOTO1 DTVAL,DMCB,(X'80',TAW4NHAD)                                      
         OI    TAW4STA3,TAW4SNHA                                                
         J     XIT                                                              
                                                                                
VNHA20   CLI   SW4NHA,C'P'         OR P                                         
         JNE   VNHAINV                                                          
         OI    TAW4STA3,TAW4SNHP                                                
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VNHAINV  LA    R2,SW4NHAH                                                       
         JNE   INVERR                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VALFTX ROUTINE                    *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE PHONE NUMBER FIELD                       *         
***********************************************************************         
                                                                                
VALPHONE NTR1  BASE=*,LABEL=*                                                   
         J     XIT                                                              
                                                                                
         CLI   SW4PHONH+5,0        IF PHONE NUMBER HAS BEEN ENTERED             
         JE    XIT                                                              
         OC    SW4PHON,SPACES      PAD IT WITH SPACES                           
                                                                                
         LA    R1,SW4PHON                                                       
         LHI   R0,L'SW4PHON        THEN ENSURE THAT IT ONLY INCLUDES            
VP10     CLI   0(R1),C' '          SPACES                                       
         JE    VP20                                                             
         CLI   0(R1),C'('          OPEN PARENTHESIS                             
         JE    VP20                                                             
         CLI   0(R1),C')'          CLOSE PARENTHESIS                            
         JE    VP20                                                             
         CLI   0(R1),C'-'          HYPENS                                       
         JE    VP20                                                             
         CLI   0(R1),C'0'          AND NUMBERS BETWEEN 0                        
         JL    VPINV                                                            
         CLI   0(R1),C'9'          AND 9                                        
         JH    VPINV                                                            
VP20     LA    R1,1(R1)                                                         
         BCT   R0,VP10                                                          
         J     XIT                                                              
                                                                                
VPINV    LA    R2,SW4PHONH                                                      
         MVI   ERROR,INVALID       IF IT INCLUDES ANYTHING ELSE                 
         GOTO1 EXIT,DMCB,0         RETURN ERROR                                 
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR VALPHONE ROUTINE                  *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE FILTERS FIELD                            *         
***********************************************************************         
                                                                                
VALFILT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SW4FILTH         FILTERS                                      
         CLI   5(R2),0                                                          
         JE    XIT                                                              
                                                                                
         USING TLW4PD,R3                                                        
         CLI   SW4FILT,TA99TYL     IF FIRST FILTER IS "L"                       
         JNE   VFILT10                                                          
         CLI   SW4TYPE,TAW4TYCO    AND MAINTAINING A CORPORATION                
         JNE   VFILT10                                                          
         CLI   ACTNUM,ACTADD                                                    
         JE    VFILT10                                                          
         LA    R3,KEY                                                           
         XC    KEY,KEY             ENSURE THAT CORPORATION                      
         MVI   TLW4PCD,TLW4CCDQ    DOES NOT HAVE MORE THAN ONE                  
         MVC   TLW4CCRP,TGSSN      ASSOCIATION                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLW4CSSN-TLW4PCD),KEYSAVE                                    
         JNE   VFILT10                                                          
         GOTO1 SEQ                                                              
         CLC   KEY(TLW4CSSN-TLW4PCD),KEYSAVE                                    
         JE    ERLLC1W                                                          
         DROP  R3                                                               
*                                                                               
VFILT10  XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAFLD,R4                                                         
         MVI   TAFLEL,TAFLELQ                                                   
         MVI   TAFLLEN,TAFLLNQ                                                  
         OC    SW4FILT,SPACES                                                   
         MVC   TAFLFLT1(4),SW4FILT                                              
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE FREQUENCY FIELD                          *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
VALFREQ  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SW4FREQH         FREQUENCY                                    
                                                                                
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         JNE   VFREQ10                                                          
         CLC   SVW4FREQ,SW4FREQ    DO NOT ALLOW VALUE TO BE                     
         JNE   INVERR              MODIFIED                                     
                                                                                
VFREQ10  CLI   5(R2),0                                                          
         JE    XIT                                                              
         MVI   TAW4FREQ,C'A'       ANNUALLY                                     
         CLI   SW4FREQ,C'A'                                                     
         JE    XIT                                                              
         MVI   TAW4FREQ,C'M'       MONTHLY                                      
         CLI   SW4FREQ,C'M'                                                     
         JE    XIT                                                              
         MVI   TAW4FREQ,C'W'       WEEKLY                                       
         CLI   SW4FREQ,C'W'                                                     
         JE    XIT                                                              
         MVI   TAW4FREQ,C'Q'       QUARTERLY                                    
         CLI   SW4FREQ,C'Q'                                                     
         JNE   INVERR                                                           
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO GET CITY FROM ZIP CODE                            *         
*        ON ENTRY, R4 = A(TAA2ELEM)                                             
***********************************************************************         
                                                                                
         USING TAA2D,R4                                                         
GETCITY  NTR1  BASE=*,LABEL=*                                                   
         XC    SVW4CITY,SVW4CITY                                                
         LA    R2,ZIPTAB                                                        
GCITY10  CLI   0(R2),X'FF'                                                      
         JE    GETCITYX                                                         
*                                                                               
         CLC   TAA2ZIP(5),0(R2)     COMPARE AGAINST FROM ZIP CODE RANGE         
         JL    GCITY20                                                          
         CLC   TAA2ZIP(5),5(R2)    COMPARE AGAINST TO ZIP CODE RANGE            
         JH    GCITY20                                                          
         MVC   SVW4CITY,10(R2)                                                  
         J     GETCITYX                                                         
*                                                                               
GCITY20  AHI   R2,ZIPTABL                                                       
         J     GCITY10                                                          
*                                                                               
GETCITYX J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
ZIPTAB   DC    C'45200',C'45299',C'CIN'                                         
ZIPTABL  EQU   *-ZIPTAB                                                         
         DC    C'45999',C'45999',C'CIN'                                         
         DC    C'48200',C'48299',C'DET'                                         
         DC    C'10000',C'10499',C'NYC'                                         
         DC    C'11004',C'11005',C'NYC'                                         
         DC    C'11100',C'11499',C'NYC'                                         
         DC    C'11600',C'11699',C'NYC'                                         
         DC    C'44100',C'44199',C'CLV'                                         
         DC    C'19100',C'19199',C'PHL'                                         
         DC    C'19019',C'19019',C'PHL'                                         
         DC    C'19092',C'19093',C'PHL'                                         
         DC    C'19099',C'19099',C'PHL'                                         
         DC    C'19244',C'19244',C'PHL'                                         
         DC    C'19255',C'19255',C'PHL'                                         
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE IS CALLED WHEN W4 IS BEING SET UP AS A TRUSTEE WHEN  *         
*        IT WAS NOT PREVIOUSLY. ROUTINE ENSURES THAT THERE ARE NO     *         
*        PENDING P+ CHECKS WHEN THIS OCCURS                           *         
***********************************************************************         
                                                                                
CHKPPHST NTR1  BASE=*,LABEL=*                                                   
         USING TLW4D,R4                                                         
         L     R4,AIO1                                                          
         MVC   TGSSN,TLW4SSN                                                    
         DROP  R4                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY           READ ALL CHECK RECORDS FOR THIS                
         MVI   TLCKPCD,TLCKECDQ  PERFORMER FOR THIS AGENCY                      
         MVC   TLCKESSN,TGSSN                                                   
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'P+ '                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKEDTE-TLCKPD+1),KEYSAVE                                   
         JE    ERPPPND                                                          
         DROP  R3                                                               
                                                                                
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF W4 TYPE IS CHANGING, ROUTINE ENSURES THAT THERE ARE NO    *         
*        PAYMENTS SITTING OUT THERE THAT HAVE NOT YET RUN THROUGH     *         
*        BILLING OR CHECKS                                            *         
*        ON ENTRY ... R3=A(W4 DETAILS ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
CHKPEND  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD                                                    
         JE    XIT                                                              
         LA    R2,SW4TYPEH                                                      
         CLC   SVW4TYPE,TAW4TYPE   IF W4 TYPE IS BEING CHANGED                  
         JNE   CPEND10                                                          
         TM    SVW4STA2,TAW4SLCK   OR W4 IS GOING FROM UNLOCKED                 
         JO    XIT                                                              
         TM    TAW4STA2,TAW4SLCK   TO LOCKED                                    
         JZ    XIT                                                              
         LA    R2,SW4STATH                                                      
         DROP  R3                                                               
                                                                                
         USING TLW4D,R4                                                         
CPEND10  L     R4,AIO1                                                          
         MVC   TGSSN,TLW4SSN       SAVE SOCIAL SECURITY NUMBER                  
         DROP  R4                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' PREPARE TO READ ALL CHECKS FOR               
         MVC   SYSFIL,=CL8'CHKFIL' THIS W4                                      
         MVC   AIO,AIO2                                                         
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKPCD,TLCKECDQ    PERFORMER                                    
         MVC   TLCKESSN,TGSSN                                                   
         GOTO1 HIGH                                                             
         J     CPEND30                                                          
CPEND20  GOTO1 SEQ                                                              
CPEND30  CLC   KEY(TLCKECUR-TLCKPD),KEYSAVE                                     
         JNE   CPENDX                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO2                                                          
         MVC   TGINV,TLCKINV                                                    
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   CPEND20                                                          
         TM    TAPDSTAT,TAPDSCNL   IF CHECK IS CANCELLED/CANCELLER              
         JO    CPEND20                                                          
         TM    TAPDPST1,TAPDPBNP   OR A BNP                                     
         JO    CPEND20                                                          
         OC    TAPDPAYI,TAPDPAYI   OR A ZERO DOLLAR CHECK                       
         JNZ   CPEND40             NO NEED TO CHECK THE CHECK DATE              
         OC    TAPDPAYC,TAPDPAYC                                                
         JNZ   CPEND40                                                          
         OC    TAPDREXP,TAPDREXP                                                
         JZ    CPEND20                                                          
         DROP  R4                                                               
                                                                                
         USING TACDD,R4                                                         
CPEND40  L     R4,AIO2             OTHERWISE ...                                
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   CPEND20                                                          
         OC    TACDDTE,TACDDTE     IF CHECK DOES NOT HAVE A CHECK               
         JZ    ERW4TCN             DATE, RETURN ERROR                           
         J     CPEND20                                                          
         DROP  R4                                                               
                                                                                
CPENDX   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'24',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        IF W4 TYPE IS CHANGING TO TRUSTEE, ROUTINE ENSURES THAT      *         
*        PERFORMER IS NOT ASSOCIATED WITH ANY CAST RECORDS            *         
*        ON ENTRY ... R3=A(W4 DETAILS ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAW4D,R3                                                         
CHKTCAST NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD                                                    
         JE    XIT                                                              
         LA    R2,SW4TYPEH                                                      
         CLC   SVW4TYPE,TAW4TYPE   IF W4 TYPE IS BEING CHANGED                  
         JE    XIT                                                              
         CLI   TAW4TYPE,TAW4TYTR   TO A TRUSTEE                                 
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         USING TLW4D,R4                                                         
         L     R4,AIO1                                                          
         MVC   TGSSN,TLW4SSN       SAVE SOCIAL SECURITY NUMBER                  
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCACCDQ    READ CAST RECORDS                            
         MVC   TLCACSSN,TGSSN      FOR THIS PERFORMER                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACCOM-TLCAPD),KEYSAVE                                     
         JE    ERW4TCT                                                          
         DROP  R3                                                               
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        EDIT FLAT RATE XX.XX                                         *         
*        R2 - A (FIELD HEADER)                                        *         
*        R3 - AMOUNT TO BE EDITED                                     *         
***********************************************************************         
                                                                                
EDITFLAT NTR1  BASE=*,LABEL=*                                                   
         EDIT  (R3),(6,8(R2)),2,ALIGN=LEFT,ZERO=BLANK                           
         OI    6(R2),X'80'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES NAME IN P+ EMPLOYEE AND TIMESHEET RECORDS    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
UPDETNAM NTR1  BASE=*,LABEL=*                                                   
         TM    STATUS,STATNMCH     IF NAME CHANGED                              
         JZ    XIT                                                              
                                                                                
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO3            AND PREPARE TO DO WORK IN AIO3               
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY              READ ALL CAST RECORDS FOR THIS               
         XC    KEY,KEY             W4                                           
         MVI   TLCAPCD,TLCACCDQ                                                 
         MVC   TLCACSSN,TGSSN                                                   
         GOTO1 HIGH                                                             
         J     UETN20                                                           
UETN10   GOTO1 SEQ                                                              
UETN20   CLC   KEY(TLCACCOM-TLCAPCD),KEYSAVE                                    
         JNE   UETN30                                                           
         BAS   RE,UPDNAM           AND UPDATE THE NAME                          
         J     UETN10                                                           
         DROP  R3                                                               
                                                                                
UETN30   MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1            AND AIO                                      
         GOTO1 GETREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE UPDATES NAME IN P+ EMPLOYEE AND TIMESHEET RECORDS    *         
*        ON ENTRY ... R3 = A(IOKEY OF EMPLOYEE/TIMESHEET RECORD)      *         
***********************************************************************         
                                                                                
UPDNAM   NTR1                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TANMD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANMELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
                                                                                
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVC   SVNMKEY,KEY                                                      
                                                                                
         MVC   TANMCRPN,NEW4CRPN                                                
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK)                                      
                                                                                
         MVC   KEY,SVNMKEY                                                      
         GOTO1 HIGH                                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR1CD                                                       
         EJECT                                                                  
*                                                                               
         ORG   SW4WORK                                                          
*                                                                               
RELO     DS    F                                                                
SVDSKADD DS    XL4                 SAVED DISK ADDRESS                           
SVW4CRPN DS    CL32                SAVED NAME                                   
NEW4CRPN DS    CL32                NEW NAME                                     
SVW4STAT DS    XL1                 SAVED STATUS BYTE                            
SVW4STA2 DS    XL1                 SAVED 2ND STATUS BYTE                        
SVW4STA3 DS    XL1                 SAVED 3RD STATUS BYTE                        
SVEXMPTN DS    0XL3                SAVED EXEMPTIONS                             
SVEXMPTF DS    CL1                 FEDERAL                                      
SVEXMPTS DS    CL1                 STATE                                        
SVEXMPTC DS    CL1                 CITY                                         
SVACCDTE DS    XL(L'TAACCDTE)      SAVED LAST CHANGED DATE                      
SVACCTIM DS    XL(L'TAACCTIM)      SAVED LAST CHANGED TIME                      
STATUS   DS    XL1                 PROGRAM STATUS BYTE                          
STATPFOK EQU   X'80'               SET INTERNAL PF KEY OK                       
STATW2   EQU   X'40'               CONFIRM PRINT W2 PENDING                     
STATNMCH EQU   X'20'               NAME CHANGED                                 
*                                                                               
SVSSNUM  DS    CL9                 SAVED SS NUMBER                              
SVKEY    DS    CL38                SAVED KEY                                    
MYBYTE   DS    XL1                                                              
W2FLAG   DS    CL1                                                              
SVW4TYPE DS    CL1                 SAVED W4 TYPE                                
NEW4TYPE DS    CL1                 NEW W4 TYPE                                  
SAVSLET  DS    CL1                 SAVED SPECIAL LETTER                         
SVW4NHAD DS    XL3                 SAVED W4 NHA DATE                            
SVW4FREQ DS    CL1                 SAVED FREQUENCY CODE                         
SVW4ZIP  DS    CL(L'TAA2ZIP)       ADDRESS ZIP CODE                             
SVW4CITY DS    CL3                 CITY (FROM ZIPTAB LOOKUP)                    
W4ZIPFLG DS    CL1                 ZIP CODE FLAG (Y = CHANGED ZIP CODE)         
*                                                                               
ORIGCITY DS    CL3                 ORIGINAL CITY                                
ORIGMINR DS    CL1                 ORIGINALLY A MINOR? (Y/N)                    
*                                                                               
EMPTAB   DS    10CL3               ROOM FOR 10 EMPLOYERS                        
*                                                                               
TEMPTAWX DS    XL12                AMT COLLECTED & W/HELD FOR 3 EMPS            
*                                                                               
CORPSSN  DS    XL(L'TGSSN)         CORPORATION SS#                              
*                                                                               
SVNMKEY  DS    XL(L'KEY)                                                        
*                                                                               
         DS    0D                                                               
BLOCK1   DS    CL100                                                            
PTRBLK   DS    CL((4*L'TLDRREC)+1)  PASSIVE POINTERS BLOCK                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         SPACE 5                                                                
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127TAGEN1C   09/09/15'                                      
         END                                                                    
