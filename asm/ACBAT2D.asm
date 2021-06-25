*          DATA SET ACBAT2D    AT LEVEL 048 AS OF 05/12/14                      
*PHASE T61B2DC                                                                  
         TITLE 'MULTIPLE ANALYSED J/E'                                          
*                                                                               
*        BATCH TYPES 45 & 53                                                    
*                                                                               
T61B2D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**T61B2D,R7,RR=R5,CLEAR=YES                         
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
*                                                                               
         TM    0(R1),TYPIOSC       TEST PRELIMINARY CALL FOR HEADINGS           
         BZ    PLIN04                                                           
         MVI   SBYTE,INTOFF        SET INTEROFFICE OFFICE                       
         LA    RF,SBYTE                                                         
         GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),(RF),0                         
*                                                                               
         CLI   0(R1),SECPYES       WRITE ACCESS?                                
         BNE   PLIN02              NO                                           
*                                                                               
         LA    R3,ATRINTTH         YES, SHOW INTOFF HEADING                     
         NI    1(R3),X'FF'-(FATBLOW)                                            
         OI    6(R3),X'80'                                                      
*                                                                               
         CLI   CSACT,ACTOPN        OPENING A BATCH?                             
         BNE   PLIN01              NO                                           
         LA    R3,ATRINTH          YES, UNPROTECT INTOFF FIELD                  
         NI    1(R3),X'FF'-(FATBPROT)                                           
*                                                                               
PLIN00   OI    6(R3),X'80'                                                      
         B     XIT                                                              
*                                                                               
PLIN01   CLI   CSACT,ACTDSP        ARE WE DISPLAYING                            
         BNE   XIT                                                              
         LA    R3,ATRINTH          YES, HIGHLIGHT THE OFFICE                    
         OI    1(R3),FATBHIGH                                                   
         B     PLIN00                                                           
*                                                                               
PLIN02   CLI   0(R1),SECPREAD      READ ACCESS, SHOW THE FIELD                  
         BNE   PLIN03                                                           
         CLI   CSACT,ACTOPN        CREATING NEW BATCH?                          
         BE    XIT                 YES, DON'T SHOW INTOFF                       
         LA    R3,ATRINTTH         NO, SHOW IT                                  
         NI    1(R3),X'FF'-(FATBLOW)                                            
         OI    6(R3),X'80'                                                      
         B     PLIN01              GO HIGHLIGHT THE OFFICE                      
*                                                                               
PLIN03   CLI   CSACT,ACTDSP        NO ACCESS AND DISPLAY                        
         BNE   XIT                                                              
         LA    R2,ATRINTH                                                       
         MVC   FLD,SPACES                                                       
         BRAS  RE,MOVEFLD          CLEAR THE INTOFF FIELD                       
         B     XIT                                                              
*                                                                               
PLIN04   GOTO1 DICTATE,DMCB,C'LU  ',DDIN1,DDOUT1                                
         CLI   CSOMODE,CSOMPLIN    TEST 'PROTECT' INPUT SCREEN LINE             
         BNE   ATR002                                                           
         LA    R3,ATRDCH           R3=A(FIRST DETAIL LINE)                      
         SR    R0,R0                                                            
         IC    R0,CSOLINE          R0=LINE NUMBER TO 'PROTECT'                  
         B     *+8                                                              
         LA    R3,ATRDC2H-ATRDCH(R3)                                            
         BCT   R0,*-4                                                           
         LR    R2,R3               SAVE A(LINE)                                 
         LA    R5,ATRDC2H-ATRDCH-1(R3)                                          
         SR    R4,R4                                                            
*                                                                               
PLIN06   IC    R4,FVTLEN-FVIHDR(R3)                                             
         SH    R4,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R3),FVAXTND                                        
         BZ    *+8                                                              
         SH    R4,=Y(L'FVIHDR)                                                  
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R3),L'FVIHDR(R3)                                      
         IC    R4,FVTLEN-FVIHDR(R3)                                             
         BXLE  R3,R4,PLIN06                                                     
*                                                                               
         MVI   FVIFLD-FVIHDR(R2),C'*'                                           
         IC    R4,FVTLEN-FVIHDR(R2)                                             
         AR    R2,R4                                                            
         LH    R4,=Y(UC@DELD-TWAD)                                              
         LA    R4,TWAD(R4)                                                      
         MVC   L'FVIHDR(L'UC@DELD,R2),0(R4)                                     
         B     XIT                                                              
*                                                                               
ATR002   CLI   CSACT,ACTINP        TEST ITEM/INPUT                              
         BNE   ATR010                                                           
         TM    BOINDS1,BOISCAND    TEST SCREEN SCANNED                          
         BZ    ATR1B               JUST VALIDATE THE SCREEN                     
*                                                                               
         LA    R2,BASSRVH                                                       
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,ATRDOCH          TOP OF SCREEN                                
         LA    R3,ATRTOTH          BOTTOM OF SCREEN                             
*                                                                               
ATR006   TM    1(R2),X'20'         PROTECTED                                    
         BO    ATR008                                                           
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BZ    ATR020                                                           
*                                                                               
ATR008   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R3               ARE WE AT BOTTOM                             
         BL    ATR006                                                           
         TWAXC ATRDCH,ATRTABH,PROT=Y    SO CLEAR THE SCREEN                     
         LA    R2,ATRDOCH          AND POINT CURSOR TO TOP                      
         B     EXIT                                                             
*                                                                               
ATR010   TM    CSOMODE,CSOMDCHA    TEST DO ITEM/CHANGE                          
         BO    ATR022                                                           
         TM    CSOMODE,CSOMPCHA    TEST PREPARE CHANGE/DISPLAY/DELETE           
         BO    *+6                                                              
         DC    H'0'                UNKNOWN FUNCTION                             
         LA    R2,1                FIRST SCREEN LINE                            
         LA    R1,ATRDCH                                                        
ATR012   LA    R0,7                7 FIELDS PER ITEM                            
         CLM   R2,1,CSOLINE        TEST THIS IS THE ITEM FOR CHANGE             
         LA    R2,1(R2)                                                         
         BNE   *+8                                                              
         LA    R1,ATRDC2H-ATRDCH(R1)                                            
ATR014   SR    RE,RE                                                            
         ICM   RE,1,FVTLEN-FVIHDR(R1)                                           
         BZ    EXIT                                                             
         SH    RE,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RE,=Y(L'FVIHDR)                                                  
         LTR   RE,RE               TEST ZERO LENGTH FIELD                       
         BZ    ATR016                                                           
         BCTR  RE,0                                                             
         CLI   CSACT,ACTCHA        ACTION CHANGE?                               
         BE    ATR016              LEAVE DATA ALONE                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    L'FVIHDR(0,R1),L'FVIHDR(R1)                                      
ATR016   OI    FVATRB-FVIHDR(R1),FVAPROT                                        
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         SR    RE,RE                                                            
         IC    RE,FVTLEN-FVIHDR(R1)                                             
         AR    R1,RE                                                            
         BCT   R0,ATR014                                                        
         B     ATR012                                                           
*                                                                               
ATR020   MVI   CSOLINE,0           ITEM/INPUT - CLEAR LINE COUNT                
ATR022   MVI   ACTV,C'N'           ACTIVITY ON PREVIOUS LINES                   
         ZAP   DEBITS,=P'0'                                                     
         ZAP   CREDITS,=P'0'                                                    
         CLI   MODE,0                                                           
         BNE   ATR1                                                             
         MVI   MODE,1                                                           
         XC    SAVECAC,SAVECAC                                                  
ATR1     ZAP   DEBITS,CSLSTCUR+LSTBTDRS-LSTTABD(L'LSTBTDRS)                     
         ZAP   CREDITS,CSLSTCUR+LSTBTCRS-LSTTABD(L'LSTBTCRS)                    
         EJECT                                                                  
*--------------------------------------------------------------                 
*        VALIDATE AND SAVE DOCUMENT NUMBER AND DATE                             
*--------------------------------------------------------------                 
*                                                                               
ATR1B    LA    R2,ATRDOCH                                                       
         BAS   RE,ANY                                                           
         CLI   INPUT,53            FOR TYPE 53                                  
         BNE   ATR1C                                                            
         CLI   5(R2),6             REFERENCE CAN'T BE MORE THAN 5               
         BL    ATR1C                                                            
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
ATR1C    MVC   SAVEDOC,ATRDOC                                                   
         OC    SAVEDOC,SPACES                                                   
         LA    R2,ATRDATH                                                       
         CLI   INPUT,53            FOR TYPE 53                                  
         BNE   *+8                                                              
         MVI   SAVEDOC+5,C'A'                                                   
         BAS   RE,ANY              DATE IS REQUIRED                             
         MVI   ERRNUM,13                                                        
         CLI   ATRDATH+5,0                                                      
         BNE   ATR2                                                             
         BAS   RE,GETODAY                                                       
         B     ATR3                                                             
*                                                                               
ATR2     GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
ATR3     GOTO1 DATCON,DMCB,(0,WORK),(1,DATE3)                                   
         CLI   DATE3,X'70'                                                      
         BL    ERROR                                                            
         GOTO1 DATECHK,DMCB,DATE3                                               
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         OI    4(R2),X'20'         TURN ON VALIDATED BIT                        
*                                                                               
*        VALIDATE AND SAVE NARRATIVE                                            
*                                                                               
         LA    R2,ATRNARH                                                       
         LA    R3,SAVENARR                                                      
         XC    SAVENARR,SAVENARR                                                
         BAS   RE,NARRSCAN                                                      
         STH   R6,LEN              LENGTH OF NARRATIVE STRING                   
*                                                                               
         LA    R2,ATRNARH                                                       
         OI    4(R2),X'20'         TURN ON VALIDATED BIT                        
         ZIC   RF,0(R2)            IN BOTH NARRATIVE LINES                      
         AR    R2,RF                                                            
         OI    4(R2),X'20'                                                      
*                                                                               
         MVI   OFFSW,C'N'          TEST FOR MANDATORY OFFICE                    
         TM    COMPSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
*                                                                               
ATR4     LA    R2,ATRINTH                                                       
         CLI   5(R2),0             ANY OFFICE ENTERED?                          
         BE    ATR5                NO                                           
         GOTO1 AVALOFFC,DMCB,(X'80',ATRINT)                                     
         CLI   ERRNUM,OK                                                        
         BE    ATR5                                                             
         MVI   ERRNUM,SPECIAL                                                   
         MVC   MSGNO,=Y(AE$IVOFF)                                               
         B     ERROR                                                            
*                                                                               
ATR5     OI    4(R2),X'20'                                                      
         LA    R2,ATRDCH           POINT TO DEBIT/CREDIT                        
         CLI   CSACT,ACTCHA        TEST CHANGE                                  
         BNE   ATR7                                                             
         SR    R1,R1               POINT TO LINE TO BE CHANGED                  
         ICM   R1,1,CSOLINE                                                     
         BNZ   *+6                                                              
         DC    H'0'                LINE NUMBER MUST BE PROVIDED                 
         BCTR  R1,0                                                             
         MH    R1,=Y(ATRDC2H-ATRDCH)                                            
         AR    R2,R1                                                            
ATR7     BAS   RE,ANY                                                           
*                                                                               
ATR8     XC    FFTELEM,FFTELEM                                                  
         CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BE    ATR8C                                                            
         SR    R3,R3                                                            
         IC    R3,CSOLINE          NUMBER OF LINES INPUT                        
         LA    R3,1(R3)                                                         
         STC   R3,CSOLINE                                                       
         CLI   8(R2),C'*'          LINE VALIDATED PREVIOUSLY?                   
         BNE   ATR9                                                             
         CH    R3,=H'5'            ARE WE ON THE LAST LINE                      
         BNE   ATR8A                                                            
         CLI   ACTV,C'Y'           ANY ACTIVITY ON PREVIOUS LINES               
         BE    ATR500                                                           
         LA    R2,ATRDCH                                                        
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
ATR8A    LA    R2,ATRDC2H-ATRDCH(R2) GO TO NEXT LINE                            
         B     ATR8                                                             
*                                                                               
ATR8C    CLI   FVIFLD-FVIHDR(R2),C'*'                                           
         BNE   ATR9                                                             
         LA    R2,ATRDCH                                                        
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
ATR9     CLI   5(R2),0             IS LINE BLANK                                
         BNE   ATR9A                                                            
         CLI   ACTV,C'Y'           ANY ACTIVITY ON PREVIOUS LINES               
         BE    ATR400                                                           
         LA    R2,ATRDCH                                                        
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
ATR9A    MVI   ACTV,C'Y'                                                        
         ST    R2,SAVEDC                                                        
         MVI   ERRNUM,2                                                         
         CLI   5(R2),1             INPUT MUST BE D OR C                         
         BNE   ERROR                                                            
         MVI   POSTDC,C'D'                                                      
         CLI   8(R2),C'D'                                                       
         BE    ATR10                                                            
         MVI   POSTDC,C'C'                                                      
         CLI   8(R2),C'C'                                                       
         BNE   ERROR                                                            
*                                                                               
ATR10    ZIC   RF,0(R2)            POINT TO ACCOUNT FIELD                       
         AR    R2,RF                                                            
         ST    R2,SAVER2                                                        
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),8(R2)                                                   
         SR    R6,R6               NO PROFILES                                  
         MVI   ERRNUM,18                                                        
         CLC   KEY+1(2),=C'S9'                                                  
         BE    ERROR                                                            
         BAS   RE,GETACC                                                        
         MVC   POSTACC,ACCTNUM                                                  
         MVC   POSTACCN,ACCTNAME                                                
         MVI   HALF,C'Y'                                                        
         BAS   RE,TESTIT           VERIFY VALID UNIT/LEDGER                     
         MVI   ERRNUM,18                                                        
         CLI   HALF,C'Y'                                                        
         BNE   ERROR                                                            
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'      LOCKED OR CLOSED                             
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,CHKXJOB          EXCLUDE X-JOBS                               
         CLI   ERRNUM,45           IS ACCOUNT AN XJOB                           
         BE    ERROR               YES                                          
*                                                                               
         MVI   STFSW,C'N'                                                       
         MVI   DEPSW,C'N'                                                       
         TM    ACCTSTAT,X'40'      PERSONAL EXPENSE (STAFF=Y)                   
         BZ    ATR40                                                            
         MVI   STFSW,C'Y'                                                       
*                                                                               
ATR40    TM    ACCTSTAT,X'08'      DEPT EXPENSE (DEPT=Y)                        
         BZ    *+8                                                              
         MVI   DEPSW,C'Y'                                                       
*                                                                               
         MVC   COSTBYTE,ACCTCOST   (ANALYSIS=)                                  
         OC    COSTBYTE,SPACES                                                  
*                                                                               
         MVI   COSTSW,0                                                         
         TM    COMPSTAT,X'10'                                                   
         BZ    ATR46                                                            
         CLI   ACCOST,C' '                                                      
         BE    ATR46                                                            
         OI    COSTSW,OLDCOST                                                   
         CLC   POSTACC+1(2),=C'SE'                                              
         BE    *+8                                                              
         MVI   COSTSW,0                                                         
*                                                                               
*                                                                               
ATR46    CLI   DEPSW,C'Y'          DOES ACCT HAVE DEPT=Y                        
         BNE   ATR47               GET THE 28 ACCOUNT                           
         MVC   KEY+1(2),=C'28'                                                  
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRDSNUM,ACCTNUM                                                  
         MVC   CRDSNAME,ACCTNAME                                                
*                                                                               
ATR47    CLI   POSTDC,C'C'         IF CREDIT, MAY NEED TO 'CHECKSA'             
         BNE   ATR50                                                            
         CLC   =C'SA',POSTACC+1    OPEN ITEM ADVANCES                           
         BE    ATR48                                                            
         CLC   =C'SB',POSTACC+1    MISC. LEDGER                                 
         BNE   ATR50                                                            
*                                                                               
ATR48    L     R2,SAVER2           POINT TO ACCOUNT FIELD                       
         MVI   ERRNUM,0                                                         
         BAS   RE,CHECKSA          FIND MATCHING DEBIT TRANSACTION              
         CLI   ERRNUM,0                                                         
         BNE   ERROR                                                            
*                                                                               
ATR50    L     R2,SAVER2                                                        
         ZIC   RF,0(R2)            POINT TO C/A                                 
         AR    R2,RF                                                            
         CLI   5(R2),0             IF NO C/A INPUT                              
         BNE   ATR55                                                            
         OC    SAVECAC,SAVECAC     USE SAVED C/A IF PRESENT                     
         BNZ   *+8                                                              
         BAS   RE,ANY                                                           
         MVC   POSTCAC,SAVECAC                                                  
         MVC   POSTCACN,SAVECACN                                                
         MVI   ERRNUM,2                                                         
ATR51    CLC   POSTACC+1(2),=C'SR' SAVED C/A CAN ONLY BE USED IF                
         BE    ATR52               BOTH PREVIOUS AND CURRENT ACCOUNTS           
         CLI   POSTCAC,X'40'       ARE S/R, OR IF NEITHER PREVIOUS OR           
         BNE   ATR72               CURRENT ACCOUNTS ARE S/R                     
         B     ERROR                                                            
*                                                                               
ATR52    CLC   POSTCAC(3),SPACES                                                
         BE    ATR72                                                            
         B     ERROR                                                            
*                                                                               
ATR55    DS    0H                                                               
         CLI   8(R2),X'41'         CAN'T START INPUT WITH A BLANK               
         BL    ERROR                                                            
         MVC   POSTCACN,SPACES                                                  
         MVC   WORK(15),8(R2)                                                   
         MVI   ERRNUM,2                                                         
         CLC   POSTACC+1(2),=C'SR'     IF ACCOUNT IS S/R                        
         BNE   ATR65                                                            
*                                                                               
ATR57    CLC   WORK(3),=C'***'     CONTRA MUST START WITH *** AND               
         BNE   ERROR                                                            
         CLC   WORK+4(1),SPACES    MUST HAVE INPUT BEYOND ***                   
         BL    ERROR                                                            
         MVC   POSTCAC,SPACES                                                   
         MVC   POSTCAC+3(12),WORK+3                                             
         OC    POSTCAC+3(12),SPACES                                             
         B     ATR70                                                            
*                                                                               
ATR65    DS    0H                                                               
         CLC   WORK(3),=C'***'     ONLY S/R CAN START WITH ***                  
         BE    ERROR                                                            
         MVC   KEY+1(48),SPACES                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),WORK                                                    
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,17                                                        
         MVC   POSTCAC,KEYSAVE                                                  
         CLC   KEY(15),KEYSAVE                                                  
         BNE   BADACC                                                           
*                                                                               
ATR67    DS    0H                                                               
         MVC   KEY(1),COMPANY                                                   
         BAS   RE,GETACC                                                        
*                                                                               
         BAS   RE,CHKXJOB          EXCLUDE X-JOBS                               
         CLI   ERRNUM,45           IS ACCOUNT AN XJOB                           
         BE    ERROR               YES                                          
*                                                                               
         MVC   POSTCACN,ACCTNAME                                                
*                                                                               
ATR70    MVC   SAVECAC,POSTCAC                                                  
         MVC   SAVECACN,POSTCACN                                                
*                                                                               
ATR72    MVC   OFFICE,SPACES       INITIALIZE                                   
         MVC   DEPT,SPACES                                                      
         MVC   STAFF,SPACES                                                     
         MVC   AOFFC,SPACES                                                     
         XC    OFFICEL,OFFICEL                                                  
         XC    DEPTL,DEPTL                                                      
         XC    STAFFL,STAFFL                                                    
         XC    AOFFCL,AOFFCL                                                    
*                                                                               
         ZIC   RF,0(R2)            POINT TO OFF/DEPT/STAFF                      
         AR    R2,RF                                                            
         ST    R2,SAVEODS          SAVE ADDRESS OF OFF/DEPT/STAFF               
         XC    BLOCK(96),BLOCK                                                  
         MVI   ERRNUM,INVALID                                                   
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),C',=,,'                              
         CLI   DMCB+4,4                                                         
         BH    ERROR                                                            
*                                                                               
         MVC   OFFICEL,BLOCK       SAVE OFFICE AND LENGTH ENTERED               
         MVC   OFFICE(L'OFFICE),BLOCK+12                                        
         OC    OFFICE,SPACES                                                    
*                                                                               
         MVC   DEPTL,BLOCK+32      SAVE DEPT AND LENGTH ENTERED                 
         MVC   DEPT(L'DEPT),BLOCK+44                                            
         OC    DEPT,SPACES                                                      
*                                                                               
         MVC   STAFFL,BLOCK+64     SAVE STAFF AND LENGTH ENTERED                
         MVC   STAFF(L'STAFF),BLOCK+76                                          
         OC    STAFF,SPACES                                                     
*                                                                               
         MVC   AOFFCL,BLOCK+96     SAVE ANAL OFFC AND LEN ENTERED               
         MVC   AOFFC(L'AOFFC),BLOCK+108                                         
         CLI   AOFFCL,0                                                         
         BNE   ATR74                                                            
         MVC   AOFFC,OFFICE                                                     
         B     ATR75                                                            
*                                                                               
ATR74    XC    ANOELM,ANOELM                                                    
         OC    AOFFC,SPACES                                                     
         CLC   AOFFC,OFFICE                                                     
         BE    ATR75                                                            
         LA    R1,ANOELM           BUILD ANALYSED OFFICE ELEMENT                
         USING ANOELD,R1                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER     PERSON OFFICE                                
         MVC   ANOOFFC,AOFFC                                                    
*                                                                               
ATR75    CLC   PRODUL,POSTACC+1    TEST IF POSTING TO PRODUCTION                
         BNE   ATR80                                                            
         MVI   ERRNUM,NOINPUT                                                   
         CLI   WRKCODEL,2          OFFICE CONTAINS WORKCODE FOR PROD            
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLC   WRKCODE,=C'99'      AND NOT 99                                   
         BE    ERROR                                                            
         CLC   WRKCODE,=C'**'      OR '**'                                      
         BE    ERROR                                                            
*                                                                               
         MVC   KEY,SPACES          VALIDATE WORK CODE                           
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),PRODUL                                                  
         MVC   KEY+4(2),WRKCODE                                                 
         BAS   RE,READ                                                          
         BE    ATR80G                                                           
         MVI   ERRNUM,INVWC                                                     
         B     ERROR                                                            
*                                                                               
ATR80    DS    0H                                                               
         L     R2,SAVEODS          CURSOR TO OFFICE/DEP/STF FIELD               
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         CLI   ERRNUM,OK                                                        
         BE    ATR80A                                                           
         MVI   ERRNUM,SPECIAL                                                   
         MVC   MSGNO,=Y(AE$IVOFF)                                               
         B     ERROR                                                            
*                                                                               
ATR80A   BAS   RE,ERRCHCK          CHECK FOR ERROR IN OFF,DPT,STAFF             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR               ERROR HAS OCCURRED IN INPUT                  
*                                                                               
         XC    EXPACC,EXPACC                                                    
         CLC   POSTACC+1(2),=C'SE'                                              
         BNE   ATR80G                                                           
         MVC   EXPACC,POSTACC                                                   
         MVC   EXPACC(1),COMPANY                                                
         BAS   RE,GOCAT                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
ATR80G   XC    COSTNUM,COSTNUM                                                  
         XC    CLIPRON,CLIPRON                                                  
         XC    DEPSTFN,DEPSTFN                                                  
         ZIC   RF,0(R2)            POINT TO CLI/PROD                            
         AR    R2,RF                                                            
         ST    R2,SAVCLPR          SAVE ADDRESS OF CLI/PRO                      
         XC    SCAN(64),SCAN                                                    
         CLI   5(R2),0             IF NOTHING THERE, DON'T BOTHER               
         BE    ATR82               WITH SCANNER                                 
         CLI   STFSW,C'Y'                                                       
         BE    ATR80J                                                           
         TM    COSTSW,OLDCOST                                                   
         BO    ATR80J                                                           
*                                                                               
         MVC   MSGNO,=Y(AE$ANFAN)  INPUT TO CLIENT NOT ALLOWED                  
         B     ERROR                                                            
*                                                                               
ATR80J   MVI   ERRNUM,2                                                         
         GOTO1 SCANNER,DMCB,(R2),(4,SCAN),C',=,,'                               
         CLI   DMCB+4,2                                                         
         BH    ERROR                                                            
         LA    R5,COMPEL                                                        
         USING ACCOMPD,R5                                                       
         LA    R6,CLIPROF                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(48),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
         ZIC   R3,SCAN                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),SCAN+12    CLIENT                                       
         BAS   RE,GETACC                                                        
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
*                                                                               
         LA    R1,FFTELEM          BUILD FFTEL                                  
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLIPRO+3                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
         CLI   SCAN+32,0           IS PRODUCT INPUT                             
         BE    ATR81                                                            
         LA    R5,KEY+3                                                         
         IC    R3,CLILNGTH         LEVEL A LENGTH                               
         AR    R5,R3                                                            
         ZIC   R3,SCAN+32          PRODUCT LENGTH                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SCAN+44     PRODUCT                                      
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
*                                                                               
         LA    R1,FFTELEM          UPDATE FFTEL                                 
         USING FFTELD,R1                                                        
         MVC   FFTPRAC,CLIPRO+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
ATR81    BAS   RE,PROFMERG                                                      
         SR    R6,R6               NO MORE PROFILES                             
         LA    R5,PROFILE          FIND 1/C ACCOUNT                             
         USING ACPROFD,R5                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRCOST                                                 
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   COSTNAME,ACCTNAME                                                
         MVC   COSTNUM,ACPRCOST                                                 
         DROP  R5                                                               
*                                                                               
ATR82    DS    0H                                                               
         TM    COSTSW,OLDCOST                                                   
         BNO   ATR150                                                           
*                                                                               
         CLI   SCAN,0              IF ANALYSIS REQUIRED,                        
         BNE   ATR82G              CLIENT IS REQUIRED                           
         MVC   MSGNO,=Y(AE$ICLPQ)                                               
         L     R2,SAVCLPR                                                       
         B     ERROR                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        SET UP 1P/OFFICE/DPT/CAT ACCT                                          
*-------------------------------------------------------------                  
*                                                                               
ATR82G   DS    0H                                                               
         L     R2,SAVEODS          CURSOR TO OFFICE/DEP/STF FIELD               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1P'     SET UP 1/P ACCOUNT                           
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'Y'          DOES ACCOUNT HAVE OFFICE IN KEY?             
         BNE   ATR83               1P/OFFC/DPT/CATEGORY                         
         ZIC   RF,OFCLNGTH                                                      
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),AOFFC      ANALYSIS OFFICE                              
         LA    R1,1(RF,R1)         BUMP TO NEXT PART OF KEY                     
*                                                                               
ATR83    DS    0H                                                               
         LA    RF,=C'9999'         DEFAULT DEPARTMENT                           
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                 OR IF SWITCH IS ON THEN USE                  
         LA    RF,DEPT             REAL DEPARTMENT                              
*                                                                               
         ZIC   R3,DPTLNGTH         DEPARTMENT LENGTH                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RF)       MOVE IN EITHER REAL DPT OR 9999              
         LA    R1,1(R3,R1)         BUMP TO NEXT PART OF KEY                     
         MVC   0(1,R1),COSTBYTE                                                 
*                                                                               
ATR90    DS    0H                                                               
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST                                    
         BNO   *+10                                                             
         MVC   KEY+3(12),=12C'9'                                                
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC         BALANCE ELEMENT                              
         MVC   CRCNUM,ACCTNUM                                                   
         MVC   CRCNAME,ACCTNAME                                                 
         EJECT                                                                  
*-------------------------------------------------------------                  
*        SET UP 13 ACCT                                                         
*-------------------------------------------------------------                  
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'13'     U/L                                          
         MVC   KEY+3(1),COSTBYTE   COST ANALYSIS                                
*                                                                               
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST                                    
         BNO   ATR97                                                            
         OC    EXPACC,EXPACC                                                    
         BNZ   ATR94                                                            
         L     R2,SAVEDC                                                        
         MVI   ERRNUM,SPECIAL                                                   
         MVC   MSGNO,=Y(AE$MSE13)                                               
         B     ERROR                                                            
*                                                                               
ATR94    OI    COSTSW,NEWCOST                                                   
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         NI    COSTSW,X'FF'-OLDCOST                                             
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,EXPACC      DEBIT ACCOUNT                                
         MVC   CATOFF,AOFFC        OFFICE                                       
         MVC   CATDPT,DEPT         DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         CLI   CATERR,0                                                         
         BE    ATR96                                                            
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     ERROR                                                            
*                                                                               
ATR96    CLI   CATPST,C'N'         NO COST POSTING                              
         BE    ATR97                                                            
         MVC   COSTANAL,CATCDE                                                  
         MVC   CR13NUM,CATACC3     SAVE 13 ACCOUNT                              
         OI    COSTSW,OLDCOST                                                   
         B     ATR150                                                           
*                                                                               
ATR97    BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CR13NUM,ACCTNUM                                                  
         MVC   CR13NAME,ACCTNAME                                                
         B     ATR150                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        SET UP 2D ACCT                                                         
*-------------------------------------------------------------                  
*                                                                               
ATR150   DS    0H                                                               
         L     R2,SAVEODS          POINT TO OFF/DEPT/STAFF                      
         CLI   DEPSW,C'N'          NO 2D POSTING IF SWITCH NOT=Y                
         BE    ATR175                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'     SET UP 2/D ACCOUNT                           
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'Y'          OFFICE IN ACCT?                              
         BNE   ATR152                                                           
         ZIC   RF,OFCLNGTH                                                      
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),AOFFC       ANALYSIS OFFICE                              
         LA    R1,1(RF,R1)                                                      
*                                                                               
ATR152   DS    0H                                                               
         ZIC   RF,DPTLNGTH         LENGTH OF DEPT                               
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPNAME,ACCTNAME                                                 
         MVC   DEPNUM,ACCTNUM                                                   
         MVC   DEPSTFN,ACCTNAME                                                 
         B     ATR175                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        SET UP 2P ACCT                                                         
*-------------------------------------------------------------                  
*                                                                               
ATR175   DS    0H                                                               
         CLI   STFSW,C'Y'                                                       
         BNE   ATR190                                                           
*                                                                               
ATR178   MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'                                                  
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   LEVEL,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    ATR179                                                           
         ZIC   RF,OFCLNGTH                                                      
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),AOFFC       MOVE OFFICE INTO KEY                         
         LA    R1,1(RF,R1)         BUMP TO NEXT PLACE IN KEY                    
         EJECT                                                                  
*                                                                               
ATR179   DS    0H                                                               
         CLI   LEVEL,2                                                          
         BL    ATR180                                                           
         ZIC   RF,DPTLNGTH                                                      
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        MOVE DEPT INTO KEY                           
         LA    R1,1(RF,R1)         BUMP TO NEXT PLACE IN KEY                    
*                                                                               
ATR180   DS    0H                                                               
         ZIC   RF,STAFFL                                                        
         SH    RF,=H'1'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE DEPT INTO KEY                           
*                                                                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STAFFNUM,ACCTNUM                                                 
         MVC   STAFFNAM,ACCTNAME                                                
         MVC   DEPSTFN,ACCTNAME                                                 
         B     ATR181                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        SET UP 29 ACCT                                                         
*-------------------------------------------------------------                  
*                                                                               
ATR181   DS    0H                                                               
         ZIC   RF,0(R2)            POINT TO CLI/PROD                            
         AR    R2,RF                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'29'     SET UP 2/9 ACCOUNT                           
         MVC   KEY+3(3),=C'999'    DEFAULT IF NO OFFICE                         
*                                                                               
         CLI   OFFSW,C'Y'                                                       
         BNE   ATR183                                                           
         MVC   KEY+3(46),SPACES                                                 
         LA    RF,AOFFC            REAL OFFICE                                  
         ZIC   R1,OFCLNGTH                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(RF)      MOVE IN OFFICE                               
         LA    RF,KEY+3                                                         
         LA    RF,1(R1,RF)         BUMP TO DEPT PART OF KEY                     
         MVC   0(4,RF),=C'9999'    MOVE IN DEFAULT CLIENT                       
*                                                                               
ATR183   OC    COSTNUM,COSTNUM     OR USE COSTING TO OVERRIDE ACCT              
         BZ    ATR185                                                           
         MVC   KEY+3(12),COSTNUM+3                                              
         B     ATR188                                                           
*                                                                               
ATR185   MVC   KEY+3(12),=12C'9'                                                
*                                                                               
ATR188   BAS   RE,GETACC           POSSIBLE 29 ACCTS: 29999                     
         BAS   RE,CHECKACC         (O=OFFICE)         29O9999                   
         MVC   CRPSNUM,ACCTNUM                        29OO9999                  
         MVC   CRPSNAME,ACCTNAME                                                
         B     ATR190                                                           
         EJECT                                                                  
*-------------------------------------------------------------                  
*        CHECK AMOUNT ON SCREEN                                                 
*-------------------------------------------------------------                  
*                                                                               
ATR190   L     R2,SAVCLPR                                                       
         ZIC   RF,0(R2)            POINT TO AMOUNT                              
         AR    R2,RF                                                            
         BAS   RE,ANY              MUST INPUT AMOUNT                            
         ZIC   R3,5(R2)                                                         
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         LA    RF,0(RF)                                                         
         ZAP   POSTCASH,0(8,RF)                                                 
         ZAP   TRANSAMT,POSTCASH                                                
*                                                                               
         ZIC   RF,0(R2)            OUTPUT DEBIT/CREDIT                          
         AR    R2,RF                                                            
         ST    R2,SAVENAM                                                       
         MVC   9(1,R2),POSTDC                                                   
*                                                                               
         CLI   CSACT,ACTINP                                                     
         BNE   *+12                                                             
         TM    BOINDS1,BOISCAND    TEST SCREEN SCANNED                          
         BZ    ATR300                                                           
*                                                                               
         L     R3,SAVER2           OUTPUT ACCOUNT NAME                          
         TM    4(R3),X'20'                                                      
         BO    ATR195                                                           
         OI    4(R3),X'20'                                                      
         MVC   12(31,R2),POSTACCN                                               
*                                                                               
ATR195   L     R3,SAVEODS          OUTPUT DEPT/STAFF NAME                       
         CLI   0(R3),1             NO INPUT IN DEPT/STAFF                       
         BNH   ATR200                                                           
         TM    4(R3),X'20'                                                      
         BO    ATR200                                                           
         OI    4(R3),X'20'                                                      
         MVC   46(15,R2),DEPSTFN                                                
*                                                                               
ATR200   L     R3,SAVCLPR          OUTPUT CLIENT/PRODUCT NAME                   
         CLI   0(R3),0                                                          
         BE    ATR210              NO INPUT IN CLI/PRD                          
         TM    4(R3),X'20'                                                      
         BO    ATR210                                                           
         OI    4(R3),X'20'                                                      
         MVC   63(12,R2),CLIPRON                                                
*                                                                               
ATR210   OI    6(R2),X'80'                                                      
*                                                                               
*        VALIDATE THE REST OF THE FIELDS ON THAT LINE                           
*                                                                               
         L     R2,SAVEDC           POINT TO DEBIT/CREDIT                        
         LA    R3,ATRDC2H-ATRDCH(R2)   POINT R3 AT BEG. OF NEXT LINE            
ATR220   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         OI    4(R2),X'20'         TURN ON VALIDATED BIT                        
         CR    R2,R3                                                            
         BL    ATR220                                                           
         LA    RF,DEBITS           UPDATE DEBIT/CREDIT TOTALS SO FAR            
         CLI   POSTDC,C'D'                                                      
         BE    *+8                                                              
         LA    RF,CREDITS                                                       
         AP    0(6,RF),POSTCASH                                                 
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD DESCRIPTION ELEMENT                                              
*--------------------------------------------------------------                 
*                                                                               
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         MVC   DLDSREF,SAVEDOC                                                  
         MVC   DLDSDATE,DATE3                                                   
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         OI    DLDSSTAT,X'08'                                                   
         XC    DLDSNARR,DLDSNARR                                                
         MVC   DLDSNARR(120),SAVENARR                                           
         LH    R6,LEN              LENGTH OF NARRATIVE STRING                   
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               LENGTH OF ELEMENT-NARRATIVE                  
         AR    R5,R6                                                            
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD POSTING ELEMENTS                                                 
*--------------------------------------------------------------                 
*                                                                               
         AR    R8,R5               BUMP TO NEXT ELEMENT                         
         USING DLPOSTD,R8                                                       
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVI   DLPSLEN,X'71'                                                    
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,POSTCASH                                                
         MVC   DLPSANAL,WRKCODE                                                 
         CLI   POSTDC,C'C'                                                      
         BE    ATR250                                                           
         MVC   DLPSDBAC(51),POSTACC        NUMBER AND NAME                      
         MVC   DLPSCRAC(51),POSTCAC        NUMBER AND NAME                      
         MVI   DLPSEL,X'69'          SINGLE DEBIT                               
         B     ATR260                                                           
*                                                                               
ATR250   MVC   DLPSCRAC(51),POSTACC        NUMBER AND NAME                      
         MVC   DLPSDBAC(51),POSTCAC        NUMBER AND NAME                      
         MVI   DLPSEL,X'6A'          SINGLE CREDIT                              
         EJECT                                                                  
*                                                                               
ATR260   CLC   POSTACC+1(2),=C'SE' IF ACCOUNT IS S/E                            
         BE    ATR260D                                                          
ATR260C  CLC   POSTACC+1(2),=C'GP' OR G/P                                       
         BNE   ATR260H                                                          
*                                                                               
ATR260D  CLI   POSTDC,C'C'         AND IT'S A CREDIT, THEN                      
         BNE   ATR260H                                                          
         MP    POSTCASH,=P'-1'     ANALYSIS POSTINGS S/B REVERSED SIGN          
*                                                                               
ATR260H  CLI   STFSW,C'Y'                                                       
         BNE   ATR262                                                           
         LR    RF,R8                                                            
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)       SECOND ELEMENT                               
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   TEMPSAV,0(RF)                                                    
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(L'TEMPSAV,R8),TEMPSAV                                          
*                                                                               
         MVI   DLPSEL,X'69'        DEBIT STAFF                                  
         MVC   DLPSDBAC,STAFFNUM                                                
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME                                                
         ZAP   DLPSAMNT,POSTCASH                                                
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         L     R5,SAVER2                                                        
         ZIC   R1,5(R5)            LENGTH OF ACCOUNT                            
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),10(R5)                                             
         CLI   TENO,X'F0'                                                       
         BL    ATR261                                                           
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,SAVER2                                                        
         MVI   ERRNUM,2                                                         
         LA    R6,1(R1)                                                         
         SR    R6,R4                                                            
         BM    ERROR                                                            
         LA    R3,10(R6,R2)                                                     
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3)                                              
         LR    R1,R4                                                            
*                                                                               
ATR261   STC   R1,BYTE                                                          
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3                                                
*                                                                               
         LR    RF,R8                                                            
         ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   TEMPSAV,0(RF)                                                    
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(L'TEMPSAV,R8),TEMPSAV                                          
*                                                                               
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVC   DLPSDBAC,DLPSCRAC                                                
         MVC   DLPSCRAC,CRPSNUM                                                 
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
*                                                                               
ATR262   DS    0H                                                               
         CLI   DEPSW,C'Y'                                                       
         BNE   ATR270                                                           
         ZIC   R3,DLPSLEN                                                       
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)       THIRD ELEMENT                                
         LA    R3,1(R3)                                                         
                                                                                
         MVC   TEMPSAV,0(RF)                                                    
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(L'TEMPSAV,R8),TEMPSAV                                          
                                                                                
         MVC   DLPSDBAC,DEPNUM                                                  
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM                                                 
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,POSTCASH                                                
         MVI   DLPSEL,X'68'                                                     
         OI    DLPSTYPE,X'80'      IN CASE WE MISSED IT ON ELEMENT 2            
*                                                                               
ATR270   DS    0H                                                               
         TM    COSTSW,OLDCOST                                                   
         BNO   ATR280                                                           
         ZIC   R3,DLPSLEN                                                       
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
                                                                                
         MVC   TEMPSAV,0(RF)                                                    
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
         MVC   0(L'TEMPSAV,R8),TEMPSAV                                          
                                                                                
         MVI   DLPSEL,X'69'        DEBIT DEPT C/A CLIENT                        
         MVC   DLPSCRAC,COSTNUM                                                 
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM                                                  
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,POSTCASH                                                
*                                                                               
ATR274   OI    DLPSTYPE,X'80'                                                   
         LA    R3,1(R3)            BACK TO DLPSLEN AGAIN                        
         LR    RF,R8                                                            
         AR    R8,R3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(RF)                                                    
                                                                                
         MVC   TEMPSAV,0(RF)                                                    
                                                                                
         BAS   RE,ADDANL                                                        
         BAS   RE,ADDADB                                                        
                                                                                
         MVC   0(L'TEMPSAV,R8),TEMPSAV                                          
                                                                                
         MVI   DLPSEL,X'6A'        CREDIT CLIENT                                
         MVC   DLPSDBAC,CR13NUM                                                 
         MVC   DLPSDBNM,CR13NAME                                                
*                                                                               
ATR280   ZIC   R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         LA    R4,IOAREA-1                                                      
         SR    R8,R4                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF      FINAL LENGTH                                 
         BAS   RE,PUTDAY           ADD RECORDS TO TRANSACTION FILE              
*--------------------------------------------------------------                 
*        BUILD TWA1 RECORD & PUT IT THERE                                       
*--------------------------------------------------------------                 
         XC    WORK,WORK                                                        
         MVC   WORK(6),SAVEDOC     REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         CLI   POSTDC,C'D'         SET DEBIT/CREDIT BIT FOR BATCH END           
         BNE   ATR290                                                           
         OI    WORK+10,X'80'                                                    
*                                                                               
ATR290   DS    0H                  PUT TWA1 RECORD BACK ON TEMPSTR              
         LA    R1,BOPARM                                                        
         LA    RE,IOAREA                                                        
         ST    RE,0(R1)                                                         
         LA    RE,BOPL61                                                        
         ST    RE,4(R1)                                                         
         LA    RE,BOWORK1                                                       
         ST    RE,8(R1)                                                         
         CLI   ATRINTH+5,0         ANY INTERCOMPANY OFFICE?                     
         BE    ATR295              NO                                           
*                                                                               
         MVI   0(R1),X'10'         YES, PASS IT TO ADDITE                       
         LA    RE,0                                                             
         ST    RE,12(R1)                                                        
         LA    RE,0                                                             
         ST    RE,16(R1)                                                        
         LA    RE,0                                                             
         ST    RE,20(R1)                                                        
         LA    RE,ATRINT                                                        
         ST    RE,24(R1)                                                        
*                                                                               
ATR295   GOTO1 AADDITE,(R1)                                                     
*                                                                               
         L     R2,SAVEDC           POINT TO DEBIT/CREDIT                        
         MVI   8(R2),C'*'          LINE HAS BEEN VALIDATED                      
         OI    6(R2),X'80'         TRANSMIT *                                   
         OI    4(R2),X'20'         TURN ON VALIDATED BIT                        
         XC    ATRTOT,ATRTOT       OUTPUT DEBIT/CREDITS SO FAR                  
         OI    ATRTOTH+6,X'80'                                                  
*                                                                               
ATR300   CLI   CSACT,ACTCHA        TEST ITEM/CHANGE                             
         BE    ATR500                                                           
*                                                                               
         CLI   CSOLINE,6           SEE IF SCREEN IS FILLED UP                   
         BE    ATR500                                                           
         L     R2,SAVENAM                                                       
         ZIC   RF,0(R2)            POINT TO NEXT FIELD                          
         AR    R2,RF                                                            
         CLI   5(R2),0             IF MORE, DO THE NEXT                         
         BNE   ATR8                                                             
*                                                                               
         CLI   CSACT,ACTINP        TEST INPUT                                   
         BNE   ATR310                                                           
         TM    BOINDS1,BOISCAND    TEST SCREEN SCANNED                          
         BO    ATR310                                                           
         OI    BOINDS1,BOISCAND    SET SCREEN SCANNED                           
         B     ATR002                                                           
ATR310   LA    R3,ATRTOTH          IF NO MORE, TURN ON PREVIOUSLY               
ATR390   OI    4(R2),X'20'         VALIDATED BIT FOR REST OF SCREEN             
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CR    R2,R3                                                            
         BL    ATR390                                                           
*                                                                               
ATR400   LA    R2,ATRDOCH          CURSOR TO DOCUMENT                           
         B     *+8                                                              
ATR500   LA    R2,ATRDCH           OTHERWISE, CURSOR TO DEBIT/CREDIT            
         CLI   CSACT,ACTINP        TEST INPUT                                   
         BNE   ATR502                                                           
         TM    BOINDS1,BOISCAND    TEST SCREEN SCANNED                          
         BO    ATR502                                                           
         OI    BOINDS1,BOISCAND    SET SCREEN SCANNED                           
         B     ATR002                                                           
ATR502   MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ERROR ROUTINE                                                          
*--------------------------------------------------------------                 
*                                                                               
*        R2=A(INVALID FIELD)                                                    
*        R3=A(ERROR MESSAGE)                                                    
*                                                                               
MYERR    DS    0H                                                               
         MVI   ERRNUM,X'FE'                                                     
         MVC   MSG,SPACES                                                       
         MVC   MSG(L'ERRMSG),0(R3)                                              
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LOOK AT 'SA' OR 'SB' LEDGER AND POSTINGS                               
*--------------------------------------------------------------                 
*                                                                               
CHECKSA  NTR1                                                                   
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),POSTACC+1                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         SR    R1,R1                                                            
*                                                                               
CHECK02  CLI   0(RF),0             SEE IF LEDGER IS OPEN ITEM                   
         BE    CHECKNO                                                          
         CLI   0(RF),X'14'                                                      
         BE    CHECK04                                                          
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     CHECK02                                                          
*                                                                               
         USING ACLEDGD,RF                                                       
CHECK04  CLI   ACLTLIKE,C'R'                                                    
         BNE   CHECKYS                                                          
         MVC   KEY(15),POSTACC                                                  
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
CHECK10  BAS   RE,SEQ                                                           
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CHECKNO                                                          
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'43'                                                      
         BNE   CHECK12                                                          
         USING TRSUBHD,RF          SAVE CONTRA-ACCOUNT DETAILS                  
         SR    R1,R1                                                            
         MVC   SANAM,SPACES                                                     
         MVC   SANUM,TRSBACNT                                                   
         IC    R1,TRSBLEN                                                       
         SH    R1,=AL2(TRSBNAME-TRSUBHD)                                        
         BZ    CHECK12                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     CHECK10                                                          
         MVC   SANAM(0),TRSBNAME                                                
*                                                                               
CHECK12  DS    0H                                                               
         CLI   0(RF),X'44'                                                      
         BNE   CHECK10                                                          
         USING TRANSD,RF                                                        
         TM    TRNSSTAT,X'80'      MUST BE DEBIT                                
         BZ    CHECK10                                                          
         CLC   DATE3,TRNSDATE      AND MATCH ON DATE                            
         BNE   CHECK10                                                          
         CLC   SAVEDOC,TRNSREF                                                  
         BE    CHECKYS                                                          
         B     CHECK10                                                          
*                                                                               
CHECKNO  MVI   ERRNUM,73                                                        
CHECKYS  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------                 
*        GOTO CATCALL TO GET 13 ACCT                                            
*--------------------------------------------------------------                 
GOCAT    NTR1                                                                   
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST                                    
         BNO   GOCATXIT                                                         
*                                                                               
GOCAT10  OI    COSTSW,NEWCOST                                                   
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         NI    COSTSW,X'FF'-OLDCOST                                             
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,EXPACC      DEBIT ACCOUNT                                
         MVC   CATOFF,AOFFC        OFFICE                                       
         MVC   CATDPT,DEPT         DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         OC    KEY,SPACES                                                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE                          
         CLI   CATERR,0                                                         
         BE    GOCAT12                                                          
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     GOCATXIT                                                         
*                                                                               
GOCAT12  CLI   CATPST,C'N'         NO COST POSTING                              
         BE    GOCATXIT                                                         
         MVC   COSTANAL,CATCDE                                                  
         MVC   CR13NUM,CATACC3     SAVE 13 ACCOUNT                              
         OI    COSTSW,OLDCOST                                                   
GOCATXIT B     XIT                                                              
*--------------------------------------------------------------                 
*        ERROR CHECK ROUTINE                                                    
*--------------------------------------------------------------                 
*                                                                               
ERRCHCK  NTR1                                                                   
         MVI   INPUTSW,0           'INPUT' INDICATES WHAT WAS ENTERED           
         MVI   ODSSW,0             CLEAR REQUIRED FLAG                          
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         CLC   DEPTL(1),DPTLNGTH                                                
         BNH   ERRB                                                             
         MVC   FVMSGNO,=Y(AE$INVDL)                                             
         B     EXIT                                                             
*                                                                               
ERRB     DS    0H                                                               
         CLI   STAFFL,8                                                         
         BNH   ERRC                                                             
         MVC   FVMSGNO,=Y(AE$INVSL)                                             
         B     EXIT                                                             
*                                                                               
ERRC     CLI   DEPSW,C'Y'          IS DEPT REQUIRED?                            
         BNE   ERRCH1                                                           
         CLI   DEPTL,0             WAS A DEPT INPUT?                            
         BNE   ERRCH1                                                           
*                                                                               
         MVC   KEY,SPACES          GET DEPT FROM DEBIT ACCNT                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),POSTACC+1                                               
         MVC   FVMSGNO,=Y(AE$UTRLR)                                             
         MVC   MSG+28(14),KEY+1                                                 
         BAS   RE,READ             GET LEDGER RECORD                            
         BNE   ERROR                                                            
*                                                                               
         L     R3,AIOAREA                                                       
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERRCH1              ENTER DPT IF NO 14 ELEM                      
*                                                                               
         USING ACLEDGD,R3                                                       
         CLI   ACLTLEN,X'20'       IF ELEMENT LENGTH IS NOT X'20'               
         BL    ERRCH1              OLD ELEMENT                                  
         CLI   ACLTDPOS,0          IF DISP TO DEPT CODE IS 0                    
         BE    ERRCH1              THEN DONT BOTHER LOOKING FOR DEPT            
         MVC   DEPTL(1),ACLTDLEN   SAVE LENGTH OF DEPT                          
         ZIC   R1,ACLTDLEN         LENGTH OF DEPT                               
         BCTR  R1,0                                                             
         LA    R4,POSTACC+2        SE OR GP ACCT NUM(EXCLUDING U/L)             
         ZIC   R0,ACLTDPOS                                                      
         AR    R4,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPT(0),0(R4)       GET DEPT CODE FROM DEBIT ACCT                
         OI    INPUTSW,DEPTBIT                                                  
         OI    ODSSW,DEPTBIT                                                    
*                                                                               
ERRCH1   DS    0H                                                               
         CLI   OFFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,OFFCBIT       IF OFFICE ENTERED THEN X'04' ON              
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,DEPTBIT       IF DEPT ENTERED THEN X'02' ON                
         CLI   STFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,STFFBIT       IF STAFF ENTERED THEN X'01' ON               
*                                                                               
ERRCH2   DS    0H                                                               
         CLI   STFSW,C'Y'          ONLY READ 2P IF STAFF IS ON                  
         BNE   ERRCH3                                                           
*                                                                               
ERRCH2A  MVC   KEY,SPACES          IF 2P NEEDS DEPT THEN IT'S NECESSARY         
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'                                                  
         BAS   RE,READ             READ LEDGER FOR STRUCTURE                    
         MVI   ELCODE,X'16'        HEIRARCHY ELEMENT                            
         L     R3,AIOAREA                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING HEIRARCHY ELEMENT                    
*                                                                               
*        1 LEVEL = STAFF                                                        
*        2 LEVEL = DEPT/STAFF                                                   
*        3 LEVEL = OFFICE/DEPT/STAFF                                            
*                                                                               
         USING ACHEIRD,R3                                                       
         MVI   LEVEL,3                                                          
         CLI   ACHRLEVC,12         3 LEVEL ACCOUNT?                             
         BE    ERRCH2C                                                          
*                                                                               
         MVI   LEVEL,2                                                          
         CLI   ACHRLEVB,12         2 LEVEL ACCOUNT?                             
         BE    ERRCH2C                                                          
*                                                                               
         MVI   LEVEL,1             1 LEVEL ACCOUNT                              
         CLI   ACHRLEVA,12                                                      
         BE    ERRCH2C                                                          
         DC    H'0'                BAD ACCOUNT LEVEL                            
*                                                                               
ERRCH2C  DS    0H                                                               
         CLI   LEVEL,1                                                          
         BE    ERRCH3                                                           
         OI    ODSSW,DEPTBIT       FOR 2 & 3 LEVEL STRUC. NEED DEPT             
*                                                                               
ERRCH3   DS    0H                  'ODSSW' HAS CORRECT BIT SETTINGS             
         CLC   OFFICE,SPACES        ENTERED IN THE OF/DP/ST FIELD.              
         BE    *+8                                                              
         OI    INPUTSW,OFFCBIT                                                  
         CLC   DEPT,SPACES                                                      
         BE    *+8                                                              
         OI    INPUTSW,DEPTBIT                                                  
         CLC   STAFF,SPACES                                                     
         BE    *+8                                                              
         OI    INPUTSW,STFFBIT                                                  
         XC    INPUTSW(1),ODSSW     IF ALL IS OK, INPUT SHOULD BE 0             
         CLI   OFFSW,C'Y'                                                       
         BE    *+8                                                              
         NI    INPUTSW,X'03'        ALLOW INPUT TO OFFICE IF OFF=N              
         CLI   INPUTSW,0                                                        
         BE    ERROK                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         L     R2,SAVEODS           POINT TO OFFICE/DPT/STFF FIELD              
         MVC   MSG,SPACES                                                       
         TM    ODSSW,OFFCBIT        SHOULD OFFICE BE ENTERED?                   
         BZ    ERRCH4                                                           
         MVC   FVMSGNO,=Y(AE$CFO)                                               
*                                                                               
ERRCH4   TM    ODSSW,DEPTBIT       SHOULD DEPT BE ENTERED?                      
         BZ    ERRCH5                                                           
         MVC   FVMSGNO,=Y(AE$CFOD)                                              
*                                                                               
ERRCH5   TM    ODSSW,STFFBIT       SHOULD STAFF BE ENTERED?                     
         BZ    ERRCH6                                                           
         MVC   FVMSGNO,=Y(AE$CFODS)                                             
         B     EXIT                                                             
*                                                                               
ERRCH6   CLI   ODSSW,0                                                          
         BNE   EXIT                                                             
         MVC   FVMSGNO,=Y(AE$INVIN)                                             
         B     EXIT                                                             
*                                                                               
ERROK    MVC   MSG,SPACES                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                RETURN TO CALLER                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SEE IF GETACC HAS PUT AN XJOB IN ACCTNUM                               
*        RETURNS ERRNUM SET TO 45 IF AN XJOB                                    
*--------------------------------------------------------------                 
CHKXJOB  NTR1                                                                   
         MVI   ERRNUM,0                                                         
*                                                                               
         CLC   PRODUL,ACCTNUM+1    PRODUCTION LEDGER?                           
         BNE   CHKXX                                                            
*                                                                               
         LA    R3,ACCTNUM+3                                                     
         ZIC   R1,PRDLNGTH                                                      
         LR    R0,R1                                                            
         LA    R3,0(R1,R3)                                                      
         IC    R1,JOBLNGTH                                                      
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BNP   CHKXX               NO JOBS !!!!                                 
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES      IS THIS A JOB?                               
         BNH   CHKXX               NO                                           
*                                                                               
         LA    R3,ACCTNUM                                                       
         GOTO1 ASETJOB,DMCB,(R3)                                                
         TM    ACOPSTAT,ACOXJOB                                                 
         BNO   CHKXX                                                            
         MVI   ERRNUM,45                                                        
CHKXX    B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------                 
*        TEST LOGICAL EXCLUSIONS                                                
*--------------------------------------------------------------                 
*                                                                               
TESTIT   NTR1                                                                   
         LA    RF,DLIST                                                         
         CLI   POSTDC,C'D'                                                      
         BE    *+8                                                              
         LA    RF,CLIST                                                         
*                                                                               
TEST2    CLI   0(RF),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(2,RF),POSTACC+1                                                
         BE    TESTNO                                                           
         LA    RF,2(RF)                                                         
         B     TEST2                                                            
*                                                                               
TESTNO   MVI   HALF,C'N'                                                        
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* MOVE DATA TO DISPLAY DATA                                          *          
**********************************************************************          
MOVEFLD  ST    RE,SAVERE                                                        
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),FLD                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD ANOELM - ANALYSIS OFFICE ELEMENT                                   
*--------------------------------------------------------------                 
ADDANL   CLI   ANOELM,0            TEST OFFICE ELEMENT                          
         BER   RE                                                               
         ST    RF,FULL                                                          
         LA    R1,ANOELM                                                        
         USING ANOELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,ANOLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),ANOEL                                                    
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         L     RF,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD FREEFORM ELEMENT FOR CLIENT AND PRODUCT                            
*--------------------------------------------------------------                 
ADDADB   CLI   FFTELEM,0           DO WE HAVE A CLIENT AND PRODUCT?             
         BER   RE                  NO, DONE                                     
         ST    RF,FULL                                                          
         LA    R1,FFTELEM                                                       
         USING FFTELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),FFTELEM                                                  
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         L     RF,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
*             NO DEBITS TO SP,SS,SV,SX,SN,SU,SI,SK,SQ,ST,SW,SY                  
*                          11,12,13,14,15,16,27,28,29                           
DLIST    DS    0H                                                               
         DC    C'SPSSSVSXSNSUSISKSQSTSWSY111213141516272829'                    
         DC    X'FF'                                                            
*                                                                               
*        NO CREDITS TO SI,SJ,SK,13,14,15,16,1P,2C,2D,2P                         
*                                                                               
CLIST    DS    0H                                                               
         DC    C'SISJSK131415161P2C2D2P'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATCODE                                                              
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
BOISCAND EQU   X'80'               SCREEN HAS BEEN SCANNED                      
         EJECT                                                                  
***********************************************************************         
* DATA DICTIONARY                                                     *         
***********************************************************************         
         SPACE 1                                                                
DDIN1    DS    0C                                                               
         DCDDL AC#CRSF,15                                                       
         DCDDL AC#DRSF,14                                                       
         DC    X'00'                                                            
DDIN2    DCDDL AC#OFF,3                                                         
         DCDDL AC#DPT,4                                                         
         DCDDL AC#STFC,5                                                        
         DCDDL AC#CLIC,3                                                        
         DCDDL AC#PRO,3                                                         
         DC    X'00'                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ERROR MESSAGE TABLE                                                    
*--------------------------------------------------------------                 
*                                                                               
ERRMSG   DS    0CL42                                                            
MISSCLI  DC    CL42'ERROR-INPUT TO CLIENT/PRODUCT REQUIRED'                     
INVALCLI DC    CL42'ERROR-ACCOUNT NOT FLAGGED FOR ANALYSIS'                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SCREENS                                                                
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATCFD                                                       
*                                                                               
         ORG   TWAHOLE                                                          
SAVECAC  DS    CL15                CONTRA ACCOUNT                               
SAVECACN DS    CL36                                                             
DEBITS   DS    PL6                 TOTAL DEBITS SO FAR                          
CREDITS  DS    PL6                 TOTAL CREDITS SO FAR                         
PASSSW   DS    CL1                                                              
EXPACC   DS    CL15                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DSECT FOR LOCAL WORKING STORAGE                                        
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
POSTACC  DS    CL15                                                             
POSTACCN DS    CL36                                                             
POSTCAC  DS    CL15                                                             
POSTCACN DS    CL36                                                             
POSTDC   DS    CL1                                                              
POSTCASH DS    PL7                                                              
CINUM    DS    CL15                1/2                                          
CINAME   DS    CL36                                                             
CLIPRO   DS    CL15                                                             
CLIPRON  DS    CL36                NAME OF CLI/PRO FOR OUTPUT                   
COSTNUM  DS    CL15                1/C                                          
COSTNAME DS    CL36                                                             
CR13NUM  DS    CL15                1/3                                          
CR13NAME DS    CL36                                                             
CRPSNUM  DS    CL15                2/9                                          
CRPSNAME DS    CL36                                                             
CRDSNUM  DS    CL15                2/8                                          
CRDSNAME DS    CL36                                                             
CRCNUM   DS    CL15                1/P                                          
CRCNAME  DS    CL36                                                             
DEPNUM   DS    CL15                2/D                                          
DEPNAME  DS    CL36                                                             
DEPSTFN  DS    CL36                NAME OF OFF/DEPT/STAFF FOR OUTPUT            
SANUM    DS    CL15                S/A CONTRA ACCOUNT DETAILS                   
SANAM    DS    CL36                                                             
SAVCLPR  DS    F                   ADDRESS OF CLIENT/PRODUCT FIELD              
SAVER2   DS    F                   ADDRESS OF R2                                
SAVERE   DS    F                   ADDRESS OF RE                                
SAVEDC   DS    F                   ADDRESS OF DEBIT/CREDIT FIELD                
SAVENAM  DS    F                   ADDRESS OF OUTPUT OF NAMES FIELD             
SAVEDOC  DS    CL6                 DOCUMENT NUMBER                              
SAVENARR DS    CL120                                                            
SAVEODS  DS    F                   ADDRESS OF OFF/DEPT/STAFF FIELD              
STAFFNUM DS    CL15                2/P                                          
STAFFNAM DS    CL36                                                             
ACTV     DS    CL1                                                              
BLOCK    DS    6CL32                                                            
BYTE     DS    CL1                                                              
COSTBYTE DS    CL1                                                              
DATE3    DS    CL3                                                              
FLAGDRCR DS    CL1                                                              
LEN      DS    H                                                                
PRELOC   DS    F                                                                
*                                                                               
SWITCHES DS    0H                                                               
AOFCBIT  EQU   X'08'               OFFICE BIT                                   
OFFCBIT  EQU   X'04'               OFFICE BIT                                   
DEPTBIT  EQU   X'02'               DEPARTMENT BIT                               
STFFBIT  EQU   X'01'               STAFF BIT                                    
OFFSW    DS    CL1                                                              
DEPSW    DS    CL1                                                              
STFSW    DS    CL1                                                              
*                                                                               
SBYTE    DS    CL1                 SECURITY BYTE                                
INTOFF   EQU   1                   FLD SEC #-INTEROFFICE OFFICE                 
*                                                                               
**T                                                                             
COSTSW   DS    CL1                                                              
OLDCOST  EQU   X'01'                                                            
NEWCOST  EQU   X'02'                                                            
NOTFRST  EQU   X'01'                                                            
**T                                                                             
ODSSW    DS    XL1                 OFF/DPT/STAFF THAT IS REQUIRED               
INPUTSW  DS    XL1                 BIT REPRESENTATION OF OFF/DPT/STAFF          
*                                  THAT THEY ENTERED                            
WRKCODE  DS    CL2                 WORKCODE IS SAME FIELD AS OFFICE             
WRKCODEL DS    XL1                                                              
         ORG   WRKCODE                                                          
OFFICE   DS    CL2                                                              
OFFICEL  DS    XL1                 LENGTH OF OFFICE ENTERED ON SCREEN           
DEPT     DS    CL4                                                              
DEPTL    DS    XL1                    "   "  DEPT      "    "     "             
STAFF    DS    CL6                                                              
STAFFL   DS    XL1                    "   "  STAFF     "    "     "             
AOFFC    DS    CL6                                                              
AOFFCL   DS    XL1                    "   "  STAFF     "    "     "             
*                                                                               
COSTANAL DS    CL5                                                              
CATBLK   DS    CL(CATLNQ)                                                       
ANOELM   DS    XL(ANOELQ)                                                       
FFTELEM  DS    CL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA)                                 
                                                                                
SCAN     DS    4CL32                                                            
LEVEL    DS    XL1                 TEMP HOLDER FOR LEVEL OF ACCT                
ELCODE   DS    XL1                 USED FOR GETEL ROUTINE                       
TEMPSAV  DS    CL113                                                            
DDOUT1   DS    0C                                                               
AC@CRSF  DS    CL(L'ACTKCULA)                                                   
AC@DRSF  DS    CL14                                                             
DDOUT2   DS    0C                                                               
AC@OFF   DS    CL3                                                              
AC@DPT   DS    CL4                                                              
AC@STFC  DS    CL5                                                              
AC@CLIC  DS    CL3                                                              
AC@PRO   DS    CL3                                                              
KEY      DS    CL49                                                             
IOAREA   DS    CL2000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        OTHER INCLUDES                                                         
*--------------------------------------------------------------                 
*                                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
* ACCDDEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACBAT2D   05/12/14'                                      
         END                                                                    
