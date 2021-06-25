*          DATA SET SPSFM39    AT LEVEL 071 AS OF 01/16/15                      
*PHASE T21739A                                                                  
T21739   TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING'                          
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* JUN01/11 068 AKAT - MIDAS=M/C FILTER                                *         
* OCT14/10 067 AKAT - LOCK=Y/N FILTER                                 *         
* JUN03/09 066 AKAT - NEW SHOW=B(BOTH)/D(DEACTIVATED) FILTER          *         
* JAN08/09 065 AKAT - SUPPORT 2 CHARACTER BOOKTYPE FILTER             *         
* NOV24/08 064 AKAT - NEW BK=? FILTER TO FILTER ON NO BOOK TYPE       *         
* AUG07/07 061 AKAT - FIX MCHO'S BUG FROM LAST VERSION AND USE AIO3   *         
*                   - TO READ ALPHA MKT FROM CTFILE SINCE THE REC     *         
*                   - CAN BE > 48 BYTES & WE WERE USING KEY (48 BYTES)*         
* JAN25/07 060 MCHO - ADDED OPTION TO LIST BY C'K' KEYS (KMKT=1521)   *         
* OCT16/05 059 AKAT - SUPPORT 2 CHARACTER OFFICE CODES                *         
* APR12/00 051 SEAN - ADDED CODES TO INTERRUPT READING IF 90% OF I/O  *         
*                     MAX IS REACHED.                                 *         
*                                                                     *         
* NOV22/99 007 SEAN - ADDED OPTION TO LIST RECORDS BY EIX=(Y/N)       *         
*                                                                     *         
* OCT25/99 007 SEAN - TOOK OUT ALL CODES THAT LISTS BY -N- TYPE REC   *         
*                                                                     *         
* OCT23/97 005 GLEE - CLEAR MYTEXT AREA IN VALKEY ROUTINE             *         
*                                                                     *         
* Sep22/97 004 GLEE - Change files in VALKEY routine                  *         
*                                                                     *         
* Aug25/97 001 GLEE - Revamped program for efficiency                 *         
*                   - Added new filters to mimic same functionality   *         
*                      as the late SPOT/INFO program                  *         
***********************************************************************         
         EJECT                                                                  
SFM39    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21739**,R7,RR=RE,CLEAR=YES                                    
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         USING OFFICED,OFCBLK                                                   
*                                                                               
         ST    RE,RELO                                                          
         ST    RB,MYBASE1                                                       
         ST    R7,MYBASE2                                                       
*                                                                               
         BAS   RE,MYINIT           INITIALIZATION                               
*                                                                               
         DS    0H                  CHECK GENCON MODES                           
         CLI   MODE,SETFILE                                                     
         BNE   *+12                                                             
         BAS   RE,SFIL                                                          
         B     XIT                                                              
                                                                                
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,LISTRECS                                                    
         BE    LREC                                                             
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+16                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         B     LREC                                                             
                                                                                
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*========================= MY INITIALIZATION =========================*         
         DS    0H                                                               
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         L     R2,=A(DISPTAB-SFM39)                                             
         LA    R2,SFM39(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,SFM39            RE = BASE OF TABLE/ROUTINE                   
         ZICM  RF,2(R2),(3)                                                     
         BZ    *+12                                                             
         LA    RF,GEND(RF)                                                      
         L     RE,0(RF)                                                         
         AR    R1,RE               R1 = A(TABLE OR ROUTINE)                     
         ZICM  RF,4(R2),(3)                                                     
         A     RF,ASYSD            RF-->PLACE TO STORE ADDRESS                  
         ST    R1,0(RF)                                                         
         LA    R2,L'DISPTAB(R2)                                                 
         BCT   R0,MI10                                                          
                                                                                
*                                                                               
MI40     DS    0H                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
         MVI   ACTELOPT,C'N'       DON'T MONITOR ACTIVITY                       
         MVC   AIO,AIO1                                                         
         OI    CONSERVH+6,X80+X01  PREVENT "NO DATA RECEIVED" MSG               
*                                                                               
         DS    0H                  TRANSLATE DATA DICT TERMS                    
         XC    DMCB(6*4),DMCB                                                   
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL                                                   
         MVI   DDRETN,DDCASEL                                                   
         MVI   DDRETN,C'M'         (NO IDEA WHY THIS YIELDS LOWER CASE)         
         MVI   DDSYS,8                                                          
         MVI   DDLANG,C' '                                                      
         L     RE,=A(DCLIST-SFM39)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
                                                                                
*                                                                               
         XC    ACURFORC,ACURFORC                                                
                                                                                
*                                                                               
         MVI   ZEROES,C'0'                                                      
         MVC   ZEROES+1(L'ZEROES-1),ZEROES                                      
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SETFILE)'                
***********************************************************************         
*========================== SET FILE ROUTINE =========================*         
SFIL     NTR1                                                                   
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   SYSDIR,=C'STATION ' SET FILENAME.                                
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,MKTKEYLN         SET KEYLENGTH.                               
         STH   R1,LKEY                                                          
*                                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (VALKEY)'                 
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VKEY     DS    0H                                                               
                                                                                
         XC    OPTR,OPTR           NO OPTIONS REQUIRED                          
         XC    OPTX,OPTX            NOR DIS-ALLOWED                             
                                                                                
         MVI   MYTEXT,0            INITIALIZE WORK AREA FOR TEXT STUFF          
         MVC   MYTEXT+1(MYTEXTL-1),SPACES                                       
*                                                                               
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   LKEY,=H'13'                                                      
         MVI   USEIO,C'N'                                                       
                                                                                
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,MSLMEDIH         CHECK FOR ANY MEDIA INPUT                    
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    MSLUIDCH+6,X'80'                                                 
         OI    MSLUIDDH+6,X'80'                                                 
***      CLI   QMED,C'R'                                                        
***      BE    VKM010                                                           
         B     VKM010                    UID FOR TV TOO !                       
         OI    MSLUIDCH+1,X'0C'          LOW INTENSITY TO HIDE                  
         OI    MSLUIDDH+1,X'0C'                                                 
         B     VKO000                                                           
*                                                                               
VKM010   NI    MSLUIDCH+1,X'FF'-X'04'    HIGH INTENSITY TO SHOW                 
         NI    MSLUIDDH+1,X'FF'-X'04'                                           
*                                                                               
*-------------------------- VALIDATE OPTIONS -------------------------*         
*                                                                               
VKO000   LA    R2,MSLOPTNH         CHECK FOR OPTIONS INPUTTED                   
*                                                                               
         CLC   =C'BBM',8(R2)       SPECIAL BBM STA DISPLAY REQUEST              
         BNE   VKO010                                                           
         CLI   MSLFMT,C'F'         TEST IT SAYS FORMAT                          
         BNE   VKO045X                                                          
         MVC   MSLFMT,=C'BBM   '                                                
         OI    MSLFMTH+6,X'80'     SET TO XMT                                   
         MVC   MSLUIDC,=C'ALPHA '                                               
         OI    MSLUIDCH+6,X'80'     SET TO XMT                                  
         B     VKO045X                                                          
*                                                                               
VKO010   CLI   MSLFMT,C'F'         TEST FORMAT FIELD SAYS FORMAT                
         BE    VKO010X                                                          
         MVC   MSLFMT,=C'FORMAT'                                                
         OI    MSLFMTH+6,X'80'     SET TO XMT                                   
         MVC   MSLUIDC,=C'UNIQID'                                               
         OI    MSLUIDCH+6,X'80'     SET TO XMT                                  
*                                                                               
VKO010X  BAS   RE,VALOPT                                                        
         BE    VKO029                                                           
*                                                                               
         DS    0H                  ERROR ENCOUNTERED IN OPTN VALIDATION         
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)             RF-->TIOB                                   
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC    SET CURSOR ONTO FIELD                       
         LR    R0,R2                                                            
         S     R0,ATWA                                                          
         STH   R0,TIOBCURD          SET DISPLACEMENT OF FIELD                   
         MVC   TIOBCURI,FLDDSPL     SET DISPLACEMENT INTO FIELD                 
         DROP  RF                                                               
                                                                                
         B     OURERROR                                                         
VKO029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CHECK MAX FILTERS ALLOWED                    
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),=AL4(ALLFILTS)                                            
         BZ    VKO045X              SKIP IF NO FILTERS ENTERRED                 
         L     RE,DUB                                                           
         SRDL  RE,32                RF CONTAINS INPUTTED FILTERS                
         LA    R0,1                 MAX # OF FILTERS ALLOWED                    
                                                                                
VKO045B  DS    0H                                                               
         SLDL  RE,1                 SHIFT ONE FLAG OVER TO RE                   
         SR    R0,RE                                                            
         BM    MANYFLT              IF NEG, # FILT INPUT > # FILT ALLOW         
         OR    RF,RF                ANY MORE FILTERS ENTERRED?                  
         BNZ   VKO045B               YEP                                        
VKO045X  EQU   *                                                                
                                                                                
*                                                                               
*------------------------------ STATION ------------------------------*         
*                                                                               
         DS    0H                                                               
         LA    R2,MSLSTATH         STATION FIELD                                
         MVC   QSTANEW,SPACES                                                   
         CLI   5(R2),0             CHECK FOR INPUT IN FIELD                     
         BE    VKS099               NO INPUT                                    
                                                                                
         CLI   OPVFRMNM,0          IGNORE STATION WHEN LISTING                  
         BE    VKS012               BY FORMAT RECORDS.                          
         MVC   MSLSTAT,SPACES                                                   
         OI    MSLSTATH+6,X'80'                                                 
         B     VKS099              GO BUILD KEY.                                
                                                                                
*                                                                               
VKS012   DS    0H                 VALIDATE INPUT                                
         TM    4(R2),X'08'         IF INPUT IS NUMERIC,                         
         BO    VKS030                                                           
         CLI   5(R2),4              OR L(INPUT) > 4,                            
         BH    VKS030                LET STAVAL VALIDATE IT                     
*                                                                               
         DS    0H                  VALIDATE INPUT MYSELF                        
         SR    R3,R3                GET L(INPUT)                                
         ICM   R3,1,5(R2)                                                       
         BZ    VKS024X                                                          
                                                                                
         STC   R3,BYTE                                                          
         BCTR  R3,0                                                             
         EXMVC R3,WORK,8(R2)                                                    
         MVI   GOSUBN,CALF#        VALIDATE FOR ALPHABETICS                     
         GOTO1 AGOSUB                                                           
         BNE   INVLSTA                                                          
                                                                                
         EXMVC R3,QSTANEW,8(R2)                                                 
VKS024X  EQU   *                                                                
         B     VKS099               NOW GO AND BUILD KEY                        
                                                                                
*                                                                               
VKS030   DS    0H                  CALL STAVAL TO VALIDATE INPUT                
         LA    R3,BLOCK                                                         
         USING STABLKD,R3                                                       
         XC    0(STBLNQ,R3),0(R3)                                               
         ST    R2,STBADDR           A(STATION INPUT)                            
         MVC   STBMED,QMED          MEDIA                                       
         MVI   STBCTRY,C'U'         COUNTRY                                     
         CLI   SVAPROF+7,C'C'        TEST CANADA                                
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'          IF TRUE, CHANGE TO CANADA                  
         MVC   STBACOM,ACOMFACS                                                 
         GOTO1 STAVAL,DMCB,(R3)                                                 
         CLI   STBERR,0                                                         
         BNE   INVLSTA                                                          
         MVC   QSTANEW,STBSTA      QSTANEW NOW HAS START-AT VALUE               
         DROP  R3                                                               
                                                                                
         CLI   QSTANEW,C'0'        IF CABLE,                                    
         BL    *+14                                                             
         MVC   8(4,R2),QSTANEW      REFORMAT INPUT FOR DISPLAY                  
         OI    6(R2),X80                                                        
                                                                                
*                                                                               
VKS099   EQU   *                                                                
         B     VK100               GO BUILD KEY                                 
         EJECT                                                                  
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VK100    DS    0H                 BUILD KEYS                                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         CLI   OPVKMKNM,0          LIST BY K KEYS?                              
         BNE   VK140                - YES WE ARE                                
*                                                                               
         CLI   OPVFRMNM,0          LIST BY FORMAT RECORDS?                      
         BNE   VK130                                                            
*                                                                               
         DS    0H                  BUILD KEY OF MSTR RECORD (TYPE -'S')         
         USING STARECD,R6                                                       
         MVI   STAKTYPE,STAKTYPQ    FORMAT RECORDS ARE TYPE-'S'                 
         MVC   STAKMED,QMED         MEDIA                                       
         MVC   STAKCALL,QSTANEW     CALL LETTERS                                
         MVC   STAKAGY,AGENCY       AGENCY                                      
         DROP  R6                                                               
         B     VKX                                                              
                                                                                
*                                                                               
VK130    DS    0H                  BUILD KEY OF FRMT RECORD (TYPE -'F')         
         USING STARECD,R6                                                       
         MVI   STFKTYPE,C'F'        FORMAT RECORDS ARE TYPE-'F'                 
         MVC   STFKAGY,AGENCY       AGENCY                                      
         MVC   STFKMED,QMED         MEDIA                                       
         MVC   STFKFORM,OPVFRMTB    FORMAT                                      
         DROP  R6                                                               
         B     VKX                                                              
                                                                                
*                                                                               
VK140    DS    0H                  BUILD KEY OF K KEY (TYPE ='K')               
         USING STKKTYPE,R6                                                      
         MVI   STKKTYPE,C'K'                                                    
         MVC   STKKAGY,AGENCY      AGENCY                                       
         MVC   STKKMED,QMED        MEDIA                                        
         MVC   STKKMKT,BFLTMKT     BINARY MARKET                                
         MVC   STKKSTA,QSTANEW     STATION                                      
         DROP  R6                                                               
         B     VKX                                                              
                                                                                
*---------------------------- VALKEY EXITS ---------------------------*         
*                                                                               
VKX      DS    0H                                                               
         MVC   MYSVKEY(STAKEYLN),KEY   SAVE KEY AROUND                          
*                                                                               
         BAS   RE,SFIL                 GO AND SET FILES                         
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,TSL#             TEST SELECT FIELDS                       
         GOTO1 AGOSUB                                                           
         BE    *+12                                                             
         MVI   OURERRCD,NCMCNQ                                                  
         B     OURERROR                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------- VALIDATE OPTIONS -------------------------*         
                                                                                
* At entry,                                                                     
*   R2-->options field.                                                         
* On a clean exit,                                                              
*   CC set to equal.                                                            
* On an error exit,                                                             
*   CC set to not equal,                                                        
*   MYTEXT(1) = l(text replace in error msg) or 0,                              
*   MYTEXT+1  = text replace if MYTEXT<>0,                                      
*   FLDDSPL   = displacement to sub-field w/ error,                             
*   OURERRCD  = error code.                                                     
                                                                                
         DS    0H                                                               
VALOPT   NTR1                                                                   
         MVI   FLDDSPL,0                                                        
         MVI   GOSUBN,IOV#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOPT200                                                          
*                                                                               
         MVI   OURERRCD,IFLDQ                                                   
         MVI   SCANLNTH,10                                                      
         MVI   BYTE,MAX#OPTQ       MAX # OPTIONS                                
         OI    BYTE,X80            RETURN DISPLACEMENT OF FIELDS                
         GOTO1 SCANNER,DMCB,(SCANLNTH,(R2)),(BYTE,BUFF),=C',=,='                
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VOPTXN              ERROR                                        
                                                                                
         STC   R0,NOPTN            STORE # OF OPTIONS INPUTTED                  
         MVI   OURERRCD,TMODQ                                                   
         CLM   R0,1,=AL1(MAX#OPTQ) CHECK AGAINST MAX # OPTNS ALLOWED            
         BH    VOPTXN                                                           
*                                                                               
         MVI   COUNTER,0                                                        
         LA    R3,BUFF             R3-->SCANNER BLOCK                           
*                                                                               
** VALIDATE OPTION KEYWORD **                                                   
*                                                                               
VOPT020  DS    0H                  RUN THRU OPTION KEYWORDS TABLE               
         L     R4,AOPTTAB                                                       
         USING OPTDSECT,R4                                                      
                                                                                
         MVC   FLDDSPL,4(R3)       HOLD ONTO DISPLACEMENT INTO FIELD            
                                                                                
VOPT022  DS    0H                                                               
         CLI   0(R4),EOT           IF END OF OPTION KEYWORD TABLE,              
         BNE   VOPT024                                                          
         MVI   OURERRCD,INVOQ                                                   
         B     VOPTXN               THEN INPUT IS INVALID                       
                                                                                
VOPT024  DS    0H                                                               
         ZICM  RF,OPDKYTB,(3)                                                   
         LA    RF,OPTDSECT(RF)     RF-->TABLE OF NAMES FOR ENTRY                
                                                                                
VOPT030  DS    0H                                                               
         CLI   0(RF),EOT           IF AT END OF NAME TABLE,                     
         BE    VOPT036              IT'S NOT THIS OPTION ENTRY                  
         LLC   R1,0(RF)                                                         
         LLC   RE,0(R3)            RE = LENGTH OF INPUTTED KEYWORD              
         CR    R1,RE               L(KYWD IN TABLE) VS L(KYWD INPUTTED)         
         BL    VOPT033              LOW: TRY ANOTHER NAME                       
         BCTR  RE,0                                                             
         EXCLC RE,12(R3),1(RF)     IF THEY MATCH,                               
         BE    VOPT040              THEN THIS IS THE OPTION ENTRY               
                                                                                
VOPT033  DS    0H                                                               
         LA    RF,1(R1,RF)         ELSE, CHECK NEXT NAME IN TABLE               
         B     VOPT030                                                          
                                                                                
VOPT036  DS    0H                  BUMP TO NEXT OPTION ENTRY                    
         LLC   R1,OPDNTRYL                                                      
         AR    R4,R1                                                            
         B     VOPT022                                                          
*                                                                               
VOPT040  DS    0H                  OPTION ENTRY LOCATED                         
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),OPDOPTB       IS THIS OPTION DUPLICATED?                  
         BZ    VOPT043                                                          
         MVI   OURERRCD,DUPOQ        YES, DUPLICATED OPTION KEYWORD             
         B     VOPTXN                                                           
                                                                                
VOPT043  DS    0H                                                               
         MVC   DUB(4),OPTX                                                      
         NC    DUB(4),OPDOPTB       IS THIS OPTION COMPATIBLE?                  
         BZ    VOPT046                                                          
         MVI   OURERRCD,IOCBQ        NO, INVALID OPTN COMBO                     
         B     VOPTXN                                                           
         EJECT                                                                  
*                                                                               
** INDIVIDUAL OPTIONS' VALIDATIONS **                                           
*                                                                               
                                                                                
VOPT046  DS    0H                                                               
         MVI   MYTEXT,0                                                         
         MVC   MYTEXT+1(MYTEXTL-1),SPACES                                       
                                                                                
                                                                                
VOFMT    DS    0H                  FORMAT                                       
         CLI   OPDNUMB,OPNFRM                                                   
         BNE   VOFMTX                                                           
*                                                                               
         MVI   OURERRCD,MOTCQ                                                   
         CLI   QMED,C'R'           FORMAT RECORDS ONLY ALLOWED                  
         BNE   VOPTXN10             FOR MEDIA 'R'                               
*                                                                               
VOFMTX   EQU   *                                                                
                                                                                
                                                                                
         ST    R4,AOPTNTRY                                                      
         OC    OPTR,OPDRQOPT       OTHER OPTIONS REQUIRED                       
         OC    OPTX,OPDNAOPT       OTHER OPTIONS NOT ALLOWED                    
         B     VOPT050                                                          
         EJECT                                                                  
*                                                                               
** VALIDATE OPTION VALUE **                                                     
*                                                                               
VOPT050  DS    0H                                                               
         MVC   FLDDSPL,8(R3)       HOLD ONTO DISPLACEMENT INTO FIELD            
                                                                                
         TM    OPDFLAG,OPDFKYWD                                                 
         BO    VOPT060                                                          
                                                                                
         CLI   1(R3),0             THERE S/B DATA INPUT TO VALIDATE             
         BH    *+12                                                             
         MVI   OURERRCD,MODQ                                                    
         B     VOPTXN                                                           
                                                                                
         TM    OPDFLAG,OPDFVRTN                                                 
         BO    VOPT080                                                          
         TM    OPDFLAG,OPDFVTBL                                                 
         BO    VOPT100                                                          
         DC    H'0'                                                             
*                                                                               
*** OPTION VALUE VALID VIA KEYWORD ***                                          
*                                                                               
VOPT060  DS    0H                                                               
         CLI   1(R3),0             SHOULD NOT HAVE OPTION DATA                  
         BE    *+12                                                             
         MVI   OURERRCD,OSKQ                                                    
         B     VOPTXN                                                           
*                                                                               
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->OUTPUT AREA FOR OPTION DATA             
         LLC   R1,0(R5)            R1 = # OF VALUES FOR OPTION SO FAR           
         CLM   R1,1,OPDMXNDV       DO WE HAVE MAX AMOUNT ALREADY                
         BL    VOPT062                                                          
         MVI   OURERRCD,TMODQ       YES, TOO MANY OPTN DATA ERROR               
         B     VOPTXN                                                           
                                                                                
VOPT062  DS    0H                                                               
         LA    R0,1(R1)                                                         
         STC   R0,0(R5)            UPDATE # OF VALUES FOR OPTION                
         LLC   RF,OPDOLEN          RF = L(OPTION OUTPUT)                        
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         LA    R0,1(R1,R5)         SET DESTINATION ADDRESS                      
         LR    R1,RF               SET DESTINATION LENGTH                       
*                                                                               
         ZICM  R5,OPDVLTB,(3)                                                   
         LA    R5,OPTDSECT(R5)     R5-->VALUE(S) FOR KEYWORD                    
         LA    RE,1(R5)            SET SOURCE ADDRESS                           
         LLC   RF,0(R5)            SET SOURCE LENGTH                            
                                                                                
         MVCL  R0,RE               USE MVCL IN CASE L'DEST <> L'SOURCE          
                                                                                
         B     VOPT150                                                          
*                                                                               
*** OPTION VALUE VALIDATED VIA ROUTINE ***                                      
*                                                                               
VOPT080  DS    0H                                                               
         LA    R0,BLOCK                                                         
         LA    R1,480              L'BLOCK                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR  BLOCK  FOR OUTPUT                     
*                                                                               
         MVC   GOSUBN,OPDRTNUM                                                  
         GOTO1 AGOSUB                                                           
         BNE   VOPTXN10            (ROUTINE SHOULD SET ALL ERROR INFO)          
*                                                                               
         LLC   R0,OPDOLEN          R0 = OUTPUT LENGTH                           
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->OUTPUT AREA FOR OPTION DATA             
         LLC   RE,0(R5)            RE = # OF OUTPUT VALUES SO FAR               
         STH   R0,HALF                                                          
         MH    RE,HALF                                                          
         LA    RE,1(RE,R5)         RE-->DESTINATION FOR OPTION VALUE            
         LA    RF,BLOCK            RF-->OPTION VALUES RETURNED FROM RTN         
                                                                                
VOPT083  DS    0H                                                               
         LR    R1,R0               R1 = OUTPUT LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),0(RF)       ANY MORE OUTPUT?                             
         BZ    VOPT090              NOPE                                        
                                                                                
         CLC   0(1,R5),OPDMXNDV    DO WE HAVE MAX # DATA VALUE ALREADY?         
         BL    VOPT086                                                          
         MVI   OURERRCD,TMODQ       YES, TOO MANY OPTN DATA ERROR               
         B     VOPTXN                                                           
                                                                                
VOPT086  DS    0H                                                               
         EXMVC R1,0(RE),0(RF)      MOVE OPTION DATA TO STORAGE                  
         AR    RE,R0               BUMP TO NEXT STORAGE LOCATION                
         AR    RF,R0               BUMP TO NEXT DATA VALUE SOURCE               
         LLC   R1,0(R5)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R5)            UPDATE # OF DATA VALUES                      
         B     VOPT083                                                          
*                                                                               
VOPT090  DS    0H                                                               
         B     VOPT150                                                          
*                                                                               
*** OPTION VALUE VALIDATED VIA TABLE ***                                        
*                                                                               
VOPT100  DS    0H                                                               
         ZICM  R2,OPDTBDSP,(3)                                                  
         A     R2,MYBASE1          R2-->TABLE OF VALID OPTION VALUES            
*                                                                               
         SR    R1,R1                                                            
VOPT112  DS    0H                                                               
         ZICM  R0,0(R2),(1)        IF L(TABLE DATA) = 0,                        
         BNZ   VOPT114                                                          
         MVI   OURERRCD,IODVQ       EOT REACHED AND INPUT IS INVALID            
         B     VOPTXN                                                           
                                                                                
VOPT114  DS    0H                                                               
         IC    R1,1(R3)            R1 = L(INPUT DATA)                           
         CR    R0,R1               L(TABLE DATA) VS L(INPUT DATA)               
         BL    VOPT116              LOW: INSUFFICIENT FOR COMPARISON            
         BCTR  R1,0                                                             
         EXCLC R1,22(R3),2(R2)                                                  
         BE    VOPT120                                                          
                                                                                
VOPT116  DS    0H                  BUMP TO NEXT TABLE ENTRY                     
         LLC   RF,1(R2)             RF = L(VALUE TO STORE)                      
         AR    RF,R0                R0 = L(TABLE DATA)                          
         LA    R2,2(RF,R2)          R2 BUMPED TO NEXT TABLE ENTRY               
         B     VOPT112                                                          
*                                                                               
VOPT120  DS    0H                  OPTION DATA INPUTTED IS VALID                
         LR    RF,R0                RF = L(TABLE DATA)                          
         LA    RF,2(RF,R2)          RF-->VALUE TO STORE FOR OPTION              
                                                                                
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->PLACE TO STORE OPTION VALUE             
         CLC   0(1,R5),OPDMXNDV    DO WE HAVE MAX # DATA VALUE ALREADY?         
         BL    VOPT123                                                          
         MVI   OURERRCD,TMODQ                                                   
         B     VOPTXN                                                           
                                                                                
VOPT123  DS    0H                                                               
         LLC   RE,0(R5)            RE = # OF DATA VALUES SO FAR                 
         LA    R1,1(RE)                                                         
         STC   R1,0(R5)             UPDATE IT                                   
         LLC   R1,OPDOLEN          R1 = L(OPTION DATA VALUE)                    
         STH   R1,HALF                                                          
         MH    RE,HALF                                                          
         LA    RE,1(RE,R5)         RE-->DESTINATION FOR OPTION VALUE            
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(RF)                                                   
*                                                                               
         B     VOPT150                                                          
*                                                                               
** OPTION INPUTTED IS VALID **                                                  
*                                                                               
VOPT150  DS    0H                                                               
         OC    OPTI,OPDOPTB        REMEMBER THAT IT WAS INPUTTED                
                                                                                
         LLC   R1,COUNTER          ANY MORE SCANNER ENTRIES?                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,NOPTN                                                       
         BNL   VOPT200              NOPE, FINISHED W/ OPTIONS                   
         STC   R1,COUNTER                                                       
         LLC   R1,SCANLNTH                                                      
         LA    R3,22(R3,R1)         YEP, BUMP TO NEXT SCANNER ENTRY             
         B     VOPT020                                                          
*                                                                               
VOPT200  DS    0H                                                               
         MVC   DUB(4),OPTR         TEST ALL REQUIRED OPTIONS INPUTTED           
         NC    DUB(4),OPTI                                                      
         CLC   DUB(4),OPTR                                                      
         BE    VOPT220              YEP, THEY'RE ALL HERE                       
                                                                                
         L     R4,AOPTTAB          LOCATE MISSING OPTION                        
VOPT212  DS    0H                                                               
         CLI   0(R4),EOT           IF END OF OPTIONS TABLE REACHED,             
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S AMISS                           
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),OPTR                                                      
         XC    DUB(4),OPTR         DUB(4) CONTAINS OPTIONS MISSING              
         NC    DUB(4),OPDOPTB      IS THIS AN OPTION MISSING?                   
         BNZ   VOPT214              YES                                         
         LLC   R0,OPDNTRYL          NO, TRY NEXT ENTRY                          
         AR    R4,R0                                                            
         B     VOPT212                                                          
                                                                                
VOPT214  DS    0H                  MOVE KEYWORD LEN & NAME INTO MYTEXT          
         ZICM  RF,OPDKYTB,(3)                                                   
         LA    RF,OPTDSECT(RF)                                                  
         LLC   RE,0(RF)                                                         
         EXMVC RE,MYTEXT,0(RF)                                                  
         MVI   OURERRCD,ROMQ                                                    
         B     VOPTXN10                                                         
*                                                                               
VOPT220  DS    0H                                                               
         B     VOPTXY                                                           
         DROP  R4                                                               
                                                                                
                                                                                
VOPTXN   DS    0H                                                               
         MVI   MYTEXT,0            NO TEXT REPLACE W/IN ERROR MESSAGE           
                                                                                
VOPTXN10 DS    0H                                                               
         B     NO                                                               
*                                                                               
VOPTXY   DS    0H                                                               
         B     YES                                                              
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (VALIDATION ERROR+        
               S)'                                                              
***********************************************************************         
*========================= VALIDATION ERRORS =========================*         
                                                                                
VKERROR  DS    0H                  (ERROR CODE MUST BE SET)                     
         B     OURERROR                                                         
                                                                                
MISSFLD  DS    0H                  MISSING INPUT FIELD ERROR                    
         MVI   OURERRCD,MFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLSTA  DS    0H                  INVALID STATION                              
         MVI   OURERRCD,ISTAQ                                                   
         B     OURERROR                                                         
                                                                                
MANYFLT  DS    0H                  TOO MANY FILTERS IN FIELD                    
         MVI   OURERRCD,TMFQ                                                    
         B     OURERROR                                                         
                                                                                
ERRIO    DS    0H                  TOO MANY FILTERS IN FIELD                    
         MVI   OURERRCD,IOMAXQ                                                  
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (LISTRECS)'               
***********************************************************************         
*============================ LIST RECORD ============================*         
LREC     DS    0H                                                               
         BAS   RE,SFIL                                                          
         OC    SVKEY1,SVKEY1       DID WE LAST LEAVE OFF ON MAX I/O ?           
         BNO   *+16                                                             
         MVC   KEY,SVKEY1                                                       
         XC    SVKEY1,SVKEY1       CLEAR IT                                     
*                                                                               
         OC    KEY(STAKEYLN),KEY   FIRST TIME THROUGH                           
         BNZ   LRRDHI              NO, READ WHERE LEFT OFF                      
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR                
                                                                                
*                                                                               
LRRDHI   DS    0H                                                               
         MVI   DMINBTS,0           RESET                                        
         GOTO1 HIGH                                                             
                                                                                
         L     R6,AIO                                                           
         CLI   OPVKMKNM,0          LIST BY K KEYS?                              
         BNE   LK00                 - YES WE ARE                                
*                                                                               
*                                                                               
         CLI   OPVFRMNM,0          LIST BY FORMAT RECORDS                       
         BNE   LFR                  YES                                         
*                                                                               
** LIST BY STATION MASTER RECORDS **                                            
*                                                                               
LR10     DS    0H                                                               
         USING STARECD,R6                  R6=AIO-->'S'-RECORD                  
*                                                                               
LR11     CLC   KEY(STAKCALL-STARECD),MYSVKEY    IS 1ST TWO BYTES                
         BNE   LRX                     WHAT WE'RE LOOKING FOR                   
*                                      NO, EXIT LISTING                         
         CLC   KEY+(STAKAGY-STARECD)(L'STAKAGY),MYSVKEY+(STAKAGY-STAREC+        
               D)                                                               
         BNE   LR100               AGENCY DOESN'T MATCH...NEXT RECORD           
*                                                                               
         CLI   OPVNMKNM,0          ARE WE FILTERING ON MKT                      
         BE    *+14                                                             
         CLC   SMKT,FLTMKT         MARKET WE WANT?                              
         BNE   LR100               DID NOT PASS FILTER, NEXT RECORD             
*                                                                               
         BAS   RE,LRSTAFLT         RUN RECORD THROUGH FILTER                    
         BNE   LR100               DID NOT PASS FILTER, NEXT RECORD             
*                                                                               
LR15     MVC   DFDEACTV,SSYSDACT           DEACTIVATED RECORD?                  
         MVC   DFMEDIA,STAKMED             MEDIA                                
         MVC   DFSTTN,SPACES                                                    
         MVC   DFSTTN(L'STAKCALL),STAKCALL STATION CALL-LETTERS                 
         MVC   DFCLNT,SPACES               ASSUME BLANK CLIENT CODE             
         CLC   STAKCLT,ZEROES                                                   
         BE    *+10                                                             
         MVC   DFCLNT,STAKCLT              NON-BLANK CLIENT CODE                
         MVC   DFMRKT,SMKT                 NUMERIC MARKET CODE                  
         MVC   DFFORMAT,SFORMAT            FORMAT                               
*                                                                               
         MVC   DFUNIQID,SPACES                                                  
         CLC   DFCLNT,SPACES                                                    
         BH    LR20                                                             
         CLC   STAKLEN,=AL2(STANCLNQ)      RECORD BIG ENOUGH?                   
         BL    LR20                        NO, CAN'T HAVE UNIQUE ID             
         MVC   DFUNIQID,STUNIQID           UNIQUE ID                            
*                                                                               
LR20     CLI   MSLFMT,C'B'         TEST DISPLAYING BBM CODES                    
         BNE   LR90                                                             
*                                                                               
         XC    DFFORMAT,DFFORMAT                                                
         MVC   DFFORMAT(4),SRS2CALL   DISPLAY BBM CODE                          
         XC    DFUNIQID,DFUNIQID                                                
         MVC   DFUNIQID(3),SMKTALPH                                             
         CLC   SMKT,=C'0000'                                                    
         BE    LR90                                                             
         CLI   SMKTALPH,C' '                                                    
         BH    *+10                                                             
         MVC   DFUNIQID(3),=C'***'                                              
         DROP  R6                                                               
*                                                                               
LR90     MVI   GOSUBN,GMKTI#       GET MARKET NAME                              
         GOTO1 AGOSUB                                                           
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         MVI   GOSUBN,PTINF#       PUT INFO                                     
         GOTO1 AGOSUB                                                           
*                                                                               
LR100    DS    0H                                                               
         MVI   DMINBTS,0                                                        
         GOTO1 SEQ                                                              
*                                                                               
         CLC   =C'SOON',CONWHEN    SKIP IO CHECK IF RUNNING SOON                
         BE    LR10                                                             
         CLC   =C'OV',CONWHEN      OR OVERNIGHT                                 
         BE    LR10                                                             
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         ICM   R5,3,FATMAXIO       MAXIO COUNT IS "SOFT"                        
         MHI   R5,9                                                             
         D     R4,=F'10'                                                        
         CLM   R5,3,FATIOCNT       COMPARE IT TO CURRENT COUNT OF IO'S          
         BH    LR10                                                             
         DROP  R1                                                               
*                                                                               
         MVC   SVKEY1,KEY          START FROM WHERE WE ARE LEAVING OFF          
         LA    R2,MSLSELH                                                       
         B     ERRIO                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*-------------------------- TYPE-'F' RECORDS -------------------------*         
*                                                                               
LFR      DS    0H                                                               
         CLC   KEY(STFKMS-STAKEY),MYSVKEY        IS 1ST 8 BYTES                 
         BNE   LRX                           WHAT WE'RE LOOKING FOR?            
*                                                                               
         USING STARECD,R6                    R6=AIO-->'F'-RECORD.               
         MVC   DFMEDIA,STFKMED               KEEP MEDIA.                        
         MVC   DFFORMAT,STFKFORM             KEEP FORMAT.                       
         MVC   DFSTTN,SPACES                                                    
         GOTO1 MSUNPK,DMCB,STFKMS,DFMRKT,DFSTTN KEEP MARKET & STATION.          
         CLI   DFSTTN+4,C' '       CHECK AM, FM, OR TV.                         
         BNE   *+8                  DFSTTN+4=' ' => TV.                         
         MVI   DFSTTN+4,C'T'                                                    
         GOTO1 CLUNPK,DMCB,STFKCLT,DFCLNT    KEEP CLIENT.                       
         DROP  R6                                                               
         MVI   GOSUBN,GMKTI#       GET MARKET NAME                              
         GOTO1 AGOSUB                                                           
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         MVI   GOSUBN,PTINF#       PUT INFO                                     
         GOTO1 AGOSUB                                                           
         MVI   DMINBTS,0                                                        
         GOTO1 SEQ                                                              
         B     LFR                                                              
         EJECT                                                                  
*                                                                               
*-------------------------- TYPE-'F' RECORDS -------------------------*         
*                                                                               
LK00     DS    0H                                                               
         CLC   KEY(STKKSTA-STKKTYPE),MYSVKEY   IS 1ST 6 BYTES                   
         BNE   LRX                           WHAT WE'RE LOOKING FOR?            
*                                                                               
         USING STKKTYPE,R6                   R6=AIO-->'K'-KEY.                  
         MVC   DFMEDIA,STKKMED     MEDIA                                        
         MVC   DFSTTN,SPACES                                                    
         MVC   DFSTTN(L'STKKSTA),STKKSTA   STATION CALL LETTERS                 
         GOTO1 CLUNPK,DMCB,STFKCLT,DFCLNT    KEEP CLIENT.                       
         MVC   DFCLNT,SPACES               ASSUME BLANK CLIENT CODE             
         CLC   STKKCLT,ZEROES                                                   
         BE    *+10                                                             
         MVC   DFCLNT,STKKCLT              NON-BLANK CLIENT CODE                
         MVC   DFMRKT,FLTMKT                                                    
***      MVC   DFFORMAT,SFORMAT    FORMAT                                       
         DROP  R6                                                               
         MVI   GOSUBN,GMKTI#       GET MARKET NAME                              
         GOTO1 AGOSUB                                                           
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS                                                    
         BE    *+8                                                              
         LA    R2,P                                                             
         MVI   GOSUBN,PTINF#       PUT INFO                                     
         GOTO1 AGOSUB                                                           
         MVI   DMINBTS,0                                                        
         GOTO1 SEQ                                                              
         B     LK00                                                             
         EJECT                                                                  
*                                                                               
*------------------------- LIST RECORDS EXIT -------------------------*         
*                                                                               
LRX      DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   XLRX                                                             
         MVC   P+9(7),=C'RECORDS'                                               
         EDIT  LISTCNTR,(7,P+1),COMMAS=YES                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
XLRX     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*-------------------- STATION MASTER RECORD FILTER -------------------*         
                                                                                
* At entry,                                                                     
*   R6-->station master record                                                  
* At exit,                                                                      
*   CC set to equal if record passed filter test                                
*   CC set to not equal otherwise                                               
                                                                                
LRSTAFLT NTR1                                                                   
         USING STARECD,R6                                                       
*                                                                               
         DS    0H                  FILTER ON PAYREP                             
         CLI   OPVPAYNM,0                                                       
         BE    LRSFPAYX                                                         
         CLC   SPAYREP,OPVPAYTB                                                 
         BNE   LRSFXN                                                           
LRSFPAYX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON CLIENT                             
         CLI   OPVCLINM,0                                                       
         BE    LRSFCLIX                                                         
         CLC   STAKCLT,OPVCLITB                                                 
         BNE   LRSFXN                                                           
LRSFCLIX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON TIME SHEET                         
         CLI   OPVTIMNM,0                                                       
         BE    LRSFTIMX                                                         
         CLC   SCONREP,OPVTIMTB                                                 
         BNE   LRSFXN                                                           
LRSFTIMX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON AFFILIATION                        
         CLI   OPVAFFNM,0                                                       
         BE    LRSFAFFX                                                         
         CLI   SVAPROF+7,C'C'                                                   
         BNE   LRSFAFF2                                                         
         CLI   SCANNTWK+3,X'40'    ONLY 3 BYTES                                 
         BNE   LRSFAFF1                                                         
         CLC   SCANNTWK(3),OPVAFFTB                                             
         BE    LRSFAFFX                                                         
         B     LRSFXN                                                           
LRSFAFF1 CLC   SCANNTWK,OPVAFFTB                                                
         BE    LRSFAFFX                                                         
         B     LRSFXN                                                           
LRSFAFF2 CLC   SNETWRK(3),OPVAFFTB                                              
         BNE   LRSFXN                                                           
LRSFAFFX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON STATION TYPE                       
         CLI   OPVTYPNM,0                                                       
         BE    LRSFTYPX                                                         
         CLC   STYPE,OPVTYPTB                                                   
         BNE   LRSFXN                                                           
LRSFTYPX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON EIX                                
         CLI   OPVEIXNM,0                                                       
         BE    LRSFEIXX                                                         
         CLC   SEIXSTA,OPVEIXTB                                                 
         BE    LRSFEIXX                                                         
         CLI   SEIXSTA,0           MOST MASTER REC'S HAVE X'00' FOR EIX         
         BNE   LRSFXN                                                           
         CLI   OPVEIXTB,C'Y'                                                    
         BE    LRSFXN                                                           
LRSFEIXX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON BOOKTYPE                           
*                                                                               
         CLI   OPVBKTNM,0          HAVE BOOKTYPE FILTER?                        
         BE    LRSFBKTX            NO                                           
         CLI   OPVBKTTB,C'?'       FILTER ON NO BOOKTYPE?                       
         BNE   LRSFBK10            NO                                           
         CLI   SBKTYPE,0           YES - BOOKTYPE ON MASTER REC?                
         BNE   LRSFXN              YES - FILTER THIS REC OUT!                   
         B     LRSFBKTX                                                         
*                                                                               
LRSFBK10 CLC   SBKTYPE,OPVBKTTB                                                 
         BNE   LRSFXN                                                           
LRSFBKTX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON UNIQUE ID                          
         CLI   OPVUIDNM,0                                                       
         BE    LRSFUIDX                                                         
         CLC   STAKLEN,=AL2(STANCLNQ)      RECORD BIG ENOUGH?                   
         BL    LRSFXN                      NO, CAN'T HAVE UNIQUE ID             
         CLC   STAKCLT,=C'000'             CAN'T BE CLT SPECIFIC                
         BL    LRSFXN                                                           
         CLC   STUNIQID,OPVUIDTB                                                
         BNE   LRSFXN                                                           
LRSFUIDX EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON BAND=                              
         CLI   OPVBNDNM,0          ANY FILTER?                                  
         BE    LRSFMIDX            NO                                           
         CLC   OPVBNDTB,STAKCALL+4 MATCH ON BAND?                               
         BNE   LRSFXN              NO - FILTER OUT                              
*                                                                               
LRSFBNDX EQU   *                   END OF BAND FILTER LOGIC                     
*                                                                               
         DS    0H                  FILTER ON MIDAS=M/C                          
         CLI   OPVMIDNM,0          ANY FILTER?                                  
         BE    LRSFMIDX            NO                                           
         CLC   OPVMIDTB,STMIDAS    MATCH ON MIDAS?                              
         BNE   LRSFXN              NO - FILTER OUT                              
*                                                                               
LRSFMIDX EQU   *                   END OF LOCK=Y/N LOGIC                        
*                                                                               
         DS    0H                  FILTER ON LOCK=Y/N                           
         CLI   OPVLOCNM,0          ANY FILTER?                                  
         BE    LRSFLOCX            NO                                           
         CLI   OPVLOCTB,C'N'       WANT LOCK=NO ONLY?                           
         BNE   LRSFLOC5            NO                                           
         TM    SFLAG1,SLOCK        RECORD LOCKED?                               
         BNZ   LRSFXN              YES - FILTER OUT                             
         B     LRSFLOCX            DONE WITH THIS FILTER                        
*                                                                               
LRSFLOC5 TM    SFLAG1,SLOCK        RECORD LOCKED?                               
         BZ    LRSFXN              NO - FILTER OUT                              
LRSFLOCX EQU   *                   END OF LOCK=Y/N LOGIC                        
*                                                                               
         DS    0H                  FILTER ON SHOW=B/D                           
         CLI   OPVSHWNM,0          ANY FILTER?                                  
         BE    LRSFSHW1            NO                                           
         CLI   OPVSHWTB,C'B'       BOTH ACTIVATED AND DEACTIVATED?              
         BE    LRSFSHWX            YES                                          
         CLI   SSYSDACT,X'FF'      DEACTIVATED RECORD?                          
         BNE   LRSFXN              NO - FILTER OUT!                             
         B     LRSFXY                                                           
*                                                                               
LRSFSHW1 CLI   SSYSDACT,X'FF'      DEACTIVATED RECORD?                          
         BE    LRSFXN              YES - FILTER OUT!                            
LRSFSHWX EQU   *                                                                
         B     LRSFXY              RECORD PASSED FILTER TEST                    
         DROP  R6                                                               
*                                                                               
** EXITS **                                                                     
*                                                                               
LRSFXN   DS    0H                                                               
         B     NO                                                               
LRSFXY   DS    0H                                                               
         B     YES                                                              
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (MISCELLANEOUS)'          
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
                                                                                
         DS    0H                                                               
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
         L     R7,MYBASE2                                                       
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  DS    0H                                                               
         GOTO1 ASUBRTN,(RC)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
OURERROR DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         B     XMSGGO                                                           
                                                                                
OURWARN  DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         B     XMSGGO                                                           
                                                                                
OURINFO  DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
                                                                                
                                                                                
*---------------------------- REPORT SPECS ---------------------------*         
                                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
                                                                                
         SSPEC H1,34,C'MASTER LIST'                                             
         SSPEC H2,34,C'-----------'                                             
                                                                                
         SSPEC H4,2,C'STATION'                                                  
         SSPEC H4,13,C'CLIENT'                                                  
         SSPEC H4,22,C'MKTCODE'                                                 
         SSPEC H4,32,C'MARKET NAME'                                             
         SSPEC H4,60,C'FORMAT'                                                  
                                                                                
         SSPEC H5,2,C'-------'                                                  
         SSPEC H5,13,C'------'                                                  
         SSPEC H5,22,C'-------'                                                 
         SSPEC H5,32,C'-----------'                                             
         SSPEC H5,60,C'------'                                                  
                                                                                
         DC    X'00'                                                            
                                                                                
                                                                                
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (LTORG && CONSTAN+        
               TS)'                                                             
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
MAINL    EQU   *-SFM39                                                          
         DS    0CL(X'2000'-MAINL+1)                                             
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01)'                 
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   SFM39+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-SFM39+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   SFM39+SUBR01Q                                                    
SUBR01   NMOD1 0,**3901**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R9             R9=A(SYSD)                                   
         USING SPOOLD,R8           R8=A(SPOOL WORK AREA)                        
                                                                                
         LLC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
CALF#    EQU   (R01_01-*)/4+1      CHECK ALPHABETICS                            
CNUM#    EQU   (R01_02-*)/4+1      CHECK NUMERICS                               
CAN#     EQU   (R01_03-*)/4+1      CHECK ALPHANUMERICS                          
IOV#     EQU   (R01_04-*)/4+1      INITIALIZE OPTIONS VALUES                    
VMKT#    EQU   (R01_05-*)/4+1      VALIDATE MARKET (OPTIONS FIELD)              
TSL#     EQU   (R01_06-*)/4+1      TEST SELECT FIELD                            
GMSTI#   EQU   (R01_07-*)/4+1      GET STATION MASTER INFO                      
GMKTI#   EQU   (R01_08-*)/4+1      GET MARKET INFO                              
PTINF#   EQU   (R01_09-*)/4+1      PUT INFO                                     
VFMT#    EQU   (R01_10-*)/4+1      VALIDATE FORMAT (OPTIONS FIELD)              
VPAY#    EQU   (R01_11-*)/4+1      VALIDATE PAYREP (OPTIONS FIELD)              
VCLI#    EQU   (R01_12-*)/4+1      VALIDATE CLIENT (OPTIONS FIELD)              
VTIM#    EQU   (R01_13-*)/4+1      VALIDATE TIME SHEET (OPTIONS FIELD)          
VAFF#    EQU   (R01_14-*)/4+1      VALIDATE AFFILIATION (OPTIONS FIELD)         
VTYP#    EQU   (R01_15-*)/4+1      VALIDATE STTN TYPE (OPTIONS FIELD)           
VEIX#    EQU   (R01_16-*)/4+1      VALIDATE EIX=(Y/N)                           
VBKT#    EQU   (R01_17-*)/4+1      VALIDATE BKT=                                
VUID#    EQU   (R01_18-*)/4+1      VALIDATE UID=                                
VSHW#    EQU   (R01_19-*)/4+1      VALIDATE SHOW=                               
VLOC#    EQU   (R01_20-*)/4+1      VALIDATE LOCK=                               
VMID#    EQU   (R01_21-*)/4+1      VALIDATE MIDAS=                              
VBND#    EQU   (R01_22-*)/4+1      VALIDATE BAND=                               
                                                                                
R01_00   DS    0H                                                               
R01_01   B     CHKALPH             CHECK ALPHABETICS                            
R01_02   B     CHKNUMRC            CHECK NUMERICS                               
R01_03   B     CHKALPH             CHECK ALPHANUMERICS                          
R01_04   B     INITOPTV            INITIALIZE OPTIONS VALUES                    
R01_05   B     VALMRKT             VALIDATE MARKET (OPTIONS FIELD)              
R01_06   B     TESTSEL             TEST SELECT FIELD                            
R01_07   B     GETMASTI            GET STATION MASTER INFO                      
R01_08   B     GETMRKTI            GET MARKET INFO                              
R01_09   B     PUTINFO             PUT INFO                                     
R01_10   B     VALFRMT             VALIDATE FORMAT (OPTIONS FIELD)              
R01_11   B     VALPAY              VALIDATE PAYREP (OPTIONS FIELD)              
R01_12   B     VALCLI              VALIDATE CLIENT (OPTIONS FIELD)              
R01_13   B     VALTIM              VALIDATE TIME SHEET (OPTIONS FIELD)          
R01_14   B     VALAFF              VALIDATE AFFILIATION (OPTIONS FIELD)         
R01_15   B     VALTYP              VALIDATE STTN TYPE (OPTIONS FIELD)           
R01_16   B     VALEIX              VALIDATE EIX=Y/N                             
R01_17   B     VALBKT              VALIDATE BKT=                                
R01_18   B     VALUID              VALIDATE UID=                                
R01_19   B     VALSHW              VALIDATE SHOW=                               
R01_20   B     VALLOC              VALIDATE LOCK=                               
R01_21   B     VALMID              VALIDATE MIDAS=                              
R01_22   B     VALBND              VALIDATE BAND=                               
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--CALF#, C+        
               NUM#, && CAN#)'                                                  
*-------------------------- CHECK ALPHABETIC -------------------------*         
                                                                                
* At entry,                                                                     
*   BYTE = L(specimen)                                                          
*   WORK contains specimen                                                      
                                                                                
CHKALPH  DS    0H                                                               
         LLC   R0,BYTE                                                          
         LA    RF,WORK                                                          
*                                                                               
CALF010  DS    0H                                                               
         CLI   0(RF),C'A'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'I'                                                       
         BNH   CALF020                                                          
                                                                                
         CLI   0(RF),C'J'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'R'                                                       
         BNH   CALF020                                                          
                                                                                
         CLI   0(RF),C'S'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'Z'                                                       
         BH    CALF030                                                          
*                                                                               
CALF020  DS    0H                  THIS CHARACTER IS AN ALPHABET                
         LA    RF,1(RF)                                                         
         BCT   R0,CALF010                                                       
         B     CALFXY                                                           
*                                                                               
CALF030  DS    0H                  NON-ALPHABETIC CHARACTER FOUND               
         CLI   GOSUBN,CAN#          IF TO CHECK ALPHANUMERICS,                  
         BE    CHKNUMRC              GO CHECK NUMERICS NOW                      
         B     CALFXN               ELSE, IT'S AN ERROR                         
                                                                                
*                                                                               
CALFXY   DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CALFXN   DS    0H                                                               
         B     NO_01                                                            
         EJECT                                                                  
*--------------------------- CHECK NUMERICS --------------------------*         
                                                                                
* At entry,                                                                     
*   BYTE = L(specimen)                                                          
*   WORK contains specimen                                                      
                                                                                
CHKNUMRC DS    0H                                                               
         LLC   R0,BYTE                                                          
         LA    RF,WORK                                                          
*                                                                               
CNUM010  DS    0H                                                               
         CLI   0(RF),C'0'                                                       
         BL    CNUMXN                                                           
         CLI   0(RF),C'9'                                                       
         BH    CNUMXN                                                           
*                                                                               
         DS    0H                  THIS CHARACTER IS A NUMERIC                  
         LA    RF,1(RF)                                                         
         BCT   R0,CNUM010                                                       
         B     CNUMXY                                                           
                                                                                
*                                                                               
CNUMXY   DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CNUMXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--IOV#)'           
*---------------------- INITIALIZE OPTION VALUES ---------------------*         
                                                                                
INITOPTV DS    0H                                                               
         MVI   NOPTN,0             CLEAR # OF OPTIONS                           
         XC    OPTI,OPTI           CLEAR OPTIONS INPUTTED                       
*                                                                               
         LA    R0,OPTVALS                                                       
         LA    R1,OPTVALSQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VMKT#)'          
*-------------------------- VALIDATE MARKET --------------------------*         
                                                                                
* Validates an inputted market code inputted in the options field.              
* At entry,                                                                     
*   R3-->SCANNER entry w/ market code in 2nd half,                              
*   R4-->entry of option keyword for market.                                    
* On exit when market code valid,                                               
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when market code invalid,                                             
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALMRKT  DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
         XC    FLTMKT,FLTMKT       CLEAR SAVED MARKET FILTER                    
*                                                                               
         TM    3(R3),X80           TEST FOR NUMERIC MARKET INPUTTED             
         BNZ   VMKTN010                                                         
         CLI   OPVKMKNM,0          LIST BY K KEYS?                              
         BNE   *+12                 - YES, GO TO ERROR MESSAGE                  
         TM    3(R3),X40           TEST FOR ALPHA MARKET INPUTTED               
         BNZ   VMKTA010                                                         
         MVI   OURERRCD,IMKTQ                                                   
         B     VMKTXN                                                           
*                                                                               
** NUMERIC MARKET INPUTTED **                                                   
*                                                                               
VMKTN010 DS    0H                                                               
         MVI   OURERRCD,MODQ                                                    
         CLI   1(R3),1             CHECK L(MARKET CODE INPUTTED)                
         BL    VMKTXN                                                           
         MVI   OURERRCD,IMKTQ      ASSUME INVALID MARKET CODE INPUTTED          
         CLI   1(R3),4                                                          
         BH    VMKTXN                                                           
*                                                                               
         LLC   RE,1(R3)            RE=L(MARKET CODE INPUTTED)                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R3)                                                     
         CVB   R1,DUB                                                           
***   SAVE OFF THE BINARY MARKET FOR K KEY FILTERS                              
         STCM  R1,3,BFLTMKT        BINARY FILTER MARKET                         
***                                                                             
         EDIT  (R1),(L'OPVNMKTB,BLOCK),FILL=0  LEFT-PAD MKT-CODE W/ 0'S         
         MVC   FLTMKT,BLOCK        SAVE MARKET FILTER                           
         B     VMKTXY                                                           
*                                                                               
** ALPHA MARKET INPUTTED **                                                     
*                                                                               
VMKTA010 DS    0H                                                               
         MVI   OURERRCD,IMKTQ      ASSUME INVALID MARKET CODE INPUTTED          
         CLI   1(R3),2              CHECK L(MARKET CODE INPUTTED)               
         BL    VMKTXN                                                           
         CLI   1(R3),3                                                          
         BH    VMKTXN                                                           
*                                                                               
*** VALIDATE ALPHA MARKET AGAINST FILE ***                                      
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R6,KEYSAVE                                                       
         USING CTDMREC,R6                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       USE MEDIA 'T'                                
         CLI   QMED,C'C'            IF INPUTTED MEDIA IS 'C'                    
         BE    VMKTA024                                                         
         CLI   QMED,C'N'            IF INPUTTED MEDIA IS 'N'                    
         BE    VMKTA024                                                         
         MVC   CTDMKMED,QMED       ELSE, USE INPUTTED MEDIA                     
*                                                                               
VMKTA024 MVC   CTDMKMKT,22(R3)     ALPHA MARKET                                 
*                                                                               
         MVI   CTDMKSRC,C'N'       TRY NIELSEN FIRST                            
         L     R5,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEYSAVE,AIO3               
         CLC   CTDMKEY(CKYMKTL),0(R5)                                           
         BE    VMKTA029                                                         
*                                                                               
         MVI   CTDMKSRC,C'A'       TRY ARBITRON NEXT                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEYSAVE,AIO3               
         CLC   CTDMKEY(CKYMKTL),0(R5)                                           
         BNE   VMKTXN                                                           
         DROP  R6                                                               
*                                                                               
*** GET NUMERIC MARKET FROM ALPHA MARKET ***                                    
*                                                                               
VMKTA029 XC    KEYSAVE,KEYSAVE                                                  
         LA    R6,KEYSAVE                                                       
         USING ANMRECD,R6                                                       
         MVI   ANMKTYPE,ANMKTYPQ                                                
         MVC   ANMKAGCY,AGENCY                                                  
         MVC   ANMKMED,QMED                                                     
         MVC   ANMKAMRK,22(R3)                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'STATION ',KEYSAVE,KEY           
         CLC   ANMKEYD(AKYAMKL),KEY                                             
         BNE   VMKTXN                                                           
         MVC   BLOCK(L'OPVNMKTB),KEY+(ANMKNMRK-ANMKEYD)                         
         MVC   FLTMKT,BLOCK        SAVE MARKET FILTER                           
         DROP  R6                                                               
*                                                                               
** VALIDATE MARKET EXITS **                                                     
*                                                                               
VMKTXY   DS    0H                                                               
         B     YES_01                                                           
VMKTXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--TSL#)'           
*------------------------- TEST SELECT FIELDS ------------------------*         
                                                                                
* TESTS THE SELECT FIELD, AND DOESN'T ALLOW CHANGES TO                          
*  BE MADE TO MEDIAS 'C' & 'N' OF CANADIAN AGENCIES                             
                                                                                
TESTSEL  DS    0H                                                               
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY                     
         BNE   TSLXY                EXIT IF NOT CANADIAN                        
*                                                                               
         CLI   QMED,C'C'           MEDIA 'C'                                    
         BE    TESTSEL1                                                         
         CLI   QMED,C'N'           MEDIA 'N'                                    
         BNE   TSLXY                                                            
*                                                                               
TESTSEL1 LA    R2,MSLSELH          R2-->FIRST SELECT FIELD                      
         LA    RF,MSLTAGH          RF-->LAST FIELD                              
*                                                                               
TESTSEL2 LLC   R1,0(R2)                                                         
         CLI   8(R2),C'C'          CHANGE FROM LIST SCREEN                      
         BE    TSLXN                                                            
         CLI   9(R2),C'C'                                                       
         BE    TSLXN                                                            
         CLI   10(R2),C'C'                                                      
         BE    TSLXN                                                            
*                                                                               
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    RF,R2                                                            
         BH    TESTSEL2                                                         
                                                                                
*                                                                               
TSLXN    DS    0H                                                               
         B     NO_01                                                            
*                                                                               
TSLXY    DS    0H                                                               
         B     YES_01                                                           
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--GMSTI#)'         
*---------------------- GET STATION MASTER INFO ----------------------*         
                                                                                
GETMASTI DS    0H                                                               
         MVI   DFFORMAT,C'*'       ELSE, MOVE IN GARBAGE                        
         MVC   DFFORMAT+1(L'DFFORMAT-1),DFFORMAT                                
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,AIO2                                                          
         USING STARECD,R6                                                       
         XC    0(15,R6),0(R6)                                                   
         MVI   STAKTYPE,C'S'       MASTER RECORDS ARE TYPE-'S'.                 
         MVC   STAKMED,DFMEDIA     MEDIA.                                       
         MVC   STAKCALL,DFSTTN     STATION.                                     
         MVC   STAKAGY,AGENCY      AGENCY.                                      
         MVC   STAKCLT,DFCLNT      THERE IS A CLIENT.                           
         MVC   STAKFILL,ZEROES                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'STATION ',(R6),(R6)                 
         CLI   8(R1),0                                                          
         BNE   GMSTI049                                                         
*                                                                               
         MVC   DFFORMAT,SFORMAT    GET FORMAT                                   
GMSTI049 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         B     GMSTIX                                                           
                                                                                
*                                                                               
GMSTIX   DS    0H                                                               
         GOTO1 READ                RESTORE THE READ-SEQUENTIALS.                
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--GMKTI#)'         
*-------------------------- GET MARKET INFO --------------------------*         
                                                                                
GETMRKTI DS    0H                                                               
*                                                                               
         CLI   DFCLNT,C'*'        ARE WE DOING OFFICE CODE??                    
         BNE   GMKTI010            - NOPE                                       
         MVC   DFMRKT,SPACES                                                    
         MVC   DFMKTNAM,SPACES                                                  
         B     GMKTI049           IT'S OVER                                     
                                                                                
GMKTI010 DS    0H                                                               
         CLC   DFMRKT,SVMKTN                                                    
         BE    GMKTI049           SAME MARKET NUMBER AS LAST READ               
*                                                                               
         MVC   DFMKTNAM,SPACES                                                  
         MVC   SVMKTN,DFMRKT                                                    
*                                                                               
         DS    0H                                                               
         L     R6,AIO2                                                          
         USING MKTRECD,R6                                                       
         MVC   0(15,R6),ZEROES                                                  
         MVI   MKTKTYPE,C'M'       MARKET RECORDS ARE TYPE-'M'.                 
         MVC   MKTKMED,DFMEDIA     MEDIA.                                       
         MVC   MKTKMKT,DFMRKT      MARKET                                       
         MVC   MKTKAGY,AGENCY      AGENCY.                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'STATION ',(R6),(R6)                 
         CLI   8(R1),0                                                          
         BE    GMKTI030                                                         
         TM    8(R1),X10+X02                                                    
         BNZ   GMKTI020                                                         
*                                                                               
         MVC   DFMKTNAM(L'MESERR),MESERR                                        
         B     GMKTI049                                                         
*                                                                               
GMKTI020 DS    0H                                                               
         MVC   DFMKTNAM(L'MESNOF),MESNOF                                        
         B     GMKTI049                                                         
*                                                                               
GMKTI030 DS    0H                                                               
         MVC   DFMKTNAM,MKTNAME    GET MARKET NAME                              
         B     GMKTI049                                                         
*                                                                               
GMKTI049 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         B     GMKTIX                                                           
                                                                                
*                                                                               
GMKTIX   DS    0H                                                               
         GOTO1 READ                RESTORE THE READ-SEQUENTIALS.                
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--PTINF#)'         
*-------------------------- PUT INFORMATION --------------------------*         
                                                                                
PUTINFO  DS    0H                                                               
         USING LISTD,R2                                                         
         CLI   DFSTTN,C'0'         IF DFSTTN IS CABLE, THEN WE DON'T            
         BL    *+8                 WANT THE 'T' APPENDED AT THE END             
         MVI   DFSTTN+4,C' '                                                    
         MVC   LSTSTAT,DFSTTN                                                   
         CLI   DFDEACTV,X'FF'                                                   
         BNE   *+8                                                              
         MVI   LSTSTAT+7,C'-'                                                   
         MVC   LSTCLNT,DFCLNT                                                   
         CLC   DFCLNT,ZEROES                                                    
         BNE   *+10                                                             
         MVC   LSTCLNT,SPACES                                                   
         CLI   LSTCLNT,C'*'        HAVE AN OFFICE CODE?                         
         BNE   PUT05               NO                                           
*                                                                               
         USING OFFICED,OFCBLK                                                   
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,LSTCLNT+1                                                 
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS)                     
         MVC   LSTCLNT+1(2),OFCOFC2                                             
*                                                                               
PUT05    MVC   LSTCODE,DFMRKT                                                   
         MVC   LSTNAME,DFMKTNAM                                                 
         MVC   LSTFORM,DFFORMAT                                                 
         MVC   LSTUID,DFUNIQID                                                  
         DROP  R2                                                               
*                                                                               
         AF    LISTCNTR,=F'1'                                                   
         CLI   MODE,PRINTREP                                                    
         BE    PUT10                                                            
         GOTO1 LISTMON                                                          
         B     XPUTINF                                                          
*                                                                               
PUT10    DS    0H                                                               
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
XPUTINF  DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VFMT#)'          
*-------------------------- VALIDATE FORMAT --------------------------*         
                                                                                
* Validates an inputted format code inputted in the options field.              
* At entry,                                                                     
*   R3-->SCANNER entry w/ format code in 2nd half,                              
*   R4-->entry of option keyword for format.                                    
* On exit when format code valid,                                               
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when format code invalid,                                             
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALFRMT  DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),4             CHECK L(FORMAT CODE INPUTTED)                
         BH    VFMTXN                                                           
*                                                                               
         MVC   BLOCK(L'OPVFRMTB),22(R3)  WANT FORMAT PADDED W/ BLANKS           
         B     VFMTXY                                                           
*                                                                               
** VALIDATE FORMAT EXITS **                                                     
*                                                                               
VFMTXY   DS    0H                                                               
         B     YES_01                                                           
VFMTXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VPAY#)'          
*-------------------------- VALIDATE PAYREP --------------------------*         
                                                                                
* Validates an inputted payrep code inputted in the options field.              
* At entry,                                                                     
*   R3-->SCANNER entry w/ payrep code in 2nd half,                              
*   R4-->entry of option keyword for payrep.                                    
* On exit when payrep code valid,                                               
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when payrep code invalid,                                             
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALPAY   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),3             CHECK L(PAYREP CODE INPUTTED)                
         BNE   VPAYXN                                                           
                                                                                
*        TM    3(R3),X80           PAYREP CODE S/B NUMERICS                     
*        BZ    VPAYXN                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         LLC   R2,1(R3)            R2=L(PAYREP CODE INPUTTED)                   
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)                                                  
         B     VPAYXY                                                           
*                                                                               
** VALIDATE PAYREP EXITS **                                                     
*                                                                               
VPAYXY   DS    0H                                                               
         B     YES_01                                                           
VPAYXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VCLI#)'          
*-------------------------- VALIDATE CLIENT --------------------------*         
                                                                                
* Validates an inputted client code inputted in the options field.              
* At entry,                                                                     
*   R3-->SCANNER entry w/ client code in 2nd half,                              
*   R4-->entry of option keyword for client.                                    
* On exit when client code valid,                                               
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when client code invalid,                                             
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALCLI   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,ICLTQ                                                   
         CLI   1(R3),3             CHECK L(CLIENT CODE INPUTTED)                
         BH    VCLIXN                                                           
*                                                                               
         CLI   22(R3),C'*'         OFFICE CODE?                                 
         BNE   VALC10              NO                                           
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCLMT,T217FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,23(R3)                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   *+12                 YES                                         
         CLI   DMCB,0                                                           
         BE    VALC05                                                           
*                                                                               
         MVI   OURERRCD,IOFFQ                                                   
         B     VCLIXN                                                           
*                                                                               
VALC05   MVC   23(1,R3),OFCOFC                                                  
         MVI   24(R3),C' '                                                      
*                                                                               
VALC10   DS    0H                                                               
         MVC   BLOCK(L'OPVCLITB),22(R3)  WANT CLT CODE PADDED W/ BLANKS         
         B     VCLIXY                                                           
*                                                                               
** VALIDATE CLIENT EXITS **                                                     
*                                                                               
VCLIXY   DS    0H                                                               
         B     YES_01                                                           
VCLIXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VTIM#)'          
*--------------------------- VALIDATE TIME ---------------------------*         
                                                                                
* Validates an inputted time sheet code inputted in the options field.          
* At entry,                                                                     
*   R3-->SCANNER entry w/ time sheet code in 2nd half,                          
*   R4-->entry of option keyword for time sheet.                                
* On exit when time sheet code valid,                                           
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when time sheet code invalid,                                         
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALTIM   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),3             CHECK L(TIME SHEET CODE INPUTTED)            
         BNE   VTIMXN                                                           
                                                                                
         TM    3(R3),X80           TIME SHEET CODE S/B NUMERICS                 
         BZ    VTIMXN                                                           
*                                                                               
         DS    0H                                                               
         LLC   R2,1(R3)            R2=L(TIME SHEET CODE INPUTTED)               
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)                                                  
         B     VTIMXY                                                           
                                                                                
*                                                                               
** VALIDATE TIME SHEET EXITS **                                                 
*                                                                               
VTIMXY   DS    0H                                                               
         B     YES_01                                                           
VTIMXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VAFF#)'          
*------------------------ VALIDATE AFFILIATION -----------------------*         
                                                                                
* Validates an inputted affiliation code inputted in the options field.         
* At entry,                                                                     
*   R3-->SCANNER entry w/ affiliation code in 2nd half,                         
*   R4-->entry of option keyword for affiliation.                               
* On exit when affiliation code valid,                                          
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when affiliation code invalid,                                        
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALAFF   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),2             AFFIL FILTER 2 CHARS?                        
         BE    VAFF10              YES - ALLOW IT                               
         CLI   1(R3),3             CHECK L(AFFILIATION CODE INPUTTED)           
         BE    VAFF10                                                           
         CLI   1(R3),4             COULD ALSO BE FOUR BYTES FOR CANADA          
         BNE   VAFFXN                                                           
*                                                                               
VAFF10   LLC   R2,1(R3)            R2=L(AFFILIATION CODE INPUTTED)              
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,WORK,22(R3)                                                   
         MVI   GOSUBN,CAN#         AFFILIATION CODE S/B ALPHANUMERICS           
         GOTO1 AGOSUB                                                           
         BNE   VAFFXN                                                           
*                                                                               
         DS    0H                                                               
         MVI   BLOCK+2,X'40'       IN CASE WE HAVE A 2-CHAR AFFIL FILT          
         EXMVC R2,BLOCK,22(R3)     R2=L(AFFILIATION CODE INPUTTED) - 1          
         B     VAFFXY                                                           
                                                                                
*                                                                               
** VALIDATE AFFILIATION EXITS **                                                
*                                                                               
VAFFXY   DS    0H                                                               
         B     YES_01                                                           
VAFFXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VTYP#)'          
*----------------------- VALIDATE STATION TYPE -----------------------*         
                                                                                
* Validates an inputted station type code inputted in the options fld.          
* At entry,                                                                     
*   R3-->SCANNER entry w/ station type code in 2nd half,                        
*   R4-->entry of option keyword for station type.                              
* On exit when station typecode valid,                                          
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when station type code invalid,                                       
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALTYP   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(STATION TYPE CODE INPUTTED)          
         BNE   VTYPXN                                                           
                                                                                
         LLC   R2,1(R3)            R2=L(STATION TYPE CODE INPUTTED)             
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,WORK,22(R3)                                                   
         MVI   GOSUBN,CAN#         STATION TYPE CODE S/B ALPHANUMERICS          
         GOTO1 AGOSUB                                                           
         BNE   VTYPXN                                                           
*                                                                               
         DS    0H                                                               
         EXMVC R2,BLOCK,22(R3)     R2=L(STATION TYPE CODE INPUTTED) - 1         
*                                                                               
** VALIDATE STATION TYPE EXITS **                                               
*                                                                               
VTYPXY   DS    0H                                                               
         B     YES_01                                                           
VTYPXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VEIX#)'          
*----------------------- VALIDATE EIX=Y/N ----------------------------*         
*   R3-->SCANNER entry with (y/n) in 2nd half,                                  
*   R4-->entry of option keyword for eix=y/n                                    
* On exit when EIX valid,                                                       
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* ON EXIT WHEN EIX FILTER IS INVALID,                                           
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALEIX   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(STATION TYPE CODE INPUTTED)          
         BNE   VEIXXN                                                           
*                                                                               
         CLI   22(R3),C'Y'         YES OR NO?                                   
         BE    VALEIX10                                                         
         CLI   22(R3),C'N'                                                      
         BNE   VEIXXN              INVALID IF NEITHER                           
*                                                                               
VALEIX10 LLC   R2,1(R3)            R2=L(STATION TYPE CODE INPUTTED)             
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)     R2=L(STATION TYPE CODE INPUTTED) - 1         
*                                                                               
VEIXXY   DS    0H                                                               
         B     YES_01                                                           
VEIXXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--LTORG &&+        
                CONSTANTS)'                                                     
*------------------------ VALIDATE BOOK TYPE -------------------------*         
                                                                                
* Validates an inputted book type inputted in the options field.                
* At entry,                                                                     
*   R3-->SCANNER entry w/ book type code in 2nd half,                           
*   R4-->entry of option keyword for book type.                                 
* On exit when book type code valid,                                            
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when book type code invalid,                                          
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALBKT   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,INVBKTQ                                                 
         CLI   1(R3),2             CHECK L(BOOKTYPE CODE INPUTTED)              
         BH    VBKTXN                                                           
*                                                                               
         CLI   22(R3),C'?'         FILTER ON NO BOOK TYPE?                      
         BNE   VALBK5              NO                                           
         MVC   BLOCK(1),22(R3)     YES - "?" IS VALID!                          
         B     VBKTXY                                                           
*                                                                               
VALBK5   L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VALBK10  CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+12                NO                                           
         MVI   OURERRCD,INVBKTQ2   YES - INVALID BOOKTYPE ERROR                 
         B     VBKTXN                                                           
         CLC   SPBKTYPA,22(R3)     MATCH ON 2 CHAR BOOKTYPE?                    
         BE    *+10                YES                                          
         AR    RF,R0               NO - BUMP TABLE ENTRY                        
         B     VALBK10                                                          
         MVC   BLOCK(1),SPBKTYPN   1 BYTE INTERNAL BOOKTYPE                     
         DROP  RF                                                               
*                                                                               
VBKTXY   DS    0H                                                               
         B     YES_01                                                           
VBKTXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
*                                                                               
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--LTORG &&+        
                CONSTANTS)'                                                     
*------------------------ VALIDATE UNIQUE ID -------------------------*         
                                                                                
* Validates an inputted book type inputted in the options field.                
* At entry,                                                                     
*   R3-->Scanner entry w/ Unique ID Code in 2nd half,                           
*   R4-->entry of option keyword for book type.                                 
* On exit when book type code valid,                                            
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when book type code invalid,                                          
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALUID   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         TM    3(R3),X'80'         CHECK NUMERIC                                
         BNZ   VUID20                                                           
         CLI   1(RA),C'*'          ONLY DDS TERMINAL CAN BE NON-NUMERIC         
         BE    VUID10                                                           
         MVI   OURERRCD,INVALID                                                 
         B     VUIDXN                                                           
VUID10   MVC   BLOCK(6),22(R3)                                                  
         B     VUIDXY                                                           
*                                                                               
VUID20   XR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R3)                                                     
         UNPK  BLOCK(6),DUB                                                     
*                                                                               
VUIDXY   DS    0H                                                               
         B     YES_01                                                           
VUIDXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
*------------------------ VALIDATE SHOW= -----------------------------*         
                                                                                
* Validates show= filter B(both) D(deactivated)                                 
*   R3-->Scanner entry w/ show filter in 2nd half                               
*   R4-->entry of option keyword for show=                                      
* On exit when show= is valid,                                                  
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when book type code invalid,                                          
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALSHW   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(SHOW= CODE INPUTTED)                 
         BNE   VSHWXN                                                           
*                                                                               
         CLI   22(R3),C'B'         BOTH?                                        
         BE    VALSHW10            YES                                          
         CLI   22(R3),C'D'         DEACTIVATED                                  
         BNE   VSHWXN              NO - ERROR                                   
*                                                                               
VALSHW10 LLC   R2,1(R3)            R2=L(SHOW= CODE INPUTTED)                    
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)     R2=L(SHOW= CODE)                             
*                                                                               
VSHWXY   DS    0H                                                               
         B     YES_01                                                           
VSHWXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
*------------------------ VALIDATE LOCK= -----------------------------*         
                                                                                
* Validates lock= filter Y/N                                                    
*   R3-->Scanner entry w/ lock filter in 2nd half                               
*   R4-->entry of option keyword for lock=                                      
* On exit when lock= is valid,                                                  
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when lock entry is invalid                                            
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALLOC   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(LOCK= CODE INPUTTED)                 
         BNE   VLOCXN                                                           
*                                                                               
         CLI   22(R3),C'Y'         YES?                                         
         BE    VALLOC10            YES                                          
         CLI   22(R3),C'N'         NO?                                          
         BNE   VLOCXN              NO - ERROR                                   
*                                                                               
VALLOC10 LLC   R2,1(R3)            R2=L(LOCK= CODE INPUTTED)                    
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)     R2=L(LOCK= CODE)                             
*                                                                               
VLOCXY   DS    0H                                                               
         B     YES_01                                                           
VLOCXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
*------------------------ VALIDATE MIDAS= ----------------------------*         
                                                                                
* Validates midas=filter M/C                                                    
*   R3-->Scanner entry w/ midas filter in 2nd half                              
*   R4-->entry of option keyword for midas=                                     
* On exit when lock= is valid,                                                  
*   CC set to equal,                                                            
*   BLOCK contains option data to be stored.                                    
* On exit when midas entry is invalid                                           
*   CC set to not equal,                                                        
*   OURERRCD = appropriate error code,                                          
*   FLDDSPL  = displacement of error input,                                     
*   MYTEXT   = text for text replace (if there are any).                        
* NOTE: Do NOT clobber BUFF here!                                               
                                                                                
VALMID   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(MIDAS= CODE INPUTTED)                
         BNE   VMIDXN                                                           
*                                                                               
         CLI   22(R3),C'M'         M?                                           
         BE    VALMID10            YES                                          
         CLI   22(R3),C'C'         C?                                           
         BNE   VMIDXN              NO - ERROR                                   
*                                                                               
VALMID10 LLC   R2,1(R3)            R2=L(MIDAS= CODE INPUTTED)                   
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)     R2=L(MIDAS= CODE)                            
*                                                                               
VMIDXY   DS    0H                                                               
         B     YES_01                                                           
VMIDXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
*------------------------ VALIDATE BAND= -----------------------------*         
                                                                                
* VALIDATES BAND=FILTER                                                         
*   R3-->SCANNER ENTRY W/ BAND FILTER IN 2ND HALF                               
*   R4-->ENTRY OF OPTION KEYWORD FOR BAND=                                      
* ON EXIT WHEN BAND= IS VALID,                                                  
*   CC SET TO EQUAL,                                                            
*   BLOCK contains option data to be stored.                                    
* ON EXIT WHEN BANDS ENTRY IS INVALID                                           
*   CC SET TO NOT EQUAL,                                                        
*   OURERRCD = APPROPRIATE ERROR CODE,                                          
*   FLDDSPL  = DISPLACEMENT OF ERROR INPUT,                                     
*   MYTEXT   = TEXT FOR TEXT REPLACE (IF THERE ARE ANY).                        
* NOTE: DO NOT CLOBBER BUFF HERE!                                               
                                                                                
VALBND   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(MIDAS= CODE INPUTTED)                
         BNE   VBNDXN                                                           
*                                                                               
VALBND10 LLC   R2,1(R3)            R2=L(MIDAS= CODE INPUTTED)                   
         STC   R2,BYTE                                                          
         BCTR  R2,0                                                             
         EXMVC R2,BLOCK,22(R3)     R2=L(BAND CODE)                              
*                                                                               
VBNDXY   DS    0H                                                               
         B     YES_01                                                           
VBNDXN   DS    0H                                                               
         B     NO_01                                                            
*                                                                               
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBR01--VTYP#)'          
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
MESNOF   DC    C'** MARKET NOT ON FILE **'                                      
MESERR   DC    C'** ERROR ENCOUNTERED **'                                       
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM)'                 
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave SFM39 entirely and displays a message go through            
*  this routine.                                                                
* At entry,                                                                     
*   MYTEXT has length and text of text-replace.                                 
                                                                                
*&&DO                                                                           
SUBRXMQ  EQU   (((*-SFM39+X'0FFF')/X'1000')*X'1000')                            
*&&                                                                             
SUBRXMQ  EQU   (((*-SFM39+X'00FF')/X'100')*X'100')                              
                                                                                
         ORG   SFM39+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**39XM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         MVC   MSGTYPE,0(R1)       GET MESSAGE TYPE                             
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   MSGTYPE,C'E'        EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   MSGTYPE,C'W'        EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   MSGTYPE,C'I'        EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
ALLMSGX  DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM--ERR MSGS+        
               )'                                                               
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* These messages require the user to re-input (the correct) data for a          
*  response.  Previous values are restored, if the key did not change,          
*  and fields modified this time around will appear modified in the             
*  next transaction.                                                            
* At entry, R2-->field to put cursor.                                           
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURERRCD,ERRX#                                                   
         BNH   XMERRGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         CLI   OURERRCD,ERRX2#                                                  
         BNH   XMERRGO                                                          
         DC    H'0'                                                             
                                                                                
XMERRGO  DS    0H                                                               
         CLI   OURERRCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURERRCD,XMERRQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LLC   RF,OURERRCD         BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
ISELQ    EQU   ((XMERR03-XMERR00)/4)+1                                          
IMKTQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
ISTAQ    EQU   ((XMERR05-XMERR00)/4)+1                                          
INVOQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
IOCBQ    EQU   ((XMERR07-XMERR00)/4)+1                                          
TMODQ    EQU   ((XMERR08-XMERR00)/4)+1                                          
MODQ     EQU   ((XMERR09-XMERR00)/4)+1                                          
IODVQ    EQU   ((XMERR10-XMERR00)/4)+1                                          
ROMQ     EQU   ((XMERR11-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR12-XMERR00)/4)+1                                          
OSKQ     EQU   ((XMERR13-XMERR00)/4)+1                                          
MOTCQ    EQU   ((XMERR14-XMERR00)/4)+1                                          
ICLTQ    EQU   ((XMERR15-XMERR00)/4)+1                                          
TMFQ     EQU   ((XMERR16-XMERR00)/4)+1                                          
NCMCNQ   EQU   ((XMERR17-XMERR00)/4)+1                                          
IOMAXQ   EQU   ((XMERR18-XMERR00)/4)+1                                          
INVBKTQ  EQU   ((XMERR19-XMERR00)/4)+1                                          
IOFFQ    EQU   ((XMERR20-XMERR00)/4)+1                                          
INVBKTQ2 EQU   ((XMERR21-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     ISEL                INVALID SELECT CODE                          
XMERR04  B     IMKT                INVALID MARKET                               
XMERR05  B     ISTA                INVALID STATION                              
XMERR06  B     INVO                INVALID OPTION KEYWORD                       
XMERR07  B     IOCB                INVALID OPTION COMBINATION                   
XMERR08  B     TMOD                TOO MANY OPTION DATA VALUES                  
XMERR09  B     MOD                 MISSING OPTION DATA                          
XMERR10  B     IODV                INVALID OPTION DATA VALUE                    
XMERR11  B     ROPTMISS            REQUIRED OPTION MISSING                      
XMERR12  B     DUPOPT              DUPLICATED OPTION KEYWORD                    
XMERR13  B     OSK                 OPTION SPECIFIED BY KEYWORD ONLY             
XMERR14  B     MOTC                MEDIA AND OPTION TYPE CONFLICT               
XMERR15  B     ICLT                INVALID CLIENT                               
XMERR16  B     TMF                 TOO MANY FILTERS IN FIELD                    
XMERR18  B     IOMAX               MAX I/O REACHED                              
XMERR19  B     INVBKT              INVALID BOOKTYPE CODE LENGTH                 
XMERR20  B     IOFFICE             INVALID OFFICE                               
XMERR21  B     INVBKT2             INVALID BOOKTYPE                             
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERR17  B     NCMCN               CAN NOT CHANGE MEDIA C/N                     
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
*                                                                               
MFLD     DS    0H                  MISSING INPUT FIELD                          
         MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
IFLD     DS    0H                  INVALID INPUT FIELD                          
         MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
ISEL     DS    0H                  INVALID SELECT CODE                          
         MVC   MSGNUM2,=H'221'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IMKT     DS    0H                  INVALID MARKET                               
         MVC   MSGNUM2,=AL2(SE#INVMK)                                           
         B     ERRGTXT                                                          
*                                                                               
ISTA     DS    0H                  INVALID STATION                              
         MVC   MSGNUM2,=H'92'                                                   
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
*                                                                               
INVO     DS    0H                  INVALID OPTION KEYWORD                       
         MVC   MSGNUM2,=H'206'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOCB     DS    0H                  INVALID OPTION COMBINATION                   
         MVC   MSGNUM2,=AL2(SE#IOCMB)                                           
         B     ERRGTXT                                                          
*                                                                               
TMOD     DS    0H                  TOO MANY OPTION DATA VALUES                  
         MVC   MSGNUM2,=AL2(SE#TMOD)                                            
         B     ERRGTXT                                                          
*                                                                               
MOD      DS    0H                  MISSING OPTION DATA                          
         MVC   MSGNUM2,=AL2(SE#MOPTD)                                           
         B     ERRGTXT                                                          
*                                                                               
IODV     DS    0H                  INVALID OPTION DATA VALUE                    
         MVC   MSGNUM2,=H'209'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
ROPTMISS DS    0H                  REQUIRE OPTION MISSING                       
         MVC   MSGNUM2,=AL2(SE#ROPTM)                                           
         B     ERRGTXT                                                          
*                                                                               
DUPOPT   DS    0H                  DUPLICATED OPTION KEYWORD                    
         MVC   MSGNUM2,=H'208'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
OSK      DS    0H                  OPTION SPECIFIED BY KEYWORD ONLY             
         MVC   MSGNUM2,=AL2(SE#OPSKW)                                           
         B     ERRGTXT                                                          
*                                                                               
MOTC     DS    0H                  MEDIA AND OPTION TYPE CONFLICT               
         MVC   MSGNUM2,=AL2(SE#MOTC)                                            
         B     ERRGTXT                                                          
*                                                                               
ICLT     DS    0H                  INVALID CLIENT                               
         MVC   MSGNUM2,=AL2(SE#INCLT)                                           
         B     ERRGTXT                                                          
*                                                                               
TMF      DS    0H                  TOO MANY FILTERS IN FIELD                    
         MVC   MSGNUM2,=AL2(137)                                                
         MVI   MSGSYS,71                                                        
         B     ERRGTXT                                                          
*                                                                               
IOMAX    DS    0H                  I/O MAX REACHED                              
         MVC   MSGNUM2,=AL2(806)                                                
         B     ERRGTXT                                                          
*                                                                               
INVBKT   DS    0H                  INVALID BOOKTYPE LENGTH                      
         MVC   MSGNUM2,=AL2(832)                                                
         B     ERRGTXT                                                          
*                                                                               
INVBKT2  DS    0H                  INVALID BOOKTYPE                             
         MVC   MSGNUM2,=AL2(1298)                                               
         B     ERRGTXT                                                          
*                                                                               
IOFFICE  DS    0H                  INVALID OFFICE                               
         MVC   MSGNUM2,=AL2(544)                                                
         B     ERRGTXT                                                          
*                                                                               
NCMCN    DS    0H                  CANNOT CHANGE MEDIA C/N                      
         MVC   0(23,R1),=C'Cannot change media C/N'                             
         B     ERREXIT                                                          
         EJECT                                                                  
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
         B     ERREXIT                                                          
                                                                                
ERREXIT  DS    0H                                                               
         LR    R0,R2               SAVE DISPL OF FIELD                          
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         B     ALLMSGX                                                          
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM--WRN MSGS+        
               )'                                                               
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* These are messages where the user needs to hit <Enter> only for a             
*  response (for acknowledgment).  Previous values are restored,                
*  except in the case when the key changed in the same transaction.             
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         LA    R2,MSLMEDIH         FORCE CURSOR TO KEY                          
                                                                                
         DS    0H                                                               
         CLI   OURWRNCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURWRNCD,XMWRNQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LLC   RF,OURWRNCD         BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMWRN00(RF)                                                      
                                                                                
****Q    EQU   ((XMWRN01-XMWRN00)/4)+1                                          
                                                                                
XMWRN00  DS    0H                                                               
XMWRN01  B     WRNEXIT             (NOT USED)                                   
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
         DS    0H                                                               
         EJECT                                                                  
WRNGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMWRN        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
WRNEXIT  DS    0H                                                               
         MVI   ERROR,0                                                          
         ICM   R0,15,ACURFORC      OVERRIDE CURSOR POSITION                     
         BZ    *+6                                                              
         LR    R2,R0                YEP                                         
                                                                                
         B     ALLMSGX                                                          
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM--INF MSGS+        
               )'                                                               
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
         LA    R1,CONHEAD                                                       
                                                                                
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURINFCD,INFX#                                                   
         BL    XMINFGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   OURINFCD,INFX2#                                                  
         BL    XMINFGO                                                          
         DC    H'0'                                                             
                                                                                
XMINFGO  DS    0H                                                               
         CLI   OURINFCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURINFCD,XMINFQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LLC   RF,OURINFCD         BRANCH OFF TO SET INFO MESSAGE               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
*****Q   EQU   ((XMINF01-XMINF00)/4)+1  (NOT USED)                              
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     INFEXIT             (NOT USED)                                   
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
INFX2#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
         DS    0H                                                               
         EJECT                                                                  
INFGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMINF        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         MVI   ERROR,0                                                          
         B     ALLMSGX                                                          
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM--LTORG &&+        
                CONSTANTS)'                                                     
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SUBRXM--MISC STU+        
                FF)'                                                            
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING'                          
***********************************************************************         
*========================== SFM39's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
MAX#OPTQ EQU   5                   MAX NUMBER OF OPTIONS ALLOWED                
CKYMKTL  EQU   CTDMKMKT-CTDMKEY+L'CTDMKMKT                                      
AKYAMKL  EQU   ANMKAMRK-ANMKEYD+L'ANMKAMRK                                      
                                                                                
*                                 ********* BIT MANIPULATIONS *********         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
X00      EQU   X'00'                                                            
                                                                                
*                                 *********** OPTION EQUATES **********         
OPNNMK   EQU   1                   (NUMERIC) MARKET: MKT=                       
OPBNMK   EQU   X'80000000'                                                      
OPNFRM   EQU   2                   FORMAT: FORM=                                
OPBFRM   EQU   X'40000000'                                                      
OPNPAY   EQU   3                   PAYREP: PAY=                                 
OPBPAY   EQU   X'20000000'                                                      
OPNCLI   EQU   4                   CLIENT: CLI=                                 
OPBCLI   EQU   X'10000000'                                                      
OPNTIM   EQU   5                   TIME SHEET(CONTRACT) REP: TIM=               
OPBTIM   EQU   X'08000000'                                                      
OPNAFF   EQU   6                   NETWORK AFFILIATION: AFF=                    
OPBAFF   EQU   X'04000000'                                                      
OPNTYP   EQU   7                   STATION TYPE: TYPE=                          
OPBTYP   EQU   X'02000000'                                                      
OPNEIX   EQU   8                   STATION TYPE: EIX=                           
OPBEIX   EQU   X'01000000'                                                      
OPNBKT   EQU   9                   BOOK TYPE: BKT=                              
OPBBKT   EQU   X'00800000'                                                      
OPNUID   EQU   10                  UNIQUE ID: UID=                              
OPBUID   EQU   X'00400000'                                                      
OPNKMK   EQU   11                  K KEYS: KMKT=                                
OPBKMK   EQU   X'00200000'                                                      
OPNSHW   EQU   12                  SHOWALL                                      
OPBSHW   EQU   X'00100000'                                                      
OPNLOC   EQU   13                  LOCK=Y/N                                     
OPBLOC   EQU   X'00080000'                                                      
OPNMID   EQU   14                  MIDAS=M/C                                    
OPBMID   EQU   X'00040000'                                                      
OPNBND   EQU   15                  BAND FILTER: BAND=                           
OPBBND   EQU   X'00020000'                                                      
                                                                                
ALLFILTS EQU   OPBNMK+OPBFRM+OPBPAY+OPBCLI+OPBTIM+OPBAFF+OPBTYP+OPBEIX          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-SFM39,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-SFM39,0,ASUBR01-SYSD)                                 
         DC    AL2(XMSGRTN-SFM39,0,AXMSGRTN-SYSD)                               
         DC    AL2(OPTTABLE-SFM39,0,AOPTTAB-SYSD)                               
         DC    AL2(OPTVALS-SYSD,ASYSD-GEND,AOPTVALS-SYSD)                       
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
DCLISTX  EQU   *                                                                
         EJECT                                                                  
*--------------------------- OPTIONS TABLE ---------------------------*         
                                                                                
OPTTABLE DS    0X                  SEE OPTDSECT                                 
*                                                                               
OP01     DS    0X                  MKT=                                         
         DC    AL1(OPNNMK),AL1(OP01X-OP01)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VMKT#,0)                                                     
         DC    AL4(OPBNMK)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVNMKMX),AL1(L'OPVNMKTB),AL2(OPVNMKNM-SYSD)                 
         DC    AL2(OP01NAM1-OP01,0)                                             
OP01NAM1 DC    AL1(OP01NAMX-OP01NAM1-1),C'MKT'                                  
OP01NAMX DC    AL1(EOT)                                                         
OP01X    EQU   *                                                                
*                                                                               
OP02     DS    0X                  FORM=                                        
         DC    AL1(OPNFRM),AL1(OP02X-OP02)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VFMT#,0)                                                     
         DC    AL4(OPBFRM)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVFRMMX),AL1(L'OPVFRMTB),AL2(OPVFRMNM-SYSD)                 
         DC    AL2(OP02NAM2-OP02,0)                                             
OP02NAM2 DC    AL1(OP02NAMX-OP02NAM2-1),C'FORM'                                 
OP02NAMX DC    AL1(EOT)                                                         
OP02X    EQU   *                                                                
*                                                                               
OP03     DS    0X                  PAY=                                         
         DC    AL1(OPNPAY),AL1(OP03X-OP03)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VPAY#,0)                                                     
         DC    AL4(OPBPAY)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVPAYMX),AL1(L'OPVPAYTB),AL2(OPVPAYNM-SYSD)                 
         DC    AL2(OP03NAM1-OP03,0)                                             
OP03NAM1 DC    AL1(OP03NAMX-OP03NAM1-1),C'PAY'                                  
OP03NAMX DC    AL1(EOT)                                                         
OP03X    EQU   *                                                                
*                                                                               
OP04     DS    0X                  CLI=                                         
         DC    AL1(OPNCLI),AL1(OP04X-OP04)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VCLI#,0)                                                     
         DC    AL4(OPBCLI)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVCLIMX),AL1(L'OPVCLITB),AL2(OPVCLINM-SYSD)                 
         DC    AL2(OP04NAM1-OP04,0)                                             
OP04NAM1 DC    AL1(OP04NAMX-OP04NAM1-1),C'CLI'                                  
OP04NAMX DC    AL1(EOT)                                                         
OP04X    EQU   *                                                                
*                                                                               
OP05     DS    0X                  TIM=                                         
         DC    AL1(OPNTIM),AL1(OP05X-OP05)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VTIM#,0)                                                     
         DC    AL4(OPBTIM)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVTIMMX),AL1(L'OPVTIMTB),AL2(OPVTIMNM-SYSD)                 
         DC    AL2(OP05NAM1-OP05,0)                                             
OP05NAM1 DC    AL1(OP05NAMX-OP05NAM1-1),C'TIM'                                  
OP05NAMX DC    AL1(EOT)                                                         
OP05X    EQU   *                                                                
*                                                                               
OP06     DS    0X                  AFF=                                         
         DC    AL1(OPNAFF),AL1(OP06X-OP06)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VAFF#,0)                                                     
         DC    AL4(OPBAFF)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVAFFMX),AL1(L'OPVAFFTB),AL2(OPVAFFNM-SYSD)                 
         DC    AL2(OP06NAM1-OP06,0)                                             
OP06NAM1 DC    AL1(OP06NAMX-OP06NAM1-1),C'AFF'                                  
OP06NAMX DC    AL1(EOT)                                                         
OP06X    EQU   *                                                                
*                                                                               
OP07     DS    0X                  TYP=                                         
         DC    AL1(OPNTYP),AL1(OP07X-OP07)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VTYP#,0)                                                     
         DC    AL4(OPBTYP)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVTYPMX),AL1(L'OPVTYPTB),AL2(OPVTYPNM-SYSD)                 
         DC    AL2(OP07NAM1-OP07,0)                                             
OP07NAM1 DC    AL1(OP07NAMX-OP07NAM1-1),C'TYPE'                                 
OP07NAMX DC    AL1(EOT)                                                         
OP07X    EQU   *                                                                
*                                                                               
OP08     DS    0X                  EIX=                                         
         DC    AL1(OPNEIX),AL1(OP08X-OP08)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VEIX#,0)                                                     
         DC    AL4(OPBEIX)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVEIXMX),AL1(L'OPVEIXTB),AL2(OPVEIXNM-SYSD)                 
         DC    AL2(OP08NAM1-OP08,0)                                             
OP08NAM1 DC    AL1(OP08NAMX-OP08NAM1-1),C'EIX'                                  
OP08NAMX DC    AL1(EOT)                                                         
OP08X    EQU   *                                                                
*                                                                               
OP09     DS    0X                  BKT=                                         
         DC    AL1(OPNBKT),AL1(OP09X-OP09)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VBKT#,0)                                                     
         DC    AL4(OPBBKT)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVBKTMX),AL1(L'OPVBKTTB),AL2(OPVBKTNM-SYSD)                 
         DC    AL2(OP09NAM1-OP09,0)                                             
OP09NAM1 DC    AL1(OP09NAMX-OP09NAM1-1),C'BKT'                                  
OP09NAMX DC    AL1(EOT)                                                         
OP09X    EQU   *                                                                
*                                                                               
OP10     DS    0X                             UID=                              
         DC    AL1(OPNUID),AL1(OP10X-OP10)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VUID#,0)                                                     
         DC    AL4(OPBUID)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVUIDMX),AL1(L'OPVUIDTB),AL2(OPVUIDNM-SYSD)                 
         DC    AL2(OP10NAM1-OP10,0)                                             
OP10NAM1 DC    AL1(OP10NAMX-OP10NAM1-1),C'UID'                                  
OP10NAMX DC    AL1(EOT)                                                         
OP10X    EQU   *                                                                
*                                                                               
OP11     DS    0X                  KMKT=                                        
         DC    AL1(OPNKMK),AL1(OP11X-OP11)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VMKT#,0)                                                     
         DC    AL4(OPBKMK)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVKMKMX),AL1(L'OPVKMKTB),AL2(OPVKMKNM-SYSD)                 
         DC    AL2(OP11NAM1-OP11,0)                                             
OP11NAM1 DC    AL1(OP11NAMX-OP11NAM1-1),C'KMKT'                                 
OP11NAMX DC    AL1(EOT)                                                         
OP11X    EQU   *                                                                
*                                                                               
OP12     DS    0X                  SHOW=A(ALL) B(BOTH) D(DEACTIVATED)           
         DC    AL1(OPNSHW),AL1(OP12X-OP12)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VSHW#,0)                                                     
         DC    AL4(OPBSHW)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVSHWMX),AL1(L'OPVSHWTB),AL2(OPVSHWNM-SYSD)                 
         DC    AL2(OP12NAM1-OP12,0)                                             
OP12NAM1 DC    AL1(OP12NAMX-OP12NAM1-1),C'SHOW'                                 
OP12NAMX DC    AL1(EOT)                                                         
OP12X    EQU   *                                                                
*                                                                               
OP13     DS    0X                  LOCK=Y/N                                     
         DC    AL1(OPNLOC),AL1(OP13X-OP13)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VLOC#,0)                                                     
         DC    AL4(OPBLOC)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVLOCMX),AL1(L'OPVLOCTB),AL2(OPVLOCNM-SYSD)                 
         DC    AL2(OP13NAM1-OP13,0)                                             
OP13NAM1 DC    AL1(OP13NAMX-OP13NAM1-1),C'LOCK'                                 
OP13NAMX DC    AL1(EOT)                                                         
OP13X    EQU   *                                                                
*                                                                               
OP14     DS    0X                  MIDAS=M/C                                    
         DC    AL1(OPNMID),AL1(OP14X-OP14)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VMID#,0)                                                     
         DC    AL4(OPBMID)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVMIDMX),AL1(L'OPVMIDTB),AL2(OPVMIDNM-SYSD)                 
         DC    AL2(OP14NAM1-OP14,0)                                             
OP14NAM1 DC    AL1(OP14NAMX-OP14NAM1-1),C'MIDAS'                                
OP14NAMX DC    AL1(EOT)                                                         
OP14X    EQU   *                                                                
*                                                                               
OP15     DS    0X                  BAND=                                        
         DC    AL1(OPNBND),AL1(OP15X-OP15)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VBND#,0)                                                     
         DC    AL4(OPBBND)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVBNDMX),AL1(L'OPVBNDTB),AL2(OPVBNDNM-SYSD)                 
         DC    AL2(OP15NAM1-OP15,0)                                             
OP15NAM1 DC    AL1(OP15NAMX-OP15NAM1-1),C'BAND'                                 
OP15NAMX DC    AL1(EOT)                                                         
OP15X    EQU   *                                                                
*                                                                               
OPTZ     DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SPSFMWORKD)'             
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SYSD)'                   
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*----------------------- OWNED BY SPSFM39 ONLY -----------------------*         
                                                                                
*                                 ************** WORK AREA ************         
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
RELO     DS    F                                                                
MYDMCB   DS    6F                                                               
FLTMKT   DS    CL4                 MARKET FILTER                                
SVMKTN   DS    CL4                 REGULATE READINGS OF MKT REC                 
SVKEY1   DS    CL48                                                             
BFLTMKT  DS    XL2                 BINARY MARKET FILTER                         
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
AOPTNTRY DS    A                   A(OPTTABLE ENTRY)                            
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
AOPTTAB  DS    A                    A(OPTTABLE)                                 
AOPTVALS DS    A                    A(OPTION VALUES)                            
                                                                                
*                                 ************* INPUT DATA ************         
INPUTKEY DS    0C                  INPUT TO KEY FIELDS                          
IKMEDIA  DS    CL1                  MEDIA                                       
IKSTTN   DS    CL5                  STATION                                     
                                                                                
*                                 *********** DATA FROM FILE **********         
DFMEDIA  DS    CL(L'STAKMED)       MEDIA                                        
DFMRKT   DS    CL(L'MKTKMKT)       MARKET CODE                                  
DFSTTN   DS    CL(L'QSTANEW)       STATION CALL LETTERS                         
DFCLNT   DS    CL(L'STAKCLT)       CLIENT                                       
DFFORMAT DS    CL(L'SFORMAT)       FORMAT                                       
DFMKTNAM DS    CL(L'MKTNAME)       MARKET NAME                                  
DFUNIQID DS    CL(L'STUNIQID)      UNIQUE ID                                    
DFDEACTV DS    CL(L'SSYSDACT)      DEACTIVATED IF SET TO X'FF'                  
                                                                                
*                                 ************ TEMP STORAGE ***********         
TMPS     DS   0X                                                                
TMPSTTN  DS    CL5                 TEMP STORAGE FOR STATION                     
TMPSX    EQU  *                                                                 
TMPSL    EQU  TMPSX-TMPS                                                        
                                                                                
*                                 *********** MISCELLANEOUS ***********         
LISTCNTR DS    F                   # OF ENTRIES PUT ONTO LIST                   
ACTVWHY  DS    CL1                 REASON FOR ACTIVITY                          
OURBYTE  DS    XL1                 ANOTHER 1-BYTE WORKING STORAGE               
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
MSGTYPE  DS    CL1                 (E)RROR, (W)ARNING, OR (I)NFO                
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
COUNTER  DS    XL1                                                              
FLDDSPL  DS    XL1                 DISPL OF SUB-FIELD INTO FIELD                
SCANLNTH DS    XL1                 L'RHS OF SCANNER BLOCK ENTRY                 
NOPTN    DS    XL1                 NUMBER OF OPTIONS INPUTTED                   
OPTI     DS    AL4                 THE OPTIONS INPUTTED                         
OPTR     DS    AL4                 OPTIONS REQUIRED                             
OPTX     DS    AL4                 OPTIONS NOT ALLOWED                          
OFCBLK   DS    XL(OFCLENQ)                                                      
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1ERRQ  EQU    X01                                                             
MF1RSTKC EQU   0                   RESET THESE ON KEY CHANGE                    
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
DSLISTX  EQU   *                                                                
*                                 ************** BUFFERS **************         
MYSVKEY  DS    XL(L'KEY)                                                        
ZEROES   DS    CL(L'KEY)                                                        
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
MYFLD    DS    0X                  MY TWA FIELD WORK AREA                       
MYFLDH   DS     XL8                 MY WORKING FIELD HEADER                     
MYFLDD   DS     XL80                MY WORKING FIELD DATA                       
MYFLDL   EQU   *-MYFLD                                                          
         EJECT                                                                  
*                                 *********** OPTION VALUES ***********         
OPTVALS  DS    0C                                                               
                                                                                
OPVNMK   DS    0X                  OPTION VALUES FOR MARKET                     
OPVNMKMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVNMKNM DS     XL1                     # OF VALUES SO FAR                      
OPVNMKTB DS     (OPVNMKMX)CL4           TABLE TO HOLD VALUES                    
OPVNMKQ  EQU   *-OPVNMK                                                         
                                                                                
OPVFRM   DS    0X                  OPTION VALUES FOR FORMAT                     
OPVFRMMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVFRMNM DS     XL1                     # OF VALUES SO FAR                      
OPVFRMTB DS     (OPVFRMMX)CL4           TABLE TO HOLD VALUES                    
OPVFRMQ  EQU   *-OPVFRM                                                         
                                                                                
OPVPAY   DS    0X                  OPTION VALUES FOR PAYREP                     
OPVPAYMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVPAYNM DS     XL1                     # OF VALUES SO FAR                      
OPVPAYTB DS     (OPVPAYMX)CL3           TABLE TO HOLD VALUES                    
OPVPAYQ  EQU   *-OPVPAY                                                         
                                                                                
OPVCLI   DS    0X                  OPTION VALUES FOR CLIENT                     
OPVCLIMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVCLINM DS     XL1                     # OF VALUES SO FAR                      
OPVCLITB DS     (OPVCLIMX)CL3           TABLE TO HOLD VALUES                    
OPVCLIQ  EQU   *-OPVCLI                                                         
                                                                                
OPVTIM   DS    0X                  OPTION VALUES FOR TIME SHEET                 
OPVTIMMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVTIMNM DS     XL1                     # OF VALUES SO FAR                      
OPVTIMTB DS     (OPVTIMMX)CL3           TABLE TO HOLD VALUES                    
OPVTIMQ  EQU   *-OPVTIM                                                         
                                                                                
OPVAFF   DS    0X                  OPTION VALUES FOR AFFILIATION                
OPVAFFMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVAFFNM DS     XL1                     # OF VALUES SO FAR                      
OPVAFFTB DS     (OPVAFFMX)CL4           TABLE TO HOLD VALUES                    
OPVAFFQ  EQU   *-OPVAFF                                                         
                                                                                
OPVTYP   DS    0X                  OPTION VALUES FOR STATION TYPE               
OPVTYPMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVTYPNM DS     XL1                     # OF VALUES SO FAR                      
OPVTYPTB DS     (OPVTYPMX)CL1           TABLE TO HOLD VALUES                    
OPVTYPQ  EQU   *-OPVTYP                                                         
                                                                                
OPVEIX   DS    0X                  OPTION VALUES FOR EIX=Y/N                    
OPVEIXMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVEIXNM DS     XL1                     # OF VALUES SO FAR                      
OPVEIXTB DS     (OPVEIXMX)CL1           TABLE TO HOLD VALUES                    
OPVEIXQ  EQU   *-OPVEIX                                                         
                                                                                
OPVBKT   DS    0X                  OPTION VALUES FOR BKT=                       
OPVBKTMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVBKTNM DS     XL1                     # OF VALUES SO FAR                      
OPVBKTTB DS     (OPVBKTMX)CL2           TABLE TO HOLD VALUES                    
OPVBKTQ  EQU   *-OPVBKT                                                         
                                                                                
OPVUID   DS    0X                  OPTION VALUES FOR BKT=                       
OPVUIDMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVUIDNM DS     XL1                     # OF VALUES SO FAR                      
OPVUIDTB DS     (OPVUIDMX)CL6           TABLE TO HOLD VALUES                    
OPVUIDQ  EQU   *-OPVUID                                                         
                                                                                
OPVKMK   DS    0X                  OPTION VALUES FOR MARKET                     
OPVKMKMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVKMKNM DS     XL1                     # OF VALUES SO FAR                      
OPVKMKTB DS     (OPVKMKMX)CL4           TABLE TO HOLD VALUES                    
OPVKMKQ  EQU   *-OPVKMK                                                         
                                                                                
OPVSHW   DS    0X                  OPTION VALUES FOR SHOW=                      
OPVSHWMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVSHWNM DS     XL1                     # OF VALUES SO FAR                      
OPVSHWTB DS     (OPVSHWMX)CL1           TABLE TO HOLD VALUES                    
OPVSHWQ  EQU   *-OPVSHW                                                         
                                                                                
OPVLOC   DS    0X                  OPTION VALUES FOR LOCK=                      
OPVLOCMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVLOCNM DS     XL1                     # OF VALUES SO FAR                      
OPVLOCTB DS     (OPVLOCMX)CL1           TABLE TO HOLD VALUES                    
OPVLOCQ  EQU   *-OPVLOC                                                         
                                                                                
OPVMID   DS    0X                  OPTION VALUES FOR MIDAS=                     
OPVMIDMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVMIDNM DS     XL1                     # OF VALUES SO FAR                      
OPVMIDTB DS     (OPVMIDMX)CL1           TABLE TO HOLD VALUES                    
OPVMIDQ  EQU   *-OPVMID                                                         
                                                                                
OPVBND   DS    0X                  OPTION VALUES FOR BAND=                      
OPVBNDMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVBNDNM DS     XL1                     # OF VALUES SO FAR                      
OPVBNDTB DS     (OPVBNDMX)CL1           TABLE TO HOLD VALUES                    
OPVBNDQ  EQU   *-OPVBND                                                         
                                                                                
OPTVALSQ EQU   *-OPTVALS                                                        
         EJECT                                                                  
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   1024-MYSSPREL         AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (TWA DSECTS)'             
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*------------------------- MASTER/LIST SCREEN ------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMECD                                                       
         EJECT                                                                  
*------------------------- SAVED STORAGE AREA ------------------------*         
         ORG                                                                    
*                                                                               
MYTWAL   EQU   *-CONHEADH                                                       
*------------------------------ DDGENTWA -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (OTHER DSECTS)'           
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* DDDICTATED                                                                    
* SPMSGEQUS                                                                     
* SPDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (SPGEN DSECTS)'           
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*------------------------------ SPGENSTA -----------------------------*         
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
*------------------------------ SPGENMKT -----------------------------*         
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
*----------------------------- SPGENANMK -----------------------------*         
                                                                                
       ++INCLUDE SPGENANMK                                                      
         EJECT                                                                  
*------------------------------ SPSTABLK -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE SPSTABLK                                                       
         PRINT ON                                                               
*----------------------------- DDOFFICED -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED         FOR OFFICER                                  
         PRINT ON                                                               
*----------------------------- DEDEMTABD -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD         FOR DEMO TABLES (2 CHAR BOOKTYPE)            
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM39 - SPOT MASTER-RECORDS LISTING (MISC DSECTS)'            
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
*----------------------- OPTION KEYWORDS DSECT -----------------------*         
                                                                                
OPTDSECT DSECT                                                                  
OPDNUMB  DS    XL1                 INTERNAL OPTION NUMBER                       
OPDNTRYL DS    XL1                 L(OPTION KEYWORD ENTRY)                      
OPDFLAG  DS    XL1                 FLAGS ABOUT KEYWORD                          
OPDFKYWD EQU    X80                 OPTION IS VALID BY KEYWORD                  
OPDFVRTN EQU    X40                 VALIDATE DATA VIA ROUTINE                   
OPDFVTBL EQU    X20                 VALIDATE DATA VIA TABLE                     
*                                    EG. AL1(3,1),CL3'YES',CL1'Y'               
*                                        AL1(2,1),CL3'NO',CL1'N'                
*                                        AL1(EOT)                               
                                                                                
OPDRTNUM DS    XL1                 VALIDATION ROUTINE # (OPDFVRTN)              
OPDRTFMT DS    XL1                 FORMATTING ROUTINE # (OPDFVRTN)              
         ORG   OPDRTNUM                                                         
OPDTBDSP DS    AL2                 VALIDATION TABLE DSPL (OPDFVTBL)             
         ORG                                                                    
                                                                                
OPDOPTB  DS    AL4                 OPTION'S BIT MASK                            
OPDRQOPT DS    AL4                 OTHER OPTIONS REQUIRED                       
OPDNAOPT DS    AL4                 OPTIONS NOT ALLOWED                          
OPDMXNDV DS    XL1                 MAX # OF DATA VALUES ALLOWED                 
OPDOLEN  DS    XL1                 OUTPUT ENTRY LENGTH                          
OPDODSPL DS    AL2                 DSPL FROM SYSD OF OUTPUT FIELD               
OPDKYTB  DS    AL2                 DSPL OF KEYWORD TABLE                        
OPDVLTB  DS    AL2                 DSPL OF VALUE TABLE                          
OPDFXLNQ EQU   *-OPTDSECT                                                       
         EJECT                                                                  
*----------------------------- LIST LINE -----------------------------*         
                                                                                
LISTD    DSECT                                                                  
LSTSTAT  DS    CL7                                                              
         DS    CL2                                                              
LSTCLNT  DS    CL3                                                              
         DS    CL5                                                              
LSTCODE  DS    CL4                                                              
         DS    CL5                                                              
LSTNAME  DS    CL24                                                             
         DS    CL2                                                              
LSTFORM  DS    CL4                                                              
         DS    CL4                                                              
LSTUID   DS    CL6                                                              
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071SPSFM39   01/16/15'                                      
         END                                                                    
