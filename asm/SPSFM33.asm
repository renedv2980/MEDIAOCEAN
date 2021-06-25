*          DATA SET SPSFM33    AT LEVEL 006 AS OF 08/07/07                      
*PHASE T21733A                                                                  
T21733   TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING'                          
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* AUG07/07 006 AKAT - USE AIO3 TO READ ALPHA MKT FROM CTFILE SINCE REC*         
*                   - CAN BE > 48 BYTES & WE WERE USING KEY (48 BYTES)*         
* SEP10/97 001 GLEE - REVAMPED PROGRAM FOR EFFICIENCY                 *         
*                   - ADDED NEW FILTERS TO MIMIC SAME FUNCTIONALITY   *         
*                      AS THE LATE SPOT/INFO PROGRAM                  *         
***********************************************************************         
         EJECT                                                                  
SFM33    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21733**,R7,RR=RE,CLEAR=YES                                    
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
                                                                                
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
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         L     R2,=A(DISPTAB-SFM33)                                             
         LA    R2,SFM33(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,SFM33            RE = BASE OF TABLE/ROUTINE                   
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
         L     RE,=A(DCLIST-SFM33)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
*                                                                               
         MVC   DCFILSPD,=C'SPTDIR  '                                            
         MVC   DCFILSPF,=C'SPTFIL  '                                            
         MVC   DCFILSTA,=C'STATION '                                            
*                                                                               
         XC    ACURFORC,ACURFORC                                                
*                                                                               
         MVI   ZEROES,C'0'                                                      
         MVC   ZEROES+1(L'ZEROES-1),ZEROES                                      
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
         MVI   MYTEXT,0                                                         
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SETFILE)'                
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (VALKEY)'                 
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VKEY     DS    0H                                                               
                                                                                
         MVC   SYSDIR,DCFILSPD                                                  
         MVC   SYSFIL,DCFILSPF                                                  
         MVC   LKEY,=H'13'                                                      
         MVI   USEIO,C'N'                                                       
*                                                                               
         XC    OPTR,OPTR           NO OPTIONS REQUIRED                          
         XC    OPTX,OPTX            NOR DIS-ALLOWED                             
*                                                                               
         XC    INPUTKEY(INPTKEYL),INPUTKEY                                      
*                                                                               
         NI    OPTFLAG,XFF-OPTLANMQ                                             
                                                                                
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,MKLMEDIH         CHECK FOR ANY MEDIA INPUT                    
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   IKMEDIA,QMED        REMEMBER MEDIA KEY INPUTTED                  
*                                                                               
*------------------------------- MARKET ------------------------------*         
*                                                                               
         LA    R2,MKLMRKTH         MARKET FIELD                                 
         CLI   5(R2),0             CHECK FOR INPUT IN FIELD                     
         BH    VKM012               THERE IS INPUT                              
         MVC   IKNMKT,SPACES        NO INPUT--LIST ALL NUMERIC MKTS             
         B     VKM089                                                           
*                                                                               
VKM012   DS    0H                 VALIDATE INPUT                                
         TM    4(R2),X04           IF INPUT IS ALPHABETICS                      
         BO    VKM020                                                           
         TM    4(R2),X08           IF INPUT IS NUMERIC                          
         BO    VKM030                                                           
         B     INVLMKT                                                          
*                                                                               
** VALIDATE INPUT AS AN ALPHA MARKET **                                         
*                                                                               
VKM020   DS    0H                                                               
         CLI   5(R2),2              INPUT CAN'T BE LESS THAN 2                  
         BL    INVLMKT                                                          
         CLI   5(R2),3               GREATER THAN 3                             
         BH    INVLMKT                                                          
*                                                                               
         DS    0H                  VALIDATE ALPHA MKT AGAINST FILE              
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R6,KEYSAVE                                                       
         USING CTDMREC,R6                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       USE MEDIA 'T'                                
         CLI   IKMEDIA,C'C'         IF INPUTTED MEDIA IS 'C'                    
         BE    VKM024C                                                          
         CLI   IKMEDIA,C'N'         IF INPUTTED MEDIA IS 'N'                    
         BE    VKM024C                                                          
         MVC   CTDMKMED,IKMEDIA    ELSE, USE INPUTTED MEDIA                     
VKM024C  EQU   *                                                                
         MVC   CTDMKMKT,8(R2)      ALPHA MARKET                                 
         OC    CTDMKMKT,SPACES                                                  
*                                                                               
         L     R5,AIO3                                                          
         MVI   CTDMKSRC,C'N'       TRY NIELSEN FIRST                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEYSAVE,AIO3               
         CLC   CTDMKEY(CKYMKTL),0(R5)                                           
         BE    VKM024X                                                          
*                                                                               
         MVI   CTDMKSRC,C'A'       TRY ARBITRON NEXT                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEYSAVE,AIO3               
         CLC   CTDMKEY(CKYMKTL),0(R5)                                           
         BE    VKM024X                                                          
*                                                                               
         B     MKTNOFIL            ALPHA MARKET NOT ON FILE                     
VKM024X  EQU   *                                                                
         MVC   IKAMKT,CTDMKMKT     REMEMBER ALPHA MARKET INPUTTED               
         OI    OPTFLAG,OPTLANMQ    FLAG TO LIST BY ALPHA MARKETS                
         OC    OPTX,=AL4(ALLFILTS) DISALLOW ALL FILTERS FOR ALPHA MKT           
         DROP  R6                                                               
         B     VKM089                                                           
*                                                                               
** VALIDATE INPUT AS A NUMERIC MARKET **                                        
*                                                                               
VKM030   DS    0H                                                               
         LLGC  R1,5(R2)            R1 = L(INPUT)                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         EDIT  (R1),(L'IKNMKT,IKNMKT),FILL=0    LEFT-PAD W/ ZEROES              
         B     VKM089                                                           
*                                                                               
VKM089   EQU   *                                                                
*                                                                               
*-------------------------- VALIDATE OPTIONS -------------------------*         
*                                                                               
         LA    R2,MKLOPTNH         CHECK FOR OPTIONS INPUTTED                   
         BAS   RE,VALOPT                                                        
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
*                                                                               
VKO029   DS    0H                  CHECK MAX FILTERS ALLOWED                    
         MVC   DUB(4),OPTI                                                      
         NC    DUB(4),=AL4(ALLFILTS)                                            
         BZ    VKO045X              SKIP IF NO FILTERS ENTERRED                 
         L     RE,DUB                                                           
         SRDL  RE,32                RF CONTAINS INPUTTED FILTERS                
         LA    R0,1                 MAX # OF FILTERS ALLOWED                    
*                                                                               
VKO045B  DS    0H                                                               
         SLDL  RE,1                 SHIFT ONE FLAG OVER TO RE                   
         SR    R0,RE                                                            
         BM    MANYFLT              IF NEG, # FILT INPUT > # FILT ALLOW         
         OR    RF,RF                ANY MORE FILTERS ENTERRED?                  
         BNZ   VKO045B               YEP                                        
*                                                                               
VKO045X  B     VK100               GO BUILD KEY                                 
         EJECT                                                                  
*                                                                               
*--------------------- MISCELLANEOUS VALKEY TASKS --------------------*         
*                                                                               
VK100    DS    0H                 BUILD KEYS                                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         OC    IKNMKT,IKNMKT       LIST BY NUMERIC MARKET?                      
         BZ    VK120                NO                                          
                                                                                
*                                                                               
         DS    0H                  BUILD KEY OF MRKT RECORD (TYPE -'M')         
         USING MKTRECD,R6                                                       
         MVI   MKTKTYPE,MKTKTYPQ    MARKET RECORDS ARE TYPE-'M'                 
         MVC   MKTKMED,IKMEDIA      MEDIA                                       
         MVC   MKTKMKT,IKNMKT       NUMERIC MARKET CODE                         
         MVC   MKTKAGY,AGENCY       AGENCY                                      
         DROP  R6                                                               
         B     VKX                                                              
                                                                                
*                                                                               
VK120    DS    0H                  BUILD KEY OF A/N RECORD (TYPE -'N')          
         USING ANMRECD,R6                                                       
         MVI   ANMKTYPE,ANMKTYPQ    A/N RECORDS ARE TYPE-'N'                    
         MVC   ANMKAGCY,AGENCY      AGENCY                                      
         MVC   ANMKMED,IKMEDIA      MEDIA                                       
         MVC   ANMKAMRK,IKAMKT      ALPHA MARKET                                
         DROP  R6                                                               
         B     VKX                                                              
                                                                                
*                                                                               
*---------------------------- VALKEY EXITS ---------------------------*         
*                                                                               
VKX      DS    0H                                                               
         MVC   MYSVKEY(MKTKEYLQ),KEY   SAVE KEY AROUND                          
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
         LLGC  R1,0(RF)                                                         
         LLGC  RE,0(R3)            RE = LENGTH OF INPUTTED KEYWORD              
         CR    R1,RE               L(KYWD IN TABLE) VS L(KYWD INPUTTED)         
         BL    VOPT033              LOW: TRY ANOTHER NAME                       
         BCTR  RE,0                                                             
         EXCLC RE,12(R3),1(RF)     IF THEY MATCH,                               
         BE    VOPT040              THEN THIS IS THE OPTION ENTRY               
                                                                                
VOPT033  DS    0H                                                               
         LA    RF,1(R1,RF)         ELSE, CHECK NEXT NAME IN TABLE               
         B     VOPT030                                                          
                                                                                
VOPT036  DS    0H                  BUMP TO NEXT OPTION ENTRY                    
         LLGC  R1,OPDNTRYL                                                      
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
         LLGC  R1,0(R5)            R1 = # OF VALUES FOR OPTION SO FAR           
         CLM   R1,1,OPDMXNDV       DO WE HAVE MAX AMOUNT ALREADY                
         BL    VOPT062                                                          
         MVI   OURERRCD,TMODQ       YES, TOO MANY OPTN DATA ERROR               
         B     VOPTXN                                                           
                                                                                
VOPT062  DS    0H                                                               
         LA    R0,1(R1)                                                         
         STC   R0,0(R5)            UPDATE # OF VALUES FOR OPTION                
         LLGC  RF,OPDOLEN          RF = L(OPTION OUTPUT)                        
         SR    R0,R0                                                            
         MR    R0,RF                                                            
         LA    R0,1(R1,R5)         SET DESTINATION ADDRESS                      
         LR    R1,RF               SET DESTINATION LENGTH                       
*                                                                               
         ZICM  R5,OPDVLTB,(3)                                                   
         LA    R5,OPTDSECT(R5)     R5-->VALUE(S) FOR KEYWORD                    
         LA    RE,1(R5)            SET SOURCE ADDRESS                           
         LLGC  RF,0(R5)            SET SOURCE LENGTH                            
                                                                                
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
         LLGC  R0,OPDOLEN          R0 = OUTPUT LENGTH                           
         ZICM  R5,OPDODSPL,(3)                                                  
         LA    R5,SYSD(R5)         R5-->OUTPUT AREA FOR OPTION DATA             
         LLGC  RE,0(R5)            RE = # OF OUTPUT VALUES SO FAR               
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
         LLGC  R1,0(R5)                                                         
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
         LLGC  RF,1(R2)             RF = L(VALUE TO STORE)                      
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
         LLGC  RE,0(R5)            RE = # OF DATA VALUES SO FAR                 
         LA    R1,1(RE)                                                         
         STC   R1,0(R5)             UPDATE IT                                   
         LLGC  R1,OPDOLEN          R1 = L(OPTION DATA VALUE)                    
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
                                                                                
         LLGC  R1,COUNTER          ANY MORE SCANNER ENTRIES?                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,NOPTN                                                       
         BNL   VOPT200              NOPE, FINISHED W/ OPTIONS                   
         STC   R1,COUNTER                                                       
         LLGC  R1,SCANLNTH                                                      
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
         LLGC  R0,OPDNTRYL          NO, TRY NEXT ENTRY                          
         AR    R4,R0                                                            
         B     VOPT212                                                          
                                                                                
VOPT214  DS    0H                  MOVE KEYWORD LEN & NAME INTO MYTEXT          
         ZICM  RF,OPDKYTB,(3)                                                   
         LA    RF,OPTDSECT(RF)                                                  
         LLGC  RE,0(RF)                                                         
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (VALIDATION ERROR+        
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
                                                                                
INVLMKT  DS    0H                  INVALID MARKET                               
         MVI   OURERRCD,IMKTQ                                                   
         B     OURERROR                                                         
                                                                                
MKTNOFIL DS    0H                  MARKET NOT ON FILE                           
         MVI   OURERRCD,MKNOFQ                                                  
         B     OURERROR                                                         
                                                                                
MANYFLT  DS    0H                  TOO MANY FILTERS IN FIELD                    
         MVI   OURERRCD,TMFQ                                                    
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (LISTRECS)'               
***********************************************************************         
*============================ LIST RECORD ============================*         
LREC     DS    0H                                                               
         BAS   RE,SFIL                                                          
                                                                                
*                                                                               
         DS    0H                 HEADING FOR OPTIONAL DATA COLUMN              
         XC    MKLODC1,MKLODC1     CLEAR AND TRANSMIT FIELDS                    
         OI    MKLODC1H+6,X80                                                   
         XC    MKLODC2,MKLODC2                                                  
         OI    MKLODC2H+6,X80                                                   
         XC    AODCNTRY,AODCNTRY                                                
         XC    AODCFRTN,AODCFRTN                                                
*                                                                               
         CLI   OPVODCTB,0          ANY OPTIONAL DATA TO DISPLAY?                
         BE    LR05X                NOPE!                                       
*                                                                               
         DS    0H                  LOCATE ENTRY                                 
         L     R5,AODCTAB                                                       
         USING ODCTABD,R5                                                       
                                                                                
LR05B    DS    0H                                                               
         CLI   0(R5),EOT           SOMETHING BAD IF EOT REACHED                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ODCDNUM,OPVODCTB    COMPARE ON ENTRY NUMBER                      
         BE    LR05G                                                            
         LLGC  R0,ODCDLEN                                                       
         AR    R5,R0                                                            
         B     LR05B                                                            
LR05G    EQU   *                                                                
*                                                                               
         DS    0H                  R5-->ENTRY FOR OPTIONAL DATA                 
         ST    R5,AODCNTRY          SAVE ADDRESS OF ENTRY                       
         ZICM  RF,ODCDFRTN,(3)                                                  
         LA    RF,SFM33(RF)                                                     
         ST    RF,AODCFRTN          GET ADDRESS OF FORMAT ROUTINE               
                                                                                
         LLGC  R1,ODCDLBLN                                                      
         CLI   ODCDLBLN,L'MKLODC1                                               
         BNH   *+8                                                              
         LA    R1,L'MKLODC1                                                     
         BCTR  R1,0                R1 = LENGTH FOR EXMVC                        
                                                                                
         LLGC  RF,ODCDLBDS                                                      
         LA    RF,ODCTABD(RF)      RF-->OPTIONAL DATA COLUMN LABEL              
         EXMVC R1,MKLODC1,0(RF)                                                 
         EXMVC R1,MKLODC2,DASHES                                                
         DROP  R5                                                               
LR05X    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         USING LISTD,R2                                                         
         LA    R2,LISTAR                                                        
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    *+8                                                              
         LA    R2,P                                                             
                                                                                
*                                                                               
         OC    IKNMKT,IKNMKT       LIST BY NUMERIC-MARKETS?                     
         BZ    LAR                  NO, GO LIST ALPHA RECORDS.                  
                                                                                
*                                                                               
         DS    0H                                                               
         OC    KEY(MKTKEYLQ),KEY   FIRST TIME THROUGH?                          
         BNZ   *+10                NO, READ WHERE LEFT OFF.                     
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
         GOTO1 HIGH                                                             
         B     LR20                NO, READ WHERE LEFT OFF.                     
*                                                                               
LR10     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(MKYMEDL),MYSVKEY  IS THIS WHAT WE'RE LOOKING FOR?            
         BNE   LRX                    NO, EXIT LISTING                          
         CLC   KEY+(MKTKAGY-MKTRECD)(L'MKTKAGY),MYSVKEY+(MKTKAGY-MKTREC+        
               D)                                                               
         BNE   LR10                AGENCY DOESN'T MATCH...NEXT RECORD.          
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,LRMKTFLT         RUN RECORD THROUGH FILTER                    
         BNE   LR10                                                             
*                                                                               
         MVI   DFDATA,C' '                                                      
         MVC   DFDATA+1(DFDATAL-1),DFDATA                                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVC   DFMRKT,MKTKMKT      MARKET NUMBER                                
         MVC   DFMKTNAM,MKTNAME    MARKET NAME                                  
         MVC   DFAMKT,MKTALST      LIST OF ALPHA MARKETS                        
         DROP  R6                                                               
                                                                                
         MVI   GOSUBN,PTINF#                                                    
         GOTO1 AGOSUB                                                           
*                                                                               
         AF    LISTCNTR,=F'1'                                                   
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   LR30                                                             
         GOTO1 LISTMON                                                          
         B     LR40                                                             
*                                                                               
LR30     DS    0H                                                               
         GOTO1 CATCHIOS            PRINT RECORDS                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR40     DS    0H                                                               
         B     LR10                                                             
         EJECT                                                                  
*                                                                               
** LIST BY ALPHA MARKET **                                                      
*                                                                               
LAR      DS    0H                                                               
         OC    KEY(ANMKEYLQ),KEY   FIRST TIME THROUGH?                          
         BNZ   *+10                 NO, READ WHERE LEFT OFF.                    
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR.               
         GOTO1 HIGH                                                             
         B     LAR20                                                            
*                                                                               
LAR10    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
LAR20    DS    0H                                                               
         CLC   KEY(AKYAMKL),MYSVKEY                                             
         BNE   LRX                                                              
*                                                                               
         MVI   DFDATA,C' '                                                      
         MVC   DFDATA+1(DFDATAL-1),DFDATA                                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     R4,AIO                                                           
         USING ANMRECD,R4                                                       
*                                                                               
         MVC   MYKEY,ZEROES                                                     
         LA    R6,MYKEY                                                         
         USING MKTRECD,R6                                                       
         MVI   MKTKTYPE,MKTKTYPQ   MARKET RECORDS ARE TYPE-'M'.                 
         MVC   MKTKMED,ANMKMED     MEDIA                                        
         MVC   MKTKMKT,ANMKNMRK    NUMERIC MARKET-CODE.                         
         MVC   MKTKAGY,ANMKAGCY    AGENCY.                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'STATION',MYKEY,AIO2                 
*                                                                               
         L     R6,AIO2                                                          
         USING MKTRECD,R6                                                       
         CLC   MKTKEY,MYKEY        IF RECORD FOUND,                             
         BE    LAR45X               MOVE ON                                     
         LA    R1,MKTKEYLQ+L'MKTRECL+L'MKTCNTL                                  
         LA    R0,MKTREC(R1)                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               ELSE, CLEAR DATA AREA OF RECORD              
         MVC   MKTNAME,MESNOF       FUDGE IN MESSAGE FOR MKT NAME               
LAR45X   EQU   *                                                                
*                                                                               
         MVC   DFMRKT,ANMKNMRK     NUMERIC MARKET-CODE                          
         MVC   DFMKTNAM,MKTNAME    MARKET NAME                                  
         MVC   DFAMKT,MKTALST      LIST OF ALPHA MARKETS                        
         DROP  R6                                                               
                                                                                
         MVI   GOSUBN,PTINF#       PUT INFO ONTO LIST LINE                      
         GOTO1 AGOSUB                                                           
*                                                                               
         GOTO1 READ                RESTORE DATAMGR'S READ.                      
         AF    LISTCNTR,=F'1'                                                   
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   LAR40                                                            
         GOTO1 LISTMON                                                          
         B     LAR50                                                            
*                                                                               
LAR40    DS    0H                                                               
         GOTO1 CATCHIOS            PRINT RECORDS                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LAR50    DS    0H                                                               
         B     LAR10                                                            
*                                                                               
         DROP  R4                                                               
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
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*------------------------ MARKET RECORD FILTER -----------------------*         
                                                                                
* AT ENTRY,                                                                     
*   R6-->MARKET RECORD                                                          
* AT EXIT,                                                                      
*   CC SET TO EQUAL IF RECORD PASSED FILTER TEST                                
*   CC SET TO NOT EQUAL OTHERWISE                                               
                                                                                
LRMKTFLT NTR1                                                                   
         USING MKTRECD,R6                                                       
*                                                                               
         DS    0H                  FILTER ON TIME ZONE                          
         CLI   OPVTZNM,0                                                        
         BE    LRMFTZX                                                          
         CLC   MKTZONE,OPVTZTB                                                  
         BNE   LRMFXN                                                           
LRMFTZX  EQU   *                                                                
*                                                                               
         DS    0H                  FILTER ON SWEEP PERIOD                       
         CLI   OPVSWPNM,0                                                       
         BE    LRMFSWPX                                                         
         CLC   MKTCLAS1,OPVSWPTB   MATCH ON EITHER RATING SERVICE CLASS         
         BE    LRMFSWPX                                                         
         CLC   MKTCLAS2,OPVSWPTB                                                
         BNE   LRMFXN                                                           
LRMFSWPX EQU   *                                                                
*                                                                               
         DS    0H                                                               
         OC    OPUNTNM(2),OPUNTNM  UNIT NAME?                                   
         BE    LRMFUNTX            NO                                           
         CLC   MKTUNIT,OPUNTTB     UNITS MATCH UNIT FILTER?                     
         BE    LRMFUNTX            YES                                          
***                                                                             
* SOMEONE CHANGED THOUSNDS OF MARKET RECORDS TO HAVE THE UNIT NUMBER            
* 3856 OR -3856.  WE NOW NEED TO TEST FOR THE NEGATIVE UNIT NUMBER              
* INPUT SO THAT THEY CAN FIND (AND CHANGE) THESE BAD UNITS THEMSELVES           
***                                                                             
         SR    R1,R1                                                            
         ICM   R1,3,OPUNTTB                                                     
         LCR   R1,R1                                                            
         STCM  R1,3,HALF                                                        
         CLC   MKTUNIT,HALF                                                     
         BNE   LRMFXN                                                           
LRMFUNTX EQU   *                                                                
*                                                                               
         DS    0H                                                               
         B     LRMFXY              RECORD PASSED FILTER TEST                    
         DROP  R6                                                               
*                                                                               
** EXITS **                                                                     
*                                                                               
LRMFXN   DS    0H                                                               
         B     NO                                                               
*                                                                               
LRMFXY   DS    0H                                                               
         B     YES                                                              
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (MISCELLANEOUS)'          
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
                                                                                
         SSPEC H1,34,C'MARKET LIST'                                             
         SSPEC H2,34,C'-----------'                                             
                                                                                
         SSPEC H4,1,C'MKT CODE'                                                 
         SSPEC H4,12,C'MARKET NAME'                                             
         SSPEC H4,39,C'ALPHA MKTS'                                              
                                                                                
         SSPEC H5,1,C'--------'                                                 
         SSPEC H5,12,C'-----------'                                             
         SSPEC H5,39,C'----------'                                              
                                                                                
         DC    X'00'                                                            
                                                                                
                                                                                
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (LTORG && CONSTAN+        
               TS)'                                                             
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
MESNOF   DC    C'** MARKET NOT ON FILE **'                                      
                                                                                
                                                                                
MAINL    EQU   *-SFM33                                                          
         DS    0CL(X'2000'-MAINL+1)                                             
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01)'                 
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   SFM33+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-SFM33+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   SFM33+SUBR01Q                                                    
SUBR01   NMOD1 0,**3901**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R9             R9=A(SYSD)                                   
         USING SPOOLD,R8           R8=A(SPOOL WORK AREA)                        
                                                                                
         LLGC  R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
IOV#     EQU   (R01_01-*)/4+1      INITIALIZE OPTIONS VALUES                    
TSL#     EQU   (R01_02-*)/4+1      TEST SELECT FIELD                            
PTINF#   EQU   (R01_03-*)/4+1      PUT INFO                                     
VTZ#     EQU   (R01_04-*)/4+1      VALIDATE TIME ZONE (OPTIONS FIELD)           
VSWP#    EQU   (R01_05-*)/4+1      VALIDATE SWEEP (OPTIONS FIELD)               
VODC#    EQU   (R01_06-*)/4+1      VALIDATE DATA  (OPTIONS FIELD)               
VUNIT#   EQU   (R01_07-*)/4+1      VALIDATE UNIT (OPTIONS FIELD)                
                                                                                
R01_00   DS    0H                                                               
R01_01   B     INITOPTV            INITIALIZE OPTIONS VALUES                    
R01_02   B     TESTSEL             TEST SELECT FIELD                            
R01_03   B     PUTINFO             PUT INFO                                     
R01_04   B     VALTMZN             VALIDATE TIME ZONE (OPTIONS FIELD)           
R01_05   B     VALSWEEP            VALIDATE SWEEP (OPTIONS FIELD)               
R01_06   B     VALODC              VALIDATE DATA  (OPTIONS FIELD)               
R01_07   B     VALUNIT             VALIDATE DATA  (OPTIONS FIELD)               
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--IOV#)'           
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--TSL#)'           
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
TESTSEL1 LA    R2,MKLSELH          R2-->FIRST SELECT FIELD                      
         LA    RF,MKLTAGH          RF-->LAST FIELD                              
*                                                                               
TESTSEL2 LLGC  R1,0(R2)                                                         
         CLI   8(R2),C'C'          CHANGE FROM LIST SCREEN                      
         BE    TSLXN                                                            
         CLI   9(R2),C'C'                                                       
         BE    TSLXN                                                            
         CLI   10(R2),C'C'                                                      
         BE    TSLXN                                                            
*                                                                               
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         LLGC  R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    RF,R2                                                            
         BH    TESTSEL2                                                         
                                                                                
*                                                                               
TSLXN    DS    0H                                                               
         B     NO_01                                                            
*                                                                               
TSLXY    DS    0H                                                               
         B     YES_01                                                           
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--PTINF#)'         
*-------------------------- PUT INFORMATION --------------------------*         
                                                                                
* AT ENTRY,                                                                     
*   R2-->AREA TO PUT INFORMATION                                                
*   R6-->MARKET RECORD                                                          
                                                                                
PUTINFO  DS    0H                                                               
         LA    R1,L'LISTAR                                                      
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    *+8                                                              
         LA    R1,L'P                                                           
                                                                                
         DS    0H                  CLEAR OUTPUT AREA                            
         LR    RF,R2                                                            
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         BCT   R1,*-8                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         USING LISTD,R2                                                         
         MVC   LSTNCDE,DFMRKT                                                   
         MVC   LSTNAME,DFMKTNAM                                                 
*                                                                               
         DS    0H                 ALPHA MARKETS                                 
         LA    R3,LSTAMKTS         R3-->LIST AREA FOR ALPHA MKTS                
         LA    R5,DFAMKT           R5-->LIST OF ALPHA MARKETS                   
         LA    R1,3                LIST MAX OF 3 ALPHA MKTS                     
*                                                                               
PUT032   DS    0H                                                               
         OC    0(3,R5),0(R5)       ANY ALPHA MARKETS LEFT?                      
         BZ    PUT035               NO                                          
                                                                                
         MVC   0(3,R3),0(R5)       MOVE ONE ALPHA MKT AT A TIME                 
         LA    R3,3(R3)                                                         
         CLI   2(R5),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                REMOVE ANY TRAILING BLANKS                   
                                                                                
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)                                                         
         BCT   R1,PUT032                                                        
*                                                                               
PUT035   DS    0H                                                               
         BCTR  R3,0                                                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '          REMOVE LAST COMMA FROM LIST                  
                                                                                
*                                                                               
         DS    0H                  FORMAT OPTIONAL DATA INTO COLUMN             
         ICM   RF,15,AODCFRTN                                                   
         BZ    PUT049                                                           
         MVC   ODCDATA,SPACES                                                   
         BR    RF                                                               
*                                                                               
PUT045   DS    0H                  RETURN HERE AFTER FORMAT ROUTINES            
         MVC   LSTODC,ODCDATA                                                   
*                                                                               
PUT049   EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
XPUTINF  DS    0H                                                               
         B     XIT_01                                                           
         EJECT                                                                  
*                                                                               
** OPTIONAL DATA FORMATTING ROUTINES **                                         
*                                                                               
         USING MKTRECD,R6                                                       
PUT_TZ   DS    0H                                                               
         MVC   ODCDATA(L'MKTZONE),MKTZONE                                       
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_SWP  DS    0H                                                               
         MVC   ODCDATA(L'MKTCLAS1),MKTCLAS1                                     
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_RNK  DS    0H                                                               
         MVC   ODCDATA(L'MKTRANK),MKTRANK                                       
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_HMS  DS    0H                                                               
         MVC   ODCDATA(L'MKTHOMES),MKTHOMES                                     
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_NTA  DS    0H                                                               
         MVC   ODCDATA(L'MKTNTA),MKTNTA                                         
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_WT   DS    0H                                                               
         EDIT  (C4,MKTWT),(5,ODCDATA),2                                         
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_SHR  DS    0H                                                               
         EDIT  (C4,MKTSHR),(5,ODCDATA),2                                        
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_NSI  DS    0H                  DATA=NSI  OR  DATA=CSI                       
         CLI   MKTRS1,C'0'                                                      
         BNE   PUT_NSI4                                                         
                                                                                
PUT_NSI2 DS    0H                                                               
         EDIT  (B2,MKTRSM1),(5,ODCDATA)                                         
         B     PUT045                                                           
                                                                                
PUT_NSI4 DS    0H                                                               
         CLI   MKTRS2,C'0'                                                      
         BNE   PUT045                                                           
                                                                                
PUT_NSI6 DS    0H                                                               
         EDIT  (B2,MKTRSM2),(5,ODCDATA)                                         
         B     PUT045                                                           
*                                                                               
PUT_UNIT DS    0H                                                               
         MVC   ODCDATA(L'MKTUNIT),MKTUNIT                                       
         B     PUT045                                                           
                                                                                
*                                                                               
PUT_ARB  DS    0H                  DATA=ARB  OR  DATA=BBM                       
         CLI   MKTRS1,C'1'                                                      
         BE    PUT_NSI2                                                         
         CLI   MKTRS2,C'1'                                                      
         BE    PUT_NSI6                                                         
         B     PUT045                                                           
         DROP  R6                                                               
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--VTZ#)'           
*------------------------- VALIDATE TIME ZONE ------------------------*         
                                                                                
* VALIDATES AN INPUTTED TIME ZONE INPUTTED IN THE OPTIONS FIELD.                
* AT ENTRY,                                                                     
*   R3-->SCANNER ENTRY W/ TIME ZONE IN 2ND HALF,                                
*   R4-->ENTRY OF OPTION KEYWORD FOR TIME ZONE.                                 
* ON EXIT WHEN TIME ZONE VALID,                                                 
*   CC SET TO EQUAL,                                                            
*   BLOCK CONTAINS OPTION DATA TO BE STORED.                                    
* ON EXIT WHEN TIME ZONE INVALID,                                               
*   CC SET TO NOT EQUAL,                                                        
*   OURERRCD = APPROPRIATE ERROR CODE,                                          
*   FLDDSPL  = DISPLACEMENT OF ERROR INPUT,                                     
*   MYTEXT   = TEXT FOR TEXT REPLACE (IF THERE ARE ANY).                        
* NOTE: DO NOT CLOBBER BUFF HERE!                                               
                                                                                
VALTMZN  DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(TIME ZONE INPUTTED)                  
         BH    VTZXN                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         TM    3(R3),X80           TEST NUMERIC                                 
         BZ    VTZXN                                                            
         CLI   22(R3),C'1'                                                      
         BL    VTZXN                                                            
         CLI   22(R3),C'4'                                                      
         BH    VTZXN                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   BLOCK(L'OPVTZTB),22(R3)                                          
         B     VTZXY                                                            
                                                                                
*                                                                               
** VALIDATE TIME ZONE EXITS **                                                  
*                                                                               
VTZXY    DS    0H                                                               
         B     YES_01                                                           
VTZXN    DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--UNIT#)'          
*------------------------- VALIDATE UNIT -----------------------------*         
                                                                                
* VALIDATES AN INPUTTED UNIT IN THE OPTIONS FIELD.                              
* AT ENTRY,                                                                     
*   R3-->SCANNER ENTRY W/ UNIT IN 2ND HALF,                                     
*   R4-->ENTRY OF OPTION KEYWORD FOR UNIT                                       
* ON EXIT WHEN TIME ZONE VALID,                                                 
*   CC SET TO EQUAL,                                                            
*   BLOCK CONTAINS OPTION DATA TO BE STORED.                                    
* ON EXIT WHEN UNIT INVALID,                                                    
*   CC SET TO NOT EQUAL,                                                        
*   OURERRCD = APPROPRIATE ERROR CODE,                                          
*   FLDDSPL  = DISPLACEMENT OF ERROR INPUT,                                     
*   MYTEXT   = TEXT FOR TEXT REPLACE (IF THERE ARE ANY).                        
* NOTE: DO NOT CLOBBER BUFF HERE!                                               
                                                                                
VALUNIT  DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         TM    3(R3),X'80'         UNIT NUMERIC?                                
         BZ    VUNITXN             NO, ERROR                                    
*                                                                               
         CLI   1(R3),1             HAVE ANY INPUT?                              
         BL    VUNITXN             NO, ERROR                                    
         CLI   1(R3),4             INPUT GREATER THAN 4?                        
         BH    VUNITXN             YES, ERROR                                   
*                                                                               
         MVC   BLOCK(2),10(R3)                                                  
*                                                                               
VUNITXY  DS    0H                                                               
         B     YES_01                                                           
VUNITXN  DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--VSWP#)'          
*--------------------------- VALIDATE SWEEP --------------------------*         
                                                                                
* VALIDATES AN INPUTTED SWEEP INPUTTED IN THE OPTIONS FIELD.                    
* AT ENTRY,                                                                     
*   R3-->SCANNER ENTRY W/ SWEEP IN 2ND HALF,                                    
*   R4-->ENTRY OF OPTION KEYWORD FOR SWEEP.                                     
* ON EXIT WHEN SWEEP VALID,                                                     
*   CC SET TO EQUAL,                                                            
*   BLOCK CONTAINS OPTION DATA TO BE STORED.                                    
* ON EXIT WHEN SWEEP INVALID,                                                   
*   CC SET TO NOT EQUAL,                                                        
*   OURERRCD = APPROPRIATE ERROR CODE,                                          
*   FLDDSPL  = DISPLACEMENT OF ERROR INPUT,                                     
*   MYTEXT   = TEXT FOR TEXT REPLACE (IF THERE ARE ANY).                        
* NOTE: DO NOT CLOBBER BUFF HERE!                                               
                                                                                
VALSWEEP DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
         CLI   1(R3),1             CHECK L(SWEEP INPUTTED)                      
         BH    VSWPXN                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         TM    3(R3),X80           TEST NUMERIC                                 
         BZ    VSWPXN                                                           
         CLI   22(R3),C'1'                                                      
         BL    VSWPXN                                                           
         CLI   22(R3),C'8'                                                      
         BH    VSWPXN                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   BLOCK(L'OPVTZTB),22(R3)                                          
         B     VSWPXY                                                           
                                                                                
*                                                                               
** VALIDATE SWEEP EXITS **                                                      
*                                                                               
VSWPXY   DS    0H                                                               
         B     YES_01                                                           
VSWPXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--VODC#)'          
*--------------------------- VALIDATE DATA ---------------------------*         
                                                                                
* VALIDATES AN OPTIONAL DATA INPUTTED IN THE OPTIONS FIELD.                     
* AT ENTRY,                                                                     
*   R3-->SCANNER ENTRY W/ OPTIONAL DATA NAME IN 2ND HALF,                       
*   R4-->ENTRY OF OPTION KEYWORD FOR OPTIONAL DATA.                             
* ON EXIT WHEN OPTIONAL DATA NAME VALID,                                        
*   CC SET TO EQUAL,                                                            
*   BLOCK CONTAINS OPTION DATA TO BE STORED.                                    
* ON EXIT WHEN OPTIONAL DATA NAME INVALID,                                      
*   CC SET TO NOT EQUAL,                                                        
*   OURERRCD = APPROPRIATE ERROR CODE,                                          
*   FLDDSPL  = DISPLACEMENT OF ERROR INPUT,                                     
*   MYTEXT   = TEXT FOR TEXT REPLACE (IF THERE ARE ANY).                        
* NOTE: DO NOT CLOBBER BUFF HERE!                                               
                                                                                
VALODC   DS    0H                                                               
         MVI   OURERRCD,0                                                       
         MVI   MYTEXT,0                                                         
         MVC   FLDDSPL,8(R3)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   OURERRCD,IODVQ                                                   
                                                                                
*                                                                               
         DS    0H                                                               
         L     R5,AODCTAB          R5-->OPTIONAL DATA TABLE                     
         USING ODCTABD,R5                                                       
*                                                                               
VODC012  DS    0H                                                               
         CLI   0(R5),EOT           IF AT END OF TABLE,                          
         BE    VODCXN               THEN INPUT IS INVALID                       
*                                                                               
         DS    0H                                                               
         CLC   1(1,R3),ODCDNMLN    L(INPUT) VS. MAX L(OPTION NAME)              
         BH    VODC018              TOO HIGH, BUMP TO NEXT ENTRY                
                                                                                
         LLGC  R1,1(R3)            R1 = L(INPUT)                                
         BCTR  R1,0                R1 = LENGTH FOR EXCLC                        
         LLGC  RF,ODCDNMDS         RF = DSPL INTO ENTRY OF OPTN DATA NM         
         LA    RF,ODCTABD(RF)      RF-->OPTION DATA NAME                        
         EXCLC R1,22(R3),0(RF)     ANY MATCH W/ THIS ENTRY?                     
         BNE   VODC018              NOPE, BUMP TO NEXT ENTRY                    
                                                                                
         ST    R5,AODCNTRY         SAVE A(ODC ENTRY)                            
         B     VODC019             GET OUT OF LOOP                              
*                                                                               
VODC018  DS    0H                  BUMP TO NEXT OPTIONAL DATA ENTRY             
         LLGC  R0,ODCDLEN                                                       
         AR    R5,R0                                                            
         B     VODC012                                                          
VODC019  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   BLOCK(L'OPVODCTB),ODCDNUM                                        
         B     VODCXY                                                           
         DROP  R5                                                               
                                                                                
*                                                                               
** VALIDATE OPTIONAL DATA NAME EXITS **                                         
*                                                                               
VODCXY   DS    0H                                                               
         B     YES_01                                                           
VODCXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBR01--LTORG &&+        
                CONSTANTS)'                                                     
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
MESERR   DC    C'** ERROR ENCOUNTERED **'                                       
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM)'                 
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave SFM33 entirely and displays a message go through            
*  this routine.                                                                
* At entry,                                                                     
*   MYTEXT has length and text of text-replace.                                 
                                                                                
*&&DO                                                                           
SUBRXMQ  EQU   (((*-SFM33+X'0FFF')/X'1000')*X'1000')                            
*&&                                                                             
SUBRXMQ  EQU   (((*-SFM33+X'00FF')/X'100')*X'100')                              
                                                                                
         ORG   SFM33+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**18XM**                                                       
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM--ERR MSGS+        
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
                                                                                
         LLGC  RF,OURERRCD         BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
ISELQ    EQU   ((XMERR03-XMERR00)/4)+1                                          
IMKTQ    EQU   ((XMERR04-XMERR00)/4)+1                                          
MKNOFQ   EQU   ((XMERR05-XMERR00)/4)+1                                          
INVOQ    EQU   ((XMERR06-XMERR00)/4)+1                                          
IOCBQ    EQU   ((XMERR07-XMERR00)/4)+1                                          
TMODQ    EQU   ((XMERR08-XMERR00)/4)+1                                          
MODQ     EQU   ((XMERR09-XMERR00)/4)+1                                          
IODVQ    EQU   ((XMERR10-XMERR00)/4)+1                                          
ROMQ     EQU   ((XMERR11-XMERR00)/4)+1                                          
DUPOQ    EQU   ((XMERR12-XMERR00)/4)+1                                          
OSKQ     EQU   ((XMERR13-XMERR00)/4)+1                                          
TMFQ     EQU   ((XMERR14-XMERR00)/4)+1                                          
NCMCNQ   EQU   ((XMERR15-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     ISEL                INVALID SELECT CODE                          
XMERR04  B     IMKT                INVALID MARKET                               
XMERR05  B     MKNOF               MARKET NOT ON FILE                           
XMERR06  B     INVO                INVALID OPTION KEYWORD                       
XMERR07  B     IOCB                INVALID OPTION COMBINATION                   
XMERR08  B     TMOD                TOO MANY OPTION DATA VALUES                  
XMERR09  B     MOD                 MISSING OPTION DATA                          
XMERR10  B     IODV                INVALID OPTION DATA VALUE                    
XMERR11  B     ROPTMISS            REQUIRED OPTION MISSING                      
XMERR12  B     DUPOPT              DUPLICATED OPTION KEYWORD                    
XMERR13  B     OSK                 OPTION SPECIFIED BY KEYWORD ONLY             
XMERR14  B     TMF                 TOO MANY FILTERS IN FIELD                    
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERR15  B     NCMCN               CAN NOT CHANGE MEDIA C/N                     
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
                                                                                
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
MKNOF    DS    0H                  MARKET NOT ON FILE                           
         MVC   MSGNUM2,=AL2(SE#MKNOF)                                           
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
TMF      DS    0H                  TOO MANY FILTERS IN FIELD                    
         MVC   MSGNUM2,=AL2(137)                                                
         MVI   MSGSYS,71                                                        
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM--WRN MSGS+        
               )'                                                               
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* These are messages where the user needs to hit <Enter> only for a             
*  response (for acknowledgment).  Previous values are restored,                
*  except in the case when the key changed in the same transaction.             
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         LA    R2,MKLMEDIH         FORCE CURSOR TO KEY                          
                                                                                
         DS    0H                                                               
         CLI   OURWRNCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURWRNCD,XMWRNQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LLGC  RF,OURWRNCD         BRANCH OFF TO SET WARNING MESSAGE            
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM--INF MSGS+        
               )'                                                               
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
         LA    R1,CONHEAD                                                       
                                                                                
         DS    0H                                                               
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
                                                                                
         LLGC  RF,OURINFCD         BRANCH OFF TO SET INFO MESSAGE               
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM--LTORG &&+        
                CONSTANTS)'                                                     
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SUBRXM--MISC STU+        
                FF)'                                                            
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING'                          
***********************************************************************         
*========================== SFM33's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
MAX#OPTQ EQU   5                   MAX NUMBER OF OPTIONS ALLOWED                
CKYMKTL  EQU   CTDMKMKT-CTDMKEY+L'CTDMKMKT                                      
AKYAMKL  EQU   ANMKAMRK-ANMKEYD+L'ANMKAMRK                                      
MKYMEDL  EQU   MKTKMED-MKTREC+L'MKTKMED                                         
                                                                                
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
OPNTZ    EQU   1                   TIME ZONE: TZ=                               
OPBTZ    EQU   X'80000000'                                                      
OPNSWP   EQU   2                   SWEEP PERIOD: SWP=                           
OPBSWP   EQU   X'40000000'                                                      
OPNODC   EQU   3                   OPTIONAL DATA COLUMN: DATA=                  
OPBODC   EQU   X'20000000'                                                      
OPNUNIT  EQU   4                   UNIT: UNIT=                                  
OPBUNIT  EQU   X'10000000'                                                      
                                                                                
ALLFILTS EQU   OPBTZ+OPBSWP                                                     
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-SFM33,0,AGOSUB-SYSD)                                   
         DC    AL2(SUBR01-SFM33,0,ASUBR01-SYSD)                                 
         DC    AL2(XMSGRTN-SFM33,0,AXMSGRTN-SYSD)                               
         DC    AL2(OPTTABLE-SFM33,0,AOPTTAB-SYSD)                               
         DC    AL2(ODCTAB-SFM33,0,AODCTAB-SYSD)                                 
         DC    AL2(OPTVALS-SYSD,ASYSD-GEND,AOPTVALS-SYSD)                       
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
DCLISTX  EQU   *                                                                
         EJECT                                                                  
*--------------------------- OPTIONS TABLE ---------------------------*         
                                                                                
OPTTABLE DS    0X                  SEE OPTDSECT                                 
*                                                                               
OP01     DS    0X                  TZ=                                          
         DC    AL1(OPNTZ),AL1(OP01X-OP01)                                       
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VTZ#,0)                                                      
         DC    AL4(OPBTZ)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVTZMX),AL1(L'OPVTZTB),AL2(OPVTZNM-SYSD)                    
         DC    AL2(OP01NAM1-OP01,0)                                             
OP01NAM1 DC    AL1(OP01NAMX-OP01NAM1-1),C'TZ'                                   
OP01NAMX DC    AL1(EOT)                                                         
OP01X    EQU   *                                                                
*                                                                               
OP02     DS    0X                  SWP=                                         
         DC    AL1(OPNSWP),AL1(OP02X-OP02)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VSWP#,0)                                                     
         DC    AL4(OPBSWP)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVSWPMX),AL1(L'OPVSWPTB),AL2(OPVSWPNM-SYSD)                 
         DC    AL2(OP02NAM2-OP02,0)                                             
OP02NAM2 DC    AL1(OP02NAMX-OP02NAM2-1),C'SWP'                                  
OP02NAMX DC    AL1(EOT)                                                         
OP02X    EQU   *                                                                
*                                                                               
OP03     DS    0X                  DATA=                                        
         DC    AL1(OPNODC),AL1(OP03X-OP03)                                      
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VODC#,0)                                                     
         DC    AL4(OPBODC)                                                      
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPVODCMX),AL1(L'OPVODCTB),AL2(OPVODCNM-SYSD)                 
         DC    AL2(OP03NAM1-OP03,0)                                             
OP03NAM1 DC    AL1(OP03NAMX-OP03NAM1-1),C'DATA'                                 
OP03NAMX DC    AL1(EOT)                                                         
OP03X    EQU   *                                                                
*                                                                               
OP04     DS    0X                  UNIT=                                        
         DC    AL1(OPNUNIT),AL1(OP04X-OP04)                                     
         DC    AL1(OPDFVRTN)                                                    
         DC    AL1(VUNIT#,0)                                                    
         DC    AL4(OPBUNIT)                                                     
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL1(OPUNTMX),AL1(L'OPUNTTB),AL2(OPUNTNM-SYSD)                    
         DC    AL2(OP04NAM1-OP04,0)                                             
OP04NAM1 DC    AL1(OP04NAMX-OP04NAM1-1),C'UNIT'                                 
OP04NAMX DC    AL1(EOT)                                                         
OP04X    EQU   *                                                                
*                                                                               
OPTZ     DC    AL1(EOT)                                                         
         EJECT                                                                  
*--------------------- OPTIONAL DATA COLUMN TABLE --------------------*         
                                                                                
* TABLE OF ALL THE DIFFERENT TYPES OF DATA THAT CAN BE DISPLAYED                
*  IN THE OPTIONAL COLUMN.                                                      
                                                                                
ODCTAB   DS    0X                  SEE ODCTABD                                  
*                                                                               
ODC01    DC    AL1(01),AL1(ODC01X-ODC01)                                        
         DC    AL1(ODC01NMX-ODC01NAM),AL1(ODC01NAM-ODC01)                       
         DC    AL1(ODC01LBX-ODC01LBL),AL1(ODC01LBL-ODC01)                       
         DC    AL2(PUT_TZ-SFM33)                                                
ODC01NAM DC    C'TZ'                                                            
ODC01NMX EQU   *                                                                
ODC01LBL DC    C'TIME ZONE'                                                     
ODC01LBX EQU   *                                                                
ODC01X   EQU   *                                                                
*                                                                               
ODC02    DC    AL1(02),AL1(ODC02X-ODC02)                                        
         DC    AL1(ODC02NMX-ODC02NAM),AL1(ODC02NAM-ODC02)                       
         DC    AL1(ODC02LBX-ODC02LBL),AL1(ODC02LBL-ODC02)                       
         DC    AL2(PUT_SWP-SFM33)                                               
ODC02NAM DC    C'SWP'                                                           
ODC02NMX EQU   *                                                                
ODC02LBL DC    C'SWEEP CLASS'                                                   
ODC02LBX EQU   *                                                                
ODC02X   EQU   *                                                                
*                                                                               
ODC03    DC    AL1(03),AL1(ODC03X-ODC03)                                        
         DC    AL1(ODC03NMX-ODC03NAM),AL1(ODC03NAM-ODC03)                       
         DC    AL1(ODC03LBX-ODC03LBL),AL1(ODC03LBL-ODC03)                       
         DC    AL2(PUT_RNK-SFM33)                                               
ODC03NAM DC    C'RANK'                                                          
ODC03NMX EQU   *                                                                
ODC03LBL DC    C'RANK'                                                          
ODC03LBX EQU   *                                                                
ODC03X   EQU   *                                                                
*                                                                               
ODC04    DC    AL1(04),AL1(ODC04X-ODC04)                                        
         DC    AL1(ODC04NMX-ODC04NAM),AL1(ODC04NAM-ODC04)                       
         DC    AL1(ODC04LBX-ODC04LBL),AL1(ODC04LBL-ODC04)                       
         DC    AL2(PUT_HMS-SFM33)                                               
ODC04NAM DC    C'HOMES'                                                         
ODC04NMX EQU   *                                                                
ODC04LBL DC    C'HOMES'                                                         
ODC04LBX EQU   *                                                                
ODC04X   EQU   *                                                                
*                                                                               
ODC05    DC    AL1(05),AL1(ODC05X-ODC05)                                        
         DC    AL1(ODC05NMX-ODC05NAM),AL1(ODC05NAM-ODC05)                       
         DC    AL1(ODC05LBX-ODC05LBL),AL1(ODC05LBL-ODC05)                       
         DC    AL2(PUT_NTA-SFM33)                                               
ODC05NAM DC    C'NTA'                                                           
ODC05NMX EQU   *                                                                
ODC05LBL DC    C'NTA'                                                           
ODC05LBX EQU   *                                                                
ODC05X   EQU   *                                                                
*                                                                               
ODC06    DC    AL1(06),AL1(ODC06X-ODC06)                                        
         DC    AL1(ODC06NMX-ODC06NAM),AL1(ODC06NAM-ODC06)                       
         DC    AL1(ODC06LBX-ODC06LBL),AL1(ODC06LBL-ODC06)                       
         DC    AL2(PUT_WT-SFM33)                                                
ODC06NAM DC    C'WEIGHT'                                                        
ODC06NMX EQU   *                                                                
ODC06LBL DC    C'WEIGHT'                                                        
ODC06LBX EQU   *                                                                
ODC06X   EQU   *                                                                
*                                                                               
ODC07    DC    AL1(07),AL1(ODC07X-ODC07)                                        
         DC    AL1(ODC07NMX-ODC07NAM),AL1(ODC07NAM-ODC07)                       
         DC    AL1(ODC07LBX-ODC07LBL),AL1(ODC07LBL-ODC07)                       
         DC    AL2(PUT_SHR-SFM33)                                               
ODC07NAM DC    C'SHARE'                                                         
ODC07NMX EQU   *                                                                
ODC07LBL DC    C'SHARE'                                                         
ODC07LBX EQU   *                                                                
ODC07X   EQU   *                                                                
*                                                                               
ODC08    DC    AL1(08),AL1(ODC08X-ODC08)                                        
         DC    AL1(ODC08NMX-ODC08NAM),AL1(ODC08NAM-ODC08)                       
         DC    AL1(ODC08LBX-ODC08LBL),AL1(ODC08LBL-ODC08)                       
         DC    AL2(PUT_NSI-SFM33)                                               
ODC08NAM DC    C'NSI'                                                           
ODC08NMX EQU   *                                                                
ODC08LBL DC    C'RSM'                                                           
ODC08LBX EQU   *                                                                
ODC08X   EQU   *                                                                
*                                                                               
ODC09    DC    AL1(09),AL1(ODC09X-ODC09)                                        
         DC    AL1(ODC09NMX-ODC09NAM),AL1(ODC09NAM-ODC09)                       
         DC    AL1(ODC09LBX-ODC09LBL),AL1(ODC09LBL-ODC09)                       
         DC    AL2(PUT_NSI-SFM33)                                               
ODC09NAM DC    C'CSI'                                                           
ODC09NMX EQU   *                                                                
ODC09LBL DC    C'RSM'                                                           
ODC09LBX EQU   *                                                                
ODC09X   EQU   *                                                                
*                                                                               
ODC10    DC    AL1(10),AL1(ODC10X-ODC10)                                        
         DC    AL1(ODC10NMX-ODC10NAM),AL1(ODC10NAM-ODC10)                       
         DC    AL1(ODC10LBX-ODC10LBL),AL1(ODC10LBL-ODC10)                       
         DC    AL2(PUT_ARB-SFM33)                                               
ODC10NAM DC    C'ARB'                                                           
ODC10NMX EQU   *                                                                
ODC10LBL DC    C'RSM'                                                           
ODC10LBX EQU   *                                                                
ODC10X   EQU   *                                                                
*                                                                               
ODC11    DC    AL1(11),AL1(ODC11X-ODC11)                                        
         DC    AL1(ODC11NMX-ODC11NAM),AL1(ODC11NAM-ODC11)                       
         DC    AL1(ODC11LBX-ODC11LBL),AL1(ODC11LBL-ODC11)                       
         DC    AL2(PUT_ARB-SFM33)                                               
ODC11NAM DC    C'BBM'                                                           
ODC11NMX EQU   *                                                                
ODC11LBL DC    C'RSM'                                                           
ODC11LBX EQU   *                                                                
ODC11X   EQU   *                                                                
*                                                                               
*                                                                               
ODC12    DC    AL1(12),AL1(ODC12X-ODC12)                                        
         DC    AL1(ODC12NMX-ODC12NAM),AL1(ODC12NAM-ODC12)                       
         DC    AL1(ODC12LBX-ODC12LBL),AL1(ODC12LBL-ODC12)                       
         DC    AL2(PUT_UNIT-SFM33)                                              
ODC12NAM DC    C'UNITS'                                                         
ODC12NMX EQU   *                                                                
ODC12LBL DC    C'UNITS'                                                         
ODC12LBX EQU   *                                                                
ODC12X   EQU   *                                                                
*                                                                               
         DC    AL1(EOT)                                                         
ODCTABX  EQU   *                                                                
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SPSFMWORKD)'             
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SYSD)'                   
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*---------------------- USED BY SPSFM34 AS WELL ----------------------*         
                                                                                
MYSVKEY  DS    XL(L'KEY)                                                        
OPTFLAG  DS    XL1                                                              
OPTLANMQ EQU    X80                LIST BY A/N RECORDS                          
                                                                                
                                                                                
*----------------------- OWNED BY SPSFM33 ONLY -----------------------*         
                                                                                
*                                 ************** WORK AREA ************         
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
RELO     DS    F                                                                
MYDMCB   DS    6F                                                               
                                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
AOPTNTRY DS    A                   A(OPTTABLE ENTRY)                            
AODCNTRY DS    A                   A(ODCTAB ENTRY)                              
AODCFRTN DS    A                   A(OPTIONAL DATA FORMAT ROUTINE)              
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
AOPTTAB  DS    A                    A(OPTTABLE)                                 
AODCTAB  DS    A                    A(ODCTAB)                                   
AOPTVALS DS    A                    A(OPTION VALUES)                            
                                                                                
*                                 ************* INPUT DATA ************         
INPUTKEY DS    0C                  INPUT TO KEY FIELDS                          
IKMEDIA  DS     CL1                 MEDIA                                       
IKNMKT   DS     CL4                 NUMERIC MARKET                              
IKAMKT   DS     CL3                 ALPHA MARKET                                
INPTKEYX EQU   *                                                                
INPTKEYL EQU   INPTKEYX-INPUTKEY                                                
                                                                                
*                                 *********** DATA FROM FILE **********         
DFDATA   DS    0X                                                               
DFMEDIA  DS     CL(L'MKTKMED)       MEDIA                                       
DFMRKT   DS     CL(L'MKTKMKT)       MARKET CODE                                 
DFMKTNAM DS     CL(L'MKTNAME)       MARKET NAME                                 
DFAMKT   DS     CL(L'MKTALST)       ALPHA MARKET LIST                           
DFDATAX  EQU   *                                                                
DFDATAL  EQU   DFDATAX-DFDATA                                                   
                                                                                
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
                                                                                
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1ERRQ  EQU    X01                                                             
MF1RSTKC EQU   0                   RESET THESE ON KEY CHANGE                    
                                                                                
*                                 ************** PROFILES *************         
                                                                                
*                                 ************* CONSTANTS *************         
DCFILSPD DS    CL8                 SPTDIR                                       
DCFILSPF DS    CL8                 SPTFIL                                       
DCFILSTA DS    CL8                 STATION                                      
                                                                                
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
DSLISTX  EQU   *                                                                
                                                                                
*                                 ************** BUFFERS **************         
MYKEY    DS    XL(L'KEY)                                                        
                                                                                
ODCDATA  DS    CL(L'MKLODC1)                                                    
                                                                                
ZEROES   DS    CL(L'KEY)                                                        
DASHES   DS    CL(L'MKLODC1)                                                    
                                                                                
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
                                                                                
OPVTZ    DS    0X                  OPTION VALUES FOR TIME ZONE                  
OPVTZMX  EQU    1                       MAX # OF VALUES ALLOWED                 
OPVTZNM  DS     XL1                     # OF VALUES SO FAR                      
OPVTZTB  DS     (OPVTZMX)CL1            TABLE TO HOLD VALUES                    
OPVTZQ   EQU   *-OPVTZ                                                          
                                                                                
OPVSWP   DS    0X                  OPTION VALUES FOR SWEEP PERIOD               
OPVSWPMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVSWPNM DS     XL1                     # OF VALUES SO FAR                      
OPVSWPTB DS     (OPVSWPMX)CL1           TABLE TO HOLD VALUES                    
OPVSWPQ  EQU   *-OPVSWP                                                         
                                                                                
OPVODC   DS    0X                  OPTION VALUES FOR OPTIONAL DATA COL          
OPVODCMX EQU    1                       MAX # OF VALUES ALLOWED                 
OPVODCNM DS     XL1                     # OF VALUES SO FAR                      
OPVODCTB DS     (OPVODCMX)XL(L'ODCDNUM) TABLE TO HOLD VALUES                    
OPVODCQ  EQU   *-OPVODC                                                         
                                                                                
OPUNIT   DS    0X                  OPTION VALUES FOR TIME ZONE                  
OPUNTMX  EQU    1                       MAX # OF VALUES ALLOWED                 
OPUNTNM  DS     XL1                     # OF VALUES SO FAR                      
OPUNTTB  DS     (OPUNTMX)CL2            TABLE TO HOLD VALUES                    
OPVUNTQ  EQU   *-OPUNIT                                                         
                                                                                
OPTVALSQ EQU   *-OPTVALS                                                        
         EJECT                                                                  
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   1024-MYSSPREL         AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (TWA DSECTS)'             
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*------------------------- MARKET/LIST SCREEN ------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD3D                                                       
                                                                                
                                                                                
         DS    0CL((L'MKLODC1-L'MKLODC2)+1)                                     
         DS    0CL((L'MKLODC2-L'MKLODC1)+1)                                     
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
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (OTHER DSECTS)'           
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
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (SPGEN DSECTS)'           
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*------------------------------ SPGENMKT -----------------------------*         
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
*----------------------------- SPGENANMK -----------------------------*         
                                                                                
       ++INCLUDE SPGENANMK                                                      
***********************************************************************         
         TITLE 'SPSFM33 - SPOT MARKET-RECORDS LISTING (MISC DSECTS)'            
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
*------------------ OPTIONAL DATA COLUMN TABLE DSECT -----------------*         
                                                                                
ODCTABD  DSECT                                                                  
ODCDNUM  DS    XL1                 ENTRY NUMBER OF OPTIONAL DATA                
ODCDLEN  DS    XL1                 LENGTH OF ENTRY                              
ODCDNMLN DS    XL1                 LENGTH OF OPTIONAL DATA NAME                 
ODCDNMDS DS    XL1                 DISPL TO OPTIONAL DATA NAME                  
ODCDLBLN DS    XL1                 LENGTH OF OPTIONAL DATA COLUMN LABEL         
ODCDLBDS DS    XL1                 DISPL TO OPTIONAL DATA COLUMN LABEL          
ODCDFRTN DS    XL2                 DISPL TO OPTIONAL DATA FORMAT RTN            
ODCDNAME DS    0C                                                               
ODCDLABL DS    0C                                                               
         EJECT                                                                  
*----------------------------- LIST LINE -----------------------------*         
                                                                                
LISTD    DSECT                                                                  
LSTNCDE  DS    CL4                 NUMERIC MARKET CODE                          
         DS    CL7                                                              
LSTNAME  DS    CL24                MARKET NAME                                  
         DS    CL3                                                              
LSTAMKTS DS    CL11                ALPHA MARKETS (MAX 3)                        
         DS    CL3                                                              
LSTODC   DS    CL15                OPTIONAL DATA COLUMN                         
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPSFM33   08/07/07'                                      
         END                                                                    
